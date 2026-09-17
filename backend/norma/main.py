"""Command line and serve loop."""

from __future__ import annotations

import argparse
import json
import os
import ssl
import threading

from norma import APP_VERSION, api, config, static
from norma.handler import NormaHandler, RedirectHandler, ThreadingServer


def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    ap = argparse.ArgumentParser(description="Serve NORMA 3 locally or as a public web server.")
    ap.add_argument(
        "--config", help="settings file (JSON); default norma.config.json in the repository root"
    )
    ap.add_argument(
        "--mode",
        choices=("local", "hosted"),
        help="local (this computer) or hosted (public web server)",
    )
    ap.add_argument(
        "--host", help="address to listen on (local default 127.0.0.1, hosted default 0.0.0.0)"
    )
    ap.add_argument("--port", type=int, help="port (default 8000; 443 for HTTPS without a proxy)")
    ap.add_argument(
        "--public-url",
        help="the address users see, e.g. https://norma.example.org/ (used in API links)",
    )
    ap.add_argument("--tls-cert", help="certificate (PEM) to serve HTTPS directly")
    ap.add_argument("--tls-key", help="private key (PEM) for --tls-cert")
    ap.add_argument(
        "--http-redirect-port",
        type=int,
        help="also listen on this port and redirect HTTP to HTTPS (e.g. 80)",
    )
    ap.add_argument(
        "--access-log",
        choices=("full", "anonymous", "off"),
        help="request log (hosted default: anonymous, without IP addresses)",
    )
    ap.add_argument("--no-api", action="store_true", help="turn off the REST API")
    ap.add_argument(
        "--no-relays", action="store_true", help="turn off the STRING, database and Arena3D relays"
    )
    ap.add_argument("--open", action="store_true", help="open NORMA in the browser")
    ap.add_argument(
        "--no-open",
        action="store_true",
        help="don't open the browser (local mode opens it by default)",
    )
    ap.add_argument(
        "--print-config", action="store_true", help="print the effective settings and exit"
    )
    return ap.parse_args(argv)


def main(argv: list[str] | None = None) -> None:
    args = parse_args(argv)
    cfg = config.load_config(args)
    config.CONFIG.clear()
    config.CONFIG.update(cfg)
    api.configure(cfg)
    if args.print_config:
        print(json.dumps(cfg, indent=2))
        return
    srv = cfg["server"]
    NormaHandler.extensions_map.update(
        {
            ".js": "text/javascript",
            ".mjs": "text/javascript",
            ".svg": "image/svg+xml",
            ".json": "application/json",
        }
    )
    tls = bool(srv["tlsCert"] and srv["tlsKey"])
    with ThreadingServer((srv["host"], int(srv["port"])), NormaHandler) as httpd:
        if tls:
            ctx = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
            ctx.minimum_version = ssl.TLSVersion.TLSv1_2
            ctx.load_cert_chain(srv["tlsCert"], srv["tlsKey"])
            httpd.socket = ctx.wrap_socket(httpd.socket, server_side=True)
        scheme = "https" if tls else "http"
        shown = "localhost" if srv["host"] in ("127.0.0.1", "0.0.0.0", "::") else srv["host"]
        port = int(srv["port"])
        default_port = (tls and port == 443) or (not tls and port == 80)
        url = srv["publicUrl"] or "{}://{}{}/".format(
            scheme,
            shown,
            "" if default_port else f":{port}",
        )
        if srv["httpRedirectPort"]:
            RedirectHandler.target = url
            redirect = ThreadingServer((srv["host"], int(srv["httpRedirectPort"])), RedirectHandler)
            threading.Thread(target=redirect.serve_forever, daemon=True).start()
        on = [k for k, v in cfg["relays"].items() if v]
        print("NORMA {} ({} mode) is running at {}".format(APP_VERSION, cfg["mode"], url))
        print("  REST API: %s" % (f"{url}api/external" if cfg["api"]["enabled"] else "off"))
        print("  relays:   %s" % (", ".join(on) if on else "off"))
        print("  settings: {}".format(cfg.get("_file", "built-in defaults")))
        print(f"  files:    {static.STATIC_DIR}")
        if not os.path.isfile(os.path.join(static.STATIC_DIR, "norma.html")):
            print("  note: the page is not built yet: cd frontend && npm install && npm run build")
        if cfg["mode"] == "hosted" and not tls and not srv["trustProxy"]:
            print("  note: public servers should use HTTPS (TLS options or a reverse proxy).")
        print("Press Ctrl+C to stop.")
        if srv["openBrowser"]:
            import webbrowser

            webbrowser.open(url)
        try:
            httpd.serve_forever()
        except KeyboardInterrupt:
            print("\nStopped.")
