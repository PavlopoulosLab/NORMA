"""HTTP routing: the page, /norma-config.js, the NORMA API and the relays."""

from __future__ import annotations

import http.server
import json
import re
import socketserver
import ssl
import sys
from typing import Any

from norma import API_VERSION, APP_VERSION, api, config, relays, static

_RELAY_PREFIXES = ("/string-api/", "/arena3d-api/", "/db-api/", "/api/")


class NormaHandler(http.server.SimpleHTTPRequestHandler):
    server_version = "NORMA3"

    def __init__(self, *args: Any, **kwargs: Any) -> None:
        super().__init__(*args, directory=static.STATIC_DIR, **kwargs)

    # one line per request; hosted servers log without IP addresses by default
    def log_message(self, format: str, *args: Any) -> None:
        mode = config.CONFIG["server"]["accessLog"]
        if mode == "off":
            return
        who = self.client_address[0] if mode == "full" else "-"
        sys.stderr.write(f"{self.log_date_time_string()}  {who}  {format % args}\n")

    def log_error(self, format: str, *args: Any) -> None:
        sys.stderr.write(f"{self.log_date_time_string()}  error  {format % args}\n")

    def _https(self) -> bool:
        if isinstance(self.connection, ssl.SSLSocket):
            return True
        trust: bool = config.CONFIG["server"]["trustProxy"]
        return trust and self.headers.get("X-Forwarded-Proto", "").lower() == "https"

    def end_headers(self) -> None:
        # the app is a single page; always serve the latest copy
        if not self.path.startswith(_RELAY_PREFIXES):
            self.send_header("Cache-Control", "no-cache")
        if self.path.startswith("/api/"):
            # the NORMA API may be called from any web page
            self.send_header("Access-Control-Allow-Origin", "*")
            self.send_header("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
            self.send_header("Access-Control-Allow-Headers", "Content-Type")
            self.send_header("Access-Control-Max-Age", "86400")
        self.send_header("X-Content-Type-Options", "nosniff")
        self.send_header("Referrer-Policy", "strict-origin-when-cross-origin")
        self.send_header(
            "Permissions-Policy", "camera=(), microphone=(), geolocation=(), interest-cohort=()"
        )
        if config.CONFIG["server"]["hsts"] and self._https():
            self.send_header("Strict-Transport-Security", "max-age=31536000")
        super().end_headers()

    # ---------- routing ----------
    def do_GET(self) -> None:
        # the application is norma.html; the site root opens it
        bare = self.path.split("?")[0]
        if bare in ("/", "/index.html"):
            query = self.path[len(bare) :]
            self.path = "/norma.html" + query
        bare = self.path.split("?")[0]
        if bare in ("/norma-config.js", "/norma-config.json"):
            return self._client_config(bare.endswith(".js"))
        cfg = config.CONFIG
        if bare.startswith("/string-api/") and not cfg["relays"]["string"]:
            return self.send_json(
                404, {"message": "The STRING relay is turned off on this server."}
            )
        if bare.startswith("/db-api/") and not cfg["relays"]["databases"]:
            return self.send_json(
                404, {"message": "The database relay is turned off on this server."}
            )
        if bare.startswith("/api/") and not cfg["api"]["enabled"]:
            return self.send_json(404, {"message": "The NORMA API is turned off on this server."})
        if bare == "/string-api/ping":
            return self.send_json(200, {"norma_proxy": True, "upstream": relays.DEFAULT_UPSTREAM})
        if bare.startswith("/string-api/"):
            return relays.string(self, "GET")
        if bare == "/db-api/fetch":
            return relays.database(self, "GET")
        if bare.startswith("/api/"):
            return self._api_get()
        if static.blocked(self.path):
            return self.send_json(404, {"message": "Not found."})
        return super().do_GET()

    def do_HEAD(self) -> None:
        if self.path.startswith("/string-api/"):
            return self.send_json(405, {"message": "Use GET or POST."})
        if static.blocked(self.path):
            return self.send_json(404, {"message": "Not found."})
        return super().do_HEAD()

    def do_POST(self) -> None:
        bare = self.path.split("?")[0]
        cfg = config.CONFIG
        off = (
            (bare.startswith("/string-api/") and not cfg["relays"]["string"])
            or (bare.startswith("/db-api/") and not cfg["relays"]["databases"])
            or (bare.startswith("/arena3d-api/") and not cfg["relays"]["arena3d"])
            or (bare.startswith("/api/") and not cfg["api"]["enabled"])
        )
        if off:
            return self.send_json(404, {"message": "This service is turned off on this server."})
        if bare.startswith("/string-api/"):
            return relays.string(self, "POST")
        if bare == "/arena3d-api/external":
            return relays.arena3d(self)
        if bare == "/db-api/fetch":
            return relays.database(self, "POST")
        if bare == "/api/external":
            return self._api_post()
        return self.send_json(
            405,
            {
                "message": "Only the NORMA API and the STRING, Arena3D and database relays "
                "accept POST requests."
            },
        )

    def do_OPTIONS(self) -> None:
        self.send_response(204)
        self.send_header("Allow", "GET, POST, OPTIONS")
        self.end_headers()

    def _client_config(self, as_js: bool) -> None:
        body = json.dumps(config.client_config(config.CONFIG), indent=1)
        if as_js:
            body = f"// generated by the NORMA server\nwindow.NORMA_CONFIG = {body};\n"
        ctype = "text/javascript; charset=utf-8" if as_js else "application/json; charset=utf-8"
        self.send_bytes(200, body.encode("utf-8"), ctype)

    # ---------- NORMA API ----------
    def _base_url(self) -> str:
        srv = config.CONFIG["server"]
        if srv["publicUrl"]:
            return str(srv["publicUrl"])
        trust = srv["trustProxy"]
        host = (
            (trust and self.headers.get("X-Forwarded-Host"))
            or self.headers.get("Host")
            or "localhost"
        )
        proto = (trust and self.headers.get("X-Forwarded-Proto")) or (
            "https" if isinstance(self.connection, ssl.SSLSocket) else "http"
        )
        prefix = ((trust and self.headers.get("X-Forwarded-Prefix")) or "").rstrip("/")
        return f"{proto}://{host}{prefix}/"

    def _api_post(self) -> None:
        length = int(self.headers.get("Content-Length") or 0)
        if length <= 0:
            return self.send_json(400, {"message": "Send the payload as a JSON body."})
        if length > api.MAX_BYTES:
            return self.send_json(
                413,
                {"message": f"The payload is larger than {api.MAX_BYTES // (1024 * 1024)} MB."},
            )
        body = self.read_body(length)
        try:
            obj = json.loads(body.decode("utf-8"))
        except (ValueError, UnicodeDecodeError):
            return self.send_json(400, {"message": "The payload is not valid JSON."})
        if not isinstance(obj, dict):
            return self.send_json(400, {"message": "The payload must be a JSON object."})
        if not any(k in obj for k in ("edges", "files", "network", "nodes", "session", "views")):
            return self.send_json(
                400, {"message": 'Nothing to show: give "edges", "files", "network" or a session.'}
            )
        token = api.put(body)
        url = self._base_url() + "norma.html?session=" + token
        return self.send_json(200, {"token": token, "url": url, "expiresInHours": api.TTL_HOURS})

    def _api_get(self) -> None:
        bare = self.path.split("?")[0]
        if bare == "/api/health":
            return self.send_json(
                200,
                {
                    "status": "ok",
                    "api": API_VERSION,
                    "version": APP_VERSION,
                    "mode": config.CONFIG["mode"],
                    "stored": api.count(),
                    "expiresInHours": api.TTL_HOURS,
                    "relays": config.CONFIG["relays"],
                },
            )
        m = re.match(r"^/api/session/([A-Za-z0-9_-]{8,64})$", bare)
        if not m:
            return self.send_json(404, {"message": "Unknown API address."})
        data = api.get(m.group(1))
        if data is None:
            return self.send_json(404, {"message": "This link has expired or does not exist."})
        self.send_bytes(200, data, "application/json; charset=utf-8", no_store=True)

    # ---------- responses ----------
    def read_body(self, length: int) -> bytes:
        return self.rfile.read(length)

    def send_bytes(
        self, status: int, data: bytes, ctype: str, relay: bool = False, no_store: bool = False
    ) -> None:
        self.send_response(status)
        if relay:
            self.send_header("X-Norma-Relay", "1")
        self.send_header("Content-Type", ctype)
        self.send_header("Content-Length", str(len(data)))
        if relay or no_store:
            self.send_header("Cache-Control", "no-store")
        self.end_headers()
        self.wfile.write(data)

    def send_json(self, status: int, obj: object, relay: bool = False) -> None:
        self.send_bytes(
            status, json.dumps(obj).encode("utf-8"), "application/json; charset=utf-8", relay
        )

    def send_text(self, status: int, text: str, relay: bool = False) -> None:
        self.send_bytes(status, text.encode("utf-8"), "text/plain; charset=utf-8", relay)


class ThreadingServer(socketserver.ThreadingMixIn, http.server.HTTPServer):
    daemon_threads = True
    allow_reuse_address = True


class RedirectHandler(http.server.BaseHTTPRequestHandler):
    """Sends plain-HTTP visitors to the HTTPS address."""

    target = ""

    def do_GET(self) -> None:
        self.send_response(301)
        self.send_header("Location", self.target.rstrip("/") + self.path)
        self.end_headers()

    do_HEAD = do_GET
    do_POST = do_GET

    def log_message(self, format: str, *args: Any) -> None:
        pass
