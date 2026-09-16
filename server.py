#!/usr/bin/env python3
"""
NORMA 3.0 server
================

Serves the NORMA web application (norma.html), offers NORMA's REST API and
relays requests for STRING, the database importers and Arena3D, so the browser
only ever talks to this server. The same program runs NORMA on a laptop
("local" mode) or as a public web server ("hosted" mode).

    python3 server.py                          # local: http://127.0.0.1:8000, opens the browser
    python3 server.py --mode hosted --config norma.config.json
    python3 server.py --help                   # every option

Settings come from, in increasing priority: built-in defaults, a JSON file
(--config, NORMA_CONFIG, or norma.config.json next to this file), NORMA_*
environment variables, and command-line options. `--print-config` shows the
result. The settings meant for the page are served as /norma-config.js.

Only the Python 3 standard library is used (Python 3.8 or newer).

The relay
---------
    GET  /string-api/ping                     -> {"norma_proxy": true, ...}
    POST /string-api/<format>/<method>?upstream=<STRING address>

The request body (form-encoded) is passed on unchanged to
    <STRING address>/api/<format>/<method>
Only STRING addresses (string-db.org and its version-specific sub-domains)
and only read-only API methods are relayed. As STRING asks, requests are
spaced at least one second apart and carry a caller identity.

    POST /arena3d-api/external?upstream=<Arena3D address>

The JSON body (a network in Arena3D's format) is passed on to
    <Arena3D address>/api/external
which answers with { "token": ..., "url": ... }. Only arena3d.org
addresses are allowed.

    GET or POST /db-api/fetch?url=<address>

relays the database importers (Reactome, OmniPath, NDEx, IntAct, QuickGO
and the GO API); only those services' API addresses are allowed.

NORMA API (for other applications):

    POST /api/external        JSON payload -> {"token", "url", "expiresInHours"}
    GET  /api/session/TOKEN   the stored payload (NORMA opens ?session=TOKEN)
    GET  /api/health          {"status": "ok", "api": "1.0"}

Payloads are kept in memory for NORMA_API_TTL_HOURS (default 24) hours, up to
50 MB each and 500 at a time. The API allows calls from any web page (CORS).
"""
import argparse
import copy
import hashlib
import ssl
import http.server
import json
import os
import re
import secrets
import socketserver
import sys
import threading
import time
import urllib.error
import urllib.parse
import urllib.request

APP_DIR = os.path.dirname(os.path.abspath(__file__))
DEFAULT_UPSTREAM = "https://string-db.org"
ALLOWED_UPSTREAM = re.compile(r"^https://([a-z0-9-]+\.)*string-db\.org$", re.I)
ALLOWED_FORMATS = {"json", "tsv", "tsv-no-header", "xml"}
ALLOWED_METHODS = {
    "version", "get_string_ids", "network", "interaction_partners",
    "enrichment", "functional_annotation", "ppi_enrichment",
    "homology", "homology_best", "functional_terms", "geneset_description",
}
MAX_BODY = 5 * 1024 * 1024          # 5 MB of identifiers is plenty
TIMEOUT = 120                        # seconds to wait for STRING
MIN_GAP = 1.0                        # seconds between STRING requests
USER_AGENT = "NORMA3-local-server"

ARENA_DEFAULT = "https://arena3d.org"
ALLOWED_ARENA = re.compile(r"^https://([a-z0-9-]+\.)*arena3d\.org$", re.I)
MAX_ARENA_BODY = 50 * 1024 * 1024    # networks can be large

ALLOWED_DB = [re.compile(p) for p in (
    r"^https://reactome\.org/ContentService/",
    r"^https://omnipathdb\.org/",
    r"^https://(www|public)\.ndexbio\.org/v[23]/",
    r"^https://www\.ebi\.ac\.uk/Tools/webservices/psicquic/",
    r"^https://www\.ebi\.ac\.uk/QuickGO/services/",
    r"^https://api\.geneontology\.org/api/",
)]
MAX_DB_BODY = 1024 * 1024

# for automated tests only: allow local mocks of STRING and Arena3D
TEST_UPSTREAM = os.environ.get("NORMA_STRING_TEST_UPSTREAM", "")
TEST_ARENA = os.environ.get("NORMA_ARENA_TEST_UPSTREAM", "")
TEST_DB = os.environ.get("NORMA_DB_TEST_PREFIX", "")

_gap_lock = threading.Lock()
_last_call = [0.0]


_short_lock = threading.Lock()
_short_last = [0.0]


def _wait_turn_short():
    # a light spacing for the database relay (several requests run at once)
    with _short_lock:
        wait = _short_last[0] + 0.05 - time.monotonic()
        if wait > 0:
            time.sleep(wait)
        _short_last[0] = time.monotonic()


def _wait_turn():
    with _gap_lock:
        wait = _last_call[0] + MIN_GAP - time.monotonic()
        if wait > 0:
            time.sleep(wait)
        _last_call[0] = time.monotonic()


API_VERSION = "1.0"
APP_VERSION = "3.0"

# ---------- settings ----------
DEFAULTS = {
    "mode": "local",                      # "local" or "hosted"
    "server": {
        "host": None,                     # local: 127.0.0.1, hosted: 0.0.0.0
        "port": 8000,
        "publicUrl": "",                  # e.g. https://norma.example.org/ (used in API links)
        "trustProxy": None,               # read X-Forwarded-* headers (hosted: true)
        "tlsCert": "",                    # PEM files to serve HTTPS directly
        "tlsKey": "",
        "httpRedirectPort": 0,            # also listen here and redirect to HTTPS (e.g. 80)
        "hsts": True,                     # Strict-Transport-Security when served over HTTPS
        "accessLog": None,                # "full", "anonymous" (no IP addresses) or "off"
        "openBrowser": None,              # local: true
    },
    "api": {"enabled": True, "ttlHours": 24, "maxMB": 50, "maxSessions": 500},
    "relays": {"string": True, "arena3d": True, "databases": True},
    "site": {
        "name": "NORMA",
        "institution": "",
        "contactName": "Pavlopoulos Lab",
        "contactEmail": "",
        "contactUrl": "https://github.com/PavlopoulosLab/NORMA/issues",
        "sourceUrl": "https://github.com/PavlopoulosLab/NORMA",
        "licenceName": "MIT License",
        "licenceUrl": "https://opensource.org/licenses/MIT",
        "privacyUrl": "",
        "imprintUrl": "",
        "maintainedUntil": "",
        "notice": "",
        "testedBrowsers": [],
    },
    "app": {"maxNodes": 5000, "theme": "white", "startTab": "welcome", "cdnFallback": True},
}
ENV_MAP = {
    "NORMA_MODE": ("mode",), "NORMA_HOST": ("server", "host"), "NORMA_PORT": ("server", "port"),
    "NORMA_PUBLIC_URL": ("server", "publicUrl"), "NORMA_TLS_CERT": ("server", "tlsCert"),
    "NORMA_TLS_KEY": ("server", "tlsKey"), "NORMA_ACCESS_LOG": ("server", "accessLog"),
    "NORMA_TRUST_PROXY": ("server", "trustProxy"), "NORMA_API": ("api", "enabled"),
    "NORMA_API_TTL_HOURS": ("api", "ttlHours"), "NORMA_API_MAX_MB": ("api", "maxMB"),
    "NORMA_RELAYS": ("relays", "*"), "NORMA_CONTACT_EMAIL": ("site", "contactEmail"),
    "NORMA_NOTICE": ("site", "notice"), "NORMA_INSTITUTION": ("site", "institution"),
    "NORMA_MAX_NODES": ("app", "maxNodes"),
}


def _merge(base, extra):
    for k, v in (extra or {}).items():
        if isinstance(v, dict) and isinstance(base.get(k), dict):
            _merge(base[k], v)
        else:
            base[k] = v
    return base


def _typed(template, value):
    if isinstance(template, bool) or template is None and value.lower() in ("true", "false", "1", "0", "yes", "no", "on", "off"):
        return value.lower() in ("1", "true", "yes", "on")
    if isinstance(template, int) and not isinstance(template, bool):
        return int(value)
    if isinstance(template, float):
        return float(value)
    return value


def _set(cfg, path, value):
    if path[-1] == "*":
        for k in cfg[path[0]]:
            cfg[path[0]][k] = _typed(True, value)
        return
    node = cfg
    for k in path[:-1]:
        node = node[k]
    node[path[-1]] = _typed(node.get(path[-1]), value)


def load_config(args):
    cfg = copy.deepcopy(DEFAULTS)
    path = args.config or os.environ.get("NORMA_CONFIG") or os.path.join(APP_DIR, "norma.config.json")
    if os.path.isfile(path):
        with open(path, encoding="utf-8") as fh:
            _merge(cfg, json.load(fh))
        cfg["_file"] = path
    elif args.config:
        sys.exit("Settings file not found: %s" % args.config)
    for env, target in ENV_MAP.items():
        if os.environ.get(env, "") != "":
            _set(cfg, target, os.environ[env])
    cli = {
        "mode": args.mode, ("server", "host"): args.host, ("server", "port"): args.port,
        ("server", "publicUrl"): args.public_url, ("server", "tlsCert"): args.tls_cert,
        ("server", "tlsKey"): args.tls_key, ("server", "httpRedirectPort"): args.http_redirect_port,
        ("server", "accessLog"): args.access_log,
    }
    for key, value in cli.items():
        if value is None:
            continue
        if key == "mode":
            cfg["mode"] = value
        else:
            cfg[key[0]][key[1]] = value
    if args.no_api:
        cfg["api"]["enabled"] = False
    if args.no_relays:
        cfg["relays"] = {k: False for k in cfg["relays"]}
    if args.open:
        cfg["server"]["openBrowser"] = True
    if args.no_open:
        cfg["server"]["openBrowser"] = False
    if cfg["mode"] not in ("local", "hosted"):
        sys.exit('"mode" must be "local" or "hosted".')
    hosted = cfg["mode"] == "hosted"
    srv = cfg["server"]
    if srv["host"] is None:
        srv["host"] = "0.0.0.0" if hosted else "127.0.0.1"
    if srv["trustProxy"] is None:
        srv["trustProxy"] = hosted
    if srv["accessLog"] is None:
        srv["accessLog"] = "anonymous" if hosted else "off"
    if srv["openBrowser"] is None:
        srv["openBrowser"] = not hosted
    if srv["publicUrl"] and not srv["publicUrl"].endswith("/"):
        srv["publicUrl"] += "/"
    return cfg


def client_config(cfg):
    """The settings the page needs; nothing secret."""
    return {
        "mode": cfg["mode"],
        "version": APP_VERSION,
        "publicUrl": cfg["server"]["publicUrl"],
        "features": {
            "restApi": bool(cfg["api"]["enabled"]),
            "relays": dict(cfg["relays"]),
            "apiTtlHours": cfg["api"]["ttlHours"],
        },
        "site": cfg["site"],
        "app": cfg["app"],
    }


CONFIG = copy.deepcopy(DEFAULTS)          # replaced in main()
API_MAX_BYTES = 50 * 1024 * 1024
API_MAX_SESSIONS = 500
API_TTL_HOURS = float(os.environ.get("NORMA_API_TTL_HOURS", "24") or 24)
_api_store = {}                  # token -> (created, bytes)
_api_lock = threading.Lock()


def api_put(data):
    token = secrets.token_urlsafe(18)
    now = time.time()
    with _api_lock:
        _api_expire(now)
        while len(_api_store) >= API_MAX_SESSIONS:
            oldest = min(_api_store, key=lambda k: _api_store[k][0])
            del _api_store[oldest]
        _api_store[token] = (now, data)
    return token


def api_get(token):
    with _api_lock:
        _api_expire(time.time())
        item = _api_store.get(token)
    return item[1] if item else None


def _api_expire(now):
    limit = API_TTL_HOURS * 3600
    for k in [k for k, (t, _) in _api_store.items() if now - t > limit]:
        del _api_store[k]


# Static files are served straight out of APP_DIR (see NormaHandler.__init__),
# which is also where deployment/VCS files live (.git, Dockerfile, deploy/, ...).
# Keep those from being handed out over HTTP regardless of what a given
# deployment happens to have sitting next to server.py.
_BLOCKED_STATIC_NAMES = {"dockerfile", "docker-compose.yml", "docker-compose.yaml"}
_BLOCKED_STATIC_DIRS = {"deploy"}


def _static_blocked(path):
    parts = [p for p in path.split("?", 1)[0].split("/") if p]
    if any(p.startswith(".") for p in parts):
        return True
    if parts and parts[0].lower() in _BLOCKED_STATIC_DIRS:
        return True
    if parts and (parts[-1].lower() in _BLOCKED_STATIC_NAMES or parts[-1].endswith(".service")):
        return True
    return False


class NormaHandler(http.server.SimpleHTTPRequestHandler):
    server_version = "NORMA3"

    def __init__(self, *args, **kwargs):
        super().__init__(*args, directory=APP_DIR, **kwargs)

    # one line per request; hosted servers log without IP addresses by default
    def log_message(self, fmt, *args):
        mode = CONFIG["server"]["accessLog"]
        if mode == "off":
            return
        who = self.client_address[0] if mode == "full" else "-"
        sys.stderr.write("%s  %s  %s\n" % (self.log_date_time_string(), who, fmt % args))

    def log_error(self, fmt, *args):
        sys.stderr.write("%s  error  %s\n" % (self.log_date_time_string(), fmt % args))

    def _https(self):
        if isinstance(self.connection, ssl.SSLSocket):
            return True
        return CONFIG["server"]["trustProxy"] and self.headers.get("X-Forwarded-Proto", "").lower() == "https"

    def end_headers(self):
        # the app is a single page; always serve the latest copy
        if not self.path.startswith(("/string-api/", "/arena3d-api/", "/db-api/", "/api/")):
            self.send_header("Cache-Control", "no-cache")
        if self.path.startswith("/api/"):
            # the NORMA API may be called from any web page
            self.send_header("Access-Control-Allow-Origin", "*")
            self.send_header("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
            self.send_header("Access-Control-Allow-Headers", "Content-Type")
            self.send_header("Access-Control-Max-Age", "86400")
        self.send_header("X-Content-Type-Options", "nosniff")
        self.send_header("Referrer-Policy", "strict-origin-when-cross-origin")
        self.send_header("Permissions-Policy", "camera=(), microphone=(), geolocation=(), interest-cohort=()")
        if CONFIG["server"]["hsts"] and self._https():
            self.send_header("Strict-Transport-Security", "max-age=31536000")
        super().end_headers()

    # ---------- routing ----------
    def do_GET(self):
        # the application is norma.html; the site root opens it
        bare = self.path.split("?")[0]
        if bare in ("/", "/index.html"):
            query = self.path[len(bare):]
            self.path = "/norma.html" + query
        bare = self.path.split("?")[0]
        if bare in ("/norma-config.js", "/norma-config.json"):
            return self._client_config(bare.endswith(".js"))
        relays = CONFIG["relays"]
        if bare.startswith("/string-api/") and not relays["string"]:
            return self._json(404, {"message": "The STRING relay is turned off on this server."})
        if bare.startswith("/db-api/") and not relays["databases"]:
            return self._json(404, {"message": "The database relay is turned off on this server."})
        if bare.startswith("/api/") and not CONFIG["api"]["enabled"]:
            return self._json(404, {"message": "The NORMA API is turned off on this server."})
        if self.path.split("?")[0] == "/string-api/ping":
            return self._json(200, {"norma_proxy": True, "upstream": DEFAULT_UPSTREAM})
        if self.path.startswith("/string-api/"):
            return self._relay("GET")
        if self.path.split("?")[0] == "/db-api/fetch":
            return self._relay_db("GET")
        if self.path.startswith("/api/"):
            return self._api_get()
        if _static_blocked(self.path):
            return self._json(404, {"message": "Not found."})
        return super().do_GET()

    def do_HEAD(self):
        if self.path.startswith("/string-api/"):
            return self._json(405, {"message": "Use GET or POST."})
        if _static_blocked(self.path):
            return self._json(404, {"message": "Not found."})
        return super().do_HEAD()

    def do_POST(self):
        bare = self.path.split("?")[0]
        relays = CONFIG["relays"]
        off = ((bare.startswith("/string-api/") and not relays["string"])
               or (bare.startswith("/db-api/") and not relays["databases"])
               or (bare.startswith("/arena3d-api/") and not relays["arena3d"])
               or (bare.startswith("/api/") and not CONFIG["api"]["enabled"]))
        if off:
            return self._json(404, {"message": "This service is turned off on this server."})
        if self.path.startswith("/string-api/"):
            return self._relay("POST")
        if self.path.split("?")[0] == "/arena3d-api/external":
            return self._relay_arena()
        if self.path.split("?")[0] == "/db-api/fetch":
            return self._relay_db("POST")
        if self.path.split("?")[0] == "/api/external":
            return self._api_post()
        return self._json(405, {"message": "Only the NORMA API and the STRING, Arena3D and database relays accept POST requests."})

    def do_OPTIONS(self):
        self.send_response(204)
        self.send_header("Allow", "GET, POST, OPTIONS")
        self.end_headers()

    def _client_config(self, as_js):
        body = json.dumps(client_config(CONFIG), indent=1)
        if as_js:
            body = "// generated by server.py from its settings\nwindow.NORMA_CONFIG = %s;\n" % body
        data = body.encode("utf-8")
        self.send_response(200)
        self.send_header("Content-Type", "text/javascript; charset=utf-8" if as_js else "application/json; charset=utf-8")
        self.send_header("Content-Length", str(len(data)))
        self.end_headers()
        self.wfile.write(data)

    # ---------- NORMA API ----------
    def _base_url(self):
        if CONFIG["server"]["publicUrl"]:
            return CONFIG["server"]["publicUrl"]
        trust = CONFIG["server"]["trustProxy"]
        host = (trust and self.headers.get("X-Forwarded-Host")) or self.headers.get("Host") or "localhost"
        proto = (trust and self.headers.get("X-Forwarded-Proto")) or ("https" if isinstance(self.connection, ssl.SSLSocket) else "http")
        prefix = ((trust and self.headers.get("X-Forwarded-Prefix")) or "").rstrip("/")
        return "%s://%s%s/" % (proto, host, prefix)

    def _api_post(self):
        length = int(self.headers.get("Content-Length") or 0)
        if length <= 0:
            return self._json(400, {"message": "Send the payload as a JSON body."})
        if length > API_MAX_BYTES:
            return self._json(413, {"message": "The payload is larger than %d MB." % (API_MAX_BYTES // (1024 * 1024))})
        body = self.rfile.read(length)
        try:
            obj = json.loads(body.decode("utf-8"))
        except (ValueError, UnicodeDecodeError):
            return self._json(400, {"message": "The payload is not valid JSON."})
        if not isinstance(obj, dict):
            return self._json(400, {"message": "The payload must be a JSON object."})
        if not any(k in obj for k in ("edges", "files", "network", "nodes", "session", "views")):
            return self._json(400, {"message": 'Nothing to show: give "edges", "files", "network" or a session.'})
        token = api_put(body)
        url = self._base_url() + "norma.html?session=" + token
        return self._json(200, {"token": token, "url": url, "expiresInHours": API_TTL_HOURS})

    def _api_get(self):
        bare = self.path.split("?")[0]
        if bare == "/api/health":
            with _api_lock:
                count = len(_api_store)
            return self._json(200, {"status": "ok", "api": API_VERSION, "version": APP_VERSION, "mode": CONFIG["mode"],
                                    "stored": count, "expiresInHours": API_TTL_HOURS,
                                    "relays": CONFIG["relays"]})
        m = re.match(r"^/api/session/([A-Za-z0-9_-]{8,64})$", bare)
        if not m:
            return self._json(404, {"message": "Unknown API address."})
        data = api_get(m.group(1))
        if data is None:
            return self._json(404, {"message": "This link has expired or does not exist."})
        self.send_response(200)
        self.send_header("Content-Type", "application/json; charset=utf-8")
        self.send_header("Content-Length", str(len(data)))
        self.send_header("Cache-Control", "no-store")
        self.end_headers()
        self.wfile.write(data)

    # ---------- helpers ----------
    def _json(self, status, obj, relay=False):
        data = json.dumps(obj).encode("utf-8")
        self.send_response(status)
        if relay:
            self.send_header("X-Norma-Relay", "1")
        self.send_header("Content-Type", "application/json; charset=utf-8")
        self.send_header("Content-Length", str(len(data)))
        self.end_headers()
        self.wfile.write(data)

    def _text(self, status, text, relay=False):
        data = text.encode("utf-8")
        self.send_response(status)
        if relay:
            self.send_header("X-Norma-Relay", "1")
        self.send_header("Content-Type", "text/plain; charset=utf-8")
        self.send_header("Content-Length", str(len(data)))
        self.end_headers()
        self.wfile.write(data)

    def _relay_db(self, verb):
        query = urllib.parse.parse_qs(urllib.parse.urlsplit(self.path).query)
        target = (query.get("url") or [""])[0]
        allowed = any(p.match(target) for p in ALLOWED_DB) or (TEST_DB and target.startswith(TEST_DB))
        if not allowed:
            return self._json(400, {"message": "This address is not one of the database importers' services.", "norma_proxy": True}, relay=True)
        body = None
        headers = {"User-Agent": USER_AGENT, "Accept": self.headers.get("Accept", "application/json")}
        if verb == "POST":
            length = int(self.headers.get("Content-Length") or 0)
            if length > MAX_DB_BODY:
                return self._json(413, {"message": "The request is too large.", "norma_proxy": True}, relay=True)
            body = self.rfile.read(length)
            headers["Content-Type"] = self.headers.get("Content-Type", "application/json")
        req = urllib.request.Request(target, data=body, method=verb, headers=headers)
        _wait_turn_short()
        try:
            with urllib.request.urlopen(req, timeout=TIMEOUT) as resp:
                data = resp.read()
                status = resp.status
                ctype = resp.headers.get("Content-Type", "application/json")
        except urllib.error.HTTPError as err:
            data = err.read()
            status = err.code
            ctype = err.headers.get("Content-Type", "text/plain")
        except (urllib.error.URLError, TimeoutError, OSError) as err:
            reason = getattr(err, "reason", err)
            return self._text(502, "The database could not be reached from this server (%s)." % reason, relay=True)
        self.send_response(status)
        self.send_header("Content-Type", ctype)
        self.send_header("Content-Length", str(len(data)))
        self.send_header("Cache-Control", "no-store")
        self.send_header("X-Norma-Relay", "1")
        self.end_headers()
        self.wfile.write(data)

    def _relay_arena(self):
        parsed = urllib.parse.urlsplit(self.path)
        query = urllib.parse.parse_qs(parsed.query)
        upstream = (query.get("upstream", [ARENA_DEFAULT])[0] or ARENA_DEFAULT).rstrip("/")
        if not (ALLOWED_ARENA.match(upstream) or (TEST_ARENA and upstream == TEST_ARENA)):
            return self._json(400, {"message": "Only arena3d.org addresses can be used.", "norma_proxy": True})
        length = int(self.headers.get("Content-Length") or 0)
        if length > MAX_ARENA_BODY:
            return self._json(413, {"message": "The network is too large to send.", "norma_proxy": True})
        body = self.rfile.read(length)
        try:
            json.loads(body.decode("utf-8"))
        except ValueError:
            return self._json(400, {"message": "The request body is not JSON.", "norma_proxy": True})
        req = urllib.request.Request(upstream + "/api/external", data=body, method="POST", headers={
            "Content-Type": "application/json",
            "User-Agent": USER_AGENT,
            "Accept": "application/json",
        })
        try:
            with urllib.request.urlopen(req, timeout=TIMEOUT) as resp:
                data = resp.read()
                status = resp.status
                ctype = resp.headers.get("Content-Type", "application/json")
        except urllib.error.HTTPError as err:
            data = err.read()
            status = err.code
            ctype = err.headers.get("Content-Type", "text/plain")
        except (urllib.error.URLError, TimeoutError, OSError) as err:
            reason = getattr(err, "reason", err)
            return self._text(502, "Arena3D could not be reached from this server (%s)." % reason)
        self.send_response(status)
        self.send_header("Content-Type", ctype)
        self.send_header("Content-Length", str(len(data)))
        self.send_header("Cache-Control", "no-store")
        self.end_headers()
        self.wfile.write(data)

    def _relay(self, verb):
        parsed = urllib.parse.urlsplit(self.path)
        parts = parsed.path.split("/")          # ['', 'string-api', fmt, method]
        if len(parts) != 4 or parts[2] not in ALLOWED_FORMATS or parts[3] not in ALLOWED_METHODS:
            return self._json(404, {"message": "Unknown STRING API call.", "norma_proxy": True})
        fmt, method = parts[2], parts[3]
        query = urllib.parse.parse_qs(parsed.query)
        upstream = (query.pop("upstream", [DEFAULT_UPSTREAM])[0] or DEFAULT_UPSTREAM).rstrip("/")
        if not (ALLOWED_UPSTREAM.match(upstream) or (TEST_UPSTREAM and upstream == TEST_UPSTREAM)):
            return self._json(400, {"message": "Only string-db.org addresses can be used."})

        body = b""
        if verb == "POST":
            length = int(self.headers.get("Content-Length") or 0)
            if length > MAX_BODY:
                return self._json(413, {"message": "The request is too large."})
            body = self.rfile.read(length)
        # make sure STRING knows who is calling
        form = urllib.parse.parse_qs(body.decode("utf-8"), keep_blank_values=True)
        for k, v in query.items():
            form.setdefault(k, v)
        form.setdefault("caller_identity", ["NORMA3"])
        payload = urllib.parse.urlencode(form, doseq=True).encode("utf-8")

        target = "%s/api/%s/%s" % (upstream, fmt, method)
        req = urllib.request.Request(target, data=payload, method="POST", headers={
            "Content-Type": "application/x-www-form-urlencoded; charset=UTF-8",
            "User-Agent": USER_AGENT,
            "Accept": "application/json, text/plain, */*",
        })
        _wait_turn()
        try:
            with urllib.request.urlopen(req, timeout=TIMEOUT) as resp:
                data = resp.read()
                status = resp.status
                ctype = resp.headers.get("Content-Type", "application/json")
        except urllib.error.HTTPError as err:
            data = err.read()
            status = err.code
            ctype = err.headers.get("Content-Type", "text/plain")
        except (urllib.error.URLError, TimeoutError, OSError) as err:
            reason = getattr(err, "reason", err)
            return self._text(502, "STRING could not be reached from this server (%s)." % reason)
        self.send_response(status)
        self.send_header("Content-Type", ctype)
        self.send_header("Content-Length", str(len(data)))
        self.send_header("Cache-Control", "no-store")
        self.end_headers()
        self.wfile.write(data)


class ThreadingServer(socketserver.ThreadingMixIn, http.server.HTTPServer):
    daemon_threads = True
    allow_reuse_address = True


class RedirectHandler(http.server.BaseHTTPRequestHandler):
    """Sends plain-HTTP visitors to the HTTPS address."""
    target = ""

    def do_GET(self):
        self.send_response(301)
        self.send_header("Location", self.target.rstrip("/") + self.path)
        self.end_headers()

    do_HEAD = do_GET
    do_POST = do_GET

    def log_message(self, fmt, *args):
        pass


def main():
    ap = argparse.ArgumentParser(description="Serve NORMA 3.0 locally or as a public web server.")
    ap.add_argument("--config", help="settings file (JSON); default norma.config.json next to server.py")
    ap.add_argument("--mode", choices=("local", "hosted"), help="local (this computer) or hosted (public web server)")
    ap.add_argument("--host", help="address to listen on (local default 127.0.0.1, hosted default 0.0.0.0)")
    ap.add_argument("--port", type=int, help="port (default 8000; 443 for HTTPS without a proxy)")
    ap.add_argument("--public-url", help="the address users see, e.g. https://norma.example.org/ (used in API links)")
    ap.add_argument("--tls-cert", help="certificate (PEM) to serve HTTPS directly")
    ap.add_argument("--tls-key", help="private key (PEM) for --tls-cert")
    ap.add_argument("--http-redirect-port", type=int, help="also listen on this port and redirect HTTP to HTTPS (e.g. 80)")
    ap.add_argument("--access-log", choices=("full", "anonymous", "off"), help="request log (hosted default: anonymous, without IP addresses)")
    ap.add_argument("--no-api", action="store_true", help="turn off the REST API")
    ap.add_argument("--no-relays", action="store_true", help="turn off the STRING, database and Arena3D relays")
    ap.add_argument("--open", action="store_true", help="open NORMA in the browser")
    ap.add_argument("--no-open", action="store_true", help="don't open the browser (local mode opens it by default)")
    ap.add_argument("--print-config", action="store_true", help="print the effective settings and exit")
    args = ap.parse_args()

    global CONFIG, API_TTL_HOURS, API_MAX_BYTES, API_MAX_SESSIONS
    CONFIG = load_config(args)
    API_TTL_HOURS = float(CONFIG["api"]["ttlHours"])
    API_MAX_BYTES = int(float(CONFIG["api"]["maxMB"]) * 1024 * 1024)
    API_MAX_SESSIONS = int(CONFIG["api"]["maxSessions"])
    if args.print_config:
        print(json.dumps(CONFIG, indent=2))
        return
    srv = CONFIG["server"]
    NormaHandler.extensions_map.update({".js": "text/javascript", ".mjs": "text/javascript", ".svg": "image/svg+xml", ".json": "application/json"})
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
        url = srv["publicUrl"] or "%s://%s%s/" % (scheme, shown, "" if default_port else ":%d" % port)
        if srv["httpRedirectPort"]:
            RedirectHandler.target = url
            redirect = ThreadingServer((srv["host"], int(srv["httpRedirectPort"])), RedirectHandler)
            threading.Thread(target=redirect.serve_forever, daemon=True).start()
        on = [k for k, v in CONFIG["relays"].items() if v]
        print("NORMA %s (%s mode) is running at %s" % (APP_VERSION, CONFIG["mode"], url))
        print("  REST API: %s" % ("%sapi/external" % url if CONFIG["api"]["enabled"] else "off"))
        print("  relays:   %s" % (", ".join(on) if on else "off"))
        print("  settings: %s" % CONFIG.get("_file", "built-in defaults"))
        if CONFIG["mode"] == "hosted" and not tls and not srv["trustProxy"]:
            print("  note: public servers should use HTTPS (TLS options or a reverse proxy); see README.")
        print("Press Ctrl+C to stop.")
        if srv["openBrowser"]:
            import webbrowser
            webbrowser.open(url)
        try:
            httpd.serve_forever()
        except KeyboardInterrupt:
            print("\nStopped.")


if __name__ == "__main__":
    main()
