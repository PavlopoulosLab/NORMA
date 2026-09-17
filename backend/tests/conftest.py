"""Fixtures: an in-process NORMA server plus a fake upstream (STRING / Arena3D / database)."""

import argparse
import copy
import http.server
import json
import os
import sys
import threading
import urllib.error
import urllib.request

import pytest

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))  # backend/
from norma import api, config, handler, relays, static  # noqa: E402


class _Echo(http.server.BaseHTTPRequestHandler):
    """Answers every request with a JSON description of what it received."""

    def _reply(self):
        length = int(self.headers.get("Content-Length") or 0)
        body = self.rfile.read(length) if length else b""
        if self.path.endswith("/fail"):
            data = b"upstream says no"
            self.send_response(503)
            self.send_header("Content-Type", "text/plain")
        else:
            data = json.dumps(
                {
                    "method": self.command,
                    "path": self.path,
                    "body": body.decode("utf-8", "replace"),
                    "content_type": self.headers.get("Content-Type", ""),
                    "user_agent": self.headers.get("User-Agent", ""),
                }
            ).encode()
            self.send_response(200)
            self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(data)))
        self.end_headers()
        self.wfile.write(data)

    do_GET = do_POST = _reply

    def log_message(self, *a):
        pass


def _serve(cls):
    httpd = handler.ThreadingServer(("127.0.0.1", 0), cls)
    # short poll interval: shutdown() waits one interval per test
    threading.Thread(
        target=httpd.serve_forever, kwargs={"poll_interval": 0.02}, daemon=True
    ).start()
    return httpd, f"http://127.0.0.1:{httpd.server_address[1]}"


def make_args(**kw):
    """argparse namespace with every server.py option, as `main()` would build it."""
    base = dict(
        config=None,
        mode=None,
        host=None,
        port=None,
        public_url=None,
        tls_cert=None,
        tls_key=None,
        http_redirect_port=None,
        access_log=None,
        no_api=False,
        no_relays=False,
        open=False,
        no_open=False,
        print_config=False,
    )
    base.update(kw)
    return argparse.Namespace(**base)


@pytest.fixture(scope="session")
def upstream():
    httpd, url = _serve(_Echo)
    yield url
    httpd.shutdown()


@pytest.fixture
def norma(upstream, tmp_path, monkeypatch):
    """A running NORMA server; returns a small client. Config is reset per test."""
    monkeypatch.setattr(config, "CONFIG", copy.deepcopy(config.DEFAULTS))
    config.CONFIG["server"]["accessLog"] = "off"
    monkeypatch.setattr(relays, "TEST_UPSTREAM", upstream)
    monkeypatch.setattr(relays, "TEST_ARENA", upstream)
    monkeypatch.setattr(relays, "TEST_DB", upstream + "/db/")
    monkeypatch.setattr(relays, "MIN_GAP", 0.0)
    monkeypatch.setattr(api, "_store", {})
    monkeypatch.setattr(static, "STATIC_DIR", str(tmp_path))
    (tmp_path / "norma.html").write_text("<html>NORMA</html>")
    (tmp_path / "Dockerfile").write_text("FROM x")
    (tmp_path / ".secret").write_text("x")
    (tmp_path / "deploy").mkdir()
    (tmp_path / "deploy" / "norma.service").write_text("x")
    (tmp_path / "assets").mkdir()
    (tmp_path / "assets" / "logo.svg").write_text("<svg/>")
    httpd, url = _serve(handler.NormaHandler)
    yield Client(url)
    httpd.shutdown()


class Client:
    def __init__(self, base):
        self.base = base

    def request(self, path, method="GET", body=None, headers=None):
        h = dict(headers or {})
        data = body
        if isinstance(body, (dict, list)):
            data = json.dumps(body).encode()
            h.setdefault("Content-Type", "application/json")
        req = urllib.request.Request(self.base + path, data=data, method=method, headers=h)
        try:
            with urllib.request.urlopen(req, timeout=10) as r:
                return r.status, dict(r.headers), r.read()
        except urllib.error.HTTPError as e:
            return e.code, dict(e.headers), e.read()

    def json(self, path, **kw):
        status, headers, body = self.request(path, **kw)
        return status, headers, json.loads(body or b"null")
