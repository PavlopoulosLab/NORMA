"""
Relays: the browser only talks to this server, which passes read-only requests on.

    GET/POST /string-api/<format>/<method>?upstream=<STRING address>
    POST     /arena3d-api/external?upstream=<Arena3D address>
    GET/POST /db-api/fetch?url=<address>

Each relay has a strict allowlist of upstream addresses and a body limit.
"""

from __future__ import annotations

import json
import os
import re
import threading
import time
import urllib.error
import urllib.parse
import urllib.request
from email.message import Message
from typing import Protocol

USER_AGENT = "NORMA3-local-server"
TIMEOUT = 120  # seconds to wait for an upstream

# STRING
DEFAULT_UPSTREAM = "https://string-db.org"
ALLOWED_UPSTREAM = re.compile(r"^https://([a-z0-9-]+\.)*string-db\.org$", re.I)
ALLOWED_FORMATS = {"json", "tsv", "tsv-no-header", "xml"}
ALLOWED_METHODS = {
    "version",
    "get_string_ids",
    "network",
    "interaction_partners",
    "enrichment",
    "functional_annotation",
    "ppi_enrichment",
    "homology",
    "homology_best",
    "functional_terms",
    "geneset_description",
}
MAX_BODY = 5 * 1024 * 1024  # 5 MB of identifiers is plenty
MIN_GAP = 1.0  # seconds between STRING requests, as STRING asks

# Arena3D
ARENA_DEFAULT = "https://arena3d.org"
ALLOWED_ARENA = re.compile(r"^https://([a-z0-9-]+\.)*arena3d\.org$", re.I)
MAX_ARENA_BODY = 50 * 1024 * 1024  # networks can be large

# database importers
ALLOWED_DB = [
    re.compile(p)
    for p in (
        r"^https://reactome\.org/ContentService/",
        r"^https://omnipathdb\.org/",
        r"^https://(www|public)\.ndexbio\.org/v[23]/",
        r"^https://www\.ebi\.ac\.uk/Tools/webservices/psicquic/",
        r"^https://www\.ebi\.ac\.uk/QuickGO/services/",
        r"^https://api\.geneontology\.org/api/",
    )
]
MAX_DB_BODY = 1024 * 1024

# for automated tests only: allow local mocks of the upstreams
TEST_UPSTREAM = os.environ.get("NORMA_STRING_TEST_UPSTREAM", "")
TEST_ARENA = os.environ.get("NORMA_ARENA_TEST_UPSTREAM", "")
TEST_DB = os.environ.get("NORMA_DB_TEST_PREFIX", "")

_gap_lock = threading.Lock()
_last_call = [0.0]
_short_lock = threading.Lock()
_short_last = [0.0]


def _wait_turn() -> None:
    with _gap_lock:
        wait = _last_call[0] + MIN_GAP - time.monotonic()
        if wait > 0:
            time.sleep(wait)
        _last_call[0] = time.monotonic()


def _wait_turn_short() -> None:
    # a light spacing for the database relay (several requests run at once)
    with _short_lock:
        wait = _short_last[0] + 0.05 - time.monotonic()
        if wait > 0:
            time.sleep(wait)
        _short_last[0] = time.monotonic()


class Responder(Protocol):
    """The part of the request handler the relays use."""

    path: str
    headers: Message

    def send_json(self, status: int, obj: object, relay: bool = False) -> None: ...
    def send_text(self, status: int, text: str, relay: bool = False) -> None: ...
    def send_bytes(
        self, status: int, data: bytes, ctype: str, relay: bool = False, no_store: bool = False
    ) -> None: ...
    def read_body(self, length: int) -> bytes: ...


def _forward(h: Responder, req: urllib.request.Request, who: str, relay: bool = False) -> None:
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
        h.send_text(502, f"{who} could not be reached from this server ({reason}).", relay)
        return
    h.send_bytes(status, data, ctype, relay, no_store=True)


def _length(h: Responder) -> int:
    return int(h.headers.get("Content-Length") or 0)


def string(h: Responder, verb: str) -> None:
    parsed = urllib.parse.urlsplit(h.path)
    parts = parsed.path.split("/")  # ['', 'string-api', fmt, method]
    if len(parts) != 4 or parts[2] not in ALLOWED_FORMATS or parts[3] not in ALLOWED_METHODS:
        return h.send_json(404, {"message": "Unknown STRING API call.", "norma_proxy": True})
    fmt, method = parts[2], parts[3]
    query = urllib.parse.parse_qs(parsed.query)
    upstream = (query.pop("upstream", [DEFAULT_UPSTREAM])[0] or DEFAULT_UPSTREAM).rstrip("/")
    if not (ALLOWED_UPSTREAM.match(upstream) or (TEST_UPSTREAM and upstream == TEST_UPSTREAM)):
        return h.send_json(400, {"message": "Only string-db.org addresses can be used."})

    body = b""
    if verb == "POST":
        length = _length(h)
        if length > MAX_BODY:
            return h.send_json(413, {"message": "The request is too large."})
        body = h.read_body(length)
    # make sure STRING knows who is calling
    form = urllib.parse.parse_qs(body.decode("utf-8"), keep_blank_values=True)
    for k, v in query.items():
        form.setdefault(k, v)
    form.setdefault("caller_identity", ["NORMA3"])
    payload = urllib.parse.urlencode(form, doseq=True).encode("utf-8")

    target = f"{upstream}/api/{fmt}/{method}"
    req = urllib.request.Request(
        target,
        data=payload,
        method="POST",
        headers={
            "Content-Type": "application/x-www-form-urlencoded; charset=UTF-8",
            "User-Agent": USER_AGENT,
            "Accept": "application/json, text/plain, */*",
        },
    )
    _wait_turn()
    _forward(h, req, "STRING")


def arena3d(h: Responder) -> None:
    parsed = urllib.parse.urlsplit(h.path)
    query = urllib.parse.parse_qs(parsed.query)
    upstream = (query.get("upstream", [ARENA_DEFAULT])[0] or ARENA_DEFAULT).rstrip("/")
    if not (ALLOWED_ARENA.match(upstream) or (TEST_ARENA and upstream == TEST_ARENA)):
        return h.send_json(
            400, {"message": "Only arena3d.org addresses can be used.", "norma_proxy": True}
        )
    length = _length(h)
    if length > MAX_ARENA_BODY:
        return h.send_json(
            413, {"message": "The network is too large to send.", "norma_proxy": True}
        )
    body = h.read_body(length)
    try:
        json.loads(body.decode("utf-8"))
    except ValueError:
        return h.send_json(400, {"message": "The request body is not JSON.", "norma_proxy": True})
    req = urllib.request.Request(
        upstream + "/api/external",
        data=body,
        method="POST",
        headers={
            "Content-Type": "application/json",
            "User-Agent": USER_AGENT,
            "Accept": "application/json",
        },
    )
    _forward(h, req, "Arena3D")


def database(h: Responder, verb: str) -> None:
    query = urllib.parse.parse_qs(urllib.parse.urlsplit(h.path).query)
    target = (query.get("url") or [""])[0]
    allowed = any(p.match(target) for p in ALLOWED_DB) or (TEST_DB and target.startswith(TEST_DB))
    if not allowed:
        return h.send_json(
            400,
            {
                "message": "This address is not one of the database importers' services.",
                "norma_proxy": True,
            },
            relay=True,
        )
    body = None
    headers = {"User-Agent": USER_AGENT, "Accept": h.headers.get("Accept", "application/json")}
    if verb == "POST":
        length = _length(h)
        if length > MAX_DB_BODY:
            return h.send_json(
                413, {"message": "The request is too large.", "norma_proxy": True}, relay=True
            )
        body = h.read_body(length)
        headers["Content-Type"] = h.headers.get("Content-Type", "application/json")
    req = urllib.request.Request(target, data=body, method=verb, headers=headers)
    _wait_turn_short()
    _forward(h, req, "The database", relay=True)
