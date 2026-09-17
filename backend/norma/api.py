"""In-memory store for the NORMA API: POST /api/external -> token, GET /api/session/TOKEN."""

from __future__ import annotations

import secrets
import threading
import time

from norma.config import Config

MAX_BYTES = 50 * 1024 * 1024
MAX_SESSIONS = 500
TTL_HOURS = 24.0

_store: dict[str, tuple[float, bytes]] = {}  # token -> (created, payload)
_lock = threading.Lock()


def configure(cfg: Config) -> None:
    global TTL_HOURS, MAX_BYTES, MAX_SESSIONS
    TTL_HOURS = float(cfg["api"]["ttlHours"])
    MAX_BYTES = int(float(cfg["api"]["maxMB"]) * 1024 * 1024)
    MAX_SESSIONS = int(cfg["api"]["maxSessions"])


def put(data: bytes) -> str:
    token = secrets.token_urlsafe(18)
    now = time.time()
    with _lock:
        _expire(now)
        while len(_store) >= MAX_SESSIONS:
            oldest = min(_store, key=lambda k: _store[k][0])
            del _store[oldest]
        _store[token] = (now, data)
    return token


def get(token: str) -> bytes | None:
    with _lock:
        _expire(time.time())
        item = _store.get(token)
    return item[1] if item else None


def count() -> int:
    with _lock:
        return len(_store)


def _expire(now: float) -> None:
    limit = TTL_HOURS * 3600
    for k in [k for k, (t, _) in _store.items() if now - t > limit]:
        del _store[k]
