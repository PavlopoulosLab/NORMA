"""Static files: the built page (frontend/dist) and what must never be served."""

from __future__ import annotations

import os

from norma.config import ROOT


def _default_root() -> str:
    for candidate in (os.path.join(ROOT, "frontend", "dist"), ROOT):
        if os.path.isfile(os.path.join(candidate, "norma.html")):
            return candidate
    return os.path.join(ROOT, "frontend", "dist")


# where norma.html and its assets are served from (NORMA_STATIC_DIR overrides)
STATIC_DIR = os.environ.get("NORMA_STATIC_DIR") or _default_root()

# Deployment/VCS files may sit next to the page; keep them from being handed out.
_BLOCKED_NAMES = {"dockerfile", "docker-compose.yml", "docker-compose.yaml"}
_BLOCKED_DIRS = {"deploy"}


def blocked(path: str) -> bool:
    parts = [p for p in path.split("?", 1)[0].split("/") if p]
    if any(p.startswith(".") for p in parts):
        return True
    if parts and parts[0].lower() in _BLOCKED_DIRS:
        return True
    return bool(parts) and (parts[-1].lower() in _BLOCKED_NAMES or parts[-1].endswith(".service"))
