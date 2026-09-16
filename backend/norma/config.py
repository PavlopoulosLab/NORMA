"""Settings: built-in defaults < JSON file < NORMA_* environment < command line."""

from __future__ import annotations

import argparse
import copy
import json
import os
import sys
from typing import Any

from norma import APP_VERSION

# repository root: backend/../ — where norma.config.json and frontend/ live
ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

Config = dict[str, Any]

DEFAULTS: Config = {
    "mode": "local",  # "local" or "hosted"
    "server": {
        "host": None,  # local: 127.0.0.1, hosted: 0.0.0.0
        "port": 8000,
        "publicUrl": "",  # e.g. https://norma.example.org/ (used in API links)
        "trustProxy": None,  # read X-Forwarded-* headers (hosted: true)
        "tlsCert": "",  # PEM files to serve HTTPS directly
        "tlsKey": "",
        "httpRedirectPort": 0,  # also listen here and redirect to HTTPS (e.g. 80)
        "hsts": True,  # Strict-Transport-Security when served over HTTPS
        "accessLog": None,  # "full", "anonymous" (no IP addresses) or "off"
        "openBrowser": None,  # local: true
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
    "app": {"maxNodes": 10000, "theme": "white", "startTab": "welcome", "cdnFallback": True},
}

ENV_MAP: dict[str, tuple[str, ...]] = {
    "NORMA_MODE": ("mode",),
    "NORMA_HOST": ("server", "host"),
    "NORMA_PORT": ("server", "port"),
    "NORMA_PUBLIC_URL": ("server", "publicUrl"),
    "NORMA_TLS_CERT": ("server", "tlsCert"),
    "NORMA_TLS_KEY": ("server", "tlsKey"),
    "NORMA_ACCESS_LOG": ("server", "accessLog"),
    "NORMA_TRUST_PROXY": ("server", "trustProxy"),
    "NORMA_API": ("api", "enabled"),
    "NORMA_API_TTL_HOURS": ("api", "ttlHours"),
    "NORMA_API_MAX_MB": ("api", "maxMB"),
    "NORMA_RELAYS": ("relays", "*"),
    "NORMA_CONTACT_EMAIL": ("site", "contactEmail"),
    "NORMA_NOTICE": ("site", "notice"),
    "NORMA_INSTITUTION": ("site", "institution"),
    "NORMA_MAX_NODES": ("app", "maxNodes"),
}

# the effective settings; replaced by main() and read by the handler at request time
CONFIG: Config = copy.deepcopy(DEFAULTS)


def _merge(base: Config, extra: Config | None) -> Config:
    for k, v in (extra or {}).items():
        if isinstance(v, dict) and isinstance(base.get(k), dict):
            _merge(base[k], v)
        else:
            base[k] = v
    return base


_BOOL_WORDS = ("true", "false", "1", "0", "yes", "no", "on", "off")


def _typed(template: Any, value: str) -> Any:
    if isinstance(template, bool) or template is None and value.lower() in _BOOL_WORDS:
        return value.lower() in ("1", "true", "yes", "on")
    if isinstance(template, int) and not isinstance(template, bool):
        return int(value)
    if isinstance(template, float):
        return float(value)
    return value


def _set(cfg: Config, path: tuple[str, ...], value: str) -> None:
    if path[-1] == "*":
        for k in cfg[path[0]]:
            cfg[path[0]][k] = _typed(True, value)
        return
    node = cfg
    for k in path[:-1]:
        node = node[k]
    node[path[-1]] = _typed(node.get(path[-1]), value)


def load_config(args: argparse.Namespace) -> Config:
    cfg = copy.deepcopy(DEFAULTS)
    path = args.config or os.environ.get("NORMA_CONFIG") or os.path.join(ROOT, "norma.config.json")
    if os.path.isfile(path):
        with open(path, encoding="utf-8") as fh:
            _merge(cfg, json.load(fh))
        cfg["_file"] = path
    elif args.config:
        sys.exit(f"Settings file not found: {args.config}")
    for env, target in ENV_MAP.items():
        if os.environ.get(env, "") != "":
            _set(cfg, target, os.environ[env])
    cli: dict[tuple[str, ...], Any] = {
        ("mode",): args.mode,
        ("server", "host"): args.host,
        ("server", "port"): args.port,
        ("server", "publicUrl"): args.public_url,
        ("server", "tlsCert"): args.tls_cert,
        ("server", "tlsKey"): args.tls_key,
        ("server", "httpRedirectPort"): args.http_redirect_port,
        ("server", "accessLog"): args.access_log,
    }
    for key, value in cli.items():
        if value is None:
            continue
        if len(key) == 1:
            cfg[key[0]] = value
        else:
            cfg[key[0]][key[1]] = value
    if args.no_api:
        cfg["api"]["enabled"] = False
    if args.no_relays:
        cfg["relays"] = dict.fromkeys(cfg["relays"], False)
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


def client_config(cfg: Config) -> Config:
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
