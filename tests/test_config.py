import json
import os

import pytest

import server
from conftest import make_args


def test_defaults_local(monkeypatch, tmp_path):
    monkeypatch.setattr(server, "APP_DIR", str(tmp_path))
    cfg = server.load_config(make_args())
    assert cfg["mode"] == "local"
    assert cfg["server"]["host"] == "127.0.0.1"
    assert cfg["server"]["trustProxy"] is False
    assert cfg["server"]["accessLog"] == "off"
    assert cfg["server"]["openBrowser"] is True
    assert "_file" not in cfg


def test_hosted_defaults(monkeypatch, tmp_path):
    monkeypatch.setattr(server, "APP_DIR", str(tmp_path))
    cfg = server.load_config(make_args(mode="hosted"))
    assert cfg["server"]["host"] == "0.0.0.0"
    assert cfg["server"]["trustProxy"] is True
    assert cfg["server"]["accessLog"] == "anonymous"
    assert cfg["server"]["openBrowser"] is False


def test_precedence_file_env_cli(monkeypatch, tmp_path):
    f = tmp_path / "c.json"
    f.write_text(json.dumps({"mode": "hosted", "server": {"port": 9000, "publicUrl": "https://x.org"}, "site": {"name": "X"}}))
    monkeypatch.setenv("NORMA_PORT", "9100")
    monkeypatch.setenv("NORMA_MAX_NODES", "10")
    monkeypatch.setenv("NORMA_API", "off")
    monkeypatch.setenv("NORMA_RELAYS", "false")
    cfg = server.load_config(make_args(config=str(f), port=9200))
    assert cfg["server"]["port"] == 9200  # cli > env > file
    assert cfg["server"]["publicUrl"] == "https://x.org/"  # trailing slash added
    assert cfg["site"]["name"] == "X" and cfg["site"]["contactName"] == "Pavlopoulos Lab"  # deep merge
    assert cfg["app"]["maxNodes"] == 10  # env typed as int
    assert cfg["api"]["enabled"] is False
    assert cfg["relays"] == {"string": False, "arena3d": False, "databases": False}
    assert cfg["_file"] == str(f)


def test_cli_switches(monkeypatch, tmp_path):
    monkeypatch.setattr(server, "APP_DIR", str(tmp_path))
    cfg = server.load_config(make_args(no_api=True, no_relays=True, no_open=True, access_log="full"))
    assert cfg["api"]["enabled"] is False
    assert not any(cfg["relays"].values())
    assert cfg["server"]["openBrowser"] is False
    assert cfg["server"]["accessLog"] == "full"


def test_missing_config_file_exits():
    with pytest.raises(SystemExit):
        server.load_config(make_args(config="/nonexistent.json"))


def test_bad_mode_exits(monkeypatch, tmp_path):
    monkeypatch.setattr(server, "APP_DIR", str(tmp_path))
    monkeypatch.setenv("NORMA_MODE", "weird")
    with pytest.raises(SystemExit):
        server.load_config(make_args())


def test_env_config_path(monkeypatch, tmp_path):
    f = tmp_path / "e.json"
    f.write_text('{"app": {"theme": "dark"}}')
    monkeypatch.setenv("NORMA_CONFIG", str(f))
    cfg = server.load_config(make_args())
    assert cfg["app"]["theme"] == "dark"


def test_client_config_has_no_server_section():
    cfg = server.load_config(make_args(mode="hosted", public_url="https://n.org"))
    cc = server.client_config(cfg)
    assert set(cc) == {"mode", "version", "publicUrl", "features", "site", "app"}
    assert cc["features"]["restApi"] is True
    assert cc["features"]["relays"] == cfg["relays"]
    assert cc["publicUrl"] == "https://n.org/"


def test_repo_config_files_load():
    root = os.path.dirname(server.__file__)
    for name in ("norma.config.json", "norma.config.hosted.json"):
        cfg = server.load_config(make_args(config=os.path.join(root, name)))
        assert cfg["mode"] in ("local", "hosted")
