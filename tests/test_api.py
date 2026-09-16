import time

import server


def test_health(norma):
    status, h, obj = norma.json("/api/health")
    assert status == 200
    assert obj["status"] == "ok" and obj["api"] == server.API_VERSION and obj["version"] == server.APP_VERSION
    assert obj["stored"] == 0 and obj["relays"] == server.CONFIG["relays"]
    assert h["Access-Control-Allow-Origin"] == "*"


def test_external_round_trip(norma):
    payload = {"name": "Demo", "edges": [{"source": "A", "target": "B"}], "groups": {"G1": ["A", "B"]}}
    status, _, obj = norma.json("/api/external", method="POST", body=payload)
    assert status == 200
    assert obj["url"] == norma.base + "/norma.html?session=" + obj["token"]
    assert obj["expiresInHours"] == server.API_TTL_HOURS
    status, h, back = norma.json("/api/session/" + obj["token"])
    assert status == 200 and back == payload and h["Cache-Control"] == "no-store"
    assert norma.json("/api/health")[2]["stored"] == 1


def test_external_url_uses_public_url_or_proxy_headers(norma):
    server.CONFIG["server"]["trustProxy"] = True
    _, _, obj = norma.json(
        "/api/external", method="POST", body={"edges": []},
        headers={"X-Forwarded-Host": "norma.org", "X-Forwarded-Proto": "https", "X-Forwarded-Prefix": "/app/"},
    )
    assert obj["url"].startswith("https://norma.org/app/norma.html?session=")
    server.CONFIG["server"]["publicUrl"] = "https://fixed.org/"
    _, _, obj = norma.json("/api/external", method="POST", body={"edges": []})
    assert obj["url"].startswith("https://fixed.org/norma.html?session=")


def test_external_rejects_bad_payloads(norma):
    assert norma.json("/api/external", method="POST", body=b"", headers={"Content-Length": "0"})[0] == 400
    assert norma.json("/api/external", method="POST", body=b"{not json", headers={"Content-Type": "application/json"})[0] == 400
    assert norma.json("/api/external", method="POST", body=[1, 2])[0] == 400
    assert norma.json("/api/external", method="POST", body={"foo": 1})[0] == 400


def test_external_size_limit(norma, monkeypatch):
    monkeypatch.setattr(server, "API_MAX_BYTES", 100)
    status, _, obj = norma.json("/api/external", method="POST", body={"edges": ["x" * 200]})
    assert status == 413


def test_session_unknown_and_bad_token(norma):
    assert norma.json("/api/session/" + "a" * 20)[0] == 404
    assert norma.json("/api/session/short")[0] == 404
    assert norma.json("/api/nothing")[0] == 404


def test_expiry_and_session_cap(monkeypatch):
    monkeypatch.setattr(server, "_api_store", {})
    monkeypatch.setattr(server, "API_MAX_SESSIONS", 2)
    t1 = server.api_put(b"1")
    t2 = server.api_put(b"2")
    t3 = server.api_put(b"3")
    assert server.api_get(t1) is None and server.api_get(t2) == b"2" and server.api_get(t3) == b"3"
    monkeypatch.setattr(server, "API_TTL_HOURS", 0.0)
    time.sleep(0.01)
    assert server.api_get(t3) is None


def test_api_disabled(norma):
    server.CONFIG["api"]["enabled"] = False
    assert norma.json("/api/health")[0] == 404
    assert norma.json("/api/external", method="POST", body={"edges": []})[0] == 404
