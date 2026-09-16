from norma import config, relays


def test_string_ping(norma):
    status, _, obj = norma.json("/string-api/ping")
    assert (
        status == 200 and obj["norma_proxy"] is True and obj["upstream"] == relays.DEFAULT_UPSTREAM
    )


def test_string_relay_get_becomes_upstream_post(norma, upstream):
    status, h, obj = norma.json(
        "/string-api/json/network?identifiers=TP53&species=9606&upstream=" + upstream
    )
    assert status == 200 and h["Cache-Control"] == "no-store"
    assert obj["method"] == "POST" and obj["path"] == "/api/json/network"
    assert "identifiers=TP53" in obj["body"] and "caller_identity=NORMA3" in obj["body"]
    assert obj["user_agent"] == relays.USER_AGENT


def test_string_relay_post_merges_query_into_form(norma, upstream):
    status, _, obj = norma.json(
        "/string-api/tsv/get_string_ids?species=9606&upstream=" + upstream,
        method="POST",
        body=b"identifiers=A%0dB",
        headers={"Content-Type": "application/x-www-form-urlencoded"},
    )
    assert status == 200 and obj["path"] == "/api/tsv/get_string_ids"
    assert "identifiers=A%0DB" in obj["body"] and "species=9606" in obj["body"]


def test_string_relay_rejects_unknown_method_format_and_host(norma, upstream):
    assert norma.json("/string-api/json/delete_everything")[0] == 404
    assert norma.json("/string-api/csv/network")[0] == 404
    assert norma.json("/string-api/json/network/extra")[0] == 404
    status, _, obj = norma.json("/string-api/json/network?upstream=https://evil.org")
    assert status == 400 and "string-db.org" in obj["message"]
    assert norma.json("/string-api/json/network?upstream=http://string-db.org")[0] == 400
    assert norma.request("/string-api/json/network", method="HEAD")[0] == 405


def test_string_allowed_upstream_regex():
    ok = ("https://string-db.org", "https://version-12-0.string-db.org", "https://STRING-DB.org")
    bad = (
        "https://string-db.org.evil.com",
        "https://notstring-db.org",
        "http://string-db.org",
        "https://string-db.orgx",
    )
    assert all(relays.ALLOWED_UPSTREAM.match(u) for u in ok)
    assert not any(relays.ALLOWED_UPSTREAM.match(u) for u in bad)


def test_string_relay_body_limit(norma, upstream, monkeypatch):
    monkeypatch.setattr(relays, "MAX_BODY", 10)
    status, _, _ = norma.json(
        "/string-api/json/network?upstream=" + upstream,
        method="POST",
        body=b"identifiers=" + b"x" * 50,
    )
    assert status == 413


def test_relay_unreachable_upstream_502(norma, monkeypatch):
    monkeypatch.setattr(relays, "TEST_UPSTREAM", "http://127.0.0.1:1")
    status, _, body = norma.request("/string-api/json/network?upstream=http://127.0.0.1:1")
    assert status == 502 and b"could not be reached" in body


def test_string_relay_off(norma):
    config.CONFIG["relays"]["string"] = False
    assert norma.json("/string-api/json/network")[0] == 404
    assert norma.json("/string-api/json/network", method="POST", body=b"")[0] == 404


def test_arena_relay(norma, upstream):
    payload = {"name": "n", "layers": []}
    status, _, obj = norma.json(
        "/arena3d-api/external?upstream=" + upstream, method="POST", body=payload
    )
    assert status == 200 and obj["method"] == "POST" and obj["path"] == "/api/external"
    assert obj["content_type"] == "application/json" and '"layers"' in obj["body"]


def test_arena_relay_validation(norma, upstream, monkeypatch):
    assert (
        norma.json("/arena3d-api/external?upstream=https://evil.org", method="POST", body={})[0]
        == 400
    )
    status, _, _ = norma.request(
        "/arena3d-api/external?upstream=" + upstream,
        method="POST",
        body=b"nope",
        headers={"Content-Type": "application/json"},
    )
    assert status == 400
    monkeypatch.setattr(relays, "MAX_ARENA_BODY", 5)
    assert (
        norma.json("/arena3d-api/external?upstream=" + upstream, method="POST", body={"a": "long"})[
            0
        ]
        == 413
    )
    config.CONFIG["relays"]["arena3d"] = False
    assert norma.json("/arena3d-api/external", method="POST", body={})[0] == 404


def test_arena_allowed_regex():
    assert relays.ALLOWED_ARENA.match("https://arena3d.org")
    assert relays.ALLOWED_ARENA.match("https://beta.arena3d.org")
    assert not relays.ALLOWED_ARENA.match("https://arena3d.org.evil.com")


def test_db_relay_get_and_post(norma, upstream):
    target = upstream + "/db/pathway?id=1"
    status, h, obj = norma.json(
        "/db-api/fetch?url=" + target.replace("&", "%26").replace("?", "%3F")
    )
    assert status == 200 and h["X-Norma-Relay"] == "1"
    assert obj["method"] == "GET" and obj["path"] == "/db/pathway?id=1"
    status, _, obj = norma.json(
        "/db-api/fetch?url=" + upstream + "/db/q", method="POST", body={"q": 1}
    )
    assert obj["method"] == "POST" and obj["body"] == '{"q": 1}'


def test_db_relay_allowlist(norma, monkeypatch):
    status, h, obj = norma.json("/db-api/fetch?url=https://evil.org/x")
    assert status == 400 and obj["norma_proxy"] is True and h["X-Norma-Relay"] == "1"
    for good in (
        "https://reactome.org/ContentService/data/x",
        "https://omnipathdb.org/interactions",
        "https://www.ndexbio.org/v2/network/x",
        "https://public.ndexbio.org/v3/x",
        "https://www.ebi.ac.uk/Tools/webservices/psicquic/x",
        "https://www.ebi.ac.uk/QuickGO/services/x",
        "https://api.geneontology.org/api/x",
    ):
        assert any(p.match(good) for p in relays.ALLOWED_DB), good
    assert not any(p.match("https://reactome.org.evil/ContentService/") for p in relays.ALLOWED_DB)
    monkeypatch.setattr(relays, "MAX_DB_BODY", 3)
    assert (
        norma.json("/db-api/fetch?url=https://omnipathdb.org/x", method="POST", body={"big": "x"})[
            0
        ]
        == 413
    )
    config.CONFIG["relays"]["databases"] = False
    assert norma.json("/db-api/fetch?url=https://omnipathdb.org/x")[0] == 404


def test_db_relay_passes_upstream_status(norma, upstream):
    status, _, body = norma.request("/db-api/fetch?url=" + upstream + "/db/fail")
    assert status == 503 and body == b"upstream says no"
