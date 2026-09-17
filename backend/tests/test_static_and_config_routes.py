from norma import config, static


def test_root_redirects_to_norma_html_with_query(norma):
    status, headers, body = norma.request("/?example=trp")
    assert status == 200 and b"NORMA" in body
    status, _, body = norma.request("/index.html")
    assert status == 200 and b"NORMA" in body


def test_security_headers(norma):
    _, h, _ = norma.request("/norma.html")
    assert h["X-Content-Type-Options"] == "nosniff"
    assert h["Referrer-Policy"] == "strict-origin-when-cross-origin"
    assert "camera=()" in h["Permissions-Policy"]
    assert h["Cache-Control"] == "no-cache"
    assert "Strict-Transport-Security" not in h  # plain http


def test_hsts_behind_trusted_proxy(norma):
    config.CONFIG["server"]["trustProxy"] = True
    _, h, _ = norma.request("/norma.html", headers={"X-Forwarded-Proto": "https"})
    assert h["Strict-Transport-Security"] == "max-age=31536000"


def test_blocked_static_paths(norma):
    for p in ("/Dockerfile", "/deploy/norma.service", "/.secret", "/deploy/", "/assets/../.secret"):
        status, _, _ = norma.request(p)
        assert status == 404, p
        status, _, _ = norma.request(p, method="HEAD")
        assert status == 404, p
    status, _, _ = norma.request("/assets/logo.svg")
    assert status == 200


def test_static_blocked_helper():
    assert static.blocked("/docker-compose.yaml")
    assert static.blocked("/x/y/.git/config")
    assert static.blocked("/deploy/nginx.conf")
    assert static.blocked("/anything.service?x=1")
    assert not static.blocked("/vendor/cytoscape.min.js")
    assert not static.blocked("/norma.html?session=abc")


def test_client_config_js_and_json(norma):
    config.CONFIG["site"]["notice"] = "hello"
    status, h, body = norma.request("/norma-config.js")
    assert status == 200 and h["Content-Type"].startswith("text/javascript")
    assert (
        body.startswith(b"// generated")
        and b"window.NORMA_CONFIG = {" in body
        and b'"notice": "hello"' in body
    )
    status, h, obj = norma.json("/norma-config.json")
    assert h["Content-Type"].startswith("application/json")
    assert obj["site"]["notice"] == "hello" and "server" not in obj


def test_options(norma):
    status, h, _ = norma.request("/api/external", method="OPTIONS")
    assert status == 204 and h["Access-Control-Allow-Origin"] == "*"


def test_unknown_post_405(norma):
    status, _, obj = norma.json("/whatever", method="POST", body={})
    assert status == 405
