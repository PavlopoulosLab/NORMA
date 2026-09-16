#!/usr/bin/env python3
"""
NORMA API — template client
===========================

A working starting point for calling NORMA's web services from Python. It
uses only the standard library (Python 3.8+). It shows how to

  1. check a NORMA server           GET  /api/health
  2. send a network with groups     POST /api/external   -> {token, url}
  3. read a stored payload back     GET  /api/session/TOKEN
  4. make a link without a server   norma.html#json=...

and parses every answer.

    python3 norma_api_client.py                              # demo network, local server
    python3 norma_api_client.py --server https://norma.example.org/
    python3 norma_api_client.py --edges edges.tsv --groups groups.tsv --open
    python3 norma_api_client.py --link-only                  # no server needed

edges.tsv: one edge per line, "source<TAB>target[<TAB>type[<TAB>weight]]",
           an optional first line "source target ..." is skipped.
groups.tsv: one group per line, "group name<TAB>member1,member2,...".

Adapt build_payload() to your own data; the payload fields are documented in
NORMA's API tab and Help (API for other applications).
"""
import argparse
import base64
import json
import sys
import urllib.error
import urllib.request
import webbrowser

DEFAULT_SERVER = "http://localhost:8000/"


# ---------------------------------------------------------------- web services
def call(method, url, payload=None, timeout=60):
    """Sends a request and returns the parsed JSON answer (raises on errors)."""
    data = None
    headers = {"Accept": "application/json", "User-Agent": "norma-api-client/1.0"}
    if payload is not None:
        data = json.dumps(payload).encode("utf-8")
        headers["Content-Type"] = "application/json"
    req = urllib.request.Request(url, data=data, method=method, headers=headers)
    try:
        with urllib.request.urlopen(req, timeout=timeout) as resp:
            return json.loads(resp.read().decode("utf-8"))
    except urllib.error.HTTPError as err:
        body = err.read().decode("utf-8", "replace")
        try:
            message = json.loads(body).get("message", body)
        except ValueError:
            message = body[:200]
        raise RuntimeError("%s %s failed (%d): %s" % (method, url, err.code, message)) from None


def health(server):
    """GET /api/health -> dict with status, API version and server mode."""
    answer = call("GET", server + "api/health")
    if answer.get("status") != "ok":
        raise RuntimeError("the server is not ready: %r" % answer)
    return answer


def submit(server, payload):
    """POST /api/external -> (token, url, hours the link stays valid)."""
    answer = call("POST", server + "api/external", payload)
    return answer["token"], answer["url"], answer.get("expiresInHours")


def fetch_session(server, token):
    """GET /api/session/TOKEN -> the payload NORMA will show."""
    return call("GET", server + "api/session/" + token)


def make_link(base, payload):
    """A link that carries the payload itself (no server needed; small networks)."""
    raw = json.dumps(payload, separators=(",", ":")).encode("utf-8")
    code = base64.urlsafe_b64encode(raw).decode("ascii").rstrip("=")
    page = base if base.endswith(".html") else base.rstrip("/") + "/norma.html"
    return page + "#json=" + code


# ---------------------------------------------------------------- your data
def read_edges(path):
    edges = []
    with open(path, encoding="utf-8") as fh:
        for i, line in enumerate(fh):
            cols = line.rstrip("\n").split("\t")
            if len(cols) < 2 or (i == 0 and cols[0].lower() in ("source", "from", "node1")):
                continue
            edge = {"source": cols[0], "target": cols[1]}
            if len(cols) > 2 and cols[2]:
                edge["type"] = cols[2]
            if len(cols) > 3 and cols[3]:
                edge["weight"] = float(cols[3])
            edges.append(edge)
    return edges


def read_groups(path):
    groups = {}
    with open(path, encoding="utf-8") as fh:
        for line in fh:
            cols = line.rstrip("\n").split("\t")
            if len(cols) >= 2:
                groups[cols[0]] = [m for m in cols[1].split(",") if m]
    return groups


def build_payload(args):
    if args.edges:
        edges = read_edges(args.edges)
        groups = read_groups(args.groups) if args.groups else {}
    else:  # a small demo: the p53 network with three groups
        pairs = [("TP53", "MDM2", "binding"), ("ATM", "TP53", "phosphorylation"), ("ATM", "CHEK2", "phosphorylation"),
                 ("CHEK2", "TP53", "phosphorylation"), ("TP53", "CDKN1A", "expression"), ("CDKN1A", "CDK2", "inhibition"),
                 ("CDK2", "CCNE1", "binding")]
        edges = [{"source": s, "target": t, "type": k, "directed": k != "binding"} for s, t, k in pairs]
        groups = {"DNA damage": ["ATM", "CHEK2"], "p53 core": ["TP53", "MDM2"], "Cell cycle": ["CDKN1A", "CDK2", "CCNE1", "TP53"]}
    return {
        "name": args.name,
        "edges": edges,
        "groups": groups,
        "settings": {"layout": "fr", "showGroupHulls": True, "hullStyle": "bubble",
                     "edgeDirection": "data", "legendShow": True},
    }


# ---------------------------------------------------------------- main
def main():
    ap = argparse.ArgumentParser(description="Send a network to NORMA and open it.")
    ap.add_argument("--server", default=DEFAULT_SERVER, help="NORMA server address (default %s)" % DEFAULT_SERVER)
    ap.add_argument("--edges", help="tab-separated edge list")
    ap.add_argument("--groups", help="tab-separated groups (name, comma-separated members)")
    ap.add_argument("--name", default="API template network", help="name of the view")
    ap.add_argument("--open", action="store_true", help="open the result in the browser")
    ap.add_argument("--link-only", action="store_true", help="only build a #json link (no server calls)")
    args = ap.parse_args()
    server = args.server if args.server.endswith("/") else args.server + "/"

    payload = build_payload(args)
    print("payload: %d edges, %d groups" % (len(payload["edges"]), len(payload["groups"])))

    link = make_link(server, payload)
    print("link without a server (%d characters):\n  %s" % (len(link), link if len(link) < 300 else link[:300] + "…"))
    if args.link_only:
        if args.open:
            webbrowser.open(link)
        return 0

    try:
        info = health(server)
        print("server: NORMA %s, API %s, %s mode" % (info.get("version"), info.get("api"), info.get("mode")))
        token, url, hours = submit(server, payload)
        print("stored as %s for %s hours:\n  %s" % (token, hours, url))
        stored = fetch_session(server, token)
        ok = len(stored.get("edges", [])) == len(payload["edges"])
        print("read back: %d edges, %s" % (len(stored.get("edges", [])), "matches" if ok else "DIFFERS"))
    except (RuntimeError, urllib.error.URLError, OSError) as err:
        print("error: %s" % err, file=sys.stderr)
        print("Is NORMA's server.py running at %s? The #json link above works without it." % server, file=sys.stderr)
        return 1
    if args.open:
        webbrowser.open(url)
    return 0


if __name__ == "__main__":
    sys.exit(main())
