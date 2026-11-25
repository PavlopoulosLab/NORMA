# NORMA 3.0 — The Network Makeup Artist

NORMA visualises and analyses networks together with their annotated groups.
It runs in the web browser; this package runs it **on your computer** or as a
**public web server** with the same files. Free and open to all users under
the MIT License; no login, no cookies, no tracking.

## Quick start

| | |
|---|---|
| Build the page once | `cd frontend && npm install && npm run build` (writes `frontend/dist/norma.html`; needs Node 22) |
| On your computer | double-click `run_local.sh` (macOS/Linux) or `run_local.bat` (Windows), or `python3 backend/server.py` |
| Without Python | open `frontend/dist/norma.html` in a browser (no REST API or relays) |
| Public server | `python3 backend/server.py --mode hosted --config norma.config.hosted.json` behind HTTPS (`deploy/nginx.conf`) |
| Docker | `docker compose up -d` (builds the page inside the image) |
| All options | `python3 backend/server.py --help`, `python3 backend/server.py --print-config` |

The server needs only Python 3.9+ (standard library); the page is built once with Node.

## What the app does

* Groups drawn as convex hulls, bubble sets or fog clouds; nodes in several groups as pie
  charts (the default fill); one node shape per group; colour-blind-safe palettes; a legend
  that can be folded to a tab.
* 18 two-dimensional layouts (nine by connections, six non-overlapping group arrangements,
  the three NORMA-2.0 strategies) and nine 3D layouts; networks open with the weighted
  Fruchterman–Reingold layout; **Separate nodes** pushes overlapping nodes apart in 2D and 3D.
* A layout benchmark that scores every layout for group separation (silhouette, outline
  intrusion and overlap) and for how much of the network's own structure it keeps
  (neighbourhood preservation, distance stress).
* Parallel and directed edges with evidence channels, curved or bundled edges; importers for
  STRING (any of its organisms), Reactome, OmniPath, NDEx, IntAct and the Gene Ontology;
  topological, group and enrichment statistics; comparison of up to ten networks; exports
  as images, GraphML, GEXF, SIF, Cytoscape JSON, sessions and Arena3D layers; gene lists (all
  nodes, or one list per group) go to Flame for functional enrichment.

## Settings

Settings come from (increasing priority) built-in defaults, `norma.config.json`
(or `--config FILE` / `NORMA_CONFIG`), `NORMA_*` environment variables and
command-line options. `mode` is `local` (127.0.0.1, opens the browser, no
access log) or `hosted` (0.0.0.0, trusts the reverse proxy, anonymous access
log, security headers). The page receives its part of the settings as
`/norma-config.js`; for static hosting edit `frontend/public/norma-config.js` before building
(or the `norma-config.js` next to the built page).

| Setting | CLI / environment | Meaning |
|---|---|---|
| `mode` | `--mode`, `NORMA_MODE` | `local` or `hosted` |
| `server.host`, `server.port` | `--host`, `--port`, `NORMA_HOST`, `NORMA_PORT` | listening address |
| `server.publicUrl` | `--public-url`, `NORMA_PUBLIC_URL` | address users see; used in API links |
| `server.tlsCert`, `tlsKey`, `httpRedirectPort`, `hsts` | `--tls-cert`, `--tls-key`, `--http-redirect-port` | HTTPS without a proxy |
| `server.trustProxy` | `NORMA_TRUST_PROXY` | trust `X-Forwarded-*` headers |
| `server.accessLog` | `--access-log`, `NORMA_ACCESS_LOG` | `off`, `anonymous` (no IPs) or `full` |
| `api.enabled`, `ttlHours`, `maxMB`, `maxSessions` | `--no-api`, `NORMA_API`, `NORMA_API_TTL_HOURS`, `NORMA_API_MAX_MB` | REST API |
| `relays.string`, `arena3d`, `flame`, `databases` | `--no-relays`, `NORMA_RELAYS` | relays for STRING, Arena3D, Flame and the database importers |
| `site.*` | `NORMA_CONTACT_EMAIL`, `NORMA_INSTITUTION`, `NORMA_NOTICE` | contact, institution, licence, privacy/imprint links, maintenance statement, tested browsers, notice banner |
| `app.maxNodes`, `theme`, `startTab`, `cdnFallback` | `NORMA_MAX_NODES` | page defaults |

For the NAR Web Server Issue requirements see `docs/NAR_CHECKLIST.md`.

## Programmatic access

`examples/norma_api_client.py` is a template client (standard library only):

```bash
python3 examples/norma_api_client.py --server http://localhost:8000/ --open
```

## NORMA API

Other applications can open NORMA with their networks and groups:

```bash
curl -X POST http://localhost:8000/api/external \
  -H "Content-Type: application/json" \
  -d '{"name":"Demo","edges":[{"source":"A","target":"B"}],"groups":{"G1":["A","B"]}}'
# -> {"token": "...", "url": "http://localhost:8000/norma.html?session=...", "expiresInHours": 24}
```

Open the returned `url`. Payloads stay in the server's memory for
`NORMA_API_TTL_HOURS` (default 24). `GET /api/health` checks the API. Links
(`norma.html?data=URL`, `#json=...`) and `postMessage` work without the server;
the API tab in NORMA documents all three and has a tester.

## Why the relays

Browsers restrict pages from calling other sites. The server serves NORMA and
passes STRING requests on under `/string-api/`, so the browser only talks to
your own server:

```
browser ──▶ http://localhost:8000/string-api/json/network?upstream=https://string-db.org
server  ──▶ https://string-db.org/api/json/network
```

* Only read-only STRING API methods are relayed (`get_string_ids`, `network`,
  `interaction_partners`, `enrichment`, `functional_annotation`, `version`, …).
* Only `https://string-db.org` and its version addresses
  (e.g. `https://version-12-0.string-db.org`) are allowed as targets.
* Requests are spaced at least one second apart and identify themselves
  (`caller_identity=NORMA3`), as STRING asks.
* `GET /string-api/ping` tells the app that the relay is available.

The server also relays **Open in Arena3D**:

```
browser ──▶ http://localhost:8000/arena3d-api/external?upstream=https://arena3d.org
server  ──▶ https://arena3d.org/api/external   (JSON body; answer: { token, url })
```

Only `arena3d.org` addresses are accepted.

and **Open in Flame** (named gene lists for functional enrichment):

```
browser ──▶ http://localhost:8000/flame-api/legacy?upstream=https://flame-enrich.org
server  ──▶ https://flame-enrich.org/api/v1/legacy   (JSON: { "list name": [genes], "organism": 9606 }; answer: { workspace, url })
```

Only `flame-enrich.org` addresses are accepted. Without the server, NORMA opens Flame's own link
format (`?url_genes=A,B;C,D&organism=9606`), which keeps the lists but not their names.

The **database importers** (Reactome, OmniPath, NDEx, IntAct, QuickGO and the
GO API) go through `/db-api/fetch?url=…`; only those services' API addresses
are relayed.

In the app, **Database importers → STRING → STRING server → Connect** is set to
*Automatically*: the relay is used when present, otherwise STRING is called
directly from the browser.

### Behind another web server

For a public server, put `backend/server.py` (hosted mode) behind nginx or Apache
with HTTPS; `deploy/nginx.conf` is a complete example, including serving NORMA under
a path (`X-Forwarded-Prefix`). `frontend/dist/` can also be served as static files
(the page then reads `norma-config.js`, and `index.html` forwards the root address
to `norma.html`); the REST API and relays need the server, and the importers then
call the services directly.

## Development

```
backend/    standard-library Python server: backend/norma/{config,api,relays,static,handler,main}.py
frontend/   Vite + TypeScript page: src/ (modules), e2e/ (Playwright), public/ (assets)
deploy/     nginx / Apache / systemd examples;  docs/  NAR checklist, design notes
```

```bash
cd backend && uv sync && uv run pytest          # server tests; ruff, mypy also via uv run
cd frontend && npm install && npm run dev        # http://localhost:5173/norma.html, API proxied to :8000
cd frontend && npm test && npm run test:e2e      # Vitest units; Playwright (starts the server itself)
pre-commit install                               # ruff, eslint, prettier, tsc before each commit
```

See `AGENTS.md` for the architecture and the rules for coding agents, `CHANGELOG.md` for changes.

## STRING logo

STRING's logo is not bundled. To show it in the importer, save the official
logo from <https://string-db.org> as `frontend/public/assets/string-logo.png` (before building); otherwise a plain
"STRING" label is shown (and the browser logs a harmless 404 for the missing file).

## Citing

* Karatzas E, Koutrouli M, Baltoumas FA, Papanikolopoulou K, Bouyioukos C,
  Pavlopoulos GA. The network makeup artist (NORMA-2.0): distinguishing annotated
  groups in a network using innovative layout strategies.
  *Bioinformatics Advances* 2022;2(1):vbac036. doi:10.1093/bioadv/vbac036
* Koutrouli M, Karatzas E, Papanikolopoulou K, Pavlopoulos GA. NORMA: The Network
  Makeup Artist, a web tool for network annotation visualization.
  *Genomics, Proteomics & Bioinformatics* 2022;20(3):578–586. doi:10.1016/j.gpb.2021.02.005
* When using Arena3D: Karatzas E, Baltoumas FA, Panayiotou NA, Schneider R,
  Pavlopoulos GA. Arena3Dweb: interactive 3D visualization of multilayered networks.
  *Nucleic Acids Research* 2021;49(W1):W36–W45. doi:10.1093/nar/gkab278
* When using Arena3D: Kokoli M, Karatzas E, Baltoumas FA, Schneider R, Pafilis E,
  Paragkamian S, Doncheva NT, Jensen LJ, Pavlopoulos GA. Arena3Dweb: interactive 3D
  visualization of multilayered networks supporting multiple directional information
  channels, clustering analysis and application integration.
  *NAR Genomics and Bioinformatics* 2023;5(2):lqad053. doi:10.1093/nargab/lqad053
* When using STRING data: Szklarczyk D *et al.* The STRING database in 2025: protein
  networks with directionality of regulation.
  *Nucleic Acids Research* 2025;53(D1):D730–D737. doi:10.1093/nar/gkae1113
