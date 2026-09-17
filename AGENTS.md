# AGENTS.md

Guidance for coding agents (Claude Code, Codex, …) working in this repository.

NORMA 3 (The Network Makeup Artist) visualises and analyses networks together with their annotated
groups. It is a **browser application** (Vite / TypeScript / Cytoscape.js, in `frontend/`) served by a
**dependency-free Python server** (`backend/`, standard library only) that adds the REST API and the
STRING / Arena3D / database relays. Everything computational (layouts, clustering, statistics,
exports) runs in the browser; the server never sees network data except for the token API.

The app was migrated from R/Shiny (branch `master`) to this stack (branches `NORMA_v3`,
`update_architecture`). The architecture decisions live in `docs/superpowers/specs/`.

## Rules for Agents

- **Never push to remote.** Commit only when explicitly asked, one commit per feature.
- **Always use the `token-saviour` skill** — and the tools/skills it routes to — wherever it makes sense.
- **When a plan is active** (`docs/superpowers/plans/*.md`): one feature per commit, tick the checkboxes
  as you go, and verify each feature at runtime before moving to the next.
- **Before every commit**: run lint, typecheck and the relevant test suite (commands below). Verify UI
  changes at runtime with Playwright.
- **Keep the server dependency-free.** `backend/` must run with a plain `python3`; dev tools (pytest,
  ruff, mypy) go in the `dev` group only.
- **Keep the single-file build working.** `frontend/dist/norma.html` must open from disk without a server.
- **Keep responses concise** — summarise rather than dumping full files.

## Running the App

**Backend** (no runtime dependencies; `uv` only for dev tools):
```bash
python3 backend/server.py                     # http://localhost:8000, serves frontend/dist
python3 backend/server.py --help              # every option; also norma.config.json / NORMA_* env
cd backend && uv sync                         # dev tools into .venv
```

**Frontend:**
```bash
cd frontend
npm install
npm run dev                                   # http://localhost:5173 — /api, /string-api, … proxied to :8000
npm run build                                 # frontend/dist/ (norma.html is self-contained)
```

**Docker:** `docker compose up -d` (two-stage image: node build → python:slim).

**Tests:**
```bash
cd backend && uv run pytest                   # server unit/integration tests (fake upstreams, no network)
cd frontend && npm test                       # Vitest unit tests
cd frontend && npm run test:e2e               # Playwright (starts backend/server.py itself)
```

**Lint / format / typecheck:**
```bash
cd backend && uv run ruff check . && uv run ruff format . && uv run mypy norma
cd frontend && npm run lint && npm run format && npx tsc --noEmit
```

`pre-commit install` wires the same checks into git.

## Architecture Overview

### Backend (`backend/norma/`)
`http.server`-based, stateless apart from the in-memory token store.
- `config.py` — `DEFAULTS`, `ENV_MAP`, `load_config` (defaults < JSON file < `NORMA_*` env < CLI),
  `client_config` (the subset sent to the page as `/norma-config.js`).
- `api.py` — token store for `POST /api/external` / `GET /api/session/TOKEN` (TTL, size and count caps).
- `relays.py` — read-only relays to STRING, Arena3D and the database importers; strict upstream
  allowlists, body limits, rate spacing.
- `handler.py` — `NormaHandler` routing, security headers, access log; `ThreadingServer`, `RedirectHandler`.
- `static.py` — static root (`frontend/dist`) and the blocklist for deployment files.
- `main.py` — argparse, TLS, browser opening, serve loop. `backend/server.py` is the entry point.

### Frontend (`frontend/src/`)
The former 15K-line `<script>` of `norma.html` was split by its section headers; every module still
carries `// @ts-nocheck` and is typed one file at a time (remove the pragma, fix the errors, add the
file to `TYPED` in `eslint.config.js`). Each module keeps its declarations at top level and its page
wiring (listeners, initial renders) in an exported `init()`; `main.ts` imports all modules and calls
the `init()`s in the original order, then runs the boot sequence.
- `main.ts` — entry; `state.ts` — `S`, the mutable state several modules assign (ES module
  bindings are read-only across files); `config.ts` — `NORMA_CFG` from `window.NORMA_CONFIG`.
- `palette.ts`, `themes.ts`, `sample_data.ts`, `cy.ts` (Cytoscape instance, stylesheet, `onCommitStyle`
  hooks), `network_state.ts`, `hulls.ts` (group hulls / fog), `parallel_edges.ts`, `metrics.ts`.
- `layouts/` — `input` (what strategies see), `run` (incl. cose in `cose.worker.ts`, inlined into the bundle), `controls`; `library.ts`, `uploads.ts`,
  `examples.ts`, `export_norma.ts`, `wiring.ts` (tabs, keyboard, main buttons).
- `profiler.ts`, `viewbar.ts`, `side_tabs.ts`, `directed_stats.ts`, `demo_downloads.ts`,
  `recording.ts` (undo history), `label_colors.ts`, `benchmark.ts`, `contours.ts`.
- `export/` — `draw`, `shading`, `raster` (PNG/JPEG/WebP/PDF), `svg`, `dialog`: painters shared by
  the 2D and 3D image export.
- `view3d/` — the software-rendered 3D view (canvas 2D, no WebGL): `state`, `cache`, `camera`,
  `painters`, `draw`, `layouts`, `tab`, `input`, `export`.
- `string/` — STRING importer (`requests`, `ui_state`, `import`, `groupings`, `wiring`);
  `arena3d.ts`, `enrichment.ts` (database importers, enrichment), `group_network.ts`, `welcome.ts`.
- `clustering/` — `leiden`, `label_propagation`, `walktrap`, `mcl`, `mapping`, `wiring`.
- `api/` — `wiring` (links, `postMessage`, `?session=`), `tester` (the API tab).
- `window.__norma = { cy, net3d, views, currentTab }` is the Playwright test hook.

### Communication
- Page ↔ server: `fetch('api/…')`, `fetch('string-api/…')`, `fetch('db-api/fetch?url=…')`,
  `fetch('arena3d-api/external')`. Relative paths so the page works under any prefix.
- Runtime settings: `index.html` loads `/norma-config.js` before the bundle; on static hosting the
  file in `public/` is used instead.

### Network Data Model
- Edges: `source<TAB>target[<TAB>type[<TAB>weight]]`; groups: `name<TAB>member1,member2,…`;
  optional expression / colour files. Sessions and NORMA API payloads are JSON (`edges`, `groups`,
  `files`, `network`, `session`, `views`). See the Help and API tabs in the app.
