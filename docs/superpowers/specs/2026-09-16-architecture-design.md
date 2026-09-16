# NORMA 3 architecture update — design

Date: 2026-09-16. Branch: `update_architecture`. Approved in chat.

## Goal

Move NORMA from one 1.4 MB `norma.html` + one `server.py` to a professional layout
modelled on Arena3Dweb, with a regression test suite written *before* the move, and
lint/typecheck/CI tooling matching Arena3D. No behaviour change for users.

## Decisions

| Topic | Decision | Why |
|---|---|---|
| Backend | Keep stdlib-only `http.server`; split into `backend/norma/` package | README/Dockerfile promise "plain python3"; no server-side algorithms, so FastAPI buys deps for nothing |
| Backend tooling | `uv` + `pyproject.toml`, dev deps only (pytest, ruff, mypy) | Same commands as Arena3D; zero runtime deps preserved |
| Frontend | Vite + TypeScript, `strict: false` initially, tighten per file later | Uniform stack with Arena3D; 15K lines cannot be typed in one go |
| Single-file build | `vite-plugin-singlefile` → `frontend/dist/norma.html` | "Open norma.html from disk, no Python" keeps working |
| Cytoscape | npm dependency, bundled | Replaces `vendor/` + CDN fallback |
| Runtime config | `index.html` loads `/norma-config.js` before the bundle | Unchanged contract with `server.py` |
| Arena3D EventBus / store / commands | Not adopted | NORMA has none; adding them is a rewrite, not a refactor |
| Root `server.py` shim | Removed; `backend/server.py` is the entry | Cleaner; docs updated |

## Layout

```
backend/
  server.py           entry: `python3 backend/server.py [--mode ...]`
  norma/
    __init__.py       APP_VERSION, API_VERSION
    config.py         DEFAULTS, ENV_MAP, load_config, client_config
    api.py            token store: api_put/api_get/expiry/limits
    relays.py         STRING / Arena3D / database relays, allowlists, rate limit
    handler.py        NormaHandler, ThreadingServer, RedirectHandler
    static.py         static root resolution (frontend/dist) + blocklist
    main.py           argparse + serve loop
  tests/              pytest (fake upstreams via threading HTTPServer)
  pyproject.toml
frontend/
  index.html          shell markup (from norma.html), loads /norma-config.js then /src/main.ts
  public/             assets/ (favicons, logo), norma-config.js (static-hosting default)
  src/
    main.ts           entry; wires sections in original order
    state.ts          top-level mutable state object
    config.ts, palette.ts, themes.ts, sample_data.ts, cy.ts, state/…, groups/hulls.ts,
    edges.ts, metrics.ts, layouts/{input,run,controls}.ts, library.ts, uploads.ts,
    examples.ts, export/{draw,raster,pdf,svg,dialog}.ts, profiler.ts, viewbar.ts,
    tabs.ts, directed.ts, demo_downloads.ts, recording.ts, search.ts, labels.ts,
    view3d/{state,colors,cache,camera,painters,draw,layouts,tab,rotate,pick,input,controls,export}.ts,
    string/{requests,scores,ui,import,groupings,wiring}.ts, welcome.ts,
    clustering/{leiden,label_propagation,walktrap,mcl,mapping}.ts,
    benchmark.ts, contours.ts, arena3d.ts, group_network.ts, enrichment.ts,
    wiring.ts, api_tab.ts, api_client_template.py?raw
    style.css
  e2e/                Playwright
  vite.config.ts, tsconfig.json, eslint.config.js, .prettierrc, package.json
deploy/               nginx, apache, systemd (paths updated)
docs/                 NAR_CHECKLIST.md, superpowers/
AGENTS.md, CLAUDE.md, CHANGELOG.md, LICENSE, README.md, Dockerfile (2-stage), docker-compose.yml,
.pre-commit-config.yaml, .github/workflows/ci.yml, run_local.sh, run_local.bat
```

Module file names above are the target; the split follows the 76 existing
`/* ---------- section ---------- */` headers and merges tiny neighbours.

## Frontend split method (the risky step)

1. Vite scaffold with the whole script as one `app.ts`; Playwright must pass on `dist/`.
2. Cut `app.ts` at section headers into files.
3. Move the 187 top-level `let/const/var` into `state.ts` as one exported object `S`;
   rewrite references `foo` → `S.foo` (ES module imports are read-only bindings, so
   cross-file assignment needs an object).
4. Generate `import { … } from './x'` lines from `tsc` "Cannot find name" errors
   (script in `scripts/`), iterate until tsc is clean.
5. Top-level side effects (DOM wiring) stay in the file they came from; `main.ts`
   imports files in the original order so evaluation order is preserved.
6. Playwright + vitest green → commit.

## Tests

- **pytest** (written first, against current `server.py`, then moved): config
  precedence (defaults < file < env < CLI), `client_config` shape, token API
  put/get/404/expiry/size/session cap, relay: allowed upstream/method/format, body
  limits, blocked hosts, static blocklist (Dockerfile, deploy/), security headers,
  `/` → `norma.html` redirect with query, `/api/health`.
- **Playwright smoke** (against served page, before and after split): load, open
  example, node/edge counts via `window.__norma`, run a layout, group hull toggle,
  export PNG/SVG produce output, 3D tab renders, profiler, API tab tester, external
  session round trip via `POST /api/external`.
- **vitest** (after split): Leiden, label propagation, Walktrap, MCL, hull/fog geometry,
  TSV/Arena3D parsers, config merge.

## CI

`.github/workflows/ci.yml`: backend job (uv sync, ruff check, ruff format --check,
mypy, pytest), frontend job (npm ci, eslint, prettier --check, tsc --noEmit, vitest,
vite build), e2e job (build, start backend, Playwright chromium). Pre-commit mirrors
the lint hooks. Branches: `master`, `NORMA_v3`, `update_architecture`.
