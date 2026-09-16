# Changelog

## [3.0.0dev] - unreleased

### Added

- Test suites: pytest for the server (config precedence, token API, relays, static blocklist,
  security headers), Vitest unit tests for the pure browser modules (community detection, hull
  geometry, Arena3D conversion, enrichment statistics, value parsing, config merge) and Playwright
  end-to-end smoke tests for the page (examples, layouts, tabs, 3D view, image export, profiler,
  API tab, REST API).
- Tooling: `uv` + ruff + mypy (strict) for the server; Vite, TypeScript, ESLint, Prettier for the
  page; GitHub Actions CI and pre-commit hooks.
- MIT `LICENSE` file, `AGENTS.md` / `CLAUDE.md` for coding agents, this changelog.

### Changed

- Repository layout: the single `server.py` is now the `backend/norma` package (`config`, `api`,
  `relays`, `static`, `handler`, `main`; entry `backend/server.py`) and the single `norma.html` is
  a Vite/TypeScript project in `frontend/` split into modules by feature (`layouts/`, `export/`,
  `view3d/`, `string/`, `clustering/`, `api/`). `npm run build` writes the self-contained
  `frontend/dist/norma.html`, which the server serves and which still opens from disk. Cytoscape.js
  comes from npm instead of `vendor/` + CDN fallback. The Docker image builds the page in a first
  stage; `run_local.*` build it when missing.
- NORMA 3 is a rewrite of the R/Shiny NORMA 2 as a browser application (Cytoscape.js) served by a
  dependency-free Python server with a REST API and relays for STRING, Arena3D and the database
  importers. Runs on a computer, as a public server or opened straight from disk.

## [2.0.0] - 2022

- NORMA-2.0 (R/Shiny), see Karatzas et al., *Bioinformatics Advances* 2022;2(1):vbac036.
