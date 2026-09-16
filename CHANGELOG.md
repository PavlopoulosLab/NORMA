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
- Progress bars for long computations: layouts, edge bundling, the profiler, comparison, the
  database importers and STRING report their progress in the status area (with a time estimate
  for tasks made of weighted steps). Default `app.maxNodes` raised to 10,000.
- The side panel can be dragged to a different width, collapsed with the arrow on its edge and
  resized from the keyboard; the choice is remembered in the browser.

### Changed

- **Spread** now makes nodes repel (right) or attract (left) each other relative to the current
  arrangement, using stress majorization over nearby pairs, instead of scaling distances around the
  centre; links hold groups together and nodes stop before overlapping. Returning to 1x restores the
  base positions exactly; undo keeps them.
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

### Fixed

- Large networks took 25-35 s to lay out on opening (cose). Once the page has painted a frame,
  Cytoscape's cose runs 5-7x slower on the main thread (a V8 effect, reproducible with plain
  Cytoscape); the progress-bar change had moved the computation after that first paint. cose for
  networks above 150 nodes now runs headless in a Web Worker: 4-8 s again, and the page stays
  responsive while it computes.
- Group shading (hulls, fog, Bubble Sets) drawn at the wrong scale or not shown after a window moved
  between displays with different pixel ratios or a canvas resize without a resize event; the
  shading canvas now re-checks its size and pixel ratio and redraws inside an animation frame.

## [2.0.0] - 2022

- NORMA-2.0 (R/Shiny), see Karatzas et al., *Bioinformatics Advances* 2022;2(1):vbac036.
