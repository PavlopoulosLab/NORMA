# Changelog

## [3.0.0dev] - unreleased

### Added

- Test suites: pytest for the server (config precedence, token API, relays, static blocklist,
  security headers) and Playwright end-to-end smoke tests for the page (examples, layouts, tabs,
  3D view, image export, profiler, API tab, REST API).
- MIT `LICENSE` file, `AGENTS.md` / `CLAUDE.md` for coding agents, this changelog.

### Changed

- NORMA 3 is a rewrite of the R/Shiny NORMA 2 as a browser application (Cytoscape.js) served by a
  dependency-free Python server with a REST API and relays for STRING, Arena3D and the database
  importers. Runs on a computer, as a public server or opened straight from disk.

## [2.0.0] - 2022

- NORMA-2.0 (R/Shiny), see Karatzas et al., *Bioinformatics Advances* 2022;2(1):vbac036.
