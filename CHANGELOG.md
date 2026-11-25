# Changelog

## [3.0.0dev] - unreleased

### Added
- Logos of the connected resources: each database importer (STRING, Reactome, OmniPath, NDEx, IntAct, Gene Ontology) shows its logo, the Export panel shows Cytoscape and Gephi next to Arena3D and Flame, and the Welcome page has a strip of all of them. Sources are listed in `assets/logos/SOURCES.txt`.
- Help, About and README cite the NORMA 3.0 bioRxiv preprint first; the Help's resource list is brought up to date (STRING 2025, Reactome, OmniPath and Gene Ontology 2026, Arena3D 3.0, Flame) and credits the topology scores.
- **Export → Flame**: send the network's genes to Flame (flame-enrich.org) for functional enrichment. A dialog first asks for all nodes as one list or one list per group of the annotation (up to 10 lists of 3,000 genes; optionally only the ticked groups and channels) and the organism. With server.py the named lists go through a new read-only relay (`/flame-api/legacy`, flame-enrich.org only) to Flame's API; without a server, Flame's link format is used. The lists can also be saved as text. The Arena3D and Flame boxes show the tools' logos.
- Runtime table (Network Profiler): the random networks' density (average degree 4, 8 or 16) and group overlap (none, 10% or 30% of nodes in two groups) can be chosen or all nine combinations run at once; new columns time the NORMA-2.0 virtual-node group layout and report the memory each network takes where the browser exposes it. Scripts under frontend/scripts regenerate the runtime grid, the use-case benchmark and example figures in headless Chromium.

- The on-screen legend can be minimized to a small "Legend" tab (button in its corner) so it does
  not cover the network; the choice is remembered in the browser.
- Layout benchmark: two topology scores next to the separation scores, neighbourhood preservation
  and distance stress, which measure how much a layout departs from the network's own structure
  without looking at the groups (`topology_fidelity.ts`, with unit tests); Help explains why the
  separation scores alone cannot judge group-aware layouts.
- **Separate nodes** buttons in the 2D and 3D Layout sections: overlapping nodes are moved slightly
  apart, keeping the arrangement; one undo step.
- A "What next?" card after a network loads, pointing to the 3D view, edge bundling and the layouts
  by groups (with a "Don't show this again" option, kept in the browser).
- STRING importer: the organism box searches all 12,535 STRING organisms by name or taxon ID (common
  organisms listed first); the number of interactors and a custom confidence cutoff can be typed exactly
  next to their sliders.
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

- Pie-chart nodes (group colors) are the default node fill, also when an expression file is
  selected; its colors or values are chosen under Node fill.
- Networks open with the weighted Fruchterman-Reingold layout instead of cose, which left nodes on
  top of each other on dense networks; `fr` is also the default of the Layout list.
- The legend is shown by default, and when parallel edges are merged per node pair it says so (grey:
  mixed channels; wider: more channels).
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
- Narrow browser windows: the View list no longer slides under the Grouping label; both lists shrink with the window, and below 1,200 px the View/Grouping labels keep only their icons so the whole header fits.
- The "Size" separator sat inside the Nodes section above its title, so the section opened with the
  separator first; it is now above the section.
- 3D image exports at more than screen resolution (without a legend) painted only the screen-sized
  area and left the rest unfilled.
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
