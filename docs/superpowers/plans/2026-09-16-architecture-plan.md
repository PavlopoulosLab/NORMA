# Architecture update — plan

One commit per step. Each step ends with tests green. Tick as done.

- [ ] 1. `test(backend): pytest suite for server.py` — fixtures: temp APP_DIR, fake upstream servers, `NORMA_*_TEST_UPSTREAM` env
- [ ] 2. `test(e2e): Playwright smoke for norma.html` — `frontend/e2e/`, `window.__norma` hook added to norma.html
- [ ] 3. `docs: LICENSE (MIT), AGENTS.md, CLAUDE.md, CHANGELOG.md`
- [ ] 4. `refactor(backend): split server.py into backend/norma package` + pyproject, uv, ruff, mypy; tests moved
- [ ] 5. `build(frontend): Vite + TypeScript scaffold` — one app.ts, singlefile build, Playwright on dist
- [ ] 6. `refactor(frontend): split app.ts into modules` — state.ts, generated imports
- [ ] 7. `test(frontend): vitest unit tests`
- [ ] 8. `ci: GitHub workflow + pre-commit`
- [ ] 9. `docs: README, NAR_CHECKLIST, Dockerfile, compose, deploy, run_local`
