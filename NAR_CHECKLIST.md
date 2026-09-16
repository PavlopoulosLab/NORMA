# NORMA 3.0 and the NAR Web Server Issue requirements

Checked against the *Nucleic Acids Research* "Submitting to the Web Server
Issue" instructions (https://academic.oup.com/nar/pages/Submission_Webserver).
Items marked **deploy** depend on how the public server is set up.

## All applications must
| Requirement | How NORMA meets it |
|---|---|
| Broad audience, simple to use | Works on any network plus any grouping; Welcome page with one-click examples |
| Functional at proposal time | **deploy**: run `python3 server.py --mode hosted` behind HTTPS (see below) |
| Simple way to try sample data | *Open an example* (Welcome page) and the Examples list; `?example=NAME` links |
| Help/tutorial with links to sample output | Help tab: *Getting started*, *Sample data and sample output* (live example links) |
| Help explains how to interpret results | Help → *Interpreting the results* |
| Standard licence allowing free non-commercial use | MIT License (set `site.licenceName`/`licenceUrl`), shown on the Welcome page |

## Web servers and services must
| Requirement | How NORMA meets it |
|---|---|
| HTTPS on port 443 | **deploy**: `deploy/nginx.conf` (HTTPS on 443, HTTP redirected), or `server.py --tls-cert --tls-key --port 443 --http-redirect-port 80`; HSTS sent over HTTPS |
| Cookie consent if permanent cookies are used; no third-party/tracking cookies | NORMA sets **no cookies** and uses no tracking; only the optional WebGL preference is kept in local storage. No fonts or scripts from other sites (`app.cdnFallback: false` on the server) |
| Sample data accessible to check formats | Help → *Example files* (every sample file downloadable) |
| Links to sample output that work like real output | Help → *Sample data and sample output* |
| Rich output | Interactive 2D/3D networks, statistics, plots, legends, many export formats, hyperlinks to the databases |
| User data private | Uploaded data never leaves the browser; API payloads are kept only in server memory, reachable only with an unguessable link, for `api.ttlHours` |
| Link to results if not immediate | Results are immediate; API calls return a bookmarkable link |
| Licence on the landing page | Welcome page footer |

## Web servers must not
| Requirement | NORMA |
|---|---|
| Require login, registration or e-mail | None (stated on the Welcome page and in About) |
| Be classifiers without rich output / mere static-data portals | NORMA analyses and visualises users' own networks |
| Use tracking and third-party cookies / guest logins | None |

## Web services must
| Requirement | How NORMA meets it |
|---|---|
| Help page listing all services with their URLs | API tab (REST table, links, postMessage) and Help → *API for other applications* |
| Template script (Python) accessing several services and parsing output | `norma_api_client.py` (health, submit, read back, link) — downloadable from the API tab |
| Prominent link to the template on the home page | Welcome page footer: *Programmatic access: Python template* |

## Also expected
| Item | NORMA |
|---|---|
| Maintained for at least five years | **deploy**: set `site.maintainedUntil` (shown in About) |
| Tested on at least two common browsers | **deploy**: test the public server in two or more browsers (e.g. Chrome and Firefox) and list them in `site.testedBrowsers` (shown in About). This build was tested automatically in Chromium. |
| Contact information | `site.contactEmail`, `site.contactUrl` (Welcome page and About) |
| No installation needed | Runs in the browser; the server needs only Python's standard library |

## Before submitting
1. Copy `norma.config.hosted.json` to `norma.config.json` and fill in `publicUrl`, `institution`, `contactEmail`, `maintainedUntil` and `testedBrowsers`.
2. Serve it over HTTPS on port 443 (`deploy/nginx.conf` + `deploy/norma.service`, or Docker).
3. Check `https://YOUR-SERVER/api/health`, open every sample link in Help, and run `python3 norma_api_client.py --server https://YOUR-SERVER/`.
4. Test the site in at least two browsers.
