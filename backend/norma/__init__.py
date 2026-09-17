"""
NORMA 3 server
==============

Serves the NORMA web application, offers NORMA's REST API and relays requests
for STRING, the database importers and Arena3D, so the browser only ever talks
to this server. The same program runs NORMA on a laptop ("local" mode) or as a
public web server ("hosted" mode). Only the Python standard library is used.

    python3 backend/server.py                 # local: http://127.0.0.1:8000, opens the browser
    python3 backend/server.py --mode hosted --config norma.config.hosted.json
    python3 backend/server.py --help          # every option

Modules: ``config`` (settings), ``api`` (token store), ``relays`` (STRING /
Arena3D / databases), ``static`` (files and blocklist), ``handler`` (HTTP
routing), ``main`` (command line and serve loop).
"""

APP_VERSION = "3.0"
API_VERSION = "1.0"
