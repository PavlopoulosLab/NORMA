#!/bin/sh
# Start NORMA on this computer and open it in the browser (macOS, Linux).
cd "$(dirname "$0")"
exec python3 server.py --mode local "$@"
