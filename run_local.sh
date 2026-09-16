#!/bin/sh
# Start NORMA on this computer and open it in the browser (macOS, Linux).
cd "$(dirname "$0")"
if [ ! -f frontend/dist/norma.html ]; then
  echo "Building the page (needs Node 22 and npm)..."
  (cd frontend && npm install && npm run build) || exit 1
fi
exec python3 backend/server.py --mode local "$@"
