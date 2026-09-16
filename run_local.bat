@echo off
REM Start NORMA on this computer and open it in the browser (Windows).
cd /d "%~dp0"
if not exist frontend\dist\norma.html (
  echo Building the page (needs Node 22 and npm)...
  cd frontend && call npm install && call npm run build && cd ..
)
python backend\server.py --mode local %*
pause
