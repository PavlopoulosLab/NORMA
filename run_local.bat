@echo off
REM Start NORMA on this computer and open it in the browser (Windows).
cd /d "%~dp0"
python backend\server.py --mode local %*
pause
