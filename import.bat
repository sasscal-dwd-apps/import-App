REM Get the path where the files are located
SET mypath=%~dp0
echo %mypath:~0,-1%

REM Replace "\" with "\\" so that the path can be read by R
setlocal ENABLEDELAYEDEXPANSION
set word=\\
set str=%mypath%
set str=%str:\=!word!%
echo %str%

REM Run R
c:
cd "C:\Program Files\R\R-3.2.1\bin\i386"
R -e  "setwd('%str%'); source('master.R')"

REM pause