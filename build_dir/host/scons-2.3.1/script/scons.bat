@REM Copyright (c) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014 The SCons Foundation
@REM src/script/scons.bat  2014/03/02 14:18:15 garyo
@echo off
set SCONS_ERRORLEVEL=
if "%OS%" == "Windows_NT" goto WinNT

@REM for 9x/Me you better not have more than 9 args
python -c "from os.path import join; import sys; sys.path = [ join(sys.prefix, 'Lib', 'site-packages', 'scons-2.3.1'), join(sys.prefix, 'Lib', 'site-packages', 'scons'), join(sys.prefix, 'scons-2.3.1'), join(sys.prefix, 'scons')] + sys.path; import SCons.Script; SCons.Script.main()" %1 %2 %3 %4 %5 %6 %7 %8 %9
@REM no way to set exit status of this script for 9x/Me
goto endscons

@REM Credit where credit is due:  we return the exit code despite our
@REM use of setlocal+endlocal using a technique from Bear's Journal:
@REM http://code-bear.com/bearlog/2007/06/01/getting-the-exit-code-from-a-batch-file-that-is-run-from-a-python-program/

:WinNT
setlocal
@REM ensure the script will be executed with the Python it was installed for
set path=%~dp0;%~dp0..;%path%
@REM try the script named as the .bat file in current dir, then in Scripts subdir
set scriptname=%~dp0%~n0.py
if not exist "%scriptname%" set scriptname=%~dp0Scripts\%~n0.py
python "%scriptname%" %*
endlocal & set SCONS_ERRORLEVEL=%ERRORLEVEL%

if NOT "%COMSPEC%" == "%SystemRoot%\system32\cmd.exe" goto returncode
if errorlevel 9009 echo you do not have python in your PATH
goto endscons

:returncode
exit /B %SCONS_ERRORLEVEL%

:endscons
call :returncode %SCONS_ERRORLEVEL%
