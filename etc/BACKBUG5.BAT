@echo off
rem ESS[BUGS]:  02/18/2004
rem runs BUGS taking commands from command file

if not "%2"=="" goto 20
if not "%1"=="" goto 10

echo usage: backbug5 [default_output_name] command_file
goto 40

:10
if exist bugs.bog attrib -r bugs.bog
bugs05.exe bugs.buf bugs.bog bugs.out bugs.ind bugs1.out bugs1.ind %1
copy /y bugs.log bugs.bog
attrib +r bugs.bog
goto 40

:20
set one=1
if exist %1.bog attrib -r %1.bog
bugs05.exe %1.buf %1.bog %1.out %1.ind %1%one%.out %1%one%.ind %2
copy /y %1.log %1.bog
attrib +r %1.bog
goto 40

:40
