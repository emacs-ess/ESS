@echo off
rem ESS[BUGS]:  02/13/2003
rem runs BUGS taking commands from command file

if not "%3"=="" goto 30
if not "%2"=="" goto 20
if not "%1"=="" goto 10

echo usage: backbugs [[number_of_bins] default_output_name] command_file
goto 40

:10
bugs0603.exe 32 bugs %1
if exist bugs.bog attrib -r bugs.bog
copy /y bugs.log bugs.bog
attrib +r bugs.bog
goto 40

:20
bugs0603.exe 32 %1 %2
if exist %1.bog attrib -r %1.bog
copy /y %1.log %1.bog
attrib +r %1.bog
goto 40

:30
bugs0603.exe %1 %2 %3
if exist %2.bog attrib -r %2.bog
copy /y %2.log %2.bog
attrib +r %2.bog

:40
