@echo off

if not "%3"=="" goto 30
if not "%2"=="" goto 20
if not "%1"=="" goto 10

echo usage: backbugs [[number_of_bins] default_output_name] command_file
goto 40

:10
if exist bugs.log del bugs.log
if exist bugs.bog del bugs.bog
bugs0603.exe 32 bugs %1
del bugs.buf
if exist bugs.log ren bugs.log bugs.bog
goto 40

:20
if exist %1.log del %1.log
if exist %1.bog del %1.bog
bugs0603.exe 32 %1 %2
del %1.buf
if exist %1.log ren %1.log %1.bog
goto 40

:30
if exist %2.log del %2.log
if exist %2.bog del %2.bog
bugs0603.exe %1 %2 %3
del %2.buf
if exist %2.log ren %2.log %2.bog

:40
