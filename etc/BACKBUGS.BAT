@echo off

if not "%3"=="" goto 30
if not "%2"=="" goto 20
if not "%1"=="" goto 10

echo usage: backbugs [[number_of_bins] default_output_name] command_file
goto 40

:10
bugs0603.exe 32 bugs %1
del bugs.buf
goto 40

:20
bugs0603.exe 32 %1 %2
del %1.buf
goto 40

:30
bugs0603.exe %1 %2 %3
del %2.buf

:40
