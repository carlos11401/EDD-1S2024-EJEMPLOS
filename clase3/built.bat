REM Create the executables directory if it doesn't exist
mkdir executable

REM Delete executable if it exists
del bin\myprgram.exe

REM Alternative way to compile the project
gfortran -g -o bin\myprogram.exe main.f90 structures/circleList.f90

REM Run the executable
bin\myprogram