REM Create the executables directory if it doesn't exist
mkdir executable

REM Delete executable if it exists
del executable\myprogram.exe

REM Description: This file is used to build the project
gfortran -c structure/linkedList.f90 -o executable/linkedList.o
gfortran -c main.f90 -o executable/main.o

REM Linking the object files
gfortran -o executable/myapp executable/main.o executable/linkedList.o

REM Run the executable
executable\myapp