REM Create the executables directory if it doesn't exist
mkdir bin
REM Delete executable if it exists
del bin\main.exe
REM Link the object files into the executable and place it in the executables directory
gfortran -g -o bin\main.exe main.f90 structures/matrix.f90 structures/header.f90
REM Run the executable
bin\main.exe