@echo off

rem Create temp directory
mkdir temp

rem Compile each .c file to an object file
gcc -c -o temp\main.o main.c -I.
gcc -c -o temp\chunk.o chunk.c -I.
gcc -c -o temp\memory.o memory.c -I.
gcc -c -o temp\debug.o debug.c -I.
gcc -c -o temp\value.o value.c -I.
gcc -c -o temp\table.o table.c -I.
gcc -c -o temp\vm.o vm.c -I.
gcc -c -o temp\compiler.o compiler.c -I.
gcc -c -o temp\scanner.o scanner.c -I.
gcc -c -o temp\object.o object.c -I.

rem Link the object files to create the executable
gcc -o main temp\main.o temp\chunk.o temp\memory.o temp\debug.o temp\value.o temp\table.o temp\vm.o temp\compiler.o temp\scanner.o temp\object.o -I.

rem Clean up the object files (optional)
del temp\*.o
rmdir temp

@echo on
