@echo off
echo.
echo --------------------
echo - Assemble Stage 3 -
echo --------------------
@echo on
del Stage3.bin
del Stage3.lst
nasm -f bin Stage3.asm -o Stage3.bin -l Stage3.lst
@echo off
echo.
pause
if x%1 == xexit exit