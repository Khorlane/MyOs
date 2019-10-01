@echo off
echo.
echo --------------------
echo - Assemble Stage 1 -
echo --------------------
@echo on
del Stage1.bin
del Stage1.lst
nasm -f bin Stage1.asm -o Stage1.bin -l Stage1.lst
@echo off
echo.
pause
if x%1 == xexit exit