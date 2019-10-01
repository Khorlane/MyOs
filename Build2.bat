@echo off
echo.
echo --------------------
echo - Assemble Stage 2 -
echo --------------------
@echo on
del Stage2.bin
del Stage2.lst
nasm -f bin Stage2.asm -o Stage2.bin -l Stage2.lst
@echo off
echo.
pause
if x%1 == xexit exit