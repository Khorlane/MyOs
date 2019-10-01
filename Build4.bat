@echo off
echo.
echo -----------------------------------
echo - Copy Stage 2 and 3 to Boot Disk -
echo -----------------------------------
@echo on
copy Stage2.bin A:
copy Stage3.bin A:
@echo off
echo.
pause
if x%1 == xexit exit