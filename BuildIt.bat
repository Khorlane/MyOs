@echo off
rem --------------------
rem - Assemble Stage 3 -
rem --------------------
call build3.bat noexit
@echo off
echo.
rem -----------------------------------------
rem - Copy Stage 2 and Stage 3 to Boot Disk -
rem -----------------------------------------
"C:\Program Files (x86)\DOSBox-0.74\DOSBox.exe" -conf C:\Dropbox\MyOs\DosBox1.txt
cls
@echo off
echo.
echo --------------------------
echo - Boot Disk prep is done -
echo --------------------------
echo.
pause
cls
echo.
echo ----------------------------
echo - Boot up MyOs using Bochs -
echo ----------------------------
pause
@echo on
"C:\Program Files (x86)\Bochs-2.6.9\bochs.exe" -q -f C:\Dropbox\MyOs\MyOs.bxrc