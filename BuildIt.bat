@echo off
echo.
echo --------------------
echo - Assemble Stage 3 -
echo --------------------
@echo on
call build3.bat
@echo off
echo.
echo -----------------------------------------
echo - Copy Stage 2 and Stage 3 to Boot Disk -
echo -----------------------------------------
@echo on
"C:\Program Files (x86)\DOSBox-0.74\DOSBox.exe" -conf C:\Dropbox\MyOs\DosBox1.txt
pause
@echo off
echo.
echo ----------------------------
echo - Boot up MyOs using Bochs -
echo ----------------------------
@echo on
"C:\Program Files (x86)\Bochs-2.6.9\bochs.exe" -q -f C:\Dropbox\MyOs\MyOs.bxrc