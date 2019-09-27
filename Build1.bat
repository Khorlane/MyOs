rem Boot Sector
del Stage1.bin
del Stage1.lst
nasm -f bin Stage1.asm -o Stage1.bin -l Stage1.lst
pause
exit