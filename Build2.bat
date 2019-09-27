rem Kernel Loader
del Stage2.bin
del Stage2.lst
nasm -f bin Stage2.asm -o Stage2.bin -l Stage2.lst
pause
exit