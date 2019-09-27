rem Kernel
del Stage3.bin
del Stage3.lst
nasm -f bin Stage3.asm -o Stage3.bin  -l Stage3.lst
pause