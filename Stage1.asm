;**************************************************************************************************
; Stage1.asm
;   A Simple Boot Sector that:
;   1. is exactly 512 bytes long
;   2. has the Magic Word at the end (0xAA55)
;   3. allows us to have a useable floppy
;      where we can put our Stage2/Stage3 code
;      by coding a proper BIOS Parameter Block
;   4. has code to load our Stage2 code
; Broken Thorn Entertainment
; Operating Systems Development Tutorial
; http://www.brokenthorn.com/Resources/OSDevIndex.html
;
; nasm -f bin Stage1.asm -o Stage1.bin -l Stage1.lst
;**************************************************************************************************

[bits 16]                               ; we are in 16 bit real mode
    ORG   0                             ; we will set regisers later
    JMP   Booter                        ; jump to start of bootloader

;--------------------------------------------------------------------------------------------------
; BIOS Parameter Block
;   and yes, this block must start at offset 0x003
;   and yes, these are the required fields
;   and yes, they must be in this order
;   you can change the names (obviously)
; Tutorial 5: Bootloaders 3
;--------------------------------------------------------------------------------------------------

; The BIOS Parameter Block begins 3 bytes from start. We do a far jump, which is 3 bytes in size.
; If you use a short jump, add a "nop" after it to offset the 3rd byte.
; See Wikipedia "Design of the FAT file system" for more info on the BIOS Parameter Block

                                        ; Hex Offset from beginning of Boot Sector
OEM                   DB "MyOs    "     ; 0x003  8 bytes padded with spaces
BytesPerSector        DW 512            ; 0x00B  2 bytes
SectorsPerCluster     DB 1              ; 0x00D  1 byte
ReservedSectors       DW 1              ; 0x00E  2 bytes
NumberOfFATs          DB 2              ; 0x010  1 bytes
RootEntries           DW 224            ; 0x011  2 bytes
TotalSectors          DW 2880           ; 0x013  2 bytes
Media                 DB 0xf0           ; 0x015  1 byte
SectorsPerFAT         DW 9              ; 0x016  2 bytes
SectorsPerTrack       DW 18             ; 0x018  2 bytes DOS 3.31 BPB
HeadsPerCylinder      DW 2              ; 0x01A  2 bytes DOS 3.31 BPB
HiddenSectors         DD 0              ; 0x01C  4 bytes DOS 3.31 BPB
TotalSectorsBig       DD 0              ; 0x020  4 bytes DOS 3.31 BPB
DriveNumber           DB 0              ; 0x024  1 byte  Extended BIOS Parameter Block
Unused                DB 0              ; 0x025  1 byte  Extended BIOS Parameter Block
ExtBootSignature      DB 0x29           ; 0x026  1 byte  Extended BIOS Parameter Block
SerialNumber          DD 0xa0a1a2a3     ; 0x027  4 bytes Extended BIOS Parameter Block
VolumeLabel           DB "MYOS FLOPPY"  ; 0x028 11 bytes Extended BIOS Parameter Block
FileSystem            DB "FAT12   "     ; 0x036  8 bytes Extended BIOS Parameter Block padded with spaces

;--------------------------------------------------------------------------------------------------
; Prints a string
; DS => SI: 0 terminated string
;--------------------------------------------------------------------------------------------------
Print:
    MOV   AH,0Eh                        ; set function code 0E (BIOS INT 10h - Teletype output) 
PrintLoop:
    LODSB                               ; Load byte at address DS:(E)SI into AL
    OR    AL,AL                         ; If AL = 0
    JZ    PrintDone                     ;   then we're done
    INT   10h                           ; Put character on the screen using bios interrupt 10
    JMP   PrintLoop                     ; Repeat until null terminator found
  PrintDone:
    RET                                 ; we are done, so return

;--------------------------------------------------------------------------------------------------
; Convert CHS to LBA
; Given: AX = Cluster to be read
; LBA = (Cluster - 2) * sectors per cluster
; Tutorial 6: Bootloaders 4
;--------------------------------------------------------------------------------------------------
ClusterLBA:
    SUB   AX,0x0002                     ; Adjust cluster to be zero based
    XOR   CX,CX                         ; CX =
    MOV   CL,BYTE [SectorsPerCluster]   ;  SectorsPerCluster
    MUL   CX                            ; AX = AX * CX
    ADD   AX,WORD [DataSector]          ; AX = AX + Data Sector
    RET

;--------------------------------------------------------------------------------------------------
; Convert LBA to CHS
; AX => LBA Address to convert
; absolute sector = (LBA %  sectors per track) + 1
; absolute head   = (LBA /  sectors per track) MOD number of heads
; absolute track  =  LBA / (sectors per track * number of heads)
; Tutorial 6: Bootloaders 4
;--------------------------------------------------------------------------------------------------
LBACHS:
    XOR   DX,DX                         ; DL = Remainder of
    DIV   WORD [SectorsPerTrack]        ;  AX \ SectorsPerTrack
    INC   DL                            ;   Plus 1
    MOV   BYTE [AbsoluteSector],DL      ;    Save DL 
    XOR   DX,DX                         ; DL = Remainder of
    DIV   WORD [HeadsPerCylinder]       ;  AX \ HeadsPerCylinder
    MOV   BYTE [AbsoluteHead],DL        ;   Save DL
    MOV   BYTE [AbsoluteTrack],AL       ; Save AL (what's left after the above dividing)
    RET

;--------------------------------------------------------------------------------------------------
; Reads a series of sectors
; CX    => Number of sectors to read
; AX    => Starting sector
; ES:BX => Buffer to read to
; Tutorial 5: Bootloaders 3
;--------------------------------------------------------------------------------------------------
ReadSector:
    MOV   DI,0x0005                     ; five retries for error
ReadSectorLoop:
    PUSH  AX
    PUSH  BX
    PUSH  CX
    CALL  LBACHS                        ; convert starting sector to CHS
    MOV   AH,0x02                       ; BIOS read sector
    MOV   AL,0x01                       ; read one sector
    MOV   CH,BYTE [AbsoluteTrack]       ; track
    MOV   CL,BYTE [AbsoluteSector]      ; sector
    MOV   DH,BYTE [AbsoluteHead]        ; head
    MOV   DL,BYTE [DriveNumber]         ; drive
    INT   0x13                          ; invoke BIOS
    JNC   ReadSectorOk                  ; test for read error
    XOR   AX,AX                         ; BIOS reset disk
    INT   0x13                          ; invoke BIOS
    DEC   DI                            ; decrement error counter
    POP   CX
    POP   BX
    POP   AX
    JNZ   ReadSectorLoop                ; attempt to read again
    INT   0x18
ReadSectorOk:
    MOV   SI,ProgressMsg
    CALL  Print                         ;
    POP   CX
    POP   BX
    POP   AX
    ADD   BX,WORD [BytesPerSector]      ; queue next buffer
    INC   AX                            ; queue next sector
    LOOP  ReadSector                    ; read next sector
    RET

;--------------------------------------------------------------------------------------------------
; Boot Loader Entry Point
;--------------------------------------------------------------------------------------------------
Booter:
    ;-------------------------------------------------------
    ;- code located at 0000:7C00, adjust segment registers
    ;-------------------------------------------------------
    CLI                                 ; disable interrupts
    MOV   AX,0x07C0                     ; setup
    MOV   DS,AX                         ;  registers
    MOV   ES,AX                         ;   to point
    MOV   FS,AX                         ;    to our
    MOV   GS,AX                         ;     segment

    ;--------------
    ;- create stack
    ;--------------
    MOV   AX,0x0000                     ; set the
    MOV   SS,AX                         ;  stack to
    MOV   SP,0xFFFF                     ;   somewhere safe
    STI                                 ; restore interrupts

    ;-------------------------
    ;- Display loading message
    ;-------------------------
    MOV   SI,LoadingMsg                 ; si points to first byte in message
    CALL  Print                         ; print message

    ;--------------------------
    ; Load root directory table 
    ; Tutorial 6: Bootloaders 4  
    ;--------------------------
    ; compute size of root directory and store in "cx"
    XOR   CX,CX                         ; zero out cx
    XOR   DX,DX                         ; zero out dx
    MOV   AX,0x0020                     ; 32 byte directory entry
    MUL   WORD [RootEntries]            ; total size of directory
    DIV   WORD [BytesPerSector]         ; sectors used by directory
    XCHG  AX,CX                         ; swap ax cx
    ; compute location of root directory and store in "ax"
    MOV   AL,BYTE [NumberOfFATs]        ; number of FATs
    MUL   WORD [SectorsPerFAT]          ; sectors used by FATs
    ADD   AX,WORD [ReservedSectors]     ; adjust for bootsector
    MOV   WORD [DataSector],AX          ; base of root directory
    ADD   WORD [DataSector],CX
    ; read root directory into memory (7C00:0200)
    MOV   BX,0x0200                     ; read root dir
    CALL  ReadSector                    ;  above bootcode

    ;-------------------------------
    ; Find Stage 2 in Root Directory
    ; Tutorial 6: Bootloaders 4
    ;-------------------------------
    MOV   CX,WORD [RootEntries]         ; load loop counter
    MOV   DI,0x0200                     ; locate first root entry
FindFat:
    PUSH  CX                            ; save loop counter on the stack
    MOV   CX,0x000B                     ; eleven character name
    MOV   SI,Stage2Name                 ; Stage2 file name to find
    PUSH  DI
    REP   CMPSB                         ; test for entry match
    POP   DI
    JE    LoadFat                       ; found our file, now load it
    POP   CX                            ; pop our loop counter
    ADD   DI,0x0020                     ; queue next directory entry
    LOOP  FindFat                       ; keep looking
    JMP   FindFatFailed                 ; file not found, this is bad!

    ;--------------------------
    ; Load FAT
    ; Tutorial 6: Bootloaders 4
    ;--------------------------
LoadFat:
    ; save starting cluster of boot image
    MOV   DX,WORD [DI + 0x001A]         ; save file's
    MOV   WORD [Cluster],DX             ;  first cluster

    ; compute size of FAT and store in "cx"
    XOR   AX,AX
    MOV   AL,BYTE [NumberOfFATs]        ; number of FATs
    MUL   WORD [SectorsPerFAT]          ; sectors used by FATs
    MOV   CX,AX

    ; compute location of FAT and store in "ax"
    MOV   AX,WORD [ReservedSectors]     ; adjust for bootsector

    ; read FAT into memory (7C00:0200)
    MOV   BX,0x0200                     ; read FAT
    CALL  ReadSector                    ;  into memory above our bootcode

;--------------------------------------------------------------------------------------------------
; Load Stage 2
; Tutorial 6: Bootloaders 4
;--------------------------------------------------------------------------------------------------
    ; read Stage2 file into memory (0050:0000)
    MOV   AX,0x0050                     ; set segment register
    MOV   ES,AX                         ;  to 50h
    MOV   BX,0x0000                     ; push our starting address (0h)
    PUSH  BX                            ;  onto the stack

LoadStage2:
    MOV   AX,WORD [Cluster]             ; cluster to read
    POP   BX                            ; buffer to read into
    CALL  ClusterLBA                    ; convert cluster to LBA
    XOR   CX,CX                         ; CL =
    MOV   CL,BYTE [SectorsPerCluster]   ;  sectors to read
    CALL  ReadSector                    ; read a sector
    PUSH  BX                            ; push buffer ptr to stack

    ; compute next cluster
    MOV   AX,WORD [Cluster]             ; identify current cluster
    MOV   CX,AX                         ; copy current cluster
    MOV   DX,AX                         ; copy current cluster
    SHR   DX,0x0001                     ; divide by two
    ADD   CX,DX                         ; sum for (3/2)
    MOV   BX,0x0200                     ; location of FAT in memory
    ADD   BX,CX                         ; index into FAT
    MOV   DX,WORD [BX]                  ; read two bytes from FAT, indexed by BX
    TEST  AX,0x0001                     ; test under mask, if cluster number is odd
    JNZ   LoadStage2OddCluster          ;  then process Odd Cluster

LoadStage2EvenCluster:
    AND   DX,0000111111111111b          ; take low  twelve bits DX x'ABCD' -> x'0BCD'
    JMP   LoadStage2CheckDone

LoadStage2OddCluster:
    SHR   DX,0x0004                     ; take high twelve bits DX x'ABCD' -> x'0ABC'

LoadStage2CheckDone:
    MOV   WORD [Cluster],DX             ; store new cluster
    CMP   DX,0x0FF0                     ; If DX is less than EOF (0x0FF0)
    JB    LoadStage2                    ;   then keep going (JB = Jump Below)

;--------------------------------------------------------------------------------------------------
; Jump to Stage 2 code
;--------------------------------------------------------------------------------------------------
    MOV   SI,Stage2Msg                  ; si points to first byte in message
    CALL  Print                         ; print message
    MOV   AH,0X00                       ; wait
    INT   0x16                          ;  for keypress
    MOV   SI,NewLineMsg                 ; print
    CALL  Print                         ;  new line
    PUSH  WORD 0x0050                   ; Jump to our Stage2 code that we put at 0050:0000
    PUSH  WORD 0x0000                   ;   by using a Far Return which pops IP(0h) then CS(50h)
    RETF                                ;   and poof, we're executing our Stage2 code!

;--------------------------------------------------------------------------------------------------
; Failed to find FAT (File Allocation Table)
;--------------------------------------------------------------------------------------------------
FindFatFailed:
    MOV   SI,FailureMsg                 ; print
    CALL  Print                         ;  failure message
    MOV   AH,0x00                       ; wait for
    INT   0x16                          ;  keypress
    INT   0x19                          ; warm boot computer

;--------------------------------------------------------------------------------------------------
; Working Storage
;--------------------------------------------------------------------------------------------------
    AbsoluteHead    DB 0x00
    AbsoluteSector  DB 0x00
    AbsoluteTrack   DB 0x00
    Cluster         DW 0x0000
    DataSector      DW 0x0000
    FailureMsg      DB 0x0D, 0x0A, "MISSING OR CURRUPT STAGE2", 0x0D, 0x0A, 0x00
    LoadingMsg      DB 0x0D, 0x0A, "MyOs v0.1.1 Stage 1", 0x00
    NewLineMsg      DB 0x0D, 0x0A, 0x00
    ProgressMsg     DB ".", 0x00
    Stage2Msg       DB 0x0D, 0x0A, " Hit Enter to Jump to Stage 2 ", 0x00
    Stage2Name      DB "STAGE2  BIN"

;--------------------------------------------------------------------------------------------------
; Make it a Boot Sector! (must be exactly 512 bytes)
;--------------------------------------------------------------------------------------------------
    TIMES 510-($-$$) DB 0               ; make boot sector exactly 512 bytes
    DW 0xAA55                           ; Magic Word that makes this a boot sector