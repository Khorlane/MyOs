;**********************************************************
; Stage2.asm
;   Stage2 Bootloader
;
; Broken Thorn Entertainment
; Operating Systems Development Tutorial
; http://www.brokenthorn.com/Resources/OSDevIndex.html
;
; nasm -f bin Stage2.asm -o Stage2.bin -l Stage2.lst
;**********************************************************

; Remember the memory map-- 500h through 7BFFh is unused above the BIOS data area.
; We are loaded at 500h (50h:0h)
[bits 16]
    ORG   0500h
    JMP   Main                          ; jump to Main

;--------------------------------------------------------------------------------------------------
; Prints a null terminated string
; DS => SI: 0 terminated string
;--------------------------------------------------------------------------------------------------
[bits 16]
PutStr:
    PUSHA                               ; save registers
    MOV   AH,0Eh                        ; Nope-Print the character
PutStr1:
    LODSB                               ; load next byte from string from SI to AL
    OR    AL,AL                         ; Does AL=0?
    JZ    PutStr2                       ; Yep, null terminator found-bail out
    INT   10h                           ; invoke BIOS
    JMP   PutStr1                       ; Repeat until null terminator found
PutStr2:
    POPA                                ; restore registers
    RET                                 ; we are done, so return

;--------------------------------------------------------------------------------------------------
; Install our GDT
; Tutorial 8: Protected Mode
;--------------------------------------------------------------------------------------------------
[bits 16]
InstallGDT:
    CLI                                 ; disable interrupts
    PUSHA                               ; save registers
    LGDT  [GDT2]                        ; load GDT into GDTR
    STI                                 ; enable interrupts
    POPA                                ; restore registers
    RET                                 ; All done!

;--------------------------------------------------------------------------------------------------
; Enable A20 line through output port
;--------------------------------------------------------------------------------------------------
[bits 16]
EnableA20:
    CLI                                 ; disable interrupts
    PUSHA

    CALL  WaitInput                     ; wait for keypress
    MOV   AL,0ADh
    OUT   64h,AL                        ; disable keyboard
    CALL  WaitInput                    

    MOV   AL,0D0h
    OUT   64h,AL                        ; tell controller to read output port
    CALL  WaitOutput                   

    IN    AL,60h
    PUSH  EAX                           ; get output port data and store it
    CALL  WaitInput                    

    MOV   AL,0D1h
    OUT   64h,AL                        ; tell controller to write output port
    CALL  WaitInput                    

    POP   EAX
    OR    AL,2                          ; set bit 1 (enable a20)
    OUT   60h,AL                        ; write out data back to the output port

    CALL  WaitInput                    
    MOV   AL,0AEh                       ; enable keyboard
    OUT   64h,AL

    CALL  WaitInput                     ; wait for keypress
    POPA
    STI                                 ; enable interrupts
    RET

;------------------------------
; Helper routines for EnableA20
;------------------------------
WaitInput:
    IN    AL,64h                        ; wait for
    TEST  AL,2                          ;  input buffer
    JNZ   WaitInput                     ;   to clear
    RET

WaitOutput:
    IN    AL,64h                        ; wait for
    TEST  AL,1                          ;  output buffer
    JZ    WaitOutput                    ;   to clear
    RET

;--------------------------------------------------------------------------------------------------
; Floppy Driver Routines
;--------------------------------------------------------------------------------------------------
;------------------------------------------
; Convert CHS to LBA
; LBA = (cluster - 2) * sectors per cluster
;------------------------------------------
[bits 16]
ClusterLBA:
    SUB   AX,0002h                      ; zero base cluster number
    XOR   CX,CX
    MOV   CL,BYTE [SectorsPerCluster]   ; convert byte to word
    MUL   CX
    ADD   AX,WORD [DataSector]          ; base data sector
    RET

;---------------------------------------------------------------------------
; Convert LBA to CHS
; AX = LBA Address to convert
;
; absolute sector = (logical sector / sectors per track) + 1
; absolute head   = (logical sector / sectors per track) MOD number of heads
; absolute track  = logical sector / (sectors per track * number of heads)
;---------------------------------------------------------------------------
[bits 16]
LBACHS:                                 ;
    XOR   DX,DX                         ; prepare dx:ax for operation
    DIV   WORD [SectorsPerTrack]        ; calculate
    INC   DL                            ; adjust for sector 0
    MOV   BYTE [AbsoluteSector],DL
    XOR   DX,DX                         ; prepare dx:ax for operation
    DIV   WORD [HeadsPerCylinder]       ; calculate
    MOV   BYTE [AbsoluteHead],DL
    MOV   BYTE [AbsoluteTrack],AL
    RET

;-----------------------------------
; Read a series of sectors
; CX     = Number of sectors to read
; AX     = Starting sector
; ES:EBX = Buffer
;-----------------------------------
[bits 16]
ReadSector:
    MOV   DI,0005h                      ; five retries for error
ReadSector1:
    PUSH  AX
    PUSH  BX
    PUSH  CX
    CALL  LBACHS                        ; convert starting sector to CHS
    MOV   AH,02h                        ; BIOS read sector
    MOV   AL,01h                        ; read one sector
    MOV   CH,BYTE [AbsoluteTrack]       ; track
    MOV   CL,BYTE [AbsoluteSector]      ; sector
    MOV   DH,BYTE [AbsoluteHead]        ; head
    MOV   DL,BYTE [DriveNumber]         ; drive
    INT   13h                           ; invoke BIOS
    JNC   ReadSector2                   ; test for read error
    XOR   AX,AX                         ; BIOS reset disk
    int   13h                           ; invoke BIOS
    DEC   DI                            ; decrement error counter
    POP   CX
    POP   BX
    POP   AX
    JNZ   ReadSector1                   ; attempt to read again
    INT   18h
ReadSector2:
    POP   CX
    POP   BX
    POP   AX
    ADD   BX,WORD [BytesPerSector]      ; queue next buffer
    INC   AX                            ; queue next sector
    LOOP  ReadSector                    ; read next sector
    RET

;------------------------------------
; Load Root Directory Table to 07E00h
;------------------------------------
[bits 16]
LoadRootDir:
    PUSHA                               ; store registers
    PUSH  ES
    ; compute size of root directory and store in "CX"
    XOR   CX,CX                         ; clear registers
    XOR   DX,DX
    MOV   AX,32                         ; 32 byte directory entry
    MUL   WORD [RootEntries]            ; total size of directory
    DIV   WORD [BytesPerSector]         ; sectors used by directory
    XCHG  AX,CX                         ; move into AX
    ; compute location of root directory and store in "AX"
    MOV   AL,BYTE [NumberOfFATs]        ; number of FATs
    MUL   WORD [SectorsPerFAT]          ; sectors used by FATs
    ADD   AX,WORD [ReservedSectors]
    MOV   WORD [DataSector],AX          ; base of root directory
    ADD   WORD [DataSector],CX
    ; read root directory into 07E00h
    PUSH  WORD RootSegment
    POP   ES
    MOV   BX,0                          ; copy root dir
    CALL  ReadSector                    ; read in directory table
    POP   ES
    POPA                                ; restore registers and return
    RET

;-----------------------------
; Loads FAT table to 07C00h
; ES:DI = Root Directory Table
;-----------------------------
[bits 16]
LoadFAT:
    PUSHA                               ; store registers
    PUSH  ES
    ; compute size of FAT and store in "CX"
    XOR   AX,AX
    MOV   AL,BYTE [NumberOfFATs]        ; number of FATs
    MUL   WORD [SectorsPerFAT]          ; sectors used by FATs
    MOV   CX,AX
    ; compute location of FAT and store in "AX"
    MOV   AX,WORD [ReservedSectors]
    ; read FAT into memory (Overwrite our bootloader at 07C00h)
    PUSH  WORD FatSegment
    POP   ES
    XOR   BX,BX
    CALL  ReadSector 
    POP   ES
    POPA                                ; restore registers and return
    RET

;----------------------------------------------------------------
; Search for filename in root table
; parm DS:SI = File name
; ret  AX    = File index number in directory table. -1 if error
;----------------------------------------------------------------
[bits 16]
FindFile:
    PUSH  CX                            ; store registers
    PUSH  DX
    PUSH  BX
    MOV   BX,SI                         ; copy filename for later
    ; browse root directory for binary image
    MOV   CX,WORD [RootEntries]         ; load loop counter
    MOV   DI,RootOffset                 ; locate first root entry at 1 MB mark
    CLD                                 ; clear direction flag
FindFile1:
    PUSH  CX
    MOV   CX,11                         ; eleven character name. Image name is in SI
    MOV   SI,BX                         ; image name is in BX
    PUSH  DI
    REP   CMPSB                         ; test for entry match
    POP   DI
    JE    FindFile2
    POP   CX
    ADD   DI,32                         ; queue next directory entry
    LOOP  FindFile1
    ; Not Found
    POP   BX                            ; restore registers and return
    POP   DX
    POP   CX
    MOV   AX,-1                         ; set error code
    RET
FindFile2:
    POP   AX                            ; return value into AX contains entry of file
    POP   BX                            ; restore registers and return
    POP   DX
    POP   CX
    RET

;-----------------------------------------
; Load file
; parm ES:SI  = File to load
; parm EBX:BP = Buffer to load file to
; ret  AX     = -1 on error, 0 on success
; ret  CX     = number of sectors read
;-----------------------------------------
[bits 16]
LoadFile:
    XOR   ECX,ECX                       ; size of file in sectors
    PUSH  ECX
    PUSH  BX                            ; BX => BP points to buffer to write to; store it for later
    PUSH  BP
    CALL  FindFile                      ; find our file. ES:SI contains our filename
    CMP   AX,-1
    JNE   LoadFile1
    ; failed to find file
    POP   BP
    POP   BX
    POP   ECX
    MOV   AX,-1
    RET
LoadFile1:
    SUB   EDI,RootOffset
    SUB   EAX,RootOffset
    ; get starting cluster
    PUSH  WORD RootSegment              ; root segment loc
    POP   ES
    MOV   DX,WORD [ES:DI + 0001Ah]      ; DI points to file entry in root directory table. Refrence the table...
    MOV   WORD [Cluster],DX             ; file's first cluster
    POP   BX                            ; get location to write to so we dont screw up the stack
    POP   ES
    PUSH  BX                            ; store location for later again
    PUSH  ES
    CALL  LoadFAT
LoadFile2:
    ; load the cluster
    MOV   AX,WORD [Cluster]             ; cluster to read
    POP   ES                            ; bx:bp=es:bx
    POP   BX
    CALL  ClusterLBA
    XOR   CX,CX
    MOV   CL,BYTE [SectorsPerCluster]
    CALL  ReadSector 
    POP   ECX
    INC   ECX                           ; add one more sector to counter
    PUSH  ECX
    PUSH  BX
    PUSH  ES
    MOV   AX,FatSegment                 ;start reading from fat
    MOV   ES,AX
    XOR   BX,BX
    ; get next cluster
    MOV   AX,WORD [Cluster]             ; identify current cluster
    MOV   CX,AX                         ; copy current cluster
    MOV   DX,AX
    SHR   DX,0001h                      ; divide by two
    ADD   CX,DX                         ; sum for (3/2)
    MOV   BX,0                          ; location of fat in memory
    ADD   BX,CX
    MOV   DX,WORD [ES:BX]
    TEST  AX,0001h                      ; test for odd or even cluster
    JNZ   LoadFile3
    AND   DX,0000111111111111b          ; Even cluster - take low 12 bits
    JMP   LoadFile4
LoadFile3:
    SHR   DX,0004h                      ; Odd cluster  - take high 12 bits
LoadFile4:
    MOV   WORD [Cluster],DX
    CMP   DX,0FF0h                      ; test for end of file marker
    JB    LoadFile2
    ; We're done
    POP   ES
    POP   BX
    POP   ECX
    XOR   AX,AX
    RET

;--------------------------------------------------------------------------------------------------
; Stage 2 Entry Point
; - Set Data segment registers and stack
; - Install GDT
; - Enable A20
; - Read Stage3 into memory
; - Protected mode (pmode)
;--------------------------------------------------------------------------------------------------
[bits 16]
Main:
    ;----------------------------
    ; Set Data Segement registers
    ;----------------------------
    CLI                                 ; disable interrupts
    XOR   AX,AX                         ; null segments
    MOV   DS,AX
    MOV   ES,AX

    ;-----------------
    ; Set up our Stack
    ;-----------------
    MOV   AX,00h                        ; stack begins at 09000h-0FFFFh
    MOV   SS,AX
    MOV   SP,0FFFFh
    STI                                 ; enable interrupts

    ;----------------
    ; Install our GDT
    ;----------------
    CALL  InstallGDT

    ;-----------
    ; Enable A20
    ;-----------
    CALL  EnableA20

    ;----------------------
    ; Print loading message
    ;----------------------
    MOV   SI,LoadingMsg
    CALL  PutStr

    ;----------------------
    ; Initialize filesystem
    ;----------------------
    CALL  LoadRootDir                   ; Load root directory table

    ;----------------------
    ; Read Stage3 from disk
    ;----------------------
    MOV   EBX,0                         ; BX:BP points to buffer to load to
    MOV   BP,RModeBase
    MOV   SI,Stage3Name                 ; our file to load
    CALL  LoadFile
    MOV   DWORD [Stage3Size],ECX        ; save the size of Stage3
    CMP   AX,0                          ; Test for success
    JE    GoProtected                   ; yep--onto Stage 3!

    ;------------------
    ; This is very bad!
    ;------------------
    MOV   SI,FailureMsg                 ; Nope--print error
    CALL  PutStr                        ;
    MOV   AH,0                          ; wait
    INT   16h                           ;  for keypress
    INT   19h                           ; warm boot computer
    CLI                                 ; If we get here, something really went wrong
    HLT

GoProtected:
    MOV   SI,Stage3Msg
    CALL  PutStr
    MOV   ah,00h                        ; wait
    INT   16h                           ;  for keypress
    ;--------------
    ; Go into pmode
    ;--------------
    CLI                                 ; clear interrupts
    MOV   EAX,CR0                       ; set bit 0 in cr0--enter pmode
    OR    EAX,1
    MOV   CR0,EAX
    JMP   CodeDesc:GoStage3             ; far jump to fix CS. Remember that the code selector is 08h!

  ; Note: Do NOT re-enable interrupts! Doing so will triple fault!
  ; We will fix this in Stage 3.

;--------------------------------------------------------------------------------------------------
; Get to Stage3 - Our Kernel!
; - Set Data Segment Register
; - Set up our Stack
; - Copy Kernel to address 1 MB
; - Jump to our Kernel!!
;--------------------------------------------------------------------------------------------------
[bits 32]
GoStage3:
    ;----------------------------
    ; Set Data Segement registers
    ;----------------------------
    MOV   AX,DataDesc                   ; set data segments to data selector (10h)
    MOV   DS,AX
    MOV   SS,AX
    MOV   ES,AX

    ;-----------------
    ; Set up our Stack
    ;-----------------
    MOV   ESP,90000h                    ; stack begins from 90000h

    ;-------------------
    ; Copy kernel to 1MB
    ;-------------------
    MOV   EAX,DWORD [Stage3Size]
    MOVZX EBX,WORD [BytesPerSector]
    MUL   EBX
    MOV   EBX,4
    DIV   EBX
    CLD
    MOV   ESI,RModeBase
    MOV   EDI,PModeBase
    MOV   ECX,EAX
    REP   MOVSD                         ; copy image to its protected mode address

    ;--------------------
    ; Jump to our Kernel!
    ;--------------------
    JMP   CodeDesc:PModeBase            ; jump to our kernel! Note: This assumes Kernel's entry point is at 1 MB

    ;-------------------
    ; We never get here! 
    ;-------------------
    CLI                                 ; Stop 
    HLT                                 ;  execution

;--------------------------------------------------------------------------------------------------
; Global Descriptor Table (GDT)
; Tutorial 8: Protected Mode
;--------------------------------------------------------------------------------------------------
GDT1:
;----------------
; null descriptor
;----------------
                  DD  0
                  DD  0
NullDesc          EQU 0
;----------------
; code descriptor
;----------------
                  DW  0FFFFh            ; limit low
                  DW  0                 ; base low
                  DB  0                 ; base middle
                  DB  10011010b         ; access
                  DB  11001111b         ; granularity
                  DB  0                 ; base high
CodeDesc          EQU 8h
;----------------
; data descriptor
;----------------
                  DW  0FFFFh            ; limit low
                  DW  0                 ; base low
                  DB  0                 ; base middle
                  DB  10010010b         ; access
                  DB  11001111b         ; granularity
                  DB  0                 ; base high
DataDesc          EQU 10h
;-------------------
; pointer to our GDT
;-------------------
GDT2:
                  DW  GDT2-GDT1-1       ; limit (Size of GDT)
                  DD  GDT1              ; base of GDT

;--------------------------------------------------------------------------------------------------
; Working Storage
;--------------------------------------------------------------------------------------------------
FatSegment        EQU 2C0h
PModeBase         EQU 100000h           ; where the kernel is to be loaded to in protected mode
RModeBase         EQU 3000h             ; where the kernel is to be loaded to in real mode
RootOffset        EQU 2E00h
RootSegment       EQU 2E0h

LoadingMsg        DB  0Dh
                  DB  0Ah
                  DB  "MyOs v0.1.1 Stage 2"
                  DB  00h

Stage3Msg         DB  0Dh
                  DB  0Ah
                  DB  " Hit Enter to Jump to Stage 3"
                  DB  00h

FailureMsg        DB  0Dh
                  DB  0Ah
                  DB  "*** FATAL: MISSING OR CURRUPT STAGE3.BIN. Press Any Key to Reboot"
                  DB  0Dh
                  DB  0Ah
                  DB  0Ah
                  DB  00h


AbsoluteHead      DB  00h
AbsoluteSector    DB  00h
AbsoluteTrack     DB  00h
BytesPerSector    DW  512
Cluster           DW  0000h
DataSector        DW  0000h
DriveNumber       DB  0
HeadsPerCylinder  DW  2
Stage3Name        DB  "STAGE3  BIN"      ; kernel name (Must be 11 bytes)
Stage3Size        DB  0                  ; size of kernel image in bytes
NumberOfFATs      DB  2
ReservedSectors   DW  1
RootEntries       DW  224
SectorsPerCluster DB  1
SectorsPerFAT     DW  9
SectorsPerTrack   DW  18