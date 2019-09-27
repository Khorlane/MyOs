;**********************************************************
; Stage3.asm
;   A basic 32 bit binary kernel running
;
; Broken Thorn Entertainment
; Operating Systems Development Tutorial
; http://www.brokenthorn.com/Resources/OSDevIndex.html
;
; nasm -f bin Stage3.asm -o Stage3.bin -l Stage3.lst
;**********************************************************

[bits  32]                              ; 32 bit code
    ORG   100000h                       ; Kernel starts at 1 MB
    JMP   Stage3                        ; Jump to entry point

;--------------------------------------------------------------------------------------------------
; Video Routines
;--------------------------------------------------------------------------------------------------

;---------------
;- Color Codes -
;---------------
;  0 0 Black
;  1 1 Blue
;  2 2 Green
;  3 3 Cyan
;  4 4 Red
;  5 5 Purple
;  6 6 Brown
;  7 7 Gray
;  8 8 Dark Gray
;  9 9 Light Blue
; 10 A Light Green
; 11 B Light Cyan
; 12 C Light Red
; 13 D Light Purple
; 14 E Yellow
; 15 F White
; Example 3F
;         ^^
;         ||
;         ||- Foreground F = White
;         |-- Background 3 = Cyan

;------------------------------------------
; Routine to calculate video memory address
;   represented by the given Row,Col
;------------------------------------------
CalcVideoAddr:
    PUSHA                               ; Save registers
    XOR   EAX,EAX                       ; Row calculation
    MOV   AL,[Row]                      ;  row
    DEC   EAX                           ;  minus 1
    MOV   EDX,160                       ;  times
    MUL   EDX                           ;  160
    PUSH  EAX                           ;  save it
    XOR   EAX,EAX                       ; Col calculation
    MOV   AL,[Col]                      ;  col
    MOV   EDX,2                         ;  times
    MUL   EDX                           ;  2
    SUB   EAX,2                         ;  minus 2
    POP   EDX                           ; Add col calculation
    ADD   EAX,EDX                       ;  to row calculation
    ADD   EAX,VidMem                    ;  plus VidMem
    MOV   [VidAdr],EAX                  ; Save in VidAdr
    POPA                                ; Restore registers
    RET                                 ; Return to caller

;------------------------------
; Put a character on the screen
; EDI = address in video memory
;------------------------------
PutChar:
    PUSHA                               ; Save registers
    MOV   EDI,[VidAdr]                  ; EDI = Video Address
    MOV   DL,[Char]                     ; DL = character
    MOV   DH,[ColorAttr]                ; DH = attribute
    MOV   WORD [EDI],DX                 ; Move attribute and character to video display
    POPA                                ; Restore registers
    RET                                 ; Return to caller

;---------------------------------
; Print a null terminated string
; EBX = address of string to print
;---------------------------------
PutStr:
    PUSHA                               ; Save registers
    CALL  CalcVideoAddr                 ; Calculate video address
    XOR   ECX,ECX                       ; Clear ECX
    PUSH  EBX                           ; Copy the string address in EBX
    POP   ESI                           ;  to ESI
    MOV   CX,[ESI]                      ; Grab string length using ESI, stuff it into CX
    SUB   CX,2                          ; Subtract out 2 bytes for the length field
    ADD   ESI,2                         ; Bump past the length field to the beginning of string
PutStr1:
    MOV   BL,BYTE [ESI]                 ; Get next character
    CMP   BL,0Ah                        ; NewLine?
    JNE   PutStr2                       ;  No
    MOV   BYTE [Col],1                  ;  Yes, back to col 1
    INC   BYTE [Row]                    ;   and bump row by 1
    CALL  CalcVideoAddr                 ; Calculate video address
    JMP   PutStr3                       ; Continue
PutStr2:
    MOV   [Char],BL                     ; Stash our character
    CALL  PutChar                       ; Print it out
    ADD   DWORD [VidAdr],2              ; Bump Video Address by 2
    INC   BYTE [Col]                    ; Bump column by 1
PutStr3:
    INC   ESI                           ; Bump ESI to next character in our string
    LOOP  PutStr1                       ; Loop (Decrement CX each time until CX is zero)
    CALL  MoveCursor                    ; Update cursor (do this once after displaying the string, more efficient)
    POPA                                ; Restore registers
    RET                                 ; Return to caller

;-----------------------
; Update hardware cursor
;-----------------------
MoveCursor:
    PUSHA                               ; Save registers
    MOV   BH,BYTE [Row]                 ; BH = row
    MOV   BL,BYTE [Col]                 ; BL = col
    DEC   BH                            ; BH-- (Make row zero based)

    XOR   EAX,EAX                       ; Clear EAX
    MOV   ECX,TotCol                    ; ECX = TotCol
    MOV   AL,BH                         ; Row
    MUL   ECX                           ;  * TotCol
    ADD   AL,BL                         ;  + Col
    MOV   EBX,EAX                       ; Save result in EBX (BL,BH in particular)

    XOR   EAX,EAX                       ; Clear EAX
    MOV   DX,03D4h                      ; Set VGA port to  03D4h (Video controller register select)
    MOV   AL,0Fh                        ; Set VGA port-index 0Fh (cursor location low byte)
    OUT   DX,AL                         ; Write to the VGA port
    MOV   DX,03D5h                      ; Set VGA port to  03D5h (Video controller data)
    MOV   AL,BL                         ; Set low byte of calculated cursor position from above
    OUT   DX,AL                         ; Write to the VGA port

    XOR   EAX,EAX                       ; Clear EAX
    MOV   DX,03D4h                      ; Set VGA port to  03D4h (Video controller register select)
    MOV   AL,0Eh                        ; Set VGA port-index 0Fh (cursor location high byte)
    OUT   DX,AL                         ; Write to the VGA port
    MOV   DX,03D5h                      ; Set VGA port to  03D5h (Video controller data)
    MOV   AL,BH                         ; Set high byte of calculated cursor position from above
    OUT   DX,AL                         ; Write to the VGA port

    POPA                                ; Restore registers
    RET                                 ; Return to caller

;-------------
; Clear Screen
;-------------
ClrScr:
    PUSHA                               ; Save registers
    CLD                                 ; Clear DF Flag, REP STOSW increments EDI
    MOV   EDI,VidMem                    ; Set EDI to beginning of Video Memory
    XOR   ECX,ECX                       ; 2,000 'words'
    MOV   CX,2000                       ;  on the screen
    MOV   AH,[ColorAttr]                ; Set color attribute
    MOV   AL,' '                        ; We're going to 'blank' out the screen
    REP   STOSW                         ; Move AX to video memory pointed to by EDI, Repeat CX times, increment EDI each time
    MOV   BYTE [Col],1                  ; Set Col to 1
    MOV   BYTE [Row],1                  ; Set Row to 1
    POPA                                ; Restore registers
    RET                                 ; Return to caller

;-------------------
;Set Color Attribute
;-------------------
SetColorAttr:
    PUSHA                               ; Save registers
    MOV   AL,[ColorBack]                ; Background color (e.g. 3)
    SHL   AL,4                          ;  goes in highest 4 bits of AL
    MOV   BL,[ColorFore]                ; Foreground color in lowest 4 bits of BL (e.g. F)
    OR    EAX,EBX                       ; AL now has the combination of background and foreground (e.g. 3F)
    MOV   [ColorAttr],AL                ; Save result in ColorAttr
    POPA                                ; Restore registers
    RET                                 ; Return to caller

;--------------------------------------------------------------------------------------------------
; Install our IDT
;--------------------------------------------------------------------------------------------------
InstallIDT:
    CLI                                 ; Disable interrupts
    PUSHA                               ; Save registers
    LIDT  [IDT2]                        ; Load IDT into IDTR
    MOV   EDI,IDT1                      ; Set EDI to beginning of IDT
    MOV   CX,2048                       ; 2048 bytes in IDT
    XOR   EAX,EAX                       ; Set all 256 IDT entries to NULL (0h)
    REP   STOSB                         ; Move AL to IDT pointed to by EDI, Repeat CX times, increment EDI each time
    STI                                 ; Enable interrupts
    POPA                                ; Restore registers
    RET                                 ; All done!

;--------------------------------------------------------------------------------------------------
; Keyboard Routines
;--------------------------------------------------------------------------------------------------
KbRead:
    ;--------------
    ; Read scancode
    ;--------------
    MOV   ECX,2FFFFh                    ; Set count for LOOP
KbWait:
    IN    AL,064h                       ; Read 8042 Status Register (bit 1 is input buffer status (0=empty, 1=full)  
    TEST  AL,1                          ; If bit 1 
    JNZ   KbGetIt                       ;  go get scan code
    LOOP  KbWait                        ; Keep looping
    MOV   AL,0FFh                       ; No scan
    MOV   [KbChar],AL                   ;  code received
    RET                                 ; All done!
KbGetIt:
    IN    AL,060h                       ; Obtain scancode from
    MOV   [KbChar],AL                   ;   Keyboard I/O Port
    RET                                 ; All done!
    ;-------------------
    ; Translate scancode
    ;-------------------
KbXlate:
    MOV   AL,[KbChar]                   ; Translate
    CMP   AL,010h                       ;  010h, the 
    JE    KbKeyQ                        ;   scancode 
    JMP   KbXlateDone                   ;    for the Q key,
KbKeyQ:                                 ;     to the
    MOV   AL,051h                       ;      ASCII code
    MOV   [KbChar],AL                   ;        for an
KbXlateDone:                            ;         uppercase
    RET                                 ;          letter Q

;---------
; Hex Dump
;---------
HexDump:
    PUSHA                               ; Save registers
    MOV   ECX,8                         ; Blank
    MOV   ESI,Buffer+2                  ;  out
    MOV   AL,020h                       ;   the
HexDump1:
    MOV   [ESI],AL                      ;    8
    INC   ESI                           ;     byte
    LOOP  HexDump1                      ;      Buffer
    MOV   ECX,8                         ; Setup
    XOR   EDX,EDX                       ;  for translating
    MOV   DL,[KbChar]                   ;   the keyboard
    MOV   EBX,HexDigits                 ;    scan code
    MOV   ESI,Buffer+9                  ;     to hex display
HexDump2:
    MOV   AL,DL                         ; Translate
    AND   AL,15                         ;  each
    XLAT                                ;   hex
    MOV   [ESI],AL                      ;    digit
    DEC   ESI                           ;     and put
    SHR   EDX,4                         ;      it in
    LOOP  HexDump2                      ;       Buffer
    POPA                                ; Restore registers
    RET                                 ; Return to caller

;--------------------------------------------------------------------------------------------------
; Stage3 - Our Kernel!
;--------------------------------------------------------------------------------------------------
Stage3:
    ;--------------
    ; Set registers
    ;--------------
    MOV   AX,10h                        ; Set data
    MOV   DS,AX                         ;  segments to
    MOV   SS,AX                         ;   data selector
    MOV   ES,AX                         ;    (10h)
    MOV   ESP,90000h                    ; Stack begins from 90000h

    CALL  InstallIDT                    ; Install our Interrupt Descriptor Table

    ;-------------
    ; Clear screen
    ;-------------
    MOV   BYTE [ColorBack],Black        ; Background color
    MOV   BYTE [ColorFore],Purple       ; Foreground colr
    CALL  SetColorAttr                  ; Set color
    CALL  ClrScr                        ; Clear screen
    
    ;--------------
    ; Print success
    ;--------------
    MOV   BYTE [Row],10                 ; Set Row,Col
    MOV   BYTE [Col],1                  ;  to 10,1
    MOV   EBX,Msg1                      ; Put
    CALL  PutStr                        ;  Msg1
    MOV   EBX,NewLine                   ; Put
    CALL  PutStr                        ;  a New Line
    MOV   EBX,Msg2                      ; Put
    CALL  PutStr                        ;  Msg2
    
    ;-------------------
    ; Get Keyboard input
    ;-------------------
    MOV   BYTE [Row],0                  ; Set starting
    MOV   BYTE [Col],1                  ;  Row, Col for hex output
    CLI                                 ; No Interrupts!
GetKey:
    MOV   AL,[Row]                      ; If Row is
    CMP   AL,25                         ;  25 or more
    JL    GetKey1                       ;   reset it
    MOV   BYTE [Row],0                  ;    to zero
GetKey1:
    CALL  KbRead                        ; Read the keyboard
    MOV   AL,[KbChar]                   ; If nothing
    CMP   AL,0FFh                       ;  read then
    JE    GetKey                        ;   jump back
    CALL  HexDump                       ; Translate to hex display
    INC   BYTE [Row]                    ; Bump row by 1
    MOV   BYTE [Col],1                  ; Reset col to 1
    MOV   EBX,Buffer                    ; Put hex out at upper left
    CALL  PutStr                        ;  corner of the screen
    CALL  KbXlate                       ; Translate scancode to ASCII
    MOV   BYTE [Col],1                  ; Reset col to 1
    INC   BYTE [Row]                    ; Put the
    CALL  CalcVideoAddr                 ;  keyboard
    MOV   BL,[KbChar]                   ;   character
    MOV   [Char],BL                     ;    on the
    CALL  PutChar                       ;     next row
    MOV   BL,[KbChar]                   ; Quit
    CMP   BL,051h                       ;  when Q (ASCII 051h)
    JE    AllDone                       ;   is
    JMP   GetKey                        ;    pressed

AllDone:
    ;---------------
    ; Print shutdown
    ;---------------
    MOV   EBX,NewLine                   ; Put
    CALL  PutStr                        ;  a New Line
    MOV   EBX,NewLine                   ; Put
    CALL  PutStr                        ;  a New Line
    MOV   EBX,Msg3                      ; Put
    CALL  PutStr                        ;  Msg3

    ;---------------
    ; Stop execution
    ;---------------
    CLI
    HLT

;--------------------------------------------------------------------------------------------------
; Interrupt Descriptor Table (IDT)
;--------------------------------------------------------------------------------------------------
IDT1:
TIMES 2048  DB 0                        ; The IDT is exactly 2048 bytes - 256 entries 8 bytes each
;-------------------
; pointer to our IDT
;-------------------
IDT2:
                  DW  IDT2-IDT1-1       ; limit (Size of IDT)
                  DD  IDT1              ; base of IDT

;--------------------------------------------------------------------------------------------------
; Working Storage
;--------------------------------------------------------------------------------------------------
%macro String 2
%1          DW  %%EndStr-%1
            DB  %2
%%EndStr:
%endmacro
String  Msg1,"------   MyOs v0.1.2   -----"
String  Msg2,"------  32 Bit Kernel  -----"
String  Msg3,"Our Kernel has ended!!"
String  NewLine,0Ah
String  Buffer,"XXXXXXXX"

ColorBack   DB  0                       ; Background color (00h - 0Fh)
ColorFore   DB  0                       ; Foreground color (00h - 0Fh)
ColorAttr   DB  0                       ; Combination of background and foreground color (e.g. 3Fh 3=cyan background,F=white text)
Char        DB  0                       ; ASCII character
KbChar      DB  0                       ; Keyboard character
Row         DB  0                       ; Row (1-25)
Col         DB  0                       ; Col (1-80)
VidAdr      DD  0                       ; Video Address
HexDigits   DB  "0123456789ABCDEF"

;--------------------------------------------------------------------------------------------------
; Equates
;--------------------------------------------------------------------------------------------------
VidMem      EQU 0B8000h                 ; Video Memory (Starting Address)
TotCol      EQU 80                      ; width and height of screen
Black       EQU 00h                     ; Black
Cyan        EQU 03h                     ; Cyan
Purple      EQU 05h                     ; Purple
White       EQU 0Fh                     ; White