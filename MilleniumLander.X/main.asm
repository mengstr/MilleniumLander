;
; MilleniumLander - A lunar lander style game for the Hackaday 1kB competition
; Copyright (c) 2016 Mats Engstrom 
;
; Licesed under the MIT LICENSE - See LICENSE.txt in parent folder
;
; Code available at github.com/SmallRoomLabs/MilleniumLander
;
; Max allowed code size is 585 (0x249) words @ 14 bits
;
    
#define UART    1
    
#include <p16f1705.inc>
    
    __CONFIG   _CONFIG1, _FOSC_INTOSC & _WDTE_OFF & _PWRTE_ON & _MCLRE_ON & _CP_OFF & _BOREN_OFF & _CLKOUTEN_OFF & _IESO_OFF & _FCMEN_OFF
    __CONFIG   _CONFIG2, _WRT_OFF & _PPS1WAY_OFF & _ZCDDIS_ON & _PLLEN_ON &  _STVREN_ON & _BORV_LO & _LPBOR_OFF & _LVP_ON

    radix dec
    errorlevel -302     ; suppress "Not in bank 0" warning message

    
ResetVector code 0x0000
    GOTO    Main

InterrupVector code 0x0004
    RETFIE

;----------------------------------------------------------------------------

#define DISPCNT 10
#define DISPLEN 8
    
;----------------------------------------------------------------------------
; Variable declarations
;----------------------------------------------------------------------------
    errorlevel -207     ; Turn off the annoying label-not-in-column-1 warnings
            
dum1 udata
    randomH	res 1
    randomL	res 1

    binU    res 1
    binH    res 1
    binL    res 1
    cycles  res 1
    displays res 1
    BINCNT  res 1
    DIGITCNT  res 1
    DECCNT  res 1
    
    RegAE   res 1
    RegA0   res 1
    RegA1   res 1
    RegA2   res 1
    bcdU    res 1
    bcdH    res 1
    bcdL    res 1
    count   res 1

displaybuffers udata
    dispbuf res DISPCNT*DISPLEN

#define display0   dispbuf+DISPLEN*0 
#define display1   dispbuf+DISPLEN*1 
#define display2   dispbuf+DISPLEN*2 
#define display3   dispbuf+DISPLEN*3 
#define display4   dispbuf+DISPLEN*4 
#define display5   dispbuf+DISPLEN*5 
#define display6   dispbuf+DISPLEN*6 
#define display7   dispbuf+DISPLEN*7 
#define display8   dispbuf+DISPLEN*8 
#define display9   dispbuf+DISPLEN*9 
 
dum2 udata_shr 	
    altitude	res 2
    latitude    res 2
    vspeed      res 1
    hspeed      res 1
    fuel        res 2
    angle       res 1
    wind        res 1
    targetA     res 2
    targetL     res 2
    score       res 2 

    errorlevel +207     ; Turn warnings back on again
;----------------------------------------------------------------------------
NEG16 MACRO DST
    comf    DST+0,F
    comf    DST+1,F
    movlw   0
    bsf     STATUS,C 
    addwfc  DST+0,F
    addwfc  DST+1,F     
    ENDM

    code

#ifdef UART

#define BACKSPACE  8
    
GetHeaderChar:
    addwf PCL
    dt "\nMillenium Lander\n\n"
    dt "Altitude "
    dt "Latitude "
    dt "  Vspeed "
    dt "  Hspeed "
    dt "    Fuel "
    dt "   Angle "
    dt "    Wind "
    dt " TargetA "
    dt " TargetL "
    dt "   Score "
    dt "\n",0

PrintHeader:
    banksel DECCNT
    CLRF    DECCNT
PrintHeaderLoop:
    movf    DECCNT,W
    incf    DECCNT,F
    call    GetHeaderChar
    xorlw   0x00            ; Exit when got 0x00
    btfsc   STATUS,Z
    return
    banksel TX1REG
    movwf   TX1REG
    GOTO    PrintHeaderLoop
;
; Initialize the HW UART for Console Debugging
;
InitUART:
    movlw   0x20        ; Enable EUSART Async TX
    banksel TX1STA
    movwf   TX1STA
    movlw   0x90        ; Enable EUSART Async RX
    banksel RC1STA
    movwf   RC1STA
    movlw   0x00        ; EUSART continous mode
    banksel BAUD1CON
    movwf   BAUD1CON
    return

PrintDisplayBuf:
    movlw   BACKSPACE
    banksel TX1REG
    movwf   TX1REG

    banksel FSR0H
    movlw   HIGH(dispbuf+7)
    movwf   FSR0H
    movlw   LOW(dispbuf+7)
    movwf   FSR0L
    banksel displays
    movlw   DISPCNT     ; Number of displays
    movwf   displays
pdb0:
    banksel cycles
    movlw   DISPLEN       ; 8 digits on each display
    movwf   cycles
pdb1:
    movf    INDF0,W
    decf    FSR0L,F
    addlw   0x30
    banksel TX1REG
    movwf   TX1REG
    banksel cycles
    decfsz  cycles,F
    goto    pdb1
    movlw   ' '
    banksel TX1REG
    movwf   TX1REG

    movlw   DISPLEN*2     ; Jump to end of next display
    addwf   FSR0,F

    banksel displays
    decfsz  displays,F
    goto    pdb0
    return
    
#endif

    ;
    ; Takes about 3.25ms with all 0  
    ;
RefreshDisplay:
    banksel FSR0H           ; The display location will autoincrement correctly
    movlw   HIGH(display0)  ; so we only need to set this once
    movwf   FSR0H
    movlw   LOW(display0)
    movwf   FSR0L

    banksel binU
    clrf    binU            ; This only needs to be cleared before first call 
                            ; since the Uint24ToAscii will clear them at exit

    movf    altitude+0,W
    movwf   binL
    movf    altitude+1,W
    movwf   binH
    call    Uint24ToAscii
    
    banksel binU
    movf    latitude+0,W
    movwf   binL
    movf    latitude+1,W
    movwf   binH
    call    Uint24ToAscii

    banksel binU
    movf    vspeed,W
    movwf   binL
    call    Uint24ToAscii

    banksel binU
    movf    hspeed,W
    movwf   binL
    call    Uint24ToAscii
    
    banksel binU
    movf    fuel+0,W
    movwf   binL
    movf    fuel+1,W
    movwf   binH
    call    Uint24ToAscii

    banksel binU
    movf    angle,W
    movwf   binL
    call    Uint24ToAscii

    banksel binU
    movf    wind,W
    movwf   binL
    call    Uint24ToAscii
    
    banksel binU
    movf    targetA+0,W
    movwf   binL
    movf    targetA+1,W
    movwf   binH
    call    Uint24ToAscii

    banksel binU
    movf    targetL+0,W
    movwf   binL
    movf    targetL+1,W
    movwf   binH
    call    Uint24ToAscii

    banksel binU
    movf    score+0,W
    movwf   binL
    movf    score+1,W
    movwf   binH
    call    Uint24ToAscii

#ifdef UART 
    call    PrintDisplayBuf
#endif
    return

    
    
;   
; .Altitude  8976..9999      8976+rand()&0x3FF
; ?Latitude  -1023..1023     rand()&0x3FF , random negate
; .TargetA   0..1023         rand()&0x3FF
; ?TargetL   -511..511       rand()&0x1FF , random negate
; .Fuel      1000            1000
; ?Wind      -64..64         rand()0x3F , random negate
; .Vspeed    0
; .Hspeed    0
; .Angle     0
; .Score     0
;
;
InitGame:
    clrf    score+0     ; Just these to zero
    clrf    score+1
    clrf    vspeed  
    clrf    hspeed  
    clrf    angle   
    movlw   LOW(1000)
    movwf   fuel+0
    movlw   HIGH(1000)
    movwf   fuel+1
    
    call    Random      ; Altitude 8976..9999
    movf    randomL,W     ; ...first store 0..1023 random
    movwf   altitude+0
    movf    randomH,W
    andlw   0x03  
    movwf   altitude+1
    movlw   LOW(8976)   ; ...then add 8976
    addwf   altitude+0,F
    movlw   HIGH(8976)
    addwfc  altitude+1,F
    
    call    Random      ; Latitude -1023..1023
    movf    randomL,W     ; ...first store 0..1023 random
    movwf   latitude+0
    movf    randomH,W
    andlw   0x03  
    movwf   latitude+1
    ;TODO NEGATE

    call    Random      ; TargetA 0..1023
    movf    randomL,W
    movwf   targetA+0
    movf    randomH
    andlw   0x03  
    movwf   targetA+1
    
    call    Random      ; TargetL -511..511
    movf    randomL,W
    movwf   targetL+0
    movf    randomH,W
    andlw   0x01   
    movwf   targetL+1
    ;TODO NEGATE

    
    call    Random      ; Wind -63..63
    movf    randomL,W
    andlw   0x3F   
    movwf   wind
    ;TODO NEGATE

    return
    
    
 ;
;
;
Main:
    banksel randomH
    INCF    randomH, F

#ifdef UART 
    call    InitUART
    call    PrintHeader
    call    PrintDisplayBuf
#endif
    
 ;   banksel binL
 ;   movlw   LOW(12345)
 ;   movwf   binL
 ;   movlw   HIGH(12345)
 ;   movwf   binH

 ;   call    Bin16ToBCD5
    
 ;   banksel TX1REG
 ;   swapf   bdcU,W
 ;   bcdU
 ;   bcdH
 ;   bdcL
 ;   movwf   TX1REG
    
 ;   movlw   0x0a
 ;   movwf   TX1REG


foo:
    call    InitGame
    call    RefreshDisplay
    CALL    Random
    GOTO    foo

    
    
;
; 16-bit Binary To 5-digit Packed BCD
;
; input  = binH:binL      = 16-bit binary number
; output = bcdU:bcdH:bcdL = 5-digit packed BCD number
;
Bin16ToBCD5:
    banksel binL
    clrf    bcdL            ; clear bcd result
    clrf    bcdH
    clrf    bcdU
    movlw   .16             ; 16 bits counter
    movwf   count
    goto    bcd0            ; branch to reduce 16 Tcy
        
bcd_lp:
    movlw   0x33            ; adjust Tens:Ones digits
    addwf   bcdL
    btfsc   bcdL,3
    andlw   0xF0
    btfsc   bcdL,7
    andlw   0x0F
    subwf   bcdL

    movlw   0x33            ; adjust Thou:Hund digits
    addwf   bcdH
    btfsc   bcdH,3
    andlw   0xF0
    btfsc   bcdH,7
    andlw   0x0F
    subwf   bcdH
        
;       movlw   0x03            ; adjust TenK digit
;       addwf   bcdU
;       btfss   bcdU,3
;       subwf   bcdU
        
bcd0:
    rlf     binL            ; shift out a binary bit
    rlf     binH
    rlf     bcdL            ; into bcd result (double)
    rlf     bcdH
    rlf     bcdU

    decfsz  count           ; repeat for all bits
    goto    bcd_lp
    return                  ; exit    
;
;
;
;//Input: 16 bit unsigned number as RegA1,2 (RegA2 low byte, RegA1 High byte)
;//Division result: Reg1 and Reg2 and reminder as char in WREG
divR16by_c10:
    banksel RegA2
    clrf    RegA0
    movlw   16             ;//init loop counter
    movwf   RegAE
    lslf    RegA2, f
divI16by_c10_:     
    rlf     RegA1, f
    rlf     RegA0, f       
    movlw   10
    subwf   RegA0, f
    btfsc   STATUS,C
    bra     divI16by_c10_OK
    addwfc  RegA0, f
    bcf     STATUS,C
divI16by_c10_OK:
    rlf     RegA2, f
    decfsz  RegAE, f
    bra     divI16by_c10_
    ;//result= W from 0..9
    addlw   0x30        ;//convert to char
    return 

;
;
;
Random:
    banksel randomH
    BCF	    STATUS,C    
    RRF	    randomH,F
    RRF	    randomL,F
    BTFSS   STATUS,C
    RETURN
    MOVLW   0xA1
    XORWF   randomH,F
    XORWF   randomL,F    
    RETURN

;
; Converts 24 bit binary held at binU,binH,binL into
; an ASCII string pointed to by FSR0H,FSR0L
;
Uint24ToAscii:
    banksel DECCNT
	movlw   DISPLEN     ; Clear display buffer
	movwf   DECCNT
_clr1:
    movlw   0x00        ; Store zero 
    movwf   INDF0
	incf    FSR0,F
	decfsz  DECCNT,F    ; Loop until all digits
	goto    _clr1  

    movlw   24          ; Decimal count
	movwf   BINCNT

BITLP:
    movlw   DISPLEN     ; Go back to beginning of display buf again...
    subwf   FSR0,F      ; ...ajusts bot for the clear above and the rounds
	rlf     binL,F      ; Shift binary left
	rlf     binH,F     
	rlf     binU,F
	movlw   DISPLEN     ; Count for the decimal digits
	movwf   DECCNT
	movlw   6           ; The Working Register holds 6 throughout. For each bit the inner loop is repeated 8 times, with shift in of the next bit, "times 2" and DecAdj of each digit

ADJLP:
	RLF     INDF0,F     ; 2*digit, then shift in "next bit?? for DIGIT0 or else the carry from the previous digit
	ADDWF   INDF0,F     ; Add 6, clears Cf and gives 1 in bit 4 if the
	BTFSS   INDF0,4     ; addition is needed; zero if not, when
	SUBWF   INDF0,F     ; we subtract it again. Sets Cf
	BSF     STATUS,C    ; Cf could be 0 or 1, so make it 1 as default
	BTFSS   INDF0,4     ; Bit 4 is the carry to the next digit
	BCF     STATUS,C    ; Reset Cf to zero if bit 4 is clear
	BCF     INDF0,4     ; For BCD clear bit 4 in case it?s one
	INCF    FSR0,F      ; Go to next digit, (Cf not affected)
	DECFSZ  DECCNT,F    ; End of inner loop. check digit count and
	GOTO    ADJLP       ; ...round again if it?s not zero
    DECFSZ  BINCNT,F    ; End of outer loop, one pass through digits,
	GOTO    BITLP       ; ...check bit count and repeat if necessary.
    clrf    binU        ; Clear the input registers to save code for next call
    clrf    binH
    clrf    binL
	return
;
; Core registers    
;  INDF0 INDF1 PCL STATUS FSR0L FSR0H FSR1L FSR1H BSR WREG PCLATH INTCON
;
    end

