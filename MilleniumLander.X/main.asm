;
; MilleniumLander - A lunar lander style game for the Hackaday 1kB competition
; Copyright (c) 2016 Mats Engstrom 
;
; Licesed under the MIT LICENSE - See LICENSE.txt in parent folder
;
; Code available at github.com/SmallRoomLabs/MilleniumLander
;
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
    errorlevel -207     ; Turn off label-not-in-column-1 warnings
                        ; for the variable declarations
            
dum1 udata
    randomH	res 1
    randomL	res 1

    BIN0    res 1
    BIN1    res 1
    BIN2    res 1
    cycles  res 1
    BINCNT  res 1
    DIGITCNT  res 1
    DECCNT  res 1
    digit1  res 1
    digit2  res 1
    digit3  res 1
    digit4  res 1
    digit5  res 1
    digit6  res 1
    digit7  res 1
    digit8  res 1
    
    RegAE   res 1
    RegA0   res 1
    RegA1   res 1
    RegA2   res 1
    binH    res 1
    binL    res 1
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

    errorlevel +207     ; Turn warnings back on again
;----------------------------------------------------------------------------
 
     code
;
;
;
Main:
    banksel randomH
    INCF    randomH, F

#ifdef UART ; Initialize the HW UART for Console Debugging
    movlw   0x20        ; Enable EUSART Async TX
    banksel TX1STA
    movwf   TX1STA
    movlw   0x90        ; Enable EUSART Async RX
    banksel RC1STA
    movwf   RC1STA
    movlw   0x00        ; EUSART continous mode
    banksel BAUD1CON
    movwf   BAUD1CON
    banksel TX1REG
    movlw   'M'
    movwf   TX1REG
    movlw   'i'
    movwf   TX1REG
    movlw   'l'
    movwf   TX1REG
    movlw   'l'
    movwf   TX1REG
    movlw   'e'
    movwf   TX1REG
    movlw   'n'
    movwf   TX1REG
    movlw   'i'
    movwf   TX1REG
    movlw   'u'
    movwf   TX1REG
    movlw   'm'
    movwf   TX1REG
    movlw   ' '
    movwf   TX1REG
    movlw   'L'
    movwf   TX1REG
    movlw   'a'
    movwf   TX1REG
    movlw   'n'
    movwf   TX1REG
    movlw   'd'
    movwf   TX1REG
    movlw   'e'
    movwf   TX1REG
    movlw   'r'
    movwf   TX1REG
    movlw   0x0a
    movwf   TX1REG
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

    banksel BIN0
    movlw   LOW(14345678>>0)
    movwf   BIN0
    movlw   LOW(14345678>>8)
    movwf   BIN1
    movlw   LOW(14345678>>16)
    movwf   BIN2

    call    bin2dec
 
 
    banksel digit8
    movf    digit8,W
    addlw   0x30
    banksel TX1REG
    movwf   TX1REG
    
    
    banksel digit7
    movf    digit7,W
    addlw   0x30
    banksel TX1REG
    movwf   TX1REG
    
    
    banksel digit6
    movf    digit6,W
    addlw   0x30
    banksel TX1REG
    movwf   TX1REG

    banksel digit5
    movf    digit5,W
    addlw   0x30
    banksel TX1REG
    movwf   TX1REG

    banksel digit4
    movf    digit4,W
    addlw   0x30
    banksel TX1REG
    movwf   TX1REG
 
    
    banksel digit3
    movf    digit3,W
    addlw   0x30
    banksel TX1REG
    movwf   TX1REG

    
    banksel digit2
    movf    digit2,W
    addlw   0x30
    banksel TX1REG
    movwf   TX1REG
 
    banksel digit1
    movf    digit1,W
    addlw   0x30
    banksel TX1REG
    movwf   TX1REG

    banksel TX1REG
    movlw   0x0a
    movwf   TX1REG


foo:
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

    
bin2dec:
    clrf	digit1
	clrf	digit2
	clrf	digit3
	clrf	digit4
	clrf	digit5
	clrf	digit6
	clrf	digit7
	clrf	digit8	
    MOVLW 24       ; Decimal count
	MOVWF BINCNT

BITLP:
	RLF BIN0,F     ; Shift binary left
	RLF BIN1,F     
	RLF BIN2,F
    movlw HIGH(digit1)
	MOVWF FSR0H
    MOVLW digit1
	MOVWF FSR0L
	MOVLW 8        ; Count for the decimal digits
	MOVWF DECCNT
	MOVLW 6        ; The Working Register holds 6 throughout. For each bit the inner loop is repeated 8 times, with shift in of the next bit, "times 2" and DecAdj of each digit

ADJLP:
	RLF INDF0,F     ; 2*digit, then shift in "next bit?? for DIGIT0 or else the carry from the previous digit
	ADDWF INDF0,F   ; Add 6, clears Cf and gives 1 in bit 4 if the
	BTFSS INDF0,4   ; addition is needed; zero if not, when
	SUBWF INDF0,F   ; we subtract it again. Sets Cf
	BSF STATUS,C   ; Cf could be 0 or 1, so make it 1 as default
	BTFSS INDF0,4   ; Bit 4 is the carry to the next digit
	BCF STATUS,C   ; Reset Cf to zero if bit 4 is clear
	BCF INDF0,4     ; For BCD clear bit 4 in case it?s one
	INCF FSR0,F     ; Go to next digit, (Cf not affected)
	DECFSZ DECCNT,F ; End of inner loop. check digit count and
	GOTO ADJLP     ; round again if it?s not zero
	DECFSZ BINCNT,F ; End of outer loop, one pass through digits,
	GOTO BITLP     ; check bit count and repeat if necessary.
	RETURN
    

    end
