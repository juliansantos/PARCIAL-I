;Author: Julian Santos B
;Description: Scheduller
    #include "p16F628A.inc"
    #define pindisplay1 PORTA,RA0;Base transistor 1
    #define pindisplay2 PORTA,RA1;Base transistor 2
    #define pindisplay3 PORTA,RA2;Base transistor 3 
    #define pindisplay4 PORTA,RA3;Base transistor 4
    #define pinDATA PORTB,RB6;Data (anode-> 7 segment) 
    #define pinLATCH PORTB,RB5;To latch the data in 595
    #define pinCLOCK PORTB,RB7 ;Clock for 595
    #define debouncing 0 ; Bit 0 for enabling debouncing
    #define buzzer 1 ;Bit 1 for enabling debouncing
    #define dysdata 2 ;Bit 2 for enabling show data 
    #define pinbuzzer PORTB,RB7
    
 __CONFIG _FOSC_XT & _WDTE_OFF & _PWRTE_OFF & _MCLRE_ON & _BOREN_OFF & _LVP_OFF & _CPD_OFF & _CP_OFF
 
    CBLOCK 0x20
    stack:8 ; Stack
    datatmp
    Task ; bit
    counterdebounce ; var to how often this subroutine is done
    counterdisplay
    counterbuzzer
    basetransistor ; states of base tran
    count8
    statustmp
    wtemp
    data595
    delayvar:3
    ENDC
    
    org 0x00 ; Start of the program 
    goto main 
    org 0x08 ;Interrupt vector high priority interrupts 
    goto ISR  

;*************************************************************FOREGROUND RoUTINE
main:
    call initialconfig
    call initialstates
    call initINT0
    call initTMR0
    
task:   
    
;    bcf PORTA,RA2; 
;    movlw d'30'
;    call delayW0ms
;    bsf PORTA,RA2
;    movlw d'30'
;    call delayW0ms
    
;    movlw 0xAA   ;SENDING DATA SERIALYS
;    movwf data595;send serial data to 595
;    call send8
;    call latch
    ;polling flags (task)
;    btfsc Task,debouncing
;    call rdebouncing
;    btfsc Task,buzzer
   ; call togglebuzzer; Bit toggle pin buzzer
    btfsc Task,dysdata
    call showdigit ; Show digit display
    goto task

rdebouncing: ; ----------------------------------------
    bsf INTCON,INTF ; Enabling interrupt flag
    return
togglebuzzer: ; ------------------------------subroutine for toggling the buzzer
    clrf counterbuzzer
    bcf Task,buzzer
    movlw b'10000000' ; Just for toggling
    xorwf PORTB
    return
showdigit: ; --------------------------------subroutine for show data in 4(7seg)
    clrf counterdisplay
    bcf STATUS,C
    rlf basetransistor,F
    movlw 0x01
    btfsc basetransistor,4 ;test if a rolls over occurred
    movwf basetransistor
    bcf pindisplay4
    bcf pindisplay3
    bcf pindisplay2
    bcf pindisplay1
    call sendata ; sending data to 595
    
    btfsc basetransistor,3 ;turn on display 4?
    bsf pindisplay4
    btfsc basetransistor,2 ;turn on display 3?
    bsf pindisplay3
    btfsc basetransistor,1 ;turn on display 2? 
    bsf pindisplay2
    btfsc basetransistor,0 ;turn on display 1?
    bsf pindisplay1
    bcf Task,dysdata
   ; goto $
    return
    
sendata: ;----------------------------subruoutine to send serial data to display
    btfsc basetransistor,3 ;turn on display 4?
    movf stack+3,W
    btfsc basetransistor,2 ;turn on display 3?
    movf stack+2,W
    btfsc basetransistor,1 ;turn on display 2? 
    movf stack+1,W
    btfsc basetransistor,0 ;turn on display 1?
    movf stack,W
    movwf datatmp
    bcf INTCON,GIE
    call tabla 
   ; movlw b'11111100'
    movwf data595;send serial data to 595
    call send8
    call latch
    bsf INTCON,GIE
    return

send8:
    bcf STATUS,C
    rrf data595,F ; LSB -> C
    btfsc STATUS,C
    bsf pinDATA ; Carry =1 then Data=1 
    btfss STATUS,C
    bcf pinDATA ; Carry =0 then Data=0
    incf count8,F ; Increment counter
    call pclock ; sending a positive edge 9clock)
    btfss count8,3 ;test if count=8
    goto send8 ;else, continue rotating
    clrf count8 ; clear counter 8 bit
    return    
    
pclock:
    bsf pinCLOCK
    bcf pinCLOCK
    return
;-----------------------------------------------Subrutine for latch data (clock)    
latch:
    bsf pinLATCH
    bcf pinLATCH    
    return    
    
initialconfig: ;----------------------------------------subrutine for MCU config
    bcf STATUS,RP0; Bank 0
    bcf STATUS,RP1 
    movlw 0x07
    movwf CMCON ; pin as digital
    bsf STATUS,RP0; Bank 1
    bcf STATUS,RP1 
    clrf TRISA ; Setting data direction PortA
    clrf TRISB ; Setting data direction PortB
    bcf STATUS,RP0 ;Bank 0 
    bcf STATUS,RP1
    return
    
initialstates: ;-------------------------subrutine for setting the initialstates   
    movlw 'H'
    movwf stack   ; initial states of the stack
    movlw '0'
    movwf stack+1
    movlw 'L'
    movwf stack+2
    movlw 'A'
    movwf stack+3
    clrf stack+4
    clrf stack+5
    clrf stack+6
    clrf stack+7 
    bcf pindisplay1 ; Turn off displays
    bcf pindisplay2
    bcf pindisplay3
    bcf pindisplay4
    bcf pinCLOCK ; idle state 595
    bcf pinDATA
    bcf pinLATCH
    bcf pinbuzzer
    clrf counterbuzzer
    clrf counterdisplay 
    clrf Task ;initial Task disemable
    movlw 0x01
    movwf basetransistor
    clrf count8
    return
    
initTMR0: ;-----------------------------------subrutine for setting the TIMER 0
    bsf STATUS,RP0; Bank 1
    bcf STATUS,RP1
    movlw b'11000000' ; disenable pull-up,  rising edge INT0, CS=0, PS=1:2
    movwf OPTION_REG
    bcf STATUS,RP0; Bank 0
    bcf STATUS,RP1
    movlw -d'25' ; 50useg   : Tosc/4=1us
    movwf TMR0 
    bcf INTCON,T0IF ; Clearing interrupt flag timer 0
    bsf INTCON,T0IE ; Enabling interrupt timer 0
    bsf INTCON,GIE ; Enabling globla interrupt
    return
    
initINT0: ;-------------------------subrutine for setting the external interrupt
    return
    
;**************************************************************BACKGROUND RUTINE    
ISR:
    ; context saving (WREG, STATUS)
    movwf wtemp
    movf STATUS,W
    movwf statustmp
    
    btfsc INTCON,T0IF
    call TMR0ISR
    ;btfsc INTCON,INTF
    ;call INT0ISR
    ;btfsc usart
    ;Restore context saving (WREG, STATUS)
    movf statustmp,W
    movwf STATUS
    movf wtemp,W
    retfie  ; Restoring the status before the interrupt
 
TMR0ISR:
    incf counterbuzzer,F      ;| counterbuzzer++
    movlw 0x05                ;| counterbuzzer=5?
    subwf counterbuzzer,W     ;| cycles=5
    btfsc STATUS,Z            ;|
    bsf Task,buzzer           ;|
    
    incf counterdisplay,F     ;| counterdisplay++
    movlw d'63'	              ;| counterdisplay=62.5? 'LOL'		
    subwf counterdisplay,W    ;| cycles=5
    btfsc STATUS,Z	      ;|
    bsf Task,dysdata          ;|

    movlw -d'20' ; 50useg (50)/2   (50-2-2-2-5-5)/2
    ;MOVLW 0X00
    movwf TMR0 
    bcf INTCON,T0IF ;Clearing interrupt flag TMR0
    return
    
INT0ISR: 
    ; Turn on anti-debouncing task
    bsf Task,debouncing
    bcf INTCON,INTF ;Clearing interrupt flag external interrupt
    bcf INTCON,INTE
    return
    
tabla:  ; Table for display data in a 7segment display
    movlw '0' 
    subwf datatmp,W
    btfsc STATUS,Z
    goto num0
    movlw '1' 
    subwf datatmp,W
    btfsc STATUS,Z
    goto num1
    movlw '2' 
    subwf datatmp,W
    btfsc STATUS,Z
    goto num2
    movlw '3' 
    subwf datatmp,W
    btfsc STATUS,Z
    goto num3
    movlw '4' 
    subwf datatmp,W
    btfsc STATUS,Z
    goto num4
    movlw '5' 
    subwf datatmp,W
    btfsc STATUS,Z
    goto num5
    movlw '6' 
    subwf datatmp,W
    btfsc STATUS,Z
    goto num6
    movlw '7' 
    subwf datatmp,W
    btfsc STATUS,Z
    goto num7
    movlw '8' 
    subwf datatmp,W
    btfsc STATUS,Z
    goto num8
    movlw '9' 
    subwf datatmp,W
    btfsc STATUS,Z
    goto num9
    movlw 'A' 
    subwf datatmp,W
    btfsc STATUS,Z
    goto letA
    movlw 'B' 
    subwf datatmp,W
    btfsc STATUS,Z
    goto letB
    movlw 'C' 
    subwf datatmp,W
    btfsc STATUS,Z
    goto letC
    movlw 'D' 
    subwf datatmp,W
    btfsc STATUS,Z
    goto letD    
    movlw 'E' 
    subwf datatmp,W
    btfsc STATUS,Z
    goto letE    
    movlw 'F' 
    subwf datatmp,W
    btfsc STATUS,Z
    goto letF    
    movlw 'H' 
    subwf datatmp,W
    btfsc STATUS,Z
    goto letH 
    movlw 'L' 
    subwf datatmp,W
    btfsc STATUS,Z
    goto letL
    movlw 'P' 
    subwf datatmp,W
    btfsc STATUS,Z
    goto letP
    movlw 'U' 
    subwf datatmp,W
    btfsc STATUS,Z
    goto letU
    movlw 'N' 
    subwf datatmp,W
    btfsc STATUS,Z
    goto letN
    goto none  ;It has not been ingresed a correct character
    
;----------------------------------------------------------- Generating a delay
delay10ms:  ;4MHz frecuency oscillator
    movlw d'84'  ;A Value
    movwf delayvar+1
d0:   movlw d'38' ;B Value
    movwf delayvar  
    nop
d1:  decfsz delayvar,F
    goto d1
    decfsz delayvar+1,F
    goto d0      
    return ;2+1+1+A[1+1+1+B+1+2B-2]+A+1+2A-2+2 => 5+A[5+3B]
delay: ;300 ms delay
    movlw .10
    call delayW0ms
    return
    
delayW0ms: ;It is neccesary load a properly value in the acumulator before use this subrutine
    movwf delayvar+2
d2:    call delay10ms
    decfsz delayvar+2,F
    goto d2
    return   
    
num0:
    retlw b'11111100' ; 0
num1:    
    retlw b'01100000' ; 1
num2:    
    retlw b'11011010' ; 2
num3:    
    retlw b'11110010' ; 3
num4:    
    retlw b'01100110' ; 4
num5:    
    retlw b'10110110' ; 5 
num6:    
    retlw b'10111110' ; 6 
num7:    
    retlw b'11100000' ; 7
num8:    
    retlw b'11111110' ; 8 
num9:    
    retlw b'11110110' ; 9
letA:    
    retlw b'11101110' ; A 
letB:    
    retlw b'00111110' ; B
letC:    
    retlw b'10011100' ; C
letD:    
    retlw b'01111010' ; D
letE:    
    retlw b'10011110' ; E
letF:    
    retlw b'10001110' ; F
letH:    
    retlw b'01101110' ; H    
letL:    
    retlw b'00011100' ; L   
letP:    
    retlw b'11001110' ; P      
letU:    
    retlw b'01111100' ; U
letN:    
    retlw b'11101100' ; N       
none:
    retlw b'00000000' ; Error
    
    END
    
    
    
    