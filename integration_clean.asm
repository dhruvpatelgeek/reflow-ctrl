
; Start/Constants
    $NOLIST
    $MOD9351
    $LIST

    CLK           EQU 7373000  ; Microcontroller system crystal frequency in Hz
    TIMER0_RATE   EQU 100     ; 2048Hz squarewave (peak amplitude of CEM-1203 speaker)
    TIMER0_RELOAD EQU ((65536-(CLK/(2*TIMER0_RATE))))
    TIMER1_RATE   EQU 100     ; 100Hz, for a timer tick of 10ms
    TIMER1_RELOAD EQU ((65536-(CLK/(2*TIMER1_RATE))))



    CCU_RATE    EQU 11025     ; 22050Hz is the sampling rate of the wav file we are playing
    CCU_RELOAD  EQU ((65536-((CLK/(2*CCU_RATE)))))
    BAUD        EQU 115200
    BRVAL       EQU ((CLK/BAUD)-16)



    ; Commands supported by the SPI flash memory according to the datasheet
    WRITE_ENABLE     EQU 0x06  ; Address:0 Dummy:0 Num:0
    WRITE_DISABLE    EQU 0x04  ; Address:0 Dummy:0 Num:0
    READ_STATUS      EQU 0x05  ; Address:0 Dummy:0 Num:1 to infinite
    READ_BYTES       EQU 0x03  ; Address:3 Dummy:0 Num:1 to infinite
    READ_SILICON_ID  EQU 0xab  ; Address:0 Dummy:3 Num:1 to infinite
    FAST_READ        EQU 0x0b  ; Address:3 Dummy:1 Num:1 to infinite
    WRITE_STATUS     EQU 0x01  ; Address:0 Dummy:0 Num:1
    WRITE_BYTES      EQU 0x02  ; Address:3 Dummy:0 Num:1 to 256
    ERASE_ALL        EQU 0xc7  ; Address:0 Dummy:0 Num:0
    ERASE_BLOCK      EQU 0xd8  ; Address:3 Dummy:0 Num:0
    READ_DEVICE_ID   EQU 0x9f  ; Address:0 Dummy:2 Num:1 to infinite
;Vectors
    ; Reset vector
    org 0x0000
        ljmp main

    ; External interrupt 0 vector (not used in this code)
    org 0x0003
        reti

    ; Timer/Counter 0 overflow interrupt vector
    org 0x000B
        ljmp Timer0_ISR

    ; External interrupt 1 vector (not used in this code)
    org 0x0013
        reti

    ; Timer/Counter 1 overflow interrupt vector
    org 0x001B
        ljmp Timer1_ISR

    ; Serial port receive/transmit interrupt vector (not used in this code)
    org 0x0023 
        reti

    org 0x005b ; CCU interrupt vector.  Used in this code to replay the wave file.
        ljmp CCU_ISR


    ;                                                        -                     
    ;                                                       -  -                    
    ;                                                      -    -                   
    ;                              leave it at this temp>>-      -                  
    ;                                                    -        -                 
    ;                                                   -          -                 
    ;                                                  -            -               
    ;                                                 -              -              
    ;                                                -                -             
    ;                                               -                  -            
    ;                                              -                    -           
    ;                                             -    reflow>>cool     -          
    ;               -----------------------------    (temperature only)  -         
    ;              -     soak (time+temp)                                 -        
    ;             -                                                        -       
    ;            -                                                          -       
    ;          -                                                             -      
    ;         -                                                               -     
    ;        -                                                                 -    
    ;      -                                                                    -    
    ;     - ramp to soak (temperature)                                           -   
    ;   -                                                                         -   
    ;   state 1 ((temp==soak)? ssr_off: ssr_on)
    ;          state 2 ((time=soak_time)?(pwm_off):(pwn_on)
    ;                                           state 3 ((temp==soak)? ssr_off: ssr_on)
    ;                                                        state 4 (cooling ssr_off)
    ;                                                                             state 5 (done)

    ; In the 8051 we can define direct access variables starting at location 0x30 up to location 0x7F
;Variables(dseg) 
    dseg at 0x30

    bcd:             ds 5
    ;FSM varialbles
    temp_soak:       ds 1 ; temp to soak
    time_soak:       ds 1 ; time to soak
    temp_refl:       ds 1 ; temp of relfow
    time_refl:       ds 1 ; time to reflow 
    state:           ds 1 ; current state 
    state_lcd:       ds 1
    temp:            ds 1 ; current temp in degree C
    sec:             ds 1 ; current time in seconds 
    product:         ds 1; pwm-currsec
    pwm:             ds 1 ; 

    ;Timer variables
    Count1ms:        ds 2 ; Used to determine when half second has passed
    reflow_temp:     ds 2
    soak_temp:       ds 2
    reflow_temp_var: ds 1
    BCD_counter:     ds 1 ; The BCD counter incrememted in the ISR and displayed in the main loop
    hour:            ds 1
    reflow_sec:      ds 1
    reflow_min:      ds 1
    soak_sec:        ds 1
    soak_min:        ds 1
    Count10ms:       ds 1 ; Used to determine when half second has passed
    Count10ms2:      ds 1
    w:               ds 3 ; 24-bit play counter.  Decremented in CCU ISR.
    minutes:         ds 1
    seconds:         ds 1
    T2S_FSM_state:   ds 1
    Count5ms:        ds 1
    five_sec_flag:   ds 1

    ; Temperature 

    x: ds 4
    y: ds 4
    Result: ds 2 
    ch0: ds 2
    ch1: ds 2
    oven_temp: ds 2
    bcd: ds 5

;flags(bseg)
    BSEG
    T2S_FSM_start:     dbit 1
    seconds_flag:      dbit 1
    mf:                dbit 1
    half_seconds_flag: dbit 1 ; Set to one in the ISR every time 500 ms had passed
    AMPM_flag:         dbit 1
    alarm_AMPM_flag:   dbit 1
    on_off_flag:       dbit 1 ; 1 is on
    alarm_buzzer_flag: dbit 1
    TR1_flag:          dbit 1
    tt_reflow_flag:    dbit 1
    tt_flag_soak:      dbit 1
    stop_flag:         dbit 1
    my_flag:           dbit 1

    ;_ _ _ _ | _ _ _ _ _ _
    ;
    ;pwm = 40 (say)
    ;then output will be 100 
    ;_________
    ;         |
    ;         |_____________
    ; where period is 1 second 
    ; In the 8051 we have variables that are 1-bit in size.  We can use the setb, clr, jb, and jnb
    ; instructions with these variables.  This is how you define a 1-bit variable:

;Pin config(cseg)

    cseg
    ; These 'equ' must match the wiring between the microcontroller and the LCD!
    LCD_RS equ P0.5
    LCD_RW equ P0.6
    LCD_E  equ P0.7
    LCD_D4 equ P1.2
    LCD_D5 equ P1.3
    LCD_D6 equ P1.4
    LCD_D7 equ P1.6

    CLEAR         equ P3.0
    FLASH_CE      EQU P2.4
    SOUND         EQU P2.7
    
    SETUP_SOAK_Button equ  P2.1
    set_BUTTON        equ  P2.0
    Button_min        equ  P2.6
    HOME_BUTTON       equ  P2.7

    SQUARE_WAVE       EQU P0.1

    ;define the connections between the ADC and MCU (P89 & MCP3008)
    CE_ADC    EQU  P1.7;P2.4   ;SS
    MY_MOSI   EQU  P3.1 ;P2.2  ;MOSI
    MY_MISO   EQU  P0.3   ;MISO
    MY_SCLK   EQU  P0.2   ;SPICLK
    
;include files
    $NOLIST
    $include(math32.inc)
    $include(LCD_4bit_LPC9351.inc) ; A library of LCD related functions and utility macros
    $LIST
;Strings
  ;                       1234567890123456
   ;General
    Newline:          db   '\r', '\n', 0
    Space:            db   '      ','\r', '\n', 0
    nothing:          db '                ',0      
    test2:            db '      Test2     ',0
    hot:			  db '      HOT       ', 0
    dots:             db ':',0
    timee:            db 'time', 0
    statee:           db '1', 0

    Initial_Message:  db 'BCD_counter: xx ', 0
   ;Home page
    Temp0:            db 'Temp:xxxC       ', 0
    Time:             db 'Time xx:xx SET  ', 0
   ;Second Page
    soak_reflw:       db '  SOAK  REFLOW  ', 0
   ;Reflow Setup
    reflow_setup:     db 'Temp',0
    reflow_setup4:    db '*REFLOW*',0
    reflow_setup2:    db 'Time',0
    reflow_setup3:    db 'HOME',0
   ;Soak Setup
    soak_setup0:      db 'Temp',0
    soak_setup1:      db ' *SOAK*',0
    soak_setup2:      db 'Time',0
    soak_setup3:      db 'HOME',0

;------ISR-------;
    ;---------------------------------;
    ; Routine to initialize the ISR   ;
    ; for timer 0                     ;
    ;---------------------------------;
    Timer0_Init:
        mov a, TMOD
        anl a, #0xf0 ; Clear the bits for timer 0
        orl a, #0x01 ; Configure timer 0 as 16-timer
        mov TMOD, a
        mov TH0, #high(TIMER0_RELOAD)
        mov TL0, #low(TIMER0_RELOAD)
        ; Enable the timer and interrupts
        setb ET0  ; Enable timer 0 interrupt
        setb TR0  ; Start timer 0
        ret

    ;---------------------------------;
    ; ISR for timer 0.  Set to execute;
    ; every 1/4096Hz to generate a    ;
    ; 2048 Hz square wave at pin P3.7 ;
    ;---------------------------------;
    Timer0_ISR:
        mov TH0, #high(TIMER0_RELOAD)
        mov TL0, #low(TIMER0_RELOAD)
        push acc
        push psw
        
        inc Count10ms2
        mov a, Count10ms2
        cjne a, #50, Timer0_ISR_done 
        setb my_flag  ; used to refresh the page even if timer 1 is disabled 

        Timer0_ISR_done:
        pop psw
        pop acc
        reti

    ;---------------------------------;
    ; Routine to initialize the ISR   ;
    ; for timer 1                     ;
    ;---------------------------------;
    Timer1_Init:
        mov a, TMOD
        anl a, #0x0f ; Clear the bits for timer 1
        orl a, #0x10 ; Configure timer 1 as 16-timer
        mov TMOD, a
        mov TH1, #high(TIMER1_RELOAD)
        mov TL1, #low(TIMER1_RELOAD)
        ; Enable the timer and interrupts
        setb ET1  ; Enable timer 1 interrupt
        setb TR1  ; Start timer 1
        ret

    ;---------------------------------;
    ; ISR for timer 1                 ;
    ;---------------------------------;
    Timer1_ISR:
        mov TH1, #high(TIMER1_RELOAD)
        mov TL1, #low(TIMER1_RELOAD)       
        ; The two registers used in the ISR must be saved in the stack
        push acc
        push psw 
        ; Increment the 8-bit 10-mili-second counter
        inc Count10ms
        ; Increment the 16-bit one mili second counter
    Inc_Done:
        mov a, Count10ms
        subb a, pwm ; if pwm greater than a count 10 ms  is  the pin is off else on 
        da a
        jnc off_segment
        setb SQUARE_WAVE
        clr c
        sjmp pass
        off_segment:
        clr SQUARE_WAVE
        clr c
        sjmp pass
        ; Check if 1 second has passed
        pass:
        mov a, Count10ms
        cjne a, #200, Timer1_ISR_done ; Warning: this instruction changes the carry flag!
        ;----------------------------;
        inc sec ; one second has passed
        mov a,sec
        da a
        mov sec,a
        mov a,sec
       ; mov minutes, #0
        mov seconds, acc 
        ;----------------------------
        ; 500 milliseconds have passed.  Set a flag so the main program knows
        setb half_seconds_flag ; Let the main program know half second had passed
        ; Reset to zero the 10-milli-seconds counter, it is a 8-bit variable
        mov Count10ms, #0x00
        ; Increment the BCD counter
        mov a, BCD_counter
        add a, #0x01
    Timer1_ISR_da:
        da a ; Decimal adjust instruction.  Check datasheet for more details!
        mov BCD_counter, a
        
    Timer1_ISR_done:
        pop psw
        pop acc
        reti





    ;------------------------------
    ;---------------------------------;
    ; Routine to initialize the CCU.  ;
    ; We are using the CCU timer in a ;
    ; manner similar to the timer 2   ;
    ; available in other 8051s        ;
    ;---------------------------------;
    CCU_Init:
        mov TH2, #high(CCU_RELOAD)
        mov TL2, #low(CCU_RELOAD)
        mov TOR2H, #high(CCU_RELOAD)
        mov TOR2L, #low(CCU_RELOAD)
        mov TCR21, #10000000b ; Latch the reload value
        mov TICR2, #10000000b ; Enable CCU Timer Overflow Interrupt
        setb ECCU ; Enable CCU interrupt
        setb TMOD20 ; Start CCU timer
        ret

    ;---------------------------------;
    ; ISR for CCU.  Used to playback  ;
    ; the WAV file stored in the SPI  ;
    ; flash memory.                   ;
    ;---------------------------------;
    CCU_ISR:
        mov TIFR2, #0 ; Clear CCU Timer Overflow Interrupt Flag bit. Actually, it clears all the bits!
        
        ; The registers used in the ISR must be saved in the stack
        push acc
        push psw
        
        ; Check if the play counter is zero.  If so, stop playing sound.
        mov a, w+0
        orl a, w+1
        orl a, w+2
        jz stop_playing
        
        ;;yolo this is gonna fuck with the speaker
        ; Increment the 16-bit one mili second counter
      ;  inc Count1ms+0    ; Increment the low 8-bits first
      ;  mov a, Count1ms+0 ; If the low 8-bits overflow, then increment high 8-bits
      ;  jnz Inc_Done
      ;  inc Count1ms+1
        ; Decrement play counter 'w'.  In this implementation 'w' is a 24-bit counter.
        mov a, #0xff
        dec w+0
        cjne a, w+0, keep_playing
        dec w+1
        cjne a, w+1, keep_playing
        dec w+2
        
    keep_playing:

        lcall Send_SPI ; Read the next byte from the SPI Flash...
        mov AD1DAT3, a ; and send it to the DAC
        
        sjmp CCU_ISR_Done

    stop_playing:
        clr TMOD20 ; Stop CCU timer
        setb FLASH_CE  ; Disable SPI Flash
        clr SOUND ; Turn speaker off

    CCU_ISR_Done:	
        pop psw
        pop acc
        reti
;----------------;

;---------------------------------;
; Initial configuration of ports. ;
; After reset the default for the ;
; pins is 'Open Drain'.  This     ;
; routine changes them pins to    ;
; Quasi-bidirectional like in the ;
; original 8051.                  ;
; Notice that P1.2 and P1.3 are   ;
; always 'Open Drain'. If those   ;
; pins are to be used as output   ;
; they need a pull-up resistor.   ;
;---------------------------------;
Ports_Init:
    ; Configure all the ports in bidirectional mode:
    mov P0M1, #00H
    mov P0M2, #00H
    mov P1M1, #00H
    mov P1M2, #00H ; WARNING: P1.2 and P1.3 need 1 kohm pull-up resistors if used as outputs!
    mov P2M1, #00H
    mov P2M2, #00H
    mov P3M1, #00H
    mov P3M2, #00H
	ret

;---------------------------------;
; Initialize ADC1/DAC1 as DAC1.   ;
; Warning, the ADC1/DAC1 can work ;
; only as ADC or DAC, not both.   ;
; The P89LPC9351 has two ADC/DAC  ;
; interfaces.  One can be used as ;
; ADC and the other can be used   ;
; as DAC.  Also configures the    ;
; pin associated with the DAC, in ;
; this case P0.4 as 'Open Drain'. ;
;---------------------------------;
InitDAC1:
    ; Configure pin P0.4 (DAC1 output pin) as open drain
	orl	P0M1,   #00010000B
	orl	P0M2,   #00010000B
    mov ADMODB, #00101000B ; Select main clock/2 for ADC/DAC.  Also enable DAC1 output (Table 25 of reference manual)
	mov	ADCON1, #00000100B ; Enable the converter
	mov AD1DAT3, #0x80     ; Start value is 3.3V/2 (zero reference for AC WAV file)
	ret

;---------------------------------;
; Change the internal RC osc. clk ;
; from 7.373MHz to 14.746MHz.     ;
;---------------------------------;
Double_Clk:
    mov dptr, #CLKCON
    movx a, @dptr
    orl a, #00001000B ; double the clock speed to 14.746MHz
    movx @dptr,a
	ret

;---------------------------------;
; Initialize the SPI interface    ;
; and the pins associated to SPI. ;
;---------------------------------;
Init_SPI:
	; Configure MOSI (P2.2), CS* (P2.4), and SPICLK (P2.5) as push-pull outputs (see table 42, page 51)
	anl P2M1, #low(not(00110100B))
	orl P2M2, #00110100B
	; Configure MISO (P2.3) as input (see table 42, page 51)
	orl P2M1, #00001000B
	anl P2M2, #low(not(00001000B)) 
	; Configure SPI
	mov SPCTL, #11010000B ; Ignore /SS, Enable SPI, DORD=0, Master=1, CPOL=0, CPHA=0, clk/4
	ret

;---------------------------------;
; Sends AND receives a byte via   ;
; SPI.                            ;
;---------------------------------;
Send_SPI:
	mov SPDAT, a
    Send_SPI_1:
        mov a, SPSTAT 
        jnb acc.7, Send_SPI_1 ; Check SPI Transfer Completion Flag
        mov SPSTAT, a ; Clear SPI Transfer Completion Flag
        mov a, SPDAT ; return received byte via accumulator
        ret

;---------------------------------;
; SPI flash 'write enable'        ;
; instruction.                    ;
;---------------------------------;
Enable_Write:
	clr FLASH_CE
	mov a, #WRITE_ENABLE
	lcall Send_SPI
	setb FLASH_CE
	ret

;---------------------------------;
; This function checks the 'write ;
; in progress' bit of the SPI     ;
; flash memory.                   ;
;---------------------------------;
Check_WIP:
	clr FLASH_CE
	mov a, #READ_STATUS
	lcall Send_SPI
	mov a, #0x55
	lcall Send_SPI
	setb FLASH_CE
	jb acc.0, Check_WIP ;  Check the Write in Progress bit
	ret
	
; Display a binary number in the LCD (must be less than 99).  Number to display passed in accumulator.
LCD_number:
	push acc
	mov b, #10
	div ab
	orl a, #'0'
	lcall ?WriteData
	mov a, b
	orl a, #'0'
	lcall ?WriteData
	pop acc
	ret
;SPEAKER
    ; Sounds we need in the SPI flash: 0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20; 30; 40; 50; minutes; seconds;
    ; Approximate index of sounds in file 'stop_watch.wav'
    ; This was generated using: computer_sender -Asw_index.asm -S2000 stop_watch.wav
    sound_index:
        db 0x00, 0x00, 0x2d ; 0 
        db 0x00, 0x31, 0x07 ; 1 
        db 0x00, 0x70, 0x07 ; 2 
        db 0x00, 0xad, 0xb9 ; 3 
        db 0x00, 0xf2, 0x66 ; 4 
        db 0x01, 0x35, 0xd5 ; 5 
        db 0x01, 0x7d, 0x33 ; 6 
        db 0x01, 0xc7, 0x61 ; 7 
        db 0x02, 0x12, 0x79 ; 8 
        db 0x02, 0x49, 0xc1 ; 9 
        db 0x02, 0x8f, 0x7a ; 10 
        db 0x02, 0xd0, 0x63 ; 11 
        db 0x03, 0x1b, 0x87 ; 12 
        db 0x03, 0x63, 0x0e ; 13 
        db 0x03, 0xb9, 0x5f ; 14 
        db 0x04, 0x11, 0x3a ; 15 
        db 0x04, 0x66, 0xc4 ; 16 
        db 0x04, 0xc0, 0x12 ; 17 
        db 0x05, 0x26, 0x98 ; 18 
        db 0x05, 0x74, 0xe9 ; 19 
        db 0x05, 0xd2, 0x8e ; 20 
        db 0x06, 0x1d, 0x83 ; 21 -> 30 
        db 0x06, 0x63, 0x42 ; 22 -> 40 
        db 0x06, 0xaa, 0xb9 ; 23 -> 50 
        db 0x06, 0xf3, 0xd6 ; 24 -> Minutes 
        db 0x07, 0x3f, 0x02 ; 25 -> Seconds 

    ; Size of each sound in 'sound_index'
    ; Generated using: computer_sender -Asw_index.asm -S2000 stop_watch.wav
    Size_Length:
        db 0x00, 0x30, 0xda ; 0 
        db 0x00, 0x3f, 0x00 ; 1 
        db 0x00, 0x3d, 0xb2 ; 2 
        db 0x00, 0x44, 0xad ; 3 
        db 0x00, 0x43, 0x6f ; 4 
        db 0x00, 0x47, 0x5e ; 5 
        db 0x00, 0x4a, 0x2e ; 6 
        db 0x00, 0x4b, 0x18 ; 7 
        db 0x00, 0x37, 0x48 ; 8 
        db 0x00, 0x45, 0xb9 ; 9 
        db 0x00, 0x40, 0xe9 ; 10 
        db 0x00, 0x4b, 0x24 ; 11 
        db 0x00, 0x47, 0x87 ; 12 
        db 0x00, 0x56, 0x51 ; 13 
        db 0x00, 0x57, 0xdb ; 14 
        db 0x00, 0x55, 0x8a ; 15 
        db 0x00, 0x59, 0x4e ; 16 
        db 0x00, 0x66, 0x86 ; 17 
        db 0x00, 0x4e, 0x51 ; 18 
        db 0x00, 0x5d, 0xa5 ; 19 
        db 0x00, 0x4a, 0xf5 ; 20 
        db 0x00, 0x45, 0xbf ; 21 -> 30
        db 0x00, 0x47, 0x77 ; 22 -> 40
        db 0x00, 0x49, 0x1d ; 23 -> 50
        db 0x00, 0x4b, 0x2c ; 24 -> minutes
        db 0x00, 0x5c, 0x87 ; 25 -> seconds

    ; The sound and its length from the two tables above is passed in the accumulator.
    Play_Sound_Using_Index:
        setb SOUND ; Turn speaker on
        clr TMOD20 ; Stop the CCU from playing previous request
        setb FLASH_CE
        
        ; There are three bytes per row in our tables, so multiply index by three
        mov b, #3
        mul ab
        mov R0, a ; Make a copy of the index*3
        
        clr FLASH_CE ; Enable SPI Flash
        mov a, #READ_BYTES
        lcall Send_SPI
        ; Set the initial position in memory of where to start playing
        mov dptr, #sound_index
        mov a, R0
        movc a, @a+dptr
        lcall Send_SPI
        inc dptr
        mov a, R0
        movc a, @a+dptr
        lcall Send_SPI
        inc dptr
        mov a, R0
        movc a, @a+dptr
        lcall Send_SPI
        ; Now set how many bytes to play
        mov dptr, #Size_Length
        mov a, R0
        movc a, @a+dptr
        mov w+2, a
        inc dptr
        mov a, R0
        movc a, @a+dptr
        mov w+1, a
        inc dptr
        mov a, R0
        movc a, @a+dptr
        mov w+0, a
        
        mov a, #0x00 ; Request first byte to send to DAC
        lcall Send_SPI
        
        setb TMOD20 ; Start playback by enabling CCU timer

        ret
    ; Send a character using the serial port
putchar:
        jnb TI, putchar 
        ; TI serial interrupt flag is set and when last bit (stop bit) 
        ; of receiving data byte is received, RI flag get set. IE register
        ; is used to enable/disable interrupt sources.
        clr TI
        mov SBUF, a
        ret

;---------------------------------------------------------------------------------;
; This is the FSM that plays minutes and seconds after the STOP button is pressed ;
; The state diagram of this FSM is available as 'Stop_Watch_FSM.pdf'              ;

    T2S_FSM:
        mov a, T2S_FSM_state

    T2S_FSM_State0: ; Checks for the start signal (T2S_FSM_Start==1)
        cjne a, #0, T2S_FSM_State1
        jnb T2S_FSM_Start, T2S_FSM_State0_Done
        ; Check if minutes is larger than 19
        clr c
        mov a, minutes
        subb a, #20
        jnc minutes_gt_19
        mov T2S_FSM_state, #1
        sjmp T2S_FSM_State0_Done
    minutes_gt_19:
        mov T2S_FSM_state, #3
    T2S_FSM_State0_Done:
        ret
        
    T2S_FSM_State1: ; Plays minutes when minutes is less than 20
        cjne a, #1, T2S_FSM_State2
        mov a, minutes
        lcall Play_Sound_Using_Index
        mov T2S_FSM_State, #2
        ret 

    T2S_FSM_State2: ; Stay in this state until sound finishes playing
        cjne a, #2, T2S_FSM_State3
        jb TMOD20, T2S_FSM_State2_Done 
        mov T2S_FSM_State, #6
    T2S_FSM_State2_Done:
        ret

    T2S_FSM_State3: ; Plays the tens when minutes is larger than 19, for example for 42 minutes, it plays 'forty'
        cjne a, #3, T2S_FSM_State4
        mov a, minutes
        mov b, #10
        div ab
        add a, #18
        lcall Play_Sound_Using_Index
        mov T2S_FSM_State, #4
        ret

    T2S_FSM_State4: ; Stay in this state until sound finishes playing
        cjne a, #4, T2S_FSM_State5
        jb TMOD20, T2S_FSM_State4_Done 
        mov T2S_FSM_State, #5
    T2S_FSM_State4_Done:
        ret

    T2S_FSM_State5: ; Plays the units when minutes is larger than 19, for example for 42 minutes, it plays 'two'
        cjne a, #5, T2S_FSM_State6
        mov a, minutes
        mov b, #10
        div ab
        mov a, b
        jz T2S_FSM_State5_Done ; Prevents from playing something like 'forty zero'
        lcall Play_Sound_Using_Index
    T2S_FSM_State5_Done:
        mov T2S_FSM_State, #2
        ret

    T2S_FSM_State6: ; Plays the word 'minutes'
        cjne a, #6, T2S_FSM_State7
        mov a, #24 ; Index 24 has the word 'minutes'
        lcall Play_Sound_Using_Index
        mov T2S_FSM_State, #7
        ret

    T2S_FSM_State7: ; Stay in this state until sound finishes playing
        cjne a, #7, T2S_FSM_State8
        jb TMOD20, T2S_FSM_State7_Done 
        ; Done playing previous sound, check if seconds is larger than 19
        clr c
        mov a, seconds
        subb a, #20
        jnc seconds_gt_19
        mov T2S_FSM_state, #8
        sjmp T2S_FSM_State0_Done
    seconds_gt_19:
        mov T2S_FSM_state, #10
    T2S_FSM_State7_Done:
        ret

    T2S_FSM_State8: ; Play the seconds when seconds is less than 20.
        cjne a, #8, T2S_FSM_State9
        mov a, seconds
        lcall Play_Sound_Using_Index
        mov T2S_FSM_state, #9
        ret

    T2S_FSM_State9: ; Stay in this state until sound finishes playing
        cjne a, #9, T2S_FSM_State10
        jb TMOD20, T2S_FSM_State9_Done 
        mov T2S_FSM_State, #13
    T2S_FSM_State9_Done:
        ret

    T2S_FSM_State10:  ; Plays the tens when seconds is larger than 19, for example for 35 seconds, it plays 'thirty'
        cjne a, #10, T2S_FSM_State11
        mov a, seconds
        mov b, #10
        div ab
        add a, #18
        lcall Play_Sound_Using_Index
        mov T2S_FSM_state, #11
        ret

    T2S_FSM_State11: ; Stay in this state until sound finishes playing
        cjne a, #11, T2S_FSM_State12
        jb TMOD20, T2S_FSM_State11_Done 
        mov T2S_FSM_State, #12
    T2S_FSM_State11_Done:
        ret

    T2S_FSM_State12: ; Plays the units when seconds is larger than 19, for example for 35 seconds, it plays 'five'
        cjne a, #12, T2S_FSM_State13
        mov a, seconds
        mov b, #10
        div ab
        mov a, b
        jz T2S_FSM_State12_Done ; Prevents from saying something like 'thirty zero'
        lcall Play_Sound_Using_Index
    T2S_FSM_State12_Done:
        mov T2S_FSM_State, #9
        ret

    T2S_FSM_State13: ; Plays the word 'seconds'
        cjne a, #13, T2S_FSM_State14
        mov a, #25 ; Index 25 has the word 'seconds'
        lcall Play_Sound_Using_Index
        mov T2S_FSM_State, #14
        ret

    T2S_FSM_State14: ; Stay in this state until sound finishes playing
        cjne a, #14, T2S_FSM_Error
        jb TMOD20, T2S_FSM_State14_Done 
        clr T2S_FSM_Start 
        mov T2S_FSM_State, #0
    T2S_FSM_State14_Done:
        ret

    T2S_FSM_Error: ; If we got to this point, there is an error in the finite state machine.  Restart it.
        mov T2S_FSM_state, #0
        clr T2S_FSM_Start
        ret
;---------------------------------------------------------------------------------;       
WaitHalfSec:
            mov R2, #178
            Lr3: mov R1, #250
            Lr2: mov R0, #166
            Lr1: djnz R0, Lr1 ; 3 cycles->3*45.21123ns*166=22.51519us
            djnz R1, Lr2 ; 22.51519us*250=5.629ms
            djnz R2, Lr3 ; 5.629ms*89=0.5s (approximately)
            ret
    	

;convert:
    ;    mov x+0, Result
    ;	mov x+1, Result+1 
    ;	mov x+2, #0
    ;	mov x+3, #0
    ;    ret
    ;    
    ;
;config_adc:
    ;        clr CE_ADC 
    ;        mov R0, #00000001B; Start bit:1 
    ;        lcall DO_SPI_G
    ;
    ;        mov R0, #10000000B; Single ended, read channel 0 
    ;        lcall DO_SPI_G 
    ;        mov a, R1          ; R1 contains bits 8 and 9 
    ;        anl a, #00000011B  ; We need only the two least significant bits 
    ;        mov Result+1, a    ; Save result high.
    ;
    ;        mov R0, #55H; It doesn't matter what we transmit... 
    ;        lcall DO_SPI_G 
    ;        mov Result, R1     ; R1 contains bits 0 to 7.  Save result low. 
    ;        setb CE_ADC 
    ;        lcall convert  
    ;        mov a, bcd ; move temp to accumulator 
    ;        ret
Display_temp:
    ;    Load_y(410)
    ;    lcall mul32
    ;    Load_y(1023)
    ;    lcall div32
    ;    Load_y(273)
    ;    lcall sub32
    ;    lcall hex2bcd
    ;    lcall InitSerialPort
        Set_Cursor(1, 1)
        Send_Constant_String(#Temp0)
    ;    lcall SendString
    ;    Set_Cursor(1, 5)    
    ;    Send_BCD(bcd+1) ; send fisrt 2 digits to putty
    ;    Display_BCD(bcd+1); send fisrt 2 digits to lcd
    ;    Set_Cursor(1, 7) 
    ;    Send_BCD(bcd) ; send last 2 digits to putty
    ;    Display_BCD(bcd+0) ; send last 2 digits to lcd
    ;    Set_Cursor(1, 5)
    ;    Send_Constant_String(#dots)
    ;    lcall SendString
    ;    mov DPTR, #Newline
    ;    lcall SendString
  ret

Reset_timer:

    clr TR1                 ; Stop timer 2
    clr a
	mov Count10ms, #0x00
	; Now clear the BCD counter and minutes
	mov BCD_counter, a
	setb TR1                ; Start timer 2

    ret
Display_time:
    Set_Cursor(2, 1)
    Send_Constant_String(#Time)
    clr half_seconds_flag ; We clear this flag in the main loop, but it is set in the ISR for timer 2
    clr my_flag
	Set_Cursor(2, 9)     ; the place in the LCD where we want the BCD counter value
	Display_BCD(BCD_counter) ; This macro is also in 'LCD_4bit.inc'
	Set_Cursor(2, 6)     ; the place in the LCD where we want the BCD counter value
	Display_BCD(minutes) ; This macro is also in 'LCD_4bit.inc'

    ret
;;Timer couter 
    sec_counter: 
        mov a,BCD_counter
        cjne a, #0x60, Continue1 ; check if the couter reached 60s
        mov a, minutes
        add a, #0x01 ; add one to the minutes
        da a ; Decimal adjust instruction.  Check datasheet for more details!
        mov minutes, a
        lcall Reset_timer
	    Continue1:
        ret
    min_counter:
		mov a,minutes
		cjne a, #0x60, Continue2
		clr TR1                 ; Stop timer 2
		clr a                   
		mov Count10ms, #0x00      ; Now clear the BCD counter
		mov minutes, a              ; Reset minutes
        setb TR1                ; Start timer 2

		Continue2:
        ret
home_page:
    ; Press Button 2.7 to clean the time
    jb P2.7, continue20
    Wait_Milli_Seconds(#50) ; debounce
    jb P2.7, continue20
    jnb p2.7, $
   ;clr TR1 
    mov BCD_counter, #0x00
    mov minutes, #0x0   
    lcall Display_time

   continue20:
    ;--------Timer----------;

    jnb half_seconds_flag, Temp_sensor
    lcall sec_counter
    lcall min_counter
    lcall Display_time

    ;-----------------------;

    ;-----TEMP SENSOR-------;
    Temp_sensor:
 ;    lcall config_adc
    lcall Display_temp
 ;    lcall  WaitHalfSec 
 ;    ;-----------------------;




    ret

setup_reflow_page:
    PushButton(set_BUTTON, continue9)
    cpl tt_reflow_flag
    continue9:

    jb tt_reflow_flag, jump1
    ;jnb tt_reflow_flag, jump1
    lcall INC_DEC_Reflow_time
    ljmp display_reflow_page
    jump1:
    lcall INC_DEC_Reflow_temp


    display_reflow_page:
    Set_Cursor(1, 5)
    Display_BCD(reflow_temp+0)
    Set_Cursor(1, 7)
    Display_BCD(reflow_temp+1)
       
    
    Set_Cursor(1, 1)
    Send_Constant_String(#reflow_setup)
    Set_Cursor(1, 9)
    Send_Constant_String(#reflow_setup4)

    Set_Cursor(2, 1)
    Send_Constant_String(#reflow_setup2)
    Set_Cursor(2, 8)
    Send_Constant_String(#dots)
    Set_Cursor(2, 12)
    Send_Constant_String(#reflow_setup3)
    Set_Cursor(2, 9)
    Display_BCD(reflow_sec)
    Set_Cursor(2, 6)
    Display_BCD(reflow_min)

    ret
    INC_DEC_Reflow_time:

        PushButton(SETUP_SOAK_Button,check_decrement) ; setup soak is also used to increment 

        mov a, reflow_sec
        cjne a, #0x59, add_reflow_sec
        mov a, reflow_min
        add a, #0x01
        da a
        mov reflow_min, a
        clr a 
        ljmp Continue5
        add_reflow_sec:
        add a, #0x01
        da a ; Decimal adjust instruction.  Check datasheet for more details!
        Continue5:
        mov reflow_sec, a

        check_decrement:
        PushButton(Button_min, continue8)
        mov a, reflow_sec
        cjne a, #0x00, sub_reflow_sec
        clr a 
        ljmp Continue6
        sub_reflow_sec:
        add a, #0x99 ; add 99 reduces 1
        da a ; Decimal adjust instruction.  Check datasheet for more details!
        Continue6:
        mov reflow_sec, a
        continue8:
        ret
    INC_DEC_Reflow_temp:

            PushButton(SETUP_SOAK_Button, check_decrement2) ; SET UP IS ALSO USED TO INCREMENT 
            mov a, reflow_temp+1
            add a, #0x01
            da a ; Decimal adjust instruction.  Check datasheet for more details!
            mov reflow_temp+1, a
            mov a, reflow_temp+1
            jnz INC_reflow_temp_done2
            mov a, reflow_temp+0
            add a, #0x01
            da a ; Decimal adjust instruction.  Check datasheet for more details!
            mov reflow_temp+0, a
            INC_reflow_temp_done2:
            
        hold_done:
        


        check_decrement2:

            PushButton( Button_min, DEC_reflow_temp_done2)
         ;   jb Button_min, DEC_reflow_temp_done2  
         ;       Wait_Milli_Seconds(#50)	
         ;   jb Button_min, DEC_reflow_temp_done2  
         ;   loop_hold_dec:

         ;   jnb Button_min, jump3
         ;   ljmp DEC_reflow_temp_done2
         ;   jump3:
         ;   Set_Cursor(1, 5)
         ;   Display_BCD(reflow_temp+0)
         ;   Set_Cursor(1, 7)
         ;   Display_BCD(reflow_temp+1)
         ;   Wait_Milli_Seconds(#100)	
            mov a, reflow_temp+1
            add a, #0x99
            da a ; Decimal adjust instruction.  Check datasheet for more details!
            mov reflow_temp+1, a
            mov a, reflow_temp+1
            jnz INC_reflow_temp_done
            mov a, reflow_temp+0
            add a, #0x99
            da a ; Decimal adjust instruction.  Check datasheet for more details!
            mov reflow_temp+0, a
          ;  mov a, reflow_temp+1
            INC_reflow_temp_done:
            
          ;  ljmp loop_hold_dec

        DEC_reflow_temp_done2:
    

    ret
setup_soak_page:
    PushButton(set_BUTTON, continue11)
    cpl tt_flag_soak
    continue11:

    jb tt_flag_soak, jump4
    lcall INC_DEC_soak_time
    ljmp display_soak_page
    jump4:
    lcall INC_DEC_soak_temp


    display_soak_page:
    Set_Cursor(1, 5)
    Display_BCD(soak_temp+0)
    Set_Cursor(1, 7)
    Display_BCD(soak_temp+1)
       
    
    Set_Cursor(1, 1)
    Send_Constant_String(#soak_setup0)
    Set_Cursor(1, 9)
    Send_Constant_String(#soak_setup1)

    Set_Cursor(2, 1)
    Send_Constant_String(#soak_setup2)
    Set_Cursor(2, 8)
    Send_Constant_String(#dots)
    Set_Cursor(2, 12)
    Send_Constant_String(#soak_setup3)
    Set_Cursor(2, 9)
    Display_BCD(soak_sec)
    Set_Cursor(2, 6)
    Display_BCD(soak_min)
    ret
    INC_DEC_soak_time:
    
        PushButton(SETUP_SOAK_Button,check_decrement_soak) ; setup soak is also used to increment 

        mov a, soak_sec
        cjne a, #0x59, add_soak_sec
        mov a, soak_min
        add a, #0x01
        da a
        mov soak_min, a
        clr a 
        ljmp Continue12
        add_soak_sec:
        add a, #0x01
        da a ; Decimal adjust instruction.  Check datasheet for more details!
        Continue12:
        mov soak_sec, a

        check_decrement_soak:
        PushButton(Button_min, continue13)
        mov a, soak_sec
        cjne a, #0x00, sub_soak_sec
        clr a 
        ljmp Continue14
        sub_soak_sec:
        add a, #0x99 ; add 99 reduces 1
        da a ; Decimal adjust instruction.  Check datasheet for more details!
        Continue14:
        mov soak_sec, a
        continue13:
        
        ret
    INC_DEC_soak_temp:
        
        PushButton(SETUP_SOAK_Button, check_decrement2_soak)
        ;    jb SETUP_SOAK_Button, check_decrement2_soak  
        ;        Wait_Milli_Seconds(#50)	
        ;    jb SETUP_SOAK_Button, check_decrement2_soak  
        ;    loop_hold_inc_soak:
      ;
        ;    jnb SETUP_SOAK_Button, jump6
        ;    Wait_Milli_Seconds(#100)
        ;    jnb SETUP_SOAK_Button, jump6
        ;    ljmp hold_done_soak
        ;    jump6:
        ;    Set_Cursor(1, 5)
        ;    Display_BCD(soak_temp+0)
        ;    Set_Cursor(1, 7)
        ;    Display_BCD(soak_temp+1)
        ;    Wait_Milli_Seconds(#200)	
            mov a, soak_temp+1
            add a, #0x01
            da a ; Decimal adjust instruction.  Check datasheet for more details!
            mov soak_temp+1, a
            mov a, soak_temp+1
            jnz INC_soak_temp_done2
            mov a, soak_temp+0
            add a, #0x01
            da a ; Decimal adjust instruction.  Check datasheet for more details!
            mov soak_temp+0, a
           ; mov a, soak_temp+1
            INC_soak_temp_done2:
            
          ;  ljmp loop_hold_inc_soak
        hold_done_soak:
        


        check_decrement2_soak:

            PushButton( Button_min, DEC_soak_temp_done2)
           ; 
           ; jb Button_min, DEC_soak_temp_done2  
           ;     Wait_Milli_Seconds(#50)	
           ; jb Button_min, DEC_soak_temp_done2  
           ; loop_hold_dec_soak:
             ;
           ; jnb Button_min, jump7
           ; Wait_Milli_Seconds(#100)
           ; jnb Button_min, jump7
           ; ljmp DEC_soak_temp_done2
           ; jump7:
           ; Set_Cursor(1, 5)
           ; Display_BCD(soak_temp+0)
           ; Set_Cursor(1, 7)
           ; Display_BCD(soak_temp+1)
           ; Wait_Milli_Seconds(#100)	
            mov a, soak_temp+1
            add a, #0x99
            da a ; Decimal adjust instruction.  Check datasheet for more details!
            mov soak_temp+1, a
            mov a, soak_temp+1
            jnz INC_soak_temp_done
            mov a, soak_temp+0
            add a, #0x99
            da a ; Decimal adjust instruction.  Check datasheet for more details!
            mov soak_temp+0, a
          ;  mov a, soak_temp+1
            INC_soak_temp_done:
            
           ; ljmp loop_hold_dec_soak

        DEC_soak_temp_done2:
        ret
second_page:
    Set_Cursor(1, 1)
    Send_Constant_String(#soak_reflw)
    Set_Cursor(2, 1)
    Send_Constant_String(#nothing)
    ret

FSM_LCD:
        mov a, state_lcd


        ;----------------STATE 0------------------;
         home_state:
            cjne a, #0, soak_reflow_state
            PushButton(set_BUTTON,done_home2)  
            ; If pushbutton set_Button is pressed it changes to soak_reflow Page otherwise it stays at the home page
            mov state_lcd, #1
            ljmp done_home
            done_home2:
            lcall home_page
            done_home:
            ljmp Forever_done           
        ;------------------------------------------;
        
     ;   ;----------------STATE 1-------------------;
        soak_reflow_state:
            cjne a, #1, setup_soak
            lcall second_page
          ;  Wait_Milli_Seconds(#50)
            lcall sec_counter ; prevent the timer to go over 60
            lcall min_counter
            PushButton(HOME_BUTTON,next_pushb) ; check if home button is pressed 
            mov state_lcd, #0
            next_pushb:
            PushButton(SETUP_SOAK_Button,next_pushb2) ; check if the the button to setup soak is pressed
            mov state_lcd, #2
            next_pushb2:
            PushButton(Button_min,done_soak) ; check if the buttion to setup the reflow was pressed 
            mov state_lcd, #3
            done_soak:
           ljmp Forever_done 
        ;------------------------------------------;

     ;   ;-----------------STATE 2------------------;
        setup_soak: ; its actually set up reflow Im dumb
            cjne a, #2, setup_reflow
            lcall setup_reflow_page
          ;  Wait_Milli_Seconds(#50)
            lcall sec_counter ; prevent the timer to go over 60
            lcall min_counter
            PushButton(HOME_BUTTON,done_setup_soak) ; check if home button is pressed 
            mov state_lcd, #0
            done_setup_soak:
            ljmp Forever_done 
        ;------------------------------------------;

     ;   ;----------------STATE 3-------------------;
        setup_reflow: ; its actually set up soak Im dumb
            cjne a, #3, FDP
            ljmp FDP2
            FDP:
            ljmp home_state
            FDP2:
            lcall setup_soak_page
            lcall sec_counter ; prevent the timer to go over 60
            lcall min_counter
            PushButton(HOME_BUTTON,done_setup_reflow) ; check if home button is pressed 
            mov state_lcd, #0
            done_setup_reflow:
            ljmp Forever_done 
     ;   ;------------------------------------------;
        Forever_done:
 ret

;---------------------------------;
; Main program. Includes hardware ;
; initialization and 'forever'    ;
; loop.                           ;
;---------------------------------;
main:
	; Initialization
    mov SP, #0x7F
    lcall Timer0_Init
    lcall Timer1_Init

    lcall Ports_Init ; Default all pins as bidirectional I/O. See Table 42.
    lcall LCD_4BIT
    lcall Double_Clk
	lcall InitDAC1 ; Call after 'Ports_Init'
	lcall CCU_Init
	lcall Init_SPI
	
	
	setb EA ; Enable global interrupts.

	; Initialize variables
	clr T2S_FSM_Start
	mov T2S_FSM_state, #0
    ; Configure all the ports in bidirectional mode:

    mov P0M1, #00H
    mov P0M2, #00H
    mov P1M1, #00H
    mov P1M2, #00H ; WARNING: P1.2 and P1.3 need 1kohm pull-up resistors!
    mov P2M1, #00H
    mov P2M2, #00H
    mov P3M1, #00H
    mov P3M2, #00H
    
    ;mov minutes, #0
	mov seconds, #0

   ; lcall LCD_4BIT
    ; For convenience a few handy macros are included in 'LCD_4bit_LPC9351.inc':
 ;	Set_Cursor(1, 1)
  ;  Display_BCD(BCD_counter)

    setb half_seconds_flag
    setb my_flag   

	mov BCD_counter, #0x00
	mov pwm , #0
	mov sec , #0
	mov state, #0
	mov temp, #150
    mov time_soak, #5
    mov temp_refl, #220
    mov temp_soak, #5
    mov five_sec_flag,#0
	; After initialization the program stays in this 'forever' loop

    mov reflow_sec, #0x00
    mov reflow_min, #0x00
    mov minutes, #0x00
    mov state_lcd, #0
    clr TR1_flag
    mov reflow_temp+0, #0x01
    mov reflow_temp+1, #0x50
    clr tt_reflow_flag
    mov soak_sec, #0x00
    mov soak_min, #0x00

    mov soak_temp+0, #0x01
    mov soak_temp+1, #0x50
    clr stop_flag

    
forever:	
    lcall FSM_LCD

    lcall T2S_FSM ; Speaker fsm

  Check_if_stop_button_is_on:
    jb P2.6, continue19 
	Wait_Milli_Seconds(#50) ; debounce
	jb P2.6, continue19
	jnb P2.6, $
	clr TR1 
	ljmp forever
   continue19:

 FSM_PWM:  
    mov a, state
  state0: 
      cjne a, #0, state1
      mov pwm, #0
      clr TR1
      jb p3.0, state0_done
      jnb p3.0, $ ;wait for key release
      setb TR1
      mov state, #1
  state0_done:
      ljmp forever
   
   state1:
      cjne a, #1 , state2
      mov pwm, #100
      mov sec, #0
      mov a, temp_soak
      clr c
      subb a, temp
      ;add branches to compare temp with 150
      jnc state1_done
      mov state, #2
  state1_done:
       ljmp forever
       
  state2: ;press p3.0 multiple time plz cos it is stuck
      cjne a, #2 , state3
      mov pwm, #20
      mov a, time_soak
      clr c
      subb a, sec
      ;add branches to compare sec with  60
      jnc state2_done
      mov state, #3
  state2_done:
       ljmp forever          
  
  state3:
      cjne a, #3 , state4
      mov pwm, #80
      mov sec, #0     
      mov a, temp_refl
      clr c
      subb a, temp
      ;add branches to compare temp with 220
      jnc state3_done
      mov state, #4
  state3_done:
       ljmp forever
       
   state4:
      cjne a, #4 , state5
      mov pwm, #20
      mov a, time_refl
      clr c
      subb a, sec
      ;add branches to compare sec with 45
      jnc state4_done
      mov state, #5
  state4_done:
       ljmp forever    
       
   state5:
      cjne a, #5 , state0
      mov pwm, #0
      clr c
      subb a, temp
      ;add branches to compare temp with 60
      jnc state5_done
      mov state, #0
  state5_done:
       ljmp forever 
       

END