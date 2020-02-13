;Start/Constants
    $NOLIST
    $MOD9351
    $LIST
    ; Reset vector
    org 0x0000
        ljmp MainProgram

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


    ;    ;CLK  EQU 22118400
    ;    CLK  EQU 22118400
    ;    ;termometer
    ;    BAUD equ 115200
    ;    BRG_VAL equ (0x100-(CLK/(16*BAUD)))
    ;    ;timer
    ;    TIMER0_RATE   EQU 4096     ; 2048Hz squarewave (peak amplitude of CEM-1203 speaker)
    ;    TIMER0_RELOAD EQU ((65536-(CLK/TIMER0_RATE)))
    ;    TIMER2_RATE   EQU 1000     ; 1000Hz, for a timer tick of 1ms
    ;    TIMER2_RELOAD EQU ((65536-(CLK/TIMER2_RATE)))
    ;
    ;CLK           EQU 7373000  ; Microcontroller system crystal frequency in Hz
    ;TIMER0_RATE   EQU 4096     ; 2048Hz squarewave (peak amplitude of CEM-1203 speaker)
    ;TIMER0_RELOAD EQU ((65536-(CLK/(2*TIMER0_RATE))))
    ;TIMER1_RATE   EQU 100     ; 100Hz, for a timer tick of 10ms
    ;TIMER1_RELOAD EQU ((65536-(CLK/(2*TIMER1_RATE))))

    CLK           EQU 7373000  ; Microcontroller system crystal frequency in Hz
    TIMER0_RATE   EQU 4096     ; 2048Hz squarewave (peak amplitude of CEM-1203 speaker)
    TIMER0_RELOAD EQU ((65536-(CLK/(2*TIMER0_RATE))))
    TIMER1_RATE   EQU 100     ; 100Hz, for a timer tick of 10ms
    TIMER1_RELOAD EQU ((65536-(CLK/(2*TIMER1_RATE))))

    CLEAR         equ P1.7
    SOUND_OUT     equ P2.7
    UPDOWN        equ P2.4

    CCU_RATE    EQU 11025     ; 22050Hz is the sampling rate of the wav file we are playing
    CCU_RELOAD  EQU ((65536-((CLK/(2*CCU_RATE)))))
    BAUD        EQU 115200
    BRVAL       EQU ((CLK/BAUD)-16)

    FLASH_CE    EQU P2.4
    SOUND       EQU P2.7

    ; There is a couple of typos in MODLP51 in the definition of the timer 0/1 reload
    ; special function registers (SFRs), so:

    TIMER0_RELOAD_L DATA 0xf2
    TIMER1_RELOAD_L DATA 0xf3
    TIMER0_RELOAD_H DATA 0xf4
    TIMER1_RELOAD_H DATA 0xf5

;Variables (dseg)
    DSEG at 30H

    Result:    ds 4
    x:         ds 4
    y:         ds 4
    bcd:       ds 5
    ;FSM varialbles
    temp_soak: ds 1
    time_soak: ds 1
    temp_refl: ds 1
    time_refl: ds 1
    state:     ds 1
    state_lcd: ds 1
    temp:      ds 1
    sec:       ds 1
    pwm:       ds 1 ; Register that controls the power of the oven 
    ;;owen_temp ds 1

    ;Timer variables
    Count1ms:     ds 2 ; Used to determine when half second has passed
    reflow_temp:  ds 2
    reflow_temp_var: ds 1
    BCD_counter:  ds 1 ; The BCD counter incrememted in the ISR and displayed in the main loop
    min:          ds 1
    hour:         ds 1
    reflow_sec:   ds 1
    reflow_min:   ds 1
    alarm_min:    ds 1
    alarm_hour:   ds 1
    day:          ds 1
    month:        ds 1
    year:         ds 1
    hour_24:      ds 1

    ;; FSM
    Count10ms:    ds 1 ; Used to determine when half second has passed
   ; state: ds 1 ; current state 
    product: ds 1; pwm-currsec

    ;;; timer
    w:             ds 3 ; 24-bit play counter.  Decremented in CCU ISR.
    minutes:       ds 1
    seconds:       ds 1
    T2S_FSM_state: ds 1
    Count5ms:      ds 1
    five_sec_flag:  ds 1


;flags (bseg)
    bseg
    mf:                dbit 1
    half_seconds_flag: dbit 1 ; Set to one in the ISR every time 500 ms had passed
    AMPM_flag:         dbit 1
    alarm_AMPM_flag:   dbit 1
    on_off_flag:       dbit 1 ; 1 is on
    alarm_buzzer_flag: dbit 1
    TR2_flag:          dbit 1
    tt_reflow_flag:    dbit 1
    T2S_FSM_start: dbit 1
    seconds_flag:  dbit 1

;Pins Config (cseg)
    CSEG 

    done_button equ P0.0    
    done 		  equ P0.1
    setup 		  equ P0.2
    setmin		  equ P0.3
    sethour 	  equ P0.4
   ; setday        equ P0.5

  ;  start         equ P0.7

     LCD_RS equ P0.5
     LCD_RW equ P0.6
     LCD_E  equ P0.7
     LCD_D4 equ P1.2
     LCD_D5 equ P1.3
     LCD_D6 equ P1.4
     LCD_D7 equ P1.6

  ;  LCD_RS        equ P1.1
  ;  LCD_RW        equ P1.2
  ;  LCD_E         equ P1.3
    start2         equ p1.7   ;in slide it was KEY.3 which should be decided later so p1.7 is just a random pin


    ; These �EQU� must match the wiring between the microcontroller and ADC 
    CE_ADC       EQU  P2.0 
    MY_MOSI      EQU  P2.1 
    MY_MISO      EQU  P2.2 
    MY_SCLK      EQU  P2.3
    SETUP_SOAK_Button equ  P2.4
    set_BUTTON           equ  P2.5
    Button_min    equ  P2.6
    HOME_BUTTON   equ  P2.7

    ;LCD 4bits data
  ;  LCD_D4        equ  P3.2
  ;  LCD_D5        equ  P3.3
  ;  LCD_D6        equ  P3.4
  ;  LCD_D7        equ  P3.5

    BOOT_BUTTON   equ  P4.5
  ;  SOUND_OUT     equ  P3.7

;include files 
    $NOLIST
    $include(math32.inc)
    $include(LCD_4bit_LPC9351.inc) ; A library of LCD related functions and utility macros
    $LIST


;ISR

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
        cpl SOUND_OUT ; Connect speaker to this pin
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
        cpl P2.6 ; To check the interrupt rate with oscilloscope. It must be precisely a 10 ms pulse.
        
        ; The two registers used in the ISR must be saved in the stack
        push acc
        push psw
        
        ; Increment the 8-bit 10-mili-second counter
        inc Count10ms

    Inc_Done:
        mov a, Count10ms
        subb a, pwm ; if pwm greater than a pwm is on else off
        da a
        mov a, product
        jnc off_segment
        setb p0.1
        clr c
        sjmp pass
        off_segment:
        clr p0.1
        clr c
        sjmp pass

        ; Check if 1 second has passed
        pass:

        ; Check if half second has passed
        mov a, Count10ms
        cjne a, #200, Timer1_ISR_done ; Warning: this instruction changes the carry flag!
        ;----------------------------
        inc sec ; one second has passed
        mov a,sec
        da a
        mov sec,a

        inc five_sec_flag ; one second has passed
        mov a,five_sec_flag
        da a
        mov five_sec_flag,a

        mov a,sec
        mov minutes, #0
        mov seconds, acc

        
        ;----------------------------
        ; 500 milliseconds have passed.  Set a flag so the main program knows
        setb half_seconds_flag ; Let the main program know half second had passed
        cpl TR0 ; Enable/disable timer/counter 0. This line creates a beep-silence-beep-silence sound.
        ; Reset to zero the 10-milli-seconds counter, it is a 8-bit variable
        mov Count10ms, #0x0
        ; Increment the BCD counter
        mov a, BCD_counter
        jnb UPDOWN, Timer1_ISR_decrement
        add a, #0x01
        sjmp Timer1_ISR_da
    Timer1_ISR_decrement:
        add a, #0x99 ; Adding the 10-complement of -1 is like subtracting 1.
    Timer1_ISR_da:
        da a ; Decimal adjust instruction.  Check datasheet for more details!
        mov BCD_counter, a
        
    Timer1_ISR_done:
        pop psw
        pop acc
        reti



    timee:  db 'time', 0
    statee:  db 'state', 0


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


;CONFIG:
    ; Configure the serial port and baud rate
   ; InitSerialPort:
   ;     ; Since the reset button bounces, we need to wait a bit before
   ;     ; sending messages, otherwise we risk displaying gibberish!
   ;     mov R1, #222
   ;     mov R0, #166
   ;     djnz R0, $   ; 3 cycles->3*45.21123ns*166=22.51519us
   ;     djnz R1, $-4 ; 22.51519us*222=4.998ms
   ;     ; Now we can proceed with the configuration
   ;     orl	PCON,#0x80
   ;     mov	SCON,#0x52
   ;     mov	BDRCON,#0x00
   ;     mov	BRL,#BRG_VAL
   ;     mov	BDRCON,#0x1E ; BDRCON=BRR|TBCK|RBCK|SPD;
   ;     ret
     ;
     ; Send a character using the serial port
     putchar:
         jnb TI, putchar 
         ; TI serial interrupt flag is set and when last bit (stop bit) 
         ; of receiving data byte is received, RI flag get set. IE register
         ; is used to enable/disable interrupt sources.
         clr TI
         mov SBUF, a
         ret
 
     getchar: 
         jnb RI, getchar 
         clr RI 
         mov a, SBUF 
         ret
 
     ; Send a constant-zero-terminated string using the serial port
     SendString:
         clr A
         movc A, @A+DPTR
         jz SendStringDone
         lcall putchar
         inc DPTR
         sjmp SendString
     SendStringDone:
         ret
    ;
    ; INIT_SPI_Lab2:     
    ;     setb MY_MISO    ; Make MISO an input pin  1 master input 0 slave out   ;MISO master in/slave out
    ;     clr MY_SCLK     ; For mode (0,0) SCLK is zero     
    ;     ret 
    ;
    ; DO_SPI_G:     
    ;     push acc     
    ;     mov R1, #0      ; Received byte stored in R1     
    ;     mov R2, #8      ; Loop counter (8-bits)
    ;     
    ; DO_SPI_G_LOOP:     
    ;     mov a, R0       ; Byte to write is in R0     
    ;     rlc a           ; Carry flag has bit to write 
    ;     mov R0, a     
    ;     mov MY_MOSI, c     
    ;     setb MY_SCLK    ; Transmit     
    ;     mov c, MY_MISO  ; Read received bit     
    ;     mov a, R1       ; Save received bit in R1     
    ;     rlc a     
    ;     mov R1, a     
    ;     clr MY_SCLK     
    ;     djnz R2, DO_SPI_G_LOOP     
    ;     pop acc     
    ;     ret 
    ; 
WaitHalfSec:
        mov R2, #178
        Lr3: mov R1, #250
        Lr2: mov R0, #166
        Lr1: djnz R0, Lr1 ; 3 cycles->3*45.21123ns*166=22.51519us
        djnz R1, Lr2 ; 22.51519us*250=5.629ms
        djnz R2, Lr3 ; 5.629ms*89=0.5s (approximately)
        ret
	
blink:
        mov SP, #7FH
        mov P3M1, #0   ; Configure P3 in bidirectional mode
    M0:
        cpl P3.7
        Set_Cursor(1, 1)
        Send_Constant_String(#nothing)
        Set_Cursor(2, 1)
        Send_Constant_String(#nothing)
        Set_Cursor(1, 1)
        Send_Constant_String(#hot)
        Set_Cursor(2, 1)
        Send_Constant_String(#hot)

        lcall WaitHalfSec

        ret

    
Strings:
    ;Hello_World:
        ;DB  'Hello, World!', '\r', '\n', 0
    Newline:
            DB   '\r', '\n', 0
    Space:
            DB   '      ','\r', '\n', 0

                    ;     1234567890123456
    Temp0:            db 'Temp:xxxC       ', 0
    nothing:          db '                ',0
    test2:            db '      Test2     ',0
    hot:			  db '      HOT       ', 0
    Time:             db 'Time xx:xx SET  ', 0
    dots:             db ':',0
    soak_reflw:       db '  SOAK  REFLOW  ', 0
    reflow_setup:     db 'Temp',0
    reflow_setup4:    db '*REFLOW*',0
    reflow_setup2:    db 'Time',0
    reflow_setup3:    db 'HOME',0


convert:
    mov x+0, Result
	mov x+1, Result+1 
	mov x+2, #0
	mov x+3, #0
    ret
Display_temp:
    Load_y(410)
    lcall mul32
    Load_y(1023)
    lcall div32
    Load_y(273)
    lcall sub32
    lcall hex2bcd
    lcall InitSerialPort
    Set_Cursor(1, 1)
    Send_Constant_String(#Temp0)
    lcall SendString
    Set_Cursor(1, 5)    
    Send_BCD(bcd+1) ; send fisrt 2 digits to putty
    Display_BCD(bcd+1); send fisrt 2 digits to lcd
    Set_Cursor(1, 7) 
    Send_BCD(bcd) ; send last 2 digits to putty
    Display_BCD(bcd+0) ; send last 2 digits to lcd
    Set_Cursor(1, 5)
    Send_Constant_String(#dots)
    lcall SendString
    mov DPTR, #Newline
    lcall SendString
    ret
config_adc:
        clr CE_ADC 
        mov R0, #00000001B; Start bit:1 
        lcall DO_SPI_G

        mov R0, #10000000B; Single ended, read channel 0 
        lcall DO_SPI_G 
        mov a, R1          ; R1 contains bits 8 and 9 
        anl a, #00000011B  ; We need only the two least significant bits 
        mov Result+1, a    ; Save result high.

        mov R0, #55H; It doesn't matter what we transmit... 
        lcall DO_SPI_G 
        mov Result, R1     ; R1 contains bits 0 to 7.  Save result low. 
        setb CE_ADC 
        lcall convert  
        mov a, bcd ; move temp to accumulator 
        ret
Reset_timer:

    clr TR1                 ; Stop timer 2
    clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Now clear the BCD counter and min
	mov BCD_counter, a
	setb TR1                ; Start timer 2

    ret
Display_time:
    Set_Cursor(2, 1)
    Send_Constant_String(#Time)
    clr half_seconds_flag ; We clear this flag in the main loop, but it is set in the ISR for timer 2
	Set_Cursor(2, 9)     ; the place in the LCD where we want the BCD counter value
	Display_BCD(BCD_counter) ; This macro is also in 'LCD_4bit.inc'
	Set_Cursor(2, 6)     ; the place in the LCD where we want the BCD counter value
	Display_BCD(min) ; This macro is also in 'LCD_4bit.inc'

    ret
;Timer couter 
    sec_counter: 
        mov a,BCD_counter
        cjne a, #0x60, Continue1 ; check if the couter reached 60s
        mov a, min
        add a, #0x01 ; add one to the minutes
        da a ; Decimal adjust instruction.  Check datasheet for more details!
        mov min, a
        lcall Reset_timer
	    Continue1:
        ret
    min_counter:
		mov a,min
		cjne a, #0x60, Continue2
		clr TR1                 ; Stop timer 2
		clr a                   
		mov Count1ms+0, a
		mov Count1ms+1, a       ; Now clear the BCD counter
		mov min, a              ; Reset min
        setb TR1                ; Start timer 2

		Continue2:
        ret
home_page:
    ;--------Timer----------;
    jnb half_seconds_flag, Temp_sensor
    lcall sec_counter
    lcall min_counter
    lcall Display_time
    ;-----------------------;
            
    ;-----TEMP SENSOR-------;
    Temp_sensor:
    lcall config_adc
    lcall Display_temp
    lcall  WaitHalfSec 
    ;-----------------------;
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
    ;PushButton(SETUP_SOAK_Button,check_decrement2) ; setup soak is also used to increment 

        jb SETUP_SOAK_Button, check_decrement2  
            Wait_Milli_Seconds(#50)	
        jb SETUP_SOAK_Button, check_decrement2  
        loop_hold_inc:

		jnb SETUP_SOAK_Button, jump2
        Wait_Milli_Seconds(#200)
        jnb SETUP_SOAK_Button, jump2
        ljmp hold_done
        jump2:
        Set_Cursor(1, 5)
        Display_BCD(reflow_temp+0)
        Set_Cursor(1, 7)
        Display_BCD(reflow_temp+1)
        Wait_Milli_Seconds(#100)	
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
        mov a, reflow_temp+1
        INC_reflow_temp_done2:
        
        ljmp loop_hold_inc
    hold_done:
    


    check_decrement2:
        jb Button_min, DEC_reflow_temp_done2  
            Wait_Milli_Seconds(#50)	
        jb Button_min, DEC_reflow_temp_done2  
        loop_hold_dec:

		jnb Button_min, jump3
        Wait_Milli_Seconds(#100)
        jnb Button_min, jump3
        ljmp DEC_reflow_temp_done2
        jump3:
        Set_Cursor(1, 5)
        Display_BCD(reflow_temp+0)
        Set_Cursor(1, 7)
        Display_BCD(reflow_temp+1)
        Wait_Milli_Seconds(#100)	
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
        mov a, reflow_temp+1
        INC_reflow_temp_done:
        
        ljmp loop_hold_dec

    DEC_reflow_temp_done2:
   

    ret
second_page:
    Set_Cursor(1, 1)
    Send_Constant_String(#soak_reflw)
    Set_Cursor(2, 1)
    Send_Constant_String(#nothing)
    ret
;FSM_PWM
    
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
;---------------------------------;
; Main program. Includes hardware ;
; initialization and 'forever'    ;
; loop.                           ;
;---------------------------------;
MainProgram:
    mov SP, #7FH ; Set the stack pointer to the begining of idata

        ; Configure all the ports in bidirectional mode:
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
    
    mov minutes, #0
	mov seconds, #0

    lcall LCD_4BIT
    ; For convenience a few handy macros are included in 'LCD_4bit_LPC9351.inc':

	mov pwm , #0
	mov sec , #0
	mov state, #0
	mov temp, #150
    mov time_soak, #5
    mov temp_refl, #220
    mov five_sec_flag,#0
	; After initialization the program stays in this 'forever' loop
    


 ;-------------------------------------------;
        ; Initialization_LCD
        lcall LCD_4BIT
        ; Initialization_Termometer
        lcall INIT_SPI_Lab2
        ; Initialization_timer
    
        ;lcall Timer0_Init
        lcall Timer1_Init
        ;lcall Timer2_Init
        setb EA   ; Enable Global interrupts
        setb half_seconds_flag
	    mov BCD_counter, #0x00
        mov reflow_sec, #0x00
        mov reflow_min, #0x00
        mov min, #0x00
        mov state_lcd, #0
        clr TR2_flag
        mov reflow_temp+0, #0x01
        mov reflow_temp+1, #0x50
        clr tt_reflow_flag

        
        


    Forever: 
     
        mov a, state_lcd
; start button
        ;----------------STATE 0------------------;
         home_state:
            cjne a, #0, soak_reflow_state
            PushButton(set_BUTTON,done_home2) 
            ;setb set_flag  
            mov state_lcd, #1
            ljmp done_home
            done_home2:
            ;clr set_flag
            lcall home_page
            done_home:
            ljmp Forever           
        ;------------------------------------------;
        
        ;----------------STATE 1-------------------;
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
           ljmp Forever
        ;------------------------------------------;

        ;-----------------STATE 2------------------;
        setup_soak:
            cjne a, #2, setup_reflow
            lcall setup_reflow_page
          ;  Wait_Milli_Seconds(#50)
            lcall sec_counter ; prevent the timer to go over 60
            lcall min_counter
            PushButton(HOME_BUTTON,done_setup_soak) ; check if home button is pressed 
            mov state_lcd, #0
            done_setup_soak:
            ljmp Forever
        ;------------------------------------------;

        ;----------------STATE 3-------------------;
        setup_reflow:
            cjne a, #3, FDP
            ljmp FDP2
            FDP:
            ljmp home_state
            FDP2:
            Set_Cursor(1, 1)
            Send_Constant_String(#test2)
            Set_Cursor(2, 1)
            Send_Constant_String(#test2)
            lcall sec_counter ; prevent the timer to go over 60
            lcall min_counter
            PushButton(HOME_BUTTON,done_setup_reflow) ; check if home button is pressed 
            mov state_lcd, #0
            done_setup_reflow:
            ljmp Forever
        ;------------------------------------------;
END
 