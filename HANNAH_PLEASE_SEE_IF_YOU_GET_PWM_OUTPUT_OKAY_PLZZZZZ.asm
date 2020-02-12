; ISR_example.asm: a) Increments/decrements a BCD variable every half second using
; an ISR for timer 2; b) Generates a 2kHz square wave at pin P3.7 using
; an ISR for timer 0; and c) in the 'main' loop it displays the variable
; incremented/decremented using the ISR for timer 2 on the LCD.  Also resets it to 
; zero if the 'CLEAR' pushbutton connected to P1.7 is pressed.
$NOLIST
$MOD9351
$LIST

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
dseg at 0x30
Count10ms:    ds 1 ; Used to determine when half second has passed
BCD_counter:  ds 1 ; The BCD counter incrememted in the ISR and displayed in the main loop
temp_soak: ds 1 ; temp to soak
time_soak: ds 1 ; time to soak
temp_refl: ds 1 ; temp of relfow
time_refl: ds 1 ; time to reflow 
state: ds 1 ; current state 
temp: ds 1 ; current temp in degree C
sec: ds 1 ; current time in seconds 
product: ds 1; pwm-currsec
pwm: ds 1 ; 

;;; timer
w:             ds 3 ; 24-bit play counter.  Decremented in CCU ISR.
minutes:       ds 1
seconds:       ds 1
T2S_FSM_state: ds 1
Count5ms:      ds 1
five_sec_flag:  ds 1

BSEG
T2S_FSM_start: dbit 1
seconds_flag:  dbit 1

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
bseg
half_seconds_flag: dbit 1 ; Set to one in the ISR every time 500 ms had passed

cseg
; These 'equ' must match the wiring between the microcontroller and the LCD!
LCD_RS equ P0.5
LCD_RW equ P0.6
LCD_E  equ P0.7
LCD_D4 equ P1.2
LCD_D5 equ P1.3
LCD_D6 equ P1.4
LCD_D7 equ P1.6
$NOLIST
$include(LCD_4bit_LPC9351.inc) ; A library of LCD related functions and utility macros
$LIST

;                     1234567890123456    <- This helps determine the location of the counter
Initial_Message:  db 'BCD_counter: xx ', 0

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

;---------------------------------------------------------------------------------;
; This is the FSM that plays minutes and seconds after the STOP button is pressed ;
; The state diagram of this FSM is available as 'Stop_Watch_FSM.pdf'              ;
;---------------------------------------------------------------------------------;
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
;------------------------------
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
    
    mov minutes, #0
	mov seconds, #0

    lcall LCD_4BIT
    ; For convenience a few handy macros are included in 'LCD_4bit_LPC9351.inc':
	Set_Cursor(1, 1)
    Display_BCD(BCD_counter)

    setb half_seconds_flag
	mov BCD_counter, #0x00
	mov pwm , #0
	mov sec , #0
	mov state, #0
	mov temp, #150
    mov time_soak, #5
    mov temp_refl, #220
    mov five_sec_flag,#0
	; After initialization the program stays in this 'forever' loop
    
forever:	
    lcall T2S_FSM
	; One second has passed, refresh the LCD with new time
	Set_Cursor(1, 1)
    Send_Constant_String(#timee)
    Set_Cursor(1, 5)
    Display_BCD(sec)
    Set_Cursor(2, 1)
    Send_Constant_String(#statee)
    Set_Cursor(2, 5)
    Display_BCD(state)


    mov a, five_sec_flag
    cjne a,#5, pass_quack
    quack_like_a_duck:
    clr TR1 ; Stop timer 1.
    mov a,#0
    mov five_sec_flag,a
    clr TR1 ; Stop timer 1.
	setb T2S_FSM_Start ; This plays the current minutes:seconds by making the state machine get out of state zero.
    pass_hash:
    pass_quack:
    setb TR1 ; en timer 1.
    
    mov a, state
  state0: 
      cjne a, #0, state1
      mov pwm, #0
      jb p2.0, state0_done
      jnb p2.0, $ ;wait for key release
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
       
  state2:
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