;FSM for project1/FEB 1/Zahra

$NOLIST
$MOD9351
$LIST

CLK           EQU 7373000  ; Microcontroller system clock frequency in Hz
TIMER0_RATE   EQU 4096     ; 2048Hz squarewave (peak amplitude of CEM-1203 speaker)
TIMER0_RELOAD EQU ((65536-(CLK/(2*TIMER0_RATE))))
CCU_RATE      EQU 100      ; 100Hz, for an overflow rate of 10ms
CCU_RELOAD    EQU ((65536-(CLK/(2*CCU_RATE))))

CLEAR         equ P1.7
SOUND_OUT     equ P2.7
UPDOWN        equ P2.4

org 0000H
   ljmp MyProgram

; External interrupt 0 vector (not used in this code)
org 0x0003
	reti

; Timer/Counter 0 overflow interrupt vector
org 0x000B
	ljmp Timer0_ISR

; External interrupt 1 vector (not used in this code)
org 0x0013
	reti

; Timer/Counter 1 overflow interrupt vector (not used in this code
org 0x001B
	reti

; Serial port receive/transmit interrupt vector (not used in this code)
org 0x0023 
	reti

; CCU interrupt vector
org 0x005b 
	ljmp CCU_ISR


define key
CSEG
start equ P1.7   ;in slide it was KEY.3 which should be decided later so p1.7 is just a random pin


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
   
;defining variables

dseg AT 30H

temp_soak: ds 1 ; temp to soak
time_soak: ds 1 ; time to soak
temp_refl: ds 1 ; temp of relfow
time_refl: ds 1 ; time to reflow 
state: ds 1 ; current state 
temp: ds 1 ; current temp in degree C
sec: ds 1 ; current time in seconds 
pwm: ds 1 ; 

;_ _ _ _ | _ _ _ _ _ _
;
;pwm = 40 (say)
;then output will be 100 
;_________
;         |
;          _____________
; where period is 1 second 

Count10ms:    ds 1 ; Used to determine when half second has passed
BCD_counter:  ds 1 ; The BCD counter incrememted in the ISR and displayed in the main loop

bseg
half_seconds_flag: dbit 1 ; Set to one in the ISR every time 500 ms had passed

cseg
; These 'equ' must match the wiring between the microcontroller and the LCD!
LCD_RS equ P0.7
LCD_RW equ P3.0
LCD_E  equ P3.1
LCD_D4 equ P2.0
LCD_D5 equ P2.1
LCD_D6 equ P2.2
LCD_D7 equ P2.3
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
; Routine to initialize the CCU   ;
; We are using the CCU timer in a ;
; manner similar to timer 2       ;
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
; ISR for CCU                     ;
;---------------------------------;
CCU_ISR:
	mov TIFR2, #0 ; Clear CCU Timer Overflow Interrupt Flag bit. Actually, it clears all the bits!
	cpl P2.6 ; To check the interrupt rate with oscilloscope. It must be precisely a 10 ms pulse.
	
	; The two registers used in the ISR must be saved in the stack
	push acc
	push psw
	
	; Increment the 8-bit 10-mili-second counter
	inc Count10ms

Inc_Done:
	; set pwm pulse
    subb a, pwm ; if pwm greater than a pwm is on else off
    jnc off_segment
    setb p0.0
    clr c
    sjmp pass
    off_segment:
    clr p0.0
    clr c
    sjmp pass

    ; Check if 1 second has passed
    pass:
	mov a, Count10ms
	cjne a, #100, CCU_ISR_done ; Warning: this instruction changes the carry flag!
    ;----------------------------
	inc sec 
    ;----------------------------
	; 1000 milliseconds have passed.  Set a flag so the main program knows
	setb half_seconds_flag ; Let the main program know half second had passed
	cpl TR0 ; Enable/disable timer/counter 0. This line creates a beep-silence-beep-silence sound.
	; Reset to zero the 10-milli-seconds counter, it is a 8-bit variable
	mov Count10ms, #0
	; Increment the BCD counter
	mov a, BCD_counter
	jnb UPDOWN, CCU_ISR_decrement
	add a, #0x01
	sjmp CCU_ISR_da
CCU_ISR_decrement:
	add a, #0x99 ; Adding the 10-complement of -1 is like subtracting 1.
CCU_ISR_da:
	da a ; Decimal adjust instruction.  Check datasheet for more details!
	mov BCD_counter, a
	
CCU_ISR_done:
	pop psw
	pop acc
	reti

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
; Main program. Includes hardware ;
; initialization and 'forever'    ;
; loop.                           ;
;---------------------------------;

CSEG
MyProgram:
	mov sp, #07FH ; Initialize the stack pointer
	
forever:	
  mov a, state
  state0: 
      cjne a, #0, state1
      mov pwm, #0
      jb start, state0_done
      jnb start, $ ;wait for key release
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
      mov pwm, #100
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