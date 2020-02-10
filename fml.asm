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
            
    ; In the 8051 we can define direct access variables starting at location 0x30 up to location 0x7F
    dseg at 0x30
    
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
    Count10ms: ds 1 ; Used to determine when half second has passed
    product:   ds 1; pwm-currsec
    ;;owen_temp ds 1

    ;Timer variables
    Count1ms:     ds 2 ; Used to determine when half second has passed
    reflow_temp:  ds 2
    soak_temp:    ds 2
    reflow_temp_var: ds 1
    BCD_counter:  ds 1 ; The BCD counter incrememted in the ISR and displayed in the main loop
    hour:         ds 1
    reflow_sec:   ds 1
    reflow_min:   ds 1
    soak_sec:     ds 1
    soak_min:     ds 1




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
    half_seconds_flag: dbit 1 ; Set to one in the ISR every time 500 ms had passed


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
   
    cseg
    ; These 'equ' must match the wiring between the microcontroller and the LCD!
    LCD_RS equ P0.5
    LCD_RW equ P0.6
    LCD_E  equ P0.7
    LCD_D4 equ P1.2
    LCD_D5 equ P1.3
    LCD_D6 equ P1.4
    LCD_D7 equ P1.6


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



    $NOLIST
    $include(LCD_4bit_LPC9351.inc) ; A library of LCD related functions and utility macros
    $LIST

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

