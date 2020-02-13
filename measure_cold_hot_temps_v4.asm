$MOD9351
org 0000H
   ljmp MainProgram
   
cseg
;definitions for clk, baud rate, etc.
XTAL EQU (14746000/2) 
BAUD EQU 115200
BRVAL EQU ((XTAL/BAUD)-16)
OP_AMP_GAIN EQU 340 ;what is the exact gain?

;define the connections between the ADC and MCU (P89 & MCP3008)
CE_ADC    EQU  P1.7;P2.4   ;SS
MY_MOSI   EQU  P3.1 ;P2.2  ;MOSI
MY_MISO   EQU  P0.3   ;MISO
MY_SCLK   EQU  P0.2   ;SPICLK

;$NOLIST
;$include(LCD_4bit.inc) ; A library of LCD related functions and utility macros
;$LIST

$NOLIST
$include(math32.inc) ;used in this program to convert voltages to temperatures
$LIST 

dseg AT 30H
;variables we need
x: ds 4
y: ds 4
Result: ds 2 
ch0: ds 2
ch1: ds 2
oven_temp: ds 2
bcd: ds 5

bseg
mf: dbit 1

cseg
;INITIALIZE AND CONFIGURE THE SERIAL PORT OF THE P89
InitSerialPort:
	;DEBOUNCING??
	; Since the reset button bounces, we need to wait a bit before
    ; sending messages, otherwise we risk displaying gibberish!
    ;mov R1, #222
    ;mov R0, #166
    ;djnz R0, $   ; 3 cycles->3*45.21123ns*166=22.51519us
    ;djnz R1, $-4 ; 22.51519us*222=4.998ms
    ; Now we can proceed with the configuration
	
	mov	BRGCON,#0x00
	mov	BRGR1,#high(BRVAL)
	mov	BRGR0,#low(BRVAL)
	mov	BRGCON,#0x03 ; Turn-on the baud rate generator
	mov	SCON,#0x52 ; Serial port in mode 1, ren, txrdy, rxempty
	mov	P1M1,#0x00 ; Enable pins RxD and TXD
	mov	P1M2,#0x00 ; Enable pins RxD and TXD
	ret
	
INIT_SPI:     
	setb MY_MISO    ; Make MISO an input pin     
	clr MY_SCLK     ; For mode (0,0) SCLK is zero     
	ret
;bit-bang SPI
DO_SPI_G:     
	push acc     
	mov R1, #0      ; Received byte stored in R1     
	mov R2, #8      ; Loop counter (8-bits)
DO_SPI_G_LOOP:     
	mov a, R0       ; Byte to write is in R0     
	rlc a           ; Carry flag has bit to write     
	mov R0, a     
	mov MY_MOSI, c 
	setb MY_SCLK    ; Transmit     
	mov c, MY_MISO  ; Read received bit     
	mov a, R1       ; Save received bit in R1    
	rlc a 
	mov R1, a     
	clr MY_SCLK     
	djnz R2, DO_SPI_G_LOOP     
	pop acc     
	ret 
	
;---------------------------------;
; Send a BCD number to PuTTY      ;
;---------------------------------;
Send_BCD mac
	push ar0
	mov r0, %0
	lcall ?Send_BCD
	pop ar0
endmac

?Send_BCD:
	push acc
	; Write most significant digit
	mov a, r0
	swap a
	anl a, #0fh
	orl a, #30h
	lcall putchar
	; write least significant digit
	mov a, r0
	anl a, #0fh
	orl a, #30h
	lcall putchar
	pop acc
	ret	 

; Send a character using the serial port
putchar:
	jbc	TI,putchar_L1
	sjmp putchar
putchar_L1:
	mov	SBUF,a
	ret
	
getchar:
	jbc	RI,getchar_L1
	sjmp getchar
getchar_L1:
	mov	a,SBUF
	ret	
    
;approximate delay of 1s
Delay:
    mov R2, #89*2
L4: mov R1, #250
L5: mov R0, #166
L6: djnz R0, L6 ; 3 cycles->3*45.21123ns*166=22.51519us
    djnz R1, L5 ; 22.51519us*250=5.629ms
    djnz R2, L4 ; 5.629ms*89=0.5s (approximately)
    ret
    
HexAscii: db '0123456789ABCDEF'

SendTemp:
	mov dptr, #HexAscii 
	
	mov a, bcd+1
	swap a
	anl a, #0xf
	movc a, @a+dptr
	lcall putchar
	mov a, bcd+1
	anl a, #0xf
	movc a, @a+dptr
	lcall putchar

	mov a, #'.'
	lcall putchar

	mov a, bcd+0
	swap a
	anl a, #0xf
	movc a, @a+dptr
	lcall putchar
	mov a, bcd+0
	anl a, #0xf
	movc a, @a+dptr
	lcall putchar
	
	mov a, #'\r'
	lcall putchar
	mov a, #'\n'
	lcall putchar	
	ret    

SendString:
    clr a
    movc a, @a+dptr
    jz SendString_L1
    lcall putchar
    inc dptr
    sjmp SendString  
SendString_L1:
	ret
    
new_line:
      DB '\r' ,'\n', 0     
    
Display_putty:
	Send_BCD(bcd+1)
	Send_BCD(bcd+0)
	ret   

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

MainProgram:
    mov SP, #7FH ; Set the stack pointer to the begining of idata
    lcall Ports_Init
    lcall InitSerialPort
    mov P2M1, #0
    mov P2M2, #0

forever:
	;read channel 0 of the ADC and transmitting this info to the MCU
	clr CE_ADC ;enable device (active low)
	;transmit the info from channel 0
	mov R0, #00000001B ;start bit:1
	lcall DO_SPI_G
	
	mov R0, #10000000B ;read channel 0
	lcall DO_SPI_G
	mov a, R1          ; R1 contains bits 8 and 9 
	anl a, #00000011B  ; We need only the two least significant bits
	mov ch0+1, a    ; Save result high
	
	mov R0, #55H   ; It doesn't matter what we transmit... 
	lcall DO_SPI_G  
	mov ch0+0, R1     ; R1 contains bits 0 to 7.  Save result low. 
	setb CE_ADC ;disable device
	
	clr CE_ADC ;enable device
	;transmit from channel 1
	mov R0, #00000001B ;start bit:1
	lcall DO_SPI_G
	
	mov R0, #10010000B ;read channel 1
	lcall DO_SPI_G
	mov a, R1          ; R1 contains bits 8 and 9 
	anl a, #00000011B  ; We need only the two least significant bits
	mov ch1+1, a    ; Save result high
	
	mov R0, #55H   ; It doesn't matter what we transmit... 
	lcall DO_SPI_G  
	mov ch1+0, R1     ; R1 contains bits 0 to 7.  Save result low. 
	setb CE_ADC ;disable device (active low)
	
	lcall Delay ;about 1s
	
	;convert the voltage from ch0(LM335) to temperature and display in putty - ch0 correspond to the cold jnc temp
	mov x+0, ch0
	mov x+1, ch0+1
	mov x+2, #0
	mov x+3, #0
	
	load_y(410)
	lcall mul32
	load_y(1023)
	lcall div32
	load_y(273)
	lcall sub32 
	lcall hex2bcd
	;lcall Display_putty
	
	;do the same for ch1(output voltage of OP AMP) - ch1 corresponds to the hot jnc temp
	mov x+0, ch1
	mov x+1, ch1+1
	mov x+2, #0
	mov x+3, #0
	
	load_y(1000000)
	lcall mul32
	load_y(OP_AMP_GAIN)
	lcall div32
	load_y(41)
	lcall div32 

    load_y(8000)
    lcall add32
    load_y(258)
    lcall div32
    
    ;load_y(24)
    ;lcall add32
    
    ;load_y(10)
    ;lcall div32

	lcall hex2bcd
	lcall Display_putty
	
	mov a, #'\r'
	lcall putchar
	mov a, #'\n'
	lcall putchar
	
	;now that we have the cold and hot junction temps in variables ch0 and ch1 respectively, we can add them to get the actual temperature
	
	
	ljmp Forever
   
END