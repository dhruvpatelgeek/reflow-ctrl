$MODLP51
org 0000H
   ljmp MainProgram





CLK  EQU 22118400
BAUD equ 115200
BRG_VAL equ (0x100-(CLK/(16*BAUD)))



DSEG at 30H
Result: ds 4
x:   ds 4
y:   ds 4
bcd: ds 5

bseg
mf: dbit 1

CSEG
; These �EQU� must match the wiring between the microcontroller and ADC 
 CE_ADC    EQU  P2.0 
 MY_MOSI   EQU  P2.1 
 MY_MISO   EQU  P2.2 
 MY_SCLK   EQU  P2.3

 LCD_RS equ P1.1
LCD_RW equ P1.2
LCD_E  equ P1.3
LCD_D4 equ P3.2
LCD_D5 equ P3.3
LCD_D6 equ P3.4
LCD_D7 equ P3.5

$NOLIST
$include(math32.inc)
$include(LCD_4bit.inc) ; A library of LCD related functions and utility macros
$LIST



; Configure the serial port and baud rate
InitSerialPort:
    ; Since the reset button bounces, we need to wait a bit before
    ; sending messages, otherwise we risk displaying gibberish!
    mov R1, #222
    mov R0, #166
    djnz R0, $   ; 3 cycles->3*45.21123ns*166=22.51519us
    djnz R1, $-4 ; 22.51519us*222=4.998ms
    ; Now we can proceed with the configuration
	orl	PCON,#0x80
	mov	SCON,#0x52
	mov	BDRCON,#0x00
	mov	BRL,#BRG_VAL
	mov	BDRCON,#0x1E ; BDRCON=BRR|TBCK|RBCK|SPD;
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

INIT_SPI:     
	setb MY_MISO    ; Make MISO an input pin  1 master input 0 slave out   ;MISO master in/slave out
	clr MY_SCLK     ; For mode (0,0) SCLK is zero     
	ret 

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

    

;Hello_World:
    ;DB  'Hello, World!', '\r', '\n', 0
Newline:
        DB   '\r', '\n', 0
Space:
        DB   '      ','\r', '\n', 0

convert:
    mov x+0, Result
	mov x+1, Result+1 
	mov x+2, #0
	mov x+3, #0
    
    Load_y(410)
    lcall mul32
    Load_y(1023)
    lcall div32
    Load_y(273)
    lcall sub32
    lcall hex2bcd
    lcall InitSerialPort
    Send_BCD(bcd+1)
    Set_Cursor(1, 7)
    Send_BCD(bcd)
    Display_BCD(bcd+0)
    lcall SendString
    mov DPTR, #Newline
    lcall SendString

ret
Test:  db 'Temp ', 0
nothing:          db '                ',0
hot:			  db '      HOT       ', 0

MainProgram:
        mov SP, #7FH ; Set the stack pointer to the begining of idata

        lcall LCD_4BIT
        Set_Cursor(1, 1)
        Send_Constant_String(#Test)
        lcall INIT_SPI
    Forever: 
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
        mov a, bcd
        cjne a, #0x27, continue
        lcall blink
        continue: jc continue2
        lcall blink
        continue2:
        lcall  WaitHalfSec 
        

        sjmp Forever
        
        
END
