;FSM for project1/FEB 1/Zahra
;; just to test connect led to p0.0
org 0000H
   ljmp MyProgram

;define key
CSEG
start equ p1.7   ;in slide it was KEY.3 which should be decided later so p1.7 is just a random pin

   
;defining variables

dseg AT 30H

temp_soak: ds 1 ;; temp to be soaked at
time_soak: ds 1 ;; time to be soaked at
temp_refl: ds 1 ;; temp to be reflow at
time_refl: ds 1 ;; temp to be reflowed at
state: ds 1     ;; what state it is at
temp: ds 1      ;; current_temp
sec: ds 1       ;; current seconds
pwm: ds 1       ;; current pwn variable

                                                                               

                                                                               

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
                                                                            

CSEG
;; send temp to be spiked to in r4 return when r4 is reached regardless of time
temp_dirac:
clr c
setb p0.0 
lcall WaitHalfSec
mov a r4
subb a temp
jnc temp_dirac
clr c
ret
;; time to sustain in r4 in seconds for reflow
sustain_temperature_for_x_seconds_ref:
mov a r4
djne a one_sec_soak_for_reflow
ret

one_sec_soak_for_reflow:
lcall WaitHalfSec
lcall WaitHalfSec
mov a temp_soak
subb a owen_temp
jnc soak_less

soak_more:
setb p0.0
clr c
sjmp sustain_temperature_for_x_seconds_ref

soak_less:
clr p0.0
clr c
sjmp sustain_temperature_for_x_seconds_ref



;; time to sustain in r4 in seconds
sustain_temperature_for_x_seconds:
mov a r4
djne a one_sec_soak
ret

one_sec_soak:
lcall WaitHalfSec
lcall WaitHalfSec
mov a temp_soak
subb a temp
jnc soak_less

soak_more:
setb p0.0
clr c
sjmp sustain_temperature_for_x_seconds

soak_less:
clr p0.0
clr c
sjmp sustain_temperature_for_x_seconds

;; time to sustain in r4 in seconds for reflow
sustain_temperature_for_x_seconds:
mov a r4
djne a one_sec_soak
ret

one_sec_soak:
lcall WaitHalfSec
lcall WaitHalfSec
mov a temp_soak
subb a temp
jnc soak_less

soak_more:
setb p0.0
clr c
sjmp sustain_temperature_for_x_seconds

soak_less:
clr p0.0
clr c
sjmp sustain_temperature_for_x_seconds

;; send in r1 var 1 
;; send in r2 var 2 
;; output  r3 ==1 if (r1 >= r2)
;;                  else 0                   
check_if_equal_or_greater:
mov a r1
subb a r2
cjne c #0 return_0
return_1:
mov a, #1
mov @r3,a
ret

return_0:
mov a, #0
mov @r3,a
ret

WaitHalfSec: 
mov R2, #89
q3: mov R1, #250
q2: mov R0, #166
q1: djnz R0, q1 ;3 cycles->3*45.21123ns*166=22.51519us
djnz R1, q2 ; 22.51519us*250=5.629ms
djnz R2, q3 ; 5.629ms*89=0.5s (approximately)
ret

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
      
      lcall check_if_equal_or_greater; checks if temp == temp soak returns r4 in a if yes else no

      mov a r3
      cjne a #1 state1 
      ;add branches to compare temp with 150
      mov state, #2
  state1_done:
       ljmp forever
       
  state2:
      cjne a, #2 , state3
      mov pwm, #20
      ;; turn the ssr on/off using pwm
      ;; mov into r4 how many seconds do you want to soak in seconds
      mov a time_soak
      mov @r4 a
      lcall sustain_temperature_for_x_seconds
      jnc state2_done
      mov state, #3
  state2_done:
       ljmp forever          
  
  state3:
      cjne a, #3 , state4
      mov pwm, #100
      mov sec, #0     
      mov a, temp_refl
      mov @r4 a
      lcall temp_dirac
      ;add branches to compare temp with 220
      mov state, #4
  state3_done:
       ljmp forever
       
   state4:
      cjne a, #4 , state5
      mov pwm, #20
      mov a, time_refl
      mov @r4 a
      lcall sustain_temperature_for_x_seconds_ref
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
       
    
        