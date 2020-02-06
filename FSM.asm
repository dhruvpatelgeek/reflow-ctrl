;FSM for project1/FEB 1/Zahra
org 0000H
   ljmp MyProgram

;define key
CSEG
start equ p1.7   ;in slide it was KEY.3 which should be decided later so p1.7 is just a random pin

   
;defining variables

dseg AT 30H

temp_soak: ds 1
time_soak: ds 1
temp_refl: ds 1
time_refl: ds 1
state: ds 1
temp: ds 1
sec: ds 1
pwm: ds 1

                                                                               

                                                                               

;                                                        -                     
;                                                       -  -                    
;                                                      -    -                   
;                                                     -     --                  
;                                                    -       --                 
;                                                   -         -                 
;                                                  -            -               
;                                                 -              -              
;                                                -                -             
;                                               -                 --            
;                                             --                   --           
;                we will use timer 1 for this -                     -          
;               -----------------------------                        -         
;              -                                                      -        
;             -                                                        --       
;            -                                                          -       
;          -                                                             -      
;         -                                                               -     
;        -                                                                 -    
;      -                                                                    -    
;     -                                                                      -   
;   -                                                                         -   
;   state 1 ((temp==soak)? ssr_off: ssr_on)
;          state 2 ((time=soak_time)?(pwm_off):(pwn_on))
;                                           state 3 ((temp==soak)? ssr_off: ssr_on)
;                                                        state 4 (cooling ssr_off)
;                                                                             state 5 (done)
                                                                            

CSEG
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
       
    
        