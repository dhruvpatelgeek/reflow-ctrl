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
       
    
        