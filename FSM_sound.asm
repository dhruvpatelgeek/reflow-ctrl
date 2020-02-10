Label1:
	ret
Label2:
	ret
Label3:
	ret
Label4:
	ret
Label5:
	ret
Label6:
	ret
Label7:
	ret
Label8:
	ret
Label9:
	ret
Label10:
	ret
Label11:
	ret
Label12:
	ret
Label13:
	ret
Label14:
	ret
Label15:
	ret
Label16:
	ret
Label17:
	ret
Label18:
	ret
Label19:
	ret
Label20:
	ret
Label30:
	ret
Label40:
	ret
Label50:
	ret
Label60:
	ret
Label70:
	ret
Label80:
	ret
Label90:
	ret
Label100:
	ret


FSM:
	mov a, FSM_state
	
State0:
	cjne a, #0, State1
	jnb FSM_start, state0_done
	clr c

State1:
        cjne FSM_state, #1, State2
	mov a, temp
	subb a, #20
	jnc temp_greater_than_20


temp_not_20:
	mov a, temp
	cjne a, #19, temp_not_19
	lcall Label19
  


temp_not_19:
	mov a, temp
	cjne a, #18, temp_not_18
        lcall Label18


temp_not_18:
	mov a, temp
	cjne a, #17, temp_not_17
        lcall Label17


temp_not_17:
	mov a, temp
	cjne a, #16, temp_not_16
        lcall Label16

temp_not_16:
	mov a, temp
	cjne a, #15, temp_not_15
        lcall Label15

temp_not_15:
	mov a, temp
	cjne a, #14, temp_not_14
        lcall Label14

temp_not_14:
	mov a, temp
	cjne a, #13, temp_not_13
        lcall Label13

temp_not_13:
	mov a, temp
	cjne a, #12, temp_not_12
        lcall Label12

temp_not_12:
	mov a, temp
	cjne a, #11, temp_not_11
        lcall Label11

temp_not_11:
	mov a, temp
	cjne a, #10, temp_not_10
        lcall Label10

temp_not_10:
	mov a, temp
	cjne a, #9, temp_not_9
        lcall Label9

temp_not_9:
	mov a, temp
	cjne a, #8, temp_not_8
        lcall Label8

temp_not_8:
	mov a, temp
	cjne a, 7, temp_not_7
        lcall Label7

temp_not_7:
	mov a, temp
	cjne a, #6, temp_not_6
        lcall Label6

temp_not_6:
	mov a, temp
	cjne a, #5, temp_not_5
        lcall Label5

temp_not_5:
	mov a, temp
	cjne a, #4, temp_not_4
        lcall Label4

temp_not_4:
	mov a, temp
	cjne a, #3, temp_not_3
        lcall Label3

temp_not_3:
	mov a, temp
	cjne a, #2, temp_not_2
        lcall Label2

temp_not_2:
	mov a, temp
	cjne a, #1, temp_not_1
	lcall Label1

temp_not_1:
	ret

temp_greater_than_20:
	mov FSM_state, #2

State2:
	cjne FSM_state, #2, State3
	mov a, temp
	cjne a, #29, State3
	lcall Label20

T2S_FSM_State4: 
	cjne a, #2, State3
	jb TMOD20, T2S_FSM_State4_Done 
	mov T2S_FSM_State, #3
T2S_FSM_State4_Done:
    ret

    subb a, #20
    subb a, #20
        lcall temp_not_10
	mov FSM_state, #3

State3:
	cjne FSM_state, #3, State4
	mov a, temp
	cjne a, #39, State4
	lcall Label30


T2S_FSM_State4: 
	cjne a, #3, State4 
	jb TMOD20, T2S_FSM_State4_Done 
	mov T2S_FSM_State, #4
T2S_FSM_State4_Done:
    ret

    subb a, #30
        lcall temp_not_10
	mov FSM_state, #4




State4:
	cjne FSM_state, #4, State5
	mov a, temp
	cjne a, #49, State5
	lcall Label40

    
T2S_FSM_State4: 
	cjne a, #4, State5
	jb TMOD20, T2S_FSM_State4_Done 
	mov T2S_FSM_State, #5
T2S_FSM_State4_Done:
    ret

    subb a, #40
        lcall temp_not_10
	mov FSM_state, #5

State5:
	cjne FSM_state, #5, State6
	mov a, temp
	cjne a, #59, State6
	lcall Label50


T2S_FSM_State4: 
	cjne a, #5, State6
	jb TMOD20, T2S_FSM_State4_Done 
	mov T2S_FSM_State, #6
T2S_FSM_State4_Done:
    ret

    subb a, #50
        lcall temp_not_10
	mov FSM_state, #6

State6:
	cjne FSM_state, #6, State7
	mov a, temp
	cjne a, #69, State7
	lcall Label60


T2S_FSM_State4: 
	cjne a, #6, State7
	jb TMOD20, T2S_FSM_State4_Done 
	mov T2S_FSM_State, #7
T2S_FSM_State4_Done:
    ret

    subb a, #60
        lcall temp_not_10
	mov FSM_state, #7

State7:
	cjne FSM_state, #7, State8
	mov a, temp
	cjne a, #79, State8
	lcall Label70


T2S_FSM_State4: 
	cjne a, #7, State8
	jb TMOD20, T2S_FSM_State4_Done 
	mov T2S_FSM_State, #8
T2S_FSM_State4_Done:
    ret

    subb a, #70
        lcall temp_not_10
	mov FSM_state, #8

State8:
	cjne FSM_state, #8, State9
	mov a, temp
	cjne a, #89, State9
	lcall Label80


T2S_FSM_State4: 
	cjne a, #8, State9
	jb TMOD20, T2S_FSM_State4_Done 
	mov T2S_FSM_State, #9
T2S_FSM_State4_Done:
    ret

    subb a, #80
        lcall temp_not_10
	mov FSM_state, #9

State9:
	cjne FSM_state, #9, State10
	mov a, temp
	cjne a, #99, State10
	lcall Label90


T2S_FSM_State4: 
	cjne a, #9, State10
	jb TMOD20, T2S_FSM_State4_Done 
	mov T2S_FSM_State, #10
T2S_FSM_State4_Done:
    ret

    subb a, #90
        lcall temp_not_10
	mov FSM_state, #10

State10:
	cjne FSM_state, #10, State11
	mov a, temp
	cjne a, #199, State11
	lcall Label100

    
T2S_FSM_State4: 
	cjne a, #10, State11
	jb TMOD20, T2S_FSM_State4_Done 
	mov T2S_FSM_State, #11
T2S_FSM_State4_Done:
    ret

	lcall State1
    subb a, #100
	mov FSM_state, #11

State11:
	cjne FSM_state, #11, State12
	mov a, temp
	cjne a, #250, State12
	lcall Label200

   
T2S_FSM_State4: 
	cjne a, #11, State12
	jb TMOD20, T2S_FSM_State4_Done 
	mov T2S_FSM_State, #12
T2S_FSM_State4_Done:
    ret

    subb a, #200
	lcall State1

State12:
	ret
















