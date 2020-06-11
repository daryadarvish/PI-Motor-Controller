;**************************************************************************************
;* Lab5 Main [includes LibV2.1]                                                       *
;**************************************************************************************
;* Summary:                                                                           *
;*   -                                                                                *
;*                                                                                    *
;* Author: Darya Darvish, Kellen Fujishin                                             *
;*   Cal Poly University                                                              *
;*   Fall 2019                                                                        *
;*                                                                                    *
;* Revision History:                                                                  *
;*   -                                                                                *
;*                                                                                    *
;* ToDo:                                                                              *
;*                                                                                    *
;**************************************************************************************

;/------------------------------------------------------------------------------------\
;| Include all associated files                                                       |
;\------------------------------------------------------------------------------------/
; The following are external files to be included during assembly


;/------------------------------------------------------------------------------------\
;| External Definitions                                                               |
;\------------------------------------------------------------------------------------/
; All labels that are referenced by the linker need an external definition

              XDEF  main

;/------------------------------------------------------------------------------------\
;| External References                                                                |
;\------------------------------------------------------------------------------------/
; All labels from other files must have an external reference

              XREF  ENABLE_MOTOR, DISABLE_MOTOR
              XREF  STARTUP_MOTOR, UPDATE_MOTOR, CURRENT_MOTOR
              XREF  STARTUP_PWM, STARTUP_ATD0, STARTUP_ATD1
              XREF  OUTDACA, OUTDACB
              XREF  STARTUP_ENCODER, READ_ENCODER
              XREF  INITLCD, SETADDR, GETADDR, CURSOR_ON, CURSOR_OFF, DISP_OFF
              XREF  OUTCHAR, OUTCHAR_AT, OUTSTRING, OUTSTRING_AT
              XREF  INITKEY, LKEY_FLG, GETCHAR
              XREF  LCDTEMPLATE, UPDATELCD_L1, UPDATELCD_L2
              XREF  LVREF_BUF, LVACT_BUF, LERR_BUF,LEFF_BUF, LKP_BUF, LKI_BUF
              XREF  Entry, ISR_KEYPAD
            
;/------------------------------------------------------------------------------------\
;| Assembler Equates                                                                  |
;\------------------------------------------------------------------------------------/
; Constant values can be equated here

PORTT         EQU     $0240
DDRT          EQU     $0242

TIOS          EQU     $0040
TCNT          EQU     $0044
TSCR          EQU     $0046
TCTL2         EQU     $0049
TMSK1         EQU     $004C
TFLG1         EQU     $004E
TC0           EQU     $0050



;/------------------------------------------------------------------------------------\
;| Variables in RAM                                                                   |
;\------------------------------------------------------------------------------------/
; The following variables are located in unpaged ram

DEFAULT_RAM:  SECTION

MOTOR_STATE       DS.B  1 ;state of motor
TC0_STATE         DS.B  1 ;state of interrupt
dispstate         DS.B  1	;state of display
KEYPAD_STATE      DS.B  1	;state of keypad
MASTERMIND_STATE  DS.B  1	;state of M^2

KEY_BUF           DS.B  1	;contains key to be analyzed
KEY_FLG           DS.B  1	;raised when keypad registers a key input
INTERVAL          DS.W  1	;used to define the period of interrupts
RUN               DS.B  1 ;boolean to tell the 
D_ON_D_OFF        DS.B  1 ;tells ISR to count down to updating the display or not
V_REF             DS.W  1 ;reference velocity imputed by user 
V_REF_BUF         DS.B  4 ;variable to temporarily store a new VREF
V_ACT             DS.W  1 ;actual velocity of the motor
VREF_PROMPT       DS.B  1 ;M^2 flag that signifies a new VREF is being imputed
VREF_CURSOR       DS.B  1 ;display flag to place cursor at the VREF prompt
KP                DS.W  1 ;gain value for KP
KP_PROMPT         DS.B  1 ;M^2 flag that signifies a new KP value is being imputed
KP_CURSOR         DS.B  1 ;display flag to place cursor at the KP prompt
KI                DS.W  1 ;gain value for KP
KI_PROMPT         DS.B  1 ;M^2 flag that signifies a new KI value is being imputed
KI_CURSOR         DS.B  1 ;display flag to place cursor at the KP prompt
FIRSTCHAR         DS.B  1 ;signifies that the first character of a message is to be printed
TAKE2COMP         DS.B  1 ;is VREF negative or not
OL_CL             DS.B  1 ;open loop or closed loop
BACKSPACEDISP     DS.B  1	;tells code to print a backspace
POINTER           DS.W  1 ;address pointer for buffers
ECHO              DS.B  1 ;tells display to echo an entered digit
COUNT             DS.B  1 ;how many digits have been converted
DIGIT_COUNT       DS.B  1 ;how many digits have been entered
DPTR              DS.W  1 ;points to character of message to be displayed
D_VREF            DS.B  1 ;display flag to display the VREF prompt
D_KP              DS.B  1 ;display flag to display the KP prompt
D_KI              DS.B  1 ;display flag to display the KI prompt

D_RUN             DS.B  1 ;displays RUN
D_STP             DS.B  1 ;displays STP
D_CL              DS.B  1 ;displays CL
D_OL              DS.B  1 ;displays OL
D_D_ON            DS.B  1 ;displays D_ON
D_D_OFF           DS.B  1 ;displays D_OFF
UPDATEDISPLAY     DS.B  1 ;tells M^2 to update Line 1

ENCODER_COUNT     DS.W  1 ;motor position
ENCODER_COUNT_OLD DS.W  1 ;previous motor position
KP_ERR            DS.W  1 ;used for SDBA to obtain a  
KI_ERR            DS.W  1 ;used for SDBA to obtain a
ESUM              DS.W  1 ;sum of error values
A_STAR            DS.W  1 ;duty cycle to be outputed to the motor
A_INIT            DS.W  1 ;first a value
ERR               DS.W  1 ;difference between VREF and VACT
ESUM_OLD          DS.W  1 ;previous ESUM
EFFORT            DS.W  1 ;effort needed by the motor
TEMP1             DS.W  1 ;quotient of the BCD2ASCII conversion
UPDATECOUNT       DS.B  1 ;counts down to update the display every 255 interrupts
TEMPADDR          DS.B  1 ;stores the LCD address before updating
REMAIN            DS.W  1 ;used to calculate effort


;/------------------------------------------------------------------------------------\
;|  Main Program Code                                                                 |
;\------------------------------------------------------------------------------------/
; Your code goes here

MyCode:       SECTION

main:     
        clr     MOTOR_STATE               ;clearing of variables              
        clr     TC0_STATE        
        clr     LKEY_FLG
        clr     TC0_STATE
        clr     dispstate
        clr     KEYPAD_STATE
        clr     MASTERMIND_STATE
        clr     VREF_PROMPT
        clr     VREF_CURSOR
        clr     KP_PROMPT
        clr     KP_CURSOR
        clr     KI_PROMPT
        clr     KI_CURSOR
        clr     TAKE2COMP
        
        clr     ECHO
        clr     COUNT
        clr     DIGIT_COUNT
        clr     D_VREF
        clr     D_KP
        clr     D_KI
        clr     D_D_OFF
        clr     D_OL
        clr     D_RUN
        clr     BACKSPACEDISP
        clr     UPDATEDISPLAY
        clr     RUN
         
       
        movb    #$01, FIRSTCHAR
;-------------------------------BUFFER_SET_TO_0----------------------------        
        ldx     #LVREF_BUF
        movb    #$20,0,x                 	;set ascii input
        movb    #$20,1,x                 	;set ascii input
        movb    #$20,2,x                 	;set ascii input
        movb    #$20,3,x                 	;set ascii input
        movb    #$20,4,x                 	;set ascii input
        ldx     #LVACT_BUF
        movb    #$20,0,x                 	;set ascii input
        movb    #$20,1,x                 	;set ascii input
        movb    #$20,2,x                 	;set ascii input
        movb    #$20,3,x                 	;set ascii input
        movb    #$20,4,x                 	;set ascii input
        ldx     #LERR_BUF
        movb    #$20,0,x                 	;set ascii input
        movb    #$20,1,x                 	;set ascii input
        movb    #$20,2,x                 	;set ascii input
        movb    #$20,3,x                 	;set ascii input
        movb    #$20,4,x                 	;set ascii input
        ldx     #LEFF_BUF
        movb    #$20,0,x                 	;set ascii input
        movb    #$20,1,x                 	;set ascii input
        movb    #$20,2,x                 	;set ascii input
        movb    #$20,3,x                 	;set ascii input
        movb    #$20,4,x                 	;set ascii input
        ldx     #LKP_BUF
        movb    #$20,0,x                 	;set ascii input
        movb    #$20,1,x                 	;set ascii input
        movb    #$20,2,x                 	;set ascii input
        movb    #$20,3,x                 	;set ascii input
        movb    #$20,4,x                 	;set ascii input
        ldx     #LKI_BUF
        movb    #$20,0,x                 	;set ascii input
        movb    #$20,1,x                 	;set ascii input
        movb    #$20,2,x                 	;set ascii input
        movb    #$20,3,x                 	;set ascii input
        movb    #$20,4,x                 	;set ascii input
;-----------------------------------------------------------------------------        
        clrw    EFFORT                    ;clearing of variables cont'd
        clrw    ERR
        clrw    V_ACT                                
        clrw    V_REF
        clrw    KI
        clrw    KP
        
        movw    #20000, INTERVAL		      ;sets interval to correspond with 0.2ms
        clrw    ENCODER_COUNT_OLD         ;clearing and setting of variables
        clrw    ESUM
        clrw    ESUM_OLD
        clrw    EFFORT
        clr     RUN 
        movb    #$01,OL_CL
        movb    #$01,D_ON_D_OFF
        movb    #255,UPDATECOUNT
        
        
        movb    #$01,D_D_ON
        movb    #$01,D_CL
        movb    #$01,D_STP
        
Top:
        jsr MOTOR                         ;execute tasks endlessly
        jsr TC0_
        jsr MASTERMIND
        jsr KEYPAD
        jsr DISPLAY
        
        bra Top
        
;-------------TASK_1 - MOTOR_INIT ----------------------------------------------------------

MOTOR:
        ldaa MOTOR_STATE                  ;execute states in order
        beq  MOTOR_STATE_1                
        deca
        beq  MOTOR_STATE_2
        
MOTOR_STATE_1:
        clrw  ENCODER_COUNT               ;clear ENCODER_COUNT
        jsr   STARTUP_PWM                 ;startup procedure for motor
        jsr   STARTUP_MOTOR
        jsr   ENABLE_MOTOR
        jsr   STARTUP_ENCODER             ;startup for motor
        movb  #$01, MOTOR_STATE           ;initialization finished
        
MOTOR_STATE_2:                            ;task completed
        rts

       
;-------------TASK_1 - TC0----------------------------------------------------------

TC0_:
        ldaa  TC0_STATE                 ;execute states in order
        beq   TC0_STATE_0		            ;initialization of interrupts
        deca  
        beq   TC0_STATE_1

TC0_STATE_0:
        bset    TIOS,  $01            	;sets IOS0
        bset    TCTL2, $01            	;TE (Timer Enable)
        bclr    TCTL2, $02            	;TE (Timer Enable)
        bset    TFLG1, $01            	;C0I (Channel Interrupt Mask Request Enabled)
        
        cli
        
        bset    TMSK1, $01            	;toggles OM0 and OL0
        bset    TSCR, $A0               ;stores 10 in TSCR
              
        ldd     TCNT                 	  ;Stores Timer Channel 0 Register in D
        addd    INTERVAL              	;adds contents of interval to contents of D
        std     TC0                   	;stores contents of d in timer channel 0 register
        movb    #$01, TC0_STATE		      ;After initialization, this task does not do anything else
        rts

TC0_STATE_1:
        rts                             ;task completed       

;-------------TASK_2 MASTERMIND-----------------------------------------------------

MASTERMIND:
        ldaa  MASTERMIND_STATE		      ;execute states in order
        beq   MASTERMIND_INIT
        deca  
        beq   MASTERMIND_WAIT_FOR_DISPLAY
        deca  
        beq   MASTERMIND_HUB
            
MASTERMIND_INIT:  
        clr   KEYPAD			              ;initialize keypad
        jsr   INITLCD			              ;initialize the LCD
        jsr   LCDTEMPLATE               ;initialize LCD template
        movb  #$01, MASTERMIND_STATE	  ;move to WAITING state
        MOVB  #$01, FIRSTCHAR           ;intialize FIRSTCHAR to 1
        rts        
MASTERMIND_WAIT_FOR_DISPLAY:
        tst   D_STP                     ;wait for STP to display
        beq   DISPLAYCHECK_2            ;if completed, branch to next check
        rts
DISPLAYCHECK_2:
        tst   D_CL                      ;wait for CL to display
        beq   DISPLAYCHECK_3            ;if completed, branch to next check
        rts
DISPLAYCHECK_3:
        tst   D_D_ON                    ;wait for D_ON to display
        beq   exit2                     ;if completed, display is ready
        rts        
exit2:        
        movb  #$02, MASTERMIND_STATE	  ;move to hub state
        rts                 
MASTERMIND_HUB:
        tst   UPDATEDISPLAY             ;test if display is to be updated
        bne   UPDATEDISP                ;branch if 1
HUBANALYZE:        
        tst   KEY_FLG			              ;tests if there is a key press
        bne   ANALYZE			              ;branch if there is a key to analyze
        rts				                      ;stay in hub if nothing has been pressed

ANALYZE:
        ldab  KEY_BUF                  	;loads the key to be analyzed
        cmpb  #$08		                	;checks if key is a backspace
        lbeq  MASTERMIND_BSPACE	      	;branch if key is a backspace
        cmpb  #$0A                     	;checks if the key is enter
        lbeq  MASTERMIND_ENTER	      	;branch if the key is enter
        cmpb  #$41			                ;corresponds to RUN/STP
        lbeq	TOGGLEA		                ;branch accordingly
        cmpb  #$42                      ;corresponds to D_ON/D_OFF
        lbeq  TOGGLEB                   ;branch accordingly
        cmpb  #$43			                ;corresponds to VREF
        lbeq  VREF_1		                ;branch accordingly
        cmpb  #$44		                	;corresponds KP
        lbeq  KP_1		                	;branch accordingly
        cmpb  #$45			                ;corresponds to KI
        lbeq  KI_1                      ;branch accordingly
        cmpb  #$46                      ;corresponds to CL/OL
        lbeq  TOGGLEF		                ;branch accordingly
        cmpb  #$F2                      ;corresponds to -
        lbeq  NEGATIVE_SIGN             ;branch accordingly
        lbra  MASTERMIND_DIGIT         	;if none of the above, it is a digit
        
        lbeq  EXIT_MAST
UPDATEDISP:
        ldd   V_REF                     ;loads VREF value
        ldy   #LVREF_BUF                ;loads address of VREF buffer
        jsr   BINARY2ASCII 
        ldd   ERR                       ;load Error value
        ldy   #LERR_BUF                 ;load address of Error buffer
        jsr   BINARY2ASCII              ;converts value to ASCII, stores in buffer
        ldd   EFFORT                    ;load Effort value
        ldy   #LEFF_BUF                 ;load address of Effort buffer
        jsr   BINARY2ASCII              ;converts value to ASCII, stores in buffer
        ldd   V_ACT                     ;load VACT value
        ldy   #LVACT_BUF                ;load address of VREF buffer
        jsr   BINARY2ASCII              ;converts value to ASCII, stores in buffer
        jsr   GETADDR                   ;get current LCD address
        staa  TEMPADDR                  ;stores the address to be read later
        jsr   UPDATELCD_L1              ;update line 1 with all buffers
        ldaa  TEMPADDR                  ;load the stored address
        jsr   SETADDR                   ;set the stored address as the current address
        clr   UPDATEDISPLAY             ;clear update display flag
        lbra  HUBANALYZE                ;resume M^2 operations
EXIT_MAST:
        rts  

;-------------BACKSPACE----------------------------       
MASTERMIND_BSPACE:
        tst   VREF_PROMPT               ;checks if backspacing VREF
        bne   BSCONTD_VREF              ;branch if backspacing VREF
        tst   KP_PROMPT                 ;checks if backspacing KP
        bne   BSCONTD_KP                ;branch if backspacing KP
        tst   KI_PROMPT                 ;checks if backspacing KI
        bne   BSCONTD_KI                ;branch if backspacing KI
        beq   BSINVALID                 ;invalid if no prompt is specified
BSCONTD_VREF:
        ldaa  DIGIT_COUNT              	;checks if digits have been loaded
        lbeq  TEST_NEGATIVE             ;test it the negative sign is being deleted
        ldx   #V_REF_BUF                ;modifies buffer to remove the offending key
        movb  #$20,a,x                 	;loads a blank instead
        movb  #$01, BACKSPACEDISP      	;sets a backspace for display
        dec   DIGIT_COUNT              	;decreases digit count
        incw  POINTER                  	;moves pointer back to the appropriate address
        movb  #$00, KEY_FLG            	;clears KEY_FLG to wait for input
        rts
BSCONTD_KP:
        ldaa  DIGIT_COUNT              	;checks if digits have been loaded
        beq   BSINVALID                	;branch to invalid backspace
        ldx   #LKP_BUF                  ;modifies buffer to remove the offending key
        decx                            ;decrements the address
        movb  #$20,a,x                 	;loads a blank instead
        movb  #$01, BACKSPACEDISP      	;sets a backspace for display
        dec   DIGIT_COUNT              	;decreases digit count
        incw  POINTER                  	;moves pointer back to the appropriate address
        movb  #$00, KEY_FLG            	;clears KEY_FLG to wait for input
        rts
BSCONTD_KI:
        ldaa  DIGIT_COUNT              	;checks if digits have been loaded
        beq   BSINVALID                	;branch to invalid backspace
        ldx   #LKI_BUF                  ;modifies buffer to remove the offending key
        decx                            ;decreases the address
        movb  #$20,a,x                 	;set ascii input
        movb  #$01, BACKSPACEDISP      	;sets a backspace for display
        dec   DIGIT_COUNT              	;decreases digit count
        incw  POINTER                  	;moves pointer back to the appropriate address
        movb  #$00, KEY_FLG            	;clears KEY_FLG to wait for input
        rts
TEST_NEGATIVE:
        tst   TAKE2COMP                 ;if negative sign has been pressed, take the 2comp
        beq   BSINVALID                 ;if no negative sign has been pressed, bspace invalid
        clr   TAKE2COMP                 ;clear negative flag
        movb  #$01, BACKSPACEDISP       ;print a backspace                
BSINVALID:
        movb  #$00, KEY_FLG             ;clears KEY_FLG to wait for valid entry
        rts
        

;-------------ENTER----------------------------  


              
MASTERMIND_ENTER:        
TEST:
        tst   DIGIT_COUNT
        lbeq  INVALIDENTRY
        tst   VREF_PROMPT               ;checks if entering VREF
        bne   VREF_CONV         	      ;branch if entering VREF
        tst   KP_PROMPT                 ;checks if entering KP
        bne   KP_CONV                   ;branch if entering KP
        tst   KI_PROMPT                 ;checks if entering KI
        bne   KI_CONV                   ;branch if entering KI
        clr   KEY_FLG                  	;clears KEY_FLG to wait for valid entry
        rts
VREF_CONV:        
        jsr   VREF_CONVERSION           ;converts BUFFER to a time period                                              
        clr   VREF_PROMPT               ;done entering digits
        clr   DIGIT_COUNT              	;resets DIGIT_COUNT for next entry
        clr   KEY_FLG                 	;clears KEY_FLG to wait for next entry
        clr   TAKE2COMP                 ;clears the negative flag
        jsr   LCDTEMPLATE               ;resets LCD template
        jsr   UPDATELCD_L1              ;update L1
        jsr   UPDATELCD_L2              ;update L2
        movb  #$01,D_D_ON               ;display should update
        tst   RUN                       ;test RUN
        beq   REPRINTD_RUN0             ;if in STP, reprint STP
        movb  #$01,D_RUN                ;if in RUN, reprint RUN
OLCL:   tst   OL_CL                     ;test OL_CL
        beq   REPRINT_OL                ;if in OL, reprint OL
        movb  #$01,D_CL                 ;if in CL, reprint CL
        rts        
REPRINT_OL
        movb  #$01,D_OL                 ;reprint OL        
        rts
REPRINTD_RUN0:
        movb  #$01,D_STP                ;reprint STP
        bra   OLCL

KP_CONV:        
        jsr   KP_CONVERSION             ;converts BUFFER to a time period                                              	
        clr   DIGIT_COUNT              	;resets DIGIT_COUNT for next entry
        movb  #$00, KEY_FLG            	;clears KEY_FLG to wait for next entry
        rts
KI_CONV:        
        jsr   KI_CONVERSION             ;converts BUFFER to a time period          
        ;clr   KI_PROMPT
        clr   DIGIT_COUNT              	;resets DIGIT_COUNT for next entry
        movb  #$00, KEY_FLG            	;clears KEY_FLG to wait for next entry
        rts
 ;-------------VREF_CONV---------------------------------------------------------


VREF_CONVERSION:
        
        pshb                           	;pushes contents of b to the stack
        pshy                           	;pushes contents of y to the stack
        movw  #V_REF_BUF, POINTER       ;move address of buffer into pointer
        incw  POINTER                   ;increment pointer to first digit
        movb  DIGIT_COUNT, COUNT        ;sets COUNT to however many digits are loaded
        clrw  V_REF                  	  ;clears VREF


VREF_LOOP:
        ldd   #$0A                     	;loads 10 hex in to accumulator D
        ldy   V_REF                    	;loads VREF into accumulator Y
        EMUL                           	;multiplies D and Y and Stores into D and Y
        std   V_REF                    	;stores contents of D into VREF                              
        ldy   POINTER                  	;loads pointer in y  
        ldaa  0,y                      	;loads content of index 0 of y into a
        ldab  #$30                     	;loads 30 hex into acuumulator B
        sba                            	;subtracts B from A and stores result in A
        tab                            	;moves contents of A into B
        clra                           	;all bits in A are cleared
    
        addd  V_REF			                ;adds contents of VREF to D and stores into D
        std   V_REF                   	;store D in VREF
        incw  POINTER                 	;increment POINTER
        
        dec   COUNT                    	;Decrements COUNT
        ldaa  COUNT                    	;If count is 0, conversion is done
        tsta                            ;test if count is 0
        bne   VREF_LOOP                	;Branch to loop if count isn't zero
        tst   TAKE2COMP                 ;if negative, take the negative of VREF
        beq   VREF_CONVERT_SET
        negw  V_REF                     ;take negative of VREF
           
        
VREF_CONVERT_SET:              

        ldaa  #$00                     	;load accumulator A with 00
        puly                           	;pull Y from stack
        pulb                           	;pull B from stack
        
VREF_EXIT:   
        clr   VREF_PROMPT               ;clear VREF prompt, wait for new prompt
        rts
 ;-------------KP_CONV---------------------------------------------------------        
KP_CONVERSION:
        pshb                           	;pushes contents of b to the stack
        pshy                           	;pushes contents of y to the stack
        movw  #LKP_BUF, POINTER       ;move address of buffer into pointer
        movb  DIGIT_COUNT, COUNT       	;sets COUNT to however many digits are loaded
        clrw  KP                      	;clears KP


KP_LOOP:
        ldd   #$0A                     	;loads 10 hex in to accumulator D
        ldy   KP                   	    ;loads KP into accumulator Y
        EMUL                           	;multiplies D and Y and Stores into D and Y
        std   KP                   	    ;stores contents of D into KP

        ldy   POINTER                  	;loads pointer in y  
        ldaa  0,y                      	;loads content of index 0 of y into a
        ldab  #$30                     	;loads 30 hex into acuumulator B
        sba                            	;subtracts B from A and stores result in A
        tab                            	;moves contents of A into B
        clra                           	;all bits in A are cleared
    
        addd  KP			                  ;adds contents of KP to D and stores into D
        std   KP                   	    ;store D in KP
        incw  POINTER                  	;increment POINTER
        
        dec   COUNT                    	;Decrements COUNT
        ldaa  COUNT                    	;If count is 0, conversion is done
        tsta                            ;test if count is 0
        bne   KP_LOOP                 	;Branch to loop if count isn't zero
           
        
KP_CONVERT_SET:              

        ldaa  #$00                     	;load accumulator A with 00
        puly                           	;pull Y from stack
        pulb                           	;pull B from stack
        
KP_EXIT:   
        clr   KP_PROMPT                 ;clear KP prompt, wait for new prompt
        rts                         
 ;-------------KI_CONV---------------------------------------------------------         
KI_CONVERSION:
        pshb                           	;pushes contents of b to the stack
        pshy                           	;pushes contents of y to the stack
        movw  #LKI_BUF, POINTER         ;move address of buffer into pointer
        movb  DIGIT_COUNT, COUNT       	;sets COUNT to however many digits are loaded
        clrw  KI                   	    ;clears KI


KI_LOOP:
        ldd   #$0A                     	;loads 10 hex in to accumulator D
        ldy   KI                   	    ;loads KI into accumulator Y
        EMUL                           	;multiplies D and Y and Stores into D and Y
        std   KI                   	    ;stores contents of D into KI

        ldy   POINTER                  	;loads pointer in y  
        ldaa  0,y                      	;loads content of index 0 of y into a
        ldab  #$30                     	;loads 30 hex into acuumulator B
        sba                            	;subtracts B from A and stores result in A
        tab                            	;moves contents of A into B
        clra                           	;all bits in A are cleared
    
        addd  KI			                  ;adds contents of KI to D and stores into D
        std   KI                   	    ;store D in KI
        incw  POINTER                  	;increment POINTER
        
        dec   COUNT                    	;Decrements COUNT
        ldaa  COUNT                    	;If count is 0, conversion is done
        tsta
        bne   KI_LOOP                 	;Branch to loop if count isn't zero
           
        
KI_CONVERT_SET:              

        ldaa  #$00                     	;load accumulator A with 00
        puly                           	;pull Y from stack
        pulb                           	;pull B from stack
        tst   OL_CL                     ;test if OL or CL
        bne   KI_EXIT                   ;if CL, exit
        clrw  KI                        ;if OL, clear KI to 0
KI_EXIT:   
        clr   KI_PROMPT                 ;clear KI prompt, wait for new prompt
        rts                      
;-------------NEGATIVE SIGN--------------------------------------------------------- 
NEGATIVE_SIGN:
                
        ldaa  VREF_PROMPT               ;tests if VREF is selected
        lbeq  INVALIDENTRY              ;if not, invalid
        ldaa  DIGIT_COUNT               ;loads digit count
        bne   INVALIDENTRY              ;if not 0, invalid
        ldaa  #$2D                      ;puts a - in a
        movb  #$2D, V_REF_BUF           ;puts a - in the first place in buffer
        staa  KEY_BUF                   ;store negative in KEY_BUF
        movb  #$01, ECHO                ;echo the negative sign
        movb  #$01, TAKE2COMP           ;tells conversion to take the 2's compliment
        clr   KEY_FLG                   ;clear KEY_FLG
        rts          

;-------------DIGIT-----------------------------------------------------------------                
MASTERMIND_DIGIT:

        
        tst   VREF_PROMPT               ;checks if VREF digit
        bne   VREF_DIGITS            	  ;branch if VREF digit
        tst   KP_PROMPT                 ;checks if KP digit
        bne   KP_DIGITS                 ;branch if KP digit
        tst   KI_PROMPT                 ;checks if KI digit
        bne   KI_DIGITS                 ;branch if KI digit
        ldaa  KEY_BUF			              ;loads key to be analyzed
        bra   INVALIDENTRY

;-------------VREF_DIGITS-----------------------------------------------
VREF_DIGITS:
        ldaa  DIGIT_COUNT              	;checks DIGIT_COUNT
        suba  #$03                      ;subtract 3
        tsta                            ;test if 0
        beq   INVALIDENTRY             	;if 3 digits have been entered, invalid
        tst   DIGIT_COUNT               ;test if no digits have been entered
        bne   VREF_CONTD1               ;branch to continue
        tst   TAKE2COMP                 ;tests if twos complement is to be taken
        bne   VREF_CONTD1               ;if not, branch to continue
        movb  #$20,V_REF_BUF           ;move blank space into VREF buffer
        
VREF_CONTD1:
        ldaa  KEY_BUF                  	;loads the digit
        suba  #$30                     	;converts ASCII to hex
        cmpa  #$0A                     	;compares to hex 10
        bge   INVALIDENTRY             	;if greater than 10, invalid
        bra   VREF_CONTD                ;branch to continue
        
        
VREF_CONTD:
        ldx   #V_REF_BUF                ;loads BUFFER
        ldaa  DIGIT_COUNT               ;loads digit count
        inca                          	;increment
        movb  KEY_BUF,a,x              	;loads digit in proper place in BUFFER
        inc   DIGIT_COUNT              	;increments DIGIT_COUNT
        movb  #$01, ECHO               	;tells display to display the digit
        movb  #$00, KEY_FLG            	;clears KEY_FLG to wait for next digit
        rts
INVALIDENTRY:
        movb  #$00, KEY_FLG            	;clears KEY_FLG to wait for valid entry
        rts
;-------------KP_DIGITS-----------------------------------------------        
KP_DIGITS:
        ldaa  DIGIT_COUNT              	;checks DIGIT_COUNT
        suba  #$05                      ;subtract 5
        tsta                            ;test digit count
        beq   INVALIDENTRY             	;if 5 digits have been entered, invalid
        ldaa  KEY_BUF                  	;loads the digit
        suba  #$30                     	;converts ASCII to hex
        cmpa  #$0A                     	;compares to hex 10
        lbge  INVALIDENTRY             	;if greater than 10, invalid
KP_CONTD:
        ldx   #LKP_BUF                  ;loads BUFFER
        ldaa  DIGIT_COUNT              	;loads the current digit count
        movb  KEY_BUF,a,x              	;loads digit in proper place in BUFFER
        inc   DIGIT_COUNT              	;increments DIGIT_COUNT
        movb  #$01, ECHO               	;tells display to display the digit
        movb  #$00, KEY_FLG            	;clears KEY_FLG to wait for next digit
        rts
;-------------KI_DIGITS-----------------------------------------------        
KI_DIGITS:
        ldaa  DIGIT_COUNT              	;checks DIGIT_COUNT
        suba  #$05                      ;subtract 5
        tsta                            ;test if 0
        beq   INVALIDENTRY             	;if 5 digits have been entered, invalid
        ldaa  KEY_BUF                  	;loads the digit
        suba  #$30                     	;converts ASCII to hex
        cmpa  #$0A                     	;compares to hex 10
        lbge  INVALIDENTRY             	;if greater than 10, invalid
KI_CONTD:
        ldx   #LKI_BUF                  ;loads BUFFER
        ldaa  DIGIT_COUNT              	;loads the current digit count
        movb  KEY_BUF,a,x              	;loads digit in proper place in BUFFER
        inc   DIGIT_COUNT              	;increments DIGIT_COUNT
        movb  #$01, ECHO               	;tells display to display the digit
        movb  #$00, KEY_FLG            	;clears KEY_FLG to wait for next digit
        rts

;-------------TOGGLE(A)-----------------------------------------------
TOGGLEA:
        tst   RUN                       ;test RUN
        beq   TURN_A_ON                 ;if off, turn on
        clr   RUN                       ;clear RUN
        movb  #$01,D_STP                ;display STP
        clr   KEY_FLG                   ;clear KEY_FLG
        rts
TURN_A_ON:
        clr   KEY_FLG                   ;clear KEY_FLG
        movb  #$01,RUN                  ;set RUN
        movb  #$01,D_RUN                ;display RUN
        jsr   READ_ENCODER              ;read the encoder
        std   ENCODER_COUNT_OLD         ;move into old encoder count
        rts
;-------------TOGGLE(B)-----------------------------------------------
TOGGLEB:
        tst   D_ON_D_OFF                ;test D_ON_D_OFF
        beq   TURN_B_ON                 ;if off, turn on
        clr   D_ON_D_OFF                ;turn off
        movb  #$01,D_D_OFF              ;display D_OFF
        clr   KEY_FLG                   ;clear KEY_FLG
        rts
TURN_B_ON:
        movb  #$01,D_D_ON               ;display D_ON
        clr   KEY_FLG                   ;clear KEY_FLG
        movb  #$01,D_ON_D_OFF           ;set D_ON_D_OFF
        rts

;-------------PROMPT_VREF(C)----------------------------                
VREF_1:
        tst   VREF_PROMPT               ;test if VREF has already been pressed
        lbne  INVALIDENTRY              ;invalid if pressed already
        tst   KP_PROMPT                 ;test if KP has already been pressed
        lbne  INVALIDENTRY              ;invalid if pressed already
        tst   KI_PROMPT                 ;test if KI has already been pressed
        lbne  INVALIDENTRY              ;invalid if pressed already
        ldx   #V_REF_BUF                ;loads Buffer into accumulator 
        movb  #$20,0,x                 	;set ascii input
        movb  #$20,1,x                 	;set ascii input
        movb  #$20,2,x                 	;set ascii input
        movb  #$20,3,x                 	;set ascii input  
        movb  #$01, VREF_PROMPT         ;sets VREF prompt
        movb  #$01, D_VREF              ;tells display to display VREF prompt
        clr   KEY_FLG            	      ;clear KEY_FLG to wait for digit
        rts        
;-------------PROMPT_KP(D)----------------------------  
KP_1:
        tst   VREF_PROMPT               ;test if VREF has already been pressed
        lbne  INVALIDENTRY              ;invalid if pressed already
        tst   KP_PROMPT                 ;test if KP has already been pressed
        lbne  INVALIDENTRY              ;invalid if pressed already
        tst   KI_PROMPT                 ;test if KI has already been pressed
        lbne  INVALIDENTRY              ;invalid if pressed already
        clr   RUN                       ;turn off motor
        movb  #$01,D_STP                ;display STP
        ldx   #LKP_BUF                  ;loads Buffer into accumulator 
        movb  #$20,0,x                 	;set ascii input
        movb  #$20,1,x                 	;set ascii input
        movb  #$20,2,x                 	;set ascii input
        movb  #$20,3,x                 	;set ascii input
        movb  #$20,4,x                 	;set ascii input
        movb  #$01, KP_PROMPT          	;sets KP_PROMPT
        movb  #$01, D_KP              	;tells display to display the KP prompt
        ;movb  #$01, D_CURSOR        	  ;tells display to put the cursor at NINT=
        clr    KEY_FLG            	    ;clear KEY_FLG to wait for digit
        rts
;-------------PROMPT_VREF(E)----------------------------  
KI_1:
        tst   VREF_PROMPT               ;test if VREF has already been pressed
        lbne  INVALIDENTRY              ;invalid if pressed already
        tst   KP_PROMPT                 ;test if KP has already been pressed
        lbne  INVALIDENTRY              ;invalid if pressed already
        tst   KI_PROMPT                 ;test if KI has already been pressed
        lbne  INVALIDENTRY              ;invalid if pressed already
        clr   RUN                       ;turn off motor
        movb  #$01,D_STP                ;display STP
        ldx   #LKI_BUF                 ;loads Buffer into accumulator 
        movb  #$20,0,x                 	;set ascii input
        movb  #$20,1,x                 	;set ascii input
        movb  #$20,2,x                 	;set ascii input
        movb  #$20,3,x                 	;set ascii input
        movb  #$20,4,x                 	;set ascii input
        movb  #$01, KI_PROMPT           ;sets KI prompt
        movb  #$01, D_KI              	;tells display to display the KI prompt
        clr   KEY_FLG            	      ;clear KEY_FLG to wait for digit
        rts
exitf:
        clr   KEY_FLG                  	;clears KEY_FLG to wait for a valid entry
        rts
        
;-------------TOGGLE(F)-----------------------------------------------
TOGGLEF:
        tst   OL_CL                     ;test OL_CL
        beq   TURN_F_ON                 ;if off, turn on
        clr   OL_CL                     ;turn off
        clrw  KI                        ;clear KI to 0
        movb  #$01,D_OL                 ;display OL
        clr   KEY_FLG                   ;clear KEY_FLG
        rts
TURN_F_ON:                   
        clr   KEY_FLG                   ;clear KEY_FLG
        movb  #$01,OL_CL                ;turn on
        movb  #$01,D_CL                 ;display CL
        rts       
;-----------------------------------------------------------------------------------       
;-------------KEYPAD---------------------------------------------------------

KEYPAD:
        ldaa  KEYPAD_STATE	          	;execute states in order
        beq   KEYPAD_STATE_0
        deca  
        beq   KEYPAD_STATE_1
        deca  
        beq   KEYPAD_STATE_2
        rts
        
        
KEYPAD_STATE_0:  
        jsr   INITKEY		               	;initialize the keypad
        jsr   CURSOR_ON		            	;initialize the cursor
        movb  #$01, KEYPAD_STATE       	;move into state 1
        rts                         

KEYPAD_STATE_1:
        tst   LKEY_FLG		            	;tests if a key has been pressed
        beq   exit		                	;if not, return
        jsr   GETCHAR		              	;gets character if a key has been pressed
        stab  KEY_BUF		              	;stores the key in key_buf to be read by M^2
        movb  #$01, KEY_FLG	           	;raises flag so M^2 knows there is a key availible
        movb  #$02, KEYPAD_STATE      	;move to state 2
        rts

KEYPAD_STATE_2:
        
        tst   KEY_FLG		              	;wait for acknowledgement
        beq   set1		                	;move to state 1 if acknowledged
        rts   
set1:
        movb  #01, KEYPAD_STATE		      ;move to state 1
        rts
exit:
        rts

;-------------DISPLAY--------------------------------------------------------

DISPLAY:
      ldaa    dispstate                 ;move through Display tasks in order
      lbeq    INIT
      deca
      lbeq    dispstate1
      deca
      lbeq    dispstate2
      deca
      lbeq    dispstate3
      deca
      lbeq    dispstate4
      deca
      lbeq    dispstate4a
      deca   
      lbeq    dispstate5a
      deca
      lbeq    dispstate6
      deca    
      lbeq    dispstate6a
      deca    
      lbeq    dispstate7
      deca
      lbeq    dispstate8
      deca
      lbeq    dispstate9
      deca
      lbeq    dispstate10
      deca
      lbeq    dispstate11
      deca
      lbeq    dispstate12
      deca      
      
      rts
INIT:
      movb    #$01, dispstate           ;move into display hub
      rts

;-------------HUB----------------------------------------------------- 
dispstate1:
      tst     BACKSPACEDISP			        ;tests for backspace    
      lbne    dispstate2                ;branches if set
      tst     ECHO                      ;tests if a digit is to be echoed
      lbne    dispstate3                ;branches if set
      tst     D_VREF                    ;tests for VREF prompt
      lbne    dispstate4                ;branches if set
      tst     VREF_CURSOR               ;tests for VREF cursor
      lbne    dispstate4a               ;branches if set
      tst     D_KP                      ;tests for KP prompt
      lbne    dispstate5                ;branches if set
      tst     KP_CURSOR                 ;tests for KP cursor
      lbne    dispstate5a               ;branches if set
      tst     D_KI                      ;tests for KI prompt
      lbne    dispstate6                ;branches if set
      tst     KI_CURSOR                 ;tests for KP cursor
      lbne    dispstate6a               ;branches if set
      tst     D_RUN                     ;tests for RUN
      lbne    dispstate7                ;branches accordingly
      tst     D_STP                     ;tests for STOP
      lbne    dispstate8                ;branches accordingly
      tst     D_CL                      ;tests for CL
      lbne    dispstate9                ;branches accordingly
      tst     D_OL                      ;tests for OL
      lbne    dispstate10               ;branches accordinly
      tst     D_D_ON                    ;tests for D_ON
      lbne    dispstate11               ;branches accordingly
      tst     D_D_OFF                   ;tests for D_OFF
      lbne    dispstate12               ;branches accordingly
      rts
;-------------BACKSPACE-----------------------------------------------
dispstate2:
      jsr     GETADDR                  	;Loads the current LCD address
      deca                              ;decrements LCD address
      jsr     SETADDR                   ;Moves the address back one space
      staa    DPTR                      ;Stores the location to be the next digit address
      ;jsr     putchar_1			
      ldab    #' '                      ;Loads a blank space to clear the digit
      jsr     OUTCHAR                   ;Prints the blank space
      jsr     GETADDR                   ;Repeat the process of moving the address back
      deca
      jsr     SETADDR                   ;Sets the new address for the next digit
      jsr     CURSOR_ON                 ;Sets the cursor at the address
      clr     BACKSPACEDISP             ;Clear message flag
      rts
;-------------ECHO-----------------------------------------------
dispstate3:
      jsr     GETADDR			              ;Loads the current LCD address
      ldab    KEY_BUF                   ;Loads the digit to be displayed
      jsr     OUTCHAR                	  ;Displays the digit at the correct address
      movb    #$01, dispstate           ;Return to hub
      clr     ECHO                      ;Clear message flag
      rts
;-------------Vref PROMPT-----------------------------------------------
dispstate4:
      tst     FIRSTCHAR                 ;Tests if the message is at the beginning
      beq     regular4                	;If not, no need to set an address
      ldaa    #$40                      ;Sets address for fixed message
      ldx     #VREF_MESS               	;Loads VREF message
      jsr     putchar_1                 ;Outputs first character of message
      bra     bottom4                   ;Goes to bottom half of the function
regular4:
      jsr     putchar                   ;Outputs a character from a message
bottom4:
      tst     FIRSTCHAR                 ;Tests first character
      beq     exit4                     ;If 0, then message is not finished
      movb    #$01, dispstate           ;Return to hub
      clr     D_VREF                   	;Clear message flag
      movb    #$01, VREF_CURSOR		      ;displays cursor at VREF prompt
exit4:
      rts
;-------------Vref CURSOR-----------------------------------------------
dispstate4a:
      ldaa    #$49			                ;Loads LCD address to be set by SETADDR
      jsr     SETADDR                   ;Sets address of the digits 
      jsr     CURSOR_ON                 ;Turns on cursor at the address
      movb    #01, dispstate            ;Return to hub
      clr     VREF_CURSOR               ;Clear message flag
      rts

;-------------KP PROMPT-----------------------------------------------
dispstate5:
      tst     FIRSTCHAR                 ;Tests if the message is at the beginning
      beq     regular5                	;If not, no need to set an address
      ldaa    #$48                      ;Sets address for fixed message
      ldx     #FIVEBLANKS               ;clears current value
      jsr     putchar_1                 ;Outputs first character of message
      bra     bottom5                   ;Goes to bottom half of the function
regular5:
      jsr     putchar                   ;Outputs a character from a message
bottom5:
      tst     FIRSTCHAR                 ;Tests first character
      beq     exit5                     ;If 0, then message is not finished
      movb    #$01, dispstate           ;Return to hub
      clr     D_KP                    	;Clear message flag
      movb    #$01, KP_CURSOR		        ;displays cursor at KP
exit5:
      rts
;-------------KP CURSOR-----------------------------------------------
dispstate5a:
      ldaa    #$48			                ;Loads LCD address to be set by SETADDR
      jsr     SETADDR                   ;Sets address of the digits 
      jsr     CURSOR_ON                 ;Turns on cursor at the address
      movb    #01, dispstate            ;Return to hub
      clr     KP_CURSOR                 ;Clear message flag
      rts
 ;-------------KI PROMPT-----------------------------------------------
dispstate6:
      tst     FIRSTCHAR                 ;Tests if the message is at the beginning
      beq     regular6                	;If not, no need to set an address
      ldaa    #$56                      ;Sets address for fixed message
      ldx     #FIVEBLANKS               ;clears current value
      jsr     putchar_1                 ;Outputs first character of message
      bra     bottom6                   ;Goes to bottom half of the function
regular6:
      jsr     putchar                   ;Outputs a character from a message
bottom6:
      tst     FIRSTCHAR                 ;Tests first character
      beq     exit6                     ;If 0, then message is not finished
      movb    #$01, dispstate           ;Return to hub
      clr     D_KI                    	;Clear message flag
      movb    #$01, KI_CURSOR		        ;displays cursor at KI
exit6:
      rts
;-------------KI CURSOR-----------------------------------------------
dispstate6a:
      ldaa    #$56			                ;Loads LCD address to be set by SETADDR
      jsr     SETADDR                   ;Sets address of the digits 
      jsr     CURSOR_ON                 ;Turns on cursor at the address
      movb    #01, dispstate            ;Return to hub
      clr     KI_CURSOR                 ;Clear message flag
      rts
;-------------RUN-----------------------------------------------
dispstate7:
      tst     FIRSTCHAR                 ;Tests if the message is at the beginning
      beq     regular7                	;If not, no need to set an address
      ldaa    #$5C                      ;Sets address for fixed message
      ldx     #RUN_MESS               	;Loads RUN message
      jsr     putchar_1                 ;Outputs first character of message
      bra     bottom7                   ;Goes to bottom half of the function
regular7:
      jsr     putchar                   ;Outputs a character from a message
bottom7:
      tst     FIRSTCHAR                 ;Tests first character
      beq     exit7                     ;If 0, then message is not finished
      movb    #$01, dispstate           ;Return to hub
      clr     D_RUN                   	;Clear message flag
exit7:
      rts
;-------------STP-----------------------------------------------
dispstate8:
      tst     FIRSTCHAR                 ;Tests if the message is at the beginning
      beq     regular8                	;If not, no need to set an address
      ldaa    #$5C                      ;Sets address for fixed message
      ldx     #STP_MESS               	;Loads STP message
      jsr     putchar_1                 ;Outputs first character of message
      bra     bottom8                   ;Goes to bottom half of the function
regular8:
      jsr     putchar                   ;Outputs a character from a message
bottom8:
      tst     FIRSTCHAR                 ;Tests first character
      beq     exit8                     ;If 0, then message is not finished
      movb    #$01, dispstate           ;Return to hub
      tst     KI_PROMPT                 ;test if being reprinted because of a new KI or KP
      bne     RESET_KI_ADDRESS          ;reset address at KI
      tst     KP_PROMPT                 
      bne     RESET_KP_ADDRESS          ;reset address at KP
clear8:
      clr     D_STP                   	;Clear message flag
exit8:
      rts
RESET_KI_ADDRESS:
      movb    #$01,KI_CURSOR            ;reset LCD at KI
      bra     clear8
RESET_KP_ADDRESS:
      movb    #$01,KP_CURSOR            ;reset LCD at KP
      bra     clear8
;-------------CL-----------------------------------------------
dispstate9:
      tst     FIRSTCHAR                 ;Tests if the message is at the beginning
      beq     regular9                	;If not, no need to set an address
      ldaa    #$60                      ;Sets address for fixed message
      ldx     #CL_MESS                	;Loads CL message
      jsr     putchar_1                 ;Outputs first character of message
      bra     bottom9                   ;Goes to bottom half of the function
regular9:
      jsr     putchar                   ;Outputs a character from a message
bottom9:
      tst     FIRSTCHAR                 ;Tests first character
      beq     exit9                     ;If 0, then message is not finished
      movb    #$01, dispstate           ;Return to hub
      clr     D_CL                    	;Clear message flag
exit9:
      rts
;-------------OL-----------------------------------------------
dispstate10:
      tst     FIRSTCHAR                 ;Tests if the message is at the beginning
      beq     regular10                	;If not, no need to set an address
      ldaa    #$60                      ;Sets address for fixed message
      ldx     #OL_MESS               	  ;Loads OL message
      jsr     putchar_1                 ;Outputs first character of message
      bra     bottom10                  ;Goes to bottom half of the function
regular10:
      jsr     putchar                   ;Outputs a character from a message
bottom10:
      tst     FIRSTCHAR                 ;Tests first character
      beq     exit10                    ;If 0, then message is not finished
      movb    #$01, dispstate           ;Return to hub
      clr    D_OL                   	  ;Clear message flag
exit10:
      rts
;-------------D_ON-----------------------------------------------
dispstate11:
      tst     FIRSTCHAR                 ;Tests if the message is at the beginning
      beq     regular11                	;If not, no need to set an address
      ldaa    #$63                      ;Sets address for fixed message
      ldx     #D_ON_MESS               	;Loads D_ON message
      jsr     putchar_1                 ;Outputs first character of message
      bra     bottom11                  ;Goes to bottom half of the function
regular11:
      jsr     putchar                   ;Outputs a character from a message
bottom11:
      tst     FIRSTCHAR                 ;Tests first character
      beq     exit11                    ;If 0, then message is not finished
      movb    #$01, dispstate           ;Return to hub
      clr     D_D_ON                   	;Clear message flag
exit11:
      rts
;-------------D_OFF-----------------------------------------------
dispstate12:
      tst     FIRSTCHAR                 ;Tests if the message is at the beginning
      beq     regular12                	;If not, no need to set an address
      ldaa    #$63                      ;Sets address for fixed message
      ldx     #D_OFF_MESS               ;Loads D_OFF message
      jsr     putchar_1                 ;Outputs first character of message
      bra     bottom12                  ;Goes to bottom half of the function
regular12:
      jsr     putchar                   ;Outputs a character from a message
bottom12:
      tst     FIRSTCHAR                 ;Tests first character
      beq     exit12                    ;If 0, then message is not finished
      movb    #$01, dispstate           ;Return to hub
      clr     D_D_OFF                   ;Clear message flag
exit12:
      rts            
                        
                  
      

;/------------------------------------------------------------------------------------\
;| Subroutines                                                                        |
;\------------------------------------------------------------------------------------/
; General purpose subroutines go here

putchar:
      ldx      DPTR                     ;Loads current display address
      ldab     0,X                 	    ;Loads character unless there is none
      beq      exit1                    ;Branch to exit if message is complete
      jsr      OUTCHAR                  ;Put character on LCD display
      incx                              ;Increment X
      stx      DPTR			                ;Stores next address in DPTR
      rts 
exit1:
  movb #$01, FIRSTCHAR                  ;Sets FIRSTCHAR so message is done printing
  rts
putchar_1:
  stx DPTR                              ;Stores DPTR in x
  jsr SETADDR                           ;Sets the new display address from x
  clr FIRSTCHAR                         ;First character has been printed
  rts


BINARY2ASCII:
         movb    #$20,0,Y               ;move blanks to buffer
         movb    #$20,1,Y                
         movb    #$20,2,Y                
         movb    #$20,3,Y                
         
         movb #01,COUNT
         std  TEMP1                     ;stores into TEMP1
         blt  subloadneg                ;branch if negative
         movb #$20,0,Y                  ;if positive, move a blank space into the first buffer space
         incy                           ;moves to the first digit place in buffer
             
subloop:         
         ldd  TEMP1                     ;loads number or quotient
         ldx  #$0A                      ;loads 10 into X
         IDIV                           ;divide the number by 10
         addd #$30                      ;add 30 to remainder, converts to ASCII
         pshb                           ;push the digit onto the stack            
         tstx                           ;tests the quotient
         beq  subfill                   ;if 0, conversion is done
         stx  TEMP1                     ;stores quotient into TEMP1
         inc  COUNT                     ;increment count
         bra  subloop                   ;branch to continue subroutine

subfill:    
         pulb                           ;pull digit from stack
         stab 0,Y                       ;store in buffer location defined by Y
         incy                           ;advance to next location
         dec  COUNT                     ;decrease count
         tst  COUNT                     ;if COUNT is 0, all digits have been loaded
         bne  subfill                   ;continues buffer filling loop
         rts                            ;return        
          
subloadneg:
         movb #$2D,0,Y                  ;moves negative sign into first buffer address
         negw TEMP1                     ;take two's comp of the number
         incy                           ;increment address
         bra  subloop                   ;branch back to the conversion

interrupt:                            	;interrupt subroutine
        tst     D_ON_D_OFF              ;tests if display should be updating
        beq     NOUPDATE                ;do not update display count
        dec     UPDATECOUNT             ;decrement count
        tst     UPDATECOUNT             ;test if 0
        bne     NOUPDATE                ;if not, branch to NOUPDATE
        movb    #$01,UPDATEDISPLAY      ;tell M^2 to update display
        movb    #255,UPDATECOUNT        ;reset count
NOUPDATE:        
        tst     RUN                     ;skip to interrupt if run is cleared, indicating new wave 
        lbeq    NOT_YET                 ;branch to interrupt        
        jsr     READ_ENCODER            ;read the encoder
        std     ENCODER_COUNT           ;store in new encoder count
        ldd     ENCODER_COUNT
        subd    ENCODER_COUNT_OLD       ;subtract old from new
        std     V_ACT                   ;store in VACT
        ldy     #13                     ;increase resolution for DAC
        EMUL
        addd    #2048
        jsr     OUTDACA                 ;OUTDAC to oscilloscope
        movw    ENCODER_COUNT, ENCODER_COUNT_OLD
        tst     OL_CL                   ;test if OL or CL
        bne     CLOSED_LOOP             ;branch if set
        ldd     V_REF                   ;load VREF into D
        std     ERR                     ;store in ERR
        bra     next_one                ;branch accordingly
CLOSED_LOOP:        
        ldd     V_REF                   ;loads Vref (setpoint)
        subd    V_ACT                   ;subtracts
        std     ERR                     ;stores Error Value
next_one:
        ldd     ERR                     ;load error into D
        ldy     KP                      ;load 1024*Kp
        EMULS                           ;multiply error by Vref
        ldx     #1024                   ;loads 1024 to x
        EDIVS                           ;divide error by 1024
        sty     KP_ERR                  ;store value in KP_ERROR
        bvs     SATURATION              ;branch if overflowed
        bra     Integral_Calc           ;branch to integral sum
SATURATION:
        tstw    ERR                     ;test error
        ble     Negative                ;branch if negative
        movw    #$7FFF, KP_ERR          ;saturate KP
        bra     Integral_Calc           ;branch to integral calc
Negative:
        movw    #$8000, KP_ERR          ;saturate KP
Integral_Calc:
        ldd     ESUM_OLD                ;load old ESUM
        ldy     ERR                     ;load error into Y
        pshy                            ;push onto the stack
        addd    0, SP                   ;add contents of stack to D
        bvc     done                    ;branch if no carry
        std     ESUM                    ;store result in ESUM
        tst     0, SP                   ;test contents of stack
        bpl     pos                     ;branch if positive
        ldd     #$8000                  ;saturate with $8000
        std     ESUM                    ;store into ESUM
        bra     done                    ;branch to DONE
pos:    
        ldd     #$7FFF                  ;saturate with $7FFF
        std     ESUM                    ;store in ESUM
done:   
        std     ESUM                    ;store ESUM in D
        movw    ESUM, ESUM_OLD          ;move ESUM to old ESUM
        puly                            ;pull from the stack
        ldd     ESUM                    ;load ESUM
        ldy     KI                      ;load KI into Y
        EMULS                           ;multiply
        ldx     #1024
        EDIVS                           ;divide
        sty     KI_ERR                  ;store in KI error
        bvs     SATURATION_1            ;if overflowed, saturate
        bra     FINAL_ADD               ;branch to final addition
SATURATION_1:
        tstw    ESUM                    ;test ESUM
        ble     Negative_1              ;branch if negative
        movw    #$8000, KI_ERR          ;saturate with $8000
        bra     FINAL_ADD               ;branch to final SDBA     
Negative_1:
        movw    #$7FFF, KI_ERR          ;saturate with $7FFF
FINAL_ADD:          
        ldy     KI_ERR                  ;load KI
        ldd     KP_ERR                  ;load KP
        pshy                            ;push KI
        addd    0, SP                   ;add to KP
        bvc     done_1                  ;branch if carry is cleared
        std     A_INIT                  ;store value in a
        tst     0, SP                   ;test KI
        bpl     pos_1                   ;branch if positive
        ldd     #$8000                  ;saturate with $8000
        std     A_INIT                  ;store in a
        bra     done_1                  ;branch to done_1
pos_1:
        ldd     #$7FFF                  ;saturate with $7FFF
        std     A_INIT                  ;store in a
done_1:
        std     A_INIT                  ;store ina
        puly                            ;pull from KI, store in Y
        ldd     A_INIT                  ;load a
        cpd     #625                    ;compare to 625
        bge     SAT_HIGH                ;if higher, saturate high
        ldd     A_INIT
        cpd     #-625                   ;compare to -625
        ble     SAT_LOW                 ;if lower, saturate low
        std     A_STAR                  ;store into A_STAR
        bra     A_STAR_SET              ;branch to A_STAR_SET
SAT_HIGH:
        movw    #625, A_STAR            ;saturate high
        bra     A_STAR_SET
SAT_LOW:
        movw    #-625, A_STAR           ;saturate low
        bra     A_STAR_SET 
A_STAR_SET:
        ldy     A_STAR                  ;load A_STAR into Y
        ldd     #100                    ;load 100
        EMULS                           ;multiply
        ldx     #625                    ;load 625
        EDIVS                           ;divide
        sty     EFFORT                  ;store the result in EFFORT
        tstw    EFFORT                  ;test EFFOT
        bra     EFFORT_SET
EFFORT_SET:
        ldd     A_STAR                  ;update the motor             
        jsr     UPDATE_MOTOR
INTER:       
        ldd     TC0                   	;Stores Timer Channel 0 Register in D
        addd    INTERVAL              	;adds contents of interval to contents of D
        std     TC0                   	;stores contents of d in timer channel 0 register
        bset    TFLG1, $01            	;clears C0F; 
        rti
NOT_YET:
        jsr     READ_ENCODER            ;read encoder
        std     ENCODER_COUNT           ;store in encoder count
        ldd     ENCODER_COUNT           ;load encoder count
        subd    ENCODER_COUNT_OLD       ;subtract old
        std     V_ACT                   ;store D in VACT
        movw    ENCODER_COUNT, ENCODER_COUNT_OLD
        ldd     V_REF                   ;loads Vref (setpoint)
        subd    V_ACT                   ;subtracts
        std     ERR                     ;stores Error Value
        movw    #$0000, EFFORT
        ldd     #$0000             
        jsr     UPDATE_MOTOR
        ldd     TC0                   	;Stores Timer Channel 0 Register in D
        addd    INTERVAL              	;adds contents of interval to contents of D
        std     TC0                   	;stores contents of d in timer channel 0 register
        bset    TFLG1, $01            	;clears C0F;       
        rti          

;/------------------------------------------------------------------------------------\
;| ASCII Messages and Constant Data                                                   |
;\------------------------------------------------------------------------------------/
; Any constants can be defined here

MESSAGE: 
        VREF_MESS:    DC.B  'new_Vref=                               ',$00
        FIVEBLANKS:   DC.B  '     ',$00
        RUN_MESS:     DC.B  'RUN',$00
        STP_MESS:     DC.B  'STP',$00
        OL_MESS:      DC.B  'OL',$00
        CL_MESS:      DC.B  'CL',$00
        D_ON_MESS:    DC.B  'D_ON ',$00
        D_OFF_MESS:   DC.B  'D_OFF',$00                  


;/------------------------------------------------------------------------------------\
;| Vectors                                                                            |
;\------------------------------------------------------------------------------------/
; Add interrupt and reset vectors here

        ORG   $FFFE                    ; reset vector address
        DC.W  Entry
        ORG   $FFEE                    ; Key Wakeup interrupt vector address [Port J]
        DC.W  interrupt
        ORG   $FFCE                    ; Key Wakeup interrupt vector address [Port J]
        DC.W  ISR_KEYPAD
