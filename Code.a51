




;Input: L VALUE in 31H RAM and W in 32H RAM of area , L VALUE in 51H RAM and W in 52H RAM of CIRCUMFERENCE
;Output: A VALUE STORAGE IN 41H RAM AND 42H RAM, C VALUE STORAGE IN 61H RAM AND 62H RAM
	

;Input: selection number
;output; one of 3 operations (rectangle,triangle,exit)


; Input:
;		Base value will be stored in 35H RAM and Height VALUE in 36H RAM for Triangle's Area Calculations.
;		Base value will be stored in 55H RAM, Height VALUE in 56H RAM, and Hypotenuse VALUE in 57H RAM for Triangle's Perimeter Calculations.

; Output:
;		The Result of Area calculations will be stored in 45H and 46H RAM locations.
;		The Result of Perimeter calculation will be stored in 65H and 66H RAM locations.








;Take selection input from user
;Temporary input until Key pad is implemnted

R    BIT    P1.3
	
LDATA EQU P0
EN EQU P0.2
RS EQU P0.0
RW EQU P0.1
	
ORG 0000H
LJMP BEGIN_PROGRAM

ORG 0003H ; EX0 interrupt vector address 
LJMP Next1433

BEGIN_PROGRAM:
		
		ACALL INLCD 


	MOV A,#80H
	ACALL CMD
	
; LINES FROM HERE TO 165 IS WRITING THE INITAL MESSAGES ON LCD (WELCOME,NAMES AND SELECTIONS)
welcome_lcd:
			MOV R0,#3
			MOV DPTR,#STR1 ;WRITE WELCOME ON LCD
	
			
		    ACALL WSTR
AGAIN124:	ACALL DELAY
			DJNZ R0,AGAIN124
			
			MOV A,#0 
			ACALL CMD
			MOV A,#0001B 
			ACALL CMD
			ACALL LDELAY ; Clear Display - LCD -
			
			

			MOV R0,#3
			MOV DPTR,#NAME1 ;WRITE NAME HAMAD AND ID ON LCD
	
			
		    ACALL WSTR
AGAIN125:	ACALL DELAY
			DJNZ R0,AGAIN125
			
			MOV A,#0 
			ACALL CMD
			MOV A,#0001B 
			ACALL CMD
			ACALL LDELAY ; Clear Display - LCD 
		
;-------------------------------------
			MOV R0,#3
			MOV DPTR,#NAME2 ;WRITE NAME NAWAF AND ID ON LCD
	
			
		    ACALL WSTR
AGAIN126:	ACALL DELAY
			DJNZ R0,AGAIN126
			
			MOV A,#0 
			ACALL CMD
			MOV A,#0001B 
			ACALL CMD
			ACALL LDELAY ; Clear Display - LCD 
		
;-------------------------------------
		

	
			MOV R0,#3
			MOV DPTR,#NAME3 ;WRITE NAME FAHAD AND ID ON LCD
	
			
		    ACALL WSTR
AGAIN127:	ACALL DELAY
			DJNZ R0,AGAIN127
			
			MOV A,#0 
			ACALL CMD
			MOV A,#0001B 
			ACALL CMD
			ACALL LDELAY ; Clear Display - LCD 
			
			
			MOV DPTR,#INTR_MSG
			ACALL WSTR
            MOV IE,#10000001B
			SETB IT0 	
			
			AJMP $
;-------------------------------------
		
;INT---------------------------------------
;-------------------------------------
		

Next1433:	
Next1434:
			

			MOV A,#0 
			ACALL CMD
			MOV A,#0001B 
			ACALL CMD
			ACALL LDELAY ; Clear Display - LCD 

			MOV R0,#3
			MOV DPTR,#S12 ;WRITE 1-RECTANGLE 2-TRIANGLE ON LCD
	
			
		    ACALL WSTR

			
			MOV A,#0C0H
			ACALL CMD
			
	
			MOV R0,#3
			MOV DPTR,#S3 ;WRITE 3- EXIT ON LCD
			ACALL WSTR
			
			AGAIN1227:	ACALL DELAY
			DJNZ R0,AGAIN1227
		
			

			
	
			
			MOV A,#0 
			ACALL CMD
			MOV A,#0001B 
			ACALL CMD
			ACALL LDELAY ; Clear Display - LCD 

ENTER_INPUT:
	
			MOV R0,#3
			MOV DPTR,#ENTER ;WRITE ENTER A SELECTION ON LCD
			
			ACALL WSTR
			
		
			ACALL KEYPAD
			ACALL WCHR
			MOV R5,A

			ACALL DELAY
		
			
			MOV A,#0 
			ACALL CMD
			MOV A,#0001B 
			ACALL CMD
			ACALL LDELAY ; Clear Display - LCD 
			
;-------------------------------------
		

	

					
Main:


       ;The input will be taken
       ;from the Keypad but for
		;Now we will assume its R0
		
				
		;ACCEPTED ASCII VALUES:
		
				;IF THE HIGHT BYTE IS 3 WE ACCEPT IT
		


;THE SELECTION
;------------------------


;MOV R0,#'2' ;INPUT YOUR SELECTION HERE
;-------------------------

;ASCII CHECK
MOV A,R5 ;
SWAP A  ;SWAP THE ASCII CODE TO CHECK IF HIGH BYTE = 3
ANL A,#00001111B ;MASK OUT TO CHECK THE VALUE OF THE HIGH BYTE 
CJNE A,#3,Checkpoint ;IF IT IS NOT 3 IT IS NOT A NUMBER THEREFORE JUMP TO ERROR

SJMP Selection1

Checkpoint:
	LJMP Otherwise

Selection1:	CJNE R5,#'1',CHECKPOINT_SS1
			LJMP RECT_LENGTH
			CHECKPOINT_SS1:
			LJMP SELECTION2
			
			;ENTER LENGTH -> CHECK IF NUMBER THEN STORE IT 
			;ENTER WIDTH -> CHECK IF NUMBER THEN STORE IT

RECT_LENGTH:	
			MOV DPTR,#rec_length ;WRITE ENTER A SELECTION ON LCD
	
			
		
		    ACALL WSTR
			ACALL KEYPAD
			ACALL WCHR
			
			MOV B,A ;39H
			
			SWAP A  ;SWAP THE ASCII CODE TO CHECK IF HIGH BYTE = 3
			ANL A,#00001111B ;MASK OUT TO CHECK THE VALUE OF THE HIGH BYTE 
			CJNE A,#3,Checkpoint2 ;IF IT IS NOT 3 IT IS NOT A NUMBER THEREFORE JUMP TO ERROR
			
		
				
			MOV A,B
			SUBB A,#30H ;09h
			MOV B,#10
			MUL AB ;90H
			MOV R7,A ;NOW R7 HAS THE FIRST DIGIT
			
		
			ACALL KEYPAD
			ACALL WCHR
			ACALL DELAY
			MOV B,A
			MOV A,B
			
			SUBB A,#30H
			ADD A,R7 ;97
			MOV 76H,A ;76H LENGTH
			MOV A,B
			SWAP A  ;SWAP THE ASCII CODE TO CHECK IF HIGH BYTE = 3
			ANL A,#00001111B ;MASK OUT TO CHECK THE VALUE OF THE HIGH BYTE 
			CJNE A,#3,Checkpoint2 ;IF IT IS NOT 3 IT IS NOT A NUMBER THEREFORE JUMP TO ERROR
			SJMP GO_NEXT

			Checkpoint2:
				ACALL ERROR_MSG
				LJMP RECT_LENGTH
			
			GO_NEXT:
			MOV A,#0 
			ACALL CMD
			MOV A,#0001B 
			ACALL CMD
			ACALL LDELAY ; Clear Display - LCD
RECT_WIDTH:		
			;ENTER WIDTH -> CHECK IF NUMBER THEN STORE IT
			MOV DPTR,#rec_width ;WRITE ENTER A SELECTION ON LCD
	
			
		    ACALL WSTR
			ACALL KEYPAD
			ACALL WCHR
			
			MOV B,A
			
			SWAP A  ;SWAP THE ASCII CODE TO CHECK IF HIGH BYTE = 3
			ANL A,#00001111B ;MASK OUT TO CHECK THE VALUE OF THE HIGH BYTE 
			CJNE A,#3,Checkpoint3 ;IF IT IS NOT 3 IT IS NOT A NUMBER THEREFORE JUMP TO ERROR
			
			MOV A,B
			SUBB A,#30H ;A = 09
			MOV B,#10 ;SWAP A = 90
			MUL AB
			MOV R7,A ;NOW R7 HAS THE FIRST DIGIT
			
			ACALL KEYPAD
			
			ACALL WCHR
			ACALL DELAY
			MOV B,A
			SUBB A,#30H
			ADD A,R7
			MOV 77H,A ;77H : WIDTH
			MOV A,B
			SWAP A  ;SWAP THE ASCII CODE TO CHECK IF HIGH BYTE = 3
			ANL A,#00001111B ;MASK OUT TO CHECK THE VALUE OF THE HIGH BYTE 
			CJNE A,#3,checkpoint3 ;IF IT IS NOT 3 IT IS NOT A NUMBER THEREFORE JUMP TO ERROR
			SJMP GO_NEXT1

			Checkpoint3:
				ACALL ERROR_MSG
				LJMP RECT_WIDTH
			
			GO_NEXT1:
			MOV A,#0 
			ACALL CMD
			MOV A,#0001B 
			ACALL CMD
			ACALL LDELAY ; Clear Display - LCD 
			
			
			ACALL RECTANGLE_CALC ; GO TO LABEL RECTANGLE_CALC TO CALCULATE RECTANGLE
			
			




Selection2: CJNE R5,#'2',checkpoint_s3
			
			sjmp cont_sel1
			
			checkpoint_s3:
			LJMP Selection3
			
		
		
			
			
			
			cont_sel1:
TRIG_BASE:				
			clr a
		
			MOV DPTR,#tri_base ;WRITE ENTER A SELECTION ON LCD
	
			
		
		    LCALL WSTR
			
		    ACALL WSTR
			ACALL KEYPAD
			ACALL WCHR
			
			MOV B,A ;39H
			
			SWAP A  ;SWAP THE ASCII CODE TO CHECK IF HIGH BYTE = 3
			ANL A,#00001111B ;MASK OUT TO CHECK THE VALUE OF THE HIGH BYTE 
			CJNE A,#3,Checkpoint4 ;IF IT IS NOT 3 IT IS NOT A NUMBER THEREFORE JUMP TO ERROR
			
			MOV A,B
			SUBB A,#30H ;09h
			MOV B,#10
			MUL AB ;90H
			MOV R7,A ;NOW R7 HAS THE FIRST DIGIT
			
			
		
			ACALL KEYPAD
			ACALL WCHR
			ACALL DELAY
			MOV B,A
			MOV A,B
			
			SUBB A,#30H
			ADD A,R7 ;97
			MOV 76H,A ;76H LENGTH
			MOV A,B
			SWAP A  ;SWAP THE ASCII CODE TO CHECK IF HIGH BYTE = 3
			ANL A,#00001111B ;MASK OUT TO CHECK THE VALUE OF THE HIGH BYTE 
			CJNE A,#3,Checkpoint4 ;IF IT IS NOT 3 IT IS NOT A NUMBER THEREFORE JUMP TO ERROR
			SJMP GO_NEXT2

			Checkpoint4:
				ACALL ERROR_MSG
				LJMP TRIG_BASE
		
			
			GO_NEXT2:
			
			MOV A,#0 
			LCALL CMD
			MOV A,#0001B 
			LCALL CMD
			LCALL LDELAY ; Clear Display - LCD
			
TRIG_HEIGHT:			
			clr a			
			MOV DPTR,#tri_height ;WRITE ENTER A SELECTION ON LCD
		    LCALL WSTR
			ACALL KEYPAD
			ACALL WCHR
			
			MOV B,A
			
			SWAP A  ;SWAP THE ASCII CODE TO CHECK IF HIGH BYTE = 3
			ANL A,#00001111B ;MASK OUT TO CHECK THE VALUE OF THE HIGH BYTE 
			CJNE A,#3,Checkpoint5 ;IF IT IS NOT 3 IT IS NOT A NUMBER THEREFORE JUMP TO ERROR
			
			MOV A,B
			SUBB A,#30H ;A = 09
			MOV B,#10 ;SWAP A = 90
			MUL AB
			MOV R7,A ;NOW R7 HAS THE FIRST DIGIT
			
			ACALL KEYPAD
			
			ACALL WCHR
			ACALL DELAY
			MOV B,A
			SUBB A,#30H
			ADD A,R7
			MOV 77H,A ;77H : WIDTH
			MOV A,B
			SWAP A  ;SWAP THE ASCII CODE TO CHECK IF HIGH BYTE = 3
			ANL A,#00001111B ;MASK OUT TO CHECK THE VALUE OF THE HIGH BYTE 
			CJNE A,#3,checkpoint5 ;IF IT IS NOT 3 IT IS NOT A NUMBER THEREFORE JUMP TO ERROR
			SJMP GO_NEXT33

			Checkpoint5:
				ACALL ERROR_MSG
				LJMP TRIG_HEIGHT
			
			GO_NEXT33:
			
			MOV A,#0 
			LCALL CMD
			MOV A,#0001B 
			LCALL CMD
			LCALL LDELAY ; Clear Display - LCD
			
TRIG_A:			
			MOV DPTR,#tri_a ;WRITE ENTER A SELECTION ON LCD
	
			
		
			ACALL WSTR
			ACALL KEYPAD
			ACALL WCHR
			
			MOV B,A 
			
			SWAP A  ;SWAP THE ASCII CODE TO CHECK IF HIGH BYTE = 3
			ANL A,#00001111B ;MASK OUT TO CHECK THE VALUE OF THE HIGH BYTE 
			CJNE A,#3,Checkpoint6 ;IF IT IS NOT 3 IT IS NOT A NUMBER THEREFORE JUMP TO ERROR
			
			MOV A,B
			SUBB A,#30H ;09h
			MOV B,#10
			MUL AB ;90H
			MOV R7,A ;NOW R7 HAS THE FIRST DIGIT
			
			ACALL KEYPAD
			ACALL WCHR
			ACALL DELAY
			MOV B,A
			
			SUBB A,#30H
			ADD A,R7
			MOV 47H,A ;90H HAS SIDE A
			MOV A,B
			SWAP A  ;SWAP THE ASCII CODE TO CHECK IF HIGH BYTE = 3
			ANL A,#00001111B ;MASK OUT TO CHECK THE VALUE OF THE HIGH BYTE 
			CJNE A,#3,checkpoint6 ;IF IT IS NOT 3 IT IS NOT A NUMBER THEREFORE JUMP TO ERROR
			LJMP GO_NEXT4

			Checkpoint6:
				ACALL ERROR_MSG
				LJMP TRIG_A
			
			GO_NEXT4:
			MOV A,#0 
			ACALL CMD
			MOV A,#0001B 
			ACALL CMD
			ACALL LDELAY ; Clear Display - LCD


TRIG_B:					
			MOV DPTR,#tri_b ;WRITE ENTER A SELECTION ON LCD

			
		
		    ACALL WSTR
			ACALL KEYPAD
			ACALL WCHR
			
			MOV B,A 
			
			SWAP A  ;SWAP THE ASCII CODE TO CHECK IF HIGH BYTE = 3
			ANL A,#00001111B ;MASK OUT TO CHECK THE VALUE OF THE HIGH BYTE 
			CJNE A,#3,Checkpoint7 ;IF IT IS NOT 3 IT IS NOT A NUMBER THEREFORE JUMP TO ERROR
			
			MOV A,B
			SUBB A,#30H ;09h
			MOV B,#10
			MUL AB ;90H
			MOV R7,A ;NOW R7 HAS THE FIRST DIGIT
			
			ACALL KEYPAD
			ACALL WCHR
			ACALL DELAY
			MOV B,A
			
			SUBB A,#30H
			ADD A,R7
			MOV 48H,A ;91H HAS SIDE B
			MOV A,B
			SWAP A  ;SWAP THE ASCII CODE TO CHECK IF HIGH BYTE = 3
			ANL A,#00001111B ;MASK OUT TO CHECK THE VALUE OF THE HIGH BYTE 
			CJNE A,#3,checkpoint7 ;IF IT IS NOT 3 IT IS NOT A NUMBER THEREFORE JUMP TO ERROR
			LJMP GO_NEXT5

			Checkpoint7:
				ACALL ERROR_MSG
				LJMP TRIG_B
			
			GO_NEXT5:
			MOV A,#0 
			ACALL CMD
			MOV A,#0001B 
			ACALL CMD
			ACALL LDELAY ; Clear Display - LCD
			
			
TRIG_C:						
			MOV DPTR,#tri_c ;WRITE ENTER A SELECTION ON LCD

			
		
		    ACALL WSTR
			ACALL KEYPAD
			ACALL WCHR
			
			MOV B,A 
			
			SWAP A  ;SWAP THE ASCII CODE TO CHECK IF HIGH BYTE = 3
			ANL A,#00001111B ;MASK OUT TO CHECK THE VALUE OF THE HIGH BYTE 
			CJNE A,#3,Checkpoint8 ;IF IT IS NOT 3 IT IS NOT A NUMBER THEREFORE JUMP TO ERROR
			
			MOV A,B
			SUBB A,#30H ;09h
			MOV B,#10
			MUL AB ;90H
			MOV R7,A ;NOW R7 HAS THE FIRST DIGIT
			
			ACALL KEYPAD
			ACALL WCHR
			ACALL DELAY
			MOV B,A
			
			SUBB A,#30H
			ADD A,R7
			MOV 49H,A ;92H HAS SIDE C
			MOV A,B
			SWAP A  ;SWAP THE ASCII CODE TO CHECK IF HIGH BYTE = 3
			ANL A,#00001111B ;MASK OUT TO CHECK THE VALUE OF THE HIGH BYTE 
			CJNE A,#3,checkpoint8 ;IF IT IS NOT 3 IT IS NOT A NUMBER THEREFORE JUMP TO ERROR
			SJMP GO_NEXT6

			Checkpoint8:
				ACALL ERROR_MSG
				LJMP TRIG_C
				
			
			GO_NEXT6:
			MOV A,#0 
			ACALL CMD
			MOV A,#0001B 
			ACALL CMD
			ACALL LDELAY ; Clear Display - LCD



			ACALL TRIANGLE_CALC ; GO TO LABEL TRIANGLE_CALC TO CALCULATE TRIANGLE
		
			LJMP Next1433
				




Selection3: CJNE R5,#'3',Otherwise
		
		END_MSGG:		
			MOV R0,#3
			MOV DPTR,#END_MSG ;WRITE PROGRAM END ON LCD
	
			
		    ACALL WSTR
AGAIN147:	ACALL DELAY
			DJNZ R0,AGAIN147
			
			MOV A,#0 
			ACALL CMD
			MOV A,#0001B 
			ACALL CMD
			ACALL LDELAY ; Clear Display - LCD 
			
		

;--- Show END message and end the program------

	DONE3:
		SJMP DONE3

Otherwise: 

;Show an error meassage for a period of time then
;Jump back to main, restarting the program
	
		ACALL ERROR_MSG
		LJMP ENTER_INPUT






ERROR_MSG:
			MOV A,#0 
			ACALL CMD
			MOV A,#0001B 
			ACALL CMD
			ACALL LDELAY ; Clear Display - LCD

			MOV DPTR,#ER_MSG ;WRITE ERROR WRONG INPUT ON LCD
	
			
		    ACALL WSTR
			ACALL DELAY
			
		
		
		
Flash_Red:

	CLR R
	ACALL DELAY
	SETB R
	ACALL DELAY
	CLR R
	ACALL DELAY
	SETB R
	ACALL DELAY
	CLR R
	ACALL DELAY
	SETB R
	ACALL DELAY
	
		MOV A,#0 
		ACALL CMD
		MOV A,#0001B 
		ACALL CMD
		ACALL LDELAY ; Clear Display - LCD
	RET
	 ;for now the program ends instead of restarting until keypad is implemented
;--------------------------------------------------------------------------		
KEYPAD:
	MOV P2,#11110000B 		;make bits P2.4-P2.7 input (columns)

K1: MOV P2,#11110000B   		;ground all rows
	MOV A,P2				;read all col. (ensure all keys open)
	ANL A,#11110000B   		;masked unused bits
	CJNE A,#11110000B,K1 		;check till all keys released

K2:	ACALL TMSEC       
	MOV A,P2				;see if any key is pressed
	ANL A,#11110000B			;mask unused bits
	CJNE A,#11110000B,OVER   	;key pressed, await closure
	SJMP K2				;check if key pressed

OVER: 
	ACALL TMSEC
	MOV A,P2				;check key closure
	ANL A,#11110000B			;mask unused bits
	CJNE A,#11110000B,OVER1  	;key pressed, find row
	SJMP K2				;if none keep polling

OVER1: 
	MOV P2,#11111110B		;ground row 0
	MOV A,P2				;read all columns
	ANL A,#11110000B			;mask unused bits
	CJNE A,#11110000B,ROW_0  	;key row 0, find the col.

	MOV P2,#11111101B		;ground row 1
	MOV A,P2				;read all columns
	ANL A,#11110000B			;mask unused bits 
	CJNE A,#11110000B,ROW_1  	;key row 1, find the col.

	MOV P2,#11111011B		;ground row 2
	MOV A,P2				;read all columns
	ANL A,#11110000B       		;mask unused bits
	CJNE A,#11110000B,ROW_2	;key row 2, find the col.

	MOV P2,#11110111B        		;ground row 3
	MOV A,P2				;read all columns
	ANL A,#11110000B			;mask unused bits
	CJNE A,#11110000B,ROW_3  	;key row 3, find the col.

	LJMP K2				;if none, false input, repeat

ROW_0: MOV DPTR,#KCODE0		;set DPTR=start of row 0        
	SJMP FIND				;find col. key belongs to

ROW_1: MOV DPTR,#KCODE1		;set DPTR=start of row 1
	SJMP FIND				;find col. key belongs to

ROW_2: MOV DPTR,#KCODE2		;set DPTR=start of row 2 
	SJMP FIND				;find col. key belongs to

ROW_3: MOV DPTR,#KCODE3		;set DPTR=start of row 3

FIND:  
	SWAP A				;exchange low and high nibble				

FIND1:
	RRC A					;see if any CY bit low
	JNC MATCH				;if zero, get the ASCII code       
	INC DPTR                 			;point to next col. address
	SJMP FIND1               		;keep searching

MATCH: 
	CLR A                    			;set A=0 (match is found)
	MOVC A,@A+DPTR           		;get ASCII code from table
	RET

; 30 msec delay
TMSEC:	
	MOV   TMOD,#00000001B   
	MOV   TL0,#0CAH         		; Timer0, 30msec delay
	MOV   TH0,#27H    	
	SETB  TR0      		
BACK: 
	JNB   	TF0,BACK    	
	CLR   	TR0         	
	CLR   	TF0        	
	RET   

	
KCODE0: DB '1','2','3','A' 		;Row0 ASCII codes
KCODE1:	DB '4','5','6','B' 		;Row1
KCODE2:	DB '7','8','9','C' 		;Row2
KCODE3:	DB '*','0','#','D' 		;Row3

;---------------------------------------------------------------------------
RECTANGLE_CALC:
	

 

 

MOV A,76H
MOV B,77H

 

MOV R0,A ; SAVE VALUE L TO USE IT FOR AREA CALCULATION
MOV R1,B ; SAVE VALUE W TO USE IT FOR AREA CALCULATION

 

MOV B,#10
DIV AB
SWAP A
ADD A,B ; CONVERT VAULE L TO BCD
MOV 31H,A ; SAVE L IN 31H RAM

 

MOV A,R1
MOV B,#10
DIV AB
SWAP A
ADD A,B ; CONVERT VAULE W TO BCD
MOV 32H,A ; SAVE W IN 32H RAM

 

MOV A,R0
MOV B,R1

 


; AREA CALCULATION OF RECTANGLE
MUL AB ; A= L * W
ACALL BCD ; CONVERT THE RESULT FROM HEX TO BCD

 

MOV A,R6
SWAP A
ADD A,R5
MOV R0,A
MOV A,R4
SWAP A
ADD A,R3
MOV B,R0 ; CONVERT THE LOWER AMD UPPER BYTE OF RESULT TO BCD

 

MOV 42H,A ; SAVE THE LOWER BYTE OF AREA IN 42H
MOV 41H,B ; ;SAVE THE UPPER BYTE OF AREA IN 41H

 

LJMP NEXT1 ; SKIP AND JUMP TO CIRCUMFERENCE CALCULATION OF RECTANGLE

 

BCD:
    ;i nput hex number stored in r2 (LSB), and R1 (MSB).
    ; output bcd number stored in R3, R4, R5, R6

 

mov r2,A  ;lsb
mov r1,B    ;msb

 

Hex2BCD:

	


        MOV R3,#0
        MOV R4,#0
        MOV R5,#0
        MOV R6,#0
        MOV R7,#0

 

 

        MOV B,#10
        MOV A,R2
        DIV AB
        MOV R3,B ; 
        MOV B,#10 ; R7,R6,R5,R4,R3
        DIV AB
        MOV R4,B
        MOV R5,A
        CJNE R1,#0H,HIGH_BYTE ; CHECK FOR HIGH BYTE
		
	
        SJMP FINISH

 

 

HIGH_BYTE:
        MOV A,#6
        ADD A,R3
        MOV B,#10
        DIV AB
        MOV R3,B
        ADD A,#5
        ADD A,R4
        MOV B,#10
        DIV AB
        MOV R4,B
        ADD A,#2
        ADD A,R5
        MOV B,#10
        DIV AB
        MOV R5,B
        CJNE R6,#0,ADD_IT
        SJMP CONTINUE
ADD_IT:
        ADD A,R6
CONTINUE:
        MOV R6,A
        DJNZ R1,HIGH_BYTE
        MOV B, #10
        MOV A,R6
        DIV AB
        MOV R6,B
        MOV R7,A



 
	
FINISH: RET

DELAY: 
      PUSH              5
      PUSH              4
      PUSH              3
      MOV               R5,#20               
X3:   MOV               R4,#200             
X2:   MOV               R3,#250             
X1:   DJNZ              R3,X1                 
      DJNZ              R4,X2		    
      DJNZ              R5,X3     
      POP               3
      POP               4
      POP               5
      RET  
  

 

NEXT1:    
; CIRCUMFERENCE CALCULATION OF RECTANGLE

 



MOV A,76H
MOV B,77H

 

MOV R0,A ; 
MOV R1,B ; 

 

MOV B,#10
DIV AB
SWAP A
ADD A,B ; CONVERT VAULE L TO BCD
MOV 51H,A ; SAVE L IN 51H RAM

 

MOV A,R1
MOV B,#10
DIV AB
SWAP A
ADD A,B ; CONVERT VAULE W TO BCD
MOV 52H,A ; SAVE W IN 52H RAM

 

MOV A,R0
MOV B,R1

 

MOV R0,A
ADD A,R0 ; C = 2L
JNC NEXT0 ; JUMP IF NO CARRY
INC R7 ; INCREMENT R7 IF HAS A CARRY
NEXT0:
MOV 81H,A ; SAVE VALUE 2L IN RAM 81H TO USE IT TO CALCULATE C = 2L + 2W

 

MOV A,B
ADD A,B ; C = 2W
JNC NEXT2 ; JUMP IF NO CARRY
INC R7 ; INCREMENT R7 IF HAS A CARRY
NEXT2:
MOV 82H,A ; SAVE VALUE 2W IN RAM 82H TO USE IT TO CALCULATE C = 2L + 2W

 

MOV A,81H
MOV B,82H
ADD A,B ; C= 2L + 2W
JNC NEXT22 ; JUMP IF NO CARRY
INC R7 ; INCREMENT R7 IF HAS A CARRY
NEXT22:
MOV B,R7
ACALL BCD ; CONVERT THE RESULT FROM HEX TO BCD

 

MOV A,R6
SWAP A
ADD A,R5
MOV R0,A
MOV A,R4
SWAP A
ADD A,R3
MOV B,R0 ;CONVERT THE LOWER AMD UPPER BYTE OF RESULT TO BCD

 

MOV 62H,A ; SAVE THE LOWER BYTE OF RESULT IN 62H RAM
MOV 61H,B ; SAVE THE LOWER BYTE OF RESULT IN 61H RAM

 ACALL RECTANGLE_LCD 
 LJMP Next1433

RET


RECTANGLE_LCD: 

MOV A,#80H
ACALL CMD

MOV DPTR,#RECAREA
ACALL WSTR ; The first line of the LCD screen shows the words in RECAREA

MOV A,#0C4H
ACALL CMD

MOV A,41H
ACALL LCD0
MOV A,42H
ACALL LCD0 ; The second line of the LCD screen shows the result of calculate area

MOV R2,#2
XX:
ACALL DELAY
DJNZ R2,XX ; appears for 2 sec

 MOV A,#0 
 ACALL CMD
 MOV A,#0001B 
 ACALL CMD
 ACALL LDELAY ; Clear Display - LCD -
 
 
 MOV A,#80H
 ACALL CMD
 
 MOV DPTR,#RECCIR
 ACALL WSTR ; The first line of the LCD screen shows the words in RECCIR
 
 MOV A,#0C4H
 ACALL CMD
 
 MOV A,61H
 ACALL LCD0
 MOV A,62H
 ACALL LCD0 ; The second line of the LCD screen shows the result of calculate circumference
 
 MOV R2,#2
XY:
ACALL DELAY
DJNZ R2,XY ; appears for 2 sec

 MOV A,#0 
 ACALL CMD
 MOV A,#0001B 
 ACALL CMD
 ACALL LDELAY ; Clear Display - LCD -
 
 
 RET
LCD0:

MOV R1,A
SWAP A
ANL A,#00001111B
ADD A,#30H
ACALL WCHR
MOV A,R1
ANL A,#00001111B
ADD A,#30H
ACALL WCHR
RET  ; Convert form BCD to ASCII to show BCD digits on LCD


TRIANGLE_CALC:



; Assigned Task:
;				Implementing the code of calculating the area and the perimeter of a triangle.



                   ; Sets the start of the program at memory address 0H
MOV A,76H                 ; Moves value from A register to R0 register
MOV B,77H                 ; Moves value from B register to R1 register

MOV R0,A                    ; Moves value from A register to R0 register
MOV R1,B                    ; Moves value from B register to R1 register

MOV B,#10                   ; Loads immediate value 10 to B register
DIV AB                      ; Divides A by B; Quotient in A, Remainder in B
SWAP A                      ; Swaps high and low nibbles of A register
ADD A,B                     ; Adds value in B to A
MOV 35H,A                   ; Stores result from A register to memory address 35H

MOV A,R1                    ; Moves value from R1 register to A register
MOV B,#10                   ; Loads immediate value 10 to B register
DIV AB                      ; Divides A by B; Quotient in A, Remainder in B
SWAP A                      ; Swaps high and low nibbles of A register
ADD A,B                     ; Adds value in B to A
MOV 36H,A                   ; Stores result from A register to memory address 36H

MOV A,R0                    ; Moves value from R0 register to A register
MOV B,R1                    ; Moves value from R1 register to B register
MUL AB  ; Multiplies A by B; Low byte of result in A, High byte in B

MOV 72H,A

CJNE A,#1,CONT99
MOV A,B
CJNE A,#0,CONT99
MOV A,#80H
ACALL CMD

 

MOV DPTR,#TRIAREA
ACALL WSTR ; The first line of the LCD screen shows the words in TRIAREA

MOV A,#0C4H
ACALL CMD
MOV DPTR,#TRI_R
ACALL WSTR

MOV R7,#2
ZP:
ACALL DELAY
DJNZ R7,ZP ; appears for 2 sec

AJMP PERIMETER

CONT99:

MOV R7,A                    ; Moves value from A register to R7 register
MOV A,B                     ; Moves value from B register to A register
CLR C                       ; Clears carry flag
RRC A                       ; Rotates A register right through carry
MOV B,A                     ; Moves value from A register to B register
MOV A,R7                    ; Moves value from R7 register to A register
RRC A                       ; Rotates A register right through carry

ACALL BCD                   ; CALL BCD SUB-ROUTINE TO CONVERT THE RESULT INTO BCD

; In the following steps the BCD result is organized and stored in A and B
MOV A,R6                    ; Moves value from R6 register to A register
SWAP A                      ; Swaps high and low nibbles of A register
ADD A,R5                    ; Adds value in R5 to A
MOV R0,A                    ; Moves value from A register to R0 register
MOV A,R4                    ; Moves value from R4 register to A register
SWAP A                      ; Swaps high and low nibbles of A register
ADD A,R3                    ; Adds value in R3 to A
MOV B,R0                    ; Moves value from R0 register to B register

MOV 46H,A                   ; Stores result from A register to memory address 46H
MOV 45H,B                   ; Stores result from B register to memory address 45H

AJMP PERIMETER             ; Jumps to label "PERIMETER" to calculate the PERIMETER of a triangle.



PERIMETER:

MOV A,47H
MOV B,48H
MOV R2,49H

MOV R0,A ; Load the contents of Accumulator into Register R0
MOV R1,B ; Load the contents of Register B into Register R1

MOV B,#10 ; Load immediate data 10 into Register B
DIV AB ; Divide Accumulator by Register B, quotient in Accumulator and remainder in B
SWAP A ; Swap the nibbles in Accumulator
ADD A,B ; Add the contents of Accumulator and Register B
MOV 55H,A ; Store the contents of Accumulator into direct address 55H

MOV A,R1 ; Load the contents of Register R1 into Accumulator
MOV B,#10 ; Load immediate data 10 into Register B
DIV AB ; Divide Accumulator by Register B, quotient in Accumulator and remainder in B
SWAP A ; Swap the nibbles in Accumulator
ADD A,B ; Add the contents of Accumulator and Register B
MOV 56H,A ; Store the contents of Accumulator into direct address 56H

MOV A,R2; Load the contents of Register R2 into Accumulator
MOV B,#10 ; Load immediate data 10 into Register B
DIV AB ; Divide Accumulator by Register B, quotient in Accumulator and remainder in B
SWAP A ; Swap the nibbles in Accumulator
ADD A,B ; Add the contents of Accumulator and Register B
MOV 57H,A ; Store the contents of Accumulator into direct address 57H

MOV A,R0 ; Load the contents of Register R0 into Accumulator
MOV B,R1 ; Load the contents of Register R1 into Register B

ADD A,B ; Add the contents of Accumulator and Register B
JNC NEXT11 ; If no carry, jump to the label NEXT1
INC R7 ; Increment Register R7
NEXT11:
ADD A,R2 ; Add the contents of Accumulator and Register R2
JNC NEXT32 ; If no carry, jump to the label NEXT2
INC R7 ; Increment Register R7
NEXT32:
MOV B,R7  ; Load the contents of Register R7 into Register B
ACALL BCD ; Call the subroutine at the address BCD

; In the following steps the BCD result is organized and stored in A and B
MOV A,R6
SWAP A
ADD A,R5
MOV R0,A
MOV A,R4
SWAP A
ADD A,R3
MOV B,R0

MOV 66H,A ; Store the contents of Accumulator into direct address 66H
MOV 65H,B ; Store the contents of Register B into direct address 65H

MOV A,72H
CJNE A,#01,NEXT88
SJMP NEXT44
NEXT88:
ACALL TRIANGLE_LCD 
RET

TRIANGLE_LCD: 

MOV A,#80H
ACALL CMD

MOV DPTR,#TRIAREA
ACALL WSTR ; The first line of the LCD screen shows the words in TRIAREA

MOV A,#0C4H
ACALL CMD

MOV A,45H
ACALL LCD0
MOV A,46H
ACALL LCD0 ; The second line of the LCD screen shows the result of calculate area

MOV R2,#2
ZZ:
ACALL DELAY
DJNZ R2,ZZ ; appears for 2 sec

 MOV A,#0 
 ACALL CMD
 MOV A,#0001B 
 ACALL CMD
 ACALL LDELAY ; Clear Display - LCD -
 
 NEXT44:
 MOV A,#80H
 ACALL CMD
 
 MOV DPTR,#TRIPER
 ACALL WSTR ; The first line of the LCD screen shows the words in TRIPER
 
 MOV A,#0C4H
 ACALL CMD
 
 MOV A,65H
 ACALL LCD0
 MOV A,66H
 ACALL LCD0 ; The second line of the LCD screen shows the result of calculate perimeter
 
 MOV R2,#2
ZW:
ACALL DELAY
DJNZ R2,ZW ; appears for 2 sec

 MOV A,#0 
 ACALL CMD
 MOV A,#0001B 
 ACALL CMD
 ACALL LDELAY ; Clear Display - LCD -
 
 
 RET



;-- LCD Initialization Procedure starts here -----

INLCD:
PUSH 7
MOV R7,#20
WAIT:
ACALL LDELAY ; Step 1
DJNZ R7,WAIT
MOV P0,#00000111B ; Initialise 3 control signals=1
MOV A,#0011B ; Step 2
ACALL CMD
ACALL LDELAY ; Step 3
MOV A,#0011B ; Step 4
ACALL CMD
MOV A,#0011B ; Step 5
ACALL CMD
MOV A,#0010B ; Step 6
ACALL CMD
MOV A,#0010B ; Step 7  send high nibble
ACALL CMD
MOV A,#1000B ; send low nibble
ACALL CMD
MOV A,#0000B ; Step 8  Turn off display  send high nibble
ACALL CMD
MOV A,#1000B ; send low nibble
ACALL CMD
MOV A,#0 ; Step 9 - Clear Display  send high nibble
ACALL CMD
MOV A,#0001B ; send low nibble
ACALL CMD
ACALL LDELAY ; 4.1 msec required for this command
MOV A,#0000B ; Step 10 - Set cursor Move RIGHT- send high nibble
ACALL CMD
MOV A,#0110B ; send low nibble
ACALL CMD
MOV A,#0000B ; Step 11 send high nibble
ACALL CMD ; Turn ON Display, Cursor ON, Blink Cursor
MOV A,#1111B ; send low nibble
ACALL CMD
POP 7
RET
;--- End of LCD initialization -----
;---- Subroutine to write COMMAND in A to the LCD -------
CMD:
PUSH ACC
PUSH B
CLR RS ; RS = 0 command write
MOV B,A
SWAP A ; Move higher nibble to lower nibble
ACALL COMMON ; write operation
MOV A,B
ACALL COMMON
POP B
POP ACC
RET
;---- Subroutine to write character in A to the LCD -------
WCHR:
PUSH ACC
PUSH B
SETB RS ; RS = 1 data write
MOV B,A
SWAP A ; Move higher nibble to lower nibble
ACALL COMMON ; write operation
MOV A,B
ACALL COMMON
POP B
POP ACC
RET
;----- Common operation for CHAR write and COMMAND write
COMMON:
CLR RW
SWAP A ; Move Lower nibble to higher nibble
ANL A,#11110000B
ANL P0,#00000111B
ORL P0,A
SETB EN
CLR EN
ACALL LDELAY
RET
;---- Subroutine to write A STRING character by character -------
WSTR:
PUSH ACC
CONT144:
CLR A
MOVC A,@A+DPTR ; move character to A
JZ EXIT1
ACALL WCHR ; call procedure to write a CHAR
INC DPTR ; get next character
AJMP CONT144 ; go to CONT1
EXIT1:
POP ACC ; restore A
RET
;------------  5.4msec DELAY -------------------------------------------

LDELAY:
PUSH 0
PUSH 1 ; save register1.
MOV R1,#20 ; move 12 to register R1
CON4:
MOV R0,#250
DJNZ R0,$
DJNZ R1,CON4 ; decrease R1, if R1 !=0, go to CON4
POP 1
POP 0
RET


STR1: DB 'Welcome to the program!', 0 ; message to be displayed.
NAME1: DB 'Hamad Naqi 2192131572',0
NAME2: DB 'Nawaf Aldhawi 2201113545',0
NAME3: DB 'Fahad Alenzi 2201121917',0
INTR_MSG: DB 'Press the INT0 button',0
ENTER: DB 'ENTER YOUR SELECTION: ',0
S12: DB '1-Rectangle 2-Triangle',0
S3: DB '3-Exit',0
REC_Length: DB 'Enter the length:',0
REC_width: DB 'Enter the width:',0
TRI_BASE: DB 'Enter the base: ',0
TRI_Height: DB 'Enter the height: ',0
TRI_A: DB 'Enter side A: ',0
TRI_b: DB 'Enter side B: ',0
TRI_c: DB 'Enter side C: ',0
END_MSG: DB 'PROGRAM END',0
ER_MSG: DB 'ERROR WRONG INPUT!',0
RECAREA: DB 'Rectangle Area',0
RECCIR:	DB 'Rectangle Circumference',0
TRIAREA: DB 'Triangle Area',0
TRIPER: DB 'Triangle Perimeter',0
TRI_R: DB '0.5',0
END
