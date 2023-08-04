;-----------------------------------------------------------
fillRectangle macro xStart,yStart,xEnd,yEnd,colorCode
   LOCAL verticalLoop,horizontalLoop,black,setPixel
   PUSH CX;saving CX and DX into stack
   PUSH DX;
   
   MOV DX,yStart;start point y coordinate
       verticalLoop:
    
       MOV CX,xStart;start point x coordinate
           horizontalLoop:
                             
           CMP CX,xStart ;Checking borders        
           JE black      ;of rectangles
           CMP CX,xEnd-1 ;to color them       
           JE black      ;to black or
           CMP DX,yStart ;desired fill        
           JE black      ;color
           CMP DX,yEnd-1 ;        
           JE black      ;
            
           MOV AL,colorCode;setting desired fill color
           JMP setPixel
           
           black:          
           MOV AL,00H;setting border color:black
          
           setPixel:
           MOV BX,00H;Page:0
           MOV AH,0CH;Set pixel 
           INT 10H;
                  
           INC CX            ;Checking horizontal
           CMP CX,xEnd       ;loop borders
           JNE horizontalLoop;     
       
       INC DX          ;Checking vertical 
       CMP DX,yEnd     ;loop borders
       JNE verticalLoop; 
   
   POP DX;Restoring DX and CX from stack
   POP CX;
endm   
;-----------------------------------------------------------
.model small
.stack 100h
.data 
    inputCharCount dw 00D   ;Contains how many valid characters in input Buffer
    inputBuffer db 14 dup(?);Used with input char count in input handling
    pressedKey db ?         ;Contains pressed keycode in main program loop
    mouseX dw ?             ;Contains mouse X coordinate
    mouseY dw ?             ;Contains mouse Y coordinate
    mouseButtons dw ?       ;Contains mouse button states
    color db ?              ;Used in various functions as input variable
    topLeftX dw 560d        ;Printing functions uses these variables as starting position
    topLeftY dw 56d         ;
    bottomRightX dw ?       ;Printing functions uses these variables as ending position
    bottomRightY dw ?       ;
    ExpansionRate dw 5     
    ;Font  consists of 8x8 bit-map characters  however
    ;while printing ,pixels are seperated and expanded 
    ;i.e. multiple pixels are treated as one pixel
    ;thus producing low-res but larger characters in dimension
    sign			db	00H, 10H, 38H, 10H, 00H, 38H, 00H, 00H	; (+-)
    sqrt			db	1EH, 10H, 10H, 10H, 10H, 50H, 30H, 10H	; (sqrt)
    percent			db	20H, 52H, 24H, 08H, 10H, 24H, 4AH, 04H	; (%)
    multiply 		db	00H, 00H, 22H, 14H, 08H, 14H, 22H, 00H	; (*)
    plus			db	00H, 18H, 18H, 7EH, 7EH, 18H, 18H, 00H	; (+)
    minus			db	00H, 00H, 00H, 7EH, 7EH, 00H, 00H, 00H	; (-)
    dot				db	00H, 00H, 00H, 00H, 00H, 00H, 18H, 18H	; (.)
    divide			db	00H, 02H, 04H, 08H, 10H, 20H, 40H, 00H	; (/)
    zero			db	3CH, 42H, 42H, 42H, 42H, 42H, 42H, 3CH	; (0)
    one				db	18H, 78H, 08H, 08H, 08H, 08H, 08H, 7EH	; (1)
    two				db	3CH, 42H, 42H, 04H, 08H, 10H, 20H, 7EH	; (2)
    three			db	3CH, 42H, 02H, 0CH, 02H, 02H, 42H, 3CH	; (3)
    four			db	04H, 0CH, 14H, 24H, 44H, 7EH, 04H, 04H	; (4)
    five			db	7EH, 40H, 40H, 7CH, 02H, 02H, 02H, 7CH	; (5)
    six				db	3EH, 40H, 40H, 40H, 7CH, 42H, 42H, 3CH	; (6)
    seven			db	7EH, 02H, 04H, 08H, 08H, 08H, 08H, 08H	; (7)
    eight			db	3CH, 42H, 42H, 42H, 3CH, 42H, 42H, 3CH	; (8)
    nine			db	3CH, 42H, 42H, 3EH, 02H, 02H, 44H, 38H	; (9)
    equals			db	00H, 00H, 7EH, 00H, 7EH, 00H, 00H, 00H	; (=)
    M_char			db	42H, 66H, 5AH, 42H, 42H, 42H, 42H, 42H	; (M)
    C_char          db  7EH, 40H, 40H, 40H, 40H, 40H, 40H, 7EH  ; (C)
    R_char  		db	7EH, 42H, 42H, 7CH, 44H, 42H, 42H, 42H	; (R) 
    P_char  		db	7EH, 42H, 42H, 7EH, 40H, 40H, 40H, 40H	; (P)
    fullSet         db  0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH                           
   ;Button starting points stored in memory
   ;for graphics and mouse applications.
   ;Ending coordinates are not needed since
   ;their coordinates are relative to starting points
   ;NAMExEnd:NAMExStart+70(width of buttons)
   ;NAMEyEnd:NAMEyStart+60(height of buttons)                       
   buttonHeight equ 60D
   buttonWidth equ 70D
      
   MCxStart equ 40D  
   MCyStart equ 160D 
   MRxStart equ 40D  
   MRyStart equ 240D  
   MPxStart equ 40D  
   MPyStart equ 320D 
   MMxStart equ 40D  
   MMyStart equ 400D
   
   SEVENxStart equ 138D
   SEVENyStart equ 160D
   FOURxStart equ 138D
   FOURyStart equ 240D
   ONExStart equ 138D
   ONEyStart equ 320D
   ZEROxStart equ 138D
   ZEROyStart equ 400D
   
   EIGHTxStart equ 236D
   EIGHTyStart equ 160D
   FIVExStart equ 236D
   FIVEyStart equ 240D
   TWOxStart equ 236D
   TWOyStart equ 320D
   DOTxStart equ 236D
   DOTyStart equ 400D
   
   NINExStart equ 334D
   NINEyStart equ 160D
   SIXxStart equ 334D
   SIXyStart equ 240D
   THREExStart equ 334D
   THREEyStart equ 320D
   SxStart equ 334D
   SyStart equ 400D
   
   DIVxStart equ 432D
   DIVyStart equ 160D
   MULxStart equ 432D
   MULyStart equ 240D
   MINxStart equ 432D
   MINyStart equ 320D
   PLSxStart equ 432D
   PLSyStart equ 400D
   
   SQRxStart equ 530D
   SQRyStart equ 160D
   PRCxStart equ 530D
   PRCyStart equ 240D
   CxStart equ 530D
   CyStart equ 320D
   EQxStart equ 530D
   EQyStart equ 400D
   
   EXITxStart equ 620D
   EXITyStart equ 1D
   EXITxEnd equ 639D
   EXITyEnd equ 20D 
   
   xStart dw ? ;Used in some functions as input variables
   yStart dw ? ;
   xEND dw ?   ;
   yEND dw ?   ;
   
    FALSE equ 00H       ;Boolean values for various variables
    TRUE  equ 0FFH      ;   
    OPERATOR EQU 0FFH   ;Used in buffers float part to indicate corresponding value is an operator
    POSITIVE EQU 0FFH     ;Sign variables uses these as content
    NEGATIVE EQU 00H      ;
    UNARYPLUS EQU 0FAH    ;Unary operator placeholders
    UNARYMINUS EQU 0FBH   ;
    ENDOFBUFFER equ 0EEH  ;Used in buffers float part (0EEH>99D thus not a valid float value)
    SQUAREROOTCHAR equ 23H;Used as square root char placeholder in buffers 
    dividend dd ?         ;Used by calculation functions
    divisor dd ?          ;
    remainder dw ?        ;
    multiplier dw ?       ;
    highWord dw ?        ;
    operand1Sign db NEGATIVE;Input and output variables for algebraic functions
    operand2Sign db NEGATIVE;Each of Operand 1 , Operand 2 output variables consists of
    operand1Int dd 400D     ;4 bytes of Integer part 
    operand1Float db 53d    ;(some functions only implement lower 2 byte and if input/output 
    operand2Int dd 250D     ;variable exceeds these bytes results will be erronous)
    operand2Float db 24d    ;1 byte of float part (only 2 digits after decimal is needed
    outputInt dd 0DDDDCCBBH ;1 byte is more than enough (maximum value is 99<255))
    outputFloat db 0ABH     ;1 byte of sign part 
    outputIsNegative db ?   ;
    outputSign db POSITIVE  ;
    carryToIntFlag db ? 
    nextGuessInt dw ?       ;Used by squareroot function as local variable
    nextGuessFloat db ?     ;
    previousGuessInt dw ?   ;
    previousGuessFloat db ? ;
    numberToFindSquarerootOfInt dw 60000d;Input variables for square root function
    numberToFindSquarerootOfFloat db 00d ; 
    ;Middle step buffer is used as first buffer after input buffer which contains
    ;numbers and operators(indicated by float part = OPERATOR(0FFH)) in input order
    middleStepBufferInt dd 0h,0h,0h,0h,0H,0h,h,0h,0h,0h,0h,0h,0h,0h   
    middleStepBufferFloat db 0h,0h,0h,0h,0H,0h,0h,0h,0h,0h,0h,0h,0h,0h
    
    afterDotFlag db FALSE;Used while translating inputted float numbers
    ;RPNbuffer contains inputted expression in reverse polish notation
    ;and this buffer is used while solving expression
    RPNBufferInt  dd 0h,0h,0h,0h,0H,0h,0h,0h,0h,0h,0h,0h,0h,0h,0h,0h,0h
    RPNBufferFloat  db 0h,0h,0h,0H,0H,0H,0h,0h,0h,0h,0h,0h,0h,0h,0h,0h,0h 
    RPNBufferNextElementIndex dw 0h
    ;These chunk of memory is used as a virtual stack variable to contain operators
    ;while solving expression
    virtualOperatorStack db 0h,0h,0h,0h,0h,0h,0h,0h,0h,0h,0h,0h,0h,0h,0h,0h
    virtualStackNextElementIndex dw 0h 
    elementOnTopOfStack dw 0h ;last element pushed to virtual operator stack
    incomingOperator db FALSE;Used as a local variable while solving RPN expression 
    
    ;Virtual output stack is used while solcing rpn expression 
    ;and some functions uses first element of this stack as input variable
    virtualOutputStackInt dd 0h,0h,0h,0h,0h,0h,0h,0h,0h,0h,0h,0h,0h,0h
    virtualOutputStackFloat db 0h,0h,0h,0h,0h,0h,0h,0h,0h,0h,0h,0h,0h,0h
    virtualOutputStackSign db 0h,0h,0h,0h,0h,0h,0h,0h,0h,0h,0h,0h,0h,0h
    virtualOutputStackNextElementIndex dw 0h
    
    memoryInt dd 0h       
    memoryFloat db 0h     
    memorySign db POSITIVE
    
    lastButtonState dw 0h    
    
    resultString db 14 dup(?)  
    
    floatSign db POSITIVE   ;Used by substract function
    lowWordSign db POSITIVE ;
    highWordSign db POSITIVE;
;-----------------------------------------------------------  
.code
main proc 
   MOV AH,0FH ;Saving current video mode
   INT 10H    ;into stack to set video
   PUSH AX    ;mode back to this mode
   
   MOV AH,00H ;Set video mode to 
   MOV AL,12H ;640x480 16 colors
   INT 10H    ; 
   INT 10H 
         
   CALL fillBackground                             
   fillRectangle 38,40,602,120,0FH 
   CALL printButtons 
   CALL printExitButton
   mov ax, 0 ;Mouse initialization
   int 33h   ;
   mov ax, 1 ;
   int 33h   ;
  
   MOV AX,@DATA
   MOV DS,AX 
   LEA DI,inputBuffer
   
   
   getmousedata: 
               MOV BX,mouseButtons   ;setting last button state variable
               MOV lastButtonState,BX;
               mov ax, 3 ;Getting mouse data
               int 33h   ;
               MOV mouseButtons,BX ;Checking if any button is clicked
               CMP mouseButtons,00H;       ;if button is not released dont check mouse location
               JNE  dontCheckMouseLocation ;                            
               CMP lastButtonState,00H     ;
               JE dontCheckMouseLocation   ;   
                  ;if button is released
                  ;SHR CX,1 ;Comment this shift for dosbox uncomment for emu8086
                  
                  MOV mouseX,CX;set mouse x and y variables 
                  MOV mouseY,DX;
                           
                    CMP mouseX,EXITxStart ;Checking if program is terminated by  clicking
                    JB notExit            ;close button
                    CMP mouseY,EXITyStart ;
                    JB notExit            ;
                    CMP mouseX,EXITxEnd   ;
                    JA notExit            ;
                    CMP mouseY,EXITyEnd   ;
                    JA notExit            ;
                       JMP terminate      ;
                    notExit:
                       
                  CALL mouseClickHandle ;calling click handle to perform operations  
                  JMP DISPLAY           ;depending on which buttons is clicked
                                        ;and jumping to display part
               dontCheckMouseLocation: 
               
               MOV AH,01D      ;Check key press 
               INT 16H         ;
               JZ getmousedata ;If not pressed get mouse data again
   
   MOV AH,00H       ;if key is pressed get key
   INT 16H          ;
   MOV pressedKey,AL;
   CMP AL,1BH         ;Ýf key is ESC terminate program
   JE terminate       ;if not
   CALL handleKeyPress;handle key press
   DISPLAY:
   
   CMP InputCharCount,01D ;if input char count is at least one display inputBuffer
   JB  getmousedata       ;else get mouse data again 
   JA  dontclearBeforeDisplaying
   fillRectangle 38,40,602,120,0FH;Clear calculator input/output screen
   dontclearBeforeDisplaying:
   CALL displayInputBuffer;
   
           
   JMP getmousedata;get new mouse data data          
                              
   terminate:                    
   POP AX    ;Reverting changes on
   MOV AH,00H;video mode
   INT 10H   ;
   
   mov ax, 4c00h;Terminating 
   int 21h      ;program

   
main endp
;------------------------------------------------------------------------------ 
;resetBuffer function : Clears(sets to 0) a chunk of memory indicated by inputs
;INPUTS:
;CX:bufferSize (bytes)
;DI:pointerToBuffer
resetBuffer PROC 
     
clearElement:
     MOV [DI],00H
     INC DI    
     LOOP clearElement         
RET
resetBuffer ENDP
;----------------------------------------------------------------------------------
;resetCalculationBuffers function: resets buffers that used whle calculating result
resetCalculationBuffers PROC
    MOV CX,56
    LEA DI,RPNBufferInt
    call resetBuffer
    
    MOV CX,14
    LEA DI,RPNBufferFloat
    call resetBuffer
    
    MOV CX,56
    LEA DI,middleStepBufferInt
    call resetBuffer
    
    MOV CX,14
    LEA DI,middleStepBufferFloat
    call resetBuffer
    
    MOV CX,14
    LEA DI,virtualOperatorStack
    call resetBuffer 
    
    MOV CX,56
    LEA DI,virtualOutputStackInt
    call resetBuffer
    
    MOV CX,14
    LEA DI,virtualOutputStackFloat
    call resetBuffer
    
    
    MOV CX,14
    LEA DI,virtualOutputStackSign
    call resetBuffer   
    
    MOV afterDotFlag,FALSE
    MOV RPNBufferNextElementIndex,00H
    MOV elementOnTopOfStack,00H 
    MOV virtualOutputStackNextElementIndex,00H
RET
resetCalculationBuffers ENDP
;---------------------------------------------------------------------------------------
;produceResultString function:Produces result string according to result of calculations 
;INPUT:
;virtualOutputStackInt,virtualOutputStackFloat,virtualOutputStackSign(first elements)
;OUTPUT:
;resultString
produceResultString PROC
      CMP virtualOutputStackNextElementIndex,1D
      JNE dontPrintResult
      
      LEA SI,resultString
      MOV CX,13H
      resetCurrentElementToNull:
      MOV [SI],00H
      INC SI
      LOOP resetCurrentElementToNull  
      
      LEA SI,resultString
      
      CMP virtualOutputStackSign,NEGATIVE
      JNE dontPrintMinus
       
      MOV [SI],'-'  
      
      dontPrintMinus:
      ADD SI,13D
      CMP virtualOutputStackFloat,00H
      JE convertIntegerPart 
      
      MOV AH,0H
      MOV AL,virtualOutputStackFloat 
      MOV BL,10d
      DIV BL 
      ADD AH,30H
      ADD AL,30H            
      MOV [SI],AH 
      DEC SI
      MOV [SI],AL
      DEC SI
      MOV [SI],'.' 
      DEC SI
      convertIntegerPart: 
      MOV DX,0H
      MOV AX,virtualOutputStackInt 
      nextDecade:
      MOV BX,10d
      DIV BX 
      ADD DX,30H           
      MOV [SI],DL
      MOV DX,00H
      DEC SI
      CMP AX,0H
      JNE nextDecade
      
      
      LEA SI,resultString
      MOV CX,13H
      checkNextCharacter:
      CMP [SI],'-'
      JNE dontShiftNextElement
      CMP [SI+1],00H
      JNE dontShiftNextElement
      
      MOV AL,[SI] 
      MOV [SI],00H
      MOV [SI+1],AL
      
      dontShiftNextElement:
      INC SI
      LOOP checkNextCharacter    
      
dontPrintResult:
      
         
RET
produceResultString ENDP 
;------------------------------- 
;displayString function:Displays indicated string on result screen of calculator
;INPUT:
;DI:String to be printed 
displayString PROC 
  
        MOV AX,@DATA
        MOV DS,AX 
        MOV topLeftX,560d
        MOV topLeftY,56d
        MOV expansionRate,5d 
        ADD DI,14D
printNextCharacter:
        DEC DI
        CMP [DI],00H          
        JE dontPrintMoreChars        
        CMP [DI],30H          
        JE print_Zero
        CMP [DI],31H          
        JE print_One
        CMP [DI],32H          
        JE print_Two       
        CMP [DI],33H          
        JE print_Three
        CMP [DI],34H          
        JE print_Four
        CMP [DI],35H          
        JE print_Five
        CMP [DI],36H          
        JE print_Six
        CMP [DI],37H          
        JE print_Seven
        CMP [DI],38H          
        JE print_Eight   
        CMP [DI],39H          
        JE print_Nine 
        CMP [DI],'.'
        JE print_Dot
        CMP [DI],2DH          
        JE print_Minus
        
        JMP printNextCharacter
        
        print_Zero:
        LEA SI,zero
        JMP callSubroutine
        print_One:
        LEA SI,one
        JMP callSubroutine
        print_Two:
        LEA SI,two
        JMP callSubroutine
        print_Three:
        LEA SI,three
        JMP callSubroutine
        print_Four:
        LEA SI,four
        JMP callSubroutine
        print_Five:
        LEA SI,five
        JMP callSubroutine
        print_Six:
        LEA SI,six
        JMP callSubroutine
        print_Seven:
        LEA SI,seven
        JMP callSubroutine
        print_Eight:
        LEA SI,eight
        JMP callSubroutine
        print_Nine:
        LEA SI,nine
        JMP callSubroutine
        print_Dot:
        LEA SI,dot
        JMP callSubroutine
        print_Minus:
        LEA SI,minus
        JMP callSubroutine
   callSubroutine:     
        
       CALL printCharacter
        
        MOV AX,8 
        MOV BX,expansionRate
        MUL BX
        SUB topLeftX,AX
        
           
        LEA SI,resultString
        CMP DI,SI
        JNE printNextCharacter
        
dontPrintMoreChars:       
    
RET                        
displayString ENDP
;--------------------------------------------------- 
printExitButton PROC 
    fillRectangle EXITxStart,EXITyStart,EXITxEnd,EXITyEnd,0FH
    MOV topleftX,EXITxStart+2
    MOV toplefty,EXITyStart+2 
    MOV expansionRate,2d
    LEA SI,multiply
    CALL printCharacter
RET
printExitButton ENDP                                                  
;-------------------------------------------------------------------------------------------
;mouseClickHandle function:handles mouse clickes on buttons and performs necessary actions
;INPUTS:
;mouseX,mouseY
;button pixel positions on screen (for example SEVENxStart,SEVENyStart)
;inputBuffer
;inputCharCount
mouseClickHandle PROC 
    CMP inputCharCount,14d 
    JE bufferIsFULL 
    
    LEA DI,inputBuffer 
    ADD DI,inputCharCount 
     
    CMP mouseX,SEVENxStart
    JB notSeven
    CMP mouseY,SEVENyStart
    JB notSeven
    CMP mouseX,SEVENxStart+buttonWidth
    JA notSeven
    CMP mouseY,SEVENyStart+buttonHeight
    JA notSeven
       
       MOV [DI],37H  
       INC inputCharCount
       RET
    notSeven: 
       
    CMP mouseX,FOURxStart
    JB notFour
    CMP mouseY,FOURyStart
    JB notFour
    CMP mouseX,FOURxStart+buttonWidth
    JA notFour
    CMP mouseY,FOURyStart+buttonHeight
    JA notFour
       
       MOV [DI],34H  
       INC inputCharCount
       RET
    notFour:
    
    CMP mouseX,ONExStart
    JB notOne
    CMP mouseY,ONEyStart
    JB notOne
    CMP mouseX,ONExStart+buttonWidth
    JA notOne
    CMP mouseY,ONEyStart+buttonHeight
    JA notOne
       
       MOV [DI],31H  
       INC inputCharCount
       RET
    notOne:
    
    CMP mouseX,EIGHTxStart
    JB notEight
    CMP mouseY,EIGHTyStart
    JB notEight
    CMP mouseX,EIGHTxStart+buttonWidth
    JA notEight
    CMP mouseY,EIGHTyStart+buttonHeight
    JA notEight
       
       MOV [DI],38H  
       INC inputCharCount
       RET
    notEight: 
       
    CMP mouseX,FIVExStart
    JB notFive
    CMP mouseY,FIVEyStart
    JB notFive
    CMP mouseX,FIVExStart+buttonWidth
    JA notFive
    CMP mouseY,FIVEyStart+buttonHeight
    JA notFive
       
       MOV [DI],35H  
       INC inputCharCount
       RET
    notFive:
    
    CMP mouseX,TWOxStart
    JB notTwo
    CMP mouseY,TWOyStart
    JB notTwo
    CMP mouseX,TWOxStart+buttonWidth
    JA notTwo
    CMP mouseY,TWOyStart+buttonHeight
    JA notTwo
       
       MOV [DI],32H  
       INC inputCharCount
        RET
    notTwo:
                               
    CMP mouseX,NINExStart
    JB notNine
    CMP mouseY,NINEyStart
    JB notNine
    CMP mouseX,NINExStart+buttonWidth
    JA notNine
    CMP mouseY,NINEyStart+buttonHeight
    JA notNine
       
       MOV [DI],39H  
       INC inputCharCount
       RET
    notNine: 
       
    CMP mouseX,SIXxStart
    JB notSix
    CMP mouseY,SIXyStart
    JB notSix
    CMP mouseX,SIXxStart+buttonWidth
    JA notSix
    CMP mouseY,SIXyStart+buttonHeight
    JA notSix
       
       MOV [DI],36H  
       INC inputCharCount
       RET
    notSix:
    
    CMP mouseX,THREExStart
    JB notThree
    CMP mouseY,THREEyStart
    JB notThree
    CMP mouseX,THREExStart+buttonWidth
    JA notThree
    CMP mouseY,THREEyStart+buttonHeight
    JA notThree
       
       MOV [DI],33H  
       INC inputCharCount
       RET
    notThree:         
    
    CMP mouseX,ZEROxStart
    JB notZero
    CMP mouseY,ZEROyStart
    JB notZero
    CMP mouseX,ZEROxStart+buttonWidth
    JA notZero
    CMP mouseY,ZEROyStart+buttonHeight
    JA notZero
       
       MOV [DI],30H  
       INC inputCharCount
       RET
    notZero: 
    
    CMP mouseX,DOTxStart
    JB notDot
    CMP mouseY,DOTyStart
    JB notDot
    CMP mouseX,DOTxStart+buttonWidth
    JA notDot
    CMP mouseY,DOTyStart+buttonHeight
    JA notDot
       
       MOV [DI],2EH  
       INC inputCharCount
       RET
    notDot:
    
    CMP mouseX,SxStart
    JB notSign
    CMP mouseY,SyStart
    JB notSign
    CMP mouseX,SxStart+buttonWidth
    JA notSign
    CMP mouseY,SyStart+buttonHeight
    JA notSign               
       
       CMP inputCharCount,0d
       JE notSign
       LEA SI,inputBuffer
       DEC SI
       checkForPlusOrMinus:
            
            CMP [DI],'+'
            JE changeWithMinus
            
            CMP [DI],'-'
            JE changeWithPlus
            
            DEC DI
            CMP SI,DI
            JE noMinusOrPlus
            JMP checkForPlusOrMinus
             
            changeWithMinus:
            MOV [DI],'-'
            RET
            changeWithPlus:
            MOV [DI],'+'  
            RET
            
    noMinusOrPlus: 
    
    CALL addMinusAtTheStartOfBuffer
    RET       
    notSign:
     
    
    CMP mouseX,DIVxStart
    JB notDiv
    CMP mouseY,DIVyStart
    JB notDiv
    CMP mouseX,DIVxStart+buttonWidth
    JA notDiv
    CMP mouseY,DIVyStart+buttonHeight
    JA notDiv
       
       MOV [DI],'/'  
       INC inputCharCount
       RET
    notDiv: 
     
    CMP mouseX,MULxStart
    JB notMul
    CMP mouseY,MULyStart
    JB notMul
    CMP mouseX,MULxStart+buttonWidth
    JA notMul
    CMP mouseY,MULyStart+buttonHeight
    JA notMul
       
       MOV [DI],'*'  
       INC inputCharCount
       RET
    notMul:
    
    CMP mouseX,MINxStart
    JB notMinus
    CMP mouseY,MINyStart
    JB notMinus
    CMP mouseX,MINxStart+buttonWidth
    JA notMinus
    CMP mouseY,MINyStart+buttonHeight
    JA notMinus
       
       MOV [DI],'-'  
       INC inputCharCount
       RET
    notMinus:
    
    CMP mouseX,PLSxStart
    JB notPlus
    CMP mouseY,PLSyStart
    JB notPlus
    CMP mouseX,PLSxStart+buttonWidth
    JA notPlus
    CMP mouseY,PLSyStart+buttonHeight
    JA notPlus
       
       MOV [DI],'+'  
       INC inputCharCount
       RET
    notPlus:  
             
       
    
    CMP mouseX,SQRxStart
    JB notSqrt
    CMP mouseY,SQRyStart
    JB notSqrt
    CMP mouseX,SQRxStart+buttonWidth
    JA notSqrt
    CMP mouseY,SQRyStart+buttonHeight
    JA notSqrt
       
       MOV [DI],SQUAREROOTCHAR  
       INC inputCharCount
       RET
    notSqrt:         
    
            
    CMP mouseX,PRCxStart
    JB notPercent
    CMP mouseY,PRCyStart
    JB notPercent
    CMP mouseX,PRCxStart+buttonWidth
    JA notPercent
    CMP mouseY,PRCyStart+buttonHeight
    JA notPercent
       
 
       
       MOV [DI],'%'  
       INC inputCharCount
       RET  
       
       
       
    notPercent: 
    bufferIsFULL: 
    CMP mouseX,CxStart
    JB notClear
    CMP mouseY,CyStart
    JB notClear
    CMP mouseX,CxStart+buttonWidth
    JA notClear
    CMP mouseY,CyStart+buttonHeight
    JA notClear
       
       CALL clearInputBuffer  
       RET
    notClear:
    
    CMP mouseX,EQxStart
    JB notEquals
    CMP mouseY,EQyStart
    JB notEquals
    CMP mouseX,EQxStart+buttonWidth
    JA notEquals
    CMP mouseY,EQyStart+buttonHeight
    JA notEquals              
       CMP inputCharCount,01d
       JNE calculateOutput 
       CMP inputBuffer,SQUAREROOTCHAR
       JNE notSquareOfOUTPUT
       
       MOV AX,virtualOutputStackInt
       MOV BX,virtualOutputStackInt+2
       MOV numberToFindSquarerootOfInt,AX
       MOV numberToFindSquarerootOfInt+2,BX
       
       MOV AL,virtualOutputStackFloat
       MOV numberToFindSquarerootOfFloat,AL
  
       call squareRoot
       
       MOV AX,outputInt
       MOV virtualOutputStackInt,AX
       MOV AL,outputFloat
       MOV virtualOutputStackFloat,AL
       MOV AL,outputSign
       MOV virtualOutputStackSign,AL 
       
       mov virtualOutputStackNextElementIndex,01h
       call produceResultString 
       call clearInputBuffer
       LEA DI,resultString
       call displayString
       
       RET
       
       notSquareOfOUTPUT:
       CMP inputBuffer,'%'
       JNE calculateOutput
       
       MOV AX,virtualOutputStackInt
       MOV BX,virtualOutputStackInt+2
       MOV operand1Int,AX
       MOV operand1Int+2,BX
       
       MOV AL,virtualOutputStackFloat
       MOV operand1Float,AL
  
       call percentFunc
       
       MOV AX,outputInt
       MOV virtualOutputStackInt,AX
       MOV AL,outputFloat
       MOV virtualOutputStackFloat,AL
       MOV AL,outputSign
       MOV virtualOutputStackSign,AL 
       
       mov virtualOutputStackNextElementIndex,01h
       call produceResultString 
       call clearInputBuffer
       LEA DI,resultString
       call displayString 
       RET
       
       calculateOutput:
       MOV [DI],ENDOFBUFFER 
       CALL resetCalculationBuffers
       call translateInputBuffer 
       call middleBufferToRPNBuffer   
       call solveRPNexpression 
       call produceResultString 
       call clearInputBuffer  
       LEA DI,resultString
       call displayString   
       RET  
       
       
       
       
    notEquals:
  
    CMP mouseX,MCxStart
    JB notMemClear
    CMP mouseY,MCyStart
    JB notMemClear
    CMP mouseX,MCxStart+buttonWidth
    JA notMemClear
    CMP mouseY,MCyStart+buttonHeight
    JA notMemClear
       
       MOV memoryInt,00h 
       MOV memoryInt+2,00h 
       MOV memoryFloat,00h
       MOV memorySign,POSITIVE
         
       RET
    notMemClear:
    
    CMP mouseX,MRxStart
    JB notMemRecall
    CMP mouseY,MRyStart
    JB notMemRecall
    CMP mouseX,MRxStart+buttonWidth
    JA notMemRecall
    CMP mouseY,MRyStart+buttonHeight
    JA notMemRecall
       
       MOV AX,memoryInt
       MOV BX,memoryInt+2
       MOV virtualOutputStackInt,AX
       MOV virtualOutputStackInt+2,BX  
       MOV AL,memoryFloat
       MOV virtualOutputStackFloat,AL
       mov AL,memorySign
       MOV virtualOutputStackSign,AL       
       mov virtualOutputStackNextElementIndex,01h
       call produceResultString 
       call clearInputBuffer
       LEA DI,resultString
       call displayString
       MOV virtualOutputStackSign,POSITIVE  
       MOV virtualOutputStackFloat,00H
       MOV virtualOutputStackInt,00H
       MOV virtualOutputStackInt+2,00h
       mov virtualOutputStackNextElementIndex,00h   
       RET
    notMemRecall:
      
    CMP mouseX,MPxStart
    JB notMemPlus
    CMP mouseY,MPyStart
    JB notMemPlus
    CMP mouseX,MPxStart+buttonWidth
    JA notMemPlus
    CMP mouseY,MPyStart+buttonHeight
    JA notMemPlus
       
       MOV AX,memoryInt
       MOV operand1Int,AX
       MOV AL,memoryFloat
       MOV operand1Float,AL
       MOV AL,memorySign
       MOV operand1Sign,AL
        
       MOV AX,virtualOutputStackInt
       MOV operand2Int,AX
       MOV AL,virtualOutputStackFloat
       MOV operand2Float,AL
       MOV AL,virtualOutputStackSign
       MOV operand2Sign,AL
       
       call newAddOperands
       
       MOV AX,outputInt
       MOV virtualOutputStackInt,AX
       MOV memoryInt,AX
       MOV AL,outputFloat
       MOV virtualOutputStackFloat,AL
       MOV memoryFloat,Al
       MOV AL,outputSign
       MOV virtualOutputStackSign,AL
       MOV memorySign,AL  
       
       mov virtualOutputStackNextElementIndex,01h
       call produceResultString 
       call clearInputBuffer
       LEA DI,resultString
       call displayString
       MOV virtualOutputStackSign,POSITIVE  
       MOV virtualOutputStackFloat,00H
       MOV virtualOutputStackInt,00H
       MOV virtualOutputStackInt+2,00h
       mov virtualOutputStackNextElementIndex,00h
         
       RET
    notMemPlus:
    
    CMP mouseX,MMxStart
    JB notMemMinus
    CMP mouseY,MMyStart
    JB notMemMinus
    CMP mouseX,MMxStart+buttonWidth
    JA notMemMinus
    CMP mouseY,MMyStart+buttonHeight
    JA notMemMinus
       
       MOV AX,memoryInt
       MOV operand1Int,AX
       MOV AL,memoryFloat
       MOV operand1Float,AL
       MOV AL,memorySign
       MOV operand1Sign,AL
        
       MOV AX,virtualOutputStackInt
       MOV operand2Int,AX
       MOV AL,virtualOutputStackFloat
       MOV operand2Float,AL
       MOV AL,virtualOutputStackSign
       MOV operand2Sign,AL
       
       call newSubOperands
       
      MOV AX,outputInt
       MOV virtualOutputStackInt,AX
       MOV memoryInt,AX
       MOV AL,outputFloat
       MOV virtualOutputStackFloat,AL
       MOV memoryFloat,AL
       MOV AL,outputSign
       MOV virtualOutputStackSign,AL
       MOV memorySign,AL 
        
       mov virtualOutputStackNextElementIndex,01h
       call produceResultString 
       call clearInputBuffer
      LEA DI,resultString
       call displayString
       MOV virtualOutputStackSign,POSITIVE  
       MOV virtualOutputStackFloat,00H
       MOV virtualOutputStackInt,00H
       MOV virtualOutputStackInt+2,00h
       mov virtualOutputStackNextElementIndex,00h
         
       RET
    notMemMinus:
    
RET 
mouseClickHandle ENDP 
;--------------------------------------------------  
;clearInputBuffer :clears input Buffer and clears calculator screen
;OUTPUT:
;inputBuffer
clearInputBuffer PROC
     LEA SI,inputBuffer
     LEA DI,inputBuffer
     ADD DI,inputCharCount
     DEC DI
clearCurrentElement:
     
     MOV [DI],00H
     DEC DI
     CMP DI,SI
     JNB clearCurrentElement

     MOV inputCharCount,0h 
     
     fillRectangle 38,40,602,120,0FH  
      
RET    
clearInputBuffer ENDP
;--------------------------------------------------
;addMinusAtTheStartOfBuffer : adds '-' char at the start of input buffer 
;and increases input char count by one
addMinusAtTheStartOfBuffer PROC
     LEA SI,inputBuffer
     DEC SI
     LEA DI,inputBuffer
     ADD DI,inputCharCount 
     DEC DI
shiftLoop:
     MOV AL,[DI]
     MOV [DI+1],AL  
     DEC DI
     CMP SI,DI
     JNE shiftLoop 
     JE addMinusATDI
     RET
     
    addMinusATDI:
     INC DI
     MOV [DI],'-'
     INC inputCharCount
RET
addMinusAtTheStartOfBuffer ENDP
;----------------------------------------------------------------------- 
;fillBackground :Fills background of calculator with cyan color
fillBackground PROC
   MOV DX,479D;start point y coordinate
   verticalLoop2:

   MOV CX,639D;start point x coordinate
   horizontalLoop2:
   MOV AH,0CH
   MOV AL,03H;Color:Cyan
   MOV BX,00H;Page:0
   
   
   INT 10H;Set pixel
   LOOP horizontalLoop2     
   
   DEC DX
   JNZ verticalLoop2
   RET
fillBackground ENDP 
;-----------------------------------------------------------------------
;;printButtons (draws buttons on screen)

printButtons PROC

   MOV AX,@DATA
   MOV DS,AX 
   
   fillRectangle MCxStart,MCyStart,MCxStart+buttonWidth,MCyStart+buttonHeight,0FH
   LEA SI,M_char
   MOV topleftx,MCxStart+2
   MOV toplefty,MCyStart+14
   MOV expansionRate,04d  
   call printCharacter
   
   LEA SI,C_char
   MOV topleftx,MCxStart+34
   MOV toplefty,MCyStart+14
   MOV expansionRate,04d  ;8x4 = 32 ,32 by 32 pixels
   call printCharacter
   
   fillRectangle MRxStart,MRyStart,MRxStart+buttonWidth,MRyStart+buttonHeight,0FH
   LEA SI,M_char
   MOV topleftx,MRxStart+2
   MOV toplefty,MRyStart+14
   MOV expansionRate,04d  
   call printCharacter
   
   LEA SI,R_char
   MOV topleftx,MRxStart+34
   MOV toplefty,MRyStart+14
   call printCharacter 
   
   fillRectangle MPxStart,MPyStart,MPxStart+buttonWidth,MPyStart+buttonHeight,0FH
   LEA SI,M_char
   MOV topleftx,MPxStart+2
   MOV toplefty,MPyStart+14
   MOV expansionRate,04d  
   call printCharacter
   
   LEA SI,P_char
   MOV topleftx,MPxStart+34
   MOV toplefty,MPyStart+14
   call printCharacter
   
   fillRectangle MMxStart,MMyStart,MMxStart+buttonWidth,MMyStart+buttonHeight,0FH 
   LEA SI,M_char
   MOV topleftx,MMxStart+2
   MOV toplefty,MMyStart+14
   MOV expansionRate,04d  
   call printCharacter
   
   LEA SI,M_char
   MOV topleftx,MMxStart+34
   MOV toplefty,MMyStart+14
   call printCharacter
   
   fillRectangle SEVENxStart,SEVENyStart,SEVENxStart+buttonWidth,SEVENyStart+buttonHeight,0FH
   LEA SI,seven
   MOV topleftx,SEVENxStart+19
   MOV toplefty,SEVENyStart+14
   call printCharacter
   fillRectangle FOURxStart,FOURyStart,FOURxStart+buttonWidth,FOURyStart+buttonHeight,0FH 
   LEA SI,four
   MOV topleftx,FOURxStart+19
   MOV toplefty,FOURyStart+14
   call printCharacter
   fillRectangle ONExStart,ONEyStart,ONExStart+buttonWidth,ONEyStart+buttonHeight,0FH  
   LEA SI,one
   MOV topleftx,ONExStart+19
   MOV toplefty,ONEyStart+14
   call printCharacter
   fillRectangle ZEROxStart,ZEROyStart,ZEROxStart+buttonWidth,ZEROyStart+buttonHeight,0FH 
   LEA SI,zero
   MOV topleftx,ZEROxStart+19
   MOV toplefty,ZEROyStart+14
   call printCharacter
   
   fillRectangle EIGHTxStart,EIGHTyStart,EIGHTxStart+buttonWidth,EIGHTyStart+buttonHeight,0FH 
   LEA SI,eight
   MOV topleftx,EIGHTxStart+19
   MOV toplefty,EIGHTyStart+14
   call printCharacter
   
   fillRectangle FIVExStart,FIVEyStart,FIVExStart+buttonWidth,FIVEyStart+buttonHeight,0FH 
   LEA SI,five
   MOV topleftx,FIVExStart+19
   MOV toplefty,FIVEyStart+14
   call printCharacter
   fillRectangle TWOxStart,TWOyStart,TWOxStart+buttonWidth,TWOyStart+buttonHeight,0FH 
   LEA SI,two
   MOV topleftx,TWOxStart+19
   MOV toplefty,TWOyStart+14
   call printCharacter
   fillRectangle DOTxStart,DOTyStart,DOTxStart+buttonWidth,DOTyStart+buttonHeight,0FH   
   LEA SI,dot
   MOV topleftx,DOTxStart+19
   MOV toplefty,DOTyStart+14
   call printCharacter
   
   fillRectangle NINExStart,NINEyStart,NINExStart+buttonWidth,NINEyStart+buttonHeight,0FH
   LEA SI,nine
   MOV topleftx,NINExStart+19
   MOV toplefty,NINEyStart+14
   call printCharacter
   fillRectangle SIXxStart,SIXyStart,SIXxStart+buttonWidth,SIXyStart+buttonHeight,0FH 
   LEA SI,six
   MOV topleftx,SIXxStart+19
   MOV toplefty,SIXyStart+14
   call printCharacter
   fillRectangle THREExStart,THREEyStart,THREExStart+buttonWidth,THREEyStart+buttonHeight,0FH
   LEA SI,three
   MOV topleftx,THREExStart+19
   MOV toplefty,THREEyStart+14
   call printCharacter
   fillRectangle SxStart,SyStart,SxStart+buttonWidth,SyStart+buttonHeight,0FH 
   LEA SI,sign
   MOV topleftx,SxStart+19
   MOV toplefty,SyStart+14
   call printCharacter
   
   fillRectangle DIVxStart,DIVyStart,DIVxStart+buttonWidth,DIVyStart+buttonHeight,0FH
   LEA SI,divide
   MOV topleftx,DIVxStart+19
   MOV toplefty,DIVyStart+14
   call printCharacter
  
   fillRectangle MULxStart,MULyStart,MULxStart+buttonWidth,MULyStart+buttonHeight,0FH 
   LEA SI,multiply
   MOV topleftx,MULxStart+19
   MOV toplefty,MULyStart+14
   call printCharacter
   fillRectangle MINxStart,MINyStart,MINxStart+buttonWidth,MINyStart+buttonHeight,0FH
   LEA SI,minus
   MOV topleftx,MINxStart+19
   MOV toplefty,MINyStart+14
   call printCharacter
   fillRectangle PLSxStart,PLSyStart,PLSxStart+buttonWidth,PLSyStart+buttonHeight,0FH
   LEA SI,plus
   MOV topleftx,PLSxStart+19
   MOV toplefty,PLSyStart+14
   call printCharacter 
   
   fillRectangle SQRxStart,SQRyStart,SQRxStart+buttonWidth,SQRyStart+buttonHeight,0FH
   LEA SI,sqrt
   MOV topleftx,SQRxStart+19
   MOV toplefty,SQRyStart+14
   call printCharacter
   fillRectangle PRCxStart,PRCyStart,PRCxStart+buttonWidth,PRCyStart+buttonHeight,0FH
   LEA SI,percent
   MOV topleftx,PRCxStart+19
   MOV toplefty,PRCyStart+14
   call printCharacter
   fillRectangle CxStart,CyStart,CxStart+buttonWidth,CyStart+buttonHeight,0FH  
   LEA SI,C_char
   MOV topleftx,CxStart+19
   MOV toplefty,CyStart+14
   call printCharacter
   fillRectangle EQxStart,EQyStart,EQxStart+buttonWidth,EQyStart+buttonHeight,0FH 
   LEA SI,equals
   MOV topleftx,EQxStart+19
   MOV toplefty,EQyStart+14
   call printCharacter
   
   RET
printButtons ENDP
;-----------------------------------------------------------------------------------------
;printCharacter : prints character at specified position and size (AX,BX,CX,DX are altered) 
;INPUTS 
;SI:pointer to character
;topLeftX:Starting point Y coordinate
;topLeftY:Starting point X coordinate
;ExpansionRate:expansion ratio of printed character  (char dimensions = 8 * ExpansionRate) 
printCharacter PROC
    
   MOV AX,@DATA
   MOV DS,AX          
              
   ;Calculating borders 
   MOV BX,topLeftX       ;bottomRightX = topLeftX + (ExpansionRate * 8)           
   MOV AX,ExpansionRate  ;                
   MOV CX,8              ;      
   MUL CX                ;      
   ADD BX,AX             ;                 
   MOV [bottomRightX],BX ;              
      
   MOV BX,topLeftY       ;bottomRightY = topLeftY + (ExpansionRate * 8)           
   MOV AX,ExpansionRate  ;                
   MOV CX,8              ;     
   MUL CX                ;      
   ADD BX,AX             ;             
   MOV bottomRightY,BX   ;      
   
              
   MOV DX,topLeftY           
   verticalLoop3:
   
      MOV CX,topLeftX
      MOV BL,[SI];Getting current row pixel data
      horizontalLoop3:  
            SHL BL,1        ;shifting pixels by 1 to check curent pixel
            JNC dontSetPixel;
            
            MOV color,00H ;set color (BLACK)
            JMP setPixel
                          
      dontSetPixel:
            MOV color,0FH ;reset color (WHITE) 
                
      setPixel:          
            MOV xStart,CX ;current X and current Y are 
            MOV yStart,DX ;starting points of expanded pixel  
                
            PUSH DX ;Saving Y counter into stack since following part uses and alters it
               
            ADD CX,expansionRate;xEND = CX + expansionRate
            MOV xEND,CX         ;
    
            ADD DX,expansionRate;yEND = DX + expansionRate
            MOV yEND,DX         ;
               
            CALL fillRect
                
            POP DX ;restoring Y counter 
                       
            MOV AX,bottomRightX;Checking row borders
            CMP CX,AX            ;
            JB horizontalLoop3   ;
            
      ADD DX,expansionRate;
      INC SI                ;increasing SI to get new row data
             
      MOV AX,bottomRightY;;Checking column borders
      CMP DX,AX            ;
      JB verticalLoop3:    ;
RET                
printCharacter ENDP
;------------------------------------------------------------------------------------------
;fillRect (Fills a rectangle on screen with desired boundaries and color)
;INPUTS:
;xStart=X coordinate of starting point 
;yStart=Y coordinate of starting point
;xEnd=X coordinate of ending point 
;yEnd=Y coordinate of ending point
;color=fill color 
fillRect PROC
PUSH CX    
PUSH DX    
 
MOV CX,xStart
MOV DX,yStart

yLOOP:
    MOV CX,xStart
    xLOOP:
         MOV BH,00H;Page:0  
         MOV AL,color;setting desired text color 
         MOV AH,0CH;Set pixel 
         INT 10H
         
         INC CX
         CMP CX,xEND
         JNE xLOOP
    INC DX 
    CMP DX,yEND
    JNE yLOOP

POP DX
POP CX 
RET
fillRect ENDP
;----------------------------------------------------------------------------------------
;handleKeyPress(Filters invalid keypresses,adds valid keypressed to buffer and counts characters)
;INPUTS
;pressedKey:scan code of pressed key 
;OUTPUTS
;inputCharCount:size of input buffer
;DI:pointer to last element of input buffer
;[DI]:scan code of pressed key after discarding invalid inputs
handleKeyPress PROC
    MOV AX,@DATA
    MOV DS,AX
    
    CMP pressedKey,25H
    JE validKey  
    CMP pressedKey,2AH          
    JB invalidKey
    CMP pressedKey,039H          
    JA invalidKey
    CMP inputCharCount,14d 
    JE invalidKey
    validKey:
    MOV AL,pressedKey
    LEA DI,inputBuffer
    ADD DI,inputCharCount
    MOV [DI],AL
      
   INC inputCharCount
   invalidKey:
    
RET       
handleKeyPress ENDP   
;----------------------------------------------------------------------------------------------
;displayInputBuffer (displays input buffer on screen with custom font and size (right aligned))
;INPUTS
;DI:pointer to last element on input buffer
;expansionRate:expansion rate of each character on display 
;OUTPUTS
;topLeftX:x coordinate of top left pixel of leftmost char on display 
displayInputBuffer PROC
        MOV AX,@DATA
        MOV DS,AX 
        MOV topLeftX,560d
        MOV topLeftY,56d
        MOV expansionRate,5d 
        LEA DI,inputBuffer
        ADD DI,inputCharCount
printChar:
        DEC DI
        CMP [DI],30H          
        JE printZero
        CMP [DI],31H          
        JE printOne
        CMP [DI],32H          
        JE printTwo       
        CMP [DI],33H          
        JE printThree
        CMP [DI],34H          
        JE printFour
        CMP [DI],35H          
        JE printFive
        CMP [DI],36H          
        JE printSix
        CMP [DI],37H          
        JE printSeven
        CMP [DI],38H          
        JE printEight   
        CMP [DI],39H          
        JE printNine   
       
        CMP [DI],2AH          
        JE printMul
        CMP [DI],2BH          
        JE printPlus
        CMP [DI],2CH          
        JE printDot
        CMP [DI],2DH          
        JE printMinus   
        CMP [DI],2EH          
        JE printDot
        CMP [DI],2FH          
        JE printDiv
        CMP [DI],'%'          
        JE printPercent
        CMP [DI],SQUAREROOTCHAR          
        JE printSqrt
        
        printZero:
        LEA SI,zero
        JMP print
        printOne:
        LEA SI,one
        JMP print
        printTwo:
        LEA SI,two
        JMP print
        printThree:
        LEA SI,three
        JMP print
        printFour:
        LEA SI,four
        JMP print
        printFive:
        LEA SI,five
        JMP print
        printSix:
        LEA SI,six
        JMP print
        printSeven:
        LEA SI,seven
        JMP print
        printEight:
        LEA SI,eight
        JMP print
        printNine:
        LEA SI,nine
        JMP print 
         
        printPercent: 
        LEA SI,percent 
        JMP print 
        printSqrt: 
        LEA SI,sqrt 
        JMP print 

        printMul:
        LEA SI,multiply
        JMP print
        printPlus:
        LEA SI,plus
        JMP print
        printMinus:
        LEA SI,minus
        JMP print
        printDot:
        LEA SI,dot
        JMP print
        printDiv:
        LEA SI,divide
    print:
    
        CALL printCharacter
        
        MOV AX,8 
        MOV BX,expansionRate
        MUL BX
        SUB topLeftX,AX
        
           
        LEA SI,inputBuffer
        CMP DI,SI
        JNE printChar
                    
RET                       
displayInputBuffer ENDP    
;--------------------------------------------------------------------------------- 
;output:OPERAND1
; 
virtualOutputStackPop PROC 
     MOV AX,@DATA
     MOV DS,AX
     
    
     LEA DI,virtualOutputStackInt
     DEC virtualOutputStackNextElementIndex
     MOV AX,virtualOutputStackNextElementIndex 
     MOV BL,4D
     MUL BL
     ADD DI,AX
     
     MOV AX,[DI]
     MOV operand1Int,AX 
     MOV [DI],0H
     MOV AX,[DI+2]
     MOV operand1Int+2,AX
     MOV [DI],0H
     
     LEA DI,virtualOutputStackFloat
     ADD DI,virtualOutputStackNextElementIndex 
     MOV AL,[DI] 
     MOV operand1Float,AL
     MOV [DI],0H
     
     LEA DI,virtualOutputStackSign
     ADD DI,virtualOutputStackNextElementIndex
     MOV AL,[DI]
     MOV operand1Sign,AL
     MOV [DI],POSITIVE
RET
virtualOutputStackPop ENDP
;----------------------------------------------------------------
;INPUT output variable
;
virtualOutputStackPush PROC 
    MOV AX,@DATA
     MOV DS,AX
    
    LEA DI,virtualOutputStackInt
    MOV AX,virtualOutputStackNextElementIndex 
    MOV BL,4D
    MUL BL
    ADD DI,AX 
     
    MOV AX,outputInt
    MOV [DI],AX
    MOV AX,[outputInt+2
    MOV [DI+2],AX
    
    LEA DI,virtualOutputStackFloat
    ADD DI,virtualOutputStackNextElementIndex
    MOV AL,outputFloat
    MOV [DI],AL
    
    LEA DI,virtualOutputStackSign
    ADD DI,virtualOutputStackNextElementIndex
    MOV AL,outputSign
    MOV [DI],AL
    INC virtualOutputStackNextElementIndex
RET
virtualOutputStackPush ENDP
;---------------------------------------------------------------- 
;solveRPNexpression:Solves RPN expression 
;INPUT
;RPNBufferFloat,RPNBufferInt,RPNBufferSign: contains rpn expression
;OUTPUT
;virtualOutputStack:if rpn expression is valid contains result at first element 
solveRPNexpression PROC
    MOV AX,@DATA
    MOV DS,AX
   
    
    MOV CX,0D
checkElement:
    LEA SI,RPNBufferFloat 
    ADD SI,CX
    CMP [SI],ENDOFBUFFER    
    JE RPNBufferIsEnded
    
    CMP [SI],OPERATOR    
    JE operatorIsFound
    ;Number IS  found
    LEA DI,RPNBufferInt
    MOV AX,CX
    MOV BL,4D
    MUL BL
    ADD DI,AX 
    
    MOV AX,[DI]   
    MOV outputInt,AX
    MOV AX,[DI+2]   
    MOV outputInt+2,AX 
    
    LEA DI,RPNBufferFloat
    ADD DI,CX 
    MOV AL,[DI]
    MOV outputFloat,AL
    
    MOV outputSign,POSITIVE
    
    CALL virtualOutputStackPush
    
    INC CX
    CMP CX,14D
    JNE checkElement
    
    operatorIsFound: 
    
    LEA SI,RPNBufferInt
    MOV AX,CX
    MOV BL,4D
    MUL BL
    ADD SI,AX 
    
    CMP [SI],UNARYMINUS
    JNE notUnaryMinus           
    call virtualOutputStackPop 
    MOV outputSign,NEGATIVE 
    
    MOV AX,operand1Int
    MOV outputInt,AX  
    
    MOV AX,operand1Int+2
    MOV outputInt+2,AX
    
    MOV AL,operand1Float 
    MOV outputFloat,AL 
    
    JMP pushOutput 
    
    notUnaryMinus:
    CMP [SI],UNARYPLUS
    JNE notUnaryPlus 
    call virtualOutputStackPop          
    MOV outputSign,POSITIVE
    
    MOV AX,operand1Int
    MOV outputInt,AX  
    
    MOV AX,operand1Int+2
    MOV outputInt+2,AX
    
    MOV AL,operand1Float 
    MOV outputFloat,AL 
    
    JMP pushOutput
     
    notUnaryPlus: 
    CMP [SI],SQUAREROOTCHAR
    JE squareRootOperand
    CMP [SI],'%'
    JE percentOperand
    
       call  virtualOutputStackPop
       MOV AX,operand1Int
       MOV operand2Int,AX
       MOV AX,operand1Int+2
       MOV operand2Int+2,AX
       MOV AL,operand1Sign
       MOV operand2Sign,AL
       MOV AL,operand1Float
       MOV operand2Float,AL
       call  virtualOutputStackPop
       
       CMP [SI],'+'
       JNE notAdd 
       PUSH CX
       call newAddOperands
       POP CX
       JMP pushOutput
       notAdd:
       
       CMP [SI],'-'
       JNE notSubstract 
       PUSH CX
       call newSubOperands 
       POP CX
       JMP pushOutput
       notSubstract:
       
       CMP [SI],'*'
       JNE notMultiply 
       PUSH CX
       call multiplyOperands 
       POP CX
       JMP pushOutput
       notMultiply:
       
       CMP [SI],'/'
       JNE notDivide  
       PUSH CX
       call divideOperands   
       POP CX
       JMP pushOutput
       notDivide: 
       
          
    squareRootOperand:
       call virtualOutputStackPop   
       MOV AX,operand1Int   
       MOV numberToFindSquarerootOfInt,AX
       MOV AL,operand1Float
       MOV numberToFindSquarerootOfFloat,AL
       PUSH CX
       call squareRoot
       MOV outputSign,POSITIVE
       POP CX
       JMP pushOutput 
       
    percentOperand:
       call virtualOutputStackPop  
       PUSH CX
       call percentFunc
       POP CX 
       JMP pushOutput    
          
          
          
      pushOutput:
       call virtualOutputStackPush      
       INC CX
       CMP CX,14D
       JMP checkElement  
RPNBufferIsEnded:   
    
RET
solveRPNexpression ENDP
;------------------------------------------------------------------ 
;newSubOperands:  Substracts operand1 and operand2  
;INPUT:
;operand1,operand2 (Int,Float,Sign)
;OUTPUT 
;output (Int,Float,Sign)
newSubOperands PROC
    
    MOV AX,@DATA
    MOV DS,AX 
    
    MOV floatsign,POSITIVE
    MOV lowwordSign,POSITIVE  
    MOV AL,operand2Sign
    CMP operand1Sign,AL
    JE substractAsUsual
     
        CMP operand1sign,POSITIVE
        JE operand2IsNegative3
         ;Operand1 is negative  
            MOV operand2Sign,NEGATIVE
            CALL newAddOperands
            RET
        operand2IsNegative3:
            MOV operand2Sign,POSITIVE
            CALL newAddOperands
            RET 
     substractAsUsual:
     mov al,operand1Float
     sub al,operand2Float
     JNS lowwordcalculate
         MOV floatsign,NEGATIVE 
         
     lowwordcalculate:
     MOV outputFloat,AL

     mov aX,operand1Int
     sub aX,operand2Int
     JNS  highwordcalculate
         MOV lowwordsign,NEGATIVE
        
     highwordcalculate:
     MOV outputInt,Ax     
      
     CMP outputInt,00h
     JNE asdf
     
     MOV AL,floatsign
     MOV outputSign,AL
     JMP adjust
     asdf:
     
     MOV AL,lowwordsign
     MOV outputSign,AL

     adjust:
      
     cmp outputSign,POSITIVE
     JNE outputIsNegative12
        CMP floatSign,POSITIVE
        JE floatIsPositive 
        
        ADD outputFloat,100d
        dec outputInt
        
        floatIsPositive:
     
        JMP ENDDD
     outputIsNegative12:
        CMP floatSign,POSITIVE
        JNE floatIsnegative 
        
        SUB outputFloat,100d
        INC outputInt
        
        floatIsnegative:
     
     NEG outputFloat 
     NEG outputInt 
     CMP outputFloat,100d
     JNE ENDDD 
     MOV outputFloat,00h
     INC outputInt
     ENDDD:
         
              
RET    
newSubOperands ENDP
;---------------------------------------------------------------- 
;newAddOperands: Adds operand1 and operand2 
;INPUT:
;operand1,operand2 (Int,Float,Sign)
;OUTPUT 
;output (Int,Float,Sign)
newAddOperands PROC
    MOV AX,@DATA
    MOV DS,AX 
    
    LEA DI,operand1Int
    LEA SI,operand2Int
    
    MOV AL,operand2Sign
    CMP operand1Sign,AL
    JE addAsUsual
        ;If signs are different
        CMP operand1sign,POSITIVE
        JE operand2IsNegative2
            ;Operand1 is negative
            push operand2Int
            push operand1Int
            pop operand2Int
            pop operand1Int
   
            push operand2Int+2
            push operand1Int+2
            pop operand2Int+2
            pop operand1Int+2
            
            MOV AL,operand2Float
            MOV BL,operand1Float
            MOV operand2Float,BL
            MOV operand1Float,AL
            
            MOV operand1Sign,POSITIVE
            
            CALL newSubOperands
            RET 
            
        operand2IsNegative2:
        MOV operand2Sign,POSITIVE
        CALL newSubOperands
        RET
    
    addAsUsual:
    MOV BX,00H
    MOV AL,operand1Float
    ADD AL,operand2Float 
    CMP AL,99D
    JB addInts
        SUB AL,100D 
        MOV BX,01D
    addInts: 
    MOV outputFloat,AL
    MOV AX,[DI]
    ADD AX,[SI]
    ADC AX,BX
    MOV outputInt,AX
    MOV AX,[DI+2]
    ADC AX,[SI+2]
    MOV outputInt+2,AX
    MOV AL,operand2Sign 
    MOV outputSign,AL
RET
newAddOperands ENDP
;-----------------------------------------------------------------------------------
;middleBufferToRPNBuffer:Transforms middlestepbuffer to reverse polish notation
;INPUT:
;middleStepBuffer (Int,Float,Sign)
;OUTPUT:
;RPNBuffer (Int,Float,Sign): Contains expression in  reverse polish notation
;
middleBufferToRPNBuffer PROC
    MOV AX,@DATA
    MOV DS,AX 
    MOV CX,0D  
    
nextToken:         
    LEA SI,middleStepBufferFloat
    ADD SI,CX
    CMP [SI],OPERATOR     
    JE tokenIsOperator     
    CMP [SI],ENDOFBUFFER
    JE bufferIsEnded     
    ;If token is a NUMBER
    LEA DI,RPNBufferFloat  ;copy float part to RPNBuffer
    ADD DI,RPNBufferNextElementIndex;
    MOV AL,[SI]            ;
    MOV [DI],AL            ;
    
    LEA DI,RPNBufferInt    ;Adjust DI to RPNBufferInt
    MOV AX,RPNBufferNextElementIndex;
    MOV BL,04D             ;
    MUL BL                 ;
    ADD DI,AX              ;
    
    LEA SI,middleStepBufferInt;Adjust SI to middleStepBufferInt
    MOV AX,CX                 ;
    MOV BL,04D                ;
    MUL BL                    ;
    ADD SI,AX                 ;
     
    MOV AX,[SI]     ;copy integer part to RPNBuffer
    MOV [DI],AX     ;
    
    INC RPNBufferNextElementIndex
    INC CX
    JMP nextToken
         
    tokenIsOperator:
    LEA DI,virtualOperatorStack
    LEA SI,middleStepBufferInt;Adjust SI to middleStepBufferInt
    MOV AX,CX                 ;
    MOV BL,04D                ;
    MUL BL                    ;
    ADD SI,AX                 ;
    MOV AL,[SI]
    MOV incomingOperator,AL 
    
    checkElementsOfStack:
    CMP virtualStackNextElementIndex,00H;Check if operator stack is empty
    JNE stackContainsElements
    ;operator stack is empty   
        CMP RPNBufferNextElementIndex,00H
        JNE pushIncomingOperatorToStack  
        
        
        CMP incomingOperator,'-'
        JE unaryMinusDetected
        
        CMP incomingOperator,'+'
        JE unaryPlusDetected
        
        JMP pushIncomingOperatorToStack
        
        unaryMinusDetected:
        MOV AL,UNARYMINUS
        JMP pushAlTostck
        
        unaryPlusDetected:
        MOV AL,UNARYPLUS
        JMP pushAlTostck
        
        pushIncomingOperatorToStack:
        MOV AL,incomingOperator
        pushAlTostck:
        CALL pushALtoVirtualStack
        INC CX
        JMP nextToken
    stackContainsElements:
   
        CMP incomingOperator,SQUAREROOTCHAR ;Checking incoming operator for precedence level
        JE higherPrecedence        
        CMP incomingOperator,'/'            
        JE precedenceLv4        
        CMP incomingOperator,'*'            
        JE precedenceLv4        
        CMP incomingOperator,'%'            
        JE precedenceLv4        
        CMP incomingOperator,'+'            
        JE precedenceLv2        
        CMP incomingOperator,'-'            
        JE precedenceLv2        

        precedenceLv4:
            
            CMP elementOnTopOfStack,'+'
            JE higherPrecedence 
            CMP elementOnTopOfStack,'-'
            JE higherPrecedence
            
            CMP elementOnTopOfStack,'*'
            JE lowerOrEqualPrecedence 
            CMP elementOnTopOfStack,'%'
            JE lowerOrEqualPrecedence
            CMP elementOnTopOfStack,'/'
            JE lowerOrEqualPrecedence
            CMP elementOnTopOfStack,UNARYMINUS ;
            JE lowerOrEqualPrecedence          ;
            CMP elementOnTopOfStack,UNARYPLUS  ;
            JE lowerOrEqualPrecedence          ;
         

        precedenceLv2: 
            LEA DI,middleStepBufferFloat;
            ADD DI,CX
            DEC DI                      ;
            CMP [DI],OPERATOR           ;         
            JNE lowerOrEqualPrecedence  ;
            ;at this point incoming operator is unary
            CMP incomingOperator,'-' 
            JE unaryMinus
            
            MOV incomingOperator,UNARYPLUS 
            JMP higherPrecedence
            unaryMinus:
            MOV incomingOperator,UNARYMINUS       
           
                       
        higherPrecedence:
            MOV AL,incomingOperator
            CALL pushALtoVirtualStack
            INC CX
            JMP nextToken
            
        lowerOrEqualPrecedence:    
            CALL popBLfromVirtualStack    
            LEA DI,RPNBufferInt
            MOV AX,RPNBufferNextElementIndex;
            MOV BH,04D                      ;
            MUL BH                          ;
            ADD DI,AX                       ;
            MOV [DI],BL                     ;
            
            LEA DI,RPNBufferFloat
            ADD DI,RPNBufferNextElementIndex
            MOV [DI],OPERATOR
            
            INC RPNBufferNextElementIndex   ;
            JMP checkElementsOfStack        ;
        bufferIsEnded: 
            CMP virtualStackNextElementIndex,0H
            JE virtualStackIsEmpty
            CALL popBLfromVirtualStack
            LEA DI,RPNBufferInt  
            MOV AL,04H
            MUL RPNBufferNextElementIndex
            ADD DI,AX
            MOV [DI],BL 
                                             
            LEA DI,RPNBufferFloat
            ADD DI,RPNBufferNextElementIndex
            MOV [DI],OPERATOR
            
            INC RPNBufferNextElementIndex
            JMP bufferIsEnded
            virtualStackIsEmpty:
            LEA DI,RPNBufferFloat  
            ADD DI,RPNBufferNextElementIndex
            MOV BL,ENDOFBUFFER
            MOV [DI],BL
        
RET
middleBufferToRPNBuffer ENDP
;-----------------------------------------------------------------  
;popBLfromVirtualStack : pops value on top of the virtualOperatorStack into BL
;OUTPUT:
;BL:contains value on top of the stack into BL
popBLfromVirtualStack PROC
   DEC virtualStackNextElementIndex
   LEA DI,virtualOperatorStack 
   MOV DX,virtualStackNextElementIndex
   ADD DI,DX
   MOV BL,[DI]
   MOV [DI],00H
   DEC DI
   MOV AX,[DI]
   MOV elementOnTopOfStack,AX
RET
popBLfromVirtualStack ENDP
;------------------------------------ 
;pushALtoVirtualStack : push AL into the virtualOperatorStack
;INPUT:
;AL:value to be pushed into virtualOperatorStack
pushALtoVirtualStack PROC
   LEA DI,virtualOperatorStack
   MOV DX,virtualStackNextElementIndex
   ADD DI,DX
   MOV AH,0H
   MOV [DI],AL
   MOV elementOnTopOfStack,AX
   INC virtualStackNextElementIndex
RET
pushALToVirtualStack ENDP
;------------------------------------
;translateInputBuffer:Translates input buffer which contains ascii characters into 
;integer,float numbers and operators
;INPUT:
;inputBuffer
;OUTPUT:
;middleStepBuffer (Int,Float,Sign)
translateInputBuffer PROC
    MOV AX,@DATA
    MOV DS,AX
      
    MOV DI,offset inputBuffer
    MOV DX,DI
    ADD DX,14D
    MOV CX,00H
getNextChar:
    CMP [DI],ENDOFBUFFER
    JE incCx
    CMP [DI],2FH
    JA numbers
    
    CMP [DI],2CH
    JE decimalPoint
    CMP [DI],2EH
    JE decimalPoint     
    
     
    operators:                 
        MOV afterDotFlag,FALSE       ;after dot flag is reset       
        LEA SI,middleStepBufferInt 
        MOV AX,4D
        MUL CL
        ADD SI,AX
        MOV AL,[DI]
        MOV [SI],AL
        
        LEA SI,middleStepBufferFloat;Since FFH is not a valid number for float part (since  >99d) 
        ADD SI,CX                   ;it will be used as indicator of operators  
        MOV [SI],OPERATOR           ;in next step of parsing

    JMP next
    
    numbers:
          CMP afterDotFlag,TRUE
        JE float
        LEA SI,middleStepBufferInt;Adjusting SI to new char (INT)
        MOV AX,4D                 ;
        MUL CL                    ;
        ADD SI,AX
        JMP calculate1            ;               
        float:
        LEA SI,middleStepBufferFloat;Adjusting SI to new char (FLOAT)
        ADD SI,CX 
        CMP [DI-1],'.'
        JNE DENEME1234
        
        MOV AX,[DI]
        SUB AX,30H 
        MOV BL,10D
        MUL BL
   
        
        JMP ASDFG
        DENEME1234:
        MOV AX,[DI]
        SUB AX,30H 
        JMP ASDFG
        
        calculate1:        
        
        MOV AX,[SI];               
        MOV BL,10D ;               
        MUL BL     ;
        MOV [SI],AX;[SI]=[SI]x10 
        
        MOV AL,[DI];Adding new number to old value
        SUB AL,30H ;
        ASDFG:
        ADD [SI],AL; 
        
        
        
        INC DI         ;Checking if next char is number 
        CMP DI,DX      ;and checking end of input
        JE incCX       ;if next char is number ,middle step buffer index is not changed
        CMP [DI],2FH   ;        but input buffer index is increased
        JA getNextChar ;if not 
        MOV afterDotFlag,FALSE;after dot flag is reset
        DEC DI                ;Both middle step buffer  and input buffer indexes increased       
        JMP next            
      
    decimalPoint:
        MOV afterDotFlag,TRUE;after dot flag is set
        DEC CX ;middle step buffer index is decreased by one
        DEC CX
    next:
    INC DI
    INC CX
    CMP DI,DX     
    JB getNextChar
    JMP markEndOfBuffer
    incCX:
    INC CX  
    markEndOfBuffer:
    LEA SI,middleStepBufferFloat;Adjusting SI to new char (FLOAT)
    ADD SI,CX
    MOV [SI],ENDOFBUFFER
 
RET    
translateInputBuffer ENDP
;---------------------------------------------------------------------------
;divideOperands
;CAUTION:If result exceeds FFFFH integer value this function causes a divide overflow error  
;INPUTS
;operand1Int,operand2Int,operand1Float,operand2float,operand1Sign,operand2Sign
;(Operand1Int and operand2Int can be max. 028FFFFFH and  28FH (with float = 0) respectively)
;(float parts of operands cant exceed 99d(thus operands can have 2 digits after decimal point))
;OUTPUTS
;outputInt,outputFloat,outputSign
;(outputInt cant exceed FFFFH)
divideOperands PROC
     MOV AX,@DATA
     MOV DS,AX
     
     MOV AX,operand1Int+2  ;Getting high-word of dividend
     MOV BX,100D           ;Multiplying it by 100 and storing it in CX
     MUL BX                ;(Assuming it doesnt overflow 16 bits)
     MOV CX,AX
      
     MOV AX,operand1Int  ;Getting low-word of dividend
     MOV BX,100D         ;Multiplying it by 10 
     MUL BX              ;
     
     ADD DX,CX           ;Adding high-wordx10 and high word part of low-wordx10
                         ;AAAA,BBBB =>> dividend
                         ;CX = AAAAx10
                         ;DX = BBBBx10(high-word part)
                         ;AX = BBBBx10(low-word part) 
                         
     MOV BL,operand1Float;Adding float part of dividend as integer  
     MOV BH,0D           ;Thus multiplying it by 100 basicaly 
     ADD AX,BX           ;0,AB x 100 = AB
         
     ADC DX,0H           ;Adjusting high-word of dividend in case of carry from previous addition
      
    ;At this point DX:AX contains operand1 multiplied by 100
     PUSH AX             ;Storing dividend in stack
     PUSH DX             ;  
     
     MOV AX,operand2Int  ;Getting Low-word of divisor  
     MOV BX,100D         ;Multiplying it by 100
     MUL BX              ;(Assuming it doesnt overflow 16 bits)
     
     MOV BL,operand2Float;Adding float part of divisor as integer 
     MOV BH,0D
     ADD AX,BX
     ;At this point AX contains operand2 multiplied by 100 (discarding overflows from 16 bits)
     MOV divisor,AX
     
     POP DX              ;Retrieving dividend from stack
     POP AX              ;
     
     DIV divisor         ;dividend/divisor
     MOV outputInt,AX    ;quotient=>outputInt
     mov remainder,dx    ;remainder=>remainder
                          
     MOV AX,remainder    ;(remainderx10)/divisor
     MOV BX,10D          ;remainder=>remainder
     MUL BX              ;quotientx10=>outputFloat
     DIV divisor         ;
     MOV remainder,DX    ;
     MUL BX              ;
     MOV outputFloat,AL  ;
     
     
     MOV AX,remainder    ;(remainderx10)/divisor
     MOV BX,10D          ;outputFloat=outputFloat+quotient
     MUL BX              ;remainder=>remainder
     DIV divisor         ;
     ADD outputFloat,AL  ; 
     MOV remainder,DX    ;
                          
                          
     MOV AX,remainder     ;Checking for additional place after decimal point 
     MOV BX,10D           ;And using that to round second place after decimal
     MUL BX               ;
     DIV divisor          ;
     CMP AX,05H           ;
     JB dontAddOneToFloat:; 
     
     INC outputFloat
     
     dontAddOneToFloat: 
     MOV AL,operand2Sign    ;Finding output sign
     CMP operand1Sign,AL    ;
     JNE outputIsNegative2  ;
     MOV outputSign,POSITIVE;   
     RET                    ;
     outputIsNegative2:     ;
     MOV outputSign,NEGATIVE;  
     
RET
divideOperands ENDP
;---------------------------------------------------------------
;multiplyOperands:Multiplies operand 1 and operand 2 
;INPUT:
;operand1 and operand2 (Int,Float,Sign)
;OUPUT
;output (Int,Float,Sign)
multiplyOperands PROC
    MOV AX,@DATA
    MOV DS,AX
    
    MOV outputInt,0h
    MOV outputFloat,0H
    
    MOV AH,0D
    MOV AL,operand1Float
    
    MUL operand2Float
    MOV BL,100D 
    DIV BL
    MOV outputFloat,AL
    
    MOV AX,operand1Int
    MUL operand2Int 
    
    LEA SI,outputInt
    MOV [SI+2],DX
    ADD outputInt,AX
    
    MOV AX,operand1Int
    MOV BH,0D
    MOV BL,operand2Float
    MUL BX
    
    MOV highWord,DX
    
    MOV BX,100D
    DIV BX  
    ADD outputInt,AX
    ADD outputFloat,DL
    CMP outputFloat,100D
    JB dontAdjust 
    
    INC outputInt
    MOV BL,100D
    SUB outputFloat,BL 
    
    dontAdjust:
    
    MOV AX,operand2Int
    MOV BH,0D
    MOV BL,operand1Float
    MUL BX
    
    ADD highWord,DX
    
    MOV BX,100D
    DIV BX  
    ADD outputInt,AX
    ADD outputFloat,DL
    CMP outputFloat,100D
    JB dontAdjust2 
    
    INC outputInt
    MOV BL,100D
    SUB outputFloat,BL 
    
    dontAdjust2:
    
    
     MOV AL,operand2Sign
     CMP operand1Sign,AL
     JNE outputIsNegative3
     MOV outputSign,POSITIVE   
     RET
     outputIsNegative3:
     MOV outputSign,NEGATIVE
    
RET
multiplyOperands ENDP
;--------------------------------------------------------------- 
;squareRoot:Uses babylonian method to find squareRoot of a number
;INPUT:
;numberToFindSquarerootOf (Int,Float)
;OUTPUT:
;output (Int,Float,Sign)
squareRoot PROC
    MOV AX,@DATA
    MOV DS,AX
    MOV previousGuessFloat,00D
    
    CMP numberToFindSquarerootOfInt,700D
    JB setInitialGuess20
    CMP numberToFindSquarerootOfInt,1500D
    JB setInitialGuess100   
    CMP numberToFindSquarerootOfInt+2,1D
    JB setInitialGuess170 
    CMP numberToFindSquarerootOfInt+2,3D
    JB setInitialGuess300
    CMP numberToFindSquarerootOfInt+2,7D
    JB setInitialGuess500
    
    MOV previousGuessInt,654D
    JMP guessAgain
     
    setInitialGuess20: 
    MOV previousGuessInt,20D
    JMP guessAgain
    setInitialGuess100: 
    MOV previousGuessInt,100D
    JMP guessAgain
    setInitialGuess170: 
    MOV previousGuessInt,170D 
    JMP guessAgain
    setInitialGuess300: 
    MOV previousGuessInt,300D
    JMP guessAgain 
    setInitialGuess500: 
    MOV previousGuessInt,500D  
     
     
guessAgain:
    MOV operand1Int+2,0H
    
    MOV AX,numberToFindSquarerootOfInt
    MOV operand1Int,AX 
    MOV AL,numberToFindSquarerootOfFloat   
    MOV operand1Float,AL
 
    MOV AX,previousGuessInt
    MOV operand2Int,AX 
    MOV AL,previousGuessFloat   
    MOV operand2Float,AL
     
    call divideOperands   
    
    MOV AX,previousGuessInt
    MOV operand1Int,AX 
    MOV AL,previousGuessFloat   
    MOV operand1Float,AL
    
    MOV AX,outputInt
    MOV operand2Int,AX 
    MOV AL,outputFloat   
    MOV operand2Float,AL 
    
    
    MOV operand1Sign,POSITIVE
    MOV operand2Sign,POSITIVE
    call newaddOperands
    
    MOV AX,outputInt
    MOV operand1Int,AX 
    MOV AL,outputFloat   
    MOV operand1Float,AL
    
    MOV AX,2D
    MOV operand2Int,AX 
    MOV AL,00D   
    MOV operand2Float,AL
    
    call divideOperands
    
    MOV AX,outputInt
    CMP previousGuessInt,AX
    JNE changePreviousGuess  
    MOV AL,outputFloat
    CMP previousGuessFloat,AL
    JNE changePreviousGuess
    JMP return2
    changePreviousGuess:
    MOV AX,outputInt
    MOV previousGuessInt,AX
    MOV AL,outputFloat
    MOV previousGuessFloat,AL
    JMP guessAgain 
    
return2:
RET
squareRoot ENDP
;--------------------------------------------------------------- 
;percentFunc:Performs percent operation on a number
;INPUT
;operand1 (Int,Float,Sign) 
;OUTPUT
;output (Int,Float,Sign) 
percentFunc PROC
    MOV operand2Int,100D 
    MOV operand2Float,0D
    MOV operand2Sign,POSITIVE
    CALL divideOperands
RET    
percentFunc ENDP    
;------------------------------------------------------------------
END main