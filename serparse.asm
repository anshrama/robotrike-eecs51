        NAME    SERPARSE

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                 SERPARSE                                   ;
;                          Serial Parser Routines                            ;
;                                  EE/CS 51                                  ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This file contains the functions for serial parsing-related functions of the robot
; for communication between the remote and target boards. It contains a Mealy state
; machine for possible values through serial and the actions that should be taken in
; conjunction with the motor functions depending on the states/transitions.
;
;    InitSerialParser: Initializes Mealy state machine
;    ResetSerialParser: Reset shared variables used in serial parser
;    ParseSerialChar: Parses a given serial character
;    GetTokenInfo: Given character, gets token type and value
;    DoAddDigitAction: Add digit to shared curr_arg variable
;    DoSetSignVarAction: Set sign_flag shared variable
;    DoSetAbsSpeedAction: Changes absolute speed of Robotrike (using SetMotorSpeed)
;    DoSetRelSpeedAction: Changes relative speed of Robotrike (using SetMotorSpeed)
;    DoSetDirAction: Changes relative direction of Robotrike (using SetMotorSpeed)
;    DoSetLaserOnAction: Changes laser state of RoboTrike on (using SetLaser)
;    DoSetLaserOffAction: Changes laser state of RoboTrike off (using SetLaser)
;    DoSetAbsTurretAngleAction: Changes abs turret angle (w/SetTurretAngle)
;    DoSetRelTurretAngleAction: Changes rel turret angle (w/SetRelTurretAngle)
;    DoSetTurretElevAction: Changes turret elevation (using SetTurretElevation)
;    DoNothingAction: does nothing (used as transition with no action associated)
;    DoErrorFoundAction: sets the error_flag shared variable
;
; Tables for Mealy state machine:
;    
;    TokenValueTable: token values table
;    TokenTypeTable: token types table
;    StateTable: transitions table
;
; Revision History:
;       11/21/2015     Anshul Ramachandran     initial rev, func specs./pseudocodes
;       11/28/2015     Anshul Ramachandran     assembly code


$INCLUDE(serparse.inc)
$INCLUDE(motors.inc)


CGROUP  GROUP   CODE
DGROUP  GROUP   DATA

CODE    SEGMENT PUBLIC 'CODE'
        ASSUME  CS:CGROUP, DS:DATA

; External references
    EXTRN   SetMotorSpeed:NEAR
    EXTRN   SetLaser:NEAR
    EXTRN   SetTurretAngle:NEAR
    EXTRN   SetRelTurretAngle:NEAR
    EXTRN   SetTurretElevation:NEAR
    EXTRN   GetMotorSpeed:NEAR
    EXTRN   GetMotorDirection:NEAR

; InitSerialParser
;
; Description:      This function resets the state of the Meady state machine so the
;                   initial state (ST_INIT) and calls supporting function to reset
;                   shared variables.
;
; Operation:        Sets the state of the parser (parser_state) to the initial state 
;                   (INIT_ST) and calls ResetSerialParser, which resets the other
;                   shared variables.
;
; Arguments:        None.
;
; Return Value:     None.
;
; Local Variables:  None.
; Shared Variables: parser_state (write), sign_flag/curr_arg/error_flag (write in
;                   call to ResetSerialParser)
;
; Global Variables: None.
;
; Input:            None.
; Output:           None.
;
; Error Handling:   None.
;
; Algorithms:       None.
; Data Structures:  None.
;
; Registers Changed:   None.
;
; Known Bugs:       None.
; Limitations:      None.
;
; Revision History:
;       11/21/2015      Anshul Ramachandran     func spec and pseudocode
;       11/28/2015      Anshul Ramachandran     assembly code


InitSerialParser      PROC    NEAR
                      PUBLIC  InitSerialParser

    MOV parser_state, ST_INIT
    CALL ResetSerialParser
    RET

InitSerialParser      ENDP


; ResetSerialParser
;
; Description:      This function resets the shared variables used in the serial
;                   parsers.
;
; Operation:        Reset sign_flag to POSITIVE, curr_arg to INIT_ARG (0), 
;                   error_flag to NO_PARSE_ERROR (0)
;
; Arguments:        None.
;
; Return Value:     None.
;
; Local Variables:  None.
; Shared Variables: sign_flag (write), curr_arg (write), error_flag (write)
;
; Global Variables: None.
;
; Input:            None.
; Output:           None.
;
; Error Handling:   None.
;
; Algorithms:       None.
; Data Structures:  None.
;
; Registers Changed:   None.
;
; Known Bugs:       None.
; Limitations:      None.
;
; Revision History:
;       11/21/2015      Anshul Ramachandran     func spec and pseudocode
;       11/28/2015      Anshul Ramachandran     assembly code


ResetSerialParser      PROC    NEAR
                       PUBLIC  ResetSerialParser

    MOV error_flag, NO_PARSE_ERROR
    MOV sign_flag, POSITIVE
    MOV curr_arg, INIT_ARG
    RET

ResetSerialParser      ENDP


; ParseSerialChar
;
; Description:      This function is passed a character c which is processed as a 
;                   serial command and passed in by value in AL. The function
;                   returns the status of the parsing operation in AX. NO_PARSE_ERROR
;                   (0) is returned if there are no parsing errors and PARSE_ERROR (1)
;                   is returned if there is a parsing error due to the passed char.
;
; Operation:        This function takes the passed-in character, finds its token info
;                   (type and value) by calling GetTokenInfo. It uses this information
;                   and curr_state to get the next state transition (looking
;                   at the table starting at current token * NUM_TOKENS). It calls
;                   the action specified by the transition and updates the curr_state
;                   shared variable to the next state. We then check the error_flag 
;                   shared variable to find what value to put in AL to output. Then,
;                   if we have reached back to ST_INIT (the new parser_state is 
;                   ST_INIT) we reset all of the shared variables (curr_arg, 
;                   sign_flag, and error_flag). If we did not go back to ST_INIT but
;                   we have the error_flag set, then error could have happened in the
;                   middle of an action (such as DoAddDigitAction), so we set the
;                   parser_state to ST_ERROR so that the rest of the characters 
;                   before the carriage return give an error as well. In any case,
;                   we then restore registers and return.
;
; Arguments:        Al = character c to be parsed
;
; Return Value:     AX = NO_PARSE_ERROR/PARSE_ERROR depending on whether error occurs
;                        in parsing c
;
; Local Variables:  None.
; Shared Variables: parser_state (read/write), error_flag (read), curr_arg/sign_flag/
;                   error_flag (write if call to ResetSerialParser)
;
; Global Variables: None.
;
; Input:            None.
; Output:           None.
;
; Error Handling:   PARSE_ERROR is returned if parsing error occurs
;
; Algorithms:       None.
; Data Structures:  None.
;
; Registers Changed:   AX
;
; Known Bugs:       None.
; Limitations:      None.
;
; Revision History:
;       11/21/2015      Anshul Ramachandran     func spec and pseudocode
;       11/28/2015      Anshul Ramachandran     assembly code


ParseSerialChar      PROC    NEAR
                     PUBLIC  ParseSerialChar

SaveRegistersParseSerialChar:
    PUSH BX                               ; Save registers
    PUSH CX
    
CallGetTokenInfo:
    CALL GetTokenInfo                     ; Get the token info of the character passed
                                          ; (token type in AH and token value in AL)
    MOV CH, AH                            ; save token type in CH
    MOV CL, AL                            ; and token value in CL

GetTransitionEntry:
    MOV AL, NUM_TOKENS                    ; Get the beginning of the set of transition
    MUL parser_state                      ; entries for the current parser_state and
    ADD AL, CH                            ; offset by the token type to get the 
                                          ; transition we want

GetTransitionTableLookupIndex:
    ADC AH, 0                             ; Carry over the carry just in case for IMUL
    IMUL BX, AX, SIZE TRANS_ENTRY         ; Convert trans offset to table lookup index

CallTransitionEntryAction:
    MOV AL, CL                            ; Restore token value in AL
    CALL CS:StateTable[BX].ACTION         ; Use table lookup to perform action for
                                          ; transition specified by the index in BX

GoToNextState:
    MOV CH, CS:StateTable[BX].NEXT_STATE  ; Get the next state and update the 
    MOV parser_state, CH                  ; parser_state shared variable

SetErrorStatus:
    MOV AX, 0                             ; Depending on current value of error_flag,
    MOV AL, error_flag                    ; set the return value in AL

CheckIfResetToInitState:
    CMP parser_state, ST_INIT             ; If the new state is ST_INIT, we just had
                                          ; a carriage return (or stayed in ST_INIT)
                                          ; Either way we want to reset all of the
                                          ; shared variables since we are restarting
                                          ; with a brand new command
    JE ResetBeforeSendingErrorStatus      ; If ST_INIT, reset serial parser
    CMP error_flag, PARSE_ERROR           ; In overflow cases in add digit, etc, we
                                          ; might set the error_flag but not actually
                                          ; go to ST_ERROR in the state machine (for
                                          ; digit parsing commands, the function goes
                                          ; back to the digit state even if overflow
                                          ; error), so to take care of these cases
                                          ; that don’t go to ST_ERROR or reset at
                                          ; ST_INIT
    JE SendToErrorState                   ; Send to ST_ERROR if error_flag is set
    JMP RestoreRegistersParseSerialChar   ; If we aren’t resetting back to ST_INIT
                                          ; nor are we sending to ST_ERROR, we just
                                          ; restore registers and return

ResetBeforeSendingErrorStatus:
    CALL ResetSerialParser                ; Reset if came back to ST_INIT
    JMP RestoreRegistersParseSerialChar

SendToErrorState:
    MOV parser_state, ST_ERROR            ; If we got an error at any point (set by
                                          ; error_flag) go to ST_ERROR
    ;JMP RestoreRegistersParseSerialChar

RestoreRegistersParseSerialChar:
    POP CX                                ; Restore registers and return
    POP BX
    
    RET

ParseSerialChar      ENDP


; GetTokenInfo
;
; Description:      This function returns the token type and token value for the
;                   character (passed in AL) to AH and AL respectively. The character
;                   is truncated to 7 bits (since tokens run from 0-127)
;
; Operation:        Truncate the passed in character using TOKEN_MASK, and use XLAT
;                   to search the TokenTypeTable and TokenValueTable for the values
;                   corresponding to the character, and put those values in AH and AL
;                   respectively.
;
; Arguments:        AL = Character to get token info for
;
; Return Value:     AH = Token type for passed in character
;                   AL = Token value for passed in character
;
; Local Variables:  None.
; Shared Variables: None.
;
; Global Variables: None.
;
; Input:            None.
; Output:           None.
;
; Error Handling:   None.
;
; Algorithms:       None.
; Data Structures:  None.
;
; Registers Changed:   AX
;
; Known Bugs:       None.
; Limitations:      None.
;
; Revision History:
;       11/21/2015      Anshul Ramachandran     func spec and pseudocode
;       11/28/2015      Anshul Ramachandran     assembly code


GetTokenInfo      PROC    NEAR
                  PUBLIC  GetTokenInfo
    
SaveRegistersGetTokenInfo:
    PUSH BX                               ; Save registers

TruncateCharacter:
    AND AL, TOKEN_MASK                    ; Truncate to 7 bits since tokens are 
                                          ; between 0 and 127
    MOV AH, AL                            ; Duplicate character in AH

GetTokenType:
    MOV BX, OFFSET(TokenTypeTable)        ; Get token table to reference to
    XLAT CS:TokenTypeTable                ; XLAT to get the token type in AL
    XCHG AH, AL                           ; Swap AH and AL so AH has token type and
                                          ; AL has the copy of the character

GetTokenValue:
    MOV BX, OFFSET(TokenValueTable)       ; Get the token value table to reference to
    XLAT CS:TokenValueTable               ; XLAT to get the token value in AL, now we
                                          ; have token type and value in the right
                                          ; registers

RestoreRegistersGetTokenInfo:
    POP BX                                ; Restore registers and return

    RET

GetTokenInfo      ENDP


; DoAddDigitAction
;
; Description:      This function adds a digit (AL) to the shared curr_arg var. The
;                   sign of 
;
; Operation:        The function multiplies the curr_arg by 10, setting the
;                   error_flag if there is an overflow. Given there is no overflow,
;                   check whether the sign_flag is set or not. If it is set to
;                   NEGATIVE, then we want to subtract the digit passed in from
;                   the negative of `0*curr_arg (because curr_arg is a negative int
;                   if the sign_flag was initially set). If there is an overflow,
;                   the error_flag is set and the function returns. If the 
;                   sign_flag was set to POSITIVE, then we want to add the digit 
;                   passed in to 10*curr_arg. If there is an overflow, then
;                   error_flag is set and the function returns. If there was no 
;                   error up to this point, we update the curr_arg shared variable
;                   and return.
;
; Arguments:        AL = Digit to add to current curr_arg value
;
; Return Value:     None.
;
; Local Variables:  None.
; Shared Variables: curr_arg (read/write), sign_flag (read), error_flag (write)
;
; Global Variables: None.
;
; Input:            None.
; Output:           None.
;
; Error Handling:   Sets error_flag if overflow on multiplication, addition, or
;                   subtractions occurs
;
; Algorithms:       None.
; Data Structures:  None.
;
; Registers Changed:   None.
;
; Known Bugs:       None.
; Limitations:      None.
;
; Revision History:
;       11/21/2015      Anshul Ramachandran     func spec and pseudocode
;       11/28/2015      Anshul Ramachandran     assembly code


DoAddDigitAction      PROC    NEAR

SaveRegistersDoAddDigitAction:
    PUSHA                                 ; Save registers

SaveDigitValue:
    MOV BX, 0                             ; Clear BX so that we can save the digit
    MOV BL, AL                            ; value in BL

GetCurrentArgValue:
    MOV AX, curr_arg                      ; Get the current total value

MultiplyArgValueBy10:
    IMUL AX, 10                           ; and multiply by 10 because we want to get
                                          ; old value * 10 + digit
    JO DigitOverflowError                 ; If there was an overflow, raise a parse
    ;JNO CheckAddSubtractDigit            ; error otherwise continue

CheckAddSubtractDigit:
    CMP sign_flag, NEGATIVE               ; Check if value is positive or negative
    JE DoSubtractDigit                    ; If negative, we want to subtract new digit
    ;JNE DoAddDigit                       ; Otherwise we want to add new digit

DoAddDigit:
    ADD AX, BX                            ; Add new digit in BX to 10*old value in AX
    JO DigitOverflowError                 ; If we have overflowed the register, raise
    MOV curr_arg, AX                      ; a parse error otherwise update curr_arg,
    JMP RestoreRegistersDoAddDigitAction  ; go to restoring registers and returning
                                          

DoSubtractDigit:
    SUB AX, BX                            ; Sub new digit in BX to 10*old value in AX
    JO DigitOverflowError                 ; If we have overflowed the register, raise
    MOV curr_arg, AX                      ; a parse error otherwise update curr_arg,
    JMP RestoreRegistersDoAddDigitAction  ; go to restoring registers and returning
                                          

DigitOverflowError:
    MOV error_flag, PARSE_ERROR           ; If an overflow error happened at some 
                                          ; point, set the error_flag and return

RestoreRegistersDoAddDigitAction:
    POPA                                  ; Restore registers and return
    
    RET

DoAddDigitAction      ENDP


; DoSetSignVarAction
;
; Description:      This function sets the sign_flag shared variable depending on
;                   input sign passed into AL. The error_flag is set if the command 
;                   is ’S’, since we cannot set the abs speed to a negative value.
;
; Operation:        This function first checks if the sign of the value passed in AL
;                   is negative. If the command is further the S command (which
;                   means parser_state is ST_ABSSPEED), then the error_flag is
;                   set and the function returns. If not, the sign_flag is set to
;                   NEGATIVE (-1) and the function returns. If the sign_flag was
;                   POSITIVE, the function returns because the default sign_flag is
;                   POSITIVE.
;
; Arguments:        AL = passed-in sign
;
; Return Value:     None.
;
; Local Variables:  None.
; Shared Variables: sign_flag (write), parser_state (read), error_flag (write)
;
; Global Variables: None.
;
; Input:            None.
; Output:           None.
;
; Error Handling:   Sets error_flag if negative sign is used with set absolute
;                   speed command
;
; Algorithms:       None.
; Data Structures:  None.
;
; Registers Changed:   None
;
; Known Bugs:       None.
; Limitations:      None.
;
; Revision History:
;       11/21/2015      Anshul Ramachandran     func spec and pseudocode
;       11/28/2015      Anshul Ramachandran     assembly code


DoSetSignVarAction      PROC    NEAR

CheckTypeOfSign:
    CMP AL, NEGATIVE                      ; Check if the passed in sign is negative
    JNE SetSignVarNoError                 ; If it isn’t there is nothing to do
                                          ; (sign_flag is default POSITIVE)
    CMP parser_state, ST_ABSSPEED         ; If it is NEGATIVE, check if we are trying
    JE SetSignVarParseError               ; to put NEGATIVE with setting the absolute
                                          ; speed, in which case this isn’t allowed
                                          ; so raise a parse error
    ;JNE SetSignNegative                  ; Otherwise just set sign_flag to NEGATIVE

SetSignNegative:
    MOV sign_flag, NEGATIVE               ; Set sign_flag to NEGATIVE
    JMP SetSignVarNoError                 ; and return

SetSignVarParseError:
    MOV error_flag, PARSE_ERROR           ; If we tried to use a negative sign for 
                                          ; absolute speed (which can take +/no sign), 
                                          ; set the error_flag and return

SetSignVarNoError:
    RET


DoSetSignVarAction      ENDP


; DoSetAbsSpeedAction
;
; Description:      This function calls SetMotorSpeed with curr_arg as the new speed
;                   and the function returns. This function therefore sets the
;                   absolute speed of the RoboTrike. Since curr_arg is a signed shared
;                   variable, the maximum absolute value that can be set is half of 
;                   MAX_SPEED, and set rel speed must be used to increase the absolute
;                   speed of the RoboTrike further.
;
; Operation:        The function calls SetMotorSpeed with curr_arg as the
;                   speed argument (AX) and DIR_NO_CHANGE as the direction
;                   argument (BX), and returns without issue.
;
; Arguments:        None.
;
; Return Value:     None.
;
; Local Variables:  None.
; Shared Variables: curr_arg (read), sign_flag/curr_arg/error_flag (write in call to 
;                   ResetSerialParser)
;
; Global Variables: None.
;
; Input:            None.
; Output:           None.
;
; Error Handling:   None.
;
; Algorithms:       None.
; Data Structures:  None.
;
; Registers Changed:   None.
;
; Known Bugs:       None.
; Limitations:      None.
;
; Revision History:
;       11/21/2015      Anshul Ramachandran     func spec and pseudocode
;       11/28/2015      Anshul Ramachandran     assembly code


DoSetAbsSpeedAction      PROC    NEAR

SaveRegistersDoSetAbsSpeedAction:
    PUSH AX                               ; Save registers
    PUSH BX

AbsSpeedCallSetMotorSpeed:
    MOV AX, curr_arg                      ; Use the shared curr_arg var as the speed
                                          ; argument to SetMotorSpeed
    MOV BX, DIR_NO_CHANGE                 ; Direction is unchanged
    CALL SetMotorSpeed                    ; Call the motor function to update

RestoreRegistersDoSetAbsSpeedAction:
    POP BX                                ; Restore registers
    POP AX

    RET

DoSetAbsSpeedAction      ENDP


; DoSetRelSpeedAction
;
; Description:      This function calls SetMotorSpeed with curr_arg as the relative
;                   change in speed to the current speed of the Robotrike and then 
;                   the function returns. This function therefore sets the
;                   relative speed of the RoboTrike
;
; Operation:        The function uses GetMotorSpeed to get the current absolute 
;                   speed and then adds the speed argument (curr_arg) to that - note
;                   that curr_arg is a signed integer so the positive and negative
;                   cases of curr_arg are treated separately to make sure that 
;                   unsigned/signed arithmetic is done properly. Then the function 
;                   calls SetMotorSpeed with this altered speed value as
;                   the speed argument (AX) and DIR_NO_CHANGE as the direction
;                   argument (BX), and returns. Note: this function ‘caps’ the 
;                   abs speed to SetMotorSpeed at ZERO_SPEED and MAX_SPEED (if
;                   relative speed change pushes the absolute speed outside the
;                   range of these values).
;
; Arguments:        None.
;
; Return Value:     None.
;
; Local Variables:  None.
; Shared Variables: curr_arg (read), sign_flag (read), sign_flag/curr_arg/
;                   error_flag (write in ResetSerialParser call)
;
; Global Variables: None.
;
; Input:            None.
; Output:           None.
;
; Error Handling:   None.
;
; Algorithms:       None.
; Data Structures:  None.
;
; Registers Changed:   None.
;
; Known Bugs:       None.
; Limitations:      None.
;
; Revision History:
;       11/21/2015      Anshul Ramachandran     func spec and pseudocode
;       11/28/2015      Anshul Ramachandran     assembly code


DoSetRelSpeedAction      PROC    NEAR

SaveRegistersDoSetRelSpeedAction:
    PUSH AX                               ; Save registers
    PUSH BX
    PUSH CX

RelSpeedGetCurrMotorSpeed:
    CALL GetMotorSpeed                    ; Get the current speed in AX

SetupForRelSpeedChange:
    MOV BX, DIR_NO_CHANGE                 ; Direction will be unchanged
    MOV CX, curr_arg                      ; Move the amount to change speed by in CX

CheckSignFlagForAddOrSubtract:
    CMP sign_flag, NEGATIVE               ; See whether we want to dec/inc speed
    JE SetRelSpeedSubtract                ; If we want to decrease, we want to 
                                          ; subtract the shared argument var
    ;JNE SetRelSpeedAdd                   ; Otherwise we want to add the shared arg

SetRelSpeedAdd:
    ADD AX, CX                            ; Add the shared arg to the current speed
    JC WrapRelSpeedMotorSpeed             ; If there was overflow issues, we have
                                          ; to cap the speed to MAX_SPEED
    CMP AX, MAX_SPEED                     ; Make sure that we don't need to wrap with
    JBE RelSpeedCallSetMotorSpeed         ; compare and call SetMotorSpeed, if we do
    ;JA WrapRelSpeedMotorSpeed            ; need to wrap still, do so

WrapRelSpeedMotorSpeed:
    MOV AX, MAX_SPEED                     ; If there was overflow, we want to cap
                                          ; the speed to MAX_SPEED
    JMP RelSpeedCallSetMotorSpeed         ; and call SetMotorSpeed
    
SetRelSpeedSubtract:
    NEG CX                                ; Negate the argument (to positive value)
    SUB AX, CX                            ; add subtract the change from current speed
    JNC RelSpeedCallSetMotorSpeed         ; Do additional check to make sure
                                          ; AX didn’t carry to negative value, if
    MOV AX, ZERO_SPEED                    ; it did, cap min speed at ZERO_SPEED
    ;JMP RelSpeedCallSetMotorSpeed        ; and call SetMotorSpeed
    
RelSpeedCallSetMotorSpeed:
    CALL SetMotorSpeed                    ; BX contains DIR_NO_CHANGE
                                          ; AX contains desired new speed

RestoreRegistersDoSetRelSpeedAction:
    POP CX                                ; Restore registers
    POP BX
    POP AX
    
    RET

DoSetRelSpeedAction      ENDP


; DoSetDirAction
;
; Description:      This function calls SetMotorSpeed with curr_arg as the direction
;                   (angle) and the speed not changed, and then returns. This function 
;                   therefore sets the direction of the RoboTrike
;
; Operation:        This function calls SetMotorSpeed with SPEED_NO_CHANGE 
;                   as the the speed argument (AX) and curr_arg as the rel direction
;                   argument (BX), puts PARSE_SUCCESS in AX. If not set, then 
;                   PARSE_ERROR is put into AX. Note: also does normalizing of 
;                   curr_arg before passing into SetMotorSpeed. The angle is 
;                   normalized between 0 and 360 so we know when adding to the  
;                   current angle from GetMotorDirection (which will be similarly 
;                   positive normalized) that there won’t be overflow or 
;                   signed/unsigned issues.
;
; Arguments:        None.
;
; Return Value:     None.
;
; Local Variables:  None.
; Shared Variables: curr_arg (read), sign_flag/curr_arg/error_flag (write in call 
;                   to ResetSerialParser)
;
; Global Variables: None.
;
; Input:            None.
; Output:           None.
;
; Error Handling:   None.
;
; Algorithms:       None.
; Data Structures:  None.
;
; Registers Changed:   None.
;
; Known Bugs:       None.
; Limitations:      None.
;
; Revision History:
;       11/21/2015      Anshul Ramachandran     func spec and pseudocode
;       11/28/2015      Anshul Ramachandran     assembly code


DoSetDirAction      PROC    NEAR

SaveRegistersDoSetDirAction:
    PUSH AX                               ; Save registers
    PUSH BX
    PUSH DX

NormalizeCurrArgAngle:                    ; If we normalize the argument we want to
                                          ; change the direction by before adding
                                          ; to the value from GetMotorDirection, we
                                          ; will guarantee that we won’t get into
                                          ; overflow issues and issues with the
                                          ; DIR_NO_CHANGE value
    MOV AX, curr_arg                      ; Get the direction argument in AX
    CMP AX, 0                             ; If we have a negative value to normalize
    JL NormalizeNegativeAngle             ; we need to do a few more steps
    ;JGE NormalizePositiveAngle           ; Otherwise we just normalize a positive
                                          ; angle argument

NormalizePositiveAngle:
    XOR DX, DX                            ; If angle is positive, get mod 360
    MOV BX, 360
    DIV BX
    MOV AX, DX                            ; Final argument in 0-359 range
    JMP GetCurrentMotorDirection          ; Continue to getting current direction

NormalizeNegativeAngle:
    NEG AX                                ; If angle is negative, take the positive
    XOR DX, DX                            ; value, take that mod 360
    MOV BX, 360
    DIV BX
    MOV AX, DX
    NEG AX                                ; Then make it negative again
    ADD AX, 360                           ; and add 360 to get equivalent angle 
                                          ; in 0-359 range
    ;JMP GetCurrentMotorDirection         ; Continue to getting current direction

GetCurrentMotorDirection:
    MOV BX, AX                            ; Save argument to BX
    CALL GetMotorDirection                ; Get current direction in AX
    ADD BX, AX                            ; Add current direction to argument in BX

SetDirCallSetMotorSpeed:
    MOV AX, SPEED_NO_CHANGE               ; Set AX to SPEED_NO_CHANGE since we are
                                          ; changing angle (and new value is in BX)
    CALL SetMotorSpeed                    ; so we can call SetMotorSpeed to update

RestoreRegistersDoSetDirAction:
    POP DX                                ; Restore registers and return
    POP BX
    POP AX
    
    RET

DoSetDirAction      ENDP


; DoSetLaserOnAction
;
; Description:      This function calls SetLaser to set the laser to ON and returns.
;
; Operation:        SetLaser is called with argument ON.
;
; Arguments:        None.
;
; Return Value:     None.
;
; Local Variables:  None.
; Shared Variables: None.
;
; Global Variables: None.
;
; Input:            None.
; Output:           None.
;
; Error Handling:   None.
;
; Algorithms:       None.
; Data Structures:  None.
;
; Registers Changed:   None.
;
; Known Bugs:       None.
; Limitations:      None.
;
; Revision History:
;       11/21/2015      Anshul Ramachandran     func spec and pseudocode
;       11/28/2015      Anshul Ramachandran     assembly code


DoSetLaserOnAction      PROC    NEAR

CallSetLaserOnAction:
    PUSH AX
    MOV AX, ON                            ; Move ON command into AX
    CALL SetLaser                         ; and call SetLaser
    POP AX
    
    RET

DoSetLaserOnAction      ENDP


; DoSetLaserOffAction
;
; Description:      This function calls SetLaser to set the laser to OFF and returns.
;
; Operation:        SetLaser is called with argument OFF.
;
; Arguments:        None.
;
; Return Value:     None.
;
; Local Variables:  None.
; Shared Variables: None.
;
; Global Variables: None.
;
; Input:            None.
; Output:           None.
;
; Error Handling:   None.
;
; Algorithms:       None.
; Data Structures:  None.
;
; Registers Changed:   None.
;
; Known Bugs:       None.
; Limitations:      None.
;
; Revision History:
;       11/21/2015      Anshul Ramachandran     func spec and pseudocode
;       11/28/2015      Anshul Ramachandran     assembly code


DoSetLaserOffAction      PROC    NEAR

CallSetLaserOffAction:
    PUSH AX
    MOV AX, OFF                           ; Move OFF command into AX
    CALL SetLaser                         ; and call SetLaser
    POP AX
    
    RET

DoSetLaserOffAction      ENDP


; DoSetAbsTurretAngleAction
;
; Description:      This function calls SetTurretAngle with curr_arg, and the function
;                   returns. This function therefore sets the absolute angle of the 
;                   RoboTrike’s turret.
;
; Operation:        This function calls SetTurretAngle with curr_arg as
;                   the angle argument (AX).
;
; Arguments:        None.
;
; Return Value:     None.
;
; Local Variables:  None.
; Shared Variables: curr_arg (read), sign_flag/curr_arg/error_flag (write in 
;                   call to ResetSerialParser)
;
; Global Variables: None.
;
; Input:            None.
; Output:           None.
;
; Error Handling:   None.
;
; Algorithms:       None.
; Data Structures:  None.
;
; Registers Changed:   None.
;
; Known Bugs:       None.
; Limitations:      None.
;
; Revision History:
;       11/21/2015      Anshul Ramachandran     func spec and pseudocode
;       11/28/2015      Anshul Ramachandran     assembly code


DoSetAbsTurretAngleAction      PROC    NEAR

SaveRegistersDoSetAbsTurretAngleAction:
    PUSH AX                               ; Save registers
    
AbsTurretCallSetTurretAngle:
    MOV AX, curr_arg                      ; Get new abs turret angle in curr_arg
    CALL SetTurretAngle                   ; Call setter function and return

RestoreRegistersDoSetAbsTurretAngleAction:
    POP AX                                ; Restore registers
    RET

DoSetAbsTurretAngleAction      ENDP


; DoSetRelTurretAngleAction
;
; Description:      This function calls SetRelTurretAngle with curr_arg, and the
;                   function returns. This function therefore sets the absolute angle
;                   of the RoboTrike’s turret.
;
; Operation:        This function calls SetRelTurretAngle with curr_arg as
;                   the rel angle argument (AX).
;
; Arguments:        None.
;
; Return Value:     None.
;
; Local Variables:  None.
; Shared Variables: curr_arg (read), sign_flag/curr_arg/error_flag (write in 
;                   call to ResetSerialParser)
;
; Global Variables: None.
;
; Input:            None.
; Output:           None.
;
; Error Handling:   None.
;
; Algorithms:       None.
; Data Structures:  None.
;
; Registers Changed:   None.
;
; Known Bugs:       None.
; Limitations:      None.
;
; Revision History:
;       11/21/2015      Anshul Ramachandran     func spec and pseudocode
;       11/28/2015      Anshul Ramachandran     assembly code


DoSetRelTurretAngleAction      PROC    NEAR

SaveRegistersDoSetRelTurretAngleAction:
    PUSH AX                               ; Save registers
    
RelTurretCallSetTurretAngle:
    MOV AX, curr_arg                      ; Get new rel turret angle in curr_arg
    CALL SetRelTurretAngle                ; Call setter function and return

RestoreRegistersDoSetRelTurretAngleAction:
    POP AX                                ; Restore registers
    RET

DoSetRelTurretAngleAction      ENDP


; DoSetTurretElevAction
;
; Description:      This function calls SetTurretElevation with curr_arg as the
;                   angle argument if the curr_arg is in the correct bounds
;                   (MIN_TURR_ELEV to MAX_TURR_ELEV). This function therefore sets 
;                   the elevation of the RoboTrike’s turret.
;
; Operation:        This function calls SetTurretElevation with curr_arg as
;                   the the angle argument (AX) after checking that the argument
;                   is in the bounds of MIN_TURR_ELEV to MAX_TURR_ELEV. If not, 
;                   error_flag is set to PARSE_ERROR and SetTurretElevation is
;                   not called.
;
; Arguments:        None.
;
; Return Value:     None.
;
; Local Variables:  None.
; Shared Variables: curr_arg (read), error_flag (write) sign_flag/curr_arg/
;                   error_flag (write in call to ResetSerialParser)
;
; Global Variables: None.
;
; Input:            None.
; Output:           None.
;
; Error Handling:   Sets error_flag to PARSE_ERROR if curr_arg is out of bounds
;                   for the turret elevation.
;
; Algorithms:       None.
; Data Structures:  None.
;
; Registers Changed:   None.
;
; Known Bugs:       None.
; Limitations:      None.
;
; Revision History:
;       11/21/2015      Anshul Ramachandran     func spec and pseudocode
;       11/28/2015      Anshul Ramachandran     assembly code


DoSetTurretElevAction      PROC    NEAR

SaveRegistersDoSetTurretElevAction:
    PUSH AX                               ; Save registers
    
TurretElevCheckArgument:
    MOV AX, curr_arg                      ; Get new turret elev angle which is in
    CMP AX, MAX_TURR_ELEV                 ; curr_arg and make sure it isn’t bigger
    JG TurretElevErrorFound               ; than the MAX_TURR_ELEV and it isn’t
    CMP AX, MIN_TURR_ELEV                 ; smaller than the MIN_TURR_ELEV before
    JL TurretElevErrorFound               ; calling SetTurretElevation, if out of
    CALL SetTurretElevation               ; range, the error_flag is set
    JMP RestoreRegistersDoSetTurretElevAction
    
TurretElevErrorFound:
    MOV error_flag, PARSE_ERROR           ; Set the error_flag and return

RestoreRegistersDoSetTurretElevAction:
    POP AX                                ; Restore registers
    RET

DoSetTurretElevAction      ENDP


; DoNothingAction
;
; Description:      This function does nothing, used when no action needs to be done
;                   when moving between states of the state machine.
;
; Operation:        Does nothing and returns.
;
; Arguments:        None.
;
; Return Value:     None.
;
; Local Variables:  None.
; Shared Variables: None.
;
; Global Variables: None.
;
; Input:            None.
; Output:           None.
;
; Error Handling:   None.
;
; Algorithms:       None.
; Data Structures:  None.
;
; Registers Changed:   None.
;
; Known Bugs:       None.
; Limitations:      None.
;
; Revision History:
;       11/21/2015      Anshul Ramachandran     func spec and pseudocode
;       11/28/2015      Anshul Ramachandran     assembly code


DoNothingAction      PROC    NEAR

    RET                                   ; Just return

DoNothingAction      ENDP


; DoErrorFoundAction
;
; Description:      This function is called when there has been a parsing error in
;                   the finite state machine (illegal sequence of characters), and
;                   it sets the error_flag to PARSE_ERROR to report the error.
;
; Operation:        Sets error_flag to PARSE_ERROR so that the error can be reported 
;                   in SerialParseChar
;
; Arguments:        None.
;
; Return Value:     None.
;
; Local Variables:  None.
; Shared Variables: error_flag (write)
;
; Global Variables: None.
;
; Input:            None.
; Output:           None.
;
; Error Handling:   None.
;
; Algorithms:       None.
; Data Structures:  None.
;
; Registers Changed:   None.
;
; Known Bugs:       None.
; Limitations:      None.
;
; Revision History:
;       11/21/2015      Anshul Ramachandran     func spec and pseudocode
;       11/28/2015      Anshul Ramachandran     assembly code


DoErrorFoundAction      PROC    NEAR

    MOV error_flag, PARSE_ERROR           ; Set error_flag to PARSE_ERROR
    RET                                   ; and return

DoErrorFoundAction      ENDP



; StateTable
; Description: This is the transition table for the Mealy state machine, using a
;              TRANS_ENTRY struct to hold the next state and the action to take. The
;              states are contained in serparse.inc
; Revision History:
;       11/21/2015      Anshul Ramachandran     init revision

TRANS_ENTRY    STRUC

    NEXT_STATE DB ?        ; next state
    ACTION     DW ?        ; action to take (Do* functions in serparse.asm)

TRANS_ENTRY    ENDS

; Macro that constructs states
%*DEFINE(TRANS(next_state, action)) (
    TRANS_ENTRY<%next_state, OFFSET(%action)>
)

StateTable    LABEL    TRANS_ENTRY

    ; ST_INIT -> ______ w/action ______  on Input token =
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_DIGIT
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_SIGN
    %TRANS(ST_DIR, DoNothingAction)           ; TOKEN_DIR
    %TRANS(ST_ELEV, DoNothingAction)          ; TOKEN_ELEV
    %TRANS(ST_ONLASER, DoNothingAction)       ; TOKEN_ONLASER
    %TRANS(ST_OFFLASER, DoNothingAction)      ; TOKEN_OFFLASER
    %TRANS(ST_ABSSPEED, DoNothingAction)      ; TOKEN_ABSSPEED
    %TRANS(ST_TURRANGLE, DoNothingAction)     ; TOKEN_TURRANGLE
    %TRANS(ST_RELSPEED, DoNothingAction)      ; TOKEN_RELSPEED
    %TRANS(ST_INIT, DoNothingAction)          ; TOKEN_IGNORE
    %TRANS(ST_INIT, DoNothingAction)          ; TOKEN_RETURN
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_OTHER

    ; ST_D_DIGIT -> ______ w/action ______  on Input token =
    %TRANS(ST_D_DIGIT, DoAddDigitAction)      ; TOKEN_DIGIT
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_SIGN
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_DIR
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_ELEV
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_ONLASER
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_OFFLASER
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_ABSSPEED
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_TURRANGLE
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_RELSPEED
    %TRANS(ST_D_DIGIT, DoNothingAction)       ; TOKEN_IGNORE
    %TRANS(ST_INIT, DoSetDirAction)           ; TOKEN_RETURN
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_OTHER

    ; ST_E_DIGIT -> ______ w/action ______  on Input token =
    %TRANS(ST_E_DIGIT, DoAddDigitAction)      ; TOKEN_DIGIT
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_SIGN
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_DIR
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_ELEV
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_ONLASER
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_OFFLASER
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_ABSSPEED
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_TURRANGLE
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_RELSPEED
    %TRANS(ST_E_DIGIT, DoNothingAction)       ; TOKEN_IGNORE
    %TRANS(ST_INIT, DoSetTurretElevAction)    ; TOKEN_RETURN
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_OTHER

    ; ST_S_DIGIT -> ______ w/action ______  on Input token =
    %TRANS(ST_S_DIGIT, DoAddDigitAction)      ; TOKEN_DIGIT
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_SIGN
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_DIR
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_ELEV
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_ONLASER
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_OFFLASER
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_ABSSPEED
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_TURRANGLE
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_RELSPEED
    %TRANS(ST_S_DIGIT, DoNothingAction)       ; TOKEN_IGNORE
    %TRANS(ST_INIT, DoSetAbsSpeedAction)      ; TOKEN_RETURN
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_OTHER

    ; ST_T_ABS_DIGIT -> ______ w/action ______  on Input token =
    %TRANS(ST_T_ABS_DIGIT, DoAddDigitAction)  ; TOKEN_DIGIT
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_SIGN
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_DIR
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_ELEV
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_ONLASER
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_OFFLASER
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_ABSSPEED
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_TURRANGLE
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_RELSPEED
    %TRANS(ST_T_ABS_DIGIT, DoNothingAction)   ; TOKEN_IGNORE
    %TRANS(ST_INIT, DoSetAbsTurretAngleAction)  ; TOKEN_RETURN
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_OTHER

    ; ST_T_REL_DIGIT -> ______ w/action ______  on Input token =
    %TRANS(ST_T_REL_DIGIT, DoAddDigitAction)  ; TOKEN_DIGIT
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_SIGN
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_DIR
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_ELEV
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_ONLASER
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_OFFLASER
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_ABSSPEED
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_TURRANGLE
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_RELSPEED
    %TRANS(ST_T_REL_DIGIT, DoNothingAction)   ; TOKEN_IGNORE
    %TRANS(ST_INIT, DoSetRelTurretAngleAction)  ; TOKEN_RETURN
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_OTHER

    ; ST_V_DIGIT -> ______ w/action ______  on Input token =
    %TRANS(ST_V_DIGIT, DoAddDigitAction)      ; TOKEN_DIGIT
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_SIGN
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_DIR
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_ELEV
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_ONLASER
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_OFFLASER
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_ABSSPEED
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_TURRANGLE
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_RELSPEED
    %TRANS(ST_V_DIGIT, DoNothingAction)       ; TOKEN_IGNORE
    %TRANS(ST_INIT, DoSetRelSpeedAction)      ; TOKEN_RETURN
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_OTHER

    ; ST_D_SIGN -> ______ w/action ______  on Input token =
    %TRANS(ST_D_DIGIT, DoAddDigitAction)      ; TOKEN_DIGIT
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_SIGN
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_DIR
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_ELEV
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_ONLASER
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_OFFLASER
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_ABSSPEED
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_TURRANGLE
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_RELSPEED
    %TRANS(ST_D_SIGN, DoNothingAction)        ; TOKEN_IGNORE
    %TRANS(ST_INIT, DoErrorFoundAction)       ; TOKEN_RETURN
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_OTHER

    ; ST_E_SIGN -> ______ w/action ______  on Input token =
    %TRANS(ST_E_DIGIT, DoAddDigitAction)      ; TOKEN_DIGIT
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_SIGN
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_DIR
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_ELEV
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_ONLASER
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_OFFLASER
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_ABSSPEED
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_TURRANGLE
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_RELSPEED
    %TRANS(ST_E_SIGN, DoNothingAction)        ; TOKEN_IGNORE
    %TRANS(ST_INIT, DoErrorFoundAction)       ; TOKEN_RETURN
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_OTHER

    ; ST_S_SIGN -> ______ w/action ______  on Input token =
    %TRANS(ST_S_DIGIT, DoAddDigitAction)      ; TOKEN_DIGIT
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_SIGN
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_DIR
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_ELEV
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_ONLASER
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_OFFLASER
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_ABSSPEED
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_TURRANGLE
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_RELSPEED
    %TRANS(ST_S_SIGN, DoNothingAction)        ; TOKEN_IGNORE
    %TRANS(ST_INIT, DoErrorFoundAction)       ; TOKEN_RETURN
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_OTHER

    ; ST_T_REL_SIGN -> ______ w/action ______  on Input token =
    %TRANS(ST_T_REL_DIGIT, DoAddDigitAction)  ; TOKEN_DIGIT
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_SIGN
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_DIR
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_ELEV
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_ONLASER
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_OFFLASER
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_ABSSPEED
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_TURRANGLE
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_RELSPEED
    %TRANS(ST_T_REL_SIGN, DoNothingAction)    ; TOKEN_IGNORE
    %TRANS(ST_INIT, DoErrorFoundAction)       ; TOKEN_RETURN
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_OTHER

    ; ST_V_SIGN -> ______ w/action ______  on Input token =
    %TRANS(ST_V_DIGIT, DoAddDigitAction)      ; TOKEN_DIGIT
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_SIGN
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_DIR
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_ELEV
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_ONLASER
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_OFFLASER
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_ABSSPEED
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_TURRANGLE
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_RELSPEED
    %TRANS(ST_V_SIGN, DoNothingAction)        ; TOKEN_IGNORE
    %TRANS(ST_INIT, DoErrorFoundAction)       ; TOKEN_RETURN
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_OTHER

    ; ST_DIR -> ______ w/action ______  on Input token =
    %TRANS(ST_D_DIGIT, DoAddDigitAction)      ; TOKEN_DIGIT
    %TRANS(ST_D_SIGN, DoSetSignVarAction)     ; TOKEN_SIGN
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_DIR
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_ELEV
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_ONLASER
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_OFFLASER
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_ABSSPEED
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_TURRANGLE
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_RELSPEED
    %TRANS(ST_DIR, DoNothingAction)           ; TOKEN_IGNORE
    %TRANS(ST_INIT, DoErrorFoundAction)       ; TOKEN_RETURN
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_OTHER

    ; ST_ELEV -> ______ w/action ______  on Input token =
    %TRANS(ST_E_DIGIT, DoAddDigitAction)      ; TOKEN_DIGIT
    %TRANS(ST_E_SIGN, DoSetSignVarAction)     ; TOKEN_SIGN
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_DIR
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_ELEV
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_ONLASER
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_OFFLASER
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_ABSSPEED
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_TURRANGLE
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_RELSPEED
    %TRANS(ST_ELEV, DoNothingAction)          ; TOKEN_IGNORE
    %TRANS(ST_INIT, DoErrorFoundAction)       ; TOKEN_RETURN
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_OTHER

    ; ST_ONLASER -> ______ w/action ______  on Input token =
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_DIGIT
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_SIGN
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_DIR
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_ELEV
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_ONLASER
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_OFFLASER
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_ABSSPEED
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_TURRANGLE
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_RELSPEED
    %TRANS(ST_ONLASER, DoNothingAction)       ; TOKEN_IGNORE
    %TRANS(ST_INIT, DoSetLaserOnAction)       ; TOKEN_RETURN
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_OTHER

    ; ST_OFFLASER -> ______ w/action ______  on Input token =
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_DIGIT
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_SIGN
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_DIR
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_ELEV
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_ONLASER
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_OFFLASER
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_ABSSPEED
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_TURRANGLE
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_RELSPEED
    %TRANS(ST_OFFLASER, DoNothingAction)      ; TOKEN_IGNORE
    %TRANS(ST_INIT, DoSetLaserOffAction)      ; TOKEN_RETURN
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_OTHER

    ; ST_ABSSPEED -> ______ w/action ______  on Input token =
    %TRANS(ST_S_DIGIT, DoAddDigitAction)      ; TOKEN_DIGIT
    %TRANS(ST_S_SIGN, DoSetSignVarAction)     ; TOKEN_SIGN
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_DIR
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_ELEV
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_ONLASER
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_OFFLASER
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_ABSSPEED
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_TURRANGLE
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_RELSPEED
    %TRANS(ST_ABSSPEED, DoNothingAction)      ; TOKEN_IGNORE
    %TRANS(ST_INIT, DoErrorFoundAction)       ; TOKEN_RETURN
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_OTHER

    ; ST_TURRANGLE -> ______ w/action ______  on Input token =
    %TRANS(ST_T_ABS_DIGIT, DoAddDigitAction)  ; TOKEN_DIGIT
    %TRANS(ST_T_REL_SIGN, DoSetSignVarAction) ; TOKEN_SIGN
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_DIR
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_ELEV
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_ONLASER
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_OFFLASER
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_ABSSPEED
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_TURRANGLE
    %TRANS(ST_ERROR, DoNothingAction)         ; TOKEN_RELSPEED
    %TRANS(ST_TURRANGLE, DoNothingAction)     ; TOKEN_IGNORE
    %TRANS(ST_INIT, DoErrorFoundAction)       ; TOKEN_RETURN
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_OTHER

    ; ST_RELSPEED -> ______ w/action ______  on Input token =
    %TRANS(ST_V_DIGIT, DoAddDigitAction)      ; TOKEN_DIGIT
    %TRANS(ST_V_SIGN, DoSetSignVarAction)     ; TOKEN_SIGN
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_DIR
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_ELEV
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_ONLASER
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_OFFLASER
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_ABSSPEED
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_TURRANGLE
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_RELSPEED
    %TRANS(ST_RELSPEED, DoNothingAction)      ; TOKEN_IGNORE
    %TRANS(ST_INIT, DoErrorFoundAction)       ; TOKEN_RETURN
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_OTHER

    ; ST_ERROR -> ______ w/action ______  on Input token =
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_DIGIT
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_SIGN
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_DIR
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_ELEV
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_ONLASER
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_OFFLASER
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_ABSSPEED
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_TURRANGLE
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_RELSPEED
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_IGNORE
    %TRANS(ST_INIT, ResetSerialParser)        ; TOKEN_RETURN
    %TRANS(ST_ERROR, DoErrorFoundAction)      ; TOKEN_OTHER


; Token type and value tables
; Description: This is a macro that will help define both the token type and values
;              tables
; Revision History:
;       11/21/2015      Anshul Ramachandran     init revision

%*DEFINE(TABLE) (
    %TABENT(TOKEN_OTHER, 0)          ; <null> (end of string)
    %TABENT(TOKEN_OTHER, 1)          ; SOH
    %TABENT(TOKEN_OTHER, 2)          ; STX
    %TABENT(TOKEN_OTHER, 3)          ; ETX
    %TABENT(TOKEN_OTHER, 4)          ; EOT
    %TABENT(TOKEN_OTHER, 5)          ; ENQ
    %TABENT(TOKEN_OTHER, 6)          ; ACK
    %TABENT(TOKEN_OTHER, 7)          ; BEL
    %TABENT(TOKEN_OTHER, 8)          ; backspace
    %TABENT(TOKEN_IGNORE, 9)         ; tab
    %TABENT(TOKEN_OTHER, 10)         ; new line
    %TABENT(TOKEN_OTHER, 11)         ; vertical tab
    %TABENT(TOKEN_OTHER, 12)         ; form feed
    %TABENT(TOKEN_RETURN, 13)        ; carriage return (end of statement)
    %TABENT(TOKEN_OTHER, 14)         ; SO
    %TABENT(TOKEN_OTHER, 15)         ; SI
    %TABENT(TOKEN_OTHER, 16)         ; DLE
    %TABENT(TOKEN_OTHER, 17)         ; DC1
    %TABENT(TOKEN_OTHER, 18)         ; DC2
    %TABENT(TOKEN_OTHER, 19)         ; DC3
    %TABENT(TOKEN_OTHER, 20)         ; DC4
    %TABENT(TOKEN_OTHER, 21)         ; NAK
    %TABENT(TOKEN_OTHER, 22)         ; SYN
    %TABENT(TOKEN_OTHER, 23)         ; ETB
    %TABENT(TOKEN_OTHER, 24)         ; CAN 
    %TABENT(TOKEN_OTHER, 25)         ; EM
    %TABENT(TOKEN_OTHER, 26)         ; SUB
    %TABENT(TOKEN_OTHER, 27)         ; Escape
    %TABENT(TOKEN_OTHER, 28)         ; FS
    %TABENT(TOKEN_OTHER, 29)         ; GS
    %TABENT(TOKEN_OTHER, 30)         ; AS
    %TABENT(TOKEN_OTHER, 31)         ; US
    %TABENT(TOKEN_IGNORE, ' ')       ; Space
    %TABENT(TOKEN_OTHER, '!')        ; !
    %TABENT(TOKEN_OTHER, '"')        ; "
    %TABENT(TOKEN_OTHER, '#')        ; #
    %TABENT(TOKEN_OTHER, '$')        ; $
    %TABENT(TOKEN_OTHER, 37)         ; Percent
    %TABENT(TOKEN_OTHER, '&')        ; &
    %TABENT(TOKEN_OTHER, 39)         ; '
    %TABENT(TOKEN_OTHER, 40)         ; Open paren
    %TABENT(TOKEN_OTHER, 41)         ; Close paren
    %TABENT(TOKEN_OTHER, '*')        ; *
    %TABENT(TOKEN_SIGN, +1)          ; + (positive sign)
    %TABENT(TOKEN_OTHER, 44)         ; ,
    %TABENT(TOKEN_SIGN, -1)          ; - (negative sign)
    %TABENT(TOKEN_OTHER, '.')        ; . (decimal point)
    %TABENT(TOKEN_OTHER, '/')        ; /
    %TABENT(TOKEN_DIGIT, 0)          ; 0 (digit)
    %TABENT(TOKEN_DIGIT, 1)          ; 1 (digit)
    %TABENT(TOKEN_DIGIT, 2)          ; 2 (digit)
    %TABENT(TOKEN_DIGIT, 3)          ; 3 (digit)
    %TABENT(TOKEN_DIGIT, 4)          ; 4 (digit)
    %TABENT(TOKEN_DIGIT, 5)          ; 5 (digit)
    %TABENT(TOKEN_DIGIT, 6)          ; 6 (digit)
    %TABENT(TOKEN_DIGIT, 7)          ; 7 (digit)
    %TABENT(TOKEN_DIGIT, 8)          ; 8 (digit)
    %TABENT(TOKEN_DIGIT, 9)          ; 9 (digit)
    %TABENT(TOKEN_OTHER, ':')        ; :
    %TABENT(TOKEN_OTHER, ';')        ; ;
    %TABENT(TOKEN_OTHER, '<')        ; <
    %TABENT(TOKEN_OTHER, '=')        ; =
    %TABENT(TOKEN_OTHER, '>')        ; >
    %TABENT(TOKEN_OTHER, '?')        ; ?
    %TABENT(TOKEN_OTHER, '@')        ; @
    %TABENT(TOKEN_OTHER, 'A')        ; A
    %TABENT(TOKEN_OTHER, 'B')        ; B
    %TABENT(TOKEN_OTHER, 'C')        ; C
    %TABENT(TOKEN_DIR, 'D')          ; D (set direction)
    %TABENT(TOKEN_ELEV, 'E')         ; E (set turret elevation)
    %TABENT(TOKEN_ONLASER, 'F')      ; F (set laser on)
    %TABENT(TOKEN_OTHER, 'G')        ; G
    %TABENT(TOKEN_OTHER, 'H')        ; H
    %TABENT(TOKEN_OTHER, 'I')        ; I
    %TABENT(TOKEN_OTHER, 'J')        ; J
    %TABENT(TOKEN_OTHER, 'K')        ; K
    %TABENT(TOKEN_OTHER, 'L')        ; L
    %TABENT(TOKEN_OTHER, 'M')        ; M
    %TABENT(TOKEN_OTHER, 'N')        ; N
    %TABENT(TOKEN_OFFLASER, 'O')     ; O (set laser off)
    %TABENT(TOKEN_OTHER, 'P')        ; P
    %TABENT(TOKEN_OTHER, 'Q')        ; Q
    %TABENT(TOKEN_OTHER, 'R')        ; R
    %TABENT(TOKEN_ABSSPEED, 'S')     ; S (set absolute speed)
    %TABENT(TOKEN_TURRANGLE, 'T')    ; T (set turret angle)
    %TABENT(TOKEN_OTHER, 'U')        ; U
    %TABENT(TOKEN_RELSPEED, 'V')     ; V (set relative speed)
    %TABENT(TOKEN_OTHER, 'W')        ; W
    %TABENT(TOKEN_OTHER, 'X')        ; X
    %TABENT(TOKEN_OTHER, 'Y')        ; Y
    %TABENT(TOKEN_OTHER, 'Z')        ; Z
    %TABENT(TOKEN_OTHER, '[')        ; [
    %TABENT(TOKEN_OTHER, 92)         ; Backslash
    %TABENT(TOKEN_OTHER, ']')        ; ]
    %TABENT(TOKEN_OTHER, '^')        ; ^
    %TABENT(TOKEN_OTHER, '_')        ; _
    %TABENT(TOKEN_OTHER, '`')        ; `
    %TABENT(TOKEN_OTHER, 'a')        ; a
    %TABENT(TOKEN_OTHER, 'b')        ; b
    %TABENT(TOKEN_OTHER, 'c')        ; c
    %TABENT(TOKEN_DIR, 'd')          ; d (set direction)
    %TABENT(TOKEN_ELEV, 'e')         ; e (set turret elevation)
    %TABENT(TOKEN_ONLASER, 'f')      ; f (set laser on)
    %TABENT(TOKEN_OTHER, 'g')        ; g
    %TABENT(TOKEN_OTHER, 'h')        ; h
    %TABENT(TOKEN_OTHER, 'i')        ; i
    %TABENT(TOKEN_OTHER, 'j')        ; j
    %TABENT(TOKEN_OTHER, 'k')        ; k
    %TABENT(TOKEN_OTHER, 'l')        ; l
    %TABENT(TOKEN_OTHER, 'm')        ; m
    %TABENT(TOKEN_OTHER, 'n')        ; n
    %TABENT(TOKEN_OFFLASER, 'o')     ; o (set laser off)
    %TABENT(TOKEN_OTHER, 'p')        ; p
    %TABENT(TOKEN_OTHER, 'q')        ; q
    %TABENT(TOKEN_OTHER, 'r')        ; r
    %TABENT(TOKEN_ABSSPEED, 's')     ; s (set absolute speed)
    %TABENT(TOKEN_TURRANGLE, 't')    ; t (set turret angle)
    %TABENT(TOKEN_OTHER, 'u')        ; u
    %TABENT(TOKEN_RELSPEED, 'v')     ; v (set relative speed)
    %TABENT(TOKEN_OTHER, 'w')        ; w
    %TABENT(TOKEN_OTHER, 'x')        ; x
    %TABENT(TOKEN_OTHER, 'y')        ; y
    %TABENT(TOKEN_OTHER, 'z')        ; z
    %TABENT(TOKEN_OTHER, '{')        ; {
    %TABENT(TOKEN_OTHER, '|')        ; |
    %TABENT(TOKEN_OTHER, '}')        ; }
    %TABENT(TOKEN_OTHER, '~')        ; ~
    %TABENT(TOKEN_OTHER, 127)        ; Rubout
)

; Macro for token type table
%*DEFINE(TABENT(tokentype, tokenvalue)) (
    DB %tokentype
)

; Token type table
TokenTypeTable  LABEL  BYTE
    %TABLE

; Macro for token value table
%*DEFINE(TABENT(tokentype, tokenvalue)) (
    DB %tokenvalue
)

; Token value table
TokenValueTable  LABEL  BYTE
    %TABLE

CODE ENDS



DATA SEGMENT PUBLIC 'DATA'

    ; Current state of Mealy state machine
    parser_state    DB     ?

    ; Sign flag (default is POSITIVE but can be set to NEGATIVE in DoSetSignVarAction)
    ; Keeps track of the sign of curr_arg
    sign_flag       DB     ?

    ; Argument passed in after command character, used as inputs in calls to
    ; motor functions
    curr_arg        DW     ?

    ; Flag that says whether some error occurred in parsing
    ; Can be set if: invalid state transition, overflow, out of bounds input , etc
    error_flag      DB     ?

DATA ENDS

    END