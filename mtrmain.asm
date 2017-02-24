        NAME    MTRMAIN

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                  MTRMAIN                                   ;
;                       Target Board Control Functions                       ;
;                                  EE/CS 51                                  ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Description:      This file contains the initialization of the RoboTrike
;                   motors as well as the main loop with associated functions
;                   and tables.
;
; Functions:
;     RemoteMainLoop: Main loop for the target board (motors) of the RoboTrike
;     ResetRemoteMainLoop: Initializes/resets shared variables for target
;                          board main loop
;     SerialReceiveHandler: Handles characters received from remote board
;     SerialErrorHandler: Handles the serial errors for the motor board
;
;
; Tables:
;     TargetEventHandlersTable: Function table for the event handlers in the
;                               motor unit of the RoboTrike
;
; Revision History:
;    11/30/15   Anshul Ramachandran       functional specification


CGROUP  GROUP   CODE

$INCLUDE(mtrmain.inc)
$INCLUDE(events.inc)
$INCLUDE(serial.inc)
$INCLUDE(serparse.inc)
$INCLUDE(general.inc)


CODE    SEGMENT PUBLIC 'CODE'


    ASSUME  CS:CGROUP, DS:DGROUP, SS:STACK


; external function declarations
    EXTRN   InitCS:NEAR
    EXTRN   ClrIRQVectors:NEAR

    EXTRN   Dec2String:NEAR
    EXTRN   Hex2String:NEAR

    EXTRN   InitTimer0:NEAR
    EXTRN   InstallTimer0EventHandler:NEAR

    EXTRN   InitParallel:NEAR

    EXTRN   InitMotor:NEAR
    EXTRN   GetMotorSpeed:NEAR
    EXTRN   GetMotorDirection:NEAR
    EXTRN   GetLaser:NEAR

    EXTRN   InitSerial:NEAR
    EXTRN   SerialPutString:NEAR
    EXTRN   InstallSerialHandler:NEAR
    EXTRN   InitSerialInterrupts:NEAR

    EXTRN   InitSerialParser:NEAR
    EXTRN   ParseSerialChar:NEAR

    EXTRN   InitEventQueue:NEAR
    EXTRN   DequeueEvent:NEAR
    EXTRN   GetFatalError:NEAR


; Fatal error message
FatalErrorMessage    LABEL    BYTE
    DB    'FATALErr', ASCII_NULL

; Serial error message
SerialErrorMessage   LABEL    BYTE
    DB    'SERErr  ', ASCII_NULL

; Parse error message
FatalErrorMessage    LABEL    BYTE
    DB    ‘PARSEErr', ASCII_NULL

; MotorMainLoop
;
; Description:      This function is the target board’s main loop. It initializes the
;                   chip select, interrupts, timer 0, motors, serial, serial parser,
;                   and event queue. Then it loops between dequeueing events from the
;                   event queue and handling received errors and characters from the
;                   serial connection with the remote board.
;
; Operation:        This function calls all of the initialization functions (the chip
;                   select and interrupts are enabled once and the others, including
;                   the initialization of shared variables for the remote main loop
;                   are done by calling ResetMotorMainLoop). After initialization, 
;                   the function goes into an infinite loop, where it first sees if
;                   a fatal error has happened (GetFatalError). If one has happened,
;                   a fatal error string is displayed to the display, everything is
;                   reset with ResetMotorMainLoop, and we loop back to the top of
;                   the infinite loop. If no fatal error has happened, we get the
;                   next event by calling DequeueEvent. If no event, then loop back
;                   to the top of the infinite loop. If there is an event, make sure
;                   it is a valid type of event (serial receive/error) and then use
;                   the TargetEventHandlersTable to call the correct handler for the 
;                   event (SerialErrorHandler, SerialReceiveHandler).
;                   After calling the handler, loop back to the top of the infinite
;                   loop.
;
; Arguments:        None.
;
; Return Value:     None.
;
; Local Variables:  None.
; Shared Variables: None.
; Global Variables: None.
;
; Input:            None.
; Output:           Motors.
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
;       11/30/2015      Anshul Ramachandran     functional specification
;
; Pseudocode:
;
; (Initialization already written)
; CALL GetFatalError
; WHILE (true)
;     IF fatal error exists
;         CALL Display (‘FATALERR’)
;         CALL ResetMotorMainLoop
;     ELSE
;         CALL DeqeueueEvent
;         IF event exists
;             IF event = (SER_RECEIVE_CODE OR SER_ERROR_CODE)
;                 BX = event << 1
;                 CALL TargetEventHandlersTable[BX]


START:  

MAIN:
    CLI

    MOV     AX, DGROUP                    ;initialize the stack pointer
    MOV     SS, AX
    MOV     SP, OFFSET(DGROUP:TopOfStack)

    MOV     AX, DGROUP                    ;initialize the data segment
    MOV     DS, AX

    CALL    InitCS                        ; initialize chip select
    CALL    ClrIRQVectors                 ; and interrupt vector table
    CALL    ResetMotorMainLoop            ; and initialize all other aspects
                                          ; of the target board/shared vars in
                                          ; this call

    STI                                   ; set the interrupts to occur

MotorMainLoopStart:
    CALL GetFatalError                    ; See if fatal error had occurred
    CMP AL, FATAL_ERROR
    JNE MotorMainLoopEventHandlers        ; If not continue to event handling
    ;JE MotorMainLoopFatalError           ; If yes, sent fatal error message to
                                          ; serial and reset the motor main loop

MotorMainLoopFatalError:
    MOV CX, CS                            ; SerialPutString takes from ES:SI, make
    MOV ES, CX                            ; fatal error message address match up
    MOV SI, OFFSET(FatalErrorMessage)
    CALL SerialPutString                  ; and call SerialPutString
    CALL ResetMotorMainLoop               ; Reset the motor main loop
    JMP MotorMainLoopStart                ; Go back to top of loop

MotorMainLoopEventHandlers:
    CALL DequeueEvent                     ; Get the event on the event queue
    JC MotorMainLoopStart                 ; If carry flag, then there wasn’t an
                                          ; event to get so go back to top of loop

MotorMainLoopCheckForValidEvent:
    MOV BX, 0                             ; Otherwise get the event type in BX
    MOV BL, AH
    CMP BX, SER_RECEIVE_CODE              ; Make sure the event has a valid event
    JE MotorMainLoopValidEvent            ; type for target board side
    CMP BX, SER_ERROR_CODE
    JE MotorMainLoopValidEvent
    JMP MotorMainLoopStart                ; If the event type is not valid, go back
                                          ; to top of loop

MotorMainLoopValidEvent:
    SHL BX, 1                             ; If it is valid, then convert event type
                                          ; to index in TargetEventHandlersTable
    CALL CS:TargetEventHandlersTable[BX]  ; then call that event handler
    JMP MotorMainLoopStart                ; and go back to top of loop


; ResetMotorMainLoop
;
; Description:      This function re-initializes the timer 0, motors, serial, serial
;                   parser, and event queue. Called when a fatal error occurs or on first 
;                   initialization of the target main loop.
;
; Operation:        This function calls all of the respective initialization functions
;                   for the timer 0, display, motors, serial, serial parser and event 
;                   queue.
;
; Arguments:        None.
;
; Return Value:     None.
;
; Local Variables:  None.
; Shared Variables: None.
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
;       11/30/2015      Anshul Ramachandran     functional specification
;
; Pseudocode:
;
; CLI
; CALL InitTimer0
; CALL InstallTimer0EventHandler
; CALL InitParallel
; CALL InitMotor
; CALL InitSerial
; CALL InstallSerialHandler
; CALL InitSerialInterrupts
; CALL InitSerialParser
; CALL InitEventQueue
; STI


ResetMotorMainLoop   PROC    NEAR

ResetMotorMainLoopStart:
    PUSHF                                 ; Save flags
    CLI                                   ; Disable interrupts during reset period

InitializeMotorSystems:
    CALL InitTimer0                       ; Initialize timer 0 for motors
    CALL InstallTimer0EventHandler        ; Install timer 0 event handler
    CALL InitParallel                     ; Initialize the parallel chip
    CALL InitMotor                        ; Initialize the motors
    CALL InitSerial                       ; Initialize the serial interface
    CALL InstallSerialHandler             ; Install serial event handler
    CALL InitSerialInterrupts             ; Initialize serial interrupts
    CALL InitSerialParser                 ; Initialize serial parser
    CALL InitEventQueue                   ; Initialize the event queue

ResetRemoteMainLoopEnd:
    STI                                   ; Re-enable interrupts
    POPF                                  ; Restore flags and return

    RET

ResetMotorMainLoop   ENDP



; SerialReceiveHandler
;
; Description:      This function handles 
;
; Operation:        This function takes the serial receive data event in AX, with
;                   AH having SER_RECEIVE_CODE and AL having the character we 
;                   are interested in. If the character TOGGLE_STATUS then toggle 
;                   access_flag between SPEED_ACCESS, DIR_ACCESS, and LASER_ACCESS,
;                   essentially giving access to the next value (mutex). Then
;                   depending on which one we gave access to, we will call 
;                   GetMotorSpeed, GetMotorDirection, or GetLaser respectively in
;                   combination with Dec2String or Hex2String to put it in string
;                   form that we can put the value into transmit_buffer (prepended
;                   by SPEED_CHAR, DIR_CHAR, or LASER_CHAR to tell what value we
;                   are displaying to the display later on). We then call 
;                   SerialPutString with transmit_buffer to send that string
;                   with the value over serial to the remote board. If AL was not
;                   one of the three status characters, then just call the
;                   ParseSerialChar function, make sure that the parse went well
;                   by looking at the result in AX (no PARSE_ERROR) and just
;                   send the data given by the access_flag as before (no need to
;                   return any values based on the parsed char back). If there
;                   was a parse error, then call SerialPutString with a parse
;                   error message and return.
;
; Arguments:        AX = serial receive event 
;                        (AH = SER_RECEIVE_CODE, AL = character)
;
; Return Value:     None.
;
; Local Variables:  None.
; Shared Variables: transmit_buffer = buffer that holds chars to send over serial
;                   access_flag = flag for which RoboTrike value we are currently 
;                                 accessing (speed, direction, laser)
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
;       11/30/2015      Anshul Ramachandran     functional specification
;
; Pseudocode:
;
; IF AL = TOGGLE_STATUS
;     IF access_flag = SPEED_ACCESS
;         access_flag = DIR_ACCESS
;     ELSE IF access_flag = DIR_ACCESS
;         access_flag = LASER_ACCESS
;     ELSE
;         access_flag = SPEED_ACCESS
; ELSE
;     CALL ParseSerialChar
;     IF PARSE_ERROR
;         CALL SerialPutString(‘PARSEERR’)
;         return
; SI = OFFSET(transmit_buffer) + 1               ; + 1 because first char is for
;                                                ; which value we are sending out
; IF access_flag = SPEED_ACCESS
;     transmit_buffer[0] = SPEED_CHAR
;     CALL GetMotorSpeed
;     CALL Hex2String
; ELSE IF access_flag = DIR_ACCESS
;     transmit_buffer[0] = DIR_CHAR
;     CALL GetMotorDirection
;     CALL Dec2String
; ELSE
;     transmit_buffer[0] = LASER_CHAR
;     CALL GetLaser
;     CALL Dec2String
; CALL SerialPutString(transmit_buffer)


SerialReceiveHandler   PROC    NEAR

SerialReceiveHandlerStart:
    PUSHA                                 ; Save registers

SerReceiveCheckIfToggleStatus:
    CMP AL, TOGGLE_STATUS                 ; See if we received the TOGGLE_STATUS
    JNE SerReceiveParseChar               ; char, if not, it is a normal character
                                          ; that should be treated as such
    ;JE SerReceiveToggleAccessFlag        ; If it is TOGGLE_STATUS, then we need
                                          ; to change what status value we want 
                                          ; to display to the board

SerReceiveToggleAccessFlag:
    CMP access_flag, SPEED_ACCESS         ; If currently accessing speed, want to
    JE SerReceiveSetToggleFlagDir         ; change access to direction
    CMP access_flag, DIR_ACCESS           ; If currently accessing direction, want
    JE SerReceiveSetToggleFlagLaser       ; to change access to laser
    ;JNE SerReceiveSetToggleFlagSpeed     ; If neither above, we are accessing laser,
                                          ; want to change access to speed

SerReceiveSetToggleFlagSpeed:
    MOV access_flag, SPEED_ACCESS         ; Set flag to speed access
    JMP SerReceiveSetupForSendData

SerReceiveSetToggleFlagDir:
    MOV access_flag, DIR_ACCESS           ; Set flag to direction access
    JMP SerReceiveSetupForSendData

SerReceiveSetToggleFlagLaser:
    MOV access_flag, LASER_ACCESS         ; Set flag to laser access
    JMP SerReceiveSetupForSendData

SerReceiveParseChar:
    CALL ParseSerialChar                  ; If normal char, parse the char
    CMP AX, PARSE_ERROR                   ; If there isn’t a parsing error, then 
    JNE SerReceiveSetupForSendData        ; continue to sending data about
                                          ; current status
    ;JE SerReceiveParseCharError          ; If there is, then we want to put
                                          ; the parse error message into serial
                                          ; to send to remote board

SerReceiveParseCharError:
    MOV CX, CS                            ; Get parse error message location in 
    MOV ES, CX                            ; ES:SI
    MOV SI, OFFSET(ParseErrorMessage)
    CALL SerialPutString                  ; and send message through serial
    JMP SerialReceiveHandlerEnd           ; and exit function

SerReceiveSetupForSendData:
    MOV SI, OFFSET(transmit_buffer)       ; Get the address of the transmit buff
    INC SI                                ; and increment this address by one
                                          ; since we will put a character in the
                                          ; first display that will indicate 
                                          ; what status value we are showing -
                                          ; we want the actual value to start at
                                          ; the second display

SerReceiveCheckAccessFlag:
    CMP access_flag, SPEED_ACCESS         ; Check for accessing speed
    JE SerReceiveSendSpeed
    CMP access_flag, DIR_ACCESS           ; Check for accessing direction
    JE SerReceiveSendDir
    ;JNE SerReceiveSendLaser              ; Otherwise we are accessing laser

SerReceiveSendLaser:
    MOV transmit_buffer, LASER_CHAR       ; Put an ‘L’ in the first display
    CALL GetLaser                         ; Get the laser status (ON/OFF)
    CALL Dec2String                       ; Convert to ASCII
    JMP SerReceiveSendString

SerReceiveSendSpeed:
    MOV transmit_buffer, SPEED_CHAR       ; Put an ’S’ in the first display
    CALL GetMotorSpeed                    ; Get the speed status (unsigned val)
    CALL Hex2String                       ; Convert to ASCII
    JMP SerReceiveSendString

SerReceiveSendDir:
    MOV transmit_buffer, DIR_CHAR         ; Put a ‘D’ in the first display
    CALL GetMotorDirection                ; Get the direction status
    CALL Dec2String                       ; Convert to ASCII
    ;JMP SerReceiveSendString

SerReceiveSendString:
    MOV CX, DS                            ; Get transmit buffer location in
    MOV ES, CX                            ; ES:SI
    MOV SI, OFFSET(transmit_buffer)
    CALL SerialPutString                  ; and put this string in serial

SerialReceiveHandlerEnd:
    POPA                                  ; Restore registers and return
    RET

SerialReceiveHandler   ENDP



; SerialErrorHandler
;
; Description:      This function handles serial error events. It just sends
;                   a serial error string over serial to remote board.
;
; Operation:        This function calls SerialPutString with a serial error 
;                   string, ‘SERERR’.
;
; Arguments:        AX = Serial event event (AH = SER_EVENT_CODE, AL not used)
;
; Return Value:     None.
;
; Local Variables:  None.
; Shared Variables: None.
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
;       11/30/2015      Anshul Ramachandran     functional specification
;
; Pseudocode:
;
; CALL SerialPutString(‘SERERR’)


SerialErrorHandler   PROC    NEAR

SerialErrorHandlerStart:
    PUSHA                                 ; Save registers

SerialErrorHandlerSendErrorString:
    MOV CX, CS                            ; Get serial error message location
    MOV ES, CX                            ; in ES:SI
    MOV SI, OFFSET(SerialErrorMessage)
    CALL SerialPutString                  ; and put the string into serial

SerialErrorHandlerEnd:
    POPA                                  ; Restore registers and return
    RET

SerialErrorHandler   ENDP



; TargetEventHandlersTable
; Description: This is a function table for the event handlers for
;              dequeued events in the target board main loop.
;
; Revision History:
;       11/30/2015      Anshul Ramachandran     init revision

TargetEventHandlersTable     LABEL     WORD
    DW   NO_EVENT
    DW   NO_EVENT
    DW   SerialReceiveHandler
    DW   SerialErrorHandler



CODE ENDS



;the data segment

DATA    SEGMENT PUBLIC  'DATA'

    ; buffer to hold characters to transmit over serial to
    ; remote unit
    transmit_buffer     DB    TRANSMIT_BUFFER_LEN   DUP(?)

    ; flag for which value we are currently accessing
    ; (mutex for accessing speed, direction, and laser values)
    access_flag         DB    ?

DATA    ENDS



;the stack

STACK   SEGMENT STACK  'STACK'

                DB      80 DUP ('Stack ')       ;240 words

TopOfStack      LABEL   WORD

STACK   ENDS



        END     START
