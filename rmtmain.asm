        NAME    RMTMAIN

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                  RMTMAIN                                   ;
;                       Remote Board Control Functions                       ;
;                                  EE/CS 51                                  ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Description:      This file contains the initialization of the RoboTrike
;                   remote as well as the main loop with associated functions
;                   and tables.
;
; Functions:
;     RemoteMainLoop: Main loop for the remote board of the RoboTrike
;     ResetRemoteMainLoop: Initializes/resets shared variables for remote
;                          board main loop
;     KeypressHandler: Handles key press events on the remote board
;     SerialReceiveHandler: Handles characters received from motor board
;     SerialErrorHandler: Handles the serial errors for the remote board
;
;
; Tables:
;     KeypressSerialCommandTable: Contains the serial commands corresponding 
;                                 to each key in the keypad that is sent to
;                                 the motor board
;     RemoteEventHandlersTable: Function table for the event handlers in the
;                               remote unit of the RoboTrike
;
; Revision History:
;    11/30/15   Anshul Ramachandran       functional specification


CGROUP  GROUP   CODE
DRGOUP  GROUP   DATA, STACK


$INCLUDE(rmtmain.inc)
$INCLUDE(events.inc)
$INCLUDE(serial.inc)
$INCLUDE(serparse.inc)
$INCLUDE(general.inc)


CODE    SEGMENT PUBLIC 'CODE'


    ASSUME  CS:CGROUP, DS:DGROUP


; external function declarations
    EXTRN   InitCS:NEAR
    EXTRN   ClrIRQVectors:NEAR

    EXTRN   InitTimer2:NEAR
    EXTRN   InstallTimer2EventHandler:NEAR

    EXTRN   InitDisplay:NEAR
    EXTRN   Display:NEAR

    EXTRN   InitKeypadDebounce:NEAR

    EXTRN   InitSerial:NEAR
    EXTRN   SerialPutString:NEAR
    EXTRN   InstallSerialHandler:NEAR
    EXTRN   InitSerialInterrupts:NEAR

    EXTRN   InitEventQueue:NEAR
    EXTRN   DequeueEvent:NEAR
    EXTRN   GetFatalError:NEAR


; Fatal error message
FatalErrorMessage    LABEL    BYTE
    DB    'FATALErr', ASCII_NULL

; Serial error message
SerialErrorMessage   LABEL    BYTE
    DB    'SERErr', ASCII_NULL


; RemoteMainLoop
;
; Description:      This function is the remote board’s main loop. It initializes the
;                   chip select, interrupts, timer 2, display, keypad, serial, and
;                   event queue. Then it loops between dequeueing events from the
;                   event queue and handling received errors and characters from the
;                   serial connection with the target board.
;
; Operation:        This function calls all of the initialization functions (the chip
;                   select and interrupts are enabled once and the others, including
;                   the initialization of shared variables for the remote main loop
;                   are done by calling ResetRemoteMainLoop). After initialization, 
;                   the function goes into an infinite loop, where it first sees if
;                   a fatal error has happened (GetFatalError). If one has happened,
;                   a fatal error string is displayed to the display, everything is
;                   reset with ResetRemoteMainLoop, and we loop back to the top of
;                   the infinite loop. If no fatal error has happened, we get the
;                   next event by calling DequeueEvent. If no event, then loop back
;                   to the top of the infinite loop. If there is an event, make sure
;                   it is a valid type of event (events in events.inc) and then use
;                   the RemoteEventHandlersTable to call the correct handler for the 
;                   event (SerialErrorHandler, SerialReceiveHandler, KeypressHandler).
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
; Input:            16-key keypad
; Output:           8 digit 14-segment display
;
; Error Handling:   Error event handlers and checking for fatal errors
;
; Algorithms:       None.
; Data Structures:  Queue
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
;         CALL ResetRemoteMainLoop
;     ELSE
;         CALL DeqeueueEvent
;         IF event exists
;             IF event = (KEY_EVENT_CODE OR SER_RECEIVE_CODE OR SER_ERROR_CODE)
;                 BX = event << 1
;                 CALL RemoteEventHandlersTable[BX]


START:  

MAIN:
    CLI

    MOV     AX, DGROUP                    ;initialize the stack pointer
    MOV     SS, AX
    MOV     SP, OFFSET(DGROUP:TopOfStack)

    MOV     AX, DGROUP                    ;initialize the data segment
    MOV     DS, AX

InitializeRemoteMainLoop:
    CALL    InitCS                        ; initialize chip select
    CALL    ClrIRQVectors                 ; and interrupt vector table
    CALL    ResetRemoteMainLoop           ; and initialize all other aspects
                                          ; of the remote board/shared vars in
                                          ; this call

    STI                                   ; set the interrupts to occur

RemoteMainLoopStart:
    CALL GetFatalError                    ; See if a fatal error had occurred
    CMP AL, FATAL_ERR
    JNE RemoteMainLoopEventHandlers       ; If not continue to event handling
    ;JE RemoteMainLoopFatalError          ; If yes, output fatal error message to the
                                          ; display and reset the remote main loop

RemoteMainLoopFatalError:
    MOV CX, CS                            ; Display takes from ES:SI, so make
    MOV ES, CX                            ; sure fatal error message (in CS) in the
    MOV SI, OFFSET(FatalErrorMessage)     ; right location
    CALL Display                          ; and call Display
    CALL ResetRemoteMainLoop              ; Reset the remote main loop
    JMP RemoteMainLoopStart               ; Go back to top of loop

RemoteMainLoopEventHandlers:
    CLC                                   ; Clear carry flag before dequeueing event
    CALL DequeueEvent                     ; Get the event
    JC RemoteMainLoopStart                ; If carry flag, then there wasn’t an
                                          ; event to get so go back to top of loop

RemoteMainLoopCheckForValidEvent:
    MOV BX, 0                             ; Otherwise get the event type in BX
    MOV BL, AH
    CMP BX, KEY_EVENT_CODE                ; Make sure the event has a valid event
    JE RemoteMainLoopValidEvent           ; type (event code)
    CMP BX, SER_RECEIVE_CODE
    JE RemoteMainLoopValidEvent
    CMP BX, SER_ERROR_CODE
    JE RemoteMainLoopValidEvent
    JMP RemoteMainLoopStart               ; If the event type is not valid, go back
                                          ; top of loop

RemoteMainLoopValidEvent:
    SHL BX, 1                             ; If it is valid, then convert event type
                                          ; to index in RemoteEventHandlersTable and
    CALL CS:RemoteEventHandlersTable[BX]  ; then call that event handler
    JMP RemoteMainLoopStart               ; and go back to top of loop


; ResetRemoteMainLoop
;
; Description:      This function re-initializes the timer 2, display, keypad, serial,
;                   and event queue, and resets the shared variables used in the 
;                   remote main loop. Called when a fatal error occurs or on first 
;                   initialization of the remote main loop.
;
; Operation:        This function calls all of the respective initialization functions
;                   for the timers, display, keypad, serial, and event queue. It then
;                   resets disp_index to DISP_INDEX_INIT (0) and disp_error to
;                   DISP_NO_ERR (0) since we want to start the populating of the
;                   disp_buffer (which is the string to display to the LED) at the
;                   first digit display (which has index 0) and no errors in displaying
;                   has happened on reset.
;
; Arguments:        None.
;
; Return Value:     None.
;
; Local Variables:  None.
; Shared Variables: disp_index = index used to write to disp_buffer (write)
;                   disp_error = error flag, set if we are displaying an error
;                                message, reset if we ware not (write)
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
; CALL InitTimer2
; CALL InstallTimer2EventHandler
; CALL InitDisplay
; CALL InitKeypadDebounce
; CALL InitSerial
; CALL InstallSerialHandler
; CALL InitSerialInterrupts
; CALL InitEventQueue
; disp_index = DISP_INDEX_INIT
; disp_error = DISP_NO_ERROR
; STI


ResetRemoteMainLoop   PROC    NEAR

ResetRemoteMainLoopStart:
    PUSHF                                 ; Save flags
    CLI                                   ; Disable interrupts during reset period

InitializeRemoteSystems:
    CALL InitTimer2                       ; Initialize timer 2 for keypad/display
    CALL InstallTimer2EventHandler        ; Install timer 2 handler
    CALL InitDisplay                      ; Initialize the display
    CALL InitKeypadDebounce               ; Initialize the keypad
    CALL InitSerial                       ; Initialize the serial interface
    CALL InstallSerialHandler             ; Install serial event handler
    CALL InitSerialInterrupts             ; Initialize serial interrupts
    CALL InitEventQueue                   ; Initialize the event queue

InitializeRemoteMainLoopSharedVariables:
    MOV disp_index, DISP_INDEX_INIT       ; Set the display index to DISP_INDEX_INIT
                                          ; (0) since we want to start putting chars
                                          ; at first digit display again after 
                                          ; initialization
    MOV disp_error, DISP_NO_ERR           ; Initially not displaying errors

ResetRemoteMainLoopEnd:
    STI                                   ; Re-enable interrupts
    POPF                                  ; Restore flags and return

    RET

ResetRemoteMainLoop   ENDP



; KeypressHandler
;
; Description:      This function handles key press events (events with event code being
;                   KEY_EVENT_CODE). It gets the serial command associated with the key
;                   press from the KeypressSerialCommandTable and sends that command
;                   to the target board using serial.
;
; Operation:        This function takes the keypress event to handle and uses it as 
;                   an index to KeypressSerialCommandTable (since the values for the
;                   key press event are 0 - 15, for each key scanning left to right,
;                   top to bottom) to get the serial command. Then, the function 
;                   calls SerialPutString to send the serial command. Note that the
;                   key press event value (the key number) needs to be multiplied
;                   by SER_COMMAND_LENGTH (8) since each string in the command table
;                   are 8 consecutive characters, so we need to multiply by this
;                   value to get the correct offset.
;
; Arguments:        AX = keypad event (AH = KEY_EVENT_CODE, AL = key number)
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
; AH = 0;
; AX = AX * SER_COMMAND_LENGTH
; AX = AX + OFFSET(KeypressSerialCommandTable)
; CALL SerialPutString

KeypressHandler   PROC    NEAR

KeypressHandlerStart:
    PUSHA                                 ; Save registers

KeypressHandlerGetCommandOffset:
    MOV AH, 0                             ; Clear the event type (only care about
                                          ; event value = key number now)
    MOV CL, SER_COMMAND_LENGTH            ; Each serial command takes 8 bytes
                                          ; (SER_COMMAND_LENGTH) in the command
                                          ; table
    MUL CL                                ; so offset from the beginning of the table
                                          ; will be 8*AL
    MOV CX, CS                            ; Get table location in ES
    MOV ES, CX
    MOV SI, OFFSET(KeypressSerialCommandTable)   ; Get address of command table
    ADD SI, AX                                   ; Add offset from start of table

KeypressHandlerCallSerialPutString:
    CALL SerialPutString                  ; Send the command over serial (over the
                                          ; transmit queue)

KeypressHandlerEnd:
    POPA                                  ; Restore registers and return
    RET

KeypressHandler   ENDP



; SerialReceiveHandler
;
; Description:      This function handles serial receive data events. It takes the
;                   data received and puts it into a display buffer (disp_buffer)
;                   and uses this display buffer as the ‘string’ to pass into the
;                   Display function to show the data on the 14-segment display.
;
; Operation:        This function takes the serial receive data event in AX, with
;                   AH having SER_RECEIVE_CODE and AL having the character we 
;                   are interested in. If the character is not the EOS symbol,
;                   we put the character in the display buffer (disp_buffer) at
;                   the position disp_index and increment disp_index. Then we 
;                   check if the new display index (disp_index) is >=
;                   than the length, DISP_BUFFER_LEN, of the display buffer
;                   (disp_buffer). If it isn’t, we are done, and we return to 
;                   wait for the next character, but if it is, then just set
;                   disp_index to the index of the last character spot in the 
;                   buffer (DSIP_BUFFER_LEN - 1) and then return. This is the 
;                   essentially the part of the function that populates the 
;                   display buffer. If we get the EOS character in AL however,
;                   we want to actually call Display with our display
;                   buffer (but only if we are currently not displaying
;                   an error - check if disp_error = DISP_ERR - if we
;                   are displaying an error, don’t do anything and exit). We
;                   first put an ASCII_NULL in the end of the characters
;                   of the display buffer (which is at index disp_index) to
;                   null terminate the string and then call Display using the
;                   disp_buffer. After this we set disp_index back to
;                   DISP_INDEX_INIT to get ready for the next string to display
;                   and return.
;
; Arguments:        AX = serial receive event 
;                        (AH = SER_RECEIVE_CODE, AL = character)
;
; Return Value:     None.
;
; Local Variables:  None.
; Shared Variables: disp_buffer = buffer that holds string to call Display on
;                   disp_index = index used to write to disp_buffer (read/write)
;                   disp_error = error flag, set if we are displaying an error
;                                message, reset if we ware not (read)
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
; IF AL != SER_EOS
;     disp_buffer[disp_index] = AL
;     disp_index++
;     IF disp_index >= DISP_BUFFER_LEN
;         disp_index = DISP_BUFFER_LEN - 1
; ELSE
;     IF disp_error = DISP_NO_ERROR
;         disp_buffer[disp_index] = ASCII_NULL
;         ES = DS
;         SI = OFFSET(disp_buffer)
;         CALL Display
;         disp_index = DISP_INDEX_INIT


SerialReceiveHandler   PROC    NEAR

SerialReceiveHandlerStart:
    PUSHA                                 ; Save registers

SerialReceiveHandlerCheckEOS:
    CMP AL, SER_EOS                       ; If we got an EOS character (carriage
    JE SerialReceiveHandlerCallDisplay    ; return) we know we want to display the
                                          ; string in the display buffer
    ;JNE SerialReceiveHandlerAddToBuffer  ; Otherwise we just want to add the
                                          ; character to the display buffer

SerialReceiveHandlerAddToBuffer:
    MOV BX, disp_index                    ; Get the current index (index we want to
                                          ; place next character in)
    MOV disp_buffer[BX], AL               ; and put the character at that location in
                                          ; the display buffer
    INC disp_index                        ; Update the index
    CMP disp_index, DISP_BUFFER_LEN       ; If we are still within the buffer (the
                                          ; index is less than the buffer length)
    JL SerialReceiveHandlerEnd            ; then we are done
    MOV disp_index, DISP_BUFFER_LEN - 1   ; Otherwise, set the index to the last
                                          ; position in the display buffer (which is
                                          ; the ninth character, and only the first
                                          ; eight are used in display, so this last
                                          ; character acts as a dump for the command
                                          ; after the first eight characters)
    JMP SerialReceiveHandlerEnd           ; and we are done

SerialReceiveHandlerCallDisplay:
    CMP disp_error, DISP_ERR              ; See if we are displaying an error
    JE SerialReceiveHandlerEnd            ; If we are, we don’t want to update the
                                          ; display, so just break from function
    MOV BX, disp_index                    ; Otherwise, get the current index in buffer
    MOV disp_buffer[BX], ASCII_NULL       ; Terminate the string with ASCII_NULL
    MOV CX, DS                            ; Get buffer segment in ES
    MOV ES, CX
    MOV SI, OFFSET(disp_buffer)           ; and start of buffer in SI
    CALL Display                          ; Call Display with the buffer as argument
    MOV disp_index, DISP_INDEX_INIT       ; Reset the display index to the first digit
                                          ; display for the next command
    ;JMP SerialReceiveHandlerEnd          ; and we are done

SerialReceiveHandlerEnd:
    POPA                                  ; Restore registers and return
    RET

SerialReceiveHandler   ENDP



; SerialErrorHandler
;
; Description:      This function handles serial error events. It just sends
;                   a serial error string to the display.
;
; Operation:        This function calls Display with a serial error string,
;                   ‘SERERR’, and then sets the disp_error shared variable to
;                   DISP_ERR and returns. 
;
; Arguments:        AX = Serial event event (AH = SER_EVENT_CODE, AL not used)
;
; Return Value:     None.
;
; Local Variables:  None.
; Shared Variables: disp_error = error flag, set if we are displaying an error
;                                message, reset if we ware not (read)
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
; CALL Display(‘SERERR’)
; disp_error = DISP_ERROR


SerialErrorHandler   PROC    NEAR

SerialErrorHandlerStart:
    PUSHA                                 ; Save registers

SerialErrorHandlerSendErrorString:
    MOV CX, CS                            ; Get serial error message location in
    MOV ES, CX                            ; ES:SI
    MOV SI, OFFSET(SerialErrorMessage)
    CALL Display                          ; and display the message

SerialErrorHandlerUpdateErrorFlag:
    MOV disp_error, DISP_ERR              ; Update the error flag to show that an
                                          ; error is being displayed

SerialErrorHandlerEnd:
    POPA                                  ; Restore registers and return
    RET

SerialErrorHandler   ENDP



; KeypressSerialCommandTable
; Description: This is a table that contains all of the serial commands
;              that correspond with each key in the keypad. The keys
;              are arranged 0-15, left to right, up to down:
;
;   0 =  Turn 10 degrees counterclockwise
;   1 =  Increase speed by 10000
;   2 =  Turn 10 degrees clockwise
;   3 =  Laser On
;   4 =  Turn 45 degrees counterclockwise
;   5 =  Increase speed by 5000
;   6 =  Turn 45 degrees clockwise
;   7 =  Laser Off
;   8 =  Turn 90 degrees counterclockwise
;   9 =  Stop (speed set to 0)
;   10 = Turn 90 degrees clockwise
;   11 = Reverse direction (180 degree turn)
;   12 = Turn 135 degrees counterclockwise
;   13 = Decrease speed by 5000
;   14 = Turn 135 degrees clockwise
;   15 = Toggle status value (speed, direction, laser)

;
;   The strings are padded with zeroes so that each string has length
;   of a byte.
;
; Revision History:
;       11/30/2015      Anshul Ramachandran     init revision

KeypressSerialCommandTable     LABEL     BYTE

    ; 0 =  Turn 10 degrees counterclockwise
        DB 'D-10   ', ASCII_NULL
    ; 1 =  Increase speed by 10000
        DB 'V+10000', ASCII_NULL
    ; 2 =  Turn 10 degrees clockwise
        DB 'D+10   ', ASCII_NULL
    ; 3 = Laser On
        DB 'F      ', ASCII_NULL
    ; 4 =  Turn 45 degrees counterclockwise
        DB 'D-45   ', ASCII_NULL
    ; 5 =  Increase speed by 5000
        DB 'V+5000 ', ASCII_NULL
    ; 6 =  Turn 45 degrees clockwise
        DB 'D+45   ', ASCII_NULL
    ; 7 = Laser Off
        DB 'O      ', ASCII_NULL
    ; 8 =  Turn 90 degrees counterclockwise
        DB 'D-90   ', ASCII_NULL
    ; 9 =  Stop (speed set to 0)
        DB 'S0     ', ASCII_NULL
    ; 10 =  Turn 90 degrees clockwise
        DB 'D+90   ', ASCII_NULL
    ; 11 =  Reverse direction (180 degree turn)
        DB 'D-180  ', ASCII_NULL
    ; 12 =  Turn 135 degrees counterclockwise
        DB 'D-135  ', ASCII_NULL
    ; 13 =  Decrease speed by 5000
        DB 'V-5000 ', ASCII_NULL
    ; 14 = Turn 135 degrees clockwise
        DB 'D+135  ', ASCII_NULL
    ; 15 = Toggle status
        DB TOGGLE_STATUS, '      ', ASCII_NULL



; RemoteEventHandlersTable
; Description: This is a function table for the event handlers for
;              dequeued events in the remote board main loop.
;
; Revision History:
;       11/30/2015      Anshul Ramachandran     init revision

RemoteEventHandlersTable     LABEL     WORD
    DW   NO_HANDLER                 ; No event handler
    DW   KeypressHandler            ; Key press handler
    DW   SerialReceiveHandler       ; Serial data received handler
    DW   SerialErrorHandler         ; Serial error handler



CODE ENDS



;the data segment

DATA    SEGMENT PUBLIC  'DATA'

    ; buffer to hold string to call Display on
    disp_buffer     DB    DISP_BUFFER_LEN   DUP(?)

    ; index in disp_buffer for writing next character
    disp_index      DW    ?

    ; flag for whether or not we are displaying error message
    disp_error      DB    ?

DATA    ENDS



;the stack

STACK   SEGMENT STACK  'STACK'

                DB      80 DUP ('Stack ')       ;240 words

TopOfStack      LABEL   WORD

STACK   ENDS



        END     START
