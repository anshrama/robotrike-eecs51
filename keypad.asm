THIS SHIT IS WRONG GET MOST RECENT COPY

        NAME    KEYPAD

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                   KEYPAD                                   ;
;                              Keypad Routines                               ;
;                                  EE/CS 51                                  ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This file contains methods to initialize the RoboTrike keypad and functionality
; for scanning and debouncing.
;    InitKeypadDebounce: Initialize shared variables used by KeypadDebounce
;    KeypadDebounce: Scans the keypad, debounces keys when called from the timer 2
;                    event handler. If the number of interrupts that have occurred
;                    for a particular key has reached a certain threshold value,
;                    the provided function EnqueueEvent is called.
;
; Revision History:
;       11/1/2015      Anshul Ramachandran     initial rev, func specs./pseudocodes
;       11/4/2015      Anshul Ramachandran     assembly code


$INCLUDE(keypad.inc)
$INCLUDE(events.inc)

CGROUP  GROUP   CODE

CODE    SEGMENT PUBLIC 'CODE'
        ASSUME  CS:CGROUP, DS:DATA

; External references
    EXTRN   EnqueueEvent:NEAR


; InitKeypadDebounce
;
; Description:      This function initializes the shared variable key_counts that is
;                   used in the KeypadDebounce function and keeps track of the 
;                   number of consecutive interrupts that have happened since each key
;                   has been pressed. Each key count in key_counts is initialized
;                   to INIT_COUNT since we will count down from this value
;                   during each interrupt until 0, which will signal when to call
;                   EnqueueEvent.
;
; Operation:        Loops through the values in shared variable key_counts and 
;                   initializes each value to INIT_COUNT
;
; Arguments:        None.
;
; Return Value:     None.
;
; Local Variables:  None.
;
; Shared Variables: key_counts = shared array (really a 4x4 table in one dimension)
;                                that keeps track of how many timer interrupts each
;                                key has been pressed down for
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
; Registers Used:   None.
;
; Known Bugs:       None.
; Limitations:      INIT_COUNT is maximum one byte in size
;
; Revision History:
;       11/1/2015      Anshul Ramachandran     func spec and pseudocode
;       11/4/2015      Anshul Ramachandran     assembly code

InitKeypadDebounce    PROC    NEAR
                      PUBLIC  InitKeypadDebounce

SaveRegistersForInitKeypadDebounce:
    PUSH DI

SetupForInitKeypadCounts:
    MOV DI, 0                             ; start DI at first key in NUM_KEYS sized
                                          ; keypad_counts array

InitKeypadCounts:
    MOV key_counts[DI], INIT_COUNT
    INC DI

LoopInitKeypadCounts:
    CMP DI, NUM_KEYS
    JGE InitCurrRow
    JMP InitKeypadCounts

InitCurrRow:
    MOV curr_row, 0

InitKeypadCountsFinished:
RestoreRegistersForInitKeypadDebounce:
    POP DI

    RET

InitKeypadDebounce    ENDP


; KeypadDebounce
;
; Description:      This function scans through all of the keys and debounces the
;                   keys after each time timer 2 event handler is called. For 
;                   every key on the keypad, it checks if it is pressed or not.
;                   If pressed, function checks for debouncing, and enqueues using
;                   EnqueueEvent accordingly. If not pressed, then function sets
;                   number of interrupts before enqueuing back to its default value.
;                   Multiple key presses are treated equivalently to enqueueing
;                   events corresponding to the individual keys very rapidly.
;
; Operation:        This function is called from Timer2EventHandler. It loops over
;                   the rows of the keypad, and within each row, it loops over
;                   the keys within the row to check for key presses/debouncing.
;                   A key is “pressed” if its bit in its row is cleared. To make
;                   sure that a key is intentionally pressed, the default value
;                   for a key in key_counts (shared variable that counts down
;                   number of interrupts for each key that must pass before we 
;                   actually call EnqueueEvent) is INIT_COUNT. For every
;                   key, this function checks if it is not pressed (in which case
;                   their its value in key_counts goes back to INIT_COUNT,
;                   since it needs that many timer interrupts again until EnqueueEvent
;                   will be called for that key). If it is pressed, then the value
;                   for that key in key_counts is decremented and if this value
;                   reaches 0 (a sufficient number of interrupts have passed for us
;                   to deem the press as an intentional one), then we call 
;                   EnqueueEvent on that key. If the key_counts value is zero
;                   then it is set to REPEAT_COUNT and the decrementing starts again
;                   in order to allow for generating more EnqueueEvent calls for a
;                   particular key in one long press of the key.
;
; Arguments:        None.
;
; Return Value:     None.
;
; Local Variables:  None.
;
; Shared Variables: key_counts = shared array (really a 4x4 table in one dimension)
;                                that keeps track of how many timer interrupts each
;                                key has been pressed down for
;
; Global Variables: None.
;
; Input:            a keypad with NUM_KEYS keys put into NUM_ROWS rows
; Output:           None.
;
; Error Handling:   None.
;
; Algorithms:       None.
; Data Structures:  None.
;
; Registers Used:   None.
;
; Known Bugs:       None.
; Limitations:      None.
;
; Revision History:
;       11/1/2015      Anshul Ramachandran     func spec and pseudocode
;       11/4/2015      Anshul Ramachandran     assembly code

KeypadDebounce    PROC    NEAR
                  PUBLIC  KeypadDebounce

SaveRegistersForKeypadDebounce:
    PUSH AX
    PUSH CX
    PUSH DX
    PUSH DI

SetupForKeypadDebounce:
    MOV DI, curr_row*4 - 4
    MOV DX, KEYPAD_ADDRESS
    ADD DX, curr_row

GetKeypadRowBits:
    IN AL, DX
    AND AL, ROW_MASK

SetupForRowDebounce:
    MOV CL, KEY_START_MASK

CheckKey:
    PUSH AX
    AND AL, CL
    JNZ RestoreKey
    ;JZ DebounceKey

DebounceKey:
    DEC key_counts[DI]
    JNZ SetupForNextKey
    ;JZ SetupForEnqueueEvent

SetupForEnqueueEvent:
    MOV key_counts[DI], REPEAT_COUNT
    MOV AX, DI
    MOV AH, KEY_EVENT_CODE

CallEnqueueEvent:
    PUSHA
    CALL EnqueueEvent
    POPA

FinishEnqueueEvent:
    JMP SetupForNextKey

RestoreKey:
    MOV key_counts[DI], INIT_COUNT
    ;JMP SetupForNextKey

SetupForNextKey:
    INC DI
    POP AX
    SHR CL, 1
    CMP CL, KEY_END_MASK
    JE SetupForNextRow
    JMP CheckKey

SetupForNextRow:
    INC curr_row

FinishKeypadDebounce:
RestoreRegistersForKeypadDebounce:
    POP DI
    POP DX
    POP CX
    POP AX

    RET

KeypadDebounce    ENDP


CODE ENDS


DATA SEGMENT PUBLIC 'DATA'

    ; Array that keeps track of number of interrupts that have elapsed since each
    ; key in the keypad has been pressed down
    key_counts     DB   NUM_KEYS   DUP (?)

    ; Current row being checked on interrupt
    curr_row       DB   ?
    
DATA ENDS


    END