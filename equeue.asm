        NAME    EQUEUE

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                  EQUEUE                                    ;
;                          Event Queue Routines                              ;
;                                 EE/CS 51                                   ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This file contains the functions for initializing and using the event queue
; for the RoboTrike
;
;    InitEventQueue: Initializes the event queue
;    EnqueueEvent: Enqueues an event into the event queue
;    DequeueEvent: Dequeues an event from the event queue
;    GetFatalError: Returns whether there was a fatal error in enqueueing or
;                   dequeueing from the event queue
;
; Revision History:
;    11/30/15   Anshul Ramachandran       functional specification

$INCLUDE(queue.inc)
$INCLUDE(events.inc)

CGROUP  GROUP   CODE

CODE    SEGMENT PUBLIC 'CODE'
        ASSUME  CS:CGROUP, DS:DATA

; external function declarations
    EXTRN   QueueInit:NEAR
    EXTRN   QueueEmpty:NEAR
    EXTRN   QueueFull:NEAR
    EXTRN   Enqueue:NEAR
    EXTRN   Dequeue:NEAR

; InitEventQueue
;
; Description:      This function initializes the shared variables used in the event
;                   queue functions (EventQueue and fatal_error)
;
; Operation:        Initialize shared values with their default values as followed:
;                   - EventQueue: initialized by calling QueueInit and using 
;                                  EQUEUE_ELEM_TYPE (word) as element type in BL and
;                                  EQUEUE_LENGTH (512) as the size of the event queue
;                                  in AX
;                   - fatal_error: initialized to NO_FATAL_ERR
;
; Arguments:        None.
;
; Return Value:     None.
;
; Local Variables:  None.
; Shared Variables: EventQueue (write), fatal_error (write)
; Global Variables: None.
;
; Input:            None.
; Output:           None.
;
; Error Handling:   None.
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
; AX = EQUEUE_LENGTH
; BL = EQUEUE_ELEM_TYPE
; SI = OFFSET(EventQueue)
; CALL QueueInit
; fatal_error = NO_FATAL_ERROR


InitEventQueue          PROC    NEAR
                        PUBLIC  InitEventQueue

InitEventQueueStart:
    PUSHA                                 ; Save registers

InitEventQueueSetupForInit:
    MOV AX, EQUEUE_LENGTH                 ; Setup for EventQueue init with a length
    MOV BL, EQUEUE_ELEM_TYPE              ; of EQUEUE_LENGTH and element type of
    MOV SI, OFFSET(EventQueue)            ; EQUEUE_ELEM_TYPE (word)

InitEventQueueCallInit:
    CALL QueueInit                        ; Initialize EventQueue

InitEventQueueInitFatalErrorFlag:
    MOV fatal_error, NO_FATAL_ERR         ; Reset fatal_error flag

InitEventQueueEnd:
    POPA                                  ; Restore registers and return
    RET

InitEventQueue          ENDP



; EnqueueEvent
;
; Description:      This function enqueues an event into the event queue. If the queue
;                   is full, the fatal error flag is set. Otherwise we Enqueue the 
;                   event, which is given in AX.
;
; Operation:        This function first calls QueueFull, and if EventQueue is full,
;                   fatal_error is set to FATAL_ERR, but if not, then Enqueue is
;                   used to enqueue the event in AX.
;
; Arguments:        AX = event (AH = event code, AL = event value)
;
; Return Value:     None.
;
; Local Variables:  None.
; Shared Variables: EventQueue (read/write), fatal_error (write)
; Global Variables: None.
;
; Input:            None.
; Output:           None.
;
; Error Handling:   fatal_error is set if queue is full and try to enqueue
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
; SI = OFFSET(EventQueue)
; CALL QueueFull
; IF (ZF = 1)
;     fatal_error = FATAL_ERROR
; ELSE
;     CALL Enqueue


EnqueueEvent          PROC    NEAR
                      PUBLIC  EnqueueEvent

EnqueueEventStart:
    PUSHA                                 ; Save registers

EnqueueEventCheckQueueFull:
    MOV SI, OFFSET(EventQueue)            ; Get location of EventQueue to check
    CALL QueueFull                        ; if it is full
    JZ EnqueueEventSetFatalErrorFlag      ; If it is, set fatal error flag and exit
    ;JNZ EnqueueEventCallEnqueue          ; Otherwise enqueue passed in event

EnqueueEventCallEnqueue:
    CALL Enqueue                          ; Enqueue event in AX
    JMP EnqueueEventEnd                   ; and return

EnqueueEventSetFatalErrorFlag:
    MOV fatal_error, FATAL_ERR            ; Set fatal error flag
    ;JMP EnqueueEventEnd

EnqueueEventEnd:
    POPA                                  ; Restore registers and return
    RET

EnqueueEvent          ENDP


; DequeueEvent
;
; Description:      This function dequeues an event from the event queue. If the queue
;                   is empty, the carry flag is set and function returns. Otherwise we  
;                   Dequeue an event, and return it in AX.
;
; Operation:        This function first calls QueueEmpty, and if EventQueue is empty,
;                   CF is set, but if not, then CF is reset and Dequeue is used to
;                   dequeue an event and put it in AX.
;
; Arguments:        None.
;
; Return Value:     AX = event (AH = event code, AL = event value)
;                   CF = set if EventQueue was empty, reset otherwise
;
; Local Variables:  None.
; Shared Variables: EventQueue (read/write)
; Global Variables: None.
;
; Input:            None.
; Output:           None.
;
; Error Handling:   CF is set if queue is empty and try to dequeue
;
; Algorithms:       None.
; Data Structures:  Queue
;
; Registers Changed:   CF, AX
;
; Known Bugs:       None.
; Limitations:      None.
;
; Revision History:
;       11/30/2015      Anshul Ramachandran     functional specification
;
; Pseudocode:
;
; SI = OFFSET(EventQueue)
; CALL QueueEmpty
; IF (ZF = 1)
;     STC
; ELSE
;     CLC
;     CALL Dequeue


DequeueEvent          PROC    NEAR
                      PUBLIC  DequeueEvent

DequeueEventStart:
    PUSH SI                               ; Save registers

DequeueEventCheckQueueEmpty:
    MOV SI, OFFSET(EventQueue)            ; Get location of EventQueue to check
    CALL QueueEmpty                       ; if it is empty
    JZ DequeueEventSetCarryFlag           ; If so, set carry flag and exit
    ;JNZ DequeueEventCallDequeue          ; Otherwise dequeue event

DequeueEventCallDequeue:
    CALL Dequeue                          ; Dequeue an event from EventQueue
    CLC                                   ; Clear CF (dequeue worked)
    JMP DequeueEventEnd                   ; and return

DequeueEventSetCarryFlag:
    STC                                   ; Set CF to show EventQueue was empty
    ;JMP DequeueEventEnd

DequeueEventEnd:
    POP SI                                ; Restore registers and return
    RET

DequeueEvent          ENDP



; GetFatalError
;
; Description:      This function returns the fatal error flag in AL
;
; Operation:        Returns fatal_error in AL
;
; Arguments:        None.
;
; Return Value:     AL = fatal_error
;
; Local Variables:  None.
; Shared Variables: fatal_error (read)
; Global Variables: None.
;
; Input:            None.
; Output:           None.
;
; Error Handling:   None.
;
; Algorithms:       None.
; Data Structures:  Queue
;
; Registers Changed:   AL
;
; Known Bugs:       None.
; Limitations:      None.
;
; Revision History:
;       11/30/2015      Anshul Ramachandran     functional specification
;
; Pseudocode:
;
; AL = fatal_error


GetFatalError         PROC    NEAR
                      PUBLIC  GetFatalError

    MOV AH, 0                             ; To be safe, clear AH
    MOV AL, fatal_error                   ; and put fatal error flag in AL
    RET                                   ; and return

GetFatalError         ENDP


CODE ENDS



DATA SEGMENT PUBLIC 'DATA'

    ; event queue itself
    EventQueue          queueSTRUC     <>

    ; flag that is set if there is a fatal error in enqueueing or dequeueing
    ; with the event queue
    fatal_error         DB     ?

DATA ENDS


    END