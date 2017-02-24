;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                  EVENTS.INC                                ;
;                     Include File For Event Related Items                   ;
;                                   EE/CS 51                                 ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This file contains event codes used with EnqueueEvent in files keypad.asm
; and serial.asm
;
; Revision History:
;     11/20/15   Anshul Ramachandran      initial revision

; Event codes
KEY_EVENT_CODE       EQU    01H      ; for keypad press event
SER_RECEIVE_CODE     EQU    02H      ; for serial received data interrupt
SER_ERROR_CODE       EQU    03H      ; for serial receiver line status interrupt

; Event handler
NO_HANDLER           EQU    0        ; no handler (needed as buffer for jump
                                     ; table of handlers in main loops)

; Event queue
EQUEUE_LENGTH        EQU    512      ; Size of event queue
EQUEUE_ELEM_TYPE     EQU    2        ; Each event is a word
NO_FATAL_ERR         EQU    0        ; value of fatal_error when no fatal errors
FATAL_ERR            EQU    1        ; value of fatal_error when fatal error
                                     ; occurs in enqueueing or dequeueing