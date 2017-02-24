;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                 INTRPTS.INC                                ;
;                      Include File For Interrupt Functions                  ;
;                                   EE/CS 51                                 ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This file contains the definitions for the interrupt controller since the timing 
; functions use them.
;
; Revision History:
;     10/29/15  Anshul Ramachandran      initial revision


; EOI signals
INTCtrlrEOI       EQU     0FF22H     ; Address of interrupt controller EOI register
NonSpecEOI        EQU     08000H     ; Non-specific EOI command


; General definitions for ClrIRQVectors
FIRST_RESERVED_VEC     EQU     1        ; Reserve vectors 1-3
LAST_RESERVED_VEC      EQU     3
NUM_IRQ_VECTORS        EQU     256      ; Number of interrupt vectors