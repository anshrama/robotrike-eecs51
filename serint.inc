;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                  SERINT.INC                                ;
;                      Include File For Serial Interrupts                    ;
;                                   EE/CS 51                                 ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This file contains constants used in serint.asm and serial.asm regarding
; functions using serial interrupts (INT2)
;
; Revision History:
;     11/20/15   Anshul Ramachandran      initial revision

; Serial interrupt settings
Int2CtrlReg            EQU    0FF3CH     ; INT 2 control register address
Int2CtrlVal            EQU    00005H     ; 00000000000----- reserved
                                         ; -----------0---- edge triggering
                                         ; ------------0--- enable interrupt
                                         ; -------------101 priority 5
INTCtrlrEOI            EQU    0FF22H     ; EOI address
INT2EOI                EQU    0000EH     ; Value of INT2 EOI
Int2Vec                EQU    14         ; Interrupt vector for INT2 handler