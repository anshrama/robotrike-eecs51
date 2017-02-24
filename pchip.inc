;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                  PCHIP.INC                                 ;
;                       Include File For Parallel Chip                       ;
;                                   EE/CS 51                                 ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This file contains constants used for dealing with the parallel chip on the
; target board
;
; Revision History:
;     11/13/15   Anshul Ramachandran      initial revision

; Parallel port constants
PORT_CLEAR             EQU    0         ; Value used to clear ports
PORTB                  EQU    181H      ; Location of port B
CONTROL_WORD           EQU    183H      ; Location of control word
CONTROL_VALUE          EQU    80H       ; Value to write to control word 
                                        ; = 1000000B

