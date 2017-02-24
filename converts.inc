;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                              CONVERTS.INC                                  ;
;                    Include File For Conversion Functions                   ;
;                                EE/CS 51                                    ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This file contains the constants used in converts.asm
; Revision History:
;     10/15/15  Anshul Ramachandran      initial revision

; DEFINITIONS
ASCII_NULL          EQU          0
PWR10_DEC           EQU          10000
MASK_HEX            EQU          0F000H
INIT_BITSHIFT       EQU          12