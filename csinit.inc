;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                 CSINIT.INC                                 ;
;                      Include File For CS Initialization                    ;
;                                   EE/CS 51                                 ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This file contains the constants used in csinit.asm
;
; Revision History:
;     10/29/15  Anshul Ramachandran      initial revision

; Chip Select PACS and MPCS register addresses
PACSreg         EQU     0FFA4H          ; PACS register
MPCSreg         EQU     0FFA8H          ; MPCS register

; Control Register Values
PACSval         EQU     00003H          ;0000000000------  starts at address 0
                                        ;----------000---  reserved
                                        ;-------------0--  wait for RDY inputs
                                        ;--------------11  3 wait states
                                        ;0000000000000011  TOTAL

MPCSval         EQU     00183H          ;0---------000---  reserved
                                        ;-0000001--------  MCS is 8KB
                                        ;--------1-------  we do output PCS5/PCS6
                                        ;---------0------  PCS in I/O space
                                        ;-------------0--  wait for RDY inputs
                                        ;--------------11  3 wait states
                                        ;0000000110000011  TOTAL
