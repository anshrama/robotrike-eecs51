;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                 MTRMAIN.INC                                ;
;                     Include File For Target Main Loop                      ;
;                                   EE/CS 51                                 ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This file contains constants used in the main loop of the target board for
; the RoboTrike
;
; Revision History:
;     11/30/15   Anshul Ramachandran      initial revision

SPEED_ACCESS         EQU    0
DIR_ACCESS           EQU    1
LASER_ACCESS         EQU    2

TRANSMIT_BUFFER_LEN  EQU    9        ; Length of buffer used to store 
                                     ; characters to send over serial