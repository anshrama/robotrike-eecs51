;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                 RMTMAIN.INC                                ;
;                     Include File For Remote Main Loop                      ;
;                                   EE/CS 51                                 ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This file contains constants used in the main loop of the remote board for
; the RoboTrike
;
; Revision History:
;     11/30/15   Anshul Ramachandran      initial revision


SER_COMMAND_LENGTH   EQU    8        ; Number of bytes in a serial command in
                                     ; KeypressSerialCommandTable

DISP_INDEX_INIT      EQU    0        ; Initial digit display is the first
                                     ; (index 0)

DISP_NO_ERR          EQU    0        ; No error message being displayed
DISP_ERR             EQU    1        ; Error message being displayed

DISP_BUFFER_LEN      EQU    9        ; Length of buffer used to store 
                                     ; characters from serial