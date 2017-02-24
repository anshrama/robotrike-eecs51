;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                 DISPLAY.INC                                ;
;                      Include File For Display Constants                    ;
;                                   EE/CS 51                                 ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This file contains constants used in display.asm for purposes of modifying and
; updating the LED display
;
; Revision History:
;     10/29/15  Anshul Ramachandran      initial revision

LEDDisplayLow       EQU    0000H    ; Address of the first physical digit display
LEDDisplayHigh      EQU    0008H    ; Address of the place to write the modifier to the 
                                    ; low byte pattern to
NUM_DIGITS          EQU    8        ; Number of physical digit displays
BLANK_DIGIT         EQU    0        ; Value used to clear a physical digit display