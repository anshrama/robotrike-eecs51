THIS SHIT IS WRONG GET MOST RECENT COPY

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                  KEYPAD.INC                                ;
;                       Include File For Keypad Constants                    ;
;                                   EE/CS 51                                 ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This file contains constants used in keypad.asm for purposes of keypad 
; scanning and debouncing
;
; Revision History:
;     11/5/15   Anshul Ramachandran      initial revision

NUM_KEYS            EQU    0008H    ; Number of keys on the keypad (4x4)
NUM_ROWS            EQU    0000H    ; Number of rows that the keys are arranged in

INIT_COUNT          EQU    50       ; Initial number of interrupts needed before
                                    ; calling EnqueueEvent for a key (interrupts
                                    ; needed before we can say press is intentional)
REPEAT_COUNT        EQU    300      ; Number of interrupts needed before repeating a
                                    ; call to EnqueueEvent for a key (the key is
                                    ; pressed down and EnqueueEvent has already
                                    ; been called at least once

ROW_MASK            EQU    00001111B    ; Mask to get last four bits of keypad
                                        ; value (bits that correspond to the keys)
KEY_START_MASK      EQU    00001000B    ; We start by first masking for the first
                                        ; key, and shift this right, masking for
                                        ; the other keys in the row
KEY_END_MASK        EQU    00000000B    ; After four shifts (all keys in the
                                        ; row have been masked for) we reach
                                        ; this mask, so after each shift we compare
                                        ; to this mask to see if we are done with
                                        ; the row

KEYPAD_ADDRESS      EQU    80H      ; I/O address of first row of keypad (next
                                    ; three rows are 81H, 82H, and 83H)