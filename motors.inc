;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                  MOTORS.INC                                ;
;                       Include File For Motor Constants                     ;
;                                   EE/CS 51                                 ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This file contains constants used in motors.asm for purposes of functions
; regarding the target board/motor unit of RoboTrike
;
; Revision History:
;     11/7/15   Anshul Ramachandran      initial revision
;     11/21/15  Anshul Ramachandran      added constants needed for serial parser


; Angle constants
ZERO_DEGREES           EQU    0
MAX_DEGREES            EQU    360        ; We always want dir between
                                         ; 0 and 360
DIR_NO_CHANGE          EQU    -32768     ; Value passed in for ‘new’ dir
                                         ; if we don’t want to change dir

; Speed constants
ZERO_SPEED             EQU    0
MAX_SPEED              EQU    65534      ; Max speed
SPEED_NO_CHANGE        EQU    65535      ; Value passed in for ‘new’ speed
                                         ; if we don’t want to change speed

; Laser constants
OFF                    EQU    0          ; Laser off value
ON                     EQU    1          ; Laser on value
LASER_ON_MASK          EQU    10000000B  ; OR bit used to turn on laser bit
                                         ; in PWMHandler

; Turret constants
MIN_TURR_ELEV          EQU    -60        ; Max elevation angle of turret
MAX_TURR_ELEV          EQU    60         ; Min elevation angle of turret

; PWM constants
COUNTER_CLEAR          EQU    0          ; Initial value of pw_counter (start
                                         ; from beginning of pulse width
NUM_MOTORS             EQU    3          ; Number of motors
PULSE_WIDTH_START      EQU    0          ; Start of pulse width
PULSE_WIDTH_END        EQU    127        ; End of pulse width
                                         ; (pw_counter loops between the start
                                         ; and end pulse width constants)

