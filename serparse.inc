;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                 SERPARSE.INC                               ;
;                        Include File For Serial Parser                      ;
;                                   EE/CS 51                                 ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This file contains constants for the Mealy state machine and other functions
; related to the serial parser (serparse.asm)
;
; Revision History:
;     11/21/15   Anshul Ramachandran      initial revision

; Sign constants
POSITIVE          EQU   1              ; Default sign_flag (+)
NEGATIVE          EQU   -1             ; Negative sign_flag (-)

; Argument constants
INIT_ARG          EQU   0              ; Init value of curr_arg (digits are
                                       ; added to this

; Parse constants
NO_PARSE_ERROR    EQU   0              ; Default error_flag value
PARSE_ERROR       EQU   1              ; If we get an error of any kind (invalid
                                       ; state transition, overflow, out of
                                       ; bounds, etc), error_flag set to this
TOKEN_MASK        EQU   01111111B      ; Used to make sure token is between
                                       ; 0 and 127 (range of tokens in token
                                       ; tables)

; States
ST_INIT           EQU   0              ; Initial idle state
ST_D_DIGIT        EQU   1              ; Parsing D command digit
ST_E_DIGIT        EQU   2              ; Parsing E command digit
ST_S_DIGIT        EQU   3              ; Parsing S command digit
ST_T_ABS_DIGIT    EQU   4              ; Parsing T command unsigned digit
ST_T_REL_DIGIT    EQU   5              ; Parsing T command signed digit
ST_V_DIGIT        EQU   6              ; Parsing V command digit
ST_D_SIGN         EQU   7              ; Parsing D command sign
ST_E_SIGN         EQU   8              ; Parsing E command sign
ST_S_SIGN         EQU   9              ; Parsing S command sign
ST_T_REL_SIGN     EQU   10             ; Parsing T command signed sign
ST_V_SIGN         EQU   11             ; Parsing V command sign
ST_DIR            EQU   12             ; Start parsing D command
ST_ELEV           EQU   13             ; Start parsing E command
ST_ONLASER        EQU   14             ; Start parsing F command
ST_OFFLASER       EQU   15             ; Start parsing O command
ST_ABSSPEED       EQU   16             ; Start parsing S command
ST_TURRANGLE      EQU   17             ; Start parsing T command
ST_RELSPEED       EQU   18             ; Start parsing V command
ST_ERROR          EQU   19             ; Error state

; Tokens
NUM_TOKENS        EQU   12
TOKEN_DIGIT       EQU   0              ; Digit token (0-9)
TOKEN_SIGN        EQU   1              ; Sign token (+/-)
TOKEN_DIR         EQU   2              ; D
TOKEN_ELEV        EQU   3              ; E
TOKEN_ONLASER     EQU   4              ; F
TOKEN_OFFLASER    EQU   5              ; O
TOKEN_ABSSPEED    EQU   6              ; S
TOKEN_TURRANGLE   EQU   7              ; T
TOKEN_RELSPEED    EQU   8              ; V
TOKEN_IGNORE      EQU   9              ; tab/space
TOKEN_RETURN      EQU   10             ; Carriage return
TOKEN_OTHER       EQU   11             ; Unused character

; Serial characters for getting status for speed, dir, and laser
TOGGLE_STATUS     EQU   'X'            ; Toggle status requested
                                       ;   Speed status -> Dir status
                                       ;   Dir status -> Laser status
                                       ;   Laser status -> Speed status
SPEED_CHAR        EQU   'S'            ; Char to prepend speed value
DIR_CHAR          EQU   'D'            ; Char to prepend direction value
LASER_CHAR        EQU   'L'            ; Char to prepend laser value  