        NAME    PCHIP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                   PCHIP                                    ;
;                               Parallel Chip                                ;
;                                  EE/CS 51                                  ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This file contains the function for initializing the parallel chip on the 
; target board.
;
;    InitParallel: Initializes the parallel chip on the target board
;
; Revision History:
;       11/13/2015     Anshul Ramachandran     initial revision

$INCLUDE(pchip.inc)

CGROUP  GROUP   CODE

CODE    SEGMENT PUBLIC 'CODE'
        ASSUME  CS:CGROUP

; InitParallel
;
; Description:      This function initializes the parallel chip on the target board.
;
; Operation:        This function first sets the control word for the parallel chip
;                   and then initializes port B
;
; Arguments:        None.
;
; Return Value:     None.
;
; Local Variables:  None.
; Shared Variables: None.
; Global Variables: None.
;
; Input:            None.
; Output:           None.
;
; Error Handling:   None.
;
; Algorithms:       None.
; Data Structures:  None.
;
; Registers Changed:   None.
;
; Known Bugs:       None.
; Limitations:      None.
;
; Revision History:
;       11/13/2015     Anshul Ramachandran     func spec and assembly

InitParallel          PROC    NEAR
                      PUBLIC  InitParallel

SaveRegistersInitParallel:
    PUSH DX                               ; Save registers
    PUSH AX

InitControlWord:
    MOV DX, CONTROL_WORD                  ; Put address of control word in DX
    MOV AX, CONTROL_VALUE                 ; and value of control word in AX
    OUT DX, AL                            ; and output

InitPortB:
    MOV DX, PORTB                         ; Put address of port B in DX and
    MOV AX, PORT_CLEAR                    ; clear it by outputting PORT_CLEAR
    OUT DX, AL

RestoreRegistersInitParallel:
    POP AX                                ; Restore registers
    POP DX

    RET

InitParallel          ENDP


CODE ENDS

    END