        NAME    CSINIT

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                  CSINIT                                    ;
;                        Chip Select Initialization                          ;
;                                  EE/CS 51                                  ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; This file contains the method to initialize the chip selects for the 80188
;       InitCS: initialize the chip selects on the 80188
;
; Revision History:
;     10/25/15  Anshul Ramachandran      initial rev, func. specs./pseudocodes
;     10/29/15  Anshul Ramachandran      assembly code

$INCLUDE(csinit.inc)


CGROUP  GROUP   CODE


CODE       SEGMENT PUBLIC 'CODE'

        ASSUME  CS:CGROUP


; InitCS
;
;
; Description:    Initialize the peripheral chip selects on the 80188
;
; Operation:      Write the initial values to the PACS and MPCS registers
;
; Arguments:        None.
;
; Return Values:    None.
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
; Registers Used:   None.
;
; Algorithms:       None.
; Data Structures:  None.
;
; Known Bugs:       None.
; Limitations:      None.
;
; Revision History: 
;     10/25/15   Anshul Ramachandran     func. spec. and pseudocode
;     10/29/15   Anshul Ramachandran      assembly code

InitCS            PROC        NEAR
                  PUBLIC      InitCS

        PUSH    AX
        PUSH    DX

        MOV     DX, PACSreg     ;setup to write to PACS register
        MOV     AX, PACSval
        OUT     DX, AL          ;write PACSval to PACS (base at 0, 3 wait states)

        MOV     DX, MPCSreg     ;setup to write to MPCS register
        MOV     AX, MPCSval
        OUT     DX, AL          ;write MPCSval to MPCS (I/O space, 3 wait states)

        POP     DX
        POP     AX

        RET                     ;done so return

InitCS            ENDP



CODE    ENDS


        END