        NAME    SERINT

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                  SERINT                                    ;
;                        Serial Interrupt Functions                          ;
;                                  EE/CS 51                                  ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; This file contains the functions to initialize the serial interrupts and
; install the serial event handler
;    InstallSerialHandler: Installs serial event handler
;    InitSerialInterrupts: Initializes serial interrupts
;
; Revision History:
;     11/20/15     Anshul Ramachandran      initial rev


$INCLUDE(serint.inc)


CGROUP  GROUP   CODE


CODE       SEGMENT PUBLIC 'CODE'

        ASSUME  CS:CGROUP

; External references
    EXTRN   SerialHandler:NEAR


; InstallSerialHandler
;
; Description:      This function installs the serial event handler for serial
;                   interrupts (INT2 interrupts)
;
; Operation:        Writes the INT2 event handler address to interrupt vector Int2Vec
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
;       11/14/2015      Anshul Ramachandran     func spec and assembly code

InstallSerialHandler  PROC    NEAR
                      PUBLIC  InstallSerialHandler

    PUSH    AX
    PUSH    ES

    XOR     AX, AX                  ;clear ES (interrupt vectors are in segment 0)
    MOV     ES, AX
                                    ;store the vector
    MOV     ES: WORD PTR (4 * Int2Vec), OFFSET(SerialHandler)
    MOV     ES: WORD PTR (4 * Int2Vec + 2), SEG(SerialHandler)

    POP     ES
    POP     AX

    RET                             ;all done, return

InstallSerialHandler  ENDP


; InitSerialInterrupts
;
; Description:      Initialize the interrupt controller to allow serial interrupts
;                   (INT2 interrupts)
;
; Operation:        Start accepting interrupts and clear pending interrupts with EOIs
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
; Output:           INT2 controller
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
;       11/14/2015      Anshul Ramachandran     func spec and assembly code

InitSerialInterrupts  PROC    NEAR
                      PUBLIC  InitSerialInterrupts

    PUSHA
    PUSHF

    MOV     DX, Int2CtrlReg   ;setup the interrupt control register
    MOV     AX, Int2CtrlVal
    OUT     DX, AL

    MOV     DX, INTCtrlrEOI   ;send an EOI to turn off any pending interrupts
    MOV     AX, INT2EOI
    OUT     DX, AL

    POPF
    POPA

    RET

InitSerialInterrupts  ENDP


CODE    ENDS


        END