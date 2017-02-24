        NAME    TIMER0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                  TIMER0                                    ;
;                              Timer0 Functions                              ;
;                                 EE/CS 51                                   ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; This file contains the functions to initialize and install the timer and event 
; handler for the RoboTrike’s motors/laser
;       Timer0EventHandler: Handles the timer interrupt and calls PWMHandler to
;                           change the values sent to port B of the target board
;                           which controls the motors/laser
;       InitTimer0: initializes 80188 timer 0
;       InstallTimer0EventHandler: Install event handler for timer interrupts
;
; Revision History:
;       11/7/2015      Anshul Ramachandran     initial rev, func specs./pseudocodes

$INCLUDE(timer0.inc)


CGROUP  GROUP   CODE


CODE       SEGMENT PUBLIC 'CODE'

        ASSUME  CS:CGROUP


; external function declarations
        EXTRN   PWMHandler:NEAR


; Timer0EventHandler
;
;
; Description:    This procedure is the event handler for the timer 2 interrupt. It
;                 calls PWMHandler to handle sending the correct bits to port B
;                 of the target board, which controls the motors and laser of the
;                 RoboTrike
;
; Operation:      Calls PWMHandler to handle sending the correct bits to port B
;                 of the target board, which controls the motors and laser of the
;                 RoboTrike and then sends out EOIs to signify end of the interrupt
;                 handler execution.
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
;     11/7/2015   Anshul Ramachandran   func. spec. and pseudocode


Timer0EventHandler     PROC        NEAR
                       PUBLIC      Timer0EventHandler

    PUSH DX
    PUSH AX

    CALL PWMHandler           ; Call PWM handling function, which sends
                              ; the current motor/laser bit patterns to
                              ; port B of the target board

    MOV DX, INTCtrlrEOI       ; Send timer EOI
    MOV AX, TimerEOI
    OUT DX, AL

    POP AX
    POP DX
    IRET                      ; IRET must be used in interrupt handler


Timer0EventHandler     ENDP



; InitTimer0
;
;
; Description:    Initialize the 80188 timer #0. The interrupt controller
;                 is also initialized to allow the timer 0 interrupts.
;
; Operation:      The appropriate values are written to the timer 0 control
;                 register in the PCB.  Also, the timer 0 count registers
;                 are reset to zero.  Finally, the interrupt controller is
;                 setup to accept timer interrupts and any pending
;                 interrupts are cleared by sending a TimerEOI to the
;                 interrupt controller.
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
; Registers Used:   AX, DX
;
; Algorithms:       None.
; Data Structures:  None.
;
; Known Bugs:       None.
; Limitations:      None.
;
; Revision History: 
;     11/7/2015   Anshul Ramachandran   func. spec. and pseudocode

InitTimer0               PROC        NEAR
                         PUBLIC      InitTimer0

        PUSH    DX
        PUSH    AX

        MOV     DX, Tmr0Count   ;initialize the count register to 0
        XOR     AX, AX
        OUT     DX, AL

        MOV     DX, Tmr0MaxCnt  ;setup max count for 1ms counts
        MOV     AX, CNT_32_MS
        OUT     DX, AL

        MOV     DX, Tmr0Ctrl    ;setup the control register
        MOV     AX, Tmr0CtrlVal
        OUT     DX, AL


        MOV     DX, INTCtrlrCtrl;setup the interrupt control register
        MOV     AX, INTCtrlrCVal
        OUT     DX, AL

        MOV     DX, INTCtrlrEOI ;send an EOI to turn off any pending interrupts
        MOV     AX, TimerEOI
        OUT     DX, AL

        POP AX
        POP DX

        RET                     ;done so return

InitTimer0               ENDP



; InstallTimer0EventHandler
;
;
; Description:    Install event handler for the timer interrupt
;
; Operation:      Write address of the timer event handler to the appropriate
;                 interrupt vector
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
; Registers Used:   flags, AX, ES
;
; Algorithms:       None.
; Data Structures:  None.
;
; Known Bugs:       None.
; Limitations:      None.
;
; Revision History: 
;     11/7/2015   Anshul Ramachandran   func. spec. and pseudocode

InstallTimer0EventHandler          PROC        NEAR
                                   PUBLIC      InstallTimer0EventHandler

        PUSH    AX
        PUSH    ES

        XOR     AX, AX                  ;clear ES (interrupt vectors are in segment 0)
        MOV     ES, AX
                                        ;store the vector
        MOV     ES: WORD PTR (4 * Tmr0Vec), OFFSET(Timer0EventHandler)
        MOV     ES: WORD PTR (4 * Tmr0Vec + 2), SEG(Timer0EventHandler)

        POP     ES
        POP     AX

        RET                             ;all done, return

InstallTimer0EventHandler          ENDP




CODE    ENDS


        END