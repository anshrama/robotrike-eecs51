        NAME    TIMER2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                   TIMER2                                   ;
;                               Timer Functions                              ;
;                                  EE/CS 51                                  ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; This file contains the functions to initialize and install the timer and event 
; handler for the LED display.
;       Timer2EventHandler: Handles the timer interrupt and calls DisplayMux, the
;                           mixing function for the display
;       InitTimer2: initializes 80188 timers
;       InstallTimer2EventHandler: Install event handler for timer interrupts
;
; Revision History:
;     10/25/15  Anshul Ramachandran      initial rev, func. specs./pseudocodes
;     10/29/15  Anshul Ramachandran      assembly code

$INCLUDE(timer2.inc)


CGROUP  GROUP   CODE


CODE       SEGMENT PUBLIC 'CODE'

        ASSUME  CS:CGROUP


; external function declarations
        EXTRN   DisplayMux:NEAR
        EXTRN   KeypadDebounce:NEAR


; Timer2EventHandler
;
;
; Description:    This procedure is the event handler for the timer 2 interrupt. It
;                 calls DisplayMux to handle multiplexing the display.
;
; Operation:      Calls DisplayMux to handle multiplexing the display and then sends
;                 out EOIs to signify end of the interrupt handler execution.
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
;     10/25/2015   Anshul Ramachandran   func. spec. and pseudocode
;     10/29/2015   Anshul Ramachandran   assembly code


Timer2EventHandler     PROC        NEAR
                       PUBLIC      Timer2EventHandler

    PUSH DX
    PUSH AX

    CALL DisplayMux
    CALL KeypadDebounce

    MOV DX, INTCtrlrEOI       ; Send timer EOI
    MOV AX, TimerEOI
    OUT DX, AL

    POP AX
    POP DX
    IRET                      ; IRET must be used in interrupt handler


Timer2EventHandler     ENDP



; InitTimer2
;
;
; Description:    Initialize the 80188 timer #2. The interrupt controller
;                 is also initialized to allow the timer 2 interrupts.
;
; Operation:      The appropriate values are written to the timer 2 control
;                 register in the PCB.  Also, the timer 2 count registers
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
;     10/25/15     Anshul Ramachandran   func. spec. and pseudocode
;     10/29/2015   Anshul Ramachandran   assembly code

InitTimer2               PROC        NEAR
                         PUBLIC      InitTimer2

        PUSH    DX
        PUSH    AX

        MOV     DX, Tmr2Count   ;initialize the count register to 0
        XOR     AX, AX
        OUT     DX, AL

        MOV     DX, Tmr2MaxCnt  ;setup max count for 1ms counts
        MOV     AX, ONE_MS_CNT
        OUT     DX, AL

        MOV     DX, Tmr2Ctrl    ;setup the control register
        MOV     AX, Tmr2CtrlVal
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

InitTimer2               ENDP



; InstallTimer2EventHandler
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
;     10/25/2015   Anshul Ramachandran   func. spec. and pseudocode
;     10/29/2015   Anshul Ramachandran   assembly code

InstallTimer2EventHandler          PROC        NEAR
                                   PUBLIC      InstallTimer2EventHandler

        PUSH    AX
        PUSH    ES

        XOR     AX, AX                  ;clear ES (interrupt vectors are in segment 0)
        MOV     ES, AX
                                        ;store the vector
        MOV     ES: WORD PTR (4 * Tmr2Vec), OFFSET(Timer2EventHandler)
        MOV     ES: WORD PTR (4 * Tmr2Vec + 2), SEG(Timer2EventHandler)

        POP     ES
        POP     AX

        RET                             ;all done, return

InstallTimer2EventHandler          ENDP




CODE    ENDS


        END