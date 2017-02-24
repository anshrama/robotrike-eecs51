        NAME    INTRPTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                  INTRPTS                                   ;
;                             Interrupt Functions                            ;
;                                  EE/CS 51                                  ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; This file contains the functions to initialize the interrupt vector table and
; to handle illegal events
;       ClrIRQVectors: Installs IllegalEventHandler for all interrupt vectors in the
;                      interrupt vector table.
;       IllegalEventHandler: Event handler for illegal (uninitialized) interrupts
;
; Revision History:
;     10/25/15  Anshul Ramachandran      initial rev, func. specs./pseudocodes
;     10/29/15  Anshul Ramachandran      assembly code


$INCLUDE(intrpts.inc)


CGROUP  GROUP   CODE


CODE       SEGMENT PUBLIC 'CODE'

        ASSUME  CS:CGROUP


; ClrIRQVectors
;
;
; Description:    This functions installs the IllegalEventHandler for all
;                 interrupt vectors in the interrupt vector table.  Note
;                 that all 256 vectors are initialized so the code must be
;                 located above 400H.  The initialization skips  (does not
;                 initialize vectors) from vectors FIRST_RESERVED_VEC to
;                 LAST_RESERVED_VEC.
;
; Operation:      Setup to store the same handler 256 times, then loop through
;                 each vector and clear it, checking whether we should store the
;                 vector or not if it is before the start of the reserved field or
;                 past the reserved field.
;
; Arguments:        None.
;
; Return Values:    None.
;
; Local Variables:  CX - vector counter
;                   ES:SI - pointer to vector table
; Shared Variables: None.
; Global Variables: None.
;
; Input:            None.
; Output:           None.
;
; Error Handling:   None.
;
; Registers Used:   flags, AX, CX, SI, ES
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

ClrIRQVectors     PROC        NEAR
                  PUBLIC      ClrIRQVectors

InitClrVectorLoop:              ;setup to store the same handler 256 times
        PUSHA                   ;save registers
        XOR     AX, AX          ;clear ES (interrupt vectors are in segment 0)
        MOV     ES, AX
        MOV     SI, 0           ;initialize SI to skip RESERVED_VECS (4 bytes each)

        MOV     CX, NUM_IRQ_VECTORS         ;up to 256 vectors to initialize


ClrVectorLoop:                  ;loop clearing each vector
				     ;check if should store the vector
	CMP     SI, 4 * FIRST_RESERVED_VEC
	JB	DoStore	     ;if before start of reserved field - store it
	CMP	SI, 4 * LAST_RESERVED_VEC
	JBE	DoneStore	     ;if in the reserved vectors - don't store it
	;JA	DoStore            ;otherwise past them - so do the store

DoStore:                        ;store the vector
        MOV     ES: WORD PTR [SI], OFFSET(IllegalEventHandler)
        MOV     ES: WORD PTR [SI + 2], SEG(IllegalEventHandler)

DoneStore:			     ;done storing the vector
        ADD     SI, 4           ;update pointer to next vector

        LOOP    ClrVectorLoop   ;loop until have cleared all vectors
        ;JMP    EndClrIRQVectors;and all done


EndClrIRQVectors:               ;all done, return
        POPA                    ;restore registers
        RET

ClrIRQVectors     ENDP



; IllegalEventHandler
;
;
; Description:    This procedure is the event handler for illegal (uninitialized)
;                 interrupts. It does nothing - it just returns after sending a 
;                 non-specific EOI
;
; Operation:      Send a non-specific EOI and return
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


IllegalEventHandler     PROC        NEAR
                        PUBLIC      IllegalEventHandler

        NOP                             ;do nothing (can set breakpoint here)

        PUSH    AX                      ;save the registers
        PUSH    DX

        MOV     DX, INTCtrlrEOI         ;send a non-sepecific EOI to the
        MOV     AX, NonSpecEOI          ;   interrupt controller to clear out
        OUT     DX, AL                  ;   the interrupt that got us here

        POP     DX                      ;restore the registers
        POP     AX

        IRET                            ;and return

IllegalEventHandler     ENDP



CODE    ENDS


        END