        NAME    SERIAL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                   SERIAL                                   ;
;                              Serial Routines                               ;
;                                  EE/CS 51                                  ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This file contains the functions for serial-related functions of the RoboTrike for
; communication between the remote board and target board. It contains functions for
; initialization of the system (shared variables, parity, baud rate) and functions to
; place chars in the serial channel.
;
;    InitSerial: Initializes shared variables used by serial channel, and initializes
;                the serial interrupts to occur
;    SerialPutChar: Puts a character into the serial channel
;    SerialPutString: Puts a string into the serial channel
;    SetBaudRate: Sets baud rate in serial channel
;    SetParity: Sets parity of serial channel
;    SerialHandler: Event handler for serial channel (called during serial interrupts)
;
; Revision History:
;       11/14/2015     Anshul Ramachandran     initial rev, func specs./pseudocodes
;       11/20/2015     Anshul Ramachandran     assembly code

$INCLUDE(serial.inc)
$INCLUDE(queue.inc)
$INCLUDE(events.inc)
$INCLUDE(serint.inc)
$INCLUDE(general.inc)


CGROUP  GROUP   CODE

CODE    SEGMENT PUBLIC 'CODE'
        ASSUME  CS:CGROUP, DS:DATA

; External references
    EXTRN   EnqueueEvent:NEAR

    EXTRN   QueueInit:NEAR
    EXTRN   QueueEmpty:NEAR
    EXTRN   QueueFull:NEAR
    EXTRN   Dequeue:NEAR
    EXTRN   Enqueue:NEAR


; BaudRateDivisorTable
;
; Table for baud divisor values that will be used in SetBaudRate. Assumes 9.216-Mhz
; crystal.
; Table is in code segment because these values are read only
BaudRateDivisorTable       LABEL   WORD
;       DW             Baud divisor value
        DW             7680             ; => baud rate = 75
        DW             3840             ; => baud rate = 150
        DW             1920             ; => baud rate = 300
        DW             960              ; => baud rate = 600
        DW             480              ; => baud rate = 1200
        DW             240              ; => baud rate = 2400
        DW             120              ; => baud rate = 4800
        DW             60               ; => baud rate = 9600
        DW             30               ; => baud rate = 19200
        DW             15               ; => baud rate = 38400

; ParitiesTable
;
; Table of bit configurations for all parity settings
; Table is in code segment because these values are read only
ParitiesTable       LABEL   BYTE
;       DB             Parity bit pattern
        DB             00000000B        ; No parity
        DB             00001000B        ; Odd parity
        DB             00011000B        ; Even parity
        DB             00101000B        ; Transmitted and set
        DB             00111000B        ; Transmitted and cleared

; SerialHandlerJumpTable
;
; Jump table of labels in the SerialHandler function for the sub handlers of each
; of the types of interrupts
; Table is in code segment because these values are read only
SerialHandlerJumpTable       LABEL   WORD
;       DW             Label to jump to in SerialHandler
        DW             ModemStatusHandler        ; Modem status interrupt
        DW             TransmitReadyHandler      ; Register empty interrupt
        DW             DataReadyHandler          ; Received data interrupt
        DW             LineStatusHandler         ; Receiver line status interrupt


; InitSerial
;
; Description:      This function initializes the shared variables used in the serial
;                   chip functions (TransmitQueue and kickstart), as well as 
;                   initializes the baud rate and parity.
;
; Operation:        Initialize shared values with their default values as followed:
;                   - TransmitQueue: initialized by calling QueueInit and using 
;                                    TQUEUE_ELEM_TYPE (byte) as element type in BL
;                   - kickstart: initialized to KICKSTART_OFF (bit cleared since no
;                                need to kickstart in the beginning)
;                   Initialize baud rate/parity as follows:
;                   - baud rate: call SetBaudRate with INIT_BAUD_IDX
;                   - parity: call SetParity with INIT_PARITY
;                   Then enable serial interrupts and setup line control register by
;                   writing its default value (INIT_LCR) to it
;
; Arguments:        None.
;
; Return Value:     None.
;
; Local Variables:  None.
; Shared Variables: TransmitQueue (write), kickstart (write)
;
; Global Variables: None.
;
; Input:            None.
; Output:           LCR and serial chip (interrupts enabled)
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
;       11/14/2015      Anshul Ramachandran     func spec and pseudocode
;       11/19/2015      Anshul Ramachandran     assembly code

InitSerial            PROC    NEAR
                      PUBLIC  InitSerial

SaveRegistersInitSerial:
    PUSHA                             ; Save registers

InitTransmitQueue:
    MOV SI, OFFSET(TransmitQueue)     ; Get address of TransmitQueue in SI and the
    MOV BL, TQUEUE_ELEM_TYPE          ; element type (bytes) in BL so that we can
    CALL QueueInit                    ; initialize TransmitQueue

InitKickstart:
    MOV kickstart, KICKSTART_OFF      ; Initialize kickstart

InitBaudRate:
    MOV BX, INIT_BAUD_IDX             ; Put the INIT_BAUD_IDX in BX so we
    CALL SetBaudRate                  ; can call SetBaudRate to initialize baud rate

InitParity:
    MOV BX, INIT_PARITY               ; Put the INIT_PARITY (no parity) in BX so we
    CALL SetParity                    ; can call SetParity to initialize parity

InitInterruptEnableRegister:
    MOV AL, EN_THRE_INT               ; Enable transmit holding register empty
    MOV DX, SER_IE_REG                ; interrupts
    OUT DX, AL

InitLineControlRegister:
    MOV AL, INIT_LCR                  ; Initialize LCR to have 8 data bits and 1
    MOV DX, SER_LC_REG                ; stop bit
    OUT DX, AL

RestoreRegistersInitSerial:
    POPA                              ; Restore registers

    RET

InitSerial            ENDP


; SerialPutChar
;
; Description:      This function puts a character c that is passed in as an argument
;                   into the serial channel (whose buffer is TransmitQueue) and
;                   returns the result of the enqueue in the carry flag. If the
;                   serial channel is already full, the carry flag is set and if the
;                   character was successfully enqueued, the carry flag is reset.
;
; Operation:        First checks if the serial channel’s queue (TransitQueue) is full
;                   using QueueFull and if full, then the carry flag is set and the
;                   function returns. Otherwise, character c is enqueued using the
;                   Enqueue function, we clear the carry flag, and then check if we
;                   need to kickstart (if the kickstart shared var is set to 
;                   KICKSTART_ON). If we need to kickstart, we disable and re-enable
;                   the transmit holding register empty interrupt and then set the
;                   kickstart shared variable to KICKSTART_OFF.
;
; Arguments:        AL = character c to be enqueued
;
; Return Value:     CF = set if TransmitQueue is full and cannot enqueue, reset
;                        otherwise and character was enqueued
;
; Local Variables:  None.
; Shared Variables: TransmitQueue (read/write), kickstart (read/write)
; Global Variables: None.
;
; Input:            Interrupt enable register
; Output:           Interrupt enable register
;
; Error Handling:   None.
;
; Algorithms:       None.
; Data Structures:  None.
;
; Registers Changed:   CF
;
; Known Bugs:       None.
; Limitations:      None.
;
; Revision History:
;       11/14/2015      Anshul Ramachandran     func spec and pseudocode
;       11/19/2015      Anshul Ramachandran     assembly pseudocode

SerialPutChar         PROC    NEAR
                      PUBLIC  SerialPutChar

SaveRegistersSerialPutChar:
    PUSHA                             ; Save registers

CheckTransmitQueueFull:
    MOV SI, OFFSET(TransmitQueue)     ; SI = memory location of TransmitQueue for
    CALL QueueFull                    ; checking if the queue is full
    JZ TransmitQueueFull
    ;JNZ TransmitQueueNotFull

TransmitQueueNotFull:
    CALL Enqueue                      ; If the queue is not full, enqueue char
    CMP kickstart, KICKSTART_ON       ; Check if we need to kickstart
    CLC                               ; Clear carry to make sure when we return, it
                                      ; wouldn’t see that TransmitQueue is full
    JNE SerialPutCharFinished         ; If we don’t need to kickstart, we are done
    ;JE KickstartNeeded               ; Otherwise kickstart

KickstartNeeded:
    MOV kickstart, KICKSTART_OFF      ; Update kickstart shared variable
    MOV AL, DIS_THRE_INT              ; Disable transmit holding register empty
    MOV DX, SER_IE_REG                ; interrupts
    OUT DX, AL
    MOV AL, EN_THRE_INT               ; and re-enable them
    OUT DX, AL                        
    JMP SerialPutCharFinished         ; and finish

TransmitQueueFull:
    STC                               ; set the carry flag to show that queue is full
    ;JMP SerialPutCharFinished

SerialPutCharFinished:
RestoreRegistersSerialPutChar:
    POPA                              ; Restore registers

    RET

SerialPutChar         ENDP


; SerialPutString
;
; Description:      This function takes a string in ES:SI and enqueues all of the
;                   characters to the TransmitQueue with repeated calls to 
;                   SerialPutChar. The string is padded with SER_EOS (carriage return)
;                   characters to make sure the later parsing goes without error. If
;                   the TransmitQueue gets full at any point, the carry flag is set
;                   and the function returns.
;
; Operation:        This function first calls SerialPutChar with a SER_EOS character
;                   and if the carry flag gets set then return because the transmit
;                   queue is full. Otherwise loop through the passed in string in 
;                   ES:SI until we get an ASCII_NULL character (end of string), and
;                   for each character, call SerialPutChar and check the CF for queue
;                   full error as before. Once we get the ASCII_NULL character, we
;                   call SerialPutChar with SER_EOS once again and return.
;
; Arguments:        ES:SI = address of string of characters to put into TransmitQueue
;
; Return Value:     CF = set if TransmitQueue is full and cannot enqueue at any point
;
; Local Variables:  None.
; Shared Variables: TransmitQueue (read/write), kickstart (read/write) 
;                    (see SerialPutChar)
; Global Variables: None.
;
; Input:            Interrupt enable register
; Output:           Interrupt enable register
;
; Error Handling:   None.
;
; Algorithms:       None.
; Data Structures:  Queue
;
; Registers Changed:   CF
;
; Known Bugs:       None.
; Limitations:      None.
;
; Revision History:
;       11/30/2015      Anshul Ramachandran     func spec and pseudocode
;
; Pseudocode:
;
; CALL SerialPutChar(SER_EOS)
; IF CF = 0
;     BX = 0
;     WHILE(ES:[SI+BX] != ASCII_NULL)
;         CALL SerialPutChar(ES:[SI+BX])
;         IF CF = 1
;             Return
;         BX++
;     CALL SerialPutChar(SER_EOS)

SerialPutString         PROC    NEAR
                        PUBLIC  SerialPutString

SerialPutStringStart:
    PUSHA                                 ; Save registers

SerialPutStringPreEOS:
    MOV AL, SER_EOS                       ; Put carriage return in the serial so
    CALL SerialPutChar                    ; that we are starting a new command with
    JC SerialPutStringEnd                 ; the string passed in - if carry flag is
                                          ; set, TransmitQueue is full and so don’t
                                          ; attempt to put the string in serial,
                                          ; exit with CF set

SerialPutStringInit:
    MOV BX, 0                             ; We are starting at index 0 of the passed
                                          ; in string

SerialPutStringLoopStart:
    MOV AL, ES:[BX+SI]                    ; Get character at index BX of passed in
    CMP AL, ASCII_NULL                    ; string and see if it is the termination 
                                          ; symbol of the string (ASCII_NULL)
    JE SerialPutStringPostEOS             ; If it is, we have finished parsing all
                                          ; characters in the string so go to putting
                                          ; another carriage return and returning
    ;JNE SerialPutStringChar              ; If it isn’t the termination symbol, it
                                          ; is another character to call SerialPutChar
                                          ; with

SerialPutStringChar:
    CALL SerialPutChar                    ; Char already in AL so call SerialPutChar
    JC SerialPutStringEnd                 ; and check if carry flag is set (i.e. the
                                          ; TransmitQueue was full), if so we want to
                                          ; just exit SerialPutString so that the UI
                                          ; does not hang with CF set

SerialPutStringLoopEnd:
    INC BX                                ; If the parsing of the character was fine,
                                          ; increment the index so that we get the
                                          ; next char in the next iteration of loop
    JMP SerialPutStringLoopStart          ; Go back to top of loop

SerialPutStringPostEOS:
    MOV AL, SER_EOS                       ; Send carriage return to serial to end the
    CALL SerialPutChar                    ; command string

SerialPutStringEnd:
    POPA                                  ; Restore registers and return
    RET

SerialPutString         ENDP



; SetBaudRate
;
; Description:      This function sets the baud rate for the serial channel using
;                   the passed in baud rate divisor.
;
; Operation:        Sets the baud rate by first turning off interrupts (critical 
;                   code) and then setting the DLAB bit/saving the prev value in the
;                   line control register, both needed before we can change the
;                   baud rate. Then the BaudRateDivisorTable is used to get the baud
;                   rate divisor using the passed in baud rate divisor index (arg
;                   in BX must be between 0 and 10). We then output the baud rate
;                   divisor into the Baud rate generator divisor (SER_BAUD_OUTPUT)
;                   and restore the line control reg value which resets the DLAB bit
;
; Arguments:        None.
;
; Return Value:     BX = Baud rate index for BaudRateDivisorTable (must be between 0
;                        and 10) for setting that particular baud rate
;
; Local Variables:  None.
; Shared Variables: None.
; Global Variables: None.
;
; Input:            New baud value
; Output:           Serial chip baud divisor registers
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
;       11/14/2015      Anshul Ramachandran     func spec and pseudocode
;       11/19/2015      Anshul Ramachandran     assembly pseudocode

SetBaudRate           PROC    NEAR
                      PUBLIC  SetBaudRate

SaveRegsitersSetBaudRate:
    PUSHA                             ; Save registers
    PUSHF                             ; Save flags

TurnOffInterruptsSetBaudRate:
    CLI                               ; Setting baud rate is critical code

SetupForSetBaudRate:
    SHL BX, 1                         ; Baud divisor table has words, so multiply
                                      ; index by 2 for table lookup

SaveLineControlRegisterValue:
    MOV DX, SER_LC_REG                ; Get the current line control register value
    IN AL, DX
    PUSH AX                           ; and save it

SetDLABBit:
    OR AL, DLAB_SET_MASK              ; Set the DLAB bit in the line control register
    OUT DX, AX                        ; value so that we can change the baud rate div

SetBaudRateDivisor:
    MOV AX, CS:BaudRateDivisorTable[BX]  ; Get the divisor corresponding to index
    MOV DX, SER_BAUD_OUTPUT           ; and output it to the baud rate generator
    OUT DX, AX                        ; divisor

ResetLineControlRegisterValue:
    POP AX                            ; Get old value of LCR (with DLAB bit unset)
    MOV DX, SER_LC_REG                ; and restore LCR to the old value
    OUT DX, AL

RestoreRegistersSetBaudRate:
    POPF                              ; Restore flags
    POPA                              ; Restore registers

    RET

SetBaudRate           ENDP


; SetParity
;
; Description:      This function sets the parity for the serial channel using
;                   the passed in index for the ParitiesTable.
;
; Operation:        Gets the corresponding parity bit pattern for the index passed in
;                   for ParitiesTable and output it into the line control register
;
; Arguments:        BX = Parity index for ParitiesTable (must be between 0 and 4)
;                        for setting that particular parity
;
; Return Value:     None.
;
; Local Variables:  None.
; Shared Variables: None.
; Global Variables: None.
;
; Input:            None.
; Output:           Serial channel line control register
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
;       11/14/2015      Anshul Ramachandran     func spec and pseudocode
;       11/19/2015      Anshul Ramachandran     assembly pseudocode

SetParity             PROC    NEAR
                      PUBLIC  SetParity

SaveRegistersSetParity:
    PUSHA                             ; Save registers

GetLineControlRegisterValue:
    MOV DX, SER_LC_REG                ; Get current value in line control register
    IN AL, DX

ClearExistingParity:
    AND AL, CLEAR_PARITY_MASK         ; Clear the error bits

SetAndChangeParity:
    OR AL, CS:ParitiesTable[BX]       ; so that we can mask in the new parity value
    OUT DX, AL                        ; that corresponds to the index passed in in the
                                      ; ParitiesTable

RestoreRegistersSetParity:
    POPA                              ; Restore registers

    RET

SetParity             ENDP


; SerialHandler
;
; Description:      This function is called when a serial interrupt occurs. The 
;                   function checks which type of serial interrupt occurred and does
;                   the corresponding sub-handler for that type of serial event. The
;                   types of serial interrupts are modem status, register empty,
;                   received data, and line status (error) interrupts.
;
; Operation:        Get the serial interrupt ID from the SER_II_REG and while there 
;                   is a serial interrupt pending (the interrupt ID is not equal to 
;                   NO_INT), we want to handle the interrupt so the function takes
;                   the interrupt ID and compares that ID to MODEM_STATUS_INT,
;                   TRNASMIT_READY_INT, DATA_READY_INT, and LINE_STATUS_INT to find
;                   which sub handler to go to. When there is no more serial 
;                   interrupts pending, the function sends an EOI and finishes.  
;                   These are the sub handlers:
;                   - Modem status: Read from SER_MS_REG (modem status register) to 
;                                   clear the interrupt
;                   - Register empty: Kickstart if TransmitQueue is empty (QueueEmpty)
;                                     and if so, set the kickstart shared var to 
;                                     KICKSTART_ON. If not empty, dequeue from
;                                     TransmitQueue (Dequeue) and output the character
;                                     received to SER_TRANS_REG (transmit holding reg)
;                   - Received data: Using the character taken from SER_RECEIVER_REG
;                                    (receiver buffer register), call EnqueueEvent
;                                    with the event code being a serial receive event
;                                    (SER_RECEIVE_CODE)
;                   - Line status: Read the error from SER_LS_REG (line status reg)
;                                  and use ERROR_MASK to get the error bits. If
;                                  this value is 0, then we don’t actually have an
;                                  error, but otherwise we do have a serial error
;                                  event and use EnqueueEvent with this value and
;                                  the event code being a serial error event
;                                  (SER_ERROR_CODE)
;
; Arguments:        None.
;
; Return Value:     None.
;
; Local Variables:  int_type = type of serial interrupt
; Shared Variables: TransmitQueue (read/write), kickstart (write)
; Global Variables: None.
;
; Input:            SER_II_REG (interrupt identification register)
;                   SER_RECEIVER_REG (receiver buffer register) for received data int
;
; Output:           SER_TRANS_REG (transmit holding register) for register empty int
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
;       11/14/2015      Anshul Ramachandran     func spec and pseudocode
;       11/19/2015      Anshul Ramachandran     assembly pseudocode

SerialHandler         PROC    NEAR
                      PUBLIC  SerialHandler

SaveRegistersSerialHandler:
    PUSHA                             ; Save registers
    PUSHF                             ; Save flags

GetInterruptType:
    MOV DX, SER_II_REG                ; Get the type of next interrupt (see serial 
    IN AL, DX                         ; handler jump table for interrupt types) in AL

CheckForNoError:
    CMP AL, NO_INT                    ; Check if there actually is an interrupt 
    JE SendSerialEOI                  ; pending, if not, go to sending EOI and 
    ;JNE JumpToInterruptSubhandler    ; finishing, otherwise continue

JumpToInterruptSubhandler:
    MOV BL, AL                        ; Put the interrupt type in bottom byte of BX
    MOV BH, 0                         ; Clear the top byte of BX to get the correct
    JMP CS:SerialHandlerJumpTable[BX] ; address to jump to (note that interrupts come
                                      ; in intervals of 2: 0, 10, 100, 110, which is
                                      ; exactly needed for the word-sized jump table)

ModemStatusHandler:                   ; Modem status interrupt
    MOV DX, SER_MS_REG                ; Get the value in the modem status register
    IN AL, DX                         ; and do nothing with it
    JMP GetInterruptType              ; Get next pending interrupt

TransmitReadyHandler:                 ; Register empty interrupt
    MOV SI, OFFSET(TransmitQueue)     ; SI = address of TransmitQueue to call 
    CALL QueueEmpty                   ; QueueEmpty
    JNZ TransmitReadyNoKickstart      ; If not empty, we don’t need to kickstart,
                                      ; but we can dequeue an element into transmit
                                      ; holding register
    ;JZ TransmitReadyKickstart        ; If empty, we need to indicate kickstart needed

TransmitReadyKickstart:               ; If kickstart needed
    MOV kickstart, KICKSTART_ON       ; set kickstart shared variable flag ons
    JMP GetInterruptType              ; Get next pending interrupt

TransmitReadyNoKickstart:
    CALL Dequeue                      ; If kickstart not needed/dequeue element
    MOV DX, SER_TRANS_REG             ; Put the character that we dequeued (which is
    OUT DX, AL                        ; now in AL after the call) into the transmit
    JMP GetInterruptType              ; holding reg and get next pending interrupt

DataReadyHandler:                     ; Received data interrupt
    CLI                               ; Turn off interrupts for critical code section
                                      ; because we don’t want updates to the serial
                                      ; receiver buffer register in the middle of
                                      ; getting a value and enqueueing an event for it
    MOV DX, SER_RECEIVER_REG          ; Get the value currently in the receiver 
    IN AL, DX                         ; buffer register in AL
    MOV AH, SER_RECEIVE_CODE          ; Put SER_RECEIVE_CODE in top byte of AX to
    CALL EnqueueEvent                 ; mark type of event and enqueue
    STI                               ; Turn interrupts back on
    JMP GetInterruptType              ; Get next pending interrupt

LineStatusHandler:                    ; Receiver line status interrupt
    MOV DX, SER_LS_REG                ; Get value in the line status register into AL
    IN AL, DX
    AND AL, ERROR_MASK                ; and mask for the bits corresponding to error
    MOV AH, SER_ERROR_CODE            ; Put SER_ERROR_CODE in top byte of AX to mark
    CALL EnqueueEvent                 ; type of event and enqueue
    JMP GetInterruptType              ; Get next bending interrupt

SendSerialEOI:
    MOV AX, INT2EOI                   ; After handled all pending interrupts, send
    MOV DX, INTCtrlrEOI               ; an INT2EOI (a serial interrupt EOI) to the
    OUT DX, AX                        ; EOI control register

RestoreRegistersSerialHandler:
    POPF                              ; Restore flags
    POPA                              ; Restore registers

    IRET

SerialHandler         ENDP

CODE ENDS


DATA SEGMENT PUBLIC 'DATA'

    ; queue structure for transmission
    TransmitQueue       queueSTRUC     <>

    ; flag that is set if kickstart needed
    kickstart           DB     ?

DATA ENDS

    END