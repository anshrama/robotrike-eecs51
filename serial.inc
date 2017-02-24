;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                  SERIAL.INC                                ;
;                       Include File For Serial Constants                    ;
;                                   EE/CS 51                                 ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This file contains constants used in serial.asm for purposes of functions
; regarding the serial unit of the RoboTrike
;
; Revision History:
;     11/14/15   Anshul Ramachandran      initial revision
;     11/20/15   Anshul Ramachandran      split up file

; Initialization constants
INIT_PARITY            EQU    0          ; Default to no parity
INIT_BAUD_IDX          EQU    7          ; Default to 9600 baud rate
TQUEUE_ELEM_TYPE       EQU    0          ; TransmitQueue elements are bytes
INIT_LCR               EQU    00000011B  ; 0-------  Access registers
                                         ; -0------  No break
                                         ; --000---  Parity = 0
                                         ; -----0--  One stop bit
                                         ; ------11  8 data bits

; SerialPutString constants
SER_EOS                EQU    13         ; End of Statement (EOS) character
                                         ; = carriage return in serial parser

; Serial registers
ACE_BASE               EQU    100H         ; Base address of ACE
SER_BAUD_OUTPUT        EQU    ACE_BASE + 0 ; Baud rate generator divisor
SER_RECEIVER_REG       EQU    ACE_BASE + 0 ; Receiver buffer register
SER_TRANS_REG          EQU    ACE_BASE + 0 ; Transmit holding register
SER_IE_REG             EQU    ACE_BASE + 1 ; Interrupt enable register
SER_II_REG             EQU    ACE_BASE + 2 ; Interrupt identification register
SER_LC_REG             EQU    ACE_BASE + 3 ; Line control register
SER_LS_REG             EQU    ACE_BASE + 5 ; Line status register
SER_MS_REG             EQU    ACE_BASE + 6 ; Modem status register

; Baud rate constants
DLAB_SET_MASK          EQU    10000000B  ; Or mask to set DLAB bit in
                                         ; SER_LC_REG value

; Parity constants
CLEAR_PARITY_MASK      EQU    11000111B  ; Used to clear previous parity bits

; Serial handler constants
NO_INT                 EQU    00000001B  ; Value in SER_II_REG when there 
                                         ; are no more interrupts
ERROR_MASK             EQU    00011110B  ; Mask used on value from SER_LS_REG
                                         ; to get just the error bits

; Kickstart constants
KICKSTART_OFF          EQU    0          ; Disable kickstart
KICKSTART_ON           EQU    1          ; Enable kickstart
DIS_SERIAL_INT         EQU    00000000B  ; Disable serial interrupts
DIS_THRE_INT           EQU    00000101B  ; Disable transmit holding register
                                         ; empty interrupt
EN_THRE_INT            EQU    00000111B  ; Enable transmit holding register
                                         ; empty interrupt

