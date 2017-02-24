;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                 TIMER0.INC                                 ;
;                      Include File For Timer 0 Functions                    ;
;                                   EE/CS 51                                 ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This file contains the definitions for the timing functions for the 80188
; timer #0, which control the pulse widths/PWM motors. Also included are some
; definitions for the interrupt controller since the timing functions use
; them.
;
; Revision History:
;     7/12/10  Glen George
;     11/7/15  Anshul Ramachandran      initial revision


; Timer Definitions

; Addresses
Tmr0Ctrl        EQU     0FF56H          ;address of Timer 0 Control Register
Tmr0MaxCnt      EQU     0FF52H          ;address of Timer 0 Max Count Register
Tmr0Count       EQU     0FF50H          ;address of Timer 0 Count Register

; Control Register Values
Tmr0CtrlVal     EQU     1110000000000001B ;Timer 0 Control Register value
                       ;1---------------  enable timer
                       ;-1--------------  write to control
                       ;--1-------------  enable interrupts
                       ;----000000-0000-  reserved
                       ;---0------0-----  read only
                       ;---------------1  continuous mode

; Interrupt Vectors
Tmr0Vec         EQU     8               ;interrupt vector for Timer 0


; Interrupt Controller

; Addresses
INTCtrlrCtrl    EQU     0FF32H          ;address of interrupt controller for timer
INTCtrlrEOI     EQU     0FF22H          ;address of interrupt controller EOI register

; Register Values
INTCtrlrCVal    EQU     00001H          ;set priority for timers to 1 and enable
                                        ;000000000000----  reserved
                                        ;------------0---  enable timer interrupt
                                        ;-------------001  timer priority
TimerEOI        EQU     00008H          ;Timer EOI command (all timers same)


; General Timing Definitions

CNT_32_MS      EQU     576              ;interrupt rate (combined with pulse width
                                        ;makes the pulse width occur every 32ms)