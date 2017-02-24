;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                 TIMER2.INC                                 ;
;                      Include File For Timer 2 Functions                    ;
;                                   EE/CS 51                                 ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This file contains the definitions for the timing functions for the 80188
; version of the Microprocessor-Based Clock.  Also included are some
; definitions for the interrupt controller since the timing functions use
; them.
;
; Revision History:
;     7/12/10  Glen George
;     10/29/15  Anshul Ramachandran      initial revision


; Timer Defintions

; Addresses
Tmr2Ctrl        EQU     0FF66H          ;address of Timer 2 Control Register
Tmr2MaxCnt      EQU     0FF62H          ;address of Timer 2 Max Count Register
Tmr2Count       EQU     0FF60H          ;address of Timer 2 Count Register

; Control Register Values
Tmr2CtrlVal     EQU     1110000000000001B ;Timer 2 Control Register value
                       ;1---------------  enable timer
                       ;-1--------------  write to control
                       ;--1-------------  enable interrupts
                       ;----000000-0000-  reserved
                       ;---0------0-----  read only
                       ;---------------1  continuous mode

; Interrupt Vectors
Tmr2Vec         EQU     19              ;interrupt vector for Timer 2


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

ONE_MS_CNT      EQU     2304            ;number of clocks per 1 ms 
                                        ;(assumes 18.432 MHz clock)