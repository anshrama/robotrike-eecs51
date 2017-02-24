;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                   Include                                  ;
;                       Include File For Queue Functions                     ;
;                                   EE/CS 51                                 ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This file contains the constants used in queue.asm and in the test functions
;
; Revision History:
;     10/22/15  Anshul Ramachandran      initial revision

; DEFINITIONS

; Defines number of bytes in queue struct before the queue array starts
QUEUE_ARRAY_OFFSET       EQU     7

; Defines number of bytes in each element_size (byte and word)
BYTE_SIZE                EQU     1
WORD_SIZE                EQU     2

; Defines maximum number of bytes that can be allocated to the queue data in the 
; queue struct, queue must be initialized with FEWER bytes than this value
MAX_QUEUE_BYTES          EQU     1024

; Defines length of queue for testing purposes
TEST_QUEUE_LENGTH        EQU     256

; Queue structure with the following elements:
;     element_size: 1 if elements are bytes, 2 if elements are words
;     max_elems   : Maximum number of elements that can be stored in the queue
;     head_index  : Index of element that marks the start of the queue elements list
;                   can be a value between 0 and max_elems - 1
;     count       : Current number of elements stored in the queue
;     queue       : Start of the queue array (the actual elements)
queueSTRUC STRUC
    element_size    DB    ?
    max_elems       DW    ?
    head_index      DW    ?
    count           DW    ?
    queue           DB MAX_QUEUE_BYTES DUP (?)
queueSTRUC ENDS