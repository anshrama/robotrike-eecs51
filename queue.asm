        NAME    QUEUE

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                    QUEUE                                   ;
;                                Queue Routines                              ;
;                                   EE/CS 51                                 ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; This file contains five methods for initializing, manipulating, and reading
; a queue structure stored in memory
;       QueueInit - Initializes a queue into memory
;       QueueEmpty - Tells whether an initialized queue is empty or not
;       QueueFull - Tells whether an initialized queue is full or not
;       Dequeue - Returns the value in the queue corresponding to the head of
;                 the queue and changes head location to pop value
;       Enqueue - Adds passed in value to the tail end of the queue
;
; Revision History:
;     10/17/15  Anshul Ramachandran      initial rev, func. specs./pseudocodes
;     10/22/15  Anshul Ramachandran      initial assembly code


$INCLUDE(queue.inc)


CGROUP  GROUP   CODE


CODE       SEGMENT PUBLIC 'CODE'


        ASSUME  CS:CGROUP


; QueueInit
;
;
; Description:    Initialize the queue of the passed length (l) and element size (s)
;                 at the passed address (a). This does all of the initialization to 
;                 prepare the queue struct. After calling this procedure, the queue
;                 is empt and ready to accept values. The passed length is the max
;                 number of items that can be stored in the queue, the element size
;                 specifies whether each entry is a byte or word. If s is true, the
;                 elements are words and if false, then they are bytes.
;
; Operation:      This function writes the queue information data (size of each 
;                 element, max number of elements, head index, and current number of
;                 elements in the queue i.e. a count) in the first seven bytes (one . 
;                 byte followed by three words). The size of each element is
;                 passed in, the number of elements is passed in, the head index is
;                 initialized to zero because the first element that we want to
;                 enqueue/dequeue from has an index of zero. The queue starts empty 
;                 so the count variable is initialized to zero. The queue element 
;                 data actually starts at byte number 8.
;
; Arguments:        AX - the max number of elements (length of queue)
;                   SI - address, a, where the queue should be initialized at
;                   BL - size of element (one byte = 0, two bytes = 1)
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
; Data Structures:  Cyclic array to store queue elements, queue structure
;
; Known Bugs:       None.
; Limitations:      None.
;
; Revision History: 10/17/15   Anshul Ramachandran     func. spec. and pseudocode
;                   10/22/15   Anshul Ramachandran     initial assembly code


QueueInit       PROC        NEAR
                PUBLIC      QueueInit

InitializeQueueInit:
    CMP BL, 0                         ; Check argument with element size
    JE InitQueueByteSize              ; If argument is zero, set up queue with its
                                      ; elements having a size of one byte
    ;JMP InitQueueWordSize            ; Otherwise, set up queue with its elements
                                      ; having a size of two bytes (one word)

InitQueueWordSize:
    MOV [SI].element_size, WORD_SIZE  ; Set the element_size in the struct to be a
    JMP InitQueueMaxElements          ; word and jump to setting up max_elems

InitQueueByteSize:
    MOV [SI].element_size, BYTE_SIZE  ; Set the element_size in the struct to be a
    ;JMP InitQueueMaxElements         ; byte and continue to setting up max_elems

InitQueueMaxElements:
    MOV [SI].max_elems, AX            ; Set the max_elems in the struct to be the arg
    ;JMP InitQueueHeadIndex           ; passed in through AX and continue to setting
                                      ; up the head index

InitQueueHeadIndex:
    MOV [SI].head_index, 0            ; Set the current head_index in the struct to be 
    ;JMP InitQueueElemCount           ; zero because the queue is currently empty, and
                                      ; we want to place the first element in the 
                                      ; first element spot in the array part of the 
                                      ; queue struct and continue to setting up the
                                      ; curr count of number of elements in the queue

InitQueueElemCount:
    MOV [SI].count, 0                 ; Set the current count in the struct to be 
                                      ; zero because the queue is currently empty

FinishQueueInit:
    RET

QueueInit       ENDP



; QueueEmpty
;
;
; Description:    This is called with the address of the queue to be checked (a) and
;                 returns with the zero flag set if the queue is empty and with the
;                 zero flag reset otherwise. The address a is passed in SI by value
;                 (i.e. queue starts at DS:SI)
;
; Operation:      This function gets the value stored in the count word in the queue’s
;                 information data, and if it is zero, then it sets the zero flag, and
;                 if not, it resets the zero flag.
;
; Arguments:        SI - address where the queue’s data starts (including info data)
;
; Return Values:    ZF - set if queue is empty, reset if queue is not
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
; Registers Used:   ZF, SI
;
; Algorithms:       None.
; Data Structures:  Cyclic array to store queue elements, queue structure
;
; Known Bugs:       None.
; Limitations:      None.
;
; Revision History: 10/17/15   Anshul Ramachandran     func. spec. and pseudocode
;                   10/22/15   Anshul Ramachandran     initial assembly code


QueueEmpty      PROC        NEAR
                PUBLIC      QueueEmpty

CheckQueueEmpty:
    CMP [SI].count, 0                 ; Check if the count of elements currently in
                                      ; the queue is zero. If it is zero, then ZF is
                                      ; set and we return and if it is not zero, then
                                      ; ZF is reset and we return, as desired. Note
                                      ; we do no operation, just set the flag and ret

FinishQueueEmpty:
    RET                               ; Return with ZF set/reset accordingly

QueueEmpty      ENDP



; QueueFull
;
;
; Description:    This is called with the address of the queue to be checked (a) and
;                 returns with the zero flag set if the queue is full and with the
;                 zero flag reset otherwise. The address a is passed in SI by value
;                 (i.e. queue starts at DS:SI)
;
; Operation:      This function gets the value stored in the count word in the queue’s
;                 information data, and if it is equal to the value stored in the, 
;                 queue’s max elements variable in the information data, then it 
;                 sets the zero flag, and if not, it resets the zero flag.
;
; Arguments:        SI - address where the queue’s data starts (including info data)
;
; Return Values:    ZF - set if queue is full, reset if queue is not
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
; Registers Used:   ZF, BX, SI
;
; Algorithms:       None.
; Data Structures:  Cyclic array to store queue elements, queue structure
;
; Known Bugs:       None.
; Limitations:      None.
;
; Revision History: 10/17/15   Anshul Ramachandran     func. spec. and pseudocode
;                   10/22/15   Anshul Ramachandran     initial assembly code


QueueFull       PROC        NEAR
                PUBLIC      QueueFull

CheckQueueFull:
    PUSH BX                           ; Save the current BX value on the stack since
    MOV BX, [SI].max_elems            ; we need to get the max_elems value into BX
                                      ; so that we can do the CMP properly

    CMP [SI].count, BX                ; Check if the count of elements currently in
                                      ; the queue is equal to max_elems. If equal,
                                      ; ZF is set and we return and if not equal, then
                                      ; ZF is reset and we return, as desired. Note
                                      ; we do no operation, just set the flag and ret

    POP BX                            ; Put the old BX value back into BX

FinishQueueFull:
    RET                               ; Return with ZF set/reset accordingly

QueueFull       ENDP



; Dequeue
;
;
; Description:    This function either removes a one-byte or two-byte value (depending
;                 on element size) from the head of the queue at the passed address
;                 and returns it in either AL or AX respectively: AL if element size 
;                 is one byte and AX if element size is two bytes. If the queue is 
;                 empty, it waits until the queue has a value to be removed and
;                 returned. It does not return until a value is taken from the queue.
;                 The address a is passed in SI by value (i.e. queue starts at DS:SI)
;
; Operation:      This function first continuously loops until the queue is not empty
;                 by calling QueueEmpty and jumping out of the loop once ZF is reset.
;                 Once the queue is not-empty, it stores the value at the head of the
;                 queue to AL or AX depending on whether elem_size is one byte or two
;                 bytes respectively. Then, it decrements the count in the queue’s 
;                 information data and sets the head of the queue to be at the next
;                 element
;
; Arguments:      SI - address where the queue’s data starts (including info data)
;
; Return Values:    AL if element_size is one byte, AX if element_size is two bytes
;                       will hold the value of the element that was at the head of
;                       the queue before dequeuing
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
; Registers Used:   AX, BX, DX, SI
;
; Algorithms:       None.
; Data Structures:  Cyclic array to store queue elements, queue structure
;
; Known Bugs:       None.
; Limitations:      None.
;
; Revision History: 10/12/15   Anshul Ramachandran     func. spec. and pseudocode
;                   10/22/15   Anshul Ramachandran     initial assembly code


Dequeue         PROC        NEAR
                PUBLIC      Dequeue

QueueEmptyBlocking:
    CALL QueueEmpty                   ; Keep looping on QueueEmpty until we get the
    JZ QueueEmptyBlocking             ; zero flag to be reset, which means that the
    ;JMP BeginDequeue                 ; queue is no longer empty, so we can proceed
                                      ; with the dequeue function

BeginDequeue:
    PUSH SI                           ; Save pointer to queue and also AX (which we
    PUSH AX                           ; use to calculate the memory offset of the
                                      ; head element)

    MOV AX, 0                         ; Clear AX so we start with 0 offset
    MOV AL, [SI].element_size         ; Store the element size

    PUSH DX                           ; Save DX (in case the multiplication overflows)
    MUL [SI].head_index               ; and multiply by the head index to get the 
                                      ; byte offset from the start of the queue array
    POP DX                            ; Bring back the old DX

    ADD AX, QUEUE_ARRAY_OFFSET        ; The array starts QUEUE_ARRAY_OFFSET bytes 
                                      ; after the start of the queue struct, so to get
                                      ; the offset from the start of the queue struct,
                                      ; we must add this in

    CMP [SI].element_size, WORD_SIZE  ; If we want to dequeue a word, jump to the
    JE DequeueWord                    ; DequeueWord label, otherwise continue on to
    ;JMP DequeueByte                  ; dequeue a byte

DequeueByte:
    ADD SI, AX                        ; Add the computed offset to the beginning of
                                      ; the queue struct’s address to get the address
                                      ; of the element we want to dequeue
    POP AX                            ; Restore the old value of AX since we are done
                                      ; using it to calculate the offset
    MOV AL, BYTE PTR[SI]              ; We take the byte value at the memory location
                                      ; corresponding to head index and store it into
                                      ; AL which is what we will return
    JMP SetupIncrementHeadIndex       ; Continue to incrementing the head index

DequeueWord:
    ADD SI, AX                        ; Add the computed offset to the beginning of
                                      ; the queue struct’s address to get the address
                                      ; of the element we want to dequeue
    POP AX                            ; Restore the old value of AX since we are done
                                      ; using it to calculate the offset
    MOV AX, WORD PTR[SI]              ; We take the word value at the memory location
                                      ; corresponding to head index and store it into
                                      ; AX which is what we will return
    ;JMP SetupIncrementHeadIndex      ; Continue to incrementing the head index

SetupIncrementHeadIndex:
    POP SI                            ; Restore the pointer to the queue struct into
                                      ; SI (top of queue struct, not the offset
                                      ; memory location)
    PUSH AX                           ; Save return value (will use AX for
                                      ; incrementing head pointer)

IncrementHeadIndex:
    MOV AX, [SI].head_index           ; Put current head index in AX
    INC AX                            ; Go to the next index (ignoring cyclic looping
                                      ; for now)
    CMP [SI].max_elems, AX            ; If the new head index is now equal to
    JE LoopHeadIndex                  ; max_elems, then we need to loop back around
                                      ; to the start of the array since head_index
                                      ; should be between 0 and max_elems - 1
    JMP FinishIncrementHeadIndex      ; If not equal, then we have incremented to the
                                      ; correct new head_index, so just skip past
                                      ; looping code

LoopHeadIndex:
    MOV AX, 0                         ; In the case we need to loop, we know that the
                                      ; new head_index must be 0, and then continue
    ;JMP FinishIncrementHeadIndex     ; with finishing the head_index changing code

FinishIncrementHeadIndex:
    MOV [SI].head_index, AX           ; Store this new head index into the queue 
    POP AX                            ; struct and restore the old value of AX (the
                                      ; return value)

DecrementCount:
    DEC [SI].count                    ; We have one less element after dequeueing so
                                      ; decrement the element count

FinishDequeue:
    RET                               ; and return

Dequeue         ENDP



; Enqueue
;
;
; Description:    This function adds the passed one-byte or two-byte value (depending
;                 on the element size) to the tail of the queue at the passed address
;                 a. If the queue is full it waits until the queue has an open space
;                 in which to add the value, and does not return until the value is 
;                 added to the queue. The address a is passed in SI by value (thus the
;                 queue starts at DS:SI) and the value to enqueue is passed by value
;                 in AL if the element size is one byte and in Ax if the element size
;                 is two bytes.
;
; Operation:      This function first continuously loops until the queue is not full.
;                 Once the queue is not full, it increments the number of elements
;                 currently in the queue, and then calculate the byte corresponding
;                 to the tail by using the head byte, the number of elements, and the
;                 element size. If element_size is one byte, it stores the value 
;                 passed in AL, if element_size is two bytes, it stores the value
;                 passed in the entirety of AX.
;
; Arguments:      SI - address where the queue’s data starts (including info data)
;                 AL (if size of value to enqueue is one byte) - value to enqueue
;                 AX (if size of value to enqueue is two bytes) - value to enqueue
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
; Registers Used:   AX, BX, DX, SI
;
; Algorithms:       None.
; Data Structures:  Cyclic array to store queue elements queue strutter
;
; Known Bugs:       None.
; Limitations:      None.
;
; Revision History: 10/12/15   Anshul Ramachandran     func. spec. and pseudocode
;                   10/22/15   Anshul Ramachandran     initial assembly code


Enqueue         PROC        NEAR
                PUBLIC      Enqueue

QueueFullBlocking:
    CALL QueueFull                    ; Keep looping on QueueFull until we get the
    JZ QueueFullBlocking              ; zero flag to be reset, which means that the
    ;JMP SetupEnqueue                 ; queue is no longer full, so we can proceed
                                      ; with the enqueue function

SetupEnqueue:
    PUSH SI                           ; Save pointer to queue and also AX (which we
    PUSH AX                           ; use to calculate the memory offset of the
                                      ; tail element)
    PUSH DX                           ; Save current value of DX (will clear for
                                      ; division for looping)
    PUSH BX                           ; Save current value of BX (will use BX to store
                                      ; max_elems for division for looping)

BeginEnqueue:
    MOV AX, [SI].head_index           ; Put the current head index in AX
    ADD AX, [SI].count                ; and add the number of elements currently in
                                      ; the array to get the index of the first
                                      ; empty spot in the queue (without looping)

    MOV BX, [SI].max_elems            ; Put the max_elems (length of queue array) into
                                      ; BX for division

    MOV DX, 0                         ; Clear DX for division
    DIV BX                            ; DX will now have (head_index + count) mod 
                                      ; max_elems, which is the index we want
    MOV AX, DX                        ; so move this tail index to AX

    POP BX                            ; No longer need BX so restore original value

ScaleTailIndexByElemSize:
    MUL [SI].element_size             ; Multiply tail index by element_size to get the 
                                      ; byte offset from the start of the queue array
                                      ; for the memory location to place the value in
                                      ; Did not have to worry about overflow into DX
                                      ; since we did not previously restore DX
    POP DX                            ; Now no longer need DX so restore old value

    ADD AX, QUEUE_ARRAY_OFFSET        ; The array starts QUEUE_ARRAY_OFFSET bytes 
                                      ; after the start of the queue struct, so to get
                                      ; the offset from the start of the queue struct
                                      ; of the array element, we need this

    CMP [SI].element_size, WORD_SIZE  ; If we want to enqueue a word, jump to the
    JE EnqueueWord                    ; EnqueueWord label, otherwise continue on to
    ;JMP EnqueueByte                  ; enqueue a byte

EnqueueByte:
    ADD SI, AX                        ; Add the computed offset to the beginning of
                                      ; the queue struct’s address to get the address
                                      ; of the element we want to enqueue
    POP AX                            ; Restore the old value of AX (which was the
                                      ; value we want to enqueue)
    MOV BYTE PTR[SI], AL              ; We take the byte value at the memory location
                                      ; corresponding to head index and store AL there
    JMP IncrementCount                ; Continue to incrementing count

EnqueueWord:
    ADD SI, AX                        ; Add the computed offset to the beginning of
                                      ; the queue struct’s address to get the address
                                      ; of the element we want to enqueue
    POP AX                            ; Restore the old value of AX (which was the
                                      ; value we want to enqueue)
    MOV WORD PTR[SI], AX              ; We take the word value at the memory location
                                      ; corresponding to head index and store AX there
    ;JMP IncrementCount               ; Continue to incrementing count

IncrementCount:
    POP SI                            ; Restore the pointer to the queue struct into
                                      ; SI (top of queue struct, not the offset
                                      ; memory location)
    INC [SI].count                    ; We have one more element after enqueueing so
                                      ; increment the element count

FinishEnqueue:
    RET                               ; and return

Enqueue         ENDP



CODE    ENDS


        END