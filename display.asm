        NAME    DISPLAY

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                   DISPLAY                                  ;
;                               Display Routines                             ;
;                                   EE/CS 51                                 ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This file contains methods to handle events regarding and updating the display
; and methods to display an ASCII string, decimal number, or decimal number on a
; 14-segment display.
;    InitDisplay: Initialize shared variables used in display functions
;    DisplayMux: Goes through the display buffer and updates the physical display with
;                the values in the buffer
;    Display: Takes an ASCII string, converts to segment patterns, and writes patterns
;             to the display buffer
;    DisplayNum: Takes a number, converts its decimal representation to ASCII string 
;                form, and calls Display to send the segment patterns to the display
;                buffer
;    DisplayHex: Takes a number, converts its hex representation to ASCII string 
;                form, and calls Display to send the segment patterns to the display
;                buffer
;
; Revision History:
;       10/25/2015      Anshul Ramachandran     initial rev, func specs./pseudocodes
;       10/29/15        Anshul Ramachandran     assembly code


$INCLUDE(general.inc)
$INCLUDE(display.inc)


CGROUP  GROUP   CODE

CODE    SEGMENT PUBLIC 'CODE'
        ASSUME  CS:CGROUP, DS:DATA

; External references
    EXTRN   ASCIISegTable:NEAR
    EXTRN   Dec2String:NEAR
    EXTRN   Hex2String:NEAR


; InitDisplay
;
; Description:      This function must be run before any display functions are run
;                   since it initializes the shared variables (d_index and d_buffer)
;
; Operation:        Sets the d_buffer to blank segment patterns, and sets d_index to
;                   0, which is the first digit display
;
; Arguments:        None.
;
; Return Value:     None.
;
; Local Variables:  BX = beginning of the array that is being cleared
;                   CX = end of the array that is being cleared (check condition)
;
; Shared Variables: d_index = the index of the digit that we are updating
;                   d_buffer = the buffer that contains the segment patterns for the
;                              characters being shown in the displays
;                   conv_string = byte array that stores the ASCII characters from
;                                 the Dec2String or Hex2String calls
;
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
; Registers Used:   None.
;
; Known Bugs:       None.
; Limitations:      None.
;
; Revision History:
;       10/25/2015      Anshul Ramachandran     func spec and pseudocode
;       10/29/2015      Anshul Ramachandran     assembly code

InitDisplay    PROC    NEAR
               PUBLIC  InitDisplay

SaveRegistersForInitDisplay:
    PUSH BX
    PUSH CX

SetupForInitDisplayBuffer:
    MOV BX, OFFSET(d_buffer)              ; BX = start of d_buffer array
    MOV CX, OFFSET(d_buffer) + NUM_DIGITS * WORD_BYTES    ; CX = d_buffer array end

InitDisplayBuffer:
    MOV WORD PTR[BX], BLANK_DIGIT         ; Clear the word at the current digit
                                          ; position given by BX’s address
    INC BX                                ; Go to the next digit’s memory address
    CMP BX, CX                            ; If we haven’t gotten to the end of the
    JNE InitDisplayBuffer                 ; d_buffer array, loop back and clear the
                                          ; word at the address that BX now points to
    ;JE InitDisplayIndex                  ; Otherwise, we have cleared all of the
                                          ; d_buffer array so continue to initializing
                                          ; d_index

InitDisplayIndex:
    MOV d_index, 0                        ; Just set d_index to 0 (first digit)

SetupForInitConvertedStringArray:
    MOV BX, OFFSET(conv_string)           ; BX = start of conv_string array
    MOV CX, OFFSET(conv_string) + NUM_DIGITS     ; CX = conv_string array end

InitConvertedStringArray:
    MOV BYTE PTR[BX], ASCII_NULL          ; Clear the byte at the current character
                                          ; position given by BX’s address
    INC BX                                ; Go to the next char’s memory address
    CMP BX, CX                            ; If we haven’t gotten to the end of the
    JNE InitConvertedStringArray          ; conv_string array, loop back and clear the
                                          ; byte at the address that BX now points to
    ;JE InitDisplayIndex                  ; Otherwise, we have cleared all of the
                                          ; conv_string array so continue to restoring
                                          ; the registers used and returnings

RestoreRegistersForInitDisplay:
    POP CX
    POP BX

    RET

InitDisplay ENDP


; DisplayMux
;
; Description:      This function is called on a timer interrupt and displays a single
;                   digit on the LED display with the next value in the display buffer
; Operation:        Since there are multiple digits, the function will read the 
;                   segment pattern for the digit indexed by the shared variable
;                   d_index and put that pattern on that digit. It then increments
;                   d_index (0 to 1, etc and NUM_DIGITS - 1 to 0) so that d_index
;                   keeps on cycling throughout the physical digit displays.
;
; Arguments:        None.
;
; Return Value:     None.
;
; Local Variables:  None.
; Shared Variables: d_index = the index of the digit that we are updating
;                   d_buffer = the buffer that contains the segment patterns for the
;                              characters being shown on the digits
;
; Global Variables: None.
;
; Input:            None.
; Output:           Segment pattern on a display
;
; Error Handling:   None.
;
; Algorithms:       None.
; Data Structures:  None.
;
; Registers Used:   None.
;
; Known Bugs:       None.
; Limitations:      None.
;
; Revision History:
;       10/25/2015      Anshul Ramachandran     func spec and pseudocode
;       10/29/2015      Anshul Ramachandran     assembly code


DisplayMux    PROC    NEAR
              PUBLIC  DisplayMux

SaveRegistersForDisplayMux:
    PUSH AX                             ; Save registers
    PUSH BX
    PUSH DX
    PUSH SI

RetrieveSegmentPattern:
    MOV BX, OFFSET(d_buffer)              ; BX = start of d_buffer array
    MOV SI, d_index                       ; SI = d_index
    SHL SI, 1                             ; SI = offset of digit’s address from start
                                          ;      of d_buffer’s array since there are
                                          ;      WORD_BYTE bytes (2 bytes) for each
                                          ;      digit in d_buffer

    MOV AX, [BX][SI]                      ; AX now has the segment pattern stored
                                          ; for the current digit

SetupForUpdatingDigit:
    SHR SI, 1                             ; Revert SI back to being d_index

UpdateDigit:                              ; We must first update high byte of segment
                                          ; pattern and then low byte of the pattern
                                          ; because that is the order that the bytes
                                          ; appear in the segment pattern table
                                          ; The first byte is essentially a modifier
                                          ; that is appended to the physical display
                                          ; that we output the lower byte to

    MOV DX, LEDDisplayHigh                ; Get the I/O address of the port of the 
                                          ; high byte of the first digit disp., which
                                          ; is port 8
    XCHG AH, AL                           ; Get the high byte of segment pattern in AL
    OUT DX, AL                            ; Output the ‘modifier’ of seg pattern

    MOV DX, LEDDisplayLow                 ; Get the I/O address of the port of the
                                          ; low byte of the first digit disp., which
                                          ; is port 0 (0-7 is port of low byte of 
                                          ; the segment pattern)
    ADD DX, SI                            ; Offset the low byte port num by d_index
    XCHG AH, AL                           ; Get the low byte of segment pattern in AL    
    OUT DX, AL                            ; Display low byte of seg pattern

GetNextDisplayIndex:
    INC SI                                ; Increment d_index
    CMP SI, NUM_DIGITS                    ; If d_index does not equal the number of
    JNE UpdateDisplayIndex                ; digits, then don't wrap around.
    ;JE  WrapDisplayIndex                 ; Otherwise, wrap around to digit index 0

WrapDisplayIndex:
    MOV SI, 0                             ; Set the next d_index value to 0

UpdateDisplayIndex:
    MOV d_index, SI                       ; Actually update d_index with new value

RestoreRegistersForDisplayMux:
    POP SI                                ; Restore registers
    POP DX
    POP BX
    POP AX
  
    RET

DisplayMux ENDP


; Display
;
; Description:      This function is used to take a null terminated string and write
;                   the segment patterns for each character in the string to the 
;                   display buffer (d_buffer), which is later read by DisplayMux to
;                   actually put those segment patterns on the physical display. This
;                   function does not actually output to the display, it just writes
;                   into the display buffer. If the string is too long to be
;                   displayed, the string is cut off to the first NUM_DIGITS (8)
;                   characters, and the string is left aligned in the display. If
;                   the string does not fill up the entire buffer, then it clears
;                   all of the trailing digits - this happens when we reach the
;                   ASCII_NULL termination of the string before reaching the end of
;                   the display buffer.
;
; Operation:        This function iterates through the characters through the 
;                   characters of the null-terminated ASCII string passed in through
;                   ES:SI and writes their corresponding segment patterns to the
;                   correct displays in the d_buffer. The i’th symbol in d_buffer 
;                   corresponds to the i’th character in the passed in ASCII string,
;                   up to digit NUM_DIGIT, after which the characters in the string
;                   are not written to the buffer at all (string cut off at NUM_DIGITS
;                   characters).
;
; Arguments:        SI - offset from ES that corresponds to the ASCII string
;
; Return Value:     None.
;
; Local Variables:  DI = address of start of d_buffer
;                   CX = address of start of segment pattern table
;                   DX = address of end of d_buffer (used to check for end of loop)
;                   BL = character from conv_string
;                   BX = total byte offset from start of segment pattern table to
;                        desired segment pattern
;
; Shared Variables: d_buffer = the buffer that contains the segment patterns for the
;                              characters being shown in the displays
;
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
; Registers Used:   None.
;
; Known Bugs:       None.
; Limitations:      Only can update display buffer for the first 8 characters of 
;                   the passed in string
;
; Revision History:
;       10/25/2015      Anshul Ramachandran     func spec and pseudocode
;       10/29/2015      Anshul Ramachandran     assembly code


Display     PROC    NEAR
            PUBLIC  Display

SaveRegistersForDisplay:
    PUSH AX                             ; Save registers
    PUSH BX
    PUSH CX
    PUSH DX
    PUSH SI
    PUSH DI

SetupBufferUpdateLoop:
    MOV DI, OFFSET(d_buffer)            ; Set DI to be the offset of d_buffer
    MOV DX, OFFSET(d_buffer) + NUM_DIGITS * WORD_BYTES    ; DX = d_buffer array end
    MOV CX, OFFSET(ASCIISegTable)       ; Set CX to be the offset into CS of the 
                                        ; segment pattern table

BufferUpdateLoop:
    MOV BL, ES:BYTE PTR [SI]            ; Set BL to the byte at ES:SI character
                                        ; in the conv_string array (first character if
                                        ; this is first time in loop since ES:SI
                                        ; is passed in as start of conv_string array)

CheckIfEndOfString:
    CMP BL, ASCII_NULL                  ; If character is ASCII_NULL, then we have
    JE GetCharSegmentPatternOffset      ; reached end of string, so don’t increment
                                        ; to the ‘next’ character because no such
                                        ; char exists and go to getting seg pattern
    ;JNE IncrementCharIndex             ; Otherwise go to IncrementCharIndex -

IncrementCharIndex:
    INC SI                              ; - which increments SI to be the offset of
                                        ; the next character in the string because
                                        ; we already have the curr char in BL

GetCharSegmentPatternOffset:
    MOV BH, 0                           ; Clear BH (so only BL is filled with current
                                        ; char and we can do operations on BX)

    SHL BX, 1                           ; BX now has the offset of the desired segment
                                        ; pattern in the pattern table because the
                                        ; table is organized in ASCII order and each
                                        ; 14-segment pattern entry takes 2 bytes,
                                        ; and this instr gets twice the ASCII value

GetCharSegmentPattern:
    ADD BX, CX                          ; Add the table offset to the segment pattern
                                        ; offset
    MOV AX, CS:[BX]                     ; AX is now the segment pattern
    MOV [DI], AX                        ; Set DS:DI (the address of the current 
                                        ; position in d_buffer) to the seg pattern

CheckBufferUpdateLop:
    ADD DI, WORD_BYTES                  ; The next position in d_buffer that we want
                                        ; to write to is WORD_BYTES bytes (2 bytes)
                                        ; after the current position since each
                                        ; entry in d_buffer is a word (2 bytes)
    CMP DI, DX                          ; If we haven’t reached the end of d_buffer,
    JNE BufferUpdateLoop                ; then continue to loop. If we have reached
    ;JE RestoreRegistersForDisplay      ; the end of d_buffer, then end Display as
                                        ; we have updated all of the segment patterns
                                        ; in d_buffer

RestoreRegistersForDisplay:
    POP DI                              ; Restore registers
    POP SI
    POP DX
    POP CX
    POP BX
    POP AX
    
    RET

Display ENDP



; DisplayNum
;
; Description:      This function is passed a 16-bit signed value (n) to output to
;                   decimal to the LED display. This does not actually output to the
;                   physical display but rather writes the segment patterns to the
;                   right digits in the display buffer (d_buffer), which is read by
;                   DisplayMux to output to the physical display. The value is
;                   at most 5 digits plus a minus sign, and the return string
;                   ends with the ASCII_NULL symbol at the end of the string. There
;                   are no leading zeroes and if the number is positive, a plus sign
;                   is not displayed (sign is only displayed if n is negative).
;
; Operation:        This function first calls Dec2String to get the null-terminated
;                   ASCII string that corresponds to the passed in number and then
;                   calls Display to take this ASCII string and put the segment
;                   patterns that correspond to the characters into d_buffer. 
;                   Dec2String returns the ASCII string into DS:SI and Display takes
;                   the string from ES:SI. The conv_string shared variable acts as
;                   a buffer intermediate to store the result of Dec2String in.
;                   Therefore we write into conv_string with Dec2String and read from
;                   conv_string when we call Display
;
; Arguments:        AX - number (n) to send to the display buffer (d_buffer)
;
; Return Value:     None.
;
; Local Variables:  SI = offset into DS of address of conv_string
;
; Shared Variables: conv_string = byte array that stores the ASCII characters from
;                                 the Dec2String call
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
; Registers Used:   None.
;
; Known Bugs:       None.
; Limitations:      None.
;
; Revision History:
;       10/25/2015      Anshul Ramachandran     func spec and pseudocode
;       10/29/2015      Anshul Ramachandran     assembly code


DisplayNum  PROC    NEAR
            PUBLIC  DisplayNum

SaveRegistersForDisplayNum:
    PUSH ES                             ; Save registers
    PUSH BX

SetupConvertAndWriteDecimal:
    MOV BX, DS                          ; We cannot directly set ES to DS but by 
    MOV ES, BX                          ; making ES = DS, we will write the ASCII
    MOV SI, OFFSET(conv_string)         ; string to the same place as Display will
                                        ; read the ASCII string from (Dec2String 
                                        ; writes to DS:SI and reads from ES:SI, but
                                        ; DS and ES are the ‘same’ and SI is the
                                        ; location of conv_string within DS

ConvertAndWriteDecimal:
    CALL Dec2String                     ; Argument for Dec2String is already in AX, 
                                        ; Dec2String writes to DS:SI
    CALL Display                        ; Argument string read from ‘ES:SI’ (really
                                        ; DS:SI)

RestoreRegistersForDisplayNum:
    POP BX                              ; Restore registers
    POP ES
    
    RET

DisplayNum ENDP


; DisplayNum
;
; Description:      This function is passed a 16-bit unsigned value (n) to output to
;                   hex to the LED display. This does not actually output to the
;                   physical display but rather writes the segment patterns to the
;                   right digits in the display buffer (d_buffer), which is read by
;                   DisplayMux to output to the physical display.
;
; Operation:        This function first calls Hex2String to get the null-terminated
;                   ASCII string that corresponds to the passed in number and then
;                   calls Display to take this ASCII string and put the segment
;                   patterns that correspond to the characters into d_buffer. 
;                   Dec2String returns the ASCII string into DS:SI and Display takes
;                   the string from ES:SI. The conv_string shared variable acts as
;                   a buffer intermediate to store the result of Dec2String in.
;                   Therefore we write into conv_string with Dec2String and read from
;                   conv_string when we call Display
;
; Arguments:        AX - number (n) to send to the display buffer (d_buffer)
;
; Return Value:     None.
;
; Local Variables:  SI = offset into DS of address of conv_string
;
; Shared Variables: conv_string = byte array that stores the ASCII characters from
;                                 the Hex2String call
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
; Registers Used:   None.
;
; Known Bugs:       None.
; Limitations:      None.
;
; Revision History:
;       10/25/2015      Anshul Ramachandran     func spec and pseudocode
;       10/29/2015      Anshul Ramachandran     assembly code

DisplayHex  PROC    NEAR
            PUBLIC  DisplayHex

SaveRegistersForDisplayHex:
    PUSH ES                             ; Save registers
    PUSH BX

SetupConvertAndWriteHex:
    MOV BX, DS                          ; We cannot directly set ES to DS but by 
    MOV ES, BX                          ; making ES = DS, we will write the ASCII
    MOV SI, OFFSET(conv_string)         ; string to the same place as Display will
                                        ; read the ASCII string from (Hex2String 
                                        ; writes to DS:SI and reads from ES:SI, but
                                        ; DS and ES are the ‘same’ and SI is the
                                        ; location of conv_string within DS

ConvertAndWriteHex:
    CALL Hex2String                     ; Argument for Hex2String is already in AX, 
                                        ; Hex2String writes to DS:SI
    CALL Display                        ; Argument string read from ‘ES:SI’ (really
                                        ; DS:SI)

RestoreRegistersForDisplayHex:
    POP BX                              ; Restore registers
    POP ES
    
    RET

DisplayHex ENDP



CODE ENDS



DATA SEGMENT PUBLIC 'DATA'

    ; Has the segment patterns for the NUM_DIGITS symbols that are being displayed
    ; at the moment physically
    d_buffer         DW   NUM_DIGITS   DUP (?)
    
    ; Has the index of the physical digit display to update next
    d_index          DW   ?
    
    ; A place in memory where the string resulting from Dec2String or Hex2String
    ; can be stored
    conv_string      DB   NUM_DIGITS   DUP (?)

DATA ENDS



    END
