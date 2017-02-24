        NAME    CONVERTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                   CONVERTS                                 ;
;                             Conversion Functions                           ;
;                                   EE/CS 51                                 ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; This file contains two methods for storing binary values in decimal or hex
; representation as an ASCII string:
;       Dec2String - converts 16-bit signed value to string of its decimal 
;                      representation stored at given address
;       Hex2String - converts 16-bit unsigned value to string of its hex 
;                      representation stored at given address
;
; Revision History:
;     10/12/15  Anshul Ramachandran      initial rev, func. specs./pseudocodes
;     10/13/15  Anshul Ramachandran      initial assembly code
;     10/14/15  Anshul Ramachandran      hex code revisions
;     10/15/15  Anshul Ramachandran      code revisions

$INCLUDE(converts.inc)

CGROUP  GROUP   CODE


CODE       SEGMENT PUBLIC 'CODE'


        ASSUME  CS:CGROUP


; Dec2String
;
;
; Description:    This function converts the 16-bit signed value passed in to
;                 a decimal representation and stores it as a string.  The 
;                 value is at most 5 digits plus a minus sign, and the return string
;                 ends with the ASCII_NULL symbol at the end of the string. There
;                 are no leading zeroes and if the number is positive, a plus sign
;                 is not displayed. The resulting string is stored starting at
;                 the memory location that is passed in as an address.
;
; Operation:      The function first starts by checking if n = 0, which is a
;                 special case. If not zero, sign of n is checked and a ‘-‘ 
;                 sign is inserted at address a (which is incremented to the)
;                 next position in memory. If negative, the value of n is 
;                 negated. To this resulting value, we start with the largest
;                 power of 10 possible (PWR10_DEC) and loops by dividing the 
;                 number by the power of 10: the quotient is the next digit
;                 and the remainder is used in the next loop iteration. Each
;                 loop iteration divides the power of 10 by 10 until it is 0.
;                 At this point the number’s decimal representation has been
;                 completely stored as an ASCII string in memory. Note that 
;                 if there are zeroes in the beginning of the string (high
;                 power digits are 0 because number is small), do not add
;                 these zeroes to the string representation (i.e. have a 
;                 separate loop until a nonzero digit has been read, after which
;                 any digit, including zero, should be printed).
;
; Arguments:        AX - 16-bit signed value, n, to convert to decimal and store
;                            as a string.
;                   SI - address, a, where the string should be stored in (at DS:SI)
;
; Return Values:    None.
;
; Local Variables:  n (AX)           value to convert to ASCII and store
;                   a (SI)           address of memory location to store string in
;                   pwr10 (BX)       current power of 10 being computed
;                   digit (AX)       computed decimal digit
; Shared Variables: None.
; Global Variables: None.
;
; Input:            None.
; Output:           None.
;
; Error Handling:   None.
;
; Registers Used:   flags, AX, BX, CX, DX, SI
;
; Algorithms:       Repeatedly divide by powers of 10 and get the quotients
;                   (which are the decimal digits).
; Data Structures:  None.
;
; Known Bugs:       None.
; Limitations:      None.
;
; Revision History: 10/12/15   Anshul Ramachandran     func. spec. and pseudocode
;                   10/13/15   Anshul Ramachandran     initial assembly code
;                   10/15/15   Anshul Ramachandran     code revisions
;                   10/16/15   Anshul Ramachandran     description revisions


Dec2String      PROC        NEAR
                PUBLIC      Dec2String

SaveDecRegisters:
       PUSHA

CheckDecForZero:
       CMP AX, 0                        ; If the value is 0, just jump to writing a
       JZ DecNumberIsZero               ; zero and exiting the function

CheckDecForNegative:
       CMP AX, 0                        ; If the value is not negative, skip to the
       JNL SetupForWhileLoop            ; normal find digits loop
       MOV BYTE PTR[SI], '-'            ; Otherwise put in a minus sign
       INC SI                           ; Move SI to next spot in memory
       NEG AX                           ; and negate the value to get its absolute
                                        ; value, which we use to get the digits
       
SetupForWhileLoop:
       MOV BX, PWR10_DEC                ; Largest power of 10 that a digit could be
                                        ; is 10000 (values are from -32768 to 32767) 

FindFirstNonzeroDigit:
       MOV DX, 0                        ; Clear DX for division, divide by 
       DIV BX                           ; pwr10 => digit in AX, remainder in DX
       CMP AX, 0                        ; See if AX is nonzero
       JNZ WriteDecDigit                ; If so, exit this loop, and go to loop
                                        ; where every digit is written (we have found
                                        ; first nonzero digit)

       MOV CX, DX                       ; Remainder moved to CX
       MOV AX, BX                       ; pwr10 moved to AX and 10 moved to BX to
       MOV BX, 10                       ; setup dividing to get next pwr10
       MOV DX, 0                        ; Clear DX for division
       DIV BX                           ; New pwr10 in AX, remainder (0) in DX

       MOV BX, AX                       ; New pwr10 in BX like where it started
       MOV AX, CX                       ; Remainder = ‘new’ value of n moved to AX
       
       JMP FindFirstNonzeroDigit        ; Continue finding the first nonzero digit

FindNextDigit:
       MOV DX, 0                        ; Clear DX for division
       DIV BX                           ; Digit is in AX, remainder in DX

WriteDecDigit:
       ADD AX, '0'                      ; Get ASCII symbol by adding '0' to digit
       MOV BYTE PTR[SI], AL             ; Move ASCII symbol into memory and then
       INC SI                           ; increment the index to where we need to 
                                        ; write the next symbol

IncrementPwr10:
       MOV CX, DX                       ; Remainder moved to CX
       MOV AX, BX                       ; pwr10 moved to AX and 10 moved to BX to
       MOV BX, 10                       ; setup dividing to get next pwr10
       MOV DX, 0                        ; Clear DX for division
       DIV BX                           ; New pwr10 in AX, remainder (0) in DX

       CMP AX, 0                        ; See if next power of 10 is 0
       JZ TerminateDec2String           ; If so, we have finished loop and have 
                                        ; written all of the digits, so end program

       MOV BX, AX                       ; New pwr10 in BX like where it started
       MOV AX, CX                       ; Remainder = ‘new’ value of n, in AX
       
       JMP FindNextDigit                ; Go back to top of loop

DecNumberIsZero:
       MOV BYTE PTR[SI], '0'            ; Move ASCII symbol for ‘0’ into memory and
       INC SI                           ; increment the index to where we need to 
                                        ; write the next symbol

TerminateDec2String:
       MOV BYTE PTR[SI], ASCII_NULL     ; Put the ASCII null symbol to end the string

RetrieveRegisters:
       POPA

       RET

Dec2String       ENDP



; Hex2String
;
;
; Description:    This function converts the 16-bit unsigned value passed in 
;                 to a hex representation and stores it as a string.  The hex
;                 value is at most 4 digits in ASCII, and the return string
;                 contains the <null> terminated decimal representation of the
;                 value in ASCII. The resulting string is stored starting at 
;                 the memory location that is passed in as an address.
;
; Operation:      The function first starts by checking if n = 0, which is a
;                 special case. If not zero, a bit mask F000h is created, 
;                 which can be used with bitwise and to extract each hex 
;                 digit. Essentially group the 16 bits into four groups of
;                 four, and use the bit mask (MASK_HEX) to get the hex value. 
;                 Then, we find whether it is a digit or letter symbol and 
;                 put this symbol into address a and increment a. Do this for
;                 all four groups, updating the bit shift and bit mask each time. 
;                 The bit shift initially starts as 12 (INIT_BITSHIFT) because we
;                 want the first digit. If there are zeroes in the beginning (high
;                 power digits are 0 because number is small), do not add
;                  these zeroes to the string representation (i.e. have a 
;                  separate loop until a nonzero digit has been read, after which
;                  any digit, including zero, should be printed).
;
; Arguments:       AX - 16-bit unsigned value, n, to convert to hex and store
;                            as a string.
;                  SI - address, a, where the string should be stored in (at DS:SI)
;
; Return Values:   None.
;
; Local Variables:  n (AX)         value to convert to ASCII and store
;                   a (SI)         address of memory location to store string in
;                   bitmask (BX)   mask used to extract a single digit
;                   bitshift (CL)  amount of bits to shift right by after &
;                   digit (DX)     computed hexadecimal digit
; Shared Variables: None.
; Global Variables: None.
;
; Input:            None.
; Output:           None.
;
; Error Handling:   None.
;
; Registers Used:   flags, AX, BX, CL, DX, SI
;
; Algorithms:       Group bits in groups of four and then mask and shift 
;                     each group to extract the hex digits 
; Data Structures:  None.
;
; Known Bugs:       None.
; Limitations:      None.
;
; Revision History: 10/12/15   Anshul Ramachandran     func. spec. and pseudocode
;                   10/13/15   Anshul Ramachandran     initial assembly code
;                   10/14/15   Anshul Ramachandran     code revisions
;                   10/15/15   Anshul Ramachandran     code revisions
;                   10/16/15   Anshul Ramachandran     description revisions

Hex2String      PROC        NEAR
                PUBLIC      Hex2String

SaveHexRegisters:
       PUSHA

CheckHexForZero:
       CMP AX, 0                        ; If the value is 0, just jump to writing a
       JZ HexNumberIsZero               ; zero and exiting the function

SetupBitmask:
       MOV BX, MASK_HEX                 ; The initial mask exposes the first digit if
                                        ; used with AND
       MOV CL, INIT_BITSHIFT            ; an AND will give the first digit with 12
                                        ; trailing zeroes, so we note that we would
                                        ; need to shift the exposed digit by 12 bits
                                        ; right to actually just get the first digit

FindNonzeroHexDigit:
       TEST AX, BX                      ; Test the digit that is exposed by the mask,
       JNZ GetNextHexDigit              ; if zero flag is not set, we have found a
                                        ; nonzero digit so break from this loop
              
       SHR BX, 4                        ; Otherwise, we have to shift the mask to
       SUB CL, 4                        ; next digit and subtract 4 from the number
                                        ; of trailing zeroes from the new digit
                                        ; location (due to an AND with the mask)

       JMP FindNonzeroHexDigit          ; Go back to top of loop

GetNextHexDigit:
       MOV DX, AX                       ; Copy number into temporary storage
       AND DX, BX                       ; Take bitwise AND between mask and number, 
       SHR DX, CL                       ; shift right number of bits to get digit.
       SHR BX, 4                        ; Update the mask to next digit
       SUB CL, 4                        ; Update number of bits to shift right for
                                        ; next hex digit (4 less bits)

GetHexSymbolOffset:       
       CMP DX, 9                        ; If symbol > 9, then must be a letter
       JG AddHexLetterOffset

AddHexDigitOffset:
       ADD DX, '0'                      ; Otherwise it is a digit, so get ASCII symbol
       JMP WriteHexSymbol               ; to store by adding '0' to digit

AddHexLetterOffset:
       SUB DX, 10                       ; If the symbol is a letter, subtract 10 from
       ADD DX, 'A'                      ; CX to get offset from the letter ‘A’ in 
                                        ; ASCII and use this calculated offset
       
WriteHexSymbol:
       MOV BYTE PTR[SI], DL             ; Write the ASCII symbol to memory
       INC SI                           ; Increment the index to where we need to 
                                        ; write the next symbol

CheckIfMoreDigits:
       CMP BX, 0                        ; If mask is now equal to 0, we are done with
       JZ TerminateHex2String           ; the entire value, 

       JMP GetNextHexDigit              ; otherwise go back to finding next hex digit

HexNumberIsZero:
       MOV BYTE PTR[SI], '0'            ; Case when passed in value is 0, just write
       INC SI                           ; and increment index in memory

TerminateHex2String:
       MOV BYTE PTR[SI], ASCII_NULL     ; Put the ASCII null symbol to end the string

RetrieveHexRegisters:
       POPA

       RET

Hex2String       ENDP


CODE    ENDS


        END
