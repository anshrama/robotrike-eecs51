        NAME    MOTORS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                   MOTORS                                   ;
;                               Motor Routines                               ;
;                                  EE/CS 51                                  ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This file contains the functions for controlling the motors on the RoboTrike motor
; unit.
;
;    InitMotor: Initializes shared variables used in motor functions
;    SetMotorSpeed: Sets the speed of the three motors on the RoboTrike motor unit
;    GetMotorSpeed: Get absolute speed of the RoboTrike
;    GetMotorDirection: Get current movement direction of the RoboTrike
;    SetLaser: Turns RoboTrike either on or off dependent on argument passed in
;    GetLaser: Get status of the laser on the RoboTrike (on or off)
;    SetTurretAngle: Dummy function for setting turret angle
;    SetRelTurretAngle: Dummy function for setting relative turret angle
;    GetTurretAngle: Dummy function for getting turret angle
;    GetRelTurretAngle: Dummy function for getting relative turret angle
;    SetTurretElevation: Dummy function for setting turret elevation
;    GetTurretElevation: Dummy function for getting turret elevation
;    PWMHandler: Actually controls/changes the PWM motor
;    SignedAngleMod: Converts a signed angle to its equivalent angle between -360
;                    and 360 degrees (retaining sign)
;
; Revision History:
;       11/7/2015      Anshul Ramachandran     initial rev, func specs./pseudocodes
;       11/13/2015     Anshul Ramachandran     assembly code

$INCLUDE(motors.inc)
$INCLUDE(pchip.inc)

CGROUP  GROUP   CODE
DGROUP  GROUP   DATA

CODE    SEGMENT PUBLIC 'CODE'
        ASSUME  CS:CGROUP, DS:DGROUP

; External references
    EXTRN   Sin_Table:WORD
    EXTRN   Cos_Table:WORD


; ForceX
;
; Table of values for x-component of force,
; Table is in code segment because these values are read only
ForceX       LABEL   WORD
;       DW             value of x-component of force for specific motor
        DW             07FFFH        ; Motor 1: Fx_1 (1)
        DW             0C000H        ; Motor 2: Fx_2 (-1/2)
        DW             0C000H        ; Motor 3: Fx_3 (-1/2)

; ForceY
;
; Table of values for y-component of force,
; Table is in code segment because these values are read only
ForceY       LABEL   WORD
;       DW             value of y-component of force for specific motor
        DW             00000H        ; Motor 1: Fy_1 (0)
        DW             09127H        ; Motor 2: Fy_2 (-sqrt3/2)
        DW             06ED9H        ; Motor 3: Fy_3 (sqrt3/2)

; MotorOnMask
;
; Table for masks to be used with portb_buffer to turn PWM motors on
; Table is in code segment because these values are read only
MotorOnMask       LABEL   BYTE
;       DB             OR mask to turn motor on
        DB             00000010B        ; Motor 1
        DB             00001000B        ; Motor 2
        DB             00100000B        ; Motor 3

; MotorReverseMask
;
; Table for masks to be used with portb_buffer to put PWM motors in reverse
; Table is in code segment because these values are read only
MotorReverseMask  LABEL   BYTE
;       DB             OR mask to put motor in reverse
        DB             00000001B        ; Motor 1
        DB             00000100B        ; Motor 2
        DB             00010000B        ; Motor 3


; InitMotor
;
; Description:      This function initializes the shared variables used in the motor
;                   functions.
;
; Operation:        Initialize shared values with their default values as followed:
;                   - laser_state = OFF (0)
;                   - motor_speeds = [ZERO_SPEED, ZERO_SPEED, ZERO_SPEED] (all motors
;                         start with zero speed)
;                   - speed = ZERO_SPEED (overall speed of RoboTrike is zero)
;                   - direction = ZERO_DEGREES (initially pointed straight - zero
;                         degrees off orientation of RoboTrike)
;                   - pw_counter = COUNTER_CLEAR (start at 0 for pulse width counter)
;
; Arguments:        None.
;
; Return Value:     None.
;
; Local Variables:  None.
; Shared Variables: laser_state (write), motor_speeds (write), speed (write), 
;                   direction (write), pw_counter (write)
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
; Registers Changed:   None.
;
; Known Bugs:       None.
; Limitations:      None.
;
; Revision History:
;       11/7/2015      Anshul Ramachandran     func spec and pseudocode
;       11/13/2015     Anshul Ramachandran     assembly code

InitMotor             PROC    NEAR
                      PUBLIC  InitMotor

                                          ; have values in words)
InitMotorSpeeds:
    MOV motor_speeds[0], ZERO_SPEED       ; Initialize all of the motor speeds
    MOV motor_speeds[1], ZERO_SPEED       ; (pulse widths) to ZERO_SPEED (0) since
    MOV motor_speeds[2], ZERO_SPEED       ; speed has not been passed in yet

InitSpeedDirection:
    MOV speed, ZERO_SPEED                 ; Initialize overall speed to ZERO_SPEED
    MOV direction, ZERO_DEGREES           ; and overall direction to ZERO_DEGREES
                                          ; since robot hasn’t been told to move yet

InitPulseWidthCounter:
    MOV pw_counter, COUNTER_CLEAR         ; Set pulse width counter starting at 
                                          ; COUNTER_CLEAR (start of the pulse width
                                          ; cycle)

InitLaserState:
    MOV laser_state, OFF                  ; Set laser state to default OFF (OFF until
                                          ; told to turn on)

    RET

InitMotor             ENDP


; SetMotorSpeed
;
; Description:      This function sets the speeds of the three motors (values in 
;                   motor_speeds) with the passed in speed and angle (relative to
;                   orientation of RoboTrike). The speed passed in is an absolute
;                   speed in the range from 0-65534, and the angle passed in is
;                   a signed angle. Passing in 65535 for speed does not change the
;                   current speed of the RoboTrike (speed) and passing in -32768 for
;                   the angle does not change the current direction of the RoboTrike
;                   (direction)
;
; Operation:        The speed argument passed in AX is an absolute speed (0 is stopped
;                   and 65534 is MAX_SPEED) while 65535 indicates to not change the
;                   current speed (speed), which is equivalent to ignoring the speed
;                   argument. The angle argument passed in BX is a signed angle for
;                   the RoboTrike to move in, with 0 indicating to move in the same
;                   direction as the orientation of the RoboTrike. A value of -32768
;                   indicates to not change the current angle of motion (direction),
;                   which is equivalent to ignoring the direction argument.
;
; Arguments:        AX = absolute speed of robot to travel at
;                   BX = angle that robot should travel in
;
; Return Value:     None.
;
; Local Variables:  None.
; Shared Variables: motor_speeds (write), speed (write), direction (write)
; Global Variables: None.
;
; Input:            None.
; Output:           None.
;
; Error Handling:   None.
;
; Algorithms:       motor_speeds[i] = ForceX[i] * speed * cos(direction) +
;                                     ForceY[i] * speed * sin(direction)
; Data Structures:  None.
;
; Registers Changed:   None.
;
; Known Bugs:       None.
; Limitations:      None.
;
; Revision History:
;       11/7/2015      Anshul Ramachandran     func spec and pseudocode
;       11/13/2015     Anshul Ramachandran     assembly code

SetMotorSpeed         PROC    NEAR
                      PUBLIC  SetMotorSpeed

SaveRegistersSetMotorSpeed:
    PUSH AX                               ; Save registers
    PUSH BX
    PUSH DX
    PUSH SI

SetupForOverallSpeedChange:
    CMP AX, SPEED_NO_CHANGE               ; If we get the SPEED_NO_CHANGE signal
    JE NoChangeOverallSpeed               ; then skip changing speed shared variable
    ;JNE ChangeOverallSpeed               ; Otherwise, change overall speed

ChangeOverallSpeed:
    MOV speed, AX                         ; Update overall speed
    JMP SetupForDirectionChange           ; and move on to updating direction

NoChangeOverallSpeed:
    MOV AX, speed                         ; Get the current overall speed in AX for
    ;JMP SetupForDirectionChange          ; later calculations, and move on to
                                          ; updating direction

SetupForDirectionChange:
    CMP BX, DIR_NO_CHANGE                 ; If we get the DIR_NO_CHANGE signal,
    JE NoChangeDirection                  ; skip changing direction shared variable
    ;JNE ChangeDirection                  ; Otherwise change direction shared var

ChangeDirection:
    XCHG AX, BX                           ; Put new direction in AX (and save speed in
    CALL AngleMod                         ; BX) and normalize new direction between
    XCHG AX, BX                           ; ZERO_DEGREES and MAX_DEGREES, then switch
    MOV direction, BX                     ; speed/direction back, update shared var
    JMP FinishDirectionChange             ; and continue to finishing direction change

NoChangeDirection:
    MOV BX, direction                     ; In this case, we don’t want to change
                                          ; shared variable, but get current direction
                                          ; for later calculations
    ;JMP FinishDirectionChange            ; and continue to finishing direction change

FinishDirectionChange:
    SHL BX, 1                             ; multiply angle (in degrees) by two
                                          ; for lookup in sine/cosine table (which
                                          ; have values in words)

SetupForMotorSpeedsUpdate:
    MOV SI, 0                             ; SI will be counter for index of motor
                                          ; currently being updated (0, 1, or 2)

UpdateMotorSpeedStart:
    SHL SI, 1                             ; ForceX and ForceY tables are words

GetMotorSpeedXComponent:
    MOV AX, speed                         ; Put speed in AX (modified each loop)
    SHR AX, 1                             ; Lose precision in speed but need to
                                          ; make sign + for signed multiplication
    IMUL WORD PTR CS:ForceX[SI]           ; DX:AX = speed * ForceX[i]
    MOV AX, DX                            ; Truncate to just value in DX
    IMUL WORD PTR CS:Cos_Table[BX]        ; DX:AX = speed * ForceX[i] * cos(direction)
    PUSH DX                               ; Truncate to just DX and save this 
                                          ; component of motor speed on stack

GetMotorSpeedYComponent:
    MOV AX, speed                         ; Put speed in AX (modified each loop)
    SHR AX, 1                             ; Lose precision in speed but need to
                                          ; make sign + for signed multiplication
    IMUL WORD PTR CS:ForceY[SI]           ; DX:AX = speed * ForceY[i]
    MOV AX, DX                            ; Truncate to just value in DX
    IMUL WORD PTR CS:Sin_Table[BX]        ; DX:AX = speed * ForceY[i] * sin(direction)
    MOV AX, DX                            ; Truncate to just value in DX, put in AX

GetTotalMotorSpeed:
    POP DX                                ; Get x-component of speed in DX
    ADD AX, DX                            ; and add it to y-component of speed
    SAL AX, 2                             ; Get rid of extra sign bits
    SHR SI, 1                             ; Reverse original SI*2 to get motor number
    MOV motor_speeds[SI], AH              ; Only take the top byte of AX so that we
                                          ; truncate values between -PULSE_WIDTH_END
                                          ; (-127) to PULSE_WIDTH_END (127), so that
                                          ; motor_speeds serves as pulse widths (with 
                                          ; the sign meaning reverse if negative, 
                                          ; forwards if positive)

SetupForNextMotor:
    INC SI                                ; Go to next motor
    CMP SI, NUM_MOTORS                    ; If we have updated all motors
    JE RestoreRegistersSetMotorSpeed      ; go to restoring registers and returning
    JMP UpdateMotorSpeedStart             ; otherwise start the updating for the next
                                          ; motor

RestoreRegistersSetMotorSpeed:
    POP SI                                ; Restore registers
    POP DX
    POP BX
    POP AX

    RET

SetMotorSpeed         ENDP


; GetMotorSpeed
;
; Description:      This function returns the speed of the RoboTrike (0 - MAX_SPEED)
;
; Operation:        Returns the speed of the RoboTrike (0 - MAX_SPEED) in AX
;
; Arguments:        None.
;
; Return Value:     AX = speed of RoboTrike (0 - MAX_SPEED)
;
; Local Variables:  None.
; Shared Variables: speed (read)
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
; Registers Changed:   AX
;
; Known Bugs:       None.
; Limitations:      None.
;
; Revision History:
;       11/7/2015      Anshul Ramachandran     func spec and pseudocode
;       11/13/2015     Anshul Ramachandran     assembly code

GetMotorSpeed         PROC    NEAR
                      PUBLIC  GetMotorSpeed

RetrieveSpeedAndReturn:
    MOV AX, speed                         ; put current overall speed in output reg.
    RET

GetMotorSpeed         ENDP


; GetMotorDirection
;
; Description:      This function returns the current direction of the RoboTrike in
;                   degrees (0 - MAX_DEGREES)
;
; Operation:        Returns the direction of the RoboTrike in degrees 0 -
;                   MAX_DEGREES) in AX
;
; Arguments:        None.
;
; Return Value:     AX = direction of RoboTrike in degrees
;
; Local Variables:  None.
; Shared Variables: direction (read)
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
; Registers Changed:   AX
;
; Known Bugs:       None.
; Limitations:      None.
;
; Revision History:
;       11/7/2015      Anshul Ramachandran     func spec and pseudocode
;       11/13/2015     Anshul Ramachandran     assembly code

GetMotorDirection     PROC    NEAR
                      PUBLIC  GetMotorDirection

RetrieveDirectionAndReturn:
    MOV AX, direction                     ; put current direction in output register
    RET

GetMotorDirection     ENDP


; SetLaser
;
; Description:      This function is passed in an argument that has a value
;                   corresponding to either on or off, which is read by the function
;                   and changes laser_state accordingly
;
; Operation:        Function takes single argument in AX that corresponds to a laser
;                   state (0 = off, not 0 = on) and sets laser_state to OFF (0) or
;                   ON (1) respectively
;
; Arguments:        AX = 0 to turn laser off, not 0 to turn laser on
;
; Return Value:     None.
;
; Local Variables:  None.
; Shared Variables: laser_state (write)
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
; Registers Changed:   None.
;
; Known Bugs:       None.
; Limitations:      None.
;
; Revision History:
;       11/7/2015      Anshul Ramachandran     func spec and pseudocode
;       11/13/2015     Anshul Ramachandran     assembly code

SetLaser              PROC    NEAR
                      PUBLIC  SetLaser

CheckArgumentStatus:
    CMP AX, OFF                           ; Check if we are getting OFF signal
    JNE TurnOnLaser                       ; If we get anything but OFF signal, change
    ;JE TurnOffLaser                      ; laser state to ON otherwise turn it to OFF

TurnOffLaser:
    MOV laser_state, OFF                  ; Update shared laser_state variable
    JMP FinishSettingLaserState           ; and return

TurnOnLaser:
    MOV laser_state, ON                   ; Update shared laser_state variable
    ;JMP FinishSettingLaserState          ; and return

FinishSettingLaserState:
    RET

SetLaser              ENDP


; GetLaser
;
; Description:      This function returns the current laser state (laser_state), with
;                   0 meaning the laser is off (OFF) and 1 meaning the laser is on
;                   (ON)
;
; Operation:        Returns laser_state in AX
;
; Arguments:        None.
;
; Return Value:     AX = current laser state (laser_state)
;
; Local Variables:  None.
; Shared Variables: laser_state (read)
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
; Registers Changed:   None.
;
; Known Bugs:       None.
; Limitations:      None.
;
; Revision History:
;       11/7/2015      Anshul Ramachandran     func spec and pseudocode
;       11/13/2015     Anshul Ramachandran     assembly code

GetLaser              PROC    NEAR
                      PUBLIC  GetLaser

GetLaserStateAndReturn:
    MOV AX, 0                             ; Clear AX (returning full word register
                                          ; even though laser_state is one byte large)
    MOV AL, laser_state                   ; Put current laser_state in lower byte
    RET                                   ; and return

GetLaser              ENDP


; SetTurretAngle
;
; Description:      This is a dummy function.
; Operation:        NOP and RET.
;
; Arguments:        None.
; Return Value:     None.
;
; Local Variables:  None.
; Shared Variables: None.
; Global Variables: None.
;
; Input:            None.
; Output:           None.
;
; Error Handling:   None.
; Algorithms:       None.
; Data Structures:  None.
;
; Registers Changed:   None.
;
; Known Bugs:       None.
; Limitations:      None.
;
; Revision History:
;       11/7/2015      Anshul Ramachandran     func spec and pseudocode

SetTurretAngle              PROC    NEAR
                            PUBLIC  SetTurretAngle

    NOP
    RET

SetTurretAngle              ENDP


; SetRelTurretAngle
;
; Description:      This is a dummy function.
; Operation:        NOP and RET.
;
; Arguments:        None.
; Return Value:     None.
;
; Local Variables:  None.
; Shared Variables: None.
; Global Variables: None.
;
; Input:            None.
; Output:           None.
;
; Error Handling:   None.
; Algorithms:       None.
; Data Structures:  None.
;
; Registers Changed:   None.
;
; Known Bugs:       None.
; Limitations:      None.
;
; Revision History:
;       11/7/2015      Anshul Ramachandran     func spec and pseudocode

SetRelTurretAngle              PROC    NEAR
                               PUBLIC  SetRelTurretAngle

    NOP
    RET

SetRelTurretAngle              ENDP


; GetTurretAngle
;
; Description:      This is a dummy function.
; Operation:        NOP and RET.
;
; Arguments:        None.
; Return Value:     None.
;
; Local Variables:  None.
; Shared Variables: None.
; Global Variables: None.
;
; Input:            None.
; Output:           None.
;
; Error Handling:   None.
; Algorithms:       None.
; Data Structures:  None.
;
; Registers Changed:   None.
;
; Known Bugs:       None.
; Limitations:      None.
;
; Revision History:
;       11/7/2015      Anshul Ramachandran     func spec and pseudocode

GetTurretAngle              PROC    NEAR
                            PUBLIC  GetTurretAngle

    NOP
    RET

GetTurretAngle              ENDP

; GetRelTurretAngle
;
; Description:      This is a dummy function.
; Operation:        NOP and RET.
;
; Arguments:        None.
; Return Value:     None.
;
; Local Variables:  None.
; Shared Variables: None.
; Global Variables: None.
;
; Input:            None.
; Output:           None.
;
; Error Handling:   None.
; Algorithms:       None.
; Data Structures:  None.
;
; Registers Changed:   None.
;
; Known Bugs:       None.
; Limitations:      None.
;
; Revision History:
;       11/7/2015      Anshul Ramachandran     func spec and pseudocode

GetRelTurretAngle              PROC    NEAR
                               PUBLIC  GetRelTurretAngle

    NOP
    RET

GetRelTurretAngle              ENDP


; SetTurretElevation
;
; Description:      This is a dummy function.
; Operation:        NOP and RET.
;
; Arguments:        None.
; Return Value:     None.
;
; Local Variables:  None.
; Shared Variables: None.
; Global Variables: None.
;
; Input:            None.
; Output:           None.
;
; Error Handling:   None.
; Algorithms:       None.
; Data Structures:  None.
;
; Registers Changed:   None.
;
; Known Bugs:       None.
; Limitations:      None.
;
; Revision History:
;       11/7/2015      Anshul Ramachandran     func spec and pseudocode

SetTurretElevation              PROC    NEAR
                                PUBLIC  SetTurretElevation

    NOP
    RET

SetTurretElevation              ENDP


; GetTurretElevation
;
; Description:      This is a dummy function.
; Operation:        NOP and RET.
;
; Arguments:        None.
; Return Value:     None.
;
; Local Variables:  None.
; Shared Variables: None.
; Global Variables: None.
;
; Input:            None.
; Output:           None.
;
; Error Handling:   None.
; Algorithms:       None.
; Data Structures:  None.
;
; Registers Changed:   None.
;
; Known Bugs:       None.
; Limitations:      None.
;
; Revision History:
;       11/7/2015      Anshul Ramachandran     func spec and pseudocode

GetTurretElevation              PROC    NEAR
                                PUBLIC  GetTurretElevation

    NOP
    RET

GetTurretElevation              ENDP


; PWMHandler
;
; Description:      Actually controls the motion of the PWM motors. Writes the bits
;                   that control the PWM motors into the portb_buffer, both the one
;                   that directs the motion of the motor (forwards or reverse) and
;                   the one that signifies whether the motor is ON or OFF (based off
;                   motor_speeds, which also serves as the pulse width). The values
;                   in portb_buffer are then outputted into portB after the bits
;                   for all motors have been se.
;
; Operation:        This function is called when a timer 0 interrupt happens. For
;                   each motor, writes a 1/0 using MotorOnMask/MotorOffMask to
;                   the motor’s on/off bit (the high bit in each bit pair) depending
;                   on the pulse width specified by motor_speeds. If the value of 
;                   motor_speeds is less than the current pulse width count (pw_count)
;                   then we use the OR MotorOnMask to turn on the motor, and if
;                   greater, then we use the AND MotorOffMask to turn off the 
;                   motor. We do the same with the bit that says whether the motor
;                   should run forwards or in reverse except using the AND 
;                   MotorForwardMask and the OR MotorReverseMask respectively. We
;                   then increment the value of the pulse width count so that we
;                   test the next count in the next interrupt, looping back to 0
;                   (PULSE_WIDTH_START) if we reach PULSE_WIDTH_END (which is the
;                   total width of the pulse). Finally, we write the bits now stored
;                   in portb_buffer to port B.
;
; Arguments:        None.
;
; Return Value:     None.
;
; Local Variables:  None.
; Shared Variables: motor_speeds (read), pw_counter (read/write)
; Global Variables: None.
;
; Input:            None.
; Output:           the three PWM motors of the RoboTrike (changes speed/direction of
;                   RoboTrike by modifying the speeds/directions of the motors)
;
; Error Handling:   None.
;
; Algorithms:       pulse width modulation
; Data Structures:  None.
;
; Registers Changed:   None.
;
; Known Bugs:       None.
; Limitations:      None.
;
; Revision History:
;       11/7/2015      Anshul Ramachandran     func spec and pseudocode
;       11/13/2015     Anshul Ramachandran     assembly code

PWMHandler            PROC    NEAR
                      PUBLIC  PWMHandler

SaveRegistersPWMHandler:
    PUSH AX                               ; Save registers
    PUSH BX
    PUSH CX
    PUSH DX
    PUSH SI

SetupForPWMHandler:
    MOV SI, 0                             ; SI will be counter for index of motor
                                          ; currently being updated (0, 1, or 2)
    MOV CX, 0                             ; CL will hold the 8-bit pattern that 
                                          ; will be outputted through port B

StartMotorMasking:
    MOV AX, 0                             ; clear AX
    MOV AL, motor_speeds[SI]              ; AX has signed pulse width
    CMP AL, ZERO_SPEED                    ; Compare the pulse width with 0
    JE GetNextMotor                       ; if pulse width = 0, we don’t want to turn
                                          ; motor on at all (keep bits 00), so we just
                                          ; continue to next motor
    JG CheckIfMotorOn                     ; if pulse width > 0, motor is in forward
                                          ; so no need to set the forward bit for the
                                          ; motor, just perhaps the on bit
    ;JL SetMotorReverse                   ; if pulse width < 0, motor is in reverse
                                          ; so we need to set the reverse bit for
                                          ; the bit and perhaps the on bit

SetMotorReverse:
    MOV BL, MotorReverseMask[SI]          ; Get the reverse mask for the motor
    OR CL, BL                             ; and set the direction bit
    NEG AL                                ; Get the absolute value of the pulse width
                                          ; to check if motor is on or not
    ;JMP CheckIfMotorOn                   ; Go to checking whether the bit should
                                          ; even be on

CheckIfMotorOn:
    CMP AL, pw_counter                    ; Compare pulse width with pw_counter
    JB GetNextMotor                       ; if pulse width < pw_counter, the motor
                                          ; should be off so we don’t want to do 
                                          ; anything with the on/off bit, so go to
                                          ; next motor
    ;JNB SetMotorOn                       ; if pulse_width >= pw_counter, the motor
                                          ; should be on so set on/off bit

SetMotorOn:
    MOV BL, MotorOnMask[SI]               ; Get the on mask for the motor
    OR CL, BL                             ; and set the on/off bit
    ;JMP GetNextMotor                     ; Go to next motor

GetNextMotor:
    INC SI                                ; Get next motor
    CMP SI, NUM_MOTORS                    ; If we have set bits for all motors
    JE SetLaserStateBit                   ; go to setting the laser state bit
    JMP StartMotorMasking                 ; otherwise start the bit masking for the
                                          ; next motor

SetLaserStateBit:
    CMP laser_state, OFF                  ; if wanted to be OFF, we don’t want to set
    JE GetNextPWCounter                   ; the bit so continue to incrementing 
                                          ; pw_counter for the next interrupt
    ;JNE SetLaserStateBitOn               ; otherwise, we need to set the laser bit on

SetLaserStateBitOn:
    OR CL, LASER_ON_MASK                  ; Set the laser state bit on
    ;JMP GetNextPWCounter                 ; and continue to incrementing pw_counter

GetNextPWCounter:
    CMP pw_counter, PULSE_WIDTH_END       ; if we were at the end of the pulse width,
    JE WrapPWCounter                      ; we want the pw_counter to go back to 
                                          ; the front of the pulse width
    ;JNE IncrementPWCounter               ; otherwise we just need to increment

IncrementPWCounter:
    INC pw_counter                        ; just get the next value for pw_counter
    JMP OutputToPortB                     ; and continue to sending to port B

WrapPWCounter:
    MOV pw_counter, PULSE_WIDTH_START     ; set pw_counter to the start of the pulse
                                          ; width (looping)
    ;JMP OutputToPortB                    ; and continue to sending to port B

OutputToPortB:
    MOV DX, PORTB                         ; get the location of port B
    MOV AL, CL                            ; and put the bit pattern in AL for output
    OUT DX, AL                            ; and send the bit pattern

RestoreRegistersPWMHandler:
    POP SI                                ; Restore registers
    POP DX
    POP CX
    POP BX
    POP AX
  
    RET

PWMHandler            ENDP


; AngleMod
;
; Description:      This function converts a signed angle to its equivalent between
;                   0 (ZERO_DEGREES) and 360 (MAX_DEGREES) degrees.
;
; Operation:        Takes a signed angle in AX and normalizes it by taking the 
;                   modulus of the angle with respect to MAX_DEGREES and correcting
;                   for sign.
;
; Arguments:        AX = signed angle
;
; Return Value:     AX = normalized angle
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
; Algorithms:       None.
; Data Structures:  None.
;
; Registers Changed:   AX
;
; Known Bugs:       None.
; Limitations:      None.
;
; Revision History:
;       11/7/2015      Anshul Ramachandran     func spec and pseudocode
;       11/13/2015     Anshul Ramachandran     assembly code

AngleMod        PROC    NEAR
                PUBLIC  AngleMod

SaveRegistersAngleMod:
    PUSH BX                               ; Save registers (other than AX)
    PUSH DX

TakeMod360:
    MOV BX, MAX_DEGREES                   ; Take the input direction angle and perform
    CWD                                   ; signed division by MAX_DEGREES (360) to 
    IDIV BX                               ; get a value between -360 and 360 in DX
                                          ; (the remainder upon division)

CheckForNegativeAngle:
    CMP DX, ZERO_DEGREES                  ; If we get a >= 0 angle, we have a good 
    JNLE AngleCorrected                   ; answer, so continue to returning; if not,
    ;JLE CorrectNegativeAngle             ; correct the negative angle to its positive
                                          ; equivalent

CorrectNegativeAngle:
    ADD DX, MAX_DEGREES                   ; We get the positive equivalent to a neg
    ;JMP AngleCorrected                   ; angle between -360 and 0 by adding 360

AngleCorrected:
    MOV AX, DX                            ; Move corrected direction into AX (return 
                                          ; register)

RestoreRegistersAngleMod:
    POP DX                                ; Restore pushed registers
    POP BX

    RET

AngleMod        ENDP


CODE ENDS


DATA SEGMENT PUBLIC 'DATA'

    ;overall speed of RoboTrike
    speed               DW     ?

    ;direction of RoboTrike
    direction           DW     ?
    
    ;array to hold the speed of each motor (also serves as pulse widths of the motors)
    motor_speeds        DB     NUM_MOTORS    dup (?)

    ;pulse width counter for the motors
    pw_counter          DB     ?

    ;state of the laser (0 = off, anything else = on)
    laser_state         DB     ?

DATA ENDS


    END