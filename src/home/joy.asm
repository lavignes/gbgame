; vim: ft=basm
?include "hardware.inc"

?section "HRAM"

joyPressed::   ?res 1
joyHeld::      ?res 1
joyReleased::  ?res 1

?section "HOME"

JoyUpdate::
    ; read directions
    ld a, (1 << HW_P1_BIT_GET_DIRECTION)
    ldh [HW_P1], a
    ldh a, [HW_P1]
    ldh a, [HW_P1]
    ; store bits in upper nibble
    cpl
    and a, $0F
    swap a
    ld l, a
    ; read buttons
    ld a, (1 << HW_P1_BIT_GET_ACTION)
    ldh [HW_P1], a
    ldh a, [HW_P1]
    ldh a, [HW_P1]
    ldh a, [HW_P1]
    ldh a, [HW_P1]
    ldh a, [HW_P1]
    ldh a, [HW_P1]
    ; store in lower nibble
    cpl
    and a, $0F
    or a, l
    ld l, a
    ; reset joypad
    ld a, (1 << HW_P1_BIT_GET_DIRECTION) | (1 << HW_P1_BIT_GET_ACTION)
    ldh [HW_P1], a
    ; Now we have the button states for this frame in `L`
    ; AND with the last frame to get the held buttons
    ldh a, [joyPressed]
    and a, l
    ldh [joyHeld], a
    ; AND with complement to get released buttons this frame
    ld a, l
    cpl
    ld h, a
    ldh a, [joyPressed]
    and a, h
    ldh [joyReleased], a
    ; Finally update the buttons for the current frame
    ; XOR with the current pressed so filter out held
    ldh a, [joyPressed]
    xor a, l
    ldh [joyPressed], a
    ret
