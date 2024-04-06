; vim: ft=basm
?section "HOME"

?include "hardware.inc"

; modifies a, hl
JoyUpdate::
    ld a, (1 << HW_P1_BIT_GET_DIRECTION)
    ldh [HW_P1], a
    ldh a, [HW_P1]
    ldh a, [HW_P1]

    ; store bits in upper nibble
    cpl
    and a, $0F
    swap a
    ld l, a

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

    ; now we have the button states for this frame in l
    ; AND with JoyPressed to get the held buttons
    ldh a, [hJoyPressed]
    and a, l
    ldh [hJoyHeld], a

    ; AND with cpl to get released buttons this frame
    ld a, l
    cpl
    ld h, a
    ldh a, [hJoyPressed]
    and a, h
    ldh [hJoyReleased], a

    ; finally update the buttons for the current frame
    ; XOR with the current pressed so filter out held
    ldh a, [hJoyPressed]
    xor a, l
    ldh [hJoyPressed], a

    ret
