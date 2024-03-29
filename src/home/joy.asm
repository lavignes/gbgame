; vim: ft=basm
?section "HOME"

?include "hardware.inc"

BIT_GET_ACTION = 5
BIT_GET_DIRECTION = 4

JoyUpdate::
    ld a, (1 << BIT_GET_DIRECTION)
    ldh [HW_IO_P1], a
    ldh a, [HW_IO_P1]
    ldh a, [HW_IO_P1]

    ; store bits in upper nibble
    cpl
    and a, $0F
    swap a
    ld b, a

    ld a, (1 << BIT_GET_ACTION)
    ldh [HW_IO_P1], a
    ldh a, [HW_IO_P1]
    ldh a, [HW_IO_P1]
    ldh a, [HW_IO_P1]
    ldh a, [HW_IO_P1]
    ldh a, [HW_IO_P1]
    ldh a, [HW_IO_P1]

    ; store in lower nibble
    cpl
    and a, $0F
    or a, b
    ld b, a

    ; reset joypad
    ld a, (1 << BIT_GET_DIRECTION) | (1 << BIT_GET_ACTION)
    ldh [HW_IO_P1], a

    ; now we have the button states for this frame in b
    ; & with JoyPressed to get the held buttons
    ldh a, [HRAM.JoyPressed]

    ret
