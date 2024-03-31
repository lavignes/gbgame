; vim: ft=basm
?section "HOME"

?include "hardware.inc"

BIT_GET_ACTION = 5
BIT_GET_DIRECTION = 4

; modifies a, bc
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
    ; AND with JoyPressed to get the held buttons
    ldh a, [hJoyPressed]
    and a, b
    ldh [hJoyHeld], a

    ; AND with cpl to get released buttons this frame
    ld a, b
    cpl
    ld c, a
    ldh a, [hJoyPressed]
    and a, c
    ldh [hJoyReleased], a

    ; finally update the buttons for the current frame
    ; XOR with the current pressed so filter out held
    ldh a, [hJoyPressed]
    xor a, b
    ldh [hJoyPressed], a

    ret
