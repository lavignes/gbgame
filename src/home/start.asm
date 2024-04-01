; vim: ft=basm
?section "HOME"

?include "hardware.inc"

Start::
    ld sp, wStack
    call StartDoubleSpeedMode

    ei
    jr *

StartDoubleSpeedMode:
    ; exit if already in double speed mode
    ld hl, HW_IO_KEY1
    bit 7, [hl]
    ret nz

    ; disable all interrupt flags (stop acts really weird without these)
    xor a, a
    ldh [HW_IO_IE], a
    ldh [HW_IO_IF], a

    ; prepare speed switch
    set 0, [hl]

    ; do weird write to joypad and stop to perform the switch
    ld a, (1 << HW_IO_P1_BIT_GET_ACTION) | (1 << HW_IO_P1_BIT_GET_DIRECTION)
    ldh [HW_IO_P1], a
    stop

    ret
