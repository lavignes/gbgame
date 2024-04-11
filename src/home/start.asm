; vim: ft=basm
?section "HOME"

?include "hardware.inc"

Start::
    ld sp, wStack
    call StartDoubleSpeedMode

    ei
    jr *

; modifies a, hl
StartDoubleSpeedMode:
    ; exit if already in double speed mode
    ld hl, HW_KEY1
    bit HW_KEY1_BIT_CURRENT_SPEED, [hl]
    ret nz

    ; disable all interrupt flags (stop acts really weird without these)
    xor a, a
    ldh [HW_IE], a
    ldh [HW_IF], a

    ; prepare speed switch
    set HW_KEY1_BIT_PREPARE_SWITCH, [hl]

    ; do weird write to joypad and stop to perform the switch
    ld a, (1 << HW_P1_BIT_GET_ACTION) | (1 << HW_P1_BIT_GET_DIRECTION)
    ldh [HW_P1], a
    stop

    ret
