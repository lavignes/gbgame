; vim: ft=basm
?section "HOME"

?include "hardware.inc"

Boop:
    ld a, (1 << HW_NR52_BIT_MASTER_ENABLE)
    ldh [HW_NR52], a
    ld a, $FF
    ldh [HW_NR51], a
    ld a, %0_111_0_111
    ld a, (1 << HW_NR14_BIT_TRIGGER)
    ldh [HW_NR14], a
    ret

Start::
    di
    ld sp, stack

    call VideoDisable
    call StartDoubleSpeedMode
    call Boop

    ei
    jr *

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
