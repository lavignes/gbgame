; vim: ft=basm
?include "hardware.inc"

?section "WRAM0"

stackTop: ?res 256
stackBase:

?section "HOME"

DoubleSpeedMode:
    ; Exit if already in double speed mode
    ld hl, HW_KEY1
    bit HW_KEY1_BIT_CURRENT_SPEED, [hl]
    ret nz
    ; Disable all interrupt flags (stop acts really weird without these)
    xor a, a
    ldh [HW_IE], a
    ldh [HW_IF], a
    ; Prepare speed switch
    set HW_KEY1_BIT_PREPARE_SWITCH, [hl]
    ; Do weird write to joypad GPIO and stop to perform the switch
    ld a, (1 << HW_P1_BIT_GET_ACTION) | (1 << HW_P1_BIT_GET_DIRECTION)
    ldh [HW_P1], a
    stop
    ret

Start::
    di
    ld sp, stackBase
    call VideoDisable
    call DoubleSpeedMode
    ; Clear HRAM
    ld hl, HW_MAP_HRAM_START
    ld bc, HW_MAP_HRAM_SIZE
    call MemZero
    ; Initialize the current ROM bank
    ld a, 1
    ldh [romBank], a
    ld [HW_MAP_MBC5_BANK_LO], a
    ; We need to be careful and clear WRAM0
    ; without making a call since we'll clobber the stack
    ld hl, HW_MAP_WRAM0_START
    ld bc, HW_MAP_WRAM0_SIZE
.WRAM0:
    ld a, c
    or a, b
    jr z, .WRAMX
    xor a, a
    ldi [hl], a
    dec bc
    jr .WRAM0
.WRAMX:
    ; Clear WRAMX
    ?for BANK, 1, 8
        ld a, BANK
        ldh [HW_SVBK], a
        ld hl, HW_MAP_WRAMX_START
        ld bc, HW_MAP_WRAMX_SIZE
        call MemZero
    ?end
    ; Use WRAM1
    ld a, 1
    ldh [HW_SVBK], a
    ; Clear timers
    xor a, a
    ldh [HW_TMA], a
    ldh [HW_TAC], a
    ldh [HW_DIV], a
    ; Clear serial
    ldh [HW_SC], a
    ; Init subsystems
    call VideoInit
    ;call AudioInit

    ei
    call VideoEnable
.Halt:
    halt
    jr .Halt

