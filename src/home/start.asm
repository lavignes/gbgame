; vim: ft=basm
?section "HOME"

?include "hardware.inc"
?include "debug.inc"

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
    call StartDoubleSpeedMode
    call VideoDisable
    call Boop
    call StartClearRAM
    ; reset a bunch of registers
    xor a, a
    ldh [HW_SVBK], a
    ldh [HW_VBK], a
    ld a, 1
    ldh [romBank], a
    ld [HW_MAP_MBC5_BANK_LO], a

    ei
.Halt:
    halt
    jr .Halt

StartClearRAM:
    ld hl, __HRAM_START__
    ld bc, __HRAM_SIZE__
    call MemZero
    ; we need to be careful and clear wram0
    ; without making a call since we clobber the stack
    ld hl, __WRAM0_START__
    ld bc, __WRAM0_SIZE__
.ClearWRAM0:
    ld a, c
    or a, b
    jr z, .ClearWRAM1_7
    xor a, a
    ldi [hl], a
    dec bc
    jr .ClearWRAM0
.ClearWRAM1_7:
    ; clear other wrams
?for BANK, 1, 7
    ld a, BANK
    ldh [HW_SVBK], a
    ld hl, \j __WRAM, BANK, _START__
    ld bc, \j __WRAM, BANK, _SIZE__
    call MemZero
?end
?for BANK, 0, 1
    ld a, BANK
    ldh [HW_VBK], a
    ld hl, HW_MAP_VRAM_START
    ld bc, HW_MAP_VRAM_SIZE
    call MemZero
?end
    ret

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

;; print bc bytes of hl to the serial port and halt
StartPanic::
    ; no more chars?
    ld a, c
    or a, b
    jr z, .Halt
.Wait:
    ; are we still txing?
    ld a, [HW_SC]
    bit HW_SC_BIT_TX_ENABLE, a
    jr nz, .Wait
    ; send next char
    ldi a, [hl]
    dec bc
    ldh [HW_SB], a
    ld a, (1 << HW_SC_BIT_TX_ENABLE) |\
          (1 << HW_SC_BIT_CLOCK_HI_SPEED) |\
          (1 << HW_SC_BIT_CLOCK_MASTER)
    ldh [HW_SC], a
    jr StartPanic
.Halt:
    halt
    jr .Halt
