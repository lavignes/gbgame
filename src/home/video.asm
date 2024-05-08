; vim: ft=basm
?section "HOME"

?include "hardware.inc"

; NOTE only call after disabling PPU interrupts
;
; TODO should I therefore automatically disable interrupts?
VideoDisable::
    ; already disabled?
    ld hl, HW_LCDC
    bit HW_LCDC_BIT_SCREEN_ENABLE, [hl]
    ret z

    call VideoWaitForVBlank

    ; disable screen
    ld hl, HW_LCDC
    res HW_LCDC_BIT_SCREEN_ENABLE, [hl]
    ret

VideoEnable::
    ld hl, HW_LCDC
    set HW_LCDC_BIT_SCREEN_ENABLE, [hl]
    ret

VideoWaitForVBlank::
    ; wait for vblank
    ld hl, HW_STAT
.Wait:
    ld a, [hl]
    and a, HW_STAT_MASK_PPU_MODE
    jr nz, .Wait
    ret

; waits for _any_ blank event
;
; TODO should I delete this?
VideoWaitForAnyBlank::
    ld hl, HW_STAT
.Wait:
    bit 2, [hl] ; drawing modes have bit 2 set
    jr nz, .Wait
    ret
