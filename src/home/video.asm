; vim: ft=basm
?section "HOME"

?include "hardware.inc"

VideoDisable::
    ; already disabled?
    ld hl, HW_LCDC
    bit HW_LCDC_BIT_SCREEN_ENABLE, [hl]
    ret z

    ; wait for vblank
    ld hl, HW_STAT
    ld a, 1
.Wait:
    cp a, [hl]
    jr nz, .Wait

    ; disable screen
    ld hl, HW_LCDC
    res HW_LCDC_BIT_SCREEN_ENABLE, [hl]
    ret

VideoEnable::
    ld hl, HW_LCDC
    set HW_LCDC_BIT_SCREEN_ENABLE, [hl]
    ret

; waits for _any_ blank event
VideoWaitForBlank::
    ld hl, HW_STAT
.Wait:
    bit 1, [hl]
    jr nz, .Wait
    ret
