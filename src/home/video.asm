; vim: ft=basm
?section "HOME"

?include "hardware.inc"

; modifies a, hl
VideoDisable::
    ; already disabled?
    ld hl, HW_LCDC
    bit HW_LCDC_BIT_SCREEN_ENABLE, [hl]
    ret z

    ; wait for vblank
    ld hl, HW_STAT
    ld a, 1
.wait:
    cp a, [hl]
    jr nz, .wait

    ; disable screen
    ld hl, HW_LCDC
    res HW_LCDC_BIT_SCREEN_ENABLE, [hl]
    ret

; modifies: hl
VideoEnable::
    ld hl, HW_LCDC
    set HW_LCDC_BIT_SCREEN_ENABLE, [hl]
    ret

; waits for _any_ blank event
;
; modifies: hl
VideoWaitForBlank::
    ld hl, HW_STAT
.wait:
    bit 1, [hl]
    jr nz, .wait
    ret
