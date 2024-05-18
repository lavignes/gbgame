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

;; waits for _any_ blank event
;
; TODO should I delete this?
VideoWaitForAnyBlank::
    ld hl, HW_STAT
.Wait:
    bit 2, [hl] ; drawing modes have bit 2 set
    jr nz, .Wait
    ret

;; copy c BG palettes from hl to palette index a
VideoBGPaletteWrite::
    sla c ; c *= 4
    sla c
    add a, a ; a *= 8
    add a, a
    add a, a
    ; load pal index
    ld de, HW_BCPS
    set HW_BCPS_BIT_INCREMENT, a ; auto-increment
    ld [de], a
    ; de is now HW_BCPD
    inc de
.Loop:
    ; each iteration copies 1 16-bit color
    ldi a, [hl]
    ld [de], a
    ldi a, [hl]
    ld [de], a
    dec c
    jr nz, .Loop
    ret

;; copy c OBJ palettes from hl to palette index a
VideoOBJPaletteWrite::
    sla c ; c *= 4
    sla c
    add a, a ; a *= 8
    add a, a
    add a, a
    ; load pal index
    ld de, HW_OCPS
    set HW_OCPS_BIT_INCREMENT, a ; auto-increment
    ld [de], a
    ; de is now HW_OCPD
    inc de
    jr VideoBGPaletteWrite.Loop
