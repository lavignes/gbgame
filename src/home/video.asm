; vim: ft=basm
?include "hardware.inc"
?include "color.inc"

?section "OAMBUF"

;; OAM source buffer
oamBuf: ?res HW_MAP_OAM_SIZE

?section "HRAM"

;; Holds the dma transfer routine
dmaFunction: ?res 10

;; Whether vblank occurred
vBlanked:: ?res 1

?section "HOME"

;; DMA function that we copy to hram
_TMP = *
DMAFunction:
    ld a, >oamBuf
    ldh [HW_DMA], a
    ld a, 40
.Wait
    dec a
    jr nz, .Wait
    ret
DMAFunctionEnd:
?if (* - _TMP) != 10
    ?fail "DMA function too big!"
?end

;; Initialize video sub-system
VideoInit::
    ; place the dma function into hram
    ld hl, dmaFunction
    ld de, DMAFunction
    ld bc, DMAFunctionEnd - DMAFunction
    call MemCopy
    ; clear vram
    ?for BANK, 0, 2
        ld a, BANK
        ldh [HW_VBK], a
        ld hl, HW_MAP_VRAM_START
        ld bc, HW_MAP_VRAM_SIZE
        call MemZero
    ?end
    ; clear palettes
    ; TODO we need a function to fade palettes, so we should reuse that
    ?for INDEX, 0, 8
        ld a, 1
        ld c, a
        ld a, INDEX
        ld hl, PaletteAllBlack
        call VideoBGPaletteWrite
        ld a, 1
        ld c, a
        ld a, INDEX
        ld hl, PaletteAllBlack
        call VideoOBJPaletteWrite
    ?end
    xor a, a
    ldh [HW_VBK], a
    ldh [HW_WY], a
    ldh [HW_WX], a
    ldh [HW_SCY], a
    ldh [HW_SCX], a
    ldh [HW_LYC], a
    ldh [HW_STAT], a
    ld a, (1 << HW_LCDC_BIT_BG_WIN_PRIORITY) |\
          (1 << HW_LCDC_BIT_OBJ_ENABLE) |\
          (1 << HW_LCDC_BIT_OBJ_SIZE) |\
          (1 << HW_LCDC_BIT_WIN_TILE_MAP)
    ldh [HW_LCDC], a
    ret

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

;; Copy `C` BG palettes from `HL` to palette index `A`
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

;; copy `C` OBJ palettes from `HL` to palette index `A`
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
