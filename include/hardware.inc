; vim: ft=basm
?ifndef HW_INC
HW_INC = 1

HW_ROM_HEADER_COMPAT_CGB_ONLY = $C0
HW_ROM_HEADER_MBC_RAM_BAT = $1B
HW_ROM_HEADER_ROM_SIZE_4096KB = $07
HW_ROM_HEADER_SRAM_SIZE_128KB = $04

?macro HW_ROM_HEADER
    ; tendy logo
    ?data8 $CE, $ED, $66, $66, $CC, $0D, $00, $0B
    ?data8 $03, $73, $00, $83, $00, $0C, $00, $0D
    ?data8 $00, $08, $11, $1F, $88, $89, $00, $0E
    ?data8 $DC, $CC, $6E, $E6, $DD, $DD, $D9, $99
    ?data8 $BB, $BB, $67, $63, $6E, $0E, $EC, $CC
    ?data8 $DD, $DC, $99, $9F, $BB, $B9, $33, $3E

    ?data8 "GAME", $00, $00, $00, $00, $00, $00, $00
    ?data8 "BASM"

    ?data8  \1      ; compat
    ?data16 $0000   ; license code
    ?data8  $00     ; no sgb
    ?data8  \2      ; MBC type
    ?data8  \3      ; ROM size
    ?data8  \4      ; SRAM size
    ?data8  $01     ; international
?end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; video
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

HW_SCREEN_WIDTH  = 160
HW_SCREEN_HEIGHT = 144

; LCD control register
HW_LCDC = $FF40
HW_LCDC_BIT_BG_WIN_PRIORITY = 0
HW_LCDC_BIT_OBJ_ENABLE      = 1
HW_LCDC_BIT_OBJ_SIZE        = 2
HW_LCDC_BIT_BG_TILE_MAP     = 3
HW_LCDC_BIT_TILE_DATA       = 4
HW_LCDC_BIT_WIN_ENABLE      = 5
HW_LCDC_BIT_WIN_TILE_MAP    = 6
HW_LCDC_BIT_SCREEN_ENABLE   = 7

; LCD status register
HW_STAT = $FF41
HW_SCY = $FF42
HW_SCX = $FF43

; LCD horizontal line counter register
;
; (read-only) holds a value between 0-153 (inclusive).
; values >= 144 indicate v-blank
HW_LY = $FF44
HW_LY_VBLANK_START = HW_SCREEN_HEIGHT
HW_LY_VBLANK_END   = 153

HW_LYC = $FF45
HW_DMA = $FF46
HW_BGP = $FF47
HW_OBP0 = $FF48
HW_OBP1 = $FF49
HW_WY = $FF4A
HW_WX = $FF4B

HW_BCPS = $FF68
HW_BCPD = $FF69
HW_OCPS = $FF6A
HW_OCPD = $FF6B

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hdma
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

HW_HDMA1 = $FF51
HW_HDMA2 = $FF52
HW_HDMA3 = $FF53
HW_HDMA4 = $FF54
HW_HDMA5 = $FF55

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; timer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

HW_DIV = $FF03
HW_TIMA = $FF04
HW_TMA = $FF05
HW_TAC = $FF06

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; serial and ir
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

HW_SB = $FF01
HW_SC = $FF02
HW_RP = $FF56

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sound
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

HW_NR50 = $FF24
HW_NR51 = $FF25
HW_NR52 = $FF26

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

HW_P1 = $FF00
HW_P1_BIT_GET_DIRECTION = 4
HW_P1_BIT_GET_ACTION    = 5

HW_KEY1 = $FF4D
HW_KEY1_BIT_PREPARE_SWITCH = 0
HW_KEY1_BIT_CURRENT_SPEED  = 7

HW_VBK = $FF4F
HW_SVBK = $FF70

HW_IF = $FF0F
HW_IE = $FFFF
HW_IF_BIT_VBLANK = 0
HW_IF_BIT_LCD    = 1
HW_IF_BIT_TIMER  = 2
HW_IF_BIT_SERIAL = 3
HW_IF_BIT_JOY    = 4

?end ; HW_INC
