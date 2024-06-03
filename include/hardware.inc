; vim: ft=basm
?ifndef HW_INC
HW_INC = 1

HW_ROM_HEADER_COMPAT_CGB_ONLY = $C0
HW_ROM_HEADER_MBC_RAM_BAT = $1B
HW_ROM_HEADER_ROM_SIZE_4096KB = $07
HW_ROM_HEADER_SRAM_SIZE_128KB = $04

?macro HW_ROM_HEADER
    ; tendy logo
    ?byte $CE, $ED, $66, $66, $CC, $0D, $00, $0B
    ?byte $03, $73, $00, $83, $00, $0C, $00, $0D
    ?byte $00, $08, $11, $1F, $88, $89, $00, $0E
    ?byte $DC, $CC, $6E, $E6, $DD, $DD, $D9, $99
    ?byte $BB, $BB, $67, $63, $6E, $0E, $EC, $CC
    ?byte $DD, $DC, $99, $9F, $BB, $B9, $33, $3E

    ?byte \1       ; Title

    ?if ?len \1 > 11
        ?fail "game title too long"
    ?end
    ?for \u, 0, 11 - ?len \1
        ?byte $00
    ?end

    ?byte "BASM"   ; Manufacturer code

    ?byte  \2      ; Compat
    ?word  $0000   ; License code
    ?byte  $00     ; No SGB
    ?byte  \3      ; MBC type
    ?byte  \4      ; ROM size
    ?byte  \5      ; SRAM size
    ?byte  $01     ; International
?end ; HW_ROM_HEADER

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Memory map
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
HW_MAP_ROM00_START    = $0000
HW_MAP_ROM00_SIZE     = $4000
HW_MAP_ROMXX_START    = $4000
HW_MAP_ROMXX_SIZE     = $4000
HW_MAP_VRAM_START     = $8000
HW_MAP_VRAM_SIZE      = $2000
HW_MAP_SRAM_START     = $A000
HW_MAP_SRAM_SIZE      = $2000
HW_MAP_WRAM0_START    = $C000
HW_MAP_WRAM0_SIZE     = $1000
HW_MAP_WRAMX_START    = $D000
HW_MAP_WRAMX_SIZE     = $1000
HW_MAP_OAM_START      = $FE00
HW_MAP_OAM_SIZE       = $A0
HW_MAP_WAVE_RAM_START = $FF30
HW_MAP_WAVE_RAM_SIZE  = 16
HW_MAP_HRAM_START     = $FF80
HW_MAP_HRAM_SIZE      = $7E

HW_MAP_MBC5_BANK_LO   = $2222

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Video
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

HW_SCREEN_WIDTH  = 160
HW_SCREEN_HEIGHT = 144

;; OAM
?struct HW_OAM
    .Y     1
    .X     1
    .Tile  1
    .Attrs 1
?end ; HW_OAM
HW_OAM_LEN = 40 ; Logical number of OAM entries
HW_OAM_ATTR_MASK_PALETTE    = %11
HW_OAM_ATTR_BIT_BANK        = 3   ; VRAM bank
HW_OAM_ATTR_BIT_DMG_PALETTE = 4
HW_OAM_ATTR_BIT_FLIP_X      = 5
HW_OAM_ATTR_BIT_FLIP_Y      = 6
HW_OAM_ATTR_BIT_PRIORITY    = 7   ; 1 = BG and WIN colors 1-3 draw above
?if (HW_OAM.SIZE * HW_OAM_LEN) != HW_MAP_OAM_SIZE
    ?fail "OAM struct incorrectly sized!"
?end

;; LCD control register
HW_LCDC = $FF40
HW_LCDC_BIT_BG_WIN_PRIORITY = 0 ; 0 = OBJs always above, 1 = Honor priority
HW_LCDC_BIT_OBJ_ENABLE      = 1
HW_LCDC_BIT_OBJ_SIZE        = 2 ; 0 = 8x8, 1 = 8x16
HW_LCDC_BIT_BG_TILE_MAP     = 3 ; 0 = 9800..9BFF, 1 = 9C00..9FFF
HW_LCDC_BIT_TILE_DATA       = 4 ; 0 = 8800..97FF, 1 = 8000..8FFF
HW_LCDC_BIT_WIN_ENABLE      = 5
HW_LCDC_BIT_WIN_TILE_MAP    = 6 ; 0 = 9800..9BFF, 1 = 9C00..9FFF
HW_LCDC_BIT_SCREEN_ENABLE   = 7

;; LCD status register
HW_STAT = $FF41
HW_STAT_MASK_PPU_MODE        = %11
HW_STAT_BIT_LY_EQUALS_LYC    = 2
HW_STAT_BIT_MODE0_INT_ENABLE = 3
HW_STAT_BIT_MODE1_INT_ENABLE = 4
HW_STAT_BIT_MODE2_INT_ENABLE = 5
HW_STAT_BIT_LYC_INT_ENABLE   = 6

;; LCD scroll
HW_SCY = $FF42
HW_SCX = $FF43

;; LCD horizontal line counter register
;;
;; (read-only) Holds a value between 0-153 (inclusive).
;; values >= 144 indicate v-blank
HW_LY = $FF44
HW_LY_VBLANK_START = HW_SCREEN_HEIGHT
HW_LY_VBLANK_END   = 153

HW_LYC = $FF45

;; DMA page source
HW_DMA = $FF46

;; DMG palettes
HW_BGP = $FF47
HW_OBP0 = $FF48
HW_OBP1 = $FF49

;; Window scroll
HW_WY = $FF4A
HW_WX = $FF4B

;; BG palette index
HW_BCPS = $FF68
HW_BCPS_BIT_INCREMENT = 7 ; auto-increment index on data write

;; BG palette data
HW_BCPD = $FF69

;; OBJ palette index
HW_OCPS = $FF6A
HW_OCPS_BIT_INCREMENT = 7 ; auto-increment index on data write

;; OBJ palette data
HW_OCPD = $FF6B

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HDMA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

HW_HDMA1 = $FF51
HW_HDMA2 = $FF52
HW_HDMA3 = $FF53
HW_HDMA4 = $FF54
HW_HDMA5 = $FF55

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Timer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Timer divider (Always counting at 64 mcycles, resets to $00 on write)
HW_DIV = $FF04

;; Timer counter
HW_TIMA = $FF05

;; Timer modulo (Reset value on interrupt)
HW_TMA = $FF06

;; Timer control
HW_TAC = $FF07
HW_TAC_MASK_RATE = %11
HW_TAC_BIT_ENABLE = 2

HW_TAC_RATE_256_MCYCLES = %00
HW_TAC_RATE_4_MCYCLES   = %01
HW_TAC_RATE_16_MCYCLES  = %10
HW_TAC_RATE_64_MCYCLES  = %11

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Serial and IR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Serial byte
HW_SB = $FF01

;; Serial control
HW_SC = $FF02
HW_SC_BIT_CLOCK_MASTER   = 0
HW_SC_BIT_CLOCK_HI_SPEED = 1
HW_SC_BIT_TX_ENABLE      = 7

;; IR port
HW_RP = $FF56

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Audio
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CH1 sweep
HW_NR10 = $FF10

;; CH1 length and duty cycle
HW_NR11 = $FF11

;; CH1 volume and envelope
HW_NR12 = $FF12

;; CH1 period lo
HW_NR13 = $FF13

;; CH1 period hi and control
HW_NR14 = $FF14
HW_NR14_BIT_LENGTH_ENABLE = 6
HW_NR14_BIT_TRIGGER       = 7

;; CH2 length and duty cycle
HW_NR21 = $FF21

;; CH2 volume and envelope
HW_NR22 = $FF22

;; CH2 period lo
HW_NR23 = $FF23

;; CH2 period hi and control
HW_NR24 = $FF24

;; CH3 DAC enable
HW_NR30 = $FF1A

;; CH3 length
HW_NR31 = $FF1B

;; CH3 output level
HW_NR32 = $FF1C

;; CH3 period lo
HW_NR33 = $FF1D

;; CH3 period hi and control
HW_NR34 = $FF1E

;; CH4 length
HW_NR41 = $FF20

;; CH4 volume and envelope
HW_NR42 = $FF21

;; CH4 frequency and randomness
HW_NR43 = $FF22

;; CH4 control
HW_NR44 = $FF23

;; Audio master volume
HW_NR50 = $FF24

;; Audio master panning
HW_NR51 = $FF25

;; Audio master control
HW_NR52 = $FF26
HW_NR52_BIT_CH1_ACTIVE    = 0 ; Active means the channel is playing audio
HW_NR52_BIT_CH2_ACTIVE    = 1
HW_NR52_BIT_CH3_ACTIVE    = 2
HW_NR52_BIT_CH4_ACTIVE    = 3
HW_NR52_BIT_MASTER_ENABLE = 7

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Joypad
HW_P1 = $FF00
HW_P1_BIT_GET_DIRECTION = 4
HW_P1_BIT_GET_ACTION    = 5

;; CPU speed switcher
HW_KEY1 = $FF4D
HW_KEY1_BIT_PREPARE_SWITCH = 0
HW_KEY1_BIT_CURRENT_SPEED  = 7

;; VRAM bank
HW_VBK = $FF4F

;; WRAMX bank
HW_SVBK = $FF70

;; Interrupt flags/enable
HW_IF = $FF0F
HW_IE = $FFFF
HW_IF_BIT_VBLANK = 0
HW_IF_BIT_LCD    = 1
HW_IF_BIT_TIMER  = 2
HW_IF_BIT_SERIAL = 3
HW_IF_BIT_JOY    = 4

?end ; HW_INC
