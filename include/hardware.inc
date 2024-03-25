; vim: ft=basm
?ifndef HW_INC
HW_INC = 1

HW_ROM_HEADER_COMPAT_CGB_ONLY = $C0
HW_ROM_HEADER_MBC_RAM_BAT = $1B
HW_ROM_HEADER_ROM_SIZE_4096KB = $07
HW_ROM_HEADER_SRAM_SIZE_128KB = $04

?macro HW_ROM_HEADER
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; IO Ports
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

HW_IO_P1 =      $FF00
HW_IO_SB =      $FF01
HW_IO_SC =      $FF02
HW_IO_DIV =     $FF03
HW_IO_TIMA =    $FF04
HW_IO_TMA =     $FF05
HW_IO_TAC =     $FF06

HW_IO_KEY1 =    $FF4D
HW_IO_RP =      $FF56

HW_IO_VBK =     $FF4F
HW_IO_SVBK =    $FF70

HW_IO_IF =      $FF0F
HW_IO_IE =      $FFFF

HW_IO_LCDC =    $FF40
HW_IO_STAT =    $FF41
HW_IO_SCY =     $FF42
HW_IO_SCX =     $FF43
HW_IO_LY =      $FF44
HW_IO_LYC =     $FF45
HW_IO_DMA =     $FF46
HW_IO_BGP =     $FF47
HW_IO_OBP0 =    $FF48
HW_IO_OBP1 =    $FF49
HW_IO_WY =      $FF4A
HW_IO_WX =      $FF4B

HW_IO_HDMA1 =   $FF51
HW_IO_HDMA2 =   $FF52
HW_IO_HDMA3 =   $FF53
HW_IO_HDMA4 =   $FF54
HW_IO_HDMA5 =   $FF55

HW_IO_BCPS =    $FF68
HW_IO_BCPD =    $FF69
HW_IO_OCPS =    $FF6A
HW_IO_OCPD =    $FF6B

?end ; HW_INC
