; vim: ft=basm
?section "GBC_HEADER"

ROM_HEADER_GBC_ONLY = $C0
ROM_HEADER_MBC5_RAM_BAT = $1B
ROM_HEADER_ROM_SIZE_4096KB = $07
ROM_HEADER_SRAM_SIZE_128KB = $04

; jump to entry point
nop
jp Start

?data8 $CE, $ED, $66, $66, $CC, $0D, $00, $0B
?data8 $03, $73, $00, $83, $00, $0C, $00, $0D
?data8 $00, $08, $11, $1F, $88, $89, $00, $0E
?data8 $DC, $CC, $6E, $E6, $DD, $DD, $D9, $99
?data8 $BB, $BB, $67, $63, $6E, $0E, $EC, $CC
?data8 $DD, $DC, $99, $9F, $BB, $B9, $33, $3E

?data8 "game       "
?data8 "basm"

?data8  ROM_HEADER_GBC_ONLY
?data16 $0000                       ; license code
?data8  $00                         ; no sgb
?data8  ROM_HEADER_MBC5_RAM_BAT
?data8  ROM_HEADER_ROM_SIZE_4096KB
?data8  ROM_HEADER_SRAM_SIZE_128KB
?data8  $01                         ; international
