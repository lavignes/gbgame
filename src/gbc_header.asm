; vim: ft=basm
?section "GBC_HEADER"

?include "hardware.inc"

PostBoot:
    nop
    jp Start

HW_ROM_HEADER HW_ROM_HEADER_COMPAT_CGB_ONLY, HW_ROM_HEADER_MBC_RAM_BAT,    \
              HW_ROM_HEADER_ROM_SIZE_4096KB, HW_ROM_HEADER_SRAM_SIZE_128KB
