; vim: ft=basm
?section "WRAM0"

?include "hardware.inc"

?if (* % $FF) != 0
    ?fail "oamBuf must be page-aligned"
?end
;; staging buffer for oam updates
oamBuf:: ?res HW_MAP_OAM_SIZE

stackTop:: ?res 256
stackBase::
