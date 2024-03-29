; vim: ft=basm
?section "HOME"

?include "hardware.inc"

Start::
    jr Start

StartDoubleSpeedMode:
    ret
