; vim: ft=basm
?include "hardware.inc"

?section "HOME"

?export Start
Start:
    jr Start

StartDoubleSpeedMode:
    ret
