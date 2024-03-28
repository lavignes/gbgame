; vim: ft=basm
?section "HOME"

?include "hardware.inc"

?export Start
Start:
    jr Start

StartDoubleSpeedMode:
    ret
