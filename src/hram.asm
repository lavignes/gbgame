; vim: ft=basm
?section "HRAM"

;; holds the dma transfer routine
dmaFunction:: ?res 10

;; scratch space
tmp:: ?res 16

;; vblank ocurred flag
vBlanked:: ?res 1

romBank::  ?res 1

joyPressed::   ?res 1
joyHeld::      ?res 1
joyReleased::  ?res 1

