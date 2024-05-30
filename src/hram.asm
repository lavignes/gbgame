; vim: ft=basm
?section "HRAM"

;; scratch space
scratch:: ?res 16

;; saved ROMX bank
romBank::  ?res 1

joyPressed::   ?res 1
joyHeld::      ?res 1
joyReleased::  ?res 1

