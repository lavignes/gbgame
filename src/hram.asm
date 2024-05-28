; vim: ft=basm
?section "HRAM"

;; scratch space
tmp:: ?res 16

;; saved rom bank
romBank::  ?res 1

joyPressed::   ?res 1
joyHeld::      ?res 1
joyReleased::  ?res 1

