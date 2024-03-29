; vim: ft=basm
?section "HRAM"

HRAM:

.ROMBankLo::    ?res 1

.JoyPressed::   ?res 1
.JoyHeld::      ?res 1
.JoyReleased::  ?res 1

.Scratch::      ?res 4
