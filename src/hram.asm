; vim: ft=basm
?section "HRAM"

HRAM:

?export HRAM.ROMBankLo
.ROMBankLo:     ?res 1

?export HRAM.JoyPressed, HRAM.JoyHeld, HRAM.JoyReleased
.JoyPressed:    ?res 1
.JoyHeld:       ?res 1
.JoyReleased:   ?res 1

?export HRAM.Scratch
.Scratch:       ?res 4
