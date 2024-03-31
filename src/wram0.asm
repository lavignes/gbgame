; vim: ft=basm
?section "WRAM0"

; todo: 128 is probably enough tbh
; especially since i might add threads
wStackTop:: ?res 256
wStack::

