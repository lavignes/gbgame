; vim: ft=basm
?section "HOME"

; call function at e:hl
FarCallHL::
    ld a, romBank
    jr *
