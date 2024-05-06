; vim: ft=basm
?section "HOME"

; call function at a:hl
;
; modifies a, de, hl
FarCallHL::
    jr *
