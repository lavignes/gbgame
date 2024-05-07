; vim: ft=basm
?section "HOME"

; copy c bytes from de to hl
MemCopy::
    or a, c
    ret z
    ldi a, [hl]
    ld [de], a
    inc de
    dec c
    jr MemCopy

; set c bytes to a starting at hl
MemSet::
    or a, c
    ret z
    ldi [hl], a
    dec c
    jr MemSet
