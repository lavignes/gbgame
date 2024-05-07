; vim: ft=basm
?section "HOME"

; copy bc bytes from de to hl
MemCopy::
    ld a, c
    or a, b
    ret z
    ldi a, [hl]
    ld [de], a
    inc de
    dec bc
    jr MemCopy

; set bc bytes to a starting at hl
MemSet::
    ld a, c
    or a, b
    ret z
    ldi [hl], a
    dec bc
    jr MemSet
