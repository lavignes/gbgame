; vim: ft=basm
?section "HOME"

;; copy bc bytes from de to hl
MemCopy::
    ld a, c
    or a, b
    ret z
    ld a, [de]
    ldi [hl], a
    inc de
    dec bc
    jr MemCopy

;; set bc bytes to 0 starting at hl
MemZero::
    xor a, a ; fallthru

;; set bc bytes to a starting at hl
MemSet::
    ld e, a
    ld a, c
    or a, b
    ret z
    ld a, e
    ldi [hl], a
    dec bc
    jr MemSet
