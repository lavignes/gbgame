; vim: ft=basm
?section "HOME"

;; Copy `BC` bytes from `DE` to `HL`
MemCopy::
    ld a, c
    or a, b
    ret z
    ld a, [de]
    ldi [hl], a
    inc de
    dec bc
    jr MemCopy

;; Set `BC` bytes to `$00` starting at `HL`
MemZero::
    xor a, a ; fallthru

;; Set `BC` bytes to `A` starting at `HL`
MemSet::
    ld e, a
    ld a, c
    or a, b
    ret z
    ld a, e
    ldi [hl], a
    dec bc
    jr MemSet

