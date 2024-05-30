; vim: ft=basm
?section "VECTORS"

; naive padding macro. the best way to really ensure this
; is to place each interrupt in a different section via the
; linker
?macro PAD_TO
    ?for \u, 0, \1 - *
        nop
    ?end
    ?if * != \1
        ?fail "Overflowed!"
    ?end
?end ; PAD_TO

;; Jump to address offset by `A` in a pointer table follwing the rst
RstJumpTable::
    add a, a  ; multiply by 2 since addrs are 16 bits
    pop hl    ; pop return address into hl
    add a, l
    ld l, a
    jr nc, .NoCarry
.NoCarry:
    ldi a, [hl]
    ld h, [hl]
    ld l, a
    jp hl

;; Short-hand for calling `FarCallHL`
PAD_TO $10
RstFarCall::
    jp FarCallHL

PAD_TO $18
RstJpHL::
    jp hl

; TODO space for more rst

; rst $38 is encoded as $FF, so accidental execution of
; $FF bytes will blow up :-)
PAD_TO $38
RstPanic::
    jp PanicFromRst

PAD_TO $40
IntVBlank:
    push af
    ld a, 1
    ldh [vBlanked], a
    pop af
    reti

PAD_TO $48
IntLcd:
    reti

PAD_TO $50
IntTimer:
    reti

PAD_TO $58
IntSerial:
    reti

PAD_TO $60
IntJoypad:
    reti
