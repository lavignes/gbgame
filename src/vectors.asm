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
        ?fail "overflowed!"
    ?end
?end ; PAD_TO

;; jump to address offset by a in table follwing the call
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

;; short-hand for calling FarCallHL
PAD_TO $10
RstFarCall::
    jp FarCallHL

PAD_TO $18
RstJpHL::
    jp hl

PAD_TO $40
IntVBlank:
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
