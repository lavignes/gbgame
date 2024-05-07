; vim: ft=basm
?section "VECTORS"

?macro PAD
?for __, 0, \1 - 1
    nop
?end
?end

; jump to address offset by a in table follwing the call
;
; modifies a, hl
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

; short-hand for calling FarCallHL
PAD $10 - *
RstFarCall::
    jp FarCallHL

PAD $40 - *
IntVBlank:
    reti

PAD $48 - *
IntLcd:
    reti

PAD $50 - *
IntTimer:
    reti

PAD $58 - *
IntSerial:
    reti

PAD $60 - *
IntJoypad:
    reti
