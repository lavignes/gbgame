; vim: ft=basm
?section "VECTORS"

?macro ALIGN
?for __, 0, \1 - *
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
    jr nc, .nocarry
.nocarry:
    ldi a, [hl]
    ld h, [hl]
    ld l, a
    jp hl

; short-hand for calling FarCallHL
ALIGN $10
RstFarCall::
    jp FarCallHL

ALIGN $40
IntVBlank:
    reti

ALIGN $48
IntLcd:
    reti

ALIGN $50
IntTimer:
    reti

ALIGN $58
IntSerial:
    reti

ALIGN $60
IntJoypad:
    reti
