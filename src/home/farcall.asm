; vim: ft=basm
?section "HOME"

?include "hardware.inc"

;; call function at e:hl
;
; NOTE only use this to call functions that dont pass
; args via registers or stack
FarCallHL::
    ldh a, [romBank]
    ; is that bank already active?
    ; TODO should I instead just throw an error?
    cp a, e
    jr nz, .SwitchBanks
    jp hl
.SwitchBanks:
    push af ; push return bank
    ld a, e
    ; switch banks
    ldh [romBank], a
    ld [HW_MAP_MBC5_BANK_LO], a
    jp hl
    ; restore bank and return
    pop af
    ld [romBank], a
    ld [HW_MAP_MBC5_BANK_LO], a
    ret
