; vim: ft=basm
?ifndef DEBUG_INC
DEBUG_INC = 1

?macro PANIC
    ?ifdef DEBUG
        ?if \# == 0
            ld bc, 0
        ?end
        ?if \# > 0
            ld hl, \j .PanicMsg, \u
            ld bc, \j .PanicMsgEnd, \u - \j .PanicMsg, \u
        ?end
        jp Panic
        ?if \# > 0
        \j .PanicMsg, \u:
            ?byte \1
        \j .PanicMsgEnd, \u:
        ?end
    ?end
?end ; PANIC

?macro PANIC_A_EQ
    ?ifdef DEBUG
        push af
        ld a, \1
        cp a, \2
        jr nz, \j .Ok, \u
        ?if \# > 2
            PANIC \3
        ?end
        ?if \# == 2
            PANIC
        ?end
    \j .Ok, \u:
        pop af
    ?end
?end ; PANIC_A_EQ

?end ; DEBUG_INC
