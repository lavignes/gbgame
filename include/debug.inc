; vim: ft=basm
?ifndef DEBUG_INC
DEBUG_INC = 1

?macro PANIC
?if \# == 0
    ld bc, 0
?end
?if \# > 0
    ld hl, \j .PanicMsg, \u
    ld bc, \j .PanicMsgEnd, \u - \j .PanicMsg, \u
?end
    jp StartPanic
?if \# > 0
\j .PanicMsg, \u:
    ?byte \1
\j .PanicMsgEnd, \u:
?end
?end ; PANIC

?macro PANIC_EQ
    push af
    ld a, \1
    cp a, \2
    jr nz, \j .Ok, \u
    PANIC \3
\j .Ok, \u:
    pop af
?end ; PANIC_EQ

?end ; DEBUG_INC
