; vim: ft=basm
?ifndef DEBUG_INC
DEBUG_INC = 1

?macro PANIC
?if \# == 0
    ld bc, 0
?end
?if \# > 0
    ld hl, \j .PanicMessage, \u
    ld bc, \j .PanicMessageEnd, \u
?end
    jp StartPanic
?if \# > 0
\j .PanicMessage, \u:
    ?byte \1
\j .PanicMessageEnd, \u:
?end
?end ; PANIC


?end ; DEBUG_INC
