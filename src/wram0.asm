; vim: ft=basm
?section "WRAM0"

NTHREADS = 4

?for THREAD, 0, NTHREADS
\j stackTop, THREAD:: ?res 256
\j stack, THREAD::
?end
