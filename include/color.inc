; vim: ft=basm
?ifndef COLOR_INC
COLOR_INC = 1

;; Approximate RGB color triple into 16-bits
?macro COLOR
    ?byte $FF & ((((\2) & $F8) << 2) | ((\1) & $F8) ~> 3)
    ?byte $FF & ((((\3) & $F8) ~> 1) | ((\2) & $F8) ~> 6)
?end

?end ; COLOR_INC
