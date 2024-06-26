syn clear
syn case ignore

syn match basmIdentifier "[a-z_\.][a-z0-9_\.]*"
syn match basmGlobalLabel "^[a-z_][a-z0-9_\.]*"
syn match basmLocalLabel "^\.[a-z_][a-z0-9_]*"

syn keyword basmRegister a b c d e h l af bc de hl sp
syn keyword basmConditions z c nc nz

syn match basmOperator display "\%(+\|-\|/\|*\|\^\|\~\|&\||\|!\|>\|<\|%\|=\)=\?"
syn match basmOperator display "&&\|||\|<<\|>>\|\~>"

syn keyword basmOpcode ld ldd ldi ldh push pop add adc sub sbc and or xor cp inc dec swap
syn keyword basmOpcode daa cpl ccf scf nop halt stop di ei rlca rla rrca rra rlc rl rrc rr
syn keyword basmOpcode sla sra srl bit set res jp jr call rst ret reti

syn match basmDirective "?byte"
syn match basmDirective "?word"
syn match basmDirective "?section"
syn match basmDirective "?include"
syn match basmDirective "?if"
syn match basmDirective "?ifdef"
syn match basmDirective "?ifndef"
syn match basmDirective "?end"
syn match basmDirective "?res"
syn match basmDirective "?macro"
syn match basmDirective "?tag"
syn match basmDirective "?len"
syn match basmDirective "?for"
syn match basmDirective "?fail"
syn match basmDirective "?struct"

syn match basmComment ";.*" contains=basmTodo
syn match basmDocComment ";;.*" contains=basmTodo
syn keyword basmTodo contained todo fixme xxx warning danger note notice bug
syn match basmEscape "\\."
syn region basmString start=+"+ end=+"+ contains=basmEscape

syn match basmNumber "[0-9][0-9_]*"
syn match basmNumber "\$[0-9a-fA-F_]\+"
syn match basmNumber "%[01_]\+"
syn match basmNumber "'[!-~]"

syn case match

hi def link basmComment       Comment
hi def link basmDocComment    SpecialComment
hi def link basmNumber        Number
hi def link basmString	      String
hi def link basmIdentifier    Identifier
hi def link basmOpcode        Keyword
hi def link basmEscape        SpecialChar
hi def link basmDirective     PreProc
hi def link basmGlobalLabel   Function
hi def link basmLocalLabel    Function
hi def link basmTodo          Todo

let b:current_syntax = "basm"
set ts=4
set sw=4
set et

