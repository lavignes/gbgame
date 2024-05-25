; vim: ft=basm
?ifndef ENTITY_INC
ENTITY_INC = 1

?macro _ENTITY_COMMON
    .TYPE 1
    .FLAGS 1
    .Y 2
    .X 2
?end

?struct ENTITY
    _ENTITY_COMMON
?end ; ENTITY

?end ; ENTITY_INC
