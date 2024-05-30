; vim: ft=basm
?include "entity.inc"

ENTITY_BUF_LEN  = 64
ENTITY_BUF_SIZE = ENTITY.SIZE * ENTITY_BUF_LEN

?section "WRAM0"

entityBuf: ?res ENTITY_BUF_SIZE
entityCount: ?res 1

?section "HOME"

EntitySpawn::
    ret
