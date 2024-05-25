; vim: ft=basm
?include "entity.inc"

ENTITY_BUF_LEN  = 64
ENTITY_BUF_SIZE = ENTITY_BUF_LEN * ENTITY.SIZE

?section "WRAM0"

entityCount: ?res 1
entityBuf: ?res ENTITY_BUF_SIZE

?section "HOME"

EntitySpawn::
    ret
