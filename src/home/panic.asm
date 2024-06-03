; vim: ft=basm
?include "hardware.inc"

?section "HOME"

;; Called by `RstPanic`
PanicFromRst::
    ld hl, .Msg
    ld bc, .MsgEnd - .Msg
    jr Panic
.Msg:
    ?byte "Executed an $FF!"
.MsgEnd:

;; Print `BC` bytes of `HL` to the serial port and halt
Panic::
    ; TODO Need to disable video, load font, disable interrupts,
    ; and re-enable video here
.SendByte:
    ; No more chars?
    ld a, c
    or a, b
    jr z, .Halt
.Wait:
    ; Are we still TX-ing?
    ld a, [HW_SC]
    bit HW_SC_BIT_TX_ENABLE, a
    jr nz, .Wait
    ; Send next char
    ldi a, [hl]
    dec bc
    ldh [HW_SB], a
    ld a, (1 << HW_SC_BIT_TX_ENABLE) |\
          (1 << HW_SC_BIT_CLOCK_HI_SPEED) |\
          (1 << HW_SC_BIT_CLOCK_MASTER)
    ldh [HW_SC], a
    jr .SendByte
.Halt:
    halt
    jr .Halt
