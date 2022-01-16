 jmp start
tmp:
 .byte 0
 .align 2
outrow:
 .word neighbors

; count neighbors across one screen row
.org $1000
start:
 lda #0
 tay
 lda buf0
next_bit:
 sta tmp
 lda #0
 tax
 lda tmp
 rol
 ror
 bcc chkbit1
 inx
chkbit1:
 ror
 bcc chkbit2
 inx
chkbit2:
 ror
 bcc store
 inx
store:
 rol
 sta tmp
 txa
 sta (outrow),y
 lda tmp
 iny
 cpy #8
 bne next_bit
end:
 brk

.org $2000
buf0:
 .fill 120 255
buf1:
 .fill 120 255
.org $2400
neighbors:
 .fill 960 0
