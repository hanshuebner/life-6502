 jmp start
tmp:
 .byte 0
savecarry:
 .byte 1
inbyte:
 .byte 0
 .align 2
outrow:
 .word neighbors
inrow:
 .word buf0

.org $1000
start:
countrow:
; count neighbors across one screen row
; inrow => pointer to row of 5 bytes
; initialize input index
 lda #0
 sta inbyte
; get carry from last byte in row
 ldy #4
 lda (inrow),y
 rol
 rol
 sta savecarry
next_byte:
 ; get next byte, exiting if at end of row
 ldy inbyte
 cpy #5
 beq end
 lda (inrow),y
 pha
 iny
 sty inbyte
 lda #0
 tay
 pla
next_bit:
 sta tmp
 lda savecarry
 ror        ; carry from previous byte
 lda #0
 tax        ; x is our neighbor counter
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
 rol
 sta savecarry
 ror
 sta tmp
 txa
 sta (outrow),y
 iny
 cpy #7
bne check_end
 ; NYI get first bit from next byte
 ; NYI if at last byte, get from first in row
 lda tmp
 bne next_bit
check_end:
 cpy #8
 beq next_byte
 lda tmp
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
