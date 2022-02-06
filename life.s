;; -*- gas -*-
              jmp   start
tmp:          .byte 0
savecarry:    .byte 1
inbyte:       .byte 0
outbyte:      .byte 0
              .align 2
outrow:       .word neighbors
inrow:        .word buf0
;; -----------------------------------------------------------------------------
              .org  $1000
start:
countrow:                           ; count neighbors across one screen row
                                    ; inrow => pointer to row of 5 bytes
              lda   #0              ; initialize input and output index
              sta   inbyte
              sta   outbyte
              ldy   #4              ; get carry from last byte in row
              lda   (inrow),y
              rol   a
              rol   a
              sta   savecarry
next_byte:    ldy   inbyte          ; get next byte, exiting if at end of row
              cpy   #5
              beq   end
              lda   (inrow),y
              pha
              iny
              sty   inbyte
              pla
next_bit:     pha
              lda   outbyte
              tay
              lda   savecarry
              ror   a               ; carry from previous byte
              lda   #0
              tax                   ; x is our neighbor counter
              pla
              rol   a
              ror   a
              bcc   chkbit1
              inx
chkbit1:      ror   a
              bcc   chkbit2
              inx
chkbit2:      ror   a
              bcc   store
              inx
store:        rol   a
              rol   a
              sta   savecarry
              ror   a
              sta   tmp
              txa
              sta   (outrow),y
              iny
              tya
              sta   outbyte
              and   #7
              tay
              cpy   #7
              bne   check_end
;;; NYI get first bit from next byte
;;; NYI if at last byte, get from first in row
              lda   tmp
              bne   next_bit
check_end:    cpy   #0
              beq   next_byte
              lda   tmp
              bne   next_bit
end:          brk

              .org  $2000
buf0:         .fill 120 $55
buf1:         .fill 120 255
              .org  $2400
neighbors:
              .fill 960 0
