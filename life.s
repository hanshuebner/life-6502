;;; -*- gas -*-

              jmp   start
tmpa:         .byte 0
savecarry:    .byte 0               ; bit 0 => carry from previous byte
inbyte:       .byte 0               ; byte offset to input
outbyte:      .byte 0               ; byte offset to output
              
              .align 2
outrow:       .word neighbors
inrow:        .word buf0
              .org  $1000
start:
;;; count neighbors across one screen row
;;; inrow => pointer to row of 5 bytes
countrow:
              lda   #0              ; initialize input and output index
              sta   inbyte
              sta   outbyte
              ldy   #4              ; get carry from last byte in row
              lda   (inrow),y
              rol   a
              rol   a
              and   #1
              sta   savecarry
next_byte:    ldy   inbyte          ; get next byte, exit if at end of row
              cpy   #5
              beq   end
              lda   (inrow),y
              inc   inbyte
next_bit:     pha                   ; A contains current byte
              lda   #0
              tax                   ; initialize neighbor counter X
              ldy   outbyte         ; get output pointer to Y
              lda   savecarry       ; get carry
              ror   a
              pla
              rol   a               ; count neighbors
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
              sta   tmpa
              txa                   ; store count
              sta   (outrow),y
              iny
              tya
              sta   outbyte         ; store incremented output counter
              and   #7              ; check for end of byte
              tay
              cpy   #7
              bne   check_end
;;; get first bit from next byte into carry
              ldy   inbyte          ; inbyte already pointing at next byte
              cpy   #5              ; at end of line?
              bne   get_next_lsb
              lda   #0              ; wraparound
              tay
get_next_lsb: lda   (inrow),y
              and   #1
              sta   savecarry
              lda   tmpa
              jmp   next_bit
check_end:    cpy   #0
              beq   next_byte
              lda   tmpa
              bne   next_bit
end:          brk

              .org  $2000
buf0:         .fill 120 $55
buf1:         .fill 120 255
              .org  $2400
neighbors:
              .fill 960 0
