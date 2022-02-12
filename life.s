;;; -*- gas -*-

              jmp   start
tmpa:         .byte 0
savecarry:    .byte 0               ; bit 0 => carry from previous byte
inbyte:       .byte 0               ; byte offset to input
outbyte:      .byte 0               ; byte offset to output
row:          .byte 0
outrow:       .word 0
inrow:        .word buf0
              .org  $1000
start:
count_neighbors:
              lda   #24
              sta   row
              lda   #.LO neighbors
              sta   outrow
              lda   #.HI neighbors
              sta   outrow+1
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
              sta   savecarry
next_byte:    ldy   inbyte          ; get next byte, exit if at end of row
              cpy   #5
              beq   next_row
              lda   (inrow),y
              inc   inbyte
next_bit:     pha                   ; A contains current byte
              lda   #0
              tax                   ; initialize neighbor counter X
              ldy   outbyte         ; get output pointer to Y
              lda   savecarry       ; get carry
              ror   a
              pla
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
              sty   outbyte         ; store output counter
              inc   outbyte         ; point to next output byte
              tya                   ; current output counter
              and   #7              ; check for last bit of this byte...
              cmp   #6              ; ...as we need to pull first bit of next byte
              bne   check_end
;;; get first bit from next byte into carry
next_to_carry:
              ldy   inbyte          ; inbyte already pointing at next byte
              cpy   #5              ; at end of line?
              bne   get_next_lsb
              lda   #0              ; wraparound
              tay
get_next_lsb: lda   (inrow),y
              ror   a               ; bit 0 of next byte -> carry
              lda   tmpa
              and   #%11111101      ; mask bit 1
              bcc   next_bit
              ora   #%00000010      ; set bit 1
              jmp   next_bit
check_end:    cmp   #7
              beq   next_byte
              lda   inbyte          ; check for end of row
              cmp   #6              ; inbyte is already incremented
              beq   next_row
              lda   tmpa
              jmp   next_bit
next_row:     dec   row
              beq   end
              clc
              lda   inrow
              adc   #5
              sta   inrow
              clc
              lda   outrow
              adc   #40
              sta   outrow
              lda   outrow+1
              adc   #0
              sta   outrow+1
              jmp   countrow
end:          brk

              .org  $2000
buf0:         .fill 20 $55
              .fill 20 $aa
              .fill 20 $ff
              .fill 20 $00
              .fill 20 $01
              .fill 20 $10
buf1:         .fill 120 255
              .org  $2400
neighbors:
              .fill 960 0
