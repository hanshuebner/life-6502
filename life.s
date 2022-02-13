;;; -*- gas -*-

              jmp   start
tmpa:         .byte 0
savecarry:    .byte 0               ; bit 0 => carry from previous byte
inbyte:       .byte 0               ; byte offset to input
outbyte:      .byte 0               ; byte offset to output
row:          .byte 0
inrow:        .word 0
outrow:       .word 0
neighrow:     .word 0
generation:   .byte 0
screen_row:   .word 0

              .org  $1000
start:
              inc   generation
count_neighbors:
              jsr   setup_pointers
              lda   #24
              sta   row
              lda   #.LO neighbors
              sta   neighrow
              lda   #.HI neighbors
              sta   neighrow+1
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
              sta   (neighrow),y
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
              beq   finish_counting
              clc
              lda   inrow
              adc   #5
              sta   inrow
              clc
              lda   neighrow
              adc   #40
              sta   neighrow
              lda   neighrow+1
              adc   #0
              sta   neighrow+1
              jmp   countrow
finish_counting:
              ;; vertical wraparound handling
              clc
              lda   #0
              tay
copy_wrap:    lda   top_neighbors,y
              sta   wrap_bottom_neighbors,y
              lda   bottom_neighbors,y
              sta   wrap_top_neighbors,y
              iny
              cpy   #40
              bne   copy_wrap


              ;; calculate next generation
tick:
              jsr   setup_pointers
              ;; inrow -> pointer to input bytes (buf0 or buf1)
              ;; outrow -> pointer to output bytes (buf1 or buf0)
              ;; row -> row counter, counts from 24 down to 0
              lda   #24
              sta   row
              lda   #.LO wrap_top_neighbors
              sta   neighrow
              lda   #.HI wrap_top_neighbors
              sta   neighrow+1
              ;; neighrow -> pointer to start of neighbor row (north of current row)
              lda   #0
              tay
next_tick_byte:
              lda   #0
              sta   outbyte
              ;; y -> current input pointer
              tya
              pha
              lsr   a
              lsr   a
              lsr   a
              tay
              lda   (inrow),y
              sta   inbyte
              pla
              tay                   ; y -> pointer to input
              lda   inbyte
next_tick_bit: 
              clc
              lda   (neighrow),y       ; neighbor counter
              tax
              tya
              adc   #40
              tay
              txa
              adc   (neighrow),y
              tax
              tya
              adc   #40
              tay
              txa
              adc   (neighrow),y
              tax                   ; x -> neighbor count
              tya
              sec
              sbc   #80
              tay                   ; y -> readjusted input pointer
              lda   inbyte
              ror   a               ; carry -> cell status
              sta   inbyte
              php
              lda   outbyte
              lsr   a
              plp
              bcc   is_dead         ; carry clear -> dead cell
              dex                   ; subtract myself
              cpx   #3
              beq   lives
              cpx   #2
              bne   dies
lives:        ora   #$80
              jmp   dies
is_dead:      cpx   #3
              beq   lives
dies:         sta   outbyte
              tya
              and   #7
              cmp   #7
              beq   end_of_tick_byte
              iny
              jmp   next_tick_bit
end_of_tick_byte: 
              ;; End of byte, get byte number into X and Y
              tya
              pha                   ; save Y
              lsr   a
              lsr   a
              lsr   a
              tax
              tay
              ;; Byte number in X and Y, store byte result using Y
              lda   outbyte
              sta   (outrow),y
              pla                   ; restore Y
              tay
              iny
              ;; Check for end of row using X
              txa
              cmp   #4
              beq   next_tick_row
              jmp   next_tick_byte
next_tick_row:
              dec   row
              beq   tick_done
              ;; increment input pointer by 5
              clc
              lda   inrow
              adc   #5
              sta   inrow
              lda   inrow+1
              adc   #0
              sta   inrow+1
              ;; increment output pointer by 5
              clc
              lda   outrow
              adc   #5
              sta   outrow
              lda   outrow+1
              adc   #0
              sta   outrow+1
              ;; increment neighbor pointer by 40
              clc
              lda   neighrow
              adc   #40
              sta   neighrow
              lda   neighrow+1
              adc   #0
              sta   neighrow+1
              lda   #0
              tay
              jmp   next_tick_byte
tick_done:    
end:          brk

setup_pointers:
              lda   generation
              ror   a
              bcs   odd_gen
even_gen:     lda   #.LO buf1
              sta   inrow
              lda   #.HI buf1
              sta   inrow+1
              lda   #.LO buf0
              sta   outrow
              lda   #.HI buf0
              sta   outrow+1
              rts
odd_gen:      lda   #.LO buf0
              sta   inrow
              lda   #.HI buf0
              sta   inrow+1
              lda   #.LO buf1
              sta   outrow
              lda   #.HI buf1
              sta   outrow+1
              rts

              .org  $2000
buf0:         .fill 20 $55
              .fill 20 $aa
              .fill 20 $ff
              .fill 20 $00
              .fill 20 $01
              .fill 20 $10
buf1:         .fill 120 0
              .org  $2400
              ;; Our neighbors array contains 26 rows so that when
              ;; calculating neighbors, we don't have to special case
              ;; for rows 0 and 23
wrap_top_neighbors:
              .fill 40 3
neighbors:
top_neighbors: 
              .fill 920 3
bottom_neighbors:
              .fill 40 3
wrap_bottom_neighbors:
              .fill 40 3
screen_row_starts:
              .word $0400
              .word $0480
              .word $0500
              .word $0580
              .word $0600
              .word $0680
              .word $0700
              .word $0780
              .word $0428
              .word $04A8
              .word $0528
              .word $05A8
              .word $0628
              .word $06A8
              .word $0728
              .word $07A8
              .word $0450
              .word $04D0
              .word $0550
              .word $05D0
              .word $0650
              .word $06D0
              .word $0750
              .word $07D0
