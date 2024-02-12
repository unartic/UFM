
print_hex:
   pha
      pha	   ; push original A to stack
         lsr
         lsr
         lsr
         lsr      ; A = A >> 4
         jsr print_hex_digit
      pla      ; pull original A back from stack
      and #$0F ; A = A & 0b00001111
      jsr print_hex_digit
   pla
rts

print_hex_digit:
   cmp #$0A
   bpl @letter
   ora #$30    ; PETSCII numbers: 1=$31, 2=$32, etc.
   bra @print
@letter:
   clc
   adc #$37		; PETSCII letters: A=$41, B=$42, etc.
@print:
   sta DEBUG
   ;jsr CHROUT
   rts
   

print_hex_debug:
   pha
      pha	   ; push original A to stack
         lsr
         lsr
         lsr
         lsr      ; A = A >> 4
         jsr print_hex_digit_debug
      pla      ; pull original A back from stack
      and #$0F ; A = A & 0b00001111
      jsr print_hex_digit_debug
   pla
rts

print_hex_digit_debug:
   cmp #$0A
   bpl @letter
   ora #$30    ; PETSCII numbers: 1=$31, 2=$32, etc.
   bra @print
@letter:
   clc
   adc #$37		; PETSCII letters: A=$41, B=$42, etc.
@print:
   ;jsr CHROUT
   sta DEBUG
rts
   


;debug function
WaitKey:
    pha
    phx
    phy
        :
          
            jsr GETIN
            cmp #0
            beq :-

    
        lda #$07    ;beep
        jsr CHROUT
        :
          
            jsr GETIN
            cmp #0
            beq :-
    ply
    plx
    pla
rts

   
hlt:   jmp hlt