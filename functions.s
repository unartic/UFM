


clearscreen:
    pha
    lda #$93
    jsr CHROUT
    pla   
rts

print_lf:
    pha
      lda #LF
      jsr CHROUT
    pla
rts


;Converts a 2 byte hex value to displayable decimal: "#####0"
ConvertHex2Dec16Bitx:

	pha
	phx
	phy
	
		
		bra Display16BitAsDec_start
            RESULT_LENGTH = 11
            
           


		Display16BitAsDec_start:

		
		;result wordt van rechts naar links geschreven
		lda #RESULT_LENGTH
		sta resultindex_32bit

		;clear result
		ldx #0
		@loop_clear:
			lda #$20    ;put spaces in result
			sta result_32bit,x
			inx
			cpx #RESULT_LENGTH
			bne @loop_clear
		
        

	@divide:	
		;Initialize remainder to zero
		lda #0
		sta mod10_32bit
		sta mod10_32bit+1
		clc	
		ldx #16

	@divloop:
		;rotate
		rol value_32bit
		rol value_32bit+1

		rol mod10_32bit
		rol mod10_32bit+1

		
		; a,y = dividend - divisor 
		;mss moet dit stukje nog aangepast worden van 16 naar 32 bits
		sec 
		lda mod10_32bit
		sbc #10

		tay	;save low byte in y
		lda mod10_32bit+1
		sbc #0
		bcc @ignore_result
		
		sty mod10_32bit
		sta mod10_32bit+1
		
	@ignore_result:		;deelgetal past niet	
		dex
		bne @divloop
		rol value_32bit;shit in the last
		rol value_32bit+1

		
		lda mod10_32bit
		clc
		adc #$30
		
		ldx resultindex_32bit
		sta result_32bit,x
		dec resultindex_32bit
	
		lda value_32bit		;check of value en value+1 beide 0 zijn, dan klaar
		ora value_32bit+1
		bne @divide
	ply
	plx
	pla    
    
    

rts   




;Get user text input like INPUT function from basic.
GetInput:
    jmp GetInput_Start



    Input_Chars_Allowed: .asciiz "0123456789abcdefghijklmnopqrstuvwxyz.ABCDEFGHIJKLMNOPQRSTUVW"
ClearKeyboardBuffer:
    :
        jsr GETIN
        cmp #0
        bne :-

rts
GetInput_Start:
     ;reverse color
    jsr SetRambank63
    sec
    jsr PLOT
    
    phy
    phx
    dey
    clc
    jsr PLOT
    
    lda #$1F
    jsr CHROUT
    lda #$01
    jsr CHROUT
    lda #$05
    jsr CHROUT
    ldy #0
    @nxt:
        lda #$20
        jsr CHROUT
        iny
        cpy #15 ;InputMaxLength
        bne @nxt
    lda #$20
    jsr CHROUT    
    plx
    ply
    clc
    jsr PLOT


    stz InputLength
    @input_loop:
        phy
        jsr GETIN   
        ply
        cmp #0
        beq @input_loop
        
        cmp #RETURN
        beq @Ready
        cmp #$14    ;backspace
        beq @Backspace
        cmp #$1B     ;esc
        beq @EscPress
        
       
        ldy InputLength
        cpy #15 ;InputMaxLength
        beq @input_loop

        ;TODO: have to extend this to allow for ISO-chars
       ; jsr CheckAForAllowedChar    ;changes A to 0 if not ok
       ; cmp #0 
       ;  beq @input_loop
         
        jsr ReverseCursor
        sta InputBuffer,y
        jsr CHROUT
        lda #95
        jsr CHROUTbin
 
        inc InputLength
        jmp @input_loop
        
        @Backspace:
            tax
                lda InputLength
                cmp #0
                beq @input_loop
                jsr ReverseCursor
                jsr ReverseCursor
            tax             
            lda #95
            jsr CHROUTbin        
            lda #$20
            jsr CHROUT    
            dec InputLength
            jsr ReverseCursor
        jmp @input_loop
        
        @EscPress:
        stz InputLength
        jmp @Ready   
        
        
    jmp @input_loop
        
    @Ready:
    iny
    lda #0
    sta InputBuffer,y
    
    ;reverse color off
    lda #$92
    jsr CHROUT
    
    jsr RestoreRambank
rts

;Helper function for GetInput to check if the current char in A is in string Input_Chars_Allowed
CheckAForAllowedChar:

    phy
    phx
        ldy #0
        :
            
            ldx Input_Chars_Allowed,y
            iny
            cpx #0
            beq @NotFound
            stx TMP
            cmp TMP
            bne :-
            
        jmp @Continue
            
        @NotFound:
            lda #0
        @Continue:
    plx
    ply

rts

;Helper function got GetInput: move cursor one char to the left
ReverseCursor:
    phy
    pha
        sec
        jsr PLOT
        dey
        clc
        jsr PLOT
    pla
    ply

rts





;Checks if a given file existsm by listing files with the filename as filter and couting the line feeds of the result
FileExists:
    lda #1   
    ldx DEVICE_OTHER ; #8 
    ldy #0  

  

    jsr SETLFS 
     ldx #0
   @CountNext:
      lda CMD_BUFFER,x
      cmp #0
      beq @CountReady
      inx
   jmp @CountNext  
   @CountReady:    
   
    txa 
    
    ldx #<CMD_BUFFER
    ldy #>CMD_BUFFER      
    jsr SETNAM

    jsr OPEN
    jsr READST


    ldx #1
    ldy #0
    jsr CHKIN        
        @next:
            jsr READST
            bne @ReadDone
            jsr CHRIN

            cmp #$22 ;line feed
            bne @next
            
            iny
            

            jmp @next
        
        
    @ReadDone:

    tya

    jsr CLRCHN


    cpy #4
    beq @FileExists
    cpy #$0e ;somehow count for sd card dir???
    beq @FileExists
        lda #0
        sta CHECKFILE_RESULT       
    jmp @Done
    @FileExists:
        lda #1
        sta CHECKFILE_RESULT
    @Done:
    jsr CLRCHN
    lda #1
    jsr CLOSE
    
    
rts

;Switch active device to device used in the current window
SwitchToCurrentDevice:
    pha
        lda DEVICE_CURRENT
        sta $03fe
    pla
rts

;Switch active device to device used in the other window then the active window
SwitchToOtherDevice:
    pha
        lda DEVICE_OTHER
        sta $03fe
        
        lda PARTITION_OTHER
        cmp #0
        beq @NoPartitionSwitch
        jsr SelectOtherPartition
        
        @NoPartitionSwitch:
    pla
rts

;Set text CHROUT colors
SetColor:
    ;x=forground
    ;y=background
    tya
    jsr CHROUT
    lda #01
    jsr CHROUT
    txa
    jsr CHROUT
    
rts


;Backup current rambank
SaveRambank:
    lda $00
    sta RAMBANK_BACKUP
rts

;Restore backed up rambank
RestoreRambank:
    lda RAMBANK_BACKUP
    sta $00
rts

;Set rambank to #63, on which several variables are stores
SetRambank63:
    jsr SaveRambank
    lda #63
    sta $00
rts

;Set rambank to #63, on which several variables are stores
SetRambank62:
    jsr SaveRambank
    lda #62
    sta $00
rts


;Account for lower uppercase chars to be printed in iso-mode
ConvertISOToPetcii:
        cmp #$41
        bcc NoChange
        cmp #$61
        bcs NoChange
        clc
        adc #$20
        
        NoChange:
        cmp #190
        bcc NoChange2
            sec 
            sbc #$80
         NoChange2:
rts

CHROUT:
    jsr ConvertISOToPetcii
    jsr jsrfar
    .word $ffd2
    .byt 0
rts


;CHROUT without char conversion to handle filenames 1-to-1
CHROUTbin:
    jsr jsrfar
    .word $ffd2
    .byt 0
rts





ConvertAToAsciiHex:
   pha
      pha	   ; push original A to stack
         lsr
         lsr
         lsr
         lsr      ; A = A >> 4
         jsr ConverHexToDigit
         sta LaunchAddressAscii
      pla      ; pull original A back from stack
      and #$0F ; A = A & 0b00001111
      jsr ConverHexToDigit
      sta LaunchAddressAscii+1
   pla   

rts

ConverHexToDigit:
   cmp #$0A
   bpl @letter
   ora #$30    ; PETSCII numbers: 1=$31, 2=$32, etc.
   bra @done
@letter:
   clc
   adc #$37		; PETSCII letters: A=$41, B=$42, etc.
@done:

rts

