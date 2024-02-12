
;Increments pointer to rambank by 'value'
.macro IncBankpointerBy value
    phy
        ldy #value
        :
            jsr IncBankPointer
            dey
            cpy #0
            bne :-
            
    
    ply
.endmacro

;Decrement 'addr' by one if > 0
.macro DecUntilZero addr
    pha
        lda addr
        cmp #0
        beq :+
        dec addr  
    :
    pla
.endmacro


;Copy rambank 'source' to rambank 'dest' (refactor to subroutine, to preserve space)
.macro CopyMemoryBank Source,Dest
    .scope
        lda ZP_RAMBANK
        pha
            lda #<RAMBANK
            sta ZP_PTR
            lda #>RAMBANK
            sta ZP_PTR+1
            
            @lp:
                lda #Source
                sta ZP_RAMBANK
                lda (ZP_PTR)
                pha
                    lda #Dest
                    sta ZP_RAMBANK
                pla
                sta (ZP_PTR)
                jsr IncBankPointer
                
                lda ZP_PTR
                cmp #$00
                bne @lp
                lda ZP_PTR+1
                ;jsr print_hex
                cmp #$b0
                bne @lp
                
        pla
        sta ZP_RAMBANK
    
    .endscope
    
.endmacro

;Check if countdown of button is 0 which means trigger a new press
.macro HandleJoyPress JOY_COUNTDOWN,JOY_PRESSED
 
    lda JOY_COUNTDOWN
    cmp #0
    bne :+   ;countdown busy, no new trigger

        lda #1
        sta JOY_PRESSED
        lda #JOY_PRESS_DELAY
        sta JOY_COUNTDOWN ; ticks, no new trigger 
    :
.endmacro


;Count number of bytes until a $0 byte is found and store it in paramcount
.macro CountUntilZero addr,paramcount
    phy
    pha
        ldy #0
        :
            lda addr,y
            iny
            cmp #0
            bne :-
        dey
        sty paramcount
    
    pla
    ply
    
.endmacro

;Print debug info to cmd window
.macro PrintUntilZero addr
    pha
    phy
        ldy #0
        :
            lda addr,Y
            cmp #0
            beq :+
            jsr CHROUT
           ; sta DEBUG
            iny
            jmp :-
        :
    ply
    pla
.endmacro



;Copy sFrom to aTo until a $0-byte is found. Store length in aStoreCnt if supplied
.macro CopyAddrUntilZero aFrom,aTo,aStoreCnt
   lda #<aFrom
   sta ZP_COPY_S
   lda #>aFrom
   sta ZP_COPY_S+1
   
   lda #<aTo
   sta ZP_COPY_D
   lda #>aTo
   sta ZP_COPY_D+1
   
   phy
   pha   
      ldy #0
      :  ;next
      lda (ZP_COPY_S),y
      sta (ZP_COPY_D),y
      iny
      cmp #0
      beq :+   ;done
      cpy #$FF    ;max 255 bytes
      beq :+   ;done
      bra :-   ;next
   :  ;done
     .if     .paramcount = 3
        sty aStoreCnt ;bytes copied, only if third parameter is given
      .endif
   pla
   ply

.endmacro


;Print a 'cnt' bytes of string 'addr' by writing directly to vram
.macro PrintCharVera addr,cnt
    phy
        ldy #0

       : ;NextChar
            lda (addr)
            jsr ConvertPetsciiToVera

            STA $9F23
            jsr IncBankPointer
            iny
            
            cpy #cnt
            beq :+      ;ToDone
        
            bra :-      ;To NextChar
        : ;Done
    ply  
.endmacro


.macro CopyFileInfoIntoVar addrTo,xcnt
    phy
        ldy #0
        :
            lda (ZP_PTR)
            sta addrTo,y
            iny
            jsr IncBankPointer
            cpy #xcnt
            bne :-
    
    ply

.endmacro

.macro CopyMem addrFrom,addrTo,xcnt
    phy
        ldy #0
        :
            lda addrFrom,y
            sta addrTo,y
            iny
            cpy #xcnt
            bne :-
    
    ply

.endmacro