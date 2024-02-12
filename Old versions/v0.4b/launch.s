.org $800D ; program gets loaded in memory at $8000
.segment "ONCE"
;was 2107b
;    1229b
.include "constants.s"
jmp start


File: .res 100,$EA
FileFM: .asciiz "fm.prg"

CMD: .res 255 ;command  ;use r0l and r0h, saves 255 bytes
cnt: .byte $0
BasicStart = $0801
FM_DEVICE: .byte 0
FM_PARTITION: .byte 0


ParamBuffer:

start:
    lda File
    cmp #0  ;goto FM
    beq goto_loadfm
    
  
    ;copy file into CMD-buffer
    ldy #0  ;first byte contains filetype B=basic,A=assembly
    @CopyByte:
      lda File+1,y
      sta CMD,y
      cmp #0
      beq @CopyDone
      iny
    bra @CopyByte
    @CopyDone:
      iny
      lda File+1,y
      sta FM_DEVICE
      lda File+2,y
      sta FM_PARTITION

      sty cnt ;file length

      
      lda File
      cmp #$41  ;A=assembly
      beq PRGFile
      
    
      ;load and start basic file
        jsr LoadBasicFileIntoMemory

        

        lda #0  ;make sure next time FM is  being loaded
        sta File    
          
        ;start basic file
        lda #$52
        jsr KEY_POKE
        lda #$55
        jsr KEY_POKE
        lda #$4E
        jsr KEY_POKE
        lda #$0D
        jsr KEY_POKE  

      rts      
      
      jmp toend
      PRGFile:
        ;load and start PRG file
        jsr LoadPRGFileIntoMemory
        jsr StartPRG
       
  
  toend:
rts

goto_loadfm:
  jmp LoadFM


LoadPRGFileIntoMemory:
    lda #1   ; Logical Number = 1
    ldx $03fe  ;#8   ; Device = "SD card" (emulation host FS)
    ldy #1   ; 0=skip header byts, 1=use header butes
    jsr SETLFS  

;  ldx #0
 ; :
  ;  lda CMD,X
    
    
  ;  jsr CHROUT
  ;  cmp #0
  ;  bne :-



    lda cnt
    ldx #<CMD
    ldy #>CMD
    jsr SETNAM
    lda #0  ;load, not verify
    jsr LOAD
    ;close not needed
    
    
rts

StartPRG:
  ;get start pointer from basic sys command
    
    ldy #0
    ldx #0  ;searxh of sys command
    @loop2:
        lda BasicStart,Y
        cmp #$9e        ;SYS command
        bne @NoActionChange
        ldx #1  ;search for first non-space and non 9e char=stat of address
        @NoActionChange:
        
        cpx #1
        bne @GotoNextChar
        cmp #$20  ;space overslaan
        beq @GotoNextChar
        cmp #$9e  ;SYS token overslaan
        beq @GotoNextChar
        jmp LoadNext4BytesAsAddress2
        
    
    @GotoNextChar:
        iny
    jmp @loop2 
    
    LoadNext4BytesAsAddress2:
        jsr DigiToHexInit
    
        ldx #0
        LoadNextCharAddress2:
            lda BasicStart,y
            AND #%00001111
            sta DigitToHexValue+1,x
            inx
            iny
            cpx #4  
            bne LoadNextCharAddress2
        @Ready: 
        jsr DigitToHex
     
    
     ;store start of program in 'BasicStart'
     lda Adder24bitValue    ;low byte
     sta ToJSR+1    ;modify JSR address
     
     lda Adder24bitValue+1    ;low byte
     sta ToJSR+2
     
     ;A871 basic run command   
     ;jmp DoBasic   
    ToJSR:
   ; jmp ToJSR
     jsr $0000 ;gets overwritte by real start ofprogram
     jsr LoadFM
rts

LastByte: .byte $00

Goto_toeof:
  jmp toeofx
  
LoadBasicFileIntoMemory:
  ;length of basic file is not correct. Does not seem to matter though.

    lda #1   ; Logical Number = 1
    ldx $03fe
    
    ;ldx #8   ; Device = "SD card" (emulation host FS)
    ldy #0   ; 0=skip header byts, 1=use header butes
    jsr SETLFS  

    lda cnt
    ldx #<CMD
    ldy #>CMD      
    jsr SETNAM
      
    jsr OPEN
    
    LDX #1
    jsr CHKIN
    
    ;load basic start address in pointer
    lda #$01
    sta ZP_PTR
    lda #$08
    sta ZP_PTR+1
    
    
    lda #1
    LDX #1
    jsr CHKIN 
     
    jsr CHRIN ;first 2 bytes contain memlocation, ignore it. WILL NOT WORK ALWAYS
    jsr CHRIN  
    NextLine:
      jsr READST
      bne Goto_toeof
      
      
      ;4 bytes containing offset next line + line number  ;always just copy
      jsr CHRIN
      sta (ZP_PTR)
      jsr IncBankPointer
      jsr READST
      bne Goto_toeof
      
      jsr CHRIN
      sta (ZP_PTR)
      jsr IncBankPointer
      jsr READST
      bne Goto_toeof
      
      jsr CHRIN
      sta (ZP_PTR)
      jsr IncBankPointer
      jsr READST
      bne Goto_toeof
      
      jsr CHRIN
      sta (ZP_PTR)
      jsr IncBankPointer

   NextByte:
      jsr READST
      bne Goto_toeof
      
      ;only when in a basic line!!!!
      jsr CHRIN
      cmp #$80    
      beq @InsertGotoSys      
      
      ;store in mem
      sta (ZP_PTR)
      jsr IncBankPointer
      cmp #0  ;next line
      beq NextLine
      bra NextByte 
      
      
      @InsertGotoSys:
           
          @ZeroAanwezig:
    
          ;write new command
          lda #$89  ;goto ;replacing #$80=end
          sta (ZP_PTR)
          jsr IncBankPointer  

          lda #$20  ;space
          sta (ZP_PTR)
          jsr IncBankPointer  

          lda #$36  ;address digit 1
          sta (ZP_PTR)
          jsr IncBankPointer  

          lda #$33  ;address digit 2
          sta (ZP_PTR)
          jsr IncBankPointer  

          lda #$39  ;address digit 3
          sta (ZP_PTR)
          jsr IncBankPointer  

          lda #$39  ;address digit 4
          sta (ZP_PTR)
          jsr IncBankPointer  

          lda #$39  ;address digit 5
          sta (ZP_PTR)
          jsr IncBankPointer  
          ;continue with rest
      
                 
      
    jmp NextByte  

    
    toeofx:
    jsr CLRCHN
    lda #1
    jsr CLOSE 
      
    
    ;add last line with SYS command
  ;  jmp skip
    ;reverse pointer to last byte before 00 00 00. The amount of zero's is not fixed it seems
    @nextzero:;    jsr DecBankPointer    
      jsr DecBankPointer   
      lda (ZP_PTR)
      cmp #0
      beq @nextzero
    jsr IncBankPointer  ;set pointer on last 00
    jsr IncBankPointer

  skip:
 ; jsr DecBankPointer
 ; jsr DecBankPointer

    ;last line is 13 bytes
    ldy #0
    @nxt:
        jsr IncBankPointer
        iny
        cpy #12
        bne @nxt

  
    ldx ZP_PTR
    ldy ZP_PTR+1

    ldy #0
    @nxt2:
        jsr DecBankPointer
        iny
        cpy #12
        bne @nxt2
    
    ;low byte next line
    txa
    sta (ZP_PTR)
    jsr IncBankPointer 
    tya
    sta (ZP_PTR)
    jsr IncBankPointer   
      
      
    ;add line number 63999
    lda #$FF
    sta (ZP_PTR)
    jsr IncBankPointer    

    lda #$F9
    sta (ZP_PTR)
    jsr IncBankPointer 
       
    lda #$9e  ;sys command
    sta (ZP_PTR)
    jsr IncBankPointer 
    ;32781
    lda #$20  ;space
    sta (ZP_PTR)
    jsr IncBankPointer 

 
    lda #$33  ;sys command
    sta (ZP_PTR)
    jsr IncBankPointer 
    
    lda #$32  ;sys command
    sta (ZP_PTR)
    jsr IncBankPointer 

    lda #$37  ;sys command
    sta (ZP_PTR)
    jsr IncBankPointer 

    lda #$38  ;sys command
    sta (ZP_PTR)
    jsr IncBankPointer 

    lda #$31  ;sys command
    sta (ZP_PTR)
    jsr IncBankPointer 

    ;end with 3 zero's
    lda #$0  ;sys command
    sta (ZP_PTR)
    jsr IncBankPointer 

    lda #$0  ;sys command
    sta (ZP_PTR)
    jsr IncBankPointer 

    lda #$0  ;sys command
    sta (ZP_PTR)
    jsr IncBankPointer 
  hlt:
   ; jmp hlt
    ;fix offseet addresses
    
    ;start at basic
    lda #$01
    sta ZP_PTR
    lda #$08
    sta ZP_PTR+1    
    
   
    ;jmp hlt ;do not fix loop

    @FixLoop:


      lda (ZP_PTR)    
      cmp #0  ;second zero
      bne @Continue
      
      ldy #1
      lda (ZP_PTR),y    
      cmp #0  ;third zero
      bne @Continue
      jmp @FixDone
      
      @Continue:
      ;store current address on the stack. This is where offset of next line will be
   
        lda ZP_PTR
        pha
        lda ZP_PTR+1  ;store 081d  0826: adress waar het nieuwe adres geschreven moet worden
        pha
          ;move pointer to beginning of basic line
          jsr IncBankPointer      
          jsr IncBankPointer      
          jsr IncBankPointer      
          jsr IncBankPointer          
          jsr GotoNextZero  ;goto address which is should be
          jsr IncBankPointer
    
        ldx ZP_PTR   ;store 0825/26 8040
        ldy ZP_PTR+1
        ;go back
        pla
        sta ZP_PTR+1
        pla
        sta ZP_PTR
        ;write new address
        txa
        sta (ZP_PTR)
        jsr IncBankPointer 
        tya
        sta (ZP_PTR)
        jsr IncBankPointer 
        
        ;move to start of next line
        jsr IncBankPointer      
        jsr IncBankPointer      
       
        jsr GotoNextZero
        jsr IncBankPointer 
      jmp @FixLoop
    @FixDone:   
    
    
    
    lda #0
    sta (ZP_PTR)    
    jsr IncBankPointer
  
   
    
rts

GotoNextZero:
  @lp:
    lda (ZP_PTR)
    cmp #0
    beq @Klaar
    jsr IncBankPointer 
  bra @lp
  @Klaar:
rts

ReversePointer:
  @lp:
    jsr DecBankPointer
    lda (ZP_PTR)
    cmp #0
    beq @DecreaseY
    jmp @lp
    
    @DecreaseY:
    dey
    cpy #0
    beq @Ready  ;allemaal gehad
  jmp @lp
  
  @Ready:
rts

GotoFMDirectory:
    ;file is al geladen geweest, dus deze mem loc kunnen we gewoongebruiken
    lda #01
    sta $00 ;switch to rambank 1

    ;switch to partition if needed
    lda FM_PARTITION
    cmp #0
    beq @NoChangeNeededHosftFS
    
    lda #$43
    sta CMD
    lda #$50
    sta CMD+1
    lda FM_PARTITION
    clc
    adc #$30
    sta CMD+2

        ldy #3  ;CP1
        ldx #<CMD
        ldy #>CMD
        jsr SETNAM
        

        lda #1   ; Logical Number = 1
        ldx $03fe   
        ldy #15   ;   15=control channel

        jsr SETLFS
        jsr OPEN
        lda #1
        jsr CLOSE
    
    rts
    
    @NoChangeNeededHosftFS:
    
    ;store current cursor position
    sec
    jsr PLOT
    phy
    phx   
        ;init vera 
        LDA #$00
        STA $9F25   ;VERA CTRL
        ;set address
        lda #$00
        STA $9F20   ;ADDRES_L
        lda #$F8
        STA $9F21   ;M
        lda #$11
        STA $9F22   ; 

        lda #<RAMBANK
        sta ZP_PTR
        lda #>RAMBANK
        sta ZP_PTR+1
        
        ldy #0
        
        ;first load CD:
        lda #$43       ; C
        sta (ZP_PTR),y
        iny        
        lda #$44       ; D
        sta (ZP_PTR),y
        iny        
        lda #$3A       ; :
        sta (ZP_PTR),y
        iny        


        ;then load path
        @nxt:
            lda $9F23
            cmp #0
            beq @Ready
            sta (ZP_PTR),y
       ;     sta CMD2,y
            iny
        jmp @nxt          
        @Ready:
        ;y contains length
       
        tya   ;y contains length, so tranfer it to A
        ldx #<RAMBANK
        ldy #>RAMBANK
        jsr SETNAM
        

        lda #1   ; Logical Number = 1
        ldx $03fe   
        ldy #15   ;   15=control channel

        jsr SETLFS

        jsr OPEN
        LDX #1
        jsr CHKIN  

        ldy #$00      ;PREPARE THE Y REGISTER TO STORE THE DATA
        @RD:   
        JSR CHRIN
       ;   sta CMD2,y
          ;STA CMD_BUFFER_RESULT,Y    ;STORE THE YTH DATA BYTE IN THE YTH
        ; jsr CHROUT
                      ;LOCATION IN THE DATA AREA.
          INY
        CMP #LF       ;IS IT A CARRIAGE RETURN?
        BNE @RD        ;NO, GET ANOTHER DATA BYTE


 

        lda #1
        jsr CLRCHN
        lda #1
        jsr CLOSE

        lda #15
        jsr CLOSE 
        jsr CLSALL
        


    ;reset current cursor position
    plx
    ply
    clc
    jsr PLOT
    


rts

LoadFM:
    ;change dir to fm.prg path
    ;jsr LoadStartDirFromVRAM
    ;change devicenr
    lda FM_DEVICE
    sta $03fe
    
    ;need to change partitions in de future!!!
    
    jsr GotoFMDirectory
    
    ;copy path from VRAM into RAMBANK01

    
    lda #1   ; Logical Number = 1
    ldx $03fe
    ;ldx #8   ; Device = "SD card" (emulation host FS)
    ldy #1   ; 0=skip header byts, 1=use header butes
    jsr SETLFS  

    ;count length of filename. 0-byte terminated    
    ldy #0
    @loop:
        lda File,y
        cmp #0
        beq @Done
        iny
    jmp @loop
        
    @Done:
    

     
    
    lda #6     ;fm.prg
    ldx #<FileFM
    ldy #>FileFM    
    jsr SETNAM
 
    lda #0  ;load, not verify
    jsr LOAD
    ;close not needed
 
   ;  rts
    
    ;get start pointer from basic sys command
    
    ldy #0
    ldx #0  ;searxh of sys command
    @loop2:
        lda BasicStart,Y
        cmp #$9e        ;SYS command
        bne @NoActionChange
        ldx #1  ;search for first non-space and non 9e char=stat of address
        @NoActionChange:
        
        cpx #1
        bne @GotoNextChar
        cmp #$20  ;space overslaan
        beq @GotoNextChar
        cmp #$9e  ;SYS token overslaan
        beq @GotoNextChar
        jmp LoadNext4BytesAsAddress3
        
    
    @GotoNextChar:
        iny
    jmp @loop2 
    
    LoadNext4BytesAsAddress3:
      lda #$40
    jsr CHROUT


        jsr DigiToHexInit
    
        ldx #0
    LoadNextCharAddress3:
        lda BasicStart,y
        AND #%00001111
        sta DigitToHexValue+1,x
         inx
        iny
        cpx #4  
        bne LoadNextCharAddress3
     @Ready: 
     jsr DigitToHex
     
   ;  jsr print_lf
     ;store start of program in 'BasicStart'
     lda Adder24bitValue    ;low byte
     sta ZP_PTR
  ;   lda #$FF
     sta ToFM+1    ;modify JSR address

     lda Adder24bitValue+1    ;low byte
     sta ZP_PTR+1
     sta ToFM+2
      
      
        
     ToFM:
     jmp $0000 ;(ZP_PTR)  ;address will be changed


rts

IncBankPointer:
    pha
        clc
        lda ZP_PTR    ;load low byte
        adc #1
        sta ZP_PTR
        bcc @Done
        inc ZP_PTR+1
        @Done:
    pla
rts

DecBankPointer: 
    pha
        clc
        lda ZP_PTR    ;load low byte
        SEC               
        sbc #1
        sta ZP_PTR

        lda ZP_PTR+1
        sbc #0
        sta ZP_PTR+1
        @Done:
    pla
rts


DigiToHexInit:
    stz DigitToHexValue
    stz DigitToHexValue+1
    stz DigitToHexValue+2
    stz DigitToHexValue+3
    stz DigitToHexValue+4
 
rts

DigitToHex:
    jmp DigitToHex_start
        
    DigitToHexValue: .byte $05,$01,$09,$03,$02  ;Store 4 byte value here. THis example is: 51932    


    
DigitToHex_start:
   jsr InitAdder24bit
   
   stz Adder24bitValue       
   stz Adder24bitValue+1       
   stz Adder24bitValue+2
   
    lda DigitToHexValue+4
    sta Adder24bitValue ;lowest stays the same    

    ;add 10 for each in byte 6-2
    lda #$0A
    sta Adder24bitToAdd    
    lda #$00
    sta Adder24bitToAdd+1     
    ldy DigitToHexValue+3
    jsr Multiply
    
    ;add 100 for each in byte 6-3
    lda #$64
    sta Adder24bitToAdd    
    lda #$00
    sta Adder24bitToAdd+1     
    ldy DigitToHexValue+2
    jsr Multiply 
 
    ;add 1000 for each in byte 6-3
    lda #$E8
    sta Adder24bitToAdd    
    lda #$03
    sta Adder24bitToAdd+1     
    ldy DigitToHexValue+1
    jsr Multiply     

    ;add 10000 for each in byte 6-3
    lda #$10
    sta Adder24bitToAdd    
    lda #$27
    sta Adder24bitToAdd+1     
    ldy DigitToHexValue
    jsr Multiply  
    



rts

Multiply:
    @loop:
        cpy #0
        beq @done
        jsr Adder24bit
        dey
        bra @loop
    @done:
rts

InitAdder24bit:
    STZ Adder24bitValue
    STZ Adder24bitValue+1
    STZ Adder24bitValue+2
    
    STZ Adder24bitToAdd
    STZ Adder24bitToAdd+1
    
rts

Adder24bit:
    jmp Adder24bit_start
        
        Adder24bitValue: .res 3
        Adder24bitToAdd: .res 2
        

    Adder24bit_start:

    LDA Adder24bitValue    ; Laad het laagste byte van de 24-bits waarde
    CLC        ; Zorg ervoor dat de carry-flag gewist is
    ADC Adder24bitToAdd   ; Tel 256 (0x100) op bij het laagste byte
    STA Adder24bitValue    ; Sla het resultaat op

    LDA Adder24bitValue+1    ; Laad het middelste byte van de 24-bits waarde
    ADC Adder24bitToAdd+1   ; Tel 1 op bij het middelste byte (de carry-flag wordt automatisch meegenomen)
    STA Adder24bitValue+1    ; Sla het resultaat op

    LDA Adder24bitValue+2    ; Laad het hoogste byte van de 24-bits waarde
    ADC #$00   ; Voeg niets toe aan het hoogste byte (alleen de carry-flag wordt meegenomen)
    STA Adder24bitValue+2    ; Sla het resultaat op
rts

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
   jsr CHROUT
   rts