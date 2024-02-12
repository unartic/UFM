
jmp DoNotExecute
    ;Tokenized basic snipped: BANK 1,16: SYS $C000
    SysToRom: .byte $ce,$98,$20,$31,$2c,$31,$36,$3a,$9e,$20,$24,$43,$30,$30,$30,$0 

    DigitToHexValue = $0602 ;byte 5

    CMD = $0400 ;recycle CMD_BUFFER from fm.prg: .res 255 ;command  ;use r0l and r0h, saves 255 bytes

    BasicStart = $0801
    cnt           = $05ff ;.byte $0
    FM_DEVICE     = $0600 ;.byte 0
    FM_PARTITION  = $0601 ;.byte 0
DoNotExecute:


LaunchMain:

    ;Set byte $02 to $80 on zeropage to zero. It seems necassary to mimic a cold start
    ldx #$00
    :
      stz $02,x
      inx
      cpx #$80
      bne :-

    ;The RAM version sets the first byte of LAUNCHFILE
    ;to $0 to indicate launch has occured.
    ;When control is back to the launcher when de launched program has finished
    ;having this byte at $0 means: load UFM again
    lda LAUNCHFILE ;File
    cmp #0  ;goto FM
    beq goto_loadfm

    ;LAUNCHFILE contains file to be launched.
    ldy #0  
    @CopyByte:
      lda LAUNCHFILE+1,y ;File+1,y
      sta CMD,y
      cmp #0
      beq @CopyDone

      iny
    bra @CopyByte
    @CopyDone:
   

    iny
    lda LAUNCHFILE+1,y ;Byte after the filename contains the device to switch to
    sta FM_DEVICE
    lda LAUNCHFILE+2,y ;Byte after the filename contains the partition to switch to
    sta FM_PARTITION
      
    sty cnt ;file length

    ;First byte of LAUNCHFILE contains a B for Tokenized basic of A of Machine language file
    lda LAUNCHFILE ; File
    cmp #$41  ;A=assembly
    beq PRGFile ;handle machine language file
    
    
    ;load and and prepare basic file
    jsr LoadBasicFileIntoMemory

    ;sets the first byte of LAUNCHFILE to $0 to indicate the next time
    ;the launcher is called, UFM needs to be loaded
    lda #0 
    sta LAUNCHFILE    
      
    ;Put RUN + [ENTER] on keyboard buffer, to start the loaded basic file
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
      

PRGFile:
  
  ;load and start Machine Language file
  jsr LoadPRGFileIntoMemory
  jsr StartPRG

rts

goto_loadfm:  jmp LoadFM

;Load the ML-file into memory
LoadPRGFileIntoMemory:
    lda #1  
    ldx $03fe 
    ldy #1  
    jsr SETLFS  

    lda cnt
    ldx #<CMD
    ldy #>CMD
    jsr SETNAM
    lda #0  ;load, not verify
    jsr LOAD
rts

;Read out SYS address of basic stub, and jsrfar to it
StartPRG:

    
    ;Read basic stub to find the address to jmp to
    ldy #0
    ldx #0 
    @loop2:
        lda BasicStart,Y
        cmp #$9e        ;Token for SYS command
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
        ;Next 4 bytes are the address to jmp to. Need to converd decimal to hex
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
  
      ;Store the addres at $02df+1. This is where the jsrfar target address is located in memory
      lda Adder24bitValue
      sta $02df+1
      lda Adder24bitValue+1
      sta $02df+2
      
      
      ;The next bit mimics the start of the regular JSRFAR-code
      ;Push return address on the stack. 
      lda #>ReturnFromJSRFAR
      pha
      lda #<ReturnFromJSRFAR
      pha
      lda $01 ;store current rombank, which JSRFAR needs to change back to
      pha
      php
      pha
      lda #4  ;ROM bank to change to
      jmp jsrfar3
      
      
    ReturnFromJSRFAR:
      nop ;needed because jsrfar assumes 3 byte to skip
      nop
      nop
   
     .ifdef ram
        jsr LoadFM
     .endif

     .ifndef ram
        jmp $c000
     .endif

rts



Goto_toeof:  jmp toeofx
  
;This subroutine does a few steps to prepare a tokenized basic file for launching:
; 1. Reads the file into memory
; 2. Replaces all END statement with GOTO 63999 statements
; 3. Adds line 63999 with a SYS command from which address is depended if it is the ROM or RAM version
; 4. Recalculate basic next line pointers  
LoadBasicFileIntoMemory:

    lda #1  
    ldx $03fe
    ldy #0  
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
     
    jsr CHRIN ;first 2 bytes contain memlocation, TODO: set pointer to address in file instead of $0801 hardcoded
    jsr CHRIN  
    NextLine:
      jsr READST
      bne Goto_toeof
      
      ;4 bytes containing offset next line + line number, just copy
      ldx #0
      :
        jsr CHRIN
        sta (ZP_PTR)
        jsr IncBankPointer
        jsr READST
        bne Goto_toeof
        inx
        cpx #4
        bne :-
   

   NextByteX:

   
      jsr READST
      bne Goto_toeof
      
      ;only when in a basic line!!!!
      jsr CHRIN
      cmp #$80      ;$80 = END statement  
      beq @InsertGotoSys  ;insert GOTO 63999 instead of END statement     
      
      ;store in mem
      sta (ZP_PTR)
      jsr IncBankPointer
      cmp #0  ;next line
      beq NextLine  ;$0 byte indicates next line
      
    bra NextByteX 
      
      
    @InsertGotoSys:

        ;write new command
        lda #$89  ;goto ;replacing #$80=end
        sta (ZP_PTR)
        jsr IncBankPointer  

        lda #$20  ;space
        sta (ZP_PTR)
        jsr IncBankPointer  

        lda #$36  ;address digit 6
        sta (ZP_PTR)
        jsr IncBankPointer  

        lda #$33  ;address digit 3
        sta (ZP_PTR)
        jsr IncBankPointer  

        lda #$39  ;address digit 9
        sta (ZP_PTR)
        jsr IncBankPointer  

        lda #$39  ;address digit 9
        sta (ZP_PTR)
        jsr IncBankPointer  

        lda #$39  ;address digit 9
        sta (ZP_PTR)
        jsr IncBankPointer  
    ;continue reading file
    jmp NextByteX

    
    toeofx:
    jsr CLRCHN
    lda #1
    jsr CLOSE 
      
    
    ;add last line with SYS command
    ;Reverse pointer to before the zero bytes which mark the end. Amount of $0 bytes is can vary!
    @nextzero:    
      jsr DecBankPointer   
      lda (ZP_PTR)
      cmp #0
      beq @nextzero       ;reverse until a non-zero byte
    jsr IncBankPointer    ;set pointer on last 00, which now becomes an end of line marker
    jsr IncBankPointer    ;advance pointer to beginning of new line

  skip:
  
    ;;;;;;;;;;;;;;;;;;;;
    ;This seems to be absolete, as all line numbers will be recalculated anyway.Just  need to add 2 bytes probably
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
      ;;;;;;;;;;;;;;;;;;;;;;;
    
      
      
    ;add line number 63999
    lda #$FF
    sta (ZP_PTR)
    jsr IncBankPointer    

    lda #$F9
    sta (ZP_PTR)
    jsr IncBankPointer 
    
    ;add sys command
    
.ifdef ram
      lda #$9e  ;sys command
      sta (ZP_PTR)
      jsr IncBankPointer 
      lda #$20  ;space
      sta (ZP_PTR)
      jsr IncBankPointer 

      lda #$33  ;Decimal address of sys command
      sta (ZP_PTR)
      jsr IncBankPointer 
      
      lda #$38  ;Decimal address of sys command
      sta (ZP_PTR)
      jsr IncBankPointer 

      lda #$36  ;Decimal address of sys command
      sta (ZP_PTR)
      jsr IncBankPointer 

      lda #$38  ;Decimal address of sys command
      sta (ZP_PTR)
      jsr IncBankPointer 

      lda #$30  ;Decimal address of sys command
      sta (ZP_PTR)
      jsr IncBankPointer 
.endif

.ifndef ram
  ;add tokenized basic line to switch back to bank 16; and sys to $c000
  ldx #0
  :
    lda SysToRom,x
    cmp #0
    beq CopyDoneXX
    sta (ZP_PTR)
    jsr IncBankPointer
    inx
    jmp :-
    CopyDoneXX:
.endif   
    

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



    ;Recalculate next line address pointers
    
    ;start at basic
    lda #$01
    sta ZP_PTR
    lda #$08
    sta ZP_PTR+1    
    
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
          ;move pointer to beginning of basic line. Skipping next line pointer and line number (4 bytes)
          jsr IncBankPointer      
          jsr IncBankPointer      
          jsr IncBankPointer      
          jsr IncBankPointer          
          jsr GotoNextZero  ;goto end of line $0 byte
          jsr IncBankPointer
    
        ldx ZP_PTR   ;Store address of the next line
        ldy ZP_PTR+1
        
        ;Set pointer back to beginning of line
        pla
        sta ZP_PTR+1
        pla
        sta ZP_PTR
        
        ;Write address of next line
        txa
        sta (ZP_PTR)
        jsr IncBankPointer 
        tya
        sta (ZP_PTR)
        jsr IncBankPointer 
        
        
        ;move to start of line, and continu to the next line
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

;Advance pointer to next $0 byte
GotoNextZero:
  @lp:
    lda (ZP_PTR)
    cmp #0
    beq @Klaar
    jsr IncBankPointer 
  bra @lp
  @Klaar:
rts



;Change directory, device and partition to where fm.prg was started from
GotoFMDirectory:
.ifdef ram
    lda #00
    sta $00 ;switch to rambank 0

    ;switch to partition if needed
    lda FM_PARTITION
    cmp #0
    beq @NoChangeNeededHosftFS  ;hostfs doesn't have partitions
    
    lda #$43
    sta CMD
    lda #$50
    sta CMD+1
    lda FM_PARTITION
    clc
    adc #$30
    sta CMD+2

        ldy #3  ;CP[partition number]
        ldx #<CMD
        ldy #>CMD
        jsr SETNAM
        

        lda #1  
        ldx $03fe   
        ldy #15 

        jsr SETLFS
        jsr OPEN
        lda #1
        jsr CLOSE
    
    rts
    
    @NoChangeNeededHosftFS:
    

      ldy #0
      
      ;first load CD:
      lda #$43       ; C
      sta CMD,y
      iny        
      lda #$44       ; D
      sta CMD,y
      iny        
      lda #$3A       ; :
      sta CMD,y
      iny        


      ;then load path
      ldx #0
      @nxt:
          lda $BF00,x
          cmp #0
          beq @Ready
          sta CMD,y
          iny
          inx
      jmp @nxt          
      @Ready:
      
      tya   ;y contains length, so tranfer it to A
      ldx #<CMD
      ldy #>CMD
      jsr SETNAM

      lda #1   ; Logical Number = 1
      ldx $03fe   
      ldy #15   ;   15=control channel

      jsr SETLFS

      jsr OPEN
      LDX #1
      jsr CHKIN  

      ldy #$00    
      @RD:   
      JSR CHRIN
    
        INY
      CMP #LF     
      BNE @RD     

      lda #1
      jsr CLRCHN
      lda #1
      jsr CLOSE

.endif
rts


;Load fm.prg into memory and jmp to beginning to run 
  LoadFM:
  .ifdef ram  
    
      lda FM_DEVICE
      sta $03fe
      
      jsr GotoFMDirectory

      lda #1  
      ldx $03fe
      ldy #1
      jsr SETLFS  

      lda #6     ;fm.prg
      ldx #<FileFM
      ldy #>FileFM    
      jsr SETNAM

      lda #0  ;load, not verify
      jsr LOAD
   
      jmp $080d 

.endif
rts








DigiToHexInit:
    stz DigitToHexValue
    stz DigitToHexValue+1
    stz DigitToHexValue+2
    stz DigitToHexValue+3
    stz DigitToHexValue+4
rts

DigitToHex:
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



   
   
