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

;Increment a 2 byte pointer which points to rambank
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

;Decrement a 2 byte pointer which points to rambank
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

;Reset vars for the 24 adder function
InitAdder24bit:
    STZ Adder24bitValue
    STZ Adder24bitValue+1
    STZ Adder24bitValue+2
    
    STZ Adder24bitToAdd
    STZ Adder24bitToAdd+1
rts




;Add a 16bit value to a 24bit value
;Adder24bitValue (24 bits) = starting value
;Adder24bitToAdd (16 bits) = value to add
Adder24bit:


    LDA Adder24bitValue   
    CLC        
    ADC Adder24bitToAdd   
    STA Adder24bitValue    

    LDA Adder24bitValue+1 
    ADC Adder24bitToAdd+1  
    STA Adder24bitValue+1  

    LDA Adder24bitValue+2  
    ADC #$00  
    STA Adder24bitValue+2 
rts

DoBasicRunCommand:
jmp DoBasicRunCommand_start
    BasicRunCommand: .asciiz "bank1,4:run"
DoBasicRunCommand_start:
    ;clear screen
    lda #$93
    jsr CHROUT
    lda #$0d
    jsr CHROUTbin
    lda #$0d
    jsr CHROUTbin
    ldx #0
    :
        lda BasicRunCommand,x
        cmp #0
        beq PrepareScreenDone
        jsr CHROUTbin
        inx
        jmp :-
    PrepareScreenDone:
    lda #$0d
    jsr CHROUTbin
    
    ldy #0
    ldx #0
    clc
    jsr PLOT
    
    
    lda #$0D
    jsr KEY_POKE  
        
 
rts


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
    lda #0  
    jsr LOAD

    ;dit stukje copieren naar de basic load routine!!!!
    stx $03E1
    sty $03E1+1
    
    
rts
;CHROUT without char conversion to handle filenames 1-to-1
CHROUTbin:
    jsr jsrfar
    .word $ffd2
    .byt 0
rts
