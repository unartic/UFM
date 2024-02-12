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


