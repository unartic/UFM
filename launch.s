.ifdef ram
  .org $9718 ;$8001 ; program gets loaded in memory at $8000
.endif
.ifndef ram
  .org $FA02 ;$FBFF ;1535 byte section at the end of the rambank.
.endif
;last 2kb of basic memory

.segment "ONCE"
.include "constants.s"


jmp LaunchMain

.include "io.inc"

jsrfar3    = $02c4
imparm     = $82
jmpfr      = $02df

FileFM: .asciiz "fm.prg"
            

    LAUNCHFILE = $04FF



Adder24bitValue = $0607 ;: .res 3
Adder24bitToAdd = $060a ;: .res 2

.include "debug.s"
.include "launcher.s"   ;shared launcher program between fmrom.prg and launcher.prg
.include "functions_shared.s"
.include "jsrfar_kernal.s"
jsrfar: .include "jsrfar.inc"


CHROUT:
    jsr jsrfar
    .word $ffd2
    .byt 0
rts


