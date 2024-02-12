;jsrfar versions of the used kernal functions




CHRIN:
    jsr jsrfar
    .word $FFCF
    .byt 0

rts

READST:
    jsr jsrfar
    .word $FFB7
    .byt 0

rts

SETLFS:
    jsr jsrfar
    .word $FFBA
    .byt 0

rts

SETNAM:
    jsr jsrfar
    .word $FFBD
    .byt 0
rts

LOAD:
    jsr jsrfar
    .word $FFD5
    .byt 0


rts

SAVE:
     jsr jsrfar
    .word $FFD8
    .byt 0

rts

CLOSE:
     jsr jsrfar
    .word $FFC3
    .byt 0

rts

OPEN:
     jsr jsrfar
    .word $FFC0
    .byt 0

rts

CHKIN:
     jsr jsrfar
    .word $FFC6
    .byt 0

rts

CHKOUT:
     jsr jsrfar
    .word $FFC9
    .byt 0

rts

GETIN:
     jsr jsrfar
    .word $FFE4
    .byt 0

rts

SCREEN:
     jsr jsrfar
    .word $FFED
    .byt 0

rts


PLOT:
    jsr jsrfar
    .word $FFF0
    .byt 0

rts

CLRCHN:
     jsr jsrfar
    .word $FFCC
    .byt 0

rts

x16Edit:
     jsr jsrfar
    .word $C006
    .byt 13

rts


CLSALL:
     jsr jsrfar
    .word $FFE7
    .byt 0

rts

KEY_POKE:
     jsr jsrfar
    .word $FEC3
    .byt 0

rts

RDTIM:
     jsr jsrfar
    .word $FFDE
    .byt 0

rts

joystick_scan:
     jsr jsrfar
    .word $FF53
    .byt 0

rts

joystick_get:
     jsr jsrfar
    .word $FF56
    .byt 0

rts

SCREEN_MODE:
     jsr jsrfar
    .word $FF5F
    .byt 0

rts

screen_set_charset:
     jsr jsrfar
    .word $FF62
    .byt 0

rts

kbdbuf_get_modifiers:
     jsr jsrfar
    .word $FEC0
    .byt 0

rts

memory_copy:
     jsr jsrfar
    .word $FEE7
    .byt 0
 
rts

ResetVera:  ;CINT
     jsr jsrfar
    .word $FF81
    .byt 0
rts