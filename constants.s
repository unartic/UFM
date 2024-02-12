 ; Zero Page
 
;ZP: $0022-$007F available to use
ZP_PTR   = $30 ;+$31: general purpose pointer
ZP_PTR2  = $22 ;+23
ZP_TEXT_PRT = $32 ;+$33 ;2 bute pointer for printline function
PARAM_PTR = $28 ;+$29 ;2 bute pointer for printline function

ZP_COPY_S = $34   ;+$35 ;for memcopy
ZP_COPY_D = $36   ;+$37 ;for memcopy

ZP_LFN_S = $38   ;+$39 ;for memcopy


DEBUG                         := $9FBB

; Kernal
;SETLFS   = $FFBA
;SETNAM   = $FFBD
;CHRIN    = $FFCF
;CHROUT   = $FFD2
;LOAD     = $FFD5
;SAVE     = $FFD8
;CLOSE    = $FFC3
;OPEN     = $FFC0
;CHKIN    = $FFC6
;CHKOUT   = $FFC9
;GETIN    = $FFE4
;SCREEN   = $FFED
;PLOT     = $FFF0
;CLRCHN   = $FFCC
;READST   = $FFB7
;CLSALL   = $FFE7

;KEY_POKE = $FEC3

;STASH    = $FF77
;FETSH    = $FF74
;STAVEC   = $03B2

;RDTIM = $FFDE

;joystick_scan = $FF53
;joystick_get = $FF56    

ZP_RAMBANK   = $00
RAMBANK  = $a000
; PETSCII
RETURN      = $0D
SPACE       = $20
COLON       = $3A
CLR         = $93
LF          = $0D

;SCREEN_MODE = $FF5F










   