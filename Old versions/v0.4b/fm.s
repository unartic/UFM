.org $080D

.segment "ONCE"

.include "constants.s"

;Rambanks used:
RB_FILES_LIST_LEFT      = 01    
RB_FILES_LIST_RIGHT     = 02
RB_LONGFILENAMES_LEFT   = 03
RB_LONGFILENAMES_RIGHT  = 04
RB_PARTITIONS           = 05


;TODO:
; - typen  gaat direct naar folder of bestand met die naam
; - sterretje = filteren
; - launch.s zo klein mogelijk maken
; - launch.s zo hoog mogelijk in mem plaatsen
; - check if ufm.prg is in current folder, ifso store cfg-file in root to re-launch fron launch.prg
; - Copy memory banks if dirs are the same on start needs to be tested if they are different
; - test all actions on and between partitions
; - move between partitions testen als we internet hebben voor de syntax
; - launch testen en partities implementeren

; Known limitations:
; - 255x200 = 51000 files max per directory
; - total path length per window is 255 bytes

; TESTS:
; MKDIR root:         x      HostFS: 06-12  SD-Card: 11-22      Real Hardware:
; MKDIR non-root:     x      HostFS: 06-12  SD-Card: 11-22      Real Hardware

; Rename root:        x      HostFS: 06-12  SD-Card: 11-22      Real Hardware:
; Rename non-root:    x      HostFS: 06-12  SD-Card: 11-22      Real Hardware

; Delete file root:   x      HostFS: 06-12  SD-Card: 11-22      Real Hardware:
; Delete file non-root: x    HostFS: 06-12  SD-Card: 11-22      Real Hardware

; Delete dir root:      x    HostFS: 06-12  SD-Card: 11-22      Real Hardware:
; Delete dir non-root:   x   HostFS: 06-12  SD-Card: 11-22      Real Hardware

; Edit File from root:    x  HostFS: 06-12  SD-Card: 11-22      Real Hardware:
; Edit file from non-root: x HostFS: 06-12  SD-Card: 11-22      Real Hardware

; Copy file root-sub:  x     HostFS: 06-12  SD-Card: 11-22      Real Hardware:
; Copy file sub-root:  x     HostFS: 06-12  SD-Card: 11-22      Real Hardware
; Copy file sub-sub:   x     HostFS: 06-12  SD-Card: 11-22      Real Hardware

; Run BAS from root         HostFS: 06-12  SD-Card: 11-22      Real Hardware
; Run LM from root          HostFS: 06-12  SD-Card: 11-22      Real Hardware

; Run bas from sub          HostFS: 06-12  SD-Card: 11-22      Real Hardware
; Run LM from sub           HostFS: 06-12  SD-Card: 11-22      Real Hardware
; Run from other device
; Run from other partition

; Rename dir           x     HostFS: 06-12  SD-Card: 11-22      Real Hardware

; move file: not supported  

;screenmodes
;Mode	Description
;$00	80x60 text
;$01	80x30 text
;$02	40x60 text
;$03	40x30 text
;$04	40x15 text
;$05	20x30 text
;$06	20x15 text
;$07	22x23 text
;$08	64x50 text
;$09	64x25 text
;$0A	32x50 text
;$0B	32x25 text
;$80	320x240@256c ;40x30 text

;Keyboard Key	SNES Equivalent
;X or Ctrl	        A
;Z or Alt	        B
;S	                X
;A	                Y
;D	                L
;C	                R
;Shift	            SELECT
;Enter	            START
;Cursor Up	        UP
;Cursor Down	    DOWN
;Cursor Left	    LEFT
;Cursor Right	    RIGHT

 ;                       128  64  32  16  8   4   2   1
; .A, byte 0:           | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
;                  SNES | B | Y |SEL|STA|UP |DN |LT |RT |;;

 ;     .X, byte 1:      | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
 ;                 SNES | A | X | L | R | 1 | 1 | 1 | 1 |
 ;     .Y, byte 2:
jmp start

iCurrentWindow: .byte $0    ;current workingn window: 0=Left, 1=Right

sGeneral: .res 200,$41        ;General purpose variable
sGeneral2: .res 200,$42        ;General purpose variable

ParamBuffer: .asciiz "12345678901234567890"    ;20 bytes

;Settings:
SET_SHOWDATE: .byte 0
SET_SHOWTIME: .byte 0
SET_SHOWSIZE: .byte 0
SET_SHOWTYPE: .byte 0
SET_DATE_LEN: .byte 10
.byte 0,0,0,0
SET_MAX_FILES_COUNT: .byte 129  ;200 fits in 1 rambank
.byte 0,0,0,0
SET_RECORD_LENGTH: .byte 40


JOY_NUMBER: .byte $1    ;0=keyboard, 1=joy1


JOY_PRESENT: .byte $0

JOY_PRESSED_A: .byte $0   
JOY_STATUS_A: .byte $0  ;0=wait for press, 1=wait for release

DEFAULT_MODAL_WIDTH: .byte 40


SLASH: .byte "/"
TMPDIR: .asciiz "/ditiseenhelelenganaam/dirx                                                           "

CAPTION_DIR: .byte "   <dir>"

VERA_PLOT_X: .byte $0
VERA_PLOT_Y: .byte $0

CNT: .byte $0        
         
TMP:  .byte $0
TMP2: .byte $0
TMP3: .byte $0
TMP_VAR: .res 255

LFN_Pointer: .byte $0,$0 ;"  " ; 2 bytes to temporatily store long filename pointer

ListSize: .byte 48  ;lines on screen
ListCounter: .byte $0

CMD_BUFFER: .res 255    ;max length of CD: and the full path
CMD_BUFFER_LENGTH: .byte $0
CMD_BUFFER_RESULT: .res 50

SCREEN_ROWS: .byte $0
SCREEN_COLS: .byte $0

DOS_DIR_TYPE: .byte $0
DOS_FILESOLD: .byte "$=t:*=p" ;prg files ;"$=t"  ;
DOS_FILES: .byte "$=l:*=p" ;prg files ;"$=t"  ;
DOS_FILES_END: .byte $0

DOS_DIROLD: .byte "$=t:*=d"    ;dirs ;"$=t"  ;
DOS_DIR: .byte "$=l:*=d"    ;dirs ;"$=t"  ;
DOS_DIR_END: .byte $0


DOS_FILEEXISTS: .asciiz "$:[f]"
DOS_CURDIR: .asciiz "cd:[c]"
DOS_CURDIROTHER: .asciiz "cd:[p]"

DOS_CMD_DEVICE: .byte 0


OFFSET_CURRENT: .byte $0    
OFFSET_LEFT: .byte $0
OFFSET_RIGHT: .byte $0

ROW_CURRENT: .byte $00   ;Number of the highlighted screen row
ROW_LEFT: .byte $00
ROW_RIGHT: .byte $00

DIR_CURRENT: .res 500    ;current path
DIR_LEFT:    .res 500
DIR_RIGHT:   .res 500

DEVICE_CURRENT: .byte 0
DEVICE_OTHER:   .byte 0
DEVICE_LEFT: .byte 0
DEVICE_RIGHT: .byte 0
DEVICE_START: .byte 0


PARTITION_CURRENT: .byte 0
PARTITION_LEFT: .byte 0
PARTITION_RIGHT: .byte 0
PARTITION_OTHER: .byte 0
PARTITION_START: .byte 0

PAGE_CURRENT: .byte 0
PAGE_LEFT: .byte 0
PAGE_RIGHT: .byte 0



MODE_CURRENT: .byte 0
MODE_LEFT: .byte 0
MODE_RIGHT: .byte 0
MODE_OTHER: .byte 0

CNT_PARTITIONS: .byte 0


FILECOUNT_CURRENT: .byte $0
FILECOUNT_LEFT: .byte $0
FILECOUNT_RIGHT: .byte $0

EMPTY_LINE: .res 38,$20
            

COL_OFFSET: .byte 1

BlankLine: .byte $20

MODAL_COL: .byte $0
MODAL_ROW: .byte $0

MODAL_COL_COUNT: .byte $0
MODAL_ROW_COUNT: .byte $0

MODAL_WIDTH: .byte $0
MODAL_HEIGHT: .byte $0

MODAL_SCREEN_BACKUP: .res 2000  ;write to a memorybank, saves a lot of space

CopyAddrCount: .byte $0 ;cnt is stored for memcopy

LFNBufferTmp: .res 255

TSRTYPE: .byte $00
TSRPRG: .asciiz "[s]/[f]" 


JOY_BTN_A_COUNTDOWN: .byte $0

JOY_BTN_DOWN_COUNTDOWN: .byte $0
JOY_PRESSED_DOWN: .byte $0

JOY_BTN_UP_COUNTDOWN: .byte $0
JOY_PRESSED_UP: .byte $0

JOY_BTN_RIGHT_COUNTDOWN: .byte $0
JOY_PRESSED_RIGHT: .byte $0

JOY_BTN_LEFT_COUNTDOWN: .byte $0
JOY_PRESSED_LEFT: .byte $0

JOY_BTN_B_COUNTDOWN: .byte $0
JOY_PRESSED_B: .byte $0

JOY_BTN_L_COUNTDOWN: .byte $0
JOY_PRESSED_L: .byte $0

JOY_BTN_R_COUNTDOWN: .byte $0
JOY_PRESSED_R: .byte $0

JOY_BTN_SELECT_COUNTDOWN: .byte $0
JOY_PRESSED_SELECT: .byte $0


JOY_PRESS_DELAY: .byte 10



LAST_RDTIM: .byte $0

.macro StoreCurrentAInMemoryMacro
    sta (ZP_PTR)
    pha
 
        
            clc
            lda ZP_PTR    ;load low byte
            adc #1
            sta ZP_PTR
            bcc :+
            inc ZP_PTR+1
            :
            
    
 
    pla
   ; jsr IncBankPointer 
.endmacro

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

.macro DecUntilZero addr
    pha
        lda addr
        cmp #0
        beq :+
        dec addr  
    :
    pla

.endmacro

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
                cmp #$c0
                bne @lp
                
        pla
        sta ZP_RAMBANK
    
    .endscope
    
.endmacro

.macro HandleJoyPress JOY_COUNTDOWN,JOY_PRESSED
 
    lda JOY_COUNTDOWN
    cmp #0
    bne :+   ;countdown busy, no new trigger

    lda #1
    sta JOY_PRESSED
    lda JOY_PRESS_DELAY
    sta JOY_COUNTDOWN ; ticks, no new trigger 

    :
 
.endmacro



ReadJoystick:
   
    jsr RDTIM
   
    cmp LAST_RDTIM
    beq @Done  ;no change, do nothing
    sta LAST_RDTIM  ;store new value
    
    
    ;decrease counters to prevent multiple presses
    DecUntilZero JOY_BTN_A_COUNTDOWN
    DecUntilZero JOY_BTN_DOWN_COUNTDOWN
    DecUntilZero JOY_BTN_UP_COUNTDOWN
    DecUntilZero JOY_BTN_RIGHT_COUNTDOWN
    DecUntilZero JOY_BTN_LEFT_COUNTDOWN
    DecUntilZero JOY_BTN_B_COUNTDOWN
    DecUntilZero JOY_BTN_L_COUNTDOWN
    DecUntilZero JOY_BTN_R_COUNTDOWN
    DecUntilZero JOY_BTN_SELECT_COUNTDOWN
    



   @Done:

    ;check if a button is pressed
    stz JOY_PRESSED_A
    stz JOY_PRESSED_DOWN
    stz JOY_PRESSED_UP
    stz JOY_PRESSED_RIGHT
    stz JOY_PRESSED_LEFT
    stz JOY_PRESSED_B
    stz JOY_PRESSED_L
    stz JOY_PRESSED_R
    stz JOY_PRESSED_SELECT
    
     
    lda JOY_NUMBER  ;0=keyboard, 1=first joystick
    jsr joystick_get 
   
    sta TMP
    and #8
    beq Goto_JoyButtonUp 
    
    lda TMP
    and #4
    beq Goto_JoyButtonDown  

    lda TMP
    and #32
    beq Goto_JoyButtonSelect  

    
    lda TMP
    and #1
    beq Goto_JoyButtonRight  

    lda TMP
    and #2
    beq Goto_JoyButtonLeft  

    lda TMP
    and #128
    beq Goto_JoyButtonB  
 
    txa
    and #128    ;button A
    beq JoyButtonA

    txa
    and #16   
    beq JoyButtonR

    txa
    and #32   
    beq JoyButtonL


    
rts

Goto_JoyButtonLeft: jmp JoyButtonLeft
Goto_JoyButtonB: jmp JoyButtonB 
Goto_JoyButtonRight: jmp JoyButtonRight
Goto_JoyButtonDown: jmp JoyButtonDown
Goto_JoyButtonUp: jmp JoyButtonUp
Goto_JoyButtonSelect: jmp JoyButtonSelect

JoyButtonA:   
    HandleJoyPress JOY_BTN_A_COUNTDOWN,JOY_PRESSED_A
rts

JoyButtonR:   
    HandleJoyPress JOY_BTN_R_COUNTDOWN,JOY_PRESSED_R
rts

JoyButtonL:   
    HandleJoyPress JOY_BTN_L_COUNTDOWN,JOY_PRESSED_L
rts


JoyButtonDown: 
    HandleJoyPress JOY_BTN_DOWN_COUNTDOWN,JOY_PRESSED_DOWN
rts

JoyButtonUp: 
    HandleJoyPress JOY_BTN_UP_COUNTDOWN,JOY_PRESSED_UP
rts

JoyButtonRight: 
    HandleJoyPress JOY_BTN_RIGHT_COUNTDOWN,JOY_PRESSED_RIGHT
rts

JoyButtonLeft: 
    HandleJoyPress JOY_BTN_LEFT_COUNTDOWN,JOY_PRESSED_LEFT
rts

JoyButtonB: 
    HandleJoyPress JOY_BTN_B_COUNTDOWN,JOY_PRESSED_B
rts

JoyButtonSelect: 
    HandleJoyPress JOY_BTN_SELECT_COUNTDOWN,JOY_PRESSED_SELECT
rts


ReadWindowVars:
    pha
    phx
        lda iCurrentWindow
        cmp #0
        beq @DoLeftWindow
            ;DoRightWindow
            lda ROW_RIGHT
            sta ROW_CURRENT

            lda PAGE_RIGHT
            sta PAGE_CURRENT

            lda DEVICE_RIGHT
            sta DEVICE_CURRENT

            lda PARTITION_RIGHT
            sta PARTITION_CURRENT

            lda MODE_RIGHT
            sta MODE_CURRENT

            lda DEVICE_LEFT
            sta DEVICE_OTHER

            lda PARTITION_LEFT
            sta PARTITION_OTHER


            lda MODE_LEFT
            sta MODE_OTHER

            lda OFFSET_RIGHT
            sta OFFSET_CURRENT

            lda FILECOUNT_RIGHT
            sta FILECOUNT_CURRENT
        
            ldx #0
            :
                lda DIR_RIGHT,x
                sta DIR_CURRENT,x
                inx
                cpx #255
                beq :+
                jmp :-
            :

            
        jmp @SwitchDone
        
        @DoLeftWindow:
            lda ROW_LEFT
            sta ROW_CURRENT

            lda PAGE_LEFT
            sta PAGE_CURRENT

            lda DEVICE_LEFT
            sta DEVICE_CURRENT

            lda PARTITION_LEFT
            sta PARTITION_CURRENT

            lda MODE_LEFT
            sta MODE_CURRENT

            
            lda DEVICE_RIGHT
            sta DEVICE_OTHER

            lda PARTITION_RIGHT
            sta PARTITION_OTHER


            lda MODE_RIGHT
            sta MODE_OTHER

            
            lda OFFSET_LEFT
            sta OFFSET_CURRENT

            lda FILECOUNT_LEFT
            sta FILECOUNT_CURRENT
        
            ldx #0
            :
                lda DIR_LEFT,x
                sta DIR_CURRENT,x
                inx
                cpx #255
                beq :+
                jmp :-
            :
        @SwitchDone:
    plx
    pla

rts

StoreWindowVars:
    pha
    phx 
        clc
        lda iCurrentWindow
        cmp #0
        beq @DoLeftWindow  
        ;DoRightWIndow
          
            lda ROW_CURRENT
            sta ROW_RIGHT

            lda PAGE_CURRENT
            sta PAGE_RIGHT

            lda DEVICE_CURRENT
            sta DEVICE_RIGHT

            lda PARTITION_CURRENT
            sta PARTITION_RIGHT

            lda OFFSET_CURRENT
            sta OFFSET_RIGHT

            lda FILECOUNT_CURRENT
            sta FILECOUNT_RIGHT

        
            ldx #0
            :
                lda DIR_CURRENT,x
                sta DIR_RIGHT,x
                inx
                cpx #255
                beq :+
                jmp :-
            :        
        jmp @SwitchDone
        @DoLeftWindow:
           
 
            lda ROW_CURRENT
            sta ROW_LEFT

            lda PAGE_CURRENT
            sta PAGE_LEFT


            lda DEVICE_CURRENT
            sta DEVICE_LEFT

            lda PARTITION_CURRENT
            sta PARTITION_LEFT


            lda OFFSET_CURRENT
            sta OFFSET_LEFT

            lda FILECOUNT_CURRENT
            sta FILECOUNT_LEFT

        
            ldx #0
            :
                lda DIR_CURRENT,x
                sta DIR_LEFT,x
                inx
                cpx #255
                beq :+
                jmp :-
            :
        @SwitchDone:
    plx
    pla

rts


SwitchWindow:
    
     
    jsr StoreWindowVars
    
    
        lda iCurrentWindow
        cmp #0
        beq @ToRightWindow;ToLeftWindow
            lda #0
            sta iCurrentWindow  ;left window
            lda #1
            sta COL_OFFSET
        jmp @SwitchDone
        @ToRightWindow:       ;ToRightWIndow
            lda #1
            sta iCurrentWindow
            
            lda #2
            clc
            adc WINDOW_WIDTH
            sta COL_OFFSET
        @SwitchDone:
    jsr ReadWindowVars
   
      
rts


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

.macro PrintUntilZero addr
    pha
    phy
        ldy #0
        :
            lda addr,Y
            cmp #0
            beq :+
            jsr CHROUT
            iny
            jmp :-
        :
    ply
    pla
.endmacro

.macro CopyAddr aFrom,aTo,acnt
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
      cpy acnt
      beq :+   ;done
      bra :-   ;next
   :  ;done
   sty CopyAddrCount ;amount copied
   pla
   ply

   
   
.endmacro

;would be better in a subroutine, but this is easier
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

.macro PrintLine addr
   lda #<addr
   sta ZP_TEXT_PRT
   lda #>addr
   sta ZP_TEXT_PRT+1
   jsr PrintText 
.endmacro

.macro PrintChar addr,cnt
    
 
    phy
        ldy #0

       :        ;NextChae
            lda (addr)
            jsr CHROUT
            jsr IncBankPointer
            iny
            
            cpy #cnt
            beq :+      ;ToDone
        
            bra :-      ;To NextChar
        :       ;Done
    ply  
   
   
.endmacro

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

.macro PrintACharVera cnt

 
    phy
        ldy #0

       :        ;NextChae
            ;lda (addr)
            ;jsr CHROUT
            ;adc #56
            jsr ConvertPetsciiToVera
            STA $9F23
            ;jsr IncBankPointer
            iny
            
            cpy #cnt
            beq :+      ;ToDone
        
            bra :-      ;To NextChar
        :       ;Done
    ply  
   
   
.endmacro



ConvertPetsciiToVera:
    cmp #$40
    bpl @DoConvert
    cmp #0
    bne @doRts
    lda #$20
    @doRts:
    rts ;no convert necesarry
    @DoConvert:
    sbc #$40

    
    
rts

SetCharset:
     ;set correct charset
    clc
    lda #2
    jsr $FF62 
   ; jsr WaitKey
rts

SetCharsetIso:
     ;set correct charset
    clc
    lda #1
    jsr $FF62 
  ;  jsr WaitKey
rts

Repaintscreen:

    sec
    jsr SCREEN_MODE
    sty SCREEN_ROWS
    stx SCREEN_COLS

    jsr SetCharset

    clc
    ldx #0
    ldy #0
    jsr PLOT


    
    jsr PrintMainScreen2
    
    jsr SwitchWindow    ;needed to update COL_OFFSET
    jsr SwitchWindow    ;needed to update COL_OFFSET
    
    

    ;check ROW_CURRENT > ListSize 
    lda ROW_LEFT
    cmp ListSize
    bmi @NoChangeLeft
    lda ListSize
    sta ROW_LEFT
    dec ROW_LEFT
        
    @NoChangeLeft:
    lda ROW_RIGHT
    cmp ListSize
    bmi @NoChangeRight
    lda ListSize
    sta ROW_RIGHT
    dec ROW_RIGHT

    @NoChangeRight:
    lda ROW_CURRENT
    cmp ListSize
    bmi @NoChangeCurrent
    lda ListSize
    sta ROW_CURRENT
    dec ROW_CURRENT
    @NoChangeCurrent:
       
    jsr LowlightRow
    jsr ShowFiles2
    jsr SwitchWindow
    jsr ShowFiles2
    jsr SwitchWindow

    jsr HighlightRow
rts
;$:*=P


CheckConnectedDevices:
jmp CheckConnectedDevices_start

    DEVICES_CONNECTED: .res 30,0 

CheckConnectedDevices_start:

    lda #8
    sta TMP
    ldy #0
    
    @lp:
        phy
            lda #1   ; Logical Number = 1
            ldx TMP
            ldy #15   ;   15=control channel
            jsr SETLFS    

            lda (DOS_FILES_END-DOS_FILES) ; filename length
            ldx #<DOS_FILES
            ldy #>DOS_FILES
            jsr SETNAM   
            jsr OPEN
            lda #1
            jsr CLOSE
            jsr READST
        ply
       
        cmp #0
        bne @Next
        lda TMP
        sta DEVICES_CONNECTED,y
        iny
        
        @Next:
            inc TMP
            lda TMP
            cmp #30
            bpl @Done
        
    jmp @lp 
    @Done: 
   
    rts 

GetFSType:
jmp GetFSType_start
    DOS_CHECKLIST: .byte "$=qw789nnasdhjgwh"
    DOS_CHECKLIST_END:
    FSTYPE: .byte $ff
GetFSType_start: 
    lda #1   ; Logical Number = 1
    ldx $03fe ;#8   ; Device = "SD card" (emulation host FS)
    ldy #0   ; Secondary Address = 15: dos command channel
    jsr SETLFS
  
    lda #(DOS_CHECKLIST_END-DOS_CHECKLIST) ; command
    ldx #<DOS_CHECKLIST
    ldy #>DOS_CHECKLIST   
 
    jsr SETNAM
    jsr OPEN
    
    LDX #1
    jsr CHKIN 
    
    ldy #0
    ldx #0
    @lp:
        jsr CHRIN
        iny
        cpy #200
        beq @Done   ;shouldnt happen
        cmp #$22
        bne @lp

        inx
        cpx #2  ;found 2 quotes
        beq @ReadType

    jmp @lp
    @ReadType:
        jsr CHRIN   ;space
        jsr CHRIN   ;H (hostfile) F=fat32
        sta FSTYPE
    
    @Done:
    jsr CLRCHN
    lda #1
    jsr CLOSE  
    
  
rts

Goto_DoneListPartitions: jmp DoneListPartitions
LoadPartitionsList:
jmp LoadPartitionsList_start
    MSG_HOSTFS: .asciiz "hostfs"
LoadPartitionsList_start:
    
    lda #RB_PARTITIONS
    sta $00 
    
    lda #<RAMBANK
    sta ZP_PTR
    lda #>RAMBANK
    sta ZP_PTR+1
    
    
 
    ldy #0
    @lp:
        lda DEVICES_CONNECTED,y

           
        cmp #0
        beq Goto_DoneListPartitions
        
        phy
            sta $03fe
            jsr GetFSType
            lda FSTYPE
            
            
            cmp #$48    ;H=hostfs
            beq @AddHostFS

            cmp #$46    ;F=fat32
            beq @AddFat32
            jmp @Continue
            
            @AddFat32:  jsr ReadPartitionTable
            
            jmp @Continue
            
            @AddHostFS:
                inc CNT_PARTITIONS
                
                ldy #0
                ;frist two bytes are device + partition
                lda $03fe
                StoreCurrentAInMemoryMacro
                iny
                lda #0
                StoreCurrentAInMemoryMacro
                iny
                
                clc
                lda $03fe
                adc #$30
                StoreCurrentAInMemoryMacro
                iny
                lda #COLON
                StoreCurrentAInMemoryMacro
                iny
                
                ldx #0
                @lp2:
                    lda MSG_HOSTFS,x
                    inx
                    iny
                    StoreCurrentAInMemoryMacro
                    cmp #0
                    bne @lp2 
                
                lda #0
                StoreCurrentAInMemoryMacro
                iny
                
                @lp3:
                    lda #$20
                    StoreCurrentAInMemoryMacro
                    iny
                    cpy SET_RECORD_LENGTH
                    bne @lp3
                    
                jsr DecBankPointer
                jsr DecBankPointer
                jsr DecBankPointer
                lda #$41
                StoreCurrentAInMemoryMacro
                jsr IncBankPointer
                jsr IncBankPointer
                
            @Continue:
            
        ply
        iny
    jmp @lp
  
    DoneListPartitions:

rts

ReadPartitionTable:
jmp ReadPartitionTable_start
    DOS_LIST_PARTITIONS: .byte "$=p"
    DOS_LIST_PARTITIONS_END:

ReadPartitionTable_start: 
    phy
    phx
        lda $03fe

        lda #1   ; Logical Number = 1
        ldx $03fe ;#8   ; Device = "SD card" (emulation host FS)

        ldy #0   ; Secondary Address = 15: dos command channel
        jsr SETLFS
    
        lda #(DOS_LIST_PARTITIONS_END-DOS_LIST_PARTITIONS) ; command
        ldx #<DOS_LIST_PARTITIONS
        ldy #>DOS_LIST_PARTITIONS
    
        jsr SETNAM
        jsr OPEN
        
        LDX #1
        jsr CHKIN 
        ;skip 8 bytes
        jsr CHRIN
        jsr CHRIN
        jsr CHRIN
        jsr CHRIN
        jsr CHRIN
        jsr CHRIN
        jsr CHRIN
        jsr CHRIN
        
        ldy #0
        ldx #0
        @lp:
            jsr READST
            bne @Goto_Done
            
            jsr CHRIN
            cmp #0
            beq @NewLine
            
            cpx #1
            beq @Goto_CheckStartName

            cpx #2
            beq @Goto_StoreName
            
        jmp @lp
        
        @Goto_CheckStartName:   jmp @CheckStartName
        @Goto_StoreName:        jmp @StoreName
        @Goto_Done:             jmp @Done
        
        @NewLine:
            jsr CHRIN
            jsr CHRIN
            jsr CHRIN
            sta TMP ;partition number
            jsr CHRIN
          ;  jsr print_hex
            ldx #1  ;find next "
        jmp @lp
        
        @CheckStartName:
            cmp #$22
            bne @lp
            inc CNT_PARTITIONS
            
            ldy #0
            ;store device
            lda $03fe
            StoreCurrentAInMemoryMacro
            iny
            ;store partition Number
            lda TMP
            StoreCurrentAInMemoryMacro
            iny
            
            ;store visual device number
            clc
            lda $03fe
            adc #$30
            StoreCurrentAInMemoryMacro
            iny
            lda #COLON
            StoreCurrentAInMemoryMacro
            iny
            
            ;store visual partition number
            clc
            lda TMP
            
            adc #$30
            StoreCurrentAInMemoryMacro
            iny 
            lda #COLON
            StoreCurrentAInMemoryMacro
            iny

            
            ldx #2  ;store name
        jmp @lp
        
        
        @StoreName:
            cmp #$22    ;"
            beq @FinishLine
            StoreCurrentAInMemoryMacro
            iny       
            
        jmp @lp
        
        @FinishLine:
            lda #0
            StoreCurrentAInMemoryMacro
            iny           
            ;add spaces
            @lp2:
                lda #$20
                StoreCurrentAInMemoryMacro
                iny
                cpy SET_RECORD_LENGTH
                bne @lp2
                jsr DecBankPointer
                jsr DecBankPointer
                jsr DecBankPointer
                lda #$41
                StoreCurrentAInMemoryMacro
                jsr IncBankPointer
                jsr IncBankPointer
            

            ldx #0
        jmp @lp
    
    @Done:
    
    jsr CLRCHN
    lda #1
    jsr CLOSE 
    plx
    ply     
rts

Goto_SelectSupportedScreenmode: jmp SelectSupportedScreenmode


start:
   ; rts
    lda $03fe
    pha
        jsr CheckConnectedDevices   ;list available devices
        jsr LoadPartitionsList
    pla
    sta $03fe
    ;begin
    
    ;03fe = address of current device number
    
    

    stz LAST_RDTIM
    ;initialize vera ctrl to use DATA_0
    LDA #$00
    STA $9F25   ;VERA CTRL

    ;check screen mode
    sec
    jsr SCREEN_MODE
    
    sty SCREEN_ROWS
    stx SCREEN_COLS

    cpx #39 ;less then 40 cols
    bmi Goto_SelectSupportedScreenmode
    
    
    ;store current directory in CUR_PATH
    jsr GetStartDirectory   
    jsr StoreStartDirInVRAM

   
    jsr PrintMainScreen2 
 
   
    stz OFFSET_LEFT
    stz OFFSET_RIGHT
    stz iCurrentWindow
    stz PAGE_LEFT
    stz PAGE_RIGHT
    
    


    lda $03fe   ;address of current device number
    cmp #0
    bne @StoreDevice    ;if 0, it is suppoed to be 8
    lda #8
    sta $03fe
    
    @StoreDevice:
    ;lda #9  ;how to dertermine the current device???
    sta DEVICE_CURRENT
    sta DEVICE_LEFT
    sta DEVICE_RIGHT
    sta DEVICE_START

    
    
    
    jsr GetCurrentPartition
    lda PARTITION_CURRENT
    sta PARTITION_START
    
    lda #0                  ;set current window to left window
    sta iCurrentWindow
        lda #$2F   ;init dirs to root \
        sta DIR_LEFT 
        lda #$0
        sta DIR_LEFT+1  ;end with $0           


        jsr ReadWindowVars
        
            jsr LoadCurrentDir2
            jsr ShowFiles2
        jsr StoreWindowVars

    jsr SwitchWindow    ;switch to right window
        lda #$2F   ;init dirs to root \
        sta DIR_RIGHT
        lda #$0
        sta DIR_RIGHT+1     ;end with $0
        
        jsr ReadWindowVars
         
           ; jsr LoadCurrentDir
            ;check if both dirs are the same
            ldy #0
            :
                lda DIR_LEFT,y
                cmp #0
                beq @DoCopy
                cmp DIR_RIGHT,y
                bne DoLoad
                iny
                jmp :-
            @DoCopy:
            CopyMemoryBank RB_FILES_LIST_LEFT,RB_FILES_LIST_RIGHT
            CopyMemoryBank RB_LONGFILENAMES_LEFT,RB_LONGFILENAMES_RIGHT
            lda FILECOUNT_LEFT
            sta FILECOUNT_RIGHT
            sta FILECOUNT_CURRENT
            jmp DoShow
            DoLoad:
            jsr LoadCurrentDir2
            DoShow:
            jsr ShowFiles2
        jsr StoreWindowVars
    jsr SwitchWindow 
    
   ;  jsr RDTIM
   ; jsr print_hex
   ; txa 
   ; jsr print_hex
   ; tya
   ; jsr print_hex
   ; jsr print_lf
       
 ;rts
  ;  lda #0                  ;set current window to left window
  ;  sta iCurrentWindow

      
    clc
    
    jsr HighlightRow
   
        lda #0
        jsr joystick_scan
        lda JOY_NUMBER
        jsr joystick_get
       ; cpy #$ff    ;no joystick present
      ;  bne lp  ;no joystick
  
        
        ;Joystick Present:
        lda #1
        sta JOY_PRESENT
        
   

;jsr CopyAFile
;rts
       
    

    lp:
      ;  clc
      ;  ldx #1
      ;  ldy #0
      ;  jsr PLOT
      ;  lda JOY_PRESENT
        
      ;  jsr print_hex
      
        
        lda JOY_PRESENT
        cmp #1
        bne SkipJoystickHandling
             ;Check SNES controller
            jsr ReadJoystick
           
            lda JOY_PRESSED_A
            cmp #1
            beq Goto_EnterPress2

            lda JOY_PRESSED_DOWN
            cmp #1
            beq Goto_RowDown

            lda JOY_PRESSED_UP
            cmp #1
            beq Goto_RowUp

            lda JOY_PRESSED_RIGHT
            cmp #1
            beq Goto_GotoSwitchWindow

            lda JOY_PRESSED_LEFT
            cmp #1
            beq Goto_GotoSwitchWindow

            lda JOY_PRESSED_SELECT
            cmp #1
            beq Goto_SelectPartition2


            ;---- Actions that cannot performed on select partition mode
            ldx MODE_CURRENT
            cpx #0
            bne @SkipIfNotMode0Joy ;--------------------------------

                lda JOY_PRESSED_B
                cmp #1
                beq Goto_ShowJoyOptions
            @SkipIfNotMode0Joy:
            
            lda JOY_PRESSED_L
            cmp #1
            beq Goto_MinusPress2

            lda JOY_PRESSED_R
            cmp #1
            beq Goto_PlusPress2

        jmp SkipJoystickHandling
            Goto_SelectPartition2:  jmp SelectPartition
            Goto_EnterPress2:       jmp EnterPress
            
        SkipJoystickHandling:
       
        jsr GETIN
        cmp #$0
        beq lp

     
        cmp #$11    ;Down
        beq Goto_RowDown
        
        cmp #$91    ;up
        beq Goto_RowUp
        
        cmp #$09    ;tab
        beq Goto_GotoSwitchWindow

        cmp #$82    ;pageup
        beq Goto_PageUp

        cmp #$02    ;pagedown
        beq Goto_PageDown

        cmp #$13    ;home
        beq Goto_GoHome    

        cmp #$04    ;end
        beq Goto_GoEnd        

        cmp #$0D    ;enter
        beq Goto_EnterPress         
       
       jmp SkipJumpBit
            ;Jump table
            Goto_MinusPress2:       jmp MinusPress
            Goto_PlusPress2:        jmp PlusPress
            Goto_RowUp:             jmp RowUp
            Goto_RowDown:           jmp RowDown
            Goto_PageDown:          jmp PageDown
            Goto_GotoSwitchWindow:  jmp GotoSwitchWindow
            Goto_PageUp:            jmp PageUp                    
            Goto_GoEnd:             jmp GoEnd
            Goto_GoHome:            jmp GoHome
            Goto_EnterPress:        jmp EnterPress
            Goto_CopyCurrentFile:   jmp CopyCurrentFile
            Goto_MoveCurrentFile:   jmp MoveCurrentFile
            Goto_DeleteFile:        jmp DeleteFile
            Goto_FileInfo:          jmp FileInfo
            Goto_Quit:              jmp Quit
            Goto_CreateDirectory:   jmp CreateDirectory
            Goto_RenameFile:        jmp RenameFile
            Goto_EditFile:          jmp EditFile
            Goto_BackspacePress:    jmp BackspacePress
            Goto_ShowJoyOptions:    jmp ShowJoyOptions
            Goto_SelectPartition:   jmp SelectPartition
       SkipJumpBit:


        cmp #$86    ;F3 - switch partition
        beq Goto_SelectPartition  
        
        cmp #$15    ;F10 - quit
        beq Goto_Quit
       
        ;---- Actions that cannot performed on select partition mode
        ldx MODE_CURRENT
        cpx #0
        bne @SkipIfNotMode0 ;--------------------------------
        
            cmp #$14    ;backspace
            beq Goto_BackspacePress

            cmp #$89    ;F2 - file info
            beq Goto_FileInfo 
            cmp #$8a    ;F4 - file edit
            beq Goto_EditFile  
            
            cmp #$87    ;F5 - copy file
            beq Goto_CopyCurrentFile

            cmp #$8B    ;F6 - rename file
            beq Goto_RenameFile

            cmp #$88    ;F7 - move file    ;not supported on SD-card!!!
            beq Goto_MoveCurrentFile

            cmp #$8C    ;F8 - Creade directory
            beq Goto_CreateDirectory


            cmp #$19    ;DEL- delete file
            beq Goto_DeleteFile
        @SkipIfNotMode0:    ;--------------------------------
       
        pha
            jsr $FEC0   ;get keyboard modifiers
            tax ;x contains modifieres
        pla
       
        cpx #04
        bne @nocontrol
            
            cmp #$2B    ;+
            beq Goto_PlusPress

            cmp #$2D    ;-
            beq Goto_MinusPress
        @nocontrol:
        
       
        
    jmp lp       
Goto_Charset:     jsr SetCharset
Goto_ChangeDevice: jmp ChangeDevice
jmp lp
Goto_CharsetISO:
 
    jsr SetCharsetIso
jmp lp

    Goto_LPX:
        jmp lp
    Goto_PlusPress:
        jmp PlusPress

    Goto_MinusPress:
        jmp MinusPress      
   
rts

ChangeDevice:
    sec
    sbc #$30
    sta $03fe
    
    ;reset this side
    
   
        
    sta DEVICE_CURRENT
    
    jsr LowlightRow
    stz OFFSET_CURRENT
    stz ROW_CURRENT
    stz PAGE_CURRENT    
    
    lda #$2F
    sta DIR_CURRENT
    lda #0
    sta DIR_CURRENT+1
    
    jsr LoadCurrentDir2
    jsr ShowFiles2
    jsr HighlightRow    
jmp lp

EditFile:
jmp EditFile_start
    FILENAME_EDIT: .asciiz "[f]"


GotoLP3:
    jmp lp
EditFile_start:

    jsr SetPointerToCurrentOffset   ;memory pointer to current offset  
    jsr AdvancePointerToCurrentRow
   
    ldy #37
    lda (ZP_PTR),y
    cmp #$44    ;directory    
    beq GotoLP3
    
 ;   jsr SwitchToCurrentDevice
 ;   lda DEVICE_CURRENT
  ;  jsr print_hex
  ;  jsr WaitKey
    jsr GotoCurrentDirectory

    CopyAddrUntilZero FILENAME_EDIT,CMD_BUFFER

    
    lda #<CMD_BUFFER
    sta ZP_TEXT_PRT
    lda #>CMD_BUFFER
    sta ZP_TEXT_PRT+1
    
    jsr ReplaceParams   ;replace message params with values      
 

    
    CountUntilZero CMD_BUFFER,TMP

    ;
    ;$02: r0L
    ;$03: r0H
    ;$04: r1L
    ;$05: r1H
    ;$06: r2L
    ;$07: r2H
    ;$08: r3L
    ;$09: r3H
    

    lda #$0F
    jsr CHROUT
    
    lda $01 ; Store current ROM bank on stack
    pha

    
        jsr find_me ; Search ROM banks
        bcs done ; Exit if X16 Edit wasn’t found



        sta $01 ; Set ROM bank
        
        ldx #$15 ; First RAM bank used by the editor
        ldy #$ff ; And last RAM bank
        
        lda #<CMD_BUFFER ; Pointer to file name (LSB)
        sta $02 ; Store in r0L
        lda #>CMD_BUFFER ; Pointer to file name (MSB)
        sta $03 ; Store in r0H
        
        lda TMP ; File name length
        sta $04 ; Store in r1L
 
        lda DEVICE_CURRENT
        sta $08        



        jsr $c006 ; Call entry point
    done:
    pla
    sta $01
    
    
    jsr Repaintscreen

  
       
jmp lp

find_me:
jmp find_me_start
    signature: .byt $58,$31,$36,$45,$44,$49,$54 ; = "X16EDIT"
find_me_start:


    stz $01 ; Prepare searching from ROM bank 0
    ldy #$00

scan:
    lda $fff0,y ; Signature starts at $fff0
    cmp signature,y
    bne next ; Signature didn’t match, check next ROM bank
    iny ; Increase char pointer
    cpy #$07 ; Have we got 7 matching chars? If not, keep looking
    bne scan
    clc ; Set C = 0 as indicator X16 Edit was found
    lda $01 ; Load ROM bank into A
    bra exit
    next:
    ldy #$00 ; Reset char pointer
    inc $01 ; Select next ROM bank
    lda $01
    cmp #$20 ; Have we checked all ROM banks?
    bne scan
    sec ; Set C = 1 as indicator X16 Edit was not found
    
exit:

rts    

ShowJoyOptions:
jmp ShowJoyOptions_start
    JOYSTICK_MSG: .asciiz "#  info#  edit#  copy#  rename#  move#  mkdir#  delete###  point and press a, b to exit  "
    JOYSTICK_CUR_LINE: .byte $0

ShowJoyOptions_start:
    CopyAddrUntilZero JOYSTICK_MSG,MSG_MODAL,TMP

    stz JOYSTICK_CUR_LINE
    
    lda #<MSG_MODAL
    sta ZP_TEXT_PRT
    lda #>MSG_MODAL
    sta ZP_TEXT_PRT+1
    
    
  


   ldy DEFAULT_MODAL_WIDTH     ;width
    ldx #17     ;height
    jsr ShowModal
    jsr ShowModalMsg
    jsr JoyOptionsShowSelect
    
    lpjoy:
        jsr ReadJoystick
        
        lda JOY_PRESSED_B
        cmp #1
        beq @Exit

        lda JOY_PRESSED_DOWN
        cmp #1
        beq JoyOptionsNextLine

        lda JOY_PRESSED_UP
        cmp #1
        beq JoyOptionsPrevLine

        lda JOY_PRESSED_A
        cmp #1
        beq JoyOptionsAPress


    
    jmp lpjoy
@Exit:
    lda #2
    sta TMP ;#2
    jsr LoopThroughModelWindow  ;Restore screen    
jmp lp

JoyOptionsPrevLine:
    lda JOYSTICK_CUR_LINE
    cmp #0
    beq lpjoy
    jsr JoyOptionsHideSelect
    dec JOYSTICK_CUR_LINE
    jsr JoyOptionsShowSelect
jmp lpjoy

JoyOptionsNextLine:
    lda JOYSTICK_CUR_LINE
    cmp #6  ;7 options to choose from
    beq lpjoy
    jsr JoyOptionsHideSelect
    inc JOYSTICK_CUR_LINE
    jsr JoyOptionsShowSelect
jmp lpjoy

JoyOptionsAPress:
    jsr ClearKeyboardBuffer
    lda #2
    sta TMP ;#2
    jsr LoopThroughModelWindow  ;Restore screen       
    
    lda JOYSTICK_CUR_LINE
    cmp #0  ;info
        beq Goto_FileInfo3
    cmp #1  ;edit
        beq Goto_EditFile3
    cmp #2  ;copy
        beq Goto_CopyCurrentFile3
    cmp #3  ;rename
        beq Goto_RenameFile3
    cmp #4  ;move   
        beq Goto_MoveCurrentFile3
    cmp #5  ;mkdir
        beq Goto_CreateDirectory3
    cmp #6  ;delete
        beq Goto_DeleteFile3
jmp lpjoy

Goto_FileInfo3:         jmp FileInfo
Goto_EditFile3:         jmp EditFile
Goto_CopyCurrentFile3:  jmp CopyCurrentFile
Goto_RenameFile3:       jmp RenameFile
Goto_MoveCurrentFile3:  jmp MoveCurrentFile
Goto_CreateDirectory3:  jmp CreateDirectory
Goto_DeleteFile3:       jmp DeleteFile

JoyOptionsShowSelect:

    ldx MODAL_ROW

    clc
    txa
    adc #2
    adc JOYSTICK_CUR_LINE
    tax
    
    lda MODAL_COL ;col
    clc
    adc #3
    tay
    clc
    jsr PLOT 

    ldx #$1F
    ldy #$05
    jsr SetColor
    
    lda #$2A
    jsr CHROUT

    jsr ResetColor
rts

JoyOptionsHideSelect:

    ldx MODAL_ROW
    clc
    txa
    adc #2
    adc JOYSTICK_CUR_LINE
    tax
    
    lda MODAL_COL ;col
    clc
    adc #3
    tay
    clc
    jsr PLOT 

    ldx #$1F
    ldy #$05
    jsr SetColor
    
    lda #$20
    jsr CHROUT

    jsr ResetColor
rts

BackspacePress:


    
    ;loop to end of path. x contains end pos
    ldx #0
    @next:
        lda DIR_CURRENT,x
        cmp #$0
        beq @Loopdone
        inx
    jmp @next
    
    @Loopdone:
    cpx #1
    beq @GotoLp     ;already in root
    
    ;remove last dir
    ;x contains end, so reverseloop to /
    
    dex
    :   ;LoopNext
        lda DIR_CURRENT,x
        cmp #$2F    ;/
        beq @EndFound
        dex
    jmp :-  ;LoopNext
    
    @EndFound:
    
    cpx #0      ;if add root level
    beq @EndPlusone ;leave root /
    
    jmp @NoPlusOne
    
    @EndPlusone:
    inx
    
    @NoPlusOne:
   
    lda #$0
    sta DIR_CURRENT,x
    
    
    jsr LowlightRow
    
    stz OFFSET_CURRENT
    stz ROW_CURRENT
    stz PAGE_CURRENT
    
    jsr LoadCurrentDir
    jsr ShowFiles2
    jsr HighlightRow
    
    jmp lp
@GotoLp:
  
    
jmp lp


File: .byte "/launch.prg"       ;verplaatst naar fm.prg, dus niet druk  maken om locatie
FIleEnd:
LaunchSYS: .asciiz "sys 32781"
CURFILE: .res 100
Goto_ChangeDirDown:   jmp ChangeDirDown
Goto_ChangePartition:   jmp ChangePartition
Goto_NextPage:          jmp NextPage
Goto_PrevPage:          jmp PrevPage

EnterPress:

    jsr GETIN   ;clear keyboard buffer
    
    
    ;;-------------------------------------    
    jsr SetPointerToCurrentOffset   ;memory pointer to current offset  
    jsr AdvancePointerToCurrentRow
   
    ldy #37 ;line type P or D
    lda (ZP_PTR),y

    cmp #$44    ;directory
    beq Goto_ChangeDirDown
    cmp #$41    ;Partitions change
    beq Goto_ChangePartition
    cmp #$3e    ;Goto next page
    beq Goto_NextPage
    cmp #$3c    ;Goto prev page
    beq Goto_PrevPage

    

    CopyAddrUntilZero TSRPRG,CMD_BUFFER,CMD_BUFFER_LENGTH
    lda #<CMD_BUFFER
    sta ZP_TEXT_PRT
    lda #>CMD_BUFFER
    sta ZP_TEXT_PRT+1
    
    jsr ReplaceParams   ;replace message params with values      
        
    jsr IsLaunchable
    
    lda TMP
    cmp #0
    beq GotoLpTmp  ;not launchable
    jmp Doorgaan
    GotoLpTmp:
        jmp lp
    
    Doorgaan:
    ;execute program
    ;load path+fm.prg into vram, to be used by launch.prg
    ;fm.prg should check if path is present, else NOT overwrite!!
    jsr StoreStartDirInVRAM
    ;load current folder in a special place in memory
  ;;  lda #20
   ; sta $00 ;goto membank 20
    
    ;make sure launch is from the programs directory. Should be changed when launch is via menu
    jsr GotoCurrentDirectory
    ;jsr SwitchToCurrentDevice
    ;jsr ChangeToCurrentDirectory
    
    jsr CopyLaunchToMem
   
    jmp SkipLoadLaunch
    
        lda #1   ; Logical Number = 1
        ldx #8   ; Device = "SD card" (emulation host FS)
        ldy #0   ; skip 2 bytes
        jsr SETLFS  
        lda #(FIleEnd-File) ; command
        ldx #<File
        ldy #>File      
        jsr SETNAM
        
        lda #0
        ldx #$01
        ldy #$80
        
        jsr LOAD

    SkipLoadLaunch:
  

    jsr clearscreen
    lda #0
    ldx #0
    ldy #1
    clc
    cld
    cli
    clv
     
     
    CopyAddrUntilZero TSRPRG,CMD_BUFFER
    
 
    lda #<CMD_BUFFER
    sta ZP_TEXT_PRT
    lda #>CMD_BUFFER
    sta ZP_TEXT_PRT+1
    
    jsr ReplaceParams   ;replace message params with values      


    ;Check if is tokenizes Basic
    jsr CheckFileType
   ; jmp  hlt
    lda TSRTYPE
    sta $8010
   ; jsr CHROUT
   ; jsr WaitKey
    
    ldy #0
    @thisloop:
        lda CMD_BUFFER,y
        cmp #0
        beq DoLaunch
      ;  jsr CHROUT
        sta $8011,Y ;8010 contains filetype
        iny
    jmp @thisloop  
    
   ; jmp hlt
    
    DoLaunch:
    ;store last zero
    sta $8011,y
    iny
    lda DEVICE_START
    sta $8011,y ;store deviceno of FM location
    iny
    lda PARTITION_START
    sta $8011,y ;store partition of FM location
    
    ldy #0
    @nxt:
        lda LaunchSYS,y
        cmp #0
        beq @dn
        jsr KEY_POKE
        iny
    jmp @nxt
    @dn:
    lda #$0d
    jsr KEY_POKE
    rts
    jjj:
    jmp jjj
    

GotoLP:
jmp lp



ChangeDirDown:
    ldy #2
    lda (ZP_PTR),y  ;first byte of filename
    cmp #$2E    ;period .
    bne @DoChangeDirDown
    ;change dir up
    jmp BackspacePress

@DoChangeDirDown:
    jsr LowlightRow
    phx
    phy
        ldx #0
        @next:
            lda DIR_CURRENT,x
            cmp #$0    ;check for $0
            beq StartAdding
            inx
        jmp @next
        
        StartAdding:
        
            ldy #2
            
            dex
            lda DIR_CURRENT,x
            inx
            cmp #$2F        ;already trailing /
            beq  @StartAdding
            
            lda #$2F        ;/
            sta DIR_CURRENT,x
            inx
            
            
        @StartAdding:
            
        jsr LoadCurrentFileInBuffer
        ldy #0
        @AddNext:   ;add new dir at the end
            
            
            lda FileInfo_Name,y  ;first byte of filename
            sta DIR_CURRENT,x
            cmp #0
            beq @AddDone
            inx
            iny
           
        jmp @AddNext  
        
           
        @AddDone:
            ;reverse to no space
            @Reverse:
                dex
                lda DIR_CURRENT,x
                cmp #$20
                bne @FoundEndOfPath
            jmp @Reverse
            
            @FoundEndOfPath:
            inx
            lda #$0
            sta DIR_CURRENT,x
        
        
        @Done:
    ply
    plx 
    
    stz OFFSET_CURRENT
    stz ROW_CURRENT
    stz PAGE_CURRENT    
    
    jsr LoadCurrentDir
    jsr ShowFiles2
    jsr HighlightRow
jmp lp

SelectPartition:
    jsr LowlightRow
    stz OFFSET_CURRENT
    stz ROW_CURRENT 
    stz PAGE_CURRENT 

    lda MODE_CURRENT
    cmp #0
    beq @ToPartitionMode
        stz MODE_CURRENT

        jsr ReloadCurrentWindow
        jsr HighlightRow
    jmp lp
    @ToPartitionMode:
        inc MODE_CURRENT
        lda CNT_PARTITIONS
        sta FILECOUNT_CURRENT        
        
        jsr ShowFiles2
        jsr HighlightRow
    jmp lp

GoHome:
    jsr LowlightRow
    stz OFFSET_CURRENT
    stz ROW_CURRENT
    jsr ShowFiles2
    jsr HighlightRow
jmp lp

Gogo_LpXXXXx: jmp lp
GoEnd:
    ldx FILECOUNT_CURRENT
    cpx #0
    beq Gogo_LpXXXXx

    lda ListSize         
    cmp FILECOUNT_CURRENT
    bpl MoveToLastItemOnPage
    
    jsr LowlightRow
    lda FILECOUNT_CURRENT   ;cnt files total
    sbc ListSize            ;minus listsize
   ; sbc #2
    sta OFFSET_CURRENT
    lda ListSize
    sbc #1
    sta ROW_CURRENT
    jsr ShowFiles2
    jsr HighlightRow   
    
    
jmp lp

MoveToLastItemOnPage:
    ;;;;;;TO BE TESTED WITH SMALL DIR
    jsr LowlightRow
    lda FILECOUNT_CURRENT
    sta ROW_CURRENT
    dec ROW_CURRENT    
    jsr HighlightRow
jmp lp

Goto_Lpxx:
    jmp lp
PlusPress:
    sec
    jsr SCREEN_MODE 

    @AddAgain:
        cmp #$09    ;64x25 -> no more
        beq Goto_Lpxx
        clc   
        adc #1
        cmp #$05
        beq @AddAgain
        cmp #$06
        beq @AddAgain
        cmp #$07
        beq @AddAgain

    
    clc 
    jsr SCREEN_MODE    
    jsr Repaintscreen
jmp lp

MinusPress:
    sec
    jsr SCREEN_MODE 
    @MinusAgain:
        cmp #0
        beq Goto_Lpxx
        
        sec   
        sbc #1
        ;unsupported screen modes
        cmp #$05
        beq @MinusAgain
        cmp #$06
        beq @MinusAgain
        cmp #$07
        beq @MinusAgain
    clc 
    jsr SCREEN_MODE    
    jsr Repaintscreen
jmp lp

ShowKey:
    clc
    pha
        ldx #59
        ldy #1
        jsr PLOT
        
        
        jsr print_hex  
 
    pla
jmp lp

GotoSwitchWindow:
    jsr LowlightRow
    jsr SwitchWindow
    jsr HighlightRow
jmp lp

PageUp:

    jsr LowlightRow
    lda ROW_CURRENT
    cmp #0  ;bovenaan
    beq GotoPrevPage
    ;bovenaan plaatsen
    stz ROW_CURRENT
    jsr HighlightRow
    @DoNothing:
jmp lp

GotoPrevPage:
    lda OFFSET_CURRENT
    cmp #0  ;reeds bovenaan
    beq AlreadyOnTop
    
    lda OFFSET_CURRENT
    cmp ListSize
    bmi GotoOffsetZero  ;no full page left
    
    lda OFFSET_CURRENT
    sbc ListSize
    sta OFFSET_CURRENT
    jsr ShowFiles2
    jsr HighlightRow
    
jmp lp

AlreadyOnTop:
    jsr HighlightRow
jmp lp

GotoOffsetZero:
    stz OFFSET_CURRENT
    stz ROW_CURRENT
    jsr ShowFiles2
    jsr HighlightRow
jmp lp

Gogo_LpXXXX: jmp lp

PageDown:
    ldx FILECOUNT_CURRENT
    cpx #0
    beq Gogo_LpXXXX

    jsr LowlightRow
    
    ;check if bottom screen
    ldx ROW_CURRENT
    inx
    cpx ListSize      
    beq GotoNextPage
    
    ;move to bottom of sceeen
    ldx ListSize
    dex
    stx ROW_CURRENT
    
    lda OFFSET_CURRENT
    adc ROW_CURRENT
    adc #2
    cmp FILECOUNT_CURRENT
    bpl GotoEndOfList
      
    jsr HighlightRow

jmp lp

GotoEndOfList:
    lda FILECOUNT_CURRENT
    sbc OFFSET_CURRENT
    sbc #1
    sta ROW_CURRENT
    jsr HighlightRow
jmp lp

GotoNextPage:
    lda OFFSET_CURRENT
    adc ListSize
    dec
    dec
    sta OFFSET_CURRENT
    
    lda #0
    sta ROW_CURRENT
    
    jsr ShowFiles2
    jsr HighlightRow    

jmp lp




Goto_EndOfScreenlist: jmp EndOfScreenlist
Gogo_LpXXX: jmp lp

RowDown:
    ldx FILECOUNT_CURRENT
    cpx #0
    beq Gogo_LpXXX
    
    
    clc

    
    jsr LowlightRow

        inc ROW_CURRENT
        ldx ROW_CURRENT
        cpx ListSize
        beq Goto_EndOfScreenlist    ;row>listsize

        lda OFFSET_CURRENT
        adc ROW_CURRENT
        cmp FILECOUNT_CURRENT
        beq Goto_EndOfScreenlist    


   
    jsr HighlightRow
    @DoNothing:
jmp lp

ReplaceParams:
    jmp ReplaceParams_start
    
    TMPSTRING: .res 255
    
    
ReplaceParams_start:
    jsr LoadFileInfoCurrentFile
   
    
    phx
    phy
        ldy #0
        ldx #0
        
        @next:
            lda (ZP_TEXT_PRT),y
            cmp #0
            beq @GotoReady
            cmp #$5b        ;[
            beq @AddParam
            sta TMPSTRING,x
            inx
            iny
        jmp @next
        @GotoReady:
            jmp @Ready
        @AddParam:

            iny ;next byte
            lda (ZP_TEXT_PRT),y
            cmp #$46    ;F
            beq @InsertFile
            cmp #$50    ;P
            beq @Goto_InsertOtherPath   
            cmp #$52    ;R
            beq @Goto_InsertOtherPathNoRoot   
                     
            cmp #$43    ;C
            beq @Goto_InsertCurrentPath            
            cmp #$53    ;S  -> current dir, but if root no /
            beq @Goto_InsertCurrentPathNoRoot

            cmp #$49    ;i
            beq @Goto_InsertLastInput            

            cmp #$59    ;y=type
            beq @Goto_InsertType    

            cmp #$5a    ;z=size
            beq @Goto_InsertSize    

            cmp #$44    ;d=date
            beq @Goto_InsertDate    

            cmp #$54    ;t=time
            beq @Goto_InsertTime    

            cmp #$31    ;1=current partition
            beq @Goto_InsertCurrentPartition

            cmp #$32    ;2=other partition
            beq @Goto_InsertOtherPartition
                

            
            ;t=time
            ;d=date
            ;z=size
                    

        jmp @next
        @Goto_InsertOtherPath:          jmp @InsertOtherPath
        @Goto_InsertCurrentPathNoRoot:  jmp @InsertCurrentPathNoRoot
        @Goto_InsertCurrentPath:        jmp @InsertCurrentPath
        @Goto_InsertLastInput:          jmp @InsertLastInput
        @Goto_InsertType:               jmp @InsertType
        @Goto_InsertOtherPathNoRoot:    jmp @InsertOtherPathNoRoot
        @Goto_InsertSize:               jmp @InsertSize
        @Goto_InsertDate:               jmp @InsertDate
        @Goto_InsertTime:               jmp @InsertTime
        @Goto_InsertCurrentPartition:   jmp @InsertCurrentPartition
        @Goto_InsertOtherPartition:     jmp @InsertOtherPartition
            
        @InsertFile:
            

            phy
                jsr LoadCurrentFileInBuffer
            
                ldy #0
                @NextChar:
                    ;lda (ZP_PTR)
                    lda FileInfo_Name,y
                    cmp #0
                    beq @AddFileDone
                    sta TMPSTRING,x
                    jsr IncBankPointer
                    inx 
                    iny
                   ; cpy #12 ;file is 12 chars long
                    beq @AddFileDone
                jmp @NextChar
                @AddFileDone:
            ply

            iny ;closing ]
            iny ;next char
        jmp @next

        @InsertType:
            phy
                ldy #0
                :
                    lda FileInfo_Type,y
                    sta TMPSTRING,x
                    inx 
                    iny
                    cpy #4
                    bne :-
                
            
            ply
           

            iny ;closing ]
            iny ;next char
        jmp @next

        @InsertSize:
            phy
                ldy #0
                :
                    lda FileInfo_Size,y
                    cmp #$20
                    beq @Skip
                    sta TMPSTRING,x
                    inx 
                    @Skip:
                    iny
                    cpy #8
                    bne :-
                
            
            ply
           

            iny ;closing ]
            iny ;next char
        jmp @next

        @InsertDate:
            phy
                ldy #0
                :
                    lda FileInfo_Date,y
                    sta TMPSTRING,x
                    inx 
                    iny
                    cpy #10
                    bne :-
                
            
            ply
           

            iny ;closing ]
            iny ;next char
        jmp @next

        @InsertTime:
            phy
                ldy #0
                :
                    lda FileInfo_Time,y
                    sta TMPSTRING,x
                    inx 
                    iny
                    cpy #5
                    bne :-
                
            
            ply
           

            iny ;closing ]
            iny ;next char
        jmp @next

        
        @InsertOtherPath:

            
            phy
                    phy
                    phx
                    jsr SwitchWindow    ;overkill, but it works
                    plx
                    ply
                    
                        ldy #0
                        @nextpathcharcx:
                            lda DIR_CURRENT,y
                            cmp #0
                            beq @addpathreadycx
                            sta TMPSTRING,x
                            sta TMP2
                            inx
                            iny
                            
                        jmp @nextpathcharcx
                        @addpathreadycx:                   
                                      
                    phy
                    phx
                    jsr SwitchWindow    ;overkill, but it works
                    plx
                    ply
            ply

            ;insert other path
            iny ;closing ]
            iny ;next char    
        jmp @next

        @InsertOtherPathNoRoot:

            
            phy
                    phy
                    phx
                    jsr SwitchWindow    ;overkill, but it works
                    plx
                    ply
                    
                        ldy #0
                        @nextpathcharcx2:
                            lda DIR_CURRENT,y
                            cmp #0
                            beq @addpathreadycx2
                            sta TMPSTRING,x
                            sta TMP2
                            inx
                            iny
                            
                        jmp @nextpathcharcx2
                        @addpathreadycx2:                   
                            lda TMP2
                            cmp #$2f
                            bne @Cont2
                            dex
                        @Cont2:                  
                    phy
                    phx
                    jsr SwitchWindow    ;overkill, but it works
                    plx
                    ply
            ply

            ;insert other path
            iny ;closing ]
            iny ;next char    
        jmp @next

        @InsertCurrentPartition:
            phy
                
                    lda PARTITION_CURRENT
                    clc
                    adc #$30
                    sta TMPSTRING,x
                    inx

            ply

            ;insert other path
            iny ;closing ]
            iny ;next char           
        jmp @next

        @InsertOtherPartition:
            phy
                
                    lda PARTITION_OTHER
                    clc
                    adc #$30
                    sta TMPSTRING,x
                    inx

            ply

            ;insert other path
            iny ;closing ]
            iny ;next char           
        jmp @next

        
        @InsertLastInput:
            phy
                    
                    ldy #0
                    @nextpathchard:
                        lda InputBuffer,y
                        cmp #0
                        beq @addpathreadyc
                        sta TMPSTRING,x
                        inx
                        iny
                        
                    jmp @nextpathchard
                    @addpathreadyd:

            ply

            ;insert other path
            iny ;closing ]
            iny ;next char           
        jmp @next
        
         @InsertCurrentPath:

            
            phy
                    
                    ldy #0
                    @nextpathcharc:
                        lda DIR_CURRENT,y
                        cmp #0
                        beq @addpathreadyc
                        sta TMPSTRING,x
                        sta TMP2
                        inx
                        iny
                        
                    jmp @nextpathcharc
                    @addpathreadyc:
  
            ply

         
            iny ;closing ]
            iny ;next char    
        jmp @next       
 
        @InsertCurrentPathNoRoot:

            
            phy
                    
                    ldy #0
                    @nextpathchard2:
                        lda DIR_CURRENT,y
                        cmp #0
                        beq @addpathreadyd2
                        sta TMPSTRING,x
                        sta TMP2
                        inx
                        iny
                        
                    jmp @nextpathchard2
                    @addpathreadyd2:
                    lda TMP2
                    cmp #$2f
                    bne @Cont
                        dex
                    @Cont:
            ply

         
            iny ;closing ]
            iny ;next char    
        jmp @next 
                
        @Ready:
        lda #0
        sta TMPSTRING,x ;end with $0
    
    
    
   
    
        ;TEMPSTRING back into  MSG_MODAL
        ldy #0
        @nextcopy:
            lda TMPSTRING,y
            cmp #0
            beq @copyready
            sta (ZP_TEXT_PRT),y
            iny
        jmp @nextcopy
        @copyready:
        lda #0
        sta (ZP_TEXT_PRT),y
    
    ply   
    plx
    
    

rts

LoadCurrentFileSize:

    pha
        jsr SetRamBank
        jsr SetPointerToCurrentOffset   ;memory pointer to current offset  
        jsr AdvancePointerToCurrentRow
        
        jsr InitAdder24bit
        
        ;4 blocks= 1 kb
        lda (ZP_PTR)
        sta Adder24bitToAdd ;PB_BLOCKS
        jsr IncBankPointer
        lda (ZP_PTR)
        sta Adder24bitToAdd+1 ;PB_BLOCKS+1
        jsr Adder24bit
        jsr Adder24bit
        jsr Adder24bit
        jsr Adder24bit
        
        lda Adder24bitValue
        sta PB_BLOCKS

        lda Adder24bitValue+1
        sta PB_BLOCKS+1        
        
        
    pla

rts

SelectSupportedScreenmode:
jmp SelectSupportedScreenmode_start
  SCREEN_MODE_MSG:  .byte $0d,"your current  screenmode is not supported. please choose one of these:",$0d,$0d,$0d
                    .byte " a. mode 0: 80x60",$0d,$0d
                    .byte " b. mode 1: 80x30",$0d,$0d
                    .byte " c. mode 2: 40x60",$0d,$0d
                    .byte " d. mode 3: 40x30",$0d,$0d
                    .byte " e. mode 4: 40x15",$0d,$0d
                    .byte " f. mode 8: 64x50",$0d,$0d
                    .byte " g. mode 9: 64x25",$0d,$0d
                    .byte $0
                    
              
SelectSupportedScreenmode_start:
  PrintUntilZero SCREEN_MODE_MSG
  @lp:
        jsr GETIN
        cmp #$41    ;a
        beq @ToMode0

        cmp #$42    ;b
        beq @ToMode1
        cmp #$43    ;c
        beq @ToMode2
        cmp #$44    ;d
        beq @ToMode3
        cmp #$45    ;e
        beq @ToMode4
        cmp #$46    ;f
        beq @ToMode8
        cmp #$47    ;g
        beq @ToMode9

    
    jmp @lp
    
    @ToMode0:
        clc
        lda #0
        jsr SCREEN_MODE
        jmp start

    @ToMode1:
        clc
        lda #1
        jsr SCREEN_MODE
        jmp start
    @ToMode2:
        clc
        lda #2
        jsr SCREEN_MODE
        jmp start
    @ToMode3:
        clc
        lda #3
        jsr SCREEN_MODE
        jmp start
    @ToMode4:
        clc
        lda #4
        jsr SCREEN_MODE
        jmp start
    @ToMode8:
        clc
        lda #8
        jsr SCREEN_MODE
        jmp start     
    @ToMode9:
        clc
        lda #9
        jsr SCREEN_MODE
        jmp start    


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

LoadFileInfoCurrentFile:
jmp LoadFileInfoCurrentFile_start
    FileInfo_Name: .res 255
    FileInfo_Size: .byte "         "
    FileInfo_Date: .res 10
    FileInfo_Time: .res 5
    FileInfo_Type: .res 4
    FileInfoDir: .byte "dir "
    FileInfoFile: .byte "file"
    FileInfoUnk: .byte "unkn"
    FileInfoBlank: .byte "        "

LoadFileInfoCurrentFile_start:

    
    jsr LoadCurrentFileInBuffer

    pha
    phx
    phy
        
       
        jsr SetRamBank
        jsr SetPointerToCurrentOffset   ;memory pointer to current offset  
        jsr AdvancePointerToCurrentRow
        
 
        ;skip 2 bytes = blocksize
        jsr IncBankPointer
        jsr IncBankPointer        
        
        ;skip 12 bytes for filename
        jsr IncBankPointer        
        jsr IncBankPointer        
        jsr IncBankPointer        
        jsr IncBankPointer        
        jsr IncBankPointer        
        jsr IncBankPointer        
        jsr IncBankPointer        
        jsr IncBankPointer        
        jsr IncBankPointer        
        jsr IncBankPointer        
        jsr IncBankPointer        
        jsr IncBankPointer        
 

        CopyFileInfoIntoVar FileInfo_Size,8
        CopyFileInfoIntoVar FileInfo_Date,10
        CopyFileInfoIntoVar FileInfo_Time,5
        lda (ZP_PTR)
        cmp #$44    ;directory
        beq @AddDir
        cmp #$50
        beq @AddFile
        
        @AddUnknown:
            CopyMem FileInfoUnk,FileInfo_Type,4
        jmp @exit
       
        @AddDir:
            CopyMem FileInfoDir,FileInfo_Type,4
            CopyMem FileInfoBlank,FileInfo_Size,8
        jmp @exit        

        @AddFile:
            CopyMem FileInfoFile,FileInfo_Type,4
        jmp @exit        
        
        @exit:
    ply
    plx
    pla


rts

LoadCurrentFileInBuffer:
    phx
    phy
    pha
        jsr SetRamBank
        jsr SetPointerToCurrentOffset   ;memory pointer to current offset  
        jsr AdvancePointerToCurrentRow
    

        
        
        ;load pointer
        stz TMP
        ldy #38
        lda (ZP_PTR),y  ;get first byte of pointer to long filename
        sta TMP ;load in TMP also to check if there is a LFN pointer
        sta ZP_PTR2

    
        iny
        
        lda (ZP_PTR),y  ;get second byte of poinrt to long filename
        sta ZP_PTR2+1

        
        ora TMP ;(ZP_PTR),y  ;OR both bytes together
        cmp #0
        bne @LoadLongFileName 
        jsr IncBankPointer  ;skip 2 bytes for file size
        jsr IncBankPointer    
        ;short filename
     
            ldy #0
            @NextChar:
                lda (ZP_PTR)
                cmp #0
                beq @AddFileDone
                sta LFNBufferTmp,y
                sta FileInfo_Name,y
                jsr IncBankPointer
                iny
                cpy #12 ;file is 12 chars long
                beq @AddFileDone
            jmp @NextChar
            @AddFileDone:   
            lda #0
            sta LFNBufferTmp,y
            sta FileInfo_Name,y
            jmp @SRDone
        
        @LoadLongFileName:
       
        jsr SetRambankLongFileNames
        
        ldy #0
        @Next:
            lda (ZP_PTR2),y
            cmp #0
            beq @Done
            sta LFNBufferTmp,y
            sta FileInfo_Name,y
            iny
            jmp @Next
        @Done:
      
        sta LFNBufferTmp,y
        sta FileInfo_Name,y
    @SRDone:
    pla
    ply
    plx
rts

Gogo_LpXXXXXX: jmp lp

FileInfo:
jmp FileInfo_Start
    
    FileInfo_Msg:   .byte "#name: [f]##"
                    .byte "type: [y]##"
                    .byte "size: [z]##"
                    .byte "date: [d]##"
                    .byte "time: [t]##"
                    .byte $0
                    
                    
FileInfo_Start:
    ldx FILECOUNT_CURRENT
    cpx #0
    beq Gogo_LpXXXXXX

   
  ;  jsr LoadCurrentFileInBuffer
    CopyAddrUntilZero FileInfo_Msg,MSG_MODAL,TMP

    
    lda #<MSG_MODAL
    sta ZP_TEXT_PRT
    lda #>MSG_MODAL
    sta ZP_TEXT_PRT+1
    
     jsr ReplaceParams    
    
    jsr ShowConfirm
    
   

jmp lp

ChangeToCurrentDirectory:
    jmp ChangeToCurrentDirectoty_start
    CMD_CD_CURRENT: .asciiz "cd:[c]"
    ChangeToCurrentDirectoty_start:
    
    CopyAddrUntilZero CMD_CD_CURRENT,CMD_BUFFER,CMD_BUFFER_LENGTH
     lda #<CMD_BUFFER
    sta ZP_TEXT_PRT
    lda #>CMD_BUFFER
    sta ZP_TEXT_PRT+1
    
    jsr ReplaceParams  
    stz CMD_BUFFER_LENGTH
    jsr DoDosCMD
rts
Goto_LPXXC: jmp lp
RenameFile:
    jmp RenameFile_start

    Msg_Rename: .asciiz "#enter new name:#######<enter> to proceed   <esc> to cancel"    
    CMD_RENAME: .asciiz "r:[i]=[f]"
    
RenameFile_start:
    ldx FILECOUNT_CURRENT
    cpx #0
    beq Goto_LPXXC
    
    jsr LoadCurrentFileInBuffer
    lda FileInfo_Name
    cmp #$2e    ;.
    beq Goto_LPXXC ;don't delete .. (updir)


   CopyAddrUntilZero Msg_Rename,MSG_MODAL
    ldx #28
    ldy #30
    clc
    jsr PLOT
    
    lda #15
    sta InputMaxLength
    
    jsr ShowGetInput
     
    lda #2
    sta TMP ;#2
    jsr LoopThroughModelWindow  ;Restore screen   

    lda InputLength
    cmp #0
    beq @Goto_GotoLp ;zero length
    jmp @Jmp
    
    @Goto_GotoLp:
        jmp @GotoLp
    
    @Jmp: 
    ;CD to current directory
   ; CopyAddrUntilZero CD_ROOT,CMD_BUFFER,CMD_BUFFER_LENGTH
   ; jsr DoDosCMD
    jsr GotoCurrentDirectory
   ; jsr WaitKey
    
    CopyAddrUntilZero CMD_RENAME,CMD_BUFFER

   
 
     lda #<CMD_BUFFER
    sta ZP_TEXT_PRT
    lda #>CMD_BUFFER
    sta ZP_TEXT_PRT+1
    
    jsr ReplaceParams   ;replace message params with values   
    
    
    stz CMD_BUFFER_LENGTH
    jsr DoDosCMD
   ; jsr WaitKey
    jsr ReloadWindows
    
@GotoLp:


jmp lp

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
CreateDirectory:
    jmp CreateDirectory_start
    
    Msg_CreateDirectory: .asciiz "#enter name for new directory:#######<enter> to proceed   <esc> to cancel"    
    CMD_MD: .asciiz "md:"
    
CreateDirectory_start:
    
    CopyAddrUntilZero Msg_CreateDirectory,MSG_MODAL
    ldx #28
    ldy #30
    clc
    jsr PLOT
    
    lda #15
    sta InputMaxLength
    
    jsr ShowGetInput
     
    lda #2
    sta TMP ;#2
    jsr LoopThroughModelWindow  ;Restore screen   

    lda InputLength
    cmp #0
    beq @GotoLp ;zero length
    
    jsr GotoCurrentDirectory
    CopyAddrUntilZero CMD_MD,CMD_BUFFER

    ldx #0
    ldy #3
    @next:
        lda InputBuffer,x
        sta CMD_BUFFER,y
        inx
        iny
        cpx InputLength
        beq @Ready
    jmp @next
    
    @Ready:
    lda #0
    sta CMD_BUFFER,y
    stz CMD_BUFFER_LENGTH
    
    
    
    jsr DoDosCMD
  ;  PrintUntilZero CMD_BUFFER_RESULT
  ;  jsr WaitKey
    
    jsr ReloadWindows
    
@GotoLp:

jmp lp

ShowGetInput:
    ldy DEFAULT_MODAL_WIDTH     ;width
    ldx #15     ;height
    
    jsr ShowModal
    jsr ShowModalMsg
    ldx MODAL_ROW
    inx
    inx
    inx
    inx
    inx
    inx
        
    lda MODAL_COL ;col
    clc
    adc #3
    tay
    clc
    jsr PLOT
    jsr GetInput 
    
rts
Goto_LPXX: jmp lp

DeleteFile:
    jmp DeleteFile_start
    Msg_DeleteFile: .asciiz "##are you sure you want to delete:###[f]####<enter> to proceed   <esc> to cancel"

    Template_Delete_File: .asciiz "s:[f]"
    Template_Delete_Dir: .asciiz "rd:[f]"

DeleteFile_start:
    ;check if dir
 
    jsr LoadCurrentFileInBuffer
    lda FileInfo_Name
    cmp #$2e    ;.
    beq Goto_LPXX ;don't delete .. (updir)
     
   
    
    CopyAddrUntilZero Msg_DeleteFile,MSG_MODAL

  
    
    
    lda #<MSG_MODAL
    sta ZP_TEXT_PRT
    lda #>MSG_MODAL
    sta ZP_TEXT_PRT+1
    
    jsr ReplaceParams   ;replace message params with values
    
    jsr ShowConfirm

    lda RESULT_KEY
    cmp #$0D     ;ENTER pressed
    bne  GotoEnd
    
    ;do delete
  
     jsr LowlightRow
  
  jsr GotoCurrentDirectory
   ; jsr ChangeToCurrentDirectory
    
    jsr SetPointerToCurrentOffset   ;memory pointer to current offset  
    jsr AdvancePointerToCurrentRow
  
   
    CopyAddrUntilZero Template_Delete_File,CMD_BUFFER

    ldy #37
    lda (ZP_PTR),y
    cmp #$44    ;directory    
    bne @Resume ;is not a directory
    CopyAddrUntilZero Template_Delete_Dir,CMD_BUFFER    
    
    @Resume:     
    lda #<CMD_BUFFER
    sta ZP_TEXT_PRT
    lda #>CMD_BUFFER
    sta ZP_TEXT_PRT+1
    
    jsr ReplaceParams   ;replace message params with values   
    stz CMD_BUFFER_LENGTH
    jsr DoDosCMD

    jsr ReloadWindows
    
GotoEnd:   
jmp lp

ReloadCurrentWindow:
    stz OFFSET_CURRENT
    stz ROW_CURRENT
   ; stz PAGE_CURRENT
    
    jsr LowlightRow
    jsr LoadCurrentDir
    jsr ShowFiles2

rts

ChangePartition:
   
    jsr SetPointerToCurrentOffset   ;memory pointer to current offset  
    jsr AdvancePointerToCurrentRow
    lda (ZP_PTR)    ;device
    sta $03fe
    sta DEVICE_CURRENT
    jsr IncBankPointer
    lda (ZP_PTR)
    sta PARTITION_CURRENT
    stz MODE_CURRENT    ;back to normal operation mode

    lda #$2F
    sta DIR_CURRENT
    lda #0
    sta DIR_CURRENT+1    
    
    jsr LowlightRow
    jsr ReloadCurrentWindow
    jsr HighlightRow
    
    
jmp lp

ReloadWindows:
    ;Make sure current selected row IS a file or DIR
    jsr LowlightRow
    clc
    lda OFFSET_CURRENT
    adc ROW_CURRENT
    adc #1
    cmp FILECOUNT_CURRENT
    bne @DoReload
   
    lda ROW_CURRENT
    cmp #0
    beq @DoReload   ;top of screen
    dec ROW_CURRENT
    jmp @DoReload

    @DoReload:

    jsr LowlightRow
    jsr LoadCurrentDir
    jsr ShowFiles2
    jsr SwitchWindow
    
    ;other side as well
    clc
    lda OFFSET_CURRENT
    adc ROW_CURRENT
    adc #1
    cmp FILECOUNT_CURRENT
    bne @DoReloadB
   
    lda ROW_CURRENT
    cmp #0
    beq @DoReloadB   ;top of screen
    dec ROW_CURRENT
    jmp @DoReloadB

    @DoReloadB:   
    
    jsr LoadCurrentDir
    jsr ShowFiles2
    jsr SwitchWindow
    jsr HighlightRow   
rts

CopyCurrentFile:
    jmp CopyCurrentFile_start
    
    Msg_CopyFile: .asciiz "##you are about to copy##file:   [f] ##to dir: [p]####<enter> to proceed   <esc> to cancel"  ;# is linefeed 
    Msg_Copying: .asciiz "####copying, please wait..."
    Msg_FileExists: .asciiz "        file already exists!        #        proceed to overwrite        "
    TemplateCopySource: .asciiz "[s]/[f]"   ;c=current directory
    
    TemplateCopyDest: .asciiz "@:[r]/[f],s,w"   ;p=directory of other window

GotoLP2:
    jmp lp
CopyCurrentFile_start:
    ldx FILECOUNT_CURRENT
    cpx #0
    beq GotoLP2

    ;check if is not dir
    ;;-------------------------------------    
    jsr SetPointerToCurrentOffset   ;memory pointer to current offset  
    jsr AdvancePointerToCurrentRow
   
    ldy #37
    lda (ZP_PTR),y
    cmp #$44    ;directory    
    beq GotoLP2
    
    CopyAddrUntilZero Msg_CopyFile,MSG_MODAL
    lda #<MSG_MODAL
    sta ZP_TEXT_PRT
    lda #>MSG_MODAL
    sta ZP_TEXT_PRT+1
    
   jsr ReplaceParams   ;replace message params with values
 
   ;Check if file exists
        ;change cur dir to other directory
        jsr SwitchToOtherDevice
            CopyAddrUntilZero DOS_CURDIROTHER,CMD_BUFFER,CMD_BUFFER_LENGTH
            lda #<CMD_BUFFER
            sta ZP_TEXT_PRT
            lda #>CMD_BUFFER
            sta ZP_TEXT_PRT+1
            jsr ReplaceParams 
            stz CMD_BUFFER_LENGTH
            inc DOS_CMD_DEVICE  ;use other device
            jsr DoDosCMD
          ;  PrintUntilZero CMD_BUFFER_RESULT
           ; jsr WaitKey

            CopyAddrUntilZero DOS_FILEEXISTS,CMD_BUFFER
            lda #<CMD_BUFFER
            sta ZP_TEXT_PRT
            lda #>CMD_BUFFER
            sta ZP_TEXT_PRT+1

            jsr ReplaceParams   ;replace message params with values  
            
            jsr FileExists

        jsr SwitchToCurrentDevice
    
    lda CHECKFILE_RESULT
    cmp #0
    beq DoCopy
    ;else add warning
  

    CountUntilZero MSG_MODAL,CNT
    ldy CNT
   ; iny
    ;add warning
    ;add 2 line feeds
    lda #$23
    sta MSG_MODAL,y
    iny
    sta MSG_MODAL,y
    iny
    
    lda #$1C
    sta MSG_MODAL,y
    iny
    lda #$01
    sta MSG_MODAL,y
    iny
    lda #$05
    sta MSG_MODAL,y
    iny
    ;copy warning in confirm message
    ldx #0
    @msgnxt:
        lda Msg_FileExists,x
        cmp #0
        beq @CopyDone
        sta MSG_MODAL,y
        iny
        inx
        jmp @msgnxt
    
    @CopyDone:


        lda #0
        sta MSG_MODAL,y    
    
    DoCopy:
   
 
    

    jsr ShowConfirm
    jsr ResetColor
    
    lda RESULT_KEY
    cmp #$0D     ;ENTER pressed
    beq PrepareCopyParams
    
@GotoLp:
jmp lp

PrepareCopyParams:
    CopyAddrUntilZero Msg_Copying,MSG_MODAL
    ldy DEFAULT_MODAL_WIDTH     ;width
    ldx #15     ;height
    jsr ShowModal
    
    jsr ShowModalMsg
         
    lda #<MSG_MODAL
    sta ZP_TEXT_PRT
    lda #>MSG_MODAL
    sta ZP_TEXT_PRT+1


    CopyAddrUntilZero TemplateCopySource,SourceFile
    CopyAddrUntilZero TemplateCopyDest,DestFile

   
    lda #<SourceFile
    sta ZP_TEXT_PRT
    lda #>SourceFile
    sta ZP_TEXT_PRT+1
    jsr ReplaceParams   ;replace message params with values

    
    lda #<DestFile
    sta ZP_TEXT_PRT
    lda #>DestFile
    sta ZP_TEXT_PRT+1
    jsr ReplaceParams   ;replace message params with values
 
  ;  PrintUntilZero SourceFile
   ; jsr print_lf
  ;  PrintUntilZero DestFile
  ;  jsr WaitKey
 
  
    jsr CopyAFile

    ldy DEFAULT_MODAL_WIDTH     ;width
    ldx #15     ;height

    lda #<MODAL_SCREEN_BACKUP
    sta ZP_PTR
    lda #>MODAL_SCREEN_BACKUP
    sta ZP_PTR+1

    lda #2
    sta TMP ;#2
    jsr LoopThroughModelWindow  ;Restore screen

    jsr ReloadWindows
     
jmp lp

Quit:
    jmp Quit_Start
    
    Msg_Quit: .asciiz "##are you sure you want to quit##and return to basic?####<enter> to proceed   <esc> to cancel"
    
    Quit_Start:
    CopyAddrUntilZero Msg_Quit,MSG_MODAL
    
    jsr ShowConfirm
    
    lda RESULT_KEY
    cmp #$0D     ;ENTER pressed
    bne @GotoLp
    
    jsr clearscreen
    
    
    rts;return to basic
    
@GotoLp:
jmp lp


Goto_LPXXCXX: jmp lp
MoveCurrentFile:
    jmp MoveCurrentFile_start
    
    Msg_MoveFile: .asciiz "##you are about to move##file:   [f] ##to dir: [p]####<enter> to proceed   <esc> to cancel"  ;# is linefeed 
    
 
    CMD_MOVE: .asciiz "r:[r]/[f]=[s]/[f]"
 
    
MoveCurrentFile_start:
    ldx FILECOUNT_CURRENT
    cpx #0
    beq Goto_LPXXCXX
    
    ;check if is not dir
    ;;-------------------------------------    
    jsr SetPointerToCurrentOffset   ;memory pointer to current offset  
    jsr AdvancePointerToCurrentRow
   
    ldy #37
    lda (ZP_PTR),y
    cmp #$44    ;directory    
    beq @GotoLp ;DIRECTORY CANT BE MOVED
    
    CopyAddrUntilZero Msg_MoveFile,MSG_MODAL
      
    lda #<MSG_MODAL
    sta ZP_TEXT_PRT
    lda #>MSG_MODAL
    sta ZP_TEXT_PRT+1
    
   jsr ReplaceParams   ;replace message params with values
    
    lda #<CMD_BUFFER
    sta ZP_TEXT_PRT
    lda #>CMD_BUFFER
    sta ZP_TEXT_PRT+1
    
   jsr ReplaceParams   ;replace message params with values

    jsr ShowConfirm
    
    lda RESULT_KEY
    cmp #$0D     ;ENTER pressed
    beq PerformMove
    
@GotoLp:
jmp lp

PerformMove:
    ;change cur dir to root
    CopyAddrUntilZero CD_ROOT,CMD_BUFFER,CMD_BUFFER_LENGTH
    jsr DoDosCMD
    
    CopyAddrUntilZero CMD_MOVE,CMD_BUFFER
    lda #<CMD_BUFFER
    sta ZP_TEXT_PRT
    lda #>CMD_BUFFER
    sta ZP_TEXT_PRT+1
    
    jsr ReplaceParams   ;replace message params with values  
    stz CMD_BUFFER_LENGTH   ;function will calculate itself
    
    jsr DoDosCMD


    jsr ReloadWindows 
     
jmp lp

ShowModalMsg:
    ldx #$1F
    ldy #$05
    jsr SetColor
    
    ldx MODAL_ROW
    ldy MODAL_COL
   
    lda MODAL_COL
 
  
    
    inx
    iny
    iny
    
    stx VERA_PLOT_X
    sty VERA_PLOT_Y
        
    clc
    ;jsr PLOTVera
    jsr PLOT
    
    phx
    ldx #0
        @next:
            lda MSG_MODAL,x
            cmp #0
            beq @ready
            cmp #$23   ;# --> goto next line
            beq @LineFeed
            
           ; jsr PrintAToVera
            jsr CHROUT
            inx
            jmp @next
            
            @LineFeed:
                
                phx
                phy 
                    
                    ldx VERA_PLOT_X ;current row
                    inx             ;next line
                    ldy VERA_PLOT_Y ;last Y (start of line)
                    ;jsr PLOTVera
                    clc
                    jsr PLOT
                    stx VERA_PLOT_X ;store new current row

                ply
                plx
               inx
            jmp @next
            
        @ready:
    plx
    jsr ResetColor
rts

ShowConfirm:
    jmp ShowConfirm_start
    
    RESULT_KEY: .byte $0
    
ShowConfirm_start:
    ;max width=50
    ;max height=20
    ldy DEFAULT_MODAL_WIDTH     ;width
    ldx #17     ;height
    jsr ShowModal
    
    jsr ShowModalMsg

    @WaitKey:
        jsr ReadJoystick
        
        lda JOY_PRESSED_A
        cmp #1
        beq @Confirm

        lda JOY_PRESSED_B
        cmp #1
        beq @Esc
        
        jsr GETIN
        
        cmp #$1B    ;ESC
        beq @StoreKeyInResult
        cmp #$0D    ;ENTER
        beq @StoreKeyInResult
    
    jmp @WaitKey

    @StoreKeyInResult:
        sta RESULT_KEY
        jmp @CloseModal
    
    @Confirm:
        lda #$0D
        sta RESULT_KEY
    jmp @CloseModal
    
    @Esc:
         lda #$1B
        sta RESULT_KEY          
    @CloseModal:
    lda #2
    sta TMP ;#2
    jsr LoopThroughModelWindow  ;Restore screen
rts

ShowModal:
jmp ShowModal_start
    MSG_MODAL: .res 255

ShowModal_start:
    ;Rij
    sty MODAL_WIDTH
    stx MODAL_HEIGHT
    

    
    lda SCREEN_ROWS
    clc
    sbc MODAL_HEIGHT
    lsr ;devide bt 2
    sta MODAL_ROW

   
    
    ;Kolom
    
    lda SCREEN_COLS
    sec
    sbc MODAL_WIDTH
       
    cmp #0
    beq @NoDivide
    clc
    lsr ;devide bt 2    
    @NoDivide:


    sta MODAL_COL

    
    




    stz TMP ;0=read bytes and store
   
    lda #<MODAL_SCREEN_BACKUP
    sta ZP_PTR
    lda #>MODAL_SCREEN_BACKUP
    sta ZP_PTR+1
    jsr LoopThroughModelWindow  ;Read from vram
    
    inc TMP
    jsr LoopThroughModelWindow  ;ShowModal


    ;reset pointer
    lda #<MODAL_SCREEN_BACKUP
    sta ZP_PTR
    lda #>MODAL_SCREEN_BACKUP
    sta ZP_PTR+1


rts

LoopThroughModelWindow:
    stz MODAL_ROW_COUNT
    stz MODAL_COL_COUNT

    ldx MODAL_ROW
    @nextrow:
        ldy MODAL_COL   ;reset column position

        jsr PLOTVeraSingle  ;1 step, so first STA is char next STA is color
        
        stz MODAL_COL_COUNT   ;reset col-count
        
        @nextcol:
            lda TMP
            cmp #0  ;read bytes from VRAM
            beq @ReadFromVRAM
            cmp #1  ;show modal
            beq @WriteToVRAM
            cmp #2  ;restor screen
            beq @RestoreScreen
            
            jmp @Ready
            
            @WriteToVRAM:
                lda #$20
                STA $9F23

                lda #$16    ;color: Foreground: White - Background: Blue
                STA $9F23            
            jmp @Ready
            
            @RestoreScreen:
                lda (ZP_PTR)
                STA $9F23
                jsr IncBankPointer

                lda (ZP_PTR)
                STA $9F23
                jsr IncBankPointer

            jmp @Ready
            
            @ReadFromVRAM:
                lda $9F23
                sta (ZP_PTR)
                jsr IncBankPointer  ;increase ZP_PTR by one
            
                lda $9F23
                sta (ZP_PTR)
                jsr IncBankPointer  ;increase ZP_PTR by one
            
            @Ready:
            
            inc MODAL_COL_COUNT
            lda MODAL_COL_COUNT
            cmp MODAL_WIDTH
            beq @Klaarcol
            jmp @nextcol  
        @Klaarcol:
        
        inx ;next line for PLOT
        inc MODAL_ROW_COUNT
        lda MODAL_ROW_COUNT
        cmp MODAL_HEIGHT
        beq @klaarrow
        jmp @nextrow
        
    @klaarrow:

rts


EndOfScreenlist:

    clc
    lda OFFSET_CURRENT
    adc ROW_CURRENT
    cmp FILECOUNT_CURRENT
    beq EndOfFilesList      ;eofilelist
    

        
    inc OFFSET_CURRENT
    jsr ShowFiles2
    dec ROW_CURRENT
    jsr HighlightRow
jmp lp

EndOfFilesList:
    dec ROW_CURRENT
    jsr HighlightRow   

jmp lp

RowUp:
    
   jmp DoRowUp   
jmp lp

DoRowUp:

    lda ROW_CURRENT
    cmp #$0

    beq TopOfScreenlist  ;kan niet verder omhoog
    
    jsr LowlightRow
    dec ROW_CURRENT
    jsr HighlightRow 

jmp lp

TopOfScreenlist:
  jmp DoTopOfScreenlist

jmp lp

DoTopOfScreenlist:
    lda OFFSET_CURRENT
    cmp #$0
    beq Goto_lp  ;bovenaan de lijst, dus niets doen
    
    jsr LowlightRow
    dec OFFSET_CURRENT
    jsr ShowFiles2
    jsr HighlightRow 

 jmp lp

Goto_lp:
    jmp lp

HighlightRow:   ;manipulate the color byte of each charactewr
    

    clc
    jsr SetVeraAddress
    ldx #0
    lda #$16    ;color
    @nextchar:
        STA $9F23   ;VERA DATA_0
        inx
        cpx WINDOW_WIDTH
        bne @nextchar
        

rts

LowlightRow:    ;manipulate the color byte of each charactewr
    clc
    jsr SetVeraAddress
    ldx #0
    lda #$61    ;color
    @nextchar:
        STA $9F23   ;VERA DATA_0
        inx
        cpx WINDOW_WIDTH
        bne @nextchar
        

rts

PrintDebugInfo:
    phy
    phx
    pha
    
    ldx #50
    ldy #50
    clc
    jsr PLOT
    lda #$31
 ;   jsr CHROUT
    lda #COLON
  ;  jsr CHROUT
    pla
   ; lda FILECOUNT_CURRENT
    jsr print_hex
   
    plx
    ply

rts

SetVeraAddress:
    pha
    phx
        lda ROW_CURRENT
        adc #7  ;offset for line start
      
      
        
        tax
        
        
        jsr InitAdder24bit
        
        ;VERA_START_TEXT =$21B000
    
        lda #$03                ;low
        sta Adder24bitValue

        lda #$B0                ;middle
        sta Adder24bitValue+1
        
        lda #$21                ;high
        sta Adder24bitValue+2
    
        ;set value to add       $0100   = 256
        lda #$00
        sta Adder24bitToAdd    
        lda #$01
        sta Adder24bitToAdd+1    
        
        
        
        @loop:      
            jsr Adder24bit  ;add 256 for each line as 1 line is 256 bytes

            dex
            cpx #0
            bne @loop
        
        clc
        lda iCurrentWindow
        cmp #0
        beq @NoOffset
            ;add 78 dec to address to offset to right window
            lda #1
             adc WINDOW_WIDTH
             adc WINDOW_WIDTH
            ;lda #$4E
            sta Adder24bitToAdd    
            lda #$00
            sta Adder24bitToAdd+1  
            jsr Adder24bit        
        @NoOffset:
        
        
        


        LDA Adder24bitValue                ;$N1B000
        STA $9F20   ;ADDRES_L

        LDA Adder24bitValue+1    ;ADDRES_M   B0
        STA $9F21

        LDA Adder24bitValue+2    ;ADDRES_H ->
        STA $9F22
    
    plx
    pla
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

IncBankPointer2:
    pha
        clc
        lda ZP_PTR2    ;load low byte
        adc #1
        sta ZP_PTR2
        bcc @Done
        inc ZP_PTR2+1
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

DecBankPointer2: 
    pha
        clc
        lda ZP_PTR2    ;load low byte
        SEC               
        sbc #1
        sta ZP_PTR2

        lda ZP_PTR2+1
        sbc #0
        sta ZP_PTR2+1
        @Done:
    pla
rts



StoreCurrentAInMemory:
    sta (ZP_PTR)
    jsr IncBankPointer 
rts

IncLFNPointer:
    pha
        clc
        lda ZP_PTR2    ;load low byte
        adc #1
        sta ZP_PTR2
        bcc @Done
        inc ZP_PTR2+1
        @Done:
    pla
rts

SetRamBank:
    pha
    lda MODE_CURRENT
    cmp #0
    bne @ToPartitionMode
    
        clc
        lda #RB_FILES_LIST_LEFT        ;01
        adc iCurrentWindow  ;if right side, then will be 8
        
        
        
        sta ZP_RAMBANK
        jmp @Continue
        
        @ToPartitionMode:
            lda #RB_PARTITIONS
            sta ZP_RAMBANK
        @Continue:
    pla 
rts
   ;------------------------SET RAMBANK FOR CORRECT WINDOW ------------------------------
    pha
        lda iCurrentWindow
        cmp #0
        beq @WindowLeft
        
            lda #02        ;set rambank to 02: :window right
            sta ZP_RAMBANK   
            bra @Window_Done
        @WindowLeft:
            lda #03        ;set rambank to 03   ;window left
            sta ZP_RAMBANK   
        @Window_Done:
    pla
    ;------------------------SET RAMBANK FOR CORRECT WINDOW ------------------------------
rts

SetRambankLongFileNames:
    pha
        clc
        lda #RB_LONGFILENAMES_LEFT 
        adc iCurrentWindow
        sta ZP_RAMBANK
    pla
    

rts

SetPointerToCurrentOffset:
    lda OFFSET_CURRENT      ;offset
    adc ROW_CURRENT         ;+current row = for pointer
    
    jsr SetRamBank
    ;Check in memory
    ;;--- Store rambank address in start pointer
    lda #<RAMBANK
    sta ZP_PTR
    lda #>RAMBANK
    sta ZP_PTR+1
    
     jsr InitAdder24bit

    lda ZP_PTR                ;low
    sta Adder24bitValue

    lda ZP_PTR+1                ;middle
    sta Adder24bitValue+1
    
    lda #$00                ;high
    sta Adder24bitValue+2

    ;set value to add       $0100   = 256
    lda SET_RECORD_LENGTH ; #$20   ;32 = 32 bytes per file
    sta Adder24bitToAdd    
    lda #$00
    sta Adder24bitToAdd+1    
    
    phx
        clc
        ldx OFFSET_CURRENT
        cpx #0  ;bij 0 niks doen
        beq DoneX
        SkipNextLine:
            jsr Adder24bit
            dex
            cpx #$0
        bne SkipNextLine
        
        ;terug in ZP_PTR plaatsen
        lda Adder24bitValue
        sta ZP_PTR

        lda Adder24bitValue+1
        sta ZP_PTR+1  
 DoneX:   
    plx

rts

AdvancePointerToCurrentRow:
 ;check if necessary
    phx
        lda ROW_CURRENT
        cmp #0
        beq @Done
        
        ;Advance pointer to current row
        ldx #0
        @next:
            jsr Adder24bit      
            inx
            cpx ROW_CURRENT
            beq @Done
        jmp @next
        
        @Done:
        
        ;terug in ZP_PTR plaatsen
        lda Adder24bitValue
        sta ZP_PTR

        lda Adder24bitValue+1
        sta ZP_PTR+1    
    plx
rts

Goto_PrintEmptyLines:   jmp PrintEmptyLines
    

ShowFiles2:
    ;Set correct rambank
    clc
    jsr SetRamBank
    
    ldy COL_OFFSET
    ldx #4
    clc
    jsr PLOT

    jsr PrintCurrentFolder
    jsr PrintCurrentDevice
    
    
    clc
    ldx #7
    ldy COL_OFFSET
    jsr PLOTVera 
    
    lda FILECOUNT_CURRENT
    cmp #0
    beq Goto_PrintEmptyLines

    ;set pointer to star of rambank
    lda #<RAMBANK
    sta ZP_PTR
    lda #>RAMBANK
    sta ZP_PTR+1

    lda OFFSET_CURRENT
    cmp #0
    beq @NoOffset
    jsr SetPointerToCurrentOffset

    @NoOffset:
    
    stz CNT
    ldx #7
    Nextline2:
        clc
        ldy COL_OFFSET
        jsr PLOTVera  
        
        ;file size
        jsr IncBankPointer
        jsr IncBankPointer
           
        PrintCharVera ZP_PTR,12     ;file name
        lda #$20
        jsr PrintAToVera  
 
        lda SET_SHOWSIZE
        cmp #0
        beq @HideSize
            PrintCharVera ZP_PTR,8     ;size
            lda #$20
            jsr PrintAToVera  
            jmp @ShowDate
        @HideSize:
            IncBankpointerBy 8
        
        @ShowDate:
        lda SET_SHOWDATE
        cmp #0
        beq @HideDate        
            
            lda SET_DATE_LEN
            cmp #8
            beq @ShowShortDate
                PrintCharVera ZP_PTR,10     ;date
                lda #$20
                jsr PrintAToVera  
                jmp @Continue3

            @ShowShortDate:   
                jsr IncBankPointer
                jsr IncBankPointer
                PrintCharVera ZP_PTR,8
            @Continue3:

            jmp @ShowTime
        @HideDate:
            IncBankpointerBy 10
    
        @ShowTime:
        lda SET_SHOWTIME
        cmp #0
        beq @HideTime        
            PrintCharVera ZP_PTR,5     ;time
            jmp @Continue2
        @HideTime:
            IncBankpointerBy 5
            
        @Continue2:
        jsr IncBankPointer  ;type
        jsr IncBankPointer  ;LFNpointer
        jsr IncBankPointer  ;LFNpointer+1                     
        
        inx
        
        inc CNT
        clc
        lda CNT
        adc OFFSET_CURRENT
        cmp FILECOUNT_CURRENT
        beq @Done
        
        lda CNT
        cmp ListSize
        bne Goto_Nextline2

        @Done:
        
        lda CNT
        cmp ListSize
        bpl Continue
        
        PrintEmptyLines:
        sta ListCounter
        ;print empty lines
        jsr FilesReadyPrintBlanks
        
        Continue:
        
rts

Goto_Nextline2: jmp Nextline2

PrintCurrentDevice:
    ldx #3
    
  
    
    ldy COL_OFFSET

   ; iny

    clc
    jsr PLOT
    
    lda #$5b
    jsr CHROUT

   
    
    lda #<RAMBANK
    sta ZP_PTR
    lda #>RAMBANK
    sta ZP_PTR+1
    
    lda ZP_RAMBANK
    pha
        lda #RB_PARTITIONS
        sta ZP_RAMBANK
     

        @lp:
            ldy #0
            lda (ZP_PTR),y
          
            cmp DEVICE_CURRENT
            bne @NextRecord

            iny
            lda (ZP_PTR),y
            cmp PARTITION_CURRENT
            bne @NextRecord
            
            jmp @Continue
        @NextRecord:
            ldy #0
            @lp3:
                
                jsr IncBankPointer
                iny
                cpy SET_RECORD_LENGTH
                bne @lp3                
            
        jmp @lp
        @Continue:
        

        jsr IncBankPointer

        jsr IncBankPointer
  

        @lp2:
            lda (ZP_PTR)
            cmp #0
            beq @Done
            jsr CHROUT
            jsr IncBankPointer
        jmp @lp2
            
        @Done:
    pla
    sta ZP_RAMBANK
    
    lda #$5d
    jsr CHROUT
    
    lda #$63
    jsr CHROUT
    jsr CHROUT
    jsr CHROUT
    jsr CHROUT
    jsr CHROUT
    jsr CHROUT
  
    
rts

PrintCurrentFolder:
    
        ldy COL_OFFSET
        ldx #4
        clc
        jsr PLOT
        
        
        CountUntilZero DIR_CURRENT,CNT
        lda CNT
        sta TMP2    ;store to use later
        cmp WINDOW_WIDTH
        bmi @PrintNormal
        phy
        phx
            ;doesn't fit, so print portion
            lda WINDOW_WIDTH ;devide by two
            sec
            sbc #4      ;using 4 dots to seperate
            clc
            lsr
            sta CNT ;CNT contains with of left and right portion
             
            ;print left portion
            ldx #0
            :
                lda DIR_CURRENT,x
                jsr CHROUT
                inx
                cpx CNT
                bne :-
            ;print 4 dots
                lda #$2e
                jsr CHROUT
                jsr CHROUT
                jsr CHROUT
                jsr CHROUT
            ;print right side of 
                ldy TMP2
                ;decrease Y [CNT] times
                ldx #0
                :
                    dey
                    inx
                    cpx CNT
                    bne :-
                ;print chars
                :
                    lda DIR_CURRENT,y
                    jsr CHROUT       
                    iny
                    cpy TMP2
                    bne :- 
        plx
        ply            
        jmp PrintDone
        
        @PrintNormal:

        phy
            ldy #0

            NextChar:
                lda DIR_CURRENT,y
                cmp #$0
                beq @PrintPathDone
                jsr CHROUT
            iny
            cpy #38
            bne NextChar
        
            @PrintPathDone:
                lda #$20
                jsr CHROUT
                iny
                cpy WINDOW_WIDTH
                beq @PrintPathSpacesDone   
            jmp @PrintPathDone  
            @PrintPathSpacesDone:    
        ply 
        PrintDone:
rts


    
FilesReady:    

 

rts

FilesReadyPrintBlanks:
    @NextLine:
        
        jsr PLOTVera
        lda #$20
        phx
            ldx #0
            :
                lda #$20
                STA $9F23
                inx
                cpx WINDOW_WIDTH
                bne :-
        
        plx
       ; PrintACharVera 38
        
        inx
        inc ListCounter
        lda ListCounter
        cmp ListSize
        beq FilesReady      
    jmp @NextLine
rts

GetCurrentPartition:
jmp GetCurrentPartition_start
    CMD_PARTITION_INFO: .byte "g-p",0
    
GetCurrentPartition_start:
    ;lda #8
   ; sta DEVICE_CURRENT
   ; sta $03fe
    CopyAddrUntilZero CMD_PARTITION_INFO,CMD_BUFFER
    stz CMD_BUFFER_LENGTH
    CountUntilZero CMD_BUFFER_RESULT,CNT
    jsr DoDosCMD
   
    ldy #2
    lda CMD_BUFFER_RESULT,y ;3rd byte is current partition
    cmp #$2c        ;2c seems to be a code for hostfs??    
    beq @Continue
    sta PARTITION_CURRENT
    sta PARTITION_LEFT
    sta PARTITION_RIGHT
    sta PARTITION_OTHER
    
    
    @Continue:
    
rts

GotoCurrentDirectory:

    ;goto current device
    lda DEVICE_CURRENT
    sta $03fe
        
    lda PARTITION_CURRENT

    cmp #0
    beq @NoPartitionToSelect
        jsr SelectCurrentPartition
    
    @NoPartitionToSelect:


    
    ;;;goto root
        stz CMD_BUFFER_LENGTH
    
        lda #$43   ;C
        sta CMD_BUFFER
        inc CMD_BUFFER_LENGTH

        lda #$44   ;D
        sta CMD_BUFFER+1
        inc CMD_BUFFER_LENGTH

        lda #$3A   ;:
        sta CMD_BUFFER+2
        inc CMD_BUFFER_LENGTH

    ldx #0

    @NextChar:
        lda DIR_CURRENT,x
       ; lda TMPDIR,x
      
        cmp #$0
        beq @Done   ;na $0 ook klaar... 
        ;jsr CHROUT
        sta CMD_BUFFER+3,x
        inx
        inc CMD_BUFFER_LENGTH

    jmp @NextChar
        
    ;@DoCMD:
   ;     jsr DoDosCMD ;do change directory
   ; jmp @NextDir ;goto next directory

@Done:
    jsr DoDosCMD
rts

DoDosCMD:

    lda DOS_CMD_DEVICE  ;0=Current, 1=Other
    cmp #0
    beq @UseCurrent
        ;use other
        lda iCurrentWindow
        cmp #0
        beq @UseRight
            lda DEVICE_LEFT
            sta DOS_CMD_DEVICE         
        
        jmp @Continue
        
        @UseRight:
            lda DEVICE_RIGHT
            sta DOS_CMD_DEVICE 
        jmp @Continue
    @UseCurrent:
         lda DEVICE_CURRENT
         sta DOS_CMD_DEVICE       
    @Continue:
    
;    lda DOS_CMD_DEVICE
;    jsr print_hex
;    jsr WaitKey
    
    lda CMD_BUFFER_LENGTH
    cmp #0
    bne @Door

    ldx #0
    @next:
        lda CMD_BUFFER,x
        cmp #$0
        beq @Door
      ;  jsr CHROUT
        inx
       inc CMD_BUFFER_LENGTH
    jmp @next
   
    @Door:
 ;   lda CMD_BUFFER_LENGTH
 ;   jsr print_hex
 ;   jsr WaitKey
 ;    clc
  ;  ldx #58
 ;   ldy #0
 ;   jsr PLOT
    
    ldx #0
    :
       lda CMD_BUFFER,x
   ;    jsr CHROUT
       inx
       cpx CMD_BUFFER_LENGTH
       bne :-
    
  ;  lda #$20
 ;  jsr CHROUT
  ;  lda CMD_BUFFER_LENGTH
  ;  jsr print_hex
  ;  lda #$20
  ;  jsr CHROUT   
    
    
   lda #1   ; Logical Number = 1
   ldx DOS_CMD_DEVICE ;DEVICE_CURRENT ;#8   
   ldy #15   ;   15=control channel
      
   jsr SETLFS



   lda CMD_BUFFER_LENGTH ; filename length
   ldx #<CMD_BUFFER
   ldy #>CMD_BUFFER
   jsr SETNAM

   
   jsr OPEN
; jmp Klaarxx  
    ldx #58
    ldy #0
  ;  jsr PLOT
    
     LDX #1
     jsr CHKIN  

     ldy #$00      ;PREPARE THE Y REGISTER TO STORE THE DATA
@RD:
    lda #1
    jsr READST
    bne @Continue2
    JSR CHRIN
    STA CMD_BUFFER_RESULT,Y    ;STORE THE YTH DATA BYTE IN THE YTH
    ;jsr CHROUT
                   ;LOCATION IN THE DATA AREA.
     INY
   ;  CMP #LF       ;IS IT A CARRIAGE RETURN?
  ;   BNE @RD        ;NO, GET ANOTHER DATA BYTE
     jmp @RD
  @Continue2:
    
    lda #0
    STA CMD_BUFFER_RESULT,Y
   
   Klaarxx:
   

   jsr CLRCHN
   lda #1
   jsr CLOSE

   lda #1
   jsr CLOSE 
   jsr CLSALL
   stz DOS_CMD_DEVICE
   rts    

rts

SelectCurrentPartition:
    stz CMD_BUFFER_LENGTH

    lda #$43   ;C
    sta CMD_BUFFER
    inc CMD_BUFFER_LENGTH

    lda #$50   ;P
    sta CMD_BUFFER+1
    inc CMD_BUFFER_LENGTH
    
    clc
    lda PARTITION_CURRENT
    adc #$30
    sta CMD_BUFFER+2
    inc CMD_BUFFER_LENGTH

    jsr DoDosCMD
rts

SelectOtherPartition:
    stz CMD_BUFFER_LENGTH

    lda #$43   ;C
    sta CMD_BUFFER
    inc CMD_BUFFER_LENGTH

    lda #$50   ;P
    sta CMD_BUFFER+1
    inc CMD_BUFFER_LENGTH
    
    clc
    lda PARTITION_OTHER
    adc #$30
    sta CMD_BUFFER+2
    inc CMD_BUFFER_LENGTH

    jsr DoDosCMD
rts

LoadCurrentDir2:
    jmp LoadCurrentDir2_start
    
    TMP_FILESIZE:       .byte 0,0
    TMP_FILESIZE_DESIGNATOR: .byte 0,0
    TMP_LFN_POINTER:    .byte 0,0
    FILE_NAME_LENGTH:   .byte 0
    LONG_FILE_NAME:     .res 255
    LONG_FILE_NAME_PTR: .res 2
    PAGE_FILE_COUNT:    .byte 0
    PAGE_CURRENT_USE:   .byte 0
    LAST_START_POINTER: .res 2,0
    MSG_LOAD_MORE: .byte  "-next page-",0
    MSG_LOAD_LESS: .byte  0,0,"-prev page-                                                     ",0 ;"
    
    LoadCurrentDir2_start:
    
   
    
    lda PAGE_CURRENT
    sta PAGE_CURRENT_USE    ;pages to skip
    
  ;  inc PAGE_CURRENT_USE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;  inc PAGE_CURRENT_USE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;  inc PAGE_CURRENT_USE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    stz PAGE_FILE_COUNT ;if PAGE_CURRENT_USE>0 then PAGE_FILE_COUNT counts until full page to skip
    
    jsr SetRamBank
    

    jsr GotoCurrentDirectory    
    
    ;set start of rambank to ZP-ptr
    lda #<RAMBANK
    sta ZP_PTR
    sta ZP_PTR2 ;pointer for long filenames
   
    lda #>RAMBANK
    sta ZP_PTR+1
    sta ZP_PTR2+1

    
    
    
    stz FILECOUNT_CURRENT
    stz TMP2
    
    lda PAGE_CURRENT
    cmp #0
    bne @Goto_NoSubdir
    ;add subdir if needed
    lda CMD_BUFFER_LENGTH
    cmp #$04    ;cd:/  -> root
    beq @Goto_NoSubdir
        ;add subdir
        ldy #0
        :
            lda UPDIR,y
            jsr StoreCurrentAInMemory
            iny
            cpy SET_RECORD_LENGTH
            bne :-     
            inc TMP2   
        ;inc FILECOUNT_CURRENT  ;added after load so file count is correct
    
    @Goto_NoSubdir:
    
    lda PAGE_CURRENT
    cmp #0
    beq @Goto_NoPageUp
         ldy #0
        :
            lda MSG_LOAD_LESS,y
            jsr StoreCurrentAInMemory
            iny
            cpy SET_RECORD_LENGTH
            bne :-     
            inc TMP2
            jsr DecBankPointer
            jsr DecBankPointer
            jsr DecBankPointer
            lda #$3c
            jsr StoreCurrentAInMemory
            jsr IncBankPointer                    
            jsr IncBankPointer                    
    
       
    @Goto_NoPageUp:
    lda ZP_PTR
    sta LAST_START_POINTER
    lda ZP_PTR+1
    sta LAST_START_POINTER+1
    
  ;  stz LAST_START_POINTER
  ;  stz LAST_START_POINTER+1
    
    
    stz DOS_DIR_TYPE    ;0=directories
    jsr LoadFilesList2
 
    lda PAGE_CURRENT_USE
   ; jsr print_hex
  ;  lda #COLON
  ;  jsr CHROUT
 ;   lda FILECOUNT_CURRENT
 ;   jsr print_hex
 ;   jsr WaitKey
      
    inc DOS_DIR_TYPE
    jsr LoadFilesList2
       
    ;if there is a updir add to count
    clc
    lda FILECOUNT_CURRENT
    adc TMP2
    sta FILECOUNT_CURRENT
    

    
rts

Goto_NoSubdir: jmp NoSubdir
LoadCurrentDir:     ;first attempt without caching
jmp LoadCurrentDir2

    jmp LoadCurrentDir_start
 

        AR_INDEX: .byte $0,$0      ;2 byte index of array
        ReadStatus: .byte $0
        LenByte: .byte $0
        UPDIR: .byte 0,0,"..                                 d",0,0   ;"
    
        
    LoadCurrentDir_start:
    
    jsr SetRamBank
    
    stz ReadStatus
    stz LenByte
    stz AR_INDEX
    stz AR_INDEX+1
    
    jsr GotoCurrentDirectory
 
    ;;--- Store rambank address in start pointer
    lda #<RAMBANK
    sta ZP_PTR
    sta ZP_PTR2
    lda #>RAMBANK
    sta ZP_PTR+1
    sta ZP_PTR2+1
 

    
  
    ;;-------------------------------------

    stz FILECOUNT_CURRENT    
 
   
 
    ;check if is root
    lda CMD_BUFFER_LENGTH
    cmp #$04    ;cd:/  -> root
    beq Goto_NoSubdir
    
    ;add row to cd..
    ldy #0
    :
        lda UPDIR,y
        jsr StoreCurrentAInMemory
        iny
        cpy #32
        bne :-
    jmp SkipLines
                
                ;2 bytes for fake size
                lda #0
                jsr StoreCurrentAInMemory
                jsr StoreCurrentAInMemory
                
                ;12 bytes for filename  BA  C0
                lda #$2E
                jsr StoreCurrentAInMemory
                jsr StoreCurrentAInMemory
                ;add 8 spaces
                lda #$20
                jsr StoreCurrentAInMemory
                jsr StoreCurrentAInMemory
                jsr StoreCurrentAInMemory
                jsr StoreCurrentAInMemory
                jsr StoreCurrentAInMemory
                jsr StoreCurrentAInMemory
                jsr StoreCurrentAInMemory
                jsr StoreCurrentAInMemory
                jsr StoreCurrentAInMemory
                jsr StoreCurrentAInMemory
                
                ;add 10 for date
                jsr StoreCurrentAInMemory
                jsr StoreCurrentAInMemory
                jsr StoreCurrentAInMemory
                jsr StoreCurrentAInMemory
                jsr StoreCurrentAInMemory
                jsr StoreCurrentAInMemory
                jsr StoreCurrentAInMemory
                jsr StoreCurrentAInMemory
                jsr StoreCurrentAInMemory
                jsr StoreCurrentAInMemory

                ;add 5 for time        
                jsr StoreCurrentAInMemory
                jsr StoreCurrentAInMemory
                jsr StoreCurrentAInMemory
                jsr StoreCurrentAInMemory
                jsr StoreCurrentAInMemory
                
                ;add 'D' for directory
                lda #$44
                jsr StoreCurrentAInMemory
                lda #0
                jsr StoreCurrentAInMemory
                jsr StoreCurrentAInMemory
                
    SkipLines:
    
    ldy #0
    @nxt2:
        jsr IncBankPointer2
        iny
        cpy #32
        bne @nxt2    

    inc FILECOUNT_CURRENT
    

    
    NoSubdir:
 
   ;record:  
    ;      SS :               2 byte blocksize, hex
    ;      AAAAAAAAAAAA:     12 byte file name
    ;      NNNNNNN            8 bytes size
    ;      DDDDDDDDDD:       10 byte date
    ;      TTTTT:             5 byte time
    ;       type:             1 byte
    ;      PTR LFN            2 byte pointer to long filename   
    ;      total 40 bytes
    
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;   
    ;;;;;;;;;;;2x aanroepen 1x met alleen dirs en 1x met alleen files
    

    
    
    lda #0  ;dirs
    sta DOS_DIR_TYPE
    jsr LoadFilesList2
    
    jsr DecBankPointer
    jsr DecBankPointer
    jsr DecBankPointer
    jsr DecBankPointer

    ;2 fileheaders are added due to BLOCKS FREE line, dirty solution!!!
    ldy #0
                
    @nxt:
        jsr DecBankPointer2
        iny
        cpy #64
        bne @nxt  
      
    
    lda #1  ;files
    sta DOS_DIR_TYPE
    jsr LoadFilesList2  
    
 
    
rts
;; REAL END OF READ

NextPage:
    jsr LowlightRow
    inc PAGE_CURRENT
    jsr ReloadCurrentWindow
    jsr HighlightRow
jmp lp

PrevPage:
    jsr LowlightRow
    dec PAGE_CURRENT
    jsr ReloadCurrentWindow
    jsr HighlightRow
jmp lp

DoRTS:
rts

LoadFilesList2:
    
  ;  lda FILECOUNT_CURRENT
  
  ;  jsr print_hex
  ;  jmp hlt
  ;  cmp SET_MAX_FILES_COUNT
  ;  bpl DoRTS   ;bpl>128 doet gek!!!!!!!!!!!!!!
    
    sec
    lda SET_MAX_FILES_COUNT
    sbc FILECOUNT_CURRENT
    bcc DoRTS
    

    lda #1   ; Logical Number = 1
    ldx DEVICE_CURRENT ;#8   ; Device = "SD card" (emulation host FS)
    ldy #0   ; Secondary Address = 15: dos command channel
    jsr SETLFS


    lda DOS_DIR_TYPE
    cmp #1
    beq @DoFiles
    ;directories
    lda #(DOS_DIR_END-DOS_DIR) ; command
    ldx #<DOS_DIR
    ldy #>DOS_DIR   
    jmp @Continue
    
    @DoFiles:
        ;overwrite if we need files
        lda #(DOS_FILES_END-DOS_FILES) ; command
        ldx #<DOS_FILES
        ldy #>DOS_FILES 
    @Continue:
    
    jsr SETNAM
    jsr OPEN
    
    LDX #1
    jsr CHKIN
    
    ;Read 8 bytes to skip over 00 bytes which are not line end
    ldy #0
    :
        jsr CHRIN 
        iny
        cpy #8
        bne :-

    
    ldx #0  ;X contains number which represents type of data being processed
    ldy #0
    

    
    jmp NextByte
        Goto_EndOfFile:             jmp EndOfFile
        Goto_NewLine:               jmp NewLine
        Goto_CheckStartOfFilename:  jmp CheckStartOfFilename
        Goto_ProcessFilename2:      jmp ProcessFilename2
        Goto_FindType:              jmp FindType
        Goto_StoreDate:             jmp StoreDateTime
        
    NextByte:
        
        lda PAGE_FILE_COUNT
        cmp SET_MAX_FILES_COUNT
        bne @Continuex
            dec PAGE_CURRENT_USE    ;pages to skip
            stz PAGE_FILE_COUNT 

        @Continuex:
        
        jsr READST     ; call READST (read status byte)
        bne Goto_EndOfFile        ; either EOF or read error
        jsr CHRIN       ;get next byte
        
        
        cmp #0
        beq CheckNewLine
        
        ContinueNextByte:
        cpx #1
        beq Goto_CheckStartOfFilename
        
        cpx #2
        beq Goto_ProcessFilename2
        
        cpx #3  ;search for first non space
        beq Goto_FindType
        
        cpx #4
        beq Goto_StoreDate
        

        
    jmp NextByte
    
    CheckNewLine:
        
            
        cpx #0
        beq NewLine
    jmp ContinueNextByte       
    
    NewLine:

        
        ldx PAGE_CURRENT_USE
        cpx #0
        beq @Continuey

            ;skip this file
           ; jsr WaitKey
  

            jsr CHRIN   ;skip past potention zero bytes in line number and file size
            jsr CHRIN
            jsr CHRIN
            jsr CHRIN
            
            ldx #0
            @lpp:
                inx
                cpx #100
                beq @SkipCount
                jsr CHRIN
                cmp #$22    ;"
                bne @lpp
                 
            cmp #$2E    ;.
            beq @SkipCount
            ;deze moet NIET uitgevoerd worden als het . of .. is......
            inc PAGE_FILE_COUNT
            @SkipCount:
            
            lda ZP_PTR
            sta LAST_START_POINTER
            lda ZP_PTR+1
            sta LAST_START_POINTER+1
                     
            ldx #0  ;read until next record
            jmp NextByte
            
        @Continuey:
      
      

        stz LFN_Pointer
        stz LFN_Pointer+1
        
        ;2 bytes contain BASIC line number, skip them
        jsr CHRIN
        jsr CHRIN
        
        ;2 bytes contain file size
        jsr CHRIN
        StoreCurrentAInMemoryMacro
        sta TMP_FILESIZE
     
        jsr CHRIN
        StoreCurrentAInMemoryMacro
        sta TMP_FILESIZE+1
        
        ;2  bytes containing KB or MB, store them for later use
        jsr CHRIN
        sta TMP_FILESIZE_DESIGNATOR
        jsr CHRIN
        sta TMP_FILESIZE_DESIGNATOR+1  
        
 
        ldx #1  ;read until " for start of file  



    
    jmp NextByte
    
    CheckStartOfFilename:
        cmp #$22        ;"
        bne @Continue
        
        ldx #2  ;following bytes are the filename
        ldy #0  ;length of filename
        
        
    @Continue:
    jmp NextByte
    
    ProcessFilename2:
        cmp #$22        ;"
        beq EndOfFilename
        ;not endeyet, so continu 

        cmp #$60       ;vergelijken met char boven kleine letter A
        bmi @IsPrintable   ;als ie kleiner is dan gewoon afdrukken
        sbc #32       ;anders naar uppercase transformeren door 32 er af te halen
    
        @IsPrintable:
        
        cpy #0
        bne @Continue3
        
            ;check if is period, if so,skip this line is \. or \..
            cmp #$2e    ;period
            bne @Continue3
            jsr DecBankPointer
            jsr DecBankPointer
            
            ldx #0
            jmp NextByte
            
        
        @Continue3:
        sta LONG_FILE_NAME,y
        iny
        
        cpy #13 ;if filename is longer then 12 bytes
        bne @Continue2
            ;store asterix as last byte of filename to mark long filename    
            jsr DecBankPointer
            
            lda #$2A
            StoreCurrentAInMemoryMacro
           
        jmp @Continue
        
        @Continue2:
        ;check if length > 12 then do nothing
        cpy #13
        bpl @Continue
        StoreCurrentAInMemoryMacro 
        
        @Continue:
    jmp NextByte
    
    EndOfFilename:
        cpy #12
        bpl @NoSpaces
        lda #$0
        StoreCurrentAInMemoryMacro
        iny
        cpy #12
        bpl @NoSpaces
        
        ;add spaces
        lda #$20
        @lpp:
            StoreCurrentAInMemoryMacro
            iny
            cpy #12
            bmi @lpp 
        @NoSpaces:
        
        
        ;store long filename in seperate rambank TODO!!!!
        cpy #13     ;check if filename>12
        bmi @Continue
        lda ZP_RAMBANK
        pha
            ;set rambank to long file names
            jsr SetRambankLongFileNames
            
            ;copy pointer of current to store later
            lda ZP_PTR2
            sta LFN_Pointer
            lda ZP_PTR2+1
            sta LFN_Pointer+1
            
            ;copy filename into rambank
            sty TMP
            ldy #0
            @lp:
                lda LONG_FILE_NAME,y
                sta (ZP_PTR2)
                jsr IncLFNPointer
                iny
                cpy TMP
                bne @lp
                
            lda #0
            sta (ZP_PTR2)
            jsr IncLFNPointer
            
        
        pla
        sta ZP_RAMBANK
     
        
        @Continue:
        ldy #0
        ldx #3  ;read type
       
    jmp NextByte
    
    Goto_Nextbyte: jmp NextByte
    FindType:
        ;find first non $20 byte
        cmp #$20
        beq Goto_Nextbyte
        
        sta TMP ;store P or D in TMP for now
        cmp #$44    ;D
        bne @NoDirectory
        ;store dir caption instead of size
            ldy #0
            @lp123:
                lda CAPTION_DIR,y
                StoreCurrentAInMemoryMacro
                iny
                cpy #8
                bne @lp123
                
        jmp @SizeDone
        @NoDirectory:

            ;convert and store file size, Size is max 8 bytes incl  designator
            
          ;  jsr ClearHex2DecValue   ;get ready to convert blocksize to dec

            lda TMP_FILESIZE
            sta value_32bit

            lda TMP_FILESIZE+1
            sta value_32bit+1       
            jsr ConvertHex2Dec16Bitx           
        
            ;copy 4 bytes as max 9999mb (more then fat 32 alows)
            ldy #RESULT_LENGTH
            dey
            dey
            dey
            dey
            
            @lp1234:
                lda result_32bit,y
                StoreCurrentAInMemoryMacro
                iny
                cpy #RESULT_LENGTH+1
                bne @lp1234
        
            lda #$20    ;space between size and designator
            StoreCurrentAInMemoryMacro
                    
            ;add designator
            lda TMP_FILESIZE_DESIGNATOR
            StoreCurrentAInMemoryMacro
        
            lda TMP_FILESIZE_DESIGNATOR+1
            StoreCurrentAInMemoryMacro
        @SizeDone:
         
        ;Skip 4 bytes to start of date
        jsr CHRIN
        jsr CHRIN
        jsr CHRIN
      ;  jsr CHRIN
        
        ldx #4  ;store date


                
       ; @Continue:
    jmp NextByte
    
    StoreDateTime:
        ;next 10 bytes is date
      
        cmp #0
        beq @NoDateTimeFound      
       ; StoreCurrentAInMemoryMacro
        ldy #0
        @lp12345:
            jsr CHRIN
            
            StoreCurrentAInMemoryMacro
            iny
            cpy #10
            bne @lp12345 
        jsr CHRIN   ;space
         
        ;next 5 bytes is time
        ldy #0
        @lp1234566:
            jsr CHRIN
            StoreCurrentAInMemoryMacro
            iny
            cpy #5
            bne @lp1234566  
        jmp @Continue
        @NoDateTimeFound:
            ldx #0
            @lp44:
                
                lda #$20
                StoreCurrentAInMemoryMacro
                ;jsr CHRIN  ;stop reading, there is nothing there if no date
                inx
                cpx #15
            bne @lp44
                
                
                ;we are already at the next line
                ;sta TMP2
                jsr StoreEndOfFile
                
                lda TMP3
                cmp #1
                beq EndOfFile
                     
                lda #0
                ldx #0
                jmp CheckNewLine
        @Continue:
        
        
        jsr StoreEndOfFile
jmp SkipStore
        ;store type
        lda TMP
        StoreCurrentAInMemoryMacro

        ;store pointer for longfile name
        lda LFN_Pointer
        StoreCurrentAInMemoryMacro
        lda LFN_Pointer+1
        StoreCurrentAInMemoryMacro
        
        inc FILECOUNT_CURRENT
        
        ;backup start if next line
        lda ZP_PTR
        sta LAST_START_POINTER
        lda ZP_PTR+1
        sta LAST_START_POINTER+1
        
       
        
        ldx FILECOUNT_CURRENT
        cpx SET_MAX_FILES_COUNT
        bpl EndOfFile2   ;maximum files per read
        
        ldx #0  ;search of next line
SkipStore:
        
        lda TMP3
        cmp #1
        beq EndOfFile2
        
    jmp NextByte
    

    
    EndOfFile:
      ;  jsr WaitKey
    jmp CleanUp
    EndOfFile2:
        
        ;add load next 200 files option
 
        lda LAST_START_POINTER
        sta ZP_PTR
        lda LAST_START_POINTER+1
        sta ZP_PTR+1  
        
        lda #0
        StoreCurrentAInMemoryMacro
        StoreCurrentAInMemoryMacro
        
        
        ;add page down line!!!!
        ldx #0
        @lp:
            lda MSG_LOAD_MORE,x
            StoreCurrentAInMemoryMacro
            inx
            cmp #0
            bne @lp
        
        inx
        inx
        lda #$20
        
        @lp2:
            StoreCurrentAInMemoryMacro
            inx
            cpx SET_RECORD_LENGTH
            bne @lp2
        jsr DecBankPointer
        jsr DecBankPointer
        jsr DecBankPointer
       ; jsr DecBankPointer
        lda #$3e
        StoreCurrentAInMemoryMacro
        jsr IncBankPointer  
      ;  jsr IncBankPointer  
        jsr IncBankPointer 
        inc FILECOUNT_CURRENT


    CleanUp:

        lda LAST_START_POINTER
        sta ZP_PTR
        lda LAST_START_POINTER+1
        sta ZP_PTR+1  
    jsr CLRCHN
    lda #1
    jsr CLOSE      
    

rts

StoreEndOfFile:
        ;store type
        lda TMP
        StoreCurrentAInMemoryMacro

        ;store pointer for longfile name
        lda LFN_Pointer
        StoreCurrentAInMemoryMacro
        lda LFN_Pointer+1
        StoreCurrentAInMemoryMacro
        
        inc FILECOUNT_CURRENT
       ; lda #$41
       ; jsr CHROUT
        ;backup start if next line
        lda ZP_PTR
        sta LAST_START_POINTER
        lda ZP_PTR+1
        sta LAST_START_POINTER+1
        
       
        stz TMP3
        
        ldx FILECOUNT_CURRENT
        cpx SET_MAX_FILES_COUNT
        bpl Eof444 ;EndOfFile2   ;maximum files per read
        
        ldx #0  ;search of next line

        
rts

Eof444:
    inc TMP3   ;tmp=1 -> is stop reading
    ldx #0    
rts

fSetGeneralPointer:
    ;sGeneral is a global address used for different puposes
    pha
        lda #<sGeneral
        sta ZP_PTR
        lda #>sGeneral
        sta ZP_PTR+1    
    pla
rts

PrintScrnLineBlank2:
    phy
    phx
        sec
        jsr PLOT    ;read cur post
        
        lda #$7D
        jsr CHROUT
        
        clc
        lda #1
        adc WINDOW_WIDTH
        tay
        
        clc
        jsr PLOT

        lda #$7D
        jsr CHROUT
        
        tya
        adc WINDOW_WIDTH
        adc #1
        tay
        
        clc
        jsr PLOT

        lda #$7D
        jsr CHROUT 
    
        jsr print_lf
    plx
    ply
    
rts

PrintScrnLineBlank:
    phy
    phx
        sec
        jsr PLOT    ;read cur post
        
        lda #$7D
        jsr CHROUT
        
        ldy #39
        clc
        jsr PLOT

        lda #$7D
        jsr CHROUT
        
        ldy #78
        clc
        jsr PLOT

        lda #$7D
        jsr CHROUT 
    
        jsr print_lf
    plx
    ply
    
rts
ResetColor:
    lda #$1F    ;first background
    jsr CHROUT
    lda #01
    jsr CHROUT
    lda #$05    ;set foreground
    jsr CHROUT
rts



PrintMainScreen2:
jmp PrintMainScreen2_start
    WINDOW_WIDTH: .byte $0
    
    ;----------- 80 cols ---------------------------
    scrnTitel80: .asciiz "                       - x16 ultimate file manager v0.4b -"
    scrnLineTop80:.byte $B0
                .byte $63,$63,$63,$63,$63,$63,$63,$63,$63,$63, $63,$63,$63,$63,$63,$63,$63,$63,$63,$63, $63,$63,$63,$63,$63,$63,$63,$63,$63,$63, $63,$63,$63,$63,$63,$63,$63,$63
                .byte $b2
                .byte $63,$63,$63,$63,$63,$63,$63,$63,$63,$63, $63,$63,$63,$63,$63,$63,$63,$63,$63,$63, $63,$63,$63,$63,$63,$63,$63,$63,$63,$63, $63,$63,$63,$63,$63,$63,$63,$63
                .byte $AE
                .byte $20
    .byte $0

    scrnLineHeader80:.byte $7D
                .byte $9e
                .byte "-filename-     -size-   -date/time-   "
                .byte $05,$7D,$9e
                .byte "-filename-     -size-   -date/time-   "
                .byte $05,$7D
                .byte $20
    .byte $0

    scrnLineMiddle80:.byte $AB
                .byte $63,$63,$63,$63,$63,$63,$63,$63,$63,$63, $63,$63,$63,$63,$63,$63,$63,$63,$63,$63, $63,$63,$63,$63,$63,$63,$63,$63,$63,$63, $63,$63,$63,$63,$63,$63,$63,$63
                .byte $7B
                .byte $63,$63,$63,$63,$63,$63,$63,$63,$63,$63, $63,$63,$63,$63,$63,$63,$63,$63,$63,$63, $63,$63,$63,$63,$63,$63,$63,$63,$63,$63, $63,$63,$63,$63,$63,$63,$63,$63
                .byte $B3
                .byte $20
    .byte $0
                                    
    scrnLineEnd80:.byte $AD
                .byte $63,$63,$63,$63,$63,$63,$63,$63,$63,$63, $63,$63,$63,$63,$63,$63,$63,$63,$63,$63, $63,$63,$63,$63,$63,$63,$63,$63,$63,$63, $63,$63,$63,$63,$63,$63,$63,$63
                .byte $B1
                .byte $63,$63,$63,$63,$63,$63,$63,$63,$63,$63, $63,$63,$63,$63,$63,$63,$63,$63,$63,$63, $63,$63,$63,$63,$63,$63,$63,$63,$63,$63, $63,$63,$63,$63,$63,$63,$63,$63
                .byte $BD
                .byte $20                    

    .byte $00
    
    scrnLineButtons80:  
                        .byte " f2=info f3=part" ;"
                        .byte " f4=edit f5=copy f6=rename f7=move f8=mkdir f10=quit del=delete "
                        .byte $0

    scrnBlank80: .res 80,$20
                 .byte $0

    ;----------- 64 cols ---------------------------
    scrnTitel64: .asciiz "               - x16 ultimate file manager v0.4b -"
    scrnLineTop64:.byte $B0
                .byte $63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63, $63,$63,$63,$63,$63,$63,$63,$63,$63,$63, $63,$63,$63,$63,$63,$63,$63,$63
                .byte $b2
                .byte $63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63, $63,$63,$63,$63,$63,$63,$63,$63,$63,$63, $63,$63,$63,$63,$63,$63,$63,$63
                .byte $AE
                .byte $0d
    .byte $0

    scrnLineHeader64:.byte $7D
                .byte $9e
                .byte "-filename-     -size-  -date- "
                .byte $05,$7D,$9e
                .byte "-filename-     -size-  -date- "
                .byte $05,$7D
                .byte $0d
    .byte $0

    scrnLineMiddle64:.byte $AB
                .byte $63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63, $63,$63,$63,$63,$63,$63,$63,$63,$63,$63, $63,$63,$63,$63,$63,$63,$63,$63
                .byte $7B
                .byte $63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63, $63,$63,$63,$63,$63,$63,$63,$63,$63,$63, $63,$63,$63,$63,$63,$63,$63,$63
                .byte $B3
                .byte $0d
    .byte $0
                                    
    scrnLineEnd64:.byte $AD
                .byte $63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63, $63,$63,$63,$63,$63,$63,$63,$63,$63,$63, $63,$63,$63,$63,$63,$63,$63,$63
                .byte $B1
                .byte $63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63, $63,$63,$63,$63,$63,$63,$63,$63,$63,$63, $63,$63,$63,$63,$63,$63,$63,$63
                .byte $BD
                .byte $0d                   

    .byte $00
    
    scrnLineButtons64: .byte "   f2=info f3=part f4=edit f5=copy f6=rename f7=move f8=mkdir",$0d,"                      f10=quit del=delete ",$0




    ;----------- 40 cols ---------------------------
    scrnTitel40: .asciiz "  - x16 ultimate file manager v0.4b -"
    scrnLineTop40:.byte $B0
                .byte $63,$63,$63,$63,$63,$63,$63,$63,$63,$63, $63,$63,$63,$63,$63,$63,$63,$63
                .byte $b2
                .byte $63,$63,$63,$63,$63,$63,$63,$63,$63,$63, $63,$63,$63,$63,$63,$63,$63,$63
                .byte $AE
                .byte $0d
    .byte $0

    scrnLineHeader40:.byte $7D
                .byte $9e
                .byte "-filename-        "
                .byte $05,$7D,$9e
                .byte "-filename-        "
                .byte $05,$7D
                .byte $0d,$05
    .byte $0

    scrnLineMiddle40:.byte $AB
                .byte $63,$63,$63,$63,$63,$63,$63,$63,$63,$63, $63,$63,$63,$63,$63,$63,$63,$63
                .byte $7B
                .byte $63,$63,$63,$63,$63,$63,$63,$63,$63,$63, $63,$63,$63,$63,$63,$63,$63,$63
                .byte $B3
                .byte $0d
    .byte $0
                                    
    scrnLineEnd40:.byte $AD
                .byte $63,$63,$63,$63,$63,$63,$63,$63,$63,$63, $63,$63,$63,$63,$63,$63,$63,$63
                .byte $B1
                .byte $63,$63,$63,$63,$63,$63,$63,$63,$63,$63, $63,$63,$63,$63,$63,$63,$63,$63
                .byte $BD
                .byte $0d                    

    .byte $00
    
    scrnLineButtons40: .byte "f2=info f3=part f4=edit  f5=copy f6=ren",$0d,"       f7=move f8=mkdir f10=quit",$0


    
PrintMainScreen2_start:
    ;set number of lines to 255 to prevent scrolling on writing past the screen
  ;  lda #$FF
  ;  sta $0387
    
    
    lda #$8F
    jsr CHROUT
    lda #$0E
    jsr CHROUT
    
    jsr ResetColor
    jsr clearscreen    

    lda #2
    jsr $FF62   ;set charset
 
    clc
    lda SCREEN_ROWS
    sbc #10
    sta ListSize 
    
    lda SCREEN_COLS
    cmp #80
    beq To80Cols
    cmp #40
    beq Goto_To40Cols
    cmp #64
    beq Goto_To64Cols

    
    
    rts 
    Goto_To40Cols:
        jmp To40Cols
    Goto_To64Cols:
        jmp To64Cols
    To80Cols:
        lda #38
        sta  WINDOW_WIDTH
        lda #10
        sta SET_DATE_LEN

            jsr print_lf
            PrintLine scrnTitel80
            jsr print_lf
            jsr print_lf
            PrintLine scrnLineTop80
            jsr PrintScrnLineBlank2 ;PrintLine scrnLineBlank
            PrintLine scrnLineMiddle80
            PrintLine scrnLineHeader80
            ldx ListSize
            @loop:
                jsr PrintScrnLineBlank2 
                ;PrintLine scrnLineBlank
                dex
                beq @loopdone
                bra @loop
            @loopdone:
            PrintLine scrnLineEnd80
           jsr print_lf
            PrintLine scrnLineButtons80

            lda #1
            sta SET_SHOWDATE
            sta SET_SHOWTIME
            sta SET_SHOWSIZE
            stz SET_SHOWTYPE       
        
        
        jmp ContinuePrint
 
     To64Cols:
        lda #30
        sta  WINDOW_WIDTH
        lda #8 ;skip first 2 bytes
        sta SET_DATE_LEN
        
            jsr print_lf
            PrintLine scrnTitel64
            jsr print_lf
            jsr print_lf
            PrintLine scrnLineTop64
            jsr PrintScrnLineBlank2 ;PrintLine scrnLineBlank
            PrintLine scrnLineMiddle64
            PrintLine scrnLineHeader64
            ldx ListSize
            @loop:
                jsr PrintScrnLineBlank2 
                ;PrintLine scrnLineBlank
                dex
                beq @loopdone
                bra @loop
            @loopdone:
            PrintLine scrnLineEnd64
            PrintLine scrnLineButtons64
            
            lda #1
            sta SET_SHOWDATE
            stz SET_SHOWTIME
            sta SET_SHOWSIZE
            stz SET_SHOWTYPE       
        
        
        jmp ContinuePrint
        
    To40Cols:
        
        lda #18
        sta  WINDOW_WIDTH
        lda #10 
        sta SET_DATE_LEN
        
            jsr print_lf
            PrintLine scrnTitel40
            jsr print_lf
            jsr print_lf
            PrintLine scrnLineTop40
            jsr PrintScrnLineBlank2 ;PrintLine scrnLineBlank
            PrintLine scrnLineMiddle40
            PrintLine scrnLineHeader40
            ldx ListSize
            @loop:
                jsr PrintScrnLineBlank2 
                ;PrintLine scrnLineBlank
                dex
                beq @loopdone
                bra @loop
            @loopdone:
            PrintLine scrnLineEnd40
           ; jsr print_lf
            PrintLine scrnLineButtons40
            
            stz SET_SHOWDATE
            stz SET_SHOWTIME
            stz SET_SHOWSIZE
            lda #1
            sta SET_SHOWTYPE

        jmp ContinuePrint
   


        
    ContinuePrint: 

    
    jsr ResetColor

rts

PrintMainScreen:
    ;Mode 64x50/64x25 
    ;Per window: 30 chars
    ;  12 chars filename
    ;   1 space
    ;   7 chars size    ; is now 9!!   65000
    ;   1 space
    ;   10 date??   ;change to 8: 23-01-12 YY-MM-DD
    ;  totaal: 31   one to many!

    jmp PrintMainScreen_start
    
    
    
        scrnTitel: .asciiz "                       - x16 ultimate file manager v0.3b -"
        scrnLineTop:.byte $B0
                    .byte $63,$63,$63,$63,$63,$63,$63,$63,$63,$63, $63,$63,$63,$63,$63,$63,$63,$63,$63,$63, $63,$63,$63,$63,$63,$63,$63,$63,$63,$63, $63,$63,$63,$63,$63,$63,$63,$63
                    .byte $b2
                    .byte $63,$63,$63,$63,$63,$63,$63,$63,$63,$63, $63,$63,$63,$63,$63,$63,$63,$63,$63,$63, $63,$63,$63,$63,$63,$63,$63,$63,$63,$63, $63,$63,$63,$63,$63,$63,$63,$63
                    .byte $AE
                    .byte $20
        .byte $0

        scrnLineBlank:.byte $7D
                    .byte "          ", "          ", "          ", "        "
                    .byte $7D
                    .byte "          ", "          ", "          ", "        "
                    .byte $7D
                    .byte $20
        .byte $0
        ;filename: 12 chrs
        ;size: 9 chars
        ;total: 21 chars does not fit in 40
        scrnLineHeader:.byte $7D
                    .byte "-filename-     -size-   -date/time-   "
                    .byte $7D
                    .byte "-filename-     -size-   -date/time-   "
                    .byte $7D
                    .byte $20
        .byte $0

        scrnLineMiddle:.byte $AB
                    .byte $63,$63,$63,$63,$63,$63,$63,$63,$63,$63, $63,$63,$63,$63,$63,$63,$63,$63,$63,$63, $63,$63,$63,$63,$63,$63,$63,$63,$63,$63, $63,$63,$63,$63,$63,$63,$63,$63
                    .byte $7B
                    .byte $63,$63,$63,$63,$63,$63,$63,$63,$63,$63, $63,$63,$63,$63,$63,$63,$63,$63,$63,$63, $63,$63,$63,$63,$63,$63,$63,$63,$63,$63, $63,$63,$63,$63,$63,$63,$63,$63
                    .byte $B3
                    .byte $20
        .byte $0
                                        
        scrnLineEnd:.byte $AD
                    .byte $63,$63,$63,$63,$63,$63,$63,$63,$63,$63, $63,$63,$63,$63,$63,$63,$63,$63,$63,$63, $63,$63,$63,$63,$63,$63,$63,$63,$63,$63, $63,$63,$63,$63,$63,$63,$63,$63
                    .byte $B1
                    .byte $63,$63,$63,$63,$63,$63,$63,$63,$63,$63, $63,$63,$63,$63,$63,$63,$63,$63,$63,$63, $63,$63,$63,$63,$63,$63,$63,$63,$63,$63, $63,$63,$63,$63,$63,$63,$63,$63
                    .byte $BD
                    .byte $20                    

        .byte $00
        
        scrnLineButtons: .asciiz "f2=info f4=edit f5=copy f6=rename f8=mkdir f10=quit del=delete "

    PrintMainScreen_start:
    lda #2
    jsr $FF62   ;set charset to ISO
    ;reset screencolor. Probably not the best solution
    jsr ResetColor
    
    jsr clearscreen    
 
    clc
    lda SCREEN_ROWS
    sbc #9
    sta ListSize
    
    jsr print_lf
    PrintLine scrnTitel
    jsr print_lf
    jsr print_lf
    PrintLine scrnLineTop
    jsr PrintScrnLineBlank ;PrintLine scrnLineBlank
    PrintLine scrnLineMiddle
    PrintLine scrnLineHeader
    ldx ListSize
    loop:
        jsr PrintScrnLineBlank 
        ;PrintLine scrnLineBlank
        dex
        beq loopdone
        bra loop
    loopdone:
    PrintLine scrnLineEnd
    PrintLine scrnLineButtons
    
    ldx #6
    ldy #1
    jsr PLOTVeraColor
    lda #$67
    ldx #1
    @next:
        STA $9F23
        inx
        cpx #38
        beq @Klaar
    jmp @next

@Klaar:

    ldx #6
    ldy #41
    jsr PLOTVeraColor
    lda #$67
    ldx #1
    @next2:
        STA $9F23
        inx
        cpx #38
        beq @Klaar2
    jmp @next2

@Klaar2:
rts

PrintAToVera:
    jsr ConvertPetsciiToVera
    STA $9F23
rts

PLOTVera:
    ;x=line
    ;y=col
    ;line-length: 256 bytes

    jsr InitAdder24bit
    
    ;VERA_START_TEXT =$21B000

    lda #$00                ;low
    sta Adder24bitValue

    lda #$B0                ;middle
    sta Adder24bitValue+1
    
    lda #$21                ;high
    sta Adder24bitValue+2
    
    ;add lines
    cpx #$0
    beq PlotCol ;Line is Ok
        phx
           
            ;set value to add       $0100   = 256
            lda #$00
            sta Adder24bitToAdd    
            lda #$01
            sta Adder24bitToAdd+1  
        
            @loop:
                jsr Adder24bit  ;add 256 for each line as 1 line is 256 bytes

                dex
                cpx #0
            bne @loop            
        plx
    
    PlotCol:
        phy
            
            tya     ;y contains byte x2
            sta Adder24bitToAdd    
            lda #$00
            sta Adder24bitToAdd+1  
            jsr Adder24bit  ;2 times
            jsr Adder24bit     
        ply
        
    LDA #$00
    STA $9F25   ;VERA CTRL

    LDA Adder24bitValue                ;$N1B000
    STA $9F20   ;ADDRES_L

    LDA Adder24bitValue+1    ;ADDRES_M   B0      ;pos
    STA $9F21

    LDA Adder24bitValue+2    ;ADDRES_H -> $2X for increment with two's, $X1 address
    STA $9F22    
    
         
    
rts

PLOTVeraColor:
    ;x=line
    ;y=col
    ;line-length: 256 bytes

    jsr InitAdder24bit
    
    ;VERA_START_TEXT =$21B000

    lda #$01                ;low
    sta Adder24bitValue

    lda #$B0                ;middle
    sta Adder24bitValue+1
    
    lda #$21                ;high
    sta Adder24bitValue+2
    
    ;add lines
    cpx #$0
    beq PlotColX ;Line is Ok
        phx
           
            ;set value to add       $0100   = 256
            lda #$00
            sta Adder24bitToAdd    
            lda #$01
            sta Adder24bitToAdd+1  
        
            @loop:
                jsr Adder24bit  ;add 256 for each line as 1 line is 256 bytes

                dex
                cpx #0
            bne @loop            
        plx
    
    PlotColX:
        phy
            
            tya     ;y contains byte x2
            sta Adder24bitToAdd    
            lda #$00
            sta Adder24bitToAdd+1  
            jsr Adder24bit  ;2 times
            jsr Adder24bit     
        ply
        
    LDA #$00
    STA $9F25   ;VERA CTRL

    LDA Adder24bitValue                ;$N1B000
    STA $9F20   ;ADDRES_L

    LDA Adder24bitValue+1    ;ADDRES_M   B0      ;pos
    STA $9F21

    LDA Adder24bitValue+2    ;ADDRES_H -> $2X for increment with two's, $X1 address
    STA $9F22    
    
         
    
rts

PLOTVeraSingle:
    ;x=line
    ;y=col
    ;line-length: 256 bytes

    jsr InitAdder24bit
    
    ;VERA_START_TEXT =$21B000

    lda #$00                ;low
    sta Adder24bitValue

    lda #$B0                ;middle
    sta Adder24bitValue+1
    
    lda #$11                ;high
    sta Adder24bitValue+2
    
    ;add lines
    cpx #$0
    beq @PlotColX ;Line is Ok
        phx
           
            ;set value to add       $0100   = 256
            lda #$00
            sta Adder24bitToAdd    
            lda #$01
            sta Adder24bitToAdd+1  
        
            @loop:
                jsr Adder24bit  ;add 256 for each line as 1 line is 256 bytes

                dex
                cpx #0
            bne @loop            
        plx
    
    @PlotColX:
        phy
            
            tya     ;y contains byte x2
            sta Adder24bitToAdd    
            lda #$00
            sta Adder24bitToAdd+1  
            jsr Adder24bit  ;2 times
            jsr Adder24bit     
        ply
        
    LDA #$00
    STA $9F25   ;VERA CTRL

    LDA Adder24bitValue                ;$N1B000
    STA $9F20               ;ADDRES_L

    LDA Adder24bitValue+1    ;ADDRES_M   B0      ;pos
    STA $9F21

    LDA Adder24bitValue+2    ;ADDRES_H -> $2X for increment with two's, $X1 address
    STA $9F22    
    
 ;   lda $9F23   ;reach 
    
         
    
rts

CopyUsingDos:
    jmp CopyUsingDos_start
    ;     DOS"C2:CALCX.PRG=1:CALC.PRG"  (dest=source)
    DOS_COPY: .asciiz "c[2]:[r]/[f]=[1]:[s]/[f]"

    CopyUsingDos_start:

    CopyAddrUntilZero DOS_COPY,CMD_BUFFER
    
    lda #<CMD_BUFFER
    sta ZP_TEXT_PRT
    lda #>CMD_BUFFER
    sta ZP_TEXT_PRT+1
    
    jsr ReplaceParams   ;replace message params with values
    stz CMD_BUFFER_LENGTH
    jsr DoDosCMD
 ;   PrintUntilZero CMD_BUFFER
 ;   jsr print_lf
 ;   PrintUntilZero CMD_BUFFER_RESULT
 ;   jsr WaitKey
rts

CopyAFile:
   jmp CopyAFile_Start

   SourceFile: .res 255 ; .byte "/ccc/copyt.txt" ; .res 255
               .byte $0
   SourceFileCnt: .byte $0E
   DestFile: .res 255 ; .byte "/ddd/copyt.txt,s,w" ; .res 255
             .byte $0
    DEVICE_DEST: .byte $0
    
   DestFileCnt: .byte $12
   CD_ROOT: .asciiz "cd:/"

   CopyBuffer: .res 255
   IsEof: .byte $0
   CopyBuffercnt:    .byte $0 
   
    PB_BLOCKS: .byte $0,$0  ;file size in blocks of 254 bytes
    PB_COUNTER: .byte $0,$0
    PB_COUNT_CHECK: .byte $0
    PB_LENGTH: .byte 36
    PB_CHARS: .byte 0
    
CopyAFile_Start:  

    ;if one of them is partition=hostfs: own copt works
    lda PARTITION_CURRENT
    cmp #0
    beq @ContinueCopy
    lda PARTITION_OTHER
    cmp #0
    beq @ContinueCopy
    ;source and dest are on sd-card, maybe on different partitions
    lda PARTITION_CURRENT
    cmp PARTITION_OTHER
    beq @ContinueCopy   ;same partition: will work
        ;save device, other partition: will not work use DOS"C: instead
        jmp CopyUsingDos
    


    @ContinueCopy:
    ;change cur dir to root
    CopyAddrUntilZero CD_ROOT,CMD_BUFFER,CMD_BUFFER_LENGTH
    jsr DoDosCMD

    
    CopyAddrUntilZero CD_ROOT,CMD_BUFFER,CMD_BUFFER_LENGTH
    inc DOS_CMD_DEVICE  ;on other device also
    jsr DoDosCMD


    ;Check if is tokenizes Basic

    


;count length of commands
 

   ;count length of commands

    CountUntilZero SourceFile,SourceFileCnt
    CountUntilZero DestFile,DestFileCnt

   ;Initialize progress bar

        jsr LoadCurrentFileSize

        jsr InitAdder24bit
        
        ldx #0   ;aantal keer dan 20 in blocksize past
            lda PB_LENGTH    ;low byte
            sta Adder24bitToAdd    
            lda #$00    ;high byte
            sta Adder24bitToAdd+1  

        @nextcount:
                jsr Adder24bit  ;add 20 to result
                inx
                ;check if size in adder>size in PB_BLOCKS
                lda Adder24bitValue+1   ;high byte
                cmp PB_BLOCKS+1   ;high byte of total of blocks
                beq @CheckLowByte
                cmp PB_BLOCKS+1
                bpl @DoneOneLess
            jmp  @nextcount
            
            @CheckLowByte:

                lda Adder24bitValue
                cmp PB_BLOCKS
                bpl @DoneOneLess
            
            jmp @nextcount
            @DoneOneLess:
                dex
                
            @DoneCount:
    
    
            stx PB_COUNTER  ;per value in PB_COUNTER: print 1 progress byte
            stz PB_COUNT_CHECK
            stz PB_CHARS


    clc
     ldx MODAL_ROW
    inx
    inx
    inx
    inx
    inx
    inx
    inx
    inx
                    
    ldy MODAL_COL    
    iny
    iny
    
  ;  ldx #30
  ;  ldy #21
    jsr PLOT
    
    ;print bar
    ldx #0
    @lpx:
        lda #$20
        jsr CHROUT
        inx
        cpx PB_LENGTH
        bne @lpx
    clc

    ldx MODAL_ROW
    inx
    inx
    inx
    inx
    inx
    inx
    inx
    inx
        
    ldy MODAL_COL    
    iny
    iny
        
   ; ldx #30
   ; ldy #21
    jsr PLOT
    
    ;set color
     lda #$9B    ;set foreground
    jsr CHROUT       
    lda #01 ;flip
    jsr CHROUT
    
;    lda iCurrentWindow
;    cmp #0
;    bne @UseDeviceLeft
;        lda DEVICE_RIGHT
 ;       sta DEVICE_OTHER   
;    jmp @Continue
 ;   @UseDeviceLeft:
;        lda DEVICE_LEFT
;        sta DEVICE_OTHER
;    @Continue:

    
    
    
 ;   PrintUntilZero SourceFile
  ;  PrintUntilZero DestFile
    ;jsr WaitKey
    
    jsr SelectCurrentPartition
   ;Open file for INPUT    
   lda #1  
   ldx DEVICE_CURRENT ;#8  
   ldy #2  
   jsr SETLFS
   lda SourceFileCnt
   ldx #<SourceFile
   ldy #>SourceFile
   jsr SETNAM
   jsr OPEN

    lda PARTITION_CURRENT
    pha
    
    lda PARTITION_OTHER
    sta PARTITION_CURRENT
   ; jsr SelectCurrentPartition ;this results in an error.  Change partitions with file open probably not good

    ;Open file for OUTPUT
    lda #2
    ldx DEVICE_OTHER ;#8  
    ldy #1   
    jsr SETLFS
    lda DestFileCnt 
    ldx #<DestFile
    ldy #>DestFile
    jsr SETNAM
    jsr OPEN
   
    pla
    sta PARTITION_CURRENT
   

    

   stz CopyBuffercnt
   stz IsEof
   loopCopyByte:

     
      ;set input channel
      LDX #1
      jsr CHKIN   
   
      ldx #0
      ReadLoop:   ;read 255 bytes in sequence, or until EOF
         ;Read byte to A
         jsr CHRIN
         sta CopyBuffer,x
         ;jsr CHROUT
         inx
         
      
         ;check end of file (or error)
         jsr READST 
         bne IsEndOfFile  ;eof file, or error
   
         cpx #255 ;check end of buffer
         beq ExitReadLoop
         
         bra ReadLoop
         

         
      IsEndOfFile:
         
         lda #1
         sta IsEof
         
      ExitReadLoop:
      inx
      stx CopyBuffercnt  ;count of current buffer  
      
     
      
      ;Set output channel
      LDX #2
      jsr CHKOUT 
      
      ldx #0
      WriteLoop:
         
         ;Save byte
         lda CopyBuffer,x
         inx
         cpx CopyBuffercnt
         beq ExitWriteLoop
         jsr CHROUT
      bra WriteLoop
      
      ExitWriteLoop:
         lda IsEof
         cmp #1
         beq @eof
 
 
    jsr CLRCHN
 ;update progressbar if needed
      inc PB_COUNT_CHECK
     
      lda PB_COUNT_CHECK
      cmp PB_COUNTER
      beq @DoUpdate
      jmp @NoUpdate
      @DoUpdate:
        lda PB_CHARS
        cmp PB_LENGTH
        beq @NoUpdate
        lda #$20
        jsr CHROUT
        stz PB_COUNT_CHECK
        inc PB_CHARS
      @NoUpdate:
      
      
   jmp loopCopyByte



   @eof:


   jsr CLRCHN
   lda #2
   jsr CLOSE
   lda #1
   jsr CLOSE
   jsr ResetColor

rts

CheckFileType:
    ;check if current file is tokenized basic

   jsr GotoCurrentDirectory


    CopyAddrUntilZero TSRPRG,CMD_BUFFER
    
    lda #<CMD_BUFFER
    sta ZP_TEXT_PRT
    lda #>CMD_BUFFER
    sta ZP_TEXT_PRT+1

    jsr ReplaceParams   ;replace message params with values      
    
    lda #1   ; Logical Number = 1
    ldx $03fe ; #8   ; Device = "SD card" (emulation host FS)
    ldy #0   ; 0=skip header byts, 1=use header butes
    jsr SETLFS  
    
    ;count length filename

    
    CountUntilZero CMD_BUFFER,CNT
    lda CNT
    ldx #<CMD_BUFFER
    ldy #>CMD_BUFFER    
    jsr SETNAM
      
    jsr OPEN
    
    LDX #1
    jsr CHKIN

    ldx #0
    stz TMP
    ldy #0
    @Nextbyte:
        jsr READST
        bne toeofy
        inx
        cpx #30     ;read until 30 bytes
        beq toeofy
        
        lda TMP
        cmp #3
        beq ReadyNoEOF        ;a byte after $00$00$00 -> assembler, else basic
        
        jsr CHRIN   ;read byte
       ; jsr print_hex
        cmp #0
        bne @NoZero 
        
        ;is $00, count in a row
        inc TMP
             
        jmp @Nextbyte
        @NoZero:
        stz TMP ;reset $00 counter
        
        
        jmp @Nextbyte

    ReadyNoEOF:
        ;is assembler
        jsr CLRCHN
        lda #1
        jsr CLOSE  
        lda #$41    ;A ;assembler
        sta TSRTYPE
        rts          

    toeofy:
    
    
    
    jsr CLRCHN
    lda #1
    jsr CLOSE 

    lda #$42    ;B ;basic
    sta TSRTYPE    
rts

GetStartDirectory:
jmp GetStartDirectory_start
    CURDIRCMD: .asciiz "$=c"
    CUR_PATH: .res 255
    CMD_GOTO: .byte "cd:/45/dir1"
    CUR_PATH_REV: .res 255
    CUR_PATH_LAST_CHAR: .byte $0
    .byte $00


GetStartDirectory_start:
    ;jump to other dir
  ;  CopyAddrUntilZero CMD_GOTO,CMD_BUFFER
 ;   jsr DoDosCMD
    
    CopyAddrUntilZero CURDIRCMD,CMD_BUFFER,CMD_BUFFER_LENGTH    

    lda #1   ; Logical Number = 1
    ldx #8   ; Device = "SD card" (emulation host FS)
    ldy #0   ; Secondary Address = 15: dos command channel
    jsr SETLFS


    lda CMD_BUFFER_LENGTH ; command
    ldx #<CMD_BUFFER
    ldy #>CMD_BUFFER

    jsr SETNAM
    
    jsr OPEN
    
    LDX #1
    jsr CHKIN
    
    ldy #0
    :
        jsr CHRIN
        iny
        cpy #26
        beq @StartReading
    jmp :-
    
    @StartReading:
    ldx #0
    ldy #0
    lda #$2F
    sta CUR_PATH_REV,y
    iny   
    
    @read:
       
        jsr READST
        bne @eof

        jsr CHRIN
        cpx #0
        beq @CheckStartName
    
        cpx #1
        beq @StoreName    
    
       
    jmp @read
    
    @CheckStartName:    
     
        cmp #$22    ;"
        bne @read   ;no quote, so continu reading
        ldx #1  ;store filenames    ;quote found, start storing name
        
    jmp @read
    
    @StoreName:
        cmp #$22    ;"
        beq @NameDone
       
        cmp #$2F
        beq @DoNotAdd
        sta CUR_PATH_REV,y
        iny
        
        @DoNotAdd:
        sta CUR_PATH_LAST_CHAR
    jmp @read
    
    @NameDone:
      
        lda CUR_PATH_LAST_CHAR
        cmp #$2F
        beq @DoNotAddSlash
            lda #$2F
            sta CUR_PATH_REV,y
            iny       
        @DoNotAddSlash:
        ldx #0
    jmp @read
    
    @eof:
   
    cpy #0
    bne @NotZeroLength
    lda #$2f
    sta CUR_PATH_REV,y
    iny   
    @NotZeroLength:
    lda #0
    sta CUR_PATH_REV,y
    
  
 
    jsr CLRCHN
    lda #1
    jsr CLOSE

 ;     lda #COLON
  ;    jsr CHROUT
  ;  PrintUntilZero CUR_PATH_REV
  ;    lda #COLON
  ;    jsr CHROUT
    

    cpy #1  ;length 1; do nothing
    beq @FillAndReturn
    ;reverse order
    
    dey ;set pointer on last char
    sty TMP ;store pointer of y
    ldx #0
    @rd:
        lda CUR_PATH_REV,y
        
        cmp #$2f  ;/
        beq @SlashFound
        
        cpy #0
        beq @Ready
        
        
        dey
    jmp @rd
    
    @SlashFound:


        sty TMP2    ;save y to restore later
        @InnerRead:
            cpy TMP
            beq @ContinueRead
            
            lda CUR_PATH_REV,y
            sta CUR_PATH,x
            inx
            iny
        jmp @InnerRead
        
    jmp @rd
    
    @ContinueRead:
        ldy TMP2    ;restore Y
        cpy #0
        beq @Ready
        sty TMP ;set pointer op /
        dey
    jmp @rd
    @Ready:
    
 rts   
 @FillAndReturn:    
    lda #$2F
    sta CUR_PATH
    lda #0
    sta CUR_PATH+1
    
    
rts







IsLaunchable:
    lda #1   ; Logical Number = 1
    ldx DEVICE_CURRENT ;#8   ; Device = "SD card" (emulation host FS)
    ldy #2   ; skip 2 bytes
    jsr SETLFS
    ;get length
    ldy #0
    @nxt:
        lda CMD_BUFFER,y
        iny
        cmp #0
        bne @nxt
      
    tya
    ldx #<CMD_BUFFER
    ldy #>CMD_BUFFER      
    jsr SETNAM
    jsr OPEN
    
    ;0=not launchable
    lda #0
    sta TMP
    
    LDX #1
    jsr CHKIN
    
    ;first char should be $01
    jsr CHRIN
    cmp #$01    
    bne @Ready
    
    ;next char should be $01
    jsr CHRIN
    cmp #$08    
    bne @Ready
    
    lda #1  ;1=launchable
    sta TMP
    
    @Ready:
    jsr CLRCHN
    lda #1
    jsr CLOSE   
    
rts

;Addresses	Description
;$00000-$12BFF	320x240@256c Bitmap
;$12C00-$12FFF	unused (1024 bytes)
;$13000-$1AFFF	Sprite Image Data (up to $1000 per sprite at 64x64 8-bit)
;;$1B000-$1EBFF	Text Mode
;$1EC00-$1EFFF	unused (1024 bytes)
;$1F000-$1F7FF	Charset
;***$1F800-$1F9BF	unused (448 bytes)
;$1F9C0-$1F9FF	VERA PSG Registers (16 x 4 bytes)
;$1FA00-$1FBFF	VERA Color Palette (256 x 2 bytes)
;$1FC00-$1FFFF	VERA Sprite Attributes (128 x 8 bytes)

StoreStartDirInVRAM:
    sec
    jsr PLOT
    phy
    phx
        lda #$00
        STA $9F20   ;ADDRES_L
        lda #$F8
        STA $9F21   ;M
        lda #$11
        STA $9F22   ;
        
        ldy #0
        @nxt:
            lda CUR_PATH,y
            cmp #0
            beq @Ready
            sta $9F23
            iny
        jmp @nxt
        @Ready:
       
       jmp NoFileName
        ;include /fm.prg
        lda #$2F
        sta $9F23
        lda #$46
        sta $9F23
        lda #$4d
        sta $9F23
        lda #$2e
        sta $9F23
        lda #$50
        sta $9F23
        lda #$52
        sta $9F23
        lda #$47
        sta $9F23
        NoFileName:
       
        lda #0
        sta $9F23
    plx
    ply
    clc
    jsr PLOT        
rts

LoadStartDirFromVRAM:
    sec
    jsr PLOT
    phy
    phx
        lda #$00
        STA $9F20   ;ADDRES_L
        lda #$F8
        STA $9F21   ;M
        lda #$11
        STA $9F22   ;
        
        ldy #0
        @nxt:
            lda $9F23
            cmp #0
            beq @Ready
            sta CMD_BUFFER,y
            iny
        jmp @nxt
    @Ready:
    lda #0
    sta CMD_BUFFER,y
    plx
    ply
    clc
    jsr PLOT
rts

CopyLaunchToMem:
;$21: r0 = r0L = $02, r0H = $03, r1 = r1L = $04 etc.
    ;set source
    lda #<launch_prg
    sta $02     ;r0Low
    lda #>launch_prg
    sta $03     ;r0High

    ;set destionation $8000
    lda #$00
    sta $04     ;r1Low
    lda #$80
    sta $05     ;r1High
    
    ;calculate and set length
    sec				; set carry for borrow purpose
	lda #<launch_prg_end  ;num1lo
	sbc #<launch_prg ;num2lo			; perform subtraction on the LSBs
	sta $06
    
	lda #>launch_prg_end    ;num1hi			; do the same for the MSBs, with carry
	sbc #>launch_prg			; set according to the previous result
	sta $07
    jsr $FEE7   ;memcopy kernal function
   

rts

 .include "functions.s"
 
launch_prg:
   .incbin "launch.prg",$01 ;skip first byte
launch_prg_end: