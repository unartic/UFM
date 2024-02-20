.segment "RAMBANK63"    ;4096 bytes
    ;rambank 63 is for temporary variable storage. Should be set accordingly
    CUR_PATH:           .res 255
    SourceFile:         .res 255
    DestFile:           .res 255
    InputBuffer:        .res 255   
    TMPSTRING2:         .res 255   ;don;t like this one.Only used for file info function! (conflicts with TMPSTRING)

    DIR_CURRENT:        .res 500    ;current path
    DIR_LEFT:           .res 500
    DIR_RIGHT:          .res 500    
    MSG_MODAL:          .res 255
    
    
.segment "RAMBANK62"
    MODAL_SCREEN_BACKUP: .res 2000  ;write to a memorybank, saves a lot of space 
.segment "GOLDENRAM"
;Window Vars
    ;order and location of the first two symbols is important for launch.prg to work!!
    CMD_BUFFER:    .res 255    ;max length of CD: and the full path
    FileInfo_Name: .res 255     
    
    RAMBANK_BACKUP: .res 1
    iCurrentWindow: .res 1  ;   0=left window, 1=rightwindow
    ListSize:       .res 1  ;   lines on screen, depends on screen mode
    ListCounter:    .res 1  ;   Temporary storage of current list item 

    OFFSET_CURRENT: .res 1  ;   Offset of files list of current window    
    OFFSET_LEFT:    .res 1  ;   Offset of files list of left window
    OFFSET_RIGHT:   .res 1  ;   Offset of files list of right window

    ROW_CURRENT:    .res 1  ;   index-offset of highlighted file current window
    ROW_LEFT:       .res 1  ;   index-offset of highlighted file left window
    ROW_RIGHT:      .res 1  ;   index-offset of highlighted file right window

    DEVICE_CURRENT: .res 1  ;   device number of current window
    DEVICE_LEFT:    .res 1  ;   device number of left window
    DEVICE_RIGHT:   .res 1  ;   device number of right window
    DEVICE_OTHER:   .res 1  ;   device number of the other window
    DEVICE_START:   .res 1  ;   device number from where UFM has been started (needed to return after launch in RAM version)
    
    PARTITION_CURRENT:  .res 1  ;   Partition number of current window (only if not hostfs)
    PARTITION_LEFT:     .res 1  ;   Partition number of left window (only if not hostfs)
    PARTITION_RIGHT:    .res 1  ;   Partition number of right window (only if not hostfs)
    PARTITION_OTHER:    .res 1  ;   Partition number of other window (only if not hostfs)
    PARTITION_START:    .res 1  ;   Partition number from where UFM has been started (needed to return after launch in RAM version)

    PAGE_CURRENT:       .res 1  ;   Displayed page of current window
    PAGE_LEFT:          .res 1  ;   Displayed page of left window
    PAGE_RIGHT:         .res 1  ;   Displayed page of right window

    MODE_CURRENT:   .res 1  ;   Display mode of current window (0=files view, 1=select device/partition)
    MODE_LEFT:      .res 1  ;   Display mode of left window (0=files view, 1=select device/partition)
    MODE_RIGHT:     .res 1  ;   Display mode of left right (0=files view, 1=select device/partition)
    MODE_OTHER:     .res 1  ;   Display mode of other window (0=files view, 1=select device/partition)

    CNT_PARTITIONS: .res 1  ;   Number of partitions

    FILECOUNT_CURRENT:  .res 1  ;   File count of current window
    FILECOUNT_LEFT:     .res 1  ;   File count of left window
    FILECOUNT_RIGHT:    .res 1  ;   File count of right window
    
    COL_OFFSET:         .res 1  ;   Screen column offset to print in left or right window

    MODAL_COL:          .res 1  ;   Column offset of the modal window
    MODAL_ROW:          .res 1  ;   Row offset of the modal window

    MODAL_COL_COUNT:    .res 1  ;   Temp storage of column while drawing modal
    MODAL_ROW_COUNT:    .res 1  ;   Temp storage of row while drawing modal

    MODAL_WIDTH:        .res 1  ;   Width in chars of modal to be drawn
    MODAL_HEIGHT:       .res 1  ;   Height in chars of modal to be drawn

;Window settings
    SET_SHOWDATE:       .res 1  ;   Setting for displaying file dates in windows
    SET_SHOWTIME:       .res 1  ;   Setting for displaying file times in windows
    SET_SHOWSIZE:       .res 1  ;   Setting for displaying file sizes in windows
    SET_DATE_LEN:       .res 1  ;   Display lenth of file dates (64 cols needs dates printed as DD-MM-YY)    
    
    WINDOW_WIDTH:       .res 1  ;   Width of the window, based on the screen mode

;Joystick buttons
    JOY_PRESSED_A:          .res 1      ;is 1 when gamepad button A is pressed
    JOY_BTN_A_COUNTDOWN:    .res 1      ;delay counter for next press

    JOY_BTN_DOWN_COUNTDOWN: .res 1 
    JOY_PRESSED_DOWN:       .res 1

    JOY_BTN_UP_COUNTDOWN:   .res 1 
    JOY_PRESSED_UP:         .res 1

    JOY_BTN_RIGHT_COUNTDOWN: .res 1
    JOY_PRESSED_RIGHT:      .res 1

    JOY_BTN_LEFT_COUNTDOWN: .res 1 
    JOY_PRESSED_LEFT:       .res 1 

    JOY_BTN_B_COUNTDOWN:    .res 1 
    JOY_PRESSED_B:          .res 1 

    JOY_BTN_L_COUNTDOWN:    .res 1
    JOY_PRESSED_L:          .res 1 

    JOY_BTN_R_COUNTDOWN:    .res 1
    JOY_PRESSED_R:          .res 1 

    JOY_BTN_SELECT_COUNTDOWN:.res 1 
    JOY_PRESSED_SELECT:     .res 1 
 
    JOYSTICK_CUR_LINE:      .res 1 ;Highligted line of joystick select modal   

    ;jsr PLOT, but then to address VERA directly (setting address of vera)
    VERA_PLOT_X:    .res 1 
    VERA_PLOT_Y:    .res 1 
    
    ;File info vars are used to fill file information into commands and messages
    FileInfo_Date: .res 10
    FileInfo_Time: .res 5
    FileInfo_Type: .res 4  
    FileInfo_Size: .res 9 
    
    TMP_FILESIZE:               .res 2  ;Temporary storage of 2 byte file size when reading files list
    TMP_FILESIZE_DESIGNATOR:    .res 2  ;Temporary storage of 2 byte file size designator (kb/mb) when reading files list 
    
;Global multi purpose vars
    CNT:            .res 1 
    TMP:            .res 1
    TMP2:           .res 1
    TMP3:           .res 1 
    LFN_Pointer:    .res 2  ; 2 bytes to temporatily store long filename pointer

    CMD_BUFFER_LENGTH:  .res 1  ;   Length of a command to be send to channel 15, or the length of a filename for open/load
    CMD_BUFFER_RESULT:  .res 50 ;   Buffer to store result of performing dos command

    SCREEN_ROWS:        .res 1 ;    Rows of the current screen
    SCREEN_COLS:        .res 1 ;    Columns of the current screen

    DOS_DIR_TYPE:       .res 1  ;    0->read directory's, 1=read files
    DOS_CMD_DEVICE:     .res 1  ;    0=Use current device number for dos-command, 1=Use other windows device number for dos-command
    
    TSRTYPE:            .res 1 ;    File type of program tobe launched (B=basic,A=assembly)
    LAST_RDTIM:         .res 1 ;    Temporarystorage of result of low byte of RDTIM
    DEVICES_CONNECTED:  .res 20 ;   String of 1 byte devicenumbers of devices that where detected

    TMPSTRING:          .res 255
    
    RESULT_KEY:         .res 1 ;    Pressed key of a confirmation modal dialog

    PAGE_FILE_COUNT:    .res 1 ;    Maximum files per page
    PAGE_CURRENT_USE:   .res 1 ;    Number of current page
    
    LAST_START_POINTER: .res 2  ;   Temporary pointer for reading files list 

    CUR_PATH_LAST_CHAR: .res 1  ;   Last char of current path
 
 ;File Copy params
    SourceFileCnt:      .res 1 ;    Length of source file name
    DEVICE_DEST:        .res 1 ;    Device number to be used to write the file
    DestFileCnt:        .res 1 ;    Length of destination file name
    PB_BLOCKS:          .res 2  ;   File size in blocks of 255 bytes
    PB_COUNTER:         .res 2  ;   Progress bar counter
    PB_COUNT_CHECK:     .res 1  ;   Storage of when to check if prograss bar needs another block
    PB_CHARS:           .res 1  ;   Buffer of blocks to be displayed in progressbar
    IsEof:              .res 1  ;   Tmp storage of end of file
    CopyBuffercnt:      .res 1  ;   Count of bytes in current copy buffer
    CHECKFILE_RESULT:   .res 1  ;   Result of check if file exists in other window
    
;vars from functions.s
    Adder24bitValue: .res 3     ;   Value of 24bit address
    Adder24bitToAdd: .res 2     ;   Value to add to the 24 bit adder value
    
    InputLength:  .res 1        ;   Length of the inputbuffer after user input via keyboard 
    
    value_32bit: .res 4         ;   temp storage of math functions
    mod10_32bit: .res 4         ;   temp storage of math functions
    result_32bit: .res 12       ;   temp storage of math functions
    resultindex_32bit: .res 1   ;   temp storage of math functions    

    LaunchAddressAscii: .res 4  ;   result of 2 byte hex to ascii conversion
    
.segment "DATA"
    ;DOS-commands
    DOS_FILEEXISTS: .asciiz "$:[f]"     
    DOS_CURDIR: .asciiz "cd:[c]"
    DOS_CURDIROTHER: .asciiz "cd:[p]"  

    DOS_DIR: .byte "$=l:*=d"   
    DOS_DIR_END: .byte $0

    DOS_FILES: .byte "$=l:*=p" 
    DOS_FILES_END: .byte $0
 
    TSRPRG: .asciiz "[s]/[f]"   ;path and filename of file to be launched 

    JOYSTICK_MSG: .asciiz "#  info#  edit#  copy#  rename#  move#  mkdir#  delete###  point and press a, b to exit  "
    
    ;basic sys command to the launcher in RAM or ROM
    .ifdef uselauncher
        LaunchSYS: .asciiz "sys $9718";$8001"
    .endif
    .ifndef uselauncher
        LaunchSYS: .asciiz "sys $";$8001

    .endif
    
    SCREEN_MODE_MSG:  .byte $0d,"screenmode is not supported. please choose one of these:",$0d,$0d,$0d
                        .byte " a. mode 0: 80x60",$0d
                        .byte " b. mode 1: 80x30",$0d
                        .byte " c. mode 2: 40x60",$0d
                        .byte " d. mode 3: 40x30",$0d
                        .byte " e. mode 4: 40x15",$0d
                        .byte " f. mode 8: 64x50",$0d
                        .byte " g. mode 9: 64x25",$0d
                        .byte " q. quit",$0d,$0d
                        .byte "(press a,b,c,d,e,f,g or q on the keyboard)"
                        
                        .byte $0
                    

   
.segment "CODE"

.include "constants.s"
.include "io.inc"
.include "banks.inc"


;set golden ram to 0
    lda #00
    sta ZP_PTR
    lda #$04
    sta ZP_PTR+1


    ldy #0
    NextYY:
    ldx #0
    :
        lda #0
        sta (ZP_PTR)
        jsr IncBankPointer
        inx
        cpx #0
        bne :-
        iny
        cpy #4
        bne NextYY

;Initialise variables
    inc COL_OFFSET  ;initialize to #1




;Rambanks used:
    RB_FILES_LIST_LEFT      = 01    
    RB_FILES_LIST_RIGHT     = 02
    RB_LONGFILENAMES_LEFT   = 03
    RB_LONGFILENAMES_RIGHT  = 04
    RB_PARTITIONS           = 05
    ;rambanks 63 and 62 are used for variable storage


SET_RECORD_LENGTH       = 40    ;Length of 1 file record
SET_MAX_FILES_COUNT     = 200   ;Max files per page

JOY_NUMBER              = 1     ;compile time setting: 0=keyboard, 1=joy1
JOY_PRESENT             = 1     ;1=check joystick input, 0 do not check
JOY_PRESS_DELAY         = 10    ;countdown delay befor next button press
DEFAULT_MODAL_WIDTH     = 40    ;default width of modal windows

x16EditFileBuffer       = $9dff ;Memory location to store the filename for x16edit to be loaded
LAUNCHFILE = $04FF              ;Memory location to store the file to be launched by the launcher


jmp start

.include "macros.s"

;Read the state of joystick buttons, and trigger press-events if needed
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
    
     
    lda #JOY_NUMBER  ;0=keyboard, 1=first joystick
    jsr joystick_get 
   
    ;TODO: TMP sta/lda can probably be removed. Test with real joystick
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


;Copy all variables from the selected window into the 'CURRENT' variables
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
            jsr SetRambank63        ;DIR_RIGHT is on rambank 63
            ldx #0
            :
                lda DIR_RIGHT,x
                sta DIR_CURRENT,x
                inx
                cpx #255
                beq :+
                jmp :-
            :
            jsr RestoreRambank      
            
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
            jsr SetRambank63     ;DIR_RIGHT is on rambank 63
            ldx #0
            :
                lda DIR_LEFT,x
                sta DIR_CURRENT,x
                inx
                cpx #255
                beq :+
                jmp :-
            :
            jsr RestoreRambank
        @SwitchDone:
    plx
    pla

rts

;Copy all vars from the current window to its designated vars
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

            jsr SetRambank63
            ldx #0  
            :
                lda DIR_CURRENT,x
                sta DIR_RIGHT,x
                inx
                cpx #255
                beq :+
                jmp :-
            :        
            jsr RestoreRambank
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

            jsr SetRambank63
            ldx #0
            :
                lda DIR_CURRENT,x
                sta DIR_LEFT,x
                inx
                cpx #255
                beq :+
                jmp :-
            :
            jsr RestoreRambank
        @SwitchDone:
    plx
    pla

rts

;Switch active window from left to right or right to left
SwitchWindow:
    jsr StoreWindowVars         ;Store vars of 'old' window
    
    
        lda iCurrentWindow
        cmp #0
        beq @ToRightWindow
            lda #0
            sta iCurrentWindow 
            lda #1
            sta COL_OFFSET      ;Set print offset to #1
        jmp @SwitchDone
        @ToRightWindow:     
            lda #1
            sta iCurrentWindow
            
            lda #2
            clc
            adc WINDOW_WIDTH
            sta COL_OFFSET      ;Set print offset to WINDOW_WIDTH+2
        @SwitchDone:
    jsr ReadWindowVars          ;Read vars of 'new' window
rts





;When printing text by writing to VRAM replace $0-bytes by spaces $20
ConvertPetsciiToVera:
    cmp #0
    beq MakeSpace
    rts
    
    MakeSpace:
    lda #$20
    rts
    
    
;Set charset to ISO and load appropriate font  
SetCharsetIso:

    clc
    lda #1
    jsr screen_set_charset 
    
    lda #$0F ;E
    jsr CHROUT
    
    
    ;alter some chars in VRAM to draw borders
    lda #$00
    sta $9F20
    lda #$F5
    sta $9F21
    lda #$11
    sta $9F22
    
    ;Vertical line
    lda #%00011000
    sta $9F23

    lda #%00011000
    sta $9F23

    lda #%00011000
    sta $9F23

    lda #%00011000
    sta $9F23

    lda #%00011000
    sta $9F23

    lda #%00011000
    sta $9F23
    
    lda #%00011000
    sta $9F23

    lda #%00011000
    sta $9F23

;Horizontal line
    lda #%00000000
    sta $9F23

    lda #%00000000
    sta $9F23

    lda #%00000000
    sta $9F23

    lda #%11111111
    sta $9F23

    lda #%11111111
    sta $9F23

    lda #%00000000
    sta $9F23
    
    lda #%00000000
    sta $9F23

    lda #%00000000
    sta $9F23
 
;Corner top left
    lda #%00000000
    sta $9F23

    lda #%00000000
    sta $9F23

    lda #%00000000
    sta $9F23

    lda #%00000111
    sta $9F23

    lda #%00001111
    sta $9F23

    lda #%00011100
    sta $9F23
    
    lda #%00011000
    sta $9F23

    lda #%00011000
    sta $9F23 

;Corner top right
    lda #%00000000
    sta $9F23

    lda #%00000000
    sta $9F23

    lda #%00000000
    sta $9F23

    lda #%11100000
    sta $9F23

    lda #%11110000
    sta $9F23

    lda #%00111000
    sta $9F23
    
    lda #%00011000
    sta $9F23

    lda #%00011000
    sta $9F23     

;T
    lda #%00000000
    sta $9F23

    lda #%00000000
    sta $9F23

    lda #%00000000
    sta $9F23

    lda #%11111111
    sta $9F23

    lda #%11111111
    sta $9F23

    lda #%00011000
    sta $9F23
    
    lda #%00011000
    sta $9F23

    lda #%00011000
    sta $9F23     

;A5 Vertical with right notch
    lda #%00011000
    sta $9F23

    lda #%00011000
    sta $9F23

    lda #%00011000
    sta $9F23

    lda #%00011111
    sta $9F23

    lda #%00011111
    sta $9F23

    lda #%00011000
    sta $9F23
    
    lda #%00011000
    sta $9F23

    lda #%00011000
    sta $9F23 

;A6 cross
    lda #%00011000
    sta $9F23

    lda #%00011000
    sta $9F23

    lda #%00011000
    sta $9F23

    lda #%11111111
    sta $9F23

    lda #%11111111
    sta $9F23

    lda #%00011000
    sta $9F23
    
    lda #%00011000
    sta $9F23

    lda #%00011000
    sta $9F23 

;A7 Vertical with left notch
    lda #%00011000
    sta $9F23

    lda #%00011000
    sta $9F23

    lda #%00011000
    sta $9F23

    lda #%11111000
    sta $9F23

    lda #%11111000
    sta $9F23

    lda #%00011000
    sta $9F23
    
    lda #%00011000
    sta $9F23

    lda #%00011000
    sta $9F23    

;A8 Corner bottom left
    lda #%00011000
    sta $9F23

    lda #%00011000
    sta $9F23

    lda #%00011100
    sta $9F23

    lda #%00001111
    sta $9F23

    lda #%00000111
    sta $9F23

    lda #%00000000
    sta $9F23
    
    lda #%00000000
    sta $9F23

    lda #%00000000
    sta $9F23  
 
;A9 Corner bottom left
    lda #%00011000
    sta $9F23

    lda #%00011000
    sta $9F23

    lda #%00111000
    sta $9F23

    lda #%11110000
    sta $9F23

    lda #%11100000
    sta $9F23

    lda #%00000000
    sta $9F23
    
    lda #%00000000
    sta $9F23

    lda #%00000000
    sta $9F23   

;AA T upside doen
    lda #%00011000
    sta $9F23

    lda #%00011000
    sta $9F23

    lda #%00011000
    sta $9F23

    lda #%11111111
    sta $9F23

    lda #%11111111
    sta $9F23

    lda #%00000000
    sta $9F23
    
    lda #%00000000
    sta $9F23

    lda #%00000000
    sta $9F23  
   
rts

;Redraw the entir screen
Repaintscreen:
    sec
    jsr SCREEN_MODE
    sty SCREEN_ROWS
    stx SCREEN_COLS

    jsr SetCharsetIso

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


;Check which device numbers are connected nu trying to list files on it and check for an error
CheckConnectedDevices:
    lda #8
    sta TMP
    ldy #0
    
    @lp:
        phy
            lda #1   ; Logical Number = 1
            ldx TMP
            ldy #15   ;   15=control channel
            jsr SETLFS    
            CopyAddrUntilZero DOS_FILES,CMD_BUFFER,CNT
            lda CNT ;#(DOS_FILES_END-DOS_FILES) ; filename length
            ldx #<CMD_BUFFER
            ldy #>CMD_BUFFER
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
            cmp #20
            bpl @Done
        
    jmp @lp 
    @Done: 
   
rts 

;Check to see if current filesystem type is hostfs or sd-card
GetFSType:
    lda #1          ; Logical Number = 1
    ldx $03fe ;#8   ;Device number ($03fe contains the last used device number)
    ldy #0   
    jsr SETLFS
    
    
    ;List with a filter of random chars 'qx' to make sure the length of the result is small
    lda #'$'
    sta CMD_BUFFER
    lda #'='
    sta CMD_BUFFER+1
    lda #'q'
    sta CMD_BUFFER+2
    lda #'x'
    sta CMD_BUFFER+3    
      
      
    lda #4 ;#(DOS_CHECKLIST_END-DOS_CHECKLIST) ; command
    ldx #<CMD_BUFFER
    ldy #>CMD_BUFFER   
 
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
        sta TMP
        ;H or F is in the accumulator
    @Done:
    jsr CLRCHN
    lda #1
    jsr CLOSE  
    
  
rts

Goto_DoneListPartitions: jmp DoneListPartitions

;Load list of partitions and devices into designated rambank
LoadPartitionsList:
   
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
            ;fstype is in TMP
            lda TMP
            
            
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
                ;first two bytes are device number + partition
                lda $03fe
                jsr StoreCurrentAInMemory
                iny
                lda #0
                jsr StoreCurrentAInMemory
                iny
                
                clc
                lda $03fe
                adc #$30
                jsr StoreCurrentAInMemory
                iny
                lda #COLON
                jsr StoreCurrentAInMemory
                iny
                
                ;This is ugly, refactor some day
                lda #'H'
                jsr StoreCurrentAInMemory
                iny

                lda #'O'
                jsr StoreCurrentAInMemory
                iny

                lda #'S'
                jsr StoreCurrentAInMemory
                iny

                lda #'T'
                jsr StoreCurrentAInMemory
                iny

                lda #'F'
                jsr StoreCurrentAInMemory
                iny

                lda #'S'
                jsr StoreCurrentAInMemory
                iny

                                
                lda #0
                jsr StoreCurrentAInMemory
                iny
                
                @lp3:
                    lda #$20
                    jsr StoreCurrentAInMemory
                    iny
                    cpy #SET_RECORD_LENGTH
                    bne @lp3
                    
                jsr DecBankPointer
                jsr DecBankPointer
                jsr DecBankPointer
                lda #$41
                jsr StoreCurrentAInMemory
                jsr IncBankPointer
                jsr IncBankPointer
                
            @Continue:
            
        ply
        iny
    jmp @lp
  
    DoneListPartitions:

rts

;Read partition table of device and store into devices/partitions rambank
ReadPartitionTable:
    phy
    phx
        lda $03fe

        lda #1  
        ldx $03fe 

        ldy #0  
        jsr SETLFS
        
        lda #'$'
        sta CMD_BUFFER
        lda #'='
        sta CMD_BUFFER+1
        lda #'p'
        sta CMD_BUFFER+2
        
    
        lda #3
        ldx #<CMD_BUFFER
        ldy #>CMD_BUFFER
    
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

            ldx #1  ;find next "
        jmp @lp
        
        @CheckStartName:
            cmp #$22
            bne @lp
            inc CNT_PARTITIONS
            
            ldy #0
            ;store device
            lda $03fe
            jsr StoreCurrentAInMemory
            iny
            ;store partition Number
            lda TMP
            jsr StoreCurrentAInMemory
            iny
            
            ;store visual device number
            clc
            lda $03fe
            adc #$30
            jsr StoreCurrentAInMemory
            iny
            lda #COLON
            jsr StoreCurrentAInMemory
            iny
            
            ;store visual partition number
            clc
            lda TMP
            
            adc #$30
            jsr StoreCurrentAInMemory
            iny 
            lda #COLON
            jsr StoreCurrentAInMemory
            iny

            
            ldx #2  ;store name
        jmp @lp
        
        
        @StoreName:
            cmp #$22    ;"
            beq @FinishLine
            jsr StoreCurrentAInMemory
            iny       
            
        jmp @lp
        
        @FinishLine:
            lda #0
            jsr StoreCurrentAInMemory
            iny           
            ;add spaces
            @lp2:
                lda #$20
                jsr StoreCurrentAInMemory
                iny
                cpy #SET_RECORD_LENGTH
                bne @lp2
                jsr DecBankPointer
                jsr DecBankPointer
                jsr DecBankPointer
                lda #$41
                jsr StoreCurrentAInMemory
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

;Main/Begin
start:

    jsr SetCharsetIso


    ;check screen mode
    sec
    jsr SCREEN_MODE
    sta TMP
    sty SCREEN_ROWS
    stx SCREEN_COLS


    cpx #39 ;if less then 40 columns, ask to select another screen mode
    bmi Goto_SelectSupportedScreenmode
    
    jsr ResetVera    
    clc
    lda TMP
    jsr SCREEN_MODE
    
    ;jsr SetCharsetIso
    
    lda $03fe   ;address of current device number
    pha
        jsr CheckConnectedDevices   ;list available devices
        jsr LoadPartitionsList      ;list partitionlist of sd-device(s)
    pla
    sta $03fe

    
    ;initialize vera ctrl to use DATA_0
    LDA #$00
    STA $9F25   ;VERA CTRL

    
    ;print window outline, header and footer    
    jsr PrintMainScreen2 

    
    ;intialize variables
    stz OFFSET_LEFT
    stz OFFSET_RIGHT
    stz iCurrentWindow
    stz PAGE_LEFT
    stz PAGE_RIGHT
    

    ;Somehow devicenumber #0 means devicenumber #8, so lets put #8 in it for future reference
    lda $03fe   ;address of current device number
    cmp #0
    bne @StoreDevice    ;if 0, it is suppoed to be 8
    lda #8
    sta $03fe
    
    @StoreDevice:
    ;set all device vars to #8
    sta DEVICE_CURRENT
    sta DEVICE_LEFT
    sta DEVICE_RIGHT
    sta DEVICE_START

    ;Get the current partitionnumber (if on sd-card) and store in partition-vars
    jsr GetCurrentPartition

    ;store current directory in CUR_PATH
    ;This directory is used for the RAM version to reload fm.prg
    jsr GetStartDirectory2  

    ;Save start up partition, to load FM.PRG in ram version      
    lda PARTITION_CURRENT
    sta PARTITION_START
    
    lda #0                  ;set current window to left window
    sta iCurrentWindow
        jsr SetRambank63
            lda #$2F   ;init dirs to root \     ;TODO: use current directory!!!!
            sta DIR_LEFT 
            lda #$0
            sta DIR_LEFT+1  ;end with $0           
        jsr RestoreRambank

        jsr ReadWindowVars
            jsr LoadCurrentDir2
            jsr ShowFiles2
        jsr StoreWindowVars

    jsr SwitchWindow    ;switch to right window
   
        jsr SetRambank63
            lda #$2F   ;init dirs to root \
            sta DIR_RIGHT
            lda #$0
            sta DIR_RIGHT+1     ;end with $0
        jsr RestoreRambank
        
        jsr ReadWindowVars

            ;check if both dirs are the same. If so just copy rambanks instead of rereading the file lists (quicker)
            jsr SetRambank63
        
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
            jsr RestoreRambank
            jsr LoadCurrentDir2
            DoShow:
            jsr RestoreRambank
            jsr ShowFiles2
        jsr StoreWindowVars
    jsr SwitchWindow 
   

    
    
    clc
    ;Highlight first row of active window
    jsr HighlightRow

    
    
    ;Don't know if these 4 lines are needed anymore. Check someday with real joystick
    lda #0
    jsr joystick_scan
    lda #JOY_NUMBER
    jsr joystick_get

         
    lp:     ;main program loop
       
       
        lda #JOY_PRESENT
        cmp #1
        bne SkipJoystickHandling        ;If no controler is present, dont bother check input
             ;Check (SNES) controller
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
            bne @SkipIfNotMode0Joy

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

                
        ;Check keyboard input      
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

        cmp #$8D    ;enter+shift
        beq Goto_EnterPress     
           
       jmp SkipJumpBit
            ;Jump table
            Goto_ShowJoyOptions:    jmp ShowJoyOptions
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

            cmp #$88    ;F7 - move file    ;not supported on SD-card, until ROM update!!!
            beq Goto_MoveCurrentFile

            cmp #$8C    ;F8 - Create directory
            beq Goto_CreateDirectory

            cmp #$19    ;DEL- delete file
            beq Goto_DeleteFile
        @SkipIfNotMode0:    ;--------------------------------
       
        pha
            jsr kbdbuf_get_modifiers   ;get keyboard modifiers
            tax ;x contains modifieres
        pla
       
        cpx #04
        bne @nocontrol  ;is control-key down?
            
            cmp #$2B    ;+  
            beq Goto_PlusPress

            cmp #$2D    ;-
            beq Goto_MinusPress
        @nocontrol:
        
           
        
    jmp lp       

Goto_ChangeDevice:  jmp ChangeDevice
Goto_LPX:           jmp lp
Goto_PlusPress:     jmp PlusPress
Goto_MinusPress:    jmp MinusPress      
   

;New partition and/or device selected, so change vars accordingly
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
    
    jsr SetRambank63
    lda #$2F
    sta DIR_CURRENT
    lda #0
    sta DIR_CURRENT+1
    jsr RestoreRambank
    jsr LoadCurrentDir2
    jsr ShowFiles2
    jsr HighlightRow    
jmp lp

GotoLP3: jmp lp


;Load highlighted file in x16edit ROM version
EditFile:
    jsr SetPointerToCurrentOffset   ;memory pointer to current offset  
    jsr AdvancePointerToCurrentRow
   
    ldy #37
    lda (ZP_PTR),y
    cmp #$44    ;directory    
    beq GotoLP3
    

    jsr GotoCurrentDirectory        ;Set correct device, partition and directory for x16edit to load from

    lda #'['
    sta CMD_BUFFER
    lda #'f'
    sta CMD_BUFFER+1
    lda #']'
    sta CMD_BUFFER+2
    lda #0
    sta CMD_BUFFER+3
    
    lda #<CMD_BUFFER
    sta ZP_TEXT_PRT
    lda #>CMD_BUFFER
    sta ZP_TEXT_PRT+1
    
    jsr ReplaceParams   ;replace message params with values      

   
    CountUntilZero CMD_BUFFER,TMP
    

    ldx #0
    :
        lda CMD_BUFFER,x
        sta x16EditFileBuffer,X
        inx
        cmp #0
        bne :-

    ;Set iso mode (probably absolete, because we are already in iso mode)
    lda #$0F
    jsr CHROUT
    

        ldx #$15 ; First RAM bank used by the editor
        ldy #60 ; And last RAM bank
         
        lda #<x16EditFileBuffer ; Pointer to file name (LSB)
        sta $02 ; Store in r0L
        lda #>x16EditFileBuffer ; Pointer to file name (MSB)
        sta $03 ; Store in r0H
        
        lda TMP ; File name length
        sta $04 ; Store in r1L
 
        lda DEVICE_CURRENT
        sta $08        
        
        jsr x16Edit
    done:

    ;When return from x16Edit, repaint screen
    
    jsr Repaintscreen
       
jmp lp


;If joystick button B is pressed, user gets a selection list of actions
ShowJoyOptions:
    jsr SetRambank63
    CopyAddrUntilZero JOYSTICK_MSG,MSG_MODAL,TMP
    jsr RestoreRambank
    
    stz JOYSTICK_CUR_LINE
    
    lda #<MSG_MODAL
    sta ZP_TEXT_PRT
    lda #>MSG_MODAL
    sta ZP_TEXT_PRT+1
 
    ldy #DEFAULT_MODAL_WIDTH     ;width
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

;Moves highlighted row one up on action selection
JoyOptionsPrevLine:
    lda JOYSTICK_CUR_LINE
    cmp #0
    beq lpjoy
    jsr JoyOptionsHideSelect
    dec JOYSTICK_CUR_LINE
    jsr JoyOptionsShowSelect
jmp lpjoy

;Moves highlighted row one down on action selection
JoyOptionsNextLine:
    lda JOYSTICK_CUR_LINE
    cmp #6  ;7 options to choose from
    beq lpjoy
    jsr JoyOptionsHideSelect
    inc JOYSTICK_CUR_LINE
    jsr JoyOptionsShowSelect
jmp lpjoy

;Action selected
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

;Highlight current action row
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

;Remove highlight current action row
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

;Backspace results in change dir to parent directory
BackspacePress:


    jsr SetRambank63
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
    
    jsr RestoreRambank
    jsr LoadCurrentDir2
    jsr ShowFiles2
    jsr HighlightRow
    
    jmp lp

@GotoLp:                jmp lp
Goto_ChangeDirDown:     jmp ChangeDirDown
Goto_ChangePartition:   jmp ChangePartition
Goto_NextPage:          jmp NextPage
Goto_PrevPage:          jmp PrevPage


;Enter press, so execute if it is a program or change to selected directory
EnterPress:

    jsr GETIN   ;clear keyboard buffer
    
 
    ;;-------------------------------------    
    jsr SetPointerToCurrentOffset   ;memory pointer to current offset  
    jsr AdvancePointerToCurrentRow
 

   
    ldy #37 ;line type 
    lda (ZP_PTR),y

    cmp #$44    ;directory
    beq Goto_ChangeDirDown
    cmp #$41    ;Partitions change
    beq Goto_ChangePartition
    cmp #$3e    ;Goto next page
    beq Goto_NextPage
    cmp #$3c    ;Goto prev page
    beq Goto_PrevPage

    ;or launch file
    CopyAddrUntilZero TSRPRG,CMD_BUFFER,CMD_BUFFER_LENGTH
    lda #<CMD_BUFFER
    sta ZP_TEXT_PRT
    lda #>CMD_BUFFER
    sta ZP_TEXT_PRT+1
    
    jsr ReplaceParams   ;replace message params with values      
        
    ;Check header of file to see if it is launchable, no matter the extention
    jsr IsLaunchable
   
    lda TMP
    cmp #0
    beq GotoLpTmp  ;not launchable
    jmp AnotherJumpTable
    GotoLpTmp:    jmp lp
    
    AnotherJumpTable:
    
    ;execute program

 clc
    jsr kbdbuf_get_modifiers 
    cmp #01
    bne ContinueNormalLaunch
    jmp LoadAndLaunch
    
    ContinueNormalLaunch:
    ;store start directory in last 256 bytes of rambank #0
    jsr StoreStartDirForLauncher
   
  
    ;make sure launch is from the programs directory. (including devicenumber and partition)
    jsr GotoCurrentDirectory

    ;switch to petscii
    lda #2
    jsr screen_set_charset 

    lda #$8F
    jsr CHROUT
    
    lda #$8E
    jsr CHROUT
   
    .ifdef uselauncher
        ;Copy launch program to last bit of basic memory
        jsr CopyLaunchToMem
    .endif


    jsr clearscreen
    lda #0
    ldx #0
    ldy #1

    CopyAddrUntilZero TSRPRG,CMD_BUFFER
    
 
    lda #<CMD_BUFFER
    sta ZP_TEXT_PRT
    lda #>CMD_BUFFER
    sta ZP_TEXT_PRT+1
    
    jsr ReplaceParams   ;replace message params with values      


    ;Check if is tokenizes Basic
    jsr CheckFileType

    lda TSRTYPE ;B(asic) or A(assembly)
    sta LAUNCHFILE 

    ;cmd buffer containsfilename to launch. We can store thisin $0400 ram, its just temporary
    ldy #0
    @thisloop:
        lda CMD_BUFFER,y
        cmp #0
        beq DoLaunch
        sta LAUNCHFILE+1,y 
        sta DEBUG
        iny
    jmp @thisloop  
    
    DoLaunch:
   
    ;store last zero
    sta LAUNCHFILE+1,y 
    
    iny
    lda DEVICE_START
    sta LAUNCHFILE+1,y ; ;store deviceno of FM.PRG location
    iny
    lda PARTITION_START
    sta LAUNCHFILE+1,y ; ;store partition of FM.PRG location
    
    
    .ifdef uselauncher
    
        ;Run the launcher by execute basic SYS-command via keyboard buffer
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
    .endif

    .ifndef uselauncher
        ;Run the launcher by execute basic SYS-command via keyboard buffer
        ldy #0 
        @nxt:
            lda LaunchSYS,y
            cmp #0
            beq @dn
            jsr KEY_POKE
            iny
        jmp @nxt
        @dn:
        
        lda #>LaunchMain
        jsr ConvertAToAsciiHex
        lda LaunchAddressAscii
        jsr KEY_POKE 
        lda LaunchAddressAscii+1
        jsr KEY_POKE 
        
        lda #<LaunchMain
        jsr ConvertAToAsciiHex
        lda LaunchAddressAscii
        jsr KEY_POKE 
        lda LaunchAddressAscii+1
        jsr KEY_POKE 

        lda #$0d
        jsr KEY_POKE        
    .endif
    
    rts
   

GotoLP: jmp lp


;Goto selected subdirectory
ChangeDirDown:
    ldy #2
    lda (ZP_PTR),y  ;first byte of filename
    cmp #$2E    ;period .
    bne @DoChangeDirDown
    ;change dir up
    jmp BackspacePress

@DoChangeDirDown:
    jsr LowlightRow
    jsr SetRambank63
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
        jsr RestoreRambank
        
        jsr LoadCurrentFileInBuffer
        ldy #0
        @AddNext:   ;add new dir at the end
            
            jsr SetRambank63
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
    
    jsr RestoreRambank
    
    jsr LoadCurrentDir2
    jsr ShowFiles2
    jsr HighlightRow
jmp lp

;Change to selected partition
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
    bcs MoveToLastItemOnPage
    
    jsr LowlightRow
    lda FILECOUNT_CURRENT   ;cnt files total
    sbc ListSize            ;minus listsize

    sta OFFSET_CURRENT
    lda ListSize
    sbc #1
    sta ROW_CURRENT
    jsr ShowFiles2
    jsr HighlightRow   
    
    
jmp lp

MoveToLastItemOnPage:
    jsr LowlightRow
    lda FILECOUNT_CURRENT
    sta ROW_CURRENT
    dec ROW_CURRENT    
    jsr HighlightRow
jmp lp

Goto_Lpxx:    jmp lp

;CTRL +  -> next supported screenmode
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

;CTRL -  -> previous supported screenmode
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


GotoSwitchWindow:
    jsr LowlightRow
    jsr SwitchWindow
    jsr HighlightRow
jmp lp

PageUp:
    jsr LowlightRow
    lda ROW_CURRENT
    cmp #0  ;top
    beq GotoPrevPage

    stz ROW_CURRENT
    jsr HighlightRow
    @DoNothing:
jmp lp

GotoPrevPage:
    lda OFFSET_CURRENT
    cmp #0  ;already at the top
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
    bcs GotoEndOfList
      
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


;Replace params in string. For example: [f] will be replaced with the current selected filename
;TODO: this subroutine can be highly optimized into smaller code. A lot of repetition
ReplaceParams:
    jsr LoadFileInfoCurrentFile
    
    jsr SetRambank63
    
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
            sta TMPSTRING2,x
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
                jsr SetRambank63
                ldy #0
                @NextChar:
                    ;lda (ZP_PTR)
                    lda FileInfo_Name,y
                    cmp #0
                    beq @AddFileDone
                    sta TMPSTRING2,x
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
                    sta TMPSTRING2,x
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
                    sta TMPSTRING2,x
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
                    sta TMPSTRING2,x
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
                    sta TMPSTRING2,x
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
                        jsr SetRambank63
                        ldy #0
                        @nextpathcharcx:
                            lda DIR_CURRENT,y
                            cmp #0
                            beq @addpathreadycx
                            sta TMPSTRING2,x
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
                        jsr SetRambank63
                        ldy #0
                        @nextpathcharcx2:
                            lda DIR_CURRENT,y
                            cmp #0
                            beq @addpathreadycx2
                            sta TMPSTRING2,x
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
                    sta TMPSTRING2,x
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
                    sta TMPSTRING2,x
                    inx

            ply

            ;insert other path
            iny ;closing ]
            iny ;next char           
        jmp @next

        
        @InsertLastInput:
            phy
                    jsr SetRambank63
                    ldy #0
                    @nextpathchard:
                        lda InputBuffer,y
                        cmp #0
                        beq @addpathreadyc
                        sta TMPSTRING2,x
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
                    jsr SetRambank63
                    ldy #0
                    @nextpathcharc:
                        lda DIR_CURRENT,y
                        cmp #0
                        beq @addpathreadyc
                        sta TMPSTRING2,x
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
                    jsr SetRambank63
                    ldy #0
                    @nextpathchard2:
                        lda DIR_CURRENT,y
                        cmp #0
                        beq @addpathreadyd2
                        sta TMPSTRING2,x
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
        sta TMPSTRING2,x ;end with $0
    
    
    
   
    
        ;TEMPSTRING back into  MSG_MODAL
        ldy #0
        @nextcopy:
            lda TMPSTRING2,y
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
    jsr RestoreRambank
    

rts


;Get filesize and convert block size to bytes size
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

;Change screenmode after user selects a supported one
SelectSupportedScreenmode:
  PrintUntilZero SCREEN_MODE_MSG
  @lp:
        jsr GETIN
        cmp #0
        beq @lp
        
      ;  jsr print_hex_debug
        cmp #$41    ;a
        beq @ToMode0
        cmp #$61    ;a
        beq @ToMode0
        cmp #$30    ;a
        beq @ToMode0

        cmp #$42    ;b
        beq @ToMode1
        cmp #$62    ;b
        beq @ToMode1
        cmp #$31    ;b
        beq @ToMode1


        cmp #$43    ;c
        beq @ToMode2
        cmp #$63    ;c
        beq @ToMode2
        cmp #$32    ;c
        beq @ToMode2

        cmp #$44    ;d
        beq @ToMode3
        cmp #$64    ;d
        beq @ToMode3
        cmp #$33    ;d
        beq @ToMode3


        cmp #$45    ;e
        beq @ToMode4
        cmp #$65    ;e
        beq @ToMode4
        cmp #$34    ;e
        beq @ToMode4

        cmp #$46    ;f
        beq @ToMode8
        cmp #$66    ;f
        beq @ToMode8
        cmp #$38    ;f
        beq @ToMode8

        cmp #$47    ;g
        beq @ToMode9
        cmp #$67    ;g
        beq @ToMode9
        cmp #$39    ;f
        beq @ToMode9
        
        cmp #$71    ;q
        beq @ToQuit
        

    
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

    @ToQuit:
       jmp PerformQuit




;Load info to display in fileinfo modal after pressing F2
LoadFileInfoCurrentFile:
jmp LoadFileInfoCurrentFile_start
  
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

;Get current filename, read from long filename rambank if needed
LoadCurrentFileInBuffer:
    lda $00
    pha
    
    
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
        sta TMP ;store in TMP also to check if there is a LFN pointer
        sta ZP_PTR2

    
        iny
        
        lda (ZP_PTR),y  ;get second byte of poinrt to long filename
        sta ZP_PTR2+1

        
        ora TMP   ;OR both bytes together
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
                sta TMPSTRING,y
                sta FileInfo_Name,y
                jsr IncBankPointer
                iny
                cpy #12 ;file is 12 chars long
                beq @AddFileDone
            jmp @NextChar
            @AddFileDone:   
            lda #0
            sta TMPSTRING,y
            sta FileInfo_Name,y
            jmp @SRDone
        
        @LoadLongFileName:
       
        jsr SetRambankLongFileNames
        
        ldy #0
        @Next:
            lda (ZP_PTR2),y
            cmp #0
            beq @Done
            sta TMPSTRING,y
            sta FileInfo_Name,y
            iny
            jmp @Next
        @Done:
      
        sta TMPSTRING,y
        sta FileInfo_Name,y
    @SRDone:
    pla
    ply
    plx
    
    pla
    sta $00

   
rts

Gogo_LpXXXXXX: jmp lp

;Show fileinfo modal
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

   
    jsr SetRambank63
    CopyAddrUntilZero FileInfo_Msg,MSG_MODAL,TMP
    jsr RestoreRambank
    
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

    Msg_Rename: .asciiz "#Enter new name:#######<ENTER> to proceed   <ESC> to cancel"    
    CMD_RENAME: .asciiz "r:[i]=[f]"
    
RenameFile_start:
    ldx FILECOUNT_CURRENT
    cpx #0
    beq Goto_LPXXC
    
    jsr LoadCurrentFileInBuffer
    lda FileInfo_Name
    cmp #$2e    ;.
    beq Goto_LPXXC ;don't delete .. (updir)

    jsr SetRambank63
    CopyAddrUntilZero Msg_Rename,MSG_MODAL
    jsr RestoreRambank
    
    ldx #28
    ldy #30
    clc
    jsr PLOT
    

    
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

    jsr GotoCurrentDirectory
    
    CopyAddrUntilZero CMD_RENAME,CMD_BUFFER

   
 
     lda #<CMD_BUFFER
    sta ZP_TEXT_PRT
    lda #>CMD_BUFFER
    sta ZP_TEXT_PRT+1
    
    jsr ReplaceParams   ;replace message params with values   
    
    
    stz CMD_BUFFER_LENGTH
    jsr DoDosCMD
    jsr ReloadWindows
    
@GotoLp:jmp lp


;Ask the user to enter the name of the new directory, and create it
CreateDirectory:
    jmp CreateDirectory_start
    
    Msg_CreateDirectory: .asciiz "#Enter name for new directory:#######<ENTER> to proceed   <ESC> to cancel"    
    CMD_MD: .asciiz "md:"
    
CreateDirectory_start:
    jsr SetRambank63
    CopyAddrUntilZero Msg_CreateDirectory,MSG_MODAL
    jsr RestoreRambank
    
    ldx #28
    ldy #30
    clc
    jsr PLOT
    

    
    jsr ShowGetInput    ;Show and handle getinput dialog
     
    lda #2
    sta TMP ;#2
    jsr LoopThroughModelWindow  ;Restore screen   

    lda InputLength
    cmp #0
    beq @GotoLp ;zero length
    
    jsr GotoCurrentDirectory
    CopyAddrUntilZero CMD_MD,CMD_BUFFER
    jsr SetRambank63
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
    jsr RestoreRambank
    
    
    jsr DoDosCMD

    
    jsr ReloadWindows
    
@GotoLp: jmp lp


;Show keyboard input dialog
ShowGetInput:
    ldy #DEFAULT_MODAL_WIDTH     ;width
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

;Delete selected file
DeleteFile:
    jmp DeleteFile_start
    Msg_DeleteFile: .asciiz "##Are you sure you want to delete:###[f]####<ENTER> to proceed   <ESC> to cancel"

    Template_Delete_File: .asciiz "s:[f]"
    Template_Delete_Dir: .asciiz "rd:[f]"

DeleteFile_start:
    ;check if dir
 
    jsr LoadCurrentFileInBuffer
    lda FileInfo_Name
    cmp #$2e    ;.
    beq Goto_LPXX ;don't delete .. (updir)
     
   
    jsr SetRambank63
    CopyAddrUntilZero Msg_DeleteFile,MSG_MODAL
    jsr RestoreRambank
  
    
    
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

;Reload fileslist of current window
ReloadCurrentWindow:
    stz OFFSET_CURRENT
    stz ROW_CURRENT
    
    jsr LowlightRow
    jsr LoadCurrentDir2
    jsr ShowFiles2

rts

;After user selects a new partition/device,it is saved and the windowis reloaded
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
    jsr SetRambank63
    lda #$2F
    sta DIR_CURRENT
    lda #0
    sta DIR_CURRENT+1    
    jsr RestoreRambank
    jsr LowlightRow
    jsr ReloadCurrentWindow
    jsr HighlightRow
    
    
jmp lp

;Reload both windows
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
    jsr LoadCurrentDir2
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
    
    jsr LoadCurrentDir2
    jsr ShowFiles2
    jsr SwitchWindow
    jsr HighlightRow   
rts

;Show file copy dialog
CopyCurrentFile:
jmp CopyCurrentFile_start
    
    Msg_CopyFile: .asciiz "##You are about to copy##File:   [f] ##To dir: [p]####<ENTER> to proceed   <ESC> to cancel"  ;# is linefeed 
    Msg_Copying: .asciiz "####Copying, please wait..."
    Msg_FileExists: .asciiz "        File already exists!        #        proceed to overwrite        "
    TemplateCopySource: .asciiz "[s]/[f]"   ;c=current directory
    
    TemplateCopyDest: .asciiz "@:[r]/[f],s,w"   ;p=directory of other window

GotoLP2: jmp lp
    
CopyCurrentFile_start:
    ldx FILECOUNT_CURRENT
    cpx #0
    beq GotoLP2

  
    jsr SetPointerToCurrentOffset   ;memory pointer to current offset  
    jsr AdvancePointerToCurrentRow
   
    ;check if is not dir

    ldy #37
    lda (ZP_PTR),y
    cmp #$44    ;directory, do nothing. Directory copying is not yet supported 
    beq GotoLP2
    
    jsr SetRambank63
    CopyAddrUntilZero Msg_CopyFile,MSG_MODAL
    jsr RestoreRambank
    
    lda #<MSG_MODAL
    sta ZP_TEXT_PRT
    lda #>MSG_MODAL
    sta ZP_TEXT_PRT+1
    
   jsr ReplaceParams   ;replace message params with values
 
   ;Check if file exists to add warning
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
  
    jsr SetRambank63
        CountUntilZero MSG_MODAL,CNT
        
        ldy CNT
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
    jsr RestoreRambank
 
    

    jsr ShowConfirm
    jsr ResetColor
    
    lda RESULT_KEY
    cmp #$0D     ;ENTER pressed
    beq PrepareCopyParams
    
@GotoLp:
jmp lp

;Prepare params befor copy
PrepareCopyParams:
    jsr SetRambank63
    CopyAddrUntilZero Msg_Copying,MSG_MODAL
    jsr RestoreRambank
    
    ldy #DEFAULT_MODAL_WIDTH     ;width
    ldx #15     ;height
    jsr ShowModal
    
    jsr ShowModalMsg
         
    lda #<MSG_MODAL
    sta ZP_TEXT_PRT
    lda #>MSG_MODAL
    sta ZP_TEXT_PRT+1

    jsr SetRambank63
    CopyAddrUntilZero TemplateCopySource,SourceFile
    CopyAddrUntilZero TemplateCopyDest,DestFile
    jsr RestoreRambank
    
   
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
 

 
  
    jsr CopyAFile

    ldy #DEFAULT_MODAL_WIDTH     ;width
    ldx #15     ;height

    jsr SetRambank62
    lda #<MODAL_SCREEN_BACKUP
    sta ZP_PTR
    lda #>MODAL_SCREEN_BACKUP
    sta ZP_PTR+1

    lda #2
    sta TMP ;#2
    jsr LoopThroughModelWindow  ;Restore screen

    jsr ReloadWindows
     
jmp lp

;User want to quit UFM
Quit:
    jmp Quit_Start
    
    Msg_Quit: .asciiz "##Are you sure you want to quit##and return to basic?####<ENTER> to proceed   <ESC> to cancel"
    
    Quit_Start:
    jsr SetRambank63
    CopyAddrUntilZero Msg_Quit,MSG_MODAL
    jsr RestoreRambank
    
    jsr ShowConfirm
    
    lda RESULT_KEY
    cmp #$0D     ;ENTER pressed
    bne GotoLpXX

PerformQuit:    
    jsr clearscreen

    ;reset charsetand stuf
    lda #2
    jsr screen_set_charset 

    lda #$8F
    jsr CHROUT
    
    lda #$8E
    jsr CHROUT
    
    
    rts;return to basic
    
;one to many!!
GotoLpXX: jmp lp
Goto_LPXXCXX: jmp lp

;Handle dialog and execution of moving a file
MoveCurrentFile:
    jmp MoveCurrentFile_start
    
    Msg_MoveFile: .asciiz "##You are about to move##File:   [f] ##To dir: [p]####<ENTER> to proceed   <ESC> to cancel"  ;# is linefeed 
    
 
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
    
    jsr SetRambank63
    CopyAddrUntilZero Msg_MoveFile,MSG_MODAL
    jsr RestoreRambank
    
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
    
@GotoLp: jmp lp


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

;Show message in a already prepared dialog
ShowModalMsg:
    jsr SetRambank63
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
    jsr RestoreRambank
rts

;Show confirmation dialog
ShowConfirm:
    ;max width=50
    ;max height=20
    ldy #DEFAULT_MODAL_WIDTH     ;width
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

;Show modal dialog
ShowModal:
    ;Row
    sty MODAL_WIDTH
    stx MODAL_HEIGHT
    

    
    lda SCREEN_ROWS
    clc
    sbc MODAL_HEIGHT
    lsr ;devide by 2
    sta MODAL_ROW

   
    
    ;Column
    
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
    
    jsr SetRambank62
    
    ;Copy bytes where the modal is going to be printed into rambank, to restore when modal is no longer needed
    lda #<MODAL_SCREEN_BACKUP
    sta ZP_PTR
    lda #>MODAL_SCREEN_BACKUP
    sta ZP_PTR+1
    jsr LoopThroughModelWindow 
    
    inc TMP
    jsr LoopThroughModelWindow  ;ShowModal


    ;reset pointer
    lda #<MODAL_SCREEN_BACKUP
    sta ZP_PTR
    lda #>MODAL_SCREEN_BACKUP
    sta ZP_PTR+1


rts

;Multipurpose subroutine to handle model showing/hiding logic
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

;if at the and of the screenlist, increment offset
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

;If after pressing arrow down there are no more files, stop at the last line
EndOfFilesList:
    dec ROW_CURRENT
    jsr HighlightRow   

jmp lp


;handle arrow up press
RowUp:

    lda ROW_CURRENT
    cmp #$0

    beq TopOfScreenlist  ;kan niet verder omhoog
    
    jsr LowlightRow
    dec ROW_CURRENT
    jsr HighlightRow 

jmp lp

;Handle if highlighted row is already at the top of the screen
TopOfScreenlist:
    lda OFFSET_CURRENT
    cmp #$0
    beq Goto_lp  ;bovenaan de lijst, dus niets doen
    
    jsr LowlightRow
    dec OFFSET_CURRENT
    jsr ShowFiles2
    jsr HighlightRow 

 jmp lp

Goto_lp: jmp lp
    
;Change color byte in vera ram of current selected row to highlight it
HighlightRow: 
    

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

;Change color byte in vera ram of current selected row to de-highlight it
LowlightRow:  
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

;Set vera addres to textmode and to the current selected row to change color
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


;Increment a 2 byte pointer which points to rambank
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



;Decrement a 2 byte pointer which points to rambank
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


;Store accumulator in rambank
StoreCurrentAInMemory:
    sta (ZP_PTR)
    jsr IncBankPointer 
rts

;TODO: same as IncBankPointer2, absolete!
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

;Set rambank to files list of left or right window
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

;Set rambank to rambank for long filenames of left or right window
SetRambankLongFileNames:
    pha
        clc
        lda #RB_LONGFILENAMES_LEFT 
        adc iCurrentWindow
        sta ZP_RAMBANK
    pla
    

rts

;Set rambank pointer to the record of the top most file in the current window
SetPointerToCurrentOffset:
    lda OFFSET_CURRENT      ;offset
    ; XXX Are we sure carry is always what we want here?
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
    lda #SET_RECORD_LENGTH ; #$20   ;32 = 32 bytes per file
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

;Advance pointer of rambank to record of the current highlighted file
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
    
;Show files list of left ot right window
ShowFiles2:

    clc
    jsr SetRamBank
    
    ldy COL_OFFSET
    ldx #4
    clc
    jsr PLOT

    jsr PrintCurrentFolder
    jsr PrintCurrentDevice
    jsr SetRamBank
    
    clc
    ldx #7
    ldy COL_OFFSET
    jsr PLOTVera        ;writing filelist is done by writing to VRAM directly. Much faster then CHROUT
    
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
        ;print empty lines to fill the window
        jsr FilesReadyPrintBlanks
        
        Continue:
        
rts

Goto_Nextline2: jmp Nextline2

;Print the current device and partition
PrintCurrentDevice:
    ldx #3
    
  
    
    ldy COL_OFFSET

    clc
    jsr PLOT
    
    lda #$5b
    jsr CHROUTbin

   
    
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
                cpy #SET_RECORD_LENGTH
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
    jsr CHROUTbin
    
    ;Whipe out any longer partition names
    lda #$A1
    jsr CHROUT
    jsr CHROUT
    jsr CHROUT
    jsr CHROUT
    jsr CHROUT
    jsr CHROUT
  
    
rts

;Print current folder and shrink it if it is too long
PrintCurrentFolder:
    
        ldy COL_OFFSET
        ldx #4
        clc
        jsr PLOT
        jsr SetRambank63
        
        CountUntilZero DIR_CURRENT,CNT
        lda CNT
        sta TMP2    ;store to use later
        cmp WINDOW_WIDTH
        bmi @PrintNormal
        phy
        phx
            ;doesn't fit, so print portion
            lda WINDOW_WIDTH 
            sec
            sbc #4      ;using 4 dots to seperate
            clc
            lsr
            sta CNT ;CNT contains with of left and right portion
             
            ;print left portion
            ldx #0
            :
                lda DIR_CURRENT,x
                jsr CHROUTbin
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
                    jsr CHROUTbin       
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
                jsr CHROUTbin
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
        jsr RestoreRambank
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

        
        inx
        inc ListCounter
        lda ListCounter
        cmp ListSize
        beq FilesReady      
    jmp @NextLine
rts


;Get the current (startup) partition
GetCurrentPartition:
jmp GetCurrentPartition_start
    CMD_PARTITION_INFO: .byte "g-p",0
    
GetCurrentPartition_start:

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

;Change to director of current window
GotoCurrentDirectory:

    ;goto current device
    lda DEVICE_CURRENT
    sta $03fe
        
    lda PARTITION_CURRENT

    cmp #0
    beq @NoPartitionToSelect
        jsr SelectCurrentPartition
    
    @NoPartitionToSelect:


    
    ;First goto root
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

    
    jsr SetRambank63
    lda #10
    sta DEBUG
    @NextChar:

        lda DIR_CURRENT,x

      
        cmp #$0
        beq @DoneXX   ;na $0 ook klaar... 

        sta CMD_BUFFER+3,x
        inx
        inc CMD_BUFFER_LENGTH

    jmp @NextChar
        
@DoneXX:

    jsr RestoreRambank
      
    
      
    jsr DoDosCMD
rts

;Execure dos command in CMD_BUFFER
DoDosCMD:

    
    lda DOS_CMD_DEVICE  ;0=Current, 1=Other --> depending on the action the dos command needs to be performed on the left or right window
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


    
    ldx #0
    :
       lda CMD_BUFFER,x
       inx
       cpx CMD_BUFFER_LENGTH
       bne :-

    

    
   lda #1   ; Logical Number = 1
   ldx DOS_CMD_DEVICE ;DEVICE_CURRENT ;#8   
   ldy #15   ;   15=control channel
      
   jsr SETLFS



   lda CMD_BUFFER_LENGTH ; filename length
   ldx #<CMD_BUFFER
   ldy #>CMD_BUFFER
   jsr SETNAM

   
   jsr OPEN

    
     LDX #1
     jsr CHKIN  

     ldy #$00      
@RD:
    lda #1
    jsr READST
    bne @Continue2
    JSR CHRIN
    STA CMD_BUFFER_RESULT,Y    

                  
     INY

     jmp @RD
  @Continue2:

       
    lda #0
    STA CMD_BUFFER_RESULT,Y
   
   jsr CLRCHN
   lda #1
   jsr CLOSE

   lda #1
   jsr CLOSE 
   jsr CLSALL
   stz DOS_CMD_DEVICE
   rts    

rts

;Change to the partition of the current window
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

;Change to partition of the other window
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

;Read files list of current window
LoadCurrentDir2:
jmp LoadCurrentDir2_start
    
    MSG_LOAD_MORE: .byte  "-next page-",0
    MSG_LOAD_LESS: .byte  0,0,"-prev page-                                                     ",0 ;"
    UPDIR: .byte 0,0,"..                                 d",0,0   ;"    
LoadCurrentDir2_start:
    
   
    
    lda PAGE_CURRENT
    sta PAGE_CURRENT_USE    ;pages to skip
    
   
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
            cpy #SET_RECORD_LENGTH
            bne :-     
            inc TMP2   

    
    @Goto_NoSubdir:
    
    lda PAGE_CURRENT
    cmp #0
    beq @Goto_NoPageUp
         ldy #0
        :
            lda MSG_LOAD_LESS,y
            jsr StoreCurrentAInMemory
            iny
            cpy #SET_RECORD_LENGTH
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
    

    
    
    stz DOS_DIR_TYPE    ;0=directories
    jsr LoadFilesList2
 
    lda PAGE_CURRENT_USE  ;absolete???
    
    inc DOS_DIR_TYPE    ;1=files
    jsr LoadFilesList2
       
    ;if there is a updir add to count
    clc
    lda FILECOUNT_CURRENT
    adc TMP2
    sta FILECOUNT_CURRENT
    

    
rts


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

;Load files or directory list for current window
LoadFilesList2:

    jsr SetRamBank
 
    sec
    lda #SET_MAX_FILES_COUNT
    sbc FILECOUNT_CURRENT
    bcc DoRTS


    lda #1   
    ldx DEVICE_CURRENT ;#8  
    ldy #0  
    jsr SETLFS

    
    CopyAddrUntilZero DOS_DIR,CMD_BUFFER,CNT
    
    lda DOS_DIR_TYPE
    cmp #1
    beq @DoFiles
    ;directories
    lda CNT 
    ldx #<CMD_BUFFER
    ldy #>CMD_BUFFER  
    jmp @Continue
   
    @DoFiles:
        ;overwrite if we need files
        CopyAddrUntilZero DOS_FILES,CMD_BUFFER,CNT
        
        lda CNT 
        ldx #<CMD_BUFFER
        ldy #>CMD_BUFFER 
    @Continue:
    
    jsr SETNAM
    jsr OPEN
    
    
    LDX #1
    jsr CHKIN
    
    ;Read 8 bytes to skip over 00 bytes which are not line ends
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
        cmp #SET_MAX_FILES_COUNT
        bne @Continuex
            dec PAGE_CURRENT_USE    ;pages to skip
            stz PAGE_FILE_COUNT 

        @Continuex:
        
        jsr READST     
        bne Goto_EndOfFile      
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
            jsr CHRIN   ;skip past potentional zero bytes in line number and file size
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
        jsr StoreCurrentAInMemory
        sta TMP_FILESIZE
     
        jsr CHRIN
        jsr StoreCurrentAInMemory
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
        sta TMPSTRING,y
        iny
        
        cpy #13 ;if filename is longer then 12 bytes
        bne @Continue2
            ;store asterix as last byte of filename to mark long filename    
            jsr DecBankPointer
            
            lda #$2A
            jsr StoreCurrentAInMemory
           
        jmp @Continue
        
        @Continue2:
        ;check if length > 12 then do nothing
        cpy #13
        bpl @Continue
        jsr StoreCurrentAInMemory 
        
        @Continue:
    jmp NextByte
    
    EndOfFilename:
        cpy #12
        bpl @NoSpaces
        lda #$0
        jsr StoreCurrentAInMemory
        iny
        cpy #12
        bpl @NoSpaces
        
        ;add spaces
        lda #$20
        @lpp:
            jsr StoreCurrentAInMemory
            iny
            cpy #12
            bmi @lpp 
        @NoSpaces:
        
        ;store long filename in seperate rambank
        cpy #13     ;check if filename>12
        bmi Continue3424
       
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
                lda TMPSTRING,y
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
        
        
        Continue3424:
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
                lda #' '
                jsr StoreCurrentAInMemory
                jsr StoreCurrentAInMemory
                jsr StoreCurrentAInMemory
                lda #'<'
                jsr StoreCurrentAInMemory
                lda #'d'
                jsr StoreCurrentAInMemory
                lda #'i'
                jsr StoreCurrentAInMemory
                lda #'r'
                jsr StoreCurrentAInMemory
                lda #'>'
                jsr StoreCurrentAInMemory
               
               
        jmp @SizeDone
        @NoDirectory:

            ;convert and store file size, Size is max 8 bytes incl  designator
            lda TMP_FILESIZE
            sta value_32bit

            lda TMP_FILESIZE+1
            sta value_32bit+1       
            jsr ConvertHex2Dec16Bitx           
        
            ;copy 4 bytes as max 9999mb (more then fat 32 allows)
            ldy #RESULT_LENGTH
            dey
            dey
            dey
            dey
            
            @lp1234:
                lda result_32bit,y
                jsr StoreCurrentAInMemory
                iny
                cpy #RESULT_LENGTH+1
                bne @lp1234
        
            lda #$20    ;space between size and designator
            jsr StoreCurrentAInMemory
                    
            ;add designator
            lda TMP_FILESIZE_DESIGNATOR
            jsr StoreCurrentAInMemory
        
            lda TMP_FILESIZE_DESIGNATOR+1
            jsr StoreCurrentAInMemory
        @SizeDone:
         
        ;Skip 3 bytes to start of date
        jsr CHRIN
        jsr CHRIN
        jsr CHRIN

        
        ldx #4  ;store date
    jmp NextByte
    
    StoreDateTime:
        ;next 10 bytes is date
      
        cmp #0
        beq @NoDateTimeFound      
        ldy #0
        @lp12345:
            jsr CHRIN
            
            jsr StoreCurrentAInMemory
            iny
            cpy #10
            bne @lp12345 
        jsr CHRIN   ;space
         
        ;next 5 bytes is time
        ldy #0
        @lp1234566:
            jsr CHRIN
            jsr StoreCurrentAInMemory
            iny
            cpy #5
            bne @lp1234566  
        jmp @Continue
        @NoDateTimeFound:
            ldx #0
            @lp44:
                
                lda #$20
                jsr StoreCurrentAInMemory
             
                inx
                cpx #15
            bne @lp44
                
                
                ;we are already at the next line
                jsr StoreEndOfFile
                
                lda TMP3
                cmp #1
                beq EndOfFile
                     
                lda #0
                ldx #0
                jmp CheckNewLine
        @Continue:
        
        
        jsr StoreEndOfFile

        
        lda TMP3
        cmp #1
        beq EndOfFile2
        
    jmp NextByte
    

    
    EndOfFile:
      
    jmp CleanUp
    
    EndOfFile2:
        
        ;add load next page option
 
        lda LAST_START_POINTER
        sta ZP_PTR
        lda LAST_START_POINTER+1
        sta ZP_PTR+1  
        
        lda #0
        jsr StoreCurrentAInMemory
        jsr StoreCurrentAInMemory
        
        
        ;add page down line!!!!
        ldx #0
        @lp:
            lda MSG_LOAD_MORE,x
            jsr StoreCurrentAInMemory
            inx
            cmp #0
            bne @lp
        
        inx
        inx
        lda #$20
        
        @lp2:
            jsr StoreCurrentAInMemory
            inx
            cpx #SET_RECORD_LENGTH
            bne @lp2
        jsr DecBankPointer
        jsr DecBankPointer
        jsr DecBankPointer

        lda #$3e
        jsr StoreCurrentAInMemory
        jsr IncBankPointer  

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
        jsr StoreCurrentAInMemory

        ;store pointer for longfile name
        lda LFN_Pointer
        jsr StoreCurrentAInMemory
        lda LFN_Pointer+1
        jsr StoreCurrentAInMemory
        
        inc FILECOUNT_CURRENT

        ;backup start if next line
        lda ZP_PTR
        sta LAST_START_POINTER
        lda ZP_PTR+1
        sta LAST_START_POINTER+1
        
       
        stz TMP3
        
        ldx FILECOUNT_CURRENT
        cpx #SET_MAX_FILES_COUNT
        bcs Eof444 ;maximum files per read
        
        ldx #0  ;search of next line

        
rts

Eof444:
    inc TMP3   ;tmp=1 -> is stop reading
    ldx #0    
rts


;Print a blank line with border chars on the screen
PrintScrnLineBlank2:
    phy
    phx
        sec
        jsr PLOT    ;read cur post
        
        lda #$A0
        jsr CHROUT
        
        clc
        lda #1
        adc WINDOW_WIDTH
        tay
        
        clc
        jsr PLOT

        lda #$A0
        jsr CHROUT
        
        tya
        adc WINDOW_WIDTH
        adc #1
        tay
        
        clc
        jsr PLOT

        lda #$A0
        jsr CHROUT 
    
        jsr print_lf
    plx
    ply
    
rts

;reset colors to white over blue
ResetColor:
    lda #$1F    ;first background
    jsr CHROUT
    lda #01
    jsr CHROUT
    lda #$05    ;set foreground
    jsr CHROUT
rts

;Print a line of the screen with special chars
PrintScreenLine:
    sta TMP2
    
    txa
    jsr CHROUT
    ldx #0
    :
        lda TMP
        jsr CHROUT
        inx
        cpx WINDOW_WIDTH
        bne :-
    lda TMP2
    jsr CHROUT
    ldx #0
    :
        lda TMP
        jsr CHROUT
        inx
        cpx WINDOW_WIDTH
        bne :-
    tya
    jsr CHROUT
    jsr print_lf

rts

PrintFromPointer:
    ldy #0
    :
        lda (ZP_PTR),y
        cmp #0
        beq @PrintDone
        jsr CHROUT
        iny
        jmp :-
        
    @PrintDone:
rts

;Print a blank screen with the structure, header and footer
PrintMainScreen2:
jmp PrintMainScreen2_start
    .ifdef uselauncher
        ProgramTitle:   .asciiz "- x16 Ultimate File Manager v1.2 RAM -"
    .endif
    .ifndef uselauncher
        ProgramTitle:   .asciiz "- x16 Ultimate File Manager v1.2 ROM -"
    .endif
    
    ColumnHeaders80:    .asciiz "-Filename-     -Size-   -Date/Time-"
    ColumnHeaders64:    .asciiz "-Filename-     -Size-  -Date-"
    ColumnHeaders40:    .asciiz "-Filename-"

    HelpInfo40: .byte "F2=info F3=part F4=edit  F5=copy F6=ren",$0d,"       F7=move F8=mkdir F10=quit",$0
    HelpInfo64: .byte "   F2=info F3=part F4=edit F5=copy F6=rename F7=move F8=mkdir",$0d,"                      F10=quit Del=delete ",$0
    HelpInfo80: .byte " F2=info F3=part F4=edit F5=copy F6=rename F7=move F8=mkdir F10=quit Del=delete",$0
    
PrintMainScreen2_start:

    ;check screen mode
    sec
    jsr SCREEN_MODE
    
    sty SCREEN_ROWS
    stx SCREEN_COLS

    ;set number of lines to 255 to prevent scrolling on writing past the screen
  ;  lda #$FF
  ;  sta $0387
    
    jsr SetCharsetIso

      
    jsr ResetColor
    jsr clearscreen    

    clc
    lda SCREEN_ROWS

    
    sbc #10
    sta ListSize 
    
    lda SCREEN_COLS
    sta TMP
    lsr ;divide by 2
    sec
    sbc #2  ;window withs
    sta WINDOW_WIDTH
    
    jsr print_lf

    ;get count of title
    stz CNT
    ldx #0
    :
        lda ProgramTitle,x
        cmp #0
        beq EndOfProgramTitle
        inc CNT
        inx
        jmp :-

    EndOfProgramTitle:

  

    lda CNT
    lsr ;divide by 2
    sta CNT
    lda WINDOW_WIDTH

        
    sec
    sbc CNT
    ldx #1
    tay
    iny
    iny


    clc
    jsr PLOT

    ;print title
    ldx #0
    :
        lda ProgramTitle,x
        cmp #0
        beq @PrintDone
        jsr CHROUT
        inx
        jmp :-
    
    @PrintDone:

    jsr print_lf
    jsr print_lf
    
    ;print top border line
    lda #$A1
    sta TMP     ;between byte
    ldx #$A2    ;left byte
    ldy #$A3    ;right byte
    lda #$A4    ;middle byte
    jsr PrintScreenLine

    jsr PrintScrnLineBlank2
    

    ;print line in between
    lda #$A1
    sta TMP     ;between byte
    ldx #$A5    ;left byte
    ldy #$A7    ;right byte
    lda #$A6    ;middle byte
    jsr PrintScreenLine    
    
    jsr PrintScrnLineBlank2
    ldx #0
    :
        phx
            jsr PrintScrnLineBlank2
        plx
         inx
         cpx ListSize
         bne :-

    ;print bottom line
    lda #$A1
    sta TMP     ;between byte
    ldx #$A8    ;left byte
    ldy #$A9    ;right byte
    lda #$AA    ;middle byte
    jsr PrintScreenLine 
    
    
    ;print column headers
    lda #$9E    
    jsr CHROUT  ;set color

    lda #1
    sta SET_SHOWDATE
    sta SET_SHOWTIME
    sta SET_SHOWSIZE
    lda #10 
    sta SET_DATE_LEN   
    
    lda SCREEN_COLS
    cmp #80
    bne Not80
        ldx #<ColumnHeaders80
        ldy #>ColumnHeaders80
        lda #<HelpInfo80
        sta ZP_PTR2
        lda #>HelpInfo80
        sta ZP_PTR2+1        
        jmp SetPointerDone
    Not80:
    cmp #40
    bne Not40
        ldx #<ColumnHeaders40
        ldy #>ColumnHeaders40
        lda #<HelpInfo40
        sta ZP_PTR2
        lda #>HelpInfo40
        sta ZP_PTR2+1        

        stz SET_SHOWDATE
        stz SET_SHOWTIME
        stz SET_SHOWSIZE        
        jmp SetPointerDone
    Not40:
    cmp #64
    bne SetPointerDone
        ldx #<ColumnHeaders64
        ldy #>ColumnHeaders64
        lda #<HelpInfo64
        sta ZP_PTR2
        lda #>HelpInfo64
        sta ZP_PTR2+1        
        stz SET_SHOWTIME
     ;   stz SET_SHOWTYPE
        lda #8
        sta SET_DATE_LEN           
    SetPointerDone:
    
    stx ZP_PTR
    sty ZP_PTR+1
    
    ;left window
    ldx #6
    ldy #1
    clc
    jsr PLOT
    jsr PrintFromPointer
    
    ;right window
    ldx #6
    lda #2
    clc
    adc WINDOW_WIDTH
    tay
    
    clc
    jsr PLOT
    jsr PrintFromPointer
    jsr ResetColor
       
    ;print help buttons
    lda ZP_PTR2
    sta ZP_PTR
    lda ZP_PTR2+1
    sta ZP_PTR+1
    
    ldx SCREEN_ROWS
    dex
    dex
    ldy #0
    clc
    jsr PLOT
    jsr PrintFromPointer
    
 
    
    
rts
   



PrintAToVera:
    jsr ConvertPetsciiToVera
    STA $9F23
rts

;Same as kernal PLOT but this sets the vera address to the x,y position with a 2 byte auto increment
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

;;;;TODO: almost the same is PLOTVera, merge into one!!
;Same as kernal PLOT but this sets the vera address to the x,y position with a 1 byte auto increment
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

;Copy a file using the dos command
;same device, other partition: will not work use DOS"C:" instead
CopyUsingDos:
jmp CopyUsingDos_start

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

rts

;Copy selected file to direcoty of other window
CopyAFile:
   jmp CopyAFile_Start
    CD_ROOT: .asciiz "cd:/"
    PB_LENGTH: .byte 36     ;Length of the progress bar
       
CopyAFile_Start:  
    jsr SetRambank63
    
    ;if one of them is partition=hostfs: own copy works
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
        ;same device, other partition: will not work use DOS"C: instead
        jmp CopyUsingDos
    


    @ContinueCopy:
    ;change cur dir to root
    CopyAddrUntilZero CD_ROOT,CMD_BUFFER,CMD_BUFFER_LENGTH
    jsr DoDosCMD

    
    CopyAddrUntilZero CD_ROOT,CMD_BUFFER,CMD_BUFFER_LENGTH
    inc DOS_CMD_DEVICE  ;on other device also
    jsr DoDosCMD


   ;count length of commands

    CountUntilZero SourceFile,SourceFileCnt
    CountUntilZero DestFile,DestFileCnt

   ;Initialize progress bar
    ;Progressbar is a non-floatingpoint version which approximated the progress, Good enough
        jsr LoadCurrentFileSize
        jsr SetRambank63
        jsr InitAdder24bit
        
        ldx #0   ;Check the amount of times PB_LENGTH fits into 255
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

    ;set correct x,y for printing the progress bar
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
        

    jsr PLOT
    
    ;set color
     lda #$9B    ;set foreground
    jsr CHROUT       
    lda #01 ;flip
    jsr CHROUT
   
    
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
        sta TMPSTRING,x
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
        lda TMPSTRING,x
        inx
        cpx CopyBuffercnt
        beq ExitWriteLoop
        jsr CHROUTbin
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
      
      
   jmp loopCopyByte ;copy next buffer



   @eof:


    jsr CLRCHN
    lda #2
    jsr CLOSE
    lda #1
    jsr CLOSE
    jsr ResetColor
    jsr RestoreRambank
rts


;check if current file is tokenized basic
CheckFileType:


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

;Get the current directory when UFM starts
GetStartDirectory2:
jmp GetStartDirectory2_start
    CURDIRCMD: .asciiz "$=c"
GetStartDirectory2_start:
    CopyAddrUntilZero CURDIRCMD,CMD_BUFFER,CMD_BUFFER_LENGTH    

    lda #1   ; Logical Number = 1
    ldx $03fe ;#8   ; Device = "SD card" (emulation host FS)
    ldy #0   ; Secondary Address = 15: dos command channel
    jsr SETLFS

    lda CMD_BUFFER_LENGTH ; command
    ldx #<CMD_BUFFER
    ldy #>CMD_BUFFER

    jsr SETNAM
    jsr OPEN
    
    LDX #1
    jsr CHKIN
    stz CNT
    ldy #0
    lda #'/'
    sta CMD_BUFFER,Y
    iny
    ldx #0
    ReadLoop2:
        jsr READST
        bne eof22

 
        jsr CHRIN
   
        inc CNT
        pha
            lda CNT
            cmp #30
        pla
        bcc ReadLoop2      
        cmp #$22
        beq InverseReadByteInX
        
        cpx #1
        bne ReadLoop2
        sta CMD_BUFFER,y

        iny
    
    jmp ReadLoop2
    
    InverseReadByteInX:
        inx
        cpx #2
        bne ReadLoop2
        lda #'/'
        sta CMD_BUFFER,y

        iny
        ldx #0
    jmp ReadLoop2
eof22:
    lda #0
    sta CMD_BUFFER+1,y

    
    sty CNT
    jsr CLRCHN
    lda #1
    jsr CLOSE
    iny
    ;skip last slashes
    ldy CNT
    :
        dey
        cpy #$FF
        beq @Done
        lda CMD_BUFFER,y
        cmp #'/'
        
        beq :-
    @Done:
    
    jsr SetRambank63
    
    ldx #0
    lda #'/'
    sta CUR_PATH,x
    inx
    
    sty TMP ;Y to read to
    NextYZZ:
       cpy #$FF
       beq CopyDone
      
       lda CMD_BUFFER,y
       cmp #'/'
       beq StoreUntilLastY
       dey
       jmp NextYZZ
    
    StoreUntilLastY:


        inc TMP
        phy
            iny
            :
                lda CMD_BUFFER,y
                sta CUR_PATH,x
                inx
                iny
                cpy TMP
                bne :-           

        ply    
        lda #'/'
        sta CUR_PATH,x
        inx
        dey
        tya

        sty TMP
    jmp NextYZZ
    
    CopyDone:

        lda #$0
        sta CUR_PATH,X

    jsr RestoreRambank
rts

;Check if selected file is launchable
IsLaunchable:
    lda #1 
    ldx DEVICE_CURRENT ;#8  
    ldy #2  
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
    
    ;next char should be $08
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

;Copy startdir to and of rambank 0
StoreStartDirForLauncher:
    lda $00
    pha
        ;$BF00
        ldx #0
        :
            phx
                lda #63
                sta $00
            plx 
            lda CUR_PATH,x
            phx
                pha
                    lda #0
                    sta $00
                pla
            plx
            sta $BF00,x
            
            inx
            cmp #0
            bne :-


    pla
    sta $00
    


rts


;Copy the launcher to  end of basic memory for RAM-version
CopyLaunchToMem:
.ifdef uselauncher
;$21: r0 = r0L = $02, r0H = $03, r1 = r1L = $04 etc.
    ;set source
    lda #<launch_prg
    sta $02     ;r0Low
    lda #>launch_prg
    sta $03     ;r0High

    ;set destionation $8000 ;$9717
    lda #$17
    sta $04     ;r1Low
    lda #$97
    sta $05     ;r1High
    
    ;calculate and set length
    sec				; set carry for borrow purpose
	lda #<launch_prg_end  ;num1lo
	sbc #<launch_prg ;num2lo			; perform subtraction on the LSBs
	sta $06
    
	lda #>launch_prg_end    ;num1hi			; do the same for the MSBs, with carry
	sbc #>launch_prg			; set according to the previous result
	sta $07
    jsr memory_copy   ;memcopy kernal function
.endif

rts


LoadAndLaunch:
    CopyAddrUntilZero CMD_BUFFER,CMD,cnt


    jsr ResetVera
    
    

   ;switch to petscii
    lda #2
    jsr screen_set_charset 

    lda #$8F
    jsr CHROUT
    
    lda #$8E
    jsr CHROUT 
    
    jsr LoadPRGFileIntoMemory
 
    jsr DoBasicRunCommand
rts    




jsrfar: .include "jsrfar.inc"

    .include "functions.s"
    .include "functions_shared.s"
    .include "jsrfar_kernal.s"
   ; .include "debug.s"
  
;RAM-version
launch_prg:
    .ifdef uselauncher
        .incbin "bin/launch.prg",$01 ;skip first byte
    .endif
launch_prg_end:

;ROM-version
.ifndef uselauncher
    .include "launcher.s"
.endif
