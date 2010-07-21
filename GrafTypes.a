;-----------------------------------------------------------------
;
;  -->  GRAFTYPES.TEXT
;
;  QUICKDRAW TYPE DECLARATIONS, USED BY ALL GRAPHICS ROUTINES
;
NIL             .EQU    0                       ;IMPLEMENTATION VALUE OF NIL


;-----------------------------------------------
;
;  QuickDraw VERBS:
;
FRAME           .EQU    0
PAINT           .EQU    1
ERASE           .EQU    2
INVERT          .EQU    3
FILL            .EQU    4


;-----------------------------------------------
;
;  QuickDraw transfer MODES:
;
srcCopy         .EQU    0
srcOr           .EQU    1
srcXor          .EQU    2
srcBic          .EQU    3
notSrcCopy      .EQU    4
notSrcOr        .EQU    5
notSrcXor       .EQU    6
notSrcBic       .EQU    7
patCopy         .EQU    8
patOr           .EQU    9
patXor          .EQU    10
patBic          .EQU    11
notPatCopy      .EQU    12
notPatOr        .EQU    13
notPatXor       .EQU    14
notPatBic       .EQU    15


;-----------------------------------------------
;
;  QuickDraw Color Separation:
;
normalBit       .EQU    0                       ;normal screen mapping
inverseBit      .EQU    1                       ;inverse screen mapping
redBit          .EQU    4                       ;RGB additive mapping
greenBit        .EQU    3                       ;for photos from screen
blueBit         .EQU    2
cyanBit         .EQU    8                       ;CMYBk subtractive mapping
magentaBit      .EQU    7                       ;for ink jet printer
yellowBit       .EQU    6
blackBit        .EQU    5

blackColor      .EQU    33
whiteColor      .EQU    30
redColor        .EQU    205
greenColor      .EQU    341
blueColor       .EQU    409
cyanColor       .EQU    273
magentaColor    .EQU    137
yellowColor     .EQU    69



;-----------------------------------------------
;
;  OFFSETS WITHIN A POINT:
;
V               .EQU    0                       ;WORD
H               .EQU    2                       ;WORD


;-----------------------------------------------
;
;  OFFSETS WITHIN A RECT:
;
TOPLEFT         .EQU   0                        ;POINT
BOTRIGHT        .EQU   4                        ;POINT

TOP             .EQU   0                        ;INTEGER
LEFT            .EQU   2                        ;INTEGER
BOTTOM          .EQU   4                        ;INTEGER
RIGHT           .EQU   6                        ;INTEGER


;-----------------------------------------------
;
;  OFFSETS WITHIN A BITMAP:
;
BASEADDR        .EQU    0                       ;LONG
ROWBYTES        .EQU    4                       ;WORD
BOUNDS          .EQU    6                       ;RECT


;-----------------------------------------------
;
;  OFFSETS WITHIN A CURSOR:
;
DATA            .EQU    0                       ;16 WORDS
MASK            .EQU    32                      ;16 WORDS
HOTSPOT         .EQU    64                      ;POINT



;-----------------------------------------------
;
;  OFFSETS WITHIN A POLYGON:
;
POLYSIZE        .EQU    0                       ;WORD, TOTAL BYTES
POLYBBOX        .EQU    2                       ;RECT
POLYPOINTS      .EQU    10                      ;ARRAY[0..0] OF Point


;-----------------------------------------------
;
;  OFFSETS WITHIN A REGION:
;
RGNSIZE         .EQU    0                       ;WORD, TOTAL BYTES
RGNBBOX         .EQU    2                       ;RECT
RGNDATA         .EQU    10                      ;START OF RGN DATA


;-----------------------------------------------
;
;  OFFSETS WITHIN A PICTURE:
;
PICSIZE         .EQU    0                       ;WORD, TOTAL BYTES
PICFRAME        .EQU    2                       ;RECT
PICDATA         .EQU    10                      ;START OF BYTE CODES


;-----------------------------------------------
;
;  OFFSETS WITHIN QDProcs RECORD:
;
textProc        .EQU    0                       ;PROCPTR
lineProc        .EQU    textProc+4              ;PROCPTR
rectProc        .EQU    lineProc+4              ;PROCPTR
rRectProc       .EQU    rectProc+4              ;PROCPTR
ovalProc        .EQU    rRectProc+4             ;PROCPTR
arcProc         .EQU    ovalProc+4              ;PROCPTR
polyProc        .EQU    arcProc+4               ;PROCPTR
rgnProc         .EQU    polyProc+4              ;PROCPTR
bitsProc        .EQU    rgnProc+4               ;PROCPTR
commentProc     .EQU    bitsProc+4              ;PROCPTR
txMeasProc      .EQU    commentProc+4           ;PROCPTR
getPicProc      .EQU    txMeasProc+4            ;PROCPTR
putPicProc      .EQU    getPicProc+4            ;PROCPTR



;-----------------------------------------------
;
;  OFFSETS WITHIN A GRAFPORT:
;
device          .EQU    0                       ;WORD
portBits        .EQU    device+2                ;BITMAP
portRect        .EQU    portBits+14             ;RECT
visRgn          .EQU    portRect+8              ;RGNPTR
clipRgn         .EQU    visRgn+4                ;RGNPTR
bkPat           .EQU    clipRgn+4               ;PATTERN
fillPat         .EQU    bkPat+8                 ;PATTERN
pnLoc           .EQU    fillPat+8               ;POINT
pnSize          .EQU    pnLoc+4                 ;POINT
pnMode          .EQU    pnSize+4                ;WORD
pnPat           .EQU    pnMode+2                ;PATTERN
pnVis           .EQU    pnPat+8                 ;WORD
txFont          .EQU    pnVis+2                 ;WORD
txFace          .EQU    txFont+2                ;WORD
txMode          .EQU    txFace+2                ;WORD
txSize          .EQU    txMode+2                ;WORD
spExtra         .EQU    txSize+2                ;Fixed Point
fgColor         .EQU    spExtra+4               ;LONG
bkColor         .EQU    fgColor+4               ;LONG
colrBit         .EQU    bkColor+4               ;WORD
patStretch      .EQU    colrBit+2               ;WORD
picSave         .EQU    patStretch+2            ;handle
rgnSave         .EQU    picSave+4               ;handle
polySave        .EQU    rgnSave+4               ;handle
grafProcs       .EQU    polySave+4              ;Pointer
PORTREC         .EQU    grafProcs+4             ;SIZE OF A GRAFPORT
PORTBOUNDS      .EQU    PORTBITS+BOUNDS


;-----------------------------------------------------
;
;  OFFSETS IN A REGION STATE RECORD:
;
RGNPTR          .EQU    0                       ;LONG
DATAPTR         .EQU    RGNPTR+4                ;LONG
SCANBUF         .EQU    DATAPTR+4               ;LONG
SCANSIZE        .EQU    SCANBUF+4               ;WORD
THISV           .EQU    SCANSIZE+2              ;WORD
NEXTV           .EQU    THISV+2                 ;WORD
MINH            .EQU    NEXTV+2                 ;WORD
MAXH            .EQU    MINH+2                  ;WORD
LEFTH           .EQU    MAXH+2                  ;WORD
RGNREC          .EQU    LEFTH+2                 ;SIZE OF A REGION RECORD


;-----------------------------------------------------
;
;  Offsets in a PicSave record:
;
thePic          .EQU    0                       ;PICHANDLE
picMax          .EQU    thePic+4                ;LongInt
picIndex        .EQU    picMax+4                ;LongInt
picClipRgn      .EQU    picIndex+4              ;RgnHandle
picBkPat        .EQU    picClipRgn+4            ;Pattern
picTxFont       .EQU    picBkPat+8              ;WORD
picTxFace       .EQU    picTxFont+2             ;Style
picTxMode       .EQU    picTxFace+2             ;WORD
picTxSize       .EQU    picTxMode+2             ;WORD
picSpExtra      .EQU    picTxSize+2             ;Fixed Point
picTxNumer      .EQU    picSpExtra+4            ;Point
picTxDenom      .EQU    picTxNumer+4            ;Point
picTxLoc        .EQU    picTxDenom+4            ;Point
picPnLoc        .EQU    picTxLoc+4              ;Point
picPnSize       .EQU    picPnLoc+4              ;Point
picPnMode       .EQU    picPnSize+4             ;WORD
picPnPat        .EQU    picPnMode+2             ;Pattern
picFillPat      .EQU    picPnPat+8              ;Pattern
picTheRect      .EQU    picFillPat+8            ;Rect
picOvSize       .EQU    picTheRect+8            ;Point
picOrigin       .EQU    picOvSize+4             ;Point
picFgColor      .EQU    picOrigin+4             ;Long
picBkColor      .EQU    picFgColor+4            ;Long

picSaveRec      .EQU    picBkColor+4            ;total size in bytes


;-----------------------------------------------------
;
;  QuickDraw GLOBAL VARIABLES:
;
;  52(A5) CONTAINS A POINTER TO THEPORT.
;  ALL OTHER GLOBAL VARIABLES ARE EXPRESSED RELATIVE TO THEPORT.
;
GRAFGLOBALS     .EQU    0                       ;A5 OFFSET TO GLOBALPTR


;-----------------------------------------------------------
;
;  QuickDraw PUBLIC GLOBAL VARIABLES:
;
thePort         .EQU    0                       ;GrafPtr
white           .EQU    thePort-8               ;Pattern
black           .EQU    white-8                 ;Pattern
gray            .EQU    black-8                 ;Pattern
ltGray          .EQU    gray-8                  ;Pattern
dkGray          .EQU    ltGray-8                ;Pattern
arrow           .EQU    dkGray-68               ;Cursor
screenBits      .EQU    arrow-14                ;BitMap
randSeed        .EQU    screenBits-4            ;LONGINT


;------------------------------------------------------------
;
;  QuickDraw private global variables:
;
wideOpen        .EQU    randSeed-4              ;RgnHandle
wideMaster      .EQU    wideOpen-4              ;RgnPtr
wideData        .EQU    wideMaster-10           ;Fake Region
rgnBuf          .EQU    wideData-4              ;PointsHandle
rgnIndex        .EQU    rgnBuf-2                ;INTEGER
rgnMax          .EQU    rgnIndex-2              ;INTEGER
playPic         .EQU    rgnMax-4                ;Long
QDSpare0        .EQU    playPic-2               ;unused word
thePoly         .EQU    QDSpare0-4              ;POLYHANDLE
polyMax         .EQU    thePoly-2               ;INTEGER
patAlign        .EQU    polyMax-4               ;Point
fixTxWid        .EQU    patAlign-4              ;Fixed Point
fontPtr         .EQU    fixTxWid-4              ;long, ^FMOutput record
playIndex       .EQU    fontPtr-4               ;long
QDSpare3        .EQU    playIndex-2             ;unused word
QDSpare4        .EQU    QDSpare3-2              ;unused word
QDSpare5        .EQU    QDSpare4-2              ;unused word
QDSpare6        .EQU    QDSpare5-2              ;unused word
QDSpare7        .EQU    QDSpare6-2              ;unused word
QDSpare8        .EQU    QDSpare7-2              ;unused word
QDSpare9        .EQU    QDSpare8-2              ;unused word
QDSpareA        .EQU    QDSpare9-2              ;unused word
QDSpareB        .EQU    QDSpareA-2              ;unused word
QDSpareC        .EQU    QDSpareB-2              ;unused word
QDSpareD        .EQU    QDSpareC-2              ;unused word
lastGrafGlob    .EQU    QDSpareD
grafSize        .EQU    4-lastGrafGlob          ;total size in bytes



        .MACRO  UNLINK
;--------------------------------------------------------------
;
;  UNLINK A6, STRIP PARAMETERS, AND RETURN.
;
;  FIRST PARAM IS NUMBER OF BYTES OF STACK BIAS.
;
        UNLK    A6                              ;RELEASE LOCAL VARIABLES

        .IF     %1=0                            ;NO PARAMETERS ?
        RTS                                     ;THEN JUST RTS

        .ELSE
        .IF     %1=4                            ;4 BYTES OF PARAMS ?
        MOVE.L  (SP)+,(SP)                      ;YES, STRIP AND ADJUST RET ADDR
        RTS

        .ELSE                                   ;NOT 0 OR 4 BYTES OF PARAMS
        MOVE.L  (SP)+,A0                        ;POP RETURN ADDR INTO A0
        ADD     #%1,SP                          ;STRIP PARAMETERS
        JMP     (A0)                            ;JUMP THRU A0 TO RETURN
        .ENDC
        .ENDC

        .ENDM



;----------------------------------------------
;
;  Trap Macros used by QuickDraw:
;

        .MACRO  _LongMul
        .WORD   $A867
        .ENDM

        .MACRO  _FixMul
        .WORD   $A868
        .ENDM

        .MACRO  _FixRatio
        .WORD   $A869
        .ENDM

        .MACRO  _NewHandle
        .WORD   $A122
        .ENDM

        .MACRO  _DisposHandle
        .WORD   $A023
        .ENDM

        .MACRO  _SetHandleSize
        .WORD   $A024
        .ENDM

        .MACRO  _GetHandleSize
        .WORD   $A025
        .ENDM

        .MACRO  _HLock
        .WORD   $A029
        .ENDM

        .MACRO  _HUnlock
        .WORD   $A02A
        .ENDM

        .MACRO  _GetScrnBits
        .WORD   $A833                           ;new trap number
        .ENDM

        .MACRO  _StackAvail
        MOVE.L  SP,D0                           ;copy stack pointer
        SUB.L   $114,D0                         ;subtract HeapEnd for stack avail
        .ENDM

        .MACRO  _SwapFont
        MOVE.L  $8E0,A0                         ;get pointer to FMSwapFont
        JSR     (A0)                            ;call font manager
        .ENDM
