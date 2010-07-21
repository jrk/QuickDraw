        .INCLUDE GRAFTYPES.TEXT
;-----------------------------------------------------------
;
;
;     ****   *****   ***   ***   ***   *   *   ***
;     *   *  *      *   *   *   *   *  *   *  *   *
;     *   *  *      *       *   *   *  **  *  *
;     ****   ***    *  **   *   *   *  * * *   ***
;     * *    *      *   *   *   *   *  *  **      *
;     *  *   *      *   *   *   *   *  *   *  *   *
;     *   *  *****   ***   ***   ***   *   *   ***
;
;
;
;  QuickDraw Routines to operate on Regions.
;

        .PROC StdRgn,2
        .REF  CheckPic,PutPicVerb,DPutPicByte,PutPicRgn
        .REF  PutRgn,FrRgn,PushVerb,DrawRgn
;---------------------------------------------------------------
;
;  PROCEDURE StdRgn(verb: GrafVerb; rgn: RgnHandle);
;
;  A6 OFFSETS OF PARAMS AFTER LINK:
;
PARAMSIZE       .EQU    6
VERB            .EQU    PARAMSIZE+8-2           ;GRAFVERB
RGN             .EQU    VERB-4                  ;LONG, RGNHANDLE

        LINK    A6,#0                           ;NO LOCALS
        MOVEM.L D6-D7/A2-A4,-(SP)               ;SAVE REGS
        MOVE.B  VERB(A6),D7                     ;GET VERB
        JSR     CHECKPIC                        ;SET UP A4,A3 AND CHECK PICSAVE
        BLE.S   NOTPIC                          ;BRANCH IF NOT PICSAVE

        MOVE.B  D7,-(SP)                        ;PUSH VERB
        JSR     PutPicVerb                      ;PUT ADDIONAL PARAMS TO THEPIC
        MOVE    #$80,D0                         ;PUT RGNNOUN IN HI NIBBLE
        ADD     D7,D0                           ;PUT VERB IN LO NIBBLE
        JSR     DPutPicByte                     ;PUT OPCODE TO THEPIC
        MOVE.L  RGN(A6),-(SP)                   ;PUSH RGNHANDLE
        JSR     PutPicRgn                       ;PUT REGION TO THEPIC

NOTPIC  MOVE.L  RGN(A6),-(SP)                   ;PUSH RGNHANDLE
        JSR     PushVerb                        ;PUSH MODE AND PATTERN
        TST.B   D7                              ;IS VERB FRAME ?
        BNE.S   NOTFR                           ;NO, CONTINUE
        TST.L   RGNSAVE(A3)                     ;YES, IS RGNSAVE TRUE ?
        BEQ.S   NOTRGN                          ;NO, CONTINUE
        MOVE.L  RGN(A6),-(SP)                   ;YES, PUSH RGNHANDLE
        MOVE.L  RGNBUF(A4),-(SP)                ;PUSH RGNBUF
        PEA     RGNINDEX(A4)                    ;PUSH VAR RGNINDEX
        PEA     RGNMAX(A4)                      ;PUSH VAR RGNMAX
        JSR     PutRgn                          ;ADD INVERSION PTS TO THERGN

NOTRGN  JSR     FrRgn                           ;FrRgn(rgn,pnMode,pnPat)
        BRA.S   GOHOME

NOTFR   JSR     DrawRgn                         ;DrawRgn(rgn,mode,pat);

GOHOME  MOVEM.L (SP)+,D6-D7/A2-A4               ;RESTORE REGS
        UNLINK  PARAMSIZE,'STDRGN  '



        .PROC FrameRgn,1
        .DEF  CallRgn,PaintRgn,EraseRgn,InvertRgn,FillRgn
        .REF  StdRgn
;-----------------------------------------------------
;
;  PROCEDURE FrameRgn(* rgn: RgnHandle *);
;
        MOVEQ   #FRAME,D0                       ;VERB = FRAME
        BRA.S   CallRgn                         ;SHARE COMMON CODE


;-----------------------------------------------------
;
;  PROCEDURE PaintRgn(* rgn: RgnHandle *);
;
PaintRgn
        MOVEQ   #PAINT,D0                       ;VERB = PAINT
        BRA.S   CallRgn                         ;SHARE COMMON CODE


;--------------------------------------------------------
;
;  PROCEDURE EraseRgn(* rgn: RgnHandle *);
;
EraseRgn
        MOVEQ   #ERASE,D0                       ;VERB = ERASE
        BRA.S   CallRgn                         ;SHARE COMMON CODE


;--------------------------------------------------------
;
;  PROCEDURE InvertRgn(* rgn: RgnHandle *);
;
InvertRgn
        MOVEQ   #INVERT,D0                      ;VERB = INVERT
        BRA.S   CallRgn                         ;SHARE COMMON CODE


;--------------------------------------------------------
;
;  PROCEDURE FillRgn(* rgn: RgnHandle; pat: Pattern *);
;
FillRgn
        MOVE.L  (SP)+,A0                        ;POP RETURN ADDR
        MOVE.L  (SP)+,A1                        ;POP ADDR OF PATTERN
        MOVE.L  A0,-(SP)                        ;PUT RETURN ADDR BACK
        MOVE.L  GRAFGLOBALS(A5),A0              ;POINT TO LISAGRAF GLOBALS
        MOVE.L  THEPORT(A0),A0                  ;GET CURRENT GRAFPORT
        LEA     FILLPAT(A0),A0                  ;POINT TO FILLPAT
        MOVE.L  (A1)+,(A0)+                     ;COPY PAT INTO FILLPAT
        MOVE.L  (A1)+,(A0)+                     ;ALL EIGHT BYTES
        MOVEQ   #FILL,D0                        ;VERB = FILL
        BRA.S   CallRgn                         ;SHARE COMMON CODE



;---------------------------------------------------------------
;
;  PROCEDURE CallRgn(rgn: RgnHandle);
;
;  code shared by FrameRgn, PaintRgn, EraseRgn, InvertRgn, and FillRgn.
;  enter with verb in D0.
;
CallRgn
        MOVE.L  (SP)+,A0                        ;POP RETURN ADDR
        MOVE.L  (SP)+,A1                        ;POP RGN
        MOVE.B  D0,-(SP)                        ;PUSH VERB
        MOVE.L  A1,-(SP)                        ;PUSH RGN
        MOVE.L  A0,-(SP)                        ;RESTORE RETURN ADDR
        MOVE.L  GRAFGLOBALS(A5),A0              ;POINT TO LISAGRAF GLOBALS
        MOVE.L  THEPORT(A0),A0                  ;GET CURRENT GRAFPORT
        MOVE.L  GRAFPROCS(A0),D0                ;IS GRAFPROCS NIL ?
        LEA     STDRGN,A0
        BEQ.S   USESTD                          ;YES, USE STD PROC
        MOVE.L  D0,A0
        MOVE.L  RGNPROC(A0),A0                  ;NO, GET PROC PTR
USESTD  JMP     (A0)                            ;GO TO IT


        .PROC DrawRgn,3
        .REF  RgnBlt
;--------------------------------------------------------
;
;  PROCEDURE DrawRgn(rgn: RgnHandle; mode: INTEGER; pat: Pattern);
;
;  A6 OFFSETS OF PARAMS AFTER LINK:
;
PARAMSIZE       .EQU    10
RGN             .EQU    PARAMSIZE+8-4           ;LONG, RGNHANDLE
MODE            .EQU    RGN-2                   ;WORD
PAT             .EQU    MODE-4                  ;LONG, ADDR OF PATTERN

        LINK    A6,#0
        MOVE.L  GRAFGLOBALS(A5),A0              ;POINT TO LISAGRAF GLOBALS
        MOVE.L  THEPORT(A0),A0                  ;GET CURRENT PORT
        TST     PNVIS(A0)                       ;IS PNVIS NEG ?
        BMI.S   DONE                            ;YES, QUIT
        PEA     PORTBITS(A0)                    ;PUSH SRCBITS
        MOVE.L  (SP),-(SP)                      ;PUSH DSTBITS
        PEA     PORTBITS+BOUNDS(A0)             ;PUSH SRCRECT
        MOVE.L  (SP),-(SP)                      ;PUSH DSTRECT
        MOVE    MODE(A6),-(SP)                  ;PUSH MODE
        MOVE.L  PAT(A6),-(SP)                   ;PUSH PAT
        MOVE.L  CLIPRGN(A0),-(SP)               ;PUSH CLIPRGN
        MOVE.L  VISRGN(A0),-(SP)                ;PUSH VISRGN
        MOVE.L  RGN(A6),-(SP)                   ;PUSH RGN
        JSR     RGNBLT                          ;CALL RGNBLT
DONE    UNLINK  PARAMSIZE,'DRAWRGN '



        .PROC FrRgn,3
        .REF  FrRect,NewRgn,CopyRgn,InsetRgn,DiffRgn,DrawRgn
;--------------------------------------------------------
;
;  PROCEDURE FrRgn(rgn: RgnHandle; mode: INTEGER; pat: Pattern);
;
;  A6 OFFSETS OF PARAMS AFTER LINK:
;
PARAMSIZE       .EQU    10
RGN             .EQU    PARAMSIZE+8-4           ;LONG, RGNHANDLE
MODE            .EQU    RGN-2                   ;WORD
PAT             .EQU    MODE-4                  ;LONG, ADDR OF PATTERN

        LINK    A6,#0
        MOVEM.L D7/A3-A4,-(SP)                  ;SAVE REGS
        MOVE.L  GRAFGLOBALS(A5),A4              ;POINT TO LISAGRAF GLOBALS
        MOVE.L  THEPORT(A4),A3                  ;GET CURRENT PORT
        TST     PNVIS(A3)                       ;IS PNVIS NEG ?
        BMI.S   DONE                            ;YES, QUIT
;
;  special case rectangular region for speed.
;
        MOVE.L  RGN(A6),A0                      ;GET RGNHANDLE
        MOVE.L  (A0),A0                         ;DE-REFERENCE IT
        CMP     #10,RGNSIZE(A0)                 ;IS IT RECTANGULAR ?
        BNE.S   NOTRECT                         ;NO, CONTINUE
        PEA     RGNBBOX(A0)                     ;YES, PUSH ADDR OF BBOX
        JSR     FRRECT                          ;FRAME IT
        BRA.S   DONE                            ;AND QUIT

NOTRECT CLR.L   -(SP)                           ;MAKE ROOM FOR FCN RESULT
        JSR     NEWRGN                          ;ALLOCATE TEMPRGN
        MOVE.L  (A7)+,D7                        ;PUT TEMPRGN IN D7
        MOVE.L  RGN(A6),-(SP)                   ;PUSH RGN
        MOVE.L  D7,-(SP)                        ;PUSH TEMPRGN
        JSR     COPYRGN                         ;COPY RGN INTO TEMPRGN
        MOVE.L  D7,-(SP)                        ;PUSH TEMPRGN
        MOVE.L  PNSIZE(A3),-(SP)                ;PUSH PNSIZE
        JSR     INSETRGN                        ;InsetRgn(tempRgn,pnSize);
        MOVE.L  RGN(A6),-(SP)                   ;PUSH RGN
        MOVE.L  D7,-(SP)                        ;PUSH TEMPRGN
        MOVE.L  D7,-(SP)                        ;PUSH TEMPRGN
        JSR     DIFFRGN                         ;DiffRgn(rgn,tempRgn,tempRgn);
        MOVE.L  D7,-(SP)                        ;PUSH TEMPRGN
        MOVE    MODE(A6),-(SP)                  ;PUSH MODE
        MOVE.L  PAT(A6),-(SP)                   ;PUSH PAT
        JSR     DRAWRGN                         ;DrawRgn(tempRgn,mode,pat);
        MOVE.L  D7,A0                           ;GET TEMPRGN
        _DisposHandle                           ;DISCARD IT
DONE    MOVEM.L (SP)+,D7/A3-A4                  ;RESTORE REGS
        UNLINK  PARAMSIZE,'FRRGN   '



        .FUNC NewRgn,0
        .REF  NewHandle
;---------------------------------------------------
;
;  FUNCTION NewRgn;
;  Allocate a new region and set it to the empty region.
;
        MOVEM.L D3/A2,-(SP)                     ;SAVE REGS
        CLR.L   -(SP)                           ;MAKE ROOM FOR FCN RESULT
        MOVE    #10,-(SP)                       ;PUSH BYTECOUNT=10
        JSR     NEWHANDLE                       ;ALLOCATE A RELOCATABLE OBJECT
        MOVE.L  (SP)+,A0                        ;POP RESULTING HANDLE
        MOVEM.L (SP)+,D3/A2                     ;RESTORE REGS
        MOVE.L  A0,4(SP)                        ;STORE INTO NEWRGN RESULT
        MOVE.L  (A0),A0                         ;DE-REFERENCE HANDLE
        MOVE    #10,(A0)+                       ;INSTALL RGNSIZE=10
        CLR.L   (A0)+                           ;INSTALL RGNBBOX=(0,0,0,0)
        CLR.L   (A0)+
        RTS                                     ;RETURN TO CALLER



        .PROC DisposeRgn,1
;---------------------------------------------------
;
;  PROCEDURE DisposeRgn(rgn: RgnHandle);
;
        MOVE.L  (SP)+,A1                        ;pop return addr
        MOVE.L  (SP)+,A0                        ;pop handle
        _DisposHandle                           ;discard it
        JMP     (A1)                            ;and return



        .PROC OpenRgn,0
        .REF  NewHandle,HidePen
;---------------------------------------------------
;
;  PROCEDURE OpenRgn;
;
        MOVEM.L D3/A2-A4,-(SP)                  ;SAVE REGS
        MOVE.L  GRAFGLOBALS(A5),A4              ;POINT TO QUICKDRAW GLOBALS
        MOVE.L  THEPORT(A4),A0                  ;GET CURRENT GRAFPORT
        MOVE.L  #1,D0
        MOVE.L  D0,RGNSAVE(A0)                  ;RGNSAVE := TRUE
        CLR     RGNINDEX(A4)                    ;RGNINDEX := 0
        CLR.L   -(SP)                           ;MAKE ROOM FOR FCN RESULT
        MOVE    #256,-(SP)                      ;PUSH BYTE COUNT = 256
        MOVE    (SP),RGNMAX(A4)                 ;RGNMAX := 256 TOO;
        JSR     NEWHANDLE
        MOVE.L  (SP)+,RGNBUF(A4)                ;RGNBUF := NEWHANDLE(RGNMAX)
        JSR     HidePen
        MOVEM.L (SP)+,D3/A2-A4                  ;RESTORE REGS
        RTS                                     ;AND RETURN



        .PROC CloseRgn,1
        .REF  ShowPen,SortPoints,CullPoints,PackRgn
;---------------------------------------------------
;
;  PROCEDURE CloseRgn(* dstRgn: RgnHandle *);
;  Sort array of inversion points and pack into region.
;
;  A6 OFFSETS OF PARAMS AND LOCALS AFTER LINK:
;
PARAMSIZE       .EQU    4                       ;TOTAL BYTES OF PARAMETERS
DSTRGN          .EQU    PARAMSIZE+8-4           ;LONG, RGNHANDLE
PTCOUNT         .EQU    -2                      ;WORD
VARSIZE         .EQU    PTCOUNT                 ;SIZE OF LOCAL VARS


        LINK    A6,#VARSIZE
        MOVEM.L D3/A2-A4,-(SP)                  ;SAVE REGS
        MOVE.L  GRAFGLOBALS(A5),A4              ;POINT TO QUICKDRAW GLOBALS
        MOVE.L  THEPORT(A4),A0                  ;GET CURRENT GRAFPORT
        TST.L   RGNSAVE(A0)                     ;IS RGNSAVE TRUE ?
        BEQ.S   DONE                            ;NO, ABORT
        CLR.L   RGNSAVE(A0)                     ;YES, RGNSAVE := FALSE
        JSR     SHOWPEN                         ;UNDO THE HIDEPEN FROM OPENRGN
        MOVE    RGNINDEX(A4),D0                 ;GET CURRENT RGNINDEX
        LSR     #2,D0                           ;DIV BY 4
        MOVE    D0,PTCOUNT(A6)                  ;FOR PTCOUNT
        MOVE.L  RGNBUF(A4),A3                   ;GET RGNBUF HANDLE
        MOVE.L  (A3),-(SP)                      ;PUSH BUF POINTER
        MOVE    D0,-(SP)                        ;PUSH PTCOUNT
        JSR     SORTPOINTS                      ;QUICKSORT IN VH ORDER
        MOVE.L  (A3),-(SP)                      ;PUSH BUF POINTER
        PEA     PTCOUNT(A6)                     ;PUSH VAR PTCOUNT
        JSR     CULLPOINTS                      ;CANCEL DUPLICATE PAIRS
        MOVE.L  A3,-(SP)                        ;PUSH RGNBUF HANDLE
        MOVE    PTCOUNT(A6),-(SP)               ;PUSH (UPDATED) PTCOUNT
        MOVE.L  DSTRGN(A6),-(SP)                ;PUSH DSTRGN
        JSR     PACKRGN                         ;PACK POINTS INTO DSTRGN
        MOVE.L  A3,A0                           ;GET RGNBUF HANDLE
        _DisposHandle                           ;DISCARD IT
DONE    MOVEM.L (SP)+,D3/A2-A4                  ;RESTORE REGS
        UNLINK  PARAMSIZE, 'CLOSERGN'



        .PROC CopyRgn,1
        .REF  SetSize
;---------------------------------------------------
;
;  PROCEDURE CopyRgn(* srcRgn,dstRgn: RgnHandle *);
;
PARAMSIZE       .EQU    8
SRCRGN          .EQU    PARAMSIZE+8-4           ;RGNHANDLE
DSTRGN          .EQU    SRCRGN-4                ;RGNHANDLE

        LINK    A6,#0                           ;ESTABLISH STACK FRAME
        MOVE.L  SRCRGN(A6),A0                   ;GET SRCRGN HANDLE
        MOVE.L  DSTRGN(A6),A1                   ;GET DSTRGN HANDLE
        CMP.L   A0,A1                           ;ARE THEY THE SAME?
        BEQ.S   DONE                            ;YES, QUIT

        MOVE.L  (A0),A0                         ;DE-REFERENCE SRCRGN HANDLE
        MOVE.L  (A1),A1                         ;DE-REFERENCE DSTRGN HANDLE
        MOVE    RGNSIZE(A0),D0                  ;GET SRC SIZE
        CMP     RGNSIZE(A1),D0                  ;IS DST SIZE SAME AS SRC ?
        BEQ.S   COPY                            ;YES, CONTINUE

        MOVEM.L D0/D3/A2,-(SP)                  ;SAVE REGS AND BYTECOUNT
        MOVE.L  DSTRGN(A6),-(SP)                ;PUSH DSTRGN HANDLE
        MOVE    D0,-(SP)                        ;PUSH NEWSIZE=SRC SIZE
        JSR     SETSIZE                         ;CHANGE SIZE OF DST
        MOVEM.L (SP)+,D0/D3/A2                  ;RESTORE REGS AND BYTECOUNT
        MOVE.L  SRCRGN(A6),A0                   ;GET SRCRGN HANDLE
        MOVE.L  DSTRGN(A6),A1                   ;GET DSTRGN HANDLE
        MOVE.L  (A0),A0                         ;DE-REFERENCE SRCRGN HANDLE
        MOVE.L  (A1),A1                         ;DE-REFERENCE DSTRGN HANDLE

COPY    LSR     #2,D0                           ;LONGCOUNT := BYTECOUNT DIV 4
        BCC.S   EVEN                            ;WAS THERE AN ODD WORD ?
        MOVE    (A0)+,(A1)+                     ;YES, DO IT TO MAKE EVEN
        BRA.S   EVEN                            ;AND CONTINUE
COPYLP  MOVE.L  (A0)+,(A1)+                     ;COPY A LONG OF SRC TO DST
EVEN    DBRA    D0,COPYLP                       ;LOOP FOR ALL LONGS

DONE    UNLINK  PARAMSIZE,'COPYRGN '



        .PROC SetEmptyRgn,1
        .REF  SetRectRgn
;---------------------------------------------------
;
;  PROCEDURE SetEmptyRgn(rgn: RgnHandle);
;
        MOVE.L  (SP)+,A0                        ;POP RETURN ADDR
        CLR.L   -(SP)                           ;PUSH LEFT, TOP
        CLR.L   -(SP)                           ;PUSH RIGHT,BOTTOM
        MOVE.L  A0,-(SP)                        ;RESTORE RETURN ADDR
        JMP     SETRECTRGN                      ;DOUBLE UP ON CODE



        .PROC SetRectRgn,5
        .REF  SetSize
;---------------------------------------------------
;
;  PROCEDURE SetRectRgn(rgn: RgnHandle; left,top,right,bottom: INTEGER);
;  make a rectangular region from 4 integers.
;
        LINK    A6,#0                           ;ESTABLISH STACK FRAME
        MOVEM.L D3/A2,-(SP)                     ;SAVE REGS
        MOVE.L  16(A6),A0                       ;GET RGN HANDLE
        MOVE.L  (A0),A1                         ;DE-REFERENCE HANDLE
        MOVEQ   #10,D0
        CMP     RGNSIZE(A1),D0                  ;IS RGNSIZE ALREADY 10 ?
        BEQ.S   SIZEOK                          ;YES, CONTINUE
        MOVE.L  A0,-(SP)                        ;PUSH RGNHANDLE
        MOVE    D0,-(SP)                        ;PUSH SIZE = 10 BYTES
        JSR     SETSIZE                         ;CHANGE SIZE OF REGION
        MOVE.L  16(A6),A0                       ;GET RGN HANDLE
        MOVE.L  (A0),A1                         ;DE-REFERENCE HANDLE
        MOVE    #10,RGNSIZE(A1)                 ;INSTALL SIZE = 10
SIZEOK  MOVE.L  12(A6),RGNBBOX+TOPLEFT(A1)      ;INSTALL RGNBBOX TOPLEFT
        MOVE.L  8(A6),RGNBBOX+BOTRIGHT(A1)      ;INSTALL RGNBBOX BOTRIGHT
        MOVE    RGNBBOX+LEFT(A1),D0
        CMP     RGNBBOX+RIGHT(A1),D0            ;IS LEFT >= RIGHT ?
        BGE.S   EMPTY                           ;YES, SET TO EMPTY

        MOVE    RGNBBOX+TOP(A1),D0
        CMP     RGNBBOX+BOTTOM(A1),D0           ;IS TOP < BOTTOM ?
        BLT.S   DONE                            ;YES, CONTINUE

EMPTY   CLR.L   RGNBBOX+TOPLEFT(A1)
        CLR.L   RGNBBOX+BOTRIGHT(A1)
DONE    MOVEM.L (SP)+,D3/A2                     ;RESTORE REGS
        UNLINK  12,'SETRECTR'



        .PROC RectRgn,2
        .REF  SetRectRgn
;---------------------------------------------------
;
;  PROCEDURE RectRgn(rgn: RgnHandle; r: Rect);
;  make a rectangular region from a rectangle
;
        MOVE.L  (SP)+,A0                        ;POP RETURN ADDR
        MOVE.L  (SP)+,A1                        ;POP ADDR OF RECT
        MOVE.L  (A1)+,-(SP)                     ;PUSH LEFT,TOP
        MOVE.L  (A1)+,-(SP)                     ;PUSH RIGHT,BOTTOM
        MOVE.L  A0,-(SP)                        ;RESTORE RETURN ADDR
        JMP     SETRECTRGN                      ;DOUBLE UP ON CODE



        .PROC OffsetRgn,3
;---------------------------------------------------------
;
;  PROCEDURE OffsetRgn(rgn: RgnHandle; dh,dv: INTEGER);
;
        MOVE.L  (SP)+,A1                        ;POP RETURN ADDR
        MOVE    (SP)+,D1                        ;POP DV
        MOVE    (SP)+,D0                        ;POP DH
        MOVE.L  (SP)+,A0                        ;POP RGN HANDLE
        MOVE.L  (A0),A0                         ;DE-REFERENCE IT
        ADD     #2,A0                           ;POINT TO RGNBBOX

;------------------------------------------------------
;
;  OFFSET THE BOUNDING BOX
;
        ADD     D1,(A0)+                        ;ADD DV TO TOP
        ADD     D0,(A0)+                        ;ADD DH TO LEFT
        ADD     D1,(A0)+                        ;ADD DV TO BOTTOM
        ADD     D0,(A0)+                        ;ADD DH TO RIGHT


;---------------------------------------------------------------------
;
;  IF NON-RECTANGULAR REGION, OFFSET THE COORDINATES TOO.
;
        CMP     #10,-10(A0)                     ;IS REGION RECTANGULAR ?
        BEQ.S   DONE                            ;IF SO, WE'RE ALL DONE
NXTVERT ADD     D1,(A0)+                        ;OFFSET VERTICAL COORD DV
NXTHOR  ADD     D0,(A0)+                        ;OFFSET LEFT COORD DH
        ADD     D0,(A0)+                        ;OFFSET RIGHT COORD DH
        CMP     #32767,(A0)                     ;HORIZ TERMINATOR ?
        BNE     NXTHOR                          ;NO, LOOP
        ADD     #2,A0                           ;YES, SKIP OVER TERMINATOR
        CMP     #32767,(A0)                     ;IS NEXT VERT = 32767 ?
        BNE     NXTVERT                         ;NO, LOOP FOR MORE
DONE    JMP     (A1)                            ;RETURN



        .PROC InsetRgn,3
        .REF  InsetRect,SortPoints
        .REF  NewHandle,RgnOp,PackRgn
;---------------------------------------------------------
;
;  PROCEDURE InsetRgn(rgn: RgnHandle; dh,dv: INTEGER);
;
;  Inset a region by (dh,dv). Outset if dh or dv neg
;
;  A6 OFFSETS OF PARAMS AFTER LINK:
;
PARAMSIZE       .EQU    8
RGN             .EQU    PARAMSIZE+8-4           ;LONG, RGNHANDLE
DH              .EQU    RGN-2                   ;WORD
DV              .EQU    DH-2                    ;WORD


        LINK    A6,#0                           ;NO LOCAL VARS
        MOVEM.L D3-D7/A2-A4,-(SP)               ;SAVE REGS
        MOVE.L  DV(A6),D6                       ;GET DV AND DH BOTH
        BEQ.S   JGOHOME                         ;QUIT IF BOTH ARE ZERO
        MOVE.L  RGN(A6),A4                      ;GET RGNHANDLE
        MOVE.L  (A4),A3                         ;DE-REFERENCE IT
        MOVE    (A3)+,D7                        ;GET RGNSIZE


;--------------------------------------------------------------
;
;  IF RECTANGULAR REGION, CALL INSETRECT AND CHECK FOR EMPTY.
;
        CMP     #10,D7                          ;IS RGN RECTANGULAR ?
        BNE.S   NOTRECT                         ;NO, CONTINUE
        MOVE.L  A3,-(SP)                        ;PUSH ADDR OF RGNBBOX
        MOVE.L  D6,-(SP)                        ;PUSH DH AND DV
        JSR     INSETRECT                       ;InsetRect(rgn^^.rgnbbox,dh,dv);
        MOVE    LEFT(A3),D0                     ;GET BBOX LEFT
        CMP     RIGHT(A3),D0                    ;IS LEFT >= RIGHT ?
        BGE.S   EMPTY                           ;YES, RETURN EMPTY RGN
        MOVE    TOP(A3),D0                      ;GET BBOX TOP
        CMP     BOTTOM(A3),D0                   ;IS TOP >= BOTTOM ?
        BLT.S   JGOHOME                         ;NO, CONTINUE
EMPTY   CLR.L   (A3)+                           ;SET RGNBBOX TO (0,0,0,0) TO
        CLR.L   (A3)+                           ;RETURN EMPTY REGION
JGOHOME BRA.S   GOHOME                          ;ALL DONE


;-----------------------------------------------------
;
;  THE REGION IS NOT RECTANGULAR. ALLOCATE A POINT BUFFER IN THE HEAP.
;
NOTRECT ADD     D7,D7                           ;TRY BYTES NEEDED = RGNSIZE*2
        CLR.L   -(SP)                           ;ROOM FOR FCN RESULT
        MOVE    D7,-(SP)                        ;PUSH BYTESNEEDED
        JSR     NEWHANDLE                       ;ALLOCATE PTBUF
        MOVE.L  (SP)+,A3                        ;PUT PTBUF HANDLE IN A3
        BSR.S   HINSET                          ;INSET HORIZ AND FLIP
        SWAP    D6                              ;GET DV INSTEAD OF DH
        BSR.S   HINSET                          ;INSET VERTICAL AND FLIP BACK
        BRA.S   DONE                            ;ALL DONE


;--------------------------------------------------------------------------
;
;  LOCAL ROUTINE TO INSET HORIZONTALLY, SWAP H AND V COORDS, AND RE-SORT
;
;  RgnOp(rgn,rgn,bufHandle,maxBytes,op,dh,TRUE);
;
HINSET  CLR.W   -(SP)                           ;ROOM FOR FCN RESULT
        MOVE.L  A4,-(SP)                        ;PUSH RGNA = USER RGN
        MOVE.L  A4,-(SP)                        ;PUSG RGNB = USER RGN ALSO
        MOVE.L  A3,-(SP)                        ;PUSH BUFHANDLE
        MOVE    D7,-(SP)                        ;PUSH MAXBYTES
        MOVE    #8,-(SP)                        ;PUSH OP = INSET
        MOVE    D6,-(SP)                        ;PUSH DH
        ST      -(SP)                           ;PUSH OKGROW = TRUE
        JSR     RGNOP
        MOVE.W  (SP)+,D5                        ;GET PTCOUNT IN D5
;
;  SWAP VERT AND HORIZ COORDS OF ALL INVERSION POINTS
;
        MOVE.L  (A3),A0                         ;DE-REFERENCE PTBUF HANDLE
        MOVE    D5,D1                           ;COPY PTCOUNT
        BRA.S   SWAP2                           ;GO TO LOOP START
SWAPLP  MOVE.L  (A0),D0                         ;GET A POINT
        SWAP    D0                              ;SWAP ITS COORDINATES
        MOVE.L  D0,(A0)+                        ;PUT IT BACK INTO ARRAY
SWAP2   DBRA    D1,SWAPLP                       ;LOOP FOR ALL POINTS
;
;  RE-SORT THE POINTS IN V.H ORDER
;
        MOVE.L  (A3),-(SP)                      ;PUSH PTBUF PTR
        MOVE    D5,-(SP)                        ;PUSH PTCOUNT
        JSR     SORTPOINTS                      ;RE-SORT INTO VH ORDER
;
;  PACK THE RESULTING POINTS
;
        MOVE.L  A3,-(SP)                        ;PUSH PTBUF HANDLE
        MOVE    D5,-(SP)                        ;PUSH PTCOUNT
        MOVE.L  A4,-(SP)                        ;PUSH RGN HANDLE
        JSR     PACKRGN                         ;PackRgn(ptBuf,ptCount,rgn);
        RTS                                     ;RETURN TO LOCAL CALLER


;----------------------------------------------
;
;  DISCARD POINT BUFFER AND QUIT.
;
DONE    MOVE.L  A3,A0                           ;GET PTBUF HANDLE
        _DisposHandle                           ;DISCARD IT

GOHOME  MOVEM.L (SP)+,D3-D7/A2-A4               ;RESTORE REGS
        UNLINK  PARAMSIZE,'INSETRGN'



        .FUNC EmptyRgn,1
        .REF  EmptyRect
;---------------------------------------------------
;
;  FUNCTION EmptyRgn(rgn: RgnHandle): BOOLEAN;
;
        MOVE.L  (SP)+,A0                        ;POP RETURN ADDR
        MOVE.L  (SP)+,A1                        ;POP RGNHANDLE
        MOVE.L  (A1),A1                         ;DE-REFERENCE HANDLE
        PEA     RGNBBOX(A1)                     ;PUSH RGNBBOX
        MOVE.L  A0,-(SP)                        ;PUSH RETURN ADDR
        JMP     EMPTYRECT                       ;USE EMPTYRECT CODE



        .FUNC EqualRgn,2
;-------------------------------------------------------
;
;  FUNCTION EqualRgn(rgnA,rgnB: RgnHandle): BOOLEAN;
;
;  RETURNS TRUE IF RGNA AND RGNB DESCRIBE THE SAME AREA
;
        MOVE.L  4(SP),A0                ;GET RGNA HANDLE
        MOVE.L  8(SP),A1                ;GET RGNB HANDLE
        CMP.L   A0,A1                   ;ARE THEY THE SAME ?
        BEQ.S   TRUE                    ;YES, THE REGIONS ARE THE SAME
        MOVE.L  (A0),A0                 ;DE-REFERENCE RGN A
        MOVE.L  (A1),A1                 ;DE-REFERENCE RGN B
        MOVE    (A0),D0                 ;GET RGNSIZE A
        MOVE    D0,D1                   ;MAKE AN EXTRA COPY
        LSR     #2,D1                   ;DIV BY 4 FOR LONG COUNT
        SUB     #1,D1                   ;INIT DBRA LOOP COUNT
LOOP    CMPM.L  (A0)+,(A1)+             ;COMPARE A LONG
        DBNE    D1,LOOP                 ;LOOK TILL DIFFERENT OR COUNT
        BNE.S   FALSE                   ;DIFFERENT --> FALSE
        AND     #3,D0                   ;GET 0..3 FINISH UP BYTES
        BEQ.S   TRUE                    ;IF NO MORE, WE MADE IT
LOOP2   CMPM.B  (A0)+,(A1)+             ;COMPARE A BYTE
        BNE.S   FALSE                   ;BR IF DIFFERENT
        SUB     #1,D0
        BNE.S   LOOP2                   ;LOOP LAST 1..3 BYTES
TRUE    MOVE.L  #1,D0                   ;TRUE
        BRA.S   DONE
FALSE   MOVE.L  #0,D0                   ;FALSE
DONE    MOVE.B  D0,12(SP)               ;SET RESULT
        MOVE.L  (SP)+,A0                ;POP RETURN ADDR
        ADD     #8,SP                   ;STRIP PARAMETERS
        JMP     (A0)                    ;RETURN



        .PROC SectRgn,3
        .DEF  DoRgnOp,UnionRgn,DiffRgn,XorRgn
        .REF  EqualRgn,CopyRgn,RSect,RectRgn,SetEmptyRgn
        .REF  NewHandle,RgnOp,PackRgn
;---------------------------------------------------------
;
;  PROCEDURE SectRgn(srcRgnA,srcRgnB,dstRgn: RgnHandle);
;  calculate the intersection of two regions.
;
        MOVEQ   #0,D0                           ;OP = SECT
        BRA.S   DoRgnOp                         ;SHARE COMMON CODE



;---------------------------------------------------------
;
;  PROCEDURE UnionRgn(srcRgnA,srcRgnB,dstRgn: RgnHandle);
;  calculate the union of two regions.
;
UnionRgn
        MOVEQ   #4,D0                           ;OP = UNION
        BRA.S   DoRgnOp                         ;SHARE COMMON CODE



;---------------------------------------------------------
;
;  PROCEDURE DiffRgn(srcRgnA,srcRgnB,dstRgn: RgnHandle);
;  calculate the difference A-B of two regions
;
DiffRgn MOVEQ   #2,D0                           ;OP = DIFF
        BRA.S   DoRgnOp                         ;SHARE COMMON CODE



;---------------------------------------------------------
;
;  PROCEDURE XorRgn(srcRgnA,srcRgnB,dstRgn: RgnHandle);
;  calculate the exclusive or of two regions
;
XorRgn  MOVEQ   #6,D0                           ;OP = DIFF
        BRA.S   DoRgnOp                         ;SHARE COMMON CODE



;---------------------------------------------------------
;
;  PROCEDURE DoRgnOp(srcRgnA,srcRgnB,dstRgn: RgnHandle);
;
;  Computes the Intersection, Difference, Union, or Xor of two regions.
;
;  enter with op in D0.
;  op = 0: SECT
;       2: DIFF A-B
;       4: UNION
;       6: XOR
;
;
;  A6 OFFSETS OF PARAMS AND LOCALS AFTER LINK:
;
PARAMSIZE       .EQU    12
RGNA            .EQU    PARAMSIZE+8-4           ;LONG, RGNHANDLE
RGNB            .EQU    RGNA-4                  ;LONG, RGNHANDLE
DSTRGN          .EQU    RGNB-4                  ;LONG, RGNHANDLE
;
TEMPRECT        .EQU    -8                      ;RECT
VARSIZE         .EQU    TEMPRECT                ;TOTAL LOCALS
;
DoRgnOp
        LINK    A6,#VARSIZE                     ;ALLOCATE STACK FRAME
        MOVEM.L D3-D7/A2-A4,-(SP)               ;SAVE REGS
        MOVE    D0,D5                           ;COPY OP INTO D5
        MOVE.L  RGNA(A6),A2                     ;GET RGNA
        MOVE.L  RGNB(A6),A3                     ;GET RGNB
        MOVE.L  DSTRGN(A6),A4                   ;GET DSTRGN
        MOVE.L  #2,D7
;
;  ARE THE TWO INPUT REGIONS THE SAME ?
;
        CLR.B   -(SP)                           ;MAKE ROOM FOR FCN RESULT
        MOVE.L  A2,-(SP)                        ;PUSH RGNA
        MOVE.L  A3,-(SP)                        ;PUSH RGNB
        JSR     EQUALRGN                        ;CALL EQUALRGN
        TST.B   (SP)+                           ;ARE THEY THE SAME ?
        BEQ.S   NOTSAME                         ;NO, CONTINUE
;
;  THE TWO REGIONS ARE THE SAME.  IF SECT OR UNION
;  THEN COPY RGNA INTO DSTRGN, ELSE ZERO OUT DSTRGN.
;
        AND     D7,D5                           ;WAS OP SECT OR UNION ?
        BNE.S   ZERO                            ;NO, ZERO OUT DSTRGN
COPY    MOVE.L  A2,-(SP)                        ;PUSH RGNA
        MOVE.L  A4,-(SP)                        ;PUSH DSTRGN
        JSR     COPYRGN                         ;COPY RGNA INTO DSTRGN
        BRA.S   JDONE                           ;AND QUIT
ZERO    MOVE.L  A4,-(SP)                        ;PUSH DSTRGN
        JSR     SetEmptyRgn                     ;SET IT TO EMPTY
        BRA.S   JDONE                           ;AND QUIT

;
;  IF OP = DIFF AND RGNB = EMPTY,  COPY RGNA INTO DST
;
NOTSAME MOVE.L  (A2),A0                         ;DE-REFERENCE RGNA
        MOVE.L  (A3),A1                         ;DE-REFERENCE RGNB
        CMP     D7,D5                           ;IS OP = DIFF ?
        BGT.S   UNIXOR                          ;NO, ITS UNION OR XOR
        BLT.S   BBOXES                          ;NO, IT'S SECT
        MOVE    RGNBBOX+LEFT(A1),D0             ;GET BBOX LEFT
        CMP     RGNBBOX+RIGHT(A1),D0            ;IS BBOX LEFT >= RIGHT ?
        BGE     COPY                            ;YES, COPY RGNA INTO DST

;
;  IF op = SECT OR DIFF, THEN INTERSECT THE BOUNDING BOXES.
;
BBOXES  PEA     RGNBBOX(A0)                     ;PUSH RGNA^^.RGNBBOX
        PEA     RGNBBOX(A1)                     ;PUSH RGNB^^.RGNBBOX
        MOVE    D7,-(SP)                        ;PUSH NRECTS = 2
        PEA     TEMPRECT(A6)                    ;PUSH DST = TEMPRECT
        JSR     RSECT                           ;CALC INTERSECTION
        BNE.S   NOTEMPTY                        ;BR IF RESULT NOT EMPTY
;
;  THE BOUNDING BOXES DON'T INTERSECT.
;  IF OP = SECT, THEN RETURN EMPTY.
;  IF OP = DIFF, THEN COPY RGNA INTO DSTRGN.
;
        TST     D5                              ;IS OP = SECT ?
        BEQ     ZERO                            ;YES, RETURN EMPTY
        BRA     COPY                            ;NO, COPY SRCA INTO DSTRGN

;
;  IF OP = SECT, THEN CHECK FOR BOTH INPUTS RECTANGULAR
;
NOTEMPTY MOVE.L (A2),A0                         ;DE-REFERENCE RGNA
        MOVE.L  (A3),A1                         ;DE-REFERENCE RGNB
        TST     D5                              ;IS OP = SECT ?
        BNE.S   NOTEASY                         ;NO, CONTINUE
        MOVEQ   #10,D0
        CMP     RGNSIZE(A0),D0                  ;IS RGNA RECTANGULAR ?
        BNE.S   NOTEASY                         ;NO, CONTINUE
        CMP     RGNSIZE(A1),D0                  ;IS RGNB RECTANGULAR ?
        BNE.S   NOTEASY                         ;NO, CONTINUE
        MOVE.L  A4,-(SP)                        ;PUSH DSTRGN
        PEA     TEMPRECT(A6)                    ;PUSH TEMPRECT
        JSR     RECTRGN                         ;RectRgn(dstRgn,tempRect);
JDONE   BRA.S   DONE

;
;  OP = UNION OR XOR:  IF EITHER REGION IS EMPTY, COPY THE OTHER.
;
UNIXOR  MOVE    RGNBBOX+LEFT(A1),D0             ;GET RGNB BBOX LEFT
        CMP     RGNBBOX+RIGHT(A1),D0            ;IS RGNB BBOX LEFT >= RIGHT ?
        BGE     COPY                            ;YES, COPY RGNA INTO DST

        MOVE    RGNBBOX+LEFT(A0),D0             ;GET RGNA BBOX LEFT
        CMP     RGNBBOX+RIGHT(A0),D0            ;IS RGNA BBOX LEFT >= RIGHT ?
        BLT.S   NOTEASY                         ;NO, CONTINUE
        MOVE.L  A3,A2                           ;YES, GET RGNB INSTEAD
        BRA     COPY                            ;COPY RGNB INTO DST


NOTEASY MOVE    RGNSIZE(A0),D4                  ;GET RGNA RGNSIZE
        ADD     RGNSIZE(A1),D4                  ;ADD RGNB RGNSIZE
        ADD     D4,D4                           ;TRY DOUBLE FOR BYTECOUNT
        CLR.L   -(SP)                           ;MAKE ROOM FOR FCN RESULT
        MOVE    D4,-(SP)                        ;PUSH BYTECOUNT
        JSR     NEWHANDLE                       ;ALLOCATE PTBUF
        MOVE.L  (SP)+,A3                        ;GET PTBUF HANDLE IN A3
;
;  PtCount := RgnOp(srcA,srcB,bufHandle,maxBytes,op,0,TRUE);
;
        CLR.W   -(SP)                           ;MAKE ROOM FOR FCN RESULT
        MOVE.L  RGNA(A6),-(SP)                  ;PUSH RGNA
        MOVE.L  RGNB(A6),-(SP)                  ;PUSH RGNB
        MOVE.L  A3,-(SP)                        ;PUSH BUFHANDLE
        MOVE    D4,-(SP)                        ;PUSH MAXBYTES
        MOVE    D5,-(SP)                        ;PUSH OP
        CLR.W   -(SP)                           ;PUSH DH=0
        ST      -(SP)                           ;PUSH OKGROW = TRUE
        JSR     RGNOP
        MOVE    (SP)+,D6                        ;GET PTCOUNT

        MOVE.L  A3,-(SP)                        ;PUSH PTBUF HANDLE
        MOVE    D6,-(SP)                        ;PUSH PTCOUNT
        MOVE.L  A4,-(SP)                        ;PUSH DSTRGN
        JSR     PACKRGN                         ;PackRgn(ptBuf,ptCount,dstRgn);

        MOVE.L  A3,A0                           ;GET PTBUF HANDLE
        _DisposHandle                           ;DISCARD IT

DONE    MOVEM.L (SP)+,D3-D7/A2-A4               ;RESTORE REGS
        UNLINK  PARAMSIZE,'DORGNOP '



        .FUNC PtInRgn,2
;------------------------------------------------------------
;
;  FUNCTION PtInRgn(pt: Point; rgn: RgnHandle): BOOLEAN;
;
;  TESTS IF A GIVEN POINT IS INSIDE A REGION.
;
;  A6 OFFSETS OF PARAMETERS AFTER LINK:
;
PARAMSIZE       .EQU    8                       ;SIZE OF PARAMETERS
RESULT          .EQU    PARAMSIZE+8             ;BOOLEAN
PT              .EQU    RESULT-4                ;POINT, VALUE
RGN             .EQU    PT-4                    ;LONG, HANDLE


ENTRY   LINK    A6,#0                           ;NO LOCAL VARS
        MOVE.L  D3,-(SP)                        ;SAVE REG
        MOVE    PT+H(A6),D1                     ;GET TEST HORIZ
        MOVE    PT+V(A6),D2                     ;GET TEST VERT
        CLR     D3                              ;INIT INSIDE:=FALSE


;-----------------------------------------------------------
;
;  FIRST CHECK BOUNDING BOX
;
        MOVE.L  RGN(A6),A0                      ;GET RGN HANDLE
        MOVE.L  (A0),A0                         ;DE-REFERENCE IT
        CMP     RGNBBOX+LEFT(A0),D1             ;IS PT.H < BBOX LEFT ?
        BLT.S   DONE                            ;YES, RETURN FALSE
        CMP     RGNBBOX+RIGHT(A0),D1            ;IS PT.H >= BBOX RIGHT ?
        BGE.S   DONE                            ;YES, RETURN FALSE
        CMP     RGNBBOX+TOP(A0),D2              ;IS PT.V < BBOX TOP ?
        BLT.S   DONE                            ;YES, RETURN FALSE
        CMP     RGNBBOX+BOTTOM(A0),D2           ;IS PT.V >= BBOX BOT ?
        BGE.S   DONE                            ;YES, RETURN FALSE
        CMP     #10,RGNSIZE(A0)                 ;IS REGION RECTANGULAR ?
        BNE.S   NOTRECT                         ;NO, CONTINUE
        NOT     D3                              ;YES, RETURN TRUE
        BRA.S   DONE


;------------------------------------------------------------------
;
;  PT IS INSIDE BOUNDING BOX AND REGION IS NOT RECTANGULAR.
;  LOOK AT THE INVERSION POINTS TO DETERMINE IF PT IN REGION.
;
NOTRECT LEA     RGNDATA(A0),A0                  ;POINT TO FIRST VERT COORD
NXTVERT CMP     (A0)+,D2                        ;IS NEXT VERT > PT.V ?
        BLT.S   DONE                            ;YES, QUIT
NEXTHOR MOVE    (A0)+,D0                        ;GET HORIZ COORD
        CMP     #32767,D0                       ;IS IT THE TERMINATOR ?
        BEQ     NXTVERT                         ;YES, GET NEXT VERT COORD
        CMP     D1,D0                           ;IS HORIZ <= PT.H ?
        BGT     NEXTHOR                         ;NO, IGNORE THIS POINT
        NOT     D3                              ;YES, TOGGLE INSIDE
        BRA     NEXTHOR                         ;AND GO FOR MORE POINTS
DONE    NEG.B   D3                              ;BOOLEAN RESULT IS 0 OR 1
        MOVE.B  D3,RESULT(A6)                   ;RETURN BOOLEAN FCN RESULT
        MOVE.L  (SP)+,D3                        ;RESTORE REG
        UNLINK  PARAMSIZE,'PTINRGN '




        .FUNC RectInRgn,2
        .REF  RSect,InitRgn,SeekRgn
;--------------------------------------------------
;
;  FUNCTION RectInRgn(r: Rect; rgn: RgnHandle): BOOLEAN;
;
;  Returns TRUE if any part of the rectangle intersects the region.
;
;
;  A6 OFFSETS OF PARAMETERS AFTER LINK:
;
PARAMSIZE       .EQU    8                       ;TOTAL SIZE OF PARAMETERS
RESULT          .EQU    PARAMSIZE+8             ;BYTE, BOOLEAN
RECT            .EQU    RESULT-4                ;LONG, VAR ADDR
RGN             .EQU    RECT-4                  ;LONG, RGNHANDLE


;------------------------------------------------------
;
;  A6 OFFSETS OF LOCAL VARIABLES AFTER LINK:
;
MINRECT         .EQU    -8                      ;RECTANGLE
SAVESTACK       .EQU    MINRECT-4               ;LONG
STATE           .EQU    SAVESTACK-RGNREC        ;REGION STATE RECORD
VARSIZE         .EQU    STATE                   ;TOTAL SIZE OF VARIABLES


        LINK    A6,#VARSIZE                     ;ALLOCATE LOCAL VARIABLES
        MOVEM.L D0-D7/A1-A5,-(SP)               ;SAVE REGISTERS
        MOVE.L  SP,SAVESTACK(A6)                ;REMEMBER STACK START
        CLR.B   RESULT(A6)                      ;INIT BOOLEAN RESULT TO FALSE
        MOVE.L  RGN(A6),A1                      ;GET REGION HANDLE
        MOVE.L  (A1),A1                         ;DE-REFERENCE IT


;--------------------------------------------------------------
;
;  FIRST CHECK IF RECTANGLE INTERSECTS BOUNDING BOX OF REGION
;
        MOVE.L  RECT(A6),-(SP)                  ;PUSH POINTER TO RECT
        PEA     RGNBBOX(A1)                     ;PUSH POINTER TO RGN BBOX
        MOVE    #2,-(SP)                        ;PUSH NRECTS=2
        PEA     MINRECT(A6)                     ;PUSH ADDR WHERE TO PUT RESULT
        JSR     RSECT                           ;CALC INTERSECTION
        BEQ.S   GOHOME                          ;QUIT IF NO INTERSECTION
        CMP     #10,RGNSIZE(A1)                 ;IS REGION RECTANGULAR ?
        BEQ.S   TRUE                            ;YES, RETURN TRUE


;------------------------------------------------------------
;
;  THE REGION IS NON-RECTANGULAR AND THE RECTANGLE INTERSECTS
;  THE REGION'S BOUNDING BOX. WE WILL PLAY BACK THE PORTION OF
;  THE REGION WITHIN MINRECT AND SEE IF ANY PART OF THE REGION
;  IS ACTUALLY INSIDE THE RECTANGLE.
;


;-------------------------------------------------
;
;  INITIALIZE RGN STATE RECORD AT TOP.
;
        MOVE.L  A1,A0                           ;GET RGNPTR IN A0
        LEA     STATE(A6),A1                    ;GET STATE RECORD IN A1
        MOVE    MINRECT+LEFT(A6),D0             ;MINH IN D0
        MOVE    MINRECT+RIGHT(A6),D1            ;MAXH IN D1
        MOVE    D0,D2                           ;BUFLEFT:=MINH
        JSR     INITRGN                         ;INIT RECORD, ALLOCATE BUFFER


;------------------------------------------------------------
;
;  PLAY THE REGION BACK INTO SCAN BUFFER UNTIL IT GETS DOWN TO
;  MINRECT, THEN CHECK EACH SCANLINE FOR NON-ZERO PLAYBACK.
;  QUIT AND RETURN TRUE IF NON-ZERO FOUND BEFORE RGN GOES BEYOND MINRECT.
;
        MOVE    SCANSIZE(A1),D5                 ;GET BUFSIZE= # LONGS -1
        MOVE    MINRECT+TOP(A6),D0
        JSR     SEEKRGN                         ;SEEK THE REGION TO MINRECT TOP
        MOVE    MINRECT+BOTTOM(A6),D6
TESTBUF MOVE.L  SCANBUF(A1),A0                  ;POINT TO BUFFER START
        MOVE    D5,D0                           ;INIT LOOP COUNT TO BUFSIZE
NXTLONG TST.L   (A0)+                           ;IS SCAN BUF NON-ZERO ?
        DBNE    D0,NXTLONG                      ;TEST LONGS TILL NON ZERO OR END
        BNE.S   TRUE                            ;WE FOUND A NON-ZERO, RETURN TRUE
        MOVE    NEXTV(A1),D0                    ;GET NEXT VERTICAL IN RGN
        CMP     D0,D6                           ;IS NEXT RGN VERT BEYOND BOTTOM ?
        BLE.S   GOHOME                          ;YES, RETURN FALSE
        JSR     SEEKRGN                         ;NO, SEEK TO NEXT VERT CHANGE
        BRA.S   TESTBUF                         ;AND SEE IF IT IS NON-ZERO

TRUE    ADDQ.B  #1,RESULT(A6)                   ;SET BOOLEAN RESULT TO TRUE
GOHOME  MOVE.L  SAVESTACK(A6),SP                ;STRIP SCAN BUFFER IF ANY
        MOVEM.L (SP)+,D0-D7/A1-A5               ;RESTORE REGISTERS
        UNLINK  PARAMSIZE,'RECTINRG'



        .FUNC TrimRect,2
        .REF  RgnOp
;------------------------------------------------------
;
;  FUNCTION TrimRect(rgn: RgnHandle; VAR dstRect: Rect): CCR TRISTATE;
;
;  RESULT IN CONDITION CODES:
;
;  =  RESULT RECTANGULAR, DSTRECT TRIMMED
;  <  RESULT EMPTY, DSTRECT NOT MODIFIED
;  >  RESULT NON-RECT, DSTRECT NOT MODIFIED
;
;  If the intersection of rgn and dstRect is rectangular,
;  then return EQUAL and put the intersection into dstRect.
;  If the intersection is empty or not rectangular, then
;  return FALSE and don't modify dstRect.
;
;   Does not call the storage allocator.
;
;   1.  Fake up a rect rgn on the stack from dstRect
;   2.  Call RgnOp with max bytes = 24, OKGROW = FALSE
;   3a. If ptCount = 4 THEN result rect, return TRUE and update dstRect.
;   3b. If ptCount < 4 THEN result empty, return TRUE and clear dstRect.
;   3c. If ptCount > 4 THEN result not rect, return FALSE
;
PARAMSIZE       .EQU    8
RGN             .EQU    PARAMSIZE+8-4           ;LONG, RGNHANDLE
DSTRECT         .EQU    RGN-4                   ;LONG, ADDR OF DSTRECT

PTDATA          .EQU    -24                     ;ROOM FOR 6 POINTS
PTMASTER        .EQU    PTDATA-4                ;LONG, FAKE MASTER
REGION          .EQU    PTMASTER-10             ;ROOM FOR RECT RGN DATA
RGNMASTER       .EQU    REGION-4                ;LONG
VARSIZE         .EQU    RGNMASTER

        LINK    A6,#VARSIZE                     ;ALLOCATE STACK FRAME
        MOVEM.L D0-D2/A1,-(SP)                  ;SAVE ALL REGS USED
        LEA     PTDATA(A6),A0                   ;POINT TO BUFFER
        MOVE.L  A0,PTMASTER(A6)                 ;INSTALL INTO FAKE MASTER
        LEA     REGION(A6),A1                   ;POINT TO REGION DATA
        MOVE.L  A1,RGNMASTER(A6)                ;INSTALL INTO FAKE MASTER
        MOVE    #10,(A1)+                       ;INSTALL RGNSIZE = 10
        MOVE.L  DSTRECT(A6),A0                  ;POINT TO DSTRECT
        MOVE.L  (A0)+,(A1)+                     ;COPY DSTRECT TOPLEFT
        MOVE.L  (A0)+,(A1)+                     ;COPY DSTRECT BOTRIGHT
;
;  RgnOp(rgn,rectRgn,BufHandle,24,sect,0,FALSE)
;
        CLR.W   -(SP)                           ;ROOM FOR FCN RESULT
        MOVE.L  RGN(A6),-(SP)                   ;PUSH RGN
        PEA     RGNMASTER(A6)                   ;PUSH FAKE RGNHANDLE
        PEA     PTMASTER(A6)                    ;PUSH BUFHANDLE
        MOVE    #24,-(SP)                       ;PUSH MAXBYTES = 24
        CLR.L   -(SP)                           ;PUSH OP = SECT, DH = 0
        CLR.B   -(SP)                           ;PUSH OKGROW = FALSE
        JSR     RGNOP                           ;RgnOp(rgn,rectRgn,buf,64,op,dh,FALSE);
        MOVE.L  DSTRECT(A6),A0                  ;POINT TO DSTRECT
        CMP     #4,(SP)+                        ;IS PT COUNT = 4 ?
        BNE.S   DONE                            ;NO, RETURN < OR > IN CCR
        MOVE.L  PTDATA(A6),(A0)+                ;UPDATE DSTRECT.TOPLEFT
        MOVE.L  PTDATA+12(A6),(A0)+             ;UPDATE DSTRECT.BOTRIGHT
        SUB     D0,D0                           ;SET EQUAL FLAG
DONE    MOVEM.L (SP)+,D0-D2/A1                  ;RESTORE ALL REGS
        UNLK    A6                              ;RELEASE STACK FRAME
        MOVE.L  (SP)+,A0                        ;POP RETURN ADDR INTO A0
        ADD     #PARAMSIZE,SP                   ;STRIP PARAMETERS
        JMP     (A0)                            ;JUMP THRU A0 TO RETURN



        .PROC MapRgn,3
        .REF  MapRect,NewHandle,PutRgn,MapPt
        .REF  SortPoints,CullPoints,PackRgn
;-------------------------------------------------------------
;
;  PROCEDURE MapRgn(rgn: RgnHandle; fromRect,toRect: Rect);
;
;  A6 OFFSETS OF PARAMETERS AND LOCALS AFTER LINK:
;
PARAMSIZE       .EQU    12
RGN             .EQU    PARAMSIZE+8-4           ;LONG, RGNHANDLE
FROMRECT        .EQU    RGN-4                   ;LONG, ADDR OF RECT
TORECT          .EQU    FROMRECT-4              ;LONG, ADDR OF RECT

INDEX           .EQU    -2                      ;INTEGER
SIZE            .EQU    INDEX-2                 ;WORD
PTCOUNT         .EQU    SIZE-2                  ;WORD
VARSIZE         .EQU    PTCOUNT                 ;TOTAL BYTES


        LINK    A6,#VARSIZE                     ;ALLOCATE STACK FRAME
        MOVEM.L D3/D6-D7/A2-A4,-(SP)            ;SAVE REGS
;
; QUIT FAST IF FROMRECT = TORECT
;
        MOVE.L  FROMRECT(A6),A0                 ;POINT TO FROMRECT
        MOVE.L  TORECT(A6),A1                   ;POINT TO TORECT
        CMPM.L  (A0)+,(A1)+                     ;IS TOPLEFT SAME ?
        BNE.S   NOTSAME                         ;NO, CONTINUE
        CMPM.L  (A0)+,(A1)+                     ;YES, IS BOTRIGHT SAME TOO ?
        BEQ.S   JDONE                           ;IF SO, JUST QUIT
;
;  SPECIAL CASE RECTANGULAR RGN
;
NOTSAME MOVE.L  RGN(A6),A4                      ;GET RGNHANDLE
        MOVE.L  (A4),A0                         ;DE-REFERENCE RGN
        CMP     #10,RGNSIZE(A0)                 ;IS RGN RECTANGULAR ?
        BNE.S   NOTRECT                         ;NO, CONTINUE
        PEA     RGNBBOX(A0)                     ;YES, PUSH RGNBBOX
        MOVE.L  FROMRECT(A6),-(SP)              ;PUSH FROMRECT
        MOVE.L  TORECT(A6),-(SP)                ;PUSH TO RECT
        JSR     MAPRECT                         ;MapRect(rgn^^.rgnBBox,from,to);
JDONE   BRA.S   DONE

NOTRECT CLR.L   -(SP)                           ;ROOM FOR FCN RESULT
        MOVE    #256,-(SP)                      ;PUSH BYTECOUNT = 256
        MOVE    (SP),SIZE(A6)                   ;SIZE := 256 BYTES
        JSR     NEWHANDLE                       ;ALLOCATE PTBUF
        MOVE.L  (SP)+,A3                        ;GET PTBUF HANDLE IN A3

        CLR     INDEX(A6)                       ;INDEX := 0
        MOVE.L  A4,-(SP)                        ;PUSH RGN
        MOVE.L  A3,-(SP)                        ;PUSH PTBUF HANDLE
        PEA     INDEX(A6)                       ;PUSH VAR INDEX
        PEA     SIZE(A6)                        ;PUSH VAR SIZE
        JSR     PUTRGN                          ;UNPACK RGN INTO INVERSION PTS
        MOVE    INDEX(A6),D7                    ;GET INDEX
        LSR     #2,D7                           ;PTCOUNT := INDEX DIV 4
;
;  MAP ALL INVERSION POINTS
;
        MOVE    D7,D6                           ;COPY PTCOUNT FOR LOOP COUNT
        MOVE.L  (A3),A2                         ;DE-REFERENCE PTBUF HANDLE
        BRA.S   MORE                            ;GO TO LOOP START
LOOP    MOVE.L  A2,-(SP)                        ;PUSH ADDR OF AN INV PT
        MOVE.L  FROMRECT(A6),-(SP)              ;PUSH FROMRECT
        MOVE.L  TORECT(A6),-(SP)                ;PUSH TORECT
        JSR     MAPPT                           ;MAP THIS POINT
        ADD.L   #4,A2                           ;BUMP TO NEXT POINT
MORE    DBRA    D6,LOOP                         ;LOOP ALL INVERSION POINTS

        MOVE.L  (A3),-(SP)                      ;PUSH PTBUF PTR
        MOVE    D7,-(SP)                        ;PUSH PTCOUNT
        JSR     SORTPOINTS                      ;SortPoints(ptBuf^,ptCount)

        MOVE.L  (A3),-(SP)                      ;PUSH PTBUF PTR
        MOVE    D7,PTCOUNT(A6)                  ;PUT PTCOUNT IN MEMORY
        PEA     PTCOUNT(A6)                     ;PUSH VAR PTCOUNT
        JSR     CULLPOINTS                      ;CullPoints(ptBuf^,ptCount)

        MOVE.L  A3,-(SP)                        ;PUSH PTBUF HANDLE
        MOVE    PTCOUNT(A6),-(SP)               ;PUSH PTCOUNT
        MOVE.L  A4,-(SP)                        ;PUSH RGN
        JSR     PACKRGN                         ;PackRgn(ptBuf,ptCount,rgn);

        MOVE.L  A3,A0                           ;PUSH PTBUF HANDLE
        _DisposHandle                           ;DISCARD IT

DONE    MOVEM.L (SP)+,D3/D6-D7/A2-A4            ;RESTORE REGS
        UNLINK  PARAMSIZE,'MAPRGN  '




        .END
