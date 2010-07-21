        .INCLUDE GRAFTYPES.TEXT
;--------------------------------------------------------------
;
;
;     ****    ***   *    *   *   ***    ***   *   *   ***
;     *   *  *   *  *     * *   *   *  *   *  *   *  *   *
;     *   *  *   *  *      *    *      *   *  **  *  *
;     ****   *   *  *      *    *  **  *   *  * * *   ***
;     *      *   *  *      *    *   *  *   *  *  **      *
;     *      *   *  *      *    *   *  *   *  *   *  *   *
;     *       ***   *****  *     ***    ***   *   *   ***
;
;

        .PROC   StdPoly,2
        .REF    CheckPic,PutPicVerb,DPutPicByte,PutPicRgn
        .REF    PushVerb,FrPoly,RSect,DrawPoly
;---------------------------------------------------------------
;
;  PROCEDURE StdPoly(verb: GrafVerb; poly: PolyHandle);
;
;  A6 OFFSETS OF PARAMS AND LOCALS AFTER LINK:
;
PARAMSIZE       .EQU    6
VERB            .EQU    PARAMSIZE+8-2           ;GRAFVERB
POLY            .EQU    VERB-4                  ;LONG, PolyHandle

MINRECT         .EQU    -8                      ;RECT
VARSIZE         .EQU    MINRECT                 ;TOTAL BYTES OF LOCALS


        LINK    A6,#VARSIZE                     ;ALLOCATE STACK FRAME
        MOVEM.L D5-D7/A2-A4,-(SP)               ;SAVE REGS
        MOVE.B  VERB(A6),D7                     ;GET VERB
        JSR     CHECKPIC                        ;SET UP A4,A3 AND CHECK PICSAVE
        BLE.S   NOTPIC                          ;BRANCH IF NOT PICSAVE

        MOVE.B  D7,-(SP)
        JSR     PutPicVerb                      ;PUT ADDIONAL PARAMS TO THEPIC
        MOVEQ   #$70,D0                         ;PUT POLYNOUN IN HI NIBBLE
        ADD     D7,D0                           ;PUT VERB IN LO NIBBLE
        JSR     DPutPicByte                     ;PUT OPCODE TO THEPIC
        MOVE.L  POLY(A6),-(SP)                  ;PUSH POLYHANDLE
        JSR     PutPicRgn                       ;TREAT SAME AS A REGION

NOTPIC  MOVE.L  POLY(A6),A2                     ;GET POLYHANDLE
        TST.B   D7                              ;IS VERB FRAME ?
        BNE.S   NOTFR                           ;NO, CONTINUE
        MOVE.L  A2,-(SP)                        ;PUSH POLYHANDLE
        JSR     FrPoly                          ;FrPoly(poly);
        BRA.S   GOHOME                          ;AND QUIT

NOTFR   MOVE.L  (A2),A0                         ;DE-REFERANCE POLYHANDLE
        PEA     POLYBBOX(A0)                    ;PUSH POLYBBOX
        MOVE.L  VISRGN(A3),A0                   ;GET VISRGN HANDLE
        MOVE.L  (A0),A0                         ;DE-REFERENCE HANDLE
        PEA     RGNBBOX(A0)                     ;PUSH VISRGN BBOX
        MOVE.L  CLIPRGN(A3),A0                  ;GET CLIPRGN HANDLE
        MOVE.L  (A0),A0                         ;DE-REFERENCE HANDLE
        PEA     RGNBBOX(A0)                     ;PUSH CLIPRGN BBOX
        MOVE    #3,-(SP)                        ;PUSH NRECTS = 3
        PEA     MINRECT(A6)                     ;PUT RESULT IN MINRECT
        JSR     RSECT                           ;CALC INTERSECTION
        BEQ.S   GOHOME                          ;QUIT IF NO INTERSECT

        MOVE.L  A2,-(SP)                        ;PUSH POLYHANDLE
        JSR     PushVerb                        ;PUSH MODE AND PATTERN
        JSR     DrawPoly                        ;DrawPoly(poly,mode,pat);

GOHOME  MOVEM.L (SP)+,D5-D7/A2-A4               ;RESTORE REGS
        UNLINK  PARAMSIZE,'STDPOLY '



        .PROC  FramePoly,1
        .DEF   CallPoly,PaintPoly,ErasePoly,InvertPoly,FillPoly
        .REF   StdPoly
;-----------------------------------------------------
;
;  PROCEDURE FramePoly(* poly: PolyHandle *);
;
        MOVEQ   #FRAME,D0                       ;VERB = FRAME
        BRA.S   CallPoly                        ;SHARE COMMON CODE



;-----------------------------------------------------
;
;  PROCEDURE PaintPoly(* poly: PolyHandle *);
;
PaintPoly
        MOVEQ   #PAINT,D0                       ;VERB = PAINT
        BRA.S   CallPoly                        ;SHARE COMMON CODE



;--------------------------------------------------------
;
;  PROCEDURE ErasePoly(* poly: PolyHandle *);
;
ErasePoly
        MOVEQ   #ERASE,D0                       ;VERB = ERASE
        BRA.S   CallPoly                        ;SHARE COMMON CODE



;--------------------------------------------------------
;
;  PROCEDURE InvertPoly(* poly: PolyHandle *);
;
InvertPoly
        MOVEQ   #INVERT,D0                      ;VERB = INVERT
        BRA.S   CallPoly                        ;SHARE COMMON CODE



;--------------------------------------------------------
;
;  PROCEDURE FillPoly(* poly: PolyHandle; pat: Pattern *);
;
FillPoly
        MOVE.L  (SP)+,A0                        ;POP RETURN ADDR
        MOVE.L  (SP)+,A1                        ;POP ADDR OF PATTERN
        MOVE.L  A0,-(SP)                        ;PUT RETURN ADDR BACK
        MOVE.L  GRAFGLOBALS(A5),A0              ;POINT TO LISAGRAF GLOBALS
        MOVE.L  THEPORT(A0),A0                  ;GET CURRENT GRAFPORT
        LEA     FILLPAT(A0),A0                  ;POINT TO FILLPAT
        MOVE.L  (A1)+,(A0)+                     ;COPY PAT INTO FILLPAT
        MOVE.L  (A1)+,(A0)+                     ;ALL EIGHT BYTES
        MOVEQ   #FILL,D0                        ;VERB = FILL
        BRA.S   CallPoly                        ;SHARE COMMON CODE



;---------------------------------------------------------------
;
;  PROCEDURE CallPoly(poly: PolyHandle);
;
;  code shared by FramePoly, PaintPoly, ErasePoly, InvertPoly, and FillPoly.
;  enter with verb in D0.
;
CallPoly
        MOVE.L  (SP)+,A0                        ;POP RETURN ADDR
        MOVE.L  (SP)+,A1                        ;POP POLY
        MOVE.B  D0,-(SP)                        ;PUSH VERB
        MOVE.L  A1,-(SP)                        ;PUSH POLY
        MOVE.L  A0,-(SP)                        ;RESTORE RETURN ADDR
        MOVE.L  GRAFGLOBALS(A5),A0              ;POINT TO LISAGRAF GLOBALS
        MOVE.L  THEPORT(A0),A0                  ;GET CURRENT GRAFPORT
        MOVE.L  GRAFPROCS(A0),D0                ;IS GRAFPROCS NIL ?
        LEA     STDPOLY,A0
        BEQ.S   USESTD                          ;YES, USE STD PROC
        MOVE.L  D0,A0
        MOVE.L  POLYPROC(A0),A0                 ;NO, GET PROC PTR
USESTD  JMP     (A0)                            ;GO TO IT



        .FUNC   OpenPoly,0
        .REF    HidePen,NewHandle
;---------------------------------------------------------------
;
;  FUNCTION OpenPoly: PolyHandle;
;
STARTSIZE       .EQU    138                     ;ENOUGH FOR 32 POINTS

        JSR     HidePen                         ;TURN OFF DRAWING
        CLR.L   -(SP)                           ;MAKE ROOM FOR FCN RESULT
        MOVE    #STARTSIZE,-(SP)                ;PUSH BYTE COUNT = STARTSIZE
        JSR     NEWHANDLE                       ;ALLOCATE NEWHANDLE
        MOVE.L  (SP)+,A1                        ;POP RESULTING HANDLE
        MOVE.L  A1,4(SP)                        ;PUT HANDLE IN FCN RESULT
        MOVE.L  GRAFGLOBALS(A5),A0              ;POINT TO QUICKDRAW GLOBALS
        MOVE.L  A1,THEPOLY(A0)                  ;REMEMBER HANDLE IN THEPOLY
        MOVE    #STARTSIZE,POLYMAX(A0)          ;POLYMAX := STARTSIZE;
        MOVE.L  THEPORT(A0),A0                  ;GET CURRENT GRAFPORT
        MOVEQ   #1,D0
        MOVE.L  D0,POLYSAVE(A0)                 ;POLYSAVE := TRUE
        MOVE.L  (A1),A1                         ;DE-REFERENCE HANDLE
        MOVE    #10,(A1)+                       ;INSTALL POLYSIZE = 10
        CLR.L   (A1)+                           ;ZERO OUT POLYBBOX
        CLR.L   (A1)+
        RTS                                     ;RETURN



        .PROC   ClosePoly,0
        .REF    SetSize,ShowPen
;---------------------------------------------------------------
;
;  PROCEDURE ClosePoly;
;
;  A6 OFFSETS OF PARAMS AND LOCALS AFTER LINK:
;
        MOVEM.L D3-D7/A4,-(SP)                  ;SAVE REGS
        MOVE.L  GRAFGLOBALS(A5),A4              ;POINT TO QUICKDRAW GLOBALS
        MOVE.L  THEPORT(A4),A0                  ;GET CURRENT GRAFPORT
        CLR.L   POLYSAVE(A0)                    ;POLYSAVE := FALSE
        MOVE.L  THEPOLY(A4),A4                  ;GET THEPOLY HANDLE
        MOVE.L  (A4),A0                         ;DE-REFERENCE THEPOLY
        MOVE    (A0)+,D7                        ;GET POLYSIZE
        CLR.L   (A0)+                           ;ZERO OUT POLYBBOX
        CLR.L   (A0)+
        MOVE    D7,D6
        SUB     #10,D6
        LSR     #2,D6                           ;NPOINTS = (SIZE-10) DIV 4
        BEQ.S   EMPTY                           ;QUIT IF NO POINTS


;-----------------------------------------------------
;
;  SCAN FOR BOUNDING BOX OF POLYGON
;
        MOVE    (A0)+,D1                        ;TOP := FIRST POINT VERT
        MOVE    D1,D2                           ;BOTTOM := FIRST POINT VERT
        MOVE    (A0)+,D3                        ;LEFT := FIRST POINT HORIZ
        MOVE    D3,D4                           ;RIGHT := FIRST POINT HORIZ
        SUB     #1,D6                           ;DECREMENT POINT COUNT
        BRA.S   RIGHTOK                         ;GO TO LOOP START
NEXTPT  MOVE    (A0)+,D0                        ;GET VERT COORD
        CMP     D1,D0                           ;IS VERT < BBOX TOP ?
        BGE.S   TOPOK                           ;NO, CONTINUE
        MOVE    D0,D1                           ;YES, UPDATE BBOX TOP
TOPOK   CMP     D2,D0                           ;IS VERT > BBOX BOTTOM ?
        BLE.S   BOTOK                           ;NO, CONTINUE
        MOVE    D0,D2                           ;YES, UPDATE BBOX BOTTOM
BOTOK   MOVE    (A0)+,D0                        ;GET HORIZ COORD
        CMP     D3,D0                           ;IS HORIZ < BBOX LEFT ?
        BGE.S   LEFTOK                          ;NO, CONTINUE
        MOVE    D0,D3                           ;YES, UPDATE BBOX LEFT
LEFTOK  CMP     D4,D0                           ;IS HORIZ > BBOX RIGHT ?
        BLE.S   RIGHTOK                         ;NO, CONTINUE
        MOVE    D0,D4                           ;YES, UPDATE BBOX RIGHT
RIGHTOK DBRA    D6,NEXTPT                       ;LOOP ALL POINTS
        MOVE.L  (A4),A0                         ;DE-REFERENCE THEPOLY
        LEA     POLYBBOX(A0),A0                 ;POINT TO POLYBBOX
        MOVE    D1,(A0)+                        ;INSTALL BBOX TOP
        MOVE    D3,(A0)+                        ;INSTALL BBOX LEFT
        MOVE    D2,(A0)+                        ;INSTALL BBOX BOTTOM
        MOVE    D4,(A0)+                        ;INSTALL BBOX RIGHT


;--------------------------------------------------------
;
;  TRIM THEPOLY TO FINAL SIZE, SHOW PEN AND QUIT
;
EMPTY   MOVE.L  A4,-(SP)                        ;PUSH THEPOLY HANDLE
        MOVE    D7,-(SP)                        ;PUSH BYTECOUNT = POLYSIZE
        JSR     SETSIZE                         ;TRIM TO MINIMUM SIZE
        JSR     SHOWPEN                         ;RESTORE PNVIS
        MOVEM.L (SP)+,D3-D7/A4                  ;RESTORE REGS
        RTS                                     ;AND RETURN



        .PROC   KillPoly,1
;---------------------------------------------------
;
;  PROCEDURE KillPoly(poly: PolyHandle);
;
        MOVE.L  (SP)+,A1                        ;pop return addr
        MOVE.L  (SP)+,A0                        ;pop handle
        _DisposHandle                           ;discard it
        JMP     (A1)                            ;and return




        .PROC   OffsetPoly,3
;---------------------------------------------------
;
;  PROCEDURE OffsetPoly(poly: PolyHandle; dh,dv: INTEGER);
;
        MOVE.L  (SP)+,A0                        ;POP RETURN ADDRESS
        MOVE    (SP)+,D0                        ;POP DV
        MOVE    (SP)+,D1                        ;POP DH
        MOVE.L  (SP)+,A1                        ;POP POLYHANDLE
        MOVE.L  (A1),A1                         ;DE-REFERENCE POLYHANDLE
        MOVE    (A1)+,D2                        ;GET POLYSIZE
        SUB     #2,D2                           ;CALC TOTAL # POINTS, INCL BBOX
        LSR     #2,D2                           ; # POINTS = (SIZE-2) DIV 4
        SUB     #1,D2                           ;INIT DBRA COUNT
NEXTPT  ADD     D0,(A1)+                        ;OFFSET VERT COORD
        ADD     D1,(A1)+                        ;OFFSET HORIZ COORD
        DBRA    D2,NEXTPT                       ;LOOP FOR ALL POINTS
        JMP     (A0)                            ;AND RETURN



        .PROC MapPoly,3
        .REF  MapPt,MapRect
;-------------------------------------------------------------
;
;  PROCEDURE MapPoly(poly: PolyHandle; fromRect,toRect: Rect);
;
;  A6 OFFSETS OF PARAMETERS AND LOCALS AFTER LINK:
;
PARAMSIZE       .EQU    12
POLY            .EQU    PARAMSIZE+8-4           ;LONG, RGNHANDLE
FROMRECT        .EQU    POLY-4                  ;LONG, ADDR OF RECT
TORECT          .EQU    FROMRECT-4              ;LONG, ADDR OF RECT

        LINK    A6,#0                           ;ALLOCATE STACK FRAME
        MOVEM.L D7/A2-A4,-(SP)                  ;SAVE REGS
;
; QUIT FAST IF FROMRECT = TORECT
;
        MOVE.L  FROMRECT(A6),A2                 ;POINT TO FROMRECT
        MOVE.L  TORECT(A6),A3                   ;POINT TO TORECT
        MOVE.L  (A2),D0
        CMP.L   (A3),D0                         ;IS TOPLEFT SAME ?
        BNE.S   NOTSAME                         ;NO, CONTINUE
        MOVE.L  4(A2),D0
        CMP.L   4(A3),D0                        ;YES, IS BOTRIGHT SAME TOO ?
        BEQ.S   DONE                            ;IF SO, JUST QUIT

NOTSAME MOVE.L  POLY(A6),A4                     ;GET POLYHANDLE
        MOVE.L  (A4),A4                         ;DE-REFERENCE POLYHANDLE
        PEA     POLYBBOX(A4)                    ;PUSH ADDR OF BBOX
        MOVE.L  A2,-(SP)                        ;PUSH FROMRECT
        MOVE.L  A3,-(SP)                        ;PUSH TORECT
        JSR     MAPRECT                         ;MAP POLYBBOX

        MOVEQ   #10,D0
        MOVE    POLYSIZE(A4),D7                 ;GET POLYSIZE
        SUB     D0,D7
        LSR     #2,D7                           ;NPOINTS = (POLYSIZE-10) DIV 4
        ADD     D0,A4                           ;POINT TO FIRST POINT
        BRA.S   START                           ;GO TO LOOP START
NEXTPT  MOVE.L  A4,-(SP)                        ;PUSH ADDR OF POINT
        MOVE.L  A2,-(SP)                        ;PUSH FROMRECT
        MOVE.L  A3,-(SP)                        ;PUSH TORECT
        JSR     MAPPT                           ;MAP THIS POINT
        ADD     #4,A4                           ;BUMP TO NEXT POINT
START   DBRA    D7,NEXTPT                       ;LOOP ALL POINTS IN POLY

DONE    MOVEM.L (SP)+,D7/A2-A4                  ;RESTORE REGS
        UNLINK  PARAMSIZE,'MAPPOLY '



        .PROC  FrPoly,1
        .REF   MoveTo,DoLine
;--------------------------------------------------------
;
;  PROCEDURE FrPoly(poly: PolyHandle);
;
;  A6 OFFSETS OF PARAMS AND LOCALS AFTER LINK:
;
PARAMSIZE       .EQU    4
POLY            .EQU    PARAMSIZE+8-4           ;LONG, POLYHANDLE

        LINK    A6,#0                           ;ALLOCATE STACK FRAME
        MOVEM.L D6-D7/A4,-(SP)                  ;SAVE REGS
        MOVE.L  POLY(A6),A4                     ;GET POLYHANDLE
        MOVE.L  (A4),A0                         ;DE-REFERENCE IT
        MOVE    (A0),D7                         ;GET POLYSIZE
        SUB     #10,D7
        LSR     #2,D7                           ;NPOINTS = (SIZE-10) DIV 4
        BEQ.S   DONE                            ;QUIT IF EMPTY POLYGON
        MOVE.L  10(A0),-(SP)                    ;PUSH FIRST POINT
        JSR     MOVETO                          ;MOVETO(FIRST POINT)
        MOVE.L  #14,D6                          ;INIT BYTE OFFSET
        SUB     #1,D7                           ;DECREMENT COUNT
        BRA.S   START                           ;GOT TO LOOP START
NEXTPT  MOVE.L  (A4),A0                         ;DE-REFERENCE POLYHANDLE
        MOVE.L  0(A0,D6),-(SP)                  ;PUSH NEXT POINT
        ADD     #4,D6                           ;BUMP BYTE OFFSET
        JSR     DOLINE                          ;DOLINE(PT)
START   DBRA    D7,NEXTPT                       ;LOOP FOR ALL POINTS
DONE    MOVEM.L (SP)+,D6-D7/A4                  ;RESTORE REGS
        UNLINK  PARAMSIZE,'FRPOLY  '



        .PROC DrawPoly,3
        .REF  OpenRgn,FrPoly,DoLine,NewRgn,CloseRgn,DrawRgn
;--------------------------------------------------------
;
;  PROCEDURE DrawPoly(poly: PolyHandle; mode: INTEGER; VAR pat: Pattern);
;
;  A6 OFFSETS OF PARAMS AND LOCALS AFTER LINK:
;
PARAMSIZE       .EQU    10
POLY            .EQU    PARAMSIZE+8-4           ;LONG, POLYHANDLE
MODE            .EQU    POLY-2                  ;WORD
PAT             .EQU    MODE-4                  ;LONG, ADDR OF PATTERN

        LINK    A6,#0                           ;NO LOCAL VARS
        MOVE.L  A4,-(SP)                        ;SAVE REG
        MOVE.L  GRAFGLOBALS(A5),A0              ;POINT TO QUICKDRAW GLOBALS
        MOVE.L  THEPORT(A0),A0                  ;GET CURRENT PORT
        TST     PNVIS(A0)                       ;IS PNVIS NEG ?
        BMI.S   DONE                            ;YES, QUIT
        JSR     OPENRGN                         ;OpenRgn
        MOVE.L  POLY(A6),-(SP)
        JSR     FRPOLY                          ;FrPoly(poly);
        MOVE.L  POLY(A6),A0
        MOVE.L  (A0),A0
        MOVE.L  10(A0),-(SP)                    ;PUSH FIRST POINT
        JSR     DOLINE                          ;MAKE SURE IT CLOSES
        CLR.L   -(SP)                           ;ROOM FOR FCN RESULT
        JSR     NEWRGN                          ;ALLOCATE TEMPRGN
        MOVE.L  (SP),A4                         ;PUT TEMPRGN IN A4
        JSR     CLOSERGN                        ;CLOSERGN(TEMPRGN)
        MOVE.L  A4,-(SP)
        MOVE    MODE(A6),-(SP)
        MOVE.L  PAT(A6),-(SP)
        JSR     DRAWRGN                         ;DrawRgn(tempRgn,mode,pat);
        MOVE.L  A4,A0                           ;get tempRgn
        _DisposHandle                           ;DISCARD IT
DONE    MOVEM.L (SP)+,A4                        ;RESTORE REG
        UNLINK  PARAMSIZE,'DRAWPOLY'



        .END
