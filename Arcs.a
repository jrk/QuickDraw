        .INCLUDE  GRAFTYPES.TEXT
;-----------------------------------------------------------
;
;
;       *    *****    ***    ***
;      * *   *    *  *   *  *   *
;     *   *  *    *  *      *
;     *   *  *****   *       ***
;     *****  *  *    *          *
;     *   *  *   *   *   *  *   *
;     *   *  *    *   ***    ***
;
;   Procedures for drawing Arcs:
;


        .PROC StdArc,4
        .REF  CheckPic,PutPicVerb,PutPicWord,PutPicRect
        .REF  PushVerb,DrawArc
;---------------------------------------------------------------
;
;  PROCEDURE StdArc(verb: GrafVerb; r: Rect; startAngle,arcAngle: INTEGER);
;
;  A6 OFFSETS OF PARAMS AFTER LINK:
;
PARAMSIZE       .EQU    10
VERB            .EQU    PARAMSIZE+8-2           ;GRAFVERB
RECT            .EQU    VERB-4                  ;LONG, ADDR OF RECT
STARTANG        .EQU    RECT-2                  ;WORD
ARCANG          .EQU    STARTANG-2              ;WORD

OVWD            .EQU    -2                      ;WORD
OVHT            .EQU    OVWD-2                  ;WORD
VARSIZE         .EQU    OVHT                    ;TOTAL BYTES OF LOCALS


        LINK    A6,#VARSIZE                     ;ALLOCATE STACK FRAME
        MOVEM.L D4/D7/A3-A4,-(SP)               ;SAVE REGS
        MOVE.B  VERB(A6),D7                     ;GET VERB
        JSR     CHECKPIC                        ;SET UP A4,A3 AND CHECK PICSAVE
        BLE.S   NOTPIC                          ;BRANCH IF NOT PICSAVE

        MOVE.B  D7,-(SP)
        JSR     PutPicVerb                      ;PUT ADDIONAL PARAMS TO THEPIC
        MOVEQ   #$60,D0                         ;PUT ARCNOUN IN HI NIBBLE
        ADD     D7,D0                           ;PUT VERB IN LO NIBBLE
        MOVE.B  D0,-(SP)                        ;PUSH OPCODE
        MOVE.L  RECT(A6),-(SP)                  ;PUSH ADDR OF RECT
        JSR     PutPicRect                      ;PUT OPCODE AND RECTANGLE
        MOVE    STARTANG(A6),-(SP)
        JSR     PutPicWord                      ;PUT STARTANGLE
        MOVE    ARCANG(A6),-(SP)
        JSR     PutPicWord                      ;PUT ARCANGLE

NOTPIC  MOVE.L  RECT(A6),A0                     ;POINT TO RECT
        MOVE    RIGHT(A0),D0
        SUB     LEFT(A0),D0
        MOVE    D0,OVWD(A6)                     ;OVWD := R.RIGHT - R.LEFT
        MOVE    BOTTOM(A0),D0
        SUB     TOP(A0),D0
        MOVE    D0,OVHT(A6)                     ;OVHT := R.BOTTOM - R.TOP

        MOVE.L  A0,-(SP)                        ;PUSH ADDR OF RECT
        CLR.B   -(SP)                           ;PUSH HOLLOW = FALSE
        TST.B   D7                              ;IS VERB FRAME ?
        BNE.S   DOIT                            ;NO, CONTINUE
;
;  Currently, FrameArc does not put inversion points to theRgn.
;  If this changes, add test and call to PutArc here.
;
        MOVE.B  #1,(SP)                         ;REPLACE, PUSH HOLLOW = TRUE
DOIT    MOVE.L  OVHT(A6),-(SP)                  ;PUSH OVWD,OVHT
        JSR     PushVerb                        ;PUSH MODE AND PATTERN
        MOVE    STARTANG(A6),-(SP)              ;PUSH STARTANGLE
        MOVE    ARCANG(A6),-(SP)                ;PUSH ARCANGLE

;  DrawArc(r,hollow,ovWd,ovHt,mode,pat,startAng,arcAng);

        JSR     DrawArc
        MOVEM.L (SP)+,D4/D7/A3-A4               ;RESTORE REGS
        UNLINK  PARAMSIZE,'STDARC  '



        .PROC FrameArc,3
        .DEF  CallArc,PaintArc,EraseArc,InvertArc,FillArc
        .REF  STDARC
;-----------------------------------------------------
;
;  PROCEDURE FrameArc(* r: Rect; startAngle,arcAngle: INTEGER *);
;
        MOVEQ   #FRAME,D0                       ;VERB = FRAME
        BRA.S   CallArc                         ;SHARE COMMON CODE


;-----------------------------------------------------
;
;  PROCEDURE PaintArc(* r: Rect; startAngle,arcAngle: INTEGER *);
;
PaintArc
        MOVEQ   #PAINT,D0                       ;VERB = PAINT
        BRA.S   CallArc                         ;SHARE COMMON CODE


;--------------------------------------------------------
;
;  PROCEDURE EraseArc(* r: Rect; startAngle,arcAngle: INTEGER *);
;
EraseArc
        MOVEQ   #ERASE,D0                       ;VERB = ERASE
        BRA.S   CallArc                         ;SHARE COMMON CODE


;--------------------------------------------------------
;
;  PROCEDURE InvertArc(* r: Rect; startAngle,arcAngle: INTEGER *);
;
InvertArc
        MOVEQ   #INVERT,D0                      ;VERB = INVERT
        BRA.S   CallArc                         ;SHARE COMMON CODE


;--------------------------------------------------------
;
;  PROCEDURE FillArc(* r: Rect; startAngle,arcAngle: INTEGER; pat: Pattern *);
;
FillArc MOVE.L  (SP)+,A0                        ;POP RETURN ADDR
        MOVE.L  (SP)+,A1                        ;POP ADDR OF PATTERN
        MOVE.L  A0,-(SP)                        ;PUT RETURN ADDR BACK
        MOVE.L  GRAFGLOBALS(A5),A0              ;POINT TO LISAGRAF GLOBALS
        MOVE.L  THEPORT(A0),A0                  ;GET CURRENT GRAFPORT
        LEA     FILLPAT(A0),A0                  ;POINT TO FILLPAT
        MOVE.L  (A1)+,(A0)+                     ;COPY PAT INTO FILLPAT
        MOVE.L  (A1)+,(A0)+                     ;ALL EIGHT BYTES
        MOVEQ   #FILL,D0                        ;VERB = FILL
        BRA.S   CallArc                         ;SHARE COMMON CODE


;---------------------------------------------------------------
;
;  PROCEDURE CallArc(r: Rect; startAngle,arcAngle: INTEGER);
;
;  code shared by FrameArc, PaintArc, EraseArc, InvertArc, and FillArc.
;  enter with verb in D0.
;
CallArc MOVE.L  (SP)+,A0                        ;POP RETURN ADDR
        MOVE.L  (SP)+,D1                        ;POP BOTH ANGLES
        MOVE.L  (SP)+,A1                        ;POP ADDR OF RECT
        MOVE.B  D0,-(SP)                        ;PUSH VERB
        MOVE.L  A1,-(SP)                        ;PUSH ADDR OF RECT
        MOVE.L  D1,-(SP)                        ;PUSH BOTH ANGLES
        MOVE.L  A0,-(SP)                        ;PUSH RETURN ADDR
        MOVE.L  GRAFGLOBALS(A5),A0              ;POINT TO LISAGRAF GLOBALS
        MOVE.L  THEPORT(A0),A0                  ;GET CURRENT GRAFPORT
        MOVE.L  GRAFPROCS(A0),D0                ;IS GRAFPROCS NIL ?
        LEA     STDARC,A0
        BEQ.S   USESTD                          ;YES, USE STD PROC
        MOVE.L  D0,A0
        MOVE.L  ARCPROC(A0),A0                  ;NO, GET PROC PTR
USESTD  JMP     (A0)                            ;GO TO IT



        .END
