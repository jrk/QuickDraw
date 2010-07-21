        .INCLUDE  GrafTypes.text

        .FUNC AngleFromSlope,1
        .DEF  SlopeFromAngle
;-----------------------------------------------------
;
;  FUNCTION AngleFromSlope(slope: Fixed): INTEGER;
;
;  Scans slope table for angle and returns angle 0..180
;
        MOVE.L  4(SP),D0                        ;GET SLOPE
        SMI     D2                              ;REMEMBER IF IT WAS NEGATIVE
        BPL.S   NOTNEG                          ;CONTINUE IF POSITIVE
        NEG.L   D0                              ;ELSE MAKE SLOPE POS
NOTNEG  SUB.L   #500,D0                         ;BIAS THE COMPARE
        MOVE.L  D0,4(SP)
        LEA     CONTINUE,A0                     ;POINT TO TABLE OF SLOPES
        MOVEQ   #-1,D1                          ;INIT ANGLE COUNT
SCAN    ADD.W   #1,D1
        MOVE.W  D1,D0
        CLR.L   -(SP)
        BRA.S   A2SLOPE
CONTINUE
        MOVE.L  8(SP),D0                        ;GET SLOPE
        CMP.L   (SP)+,D0                        ;SCAN THRU SLOPE TABLE
        BGT.S   SCAN
        MOVE    #180,D0
        SUB     D1,D0                           ;CALC 180-ANGLE = 90..180
        TST.B   D2                              ;WAS DH POS ?
        BPL.S   DONE                            ;NO, RETURN 90..180
        MOVE    D1,D0                           ;YES, RETURN 0..90
DONE    MOVE.L  (SP)+,(SP)                      ;STRIP PARAM
        MOVE.W  D0,4(SP)                        ;RETURN FUNCTION RESULT
        RTS



;----------------------------------------------------------------
;
;  FUNCTION SlopeFromAngle(angle: INTEGER): Fixed;
;
;  calculate the fixed point slope of a line, DH/DV = -65536 * Tan(angle).
;  Input angle is treated MOD 180.
;
SlopeFromAngle
        MOVE.L  (SP)+,A0                        ;POP RETURN ADDR
        MOVE    (SP)+,D0                        ;GET INTEGER ANGLE
        EXT.L   D0                              ;SIGN EXTEND FOR DIVIDE
        DIVS    #180,D0                         ;TREAT ANGLE MOD 180
        SWAP    D0                              ;GET THE REMAINDER
        TST     D0                              ;WAS IT NEGATIVE ?
        BPL.S   OK1                             ;NO, CONTINUE
        ADD     #180,D0                         ;YES, PUT IN RANGE 0..179
OK1     MOVE    #$8000,(SP)
A2SLOPE CMP     #90,D0
        BLE.S   OK2
        CLR.W   (SP)
        SUB     #180,D0
        NEG     D0
OK2     CMP     #45,D0
        BLT.S   SHARE
        ADD     #1,(SP)
        CMP     #64,D0
        BLT.S   SHARE
        MOVE.B  SLOPE-91(D0),1(SP)
        BPL.S   SHARE
        OR.B    #$7F,(SP)
SHARE   ADD     D0,D0
        MOVE.W  SLOPE(D0),2(SP)
CHECK   BCLR    #7,(SP)
        BEQ.S   OK3
        NEG.L   (SP)
OK3     JMP     (A0)



;       .BYTE   $01             ;45
;       .BYTE   $01
;       .BYTE   $01
;       .BYTE   $01
;       .BYTE   $01
;       .BYTE   $01             ;50
;       .BYTE   $01
;       .BYTE   $01
;       .BYTE   $01
;       .BYTE   $01
;       .BYTE   $01             ;55
;       .BYTE   $01
;       .BYTE   $01
;       .BYTE   $01
;       .BYTE   $01
;       .BYTE   $01             ;60
;       .BYTE   $01
;       .BYTE   $01
        .BYTE   $01
        .BYTE   $02
        .BYTE   $02             ;65
        .BYTE   $02
        .BYTE   $02
        .BYTE   $02
        .BYTE   $02
        .BYTE   $02             ;70
        .BYTE   $02
        .BYTE   $03
        .BYTE   $03
        .BYTE   $03
        .BYTE   $03             ;75
        .BYTE   $04
        .BYTE   $04
        .BYTE   $04
        .BYTE   $05
        .BYTE   $05             ;80
        .BYTE   $06
        .BYTE   $07
        .BYTE   $08
        .BYTE   $09
        .BYTE   $0B             ;85
        .BYTE   $0E
        .BYTE   $13
        .BYTE   $1C
        .BYTE   $39
        .BYTE   $FF             ;90
SLOPE
        .WORD   $0000           ;0
        .WORD   $0478
        .WORD   $08F1
        .WORD   $0D6B
        .WORD   $11E7
        .WORD   $1666           ;5
        .WORD   $1AE8
        .WORD   $1F6F
        .WORD   $23FA
        .WORD   $288C
        .WORD   $2D24           ;10
        .WORD   $31C3
        .WORD   $366A
        .WORD   $3B1A
        .WORD   $3FD4
        .WORD   $4498           ;15
        .WORD   $4968
        .WORD   $4E44
        .WORD   $532E
        .WORD   $5826
        .WORD   $5D2D           ;20
        .WORD   $6245
        .WORD   $676E
        .WORD   $6CAA
        .WORD   $71FB
        .WORD   $7760           ;25
        .WORD   $7CDC
        .WORD   $8270
        .WORD   $881E
        .WORD   $8DE7
        .WORD   $93CD           ;30
        .WORD   $99D2
        .WORD   $9FF7
        .WORD   $A640
        .WORD   $ACAD
        .WORD   $B341           ;35
        .WORD   $B9FF
        .WORD   $C0E9
        .WORD   $C802
        .WORD   $CF4E
        .WORD   $D6CF           ;40
        .WORD   $DE8A
        .WORD   $E681
        .WORD   $EEB9
        .WORD   $F737
        .WORD   $0000           ;45
        .WORD   $0919
        .WORD   $1287
        .WORD   $1C51
        .WORD   $267F
        .WORD   $3117           ;50
        .WORD   $3C22
        .WORD   $47AA
        .WORD   $53B9
        .WORD   $605B
        .WORD   $6D9B           ;55
        .WORD   $7B89
        .WORD   $8A35
        .WORD   $99AF
        .WORD   $AA0E
        .WORD   $BB68           ;60
        .WORD   $CDD6
        .WORD   $E177
        .WORD   $F66E
        .WORD   $0CE1
        .WORD   $24FE           ;65
        .WORD   $3EFC
        .WORD   $5B19
        .WORD   $799F
        .WORD   $9AE7
        .WORD   $BF5B           ;70
        .WORD   $E77A
        .WORD   $13E3
        .WORD   $4556
        .WORD   $7CC7
        .WORD   $BB68           ;75
        .WORD   $02C2
        .WORD   $54DB
        .WORD   $B462
        .WORD   $2501
        .WORD   $ABD9           ;80
        .WORD   $5051
        .WORD   $1D88
        .WORD   $24F3
        .WORD   $83AD
        .WORD   $6E17           ;85
        .WORD   $4CF5
        .WORD   $14BD
        .WORD   $A2D7
        .WORD   $4A30
        .WORD   $FFFF           ;90


        .PROC   PtToAngle,3
        .REF    AngleFromSlope
;--------------------------------------------------------------
;
;  PROCEDURE PtToAngle(r: Rect; pt: Point; VAR angle: INTEGER);
;
;  Given a rectangle and a point, return the angle subtended by pt.
;
;  A6 OFFSETS OF PARAMETERS AFTER LINK:
;
PARAMSIZE       .EQU    12                      ;TOTAL BYTES OF PARAMS
RECT            .EQU    PARAMSIZE+8-4           ;ADDR OF RECT
PT              .EQU    RECT-4                  ;POINT
ANGLE           .EQU    PT-4                    ;ADDR OF INTEGER;

        LINK    A6,#0                           ;NO LOCALS
        MOVEM.L D6-D7/A4,-(SP)                  ;SAVE REGS
        MOVE.L  RECT(A6),A4                     ;POINT TO RECT

        MOVE    BOTTOM(A4),D0
        ADD     TOP(A4),D0
        ASR     #1,D0                           ;CENTER.V := (TOP+BOTTOM)/2
        MOVE    PT+V(A6),D1
        SUB     D0,D1                           ;DV := PT.V - CENTER.V

        MOVE    RIGHT(A4),D0
        ADD     LEFT(A4),D0
        ASR     #1,D0                           ;CENTER.H := (LEFT+RIGHT)/2
        MOVE    PT+H(A6),D7
        SUB     D0,D7                           ;DH := PT.H - CENTER.H
        BNE.S   DHOK                            ;CONTINUE IF DH <> 0
        TST     D1                              ;WAS DV > 0 ?
        BLE.S   ZERO                            ;NO, RETURN ANGLE = 0
        MOVE    #180,D0                         ;YES, RETURN ANGLE = 180
        BRA.S   DONE

DHOK    CLR.L   -(SP)                           ;ROOM FOR FCN RESULT
        MOVE    D7,-(SP)                        ;PUSH DH
        MOVE    D1,-(SP)                        ;PUSH DV
        _FixRatio                               ;CALC SLOPE := DH/DV
        MOVE.L  (SP)+,D6                        ;GET SLOPE RESULT

        CLR.L   -(SP)                           ;ROOM FOR FCN RESULT
        MOVE    BOTTOM(A4),D0
        SUB     TOP(A4),D0
        MOVE    D0,-(SP)                        ;PUSH HEIGHT
        MOVE    RIGHT(A4),D0
        SUB     LEFT(A4),D0
        MOVE    D0,-(SP)                        ;PUSH WIDTH
        _FixRatio                               ;CALC ASPECT := HT/WD
        MOVE.L  (SP)+,D0                        ;GET ASPECT RESULT

        CLR.L   -(SP)                           ;ROOM FOR FCN RESULT
        MOVE.L  D6,-(SP)                        ;PUSH SLOPE
        MOVE.L  D0,-(SP)                        ;PUSH ASPECT
        _FixMul                                 ;CALC SLOPE*ASPECT
        MOVE.L  (SP)+,D0                        ;GET RESULT SLOPE2

        CLR.W   -(SP)                           ;ROOM FOR FCN RESULT
        MOVE.L  D0,-(SP)                        ;PUSH SLOPE2
        JSR     AngleFromSlope                  ;SCAN FOR ARCTAN
        MOVE    (SP)+,D0                        ;GET RESULT ANGLE

        TST     D7                              ;WAS DH POSITIVE ?
        BPL.S   DONE                            ;YES, CONTINUE
        ADD     #180,D0                         ;NO, ADD 180 TO ANG
        CMP     #360,D0                         ;IS RESULT = 360 ?
        BNE.S   DONE                            ;NO, CONTINUE
ZERO    CLR     D0                              ;YES, ANGLE := 0
DONE    MOVE.L  ANGLE(A6),A0                    ;GET VAR ADDR
        MOVE    D0,(A0)                         ;STORE INTO ANGLE
        MOVEM.L (SP)+,D6-D7/A4                  ;RESTORE REGS
        UNLINK  PARAMSIZE,'PTTOANGL'



        .END
