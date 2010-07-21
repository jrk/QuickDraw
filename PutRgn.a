        .INCLUDE  GRAFTYPES.TEXT


        .PROC   PUTRGN,4
        .REF    SETSIZE
;----------------------------------------------------------------
;
;  PROCEDURE PutRgn(Rgn: RgnHandle; bufHandle: Handle; VAR index,size: INTEGER);
;
;  Expands a region out to an array of inversion points.
;


;------------------------------------------------
;
;  A6 OFFSETS OF PARAMETERS AFTER LINK:
;
PARAMSIZE       .EQU    16                      ;TOTAL SIZE OF PARAMETERS
RGNHANDLE       .EQU    PARAMSIZE+8-4           ;LONG,RGNHANDLE
BUFHANDLE       .EQU    RGNHANDLE-4             ;LONG, HANDLE
INDEX           .EQU    BUFHANDLE-4             ;LONG, VAR
SIZE            .EQU    INDEX-4                 ;LONG, VAR


        LINK    A6,#0                           ;NO LOCAL VARS
        MOVEM.L D7/A2-A4,-(SP)                  ;SAVE REGS

;-------------------------------------------------------
;
;  EXPAND BUF TO FIT WORST CASE BYTESNEEDED = RGNSIZE * 2
;
        MOVE.L  BUFHANDLE(A6),A4                ;GET BUFHANDLE
        MOVE.L  RGNHANDLE(A6),A3                ;GET RGNHANDLE
        MOVE.L  INDEX(A6),A2                    ;POINT TO CURRENT INDEX
        MOVE.L  (A3),A0                         ;DE-REFERENCE RGN
        MOVE    (A0),D7                         ;GET RGNSIZE
        ADD     D7,D7                           ;TIMES 2
        ADD     (A2),D7                         ;ADD CURRENT INDEX
        MOVE.L  SIZE(A6),A1                     ;POINT TO SIZE
        CMP     (A1),D7                         ;IS REQUIRED > CURRENT SIZE ?
        BLE.S   NOGROW                          ;NO, CONTINUE

        ADD     #256,D7                         ;GROW IN CHUNKS
        MOVE    D7,(A1)                         ;UPDATE CURRENT SIZE
        MOVE.L  A4,-(SP)                        ;PUSH BUFHANDLE
        MOVE    D7,-(SP)                        ;PUSH NEW SIZE
        JSR     SETSIZE                         ;MAKE ROOM IN BUF
        MOVE.L  (A3),A0                         ;RE-DEREFERENCE RGNHANDLE
        MOVE.L  INDEX(A6),A2                    ;GET ADDR OF INDEX AGAIN

NOGROW  MOVE.L  (A4),A1                         ;DE-REFERENCE BUFHANDLE
        ADD     (A2),A1                         ;ADD INDEX TO BUFPTR
        CMP     #10,RGNSIZE(A0)                 ;IS REGION RECTANGULAR ?
        BNE.S   NOTRECT                         ;NO, CONTINUE
        ADD     #2,A0                           ;YES, POINT TO BBOX TOPLEFT
        MOVE.L  (A0)+,(A1)+                     ;COPY TOPLEFT
        MOVE.L  (A0)+,(A1)+                     ;COPY BOTRIGHT
        BRA.S   DONE                            ;UPDATE INDEX AND QUIT

NOTRECT LEA     RGNDATA(A0),A0                  ;POINT TO TOP VERT IN RGN
NXTVERT MOVE.L  (A0)+,D7                        ;GET VERT AND HORIZ COORDS
NXTHOR  MOVE.L  D7,(A1)+                        ;PUT LEFT POINT TO DST
        MOVE    (A0)+,D7                        ;GET HORIZ COORD
        MOVE.L  D7,(A1)+                        ;PUT RIGHT POINT TO DST
        MOVE    (A0)+,D7                        ;GET NEXT HORIZ COORD
        CMP     #32767,D7                       ;END OF SCAN FLAG ?
        BNE     NXTHOR                          ;NO, GO FOR MORE
        CMP     #32767,(A0)                     ;END OF REGION ?
        BNE     NXTVERT                         ;NO, LOOP FOR MORE

DONE    SUB.L   (A4),A1                         ;CALC DSTPTR - DSTSTART
        MOVE    A1,(A2)                         ;UPDATE VAR INDEX
GOHOME  MOVEM.L (SP)+,D7/A2-A4               ;RESTORE REGISTERS
        UNLINK  PARAMSIZE,'PUTRGN  '




        .END
