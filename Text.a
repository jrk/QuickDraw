        .INCLUDE GRAFTYPES.TEXT
;---------------------------------------------------------
;
;
;     *****  *****  *   *  *****
;       *    *      *   *    *
;       *    *       * *     *
;       *    ***      *      *
;       *    *       * *     *
;       *    *      *   *    *
;       *    *****  *   *    *
;
;
;  Routines for measuring and drawing Text.
;


;-------------------------------------------
;
;  KERNED STRIKE FONT FORMAT OFFSETS:
;
FORMAT          .EQU    0               ;WORD
MINCHAR         .EQU    2               ;WORD
MAXCHAR         .EQU    4               ;WORD
MAXWD           .EQU    6               ;WORD
FBBOX           .EQU    8               ;WORD
FBBOY           .EQU    10              ;WORD
FBBDX           .EQU    12              ;WORD
FBBDY           .EQU    14              ;WORD
LENGTH          .EQU    16              ;WORD
ASCENT          .EQU    18              ;WORD
DESCENT         .EQU    20              ;WORD
XOFFSET         .EQU    22              ;WORD
RASTER          .EQU    24              ;WORD




        .PROC StdText,4
        .REF  CheckPic,DPutPicByte,PutPicData,PutPicWord,PutPicLong
        .REF  DrText
;--------------------------------------------------------------------------
;
;  PROCEDURE StdText(count: INTEGER; textAddr: Ptr; numer,denom: Point);
;
;  A6 OFFSETS OF PARAMS AND LOCALS AFTER LINK:
;
PARAMSIZE       .EQU    14
COUNT           .EQU    PARAMSIZE+8-2           ;WORD
TEXTADDR        .EQU    COUNT-4                 ;LONG
NUMER           .EQU    TEXTADDR-4              ;POINT
DENOM           .EQU    NUMER-4                 ;POINT

TXLOC           .EQU    -4                      ;POINT
VARSIZE         .EQU    TXLOC                   ;TOTAL LOCALS

        LINK    A6,#VARSIZE                     ;ALLOCATE STACK FRAME
        MOVEM.L D5-D7/A3-A4,-(SP)               ;SAVE REGS
TXTLOOP MOVE    COUNT(A6),D6                    ;GET CHARACTER COUNT
        BLE     GOHOME                          ;QUIT IF COUNT <= 0
        CMP     #255,D6                         ;is count > 255 ?
        BLE.S   COUNTOK                         ;no, continue
        MOVE    #255,D6                         ;yes, pin at 255

COUNTOK JSR     CHECKPIC                        ;SET UP A4,A3 AND CHECK PICSAVE
        BLE     NOTPIC
        MOVE.L  PICSAVE(A3),A4                  ;GET PICSAVE HANDLE
        MOVE.L  (A4),A4                         ;DE-REFERENCE PICSAVE
;
;  CHECK TXFONT
;
        MOVE    TXFONT(A3),D7                   ;GET THEPORT^.TXFONT
        CMP     PICTXFONT(A4),D7                ;HAS IT CHANGED ?
        BEQ.S   FONTOK                          ;NO, CONTINUE
        MOVEQ   #3,D0                           ;YES, PUSH TXFONT PARAM OPCODE
        JSR     DPutPicByte                     ;PUT OPCODE
        MOVE    D7,-(SP)
        JSR     PutPicWord                      ;PUT TXFONT PARAM
        MOVE.L  PICSAVE(A3),A4                  ;GET PICSAVE HANDLE
        MOVE.L  (A4),A4                         ;DE-REFERENCE PICSAVE
        MOVE    D7,PICTXFONT(A4)                ;UPDATE CURRENT STATE
;
;  CHECK TXFACE
;
FONTOK  MOVE.B  TXFACE(A3),D7                   ;GET THEPORT^.TXFACE
        CMP.B   PICTXFACE(A4),D7                ;HAS IT CHANGED ?
        BEQ.S   FACEOK                          ;NO, CONTINUE
        MOVEQ   #4,D0                           ;YES, PUSH TXFACE PARAM OPCODE
        JSR     DPutPicByte                     ;PUT OPCODE
        MOVE.B  D7,D0
        JSR     DPutPicByte                     ;PUT TXFACE PARAM
        MOVE.L  PICSAVE(A3),A4                  ;GET PICSAVE HANDLE
        MOVE.L  (A4),A4                         ;DE-REFERENCE PICSAVE
        MOVE.B  D7,PICTXFACE(A4)                ;UPDATE CURRENT STATE
;
;  CHECK TXMODE
;
FACEOK  MOVE    TXMODE(A3),D7                   ;GET THEPORT^.TXMODE
        CMP     PICTXMODE(A4),D7                ;HAS IT CHANGED ?
        BEQ.S   MODEOK                          ;NO, CONTINUE
        MOVEQ   #5,D0                           ;YES, PUSH TXMODE PARAM OPCODE
        JSR     DPutPicByte                     ;PUT OPCODE
        MOVE    D7,-(SP)
        JSR     PutPicWord                      ;PUT TXMODE PARAM
        MOVE.L  PICSAVE(A3),A4                  ;GET PICSAVE HANDLE
        MOVE.L  (A4),A4                         ;DE-REFERENCE PICSAVE
        MOVE    D7,PICTXMODE(A4)                ;UPDATE CURRENT STATE
;
;  CHECK TXSIZE
;
MODEOK  MOVE    TXSIZE(A3),D7                   ;GET THEPORT^.TXSIZE
        CMP     PICTXSIZE(A4),D7                ;HAS IT CHANGED ?
        BEQ.S   SIZEOK                          ;NO, CONTINUE
        MOVEQ   #$0D,D0                         ;YES, PUSH TXSIZE PARAM OPCODE
        BSR.S   JDPutPicByte                    ;PUT OPCODE
        MOVE    D7,-(SP)
        JSR     PutPicWord                      ;PUT TXSIZE PARAM
        MOVE.L  PICSAVE(A3),A4                  ;GET PICSAVE HANDLE
        MOVE.L  (A4),A4                         ;DE-REFERENCE PICSAVE
        MOVE    D7,PICTXSIZE(A4)                ;UPDATE CURRENT STATE
;
;  CHECK SPEXTRA
;
SIZEOK  MOVE.L  SPEXTRA(A3),D7                  ;GET THEPORT^.SPEXTRA
        CMP.L   PICSPEXTRA(A4),D7               ;HAS IT CHANGED ?
        BEQ.S   SPOK                            ;NO, CONTINUE
        MOVEQ   #6,D0                           ;YES, PUSH SPEXTRA PARAM OPCODE
        BSR.S   JDPutPicByte                    ;PUT OPCODE
        MOVE.L  D7,-(SP)
        BSR.S   JPutPicLong                     ;PUT SPEXTRA PARAM
        MOVE.L  PICSAVE(A3),A4                  ;GET PICSAVE HANDLE
        MOVE.L  (A4),A4                         ;DE-REFERENCE PICSAVE
        MOVE.L  D7,PICSPEXTRA(A4)               ;UPDATE CURRENT STATE
;
;  CHECK NUMER, DENOM
;
SPOK    MOVE.L  NUMER(A6),D7                    ;GET NUMER
        MOVE.L  DENOM(A6),D5                    ;GET DENOM
        CMP.L   PICTXNUMER(A4),D7               ;HAS IT CHANGED ?
        BNE.S   NOTSAME                         ;YES, RECORD CHANGE
        CMP.L   PICTXDENOM(A4),D5               ;HAS IT CHANGED ?
        BEQ.S   NUMEROK                         ;NO, CONTINUE
NOTSAME MOVEQ   #$10,D0                         ;YES, PUSH TXRATIO OPCODE
        BSR.S   JDPutPicByte                    ;PUT OPCODE
        MOVE.L  D7,-(SP)
        BSR.S   JPutPicLong                     ;PUT NUMER
        MOVE.L  D5,-(SP)
        BSR.S   JPutPicLong                     ;PUT DENOM
        MOVE.L  PICSAVE(A3),A4                  ;GET PICSAVE HANDLE
        MOVE.L  (A4),A4                         ;DE-REFERENCE PICSAVE
        MOVE.L  D7,PICTXNUMER(A4)               ;UPDATE CURRENT STATE
        MOVE.L  D5,PICTXDENOM(A4)               ;UPDATE CURRENT STATE
NUMEROK


;-------------------------------------------------------------
;
;  USE DH AND DV TO CHOOSE ONE OF FOUR TEXT OPCODES.
;
        MOVE.L  PNLOC(A3),D5                    ;GET CURRENT PNLOC
        SUB.L   PICTXLOC(A4),D5                 ;CALC DV.DH
        MOVE.L  D5,D0                           ;COPY DV.DH
        AND.L   #$FF00FF00,D0                   ;ARE DH AND DV BOTH 0..255 ?
        BEQ.S   SHORT                           ;YES, USE SHORT FORM
        MOVEQ   #$28,D0
        BSR.S   JDPutPicByte                    ;NO, PUT LONGTEXT OPCODE
        MOVE.L  PNLOC(A3),-(SP)
        BSR.S   JPutPicLong                     ;PUT PNLOC 4 BYTES
        BRA.S   TEXT2                           ;AND CONTINUE
JDPutPicByte
        JMP     DPutPicByte
JPutPicLong
        JMP     PutPicLong

SHORT   MOVE.L  D5,D0                           ;YES, COPY DV.DH
        AND.L   #$00FF0000,D0                   ;IS DV = 0 ?
        BNE.S   DV                              ;NO, CONTINUE
        MOVEQ   #$29,D0
        BSR.S   JDPutPicByte                    ;YES, PUT DHTEXT OPCODE
        BRA.S   SHARE2                          ;SHARE COMMON CODE

DV      TST.B   D5                              ;IS DH = 0 ?
        BNE.S   DHDV                            ;NO, CONTINUE
        MOVEQ   #$2A,D0
        BSR.S   JDPutPicByte                    ;YES, PUT DVTEXT OPCODE
        BRA.S   SHARE1                          ;SHARE COMMON CODE

DHDV    MOVEQ   #$2B,D0
        BSR.S   JDPutPicByte                    ;PUT DHDVTEXT OPCODE
        MOVE.B  D5,D0
        BSR.S   JDPutPicByte                    ;PUT DH 0..255 TO PIC

SHARE1  SWAP    D5                              ;PUT DV IN LO WORD
SHARE2  MOVE.B  D5,D0
        BSR.S   JDPutPicByte                    ;PUT DH OR DV 0..255 TO PIC

TEXT2   MOVE.B  D6,D0
        BSR.S   JDPutPicByte                    ;PUT COUNT BYTE TO PIC
        MOVE.L  TEXTADDR(A6),-(SP)              ;PUSH ADDR OF TEXT
        MOVE    D6,-(SP)                        ;PUSH COUNT
        JSR     PutPicData                      ;PUT TEXT DATA
        MOVE.L  PICSAVE(A3),A4                  ;GET PICSAVE HANDLE
        MOVE.L  (A4),A4                         ;DE-REFERENCE PICSAVE
        MOVE.L  PNLOC(A3),PICTXLOC(A4)          ;UPDATE PICTXLOC STATE

;
;  DrText(count,textAddr,numer,denom);
;
NOTPIC  MOVE    D6,-(SP)                        ;PUSH COUNT
        MOVE.L  TEXTADDR(A6),-(SP)              ;PUSH TEXTADDR
        MOVE.L  NUMER(A6),-(SP)                 ;PUSH NUMER
        MOVE.L  DENOM(A6),-(SP)                 ;PUSH DENOM
        JSR     DrText                          ;DRAW THE TEXT

        SUB     D6,COUNT(A6)                    ;was count > 255 ?
        BLE.S   GOHOME                          ;no, quit
        MOVE.L  TEXTADDR(A6),A0                 ;yes, get old textaddr
        ADD     D6,A0                           ;offset for characters done
        MOVE.L  A0,TEXTADDR(A6)                 ;update textAddr
        BRA     TXTLOOP                         ;and loop for more

GOHOME  MOVEM.L (SP)+,D5-D7/A3-A4               ;RESTORE REGS
        UNLINK  PARAMSIZE,'STDTEXT '



        .PROC CallText,2
        .REF  STDTEXT
;---------------------------------------------------------------
;
;  PROCEDURE CallText(count: INTEGER; textAddr: Ptr);
;
        MOVE.L  (SP)+,A0                        ;POP RETURN ADDR
        MOVE.L  #$00010001,-(SP)                ;PUSH NUMER = (1,1)
        MOVE.L  (SP),-(SP)                      ;PUSH DENOM = (1,1)
        MOVE.L  A0,-(SP)                        ;RESTORE RETURN ADDR
        MOVE.L  GRAFGLOBALS(A5),A0              ;POINT TO QuickDraw GLOBALS
        MOVE.L  THEPORT(A0),A0                  ;GET CURRENT GRAFPORT
        MOVE.L  GRAFPROCS(A0),D0                ;IS GRAFPROCS NIL ?
        LEA     STDTEXT,A0
        BEQ.S   USESTD                          ;YES, USE STD PROC
        MOVE.L  D0,A0
        MOVE.L  TEXTPROC(A0),A0                 ;NO, GET PROC PTR
USESTD  JMP     (A0)                            ;GO TO IT



        .PROC TextFace,1
        .DEF  DrawChar,CharWidth
        .REF  CallText,TextWidth
;-------------------------------------------------------
;
;  PROCEDURE TextFace(face: Style);
;
        MOVE.L  GRAFGLOBALS(A5),A0              ;POINT TO QuickDraw GLOBALS
        MOVE.L  THEPORT(A0),A0                  ;POINT TO THEPORT
        MOVE.B  5(SP),TXFACE(A0)                ;INSTALL TXFACE
        BRA.S   SHARE                           ;STRIP PARAM AND RETURN


;----------------------------------------------------
;
;  PROCEDURE DrawChar(ch: CHAR);
;
DrawChar
        MOVE    #1,-(SP)                        ;PUSH COUNT=1
        PEA     7(SP)                           ;PUSH TEXTADDR
        JSR     CallText                        ;CALL TEXT ROUTINE
        BRA.S   SHARE                           ;STRIP PARAM AND RETURN


;---------------------------------------------
;
;   FUNCTION CharWidth(ch: CHAR): INTEGER;
;
CharWidth
        CLR     -(SP)                           ;ROOM FOR FCN RESULT
        MOVE.L  SP,-(SP)                        ;PUSH TEXTBUF
        MOVE.L  #$00010007,-(SP)                ;PUSH OFFSET = 7 & COUNT = 1
        JSR     TEXTWIDTH
        MOVE    (SP)+,6(SP)                     ;MOVE UP RESULT
SHARE   MOVE.L  (SP)+,A0                        ;POP RETURN ADDR
        ADD     #2,SP                           ;STRIP CHAR PARAM
        JMP     (A0)                            ;AND RETURN



        .PROC TextFont,1
        .DEF  TextSize
        .DEF  TextMode
        .REF  PortWord
;-------------------------------------------------------
;
;  PROCEDURE TextFont(font: INTEGER);
;
        MOVEQ   #TXFONT,D0                      ;PUT PORT OFFSET IN D0
        BRA.S   SHARE


;-------------------------------------------------------
;
;  PROCEDURE TextMode(mode: INTEGER);
;
TextMode
        MOVEQ   #TXMODE,D0                      ;PUT PORT OFFSET IN D0
        BRA.S   SHARE


;-------------------------------------------------------
;
;  PROCEDURE TextSize(mode: INTEGER);
;
TextSize
        MOVEQ   #TXSIZE,D0                      ;PUT PORT OFFSET IN D0
SHARE   JMP     PORTWORD                        ;INSTALL PARAM INTO THEPORT


        .PROC SpaceExtra,1
        .DEF  DrawString,DrawText
        .REF  CallText
;-------------------------------------------------------
;
;  PROCEDURE SpaceExtra(extra: LongInt);
;
        MOVE.L  GRAFGLOBALS(A5),A0              ;POINT TO QuickDraw GLOBALS
        MOVE.L  THEPORT(A0),A0                  ;POINT TO THEPORT
        MOVE.L  4(SP),SPEXTRA(A0)               ;INSTALL FIXED POINT SPEXTRA
        BRA.S   SHARE


;----------------------------------------------------
;
;  PROCEDURE DrawString(s: Str255);
;
DrawString
        MOVE.L  4(SP),A0                        ;POINT TO STRING
        CLR     D0                              ;GET READY FOR BYTE
        MOVE.B  (A0)+,D0                        ;GET STRING LENGTH
        MOVE    D0,-(SP)                        ;PUSH COUNT
        MOVE.L  A0,-(SP)                        ;PUSH TEXTADDR
        JSR     CallText                        ;CALL TEXT ROUTINE
        BRA.S   SHARE


;----------------------------------------------------
;
;  PROCEDURE DrawText(textBuf: WordPtr; start,count: INTEGER);
;
DrawText
        MOVE.L  8(SP),A0                        ;POINT TO TEXTBUF
        ADD     6(SP),A0                        ;ADD STARTING OFFSET
        MOVE    4(SP),-(SP)                     ;PUSH COUNT
        MOVE.L  A0,-(SP)                        ;PUSH TEXTADDR
        JSR     CallText                        ;CALL TEXT ROUTINE
        MOVE.L  (SP)+,(SP)
SHARE   MOVE.L  (SP)+,(SP)                      ;STRIP PARAMS
        RTS                                     ;AND RETURN


        .FUNC   StringWidth,1
        .REF    TextWidth
;---------------------------------------------
;
;   FUNCTION StringWidth(s: Str255): INTEGER;
;
        MOVE.L  (SP)+,A1                        ;POP RETURN ADDR
        MOVE.L  (SP)+,A0                        ;POP ADDR OF STRING
        CLR     D0
        MOVE.B  (A0)+,D0                        ;GET UNSIGNED BYTE
        MOVE.L  A0,-(SP)                        ;PUSH TEXTADDR
        CLR     -(SP)                           ;FIRSTBYTE := 0
        MOVE    D0,-(SP)                        ;PUSH BYTECOUNT
        MOVE.L  A1,-(SP)                        ;PUT BACK RETURN ADDR
;
;  FALL THRU INTO TEXTWIDTH
;
        .FUNC   TextWidth,3
        .REF    StdTxMeas
;------------------------------------------
;
;  FUNCTION TEXTWIDTH(TEXTBUF: WordPtr; firstbyte,byteCount: INTEGER): INTEGER;
;
PARAMSIZE       .EQU    8
RESULT          .EQU    PARAMSIZE+8
TEXTBUF         .EQU    RESULT-4                ;LONG
FIRSTBYTE       .EQU    TEXTBUF-2               ;WORD
BYTECOUNT       .EQU    FIRSTBYTE-2             ;WORD

INFO            .EQU    -8                      ;4 WORDS
NUMER           .EQU    INFO-4                  ;POINT
DENOM           .EQU    NUMER-4                 ;POINT
VARSIZE         .EQU    DENOM                   ;TOTAL BYTES OF LOCALS


        LINK    A6,#VARSIZE                     ;ALLOCATE STACK FRAME
        CLR     RESULT(A6)                      ;INIT RESULT TO 0
        CLR     -(SP)                           ;MAKE ROOM FOR FCN CALL BELOW
        MOVE    BYTECOUNT(A6),-(SP)             ;PUSH BYTE COUNT
        BLE.S   GOHOME                          ;QUIT IF COUNT <= 0
                                                ;UNLK TAKES CARE OF SP
        MOVE.L  TEXTBUF(A6),A0                  ;GET ADDR OF BUFFER
        ADD     FIRSTBYTE(A6),A0                ;ADD STARTING INDEX
        MOVE.L  A0,-(SP)                        ;PUSH TEXTADDR
        MOVE.L  #$00010001,D0
        MOVE.L  D0,NUMER(A6)                    ;NUMER := (1,1)
        MOVE.L  D0,DENOM(A6)                    ;DENOM := (1,1)
        PEA     NUMER(A6)                       ;PUSH VAR NUMER
        PEA     DENOM(A6)                       ;PUSH VAR DENOM
        PEA     INFO(A6)                        ;PUSH VAR INFO
        MOVE.L  GRAFGLOBALS(A5),A0              ;POINT TO QUICKDRAW GLOBALS
        MOVE.L  THEPORT(A0),A0                  ;GET CURRENT GRAFPORT
        MOVE.L  GRAFPROCS(A0),D0                ;IS GRAFPROCS NIL ?
        BEQ.S   STD                             ;YES, USE STDTXMEAS
        MOVE.L  D0,A0                           ;NO, GET GRAFPROCS
        MOVE.L  TXMEASPROC(A0),A0               ;GET TXMEAS CAPTURE PROC
        JSR     (A0)                            ;CALL IT
        BRA.S   NOTSTD                          ;AND CONTINUE
STD     JSR     STDTXMEAS
NOTSTD  MOVE    (SP)+,D1                        ;POP UNSCALED WIDTH
        MOVE    NUMER+H(A6),D0                  ;get numer
        MOVE    DENOM+H(A6),D2                  ;get denom
        CMP     D2,D0                           ;is numer same as denom ?
        BEQ.S   DONE                            ;yes, skip muldiv
        MULU    D0,D1                           ;MUL BY NUMER
        MOVE    D2,D0                           ;COPY DENOM
        LSR     #1,D0                           ;CALC DENOM DIV 2
        ADD     D0,D1                           ;ADD DENOM DIV 2
        DIVU    D2,D1                           ;DIV BY DENOM
DONE    MOVE    D1,RESULT(A6)                   ;RETURN SCALED WIDTH
GOHOME  UNLINK  PARAMSIZE,'TEXTWIDT'




        .FUNC   StdTxMeas,5
;------------------------------------------
;
;  FUNCTION  StdTxMeas(count: INTEGER; textAddr: Ptr;
;                      VAR numer,denom: Point;
;                      VAR info: FontInfo): INTEGER;
;
;  Measure some text, returning unscaled values plus updated scale factor.
;  Fills info record with unscaled ascent, descent, widMax, and leading,
;  and returns unscaled integer width as the function value.
;
;  Also leaves unscaled fixed point width in QD global 'fixTxWid'
;  and stashes FMOutPtr in QD global 'fontPtr' for DrawText.
;
PARAMSIZE       .EQU    18
RESULT          .EQU    PARAMSIZE+8             ;FCN RESULT IS A WORD
COUNT           .EQU    RESULT-2                ;WORD
TEXTADDR        .EQU    COUNT-4                 ;LONG
NUMER           .EQU    TEXTADDR-4              ;LONG, VAR ADDR
DENOM           .EQU    NUMER-4                 ;LONG, VAR ADDR
INFO            .EQU    DENOM-4                 ;LONG, ADDR OF FONTINFO

INREC           .EQU    -16                     ;FMInput record
VARSIZE         .EQU    INREC


        LINK    A6,#VARSIZE                     ;ALLOCATE LOCALS
        MOVE.L  A4,-(SP)                        ;SAVE REG
        MOVE.L  GRAFGLOBALS(A5),A4              ;POINT TO QUICKDRAW GLOBALS
        MOVE.L  THEPORT(A4),A0                  ;GET CURRENT GRAFPORT
        LEA     INREC(A6),A1                    ;POINT TO FMINPUT RECORD
        MOVE    TXFONT(A0),(A1)+                ;GET TXFONT FROM THEPORT
        MOVE    TXSIZE(A0),(A1)+                ;GET TXSIZE FROM THEPORT
        MOVE.B  TXFACE(A0),(A1)+                ;GET TXFACE FROM THEPORT
        ST      (A1)+                           ;ALWAYS SET NEEDBITS TRUE
        MOVE    DEVICE(A0),(A1)+                ;GET DEVICE FROM THEPORT
        MOVE.L  NUMER(A6),A0                    ;POINT TO NUMER
        MOVE.L  (A0),(A1)+                      ;INSTALL INPUT NUMER
        MOVE.L  DENOM(A6),A0                    ;POINT TO DENOM
        MOVE.L  (A0),(A1)+                      ;INSTALL INPUT DENOM
        CLR.L   -(SP)                           ;ROOM FOR FCN RESULT
        PEA     INREC(A6)                       ;PUSH INPUT RECORD
        _SwapFont                               ;CALL FMSWAPFONT
        MOVE.L  (SP)+,A1                        ;POP FMOUTPUT POINTER
        MOVE.L  A1,FONTPTR(A4)                  ;STASH FMOUTPTR FOR LATER

        MOVE.L  INFO(A6),A0                     ;POINT TO VAR INFO RECORD
        CLR.L   (A0)                            ;INIT TO (0,0,0,0)
        CLR.L   4(A0)                           ;ALL 4 WORDS
        MOVE.B  13(A1),1(A0)                    ;FILL IN UNSIGNED ASCENT
        MOVE.B  14(A1),3(A0)                    ;FILL IN UNSIGNED DESCENT
        MOVE.B  15(A1),5(A0)                    ;FILL IN UNSIGNED WIDMAX
        MOVE.B  16(A1),D0                       ;GET SIGNED LEADING
        EXT.W   D0                              ;SIGN EXTEND TO WORD
        MOVE.W  D0,6(A0)                        ;FILL IN LEADING
;
;  UPDATE NUMER AND DENOM
;
        MOVE.L  NUMER(A6),A0                    ;GET VAR ADDR
        MOVE.L  18(A1),(A0)                     ;UPDATE NUMER
        MOVE.L  DENOM(A6),A0                    ;GET VAR ADDR
        MOVE.L  22(A1),(A0)                     ;UPDATE DENOM

        MOVE.L  TEXTADDR(A6),A0                 ;POINT TO CHARACTERS
        MOVE.L  WidthPtr,A1                     ;POINT TO WIDTH TABLE
        CLR.L   D1                              ;INIT WIDTH TO 0.0
        MOVE    COUNT(A6),D2                    ;GET CHARACTER COUNT
        BRA.S   MORE                            ;GO TO LOOP START
NEXTCH  CLR     D0                              ;GET READY FOR BYTE
        MOVE.B  (A0)+,D0                        ;GET A CHARACTER
        LSL     #2,D0                           ;QUAD FOR TABLE OFFSET
        ADD.L   0(A1,D0),D1                     ;ADD FIXED POINT WIDTH
MORE    DBRA    D2,NEXTCH                       ;LOOP FOR ALL CHARS
        MOVE.L  D1,fixTxWid(A4)                 ;STASH FIXED POINT WIDTH
        SWAP    D1                              ;GET HI WORD = INTEGER PORTION
        MOVE    D1,RESULT(A6)                   ;UPDATE FUNCTION RESULT
        MOVE.L (SP)+,A4                         ;RESTORE REG
        UNLINK  PARAMSIZE,'STDTXMEA'




        .PROC   MeasureText
;--------------------------------------------------------------------
;
;  PROCEDURE MeasureText(count: INTEGER; textAddr,charLocs: Ptr);
;
;  Measure some text, returning (scaled) screen widths in charlocs.
;
;  Charlocs points to an array of count+1 integers.
;
PARAMSIZE       .EQU    10
COUNT           .EQU    PARAMSIZE+8-2           ;WORD
TEXTADDR        .EQU    COUNT-4                 ;LONG, Ptr to ASCII
CHARLOCS        .EQU    TEXTADDR-4              ;LONG, Ptr to output array

INREC           .EQU    -16                     ;FMInput record
VARSIZE         .EQU    INREC


        LINK    A6,#VARSIZE                     ;ALLOCATE LOCALS
        MOVEM.L D3-D4/A2,-(SP)                  ;SAVE REGS
        MOVE.L  GRAFGLOBALS(A5),A0              ;POINT TO QUICKDRAW GLOBALS
        MOVE.L  THEPORT(A0),A0                  ;GET CURRENT GRAFPORT
;
;  Call swapfont to set up width table and return numer,denom:
;
        LEA     INREC(A6),A1                    ;POINT TO FMINPUT RECORD
        MOVE    TXFONT(A0),(A1)+                ;GET TXFONT FROM THEPORT
        MOVE    TXSIZE(A0),(A1)+                ;GET TXSIZE FROM THEPORT
        MOVE.B  TXFACE(A0),(A1)+                ;GET TXFACE FROM THEPORT
        ST      (A1)+                           ;ALWAYS SET NEEDBITS TRUE
        MOVE    DEVICE(A0),(A1)+                ;GET DEVICE FROM THEPORT
        MOVE.L  #$00010001,(A1)+                ;INSTALL INPUT NUMER = 1,1
        MOVE.L  #$00010001,(A1)+                ;INSTALL INPUT DENOM = 1,1
        CLR.L   -(SP)                           ;ROOM FOR FCN RESULT
        PEA     INREC(A6)                       ;PUSH INPUT RECORD
        _SwapFont                               ;CALL FMSWAPFONT
        MOVE.L  (SP)+,A0                        ;POP FMOUTPUT POINTER
        MOVE.W  18+H(A0),D3                     ;GET NUMER.H
        MOVE.W  22+H(A0),D4                     ;GET DENOM.H

;
;  Step thru characters, adding up unscaled widths and storing in charLocs:
;
        MOVE.L  TEXTADDR(A6),A0                 ;POINT TO CHARACTERS
        MOVE.L  WidthPtr,A1                     ;POINT TO WIDTH TABLE
        MOVE.L  CHARLOCS(A6),A2                 ;POINT TO CHARLOCS
        CLR.L   D1                              ;INIT WIDTH TO 0.0
        MOVE    COUNT(A6),D2                    ;GET CHARACTER COUNT
NEXTCH  SWAP    D1                              ;GET HI WORD OF WIDTH
        MOVE.W  D1,(A2)+                        ;STORE IN CHARLOCS
        SWAP    D1                              ;RETURN WIDTH TO FIXED POINT
        CLR     D0                              ;GET READY FOR BYTE
        MOVE.B  (A0)+,D0                        ;GET A CHARACTER
        LSL     #2,D0                           ;QUAD FOR TABLE OFFSET
        ADD.L   0(A1,D0),D1                     ;ADD FIXED POINT WIDTH
MORE    DBRA    D2,NEXTCH                       ;LOOP FOR COUNT+1 CHARLOCS

;
;  if font is horizontally stretched, scale all widths accordingly
;
        CMP     D3,D4                           ;IS NUMER.H = DENOM.H ?
        BEQ.S   NOSCALE                         ;YES, SKIP SCALING
        MOVE    D4,D1                           ;COPY DENOM
        LSR     #1,D1                           ;CALC DENOM DIV 2
        MOVE.L  CHARLOCS(A6),A2                 ;NO, POINT TO CHARLOCS
        MOVE    COUNT(A6),D2                    ;GET CHARACTER COUNT
NEXTCH2 MOVE    (A2),D0                         ;GET CHARLOC
        MULU    D3,D0                           ;MUL BY NUMER
        ADD     D1,D0                           ;ADD DENOM DIV 2
        DIVU    D4,D0                           ;DIV BY DENOM
        MOVE    D0,(A2)+                        ;UPDATE CHARLOC
        DBRA    D2,NEXTCH2                      ;LOOP FOR COUNT+1 CHARLOCS
NOSCALE

        MOVEM.L(SP)+,D3-D4/A2                   ;RESTORE REGS
        UNLINK  PARAMSIZE,'MEASURET'




;--------------------------------------------------------------------
;
;  FUNCTION FMSwapFont(inRec: FMInput): FMOutPtr;
;
;  FMSwapFont is the only contact between QuickDraw and the Font Manager.
;  It swaps in the requested font and returns a pointer to an output record
;  telling how to use the font.  FMSwapFont is called from StdTxMeas
;  in response to DrawChar, DrawString, DrawText, CharWidth, StringWidth,
;  TextWidth, and GetFontInfo.
;
;  IF fontHandle returns as Nil (can't find the font), then:
;    1. The output record will be undefined except for errNum and fontHandle.
;    2. DrawString will neither draw the text nor bump the pen.
;    3. StringWidth will return 0.
;    4. GetFontInfo will return 0,0,0,0.
;
;
;     FMInput  = PACKED RECORD
;                  family:     INTEGER;     { i.e. Century               }
;                  size:       INTEGER;     { i.e. 12 point              }
;                  face:       Style;       { i.e. [bold,underlined]     }
;                  needBits:   BOOLEAN;     { do we need the bitmaps ?   }
;                  device:     INTEGER;     { i.e. 0 for screen          }
;                  numer:      Point;       { current drawing scale      }
;                  denom:      Point;       { current drawing scale      }
;                END;
;
;
;     FMOutPtr = ^FMOutPut;
;     FMOutput = PACKED RECORD
;                  errNum:     INTEGER;     { not used                   }
;                  fontHandle: Handle;      { handle to font             }
;                  bold:       Byte;        { how much to smear horiz    }
;                  italic:     Byte;        { how much to shear          }
;                  ulOffset:   Byte;        { pixels below baseline      }
;                  ulShadow:   Byte;        { how big is the halo        }
;                  ulThick:    Byte;        { how thick is the underline }
;                  shadow:     Byte;        { 0,1,2,or 3 only            }
;                  extra:      SignedByte;  { extra white dots each char }
;                  ascent:     Byte;        { ascent measure for font    }
;                  descent:    Byte;        { descent measure for font   }
;                  widMax:     Byte;        { width of widest char       }
;                  leading:    SignedByte;  { leading between lines      }
;                  unused:     Byte;
;                  numer:      Point;       { use this modified scale to }
;                  denom:      Point;       { draw or measure text with  }
;                END;
;
;
;
;--------------------------------------------------------------------------


        .PROC GetFontInfo,1
        .REF  StdTxMeas
;------------------------------------------
;
;  PROCEDURE GetFontInfo(VAR info: FontInfo);
;
;  Calls StdTxMeas thru capture proc, then adjusts and scales the result.
;
;  13 MAY 85, changed so that all 4 values round UP in the case of scaling.
;
PARAMSIZE       .EQU    4
INFO            .EQU    PARAMSIZE+8-4           ;LONG, ADDR OF INFO

NUMER           .EQU    -4                      ;POINT
DENOM           .EQU    NUMER-4                 ;POINT
VARSIZE         .EQU    DENOM                   ;TOTAL LOCALS

        LINK    A6,#VARSIZE                     ;ALLOCATE LOCALS
        MOVE.L  #$00010001,NUMER(A6)            ;NUMER := (1,1)
        MOVE.L  #$00010001,DENOM(A6)            ;DENOM := (1,1)
        CLR.L   -(SP)                           ;ROOM FOR FCN, COUNT = 0
        CLR.L   -(SP)                           ;TEXTADDR := NIL
        PEA     NUMER(A6)                       ;PUSH VAR NUMER
        PEA     DENOM(A6)                       ;PUSH VAR DENOM
        MOVE.L  INFO(A6),-(SP)                  ;PUSH VAR INFO
        MOVE.L  GRAFGLOBALS(A5),A0              ;POINT TO QUICKDRAW GLOBALS
        MOVE.L  THEPORT(A0),A0                  ;GET CURRENT GRAFPORT
        MOVE.L  GRAFPROCS(A0),D0                ;IS GRAFPROCS NIL ?
        LEA     STDTXMEAS,A0
        BEQ.S   USESTD                          ;YES, USE STD PROC
        MOVE.L  D0,A0
        MOVE.L  TXMEASPROC(A0),A0               ;NO, GET TXMEAS CAPTURE PROC
USESTD  JSR     (A0)                            ;CALL IT
        TST     (SP)+                           ;DISCARD WIDTH FCN RSLT

;
;  ADJUST WIDMAX FOR EXTRA
;
        MOVE.L  GRAFGLOBALS(A5),A0              ;POINT TO QUICKDRAW GLOBALS
        MOVE.L  FONTPTR(A0),A0                  ;GET FMOUTPUT RECORD
        MOVE.L  INFO(A6),A1                     ;POINT TO RESULT INFO
        MOVE.B  12(A0),D0                       ;GET SIGNED EXTRA
        EXT.W   D0                              ;EXTEND TO WORD
        ADD     D0,4(A1)                        ;ADD TO WIDMAX
;
;  ADJUST ASCENT & DESCENT FOR SHADOW
;
        CLR     D0                              ;GET READY FOR BYTE
        MOVE.B  11(A0),D0                       ;GET SHADOW COUNT
        BEQ.S   NOTSHAD                         ;SKIP IF ZERO
        ADD     #1,0(A1)                        ;ADJUST ASCENT
        ADD     D0,2(A1)                        ;ADJUST DESCENT
NOTSHAD

;
; SCALE RESULT IF NUMER <> DENOM
;
        MOVE.L  NUMER(A6),D0
        CMP.L   DENOM(A6),D0                    ;IS NUMER SAME AS DENOM ?
        BEQ.S   NOSCALE                         ;YES, SKIP SCALING

        BSR.S   SCALE                           ;SCALE ASCENT

        BSR.S   SCALE                           ;SCALE DESCENT

        MOVE    (A1),D0                         ;GET MAXWID
        MULU    NUMER+H(A6),D0                  ;SCALE MAXWID
        MOVE    DENOM+H(A6),D1                  ;get denom
        SUB     #1,D1                           ;calc denom-1
        EXT.L   D1                              ;extend to long
        ADD.L   D1,D0                           ;add denom-1 to round up
        DIVU    DENOM+H(A6),D0                  ;divide by denom
        MOVE    D0,(A1)+                        ;UPDATE MAXWID

        BSR.S   SCALE                           ;SCALE LEADING

NOSCALE UNLINK  PARAMSIZE,'GETFONTI'


SCALE   MOVE    (A1),D0                         ;GET IT
        MULU    NUMER+V(A6),D0                  ;SCALE IT
        MOVE    DENOM+V(A6),D1                  ;get denom
        SUB     #1,D1                           ;calc denom-1
        EXT.L   D1                              ;extend to long
        ADD.L   D1,D0                           ;add denom-1 to round up
        DIVU    DENOM+V(A6),D0                  ;divide by denom
        MOVE    D0,(A1)+                        ;UPDATE IT
        RTS


        .END
