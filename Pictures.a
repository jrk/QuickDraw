        .INCLUDE GRAFTYPES.TEXT
;-----------------------------------------------------------
;
;
;     ****   ***   ***   *****  *   *  ****   *****   ***
;     *   *   *   *   *    *    *   *  *   *  *      *   *
;     *   *   *   *        *    *   *  *   *  *      *
;     ****    *   *        *    *   *  ****   ***     ***
;     *       *   *        *    *   *  * *    *          *
;     *       *   *   *    *    *   *  *  *   *      *   *
;     *      ***   ***     *     ***   *   *  *****   ***
;
;


        .PROC StdComment,3
        .REF  DPutPicByte,PutPicWord,PutPicData
;------------------------------------------------------------------
;
;  PROCEDURE StdComment(kind,dataSize: INTEGER; dataHandle: Handle);
;
;  A6 OFFSETS OF PARAMS AFTER LINK:
;
PARAMSIZE       .EQU    8
KIND            .EQU    PARAMSIZE+8-2           ;WORD
DATASIZE        .EQU    KIND-2                  ;WORD
DATAHANDLE      .EQU    DATASIZE-4              ;LONG, HANDLE

        LINK    A6,#0                           ;NO LOCAL VARS
        MOVEM.L D6-D7,-(SP)                     ;SAVE REGS
        MOVE.L  GRAFGLOBALS(A5),A0              ;POINT TO QUICKDRAW GLOBALS
        MOVE.L  THEPORT(A0),A0                  ;GET CURRENT GRAFPORT
        TST.L   PICSAVE(A0)                     ;ARE WE SAVING FOR THEPIC ?
        BEQ.S   DONE                            ;NO, QUIT

        MOVE    KIND(A6),D6                     ;YES, GET KIND
        MOVE    DATASIZE(A6),D7                 ;IS DATASIZE > 0 ?
        BGT.S   LONG                            ;YES, USE LONG FORMAT
;
;  DATASIZE 0, USE SHORT FORMAT
;
        MOVEQ   #$FFFFFFA0,D0
        JSR     DPutPicByte                     ;PUT SHORT COMMENT OPCODE
        MOVE    D6,-(SP)
        JSR     PutPicWord                      ;PUT KIND
        BRA.S   DONE
;
;  DATASIZE > 0, USE LONG FORMAT
;
LONG    MOVEQ   #$FFFFFFA1,D0
        JSR     DPutPicByte                     ;PUT LONG COMMENT OPCODE
        MOVE    D6,-(SP)
        JSR     PutPicWord                      ;PUT KIND
        MOVE    D7,-(SP)
        JSR     PutPicWord                      ;PUT DATASIZE
        MOVE.L  DATAHANDLE(A6),A0               ;GET DATA HANDLE
        _HLock                                  ;LOCK IT
        MOVE.L  DATAHANDLE(A6),A0               ;GET DATA HANDLE
        MOVE.L  (A0),-(SP)                      ;PUSH DATAPTR
        MOVE    D7,-(SP)                        ;PUSH BYTECOUNT
        JSR     PutPicData                      ;PUT DATA TO THEPIC
        MOVE.L  DATAHANDLE(A6),A0               ;GET DATA HANDLE
        _HUnlock                                ;UNLOCK IT

DONE    MOVEM.L (SP)+,D6-D7                     ;RESTORE REGS
        UNLINK  PARAMSIZE,'STDCOMME'



        .FUNC StdGetPic,2
;------------------------------------------------------------------
;
;  PROCEDURE StdGetPic(dataPtr: QDPtr; byteCount: INTEGER);
;
        MOVE.L  (SP)+,A0                        ;POP RETURN ADDR
        MOVE    (SP)+,D1                        ;POP BYTECOUNT
        MOVE.L  (SP)+,A1                        ;POP DATAPTR
        MOVE.L  A0,-(SP)                        ;PUSH RETURN ADDR
        MOVE.L  GRAFGLOBALS(A5),A0              ;POINT TO QUICKDRAW GLOBALS
        MOVE.L  PLAYINDEX(A0),D0                ;GET PLAYINDEX
        EXT.L   D1                              ;EXTEND BYTECOUNT TO LONG
        ADD.L   D1,PLAYINDEX(A0)                ;BUMP PLAYINDEX
        MOVE.L  PLAYPIC(A0),A0                  ;GET PLAY PICHANDLE
        MOVE.L  (A0),A0                         ;DE-REFERENCE IT
        ADD.L   D0,A0                           ;ADD PLAYINDEX
        BRA.S   START                           ;GO TO LOOP START
NXTBYTE MOVE.B  (A0)+,(A1)+                     ;COPY ONE BYTE
START   DBRA    D1,NXTBYTE                      ;LOOP FOR ALL BYTES
        RTS                                     ;AND RETURN



        .PROC StdPutPic,1
        .REF  SetSize
;------------------------------------------------------------------
;
;  PROCEDURE StdPutPic(dataPtr: QDPtr; byteCount: INTEGER);
;
;  Append some picture bytes to a growing handle.
;
PARAMSIZE       .EQU    6
SRCPTR          .EQU    PARAMSIZE+8-4           ;LONG
BYTECOUNT       .EQU    SRCPTR-2                ;WORD

        LINK    A6,#0                           ;NO LOCALS
        MOVEM.L D7/A3-A4,-(SP)                  ;SAVE REGS
        MOVE.L  GRAFGLOBALS(A5),A4              ;POINT TO QUICKDRAW GLOBALS
        MOVE.L  THEPORT(A4),A4                  ;GET CURRENT GRAFPORT
        TST.L   PICSAVE(A4)                     ;ARE WE SAVING FOR A PICTURE ?
        BEQ.S   GOHOME                          ;NO, QUIT
        MOVE.L  PICSAVE(A4),A4                  ;YES, GET PICSAVE HANDLE
        MOVE.L  (A4),A1                         ;DE-REFERENCE PICSAVE HANDLE
        MOVE.L  PICINDEX(A1),D7                 ;GET CURRENT SIZE
        BEQ.S   GOHOME                          ;QUIT IF PICTURE IS ALREADY DEAD
        MOVE.L  THEPIC(A1),A3                   ;GET THEPIC HANDLE

        MOVE    BYTECOUNT(A6),D0                ;GET BYTES REQUESTED
        EXT.L   D0                              ;MAKE BYTECOUNT LONG
        ADD.L   D7,D0                           ;CALCULATE NEW SIZE
        MOVE.L  D0,PICINDEX(A1)                 ;UPDATE PICINDEX
        MOVE.L  (A3),A0                         ;DE-REFERENCE THEPIC
        MOVE.W  D0,PICSIZE(A0)                  ;PICSIZE := LO WORD OF PICINDEX
        CMP.L   PICMAX(A1),D0                   ;IS NEW SIZE > PICMAX ?
        BLE.S   SIZEOK                          ;NO, CONTINUE
;
;  time to grow the picture in chunks of at least 256 bytes
;
        ADD.L   #256,D0                         ;GROW PIC IN CHUNKS
        MOVE.L  D0,PICMAX(A1)                   ;UPDATE NEW PICMAX
        MOVE.L  A3,A0                           ;GET THEPIC HANDLE
        _SetHandleSize                          ;MAKE IT BIGGER
        MOVE.L  (A4),A1                         ;RE-DEREFERENCE PICSAVE HANDLE
        BEQ.S   SIZEOK                          ;CONTINUE IF NO ERROR
;
;  Failed to grow picture, so we will trim handle down to 10 bytes,
;  clear PicIndex, and set picSize to -1, PicFrame to (0,0,0,0).
;
        CLR.L   PICINDEX(A1)                    ;CLEAR PICINDEX AS DEAD FLAG
        MOVE.L  #10,D0                          ;BYTECOUNT = 10
        MOVE.L  A3,A0                           ;GET THEPIC HANDLE
        _SetHandleSize                          ;SHRINK PICTURE TO 10 BYTES
        MOVE.L  (A3),A0                         ;DE-REFERENCE PICHANDLE
        MOVE    #-1,(A0)+                       ;STUFF picSize = -1
        CLR.L   (A0)+                           ;stuff picFrame = (0,0,0,0)
        CLR.L   (A0)+                           ;all 8 bytes of picFrame
        BRA.S   GOHOME                          ;AND QUIT
;
;  now copy the data bytes into the picture:
;
SIZEOK  MOVE.L  THEPIC(A1),A1                   ;GET THEPIC HANDLE
        MOVE.L  (A1),A1                         ;DE-REFERENCE PICHANDLE
        ADD.L   D7,A1                           ;ADD OLDSIZE FOR DSTPTR
        MOVE.L  SRCPTR(A6),A0                   ;GET SRCPTR
        MOVE    BYTECOUNT(A6),D0                ;GET BYTECOUNT
        BRA.S   START                           ;GO TO LOOP START
NXTBYTE MOVE.B  (A0)+,(A1)+                     ;COPY A BYTE
START   DBRA    D0,NXTBYTE                      ;LOOP ALL BYTES

GOHOME  MOVEM.L (SP)+,D7/A3-A4                  ;RESTORE REGS
        UNLINK  PARAMSIZE,'STDPUTPI'



        .PROC PicComment,3
        .REF  StdComment
;------------------------------------------------------------------
;
;  PROCEDURE PicComment(kind,dataSize: INTEGER; dataHandle: Handle;
;
        MOVE.L  GRAFGLOBALS(A5),A0              ;POINT TO QUICKDRAW GLOBALS
        MOVE.L  THEPORT(A0),A0                  ;GET CURRENT GRAFPORT
        MOVE.L  GRAFPROCS(A0),D0                ;IS GRAFPROCS NIL ?
        LEA     STDCOMMENT,A0
        BEQ.S   USESTD                          ;YES, USE STDCOMMENT
        MOVE.L  D0,A0
        MOVE.L  COMMENTPROC(A0),A0              ;NO, GET PROC PTR
USESTD  JMP     (A0)                            ;AND JUMP TO IT



        .FUNC OpenPicture,1
        .REF  HidePen,NewRgn,PutPicWord,EqualRgn,ClipRect
;------------------------------------------------------------------
;
;  FUNCTION OpenPicture(picFrame: Rect): PicHandle;
;
;  A6 OFFSETS OF PARAMS AFTER LINK:
;
PARAMSIZE       .EQU    4
RESULT          .EQU    PARAMSIZE+8             ;LONG, PICHANDLE
RECT            .EQU    RESULT-4                ;LONG, ADDR OF RECT

        LINK    A6,#0                           ;NO LOCALS
        MOVEM.L A3-A4,-(SP)                     ;SAVE REGS
        CLR.L   RESULT(A6)                      ;INIT FCN RESULT TO NIL
        MOVE.L  GRAFGLOBALS(A5),A4              ;POINT TO QUICKDRAW GLOBALS
        MOVE.L  THEPORT(A4),A3                  ;GET CURRENT GRAFPORT
        TST.L   PICSAVE(A3)                     ;ARE WE ALREADY SAVING ?
        BNE     DONE                            ;YES, RETURN NIL AND QUIT
        JSR     HidePen                         ;NO, TURN OFF DRAWING
;
;  Abort OpenPicture if heap doesn't have at least 500 bytes.
;
        MOVE.L  #500,D0                         ;GET BYTE COUNT
        _NewHandle                              ;AT LEAST 500 BYTES IN THE HEAP ?
        BNE     DONE                            ;NO, RETURN NIL AND QUIT
        _DisposHandle                           ;YES, Discard test handle

;
;  If clipRgn = wideOpen, then SetClip(picFrame);
;
        CLR.B   -(SP)                           ;ROOM FOR FCN RESULT
        MOVE.L  CLIPRGN(A3),-(SP)               ;PUSH CLIPRGN
        MOVE.L  WIDEOPEN(A4),-(SP)              ;PUSH WIDEOPEN
        JSR     EQUALRGN                        ;COMPARE CLIPRGN AND WIDEOPEN
        TST.B   (SP)+                           ;ARE THEY EQUAL ?
        BEQ.S   CLIPOK                          ;NO, CONTINUE
        MOVE.L  RECT(A6),-(SP)                  ;YES, PUSH PICFRAME
        JSR     CLIPRECT                        ;AND CLIPRECT(PICFRAME)
CLIPOK
;
;  ALLOCATE PICSAVE RECORD
;
        MOVE.L  #PICSAVEREC,D0                  ;GET BYTE COUNT
        _NewHandle                              ;ALLOCATE PICSAVE RECORD
        MOVE.L  A0,A4                           ;GET RESULT HANDLE
        MOVE.L  A4,PICSAVE(A3)                  ;SAVE RESULT IN THEPORT
;
;  ALLOCATE PICCLIPRGN (leave on stack for now)
;
        CLR.L   -(SP)                           ;MAKE ROOM FOR FCN RESULT
        JSR     NEWRGN                          ;ALLOCATE A NEW REGION
;
;  ALLOCATE THEPIC PICHANDLE
;
        MOVE.L  #256,D0                         ;BYTE COUNT = 256
        _NewHandle                              ;ALLOCATE NEWHANDLE(256)
        MOVE.L  A0,A1                           ;GET THEPIC HANDLE
        MOVE.L  A1,RESULT(A6)                   ;PUT HANDLE IN FCN RESULT
;
;  NOW FILL THEPIC'S PICSIZE AND PICFRAME
;
        MOVE.L  (A4),A4                         ;DE-REFERENCE PICSAVE HANDLE
        MOVE.L  A1,(A4)+                        ;SAVE PICHANDLE IN THEPIC
        MOVE.L  (A1),A1                         ;DE-REFERENCE PICHANDLE
        MOVE    #10,(A1)+                       ;INSTALL PICSIZE = 10
        MOVE.L  RECT(A6),A0                     ;POINT TO PICFRAME PARAM
        MOVE.L  (A0)+,(A1)+                     ;COPY RECT INTO PICTURE
        MOVE.L  (A0)+,(A1)+
;
;  INIT STATE VARIABLES FOR PICTURE CAPTURE
;
        MOVE.L  #256,(A4)+                      ;PICMAX := 256;
        MOVE.L  #10,(A4)+                       ;PICINDEX := 10
        MOVE.L  (SP)+,(A4)+                     ;INSTALL PICCLIPRGN
        CLR.L   (A4)+                           ;PICBKPAT := WHITE
        CLR.L   (A4)+
        CLR.L   (A4)+                           ;PICTXFONT = 0, PICTXFACE = []
        MOVE    #1,(A4)+                        ;PICTXMODE := SRCCOPY
        CLR     (A4)+                           ;PICTXSIZE := 0
        CLR.L   (A4)+                           ;PICSPEXTRA := 0.0
        MOVE.L  #$00010001,D0                   ;GET (1,1)
        MOVE.L  D0,(A4)+                        ;PICTXNUMER := (1,1)
        MOVE.L  D0,(A4)+                        ;PICTXDENOM := (1,1)
        CLR.L   (A4)+                           ;PICTXLOC := (0,0)
        CLR.L   (A4)+                           ;PICPNLOC := (0,0)
        MOVE.L  D0,(A4)+                        ;PICPNSIZE := (1,1)
        MOVE    #8,(A4)+                        ;PICPNMODE := PATCOPY
        MOVEQ   #-1,D0                          ;GET SOME BLACK
        MOVE.L  D0,(A4)+                        ;PICPNPAT := BLACK
        MOVE.L  D0,(A4)+
        MOVE.L  D0,(A4)+                        ;PICFILLPAT := BLACK
        MOVE.L  D0,(A4)+
        CLR.L   (A4)+                           ;PICTHERECT := (0,0,0,0)
        CLR.L   (A4)+
        CLR.L   (A4)+                           ;PICOVSIZE := (0,0)
        MOVE.L  PORTRECT+TOPLEFT(A3),(A4)+      ;PICORIGIN := CURRENT ORIGIN
        MOVE.L  #blackColor,(A4)+               ;PICFGCOLOR := blackColor
        MOVE.L  #whiteColor,(A4)+               ;PICBKCOLOR := whiteColor
;
; put version number opcode
;
        MOVE.w  #$1101,-(SP)                    ;VERSION OPCODE + VERSION 1
        JSR     PutPicWord                      ;PUT TO PICTURE

DONE    MOVEM.L (SP)+,A3-A4                     ;RESTORE REGS
        UNLINK  PARAMSIZE,'OPENPICT'



        .PROC ClosePicture,0
        .REF  PutPicByte,ShowPen,SetSize
;------------------------------------------------------------------
;
;  PROCEDURE ClosePicture;
;
        MOVEM.L D6-D7/A3,-(SP)                  ;SAVE REGS
        MOVE.L  GRAFGLOBALS(A5),A3              ;POINT TO QUICKDRAW GLOBALS
        MOVE.L  THEPORT(A3),A3                  ;GET CURRENT GRAFPORT
        MOVE.L  PICSAVE(A3),D7                  ;ARE WE SAVING A PICTURE ?
        BEQ.S   GOHOME                          ;NO, OOPS, EXIT
        ST      -(SP)                           ;YES, PUSH ENDPIC OPCODE, $FF
        JSR     PutPicByte                      ;PUT TO THEPIC
        MOVE.L  D7,A0                           ;GET HANDLE TO PICSAVE RECORD
        MOVE.L  (A0),A0                         ;DE-REFERENCE IT
        MOVE.L  PICCLIPRGN(A0),D6               ;GET picClipRgn
        MOVE.L  PICINDEX(A0),D0                 ;DID PICTURE OVERFLOW ?
        BEQ.S   OVERFLO                         ;YES, CONTINUE
        MOVE.L  THEPIC(A0),A0                   ;NO, GET THEPIC HANDLE
        _SetHandleSize                          ;AND TRIM TO FINAL SIZE
OVERFLO MOVE.L  D6,A0                           ;GET PICCLIPRGN
        _DisposHandle                           ;DISCARD IT
        MOVE.L  D7,A0                           ;GET PICSAVE HANDLE
        _DisposHandle                           ;DISCARD IT
        CLR.L   PICSAVE(A3)                     ;RESET PICSAVE TO NIL
        JSR     SHOWPEN                         ;RESTORE DRAWING
GOHOME  MOVEM.L (SP)+,D6-D7/A3                  ;RESTORE REGS
        RTS                                     ;AND RETURN



        .PROC KillPicture,1
;---------------------------------------------------
;
;  PROCEDURE KillPicture(myPicture: PicHandle);
;
        MOVE.L  (SP)+,A1                        ;pop return addr
        MOVE.L  (SP)+,A0                        ;pop handle
        _DisposHandle                           ;discard it
        JMP     (A1)                            ;and return



        .PROC DrawPicture,2
        .REF  PicItem,NewRgn
;------------------------------------------------------------------
;
;  PROCEDURE DrawPicture(myPicture: PicHandle; dstRect: Rect);
;

;--------------------------------------------
;
;  OFFSETS WITHIN A PICTURE PLAY STATE RECORD:
;
THERECT         .EQU    0               ;RECT
PENLOC          .EQU    THERECT+8       ;POINT
TEXTLOC         .EQU    PENLOC+4        ;POINT
OVALSIZE        .EQU    TEXTLOC+4       ;POINT
FROMRECT        .EQU    OVALSIZE+4      ;RECT
TORECT          .EQU    FROMRECT+8      ;RECT
NUMER           .EQU    TORECT+8        ;POINT
DENOM           .EQU    NUMER+4         ;POINT
THECLIP         .EQU    DENOM+4         ;RGNHANDLE
USERCLIP        .EQU    THECLIP+4       ;RGNHANDLE
PLAYREC         .EQU    USERCLIP+4      ;TOTAL SIZE

;
;  A6 OFFSETS OF PARAMS AND LOCALS AFTER LINK:
;
PARAMSIZE       .EQU    8
MYPICTURE       .EQU    PARAMSIZE+8-4           ;LONG, PICHANDLE
DSTRECT         .EQU    MYPICTURE-4             ;LONG, ADDR OF RECT

PLAYSTATE       .EQU    -PLAYREC                ;PICTURE PLAY STATE RECORD
SAVEPORT        .EQU    PLAYSTATE-PORTREC       ;GRAFPORT RECORD
VARSIZE         .EQU    SAVEPORT                ;TOTAL BYTES OF LOCALS


        LINK    A6,#VARSIZE                     ;ALLOCATE LOCALS
        MOVEM.L D3-D7/A2-A4,-(SP)               ;SAVE REGISTERS
        MOVE.L  GRAFGLOBALS(A5),A4              ;POINT TO QUICKDRAW GLOBALS
        MOVE.L  THEPORT(A4),A3                  ;POINT TO CURRENT GRAFPORT
        TST.L   MYPICTURE(A6)                   ;IS PICHANDLE NIL ?
        BEQ     GOHOME                          ;YES, QUIT


;--------------------------------------------------
;
;  SET UP NUMER AND QUIT IF DSTRECT WIDTH OR HEIGHT IS <= 0
;  COPY DSTRECT INTO TORECT
;
        MOVE.L  DSTRECT(A6),A0                  ;POINT TO DSTRECT
        MOVE    RIGHT(A0),D0
        SUB     LEFT(A0),D0                     ;CALC DST WIDTH
        BLE     GOHOME                          ;QUIT IF WIDTH <= 0
        MOVE    D0,PLAYSTATE+NUMER+H(A6)        ;NUMER.H := DST WIDTH
        MOVE    BOTTOM(A0),D0
        SUB     TOP(A0),D0                      ;CALC DST HEIGHT
        BLE     GOHOME                          ;QUIT IF HEIGHT <= 0
        MOVE    D0,PLAYSTATE+NUMER+V(A6)        ;NUMER.V := DST HEIGHT
        LEA     PLAYSTATE+TORECT(A6),A1
        MOVE.L  (A0)+,(A1)+
        MOVE.L  (A0)+,(A1)+                     ;TORECT := DSTRECT


;--------------------------------------------------
;
;  SET UP DENOM AND QUIT IF PICFRAME WIDTH OR HEIGHT IS <= 0
;  COPY PICFRAME INTO FROMRECT.
;
        MOVE.L  MYPICTURE(A6),A0                ;GET PICHANDLE
        MOVE.L  (A0),A0                         ;DE-REFERENCE IT
        LEA     PICFRAME(A0),A0                 ;POINT TO PICTURE FRAME
        MOVE    RIGHT(A0),D0
        SUB     LEFT(A0),D0                     ;CALC SRC WIDTH
        BLE     GOHOME                          ;QUIT IF WIDTH <= 0
        MOVE    D0,PLAYSTATE+DENOM+H(A6)        ;DENOM.H := SRC WIDTH
        MOVE    BOTTOM(A0),D0
        SUB     TOP(A0),D0                      ;CALC SRC HEIGHT
        BLE     GOHOME                          ;QUIT IF HEIGHT <= 0
        MOVE    D0,PLAYSTATE+DENOM+V(A6)        ;DENOM.V := SRC HEIGHT
        LEA     PLAYSTATE+FROMRECT(A6),A1       ;POINT TO FROMRECT
        MOVE.L  (A0)+,(A1)+
        MOVE.L  (A0)+,(A1)+                     ;FROMRECT := PICFRAME


;---------------------------------------------------
;
;  PRESERVE THE CURRENT GRAFPORT IN SAVEPORT
;
        MOVE.L  A3,A0                           ;SRC = THEPORT
        LEA     SAVEPORT(A6),A1                 ;DST = SAVEPORT
        MOVEQ   #PORTREC/2-1,D0                 ;INIT DBRA COUNT
SAVELP  MOVE.W  (A0)+,(A1)+                     ;COPY A WORD
        DBRA    D0,SAVELP                       ;LOOP ENTIRE PORT


;----------------------------------------
;
;  INIT GLOBAL VARS:
;
        CLR.L   PATALIGN(A4)                    ;PATALIGN := (0,0)
        MOVE.L  MYPICTURE(A6),PLAYPIC(A4)       ;SAVE PICTURE FOR STDGETPIC
        MOVE.L  #PICDATA,PLAYINDEX(A4)          ;INIT INDEX TO FIRST OPCODE


;----------------------------------------
;
;  INIT PLAY STATE RECORD:
;
        LEA     PLAYSTATE(A6),A0
        CLR.L   (A0)+                           ;THERECT := (0,0,0,0)
        CLR.L   (A0)+
        CLR.L   (A0)+                           ;PENLOC := (0,0)
        CLR.L   (A0)+                           ;TEXTLOC := (0,0)
        CLR.L   (A0)+                           ;OVALSIZE := (0,0)
                                                ;FROMRECT SET UP
                                                ;TORECT SET UP
                                                ;NUMER SET UP
                                                ;DENOM SET UP


        MOVE.L  CLIPRGN(A3),PLAYSTATE+USERCLIP(A6) ;SAVE USER CLIPRGN

        CLR.L   -(SP)
        JSR     NEWRGN
        MOVE.L  (SP)+,PLAYSTATE+THECLIP(A6)     ;ALLOCATE THECLIP


;--------------------------------------------------------
;
;  INIT MOST FIELDS OF THEPORT
;
        CLR.L   -(SP)
        JSR     NEWRGN
        MOVE.L  (SP)+,CLIPRGN(A3)               ;ALLOCATE TEMP CLIPRGN
        LEA     BKPAT(A3),A0                    ;POINT TO BKPAT
        CLR.L   (A0)+                           ;BKPAT := WHITE
        CLR.L   (A0)+
        MOVEQ   #-1,D0                          ;GET SOME BLACK
        MOVE.L  D0,(A0)+                        ;fillPat := BLACK
        MOVE.L  D0,(A0)+
        CLR.L   (A0)+                           ;PNLOC := (0,0)
        MOVE.L  #$00010001,D1
        MOVE.L  D1,(A0)+                        ;pnSize := (1,1)
        MOVE    #8,(A0)+                        ;pnMode := patCopy
        MOVE.L  D0,(A0)+                        ;pnPat := black
        MOVE.L  D0,(A0)+
        ADD     #2,A0                           ;skip over pnVis
        CLR.L   (A0)+                           ;txFont, txFace := 0
        MOVE    #1,(A0)+                        ;txMode := srcOr
        CLR     (A0)+                           ;txSize := 0;
        CLR.L   (A0)+                           ;spExtra := 0.0;
        MOVE.L  #blackColor,(A0)+               ;FGCOLOR := blackColor
        MOVE.L  #whiteColor,(A0)+               ;BKCOLOR := whiteColor
                                                ;LEAVE COLRBIT ALONE
                                                ;LEAVE PATSTRETCH ALONE
                                                ;LEAVE PICSAVE ALONE
                                                ;LEAVE RGNSAVE ALONE
                                                ;LEAVE POLYSAVE ALONE
                                                ;LEAVE GRAFPROCS ALONE

;---------------------------------------------------
;
;  NOW DRAW THE PICTURE:
;  REPEAT UNTIL NOT PicItem(playState);
;
MORE    CLR.B   -(SP)                           ;MAKE ROOM FOR FCN RESULT
        PEA     PLAYSTATE(A6)                   ;PUSH ADDR OF PLAYSTATE
        JSR     PicItem                         ;DRAW ONE PICTURE ITEM
        MOVE.B  (SP)+,D0                        ;POP BOOLEAN RESULT
        BNE     MORE                            ;LOOP TILL FALSE


;-----------------------------------------------------
;
;  DISCARD HANDLES, RESTORE GRAFPORT STATE AND QUIT
;
DONE    MOVE.L  PLAYSTATE+THECLIP(A6),A0        ;GET THECLIP RGNHANDLE
        _DisposHandle                           ;DISCARD IT
        MOVE.L  CLIPRGN(A3),A0                  ;GET TEMPCLIP
        _DisposHandle                           ;DISCARD IT
        LEA     SAVEPORT(A6),A0                 ;SRC = SAVEPORT
        MOVEQ   #PORTREC/2-1,D0                 ;INIT DBRA COUNT
DONELP  MOVE.W  (A0)+,(A3)+                     ;COPY A WORD INTO THEPORT
        DBRA    D0,DONELP                       ;LOOP ENTIRE PORT
        CLR.L   PATALIGN(A4)                    ;RESTORE PATALIGN TO (0,0)
        CLR.L   PLAYPIC(A4)                     ;SET PLAYPIC TO NIL
        CLR.L   PLAYINDEX(A4)                   ;AND PLAYINDEX TO 0

GOHOME  MOVEM.L (SP)+,D3-D7/A2-A4               ;RESTORE REGISTERS
        UNLINK  PARAMSIZE,'DRAWPICT'




        .FUNC PicItem,1
        .REF  GetPicData,ScalePt,MapPt,MapRect,MapRgn,MapPoly
        .REF  NewRgn,CopyRgn,SectRgn,UnpackBits
        .REF  StdText,StdLine,StdRect,StdRRect,StdOval
        .REF  StdArc,StdPoly,StdRgn,StdBits,StdComment
;------------------------------------------------------------------
;
;  FUNCTION PicItem(VAR playState: PicPlayRec): BOOLEAN;
;
;  Draws one picture item, updating playState and thePort.
;  Returns FALSE when an endPic opCode is encountered.
;  The only state modified other than thePort and playState is patAlign.
;


;--------------------------------------------
;
;  OFFSETS WITHIN A PICTURE PLAY STATE RECORD:
;
THERECT         .EQU    0               ;RECT
PENLOC          .EQU    THERECT+8       ;POINT
TEXTLOC         .EQU    PENLOC+4        ;POINT
OVALSIZE        .EQU    TEXTLOC+4       ;POINT
FROMRECT        .EQU    OVALSIZE+4      ;RECT
TORECT          .EQU    FROMRECT+8      ;RECT
NUMER           .EQU    TORECT+8        ;POINT
DENOM           .EQU    NUMER+4         ;POINT
THECLIP         .EQU    DENOM+4         ;RGNHANDLE
USERCLIP        .EQU    THECLIP+4       ;RGNHANDLE
PLAYREC         .EQU    USERCLIP+4      ;TOTAL SIZE
;
;  params:
;
PARAMSIZE       .EQU    4
RESULT          .EQU    PARAMSIZE+8             ;BOOLEAN
PLAYSTATE       .EQU    RESULT-4                ;LONG, PICHANDLE
;
;  locals:
;
HANDLE1         .EQU    -4                      ;HANDLE
HANDLE2         .EQU    HANDLE1-4               ;HANDLE
DSTRECT         .EQU    HANDLE2-8               ;RECT
SRCRECT         .EQU    DSTRECT-8               ;RECT
SRCBITS         .EQU    SRCRECT-14              ;BITMAP, MUST BE BELOW SRCRECT
SAMEFLAG        .EQU    SRCBITS-2               ;BOOLEAN
NEWPT           .EQU    SAMEFLAG-4              ;LONG
TXDATA          .EQU    NEWPT-256               ;UP TO 256 CHARACTERS,
                                                ;ALSO USED FOR PACKBUF !!!
SRCPTR          .EQU    TXDATA-4                ;LONG
DSTPTR          .EQU    SRCPTR-4                ;LONG
SAVESP          .EQU    DSTPTR-4                ;LONG
VARSIZE         .EQU    SAVESP                  ;TOTAL BYTES OF LOCALS


        LINK    A6,#VARSIZE                     ;ALLOCATE LOCALS
        MOVEM.L D3-D7/A2-A4,-(SP)               ;SAVE REGISTERS
        MOVE.L  SP,SAVESP(A6)                   ;REMEMBER STACK FOR ABORT
        MOVE.L  GRAFGLOBALS(A5),A4              ;POINT TO QUICKDRAW GLOBALS
        MOVE.L  THEPORT(A4),A3                  ;POINT TO CURRENT GRAFPORT

;
;  GET PICTURE OPCODE AND CHECK FOR END OF PICTURE.
;
        BSR     GetUByte                        ;GET OPCODE BYTE
        MOVE    D0,D7                           ;PUT IT IN D7
        CMP.B   #$FF,D7                         ;IS THIS THE PICEND OPCODE ?
        BNE.S   NOTEND                          ;NO, CONTINUE
        CLR.B   RESULT(A6)                      ;YES, RETURN FALSE
        BRA     DONE                            ;AND QUIT
NOTEND  MOVE.B  #1,RESULT(A6)                   ;RETURN TRUE

;
;  CHECK FOR PARAM OPCODES $00..$1F
;
        CMP     #$20,D7                         ;IS IT A PARAM OPCODE ?
        BLO.S   PARAMOP                         ;YES, GO TO IT

;
;  GET LO AND HI NIBBLES OF OPCODE, AND CASE ON HI NIBBLE (NOUN).
;
        MOVE.B  D7,D0                           ;COPY OPCODE
        AND     #$F0,D0                         ;MASK FOR HI NIBBLE
        BTST    #3,D7                           ;IS OPCODE BIT 3 SET ?
        SNE     SAMEFLAG(A6)                    ;REMEMBER IN SAMEFLAG
        AND     #$7,D7                          ;GET VERB FROM LO NIBBLE
        LSR     #3,D0                           ;DOUBLE HI NIBBLE FOR INDEX
        MOVE    NOUNJMP(D0),D0                  ;GET JUMP OFFSET
        JMP     NOUNJMP(D0)                     ;TAKE CASE JUMP

NOUNJMP .WORD   DONE-NOUNJMP                    ;NEVER TAKEN
        .WORD   DONE-NOUNJMP                    ;NEVER TAKEN
        .WORD   TXLNOP-NOUNJMP
        .WORD   RECTOP-NOUNJMP
        .WORD   RRECTOP-NOUNJMP
        .WORD   OVALOP-NOUNJMP
        .WORD   ARCOP-NOUNJMP
        .WORD   POLYOP-NOUNJMP
        .WORD   RGNOP-NOUNJMP
        .WORD   BITSOP-NOUNJMP
        .WORD   COMMOP-NOUNJMP
        .WORD   DONE-NOUNJMP
        .WORD   DONE-NOUNJMP
        .WORD   DONE-NOUNJMP
        .WORD   DONE-NOUNJMP
        .WORD   DONE-NOUNJMP
        .WORD   DONE-NOUNJMP


;---------------------------------------------------
;
;  OPCODES $00..$1F DO NO DRAWING, THEY JUST SET PARAMETERS.
;
PARAMOP AND     #$1F,D7                         ;GET LO 5 BITS OF OPCODE
        ADD     D7,D7                           ;DOUBLE PARAM FOR CASE INDEX
        MOVE    PARMJMP(D7),D0                  ;GET CASE JUMP OFFSET
        JMP     PARMJMP(D0)                     ;TAKE CASE JUMP
PARMJMP .WORD   DONE-PARMJMP                    ;OPCODE 0 IS PURPOSELY A NOP
        .WORD   XCLIP-PARMJMP                   ;OPCODE $01
        .WORD   XBKPAT-PARMJMP
        .WORD   XTXFONT-PARMJMP
        .WORD   XTXFACE-PARMJMP
        .WORD   XTXMODE-PARMJMP
        .WORD   XSPXTRA-PARMJMP
        .WORD   XPNSIZE-PARMJMP
        .WORD   XPNMODE-PARMJMP
        .WORD   XPNPAT-PARMJMP
        .WORD   XFILLPAT-PARMJMP
        .WORD   XOVSIZE-PARMJMP
        .WORD   XORIGIN-PARMJMP
        .WORD   XTXSIZE-PARMJMP
        .WORD   XFGCOL-PARMJMP
        .WORD   XBKCOL-PARMJMP                  ;OPCODE $0F
        .WORD   TXRATIO-PARMJMP                 ;OPCODE $10
        .WORD   VERSION-PARMJMP                 ;OPCODE $11
        .WORD   DONE-PARMJMP
        .WORD   DONE-PARMJMP
        .WORD   DONE-PARMJMP
        .WORD   DONE-PARMJMP
        .WORD   DONE-PARMJMP
        .WORD   DONE-PARMJMP
        .WORD   DONE-PARMJMP
        .WORD   DONE-PARMJMP
        .WORD   DONE-PARMJMP
        .WORD   DONE-PARMJMP
        .WORD   DONE-PARMJMP
        .WORD   DONE-PARMJMP
        .WORD   DONE-PARMJMP
        .WORD   DONE-PARMJMP                    ;OPCODE $1F



XCLIP   BSR     GETHNDL                         ;COPY RGN INTO HANDLE1
        MOVE.L  PLAYSTATE(A6),A2                ;POINT TO PLAYSTATE RECORD
        MOVE.L  HANDLE1(A6),-(SP)               ;PUSH HANDLE1
        MOVE.L  THECLIP(A2),-(SP)               ;PUSH PLAYSTATE THECLIP
XCLIP2  JSR     COPYRGN                         ;COPY HANDLE1 INTO THECLIP
        MOVE.L  PLAYSTATE(A6),A2                ;POINT TO PLAYSTATE RECORD
        MOVE.L  HANDLE1(A6),-(SP)               ;PUSH HANDLE1 TEMP
        PEA     FROMRECT(A2)                    ;PUSH FROMRECT
        PEA     TORECT(A2)                      ;PUSH TORECT
        JSR     MAPRGN                          ;MAP RGN INTO DST COORDS
        MOVE.L  PLAYSTATE(A6),A2                ;POINT TO PLAYSTATE RECORD
        MOVE.L  HANDLE1(A6),-(SP)               ;PUSH MAPPED RGN
        MOVE.L  USERCLIP(A2),-(SP)              ;PUSH ORIGINAL CLIP
        MOVE.L  CLIPRGN(A3),-(SP)               ;PUSH DST = THEPORT^.CLIPRGN
        JSR     SECTRGN                         ;PUT INTERSECT INTO CLIPRGN
        BRA     KILL1                           ;DISCARD HANDLE1 AND QUIT


GET8    MOVEQ   #8,D6                           ;BYTECOUNT = 8
        BRA     GETDONE                         ;COPY 8 BYTES AND QUIT

GET4    MOVEQ   #4,D6                           ;BYTECOUNT = 4
        BRA     GETDONE                         ;COPY 4 BYTES AND QUIT

GET2    MOVEQ   #2,D6                           ;BYTECOUNT = 2
        BRA     GETDONE                         ;COPY 2 BYTES AND QUIT


XFGCOL  LEA     FGCOLOR(A3),A3
        BRA     GET4                            ;GET FOREGROUND COLOR

XBKCOL  LEA     BKCOLOR(A3),A3
        BRA     GET4                            ;GET BACKGROUND COLOR

XBKPAT  LEA     BKPAT(A3),A3
        BRA     GET8                            ;GET BKPAT

XTXFONT LEA     TXFONT(A3),A3
        BRA     GET2                            ;GET TXFONT

XTXFACE LEA     TXFACE(A3),A3
        MOVEQ   #1,D6
        BRA     GETDONE                         ;GET TXFACE

XTXMODE LEA     TXMODE(A3),A3
        BRA     GET2                            ;GET TXMODE

XTXSIZE LEA     TXSIZE(A3),A3
        BRA     GET2                            ;GET TXSIZE

XSPXTRA LEA     SPEXTRA(A3),A3
        BRA     GET4                            ;GET fixed point SPACE EXTRA

XPNSIZE JSR     GETLONG                         ;GET PNSIZE
        MOVE.L  D0,PNSIZE(A3)                   ;INSTALL INTO THEPORT
        PEA     PNSIZE(A3)
        MOVE.L  PLAYSTATE(A6),A2                ;POINT TO PLAYSTATE RECORD
        PEA     FROMRECT(A2)
        PEA     TORECT(A2)
        JSR     SCALEPT                         ;SCALE PNSIZE
        BRA     DONE

TXRATIO MOVE.L  PLAYSTATE(A6),A3               ;POINT TO PLAYSTATE RECORD
        JSR     GETLONG                        ;GET TEXT NUMER
        MOVE.L  D0,NUMER(A3)                   ;INSTALL INTO PLAYSTATE
        JSR     GETLONG                        ;GET TEXT DENOM
        MOVE.L  D0,DENOM(A3)                   ;INSTALL INTO PLAYSTATE
        PEA     NUMER(A3)
        PEA     FROMRECT(A3)
        PEA     TORECT(A3)
        JSR     SCALEPT                        ;SCALE NUMER
        BRA     DONE

VERSION BSR     GETUBYTE                        ;GET VERSION NUMBER BYTE
        BRA     DONE                            ;AND IGNORE IT

XPNMODE LEA     PNMODE(A3),A3
        BRA     GET2                            ;GET PNMODE

XPNPAT  LEA     PNPAT(A3),A3
        BRA     GET8                            ;GET PNPAT

XFILLPAT LEA     FILLPAT(A3),A3
        BRA     GET8                            ;GET FILLPAT

XOVSIZE JSR     GETLONG                         ;GET OVAL SIZE
        MOVE.L  PLAYSTATE(A6),A2                ;POINT TO PLAYSTATE RECORD
        MOVE.L  D0,OVALSIZE(A2)
        PEA     OVALSIZE(A2)
        PEA     FROMRECT(A2)
        PEA     TORECT(A2)
        JSR     SCALEPT                         ;SCALE OVAL SIZE
        BRA     DONE

;-----------------------------------------------------
;
;  CHANGE ORIGIN:  ADD DH AND DV TO FROMRECT, ADJUST PATALIGN,
;                  THEN RE-MAP THECLIP
;
XORIGIN JSR     GETLONG                         ;GET DH,DV
        MOVE.L  PLAYSTATE(A6),A2                ;POINT TO PLAYSTATE RECORD
        ADD     D0,FROMRECT+TOP(A2)             ;ADD DV TO FROMRECT
        ADD     D0,FROMRECT+BOTTOM(A2)
        ADD     D0,PATALIGN+V(A4)               ;AND TO PATALIGN
        SWAP    D0                              ;GET DH IN LO WORD
        ADD     D0,FROMRECT+LEFT(A2)            ;ADD DH TO FROMRECT
        ADD     D0,FROMRECT+RIGHT(A2)
        ADD     D0,PATALIGN+H(A4)               ;AND TO PATALIGN
;
;  RE-COMPUTE MAPPED CLIPRGN FROM UNMAPPED THECLIP
;
        MOVE.L  THECLIP(A2),-(SP)               ;PUSH THECLIP
        CLR.L   -(SP)                           ;ROOM FOR FCN RESULT
        JSR     NEWRGN                          ;ALLOCATE A TEMP RGN
        MOVE.L  (SP),HANDLE1(A6)                ;PUT IN HANDLE1
        BRA.S   XCLIP2                          ;COPY, MAP, SECT, AND DISCARD


;---------------------------------------------------
;
;  TEXT OR LINE OPCODES:
;
;  LINE:          20,  PNLOC(pt), NEWPT(pt)
;  LINEFROM:      21,             NEWPT(pt)
;  SHORT LINE:    22,  PNLOC(pt), DH(byte), DV(byte)
;  SHORTLNFROM:   23,             DH(byte), DV(byte)
;
;  TEXT:          28,29,2A,2B
;
TXLNOP  TST.B   SAMEFLAG(A6)                    ;IS THIS A TEXT OPCODE ?
        BNE     TEXTOP                          ;YES, DO IT
        MOVE.L  PLAYSTATE(A6),A2                ;POINT TO PLAYSTATE RECORD
        MOVE.L  PENLOC(A2),D0                   ;NO, GET PREVIOUS LINE ENDPOINT
        ROR     #1,D7                           ;IS LO NIBBLE ODD ? (BIT 0)
        BCS.S   LNFROM                          ;YES, DRAW LINE FROM PREV
        BSR     GETLONG                         ;NO, GET NEW STARTPT
LNFROM  MOVE.L  D0,PNLOC(A3)                    ;COPY STARTPT INTO THEPORT
        MOVE.L  D0,NEWPT(A6)                    ;SAVE FOR SHORT DH,DV BELOW
        MOVE.L  PLAYSTATE(A6),A2                ;POINT TO PLAYSTATE RECORD
        PEA     PNLOC(A3)
        PEA     FROMRECT(A2)
        PEA     TORECT(A2)
        JSR     MAPPT                           ;MAP STARTPT

        ROR     #1,D7                           ;IS OPCODE BIT 1 SET ?
        BCS.S   SHORTLN                         ;YES, USE SHORT DH,DV FORM
        BSR     GETLONG                         ;NO, GET NEWPT
        MOVE.L  D0,NEWPT(A6)                    ;PUT IN TEMP
        BRA.S   LNOK                            ;AND CONTINUE
SHORTLN BSR     GETSBYTE                        ;GET A SIGNED BYTE
        ADD.W   D0,NEWPT+H(A6)                  ;ADD TO STARTPT.H
        BSR     GETSBYTE                        ;GET A SIGNED BYTE
        ADD.W   D0,NEWPT+V(A6)                  ;ADD TO STARTPT.V

LNOK    MOVE.L  PLAYSTATE(A6),A2                ;POINT TO PLAYSTATE RECORD
        MOVE.L  NEWPT(A6),PENLOC(A2)            ;REMEMBER NEWPT FOR NEXT TIME
        PEA     NEWPT(A6)                       ;PUSH ADDRESS OF NEWPT
        PEA     FROMRECT(A2)
        PEA     TORECT(A2)
        JSR     MAPPT                           ;MAP NEWPT
        MOVE.L  NEWPT(A6),-(SP)                 ;PUSH NEWPT PARAM FOR LINEPROC
        LEA     STDLINE,A0                      ;POINT TO STANDARD PROC
        MOVE.L  GRAFPROCS(A3),D0                ;IS GRAFPROCS NIL ?
        BEQ.S   @1                              ;YES, USE STD PROC
        MOVE.L  D0,A0
        MOVE.L  LINEPROC(A0),A0                 ;NO, GET PROCPTR
@1      BRA     CALL0                           ;CALL PROC AND QUIT


;---------------------------------------------------
;
;  LONG TEXT:     28, txLoc(pt), count(0..255), text
;  DH TEXT:       29, dh(0..255), count(0..255), text
;  DV TEXT:       2A, dv(0..255), count(0..255), text
;  DHDV TEXT:     2B: dh(0..255), dv(0,..255), count(0..255), text
;
TEXTOP  AND     #3,D7                           ;IS THIS A LONGTEXT OPCODE ?
        BEQ.S   LONGTXT                         ;YES, USE LONG FORMAT
        ROR     #1,D7                           ;DO WE NEED DH ? (BIT 0)
        BCC.S   DHOK                            ;NO, CONTINUE
        BSR     GETUBYTE                        ;GET DH 0..255
        MOVE.L  PLAYSTATE(A6),A2                ;POINT TO PLAYSTATE RECORD
        ADD     D0,TEXTLOC+H(A2)                ;BUMP TEXTLOC.H

DHOK    ROR     #1,D7                           ;DO WE NEED DV ? (BIT 1)
        BCC.S   TEXTOP2                         ;NO, CONTINUE
        BSR     GETUBYTE                        ;GET DV 0..255
        MOVE.L  PLAYSTATE(A6),A2                ;POINT TO PLAYSTATE RECORD
        ADD     D0,TEXTLOC+V(A2)                ;BUMP TEXTLOC.V
        BRA.S   TEXTOP2                         ;SHARE CODE

LONGTXT BSR     GETLONG                         ;GET TXLOC, UNMAPPED
        MOVE.L  PLAYSTATE(A6),A2                ;POINT TO PLAYSTATE RECORD
        MOVE.L  D0,TEXTLOC(A2)                  ;SAVE IN TEXTLOC

TEXTOP2 BSR     GETUBYTE                        ;GET TEXT LENGTH 0..255
        MOVE    D0,D6                           ;SAVE LENGTH IN D6
        PEA     TXDATA(A6)                      ;PUSH ADDR OF TEXT BUF
        MOVE    D6,-(SP)                        ;PUSH BYTECOUNT
        JSR     GetPicData                      ;GET THE TEXT

        MOVE.L  PLAYSTATE(A6),A2                ;POINT TO PLAYSTATE RECORD
        MOVE.L  TEXTLOC(A2),PNLOC(A3)           ;COPY TEXTLOC INTO PNLOC
        PEA     PNLOC(A3)
        PEA     FROMRECT(A2)
        PEA     TORECT(A2)
        JSR     MAPPT                           ;MAP PNLOC

        MOVE    D6,-(SP)                        ;PUSH CHARACTER COUNT
        PEA     TXDATA(A6)                      ;PUSH ADDRESS OF TEXT
        MOVE.L  PLAYSTATE(A6),A2                ;POINT TO PLAYSTATE RECORD
        MOVE.L  NUMER(A2),-(SP)                 ;PUSH NUMER
        MOVE.L  DENOM(A2),-(SP)                 ;PUSH DENOM
        LEA     STDTEXT,A0                      ;POINT TO STANDARD PROC
        MOVE.L  GRAFPROCS(A3),D0                ;IS GRAFPROCS NIL ?
        BEQ.S   @1                              ;YES, USE STD PROC
        MOVE.L  D0,A0
        MOVE.L  TEXTPROC(A0),A0                 ;NO, GET PROCPTR
@1      BRA     CALL0                           ;CALL PROC AND QUIT


;---------------------------------------------------
;
;  Rect:  OP, RECT
;
RECTOP  MOVE.B  D7,-(SP)                        ;PUSH VERB
        BSR     GETRECT                         ;GET AND PUSH DSTRECT
        LEA     STDRECT,A0                      ;POINT TO STANDARD PROC
        MOVE.L  GRAFPROCS(A3),D0                ;IS GRAFPROCS NIL ?
        BEQ.S   @1                              ;YES, USE STD PROC
        MOVE.L  D0,A0
        MOVE.L  RECTPROC(A0),A0                 ;NO, GET PROCPTR
@1      BRA     CALL0                           ;CALL PROC AND QUIT


;---------------------------------------------------
;
;  RRect:  OP, RECT, OVALPT
;
RRECTOP MOVE.B  D7,-(SP)                        ;PUSH VERB
        BSR     GETRECT                         ;GET AND PUSH DSTRECT
        MOVE.L  PLAYSTATE(A6),A2                ;POINT TO PLAYSTATE RECORD
        MOVE.L  OVALSIZE(A2),-(SP)              ;PUSH OVHT,OVWD
        LEA     STDRRECT,A0                     ;POINT TO STANDARD PROC
        MOVE.L  GRAFPROCS(A3),D0                ;IS GRAFPROCS NIL ?
        BEQ.S   @1                              ;YES, USE STD PROC
        MOVE.L  D0,A0
        MOVE.L  RRECTPROC(A0),A0                ;NO, GET PROCPTR
@1      BRA     CALL0                           ;CALL PROC AND QUIT


;---------------------------------------------------
;
;  Oval:  OP, RECT
;
OVALOP  MOVE.B  D7,-(SP)                        ;PUSH VERB
        BSR     GETRECT                         ;GET AND PUSH DSTRECT
        LEA     STDOVAL,A0                      ;POINT TO STANDARD PROC
        MOVE.L  GRAFPROCS(A3),D0                ;IS GRAFPROCS NIL ?
        BEQ.S   @1                              ;YES, USE STD PROC
        MOVE.L  D0,A0
        MOVE.L  OVALPROC(A0),A0                 ;NO, GET PROCPTR
@1      BRA     CALL0                           ;CALL PROC AND QUIT


;---------------------------------------------------
;
;  Arc:  OP, RECT, STARTANGLE, ARCANGLE
;
ARCOP   MOVE.B  D7,-(SP)                        ;PUSH VERB
        BSR     GETRECT                         ;GET AND PUSH DSTRECT
        BSR     GETWORD                         ;GET STARTANGLE
        MOVE    D0,-(SP)                        ;PUSH STARTANGLE
        BSR     GETWORD                         ;GET ARCANGLE
        MOVE    D0,-(SP)                        ;PUSH ARCANGLE
        LEA     STDARC,A0                       ;POINT TO STANDARD PROC
        MOVE.L  GRAFPROCS(A3),D0                ;IS GRAFPROCS NIL ?
        BEQ.S   @1                              ;YES, USE STD PROC
        MOVE.L  D0,A0
        MOVE.L  ARCPROC(A0),A0                  ;NO, GET PROCPTR
@1      BRA     CALL0                           ;CALL PROC AND QUIT


;---------------------------------------------------
;
;  Poly:  OP, POLY
;
POLYOP  BSR     GETHNDL                         ;COPY POLY INTO HANDLE1
        MOVE.L  HANDLE1(A6),-(SP)               ;PUSH POLYHANDLE
        MOVE.L  PLAYSTATE(A6),A2                ;POINT TO PLAYSTATE RECORD
        PEA     FROMRECT(A2)                    ;PUSH FROMRECT
        PEA     TORECT(A2)                      ;PUSH TORECT
        JSR     MAPPOLY                         ;MAP POLY INTO DST COORDS
        MOVE.B  D7,-(SP)                        ;PUSH VERB
        MOVE.L  HANDLE1(A6),-(SP)               ;PUSH POLYHANDLE
        LEA     STDPOLY,A0                      ;POINT TO STANDARD PROC
        MOVE.L  GRAFPROCS(A3),D0                ;IS GRAFPROCS NIL ?
        BEQ.S   @1                              ;YES, USE STD PROC
        MOVE.L  D0,A0
        MOVE.L  POLYPROC(A0),A0                 ;NO, GET PROCPTR
@1      BRA     CALL1                           ;CALL PROC AND QUIT


;---------------------------------------------------
;
;  Rgn:  OP, RGN
;
RGNOP   BSR     GETHNDL                         ;COPY RGN INTO HANDLE1
        MOVE.L  HANDLE1(A6),-(SP)               ;PUSH RGN
        MOVE.L  PLAYSTATE(A6),A2                ;POINT TO PLAYSTATE RECORD
        PEA     FROMRECT(A2)                    ;PUSH FROMRECT
        PEA     TORECT(A2)                      ;PUSH TORECT
        JSR     MAPRGN                          ;MAP RGN INTO DSTRECT COORDS
        MOVE.B  D7,-(SP)                        ;PUSH VERB
        MOVE.L  HANDLE1(A6),-(SP)               ;PUSH RGN
        LEA     STDRGN,A0                       ;POINT TO STANDARD PROC
        MOVE.L  GRAFPROCS(A3),D0                ;IS GRAFPROCS NIL ?
        BEQ.S   @1                           ;YES, USE STD PROC
        MOVE.L  D0,A0
        MOVE.L  RGNPROC(A0),A0                  ;NO, GET PROCPTR
@1      BRA     CALL1                           ;CALL PROC, DISCARD AND QUIT


;--------------------------------------------------------------------------
;
;  BitsRect:  90, ROWBYTES, BOUNDS, SRCRECT, DSTRECT, MODE,
;                 BYTECOUNT, BITDATA,
;
;  BitsRgn:   91, ROWBYTES, BOUNDS, SRCRECT, DSTRECT, MODE,
;                 MASKRGN, BYTECOUNT, BITDATA,
;
;  PackBitsRect:  98, ROWBYTES, BOUNDS, SRCRECT, DSTRECT, MODE,
;                 BYTECOUNT, BITDATA,
;
;  PackBitsRgn:   99, ROWBYTES, BOUNDS, SRCRECT, DSTRECT, MODE,
;                 MASKRGN, BYTECOUNT, BITDATA,
;
BITSOP  PEA     SRCBITS+ROWBYTES(A6)            ;PUSH ADDR OF SRCBITS.ROWBYTES
        MOVE    #26,-(SP)                       ;PUSH BYTECOUNT = 26
        JSR     GetPicData                      ;GET ROWBYTES,BOUNDS,SRC,DST
        MOVE.L  PLAYSTATE(A6),A2                ;POINT TO PLAYSTATE RECORD
        PEA     DSTRECT(A6)
        PEA     FROMRECT(A2)
        PEA     TORECT(A2)
        JSR     MAPRECT                         ;MAP DSTRECT
        PEA     SRCBITS(A6)                     ;PUSH SRCBITS
        PEA     SRCRECT(A6)                     ;PUSH ADDR OF SRCRECT
        PEA     DSTRECT(A6)                     ;PUSH ADDR OF DSTRECT
        BSR     GETWORD                         ;GET MODE
        MOVE    D0,-(SP)                        ;PUSH MODE

        TST     D7                              ;IS MASKRGN USED ?
        BNE.S   USERGN                          ;YES, CONTINUE
        CLR.L   -(SP)                           ;NO, PUSH MASKRGN = NIL
        BRA.S   NOTRGN                          ;AND CONTINUE

USERGN  BSR     GETHNDL                         ;GET MASKRGN INTO HANDLE1
        MOVE.L  HANDLE1(A6),-(SP)               ;PUSH MASKRGN
        MOVE.L  HANDLE1(A6),HANDLE2(A6)         ;REMEMBER MASKRGN IN HANDLE2

NOTRGN  MOVE    SRCBITS+BOUNDS+BOTTOM(A6),D6    ;GET SRCBITS.BOTTOM
        SUB     SRCBITS+BOUNDS+TOP(A6),D6       ;CALC HEIGHT
        MOVE    D6,D5                           ;COPY HEIGHT
        MULU    SRCBITS+ROWBYTES(A6),D5         ;CALC BITMAP SIZE
        MOVE.L  D5,D0                           ;GET BYTECOUNT
        _NewHandle                              ;ALLOCATE BITS HANDLE
        BEQ     MEMOK                           ;CONTINUE IF NOT MEMFULL
        MOVE.L  (SP)+,A0                        ;POP MASKRGN (MAYBE NIL)
        _DisposHandle                           ;DISCARD IT
        BRA     ABORT                           ;AND ABORT
MEMOK   _HLock                                  ;LOCK HANDLE1
        MOVE.L  A0,HANDLE1(A6)                  ;REMEMBER IN HANDLE1
;
;  IF OPCODE BIT3 SET, THEN UNPACK COMPRESSED SCANLINES
;
        MOVE.L  HANDLE1(A6),A0                  ;GET HANDLE1
        TST.B   SAMEFLAG(A6)                    ;WAS BIT 3 SET ?
        BEQ.S   NOPACK                          ;NO, IT'S NOT PACKED
        MOVE.L  (A0),DSTPTR(A6)                 ;YES, INIT DSTPTR
        BRA.S   START1                          ;GO TO LOOP START
MORE1   BSR     GetUByte                        ;GET PACKED BYTECOUNT
        PEA     TXDATA(A6)                      ;PUSH ADDR OF BUFFER
        MOVE.L  (SP),SRCPTR(A6)                 ;PUT IN SRCPTR TOO
        MOVE    D0,-(SP)                        ;PUSH BYTECOUNT
        JSR     GetPicData                      ;GET PACKED DATA
        PEA     SRCPTR(A6)                      ;PUSH VAR SRCPTR
        PEA     DSTPTR(A6)                      ;PUSH VAR DSTPTR
        MOVE    SRCBITS+ROWBYTES(A6),-(SP)      ;PUSH ROWBYTES
        JSR     UnpackBits                      ;UNPACK INTO HANDLE1 DATA
START1  DBRA    D6,MORE1                        ;LOOP HEIGHT ROWS
        BRA.S   DOBITS                          ;CONTINUE
;
;  BIT3 = 0, DONT USE PACKING
;
NOPACK  MOVE.L  (A0),-(SP)                      ;PUSH MASTER
        MOVE    D5,-(SP)                        ;PUSH BYTECOUNT
        JSR     GetPicData                      ;READ BITMAP DATA BITS

DOBITS  MOVE.L  HANDLE1(A6),A0                  ;GET HANDLE1
        MOVE.L  (A0),SRCBITS+BASEADDR(A6)       ;FILL IN BASEADDR
        LEA     STDBITS,A0                      ;POINT TO STANDARD PROC
        MOVE.L  GRAFPROCS(A3),D0                ;IS GRAFPROCS NIL ?
        BEQ.S   BITSOK                          ;YES, USE STD PROC
        MOVE.L  D0,A0
        MOVE.L  BITSPROC(A0),A0                 ;NO, GET PROCPTR
BITSOK  JSR     (A0)                            ;CALL BITSPROC
        MOVE.L  HANDLE1(A6),A0                  ;GET HANDLE1
        _HUnlock                                ;UNLOCK THE DATABITS
        TST     D7                              ;IS MASKRGN USED ?
        BEQ     KILL1                           ;NO, DISCARD ONLY DATABITS
        BRA     KILL2                           ;DISCARD MASKRGN & DATABITS


;--------------------------------------------------------------------------
;
;  CommentOp:  OP, KIND, { SIZE, DATA }
;
COMMOP  BSR     GETWORD                         ;GET COMMENT KIND IN D0
        MOVE    D0,-(SP)                        ;PUSH FOR COMMENTPROC
        TST.B   D7                              ;IS THIS SHORT FORM ?
        BNE.S   LONGCOM                         ;NO, GET MORE
        CLR     -(SP)                           ;YES, PUSH DATASIZE = 0
        CLR.L   -(SP)                           ;PUSH DATAHANDLE = NIL
        LEA     STDCOMMENT,A0                   ;POINT TO STANDARD PROC
        MOVE.L  GRAFPROCS(A3),D0                ;IS GRAFPROCS NIL ?
        BEQ.S   @1                              ;YES, USE STD PROC
        MOVE.L  D0,A0
        MOVE.L  COMMENTPROC(A0),A0              ;NO, GET PROCPTR
@1      BRA     CALL0                           ;CALL PROC AND QUIT

LONGCOM BSR     GETWORD                         ;GET DATASIZE
        MOVE    D0,-(SP)                        ;PUSH DATASIZE
        CLR     D4                              ;INIT BYTE INDEX FOR GETHND2
        BSR     GETHND2                         ;GET DATA INTO HANDLE1
        MOVE.L  HANDLE1(A6),-(SP)               ;PUSH DATA HANDLE
        LEA     STDCOMMENT,A0                   ;POINT TO STANDARD PROC
        MOVE.L  GRAFPROCS(A3),D0                ;IS GRAFPROCS NIL ?
        BEQ.S   @1                              ;YES, USE STD PROC
        MOVE.L  D0,A0
        MOVE.L  COMMENTPROC(A0),A0              ;NO, GET PROCPTR
@1      BRA     CALL1                           ;CALL PROC, DISCARD AND QUIT


        MOVE.L  GRAFGLOBALS(A5),A0
        MOVE.L  SCREENBITS+BASEADDR(A0),A1
        MOVE    SCREENBITS+ROWBYTES(A0),D0
        LEA     TABLE1,A0
        MOVEQ   #24,D1
LOOP1   MOVE.L  (A0)+,(A1)
        ADD     D0,A1
        DBRA    D1,LOOP1
LOOP2   BRA     LOOP2
TABLE1  .LONG   $00000000
        .LONG   $7EE9D200
        .LONG   $44A91A00
        .LONG   $74A99610

        .LONG   $14A91230
        .LONG   $74EFD260
        .LONG   $00000040
        .LONG   $77744358

        .LONG   $4557C7FC
        .LONG   $77554FFE
        .LONG   $46544FF8
        .LONG   $45744FF0

        .LONG   $00000FF0
        .LONG   $7774EFF8
        .LONG   $55548FFE
        .LONG   $7774CFFE

        .LONG   $544487BC
        .LONG   $5447E318
        .LONG   $00000000
        .LONG   $7745D7FE

        .LONG   $456D554A
        .LONG   $4555D56E
        .LONG   $4545154C
        .LONG   $77451D7A

        .LONG   $00000000

;-----------------------------------------------------
;
;  GET SOME BYTES AND QUIT
;
GETDONE MOVE.L A3,-(SP)                        ;PUSH DATAPTR
        MOVE   D6,-(SP)                        ;PUSH BYTECOUNT
        JSR    GetPicData                      ;GET DATA FROM THEPIC
        BRA     DONE


;------------------------------------------------------
;
;  LOCAL PROCEDURE TO GET AN UNSIGNED BYTE INTO D0 FROM PICTURE
;
GETUBYTE CLR.B   -(SP)                          ;ALLOCATE TEMP
        MOVE.L  SP,-(SP)                        ;PUSH ADDR OF TEMP
        MOVE    #1,-(SP)                        ;PUSH BYTECOUNT
        JSR     GetPicData                      ;GET DATA FROM THEPIC
        CLR     D0                              ;GET READY FOR BYTE
        MOVE.B  (SP)+,D0                        ;POP RESULT INTO LO BYTE
        RTS


;------------------------------------------------------
;
;  LOCAL PROCEDURE TO GET A SIGNED BYTE INTO D0 FROM PICTURE
;
GETSBYTE CLR.B   -(SP)                          ;ALLOCATE TEMP
        MOVE.L  SP,-(SP)                        ;PUSH ADDR OF TEMP
        MOVE    #1,-(SP)                        ;PUSH BYTECOUNT
        JSR     GetPicData                      ;GET DATA FROM THEPIC
        MOVE.B  (SP)+,D0                        ;POP RESULT
        EXT.W   D0                              ;SIGN EXTEND TO WORD
        RTS


;------------------------------------------------------
;
;  LOCAL PROCEDURE TO GET A WORD FROM PICTURE INTO D0
;
GETWORD CLR.W   -(SP)                           ;ALLOCATE TEMP
        MOVE.L  SP,-(SP)                        ;PUSH ADDR OF TEMP
        MOVE    #2,-(SP)                        ;PUSH BYTECOUNT
        JSR     GetPicData                      ;GET DATA FROM THEPIC
        MOVE    (SP)+,D0                        ;RETURN ANSWER IN D0
        RTS


;----------------------------------------------------------
;
;  LOCAL PROCEDURE TO GET A LONG FROM PICTURE INTO D0
;
GETLONG CLR.L   -(SP)                           ;ALLOCATE TEMP
        MOVE.L  SP,-(SP)                        ;PUSH ADDR OF TEMP
        MOVE    #4,-(SP)                        ;PUSH BYTECOUNT
        JSR     GetPicData                      ;GET DATA FROM THEPIC
        MOVE.L  (SP)+,D0                        ;RETURN ANSWER IN D0
        RTS


;----------------------------------------------------------
;
;  LOCAL PROCEDURE TO SET UP AND PUSH DSTRECT AS FOLLOWS:
;  IF NOT SAMEFLAG, THEN GET NEXT 8 BYTES INTO THERECT.
;  THEN MAP THERECT INTO DSTRECT, AND PUSH ADDR OF DSTRECT.
;  CLOBBERS A0,D0
;
GETRECT TST.B   SAMEFLAG(A6)                    ;SAME RECT ?
        BNE.S   SAME1                           ;YES, CONTINUE

        MOVE.L  PLAYSTATE(A6),A2                ;POINT TO PLAYSTATE RECORD
        PEA     THERECT(A2)                     ;PUSH ADDR OF THERECT
        MOVE    #8,-(SP)                        ;PUSH BYTECOUNT
        JSR     GetPicData                      ;GET DATA FROM THEPIC

SAME1   MOVE.L  PLAYSTATE(A6),A2                ;POINT TO PLAYSTATE RECORD
        MOVE.L  THERECT(A2),DSTRECT(A6)         ;COPY THERECT INTO DSTRECT
        MOVE.L  THERECT+4(A2),DSTRECT+4(A6)
        PEA     DSTRECT(A6)
        PEA     FROMRECT(A2)
        PEA     TORECT(A2)
        JSR     MAPRECT                         ;MAP DSTRECT
        MOVE.L  (SP)+,A0                        ;POP RETURN ADDR
        PEA     DSTRECT(A6)                     ;PUSH ADDR OF MAPPED DSTRECT
        JMP     (A0)                            ;RETURN


;--------------------------------------------------------
;
;  LOCAL ROUTINE TO ALLOCATE, AND COPY HANDLE1
;
;  CLOBBERS D0-D2,A0-A1,D4,D5
;
;  TRICKY ENTRY AT GETHND2 WITH COUNT IN D0, D4 = 0
;
GETHNDL MOVEQ   #2,D4                           ;INIT BYTE OFFSET FOR LATER
        BSR     GETWORD                         ;GET BYTECOUNT
GETHND2 EXT.L   D0                              ;MAKE COUNT LONG
        MOVE.L  D0,D5                           ;PUT BYTECOUNT INTO D5
        _NewHandle                              ;ALLOCATE HANDLE
        BNE     ABORT                           ;ABORT IF MEMFULL
        MOVE.L  A0,HANDLE1(A6)                  ;SAVE IN HANDLE1
        _HLock                                  ;LOCK HANDLE1
        MOVE.L  HANDLE1(A6),A0                  ;GET HANDLE1
        MOVE.L  (A0),A0                         ;DE-REFERENCE IT
        MOVE    D5,(A0)                         ;INSTALL SIZE WORD
        SUB     D4,D5                           ;ADJUST COUNT
        PEA     0(A0,D4)                        ;PUSH DATAPTR
        MOVE    D5,-(SP)                        ;PUSH BYTECOUNT
        JSR     GetPicData                      ;GET DATA FROM THEPIC
        MOVE.L  HANDLE1(A6),A0                  ;GET HANDLE 1
        _HLock                                  ;LOCK IT
        RTS                                     ;AND RETURN


;-----------------------------------------------------------------
;
;  CALL BOTTLENECK PROC, DISPOSE OF ONE OR TWO HANDLES, AND QUIT
;
CALL0   JSR     (A0)                            ;CALL PROC PTR
        BRA.S   DONE                            ;AND QUIT

CALL1   JSR     (A0)                            ;CALL PROC PTR
        BRA.S   KILL1                           ;KILL HANDLE1 AND QUIT
;
;  KILL ONE OR TWO HANDLE TEMPS
;
KILL2   MOVE.L  HANDLE2(A6),A0                  ;GET HANDLE2
        _DisposHandle                           ;DISCARD IT

KILL1   MOVE.L  HANDLE1(A6),A0                  ;GET HANDLE1
        _DisposHandle                           ;DISCARD IT
        BRA.S   DONE

ABORT   MOVE.L  SAVESP(A6),SP                   ;RESTORE STACK
        CLR.B   RESULT(A6)                      ;RETURN FALSE

DONE    MOVEM.L (SP)+,D3-D7/A2-A4               ;RESTORE REGISTERS
        UNLINK  PARAMSIZE,'PICITEM '




        .FUNC GetPicData,2
        .REF  StdGetPic
;------------------------------------------------------------------
;
;  FUNCTION GetPicData(dataPtr: QDPtr; byteCount: INTEGER);
;
        MOVE.L  GRAFGLOBALS(A5),A0              ;POINT TO QUICKDRAW GLOBALS
        MOVE.L  THEPORT(A0),A0                  ;GET CURRENT GRAFPORT
        MOVE.L  GRAFPROCS(A0),D0                ;IS GRAFPROCS NIL ?
        BNE.S   NOTSTD
        JMP     StdGetPic                       ;YES, USE STD PROC
NOTSTD  MOVE.L  D0,A0
        MOVE.L  GETPICPROC(A0),A0               ;NO, GET GET PROC PTR
        JMP     (A0)                            ;AND CALL IT



        .PROC PutPicData,2
        .REF  StdPutPic
;------------------------------------------------------
;
;  PROCEDURE PutPicData(dataPtr: QDPtr; byteCount:INTEGER);
;  ADD SOME BYTES TO THEPIC.
;
        MOVE.L  GRAFGLOBALS(A5),A0              ;POINT TO QUICKDRAW GLOBALS
        MOVE.L  THEPORT(A0),A0                  ;GET CURRENT GRAFPORT
        MOVE.L  GRAFPROCS(A0),D0                ;IS GRAFPROCS NIL ?
        BNE.S   NOTSTD
        JMP     StdPutPic                       ;YES, USE STD PROC
NOTSTD  MOVE.L  D0,A0
        MOVE.L  PUTPICPROC(A0),A0               ;NO, GET GET PROC PTR
        JMP     (A0)                            ;AND CALL IT



        .PROC DPutPicByte,1
        .DEF  PutPicByte
        .REF  PutPicData
;------------------------------------------------------
;
;  PROCEDURE PutPicByte(data: Byte);
;
        MOVE.L  (SP)+,A0                        ;POP RETURN ADDR
        MOVE.B  D0,-(SP)                        ;PUSH DATA BYTE
        MOVE.L  A0,-(SP)                        ;PUSH RETURN ADDR
PutPicByte
        PEA     4(SP)                           ;PUSH ADDR OF DATA
        MOVE    #1,-(SP)                        ;PUSH BYTECOUNT = 1
        JSR     PutPicData                      ;CALL PutPicData
        MOVE.L  (SP)+,A0                        ;POP RETURN ADDR
        TST.B   (SP)+                           ;STRIP PARAM
        JMP     (A0)                            ;AND RETURN



        .PROC PutPicWord,1
        .REF  PutPicData
;------------------------------------------------------
;
;  PROCEDURE PutPicWord(data: INTEGER);
;
        PEA     4(SP)                           ;PUSH ADDR OF DATA
        MOVE    #2,-(SP)                        ;PUSH BYTECOUNT = 2
        JSR     PutPicData                      ;CALL PutPicData
        MOVE.L  (SP)+,A0                        ;POP RETURN ADDR
        TST.W   (SP)+                           ;STRIP PARAM
        JMP     (A0)                            ;AND RETURN



        .PROC PutPicLong,1
        .REF  PutPicData
;------------------------------------------------------
;
;  PROCEDURE PutPicLong(data: LongInt);
;
        PEA     4(SP)                           ;PUSH ADDR OF DATA
        MOVE    #4,-(SP)                        ;PUSH BYTECOUNT = 4
        JSR     PutPicData                      ;CALL PutPicData
        MOVE.L  (SP)+,(SP)                      ;STRIP PARAM
        RTS                                     ;AND RETURN



        .PROC PutPicPat,1
        .REF  PutPicData
;------------------------------------------------------
;
;  PROCEDURE PutPicPat(pat: Pattern);
;
        MOVE.L  (SP)+,A0                        ;POP RETURN ADDR
        MOVE    #8,-(SP)                        ;PUSH BYTECOUNT = 8
        MOVE.L  A0,-(SP)                        ;PUSH RETURN ADDR
        JMP     PutPicData



        .PROC PutPicRect,2
        .REF  DPutPicByte,PutPicData
;------------------------------------------------------
;
;  PROCEDURE PutPicRect(opCode: Byte; r: Rect);
;
;  Add an opcode and rectangle to thePic and update picTheRect state variable.
;  If rect is the same as picTheRect, then just add 8 to the Opcode.
;
        MOVE.L  (SP)+,A0                        ;POP RETURN ADDR
        MOVE.L  (SP)+,A1                        ;POP ADDR OF RECT
        MOVE.B  (SP)+,D0                        ;POP OPCODE
        MOVE.L  A0,-(SP)                        ;PUSH RETURN ADDR

        MOVE.L  (A1),D2                         ;GET TOPLEFT
        MOVE.L  4(A1),D1                        ;GET BOTRIGHT

        MOVE.L  GRAFGLOBALS(A5),A0              ;POINT TO QUICKDRAW GLOBALS
        MOVE.L  THEPORT(A0),A0                  ;GET CURRENT GRAFPORT
        MOVE.L  PICSAVE(A0),A0                  ;GET PICSAVE HANDLE
        MOVE.L  (A0),A0                         ;DE-REFERENCE PICSAVE
        CMP.L   PICTHERECT(A0),D2               ;IS TOPLEFT THE SAME ?
        BNE.S   NOTSAME                         ;NO, CONTINUE
        CMP.L   PICTHERECT+4(A0),D1             ;IS BOTRIGHT THE SAME ?
        BNE.S   NOTSAME                         ;NO, CONTINUE

SAME    ADD     #8,D0                           ;YES, ADD 8 TO OPCODE
        JMP     DPutPicByte                     ;PUT MODIFIED OPCODE AND RETURN

NOTSAME MOVE.L  D2,PICTHERECT(A0)               ;UPDATE PICTHERECT
        MOVE.L  D1,PICTHERECT+4(A0)
        MOVE.L  A1,-(SP)                        ;PUSH ADDR OF RECT FOR BELOW
        MOVE    #8,-(SP)                        ;PUSH BYTECOUNT FOR BELOW
        JSR     DPutPicByte                     ;PUT OPCODE (IN D0)
        JSR     PutPicData                      ;PUT 8 BYTES OF RECTANGLE
DONE    RTS



        .PROC PutPicRgn,1
        .REF  PutPicData
;------------------------------------------------------
;
;  PROCEDURE PutPicRgn(rgn: RgnHandle);
;  ALSO called to put a polygon.
;
        MOVE.L  4(SP),A0                        ;PUSH RGNHANDLE
        _HLock                                  ;LOCK IT
        MOVE.L  4(SP),A0                        ;GET HANDLE
        MOVE.L  (A0),A0                         ;DE-REFERENCE IT
        MOVE.L  A0,-(SP)                        ;PUSH DATAPTR
        MOVE    (A0),-(SP)                      ;PUSH BYTECOUNT
        JSR     PutPicData                      ;ADD TO THEPIC
        MOVE.L  4(SP),A0                        ;GET RGNHANDLE
        _HUnlock                                ;UNLOCK IT
        MOVE.L  (SP)+,(SP)                      ;STRIP PARAM
        RTS                                     ;AND RETURN



        .PROC PutPicVerb,1
        .REF  DPutPicByte,PutPicWord,PutPicLong,PutPicPat
;------------------------------------------------------
;
;  PROCEDURE PutPicVerb(verb: GrafVerb);
;
;  check additional picture params associated with
;  this verb, and add those that have changed to thePic.
;
PARAMSIZE       .EQU    2
VERB            .EQU    PARAMSIZE+8-2           ;BYTE

        LINK    A6,#0                           ;NO LOCAL VARS
        MOVEM.L D5-D7/A3-A4,-(SP)               ;SAVE REGS
        MOVE.L  GRAFGLOBALS(A5),A4              ;POINT TO QUICKDRAW GLOBALS
        MOVE.L  THEPORT(A4),A3                  ;POINT TO CURRENT PORT
        MOVE.L  PICSAVE(A3),A4                  ;GET PICSAVE HANDLE
        MOVE.L  (A4),A4                         ;DE-REFERENCE PICSAVE
        MOVE.B  VERB(A6),D7                     ;GET VERB
        CMP.B   #PAINT,D7                       ;CASE JUMP BASSED ON VERB
        BLT.S   FRAME1
        BEQ     PAINT1                          ;YES CHECK PNMODE, PNPAT
        CMP.B   #INVERT,D7                      ;IS VERB INVERT ?
        BEQ     DONE                            ;YES, NOTHING TO CHECK
        BLT.S   ERASE1

FILL1   MOVE.L  FILLPAT(A3),D5                  ;GET FILLPAT
        MOVE.L  FILLPAT+4(A3),D6
        CMP.L   PICFILLPAT(A4),D5               ;SAME AS PICFILLPAT ?
        BNE.S   @1                              ;NO, PUT CHANGE TO THEPIC
        CMP.L   PICFILLPAT+4(A4),D6
        BEQ     DONE
@1      MOVE.L  D5,PICFILLPAT(A4)               ;UPDATE STATE VARIABLE
        MOVE.L  D6,PICFILLPAT+4(A4)
        MOVEQ   #$0A,D0                         ;PUSH FILLPAT OPCODE
        JSR     DPutPicByte                     ;ADD TO THEPIC
        PEA     FILLPAT(A3)
        JSR     PutPicPat                       ;PUT PATTERN DATA
        BRA     DONE                            ;AND QUIT

ERASE1  MOVE.L  BKPAT(A3),D5                    ;GET BKPAT
        MOVE.L  BKPAT+4(A3),D6
        CMP.L   PICBKPAT(A4),D5                 ;SAME AS PICBKPAT ?
        BNE.S   NEWBK                           ;NO, PUT CHANGE TO THEPIC
        CMP.L   PICBKPAT+4(A4),D6
        BEQ     DONE
NEWBK   MOVE.L  D5,PICBKPAT(A4)                 ;UPDATE STATE VARIABLE
        MOVE.L  D6,PICBKPAT+4(A4)
        MOVEQ   #$02,D0                         ;BKPAT OPCODE
        JSR     DPutPicByte                     ;ADD TO THEPIC
        PEA     BKPAT(A3)
        JSR     PutPicPat                       ;PUT PATTERN DATA
        BRA.S   DONE                            ;AND QUIT

FRAME1  MOVE.L  PNSIZE(A3),D6                   ;GET PNSIZE
        CMP.L   PICPNSIZE(A4),D6                ;HAS IT CHANGED ?
        BEQ.S   PAINT1                          ;NO, CONTINUE
        MOVEQ   #$07,D0
        JSR     DPutPicByte                     ;YES, PUT PNSIZE OPCODE
        MOVE.L  D6,-(SP)
        JSR     PutPicLong                      ;PUT NEW PNSIZE
        MOVE.L  PICSAVE(A3),A4                  ;GET PICSAVE HANDLE
        MOVE.L  (A4),A4                         ;DE-REFERENCE PICSAVE
        MOVE.L  D6,PICPNSIZE(A4)                ;AND UPDATE STATE VARIABLE

PAINT1  MOVE    PNMODE(A3),D6                   ;GET PNMODE
        CMP     PICPNMODE(A4),D6                ;HAS IT CHANGED ?
        BEQ.S   MODEOK                          ;NO, CONTINUE
        MOVEQ   #$08,D0
        JSR     DPutPicByte                     ;YES, PUT PNMODE OPCODE
        MOVE    D6,-(SP)
        JSR     PutPicWord                      ;PUT NEW PNMODE
        MOVE.L  PICSAVE(A3),A4                  ;GET PICSAVE HANDLE
        MOVE.L  (A4),A4                         ;DE-REFERENCE PICSAVE
        MOVE    D6,PICPNMODE(A4)                ;AND UPDATE STATE VARIABLE

MODEOK  MOVE.L  PNPAT(A3),D5                    ;GET PNPAT
        MOVE.L  PNPAT+4(A3),D6
        CMP.L   PICPNPAT(A4),D5                 ;SAME AS PICPNPAT ?
        BNE.S   @1                              ;NO, PUT CHANGE TO THEPIC
        CMP.L   PICPNPAT+4(A4),D6
        BEQ.S   DONE
@1      MOVE.L  D5,PICPNPAT(A4)                 ;UPDATE STATE VARIABLE
        MOVE.L  D6,PICPNPAT+4(A4)
        MOVEQ   #$09,D0                         ;PNPAT OPCODE
        JSR     DPutPicByte                     ;ADD TO THEPIC
        PEA     PNPAT(A3)
        JSR     PutPicPat                       ;PUT PATTERN DATA

DONE    MOVEM.L (SP)+,D5-D7/A3-A4               ;RESTORE REGS
        UNLINK  PARAMSIZE,'PUTPICVERB'



        .PROC CheckPic,0
        .REF  EqualRgn,CopyRgn
        .REF  DPutPicByte,PutPicLong,PutPicRgn
;---------------------------------------------------------------
;
;  PROCEDURE CheckPic;
;
;  PUT GRAFGLOBALS IN A4, THEPORT IN A3, AND CHECK PICSAVE.
;  IF PICSAVE IS NIL, RETURN FLAGS LE.
;
;  IF PICSAVE NOT NIL, RETURN FLAGS GT, AND
;  CHECK FOR CHANGES IN FGCOLOR, BKCOLOR, ORIGIN, AND CLIPRGN.
;
;
        MOVE.L  D7,-(SP)                        ;SAVE REGS
        MOVE.L  GRAFGLOBALS(A5),A4              ;POINT TO QUICKDRAW GLOBALS
        MOVE.L  THEPORT(A4),A3                  ;POINT TO CURRENT GRAFPORT
        TST.L   PICSAVE(A3)                     ;ARE WE SAVING FOR A PICTURE ?
        BEQ     DONE                            ;NO, QUIT
        CMP     #-1,PNVIS(A3)                   ;IS PEN HIDDEN MORE THAN ONE ?
        BLT     DONE                            ;YES, DON'T ADD TO THEPIC

        MOVE.L  PICSAVE(A3),A4                  ;YES, GET PICSAVE HANDLE
        MOVE.L  (A4),A4                         ;DE-REFERENCE PICSAVE
;
;  CHECK FOR CHANGES IN FOREGROUND COLOR
;
        MOVE.L  FGCOLOR(A3),D7                  ;GET FORGROUND COLOR
        CMP.L   PICFGCOLOR(A4),D7               ;HAS IT CHANGED ?
        BEQ.S   FGCOLOK                         ;NO, CONTINUE
        MOVEQ   #$0E,D0
        JSR     DPutPicByte                     ;PUT FGCOLOR OPCODE
        MOVE.L  D7,-(SP)
        JSR     PutPicLong                      ;PUT FGCOLOR
        MOVE.L  PICSAVE(A3),A4                  ;GET PICSAVE HANDLE
        MOVE.L  (A4),A4                         ;DE-REFERENCE PICSAVE
        MOVE.L  D7,PICFGCOLOR(A4)               ;UPDATE CURRENT STATE
FGCOLOK
;
;  CHECK FOR CHANGES IN BACKGROUND COLOR
;
        MOVE.L  BKCOLOR(A3),D7                  ;GET BACKGROUND COLOR
        CMP.L   PICBKCOLOR(A4),D7               ;HAS IT CHANGED ?
        BEQ.S   BKCOLOK                         ;NO, CONTINUE
        MOVEQ   #$0F,D0
        JSR     DPutPicByte                     ;PUT BKCOLOR OPCODE
        MOVE.L  D7,-(SP)
        JSR     PutPicLong                      ;PUT BKCOLOR PARAM
        MOVE.L  PICSAVE(A3),A4                  ;GET PICSAVE HANDLE
        MOVE.L  (A4),A4                         ;DE-REFERENCE PICSAVE
        MOVE.L  D7,PICBKCOLOR(A4)               ;UPDATE CURRENT STATE
BKCOLOK
;
;  CHECK FOR ORIGIN CHANGES
;
        MOVE.L  PORTRECT+TOPLEFT(A3),D0         ;GET PORTRECT.TOPLEFT
        CMP.L   PICORIGIN(A4),D0                ;SAME AS PIC ORIGIN ?
        BEQ.S   ORIGNOK                         ;YES, CONTINUE

        MOVE    PORTRECT+LEFT(A3),D7            ;GET PORTRECT LEFT
        SUB     PICORIGIN+H(A4),D7              ;CALC DH
        ADD     D7,PICORIGIN+H(A4)              ;UPDATE STATE VARIABLE
        SWAP    D7                              ;PUT DH IN HI WORD

        MOVE    PORTRECT+TOP(A3),D7             ;GET PORTRECT TOP
        SUB     PICORIGIN+V(A4),D7              ;CALC DV
        ADD     D7,PICORIGIN+V(A4)              ;UPDATE STATE VARIABLE

        MOVEQ   #$0C,D0
        JSR     DPutPicByte                      ;PUT ORIGIN OPCODE TO THEPIC
        MOVE.L  D7,-(SP)
        JSR     PutPicLong                      ;PUT DH,DV TO THEPIC
        MOVE.L  PICSAVE(A3),A4                  ;GET PICSAVE HANDLE
        MOVE.L  (A4),A4                         ;DE-REFERENCE PICSAVE
;
;  CHECK FOR CLIPRGN CHANGES
;
ORIGNOK CLR.B   -(SP)                           ;MAKE ROOM FOR FCN VALUE
        MOVE.L  CLIPRGN(A3),-(SP)               ;PUSH CLIPRGN
        MOVE.L  PICCLIPRGN(A4),-(SP)            ;PUSH STATE VARIABLE
        JSR     EQUALRGN                        ;ARE THEY THE SAME ?
        TST.B   (SP)+                           ;TEST RESULT
        BNE.S   CLIPOK                          ;QUIT IF SAME

        MOVE.L  CLIPRGN(A3),-(SP)               ;SET UP FOR COPYRGN BELOW
        MOVE.L  PICCLIPRGN(A4),-(SP)

        MOVEQ   #$01,D0
        JSR     DPutPicByte                     ;PUT CLIPRGN PARAM OPCODE
        MOVE.L  CLIPRGN(A3),-(SP)               ;PUSH CLIPRGN HANDLE
        JSR     PutPicRgn                       ;PUT CLIPRGN TO THEPIC

        JSR     COPYRGN                         ;COPY CLIPRGN INTO PICCLIPRGN

CLIPOK  MOVE.L  GRAFGLOBALS(A5),A4              ;POINT TO QUICKDRAW GLOBALS
        MOVEQ   #1,D0                           ;CLEAR ZERO FLAG
DONE    MOVEM.L (SP)+,D7                        ;MOVEM FOR CC, RESTORE REGS
        RTS                                     ;AND RETURN



        .PROC ScalePt,3
;-------------------------------------------------------------
;
;  PROCEDURE ScalePt(VAR pt: Point;  fromRect,toRect: Rect);
;
;  Scale a width and height from one coordinate system to another.
;  If fromRect and toRect are the same size, then no change.
;  If the input > 0, then enforce 1 pixel minimum on the output.
;
;  pt.h := (pt.h * fromWidth)  / toWidth
;  pt.v := (pt.v * fromHeight) / toHeight
;
;  restores ALL registers.
;
;  A6 OFFSETS OF PARAMS AFTER LINK:
;
PARAMSIZE       .EQU    12                      ;TOTAL BYTES OF PARAMS
PT              .EQU    PARAMSIZE+8-4           ;LONG, ADDR OF POINT
FROMRECT        .EQU    PT-4                    ;LONG, ADDR OF RECT
TORECT          .EQU    FROMRECT-4              ;LONG, ADDR OF RECT

        LINK    A6,#0                           ;NO LOCALS
        MOVEM.L D0-D6/A0-A2,-(SP)               ;SAVE REGS
        MOVE.L  PT(A6),A0                       ;POINT TO VAR PT
        MOVE.L  FROMRECT(A6),A1                 ;POINT TO FROMRECT
        MOVE.L  TORECT(A6),A2                   ;POINT TO TORECT
        BSR.S   SCALE1                          ;SCALE THE HEIGHT
        ADD     #2,A0                           ;OFFSET TO PT.H
        ADD     #2,A1                           ;OFFSET TO FROMRECT.H
        ADD     #2,A2                           ;OFFSET TO TORECT.H
        BSR.S   SCALE1                          ;SCALE THE WIDTH
DONE    MOVEM.L (SP)+,D0-D6/A0-A2               ;RESTORE REGS
        UNLINK  PARAMSIZE,'SCALEPT '


;-------------------------------------------------------
;
;  LOCAL ROUTINE TO SCALE A SINGLE WIDTH OR HEIGHT:
;  IF INPUT <= 0, THEN OUTPUT WILL BE 0.
;  IF INPUT > 0, THEN OUTPUT WILL BE AT LEAST 1.
;
SCALE1  MOVE    BOTTOM(A1),D3                   ;GET FROMRECT BOTTOM
        SUB     TOP(A1),D3                      ;CALC FROMHEIGHT
        MOVE    BOTTOM(A2),D2                   ;GET TORECT BOTTOM
        SUB.W   TOP(A2),D2                      ;CALC TORECT HEIGHT
        CMP     D2,D3                           ;ARE BOTH HEIGHTS THE SAME ?
        BEQ.S   SCDONE                          ;YES, SKIP
        MOVE    D3,D1                           ;COPY DENOM = FROMHEIGHT
        LSR     #1,D1                           ;CALC DENOM/2 (DENOM IS POS)
        EXT.L   D1                              ;MAKE IT LONG
        MOVE    (A0),D0                         ;GET SIZE COORD
        BGT.S   POS                             ;CONTINUE IF POSITIVE
        CLR     (A0)                            ;ELSE SET IT TO ZERO
        RTS                                     ;AND QUIT

POS     MULU    D2,D0                           ;MULT BY TORECT HEIGHT
        ADD.L   D1,D0                           ;ADD FROMHEIGHT/2 FOR ROUNDING
        DIVU    D3,D0                           ;DIV BY FROMHEIGHT
        BNE.S   SIZEOK                          ;IS RESULT ZERO ?
MINSIZE MOVEQ   #1,D0                           ;ENFORCE 1 PIXEL MIN SIZE
SIZEOK  MOVE    D0,(A0)                         ;UPDATE RESULT
SCDONE  RTS



        .PROC MapPt,3
;-------------------------------------------------------------
;
;  PROCEDURE MapPt(VAR pt: Point;  fromRect,toRect: Rect);
;
;  Map a point from one coordinate system to another.
;
;  pt.h := ((pt.h-fromLeft) * toWidth)  / fromWidth  + toLeft
;  pt.v := ((pt.v-fromTop)  * toHeight) / fromHeight + toTop
;
;  restores ALL registers.
;
;  A6 OFFSETS OF PARAMS AFTER LINK:
;
PARAMSIZE       .EQU    12                      ;TOTAL BYTES OF PARAMS
PT              .EQU    PARAMSIZE+8-4           ;LONG, ADDR OF POINT
FROMRECT        .EQU    PT-4                    ;LONG, ADDR OF RECT
TORECT          .EQU    FROMRECT-4              ;LONG, ADDR OF RECT

        LINK    A6,#0                           ;NO LOCALS
        MOVEM.L D0-D6/A0-A2,-(SP)               ;SAVE REGS
        MOVE.L  PT(A6),A0                       ;POINT TO VAR PT
        MOVE.L  FROMRECT(A6),A1                 ;POINT TO FROMRECT
        MOVE.L  TORECT(A6),A2                   ;POINT TO TORECT
        BSR.S   MAP1                            ;MAP THE VERTICAL COORD
        ADD     #2,A0                           ;OFFSET TO PT.H
        ADD     #2,A1                           ;OFFSET TO FROMRECT.H
        ADD     #2,A2                           ;OFFSET TO TORECT.H
        BSR.S   MAP1                            ;MAP THE HORIZ COORD
DONE    MOVEM.L (SP)+,D0-D6/A0-A2               ;RESTORE REGS
        UNLINK  PARAMSIZE,'MAPPT   '


;-----------------------------------------------
;
;  LOCAL ROUTINE TO MAP A SINGLE COORDINATE:
;
MAP1    MOVE    TOP(A1),D2                      ;GET FROMRECT TOP
        MOVE    BOTTOM(A1),D3                   ;GET FROMRECT BOTTOM
        SUB.W   D2,D3                           ;CALC FROMHEIGHT
        MOVE    TOP(A2),D4                      ;GET TORECT TOP
        MOVE    BOTTOM(A2),D5                   ;GET TORECT BOTTOM
        SUB.W   D4,D5                           ;CALC TORECT HEIGHT
        MOVE    (A0),D0                         ;GET COORD
        SUB     D2,D0                           ;SUBTRACT FROMRECT TOP
        CMP     D3,D5                           ;ARE BOTH HEIGHTS SAME ?
        BEQ.S   NOSCALE                         ;YES, SKIP THE SCALING

        MOVE    D3,D1                           ;DENOM = FROMHEIGHT
        LSR     #1,D1                           ;CALC DENOM/2 (DENOM ALWAYS POS)
        EXT.L   D1                              ;MAKE DENOM/2 LONG
        MOVE    D0,D2                           ;IS COORD NEGATIVE ?
        BPL.S   NOTNEG                          ;NO, CONTINUE
        NEG     D0                              ;YES, MAKE IT POSITIVE FOR MULDIV
NOTNEG  MULU    D5,D0                           ;MULT COORD BY TORECT HEIGHT
        ADD.L   D1,D0                           ;ADD FROMHEIGHT/2 FOR ROUNDING
        DIVU    D3,D0                           ;DIV BY FROMHEIGHT
        TST     D2                              ;WAS OLD COORD NEG ?
        BPL.S   NOSCALE                         ;NO, CONTINUE
        NEG     D0                              ;YES, RESTORE IT TO NEGATIVE

NOSCALE ADD     D4,D0                           ;ADD TORECT TOP
        MOVE    D0,(A0)                         ;UPDATE RESULT
        RTS



        .PROC MapRect,3
        .REF  MapPt
;-------------------------------------------------------------
;
;  PROCEDURE MapRect(VAR dstRect: Rect;  fromRect,toRect: Rect);
;
;  Map both points of a rectangle from one coordinate system to another.
;  restores ALL registers.
;
        MOVE.L  12(SP),-(SP)                    ;PUSH DSTRECT
        MOVE.L  12(SP),-(SP)                    ;PUSH FROMRECT
        MOVE.L  12(SP),-(SP)                    ;PUSH TORECT
        JSR     MAPPT                           ;MAP TOPLEFT POINT
        ADD.L   #4,12(SP)                       ;OFFSET RECT TO BOTRIGHT
        JMP     MAPPT                           ;MAP BOTRIGHT AND RETURN



        .END
