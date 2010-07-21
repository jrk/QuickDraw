        .INCLUDE  GRAFTYPES.TEXT
;------------------------------------------------------------
;
;  --> GRAFASM.TEXT
;
;  Miscellaneous unclassified routines.
;


        .PROC InitGraf,1
;--------------------------------------------------
;
;  PROCEDURE InitGraf(globalPtr: Ptr);
;
;
PARAMSIZE       .EQU    4
GLOBALPTR       .EQU    PARAMSIZE+8-4           ;LONG


        LINK    A6,#0                           ;NO LOCALS
        MOVE.L  A4,-(SP)                        ;SAVE REG
        MOVE.L  GLOBALPTR(A6),A4                ;GET POINTER TO QUICKDRAW GLOBALS
        MOVE.L  A4,GRAFGLOBALS(A5)              ;SAVE IN MAGIC LOCATION
;
; new addition 22 Apr 85
;
        CLR.B   $8F3                            ; set lo-mem flag, QDExist

        LEA     lastGrafGlob(A4),A0             ;SET UP START POINTER
        LEA     thePort+4(A4),A1                ;SET UP LIMIT POINTER
CLRLP   CLR.W   (A0)+                           ;CLEAR A WORD
        CMPA.L  A1,A0                           ;CHECK LIMIT POINTER
        BNE     CLRLP                           ;CLEAR ALL GLOBALS

                                                ;QDSpareD..QDSpare3 = all zeros
                                                ;playIndex := 0
                                                ;fontPtr = Nil
                                                ;FixTxWid := 0.0
                                                ;patAlign := (0,0)
                                                ;polyMax := 0
                                                ;thePoly := Nil
                                                ;QDSpare0 := 0
                                                ;playPic := Nil
                                                ;rgnMax := 0
                                                ;rgnIndex := 0
                                                ;rgnBuf := Nil
        LEA     wideData(A4),A4
        MOVE.L  A4,D0                           ;REMEMBER ADDR OF WIDEDATA
        MOVE    #10,(A4)+                       ;wideData.rgnSize := 10
        MOVE.L  #$80018001,(A4)+                ;wideData.rgnBBox :=
        MOVE.L  #$7FFF7FFF,(A4)+                ;(-32767,-32767,32767,32767)
        MOVE.L  A4,D1                           ;REMEMBER ADDR OF WIDEMASTER
        MOVE.L  D0,(A4)+                        ;wideMaster := @wideData
        MOVE.L  D1,(A4)+                        ;wideOpen := @wideMaster
        MOVEQ   #1,D0
        MOVE.L  D0,(A4)+                        ;randSeed := 1
        MOVE.L  A4,-(SP)                        ;point to screenBits
        _GetScrnBits                            ;fill in screenBits
        ADD     #14,A4                          ;bump past screenBits
        MOVEQ   #26,D0                          ;INIT LOOP COUNT
        LEA     CURDATA,A0                      ;POINT TO CURSOR DATA
CRSRLP  MOVE.L  (A0)+,(A4)+                     ;COPY A LONG INTO GLOBALS
        DBRA    D0,CRSRLP                       ;LOOP FOR 27 LONGS
                                                ;thePort := NIL
        MOVE.L  (SP)+,A4                        ;RESTORE REG
        UNLINK  PARAMSIZE,'INITGRAF'


CURDATA .WORD   $0000,$4000,$6000,$7000         ;ARROW.DATA
        .WORD   $7800,$7C00,$7E00,$7F00
        .WORD   $7F80,$7C00,$6C00,$4600
        .WORD   $0600,$0300,$0300,$0000

        .WORD   $C000,$E000,$F000,$F800         ;ARROW.MASK
        .WORD   $FC00,$FE00,$FF00,$FF80
        .WORD   $FFC0,$FFE0,$FE00,$EF00
        .WORD   $CF00,$8780,$0780,$0380

        .WORD   $0001,$0001                     ;ARROW.HOTSPOT := (1,1)

        .LONG   $77DD77DD,$77DD77DD             ;dkGray
        .LONG   $88228822,$88228822             ;ltGray
        .LONG   $AA55AA55,$AA55AA55             ;gray
        .LONG   $FFFFFFFF,$FFFFFFFF             ;black
        .LONG   $00000000,$00000000             ;white



        .PROC OpenPort,1
        .REF  NewRgn
;-------------------------------------------------------------
;
;  PROCEDURE OpenPort(port: GrafPtr);
;  { allocate clipRgn and visRgn, then call InitPort.
;
        CLR.L   -(SP)                           ;MAKE ROOM FOR FUNCTION RESULT
        JSR     NEWRGN                          ;ALLOCATE A NEW REGION
        CLR.L   -(SP)                           ;MAKE ROOM FOR FUNCTION RESULT
        JSR     NEWRGN                          ;ALLOCATE A SECOND NEW REGION
        MOVE.L  12(SP),A0                       ;POINT TO PORT
        MOVE.L  (SP)+,CLIPRGN(A0)               ;INSTALL NEW REGION INTO CLIPRGN
        MOVE.L  (SP)+,VISRGN(A0)                ;AND OTHER INTO VISRGN
                                                ;FALL THRU TO InitPort


        .PROC InitPort,1
        .REF  RectRgn,CopyRgn
;-------------------------------------------------------------
;
;  PROCEDURE InitPort(port: GrafPtr);
;
;  { initialize all fields of an existing GrafPort }
;
        MOVE.L  GRAFGLOBALS(A5),A0              ;POINT TO QUICKDRAW GLOBALS
        MOVE.L  4(SP),A1                        ;GET PORT PARAM
        MOVE.L  A1,THEPORT(A0)                  ;SetPort(port)
        CLR.W   (A1)+                           ;DEVICE := 0
        LEA     SCREENBITS(A0),A0               ;POINT TO SCREENBITS
        MOVE.L  (A0)+,(A1)+                     ;portBits := screenBits
        MOVE.W  (A0)+,(A1)+                     ;COPY ROWBYTES
        MOVE.L  (A0),(A1)+                      ;COPY TOPLEFT
        MOVE.L  4(A0),(A1)+                     ;COPY BOTRIGHT
        MOVE.L  (A0),(A1)+                      ;portRect := screenBits.bounds
        MOVE.L  4(A0),(A1)+                     ;all 8 bytes
        MOVE.L  (A1)+,-(SP)                     ;visRgn := screenBits.bounds
        MOVE.L  A0,-(SP)
        JSR     RECTRGN
        MOVE.L  4(SP),A1                        ;GET PORT PARAM
        MOVE.L  GRAFGLOBALS(A5),A0              ;POINT TO QUICKDRAW GLOBALS
        MOVE.L  WIDEOPEN(A0),-(SP)              ;PUSH WIDE OPEN RGN
        MOVE.L  CLIPRGN(A1),-(SP)               ;PUSH CLIPRGN
        JSR     COPYRGN                         ;SET TO WIDE OPEN
        MOVE.L  4(SP),A1                        ;GET PORT PARAM
        LEA     BKPAT(A1),A1
        CLR.L   (A1)+                           ;bkPat := white
        CLR.L   (A1)+
        MOVEQ   #-1,D0
        MOVE.L  D0,(A1)+                        ;fillPat := Black
        MOVE.L  D0,(A1)+
        CLR.L   (A1)+                           ;pnLoc := (0,0)
        MOVE.L  #$00010001,(A1)+                ;pnSize := (1,1)
        MOVE    #8,(A1)+                        ;pnMode := patCopy
        MOVE.L  D0,(A1)+                        ;pnPat := black
        MOVE.L  D0,(A1)+
        CLR.W   (A1)+                           ;pnVis := 0
        CLR.L   (A1)+                           ;txFont, txFace := 0
        MOVE    #1,(A1)+                        ;txMode := srcOr
        CLR     (A1)+                           ;txSize := 0
        CLR.L   (A1)+                           ;spExtra := 0.0
        MOVE.L  #blackColor,(A1)+               ;fgColor := blackColor
        MOVE.L  #whiteColor,(A1)+               ;bkColor := whiteColor
        CLR.L   (A1)+                           ;colrBit,patStretch := 0
        CLR.L   (A1)+                           ;picSave := Nil
        CLR.L   (A1)+                           ;rgnSave := Nil
        CLR.L   (A1)+                           ;polySave := Nil
        CLR.L   (A1)+                           ;grafProcs := Nil
        MOVE.L  (SP)+,(SP)                      ;STRIP PARAM
        RTS                                     ;AND RETURN



        .PROC ClosePort,1
;-------------------------------------------------------------
;
;  PROCEDURE ClosePort(port: GrafPtr);
;
;  { just disposes of clipRgn and visRgn }
;
        MOVE.L  4(SP),A0                        ;GET PORT
        MOVE.L  CLIPRGN(A0),A0                  ;GET CLIPRGN HANDLE
        _DisposHandle                           ;DISCARD IT
        MOVE.L  4(SP),A0                        ;GET PORT
        MOVE.L  VISRGN(A0),A0                   ;GET VISRGN HANDLE
        _DisposHandle                           ;DISCARD IT
        MOVE.L  (SP)+,(SP)                      ;STRIP PARAM
        RTS                                     ;AND RETURN


        .PROC SetStdProcs,1
        .REF  StdText,StdLine,StdRect,StdRRect,StdOval,StdArc,StdPoly
        .REF  StdRgn,StdBits,StdComment,StdTxMeas,StdGetPic,StdPutPic
;-------------------------------------------------------------
;
;  PROCEDURE SetStdProcs(VAR procs: QDProcs);
;
        MOVE.L  4(SP),A1                        ;GET ADDRESS OF PROC RECORD
        LEA     StdText,A0
        MOVE.L  A0,(A1)+
        LEA     StdLine,A0
        MOVE.L  A0,(A1)+
        LEA     StdRect,A0
        MOVE.L  A0,(A1)+
        LEA     StdRRect,A0
        MOVE.L  A0,(A1)+
        LEA     StdOval,A0
        MOVE.L  A0,(A1)+
        LEA     StdArc,A0
        MOVE.L  A0,(A1)+
        LEA     StdPoly,A0
        MOVE.L  A0,(A1)+
        LEA     StdRgn,A0
        MOVE.L  A0,(A1)+
        LEA     StdBits,A0
        MOVE.L  A0,(A1)+
        LEA     StdComment,A0
        MOVE.L  A0,(A1)+
        LEA     StdTxMeas,A0
        MOVE.L  A0,(A1)+
        LEA     StdGetPic,A0
        MOVE.L  A0,(A1)+
        LEA     StdPutPic,A0
        MOVE.L  A0,(A1)+
        MOVE.L  (SP)+,(SP)                      ;STRIP PARAM
        RTS                                     ;AND RETURN



        .PROC LocalToGlobal,1
        .DEF  GlobalToLocal,AddPt,SubPt,SetPort,GetPort
;-------------------------------------------------------------
;
;  PROCEDURE LocalToGlobal(VAR pt: Point);
;
;  restores all registers.
;
        MOVEM.L D0-D2/A0/A1,-(SP)               ;SAVE REGS
        MOVE.L  #1,D2                           ;INDICATE SUB
        BRA.S   SHARE



;-------------------------------------------------------------
;
;  PROCEDURE GlobalToLocal(VAR pt: Point);
;
;  restores all registers.
;
GlobalToLocal
        MOVEM.L D0-D2/A0/A1,-(SP)               ;SAVE REGS
        MOVE.L  #0,D2                           ;INDICATE ADD
SHARE   MOVE.L  GRAFGLOBALS(A5),A0              ;POINT TO QUICKDRAW GLOBALS
        MOVE.L  THEPORT(A0),A0                  ;POINT TO CURRENT GRAFPORT
        MOVE.L  24(SP),A1                       ;POINT TO VAR PT
        MOVE    PORTBITS+BOUNDS+TOP(A0),D0      ;GET DV
        MOVE    PORTBITS+BOUNDS+LEFT(A0),D1     ;GET DH
        BSR.S   ADDSUB                          ;CONVERT TO LOCAL
        MOVEM.L (SP)+,D0-D2/A0/A1               ;RESTORE REGS
        BRA.S   SHARE3                          ;STRIP 4 BYTES AND RETURN
;
;
;
ADDSUB  TST     D2
        BEQ.S   JUSTADD
        NEG     D0
        NEG     D1
JUSTADD ADD     D0,(A1)+
        ADD     D1,(A1)+
        RTS



;-------------------------------------------------------------
;
;  PROCEDURE AddPt(src: Point; VAR dst: Point);
;  { add two points together, restores all regs }
;
AddPt   MOVEM.L D0-D2/A1,-(SP)                  ;SAVE REGS
        MOVE.L  #0,D2                           ;INDICATE ADD
        BRA.S   SHARE2



;-------------------------------------------------------------
;
;  PROCEDURE SubPt(src: Point; VAR dst: Point);
;  { subtract src Point from dst point, restores all regs }
;
SubPt   MOVEM.L D0-D2/A1,-(SP)                  ;SAVE REGS
        MOVE.L  #1,D2                           ;INDICATE SUB
SHARE2  MOVE.L  20(SP),A1                       ;POINT TO DST
        MOVE    24+V(SP),D0                     ;GET SRC.V
        MOVE    24+H(SP),D1                     ;GET SRC.H
        BSR.S   ADDSUB
        MOVEM.L (SP)+,D0-D2/A1                  ;RESTORE REGS
        MOVE.L  (SP)+,(SP)
SHARE3  MOVE.L  (SP)+,(SP)
        RTS                                     ;AND RETURN



;----------------------------------------------------------
;
;  PROCEDURE SetPort(gp: GrafPtr);
;  { switch the current port to a different GrafPort }
;
SetPort MOVE.L  GRAFGLOBALS(A5),A0              ;POINT TO QUICKDRAW GLOBALS
        MOVE.L  4(SP),THEPORT(A0)               ;INSTALL INTO THEPORT
        BRA.S   SHARE3                          ;STRIP 4 BYTES AND RETURN



;----------------------------------------------------------
;
;  PROCEDURE GetPort(VAR gp: GrafPtr);
;  { inquire the current GrafPort }
;
GetPort MOVE.L  GRAFGLOBALS(A5),A0              ;POINT TO QUICKDRAW GLOBALS
        MOVE.L  4(SP),A1                        ;POINT TO VAR GP
        MOVE.L  THEPORT(A0),(A1)                ;COPY FROM THEPORT
        BRA.S   SHARE3                          ;STRIP 4 BYTES AND RETURN



        .PROC GrafDevice,1
        .REF  PortWord
;----------------------------------------------------------
;
;  PROCEDURE GrafDevice(device: INTEGER);
;
        MOVEQ   #DEVICE,D0                      ;PUT PORT OFFSET IN D0
        JMP     PORTWORD                        ;INSTALL PARAM INTO THEPORT



        .PROC SetPortBits,1
        .DEF  BackPat
;----------------------------------------------------------
;
;  PROCEDURE SetPortBits(bm: BitMap);
;  { re-direct output to a different BitMap }
;
        MOVE.L  GRAFGLOBALS(A5),A0              ;POINT TO QUICKDRAW GLOBALS
        MOVE.L  THEPORT(A0),A0                  ;GET CURRENT GRAFPORT
        LEA     PORTBITS(A0),A0                 ;POINT TO PORTBITS
        MOVE.L  4(SP),A1                        ;POINT TO BITMAP
        MOVE.L  (A1)+,(A0)+                     ;COPY BASEADDR
        MOVE.W  (A1)+,(A0)+                     ;COPY ROWBYTES
SHARE   MOVE.L  (A1)+,(A0)+                     ;COPY BOUNDS.TOPLEFT
        MOVE.L  (A1)+,(A0)+                     ;COPY BOUNDS.BOTRIGHT
        MOVE.L  (SP)+,(SP)                      ;STRIP 4 BYTES
        RTS                                     ;AND RETURN



;----------------------------------------------------------
;
;  PROCEDURE BackPat(pat: Pattern);
;  { set the background pattern }
;
BackPat MOVE.L  GRAFGLOBALS(A5),A0              ;POINT TO QUICKDRAW GLOBALS
        MOVE.L  THEPORT(A0),A0                  ;GET CURRENT GRAFPORT
        LEA     BKPAT(A0),A0                    ;POINT TO BKPAT
        MOVE.L  4(SP),A1                        ;GET ADDR OF PATTERN
        BRA.S   SHARE



        .PROC PortSize,2
;----------------------------------------------------------
;
;  PROCEDURE PortSize(width,height: INTEGER);
;
        MOVE.L  GRAFGLOBALS(A5),A0              ;POINT TO QUICKDRAW GLOBALS
        MOVE.L  THEPORT(A0),A0                  ;GET CURRENT GRAFPORT
        MOVE    PORTRECT+LEFT(A0),D0            ;GET PORTRECT.LEFT
        ADD     6(SP),D0                        ;ADD WIDTH
        MOVE    D0,PORTRECT+RIGHT(A0)           ;UPDATE PORTRECT.RIGHT
        MOVE    PORTRECT+TOP(A0),D0             ;GET PORTRECT.TOP
        ADD     4(SP),D0                        ;ADD HEIGHT
        MOVE    D0,PORTRECT+BOTTOM(A0)          ;UPDATE PORTRECT.BOTTOM
        MOVE.L  (SP)+,(SP)                      ;STRIP 4 BYTES
        RTS                                     ;AND RETURN



        .PROC MovePortTo,2
        .DEF  SetOrigin,ClipRect
        .REF  OffsetRgn,RectRgn
;----------------------------------------------------------
;
;  PROCEDURE MovePortTo(leftGlobal,topGlobal: INTEGER);
;  { move portRect to a different part of the bitmap }
;
        MOVE.L  GRAFGLOBALS(A5),A0              ;POINT TO QUICKDRAW GLOBALS
        MOVE.L  THEPORT(A0),A0                  ;GET CURRENT GRAFPORT
        MOVE    PORTRECT+LEFT(A0),D0            ;GET PORTRECT.LEFT
        SUB     PORTBITS+BOUNDS+LEFT(A0),D0     ;CONVERT TO GLOBAL
        SUB     6(SP),D0                        ;SUB LEFTGLOBAL FOR DH
        MOVE    PORTRECT+TOP(A0),D1             ;GET PORTRECT.TOP
        SUB     PORTBITS+BOUNDS+TOP(A0),D1      ;CONVERT TO GLOBAL
        SUB     4(SP),D1                        ;SUB TOPGLOBAL FOR DV
        MOVE.L  (SP)+,(SP)                      ;STRIP 4 BYTES
OFSPORT LEA     PORTBITS+BOUNDS(A0),A0          ;OFFSET PORTBITS.BOUNDS DH,DV
OFSRECT ADD     D1,(A0)+                        ;OFFSET TOP
        ADD     D0,(A0)+                        ;OFFSET LEFT
        ADD     D1,(A0)+                        ;OFFSET BOTTOM
        ADD     D0,(A0)+                        ;OFFSET RIGHT
        RTS                                     ;AND RETURN



;----------------------------------------------------------
;
;  PROCEDURE SetOrigin(h,v: INTEGER);
;  { re-define the local coords by adjusting portBits.bounds, }
;  { portRect, and visRgn }
;
SetOrigin
        MOVE.L  GRAFGLOBALS(A5),A0              ;POINT TO QUICKDRAW GLOBALS
        MOVE.L  THEPORT(A0),A0                  ;GET CURRENT GRAFPORT
        MOVE.L  4(SP),D0                        ;GET V AND H BOTH
        CMP.L   PORTRECT+TOPLEFT(A0),D0         ;SAME AS ALREADY IN THEPORT ?
        BEQ.S   DONE                            ;YES, QUIT
        MOVE    6(SP),D0                        ;GET H
        SUB     PORTRECT+LEFT(A0),D0            ;DH:=H-PORTRECT.LEFT
        MOVE    4(SP),D1                        ;GET V
        SUB     PORTRECT+TOP(A0),D1             ;DV:=V-PORTRECT.TOP
        MOVE.L  VISRGN(A0),-(SP)                ;PUSH PARMS FOR LATER
        MOVE    D0,-(SP)
        MOVE    D1,-(SP)
        BSR.S   OFSPORT                         ;OFFSET PORTBITS.BOUNDS
        LEA     PORTRECT-PORTBITS-BOUNDS-8(A0),A0 ;POINT A0 AT PORTRECT
        BSR.S   OFSRECT                         ;OFFSET PORTRECT
        JSR     OFFSETRGN
DONE    MOVE.L  (SP)+,(SP)                      ;STRIP 4 BYTES
        RTS                                     ;AND RETURN



;----------------------------------------------------------
;
;  PROCEDURE ClipRect(r: Rect);
;  { Make the current grafport's clipRgn match a given rectangle }
;
ClipRect
        MOVE.L  GRAFGLOBALS(A5),A0              ;POINT TO QUICKDRAW GLOBALS
        MOVE.L  THEPORT(A0),A0                  ;GET CURRENT GRAFPORT
        MOVE.L  CLIPRGN(A0),-(SP)               ;PUSH CLIPRGN
        MOVE.L  8(SP),-(SP)                     ;PUCH ADDR OF RECT
        JSR     RECTRGN
        BRA.S   DONE



        .PROC SetClip,1
        .DEF  GetClip
        .REF  CopyRgn
;----------------------------------------------------------
;
;  PROCEDURE SetClip(rgn: RgnHandle);
;
;  copy rgn into theport^.clipRgn
;
        MOVE.L  (SP)+,A0                        ;POP RETURN ADDR
        MOVE.L  GRAFGLOBALS(A5),A1              ;POINT TO QUICKDRAW GLOBALS
        MOVE.L  THEPORT(A1),A1                  ;GET CURRENT GRAFPORT
        MOVE.L  CLIPRGN(A1),-(SP)               ;PUSH THEPORT^.CLIPRGN
        BRA.S   SHARE



;----------------------------------------------------------
;
;  PROCEDURE GetClip(rgn: RgnHandle);
;
;  copy from theport^.clipRgn into rgn.
;
GetClip
        MOVE.L  (SP)+,A0                        ;POP RETURN ADDR
        MOVE.L  (SP)+,D0                        ;POP RGN HANDLE
        MOVE.L  GRAFGLOBALS(A5),A1              ;POINT TO QUICKDRAW GLOBALS
        MOVE.L  THEPORT(A1),A1                  ;GET CURRENT GRAFPORT
        MOVE.L  CLIPRGN(A1),-(SP)               ;PUSH THEPORT^.CLIPRGN
        MOVE.L  D0,-(SP)                        ;PUSH RGN
SHARE   MOVE.L  A0,-(SP)                        ;RESTORE RETURN ADDR
        JMP     COPYRGN                         ;AND GO TO COPYRGN



        .PROC SetPt,3
;-------------------------------------------------------------
;
;  PROCEDURE SetPt(VAR pt: Point; h,v: INTEGER);
;
        MOVE.L  (SP)+,A0                        ;POP RETURN ADDR
        MOVE.L  (SP)+,D0                        ;POP H,V
        MOVE.L  (SP)+,A1                        ;POP VAR ADDR OF PT
        MOVE.L  D0,(A1)                         ;STORE H,V INTO PT
        JMP     (A0)                            ;RETURN


        .FUNC EqualPt,2
;----------------------------------------------------------
;
;  FUNCTION  EqualPt(pt1,pt2: Point): BOOLEAN;
;
;  CLOBBERS D0,A0.
;
        MOVE.L  (SP)+,A0                        ;POP RETURN ADDR
        MOVE.L  (SP)+,D0                        ;pop point1
        CMP.L   (SP)+,D0                        ;is point2 = point1 ?
        SEQ     (SP)                            ;IF YES, SET TO TRUE
        NEG.B   (SP)                            ;CONVERT -1 TO 1
        JMP     (A0)                            ;RETURN


        .END
