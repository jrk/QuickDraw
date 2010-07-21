        .INCLUDE GRAFTYPES.TEXT

;------------------------------------------------------------------
;
;  --> LCURSOR.TEXT
;
;  Links to MacIntosh Cursor routines.
;
;  System Graphic Jump Vectors:
;
;  Long pointers to system routine entry points.
;
GRAFBEGIN       .EQU    $800                    ;GRAF GLOBAL AREA
JHIDECURSOR     .EQU    GRAFBEGIN
JSHOWCURSOR     .EQU    JHIDECURSOR+4
JSHIELDCURSOR   .EQU    JSHOWCURSOR+4
JSCRNADDR       .EQU    JSHIELDCURSOR+4         ;not used (see _GetScrnBits)
JSCRNSIZE       .EQU    JSCRNADDR+4             ;not used (see _GetScrnBits)
JINITCRSR       .EQU    JSCRNSIZE+4
JSETCRSR        .EQU    JINITCRSR+4
JCRSROBSCURE    .EQU    JSETCRSR+4




        .PROC InitCursor,0
        .REF  SetCursor
;----------------------------------------------------------
;
;  PROCEDURE InitCursor;
;
        MOVE.L  GRAFGLOBALS(A5),A0              ;POINT TO QUICKDRAW GLOBALS
        PEA     ARROW(A0)                       ;PUSH ADDR OF ARROW
        JSR     SETCURSOR                       ;INSTALL ARROW CURSOR
        MOVE.L  JInitCrsr,A0                    ;get lo mem pointer
        JMP     (A0)                            ;and call it



        .PROC SetCursor,1
;---------------------------------------------------
;
;  PROCEDURE SetCursor(crsr: Cursor);
;
        MOVE.L  4(SP),A0                        ;Point to Cursor
        MOVE.L  HOTSPOT+V(A0),-(SP)             ;PUSH HOTX & HOTY
        MOVE    #16,-(SP)                       ;HEIGHT:=16
        PEA     DATA(A0)                        ;PUSH ADDR OF DATA
        PEA     MASK(A0)                        ;PUSH ADDR OF MASK
        MOVE.L  JSetCrsr,A0                     ;get lo mem vector
        JSR     (A0)                            ;call vector
        MOVE.L  (SP)+,(SP)                      ;strip param
        RTS                                     ;and return



        .PROC HideCursor,0
;---------------------------------------------------------
;
;  PROCEDURE HideCursor;
;
;  ALL REGS PRESERVED.
;
        MOVE.L  JHideCursor,-(SP)               ;get lo mem vector
        RTS                                     ;and call it



        .PROC ShowCursor,0
;---------------------------------------------------------
;
;  PROCEDURE ShowCursor;
;
;  ALL REGS PRESERVED.
;
        MOVE.L  JShowCursor,-(SP)               ;get lo mem vector
        RTS                                     ;and call it



        .PROC ShieldCursor,2
;---------------------------------------------------------
;
;  PROCEDURE ShieldCursor(shieldRect: Rect; offset: Point);
;
;  ALL REGS PRESERVED.
;
        MOVEM.L D0-D3/A0-A1,-(SP)               ;SAVE REGS
        MOVE.L  32(SP),A0                       ;POINT TO SHIELDRECT
        MOVEM.W (A0)+,D0/D1/D2/D3               ;GET TOP ... RIGHT
        LEA     28(SP),A1
        SUB     (A1),D0                         ;TOP - OFFSET.V
        SUB     (A1)+,D2                        ;BOTTOM - OFFSET.V
        SUB     (A1),D1                         ;LEFT - OFFSET.H
        SUB     (A1),D3                         ;RIGHT - OFFSET.H
        MOVE    D1,-(SP)                        ;PUSH GLOBAL LEFT
        MOVE    D0,-(SP)                        ;PUSH GLOBAL TOP
        MOVE    D3,-(SP)                        ;PUSH GLOBAL RIGHT
        MOVE    D2,-(SP)                        ;PUSH GLOBAL BOTTOM
        MOVE.L  JShieldCursor,A0                ;get lo mem vector
        JSR     (A0)                            ;and call it
        MOVEM.L (SP)+,D0-D3/A0-A1               ;RESTORE REGS
        MOVE.L  (SP)+,(SP)
        MOVE.L  (SP)+,(SP)                      ;STRIP 8 BYTES
        RTS                                     ;AND RETURN



        .PROC ObscureCursor,0
;---------------------------------------------------------
;
;  PROCEDURE ObscureCursor;
;
;  Hide the cursor image until the next time the mouse moves.
;
        MOVE.L  JCrsrObscure,A0                 ;get lo mem vector
        JMP     (A0)                            ;and call it




        .END
