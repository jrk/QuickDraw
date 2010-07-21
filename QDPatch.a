;
;       QuickDraw patch program
;
;--------------------------------------------------------------------------------

            .NoList
            .Include    TLASM:GrafEQU.TEXT            ; All definitions
            .Include    TLASM:SYSEQU.TEXT            ; All definitions
            .Include    TLASM:SYSMACS.TEXT            ; All definitions
            .Include    TLASM:TOOLMACS.TEXT            ; All definitions
            .List


            .Main       BlockHead

            LEA         fastStart,A0

            MOVE.L      #30000,D0                ; copy into system
            _NewHandle  ,SYS
            _HLock

            MOVE.L      (A0),A1
            MOVE.L      A1,A2                   ; save ptr
            LEA         fastStart,A0
            MOVE.L      #30000,D0                ; copy into system
            _BlockMove

            JSR         (A2)

            _ExitToShell

fastStart
                .REF   CopyMask
                LEA     CopyMask,A0
                MOVE.L  A0,$C00+<4*$32>         ;Set new trap address kludge

                .REF   GetMaskTab
                LEA     GetMaskTab,A0
                MOVE.L  A0,$C00+<4*$36>         ;Set new trap address kludge

                .REF   MeasureText
                LEA     MeasureText,A0
                MOVE.L  A0,$C00+<4*$37>         ;Set new trap address kludge

                .REF   CalcMask
                LEA     CalcMask,A0
                MOVE.L  A0,$C00+<4*$38>         ;Set new trap address kludge

                .REF   SeedFill
                LEA     SeedFill,A0
                MOVE.L  A0,$C00+<4*$39>         ;Set new trap address kludge

                .REF   InitCurs
                LEA     InitCurs,A0
                MOVE    #$50,D0
                _SetTrapAddress

                .REF   SetCurso
                LEA     SetCurso,A0
                MOVE    #$51,D0
                _SetTrapAddress

                .REF   HideCurs
                LEA     HideCurs,A0
                MOVE    #$52,D0
                _SetTrapAddress

                .REF   ShowCurs
                LEA     ShowCurs,A0
                MOVE    #$53,D0
                _SetTrapAddress


                .REF   ShieldCu
                LEA     ShieldCu,A0
                MOVE    #$55,D0
                _SetTrapAddress


                .REF   ObscureC
                LEA     ObscureC,A0
                MOVE    #$56,D0
                _SetTrapAddress


                .REF   BitAnd
                LEA     BitAnd  ,A0
                MOVE    #$58,D0
                _SetTrapAddress


                .REF   BitXOr
                LEA     BitXOr  ,A0
                MOVE    #$59,D0
                _SetTrapAddress


                .REF   BitNot
                LEA     BitNot  ,A0
                MOVE    #$5A,D0
                _SetTrapAddress


                .REF   BitOr
                LEA     BitOr   ,A0
                MOVE    #$5B,D0
                _SetTrapAddress


                .REF   BitShift
                LEA     BitShift,A0
                MOVE    #$5C,D0
                _SetTrapAddress


                .REF   BitTst
                LEA     BitTst  ,A0
                MOVE    #$5D,D0
                _SetTrapAddress


                .REF   BitSet
                LEA     BitSet  ,A0
                MOVE    #$5E,D0
                _SetTrapAddress


                .REF   BitClr
                LEA     BitClr  ,A0
                MOVE    #$5F,D0
                _SetTrapAddress


                .REF   Random
                LEA     Random  ,A0
                MOVE    #$61,D0
                _SetTrapAddress


                .REF   ForeColo
                LEA     ForeColo,A0
                MOVE    #$62,D0
                _SetTrapAddress


                .REF   BackColo
                LEA     BackColo,A0
                MOVE    #$63,D0
                _SetTrapAddress


                .REF   ColorBit
                LEA     ColorBit,A0
                MOVE    #$64,D0
                _SetTrapAddress


                .REF   GetPixel
                LEA     GetPixel,A0
                MOVE    #$65,D0
                _SetTrapAddress


                .REF   StuffHex
                LEA     StuffHex,A0
                MOVE    #$66,D0
                _SetTrapAddress


                .REF   InitPort
                LEA     InitPort,A0
                MOVE    #$6D,D0
                _SetTrapAddress


                .REF   InitGraf
                LEA     InitGraf,A0
                MOVE    #$6E,D0
                _SetTrapAddress


                .REF   OpenPort
                LEA     OpenPort,A0
                MOVE    #$6F,D0
                _SetTrapAddress


                .REF   LocalToG
                LEA     LocalToG,A0
                MOVE    #$70,D0
                _SetTrapAddress


                .REF   GlobalTo
                LEA     GlobalTo,A0
                MOVE    #$71,D0
                _SetTrapAddress


                .REF   GrafDevi
                LEA     GrafDevi,A0
                MOVE    #$72,D0
                _SetTrapAddress


                .REF   SetPort
                LEA     SetPort ,A0
                MOVE    #$73,D0
                _SetTrapAddress


                .REF   GetPort
                LEA     GetPort ,A0
                MOVE    #$74,D0
                _SetTrapAddress


                .REF   SetPortB
                LEA     SetPortB,A0
                MOVE    #$75,D0
                _SetTrapAddress


                .REF   PortSize
                LEA     PortSize,A0
                MOVE    #$76,D0
                _SetTrapAddress


                .REF   MovePort
                LEA     MovePort,A0
                MOVE    #$77,D0
                _SetTrapAddress


                .REF   SetOrigi
                LEA     SetOrigi,A0
                MOVE    #$78,D0
                _SetTrapAddress


                .REF   SetClip
                LEA     SetClip ,A0
                MOVE    #$79,D0
                _SetTrapAddress


                .REF   GetClip
                LEA     GetClip ,A0
                MOVE    #$7A,D0
                _SetTrapAddress


                .REF   ClipRect
                LEA     ClipRect,A0
                MOVE    #$7B,D0
                _SetTrapAddress


                .REF   BackPat
                LEA     BackPat ,A0
                MOVE    #$7C,D0
                _SetTrapAddress


                .REF   ClosePor
                LEA     ClosePor,A0
                MOVE    #$7D,D0
                _SetTrapAddress


                .REF   AddPt
                LEA     AddPt   ,A0
                MOVE    #$7E,D0
                _SetTrapAddress


                .REF   SubPt
                LEA     SubPt   ,A0
                MOVE    #$7F,D0
                _SetTrapAddress


                .REF   SetPt
                LEA     SetPt   ,A0
                MOVE    #$80,D0
                _SetTrapAddress


                .REF   EqualPt
                LEA     EqualPt ,A0
                MOVE    #$81,D0
                _SetTrapAddress


                .REF   StdText
                LEA     StdText ,A0
                MOVE    #$82,D0
                _SetTrapAddress


                .REF   DrawChar
                LEA     DrawChar,A0
                MOVE    #$83,D0
                _SetTrapAddress


                .REF   DrawStri
                LEA     DrawStri,A0
                MOVE    #$84,D0
                _SetTrapAddress


                .REF   DrawText
                LEA     DrawText,A0
                MOVE    #$85,D0
                _SetTrapAddress


                .REF   TextWidt
                LEA     TextWidt,A0
                MOVE    #$86,D0
                _SetTrapAddress


                .REF   TextFont
                LEA     TextFont,A0
                MOVE    #$87,D0
                _SetTrapAddress


                .REF   TextFace
                LEA     TextFace,A0
                MOVE    #$88,D0
                _SetTrapAddress


                .REF   TextMode
                LEA     TextMode,A0
                MOVE    #$89,D0
                _SetTrapAddress


                .REF   TextSize
                LEA     TextSize,A0
                MOVE    #$8A,D0
                _SetTrapAddress


                .REF   GetFontI
                LEA     GetFontI,A0
                MOVE    #$8B,D0
                _SetTrapAddress


                .REF   StringWi
                LEA     StringWi,A0
                MOVE    #$8C,D0
                _SetTrapAddress


                .REF   CharWidt
                LEA     CharWidt,A0
                MOVE    #$8D,D0
                _SetTrapAddress


                .REF   SpaceExt
                LEA     SpaceExt,A0
                MOVE    #$8E,D0
                _SetTrapAddress


                .REF   StdLine
                LEA     StdLine ,A0
                MOVE    #$90,D0
                _SetTrapAddress


                .REF   LineTo
                LEA     LineTo  ,A0
                MOVE    #$91,D0
                _SetTrapAddress


                .REF   Line
                LEA     Line    ,A0
                MOVE    #$92,D0
                _SetTrapAddress


                .REF   MoveTo
                LEA     MoveTo  ,A0
                MOVE    #$93,D0
                _SetTrapAddress


                .REF   Moov
                LEA     Moov    ,A0
                MOVE    #$94,D0
                _SetTrapAddress


                .REF   HidePen
                LEA     HidePen ,A0
                MOVE    #$96,D0
                _SetTrapAddress


                .REF   ShowPen
                LEA     ShowPen ,A0
                MOVE    #$97,D0
                _SetTrapAddress


                .REF   GetPenSt
                LEA     GetPenSt,A0
                MOVE    #$98,D0
                _SetTrapAddress


                .REF   SetPenSt
                LEA     SetPenSt,A0
                MOVE    #$99,D0
                _SetTrapAddress


                .REF   GetPen
                LEA     GetPen  ,A0
                MOVE    #$9A,D0
                _SetTrapAddress


                .REF   PenSize
                LEA     PenSize ,A0
                MOVE    #$9B,D0
                _SetTrapAddress


                .REF   PenMode
                LEA     PenMode ,A0
                MOVE    #$9C,D0
                _SetTrapAddress


                .REF   PenPat
                LEA     PenPat  ,A0
                MOVE    #$9D,D0
                _SetTrapAddress


                .REF   PenNorma
                LEA     PenNorma,A0
                MOVE    #$9E,D0
                _SetTrapAddress


                .REF   StdRect
                LEA     StdRect ,A0
                MOVE    #$A0,D0
                _SetTrapAddress


                .REF   FrameRec
                LEA     FrameRec,A0
                MOVE    #$A1,D0
                _SetTrapAddress


                .REF   PaintRec
                LEA     PaintRec,A0
                MOVE    #$A2,D0
                _SetTrapAddress


                .REF   EraseRec
                LEA     EraseRec,A0
                MOVE    #$A3,D0
                _SetTrapAddress


                .REF   InvertRe
                LEA     InvertRe,A0
                MOVE    #$A4,D0
                _SetTrapAddress


                .REF   FillRect
                LEA     FillRect,A0
                MOVE    #$A5,D0
                _SetTrapAddress


                .REF   EqualRec
                LEA     EqualRec,A0
                MOVE    #$A6,D0
                _SetTrapAddress


                .REF   SetRect
                LEA     SetRect ,A0
                MOVE    #$A7,D0
                _SetTrapAddress


                .REF   OffSetRe
                LEA     OffSetRe,A0
                MOVE    #$A8,D0
                _SetTrapAddress


                .REF   InSetRec
                LEA     InSetRec,A0
                MOVE    #$A9,D0
                _SetTrapAddress


                .REF   SectRect
                LEA     SectRect,A0
                MOVE    #$AA,D0
                _SetTrapAddress


                .REF   UnionRec
                LEA     UnionRec,A0
                MOVE    #$AB,D0
                _SetTrapAddress


                .REF   Pt2Rect
                LEA     Pt2Rect ,A0
                MOVE    #$AC,D0
                _SetTrapAddress


                .REF   PtInRect
                LEA     PtInRect,A0
                MOVE    #$AD,D0
                _SetTrapAddress


                .REF   EmptyRec
                LEA     EmptyRec,A0
                MOVE    #$AE,D0
                _SetTrapAddress


                .REF   StdRRect
                LEA     StdRRect,A0
                MOVE    #$AF,D0
                _SetTrapAddress


                .REF   FrameRou
                LEA     FrameRou,A0
                MOVE    #$B0,D0
                _SetTrapAddress


                .REF   PaintRou
                LEA     PaintRou,A0
                MOVE    #$B1,D0
                _SetTrapAddress


                .REF   EraseRou
                LEA     EraseRou,A0
                MOVE    #$B2,D0
                _SetTrapAddress


                .REF   InvertRo
                LEA     InvertRo,A0
                MOVE    #$B3,D0
                _SetTrapAddress


                .REF   FillRoun
                LEA     FillRoun,A0
                MOVE    #$B4,D0
                _SetTrapAddress


                .REF   StdOval
                LEA     StdOval ,A0
                MOVE    #$B6,D0
                _SetTrapAddress


                .REF   FrameOva
                LEA     FrameOva,A0
                MOVE    #$B7,D0
                _SetTrapAddress


                .REF   PaintOva
                LEA     PaintOva,A0
                MOVE    #$B8,D0
                _SetTrapAddress


                .REF   EraseOva
                LEA     EraseOva,A0
                MOVE    #$B9,D0
                _SetTrapAddress


                .REF   InvertOv
                LEA     InvertOv,A0
                MOVE    #$BA,D0
                _SetTrapAddress


                .REF   FillOval
                LEA     FillOval,A0
                MOVE    #$BB,D0
                _SetTrapAddress


                .REF   StdArc
                LEA     StdArc  ,A0
                MOVE    #$BD,D0
                _SetTrapAddress


                .REF   FrameArc
                LEA     FrameArc,A0
                MOVE    #$BE,D0
                _SetTrapAddress


                .REF   PaintArc
                LEA     PaintArc,A0
                MOVE    #$BF,D0
                _SetTrapAddress


                .REF   EraseArc
                LEA     EraseArc,A0
                MOVE    #$C0,D0
                _SetTrapAddress


                .REF   InvertAr
                LEA     InvertAr,A0
                MOVE    #$C1,D0
                _SetTrapAddress


                .REF   FillArc
                LEA     FillArc ,A0
                MOVE    #$C2,D0
                _SetTrapAddress


                .REF   PtToAngl
                LEA     PtToAngl,A0
                MOVE    #$C3,D0
                _SetTrapAddress


                .REF   StdPoly
                LEA     StdPoly ,A0
                MOVE    #$C5,D0
                _SetTrapAddress


                .REF   FramePol
                LEA     FramePol,A0
                MOVE    #$C6,D0
                _SetTrapAddress


                .REF   PaintPol
                LEA     PaintPol,A0
                MOVE    #$C7,D0
                _SetTrapAddress


                .REF   ErasePol
                LEA     ErasePol,A0
                MOVE    #$C8,D0
                _SetTrapAddress


                .REF   InvertPo
                LEA     InvertPo,A0
                MOVE    #$C9,D0
                _SetTrapAddress


                .REF   FillPoly
                LEA     FillPoly,A0
                MOVE    #$CA,D0
                _SetTrapAddress


                .REF   OpenPoly
                LEA     OpenPoly,A0
                MOVE    #$CB,D0
                _SetTrapAddress


                .REF   ClosePol
                LEA     ClosePol,A0
                MOVE    #$CC,D0
                _SetTrapAddress


                .REF   KillPoly
                LEA     KillPoly,A0
                MOVE    #$CD,D0
                _SetTrapAddress


                .REF   OffSetPo
                LEA     OffSetPo,A0
                MOVE    #$CE,D0
                _SetTrapAddress


                .REF   PackBits
                LEA     PackBits,A0
                MOVE    #$CF,D0
                _SetTrapAddress


                .REF   UnpackBi
                LEA     UnpackBi,A0
                MOVE    #$D0,D0
                _SetTrapAddress


                .REF   StdRgn
                LEA     StdRgn  ,A0
                MOVE    #$D1,D0
                _SetTrapAddress


                .REF   FrameRgn
                LEA     FrameRgn,A0
                MOVE    #$D2,D0
                _SetTrapAddress


                .REF   PaintRgn
                LEA     PaintRgn,A0
                MOVE    #$D3,D0
                _SetTrapAddress


                .REF   EraseRgn
                LEA     EraseRgn,A0
                MOVE    #$D4,D0
                _SetTrapAddress


                .REF   InvertRg
                LEA     InvertRg,A0
                MOVE    #$D5,D0
                _SetTrapAddress


                .REF   FillRgn
                LEA     FillRgn ,A0
                MOVE    #$D6,D0
                _SetTrapAddress


                .REF   NewRgn
                LEA     NewRgn  ,A0
                MOVE    #$D8,D0
                _SetTrapAddress


                .REF   DisposeR
                LEA     DisposeR,A0
                MOVE    #$D9,D0
                _SetTrapAddress


                .REF   OpenRgn
                LEA     OpenRgn ,A0
                MOVE    #$DA,D0
                _SetTrapAddress


                .REF   CloseRgn
                LEA     CloseRgn,A0
                MOVE    #$DB,D0
                _SetTrapAddress


                .REF   CopyRgn
                LEA     CopyRgn ,A0
                MOVE    #$DC,D0
                _SetTrapAddress


                .REF   SetEmpty
                LEA     SetEmpty,A0
                MOVE    #$DD,D0
                _SetTrapAddress


                .REF   SetRectR
                LEA     SetRectR,A0
                MOVE    #$DE,D0
                _SetTrapAddress


                .REF   RectRgn
                LEA     RectRgn ,A0
                MOVE    #$DF,D0
                _SetTrapAddress


                .REF   OffSetRg
                LEA     OffSetRg,A0
                MOVE    #$E0,D0
                _SetTrapAddress


                .REF   InSetRgn
                LEA     InSetRgn,A0
                MOVE    #$E1,D0
                _SetTrapAddress


                .REF   EmptyRgn
                LEA     EmptyRgn,A0
                MOVE    #$E2,D0
                _SetTrapAddress


                .REF   EqualRgn
                LEA     EqualRgn,A0
                MOVE    #$E3,D0
                _SetTrapAddress


                .REF   SectRgn
                LEA     SectRgn ,A0
                MOVE    #$E4,D0
                _SetTrapAddress


                .REF   UnionRgn
                LEA     UnionRgn,A0
                MOVE    #$E5,D0
                _SetTrapAddress


                .REF   DiffRgn
                LEA     DiffRgn ,A0
                MOVE    #$E6,D0
                _SetTrapAddress


                .REF   XOrRgn
                LEA     XOrRgn  ,A0
                MOVE    #$E7,D0
                _SetTrapAddress


                .REF   PtInRgn
                LEA     PtInRgn ,A0
                MOVE    #$E8,D0
                _SetTrapAddress


                .REF   RectInRg
                LEA     RectInRg,A0
                MOVE    #$E9,D0
                _SetTrapAddress


                .REF   StdBits
                LEA     StdBits ,A0
                MOVE    #$EB,D0
                _SetTrapAddress


                .REF   CopyBits
                LEA     CopyBits,A0
                MOVE    #$EC,D0
                _SetTrapAddress


                .REF   ScrollRe
                LEA     ScrollRe,A0
                MOVE    #$EF,D0
                _SetTrapAddress


                .REF   SetStdPr
                LEA     SetStdPr,A0
                MOVE    #$EA,D0
                _SetTrapAddress


                .REF   StdTxMea
                LEA     StdTxMea,A0
                MOVE    #$ED,D0
                _SetTrapAddress


                .REF   StdGetPi
                LEA     StdGetPi,A0
                MOVE    #$EE,D0
                _SetTrapAddress


                .REF   StdPutPi
                LEA     StdPutPi,A0
                MOVE    #$F0,D0
                _SetTrapAddress


                .REF   StdComme
                LEA     StdComme,A0
                MOVE    #$F1,D0
                _SetTrapAddress


                .REF   PicComme
                LEA     PicComme,A0
                MOVE    #$F2,D0
                _SetTrapAddress


                .REF   OpenPict
                LEA     OpenPict,A0
                MOVE    #$F3,D0
                _SetTrapAddress


                .REF   ClosePic
                LEA     ClosePic,A0
                MOVE    #$F4,D0
                _SetTrapAddress


                .REF   KillPict
                LEA     KillPict,A0
                MOVE    #$F5,D0
                _SetTrapAddress

                .REF   DrawPict
                LEA     DrawPict,A0
                MOVE    #$F6,D0
                _SetTrapAddress

                .REF   ScalePt
                LEA     ScalePt,A0
                MOVE    #$F8,D0
                _SetTrapAddress


                .REF   MapPt
                LEA     MapPt,A0
                MOVE    #$F9,D0
                _SetTrapAddress


                .REF   MapRect
                LEA     MapRect,A0
                MOVE    #$FA,D0
                _SetTrapAddress


                .REF   MapRgn
                LEA     MapRgn,A0
                MOVE    #$FB,D0
                _SetTrapAddress


                .REF   MapPoly
                LEA     MapPoly,A0
                MOVE    #$FC,D0
                _SetTrapAddress


; FONT MANAGER

                .REF   InitFont
                LEA     InitFont,A0
                MOVE    #$FE,D0
                _SetTrapAddress


                .REF   GetFontName
                LEA     GetFontName,A0
                MOVE    #$FF,D0
                _SetTrapAddress


                .REF   GetFNum
                LEA     GetFNum,A0
                MOVE    #$100,D0
                _SetTrapAddress


                .REF   FMSwapFont
                LEA     FMSwapFont,A0
                MOVE    #$101,D0
                _SetTrapAddress


                .REF   RealFont
                LEA     RealFont,A0
                MOVE    #$102,D0
                _SetTrapAddress


                .REF   SetFontLo
                LEA     SetFontLo,A0
                MOVE    #$103,D0
                _SetTrapAddress

                LEA     FMSwapFont,A0
                MOVE.L  A0,JSwapFont

            RTS



            .END
