UNIT QuickDraw;

{  Copyright 1983 Apple Computer Inc.  }
{  Written by Bill Atkinson            }

INTERFACE

CONST srcCopy      = 0;  { the 16 transfer modes }
      srcOr        = 1;
      srcXor       = 2;
      srcBic       = 3;
      notSrcCopy   = 4;
      notSrcOr     = 5;
      notSrcXor    = 6;
      notSrcBic    = 7;
      patCopy      = 8;
      patOr        = 9;
      patXor       = 10;
      patBic       = 11;
      notPatCopy   = 12;
      notPatOr     = 13;
      notPatXor    = 14;
      notPatBic    = 15;

{ QuickDraw color separation constants }

      normalBit    = 0;       { normal screen mapping   }
      inverseBit   = 1;       { inverse screen mapping  }
      redBit       = 4;       { RGB additive mapping }
      greenBit     = 3;
      blueBit      = 2;
      cyanBit      = 8;       { CMYBk subtractive mapping }
      magentaBit   = 7;
      yellowBit    = 6;
      blackBit     = 5;

      blackColor   = 33;      { colors expressed in these mappings }
      whiteColor   = 30;
      redColor     = 205;
      greenColor   = 341;
      blueColor    = 409;
      cyanColor    = 273;
      magentaColor = 137;
      yellowColor  = 69;

      picLParen    = 0;       { standard picture comments }
      picRParen    = 1;


TYPE QDByte    =  -128..127;
     QDPtr     =  ^QDByte;              { blind pointer }
     QDHandle  =  ^QDPtr;               { blind handle  }
     Str255    =  String[255];
     Pattern   =  PACKED ARRAY[0..7] OF 0..255;
     Bits16    =  ARRAY[0..15] OF INTEGER;
     VHSelect  =  (v,h);
     GrafVerb  =  (frame,paint,erase,invert,fill);
     StyleItem =  (bold,italic,underline,outline,shadow,condense,extend);
     Style     =  SET OF StyleItem;

     FontInfo  =  RECORD
                    ascent:  INTEGER;
                    descent: INTEGER;
                    widMax:  INTEGER;
                    leading: INTEGER;
                  END;

     Point = RECORD CASE INTEGER OF

               0: (v: INTEGER;
                   h: INTEGER);

               1: (vh: ARRAY[VHSelect] OF INTEGER);

             END;


     Rect = RECORD CASE INTEGER OF

              0: (top:      INTEGER;
                  left:     INTEGER;
                  bottom:   INTEGER;
                  right:    INTEGER);

              1: (topLeft:  Point;
                  botRight: Point);
            END;


     BitMap = RECORD
                baseAddr: QDPtr;
                rowBytes: INTEGER;
                bounds:   Rect;
              END;


     Cursor = RECORD
                data:    Bits16;
                mask:    Bits16;
                hotSpot: Point;
              END;


     PenState = RECORD
                  pnLoc:   Point;
                  pnSize:  Point;
                  pnMode:  INTEGER;
                  pnPat:   Pattern;
                END;


     PolyHandle = ^PolyPtr;
     PolyPtr    = ^Polygon;
     Polygon    = RECORD
                    polySize:   INTEGER;
                    polyBBox:   Rect;
                    polyPoints: ARRAY[0..0] OF Point;
                  END;


     RgnHandle = ^RgnPtr;
     RgnPtr    = ^Region;
     Region    =  RECORD
                    rgnSize:   INTEGER;  { rgnSize = 10 for rectangular }
                    rgnBBox:   Rect;
                    { plus more data if not rectangular }
                  END;


     PicHandle = ^PicPtr;
     PicPtr    = ^Picture;
     Picture   =  RECORD
                    picSize:    INTEGER;
                    picFrame:   Rect;
                    { plus byte codes for picture content }
                  END;


     QDProcsPtr = ^QDProcs;
     QDProcs = RECORD
                 textProc:    QDPtr;
                 lineProc:    QDPtr;
                 rectProc:    QDPtr;
                 rRectProc:   QDPtr;
                 ovalProc:    QDPtr;
                 arcProc:     QDPtr;
                 polyProc:    QDPtr;
                 rgnProc:     QDPtr;
                 bitsProc:    QDPtr;
                 commentProc: QDPtr;
                 txMeasProc:  QDPtr;
                 getPicProc:  QDPtr;
                 putPicProc:  QDPtr;
               END;


     GrafPtr  = ^GrafPort;
     GrafPort = RECORD
                  device:      INTEGER;
                  portBits:    BitMap;
                  portRect:    Rect;
                  visRgn:      RgnHandle;
                  clipRgn:     RgnHandle;
                  bkPat:       Pattern;
                  fillPat:     Pattern;
                  pnLoc:       Point;
                  pnSize:      Point;
                  pnMode:      INTEGER;
                  pnPat:       Pattern;
                  pnVis:       INTEGER;
                  txFont:      INTEGER;
                  txFace:      Style;
                  txMode:      INTEGER;
                  txSize:      INTEGER;
                  spExtra:     LongInt;
                  fgColor:     LongInt;
                  bkColor:     LongInt;
                  colrBit:     INTEGER;
                  patStretch:  INTEGER;
                  picSave:     QDHandle;
                  rgnSave:     QDHandle;
                  polySave:    QDHandle;
                  grafProcs:   QDProcsPtr;
                END;



VAR thePort:    GrafPtr;
    white:      Pattern;
    black:      Pattern;
    gray:       Pattern;
    ltGray:     Pattern;
    dkGray:     Pattern;
    arrow:      Cursor;
    screenBits: BitMap;
    randSeed:   LongInt;


{ GrafPort Routines }

PROCEDURE InitGraf   (globalPtr: QDPtr);
PROCEDURE OpenPort   (port: GrafPtr);
PROCEDURE InitPort   (port: GrafPtr);
PROCEDURE ClosePort  (port: GrafPtr);
PROCEDURE SetPort    (port: GrafPtr);
PROCEDURE GetPort    (VAR port: GrafPtr);
PROCEDURE GrafDevice (device: INTEGER);
PROCEDURE SetPortBits(bm: BitMap);
PROCEDURE PortSize   (width,height: INTEGER);
PROCEDURE MovePortTo (leftGlobal,topGlobal: INTEGER);
PROCEDURE SetOrigin  (h,v: INTEGER);
PROCEDURE SetClip    (rgn: RgnHandle);
PROCEDURE GetClip    (rgn: RgnHandle);
PROCEDURE ClipRect   (r: Rect);
PROCEDURE BackPat    (pat: Pattern);


{ Cursor Routines }

PROCEDURE InitCursor;
PROCEDURE SetCursor(crsr: Cursor);
PROCEDURE HideCursor;
PROCEDURE ShowCursor;
PROCEDURE ObscureCursor;


{ Line Routines }

PROCEDURE HidePen;
PROCEDURE ShowPen;
PROCEDURE GetPen     (VAR pt: Point);
PROCEDURE GetPenState(VAR pnState: PenState);
PROCEDURE SetPenState(pnState: PenState);
PROCEDURE PenSize    (width,height: INTEGER);
PROCEDURE PenMode    (mode: INTEGER);
PROCEDURE PenPat     (pat: Pattern);
PROCEDURE PenNormal;
PROCEDURE MoveTo     (h,v: INTEGER);
PROCEDURE Move       (dh,dv: INTEGER);
PROCEDURE LineTo     (h,v: INTEGER);
PROCEDURE Line       (dh,dv: INTEGER);


{ Text Routines }

PROCEDURE TextFont     (font: INTEGER);
PROCEDURE TextFace     (face: Style);
PROCEDURE TextMode     (mode: INTEGER);
PROCEDURE TextSize     (size: INTEGER);
PROCEDURE SpaceExtra   (extra: LongInt);
PROCEDURE DrawChar     (ch: char);
PROCEDURE DrawString   (s: Str255);
PROCEDURE DrawText     (textBuf: QDPtr; firstByte,byteCount: INTEGER);
FUNCTION  CharWidth    (ch: CHAR): INTEGER;
FUNCTION  StringWidth  (s: Str255): INTEGER;
FUNCTION  TextWidth    (textBuf: QDPtr; firstByte,byteCount: INTEGER): INTEGER;
PROCEDURE GetFontInfo  (VAR info: FontInfo);


{ Point Calculations }

PROCEDURE AddPt         (src: Point; VAR dst: Point);
PROCEDURE SubPt         (src: Point; VAR dst: Point);
PROCEDURE SetPt         (VAR pt: Point; h,v: INTEGER);
FUNCTION  EqualPt       (pt1,pt2: Point): BOOLEAN;
PROCEDURE ScalePt       (VAR pt: Point;    fromRect,toRect: Rect);
PROCEDURE MapPt         (VAR pt: Point;    fromRect,toRect: Rect);
PROCEDURE LocalToGlobal (VAR pt: Point);
PROCEDURE GlobalToLocal (VAR pt: Point);


{ Rectangle Calculations }

PROCEDURE SetRect    (VAR r: Rect; left,top,right,bottom: INTEGER);
FUNCTION  EqualRect  (rect1,rect2: Rect): BOOLEAN;
FUNCTION  EmptyRect  (r: Rect): BOOLEAN;
PROCEDURE OffsetRect (VAR r: Rect; dh,dv: INTEGER);
PROCEDURE MapRect    (VAR r:  Rect;     fromRect,toRect: Rect);
PROCEDURE InsetRect  (VAR r: Rect; dh,dv: INTEGER);
FUNCTION  SectRect   (src1,src2: Rect; VAR dstRect: Rect): BOOLEAN;
PROCEDURE UnionRect  (src1,src2: Rect; VAR dstRect: Rect);
FUNCTION  PtInRect   (pt: Point; r: Rect): BOOLEAN;
PROCEDURE Pt2Rect    (pt1,pt2: Point; VAR dstRect: Rect);


{ Graphical Operations on Rectangles }

PROCEDURE FrameRect  (r: Rect);
PROCEDURE PaintRect  (r: Rect);
PROCEDURE EraseRect  (r: Rect);
PROCEDURE InvertRect (r: Rect);
PROCEDURE FillRect   (r: Rect; pat: Pattern);


{ RoundRect Routines }

PROCEDURE FrameRoundRect  (r: Rect; ovWd,ovHt: INTEGER);
PROCEDURE PaintRoundRect  (r: Rect; ovWd,ovHt: INTEGER);
PROCEDURE EraseRoundRect  (r: Rect; ovWd,ovHt: INTEGER);
PROCEDURE InvertRoundRect (r: Rect; ovWd,ovHt: INTEGER);
PROCEDURE FillRoundRect   (r: Rect; ovWd,ovHt: INTEGER; pat: Pattern);


{ Oval Routines }

PROCEDURE FrameOval  (r: Rect);
PROCEDURE PaintOval  (r: Rect);
PROCEDURE EraseOval  (r: Rect);
PROCEDURE InvertOval (r: Rect);
PROCEDURE FillOval   (r: Rect; pat: Pattern);


{ Arc Routines }

PROCEDURE FrameArc  (r: Rect; startAngle,arcAngle: INTEGER);
PROCEDURE PaintArc  (r: Rect; startAngle,arcAngle: INTEGER);
PROCEDURE EraseArc  (r: Rect; startAngle,arcAngle: INTEGER);
PROCEDURE InvertArc (r: Rect; startAngle,arcAngle: INTEGER);
PROCEDURE FillArc   (r: Rect; startAngle,arcAngle: INTEGER; pat: Pattern);
PROCEDURE PtToAngle (r: Rect; pt: Point; VAR angle: INTEGER);


{ Polygon Routines }

FUNCTION  OpenPoly:    PolyHandle;
PROCEDURE ClosePoly;
PROCEDURE KillPoly    (poly: PolyHandle);
PROCEDURE OffsetPoly  (poly: PolyHandle; dh,dv: INTEGER);
PROCEDURE MapPoly     (poly: PolyHandle; fromRect,toRect: Rect);
PROCEDURE FramePoly   (poly: PolyHandle);
PROCEDURE PaintPoly   (poly: PolyHandle);
PROCEDURE ErasePoly   (poly: PolyHandle);
PROCEDURE InvertPoly  (poly: PolyHandle);
PROCEDURE FillPoly    (poly: PolyHandle; pat: Pattern);


{ Region Calculations }

FUNCTION  NewRgn:   RgnHandle;
PROCEDURE DisposeRgn(rgn: RgnHandle);
PROCEDURE CopyRgn   (srcRgn,dstRgn: RgnHandle);
PROCEDURE SetEmptyRgn(rgn: RgnHandle);
PROCEDURE SetRectRgn(rgn: RgnHandle; left,top,right,bottom: INTEGER);
PROCEDURE RectRgn   (rgn: RgnHandle; r: Rect);
PROCEDURE OpenRgn;
PROCEDURE CloseRgn  (dstRgn: RgnHandle);
PROCEDURE OffsetRgn (rgn: RgnHandle; dh,dv: INTEGER);
PROCEDURE MapRgn    (rgn: RgnHandle;   fromRect,toRect: Rect);
PROCEDURE InsetRgn  (rgn: RgnHandle; dh,dv: INTEGER);
PROCEDURE SectRgn   (srcRgnA,srcRgnB,dstRgn: RgnHandle);
PROCEDURE UnionRgn  (srcRgnA,srcRgnB,dstRgn: RgnHandle);
PROCEDURE DiffRgn   (srcRgnA,srcRgnB,dstRgn: RgnHandle);
PROCEDURE XorRgn    (srcRgnA,srcRgnB,dstRgn: RgnHandle);
FUNCTION  EqualRgn  (rgnA,rgnB: RgnHandle): BOOLEAN;
FUNCTION  EmptyRgn  (rgn: RgnHandle): BOOLEAN;
FUNCTION  PtInRgn   (pt: Point; rgn: RgnHandle): BOOLEAN;
FUNCTION  RectInRgn (r: Rect; rgn: RgnHandle): BOOLEAN;


{ Graphical Operations on Regions }

PROCEDURE FrameRgn  (rgn: RgnHandle);
PROCEDURE PaintRgn  (rgn: RgnHandle);
PROCEDURE EraseRgn  (rgn: RgnHandle);
PROCEDURE InvertRgn (rgn: RgnHandle);
PROCEDURE FillRgn   (rgn: RgnHandle; pat: Pattern);


{ Graphical Operations on BitMaps }

PROCEDURE ScrollRect(dstRect: Rect; dh,dv: INTEGER; updateRgn: rgnHandle);
PROCEDURE CopyBits  (srcBits,dstBits: BitMap;
                     srcRect,dstRect: Rect;
                     mode:            INTEGER;
                     maskRgn:         RgnHandle);

{ Picture Routines }

FUNCTION  OpenPicture(picFrame: Rect): PicHandle;
PROCEDURE ClosePicture;
PROCEDURE DrawPicture(myPicture: PicHandle; dstRect: Rect);
PROCEDURE PicComment(kind,dataSize: INTEGER; dataHandle: QDHandle);
PROCEDURE KillPicture(myPicture: PicHandle);


{  The Bottleneck Interface:   }

PROCEDURE SetStdProcs(VAR procs: QDProcs);
PROCEDURE StdText    (count: INTEGER; textAddr: QDPtr; numer,denom: Point);
PROCEDURE StdLine    (newPt: Point);
PROCEDURE StdRect    (verb: GrafVerb; r: Rect);
PROCEDURE StdRRect   (verb: GrafVerb; r: Rect; ovWd,ovHt: INTEGER);
PROCEDURE StdOval    (verb: GrafVerb; r: Rect);
PROCEDURE StdArc     (verb: GrafVerb; r: Rect; startAngle,arcAngle: INTEGER);
PROCEDURE StdPoly    (verb: GrafVerb; poly: PolyHandle);
PROCEDURE StdRgn     (verb: GrafVerb; rgn: RgnHandle);
PROCEDURE StdBits    (VAR srcBits: BitMap; VAR srcRect,dstRect: Rect;
                      mode: INTEGER; maskRgn: RgnHandle);
PROCEDURE StdComment (kind,dataSize: INTEGER; dataHandle: QDHandle);
FUNCTION  StdTxMeas  (count: INTEGER; textAddr: QDPtr;
                      VAR numer,denom: Point; VAR info: FontInfo): INTEGER;
PROCEDURE StdGetPic  (dataPtr: QDPtr; byteCount: INTEGER);
PROCEDURE StdPutPic  (dataPtr: QDPtr; byteCount: INTEGER);


{ Misc Utility Routines }

FUNCTION  GetPixel  (h,v: INTEGER): BOOLEAN;
FUNCTION  Random:   INTEGER;
PROCEDURE StuffHex  (thingptr: QDPtr; s:Str255);
PROCEDURE ForeColor (color: LongInt);
PROCEDURE BackColor (color: LongInt);
PROCEDURE ColorBit  (whichBit: INTEGER);


IMPLEMENTATION

{$I QuickDraw2.text }
