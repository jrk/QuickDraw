{ QuickDraw2.text: Implementation part of QuickDraw }

{$S Graf }

TYPE FMOutPtr = ^FMOutRec;
     FMOutrec = PACKED RECORD
                  errNum:     INTEGER;     { used only for GrafError    }
                  fontHandle: QDHandle;    { handle to font             }
                  bold:       0..255;      { how much to smear horiz    }
                  italic:     0..255;      { how much to shear          }
                  ulOffset:   0..255;      { pixels below baseline      }
                  ulShadow:   0..255;      { how big is the halo        }
                  ulThick:    0..255;      { how thick is the underline }
                  shadow:     0..255;      { 0,1,2,or 3 only            }
                  extra:   -128..127;      { extra white dots each char }
                  ascent:     0..255;      { ascent measure for font    }
                  descent:    0..255;      { descent measure for font   }
                  widMax:     0..255;      { width of widest char       }
                  leading: -128..127;      { leading between lines      }
                  unused:     0..255;
                  numer:      Point;       { use this modified scale to }
                  denom:      Point;       { draw or measure text with  }
                END;



VAR wideOpen:   RgnHandle;     { a dummy rectangular region, read-only }
    wideMaster: RgnPtr;
    wideData:   Region;
    rgnBuf:     QDHandle;      { point saving buffer for OpenRgn       }
    rgnIndex:   INTEGER;       { current bytes used in rgnBuf          }
    rgnMax:     INTEGER;       { max bytes allocated so far to rgnBuf  }
    playPic:    PicHandle;     { used by StdGetPic                     }
    QDSpare0:   INTEGER;       { unused word }
    thePoly:    PolyHandle;    { the current polygon being defined     }
    polyMax:    INTEGER;       { max bytes allocated so far to thePoly }
    patAlign:   Point;         { to align pattern during DrawPicture   }
    fixTxWid:   Fixed;         { Fixed Point width from StdTxMeas.     }
    fontPtr:    FMOutPtr;      { the last font used, used by DrawText  }
    playIndex:  LongInt;       { used by StdGetPic during DrawPicture  }
    QDSpare3:   INTEGER;       { unused word }
    QDSpare4:   INTEGER;       { unused word }
    QDSpare5:   INTEGER;       { unused word }
    QDSpare6:   INTEGER;       { unused word }
    QDSpare7:   INTEGER;       { unused word }
    QDSpare8:   INTEGER;       { unused word }
    QDSpare9:   INTEGER;       { unused word }
    QDSpareA:   INTEGER;       { unused word }
    QDSpareB:   INTEGER;       { unused word }
    QDSpareC:   INTEGER;       { unused word }
    QDSpareD:   INTEGER;       { unused word }




{ grafPort routines }

PROCEDURE InitGraf;             EXTERNAL;
PROCEDURE OpenPort;             EXTERNAL;
PROCEDURE InitPort;             EXTERNAL;
PROCEDURE ClosePort;            EXTERNAL;
PROCEDURE GrafDevice;           EXTERNAL;
PROCEDURE SetPort;              EXTERNAL;
PROCEDURE GetPort;              EXTERNAL;
PROCEDURE SetPortBits;          EXTERNAL;
PROCEDURE PortSize;             EXTERNAL;
PROCEDURE MovePortTo;           EXTERNAL;
PROCEDURE SetOrigin;            EXTERNAL;
PROCEDURE SetClip;              EXTERNAL;
PROCEDURE GetClip;              EXTERNAL;
PROCEDURE ClipRect;             EXTERNAL;
PROCEDURE BackPat;              EXTERNAL;


{ cursor routines }

PROCEDURE InitCursor;           EXTERNAL;
PROCEDURE SetCursor;            EXTERNAL;
PROCEDURE HideCursor;           EXTERNAL;
PROCEDURE ShowCursor;           EXTERNAL;
PROCEDURE ObscureCursor;        EXTERNAL;


{ text routines }

PROCEDURE TextFont;             EXTERNAL;
PROCEDURE TextFace;             EXTERNAL;
PROCEDURE TextMode;             EXTERNAL;
PROCEDURE TextSize;             EXTERNAL;
PROCEDURE SpaceExtra;           EXTERNAL;
PROCEDURE DrawChar;             EXTERNAL;
PROCEDURE DrawString;           EXTERNAL;
PROCEDURE DrawText;             EXTERNAL;
FUNCTION  CharWidth;            EXTERNAL;
FUNCTION  StringWidth;          EXTERNAL;
FUNCTION  TextWidth;            EXTERNAL;
PROCEDURE GetFontInfo;          EXTERNAL;


{ line routines }

PROCEDURE HidePen;              EXTERNAL;
PROCEDURE ShowPen;              EXTERNAL;
PROCEDURE GetPen;               EXTERNAL;
PROCEDURE GetPenState;          EXTERNAL;
PROCEDURE SetPenState;          EXTERNAL;
PROCEDURE PenSize;              EXTERNAL;
PROCEDURE PenMode;              EXTERNAL;
PROCEDURE PenPat;               EXTERNAL;
PROCEDURE PenNormal;            EXTERNAL;
PROCEDURE MoveTo;               EXTERNAL;
PROCEDURE Move;                 EXTERNAL;
PROCEDURE LineTo;               EXTERNAL;
PROCEDURE Line;                 EXTERNAL;


{ rectangle calculations }

PROCEDURE SetRect;              EXTERNAL;
FUNCTION  EqualRect;            EXTERNAL;
FUNCTION  EmptyRect;            EXTERNAL;
PROCEDURE OffsetRect;           EXTERNAL;
PROCEDURE MapRect;              EXTERNAL;
PROCEDURE InsetRect;            EXTERNAL;
FUNCTION  SectRect;             EXTERNAL;
PROCEDURE UnionRect;            EXTERNAL;
FUNCTION  PtInRect;             EXTERNAL;
PROCEDURE Pt2Rect;              EXTERNAL;


{ graphical operations on rectangles }

PROCEDURE FrameRect;            EXTERNAL;
PROCEDURE PaintRect;            EXTERNAL;
PROCEDURE EraseRect;            EXTERNAL;
PROCEDURE InvertRect;           EXTERNAL;
PROCEDURE FillRect;             EXTERNAL;


{ graphical operations on RoundRects }

PROCEDURE FrameRoundRect;       EXTERNAL;
PROCEDURE PaintRoundRect;       EXTERNAL;
PROCEDURE EraseRoundRect;       EXTERNAL;
PROCEDURE InvertRoundRect;      EXTERNAL;
PROCEDURE FillRoundRect;        EXTERNAL;


{ graphical operations on Ovals }

PROCEDURE FrameOval;            EXTERNAL;
PROCEDURE PaintOval;            EXTERNAL;
PROCEDURE EraseOval;            EXTERNAL;
PROCEDURE InvertOval;           EXTERNAL;
PROCEDURE FillOval;             EXTERNAL;


{ Arc routines }

PROCEDURE FrameArc;             EXTERNAL;
PROCEDURE PaintArc;             EXTERNAL;
PROCEDURE EraseArc;             EXTERNAL;
PROCEDURE InvertArc;            EXTERNAL;
PROCEDURE FillArc;              EXTERNAL;
PROCEDURE PtToAngle;            EXTERNAL;


{ polygon routines }

FUNCTION  OpenPoly;             EXTERNAL;
PROCEDURE ClosePoly;            EXTERNAL;
PROCEDURE KillPoly;             EXTERNAL;
PROCEDURE OffsetPoly;           EXTERNAL;
PROCEDURE MapPoly;              EXTERNAL;

PROCEDURE FramePoly;            EXTERNAL;
PROCEDURE PaintPoly;            EXTERNAL;
PROCEDURE ErasePoly;            EXTERNAL;
PROCEDURE InvertPoly;           EXTERNAL;
PROCEDURE FillPoly;             EXTERNAL;


{ region calculations }

FUNCTION  NewRgn;               EXTERNAL;
PROCEDURE DisposeRgn;           EXTERNAL;
PROCEDURE OpenRgn;              EXTERNAL;
PROCEDURE CloseRgn;             EXTERNAL;
PROCEDURE OffsetRgn;            EXTERNAL;
PROCEDURE MapRgn;               EXTERNAL;
PROCEDURE InsetRgn;             EXTERNAL;
PROCEDURE SectRgn;              EXTERNAL;
PROCEDURE CopyRgn;              EXTERNAL;
PROCEDURE SetEmptyRgn;          EXTERNAL;
PROCEDURE SetRectRgn;           EXTERNAL;
PROCEDURE RectRgn;              EXTERNAL;
PROCEDURE UnionRgn;             EXTERNAL;
PROCEDURE DiffRgn;              EXTERNAL;
PROCEDURE XorRgn;               EXTERNAL;
FUNCTION  EqualRgn;             EXTERNAL;
FUNCTION  EmptyRgn;             EXTERNAL;
FUNCTION  PtInRgn;              EXTERNAL;
FUNCTION  RectInRgn;            EXTERNAL;


{ graphical operations on Regions }

PROCEDURE FrameRgn;             EXTERNAL;
PROCEDURE PaintRgn;             EXTERNAL;
PROCEDURE EraseRgn;             EXTERNAL;
PROCEDURE InvertRgn;            EXTERNAL;
PROCEDURE FillRgn;              EXTERNAL;


{ BitMap routines }

PROCEDURE CopyBits;             EXTERNAL;
PROCEDURE ScrollRect;           EXTERNAL;


{ Picture routines }

FUNCTION  OpenPicture;          EXTERNAL;
PROCEDURE ClosePicture;         EXTERNAL;
PROCEDURE KillPicture;          EXTERNAL;
PROCEDURE DrawPicture;          EXTERNAL;
PROCEDURE PicComment;           EXTERNAL;


{ BottleNeck routines }

PROCEDURE StdText;              EXTERNAL;
PROCEDURE StdLine;              EXTERNAL;
PROCEDURE StdRect;              EXTERNAL;
PROCEDURE StdRRect;             EXTERNAL;
PROCEDURE StdOval;              EXTERNAL;
PROCEDURE StdArc;               EXTERNAL;
PROCEDURE StdPoly;              EXTERNAL;
PROCEDURE StdRgn;               EXTERNAL;
PROCEDURE StdBits;              EXTERNAL;
PROCEDURE StdComment;           EXTERNAL;
FUNCTION  StdTxMeas;            EXTERNAL;
PROCEDURE StdGetPic;            EXTERNAL;
PROCEDURE StdPutPic;            EXTERNAL;


{ misc utility routines }

FUNCTION  GetPixel;             EXTERNAL;
FUNCTION  Random;               EXTERNAL;
PROCEDURE AddPt;                EXTERNAL;
PROCEDURE SubPt;                EXTERNAL;
PROCEDURE SetPt;                EXTERNAL;
FUNCTION  EqualPt;              EXTERNAL;
PROCEDURE StuffHex;             EXTERNAL;
PROCEDURE LocalToGlobal;        EXTERNAL;
PROCEDURE GlobalToLocal;        EXTERNAL;
PROCEDURE ScalePt;              EXTERNAL;
PROCEDURE MapPt;                EXTERNAL;
PROCEDURE ForeColor;            EXTERNAL;
PROCEDURE BackColor;            EXTERNAL;
PROCEDURE ColorBit;             EXTERNAL;
PROCEDURE SetStdProcs;          EXTERNAL;



END. { of UNIT }
