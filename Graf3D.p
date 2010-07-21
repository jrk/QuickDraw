{$S Graf    }

UNIT Graf3D;

{ three-dimensional graphics routines layered on top of QuickDraw }

INTERFACE

USES {$U obj:QuickDraw }  QuickDraw;

CONST radConst=57.29578;

TYPE Point3D=RECORD
               x: REAL;
               y: REAL;
               z: REAL;
             END;

     Point2D=RECORD
               x: REAL;
               y: REAL;
             END;

     XfMatrix  = ARRAY[0..3,0..3] OF REAL;
     Port3DPtr = ^Port3D;
     Port3D    = RECORD
                   GPort:                     GrafPtr;
                   viewRect:                  Rect;
                   xLeft,yTop,xRight,yBottom: REAL;
                   pen,penPrime,eye:          Point3D;
                   hSize,vSize:               REAL;
                   hCenter,vCenter:           REAL;
                   xCotan,yCotan:             REAL;
                   ident:                     BOOLEAN;
                   xForm:                     XfMatrix;
                 END;


VAR thePort3D: Port3DPtr;


  PROCEDURE Open3DPort  (port: Port3DPtr);
  PROCEDURE SetPort3D   (port: Port3DPtr);
  PROCEDURE GetPort3D   (VAR port: Port3DPtr);

  PROCEDURE MoveTo2D(x,y: REAL);  PROCEDURE MoveTo3D(x,y,z: REAL);
  PROCEDURE LineTo2D(x,y: REAL);  PROCEDURE LineTo3D(x,y,z: REAL);
  PROCEDURE Move2D(dx,dy: REAL);  PROCEDURE Move3D(dx,dy,dz: REAL);
  PROCEDURE Line2D(dx,dy: REAL);  PROCEDURE Line3D(dx,dy,dz: REAL);

  PROCEDURE ViewPort    (r: Rect);
  PROCEDURE LookAt      (left,top,right,bottom: REAL);
  PROCEDURE ViewAngle   (angle: REAL);
  PROCEDURE Identity;
  PROCEDURE Scale       (xFactor,yFactor,zFactor: REAL);
  PROCEDURE Translate   (dx,dy,dz: REAL);
  PROCEDURE Pitch       (xAngle: REAL);
  PROCEDURE Yaw         (yAngle: REAL);
  PROCEDURE Roll        (zAngle: REAL);
  PROCEDURE Skew        (zAngle: REAL);
  PROCEDURE TransForm   (src: Point3D; VAR dst: Point3D);
  FUNCTION  Clip3D      (src1,src2: Point3D; VAR dst1,dst2: POINT): BOOLEAN;

  PROCEDURE SetPt3D     (VAR pt3D: Point3D; x,y,z: REAL);
  PROCEDURE SetPt2D     (VAR pt2D: Point2D; x,y: REAL);



IMPLEMENTATION



PROCEDURE Open3DPort(* port: Port3DPtr *);
{ initialize all values in port^ to their defaults }
BEGIN
  thePort3D:=port;
  port^.GPort:=thePort;
  ViewPort(thePort^.portRect);
  WITH thePort^.portRect DO LookAt(left,top,right,bottom);
  ViewAngle(0);
  Identity;
  MoveTo3D(0,0,0);
END;


PROCEDURE SetPort3D(* port: Port3DPtr *);
{ change to another Port3D }
BEGIN
  thePort3D:=port;
  SetPort(port^.GPort);
END;


PROCEDURE GetPort3D(* VAR port: Port3DPtr *);
{ inquire the current Port3D }
BEGIN
  port:=thePort3D;
END;


PROCEDURE MoveTo3D(* x,y,z: REAL *);
{  Move from current position to x,y,z without drawing.  }
VAR pt1,pt2: POINT;
    oldPrime: Point3D;
BEGIN
  WITH thePort3D^ DO
    BEGIN
      oldPrime:=penPrime;
      pen.x:=x;
      pen.y:=y;
      pen.z:=z;
      TransForm(pen,penPrime);
      IF Clip3D(oldPrime,penPrime,pt1,pt2) THEN MoveTo(pt2.H,pt2.V);
    END;
END;


PROCEDURE LineTo3D(* x,y,z: REAL *);
{ draw a 3-D line from current position to x,y,z. }
VAR oldPrime: Point3D;
    pt1,pt2: POINT;
BEGIN
  WITH thePort3D^ DO
    BEGIN
      oldPrime:=penPrime;
      pen.x:=x;
      pen.y:=y;
      pen.z:=z;
      TransForm(pen,penPrime);
      IF Clip3D(oldPrime,penPrime,pt1,pt2) THEN
        BEGIN
          MoveTo(pt1.h,pt1.v);
          LineTo(pt2.H,pt2.V);
        END;
    END;
END;


PROCEDURE Move3D(* dx,dy,dz: REAL *);
BEGIN
  WITH thePort3D^ DO MoveTo3D(pen.x+dx,pen.y+dy,pen.z+dz);
END;


PROCEDURE Line3D(* dx,dy,dz: REAL *);
BEGIN
  WITH thePort3D^ DO LineTo3D(pen.x+dx,pen.y+dy,pen.z+dz);
END;


PROCEDURE MoveTo2D(* x,y: REAL *);
BEGIN
  MoveTo3D(x,y,thePort3D^.pen.z);
END;


PROCEDURE Move2D(* dx,dy: REAL *);
BEGIN
  Move3D(dx,dy,0.0);
END;


PROCEDURE LineTo2D(* x,y: REAL *);
BEGIN
  LineTo3D(x,y,thePort3D^.pen.z);
END;


PROCEDURE Line2D(* dx,dy: REAL *);
BEGIN
  Line3D(dx,dy,0.0);
END;


PROCEDURE ViewLook;
{ re-calculate offsets and scales after LookAt or ViewPort }
BEGIN
  WITH thePort3D^ DO
    WITH viewRect DO
      BEGIN
        hSize:=(right-left)/2.0;
        vSize:=(bottom-top)/(-2.0);  { vert pos down, y pos up }
        hCenter:=left + hSize;
        vCenter:=top  - vSize;
      END;
END;


PROCEDURE ViewPort(* r: Rect *);
{ specify what portion of the folder to map onto }
BEGIN
  thePort3D^.viewRect:=r;
  ViewLook;  { re-calculate scales and offsets }
END;


PROCEDURE LookAt(* left,top,right,bottom: REAL *);
{ specify the real number coordinates of the portRect }
BEGIN
  WITH thePort3D^ DO
    BEGIN
      xLeft:=left;
      xRight:=right;
      yBottom:=bottom;
      yTop:=top;
      eye.x:=(left+right)/2.0;
      eye.y:=(top+bottom)/2.0;
    END;
  ViewLook;  { re-calculate scales and offsets }
END;


PROCEDURE ViewAngle(* angle: REAL *);
{ specify the horizontal angle subtended by the viewing pyramid }
BEGIN
  WITH thePort3D^ DO
    BEGIN
      IF angle < 0.1 THEN angle:=0.1;
      angle:=angle/(2.0*radConst);      { halve angle & convert to rad }
      xCotan:=COS(angle)/SIN(angle);    { remember for perspective calc }
      yCotan:=xCotan * (xRight-xLeft)/(yTop-yBottom);
      eye.z:=xCotan * (xRight-xLeft)/2;
    END;
END;


PROCEDURE TransForm(* src: Point3D; VAR dst: Point3D *);
{  use the current xForm matrix to transform      }
{  a 3D source point into a 3D destination point. }
BEGIN
  IF thePort3D^.ident THEN dst:=src
  ELSE WITH thePort3D^ DO
    BEGIN
      dst.x:=src.x * xForm[0,0] + src.y * xForm[1,0]
           + src.z * xForm[2,0] + xForm[3,0];
      dst.y:=src.x * xForm[0,1] + src.y * xForm[1,1]
           + src.z * xForm[2,1] + xForm[3,1];
      dst.z:=src.x * xForm[0,2] + src.y * xForm[1,2]
           + src.z * xForm[2,2] + xForm[3,2];
    END;
END;


FUNCTION  Clip3D(* src1,src2: Point3D; VAR dst1,dst2: POINT *);
{  do full 3D clipping to viewing pyramid and return 2D        }
{  screen coords in dst.  Function value true if visible.      }
LABEL 0;
TYPE Edge=(left,top,right,bottom);
     OutCode=SET OF Edge;
VAR c,c1,c2: OutCode;
    pt3D: Point3D;
    t: REAL;
    pt1,pt2: POINT;

  PROCEDURE Code(pt3D: Point3D; VAR c: OutCode);
  BEGIN
    c:=[];
    IF pt3D.x < -pt3D.z THEN c:=[left] ELSE IF pt3D.x > pt3D.z THEN c:=[right];
    IF pt3D.y < -pt3D.z THEN c:=c+[bottom] ELSE IF pt3D.y > pt3D.z THEN c:=c+[top];
  END;

BEGIN
  Clip3D:=FALSE;
  WITH thePort3D^ DO
    BEGIN  { convert both points into clipping coord system }
      src1.x:=(src1.x - eye.x) * xCotan;
      src1.y:=(src1.y - eye.y) * yCotan;
      src1.z:=eye.z - src1.z;

      src2.x:=(src2.x - eye.x) * xCotan;
      src2.y:=(src2.y - eye.y) * yCotan;
      src2.z:=eye.z - src2.z;
    END;


  Code(src1,c1); Code(src2,c2);
  WHILE c1+c2 <> [] DO
    BEGIN
      IF c1*c2 <> [] THEN GOTO 0; { both out on same side }
      c:=c1; IF c=[] THEN c:=c2;

      IF left IN c THEN  { calc intersect with left edge }
        BEGIN
          t:=(src1.z+src1.x) / ((src1.x-src2.x) - (src2.z-src1.z));
          pt3D.z:=t*(src2.z-src1.z) + src1.z;
          pt3D.x:=-pt3D.z;
          pt3D.y:=t*(src2.y-src1.y) + src1.y;
        END

      ELSE IF right IN c THEN  { calc intersect with right edge }
        BEGIN
          t:=(src1.z-src1.x) / ((src2.x-src1.x) - (src2.z-src1.z));
          pt3D.z:=t*(src2.z-src1.z) + src1.z;
          pt3D.x:=pt3D.z;
          pt3D.y:=t*(src2.y-src1.y) + src1.y;
        END

      ELSE IF bottom IN c THEN  { calc intersect with bottom edge }
        BEGIN
          t:=(src1.z+src1.y) / ((src1.y-src2.y) - (src2.z-src1.z));
          pt3D.z:=t*(src2.z-src1.z) + src1.z;
          pt3D.x:=t*(src2.x-src1.x) + src1.x;
          pt3D.y:=-pt3D.z;
        END

      ELSE IF top IN c THEN  { calc intersect with top edge }
        BEGIN
          t:=(src1.z-src1.y) / ((src2.y-src1.y) - (src2.z-src1.z));
          pt3D.z:=t*(src2.z-src1.z) + src1.z;
          pt3D.x:=t*(src2.x-src1.x) + src1.x;
          pt3D.y:=pt3D.z;
        END;

      IF c=c1 THEN BEGIN src1:=pt3D; Code(src1,c1); END
      ELSE BEGIN src2:=pt3D; Code(src2,c2); END;

    END;

  { if we reach here, the line from src1 to src2 is visible }
  Clip3D:=TRUE;
  WITH thePort3D^ DO
    WITH GPort^ DO
      BEGIN  { convert clip coords to screen coords }
        dst1.H:=ROUND(hCenter + hSize * src1.x / src1.z);
        dst1.V:=ROUND(vCenter + vSize * src1.y / src1.z);
        dst2.H:=ROUND(hCenter + hSize * src2.x / src2.z);
        dst2.V:=ROUND(vCenter + vSize * src2.y / src2.z);
      END;

0: END;


PROCEDURE Identity;
{  reset the transform matrix to identity }
VAR ROW,COL: INTEGER;
BEGIN;
  WITH thePort3D^ DO
    BEGIN
      FOR ROW:=0 TO 3 DO
        FOR COL:=0 TO 3 DO
          IF ROW=COL THEN xForm[ROW,COL]:=1.0
                     ELSE xForm[ROW,COL]:=0.0;
      ident:=TRUE; { SET FLAG SO xForm CAN BE SKIPPED }
    END;
END;


PROCEDURE Scale(* xFactor,yFactor,zFactor: REAL *);
{  change xForm matrix to provide scaling  }
VAR ROW: INTEGER;
BEGIN
  WITH thePort3D^ DO
    BEGIN
      ident:=FALSE;
      FOR ROW:=0 TO 3 DO
        BEGIN
          xForm[ROW,0]:=xForm[ROW,0]*xFactor;
          xForm[ROW,1]:=xForm[ROW,1]*yFactor;
          xForm[ROW,2]:=xForm[ROW,2]*zFactor;
        END;
    END;
END;


PROCEDURE Translate(* dx,dy,dz: REAL *);
{  change xForm matrix to translate   }
BEGIN
  WITH thePort3D^ DO
    BEGIN
      ident:=FALSE;
      xForm[3,0]:=xForm[3,0]+dx;
      xForm[3,1]:=xForm[3,1]+dy;
      xForm[3,2]:=xForm[3,2]+dz;
    END;
END;


PROCEDURE Pitch(* xAngle: REAL *);
{  change xForm matrix to rotate xAngle degrees around x-Axis  }
VAR si,co,TEMP: REAL;
BEGIN
  xAngle:=xAngle/radConst; { convert degrees to rads }
  si:=SIN(xAngle); co:=COS(xAngle);
  WITH thePort3D^ DO
    BEGIN
      ident:=FALSE;
      TEMP:=xForm[0,1]*co+xForm[0,2]*si;
      xForm[0,2]:=xForm[0,2]*co-xForm[0,1]*si; xForm[0,1]:=TEMP;
      TEMP:=xForm[1,1]*co+xForm[1,2]*si;
      xForm[1,2]:=xForm[1,2]*co-xForm[1,1]*si; xForm[1,1]:=TEMP;
      TEMP:=xForm[2,1]*co+xForm[2,2]*si;
      xForm[2,2]:=xForm[2,2]*co-xForm[2,1]*si; xForm[2,1]:=TEMP;
      TEMP:=xForm[3,1]*co+xForm[3,2]*si;
      xForm[3,2]:=xForm[3,2]*co-xForm[3,1]*si; xForm[3,1]:=TEMP;
    END;
END;


PROCEDURE Yaw(* yAngle: REAL *);
{  change xForm matrix to rotate yAngle degrees around y-Axis  }
VAR si,co,TEMP: REAL;
BEGIN
  yAngle:=yAngle/radConst; { convert degrees to rads }
  si:=SIN(yAngle); co:=COS(yAngle);
  WITH thePort3D^ DO
    BEGIN
      ident:=FALSE;
      TEMP:=xForm[0,0]*co-xForm[0,2]*si;
      xForm[0,2]:=xForm[0,0]*si+xForm[0,2]*co; xForm[0,0]:=TEMP;
      TEMP:=xForm[1,0]*co-xForm[1,2]*si;
      xForm[1,2]:=xForm[1,0]*si+xForm[1,2]*co; xForm[1,0]:=TEMP;
      TEMP:=xForm[2,0]*co-xForm[2,2]*si;
      xForm[2,2]:=xForm[2,0]*si+xForm[2,2]*co; xForm[2,0]:=TEMP;
      TEMP:=xForm[3,0]*co-xForm[3,2]*si;
      xForm[3,2]:=xForm[3,0]*si+xForm[3,2]*co; xForm[3,0]:=TEMP;
    END;
END;


PROCEDURE Roll(* zAngle: REAL *);
{  change xForm matrix to rotate zAngle degrees around z-Axis  }
VAR si,co,TEMP: REAL;
BEGIN
  zAngle:=zAngle/radConst; { convert degrees to rads }
  si:=SIN(zAngle); co:=COS(zAngle);
  WITH thePort3D^ DO
    BEGIN
      ident:=FALSE;
      TEMP:=xForm[0,0]*co+xForm[0,1]*si;
      xForm[0,1]:=xForm[0,1]*co-xForm[0,0]*si; xForm[0,0]:=TEMP;
      TEMP:=xForm[1,0]*co+xForm[1,1]*si;
      xForm[1,1]:=xForm[1,1]*co-xForm[1,0]*si; xForm[1,0]:=TEMP;
      TEMP:=xForm[2,0]*co+xForm[2,1]*si;
      xForm[2,1]:=xForm[2,1]*co-xForm[2,0]*si; xForm[2,0]:=TEMP;
      TEMP:=xForm[3,0]*co+xForm[3,1]*si;
      xForm[3,1]:=xForm[3,1]*co-xForm[3,0]*si; xForm[3,0]:=TEMP;
    END;
END;


PROCEDURE Skew(* zAngle: REAL *);
{  change xForm matrix to skew zAngle degrees around z-Axis  }
{  x := (x + y*TAN(zAngle))  zAngle limited to +-90 degrees  }
VAR co,TA: REAL;
    COL: INTEGER;
BEGIN
  zAngle:=zAngle/radConst; { convert degrees to rads }
  co:= COS(zAngle);
  IF ABS(co) > 1.0E-5 THEN
    BEGIN
      TA:= SIN(zAngle)/co;
      WITH thePort3D^ DO
        BEGIN
          ident:=FALSE;
          FOR COL:=0 TO 2 DO
            xForm[1,COL]:=xForm[1,COL]+xForm[0,COL]*TA;
        END;
    END;
END;


PROCEDURE SetPt3D(* VAR pt3D: Point3D; x,y,z: REAL *);
BEGIN
  pt3D.x:=x;
  pt3D.y:=y;
  pt3D.z:=z;
END;


PROCEDURE SetPt2D(* VAR pt2D: Point2D; x,y: REAL *);
BEGIN
  pt2D.x:=x;
  pt2D.y:=y;
END;


END.   { of Unit }
