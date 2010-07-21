PROGRAM TestGraf;

{ Quick Checkout for QuickDraw }

USES {$U obj:QuickDraw }  QuickDraw,
     {$U obj:QDSupport }  QDSupport,
     {$U obj:GrafUtil  }  GrafUtil;

LABEL 1;

CONST heapSize = $10000;   { 64k bytes }

TYPE IconData = ARRAY[0..95] OF INTEGER;

VAR heapStart:  QDPtr;
    heapLimit:  QDPtr;
    port1:      GrafPtr;
    tempRect:   Rect;
    myPoly:     PolyHandle;
    myRgn:      RgnHandle;
    myPattern:  Pattern;
    myPicture:  PicHandle;
    bigPicture: PicHandle;
    icons:      ARRAY[0..5] OF IconData;
    i,errNum:   INTEGER;
    numerArray: ARRAY[0..30] OF INTEGER;
    denomArray: ARRAY[0..30] OF INTEGER;
    srcRect:    Rect;
    dstRect:    Rect;
    ch:         CHAR;



FUNCTION HeapError(hz: QDPtr; bytesNeeded: INTEGER): INTEGER;
{ this function will be called if the heapZone runs out of space }
BEGIN
  WRITELN('The heap is full.  User Croak !! ');
  Halt;
END;


PROCEDURE InitIcons;
{ Manually stuff some icons.  Normally we would read them from a file }
BEGIN
  { Lisa }
  StuffHex(@icons[0, 0],'000000000000000000000000000000000000001FFFFFFFFC');
  StuffHex(@icons[0,12],'00600000000601800000000B0600000000130FFFFFFFFFA3');
  StuffHex(@icons[0,24],'18000000004311FFFFF00023120000080F231200000BF923');
  StuffHex(@icons[0,36],'120000080F23120000080023120000080023120000080F23');
  StuffHex(@icons[0,48],'1200000BF923120000080F2312000008002311FFFFF00023');
  StuffHex(@icons[0,60],'08000000004307FFFFFFFFA30100000000260FFFFFFFFE2C');
  StuffHex(@icons[0,72],'18000000013832AAAAA8A9F0655555515380C2AAAA82A580');
  StuffHex(@icons[0,84],'800000000980FFFFFFFFF300800000001600FFFFFFFFFC00');

  { Printer }
  StuffHex(@icons[1, 0],'000000000000000000000000000000000000000000000000');
  StuffHex(@icons[1,12],'00000000000000007FFFFF00000080000280000111514440');
  StuffHex(@icons[1,24],'0002000008400004454510400004000017C00004A5151000');
  StuffHex(@icons[1,36],'0004000010000004A54510000004000017FE00F4A5151003');
  StuffHex(@icons[1,48],'0184000013870327FFFFF10F06400000021B0CFFFFFFFC37');
  StuffHex(@icons[1,60],'18000000006B3000000000D77FFFFFFFFFABC00000000356');
  StuffHex(@icons[1,72],'8000000001AC87F000000158841000CCC1B087F000CCC160');
  StuffHex(@icons[1,84],'8000000001C0C000000003807FFFFFFFFF0007800001E000');

  { Trash Can }
  StuffHex(@icons[2, 0],'000001FC000000000E0600000000300300000000C0918000');
  StuffHex(@icons[2,12],'00013849800000026C4980000004C0930000000861260000');
  StuffHex(@icons[2,24],'0010064FE0000031199830000020E6301800002418E00800');
  StuffHex(@icons[2,36],'0033E3801C0000180E002C00000FF801CC0000047FFE0C00');
  StuffHex(@icons[2,48],'000500004C000005259A4C000005250A4C00000525FA4C00');
  StuffHex(@icons[2,60],'000524024C00000524924C00600524924C0090E524924C7C');
  StuffHex(@icons[2,72],'932524924C82A44524924D01C88524924CF10C4524924C09');
  StuffHex(@icons[2,84],'0784249258E70003049233100000E000E40800001FFFC3F0');

  { tray }
  StuffHex(@icons[3, 0],'000000000000000000000000000000000000000000000000');
  StuffHex(@icons[3,12],'0000000000000000000000000000000000000007FFFFFFF0');
  StuffHex(@icons[3,24],'000E00000018001A00000038003600000078006A000000D8');
  StuffHex(@icons[3,36],'00D7FFFFFFB801AC000003580358000006B807FC000FFD58');
  StuffHex(@icons[3,48],'040600180AB80403FFF00D58040000000AB8040000000D58');
  StuffHex(@icons[3,60],'040000000AB807FFFFFFFD5806AC00000AB8055800000D58');
  StuffHex(@icons[3,72],'06B000000AB807FC000FFD70040600180AE00403FFF00DC0');
  StuffHex(@icons[3,84],'040000000B80040000000F00040000000E0007FFFFFFFC00');

  { File Cabinet }
  StuffHex(@icons[4, 0],'0007FFFFFC00000800000C00001000001C00002000003400');
  StuffHex(@icons[4,12],'004000006C0000FFFFFFD40000800000AC0000BFFFFED400');
  StuffHex(@icons[4,24],'00A00002AC0000A07F02D40000A04102AC0000A07F02D400');
  StuffHex(@icons[4,36],'00A00002AC0000A08082D40000A0FF82AC0000A00002D400');
  StuffHex(@icons[4,48],'00A00002AC0000BFFFFED40000800000AC0000BFFFFED400');
  StuffHex(@icons[4,60],'00A00002AC0000A07F02D40000A04102AC0000A07F02D400');
  StuffHex(@icons[4,72],'00A00002AC0000A08082D40000A0FF82AC0000A00002D800');
  StuffHex(@icons[4,84],'00A00002B00000BFFFFEE00000800000C00000FFFFFF8000');

  { drawer }
  StuffHex(@icons[5, 0],'000000000000000000000000000000000000000000000000');
  StuffHex(@icons[5,12],'000000000000000000000000000000000000000000000000');
  StuffHex(@icons[5,24],'000000000000000000000000000000000000000000000000');
  StuffHex(@icons[5,36],'00000000000000000000000000000000000000001FFFFFF0');
  StuffHex(@icons[5,48],'0000380000300000680000700000D80000D0003FFFFFF1B0');
  StuffHex(@icons[5,60],'0020000013500020000016B000201FE01D50002010201AB0');
  StuffHex(@icons[5,72],'00201FE01560002000001AC0002000001580002020101B00');
  StuffHex(@icons[5,84],'00203FF01600002000001C00002000001800003FFFFFF000');

END;


PROCEDURE DrawIcon(whichIcon,h,v: INTEGER);
VAR srcBits: BitMap;
    srcRect,dstRect: Rect;
BEGIN
  srcBits.baseAddr:=@icons[whichIcon];
  srcBits.rowBytes:=6;
  SetRect(srcBits.bounds,0,0,48,32);
  srcRect:=srcBits.bounds;

  dstRect:=srcRect;
  OffsetRect(dstRect,h,v);
  CopyBits(srcBits,thePort^.portBits,srcRect,dstRect,srcOr,Nil);
END;


PROCEDURE DrawStuff;
VAR i: INTEGER;
    tempRect: Rect;
    srcRect: Rect;
    dstRect: Rect;

    dataPtr: QDPtr;
    tempStr: Str255;

BEGIN
  BackColor(whiteColor);
  ForeColor(blackColor);

  { test comments }
  PicComment(100,0,Nil);
  tempStr := 'Hello Test';
  dataPtr := @tempStr;
  PicComment(200,11,@dataPtr);


  tempRect := thePort^.portRect;
  ClipRect(tempRect);
  EraseRoundRect(tempRect,30,20);
  FrameRoundRect(tempRect,30,20);

  { draw two horizontal lines across the top }
  MoveTo(0,18);
  LineTo(719,18);
  MoveTo(0,20);
  LineTo(719,20);

  { draw divider lines }
  MoveTo(0,134);
  LineTo(719,134);
  MoveTo(0,248);
  LineTo(719,248);
  MoveTo(240,21);
  LineTo(240,363);
  MoveTo(480,21);
  LineTo(480,363);

  { draw title }
  TextFont(0);
  MoveTo(210,14);
  DrawString('Look what you can draw with QuickDraw');



  {---------  draw text samples --------- }

  ForeColor(redColor);
  MoveTo(80,34);  DrawString('Red');

  ForeColor(greenColor);
  TextFace([bold]);
  MoveTo(70,55);  DrawString('Green');

  ForeColor(blueColor);
  TextFace([italic]);
  MoveTo(70,70); DrawString('Blue');

  ForeColor(cyanColor);
  TextFace([underline]);
  MoveTo(70,85); DrawString('Cyan');

  ForeColor(magentaColor);
  TextFace([outline]);
  MoveTo(70,100); DrawString('Magenta');

  ForeColor(yellowColor);
  TextFace([shadow]);
  MoveTo(70,115); DrawString('Yellow');

  TextFace([]);   { restore to normal }



  { --------- draw line samples --------- }

  ForeColor(blackColor);
  MoveTo(330,34);  DrawString('Lines');

  ForeColor(redColor);
  MoveTo(280,25);  Line(160,40);

  ForeColor(greenColor);
  PenSize(3,2);
  MoveTo(280,35);  Line(160,40);

  ForeColor(blueColor);
  PenSize(6,4);
  MoveTo(280,46);  Line(160,40);

  ForeColor(cyanColor);
  PenSize(12,8);
  PenPat(gray);
  MoveTo(280,61); Line(160,40);

  ForeColor(magentaColor);
  PenSize(15,10);
  PenPat(myPattern);
  MoveTo(280,80); Line(160,40);
  PenNormal;



  { --------- draw rectangle samples --------- }

  ForeColor(blackColor);
  MoveTo(560,34);  DrawString('Rectangles');

  ForeColor(redColor);
  SetRect(tempRect,510,40,570,70);
  FrameRect(tempRect);

  ForeColor(greenColor);
  OffsetRect(tempRect,25,15);
  PenSize(3,2);
  EraseRect(tempRect);
  FrameRect(tempRect);

  ForeColor(blueColor);
  OffsetRect(tempRect,25,15);
  PaintRect(tempRect);

  ForeColor(cyanColor);
  OffsetRect(tempRect,25,15);
  PenNormal;
  FillRect(tempRect,gray);
  FrameRect(tempRect);

  ForeColor(magentaColor);
  OffsetRect(tempRect,25,15);
  FillRect(tempRect,myPattern);
  FrameRect(tempRect);


  { --------- draw roundRect samples --------- }

  ForeColor(blackColor);
  MoveTo(70,148);  DrawString('RoundRects');

  ForeColor(redColor);
  SetRect(tempRect,30,150,90,180);
  FrameRoundRect(tempRect,30,20);

  ForeColor(greenColor);
  OffsetRect(tempRect,25,15);
  PenSize(3,2);
  EraseRoundRect(tempRect,30,20);
  FrameRoundRect(tempRect,30,20);

  ForeColor(blueColor);
  OffsetRect(tempRect,25,15);
  PaintRoundRect(tempRect,30,20);

  ForeColor(cyanColor);
  OffsetRect(tempRect,25,15);
  PenNormal;
  FillRoundRect(tempRect,30,20,gray);
  FrameRoundRect(tempRect,30,20);

  ForeColor(magentaColor);
  OffsetRect(tempRect,25,15);
  FillRoundRect(tempRect,30,20,myPattern);
  FrameRoundRect(tempRect,30,20);


  { --------- draw bitmap samples --------- }

  ForeColor(blackColor);
  MoveTo(320,148);  DrawString('BitMaps');

  ForeColor(redColor);
  DrawIcon(0,266,156);
  ForeColor(greenColor);
  DrawIcon(1,336,156);
  ForeColor(blueColor);
  DrawIcon(2,406,156);
  ForeColor(cyanColor);
  DrawIcon(3,266,196);
  ForeColor(magentaColor);
  DrawIcon(4,336,196);
  ForeColor(yellowColor);
  DrawIcon(5,406,196);


  { --------- draw ARC samples --------- }

  ForeColor(blackColor);
  MoveTo(570,148);  DrawString('Arcs');

  SetRect(tempRect,520,153,655,243);
  ForeColor(redColor);
  FillArc(tempRect,135,65,dkGray);
  ForeColor(greenColor);
  FillArc(tempRect,200,130,myPattern);
  ForeColor(blueColor);
  FillArc(tempRect,330,75,gray);
  ForeColor(cyanColor);
  FrameArc(tempRect,135,270);
  OffsetRect(tempRect,20,0);
  ForeColor(magentaColor);
  PaintArc(tempRect,45,90);


  { --------- draw polygon samples --------- }

  ForeColor(blackColor);
  MoveTo(80,262);  DrawString('Polygons');

  myPoly:=OpenPoly;
    MoveTo(30,290);
    LineTo(30,280);
    LineTo(50,265);
    LineTo(90,265);
    LineTo(80,280);
    LineTo(95,290);
    LineTo(30,290);
  ClosePoly;       { end of definition }

  ForeColor(redColor);
  FramePoly(myPoly);

  ForeColor(greenColor);
  OffsetPoly(myPoly,25,15);
  PenSize(3,2);
  ErasePoly(myPoly);
  FramePoly(myPoly);

  ForeColor(blueColor);
  OffsetPoly(myPoly,25,15);
  PaintPoly(myPoly);

  ForeColor(cyanColor);
  OffsetPoly(myPoly,25,15);
  PenNormal;
  FillPoly(myPoly,gray);
  FramePoly(myPoly);

  ForeColor(magentaColor);
  OffsetPoly(myPoly,25,15);
  FillPoly(myPoly,myPattern);
  FramePoly(myPoly);

  KillPoly(myPoly);

(*

  { --------- draw region samples --------- }

  ForeColor(blackColor);
  MoveTo(80,262);  DrawString('Regions');

  myRgn := NewRgn;
  OpenRgn;
    MoveTo(30,290);
    LineTo(30,280);
    LineTo(50,265);
    LineTo(90,265);
    LineTo(80,280);
    LineTo(95,290);
    LineTo(30,290);
  CloseRgn(myRgn);       { end of definition }

  ForeColor(redColor);
  FrameRgn(myRgn);

  ForeColor(greenColor);
  OffsetRgn(myRgn,25,15);
  PenSize(3,2);
  EraseRgn(myRgn);
  FrameRgn(myRgn);

  ForeColor(blueColor);
  OffsetRgn(myRgn,25,15);
  PaintRgn(myRgn);

  ForeColor(cyanColor);
  OffsetRgn(myRgn,25,15);
  PenNormal;
  FillRgn(myRgn,gray);
  FrameRgn(myRgn);

  ForeColor(magentaColor);
  OffsetRgn(myRgn,25,15);
  FillRgn(myRgn,myPattern);
  FrameRgn(myRgn);

  DisposeRgn(myRgn);

*)

  { --------- demonstrate region clipping --------- }

  ForeColor(blackColor);
  MoveTo(320,262);  DrawString('Regions');

  myRgn:=NewRgn;
  OpenRgn;
    ShowPen;

    ForeColor(yellowColor);
    SetRect(tempRect,260,270,460,350);
    FrameRoundRect(tempRect,24,16);

    MoveTo(275,335);  { define triangular hole }
    LineTo(325,285);
    LineTo(375,335);
    LineTo(275,335);

    SetRect(tempRect,365,277,445,325);   { oval hole }
    FrameOval(tempRect);

    HidePen;
  CloseRgn(myRgn);       { end of definition }

  SetClip(myRgn);

  BackColor(blueColor);
  ForeColor(greenColor);
  FOR i:=0 TO 6 DO  { draw stuff inside the clip region }
    BEGIN
      MoveTo(260,280+12*i);
      DrawString('Arbitrary Clipping Regions');
    END;
  BackColor(whiteColor);

  ClipRect(thePort^.portRect);
  DisposeRgn(myRgn);


  { --------- draw oval samples --------- }

  ForeColor(blackColor);
  MoveTo(580,262);  DrawString('Ovals');

  ForeColor(redColor);
  SetRect(tempRect,510,264,570,294);
  FrameOval(tempRect);

  ForeColor(greenColor);
  OffsetRect(tempRect,25,15);
  PenSize(3,2);
  EraseOval(tempRect);
  FrameOval(tempRect);

  ForeColor(blueColor);
  OffsetRect(tempRect,25,15);
  PaintOval(tempRect);

  ForeColor(cyanColor);
  OffsetRect(tempRect,25,15);
  PenNormal;
  FillOval(tempRect,gray);
  FrameOval(tempRect);

  ForeColor(magentaColor);
  OffsetRect(tempRect,25,15);
  FillOval(tempRect,myPattern);
  FrameOval(tempRect);

  BackColor(whiteColor);
  ForeColor(blackColor);

  { test large CopyBits }
  SetRect(srcRect,0,0,200,100);
  SetRect(dstRect,50,50,250,150);
  CopyBits(thePort^.portBits,thePort^.portBits,srcRect,dstRect,0,Nil);

END;  { DrawStuff }


PROCEDURE InitScales;
{ initialize an array of common scale factors }
BEGIN
  numerArray[ 0] :=  1;   denomArray[ 0] :=  8;
  numerArray[ 1] :=  1;   denomArray[ 1] :=  4;
  numerArray[ 2] :=  1;   denomArray[ 2] :=  3;
  numerArray[ 3] :=  3;   denomArray[ 3] :=  8;
  numerArray[ 4] :=  1;   denomArray[ 4] :=  2;
  numerArray[ 5] :=  2;   denomArray[ 5] :=  3;
  numerArray[ 6] :=  3;   denomArray[ 6] :=  4;
  numerArray[ 7] :=  1;   denomArray[ 7] :=  1;
  numerArray[ 8] :=  4;   denomArray[ 8] :=  3;
  numerArray[ 9] :=  3;   denomArray[ 9] :=  2;
  numerArray[10] :=  2;   denomArray[10] :=  1;
  numerArray[11] :=  3;   denomArray[11] :=  1;
  numerArray[12] :=  4;   denomArray[12] :=  1;
  numerArray[13] :=  6;   denomArray[13] :=  1;
  numerArray[14] :=  8;   denomArray[14] :=  1;
  numerArray[15] := 16;   denomArray[15] :=  1;
  numerArray[16] := 24;   denomArray[16] :=  1;
  numerArray[17] := 32;   denomArray[17] :=  1;
  numerArray[18] := 40;   denomArray[18] :=  1;
  numerArray[19] := 48;   denomArray[19] :=  1;
  numerArray[20] := 56;   denomArray[20] :=  1;
  numerArray[21] := 64;   denomArray[21] :=  1;
END;


PROCEDURE SetScale(numer,denom: LongInt);
BEGIN
  WITH dstRect DO
    BEGIN
      left  := 360 - (360 * numer) DIV denom;
      right := 360 + (360 * numer) DIV denom;
      top   := 182 - (182 * numer) DIV denom;
      bottom:= 182 + (182 * numer) DIV denom;
    END;
END;


PROCEDURE DumpPicture(myPicture: PicHandle);
LABEL 1;
VAR ch: CHAR;
    i,byteCount: INTEGER;
    count,total: INTEGER;
    picPtr: QDPtr;
    opCode,hiNibble,loNibble: INTEGER;
    sameFlag: BOOLEAN;
    srcBits: BitMap;

    FUNCTION GetWord: INTEGER;
    VAR word: INTEGER;
    BEGIN
      word := BitAnd(picPtr^,$FF);
      picPtr := Pointer(ORD(picPtr)+1);
      word := BitShift(word,+8) + BitAnd(picPtr^,$FF);
      picPtr := Pointer(ORD(picPtr)+1);
      GetWord := word;
    END;

    FUNCTION GetSByte: INTEGER;
    BEGIN
      GetSByte := picPtr^;
      picPtr := Pointer(ORD(picPtr)+1);
    END;

    FUNCTION GetUByte: INTEGER;
    BEGIN
      GetUByte := BitAnd(picPtr^,$FF);
      picPtr := Pointer(ORD(picPtr)+1);
    END;

    FUNCTION GetLong: LongInt;
    VAR long: LongInt;
    BEGIN
      long := BitAnd(picPtr^,$FF);
      picPtr := Pointer(ORD(picPtr)+1);
      long := BitShift(long,+8) + BitAnd(picPtr^,$FF);
      picPtr := Pointer(ORD(picPtr)+1);
      long := BitShift(long,+8) + BitAnd(picPtr^,$FF);
      picPtr := Pointer(ORD(picPtr)+1);
      long := BitShift(long,+8) + BitAnd(picPtr^,$FF);
      picPtr := Pointer(ORD(picPtr)+1);
      GetLong := long;
    END;

BEGIN
  WRITELN;
  WRITELN('picSize = ',myPicture^^.picSize,' bytes');
  WITH myPicture^^.picFrame DO
  WRITELN('picFrame = (',left:1,',',top:1,',',right:1,',',bottom:1,')');

  picPtr := Pointer(ORD(myPicture^) + 10);

1: opCode := GetSByte;
   WRITELN;
   IF opCode = -1 THEN EXIT(DumpPicture);

   loNibble := BitAnd(opCode,$F);
   hiNibble := BitShift(BitAnd(opCode,$F0),-4);

   IF hiNibble = 0 THEN
     BEGIN
       CASE loNibble OF

          1: BEGIN
               WRITE('Set clipRgn ');
               byteCount := GetWord;
               WRITE('rgnSize = ',byteCount);
               picPtr := Pointer(ORD(picPtr) + byteCount - 2);
             END;

          2: BEGIN
               WRITE('Set bkPat');
               picPtr := Pointer(ORD(picPtr) + 8);
             END;

          3: BEGIN
               WRITE('Set txFont ',GetWord);
             END;

          4: BEGIN
               WRITE('Set txFace ',GetUByte);
             END;

          5: BEGIN
               WRITE('Set txMode ',GetWord);
             END;

          6: BEGIN
               WRITE('Set spExtra ',GetWord);
             END;

          7: BEGIN
               WRITE('Set pnSize ',GetWord,GetWord);
             END;

          8: BEGIN
               WRITE('Set pnMode ',GetWord);
             END;

          9: BEGIN
               WRITE('Set pnPat');
               picPtr := Pointer(ORD(picPtr) + 8);
             END;

         10: BEGIN
               WRITE('Set thePat');
               picPtr := Pointer(ORD(picPtr) + 8);
             END;

         11: BEGIN
               WRITE('Set ovalSize ',GetWord,GetWord);
             END;

         12: BEGIN
               WRITE('Set Origin ',GetWord,GetWord);
             END;

         13: BEGIN
               WRITE('Set txSize ',GetWord);
             END;

         14: BEGIN
               WRITE('Set ForeColor ',GetLong);
             END;

         15: BEGIN
               WRITE('Set BackColor ',GetLong);
             END;

       END; { case }
       GOTO 1;
     END;  { if hiNibble = 0 }


   IF hiNibble = 1 THEN
     BEGIN
       CASE loNibble OF

          0: BEGIN
               WRITE('txNumer,txDenom = ',GetWord,GetWord,GetWord,GetWord);
             END;

          1: BEGIN
               WRITE('picVersion = ',GetUByte);
             END;

          OTHERWISE WRITE('OOPS ! OPCODE WAS ',opCode);
       END; { case }
       GOTO 1;
     END;  { if hiNibble = 1 }


   IF hiNibble = 2 THEN
     BEGIN  { text or line }
       CASE loNibble OF
         0: WRITE('Line from ',GetWord,GetWord,' to ',GetWord,GetWord);
         1: WRITE('Line to ',GetWord,GetWord);
         2: WRITE('Line from ',GetWord,GetWord, ' dh,dv = ',GetSByte,GetSByte);
         3: WRITE('Line dh,dv = ',GetSByte,GetSByte);
         8,9,10,11:
            BEGIN  { text }
              CASE loNibble OF
                 8: WRITE('LongText at ',GetWord,GetWord,'  ');
                 9: WRITE('DH Text, dh = ',GetUByte,'   ');
                10: WRITE('DV Text, dv = ',GetUByte,'   ');
                11: WRITE('DHDV Text, dh,dv = ',GetUByte,GetUByte,'   ');
              END;
              byteCount := GetUByte;
              FOR i:= 1 to byteCount DO WRITE(CHR(GetUByte));
            END;
      END;  { case loNibble }
      GOTO 1;
    END;


   IF hiNibble = 9 THEN WITH srcBits, bounds DO
     BEGIN
       sameFlag := FALSE;   { not packed }
       IF BitAnd(loNibble,$8) <> 0 THEN
         BEGIN
           sameFlag := TRUE;  { packed }
           loNibble := BitAnd(loNibble,$7);
           WRITE('Pack');
         END;

       IF loNibble = 0
       THEN WRITELN('BitsRect: ') ELSE WRITELN('BitsRgn: ');
       rowBytes := GetWord;
       top := GetWord;
       left := GetWord;
       bottom := GetWord;
       right := GetWord;

       WRITELN('  rowBytes = ',rowBytes);
       WRITELN('  bounds = ',top,left,bottom,right);
       WRITELN('  srcRect = ',GetWord,GetWord,GetWord,GetWord);
       WRITELN('  dstRect = ',GetWord,GetWord,GetWord,GetWord);
       WRITELN('  mode = ',GetWord);
       IF loNibble <> 0 THEN
         BEGIN
           byteCount := GetWord;
           WRITELN('  maskRgn rgnSize = ',byteCount);
           picPtr := Pointer(ORD(picPtr) + byteCount-2);
         END;
       byteCount := rowBytes*(bottom-top);
       IF sameFlag THEN
         BEGIN
           total := 0;
           FOR i := top TO bottom - 1 DO
             BEGIN
               count := GetUByte;
               total := total + count;
               picPtr := Pointer(ORD(picPtr) + count);
             END;
           WRITELN('  ',byteCount:1,' bytes compressed to ',total);
         END
       ELSE
         BEGIN
           WRITELN(' Uncompressed bytes = ',byteCount);
           picPtr := Pointer(ORD(picPtr) + byteCount);
         END;
       GOTO 1;
     END;

   IF hiNibble = 10 THEN
     BEGIN
       IF loNibble = 0 THEN
         BEGIN
           WRITE('Short Comment ',GetWord);
           GOTO 1;
         END;

       WRITE('Long Comment ',GetWord);
       byteCount := GetWord;
       picPtr := Pointer(ORD(picPtr) + byteCount);
       GOTO 1;
     END;

   IF hiNibble > 10 THEN
     BEGIN
       WRITE('OOPS, hiNibble > 10 !  opcode was ',opCode);
       READLN;
       GOTO 1;
     END;


   { hi nibble is 3..8 }

   sameFlag := FALSE;
   IF BitAnd(loNibble,$8) <> 0 THEN
     BEGIN
       sameFlag := TRUE;
       loNibble := BitAnd(loNibble,$7);
     END;

   CASE loNibble OF
     0: WRITE('Frame');
     1: WRITE('Paint');
     2: WRITE('Erase');
     3: WRITE('Invert');
     4: WRITE('Fill');
   END;

   IF sameFlag THEN WRITE('Same');

   CASE hiNibble OF
     3: BEGIN
          WRITE('Rect');
          IF NOT sameFlag THEN WRITE(GetWord,GetWord,GetWord,GetWord);
        END;

     4: BEGIN
          WRITE('RRect');
          IF NOT sameFlag THEN WRITE(GetWord,GetWord,GetWord,GetWord);
        END;

     5: BEGIN
          WRITE('Oval');
          IF NOT sameFlag THEN WRITE(GetWord,GetWord,GetWord,GetWord);
        END;

     6: BEGIN
          WRITE('Arc');
          IF NOT sameFlag THEN WRITE(GetWord,GetWord,GetWord,GetWord);
          WRITE(GetWord,GetWord);
        END;

     7: BEGIN
          WRITE('Poly');
          byteCount := GetWord;
          WRITE(' polySize = ',byteCount);
          picPtr := Pointer(ORD(picPtr) + byteCount-2);
        END;

     8: BEGIN
          WRITE('Rgn');
          byteCount := GetWord;
          WRITE(' rgnSize = ',byteCount);
          picPtr := Pointer(ORD(picPtr) + byteCount-2);
        END;

   END;

 GOTO 1;

END;


BEGIN  { main program }
  WRITE('Press return '); READLN;
  NEW(heapStart);
  heapLimit:=Pointer(ORD(heapStart)+heapSize);
  RELEASE(heapLimit);   { forward release to allocate }
  InitHeap(heapStart,heapLimit,@heapError);
  InitGraf(@thePort);

  InitCursor;
  HideCursor;
  FMInit(errNum);
  IF errNum <> 0 THEN
    BEGIN
      WRITELN('FMInit says errNum=',errNum);
      HALT;
    END;

  StuffHex(@myPattern,'8040200002040800');

  InitIcons;
  InitScales;

  NEW(port1);
  OpenPort(port1);
  PaintRect(thePort^.portRect);
  DrawStuff;
  READLN;

  myPicture := OpenPicture(thePort^.portRect);
    DrawStuff;
  ClosePicture;
  WRITELN('picSize = ',myPicture^^.picSize:1);
  READLN;
  DrawPicture(myPicture,thePort^.portRect);

  WRITE('Dump ?'); READ(ch); WRITELN;
  IF ch IN ['Y','y'] THEN
    BEGIN
      DumpPicture(myPicture);
      READLN;
    END;

  WRITE('ABOUT TO make bigPicture '); READLN;
  bigPicture := OpenPicture(thePort^.portRect);
    DrawPicture(myPicture,thePort^.portRect);
    tempRect := thePort^.portRect;
    InsetRect(tempRect,100,50);
    DrawPicture(myPicture,tempRect);
    InsetRect(tempRect,100,50);
    DrawPicture(myPicture,tempRect);
  ClosePicture;
  WRITELN('big picSize = ',bigPicture^^.picSize:1);
  READLN;
  DrawPicture(bigPicture,thePort^.portRect);

  WRITE('Dump Big ?'); READ(ch); WRITELN;
  IF ch IN ['Y','y'] THEN
    BEGIN
      DumpPicture(bigPicture);
      READLN;
    END;

  KillPicture(bigPicture);

  WRITE('ABOUT TO DO NORMAL from picture '); READLN;
  ColorBit(normalBit);
  DrawPicture(myPicture,thePort^.portRect);

  WRITE('ABOUT TO DO INVERSE from picture '); READLN;
  ColorBit(inverseBit);
  DrawPicture(myPicture,thePort^.portRect);

  WRITE('ABOUT TO DO CYAN from picture '); READLN;
  ColorBit(cyanBit);
  DrawPicture(myPicture,thePort^.portRect);

  WRITE('ABOUT TO DO MAGENTA from picture '); READLN;
  ColorBit(magentaBit);
  DrawPicture(myPicture,thePort^.portRect);

  WRITE('ABOUT TO DO YELLOW from picture '); READLN;
  ColorBit(yellowBit);
  DrawPicture(myPicture,thePort^.portRect);

  WRITE('ABOUT TO DO BLACK from picture'); READLN;
  ColorBit(blackBit);
  DrawPicture(myPicture,thePort^.portRect);

  ColorBit(normalBit);
  READLN;

  PaintRect(thePort^.portRect);
  REPEAT
    FOR i:=0 to 15 DO
      BEGIN
        SetScale(numerArray[i],denomArray[i]);
        DrawPicture(myPicture,dstRect);
        IF MouseButton THEN GOTO 1;
      END;

    FOR i:=14 DOWNTO 1 DO
      BEGIN
        SetScale(numerArray[i],denomArray[i]);
        DrawPicture(myPicture,dstRect);
        IF MouseButton THEN GOTO 1;
      END;
  UNTIL FALSE;

1: ShowCursor;

  SetRect(myPicture^^.picFrame,0,0,200,100);

   REPEAT UNTIL NOT MouseButton;
   REPEAT
     REPEAT UNTIL MouseButton;
     GetMouse(dstRect.topLeft);
     REPEAT UNTIL NOT MouseButton;
     GetMouse(dstRect.botRight);
     ClipRect(thePort^.portRect);
     PaintRect(thePort^.portRect);
     DrawPicture(myPicture,dstRect);
     IF MouseButton THEN EXIT(TestGraf);
   UNTIL FALSE;

  KillPicture(myPicture);
END.
