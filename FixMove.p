PROGRAM FixMove;

{ Assembler restrictions require procedure Move to assembled as MQVE.  }
{ This program back patches the object file replacing 'MQVE' with 'MOVE' }

VAR f: FILE;
    fileName: String[30];
    buffer: PACKED ARRAY[0..1023] OF CHAR;
    i,hitCount: INTEGER;

BEGIN

  REPEAT
    WRITE('file to patch:');
    READLN(fileName);
    RESET(f,fileName);
  UNTIL IORESULT = 0;

  i := BlockRead(f,buffer,2,0);

  hitCount := 0;
  FOR i := 0 TO 1020 DO
    BEGIN
      IF  (buffer[i  ] = 'M')
      AND (buffer[i+1] = 'Q')
      AND (buffer[i+2] = 'V')
      AND (buffer[i+3] = 'E')
      THEN
        BEGIN
          buffer[i+1] := 'O';
          hitCount := hitCount + 1;
        END;
    END;

  WRITE(hitCount:1,' matches found.');
  IF hitCount = 0 THEN WRITELN(CHR(7))
  ELSE
    BEGIN
      i := BlockWrite(f,buffer,2,0);
      IF IORESULT <> 0 THEN WRITELN('Oops, trouble writing');
    END;

  CLOSE(f,lock);

END.
