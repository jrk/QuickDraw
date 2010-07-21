UNIT GrafUtil;

INTERFACE

USES {$U obj:QuickDraw }  QuickDraw;

TYPE Fixed = LongInt;
     Int64Bit = RECORD
                  hiLong: LongInt;
                  loLong: LongInt;
                END;

FUNCTION  BitAnd   (long1,long2: LongInt): LongInt;
FUNCTION  BitOr    (long1,long2: LongInt): LongInt;
FUNCTION  BitXor   (long1,long2: LongInt): LongInt;
FUNCTION  BitNot   (long: LongInt): LongInt;
FUNCTION  BitShift (long: LongInt; count: INTEGER): LongInt;
FUNCTION  BitTst   (bytePtr: QDPtr; bitNum: LongInt): BOOLEAN;
PROCEDURE BitSet   (bytePtr: QDPtr; bitNum: LongInt);
PROCEDURE BitClr   (bytePtr: QDPtr; bitNum: LongInt);
PROCEDURE LongMul  (a,b: LongInt; VAR dst: Int64Bit);
FUNCTION  FixMul   (a,b: Fixed): Fixed;
FUNCTION  FixRatio (numer,denom: INTEGER): Fixed;
FUNCTION  HiWord   (x: Fixed): INTEGER;
FUNCTION  LoWord   (x: Fixed): INTEGER;
FUNCTION  FixRound (x: Fixed): INTEGER;


IMPLEMENTATION

FUNCTION  BitAnd;     EXTERNAL;
FUNCTION  BitOr;      EXTERNAL;
FUNCTION  BitXor;     EXTERNAL;
FUNCTION  BitNot;     EXTERNAL;
FUNCTION  BitShift;   EXTERNAL;
FUNCTION  BitTst;     EXTERNAL;
PROCEDURE BitSet;     EXTERNAL;
PROCEDURE BitClr;     EXTERNAL;
PROCEDURE LongMul;    EXTERNAL;
FUNCTION  FixMul;     EXTERNAL;
FUNCTION  FixRatio;   EXTERNAL;
FUNCTION  HiWord;     EXTERNAL;
FUNCTION  LoWord;     EXTERNAL;
FUNCTION  FixRound;   EXTERNAL;


END.  { of unit }
