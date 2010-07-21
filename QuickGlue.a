; File: QuickGlue.TEXT
;------------------------------------------------------------------
;
;  QuickDraw/Mac OS Interface
;
;  written by Andy Hertzfeld  16-Sept-82
;
;  (c) 1982 by Apple Computer, Inc.  All rights reserved.
;
;   QuickGlue is the QuickDraw/Mac OS interface.  It is linked with QuickDraw and
;   defines all of the externals required by QuickDraw except those of the
;   font manager.  All of these are very short and simple (memory manager traps or
;   jumps through the graphics jump table).
;
;  Modification History
;
;   16-Nov-82  AJH  Made font manager interface go through graphics jump table
;   09-Feb-83  AJH  Added LockHandle, UnLockHandle
;   17-Aug-83  SC   Made all cursor jumps preserve A0
;   22-Apr-85  LAK  Removed RInitGraf (coordinated with Bill clearing
;                    QDExist flag in InitGraf).
;------------------------------------------------------------------

        .INCLUDE        tlasm-SysTlQk.Sym

;
;  Here is a subset of Unit Storage (the ones needed by
;  QuickDraw), implemented by trapping to the Mac OS.
;


;
; FUNCTION NewHandle(byteCount: INTEGER): Ptr;
;
       .FUNC   NewHandle,1
;
       MOVEQ    #0,D0          ;clear out high part
       MOVE.L  (SP)+,A1        ;get return address
       MOVE.W  (SP)+,D0        ;get the byte count
       _NEWHANDLE              ;ask OS to do request
       BNE.S    MemFull        ;if memory full, deep shit!
       MOVE.L  A0,(SP)         ;return result handle on stack
       JMP     (A1)            ;return to caller

; handle the memory full error by deep-shitting

MemFull
       MOVEQ    #DSMemFullErr,D0
       _SysError
       .WORD    $A9FF           ;invoke debugger just in case it comes back

;
; PROCEDURE SetSize(h: Handle; newSize: INTEGER);
;
        .DEF    SetSize
;
SetSize
        MOVEQ    #0,D0          ;clear out high part
        MOVE.L  (SP)+,A1        ;get return address
        MOVE.W  (SP)+,D0        ;get the new size
        MOVE.L  (SP)+,A0        ;get the handle
        _SETHANDLESIZE          ;let OS do it
        BNE.S   MemFull         ;if out of memory, deepShit
        JMP     (A1)            ;return to caller

;
; PROCEDURE DisposeHandle(h: Handle);
;
       .PROC   DisposeHandle,2
;
       MOVE.L  (SP)+,A1        ;get return address
       MOVE.L  (SP)+,A0        ;get parameter
       _DISPOSHANDLE           ;let OS do work
       JMP     (A1)            ;return to caller
;
; PROCEDURE LockHandle(h: Handle);
;
        .PROC   LockHandle

        MOVE.L  4(SP),A0
        BSET    #7,(A0)
        MOVE.L  (SP)+,(SP)
        RTS
;
; PROCEDURE UnLockHandle(h: handle);
;
        .PROC   UnlockHandle

        MOVE.L  4(SP),A0
        BCLR    #7,(A0)
        MOVE.L  (SP)+,(SP)
        RTS

;
;  Following is the QuickDraw cursor interface, implemented by accessing
;  system routines through the graphics jump table
;
        .PROC   CursorDisplay,0
;
        MOVE.L  JShowCursor,-(SP)
        RTS
;
        .PROC   CursorHide,0
;
        MOVE.L  JHideCursor,-(SP)
        RTS
;
        .PROC   CursorImage,0
;
        MOVE.L  JSetCrsr,-(SP)
        RTS
;
        .PROC   CursorInit,0
;
        MOVE.L  JInitCrsr,-(SP)
        RTS
;
        .PROC   CursorObscure,0
;
        MOVE.L  JCrsrObscure,-(SP)
        RTS
;
        .PROC   CursorShield,0
;
        MOVE.L  JShieldCursor,-(SP)
        RTS
;
        .PROC   ScreenAdress,0
;
        MOVE.L  JScrnAddr,-(SP)
        RTS
;
        .PROC   ScreenSize,0
;
        MOVE.L  JScrnSize,-(SP)
        RTS
;
        .PROC   FMSwapFont,0

        MOVE.L  JSwapFont,-(SP)
        RTS



       .END
