UNIT rbGamePad;

INTERFACE
USES RobPort, Windows, SysUtils, MMSystem;
//     Classes, Messages;


CONST PadsMAX = 4;

TYPE  TprocEvent = PROCEDURE(J:DWORD; VAR GPD; EVFLAG:DWORD); STDCALL;    //GPD:TRBGamePadWork

CONST RBFLAG_MOVE       = $00000001;
CONST RBFLAG_PRESSDOWN  = $00000002;
CONST RBFLAG_PRESSUP    = $00000004;

CONST RBFL_NOTDEFINED   = $00000000;
CONST RBFL_XP           = $00010000;
CONST RBFL_YP           = $00020000;
CONST RBFL_ZP           = $00030000;
CONST RBFL_XQ           = $00040000;
CONST RBFL_YQ           = $00050000;
CONST RBFL_ZQ           = $00060000;
CONST RBFL_BUTA         = $00100000;
CONST RBFL_BUTB         = $00110000;
CONST RBFL_BUTX         = $00120000;
CONST RBFL_BUTY         = $00130000;
CONST RBFL_BUTLB        = $00140000;
CONST RBFL_BUTRB        = $00150000;
CONST RBFL_BUTBA        = $00160000;
CONST RBFL_BUTST        = $00170000;
CONST RBFL_BUTJ1        = $00180000;
CONST RBFL_BUTJ2        = $00190000;
CONST RBFL_BUTLT        = $001A0000;
CONST RBFL_BUTRT        = $001B0000;
CONST RBFL_BUTPOVUP     = $001C0000;
CONST RBFL_BUTPOVRI     = $001D0000;
CONST RBFL_BUTPOVDO     = $001E0000;
CONST RBFL_BUTPOVLE     = $001F0000;

CONST RBFL_EVENT_MASK   = $00FFF000;

TYPE TRBDifVal = RECORD
      V:ARRAY[0..1] OF LONGINT;
      EV              :LONGINT;
      DEL             :LONGINT;
      DELS            :LONGINT;
      FLAG            :DWORD;
     END;

TYPE TRBGamePadWork=RECORD
      PAD       :ARRAY[0..PadsMAX-1] OF RECORD
       FJoy       :JOYCAPS;
       FJoyInfo   :JOYINFOEX;
       Present    :BOOLEAN;
       EventP     :TprocEvent;

       DIF        :RECORD
        XP        :TRBDifVal;
        YP        :TRBDifVal;
        ZP        :TRBDifVal;
        XQ        :TRBDifVal;
        YQ        :TRBDifVal;
        ZQ        :TRBDifVal;
        BUTA      :TRBDifVal;
        BUTB      :TRBDifVal;
        BUTX      :TRBDifVal;
        BUTY      :TRBDifVal;
        BUTLB     :TRBDifVal;
        BUTRB     :TRBDifVal;
        BUTBA     :TRBDifVal;
        BUTST     :TRBDifVal;
        BUTJ1     :TRBDifVal;
        BUTJ2     :TRBDifVal;
        BUTLT     :TRBDifVal;
        BUTRT     :TRBDifVal;
        BUTPOVUP  :TRBDifVal;
        BUTPOVRI  :TRBDifVal;
        BUTPOVDO  :TRBDifVal;
        BUTPOVLE  :TRBDifVal;
       END;
       EventCounter :DWORD;
      END;
      THREAD:RECORD
       ThreadHND              :Integer;
       ThreadID               :LongWord;
       WO                     :DWORD;
      END;
      TimeRefreshMS   :DWORD;
      TimerRefMS      :INT64;

      NAME            :ShortString;
     END;

VAR GMPadWork:TRBGamePadWork;


TYPE TGamePadLST = RECORD
      PADID     :DWORD;
      NAME      :STRING;
     END;
TYPE TTGamePadLST = ^TGamePadLST;

CONST VK_NOTDEFINED = $FFFFFFFF;

VAR GamePad_Key_List:ARRAY[0..23] OF TGamePadLST=(
  (PADID:RBFL_NOTDEFINED    ;NAME:'NONE                '     ),
  (PADID:RBFL_BUTA          ;NAME:'PAD_Button A        '     ),
  (PADID:RBFL_BUTB          ;NAME:'PAD_Button B        '     ),
  (PADID:RBFL_BUTX          ;NAME:'PAD_Button X        '     ),
  (PADID:RBFL_BUTY          ;NAME:'PAD_Button Y        '     ),
  (PADID:RBFL_BUTLB         ;NAME:'PAD_Button LB       '     ),
  (PADID:RBFL_BUTRB         ;NAME:'PAD_Button RB       '     ),
  (PADID:RBFL_BUTLT         ;NAME:'PAD_Button LT       '     ),
  (PADID:RBFL_BUTRT         ;NAME:'PAD_Button RT       '     ),
  (PADID:RBFL_BUTPOVUP      ;NAME:'PAD_Button Pov Up   '     ),
  (PADID:RBFL_BUTPOVDO      ;NAME:'PAD_Button Pov Down '     ),
  (PADID:RBFL_BUTPOVLE      ;NAME:'PAD_Button Pov Left '     ),
  (PADID:RBFL_BUTPOVRI      ;NAME:'PAD_Button Pov Right'     ),
  (PADID:RBFL_BUTST         ;NAME:'PAD_Button START    '     ),
  (PADID:RBFL_BUTBA         ;NAME:'PAD_Button BACK     '     ),
  (PADID:RBFL_BUTJ1         ;NAME:'PAD_Button Joy 1    '     ),
  (PADID:RBFL_BUTJ2         ;NAME:'PAD_Button Joy 2    '     ),
  (PADID:RBFL_XP            ;NAME:'PAD_Axis_X Joy 1    '     ),
  (PADID:RBFL_YP            ;NAME:'PAD_Axis_Y Joy 1    '     ),
  (PADID:RBFL_XQ            ;NAME:'PAD_Axis_X Joy 2    '     ),
  (PADID:RBFL_YQ            ;NAME:'PAD_Axis_Y Joy 2    '     ),
  (PADID:RBFL_ZP            ;NAME:'PAD_Axis_Z xT       '     ),
  (PADID:RBFL_ZQ            ;NAME:'PAD_Axis_Z xQ       '     ),
  (PADID:0                  ;NAME:''                         )
);
//  (PADID:RBFL_XP            ;NAME:'PAD_XP   '     ),

FUNCTION RBGamePad_LST_COUNT:DWORD;
FUNCTION RBGamePad_LST(NUM:DWORD):TTGamePadLST;
FUNCTION RBGamePad_STR(ID:DWORD):STRING;


PROCEDURE RBGamePadInit(PERMS:DWORD);
PROCEDURE RBGamePadClose;
FUNCTION RBGamePadRefreshJoy(J:DWORD):BOOLEAN;
FUNCTION RBGamePadRefreshJoyALL:BOOLEAN;


IMPLEMENTATION





FUNCTION RBGamePad_LST_COUNT:DWORD;
VAR N:LONGINT;
    S:^STRING;
    V:^DWORD;
BEGIN
    N:=0;
    REPEAT
     RESULT:=N;
     V:=@GamePad_Key_List[N].PADID;
     S:=@GamePad_Key_List[N].NAME;
     IF ((V^=0) AND (S^='')) THEN BREAK;
     INC(N);
    UNTIL FALSE;
END;


FUNCTION RBGamePad_LST(NUM:DWORD):TTGamePadLST;
VAR N:LONGINT;
    S:^STRING;
    V:^DWORD;
BEGIN
    N:=0;
    REPEAT
     RESULT:=@GamePad_Key_List[N];
     V:=@GamePad_Key_List[N].PADID;
     S:=@GamePad_Key_List[N].NAME;
     IF ((V^=0) AND (S^='')) OR (N=NUM) THEN BREAK;
     INC(N);
    UNTIL FALSE;
END;

FUNCTION RBGamePad_STR(ID:DWORD):STRING;
VAR N:LONGINT;
    S:^STRING;
    V:^DWORD;
BEGIN
    N:=0;
    REPEAT
     V:=@GamePad_Key_List[N].PADID;
     S:=@GamePad_Key_List[N].NAME;
     RESULT:=S^;
     IF ((V^=0) AND (S^='')) OR (V^=ID) THEN BREAK;
     INC(N);
    UNTIL FALSE;
END;












PROCEDURE RBGamePadTHREAD(VAR WPDD:TRBGamePadWork); STDCALL;
VAR N,M,I:LONGINT;
BEGIN
    IF (GMPadWork.TimeRefreshMS<  10) THEN GMPadWork.TimeRefreshMS:=10;
    IF (GMPadWork.TimeRefreshMS>1000) THEN GMPadWork.TimeRefreshMS:=1000;
    muStartTimerWait(GMPadWork.TimerRefMS, 1);
    REPEAT

     CASE GMPadWork.THREAD.WO OF
      $00000000:BEGIN
       IF ( muCheckTimerWait(GMPadWork.TimerRefMS) ) THEN BEGIN
        RBGamePadRefreshJoyALL();
        muStartTimerWait(GMPadWork.TimerRefMS, GMPadWork.TimeRefreshMS*1000);//SLEEP(GMPadWork.TimeRefreshMS);
       END;
      END;
      $FFFFFFFF:BEGIN
       BREAK;
      END;
      ELSE GMPadWork.THREAD.WO:=$00000000;
     END;
    UNTIL FALSE;
    GMPadWork.THREAD.WO:=0;
    EndThread(0);
END;

FUNCTION RBGamePadCreateJoy(J:INTEGER):BOOLEAN;
BEGIN
  IF joyGetDevCaps(J, @GMPadWork.PAD[J].FJoy, SizeOf(GMPadWork.PAD[J].FJoy)) = MMSYSERR_NODRIVER THEN BEGIN
   RESULT:=FALSE;
  END ELSE BEGIN
   RESULT:=TRUE;
  END;

END;

PROCEDURE RBGamePadInit(PERMS:DWORD);
VAR N:INTEGER;
BEGIN
    FOR N:=0 TO PadsMAX-1 DO BEGIN
     RBGamePadCreateJoy(N);
    END;
    GMPadWork.NAME := 'Game Pad By Rob F.';
    GMPadWork.TimeRefreshMS := PERMS;
    GMPadWork.THREAD.WO := $00000000;
    GMPadWork.THREAD.ThreadHND := BeginThread(NIL, 0, Addr(RBGamePadTHREAD), POINTER(DWORD(@GMPadWork)), 0, GMPadWork.THREAD.ThreadID);
END;

PROCEDURE RBGamePadClose;
VAR N:INTEGER;
BEGIN
    GMPadWork.THREAD.WO := $FFFFFFFF;
    N:=9;
    WHILE N>0 DO BEGIN
     IF (GMPadWork.THREAD.WO=0) THEN BEGIN
      BREAK;
     END;
     SLEEP(10);
     DEC(N);
    END;
    IF (N=0) THEN BEGIN
     TerminateThread(GMPadWork.THREAD.ThreadHND, 0);
    END;
    FOR N:=0 TO PadsMAX-1 DO BEGIN
     joyReleaseCapture(N);
    END;
END;

FUNCTION RBGamePadRefreshJoy(J:DWORD):BOOLEAN;
BEGIN
    GMPadWork.PAD[J].FJoyInfo.dwSize := SizeOf(JOYINFOEX);
    GMPadWork.PAD[J].FJoyInfo.dwFlags := JOY_RETURNALL;
    IF joyGetPosEX(J, @GMPadWork.PAD[J].FJoyInfo) = JOYERR_NOERROR THEN BEGIN
     GMPadWork.PAD[J].Present:=TRUE;
     RESULT:=TRUE;
    END ELSE BEGIN
     GMPadWork.PAD[J].Present:=FALSE;
     RESULT:=FALSE;
    END;
END;


FUNCTION RBGamePadProcDIF(J:DWORD; VAR DF:TRBDifVal; V:LONGINT; AFL:DWORD):DWORD;
VAR FLAG:DWORD;
BEGIN
     FLAG := 0;
     DF.EV := V;
     DF.V[1] := DF.V[0];
     DF.V[0] := V;
     DF.DEL := DF.V[1] - DF.V[0];
     DF.DELS := DF.DELS + DF.DEL;
     IF DF.V[0]<>DF.V[1] THEN BEGIN
      FLAG := AFL OR RBFLAG_MOVE;
      DF.FLAG := FLAG;
      IF (DWORD(@GMPadWork.PAD[0].EventP)<>0) THEN BEGIN
       GMPadWork.PAD[0].EventP(J, GMPadWork, FLAG );
      END;
     END;
     RESULT:=FLAG;
END;

FUNCTION RBGamePadProcBUT(J:DWORD; VAR DF:TRBDifVal; V:DWORD; AFL:DWORD):DWORD;
VAR FLAG:DWORD;
BEGIN
     IF (V<>0) THEN BEGIN
      DF.EV := 1;
     END ELSE BEGIN
      DF.EV := 0;
     END;
     FLAG := 0;
     DF.V[1] := DF.V[0];
     DF.V[0] := V;
     IF DF.V[0]<>DF.V[1] THEN BEGIN
      FLAG := AFL;
      IF V<>0 THEN BEGIN
       FLAG := FLAG OR RBFLAG_PRESSDOWN;
      END ELSE BEGIN
       FLAG := FLAG OR RBFLAG_PRESSUP;
      END;
      DF.FLAG := FLAG;
      IF (DWORD(@GMPadWork.PAD[0].EventP)<>0) THEN BEGIN
       GMPadWork.PAD[0].EventP(J, GMPadWork, FLAG );
      END;
     END;
     RESULT:=FLAG;
END;

FUNCTION RBGamePadProcBUTPOV(J:DWORD; VAR DF:TRBDifVal; V:DWORD; DIR:LONGINT; AFL:DWORD):DWORD;
VAR BUTPOV:DWORD;
    DIRL:LONGINT;
    DIRC:LONGINT;
    DIRR:LONGINT;
BEGIN
     DIRL := DIR - 4500;
     DIRC := DIR;
     DIRR := DIR + 4500;
     IF (DIRL<0) THEN BEGIN
      DIRL:=DIRL+36000;
      DIRC:=DIRC+36000;
     END;
     BUTPOV := ( BYTE( ((V>=DIRL) AND (V<=DIRC)) OR ((V>=DIR) AND (V<=DIRR)) ) );
     RESULT := RBGamePadProcBUT(J, DF, BUTPOV, AFL);
END;


PROCEDURE RBGamePadProcessNeedEvent(J:DWORD);
BEGIN
     IF RBGamePadProcDIF(J, GMPadWork.PAD[J].DIF.XP, Smallint(GMPadWork.PAD[J].FJoyInfo.wXpos +32768),  RBFL_XP ) <> 0 THEN INC(GMPadWork.PAD[J].EventCounter);
     IF RBGamePadProcDIF(J, GMPadWork.PAD[J].DIF.YP, Smallint(GMPadWork.PAD[J].FJoyInfo.wYpos +32768),  RBFL_YP ) <> 0 THEN INC(GMPadWork.PAD[J].EventCounter);
     IF RBGamePadProcDIF(J, GMPadWork.PAD[J].DIF.ZP, Smallint(GMPadWork.PAD[J].FJoyInfo.wZpos +32768),  RBFL_ZP ) <> 0 THEN INC(GMPadWork.PAD[J].EventCounter);
     IF RBGamePadProcDIF(J, GMPadWork.PAD[J].DIF.XQ, Smallint(GMPadWork.PAD[J].FJoyInfo.dwUpos+32768),  RBFL_XQ ) <> 0 THEN INC(GMPadWork.PAD[J].EventCounter);
     IF RBGamePadProcDIF(J, GMPadWork.PAD[J].DIF.YQ, Smallint(GMPadWork.PAD[J].FJoyInfo.dwRpos+32768),  RBFL_YQ ) <> 0 THEN INC(GMPadWork.PAD[J].EventCounter);
     IF RBGamePadProcDIF(J, GMPadWork.PAD[J].DIF.ZQ, Smallint(GMPadWork.PAD[J].FJoyInfo.dwVpos+0    ),  RBFL_ZQ ) <> 0 THEN INC(GMPadWork.PAD[J].EventCounter);

     IF RBGamePadProcBUT(J, GMPadWork.PAD[J].DIF.BUTA,  GMPadWork.PAD[J].FJoyInfo.wButtons AND $0001,  RBFL_BUTA  ) <> 0 THEN INC(GMPadWork.PAD[J].EventCounter);
     IF RBGamePadProcBUT(J, GMPadWork.PAD[J].DIF.BUTB,  GMPadWork.PAD[J].FJoyInfo.wButtons AND $0002,  RBFL_BUTB  ) <> 0 THEN INC(GMPadWork.PAD[J].EventCounter);
     IF RBGamePadProcBUT(J, GMPadWork.PAD[J].DIF.BUTX,  GMPadWork.PAD[J].FJoyInfo.wButtons AND $0004,  RBFL_BUTX  ) <> 0 THEN INC(GMPadWork.PAD[J].EventCounter);
     IF RBGamePadProcBUT(J, GMPadWork.PAD[J].DIF.BUTY,  GMPadWork.PAD[J].FJoyInfo.wButtons AND $0008,  RBFL_BUTY  ) <> 0 THEN INC(GMPadWork.PAD[J].EventCounter);
     IF RBGamePadProcBUT(J, GMPadWork.PAD[J].DIF.BUTLB, GMPadWork.PAD[J].FJoyInfo.wButtons AND $0010,  RBFL_BUTLB ) <> 0 THEN INC(GMPadWork.PAD[J].EventCounter);
     IF RBGamePadProcBUT(J, GMPadWork.PAD[J].DIF.BUTRB, GMPadWork.PAD[J].FJoyInfo.wButtons AND $0020,  RBFL_BUTRB ) <> 0 THEN INC(GMPadWork.PAD[J].EventCounter);
     IF RBGamePadProcBUT(J, GMPadWork.PAD[J].DIF.BUTBA, GMPadWork.PAD[J].FJoyInfo.wButtons AND $0040,  RBFL_BUTBA ) <> 0 THEN INC(GMPadWork.PAD[J].EventCounter);
     IF RBGamePadProcBUT(J, GMPadWork.PAD[J].DIF.BUTST, GMPadWork.PAD[J].FJoyInfo.wButtons AND $0080,  RBFL_BUTST ) <> 0 THEN INC(GMPadWork.PAD[J].EventCounter);
     IF RBGamePadProcBUT(J, GMPadWork.PAD[J].DIF.BUTJ1, GMPadWork.PAD[J].FJoyInfo.wButtons AND $0100,  RBFL_BUTJ1 ) <> 0 THEN INC(GMPadWork.PAD[J].EventCounter);
     IF RBGamePadProcBUT(J, GMPadWork.PAD[J].DIF.BUTJ2, GMPadWork.PAD[J].FJoyInfo.wButtons AND $0200,  RBFL_BUTJ2 ) <> 0 THEN INC(GMPadWork.PAD[J].EventCounter);

     IF RBGamePadProcBUT(J, GMPadWork.PAD[J].DIF.BUTLT, BYTE(GMPadWork.PAD[J].DIF.ZP.V[0]>( 8192)),    RBFL_BUTLT ) <> 0 THEN INC(GMPadWork.PAD[J].EventCounter);
     IF RBGamePadProcBUT(J, GMPadWork.PAD[J].DIF.BUTRT, BYTE(GMPadWork.PAD[J].DIF.ZP.V[0]<(-8192)),    RBFL_BUTRT ) <> 0 THEN INC(GMPadWork.PAD[J].EventCounter);

     IF RBGamePadProcBUTPOV(J, GMPadWork.PAD[J].DIF.BUTPOVUP, GMPadWork.PAD[J].FJoyInfo.dwPOV,     0, RBFL_BUTPOVUP ) <> 0 THEN INC(GMPadWork.PAD[J].EventCounter);
     IF RBGamePadProcBUTPOV(J, GMPadWork.PAD[J].DIF.BUTPOVRI, GMPadWork.PAD[J].FJoyInfo.dwPOV,  9000, RBFL_BUTPOVRI ) <> 0 THEN INC(GMPadWork.PAD[J].EventCounter);
     IF RBGamePadProcBUTPOV(J, GMPadWork.PAD[J].DIF.BUTPOVDO, GMPadWork.PAD[J].FJoyInfo.dwPOV, 18000, RBFL_BUTPOVDO ) <> 0 THEN INC(GMPadWork.PAD[J].EventCounter);
     IF RBGamePadProcBUTPOV(J, GMPadWork.PAD[J].DIF.BUTPOVLE, GMPadWork.PAD[J].FJoyInfo.dwPOV, 27000, RBFL_BUTPOVLE ) <> 0 THEN INC(GMPadWork.PAD[J].EventCounter);

END;



FUNCTION RBGamePadRefreshJoyALL:BOOLEAN;
VAR N:INTEGER;
BEGIN
    RESULT:=FALSE;
    FOR N:=0 TO PadsMAX-1 DO BEGIN
     IF ( RBGamePadRefreshJoy(N) ) THEN BEGIN
      RBGamePadProcessNeedEvent(N);
      RESULT:=TRUE;
     END;
    END;
END;


BEGIN
//    RBGamePadInit();

  FillChar(GMPadWork, SIZEOF(GMPadWork), 0);

END.




