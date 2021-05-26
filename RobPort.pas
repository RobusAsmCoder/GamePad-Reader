//------------------------------------------------------------------------
// Author      : Rob F. / Entire Group
// Email       : RobusAsmoder@ukr.net
// Date        : 12 june 2001
// Description : Ports Unit With Extra Fast Transfering
//------------------------------------------------------------------------
UNIT RobPort;

INTERFACE
USES SysUtils, RobFIFO, Graphics, Windows, Forms;

CONST Com_BufMax=1024;
      Com_BufMaxMask=Com_BufMAX-1;
      Com_BufMin=Com_BufMAX SHR 1;
      Com_BufMinMask=Com_BufMAX-1;
      Com_FIFOBUFSIZE=32768;
      Com_ListMax=256;

VAR TimerWStart,TimerWNext:Int64;
    ComListString_SearchMode:BYTE=0;

CONST
  CDB_REPORT_BITS = 0;
  DB_REPORT_BYTES = 1;

      ERROR_SUCCESS                 = 0;
      ERROR_BADDB                   = 1;
      ERROR_BADKEY                  = 2;
      ERROR_CANTOPEN                = 3;
      ERROR_CANTREAD                = 4;
      ERROR_CANTWRITE               = 5;
      ERROR_OUTOFMEMORY             = 6;
      ERROR_INVALID_PARAMETER       = 7;
      ERROR_ACCESS_DENIED           = 8;

CONST INTE_RX    =$00000001;
      INTE_TX    =$00000002;
      INTE_CTS   =$00000010;  //C
      INTE_DSR   =$00000020;  //S
      INTE_RI    =$00000040;  //R
      INTE_DCD   =$00000080;  //D

      INTE_D_TX  =$00000002;

CONST ComCFG_SPD_115200  =$000F0100;
      ComCFG_SPD_57600   =$000F0200;
      ComCFG_SPD_38400   =$000F0300;
      ComCFG_SPD_28800   =$000F0400;
      ComCFG_SPD_19200   =$000F0600;
      ComCFG_SPD_14400   =$000F0800;
      ComCFG_SPD_12800   =$000F0900;
      ComCFG_SPD_11520   =$000F0A00;
      ComCFG_SPD_9600    =$000F0C00;
      ComCFG_SPD_7200    =$000F1000;
      ComCFG_SPD_6400    =$000F1200;
      ComCFG_SPD_4800    =$000F1800;
      ComCFG_SPD_2400    =$000F3000;
      ComCFG_SPD_1200    =$000F6000;
      ComCFG_SPD_600     =$000FC000;

      ComCFG_PAR_N       =$00000000;
      ComCFG_PAR_O       =$00100000;
      ComCFG_PAR_E       =$00200000;
      ComCFG_PAR_M       =$00300000;
      ComCFG_PAR_S       =$00400000;

      ComCFG_STB_1       =$00000000;
      ComCFG_STB_15      =$01000000;
      ComCFG_STB_2       =$02000000;

      ComCFG_BIT_8       =$00000000;
      ComCFG_BIT_7       =$10000000;
      ComCFG_BIT_6       =$20000000;
      ComCFG_BIT_5       =$30000000;

TYPE COM_PROCEDURE_TYPE=PROCEDURE(VAR ComPRT);
     COM_PROCEDURE_USAGE_PORTS_TYPE=PROCEDURE(S:ShortString);
     COM_PROCEDURE_SEND_BUF_EVENT_TYPE=PROCEDURE(VAR ComPRT);


TYPE COM_SEND_THREAD_TYPE=RECORD
      ThreadHND             :Integer;
      ThreadID              :LongWord;
      BUF                   :POINTER;
      EVNT                  :COM_PROCEDURE_SEND_BUF_EVENT_TYPE;
      SIZE                  :DWORD;
     END;

TYPE COM_TYPE=RECORD
      ComHND                :THandle;
      ComDCM                :TDCB;
      CommTimeouts          :TCommTimeouts;
      ThreadHND             :Integer;
      ThreadID              :LongWord;
      OpenedThreadHND       :Integer;
      OpenedThreadID        :LongWord;
      OpenedStatus          :BYTE;    //0-FREE 1-IN PROCESS 2-START 3-OK 4-ERROR
      BaudRate              :DWORD;
      PortNumber            :DWORD;
//      RdBuf                 :ARRAY [0..Com_BufMaxMask] OF BYTE;
//      RdBuf_WR              :DWORD;
//      RdBuf_RD              :DWORD;
      FIFOBUF               :tFIFO;
      WrBufPTR              :POINTER;
      WrBufSize             :DWORD;
      RdCounter             :DWORD;
      WrCounter             :DWORD;
      WrCounterFinish       :DWORD;
      Status                :DWORD;
      StatusOld             :DWORD;
      IntProc               :COM_PROCEDURE_TYPE;
      DbgProc               :COM_PROCEDURE_TYPE;
      DbgProcBefore         :COM_PROCEDURE_TYPE;
      IntEvent              :DWORD;
      IntEventDEBUG         :DWORD;
      Proc                  :PROCEDURE();
      ProcCounterEXEC       :DWORD;
      ThreadPriority        :LONGINT;
      Actived               :DWORD;
      PresentInSystem       :BOOLEAN;
      Blocked               :BOOLEAN;
      OverlappedWR          :TOverlapped;
      PinVal                :DWORD;
      ConstChar             :DWORD;
      SendMutex             :WORD;
      SendMutexCNT          :DWORD;
      SEND_THREAD           :COM_SEND_THREAD_TYPE;
     END;


VAR ComList:ARRAY[0..Com_ListMax-1] OF COM_TYPE;

function GetVersion(sFileName:String): ShortString;
PROCEDURE SetMainForm(VAR FormVal); STDCALL;
function Darker(Color:DWORD; Percent:Byte):DWORD; STDCALL;
function Lighter(Color:DWORD; Percent:Byte):DWORD; STDCALL;


FUNCTION WinToDos(St: STRING): STRING; STDCALL;
FUNCTION DosToWin(St: STRING): STRING; STDCALL;

PROCEDURE muSleep(muV:DOUBLE);
PROCEDURE muStartTimerDiscrete(muV:DOUBLE);
//FUNCTION muCheckTimerDiscreteStep():BOOLEAN;
PROCEDURE muWaitTimerDiscreteStep();
PROCEDURE muStartTimerWait(muV:DOUBLE); OVERLOAD;
FUNCTION muCheckTimerWait():BOOLEAN; OVERLOAD;
PROCEDURE muStartTimerWait(VAR PTst:Int64; muV:DOUBLE); OVERLOAD;
FUNCTION muCheckTimerWait(VAR PTst:Int64):BOOLEAN; OVERLOAD;
FUNCTION muGetTimerWait(VAR PTst:Int64; MULLER:DWORD=1):Int64;

FUNCTION ComGetUsagePorts(CheckOpen:BOOLEAN=FALSE; MultiThreadInterrup:COM_PROCEDURE_USAGE_PORTS_TYPE=NIL):ShortString;
PROCEDURE ComSendByte(PO:DWORD;B:BYTE);
PROCEDURE ComSendString(PO:DWORD;S:ShortString);
PROCEDURE ComSendBufferThreadEvent(VAR CO:COM_TYPE;VAR BUF;BuSI:DWORD; EVNT:COM_PROCEDURE_SEND_BUF_EVENT_TYPE); OVERLOAD;
PROCEDURE ComSendBufferThreadEvent(PO:DWORD;VAR BUF;BuSI:DWORD; EVNT:COM_PROCEDURE_SEND_BUF_EVENT_TYPE); OVERLOAD;
PROCEDURE ComSendBuffer(PO:DWORD;VAR BUF;BuSI:DWORD); STDCALL

PROCEDURE PutComPin(VAR ComPRT:COM_TYPE;VAL:BYTE); OVERLOAD;
PROCEDURE PutComPin(PO:DWORD;VAL:BYTE); OVERLOAD;
PROCEDURE PutComPinRTS(VAR ComPRT:COM_TYPE;VAL:BYTE); OVERLOAD;
PROCEDURE PutComPinRTS(PO:DWORD;VAL:BYTE); OVERLOAD;
PROCEDURE PutComPinDTR(VAR ComPRT:COM_TYPE;VAL:BYTE); OVERLOAD;
PROCEDURE PutComPinDTR(PO:DWORD;VAL:BYTE); OVERLOAD;
PROCEDURE PutComPinTXD(VAR ComPRT:COM_TYPE;VAL:BYTE); OVERLOAD;
PROCEDURE PutComPinTXD(PO:DWORD;VAL:BYTE); OVERLOAD;

FUNCTION ComSetSpeed(ComPort,ComConfig:DWORD):BOOLEAN; OVERLOAD;
FUNCTION ComSetSpeed(VAR ComPort:COM_TYPE;ComConfig:DWORD):BOOLEAN; OVERLOAD;
FUNCTION ComSetConfig(ComPort,ComConfig:DWORD):BOOLEAN;
FUNCTION ComIsUsed(VAR ComPRT:COM_TYPE):BOOLEAN; OVERLOAD;
FUNCTION ComIsUsed(PO:BYTE):BOOLEAN; OVERLOAD;
FUNCTION ComClose(ComPort:BYTE):BOOLEAN;
FUNCTION ComOpen(ComPort:BYTE; ComConfig:DWORD; OverlapedMode:BOOLEAN=FALSE):BOOLEAN;
FUNCTION ComDBOpen(PHComDB: PLongWord): Longint; stdcall;
FUNCTION ComDBClose(HComDB: LongWord): Longint; stdcall;
FUNCTION ComDBGetCurrentPortUsage(HCOMDB: LongWord;
  Buffer: Pointer; BufferSize: LongWord; ReportType: LongInt;
  MaxPortsReported: PLongWord): Longint; stdcall;
FUNCTION ComDBReleasePort(HComDB: LongWord; ComNumber:DWORD): Longint; stdcall;
FUNCTION ComDBClaimPort(HComDB: LongWord; ComNumber:DWORD; ForceClaim:BOOL; Forced:PBOOL): Longint; stdcall;
PROCEDURE ComExecProcedure(Po:DWORD;P:Pointer;Prior:LONGINT); OVERLOAD;
PROCEDURE ComExecProcedure(Po:DWORD;P:Pointer); OVERLOAD;
FUNCTION ComRdCount(VAR ComPRT:COM_TYPE):DWORD; OVERLOAD;
FUNCTION ComRdCount(PO:DWORD):DWORD; OVERLOAD;
FUNCTION ComRd(VAR ComPRT:COM_TYPE):BYTE; OVERLOAD;
FUNCTION ComRd(PO:DWORD):DWORD; OVERLOAD;
FUNCTION ComRdO(VAR ComPRT:COM_TYPE;OFS:DWORD):BYTE; OVERLOAD;
FUNCTION ComRdO(PO,OFS:DWORD):BYTE; OVERLOAD;
FUNCTION ComGetStatus(VAR ComPRT:COM_TYPE):BYTE; OVERLOAD;
FUNCTION ComGetStatus(PO:DWORD):BYTE; OVERLOAD;
FUNCTION ComWrBufFill(VAR ComPRT:COM_TYPE):DWORD; OVERLOAD;
FUNCTION ComWrBufFill(PO:DWORD):DWORD; OVERLOAD;
PROCEDURE ComRdFlush(VAR ComPRT:COM_TYPE); OVERLOAD;
PROCEDURE ComRdFlush(PO:DWORD); OVERLOAD;

IMPLEMENTATION

VAR
  TimeElapsed,TimeStart,TimeFrequency: Int64;             // Elapsed time between frames
  TimeFreq,TimeSEC,TimeMCS,TimeMS:DOUBLE;

FUNCTION ComDBOpen; external 'msports.dll' name 'ComDBOpen';
FUNCTION ComDBClose; external 'msports.dll' name 'ComDBClose';
FUNCTION ComDBGetCurrentPortUsage; external 'msports.dll' name 'ComDBGetCurrentPortUsage';
FUNCTION ComDBReleasePort; external 'msports.dll' name 'ComDBReleasePort';
FUNCTION ComDBClaimPort; external 'msports.dll' name 'ComDBClaimPort';


function GetVersion(sFileName:String): ShortString;
var
  VerInfoSize: DWORD;
  VerInfo: Pointer;
  VerValueSize: DWORD;
  VerValue: PVSFixedFileInfo;
  Dummy: DWORD;
begin
  VerInfoSize := GetFileVersionInfoSize(PChar(sFileName), Dummy);
  GetMem(VerInfo, VerInfoSize);
  GetFileVersionInfo(PChar(sFileName), 0, VerInfoSize, VerInfo);
  VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
  with VerValue^ do
  begin
    Result := IntToStr(dwFileVersionMS shr 16);
    Result := Result + '.' + IntToStr(dwFileVersionMS and $FFFF);
    Result := Result + '.' + IntToStr(dwFileVersionLS shr 16);
    Result := Result + '.' + IntToStr(dwFileVersionLS and $FFFF);
  end;
  FreeMem(VerInfo, VerInfoSize);
end;




PROCEDURE SetMainForm(VAR FormVal);
VAR P:POINTER;
BEGIN
  P:=@Application.MainForm;
  Pointer(P^) := @FormVal;
END;


function Darker(Color:DWORD; Percent:Byte):DWORD;
var
  r, g, b: Byte;
begin
  Color:=ColorToRGB(Color);
  r:=GetRValue(Color);
  g:=GetGValue(Color);
  b:=GetBValue(Color);
  r:=r-muldiv(r,Percent,100);  //процент% уменьшения яркости
  g:=g-muldiv(g,Percent,100);
  b:=b-muldiv(b,Percent,100);
  result:=RGB(r,g,b);
end;

function Lighter(Color:DWORD; Percent:Byte):DWORD;
var
  r, g, b: Byte;
begin
  Color:=ColorToRGB(Color);
  r:=GetRValue(Color);
  g:=GetGValue(Color);
  b:=GetBValue(Color);
  r:=r+muldiv(255-r,Percent,100); //процент% увеличения яркости
  g:=g+muldiv(255-g,Percent,100);
  b:=b+muldiv(255-b,Percent,100);
  result:=RGB(r,g,b);
end;





FUNCTION WinToDos(St: STRING): STRING;
VAR Ch: PChar;
BEGIN
  Ch := StrAlloc(Length(St) + 1);
  AnsiToOem(PChar(St), Ch);
  Result := Ch;
  StrDispose(Ch);
end;

FUNCTION DosToWin(St: STRING): STRING;
VAR Ch: PChar;
BEGIN
  Ch := StrAlloc(Length(St) + 1);
  OemToAnsi(PChar(St), Ch);
  Result := Ch;
  StrDispose(Ch);
END;


FUNCTION ComListString(MODE:BYTE):ShortString;
VAR N,M,I,O:LONGINT;
    ComDB:LongWord;
    Buffer: array of Byte;
    BufferSize: LongWord;
    C:CHAR;
VAR KeyHandle: HKEY;
    ErrCode, Index: Integer;
    ValueName, Data: String;
    ValueLen, DataLen, ValueType: DWORD;
CONST BufSize = $FFFF;
VAR
 Buf_DevList: ARRAY[0..BufSize] OF CHAR;
 DevName: PChar;

BEGIN
 RESULT:='';
 IF MODE=0 THEN BEGIN
  // MODE OLD
  ErrCode := RegOpenKeyEx(HKEY_LOCAL_MACHINE, 'HARDWARE\DEVICEMAP\SERIALCOMM', 0, KEY_READ, KeyHandle);
  IF ErrCode <> ERROR_SUCCESS THEN BEGIN
    IF ComDBOpen(@ComDB) = ERROR_SUCCESS THEN BEGIN
     ComDBGetCurrentPortUsage(ComDB, NIL, 0, DB_REPORT_BYTES, @BufferSize);
     SetLength(Buffer, BufferSize);
     ComDBGetCurrentPortUsage(ComDB, Buffer, BufferSize, DB_REPORT_BYTES, @BufferSize);
     IF BufferSize>Com_ListMax THEN BufferSize:=Com_ListMax;
     FOR N:=0 TO BufferSize-1 DO BEGIN
      IF Buffer[N]<>0 THEN BEGIN
//       ComList[N+1].PresentInSystem:=TRUE;
       RESULT:=RESULT+CHAR(N+1);
      END;
     END;
     ComDBClose(ComDB);
    END;
  END ELSE BEGIN
    Index := 0;
    REPEAT
      ValueLen := 256;
      DataLen := 256;
      SetLength(ValueName, ValueLen);
      SetLength(Data, DataLen);
      ErrCode := RegEnumValue(KeyHandle, Index, PChar(ValueName),
                              {$IFDEF VER120}
                              Cardinal(ValueLen),
                              {$ELSE}
                              ValueLen,
                              {$ENDIF}
                              nil, @ValueType, PByte(PChar(Data)), @DataLen);
      IF ErrCode = ERROR_SUCCESS THEN BEGIN
        SetLength(Data, DataLen);
        N:=1;
        WHILE N<LENGTH(DATA) DO BEGIN
         IF IsCharAlphaNumeric(DATA[N]) AND (NOT IsCharAlpha(DATA[N]))  THEN BEGIN
          INC(N);
         END ELSE BEGIN
          DELETE(DATA,N,1);
         END;
        END;
        RESULT:=RESULT+CHAR(StrToInt(DATA));
        Inc(Index);
      END ELSE BEGIN
        IF ErrCode <> ERROR_NO_MORE_ITEMS THEN BEGIN
        END;
      END;
    UNTIL (ErrCode <> ERROR_SUCCESS) ;
  END;
 END ELSE BEGIN
  // MODE NEW
  Win32check(QueryDosDevice(nil, Buf_DevList, BufSize) <> 0);
  DevName := @Buf_DevList;
  WHILE DevName^ <> #00 DO BEGIN
    IF (StrLIComp('COM', DevName, 3) = 0) THEN BEGIN
      //ListBox1.Items.Add(DevName);
      DATA:=DevName;
        N:=1;
        WHILE N<LENGTH(DATA) DO BEGIN
         IF IsCharAlphaNumeric(DATA[N]) AND (NOT IsCharAlpha(DATA[N]))  THEN BEGIN
          INC(N);
         END ELSE BEGIN
          DELETE(DATA,N,1);
         END;
        END;
        RESULT:=RESULT+CHAR(StrToInt(DATA));

    END;
    DevName := StrEnd(DevName)+1;
  END;

 END;
  //RESULT:=#0#1#1#$14#$15#$15;
  //SORT RESULT
  FOR N:=1 TO LENGTH(RESULT) DO BEGIN
   FOR M:=N TO LENGTH(RESULT) DO BEGIN
    IF RESULT[N]>RESULT[M] THEN BEGIN
     C:=RESULT[N];
     RESULT[N]:=RESULT[M];
     RESULT[M]:=C;
    END;
   END;
  END;
  //DELETE DUBLICATE
  N:=1;
  WHILE N<=(LENGTH(RESULT)-1) DO BEGIN
   WHILE (RESULT[N]=RESULT[N+1]) AND (N<LENGTH(RESULT)) DO DELETE(RESULT,N,1);
   INC(N);
  END;
END;





FUNCTION ComGetUsagePortsOneShot():ShortString;
VAR N,M,I,O:LONGINT;
    ComLiPB:ARRAY[0..Com_ListMax-1] OF RECORD
      PresentInSystem         :BOOLEAN;
      Blocked                 :BOOLEAN;
    END;
BEGIN
  FOR N:=0 TO Com_ListMax-1 DO BEGIN
   ComLiPB[N].PresentInSystem:=FALSE;
   ComLiPB[N].Blocked:=FALSE;
  END;

  RESULT:=ComListString(ComListString_SearchMode);

  N:=1;
  WHILE N<=LENGTH(RESULT) DO BEGIN
   ComLiPB[BYTE(RESULT[N])].PresentInSystem:=TRUE;
   INC(N);
  END;

  //Update ComList Status
  FOR N:=0 TO Com_ListMax-1 DO BEGIN
   ComList[N].PresentInSystem:=ComLiPB[N].PresentInSystem;
  END;
END;





PROCEDURE OpenedMultiPoll(VAR ComPRT:COM_TYPE); STDCALL;
VAR N,M,I:LONGINT;
BEGIN
    I:=ComPRT.PortNumber;
//    SetThreadPriority(ComList[I].OpenedThreadHND, THREAD_PRIORITY_HIGHEST);
    ComList[I].OpenedStatus:=2;
    IF ComOpen(ComPRT.PortNumber,9600) THEN BEGIN
     ComList[I].OpenedStatus:=3;
     ComClose(ComPRT.PortNumber);
    END ELSE BEGIN
     ComList[I].OpenedStatus:=4;
     ComPRT.Blocked:=TRUE;
    END;
    EndThread(0);
END;

PROCEDURE OpenedMultiStart(VAR ComPRT:COM_TYPE);
BEGIN
    ComList[ComPRT.PortNumber].OpenedStatus:=1;
    ComList[ComPRT.PortNumber].OpenedThreadHND:=BeginThread(NIL, 0, Addr(OpenedMultiPoll), Addr(ComList[ComPRT.PortNumber]), 0, ComList[ComPRT.PortNumber].OpenedThreadID);
//    WHILE ComList[ComPRT.PortNumber].OpenedStatus=1 DO SLEEP(1);
END;


VAR US_ThreadHND             :Integer=0;
    US_ThreadID              :LongWord=0;
    US_IntProc               :COM_PROCEDURE_USAGE_PORTS_TYPE;

PROCEDURE ComGetUsagePortsThreadPool(PROC_EXEC:POINTER);
VAR PORTS_STR,S:ShortString;
BEGIN
    PORTS_STR:='';
    REPEAT
     S:=ComGetUsagePortsOneShot();
     IF S<>PORTS_STR THEN BEGIN
      PORTS_STR:=S;
      US_IntProc(S);
     END;
     Sleep(500);
    UNTIL FALSE;
    EndThread(0);
END;



FUNCTION ComGetUsagePorts(CheckOpen:BOOLEAN=FALSE; MultiThreadInterrup:COM_PROCEDURE_USAGE_PORTS_TYPE=NIL):ShortString;
VAR N,M,I,O:LONGINT;
BEGIN

//ComList[ComPort].ThreadHND:=BeginThread(NIL,0,Addr(ComPollPort),Addr(ComList[ComPort]),0,ComList[ComPort].ThreadID);


  RESULT:=ComGetUsagePortsOneShot();
  //Check Opened Ports By Enother Process
  IF CheckOpen THEN BEGIN
   N:=1;
   FOR N:=1 TO LENGTH(RESULT) DO BEGIN
    I:=BYTE(RESULT[N]);
    OpenedMultiStart(ComList[I]);
   END;
   REPEAT
    M:=0;
    FOR N:=0 TO Com_ListMax-1 DO BEGIN
     I:=ComList[N].OpenedStatus;
     IF (I>=1) AND (I<=2) THEN M:=1;
    END;
    IF M=0 THEN BREAK;
    SLEEP(1);
   UNTIL FALSE;
   N:=1;
   WHILE N<=LENGTH(RESULT) DO BEGIN
    I:=BYTE(RESULT[N]);
    IF ComList[I].Blocked THEN BEGIN
     DELETE(RESULT,N,1);
    END ELSE BEGIN
     INC(N);
    END;
   END;
  END;

  IF (PDWord(@MultiThreadInterrup)<>NIL) AND ((US_ThreadHND OR US_ThreadID)=0) THEN BEGIN
   US_IntProc:=MultiThreadInterrup;
   US_ThreadHND:=BeginThread(NIL, 0, Addr(ComGetUsagePortsThreadPool), NIL, 0, US_ThreadID);
  END;

END;

FUNCTION ComWrBufFill(VAR ComPRT:COM_TYPE):DWORD; OVERLOAD;
BEGIN
      RESULT:=ComPRT.WrCounter - ComPRT.WrCounterFinish;
END;
FUNCTION ComWrBufFill(PO:DWORD):DWORD; OVERLOAD;
BEGIN
      RESULT:=ComList[PO].WrCounter - ComList[PO].WrCounterFinish;
END;


PROCEDURE ComRdFlush(VAR ComPRT:COM_TYPE); OVERLOAD;
BEGIN
    FIFO_FLUSH(ComPRT.FIFOBUF); //ComPRT.RdBuf_RD:=ComPRT.RdBuf_WR;
END;
PROCEDURE ComRdFlush(PO:DWORD); OVERLOAD;
BEGIN
    FIFO_FLUSH(ComList[PO].FIFOBUF); //ComList[PO].RdBuf_RD:=ComList[PO].RdBuf_WR;
END;

FUNCTION ComRdCount(VAR ComPRT:COM_TYPE):DWORD; OVERLOAD;
BEGIN
    RESULT:=FIFO_SIZE(ComPRT.FIFOBUF); //RESULT:=(ComPRT.RdBuf_WR-ComPRT.RdBuf_RD) AND Com_BufMaxMask;
END;
FUNCTION ComRdCount(PO:DWORD):DWORD; OVERLOAD;
BEGIN
    RESULT:=FIFO_SIZE(ComList[PO].FIFOBUF); //RESULT:=(ComList[PO].RdBuf_WR-ComList[PO].RdBuf_RD) AND Com_BufMaxMask;
END;


FUNCTION ComRd(VAR ComPRT:COM_TYPE):BYTE; OVERLOAD;
BEGIN
//      IF ComPRT.RdBuf_RD<>ComPRT.RdBuf_WR THEN BEGIN
//       RESULT:=ComPRT.RdBuf[ComPRT.RdBuf_RD];
//       ComPRT.RdBuf_RD:=(ComPRT.RdBuf_RD+1) AND Com_BufMaxMask;
//      END;
      IF FIFO_SIZE(ComPRT.FIFOBUF)<>0 THEN BEGIN
       RESULT:=FIFO_RD(ComPRT.FIFOBUF);
      END;
END;
FUNCTION ComRd(PO:DWORD):DWORD; OVERLOAD;
BEGIN
//      IF ComList[PO].RdBuf_RD<>ComList[PO].RdBuf_WR THEN BEGIN
//       RESULT:=ComList[PO].RdBuf[ComList[PO].RdBuf_RD];
//       ComList[PO].RdBuf_RD:=(ComList[PO].RdBuf_RD+1) AND Com_BufMaxMask;
//      END;
      IF FIFO_SIZE(ComList[PO].FIFOBUF)<>0 THEN BEGIN
       RESULT:=FIFO_RD(ComList[PO].FIFOBUF);
      END;
END;


FUNCTION ComRdO(VAR ComPRT:COM_TYPE; OFS:DWORD):BYTE; OVERLOAD;
BEGIN
//      RESULT:=ComPRT.RdBuf[(ComPRT.RdBuf_RD+OFS) AND Com_BufMaxMask];
      RESULT:=FIFO_RDOFS(ComPRT.FIFOBUF, OFS);
END;
FUNCTION ComRdO(PO, OFS:DWORD):BYTE; OVERLOAD;
BEGIN
//      RESULT:=ComList[PO].RdBuf[(ComList[PO].RdBuf_RD+OFS) AND Com_BufMaxMask];
      RESULT:=FIFO_RDOFS(ComList[PO].FIFOBUF, OFS);
END;



FUNCTION ComGetStatus(VAR ComPRT:COM_TYPE):BYTE; OVERLOAD;
BEGIN
    GetCommModemStatus(ComPRT.ComHND,ComPRT.Status);
    RESULT:=ComPRT.Status;
END;
FUNCTION ComGetStatus(PO:DWORD):BYTE; OVERLOAD;
BEGIN
    GetCommModemStatus(ComList[PO].ComHND,ComList[PO].Status);
    RESULT:=ComList[PO].Status;
END;



////////////////////////////////////////////////////////////////////////////////
//
//  Poll Mode
//
////////////////////////////////////////////////////////////////////////////////
PROCEDURE ComPollPort(VAR ComPRT:COM_TYPE);
VAR
  ByteReaded:DWORD;
  //RdBuf:ARRAY[0..Com_BufMinMask] OF BYTE;
  RdBufPTR:POINTER;
  RdBufSIZE:DWORD;
  N,M,I:LONGINT;
  signal:DWORD;
BEGIN


  ComPRT.Actived:=1;
  GetCommModemStatus(ComPRT.ComHND,ComPRT.Status);
  ComPRT.StatusOld:=NOT ComPRT.Status;
  ComPRT.RdCounter:=0;
  ComPRT.WrCounter:=0;
  ComPRT.WrCounterFinish:=0;

  ComPRT.OverlappedWR.hEvent:=CreateEvent(NIL, FALSE, FALSE, NIL);

  WHILE ComPRT.Actived=1 DO BEGIN

   GetCommModemStatus(ComPRT.ComHND,ComPRT.Status);
   ComPRT.IntEvent:=(ComPRT.StatusOld XOR ComPRT.Status) AND $0F0;
   ComPRT.StatusOld:=ComPRT.Status;

   signal := WaitForSingleObject(ComPRT.OverlappedWR.hEvent, 1);
   IF signal =  WAIT_OBJECT_0 THEN BEGIN
    IF GetOverlappedResult(ComPRT.ComHND, ComPRT.OverlappedWR, ByteReaded, true) THEN BEGIN
     IF ByteReaded<>0 THEN BEGIN
      INC(ComPRT.WrCounterFinish, ByteReaded);
      ComPRT.IntEvent:=ComPRT.IntEvent OR INTE_TX;
     END;
    END;
   END;
   RdBufSIZE:=FIFO_WR_GET_PTR_BLOCK_SIZE(ComPRT.FIFOBUF, RdBufPTR);
   IF ReadFile(ComPRT.ComHND, RdBufPTR^, RdBufSIZE, ByteReaded, NIL) THEN BEGIN
    IF ByteReaded<>0 THEN BEGIN
     INC(ComPRT.RdCounter,ByteReaded);
     FIFO_WR_ADD_SIZE(ComPRT.FIFOBUF, ByteReaded);
     ComPRT.IntEvent:=ComPRT.IntEvent OR INTE_RX;
    END;
   END;

//   INC (ComPRT.RdCounter);
   IF Dword(@ComPRT.Proc)<>0 THEN BEGIN
    ComPRT.Proc();
    ComPRT.Proc:=NIL;
    SetThreadPriority(ComPRT.ThreadHND,ComPRT.ThreadPriority);
   END;
   IF (Dword(@ComPRT.IntProc)<>0) AND (ComPRT.IntEvent<>0)  THEN BEGIN
    ComPRT.IntProc(ComPRT);
   END ELSE Sleep(1);


  END;
  CloseHandle(ComPRT.OverlappedWR.hEvent);
  ComPRT.Actived:=3;
  EndThread(0);
  EXIT;

//  GetCommModemStatus(ComHND,Com_PIN);

{
  GetCommModemStatus(ComHND,Com_PIN);
  If ReadFile(ComHND,MAS,SizeOf(MAS), ByteReaded, NIL  ) THEN  BEGIN
   IF ByteReaded<>0 THEN BEGIN
    Com_RD_Counter:=Com_RD_Counter+ByteReaded;
    Com_EVENT(MAS,ByteReaded);
//    ASM
//     PUSH ByteReaded
//     PUSH OFFSET MAS
//     CALL Com_EVENT
//    END;

   END;
  END;
}


//  WaitCommEvent(ComHND







{
//данные пришли
If ByteReaded>0 Then
Begin
//посчитаем общее количество прочитанных байтов
ReciveBytes:=ReciveBytes+ByteReaded;
//преобразуем массив в строку
Str:=String(MyBuff);
//отправим строку на просмотр
fmMain.Memo1.Text:=fmMain.Memo1.Text+ Str;
//покажем количество считанных байтов
fmMain.lbRecv.Caption:='recv: '+IntToStr(ReciveBytes)+' bytes...';
}



//    INC (COM_COUNTER);
END;





////////////////////////////////////////////////////////////////////////////////
//
//  OverlappedMode
//
////////////////////////////////////////////////////////////////////////////////
PROCEDURE ComPollPortOverlapped(VAR ComPRT:COM_TYPE);
VAR
  ByteReaded,ByteReadedCapacitor:DWORD;
  RdBuf:ARRAY[0..Com_BufMinMask] OF BYTE;
  N,M,I:LONGINT;
  signal:DWORD;
  FLAG,FSLEEP:BOOLEAN;
  state:DWORD;
  ByteReadedTotal:DWORD;
BEGIN


  ComPRT.Actived:=1;
  GetCommModemStatus(ComPRT.ComHND,ComPRT.Status);
  ComPRT.StatusOld:=NOT ComPRT.Status;
  ComPRT.RdCounter:=0;
  ComPRT.WrCounter:=0;
  ComPRT.WrCounterFinish:=0;


   // Создаем объект синхронизации
   ComPRT.OverlappedWR.hEvent:=CreateEvent(NIL, TRUE, FALSE, NIL);

              SetCommMask(ComPRT.ComHND, EV_RXCHAR   OR
                                         EV_RXFLAG   OR
                                         EV_TXEMPTY  OR
                                         EV_CTS      OR
                                         EV_DSR      OR
                                         EV_RLSD     OR
                                         EV_BREAK    OR
                                         EV_ERR      OR
                                         EV_RING     OR
                                         EV_PERR     OR
                                         EV_RX80FULL OR
                                         EV_EVENT1   OR
                                         EV_EVENT2   OR
                                           0 );
//              SetCommMask(ComPRT.ComHND, EV_RXCHAR);

  FLAG:=FALSE;
  FSLEEP:=FALSE;
  GetCommModemStatus(ComPRT.ComHND,ComPRT.Status);
  ComPRT.IntEvent:=INTE_CTS OR INTE_DSR OR INTE_RI OR INTE_DCD;
  ComPRT.StatusOld:=ComPRT.Status;// XOR $F0;
  WHILE ComPRT.Actived=1 DO BEGIN


   IF (NOT FLAG) AND (ComPRT.IntEvent=0) THEN BEGIN
//   GetCommModemStatus(ComPRT.ComHND,ComPRT.Status);
//   ComPRT.IntEvent:=(ComPRT.StatusOld XOR ComPRT.Status) AND $0F0;
//   ComPRT.StatusOld:=ComPRT.Status;

    // Связываем порт и объект синхронизации
    WaitCommEvent(ComPRT.ComHND, state, @ComPRT.OverlappedWR);
    // Начинаем ожидание данных
    signal := WaitForSingleObject(ComPRT.OverlappedWR.hEvent, 100);

    // Данные получены
    IF signal = WAIT_OBJECT_0 THEN BEGIN
     IF (state AND EV_RXCHAR)<>0 THEN BEGIN
      IF GetOverlappedResult(ComPRT.ComHND, ComPRT.OverlappedWR, ByteReadedTotal, FALSE) THEN BEGIN
       FLAG:=TRUE;
       ByteReadedCapacitor:=0;
      END ELSE BEGIN
       //ERROR !!!
      END;
     END;
     IF (state AND (EV_CTS OR EV_DSR OR EV_RING OR EV_EVENT1 OR EV_EVENT2))<>0 THEN BEGIN
      GetCommModemStatus(ComPRT.ComHND,ComPRT.Status);
      ComPRT.IntEvent:=(ComPRT.StatusOld XOR ComPRT.Status) AND $0F0;
      ComPRT.StatusOld:=ComPRT.Status;
     END;

    END;
   END;

   IF FLAG THEN BEGIN
    FSLEEP:=FALSE;
    // Начинаем чтение данных
    I:=Com_BufMax-8;
    N:=Com_BufMax-ComRdCount(ComPRT);
    IF N<I THEN I:=N;
    IF I<>0 THEN BEGIN
     FillChar(RdBuf, SizeOf(RdBuf), 0);
     ReadFile(ComPRT.ComHND, RdBuf, I, ByteReaded, @ComPRT.OverlappedWR);
      IF ByteReaded<>0 THEN BEGIN
       IF ComPRT.ConstChar<>$FFFFFFFF THEN BEGIN
        FillChar(RdBuf, ByteReaded, ComPRT.ConstChar);
       END;
       INC(ComPRT.RdCounter,ByteReaded);
       INC(ByteReadedCapacitor,ByteReaded);
       DEC(ByteReadedTotal,ByteReaded);
       FOR N:=0 TO ByteReaded-1 DO BEGIN
//        ComPRT.RdBuf[ComPRT.RdBuf_WR]:=RdBuf[N];
//        ComPRT.RdBuf_WR:=(ComPRT.RdBuf_WR+1) AND Com_BufMaxMask;
         FIFO_WR(ComPRT.FIFOBUF, RdBuf[N]);
       END;
       ComPRT.IntEvent:=ComPRT.IntEvent OR INTE_RX;
       IF ByteReaded<I THEN BEGIN
        FLAG:=TRUE;
       END ELSE BEGIN
        FLAG:=FALSE;
       END;
       FLAG:=FALSE;
      END ELSE BEGIN
       FLAG:=FALSE;
      END;
    END;
   END ELSE BEGIN
    FSLEEP:=TRUE;
   END;

{
//   I:=Com_BufMax-ComRdCount(ComPRT);
//   IF I>256 THEN BEGIN               //SizeOf(RdBuf)
//    DEC(I,128);
//    I:=32;
    I:=512;
    IF ReadFile(ComPRT.ComHND, RdBuf, I, ByteReaded, NIL) THEN BEGIN
     IF ByteReaded<>0 THEN BEGIN
      INC(ComPRT.RdCounter,ByteReaded);
      FOR N:=0 TO ByteReaded-1 DO BEGIN
       ComPRT.RdBuf[ComPRT.RdBuf_WR]:=RdBuf[N];
       ComPRT.RdBuf_WR:=(ComPRT.RdBuf_WR+1) AND Com_BufMaxMask;
      END;
      ComPRT.IntEvent:=ComPRT.IntEvent OR INTE_RX;
      FLAG:=TRUE;
     END;
    END;
//   END;
}
//   INC (ComPRT.RdCounter);
   IF Dword(@ComPRT.Proc)<>0 THEN BEGIN
    ComPRT.Proc();
    ComPRT.Proc:=NIL;
    SetThreadPriority(ComPRT.ThreadHND,ComPRT.ThreadPriority);
   END;
   IF (Dword(@ComPRT.IntProc)<>0) AND (ComPRT.IntEvent<>0)  THEN BEGIN
    ComPRT.IntProc(ComPRT);
    ComPRT.IntEvent:=0;
   END ELSE BEGIN
    IF FSLEEP THEN BEGIN
     Sleep(1);
    END;
   END;


  END;
  //CloseHandle(ComPRT.OverlappedWR.hEvent);
   CloseHandle(ComPRT.OverlappedWR.hEvent);
  ComPRT.Actived:=3;
  EndThread(0);
  EXIT;

//  GetCommModemStatus(ComHND,Com_PIN);

{
  GetCommModemStatus(ComHND,Com_PIN);
  If ReadFile(ComHND,MAS,SizeOf(MAS), ByteReaded, NIL  ) THEN  BEGIN
   IF ByteReaded<>0 THEN BEGIN
    Com_RD_Counter:=Com_RD_Counter+ByteReaded;
    Com_EVENT(MAS,ByteReaded);
//    ASM
//     PUSH ByteReaded
//     PUSH OFFSET MAS
//     CALL Com_EVENT
//    END;

   END;
  END;
}


//  WaitCommEvent(ComHND







{
//данные пришли
If ByteReaded>0 Then
Begin
//посчитаем общее количество прочитанных байтов
ReciveBytes:=ReciveBytes+ByteReaded;
//преобразуем массив в строку
Str:=String(MyBuff);
//отправим строку на просмотр
fmMain.Memo1.Text:=fmMain.Memo1.Text+ Str;
//покажем количество считанных байтов
fmMain.lbRecv.Caption:='recv: '+IntToStr(ReciveBytes)+' bytes...';
}



//    INC (COM_COUNTER);
END;



FUNCTION ComDecodeConfigSpeed(ComConfig:DWORD):DWORD;
VAR Com_Speed:DWORD;
BEGIN
    IF (ComConfig AND $000F0000)=$000F0000 THEN BEGIN
     Com_Speed:=ComConfig AND $0000FFFF;
     IF Com_Speed=0 THEN INC(Com_Speed);
     Com_Speed:=ROUND((115200*256)/Com_Speed);
    END ELSE Com_Speed:=ComConfig AND $000FFFFF;
    RESULT:=Com_Speed;
END;

FUNCTION ComSetSpeed(VAR ComPort:COM_TYPE;ComConfig:DWORD):BOOLEAN; OVERLOAD;
VAR Com_Speed:DWORD;
BEGIN
    Com_Speed:=ComDecodeConfigSpeed(ComConfig);
    GetCommState(ComPort.ComHND, ComPort.ComDCM);
    ComPort.ComDCM.BaudRate:=Com_Speed;
    RESULT:=SetCommState(ComPort.ComHND,ComPort.ComDCM);
END;

FUNCTION ComSetSpeed(ComPort,ComConfig:DWORD):BOOLEAN; OVERLOAD;
BEGIN
    ComSetSpeed(ComList[ComPort],ComConfig);
END;
//FUNCTION ComRdO(VAR ComPRT:COM_TYPE;OFS:DWORD):BYTE; OVERLOAD;


FUNCTION ComSetConfig(ComPort,ComConfig:DWORD):BOOLEAN;
VAR Com_Speed:DWORD;
    Com_Parity:DWORD;
    Com_Bit:DWORD;
    Com_Stop:DWORD;
BEGIN

    Com_Speed:=ComDecodeConfigSpeed(ComConfig);
    CASE (ComConfig AND $00F00000) OF
     ComCFG_PAR_N: Com_Parity:=NOPARITY;
     ComCFG_PAR_O: Com_Parity:=ODDPARITY;
     ComCFG_PAR_E: Com_Parity:=EVENPARITY;
     ComCFG_PAR_M: Com_Parity:=MARKPARITY;
     ComCFG_PAR_S: Com_Parity:=SPACEPARITY;
      ELSE Com_Parity:=NOPARITY;
    END;

    CASE (ComConfig AND $0F000000) OF
     ComCFG_STB_1 : Com_Stop:=ONESTOPBIT;
     ComCFG_STB_15: Com_Stop:=ONE5STOPBITS;
     ComCFG_STB_2 : Com_Stop:=TWOSTOPBITS;
     ELSE Com_Stop:=ONESTOPBIT;
    END;


    CASE (ComConfig AND $F0000000) OF
     ComCFG_BIT_8: Com_Bit:=8;
     ComCFG_BIT_7: Com_Bit:=7;
     ComCFG_BIT_6: Com_Bit:=6;
     ComCFG_BIT_5: Com_Bit:=5;
     ELSE Com_Bit:=8;
    END;

     FillChar (ComList[ComPort].ComDCM,SizeOf(ComList[ComPort].ComDCM),0);
     ComList[ComPort].ComDCM.DCBlength                         := SizeOf(TDCB);
     GetCommState(ComList[ComPort].ComHND, ComList[ComPort].ComDCM);
     ComList[ComPort].ComDCM.BaudRate                          := Com_Speed;
     ComList[ComPort].ComDCM.ByteSize                          := Com_Bit;
     ComList[ComPort].ComDCM.Parity                            := Com_Parity;

     ComList[ComPort].ComDCM.StopBits                          := Com_Stop;
     ComList[ComPort].ComDCM.XonChar                           := #0;
     ComList[ComPort].ComDCM.XoffChar                          := #255;
     ComList[ComPort].ComDCM.ErrorChar                         := #0;
     ComList[ComPort].ComDCM.XonLim                            := 128;
     ComList[ComPort].ComDCM.XoffLim                           := 128;
     ComList[ComPort].ComDCM.Flags                             := EV_RXCHAR OR EV_EVENT2;


     RESULT:=SetCommState(ComList[ComPort].ComHND,ComList[ComPort].ComDCM);

END;

FUNCTION ComOpen(ComPort:BYTE; ComConfig:DWORD; OverlapedMode:BOOLEAN=FALSE):BOOLEAN;
VAR S:STRING;
BEGIN


    Result:=FALSE;
    IF (ComList[ComPort].ComHND<>0) {OR (ComPort=0)} THEN EXIT;
    S:='\\.\COM'+IntToStr(ComPort);
//    S:='COM'+IntToStr(ComPort);
    IF OverlapedMode THEN BEGIN
     ComList[ComPort].ComHND:=CreateFile(PCHAR(S),
                                         GENERIC_READ OR GENERIC_WRITE,
                                         0,//FILE_SHARE_READ OR FILE_SHARE_WRITE,
                                         NIL,
                                         OPEN_EXISTING,
                                         FILE_FLAG_OVERLAPPED,// OR FILE_FLAG_NO_BUFFERING,//FILE_ATTRIBUTE_NORMAL,
                                         0);
    END ELSE BEGIN
     ComList[ComPort].ComHND:=CreateFile(PCHAR(S), GENERIC_READ OR GENERIC_WRITE,
                                         FILE_SHARE_READ OR FILE_SHARE_WRITE,
                                         NIL,
                                         OPEN_EXISTING,
                                         FILE_ATTRIBUTE_NORMAL,
                                         0);
    END;

    
    IF ComList[ComPort].ComHND = INVALID_HANDLE_VALUE THEN BEGIN
      ComList[ComPort].ComHND:=0;
      EXIT;
    END ELSE BEGIN
     SetCommMask(ComList[ComPort].ComHND,EV_RXCHAR   OR
                                         EV_RXFLAG   OR
                                         EV_TXEMPTY  OR
                                         EV_CTS      OR
                                         EV_DSR      OR
                                         EV_RLSD     OR
                                         EV_BREAK    OR
                                         EV_ERR      OR
                                         EV_RING     OR
                                         EV_PERR     OR
                                         EV_RX80FULL OR
                                         EV_EVENT1   OR
                                         EV_EVENT2   OR
                                           0 );
     
     SetupComm(ComList[ComPort].ComHND,1024,1024);

     FillChar (ComList[ComPort].CommTimeouts,SizeOf(COMMTIMEOUTS),0);
     ComList[ComPort].CommTimeouts.ReadIntervalTimeout         := $FFFFFFFF;
     ComList[ComPort].CommTimeouts.ReadTotalTimeoutMultiplier  := 0;
     ComList[ComPort].CommTimeouts.ReadTotalTimeoutConstant    := 0;
     ComList[ComPort].CommTimeouts.WriteTotalTimeoutMultiplier := 0;
     ComList[ComPort].CommTimeouts.WriteTotalTimeoutConstant   := 0;

     IF SetCommTimeOuts(ComList[ComPort].ComHND,ComList[ComPort].COMMTIMEOUTS)
//        AND SetCommState(ComList[ComPort].ComHND,ComList[ComPort].ComDCM)
        AND ComSetConfig(ComPort,ComConfig) THEN BEGIN
         Result:=TRUE;
//         ComList[ComPort].PortNumber:=ComPort;
         ComList[ComPort].Actived:=0;
//         CommThread:=TCommThread.Create(False);
         ComList[ComPort].Proc:=NIL;
         ComList[ComPort].IntProc:=NIL;
         ComList[ComPort].DbgProc:=NIL;
         ComList[ComPort].DbgProcBefore:=NIL;
         ComList[ComPort].PinVal:=0;
         ComList[ComPort].ConstChar:=$FFFFFFFF;
         FIFO_OPEN(ComList[ComPort].FIFOBUF, Com_FIFOBUFSIZE);
         IF OverlapedMode THEN BEGIN
          ComList[ComPort].ThreadHND:=BeginThread(NIL,0,Addr(ComPollPortOverlapped),Addr(ComList[ComPort]),0,ComList[ComPort].ThreadID);
         END ELSE BEGIN
          ComList[ComPort].ThreadHND:=BeginThread(NIL,0,Addr(ComPollPort),Addr(ComList[ComPort]),0,ComList[ComPort].ThreadID);
         END; 
         ComList[ComPort].ThreadPriority:=GetThreadPriority(ComList[ComPort].ThreadHND);
//         ComList[ComPort].ThreadPriority:=THREAD_PRIORITY_NORMAL;
//         ComList[ComPort].ThreadPriority:=THREAD_PRIORITY_LOWEST;
//         ComList[ComPort].ThreadPriority:=THREAD_PRIORITY_IDLE;
//         ComList[ComPort].ThreadPriority:=THREAD_PRIORITY_TIME_CRITICAL;
         ComList[ComPort].ThreadPriority:=THREAD_PRIORITY_HIGHEST;
         SetThreadPriority(ComList[ComPort].ThreadHND, ComList[ComPort].ThreadPriority);
         WHILE ComList[ComPort].Actived<>1 DO;
         EXIT;
     END;

     CloseHandle(ComList[ComPort].ComHND);
     ComList[ComPort].ComHND:=0;
    END;



END;

PROCEDURE ComDebugEventBefore(VAR ComPRT:COM_TYPE; VAR BUF; BuSI:DWORD);
BEGIN
//       EXIT;
//      IF (ComPRT.ComHND<>0) THEN BEGIN
       IF (Dword(@ComPRT.DbgProcBefore)<>0) THEN BEGIN
        ComPRT.WrBufPTR  := @BUF;
        ComPRT.WrBufSize := BuSI;
        INC(ComPRT.WrCounter,BuSI);
        ComPRT.IntEventDEBUG := INTE_D_TX;
        ComPRT.DbgProcBefore(ComPRT);
        ComPRT.IntEventDEBUG:=0;
       END;
//      END;
END;

PROCEDURE ComDebugEvent(VAR ComPRT:COM_TYPE; VAR BUF; BuSI:DWORD);
BEGIN
//       EXIT;
//      IF (ComPRT.ComHND<>0) THEN BEGIN
       IF (Dword(@ComPRT.DbgProc)<>0) THEN BEGIN
        ComPRT.WrBufPTR  := @BUF;
        ComPRT.WrBufSize := BuSI;
        INC(ComPRT.WrCounter,BuSI);
        ComPRT.IntEventDEBUG := INTE_D_TX;
        ComPRT.DbgProc(ComPRT);
        ComPRT.IntEventDEBUG:=0;
       END;
//      END;
END;

PROCEDURE ComSendStartWaitMutex(VAR CO:COM_TYPE);
BEGIN
    INC(CO.SendMutex);
    WHILE CO.SendMutex<>1 DO BEGIN
     INC(CO.SendMutexCNT);
     SLEEP(1);
    END;
END;

PROCEDURE ComSendEndWaitMutex(VAR CO:COM_TYPE);
BEGIN
    DEC(CO.SendMutex);
END;

PROCEDURE ComSendByte(PO:DWORD;B:BYTE);
VAR N,M,I:DWORD;
BEGIN
//      TransmitCommChar(ComList[PO].ComHND,CHAR(B));
      IF (ComList[PO].ComHND<>0) THEN BEGIN
       ComDebugEventBefore(ComList[PO],B,1);
       WriteFile(ComList[PO].ComHND,B,1,I, @ComList[PO].OverlappedWR);
       ComDebugEvent(ComList[PO],B,1);
      END;
END;

PROCEDURE ComSendString(PO:DWORD;S:ShortString);
VAR N,M,I:DWORD;
BEGIN
//      TransmitCommChar(ComList[PO].ComHND,CHAR(B));
      IF (ComList[PO].ComHND<>0) THEN BEGIN
       ComSendStartWaitMutex(ComList[PO]);
       ComDebugEventBefore(ComList[PO], S[1], Length(S));
       WriteFile(ComList[PO].ComHND, S[1], Length(S), I, @ComList[PO].OverlappedWR);
       ComDebugEvent(ComList[PO], S[1], Length(S));
       ComSendEndWaitMutex(ComList[PO]);
      END;
END;



PROCEDURE ComSendBufferThreadWorkThread(POPO:POINTER); STDCALL;
VAR CO:^COM_TYPE;
VAR N,M,I:DWORD;
BEGIN
       CO:=POINTER(POPO);
       ComSendStartWaitMutex(CO^);
       ComDebugEventBefore(CO^, CO.SEND_THREAD.BUF^, CO.SEND_THREAD.SIZE);
       WriteFile(CO.ComHND, CO.SEND_THREAD.BUF^, CO.SEND_THREAD.SIZE, I, @CO.OverlappedWR);
       ComDebugEvent(CO^, CO.SEND_THREAD.BUF^, CO.SEND_THREAD.SIZE);
       ComSendEndWaitMutex(CO^);
       IF ( DWORD(@CO.SEND_THREAD.EVNT)<>0 ) THEN BEGIN
        CO.SEND_THREAD.EVNT(CO);
       END;
       ExitThread(0);
END;

PROCEDURE ComSendBufferThreadEvent(VAR CO:COM_TYPE;VAR BUF;BuSI:DWORD; EVNT:COM_PROCEDURE_SEND_BUF_EVENT_TYPE); OVERLOAD;
//VAR P:POINTER;
BEGIN
//      P:=@CO;
      CO.SEND_THREAD.BUF := @BUF;
      CO.SEND_THREAD.SIZE := BuSI;
      CO.SEND_THREAD.EVNT := EVNT;
      CO.SEND_THREAD.ThreadHND := CreateThread(NIL,0,@ComSendBufferThreadWorkThread, @CO, 0, CO.SEND_THREAD.ThreadID);
        //BeginThread(NIL,0,@ComSendBufferThreadWorkThread, POINTER($1234), 0, CO.SEND_THREAD.ThreadID);
END;

PROCEDURE ComSendBufferThreadEvent(PO:DWORD;VAR BUF;BuSI:DWORD; EVNT:COM_PROCEDURE_SEND_BUF_EVENT_TYPE); OVERLOAD;
BEGIN
      ComSendBufferThreadEvent(ComList[PO], BUF, BuSI, EVNT);
END;

PROCEDURE ComSendBuffer(PO:DWORD;VAR BUF;BuSI:DWORD); STDCALL;
VAR N,M,I:DWORD;
//    P:POINTER;
BEGIN
      IF (ComList[PO].ComHND<>0) THEN BEGIN
       ComSendStartWaitMutex(ComList[PO]);
       ComDebugEventBefore(ComList[PO], BUF, BuSI);
       WriteFile(ComList[PO].ComHND, BUF, BuSI, I, @ComList[PO].OverlappedWR);
       ComDebugEvent(ComList[PO], BUF, BuSI);
       ComSendEndWaitMutex(ComList[PO]);
      END;
END;

PROCEDURE PutComPin(VAR ComPRT:COM_TYPE;VAL:BYTE); OVERLOAD;
VAR I:DWORD;
BEGIN
      IF ComPRT.ComHND<>0 THEN BEGIN
       I:=$0000;
       CASE VAL OF
        $00:BEGIN I:=$01; EscapeCommFunction(ComPRT.ComHND,SETRTS);    END;
        $01:BEGIN I:=$10; EscapeCommFunction(ComPRT.ComHND,CLRRTS);    END;
        $02:PutComPin(ComPRT,$00 OR ((ComPRT.PinVal SHR 0) AND 1) XOR 1);
        $10:BEGIN I:=$02; EscapeCommFunction(ComPRT.ComHND,SETDTR);    END;
        $11:BEGIN I:=$20; EscapeCommFunction(ComPRT.ComHND,CLRDTR);    END;
        $12:PutComPin(ComPRT,$10 OR ((ComPRT.PinVal SHR 1) AND 1) XOR 1);
        $20:BEGIN I:=$04; EscapeCommFunction(ComPRT.ComHND,SETBREAK);  END;
        $21:BEGIN I:=$40; EscapeCommFunction(ComPRT.ComHND,CLRBREAK);  END;
        $22:PutComPin(ComPRT,$20 OR ((ComPRT.PinVal SHR 2) AND 1) XOR 1);
       END;
       ComPRT.PinVal:=ComPRT.PinVal AND (I XOR $FF);
       ComPRT.PinVal:=ComPRT.PinVal OR  (I SHR 4);
      END;
END;
PROCEDURE PutComPin(PO:DWORD;VAL:BYTE); OVERLOAD;
BEGIN
      PutComPin(ComList[PO],VAL);
END;


PROCEDURE PutComPinRTS(VAR ComPRT:COM_TYPE;VAL:BYTE); OVERLOAD;
BEGIN
      PutComPin(ComPRT,     $00 OR VAL);
END;
PROCEDURE PutComPinRTS(PO:DWORD;VAL:BYTE); OVERLOAD;
BEGIN
      PutComPin(ComList[PO],$00 OR VAL);
END;

PROCEDURE PutComPinDTR(VAR ComPRT:COM_TYPE;VAL:BYTE); OVERLOAD;
BEGIN
      PutComPin(ComPRT,     $10 OR VAL);
END;
PROCEDURE PutComPinDTR(PO:DWORD;VAL:BYTE); OVERLOAD;
BEGIN
      PutComPin(ComList[PO],$10 OR VAL);
END;

PROCEDURE PutComPinTXD(VAR ComPRT:COM_TYPE;VAL:BYTE); OVERLOAD;
BEGIN
      PutComPin(ComPRT,     $20 OR VAL);
END;
PROCEDURE PutComPinTXD(PO:DWORD;VAL:BYTE); OVERLOAD;
BEGIN
      PutComPin(ComList[PO],$20 OR VAL);
END;

FUNCTION ComIsUsed(VAR ComPRT:COM_TYPE):BOOLEAN; OVERLOAD;
BEGIN
      RESULT:=ComPRT.ComHND<>0;
END;
FUNCTION ComIsUsed(PO:BYTE):BOOLEAN; OVERLOAD;
BEGIN
      RESULT:=ComIsUsed(ComList[PO]);
END;

FUNCTION ComClose(ComPort:BYTE):BOOLEAN;
BEGIN

     Result:=ComList[ComPort].ComHND<>0;
     IF Result THEN BEGIN
      ComList[ComPort].Actived:=2;
      WHILE ComList[ComPort].Actived=2 DO Sleep(1);
      CloseHandle(ComList[ComPort].ComHND);
//      FillChar(ComList[ComPort],SizeOf(ComList[ComPort]),0);
//      Sleep(100);
      FIFO_CLOSE(ComList[ComPort].FIFOBUF);
     END;
     ComList[ComPort].ComHND:=0;

END;


PROCEDURE muSleep(muV:DOUBLE);
VAR TimeCU,TimeEN:Int64;
BEGIN
      QueryPerformanceCounter(TimeCU);
      TimeEN:=ROUND(TimeCU+muV*TimeMCS);
      WHILE TimeCU<TimeEN DO QueryPerformanceCounter(TimeCU);
END;


PROCEDURE muStartTimerWait(muV:DOUBLE); OVERLOAD;
BEGIN
      QueryPerformanceCounter(TimerWStart);
      TimerWNext:=ROUND(TimerWStart+muV*TimeMCS);
END;

FUNCTION muCheckTimerWait():BOOLEAN; OVERLOAD;
VAR TimeCU:Int64;
BEGIN
      QueryPerformanceCounter(TimeCU);
      RESULT:=TimeCU>TimerWNext;
END;


PROCEDURE muStartTimerWait(VAR PTst:Int64; muV:DOUBLE); OVERLOAD;
BEGIN
      QueryPerformanceCounter(TimerWStart);
      PTst:=ROUND(TimerWStart+muV*TimeMCS);
END;

FUNCTION muCheckTimerWait(VAR PTst:Int64):BOOLEAN; OVERLOAD;
VAR TimeCU:Int64;
BEGIN
      QueryPerformanceCounter(TimeCU);
      RESULT:=TimeCU>PTst;
END;

FUNCTION muGetTimerWait(VAR PTst:Int64; MULLER:DWORD=1):Int64;
VAR TimeCU:Int64;
BEGIN
      QueryPerformanceCounter(TimeCU);
      RESULT:=ROUND(((PTst-TimeCU)*MULLER)/TimeMCS);
END;



VAR TimerDiscreteStart,TimerDiscreteNext:Int64;
    TimerDiscreteCount:DWORD;
    TimerDiscreteStep:DOUBLE;

PROCEDURE muStartTimerDiscrete(muV:DOUBLE);
BEGIN
      QueryPerformanceCounter(TimerDiscreteStart);
      TimerDiscreteCount:=0;
      TimerDiscreteStep:=muV*TimeMCS;
      TimerDiscreteNext:=TimerDiscreteStart;
END;
{
FUNCTION muCheckTimerDiscreteStep():BOOLEAN;
VAR TimeCU:Int64;
BEGIN
      QueryPerformanceCounter(TimeCU);
      RESULT:=TimeCU<TimerDiscreteNext;
END;
}

PROCEDURE muWaitTimerDiscreteStep();
VAR TimeCU:Int64;
BEGIN
      REPEAT
       QueryPerformanceCounter(TimeCU);
      UNTIL NOT (TimeCU<TimerDiscreteNext);
//      WHILE NOT muCheckTimerWait() DO;

      INC(TimerDiscreteCount);
      TimerDiscreteNext:=ROUND(TimerDiscreteStart+TimerDiscreteStep*TimerDiscreteCount);
END;

PROCEDURE ComExecProcedure(Po:DWORD;P:Pointer;Prior:LONGINT); OVERLOAD;
VAR ThreadHND:Integer;
    ThreadID:LongWord;
    N,M,I,O:LONGINT;
BEGIN

    IF Dword(@ComList[Po].Proc)=0 THEN BEGIN
     SetThreadPriority(ComList[Po].ThreadHND,Prior);
//     SetThreadPriorityBoost(ComList[Po].ThreadHND,TRUE);
     INC(ComList[Po].ProcCounterEXEC);
     ComList[Po].Proc:=P;
     WHILE Dword(@ComList[Po].Proc)<>0 DO;
    END;

//    ThreadHND:=BeginThread(NIL,0,P,0,0,ThreadID);
//    SetThreadPriority(ThreadHND,THREAD_PRIORITY_TIME_CRITICAL);

//      I:=GetCurrentProcessId();
//      I:=GetCurrentProcess();
//      SetProcessPriorityBoost(I,FALSE);
//      SetPriorityClass(I,REALTIME_PRIORITY_CLASS);


//      EndThread(0);
END;

PROCEDURE ComExecProcedure(Po:DWORD;P:Pointer); OVERLOAD;
BEGIN
      ComExecProcedure(Po,P,ComList[Po].ThreadPriority);
END;


VAR N,M,I:LONGINT;

BEGIN

  QueryPerformanceFrequency( TimeFrequency );
  TimeFreq:=1/TimeFrequency;
  TimeSEC:=TimeFrequency;
  TimeMS:=TimeFrequency/1000;
  TimeMCS:=TimeFrequency/1000000;
  QueryPerformanceCounter(TimeStart);


//      BeginThread(ThreadID
//    BeginThread(NIL,0,Addr(PollPort),Addr(ComList[0]),0,ThreadID);
//    BeginThread(NIL,0,Addr(PollPort),Addr(ComList[2]),0,ThreadID);
     FillChar(ComList,SizeOf(ComList),0);
     FOR N:=1 TO Com_ListMax-1 DO BEGIN
      ComList[N].PortNumber:=N;
     END;
//     ComList[ComPort].PortNumber:=ComPort;
     ComGetUsagePorts(FALSE);
END.
