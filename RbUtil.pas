////////////////////////////////////////////////////////////////////////////
//
// Unit For MultiThread using
//
// Written By Rob F. / Entire Group ... Speccy Forever
//
////////////////////////////////////////////////////////////////////////////



UNIT RbUtil;



INTERFACE
USES Registry, Windows, SysUtils, Graphics;

VAR StrSpaceRob:CHAR=' ';
VAR RegSTR:ShortString='\Software\Entire Group\Mason';

FUNCTION RegCheck(S:ShortString=''):BOOLEAN;
PROCEDURE RegWrite(NAME,VALUE:ShortString);
FUNCTION RegRead(NAME:ShortString):ShortString;
FUNCTION RegGetDrivePATH(S:ShortString=''):ShortString;

FUNCTION GetTikInTime(TIK:DWORD):ShortString; STDCALL;
FUNCTION IntToStrRob(Num:LONGINT;SIZ:LONGINT=0;CUR_SPACE:CHAR=#0):ShortString; STDCALL;
FUNCTION IntToHexRob(Num:LONGINT;SIZ:LONGINT=0):ShortString; STDCALL;
FUNCTION HexB(Num:LONGINT):ShortString; STDCALL;
FUNCTION HexW(Num:LONGINT):ShortString; STDCALL;
FUNCTION HexD(Num:LONGINT):ShortString; STDCALL;
FUNCTION CopyBinToString(VAR _FR;SIZ:DWORD):ShortString; STDCALL;
FUNCTION CopyDWordToString(_FR:DWORD):ShortString; STDCALL;
FUNCTION CopyWordToString(_FR:WORD):ShortString; STDCALL;
PROCEDURE SeparateShortString(VAR S1,S2:ShortString; CharSep:CHAR); STDCALL;
PROCEDURE StrToChar (S:ShortString;VAR CH:ARRAY OF CHAR;SIZ:LONGINT);
FUNCTION CharToStr(CPTR:PChar;CSIZ:LONGINT):ShortString;
FUNCTION DelFreeSpace(S:ShortString):ShortString;

PROCEDURE SwapMemPTR(VAR P0; VAR P1; SIZ:DWORD); ASSEMBLER; STDCALL;

FUNCTION Mid(S:ShortString; OFS,SIZ:LONGINT):ShortString;
FUNCTION Right(S:ShortString; SIZ:LONGINT):ShortString;
FUNCTION DeleteSpace(S:STRING):STRING;
FUNCTION GetEle(S, SF:STRING; ElN:DWORD; STRSKIP:BOOLEAN=FALSE):ShortString;
FUNCTION GetEleS(S, SF:STRING; ElN:DWORD):ShortString;
FUNCTION HexToInt(Str : ShortString): integer;
FUNCTION DecToInt(Str : ShortString): integer;

FUNCTION BinStrToHex(BINDATAS:ShortString):ShortString;
FUNCTION HexToBinStr(S:ShortString):ShortString;

FUNCTION ColorDarker(Color: TColor; Percent: Byte): TColor;
FUNCTION ColorLighter(Color: TColor; Percent: Byte): TColor;

FUNCTION DetBin(S:STRING):BOOLEAN;
FUNCTION DetHex(S:STRING):BOOLEAN;
FUNCTION DetVal(S:STRING):BOOLEAN;
FUNCTION DetValS(S:STRING):BOOLEAN;
FUNCTION DetVar(S:STRING):BOOLEAN;
FUNCTION DetVarS(S:STRING):BOOLEAN;
FUNCTION GetVar(S:STRING):LONGINT;
FUNCTION ValAutoConvertSTRSTR(S:ShortString; MINSI:DWORD=1):ShortString;

FUNCTION GetStrB(VAR S:ShortString; OFS:DWORD=1):BYTE;
FUNCTION GetStrW(VAR S:ShortString; OFS:DWORD=1):WORD;
FUNCTION GetStrD(VAR S:ShortString; OFS:DWORD=1):DWORD;

FUNCTION CutStrB(VAR S:ShortString):BYTE;
FUNCTION CutStrW(VAR S:ShortString):WORD;
FUNCTION CutStrD(VAR S:ShortString):DWORD;

FUNCTION CHRISU(S,SCH:ShortString):BOOLEAN;

FUNCTION WinToDosR(b:CHAR):CHAR;
FUNCTION DosToWinR(b:CHAR):CHAR;

FUNCTION AlignD(D,AL:DWORD):DWORD;
FUNCTION AlignVarD(VAR D:DWORD; AL:DWORD):DWORD;

IMPLEMENTATION


FUNCTION GetTikInTime(TIK:DWORD):ShortString;  STDCALL;
VAR N,M,I:LONGINT;
    HOU:DWORD;
    MIN:DWORD;
    SEC:DWORD;
    C,CC:CHAR;
BEGIN
      CC:=' ';
      IF ((TIK SHR 4) AND 1)=0 THEN BEGIN
       CC:=':';
      END;
      N:=50*60*60;  HOU:=TIK DIV N; DEC(TIK,HOU*N);
      N:=50*60;     MIN:=TIK DIV N; DEC(TIK,MIN*N);
      N:=50;        SEC:=TIK DIV N; DEC(TIK,SEC*N);
      RESULT:=IntToStrRob(HOU,0,'0')+CC+IntToStrRob(MIN,2,'0')+CC+IntToStrRob(SEC,2,'0')+'.'+IntToStrRob(ROUND(TIK/49*99),2,'0');
END;



FUNCTION IntToStrRob(Num:LONGINT;SIZ:LONGINT=0;CUR_SPACE:CHAR=#0):ShortString; STDCALL;
ASM
        PUSHAD
        MOV AL,[StrSpaceRob]
        SHL EAX,16
        MOV EDI,@Result
        INC EDI

        MOV AL,CUR_SPACE
        OR AX,AX
        JZ @M10
        SHL EAX,16
@M10:   MOV EBX,Num
        MOV ESI,SIZ
        PUSH EBP

        MOV EBP,EBX
        AND EBP,80000000H
        JZ @M9
        NEG EBX
        OR ESI,ESI
        JZ @M9
        DEC ESI
@M9:    XOR ECX,ECX
        OR ESI,ESI
        JZ @M6
        MOV CL,10
        SUB ECX,ESI
        JNC @M6
        NEG ECX
        ROR EAX,8
        MOV AL,AH
        ROL EAX,8
        MOV AL,AH
        MOV AH,CL
        XOR ECX,ECX
@M8:    CALL @STOSB
        DEC AH
        JNZ @M8

//        MOV CH,AH
@M6:    XOR AX,AX

        MOV EDX,1000000000
        CALL @M1
        MOV EDX,100000000
        CALL @M1
        MOV EDX,10000000
        CALL @M1
        MOV EDX,1000000
        CALL @M1
        MOV EDX,100000
        CALL @M1
        MOV EDX,10000
        CALL @M1
        MOV  DX,1000
        CALL @M1
        MOV  DX,100
        CALL @M1
        MOV  DL,10
        CALL @M1
        MOV AL,BL
        CALL @M4
        POP EBP
        MOV EDI,@Result
        MOV [EDI],CH
        POPAD
        JMP @EXIT

@M1:
        XOR AL,AL
        SUB EBX,EDX
        JC @M2
        INC AL
        SUB EBX,EDX
        JC @M2
        INC AL
        SUB EBX,EDX
        JC @M2
        INC AL
        SUB EBX,EDX
        JC @M2
        INC AL
        SUB EBX,EDX
        JC @M2
        INC AL
        SUB EBX,EDX
        JC @M2
        INC AL
        SUB EBX,EDX
        JC @M2
        INC AL
        SUB EBX,EDX
        JC @M2
        INC AL
        SUB EBX,EDX
        JC @M2
        INC AL
        SUB EBX,EDX
@M2:    ADD EBX,EDX
        OR AH,AL
        JZ @M3
        OR CL,CL
        JNZ @M5
@M4:    ADD AL,'0'
        CALL @STOSB
//        INC CH
        RET 0
@M5:    DEC CL
        RET 0
@M3:    OR CL,CL
        JNZ @M5
        OR ESI,ESI
        JZ @M7
        ROR EAX,16
        CALL @STOSB
        ROL EAX,16
//        INC CH
@M7:    RET 0

@STOSB: OR EBP,EBP
        JNZ @STOSB1
        STOSB
        INC CH
        RET 0
@STOSB1:PUSH AX
        MOV AH,AL
        MOV AL,'-'
        STOSW
        POP AX
        INC CH
        INC CH
        XOR EBP,EBP
        RET 0

@EXIT:

END;

FUNCTION IntToHexRob(Num:LONGINT;SIZ:LONGINT=0):ShortString; STDCALL;
ASM
        PUSHAD

        MOV EDI,@Result
        MOV EDX,Num
        MOV ECX,SIZ
        MOV EBX,OFFSET @HEXTBL
        JECXZ @M2
//SIZE ZERRO
@M1:    MOV AL,CL
        STOSB
        MOV AL,'0'
        PUSH ECX
        REP STOSB
        POP ECX
@M3:    OR EDX,EDX
        JZ @EXIT
//        MOV BL,DL
//        AND BL,0FH
//        MOV AL,BYTE PTR [@HEXTBL+EBX]
        MOV AL,DL
        XLATB
        DEC EDI
        MOV [EDI],AL
        SHR EDX,4
        DEC ECX
        JZ @EXIT
//        MOV BL,DL
//        AND BL,0FH
//        MOV AL,BYTE PTR [@HEXTBL+EBX]
        MOV AL,DL
        XLATB
        DEC EDI
        MOV [EDI],AL
        SHR EDX,4
        LOOP @M3
        JMP @EXIT
@M2:    MOV ECX,8
        TEST EDX,0FF000000H
        JNZ @M1
        DEC ECX
        DEC ECX
        TEST EDX,000FF0000H
        JNZ @M1
        DEC ECX
        DEC ECX
        TEST EDX,00000FF00H
        JNZ @M1
        DEC ECX
        DEC ECX
        JMP @M1
@HEXTBL:DB '0123456789ABCDEF'
        DB '0123456789ABCDEF'
        DB '0123456789ABCDEF'
        DB '0123456789ABCDEF'
        DB '0123456789ABCDEF'
        DB '0123456789ABCDEF'
        DB '0123456789ABCDEF'
        DB '0123456789ABCDEF'
        DB '0123456789ABCDEF'
        DB '0123456789ABCDEF'
        DB '0123456789ABCDEF'
        DB '0123456789ABCDEF'
        DB '0123456789ABCDEF'
        DB '0123456789ABCDEF'
        DB '0123456789ABCDEF'
        DB '0123456789ABCDEF'
@EXIT:  POPAD

END;



FUNCTION HexB(Num:LONGINT):ShortString; STDCALL;
BEGIN
        RESULT:=IntToHexRob(Num,2);
END;
FUNCTION HexW(Num:LONGINT):ShortString; STDCALL;
BEGIN
        RESULT:=IntToHexRob(Num,4);
END;
FUNCTION HexD(Num:LONGINT):ShortString; STDCALL;
BEGIN
        RESULT:=IntToHexRob(Num,8);
END;



FUNCTION CopyBinToString(VAR _FR;SIZ:DWORD):ShortString; STDCALL;
BEGIN
    SetLength(RESULT,SIZ);
    MOVE (_FR,RESULT[1],SIZ);
END;

FUNCTION CopyDWordToString(_FR:DWORD):ShortString; STDCALL;
BEGIN
    RESULT:=CopyBinToString(_FR,4);
END;
FUNCTION CopyWordToString(_FR:WORD):ShortString; STDCALL;
BEGIN
    RESULT:=CopyBinToString(_FR,2);
END;


PROCEDURE SeparateShortString(VAR S1,S2:ShortString; CharSep:CHAR); STDCALL;
VAR N,M,I:LONGINT;
    B:BYTE;
BEGIN
    N:=1;
    WHILE (N<Length(S1)) AND (S1[N]<>CharSep) DO INC(N);
    B:=Length(S1)-N;
    S2[0]:=CHAR(B);
    Move(S1[N+1],S2[1],B);
    S1[0]:=CHAR(N-1);
END;



PROCEDURE StrToChar (S:ShortString;VAR CH:ARRAY OF CHAR;SIZ:LONGINT);
BEGIN
        FillChar(CH,SIZ,0);
        Move(S[1],CH,LENGTH(S));
END;

FUNCTION CharToStr(CPTR:PChar;CSIZ:LONGINT):ShortString;
BEGIN
        SetLength(Result,CSIZ);
        MOVE (CPTR^,Result[1],CSIZ);
END;

FUNCTION DelFreeSpace(S:ShortString):ShortString;
BEGIN
        WHILE (S[Length(S)]=' ') AND (Length(S)>0) DO Delete(S,Length(S),1);
        Result:=S;
END;



FUNCTION RegCheck(S:ShortString=''):BOOLEAN;
VAR Reg:TRegistry;
BEGIN
     Reg := TRegistry.Create;
     Reg.RootKey := HKEY_LOCAL_MACHINE;
     Reg.Access := KEY_WRITE;
     RESULT := Reg.KeyExists(RegSTR);
     IF RESULT THEN BEGIN
      IF S<>'' THEN BEGIN
       RESULT := Reg.ValueExists(S);
      END;
     END;

     Reg.Free;
END;

PROCEDURE RegWrite(NAME,VALUE:ShortString);
VAR Reg:TRegistry;
BEGIN
     Reg := TRegistry.Create;
     Reg.RootKey := HKEY_LOCAL_MACHINE;
     Reg.Access := KEY_WRITE;
     IF Reg.OpenKey(RegSTR, TRUE) THEN BEGIN
      Reg.WriteString(NAME,VALUE);
      Reg.CloseKey;
     END;
     Reg.Free;
END;

FUNCTION RegRead(NAME:ShortString):ShortString;
VAR Reg:TRegistry;
BEGIN
     Reg := TRegistry.Create;
     Reg.RootKey := HKEY_LOCAL_MACHINE;
     Reg.Access := KEY_WRITE;
     RESULT:='';
     IF Reg.OpenKeyReadOnly(RegSTR) THEN BEGIN
      RESULT:=Reg.ReadString(NAME);
      Reg.CloseKey;
     END;
     Reg.Free;
END;

FUNCTION RegGetDrivePATH(S:ShortString=''):ShortString;
VAR N,M,I:LONGINT;
    D:CHAR;
    S1:ShortString;
LABEL GOTOEXIT;
BEGIN
      IF S[1]='+' THEN BEGIN
       DELETE(S,1,1);
       GOTO GOTOEXIT;
      END;

      IF (S='') OR (S[2]<>':') THEN BEGIN
       S:=GetCurrentDir();
      END;
      D:=UpCase(S[1]);
      S1:=RegRead('DRIVE'+D);
      IF S1='' THEN BEGIN
       GetDir(BYTE(D)-BYTE('A')+1,S1);
       IF S1[1]<>D THEN S1:=S;
      END;
      S:=S1;
      IF S[Length(S)]<>'\' THEN S:=S+'\';
      IF DirectoryExists(S) THEN GOTO GOTOEXIT;
      WHILE (NOT DirectoryExists(S)) AND (LENGTH(S)>3) DO BEGIN
       REPEAT
        DELETE(S,Length(S),1);
       UNTIL S[Length(S)]='\';
      END;
GOTOEXIT:
      RegWrite('DRIVE'+S[1],S);
      RESULT:=S;

END;

PROCEDURE SwapMemPTR(VAR P0; VAR P1; SIZ:DWORD); ASSEMBLER; STDCALL;
ASM
    PUSH EDX
    PUSH EBX
    PUSH ECX
    MOV ECX,DWORD PTR [SIZ]
    JECXZ @M2
    MOV EBX,DWORD PTR [P0]
    MOV EDX,DWORD PTR [P1]
@M1:XCHG CL,BYTE PTR [EBX]
    XCHG CL,BYTE PTR [EDX]
    XCHG CL,BYTE PTR [EBX]
    INC EBX
    INC EDX
    LOOP @M1
@M2:POP ECX
    POP EBX
    POP EDX
END;


FUNCTION Mid(S:ShortString; OFS,SIZ:LONGINT):ShortString;
BEGIN
      RESULT:=COPY(S,OFS,SIZ);
END;

FUNCTION Right(S:ShortString; SIZ:LONGINT):ShortString;
BEGIN
      RESULT:=COPY(S,LENGTH(S)-SIZ+1,SIZ);
END;

FUNCTION OneSkipSizeELE(VAR SF,S:STRING; N:DWORD; STRSKIP:BOOLEAN=FALSE):DWORD;
VAR M,I,OLDN:LONGINT;
    SKIBO:BOOLEAN;
BEGIN
      OLDN:=N;
      SKIBO:=FALSE;
      WHILE (N<=Length(S)) DO BEGIN
       IF (SKIBO=FALSE) THEN BEGIN
        IF (IsDelimiter(SF,S,N)) THEN BREAK;
       END;
       IF (STRSKIP=TRUE) AND (IsDelimiter(#34#39,S,N)) THEN SKIBO:=NOT SKIBO;
       INC(N);
      END;
      RESULT:=N-OLDN;
END;

FUNCTION OneSkipSpaceELE(VAR S:STRING; N:DWORD):DWORD;
VAR M,I,OLDN:LONGINT;
BEGIN
      OLDN:=N;
      WHILE IsDelimiter(' '+#9,S,N) DO INC (N);
      RESULT:=N-OLDN;
END;

FUNCTION DeleteSpace(S:STRING):STRING;
VAR N,M,I:LONGINT;
BEGIN
        N:=OneSkipSpaceELE(S,1);
        DELETE(S,1,N);

        N:=LENGTH(S);
        WHILE (N>0) AND (IsDelimiter(' '+#9,S,N)) DO DEC(N);
        DELETE(S,N+1,LENGTH(S)-N);
        RESULT:=S;

END;


FUNCTION GetEle(S, SF:STRING; ElN:DWORD; STRSKIP:BOOLEAN=FALSE):ShortString;
VAR N,M,I,O:DWORD;
BEGIN
        N:=1;
        IF IsDelimiter(' ',SF,1) AND NOT IsDelimiter(#9,SF,1) THEN SF:=SF+#9;
        INC(N,OneSkipSpaceELE(S,N));
        WHILE ElN<>0 DO BEGIN
         INC(N,OneSkipSizeELE(SF,S,N,STRSKIP));
         INC(N,OneSkipSpaceELE(S,N));
         IF IsDelimiter(SF,S,N) THEN INC (N);
         INC(N,OneSkipSpaceELE(S,N));
         DEC(ElN);
        END;

        M:=N;
        INC(N,OneSkipSizeELE(SF,S,N,STRSKIP));
        INC(N,OneSkipSpaceELE(S,N));

        O:=N-Length(S)-1;
        S:=Mid(S,M,N-M);
//        INC(N,OneSkipSpaceELE(S,N));
        N:=LENGTH(S);
        WHILE (N>0) AND (IsDelimiter(' '+#9,S,N)) DO DEC(N);
        DELETE(S,N+1,LENGTH(S)-N);
        IF (LENGTH(S)=0) AND (O=0) THEN S:=#255;
        RESULT:=S;
END;

FUNCTION GetEleS(S, SF:STRING; ElN:DWORD):ShortString;
BEGIN
        RESULT:=GetEle(S,SF,ElN, TRUE);
END;


FUNCTION HexToInt(Str : ShortString): integer;
VAR i, r : integer;
BEGIN
  val('$'+Trim(Str),r, i);
  if i<>0 then RESULT := 0 {была ошибка в написании числа}
  else RESULT := r;
END;

FUNCTION DecToInt(Str : ShortString): integer;
VAR i, r : integer;
BEGIN
  val(Trim(Str),r, i);
  if i<>0 then RESULT := 0 {была ошибка в написании числа}
  else RESULT := r;
END;
{
function HexToInt(Value: String): LongWord;
const
  HexStr: String = '0123456789abcdef';
var
  i: Word;
begin
  Result := 0;
  if Value = '' then Exit;
  for i := 1 to Length(Value) do
    Inc(Result, (Pos(Value[i], HexStr) - 1) shl ((Length(Value) - i) shl 2));
end;
}


FUNCTION BinStrToHex(BINDATAS:ShortString):ShortString;
VAR N,M,I:LONGINT;
BEGIN
        RESULT:='';
        FOR N:=1 TO Length(BINDATAS) DO BEGIN
         RESULT:=RESULT+HexB(BYTE(BINDATAS[N]));
        END;
END;

FUNCTION HexToBinStr(S:ShortString):ShortString;
VAR N,M,I:LONGINT;
    B:BYTE;
BEGIN
        RESULT:='';
        N:=1;
        WHILE N<=Length(S) DO BEGIN
         B:=HexToInt(Mid(S,N,2));
         RESULT:=RESULT+CHAR(B);
         INC(N,2);
        END;

END;


FUNCTION ColorDarker(Color: TColor; Percent: Byte): TColor;
var r, g, b: Byte;
begin Color := ColorToRGB(Color);
  r := GetRValue(Color);
  g := GetGValue(Color);
  b := GetBValue(Color);
  r := r - muldiv(r, Percent, 100);
  //процент% уменьшени€ €ркости
  g := g - muldiv(g, Percent, 100);
  b := b - muldiv(b, Percent, 100);
  result := RGB(r, g, b);
end;

FUNCTION ColorLighter(Color: TColor; Percent: Byte): TColor;
var r, g, b: Byte;
begin
  Color := ColorToRGB(Color);
  r := GetRValue(Color);
  g := GetGValue(Color);
  b := GetBValue(Color);
  r := r + muldiv(255 - r, Percent, 100);
  //процент% увеличени€ €ркости
  g := g + muldiv(255 - g, Percent, 100);
  b := b + muldiv(255 - b, Percent, 100);
  result := RGB(r, g, b);
end;







FUNCTION DetCutSIGN(VAR S:STRING):LONGINT;
VAR I:LONGINT;
BEGIN
        I:=1;
        WHILE (LENGTH(S)<>0) AND (IsDelimiter('-+',S[1],1)) DO BEGIN
         IF S[1]='-' THEN I:=-I;
         DELETE(S,1,1);
        END;
        RESULT:=I;
END;


FUNCTION DetBin(S:STRING):BOOLEAN;
VAR N,M,I,O:DWORD;
    S1:STRING;
BEGIN
        DetBin:=FALSE;
        S1:=GetEle (S,'.',1);
        S:=GetEle (S,'.',0);
        IF IsDelimiter('Bb',S,Length(S)) AND IsDelimiter('01',S[1],1) THEN IF IsDelimiter('0123456789'+#255,S1,1) THEN BEGIN
         DetBin:=TRUE;
         FOR N:=1 TO Length(S)-1 DO IF NOT IsDelimiter('01',S[N],1) THEN DetBin:=FALSE;
        END;
END;

FUNCTION DetHex(S:STRING):BOOLEAN;
VAR N,M,I,O:DWORD;
    S1:STRING;
BEGIN
        DetCutSIGN(S);
        DetHex:=FALSE;
        S1:=GetEle (S,'.',1);
        S:=GetEle (S,'.',0);
        IF IsDelimiter('Hh',S,Length(S)) AND IsDelimiter('0123456789',S[1],1) THEN IF IsDelimiter('0123456789'+#255,S1,1) THEN BEGIN
         DetHex:=TRUE;
         FOR N:=1 TO Length(S)-1 DO IF NOT IsDelimiter('0123456789ABCDEF',UpperCase(S[N]),1) THEN DetHex:=FALSE;
        END;
END;

FUNCTION DetVal(S:STRING):BOOLEAN;
VAR N,M,I,O:DWORD;
    S1:STRING;
BEGIN
        DetCutSIGN(S);
        DetVal:=Length(S)<>0;
        FOR N:=1 TO Length(S) DO IF NOT IsDelimiter('0123456789',S[N],1) THEN DetVal:=FALSE;
        IF S='$' THEN DetVal:=TRUE;
END;

FUNCTION DetValS(S:STRING):BOOLEAN;
VAR N,M,I,O:DWORD;
BEGIN
        DetCutSIGN(S);
        DetValS:=Length(S)<>0;
        FOR N:=1 TO Length(S) DO IF NOT IsDelimiter('0123456789.',S[N],1) THEN DetValS:=FALSE;
        IF S[1]='.' THEN DetValS:=FALSE;
        IF S='$' THEN DetValS:=TRUE;
END;




FUNCTION DetVar(S:STRING):BOOLEAN; BEGIN
        DetCutSIGN(S);
        DetVar:=DetBin(S) OR DetHex(S) OR DetVal(S);
END;


FUNCTION DetVarS(S:STRING):BOOLEAN; BEGIN
        DetCutSIGN(S);
        DetVarS:=DetBin(S) OR DetHex(S) OR DetValS(S);
END;



FUNCTION GetVar(S:STRING):LONGINT;
VAR N,M,I,O,II:LONGINT;
BEGIN
      II:=DetCutSIGN(S);
      I:=0;
      S:=GetEle (S,'.',0);
      IF DetBin (S) THEN FOR N:=1 TO Length(S)-1 DO I:=I SHL 1 OR (BYTE(S[N])-48);
      IF DetHex (S) THEN I:=DWord(HexToInt(Mid(S,1,Length(S)-1)));
//      IF DetVal (S) THEN IF S='$' THEN I:=CoAddr ELSE Val(S,I,N);
      IF DetVal (S) THEN Val(S,I,N);
      GetVar:=I*II;
END;



FUNCTION ValAutoConvertSTRSTR(S:ShortString; MINSI:DWORD=1):ShortString;
VAR N,M,I,O,P:LONGINT;
    D,MA:DWORD;
    SS:ShortString;
    MD:DWORD;
BEGIN
     SS:='';
     IF S<>'' THEN BEGIN
      MD:=0;      //DEC
      IF S[1]='$' THEN BEGIN
       DELETE(S,1,1);
       MD:=1;     //HEX
      END ELSE IF S[LENGTH(S)]='H' THEN BEGIN
       DELETE(S,LENGTH(S),1);
       MD:=1;     //HEX
      END ELSE IF COPY(S,1,2)='0x' THEN BEGIN
       DELETE(S,1,2);
       MD:=1;     //HEX
      END;

      N:=MINSI;     //NUMBER OF BYTES
      IF MD=1 THEN BEGIN
       D:=HexToInt(S);
       CASE LENGTH(S) OF
        0,1,2:N:=1;
        3,4:N:=2;
        5,6:N:=3;
        ELSE N:=4;
       END;
      END ELSE BEGIN
       D:=DecToInt(S);
      END;

      MA:=$FFFFFF00 SHL (8*(N-1));
      WHILE ((D AND MA)<>0) DO BEGIN
       MA:=MA SHL 8;
       INC(N);
      END;

      WHILE N<>0 DO BEGIN
       SS:=SS+CHAR(D);
       D:=D SHR 8;
       DEC(N);
      END;

     END;
     RESULT:=SS;
END;

FUNCTION GetStrB(VAR S:ShortString; OFS:DWORD=1):BYTE;
BEGIN
      RESULT:=PBYTE(@S[OFS])^;
END;

FUNCTION GetStrW(VAR S:ShortString; OFS:DWORD=1):WORD;
BEGIN
      RESULT:=PWORD(@S[OFS])^;
END;

FUNCTION GetStrD(VAR S:ShortString; OFS:DWORD=1):DWORD;
BEGIN
      RESULT:=PDWORD(@S[OFS])^;
END;


FUNCTION CutStrB(VAR S:ShortString):BYTE;
BEGIN
      RESULT:=GetStrB(S);
      DELETE(S,1,1);
END;

FUNCTION CutStrW(VAR S:ShortString):WORD;
BEGIN
      RESULT:=GetStrW(S);
      DELETE(S,1,2);
END;

FUNCTION CutStrD(VAR S:ShortString):DWORD;
BEGIN
      RESULT:=GetStrD(S);
      DELETE(S,1,4);
END;

FUNCTION CHRISU(S,SCH:ShortString):BOOLEAN;
BEGIN
    RESULT := UpperCase(Right(S,Length(SCH))) = UpperCase(SCH);
END;

FUNCTION WinToDosR(b:CHAR):CHAR;
BEGIN
    IF (b<#$C0) THEN BEGIN
      CASE (b) OF
        #$A8: b:=#$F0;
        #$B8: b:=#$F1;
        #$AA: b:=#$F2;
        #$BA: b:=#$F3;
        #$AF: b:=#$F4;
        #$BF: b:=#$F5;
      END;
    END ELSE IF (b<#$F0) THEN BEGIN
      DEC(b,$40);
    END ELSE BEGIN
      DEC(b,$10);
    END;
    RESULT:=b;
END;

FUNCTION DosToWinR(b:CHAR):CHAR;
BEGIN
    IF (b<#$80) THEN BEGIN
    END ELSE IF (b<#$B0) THEN BEGIN
      INC(b,$40);
    END ELSE IF (b<#$E0) THEN BEGIN
    END ELSE IF (b<#$F0) THEN BEGIN
      INC(b,$10);
    END ELSE BEGIN
      CASE (b) OF
        #$F0: b:=#$A8;
        #$F1: b:=#$B8;
        #$F2: b:=#$AA;
        #$F3: b:=#$BA;
        #$F4: b:=#$AF;
        #$F5: b:=#$BF;
      END;
    END;
    RESULT:=b;
END;

FUNCTION AlignD(D,AL:DWORD):DWORD;
BEGIN
    DEC(D);
    RESULT:=D+AL-(D MOD AL);
END;

FUNCTION AlignVarD(VAR D:DWORD; AL:DWORD):DWORD;
BEGIN
    D:=AlignD(D,AL);
    RESULT:=D;
END;


BEGIN

END.

