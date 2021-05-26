//------------------------------------------------------------------------
//
// Name        : Cyclic Redundancy Code (CRC)
// Authors     : Rob F. / Entire Group
// Email       : RobusAsmoder@ukr.net
// Date        : 23 june 2008 ... 04.02.2014
// Description : Oh ...
//
//    GenCRC(MAS,$31,CRC_MODE_8);                     //STANDART CRC8   P=31H
//    GenCRC(MAS,$8005,CRC_MODE_16 OR CRC_REVERT);    //STANDART CRC16  P=8005H     + REVERT
//    GenCRC(MAS,$1021,CRC_MODE_16);                  //EXPERT   CRC16  P=1021H
//    GenCRC(MAS,$04C11DB7,CRC_MODE_32 OR CRC_REVERT);//STANDART CRC32  P=04C11DB7H + REVERT
//    GenCRC(MAS,$77073096,CRC_MODE_32);              //EXPERT   CRC32  P=77073096H
//
//------------------------------------------------------------------------


Unit RBCRC;


Interface
  uses SysUtils, Windows;

CONST CRC_MODE_8  = $0001;
      CRC_MODE_16 = $0002;
      CRC_MODE_32 = $0008;
      CRC_REVERT  = $0010;
//TYPE DWORD = Cardinal;

TYPE tCRC=RECORD
      MODE:DWORD;
      CRC_S:DWORD;
      CRC:DWORD;
      DAT:POINTER;
      ID:ShortString;
     END;

CONST rbCRC_ID_STR='RHIWERIOUGHWEIUORGILVDDEBIL';

PROCEDURE GenCRC(VAR MCRC;CRC_S,CRC_MODE:DWORD); STDCALL;

FUNCTION GetCRCT32(VAR MCRC;CRC_V:DWORD;CRC_D:BYTE):DWORD; STDCALL;
FUNCTION GetCRCT16(VAR MCRC;CRC_V:WORD;CRC_D:BYTE):WORD; STDCALL;
FUNCTION GetCRCT8(VAR MCRC;CRC_V:BYTE;CRC_D:BYTE):BYTE; STDCALL;

FUNCTION CRC_OPEN(VAR CR:tCRC; CRC_S,CRC_MODE:DWORD):BOOLEAN; STDCALL;
FUNCTION CRC_CLOSE(VAR CR:tCRC):BOOLEAN; STDCALL;
FUNCTION CRC_BUF(VAR CR:tCRC; VAR BUF; SIZE:DWORD; CRC_START:DWORD=0):DWORD; STDCALL;
FUNCTION CRC_STR(VAR CR:tCRC; VAR S:ShortString; CRC_START:DWORD=0):DWORD; STDCALL;
Implementation
//VAR N,M,I:DWORD;
//VAR CRC_TABLE:ARRAY[0..255] OF DWORD;



FUNCTION GetCRCT32(VAR MCRC;CRC_V:DWORD;CRC_D:BYTE):DWORD; STDCALL; ASM
        MOV EAX,CRC_V
        XOR AL,CRC_D
        MOVZX EDX,AL
        SHL EDX,2
        ADD EDX,MCRC
        SHR EAX,8
        XOR EAX,[EDX]
END;
FUNCTION GetCRCT16(VAR MCRC;CRC_V:WORD;CRC_D:BYTE):WORD; STDCALL; ASM
        MOVZX EAX,CRC_V
        MOVZX EDX,AL
        XOR DL,CRC_D
        SHL EDX,2
        ADD EDX,MCRC
        SHR EAX,8
        XOR AX,[EDX]
END;
FUNCTION GetCRCT8(VAR MCRC;CRC_V:BYTE;CRC_D:BYTE):BYTE; STDCALL; ASM
        MOVZX EAX,CRC_V
        XOR AL,CRC_D
        SHL EAX,2
        ADD EAX,MCRC
        MOVZX EAX,BYTE PTR [EAX]
END;

{
FUNCTION GetCRC32(VAR CRC_V:DWORD;CRC_D:BYTE):DWORD; STDCALL; ASM

        MOV EAX,CRC_V
        XOR AL,CRC_D
        MOVZX EDX,AL
        SHR EAX,8
        XOR EAX,DWORD PTR [CRC_TABLE+EDX*4]

END;
}



//Generator of CRC UNIVERSAL
PROCEDURE GenCRC(VAR MCRC;CRC_S,CRC_MODE:DWORD); STDCALL; ASM
        PUSH EBX
        PUSH ESI
        PUSH EDI

        MOV EAX,CRC_S
        TEST CRC_MODE,CRC_REVERT
        JZ @M3x1
        MOV ECX,32
@M3:    SHR CRC_S,1
        RCL EAX,1
        LOOP @M3
        TEST CRC_MODE,CRC_MODE_32
        JNZ @M3x1
        SHR EAX,16
        TEST CRC_MODE,CRC_MODE_16
        JNZ @M3x1
//        MOV AL,AH
        SHR AX,8
@M3x1:  MOV CRC_S,EAX

//        SHR CRC_S,8

        MOV EDI,MCRC
        MOV ECX,256
@M1:
        MOVZX EAX,CL
        NEG AL
        TEST CRC_MODE,CRC_REVERT
        JNZ @M1x1
        TEST CRC_MODE,CRC_MODE_8
        JNZ @M1x1
        SHL EAX,8
        TEST CRC_MODE,CRC_MODE_16
        JNZ @M1x1
        SHL EAX,16
@M1x1:
        CALL @M0
        CALL @M0
        CALL @M0
        CALL @M0
        CALL @M0
        CALL @M0
        CALL @M0
        CALL @M0
{        STOSB
        TEST CRC_MODE,CRC_MODE_8
        JNZ @M1x2
        SHR EAX,8
        STOSB
        TEST CRC_MODE,CRC_MODE_16
        JNZ @M1x2
        SHR EAX,8
        STOSW
@M1x2:  }
        STOSD
        LOOP @M1
        JMP @EXIT

@M0:    TEST CRC_MODE,CRC_REVERT
        JNZ @M0x1
        TEST CRC_MODE,CRC_MODE_8
        JNZ @M0x2
        TEST CRC_MODE,CRC_MODE_16
        JNZ @M0x3
        SHL EAX,1
        JNC @M2
        XOR EAX,CRC_S
        RET 0

@M0x2:  SHL AL,1
        JNC @M2
        XOR EAX,CRC_S
        RET 0

@M0x3:  SHL AX,1
        JNC @M2
        XOR EAX,CRC_S
        RET 0

@M0x1:  SHR EAX,1
        JNC @M2
        XOR EAX,CRC_S
@M2:    RET 0


@EXIT:
        POP EDI
        POP ESI
        POP EBX
END;




FUNCTION CRC_OPEN(VAR CR:tCRC; CRC_S,CRC_MODE:DWORD):BOOLEAN; STDCALL;
VAR N,M,I:LONGINT;
BEGIN
      IF CR.ID<>rbCRC_ID_STR THEN BEGIN
       CR.DAT:=GetMemory(1024);
       CR.MODE:=CRC_MODE;
       CR.CRC_S:=CRC_S;
       CR.CRC:=0;
       GenCRC(CR.DAT^, CRC_S, CRC_MODE);
       CR.ID:=rbCRC_ID_STR;
       RESULT:=TRUE;
      END ELSE BEGIN
       RESULT:=FALSE;
      END;
END;

FUNCTION CRC_CLOSE(VAR CR:tCRC):BOOLEAN; STDCALL;
BEGIN
      IF CR.ID=rbCRC_ID_STR THEN BEGIN
       FreeMem(CR.DAT,1024);
       CR.ID:='';
       RESULT:=TRUE;
      END ELSE BEGIN
       RESULT:=FALSE;
      END;
END;

FUNCTION CRC_BUF(VAR CR:tCRC; VAR BUF; SIZE:DWORD; CRC_START:DWORD=0):DWORD; STDCALL;
VAR N:LONGINT;
BEGIN
      CR.CRC:=CRC_START;
      IF (CR.MODE AND CRC_MODE_8) <> 0 THEN BEGIN
       FOR N:=0 TO SIZE-1 DO CR.CRC:=GetCRCT8(CR.DAT^, CR.CRC, PByte(DWORD(@BUF)+N)^);
      END ELSE IF (CR.MODE AND CRC_MODE_16) <> 0 THEN BEGIN
       FOR N:=0 TO SIZE-1 DO CR.CRC:=GetCRCT16(CR.DAT^, CR.CRC, PByte(DWORD(@BUF)+N)^);
      END ELSE IF (CR.MODE AND CRC_MODE_32) <> 0 THEN BEGIN
       FOR N:=0 TO SIZE-1 DO CR.CRC:=GetCRCT32(CR.DAT^, CR.CRC, PByte(DWORD(@BUF)+N)^);
      END;
      RESULT:=CR.CRC;
END;

FUNCTION CRC_STR(VAR CR:tCRC; VAR S:ShortString; CRC_START:DWORD=0):DWORD; STDCALL;
BEGIN
      RESULT:=CRC_BUF(CR, S[1], Length(S), CRC_START);
END;

BEGIN
//        GenCRC32(N,0);
END.
