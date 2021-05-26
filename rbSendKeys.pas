UNIT rbSendKeys;

INTERFACE
USES Windows, SysUtils, TlHelp32;

TYPE TKeyLST = RECORD
      VKKEY     :DWORD;
      NAME      :STRING;
     END;
TYPE TTKeyLST = ^TKeyLST;

CONST CONST_VKKEY = $00000000;

CONST VK_0        = CONST_VKKEY + ORD('0');
CONST VK_1        = CONST_VKKEY + ORD('1');
CONST VK_2        = CONST_VKKEY + ORD('2');
CONST VK_3        = CONST_VKKEY + ORD('3');
CONST VK_4        = CONST_VKKEY + ORD('4');
CONST VK_5        = CONST_VKKEY + ORD('5');
CONST VK_6        = CONST_VKKEY + ORD('6');
CONST VK_7        = CONST_VKKEY + ORD('7');
CONST VK_8        = CONST_VKKEY + ORD('8');
CONST VK_9        = CONST_VKKEY + ORD('9');
CONST VK_A        = CONST_VKKEY + ORD('A');
CONST VK_B        = CONST_VKKEY + ORD('B');
CONST VK_C        = CONST_VKKEY + ORD('C');
CONST VK_D        = CONST_VKKEY + ORD('D');
CONST VK_E        = CONST_VKKEY + ORD('E');
CONST VK_F        = CONST_VKKEY + ORD('F');
CONST VK_G        = CONST_VKKEY + ORD('G');
CONST VK_H        = CONST_VKKEY + ORD('H');
CONST VK_I        = CONST_VKKEY + ORD('I');
CONST VK_J        = CONST_VKKEY + ORD('J');
CONST VK_K        = CONST_VKKEY + ORD('K');
CONST VK_L        = CONST_VKKEY + ORD('L');
CONST VK_M        = CONST_VKKEY + ORD('M');
CONST VK_N        = CONST_VKKEY + ORD('N');
CONST VK_O        = CONST_VKKEY + ORD('O');
CONST VK_P        = CONST_VKKEY + ORD('P');
CONST VK_Q        = CONST_VKKEY + ORD('Q');
CONST VK_R        = CONST_VKKEY + ORD('R');
CONST VK_S        = CONST_VKKEY + ORD('S');
CONST VK_T        = CONST_VKKEY + ORD('T');
CONST VK_U        = CONST_VKKEY + ORD('U');
CONST VK_V        = CONST_VKKEY + ORD('V');
CONST VK_W        = CONST_VKKEY + ORD('W');
CONST VK_X        = CONST_VKKEY + ORD('X');
CONST VK_Y        = CONST_VKKEY + ORD('Y');
CONST VK_Z        = CONST_VKKEY + ORD('Z');


CONST VK_NOTDEFINED = $FFFFFFFF;

VAR Key_VK_List:ARRAY[0..102+36] OF TKeyLST=(
  (VKKEY:VK_NOTDEFINED      ;NAME:'NONE         '         ),
  (VKKEY:VK_RETURN          ;NAME:'VK_RETURN    '         ),
  (VKKEY:VK_PAUSE           ;NAME:'VK_PAUSE     '         ),
  (VKKEY:VK_LEFT            ;NAME:'VK_LEFT      '         ),
  (VKKEY:VK_UP              ;NAME:'VK_UP        '         ),
  (VKKEY:VK_RIGHT           ;NAME:'VK_RIGHT     '         ),
  (VKKEY:VK_DOWN            ;NAME:'VK_DOWN      '         ),
  (VKKEY:VK_ESCAPE          ;NAME:'VK_ESCAPE    '         ),
  (VKKEY:VK_LSHIFT          ;NAME:'VK_LSHIFT    '         ),
  (VKKEY:VK_RSHIFT          ;NAME:'VK_RSHIFT    '         ),
  (VKKEY:VK_LCONTROL        ;NAME:'VK_LCONTROL  '         ),
  (VKKEY:VK_RCONTROL        ;NAME:'VK_RCONTROL  '         ),
  (VKKEY:VK_LBUTTON         ;NAME:'VK_LBUTTON   '         ),
  (VKKEY:VK_RBUTTON         ;NAME:'VK_RBUTTON   '         ),
  (VKKEY:VK_MBUTTON         ;NAME:'VK_MBUTTON   '         ),
  (VKKEY:VK_SPACE           ;NAME:'VK_SPACE     '         ),

  (VKKEY:VK_0               ;NAME:'0            '         ),
  (VKKEY:VK_1               ;NAME:'1            '         ),
  (VKKEY:VK_2               ;NAME:'2            '         ),
  (VKKEY:VK_3               ;NAME:'3            '         ),
  (VKKEY:VK_4               ;NAME:'4            '         ),
  (VKKEY:VK_5               ;NAME:'5            '         ),
  (VKKEY:VK_6               ;NAME:'6            '         ),
  (VKKEY:VK_7               ;NAME:'7            '         ),
  (VKKEY:VK_8               ;NAME:'8            '         ),
  (VKKEY:VK_9               ;NAME:'9            '         ),
  (VKKEY:VK_A               ;NAME:'A            '         ),
  (VKKEY:VK_B               ;NAME:'B            '         ),
  (VKKEY:VK_C               ;NAME:'C            '         ),
  (VKKEY:VK_D               ;NAME:'D            '         ),
  (VKKEY:VK_E               ;NAME:'E            '         ),
  (VKKEY:VK_F               ;NAME:'F            '         ),
  (VKKEY:VK_G               ;NAME:'G            '         ),
  (VKKEY:VK_H               ;NAME:'H            '         ),
  (VKKEY:VK_I               ;NAME:'I            '         ),
  (VKKEY:VK_J               ;NAME:'J            '         ),
  (VKKEY:VK_K               ;NAME:'K            '         ),
  (VKKEY:VK_L               ;NAME:'L            '         ),
  (VKKEY:VK_M               ;NAME:'M            '         ),
  (VKKEY:VK_N               ;NAME:'N            '         ),
  (VKKEY:VK_O               ;NAME:'O            '         ),
  (VKKEY:VK_P               ;NAME:'P            '         ),
  (VKKEY:VK_Q               ;NAME:'Q            '         ),
  (VKKEY:VK_R               ;NAME:'R            '         ),
  (VKKEY:VK_S               ;NAME:'S            '         ),
  (VKKEY:VK_T               ;NAME:'T            '         ),
  (VKKEY:VK_U               ;NAME:'U            '         ),
  (VKKEY:VK_V               ;NAME:'V            '         ),
  (VKKEY:VK_W               ;NAME:'W            '         ),
  (VKKEY:VK_X               ;NAME:'X            '         ),
  (VKKEY:VK_Y               ;NAME:'Y            '         ),
  (VKKEY:VK_Z               ;NAME:'Z            '         ),

  (VKKEY:VK_CANCEL          ;NAME:'VK_CANCEL    '         ),
  (VKKEY:VK_BACK            ;NAME:'VK_BACK      '         ),
  (VKKEY:VK_TAB             ;NAME:'VK_TAB       '         ),
  (VKKEY:VK_CLEAR           ;NAME:'VK_CLEAR     '         ),
  (VKKEY:VK_SHIFT           ;NAME:'VK_SHIFT     '         ),
  (VKKEY:VK_CONTROL         ;NAME:'VK_CONTROL   '         ),
  (VKKEY:VK_MENU            ;NAME:'VK_MENU      '         ),
  (VKKEY:VK_CAPITAL         ;NAME:'VK_CAPITAL   '         ),
  (VKKEY:VK_KANA            ;NAME:'VK_KANA      '         ),
  (VKKEY:VK_HANGUL          ;NAME:'VK_HANGUL    '         ),
  (VKKEY:VK_JUNJA           ;NAME:'VK_JUNJA     '         ),
  (VKKEY:VK_FINAL           ;NAME:'VK_FINAL     '         ),
  (VKKEY:VK_HANJA           ;NAME:'VK_HANJA     '         ),
  (VKKEY:VK_KANJI           ;NAME:'VK_KANJI     '         ),
  (VKKEY:VK_CONVERT         ;NAME:'VK_CONVERT   '         ),
  (VKKEY:VK_NONCONVERT      ;NAME:'VK_NONCONVERT'         ),
  (VKKEY:VK_ACCEPT          ;NAME:'VK_ACCEPT    '         ),
  (VKKEY:VK_MODECHANGE      ;NAME:'VK_MODECHANGE'         ),
  (VKKEY:VK_PRIOR           ;NAME:'VK_PRIOR     '         ),
  (VKKEY:VK_NEXT            ;NAME:'VK_NEXT      '         ),
  (VKKEY:VK_END             ;NAME:'VK_END       '         ),
  (VKKEY:VK_HOME            ;NAME:'VK_HOME      '         ),
  (VKKEY:VK_SELECT          ;NAME:'VK_SELECT    '         ),
  (VKKEY:VK_PRINT           ;NAME:'VK_PRINT     '         ),
  (VKKEY:VK_EXECUTE         ;NAME:'VK_EXECUTE   '         ),
  (VKKEY:VK_SNAPSHOT        ;NAME:'VK_SNAPSHOT  '         ),
  (VKKEY:VK_INSERT          ;NAME:'VK_INSERT    '         ),
  (VKKEY:VK_DELETE          ;NAME:'VK_DELETE    '         ),
  (VKKEY:VK_HELP            ;NAME:'VK_HELP      '         ),
  (VKKEY:VK_LWIN            ;NAME:'VK_LWIN      '         ),
  (VKKEY:VK_RWIN            ;NAME:'VK_RWIN      '         ),
  (VKKEY:VK_APPS            ;NAME:'VK_APPS      '         ),
  (VKKEY:VK_NUMPAD0         ;NAME:'VK_NUMPAD0   '         ),
  (VKKEY:VK_NUMPAD1         ;NAME:'VK_NUMPAD1   '         ),
  (VKKEY:VK_NUMPAD2         ;NAME:'VK_NUMPAD2   '         ),
  (VKKEY:VK_NUMPAD3         ;NAME:'VK_NUMPAD3   '         ),
  (VKKEY:VK_NUMPAD4         ;NAME:'VK_NUMPAD4   '         ),
  (VKKEY:VK_NUMPAD5         ;NAME:'VK_NUMPAD5   '         ),
  (VKKEY:VK_NUMPAD6         ;NAME:'VK_NUMPAD6   '         ),
  (VKKEY:VK_NUMPAD7         ;NAME:'VK_NUMPAD7   '         ),
  (VKKEY:VK_NUMPAD8         ;NAME:'VK_NUMPAD8   '         ),
  (VKKEY:VK_NUMPAD9         ;NAME:'VK_NUMPAD9   '         ),
  (VKKEY:VK_MULTIPLY        ;NAME:'VK_MULTIPLY  '         ),
  (VKKEY:VK_ADD             ;NAME:'VK_ADD       '         ),
  (VKKEY:VK_SEPARATOR       ;NAME:'VK_SEPARATOR '         ),
  (VKKEY:VK_SUBTRACT        ;NAME:'VK_SUBTRACT  '         ),
  (VKKEY:VK_DECIMAL         ;NAME:'VK_DECIMAL   '         ),
  (VKKEY:VK_DIVIDE          ;NAME:'VK_DIVIDE    '         ),
  (VKKEY:VK_F1              ;NAME:'VK_F1        '         ),
  (VKKEY:VK_F2              ;NAME:'VK_F2        '         ),
  (VKKEY:VK_F3              ;NAME:'VK_F3        '         ),
  (VKKEY:VK_F4              ;NAME:'VK_F4        '         ),
  (VKKEY:VK_F5              ;NAME:'VK_F5        '         ),
  (VKKEY:VK_F6              ;NAME:'VK_F6        '         ),
  (VKKEY:VK_F7              ;NAME:'VK_F7        '         ),
  (VKKEY:VK_F8              ;NAME:'VK_F8        '         ),
  (VKKEY:VK_F9              ;NAME:'VK_F9        '         ),
  (VKKEY:VK_F10             ;NAME:'VK_F10       '         ),
  (VKKEY:VK_F11             ;NAME:'VK_F11       '         ),
  (VKKEY:VK_F12             ;NAME:'VK_F12       '         ),
  (VKKEY:VK_F13             ;NAME:'VK_F13       '         ),
  (VKKEY:VK_F14             ;NAME:'VK_F14       '         ),
  (VKKEY:VK_F15             ;NAME:'VK_F15       '         ),
  (VKKEY:VK_F16             ;NAME:'VK_F16       '         ),
  (VKKEY:VK_F17             ;NAME:'VK_F17       '         ),
  (VKKEY:VK_F18             ;NAME:'VK_F18       '         ),
  (VKKEY:VK_F19             ;NAME:'VK_F19       '         ),
  (VKKEY:VK_F20             ;NAME:'VK_F20       '         ),
  (VKKEY:VK_F21             ;NAME:'VK_F21       '         ),
  (VKKEY:VK_F22             ;NAME:'VK_F22       '         ),
  (VKKEY:VK_F23             ;NAME:'VK_F23       '         ),
  (VKKEY:VK_F24             ;NAME:'VK_F24       '         ),
  (VKKEY:VK_NUMLOCK         ;NAME:'VK_NUMLOCK   '         ),
  (VKKEY:VK_SCROLL          ;NAME:'VK_SCROLL    '         ),
  (VKKEY:VK_LMENU           ;NAME:'VK_LMENU     '         ),
  (VKKEY:VK_RMENU           ;NAME:'VK_RMENU     '         ),
  (VKKEY:VK_PROCESSKEY      ;NAME:'VK_PROCESSKEY'         ),
  (VKKEY:VK_ATTN            ;NAME:'VK_ATTN      '         ),
  (VKKEY:VK_CRSEL           ;NAME:'VK_CRSEL     '         ),
  (VKKEY:VK_EXSEL           ;NAME:'VK_EXSEL     '         ),
  (VKKEY:VK_EREOF           ;NAME:'VK_EREOF     '         ),
  (VKKEY:VK_PLAY            ;NAME:'VK_PLAY      '         ),
  (VKKEY:VK_ZOOM            ;NAME:'VK_ZOOM      '         ),
  (VKKEY:VK_NONAME          ;NAME:'VK_NONAME    '         ),
  (VKKEY:VK_PA1             ;NAME:'VK_PA1       '         ),
  (VKKEY:VK_OEM_CLEAR       ;NAME:'VK_OEM_CLEAR '         ),
  (VKKEY:0                  ;NAME:''                      )
);

FUNCTION KeybdVK_LST_COUNT:DWORD;
FUNCTION KeybdVK_LST(NUM:DWORD):TTKeyLST;
FUNCTION KeybdVK_STR(VK:DWORD):STRING;

PROCEDURE KeybdInputStart;
FUNCTION KeybdInputAdd(VKey: Byte; Flags: DWORD):BOOLEAN; OVERLOAD;
FUNCTION KeybdInputAdd(VKey: Byte):BOOLEAN; OVERLOAD;
FUNCTION KeybdInputAdd(VKey: CHAR; Flags: DWORD):BOOLEAN; OVERLOAD;
FUNCTION KeybdInputAdd(VKey: CHAR):BOOLEAN; OVERLOAD;
FUNCTION KeybdInputAdd(VKeySTR: ShortString; Flags: DWORD):BOOLEAN; OVERLOAD;
FUNCTION KeybdInputAdd(VKeySTR: ShortString):BOOLEAN; OVERLOAD;
FUNCTION KeybdInputAddKeysStr(S: ShortString):BOOLEAN;
PROCEDURE KeybdInputEnd(hndl:HWND; SLEEPTIME:WORD=0);
PROCEDURE KeybdPutKey(hndl:HWND; VK:DWORD; isPressed:BOOLEAN; SLEEPTIME:WORD);
PROCEDURE KeybdSendKey(Wnd,VK:DWORD; Ctrl,Alt,Shift:BOOLEAN);
PROCEDURE KeybdInit(KeysMAX:WORD);
FUNCTION processExists(exeFileName: string): THandle;
FUNCTION GetHWndByPID(const hPID: THandle): THandle;

CONST KEYEVENTF_KEYDOWN = 0;

IMPLEMENTATION
VAR KeyInputs: ARRAY of TInput;
    KeyInputsMAX:WORD = 0;
    KeyInputsCNT:WORD = 0;

PROCEDURE KeybdInputStart;
BEGIN
    KeyInputsCNT:=0;
END;
//--------------------------------------------
FUNCTION KeybdInputAdd(VKey: Byte; Flags: DWORD):BOOLEAN; OVERLOAD;
BEGIN
   IF KeyInputsCNT<KeyInputsMAX THEN BEGIN
    KeyInputs[KeyInputsCNT].Itype := INPUT_KEYBOARD;
    WITH KeyInputs[KeyInputsCNT].ki DO BEGIN
      wVk := VKey;
      wScan := MapVirtualKey(VKey, 0);
      dwFlags := Flags;
    END;
    INC(KeyInputsCNT);
    RESULT:=TRUE;
   END ELSE BEGIN
    RESULT:=FALSE;
   END;
END;

FUNCTION KeybdInputAdd(VKey: CHAR; Flags: DWORD):BOOLEAN; OVERLOAD;
BEGIN
    RESULT:=KeybdInputAdd(ORD(VKey), Flags);
END;

FUNCTION KeybdInputAdd(VKey: CHAR):BOOLEAN; OVERLOAD;
BEGIN
    RESULT:=FALSE;
    IF NOT KeybdInputAdd(VKey, 0) THEN EXIT;
    IF NOT KeybdInputAdd(VKey, KEYEVENTF_KEYUP) THEN EXIT;
    RESULT:=TRUE;
END;

FUNCTION KeybdInputAdd(VKey: BYTE):BOOLEAN; OVERLOAD;
BEGIN
    RESULT:=KeybdInputAdd(CHAR(VKey));
END;

FUNCTION KeybdInputAdd(VKeySTR: ShortString; Flags: DWORD):BOOLEAN; OVERLOAD;
VAR N:LONGINT;
BEGIN
      RESULT:=FALSE;
      FOR N:=1 TO Length(VKeySTR) DO BEGIN
       IF NOT KeybdInputAdd(VKeySTR[N], Flags) THEN EXIT;
      END;
      RESULT:=TRUE;
END;

FUNCTION KeybdInputAdd(VKeySTR: ShortString):BOOLEAN; OVERLOAD;
VAR N:LONGINT;
BEGIN
      RESULT:=FALSE;
      FOR N:=1 TO Length(VKeySTR) DO BEGIN
       IF NOT KeybdInputAdd(CHAR(VKeySTR[N])) THEN EXIT;
      END;
      RESULT:=TRUE;
END;



FUNCTION KeybdVK_LST_COUNT:DWORD;
VAR N:LONGINT;
    S:^STRING;
    V:^DWORD;
BEGIN
    N:=0;
    REPEAT
     RESULT:=N;
     V:=@Key_VK_List[N].VKKEY;
     S:=@Key_VK_List[N].NAME;
     IF ((V^=0) AND (S^='')) THEN BREAK;
     INC(N);
    UNTIL FALSE;
END;


FUNCTION KeybdVK_LST(NUM:DWORD):TTKeyLST;
VAR N:LONGINT;
    S:^STRING;
    V:^DWORD;
BEGIN
    N:=0;
    REPEAT
     RESULT:=@Key_VK_List[N];
     V:=@Key_VK_List[N].VKKEY;
     S:=@Key_VK_List[N].NAME;
     IF ((V^=0) AND (S^='')) OR (N=NUM) THEN BREAK;
     INC(N);
    UNTIL FALSE;
END;

FUNCTION KeybdVK_STR(VK:DWORD):STRING;
VAR N:LONGINT;
    S:^STRING;
    V:^DWORD;
BEGIN
    N:=0;
    REPEAT
     V:=@Key_VK_List[N].VKKEY;
     S:=@Key_VK_List[N].NAME;
     RESULT:=S^;
     IF ((V^=0) AND (S^='')) OR (V^=VK) THEN BREAK;
     INC(N);
    UNTIL FALSE;
END;

//FUNCTION KeybdVK_VK()

// !"#$%&'()*+,-./0123456789:;<=>?
//@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_
//`abcdefghijklmnopqrstuvwxyz{|}~
{
CONST KECHTBL:ARRAY[0..47] OF WORD=(
  //$00..$0F
  0,0,0,0,0,0,0,0,0,0,0,0,0,VK_RETURN,0,0,

  //$10..$1F
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  //$20..$2F
  //      !     "      #    $     %     &    '      (     )     *     +     ,     -     .     /
  $0420,$0431,$04DE,$0433,$0434,$0435,$0437,$00DE,$0439,$0430,$0438,$04BB,$00BC,$00BD,$00BE,$00BF,
  //$30..$3F
  //0     1      2     3     4    5     6     7     8    9     :     ;      <     =     >    ?
  $0030,$0031,$0032,$0033,$0034,$0035,$0036,$0037,$0038,$0039,$04BA,$00BA,$04BC,$00BB,$04BE,$04BF,
  //$40..$4F
  //@      A     B    C      D     E    F     G     H     I     J     K     L     M     N     O
  $0432,$0041,$0042,$0043,$0044,$0045,$0046,$0047,$0048,$0049,$004A,$004B,$004C,$004D,$004E,$004F,
  //$50..$5F
  //P     Q     R     S     T     U     V     W     X     Y     Z     [     \     ]     ^     _
  $0050,$0051,$0052,$0053,$0054,$0055,$0056,$0057,$0058,$0059,$005A,$00DB,$00DC,$00DD,$0436,$04BD,
  //$60..$6F
  //`     a     b     c     d     e     f     g     h     i     j     k     l     m     n     o
  $04C0,$0441,$0442,$0443,$0444,$0445,$0446,$0447,$0448,$0449,$044A,$044B,$044C,$044D,$044E,$044F,
  //$70..$7F
  //p     q     r     s     t     u     v     w     x     y     z   } {     |     }   { ~     
  $0050,$0051,$0052,$0053,$0054,$0055,$0056,$0057,$0058,$0059,$005A,$00DB,$00DC,$00DD,$0436,$04BD,
);
}
FUNCTION GetVKKeyFromChar(C:CHAR):BYTE;
BEGIN

END;



FUNCTION KeybdInputAddKeysStr(S: ShortString):BOOLEAN;
VAR N,M,I:LONGINT;
    CAP:BOOLEAN;
    SHI:BYTE;
    US,LS,SS:ShortString;
BEGIN
      FOR N:=1 TO Length(S) DO BEGIN
       KeybdInputAdd(VK_MENU, 0);
       SS:=IntToStr(BYTE(S[N]));
       FOR I:=1 TO LENGTH(SS) DO BEGIN
        KeybdInputAdd( VK_NUMPAD0+(BYTE(SS[I]) AND 15) );
       END;
       KeybdInputAdd(VK_MENU, KEYEVENTF_KEYUP);
      END;
END;

//OUT:OLD HWND
FUNCTION KeybdHWND_Active(hndl:HWND):HWND;
VAR OLDHWND:HWND;
    P: ^DWORD;
    D: DWORD;
BEGIN
    OLDHWND:=0;
    IF hndl<>0 THEN BEGIN
     OLDHWND := GetForegroundWindow();
     P := @D;
     SystemParametersInfo(//get flashing timeout on win98
       SPI_GETFOREGROUNDLOCKTIMEOUT,
       0,
       P,
       0);
     SetForeGroundWindow(hndl);
    END;
    RESULT:=OLDHWND;
END;

FUNCTION KeybdHWND_Return(OLDHWND:HWND):BOOLEAN;
VAR P: ^DWORD;
    D: DWORD;
BEGIN
    IF OLDHWND<>0 THEN BEGIN
     SystemParametersInfo(//set flashing TimeOut=0
       SPI_SETFOREGROUNDLOCKTIMEOUT,
       0,
       nil,
       0);
//     SetForegroundWindow(TWinControl(TComponent(Self).Owner).Handle);
//     SetForegroundWindow(TWinControl(TComponent(OLDHWND).Owner).Handle);

   //  SetForegroundWindow(Application.Handle);
     SetForegroundWindow(OLDHWND);
//     SLEEP(SLEEPTIME);
     //->typecast working...
     SystemParametersInfo(//set flashing TimeOut=previous value
       SPI_SETFOREGROUNDLOCKTIMEOUT,
       D,
       nil,
       0);
     //SLEEP(SLEEPTIME);
     FillChar(KeyInputs[0], SizeOf(KeyInputs), 0);
     RESULT:=TRUE;
    END ELSE BEGIN
     RESULT:=FALSE;
    END;
END;

PROCEDURE KeybdInputEnd(hndl:HWND; SLEEPTIME:WORD=0);
VAR OLDHWND:HWND;
BEGIN

    OLDHWND:=KeybdHWND_Active(hndl);

    SendInput(KeyInputsCNT, KeyInputs[0], SizeOf(KeyInputs[0]));
    SLEEP(SLEEPTIME);
    KeyInputsCNT:=0;

    IF ( KeybdHWND_Return(OLDHWND) ) THEN BEGIN
     SLEEP(SLEEPTIME);
    END;
END;

PROCEDURE KeybdPutKey(hndl:HWND; VK:DWORD; isPressed:BOOLEAN; SLEEPTIME:WORD);
VAR OLDHWND:HWND;
BEGIN
    OLDHWND:=KeybdHWND_Active(hndl);
    IF (isPressed) THEN BEGIN
     keybd_event(VK,0,KEYEVENTF_KEYDOWN, 0);
    END ELSE BEGIN
     keybd_event(VK,0,KEYEVENTF_KEYUP, 0);
    END;
    SLEEP(SLEEPTIME);
    KeybdHWND_Return(OLDHWND);
END;

PROCEDURE KeybdSendKey(Wnd,VK:DWORD; Ctrl,Alt,Shift:BOOLEAN);
VAR
  MC,MA,MS:BOOLEAN;
BEGIN
  // Try to bring target window to foreground
  ShowWindow(Wnd,SW_SHOW);
  SetForegroundWindow(Wnd);

  // Get current state of modifier keys
  MC:=Hi(GetAsyncKeyState(VK_CONTROL))>127;
  MA:=Hi(GetAsyncKeyState(VK_MENU))>127;
  MS:=Hi(GetAsyncKeyState(VK_SHIFT))>127;

  // Press modifier keys if necessary (unless already pressed by real user)
  IF Ctrl<>MC THEN keybd_event(VK_CONTROL,0,Byte(MC)*KEYEVENTF_KEYUP,0);
  IF Alt<>MA THEN keybd_event(VK_MENU,0,Byte(MA)*KEYEVENTF_KEYUP,0);
  IF Shift<>MS THEN keybd_event(VK_SHIFT,0,Byte(MS)*KEYEVENTF_KEYUP,0);

  // Press key
  keybd_event(VK,0,KEYEVENTF_KEYDOWN,0);
  keybd_event(VK,0,KEYEVENTF_KEYUP,0);

  // Release modifier keys if necessary
  IF Ctrl<>MC THEN keybd_event(VK_CONTROL,0,Byte(Ctrl)*KEYEVENTF_KEYUP,0);
  IF Alt<>MA THEN keybd_event(VK_MENU,0,Byte(Alt)*KEYEVENTF_KEYUP,0);
  IF Shift<>MS THEN keybd_event(VK_SHIFT,0,Byte(Shift)*KEYEVENTF_KEYUP,0);
END;

PROCEDURE KeybdInit(KeysMAX:WORD);
BEGIN
  KeyInputsMAX := KeysMAX;
  SetLength(KeyInputs, KeysMAX);
END;




FUNCTION processExists(exeFileName: string): THandle;
var
  ContinueLoop: BOOL;
  FSnapshotHandle: THandle;
  FProcessEntry32: TProcessEntry32;
begin
  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  FProcessEntry32.dwSize := SizeOf(FProcessEntry32);
  ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);
  Result := 0;//False;
  while Integer(ContinueLoop) <> 0 do
  begin
    if ((UpperCase(ExtractFileName(FProcessEntry32.szExeFile)) =
      UpperCase(ExeFileName)) or (UpperCase(FProcessEntry32.szExeFile) =
      UpperCase(ExeFileName))) then
    begin
      Result := FProcessEntry32.th32ProcessID;
      //Result := True;
    end;
    ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
  end;
  CloseHandle(FSnapshotHandle);
end;


function GetHWndByPID(const hPID: THandle): THandle;
type
    PEnumInfo = ^TEnumInfo;
    TEnumInfo = record
    ProcessID: DWORD;
    HWND: THandle;
    end;

    function EnumWindowsProc(Wnd: DWORD; var EI: TEnumInfo): Bool; stdcall;
    var
    PID: DWORD;
    begin
    GetWindowThreadProcessID(Wnd, @PID);
    Result := (PID <> EI.ProcessID) or
        (not IsWindowVisible(WND)) or
        (not IsWindowEnabled(WND));

    if not Result then EI.HWND := WND;
    end;

    function FindMainWindow(PID: DWORD): DWORD;
    var
    EI: TEnumInfo;
    begin
    EI.ProcessID := PID;
    EI.HWND := 0;
    EnumWindows(@EnumWindowsProc, Integer(@EI));
    Result := EI.HWND;
    end;
begin
    if hPID<>0 then
    Result:=FindMainWindow(hPID)
    else
    Result:=0;
end;



BEGIN
    KeybdInit(1024);
END.
