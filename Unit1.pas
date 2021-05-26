unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  rbSendKeys, rbGamePad, RobPort, RbUtil,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, Buttons;

type
  TForm1 = class(TForm)
    Timer1: TTimer;
    ButtonGetHWND: TButton;
    LabelHWND: TLabel;
    TimerFindHWND: TTimer;
    PanelPadInfo: TPanel;
    LabelPadInfo0: TLabel;
    LabelPadInfo1: TLabel;
    LabelPadInfo2: TLabel;
    LabelPadInfo3: TLabel;
    PanelBind: TPanel;
    CheckBoxEventMode: TCheckBox;
    PanelTestKey: TPanel;
    SpeedButton2: TSpeedButton;
    SpeedButtonSave: TSpeedButton;
    SpeedButtonLoad: TSpeedButton;
    SpeedButtonDefault: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Timer1Timer(Sender: TObject);
    procedure ButtonGetHWNDClick(Sender: TObject);
    procedure TimerFindHWNDTimer(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButtonLoadClick(Sender: TObject);
    procedure SpeedButtonSaveClick(Sender: TObject);
    procedure SpeedButtonDefaultClick(Sender: TObject);
  private
    { Private declarations }
    PROCEDURE LAB_ComboBoxEventKEY(Sender: TObject);
    PROCEDURE LAB_ComboBoxEventPAD(Sender: TObject);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

VAR isFindHWND:BOOLEAN=FALSE;
    FindHWND:HWND;
    WorkHWND:HWND=0;

//////////////////////////////////////////////////
//////////////////////////////////////////////////
//////////////////////////////////////////////////

VAR DefFontName:STRING = 'Times New Roman';//'Courier New';//'Terminal';//'System';//'Verdana';//'Arial';//
    DefFontSize:DWORD = 9;


CONST LAB_MAX=20;

TYPE T_LABA=PACKED RECORD
      NUM       :DWORD;
      TMR       :INT64;
      PAN       :TPanel;
      COMBOKEY  :TComboBox;
      COMBOPAD  :TComboBox;
      LABNAME   :TLabel;
//      BTNFILE   :TSpeedButton;
//      STATSHAPE :TShape;
      STATUS    :DWORD;
     END;

VAR LABAR:ARRAY[0..LAB_MAX-1] OF T_LABA;


//////////////////////////////////////////////////
//////////////////////////////////////////////////
//////////////////////////////////////////////////

PROCEDURE JoyBindingKey(EVFLAG:DWORD);
VAR N,IK,IP:LONGINT;
    ID:DWORD;
    VK:DWORD;
    EID:DWORD;
    PL:TTGamePadLST;
    KL:TTKeyLST;
    isAxis:BOOLEAN;
    isPressed:BOOLEAN;
BEGIN
    IF ((EVFLAG AND RBFLAG_MOVE)<>0) THEN BEGIN
     isAxis := TRUE;
     isPressed := FALSE;
    END ELSE BEGIN
     isAxis := FALSE;
     IF ((EVFLAG AND RBFLAG_PRESSDOWN)<>0) THEN BEGIN
      isPressed := TRUE;
     END ELSE BEGIN
      isPressed := FALSE;
     END;

    END;

    EID := EVFLAG AND RBFL_EVENT_MASK;
    FOR N:=0 TO LAB_MAX-1 DO BEGIN
     IK:=LABAR[N].COMBOKEY.ItemIndex;
     IP:=LABAR[N].COMBOPAD.ItemIndex;
     KL:=TTKeyLST(    LABAR[N].COMBOKEY.Items.Objects[IK]);
     PL:=TTGamePadLST(LABAR[N].COMBOPAD.Items.Objects[IP]);
     ID:=PL.PADID;
     VK:=KL.VKKEY;
     IF (ID<>RBFL_NOTDEFINED) AND (VK<>VK_NOTDEFINED) THEN BEGIN
      IF (ID=EID) THEN BEGIN
       IF (isAxis) THEN BEGIN
        muStartTimerWait(LABAR[N].TMR, 100*1000);
        LABAR[N].PAN.Color := RGB($1F,$2F,$FF);
       END ELSE BEGIN
        KeybdPutKey(WorkHWND, VK, isPressed, 1);
        muStartTimerWait(LABAR[N].TMR, 100*1000);
        LABAR[N].PAN.Color := RGB($CF,$BF,$DF);
       END;
      END;
     END;
    END;
END;


PROCEDURE JoyEvent(J:DWORD; VAR GPD:TRBGamePadWork; EVFLAG:DWORD); STDCALL;
VAR isPress:BOOLEAN;
BEGIN
    IF ((EVFLAG AND RBFLAG_PRESSDOWN)<>0) THEN BEGIN
     isPress:=TRUE;
    END ELSE BEGIN
     isPress:=FALSE;
    END;

    JoyBindingKey(EVFLAG);

    IF (GPD.PAD[J].DIF.BUTJ1.EV<>0) AND (GPD.PAD[J].DIF.BUTJ2.EV<>0) THEN BEGIN
     IF (GPD.PAD[J].DIF.BUTJ1.FLAG<>0) AND (GPD.PAD[J].DIF.BUTJ2.FLAG<>0) THEN BEGIN
      GPD.PAD[J].DIF.BUTJ1.FLAG := 0;
      GPD.PAD[J].DIF.BUTJ2.FLAG := 0;
      IF isFindHWND THEN BEGIN
       BEEP();
       WorkHWND:=FindHWND;
       isFindHWND:=FALSE;
      END;
     END;
    END;


END;

//////////////////////////////////////////////////
//////////////////////////////////////////////////
//////////////////////////////////////////////////

PROCEDURE KeyComboBoxSelect(CMB:TComboBox; VK:DWORD);
VAR N:LONGINT;
    KL:TTKeyLST;
BEGIN
    FOR N:=0 TO CMB.Items.Count-1 DO BEGIN
     KL:=TTKeyLST(CMB.Items.Objects[N]);
     IF (KL^.VKKEY=VK) THEN BEGIN
      CMB.ItemIndex := N;
      BREAK;
     END;
    END;
END;

PROCEDURE KeyComboBoxCreate(CMB:TComboBox; DEFVK:DWORD=VK_NOTDEFINED);
VAR N:LONGINT;
    SP:^STRING;
BEGIN
    CMB.Clear();
    FOR N:=0 TO KeybdVK_LST_COUNT()-1 DO BEGIN
     SP:=@Key_VK_List[N].NAME;
     CMB.Items.AddObject(SP^, TObject(@Key_VK_List[N]));
    END;
    KeyComboBoxSelect(CMB, DEFVK);
END;

//////////////////////////////////////////////////
//////////////////////////////////////////////////
//////////////////////////////////////////////////

PROCEDURE PadComboBoxSelect(CMB:TComboBox; ID:DWORD);
VAR N:LONGINT;
    PL:TTGamePadLST;
BEGIN
    FOR N:=0 TO CMB.Items.Count-1 DO BEGIN
     PL:=TTGamePadLST(CMB.Items.Objects[N]);
     IF (PL^.PADID=ID) THEN BEGIN
      CMB.ItemIndex := N;
      BREAK;
     END;
    END;
END;

PROCEDURE PadComboBoxCreate(CMB:TComboBox; DEFID:DWORD=RBFL_NOTDEFINED);
VAR N:LONGINT;
    SP:^STRING;
BEGIN
    CMB.Clear();
    FOR N:=0 TO RBGamePad_LST_COUNT()-1 DO BEGIN
     SP:=@GamePad_Key_List[N].NAME;
     CMB.Items.AddObject(SP^, TObject(@GamePad_Key_List[N]));
    END;
    PadComboBoxSelect(CMB, DEFID);
END;

//////////////////////////////////////////////////
//////////////////////////////////////////////////
//////////////////////////////////////////////////


PROCEDURE LAB_ResetValue;
VAR N:LONGINT;
BEGIN
       FOR N:=0 TO LAB_MAX-1 DO BEGIN
        PadComboBoxCreate(LABAR[N].COMBOPAD);
        KeyComboBoxCreate(LABAR[N].COMBOKEY);
       END;
END;



VAR SHOWTIMER_VAR:INT64=0;

PROCEDURE LAB_ShowRefresh(NUM:LONGINT=-1);
VAR N,M,I:LONGINT;
    LABA:^T_LABA;
    S:ShortString;
    D,DD:DWORD;
    tms:DWORD;
BEGIN

      IF SHOWTIMER_VAR=0 THEN BEGIN
       muStartTimerWait(SHOWTIMER_VAR, 0);
      END;
      tms := muGetTimerWait(SHOWTIMER_VAR) DIV -1000;

      N:=0;
      M:=LAB_MAX;
      IF NUM<>-1 THEN BEGIN
       N:=NUM;
       M:=N+1;
      END;
      WHILE N<M DO BEGIN
       LABA := @LABAR[N];
       LABA.LABNAME.Visible := TRUE;
       //DD:=RANDOM($10);
//       IF (HD_Parser_Timer_1ms_Counter MOD 600)>300 THEN BEGIN
       IF (tms MOD 600)>300 THEN BEGIN
        DD:=$00;
       END ELSE BEGIN
        DD:=$40;
       END;

       IF (LABA.TMR<>0) THEN BEGIN
        IF ( muCheckTimerWait(LABA.TMR) ) THEN BEGIN
         LABA.TMR:=0;
        END ELSE BEGIN

        END;
       END ELSE BEGIN
        LABA.PAN.Color := Form1.PanelBind.Color;
       END;

       INC(N);
      END;

END;



PROCEDURE TForm1.LAB_ComboBoxEventKEY(Sender: TObject);
VAR LABA:^T_LABA;
BEGIN
      LABA := TPanel(TControl(Sender).Parent).VCLComObject;
END;

PROCEDURE TForm1.LAB_ComboBoxEventPAD(Sender: TObject);
VAR LABA:^T_LABA;
BEGIN
      LABA := TPanel(TControl(Sender).Parent).VCLComObject;
END;


PROCEDURE UpdateFontParameter(FNT:TFont; SIZ:DWORD; NAME:ShortString; isBold:BOOLEAN=FALSE);
BEGIN
    FNT.Size := SIZ;
//    FNT.Pitch := 1;
    FNT.Name := NAME;//'Lucida Console';//'Arial';//'Courier New';
    FNT.Charset := OEM_CHARSET;//RUSSIAN_CHARSET;//SYMBOL_CHARSET;//OEM_CHARSET;//RUSSIAN_CHARSET; //OEM_CHARSET; //
    IF isBold THEN BEGIN
     FNT.Style := [fsBold];
    END ELSE BEGIN
     FNT.Style := [];
    END;
END;



PROCEDURE LAB_Delete(VAR LAB:T_LABA);
BEGIN
//       LAB.STATSHAPE.Free;
       LAB.COMBOKEY.Free;
       LAB.COMBOPAD.Free;
//       LAB.BTNFILE.Free;
       LAB.LABNAME.Free;
       LAB.PAN.Free;
END;



PROCEDURE LAB_Create(VAR LAB:T_LABA);
VAR HE:DWORD;
    WI:DWORD;
    LAB_PAN_YS:DWORD;
BEGIN
      WITH Form1 DO BEGIN

       LAB_PAN_YS:=ROUND(PanelBind.Height/LAB_MAX);
       PanelBind.Height := LAB_PAN_YS*LAB_MAX;

      //Create + Parent
       LAB.PAN := TPanel.Create(PanelBind);
       LAB.PAN.Parent := PanelBind;
//       LAB.STATSHAPE := TShape.Create(LAB.PAN);
//       LAB.STATSHAPE.Parent := LAB.PAN;
       LAB.COMBOKEY := TComboBox.Create(LAB.PAN);
       LAB.COMBOKEY.Parent := LAB.PAN;
       LAB.COMBOPAD := TComboBox.Create(LAB.PAN);
       LAB.COMBOPAD.Parent := LAB.PAN;
//       LAB.BTNFILE := TSpeedButton.Create(PanelBind);
//       LAB.BTNFILE.Parent := LAB.PAN;
       LAB.LABNAME := TLabel.Create(PanelBind);
       LAB.LABNAME.Parent := LAB.PAN;

      //Init
       LAB.PAN.Top := LAB.NUM * LAB_PAN_YS;
       LAB.PAN.Left := 0;
       LAB.PAN.Width := PanelBind.ClientWidth;
       LAB.PAN.ClientHeight := LAB_PAN_YS;
       LAB.PAN.Caption := '';
       LAB.PAN.VCLComObject := (@LAB);


//       LAB.COMBOKEY.Height := 6;//LAB_PAN_YS;
//       LAB.COMBOPAD.Height := 6;//
       UpdateFontParameter(LAB.PAN.Font, DefFontSize, DefFontName);
       UpdateFontParameter(LAB.COMBOKEY.Font, DefFontSize, DefFontName, TRUE);
       UpdateFontParameter(LAB.COMBOPAD.Font, DefFontSize, DefFontName, TRUE);

       HE := LAB.COMBOKEY.ClientHeight;
       WI := LAB.PAN.ClientWidth;

      //Init
       LAB.COMBOKEY.ClientWidth := ROUND(WI*0.30);
       LAB.COMBOKEY.Top := ROUND( (LAB.PAN.ClientHeight - HE)/2 );
       LAB.COMBOKEY.Left := LAB.PAN.Width - LAB.COMBOKEY.ClientWidth - 4;
       LAB.COMBOKEY.Style := csDropDownList;
       LAB.COMBOKEY.Tag := 1;
       KeyComboBoxCreate(LAB.COMBOKEY); //LAB.COMBOKEY.ItemIndex := 0;
       LAB.COMBOKEY.OnChange := LAB_ComboBoxEventKEY;

      //Init
       LAB.COMBOPAD.ClientWidth := ROUND(WI*0.40);
       LAB.COMBOPAD.Top := ROUND( (LAB.PAN.ClientHeight - HE)/2 );
       LAB.COMBOPAD.Left := LAB.COMBOKEY.Left - LAB.COMBOPAD.ClientWidth - 8;
       LAB.COMBOPAD.Style := csDropDownList;//csOwnerDrawFixed;
       LAB.COMBOPAD.Tag := 1;
       PadComboBoxCreate(LAB.COMBOPAD); //LAB.COMBOPAD.ItemIndex := 0;
       LAB.COMBOPAD.OnChange := LAB_ComboBoxEventPAD;

      //Init
       LAB.LABNAME.Top := ((LAB.PAN.Height-LAB.LABNAME.Canvas.TextHeight('12')) DIV 2);
       LAB.LABNAME.Left := 4;
       LAB.LABNAME.AutoSize := TRUE;
       LAB.LABNAME.Caption := '';//'Bind Key #'+IntToStr(LAB.NUM);

      END;

END;

PROCEDURE LAB_Init;
VAR N,M,I:LONGINT;
    S:ShortString;
BEGIN

      FOR N:=0 TO LAB_MAX-1 DO BEGIN
       LABAR[N].NUM := N;
       LABAR[N].STATUS := 0;
       LAB_Create(LABAR[N]);
       S:='#'+IntToStr(N);//ConfigLoadELE(N);
{       IF (S<>'') AND (S[2]=',') THEN BEGIN
        I:=BYTE(S[1])-48;
        LABAR[N].COMBOKEY.ItemIndex := I;
        DELETE (S,1,2);
        LABAR[N].LABNAME.Caption := S;
       END;}
       LABAR[N].LABNAME.Caption := 'Bind '+S;
       LABAR[N].TMR := 0;
      END;

      LAB_ShowRefresh();

END;



//////////////////////////////////////////////////
//////////////////////////////////////////////////
//////////////////////////////////////////////////

CONST FILEEXTNamePAD = 'GamePadConfig|*.gpadCFG';

FUNCTION ConfigFileSelect(fiext:STRING; isLoad:BOOLEAN=TRUE):STRING;
VAR LAB_openDialog : TOpenDialog;    // OpenDialog
    LAB_saveDialog : TSaveDialog;    // SaveDialog
    SEXT:STRING;
BEGIN

      IF (isLoad) THEN BEGIN
       LAB_openDialog := TOpenDialog.Create(Form1);
       LAB_openDialog.InitialDir := GetCurrentDir;
       LAB_openDialog.Options := [ofPathMustExist, ofFileMustExist];
       LAB_openDialog.Filter := fiext;
       LAB_openDialog.FilterIndex := 0;
       IF LAB_openDialog.Execute THEN BEGIN
        RESULT := LAB_openDialog.FileName;
       END ELSE BEGIN
        RESULT := '';
       END;
       LAB_openDialog.Free;
      END ELSE BEGIN

       LAB_saveDialog := TSaveDialog.Create(Form1);
       LAB_saveDialog.InitialDir := GetCurrentDir;
       LAB_saveDialog.Options := [];
       LAB_saveDialog.Filter := fiext;
       LAB_saveDialog.FilterIndex := 0;
       IF LAB_saveDialog.Execute THEN BEGIN
        RESULT := LAB_saveDialog.FileName;
        SEXT := ExtractFileExt(RESULT);
        IF SEXT='' THEN BEGIN
         SEXT:=ExtractFileExt(fiext);
         RESULT:=RESULT+SEXT;
        END;
       END ELSE BEGIN
        RESULT := '';
       END;
       LAB_saveDialog.Free;
      END;



END;

PROCEDURE ConfigLoad;
VAR N,IK,IP:LONGINT;
    PL:TTGamePadLST;
    KL:TTKeyLST;
    FNAM:STRING;
    S,S0,S1,S2:STRING;
    V1,V2:DWORD;
    T:TextFile;
BEGIN
      FNAM := ConfigFileSelect(FILEEXTNamePAD, TRUE);
      IF FNAM<>'' THEN BEGIN
       AssignFile(T,FNAM);
       Reset(T);
       LAB_ResetValue();
       WHILE (NOT EOF(T)) DO BEGIN
        ReadLn(T,S);
        WHILE (LENGTH(S)<>0) AND (S[1]=' ') DO DELETE(S,1,1);
        IF (S<>'') AND (S<>';') THEN BEGIN
         S0:=GetEle(S,'|',0);
         S1:=GetEle(S,'|',1);
         S2:=GetEle(S,'|',2);
         N:=HexToInt(S0);
         IF N<LAB_MAX THEN BEGIN
          V1:=HexToInt(S1);
          V2:=HexToInt(S2);
          PadComboBoxSelect(LABAR[N].COMBOPAD,V1);
          KeyComboBoxSelect(LABAR[N].COMBOKEY,V2);
         END;
        END;
       //GetEle(
       END;
       CloseFile(T);
      END;
      //Form1.Caption := FNAM;
END;

PROCEDURE ConfigSave;
VAR N,IK,IP:LONGINT;
    PL:TTGamePadLST;
    KL:TTKeyLST;
    FNAM:STRING;
    S:STRING;
    T:TextFile;
BEGIN
      FNAM := ConfigFileSelect(FILEEXTNamePAD, FALSE);
      IF FNAM<>'' THEN BEGIN
       AssignFile(T,FNAM);
       Rewrite(T);

       FOR N:=0 TO LAB_MAX-1 DO BEGIN

        IK:=LABAR[N].COMBOKEY.ItemIndex;
        IP:=LABAR[N].COMBOPAD.ItemIndex;
        KL:=TTKeyLST(    LABAR[N].COMBOKEY.Items.Objects[IK]);
        PL:=TTGamePadLST(LABAR[N].COMBOPAD.Items.Objects[IP]);

        S:= IntToHex(N       ,4)   + ' | ' +
            IntToHex(PL.PADID,8)   + ' | ' +
            IntToHex(KL.VKKEY,8)   +
           '';
        WriteLn(T,S);
       END;

       CloseFile(T);
      END;
      //Form1.Caption := FNAM;

END;

PROCEDURE ConfigSetVAL(N:LONGINT; VPAD,VKEY:DWORD);
BEGIN
          PadComboBoxSelect(LABAR[N].COMBOPAD, VPAD);
          KeyComboBoxSelect(LABAR[N].COMBOKEY, VKEY);
END;

PROCEDURE ConfigDefault;
BEGIN
      LAB_ResetValue();

      ConfigSetVAL(13, RBFL_BUTA,      VK_RETURN    );
      ConfigSetVAL(14, RBFL_BUTA,      VK_SPACE     );
      ConfigSetVAL(15, RBFL_BUTB,      VK_ESCAPE    );
      ConfigSetVAL(16, RBFL_BUTPOVUP,  VK_W         );
      ConfigSetVAL(17, RBFL_BUTPOVDO,  VK_S         );
      ConfigSetVAL(18, RBFL_BUTPOVLE,  VK_A         );
      ConfigSetVAL(19, RBFL_BUTPOVRI,  VK_D         );

END;

//////////////////////////////////////////////////
//////////////////////////////////////////////////
//////////////////////////////////////////////////

function GetMouseHandle: HWND;
begin
  Result := WindowFromPoint(Mouse.CursorPos);
end;







procedure TForm1.FormCreate(Sender: TObject);
begin
    DefFontName := FORM1.Font.Name;

    LabelPadInfo1.Top   := LabelPadInfo0.Top;
    LabelPadInfo1.Left  := PanelPadInfo.ClientWidth DIV 2;

    LabelPadInfo2.Top   := LabelPadInfo0.Top + (PanelPadInfo.ClientHeight-LabelPadInfo0.Top) DIV 2;
    LabelPadInfo2.Left  := LabelPadInfo0.Left;

    LabelPadInfo3.Top   := LabelPadInfo2.Top;
    LabelPadInfo3.Left  := LabelPadInfo1.Left;

    UpdateFontParameter(FORM1.Font, DefFontSize, DefFontName);

    LAB_Init();

    RBGamePadInit(ROUND(1000/50));
    GMPadWork.PAD[0].EventP := @JoyEvent;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    RBGamePadClose();

end;

FUNCTION SEQU(A,B:DWORD; ST,SF:STRING):STRING;
BEGIN
  IF A=B THEN BEGIN
   RESULT:=ST;
  END ELSE BEGIN
   RESULT:=SF;
  END;
END;


FUNCTION MakeGamePadStateString(J:INTEGER; isEventMode:BOOLEAN):STRING;
BEGIN
      RESULT:='';
      RESULT:=RESULT + '   PAD #'+IntToStr(J) + ' ' + SEQU(BYTE(GMPadWork.PAD[J].Present), BYTE(TRUE), 'ACTIVE','NONE');
      IF ( GMPadWork.PAD[J].Present ) THEN BEGIN
       IF ( NOT isEventMode ) THEN BEGIN
        RESULT:=RESULT + #13 +
         'BUTTONS ........ '+IntToHex(GMPadWork.PAD[J].FJoyInfo.wButtons,4)             + #13 +
         'XP ............. '+IntToStr(GMPadWork.PAD[J].FJoyInfo.wXpos)                  + #13 +
         'YP ............. '+IntToStr(GMPadWork.PAD[J].FJoyInfo.wYpos)                  + #13 +
         'ZP ............. '+IntToStr(GMPadWork.PAD[J].FJoyInfo.wZpos)                  + #13 +
         'XQ ............. '+IntToStr(GMPadWork.PAD[J].FJoyInfo.dwUpos)                 + #13 +
         'YQ ............. '+IntToStr(GMPadWork.PAD[J].FJoyInfo.dwRpos)                 + #13 +
         'ZQ ............. '+IntToStr(GMPadWork.PAD[J].FJoyInfo.dwVpos)                 + #13 +
         'POV ............ '+IntToStr(GMPadWork.PAD[J].FJoyInfo.dwPOV)                  + #13 +
        '';
       END ELSE BEGIN
        RESULT:=RESULT + #13 +
         'BUTTONS ........ '+IntToHex(GMPadWork.PAD[J].FJoyInfo.wButtons,4)             + #13 +
         'XP ............. '+IntToStr(GMPadWork.PAD[J].DIF.XP.EV)                       + #13 +
         'YP ............. '+IntToStr(GMPadWork.PAD[J].DIF.YP.EV)                       + #13 +
         'ZP ............. '+IntToStr(GMPadWork.PAD[J].DIF.ZP.EV)                       + #13 +
         'XQ ............. '+IntToStr(GMPadWork.PAD[J].DIF.XQ.EV)                       + #13 +
         'YQ ............. '+IntToStr(GMPadWork.PAD[J].DIF.YQ.EV)                       + #13 +
         'ZQ ............. '+IntToStr(GMPadWork.PAD[J].DIF.ZQ.EV)                       + #13 +
         'ButA/B/X/Y ..... '+IntToStr(GMPadWork.PAD[J].DIF.BUTA.EV)                     +
                             IntToStr(GMPadWork.PAD[J].DIF.BUTB.EV)                     +
                             IntToStr(GMPadWork.PAD[J].DIF.BUTX.EV)                     +
                             IntToStr(GMPadWork.PAD[J].DIF.BUTY.EV)                     + #13 +
         'ButBACK/START .. '+IntToStr(GMPadWork.PAD[J].DIF.BUTBA.EV)                    +
                             IntToStr(GMPadWork.PAD[J].DIF.BUTST.EV)                    + #13 +
         'ButLB/RB ....... '+IntToStr(GMPadWork.PAD[J].DIF.BUTLB.EV)                    +
                             IntToStr(GMPadWork.PAD[J].DIF.BUTRB.EV)                    + #13 +
         'ButLT/RT ....... '+IntToStr(GMPadWork.PAD[J].DIF.BUTLT.EV)                    +
                             IntToStr(GMPadWork.PAD[J].DIF.BUTRT.EV)                    + #13 +
         'ButPOV(URDL) ... '+IntToStr(GMPadWork.PAD[J].DIF.BUTPOVUP.EV)                 +
                             IntToStr(GMPadWork.PAD[J].DIF.BUTPOVRI.EV)                 +
                             IntToStr(GMPadWork.PAD[J].DIF.BUTPOVDO.EV)                 +
                             IntToStr(GMPadWork.PAD[J].DIF.BUTPOVLE.EV)                 + #13 +
         'ButJ1/J2 ....... '+IntToStr(GMPadWork.PAD[J].DIF.BUTJ1.EV)                    +
                             IntToStr(GMPadWork.PAD[J].DIF.BUTJ2.EV)                    + #13 +
        '';
       END;
        RESULT:=RESULT + 
        'EvCNT .......... '+IntToStr(GMPadWork.PAD[J].EventCounter)                    + #13 +
        '';
      END;


END;


procedure TForm1.Timer1Timer(Sender: TObject);
VAR isEventMode:BOOLEAN;
begin
    isEventMode := CheckBoxEventMode.Checked;
    LabelPadInfo0.Caption := MakeGamePadStateString(0, isEventMode);
    LabelPadInfo1.Caption := MakeGamePadStateString(1, isEventMode);
    LabelPadInfo2.Caption := MakeGamePadStateString(2, isEventMode);
    LabelPadInfo3.Caption := MakeGamePadStateString(3, isEventMode);
    LAB_ShowRefresh();
end;

const KEYEVENTF_KEYDOWN = 0;

procedure TForm1.ButtonGetHWNDClick(Sender: TObject);
begin
  IF isFindHWND THEN BEGIN
   WorkHWND:=FindHWND;
   isFindHWND:=FALSE;
  END ELSE BEGIN
   isFindHWND:=TRUE;
  END;
end;

procedure TForm1.TimerFindHWNDTimer(Sender: TObject);
begin
  IF isFindHWND THEN BEGIN
   FindHWND:=GetMouseHandle();
   LabelHWND.Caption := 'HWND Under Cursor = '+IntToHex(FindHWND, 8);
   LabelHWND.Color := RGB (255,0,0);
  END ELSE BEGIN
   LabelHWND.Caption := 'HWND Work = '+IntToHex(WorkHWND, 8);
   LabelHWND.Color := clBtnFace;
  END;
end;

procedure TForm1.SpeedButton2Click(Sender: TObject);
begin

      KeybdInputAddKeysStr('Hello World ... ');
      KeybdInputEnd(WorkHWND, 5);

end;



















procedure TForm1.SpeedButtonLoadClick(Sender: TObject);
begin
      ConfigLoad();
end;

procedure TForm1.SpeedButtonSaveClick(Sender: TObject);
begin
      ConfigSave();
end;

procedure TForm1.SpeedButtonDefaultClick(Sender: TObject);
begin
      ConfigDefault();
end;

end.
