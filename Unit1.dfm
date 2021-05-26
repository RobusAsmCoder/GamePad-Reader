object Form1: TForm1
  Left = 192
  Top = 124
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'GamePad Reader By Rob F. ...'
  ClientHeight = 517
  ClientWidth = 749
  Color = clBtnFace
  Font.Charset = OEM_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Terminal'
  Font.Style = []
  Icon.Data = {
    000001000200101010000000000028010000260000002020100000000000E802
    00004E0100002800000010000000200000000100040000000000C00000000000
    0000000000001000000000000000000000000000800000800000008080008000
    00008000800080800000C0C0C000808080000000FF0000FF000000FFFF00FF00
    0000FF00FF00FFFF0000FFFFFF000877C4C4048800000CF7C4C4048000008777
    C4C44800000077F7C48444000000FF7FCC4000080000FFF7CCC448000000FFF7
    77C4440000007CCC888800000000CCCCC88880000000C77CC888800000007888
    84888000000070190488800009108099C44880000990777CC44480000000FF7C
    4444480000007F7CCC44440000000000FFFF0000FFFF0000FFFF0000FFFF0000
    FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000
    FFFF0000FFFF0000FFFF0000FFFF280000002000000040000000010004000000
    0000800200000000000000000000100000000000000000000000000080000080
    000000808000800000008000800080800000C0C0C000808080000000FF0000FF
    000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000C777CCCC40440084
    4800000000000004C7C7CCCC44C4000448000000000000047C77CCC444C48004
    480400000000004C7777CC4C48C4400448040000000004C7C777CCC448C44484
    4004000000004C7C777F7C4C447C444480040000000077C7F7F77CC4447C444C
    400400000000C77F7F7F7CCC40CC4004C4040000000077F7F7F77C7CC4400000
    0004000000007F7F7F7F77CC4C44044000000000000077FFFFF77C7CCC4C4444
    000000000000FFFFFFFF77CCC4C4488C400000000000FFFFFFFF7C7CCC44400C
    400000000000FFFFF777CCCC44000000000000000000777CCC44488888888800
    000000000000C7C7C7CCC44888888880000000000000CC777CCCCCC888888880
    000000000000C77777C7CCCC88888880000000000000CCCCCC4C44CCC8888880
    000000000000C7C7C77CC480488888800000000000007FF77787C4440C888880
    0000000000007F780000004408888880000000000000F7800111004404888880
    000000001100F700099990CC04888880000000099910770009998CC404488880
    000000099910F7008877CCCC044488800000000009107F77FF77CCC404448880
    0000000000007FFFF77CC4C04C444480040000000000CCF777CCCC44C44C4444
    0000000000007F7F777CC4CC4C444440040000000000FFFFF77CCCC4C44C4444
    00000000000077777CCCCCCC4C44444444000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    00000000000000000000000000000000000000000000}
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 12
  object LabelHWND: TLabel
    Left = 260
    Top = 12
    Width = 45
    Height = 12
    Caption = 'NEED READ'
  end
  object PanelBind: TPanel
    Left = 4
    Top = 32
    Width = 369
    Height = 449
    TabOrder = 2
  end
  object PanelPadInfo: TPanel
    Left = 376
    Top = 32
    Width = 369
    Height = 449
    TabOrder = 1
    object LabelPadInfo0: TLabel
      Left = 4
      Top = 24
      Width = 65
      Height = 12
      Caption = 'LabelPadInfo0'
    end
    object LabelPadInfo1: TLabel
      Left = 4
      Top = 52
      Width = 65
      Height = 12
      Caption = 'LabelPadInfo1'
    end
    object LabelPadInfo2: TLabel
      Left = 4
      Top = 72
      Width = 65
      Height = 12
      Caption = 'LabelPadInfo2'
    end
    object LabelPadInfo3: TLabel
      Left = 4
      Top = 92
      Width = 65
      Height = 12
      Caption = 'LabelPadInfo3'
    end
    object CheckBoxEventMode: TCheckBox
      Left = 4
      Top = 4
      Width = 97
      Height = 17
      TabStop = False
      Caption = 'Show Event Mode'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
  end
  object ButtonGetHWND: TButton
    Left = 4
    Top = 4
    Width = 249
    Height = 25
    Caption = 'Read HWND From Cursor'
    TabOrder = 0
    OnClick = ButtonGetHWNDClick
  end
  object PanelTestKey: TPanel
    Left = 4
    Top = 484
    Width = 741
    Height = 29
    TabOrder = 3
    object SpeedButton2: TSpeedButton
      Left = 536
      Top = 4
      Width = 201
      Height = 21
      Caption = 'Send "Hello World ..."'
      OnClick = SpeedButton2Click
    end
    object SpeedButtonSave: TSpeedButton
      Left = 116
      Top = 4
      Width = 125
      Height = 21
      Caption = 'Save'
      OnClick = SpeedButtonSaveClick
    end
    object SpeedButtonLoad: TSpeedButton
      Left = 244
      Top = 4
      Width = 125
      Height = 21
      Caption = 'Load'
      OnClick = SpeedButtonLoadClick
    end
    object SpeedButtonDefault: TSpeedButton
      Left = 4
      Top = 4
      Width = 77
      Height = 21
      Caption = 'Default'
      OnClick = SpeedButtonDefaultClick
    end
  end
  object Timer1: TTimer
    Interval = 50
    OnTimer = Timer1Timer
    Left = 340
    Top = 4
  end
  object TimerFindHWND: TTimer
    Interval = 100
    OnTimer = TimerFindHWNDTimer
    Left = 308
    Top = 4
  end
end
