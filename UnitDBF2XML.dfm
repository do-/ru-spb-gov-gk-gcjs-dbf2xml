object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = #1050#1086#1085#1074#1077#1088#1090#1077#1088' DBF '#1074' XML '#1076#1083#1103' '#1043#1062#1046#1057
  ClientHeight = 43
  ClientWidth = 480
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Button: TButton
    Left = 8
    Top = 8
    Width = 113
    Height = 25
    Caption = #1042#1099#1073#1088#1072#1090#1100' '#1092#1072#1081#1083'...'
    TabOrder = 0
    OnClick = ButtonClick
  end
  object ProgressBar: TProgressBar
    Left = 127
    Top = 8
    Width = 345
    Height = 25
    Enabled = False
    TabOrder = 1
  end
  object OpenDialog: TOpenDialog
    Filter = 'DBF|*.dbf'
    Left = 280
    Top = 8
  end
  object Dbf: TDbf
    IndexDefs = <>
    TableLevel = 5
    AfterOpen = DbfAfterOpen
    AfterScroll = DbfAfterScroll
    Left = 344
    Top = 8
  end
end
