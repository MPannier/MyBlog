object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Log Files IO Test'
  ClientHeight = 289
  ClientWidth = 554
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object LabelStatus: TLabel
    Left = 8
    Top = 102
    Width = 56
    Height = 13
    Caption = 'LabelStatus'
  end
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 8
    Width = 321
    Height = 57
    Caption = 'Methode'
    Columns = 3
    ItemIndex = 0
    Items.Strings = (
      'TStringList'
      'TFileStream'
      'TextFile')
    TabOrder = 0
  end
  object ButtonStart: TButton
    Left = 8
    Top = 71
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 1
    OnClick = ButtonStartClick
  end
  object ButtonRemoveLastLine: TButton
    Left = 176
    Top = 71
    Width = 153
    Height = 25
    Caption = 'Remove Last Line'
    TabOrder = 2
    OnClick = ButtonRemoveLastLineClick
  end
end
