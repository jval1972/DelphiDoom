object frmGotoLine: TfrmGotoLine
  Left = 617
  Top = 426
  BorderStyle = bsDialog
  Caption = 'Go to Line Number'
  ClientHeight = 76
  ClientWidth = 356
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lblLineNumber: TLabel
    Left = 16
    Top = 14
    Width = 60
    Height = 13
    Caption = 'Line Number'
  end
  object lblCharNumber: TLabel
    Left = 16
    Top = 47
    Width = 75
    Height = 13
    Caption = 'Column Number'
  end
  object edtCharNumber: TEdit
    Left = 120
    Top = 43
    Width = 121
    Height = 21
    TabOrder = 1
  end
  object edtLineNumber: TEdit
    Left = 120
    Top = 10
    Width = 121
    Height = 21
    TabOrder = 0
  end
  object Button1: TButton
    Left = 272
    Top = 41
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object btnGoto: TButton
    Left = 272
    Top = 7
    Width = 75
    Height = 26
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
end
