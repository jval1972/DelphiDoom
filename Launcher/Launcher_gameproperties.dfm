object GamePropertiesForm: TGamePropertiesForm
  Left = 804
  Top = 508
  BorderStyle = bsDialog
  Caption = 'Game properties'
  ClientHeight = 228
  ClientWidth = 457
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 24
    Width = 60
    Height = 13
    Caption = 'Description: '
    FocusControl = DescriptionEdit
  end
  object Label2: TLabel
    Left = 8
    Top = 56
    Width = 56
    Height = 13
    Caption = 'Main WAD: '
    FocusControl = MainWADEdit
  end
  object Label3: TLabel
    Left = 8
    Top = 88
    Width = 71
    Height = 13
    Caption = 'Extra PWADs: '
    FocusControl = PWADEdit
  end
  object Label4: TLabel
    Left = 8
    Top = 152
    Width = 39
    Height = 13
    Caption = 'Engine: '
    FocusControl = GameEngineComboBox
  end
  object Label5: TLabel
    Left = 8
    Top = 120
    Width = 92
    Height = 13
    Caption = 'Additional params: '
    FocusControl = AdditionalParamsEdit
  end
  object Panel1: TPanel
    Left = 326
    Top = 0
    Width = 131
    Height = 228
    Align = alRight
    BevelOuter = bvNone
    Caption = ' '
    Color = clWhite
    TabOrder = 0
    object RunDelphiDoomButton: TButton
      Left = 8
      Top = 24
      Width = 107
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object Button2: TButton
      Left = 8
      Top = 56
      Width = 107
      Height = 25
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object DescriptionEdit: TEdit
    Left = 104
    Top = 24
    Width = 193
    Height = 21
    TabOrder = 1
  end
  object MainWADEdit: TEdit
    Left = 104
    Top = 56
    Width = 193
    Height = 21
    TabOrder = 2
  end
  object SelectMainWADButton: TButton
    Left = 298
    Top = 56
    Width = 24
    Height = 22
    Caption = '...'
    TabOrder = 3
    OnClick = SelectMainWADButtonClick
  end
  object SelectPWADsButton: TButton
    Left = 298
    Top = 88
    Width = 24
    Height = 22
    Caption = '...'
    TabOrder = 5
    OnClick = SelectPWADsButtonClick
  end
  object PWADEdit: TEdit
    Left = 104
    Top = 88
    Width = 193
    Height = 21
    TabOrder = 4
  end
  object GameEngineComboBox: TComboBox
    Left = 104
    Top = 152
    Width = 81
    Height = 22
    Style = csOwnerDrawFixed
    ItemHeight = 16
    TabOrder = 7
    Items.Strings = (
      'Doom'
      'Heretic'
      'Hexen'
      'Strife')
  end
  object AdditionalParamsEdit: TEdit
    Left = 104
    Top = 120
    Width = 193
    Height = 21
    TabOrder = 6
  end
  object OpenPWADDialog: TOpenDialog
    DefaultExt = 'wad'
    Filter = 'WAD files (*.wad)|*.wad|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 212
    Top = 108
  end
  object OpenMainWADDialog: TOpenDialog
    DefaultExt = 'wad'
    Filter = 'WAD files (*.wad)|*.wad|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 236
    Top = 116
  end
end
