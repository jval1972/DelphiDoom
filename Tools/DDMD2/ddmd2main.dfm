object Form1: TForm1
  Left = 243
  Top = 126
  Width = 928
  Height = 600
  Caption = 'DelphiDoom MD2 Models Tool'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 920
    Height = 573
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Actor Definition'
      object Label1: TLabel
        Left = 16
        Top = 80
        Width = 38
        Height = 13
        Caption = 'FLAGS: '
        FocusControl = CheckListBox1
      end
      object Label2: TLabel
        Left = 192
        Top = 80
        Width = 56
        Height = 13
        Caption = 'FLAGS_EX: '
        FocusControl = CheckListBox2
      end
      object Label3: TLabel
        Left = 368
        Top = 80
        Width = 62
        Height = 13
        Caption = 'FLAGS2_EX: '
        FocusControl = CheckListBox3
      end
      object Label4: TLabel
        Left = 24
        Top = 16
        Width = 39
        Height = 13
        Caption = 'Radius: '
      end
      object CheckListBox1: TCheckListBox
        Left = 16
        Top = 96
        Width = 172
        Height = 420
        ItemHeight = 13
        TabOrder = 0
      end
      object CheckListBox2: TCheckListBox
        Left = 192
        Top = 96
        Width = 172
        Height = 420
        ItemHeight = 13
        TabOrder = 1
      end
      object CheckListBox3: TCheckListBox
        Left = 368
        Top = 96
        Width = 172
        Height = 420
        ItemHeight = 13
        TabOrder = 2
      end
      object RadiusEdit: TEdit
        Left = 64
        Top = 16
        Width = 121
        Height = 21
        TabOrder = 3
        Text = '16'
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'MD2 Model'
      ImageIndex = 1
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 912
        Height = 545
        Align = alClient
        Caption = 'Panel1'
        TabOrder = 0
        OnResize = Panel1Resize
      end
    end
  end
end
