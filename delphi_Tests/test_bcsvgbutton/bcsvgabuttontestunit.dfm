object Form1: TForm1
  Left = 574
  Top = 226
  Caption = 'BCSVGButton'
  ClientHeight = 392
  ClientWidth = 325
  Color = 3618615
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 272
    Width = 325
    Height = 120
    Align = alBottom
    TabOrder = 0
    object Button2: TButton
      Left = 12
      Top = 62
      Width = 181
      Height = 31
      Caption = 'Set Down = False'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnClick = Button2Click
    end
    object Button1: TButton
      Left = 12
      Top = 12
      Width = 181
      Height = 31
      Caption = 'Set Down = True'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnClick = Button1Click
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 325
    Height = 272
    Align = alClient
    TabOrder = 1
  end
end
