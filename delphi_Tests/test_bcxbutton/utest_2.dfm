object Form1: TForm1
  Left = 353
  Top = 169
  Caption = 'BCXButton'
  ClientHeight = 283
  ClientWidth = 368
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object vs1: TBGRAVirtualScreen
    Left = 0
    Top = 0
    Width = 368
    Height = 283
    OnRedraw = vs1Redraw
    Align = alClient
    Alignment = taLeftJustify
    Color = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    object BCXButton2: TBCXButton
      Left = 12
      Top = 120
      Width = 318
      Height = 39
      OnRenderControl = BCXButton1RenderControl
      Caption = 'Button 2'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object BCXButton1: TBCXButton
      Left = 12
      Top = 6
      Width = 102
      Height = 30
      OnRenderControl = BCXButton1RenderControl
      Caption = 'Button 1'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object BCXButton3: TBCXButton
      Left = 12
      Top = 42
      Width = 102
      Height = 30
      OnRenderControl = BCXButton3RenderControl
      Caption = 'Button 3'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object BCXButton4: TBCXButton
      Left = 12
      Top = 78
      Width = 102
      Height = 30
      OnRenderControl = BCXButton3RenderControl
      Caption = 'Button 4'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object BCXButton5: TBCXButton
      Left = 120
      Top = 6
      Width = 102
      Height = 102
      OnRenderControl = BCXButton1RenderControl
      Caption = 'Button 5'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object BCXButton6: TBCXButton
      Left = 228
      Top = 6
      Width = 102
      Height = 102
      OnRenderControl = BCXButton1RenderControl
      Caption = 'Button 6'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object BCXButton7: TBCXButton
      Left = 12
      Top = 174
      Width = 318
      Height = 66
      OnRenderControl = BCXButton1RenderControl
      Caption = 'Button 7'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
  end
end
