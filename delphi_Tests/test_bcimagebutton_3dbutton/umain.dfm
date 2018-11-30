object frmMain: TfrmMain
  Left = 343
  Top = 103
  Caption = 'Test Drawing'
  ClientHeight = 353
  ClientWidth = 491
  Color = clSilver
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object BCImageButton1: TBCImageButton
    Left = 8
    Top = 8
    Width = 188
    Height = 137
    BitmapOptions.MarginTop = 9
    BitmapOptions.MarginRight = 13
    BitmapOptions.MarginBottom = 15
    BitmapOptions.MarginLeft = 13
    BitmapOptions.Direction = sdVertical
    Caption = 'Button'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 30
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = False
  end
  object BCImageButton2: TBCImageButton
    Left = 224
    Top = 0
    Width = 188
    Height = 137
    BitmapOptions.Direction = sdVertical
    Caption = 'Toggle Button'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 30
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = False
    Toggle = True
    Pressed = True
  end
end
