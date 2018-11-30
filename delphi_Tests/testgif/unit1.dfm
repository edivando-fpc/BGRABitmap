object Form1: TForm1
  Left = 442
  Top = 288
  Caption = 'Form1'
  ClientHeight = 202
  ClientWidth = 304
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnPaint = FormPaint
  PixelsPerInch = 96
  TextHeight = 13
  object Timer1: TTimer
    Interval = 15
    OnTimer = Timer1Timer
    Left = 111
    Top = 44
  end
end
