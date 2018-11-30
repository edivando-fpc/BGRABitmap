object Form1: TForm1
  Left = 333
  Top = 185
  Caption = 'SVG Viewer'
  ClientHeight = 450
  ClientWidth = 900
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object OpenDialog1: TOpenDialog
    DefaultExt = '.svg'
    Filter = 'Fichier SVG (*.svg);*.svg'
    Left = 337
    Top = 183
  end
end
