{ ***************************************************************************
 *                                                                          *
 *  This file is part of BGRABitmap library which is distributed under the  *
 *  modified LGPL.                                                          *
 *                                                                          *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,   *
 *  for details about the copyright.                                        *
 *                                                                          *
 *  This program is distributed in the hope that it will be useful,         *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of          *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    *
 *                                                                          *
 ************************* BGRABitmap library  ******************************

 - Drawing routines with transparency and antialiasing with Lazarus.
   Offers also various transforms.
 - These routines allow to manipulate 32bit images in BGRA format or RGBA
   format (depending on the platform).
 - This code is under modified LGPL (see COPYING.modifiedLGPL.txt).
   This means that you can link this library inside your programs for any purpose.
   Only the included part of the code must remain LGPL.

 - If you make some improvements to this library, please notify here:
   http://www.lazarus.freepascal.org/index.php/topic,12037.0.html

   ********************* Contact : Circular at operamail.com *******************


   ******************************* CONTRIBUTOR(S) ******************************
   - Edivando S. Santos Brasil | mailedivando@gmail.com
     (Compatibility with FPC ($Mode objfpc/delphi) and delphi VCL 11/2018)

   ***************************** END CONTRIBUTOR(S) *****************************}


// This Unit provides some optimisations of TFPReaderPCX: decompression using a read buffer.
//  It also fixes the progress message and the InternalCheck. }

unit BGRAReadPCX;

{$i bgrabitmap.inc}{$H+}

interface

uses FPImage, Classes, SysUtils, BGRATypes,{$IFNDEF FPC}Types, GraphType, BGRAGraphics,{$ENDIF} FPReadPCX;

type

  { TBGRAReaderPCX }

  TBGRAReaderPCX = class(TFPReaderPCX)
  protected
    FBuffer: packed array of byte;
    FBufferPos, FBufferSize: integer;
    FBufferStream: TStream;
    function InternalCheck(Stream: TStream): boolean; override;
    procedure InternalRead(Stream: TStream; Img: TFPCustomImage); override;
    procedure ReadScanLine({%H-}Row: integer; Stream: TStream); override;
    procedure WriteScanLine(Row: integer; Img: TFPCustomImage); override;
    procedure InitReadBuffer(AStream: TStream; ASize: integer);
    procedure CloseReadBuffer;
    function GetNextBufferByte: byte;
  end;

implementation

uses BGRABitmapTypes;

procedure TBGRAReaderPCX.ReadScanLine(Row: integer; Stream: TStream);
var
  P: PByte;
  B: BGRANativeUInt;
  bytes, Count: BGRANativeUInt;
begin
  if FLineSize <= 0 then exit;
  P     := FScanLine;
  bytes := FLineSize;
  if Compressed then
  begin
    while bytes > 0 do
    begin
      B := GetNextBufferByte;
      if (B < $c0) then
        Count := 1
      else
      begin
        Count := B - $c0;
        B := GetNextBufferByte;
      end;
      if Count = 0 then continue else
      if Count = 1 then
      begin
        P^ := B;
        Inc(P);
        Dec(bytes);
      end else
      begin
        if Count > bytes then Count := bytes;
        fillchar(p^, count, B);
        Inc(p, count);
        dec(bytes, count);
      end;
    end;
  end
  else
    Stream.ReadBuffer(FScanLine^, FLineSize);
end;

procedure TBGRAReaderPCX.InternalRead(Stream: TStream; Img: TFPCustomImage);
var
  H, Row:   integer;
  continue: boolean;
  emptyRect: TRect;
begin
  emptyRect := rect(0,0,0,0);
  continue    := True;
  Progress(psStarting, 0, False, emptyRect, '', continue);
  Stream.Read(Header, SizeOf(Header));
  AnalyzeHeader(Img);
  case BytesPerPixel of
    1: CreateBWPalette(Img);
    4: CreatePalette16(Img);
    8: ReadPalette(stream, Img);
    else
      if (Header.PaletteType = 2) then
        CreateGrayPalette(Img);
  end;
  H := Img.Height;
  if Compressed then InitReadBuffer(Stream,2048);
  for Row := 0 to H - 1 do
  begin
    ReadScanLine(Row, Stream);
    WriteScanLine(Row, Img);
    Progress(psRunning, (Row+1) div H, False, emptyRect, '', continue);
  end;
  if Compressed then CloseReadBuffer;
  Progress(psEnding, 100, False, emptyRect, '', continue);
  freemem(FScanLine);
end;

procedure TBGRAReaderPCX.WriteScanLine(Row: integer; Img: TFPCustomImage);
var
  Col:   integer;
  C:     TFPColor;
  P, P1, P2, P3: PByte;
  Z2:    BGRAWord;
  color: byte;
begin
  C.Alpha := AlphaOpaque;
  P  := FScanLine;
  Z2 := Header.BytesPerLine;
  begin
    case BytesPerPixel of
      1:
      begin
        for Col := 0 to Img.Width - 1 do
        begin
          if (P[col div 8] and (128 shr (col mod 8))) <> 0 then
            Img.Colors[Col, Row] := Img.Palette[1]
          else
            Img.Colors[Col, Row] := Img.Palette[0];
        end;
      end;
      4:
      begin
        P1 := P;
        Inc(P1, Z2);
        P2 := P;
        Inc(P2, Z2 * 2);
        P3 := P;
        Inc(P3, Z2 * 3);
        for Col := 0 to Img.Width - 1 do
        begin
          color := 0;
          if (P[col div 8] and (128 shr (col mod 8))) <> 0 then
            Inc(color, 1);
          if (P1[col div 8] and (128 shr (col mod 8))) <> 0 then
            Inc(color, 1 shl 1);
          if (P2[col div 8] and (128 shr (col mod 8))) <> 0 then
            Inc(color, 1 shl 2);
          if (P3[col div 8] and (128 shr (col mod 8))) <> 0 then
            Inc(color, 1 shl 3);
          Img.Colors[Col, Row] := Img.Palette[color];
        end;
      end;
      8:
      begin
        for Col := 0 to Img.Width - 1 do
        begin
          Img.Colors[Col, Row] := Img.Palette[P[Col]];
        end;
      end;
      24:
      begin
        for Col := 0 to Img.Width - 1 do
        begin
          with C do
          begin
            Red   := P[col] or (P[col] shl 8);
            Blue  := P[col + Z2 * 2] or (P[col + Z2 * 2] shl 8);
            Green := P[col + Z2] or (P[col + Z2] shl 8);
            Alpha := alphaOpaque;
          end;
          Img[col, row] := C;
        end;
      end;
    end;
  end;
end;

procedure TBGRAReaderPCX.InitReadBuffer(AStream: TStream; ASize: integer);
begin
  setLength(FBuffer,ASize);
  FBufferSize := AStream.Read(FBuffer[0],ASize);
  FBufferPos := 0;
  FBufferStream := AStream;
end;

procedure TBGRAReaderPCX.CloseReadBuffer;
begin
  FBufferStream.Position:= FBufferStream.Position-FBufferSize+FBufferPos;
end;

function TBGRAReaderPCX.GetNextBufferByte: byte;
begin
  if FBufferPos < FBufferSize then
  begin
    result := FBuffer[FBufferPos];
    inc(FBufferPos);
  end else
  if FBufferSize = 0 then
    result := 0
  else
  begin
    FBufferSize := FBufferStream.Read(FBuffer[0],length(FBuffer));
    FBufferPos := 0;
    if FBufferPos < FBufferSize then
    begin
      result := FBuffer[FBufferPos];
      inc(FBufferPos);
    end else
      result := 0;
  end;
end;

function TBGRAReaderPCX.InternalCheck({%H-}Stream: TStream): boolean;
var
  {%H-}magic: packed array[0..3] of byte;
  oldPos: BGRAInt64;
begin
  oldPos:= stream.Position;
  result := stream.Read({%H-}magic,SizeOf(magic)) = sizeof(magic);
  stream.Position:= oldPos;
  if result then
    result := (magic[0] in[$0a,$0c,$cd]) and (magic[1] in [0,2,3,4,5]) and (magic[2] in[0,1]) and (magic[3] in[1,2,4,8])
end;

initialization

  DefaultBGRAImageReader[ifPcx] := TBGRAReaderPCX;

end.
