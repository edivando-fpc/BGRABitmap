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


Unit BGRAReadIco;

{$i bgrabitmap.inc}{$H+}

interface

uses
  Classes, SysUtils, BGRATypes,{$IFNDEF FPC}Types, GraphType, BGRAGraphics,{$ENDIF} FPImage{$IFDEF BGRABITMAP_USE_LCL}, Graphics{$ENDIF};

type
  {$IFDEF BGRABITMAP_USE_LCL}TCustomIconClass = class of TCustomIcon;{$ENDIF}
  TByteSet = set of byte;

  { TBGRAReaderIcoOrCur }

  TBGRAReaderIcoOrCur = class(TFPCustomImageReader)
  protected
    procedure InternalRead({%H-}Str: TStream; {%H-}Img: TFPCustomImage); override;
    function InternalCheck(Str: TStream): boolean; override;
    function ExpectedMagic: TByteSet; virtual; abstract;
    {$IFDEF BGRABITMAP_USE_LCL}function LazClass: TCustomIconClass; virtual; abstract;{$ENDIF}
  public
    WantedWidth, WantedHeight : integer;
  end;

  TBGRAReaderIco = class(TBGRAReaderIcoOrCur)
  protected
    function ExpectedMagic: TByteSet; override;
    {$IFDEF BGRABITMAP_USE_LCL}function LazClass: TCustomIconClass; override;{$ENDIF}
  end;

  { TBGRAReaderCur }

  TBGRAReaderCur = class(TBGRAReaderIcoOrCur)
  protected
    function ExpectedMagic: TByteSet; override;
    {$IFDEF BGRABITMAP_USE_LCL}function LazClass: TCustomIconClass; override;{$ENDIF}
  end;

implementation

uses BGRABitmapTypes{$IFNDEF BGRABITMAP_USE_LCL}, BGRAIconCursor{$ENDIF};

{ TBGRAReaderCur }

function TBGRAReaderCur.ExpectedMagic: TByteSet;
begin
  result := [2];
end;

{$IFDEF BGRABITMAP_USE_LCL}function TBGRAReaderCur.LazClass: TCustomIconClass;
begin
  result := TCursorImage;
end;{$ENDIF}

{ TBGRAReaderIco }

function TBGRAReaderIco.ExpectedMagic: TByteSet;
begin
  result := [1,2];
end;

{$IFDEF BGRABITMAP_USE_LCL}function TBGRAReaderIco.LazClass: TCustomIconClass;
begin
  result := TIcon;
end;{$ENDIF}

{ TBGRAReaderIcoOrCur }

procedure TBGRAReaderIcoOrCur.InternalRead(Str: TStream; Img: TFPCustomImage);
{$IFDEF BGRABITMAP_USE_LCL}
var ico: TCustomIcon; i,bestIdx: integer;
    height,width: BGRAWord; format:TPixelFormat;
    bestHeight,bestWidth: integer; maxFormat: TPixelFormat;
    compWidth,compHeight: integer;
begin
  if WantedWidth > 0 then compWidth:= WantedWidth else compWidth:= 65536;
  if WantedHeight > 0 then compHeight:= WantedHeight else compHeight:= 65536;
  ico := LazClass.Create;
  try
    ico.LoadFromStream(Str);
    bestIdx := -1;
    bestHeight := 0;
    bestWidth := 0;
    maxFormat := pfDevice;
    for i := 0 to ico.Count-1 do
    begin
      ico.GetDescription(i,format,height,width);
      if (bestIdx = -1) or (abs(height-compHeight)+abs(width-compWidth) < abs(bestHeight-compHeight)+abs(bestWidth-compWidth)) or
      ((height = bestHeight) and (width = bestWidth) and (format > maxFormat)) then
      begin
        bestIdx := i;
        bestHeight := height;
        bestWidth := width;
        maxFormat := format;
      end;
    end;
    if (bestIdx = -1) or (bestWidth = 0) or (bestHeight = 0) then
      raise exception.Create('No adequate icon found') else
    begin
      ico.Current := bestIdx;
      Img.Assign(ico);
    end;
  finally
    ico.free;
  end;
end;
{$ELSE}
var icoCur: TBGRAIconCursor;
    compWidth,compHeight: integer;
    bmp: TBGRACustomBitmap;
begin
  if WantedWidth > 0 then compWidth:= WantedWidth else compWidth:= 65536;
  if WantedHeight > 0 then compHeight:= WantedHeight else compHeight:= 65536;
  icoCur := TBGRAIconCursor.Create(Str);
  try
    bmp := icoCur.GetBestFitBitmap(compWidth,compHeight);
    try
      Img.Assign(bmp);
    finally
      bmp.Free;
    end;
  finally
    icoCur.Free;
  end;
end;
{$ENDIF}

function TBGRAReaderIcoOrCur.InternalCheck(Str: TStream): boolean;
var {%H-}magic: packed array[0..5] of byte;
    oldPos: BGRAInt64;
begin
  oldPos := str.Position;
  result := (str.Read({%H-}magic,sizeof(magic)) = sizeof(magic));
  str.Position:= oldPos;
  if result then
    result := (magic[0] = $00) and (magic[1] = $00) and (magic[2] in ExpectedMagic) and (magic[3] = $00) and
             (magic[4] + (magic[5] shl 8) > 0);
end;

initialization

  DefaultBGRAImageReader[ifIco] := TBGRAReaderIco;
  DefaultBGRAImageReader[ifCur] := TBGRAReaderCur;

end.

