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


Unit BGRALCLBitmap;

{$i bgrabitmap.inc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, BGRATypes,{$IFNDEF FPC}windows, Types,{$ENDIF} BGRAGraphics, BGRABitmapTypes, GraphType, BGRADefaultBitmap;

type
  { TBGRALCLBitmap }

  TBGRALCLBitmap = class(TBGRADefaultBitmap)
  protected
    function LoadFromRawImage(ARawImage: TRawImage; DefaultOpacity: byte;
      AlwaysReplaceAlpha: boolean = False; RaiseErrorOnInvalidPixelFormat: boolean = True): boolean; override;
    function CreateDefaultFontRenderer: TBGRACustomFontRenderer; override;
    procedure DoLoadFromBitmap; override;
    procedure RebuildBitmap; override;
    function CreatePtrBitmap(AWidth, AHeight: integer; AData: PBGRAPixel
      ): TBGRAPtrBitmap; override;
    procedure AssignRasterImage(ARaster: TRasterImage); virtual;
    procedure ExtractXorMask;
  public
    procedure Assign(Source: TPersistent); override;
    procedure DataDrawTransparent(ACanvas: TCanvas; Rect: TRect;
      AData: Pointer; ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer); override;
    procedure DataDrawOpaque(ACanvas: TCanvas; ARect: TRect; AData: Pointer;
      ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer); override;
    procedure GetImageFromCanvas(CanvasSource: TCanvas; x, y: integer); override;
    procedure LoadFromDevice({%H-}DC: HDC); override;
    procedure LoadFromDevice({%H-}DC: HDC; {%H-}ARect: TRect); override;
    procedure TakeScreenshotOfPrimaryMonitor; override;
    procedure TakeScreenshot({%H-}ARect: TRect); override;
  end;

  { TBGRALCLPtrBitmap }

  TBGRALCLPtrBitmap = class(TBGRAPtrBitmap)

    procedure RebuildBitmap; override;
    function CreatePtrBitmap(AWidth, AHeight: Integer; AData: PBGRAPixel): TBGRAPtrBitmap; override;
    function CreateDefaultFontRenderer: TBGRACustomFontRenderer; override;
    function LoadFromRawImage(ARawImage: TRawImage; DefaultOpacity: byte;
      AlwaysReplaceAlpha: boolean=False; {%H-}RaiseErrorOnInvalidPixelFormat: boolean
      =True): boolean; override;
  public
    procedure GetImageFromCanvas(CanvasSource: TCanvas; x, y: Integer); override;
    procedure DataDrawTransparent(ACanvas: TCanvas; Rect: TRect; AData: Pointer;
      ALineOrder: TRawImageLineOrder; AWidth, AHeight: Integer); override;
    procedure DataDrawOpaque(ACanvas: TCanvas; Rect: TRect; AData: Pointer;
      ALineOrder: TRawImageLineOrder; AWidth, AHeight: Integer); override;
  end;

implementation

uses BGRAText, {$IFDEF FPC}LCLType, LCLIntf,{$ENDIF} FPImage;

type
  TCopyPixelProc = procedure (psrc: PByte; pdest: PBGRAPixel; count: BGRANativeInt; sourcePixelSize: BGRAPtrInt; defaultOpacity: byte);

procedure ApplyMask1bit(psrc: PByte; pdest: PBGRAPixel; count: BGRANativeInt; {%H-}sourcePixelSize: BGRAPtrInt; {%H-}defaultOpacity: byte);
var currentBit: byte;
begin
  currentBit := 1;
  while count > 0 do
  begin
    if psrc^ and currentBit <> 0 then pdest^.alpha := 0;
    inc(pdest);
    if currentBit = 128 then
    begin
      currentBit := 1;
      inc(psrc);
    end else
      currentBit := currentBit shl 1;
    dec(count);
  end;
end;

procedure ApplyMask1bitRev(psrc: PByte; pdest: PBGRAPixel; count: BGRANativeInt; {%H-}sourcePixelSize: BGRAPtrInt; {%H-}defaultOpacity: byte);
var currentBit: byte;
begin
  currentBit := 128;
  while count > 0 do
  begin
    if psrc^ and currentBit <> 0 then pdest^.alpha := 0;
    inc(pdest);
    if currentBit = 1 then
    begin
      currentBit := 128;
      inc(psrc);
    end else
      currentBit := currentBit shr 1;
    dec(count);
  end;
end;

procedure CopyFromBW_SetAlpha(psrc: PByte; pdest: PBGRAPixel; count: BGRANativeInt; {%H-}sourcePixelSize: BGRAPtrInt; defaultOpacity: byte);
var currentBit: byte;
begin
  currentBit := 1;
  while count > 0 do
  begin
    if psrc^ and currentBit <> 0 then
      pdest^ := BGRAWhite
    else
      pdest^ := BGRABlack;
    pdest^.alpha := DefaultOpacity;
    inc(pdest);
    if currentBit = 128 then
    begin
      currentBit := 1;
      inc(psrc);
    end else
      currentBit := currentBit shl 1;
    dec(count);
  end;
end;

procedure CopyFromBW_SetAlphaBitRev(psrc: PByte; pdest: PBGRAPixel; count: BGRANativeInt; {%H-}sourcePixelSize: BGRAPtrInt; defaultOpacity: byte);
var currentBit: byte;
begin
  currentBit := 128;
  while count > 0 do
  begin
    if psrc^ and currentBit <> 0 then
      pdest^ := BGRAWhite
    else
      pdest^ := BGRABlack;
    pdest^.alpha := DefaultOpacity;
    inc(pdest);
    if currentBit = 1 then
    begin
      currentBit := 128;
      inc(psrc);
    end else
      currentBit := currentBit shr 1;
    dec(count);
  end;
end;

procedure CopyFrom24Bit(psrc: PByte; pdest: PBGRAPixel; count: BGRANativeInt; sourcePixelSize: BGRAPtrInt; defaultOpacity: byte);
begin
  while count > 0 do
  begin
    {$IFDEF BDS}move(psrc^, pdest^, sizeof(BGRAWord));{$ELSE}PBGRAWord(pdest)^ := PBGRAWord(psrc)^;{$ENDIF}
    (PByte(pdest)+2)^ := (psrc+2)^;
    pdest^.alpha := DefaultOpacity;
    inc(psrc,sourcePixelSize);
    inc(pdest);
    dec(count);
  end;
end;

procedure CopyFrom24Bit_SwapRedBlue(psrc: PByte; pdest: PBGRAPixel; count: BGRANativeInt; sourcePixelSize: BGRAPtrInt; defaultOpacity: byte);
begin
  while count > 0 do
  begin
    PByte(pdest)^ := (psrc+2)^;
    (PByte(pdest)+1)^ := (psrc+1)^;
    (PByte(pdest)+2)^ := psrc^;
    pdest^.alpha := DefaultOpacity;
    inc(psrc,sourcePixelSize);
    inc(pdest);
    dec(count);
  end;
end;

procedure CopyFromARGB_KeepAlpha(psrc: PByte; pdest: PBGRAPixel; count: BGRANativeInt; sourcePixelSize: BGRAPtrInt; {%H-}defaultOpacity: byte);
{$IFDEF BDS}var _BGRADWord : BGRADWord;{$ENDIF}//#
begin
  while count > 0 do
  begin
    {$IFDEF BDS}_BGRADWord{$ELSE}PBGRADWord(pdest)^{$ENDIF} := ((PByte(psrc)+3)^ shl TBGRAPixel_BlueShift) or
                      ((PByte(psrc)+2)^ shl TBGRAPixel_GreenShift) or
                      ((PByte(psrc)+1)^ shl TBGRAPixel_RedShift) or
                      (PByte(psrc)^ shl TBGRAPixel_AlphaShift);
    {$IFDEF BDS}move(_BGRADWord, pdest^, sizeof(BGRADWord));{$ENDIF}
    dec(count);
    inc(pdest);
    inc(psrc, sourcePixelSize);
  end;
end;

procedure CopyFromARGB_SetAlpha(psrc: PByte; pdest: PBGRAPixel; count: BGRANativeInt; sourcePixelSize: BGRAPtrInt; defaultOpacity: byte);
{$IFDEF BDS}var _BGRADWord : BGRADWord;{$ENDIF}//#
begin
  while count > 0 do
  begin
    {$IFDEF BDS}_BGRADWord{$ELSE}PBGRADWord(pdest)^{$ENDIF} := ((PByte(psrc)+3)^ shl TBGRAPixel_BlueShift) or
                      ((PByte(psrc)+2)^ shl TBGRAPixel_GreenShift) or
                      ((PByte(psrc)+1)^ shl TBGRAPixel_RedShift) or
                      (DefaultOpacity shl TBGRAPixel_AlphaShift);
    {$IFDEF BDS}move(_BGRADWord, pdest^, sizeof(BGRADWord));{$ENDIF}
    inc(psrc, sourcePixelSize);
    inc(pdest);
    dec(count);
  end;
end;

procedure CopyFromARGB_ReplaceZeroAlpha(psrc: PByte; pdest: PBGRAPixel; count: BGRANativeInt; sourcePixelSize: BGRAPtrInt; defaultOpacity: byte);
const ARGB_ColorMask = {$IFDEF ENDIAN_LITTLE}$FFFFFF00{$ELSE}$00FFFFFF{$ENDIF};
      ARGB_RedShift = {$IFDEF ENDIAN_LITTLE}8{$ELSE}16{$ENDIF};
      ARGB_GreenShift = {$IFDEF ENDIAN_LITTLE}16{$ELSE}8{$ENDIF};
      ARGB_BlueShift = {$IFDEF ENDIAN_LITTLE}24{$ELSE}0{$ENDIF};
var
  sourceval: BGRANativeUInt;
  alphaValue: BGRANativeUInt;
  OpacityOrMask: BGRANativeUInt;
  {$IFDEF BDS}_BGRADWord : BGRADWord;{$ENDIF}//#
begin
  OpacityOrMask := DefaultOpacity shl TBGRAPixel_AlphaShift;
  while count > 0 do
  begin
    {$IFDEF BDS}move(psrc^, sourceval, sizeof(BGRALongWord));{$ELSE}sourceval := PBGRALongWord(psrc)^;{$ENDIF}

    alphaValue := {$IFDEF ENDIAN_LITTLE}sourceval and $ff{$ELSE}sourceval shr 24{$ENDIF};
    if (alphaValue = 0) and ((sourceval and ARGB_ColorMask) <> 0) then //if not black but transparent
    begin
      {$IFDEF BDS}_BGRADWord{$ELSE}PBGRADWord(pdest)^{$ENDIF} := (((sourceval shr ARGB_BlueShift) and $ff) shl TBGRAPixel_BlueShift) or
                        (((sourceval shr ARGB_GreenShift) and $ff) shl TBGRAPixel_GreenShift) or
                        (((sourceval shr ARGB_RedShift) and $ff) shl TBGRAPixel_RedShift) or
                        OpacityOrMask;
      {$IFDEF BDS}move(_BGRADWord, pdest^, sizeof(BGRADWord));{$ENDIF}
    end else
    begin
      {$IFDEF BDS}_BGRADWord{$ELSE}PBGRADWord(pdest)^{$ENDIF} := (((sourceval shr ARGB_BlueShift) and $ff) shl TBGRAPixel_BlueShift) or
                        (((sourceval shr ARGB_GreenShift) and $ff) shl TBGRAPixel_GreenShift) or
                        (((sourceval shr ARGB_RedShift) and $ff) shl TBGRAPixel_RedShift) or
                        (alphaValue shl TBGRAPixel_AlphaShift);
      {$IFDEF BDS}move(_BGRADWord, pdest^, sizeof(BGRADWord));{$ENDIF}
    end;
    dec(count);
    inc(pdest);
    inc(psrc, sourcePixelSize);
  end;
end;

const
  BGRA_AlphaMask = 255 shl TBGRAPixel_AlphaShift;
  BGRA_RedMask = 255 shl TBGRAPixel_RedShift;
  BGRA_GreenMask = 255 shl TBGRAPixel_GreenShift;
  BGRA_BlueMask = 255 shl TBGRAPixel_BlueShift;
  BGRA_ColorMask = BGRA_RedMask or BGRA_GreenMask or BGRA_BlueMask;

procedure CopyFrom32Bit_KeepAlpha(psrc: PByte; pdest: PBGRAPixel; count: BGRANativeInt; sourcePixelSize: BGRAPtrInt; {%H-}defaultOpacity: byte);
begin
  if sourcePixelSize = 4 then
    move(psrc^,pdest^,count*sizeof(TBGRAPixel))
  else
  begin
    while count > 0 do
    begin
      {$IFDEF BDS}move(psrc^, pdest^, sizeof(BGRADWord)){$ELSE}PBGRADWord(pdest)^ := PBGRADWord(psrc)^{$ENDIF};
      dec(count);
      inc(pdest);
      inc(psrc, sourcePixelSize);
    end;
  end;
end;

procedure CopyFrom32Bit_SwapRedBlue_KeepAlpha(psrc: PByte; pdest: PBGRAPixel; count: BGRANativeInt; sourcePixelSize: BGRAPtrInt; {%H-}defaultOpacity: byte);
var srcValue: BGRANativeUInt;
{$IFDEF BDS}_BGRADWord : BGRADWord;{$ENDIF}//#
begin
  while count > 0 do
  begin
    {$IFDEF BDS}move(psrc^, srcValue, sizeof(BGRADWord));{$ELSE}srcValue := PBGRADWord(psrc)^;{$ENDIF}

    {$IFDEF BDS}_BGRADWord{$ELSE}PBGRADWord(pdest)^{$ENDIF} := (srcValue and not (BGRA_RedMask or BGRA_BlueMask))
                   or (((srcValue and BGRA_RedMask) shr TBGRAPixel_RedShift) shl TBGRAPixel_BlueShift)
                   or (((srcValue and BGRA_BlueMask) shr TBGRAPixel_BlueShift) shl TBGRAPixel_RedShift);
    {$IFDEF BDS}move(_BGRADWord, pdest^, sizeof(BGRADWord));{$ENDIF}
    dec(count);
    inc(pdest);
    inc(psrc, sourcePixelSize);
  end;
end;

procedure CopyFrom32Bit_SetAlpha(psrc: PByte; pdest: PBGRAPixel; count: BGRANativeInt; sourcePixelSize: BGRAPtrInt; defaultOpacity: byte);
var
  OpacityOrMask: BGRANativeUInt;
  {$IFDEF BDS}_BGRADWord : BGRADWord;{$ENDIF}//#
begin
  OpacityOrMask := DefaultOpacity shl TBGRAPixel_AlphaShift;
  while count > 0 do
  begin
    {$IFDEF BDS}_BGRADWord{$ELSE}PBGRADWord(pdest)^{$ENDIF} := ({$IFDEF BDS}_BGRADWord{$ELSE}PBGRADWord(psrc)^{$ENDIF} and not BGRA_AlphaMask) or OpacityOrMask;
    {$IFDEF BDS}move(_BGRADWord, pdest^, sizeof(BGRADWord));{$ENDIF}
    inc(psrc, sourcePixelSize);
    inc(pdest);
    dec(count);
  end;
end;

procedure CopyFrom32Bit_SwapRedBlue_SetAlpha(psrc: PByte; pdest: PBGRAPixel; count: BGRANativeInt; sourcePixelSize: BGRAPtrInt; defaultOpacity: byte);
begin
  while count > 0 do
  begin
    pdest^.red := PBGRAPixel(psrc)^.blue;
    pdest^.green := PBGRAPixel(psrc)^.green;
    pdest^.blue := PBGRAPixel(psrc)^.red;
    pdest^.alpha := DefaultOpacity; //use default opacity
    inc(psrc, sourcePixelSize);
    inc(pdest);
    dec(count);
  end;
end;

procedure CopyFrom32Bit_ReplaceZeroAlpha(psrc: PByte; pdest: PBGRAPixel; count: BGRANativeInt; sourcePixelSize: BGRAPtrInt; defaultOpacity: byte);
var sourceval: BGRANativeUInt;
  OpacityOrMask : BGRANativeUInt;
  {$IFDEF BDS}_BGRALongWord : BGRALongWord;{$ENDIF}//#
begin
  OpacityOrMask := DefaultOpacity shl TBGRAPixel_AlphaShift;
  while count > 0 do
  begin
    {$IFDEF BDS}move(psrc^, sourceval, sizeof(BGRALongWord));{$ELSE}sourceval := PBGRALongWord(psrc)^;{$ENDIF}
    if ((sourceVal shr TBGRAPixel_AlphaShift) and $ff = 0) and ((sourceval and BGRA_ColorMask) <> 0) then //if not black but transparent
    begin
      {$IFDEF BDS}_BGRALongWord{$ELSE}PBGRALongWord(pdest)^{$ENDIF} := (sourceval and BGRA_ColorMask) or OpacityOrMask; //use default opacity
      {$IFDEF BDS}move(_BGRALongWord, pdest^, sizeof(BGRALongWord));{$ENDIF}
    end
    else
    begin
      {$IFDEF BDS}move(psrc^, _BGRALongWord, sizeof(PBGRALongWord));{$ELSE}PBGRALongWord(pdest)^ := PBGRALongWord(psrc)^;{$ENDIF}
      {$IFDEF BDS}move(_BGRALongWord, pdest^, sizeof(BGRALongWord));{$ENDIF}
    end;
    dec(count);
    inc(pdest);
    inc(psrc, sourcePixelSize);
  end;
end;

procedure CopyFrom32Bit_SwapRedBlue_ReplaceZeroAlpha(psrc: PByte; pdest: PBGRAPixel; count: BGRANativeInt; sourcePixelSize: BGRAPtrInt; defaultOpacity: byte);
var sourceval: BGRANativeUInt;
  OpacityOrMask : BGRANativeUInt;
  {$IFDEF BDS}_BGRALongWord : BGRALongWord;{$ENDIF}//#
begin
  OpacityOrMask := DefaultOpacity shl TBGRAPixel_AlphaShift;
  while count > 0 do
  begin
    {$IFDEF BDS}move(psrc^, sourceval, sizeof(BGRALongWord));{$ELSE}sourceval := PBGRALongWord(psrc)^;{$ENDIF}
    if ((sourceVal shr TBGRAPixel_AlphaShift) and $ff = 0) and ((sourceval and BGRA_ColorMask) <> 0) then //if not black but transparent
    begin
      {$IFDEF BDS}_BGRALongWord{$ELSE}PBGRALongWord(pdest)^{$ENDIF} := (((sourceval and BGRA_RedMask) shr TBGRAPixel_RedShift) shl TBGRAPixel_BlueShift)
                        or (((sourceval and BGRA_BlueMask) shr TBGRAPixel_BlueShift) shl TBGRAPixel_RedShift)
                        or (sourceval and BGRA_GreenMask)
                        or OpacityOrMask;
      {$IFDEF BDS}move(_BGRALongWord, pdest^, sizeof(BGRALongWord));{$ENDIF}
    end
    else
    begin
      {$IFDEF BDS}_BGRALongWord{$ELSE}PBGRALongWord(pdest)^{$ENDIF} := (((sourceval and BGRA_RedMask) shr TBGRAPixel_RedShift) shl TBGRAPixel_BlueShift)
                        or (((sourceval and BGRA_BlueMask) shr TBGRAPixel_BlueShift) shl TBGRAPixel_RedShift)
                        or (sourceval and (BGRA_GreenMask or BGRA_AlphaMask));
      {$IFDEF BDS}move(_BGRALongWord, pdest^, sizeof(BGRALongWord));{$ENDIF}
    end;
    dec(count);
    inc(pdest);
    inc(psrc, sourcePixelSize);
  end;
end;

procedure DoCopyProc(ADestination: TBGRACustomBitmap; ACopyProc: TCopyPixelProc; AData: PByte; ABytesPerLine, ABitsPerPixel: integer; ALineOrder: TRawImageLineOrder; ADefaultOpacity: byte);
var
  n: integer;
  psource_byte, pdest_byte,
  psource_first, pdest_first: PByte;
  psource_delta, pdest_delta: integer;
begin
  if (ALineOrder = ADestination.LineOrder) and
    (ABytesPerLine = (ABitsPerPixel shr 3) * BGRACardinal(ADestination.Width)) then
    ACopyProc(AData, ADestination.Data, ADestination.NbPixels, ABitsPerPixel shr 3, ADefaultOpacity)
  else
  begin
    if ALineOrder = riloTopToBottom then
    begin
      psource_first := AData;
      psource_delta := ABytesPerLine;
    end else
    begin
      psource_first := AData + (ADestination.Height-1) * ABytesPerLine;
      psource_delta := -ABytesPerLine;
    end;

    if ADestination.LineOrder = riloTopToBottom then
    begin
      pdest_first := PByte(ADestination.Data);
      pdest_delta := ADestination.Width*sizeof(TBGRAPixel);
    end else
    begin
      pdest_first := PByte(ADestination.Data) + (ADestination.Height-1)*ADestination.Width*sizeof(TBGRAPixel);
      pdest_delta := -ADestination.Width*sizeof(TBGRAPixel);
    end;

    psource_byte := psource_first;
    pdest_byte := pdest_first;
    for n := ADestination.Height-1 downto 0 do
    begin
      ACopyProc(psource_byte, PBGRAPixel(pdest_byte), ADestination.Width, ABitsPerPixel shr 3, ADefaultOpacity);
      inc(psource_byte, psource_delta);
      inc(pdest_byte, pdest_delta);
    end;
  end;
end;

procedure ApplyRawImageMask(ADestination: TBGRACustomBitmap; const ARawImage: TRawImage);
var
  copyProc: TCopyPixelProc;
begin
  if (ARawImage.Description.MaskBitsPerPixel = 1) and (ARawImage.Mask <> nil) then
  begin
    if ARawImage.Description.BitOrder = riboBitsInOrder then
      copyProc := @ApplyMask1bit
    else
      copyProc := @ApplyMask1bitRev;
    DoCopyProc(ADestination, copyProc, ARawImage.Mask, ARawImage.Description.MaskBytesPerLine, ARawImage.Description.MaskBitsPerPixel, ARawImage.Description.LineOrder, 0);
    ADestination.InvalidateBitmap;
  end;
end;

{ Load raw image data. It must be 32bit, 24 bits or 1bit per pixel}
function LoadFromRawImageImplementation(ADestination: TBGRADefaultBitmap; const ARawImage: TRawImage;
  DefaultOpacity: byte; AlwaysReplaceAlpha: boolean; RaiseErrorOnInvalidPixelFormat: boolean): boolean;
var
  mustSwapRedBlue: boolean;
  copyProc: TCopyPixelProc;
  nbColorChannels: integer;

  function FormatError(message: string): boolean;
  begin
    if RaiseErrorOnInvalidPixelFormat then
      raise Exception.Create('Invalid raw image format. ' + message)
    else
      result := false;
  end;

begin
  if (ARawImage.Description.Width <> BGRACardinal(ADestination.Width)) or
    (ARawImage.Description.Height <> BGRACardinal(ADestination.Height)) then
    raise Exception.Create('Bitmap size is inconsistant');

  if (ADestination.Height=0) or (ADestination.Width=0) then
  begin
    result := true;
    exit;
  end;

  if ARawImage.Description.BitsPerPixel = 1 then
  begin
    if ARawImage.Description.BitOrder = riboBitsInOrder then
      copyProc := @CopyFromBW_SetAlpha
    else
      copyProc := @CopyFromBW_SetAlphaBitRev;
    DefaultOpacity := 255;
  end else
  begin
    if ((ARawImage.Description.BitsPerPixel and 7) <> 0) then
    begin
      result := FormatError(IntToStr(ARawImage.Description.Depth) + 'bit found but multiple of 8bit expected');
      exit;
    end;

    if (ARawImage.Description.BitsPerPixel < 24) then
    begin
      result := FormatError(IntToStr(ARawImage.Description.Depth) + 'bit found but at least 24bit expected');
      exit;
    end;

    nbColorChannels := 0;
    if (ARawImage.Description.RedPrec > 0)  then inc(nbColorChannels);
    if (ARawImage.Description.GreenPrec > 0)  then inc(nbColorChannels);
    if (ARawImage.Description.BluePrec > 0)  then inc(nbColorChannels);

    if (nbColorChannels < 3) then
    begin
      result := FormatError('One or more color channel is missing (RGB expected)');
      exit;
    end;

    //channels are in ARGB order
    if (ARawImage.Description.BitsPerPixel >= 32) and
       (ARawImage.Description.AlphaPrec = 8) and
      (((ARawImage.Description.AlphaShift = 0) and
      (ARawImage.Description.RedShift = 8) and
      (ARawImage.Description.GreenShift = 16) and
      (ARawImage.Description.BlueShift = 24) and
      (ARawImage.Description.ByteOrder = riboLSBFirst)) or
      ((ARawImage.Description.AlphaShift = ARawImage.Description.BitsPerPixel - 8) and
      (ARawImage.Description.RedShift = ARawImage.Description.BitsPerPixel - 16) and
      (ARawImage.Description.GreenShift = ARawImage.Description.BitsPerPixel - 24) and
      (ARawImage.Description.BlueShift = ARawImage.Description.BitsPerPixel - 32) and
      (ARawImage.Description.ByteOrder = riboMSBFirst))) then
      begin
        if AlwaysReplaceAlpha then
          copyProc := @CopyFromARGB_SetAlpha
        else if DefaultOpacity = 0 then
          copyProc := @CopyFromARGB_KeepAlpha
        else
          copyProc := @CopyFromARGB_ReplaceZeroAlpha;
      end
    else //channels are in ARGB order but alpha is not used
    if (ARawImage.Description.BitsPerPixel >= 32) and
       (ARawImage.Description.AlphaPrec = 0) and
      (((ARawImage.Description.RedShift = 8) and
      (ARawImage.Description.GreenShift = 16) and
      (ARawImage.Description.BlueShift = 24) and
      (ARawImage.Description.ByteOrder = riboLSBFirst)) or
      ((ARawImage.Description.RedShift = ARawImage.Description.BitsPerPixel - 16) and
      (ARawImage.Description.GreenShift = ARawImage.Description.BitsPerPixel - 24) and
      (ARawImage.Description.BlueShift = ARawImage.Description.BitsPerPixel - 32) and
      (ARawImage.Description.ByteOrder = riboMSBFirst))) then
      begin
        DefaultOpacity := 255;
        copyProc := @CopyFromARGB_SetAlpha;
      end
    else
    begin
      //channels are in RGB order (alpha channel may follow)
      if (ARawImage.Description.BitsPerPixel >= 24) and
         (((ARawImage.Description.RedShift = 0) and
           (ARawImage.Description.GreenShift = 8) and
           (ARawImage.Description.BlueShift = 16) and
           (ARawImage.Description.ByteOrder = riboLSBFirst)) or
          ((ARawImage.Description.RedShift = ARawImage.Description.BitsPerPixel - 8) and
           (ARawImage.Description.GreenShift = ARawImage.Description.BitsPerPixel - 16) and
           (ARawImage.Description.BlueShift = ARawImage.Description.BitsPerPixel - 24) and
           (ARawImage.Description.ByteOrder = riboMSBFirst))) then
      begin
        mustSwapRedBlue:= not TBGRAPixel_RGBAOrder;
      end
      else
      //channels are in BGR order (alpha channel may follow)
      if (ARawImage.Description.BitsPerPixel >= 24) and
         (((ARawImage.Description.BlueShift = 0) and
           (ARawImage.Description.GreenShift = 8) and
           (ARawImage.Description.RedShift = 16) and
           (ARawImage.Description.ByteOrder = riboLSBFirst)) or
          ((ARawImage.Description.BlueShift = ARawImage.Description.BitsPerPixel - 8) and
           (ARawImage.Description.GreenShift = ARawImage.Description.BitsPerPixel - 16) and
           (ARawImage.Description.RedShift = ARawImage.Description.BitsPerPixel - 24) and
           (ARawImage.Description.ByteOrder = riboMSBFirst))) then
      begin
        mustSwapRedBlue:= TBGRAPixel_RGBAOrder;
      end
      else
      begin
        result := FormatError('BitsPerPixel: ' + IntToStr(ARawImage.Description.BitsPerPixel) + ', '
          + 'RedShit: ' + IntToStr(ARawImage.Description.RedShift) + ', Prec: ' + IntToStr(ARawImage.Description.RedPrec)+ ', '
          + 'GreenShit: ' + IntToStr(ARawImage.Description.GreenShift) + ', Prec: ' + IntToStr(ARawImage.Description.GreenPrec)+ ', '
          + 'BlueShift: ' + IntToStr(ARawImage.Description.BlueShift) + ', Prec: ' + IntToStr(ARawImage.Description.BluePrec)+ ', '
          + 'AlphaShift: ' + IntToStr(ARawImage.Description.AlphaShift) + ', Prec: ' + IntToStr(ARawImage.Description.AlphaPrec) );
        exit;
      end;

      if not mustSwapRedBlue then
      begin
        if ARawImage.Description.BitsPerPixel = 24 then
          copyProc := @CopyFrom24Bit
        else
        if AlwaysReplaceAlpha or (ARawImage.Description.AlphaPrec = 0) then
          copyProc := @CopyFrom32Bit_SetAlpha
        else if DefaultOpacity = 0 then
          copyProc := @CopyFrom32Bit_KeepAlpha
        else
          copyProc := @CopyFrom32Bit_ReplaceZeroAlpha;
      end else
      begin
        if ARawImage.Description.BitsPerPixel = 24 then
          copyProc := @CopyFrom24Bit_SwapRedBlue
        else
        if AlwaysReplaceAlpha or (ARawImage.Description.AlphaPrec = 0) then
          copyProc := @CopyFrom32Bit_SwapRedBlue_SetAlpha
        else if DefaultOpacity = 0 then
          copyProc := @CopyFrom32Bit_SwapRedBlue_KeepAlpha
        else
          copyProc := @CopyFrom32Bit_SwapRedBlue_ReplaceZeroAlpha;
      end;
    end;
  end;

  DoCopyProc(ADestination, copyProc, ARawImage.Data, ARawImage.Description.BytesPerLine, ARawImage.Description.BitsPerPixel, ARawImage.Description.LineOrder, DefaultOpacity);
  ADestination.InvalidateBitmap;

  ApplyRawImageMask(ADestination, ARawImage);
  result := true;
end;

{ Draw BGRA data to a canvas with transparency }
procedure DataDrawTransparentImplementation(ACanvas: TCanvas; Rect: TRect;
  AData: Pointer; ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer);
var
  Temp:     TBitmap;
  RawImage: TRawImage;
  BitmapHandle, MaskHandle: HBitmap;
begin
  RawImage.Init;
  {$IFNDEF FPC}
  RawImage.Description.Height := AHeight;
  RawImage.Description.Width  := AWidth;
  {$ENDIF}
  RawImage.Description.Init_BPP32_B8G8R8A8_BIO_TTB(AWidth, AHeight);
  RawImage.Description.LineOrder := ALineOrder;
  RawImage.Data     := PByte(AData);
  RawImage.DataSize := AWidth * AHeight * sizeof(TBGRAPixel);
  if not RawImage_CreateBitmaps(RawImage, BitmapHandle, MaskHandle, False) then
    raise FPImageException.Create('Failed to create bitmap handle');
  Temp := TBitmap.Create;
  Temp.Handle := BitmapHandle;
  Temp.MaskHandle := MaskHandle;
  ACanvas.StretchDraw(Rect, Temp);
  Temp.Free;
end;

{ Draw BGRA data to a canvas without transparency }
procedure DataDrawOpaqueImplementation(ACanvas: TCanvas; Rect: TRect;
  AData: Pointer; ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer);
var
  Temp:      TBitmap;
  RawImage:  TRawImage;
  BitmapHandle, MaskHandle: HBitmap;
  CreateResult: boolean;
  tempShift: byte;
begin
  if (AHeight = 0) or (AWidth = 0) then
    exit;

  RawImage.Init;
  {$IFNDEF FPC}
  RawImage.Description.Height := AHeight;
  RawImage.Description.Width  := AWidth;
  {$ENDIF}
  RawImage.Description.Init_BPP32_B8G8R8_BIO_TTB(AWidth,AHeight);
  RawImage.Description.LineOrder := ALineOrder;
  RawImage.Description.LineEnd := rileDWordBoundary;
  RawImage.Data := PByte(AData);
  RawImage.DataSize:= AWidth*AHeight*sizeof(TBGRAPixel);
  if TBGRAPixel_RGBAOrder then
  begin
    tempShift := RawImage.Description.RedShift;
    RawImage.Description.RedShift := RawImage.Description.BlueShift;
    RawImage.Description.BlueShift := tempShift;
  end;
  CreateResult := RawImage_CreateBitmaps(RawImage, BitmapHandle, MaskHandle, False);

  if not CreateResult then
    raise FPImageException.Create('Failed to create bitmap handle');

  Temp := TBitmap.Create;
  Temp.Handle := BitmapHandle;
  Temp.MaskHandle := MaskHandle;
  ACanvas.StretchDraw(Rect, Temp);
  Temp.Free;
end;

procedure GetImageFromCanvasImplementation(ADestination: TBGRADefaultBitmap; CanvasSource: TCanvas; x, y: integer);
var
  bmp: TBitmap;
  subBmp: TBGRACustomBitmap;
  subRect: TRect;
  cw,ch: integer;
begin
  cw := CanvasSource.Width;
  ch := CanvasSource.Height;
  if (x < 0) or (y < 0) or (x+ADestination.Width > cw) or
    (y+ADestination.Height > ch) then
  begin
    ADestination.FillTransparent;
    if (x+ADestination.Width <= 0) or (y+ADestination.Height <= 0) or
      (x >= cw) or (y >= ch) then
      exit;

    if (x > 0) then subRect.Left := x else subRect.Left := 0;
    if (y > 0) then subRect.Top := y else subRect.Top := 0;
    if (x+ADestination.Width > cw) then subRect.Right := cw else
      subRect.Right := x+ADestination.Width;
    if (y+ADestination.Height > ch) then subRect.Bottom := ch else
      subRect.Bottom := y+ADestination.Height;

    subBmp := ADestination.NewBitmap(subRect.Right-subRect.Left,subRect.Bottom-subRect.Top);
    subBmp.GetImageFromCanvas(CanvasSource,subRect.Left,subRect.Top);
    ADestination.PutImage(subRect.Left-x,subRect.Top-y,subBmp,dmSet);
    subBmp.Free;
    exit;
  end;
  bmp := TBitmap.Create;
  bmp.PixelFormat := pf24bit;
  bmp.Width := ADestination.Width;
  bmp.Height := ADestination.Height;
  bmp.Canvas.CopyRect(Classes.rect(0, 0, ADestination.Width, ADestination.Height), CanvasSource,
    Classes.rect(x, y, x + ADestination.Width, y + ADestination.Height));
  LoadFromRawImageImplementation(ADestination, bmp.RawImage, 255, True, False);
  bmp.Free;
  ADestination.InvalidateBitmap;
end;

{ TBGRALCLPtrBitmap }

procedure TBGRALCLPtrBitmap.RebuildBitmap;
var
  RawImage: TRawImage;
  BitmapHandle, MaskHandle: HBitmap;
begin
  if FBitmap <> nil then
    FBitmap.Free;

  FBitmap := TBitmapTracker.Create(self);

  if (FWidth > 0) and (FHeight > 0) then
  begin
    RawImage.Init;
    {$IFNDEF FPC}
    RawImage.Description.Height := FHeight;
    RawImage.Description.Width  := FWidth;
    {$ENDIF}
    if TBGRAPixel_RGBAOrder then
      RawImage.Description.Init_BPP32_R8G8B8A8_BIO_TTB(FWidth, FHeight)
    else
      RawImage.Description.Init_BPP32_B8G8R8A8_BIO_TTB(FWidth, FHeight);
    RawImage.Description.LineOrder := FLineOrder;
    RawImage.Data     := PByte(FData);
    RawImage.DataSize := FWidth * FHeight * sizeof(TBGRAPixel);
    if not RawImage_CreateBitmaps(RawImage, BitmapHandle, MaskHandle, False) then
      raise FPImageException.Create('Failed to create bitmap handle');
    FBitmap.Handle     := BitmapHandle;
    FBitmap.MaskHandle := MaskHandle;
  end;

  FBitmap.Canvas.AntialiasingMode := amOff;
  FBitmapModified := False;
end;

function TBGRALCLPtrBitmap.CreatePtrBitmap(AWidth, AHeight: integer;
  AData: PBGRAPixel): TBGRAPtrBitmap;
begin
  Result:= TBGRALCLPtrBitmap.Create(AWidth,AHeight,AData);
end;

function TBGRALCLPtrBitmap.CreateDefaultFontRenderer: TBGRACustomFontRenderer;
begin
  result := TLCLFontRenderer.Create;
end;

function TBGRALCLPtrBitmap.LoadFromRawImage(ARawImage: TRawImage;
  DefaultOpacity: byte; AlwaysReplaceAlpha: boolean;
  RaiseErrorOnInvalidPixelFormat: boolean): boolean;
begin
  DiscardBitmapChange;
  result := LoadFromRawImageImplementation(self,ARawImage,DefaultOpacity,AlwaysReplaceAlpha,RaiseErrorOnInvalidPixelFormat);
end;

procedure TBGRALCLPtrBitmap.GetImageFromCanvas(CanvasSource: TCanvas; x,
  y: integer);
begin
  DiscardBitmapChange;
  GetImageFromCanvasImplementation(self,CanvasSource,x,y);
end;

procedure TBGRALCLPtrBitmap.DataDrawTransparent(ACanvas: TCanvas; Rect: TRect;
  AData: Pointer; ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer);
begin
  DataDrawTransparentImplementation(ACanvas, Rect, AData, ALineOrder, AWidth, AHeight);
end;

procedure TBGRALCLPtrBitmap.DataDrawOpaque(ACanvas: TCanvas; Rect: TRect;
  AData: Pointer; ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer);
begin
  DataDrawOpaqueImplementation(ACanvas, Rect, AData, ALineOrder, AWidth, AHeight);
end;

function TBGRALCLBitmap.LoadFromRawImage(ARawImage: TRawImage;
  DefaultOpacity: byte; AlwaysReplaceAlpha: boolean;
  RaiseErrorOnInvalidPixelFormat: boolean): boolean;
begin
  DiscardBitmapChange;
  result := LoadFromRawImageImplementation(self,ARawImage,DefaultOpacity,AlwaysReplaceAlpha,RaiseErrorOnInvalidPixelFormat);
end;

function TBGRALCLBitmap.CreateDefaultFontRenderer: TBGRACustomFontRenderer;
begin
  result := TLCLFontRenderer.Create;
end;

procedure TBGRALCLBitmap.DoLoadFromBitmap;
begin
  if FBitmap <> nil then
  begin
    LoadFromRawImage(FBitmap.RawImage, FCanvasOpacity);
    if FAlphaCorrectionNeeded then DoAlphaCorrection;
  end;
end;

procedure TBGRALCLBitmap.RebuildBitmap;
var
  RawImage: TRawImage;
  BitmapHandle, MaskHandle: HBitmap;
begin
  if FBitmap <> nil then
    FBitmap.Free;

  FBitmap := TBitmapTracker.Create(self);

  if (FWidth > 0) and (FHeight > 0) then
  begin
    RawImage.Init;
    {$IFNDEF FPC}
    RawImage.Description.Height := FHeight;
    RawImage.Description.Width  := FWidth;
    {$ENDIF}
    if TBGRAPixel_RGBAOrder then
      RawImage.Description.Init_BPP32_R8G8B8A8_BIO_TTB(FWidth, FHeight)
    else
      RawImage.Description.Init_BPP32_B8G8R8A8_BIO_TTB(FWidth, FHeight);
    RawImage.Description.LineOrder := FLineOrder;
    RawImage.Data     := PByte(FData);
    RawImage.DataSize := FWidth * FHeight * sizeof(TBGRAPixel);
    if not RawImage_CreateBitmaps(RawImage, BitmapHandle, MaskHandle, False) then
      raise FPImageException.Create('Failed to create bitmap handle');
    FBitmap.Handle     := BitmapHandle;
    FBitmap.MaskHandle := MaskHandle;
  end;

  FBitmap.Canvas.AntialiasingMode := amOff;
  FBitmapModified := False;
  FAlphaCorrectionNeeded:= false;
end;

function TBGRALCLBitmap.CreatePtrBitmap(AWidth, AHeight: integer;
  AData: PBGRAPixel): TBGRAPtrBitmap;
begin
  Result:= TBGRALCLPtrBitmap.Create(AWidth, AHeight, AData);
end;

procedure TBGRALCLBitmap.Assign(Source: TPersistent);
begin
  if Source is TRasterImage then
  begin
    AssignRasterImage(TRasterImage(Source));
  end else
    inherited Assign(Source);

  {$IFDEF FPC}//#
  if Source is TCursorImage then
  begin
    HotSpot := TCursorImage(Source).HotSpot;
    ExtractXorMask;
  end
  else if Source is TIcon then
  begin
    HotSpot := Point(0,0);
    ExtractXorMask;
  end;
  {$ENDIF}
end;

procedure TBGRALCLBitmap.AssignRasterImage(ARaster: TRasterImage);
var TempBmp: TBitmap;
{$IFNDEF FPC}
var
  RawImg: TRawImage;
  i, i2, iSize, iForIni, iForFin, iLineSize : Integer;
  PixelFormat : TPixelFormat;
{$ENDIF}
begin
  DiscardBitmapChange;
  SetSize(ARaster.Width, ARaster.Height);
{$IFNDEF FPC}
  PixelFormat := ARaster.PixelFormat;
  ARaster.PixelFormat := pf32bit;
  RawImg.Init;
  if RawImage_FromBitmap(RawImg, ARaster.Handle, ARaster.MaskHandle) then
  begin
    ARaster.AssignRawImage(RawImg);
  end;
  ARaster.PixelFormat := PixelFormat;
{$ENDIF}
  if LoadFromRawImage(ARaster.RawImage,0,False,False) then
  begin
    If Empty then
    begin
      AlphaFill(255); // if bitmap seems to be empty, assume
                      // it is an opaque bitmap without alpha channel
      ApplyRawImageMask(self, ARaster.RawImage);
    end;
  end else                //#
  if (ARaster is TBitmap) {$IFDEF FPC}or (ARaster is TCustomIcon){$ENDIF} then
  begin //try to convert
    TempBmp := TBitmap.Create;
    TempBmp.Width := ARaster.Width;
    TempBmp.Height := ARaster.Height;
    TempBmp.Canvas.Draw(0,0,ARaster);
    try
      LoadFromRawImage(TempBmp.RawImage,255,False,true);
      ApplyRawImageMask(self, ARaster.RawImage);
    finally
      TempBmp.Free;
    end;
  end else
    raise Exception.Create('Unable to convert image to 24 bit');
end;

procedure TBGRALCLBitmap.ExtractXorMask;
var
  y, x: Integer;
  p: PBGRAPixel;
begin
  DiscardXorMask;
  for y := 0 to Height-1 do
  begin
    p := ScanLine[y];
    for x := 0 to Width-1 do
    begin
      if (p^.alpha = 0) and (PBGRADWord(p)^<>0) then
      begin
        NeedXorMask;
        XorMask.SetPixel(x,y, p^);
      end;
      inc(p);
    end;
  end;
end;

procedure TBGRALCLBitmap.DataDrawTransparent(ACanvas: TCanvas; Rect: TRect;
  AData: Pointer; ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer);
begin
  DataDrawTransparentImplementation(ACanvas, Rect, AData, ALineOrder, AWidth, AHeight);
end;

procedure TBGRALCLBitmap.DataDrawOpaque(ACanvas: TCanvas; ARect: TRect;
  AData: Pointer; ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer);
begin
  DataDrawOpaqueImplementation(ACanvas, ARect, AData, ALineOrder, AWidth, AHeight);
end;

procedure TBGRALCLBitmap.GetImageFromCanvas(CanvasSource: TCanvas; x, y: integer
  );
begin
  DiscardBitmapChange;
  GetImageFromCanvasImplementation(self,CanvasSource,x,y);
end;

procedure TBGRALCLBitmap.LoadFromDevice(DC: HDC);
var
  rawImage: TRawImage;
  sourceSize: TPoint;
begin
  sourceSize := Point(0,0);
  GetDeviceSize(DC, sourceSize);
  if (sourceSize.x = 0) or (sourceSize.y = 0) then
  begin
    SetSize(0,0);
    exit;
  end;
  try
    if not RawImage_FromDevice(rawImage, DC, rect(0,0,sourceSize.x,sourceSize.y)) then
      raise Exception.Create('Cannot get raw image from device');
    SetSize(rawImage.Description.Width, rawImage.Description.Height);
    LoadFromRawImage(rawImage,255);
  finally
    rawImage.FreeData;
  end;
end;

procedure TBGRALCLBitmap.LoadFromDevice(DC: HDC; ARect: TRect);
var
  rawImage: TRawImage;
begin
  if (ARect.Right <= ARect.Left) or (ARect.Bottom <= ARect.Top) then
  begin
    SetSize(0,0);
    exit;
  end;
  try
    if not RawImage_FromDevice(rawImage, DC, ARect) then
      raise Exception.Create('Cannot get raw image from device');
    SetSize(rawImage.Description.Width, rawImage.Description.Height);
    LoadFromRawImage(rawImage,255);
  finally
    rawImage.FreeData;
  end;
end;

procedure TBGRALCLBitmap.TakeScreenshotOfPrimaryMonitor;
var primaryDC: THandle;
begin
  primaryDC := {$IFDEF FPC}LCLIntf{$ELSE}windows{$ENDIF}.GetDC(0);
  LoadFromDevice(primaryDC);
  {$IFDEF FPC}LCLIntf{$ELSE}windows{$ENDIF}.ReleaseDC(0, primaryDC);
end;

procedure TBGRALCLBitmap.TakeScreenshot(ARect: TRect);
var primaryDC: THandle;
begin
  primaryDC := {$IFDEF FPC}LCLIntf{$ELSE}windows{$ENDIF}.GetDC(0);
  LoadFromDevice(primaryDC, ARect);
  {$IFDEF FPC}LCLIntf{$ELSE}windows{$ENDIF}.ReleaseDC(0, primaryDC);
end;

end.

