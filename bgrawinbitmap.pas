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


// This Unit should NOT be added to the 'uses' clause.
//It contains accelerations for Windows. Notably, it
//provides direct access to bitmap data.

unit BGRAWinBitmap;

{$i bgrabitmap.inc}{$H+}

interface

uses
  Classes, SysUtils, BGRATypes, {$IFNDEF FPC}Types, {$ENDIF} BGRALCLBitmap, Windows, Graphics, BGRAGraphics, GraphType;

type
  { TBGRAWinBitmap }

  TBGRAWinBitmap = class(TBGRALCLBitmap)
  private
    procedure AlphaCorrectionNeeded;
  protected
    DIB_SectionHandle: HBITMAP;
    FReversed: boolean;
    function DIBitmapInfo(AWidth, AHeight: integer): TBitmapInfo;

    procedure ReallocData; override;
    procedure FreeData; override;

    procedure RebuildBitmap; override;
    procedure FreeBitmap; override;

    procedure Init; override;
    function GetBitmap: TBitmap; override;

  public
    procedure LoadFromBitmapIfNeeded; override;
    procedure Draw(ACanvas: TCanvas; x, y: integer; Opaque: boolean=True); overload; override;
    procedure Draw(ACanvas: TCanvas; Rect: TRect; Opaque: boolean = True); overload; override;
    procedure DataDrawOpaque(ACanvas: TCanvas; ARect: TRect; AData: Pointer;
      ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer); override;
    procedure GetImageFromCanvas(CanvasSource: TCanvas; x, y: integer); override;
  end;


{$IFnDEF FPC}//#
type
  TRGBAQuad = record
    Blue:  Byte;
    Green: Byte;
    Red:   Byte;
    Alpha: Byte;
  end;
  PRGBAQuad = ^TRGBAQuad;
  TRGBAQuadArray = array of TRGBAQuad;

function _AlphaBlend(hdcDest: HDC; nXOriginDest, nYOriginDest, nWidthDest, nHeightDest: Integer; hdcSrc: HDC; nXOriginSrc, nYOriginSrc, nWidthSrc, nHeightSrc: Integer; blendFunction: TBlendFunction): BOOL; stdcall;
function GetBitmapOrder(AWinBmp: Windows.TBitmap; ABitmap: HBITMAP): TRawImageLineOrder;
function GetBitmapBytes(AWinBmp: Windows.TBitmap; ABitmap: HBITMAP; const ARect: TRect; ALineEnd: TRawImageLineEnd; ALineOrder: TRawImageLineOrder; out AData: Pointer; out ADataSize: PtrUInt): Boolean;

var
  msimg32handle: THandle;
  AlphaBlend: function(hdcDest: HDC; nXOriginDest, nYOriginDest, nWidthDest, nHeightDest: Integer; hdcSrc: HDC;
              nXOriginSrc, nYOriginSrc, nWidthSrc, nHeightSrc: Integer; blendFunction: TBlendFunction): BOOL; stdcall;
{$ENDIF}

implementation

uses BGRADefaultBitmap, BGRABitmapTypes;

type
  { TWinBitmapTracker }

  TWinBitmapTracker = class(TBitmap)
  protected
    FUser: TBGRAWinBitmap;
    {$IFnDEF FPC}//#
    FMasked : boolean;
    function GetMasked: Boolean;
    procedure SetMasked(AValue: Boolean); virtual;
    {$ENDIF}
    procedure Changed(Sender: TObject); override;
  public
    constructor Create(AUser: TBGRAWinBitmap); overload;
    {$IFnDEF FPC}//#
    procedure Draw(DestCanvas: TCanvas; const DestRect: TRect); override;
    function StretchMaskBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC;
             XSrc, YSrc, SrcWidth, SrcHeight: Integer; Mask: HBITMAP; XMask, YMask: Integer; Rop: DWORD): Boolean;
    property Masked: Boolean read GetMasked write SetMasked default False;
    {$ENDIF}
  end;

procedure TWinBitmapTracker.Changed(Sender: TObject);
begin
  if FUser <> nil then
    FUser.AlphaCorrectionNeeded;
  inherited Changed(Sender);
end;

constructor TWinBitmapTracker.Create(AUser: TBGRAWinBitmap);
begin
  {$IFnDEF FPC}//#
  FMasked := False;
  {$ENDIF}
  FUser := AUser;
  inherited Create;
end;

{ TBGRAWinBitmap }

procedure TBGRAWinBitmap.FreeData;
begin
  if DIB_SectionHandle <> 0 then
  begin
    DeleteObject(DIB_SectionHandle);
    FData := nil;
    DIB_SectionHandle := 0;
  end;
end;

procedure TBGRAWinBitmap.RebuildBitmap;
begin
  if FBitmap = nil then
  begin
    FBitmap := TWinBitmapTracker.Create(self);
    FBitmap.Handle := DIB_SectionHandle;
  end;
end;

procedure TBGRAWinBitmap.FreeBitmap;
begin
  if FBitmap <> nil then
  begin
    FBitmap.Handle := 0;
    FBitmap.Free;
    FBitmap := nil;
  end;
end;

procedure TBGRAWinBitmap.Init;
begin
  inherited Init;
  FLineOrder := riloBottomToTop;
end;

function TBGRAWinBitmap.GetBitmap: TBitmap;
begin
  Result:=inherited GetBitmap;
  if (LineOrder = riloTopToBottom) and not FReversed then
  begin
    VerticalFlip;
    FReversed:= true;
  end;
end;

procedure TBGRAWinBitmap.LoadFromBitmapIfNeeded;
begin
  if FReversed then
  begin
    FReversed := false;
    VerticalFlip;
  end;
  if FAlphaCorrectionNeeded then
  begin
    DoAlphaCorrection;
  end;
end;

procedure TBGRAWinBitmap.Draw(ACanvas: TCanvas; x, y: integer; Opaque: boolean);
begin
  if self = nil then exit;
  Draw(ACanvas, Classes.Rect(x,y,x+Width,y+Height), Opaque);
end;

procedure TBGRAWinBitmap.Draw(ACanvas: TCanvas; Rect: TRect; Opaque: boolean);
var
  info:      TBITMAPINFO;
begin
  if (self = nil) or (Width = 0) or (Height = 0) then exit;
  if TBGRAPixel_RGBAOrder then SwapRedBlue;
  if Opaque then
  begin
    info := DIBitmapInfo(Width, Height);
    if LineOrder = riloTopToBottom then
      StretchDIBits(ACanvas.Handle, Rect.Left, Rect.Bottom, Rect.Right -
        Rect.Left, Rect.Top - Rect.Bottom,
        0, 0, Width, Height, Data, info, DIB_RGB_COLORS, SRCCOPY)
    else
      StretchDIBits(ACanvas.Handle, Rect.Left, Rect.Top, Rect.Right -
        Rect.Left, Rect.Bottom - Rect.Top,
        0, 0, Width, Height, Data, info, DIB_RGB_COLORS, SRCCOPY);
  end
  else
  begin
    if Empty then exit;
    if LineOrder = riloTopToBottom then VerticalFlip;
    LoadFromBitmapIfNeeded;
    ACanvas.StretchDraw(Rect, Bitmap);
    if LineOrder = riloTopToBottom then VerticalFlip;
  end;
  if TBGRAPixel_RGBAOrder then SwapRedBlue;
end;

procedure TBGRAWinBitmap.DataDrawOpaque(ACanvas: TCanvas; ARect: TRect;
  AData: Pointer; ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer);
var
  info:      TBITMAPINFO;
  IsFlipped: boolean;
  Temp:      TBGRAPtrBitmap;
begin
  Temp      := nil;
  IsFlipped := False;
  if ALineOrder = riloTopToBottom then
  begin
    Temp := TBGRAPtrBitmap.Create(AWidth, AHeight, AData);
    Temp.VerticalFlip;
    IsFlipped := True;
  end;
  if TBGRAPixel_RGBAOrder then
  begin
    if Temp = nil then
      Temp := TBGRAPtrBitmap.Create(AWidth, AHeight, AData);
    Temp.SwapRedBlue;
  end;

  info := DIBitmapInfo(AWidth, AHeight);
  StretchDIBits(ACanvas.Handle, ARect.Left, ARect.Top, ARect.Right -
    ARect.Left, ARect.Bottom - ARect.Top,
    0, 0, AWidth, AHeight, AData, info, DIB_RGB_COLORS, SRCCOPY);

  if Temp <> nil then
  begin
    if TBGRAPixel_RGBAOrder then Temp.SwapRedBlue;
    if IsFlipped then
      Temp.VerticalFlip;
    Temp.Free;
  end;
end;

procedure TBGRAWinBitmap.AlphaCorrectionNeeded; //@{$ifdef inline}inline;{$endif}
begin
  FAlphaCorrectionNeeded := True;
end;

function TBGRAWinBitmap.DIBitmapInfo(AWidth, AHeight: integer): TBitmapInfo;
begin
  with {%H-}Result.bmiHeader do
  begin
    biSize      := sizeof(Result.bmiHeader);
    biWidth     := AWidth;
    biHeight    := AHeight;
    biPlanes    := 1;
    biBitCount  := 32;
    biCompression := BI_RGB;
    biSizeImage := 0;
    biXPelsPerMeter := 0;
    biYPelsPerMeter := 0;
    biClrUsed   := 0;
    biClrImportant := 0;
  end;
end;

procedure TBGRAWinBitmap.ReallocData;
var
  ScreenDC: HDC;
  info:     TBitmapInfo;
begin
  FreeData;
  if (Width <> 0) and (Height <> 0) then
  begin
    ScreenDC := GetDC(0);
    info     := DIBitmapInfo(Width, Height);
    DIB_SectionHandle := CreateDIBSection(ScreenDC, info, DIB_RGB_COLORS, {$IFDEF FPC}FData{$ELSE}Pointer(FData){$ENDIF}, 0, 0);

    if (NbPixels > 0) and (FData = nil) then
      raise EOutOfMemory.Create('TBGRAWinBitmap.ReallocBitmap: Windows error ' +
        IntToStr(GetLastError));

    ReleaseDC(0, ScreenDC);
  end;
  InvalidateBitmap;
end;

procedure TBGRAWinBitmap.GetImageFromCanvas(CanvasSource: TCanvas; x, y: integer);
begin
  self.Canvas.CopyRect(Classes.rect(0, 0, Width, Height), CanvasSource,
    Classes.rect(X, Y, X + Width, Y + Height));
end;

{$IFnDEF FPC}//#
function TWinBitmapTracker.GetMasked: Boolean;
begin
  Result := FMasked;
end;

procedure TWinBitmapTracker.SetMasked(AValue: Boolean);
begin
  if AValue = Masked then Exit;

  FMasked := AValue;
  Changed(Self);
end;

procedure TWinBitmapTracker.Draw(DestCanvas: TCanvas; const DestRect: TRect);
var
  UseMaskHandle: HBitmap;
  SrcDC: hDC;
  DestDC: hDC;
begin
  if (Width=0) or (Height=0)
  then Exit;

  HandleNeeded;
  if not HandleAllocated then Exit;

  if Masked then
    UseMaskHandle:=MaskHandle
  else
    UseMaskHandle:=0;

  SrcDC := Canvas.Handle;
//  DestCanvas.Changing;
  DestDC := DestCanvas.Handle;
  StretchMaskBlt(DestDC,
          DestRect.Left,DestRect.Top,
          DestRect.Right-DestRect.Left,DestRect.Bottom-DestRect.Top,
          SrcDC,0,0,Width,Height, UseMaskHandle,0,0,DestCanvas.CopyMode);
//  DestCanvas.Changed;
end;

function TWinBitmapTracker.StretchMaskBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; Mask: HBITMAP; XMask, YMask: Integer; Rop: DWORD): Boolean;

  function CreatePremultipliedBitmap(AWinBmp: Windows.TBitmap; ABitmap: HBITMAP; out AAlphaBmp: HBITMAP): Boolean;
  var
    Data: Pointer;
    Pixel: PRGBAQuad;
    ByteCount: PtrUInt;
    Header: Windows.TBitmapInfoHeader;
    HasAlpha0, HasAlphaN, HasAlpha255: Boolean;
  begin
    // todo, process only requested rectangle
    Result := False;
    if not GetBitmapBytes(AWinBmp, ABitmap, Rect(0, 0, AWinBmp.bmWidth, AWinBmp.bmHeight), rileDWordBoundary, riloTopToBottom, Data, ByteCount)
    then Exit;

    HasAlpha0 := False;
    HasAlphaN := False;
    HasAlpha255 := False;
    Pixel := Data;
    ByteCount := ByteCount shr 2;
    while ByteCount > 0 do
    begin
      case Pixel^.Alpha of
        0: begin
          Pixel^.Red   := 0;
          Pixel^.Green := 0;
          Pixel^.Blue  := 0;
          HasAlpha0 := True;
        end;
        255: begin
          HasAlpha255 := True;
        end;
      else
        Pixel^.Red   := (Pixel^.Red   * Pixel^.Alpha) div 255;
        Pixel^.Green := (Pixel^.Green * Pixel^.Alpha) div 255;
        Pixel^.Blue  := (Pixel^.Blue  * Pixel^.Alpha) div 255;
        HasAlphaN := True;
      end;
      Inc(Pixel);
      Dec(ByteCount);
    end;

    // only create bitmap when not opaque or not fully transparent
    // (all zero alpha is unlikly for alpha bitmap, so it is probably a bitmap without alpha channel)
    Result := HasAlphaN or (HasAlpha0 and HasAlpha255);
    if Result
    then begin
      FillChar(Header, SizeOf(Header), 0);
      Header.biSize := SizeOf(Header);
      Header.biWidth := AWinBmp.bmWidth;
      Header.biHeight := -AWinBmp.bmHeight;
      Header.biPlanes := 1;
      Header.biBitCount := 32;
      Header.biCompression := BI_RGB;

      AAlphaBmp := Windows.CreateDIBitmap(SrcDC, Header, CBM_INIT, Data, Windows.TBitmapInfo((@Header)^), DIB_RGB_COLORS);
    end;
    Freemem(Data);
  end;
var
  MaskDC, CopyDC, AlphaDC: HDC;
  MaskObj, CopyObj, AlphaObj: HGDIOBJ;
  PrevTextColor, PrevBkColor: COLORREF;
  WinBmp: Windows.TBitmap;
  Bmp, CopyBmp, AlphaBmp: HBITMAP;
  HasAlpha: Boolean;
  Blend: TBlendFunction;
begin
  //DbgDumpBitmap(Mask, 'StretchMaskBlt - Mask');

  // check if the Src has an alpha channel
  Bmp := Windows.GetCurrentObject(SrcDC, OBJ_BITMAP);
  // get info
  HasAlpha := (Windows.GetObject(bmp, SizeOf(WinBmp), @WinBmp) <> 0)
          and (WinBmp.bmBitsPixel = 32)
          and CreatePremultipliedBitmap(WinBmp, Bmp, AlphaBmp);

  if HasAlpha
  then begin
    AlphaDC := Windows.CreateCompatibleDC(SrcDC);
    AlphaObj := Windows.SelectObject(AlphaDC, AlphaBmp);

    // init blendfunction
    Blend.BlendOp := AC_SRC_OVER;
    Blend.BlendFlags := 0;
    Blend.SourceConstantAlpha := 255;
    Blend.AlphaFormat := AC_SRC_ALPHA;
  end
  else begin
    // ToDo: Initialize AlphaDC, AlphaObj and Blend.
  end;

  Windows.SetBrushOrgEx(DestDC, 0, 0, nil);
  if Mask = 0 then
  begin
    if HasAlpha
    then begin
      AlphaBlend(DestDC, X, Y, Width, Height, AlphaDC, XSrc, YSrc, SrcWidth, SrcHeight, Blend);
    end
    else begin
      if (Width = SrcWidth) and (Height = SrcHeight) then
      begin
        Result := Windows.BitBlt(DestDC, X, Y, Width, Height, SrcDC, XSrc, YSrc, Rop);
      end
      else begin
        Result := Windows.StretchBlt(DestDC, X, Y, Width, Height, SrcDC, XSrc, YSrc, SrcWidth, SrcHeight, Rop);
      end;
    end;
  end
  else begin
    MaskDC := Windows.CreateCompatibleDC(DestDC);
    MaskObj := Windows.SelectObject(MaskDC, Mask);

    PrevTextColor := Windows.SetTextColor(DestDC, $00000000);
    PrevBkColor := Windows.SetBkColor(DestDC, $00FFFFFF);

    if HasAlpha
    then begin
      // create copy of masked destination
      CopyDC := Windows.CreateCompatibleDC(DestDC);
      CopyBmp := Windows.CreateCompatibleBitmap(DestDC, Width, Height);
      CopyObj := Windows.SelectObject(CopyDC, CopyBmp);
      Windows.BitBlt(CopyDC, 0, 0, Width, Height, DestDC, X, Y, SRCCOPY);
      // wipe non masked area -> white
      Windows.SetTextColor(CopyDC, $00FFFFFF);
      Windows.SetBkColor(CopyDC, $00000000);
      if (Width = SrcWidth) and (Height = SrcHeight)
      then Windows.BitBlt(CopyDC, 0, 0, Width, Height, MaskDC, XSrc, YSrc, SRCPAINT)
      else Windows.StretchBlt(CopyDC, 0, 0, Width, Height, MaskDC, XSrc, YSrc, SrcWidth, SrcHeight, SRCPAINT);

      // copy source
      AlphaBlend(DestDC, X, Y, Width, Height, AlphaDC, XSrc, YSrc, SrcWidth, SrcHeight, Blend);
      // wipe masked area -> white
      if (Width = SrcWidth) and (Height = SrcHeight)
      then Windows.BitBlt(DestDC, X, Y, Width, Height, MaskDC, XSrc, YSrc, SRCPAINT)
      else Windows.StretchBlt(DestDC, X, Y, Width, Height, MaskDC, XSrc, YSrc, SrcWidth, SrcHeight, SRCPAINT);

      // paint copied destination
      Windows.BitBlt(DestDC, X, Y, Width, Height, CopyDC, 0, 0, SRCAND);

      // Restore stuff
      Windows.SelectObject(CopyDC, CopyObj);
      Windows.DeleteObject(CopyBmp);
      Windows.DeleteDC(CopyDC);
    end
    else begin
      if (Width = SrcWidth) and (Height = SrcHeight) then
      begin
        Windows.BitBlt(DestDC, X, Y, Width, Height, SrcDC,  XSrc, YSrc, SRCINVERT);
        Windows.BitBlt(DestDC, X, Y, Width, Height, MaskDC, XSrc, YSrc, SRCAND);
        Windows.BitBlt(DestDC, X, Y, Width, Height, SrcDC,  XSrc, YSrc, SRCINVERT);
      end
      else begin
        Windows.StretchBlt(DestDC, X, Y, Width, Height, SrcDC,  XSrc, YSrc, SrcWidth, SrcHeight, SRCINVERT);
        Windows.StretchBlt(DestDC, X, Y, Width, Height, MaskDC, XSrc, YSrc, SrcWidth, SrcHeight, SRCAND);
        Windows.StretchBlt(DestDC, X, Y, Width, Height, SrcDC,  XSrc, YSrc, SrcWidth, SrcHeight, SRCINVERT);
      end;
    end;
    Windows.SetTextColor(DestDC, PrevTextColor);
    Windows.SetBkColor(DestDC, PrevBkColor);
    Windows.SelectObject(MaskDC, MaskObj);
    Windows.DeleteDC(MaskDC);
  end;

  if HasAlpha
  then begin
    Windows.SelectObject(AlphaDC, AlphaObj);
    Windows.DeleteObject(AlphaBmp);
    Windows.DeleteDC(AlphaDC);
  end;

  Result := true;
end;

function GetBitmapOrder(AWinBmp: Windows.TBitmap; ABitmap: HBITMAP): TRawImageLineOrder;
var
  SrcPixel: Pointer; //absolute AWinBmp.bmBits;
  OrgPixel, TstPixel: Cardinal;
  Scanline: Pointer;
  DC: HDC;
  Info: record
    Header: Windows.TBitmapInfoHeader;
    Colors: array[Byte] of Cardinal; // reserve extra color for colormasks
  end;

  FullScanLine: Boolean; // win9x requires a full scanline to be retrieved
                         // others won't fail when one pixel is requested
begin
  SrcPixel := AWinBmp.bmBits;

  if AWinBmp.bmBits = nil
  then begin
    // no DIBsection so always bottom-up
    Exit(riloBottomToTop);
  end;

  // try to figure out the orientation of the given bitmap.
  // Unfortunately MS doesn't provide a direct function for this.
  // So modify the first pixel to see if it changes. This pixel is always part
  // of the first scanline of the given bitmap.
  // When we request the data through GetDIBits as bottom-up, windows adjusts
  // the data when it is a top-down. So if the pixel doesn't change the bitmap
  // was internally a top-down image.

  FullScanLine := Win32Platform = VER_PLATFORM_WIN32_WINDOWS;
  if FullScanLine
  then GetMem(ScanLine, AWinBmp.bmWidthBytes)
  else ScanLine := nil;

  FillChar(Info.Header, sizeof(Windows.TBitmapInfoHeader), 0);
  Info.Header.biSize := sizeof(Windows.TBitmapInfoHeader);
  DC := Windows.GetDC(0);
  if Windows.GetDIBits(DC, ABitmap, 0, 1, nil, Windows.PBitmapInfo(@Info)^, DIB_RGB_COLORS) = 0
  then begin
    // failed ???
    Windows.ReleaseDC(0, DC);
    Exit(riloBottomToTop);
  end;

  // Get only 1 pixel (or full scanline for win9x)
  OrgPixel := 0;
  if FullScanLine
  then begin
    if Windows.GetDIBits(DC, ABitmap, 0, 1, ScanLine, Windows.PBitmapInfo(@Info)^, DIB_RGB_COLORS) = 0
    then
    else OrgPixel := PCardinal(ScanLine)^;
  end
  else begin
    Info.Header.biWidth := 1;
    if Windows.GetDIBits(DC, ABitmap, 0, 1, @OrgPixel, Windows.PBitmapInfo(@Info)^, DIB_RGB_COLORS) = 0
    then
  end;

  // modify pixel
  PCardinal(SrcPixel)^ := not PCardinal(SrcPixel)^;

  // get test
  TstPixel := 0;
  if FullScanLine
  then begin
    if Windows.GetDIBits(DC, ABitmap, 0, 1, ScanLine, Windows.PBitmapInfo(@Info)^, DIB_RGB_COLORS) = 0
    then
    else TstPixel := PCardinal(ScanLine)^;
  end
  else begin
    if Windows.GetDIBits(DC, ABitmap, 0, 1, @TstPixel, Windows.PBitmapInfo(@Info)^, DIB_RGB_COLORS) = 0
    then
  end;

  if OrgPixel = TstPixel
  then Result := riloTopToBottom
  else Result := riloBottomToTop;

  // restore pixel & cleanup
  PCardinal(SrcPixel)^ := not PCardinal(SrcPixel)^;
  Windows.ReleaseDC(0, DC);
  if FullScanLine
  then FreeMem(Scanline);
end;

function GetBitmapBytes(AWinBmp: Windows.TBitmap; ABitmap: HBITMAP; const ARect: TRect; ALineEnd: TRawImageLineEnd; ALineOrder: TRawImageLineOrder; out AData: Pointer; out ADataSize: PtrUInt): Boolean;
var
  DC: HDC;
  Info: record
    Header: Windows.TBitmapInfoHeader;
    Colors: array[Byte] of TRGBQuad; // reserve extra colors for palette (256 max)
  end;
  H: Cardinal;
  R: TRect;
  SrcData: PByte;
  SrcSize: PtrUInt;
  SrcLineBytes: Cardinal;
  SrcLineOrder: TRawImageLineOrder;
  StartScan: Integer;
begin
  SrcLineOrder := GetBitmapOrder(AWinBmp, ABitmap);
  SrcLineBytes := (AWinBmp.bmWidthBytes + 3) and not 3;

  if AWinBmp.bmBits <> nil
  then begin
    // this is bitmapsection data :) we can just copy the bits

    // We cannot trust windows with bmWidthBytes. Use SrcLineBytes which takes
    // DWORD alignment into consideration
    with AWinBmp do
      Result := CopyImageData(bmWidth, bmHeight, SrcLineBytes, bmBitsPixel, bmBits, ARect, SrcLineOrder, ALineOrder, ALineEnd, AData, ADataSize);
    Exit;
  end;

  // retrieve the data though GetDIBits

  // initialize bitmapinfo structure
  Info.Header.biSize := sizeof(Info.Header);
  Info.Header.biPlanes := 1;
  Info.Header.biBitCount := AWinBmp.bmBitsPixel;
  Info.Header.biCompression := BI_RGB;
  Info.Header.biSizeImage := 0;

  Info.Header.biWidth := AWinBmp.bmWidth;
  H := ARect.Bottom - ARect.Top;
  // request a top-down DIB
  if AWinBmp.bmHeight > 0
  then begin
    Info.Header.biHeight := -AWinBmp.bmHeight;
    StartScan := AWinBmp.bmHeight - ARect.Bottom;
  end
  else begin
    Info.Header.biHeight := AWinBmp.bmHeight;
    StartScan := ARect.Top;
  end;
  // adjust height
  if StartScan < 0
  then begin
    Inc(H, StartScan);
    StartScan := 0;
  end;

  // alloc buffer
  SrcSize := SrcLineBytes * H;
  GetMem(SrcData, SrcSize);

  DC := Windows.GetDC(0);
  Result := Windows.GetDIBits(DC, ABitmap, StartScan, H, SrcData, Windows.PBitmapInfo(@Info)^, DIB_RGB_COLORS) <> 0;
  Windows.ReleaseDC(0, DC);

  // since we only got the needed scanlines, adjust top and bottom
  R.Left := ARect.Left;
  R.Top := 0;
  R.Right := ARect.Right;
  R.Bottom := H;

  with Info.Header do
    Result := Result and CopyImageData(biWidth, H, SrcLineBytes, biBitCount, SrcData, R, riloTopToBottom, ALineOrder, ALineEnd, AData, ADataSize);

  FreeMem(SrcData);
end;

function _AlphaBlend(hdcDest: HDC; nXOriginDest, nYOriginDest, nWidthDest, nHeightDest: Integer; hdcSrc: HDC; nXOriginSrc, nYOriginSrc, nWidthSrc, nHeightSrc: Integer; blendFunction: TBlendFunction): BOOL; stdcall;
var
  SCA: PByte;// absolute blendFunction.SourceConstantAlpha;

  R: TRect;
  DC, TmpDC: HDC;
  OldBmp, OldTmpBmp, SrcBmp, DstBmp, TmpBmp, AlphaBmp: HBITMAP;
  StretchSrc: Boolean;
  SrcSection, DstSection: TDIBSection;
  Info: record
    Header: TBitmapInfoHeader;
    Colors: array[0..3] of Cardinal; // reserve extra color for colormasks
  end;

  SrcBytesPtr, DstBytesPtr, TmpBytesPtr, AlphaBytesPtr: Pointer;
  SrcLinePtr, DstLinePtr: PByte;
  CleanupSrc, CleanupSrcPtr, CleanupDst, CleanupAlpha: Boolean;
  SrcSize: PtrUInt;
  SrcPixelBytes, DstPixelBytes: Byte;
  SrcRowStride, DstRowStride: Integer;
  SrcLineOrder: TRawImageLineOrder;

  X, Y: Integer;
  SrcRGBA, TmpRGBA, DstRGBA: PRGBAQuad;
  SrcAlpha: PByte;
  NotAlpha: Byte;
begin
  SCA := @blendFunction.SourceConstantAlpha;

  if nXOriginSrc < 0 then Exit(False);
  if nYOriginSrc < 0 then Exit(False);
  if nWidthSrc < 0 then Exit(False);
  if nHeightSrc < 0 then Exit(False);
  if nWidthDest < 0 then Exit(False);
  if nHeightDest < 0 then Exit(False);

  if blendFunction.SourceConstantAlpha = 0
  then Exit(True); // nothing to do

  if (blendFunction.AlphaFormat = 0)
  and (blendFunction.SourceConstantAlpha = 255)
  then begin
    // simple strechblt
    Result := StretchBlt(hdcDest, nXOriginDest, nYOriginDest, nWidthDest, nHeightDest, hdcSrc, nXOriginSrc, nYOriginSrc, nWidthSrc, nHeightSrc, SRCCOPY);
    Exit;
  end;

  // get source info, atleast bitmap, if possible also section
  if GetObjectType(hdcSrc) <> OBJ_MEMDC then Exit(False);
  SrcBmp := GetCurrentObject(hdcSrc, OBJ_BITMAP);
  if GetObject(SrcBmp, SizeOf(SrcSection), @SrcSection) = 0 then Exit(False);
  if nXOriginSrc + nWidthSrc > SrcSection.dsBm.bmWidth then Exit(False);
  if nYOriginSrc + nHeightSrc > SrcSection.dsBm.bmHeight then Exit(False);

  if (blendFunction.AlphaFormat = AC_SRC_ALPHA) and (SrcSection.dsBm.bmBitsPixel <> 32) then Exit(False); // invalid

  // get destination info, atleast bitmap, if possible also section
  {if WindowsVersion in [wv95, wv98]
  then begin
    // under windows 98 GetObjectType() sometimes produce AV inside and
    // as result our debugger stopes and show exception
    // lazarus is not alone application with such problem under windows 98
    // here is workaround for windows 9x
    DstBmp := GetCurrentObject(hdcDest, OBJ_BITMAP);
    DstSection.dsBm.bmBits := nil;
    if (DstBmp <> 0)
    and ((GetObject(DstBmp, SizeOf(DstSection), @DstSection) < SizeOf(TDIBSection)) or (DstSection.dsBm.bmBits = nil))
    then DstBmp := 0;
  end
  else }
  begin
    if GetObjectType(hdcDest) = OBJ_MEMDC
    then DstBmp := GetCurrentObject(hdcDest, OBJ_BITMAP)
    else DstBmp := 0;
    if (DstBmp <> 0) and (GetObject(DstBmp, SizeOf(DstSection), @DstSection) = 0)
    then DstBmp := 0;
  end;

  if (DstBmp = 0)
  then begin
    // GetCurrentObject can only be used on memory devices,
    // so fill in some values manually
    DstSection.dsBm.bmWidth := GetDeviceCaps(hdcDest, HORZRES);
    DstSection.dsBm.bmHeight := GetDeviceCaps(hdcDest, VERTRES);
    DstSection.dsBm.bmBitsPixel := GetDeviceCaps(hdcDest, BITSPIXEL);
    DstSection.dsBm.bmBits := nil;
  end;

  // docs doesn't require dest retangle inside dest.
  // however if dest rect is outside the destination, we're done here
  if nXOriginDest + nWidthDest < 0 then Exit(True);
  if nYOriginDest + nHeightDest < 0 then Exit(True);
  if nXOriginDest >= DstSection.dsBm.bmWidth then Exit(True);
  if nYOriginDest >= DstSection.dsBm.bmHeight then Exit(True);

  // get lineorder of source so we use the right direction
  SrcLineOrder := GetBitmapOrder(SrcSection.dsBm, SrcBmp);

  // setup info shared by alpha, source and destination bytes
  FillChar(Info, sizeof(Info), 0);
  Info.Header.biSize := sizeof(Windows.TBitmapInfoHeader);
  Info.Header.biWidth := nWidthDest;
  if SrcLineOrder = riloBottomToTop
  then Info.Header.biHeight := nHeightDest
  else Info.Header.biHeight := -nHeightDest;
  Info.Header.biPlanes := 1;
  Info.Header.biBitCount := 32;
  Info.Header.biSizeImage := nWidthDest * nHeightDest * 4;
  Info.Header.biCompression := BI_BITFIELDS;
  // when 24bpp, CE only supports B8G8R8 encoding
  Info.Colors[0] := $FF0000; {le-red}
  Info.Colors[1] := $00FF00; {le-green}
  Info.Colors[2] := $0000FF; {le-blue}

  Result := False;
  StretchSrc := (nWidthDest <> nWidthSrc) or (nHeightDest <> nHeightSrc);
  if StretchSrc
  then begin
    // we need to strech the source

    // create alphabmp
    if blendFunction.AlphaFormat = AC_SRC_ALPHA
    then begin
      // create alpha source data
      R := Classes.Rect(nXOriginSrc, nYOriginSrc, nXOriginSrc + nWidthSrc, nYOriginSrc + nHeightSrc);
      if not GetBitmapBytes(SrcSection.dsBm, SrcBmp, R, rileDWordBoundary, SrcLineOrder, SrcBytesPtr, SrcSize) then Exit(False);

      // set info to source size
      Info.Header.biWidth := nWidthSrc;
      if SrcLineOrder = riloBottomToTop
      then Info.Header.biHeight := nHeightSrc
      else Info.Header.biHeight := -nHeightSrc;
      Info.Header.biSizeImage := nWidthSrc * nHeightSrc * 4;

      // create temp bitmap to store orginal grayscale alpha
      TmpBmp := CreateDIBSection(hdcSrc, PBitmapInfo(@Info)^, DIB_RGB_COLORS, TmpBytesPtr, 0, 0);
      if TmpBmp = 0 then Exit(False);
      if TmpBytesPtr = nil
      then begin
        FreeMem(SrcBytesPtr);
        DeleteObject(TmpBmp);
        Exit(False);
      end;

      // create grayscale image from alpha
      TmpRGBA := TmpBytesPtr;
      SrcRGBA := SrcBytesPtr;
      while SrcSize > 0 do
      begin
        TmpRGBA^.Blue := SrcRGBA^.Alpha;
        TmpRGBA^.Green := SrcRGBA^.Alpha;
        TmpRGBA^.Red := SrcRGBA^.Alpha;
        TmpRGBA^.Alpha := 255;
        Inc(SrcRGBA);
        Inc(TmpRGBA);
        Dec(SrcSize, 4);
      end;

      // restore to destination size
      Info.Header.biWidth := nWidthDest;
      if SrcLineOrder = riloBottomToTop
      then Info.Header.biHeight := nHeightDest
      else Info.Header.biHeight := -nHeightDest;
      Info.Header.biSizeImage := nWidthDest * nHeightDest * 4;

      // create bitmap to store stretched grayscale alpha
      AlphaBmp := CreateDIBSection(hdcSrc, PBitmapInfo(@Info)^, DIB_RGB_COLORS, AlphaBytesPtr, 0, 0);
      if (AlphaBmp = 0) or (AlphaBytesPtr = nil)
      then begin
        FreeMem(SrcBytesPtr);
        DeleteObject(TmpBmp);
        DeleteObject(AlphaBmp);
        Exit(False);
      end;

      // stretch grayscale alpha bitmap
      DC := CreateCompatibleDC(hdcSrc);
      OldBmp := SelectObject(DC, AlphaBmp);
      TmpDC := CreateCompatibleDC(hdcSrc);
      OldTmpBmp := SelectObject(TmpDC, TmpBmp);
      StretchBlt(DC, 0, 0, nWidthDest, nHeightDest, TmpDC, 0, 0, nWidthSrc, nHeightSrc, SRCCOPY);
      SelectObject(DC, OldBmp);
      DeleteDC(DC);
      SelectObject(TmpDC, OldTmpBmp);
      DeleteDC(TmpDC);
      DeleteObject(TmpBmp);
      FreeMem(SrcBytesPtr);

      // as long as AlphaBmp exists, AlphaBytesPtr is valid.
      CleanupAlpha := True;
    end
    else begin
      CleanupAlpha := False;
      AlphaBmp := INVALID_HANDLE_VALUE;
    end;

    // create new srcbmp
    SrcBmp := CreateDIBSection(hdcSrc, PBitmapInfo(@Info)^, DIB_RGB_COLORS, SrcBytesPtr, 0, 0);
    if (SrcBmp = 0) or (SrcBytesPtr = nil)
    then begin
      DeleteObject(AlphaBmp);
      DeleteObject(SrcBmp);
      Exit(False);
    end;
    SrcSize := Info.Header.biSizeImage;
    CleanupSrc := True;
    CleanupSrcPtr := False;
    SrcPixelBytes := 4;
    SrcRowStride := nWidthDest * SrcPixelBytes;

    DC := CreateCompatibleDC(hdcSrc);
    OldBmp := SelectObject(DC, SrcBmp);
    StretchBlt(DC, 0, 0, nWidthDest, nHeightDest, hdcSrc, nXOriginSrc, nYOriginSrc, nWidthSrc, nHeightSrc, SRCCOPY);
    SelectObject(DC, OldBmp);
    DeleteDC(DC);

    // adjust source size
    nWidthSrc := nWidthDest;
    nHeightSrc := nHeightDest;
    nXOriginSrc := 0;
    nYOriginSrc := 0;
  end
  else begin
    // only get source data
    SrcPixelBytes := SrcSection.dsBm.bmBitsPixel shr 3;
    if SrcSection.dsBm.bmBits <> nil
    then begin
      // source is a dibsection :)
      SrcBytesPtr := SrcSection.dsBm.bmBits;
      SrcRowStride := SrcSection.dsBm.bmWidthBytes;
      CleanupSrc := False;
      CleanupSrcPtr := False;
    end
    else begin
      R := Classes.Rect(nXOriginSrc, nYOriginSrc, nXOriginSrc + nWidthSrc, nYOriginSrc + nHeightSrc);
      if not GetBitmapBytes(SrcSection.dsBm, SrcBmp, R, rileDWordBoundary, SrcLineOrder, SrcBytesPtr, SrcSize) then Exit;
      SrcRowStride := nWidthSrc * SrcPixelBytes;
      CleanupSrc := False;
      CleanupSrcPtr := True;
      nXOriginSrc := 0;
      nYOriginSrc := 0;
    end;
    AlphaBytesPtr := nil;
    CleanupAlpha := False;
  end;

  // if a palette destination or destination isn't a section, create a temp DIB
  if (DstSection.dsBm.bmBitsPixel < 24)
  or (DstSection.dsBm.bmBits = nil)
  or (DstSection.dsBmih.biCompression <> BI_RGB)
  then begin
    // create temp dib
    DstBmp := CreateDIBSection(hdcSrc, PBitmapInfo(@Info)^, DIB_RGB_COLORS, DstBytesPtr, 0, 0);
    // copy destination
    DC := CreateCompatibleDC(hdcDest);
    OldBmp := SelectObject(DC, DstBmp);
    BitBlt(DC, 0, 0, nWidthDest, nHeightDest, hdcDest, nXOriginDest, nYOriginDest, SRCCOPY);
    SelectObject(DC, OldBmp);
    DeleteDC(DC);
    DstPixelBytes := 4;
    DstRowStride := nWidthDest * DstPixelBytes;
    CleanupDst := True;
  end
  else begin
    DstBytesPtr := DstSection.dsBm.bmBits;
    DstPixelBytes := DstSection.dsBm.bmBitsPixel shr 3;
    DstRowStride := DstSection.dsBm.bmWidthBytes;
    Inc(PByte(DstBytesPtr), nXOriginDest + nYOriginDest * DstRowStride);
    CleanupDst := False;
  end;

  // blend image
  SrcLinePtr := SrcBytesPtr;
  Inc(SrcLinePtr, nXOriginSrc * SrcPixelBytes + nYOriginSrc * SrcRowStride);
  DstLinePtr := DstBytesPtr;

  SrcAlpha := nil;
  if blendFunction.AlphaFormat = AC_SRC_ALPHA
  then begin
    if AlphaBytesPtr <> nil
    then SrcAlpha := AlphaBytesPtr;

    if SCA^ {blendFunction.SourceConstantAlpha} = 255
    then begin
      for y := 1 to nHeightDest do
      begin
        SrcRGBA := Pointer(SrcLinePtr);
        if AlphaBytesPtr = nil
        then SrcAlpha := @SrcRGBA^.Alpha;
        DstRGBA := Pointer(DstLinePtr);
        for x := 1 to nWidthDest do
        begin
          if SrcAlpha^ <> 0
          then begin
            NotAlpha := not SrcAlpha^;
            DstRGBA^.Red   := SrcRgba^.Red   + (DstRGBA^.Red   * NotAlpha) div 255;
            DstRGBA^.Green := SrcRgba^.Green + (DstRGBA^.Green * NotAlpha) div 255;
            DstRGBA^.Blue  := SrcRgba^.Blue  + (DstRGBA^.Blue  * NotAlpha) div 255;
            if DstPixelBytes = 4
            then DstRGBA^.Alpha := SrcAlpha^ + (DstRGBA^.Alpha * NotAlpha) div 255;
          end;
          Inc(SrcRGBA);
          Inc(SrcAlpha, 4);
          Inc(PByte(DstRGBA), DstPixelBytes);
        end;
        Inc(SrcLinePtr, SrcRowStride);
        Inc(DstLinePtr, DstRowStride);
      end;
    end
    else begin
      for y := 1 to nHeightDest do
      begin
        SrcRGBA := Pointer(SrcLinePtr);
        if AlphaBytesPtr = nil
        then SrcAlpha := @SrcRGBA^.Alpha;
        DstRGBA := Pointer(DstLinePtr);
        for x := 1 to nWidthDest do
        begin
          if SrcAlpha^ <> 0
          then begin
            NotAlpha := not SrcAlpha^;
            DstRGBA^.Red   := (SrcRgba^.Red   * SCA^ + DstRGBA^.Red   * NotAlpha) div 255;
            DstRGBA^.Green := (SrcRgba^.Green * SCA^ + DstRGBA^.Green * NotAlpha) div 255;
            DstRGBA^.Blue  := (SrcRgba^.Blue  * SCA^ + DstRGBA^.Blue  * NotAlpha) div 255;
            if DstPixelBytes = 4
            then DstRGBA^.Alpha := (SrcAlpha^ * SCA^ + DstRGBA^.Alpha * NotAlpha) div 255;
          end;
          Inc(SrcRGBA);
          Inc(SrcAlpha, 4);
          Inc(PByte(DstRGBA), DstPixelBytes);
        end;
        Inc(SrcLinePtr, SrcRowStride);
        Inc(DstLinePtr, DstRowStride);
      end;
    end;
  end
  else begin
    // no source alpha
    NotAlpha := not SCA^;
    for y := 1 to nHeightDest do
    begin
      SrcRGBA := Pointer(SrcLinePtr);
      if AlphaBytesPtr = nil
      then SrcAlpha := @SrcRGBA^.Alpha;
      DstRGBA := Pointer(DstLinePtr);
      for x := 1 to nWidthDest do
      begin
        DstRGBA^.Red :=   (SrcRGBA^.Red   * SCA^ + DstRGBA^.Red   * NotAlpha) div 255;
        DstRGBA^.Green := (SrcRGBA^.Green * SCA^ + DstRGBA^.Green * NotAlpha) div 255;
        DstRGBA^.Blue :=  (SrcRGBA^.Blue  * SCA^ + DstRGBA^.Blue  * NotAlpha) div 255;
        if (DstPixelBytes = 4) and (SrcPixelBytes = 4)
        then DstRGBA^.Alpha := (SrcAlpha^ * SCA^ + DstRGBA^.Alpha * NotAlpha) div 255;
        Inc(PByte(SrcRGBA), SrcPixelBytes);
        Inc(PByte(DstRGBA), DstPixelBytes);
        Inc(SrcAlpha, 4);
      end;
      Inc(SrcLinePtr, SrcRowStride);
      Inc(DstLinePtr, DstRowStride);
    end;
  end;

  // Replace destination if needed and do cleanup
  if CleanupDst
  then begin
    DC := CreateCompatibleDC(hdcDest);
    OldBmp := SelectObject(DC, DstBmp);
    BitBlt(hdcDest, nXOriginDest, nYOriginDest, nWidthDest, nHeightDest, DC, 0, 0, SRCCOPY);
    SelectObject(DC, OldBmp);
    DeleteDC(DC);
    DeleteObject(DstBmp);
  end;
  if CleanupSrc
  then DeleteObject(SrcBmp);
  if CleanupSrcPtr
  then FreeMem(SrcBytesPtr);
  if CleanupAlpha
  then DeleteObject(AlphaBmp);
end;

procedure Initialize;
var
  p: Pointer;
begin
  // defaults
  //Pointer(GradientFill) := @_GradientFill;
  AlphaBlend    := _AlphaBlend;
  msimg32handle := 0;
  msimg32handle := LoadLibrary('msimg32.dll');

  if msimg32handle <> 0 then
  begin
    //if WindowsVersion <> wv98 then begin
    p := GetProcAddress(msimg32handle, 'AlphaBlend');
    if p <> nil then
      AlphaBlend := p;
    //end;

    {p := GetProcAddress(msimg32handle, 'GradientFill');
    if p <> nil
    then Pointer(GradientFill) := p;}
  end;
end;

procedure Finalize;
begin
  AlphaBlend := _AlphaBlend;
  if msimg32handle <> 0 then
    FreeLibrary(msimg32handle);
  msimg32handle := 0;
end;


initialization
  Initialize;

finalization
  Finalize;

{$ENDIF}

end.

