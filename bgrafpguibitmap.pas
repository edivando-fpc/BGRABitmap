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


Unit BGRAfpGUIBitmap;

{$i bgrabitmap.inc}{$H+}

interface

uses
  SysUtils, Classes, BGRAGraphics, BGRABitmapTypes, BGRADefaultBitmap,
  BGRAFreeType, {$IFDEF FPC} EasyLazFreeType, LazFreeTypeFontCollection,{$ENDIF}
  BGRACanvas;

type

  { TBGRAfpGUIBitmap }

  TBGRAfpGUIBitmap = class(TBGRADefaultBitmap)
  private
    FPseudoCanvas: TBGRACanvas;
    function GetPseudoCanvas: TBGRACanvas;
    function GetBitmapTransparent: boolean;
    procedure SetBitmapTransparent(AValue: boolean);
  protected
    procedure RebuildBitmap; override;
    function CreateDefaultFontRenderer: TBGRACustomFontRenderer; override;
    function LoadFromRawImage(ARawImage: TRawImage; DefaultOpacity: byte;
      AlwaysReplaceAlpha: boolean=False; {%H-}RaiseErrorOnInvalidPixelFormat: boolean
      =True): boolean; override;
    procedure Init; override;
    procedure FreeData; override;
    procedure ReallocData; override;
    procedure FreeBitmap; override;
    procedure NotAvailable;
  public
    destructor Destroy; override;
    class procedure AddFreeTypeFontFolder(ADirectory: string; AUTF8: boolean = false);
    class procedure AddFreeTypeFontFile(AFilename: string; AUTF8: boolean = false);
    procedure Draw(ACanvas: TCanvas; x, y: integer; {%H-}Opaque: boolean=True); override;
    procedure Draw(ACanvas: TCanvas; Rect: TRect; {%H-}Opaque: boolean=True); override;
    procedure Draw(ACanvas: TGUICanvas; x, y: integer; {%H-}Opaque: boolean=True); overload;
    procedure Draw(ACanvas: TGUICanvas; Rect: TRect; {%H-}Opaque: boolean=True); overload;
    procedure GetImageFromCanvas({%H-}CanvasSource: TCanvas; {%H-}x, {%H-}y: integer); override; //not available
    procedure DataDrawTransparent(ACanvas: TCanvas; Rect: TRect; AData: Pointer;
      ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer); override;
    procedure DataDrawOpaque(ACanvas: TCanvas; Rect: TRect; AData: Pointer;
      ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer); override;
    procedure TakeScreenshot({%H-}ARect: TRect); override; //not available
    procedure TakeScreenshotOfPrimaryMonitor; override; //not available
    procedure LoadFromDevice({%H-}DC: System.THandle); override; //not available
    procedure LoadFromDevice({%H-}DC: System.THandle; {%H-}ARect: TRect); override; //not available
    property BitmapTransparent: boolean read GetBitmapTransparent write SetBitmapTransparent;
    property Canvas: TBGRACanvas read GetPseudoCanvas;
  end;

implementation

{ TBGRAfpGUIBitmap }

function TBGRAfpGUIBitmap.GetBitmapTransparent: boolean;
begin
  result := FBitmap.Transparent;
end;

function TBGRAfpGUIBitmap.GetPseudoCanvas: TBGRACanvas;
begin
  if FPseudoCanvas = nil then
  begin
    FPseudoCanvas := TBGRACanvas.Create(self);
    FPseudoCanvas.AntialiasingMode := amOff;
  end;
  result := FPseudoCanvas;
end;

procedure TBGRAfpGUIBitmap.SetBitmapTransparent(AValue: boolean);
begin
  if FBitmap.Transparent <> AValue then
  begin
    FBitmap.Transparent:= AValue;
    InvalidateBitmap;
  end;
end;

procedure TBGRAfpGUIBitmap.RebuildBitmap;
var pmask, pmaskline: PByte;
  pdata: PBGRAPixel;
  raw: TRawImage;
  x,y,bit,masklinesize,curmaskbyte: BGRANativeUInt;
begin
  if FBitmap.Transparent then
  begin
    raw := FBitmap.RawImage;
    masklinesize := ((Width+31) div 32)*4;
    pmaskline := FBitmap.RawImage.MaskData;
    pdata := raw.ImageData;
    for y := 0 to Height-1 do
      begin
        pmask:= pmaskline;
        bit := $80;
        curmaskbyte := 0;
        for x := Width-1 downto 0 do
        begin
          if pdata^.alpha >= $80 then
            curmaskbyte := curmaskbyte or bit;
          bit := bit shr 1;
          if bit = 0 then
          begin
            bit := $80;
            pmask^ := curmaskbyte;
            inc(pmask);
            curmaskbyte := 0;
          end;
          inc(pdata);
        end;
        if bit <> $80 then
          pmask^ := curmaskbyte;
        inc(pmaskline, masklinesize);
      end;
  end;
  FBitmap.RawImage.UpdateImage;
end;

function TBGRAfpGUIBitmap.CreateDefaultFontRenderer: TBGRACustomFontRenderer;
begin
  result := TBGRAFreeTypeFontRenderer.Create;
end;

function TBGRAfpGUIBitmap.LoadFromRawImage(ARawImage: TRawImage;
  DefaultOpacity: byte; AlwaysReplaceAlpha: boolean;
  RaiseErrorOnInvalidPixelFormat: boolean): boolean;
var
  lineSize: integer;
  y: Integer;
begin
  if (ARawImage.Width <> Width) or
    (ARawImage.Height <> Height) then
    raise Exception.Create('Bitmap size is inconsistant');

  lineSize := Width*sizeof(TBGRAPixel);
  for y := 0 to Height-1 do
    move(ARawImage.ScanLine[y]^, ScanLine[y]^, lineSize);
  if AlwaysReplaceAlpha then AlphaFill(DefaultOpacity);
  result := true;
end;

procedure TBGRAfpGUIBitmap.Init;
begin
  inherited Init;
  FBitmap := TBitmap.Create;
  FontAntialias:= true;
end;

procedure TBGRAfpGUIBitmap.ReallocData;
begin
  FBitmap.Width := Width;
  FBitmap.Height:= Height;
  FData := FBitmap.RawImage.ImageData;
  InvalidateBitmap;
  FScanPtr := nil;
end;

procedure TBGRAfpGUIBitmap.FreeData;
begin
  //nothing
end;

procedure TBGRAfpGUIBitmap.FreeBitmap;
begin
  //nothing
end;

procedure TBGRAfpGUIBitmap.NotAvailable;
begin
  raise exception.Create('Function not available with fpGUI');
end;

destructor TBGRAfpGUIBitmap.Destroy;
begin
  FreeAndNil(FBitmap);
  FreeAndNil(FPseudoCanvas);
  inherited Destroy;
end;

class procedure TBGRAfpGUIBitmap.AddFreeTypeFontFolder(ADirectory: string; AUTF8: boolean);
begin
  if AUTF8 then ADirectory:= Utf8ToAnsi(ADirectory);
  EasyLazFreeType.FontCollection.AddFolder(ADirectory);
end;

class procedure TBGRAfpGUIBitmap.AddFreeTypeFontFile(AFilename: string; AUTF8: boolean);
begin
  if AUTF8 then AFilename:= Utf8ToAnsi(AFilename);
  EasyLazFreeType.FontCollection.AddFile(AFilename);
end;

procedure TBGRAfpGUIBitmap.Draw(ACanvas: TCanvas; x, y: integer; Opaque: boolean);
begin
  Draw(ACanvas.GUICanvas, x, y, Opaque);
end;

procedure TBGRAfpGUIBitmap.Draw(ACanvas: TCanvas; Rect: TRect; Opaque: boolean);
begin
  Draw(ACanvas.GUICanvas, Rect, Opaque);
end;

procedure TBGRAfpGUIBitmap.Draw(ACanvas: TGUICanvas; x, y: integer;
  Opaque: boolean);
begin
  BitmapTransparent := not Opaque;
  ACanvas.DrawImage(x,y, Bitmap.RawImage);
end;

procedure TBGRAfpGUIBitmap.Draw(ACanvas: TGUICanvas; Rect: TRect;
  Opaque: boolean);
begin
  BitmapTransparent := not Opaque;
  ACanvas.StretchDraw(rect.left,rect.top,rect.right-rect.left,rect.bottom-rect.top, FBitmap.RawImage);
end;

procedure TBGRAfpGUIBitmap.GetImageFromCanvas(CanvasSource: TCanvas; x,
  y: integer);
begin
  NotAvailable;
end;

procedure TBGRAfpGUIBitmap.DataDrawTransparent(ACanvas: TCanvas; Rect: TRect;
  AData: Pointer; ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer);
var temp: TBGRAfpGUIBitmap;
begin
  temp := TBGRAfpGUIBitmap.Create(AWidth,AHeight);
  move(AData^, temp.Data^, temp.NbPixels*sizeof(TBGRAPixel));
  if ALineOrder <> temp.LineOrder then temp.VerticalFlip;
  temp.Draw(ACanvas, Rect, False);
  temp.Free;
end;

procedure TBGRAfpGUIBitmap.DataDrawOpaque(ACanvas: TCanvas; Rect: TRect;
  AData: Pointer; ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer);
var temp: TBGRAfpGUIBitmap;
begin
  temp := TBGRAfpGUIBitmap.Create(AWidth,AHeight);
  move(AData^, temp.Data^, temp.NbPixels*sizeof(TBGRAPixel));
  if ALineOrder <> temp.LineOrder then temp.VerticalFlip;
  temp.Draw(ACanvas, Rect, True);
  temp.Free;
end;

procedure TBGRAfpGUIBitmap.TakeScreenshot(ARect: TRect);
begin
  NotAvailable;
end;

procedure TBGRAfpGUIBitmap.TakeScreenshotOfPrimaryMonitor;
begin
  NotAvailable;
end;

procedure TBGRAfpGUIBitmap.LoadFromDevice(DC: System.THandle);
begin
  NotAvailable;
end;

procedure TBGRAfpGUIBitmap.LoadFromDevice(DC: System.THandle; ARect: TRect);
begin
  NotAvailable;
end;

end.

