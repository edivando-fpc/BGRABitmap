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


Unit BGRAMacBitmap;

{$i bgrabitmap.inc}{$H+}

interface

uses
  Classes, SysUtils, BGRALCLBitmap, BGRAGraphics, BGRABitmapTypes;

type

  { TBGRAMacBitmap }

  TBGRAMacBitmap = class(TBGRALCLBitmap)
     procedure DataDrawOpaque(ACanvas: TCanvas; Rect: TRect; AData: Pointer;
       ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer); override;
  end;

implementation

uses LCLType, GraphType, LCLIntf, FPImage;

procedure DataDrawOpaqueImplementation(ACanvas: TCanvas; Rect: TRect;
  AData: Pointer; ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer);
type
  PARGB = ^TARGB;
  TARGB = packed record
    alpha,red,green,blue: byte;
  end;

var
  Temp:      TBitmap;
  RawImage:  TRawImage;
  BitmapHandle, MaskHandle: HBitmap;
  CreateResult: boolean;
  psrc: PBGRAPixel;
  pdest: PARGB;
  n: Integer;
begin
  if (AHeight = 0) or (AWidth = 0) then
    exit;

  RawImage.Init;
  RawImage.Description.Init_BPP32_A8R8G8B8_BIO_TTB(AWidth,AHeight);
  RawImage.Description.Depth := 24;
  RawImage.Description.AlphaPrec := 0;
  RawImage.Description.LineOrder := ALineOrder;
  RawImage.Description.LineEnd := rileDWordBoundary;
  RawImage.CreateData(False);
  psrc := PBGRAPixel(AData);
  pdest := PARGB(RawImage.Data);
  for n := AWidth*AHeight-1 downto 0 do
  begin
    pdest^.alpha := 255;
    pdest^.red := psrc^.red;
    pdest^.green := psrc^.green;
    pdest^.blue := psrc^.blue;
    inc(pdest);
    inc(psrc);
  end;
  CreateResult := RawImage_CreateBitmaps(RawImage, BitmapHandle, MaskHandle, False);
  RawImage.FreeData;

  if not CreateResult then
    raise FPImageException.Create('Failed to create bitmap handle');

  Temp := TBitmap.Create;
  Temp.Handle := BitmapHandle;
  Temp.MaskHandle := MaskHandle;
  ACanvas.StretchDraw(Rect, Temp);
  Temp.Free;
end;

{ TBGRAMacBitmap }

procedure TBGRAMacBitmap.DataDrawOpaque(ACanvas: TCanvas; Rect: TRect;
  AData: Pointer; ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer);
begin
  DataDrawOpaqueImplementation(ACanvas, Rect, AData, ALineOrder, AWidth, AHeight);
end;

end.

