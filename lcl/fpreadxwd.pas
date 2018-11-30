{*****************************************************************************}
{
    This file is part of the Free Pascal's "Free Components Library".
    Copyright (c) 2003 by Mazen NEIFER of the Free Pascal development team

    BMP reader implementation.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{*****************************************************************************}
{ 08/2005 by Giulio Bernardi:
   - Added support for 16 and 15 bpp bitmaps.
   - If we have bpp <= 8 make an indexed image instead of converting it to RGB
   - Support for RLE4 and RLE8 decoding
   - Support for top-down bitmaps
}

{$i FPImage.inc}{$H+}

unit FPReadXWD;

interface

uses FPImage, classes, sysutils, Types;

const
  XWD_FILE_VERSION = 7;
  sz_XWDheader = 100;
  sz_XWDColor = 12;

{ Values in the file are most significant byte first. }

type
  TXWDFileHeader = record
    { header_size = SIZEOF(XWDheader) + length of null-terminated
    * window name. }
    header_size: Cardinal;

    file_version: Cardinal;	// = XWD_FILE_VERSION above */
    pixmap_format: Cardinal;	// ZPixmap or XYPixmap */
    pixmap_depth: Cardinal;	// Pixmap depth */
    pixmap_width: Cardinal;	// Pixmap width */
    pixmap_height: Cardinal;	// Pixmap height */
    xoffset: Cardinal;		// Bitmap x offset, normally 0 */
    byte_order: Cardinal;	// of image data: MSBFirst, LSBFirst */

    { bitmap_unit applies to bitmaps (depth 1 format XY) only.
    * It is the number of bits that each scanline is padded to. }
    bitmap_unit: Cardinal;

    bitmap_bit_order: Cardinal;	// bitmaps only: MSBFirst, LSBFirst */

    { bitmap_pad applies to pixmaps (non-bitmaps) only.
    * It is the number of bits that each scanline is padded to. }
    bitmap_pad: Cardinal;

    bits_per_pixel: Cardinal;	// Bits per pixel */

    { bytes_per_line is pixmap_width padded to bitmap_unit (bitmaps)
    * or bitmap_pad (pixmaps).  It is the delta (in bytes) to get
    * to the same x position on an adjacent row. }
    bytes_per_line: Cardinal;
    visual_class: Cardinal;	// Class of colormap
    red_mask: Cardinal;		// Z red mask
    green_mask: Cardinal;	// Z green mask
    blue_mask: Cardinal;	// Z blue mask
    bits_per_rgb: Cardinal;	// Log2 of distinct color values
    colormap_entries: Cardinal; // Number of entries in colormap; not used?
    ncolors: Cardinal;		// Number of XWDColor structures
    window_width: Cardinal;	// Window width
    window_height: Cardinal;	// Window height
    window_x: Cardinal;		// Window upper left X coordinate
    window_y: Cardinal;		// Window upper left Y coordinate
    window_bdrwidth: Cardinal;	// Window border width
  end;

{ Null-terminated window name follows the above structure. }

{ Next comes XWDColor structures, at offset XWDFileHeader.header_size in
 * the file.  XWDFileHeader.ncolors tells how many XWDColor structures
 * there are.
 }

  TXWDColor = record
    pixel: Cardinal;
    red: Word;
    green: Word;
    blue: Word;
    flags: AnsiChar;
    pad: AnsiChar;
  end;

{ Last comes the image data in the format described by XWDFileHeader. }

type
  TXWDColors = array of TXWDColor;

  { TFPReaderXWD }

  TFPReaderXWD = class (TFPCustomImageReader)
    private
      continue: boolean;              // needed for onprogress event
      percent: byte;
      percentinterval : longword;
      percentacc : longword;
      Rect : TRect;
      procedure SwapXWDFileHeader(var Header: TXWDFileHeader);
      procedure SwapXWDColor(var Color: TXWDColor);
      procedure WriteScanLine(Row: Integer; Img: TFPCustomImage);
    protected
      XWDFileHeader: TXWDFileHeader;  // The header, as read from the file
      WindowName: array of AnsiChar;
      XWDColors: TXWDColors;
      LineBuf: PByte;                 // Buffer for 1 line

      // required by TFPCustomImageReader
      procedure InternalRead  (Stream:TStream; Img:TFPCustomImage); override;
      function  InternalCheck (Stream:TStream) : boolean; override;
    public
      constructor Create; override;
      destructor Destroy; override;
  end;

implementation

//==============================================================================
// Endian utils
//
// Copied from LCLProc unit
//==============================================================================
{$R-}
function BEtoN(const AValue: DWord): DWord;
begin
  Result := (AValue shl 24)
         or ((AValue and $0000FF00) shl 8)
         or ((AValue and $00FF0000) shr 8)
         or (AValue shr 24);
end;


constructor TFPReaderXWD.create;
begin
  inherited create;

end;

destructor TFPReaderXWD.Destroy;
begin
  If (LineBuf<>Nil) then
    begin
    FreeMem(LineBuf);
    LineBuf:=Nil;
    end;

  SetLength(WindowName, 0);

  SetLength(XWDColors, 0);

  inherited destroy;
end;

procedure TFPReaderXWD.SwapXWDColor(var Color: TXWDColor);
begin
  Color.pixel := BEtoN(Color.pixel);

  Color.red := swap(Color.red);
  Color.green := swap(Color.green);
  Color.blue := swap(Color.blue);
end;

procedure TFPReaderXWD.SwapXWDFileHeader(var Header: TXWDFileHeader);
begin
  Header.header_size := BEtoN(Header.header_size);
  Header.file_version := BEtoN(Header.file_version);
  Header.pixmap_format := BEtoN(Header.pixmap_format);
  Header.pixmap_depth := BEtoN(Header.pixmap_depth);
  Header.pixmap_width := BEtoN(Header.pixmap_width);
  Header.pixmap_height := BEtoN(Header.pixmap_height);
  Header.xoffset := BEtoN(Header.xoffset);
  Header.byte_order := BEtoN(Header.byte_order);
  Header.bitmap_unit := BEtoN(Header.bitmap_unit);
  Header.bitmap_unit := BEtoN(Header.bitmap_bit_order);
  Header.bitmap_pad := BEtoN(Header.bitmap_pad);
  Header.bits_per_pixel := BEtoN(Header.bits_per_pixel);
  Header.bytes_per_line := BEtoN(Header.bytes_per_line);
  Header.visual_class := BEtoN(Header.visual_class);
  Header.red_mask := BEtoN(Header.red_mask);
  Header.green_mask := BEtoN(Header.green_mask);
  Header.blue_mask := BEtoN(Header.blue_mask);
  Header.bits_per_rgb := BEtoN(Header.bits_per_rgb);
  Header.colormap_entries := BEtoN(Header.colormap_entries);
  Header.ncolors := BEtoN(Header.ncolors);
  Header.window_width := BEtoN(Header.window_width);
  Header.window_height := BEtoN(Header.window_height);
  Header.window_x := BEtoN(Header.window_x);
  Header.window_y := BEtoN(Header.window_y);
  Header.window_bdrwidth := BEtoN(Header.window_bdrwidth);
end;

procedure TFPReaderXWD.WriteScanLine(Row : Integer; Img : TFPCustomImage);
var
  Column: Integer;
  buffer: Cardinal;
  MyColor: TFPColor;
begin
  MyColor.alpha := 0;
  
  case XWDFileHeader.bits_per_pixel of
   1 :
     for Column:=0 to Img.Width-1 do
       if ((LineBuf[Column div 8] shr (7-(Column and 7)) ) and 1) <> 0 then
         img.Pixels[Column,Row]:=1
       else
         img.Pixels[Column,Row]:=0;
   4 :
      for Column:=0 to img.Width-1 do
        img.Pixels[Column,Row]:=(LineBuf[Column div 2] shr (((Column+1) and 1)*4)) and $0f;
   8 :
      for Column:=0 to img.Width-1 do
        img.Pixels[Column,Row]:=LineBuf[Column];
   16 :
      for Column:=0 to img.Width-1 do
        img.Pixels[Column,Row]:=LineBuf[Column];
   24 :
      for Column:=0 to img.Width-1 do
        img.Pixels[Column,Row]:=LineBuf[Column];
   32 :
      for Column:=0 to img.Width-1 do
      begin
        Move(LineBuf[Column * 4], buffer, 4);
//        WriteLn(IntToHex(buffer, 8));

{        buffer := buffer mod (256 * 256 * 256);
        MyColor.red := Word((buffer div 256 * 256) * 256);
        buffer := buffer mod (256 * 256);
        MyColor.green := Word((buffer div 256) * 256);
        buffer := buffer mod 256;
        MyColor.blue := Word((buffer) * 256);}

        buffer := buffer mod (256 * 256 * 256);
        MyColor.blue := Word((buffer div 256 * 256) * 256);
        buffer := buffer mod (256 * 256);
        MyColor.green := Word((buffer div 256) * 256);
        buffer := buffer mod 256;
        MyColor.red := Word((buffer) * 256);

        img.Colors[Column,Row] := MyColor;
      end;
  end;

{    inc(percentacc,4);
    if percentacc>=percentinterval then
    begin
      percent:=percent+(percentacc div percentinterval);
      percentacc:=percentacc mod percentinterval;
      Progress(psRunning,percent,false,Rect,'',continue);
    end;}
end;

procedure TFPReaderXWD.InternalRead(Stream: TStream; Img: TFPCustomImage);
var
  Color: TFPColor;
  Size, Row, i, Index: Integer;
begin
  {****************************************************************************
    Initialization
   ****************************************************************************}
  Rect.Left:=0; Rect.Top:=0; Rect.Right:=0; Rect.Bottom:=0;
  continue:=true;
  Progress(psStarting,0,false,Rect,'',continue);
  if not continue then exit;

  Img.UsePalette := True;
//  Img.Palette.Clear;
  Color.alpha := 0;

  {****************************************************************************
    The file is on big-endian format, so it needs to be swaped on little-endian CPUs
   ****************************************************************************}
  Stream.Position := 0; //* Causes error if removed, but should be

  Stream.Read(XWDFileHeader, SizeOf(TXWDFileHeader));

{$ifdef ENDIAN_LITTLE}
  SwapXWDFileHeader(XWDFileHeader);
{$endif}

  {****************************************************************************
    Now reads the window name
   ****************************************************************************}
  Size := XWDFileHeader.header_size - SizeOf(TXWDFileHeader);

  // Avoids allocating too much space for the string
  if Size > 256 then raise Exception.Create('Window name string too big. The file might be corrupted.');
  
  SetLength(WindowName, Size);

  Stream.Read(WindowName[0], Size);

  {****************************************************************************
    Fills the palette
   ****************************************************************************}
  SetLength(XWDColors, XWDFileHeader.ncolors);

  Img.Palette.Count := 256;

  for i := 1 to XWDFileHeader.ncolors do
  begin
    Stream.Read(XWDColors[i - 1], SizeOf(TXWDColor));

    {$ifdef ENDIAN_LITTLE}
    SwapXWDColor(XWDColors[i - 1]);
    {$endif}

    Color.red := XWDColors[i - 1].red;
    Color.green := XWDColors[i - 1].green;
    Color.blue := XWDColors[i - 1].blue;

    Index := XWDColors[i - 1].pixel mod 256;
//    WriteLn(IntToHex(Index, 8));
    Img.Palette.Color[Index] := Color;
  end;

  {****************************************************************************
    Reads the matrix of colors
   ****************************************************************************}
  Img.SetSize(XWDFileHeader.pixmap_width, XWDFileHeader.pixmap_height);

  GetMem(LineBuf, XWDFileHeader.bytes_per_line);

  for Row := 0 to Img.Height - 1 do
  begin
    Stream.Read(LineBuf[0], XWDFileHeader.bytes_per_line);
    WriteScanLine(Row, Img);
    if not continue then exit;
  end;

  Progress(psEnding,100,false,Rect,'',continue);
end;

function TFPReaderXWD.InternalCheck (Stream:TStream): boolean;
var
  Header: TXWDFileHeader;
begin
  stream.Read(Header, SizeOf(Header));
  {$IFDEF ENDIAN_LITTLE}
  SwapXWDFileHeader(Header);
  {$ENDIF}
  Result := Header.file_version = XWD_FILE_VERSION; // Just check magic number
end;

initialization

  ImageHandlers.RegisterImageReader ('XWD Format', 'xwd', TFPReaderXWD);
  
end.
