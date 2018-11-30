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


Unit BGRAGraphics;
{=== Types imported from Graphics ===}
{$i bgrabitmap.inc}{$H+}

interface

{$IFDEF BGRABITMAP_USE_LCL}
uses  Windows, Classes, Types, SysUtils, BGRATypes, {$IFDEF BDS} Generics.Collections, CommCtrl, ImgList, {$ENDIF} Graphics, GraphType, FPImage;

type
  PColor = Graphics.PColor;
  TColor = Graphics.TColor;

  {$IF NOT DEFINED(FPC) AND DEFINED(BDS)}
  TAntialiasingMode = GraphType.TAntialiasingMode;

  TFPPenJoinStyle = (
    pjsRound,
    pjsBevel,
    pjsMiter
  );
  TPenJoinStyle = TFPPenJoinStyle;
  TFPPenEndCap = (
    pecRound,
    pecSquare,
    pecFlat
  );
  TPenEndCap = TFPPenEndCap;
  TFPPenStyle = (psSolid, psDash, psDot, psDashDot, psDashDotDot, psinsideFrame, psPattern,psClear);
  TFPPenStyleSet = set of TFPPenStyle;
  TFPPenMode = (pmBlack, pmWhite, pmNop, pmNot, pmCopy, pmNotCopy,
                pmMergePenNot, pmMaskPenNot, pmMergeNotPen, pmMaskNotPen, pmMerge,
                pmNotMerge, pmMask, pmNotMask, pmXor, pmNotXor);
  TPenPattern = BGRALongWord;
  TPenStyle = TFPPenStyle;
  TPenMode = TFPPenMode;

  TFillStyle = Graphics.TFillStyle;
  TGradientDirection = GraphType.TGradientDirection;

  {$ELSE}
  TAntialiasingMode = Graphics.TAntialiasingMode;
  TGradientDirection = Graphics.TGradientDirection;
  TPenEndCap = Graphics.TPenEndCap;
  TPenJoinStyle = Graphics.TPenJoinStyle;
  TPenStyle = Graphics.TPenStyle;
  {$IFEND}

const
  {$IF NOT DEFINED(FPC) AND DEFINED(BDS)}

  {$ELSE}

  amDontCare = Graphics.amDontCare;
  amOn = Graphics.amOn;
  amOff = Graphics.amOff;

  gdVertical = Graphics.gdVertical;
  gdHorizontal = Graphics.gdHorizontal;

  pecRound = Graphics.pecRound;
  pecSquare = Graphics.pecSquare;
  pecFlat = Graphics.pecFlat;

  pjsRound = Graphics.pjsRound;
  pjsBevel = Graphics.pjsBevel;
  pjsMiter = Graphics.pjsMiter;

  psPattern = Graphics.psPattern;

  psSolid = Graphics.psSolid;
  psDash = Graphics.psDash;
  psDot = Graphics.psDot;
  psDashDot = Graphics.psDashDot;
  psDashDotDot = Graphics.psDashDotDot;
  psClear = Graphics.psClear;
  psInsideframe = Graphics.psInsideframe;
  {$IFEND}

  tmAuto = Graphics.tmAuto;
  tmFixed = Graphics.tmFixed;

type
  {$IF NOT DEFINED(FPC) AND DEFINED(BDS)}
  TAlignment  = (taLeftJustify,taRightJustify,taCenter);
  TTextLayout = (tlTop, tlCenter, tlBottom);
  TTextStyle  = packed record
    Alignment : TAlignment;
    Layout    : TTextLayout;
    SingleLine: boolean;
    Clipping  : boolean;
    ExpandTabs: boolean;
    ShowPrefix: boolean;
    Wordbreak : boolean;
    Opaque    : boolean;
    SystemFont: Boolean;
    RightToLeft: Boolean;
    EndEllipsis: Boolean;
  end;
  {$ELSE}
  TPen = Graphics.TPen;
  TTextLayout = Graphics.TTextLayout;
  TTextStyle = Graphics.TTextStyle;
  TFillStyle = Graphics.TFillStyle;
  {$IFEND}
  TFillMode = Graphics.TFillMode;
  TBrushStyle = Graphics.TBrushStyle;

const
  {$IF NOT DEFINED(FPC) AND DEFINED(BDS)}

  {$ELSE}
  tlTop    = Graphics.tlTop;
  tlCenter = Graphics.tlCenter;
  tlBottom = Graphics.tlBottom;

  fsSurface = GraphType.fsSurface;
  fsBorder  = GraphType.fsBorder;
  {$IFEND}
  fmAlternate = Graphics.fmAlternate;
  fmWinding   = Graphics.fmWinding;

  bsSolid = Graphics.bsSolid;
  bsClear = Graphics.bsClear;
  bsHorizontal = Graphics.bsHorizontal;
  bsVertical   = Graphics.bsVertical;
  bsFDiagonal  = Graphics.bsFDiagonal;
  bsBDiagonal  = Graphics.bsBDiagonal;
  bsCross      = Graphics.bsCross;
  bsDiagCross  = Graphics.bsDiagCross;

type
  TBrush   = Graphics.TBrush;
  TCanvas  = Graphics.TCanvas;
  TGraphic = Graphics.TGraphic;

  {$IF NOT DEFINED(FPC) AND DEFINED(BDS)}
  TBitmap = class(Graphics.TBitmap)
  private
    FRefreshRawImage : boolean;
    FRawImage : TRawImage;
    function GetRawImage : TRawImage;
    procedure FreeData;
  protected
    procedure Changed(Sender: TObject); override;
    procedure UpdateRawImage; virtual;
  public
    constructor Create; override;
    Destructor Destroy; override;
    Property   RawImage : TRawImage read GetRawImage;
    procedure  AssignRawImage(ARawImage : TRawImage);
    procedure  ResetRawImage;
  end;

  TRasterImage = TBitmap;

  TRawImage    = GraphType.TRawImage;
  TCanvasVar   = GraphType.TCanvasVar;
  {$ELSE}
  TBitmap      = Graphics.TBitmap;
  TRasterImage = Graphics.TRasterImage;
  TRawImage    = GraphType.TRawImage;
  {$IFEND}

  TFontStyle   = Graphics.TFontStyle;
  TFontStyles  = Graphics.TFontStyles;
  TFontQuality = Graphics.TFontQuality;
  TFont        = Graphics.TFont;

const
  fsBold   = Graphics.fsBold;
  fsItalic = Graphics.fsItalic;
  fsStrikeOut = Graphics.fsStrikeOut;
  fsUnderline = Graphics.fsUnderline;

  fqDefault = Graphics.fqDefault;
  fqDraft   = Graphics.fqDraft;
  fqProof   = Graphics.fqProof;
  fqNonAntialiased = Graphics.fqNonAntialiased;
  fqAntialiased = Graphics.fqAntialiased;
  fqCleartype   = Graphics.fqCleartype;
  fqCleartypeNatural = Graphics.fqCleartypeNatural;

  clNone = Graphics.clNone;

  clBlack   = Graphics.clBlack;
  clMaroon  = Graphics.clMaroon;
  clGreen   = Graphics.clGreen;
  clOlive   = Graphics.clOlive;
  clNavy    = Graphics.clNavy;
  clPurple  = Graphics.clPurple;
  clTeal    = Graphics.clTeal;
  clGray    = Graphics.clGray;
  clSilver  = Graphics.clSilver;
  clRed     = Graphics.clRed;
  clLime    = Graphics.clLime;
  clYellow  = Graphics.clYellow;
  clBlue    = Graphics.clBlue;
  clFuchsia = Graphics.clFuchsia;
  clAqua    = Graphics.clAqua;
  clLtGray  = Graphics.clLtGray; // clSilver alias
  clDkGray  = Graphics.clDkGray; // clGray alias
  clWhite   = Graphics.clWhite;

type
  TGBRACustomGraphic = class(TGraphic)
  public
    function GetMimeType: string; {$IFDEF FPC}override;{$ELSE}virtual;{$ENDIF}
    procedure Clear; {$IFDEF FPC}override;{$ELSE}virtual;{$ENDIF}
  end;

{$IF NOT DEFINED(FPC) AND DEFINED(BDS)}
  TFontPitch = graphics.TFontPitch;
  TFontName = graphics.TFontName;
  TFontDataName = string[LF_FACESIZE -1];
  TFontStylesbase = set of TFontStyle;
  TFontCharSet = 0..255;
  TFontData = graphics.TFontData;

//TDicBitmapRawImage = TDictionary<TBitmap,TRawImage>;
  TDicCanvasVar = TDictionary<TCanvas,TCanvasVar>;

var
//  DicBitmapRawImage : TDicBitmapRawImage;
  DicCanvasVar : TDicCanvasVar;

type
  TRGBAQuad = record
    Blue:  Byte;
    Green: Byte;
    Red:   Byte;
    Alpha: Byte;
  end;
  PRGBAQuad = ^TRGBAQuad;
  TRGBAQuadArray = array of TRGBAQuad;

  TCustomImageListHelper = class helper for TCustomImageList
    procedure GetBitmapRaw(Index: Integer; Image: TBitmap); overload;
//    procedure GetBitmapRaw(Index: Integer; Image: TBitmap); overload;
  end;

{ TBitmapHelper = class helper for TBitmap
  private
    class var FRawImage : TRawImage;
    function GetRawImage : TRawImage;
  public
    Property RawImage : TRawImage read GetRawImage;
    function UpdateRawImage : boolean;
    procedure AssignRawImage(ARawImage : TRawImage);
    procedure ResetRawImage;
  end;}

  TStreamHelper = class helper for TStream
    procedure WriteByte(b : Byte);
    procedure WriteWord(w : BGRAWord);
    procedure WriteDWord(d : BGRACardinal);
    procedure WriteQWord(q: BGRAQWord);
    function ReadDWord : BGRACardinal;
  end;

  TFontHelper = class helper for TFont
    function GetColor: TColor;
  end;

  TCanvasHelper = class helper for TCanvas
  private
    class var FCanvasVar : TCanvasVar;
  public
    function TextFitInfo(const Text: string; MaxWidth: Integer): Integer;
    function GetUpdatedHandle(ReqState: TCanvasState): HDC;

    procedure GetTextSize (text:string; var w,h:integer);
    function  GetTextHeight (text:string) : integer;
    function  GetTextWidth (text:string) : integer;
    procedure TextRect(ARect: TRect; X, Y: integer; const Text: string; const Style: TTextStyle);
    function  GetWidth: integer;
    function  GetHeight: integer;
    property  Height: integer read GetHeight;
    property  Width: integer read GetWidth;

    function  GetAntialiasingMode : TAntialiasingMode;
    procedure SetAntialiasingMode(AValue : TAntialiasingMode);

    function GetState : TCanvasState;
    function GetCanvasVar : TCanvasVar;
    Property CanvasVar : TCanvasVar read GetCanvasVar;
    Property State : TCanvasState read GetState;
    Property AntialiasingMode : TAntialiasingMode read GetAntialiasingMode write SetAntialiasingMode;
  end;

{$IFEND}

{$IF NOT DEFINED(FPC) AND DEFINED(BDS)}
function GetDeviceSize(DC: HDC; var p: TPoint): boolean;
procedure FillRawImageDescriptionColors(var ADesc: TRawImageDescription);
procedure FillRawImageDescription(const ABitmapInfo: Windows.TBitmap; out ADesc: TRawImageDescription);
function RawImage_CreateBitmaps(const ARawImage: TRawImage; out ABitmap, AMask: HBitmap; ASkipMask: Boolean): Boolean;
function RawImage_FromDevice(out ARawImage: TRawImage; ADC: HDC; const ARect: TRect): Boolean;
function RawImage_FromBitmap(out ARawImage: TRawImage; ABitmap, AMask: HBITMAP; ARect: PRect = nil): Boolean;
function TryStringToGUID(const S: Ansistring; out Guid: TGUID): Boolean;
function GetBitmapOrder(AWinBmp: Windows.TBitmap; ABitmap: HBITMAP): TRawImageLineOrder;
function GetBitmapBytes(AWinBmp: Windows.TBitmap; ABitmap: HBITMAP; const ARect: TRect; ALineEnd: TRawImageLineEnd; ALineOrder: TRawImageLineOrder; out AData: Pointer; out ADataSize: PtrUInt): Boolean;


function Blue(rgb: TColorRef): BYTE; // does not work on system color
function Green(rgb: TColorRef): BYTE; // does not work on system color
function Red(rgb: TColorRef): BYTE; // does not work on system color
function RGBToColor(R, G, B: Byte): TColor;
procedure RedGreenBlue(rgb: TColorRef; out Red, Green, Blue: Byte); // does not work on system color
function FPColorToTColorRef(const FPColor: TFPColor): TColorRef;
function FPColorToTColor(const FPColor: TFPColor): TColor;
function TColorToFPColor(const c: TColorRef): TFPColor; overload;
function TColorToFPColor(const c: TColor): TFPColor; overload; // does not work on system color
function MulDiv(nNumber, nNumerator, nDenominator: Integer): Integer;
function MathRound(AValue: ValReal): BGRAInt64;
function GetFontData(Font: HFont): TFontData;
function PtInRect(const Rect : TRect; const Point : TPoint) : Boolean;
function Point(AX, AY: Integer): TPoint;
function Rect(ALeft, ATop, ARight, ABottom: Integer): TRect;
function Bounds(ALeft, ATop, AWidth, AHeight: Integer): TRect;
procedure SendKey(Key: BGRAWord); overload;
procedure SendKey(Key: BGRAWord; const Shift: TShiftState; SpecialKey: boolean = False); overload;
function CompareMethods(const m1, m2: TMethod): boolean;
{$IFEND}

function ColorToRGB(c: TColor): TColor;
function clRgbBtnHighlight: TColor;
function clRgbBtnShadow: TColor;

{$IFDEF FPC}
function FPColorToTColor(const FPColor: TFPColor): TColor;
function TColorToFPColor(const c: TColor): TFPColor;
function RGBToColor(R, G, B: Byte): TColor;
procedure RedGreenBlue(rgb: TColor; out Red, Green, Blue: Byte);
{$ENDIF}


procedure Filldword_(var x;count : BGRACardinal;value : BGRADWord);

implementation

{$IF NOT DEFINED(FPC) AND DEFINED(BDS)}
uses
  BGRABitmapTypes;
{$IFEND}

procedure filldword_(var x;count : BGRACardinal; value : BGRADWord);assembler;
asm
  PUSH EDI
  MOV EDI, x
  MOV EAX, value
  MOV ECX, count
  CLD
  REP STOSD
  POP EDI
end;

{$IF NOT DEFINED(FPC) AND DEFINED(BDS)}
function CompareMethods(const m1, m2: TMethod): boolean;
begin
  Result:=(m1.Code=m2.Code) and (m1.Data=m2.Data);
end;

procedure SendKey(Key: BGRAWord);
begin
  Keybd_Event(Key, 0, 0, 0);
end;

procedure SendKey(Key: BGRAWord; const Shift: TShiftState; SpecialKey: boolean);
  type
    TShiftKeyInfo = record
      shift: Byte;
      vkey: Byte;
  end;
  byteset = set of 0..7;
const
  ShiftKeys: array[1..3] of TShiftKeyInfo = ((shift: Ord(ssCtrl); vkey: VK_CONTROL), (shift: Ord(ssShift); vkey: VK_SHIFT), (shift: Ord(ssAlt); vkey: VK_MENU));
var
  Flag: BGRADWord;
  bShift: ByteSet absolute shift;
  i: Integer;
begin
  for i := 1 to 3 do
  begin
    if shiftkeys[i].shift in bShift then
      Keybd_Event(ShiftKeys[i].vkey, MapVirtualKey(ShiftKeys[i].vkey, 0), 0, 0);
  end;
  if SpecialKey then
    Flag := KEYEVENTF_EXTENDEDKEY
  else
    Flag := 0;

  Keybd_Event(Key, MapvirtualKey(Key, 0), Flag, 0);
  Flag := Flag or KEYEVENTF_KEYUP;
  Keybd_Event(Key, MapvirtualKey(Key, 0), Flag, 0);
  for i := 3 downto 1 do
  begin
    if ShiftKeys[i].shift in bShift then
      Keybd_Event(shiftkeys[i].vkey, MapVirtualKey(ShiftKeys[i].vkey, 0), KEYEVENTF_KEYUP, 0);
  end;
end;

function Point(AX, AY: Integer): TPoint;
begin
  with Result do
  begin
    X := AX;
    Y := AY;
  end;
end;

function Rect(ALeft, ATop, ARight, ABottom: Integer): TRect;
begin
  with Result do
  begin
    Left := ALeft;
    Top := ATop;
    Right := ARight;
    Bottom := ABottom;
  end;
end;

function Bounds(ALeft, ATop, AWidth, AHeight: Integer): TRect;
begin
  with Result do
  begin
    Left := ALeft;
    Top := ATop;
    Right := ALeft + AWidth;
    Bottom :=  ATop + AHeight;
  end;
end;

function PtInRect(const Rect : TRect; const Point : TPoint) : Boolean;
begin
  Result := ((Point.X >= Rect.Left) and
             (Point.X < Rect.Right) and
             (Point.Y >= Rect.Top)  and
             (Point.Y < Rect.Bottom)
          );
end;

function GetFontData(Font: HFont): TFontData;
var
  ALogFont: TLogFont;
begin
  Result := DefFontData;
  if Font <> 0 then
  begin
    if GetObject(Font, SizeOf(ALogFont), @ALogFont) <> 0 then
      with Result, ALogFont do
      begin
        Height := lfHeight;
        if lfWeight >= FW_BOLD then
          Include(Style, fsBold);
        if lfItalic > 0 then
          Include(Style, fsItalic);
        if lfUnderline > 0 then
          Include(Style, fsUnderline);
        if lfStrikeOut > 0 then
          Include(Style, fsStrikeOut);
        Charset := TFontCharset(lfCharSet);
        Name := TFontDataName(lfFaceName);
        case lfPitchAndFamily and $F of
          VARIABLE_PITCH: Pitch := fpVariable;
          FIXED_PITCH: Pitch := fpFixed;
        else
          Pitch := fpDefault;
        end;
        Orientation := lfOrientation;
        Handle := Font;
      end;
  end;
end;

function MathRound(AValue: ValReal): BGRAInt64;
begin
  if AValue >= 0 then
    Result := Trunc(AValue + 0.5)
  else
    Result := Trunc(AValue - 0.5);
end;

function MulDiv(nNumber, nNumerator, nDenominator: Integer): Integer;
begin
  if nDenominator = 0 then
    Result := -1
  else
  if nNumerator = nDenominator then
    Result := nNumber
  else
    Result := MathRound(BGRAInt64(nNumber) * BGRAInt64(nNumerator) / nDenominator);
end;

function Blue(rgb: TColorRef): BYTE; // does not work on system color
begin
  Result := (rgb shr 16) and $000000ff;
end;

function Green(rgb: TColorRef): BYTE; // does not work on system color
begin
  Result := (rgb shr 8) and $000000ff;
end;

function Red(rgb: TColorRef): BYTE; // does not work on system color
begin
  Result := rgb and $000000ff;
end;

function RGBToColor(R, G, B: Byte): TColor;
begin
  Result := (B shl 16) or (G shl 8) or R;
end;

procedure RedGreenBlue(rgb: TColorRef; out Red, Green, Blue: Byte); // does not work on system color
begin
  Red := rgb and $000000ff;
  Green := (rgb shr 8) and $000000ff;
  Blue := (rgb shr 16) and $000000ff;
end;

function FPColorToTColorRef(const FPColor: TFPColor): TColorRef;
begin
  Result:=((FPColor.Red shr 8) and $ff)
       or (FPColor.Green and $ff00)
       or ((FPColor.Blue shl 8) and $ff0000);
end;

function FPColorToTColor(const FPColor: TFPColor): TColor;
begin
  Result:=TColor(FPColorToTColorRef(FPColor));
end;

function TColorToFPColor(const c: TColorRef): TFPColor; overload;
begin
  Result.Red:=(c and $ff);
  Result.Red:=Result.Red+(Result.Red shl 8);
  Result.Green:=(c and $ff00);
  Result.Green:=Result.Green+(Result.Green shr 8);
  Result.Blue:=(c and $ff0000) shr 8;
  Result.Blue:=Result.Blue+(Result.Blue shr 8);
  Result.Alpha:=FPImage.alphaOpaque;
end;

function TColorToFPColor(const c: TColor): TFPColor; overload; // does not work on system color
begin
  Result:=TColorToFPColor(TColorRef(c));
end;

function TryStringToGUID(const S: Ansistring; out Guid: TGUID): Boolean;
var
  e: Boolean;
  p: PAnsiChar;

  function rb: Byte;
  begin
    case p^ of
      '0'..'9': Result := Byte(p^) - Byte('0');
      'a'..'f': Result := Byte(p^) - Byte('a') + 10;
      'A'..'F': Result := Byte(p^) - Byte('A') + 10;
      else e := False;
    end;
    Inc(p);
  end;

  procedure nextChar(c: AnsiChar);
  begin
    if p^ <> c then
      e := False;
    Inc(p);
  end;

begin
  if Length(S)<>38 then Exit(False);
  e := True;
  p := PAnsiChar(S);
  nextChar('{');
  Guid.D1 := rb shl 28 or rb shl 24 or rb shl 20 or rb shl 16 or rb shl 12 or rb shl 8 or rb shl 4 or rb;
  nextChar('-');
  Guid.D2 := rb shl 12 or rb shl 8 or rb shl 4 or rb;
  nextChar('-');
  Guid.D3 := rb shl 12 or rb shl 8 or rb shl 4 or rb;
  nextChar('-');
  Guid.D4[0] := rb shl 4 or rb;
  Guid.D4[1] := rb shl 4 or rb;
  nextChar('-');
  Guid.D4[2] := rb shl 4 or rb;
  Guid.D4[3] := rb shl 4 or rb;
  Guid.D4[4] := rb shl 4 or rb;
  Guid.D4[5] := rb shl 4 or rb;
  Guid.D4[6] := rb shl 4 or rb;
  Guid.D4[7] := rb shl 4 or rb;
  nextChar('}');
  Result := e;
end;

{TCustomImageListHelper}

{procedure TCustomImageListHelper.GetBitmapRaw(Index: Integer; Image: TBitmap);
begin
  GetBitmapRaw(Index, TBaseBitmap(Image));
end;}

procedure TCustomImageListHelper.GetBitmapRaw(Index: Integer; Image: TBitmap);
var
  RawImg: TRawImage;
  hbmImage : TImageInfo;
  WinDIB: Windows.TDIBSection;
  WinBmp: Windows.TBitmap absolute WinDIB.dsBm;
  i, i2, iSize, iForIni, iForFin, iLineSize : Integer;
  PixelFormat : TPixelFormat;
begin
  if (Index < 0) or (Index >= Count) or (Image = nil) then Exit;

{  if not Image.HandleAllocated then
    GetBitmap(Index, Image);
  if not Image.UpdateRawImage then
    raise Exception.Create('Error UpdateRawImage.'); }
  PixelFormat := Image.PixelFormat;
  Image.PixelFormat := pf32bit;

  RawImg.Init;
  ImageList_GetImageInfo(Handle, Index, hbmImage);

  if not Image.HandleAllocated then
    GetBitmap(Index, Image);

  if RawImage_FromBitmap(RawImg, Image.Handle, Image.MaskHandle) then
    Image.AssignRawImage(RawImg);

  Image.PixelFormat := PixelFormat;

//    Image.Handle     := hbmImage.hbmImage;
//    Image.MaskHandle := hbmImage.hbmImage;

//    Image.SetSize(RawImg.Description.Width, RawImg.Description.Height);

//    Image.TransparentColor := Self.BkColor;

//    if Image.RawImage.Data <> nil then
//      FreeMem(Image.RawImage.Data);

{    iSize   := Trunc(RawImg.DataSize / (RawImg.Description.Height / Height));
    GetMem(_PByte, iSize);

    i2        := 0;
    iLineSize := Trunc(iSize / Height);
    iForIni   := (Height * Index);
    iForFin   := iForIni + Height;
    for i := iForIni to iForFin do
    begin
      system.Move(RawImg.Data[i],_PByte[i2], iLineSize);
      inc(i2);
    end;

//    FreeMem(RawImg.Data);
//    FreeMem(RawImg.Mask);
    RawImg.DataSize := iSize;
    RawImg.MaskSize := 0;
//    RawImg.Data := nil;
//    RawImg.Mask := nil;

//    system.Move(RawImg.Data[i],_PByte[i2], iLineSize);
    RawImg.data    := _PByte;
//    system.Move(_PByte,RawImg.data, iSize); }
end;

{TFontHelper}

function TFontHelper.GetColor: TColor;
begin
  Result := Color;
//  if (Result = clDefault) and (Canvas is TCanvas) then
//    Result := TCanvas(Canvas).GetDefaultColor(dctFont);
end;

{TCanvasHelper}

function TCanvasHelper.GetState : TCanvasState;
begin
  Result := CanvasVar.FState;
end;

function TCanvasHelper.GetCanvasVar : TCanvasVar;
var
  NewCanvasVar : TCanvasVar;
begin
  if DicCanvasVar.ContainsKey(Self) then
  begin
    Result     := DicCanvasVar.Items[Self];
    FCanvasVar := Result;
  end
  else
  begin
    NewCanvasVar.FCanvas := Self;
    DicCanvasVar.Add(Self, NewCanvasVar);
    Result := NewCanvasVar;
    FCanvasVar := Result;
  end;
end;

function  TCanvasHelper.GetAntialiasingMode : TAntialiasingMode;
begin
  Result := GetCanvasVar.FAntialiasingMode;
end;

procedure TCanvasHelper.SetAntialiasingMode(AValue : TAntialiasingMode);
begin
  GetCanvasVar;
  FCanvasVar.FAntialiasingMode :=  AValue;
end;

function TCanvasHelper.GetUpdatedHandle(ReqState: TCanvasState): HDC;
begin
  RequiredState(ReqState+[csHandleValid]);
  Result := Handle;
end;

procedure TCanvasHelper.TextRect(ARect: TRect; X, Y: integer; const Text: string; const Style: TTextStyle);
var
  Options : BGRALongInt;
  fRect : TRect;
  DCIndex: Integer;
  DC: HDC;
  ReqState: TCanvasState;

  procedure SaveState;
  begin
    if DCIndex<>0 then exit;
    DCIndex:=SaveDC(DC);
  end;

  procedure RestoreState;
  begin
    if DCIndex=0 then exit;
    RestoreDC(DC,DCIndex);
  end;
begin
  //debugln(['TCanvas.TextRect ',DbgSName(Self),' Text="',Text,'" ',dbgs(ARect),' X=',X,',Y=',Y]);
  Changing;

  Options := 0;
  case Style.Alignment of
    taRightJustify : Options := DT_RIGHT;
    taCenter : Options := DT_CENTER;
  end;
  case Style.Layout of
    tlCenter : Options := Options or DT_VCENTER;
    tlBottom : Options := Options or DT_BOTTOM;
  end;
  if Style.EndEllipsis then
    Options := Options or DT_END_ELLIPSIS;
  if Style.WordBreak then begin
    Options := Options or DT_WORDBREAK;
    if Style.EndEllipsis then
      Options := Options and not DT_END_ELLIPSIS;
  end;

  if Style.SingleLine then
    Options := Options or DT_SINGLELINE;

  if not Style.Clipping then
    Options := Options or DT_NOCLIP;

  if Style.ExpandTabs then
    Options := Options or DT_EXPANDTABS;

  if not Style.ShowPrefix then
    Options := Options or DT_NOPREFIX;

  if Style.RightToLeft then
    Options := Options or DT_RTLREADING;

  ReqState:=[csHandleValid];
  if not Style.SystemFont then
    Include(ReqState,csFontValid);
  if Style.Opaque then
    Include(ReqState,csBrushValid);
  DC:=GetUpdatedHandle(ReqState);

  DCIndex:=0;
  if Style.SystemFont or Style.Clipping or (not Style.Opaque) then
    SaveState;

//  if Style.SystemFont then
//    Windows.SelectObject(DC, OnGetSystemFont());

  // calculate text rectangle
  FRect := ARect;
  if Style.Alignment = taLeftJustify then
    fRect.Left := X;
  if Style.Layout = tlTop then
    fRect.Top := Y;

  if (Style.Alignment in [taRightJustify,taCenter]) or
    (Style.Layout in [tlCenter,tlBottom]) then
  begin
    DrawText(DC, (*{$IFDEF FPC}PChar{$ELSE}PAnsiChar{$ENDIF}*)PChar(Text), Length(Text), fRect, DT_CALCRECT or Options);
    case Style.Alignment of
      taRightJustify : OffsetRect(fRect, ARect.Right - fRect.Right, 0);
      taCenter : OffsetRect(fRect, (ARect.Right - fRect.Right) div 2, 0);
    end;
    case Style.Layout of
      tlCenter : OffsetRect(fRect, 0,
               ((ARect.Bottom - ARect.Top) - (fRect.Bottom - fRect.Top)) div 2);
      tlBottom : OffsetRect(fRect, 0, ARect.Bottom - fRect.Bottom);
    end;
  end;

  if Style.Clipping then
  begin
    with ARect do
      InterSectClipRect(DC, Left, Top, Right, Bottom);
    Options := Options or DT_NOCLIP; // no clipping as we are handling it here
  end;

  if Style.Opaque then
    FillRect(fRect)
  else
    SetBkMode(DC, TRANSPARENT);

  if Style.SystemFont then
    SetTextColor(DC, TColorRef(Font.GetColor));

  //debugln('TCanvas.TextRect DRAW Text="',Text,'" ',dbgs(fRect));
  DrawText(DC, (*{$IFDEF FPC}PChar{$ELSE}PAnsiChar{$ENDIF}*)PChar(Text), Length(Text), fRect, Options);

  if Style.Opaque and (csBrushValid in State) then
  begin
    if Brush.Style=bsSolid then // restore BKMode
      SetBkMode(DC, OPAQUE)
  end;
  RestoreState;
  Changed;
end;

function TCanvasHelper.TextFitInfo(const Text: string; MaxWidth: Integer): Integer;
var lSize: TSize;
begin
  windows.GetTextExtentExPoint(Self.Handle, PChar(Text), Length(Text), MaxWidth, @Result, nil, lSize);
end;

procedure TCanvasHelper.GetTextSize (text:string; var w,h:integer);
var
  TxtSize: TSize;
begin
  TxtSize:=TextExtent(Text);
  w:=TxtSize.cx;
  h:=TxtSize.cy;
end;

function TCanvasHelper.GetTextHeight (text:string) : integer;
begin
  Result:=TextHeight(Text);
end;

function TCanvasHelper.GetTextWidth (text:string) : integer;
begin
  Result:=TextWidth(Text);
end;

{TStreamHelper}

procedure TStreamHelper.WriteByte(b : Byte);
begin
   WriteBuffer(b,1);
end;

procedure TStreamHelper.WriteWord(w : BGRAWord);
begin
   WriteBuffer(w,2);
end;

procedure TStreamHelper.WriteDWord(d : BGRACardinal);
begin
   WriteBuffer(d,4);
end;

procedure TStreamHelper.WriteQWord(q: BGRAQWord);
begin
  WriteBuffer(q,8);
end;

function TStreamHelper.ReadDWord : BGRACardinal;
var
   d : BGRACardinal;
begin
   ReadBuffer(d,4);
   ReadDWord:=d;
end;

function TCanvasHelper.GetHeight: integer;
var
  p: TPoint;
begin
  if HandleAllocated then
  begin
    GetDeviceSize(Handle,p);
    Result:=p.y;
  end
  else
    Result:=0;
end;

function TCanvasHelper.GetWidth: integer;
var
  p: TPoint;
begin
  if HandleAllocated then
  begin
    GetDeviceSize(Handle,p);
    Result:=p.x;
  end
  else
    Result:=0;
end;

{TBitmap}
procedure TBitmap.Changed(Sender: TObject);
begin
  inherited;
  FRefreshRawImage := True;
end;

procedure TBitmap.FreeData;
begin
  FreeMem(FRawImage.Data);
  FreeMem(FRawImage.Mask);
  FRawImage.Data := nil;
  FRawImage.Mask := nil;
end;

procedure TBitmap.UpdateRawImage;
var
  PixelFormat : TPixelFormat;
begin
  PixelFormat      := Self.PixelFormat;

  if Self.PixelFormat <> pf32bit then
    Self.PixelFormat := pf32bit;

  FRawImage.Init;

  if not RawImage_FromBitmap(FRawImage, Self.Handle, Self.MaskHandle) then
    raise Exception.Create('Error in method RawImage_FromBitmap');

  if Self.PixelFormat <> PixelFormat then
    Self.PixelFormat := PixelFormat;

  FRefreshRawImage := False;
end;

constructor TBitmap.Create;
begin
  inherited;
  FRawImage.Init;
end;

Destructor TBitmap.Destroy;
begin
  FreeData;
  inherited;
end;

procedure TBitmap.AssignRawImage(ARawImage : TRawImage);
begin
  FreeData;
  FRawImage := ARawImage;
end;

procedure TBitmap.ResetRawImage;
begin
  FreeData;
  FRawImage.Init;
end;

function TBitmap.GetRawImage : TRawImage;

  procedure GetBitmapRaw(var RawImg: TRawImage);
  var
    WinDIB: Windows.TDIBSection;
    WinBmp: Windows.TBitmap absolute WinDIB.dsBm;
    I : integer;
    OldPixelFormat : TPixelFormat;
  begin
    RawImg.Init;
    FillChar(WinDIB, SizeOf(WinDIB), 0);
    if Windows.GetObject(Handle, SizeOf(WinDIB), @WinDIB) = 0 then
      Exit;
    FillRawImageDescription(WinBmp, RawImg.Description);
  end;
begin
  Result := FRawImage;
  if HandleAllocated and FRefreshRawImage then
  begin
    FreeData;
    UpdateRawImage;
  end;
end;

function GetDeviceSize(DC: HDC; var p: TPoint): boolean;
var
  hBitmap: HGDIOBJ;
  hWindow: HWND;
  BitmapInfo: BITMAP;
  ClientRect: TRect;
begin
  // check if memory dc with bitmap
  Result := false;
  case GetObjectType(DC) of
    OBJ_MEMDC:
      begin
        hBitmap := GetCurrentObject(DC, OBJ_BITMAP);
        if hBitmap <> HGDIOBJ(nil) then
        begin
          GetObject(hBitmap, SizeOf(BITMAP), @BitmapInfo);
          P.x := BitmapInfo.bmWidth;
          P.y := BitmapInfo.bmHeight;
          Result := true;
        end;
      end;
    OBJ_DC:
      begin
        hWindow := WindowFromDC(DC);
        if hWindow <> HWND(nil) then
        begin
          Result := GetClientRect(hWindow, ClientRect);
          P.x := ClientRect.Right;
          P.y := ClientRect.Bottom;
        end;
      end;
  else
  end;

  if not Result then
  begin
    p.X := 0;
    p.Y := 0;
    Result := false;
  end;
end;

procedure FillRawImageDescriptionColors(var ADesc: TRawImageDescription);
begin
  case ADesc.BitsPerPixel of
    1,4,8:
      begin
        // palette mode, no offsets
        ADesc.Format := ricfGray;
        ADesc.RedPrec := ADesc.BitsPerPixel;
        ADesc.GreenPrec := 0;
        ADesc.BluePrec := 0;
        ADesc.RedShift := 0;
        ADesc.GreenShift := 0;
        ADesc.BlueShift := 0;
      end;
    16:
      begin
        // 5-5-5 mode
        ADesc.RedPrec := 5;
        ADesc.GreenPrec := 5;
        ADesc.BluePrec := 5;
        ADesc.RedShift := 10;
        ADesc.GreenShift := 5;
        ADesc.BlueShift := 0;
        ADesc.Depth := 15;
      end;
    24:
      begin
        // 8-8-8 mode
        ADesc.RedPrec := 8;
        ADesc.GreenPrec := 8;
        ADesc.BluePrec := 8;
        ADesc.RedShift := 16;
        ADesc.GreenShift := 8;
        ADesc.BlueShift := 0;
      end;
  else    //  32:
    // 8-8-8-8 mode, high byte can be native alpha or custom 1bit maskalpha
    ADesc.AlphaPrec := 8;
    ADesc.RedPrec := 8;
    ADesc.GreenPrec := 8;
    ADesc.BluePrec := 8;
    ADesc.AlphaShift := 24;
    ADesc.RedShift := 16;
    ADesc.GreenShift := 8;
    ADesc.BlueShift := 0;
    ADesc.Depth := 32;
  end;
end;

procedure FillRawImageDescription(const ABitmapInfo: Windows.TBitmap; out ADesc: TRawImageDescription);
begin
  ADesc.Format := ricfRGBA;

  ADesc.Depth := ABitmapInfo.bmBitsPixel;             // used bits per pixel
  ADesc.Width := ABitmapInfo.bmWidth;
  ADesc.Height := ABitmapInfo.bmHeight;
  ADesc.BitOrder := riboReversedBits;
  ADesc.ByteOrder := riboLSBFirst;
  ADesc.LineOrder := riloTopToBottom;
  ADesc.BitsPerPixel := ABitmapInfo.bmBitsPixel;      // bits per pixel. can be greater than Depth.
  ADesc.LineEnd := rileDWordBoundary;

  if ABitmapInfo.bmBitsPixel <= 8 then
  begin
    // each pixel is an index in the palette
    // TODO, ColorCount
    ADesc.PaletteColorCount := 0;
  end
  else
    ADesc.PaletteColorCount := 0;
  FillRawImageDescriptionColors(ADesc);
  ADesc.MaskBitsPerPixel := 1;
  ADesc.MaskShift := 0;
  ADesc.MaskLineEnd := rileWordBoundary; // CreateBitmap requires BGRAWord boundary
  ADesc.MaskBitOrder := riboReversedBits;
end;

function GetBitmapOrder(AWinBmp: Windows.TBitmap; ABitmap: HBITMAP): TRawImageLineOrder;
  procedure DbgLog(const AFunc: String);
  begin
    //# DebugLn('GetBitmapOrder - GetDIBits ', AFunc, ' failed: ', GetLastErrorText(Windows.GetLastError));
  end;
var
  SrcPixel: PBGRACardinal; // absolute AWinBmp.bmBits;
  OrgPixel, TstPixel: BGRACardinal;
  Scanline: Pointer;
  DC: HDC;
  Info: record
    Header: Windows.TBitmapInfoHeader;
    Colors: array[Byte] of BGRACardinal; // reserve extra color for colormasks
  end;
  FullScanLine: Boolean; // win9x requires a full scanline to be retrieved
                         // others won't fail when one pixel is requested
begin
  SrcPixel := AWinBmp.bmBits;

  if AWinBmp.bmBits = nil then
  begin
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
  if FullScanLine then
    GetMem(ScanLine, AWinBmp.bmWidthBytes)
  else
    ScanLine := nil;

  FillChar(Info.Header, sizeof(Windows.TBitmapInfoHeader), 0);
  Info.Header.biSize := sizeof(Windows.TBitmapInfoHeader);
  DC := Windows.GetDC(0);
  if Windows.GetDIBits(DC, ABitmap, 0, 1, nil, Windows.PBitmapInfo(@Info)^, DIB_RGB_COLORS) = 0 then
  begin
    DbgLog('Getinfo');
    // failed ???
    Windows.ReleaseDC(0, DC);
    Exit(riloBottomToTop);
  end;

  // Get only 1 pixel (or full scanline for win9x)
  OrgPixel := 0;
  if FullScanLine
  then begin
    if Windows.GetDIBits(DC, ABitmap, 0, 1, ScanLine, Windows.PBitmapInfo(@Info)^, DIB_RGB_COLORS) = 0 then
      DbgLog('OrgPixel')
    else
      OrgPixel := PBGRACardinal(Scanline)^;
  end
  else
  begin
    Info.Header.biWidth := 1;
    if Windows.GetDIBits(DC, ABitmap, 0, 1, @OrgPixel, Windows.PBitmapInfo(@Info)^, DIB_RGB_COLORS) = 0 then
      DbgLog('OrgPixel');
  end;

  // modify pixel
  SrcPixel^ := not SrcPixel^;

  // get test
  TstPixel := 0;
  if FullScanLine then
  begin
    if Windows.GetDIBits(DC, ABitmap, 0, 1, ScanLine, Windows.PBitmapInfo(@Info)^, DIB_RGB_COLORS) = 0 then
      DbgLog('TstPixel')
    else
      TstPixel := PBGRACardinal(Scanline)^;
  end
  else
  begin
    if Windows.GetDIBits(DC, ABitmap, 0, 1, @TstPixel, Windows.PBitmapInfo(@Info)^, DIB_RGB_COLORS) = 0 then
      DbgLog('TstPixel');
  end;

  if OrgPixel = TstPixel then
    Result := riloTopToBottom
  else
    Result := riloBottomToTop;

  // restore pixel & cleanup
  SrcPixel^ := not SrcPixel^;
  Windows.ReleaseDC(0, DC);
  if FullScanLine then
    FreeMem(Scanline);
end;

function GetBitmapBytes(AWinBmp: Windows.TBitmap; ABitmap: HBITMAP; const ARect: TRect; ALineEnd: TRawImageLineEnd; ALineOrder: TRawImageLineOrder; out AData: Pointer; out ADataSize: BGRAPtrUInt): Boolean;
var
  DC: HDC;
  Info: record
    Header: Windows.TBitmapInfoHeader;
    Colors: array[Byte] of TRGBQuad; // reserve extra colors for palette (256 max)
  end;
  H: BGRACardinal;
  R: TRect;
  SrcData: PByte;
  SrcSize: BGRAPtrUInt;
  SrcLineBytes: BGRACardinal;
  SrcLineOrder: TRawImageLineOrder;
  StartScan: Integer;
begin
  SrcLineOrder := GetBitmapOrder(AWinBmp, ABitmap);
  SrcLineBytes := (AWinBmp.bmWidthBytes + 3) and not 3;

  if AWinBmp.bmBits <> nil then
  begin
    // this is bitmapsection data :) we can just copy the bits
    // We cannot trust windows with bmWidthBytes. Use SrcLineBytes which takes
    // BGRADWord alignment into consideration
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
  if AWinBmp.bmHeight > 0 then
  begin
    Info.Header.biHeight := -AWinBmp.bmHeight;
    StartScan := AWinBmp.bmHeight - ARect.Bottom;
  end
  else
  begin
    Info.Header.biHeight := AWinBmp.bmHeight;
    StartScan := ARect.Top;
  end;
  // adjust height
  if StartScan < 0 then
  begin
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

function RawImage_FromBitmap(out ARawImage: TRawImage; ABitmap, AMask: HBITMAP; ARect: PRect = nil): Boolean;
var
  WinDIB: Windows.TDIBSection;
  WinBmp: Windows.TBitmap absolute WinDIB.dsBm;
  ASize: Integer;
  R: TRect;
begin
  ARawImage.Init;
  FillChar(WinDIB, SizeOf(WinDIB), 0);
  ASize := Windows.GetObject(ABitmap, SizeOf(WinDIB), @WinDIB);
  if ASize = 0 then
    Exit(False);

  //DbgDumpBitmap(ABitmap, 'FromBitmap - Image');
  //DbgDumpBitmap(AMask, 'FromMask - Mask');

  FillRawImageDescription(WinBmp, ARawImage.Description);
  // if it is not DIB then alpha in bitmaps is not supported => use 0 alpha prec
  if ASize < SizeOf(WinDIB) then
    ARawImage.Description.AlphaPrec := 0;

  if ARect = nil then
    R := Rect(0, 0, WinBmp.bmWidth, WinBmp.bmHeight)
  else
  begin
    R := ARect^;
    if R.Top > WinBmp.bmHeight then
      R.Top := WinBmp.bmHeight;
    if R.Bottom > WinBmp.bmHeight then
      R.Bottom := WinBmp.bmHeight;
    if R.Left > WinBmp.bmWidth then
      R.Left := WinBmp.bmWidth;
    if R.Right > WinBmp.bmWidth then
      R.Right := WinBmp.bmWidth;
  end;

  ARawImage.Description.Width := R.Right - R.Left;
  ARawImage.Description.Height := R.Bottom - R.Top;

  // copy bitmap
  Result := GetBitmapBytes(WinBmp, ABitmap, R, ARawImage.Description.LineEnd, ARawImage.Description.LineOrder, Pointer(ARawImage.Data), ARawImage.DataSize);

  // check mask
  if AMask <> 0 then
  begin
    if Windows.GetObject(AMask, SizeOf(WinBmp), @WinBmp) = 0 then
      Exit(False);
    Result := GetBitmapBytes(WinBmp, AMask, R, ARawImage.Description.MaskLineEnd, ARawImage.Description.LineOrder, Pointer(ARawImage.Mask), ARawImage.MaskSize);
  end
  else begin
    ARawImage.Description.MaskBitsPerPixel := 0;
  end;
end;

function RawImage_FromDevice(out ARawImage: TRawImage; ADC: HDC; const ARect: TRect): Boolean;
const
  FILL_PIXEL: array[0..3] of Byte = ($00, $00, $00, $FF);
var
  Info: record
    Header: Windows.TBitmapInfoHeader;
    Colors: array[0..1] of BGRACardinal; // reserve extra color for mono bitmaps
  end;
  BitsPtr: Pointer;
  copyDC, fillDC: HDC;
  bmp, copyOld, fillOld, copyBmp, fillBmp: HBITMAP;
  w, h: Integer;
begin
  if Windows.GetObjectType(ADC) = OBJ_MEMDC then
  begin
    // we can use bitmap directly
    bmp := Windows.GetCurrentObject(ADC, OBJ_BITMAP);
    copyBmp := 0;
  end
  else
  begin
    // we need to copy the image
    // use a dibsection, so we can easily retrieve the bytes
    copyDC := Windows.CreateCompatibleDC(ADC);
    w      := Windows.GetDeviceCaps(ADC, DESKTOPHORZRES);
    if w = 0 then
      w := Windows.GetDeviceCaps(ADC, HORZRES);
    h := Windows.GetDeviceCaps(ADC, DESKTOPVERTRES);
    if h = 0 then
      h := Windows.GetDeviceCaps(ADC, VERTRES);

    FillChar(Info, SizeOf(Info), 0);
    Info.Header.biSize := SizeOf(Info.Header);
    Info.Header.biWidth := w;
    Info.Header.biHeight := -h;
    Info.Header.biPlanes := 1;
    Info.Header.biBitCount := Windows.GetDeviceCaps(ADC, BITSPIXEL);
    Info.Header.biCompression := BI_RGB;

    copyBmp := Windows.CreateDIBSection(copyDC, Windows.PBitmapInfo(@Info)^, DIB_RGB_COLORS, BitsPtr, 0, 0);
    copyOld := Windows.SelectObject(copyDC, copyBmp);

    // prefill bitmap, to create an alpha channel in case of 32bpp bitmap
    if Info.Header.biBitCount > 24 then
    begin
      // using a stretchblt is faster than filling the memory ourselves,
      // which is in its turn faster than using a 24bpp bitmap
      fillBmp := Windows.CreateBitmap(1, 1, 1, 32, @FILL_PIXEL);
      fillDC  := Windows.CreateCompatibleDC(ADC);
      fillOld := Windows.SelectObject(fillDC, fillBmp);

      Windows.StretchBlt(copyDC, 0, 0, w, h, fillDC, 0, 0, 1, 1, SRCCOPY);
      Windows.SelectObject(fillDC, fillOld);
      Windows.DeleteDC(fillDC);
      Windows.DeleteObject(fillBmp);
      Windows.BitBlt(copyDC, 0, 0, w, h, ADC, 0, 0, SRCPAINT);
    end
    else
    begin
      // copy image
      Windows.BitBlt(copyDC, 0, 0, w, h, ADC, 0, 0, SRCCOPY);
    end;

    Windows.SelectObject(copyDC, copyOld);
    Windows.DeleteDC(copyDC);
    bmp := copyBmp;
  end;
  if bmp = 0 then
    Exit(False);
  Result := RawImage_FromBitmap(ARawImage, bmp, 0, @ARect);
  if copyBmp <> 0 then
    Windows.DeleteObject(copyBmp);
end;

function RawImage_CreateBitmaps(const ARawImage: TRawImage; out ABitmap, AMask: HBitmap; ASkipMask: Boolean): Boolean;
var
  ADesc: TRawImageDescription absolute ARawImage.Description;

  function DoBitmap: Boolean;
  var
    DC: HDC;
    Info: record
      Header: Windows.TBitmapInfoHeader;
      Colors: array[0..1] of BGRACardinal; // reserve extra color for mono bitmaps
    end;
    DstLinePtr, SrcLinePtr: PByte;
    SrcPixelPtr, DstPixelPtr: PByte;
    DstLineSize, SrcLineSize: BGRAPtrUInt;
    x, y: Integer;
    Ridx, Gidx, Bidx, Aidx, Align, SrcBytes, DstBpp: Byte;
  begin
    if (ADesc.BitsPerPixel = 1) and (ADesc.LineEnd = rileWordBoundary) then
    begin
      // default BW, BGRAWord aligned bitmap
      ABitmap := Windows.CreateBitmap(ADesc.Width, ADesc.Height, 1, 1, ARawImage.Data);
      Exit(ABitmap <> 0);
    end;

    // for 24 bits images, BPP can be 24 or 32
    // 32 shouldn't be use since we don't fill the alpha channel

    if ADesc.Depth = 24 then
      DstBpp := 24
    else
      DstBpp := ADesc.BitsPerPixel;

    FillChar(Info, SizeOf(Info), 0);
    Info.Header.biSize := SizeOf(Info.Header);
    Info.Header.biWidth := ADesc.Width;
    if ADesc.LineOrder = riloTopToBottom  then
      Info.Header.biHeight := -ADesc.Height // create top to bottom
    else
      Info.Header.biHeight := ADesc.Height; // create bottom to top
    Info.Header.biPlanes := 1;
    Info.Header.biBitCount := DstBpp;
    Info.Header.biCompression := BI_RGB;
    {Info.Header.biSizeImage := 0;}
    { first color is black, second color is white, for monochrome bitmap }
    Info.Colors[1] := $FFFFFFFF;

    DC := Windows.GetDC(0);
    // Use createDIBSection, since only devicedepth bitmaps can be selected into a DC
    // when they are created with createDIBitmap
    //  ABitmap := Windows.CreateDIBitmap(DC, Info.Header, CBM_INIT, ARawImage.Data, Windows.PBitmapInfo(@Info)^, DIB_RGB_COLORS);
    ABitmap := Windows.CreateDIBSection(DC, Windows.PBitmapInfo(@Info)^, DIB_RGB_COLORS, Pointer(DstLinePtr), 0, 0);
    Windows.ReleaseDC(0, DC);

    if ABitmap = 0 then
      Exit(False);
    if DstLinePtr = nil then
      Exit(False);

    DstLineSize := Windows.MulDiv(DstBpp, ADesc.Width, 8);
    // align to BGRADWord
    Align := DstLineSize and 3;
    if Align > 0 then
      Inc(DstLineSize, BGRAPtrUInt(4 - Align));

    SrcLinePtr  := ARawImage.Data;
    SrcLineSize := ADesc.BytesPerLine;

    // copy the image data
    if ADesc.Depth >= 24 then
    begin
      // check if a pixel copy is needed
      // 1) Windows uses alpha channel in 32 bpp modes, despite documentation statement that it is ignored. Tested under Windows XP SP3
      // Wine also relies on this undocumented behaviour!
      // So, we need to cut unused A-channel, otherwise we would get black image
      //
      // 2) incompatible channel order
      ADesc.GetRGBIndices(Ridx, Gidx, Bidx, Aidx);

      if ((ADesc.BitsPerPixel = 32) and (ADesc.Depth = 24)) or (Bidx <> 0) or (Gidx <> 1) or (Ridx <> 2) then
      begin
        // copy pixels
        SrcBytes := ADesc.BitsPerPixel div 8;
        for y := 0 to ADesc.Height - 1 do
        begin
          DstPixelPtr := DstLinePtr;
          SrcPixelPtr := SrcLinePtr;
          for x := 0 to ADesc.Width - 1 do
          begin
            DstPixelPtr[0] := SrcPixelPtr[Bidx];
            DstPixelPtr[1] := SrcPixelPtr[Gidx];
            DstPixelPtr[2] := SrcPixelPtr[Ridx];

            Inc(DstPixelPtr, 3); //move to the next dest RGB triple
            Inc(SrcPixelPtr, SrcBytes);
          end;
          Inc(DstLinePtr, DstLineSize);
          Inc(SrcLinePtr, SrcLineSize);
        end;
        Exit(True);
      end;
    end;

    // no pixelcopy needed
    // check if we can move using one call
    if ADesc.LineEnd = rileDWordBoundary then
    begin
      Move(SrcLinePtr^, DstLinePtr^, DstLineSize * ADesc.Height);
      Exit(True);
    end;

    //Can't use just one move, as different alignment
    for y := 0 to ADesc.Height - 1 do
    begin
      Move(SrcLinePtr^, DstLinePtr^, DstLineSize);
      Inc(DstLinePtr, DstLineSize);
      Inc(SrcLinePtr, SrcLineSize);
    end;
    Result := True;
  end;

begin
  AMask := 0;
  Result := DoBitmap;
  if not Result then Exit;

  //DbgDumpBitmap(ABitmap, 'CreateBitmaps - Image');
  if ASkipMask then Exit;

  AMask := Windows.CreateBitmap(ADesc.Width, ADesc.Height, 1, 1, ARawImage.Mask);
//  if AMask = 0 then
//    DebugLn('Windows.CreateBitmap returns 0. Reason = ' + GetLastErrorText(Windows.GetLastError));
  Result := AMask <> 0;
  //DbgDumpBitmap(AMask, 'CreateBitmaps - Mask');
end;

{$IFEND}

{$IFDEF FPC}
function FPColorToTColor(const FPColor: TFPColor): TColor;
begin
  result := Graphics.FPColorToTColor(FPColor);
end;

function TColorToFPColor(const c: TColor): TFPColor;
begin
  result := Graphics.TColorToFPColor(c);
end;

function RGBToColor(R, G, B: Byte): TColor;
begin
  result := Graphics.RGBToColor(R, G, B);
end;

procedure RedGreenBlue(rgb: TColor; out Red, Green, Blue: Byte);
begin
  Graphics.RedGreenBlue(rgb, Red, Green, Blue);
end;
{$ENDIF}

function ColorToRGB(c: TColor): TColor;
begin
  result := Graphics.ColorToRGB(c);
end;

function clRgbBtnHighlight: TColor;
begin
  result := Graphics.ColorToRGB(clBtnHighlight);
end;

function clRgbBtnShadow: TColor;
begin
  result := Graphics.ColorToRGB(clBtnShadow);
end;

procedure TGBRACustomGraphic.Clear;
begin
  {$IFNDEF BDS}inherited Clear;{$ENDIF}
end;

function TGBRACustomGraphic.GetMimeType: string;
begin
  Result := {$IFNDEF BDS}inherited GetMimeType;{$ELSE}'';{$ENDIF}
end;

{$IFNDEF FPC}
initialization
  DicCanvasVar      := TDicCanvasVar.Create;

finalization
  FreeAndNil(DicCanvasVar);
{$ENDIF}

end.

{$ELSE}

{$IFDEF FPC}
uses
  Classes, FPCanvas, FPImage
  {$DEFINE INCLUDE_USES}
  {$IFDEF BGRABITMAP_USE_FPGUI}
    {$i bgrafpgui.inc}
  {$ELSE}
    {$i bgranogui.inc}
  {$ENDIF};

{$DEFINE INCLUDE_INTERFACE}
{$IFDEF BGRABITMAP_USE_FPGUI}
  {$i bgrafpgui.inc}
{$ELSE}
  {$i bgranogui.inc}
{$ENDIF}

type
  {* Pointer to a ''TColor'' value }
  PColor = ^TColor;
  {* Contains a color stored as RGB. The red/green/blue values
   range from 0 to 255. The formula to get the color value is:
   * ''color'' = ''red'' + (''green'' '''shl''' 8) + (''blue'' '''shl''' 16)
   *except with fpGUI where it is:
   * ''color'' = (''red'' '''shl''' 16) + (''green'' '''shl''' 8) + ''blue'' }{import
  TColor = Int32;
  }
  {** Converts a ''TFPColor'' into a ''TColor'' value }
  function FPColorToTColor(const FPColor: TFPColor): TColor;
  {** Converts a ''TColor'' into a ''TFPColor'' value }
  function TColorToFPColor(const c: TColor): TFPColor;

type
  {* Direction of change in a gradient }
  TGradientDirection = (
    {** Color changes vertically }
    gdVertical,
    {** Color changes horizontally }
    gdHorizontal);

  {* Antialiasing mode for a Canvas }
  TAntialiasingMode = (
    {** It does not matter if there is antialiasing or not }
    amDontCare,
    {** Antialiasing is required (BGRACanvas provide it) }
    amOn,
    {** Antialiasing is disabled }
    amOff);

  {* How to draw the end of line }
  TPenEndCap = TFPPenEndCap;

const
    {** Draw a half-disk at the end of the line. The diameter of the disk is
        equal to the pen width. }
    pecRound = FPCanvas.pecRound;
    {** Draw a half-square. The size of the square is equal to the pen width.
        This is visually equivalent to extend the line of half the pen width }
    pecSquare = FPCanvas.pecSquare;
    {** The line ends exactly at the end point }
    pecFlat = FPCanvas.pecFlat;

type
  {* How to join segments. This makes sense only for geometric pens (that
     have a certain width) }
  TPenJoinStyle = TFPPenJoinStyle;

const
    {** Segments are joined by filling the gap with an arc }
    pjsRound = FPCanvas.pjsRound;
    {** Segments are joind by filling the gap with an intermediary segment }
    pjsBevel = FPCanvas.pjsBevel;
    {** Segments are joined by extending them up to their intersection.
        There is a miter limit so that if the intersection is too far,
        an intermediary segment is used }
    pjsMiter = FPCanvas.pjsMiter;

type
  {* Style to use for the pen. The unit for the pattern is the width of the
     line }
  TPenStyle = TFPPenStyle;

const
  {** Pen is continuous }
  psSolid = FPCanvas.psSolid;
  {** Pen is dashed. The dash have a length of 3 unit and the gaps of 1 unit }
  psDash = FPCanvas.psDash;
  {** Pen is dotted. The dots have a length of 1 unit and the gaps of 1 unit }
  psDot = FPCanvas.psDot;
  {** Pattern is a dash of length 3 followed by a dot of length 1, separated by a gap of length 1 }
  psDashDot = FPCanvas.psDashDot;
  {** Dash of length 3, and two dots of length 1 }
  psDashDotDot = FPCanvas.psDashDotDot;
  {** Pen is not drawn }
  psClear = FPCanvas.psClear;
  {** Not used. Provided for compatibility }
  psInsideframe = FPCanvas.psInsideframe;
  {** Custom pattern used }
  psPattern = FPCanvas.psPattern;

type
  TTransparentMode = (
    tmAuto,
    tmFixed
    );

  { TPen }
  {* A class containing a pen }
  TPen = class(TFPCustomPen)
  private
    FEndCap: TPenEndCap;
    FJoinStyle: TPenJoinStyle;
    function GetColor: TColor;
    procedure SetColor(AValue: TColor);
  public
    constructor Create; override;
    {** Color of the pen }
    property Color: TColor read GetColor write SetColor;
    {** End cap of the pen: how to draw the ends of the lines }
    property EndCap;
    {** Join style: how to join the segments of a polyline }
    property JoinStyle;
    {** Pen style: solid, dash, dot... }{inherited
    property Style : TPenStyle read write;
   }{** Pen width in pixels }{inherited
    property Width : Integer read write;
   }
  end;

  {* Vertical position of a text }
  TTextLayout = (tlTop, tlCenter, tlBottom);
  {* Styles to describe how a text is drawn in a rectangle }
  TTextStyle = packed record
    {** Horizontal alignment }
    Alignment : TAlignment;

    {** Vertical alignment }
    Layout    : TTextLayout;

    {** If WordBreak is false then process #13, #10 as
        standard chars and perform no Line breaking }
    SingleLine: boolean;

    {** Clip Text to passed Rectangle }
    Clipping  : boolean;

    {** Replace #9 by apropriate amount of spaces (default is usually 8) }
    ExpandTabs: boolean;

    {** Process first single '&' per line as an underscore and draw '&&' as '&' }
    ShowPrefix: boolean;

    {** If line of text is too long too fit between left and right boundaries
        try to break into multiple lines between words. See also ''EndEllipsis'' }
    Wordbreak : boolean;

    {** Fills background with current brush }
    Opaque    : boolean;

    {** Use the system font instead of canvas font }
    SystemFont: Boolean;

    {** For RightToLeft text reading (Text Direction) }
    RightToLeft: Boolean;

    {** If line of text is too long to fit between left and right boundaries
        truncates the text and adds "...". If Wordbreak is set as well,
        Workbreak will dominate }
    EndEllipsis: Boolean;
  end;

  {* Option for floodfill (used in BGRACanvas) }
  TFillStyle =
    (
      {** Fill up to the color (it fills all except the specified color) }
      fsSurface,
      {** Fill the specified color (it fills only connected pixels of this color) }
      fsBorder
    );
  {* How to handle polygons that intersect with themselves and
     overlapping polygons }
  TFillMode = (
    {** Each time a boundary is found, it enters or exit the filling zone }
    fmAlternate,
    {** Adds or subtract 1 depending on the order of the points of the
        polygons (clockwise or counter clockwise) and fill when the
        result is non-zero. So, to draw a hole, you must specify the points
        of the hole in the opposite order }
    fmWinding);

  {* Pattern when filling with a brush. It is used in BGRACanvas but can
     also be created with TBGRABitmap.CreateBrushTexture function }
  TBrushStyle = TFPBrushStyle;

const
  {** Fill with the current color }
  bsSolid = FPCanvas.bsSolid;
  {** Does not fill at all }
  bsClear = FPCanvas.bsClear;
  {** Draw horizontal lines }
  bsHorizontal = FPCanvas.bsHorizontal;
  {** Draw vertical lines }
  bsVertical = FPCanvas.bsVertical;
  {** Draw diagonal lines from top-left to bottom-right }
  bsFDiagonal = FPCanvas.bsFDiagonal;
  {** Draw diagonal lines from bottom-left to top-right }
  bsBDiagonal = FPCanvas.bsBDiagonal;
  {** Draw both horizontal and vertical lines }
  bsCross = FPCanvas.bsCross;
  {** Draw both diagonal lines }
  bsDiagCross = FPCanvas.bsDiagCross;
{$IFDEF FPC}

type
  { TBrush }
  {* A class describing a brush }
  TBrush = class(TFPCustomBrush)
  private
    function GetColor: TColor;
    procedure SetColor(AValue: TColor);
    public
      constructor Create; override;
      {** Color of the brush }
      property Color: TColor read GetColor write SetColor;
      {** Style of the brush: solid, diagonal lines, horizontal lines... }{inherited
      property Style : TBrushStyle read write;
      }
  end;

  TGraphic = class;
  { TCanvas }
  {* A surface on which to draw }
  TCanvas = class
  protected
    FCanvas: TGUICanvas;
  public
    constructor Create(ACanvas: TGUICanvas);
    {** Draw an image with top-left corner at (''x'',''y'') }
    procedure Draw(x,y: integer; AImage: TGraphic);
    {** Draw and stretch an image within the rectangle ''ARect'' }
    procedure StretchDraw(ARect: TRect; AImage: TGraphic);
    property GUICanvas: TGUICanvas read FCanvas;
  end;

  { TGraphic }
  {* A class containing any element that can be drawn within rectangular bounds }
  TGraphic = class(TPersistent)
  protected
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); virtual; abstract;
    function GetEmpty: Boolean; virtual; abstract;
    function GetHeight: Integer; virtual; abstract;
    function GetWidth: Integer; virtual; abstract;
    function GetTransparent: Boolean; virtual; abstract;
    procedure SetTransparent(Value: Boolean); virtual; abstract;
    procedure SetHeight(Value: Integer); virtual; abstract;
    procedure SetWidth(Value: Integer); virtual; abstract;
    function GetMimeType: string; virtual;
  public
    constructor Create; virtual;
    {** Load the content from a given file }
    procedure LoadFromFile({%H-}const Filename: string); virtual;
    {** Load the content from a given stream }
    procedure LoadFromStream(Stream: TStream); virtual; abstract;
    {** Saves the content to a file }
    procedure SaveToFile({%H-}const Filename: string); virtual;
    {** Saves the content into a given stream }
    procedure SaveToStream(Stream: TStream); virtual; abstract;
    {** Returns the list of possible file extensions }
    class function GetFileExtensions: string; virtual;
    {** Clears the content }
    procedure Clear; virtual;
  public
    {** Returns if the content is completely empty }
    property Empty: Boolean read GetEmpty;
    {** Returns the height of the bounding rectangle }
    property Height: Integer read GetHeight write SetHeight;
    {** Returns the width of the bounding rectangle }
    property Width: Integer read GetWidth write SetWidth;
    {** Gets or sets if it is drawn with transparency }
    property Transparent: Boolean read GetTransparent write SetTransparent;
  end;

  { TBitmap }
  {* Contains a bitmap }
  TBitmap = class(TGraphic)
  private
    FHeight: integer;
    FWidth: integer;
    FInDraw: boolean;
    FTransparent: boolean;
    FTransparentColor: TColor;
    FTransparentMode: TTransparentMode;
    function GetCanvas: TCanvas;
    function GetRawImage: TRawImage;
    procedure SetTransparentColor(AValue: TColor);
    procedure SetTransparentMode(AValue: TTransparentMode);
  protected
    FRawImage: TRawImage;
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;
    procedure Changed(Sender: TObject); virtual;
    function GetHeight: Integer; override;
    function GetWidth: Integer; override;
    procedure SetHeight(Value: Integer); override;
    procedure SetWidth(Value: Integer); override;
    function GetEmpty: Boolean; override;
    function GetTransparent: Boolean; override;
    procedure SetTransparent({%H-}Value: Boolean); override;
    function GetMimeType: string; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure LoadFromStream({%H-}Stream: TStream); override;
    procedure SaveToStream({%H-}Stream: TStream); override;
    {** Width of the bitmap in pixels }
    property Width: integer read GetWidth write SetWidth;
    {** Height of the bitmap in pixels }
    property Height: integer read GetHeight write SetHeight;
    property RawImage: TRawImage read GetRawImage;
    property Canvas: TCanvas read GetCanvas;
    property TransparentColor: TColor read FTransparentColor
             write SetTransparentColor default clDefault;
    property TransparentMode: TTransparentMode read FTransparentMode
             write SetTransparentMode default tmAuto;
  end;
  {$ENDIF}
  TRasterImage = TBitmap;

  {* Available font styles }
  TFontStyle = (
    {** Font is bold }
    fsBold,
    {** Font is italic }
    fsItalic,
    {** An horizontal line is drawn in the middle of the text }
    fsStrikeOut,
    {** Text is underlined }
    fsUnderline);
  {** A combination of font styles }
  TFontStyles = set of TFontStyle;
  {* Quality to use when font is rendered by the system }
  TFontQuality = (fqDefault, fqDraft, fqProof, fqNonAntialiased, fqAntialiased, fqCleartype, fqCleartypeNatural);

  {$IFDEF FPC}
  { TFont }
  {* Contains the description of a font }
  TFont = class(TFPCustomFont)
  private
    FPixelsPerInch, FHeight: Integer;
    FQuality: TFontQuality;
    FStyle: TFontStyles;
    function GetColor: TColor;
    function GetHeight: Integer;
    function GetSize: Integer;
    function GetStyle: TFontStyles;
    procedure SetColor(AValue: TColor);
    procedure SetHeight(AValue: Integer);
    procedure SetQuality(AValue: TFontQuality);
    procedure SetStyle(AValue: TFontStyles);
  protected
    procedure SetSize(AValue: Integer); override;
  public
    constructor Create; override;
    {** Pixels per inches }
    property PixelsPerInch: Integer read FPixelsPerInch write FPixelsPerInch;
    {** Color of the font }
    property Color: TColor read GetColor write SetColor;
    {** Height of the font in pixels. When the number is negative, it indicates a size in pixels }
    property Height: Integer read GetHeight write SetHeight;
    {** Size of the font in inches. When the number is negative, it indicates a height in inches }
    property Size: Integer read GetSize write SetSize;
    {** Quality of the font rendering }
    property Quality: TFontQuality read FQuality write SetQuality;
    {** Style to apply to the text }
    property Style: TFontStyles read GetStyle write SetStyle;
  end;
  {$ENDIF}
{* Multiply and divide the number allowing big intermediate number and rounding the result }
function MulDiv(nNumber, nNumerator, nDenominator: Integer): Integer;
{* Round the number using math convention }
function MathRound(AValue: ValReal): BGRAInt64; {$ifdef inline}inline;{$endif}

implementation

uses sysutils, BGRAUTF8;

{$DEFINE INCLUDE_IMPLEMENTATION}
{$IFDEF BGRABITMAP_USE_FPGUI}
  {$i bgrafpgui.inc}
{$ELSE}
  {$i bgranogui.inc}
{$ENDIF}

function MathRound(AValue: ValReal): BGRAInt64; {$ifdef inline}inline;{$endif}
begin
  if AValue >= 0 then
    Result := Trunc(AValue + 0.5)
  else
    Result := Trunc(AValue - 0.5);
end;

function MulDiv(nNumber, nNumerator, nDenominator: Integer): Integer;
begin
  if nDenominator = 0 then
    Result := -1
  else
    Result := MathRound(BGRAInt64(nNumber) * BGRAInt64(nNumerator) / nDenominator);
end;

function FPColorToTColor(const FPColor: TFPColor): TColor;
begin
  {$IFDEF BGRABITMAP_USE_FPGUI}
  Result:=((FPColor.Blue shr 8) and $ff)
       or (FPColor.Green and $ff00)
       or ((FPColor.Red shl 8) and $ff0000);
  {$ELSE}
  Result:=((FPColor.Red shr 8) and $ff)
       or (FPColor.Green and $ff00)
       or ((FPColor.Blue shl 8) and $ff0000);
  {$ENDIF}
end;

function TColorToFPColor(const c: TColor): TFPColor;
begin
  {$IFDEF BGRABITMAP_USE_FPGUI}
  Result.Blue:=(c and $ff);
  Result.Blue:=Result.Blue+(Result.Blue shl 8);
  Result.Green:=(c and $ff00);
  Result.Green:=Result.Green+(Result.Green shr 8);
  Result.Red:=(c and $ff0000) shr 8;
  Result.Red:=Result.Red+(Result.Red shr 8);
  {$ELSE}
  Result.Red:=(c and $ff);
  Result.Red:=Result.Red+(Result.Red shl 8);
  Result.Green:=(c and $ff00);
  Result.Green:=Result.Green+(Result.Green shr 8);
  Result.Blue:=(c and $ff0000) shr 8;
  Result.Blue:=Result.Blue+(Result.Blue shr 8);
  {$ENDIF}
  Result.Alpha:=FPImage.alphaOpaque;
end;

{$IFDEF FPC}

{ TGraphic }

function TGraphic.GetMimeType: string;
begin
  result := '';
end;

constructor TGraphic.Create;
begin
  //nothing
end;

procedure TGraphic.LoadFromFile(const Filename: string);
var
  Stream: TStream;
begin
  Stream := TFileStreamUTF8.Create(Filename, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TGraphic.SaveToFile(const Filename: string);
var
  Stream: TStream;
begin
  Stream := TFileStreamUTF8.Create(Filename, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

class function TGraphic.GetFileExtensions: string;
begin
  result := '';
end;

procedure TGraphic.Clear;
begin
  //nothing
end;

{ TCanvas }

constructor TCanvas.Create(ACanvas: TGUICanvas);
begin
  FCanvas := ACanvas;
end;

procedure TCanvas.Draw(x, y: integer; AImage: TGraphic);
begin
  if AImage is TBitmap then
    FCanvas.DrawImage(x,y, TBitmap(AImage).RawImage)
  else
    AImage.Draw(self, rect(x,y,x+AImage.Width,y+AImage.Height));
end;

procedure TCanvas.StretchDraw(ARect: TRect; AImage: TGraphic);
begin
  if AImage is TBitmap then
    FCanvas.StretchDraw(ARect.Left,ARect.Top,ARect.Right-ARect.Left,ARect.Bottom-ARect.Top, TBitmap(AImage).RawImage)
  else
    AImage.Draw(self, ARect);
end;

{ TPen }

procedure TPen.SetColor(AValue: TColor);
begin
  FPColor := TColorToFPColor(AValue);
end;

function TPen.GetColor: TColor;
begin
  result := FPColorToTColor(FPColor);
end;

constructor TPen.Create;
begin
  inherited Create;
  Mode := pmCopy;
  Style := psSolid;
  Width := 1;
  FPColor := colBlack;
  FEndCap:= pecRound;
  FJoinStyle:= pjsRound;
end;

{ TBrush }

function TBrush.GetColor: TColor;
begin
  result := FPColorToTColor(FPColor);
end;

procedure TBrush.SetColor(AValue: TColor);
begin
  FPColor := TColorToFPColor(AValue);
end;

constructor TBrush.Create;
begin
  inherited Create;
  FPColor := colWhite;
end;

{ TFont }

function TFont.GetColor: TColor;
begin
  result := FPColorToTColor(FPColor);
end;

function TFont.GetHeight: Integer;
begin
  result := FHeight;
end;

function TFont.GetSize: Integer;
begin
  Result := inherited Size;
end;

function TFont.GetStyle: TFontStyles;
begin
  result := FStyle;
end;

procedure TFont.SetColor(AValue: TColor);
begin
  FPColor := TColorToFPColor(AValue);
end;

procedure TFont.SetHeight(AValue: Integer);
begin
  if Height <> AValue then
  begin
    FHeight := AValue;
    inherited SetSize(-MulDiv(AValue, 72, FPixelsPerInch));
  end;
end;

procedure TFont.SetQuality(AValue: TFontQuality);
begin
  if FQuality=AValue then Exit;
  FQuality:=AValue;
end;

procedure TFont.SetSize(AValue: Integer);
begin
  if Size <> AValue then
  begin
    inherited SetSize(AValue);
    FHeight := -MulDiv(AValue, FPixelsPerInch, 72);
  end;
end;

procedure TFont.SetStyle(AValue: TFontStyles);
begin
  if FStyle <> AValue then
  begin
    FStyle := AValue;
    inherited SetFlags(5, fsBold in FStyle);
    inherited SetFlags(6, fsItalic in FStyle);
    inherited SetFlags(7, fsUnderline in FStyle);
    inherited SetFlags(8, fsStrikeOut in FStyle);
  end;
end;

constructor TFont.Create;
begin
  FPixelsPerInch := GetScreenDPIY;
  FQuality := fqDefault;
  FPColor := colBlack;
end;

{ TBitmap }

procedure TBitmap.SetWidth(Value: Integer);
begin
  if FWidth=Value then Exit;
  FWidth:=Value;
end;

function TBitmap.GetEmpty: Boolean;
begin
  result := (Width = 0) or (Height = 0);
end;

function TBitmap.GetTransparent: Boolean;
begin
  result := FTransparent;
end;

procedure TBitmap.SetTransparent(Value: Boolean);
begin
  if Value = FTransparent then exit;
  FTransparent:= Value;
end;

procedure TBitmap.SetTransparentColor(AValue: TColor);
begin
  if FTransparentColor = AValue then exit;
  FTransparentColor := AValue;

  if AValue = clDefault then
    FTransparentMode := tmAuto
  else
    FTransparentMode := tmFixed;
end;

procedure TBitmap.SetTransparentMode(AValue: TTransparentMode);
begin
  if AValue = TransparentMode then exit;
  FTransparentMode := AValue;

  if AValue = tmAuto
  then TransparentColor := clDefault
end;

function TBitmap.GetMimeType: string;
begin
  Result:= 'image/bmp';
end;

procedure TBitmap.Changed(Sender: TObject);
begin
  //nothing
end;

procedure TBitmap.LoadFromStream(Stream: TStream);
begin
  raise exception.Create('Not implemented');
end;

procedure TBitmap.SaveToStream(Stream: TStream);
begin
  raise exception.Create('Not implemented');
end;

procedure TBitmap.SetHeight(Value: Integer);
begin
  if FHeight=Value then Exit;
  FHeight:=Value;
end;

function TBitmap.GetRawImage: TRawImage;
begin
  FRawImage.BGRASetSizeAndTransparency(FWidth, FHeight, FTransparent);
  result := FRawImage;
end;

procedure TBitmap.Draw(ACanvas: TCanvas; const Rect: TRect);
begin
  if FInDraw then exit;
  FInDraw := true;
  ACanvas.StretchDraw(Rect, self);
  FInDraw := false;
end;

function TBitmap.GetHeight: Integer;
begin
  result := FHeight;
end;

function TBitmap.GetWidth: Integer;
begin
  result := FWidth;
end;

function TBitmap.GetCanvas: TCanvas;
begin
  result := nil;
  raise exception.Create('Canvas not available');
end;

constructor TBitmap.Create;
begin
  FRawImage := TRawImage.Create;
  FTransparent:= false;
end;

destructor TBitmap.Destroy;
begin
  FRawImage.Free;
  inherited Destroy;
end;
{$ENDIF}
{$ENDIF}

end.
{$ENDIF}



(*procedure TCustomImageListHelper.GetRawImage(Index: Integer; out Image: TRawImage);
var
  FData: array of TRGBAQuad;
  procedure FillDescription(out ADesc: TRawImageDescription);
  begin
    ADesc.Init;
    ADesc.Format := ricfRGBA;
    ADesc.PaletteColorCount := 0;
    ADesc.MaskBitsPerPixel := 0;
    ADesc.Depth := 32;
    ADesc.Width := Width;
    ADesc.Height := Height;
    ADesc.BitOrder := riboBitsInOrder;
    ADesc.ByteOrder := riboMSBFirst;
    ADesc.LineOrder := riloTopToBottom;
    ADesc.BitsPerPixel := 32;
    ADesc.LineEnd := rileDWordBoundary;
    ADesc.RedPrec := 8; // red precision. bits for red
    ADesc.RedShift := 8;
    ADesc.GreenPrec := 8;
    ADesc.GreenShift := 16;
    ADesc.BluePrec := 8;
    ADesc.BlueShift := 24;
    ADesc.AlphaPrec := 8;
    ADesc.AlphaShift := 0;
  end;

  procedure CheckIndex(AIndex: Integer; AForInsert: Boolean);
    procedure Error; // aviod exceptionframe generation
    begin
      raise EInvalidOperation.Create('Invalid Image Index');
    end;
  begin
    if AForInsert
    then begin
      if AIndex > Count then Error;
    end
    else begin
      if AIndex >= Count then Error;
    end;
  end;

  procedure BitmapToData;
  Var
    MemDC: HDC;
    hbmImage : TImageInfo;
    Info: record
      Header: Windows.TBitmapInfoHeader;
      Colors: array[Byte] of Cardinal; // reserve extra color for colormasks
    end;
  begin
    MemDC := CreateCompatibleDC(0);

    ImageList_GetImageInfo(Handle, Index, hbmImage);
    SelectObject(MemDC, hbmImage.hbmImage);

    {FillChar(Info.Header, sizeof(Windows.TBitmapInfoHeader), 0);
    Info.Header.biSize := sizeof(Windows.TBitmapInfoHeader);
    if GetDIBits(MemDC, hbmImage.hbmImage, 0, 1, nil, Windows.PBitmapInfo(@Info)^, DIB_RGB_COLORS) = 0
    then begin
      // failed ???
      ReleaseDC(0, MemDC);
      Exit;
    end;}
    // retrieve the data though GetDIBits

    // initialize bitmapinfo structure
    Info.Header.biSize := sizeof(Info.Header);
    Info.Header.biPlanes := 1;
    Info.Header.biBitCount := 32;
    Info.Header.biCompression := BI_RGB;
    Info.Header.biSizeImage := 0;
    Info.Header.biWidth := hbmImage.rcImage.Width;
    Info.Header.biHeight := -hbmImage.rcImage.Height;

    GetDIBits(MemDC, hbmImage.hbmImage, 0, 1, FData, Windows.PBitmapInfo(@Info)^, DIB_RGB_COLORS);
    DeleteDC(MemDC);
  end;
begin
  Image.Init;
  if (Count = 0) then Exit;
  CheckIndex(Index, False);
  FillDescription(Image.Description);
  if Index >= 0 then
  begin
    BitmapToData;
    Image.DataSize := Width * Height * SizeOf(FData[0]);
    Image.Data     := @FData{[Index * Width * Height]};
  end;
end; *)



