unit IntfGraphics;

interface

uses sysutils, {$IFnDEF FPC}Windows, Messages, Graphics, Controls, types, {$ENDIF} GraphType, strutils, classes, FPImage;

  { TLazIntfImage }
(*type
  TGraphicsColor = -$7FFFFFFF-1..$7FFFFFFF;

  TLazIntfImageGetPixelProc = procedure(x, y: integer; out Color: TFPColor) of object;
  TLazIntfImageSetPixelProc = procedure(x, y: integer; const Color: TFPColor) of object;

  TOnReadRawImageBits = procedure(TheData: PByte;
    const Position: TRawImagePosition;
    Prec, Shift: cardinal; var Bits: word);

  TOnWriteRawImageBits = procedure(TheData: PByte;
    const Position: TRawImagePosition;
    Prec, Shift: cardinal; Bits: word);

  TLazIntfImage = class(TFPCustomImage)
  private
    FRawImage: TRawImage;
    FLineStarts: PRawImageLineStarts;
    FMaskLineStarts: PRawImageLineStarts;
    FMaskSet: Boolean; // Set when at least one maskpixel is set
    FUpdateCount: integer;
    fCreateAllDataNeeded: boolean;
    FGetSetColorFunctionsUpdateNeeded: boolean;
    FReadRawImageBits: TOnReadRawImageBits;
    FWriteRawImageBits: TOnWriteRawImageBits;
    FMaskReadRawImageBits: TOnReadRawImageBits;
    FMaskWriteRawImageBits: TOnWriteRawImageBits;
    FDataOwner: Boolean;
    function GetMasked(x, y: integer): Boolean;
    function GetTColors(x, y: integer): TGraphicsColor;

    procedure InternalSetSize(AWidth, AHeight: integer);

    procedure SetMasked(x, y: integer; const AValue: Boolean);
    procedure SetTColors(x, y: integer; const AValue: TGraphicsColor);
  protected
    FGetInternalColorProc: TLazIntfImageGetPixelProc;
    FSetInternalColorProc: TLazIntfImageSetPixelProc;
    procedure SetUsePalette (Value: boolean); override;
    procedure SetInternalColor(x, y: integer; const Value: TFPColor); override;
    function  GetInternalColor(x, y: integer): TFPColor; override;
    procedure SetInternalPixel (x,y:integer; Value:integer); override;
    function  GetInternalPixel (x,y:integer) : integer; override;
    procedure FreeData; virtual;
    procedure SetDataDescription(const ADescription: TRawImageDescription); virtual;
    procedure ChooseGetSetColorFunctions; virtual;
    procedure ChooseRawBitsProc(BitsPerPixel: cardinal;
                                ByteOrder: TRawImageByteOrder;
                                BitOrder: TRawImageBitOrder;
                                out ProcReadRawImageBits: TOnReadRawImageBits;
                                out ProcWriteRawImageBits: TOnWriteRawImageBits);
    // get color functions
    procedure GetColor_Generic(x, y: integer; out Value: TFPColor);
    procedure GetColor_RGBA_NoPalette(x, y: integer; out Value: TFPColor);
    procedure GetColor_RGB_NoPalette(x, y: integer; out Value: TFPColor);
    procedure GetColor_Gray_NoPalette(x, y: integer; out Value: TFPColor);
    procedure GetColor_GrayAlpha_NoPalette(x, y: integer; out Value: TFPColor);
    procedure GetColor_NULL(x, y: integer; out Value: TFPColor);
    // 32 bpp alpha
    procedure GetColor_BPP32_A8R8G8B8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_A8B8G8R8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_A8G8R8B8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_A8G8B8R8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_A8R8B8G8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_A8B8R8G8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_B8G8R8A8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_R8G8B8A8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_G8B8R8A8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_G8R8B8A8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_B8R8G8A8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_R8B8G8A8_BIO_TTB(x, y: integer; out Value: TFPColor);
    // 32 bpp no alpha
    procedure GetColor_BPP32_X8R8G8B8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_X8B8G8R8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_X8G8R8B8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_X8G8B8R8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_X8R8B8G8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_X8B8R8G8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_B8G8R8X8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_R8G8B8X8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_G8B8R8X8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_G8R8B8X8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_B8R8G8X8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP32_R8B8G8X8_BIO_TTB(x, y: integer; out Value: TFPColor);
    // 24 bpp
    procedure GetColor_BPP24_B8G8R8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP24_R8G8B8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP24_G8B8R8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP24_G8R8B8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP24_B8R8G8_BIO_TTB(x, y: integer; out Value: TFPColor);
    procedure GetColor_BPP24_R8B8G8_BIO_TTB(x, y: integer; out Value: TFPColor);

    procedure GetMask_Generic(x, y: integer; out AValue: Boolean);

    // set color functions
    procedure SetColor_Generic(x, y: integer; const Value: TFPColor);
    procedure SetColor_RGBA_NoPalette(x, y: integer; const Value: TFPColor);
    procedure SetColor_RGB_NoPalette(x, y: integer; const Value: TFPColor);
    procedure SetColor_Gray_NoPalette(x, y: integer; const Value: TFPColor);
    procedure SetColor_GrayAlpha_NoPalette(x, y: integer; const Value: TFPColor);
    procedure SetColor_NULL(x, y: integer; const Value: TFPColor);
    // 32 bpp alpha
    procedure SetColor_BPP32_A8R8G8B8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_A8B8G8R8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_A8G8R8B8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_A8G8B8R8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_A8R8B8G8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_A8B8R8G8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_B8G8R8A8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_R8G8B8A8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_G8B8R8A8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_G8R8B8A8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_B8R8G8A8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_R8B8G8A8_BIO_TTB(x, y: integer; const Value: TFPColor);
    // 32 bpp no alpha
    procedure SetColor_BPP32_X8R8G8B8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_X8B8G8R8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_X8G8R8B8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_X8G8B8R8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_X8R8B8G8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_X8B8R8G8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_B8G8R8X8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_R8G8B8X8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_G8B8R8X8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_G8R8B8X8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_B8R8G8X8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP32_R8B8G8X8_BIO_TTB(x, y: integer; const Value: TFPColor);
    // 24 bpp
    procedure SetColor_BPP24_B8G8R8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP24_R8G8B8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP24_G8B8R8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP24_G8R8B8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP24_B8R8G8_BIO_TTB(x, y: integer; const Value: TFPColor);
    procedure SetColor_BPP24_R8B8G8_BIO_TTB(x, y: integer; const Value: TFPColor);

    procedure SetMask_Generic(x, y: integer; const AValue: Boolean);
  public
    constructor Create(AWidth, AHeight: integer); overload; override;
    constructor Create(AWidth, AHeight: integer; AFlags: TRawImageQueryFlags); overload;
    constructor Create(ARawImage: TRawImage; ADataOwner: Boolean); overload;
    constructor CreateCompatible(IntfImg: TLazIntfImage; AWidth, AHeight: integer);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure SetSize(AWidth, AHeight: integer); override;
    function CheckDescription(const ADescription: TRawImageDescription;
                              ExceptionOnError: boolean): boolean; virtual;
    procedure LoadFromDevice(DC: HDC); virtual;
    procedure LoadFromBitmap(ABitmap, AMaskBitmap: HBitmap; AWidth: integer = -1; AHeight: integer = -1); virtual;
    procedure CreateBitmaps(out ABitmap, AMask: HBitmap; ASkipMask: boolean = False); virtual;
    procedure SetRawImage(const ARawImage: TRawImage; ADataOwner: Boolean = True); virtual;
    procedure GetRawImage(out ARawImage: TRawImage; ATransferOwnership: Boolean = False); virtual;
    procedure FillPixels(const Color: TFPColor); virtual;
    procedure CopyPixels(ASource: TFPCustomImage; XDst: Integer = 0; YDst: Integer = 0;
                         AlphaMask: Boolean = False; AlphaTreshold: Word = 0); virtual;
    procedure AlphaBlend(ASource, ASourceAlpha: TLazIntfImage; const ADestX, ADestY: Integer);
    procedure AlphaFromMask(AKeepAlpha: Boolean = True);
    procedure Mask(const AColor: TFPColor; AKeepOldMask: Boolean = False);
    procedure GetXYDataPosition(x, y: integer; out Position: TRawImagePosition);
    procedure GetXYMaskPosition(x, y: integer; out Position: TRawImagePosition);
    function  GetDataLineStart(y: integer): Pointer;// similar to Delphi TBitmap.ScanLine. Only works with lines aligned to whole bytes.
    procedure CreateData; virtual;
    function  HasTransparency: boolean; virtual;
    function  HasMask: boolean; virtual;
    procedure SetDataDescriptionKeepData(const ADescription: TRawImageDescription);
  public
    property PixelData: PByte read FRawImage.Data;
    property MaskData: PByte read FRawImage.Mask;
    property DataDescription: TRawImageDescription read FRawImage.Description
                                                   write SetDataDescription;
    property TColors[x,y: integer]: TGraphicsColor read GetTColors write SetTColors;
    property Masked[x,y:integer]: Boolean read GetMasked write SetMasked;
  end; *)

implementation

end.
