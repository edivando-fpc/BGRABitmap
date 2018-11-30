{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    FPImage base definitions.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$i FPImage.inc}{$H+}
unit FPImage;


interface

uses sysutils, {$IFnDEF FPC}Windows, Messages, Graphics, Controls, types, {$ENDIF} strutils, classes;

type
  {$IFNDEF FPC}
    ValReal  = Extended;
    {$IFDEF CPU64}     //WORD = 2 bytes = 4 nybbles = 16 bits    for 32bits
    PtrInt   = Int64;
    PtrUInt  = QWord;  //QWORD = 2 DWORDs = 4 WORDs = ….. = 64 bits       for 32bits
    {$ELSE}            //DWORD = 2 WORDs = 4 bytes = 8 nybbles = 32 bits   for 32bits
    PtrInt   = LongInt;
    PtrUInt  = LongWord;
    QWord    = LongWord;
    {$ENDIF}
    PPtrInt  = ^PtrInt;
    PPtrUInt = ^PtrUInt;
    PQWord   = ^QWord;
    PDWord   = ^DWord;
    HDC      = windows.HDC;
    PUnicodeChar = windows.PWChar;
    UnicodeChar = windows.WCHAR;
  {$ENDIF}

  TFPCustomImageReader = class;
  TFPCustomImageReaderClass = class of TFPCustomImageReader;
  TFPCustomImageWriter = class;
  TFPCustomImageWriterClass = class of TFPCustomImageWriter;
  TIHData = class;
  TFPCustomImage = class;

  FPImageException = class (exception);

  TFPColor = record
    red,green,blue,alpha : word;

    class operator Equal(const c,d:TFPColor) : boolean;
    class operator NotEqual(const c,d:TFPColor) : boolean;
    class operator LogicalOr(const c,d:TFPColor) : TFPColor;
    class operator LogicalAnd(const c,d:TFPColor) : TFPColor;
    class operator LogicalXor(const c,d:TFPColor) : TFPColor;
  end;
  PFPColor = ^TFPColor;

  TColorFormat = (cfMono,cfGray2,cfGray4,cfGray8,cfGray16,cfGray24,
                  cfGrayA8,cfGrayA16,cfGrayA32,
                  cfRGB15,cfRGB16,cfRGB24,cfRGB32,cfRGB48,
                  cfRGBA8,cfRGBA16,cfRGBA32,cfRGBA64,
                  cfBGR15,cfBGR16,cfBGR24,cfBGR32,cfBGR48,
                  cfABGR8,cfABGR16,cfABGR32,cfABGR64);
  TColorData = qword;
  PColorData = ^TColorData;

  TDeviceColor = record
    Fmt : TColorFormat;
    Data : TColorData;
  end;

{$ifdef CPU68K}
  { 1.0 m68k cpu compiler does not allow
    types larger than 32k....
    if we remove range checking all should be fine PM }
  TFPColorArray = array [0..0] of TFPColor;
{$R-}
{$else not CPU68K}
  TFPColorArray = array [0..(maxint-1) div sizeof(TFPColor)-1] of TFPColor;
{$endif CPU68K}
  PFPColorArray = ^TFPColorArray;

  TFPImgProgressStage = (psStarting, psRunning, psEnding);
  TFPImgProgressEvent = procedure (Sender: TObject; Stage: TFPImgProgressStage;
                                   PercentDone: Byte; RedrawNow: Boolean; const R: TRect;
                                   const Msg: AnsiString; var Continue : Boolean) of object;
  // Delphi compatibility
  TProgressStage = TFPImgProgressStage;
  TProgressEvent = TFPImgProgressEvent;

  TFPPalette = class
    protected
      FData : PFPColorArray;
      FCount, FCapacity : integer;
      procedure SetCount (Value:integer); virtual;
      function GetCount : integer;
      procedure SetColor (index:integer; const Value:TFPColor); virtual;
      function GetColor (index:integer) : TFPColor;
      procedure SetCapacity (ind : Integer);
      procedure CheckIndex (index:integer); virtual;
      procedure EnlargeData; virtual;
    public
      constructor Create (ACount : integer);
      destructor Destroy; override;
      procedure Build (Img : TFPCustomImage); virtual;
      procedure Copy (APalette: TFPPalette); virtual;
      procedure Merge (pal : TFPPalette); virtual;
      function IndexOf (const AColor: TFPColor) : integer; virtual;
      function Add (const Value: TFPColor) : integer; virtual;
      procedure Clear; virtual;
      property Color [Index : integer] : TFPColor read GetColor write SetColor; default;
      property Count : integer read GetCount write SetCount;
      property Capacity : integer read FCapacity write SetCapacity;
  end;

  TFPCustomImage = class(TPersistent)
    private
      FOnProgress : TFPImgProgressEvent;
      FExtra : TStringlist;
      FPalette : TFPPalette;
      FHeight, FWidth : integer;
      procedure SetHeight (Value : integer);
      procedure SetWidth (Value : integer);
      procedure SetExtra (const key:String; const AValue:string);
      function GetExtra (const key:String) : string;
      procedure SetExtraValue (index:integer; const AValue:string);
      function GetExtraValue (index:integer) : string;
      procedure SetExtraKey (index:integer; const AValue:string);
      function GetExtraKey (index:integer) : string;
      procedure CheckIndex (x,y:integer);
      procedure CheckPaletteIndex (PalIndex:integer);
      procedure SetColor (x,y:integer; const Value:TFPColor);
      function GetColor (x,y:integer) : TFPColor;
      procedure SetPixel (x,y:integer; Value:integer);
      function GetPixel (x,y:integer) : integer;
      function GetUsePalette : boolean;
    protected
      // Procedures to store the data. Implemented in descendants
      procedure SetInternalColor (x,y:integer; const Value:TFPColor); virtual;
      function GetInternalColor (x,y:integer) : TFPColor; virtual;
      procedure SetInternalPixel (x,y:integer; Value:integer); virtual; abstract;
      function GetInternalPixel (x,y:integer) : integer; virtual; abstract;
      procedure SetUsePalette (Value:boolean);virtual;
      procedure Progress(Sender: TObject; Stage: TProgressStage;
                         PercentDone: Byte;  RedrawNow: Boolean; const R: TRect;
                         const Msg: AnsiString; var Continue: Boolean); Virtual;
    public
      constructor create (AWidth,AHeight:integer); virtual;
      destructor destroy; override;
      procedure Assign(Source: TPersistent); override;
      // Image handlers
      class function FindHandlerFromStream(Str:TStream): TIHData;
      class function FindReaderFromStream(Str:TStream): TFPCustomImageReaderClass;
      class function FindHandlerFromExtension(extension:String): TIHData;
      class function FindReaderFromFileName(const filename:String): TFPCustomImageReaderClass;
      class function FindReaderFromExtension(const extension:String): TFPCustomImageReaderClass;
      class function FindWriterFromFileName(const filename:String): TFPCustomImageWriterClass;
      class function FindWriterFromExtension(const extension:String): TFPCustomImageWriterClass;
      // Saving and loading
      procedure LoadFromStream (Str:TStream; Handler:TFPCustomImageReader); {$IFnDEF FPC}overload;{$ENDIF}
      procedure LoadFromStream (Str:TStream); {$IFnDEF FPC}overload;{$ENDIF}
      procedure LoadFromFile (const filename:String; Handler:TFPCustomImageReader); {$IFnDEF FPC}overload;{$ENDIF}
      function LoadFromFile (const filename:String): Boolean; {$IFnDEF FPC}overload;{$ENDIF}
      procedure SaveToStream (Str:TStream; Handler:TFPCustomImageWriter);
      procedure SaveToFile (const filename:String; Handler:TFPCustomImageWriter); {$IFnDEF FPC}overload;{$ENDIF}
      function SaveToFile (const filename:String): Boolean; {$IFnDEF FPC}overload;{$ENDIF}
      // Size and data
      procedure SetSize (AWidth, AHeight : integer); virtual;
      property  Height : integer read FHeight write SetHeight;
      property  Width : integer read FWidth write SetWidth;
      property  Colors [x,y:integer] : TFPColor read GetColor write SetColor; default;
      // Use of palette for colors
      property  UsePalette : boolean read GetUsePalette write SetUsePalette;
      property  Palette : TFPPalette read FPalette;
      property  Pixels [x,y:integer] : integer read GetPixel write SetPixel;
      // Info unrelated with the image representation
      property  Extra [const key:string] : string read GetExtra write SetExtra;
      property  ExtraValue [index:integer] : string read GetExtraValue write SetExtraValue;
      property  ExtraKey [index:integer] : string read GetExtraKey write SetExtraKey;
      procedure RemoveExtra (const key:string);
      function  ExtraCount : integer;
      property OnProgress: TFPImgProgressEvent read FOnProgress write FOnProgress;
  end;
  TFPCustomImageClass = class of TFPCustomImage;

{$ifdef CPU68K}
  { 1.0 m68k cpu compiler does not allow
    types larger than 32k....
    if we remove range checking all should be fine PM }
  TFPIntegerArray = array [0..0] of integer;
{$R-}
{$else not CPU68K}
  TFPIntegerArray = array [0..(maxint-1) div sizeof(integer)-1] of integer;
{$endif CPU68K}
  PFPIntegerArray = ^TFPIntegerArray;

  TFPMemoryImage = class (TFPCustomImage)
    protected
      function GetInternalColor(x,y:integer):TFPColor;override;
      procedure SetInternalColor (x,y:integer; const Value:TFPColor);override;
      procedure SetUsePalette (Value:boolean);override;
    protected
      FData : PFPIntegerArray;
      procedure SetInternalPixel (x,y:integer; Value:integer); override;
      function GetInternalPixel (x,y:integer) : integer; override;
    public
      constructor create (AWidth,AHeight:integer); override;
      destructor destroy; override;
      procedure SetSize (AWidth, AHeight : integer); override;
  end;

  TFPCustomImageHandler = class
    private
      FOnProgress : TFPImgProgressEvent;
      FStream : TStream;
      FImage : TFPCustomImage;
    protected
      procedure Progress(Stage: TProgressStage; PercentDone: Byte;  RedrawNow: Boolean; const R: TRect;
                         const Msg: AnsiString; var Continue: Boolean); Virtual;
      property TheStream : TStream read FStream;
      property TheImage : TFPCustomImage read FImage;
    public
      constructor Create; virtual;
      Property OnProgress : TFPImgProgressEvent Read FOnProgress Write FOnProgress;
  end;

  TFPCustomImageReader = class (TFPCustomImageHandler)
    private
      FDefImageClass:TFPCustomImageClass;
    protected
      procedure InternalRead  (Str:TStream; Img:TFPCustomImage); virtual; abstract;
      function  InternalCheck (Str:TStream) : boolean; virtual; abstract;
      class function InternalSize  (Str:TStream): TPoint; virtual;
    public
      constructor Create; override;
      function ImageRead (Str:TStream; Img:TFPCustomImage) : TFPCustomImage;
      // reads image
      function CheckContents (Str:TStream) : boolean;
      // Returns true if the content is readable
      class function ImageSize(Str:TStream): TPoint;
      // returns the size of image in stream without loading it completely. -1,-1 means this is not implemented.
      property DefaultImageClass : TFPCustomImageClass read FDefImageClass write FDefImageClass;
      // Image Class to create when no img is given for reading
  end;

  TFPCustomImageWriter = class (TFPCustomImageHandler)
    protected
      procedure InternalWrite (Str:TStream; Img:TFPCustomImage); virtual; abstract;
    public
      procedure ImageWrite (Str:TStream; Img:TFPCustomImage);
      // writes given image to stream
  end;

  TIHData = class
    private
      FExtension, FTypeName, FDefaultExt : string;
      FReader : TFPCustomImageReaderClass;
      FWriter : TFPCustomImageWriterClass;
    public
      property Extension: string read FExtension;
      property TypeName: string read FTypeName;
      property DefaultExt: string read FDefaultExt;
      property Reader : TFPCustomImageReaderClass read FReader;
      property Writer : TFPCustomImageWriterClass read FWriter;
  end;

  TImageHandlersManager = class
    private
      FData : TList;
      function GetReader (const TypeName:string) : TFPCustomImageReaderClass;
      function GetWriter (const TypeName:string) : TFPCustomImageWriterClass;
      function GetExt (const TypeName:string) : string;
      function GetDefExt (const TypeName:string) : string;
      function GetTypeName (index:integer) : string;
      function GetData (const ATypeName:string) : TIHData; {$IFnDEF FPC}overload;{$ENDIF}
      function GetData (index : integer) : TIHData; {$IFnDEF FPC}overload;{$ENDIF}
      function GetCount : integer;
    public
      constructor Create;
      destructor Destroy; override;
      procedure RegisterImageHandlers (const ATypeName,TheExtensions:string;
                   AReader:TFPCustomImageReaderClass; AWriter:TFPCustomImageWriterClass);
      procedure RegisterImageReader (const ATypeName,TheExtensions:string;
                   AReader:TFPCustomImageReaderClass);
      procedure RegisterImageWriter (const ATypeName,TheExtensions:string;
                   AWriter:TFPCustomImageWriterClass);
      procedure UnregisterImageHandlers(const ATypeName: string; ARemoveReader: boolean = True; ARemoveWriter: boolean = True);
      property Count : integer read GetCount;
      property ImageReader [const TypeName:string] : TFPCustomImageReaderClass read GetReader;
      property ImageWriter [const TypeName:string] : TFPCustomImageWriterClass read GetWriter;
      property Extensions [const TypeName:string] : string read GetExt;
      property DefaultExtension [const TypeName:string] : string read GetDefExt;
      property TypeNames [index:integer] : string read GetTypeName;
    end;

{function ShiftAndFill (initial:word; CorrectBits:byte):word;
function FillOtherBits (initial:word;CorrectBits:byte):word;
}
function CalculateGray (const From : TFPColor) : word;
(*
function ConvertColor (const From : TDeviceColor) : TFPColor;
function ConvertColor (const From : TColorData; FromFmt:TColorFormat) : TFPColor;
function ConvertColorToData (const From : TFPColor; Fmt : TColorFormat) : TColorData;
function ConvertColorToData (const From : TDeviceColor; Fmt : TColorFormat) : TColorData;
function ConvertColor (const From : TFPColor; Fmt : TColorFormat) : TDeviceColor;
function ConvertColor (const From : TDeviceColor; Fmt : TColorFormat) : TDeviceColor;
*)

function AlphaBlend(color1, color2: TFPColor): TFPColor;

function FPColor (r,g,b,a:word) : TFPColor; {$IFnDEF FPC}overload;{$ENDIF}
function FPColor (r,g,b:word) : TFPColor; {$IFnDEF FPC}overload;{$ENDIF}
{$ifdef debug}function MakeHex (n:TColordata;nr:byte): string;{$endif}

function CompareColors(const Color1, Color2: TFPColor): integer;

var ImageHandlers : TImageHandlersManager;

type
  TErrorTextIndices = (
    StrInvalidIndex,
    StrNoImageToWrite,
    StrNoFile,
    StrNoStream,
    StrPalette,
    StrImageX,
    StrImageY,
    StrImageExtra,
    StrTypeAlreadyExist,
    StrTypeReaderAlreadyExist,
    StrTypeWriterAlreadyExist,
    StrCantDetermineType,
    StrNoCorrectReaderFound,
    StrReadWithError,
    StrWriteWithError,
    StrNoPaletteAvailable,
    StrInvalidHTMLColor
    );

const
  // MG: ToDo: move to implementation and add a function to map to resourcestrings
  ErrorText : array[TErrorTextIndices] of string =
    ('Invalid %s index %d',
     'No image to write',
     'File "%s" does not exist',
     'No stream to write to',
     'palette',
     'horizontal pixel',
     'vertical pixel',
     'extra',
     'Image type "%s" already exists',
     'Image type "%s" already has a reader class',
     'Image type "%s" already has a writer class',
     'Error while determining image type of stream: %s',
     'Can''t determine image type of stream',
     'Error while reading stream: %s',
     'Error while writing stream: %s',
     'No palette available',
     'Invalid HTML color : %s'
     );

{.$i fpcolors.inc}
const
  BytesNeeded : array[TColorFormat] of byte =
      (1,1,1,1,2,3,1,2,4,2,2,3,4,6,1,2,4,8,2,2,3,4,6,1,2,4,8);

  alphaTransparent = $0000;
  alphaOpaque      = $FFFF;
  colTransparent: TFPColor = (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaTransparent);
  colBlack      : TFPColor = (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque);
  colBlue       : TFPColor = (Red: $0000; Green: $0000; Blue: $ffff; Alpha: alphaOpaque);
  colGreen      : TFPColor = (Red: $0000; Green: $ffff; Blue: $0000; Alpha: alphaOpaque);
  colCyan       : TFPColor = (Red: $0000; Green: $ffff; Blue: $ffff; Alpha: alphaOpaque);
  colRed        : TFPColor = (Red: $ffff; Green: $0000; Blue: $0000; Alpha: alphaOpaque);
  colMagenta    : TFPColor = (Red: $ffff; Green: $0000; Blue: $ffff; Alpha: alphaOpaque);
  colYellow     : TFPColor = (Red: $ffff; Green: $ffff; Blue: $0000; Alpha: alphaOpaque);
  colWhite      : TFPColor = (Red: $ffff; Green: $ffff; Blue: $ffff; Alpha: alphaOpaque);
  colGray       : TFPColor = (Red: $8000; Green: $8000; Blue: $8000; Alpha: alphaOpaque);
  colLtGray     : TFPColor = (Red: $c000; Green: $c000; Blue: $c000; Alpha: alphaOpaque);
  colDkGray     : TFPColor = (Red: $4000; Green: $4000; Blue: $4000; Alpha: alphaOpaque);
  colDkBlue     : TFPColor = (Red: $0000; Green: $0000; Blue: $8000; Alpha: alphaOpaque);
  colDkGreen    : TFPColor = (Red: $0000; Green: $8000; Blue: $0000; Alpha: alphaOpaque);
  colDkCyan     : TFPColor = (Red: $0000; Green: $8000; Blue: $8000; Alpha: alphaOpaque);
  colDkRed      : TFPColor = (Red: $8000; Green: $0000; Blue: $0000; Alpha: alphaOpaque);
  colDkMagenta  : TFPColor = (Red: $8000; Green: $0000; Blue: $8000; Alpha: alphaOpaque);
  colDkYellow   : TFPColor = (Red: $8000; Green: $8000; Blue: $0000; Alpha: alphaOpaque);
  colMaroon     : TFPColor = (Red: $8000; Green: $0000; Blue: $0000; Alpha: alphaOpaque);
  colLtGreen    : TFPColor = (Red: $0000; Green: $8000; Blue: $0000; Alpha: alphaOpaque);
  colOlive      : TFPColor = (Red: $8000; Green: $8000; Blue: $0000; Alpha: alphaOpaque);
  colNavy       : TFPColor = (Red: $0000; Green: $0000; Blue: $8000; Alpha: alphaOpaque);
  colPurple     : TFPColor = (Red: $8000; Green: $0000; Blue: $8000; Alpha: alphaOpaque);
  colTeal       : TFPColor = (Red: $0000; Green: $8000; Blue: $8000; Alpha: alphaOpaque);
  colSilver     : TFPColor = (Red: $c000; Green: $c000; Blue: $c000; Alpha: alphaOpaque);
  colLime       : TFPColor = (Red: $0000; Green: $ffff; Blue: $0000; Alpha: alphaOpaque);
  colFuchsia    : TFPColor = (Red: $ffff; Green: $0000; Blue: $ffff; Alpha: alphaOpaque);
  colAqua       : TFPColor = (Red: $0000; Green: $ffff; Blue: $ffff; Alpha: alphaOpaque);


type
  TGrayConvMatrix = record
    red, green, blue : single;
  end;

var
  GrayConvMatrix : TGrayConvMatrix;

const
  GCM_NTSC : TGrayConvMatrix = (red:0.299; green:0.587; blue:0.114);
  GCM_JPEG : TGrayConvMatrix = (red:0.299; green:0.587; blue:0.114);
  GCM_Mathematical : TGrayConvMatrix = (red:0.334; green:0.333; blue:0.333);
  GCM_Photoshop : TGrayConvMatrix = (red:0.213; green:0.715; blue:0.072);

function CreateBlackAndWhitePalette : TFPPalette;
function CreateWebSafePalette : TFPPalette;
function CreateGrayScalePalette : TFPPalette;
function CreateVGAPalette : TFPPalette;

Type
  TFPCompactImgDesc = record
    Gray: boolean; // true = red=green=blue, false: a RGB image
    Depth: word; // 8 or 16 bit
    HasAlpha: boolean; // has alpha channel
  end;

  { TFPCompactImgBase }

  TFPCompactImgBase = class(TFPCustomImage)
  private
    FDesc: TFPCompactImgDesc;
  public
    property Desc: TFPCompactImgDesc read FDesc;
  end;
  TFPCompactImgBaseClass = class of TFPCompactImgBase;

  { TFPCompactImgGray16Bit }

  TFPCompactImgGray16Bit = class(TFPCompactImgBase)
  protected
    FData: PWord;
    function GetInternalColor(x, y: integer): TFPColor; override;
    function GetInternalPixel({%H-}x, {%H-}y: integer): integer; override;
    procedure SetInternalColor (x, y: integer; const Value: TFPColor); override;
    procedure SetInternalPixel({%H-}x, {%H-}y: integer; {%H-}Value: integer); override;
  public
    constructor Create(AWidth, AHeight: integer); override;
    destructor Destroy; override;
    procedure SetSize(AWidth, AHeight: integer); override;
  end;

  TFPCompactImgGrayAlpha16BitValue = packed record
    g,a: word;
  end;
  PFPCompactImgGrayAlpha16BitValue = ^TFPCompactImgGrayAlpha16BitValue;

  { TFPCompactImgGrayAlpha16Bit }

  TFPCompactImgGrayAlpha16Bit = class(TFPCompactImgBase)
  protected
    FData: PFPCompactImgGrayAlpha16BitValue;
    function GetInternalColor(x, y: integer): TFPColor; override;
    function GetInternalPixel({%H-}x, {%H-}y: integer): integer; override;
    procedure SetInternalColor (x, y: integer; const Value: TFPColor); override;
    procedure SetInternalPixel({%H-}x, {%H-}y: integer; {%H-}Value: integer); override;
  public
    constructor Create(AWidth, AHeight: integer); override;
    destructor Destroy; override;
    procedure SetSize(AWidth, AHeight: integer); override;
  end;

  { TFPCompactImgGray8Bit }

  TFPCompactImgGray8Bit = class(TFPCompactImgBase)
  protected
    FData: PByte;
    function GetInternalColor(x, y: integer): TFPColor; override;
    function GetInternalPixel({%H-}x, {%H-}y: integer): integer; override;
    procedure SetInternalColor (x, y: integer; const Value: TFPColor); override;
    procedure SetInternalPixel({%H-}x, {%H-}y: integer; {%H-}Value: integer); override;
  public
    constructor Create(AWidth, AHeight: integer); override;
    destructor Destroy; override;
    procedure SetSize(AWidth, AHeight: integer); override;
  end;

  TFPCompactImgGrayAlpha8BitValue = packed record
    g,a: byte;
  end;
  PFPCompactImgGrayAlpha8BitValue = ^TFPCompactImgGrayAlpha8BitValue;

  { TFPCompactImgGrayAlpha8Bit }

  TFPCompactImgGrayAlpha8Bit = class(TFPCompactImgBase)
  protected
    FData: PFPCompactImgGrayAlpha8BitValue;
    function GetInternalColor(x, y: integer): TFPColor; override;
    function GetInternalPixel({%H-}x, {%H-}y: integer): integer; override;
    procedure SetInternalColor (x, y: integer; const Value: TFPColor); override;
    procedure SetInternalPixel({%H-}x, {%H-}y: integer; {%H-}Value: integer); override;
  public
    constructor Create(AWidth, AHeight: integer); override;
    destructor Destroy; override;
    procedure SetSize(AWidth, AHeight: integer); override;
  end;

  TFPCompactImgRGBA8BitValue = packed record
    r,g,b,a: byte;
  end;
  PFPCompactImgRGBA8BitValue = ^TFPCompactImgRGBA8BitValue;

  { TFPCompactImgRGBA8Bit }

  TFPCompactImgRGBA8Bit = class(TFPCompactImgBase)
  protected
    FData: PFPCompactImgRGBA8BitValue;
    function GetInternalColor(x, y: integer): TFPColor; override;
    function GetInternalPixel({%H-}x, {%H-}y: integer): integer; override;
    procedure SetInternalColor (x, y: integer; const Value: TFPColor); override;
    procedure SetInternalPixel({%H-}x, {%H-}y: integer; {%H-}Value: integer); override;
  public
    constructor Create(AWidth, AHeight: integer); override;
    destructor Destroy; override;
    procedure SetSize(AWidth, AHeight: integer); override;
  end;

  TFPCompactImgRGB8BitValue = packed record
    r,g,b: byte;
  end;
  PFPCompactImgRGB8BitValue = ^TFPCompactImgRGB8BitValue;

  { TFPCompactImgRGB8Bit }

  TFPCompactImgRGB8Bit = class(TFPCompactImgBase)
  protected
    FData: PFPCompactImgRGB8BitValue;
    function GetInternalColor(x, y: integer): TFPColor; override;
    function GetInternalPixel({%H-}x, {%H-}y: integer): integer; override;
    procedure SetInternalColor (x, y: integer; const Value: TFPColor); override;
    procedure SetInternalPixel({%H-}x, {%H-}y: integer; {%H-}Value: integer); override;
  public
    constructor Create(AWidth, AHeight: integer); override;
    destructor Destroy; override;
    procedure SetSize(AWidth, AHeight: integer); override;
  end;

  TFPCompactImgRGB16BitValue = packed record
    r,g,b: word;
  end;
  PFPCompactImgRGB16BitValue = ^TFPCompactImgRGB16BitValue;

  { TFPCompactImgRGB16Bit }

  TFPCompactImgRGB16Bit = class(TFPCompactImgBase)
  protected
    FData: PFPCompactImgRGB16BitValue;
    function GetInternalColor(x, y: integer): TFPColor; override;
    function GetInternalPixel({%H-}x, {%H-}y: integer): integer; override;
    procedure SetInternalColor (x, y: integer; const Value: TFPColor); override;
    procedure SetInternalPixel({%H-}x, {%H-}y: integer; {%H-}Value: integer); override;
  public
    constructor Create(AWidth, AHeight: integer); override;
    destructor Destroy; override;
    procedure SetSize(AWidth, AHeight: integer); override;
  end;

  { TFPCompactImgRGBA16Bit }

  TFPCompactImgRGBA16Bit = class(TFPCompactImgBase)
  protected
    FData: PFPColor;
    function GetInternalColor(x, y: integer): TFPColor; override;
    function GetInternalPixel({%H-}x, {%H-}y: integer): integer; override;
    procedure SetInternalColor (x, y: integer; const Value: TFPColor); override;
    procedure SetInternalPixel({%H-}x, {%H-}y: integer; {%H-}Value: integer); override;
  public
    constructor Create(AWidth, AHeight: integer); override;
    destructor Destroy; override;
    procedure SetSize(AWidth, AHeight: integer); override;
  end;

{ Create a descriptor to select a CompactImg class }
function GetFPCompactImgDesc(Gray: boolean; Depth: word; HasAlpha: boolean): TFPCompactImgDesc;

{ Returns a CompactImg class that fits the descriptor }
function GetFPCompactImgClass(const Desc: TFPCompactImgDesc): TFPCompactImgBaseClass;

{ Create a CompactImg with the descriptor }
function CreateFPCompactImg(const Desc: TFPCompactImgDesc; Width, Height: integer): TFPCustomImage;

{ Create a CompactImg with the same features as Img.
If Img is a TFPCompactImgBaseClass it will create that.
Otherwise it returns a CompactImg that fits the Img using GetMinimumPTDesc. }
function CreateCompatibleFPCompactImg(Img: TFPCustomImage; Width, Height: integer
): TFPCustomImage;

{ As CreateCompatibleFPCompactImg, but the image has always an alpha channel. }
function CreateCompatibleFPCompactImgWithAlpha(Img: TFPCustomImage;
Width, Height: integer): TFPCustomImage;

{ Returns the smallest descriptor that allows to store the Img.
It returns HasAlpha=false if all pixel are opaque.
It returns Gray=true if all red=green=blue.
It returns Depth=8 if all lo byte equals the hi byte or all lo bytes are 0.
To ignore rounding errors you can pass a FuzzyDepth. For example a FuzzyDepth
of 3 ignores the lower 3 bits when comparing.  }
function GetMinimumPTDesc(Img: TFPCustomImage; FuzzyDepth: word = 4): TFPCompactImgDesc;

{ Create a smaller CompactImg with the same information as Img.
Pass FreeImg=true to call Img.Free }
function GetMinimumFPCompactImg(Img: TFPCustomImage; FreeImg: boolean;
FuzzyDepth: word = 4): TFPCustomImage;

{ HTML Color support. RRGGBB or color name. Only W3 color names s are supported}

function TryHtmlToFPColor(const S: String; out FPColor: TFPColor): Boolean;
function HtmlToFPColorDef(const S: String; out FPColor: TFpColor; Def: TFPColor): TFPColor;
function HtmlToFpColor(const S: String): TFPColor;


implementation

procedure FPImgError (Fmt:TErrorTextIndices; data : array of const); overload;
begin
  raise FPImageException.CreateFmt (ErrorText[Fmt],data);
end;

procedure FPImgError (Fmt:TErrorTextIndices); overload;
begin
  raise FPImageException.Create (ErrorText[Fmt]);
end;

{.$i FPImage.inc}
{ TFPCustomImage }

constructor TFPCustomImage.create (AWidth,AHeight:integer);
begin
  inherited create;
  FExtra := TStringList.Create;
  FWidth := 0;
  FHeight := 0;
  FPalette := nil;
  SetSize (AWidth,AHeight);
end;

destructor TFPCustomImage.destroy;
begin
  FExtra.Free;
  if assigned (FPalette) then
    FPalette.Free;
  inherited;
end;

procedure TFPCustomImage.LoadFromStream (Str:TStream; Handler:TFPCustomImagereader);
begin
  Handler.ImageRead (Str, self);
end;

procedure TFPCustomImage.LoadFromFile (const filename:String; Handler:TFPCustomImageReader);
var
  fs : TStream;
begin
  if FileExists (filename) then
    begin
    fs := TFileStream.Create (filename, fmOpenRead);
    try
      LoadFromStream (fs, handler);
    finally
      fs.Free;
    end;
    end
  else
    FPImgError (StrNoFile, [filename]);
end;

procedure TFPCustomImage.SaveToStream (Str:TStream; Handler:TFPCustomImageWriter);
begin
  Handler.ImageWrite (Str, Self);
end;

procedure TFPCustomImage.SaveToFile (const filename:String; Handler:TFPCustomImageWriter);
var
  fs : TStream;
begin
  fs := TFileStream.Create (filename, fmCreate);
  try
    SaveToStream (fs, handler);
  finally
    fs.Free;
  end
end;

function TFPCustomImage.SaveToFile (const filename:String):boolean;

var h : TFPCustomImageWriterClass;
    Writer : TFPCustomImageWriter;
    Msg : string;

begin
  Msg := '';
  try
    h := FindWriterFromFileName(filename);
    Result := assigned (h);
    if Result then
      begin
      Writer := h.Create;
      try
        SaveTofile (filename, Writer);
      finally
        Writer.Free;
      end;
      end;
  except
    on e : exception do
      Msg := e.message;
  end;
  if (Msg<>'') then
    FPImgError (StrWriteWithError, [Msg]);
end;


procedure TFPCustomImage.LoadFromStream (Str:TStream);
var r : integer;
    h : TFPCustomImageReaderClass;
    reader : TFPCustomImageReader;
    msg : string;
    d : TIHData;
    startPos: Int64;
begin
  msg := '';
  startPos := str.Position;
  with ImageHandlers do
    try
      r := count-1;
      while (r >= 0) do
        begin
        d := GetData(r);
        if assigned (d) then
          h := d.FReader
        else
          h := nil;
        if assigned (h) then
          begin
          reader := h.Create;
          with reader do
            try
              if CheckContents (str) then
                try
                  FStream := str;
                  FImage := self;
                  InternalRead (str, self);
                  msg := '';
                  break;
                except
                  on e : exception do
                    msg := e.message;
                end;
            finally
              Free;
              str.Position := startPos;
            end;
          end;
        dec (r);
        end;
    except
      on e : exception do
        FPImgError (StrCantDetermineType, [e.message]);
    end;
  if r < 0 then
    if msg = '' then
      FPImgError (StrNoCorrectReaderFound)
    else
      FPImgError (StrReadWithError, [Msg]);
end;

function TFPCustomImage.LoadFromFile (const filename:String):boolean;
var f : TFileStream;
    h : TFPCustomImageReaderClass;
    reader : TFPCustomImageReader;
    Msg : string;
begin
  Msg := '';
  try
    h := FindReaderFromFileName(filename);
    Result := assigned (h);
    if Result then
      begin
      reader := h.Create;
      try
        loadfromfile (filename, reader);
      finally
        Reader.Free;
      end;
      end;
  except
    on e : exception do
      Msg := e.message;
  end;
  if Msg = '' then
    begin
    if h = nil then
      begin
      f := TFileStream.Create (filename, fmOpenRead);
      try
        LoadFromStream (f);
      finally
        f.Free;
      end;
      end;
    end
  else
    FPImgError (StrReadWithError, [Msg]);
end;

procedure TFPCustomImage.SetHeight (Value : integer);
begin
  if Value <> FHeight then
    SetSize (FWidth, Value);
end;

procedure TFPCustomImage.SetWidth (Value : integer);
begin
  if Value <> FWidth then
    SetSize (Value, FHeight);
end;

procedure TFPCustomImage.SetSize (AWidth, AHeight : integer);
begin
  FWidth := AWidth;
  FHeight := AHeight;
end;

procedure TFPCustomImage.SetExtraValue (index:integer; const AValue:string);
var s : string;
    p : integer;
begin
  s := FExtra[index];
  p := pos ('=', s);
  if p > 0 then
    FExtra[index] := copy(s, 1, p) + AValue
  else
    FPImgError (StrInvalidIndex,[ErrorText[StrImageExtra],index]);
end;

function TFPCustomImage.GetExtraValue (index:integer) : string;
var s : string;
    p : integer;
begin
  s := FExtra[index];
  p := pos ('=', s);
  if p > 0 then
    result := copy(s, p+1, maxint)
  else
    result := '';
end;

procedure TFPCustomImage.SetExtraKey (index:integer; const AValue:string);
var s : string;
    p : integer;
begin
  s := FExtra[index];
  p := pos('=',s);
  if p > 0 then
    s := AValue + copy(s,p,maxint)
  else
    s := AValue;
  FExtra[index] := s;
end;

function TFPCustomImage.GetExtraKey (index:integer) : string;
begin
  result := FExtra.Names[index];
end;

procedure TFPCustomImage.SetExtra (const key:String; const AValue:string);
begin
  FExtra.values[key] := AValue;
end;

function TFPCustomImage.GetExtra (const key:String) : string;
begin
  result := FExtra.values[key];
end;

function  TFPCustomImage.ExtraCount : integer;
begin
  result := FExtra.count;
end;

const dumchar = ';';
class function TFPCustomImage.FindHandlerFromExtension(extension: String
  ): TIHData;
var s : string;
    r : integer;
begin
  if extension='' then
    Exit(nil);
  extension := lowercase (extension);
  if (extension <> '') and (extension[1] = '.') then
    delete (extension,1,1);
  with ImageHandlers do
    begin
      r := count-1;
      s := dumchar  + extension + dumchar;
      while (r >= 0) do
        begin
        Result := GetData(r);
        if (pos(s, dumchar+Result.Fextension+dumchar) <> 0) then
          Exit;
        dec (r);
        end;
    end;
  Result := nil;
end;

class function TFPCustomImage.FindHandlerFromStream(Str: TStream): TIHData;
var r : integer;
    p: Int64;
    reader: TFPCustomImageReader;
begin
  r := ImageHandlers.Count-1;
  p := Str.Position;
  while (r >= 0) do
    begin
    Result := ImageHandlers.GetData(r);
    if Result.Reader<>nil then
      begin
      reader := Result.Reader.Create;
      try
        if reader.CheckContents(Str) then
          Exit;
      finally
        reader.free;
        Str.Position := p;
      end;
      end;
    dec (r);
    end;
  Result := nil;
end;

class function TFPCustomImage.FindReaderFromExtension(const extension: String
  ): TFPCustomImageReaderClass;
var d : TIHData;
begin
  d := FindHandlerFromExtension(extension);
  if d<>nil then
    Result := d.FReader
  else
    Result := nil;
end;

class function TFPCustomImage.FindReaderFromFileName(const filename: String
  ): TFPCustomImageReaderClass;
begin
  Result := FindReaderFromExtension(ExtractFileExt(filename));
end;

class function TFPCustomImage.FindReaderFromStream(
  Str: TStream): TFPCustomImageReaderClass;
var d : TIHData;
begin
  d := FindHandlerFromStream(Str);
  if d<>nil then
    Result := d.FReader
  else
    Result := nil;
end;

class function TFPCustomImage.FindWriterFromExtension(const extension: String
  ): TFPCustomImageWriterClass;
var d : TIHData;
begin
  d := FindHandlerFromExtension(extension);
  if d<>nil then
    Result := d.FWriter
  else
    Result := nil;
end;

class function TFPCustomImage.FindWriterFromFileName(const filename: String
  ): TFPCustomImageWriterClass;
begin
  Result := FindWriterFromExtension(ExtractFileExt(filename));
end;

procedure TFPCustomImage.RemoveExtra (const key:string);
var p : integer;
begin
  p := FExtra.IndexOfName(key);
  if p >= 0 then
    FExtra.Delete (p);
end;

procedure TFPCustomImage.SetPixel (x,y:integer; Value:integer);
begin
  CheckPaletteIndex (Value);
  CheckIndex (x,y);
  SetInternalPixel (x,y,Value);
end;

function TFPCustomImage.GetPixel (x,y:integer) : integer;
begin
  CheckIndex (x,y);
  result := GetInternalPixel(x,y);
end;

procedure TFPCustomImage.SetColor (x,y:integer; const Value:TFPColor);
begin
  CheckIndex (x,y);
  SetInternalColor (x,y,Value);
end;

function TFPCustomImage.GetColor (x,y:integer) : TFPColor;
begin
  CheckIndex (x,y);
  result := GetInternalColor(x,y);
end;

procedure TFPCustomImage.SetInternalColor (x,y:integer; const Value:TFPColor);
var i : integer;
begin
  i := FPalette.IndexOf (Value);
  SetInternalPixel (x,y,i);
end;

function TFPCustomImage.GetInternalColor (x,y:integer) : TFPColor;
begin
  result := FPalette.Color[GetInternalPixel(x,y)];
end;

function TFPCustomImage.GetUsePalette : boolean;
begin
  result := assigned(FPalette);
end;

procedure TFPCustomImage.SetUsePalette(Value:boolean);
begin
  if Value <> assigned(FPalette)
  then
    if Value
    then
      begin
        FPalette := TFPPalette.Create (0);
        // FPalette.Add (colTransparent);
      end
    else
      begin
        FPalette.Free;
        FPalette := nil;
      end;
end;

procedure TFPCustomImage.CheckPaletteIndex (PalIndex:integer);
begin
  if UsePalette then
    begin
    if (PalIndex < -1) or (PalIndex >= FPalette.Count) then
      FPImgError (StrInvalidIndex,[ErrorText[StrPalette],PalIndex]);
    end
  else
    FPImgError (StrNoPaletteAvailable);
end;

procedure TFPCustomImage.CheckIndex (x,y:integer);
begin
  if (x < 0) or (x >= FWidth) then
    FPImgError (StrInvalidIndex,[ErrorText[StrImageX],x]);
  if (y < 0) or (y >= FHeight) then
    FPImgError (StrInvalidIndex,[ErrorText[StrImageY],y]);
end;

Procedure TFPCustomImage.Progress(Sender: TObject; Stage: TProgressStage;
                         PercentDone: Byte;  RedrawNow: Boolean; const R: TRect;
                         const Msg: AnsiString; var Continue: Boolean);
begin
  If Assigned(FOnProgress) then
    FonProgress(Sender,Stage,PercentDone,RedrawNow,R,Msg,Continue);
end;

Procedure TFPCustomImage.Assign(Source: TPersistent);

Var
  Src : TFPCustomImage;
  X,Y : Integer;

begin
  If Source is TFPCustomImage then
    begin
    Src:=TFPCustomImage(Source);
    // Copy extra info
    FExtra.Assign(Src.Fextra);
    // Copy palette if needed.
    SetSize(0,0); { avoid side-effects in descendant classes }
    UsePalette:=Src.UsePalette;
    If UsePalette then
      begin
      Palette.Count:=0;
      Palette.Merge(Src.Palette);
      end;
    // Copy image.
    SetSize(Src.Width,Src.height);
    If UsePalette then
      For x:=0 to Src.Width-1 do
        For y:=0 to src.Height-1 do
          pixels[X,Y]:=src.pixels[X,Y]
    else
      For x:=0 to Src.Width-1 do
        For y:=0 to src.Height-1 do
          self[X,Y]:=src[X,Y];
    end
  else
    Inherited Assign(Source);
end;

{ TFPMemoryImage }

constructor TFPMemoryImage.Create (AWidth,AHeight:integer);
begin
  Fdata := nil;
  inherited create (AWidth,AHeight);
  SetUsePalette(False);
end;

destructor TFPMemoryImage.Destroy;
begin
  // MG: missing if
  if FData<>nil then
    FreeMem (FData);
  inherited Destroy;
end;

function TFPMemoryImage.GetInternalColor(x,y:integer):TFPColor;
  begin
    if Assigned(FPalette)
    then
      Result:=inherited GetInternalColor(x,y)
    else
      Result:=PFPColorArray(FData)^[y*FWidth+x];
  end;

function TFPMemoryImage.GetInternalPixel (x,y:integer) : integer;
begin
  result := FData^[y*FWidth+x];
end;

procedure TFPMemoryImage.SetInternalColor (x,y:integer; const Value:TFPColor);
  begin
    if Assigned(FPalette)
    then
      inherited SetInternalColor(x,y,Value)
    else
      PFPColorArray(FData)^[y*FWidth+x]:=Value;
  end;

procedure TFPMemoryImage.SetInternalPixel (x,y:integer; Value:integer);
begin
  FData^[y*FWidth+x] := Value;
end;

function Lowest (a,b : integer) : integer;
begin
  if a <= b then
    result := a
  else
    result := b;
end;

procedure TFPMemoryImage.SetSize (AWidth, AHeight : integer);
var w, h, r, old : integer;
    NewData : PFPIntegerArray;
begin
  if (AWidth <> Width) or (AHeight <> Height) then
    begin
    old := Height * Width;
    r:=AWidth*AHeight;
    if Assigned(FPalette)
    then
      r:=SizeOf(integer)*r
    else
      r:=SizeOf(TFPColor)*r;
    if r = 0 then
      NewData := nil
    else
      begin
      GetMem (NewData, r);
      FillChar (Newdata^[0], r div sizeof(word), 0);
      end;
    // MG: missing "and (NewData<>nil)"
    if (old <> 0) and assigned(FData) and (NewData<>nil) then
      begin
      if r <> 0 then
        begin
        w := Lowest(Width, AWidth);
        h := Lowest(Height, AHeight);
        for r := 0 to h-1 do
          move (FData^[r*Width], NewData^[r*AWidth], w);
        end;
      end;
    if Assigned(FData) then FreeMem(FData);
    FData := NewData;
    inherited;
    end;
end;

procedure TFPMemoryImage.SetUsePalette(Value:boolean);
var
  OldColors:PFPColorArray;
  OldPixels:PFPIntegerArray;
  r,c:Integer;
begin
  if Value<>assigned(FPalette)
  then
    if Value
    then
      begin
        FPalette:=TFPPalette.Create(0);
        //FPalette.Add(colTransparent);
        if assigned(FData) then
          begin
          OldColors:=PFPColorArray(FData);
          GetMem(FData,FWidth*FHeight*SizeOf(Integer));
          for r:=0 to FHeight-1 do
            for c:=0 to FWidth-1 do
              Colors[c,r]:=OldColors^[r*FWidth+c];
          FreeMem(OldColors);
          end;
      end
    else
      begin
        if Assigned(FData) then
          begin
          OldPixels:=PFPIntegerArray(FData);
          GetMem(FData,FWidth*FHeight*SizeOf(TFPColor));
          for r:=0 to FHeight-1 do
            for c:=0 to FWidth-1 do
              Colors[c,r]:=FPalette.Color[OldPixels^[r*FWidth+c]];
          FreeMem(OldPixels);
          end;
        FPalette.Free;
        FPalette:=nil;
      end;
end;

{.$i FPHandler.inc}
{ TImageHandlersManager }

constructor TImageHandlersManager.Create;
begin
  inherited create;
  FData := Tlist.Create;
end;

destructor TImageHandlersManager.Destroy;
var r : integer;
begin
  for r := FData.count-1 downto 0 do
    TIHData(FData[r]).Free;
  FData.Free;
  inherited Destroy;
end;

function CalcDefExt (TheExtensions:string) : string;
var p : integer;
begin
  p := pos (';',TheExtensions);
  if p = 0 then
    result := TheExtensions
  else
    result := copy(TheExtensions, 1, p-1);
end;

procedure TImageHandlersManager.RegisterImageHandlers (const ATypeName,TheExtensions:string;
                   AReader:TFPCustomImageReaderClass; AWriter:TFPCustomImageWriterClass);
var ih : TIHData;
begin
  ih := GetData (ATypeName);
  if assigned (ih) then
    FPImgError (StrTypeAlreadyExist,[ATypeName]);
  ih := TIHData.Create;
  with ih do
    begin
    FTypeName := ATypeName;
    FExtension := lowercase(TheExtensions);
    FDefaultExt := CalcDefExt (TheExtensions);
    FReader := AReader;
    FWriter := AWriter;
    end;
  FData.Add (ih);
end;

procedure TImageHandlersManager.RegisterImageReader (const ATypeName,TheExtensions:string;
                   AReader:TFPCustomImageReaderClass);
var ih : TIHData;
begin
  ih := GetData (ATypeName);
  if assigned (ih) then
    begin
      if assigned (ih.FReader) then
        FPImgError (StrTypeReaderAlreadyExist,[ATypeName])
      else
        ih.FReader := AReader;
    end
  else
    begin
    ih := TIHData.Create;
    with ih do
      begin
      FTypeName := ATypeName;
      FExtension := Lowercase(TheExtensions);
      FDefaultExt := CalcDefExt (TheExtensions);
      FReader := AReader;
      FWriter := nil;
      end;
    FData.Add (ih);
    end;
end;

procedure TImageHandlersManager.RegisterImageWriter (const ATypeName,TheExtensions:string;
                   AWriter:TFPCustomImageWriterClass);
var ih : TIHData;
begin
  ih := GetData (ATypeName);
  if assigned (ih) then
    begin
    if assigned (ih.FWriter) then
      FPImgError (StrTypeWriterAlreadyExist,[ATypeName])
    else
      ih.FWriter := AWriter;
    end
  else
    begin
    ih := TIHData.Create;
    with ih do
      begin
      FTypeName := ATypeName;
      FExtension := lowercase(TheExtensions);
      FDefaultExt := CalcDefExt (TheExtensions);
      FReader := nil;
      FWriter := AWriter;
      end;
    FData.Add (ih);
    end;
end;

procedure TImageHandlersManager.UnregisterImageHandlers(const ATypeName: string; ARemoveReader: boolean = True;
  ARemoveWriter: boolean = True);
var r : integer;
    ih: TIHData;
begin
  if not ARemoveReader and not ARemoveWriter then
    exit;
  r := FData.count;
  repeat
    dec (r);
  until (r < 0) or (compareText (TIHData(FData[r]).FTypeName, ATypeName) = 0);
  if r >= 0 then begin
    ih:= TIHData(FData[r]);
    if ARemoveReader then
      ih.FReader:= nil;
    if ARemoveWriter then
      ih.FWriter:= nil;
    if not Assigned(ih.FReader) and not Assigned(ih.FWriter) then begin
      FData.Delete(r);
      ih.Free;
    end;
  end;
end;

function TImageHandlersManager.GetCount : integer;
begin
  result := FData.Count;
end;

function TImageHandlersManager.GetData (const ATypeName:string) : TIHData;
var r : integer;
begin
  r := FData.count;
  repeat
    dec (r);
  until (r < 0) or (compareText (TIHData(FData[r]).FTypeName, ATypeName) = 0);
  if r >= 0 then
    result := TIHData(FData[r])
  else
    result := nil;
end;

function TImageHandlersManager.GetData (index:integer) : TIHData;
begin
  if (index >= 0) and (index < FData.count) then
    result := TIHData (FData[index])
  else
    result := nil;
end;

function TImageHandlersManager.GetTypeName (index:integer) : string;
var ih : TIHData;
begin
  ih := TIHData (FData[index]);
  result := ih.FTypeName;
end;

function TImageHandlersManager.GetReader (const TypeName:string) : TFPCustomImageReaderClass;
var ih : TIHData;
begin
  ih := GetData (TypeName);
  if assigned(ih) then
    result := ih.FReader
  else
    result := nil;
end;

function TImageHandlersManager.GetWriter (const TypeName:string) : TFPCustomImageWriterClass;
var ih : TIHData;
begin
  ih := GetData (TypeName);
  if assigned(ih) then
    result := ih.FWriter
  else
    result := nil;
end;

function TImageHandlersManager.GetExt (const TypeName:string) : string;
var ih : TIHData;
begin
  ih := GetData (TypeName);
  if assigned(ih) then
    result := ih.FExtension
  else
    result := '';
end;

function TImageHandlersManager.GetDefExt (const TypeName:string) : string;
var ih : TIHData;
begin
  ih := GetData (TypeName);
  if assigned(ih) then
    result := ih.FDefaultExt
  else
    result := '';
end;

{ TFPCustomImageHandler }

constructor TFPCustomImageHandler.create;
begin
  inherited create;
end;

procedure TFPCustomImageHandler.Progress(Stage: TProgressStage;
                         PercentDone: Byte;  RedrawNow: Boolean; const R: TRect;
                         const Msg: AnsiString; var Continue: Boolean);

begin
  If Assigned(FOnProgress) then
    FOnProgress(Self,Stage,PercentDone,RedrawNow,R,Msg,Continue)
  else If Assigned(FImage) then
    // It is debatable whether we should pass ourselves or the image ?
    FImage.Progress(Self,Stage,PercentDone,RedrawNow,R,Msg,Continue);
end;

{ TFPCustomImageReader }

constructor TFPCustomImageReader.Create;
begin
  inherited create;
  FDefImageClass := TFPMemoryImage;
end;

function TFPCustomImageReader.ImageRead (Str:TStream; Img:TFPCustomImage) : TFPCustomImage;
begin
  try
    if not assigned(Str) then
      raise FPImageException.Create(ErrorText[StrNoStream]);
    FStream := Str;
    if not assigned(img) then
      result := FDefImageClass.Create(0,0)
    else
      result := Img;
    FImage := result;
    if FImage.UsePalette then
      FImage.Palette.Clear;
    if CheckContents (Str) then
      begin
      InternalRead (Str, result)
      end
    else
      raise FPImageException.Create ('Wrong image format');
  finally
    FStream := nil;
    FImage := nil;
  end;
end;

function TFPCustomImageReader.CheckContents (Str:TStream) : boolean;
var InRead : boolean;
begin
  InRead := assigned(FStream);
  if not assigned(Str) then
    raise FPImageException.Create(ErrorText[StrNoStream]);
  try
    FSTream := Str;
    result := InternalCheck (Str);
  finally
    if not InRead then
      FStream := nil;
  end;
end;

class function TFPCustomImageReader.InternalSize(Str: TStream): TPoint;

begin
  Result.X:=-1;
  Result.Y:=-1;
end;

class function TFPCustomImageReader.ImageSize(Str: TStream): TPoint;
var
  P: Int64;
begin
  P:=Str.Position;
  result := InternalSize (Str);
  Str.Position:=P;
end;


{ TFPCustomImageWriter }

procedure TFPCustomImageWriter.ImageWrite (Str:TStream; Img:TFPCustomImage);
begin
  if not assigned(img) then
    raise FPImageException.Create(ErrorText[StrNoImageToWrite]);
  if not assigned(Str) then
    raise FPImageException.Create(ErrorText[StrNoStream]);
  try
    FStream := str;
    FImage := img;
    Str.position := 0;
    Str.Size := 0;
    InternalWrite(Str, Img);
  finally
    FStream := nil;
    FImage := nil;
  end;
end;

{.$i FPPalette.inc}
{ TFPPalette }

constructor TFPPalette.create (ACount : integer);
begin
  inherited create;
  if aCount > 0 then
    getmem (FData, sizeof(TFPColor)*ACount)
  else
    FData := nil;
  FCapacity := ACount;
  SetCount (0);
end;

destructor TFPPalette.destroy;
begin
  if FCapacity > 0 then
    freemem (FData);
  inherited;
end;

procedure TFPPalette.Build (Img : TFPCustomImage);
var x,y : integer;
begin
  if (Img.Palette <> self) then
    begin
    Count := 0;
    for x := 0 to img.width-1 do
      for y := 0 to img.height-1 do
        IndexOf(img[x,y]);
    end;
end;

procedure TFPPalette.Copy(APalette: TFPPalette);
var
  x: integer;
begin
  if (APalette <> Self) then
  begin
    Self.Clear;
    for x := 0 to APalette.Count - 1 do
        Add(APalette.Color[x])
  end;
end;

procedure TFPPalette.Merge (pal : TFPPalette);
var r : integer;
begin
  for r := 0 to pal.count-1 do
    IndexOf (pal[r]);
end;

procedure TFPPalette.CheckIndex (index:integer);
begin
  if (index >= FCount) or (index < 0) then
    FPImgError (StrInvalidIndex,[ErrorText[StrPalette],index]);
end;

function TFPPalette.Add (const Value:TFPColor) : integer;
begin
  result := FCount;
  inc (FCount);
  if FCount > FCapacity then
    EnlargeData;
  FData^[result] := Value;
end;

procedure TFPPalette.SetColor (index:integer; const Value:TFPColor);
begin
  if index = FCount then
    Add (Value)
  else
    begin
    CheckIndex (index);
    FData^[index] := Value;
    end;
end;

function TFPPalette.GetColor (index:integer) : TFPColor;
begin
  CheckIndex (index);
  result := FData^[index];
end;

function TFPPalette.GetCount : integer;
begin
  result := FCount;
end;

procedure TFPPalette.EnlargeData;
var old : integer;
    NewData : PFPColorArray;
begin
  old := FCapacity;
  if FCapacity <= 16 then
    FCapacity := 32
  else if FCapacity <= 128 then
    FCapacity := 256
  else
    // MG: changed to exponential growth
    inc (FCapacity, FCapacity);
  GetMem (NewData, sizeof(TFPColor)*FCapacity);
  if old > 0 then
    begin
    move (FData^[0], NewData^[0], sizeof(TFPColor)*FCount);
    FreeMem (FData);
    end;
  FData := NewData;
end;

procedure TFPPalette.SetCount (Value:integer);
var
    O : integer;
begin
  if Value <> FCount then
    begin
    if Value > FCapacity then
      begin
        FCapacity := Value+8;
        Reallocmem(FData,sizeof(TFPColor)*FCapacity);
      end;
    for o := FCount to Value-1 do
      FData^[o] := colBlack;
    FCount := Value;
    end;
end;

procedure TFPPalette.SetCapacity (ind : Integer);
var o : Integer;
begin
  if ind<count then ind:=count;
  if ind<>fcapacity then
    begin
      fcapacity:=ind;
      Reallocmem(FData,sizeof(TFPColor)*FCapacity);
    end;
  if ind>count then
    begin
      for o := FCount to ind-1 do
        FData^[o] := colBlack;
    end;
end;

function TFPPalette.IndexOf (const AColor:TFPColor) : integer;
begin
  result := FCount;
  repeat
    dec (result);
  until (result < 0) or (FData^[result]=AColor);
  if result < 0 then
    result := Add (AColor);
end;

procedure TFPPalette.Clear;
begin
  SetCount (0);
end;


{ Functions to create standard palettes, by Giulio Bernardi 2005 }

{ A simple 1 bit black and white palette }
function CreateBlackAndWhitePalette : TFPPalette;
var fppal : TFPPalette;
    Col : TFPColor;
begin
  fppal:=TFPPalette.Create(2);
  Col.Alpha:=AlphaOpaque;
  Col.Red:=$FFFF; Col.Green:=$FFFF; Col.Blue:=$FFFF;
  fppal.Color[0]:=Col;
  Col.Red:=$0000; Col.Green:=$0000; Col.Blue:=$0000;
  fppal.Color[1]:=Col;
  Result:=fppal;
end;

{ The "standard" netscape 216-color palette (aka: web safe palette) }
function CreateWebSafePalette : TFPPalette;
var Col : TFPColor;
    i : integer;
    fppal : TFPPalette;
begin
  fppal:=TFPPalette.Create(216);
  Col.Alpha:=AlphaOpaque;
  i:=0;
  Col.Red:=$FFFF;
  while true do
  begin
    Col.Green:=$FFFF;
    while true do
    begin
      Col.Blue:=$FFFF;
      while true do
      begin
        fppal.Color[i]:=Col;
        if Col.Blue=0 then break;
        dec(Col.Blue,$3333);
      end;
      if Col.Green=0 then break;
      dec(Col.Green,$3333);
    end;
    if Col.Red=0 then break;
    dec(Col.Red,$3333);
  end;
  Result:=fppal;
end;

{ A grayscale palette. Not very useful. }
function CreateGrayScalePalette : TFPPalette;
var Col : TFPColor;
    i : integer;
    fppal : TFPPalette;
begin
  fppal:=TFPPalette.Create(256);
  Col.Alpha:=AlphaOpaque;
  for i:=0 to $FF do
  begin
    Col.Red:=i;
    Col.Red:=(Col.Red shl 8) + Col.Red;
    Col.Green:=Col.Red;
    Col.Blue:=Col.Red;
    fppal.Color[i]:=Col;
  end;
  Result:=fppal;
end;

{ Standard VGA 16 color palette. }
function CreateVGAPalette : TFPPalette;
var fppal : TFPPalette;
begin
  fppal:=TFPPalette.Create(16);
  fppal.Color[0]:=colBlack;
  fppal.Color[1]:=colNavy;
  fppal.Color[2]:=colBlue;
  fppal.Color[3]:=colMaroon;
  fppal.Color[4]:=colPurple;
  fppal.Color[5]:=colDkGreen;
  fppal.Color[6]:=colRed;
  fppal.Color[7]:=colTeal;
  fppal.Color[8]:=colFuchsia;
  fppal.Color[9]:=colOlive;
  fppal.Color[10]:=colGray;
  fppal.Color[11]:=colLime;
  fppal.Color[12]:=colAqua;
  fppal.Color[13]:=colSilver;
  fppal.Color[14]:=colYellow;
  fppal.Color[15]:=colWhite;
  Result:=fppal;
end;

{.$i FPColCnv.inc}
{function FillOtherBits (initial:word;CorrectBits:byte):word;
var r,c : byte;
begin
  c := 16 div CorrectBits;
  result := initial;
  for r := 1 to c do
    result := (result shr CorrectBits) or result;
end;

function ShiftAndFill (initial:word; CorrectBits:byte):word;
begin
  result := FillOtherBits (initial shl (16-correctbits), correctbits);
end;
}
function CalculateGray (const from : TFPcolor) : word;
var temp : longword;
begin
  with GrayConvMatrix do
    temp := round(red*from.red + green*from.green + blue*from.blue);
  if temp > $ffff then
    result := $ffff
  else
    result := temp;
end;

(*
type
  TColorBits = array [0..3] of TColorData;
     // 0:alpha, 1:red, 2:green, 3:blue
  TShiftBits = array [0..3] of shortint;

const
  ColorBits : array[cfRGB15..cfABGR64] of TColorBits = (
    //          alpha       red        green      blue
    {cfRGB15} ($00000000, $00007C00, $000003E0, $0000001F),
    {cfRGB16} ($00000000, $00007C00, $000003E0, $0000001F),
    {cfRGB24} ($00000000, $00FF0000, $0000FF00, $000000FF),
    {cfRGB32} ($00000000, $00FF0000, $0000FF00, $000000FF),
    {cfRGB48} ($00000000, $FFFF0000, $FFFF0000, $0000FFFF),
    //                     shl 16
    {cfRGBA8} ($00000003, $000000C0, $00000030, $0000000C),
    {cfRGBA16}($0000000F, $0000F000, $00000F00, $000000F0),
    {cfRGBA32}($000000FF, $FF000000, $00FF0000, $0000FF00),
    {cfRGBA64}($0000FFFF, $FFFF0000, $FFFF0000, $FFFF0000),
    //                    shl 32     shl 16
    {cfBGR15} ($00000000, $0000001F, $000003E0, $00007C00),
    {cfBGR16} ($00000000, $0000001F, $000003E0, $00007C00),
    {cfBGR24} ($00000000, $000000FF, $0000FF00, $00FF0000),
    {cfBGR32} ($00000000, $000000FF, $0000FF00, $00FF0000),
    {cfBGR48} ($00000000, $0000FFFF, $FFFF0000, $FFFF0000),
    //                                           shl 16
    {cfABGR8} ($000000C0, $00000003, $0000000C, $00000030),
    {cfABGR16}($0000F000, $0000000F, $000000F0, $00000F00),
    {cfABGR32}($FF000000, $000000FF, $0000FF00, $00FF0000),
    {cfABGR64}($FFFF0000, $0000FFFF, $FFFF0000, $FFFF0000)
    //          shl 32                          shl 16
  );
  ShiftBits : array[cfRGB15..cfABGR64] of TShiftBits = (  // <0:shl, >0:shr
    {cfRGB15} (  0,  -1,  -6, -11),
    {cfRGB16} (  0,  -1,  -6, -11),
    {cfRGB24} (  0,   8,   0,  -8),
    {cfRGB32} (  0,   8,   0,  -8),
    {cfRGB48} (  0,  32,  16,   0),
    {cfRGBA8} (-14,  -8, -10, -12),
    {cfRGBA16}(-12,   0,  -4,  -8),
    {cfRGBA32}( -8,  16,   8,   0),
    {cfRGBA64}(  0,  48,  32,  16),
    {cfBGR15} (  0, -11,  -6,  -1),
    {cfBGR16} (  0, -11,  -6,  -1),
    {cfBGR24} (  0,  -8,   0,   8),
    {cfBGR32} (  0,  -8,   0,   8),
    {cfBGR48} (  0,   0,  16,  32),
    {cfBGRA8} ( -8, -14, -12, -10),
    {cfBGRA16}(  0, -12,  -8,  -4),
    {cfBGRA32}( 16,  -8,   0,   8),
    {cfBGRA64}( 48,   0,  16,  32)
    );
  Bitdepths : array[cfRGB15..cfABGR64] of byte=
    (5,5,8,8,16, 2,4,8,16, 5,5,8,8,16, 2,4,8,16);

function EnlargeColor (data:TColorData;CFmt:TColorFormat;component:byte):word;
var w : word;
    i : TColorData;
    s : shortint;
begin
  i := data and ColorBits[CFmt,component];
  s := ShiftBits[CFmt,component];
  if s = 0 then
    w := i
  else if s < 0 then
    w := i shl -s
  else
    w := i shr s;
  result := FillOtherBits (w ,BitDepths[CFmt]);
end;

function ConvertColor (const From : TColorData; FromFmt:TColorFormat) : TFPColor;
  function SetGrayScale (value : word) : TFPColor;
  begin
    with result do
      begin
      red := Value;
      green := value;
      blue := Value;
      end;
  end;
  function SetGrayScaleA (value : word) : TFPColor;
  begin
    result := SetGrayScale (value);
    result.alpha := alphaOpaque;
  end;
var m : qword;
begin
  case FromFmt of
    cfMono : result := SetGrayScaleA (ShiftAndFill(From,1));
    cfGray2 : result := SetGrayScaleA (ShiftAndFill(From,2));
    cfGray4 : result := SetGrayScaleA (ShiftAndFill(From,4));
    cfGray8 : result := SetGrayScaleA (ShiftAndFill(From,8));
    cfGray16 : result := SetGrayScaleA (From);
    cfGray24 : result := SetGrayScaleA ((From and $00FFFF00) shr 8);
    cfGrayA8 :
      begin
      result := SetGrayScale (FillOtherBits((From and $000000F0) shl 8,4));
      result.alpha := ShiftAndFill((From and $0000000F),4);
      end;
    cfGrayA16 :
      begin
      result := SetGrayScale (FillOtherBits((From and $0000FF00),8));
      result.alpha := ShiftAndFill((From and $000000FF),8);
      end;
    cfGrayA32 :
      begin
      result := SetGrayScale ((From and $FFFF0000) shr 16);
      result.alpha := (From and $0000FFFF);
      end;
    cfRGB15,cfRGB16,cfRGB24,cfRGB32,cfRGB48,
    cfBGR15,cfBGR16,cfBGR24,cfBGR32,cfBGR48 :
      begin
      result.alpha := AlphaOpaque;
      result.red := EnlargeColor(From, FromFmt, 1);
      result.green := EnlargeColor(From, FromFmt, 2);
      result.blue := EnlargeColor(From, FromFmt, 3);
      end;
    cfRGBA8,cfRGBA16,cfRGBA32,cfRGBA64,
    cfABGR8,cfABGR16,cfABGR32,cfABGR64 :
      begin
      result.alpha := EnlargeColor(From, FromFmt, 0);
      result.red := EnlargeColor(From, FromFmt, 1);
      result.green := EnlargeColor(From, FromFmt, 2);
      result.blue := EnlargeColor(From, FromFmt, 3);
      end;
  end;
end;

function ConvertColor (const From : TDeviceColor) : TFPColor;
begin
  result := ConvertColor (From.data, From.Fmt)
end;

const BitMasks : array[1..32] of longword =
    ($8000000, $C000000, $E000000, $F000000,
     $F800000, $FC00000, $FE00000, $FF00000,
     $FF80000, $FFC0000, $FFE0000, $FFF0000,
     $FFF8000, $FFFC000, $FFFE000, $FFFF000,
     $FFFF800, $FFFFC00, $FFFFE00, $FFFFF00,
     $FFFFF80, $FFFFFC0, $FFFFFE0, $FFFFFF0,
     $FFFFFF8, $FFFFFFC, $FFFFFFE, $FFFFFFF,
     $FFFFFFF, $FFFFFFF, $FFFFFFF, $FFFFFFF);

procedure PrepareBitMasks;
{ Putting the correct bits in the array (problem with constants in compiler 1.0)}
var r : integer;
begin
  for r := 1 to 32 do
    BitMasks[r] := BitMasks[r] shl 4;
  inc (BitMasks[29], $8);
  inc (BitMasks[30], $C);
  inc (BitMasks[31], $E);
  inc (BitMasks[32], $F);
end;

function CalculateGray (const c : TFPcolor; Bits:byte) : TColorData;
var temp : longword;
begin
  with GrayConvMatrix do
    temp := round(red*c.red + green*c.green + blue*c.blue);
  result := temp;
  //temp := temp + (result shl 16);
  //result := temp and BitMasks[Bits];
  {if not (c = colBlack) then
    with c do
      //writeln ('red:',red,' - green:',green,' - blue:',blue, ' : result=',result);
      writeln (format('red:%4x - green:%4x - blue:%4x => result:%4x',[integer(red),
               integer(green),integer(blue),integer(result)]));}
end;

function CalculateGrayA (const c : TFPcolor; Bits:byte) : TColorData;
var r : longword;
    d : byte;
begin
  d := bits div 2;
  r := CalculateGray (c, d);
  result := r shl d;
  r := c.alpha shr (16-d);
  result := result or r;
end;

function ConvertColorToData (const From : TFPColor; Fmt : TColorFormat) : TColorData;
var sb : TShiftBits;
    cb : TColorBits;
  function MakeSample (Value:word; ToShift:shortint; ToUse:TColorData) : TColorData;
  var sh : word;
  begin
    result := Value;
    if ToShift >= 0 then
      begin
      sh := ToShift;            // if not converting first to word, there will be a
      result := result shl Sh;  // color shift
      end
    else
      begin
      sh := -ToShift;
      result := result shr Sh;
      end;
    result := result and ToUse;
  end;
begin
  case Fmt of
    cfMono : result := CalculateGray (From,1);
    cfGray2 : result := CalculateGray (From,2);
    cfGray4 : result := CalculateGray (From,4);
    cfGray8 : result := CalculateGray (From,8);
    cfGray16 : result := CalculateGray (From,16);
    cfGray24 : result := CalculateGray (From,24);
    cfGrayA8 : result := CalculateGrayA (From, 8);
    cfGrayA16 : result := CalculateGrayA (From, 16);
    cfGrayA32 : result := CalculateGrayA (From, 32);
    cfRGB15,cfRGB16,cfRGB24,cfRGB32,cfRGB48,
    cfBGR15,cfBGR16,cfBGR24,cfBGR32,cfBGR48 :
      begin
      sb := ShiftBits[Fmt];
      cb := ColorBits[Fmt];
      result := MakeSample(From.blue, sb[3], cb[3]) or
                MakeSample(From.red, sb[1], cb[1]) or
                MakeSample(From.green, sb[2], cb[2]);
     {$ifdef FPC_Debug_Image}
      with From do
        writeln (red,',',green,',',blue,',',result);
      end;
     {$endif}
    cfRGBA8,cfRGBA16,cfRGBA32,cfRGBA64,
    cfABGR8,cfABGR16,cfABGR32,cfABGR64 :
      begin
      sb := ShiftBits[Fmt];
      cb := ColorBits[Fmt];
      result := MakeSample(From.alpha, sb[0], cb[0]) or
                MakeSample(From.red, sb[1], cb[1]) or
                MakeSample(From.green, sb[2], cb[2]) or
                MakeSample(From.blue, sb[3], cb[3]);
      end;
  end;
end;

function ConvertColor (const From : TFPColor; Fmt : TColorFormat) : TDeviceColor;
begin
  result.Fmt := Fmt;
  result.data := convertColorToData(From, Fmt);
end;

function ConvertColorToData (const From : TDeviceColor; Fmt : TColorFormat) : TColorData;
var c : TFPColor;
begin
  c := ConvertColor (From);
  result := ConvertColorToData (c, Fmt);
end;

function ConvertColor (const From : TDeviceColor; Fmt : TColorFormat) : TDeviceColor;
begin
  result.Fmt := Fmt;
  result.data := ConvertColorToData (From, Fmt);
end;
*)

function AlphaBlend(color1, color2: TFPColor): TFPColor;
var
  factor: single;
begin
  if color2.alpha = $ffff then
    Result := color2
  else
  begin
    factor := (color1.alpha / $ffff) * (1 - color2.alpha / $ffff);

    Result.red := Round(color1.red * factor + color2.red * color2.alpha / $ffff);
    Result.green := Round(color1.green * factor + color2.green * color2.alpha / $ffff);
    Result.blue := Round(color1.blue * factor + color2.blue * color2.alpha / $ffff);
    Result.alpha := Round(factor * $ffff + color2.alpha);
  end;
end;

function CompareColors(const Color1, Color2: TFPColor): integer;
begin
  Result:=integer(Color1.Red)-integer(Color2.Red);
  if Result<>0 then exit;
  Result:=integer(Color1.Green)-integer(Color2.Green);
  if Result<>0 then exit;
  Result:=integer(Color1.Blue)-integer(Color2.Blue);
  if Result<>0 then exit;
  Result:=integer(Color1.Alpha)-integer(Color2.Alpha);
end;

{.$i fpcompactimg.inc}
function GetFPCompactImgDesc(Gray: boolean; Depth: word; HasAlpha: boolean
  ): TFPCompactImgDesc;
begin
  Result.Gray:=Gray;
  Result.Depth:=Depth;
  Result.HasAlpha:=HasAlpha;
end;

function GetFPCompactImgClass(const Desc: TFPCompactImgDesc): TFPCompactImgBaseClass;
begin
  if Desc.Gray then begin
    if Desc.HasAlpha then begin
      // gray, alpha
      if Desc.Depth<=8 then
        Result:=TFPCompactImgGrayAlpha8Bit
      else
        Result:=TFPCompactImgGrayAlpha16Bit;
    end else begin
      // gray, no alpha
      if Desc.Depth<=8 then
        Result:=TFPCompactImgGray8Bit
      else
        Result:=TFPCompactImgGray16Bit;
    end;
  end else begin
    // RGB
    if Desc.HasAlpha then begin
      // RGB, alpha
      if Desc.Depth<=8 then
        Result:=TFPCompactImgRGBA8Bit
      else
        Result:=TFPCompactImgRGBA16Bit;
    end else begin
      // RGB, no alpha
      if Desc.Depth<=8 then
        Result:=TFPCompactImgRGB8Bit
      else
        Result:=TFPCompactImgRGB16Bit;
    end;
  end;
end;

function CreateFPCompactImg(const Desc: TFPCompactImgDesc; Width, Height: integer
  ): TFPCustomImage;
var
  ImgClass: TFPCompactImgBaseClass;
begin
  ImgClass:=GetFPCompactImgClass(Desc);
  Result:=ImgClass.Create(Width,Height);
end;

function CreateCompatibleFPCompactImg(Img: TFPCustomImage; Width, Height: integer
  ): TFPCustomImage;
begin
  if Img is TFPCompactImgBase then
    Result:=CreateFPCompactImg(TFPCompactImgBase(Img).Desc,Width,Height)
  else
    Result:=CreateFPCompactImg(GetMinimumPTDesc(Img),Width,Height);
end;

function CreateCompatibleFPCompactImgWithAlpha(Img: TFPCustomImage; Width,
  Height: integer): TFPCustomImage;
var
  Desc: TFPCompactImgDesc;
begin
  if Img is TFPCompactImgBase then
    Desc:=TFPCompactImgBase(Img).Desc
  else
    Desc:=GetMinimumPTDesc(Img);
  Desc.HasAlpha:=true;
  Result:=CreateFPCompactImg(Desc,Width,Height);
end;

function GetMinimumPTDesc(Img: TFPCustomImage; FuzzyDepth: word = 4): TFPCompactImgDesc;
var
  AllLoEqualsHi, AllLoAre0: Boolean;
  FuzzyMaskLoHi: dWord;

  {procedure Need16Bit(c: word); inline;
  var
    l: Byte;
  begin
    c:=c and FuzzyMaskLoHi;
    l:=Lo(c);
    AllLoAre0:=AllLoAre0 and (l=0);
    AllLoEqualsHi:=AllLoEqualsHi and (l=Hi(c));
  end;}

var
  TestGray: Boolean;
  TestAlpha: Boolean;
  Test16Bit: Boolean;
  BaseImg: TFPCompactImgBase;
  ImgDesc: TFPCompactImgDesc;
  y: Integer;
  x: Integer;
  col: TFPColor;
  FuzzyMaskWord: Word;
  FuzzyOpaque: Word;
begin
  TestGray:=true;
  TestAlpha:=true;
  Test16Bit:=FuzzyDepth<8;
  Result.HasAlpha:=false;
  Result.Gray:=true;
  Result.Depth:=8;
  if Img is TFPCompactImgBase then begin
    BaseImg:=TFPCompactImgBase(Img);
    ImgDesc:=BaseImg.Desc;
    if ImgDesc.Depth<=8 then Test16Bit:=false;
    if ImgDesc.Gray then TestGray:=false;
    if not ImgDesc.HasAlpha then TestAlpha:=false;
  end;

  if (not TestGray) and (not TestAlpha) and (not Test16Bit) then exit;

  FuzzyMaskWord:=Word($ffff) shl FuzzyDepth;
  FuzzyOpaque:=alphaOpaque and FuzzyMaskWord;
  FuzzyMaskLoHi:=Word(lo(FuzzyMaskWord))+(Word(lo(FuzzyMaskWord)) shl 8);
  AllLoAre0:=true;
  AllLoEqualsHi:=true;
  for y:=0 to Img.Height-1 do begin
    for x:=0 to Img.Width-1 do begin
      col:=Img.Colors[x,y];
      if TestAlpha and ((col.alpha and FuzzyMaskWord)<>FuzzyOpaque) then begin
        TestAlpha:=false;
        Result.HasAlpha:=true;
        if (not TestGray) and (not Test16Bit) then break;
      end;
      if TestGray
      and ((col.red and FuzzyMaskWord)<>(col.green and FuzzyMaskWord))
      or ((col.red and FuzzyMaskWord)<>(col.blue and FuzzyMaskWord)) then begin
        TestGray:=false;
        Result.Gray:=false;
        if (not TestAlpha) and (not Test16Bit) then break;
      end;
      {if Test16Bit then begin
        Need16Bit(col.red);
        Need16Bit(col.green);
        Need16Bit(col.blue);
        Need16Bit(col.alpha);
        if (not AllLoAre0) and (not AllLoEqualsHi) then begin
          Test16Bit:=false;
          Result.Depth:=16;
          if (not TestAlpha) and (not TestGray) then break;
        end;
      end;}
    end;
  end;
end;

function GetMinimumFPCompactImg(Img: TFPCustomImage; FreeImg: boolean;
  FuzzyDepth: word = 4): TFPCustomImage;
var
  Desc: TFPCompactImgDesc;
  ImgClass: TFPCompactImgBaseClass;
  y: Integer;
  x: Integer;
begin
  Desc:=GetMinimumPTDesc(Img,FuzzyDepth);
  ImgClass:=GetFPCompactImgClass(Desc);
  if Img.ClassType=ImgClass then
    exit(Img);
  Result:=CreateFPCompactImg(Desc,Img.Width,Img.Height);
  for y:=0 to Img.Height-1 do
    for x:=0 to Img.Width-1 do
      Result.Colors[x,y]:=Img.Colors[x,y];
  if FreeImg then
    Img.Free;
end;

function ColorRound (c : double) : word;
begin
  if c > $FFFF then
    result := $FFFF
  else if c < 0.0 then
    result := 0
  else
    result := round(c);
end;

{ TFPCompactImgGrayAlpha16Bit }

function TFPCompactImgGrayAlpha16Bit.GetInternalColor(x, y: integer): TFPColor;
var
  v: TFPCompactImgGrayAlpha16BitValue;
begin
  v:=FData[x+y*Width];
  Result.red:=v.g;
  Result.green:=Result.red;
  Result.blue:=Result.red;
  Result.alpha:=v.a;
end;

function TFPCompactImgGrayAlpha16Bit.GetInternalPixel(x, y: integer): integer;
begin
  Result:=0;
end;

procedure TFPCompactImgGrayAlpha16Bit.SetInternalColor(x, y: integer;
  const Value: TFPColor);
var
  v: TFPCompactImgGrayAlpha16BitValue;
begin
  v.g:=Value.red;
  v.a:=Value.alpha;
  FData[x+y*Width]:=v;
end;

procedure TFPCompactImgGrayAlpha16Bit.SetInternalPixel(x, y: integer; Value: integer
  );
begin

end;

constructor TFPCompactImgGrayAlpha16Bit.Create(AWidth, AHeight: integer);
begin
  FDesc:=GetFPCompactImgDesc(true,16,true);
  inherited Create(AWidth, AHeight);
end;

destructor TFPCompactImgGrayAlpha16Bit.Destroy;
begin
  ReAllocMem(FData,0);
  inherited Destroy;
end;

procedure TFPCompactImgGrayAlpha16Bit.SetSize(AWidth, AHeight: integer);
begin
  if (AWidth=Width) and (AHeight=Height) then exit;
  ReAllocMem(FData,SizeOf(TFPCompactImgGrayAlpha16BitValue)*AWidth*AHeight);
  inherited SetSize(AWidth, AHeight);
end;

{ TFPCompactImgGrayAlpha8Bit }

function TFPCompactImgGrayAlpha8Bit.GetInternalColor(x, y: integer): TFPColor;
var
  v: TFPCompactImgGrayAlpha8BitValue;
begin
  v:=FData[x+y*Width];
  Result.red:=(v.g shl 8)+v.g;
  Result.green:=Result.red;
  Result.blue:=Result.red;
  Result.alpha:=(v.a shl 8)+v.a;
end;

function TFPCompactImgGrayAlpha8Bit.GetInternalPixel(x, y: integer): integer;
begin
  Result:=0;
end;

procedure TFPCompactImgGrayAlpha8Bit.SetInternalColor(x, y: integer;
  const Value: TFPColor);
var
  v: TFPCompactImgGrayAlpha8BitValue;
begin
  v.g:=Value.red shr 8;
  v.a:=Value.alpha shr 8;
  FData[x+y*Width]:=v;
end;

procedure TFPCompactImgGrayAlpha8Bit.SetInternalPixel(x, y: integer; Value: integer
  );
begin

end;

constructor TFPCompactImgGrayAlpha8Bit.Create(AWidth, AHeight: integer);
begin
  FDesc:=GetFPCompactImgDesc(true,8,true);
  inherited Create(AWidth, AHeight);
end;

destructor TFPCompactImgGrayAlpha8Bit.Destroy;
begin
  ReAllocMem(FData,0);
  inherited Destroy;
end;

procedure TFPCompactImgGrayAlpha8Bit.SetSize(AWidth, AHeight: integer);
begin
  if (AWidth=Width) and (AHeight=Height) then exit;
  ReAllocMem(FData,SizeOf(TFPCompactImgGrayAlpha8BitValue)*AWidth*AHeight);
  inherited SetSize(AWidth, AHeight);
end;

{ TFPCompactImgGray16Bit }

function TFPCompactImgGray16Bit.GetInternalColor(x, y: integer): TFPColor;
begin
  Result.red:=FData[x+y*Width];
  Result.green:=Result.red;
  Result.blue:=Result.red;
  Result.alpha:=alphaOpaque;
end;

function TFPCompactImgGray16Bit.GetInternalPixel(x, y: integer): integer;
begin
  Result:=0;
end;

procedure TFPCompactImgGray16Bit.SetInternalColor(x, y: integer;
  const Value: TFPColor);
begin
  FData[x+y*Width]:=Value.red;
end;

procedure TFPCompactImgGray16Bit.SetInternalPixel(x, y: integer; Value: integer);
begin

end;

constructor TFPCompactImgGray16Bit.Create(AWidth, AHeight: integer);
begin
  FDesc:=GetFPCompactImgDesc(true,16,false);
  inherited Create(AWidth, AHeight);
end;

destructor TFPCompactImgGray16Bit.Destroy;
begin
  ReAllocMem(FData,0);
  inherited Destroy;
end;

procedure TFPCompactImgGray16Bit.SetSize(AWidth, AHeight: integer);
begin
  if (AWidth=Width) and (AHeight=Height) then exit;
  ReAllocMem(FData,SizeOf(Word)*AWidth*AHeight);
  inherited SetSize(AWidth,AHeight);
end;

{ TFPCompactImgGray8Bit }

function TFPCompactImgGray8Bit.GetInternalColor(x, y: integer): TFPColor;
begin
  Result.red:=FData[x+y*Width];
  Result.red:=(Word(Result.red) shl 8)+Result.red;
  Result.green:=Result.red;
  Result.blue:=Result.red;
  Result.alpha:=alphaOpaque;
end;

function TFPCompactImgGray8Bit.GetInternalPixel(x, y: integer): integer;
begin
  Result:=0;
end;

procedure TFPCompactImgGray8Bit.SetInternalColor(x, y: integer;
  const Value: TFPColor);
begin
  FData[x+y*Width]:=Value.red shr 8;
end;

procedure TFPCompactImgGray8Bit.SetInternalPixel(x, y: integer; Value: integer);
begin

end;

constructor TFPCompactImgGray8Bit.Create(AWidth, AHeight: integer);
begin
  FDesc:=GetFPCompactImgDesc(true,8,false);
  inherited Create(AWidth, AHeight);
end;

destructor TFPCompactImgGray8Bit.Destroy;
begin
  ReAllocMem(FData,0);
  inherited Destroy;
end;

procedure TFPCompactImgGray8Bit.SetSize(AWidth, AHeight: integer);
begin
  if (AWidth=Width) and (AHeight=Height) then exit;
  ReAllocMem(FData,SizeOf(Byte)*AWidth*AHeight);
  inherited SetSize(AWidth,AHeight);
end;

{ TFPCompactImgRGBA8Bit }

function TFPCompactImgRGBA8Bit.GetInternalColor(x, y: integer): TFPColor;
var
  v: TFPCompactImgRGBA8BitValue;
begin
  v:=FData[x+y*Width];
  Result.red:=(v.r shl 8)+v.r;
  Result.green:=(v.g shl 8)+v.g;
  Result.blue:=(v.b shl 8)+v.b;
  Result.alpha:=(v.a shl 8)+v.a;
end;

function TFPCompactImgRGBA8Bit.GetInternalPixel(x, y: integer): integer;
begin
  Result:=0;
end;

procedure TFPCompactImgRGBA8Bit.SetInternalColor(x, y: integer;
  const Value: TFPColor);
var
  v: TFPCompactImgRGBA8BitValue;
begin
  v.r:=Value.red shr 8;
  v.g:=Value.green shr 8;
  v.b:=Value.blue shr 8;
  v.a:=Value.alpha shr 8;
  FData[x+y*Width]:=v;
end;

procedure TFPCompactImgRGBA8Bit.SetInternalPixel(x, y: integer; Value: integer);
begin

end;

constructor TFPCompactImgRGBA8Bit.Create(AWidth, AHeight: integer);
begin
  FDesc:=GetFPCompactImgDesc(false,8,true);
  inherited Create(AWidth, AHeight);
end;

destructor TFPCompactImgRGBA8Bit.Destroy;
begin
  ReAllocMem(FData,0);
  inherited Destroy;
end;

procedure TFPCompactImgRGBA8Bit.SetSize(AWidth, AHeight: integer);
begin
  if (AWidth=Width) and (AHeight=Height) then exit;
  ReAllocMem(FData,SizeOf(TFPCompactImgRGBA8BitValue)*AWidth*AHeight);
  inherited SetSize(AWidth,AHeight);
end;

{ TFPCompactImgRGB8Bit }

function TFPCompactImgRGB8Bit.GetInternalColor(x, y: integer): TFPColor;
var
  v: TFPCompactImgRGB8BitValue;
begin
  v:=FData[x+y*Width];
  Result.red:=(v.r shl 8)+v.r;
  Result.green:=(v.g shl 8)+v.g;
  Result.blue:=(v.b shl 8)+v.b;
  Result.alpha:=alphaOpaque;
end;

function TFPCompactImgRGB8Bit.GetInternalPixel(x, y: integer): integer;
begin
  Result:=0;
end;

procedure TFPCompactImgRGB8Bit.SetInternalColor(x, y: integer; const Value: TFPColor
  );
var
  v: TFPCompactImgRGB8BitValue;
begin
  v.r:=Value.red shr 8;
  v.g:=Value.green shr 8;
  v.b:=Value.blue shr 8;
  FData[x+y*Width]:=v;
end;

procedure TFPCompactImgRGB8Bit.SetInternalPixel(x, y: integer; Value: integer);
begin

end;

constructor TFPCompactImgRGB8Bit.Create(AWidth, AHeight: integer);
begin
  FDesc:=GetFPCompactImgDesc(false,8,false);
  inherited Create(AWidth, AHeight);
end;

destructor TFPCompactImgRGB8Bit.Destroy;
begin
  ReAllocMem(FData,0);
  inherited Destroy;
end;

procedure TFPCompactImgRGB8Bit.SetSize(AWidth, AHeight: integer);
begin
  if (AWidth=Width) and (AHeight=Height) then exit;
  ReAllocMem(FData,SizeOf(TFPCompactImgRGB8BitValue)*AWidth*AHeight);
  inherited SetSize(AWidth,AHeight);
end;

{ TFPCompactImgRGB16Bit }

function TFPCompactImgRGB16Bit.GetInternalColor(x, y: integer): TFPColor;
var
  v: TFPCompactImgRGB16BitValue;
begin
  v:=FData[x+y*Width];
  Result.red:=v.r;
  Result.green:=v.g;
  Result.blue:=v.b;
  Result.alpha:=alphaOpaque;
end;

function TFPCompactImgRGB16Bit.GetInternalPixel(x, y: integer): integer;
begin
  Result:=0;
end;

procedure TFPCompactImgRGB16Bit.SetInternalColor(x, y: integer;
  const Value: TFPColor);
var
  v: TFPCompactImgRGB16BitValue;
begin
  v.r:=Value.red;
  v.g:=Value.green;
  v.b:=Value.blue;
  FData[x+y*Width]:=v;
end;

procedure TFPCompactImgRGB16Bit.SetInternalPixel(x, y: integer; Value: integer);
begin

end;

constructor TFPCompactImgRGB16Bit.Create(AWidth, AHeight: integer);
begin
  FDesc:=GetFPCompactImgDesc(false,16,false);
  inherited Create(AWidth, AHeight);
end;

destructor TFPCompactImgRGB16Bit.Destroy;
begin
  ReAllocMem(FData,0);
  inherited Destroy;
end;

procedure TFPCompactImgRGB16Bit.SetSize(AWidth, AHeight: integer);
begin
  if (AWidth=Width) and (AHeight=Height) then exit;
  ReAllocMem(FData,SizeOf(TFPCompactImgRGB16BitValue)*AWidth*AHeight);
  inherited SetSize(AWidth,AHeight);
end;

{ TFPCompactImgRGBA16Bit }

function TFPCompactImgRGBA16Bit.GetInternalColor(x, y: integer): TFPColor;
begin
  Result:=FData[x+y*Width];
end;

function TFPCompactImgRGBA16Bit.GetInternalPixel(x, y: integer): integer;
begin
  Result:=0;
end;

procedure TFPCompactImgRGBA16Bit.SetInternalColor(x, y: integer;
  const Value: TFPColor);
begin
  FData[x+y*Width]:=Value;
end;

procedure TFPCompactImgRGBA16Bit.SetInternalPixel(x, y: integer; Value: integer);
begin

end;

constructor TFPCompactImgRGBA16Bit.Create(AWidth, AHeight: integer);
begin
  FDesc:=GetFPCompactImgDesc(false,16,true);
  inherited Create(AWidth, AHeight);
end;

destructor TFPCompactImgRGBA16Bit.Destroy;
begin
  ReAllocMem(FData,0);
  inherited Destroy;
end;

procedure TFPCompactImgRGBA16Bit.SetSize(AWidth, AHeight: integer);
begin
  if (AWidth=Width) and (AHeight=Height) then exit;
  ReAllocMem(FData,SizeOf(TFPColor)*AWidth*AHeight);
  inherited SetSize(AWidth,AHeight);
end;


function FPColor (r,g,b:word) : TFPColor;
begin
  with result do
    begin
    red := r;
    green := g;
    blue := b;
    alpha := alphaOpaque;
    end;
end;

function FPColor (r,g,b,a:word) : TFPColor;
begin
  with result do
    begin
    red := r;
    green := g;
    blue := b;
    alpha := a;
    end;
end;

class operator TFPColor.Equal(const c,d:TFPColor) : boolean;
begin
  result := (c.Red = d.Red) and
            (c.Green = d.Green) and
            (c.Blue = d.Blue) and
            (c.Alpha = d.Alpha);
end;

class operator TFPColor.NotEqual(const c,d:TFPColor) : boolean;
begin
  result := (c.Red <> d.Red) and
            (c.Green <> d.Green) and
            (c.Blue <> d.Blue) and
            (c.Alpha <> d.Alpha);
end;

function GetFullColorData (color:TFPColor) : TColorData;
begin
  result := PColorData(@color)^;
end;

function SetFullColorData (color:TColorData) : TFPColor;
begin
  result := PFPColor (@color)^;
end;

class operator TFPColor.LogicalOr(const c,d:TFPColor) : TFPColor;
begin
  result := SetFullColorData(GetFullColorData(c) OR GetFullColorData(d));
end;

class operator TFPColor.LogicalAnd(const c,d:TFPColor) : TFPColor;
begin
  result := SetFullColorData(GetFullColorData(c) AND GetFullColorData(d));
end;

class operator TFPColor.LogicalXor(const c,d:TFPColor) : TFPColor;
begin
  result := SetFullColorData(GetFullColorData(c) XOR GetFullColorData(d));
end;

{$ifdef debug}
function MakeHex (n:TColordata;nr:byte): string;
const hexnums : array[0..15] of AnsiChar =
              ('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F');
var r : integer;
begin
  result := '';
  for r := 0 to nr-1 do
    begin
    result := hexnums[n and $F] + result;
    n := n shr 4;
    if ((r+1) mod 4) = 0 then
      result := ' ' + result;
    end;
end;
{$endif}

type
  THtmlColorName = (
    hcnWhite, hcnSilver, hcnGray, hcnBlack,
    hcnRed, hcnMaroon, hcnYellow, hcnOlive,
    hcnLime, hcnGreen, hcnAqua, hcnTeal, hcnBlue,
    hcnNavy, hcnFuchsia, hcnPurple);

const
  HtmlColorNameToFPColorMap: array[THtmlColorName] of TFPColor = (
    (red: $ff; green: $ff; blue: $ff; alpha: alphaOpaque), //hcnWhite
    (red: $c0; green: $c0; blue: $c0; alpha: alphaOpaque), //hcnSilver
    (red: $80; green: $80; blue: $80; alpha: alphaOpaque), //hcnGray
    (red: $00; green: $00; blue: $00; alpha: alphaOpaque), //hcnBlack
    (red: $ff; green: $00; blue: $00; alpha: alphaOpaque), //hcnRed
    (red: $80; green: $00; blue: $00; alpha: alphaOpaque), //hcnMaroon
    (red: $ff; green: $ff; blue: $00; alpha: alphaOpaque), //hcnYellow
    (red: $80; green: $80; blue: $00; alpha: alphaOpaque), //hcnOlive
    (red: $00; green: $ff; blue: $00; alpha: alphaOpaque), //hcnLime
    (red: $00; green: $80; blue: $00; alpha: alphaOpaque), //hcnGreen
    (red: $00; green: $ff; blue: $ff; alpha: alphaOpaque), //hcnAqua
    (red: $00; green: $80; blue: $80; alpha: alphaOpaque), //hcnTeal
    (red: $00; green: $00; blue: $ff; alpha: alphaOpaque), //hcnBlue
    (red: $00; green: $00; blue: $80; alpha: alphaOpaque), //hcnNavy
    (red: $ff; green: $00; blue: $ff; alpha: alphaOpaque), //hcnFuchsia
    (red: $80; green: $00; blue: $80; alpha: alphaOpaque)  //hcnPurple
  );

function TryStrToHtmlColorName(const S: String; out AName: THtmlColorName): Boolean;
begin
   Result := True;
   case AnsiIndexStr(LowerCase(S) , ['white','silver','gray','black','red','maroon','yellow','olive','lime','green','aqua','teal','blue','navy','fuchsia','purple']) of
     00: AName := hcnWhite;
     01: AName := hcnSilver;
     02: AName := hcnGray;
     03: AName := hcnBlack;
     04: AName := hcnRed;
     05: AName := hcnMaroon;
     06: AName := hcnYellow;
     07: AName := hcnOlive;
     08: AName := hcnLime;
     09: AName := hcnGreen;
     10: AName := hcnAqua;
     11: AName := hcnTeal;
     12: AName := hcnBlue;
     13: AName := hcnNavy;
     14: AName := hcnFuchsia;
     15: AName := hcnPurple;
  else
    Result := False;
  end;
end;

{ Try to translate HTML color code into TFPColor
  Supports following formats
    '#rgb'
    '#rrggbb'
    W3C Html color name
}
function TryHtmlToFPColor(const S: String; out FPColor: TFPColor): Boolean;

  function TryHexStrToWord(const Hex: String; out W: Word): Boolean;
  var
    Code: Integer;
  begin
    Val('$'+Hex, W, Code);
    Result := (Code = 0);
    if not Result then W := 0;
  end;

var
  AName: THtmlColorName;
begin
  Result := False;
  FPColor.red := 0;
  FPColor.green := 0;
  FPColor.blue := 0;
  FPColor.alpha := alphaOpaque;
  if (Length(S) = 0) then
    Exit;
  if (S[1] = '#') then
  begin
    if Length(S) = 4 then
    begin  // #rgb
      Result := (TryHexstrToWord(S[2]+S[2], FPColor.red) and
                 TryHexstrToWord(S[3]+S[3], FPColor.green) and
                 TryHexstrToWord(S[4]+S[4], FPColor.blue));
    end
    else if Length(S) = 7 then
    begin  // #rrggbb
      Result := (TryHexstrToWord(S[2]+S[3], FPColor.red) and
                 TryHexstrToWord(S[4]+S[5], FPColor.green) and
                 TryHexstrToWord(S[6]+S[7], FPColor.blue));
    end;
  end
  else
  begin
    Result := TryStrToHtmlColorName(S, AName);
    if Result then
      FPColor := HtmlColorNameToFPColorMap[AName];
  end;
end;

function HtmlToFPColorDef(const S: String; out FPColor: TFpColor; Def: TFPColor): TFPColor;
begin
  if not TryHtmlToFPColor(S, Result) then
    Result := Def;
end;

function HtmlToFpColor(const S: String): TFPColor;
begin
  if not TryHtmlToFpColor(S, Result) then
    raise EConvertError.CreateFmt(ErrorText[StrInvalidHTMLColor], [S]);
end;


initialization
  ImageHandlers := TImageHandlersManager.Create;
  GrayConvMatrix := GCM_JPEG;
  // Following lines are here because the compiler 1.0 can't work with int64 constants
(*  ColorBits [cfRGB48,1] := ColorBits [cfRGB48,1] shl 16;
  ColorBits [cfRGBA64,1] := ColorBits [cfRGBA64,1] shl 32;
  ColorBits [cfRGBA64,2] := ColorBits [cfRGBA64,2] shl 16;
  ColorBits [cfABGR64,0] := ColorBits [cfABGR64,0] shl 32;
  ColorBits [cfABGR64,3] := ColorBits [cfABGR64,3] shl 16;
  ColorBits [cfBGR48,3] := ColorBits [cfBGR48,3] shl 16;
  PrepareBitMasks;*)

finalization
  ImageHandlers.Free;

end.
