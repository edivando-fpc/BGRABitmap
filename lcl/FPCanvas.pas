{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    Basic canvas definitions.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$i FPImage.inc}{$H+}
unit FPCanvas;

interface

uses Math, sysutils, classes, FPImage, Types;

const
  PatternBitCount = sizeof(longword) * 8;

type

  PPoint = ^TPoint;
  TFPCanvasException = class (Exception);
  TFPPenException = class (TFPCanvasException);
  TFPBrushException = class (TFPCanvasException);
  TFPFontException = class (TFPCanvasException);

  TFPCustomCanvas = class;

  { TFPCanvasHelper }

  TFPCanvasHelper = class(TPersistent)
  private
    FDelayAllocate: boolean;
    FFPColor : TFPColor;
    FAllocated,
    FFixedCanvas : boolean;
    FCanvas : TFPCustomCanvas;
    FFlags : word;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    procedure NotifyCanvas;
  protected
    // flags 0-15 are reserved for FPCustomCanvas
    function GetAllocated: boolean; virtual;
    procedure SetFlags (index:integer; AValue:boolean); virtual;
    function GetFlags (index:integer) : boolean; virtual;
    procedure CheckAllocated (ValueNeeded:boolean);
    procedure SetFixedCanvas (AValue : boolean);
    procedure DoAllocateResources; virtual;
    procedure DoDeAllocateResources; virtual;
    procedure DoCopyProps (From:TFPCanvasHelper); virtual;
    procedure SetFPColor (const AValue:TFPColor); virtual;
    procedure Changing; dynamic;
    procedure Changed; dynamic;
    Procedure Lock;
    Procedure UnLock;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    // prepare helper for use
    procedure AllocateResources (ACanvas : TFPCustomCanvas;
                                 CanDelay: boolean = true);
    // free all resource used by this helper
    procedure DeallocateResources;
    property Allocated : boolean read GetAllocated;
    // properties cannot be changed when allocated
    property FixedCanvas : boolean read FFixedCanvas;
    // Canvas for which the helper is allocated
    property Canvas : TFPCustomCanvas read FCanvas;
    // color of the helper
    property FPColor : TFPColor read FFPColor Write SetFPColor;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property DelayAllocate: boolean read FDelayAllocate write FDelayAllocate;
  end;

  TFPCustomFont = class (TFPCanvasHelper)
  private
    FName : string;
    FOrientation,
    FSize : integer;
  protected
    procedure DoCopyProps (From:TFPCanvasHelper); override;
    procedure SetName (AValue:string); virtual;
    procedure SetSize (AValue:integer); virtual;
    procedure SetOrientation (AValue:integer); virtual;
    function GetOrientation : Integer;
  public
    function CopyFont : TFPCustomFont;
    // Creates a copy of the font with all properties the same, but not allocated
    procedure GetTextSize (text:string; var w,h:integer);
    function GetTextHeight (text:string) : integer;
    function GetTextWidth (text:string) : integer;
    property Name : string read FName write SetName;
    property Size : integer read FSize write SetSize;
    property Bold : boolean index 5 read GetFlags write SetFlags;
    property Italic : boolean index 6 read GetFlags write SetFlags;
    property Underline : boolean index 7 read GetFlags write SetFlags;
    property StrikeThrough : boolean index 8 read GetFlags write SetFlags;
    property Orientation: Integer read GetOrientation write SetOrientation default 0;

  end;
  TFPCustomFontClass = class of TFPCustomFont;

  TFPPenStyle = (psSolid, psDash, psDot, psDashDot, psDashDotDot, psinsideFrame, psPattern,psClear);
  TFPPenStyleSet = set of TFPPenStyle;
  TFPPenMode = (pmBlack, pmWhite, pmNop, pmNot, pmCopy, pmNotCopy,
                pmMergePenNot, pmMaskPenNot, pmMergeNotPen, pmMaskNotPen, pmMerge,
                pmNotMerge, pmMask, pmNotMask, pmXor, pmNotXor);
  TPenPattern = Longword;
  TFPPenEndCap = (
    pecRound,
    pecSquare,
    pecFlat
  );
  TFPPenJoinStyle = (
    pjsRound,
    pjsBevel,
    pjsMiter
  );

  { TFPCustomPen }

  TFPCustomPen = class (TFPCanvasHelper)
  private
    FStyle : TFPPenStyle;
    FWidth : Integer;
    FMode : TFPPenMode;
    FPattern : longword;
    FEndCap: TFPPenEndCap;
    FJoinStyle: TFPPenJoinStyle;
  protected
    procedure DoCopyProps (From:TFPCanvasHelper); override;
    procedure SetMode (AValue : TFPPenMode); virtual;
    procedure SetWidth (AValue : Integer); virtual;
    procedure SetStyle (AValue : TFPPenStyle); virtual;
    procedure SetPattern (AValue : longword); virtual;
    procedure SetEndCap(AValue: TFPPenEndCap); virtual;
    procedure SetJoinStyle(AValue: TFPPenJoinStyle); virtual;
  public
    function CopyPen : TFPCustomPen;
    // Creates a copy of the pen with all properties the same, but not allocated
    property Style : TFPPenStyle read FStyle write SetStyle;
    property Width : Integer read FWidth write SetWidth;
    property Mode : TFPPenMode read FMode write SetMode;
    property Pattern : longword read FPattern write SetPattern;
    property EndCap : TFPPenEndCap read FEndCap write SetEndCap;
    property JoinStyle : TFPPenJoinStyle read FJoinStyle write SetJoinStyle;
  end;
  TFPCustomPenClass = class of TFPCustomPen;

  TFPBrushStyle = (bsSolid, bsClear, bsHorizontal, bsVertical, bsFDiagonal,
                   bsBDiagonal, bsCross, bsDiagCross, bsImage, bsPattern);
  TBrushPattern = array[0..PatternBitCount-1] of TPenPattern;
  PBrushPattern = ^TBrushPattern;

  TFPCustomBrush = class (TFPCanvasHelper)
  private
    FStyle : TFPBrushStyle;
    FImage : TFPCustomImage;
    FPattern : TBrushPattern;
  protected
    procedure SetStyle (AValue : TFPBrushStyle); virtual;
    procedure SetImage (AValue : TFPCustomImage); virtual;
    procedure DoCopyProps (From:TFPCanvasHelper); override;
  public
    function CopyBrush : TFPCustomBrush;
    property Style : TFPBrushStyle read FStyle write SetStyle;
    property Image : TFPCustomImage read FImage write SetImage;
    property Pattern : TBrushPattern read FPattern write FPattern;
  end;
  TFPCustomBrushClass = class of TFPCustomBrush;

  { TFPCustomInterpolation }

  TFPCustomInterpolation = class
  private
    fcanvas: TFPCustomCanvas;
    fimage: TFPCustomImage;
  protected
    procedure Initialize (aimage:TFPCustomImage; acanvas:TFPCustomCanvas); virtual;
    procedure Execute (x,y,w,h:integer); virtual; abstract;
  public
    property Canvas : TFPCustomCanvas read fcanvas;
    property Image : TFPCustomImage read fimage;
  end;

  { TFPBoxInterpolation }

  TFPBoxInterpolation = class(TFPCustomInterpolation)
  public
    procedure Execute (x,y,w,h:integer); override;
  end;

  { TFPBaseInterpolation }

  TFPBaseInterpolation = class (TFPCustomInterpolation)
  private
    procedure CreatePixelWeights (OldSize, NewSize: integer;
      out Entries: Pointer; out EntrySize: integer; out Support: integer);
  protected
    procedure Execute (x,y,w,h : integer); override;
    function Filter (x : double): double; virtual;
    function MaxSupport : double; virtual;
  end;

  { TMitchelInterpolation }

  TMitchelInterpolation = class (TFPBaseInterpolation)
  protected
    function Filter (x : double) : double; override;
    function MaxSupport : double; override;
  end;

  TFPCustomRegion = class
  public
    function GetBoundingRect: TRect; virtual; abstract;
    function IsPointInRegion(AX, AY: Integer): Boolean; virtual; abstract;
  end;

  { TFPRectRegion }

  TFPRectRegion = class(TFPCustomRegion)
  public
    Rect: TRect;
    function GetBoundingRect: TRect; override;
    function IsPointInRegion(AX, AY: Integer): Boolean; override;
  end;

  { TFPCustomCanvas }

  TFPCustomCanvas = class(TPersistent)
  private
    FClipping,
    FManageResources: boolean;
    FRemovingHelpers : boolean;
    FHelpers : TList;
    FLocks : integer;
    FInterpolation : TFPCustomInterpolation;
    function AllowFont (AFont : TFPCustomFont) : boolean;
    function AllowBrush (ABrush : TFPCustomBrush) : boolean;
    function AllowPen (APen : TFPCustomPen) : boolean;
    function CreateDefaultFont : TFPCustomFont;
    function CreateDefaultPen : TFPCustomPen;
    function CreateDefaultBrush : TFPCustomBrush;
    procedure RemoveHelpers;
    function GetFont : TFPCustomFont;
    function GetBrush : TFPCustomBrush;
    function GetPen : TFPCustomPen;
  protected
    FDefaultFont, FFont : TFPCustomFont;
    FDefaultBrush, FBrush : TFPCustomBrush;
    FDefaultPen, FPen : TFPCustomPen;
    FPenPos : TPoint;
    FClipRegion : TFPCustomRegion;
    function DoCreateDefaultFont : TFPCustomFont; virtual; abstract;
    function DoCreateDefaultPen : TFPCustomPen; virtual; abstract;
    function DoCreateDefaultBrush : TFPCustomBrush; virtual; abstract;
    procedure SetFont (AValue:TFPCustomFont); virtual;
    procedure SetBrush (AValue:TFPCustomBrush); virtual;
    procedure SetPen (AValue:TFPCustomPen); virtual;
    function  DoAllowFont (AFont : TFPCustomFont) : boolean; virtual;
    function  DoAllowPen (APen : TFPCustomPen) : boolean; virtual;
    function  DoAllowBrush (ABrush : TFPCustomBrush) : boolean; virtual;
    procedure SetColor (x,y:integer; const Value:TFPColor); Virtual; abstract;
    function  GetColor (x,y:integer) : TFPColor; Virtual; abstract;
    procedure SetHeight (AValue : integer); virtual; abstract;
    function  GetHeight : integer; virtual; abstract;
    procedure SetWidth (AValue : integer); virtual; abstract;
    function  GetWidth : integer; virtual; abstract;
    function  GetClipRect: TRect; virtual;
    procedure SetClipRect(const AValue: TRect); virtual;
    function  GetClipping: boolean; virtual;
    procedure SetClipping(const AValue: boolean); virtual;
    procedure SetPenPos(const AValue: TPoint); virtual;
    procedure DoLockCanvas; virtual;
    procedure DoUnlockCanvas; virtual;
    procedure DoTextOut (x,y:integer;text:string); virtual; abstract;
    procedure DoGetTextSize (text:string; var w,h:integer);  virtual; abstract;
    function  DoGetTextHeight (text:string) : integer; virtual; abstract;
    function  DoGetTextWidth (text:string) : integer; virtual; abstract;
//    procedure DoTextOut (x,y:integer;text:unicodestring); overload; virtual;
//    procedure DoGetTextSize (text:unicodestring; var w,h:integer); overload; virtual;
//    function  DoGetTextHeight (text:unicodestring) : integer; virtual;
//    function  DoGetTextWidth (text:unicodestring) : integer; virtual;
    procedure DoRectangle (Const Bounds:TRect); virtual; abstract;
    procedure DoRectangleFill (Const Bounds:TRect); virtual; abstract;
    procedure DoRectangleAndFill (Const Bounds:TRect); virtual;
    procedure DoEllipseFill (Const Bounds:TRect); virtual; abstract;
    procedure DoEllipse (Const Bounds:TRect); virtual; abstract;
    procedure DoEllipseAndFill (Const Bounds:TRect); virtual;
    procedure DoPolygonFill (const points:array of TPoint); virtual; abstract;
    procedure DoPolygon (const points:array of TPoint); virtual; abstract;
    procedure DoPolygonAndFill (const points:array of TPoint); virtual;
    procedure DoPolyline (const points:array of TPoint); virtual; abstract;
    procedure DoFloodFill (x,y:integer); virtual; abstract;
    procedure DoMoveTo (x,y:integer); virtual;
    procedure DoLineTo (x,y:integer); virtual;
    procedure DoLine (x1,y1,x2,y2:integer); virtual; abstract;
    procedure DoCopyRect (x,y:integer; canvas:TFPCustomCanvas; Const SourceRect:TRect); virtual; abstract;
    procedure DoDraw (x,y:integer; Const image:TFPCustomImage); virtual; abstract;
    procedure DoRadialPie(x1, y1, x2, y2, StartAngle16Deg, Angle16DegLength: Integer); virtual;
    procedure DoPolyBezier(Points: PPoint; NumPts: Integer;
                           Filled: boolean = False;
                           Continuous: boolean = False); virtual;
    procedure CheckHelper (AHelper:TFPCanvasHelper); virtual;
    procedure AddHelper (AHelper:TFPCanvasHelper);
  public
    constructor create;
    destructor destroy; override;
    procedure LockCanvas;
    procedure UnlockCanvas;
    function Locked: boolean;
    function CreateFont : TFPCustomFont;
    function CreatePen : TFPCustomPen;
    function CreateBrush : TFPCustomBrush;
    // using font
    procedure TextOut (x,y:integer;text:string); virtual;
    procedure GetTextSize (text:string; var w,h:integer);
    function GetTextHeight (text:string) : integer;
    function GetTextWidth (text:string) : integer;
    function TextExtent(const Text: string): TSize; virtual;
    function TextHeight(const Text: string): Integer; virtual;
    function TextWidth(const Text: string): Integer; virtual;
//    procedure TextOut (x,y:integer;text:unicodestring); virtual;
//    procedure GetTextSize (text:unicodestring; var w,h:integer);
//    function GetTextHeight (text:unicodestring) : integer;
//    function GetTextWidth (text:unicodestring) : integer;
//    function TextExtent(const Text: unicodestring): TSize; virtual;
//    function TextHeight(const Text: unicodestring): Integer; virtual;
//    function TextWidth(const Text: unicodestring): Integer; virtual;
    // using pen and brush
    procedure Arc(ALeft, ATop, ARight, ABottom, Angle16Deg, Angle16DegLength: Integer); overload; virtual;
    procedure Arc(ALeft, ATop, ARight, ABottom, SX, SY, EX, EY: Integer); overload; virtual;
    procedure Ellipse (Const Bounds:TRect); overload; virtual;
    procedure Ellipse (left,top,right,bottom:integer); overload; virtual;
    procedure EllipseC (x,y:integer; rx,ry:longword);
    procedure Polygon (Const points:array of TPoint); virtual;
    procedure Polyline (Const points:array of TPoint); virtual;
    procedure RadialPie(x1, y1, x2, y2, StartAngle16Deg, Angle16DegLength: Integer); virtual;
    procedure PolyBezier(Points: PPoint; NumPts: Integer;
                         Filled: boolean = False;
                         Continuous: boolean = False); overload;  virtual;
    procedure PolyBezier(const Points: array of TPoint;
                         Filled: boolean = False;
                         Continuous: boolean = False); overload; virtual;
    procedure Rectangle (Const Bounds : TRect); overload; virtual;
    procedure Rectangle (left,top,right,bottom:integer); overload; virtual;
    procedure FillRect(const ARect: TRect); overload;  virtual;
    procedure FillRect(X1,Y1,X2,Y2: Integer); overload; virtual;
    // using brush
    procedure FloodFill (x,y:integer); virtual;
    procedure Clear;
    // using pen
    procedure MoveTo (x,y:integer); overload;
    procedure MoveTo (p:TPoint); overload;
    procedure LineTo (x,y:integer); overload;
    procedure LineTo (p:TPoint); overload;
    procedure Line (x1,y1,x2,y2:integer); overload;
    procedure Line (const p1,p2:TPoint); overload;
    procedure Line (const points:TRect); overload;
    // other procedures
    procedure CopyRect (x,y:integer; canvas:TFPCustomCanvas; SourceRect:TRect);
    procedure Draw (x,y:integer; image:TFPCustomImage);
    procedure StretchDraw (x,y,w,h:integer; source:TFPCustomImage);
    procedure Erase;virtual;
    // properties
    property LockCount: Integer read FLocks;
    property Font : TFPCustomFont read GetFont write SetFont;
    property Pen : TFPCustomPen read GetPen write SetPen;
    property Brush : TFPCustomBrush read GetBrush write SetBrush;
    property Interpolation : TFPCustomInterpolation read FInterpolation write FInterpolation;
    property Colors [x,y:integer] : TFPColor read GetColor write SetColor;
    property ClipRect : TRect read GetClipRect write SetClipRect;
    property ClipRegion : TFPCustomRegion read FClipRegion write FClipRegion;
    property Clipping : boolean read GetClipping write SetClipping;
    property PenPos : TPoint read FPenPos write SetPenPos;
    property Height : integer read GetHeight write SetHeight;
    property Width : integer read GetWidth write SetWidth;
    property ManageResources: boolean read FManageResources write FManageResources;
  end;

  TFPCustomDrawFont = class (TFPCustomFont)
  private
    procedure DrawText (x,y:integer; text:string);
    procedure GetTextSize (text:string; var w,h:integer);
    function GetTextHeight (text:string) : integer;
    function GetTextWidth (text:string) : integer;
//    procedure DrawText (x,y:integer; text:unicodestring);
//    procedure GetTextSize (text: unicodestring; var w,h:integer);
//    function GetTextHeight (text: unicodestring) : integer;
//    function GetTextWidth (text: unicodestring) : integer;
  protected
    procedure DoDrawText (x,y:integer; text:string); virtual; abstract;
    procedure DoGetTextSize (text:string; var w,h:integer); virtual; abstract;
    function DoGetTextHeight (text:string) : integer; virtual; abstract;
    function DoGetTextWidth (text:string) : integer; virtual; abstract;
//    procedure DoDrawText (x,y:integer; text:unicodestring); virtual;
//    procedure DoGetTextSize (text: unicodestring; var w,h:integer); virtual;
//    function DoGetTextHeight (text: unicodestring) : integer; virtual;
//    function DoGetTextWidth (text: unicodestring) : integer; virtual;
  end;

  TFPEmptyFont = class (TFPCustomFont)
  end;

  TFPCustomDrawPen = class (TFPCustomPen)
  private
    procedure DrawLine (x1,y1,x2,y2:integer);
    procedure Polyline (const points:array of TPoint; close:boolean);
    procedure Ellipse (left,top, right,bottom:integer);
    procedure Rectangle (left,top, right,bottom:integer);
  protected
    procedure DoDrawLine (x1,y1,x2,y2:integer); virtual; abstract;
    procedure DoPolyline (const points:array of TPoint; close:boolean); virtual; abstract;
    procedure DoEllipse (left,top, right,bottom:integer); virtual; abstract;
    procedure DoRectangle (left,top, right,bottom:integer); virtual; abstract;
  end;

  TFPEmptyPen = class (TFPCustomPen)
  end;

  TFPCustomDrawBrush = class (TFPCustomBrush)
  private
    procedure Rectangle (left,top, right,bottom:integer);
    procedure FloodFill (x,y:integer);
    procedure Ellipse (left,top, right,bottom:integer);
    procedure Polygon (const points:array of TPoint);
  public
    procedure DoRectangle (left,top, right,bottom:integer); virtual; abstract;
    procedure DoEllipse (left,top, right,bottom:integer); virtual; abstract;
    procedure DoFloodFill (x,y:integer); virtual; abstract;
    procedure DoPolygon (const points:array of TPoint); virtual; abstract;
  end;

  TFPEmptyBrush = class (TFPCustomBrush)
  end;

procedure DecRect (var rect : TRect; delta:integer); overload;
procedure IncRect (var rect : TRect; delta:integer); overload;
procedure DecRect (var rect : TRect); overload;
procedure IncRect (var rect : TRect); overload;

implementation

uses clipping;

const
  EFont = 'Font';
  EPen = 'Pen';
  EBrush = 'Brush';
  ErrAllocation = '%s %s be allocated.';
  ErrAlloc : array [boolean] of string = ('may not','must');
  ErrCouldNotCreate = 'Could not create a %s.';
  ErrNoLock = 'Canvas not locked.';

procedure DecRect (var rect : TRect; delta:integer);
begin
  with rect do
    begin
    left := left + delta;
    right := right - delta;
    top := top + delta;
    bottom := bottom - delta;
    end;
end;

procedure DecRect (var rect : trect);
begin
  DecRect (rect, 1);
end;

procedure IncRect (var rect : trect);
begin
  IncRect (rect, 1);
end;

procedure IncRect (var rect : TRect; delta:integer);
begin
  with rect do
    begin
    left := left - delta;
    right := right + delta;
    top := top - delta;
    bottom := bottom + delta;
    end;
end;

{ TFPRectRegion }

function TFPRectRegion.GetBoundingRect: TRect;
begin
  Result := Rect;
end;

function TFPRectRegion.IsPointInRegion(AX, AY: Integer): Boolean;
begin
  Result := (AX >= Rect.Left) and (AX <= Rect.Right) and
    (AY >= Rect.Top) and (AY <= Rect.Bottom);
end;


{ TFPCanvasHelper }

constructor TFPCanvasHelper.Create;
begin
  inherited create;
  FCanvas := nil;
  FFixedCanvas := false;
  FAllocated := false;
end;

destructor TFPCanvasHelper.destroy;
begin
  if Allocated then
    DeAllocateResources;
  inherited;
end;

procedure TFPCanvasHelper.SetFixedCanvas (AValue : boolean);
begin
  FFixedCanvas := AValue;
end;

procedure TFPCanvasHelper.NotifyCanvas;
// called to unbind from canvas
begin
  if FCanvas<>nil then
    FCanvas.CheckHelper (self);
end;

procedure TFPCanvasHelper.CheckAllocated (ValueNeeded:boolean);

  procedure RaiseErrAllocation;
  begin
    Raise TFPFontException.CreateFmt (ErrAllocation,
                                      [EFont, ErrAlloc[ValueNeeded]]);
  end;

begin
  if (Allocated <> ValueNeeded) then
    RaiseErrAllocation;
end;

procedure TFPCanvasHelper.SetFPColor(const AValue:TFPColor);
begin
  FFPColor := AValue;
end;

procedure TFPCanvasHelper.Changing;
begin
  if Assigned(FOnChanging) then FOnChanging(Self);
end;

procedure TFPCanvasHelper.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TFPCanvasHelper.Lock;
begin

end;

procedure TFPCanvasHelper.UnLock;
begin

end;

procedure TFPCanvasHelper.SetFlags (index:integer; AValue:boolean);
begin
  if AValue then
    FFlags := FFlags or (1 shl index)
  else
    FFlags := FFlags and not (1 shl index);
end;

function TFPCanvasHelper.GetFlags (index:integer) : boolean;
begin
  result := (FFlags and (1 shl index)) <> 0;
end;

function TFPCanvasHelper.GetAllocated : boolean;
begin
  if FFixedCanvas then
    result := assigned(FCanvas)
  else
    result := FAllocated;
end;

procedure TFPCanvasHelper.AllocateResources (ACanvas : TFPCustomCanvas;
  CanDelay: boolean);
begin
  if FFixedCanvas and FAllocated then
    DeallocateResources;
  FCanvas := ACanvas;
  if DelayAllocate and CanDelay then exit;
  try
    DoAllocateResources;
    FAllocated := True;
  except
    FCanvas := nil;
    FAllocated := False;
  end;
end;

procedure TFPCanvasHelper.DeallocateResources;
begin
  if FAllocated then
    try
      DoDeallocateResources;
    finally
      FAllocated := false;
      NotifyCanvas;
      FCanvas := nil;
    end;
end;

procedure TFPCanvasHelper.DoCopyProps (From:TFPCanvasHelper);
begin
  FPColor := from.FPColor;
end;

procedure TFPCanvasHelper.DoAllocateResources;
begin
end;

procedure TFPCanvasHelper.DoDeallocateResources;
begin
end;

{ FPCustomFont }

procedure TFPCustomFont.SetName (AValue:string);
begin
  FName := AValue;
end;

procedure TFPCustomFont.SetSize (AValue:integer);
begin
  FSize := AValue;
end;

procedure TFPCustomFont.SetOrientation (AValue:integer);
begin
  FOrientation := AValue;
end;

function TFPCustomFont.GetOrientation : Integer;
begin
  Result := FOrientation;
end;


procedure TFPCustomFont.DoCopyProps (From:TFPCanvasHelper);
begin
  with from as TFPCustomFont do
    begin
    self.FName := FName;
    self.FSize := FSize;
    self.FFPColor := FFPColor;
    self.FFlags := FFlags;
    end;
end;

function TFPCustomFont.CopyFont : TFPCustomFont;
begin
  result := TFPCustomFont(self.ClassType.Create);
  result.DoCopyProps (self);
end;

procedure TFPCustomFont.GetTextSize (text:string; var w,h:integer);
begin
  if inheritsFrom (TFPCustomDrawFont) then
    TFPCustomDrawFont(self).DoGetTextSize (text,w,h)
  else
    FCanvas.DoGetTextSize (text, w,h);
end;

function TFPCustomFont.GetTextHeight (text:string) : integer;
begin
  if inheritsFrom (TFPCustomDrawFont) then
    result := TFPCustomDrawFont(self).DoGetTextHeight (text)
  else
   if assigned(FCanvas) then
     result := FCanvas.GetTextHeight (text)
   else
     result :=16; // *some* default better than none.
end;

function TFPCustomFont.GetTextWidth (text:string) : integer;
begin
  if inheritsFrom (TFPCustomDrawFont) then
    result := TFPCustomDrawFont(self).DoGetTextWidth (text)
  else
   if assigned(FCanvas) then
     result := FCanvas.GetTextWidth (text)
   else
     result :=16; // *some* default better than none.
end;

{ TFPCustomPen }

procedure TFPCustomPen.SetMode (AValue : TFPPenMode);
begin
  FMode := AValue;
end;

procedure TFPCustomPen.SetWidth (AValue : Integer);
begin
  if AValue < 0 then
    FWidth := 0
  else
    FWidth := AValue;
end;

procedure TFPCustomPen.SetStyle (AValue : TFPPenStyle);
begin
  FStyle := AValue;
end;

procedure TFPCustomPen.SetPattern (AValue : longword);
begin
  FPattern := AValue;
end;

procedure TFPCustomPen.SetEndCap(AValue: TFPPenEndCap);
begin
  if FEndCap=AValue then Exit;
  FEndCap:=AValue;
end;

procedure TFPCustomPen.SetJoinStyle(AValue: TFPPenJoinStyle);
begin
  if FJoinStyle=AValue then Exit;
  FJoinStyle:=AValue;
end;

procedure TFPCustomPen.DoCopyProps (From:TFPCanvasHelper);
begin
  with From as TFPCustomPen do
    begin
    self.Style := Style;
    self.Width := Width;
    self.Mode := Mode;
    self.pattern := pattern;
    end;
  inherited;
end;

function TFPCustomPen.CopyPen : TFPCustomPen;
begin
  result := TFPCustomPen(self.ClassType.Create);
  result.DoCopyProps (self);
end;

{ TFPCustomBrush }

procedure TFPCustomBrush.SetStyle (AValue : TFPBrushStyle);
begin
  FStyle := AValue
end;

procedure TFPCustomBrush.SetImage (AValue : TFPCustomImage);
begin
  FImage := AValue;
end;

procedure TFPCustomBrush.DoCopyProps (From:TFPCanvasHelper);
begin
  with From as TFPCustomBrush do
    begin
    self.Style := Style;
    self.Image := Image;
    end;
  inherited DoCopyProps(From);
end;

function TFPCustomBrush.CopyBrush : TFPCustomBrush;
begin
  result := TFPCustomBrush(self.ClassType.Create);
  result.DoCopyProps (self);
end;

{ TFPCustomInterpolation }

procedure TFPCustomInterpolation.Initialize(aimage: TFPCustomImage; acanvas: TFPCustomCanvas);
begin
  fimage := aimage;
  fcanvas := acanvas;
end;

{ TFPBoxInterpolation }

procedure TFPBoxInterpolation.Execute(x,y,w,h:integer);
var
  dx, dy, iw, ih: Integer;
begin
  iw := Image.Width;
  ih := Image.Height;

  for dx := 0 to w-1 do
    for dy := 0 to h-1 do
      Canvas.Colors[x+dx,y+dy] := Image.Colors[dx*iw div w, dy*ih div h];
end;

{ TFPBaseInterpolation }

procedure TFPBaseInterpolation.CreatePixelWeights(OldSize, NewSize: integer;
  out Entries: Pointer; out EntrySize: integer; out Support: integer);
// create an array of #NewSize entries. Each entry starts with an integer
// for the StartIndex, followed by #Support singles for the pixel weights.
// The sum of weights for each entry is 1.
var
  Entry: Pointer;

  procedure SetSupport(NewSupport: integer);
  begin
    Support:=NewSupport;
    EntrySize:=SizeOf(integer)+SizeOf(Single)*Support;
    Getmem(Entries,EntrySize*NewSize);
    Entry:=Entries;
  end;

var
  i: Integer;
  Factor: double;
  StartPos: Double;
  StartIndex: Integer;
  j: Integer;
  FirstValue: Double;
  //Sum: double;
begin
  if NewSize=OldSize then
  begin
    SetSupport(1);
    for i:=0 to NewSize-1 do
    begin
      // 1:1
      PInteger(Entry)^:=i;
      inc(PInteger(Entry),SizeOf(Integer));
      PSingle(Entry)^:=1.0;
      inc(PSingle(Entry),SizeOf(Single));
    end;
  end else if NewSize<OldSize then
  begin
    // shrink
    SetSupport(Max(2,(OldSize+NewSize-1) div NewSize));
//#    Factor:= double(OldSize)/double(NewSize);
    Factor:= OldSize/NewSize;
    for i:=0 to NewSize-1 do
    begin
      StartPos:=Factor*i;
      StartIndex:=Floor(StartPos);
      PInteger(Entry)^:=StartIndex;
      inc(PInteger(Entry),SizeOf(Integer));
      // first pixel
//#      FirstValue:=(1.0-(StartPos-double(StartIndex)));
      FirstValue:=(1.0-(StartPos- StartIndex));
      PSingle(Entry)^:=FirstValue/Factor;
      inc(PSingle(Entry),SizeOf(Single));
      // middle pixel
      for j:=1 to Support-2 do
      begin
        PSingle(Entry)^:=1.0/Factor;
        inc(PSingle(Entry),SizeOf(Single));
      end;
      // last pixel
      PSingle(Entry)^:=(Factor-FirstValue-(Support-2))/Factor;
      inc(PSingle(Entry),SizeOf(Single));
    end;
  end else
  begin
    // enlarge
    if OldSize=1 then
    begin
      SetSupport(1);
      for i:=0 to NewSize-1 do
      begin
        // nothing to interpolate
        PInteger(Entry)^:=0;
        inc(PInteger(Entry),SizeOf(Integer));
        PSingle(Entry)^:=1.0;
        inc(PSingle(Entry),SizeOf(Single));
      end;
    end else
    begin
      SetSupport(2);
//#      Factor:=double(OldSize-1)/double(NewSize);
      Factor:=(OldSize-1)/(NewSize);
      for i:=0 to NewSize-1 do
      begin
        StartPos:=Factor*i+Factor/2;
        StartIndex:=Floor(StartPos);
        PInteger(Entry)^:=StartIndex;
        inc(PInteger(Entry),SizeOf(Integer));
        // first pixel
//#        FirstValue:=(1.0-(StartPos-double(StartIndex)));
        FirstValue:=(1.0-(StartPos-StartIndex));
        // convert linear distribution
        FirstValue:=Min(1.0,Max(0.0,Filter(FirstValue/MaxSupport)));
        PSingle(Entry)^:=FirstValue;
        inc(PSingle(Entry),SizeOf(Single));
        // last pixel
        PSingle(Entry)^:=1.0-FirstValue;
        inc(PSingle(Entry),SizeOf(Single));
      end;
    end;
  end;
//#  if Entry <> (Entries+EntrySize*NewSize) then
//#    raise Exception.Create('TFPBase2Interpolation.Execute inconsistency');
end;

procedure TFPBaseInterpolation.Execute(x, y, w, h: integer);
// paint Image on Canvas at x,y,w*h
var
  dy: Integer;
  dx: Integer;
  HorzResized: PFPColor;
  xEntries: Pointer;
  xEntrySize: integer;
  xSupport: integer;// how many horizontal pixel are needed to create one pixel
  yEntries: Pointer;
  yEntrySize: integer;
  ySupport: integer;// how many vertizontal pixel are needed to create one pixel
  NewSupportLines: LongInt;
  yEntry: Pointer;
  SrcStartY: LongInt;
  LastSrcStartY: LongInt;
  LastyEntry: Pointer;
  sy: Integer;
  xEntry: Pointer;
  sx: LongInt;
  cx: Integer;
  f: Single;
  NewCol: TFPColor;
  Col: TFPColor;
  CurEntry: Pointer;
begin
  if (w<=0) or (h<=0) or (image.Width=0) or (image.Height=0) then
    exit;

  xEntries:=nil;
  yEntries:=nil;
  HorzResized:=nil;
  try
    CreatePixelWeights(image.Width,w,xEntries,xEntrySize,xSupport);
    CreatePixelWeights(image.Height,h,yEntries,yEntrySize,ySupport);
    // create temporary buffer for the horizontally resized pixel for the
    // current y line
    GetMem(HorzResized,w*ySupport*SizeOf(TFPColor));

    LastyEntry:=nil;
    SrcStartY:=0;
    for dy:=0 to h-1 do
    begin
      if dy=0 then
      begin
        yEntry:=yEntries;
        SrcStartY:=PInteger(yEntry)^;
        NewSupportLines:=ySupport;
      end else
      begin
        LastyEntry:=yEntry;
        LastSrcStartY:=SrcStartY;
        inc(PInteger(yEntry),yEntrySize);
        SrcStartY:=PInteger(yEntry)^;
        NewSupportLines:=SrcStartY-LastSrcStartY;
        // move lines up
        if (NewSupportLines>0) and (ySupport>NewSupportLines) then
          System.Move(HorzResized[NewSupportLines*w],
                      HorzResized[0],
                      (ySupport-NewSupportLines)*w*SizeOf(TFPColor));
      end;

      // compute new horizontally resized line(s)
      for sy:=ySupport-NewSupportLines to ySupport-1 do
      begin
        xEntry:=xEntries;
        for dx:=0 to w-1 do
        begin
          sx:=PInteger(xEntry)^;
          inc(PInteger(xEntry),SizeOf(integer));
          NewCol:= colTransparent;
          for cx:=0 to xSupport-1 do
          begin
            f:=PSingle(xEntry)^;
            inc(PSingle(xEntry),SizeOf(Single));
            Col:=image.Colors[sx+cx,SrcStartY+sy];
            NewCol.red:=Min(NewCol.red+round(Col.red*f),$ffff);
            NewCol.green:=Min(NewCol.green+round(Col.green*f),$ffff);
            NewCol.blue:=Min(NewCol.blue+round(Col.blue*f),$ffff);
            NewCol.alpha:=Min(NewCol.alpha+round(Col.alpha*f),$ffff);
          end;
          HorzResized[dx+sy*w]:=NewCol;
        end;
      end;

      // compute new vertically resized line
      for dx:=0 to w-1 do
      begin
        CurEntry:=PInteger(yEntry)+SizeOf(integer);
        NewCol:=colTransparent;
        for sy:=0 to ySupport-1 do
        begin
          f:=PSingle(CurEntry)^;
          inc(PSingle(CurEntry),SizeOf(Single));
          Col:=HorzResized[dx+sy*w];
          NewCol.red:=Min(NewCol.red+round(Col.red*f),$ffff);
          NewCol.green:=Min(NewCol.green+round(Col.green*f),$ffff);
          NewCol.blue:=Min(NewCol.blue+round(Col.blue*f),$ffff);
          NewCol.alpha:=Min(NewCol.alpha+round(Col.alpha*f),$ffff);
        end;
        Canvas.Colors[x+dx,y+dy]:=NewCol;
      end;
    end;
  finally
    if xEntries<>nil then FreeMem(xEntries);
    if yEntries<>nil then FreeMem(yEntries);
    if HorzResized<>nil then FreeMem(HorzResized);
  end;
end;

function TFPBaseInterpolation.Filter(x: double): double;
begin
  Result:=x;
end;

function TFPBaseInterpolation.MaxSupport: double;
begin
  Result:=1.0;
end;

{ TMitchelInterpolation }

function TMitchelInterpolation.Filter(x: double): double;
const
  B  = (1.0/3.0);
  C  = (1.0/3.0);
  P0 = ((  6.0- 2.0*B       )/6.0);
  P2 = ((-18.0+12.0*B+ 6.0*C)/6.0);
  P3 = (( 12.0- 9.0*B- 6.0*C)/6.0);
  Q0 = ((       8.0*B+24.0*C)/6.0);
  Q1 = ((     -12.0*B-48.0*C)/6.0);
  Q2 = ((       6.0*B+30.0*C)/6.0);
  Q3 = ((     - 1.0*B- 6.0*C)/6.0);
begin
  if (x < -2.0) then
    result := 0.0
  else if (x < -1.0) then
    result := Q0-x*(Q1-x*(Q2-x*Q3))
  else if (x < 0.0) then
    result := P0+x*x*(P2-x*P3)
  else if (x < 1.0) then
    result := P0+x*x*(P2+x*P3)
  else if (x < 2.0) then
    result := Q0+x*(Q1+x*(Q2+x*Q3))
  else
  result := 0.0;
end;

function TMitchelInterpolation.MaxSupport: double;
begin
  result := 2.0;
end;

{ TFPCustomCanvas }

constructor TFPCustomCanvas.Create;
begin
  inherited create;
  FClipping := false;
  FRemovingHelpers := false;
  FHelpers := TList.Create;
  FDefaultFont := CreateDefaultFont;
  FDefaultPen := CreateDefaultPen;
  FDefaultBrush := CreateDefaultBrush;
end;

destructor TFPCustomCanvas.Destroy;
begin
  FreeAndNil(FClipRegion);
  FRemovingHelpers := True;
  // first remove all helper references
  RemoveHelpers;
  // then free helpers
  FDefaultFont.Free;
  FDefaultBrush.Free;
  FDefaultPen.Free;
  FHelpers.Free;
  FRemovingHelpers := False;
  inherited;
end;

procedure TFPCustomCanvas.CheckHelper (AHelper:TFPCanvasHelper);
// remove references to AHelper
begin
  if AHelper = FPen then
    FPen := nil
  else if AHelper = FFont then
    FFont := nil
  else if AHelper = FBrush then
    FBrush := nil;
  if not FRemovingHelpers then
    begin
    if AHelper = FDefaultFont then
      FDefaultFont := CreateDefaultFont
    else if AHelper = FDefaultPen then
      FDefaultPen := CreateDefaultPen
    else if AHelper = FDefaultBrush then
      FDefaultBrush := CreateDefaultBrush;
    end;
  FHelpers.Remove (AHelper);
end;

procedure TFPCustomCanvas.RemoveHelpers;
var r : integer;
    OldState : boolean;
begin
  for r := FHelpers.count-1 downto 0 do
    with TFPCanvasHelper(FHelpers[r]) do
      if FCanvas = self then
        if FFixedCanvas then
          DeallocateResources
        else
          FCanvas := nil;
  FHelpers.Clear;
end;

procedure TFPCustomCanvas.AddHelper (AHelper : TFPCanvasHelper);
var r : integer;
begin
  r := FHelpers.IndexOf (AHelper);
  if r < 0 then
    FHelpers.Add (AHelper);
end;

function TFPCustomCanvas.CreateDefaultFont : TFPCustomFont;
begin
  result := DoCreateDefaultFont;
  if not assigned (result) then
    raise TFPCanvasException.CreateFmt (ErrCouldNotCreate, [EFont])
  else
    begin
    result.AllocateResources (self);
    FHelpers.Add (result);
    end;
end;

function TFPCustomCanvas.CreateDefaultPen : TFPCustomPen;
begin
  result := DoCreateDefaultPen;
  if not assigned (result) then
    raise TFPCanvasException.CreateFmt (ErrCouldNotCreate, [EPen])
  else
    begin
    result.AllocateResources (self);
    FHelpers.Add (result);
    end;
end;

function TFPCustomCanvas.CreateDefaultBrush : TFPCustomBrush;
begin
  result := DoCreateDefaultBrush;
  if not assigned (result) then
    raise TFPCanvasException.CreateFmt (ErrCouldNotCreate, [EBrush])
  else
    begin
    result.AllocateResources (self);
    FHelpers.Add (result);
    end;
end;

function TFPCustomCanvas.GetClipping: boolean;
begin
  Result:=FClipping;
end;

function TFPCustomCanvas.GetClipRect: TRect;
begin
  if FClipRegion = nil then Result := Bounds(0, 0, 0, 0)
  else Result:=FClipRegion.GetBoundingRect();
end;

function TFPCustomCanvas.CreateFont : TFPCustomFont;
begin
  result := DoCreateDefaultFont;
end;

function TFPCustomCanvas.CreatePen : TFPCustomPen;
begin
  result := DoCreateDefaultPen;
end;

function TFPCustomCanvas.CreateBrush : TFPCustomBrush;
begin
  result := DoCreateDefaultBrush;
end;

function TFPCustomCanvas.AllowFont (AFont : TFPCustomFont) : boolean;
begin
  if AFont is TFPCustomDrawFont then
    result := true
  else
    result := DoAllowFont (AFont);
end;

procedure TFPCustomCanvas.SetFont (AValue:TFPCustomFont);
begin
  if (AValue <> FFont) and AllowFont(AValue) then
    begin
      if FManageResources then
        FFont.Assign(AValue)
      else
        begin
          AValue.AllocateResources (self);
          FFont := AValue;
          AddHelper (AValue);
        end;
    end;
end;

function TFPCustomCanvas.GetFont : TFPCustomFont;
begin
  if assigned (FFont) then
    result := FFont
  else
    result := FDefaultFont;
end;

function TFPCustomCanvas.DoAllowFont (AFont : TFPCustomFont) : boolean;
begin
  result := false;
end;

function TFPCustomCanvas.AllowBrush (ABrush : TFPCustomBrush) : boolean;
begin
  if ABrush is TFPCustomDrawBrush then
    result := true
  else
    result := DoAllowBrush (ABrush);
end;

procedure TFPCustomCanvas.SetBrush (AValue:TFPCustomBrush);
begin
  if (AValue <> FBrush) and AllowBrush(AValue) then
    begin
      if FManageResources then
        FBrush.Assign(AValue)
      else
        begin
          AValue.AllocateResources (self);
          FBrush := AValue;
          AddHelper (AValue);
        end;
    end;
end;

function TFPCustomCanvas.GetBrush : TFPCustomBrush;
begin
  if assigned (FBrush) then
    result := FBrush
  else
    result := FDefaultBrush
end;

function TFPCustomCanvas.DoAllowBrush (ABrush : TFPCustomBrush) : boolean;
begin
  result := false;
end;

function TFPCustomCanvas.AllowPen (APen : TFPCustomPen) : boolean;
begin
  if APen is TFPCustomDrawPen then
    result := true
  else
    result := DoAllowPen (APen);
end;

procedure TFPCustomCanvas.SetPen (AValue:TFPCustomPen);
begin
  if (AValue <> FPen) and AllowPen (AValue) then
    begin
      if FManageResources then
        FPen.Assign(AValue)
      else
        begin
          AValue.AllocateResources (self);
          FPen := AValue;
          AddHelper (AValue);
        end;
    end;
end;

function TFPCustomCanvas.GetPen : TFPCustomPen;
begin
  if assigned (FPen) then
    result := FPen
  else
    result := FDefaultPen;
end;

procedure TFPCustomCanvas.SetClipping(const AValue: boolean);
begin
  FClipping:=AValue;
end;

procedure TFPCustomCanvas.SetClipRect(const AValue: TRect);
var
  lNewRegion: TFPRectRegion;
begin
  lNewRegion := TFPRectRegion.Create;
  lNewRegion.Rect := AValue;
  if FClipRegion <> nil then FClipRegion.Free;
  FClipRegion := lNewRegion;
end;

procedure TFPCustomCanvas.SetPenPos(const AValue: TPoint);
begin
  FPenPos:=AValue;
end;

function TFPCustomCanvas.DoAllowPen (APen : TFPCustomPen) : boolean;
begin
  result := false;
end;

procedure TFPCustomCanvas.DoLockCanvas;
begin
end;

procedure TFPCustomCanvas.DoUnlockCanvas;
begin
end;

procedure TFPCustomCanvas.LockCanvas;
begin
  if FLocks = 0 then
    DoLockCanvas;
  inc (FLocks);
end;

procedure TFPCustomCanvas.UnlockCanvas;
begin
  if FLocks > 0 then
    begin
    dec (FLocks);
    if FLocks = 0 then
      DoUnlockCanvas;
    end
  else
    raise TFPCanvasException.Create (ErrNoLock);
end;

function TFPCustomCanvas.Locked: boolean;
begin
  Result:=FLocks>0;
end;

procedure TFPCustomCanvas.TextOut (x,y:integer;text:string);
begin
  if Font is TFPCustomDrawFont then
    TFPCustomDrawFont(Font).DrawText(x,y, text)
  else
    DoTextOut (x,y, text);
end;

procedure TFPCustomCanvas.GetTextSize (text:string; var w,h:integer);
begin
  if Font is TFPCustomDrawFont then
    TFPCustomDrawFont(Font).GetTextSize (text, w, h)
  else
    DoGetTextSize (Text, w, h);
end;

function TFPCustomCanvas.GetTextHeight (text:string) : integer;
begin
  Result := TextHeight(Text);
end;

function TFPCustomCanvas.GetTextWidth (text:string) : integer;
begin
  Result := TextWidth(Text);
end;

function TFPCustomCanvas.TextExtent(const Text: string): TSize;
begin
  GetTextSize(Text, Result.cx, Result.cy);
end;

function TFPCustomCanvas.TextHeight(const Text: string): Integer;
begin
  if Font is TFPCustomDrawFont then
    result := TFPCustomDrawFont(Font).GetTextHeight (text)
  else
    result := DoGetTextHeight (Text);
end;

function TFPCustomCanvas.TextWidth(const Text: string): Integer;
begin
  if Font is TFPCustomDrawFont then
    result := TFPCustomDrawFont(Font).GetTextWidth (text)
  else
    result := DoGetTextWidth (Text);
end;

{procedure TFPCustomCanvas.TextOut (x,y:integer;text:unicodestring);
begin
  if Font is TFPCustomDrawFont then
    TFPCustomDrawFont(Font).DrawText(x,y, text)
  else
    DoTextOut (x,y, text);
end;

procedure TFPCustomCanvas.GetTextSize (text:unicodestring; var w,h:integer);
begin
  if Font is TFPCustomDrawFont then
    TFPCustomDrawFont(Font).GetTextSize (text, w, h)
  else
    DoGetTextSize (Text, w, h);
end;

function TFPCustomCanvas.GetTextHeight (text:unicodestring) : integer;
begin
  Result := TextHeight(Text);
end;

function TFPCustomCanvas.GetTextWidth (text:unicodestring) : integer;
begin
  Result := TextWidth(Text);
end;

function TFPCustomCanvas.TextExtent(const Text: unicodestring): TSize;
begin
  GetTextSize(Text, Result.cx, Result.cy);
end;

function TFPCustomCanvas.TextHeight(const Text: unicodestring): Integer;
begin
  if Font is TFPCustomDrawFont then
    result := TFPCustomDrawFont(Font).GetTextHeight (text)
  else
    result := DoGetTextHeight (Text);
end;

function TFPCustomCanvas.TextWidth(const Text: unicodestring): Integer;
begin
  if Font is TFPCustomDrawFont then
    result := TFPCustomDrawFont(Font).GetTextWidth (text)
  else
    result := DoGetTextWidth (Text);
end;

procedure TFPCustomCanvas.DoTextOut (x,y:integer;text:unicodestring);

begin
  DoTextOut(x,y,string(text));
end;

procedure TFPCustomCanvas.DoGetTextSize (text:unicodestring; var w,h:integer);

begin
  DoGetTextSize(String(Text),w,h);
end;

function  TFPCustomCanvas.DoGetTextHeight (text:unicodestring) : integer;

begin
  Result:=DoGetTextHeight(String(text));
end;

function  TFPCustomCanvas.DoGetTextWidth (text:unicodestring) : integer;

begin
  Result:=DoGetTextWidth(String(text));
end;}

procedure TFPCustomCanvas.Arc(ALeft, ATop, ARight, ABottom, Angle16Deg,
  Angle16DegLength: Integer);
begin

end;

procedure TFPCustomCanvas.Arc(ALeft, ATop, ARight, ABottom, SX, SY, EX,
  EY: Integer);
begin

end;

procedure TFPCustomCanvas.DoMoveTo (x,y:integer);
begin
end;

procedure TFPCustomCanvas.DoLineTo (x,y:integer);
begin
  DoLine (FPenPos.X,FPenPos.y, x,y);
end;

procedure TFPCustomCanvas.MoveTo (x,y:integer);
begin
  FPenPos.x := x;
  FPenPos.y := y;
  DoMoveTo (x,y);
end;

procedure TFPCustomCanvas.MoveTo (p:TPoint);
begin
  FPenPos := p;
  DoMoveTo (p.x,p.y);
end;

procedure TFPCustomCanvas.LineTo (x,y:integer);
begin
  if Pen.Style <> psClear then
    if Pen is TFPCustomDrawPen then
      TFPCustomDrawPen(Pen).DrawLine (FPenPos.x, FPenPos.y, x, y)
    else
      DoLineTo (x,y);
  FPenPos.x := x;
  FPenPos.y := y;
end;

procedure TFPCustomCanvas.LineTo (p:TPoint);
begin
  LineTo (p.x, p.y);
end;

procedure TFPCustomCanvas.Line (x1,y1,x2,y2:integer);
begin
  if Pen.Style <> psClear then
    if Pen is TFPCustomDrawPen then
      TFPCustomDrawPen(Pen).DrawLine (x1,y1, x2,y2)
    else
      DoLine (x1,y1, x2,y2);
  FPenPos.x := x2;
  FPenPos.y := y2;
end;

procedure TFPCustomCanvas.Line (const p1,p2:TPoint);
begin
  Line (p1.x,p1.y,p2.x,p2.y);
end;

procedure TFPCustomCanvas.Line (const points:TRect);
begin
  with points do
    Line (left,top, right,bottom);
end;

procedure TFPCustomCanvas.Polyline (Const points:array of TPoint);
begin
  if Pen.Style <> psClear then
   if Pen is TFPCustomDrawPen then
     TFPCustomDrawPen(Pen).Polyline (points,false)
   else
     DoPolyline (points);
  FPenPos := points[high(points)];
end;

procedure TFPCustomCanvas.RadialPie(x1, y1, x2, y2, StartAngle16Deg, Angle16DegLength: Integer);

begin
  DoRadialPie(X1, y1, x2, y2, StartAngle16Deg, Angle16DegLength);
end;

procedure TFPCustomCanvas.DoRadialPie(x1, y1, x2, y2, StartAngle16Deg, Angle16DegLength: Integer);

begin
  // To be implemented
end;

procedure TFPCustomCanvas.DoPolyBezier(Points: PPoint; NumPts: Integer;
                           Filled: boolean = False;
                           Continuous: boolean = False);

begin
 // To be implemented
end;

procedure TFPCustomCanvas.PolyBezier(Points: PPoint; NumPts: Integer;
                     Filled: boolean = False;
                     Continuous: boolean = False);
begin
  DoPolyBezier(Points,NumPts,Filled,Continuous);
end;

procedure TFPCustomCanvas.PolyBezier(const Points: array of TPoint;
                     Filled: boolean = False;
                     Continuous: boolean = False);
var
  NPoints{, i}: integer;
//  PointArray: ^TPoint;
begin
  NPoints:=High(Points)-Low(Points)+1;
  if NPoints>0 then
    DoPolyBezier(@Points[Low(Points)],NPoints,Filled,Continuous);
{
  NPoints:=High(Points)-Low(Points)+1;
  if NPoints<=0 then exit;
    GetMem(PointArray,SizeOf(TPoint)*NPoints);
  try
    for i:=0 to NPoints-1 do
      PointArray[i]:=Points[i+Low(Points)];
    DoPolyBezier(PointArray, NPoints, Filled, Continuous);
  finally
    FreeMem(PointArray);
  end;}
end;

procedure TFPCustomCanvas.Clear;
var r : TRect;
begin
  if Brush.Style <> bsClear then
    begin
    if Brush is TFPCustomDrawBrush then
      TFPCustomDrawBrush(Brush).Rectangle(0,0, width, height)
    else
      begin
      r := Rect(0,0, width, height);
      DoRectangleFill (r);
      end;
    end;
end;

procedure TFPCustomCanvas.Erase;
var
  x,y:Integer;
begin
  for x:=0 to Width-1 do
    for y:=0 to Height-1 do
      Colors[x,y]:=colTransparent;
end;

procedure TFPCustomCanvas.DoRectangleAndFill (const Bounds:TRect);
begin
  DoRectangleFill (Bounds);
  DoRectangle (Bounds);
end;

procedure TFPCustomCanvas.DoEllipseAndFill (const Bounds:TRect);
begin
  DoEllipseFill (Bounds);
  DoEllipse (Bounds);
end;

procedure TFPCustomCanvas.DoPolygonAndFill (const points:array of TPoint);
begin
  DoPolygonFill (points);
  DoPolygon (points);
end;

procedure TFPCustomCanvas.Ellipse (const Bounds:TRect);
var p,b,dp,db,pb : boolean;
begin
  p := Pen.style <> psClear;
  b := Brush.style <> bsClear;
  pb := false;
  dp:=False;
  db:=False;
  if p and (Pen is TFPCustomDrawPen) then
      begin
      p := false;
      dp := true;
      end;
  if b and (Brush is TFPCustomDrawBrush) then
      begin
      b := false;
      db := true;
      end;
  if p and b then
    begin
    p := false;
    b := false;
    pb := true;
    end;
  if pb then
    DoEllipseAndFill (bounds)
  else
    begin
    if p then
      DoEllipse (bounds)
    else if dp then
      with bounds do
        TFPCustomDrawPen(Pen).Ellipse (left,top,right,bottom);
    if b then
      DoEllipseFill (bounds)
    else if db then
      with bounds do
        TFPCustomDrawBrush(Brush).Ellipse (left,top,right,bottom);
    end;
end;

procedure TFPCustomCanvas.Ellipse (left,top,right,bottom:integer);
begin
  Ellipse (Rect(left,top,right,bottom));
end;

procedure TFPCustomCanvas.EllipseC (x,y:integer; rx,ry:longword);
begin
  Ellipse (Rect(x-rx,y-ry,x+rx,y+ry));
end;

procedure TFPCustomCanvas.Rectangle (left,top,right,bottom:integer);
begin
  Rectangle (Rect(left,top,right,bottom));
end;

procedure TFPCustomCanvas.FillRect(const ARect: TRect);

begin
  if (Brush.style <> bsClear) then
    begin
    if not (brush is TFPCustomDrawBrush) then
      DoRectangleFill (ARect)
    else
      with ARect do
        TFPCustomDrawBrush(Brush).Rectangle (left,top,right,bottom);
    end;
end;

procedure TFPCustomCanvas.FillRect(X1,Y1,X2,Y2: Integer);

begin
  FillRect (Rect(X1,Y1,X2,Y2));
end;

procedure TFPCustomCanvas.Rectangle (const Bounds:TRect);
var np,nb,dp,db,pb : boolean;
begin
  np:= Pen.style <> psClear; // Need pen ?
  nb:= Brush.style <> bsClear;  // Need brush ?
  dp:=(pen is TFPCustomDrawPen); // Pen draws ?
  db:=(brush is TFPCustomDrawBrush); // Brush draws ?
  if (np and nb) and not (db or dp) then
    DoRectangleAndFill (bounds)
  else
    begin
    if np then
      begin
      If not dp then
        DoRectangle (bounds)
      else
        with bounds do
          TFPCustomDrawPen(Pen).Rectangle (left,top,right,bottom);
      end;
    if Nb then
      begin
      if not db then
        DoRectangleFill (bounds)
      else
        with bounds do
          TFPCustomDrawBrush(Brush).Rectangle (left,top,right,bottom);
      end;
    end;
end;

procedure TFPCustomCanvas.FloodFill (x,y:integer);
begin
  if Brush.Style <> bsClear then
    begin
    if Brush is TFPCustomDrawBrush then
      TFPCustomDrawBrush (Brush).FloodFill (x,y)
    else
      DoFloodFill (x,y);
    end;
end;

procedure TFPCustomCanvas.Polygon (const points:array of TPoint);
var p,b,dp,db,pb : boolean;
begin
  p := Pen.style <> psClear;
  b := Brush.style <> bsClear;
  dp:=false;
  db:=false;
  pb:=False;
  if p and (pen is TFPCustomDrawPen) then
      begin
      p := false;
      dp := true;
      end;
  if b and (brush is TFPCustomDrawBrush) then
      begin
      b := false;
      db := true;
      end;
  if p and b then
    begin
    p := false;
    b := false;
    pb := true;
    end;
  if pb then
    DoPolygonAndFill (points)
  else
    begin
    if p then
      DoPolygon (points)
    else if dp then
      TFPCustomDrawPen(Pen).Polyline (points, true);
    if b then
      DoPolygonFill (points)
    else if db then
      TFPCustomDrawBrush(Brush).Polygon (points);
    end;
end;

procedure TFPCustomCanvas.CopyRect (x,y:integer; canvas:TFPCustomCanvas;
  SourceRect:TRect);
var yy,r,t : integer;
begin
//#  SortRect (SourceRect);
  with SourceRect do begin
    for t := top to bottom do begin
      yy := t - top + y;
      for r := left to right do
        colors[r - left + x,yy] := canvas.colors[r,t];
    end;
  end;
end;

procedure TFPCustomCanvas.Draw (x,y:integer; image:TFPCustomImage);
var xx,xi,yi,xm,ym,r,t : integer;
begin
  xm := x + image.width-1;
  if xm >= width then
    xm := width - 1;
  ym := y + image.height-1;
  if ym >= height then
    ym := height - 1;
  xi := x;
  yi := y;
//#  if clipping then
//#    CheckRectClipping (ClipRect, xi,yi, xm,ym);
  for r := xi to xm do
    begin
    xx := r - x;
    for t := yi to ym do
      colors [r,t] := image.colors[xx,t-y];
    end;
end;

procedure TFPCustomCanvas.StretchDraw(x, y, w, h: integer; source: TFPCustomImage);
var i : TFPCustomInterpolation;
    FreeInterpolation : boolean;
    IP : TFPCustomInterpolation;
begin
  FreeInterpolation := not assigned (FInterpolation);
  if FreeInterpolation then
    IP := TMitchelInterpolation.Create
  else
    IP := FInterpolation;
  try
    with IP do
      begin
      Initialize (source, self);
      Execute (x,y,w,h);
      end;
  finally
    if FreeInterpolation then
      IP.Free;
  end;
end;

{ TFPCustomDrawPen }

procedure TFPCustomDrawPen.DrawLine (x1,y1,x2,y2:integer);
begin
  DoDrawLine (x1,y1,x2,y2);
end;

procedure TFPCustomDrawPen.Polyline (const points:array of TPoint; close:boolean);
begin
  DoPolyLine (points, false);
end;

procedure TFPCustomDrawPen.Ellipse (left,top, right,bottom:integer);
begin
  DoEllipse (left,top,right,bottom);
end;

procedure TFPCustomDrawPen.Rectangle (left,top, right,bottom:integer);
begin
  DoRectangle (left,top,right,bottom);
end;

{ TFPCustomDrawBrush }

procedure TFPCustomDrawBrush.Rectangle (left,top,right,bottom:integer);
begin
  DoRectangle (left,top,right,bottom);
end;

procedure TFPCustomDrawBrush.FloodFill (x,y:integer);
begin
  DoFloodFill (x,y);
end;

procedure TFPCustomDrawBrush.Ellipse (left,top, right,bottom:integer);
begin
  DoEllipse (left,top,right,bottom);
end;

procedure TFPCustomDrawBrush.Polygon (const points:array of TPoint);
begin
  DoPolygon (points);
end;

{ TFPCustomDrawFont }

procedure TFPCustomDrawFont.DrawText (x,y:integer; text:string);
begin
  DoDrawText (x,y, text);
end;

procedure TFPCustomDrawFont.GetTextSize (text:string; var w,h:integer);
begin
  DoGetTextSize (text, w,h);
end;

function TFPCustomDrawFont.GetTextHeight (text:string) : integer;
begin
  result := DoGetTextHeight (Text);
end;

function TFPCustomDrawFont.GetTextWidth (text:string) : integer;
begin
  result := DoGetTextWidth (Text);
end;

{procedure TFPCustomDrawFont.DrawText (x,y:integer; text:UnicodeString);
begin
  DoDrawText (x,y, text);
end;

procedure TFPCustomDrawFont.GetTextSize (text:UnicodeString; var w,h:integer);
begin
  DoGetTextSize (text, w,h);
end;

function TFPCustomDrawFont.GetTextHeight (text:UnicodeString) : integer;
begin
  result := DoGetTextHeight (Text);
end;

function TFPCustomDrawFont.GetTextWidth (text:UnicodeString) : integer;
begin
  result := DoGetTextWidth (Text);
end;

procedure TFPCustomDrawFont.DoDrawText (x,y:integer; text:unicodestring);

begin
  DoDrawText(x,y,String(text));
end;

procedure TFPCustomDrawFont.DoGetTextSize (text: unicodestring; var w,h:integer);

begin
  DoGetTextSize(String(text),w,h);
end;

function TFPCustomDrawFont.DoGetTextHeight (text: unicodestring) : integer;

begin
  Result:=DoGetTextHeight(String(text));
end;

function TFPCustomDrawFont.DoGetTextWidth (text: unicodestring) : integer;

begin
  Result:=DoGetTextWidth(String(text));
end;
}

end.
