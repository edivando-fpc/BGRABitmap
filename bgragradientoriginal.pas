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


Unit BGRAGradientOriginal;

{$i bgrabitmap.inc}{$H+}

interface

uses
  Classes, SysUtils, BGRATypes, {$IFNDEF FPC}Types, GraphType,{$ENDIF} BGRALayerOriginal, BGRABitmap, BGRABitmapTypes, BGRAGradientScanner;

type
  TBGRAColorInterpolation = BGRAGradientScanner.TBGRAColorInterpolation;
  TBGRAGradientRepetition = BGRAGradientScanner.TBGRAGradientRepetition;

  { TBGRALayerGradientOriginal }

  TBGRALayerGradientOriginal = class(TBGRALayerCustomOriginal)
  private
    procedure SetColorInterpoaltion(AValue: TBGRAColorInterpolation);
    procedure SetEndColor(AValue: TBGRAPixel);
    procedure SetFocalPoint(AValue: TPointF);
    procedure SetFocalRadius(AValue: Single);
    procedure SetGradientType(AValue: TGradientType);
    procedure SetOrigin(AValue: TPointF);
    procedure SetRadius(AValue: Single);
    procedure SetRepetition(AValue: TBGRAGradientRepetition);
    procedure SetStartColor(AValue: TBGRAPixel);
    procedure SetXAxis(AValue: TPointF);
    procedure SetYAxis(AValue: TPointF);
  protected
    FStartColor,FEndColor: TBGRAPixel;
    FGradientType: TGradientType;
    FOrigin,FXAxis,FYAxis,FFocalPoint: TPointF;
    FRadius,FFocalRadius: single;
    FColorInterpolation: TBGRAColorInterpolation;
    FRepetition: TBGRAGradientRepetition;
    function GetComputedRadius: single;
    function GetComputedYAxis: TPointF;
    function GetComputedFocalPoint: TPointF;
    function GetComputedFocalRadius: single;
    procedure OnMoveOrigin({%H-}ASender: TObject; {%H-}APrevCoord, ANewCoord: TPointF);
    procedure OnMoveXAxis({%H-}ASender: TObject; {%H-}APrevCoord, ANewCoord: TPointF);
    procedure OnMoveYAxis({%H-}ASender: TObject; {%H-}APrevCoord, ANewCoord: TPointF);
    procedure OnMoveFocalPoint({%H-}ASender: TObject; {%H-}APrevCoord, ANewCoord: TPointF);
    procedure OnMoveFocalRadius({%H-}ASender: TObject; {%H-}APrevCoord, ANewCoord: TPointF);
  public
    constructor Create; override;
    procedure Render(ADest: TBGRABitmap; AMatrix: TAffineMatrix; ADraft: boolean); override;
    procedure ConfigureEditor(AEditor: TBGRAOriginalEditor); override;
    function GetRenderBounds(ADestRect: TRect; {%H-}AMatrix: TAffineMatrix): TRect; override;
    procedure LoadFromStorage(AStorage: TBGRACustomOriginalStorage); override;
    procedure SaveToStorage(AStorage: TBGRACustomOriginalStorage); override;
    class function StorageClassName: RawByteString; override;
    property ComputedYAxis: TPointF read GetComputedYAxis;
    property ComputedRadius: single read GetComputedRadius;
    property ComputedFocalPoint: TPointF read GetComputedFocalPoint;
    property ComputedFocalRadius: single read GetComputedFocalRadius;

    property StartColor: TBGRAPixel read FStartColor write SetStartColor;
    property EndColor: TBGRAPixel read FEndColor write SetEndColor;
    property GradientType: TGradientType read FGradientType write SetGradientType;   //default gtLinear
    property Origin: TPointF read FOrigin write SetOrigin;
    property XAxis: TPointF read FXAxis write SetXAxis;
    property YAxis: TPointF read FYAxis write SetYAxis;
    property FocalPoint: TPointF read FFocalPoint write SetFocalPoint;     //default Origin
    property Radius: Single read FRadius write SetRadius;                  //default 1
    property FocalRadius: Single read FFocalRadius write SetFocalRadius;   //default 0
    property ColorInterpolation: TBGRAColorInterpolation read FColorInterpolation write SetColorInterpoaltion;
    property Repetition: TBGRAGradientRepetition read FRepetition write SetRepetition;

  end;

implementation

uses BGRATransform, StrUtils;

{ TBGRALayerGradientOriginal }

function TBGRALayerGradientOriginal.GetComputedRadius: single;
begin
  if FRadius = EmptySingle then result := 1 else result := FRadius;
end;

procedure TBGRALayerGradientOriginal.SetColorInterpoaltion(
  AValue: TBGRAColorInterpolation);
begin
  if FColorInterpolation=AValue then Exit;
  FColorInterpolation:=AValue;
  NotifyChange;
end;

procedure TBGRALayerGradientOriginal.SetEndColor(AValue: TBGRAPixel);
begin
  if BGRAPixelEqual(FEndColor,{=}AValue) then Exit;
  FEndColor:=AValue;
  NotifyChange;
end;

procedure TBGRALayerGradientOriginal.SetFocalPoint(AValue: TPointF);
begin
  if FFocalPoint=AValue then Exit;
  FFocalPoint:=AValue;
  NotifyChange;
end;

procedure TBGRALayerGradientOriginal.SetFocalRadius(AValue: Single);
begin
  if FFocalRadius=AValue then Exit;
  FFocalRadius:=AValue;
  NotifyChange;
end;

procedure TBGRALayerGradientOriginal.SetGradientType(AValue: TGradientType);
begin
  if FGradientType=AValue then Exit;
  FGradientType:=AValue;
  if FGradientType in [gtLinear,gtReflected] then FYAxis := EmptyPointF;
  NotifyChange;
end;

procedure TBGRALayerGradientOriginal.SetOrigin(AValue: TPointF);
begin
  if FOrigin=AValue then Exit;
  FOrigin:=AValue;
  NotifyChange;
end;

procedure TBGRALayerGradientOriginal.SetRadius(AValue: Single);
begin
  if FRadius=AValue then Exit;
  FRadius:=AValue;
  NotifyChange;
end;

procedure TBGRALayerGradientOriginal.SetRepetition(
  AValue: TBGRAGradientRepetition);
begin
  if FRepetition=AValue then Exit;
  FRepetition:=AValue;
  NotifyChange;
end;

procedure TBGRALayerGradientOriginal.SetStartColor(AValue: TBGRAPixel);
begin
  if BGRAPixelEqual(FStartColor,{=}AValue) then Exit;
  FStartColor:=AValue;
  NotifyChange;
end;

procedure TBGRALayerGradientOriginal.SetXAxis(AValue: TPointF);
begin
  if FXAxis=AValue then Exit;
  FXAxis:=AValue;
  NotifyChange;
end;

procedure TBGRALayerGradientOriginal.SetYAxis(AValue: TPointF);
begin
  if FYAxis=AValue then Exit;
  FYAxis:=AValue;
  NotifyChange;
end;

function TBGRALayerGradientOriginal.GetComputedYAxis: TPointF;
var
  u: TPointF;
begin
  if isEmptyPointF(FYAxis) then
  begin
    u := FXAxis - FOrigin;
    result := FOrigin + PointF(-u.y,u.x)
  end
  else
    result := FYAxis;
end;

function TBGRALayerGradientOriginal.GetComputedFocalPoint: TPointF;
begin
  if isEmptyPointF(FFocalPoint) then result := FOrigin else result := FFocalPoint;
end;

function TBGRALayerGradientOriginal.GetComputedFocalRadius: single;
begin
  if FFocalRadius = EmptySingle then result := 0 else result := FFocalRadius;
end;

procedure TBGRALayerGradientOriginal.OnMoveOrigin(ASender: TObject; APrevCoord,
  ANewCoord: TPointF);
var
  delta: TPointF;
begin
  delta := ANewCoord-FOrigin;
  FOrigin := ANewCoord;
  if not isEmptyPointF(FXAxis) then FXAxis := FXAxis +delta;
  if not isEmptyPointF(FYAxis) then FYAxis := FYAxis +delta;
  if not isEmptyPointF(FFocalPoint) then FFocalPoint := FFocalPoint +delta;
  NotifyChange;
end;

procedure TBGRALayerGradientOriginal.OnMoveXAxis(ASender: TObject; APrevCoord,
  ANewCoord: TPointF);
begin
  XAxis := ANewCoord;
end;

procedure TBGRALayerGradientOriginal.OnMoveYAxis(ASender: TObject; APrevCoord,
  ANewCoord: TPointF);
begin
  YAxis := ANewCoord;
end;

procedure TBGRALayerGradientOriginal.OnMoveFocalPoint(ASender: TObject;
  APrevCoord, ANewCoord: TPointF);
begin
  FocalPoint := ANewCoord;
end;

procedure TBGRALayerGradientOriginal.OnMoveFocalRadius(ASender: TObject;
  APrevCoord, ANewCoord: TPointF);
var refLen: single;
  u, focalOrig: TPointF;
begin
  focalOrig := ComputedFocalPoint;
  if isEmptyPointF(focalOrig) or isEmptyPointF(FOrigin) or isEmptyPointF(FXAxis) then exit;
  refLen := VectLen(FOrigin-FXAxis);
  if refLen = 0 then exit;

  u := (FOrigin-FXAxis)*(1/refLen);
  FFocalRadius := u * (ANewCoord-focalOrig) / refLen - 0.1;
  if FFocalRadius < 0 then FFocalRadius:= 0;
  NotifyChange;
end;

constructor TBGRALayerGradientOriginal.Create;
begin
  inherited Create;
  FStartColor := BGRABlack;
  FEndColor := BGRAWhite;
  FGradientType := gtLinear;
  FColorInterpolation:= ciStdRGB;
  FRepetition := grPad;
  FRadius := EmptySingle;
  FFocalRadius := EmptySingle;
  FFocalPoint := EmptyPointF;
  FOrigin := PointF(0,0);
  FXAxis := EmptyPointF;
  FYAxis := EmptyPointF;
end;

procedure TBGRALayerGradientOriginal.Render(ADest: TBGRABitmap;
  AMatrix: TAffineMatrix; ADraft: boolean);
var
  colors: TBGRACustomGradient;
  grad: TBGRAGradientScanner;
  dither: TDitheringAlgorithm;
begin
  if isEmptyPointF(FOrigin) or isEmptyPointF(FXAxis) then exit;

  colors := TBGRASimpleGradient.CreateAny(FColorInterpolation, FStartColor,FEndColor, FRepetition);

  if FGradientType = gtRadial then
  begin
    grad := TBGRAGradientScanner.Create(FOrigin,FXAxis,ComputedYAxis,ComputedFocalPoint,ComputedRadius,ComputedFocalRadius);
  end else
    grad := TBGRAGradientScanner.Create(FGradientType, FOrigin,FXAxis,ComputedYAxis);

  grad.SetGradient(colors, false);
  grad.Transform := AMatrix;

  if ADraft then dither := daNearestNeighbor else dither := daFloydSteinberg;
  ADest.FillRect(ADest.ClipRect, grad,dmSet, dither);
  grad.Free;
  colors.Free;
end;

procedure TBGRALayerGradientOriginal.ConfigureEditor(
  AEditor: TBGRAOriginalEditor);
var
  originPoint: Integer;
begin
  if not isEmptyPointF(FOrigin) then
  begin
    originPoint := AEditor.AddPoint(FOrigin, {$IFDEF OBJ}@{$ENDIF}OnMoveOrigin);
    if not isEmptyPointF(FXAxis) then
    begin
      AEditor.AddArrow(FOrigin, FXAxis, {$IFDEF OBJ}@{$ENDIF}OnMoveXAxis);
      if FGradientType in[gtDiamond, gtRadial] then
        AEditor.AddArrow(FOrigin, ComputedYAxis, {$IFDEF OBJ}@{$ENDIF}OnMoveYAxis);
    end;
    if FGradientType = gtRadial then
    begin
      AEditor.AddPoint(ComputedFocalPoint, {$IFDEF OBJ}@{$ENDIF}OnMoveFocalPoint, true, originPoint);
      AEditor.AddArrow(ComputedFocalPoint, ComputedFocalPoint - (FXAxis - FOrigin) * (ComputedFocalRadius + 0.1), {$IFDEF OBJ}@{$ENDIF}OnMoveFocalRadius, true);
    end;
  end;
end;

function TBGRALayerGradientOriginal.GetRenderBounds(ADestRect: TRect;
  AMatrix: TAffineMatrix): TRect;
begin
  result := ADestRect;
end;

procedure TBGRALayerGradientOriginal.LoadFromStorage(
  AStorage: TBGRACustomOriginalStorage);
var
  colorArray: ArrayOfTBGRAPixel;
begin
  colorArray := AStorage.ColorArray['colors'];

  FStartColor := colorArray[0];
  FEndColor := colorArray[high(colorArray)];

  case AnsiIndexStr(LowerCase(AStorage.RawString['gradient-type']), ['reflected', 'radial', 'diamond']) of
  00: FGradientType := gtReflected;
  01: FGradientType := gtRadial;
  02: FGradientType := gtDiamond;
  else {'linear'} FGradientType := gtLinear;
  end;

  FOrigin := AStorage.PointF['origin'];
  FXAxis := AStorage.PointF['x-axis'];
  FYAxis := AStorage.PointF['y-axis'];
  FFocalPoint := AStorage.PointF['focal-point'];

  FRadius := AStorage.Float['radial'];
  FFocalRadius := AStorage.Float['focal-radius'];

  case AnsiIndexStr(UpperCase(AStorage.RawString['color-interpolation']), ['RGB', 'HSL+', 'HSL-', 'GSB+', 'GSB-']) of
  00: FColorInterpolation:= ciLinearRGB;
  01: FColorInterpolation:= ciLinearHSLPositive;
  02: FColorInterpolation:= ciLinearHSLNegative;
  03: FColorInterpolation:= ciGSBPositive;
  04: FColorInterpolation:= ciGSBNegative;
  else {'sRGB'} FColorInterpolation:= ciStdRGB;
  end;

  case AnsiIndexStr(LowerCase(AStorage.RawString['repetition']), ['repeat','reflect', 'sine']) of
  00: FRepetition:= grRepeat;
  01: FRepetition:= grReflect;
  02: FRepetition := grSine;
  else {'pad'} FRepetition:= grPad;
  end;
end;

procedure TBGRALayerGradientOriginal.SaveToStorage(
  AStorage: TBGRACustomOriginalStorage);
var
  gtStr, ciStr: String;
  colorArray: ArrayOfTBGRAPixel;
begin
  setlength(colorArray,2);
  colorArray[0] := FStartColor;
  colorArray[1] := FEndColor;
  AStorage.ColorArray['colors'] := colorArray;

  case FGradientType of
  gtReflected: gtStr := 'reflected';
  gtRadial: gtStr := 'radial';
  gtDiamond: gtStr := 'diamond';
  else {gtLinear} gtStr := 'linear';
  end;
  AStorage.RawString['gradient-type'] := gtStr;

  AStorage.PointF['origin'] := FOrigin;
  AStorage.PointF['x-axis'] := FXAxis;

  if FGradientType in[gtRadial,gtDiamond] then
    AStorage.PointF['y-axis'] := FYAxis
  else
    AStorage.RemoveAttribute('y-axis');

  if FGradientType = gtRadial then
  begin
    AStorage.Float['radius'] := FRadius;
    AStorage.Float['focal-radius'] := FFocalRadius;
    AStorage.PointF['focal-point'] := FFocalPoint;
  end else
  begin
    AStorage.RemoveAttribute('radius');
    AStorage.RemoveAttribute('focal-radius');
  end;

  case FColorInterpolation of
  ciLinearRGB: ciStr := 'RGB';
  ciLinearHSLPositive: ciStr := 'HSL+';
  ciLinearHSLNegative: ciStr := 'HSL-';
  ciGSBPositive: ciStr := 'GSB+';
  ciGSBNegative: ciStr := 'GSB-';
  else {ciStdRGB} ciStr := 'sRGB';
  end;
  AStorage.RawString['color-interpolation'] := ciStr;

  case FRepetition of
  grRepeat: AStorage.RawString['repeat'] := 'repeat';
  grReflect: AStorage.RawString['repeat'] := 'reflect';
  grSine: AStorage.RawString['repeat'] := 'sine';
  else {grPad} AStorage.RawString['repeat'] := 'pad';
  end;
end;

class function TBGRALayerGradientOriginal.StorageClassName: RawByteString;
begin
  result := 'gradient';
end;

initialization

  RegisterLayerOriginal(TBGRALayerGradientOriginal);

end.
