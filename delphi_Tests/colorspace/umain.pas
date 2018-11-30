unit umain;

interface

uses
  Windows, Classes, SysUtils, Types, Forms, Controls, Graphics, Dialogs, Math,
  BGRAGraphics, FPImage, BGRABitmap, BGRATypes, BGRABitmapTypes, BCTypes, Vcl.ExtCtrls,
  GraphType, BCBaseCtrls, BCTools;

type
  TBGRAShapeType = (stRegularPolygon, stEllipse);

  { TForm1 }

  TForm1 = class(TForm)
    Shape1: TShape;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { private declarations }
    FBorderColor: TColor;
    FBorderOpacity: byte;
    FBorderStyle: TPenStyle;
    FBorderWidth: integer;
    FBorderGradient: TBCGradient;
    FUseBorderGradient: boolean;
    FFillColor:  TColor;
    FFillOpacity: byte;
    FFillGradient: TBCGradient;
    FUseFillGradient: boolean;
    FRoundRadius: integer;
    FSideCount:  integer;
    FRatioXY:    single;
    FUseRatioXY: boolean;
    FAngle:      single;
    FShapeType:  TBGRAShapeType;
    bresize   : boolean;
  public
    { public declarations }
    itot : integer;
    procedure DrawEllipseHello(bmp: TBGRABitmap);
    procedure DrawShape(bmp: TBGRABitmap);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.DrawEllipseHello(bmp: TBGRABitmap);
var br: TBGRACustomBitmap;
 i, i2 : integer;
begin
  bmp.Fill(BGRAPixelTransparent);

  RenderText(rECT(0,0, bmp.Width, bmp.Height), TBCFont(sELF.Font), 'Hello world', bmp);

//  bmp.TextOut(bmp.Width/2,bmp.Height/2 - (bmp.FontPixelMetric.CapLine+bmp.FontPixelMetric.Baseline)/2,
//      'Hello world', BGRABlack, taCenter);
  exit;
  bmp.CustomPenStyle := BGRAPenStyle(2,1);
  bmp.FillEllipseLinearColorAntialias(bmp.Width/2,bmp.Height/2,bmp.Width/2-5,bmp.Height/2-5, BGRAPixelTransparent, BGRAWhite);
    bmp.EllipseAntialias(bmp.Width/2,bmp.Height/2,bmp.Width/2-5,bmp.Height/2-5,CSSRed,5);
  EXIT;
  if bmp.Height div 10 < 10 then
    bmp.FontHeight := 10
  else
    bmp.FontHeight := bmp.Height div 10;
  i  := bmp.FontPixelMetric.CapLine;
  i2 := bmp.FontPixelMetric.Baseline;
  bmp.TextOut(bmp.Width/2,bmp.Height/2 - (bmp.FontPixelMetric.CapLine+bmp.FontPixelMetric.Baseline)/2,'Hello world', BGRABlack, taCenter);
  bmp.Canvas.Pen.Color := clBlue;
  bmp.Canvas.MoveTo(0,0);
  bmp.Canvas.LineTo(bmp.Width,bmp.Height);
  br := bmp.CreateBrushTexture(bsDiagCross, CSSYellow,CSSRed);
  bmp.FillPieInRect(rect(10,10,100,100),0,3*Pi/2,br);
  bmp.TextOutAngle(50,50, -300, 'Test angle', CSSGreen, taLeftJustify);
  br.Free;
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  itot := 0;
  bresize := False;
  FBorderColor := clWindowText;
  FBorderOpacity := 255;
  FBorderWidth := 1;
  FBorderStyle := psSolid;
  FBorderGradient := TBCGradient.Create(Self);
  FBorderGradient.Point2XPercent := 100;
  FBorderGradient.StartColor := clWhite;
  FBorderGradient.EndColor := clBlack;

  FFillColor := clWindow;
  FFillOpacity := 255;
  FFillGradient := TBCGradient.Create(Self);

  FRoundRadius := 0;
  FSideCount := 4;
  FRatioXY := 1;
  FUseRatioXY := False;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  if bresize then
    invalidate;
end;


procedure TForm1.FormPaint(Sender: TObject);
var
  bmp: TBGRABitmap;
  i, i2 : integer;
  ARect : TRect;
begin
//  Canvas.Brush.Color := clBlue;
//  Canvas.Ellipse(0,0,ClientWidth,ClientHeight);
//  LockWindowUpdate(Handle);
  i   := Width;
  i2  := Height;
  ARect := Rect(100, 100, 200, 200);

  bmp := TBGRABitmap.Create(i, i2, BGRAPixelTransparent);
  DrawEllipseHello(bmp);
  bmp.Draw(Canvas,0,0, False);
//  DrawShape(bmp);
//  bmp.Draw(Canvas, 0, 0, True);
  bmp.Free;
//  LockWindowUpdate(0);
  bresize := true;
end;

procedure TForm1.DrawShape(bmp: TBGRABitmap);
var
  cx, cy, rx, ry, curRatio, a, see, coo: single;
  coords: array of TPointF;
  pii : extended;
  p1, p2, minCoord, maxCoord, TempPoint: TPointF;
  i, i2: integer;
  borderGrad, fillGrad: TBGRACustomScanner;
  Temp : TBGRAPixel;
begin
  bmp.FillTransparent;
  bmp.PenStyle := FBorderStyle;
  begin
    bmp.Canvas2D.lineJoin := 'round';
    {if FUseBorderGradient then
    begin
      borderGrad := BCTools.CreateGradient(FBorderGradient, Classes.rect(0, 0, Width, Height));
      bmp.Canvas2D.strokeStyle(borderGrad);
    end
    else}
    begin
      borderGrad := nil;
//      bmp.Canvas2D.strokeStyle(ColorToBGRA(ColorToRGB(FBorderColor), FBorderOpacity));
    end;
    bmp.Canvas2D.lineStyle(bmp.CustomPenStyle);
    bmp.Canvas2D.lineWidth := FBorderWidth;
    {if FUseFillGradient then
    begin
      fillGrad := CreateGradient(FFillGradient, Classes.rect(0, 0, Width, Height));
      bmp.Canvas2D.fillStyle(fillGrad);
    end
    else}
    begin
      fillGrad := nil;
//      Temp := ColorToBGRA(ColorToRGB(clred),  100);
      Temp.blue  := 0;
      Temp.red   := 255;
      Temp.green := 0;
      Temp.alpha := 100;
      bmp.Canvas2D.fillStyle(Temp);
    end;
    cx := Width / 2;
    cy := Height / 2;
    rx := (Width - FBorderWidth) / 2;
    ry := (Height - FBorderWidth) / 2;
    if FUseRatioXY and (ry <> 0) and (FRatioXY <> 0) then
    begin
      {curRatio := rx / ry;
      if FRatioXY > curRatio then
        ry := ry / (FRatioXY / curRatio)
      else
        rx := rx / (curRatio / FRatioXY);}
    end;
    if FShapeType = stRegularPolygon then
    begin
      setlength(coords, FSideCount);
      for i := 0 to high(coords) do
      begin
        pii := roundto(Pi,-8);
        a := roundto((i / FSideCount + FAngle / 360) * 2 * Pii, -8);
        see := roundto(sin(a),-8);
        coo := roundto(cos(a),-8);
        coords[i] := PointF(see, -coo);
      end;
      minCoord := coords[0];
      maxCoord := coords[0];
      for i := 1 to high(coords) do
      begin
        if coords[i].x < minCoord.x then
          minCoord.x := coords[i].x;
        if coords[i].y < minCoord.y then
          minCoord.y := coords[i].y;
        if coords[i].x > maxCoord.x then
          maxCoord.x := coords[i].x;
        if coords[i].y > maxCoord.y then
          maxCoord.y := coords[i].y;
      end;
      for i := 0 to high(coords) do
      begin
        TempPoint := (coords[i] - minCoord);
        with TempPoint do
          coords[i] := PointF((x / (maxCoord.x - minCoord.x) - 0.5) * 2 * rx + cx, (y / (maxCoord.y - minCoord.y) - 0.5) * 2 * ry + cy);
      end;
//      bmp.Canvas2D.beginPath;
      for i := 0 to high(coords) do
      begin
//        bmp.Canvas2D.lineTo( (coords[i] + coords[(i + 1) mod length(coords)]) * (1 / 2) );
        i2 := (i + 1) mod length(coords);
        p1 := coords[i2];
        i2 := (i + 2) mod length(coords);
        p2 := coords[i2];
        bmp.Canvas2D.arcTo(p1 , p2, FRoundRadius);
      end;
//    bmp.Canvas2D.closePath;
    end
    else
    begin
    { bmp.Canvas2D.save;
      bmp.Canvas2D.translate(cx, cy);
      bmp.Canvas2D.scale(rx, ry);
      bmp.Canvas2D.beginPath;
      bmp.Canvas2D.arc(0, 0, 1, 0, 2 * Pi);
      bmp.Canvas2D.restore;}
    end;

    bmp.Canvas2D.fill;
//    if BorderWidth <> 0 then
//      bmp.Canvas2D.stroke;

//    bmp.Canvas2D.fillStyle(BGRAWhite);
//    bmp.Canvas2D.strokeStyle(BGRABlack);
    fillGrad.Free;
    borderGrad.Free;
  end;
end;

end.

(*  sTemp := '';
    {$IFDEF CPU64}
    sTemp := sTemp  + 'BGRAPtrInt     ' + inttostr(High( BGRAPtrInt      )) + #10#13;
    sTemp := sTemp  + 'BGRAPtrUInt    ' + inttostr(High( BGRAPtrUInt     )) + #10#13;
    {$ELSE}
    sTemp := sTemp  + 'BGRAPtrInt     ' + inttostr(High( BGRAPtrInt      )) + #10#13;
    sTemp := sTemp  + 'BGRAPtrUInt    ' + inttostr(High( BGRAPtrUInt     )) + #10#13;
    sTemp := sTemp  + 'BGRAQWord      ' + inttostr(High( BGRAQWord       )) + #10#13;
    {$ENDIF}
    sTemp := sTemp  + 'BGRAWord       ' + inttostr(High( BGRAWord        )) + #10#13;
    sTemp := sTemp  + 'PBGRAWord      ' + inttostr(SizeOf( PBGRAWord       )) + #10#13;
    sTemp := sTemp  + 'BGRADWord      ' + inttostr(High( BGRADWord       )) + #10#13;
    sTemp := sTemp  + 'BGRALongWord   ' + inttostr(High( BGRALongWord    )) + #10#13;

    sTemp := sTemp  + 'PBGRAQWord     ' + inttostr(SizeOf( PBGRAQWord      )) + #10#13;
    sTemp := sTemp  + 'PBGRADWord     ' + inttostr(SizeOf( PBGRADWord      )) + #10#13;
    sTemp := sTemp  + 'PBGRALongWord  ' + inttostr(SizeOf( PBGRALongWord   )) + #10#13;
    sTemp := sTemp  + 'BGRANativeInt  ' + inttostr(High( BGRANativeInt   )) + #10#13;
    sTemp := sTemp  + 'BGRANativeUInt ' + inttostr(High( BGRANativeUInt  )) + #10#13;
    sTemp := sTemp  + 'BGRALongInt    ' + inttostr(High( BGRALongInt     )) + #10#13;
    sTemp := sTemp  + 'BGRAInt64      ' + inttostr(High( BGRAInt64       )) + #10#13;
    sTemp := sTemp  + 'BGRAUInt64     ' + inttostr(High( BGRAUInt64      )) + #10#13;
    sTemp := sTemp  + 'BGRACardinal   ' + inttostr(High( BGRACardinal    )) + #10#13;
    sTemp := sTemp  + 'PBGRACardinal  ' + inttostr(SizeOf( PBGRACardinal   )) + #10#13;
    sTemp := sTemp  + 'HDC            ' + inttostr(High( HDC             )) + #10#13;

    ShowMessage(sTemp); *)
