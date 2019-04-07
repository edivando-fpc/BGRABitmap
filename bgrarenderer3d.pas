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


Unit BGRARenderer3D;

{$i bgrabitmap.inc}{$H+}

interface

uses {$IFNDEF FPC}Types, GraphType,{$ENDIF} BGRATypes, BGRAGraphics, BGRABitmapTypes,
  BGRASceneTypes, BGRASSE,
  BGRAPolygon, BGRAColorInt,
  Classes, BGRAMatrix3D,
  BGRAPolygonAliased;

type
  TInt65536ShaderFunction3D = function (Context: PBasicLightingContext; Color: TBGRAPixel): TColorInt65536 of object;

  { TBGRAShader3D }

  TBGRAShader3D = class
  protected
    FAmbiantLightness: integer;
    FAmbiantLightColor: TColorInt65536;
    FUseAmbiantColor: boolean;
    FLights: TList;
    FContextBlock: TMemoryBlockAlign128;
    FShaderFunc: TShaderFunction3D;
    FInt65536ShaderFunc: TInt65536ShaderFunction3D;
    FContext: PBasicLightingContext;
    FOnlyDirectionalLights: boolean;
    FWhiteMaterial: boolean;

    procedure ComputeDiffuseLightness(Context: PSceneLightingContext); {$ifdef inline}inline;{$endif}
    procedure ComputeDiffuseLight(Context: PSceneLightingContext); {$ifdef inline}inline;{$endif}
    procedure ComputeDiffuseAndSpecularLight(Context: PSceneLightingContext); {$ifdef inline}inline;{$endif}

    function ApplyNoLighting(Context_: PBasicLightingContext{PSceneLightingContext}; Color: TBGRAPixel): TBGRAPixel;
    function ApplyLightingWithAmbiantLightnessOnly(Context_: PBasicLightingContext{PSceneLightingContext}; Color: TBGRAPixel): TBGRAPixel;
    function ApplyLightingWithLightness(Context_: PBasicLightingContext{PSceneLightingContext}; Color: TBGRAPixel): TBGRAPixel;
    function ApplyLightingWithDiffuseColor(Context_: PBasicLightingContext{PSceneLightingContext}; Color: TBGRAPixel): TBGRAPixel;
    function ApplyLightingWithDiffuseAndSpecularColor(Context_: PBasicLightingContext{PSceneLightingContext}; Color: TBGRAPixel): TBGRAPixel;

    function Int65536ApplyNoLighting(Context_: PBasicLightingContext{PSceneLightingContext}; Color: TBGRAPixel): TColorInt65536;
    function Int65536ApplyLightingWithAmbiantLightnessOnly(Context_: PBasicLightingContext{PSceneLightingContext}; Color: TBGRAPixel): TColorInt65536;
    function Int65536ApplyLightingWithLightness(Context_: PBasicLightingContext{PSceneLightingContext}; Color: TBGRAPixel): TColorInt65536;
    function Int65536ApplyLightingWithDiffuseColor(Context_: PBasicLightingContext{PSceneLightingContext}; Color: TBGRAPixel): TColorInt65536;
    function Int65536ApplyLightingWithDiffuseAndSpecularColor(Context_: PBasicLightingContext{PSceneLightingContext}; Color: TBGRAPixel): TColorInt65536;
  public
    constructor Create(const AAmbiantLightColorF: TColorF; ALights: TList);
    destructor Destroy; override;
    function Apply(APosition: TPoint3D_128; ANormal: TPoint3D_128; AColor: TBGRAPixel): TBGRAPixel;
    function Int65536Apply(APosition: TPoint3D_128; ANormal: TPoint3D_128; AColor: TBGRAPixel): TColorInt65536;
    procedure Prepare({$IFDEF FPC}constref{$ELSE}const{$ENDIF} ADescription: TFaceRenderingDescription);
    property ShaderFunction: TShaderFunction3D read FShaderFunc;
    property Int65536ShaderFunction: TInt65536ShaderFunction3D read FInt65536ShaderFunc;
    property Context: PBasicLightingContext read FContext;
    property OnlyDirectionalLights: boolean read FOnlyDirectionalLights;
  end;

  { TBGRARenderer3D }

  TBGRARenderer3D = class(TCustomRenderer3D)
  protected
    FColorGradientTempBmp: TBGRACustomBitmap;
    FZBuffer: PSingle;
    FOutputSurface, FRenderSurface: TBGRACustomBitmap;
    FRenderSurfaceMultisample: Integer;
    FMultishapeFiller: TBGRAMultishapeFiller;
    FOptions: TRenderingOptions;
    FShader: TBGRAShader3D;
    FDepths: array of single;
    FLightings: array of BGRAWord;

    FShadedColors: array of TBGRAPixel;
    FSameShadedColors: boolean;

    FCenter: record
      proj: TPointF;
      pos3D,normal3D: TPoint3D_128;
      color: TBGRAPixel;
    end;

    function GetHasZBuffer: boolean; override;
    function GetGlobalScale: single; override;
    function GetSurfaceWidth: integer; override;
    function GetSurfaceHeight: integer; override;
    function GetHandlesNearClipping: boolean; override;
    function GetHandlesFaceCulling: boolean; override;
  public
    constructor Create(AOutputSurface: TBGRACustomBitmap;
      ARenderingOptions: TRenderingOptions;
      AAmbiantLightColorF: TColorF;
      ALights: TList);
    function RenderFace(var ADescription: TFaceRenderingDescription;
      AComputeCoordinate: TComputeProjectionFunc): boolean; override;
    destructor Destroy; override;
  end;

implementation

uses SysUtils, BGRAResample;

{ TBGRAShader3D }

constructor TBGRAShader3D.Create(const AAmbiantLightColorF: TColorF;
  ALights: TList);
var
  j: Integer;
begin
  FAmbiantLightColor := ColorFToColorInt65536(AAmbiantLightColorF);
  FAmbiantLightness := round((AAmbiantLightColorF[1]+AAmbiantLightColorF[2]+AAmbiantLightColorF[3])/3*32768);
  FUseAmbiantColor:= (FAmbiantLightColor.r <> FAmbiantLightColor.g) or (FAmbiantLightColor.g <> FAmbiantLightColor.b);
  FLights := ALights;
  FContextBlock := TMemoryBlockAlign128.Create(sizeof(TSceneLightingContext));
  FOnlyDirectionalLights:= true;
  for j := 0 to FLights.Count-1 do
    if not TBGRALight3D(FLights[j]).IsDirectional then FOnlyDirectionalLights := false;
end;

destructor TBGRAShader3D.Destroy;
begin
  FreeAndNil(FContextBlock);
  inherited Destroy;
end;

function TBGRAShader3D.Apply(APosition: TPoint3D_128; ANormal: TPoint3D_128;
  AColor: TBGRAPixel): TBGRAPixel;
begin
  with Context^ do
  begin
    Position := APosition;
    Normal := ANormal;
  end;
  result := ShaderFunction(Context,AColor);
end;

function TBGRAShader3D.Int65536Apply(APosition: TPoint3D_128;
  ANormal: TPoint3D_128; AColor: TBGRAPixel): TColorInt65536;
begin
  with Context^ do
  begin
    Position := APosition;
    Normal := ANormal;
  end;
  result := Int65536ShaderFunction(Context,AColor);
end;

procedure TBGRAShader3D.Prepare({$IFDEF FPC}constref{$ELSE}const{$ENDIF}
  ADescription: TFaceRenderingDescription);
var
  UseDiffuseColor: Boolean;
  j: Integer;
  ctx: PSceneLightingContext;
begin
  with ADescription do
  begin
   FWhiteMaterial:= Texture <> nil;
   if Material.GetSpecularOn then
   begin
     {$IFDEF OBJ}
     FShaderFunc := TShaderFunction3D({$IFDEF OBJ}@{$ENDIF}ApplyLightingWithDiffuseAndSpecularColor);
     FInt65536ShaderFunc := TInt65536ShaderFunction3D({$IFDEF OBJ}@{$ENDIF}Int65536ApplyLightingWithDiffuseAndSpecularColor);
     {$ELSE}
     FShaderFunc := ApplyLightingWithDiffuseAndSpecularColor;
     FInt65536ShaderFunc := Int65536ApplyLightingWithDiffuseAndSpecularColor;
     {$ENDIF}
   end else
   begin
     UseDiffuseColor := FUseAmbiantColor;
     if not UseDiffuseColor then
     begin
       with Material.GetDiffuseColorInt do
        UseDiffuseColor := (r <> g) or (g <> b);
       if not UseDiffuseColor and Material.GetAutoDiffuseColor then
       begin
         for j := 0 to FLights.Count-1 do
           if TBGRALight3D(FLights[j]).GetColoredLight then
           begin
             UseDiffuseColor := true;
             break;
           end;
       end;
     end;
     if UseDiffuseColor then
     begin
       {$IFDEF OBJ}
       FShaderFunc := TShaderFunction3D({$IFDEF OBJ}@{$ENDIF}ApplyLightingWithDiffuseColor);
       FInt65536ShaderFunc := TInt65536ShaderFunction3D({$IFDEF OBJ}@{$ENDIF}Int65536ApplyLightingWithDiffuseColor);
       {$ELSE}
       FShaderFunc := ApplyLightingWithDiffuseColor;
       FInt65536ShaderFunc := Int65536ApplyLightingWithDiffuseColor;
       {$ENDIF}
     end else
     begin
       if FLights.Count = 0 then
       begin
         if FAmbiantLightness = 32768 then
         begin
           {$IFDEF OBJ}
           FShaderFunc := TShaderFunction3D({$IFDEF OBJ}@{$ENDIF}ApplyNoLighting);
           FInt65536ShaderFunc := TInt65536ShaderFunction3D({$IFDEF OBJ}@{$ENDIF}Int65536ApplyNoLighting);
           {$ELSE}
           FShaderFunc := ApplyNoLighting;
           FInt65536ShaderFunc := Int65536ApplyNoLighting;
           {$ENDIF}
         end else
         begin
           {$IFDEF OBJ}
           FShaderFunc := TShaderFunction3D({$IFDEF OBJ}@{$ENDIF}ApplyLightingWithAmbiantLightnessOnly);
           FInt65536ShaderFunc := TInt65536ShaderFunction3D({$IFDEF OBJ}@{$ENDIF}Int65536ApplyLightingWithAmbiantLightnessOnly);
           {$ELSE}
           FShaderFunc := ApplyLightingWithAmbiantLightnessOnly;
           FInt65536ShaderFunc := Int65536ApplyLightingWithAmbiantLightnessOnly;
           {$ENDIF}
         end;
       end else
       begin
        {$IFDEF OBJ}
        FShaderFunc := TShaderFunction3D({$IFDEF OBJ}@{$ENDIF}ApplyLightingWithLightness);
        FInt65536ShaderFunc := TInt65536ShaderFunction3D({$IFDEF OBJ}@{$ENDIF}Int65536ApplyLightingWithLightness);
        {$ELSE}
        FShaderFunc := ApplyLightingWithLightness;
        FInt65536ShaderFunc := Int65536ApplyLightingWithLightness;
        {$ENDIF}
       end;
     end;
   end;

    ctx := PSceneLightingContext( FContextBlock.Data );
    ctx^.material := Material;
    ctx^.LightThroughFactor := LightThroughFactor;
    ctx^.LightThrough := ctx^.LightThroughFactor > 0;
    ctx^.SaturationHighF := Material.GetSaturationHigh;
    ctx^.SaturationLowF := Material.GetSaturationLow;
    ctx^.SaturationHigh := round(Material.GetSaturationHigh*32768);
    ctx^.SaturationLow := round(Material.GetSaturationLow*32768);
    FContext := PBasicLightingContext(ctx);
  end;
end;

procedure TBGRAShader3D.ComputeDiffuseLightness(
  Context: PSceneLightingContext);
var i: BGRANativeInt;
begin
  Context^.lightness := FAmbiantLightness;

  i := FLights.Count-1;
  while i >= 0 do
  begin
    TBGRALight3D(FLights[i]).ComputeDiffuseLightness(Context);
    dec(i);
  end;
end;

procedure TBGRAShader3D.ComputeDiffuseLight(Context: PSceneLightingContext);
var i: BGRANativeInt;
  m: TBGRAMaterial3D;
begin
  m := TBGRAMaterial3D(Context^.material);

  if FWhiteMaterial or m.GetAutoAmbiantColor then
    Context^.diffuseColor := FAmbiantLightColor
  else
    Context^.diffuseColor := FAmbiantLightColor*m.GetAmbiantColorInt;

  i := FLights.Count-1;
  while i >= 0 do
  begin
    TBGRALight3D(FLights[i]).ComputeDiffuseColor(Context);
    dec(i);
  end;

  Context^.diffuseColor.a := 65536;
end;

procedure TBGRAShader3D.ComputeDiffuseAndSpecularLight(
  Context: PSceneLightingContext);
var i: BGRANativeInt;
  m: TBGRAMaterial3D;
begin
  m := TBGRAMaterial3D(Context^.material);

  if FWhiteMaterial or m.GetAutoAmbiantColor then
    Context^.diffuseColor := FAmbiantLightColor
  else
    Context^.diffuseColor := FAmbiantLightColor*m.GetAmbiantColorInt;
  Context^.specularColor := ColorInt65536(0,0,0,0);

  i := FLights.Count-1;
  while i >= 0 do
  begin
    TBGRALight3D(FLights[i]).ComputeDiffuseAndSpecularColor(Context);
    dec(i);
  end;

  Context^.diffuseColor.a := 65536;
end;

function TBGRAShader3D.ApplyNoLighting(Context_: PBasicLightingContext{PSceneLightingContext};
  Color: TBGRAPixel): TBGRAPixel;
var
  m: TBGRAMaterial3D;
  _Context : PSceneLightingContext;
begin
  _Context := @Context_;
  m := TBGRAMaterial3D(_Context^.material);

  if FWhiteMaterial or m.GetAutoAmbiantColor then
    result := Color
  else
    result := ColorIntToBGRA(BGRAToColorInt(Color,True)*m.GetAmbiantColorInt,True);
end;

function TBGRAShader3D.ApplyLightingWithAmbiantLightnessOnly(
  Context_: PBasicLightingContext{PSceneLightingContext}; Color: TBGRAPixel): TBGRAPixel;
var
  m: TBGRAMaterial3D;
  _Context : PSceneLightingContext;
begin
  _Context := @Context_;
  m := TBGRAMaterial3D(_Context^.material);

  if not FWhiteMaterial and not m.GetAutoAmbiantColor then
    Color := ColorIntToBGRA(BGRAToColorInt(Color,True)* m.GetAmbiantColorInt,True);

  if FAmbiantLightness <= 0 then
    result := BGRA(0,0,0,color.alpha)
  else
    result := ApplyIntensityFast(Color, FAmbiantLightness);
end;

function TBGRAShader3D.ApplyLightingWithLightness(Context_: PBasicLightingContext{PSceneLightingContext};
  Color: TBGRAPixel): TBGRAPixel;
var
  m: TBGRAMaterial3D;
  _Context : PSceneLightingContext;
begin
  _Context := @Context_;
  ComputeDiffuseLightness(_Context);

  m := TBGRAMaterial3D(_Context^.material);
  if not FWhiteMaterial and not m.GetAutoSimpleColor then
    Color := ColorIntToBGRA(BGRAToColorInt(Color,True)*m.GetSimpleColorInt,True);

  with _Context^ do
    if Lightness <= 0 then
      result := BGRA(0,0,0,color.alpha)
    else
    begin
      if Lightness <= SaturationLow then
        result := ApplyIntensityFast(Color, Lightness)
      else if Lightness >= SaturationHigh then
        result := BGRA(255,255,255,color.alpha)
      else
        result := ApplyLightnessFast( ApplyIntensityFast(Color, SaturationLow),
                              (Lightness - SaturationLow)*32767 div (SaturationHigh-SaturationLow)+32768 );
    end;
end;

function TBGRAShader3D.ApplyLightingWithDiffuseColor(Context_: PBasicLightingContext{PSceneLightingContext};
  Color: TBGRAPixel): TBGRAPixel;
var
  _Context : PSceneLightingContext;
begin
  _Context := @Context_;
  ComputeDiffuseLight(_Context);
  result := ColorIntToBGRA(BGRAToColorInt(Color,True)*_Context^.diffuseColor,True);
end;

function TBGRAShader3D.ApplyLightingWithDiffuseAndSpecularColor(Context_: PBasicLightingContext{PSceneLightingContext};
  Color: TBGRAPixel): TBGRAPixel;
var
  _Context : PSceneLightingContext;
begin
  _Context := @Context_;
  ComputeDiffuseAndSpecularLight(_Context);
  result := ColorIntToBGRA(BGRAToColorInt(Color,True)*_Context^.diffuseColor + _Context^.specularColor,True);
end;

function TBGRAShader3D.Int65536ApplyNoLighting(Context_: PBasicLightingContext{PSceneLightingContext};
  Color: TBGRAPixel): TColorInt65536;
var
  m: TBGRAMaterial3D;
  _Context : PSceneLightingContext;
begin
  _Context := @Context_;
  m := TBGRAMaterial3D(_Context^.material);

  if not FWhiteMaterial and not m.GetAutoAmbiantColor then
    result := BGRAToColorInt(Color,True)* m.GetAmbiantColorInt
  else
    result := BGRAToColorInt(Color,True);
end;

function TBGRAShader3D.Int65536ApplyLightingWithAmbiantLightnessOnly(
  Context_: PBasicLightingContext{PSceneLightingContext}; Color: TBGRAPixel): TColorInt65536;
var
  m: TBGRAMaterial3D;
  MaterialColor: TColorInt65536;
  _Context : PSceneLightingContext;
begin
  _Context := @Context_;
  m := TBGRAMaterial3D(_Context^.material);

  if not FWhiteMaterial and not m.GetAutoAmbiantColor then
    MaterialColor := BGRAToColorInt(Color,True)* m.GetAmbiantColorInt
  else
    MaterialColor := BGRAToColorInt(Color,True);

  if FAmbiantLightness <= 0 then
    result := ColorInt65536(0,0,0,MaterialColor.a)
  else
    result := MaterialColor *
           ColorInt65536(FAmbiantLightness shl 1,FAmbiantLightness shl 1,FAmbiantLightness shl 1,65536);
end;

function TBGRAShader3D.Int65536ApplyLightingWithLightness(
  Context_: PBasicLightingContext{PSceneLightingContext}; Color: TBGRAPixel): TColorInt65536;
var
  MaterialColor: TColorInt65536;
  m: TBGRAMaterial3D;
  Extra: BGRANativeInt;
  _Context : PSceneLightingContext;
begin
  _Context := @Context_;
  ComputeDiffuseLightness(_Context);

  m := TBGRAMaterial3D(_Context^.material);
  if not FWhiteMaterial and not m.GetAutoSimpleColor then
    MaterialColor := BGRAToColorInt(Color,True)*m.GetSimpleColorInt
  else
    MaterialColor := BGRAToColorInt(Color,True);

  with _Context^ do
    if Lightness <= 0 then
      result := ColorInt65536(0,0,0,MaterialColor.a)
    else
    begin
      if Lightness <= SaturationLow then
        result := MaterialColor * ColorInt65536(Lightness shl 1,Lightness shl 1,Lightness shl 1,65536)
      else if Lightness >= SaturationHigh then
        result := ColorInt65536(65536,65536,65536,MaterialColor.a)
      else
      begin
        result := MaterialColor * ColorInt65536(Lightness shl 1,Lightness shl 1,Lightness shl 1,65536);
        Extra := (Lightness - SaturationLow)*65536 div (SaturationHigh-SaturationLow);
        result.r := result.r +Extra;
        result.g := result.g +Extra;
        result.b := result.b +Extra;
      end;
    end;
end;

function TBGRAShader3D.Int65536ApplyLightingWithDiffuseColor(
  Context_: PBasicLightingContext{PSceneLightingContext}; Color: TBGRAPixel): TColorInt65536;
var
  _Context : PSceneLightingContext;
begin
  _Context := @Context_;
  ComputeDiffuseLight(_Context);
  result := BGRAToColorInt(Color,True)* _Context^.diffuseColor;
end;

function TBGRAShader3D.Int65536ApplyLightingWithDiffuseAndSpecularColor(
  Context_: PBasicLightingContext{PSceneLightingContext}; Color: TBGRAPixel): TColorInt65536;
var
  _Context : PSceneLightingContext;
begin
  _Context := @Context_;
  ComputeDiffuseAndSpecularLight(_Context);
  result := BGRAToColorInt(Color,True)*_Context^.diffuseColor + _Context^.specularColor;
end;

{ TBGRARenderer3D }

function TBGRARenderer3D.GetHasZBuffer: boolean;
begin
  result := Assigned(FZBuffer);
end;

function TBGRARenderer3D.GetGlobalScale: single;
begin
  result := FRenderSurfaceMultisample;
end;

function TBGRARenderer3D.GetSurfaceWidth: integer;
begin
  result := FOutputSurface.Width;
end;

function TBGRARenderer3D.GetSurfaceHeight: integer;
begin
  result := FOutputSurface.Height;
end;

function TBGRARenderer3D.GetHandlesNearClipping: boolean;
begin
  result := false;
end;

function TBGRARenderer3D.GetHandlesFaceCulling: boolean;
begin
  result := false;
end;

constructor TBGRARenderer3D.Create(AOutputSurface: TBGRACustomBitmap;
            ARenderingOptions: TRenderingOptions;
            AAmbiantLightColorF: TColorF;
            ALights: TList);
begin
  if AOutputSurface = nil then
    raise exception.Create('No surface specified');
  FOutputSurface := AOutputSurface;
  FOptions := ARenderingOptions;

  if (FOptions.AntialiasingMode = am3dResample)
    and (FOptions.AntialiasingResampleLevel > 1) then
  begin
    FRenderSurface := FOutputSurface.NewBitmap(FOutputSurface.Width*FOptions.AntialiasingResampleLevel,
                                         FOutputSurface.Height*FOptions.AntialiasingResampleLevel);
    FRenderSurfaceMultisample := FOptions.AntialiasingResampleLevel;
  end else
  begin
    FRenderSurface := FOutputSurface;
    FRenderSurfaceMultisample := 1;
  end;

  FColorGradientTempBmp := FRenderSurface.NewBitmap(2,2);
  FColorGradientTempBmp.ScanInterpolationFilter := rfLinear;

  if FOptions.PerspectiveMode = pmZBuffer then
  begin
    Getmem(FZBuffer, FRenderSurface.NbPixels*sizeof(single));
    FillDWord_(FZBuffer^, FRenderSurface.NbPixels, {$IFDEF OBJ}BGRADWord(single(0)){$ELSE}0{$ENDIF} );
  end
  else
    FZBuffer := nil;

  if (FOptions.AntialiasingMode = am3dMultishape) and
    (FOptions.PerspectiveMode <> pmZBuffer) then
  begin
    FMultishapeFiller := TBGRAMultishapeFiller.Create;
    FMultishapeFiller.PolygonOrder := poLastOnTop;
  end
  else
    FMultishapeFiller := nil;

  FShader := TBGRAShader3D.Create(AAmbiantLightColorF, ALights);
end;

function TBGRARenderer3D.RenderFace(var ADescription: TFaceRenderingDescription;
  AComputeCoordinate: TComputeProjectionFunc): boolean;

  procedure ComputeCenter;
  var j: BGRANativeInt;
  begin
    with ADescription do
    begin
      with FCenter do
      begin
        ClearPoint3D_128(pos3D);
        ClearPoint3D_128(normal3D);
        color := MergeBGRA({$IFDEF BDS}slice(PHackArrayOfTBGRAPixel(Colors)^{$ELSE}slice(Colors{$ENDIF},NbVertices));
      end;
      for j := 0 to NbVertices-1 do
      begin
        FCenter.pos3D    := FCenter.pos3D    +Positions3D[j];
        FCenter.normal3D := FCenter.normal3D +Normals3D[j];
      end;
      with FCenter do
      begin
        pos3D := pos3D *(1/NbVertices);
        Normalize3D_128(normal3D);
      end;
    end;
    FCenter.proj := AComputeCoordinate(FCenter.pos3D);
  end;

  procedure DrawFaceWithShader;
  var
    j,k: BGRANativeInt;
    SameColor: boolean;
  begin
    with ADescription do
    begin
      if Texture <> nil then
      begin
        {$IFDEF BDS}
        BGRAPolygonAliased.PolygonPerspectiveMappingShaderAliased(FRenderSurface,
            slice(PHackArrayOfTPointF(Projections)^,NbVertices),slice(PHackArrayOfTPoint3D_128(Positions3D)^,NbVertices),slice(PHackArrayOfTPoint3D_128(Normals3D)^,NbVertices),
            Texture,slice(PHackArrayOfTPointF(TexCoords)^,NbVertices),FOptions.TextureInterpolation,
            FShader.ShaderFunction,True, BGRAPixelTransparent,FZBuffer,FShader.Context);
        {$ELSE}
        BGRAPolygonAliased.PolygonPerspectiveMappingShaderAliased(FRenderSurface,
            slice(PHackArrayOfTPointF(Projections)^,NbVertices),slice(Positions3D,NbVertices),slice(Normals3D,NbVertices),
            Texture,slice(PHackArrayOfTPointF(TexCoords)^,NbVertices),FOptions.TextureInterpolation,
            FShader.ShaderFunction,True, BGRAPixelTransparent,FZBuffer,FShader.Context);
        {$ENDIF}
        exit;
      end;

      SameColor := True;
      for j := 1 to NbVertices-1 do
        if not BGRAPixelEqual(Colors[j],{<>}Colors[j-1]) then SameColor := False;

      if SameColor then
      begin
        {$IFDEF BDS}
        BGRAPolygonAliased.PolygonPerspectiveMappingShaderAliased(FRenderSurface,
          slice(PHackArrayOfTPointF(Projections)^,NbVertices),slice(PHackArrayOfTPoint3D_128(Positions3D)^,NbVertices),slice(PHackArrayOfTPoint3D_128(Normals3D)^,NbVertices),nil,
            slice(PHackArrayOfTPointF(TexCoords)^,NbVertices),False,FShader.ShaderFunction,True,Colors[0],FZBuffer,FShader.Context);
        {$ELSE}
        BGRAPolygonAliased.PolygonPerspectiveMappingShaderAliased(FRenderSurface,
          slice(Projections,NbVertices),slice(Positions3D,NbVertices),slice(Normals3D,NbVertices),nil,
            slice(TexCoords,NbVertices),False,FShader.ShaderFunction,True,Colors[0],FZBuffer,FShader.Context);
        {$ENDIF}
      end else
      if NbVertices = 3 then
      begin
        FColorGradientTempBmp.SetPixel(0,0,Colors[0]);
        FColorGradientTempBmp.SetPixel(1,0,Colors[1]);
        FColorGradientTempBmp.SetPixel(0,1,Colors[2]);
        FColorGradientTempBmp.SetPixel(1,1,MergeBGRA(Colors[1],Colors[2]));
        {$IFDEF BDS}
        BGRAPolygonAliased.PolygonPerspectiveMappingShaderAliased(FRenderSurface,
          slice(PHackArrayOfTPointF(Projections)^,NbVertices),slice(PHackArrayOfTPoint3D_128(Positions3D)^,NbVertices),slice(PHackArrayOfTPoint3D_128(Normals3D)^,NbVertices),FColorGradientTempBmp,
            [PointF(0,0),PointF(1,0),PointF(0,1)],True,FShader.ShaderFunction,True, BGRAPixelTransparent,FZBuffer,FShader.Context);
        {$ELSE}
        BGRAPolygonAliased.PolygonPerspectiveMappingShaderAliased(FRenderSurface,
          slice(Projections,NbVertices),slice(Positions3D,NbVertices),slice(Normals3D,NbVertices),FColorGradientTempBmp,
            [PointF(0,0),PointF(1,0),PointF(0,1)],True,FShader.ShaderFunction,True, BGRAPixelTransparent,FZBuffer,FShader.Context);

        {$ENDIF}
      end else
      if NbVertices = 4 then
      begin
        FColorGradientTempBmp.SetPixel(0,0,Colors[0]);
        FColorGradientTempBmp.SetPixel(1,0,Colors[1]);
        FColorGradientTempBmp.SetPixel(1,1,Colors[2]);
        FColorGradientTempBmp.SetPixel(0,1,Colors[3]);
        {$IFDEF BDS}
        BGRAPolygonAliased.PolygonPerspectiveMappingShaderAliased(FRenderSurface,
          slice(PHackArrayOfTPointF(Projections)^,NbVertices),slice(PHackArrayOfTPoint3D_128(Positions3D)^,NbVertices),slice(PHackArrayOfTPoint3D_128(Normals3D)^,NbVertices),FColorGradientTempBmp,
            [PointF(0,0),PointF(1,0),PointF(1,1),PointF(0,1)],True,FShader.ShaderFunction,True, BGRAPixelTransparent,FZBuffer,FShader.Context);
         {$ELSE}
        BGRAPolygonAliased.PolygonPerspectiveMappingShaderAliased(FRenderSurface,
          slice(Projections,NbVertices),slice(Positions3D,NbVertices),slice(Normals3D,NbVertices),FColorGradientTempBmp,
            [PointF(0,0),PointF(1,0),PointF(1,1),PointF(0,1)],True,FShader.ShaderFunction,True, BGRAPixelTransparent,FZBuffer,FShader.Context);
         {$ENDIF}
      end else
      if NbVertices >= 3 then
      begin //split into triangles
        ComputeCenter;
        k := NbVertices-1;
        for j := 0 to NbVertices-1 do
        begin
          FColorGradientTempBmp.SetPixel(0,0,Colors[k]);
          FColorGradientTempBmp.SetPixel(1,0,Colors[j]);
          FColorGradientTempBmp.SetPixel(0,1,FCenter.color);
          FColorGradientTempBmp.SetPixel(1,1,MergeBGRA(Colors[j],FCenter.color));
          BGRAPolygonAliased.PolygonPerspectiveMappingShaderAliased(FRenderSurface,
            [Projections[k],Projections[j],FCenter.proj], [Positions3D[k],Positions3D[j],FCenter.pos3D],
            [Normals3D[k],Normals3D[j],FCenter.normal3D], FColorGradientTempBmp,
              [PointF(0,0),PointF(1,0),PointF(0,1)],True,FShader.ShaderFunction,True, BGRAPixelTransparent,FZBuffer,FShader.Context);
          k := j;
        end;
      end;
    end;
  end;

  procedure ComputeShadedColors;
  var
    j: BGRANativeInt;
  begin
    with ADescription do
    begin
      //Vertex lighting interpolation (low-quality Gouraud, low-quality Phong)
      if length(FShadedColors) < NbVertices then
        setlength(FShadedColors, NbVertices);

      for j := 0 to NbVertices-1 do
        FShadedColors[j] := FShader.Apply(Positions3D[j],Normals3D[j],Colors[j]);

      FSameShadedColors := True;
      for j := 1 to NbVertices-1 do
        if not BGRAPixelEqual(FShadedColors[j],{<>}FShadedColors[j-1]) then
        begin
          FSameShadedColors := False;
          break;
        end;
    end;
  end;

  procedure DrawWithMultishape;
  var shadedCenter: TBGRAPixel;
    j,k: BGRANativeInt;
  begin
    with ADescription do
    begin
      if Texture <> nil then
      begin
        if (FOptions.PerspectiveMode <> pmLinearMapping) and (NbVertices=4) then
          FMultishapeFiller.AddQuadPerspectiveMapping(
             Projections[0],Projections[1],Projections[2],Projections[3],
             Texture,TexCoords[0],TexCoords[1],TexCoords[2],TexCoords[3])
        else
        if NbVertices>=3 then
        begin
          for j := 0 to NbVertices-3 do
            FMultishapeFiller.AddTriangleLinearMapping(
                Projections[j],Projections[j+1],Projections[j+2],
                Texture,TexCoords[j],TexCoords[j+1],TexCoords[j+2]);
        end;
      end
      else
      begin
        ComputeShadedColors;

        if FSameShadedColors then
        {$IFDEF BDS}
          FMultishapeFiller.AddPolygon(slice(PHackArrayOfTPointF(Projections)^,NbVertices),FShadedColors[0])
        {$ELSE}
          FMultishapeFiller.AddPolygon(slice(Projections,NbVertices),FShadedColors[0])
        {$ENDIF}
        else
        if NbVertices=3 then
          FMultishapeFiller.AddTriangleLinearColor(
             Projections[0],Projections[1],Projections[2],
             FShadedColors[0],FShadedColors[1],FShadedColors[2])
        else
        if NbVertices>=3 then
        begin  //split into triangles
          ComputeCenter;
          shadedCenter := FShader.Apply(FCenter.pos3D,FCenter.normal3D,FCenter.color);
          k := NbVertices-1;
          for j := 0 to NbVertices-1 do
          begin
            FMultishapeFiller.AddTriangleLinearColor(
               Projections[k],Projections[j],FCenter.proj,
               FShadedColors[k],FShadedColors[j],shadedCenter);
            k := j;
          end;
        end;
      end;
    end;
  end;

  procedure DrawAliasedColoredFace;
  var j,k: integer;
    shadedCenter: TBGRAPixel;
  begin
    with ADescription do
    begin
      ComputeShadedColors;

      if FSameShadedColors then
      begin
        if FOptions.PerspectiveMode = pmZBuffer then
        {$IFDEF BDS}
          BGRAPolygonAliased.PolygonPerspectiveColorGradientAliased(FRenderSurface, slice(PHackArrayOfTPointF(Projections)^,NbVertices),
          slice(PHackArrayOfSingle(FDepths)^,NbVertices), slice(PHackArrayOfTBGRAPixel(FShadedColors)^,NbVertices),True,FZBuffer)
        {$ELSE}
          BGRAPolygonAliased.PolygonPerspectiveColorGradientAliased(FRenderSurface, slice(Projections,NbVertices),
          slice(FDepths,NbVertices),slice(FShadedColors,NbVertices),True,FZBuffer)
        {$ENDIF}
        else
         {$IFDEF BDS}
           FRenderSurface.FillPoly(slice(PHackArrayOfTPointF(Projections)^,NbVertices),FShadedColors[0],dmDrawWithTransparency);
         {$ELSE}
           FRenderSurface.FillPoly(slice(Projections,NbVertices),FShadedColors[0],dmDrawWithTransparency);
         {$ENDIF}
      end
      else
      begin
        if NbVertices > 4 then
        begin //split into triangles
          ComputeCenter;
          shadedCenter := FShader.Apply(FCenter.pos3D,FCenter.normal3D,FCenter.color);
          k := NbVertices-1;
          if FOptions.PerspectiveMode = pmLinearMapping then
          begin
            for j := 0 to NbVertices-1 do
            begin
              FRenderSurface.FillPolyLinearColor([Projections[k],Projections[j],FCenter.proj],[FShadedColors[k],FShadedColors[j],shadedCenter]);
              k := j;
            end;
          end else
          begin
            for j := 0 to NbVertices-1 do
            begin
              BGRAPolygonAliased.PolygonPerspectiveColorGradientAliased(FRenderSurface, [Projections[k],Projections[j],FCenter.proj],
               [FDepths[k],FDepths[j],FCenter.pos3D.z], [FShadedColors[k],FShadedColors[j],shadedCenter],True,FZBuffer);
              k := j;
            end;
          end;
        end else
        begin
          if FOptions.PerspectiveMode = pmLinearMapping then
           {$IFDEF BDS}
            FRenderSurface.FillPolyLinearColor(slice(PHackArrayOfTPointF(Projections)^,NbVertices),slice(PHackArrayOfTBGRAPixel(FShadedColors)^,NbVertices))
           {$ELSE}
            FRenderSurface.FillPolyLinearColor(slice(Projections,NbVertices),slice(FShadedColors,NbVertices))
           {$ENDIF}
          else
           {$IFDEF BDS}
            BGRAPolygonAliased.PolygonPerspectiveColorGradientAliased(FRenderSurface, slice(PHackArrayOfTPointF(Projections)^,NbVertices),
            slice(PHackArrayOfSingle(FDepths)^,NbVertices), slice(PHackArrayOfTBGRAPixel(FShadedColors)^,NbVertices),True,FZBuffer);
           {$ELSE}
            BGRAPolygonAliased.PolygonPerspectiveColorGradientAliased(FRenderSurface, slice(Projections,NbVertices),
             slice(FDepths,NbVertices), slice(FShadedColors,NbVertices),True,FZBuffer);
           {$ENDIF}
        end;
      end;
    end;
  end;

  procedure DrawWithoutShader;
  var
    noLighting: Boolean;
    j: BGRANativeInt;
  begin
    with ADescription do
    begin
      if length(FDepths) < NbVertices then
        setlength(FDepths, NbVertices);
      for j := 0 to NbVertices-1 do
        FDepths[j] := Positions3D[j].z;

      if Texture <> nil then
      begin
        noLighting := True;
        if length(FLightings) < NbVertices then
            setlength(FLightings, NbVertices);
        for j := 0 to NbVertices-1 do
        begin
          FLightings[j] := FShader.Int65536Apply(Positions3D[j],Normals3D[j],BGRAWhite).g div 2;
          if abs(FLightings[j]-32768) > 1 then noLighting := false;
        end;

        if noLighting then
        begin
          if FOptions.PerspectiveMode <> pmLinearMapping then
            {$IFDEF BDS}
            FRenderSurface.FillPolyPerspectiveMapping(slice(PHackArrayOfTPointF(Projections)^,NbVertices),
            slice(PHackArrayOfSingle(FDepths)^,NbVertices),Texture,slice(PHackArrayOfTPointF(TexCoords)^,NbVertices),
            FOptions.TextureInterpolation, FZBuffer)
            {$ELSE}
            FRenderSurface.FillPolyPerspectiveMapping(slice(Projections,NbVertices),
            slice(FDepths,NbVertices),Texture,slice(TexCoords,NbVertices),
            FOptions.TextureInterpolation, FZBuffer)
            {$ENDIF}
          else
            {$IFDEF BDS}
            FRenderSurface.FillPolyLinearMapping(slice(PHackArrayOfTPointF(Projections)^,NbVertices),
            Texture,slice(PHackArrayOfTPointF(TexCoords)^,NbVertices),FOptions.TextureInterpolation);
            {$ELSE}
            FRenderSurface.FillPolyLinearMapping(slice(Projections,NbVertices),
            Texture,slice(TexCoords,NbVertices),FOptions.TextureInterpolation);
            {$ENDIF}
        end else
        begin
          if FOptions.PerspectiveMode <> pmLinearMapping then
            {$IFDEF BDS}
            FRenderSurface.FillPolyPerspectiveMappingLightness(
              slice(PHackArrayOfTPointF(Projections)^,NbVertices),slice(PHackArrayOfSingle(FDepths)^,NbVertices),Texture,
              slice(PHackArrayOfTPointF(TexCoords)^,NbVertices),slice(PHackArrayOfBGRAWord(FLightings)^,NbVertices),
              FOptions.TextureInterpolation, FZBuffer)
            {$ELSE}
            FRenderSurface.FillPolyPerspectiveMappingLightness(
              slice(Projections,NbVertices),slice(FDepths,NbVertices),Texture,
              slice(TexCoords,NbVertices),slice(FLightings,NbVertices),
              FOptions.TextureInterpolation, FZBuffer)
            {$ENDIF}
          else
            {$IFDEF BDS}
            FRenderSurface.FillPolyLinearMappingLightness(
              slice(PHackArrayOfTPointF(Projections)^,NbVertices),Texture,slice(PHackArrayOfTPointF(TexCoords)^,NbVertices),
              slice(PHackArrayOfBGRAWord(FLightings)^,NbVertices),FOptions.TextureInterpolation);
            {$ELSE}
            FRenderSurface.FillPolyLinearMappingLightness(
              slice(Projections,NbVertices),Texture,slice(TexCoords,NbVertices),
              slice(FLightings,NbVertices),FOptions.TextureInterpolation);
            {$ENDIF}
        end;
      end
      else
        DrawAliasedColoredFace;  //already low-quality shaded
    end;
  end;

var
  j: integer;
  SkipShader: boolean;
begin
  result := true;
  FShader.Prepare(ADescription);
  with ADescription do
  begin
    //high-quality lighting interpolation, necessary for Phong and high-quality Gouraud
    if (  (FOptions.LightingInterpolation = liAlwaysHighQuality) or
          ((FOptions.LightingInterpolation = liSpecularHighQuality) and Material.GetSpecularOn) )
      and (NormalsMode <> lnNone) then
    begin
      //if there are only directional lights and all the normals are the same,
      //then the lighting will be uniform so we can skip the shader
      if FShader.OnlyDirectionalLights then
      begin
        SkipShader := true;
        for j := 1 to NbVertices-1 do
          if Normals3D[j] <> Normals3D[j-1] then
          begin
            SkipShader := false;
            break;
          end;
      end else
        SkipShader := false;

      if not SkipShader then
      begin
        DrawFaceWithShader;
        exit;
      end;
    end;

    if Assigned(FMultishapeFiller) then //high-quality antialiasing
      DrawWithMultishape
    else
      DrawWithoutShader;
  end;
end;

destructor TBGRARenderer3D.Destroy;
begin
  FreeAndNil(FShader);

  if Assigned(FMultishapeFiller) then
  begin
    FMultishapeFiller.Draw(FRenderSurface);
    FreeAndNil(FMultishapeFiller);
  end;

  if FZBuffer <> nil then
  begin
    FreeMem(FZBuffer);
    FZBuffer := nil;
  end;

  FreeAndNil(FColorGradientTempBmp);

  if FRenderSurfaceMultisample > 1 then
  begin
    BGRAResample.DownSamplePutImage(FRenderSurface,
               FRenderSurfaceMultisample,FRenderSurfaceMultisample,
               FOutputSurface, 0,0, dmDrawWithTransparency);
    FreeAndNil(FRenderSurface);
    FRenderSurfaceMultisample := 1;
  end
  else
    FRenderSurface := nil;

  inherited Destroy;
end;

end.
