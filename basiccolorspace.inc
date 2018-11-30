{$IFDEF INCLUDE_INTERFACE}
{$UNDEF INCLUDE_INTERFACE}
type
  {* Possible channels in a bitmap using any RGBA colorspace }
  TChannel = (cRed, cGreen, cBlue, cAlpha);
  {** Combination of channels }
  TChannels = set of TChannel;

{ Gamma conversion arrays. Should be used as readonly }
var
  // TBGRAPixel -> TExpandedPixel
  GammaExpansionTab:   packed array[0..255] of BGRAWord;

  // TExpandedPixel -> TBGRAPixel
  GammaCompressionTab : packed array[0..65535] of byte;          //rounded value
  GammaCompressionTabFrac : packed array[0..65535] of shortint;  //fractional part of value from -0.5 to +0.5

procedure BGRASetGamma(AGamma: single = 1.7);
function BGRAGetGamma: single;

type
  PExpandedPixel = ^TExpandedPixel;
  { TExpandedPixel }
  {* Stores a gamma expanded RGB color. Values range from 0 to 65535 }
  TExpandedPixel = packed record
    red, green, blue, alpha: BGRAWord;
    class Operator  {$IFDEF OBJ} := {$ELSE} Implicit{$ENDIF}(const AValue: TExpandedPixel): TColor;
    class Operator  {$IFDEF OBJ} := {$ELSE} Implicit{$ENDIF}(const AValue: TColor): TExpandedPixel;
    class Operator  {$IFDEF OBJ} := {$ELSE} Implicit{$ENDIF}(const Source: TExpandedPixel): TBGRAPixel;
    class Operator  {$IFDEF OBJ} := {$ELSE} Implicit{$ENDIF}(const Source: TBGRAPixel): TExpandedPixel;
    function ToFPColor(AGammaCompression: boolean = true): TFPColor;
    procedure FromFPColor(const AValue: TFPColor; AGammaExpansion: boolean = true);
  end;
  TExpandedPixelBuffer = packed array of TExpandedPixel;

  procedure AllocateExpandedPixelBuffer(var ABuffer: TExpandedPixelBuffer; ASize: integer);

  {** Converts a pixel from sRGB to gamma expanded RGB }
  function GammaExpansion(c: TBGRAPixel): TExpandedPixel; {$ifdef inline}inline;{$endif}
  {** Converts a pixel from gamma expanded RGB to sRGB }
  function GammaCompression(const ec: TExpandedPixel): TBGRAPixel;{$ifdef inline}inline;{$endif}overload;
  {** Converts a pixel from gamma expanded RGB to sRGB }
  function GammaCompression(red,green,blue,alpha: BGRAWord): TBGRAPixel;{$ifdef inline}inline;{$endif}overload;
  {** Returns the intensity of an gamma-expanded pixel. The intensity is the
     maximum value reached by any component }
  function GetIntensity(const c: TExpandedPixel): BGRAWord;{$ifdef inline}inline;{$endif}overload;
  {** Sets the intensity of a gamma-expanded pixel }
  function SetIntensity(const c: TExpandedPixel; intensity: BGRAWord): TExpandedPixel;  overload;
  {** Returns the lightness of an gamma-expanded pixel. The lightness is the
     perceived brightness, 0 being black and 65535 being white }
  function GetLightness(const c: TExpandedPixel): BGRAWord;{$ifdef inline}inline;{$endif}overload;
  {** Sets the lightness of a gamma-expanded pixel }
  function SetLightness(const c: TExpandedPixel; lightness: BGRAWord): TExpandedPixel; overload;
  {** Sets the lightness of a gamma expanded pixel, provided you already know the current
     value of lightness ''curLightness''. It is a bit faster than the previous function }
  function SetLightness(const c: TExpandedPixel; lightness: BGRAWord; curLightness: BGRAWord): TExpandedPixel; overload;
  {** Returns the importance of the color. It is similar to saturation
      in HSL colorspace, except it is gamma corrected. A value of zero indicates
      a black/gray/white, and a value of 65535 indicates a bright color }
  function ColorImportance(ec: TExpandedPixel): BGRAWord;
  {** Merge two gamma expanded pixels (so taking into account gamma correction) }
  function MergeBGRA(ec1, ec2: TExpandedPixel): TExpandedPixel; overload;
  {** Computes the difference (with gamma correction) between two pixels,
      taking into account all dimensions, including transparency. The
      result ranges from 0 to 65535 }
  function ExpandedDiff(ec1, ec2: TExpandedPixel): BGRAWord;

type
  {* General purpose color variable with single-precision floating point values }
  PColorF = ^TColorF;
  TColorF = packed array[1..4] of single;
  ArrayOfTColorF = array of TColorF;

  {** Creates a TColorF structure }
  function ColorF(red,green,blue,alpha: single): TColorF;
  function BGRAToColorF(c: TBGRAPixel; AGammaExpansion: boolean): TColorF; overload;
  function BGRAToColorF(const a: array of TBGRAPixel; AGammaExpansion: boolean): ArrayOfTColorF; overload;
  function ColorFToBGRA(c: TColorF; AGammaCompression: boolean): TBGRAPixel;
  function GammaCompressionF(c: TColorF): TColorF;
  function GammaExpansionF(c: TColorF): TColorF;
  {** ColorFSub each component separately }
  function ColorFSub (const c1, c2: TColorF): TColorF; {$ifdef inline}inline;{$endif}
  {** ColorFAdd each component separately }
  function ColorFAdd (const c1, c2: TColorF): TColorF; {$ifdef inline}inline;{$endif}
  {** ColorFMult each component separately }
  function ColorFMult (const c1, c2: TColorF): TColorF;{$ifdef inline}inline;{$endif}overload;
  {** ColorFMult each component by ''factor'' }
  function ColorFMult (const c1: TColorF; factor: single): TColorF;{$ifdef inline}inline;{$endif}overload;

type
  {* Pixel color defined in HSL colorspace. Values range from 0 to 65535 }

  { THSLAPixel }
  PHSLAPixel  = ^THSLAPixel;
  THSLAPixel = packed record
    {** Hue of the pixel. Extremum values 0 and 65535 are red }
    hue: BGRAWord;
    {** Saturation of the color. 0 is gray and 65535 is the brightest color (including white) }
    saturation: BGRAWord;
    {** Lightness of the color. 0 is black, 32768 is normal, and 65535 is white }
    lightness: BGRAWord;
    {** Opacity of the pixel. 0 is transparent and 65535 is opaque }
    alpha: BGRAWord;

    class Operator{$IFDEF OBJ}:={$ELSE}Implicit{$ENDIF}(const Source: THSLAPixel): TBGRAPixel;
    class Operator{$IFDEF OBJ}:={$ELSE}Implicit{$ENDIF}(const Source: TBGRAPixel): THSLAPixel;
    class Operator{$IFDEF OBJ}:={$ELSE}Implicit{$ENDIF}(const Source: THSLAPixel): TExpandedPixel;
    class Operator{$IFDEF OBJ}:={$ELSE}Implicit{$ENDIF}(const Source: TExpandedPixel): THSLAPixel;
    class Operator{$IFDEF OBJ}:={$ELSE}Implicit{$ENDIF}(const AValue: THSLAPixel): TColor;
    class Operator{$IFDEF OBJ}:={$ELSE}Implicit{$ENDIF}(const AValue: TColor): THSLAPixel;
  end;

  {** Creates a pixel with given HSLA values, where A stands for alpha }
  function HSLA(hue, saturation, lightness, alpha: BGRAWord): THSLAPixel; overload; {$ifdef inline}inline;{$endif}
  {** Creates an opaque pixel with given HSL values }
  function HSLA(hue, saturation, lightness: BGRAWord): THSLAPixel; overload; {$ifdef inline}inline;{$endif}
  {** Converts a pixel from sRGB to HSL color space }
  function BGRAToHSLA(c: TBGRAPixel): THSLAPixel;
  {** Converts a pixel from gamma expanded RGB to HSL color space }
  function ExpandedToHSLA(const ec: TExpandedPixel): THSLAPixel;
  {** Converts a pixel from HSL colorspace to sRGB }
  function HSLAToBGRA(const c: THSLAPixel): TBGRAPixel;
  {** Converts a pixel from HSL colorspace to gamma expanded RGB }
  function HSLAToExpanded(const c: THSLAPixel): TExpandedPixel;
  {** Computes the hue difference }
  function HueDiff(h1, h2: BGRAWord): BGRAWord;
  {** Returns the hue of a gamma expanded pixel }
  function GetHue(ec: TExpandedPixel): BGRAWord;

type
  {* Pixel color defined in corrected HSL colorspace. G stands for corrected hue
     and B stands for actual brightness. Values range from 0 to 65535 }
  PGSBAPixel  = ^TGSBAPixel;
  TGSBAPixel = packed record
    {** Hue of the pixel. Extremum values 0 and 65535 are red }
    hue: BGRAWord;
    {** Saturation of the color. 0 is gray and 65535 is the brightest color (excluding white) }
    saturation: BGRAWord;
    {** Actual perceived brightness. 0 is black, 32768 is normal, and 65535 is white }
    lightness: BGRAWord;
    {** Opacity of the pixel. 0 is transparent and 65535 is opaque }
    alpha: BGRAWord;

    class Operator{$IFDEF OBJ}:={$ELSE}Implicit{$ENDIF}(const Source: TGSBAPixel): TBGRAPixel;
    class Operator{$IFDEF OBJ}:={$ELSE}Implicit{$ENDIF}(const Source: TBGRAPixel): TGSBAPixel;
    class Operator{$IFDEF OBJ}:={$ELSE}Implicit{$ENDIF}(const Source: TGSBAPixel): TExpandedPixel;
    class Operator{$IFDEF OBJ}:={$ELSE}Implicit{$ENDIF}(const Source: TExpandedPixel): TGSBAPixel;
    class Operator{$IFDEF OBJ}:={$ELSE}Implicit{$ENDIF}(const AValue: TColor): TGSBAPixel;
    class Operator{$IFDEF OBJ}:={$ELSE}Implicit{$ENDIF}(const AValue: TGSBAPixel): TColor;
    class Operator{$IFDEF OBJ}:={$ELSE}Implicit{$ENDIF}(const Source: TGSBAPixel): THSLAPixel; //no conversion, just copying for backward compatibility (use ToHSLAPixel instead for conversion)
    class Operator{$IFDEF OBJ}:={$ELSE}Implicit{$ENDIF}(const Source: THSLAPixel): TGSBAPixel; //no conversion, just copying for backward compatibility (use ToGSBAPixel instead for conversion)
  end;

  {** Converts a pixel from sRGB to correct HSL color space }
  function BGRAToGSBA(c: TBGRAPixel): TGSBAPixel;
  {** Converts a pixel from gamma expanded RGB to correct HSL color space }
  function ExpandedToGSBA(ec: TExpandedPixel): TGSBAPixel;
  {** Converts a G hue (GSBA) to a H hue (HSLA) }
  function GtoH(ghue: BGRAWord): BGRAWord;
  {** Converts a H hue (HSLA) to a G hue (GSBA) }
  function HtoG(hue: BGRAWord): BGRAWord;
  {** Converts a pixel from corrected HSL to sRGB }
  function GSBAToBGRA(c: TGSBAPixel): TBGRAPixel; overload;
  function GSBAToBGRA(const c: THSLAPixel): TBGRAPixel; overload;
  {** Converts a pixel from correct HSL to gamma expanded RGB }
  function GSBAToExpanded(c: TGSBAPixel): TExpandedPixel; overload;
  function GSBAToExpanded(const c: THSLAPixel): TExpandedPixel; overload;
  {** Converts a pixel from correct HSL to usual HSL }
  function GSBAToHSLA(const c: TGSBAPixel): THSLAPixel; overload;
  function GSBAToHSLA(const c: THSLAPixel): THSLAPixel; overload;
  function HSLAToGSBA(const c: THSLAPixel): TGSBAPixel;

type
  { TBGRAPixelBasicHelper }

  TBGRAPixelBasicHelper = record helper for TBGRAPixel
    function ToExpanded: TExpandedPixel;
    procedure FromExpanded(const AValue: TExpandedPixel);
    function ToHSLAPixel: THSLAPixel;
    procedure FromHSLAPixel(const AValue: THSLAPixel);
    function ToGSBAPixel: TGSBAPixel;
    procedure FromGSBAPixel(const AValue: TGSBAPixel); overload;
    procedure FromGSBAPixel(const AValue: THSLAPixel); overload;
    function ToColorF(AGammaExpansion: boolean): TColorF;
    procedure FromColorF(const AValue: TColorF; AGammaCompression: boolean);
  end;

  { TExpandedPixelBasicHelper }

  TExpandedPixelBasicHelper = record helper for TExpandedPixel
    function ToFPColor(AGammaCompression: boolean = true): TFPColor;
    procedure FromFPColor(const AValue: TFPColor; AGammaExpansion: boolean = true);
    function ToColor: TColor;
    procedure FromColor(const AValue: TColor);
    function ToBGRAPixel: TBGRAPixel;
    procedure FromBGRAPixel(AValue: TBGRAPixel);
    function ToHSLAPixel: THSLAPixel;
    procedure FromHSLAPixel(const AValue: THSLAPixel);
    function ToGSBAPixel: TGSBAPixel;
    procedure FromGSBAPixel(const AValue: TGSBAPixel); overload;
    procedure FromGSBAPixel(const AValue: THSLAPixel); overload;
  end;

type
  { TFPColorBasicHelper }

  TFPColorBasicHelper = record helper for TFPColor
    function ToColor: TColor;
    procedure FromColor(const AValue: TColor);
    function ToBGRAPixel: TBGRAPixel;
    procedure FromBGRAPixel(AValue: TBGRAPixel);
    function ToExpanded(AGammaExpansion: boolean = true): TExpandedPixel;
    procedure FromExpanded(const AValue: TExpandedPixel; AGammaCompression: boolean = true);
  end;

  { THSLAPixelBasicHelper }

  THSLAPixelBasicHelper = record helper for THSLAPixel
    function ToColor: TColor;
    procedure FromColor(const AValue: TColor);
    function ToBGRAPixel: TBGRAPixel;
    procedure FromBGRAPixel(AValue: TBGRAPixel);
    function ToGSBAPixel: TGSBAPixel;
    procedure FromGSBAPixel(AValue: TGSBAPixel);
    function ToExpanded: TExpandedPixel;
    procedure FromExpanded(AValue: TExpandedPixel);
  end;

type
  { TGSBAPixelBasicHelper }

  TGSBAPixelBasicHelper = record helper for TGSBAPixel
    function ToColor: TColor;
    procedure FromColor(const AValue: TColor);
    function ToBGRAPixel: TBGRAPixel;
    procedure FromBGRAPixel(AValue: TBGRAPixel);
    function ToHSLAPixel: THSLAPixel;
    procedure FromHSLAPixel(AValue: THSLAPixel);
    function ToExpanded: TExpandedPixel;
    procedure FromExpanded(AValue: TExpandedPixel);
  end;

{$ENDIF}


{$IFDEF INCLUDE_IMPLEMENTATION}
{$UNDEF INCLUDE_IMPLEMENTATION}
{ TBGRAPixel }

function TBGRAPixel.GetClassIntensity: BGRAWord;
begin
  result := GetIntensity(self);
end;

function TBGRAPixel.GetClassLightness: BGRAWord;
begin
  result := GetLightness(self);
end;

procedure TBGRAPixel.SetClassIntensity(AValue: BGRAWord);
begin
  self := SetIntensity(self, AValue);
end;

procedure TBGRAPixel.SetClassLightness(AValue: BGRAWord);
begin
  self := SetLightness(self, AValue);
end;

procedure TBGRAPixel.FromRGB(ARed, AGreen, ABlue: Byte; AAlpha: Byte);
begin
  red := ARed;
  green := AGreen;
  blue := ABlue;
  alpha := AAlpha;
end;

procedure TBGRAPixel.FromColor(AColor: TColor; AAlpha: Byte);
begin
  if AColor = clNone then
    Self := BGRAPixelTransparent
  else
  begin
    RedGreenBlue(ColorToRGB(AColor), red,green,blue);
    alpha := AAlpha;
  end;
end;

procedure TBGRAPixel.FromString(AStr: string);
begin
  Self := StrToBGRA(AStr);
end;

procedure TBGRAPixel.FromFPColor(AColor: TFPColor);
begin
  self := FPColorToBGRA(AColor);
end;

procedure TBGRAPixel.ToRGB(out ARed, AGreen, ABlue, AAlpha: Byte);
begin
  ARed := red;
  AGreen := green;
  ABlue := blue;
  AAlpha := alpha;
end;

procedure TBGRAPixel.ToRGB(out ARed, AGreen, ABlue: Byte);
begin
  ARed := red;
  AGreen := green;
  ABlue := blue
end;

function TBGRAPixel.ToColor: TColor;
begin
  if alpha = 0 then
    result := clNone
  else
    result := RGBToColor(red,green,blue);
end;

function TBGRAPixel.ToString: string;
begin
  result := BGRAToStr(Self, CSSColors);
end;

function TBGRAPixel.ToGrayscale(AGammaCorrection: boolean): TBGRAPixel;
begin
  if AGammaCorrection then
    result := BGRAToGrayscale(self)
  else
    result := BGRAToGrayscaleLinear(self);
end;

function TBGRAPixel.ToFPColor: TFPColor;
begin
  result := BGRAToFPColor(Self);
end;

class operator TBGRAPixel.{$IFDEF OBJ}:={$ELSE}Implicit{$ENDIF}(Source: TBGRAPixel): TColor;
begin
  result := Source.ToColor;
end;

class operator TBGRAPixel.{$IFDEF OBJ}:={$ELSE}Implicit{$ENDIF}(Source: TColor): TBGRAPixel;
begin
  result.FromColor(Source);
end;

class Operator TBGRAPixel.{$IFDEF OBJ}<>{$ELSE}NotEqual{$ENDIF}(v1, v2: TBGRAPixel): boolean;
begin
  result := (v1.blue <> v2.blue) and (v1.green <> v2.green) and (v1.red <> v2.red) and (v1.alpha <> v2.alpha);
end;

class Operator TBGRAPixel.{$IFDEF OBJ}={$ELSE}Equal{$ENDIF}(v1, v2: TBGRAPixel): boolean;
begin
  result := (v1.blue = v2.blue) and (v1.green = v2.green) and (v1.red = v2.red) and (v1.alpha = v2.alpha);
end;

{ The gamma correction is approximated here by a power function }
var
  GammaExpFactor   : single; //exponent

const
  redWeightShl10   = 306; // = 0.299
  greenWeightShl10 = 601; // = 0.587
  blueWeightShl10  = 117; // = 0.114

procedure BGRANoGamma;
var i: integer;
begin
  GammaExpFactor := 1;
  for i := 0 to 255 do
    GammaExpansionTab[i] := (i shl 8) + i;
  for i := 0 to 65535 do
    GammaCompressionTab[i] := i shr 8;
end;

procedure BGRASetGamma(AGamma: single);
var
  GammaLinearFactor: single;
  i,j,prevpos,nextpos,midpos: BGRANativeInt;
begin
  if AGamma = 1 then
  begin
    BGRANoGamma;
    exit;
  end;
  GammaExpFactor := AGamma;
  //the linear factor is used to normalize expanded values in the range 0..65535
  GammaLinearFactor := 65535 / power(255, GammaExpFactor);
  GammaExpansionTab[0] := 0;
  GammaCompressionTab[0] := 0;
  nextpos := 0;
  for i := 0 to 255 do
  begin
    prevpos := nextpos;
    midpos := round(power(i, GammaExpFactor) * GammaLinearFactor);
    if i = 255 then
      nextpos := 65536
    else
      nextpos := round(power(i+0.5, GammaExpFactor) * GammaLinearFactor);
    GammaExpansionTab[i] := midpos;
    for j := prevpos to midpos-1 do
    begin
      GammaCompressionTab[j] := i;
      GammaCompressionTabFrac[j] := -128 + (j-prevpos)*128 div (midpos-prevpos);
    end;
    for j := midpos to nextpos-1 do
    begin
      GammaCompressionTab[j] := i;
      GammaCompressionTabFrac[j] := (j-midpos)*128 div (nextpos-midpos);
    end;
  end;
  GammaCompressionTab[0] := 0;
end;

function BGRAGetGamma: single;
begin
  result := GammaExpFactor;
end;

procedure AllocateExpandedPixelBuffer(var ABuffer: TExpandedPixelBuffer;
  ASize: integer);
begin
  if ASize > length(ABuffer) then
    setlength(ABuffer, max(length(ABuffer)*2,ASize));
end;

{ Apply gamma correction using conversion tables }
function GammaExpansion(c: TBGRAPixel): TExpandedPixel;
begin
  Result.red   := GammaExpansionTab[c.red];
  Result.green := GammaExpansionTab[c.green];
  Result.blue  := GammaExpansionTab[c.blue];
  Result.alpha := c.alpha shl 8 + c.alpha;
end;

function GammaCompression(const ec: TExpandedPixel): TBGRAPixel;
begin
  Result.red   := GammaCompressionTab[ec.red];
  Result.green := GammaCompressionTab[ec.green];
  Result.blue  := GammaCompressionTab[ec.blue];
  Result.alpha := ec.alpha shr 8;
end;

function GammaCompression(red, green, blue, alpha: BGRAWord): TBGRAPixel;
begin
  Result.red   := GammaCompressionTab[red];
  Result.green := GammaCompressionTab[green];
  Result.blue  := GammaCompressionTab[blue];
  Result.alpha := alpha shr 8;
end;

function GammaExpansionW(ACompressed: BGRAWord): BGRAWord;
var
  intPart: Integer;
  f,fracPart: Single;
begin
  if ACompressed = 0 then
    result := 0
  else if ACompressed = $ffff then
    result := $ffff
  else
  begin
    f := ACompressed/$101;
    intPart := trunc(f);
    fracPart := f - intPart;
    if fracPart = 0 then
      result := GammaExpansionTab[intPart]
    else
      result := round(GammaExpansionTab[intPart]*(1-fracPart)+GammaExpansionTab[intPart+1]*fracPart);
  end;
end;

function GammaCompressionW(AExpanded: BGRAWord): BGRAWord;
begin
  if AExpanded = 0 then
    result := 0
  else if AExpanded = $ffff then
    result := $ffff
  else
  begin
    result := GammaCompressionTab[AExpanded];
    result := (result shl 8) + result;
    result := result + GammaCompressionTabFrac[AExpanded];
  end;
end;

{ The intensity is defined here as the maximum value of any color component }
function GetIntensity(const c: TExpandedPixel): BGRAWord; {$ifdef inline}inline;{$endif}
begin
  Result := c.red;
  if c.green > Result then
    Result := c.green;
  if c.blue > Result then
    Result := c.blue;
end;

function SetIntensity(const c: TExpandedPixel; intensity: BGRAWord): TExpandedPixel;
var
  curIntensity: BGRAWord;
begin
  curIntensity := GetIntensity(c);
  if curIntensity = 0 then //suppose it's gray if there is no color information
  begin
    Result.red := intensity;
    Result.green := intensity;
    Result.blue := intensity;
    result.alpha := c.alpha;
  end
  else
  begin
    //linear interpolation to reached wanted intensity
    Result.red   := (c.red * intensity + (curIntensity shr 1)) div curIntensity;
    Result.green := (c.green * intensity + (curIntensity shr 1)) div curIntensity;
    Result.blue  := (c.blue * intensity + (curIntensity shr 1)) div curIntensity;
    Result.alpha := c.alpha;
  end;
end;

{ The lightness here is defined as the subjective sensation of luminosity, where
  blue is the darkest component and green the lightest }
function GetLightness(const c: TExpandedPixel): BGRAWord; {$ifdef inline}inline;{$endif}
begin
  Result := (c.red * redWeightShl10 + c.green * greenWeightShl10 +
    c.blue * blueWeightShl10 + 512) shr 10;
end;

function SetLightness(const c: TExpandedPixel; lightness: BGRAWord): TExpandedPixel;
var
  curLightness: BGRAWord;
begin
  curLightness := GetLightness(c);
  if lightness = curLightness then
  begin //no change
    Result := c;
    exit;
  end;
  result := SetLightness(c, lightness, curLightness);
end;

function SetLightness(const c: TExpandedPixel; lightness: BGRAWord; curLightness: BGRAWord): TExpandedPixel;
var
  AddedWhiteness, maxBeforeWhite: BGRAWord;
  clip: boolean;
begin
  if lightness = curLightness then
  begin //no change
    Result := c;
    exit;
  end;
  if lightness = 65535 then //set to white
  begin
    Result.red   := 65535;
    Result.green := 65535;
    Result.blue  := 65535;
    Result.alpha := c.alpha;
    exit;
  end;
  if lightness = 0 then  //set to black
  begin
    Result.red   := 0;
    Result.green := 0;
    Result.blue  := 0;
    Result.alpha := c.alpha;
    exit;
  end;
  if curLightness = 0 then  //set from black
  begin
    Result.red   := lightness;
    Result.green := lightness;
    Result.blue  := lightness;
    Result.alpha := c.alpha;
    exit;
  end;
  if lightness < curLightness then //darker is easy
  begin
    result.alpha:= c.alpha;
    result.red := (c.red * lightness + (curLightness shr 1)) div curLightness;
    result.green := (c.green * lightness + (curLightness shr 1)) div curLightness;
    result.blue := (c.blue * lightness + (curLightness shr 1)) div curLightness;
    exit;
  end;
  //lighter and grayer
  Result := c;
  AddedWhiteness := lightness - curLightness;
  maxBeforeWhite := 65535 - AddedWhiteness;
  clip   := False;
  if Result.red <= maxBeforeWhite then
    Inc(Result.red, AddedWhiteness)
  else
  begin
    Result.red := 65535;
    clip := True;
  end;
  if Result.green <= maxBeforeWhite then
    Inc(Result.green, AddedWhiteness)
  else
  begin
    Result.green := 65535;
    clip := True;
  end;
  if Result.blue <= maxBeforeWhite then
    Inc(Result.blue, AddedWhiteness)
  else
  begin
    Result.blue := 65535;
    clip := True;
  end;

  if clip then //light and whiter
  begin
    curLightness   := GetLightness(Result);
    addedWhiteness := lightness - curLightness;
    maxBeforeWhite := 65535 - curlightness;
    Result.red     := Result.red + addedWhiteness * (65535 - Result.red) div
      maxBeforeWhite;
    Result.green   := Result.green + addedWhiteness * (65535 - Result.green) div
      maxBeforeWhite;
    Result.blue    := Result.blue + addedWhiteness * (65535 - Result.blue) div
      maxBeforeWhite;
  end;
end;

function ColorImportance(ec: TExpandedPixel): BGRAWord;
var min,max: BGRAWord;
begin
  min := ec.red;
  max := ec.red;
  if ec.green > max then
    max := ec.green
  else
  if ec.green < min then
    min := ec.green;
  if ec.blue > max then
    max := ec.blue
  else
  if ec.blue < min then
    min  := ec.blue;
  result := max - min;
end;

{ Merge two colors of same importance }
function MergeBGRA(ec1, ec2: TExpandedPixel): TExpandedPixel;
var c12: BGRACardinal;
begin
  if (ec1.alpha = 0) then
    Result := ec2
  else
  if (ec2.alpha = 0) then
    Result := ec1
  else
  begin
    c12 := ec1.alpha + ec2.alpha;
    Result.red   := (BGRAInt64(ec1.red) * ec1.alpha + BGRAInt64(ec2.red) * ec2.alpha + c12 shr 1) div c12;
    Result.green := (BGRAInt64(ec1.green) * ec1.alpha + BGRAInt64(ec2.green) * ec2.alpha + c12 shr 1) div c12;
    Result.blue  := (BGRAInt64(ec1.blue) * ec1.alpha + BGRAInt64(ec2.blue) * ec2.alpha + c12 shr 1) div c12;
    Result.alpha := (c12 + 1) shr 1;
  end;
end;

function LessStartSlope65535(value: BGRAWord): BGRAWord;
var factor: BGRAWord;
begin
  factor := 4096 - (not value)*3 shr 7;
  result := value*factor shr 12;
end;

function ExpandedDiff(ec1, ec2: TExpandedPixel): BGRAWord;
var
  CompRedAlpha1, CompGreenAlpha1, CompBlueAlpha1, CompRedAlpha2,
  CompGreenAlpha2, CompBlueAlpha2: integer;
  DiffAlpha: BGRAWord;
  ColorDiff: BGRAWord;
  TempHueDiff: BGRAWord;
begin
  CompRedAlpha1 := ec1.red * ec1.alpha shr 16; //gives 0..65535
  CompGreenAlpha1 := ec1.green * ec1.alpha shr 16;
  CompBlueAlpha1 := ec1.blue * ec1.alpha shr 16;
  CompRedAlpha2 := ec2.red * ec2.alpha shr 16;
  CompGreenAlpha2 := ec2.green * ec2.alpha shr 16;
  CompBlueAlpha2 := ec2.blue * ec2.alpha shr 16;
  Result    := (Abs(CompRedAlpha2 - CompRedAlpha1)*redWeightShl10 +
    Abs(CompBlueAlpha2 - CompBlueAlpha1)*blueWeightShl10 +
    Abs(CompGreenAlpha2 - CompGreenAlpha1)*greenWeightShl10) shr 10;
  ColorDiff := min(ColorImportance(ec1),ColorImportance(ec2));
  if ColorDiff > 0 then
  begin
    TempHueDiff := HueDiff(HtoG(GetHue(ec1)),HtoG(GetHue(ec2)));
    if TempHueDiff < 32768 then
      TempHueDiff := LessStartSlope65535(TempHueDiff shl 1) shr 4
    else
      TempHueDiff := TempHueDiff shr 3;
    Result := ((Result shr 4)* (not ColorDiff) + TempHueDiff*ColorDiff) shr 12;
  end;
  DiffAlpha := Abs(integer(ec2.Alpha) - integer(ec1.Alpha));
  if DiffAlpha > Result then
    Result := DiffAlpha;
end;

function ColorF(red, green, blue, alpha: single): TColorF;
begin
  result[1] := red;
  result[2] := green;
  result[3] := blue;
  result[4] := alpha;
end;

function BGRAToColorF(c: TBGRAPixel; AGammaExpansion: boolean): TColorF;
const OneOver255 = 1/255;
      OneOver65535 = 1/65535;
begin
  if not AGammaExpansion then
  begin
    result[1] := c.red*OneOver255;
    result[2] := c.green*OneOver255;
    result[3] := c.blue*OneOver255;
    result[4] := c.alpha*OneOver255;
  end else
  with GammaExpansion(c) do
  begin
    result[1] := red*OneOver65535;
    result[2] := green*OneOver65535;
    result[3] := blue*OneOver65535;
    result[4] := alpha*OneOver65535;
  end;
end;

function BGRAToColorF(const a: array of TBGRAPixel; AGammaExpansion: boolean
  ): ArrayOfTColorF;
var
  i: BGRANativeInt;
begin
  setlength(result, length(a));
  for i := 0 to high(a) do
    result[i] := BGRAToColorF(a[i],AGammaExpansion);
end;

function ColorFToBGRA(c: TColorF; AGammaCompression: boolean): TBGRAPixel;
begin
  if not AGammaCompression then
  begin
    result.red := Min(255,Max(0,round(c[1]*255)));
    result.green := Min(255,Max(0,round(c[1]*255)));
    result.blue := Min(255,Max(0,round(c[1]*255)));
  end else
  begin
    result.red := GammaCompressionTab[Min(65535,Max(0,round(c[1]*65535)))];
    result.green := GammaCompressionTab[Min(65535,Max(0,round(c[1]*65535)))];
    result.blue := GammaCompressionTab[Min(65535,Max(0,round(c[1]*65535)))];
  end;
  result.alpha := Min(255,Max(0,round(c[4]*255)));
end;

function GammaCompressionF(c: TColorF): TColorF;
var inv: single;
begin
  inv := 1/GammaExpFactor;
  result := ColorF(power(c[1],inv),power(c[2],inv),power(c[3],inv),c[4]);
end;

function GammaExpansionF(c: TColorF): TColorF;
begin
  result := ColorF(power(c[1],GammaExpFactor),power(c[2],GammaExpFactor),power(c[3],GammaExpFactor),c[4]);
end;

function ColorFSub(const c1, c2: TColorF): TColorF;
begin
  result[1] := c1[1]-c2[1];
  result[2] := c1[2]-c2[2];
  result[3] := c1[3]-c2[3];
  result[4] := c1[4]-c2[4];
end;

function ColorFAdd(const c1, c2: TColorF): TColorF;
begin
  result[1] := c1[1]+c2[1];
  result[2] := c1[2]+c2[2];
  result[3] := c1[3]+c2[3];
  result[4] := c1[4]+c2[4];
end;

function ColorFMult(const c1, c2: TColorF): TColorF;
begin
  result[1] := c1[1]*c2[1];
  result[2] := c1[2]*c2[2];
  result[3] := c1[3]*c2[3];
  result[4] := c1[4]*c2[4];
end;

function ColorFMult(const c1: TColorF; factor: single): TColorF;
begin
  result[1] := c1[1]*factor;
  result[2] := c1[2]*factor;
  result[3] := c1[3]*factor;
  result[4] := c1[4]*factor;
end;

{ THSLAPixel }

function HSLA(hue, saturation, lightness, alpha: BGRAWord): THSLAPixel;
begin
  Result.hue   := hue;
  Result.saturation := saturation;
  Result.lightness  := lightness;
  Result.alpha := alpha;
end;

function HSLA(hue, saturation, lightness: BGRAWord): THSLAPixel;
begin
  Result.hue   := hue;
  Result.saturation := saturation;
  Result.lightness  := lightness;
  Result.alpha := $ffff;
end;

{ Conversion from RGB value to HSL colorspace. See : http://en.wikipedia.org/wiki/HSL_color_space }
function BGRAToHSLA(c: TBGRAPixel): THSLAPixel;
begin
  result := ExpandedToHSLA(GammaExpansion(c));
end;

procedure ExpandedToHSLAInline(r,g,b: Int32Or64; var dest: THSLAPixel); {$ifdef inline}inline;{$endif}
const
  deg60  = 10922;
  deg120 = 21845;
  deg240 = 43690;
var
  min, max, minMax: Int32or64;
  UMinMax,UTwiceLightness: UInt32or64;
begin
  if g > r then
  begin
    max := g;
    min := r;
  end else
  begin
    max := r;
    min := g;
  end;
  if b > max then
    max := b else
  if b < min then
    min := b;
  minMax := max - min;

  if minMax = 0 then
    dest.hue := 0
  else
  if max = r then
    {$IFDEF FPC}{$PUSH}{$ENDIF}{$RANGECHECKS OFF}
    dest.hue := ((g - b) * deg60) div minMax
    {$IFDEF FPC}{$POP}{$ENDIF}
  else
  if max = g then
    dest.hue := ((b - r) * deg60) div minMax + deg120
  else
    {max = b} dest.hue := ((r - g) * deg60) div minMax + deg240;
  UTwiceLightness := max + min;
  if min = max then
    dest.saturation := 0 else
  begin
    UMinMax:= minMax;
    if UTwiceLightness < 65536 then
      dest.saturation := (UMinMax shl 16) div (UTwiceLightness + 1)
    else
      dest.saturation := (UMinMax shl 16) div (131072 - UTwiceLightness);
  end;
  dest.lightness := UTwiceLightness shr 1;
end;

function ExpandedToHSLA(const ec: TExpandedPixel): THSLAPixel;
begin
  result.alpha := ec.alpha;
  ExpandedToHSLAInline(ec.red,ec.green,ec.blue,result);
end;

{ Conversion from HSL colorspace to RGB. See : http://en.wikipedia.org/wiki/HSL_color_space }
function HSLAToBGRA(const c: THSLAPixel): TBGRAPixel;
var ec: TExpandedPixel;
begin
  ec := HSLAToExpanded(c);
  Result := GammaCompression(ec);
end;

function HSLAToExpanded(const c: THSLAPixel): TExpandedPixel;
const
  deg30  = 4096;
  deg60  = 8192;
  deg120 = deg60 * 2;
  deg180 = deg60 * 3;
  deg240 = deg60 * 4;
  deg360 = deg60 * 6;

  function ComputeColor(p, q: Int32or64; h: Int32or64): Int32or64; {$ifdef inline}inline;{$endif}
  begin
    if h < deg180 then
    begin
      if h < deg60 then
        Result := p + ((q - p) * h + deg30) div deg60
      else
        Result := q
    end else
    begin
      if h < deg240 then
        Result := p + ((q - p) * (deg240 - h) + deg30) div deg60
      else
        Result := p;
    end;
  end;

var
  q, p, L, S, H: Int32or64;
begin
  L := c.lightness;
  S := c.saturation;
  if S = 0 then  //gray
  begin
    result.red   := L;
    result.green := L;
    result.blue  := L;
    result.alpha := c.alpha;
    exit;
  end;
  {$hints off}
  if L < 32768 then
    q := (L shr 1) * ((65535 + S) shr 1) shr 14
  else
    q := L + S - ((L shr 1) *
      (S shr 1) shr 14);
  {$hints on}
  if q > 65535 then q := 65535;
  p   := (L shl 1) - q;
  if p > 65535 then p := 65535;
  H := c.hue * deg360 shr 16;
  result.green := ComputeColor(p, q, H);
  inc(H, deg120);
  if H > deg360 then Dec(H, deg360);
  result.red   := ComputeColor(p, q, H);
  inc(H, deg120);
  if H > deg360 then Dec(H, deg360);
  result.blue  := ComputeColor(p, q, H);
  result.alpha := c.alpha;
end;

function HueDiff(h1, h2: BGRAWord): BGRAWord;
begin
  result := abs(integer(h1)-integer(h2));
  if result > 32768 then result := 65536-result;
end;

function GetHue(ec: TExpandedPixel): BGRAWord;
const
  deg60  = 8192;
  deg120 = deg60 * 2;
  deg240 = deg60 * 4;
  deg360 = deg60 * 6;
var
  min, max, minMax: integer;
  r,g,b: integer;
begin
  r := ec.red;
  g := ec.green;
  b := ec.blue;
  min := r;
  max := r;
  if g > max then
    max := g
  else
  if g < min then
    min := g;
  if b > max then
    max := b
  else
  if b < min then
    min  := b;
  minMax := max - min;

  if minMax = 0 then
    Result := 0
  else
  if max = r then
    Result := (((g - b) * deg60) div
      minMax + deg360) mod deg360
  else
  if max = g then
    Result := ((b - r) * deg60) div minMax + deg120
  else
    {max = b} Result :=
      ((r - g) * deg60) div minMax + deg240;

  Result   := (Result shl 16) div deg360; //normalize
end;

{ TGSBAPixel }

function BGRAToGSBA(c: TBGRAPixel): TGSBAPixel;
var lightness: UInt32Or64;
    red,green,blue: Int32or64;
    hsla: THSLAPixel;
begin
  red   := GammaExpansionTab[c.red];
  green := GammaExpansionTab[c.green];
  blue  := GammaExpansionTab[c.blue];
  hsla.alpha := c.alpha shl 8 + c.alpha;

  lightness := (red * redWeightShl10 + green * greenWeightShl10 +
    blue * blueWeightShl10 + 512) shr 10;

  ExpandedToHSLAInline(red,green,blue,hsla);
  result := TGSBAPixel(hsla);

  if result.lightness > 32768 then
    result.saturation := result.saturation* UInt32or64(not result.lightness) div 32767;
  result.lightness := lightness;
  result.hue := HtoG(result.hue);
end;

function ExpandedToGSBA(ec: TExpandedPixel): TGSBAPixel;
var lightness: UInt32Or64;
    red,green,blue: Int32or64;
    hsla: THSLAPixel;
begin
  red   := ec.red;
  green := ec.green;
  blue  := ec.blue;
  hsla.alpha := ec.alpha;

  lightness := (red * redWeightShl10 + green * greenWeightShl10 +
    blue * blueWeightShl10 + 512) shr 10;

  ExpandedToHSLAInline(red,green,blue,hsla);
  result := TGSBAPixel(hsla);

  if result.lightness > 32768 then
    result.saturation := result.saturation* UInt32or64(not result.lightness) div 32767;
  result.lightness := lightness;
  result.hue := HtoG(result.hue);
end;

function GtoH(ghue: BGRAWord): BGRAWord;
const
  segment: array[0..5] of BGRANativeUInt =
     (13653, 10923, 8192, 13653, 10923, 8192);
var g: BGRANativeUInt;
begin
  g := ghue;
  if g < segment[0] then
    result := g * 10923 div segment[0]
  else
  begin
    g := g - segment[0];
    if g < segment[1] then
      result := g * (21845-10923) div segment[1] + 10923
    else
    begin
      g := g -segment[1];
      if g < segment[2] then
        result := g * (32768-21845) div segment[2] + 21845
      else
      begin
        g := g -segment[2];
        if g < segment[3] then
          result := g * (43691-32768) div segment[3] + 32768
        else
        begin
          g := g -segment[3];
          if g < segment[4] then
            result := g * (54613-43691) div segment[4] + 43691
          else
          begin
            g := g -segment[4];
            result := g * (65536-54613) div segment[5] + 54613;
          end;
        end;
      end;
    end;
  end;
end;

function HtoG(hue: BGRAWord): BGRAWord;
const
  segmentDest: array[0..5] of BGRANativeUInt =
     (13653, 10923, 8192, 13653, 10923, 8192);
  segmentSrc: array[0..5] of BGRANativeUInt =
     (10923, 10922, 10923, 10923, 10922, 10923);
var
  h,g: BGRANativeUInt;
begin
  h := hue;
  if h < segmentSrc[0] then
    g := h * segmentDest[0] div segmentSrc[0]
  else
  begin
    g := segmentDest[0];
    h := h -segmentSrc[0];
    if h < segmentSrc[1] then
      g := g + (h * segmentDest[1] div segmentSrc[1])
    else
    begin
      g := g + segmentDest[1];
      h := h - segmentSrc[1];
      if h < segmentSrc[2] then
        g := g + (h * segmentDest[2] div segmentSrc[2])
      else
      begin
        g := g +segmentDest[2];
        h := h -segmentSrc[2];
        if h < segmentSrc[3] then
          g := g +(h * segmentDest[3] div segmentSrc[3])
        else
        begin
          g := g +segmentDest[3];
          h := h -segmentSrc[3];
          if h < segmentSrc[4] then
            g := g +(h * segmentDest[4] div segmentSrc[4])
          else
          begin
            g := g +segmentDest[4];
            h := h -segmentSrc[4];
            g := g +(h * segmentDest[5] div segmentSrc[5]);
          end;
        end;
      end;
    end;
  end;
  result := g;
end;

function GSBAToBGRA(c: TGSBAPixel): TBGRAPixel;
var ec: TExpandedPixel;
    lightness: BGRAWord;
begin
  c.hue := GtoH(c.hue);
  lightness := c.lightness;
  c.lightness := 32768;
  ec := HSLAToExpanded(THSLAPixel(c));
  result := GammaCompression(SetLightness(ec, lightness));
end;

function GSBAToBGRA(const c: THSLAPixel): TBGRAPixel;
begin
  result := GSBAToBGRA(TGSBAPixel(c));
end;

function GSBAToExpanded(c: TGSBAPixel): TExpandedPixel;
var lightness: BGRAWord;
begin
  c.hue := GtoH(c.hue);
  lightness := c.lightness;
  c.lightness := 32768;
  result := SetLightness(HSLAToExpanded(THSLAPixel(c)),lightness);
end;

function GSBAToExpanded(const c: THSLAPixel): TExpandedPixel;
begin
  result := GSBAToExpanded(TGSBAPixel(c));
end;

function GSBAToHSLA(const c: TGSBAPixel): THSLAPixel;
begin
  result := ExpandedToHSLA(GSBAToExpanded(c));
end;

function GSBAToHSLA(const c: THSLAPixel): THSLAPixel;
begin
  result := ExpandedToHSLA(GSBAToExpanded(TGSBAPixel(c)));
end;

function HSLAToGSBA(const c: THSLAPixel): TGSBAPixel;
begin
  result := ExpandedToGSBA(HSLAToExpanded(c));
end;

{ TBGRAPixelBasicHelper }

function TBGRAPixelBasicHelper.ToExpanded: TExpandedPixel;
begin
  result := GammaExpansion(self);
end;

procedure TBGRAPixelBasicHelper.FromExpanded(const AValue: TExpandedPixel);
begin
  Self := GammaCompression(AValue);
end;

function TBGRAPixelBasicHelper.ToHSLAPixel: THSLAPixel;
begin
  result := BGRAToHSLA(Self);
end;

procedure TBGRAPixelBasicHelper.FromHSLAPixel(const AValue: THSLAPixel);
begin
  Self := HSLAToBGRA(AValue);
end;

function TBGRAPixelBasicHelper.ToGSBAPixel: TGSBAPixel;
begin
  result := BGRAToGSBA(Self);
end;

procedure TBGRAPixelBasicHelper.FromGSBAPixel(const AValue: TGSBAPixel);
begin
  Self := GSBAToBGRA(AValue);
end;

procedure TBGRAPixelBasicHelper.FromGSBAPixel(const AValue: THSLAPixel);
begin
  Self := GSBAToBGRA(AValue);
end;

function TBGRAPixelBasicHelper.ToColorF(AGammaExpansion: boolean): TColorF;
begin
  result := BGRAToColorF(Self,AGammaExpansion);
end;

procedure TBGRAPixelBasicHelper.FromColorF(const AValue: TColorF;
    AGammaCompression: boolean);
begin
  Self := ColorFToBGRA(AValue,AGammaCompression);
end;

{ TExpandedPixelBasicHelper }

function TExpandedPixelBasicHelper.ToFPColor(AGammaCompression: boolean): TFPColor;
begin
  if AGammaCompression then
  begin
    result.red := GammaCompressionW(self.red);
    result.green := GammaCompressionW(self.green);
    result.blue := GammaCompressionW(self.blue);
  end else
  begin
    result.red := self.red;
    result.green := self.green;
    result.blue := self.blue;
  end;
  result.alpha := self.alpha;
end;

procedure TExpandedPixelBasicHelper.FromFPColor(const AValue: TFPColor;
  AGammaExpansion: boolean);
begin
  if AGammaExpansion then
  begin
    self.red := GammaExpansionW(AValue.red);
    self.green := GammaExpansionW(AValue.green);
    self.blue := GammaExpansionW(AValue.blue);
  end else
  begin
    self.red := AValue.red;
    self.green := AValue.green;
    self.blue := AValue.blue;
  end;
  self.alpha := AValue.alpha;
end;

function TExpandedPixelBasicHelper.ToColor: TColor;
begin
  result := BGRAToColor(GammaCompression(self));
end;

procedure TExpandedPixelBasicHelper.FromColor(const AValue: TColor);
begin
  self := GammaExpansion(ColorToBGRA(AValue));
end;

function TExpandedPixelBasicHelper.ToBGRAPixel: TBGRAPixel;
begin
  result := GammaCompression(Self);
end;

procedure TExpandedPixelBasicHelper.FromBGRAPixel(AValue: TBGRAPixel);
begin
  Self := GammaExpansion(AValue);
end;

function TExpandedPixelBasicHelper.ToHSLAPixel: THSLAPixel;
begin
  result := ExpandedToHSLA(Self);
end;

procedure TExpandedPixelBasicHelper.FromHSLAPixel(const AValue: THSLAPixel);
begin
  Self := HSLAToExpanded(AValue);
end;

function TExpandedPixelBasicHelper.ToGSBAPixel: TGSBAPixel;
begin
  result := ExpandedToGSBA(Self);
end;

procedure TExpandedPixelBasicHelper.FromGSBAPixel(const AValue: TGSBAPixel);
begin
  Self := GSBAToExpanded(AValue);
end;

procedure TExpandedPixelBasicHelper.FromGSBAPixel(const AValue: THSLAPixel);
begin
  Self := GSBAToExpanded(AValue);
end;


class operator TExpandedPixel.{$IFDEF OBJ}:={$ELSE}Implicit{$ENDIF}(const AValue: TExpandedPixel): TColor;
begin Result := BGRAToColor(GammaCompression(AValue)); end;

class operator TExpandedPixel.{$IFDEF OBJ}:={$ELSE}Implicit{$ENDIF}(const AValue: TColor): TExpandedPixel;
begin Result := GammaExpansion(ColorToBGRA(ColorToRGB(AValue))) end;

class operator TExpandedPixel.{$IFDEF OBJ}:={$ELSE}Implicit{$ENDIF}(const Source: TExpandedPixel): TBGRAPixel;
begin
  result := GammaCompression(Source);
end;

class operator TExpandedPixel.{$IFDEF OBJ}:={$ELSE}Implicit{$ENDIF}(const Source: TBGRAPixel): TExpandedPixel;
begin
  result := GammaExpansion(Source);
end;

function TExpandedPixel.ToFPColor(AGammaCompression : boolean) : TFPColor;
begin
  if AGammaCompression then
  begin
    result.red := GammaCompressionW(self.red);
    result.green := GammaCompressionW(self.green);
    result.blue := GammaCompressionW(self.blue);
  end else
  begin
    result.red := self.red;
    result.green := self.green;
    result.blue := self.blue;
  end;
  result.alpha := self.alpha;
end;

procedure TExpandedPixel.FromFPColor(const AValue : TFPColor;
 AGammaExpansion : boolean);
begin
  if AGammaExpansion then
  begin
    self.red := GammaExpansionW(AValue.red);
    self.green := GammaExpansionW(AValue.green);
    self.blue := GammaExpansionW(AValue.blue);
  end else
  begin
    self.red := AValue.red;
    self.green := AValue.green;
    self.blue := AValue.blue;
  end;
  self.alpha := AValue.alpha;
end;

{ TFPColorBasicHelper }

function TFPColorBasicHelper.ToColor: TColor;
begin
  result := FPColorToTColor(self);
end;

procedure TFPColorBasicHelper.FromColor(const AValue: TColor);
begin
  self := TColorToFPColor(AValue);
end;

function TFPColorBasicHelper.ToBGRAPixel: TBGRAPixel;
begin
  result := FPColorToBGRA(self);
end;

procedure TFPColorBasicHelper.FromBGRAPixel(AValue: TBGRAPixel);
begin
  self := BGRAToFPColor(AValue);
end;

function TFPColorBasicHelper.ToExpanded(AGammaExpansion: boolean): TExpandedPixel;
begin
  result.FromFPColor(self, AGammaExpansion);
end;

procedure TFPColorBasicHelper.FromExpanded(const AValue: TExpandedPixel;
  AGammaCompression: boolean);
begin
  self := AValue.ToFPColor(AGammaCompression);
end;

{ THSLAPixelBasicHelper }

function THSLAPixelBasicHelper.ToColor: TColor;
begin
  result := BGRAToColor(HSLAToBGRA(self));
end;

procedure THSLAPixelBasicHelper.FromColor(const AValue: TColor);
begin
  self := BGRAToHSLA(ColorToBGRA(AValue));
end;

function THSLAPixelBasicHelper.ToBGRAPixel: TBGRAPixel;
begin
  result := HSLAToBGRA(self);
end;

procedure THSLAPixelBasicHelper.FromBGRAPixel(AValue: TBGRAPixel);
begin
  self := BGRAToHSLA(AValue);
end;

function THSLAPixelBasicHelper.ToGSBAPixel: TGSBAPixel;
begin
  result := HSLAToGSBA(self);
end;

procedure THSLAPixelBasicHelper.FromGSBAPixel(AValue: TGSBAPixel);
begin
  self := GSBAToHSLA(AValue);
end;

function THSLAPixelBasicHelper.ToExpanded: TExpandedPixel;
begin
  result := HSLAToExpanded(Self);
end;

procedure THSLAPixelBasicHelper.FromExpanded(AValue: TExpandedPixel);
begin
  Self := ExpandedToHSLA(AValue);
end;

class operator THSLAPixel.{$IFDEF OBJ}:={$ELSE}Implicit{$ENDIF}(const Source: THSLAPixel): TBGRAPixel;
begin
  result := HSLAToBGRA(Source);
end;

class operator THSLAPixel.{$IFDEF OBJ}:={$ELSE}Implicit{$ENDIF}(const Source: TBGRAPixel): THSLAPixel;
begin
  result := BGRAToHSLA(Source);
end;

class operator THSLAPixel.{$IFDEF OBJ}:={$ELSE}Implicit{$ENDIF}(const Source: THSLAPixel): TExpandedPixel;
begin
  result := HSLAToExpanded(Source);
end;

class operator THSLAPixel.{$IFDEF OBJ}:={$ELSE}Implicit{$ENDIF}(const Source: TExpandedPixel): THSLAPixel;
begin
  result := ExpandedToHSLA(Source);
end;

class operator THSLAPixel.{$IFDEF OBJ}:={$ELSE}Implicit{$ENDIF} (const AValue: THSLAPixel): TColor;
begin Result := BGRAToColor(HSLAToBGRA(AValue)) end;

class operator THSLAPixel.{$IFDEF OBJ}:={$ELSE}Implicit{$ENDIF} (const AValue: TColor): THSLAPixel;
begin Result := BGRAToHSLA(ColorToBGRA(ColorToRGB(AValue))) end;
{ TGSBAPixelBasicHelper }

function TGSBAPixelBasicHelper.ToColor: TColor;
begin
  result := BGRAToColor(GSBAToBGRA(self));
end;

procedure TGSBAPixelBasicHelper.FromColor(const AValue: TColor);
begin
  self := BGRAToGSBA(ColorToBGRA(AValue));
end;

function TGSBAPixelBasicHelper.ToBGRAPixel: TBGRAPixel;
begin
  result := GSBAToBGRA(self);
end;

procedure TGSBAPixelBasicHelper.FromBGRAPixel(AValue: TBGRAPixel);
begin
  self := BGRAToGSBA(AValue);
end;

function TGSBAPixelBasicHelper.ToHSLAPixel: THSLAPixel;
begin
  result := GSBAToHSLA(self);
end;

procedure TGSBAPixelBasicHelper.FromHSLAPixel(AValue: THSLAPixel);
begin
  self := HSLAToGSBA(AValue);
end;

function TGSBAPixelBasicHelper.ToExpanded: TExpandedPixel;
begin
  result := GSBAToExpanded(self);
end;

procedure TGSBAPixelBasicHelper.FromExpanded(AValue: TExpandedPixel);
begin
  self := ExpandedToGSBA(AValue);
end;

class operator TGSBAPixel.{$IFDEF OBJ}:={$ELSE}Implicit{$ENDIF}(const Source: TGSBAPixel): TBGRAPixel;
begin
  result := GSBAToBGRA(Source);
end;

class operator TGSBAPixel.{$IFDEF OBJ}:={$ELSE}Implicit{$ENDIF}(const Source: TBGRAPixel): TGSBAPixel;
begin
  result := BGRAToGSBA(Source);
end;

class operator TGSBAPixel.{$IFDEF OBJ}:={$ELSE}Implicit{$ENDIF}(const Source: TGSBAPixel): TExpandedPixel;
begin
  result := GSBAToExpanded(Source);
end;

class operator TGSBAPixel.{$IFDEF OBJ}:={$ELSE}Implicit{$ENDIF}(const Source: TExpandedPixel): TGSBAPixel;
begin
  result := ExpandedToGSBA(Source);
end;

class operator TGSBAPixel.{$IFDEF OBJ}:={$ELSE}Implicit{$ENDIF} (const AValue: TColor): TGSBAPixel;
begin Result := BGRAToGSBA(ColorToBGRA(ColorToRGB(AValue))) end;

class operator TGSBAPixel.{$IFDEF OBJ}:={$ELSE}Implicit{$ENDIF} (const AValue: TGSBAPixel): TColor;
begin Result := BGRAToColor(GSBAToBGRA(AValue)) end;

class operator TGSBAPixel.{$IFDEF OBJ}:={$ELSE}Implicit{$ENDIF}(const Source: TGSBAPixel): THSLAPixel;
begin
  result := THSLAPixel(Pointer(@Source)^);
end;

class operator TGSBAPixel.{$IFDEF OBJ}:={$ELSE}Implicit{$ENDIF}(const Source: THSLAPixel): TGSBAPixel;
begin
  result := TGSBAPixel(Pointer(@Source)^);
end;
{$ENDIF}
