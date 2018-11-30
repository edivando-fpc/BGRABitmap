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


Unit BGRAReadJpeg;

{$i bgrabitmap.inc}{$H+}

interface

uses
  Classes, SysUtils, BGRATypes,{$IFNDEF FPC}Types, GraphType, BGRAGraphics,{$ENDIF} FPReadJPEG;

type
  TJPEGScale = FPReadJPEG.TJPEGScale;
  TJPEGReadPerformance = FPReadJPEG.TJPEGReadPerformance;

const
  jsFullSize = FPReadJPEG.jsFullSize;
  jsHalf = FPReadJPEG.jsHalf;
  jsQuarter = FPReadJPEG.jsQuarter;
  jsEighth = FPReadJPEG.jsEighth;

  jpBestQuality = FPReadJPEG.jpBestQuality;
  jpBestSpeed = FPReadJPEG.jpBestSpeed;

type
  { TBGRAReaderJpeg }

  TBGRAReaderJpeg = class(TFPReaderJPEG)
    constructor Create; override;
  protected
    function InternalCheck(Str: TStream): boolean; override;
  end;

implementation

uses BGRABitmapTypes;

{ TBGRAReaderJpeg }

constructor TBGRAReaderJpeg.Create;
begin
  inherited Create;
  Performance := jpBestQuality;
end;

function TBGRAReaderJpeg.InternalCheck(Str: TStream): boolean;
var {%H-}magic: packed array[0..3] of byte;
  OldPos,BytesRead:BGRAInt64;
begin
  Result:=false;
  if Str=nil then exit;
  OldPos:= str.Position;
  BytesRead := str.Read({%H-}magic,sizeof(magic));
  str.Position:=OldPos;
  if BytesRead<>sizeof(magic) then exit;
  if (magic[0] = $ff) and (magic[1] = $d8) and (magic[2] = $ff) and (magic[3] >= $c0) then result := true;
end;

initialization

  DefaultBGRAImageReader[ifJpeg] := TBGRAReaderJpeg;

end.

