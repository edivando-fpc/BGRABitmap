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


Unit BGRAReadXPM;

{$i bgrabitmap.inc}{$H+}

interface

uses
  Classes, SysUtils, BGRATypes,{$IFNDEF FPC}Types, GraphType, BGRAGraphics,{$ENDIF} FPReadXPM, FPImage;

type

  { TBGRAReaderXPM }

  TBGRAReaderXPM = class(TFPReaderXPM)
    protected
      procedure InternalRead(Str: TStream; Img: TFPCustomImage); override;
      function InternalCheck(Str: TStream): boolean; override;
    public
      class procedure ConvertToXPM3(ASource: TStream; ADestination: TStream);
  end;

implementation

uses BGRABitmapTypes;

{ TBGRAReaderXPM }

procedure TBGRAReaderXPM.InternalRead(Str: TStream; Img: TFPCustomImage);
var tempStream: TMemoryStream;
begin
  tempStream := TMemoryStream.Create;
  try
    ConvertToXPM3(Str, tempStream);
    tempStream.Position:= 0;
    try
      img.UsePalette := true;
      inherited InternalRead(tempStream, Img);
    finally
    end;
  finally
    tempStream.free;
  end;
end;

function TBGRAReaderXPM.InternalCheck(Str: TStream): boolean;
var {%H-}magic : array[0..5] of {$IFDEF FPC}Char{$ELSE}AnsiChar{$ENDIF};
    l : integer;
    prevPos: BGRAInt64;
begin
  try
    prevPos := str.Position;
    l := str.Read ({%H-}magic[0],sizeof(magic));
    str.Position:= prevPos;
    result := (l = sizeof(magic)) and (magic = '! XPM2');
    if not result then result := inherited InternalCheck(Str)
  except
    result := false;
  end;
end;

class procedure TBGRAReaderXPM.ConvertToXPM3(ASource: TStream;
  ADestination: TStream);
var
  lst: TStringList;
  i : integer;
begin
  lst := TStringList.Create;
  try
    lst.LoadFromStream(ASource);
    if (lst[0] = '! XPM2') and (lst.count > 1) then
    begin
      lst[0] := '/* XPM */';
      lst.Insert(1, 'static char * data[] = {');
      for i := 2 to lst.Count-2 do
        lst[i] := '"' + lst[i] + '",';
      lst[lst.count-1] := '"' + lst[lst.count-1] + '"';
      lst.Add('}');
    end;
    lst.SaveToStream(ADestination);
  finally
    lst.free;
  end;
end;

initialization

  DefaultBGRAImageReader[ifXPixMap] := TBGRAReaderXPM;

end.

