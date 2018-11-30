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


Unit BGRAPhongTypes;

{$i bgrabitmap.inc}{$H+}

interface

uses
  Classes, SysUtils, BGRATypes, {$IFNDEF FPC}Types, GraphType,{$ENDIF} BGRABitmapTypes;

type TCustomPhongShading = class

   LightPosition : TPoint;

   { Render the specified map on the destination bitmap with one solid color. Map altitude
     indicate the global height of the map. }
   procedure Draw(dest: TBGRACustomBitmap; map: TBGRACustomBitmap; mapAltitude: integer; ofsX,ofsY: integer;
                  Color : TBGRAPixel);  overload; virtual; abstract;

   { Render with a color map of the same size as the height map. Map altitude
     indicate the global height of the map. }
   procedure Draw(dest: TBGRACustomBitmap; map: TBGRACustomBitmap; mapAltitude: integer; ofsX,ofsY: integer;
                  ColorMap : TBGRACustomBitmap);  overload; virtual; abstract;

   { Render with a scanner. Map altitude
     indicate the global height of the map. }
   procedure DrawScan(dest: TBGRACustomBitmap; map: TBGRACustomBitmap; mapAltitude: integer; ofsX,ofsY: integer;
                  ColorScan : IBGRAScanner); virtual; abstract;

 end;

implementation

end.

