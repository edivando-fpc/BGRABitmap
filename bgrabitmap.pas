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


Unit BGRABitmap;

{$i bgrabitmap.inc}{$H+}

interface

{ Compiler directives are used to include the best version according
  to the platform }

uses
  Classes, SysUtils, BGRATypes, {$IFNDEF FPC}Types, GraphType,{$ENDIF}
{$IFDEF BGRABITMAP_USE_FPGUI}
    BGRAfpGUIBitmap,
{$ELSE}
 {$IFDEF FPC}
	{$IFDEF BGRABITMAP_USE_LCL}
	  {$IFDEF LCLwin32}
		BGRAWinBitmap,
	  {$ELSE}
		{$IFDEF LCLgtk}
		BGRAGtkBitmap,
		{$ELSE}
		  {$IFDEF LCLgtk2}
		BGRAGtkBitmap,
		  {$ELSE}
			{$IFDEF LCLqt}
		BGRAQtBitmap,
			{$ELSE}
              {$IFDEF DARWIN}
        BGRAMacBitmap,
              {$ELSE}
		BGRALCLBitmap,
              {$ENDIF}
			{$ENDIF}
		  {$ENDIF}
		{$ENDIF}
	  {$ENDIF}
	{$ELSE}
	  BGRANoGuiBitmap,
	{$ENDIF}
	{$ELSE}
	  BGRAWinBitmap,
	{$ENDIF}
{$ENDIF}
  BGRAGraphics;

type
{$IFDEF BGRABITMAP_USE_FPGUI}
  TBGRABitmap = class(TBGRAfpGUIBitmap);
{$ELSE}
    {$IFDEF BGRABITMAP_USE_LCL}
      {$IFDEF FPC}
      {$IFDEF LCLwin32}
        TBGRABitmap = class(TBGRAWinBitmap);
      {$ELSE}
        {$IFDEF LCLgtk}
        TBGRABitmap = class(TBGRAGtkBitmap);
        {$ELSE}
          {$IFDEF LCLgtk2}
        TBGRABitmap = class(TBGRAGtkBitmap);
          {$ELSE}
            {$IFDEF LCLqt}
        TBGRABitmap = class(TBGRAQtBitmap);
            {$ELSE}
              {$IFDEF DARWIN}
        TBGRABitmap = class(TBGRAMacBitmap);
              {$ELSE}
        TBGRABitmap = class(TBGRALCLBitmap);
              {$ENDIF}
            {$ENDIF}
          {$ENDIF}
        {$ENDIF}
      {$ENDIF}
      {$ELSE}
        TBGRABitmap = class(TBGRAWinBitmap);
      {$ENDIF}
    {$ELSE}
      TBGRABitmap = class(TBGRANoGUIBitmap);
    {$ENDIF}
{$ENDIF}

// draw a bitmap from pure data
procedure BGRABitmapDraw(ACanvas: TCanvas; Rect: TRect; AData: Pointer;
  VerticalFlip: boolean; AWidth, AHeight: integer; Opaque: boolean);
  
{ Replace the content of the variable Destination with the variable
  Temp and frees previous object contained in Destination.
  
  This function is useful as a shortcut for :
 
  var
    temp: TBGRABitmap;
  begin
    ...
    temp := someBmp.Filter... as TBGRABitmap;
    someBmp.Free;
    someBmp := temp;
  end;
  
  which becomes :
  
  begin
    ...
    BGRAReplace(someBmp, someBmp.Filter... );
  end;
}
procedure BGRAReplace(var Destination: TBGRABitmap; Temp: TObject);

implementation

uses BGRABitmapTypes, BGRAReadBMP, BGRAReadBmpMioMap, BGRAReadGif,
  {$IFDEF FPC}BGRAReadIco, BGRAReadLzp, BGRAWriteLzp, BGRAReadPCX, BGRAReadPSD, BGRAReadTGA,{$ENDIF}
  BGRAReadJpeg, BGRAReadPng, BGRAReadXPM;

var
  tempBmp: TBGRABitmap;

procedure BGRABitmapDraw(ACanvas: TCanvas; Rect: TRect; AData: Pointer;
  VerticalFlip: boolean; AWidth, AHeight: integer; Opaque: boolean);
var
  LineOrder: TRawImageLineOrder;
begin
  if tempBmp = nil then
    tempBmp := TBGRABitmap.Create;
  if VerticalFlip then
    LineOrder := riloBottomToTop
  else
    LineOrder := riloTopToBottom;
  if Opaque then
    tempBmp.DataDrawOpaque(ACanvas, Rect, AData, LineOrder, AWidth, AHeight)
  else
    tempBmp.DataDrawTransparent(ACanvas, Rect, AData, LineOrder, AWidth, AHeight);
end;

procedure BGRAReplace(var Destination: TBGRABitmap; Temp: TObject);
begin
  Destination.Free;
  Destination := Temp as TBGRABitmap;
end;

initialization

  //this variable is assigned to access appropriate functions
  //depending on the platform
  BGRABitmapFactory := TBGRABitmap;

finalization

  tempBmp.Free;

end.

