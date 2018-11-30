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

unit bgratypes;

interface

uses
 Classes, SysUtils, {$IFNDEF FPC}Windows,{$ELSE}LCLType,{$ENDIF} Types;
type
  {$IFDEF FPC}
    {$IFDEF CPU64}
    BGRAPtrInt      = PtrInt;           //    Cardinal;//PtrInt;
    BGRAPtrUInt     = PtrUInt;          //    Cardinal;//PtrUInt;
    {$ELSE}
    BGRAPtrInt      = PtrInt;           //    LongInt;//PtrInt;
    BGRAPtrUInt     = PtrUInt;          //    Cardinal;//PtrUInt;
    BGRAQWord       = Int64;            //    Cardinal;//QWord;
    {$ENDIF}
    BGRAWord        = Word;                        //    Word;
    PBGRAWord       = PWord;                       //    PWord;
    BGRADWord       = DWord;          //    Cardinal;  //DWord;
    BGRALongWord    = LongWord;       //    Cardinal;  //LongWord;
    PBGRAQWord      = PQWord;         //    PCardinal; //PQWord;
    PBGRADWord      = PDWord;         //    PCardinal; //PDWord;
    PBGRALongWord   = PLongWord;      //    PCardinal; //PLongWord;
    BGRANativeInt   = NativeInt;      //    NativeInt;  //NativeInt;
    BGRANativeUInt  = NativeUInt;     //    Cardinal;  //NativeUInt;
    BGRALongInt     = LongInt;        //    Cardinal;  //LongInt;
    BGRAInt64       = Int64;                       //    Int64;
    BGRAUInt64      = Int64;                      //    UInt64;
    BGRACardinal    = Cardinal;                    //    Cardinal;
    PBGRACardinal   = PCardinal;                   //    PCardinal;

    PBGRAPtrInt      = PPtrInt;           //    LongInt;//PtrInt;
    PBGRAPtrUInt     = PPtrUInt;          //    Cardinal;//PtrUInt;

    HDC             = {$IFDEF BGRABITMAP_USE_LCL}LCLType.HDC{$ELSE}BGRAPtrUInt{$ENDIF};
    PPTrint         = ^PtrInt;
  {$ELSE}
    ValReal         = Extended;
    {$IFDEF CPU64}
    BGRAPtrInt      = Int64;
    BGRAPtrUInt     = QWord;
    {$ELSE}
    BGRAPtrInt      = LongInt;        //      LongInt;//LongInt;
    BGRAPtrUInt     = LongWord;       //      Cardinal;//LongWord;
    BGRAQWord       = Int64;          //      Cardinal;//LongWord;
    {$ENDIF}
    BGRAWord        = Word;           //                 Word;
    PBGRAWord       = PWord;          //                 PWord;
    BGRADWord       = DWord;          //      Cardinal;//DWord;
    BGRALongWord    = LongWord;       //      Cardinal;//LongWord;
    PBGRAPtrInt     = ^BGRAPtrInt;    //     PCardinal;//^BGRAPtrInt;
    PBGRAPtrUInt    = ^BGRAPtrUInt;   //     PCardinal;//^BGRAPtrUInt;
    PBGRAQWord      = ^BGRAQWord;     //     PCardinal;//^BGRAQWord;
    PBGRADWord      = PDWord;         //     PCardinal;//PDWord;
    PBGRALongWord   = PLongWord;      //     PCardinal;//PLongWord;
    BGRANativeInt   = NativeInt;      //     NativeInt;//NativeInt;
    BGRANativeUInt  = NativeUInt;     //      Cardinal;//NativeUInt;
    BGRALongInt     = LongInt;        //                 LongInt;
    BGRAInt64       = Int64;          //                 Int64;
    BGRAUInt64      = Int64;          //                 UInt64;
    BGRACardinal    = Cardinal;       //                 Cardinal;
    PBGRACardinal   = PCardinal;      //                 PCardinal;

    HDC             = Windows.HDC;    //
    PUnicodeChar    = Windows.PWChar; //
    UnicodeChar     = Windows.WCHAR;  //
(*    ValReal         = FPImage.ValReal;
    {$IFDEF CPU64}     //WORD = 2 bytes = 4 nybbles = 16 bits    for 32bits
    BGRAPtrInt      = FPImage.BGRAPtrInt;
    BGRAPtrUInt     = FPImage.BGRAPtrUInt;  //QWORD = 2 DWORDs = 4 WORDs = ….. = 64 bits       for 32bits
    {$ELSE}               //BGRADWord = 2 WORDs = 4 bytes = 8 nybbles = 32 bits   for 32bits
    BGRAPtrInt      = FPImage.BGRAPtrInt;
    BGRAPtrUInt     = FPImage.BGRAPtrUInt;
    BGRAQWord       = FPImage.BGRAQWord;
    {$ENDIF}
    BGRADWord       = FPImage.BGRADWord;
    BGRALongWord    = FPImage.BGRALongWord;
    PBGRAPtrInt     = FPImage.PBGRAPtrInt;
    PBGRAPtrUInt    = FPImage.PBGRAPtrUInt;
    PBGRAQWord      = FPImage.PBGRAQWord;
    PBGRADWord      = FPImage.PBGRADWord;
    HDC             = FPImage.HDC;
    BGRANativeInt   = FPImage.BGRANativeInt;
    PBGRALongWord   = FPImage.PBGRALongWord;

    PUnicodeChar    = FPImage.PUnicodeChar;
    UnicodeChar     = FPImage.UnicodeChar;    *)
  {$ENDIF}

implementation

end.

