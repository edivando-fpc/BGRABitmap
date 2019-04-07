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


Unit BGRAUTF8;

{$i bgrabitmap.inc}{$H+}

interface

uses
  Classes, SysUtils, BGRATypes,{$IFNDEF FPC}Types, GraphType, BGRAGraphics,{$ENDIF}
  BGRAUnicode{$IFDEF FPC}{$IFDEF BGRABITMAP_USE_LCL}, lazutf8classes{$ENDIF}{$ENDIF};

{$IFDEF FPC}
{$IFDEF BGRABITMAP_USE_LCL}
type
  TFileStreamUTF8 = lazutf8classes.TFileStreamUTF8;
  TStringListUTF8 = lazutf8classes.TStringListUTF8;
{$ELSE}
type
  TFileStreamUTF8 = class(THandleStream)
  private
    FFileName: utf8string;
  public
    constructor Create(const AFileName: utf8string; Mode: BGRAWord); overload;
    constructor Create(const AFileName: utf8string; Mode: BGRAWord; Rights: BGRACardinal);overload;
    destructor Destroy; override;
    property FileName: utf8string Read FFilename;
  end;

  TStringListUTF8 = class(TStringList)
  protected
    function DoCompareText(const s1,s2 : string) : BGRAPtrInt; override;
  public
    procedure LoadFromFile(const FileName: string); override;
    procedure SaveToFile(const FileName: string); override;
  end;
{$ENDIF}
{$ELSE}
const
  feInvalidHandle : THandle = THandle(-1);  //return value on FileOpen error

type
  BGRAPtrInt  = {$ifdef CPU64}BGRAInt64{$else}BGRALongInt{$ENDIF};
  SizeInt = {$ifdef CPU64}BGRAInt64{$else}BGRALongInt{$ENDIF};

  TFileStreamUTF8 = class({$ifdef bds}TFileStream{$else}THandleStream{$ENDIF})
  private
    FFileName: utf8string;
  public
    constructor Create(const AFileName: utf8string; Mode: BGRAWord); overload;
    constructor Create(const AFileName: utf8string; Mode: BGRAWord; Rights: BGRACardinal); overload;
    destructor Destroy; override;
    property FileName: utf8string Read FFilename;
  end;

  TStringListUTF8 = class(TStringList)
  protected
    function DoCompareText(const s1,s2 : string) : BGRAPtrInt;
  public
    procedure LoadFromFile(const FileName: string); override;
    procedure SaveToFile(const FileName: string); override;
  end;
{$ENDIF}

procedure LoadStringsFromFileUTF8(List: TStrings; const FileName: string);
procedure SaveStringsToFileUTF8(List: TStrings; const FileName: string);

function UTF8ToSys(const s: string): string;
function SysToUTF8(const s: string): string;

function UTF8LowerCase(const s: string): string;
function UTF8UpperCase(const s: string): string;

function UTF8CompareStr(const S1, S2: string): Integer;
function UTF8CompareText(const S1, S2: string): Integer;

function UTF8CharStart(UTF8Str: PChar(*{$IFDEF FPC}PChar{$ELSE}PAnsiChar{$ENDIF}*); Len, CharIndex: BGRAPtrInt): PChar; //@{$IFDEF FPC}PChar{$ELSE}PAnsiChar{$ENDIF};

function FileOpenUTF8(Const FileName : string; Mode : Integer) : THandle;
function FileCreateUTF8(Const FileName : string) : THandle; overload;
function FileCreateUTF8(Const FileName : string; Rights: BGRACardinal) : THandle; overload;
function FileExistsUTF8(Const FileName : string): boolean;
function FindFirstUTF8(const Path: string; Attr: BGRALongInt; out Rslt: TSearchRec): BGRALongInt;
function FindNextUTF8(var Rslt: TSearchRec): BGRALongInt;
procedure FindCloseUTF8(var F: TSearchrec);

{$IFDEF FPC}
type
  string4 = string[4];
{$ELSE}
type
  string4 = string;
{$ENDIF}

function UTF8CharacterLength(p: PChar): integer; //@ {$IFDEF FPC}PChar{$ELSE}PAnsiChar{$ENDIF}): integer;
function UTF8Length(const s: string): BGRAPtrInt; overload;
function UTF8Length(p: PChar (*{$IFDEF FPC}PChar{$ELSE}PAnsiChar{$ENDIF}*); ByteCount: BGRAPtrInt): BGRAPtrInt; overload;
function UnicodeCharToUTF8(u: BGRACardinal): string4;
function UTF8ReverseString(const s: string): string;
function UTF8CodepointToUnicode(p: PChar (*{$IFDEF FPC}PChar{$ELSE}PAnsiChar{$ENDIF}*); ACodePointLen: integer): BGRACardinal;

type
  TBidiUTF8Info = packed record
    Offset: Integer;
    BidiInfo: TUnicodeBidiInfo;
  end;
  TBidiUTF8Array = packed array of TBidiUTF8Info;
  TUnicodeDisplayOrder = BGRAUnicode.TUnicodeDisplayOrder;

function GetBidiClassUTF8(P: PChar(*{$IFDEF FPC}PChar{$ELSE}PAnsiChar{$ENDIF}*)): TUnicodeBidiClass;
function GetFirstStrongBidiClassUTF8(const sUTF8: string): TUnicodeBidiClass;
function GetLastStrongBidiClassUTF8(const sUTF8: string): TUnicodeBidiClass;
function IsRightToLeftUTF8(const sUTF8: string): boolean;
function IsZeroWidthUTF8(const sUTF8: string): boolean;
function AddParagraphBidiUTF8(s: string; ARightToLeft: boolean): string;
function AnalyzeBidiUTF8(const sUTF8: string; ARightToLeft: boolean): TBidiUTF8Array; overload;
function AnalyzeBidiUTF8(const sUTF8: string): TBidiUTF8Array; overload;
function GetUTF8DisplayOrder(const ABidi: TBidiUTF8Array): TUnicodeDisplayOrder;
function ContainsBidiIsolateOrFormattingUTF8(const sUTF8: string): boolean;

function UTF8OverrideDirection(const sUTF8: string; ARightToLeft: boolean): string;
function UTF8EmbedDirection(const sUTF8: string; ARightToLeft: boolean): string;

//little endian stream functions
function LEReadInt64(Stream: TStream): BGRAInt64;
procedure LEWriteInt64(Stream: TStream; AValue: BGRAInt64);
function LEReadLongint(Stream: TStream): BGRALongInt;
procedure LEWriteLongint(Stream: TStream; AValue: BGRALongInt);
function LEReadByte(Stream: TStream): byte;
procedure LEWriteByte(Stream: TStream; AValue: Byte);
function LEReadSingle(Stream: TStream): single;
procedure LEWriteSingle(Stream: TStream; AValue: single);
procedure LEWriteVar(Stream: TStream; var AValue);
procedure LEReadVar(Stream: TStream; out AValue);

implementation

uses
 {$IFDEF BDS}bgraendian;{$ENDIF}

{$IF DEFINED(FPC) AND DEFINED(BGRABITMAP_USE_LCL)}
 LazFileUtils, LazUtf8;

procedure LoadStringsFromFileUTF8(List: TStrings; const FileName: string);
begin
  lazutf8classes.LoadStringsFromFileUTF8(List,FileName);
end;

procedure SaveStringsToFileUTF8(List: TStrings; const FileName: string);
begin
  lazutf8classes.SaveStringsToFileUTF8(List,FileName);
end;

function UTF8ToSys(const s: string): string;
begin
  result := LazUtf8.UTF8ToSys(s);
end;

function SysToUTF8(const s: string): string;
begin
  result := LazUtf8.SysToUTF8(s);
end;

function UTF8LowerCase(const s: string): string;
begin
  result := LazUtf8.UTF8LowerCase(s);
end;

function UTF8UpperCase(const s: string): string;
begin
  result := LazUtf8.UTF8UpperCase(s);
end;

function UTF8CompareStr(const S1, S2: string): Integer;
begin
  result := LazUtf8.UTF8CompareStr(S1,S2);
end;

function UTF8CompareText(const S1, S2: string): Integer;
begin
  result := LazUtf8.UTF8CompareText(S1,S2);
end;

function FileOpenUTF8(Const FileName : string; Mode : Integer) : THandle;
begin
  result := LazFileUtils.FileOpenUTF8(FileName, Mode);
end;

function FileCreateUTF8(Const FileName : string) : THandle; overload;
begin
  result := LazFileUtils.FileCreateUTF8(FileName);
end;

function FileCreateUTF8(Const FileName : string; Rights: BGRACardinal) : THandle; overload;
begin
  result := LazFileUtils.FileCreateUTF8(FileName, Rights);
end;

function FileExistsUTF8(Const FileName : string): boolean;
begin
  result := LazFileUtils.FileExistsUTF8(FileName);
end;

function FindFirstUTF8(const Path: string; Attr: BGRALongInt; out Rslt: TSearchRec
  ): BGRALongInt;
begin
  result := LazFileUtils.FindFirstUTF8(Path,Attr,Rslt);
end;

function FindNextUTF8(var Rslt: TSearchRec): BGRALongInt;
begin
  result := LazFileUtils.FindNextUTF8(Rslt);
end;

procedure FindCloseUTF8(var F: TSearchrec);
begin
  LazFileUtils.FindCloseUTF8(F);
end;

function UTF8CharacterLength(p: PChar): integer; //@{$IFDEF FPC}PChar{$ELSE}PAnsiChar{$ENDIF}): integer;
begin
  result := LazUtf8.UTF8CharacterLength(p);
end;

function UTF8Length(const s: string): BGRAPtrInt;
begin
  result := LazUtf8.UTF8Length(s);
end;

function UTF8Length(p: PChar(*{$IFDEF FPC}PChar{$ELSE}PAnsiChar{$ENDIF}*); ByteCount: BGRAPtrInt): BGRAPtrInt;
begin
  result := LazUtf8.UTF8Length(p, ByteCount);
end;

function UnicodeCharToUTF8(u: BGRACardinal): string4;
begin
  result := LazUtf8.UnicodeToUTF8(u);
end;

{$IFEND}
{$IF NOT DEFINED(FPC) AND DEFINED(BGRABITMAP_USE_LCL)}
{.$ELSE}

procedure LoadStringsFromFileUTF8(List: TStrings; const FileName: string);
var
  uList: TStringListUTF8;
begin
  if List is TStringListUTF8 then
  begin
    List.LoadFromFile(FileName);
    exit;
  end;
  uList:=TStringListUTF8.Create;
  try
    uList.LoadFromFile(FileName);
    List.Assign(uList);
  finally
    uList.Free;
  end;
end;

procedure SaveStringsToFileUTF8(List: TStrings; const FileName: string);
var
  uList: TStringListUTF8;
begin
  if List is TStringListUTF8 then
  begin
    List.SaveToFile(FileName);
    exit;
  end;
  uList:=TStringListUTF8.Create;
  try
    uList.Assign(List);
    uList.SaveToFile(FileName);
  finally
    uList.Free;
  end;
end;

function UTF8LowerCase(const s: string): string;
begin
  {$IFDEF FPC}
  result := UTF8Encode(UnicodeLowerCase(UTF8Decode(s)));
  {$ELSE}
  result := UTF8Encode(LowerCase(UTF8Decode(s)));
  {$ENDIF}
end;

function UTF8UpperCase(const s: string): string;
begin
  {$IFDEF FPC}
  result := UTF8Encode(UnicodeUpperCase(UTF8Decode(s)));
  {$ELSE}
  result := UTF8Encode(UpperCase(UTF8Decode(s)));
  {$ENDIF}
end;

function UTF8CompareStr(const S1, S2: string): Integer;
begin
  Result := SysUtils.CompareStr(S1, S2);
end;

function UTF8CompareText(const S1, S2: string): Integer;
begin
  {$IFDEF FPC}
  Result := UnicodeCompareText(UTF8Decode(S1), UTF8Decode(S2));
  {$ELSE}
  Result := SysUtils.CompareStr(S1, S2);
  {$ENDIF}
end;

function FileOpenUTF8(const FileName: string; Mode: Integer): THandle;
begin
  result := FileOpen(UTF8ToSys(FileName),Mode);
end;

function FileCreateUTF8(const FileName: string): THandle;
begin
  result := FileCreate(UTF8ToSys(FileName));
end;

function FileCreateUTF8(const FileName: string; Rights: BGRACardinal): THandle;
begin
  result := FileCreate(UTF8ToSys(FileName),Rights);
end;

function FileExistsUTF8(const FileName: string): boolean;
begin
  result := FileExists(UTF8ToSys(FileName));
end;

function FindFirstUTF8(const Path: string; Attr: BGRALongInt; out Rslt: TSearchRec
  ): BGRALongInt;
begin
  result := FindFirst(UTF8ToSys(Path),Attr,Rslt);
  Rslt.Name := SysToUTF8(Rslt.Name);
end;

function FindNextUTF8(var Rslt: TSearchRec): BGRALongInt;
begin
  result := FindNext(Rslt);
  if result = 0 then
    Rslt.Name := SysToUTF8(Rslt.Name);
end;

procedure FindCloseUTF8(var F: TSearchrec);
begin
  FindClose(F);
end;

function UTF8ToSys(const s: string): string;
begin
  result := Utf8ToAnsi(s);
end;

function SysToUTF8(const s: string): string;
begin
  result := AnsiToUtf8(s);
end;

function UTF8CharacterLength(p: PChar): integer; //@{$IFDEF FPC}PChar{$ELSE}PAnsiChar{$ENDIF}): integer;
begin
  if p<>nil then begin
    if ord(p^)<{$IFDEF FPC}%11000000{$ELSE}$11000000{$ENDIF} then begin
      // regular single byte character (#0 is a character, this is pascal ;)
      Result:=1;
    end
    else begin
      // multi byte
      if ((ord(p^) and {$IFDEF FPC}%11100000{$ELSE}$11100000{$ENDIF}) = {$IFDEF FPC}%11000000{$ELSE}$11000000{$ENDIF}) then begin
        // could be 2 byte character
        if (ord(p[1]) and {$IFDEF FPC}%11000000{$ELSE}$11000000{$ENDIF}) = {$IFDEF FPC}%10000000{$ELSE}$10000000{$ENDIF} then
          Result:=2
        else
          Result:=1;
      end
      else if ((ord(p^) and {$IFDEF FPC}%11110000{$ELSE}$11110000{$ENDIF}) = {$IFDEF FPC}%11100000{$ELSE}$11100000{$ENDIF}) then begin
        // could be 3 byte character
        if ((ord(p[1]) and {$IFDEF FPC}%11000000{$ELSE}$11000000{$ENDIF}) = {$IFDEF FPC}%10000000{$ELSE}$10000000{$ENDIF})
        and ((ord(p[2]) and {$IFDEF FPC}%11000000{$ELSE}$11000000{$ENDIF}) = {$IFDEF FPC}%10000000{$ELSE}$10000000{$ENDIF}) then
          Result:=3
        else
          Result:=1;
      end
      else if ((ord(p^) and {$IFDEF FPC}%11111000{$ELSE}$11111000{$ENDIF}) = {$IFDEF FPC}%11110000{$ELSE}$11110000{$ENDIF}) then begin
        // could be 4 byte character
        if ((ord(p[1])  and {$IFDEF FPC}%11000000{$ELSE}$11000000{$ENDIF}) = {$IFDEF FPC}%10000000{$ELSE}$10000000{$ENDIF})
        and ((ord(p[2]) and {$IFDEF FPC}%11000000{$ELSE}$11000000{$ENDIF}) = {$IFDEF FPC}%10000000{$ELSE}$10000000{$ENDIF})
        and ((ord(p[3]) and {$IFDEF FPC}%11000000{$ELSE}$11000000{$ENDIF}) = {$IFDEF FPC}%10000000{$ELSE}$10000000{$ENDIF}) then
          Result:=4
        else
          Result:=1;
      end
      else
        Result:=1;
    end;
  end else
    Result:=0;
end;

function UTF8Length(const s: string): BGRAPtrInt;
begin
  Result:=UTF8Length((*{$IFDEF FPC}PChar{$ELSE}PAnsiChar{$ENDIF}*)PChar(s),length(s));
end;

function UTF8Length(p: PChar(*{$IFDEF FPC}PChar{$ELSE}PAnsiChar{$ENDIF}*); ByteCount: BGRAPtrInt): BGRAPtrInt;
var
  CharLen: BGRALongInt;
begin
  Result:=0;
  while (ByteCount>0) do begin
    inc(Result);
    CharLen:=UTF8CharacterLength(p);
    inc(p,CharLen);
    dec(ByteCount,CharLen);
  end;
end;

function UnicodeToUTF8Inline(CodePoint: BGRACardinal; Buf: PChar(*{$IFDEF FPC}PChar{$ELSE}PAnsiChar{$ENDIF}*)): integer;
begin
  case CodePoint of
    0..$7f:
      begin
        Result:=1;
        Buf[0]:= (*{$IFDEF FPC}Char{$ELSE}AnsiChar{$ENDIF}*)Char(byte(CodePoint));
      end;
    $80..$7ff:
      begin
        Result:=2;
        Buf[0]:=(*{$IFDEF FPC}Char{$ELSE}AnsiChar{$ENDIF}*)Char(byte($c0 or (CodePoint shr 6)));
        Buf[1]:=(*{$IFDEF FPC}Char{$ELSE}AnsiChar{$ENDIF}*)Char(byte($80 or (CodePoint and $3f)));
      end;
    $800..$ffff:
      begin
        Result:=3;
        Buf[0]:=(*{$IFDEF FPC}Char{$ELSE}AnsiChar{$ENDIF}*)Char(byte($e0 or (CodePoint shr 12)));
        Buf[1]:=(*{$IFDEF FPC}Char{$ELSE}AnsiChar{$ENDIF}*)Char(byte((CodePoint shr 6) and $3f) or $80);
        Buf[2]:=(*{$IFDEF FPC}Char{$ELSE}AnsiChar{$ENDIF}*)Char(byte(CodePoint and $3f) or $80);
      end;
    $10000..$10ffff:
      begin
        Result:=4;
        Buf[0]:=(*{$IFDEF FPC}Char{$ELSE}AnsiChar{$ENDIF}*)Char(byte($f0 or (CodePoint shr 18)));
        Buf[1]:=(*{$IFDEF FPC}Char{$ELSE}AnsiChar{$ENDIF}*)Char(byte((CodePoint shr 12) and $3f) or $80);
        Buf[2]:=(*{$IFDEF FPC}Char{$ELSE}AnsiChar{$ENDIF}*)Char(byte((CodePoint shr 6) and $3f) or $80);
        Buf[3]:=(*{$IFDEF FPC}Char{$ELSE}AnsiChar{$ENDIF}*)Char(byte(CodePoint and $3f) or $80);
      end;
  else
    Result:=0;
  end;
end;

function UnicodeCharToUTF8(u: BGRACardinal): string4;
begin
  {$IFDEF FPC}
  result[0] := chr(UnicodeToUTF8Inline(u,@result[1]));
  {$ELSE}
  result := chr(UnicodeToUTF8Inline(u,@result[1]));
  {$ENDIF}
end;

constructor TFileStreamUTF8.Create(const AFileName: utf8string; Mode: BGRAWord);
var
  lHandle: THandle;
begin
  FFileName:= AFileName;
  if Mode = fmcreate then
    lHandle:= FileCreateUTF8(AFileName)
  else
    lHandle:= FileOpenUTF8(AFileName, Mode);

  If (THandle(lHandle)= feInvalidHandle) then
  begin
    if Mode = fmCreate then
      raise EFCreateError.createfmt({SFCreateError}'Unable to create file "%s"', [AFileName])
    else
      raise EFOpenError.Createfmt({SFOpenError}'Unable to open file "%s"', [AFilename]);
  end
  else
    inherited Create(lHandle);
end;

constructor TFileStreamUTF8.Create(const AFileName: utf8string; Mode: BGRAWord; Rights: BGRACardinal);
var
  lHandle: THandle;
begin
  FFileName:=AFileName;
  if Mode=fmcreate then
    lHandle:=FileCreateUTF8(AFileName,Rights)
  else
    lHandle:=FileOpenUTF8(AFileName,Mode);

  if (THandle(lHandle)=feInvalidHandle) then
  begin
    if Mode=fmcreate then
      raise EFCreateError.createfmt({SFCreateError}'Unable to create file "%s"',[AFileName])
    else
      raise EFOpenError.Createfmt({SFOpenError}'Unable to open file "%s"',[AFilename]);
  end
  else
    inherited Create(lHandle);
end;

destructor TFileStreamUTF8.Destroy;
begin
  FileClose(Handle);
end;

function TStringListUTF8.DoCompareText(const s1, s2: string): BGRAPtrInt;
begin
  if CaseSensitive then
    Result:= UTF8CompareStr(s1,s2)
  else
    Result:= UTF8CompareText(s1,s2);
end;

procedure TStringListUTF8.LoadFromFile(const FileName: string);
var
  TheStream: TFileStreamUTF8;
begin
  TheStream:= TFileStreamUTF8.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(TheStream);
  finally
    TheStream.Free;
  end;
end;

procedure TStringListUTF8.SaveToFile(const FileName: string);
var
  TheStream: TFileStreamUTF8;
begin
  TheStream:=TFileStreamUTF8.Create(FileName,fmCreate);
  try
    SaveToStream(TheStream);
  finally
    TheStream.Free;
  end;
end;

{.$ENDIF}
{$IFEND}

function UTF8ReverseString(const s: string): string;
var
  pSrc,pDest,pEnd: PChar; //@{$IFDEF FPC}PChar{$ELSE}PAnsiChar{$ENDIF};
  charLen: Integer;
begin
  if s = '' then
  begin
    result := '';
    exit;
  end;
  setlength(result, length(s));
  pDest := (*{$IFDEF FPC}PChar{$ELSE}PAnsiChar{$ENDIF}*)PChar(@result[1]) + length(result);
  pSrc := @s[1];
  pEnd := pSrc+length(s);
  while pSrc < pEnd do
  begin
    charLen := UTF8CharacterLength(pSrc);
    if (charLen = 0) or (pSrc+charLen > pEnd) then break;
    dec(pDest, charLen);
    move(pSrc^, pDest^, charLen);
    inc(pSrc, charLen);
  end;
end;

function UTF8CodepointToUnicode(p: PChar(*{$IFDEF FPC}PChar{$ELSE}PAnsiChar{$ENDIF}*); ACodePointLen: integer): BGRACardinal;
begin
  case ACodePointLen of
    0: result := 0;
    1: result := ord(p^);
    2: result := ((ord(p^) and $00011111) shl 6) or (ord(p[1]) and $00111111);
    3: result := ((ord(p^) and $00011111) shl 12) or ((ord(p[1]) and $00111111) shl 6)
                or (ord(p[2]) and $00111111);
    4: result := ((ord(p^) and $00001111) shl 18) or ((ord(p[1]) and $00111111) shl 12)
                or ((ord(p[2]) and $00111111) shl 6) or (ord(p[3]) and $00111111);
    else
      raise exception.Create('Invalid code point length');
  end;
end;

function UTF8CharStart(UTF8Str: PChar(*{$IFDEF FPC}PChar{$ELSE}PAnsiChar{$ENDIF}*); Len, CharIndex: BGRAPtrInt): PChar; //@{$IFDEF FPC}PChar{$ELSE}PAnsiChar{$ENDIF};
var
  CharLen: BGRALongInt;
begin
  Result:=UTF8Str;
  if Result<>nil then begin
    while (CharIndex>0) and (Len>0) do begin
      CharLen:=UTF8CharacterLength(Result);
      dec(Len,CharLen);
      dec(CharIndex);
      inc(Result,CharLen);
    end;
    if (CharIndex<>0) or (Len<0) then
      Result:=nil;
  end;
end;

function GetBidiClassUTF8(P: PChar(*{$IFDEF FPC}PChar{$ELSE}PAnsiChar{$ENDIF}*)): TUnicodeBidiClass;
begin
  result := GetUnicodeBidiClass(UTF8CodepointToUnicode(P, UTF8CharacterLength(p)));
end;

function GetFirstStrongBidiClassUTF8(const sUTF8: string): TUnicodeBidiClass;
var
  p,pEnd: PChar; //@{$IFDEF FPC}PChar{$ELSE}PAnsiChar{$ENDIF};
  charLen: Integer;
  u: BGRACardinal;
  curBidi: TUnicodeBidiClass;
  isolateNesting: integer;
begin
  if sUTF8 = '' then exit(ubcUnknown);
  p := @sUTF8[1];
  pEnd := p + length(sUTF8);
  isolateNesting:= 0;
  while p < pEnd do
  begin
    charLen := UTF8CharacterLength(p);
    if (charLen = 0) or (p+charLen > pEnd) then break;
    u := UTF8CodepointToUnicode(p, charLen);
    case u of
      UNICODE_POP_DIRECTIONAL_ISOLATE: if isolateNesting > 0 then dec(isolateNesting);
    end;
    curBidi := GetUnicodeBidiClass(u);
    if isolateNesting = 0 then
    begin
      if curBidi in[ubcLeftToRight,ubcRightToLeft,ubcArabicLetter] then
        exit(curBidi);
    end;
    case u of
      UNICODE_FIRST_STRONG_ISOLATE, UNICODE_LEFT_TO_RIGHT_ISOLATE, UNICODE_RIGHT_TO_LEFT_ISOLATE: inc(isolateNesting);
    end;
    if curBidi = ubcParagraphSeparator then isolateNesting:= 0;
    inc(p,charLen);
  end;
  exit(ubcUnknown);
end;

function GetLastStrongBidiClassUTF8(const sUTF8: string): TUnicodeBidiClass;
var
  p,pEnd: PChar; //{$IFDEF FPC}PChar{$ELSE}PAnsiChar{$ENDIF};
  charLen: Integer;
  u: BGRACardinal;
  curBidi: TUnicodeBidiClass;
  isolateNesting: integer;
begin
  if sUTF8 = '' then exit(ubcUnknown);
  p := @sUTF8[1];
  pEnd := p + length(sUTF8);
  isolateNesting:= 0;
  result := ubcUnknown;
  while p < pEnd do
  begin
    charLen := UTF8CharacterLength(p);
    if (charLen = 0) or (p+charLen > pEnd) then break;
    u := UTF8CodepointToUnicode(p, charLen);
    case u of
      UNICODE_POP_DIRECTIONAL_ISOLATE: if isolateNesting > 0 then dec(isolateNesting);
    end;
    curBidi := GetUnicodeBidiClass(u);
    if isolateNesting = 0 then
    begin
      if curBidi in[ubcLeftToRight,ubcRightToLeft,ubcArabicLetter] then
        result := curBidi;
    end;
    case u of
      UNICODE_FIRST_STRONG_ISOLATE, UNICODE_LEFT_TO_RIGHT_ISOLATE, UNICODE_RIGHT_TO_LEFT_ISOLATE: inc(isolateNesting);
    end;
    if curBidi = ubcParagraphSeparator then isolateNesting:= 0;
    inc(p,charLen);
  end;
end;

function IsRightToLeftUTF8(const sUTF8: string): boolean;
begin
  result := GetFirstStrongBidiClassUTF8(sUTF8) in[ubcRightToLeft,ubcArabicLetter];
end;

function IsZeroWidthUTF8(const sUTF8: string): boolean;
var
  p,pEnd: PChar; //{$IFDEF FPC}PChar{$ELSE}PAnsiChar{$ENDIF};
  charLen: Integer;
  u: BGRACardinal;
begin
  if sUTF8 = '' then exit(true);
  p := @sUTF8[1];
  pEnd := p + length(sUTF8);
  while p < pEnd do
  begin
    charLen := UTF8CharacterLength(p);
    if (charLen = 0) or (p+charLen > pEnd) then break;
    u := UTF8CodepointToUnicode(p, charLen);
    if not IsZeroWidthUnicode(u) then exit(false);
    inc(p,charLen);
  end;
  exit(true);
end;

function AddParagraphBidiUTF8(s: string; ARightToLeft: boolean): string;
var
  i,curParaStart: Integer;

  procedure CheckParagraph;
  var
    para,newPara: string;
    paraRTL: boolean;
  begin
    if i > curParaStart then
    begin
      para := copy(s,curParaStart,i-curParaStart);
      paraRTL := GetFirstStrongBidiClassUTF8(para) in[ubcRightToLeft,ubcArabicLetter];
      //detected paragraph does not match overall RTL option
      if paraRTL <> ARightToLeft then
      begin
        if not paraRTL then
          newPara := UnicodeCharToUTF8(UNICODE_LEFT_TO_RIGHT_MARK)+para+UnicodeCharToUTF8(UNICODE_LEFT_TO_RIGHT_MARK)
        else
          newPara := UnicodeCharToUTF8(UNICODE_RIGHT_TO_LEFT_MARK)+para+UnicodeCharToUTF8(UNICODE_RIGHT_TO_LEFT_MARK);
        inc(i, length(newPara)-length(para));
        delete(s, curParaStart, length(para));
        insert(newPara, s, curParaStart);
      end;
    end;
  end;

var
  charLen: integer;
  u: BGRACardinal;

begin
  i := 1;
  curParaStart := 1;
  while i <= length(s) do
  begin
    charLen := UTF8CharacterLength(@s[i]);
    u := UTF8CodepointToUnicode(@s[i], charLen);
    if IsUnicodeParagraphSeparator(u) then
    begin
      CheckParagraph;
      //skip end of line
      inc(i);
      //skip second CRLF
      if ((u = 10) or (u = 13)) and (i <= length(s)) and (s[i] in[#13,#10]) and (s[i]<>s[i-1]) then inc(i);
      curParaStart := i;
    end else
      inc(i);
  end;
  CheckParagraph;
  result := s;
end;

type
  TUnicodeArray = packed array of BGRACardinal;
  TIntegerArray = array of integer;

procedure UTF8ToUnicode(const sUTF8: string; out u: TUnicodeArray; out ofs: TIntegerArray);
var
  index,len,charLen: integer;
  p,pStart,pEnd: PChar; //{$IFDEF FPC}PChar{$ELSE}PAnsiChar{$ENDIF};
begin
  if sUTF8 = '' then
  begin
    u := nil;
    ofs := nil;
  end
  else
  begin
    pStart := @sUTF8[1];
    pEnd := pStart + length(sUTF8);
    p := pStart;
    len := 0;
    while p < pEnd do
    begin
      charLen := UTF8CharacterLength(p);
      inc(len);
      inc(p,charLen);
    end;

    setlength(u, len);
    setlength(ofs, len);
    p := pStart;
    index := 0;
    while p < pEnd do
    begin
      charLen := UTF8CharacterLength(p);
      u[index] := UTF8CodepointToUnicode(p, charLen);
      ofs[index] := p - pStart;
      inc(index);
      inc(p,charLen);
    end;
  end;
end;

function AnalyzeBidiUTF8(const sUTF8: string; ABaseDirection: BGRACardinal): TBidiUTF8Array; overload;
var
  u: TUnicodeArray;
  ofs: TIntegerArray;
  a: TUnicodeBidiArray;
  i: Integer;
begin
  if sUTF8 = '' then
    result := nil
  else
  begin
    UTF8ToUnicode(sUTF8, u, ofs);
    a := AnalyzeBidiUnicode(@u[0], length(u), ABaseDirection);
    setlength(result, length(u));
    for i := 0 to high(result) do
    begin
      result[i].Offset:= ofs[i];
      result[i].BidiInfo := a[i];
    end;
  end;
end;

function AnalyzeBidiUTF8(const sUTF8: string; ARightToLeft: boolean): TBidiUTF8Array;
begin
  if ARightToLeft then
    result := AnalyzeBidiUTF8(sUTF8, UNICODE_RIGHT_TO_LEFT_ISOLATE)
  else
    result := AnalyzeBidiUTF8(sUTF8, UNICODE_LEFT_TO_RIGHT_ISOLATE);
end;

function AnalyzeBidiUTF8(const sUTF8: string): TBidiUTF8Array;
begin
  result := AnalyzeBidiUTF8(sUTF8, UNICODE_FIRST_STRONG_ISOLATE)
end;

function GetUTF8DisplayOrder(const ABidi: TBidiUTF8Array): TUnicodeDisplayOrder;
begin
  if length(ABidi) = 0 then
    result := nil
  else
    result := GetUnicodeDisplayOrder(@ABidi[0].BidiInfo, sizeof(TBidiUTF8Info), length(ABidi));
end;

function ContainsBidiIsolateOrFormattingUTF8(const sUTF8: string): boolean;
var
  p,pEnd: PChar; //{$IFDEF FPC}PChar{$ELSE}PAnsiChar{$ENDIF};
  charLen: Integer;
  u: BGRACardinal;
begin
  if sUTF8 = '' then exit(false);
  p := @sUTF8[1];
  pEnd := p + length(sUTF8);
  while p < pEnd do
  begin
    charLen := UTF8CharacterLength(p);
    if (charLen = 0) or (p+charLen > pEnd) then break;
    u := UTF8CodepointToUnicode(p, charLen);
    case u of
      UNICODE_LEFT_TO_RIGHT_ISOLATE, UNICODE_RIGHT_TO_LEFT_ISOLATE, UNICODE_FIRST_STRONG_ISOLATE,
      UNICODE_LEFT_TO_RIGHT_EMBEDDING, UNICODE_RIGHT_TO_LEFT_EMBEDDING,
      UNICODE_LEFT_TO_RIGHT_OVERRIDE, UNICODE_RIGHT_TO_LEFT_OVERRIDE: exit(true);
    end;
    inc(p,charLen);
  end;
  exit(false);
end;

function UTF8OverrideDirection(const sUTF8: string; ARightToLeft: boolean): string;
begin
  if ARightToLeft then
    result := UnicodeCharToUTF8(UNICODE_RIGHT_TO_LEFT_OVERRIDE) + sUTF8 + UnicodeCharToUTF8(UNICODE_POP_DIRECTIONAL_FORMATTING)
  else
    result := UnicodeCharToUTF8(UNICODE_LEFT_TO_RIGHT_OVERRIDE) + sUTF8 + UnicodeCharToUTF8(UNICODE_POP_DIRECTIONAL_FORMATTING);
end;

function UTF8EmbedDirection(const sUTF8: string; ARightToLeft: boolean): string;
begin
  if ARightToLeft then
    result := UnicodeCharToUTF8(UNICODE_RIGHT_TO_LEFT_EMBEDDING) + sUTF8 + UnicodeCharToUTF8(UNICODE_POP_DIRECTIONAL_FORMATTING)
  else
    result := UnicodeCharToUTF8(UNICODE_LEFT_TO_RIGHT_EMBEDDING) + sUTF8 + UnicodeCharToUTF8(UNICODE_POP_DIRECTIONAL_FORMATTING);
end;

//little endian stream functions
function LEReadInt64(Stream: TStream): BGRAInt64;
begin
  Result := 0;
  stream.Read(Result, sizeof(Result));
  Result := LEtoN(Result);
end;

procedure LEWriteInt64(Stream: TStream; AValue: BGRAInt64);
begin
  AValue := NtoLE(AValue);
  stream.Write(AValue, sizeof(AValue));
end;

function LEReadLongint(Stream: TStream): BGRALongInt;
begin
  Result := 0;
  stream.Read(Result, sizeof(Result));
  Result := LEtoN(Result);
end;

procedure LEWriteLongint(Stream: TStream; AValue: BGRALongInt);
begin
  AValue := NtoLE(AValue);
  stream.Write(AValue, sizeof(AValue));
end;

function LEReadByte(Stream: TStream): byte;
begin
  Result := 0;
  stream.Read(Result, sizeof(Result));
end;

procedure LEWriteByte(Stream: TStream; AValue: Byte);
begin
  stream.Write(AValue, sizeof(AValue));
end;

function LEReadSingle(Stream: TStream): single;
var
  ResultAsDWord : BGRALongWord absolute result;
begin
  ResultAsDWord := 0;
  stream.Read(ResultAsDWord, sizeof(Result));
  ResultAsDWord := LEtoN(ResultAsDWord);
end;

procedure LEWriteSingle(Stream: TStream; AValue: single);
var
  ValueAsDWord : BGRALongWord absolute AValue;
begin
  ValueAsDWord := NtoLE(ValueAsDWord);
  stream.Write(ValueAsDWord, sizeof(AValue));
end;

procedure LEWriteVar(Stream: TStream; var AValue);
begin
  stream.Write(AValue, sizeof(AValue));
end;

procedure LEReadVar(Stream: TStream; out AValue);
begin
  stream.Read(AValue, sizeof(AValue));
end;


end.

