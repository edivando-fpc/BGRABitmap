{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    Implements OS-independent loading of dynamic libraries.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit dynlibs;

{$INLINE ON}

interface

USES windows, sysutils, classes, types;

type
  TLibHandle = THandle;
  TOrdinalEntry = word;

const
  NilHandle = 0;
  SharedSuffix = 'dll';

Function SafeLoadLibrary(const Name : RawByteString) : TLibHandle; inline; overload;
Function LoadLibrary(const Name : RawByteString) : TLibHandle; inline; overload;
Function SafeLoadLibrary(const Name : UnicodeString) : TLibHandle; inline; overload;
Function LoadLibrary(const Name : UnicodeString) : TLibHandle; inline; overload;

Function GetProcedureAddress(Lib : TlibHandle; const ProcName : AnsiString) : {$ifdef cpui8086}FarPointer{$else}Pointer{$endif}; inline; overload;
Function GetProcedureAddress(Lib : TLibHandle; Ordinal: TOrdinalEntry) : {$ifdef cpui8086}FarPointer{$else}Pointer{$endif}; inline; overload;
Function UnloadLibrary(Lib : TLibHandle) : Boolean; inline;
Function GetLoadErrorStr: string; inline;

// Kylix/Delphi compability

Function FreeLibrary(Lib : TLibHandle) : Boolean; inline;
Function GetProcAddress(Lib : TlibHandle; const ProcName : AnsiString) : {$ifdef cpui8086}FarPointer{$else}Pointer{$endif}; inline;

Type
  HModule = TLibHandle;

Implementation


Function SafeLoadLibrary(const Name : RawByteString) : TLibHandle;
begin
  Result:= SafeLoadLibrary(Name);
end;

Function LoadLibrary(const Name : RawByteString) : TLibHandle;
begin
  Result:= LoadLibrary(Name);
end;

Function SafeLoadLibrary(const Name : UnicodeString) : TLibHandle;
begin
  Result:= SafeLoadLibrary(Name);
end;

Function LoadLibrary(const Name : UnicodeString) : TLibHandle;
begin
  Result:= LoadLibrary(Name);
end;


Function GetProcedureAddress(Lib : TLibHandle; const ProcName: AnsiString) : {$ifdef cpui8086}FarPointer{$else}Pointer{$endif};
begin
  Result:=GetProcedureAddress(Lib, ProcName);
end;

Function GetProcedureAddress(Lib : TLibHandle; Ordinal : TOrdinalEntry) : {$ifdef cpui8086}FarPointer{$else}Pointer{$endif};
begin
  Result:=GetProcedureAddress(Lib, Ordinal);
end;

Function UnloadLibrary(Lib : TLibHandle) : Boolean;
begin
  Result:=UnloadLibrary(Lib);
end;

Function GetLoadErrorStr: String;
begin
  Result:=GetLoadErrorStr;
end;

Function FreeLibrary(Lib : TLibHandle) : Boolean;

begin
  Result:=FreeLibrary(lib);
end;

Function GetProcAddress(Lib : TlibHandle; const ProcName : AnsiString) : {$ifdef cpui8086}FarPointer{$else}Pointer{$endif};

begin
  Result:=GetProcedureAddress(Lib,Procname);
end;

initialization

finalization
{$if declared(DoneDynLibs)}
  DoneDynLibs;
{$ifend}
end.
