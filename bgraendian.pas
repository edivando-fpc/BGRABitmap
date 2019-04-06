unit bgraendian;
{
Copyright (C) 2019 by Domingo Galmés

Permission to use, copy, modify, and/or distribute this software for any purpose with 
or without fee is hereby granted.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO 
THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO 
EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER
IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
}

{$IFDEF FPC}
{$mode delphi}{$H+}
{$ASMMODE intel}
{$ELSE} //delphi
{$DEFINE ENDIAN_LITTLE}
{$ENDIF}

{$ifdef CPUX64}
{$else}
{$ifdef CPUX86}
{$else}
  {$DEFINE PUREPASCAL}
{$endif}
{$endif}


//{$DEFINE PUREPASCAL}


interface

uses
  Classes;// SysUtils;

{$IFDEF PUREPASCAL}
function SwapEndian64(const Value: UInt64): UInt64;
function SwapEndian32(const Value: UInt32): UInt32;
function SwapEndian16(const Value: UInt16): UInt16;inline;
{$ELSE}
function SwapEndian64(const Value: UInt64): UInt64;assembler;
function SwapEndian32(const Value: UInt32): UInt32; register;assembler;
function SwapEndian16(const Value: UInt16): UInt16; register;  assembler;
{$ENDIF}
function SwapEndianWord(const AValue: Word): Word;inline;

function BEToN(Value:UInt16):UInt16;overload;
function BEToN(Value:UInt32):UInt32;overload;
function BEToN(Value:UInt64):UInt64;overload;
function BEToN(Value:Int16):Int16;overload;
function BEToN(Value:Int32):Int32;overload;
function BEToN(Value:Int64):Int64;overload;
function BEToN(Value:Single):Single;overload;

function LEToN(Value:UInt16):UInt16;overload;
function LEToN(Value:UInt32):UInt32;overload;
function LEToN(Value:UInt64):UInt64;overload;
function LEToN(Value:Int16):Int16;overload;
function LEToN(Value:Int32):Int32;overload;
function LEToN(Value:Int64):Int64;overload;
function LEToN(Value:Single):Single;overload;

function NToBE(Value:UInt16):UInt16;overload;
function NToBE(Value:UInt32):UInt32;overload;
function NToBE(Value:UInt64):UInt64;overload;
function NToBE(Value:Int16):Int16;overload;
function NToBE(Value:Int32):Int32;overload;
function NToBE(Value:Int64):Int64;overload;
function NToBE(Value:Single):Single;overload;

function NToLE(Value:UInt16):UInt16;overload;
function NToLE(Value:UInt32):UInt32;overload;
function NToLE(Value:UInt64):UInt64;overload;
function NToLE(Value:Int16):Int16;overload;
function NToLE(Value:Int32):Int32;overload;
function NToLE(Value:Int64):Int64;overload;
function NToLE(Value:Single):Single;overload;


implementation


{$IFDEF PUREPASCAL}

type
  T64union=record
    case integer of
    0:
      (valor:UInt64);
    1:
      (bytes:array [0..7] of byte);
  end;

  T32union=record
    case integer of
    0:
      (valor:UInt32);
    1:
      (bytes:array [0..3] of byte);
  end;

function SwapEndian64(const Value: UInt64): UInt64;
var
  wo,wd:T64union;
begin
  wo.valor:=Value;
  wd.bytes[0]:=wo.bytes[7];
  wd.bytes[1]:=wo.bytes[6];
  wd.bytes[2]:=wo.bytes[5];
  wd.bytes[3]:=wo.bytes[4];
  wd.bytes[4]:=wo.bytes[3];
  wd.bytes[5]:=wo.bytes[2];
  wd.bytes[6]:=wo.bytes[1];
  wd.bytes[7]:=wo.bytes[0];
  result:=wd.valor;
end;

function SwapEndian32(const Value: UInt32): UInt32;
var
  wo,wd:T32union;
begin
  wo.valor:=Value;
  wd.bytes[0]:=wo.bytes[3];
  wd.bytes[1]:=wo.bytes[2];
  wd.bytes[2]:=wo.bytes[1];
  wd.bytes[3]:=wo.bytes[0];
  result:=wd.valor;
end;

function SwapEndian16(const Value: UInt16): UInt16;inline;
begin
  Result := UInt16((Word(Value) shr 8) or (Word(Value) shl 8));
end;

{$ELSE}
{$IFDEF CPUX86}
function SwapEndian64(const Value: UInt64): UInt64;assembler;
asm
 MOV     EDX,dword ptr [EBP + 12]
 MOV     EAX,dword ptr [EBP + 8]
 BSWAP   EAX
 XCHG    EAX,EDX
 BSWAP   EAX
end;

function SwapEndian32(const Value: UInt32): UInt32; register;assembler;{$IFDEF FPC}nostackframe;{$ENDIF}
asm
  bswap eax
end;

function SwapEndian16(const Value: UInt16): UInt16; register;  assembler;{$IFDEF FPC}nostackframe;{$ENDIF}
asm
  xchg  al, ah
end;
{$ENDIF}

//(* OJO tipo ensamblador
// ABI de windows 64 es diferente del de linux.
{$IFDEF CPUX64}
function SwapEndian64(const Value: UInt64): UInt64;
asm
  {$ifdef WIN64}
   mov rax,rcx
  {$else}
   mov rax,rdi
  {$endif}
  BSWAP  RAX
end;

function SwapEndian32(const Value: UInt32): UInt32; register;assembler;{$IFDEF FPC}nostackframe;{$ENDIF}
asm
 {$ifdef WIN64}
  mov eax,ecx
 {$else}
  mov eax,edi
 {$endif}
  bswap eax
end;

function SwapEndian16(const Value: UInt16): UInt16; register;  assembler;{$IFDEF FPC}nostackframe;{$ENDIF}
asm
 {$ifdef WIN64}
  mov eax,ecx
 {$else}
  mov eax,edi
 {$endif}
  xchg  al, ah
end;

{$ENDIF}

{$ENDIF}

function SwapEndianWord(const AValue: Word): Word;inline;
begin
  Result := Word((AValue shr 8) or (AValue shl 8));
end;


{$IFDEF ENDIAN_LITTLE}

function BEToN(Value: UInt16): UInt16;
begin
  Result:=SwapEndian16(Value);
end;

function BEToN(Value: UInt32): UInt32;
begin
  Result:=SwapEndian32(Value);
end;

function BEToN(Value: UInt64): UInt64;
begin
  Result:=SwapEndian64(Value);
end;

function BEToN(Value: Int16): Int16;
begin
  Result:=SwapEndian16(Value);
end;

function BEToN(Value: Int32): Int32;
begin
  Result:=SwapEndian32(Value);
end;

function BEToN(Value: Int64): Int64;
begin
  Result:=SwapEndian64(Value);
end;

function BEToN(Value:Single):Single;
var
  wTemp:UInt32 absolute Value;
begin
  wTemp:=SwapEndian32(wTemp);
  Result:=Value;
end;


function LEToN(Value: UInt16): UInt16;
begin
  Result:=Value;
end;

function LEToN(Value: UInt32): UInt32;
begin
  Result:=Value;
end;

function LEToN(Value: UInt64): UInt64;
begin
  Result:=Value;
end;

function LEToN(Value: Int16): Int16;
begin
  Result:=Value;
end;

function LEToN(Value: Int32): Int32;
begin
  Result:=Value;
end;

function LEToN(Value: Int64): Int64;
begin
  Result:=Value;
end;

function LEToN(Value:Single):Single;
begin
  Result:=Value;
end;

function NToBE(Value: UInt16): UInt16;
begin
  Result:=SwapEndian16(Value);
end;

function NToBE(Value: UInt32): UInt32;
begin
  Result:=SwapEndian32(Value);
end;

function NToBE(Value: UInt64): UInt64;
begin
  Result:=SwapEndian64(Value);
end;

function NToBE(Value: Int16): Int16;
begin
  Result:=SwapEndian16(Value);
end;

function NToBE(Value: Int32): Int32;
begin
  Result:=SwapEndian32(Value);
end;

function NToBE(Value: Int64): Int64;
begin
  Result:=SwapEndian64(Value);
end;

function NToBE(Value:Single):Single;
var
  wTemp:UInt32 absolute Value;
begin
  wTemp:=SwapEndian32(wTemp);
  Result:=Value;
end;


function NToLE(Value: UInt16): UInt16;
begin
  Result:=Value;
end;

function NToLE(Value: UInt32): UInt32;
begin
  Result:=Value;
end;

function NToLE(Value: UInt64): UInt64;
begin
  Result:=Value;
end;

function NToLE(Value: Int16): Int16;
begin
  Result:=Value;
end;

function NToLE(Value: Int32): Int32;
begin
  Result:=Value;
end;

function NToLE(Value: Int64): Int64;
begin
  Result:=Value;
end;

function NToLE(Value:Single):Single;
begin
  Result:=Value;
end;


{$ELSE}

 function LEToN(Value: UInt16): UInt16;
begin
  Result:=SwapEndian16(Value);
end;

function LEToN(Value: UInt32): UInt32;
begin
  Result:=SwapEndian32(Value);
end;

function LEToN(Value: UInt64): UInt64;
begin
  Result:=SwapEndian64(Value);
end;

function LEToN(Value: Int16): Int16;
begin
  Result:=SwapEndian16(Value);
end;

function LEToN(Value: Int32): Int32;
begin
  Result:=SwapEndian32(Value);
end;

function LEToN(Value: Int64): Int64;
begin
  Result:=SwapEndian64(Value);
end;

function LEToN(Value:Single):Single;
var
  wTemp:UInt32 absolute Value;
begin
  wTemp:=SwapEndian32(wTemp);
  Result:=Value;
end;

function BEToN(Value: UInt16): UInt16;
begin
  Result:=Value;
end;

function BEToN(Value: UInt32): UInt32;
begin
  Result:=Value;
end;

function BEToN(Value: UInt64): UInt64;
begin
  Result:=Value;
end;

function BEToN(Value: Int16): Int16;
begin
  Result:=Value;
end;

function BEToN(Value: Int32): Int32;
begin
  Result:=Value;
end;

function BEToN(Value: Int64): Int64;
begin
  Result:=Value;
end;

function BEToN(Value:Single):Single;
begin
  Result:=Value;
end;

function NToLE(Value: UInt16): UInt16;
begin
  Result:=SwapEndian16(Value);
end;

function NToLE(Value: UInt32): UInt32;
begin
  Result:=SwapEndian32(Value);
end;

function NToLEN(Value: UInt64): UInt64;
begin
  Result:=SwapEndian64(Value);
end;

function NToLE(Value: Int16): Int16;
begin
  Result:=SwapEndian16(Value);
end;

function NToLE(Value: Int32): Int32;
begin
  Result:=SwapEndian32(Value);
end;

function NToLE(Value: Int64): Int64;
begin
  Result:=SwapEndian64(Value);
end;

function NToLE(Value:Single):Single;
var
  wTemp:UInt32 absolute Value;
begin
  wTemp:=SwapEndian32(wTemp);
  Result:=Value;
end;


function NToBE(Value: UInt16): UInt16;
begin
  Result:=Value;
end;

function NToBE(Value: UInt32): UInt32;
begin
  Result:=Value;
end;

function NToBE(Value: UInt64): UInt64;
begin
  Result:=Value;
end;

function NToBE(Value: Int16): Int16;
begin
  Result:=Value;
end;

function NToBE(Value: Int32): Int32;
begin
  Result:=Value;
end;

function NToBE(Value: Int64): Int64;
begin
  Result:=Value;
end;

function NToBE(Value:Single):Single;
begin
  Result:=Value;
end;

{$ENDIF}

end.

