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


Unit BGRAMemDirectory;

{$i bgrabitmap.inc}{$H+}

interface

uses
  Classes, SysUtils, BGRATypes,{$IFNDEF FPC}Types, GraphType, BGRAGraphics,{$ENDIF} BGRAMultiFileType, {$IFDEF OBJ}fgl{$ELSE}generics.collections{$ENDIF};

const
  MemDirectoryFileHeader = 'TMemDirectory'#26#0#0;
  MemDirectoryEntry_FlagDirectory = 1;   //entry is a directory
  MemDirectoryEntry_FlagCompressed = 2;  //the stream is compressed
  MemDirectoryEntry_FlagSmallEntryPacked = $8000; //name and size <= 255

type
  TMemDirectory = class;
  TEntryFilename = BGRAMultiFileType.TEntryFilename;

//type
  TMemDirectoryPath = {$IFDEF OBJ}specialize TFPGList{$ELSE}TList{$ENDIF}<TEntryFilename>;

  { TMemDirectoryEntry }

  TMemDirectoryEntry = class(TMultiFileEntry)
  private
    FStream: TStream;
    function GetIsCompressed: boolean;
    function GetCompressedSize: BGRAInt64;
    function GetIsDirectory: boolean;
    procedure SetIsCompressed(AValue: boolean);
    constructor Create(AContainer: TMultiFileContainer; AFilename: TEntryFilename; AStream: TStream; AOwnStream: boolean; AUncompressedSize: BGRAInt64; AFlags: BGRAWord);overload;
    procedure LoadExtraFromEmbeddedStream(ADataStream: TStream; AStartPos: BGRAInt64);
    procedure SaveToEmbeddedStream(AEntryStream, ADataStream: TStream; AStartPos: BGRAInt64; out uncompressedSize: BGRAInt64);
  protected
    FFlags: BGRAWord;
    FName,FExtension: utf8String;
    FUncompressedSize: BGRAInt64;
    FEmbeddedStreamPos: BGRAInt64;
    FMemDirectory: TMemDirectory;
    function GetName: utf8string; override;
    procedure SetName(AValue: utf8string); override;
    function GetFileSize: BGRAInt64; override;
    function GetExtension: utf8string; override;
    function InternalCopyTo({%H-}ADestination: TStream): BGRAInt64;
  public
    function CopyTo({%H-}ADestination: TStream): BGRAInt64; override;
    constructor Create(AContainer: TMultiFileContainer; AFilename: TEntryFilename; AUncompressedStream: TStream; AOwnStream: boolean);overload;
    constructor CreateDirectory(AContainer: TMultiFileContainer; AFilename: TEntryFilename);
    destructor Destroy; override;
    property EmbeddedStreamPos: BGRAInt64 read FEmbeddedStreamPos write FEmbeddedStreamPos;
    property IsCompressed: boolean read GetIsCompressed write SetIsCompressed;
    property IsDirectory: boolean read GetIsDirectory;
    property CompressedSize: BGRAInt64 read GetCompressedSize;
    property Flags: BGRAWord read FFlags;
    property MemDirectory: TMemDirectory read FMemDirectory;
  end;

  TMemDirectory = class(TMultiFileContainer)
  private
    FParentDirectory: TMemDirectory;
    function GetEntryCompressed(AIndex: integer): boolean;
    function GetIsDirectory(AIndex: integer): boolean;
    function GetDirectory(AIndex: integer): TMemDirectory;
    procedure SetEntryCompressed(AIndex: integer; AValue: boolean);
  protected
    function CreateEntry(AName: utf8string; AExtension: utf8string; AContent: TStream): TMultiFileEntry; override;
    function SplitPath(APath: utf8string): TMemDirectoryPath;
  public
    constructor Create(AParentDirectory: TMemDirectory = nil);
    procedure LoadFromStream(AStream: TStream); override;
    procedure LoadFromEmbeddedStream(ARootStream, ADataStream: TStream; AStartPos: BGRAInt64);
    procedure SaveToStream(ADestination: TStream); override;
    procedure SaveToEmbeddedStream(ARootDest, ADataDest: TStream; AStartPos: BGRAInt64);
    function AddDirectory(AName: utf8string; AExtension: utf8string= ''; ACaseSensitive: boolean= true): integer;
    function FindPath(APath: utf8String; ACaseSensitive: boolean = true): TMemDirectory;
    function FindEntry(APath: utf8String; ACaseSensitive: boolean = true): TMemDirectoryEntry;
    property IsEntryCompressed[AIndex: integer]: boolean read GetEntryCompressed write SetEntryCompressed;
    property Directory[AIndex: integer]: TMemDirectory read GetDirectory;
    property IsDirectory[AIndex: integer]: boolean read GetIsDirectory;
    property ParentDirectory: TMemDirectory read FParentDirectory;
  end;

implementation

uses zstream, BGRAUTF8, strutils;

type
  TDirEntryRecord = packed record
    Flags: BGRAWord;
    FilenameSize: BGRAWord;
    Offset: BGRAInt64;
  end;

{ TMemDirectory }

function TMemDirectory.GetEntryCompressed(AIndex: integer): boolean;
begin
  result := (Entry[AIndex] as TMemDirectoryEntry).IsCompressed;
end;

function TMemDirectory.GetIsDirectory(AIndex: integer): boolean;
begin
  result := (Entry[AIndex] as TMemDirectoryEntry).IsDirectory;
end;

function TMemDirectory.GetDirectory(AIndex: integer): TMemDirectory;
begin
  result := (Entry[AIndex] as TMemDirectoryEntry).MemDirectory;
end;

procedure TMemDirectory.SetEntryCompressed(AIndex: integer; AValue: boolean);
begin
  (Entry[AIndex] as TMemDirectoryEntry).IsCompressed := AValue;
end;

function TMemDirectory.CreateEntry(AName: utf8string; AExtension: utf8string;
  AContent: TStream): TMultiFileEntry;
begin
  result := TMemDirectoryEntry.Create(self, EntryFilename(AName, AExtension), AContent, true);
end;

procedure TMemDirectory.LoadFromStream(AStream: TStream);
var rootPos, rootSize: integer;
  header: string;
  rootStream: TStream;
  startPos: BGRAInt64;
begin
  startPos := AStream.Position;
  setlength(header, length(MemDirectoryFileHeader));
  AStream.ReadBuffer(header[1], length(header));
  if header<>MemDirectoryFileHeader then
    raise exception.Create('Invalid header');
  rootPos := LEReadInt64(AStream);
  if rootPos = 0 then
    raise exception.Create('Invalid root offset');
  rootSize := LEReadInt64(AStream);
  if rootSize <= 4 then
    raise exception.Create('Invalid root size');
  AStream.Position:= rootPos + startPos;
  rootStream:= TMemoryStream.Create;
  try
    rootStream.CopyFrom(AStream, rootSize);
    LoadFromEmbeddedStream(rootStream, AStream, startPos);
  finally
    rootStream.Free;
  end;
end;

procedure TMemDirectory.LoadFromEmbeddedStream(ARootStream, ADataStream: TStream;
  AStartPos: BGRAInt64);
var
  nbEntries,i: BGRALongInt;
  entryRec: TDirEntryRecord;
  filename: string;
  entryData: TStream;
  newEntry: TMemDirectoryEntry;
  compressedSize, uncompressedSize: BGRAInt64;

begin
  Clear;
  ARootStream.Position := 0;
  nbEntries := LEReadLongint(ARootStream);
  for i := 1 to nbEntries do
  begin
    ARootStream.ReadBuffer({%H-}entryRec, sizeof(entryRec));
    {$IFNDEF BDS}entryRec.Offset:= LEtoN(entryRec.Offset);{$ENDIF}
    {$IFNDEF BDS}entryRec.Flags:= LEtoN(entryRec.Flags);{$ENDIF}
    {$IFNDEF BDS}entryRec.FilenameSize:= LEtoN(entryRec.FilenameSize);{$ENDIF}

    if (entryRec.Flags and MemDirectoryEntry_FlagSmallEntryPacked) <> 0 then
    begin
      entryRec.Flags := entryRec.Flags xor MemDirectoryEntry_FlagSmallEntryPacked;
      compressedSize := entryRec.FilenameSize shr 8;
      uncompressedSize := compressedSize;
      entryRec.FilenameSize := entryRec.FilenameSize and 255;
    end else
    begin
      compressedSize := LEReadInt64(ARootStream);
      uncompressedSize := LEReadInt64(ARootStream);
    end;

    setlength(filename, entryRec.FilenameSize);
    if length(filename)> 0 then
      ARootStream.ReadBuffer(filename[1], entryRec.FilenameSize);

    ADataStream.Position:= entryRec.Offset + AStartPos;
    entryData := TMemoryStream.Create;
    try
      entryData.CopyFrom(ADataStream, compressedSize);
      newEntry := TMemDirectoryEntry.Create(self, EntryFilename(filename), entryData, true,
                  uncompressedSize, entryRec.Flags);
      newEntry.LoadExtraFromEmbeddedStream(ADataStream, AStartPos);
      AddEntry(newEntry);
      entryData := nil;
    finally
      entryData.Free;
    end;
  end;
end;

procedure TMemDirectory.SaveToStream(ADestination: TStream);
var rootPos,rootSize: integer;
  header: string;
  rootRecPos, startPos, endPos: BGRAInt64;
  rootStream: TStream;
begin
  startPos := ADestination.Position;
  header := MemDirectoryFileHeader;
  ADestination.WriteBuffer(header[1], length(header));

  rootRecPos := ADestination.Position;
  LEWriteInt64(ADestination,0); //root pos
  LEWriteInt64(ADestination,0); //root size

  rootStream := TMemoryStream.Create;
  try
    SaveToEmbeddedStream(rootStream, ADestination, startPos);
    rootStream.Position := 0;
    rootPos := ADestination.Position - startPos;
    rootSize := rootStream.Size;
    ADestination.CopyFrom(rootStream, rootStream.Size);
    FreeAndNil(rootStream);
    endPos := ADestination.Position;
    ADestination.Position := rootRecPos;
    LEWriteInt64(ADestination, rootPos);
    LEWriteInt64(ADestination, rootSize);
    ADestination.Position := endPos;
  finally
    rootStream.Free;
  end;
end;

procedure TMemDirectory.SaveToEmbeddedStream(ARootDest, ADataDest: TStream;
  AStartPos: BGRAInt64);
var
  entryRec: TDirEntryRecord;
  entryStream: TMemoryStream;
  curEntry: TMemDirectoryEntry;
  filename: string;
  i: Integer;
  uncompressedSize: BGRAInt64;
begin
  LEWriteLongint(ARootDest, Count);
  entryStream := TMemoryStream.Create;
  try
    for i := 0 to Count-1 do
    begin
      curEntry := Entry[i] as TMemDirectoryEntry;
      entryStream.Clear;
      curEntry.SaveToEmbeddedStream(entryStream, ADataDest, AStartPos, uncompressedSize);

      entryRec.Offset:= ADataDest.Position - AStartPos;
      {$IFNDEF BDS}entryRec.Offset:= NtoLE(entryRec.Offset);{$ENDIF}
      if curEntry.Extension <> '' then
        filename := curEntry.Name+'.'+curEntry.Extension
      else
        filename := curEntry.Name;

      if ((curEntry.Flags and MemDirectoryEntry_FlagCompressed)=0) and
         (Length(filename)<=255) and (entryStream.Size<=255) then
      begin
        entryRec.Flags:= curEntry.Flags or MemDirectoryEntry_FlagSmallEntryPacked;
        {$IFNDEF BDS}entryRec.Flags:= NtoLE(entryRec.Flags);{$ENDIF}
        entryRec.FilenameSize:= length(filename) + (entryStream.Size shl 8);
        {$IFNDEF BDS}entryRec.FilenameSize := NtoLE(entryRec.FilenameSize);{$ENDIF}
        ARootDest.WriteBuffer(entryRec, sizeof(entryRec));
      end else
      begin
        entryRec.Flags:= curEntry.Flags;
        {$IFNDEF BDS}entryRec.Flags:= NtoLE(entryRec.Flags);{$ENDIF}
        entryRec.FilenameSize:= length(filename);
        {$IFNDEF BDS}entryRec.FilenameSize := NtoLE(entryRec.FilenameSize);{$ENDIF}
        ARootDest.WriteBuffer(entryRec, sizeof(entryRec));
        LEWriteInt64(ARootDest, entryStream.Size);
        LEWriteInt64(ARootDest, uncompressedSize);
      end;

      if filename <> '' then
        ARootDest.WriteBuffer(filename[1], length(filename));

      entryStream.Position:= 0;
      ADataDest.CopyFrom(entryStream, entryStream.Size);
    end;
  finally
    entryStream.Free;
  end;
end;

function TMemDirectory.AddDirectory(AName: utf8string; AExtension: utf8string;
  ACaseSensitive: boolean): integer;
var
  newEntry: TMemDirectoryEntry;
begin
  result := IndexOf(AName,AExtension,ACaseSensitive);
  if result <> -1 then
  begin
    if not IsDirectory[result] then
      raise exception.Create('There is already a file with this name and extension');
    exit;
  end;
  newEntry := TMemDirectoryEntry.CreateDirectory(self, EntryFilename(AName, AExtension));
  result := AddEntry(newEntry);
end;

function TMemDirectory.FindPath(APath: utf8String; ACaseSensitive: boolean): TMemDirectory;
var
  path: TMemDirectoryPath;
  idxPath: integer;
  idxSub: BGRALongInt;
begin
  path := SplitPath(APath);
  result := self;
  if path.Items[0].IsEmpty then
  begin
    idxPath := 1;
    while Assigned(result.ParentDirectory) do result := result.ParentDirectory;
  end
  else
    idxPath := 0;

  while idxPath < path.Count do
  begin
    idxSub := result.IndexOf(path[idxPath], ACaseSensitive);
    if idxSub= -1 then
    begin
      result := nil;
      break;
    end;
    result := result.Directory[idxSub];
    inc(idxPath);
  end;

  path.Free;
end;

function TMemDirectory.FindEntry(APath: utf8String; ACaseSensitive: boolean): TMemDirectoryEntry;
var
  path: TMemDirectoryPath;
  idxPath: integer;
  idxSub, idxEntry: BGRALongInt;
  curDir: TMemDirectory;
begin
  path := SplitPath(APath);
  curDir := self;
  if path.Items[0].IsEmpty then
  begin
    idxPath := 1;
    while Assigned(curDir.ParentDirectory) do curDir := curDir.ParentDirectory;
  end
  else
    idxPath := 0;

  while idxPath < path.Count-1 do
  begin
    idxSub := curDir.IndexOf(path[idxPath], ACaseSensitive);
    if idxSub= -1 then
    begin
      curDir := nil;
      break;
    end;
    curDir := curDir.Directory[idxSub];
    inc(idxPath);
  end;

  if Assigned(curDir) and (idxPath < path.Count) then
  begin
    idxEntry := curDir.IndexOf(path[idxPath], ACaseSensitive);
    if idxEntry = -1 then
      result := nil
    else
      result := curDir.Entry[idxEntry] as TMemDirectoryEntry;
  end
  else
    result := nil;

  path.Free;
end;

function TMemDirectory.SplitPath(APath: utf8string): TMemDirectoryPath;
var idx,idxSlash: integer;
begin
  result := TMemDirectoryPath.Create;
  idx := 1;
  repeat
    idxSlash := PosEx('/',APath,idx);
    if idxSlash = 0 then
    begin
      result.Add(EntryFilename(copy(APath, idx, length(APath)-idx+1)));
      break;
    end else
    begin
      result.Add(EntryFilename(copy(APath, idx, idxSlash-idx)));
      idx := idxSlash+1;
    end;
  until false;
end;

constructor TMemDirectory.Create(AParentDirectory: TMemDirectory);
begin
  inherited Create;
  FParentDirectory := AParentDirectory;
end;

{ TMemDirectoryEntry }

function TMemDirectoryEntry.GetIsCompressed: boolean;
begin
  result := (FFlags and MemDirectoryEntry_FlagCompressed) <> 0;
end;

function TMemDirectoryEntry.GetCompressedSize: BGRAInt64;
begin
  if not IsDirectory and Assigned(FStream) then
    result := FStream.Size
  else
    result := 0;
end;

function TMemDirectoryEntry.GetIsDirectory: boolean;
begin
  result := (FFlags and MemDirectoryEntry_FlagDirectory) <> 0;
end;

procedure TMemDirectoryEntry.SetIsCompressed(AValue: boolean);
var compressedStream,decompressed: TMemoryStream;
  compression: Tcompressionstream;
begin
  if AValue = IsCompressed then exit;

  if Assigned(FStream) then
  begin
    if AValue then //compress
    begin
      compressedStream := TMemoryStream.Create;
      compression := nil;
      try
        compression := Tcompressionstream.create(cldefault, compressedStream, true);
        FStream.Position := 0;
        compression.CopyFrom(FStream,FStream.Size);
        FStream.Free;
        FStream := compressedStream;
        compressedStream := nil;
        FFlags := FFlags xor MemDirectoryEntry_FlagCompressed;
      finally
        compression.Free;
        compressedStream.Free;
      end;
    end else
    begin //decompress
      decompressed := TMemoryStream.Create;
      try
        InternalCopyTo(decompressed);
        FStream.Free;
        FStream := decompressed;
        decompressed := nil;
        FFlags := FFlags xor MemDirectoryEntry_FlagCompressed;
      finally
        decompressed.Free;
      end;
    end;
  end else
    FFlags := FFlags xor MemDirectoryEntry_FlagCompressed;
end;

function TMemDirectoryEntry.GetName: utf8string;
begin
  result := FName;
end;

procedure TMemDirectoryEntry.SetName(AValue: utf8string);
begin
  while AValue[length(AValue)] = '.' do delete(AValue, length(AValue), 1);
  FName := AValue;
end;

function TMemDirectoryEntry.GetFileSize: BGRAInt64;
begin
  if IsDirectory then
    result := 0
  else
    Result:= FUncompressedSize;
end;

function TMemDirectoryEntry.GetExtension: utf8string;
begin
  Result:= FExtension;
end;

function TMemDirectoryEntry.InternalCopyTo(ADestination: TStream): BGRAInt64;
var
  decomp: Tdecompressionstream;
begin
  if not Assigned(FStream) then exit(0);
  if IsCompressed then
  begin
    FStream.Position := 0;
    decomp := Tdecompressionstream.Create(FStream,true);
    try
      result := ADestination.CopyFrom(decomp,FUncompressedSize);
    finally
      decomp.Free;
    end;
  end else
  begin
    FStream.Position := 0;
    result := ADestination.CopyFrom(FStream, FStream.Size);
  end;
end;

function TMemDirectoryEntry.CopyTo(ADestination: TStream): BGRAInt64;
begin
  if IsDirectory then exit(0);
  result := InternalCopyTo(ADestination);
end;

constructor TMemDirectoryEntry.Create(AContainer: TMultiFileContainer; AFilename: TEntryFilename;
  AUncompressedStream: TStream; AOwnStream: boolean);
begin
  inherited Create(AContainer);
  Create(AContainer, AFilename, AUncompressedStream, AOwnStream, AUncompressedStream.Size, 0);
end;

constructor TMemDirectoryEntry.Create(AContainer: TMultiFileContainer; AFilename: TEntryFilename;
  AStream: TStream; AOwnStream: boolean;
  AUncompressedSize: BGRAInt64; AFlags: BGRAWord);
begin
  inherited Create(AContainer);
  Name := AFilename.Name;
  FExtension:= AFilename.Extension;
  if AOwnStream then
    FStream := AStream
  else
  begin
    FStream := TMemoryStream.Create;
    AStream.Position:= 0;
    FStream.CopyFrom(AStream, AStream.Size);
  end;
  FUncompressedSize:= AUncompressedSize;
  FFlags:= AFlags;
  FMemDirectory := nil;
end;

procedure TMemDirectoryEntry.SaveToEmbeddedStream(AEntryStream, ADataStream: TStream;
  AStartPos: BGRAInt64; out uncompressedSize: BGRAInt64);
var
  entryStartPos: BGRAInt64;
begin
  if IsDirectory then
  begin
    if not Assigned(FMemDirectory) then
      raise exception.Create('Directory not allocated');
    FreeAndNil(FStream);
    IsCompressed:= false;
    entryStartPos := AEntryStream.Position;
    FMemDirectory.SaveToEmbeddedStream(AEntryStream, ADataStream, AStartPos);
    uncompressedSize:= AEntryStream.Position - entryStartPos;
  end else
  if Assigned(FStream) then
  begin
    FStream.Position:= 0;
    AEntryStream.CopyFrom(FStream, FStream.Size);
    uncompressedSize:= FUncompressedSize;
  end;
end;

procedure TMemDirectoryEntry.LoadExtraFromEmbeddedStream(ADataStream: TStream;
  AStartPos: BGRAInt64);
begin
  if IsDirectory and Assigned(FStream) then
  begin
    IsCompressed:= false;
    if not Assigned(FMemDirectory) then
      FMemDirectory := TMemDirectory.Create(Container as TMemDirectory);
    FMemDirectory.LoadFromEmbeddedStream(FStream, ADataStream, AStartPos);
    FreeAndNil(FStream);
  end;
end;

constructor TMemDirectoryEntry.CreateDirectory(AContainer: TMultiFileContainer;
  AFilename: TEntryFilename);
begin
  Name := AFilename.Name;
  FExtension:= AFilename.Extension;
  FStream := nil;
  FUncompressedSize:= 0;
  FFlags := MemDirectoryEntry_FlagDirectory;
  FMemDirectory := TMemDirectory.Create(Container as TMemDirectory);
end;

destructor TMemDirectoryEntry.Destroy;
begin
  FStream.Free;
  FMemDirectory.Free;
  inherited Destroy;
end;

end.

