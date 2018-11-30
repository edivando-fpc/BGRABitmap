unit JpgImg;
{$i FPImage.inc}{$H+}
{$define TWO_FILE_COMMANDLINE}
{$define BMP_SUPPORTED}

{$I jconfig.inc}
(*$define save *)

(*@/// interface *)
interface

(*@/// uses *)
uses
  SysUtils,
  Classes,
  ExtCtrls,
  graphics,
  controls,
  Dialogs, {for property editor}
  messages,
  db,
  dbtables,
  dbctrls,
  jmorecfg,
  jpeglib,
  jerror,
  jdeferr,
  jdapimin, jdapistd, jdatasrc, jdatadst, wrbmp,
  jcapimin, jcapistd, jcparam, rdbmp,
  cdjpeg;               { Common decls for cjpeg/djpeg applications }
(*@\\\0000000701*)

type
  EJPEGException = class(Exception);
  TNumOfColors = (_16, _256, TrueColor);
  TDCTMethod = (Integ, FastInteg, FloatingPoint);
  TDither = (None, FS, Ordered);
  TDelta = 1..100;
  t_jpeg_quality = 0..100;

  (*@/// IMAGE_FORMATS = ( *)
  IMAGE_FORMATS = (
        FMT_BMP,                { BMP format (Windows flavor) }
        FMT_GIF,                { GIF format }
        FMT_OS2,                { BMP format (OS/2 flavor) }
        FMT_PPM,                { PPM/PGM (PBMPLUS formats) }
        FMT_RLE,                { RLE format }
        FMT_TARGA,              { Targa format }
        FMT_TIFF);              { TIFF format }
  (*@\\\*)
  (*@/// TCustomJPEGImage = class(TImage) *)
  TCustomJPEGImage = class(TImage)
  protected
    F_Stream: TMemoryStream;
    FNumOfColors: TNumOfColors;
    FDCTMethod: TDCTMethod;
    FDither: TDither;
    FFastProcessing, FGrayScale, FNoSmooth, FOnePass: Boolean;
    FFileName: string;
    FProgressStep: TDelta;
    FOnProgress: TNotifyEvent;
    f_quality: t_jpeg_quality;
    f_progress: boolean;

    {Procs for properties}
    procedure SetNumOfColors(Value: TNumOfColors);
    procedure SetDCTMethod(Value: TDCTMethod);
    procedure SetDither(Value: TDither);
    procedure SetBoolValue(Index: Integer; Value: Boolean);
    procedure SetFileName(Value: string);
  protected
    procedure parse_switches (cinfo : j_decompress_ptr;
                          last_file_arg_seen : int;
                          for_real : boolean);
    procedure UpdateIMG;
  protected
    property FileName: string read FFileName write SetFileName;
  public
    {Constructor/destructor}
    constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
    procedure LoadFromStream(value:TStream);
    procedure SaveToStream(value:TStream);
    procedure SaveToFile(const s:string);
    procedure LoadFromFile(const s:string);
  published
    property NumOfColors: TNumOfColors read FNumOfColors write SetNumOfColors;
    property DCTMethod: TDCTMethod read FDCTMethod write SetDCTMethod;
    property Dither: TDither read FDither write SetDither;
    property FastProcessing: Boolean index 0 read FFastProcessing write SetBoolValue;
    property GrayScale: Boolean index 1 read FGrayScale write SetBoolValue;
    property NoSmooth: Boolean index 2 read FNoSmooth write SetBoolValue;
    property OnePass: Boolean index 3 read FOnePass write SetBoolValue;
    property ProgressStep: TDelta read FProgressStep write FProgressStep;
    property OnProgress: TNotifyEvent read FOnProgress write FOnProgress;
    property Quality: t_jpeg_quality read f_quality write f_quality;
    property Progressive: boolean read f_progress write f_progress default false;
  end;
  (*@\\\0000001E0D*)
  (*@/// TJPEGImage = class(TCustomJPEGImage) *)
  TJPEGImage = class(TCustomJPEGImage)
  published
    property FileName;
  end;
  (*@\\\0000000315*)
  (*@/// TDBJPEGImage = class(TCustomJPEGImage) *)
  TDBJPEGImage = class(TCustomJPEGImage)
  protected
    fdatalink: tfielddatalink;
    fautodisplay: boolean;
    imageloaded: boolean;
    changing: boolean;
    oldchanged: tnotifyevent;
    function GetDataField: string;
    function getDataSource: TdataSource;
    procedure SetDataField(const value:string);
    procedure SetDataSource(value:TDatasource);
    procedure SetAutoDisplay(value: boolean);
    procedure DataChange(sender:TObject);
    procedure notification(component:TComponent; operation:TOperation); override;
    (*$ifndef ver80 *)
    procedure cmgetdatalink(var msg:TMessage); message cm_getdatalink;
    (*$endif *)
    procedure changed(sender:TObject);
    procedure updatedata(sender:Tobject);
  public
    constructor create(Owner:TComponent); override;
    destructor destroy; override;
    procedure LoadImage;
    procedure DBLoadFromStream(stream:TStream);
    procedure DBLoadFromFile(const filename:string);
    procedure DBSaveToStream(stream:TStream);
    procedure DBSaveToFile(const filename:string);
  published
    property DataField: string read getdatafield write setdatafield;
    property DataSource:TDataSource read GetDataSource write SetDataSource;
    property AutoDisplay: boolean read fautodisplay write SetAutoDisplay;
    property Picture stored false;
  end;
  (*@\\\000000170D*)

const
  DEFAULT_FMT = FMT_PPM;

var
  requested_fmt : IMAGE_FORMATS;

{Library procedures and functions}
function jpeg_getc (cinfo : j_decompress_ptr) : char;
function COM_handler (cinfo : j_decompress_ptr) : boolean;
(*@\\\0000001801*)
(*@/// implementation *)
implementation

(*@/// function jpeg_getc (cinfo : j_decompress_ptr) : char; *)
function jpeg_getc (cinfo : j_decompress_ptr) : char;
{ Read next byte }
var
  datasrc : jpeg_source_mgr_ptr;
begin
  datasrc := cinfo^.src;

  if (datasrc^.bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer (cinfo)) then
      ERREXIT(j_common_ptr(cinfo), JERR_CANT_SUSPEND);
  end;
  Dec(datasrc^.bytes_in_buffer);
  jpeg_getc := char(GETJOCTET(datasrc^.next_input_byte^));
  Inc(datasrc^.next_input_byte);
  end;
(*@\\\0000000201*)
(*@/// function COM_handler (cinfo : j_decompress_ptr) : boolean; {far;} *)
function COM_handler (cinfo : j_decompress_ptr) : boolean; {far;}
const
  LF = #10;
  CR = #13;
var
  length : INT32;
  ch : char;
  lastch : char;
begin
  lastch := #0;

  length := byte(jpeg_getc(cinfo)) shl 8;
  Inc(length, byte(jpeg_getc(cinfo)));
  Dec(length, 2);                       { discount the length word itself }

  while (length > 0) do begin
    Dec(length);
    ch := jpeg_getc(cinfo);
    end;
  COM_handler := TRUE;
  end;
(*@\\\*)

(*@/// TCustomJPEGImage=class(TImage) *)
(*@/// constructor TCustomJPEGImage.Create(AOwner: TComponent); *)
constructor TCustomJPEGImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  f_stream:=TMemoryStream.Create;
  FileName := '';
  NumOfColors := _256;
  DCTMethod := Integ;
  Dither := FS;
  GrayScale := False;
  NoSmooth := False;
  OnePass := False;
  FProgressStep := 10;
  f_quality := 75;
end;
(*@\\\0000000D12*)
(*@/// destructor TCustomJPEGImage.Destroy; *)
destructor TCustomJPEGImage.Destroy;
begin
  f_stream.free;
  inherited destroy;
  end;
(*@\\\*)
(*@/// procedure TCustomJPEGImage.parse_switches (cinfo : j_decompress_ptr; *)
procedure TCustomJPEGImage.parse_switches (cinfo : j_decompress_ptr;
                                    last_file_arg_seen : int;
                                    for_real : boolean);
const
  printed_version : boolean = FALSE;
begin
  { Set up default JPEG parameters. }
  requested_fmt := DEFAULT_FMT; { set default output file format }
  cinfo^.err^.trace_level := 0;
  { BMP output format. }
  requested_fmt := FMT_BMP;
  { Num of colors }
  if FNumOfColors <> TrueColor then
  begin
    case FNumOfColors of
          _16: cinfo^.desired_number_of_colors := 16;
         _256: cinfo^.desired_number_of_colors := 256;
    end;
    cinfo^.quantize_colors := TRUE;
  end;
  { DCT Method }
  case DCTMethod of
            Integ: cinfo^.dct_method := JDCT_ISLOW;
        FastInteg: cinfo^.dct_method := JDCT_IFAST;
    FloatingPoint: cinfo^.dct_method := JDCT_FLOAT;
  end;
  { Dither }
  case Dither of
       None: cinfo^.dither_mode := JDITHER_NONE;
         FS: cinfo^.dither_mode := JDITHER_FS;
    Ordered: cinfo^.dither_mode := JDITHER_ORDERED;
  end;
  { Fast Processing }
  if FFastProcessing then
  begin
    { Select recommended processing options for quick-and-dirty output. }
    cinfo^.two_pass_quantize := FALSE;
    cinfo^.dither_mode := JDITHER_ORDERED;
    if (not cinfo^.quantize_colors) then { don't override an earlier -colors }
      cinfo^.desired_number_of_colors := 216;
    cinfo^.dct_method := JDCT_FASTEST;
    cinfo^.do_fancy_upsampling := FALSE;
  end;
  { Grayscale }
  if FGrayScale then
    { Force monochrome output. }
    cinfo^.out_color_space := JCS_GRAYSCALE;
  if FNoSmooth then
    { Suppress fancy upsampling }
    cinfo^.do_fancy_upsampling := FALSE;
  if FOnePass then
    { Use fast one-pass quantization. }
    cinfo^.two_pass_quantize := FALSE;
end;
(*@\\\0000001601*)

(*@/// procedure TCustomJPEGImage.SetNumOfColors(Value: TNumOfColors); *)
procedure TCustomJPEGImage.SetNumOfColors(Value: TNumOfColors);
begin
  FNumOfColors := Value;
  UpdateIMG;
end;
(*@\\\*)
(*@/// procedure TCustomJPEGImage.SetDCTMethod(Value: TDCTMethod); *)
procedure TCustomJPEGImage.SetDCTMethod(Value: TDCTMethod);
begin
  FDCTMethod := Value;
  UpdateIMG;
end;
(*@\\\000000030D*)
(*@/// procedure TCustomJPEGImage.SetDither(Value: TDither); *)
procedure TCustomJPEGImage.SetDither(Value: TDither);
begin
  FDither := Value;
  UpdateIMG;
end;
(*@\\\*)
(*@/// procedure TCustomJPEGImage.SetBoolValue(Index: Integer; Value: Boolean); *)
procedure TCustomJPEGImage.SetBoolValue(Index: Integer; Value: Boolean);
begin
  case Index of
    0: FFastProcessing := Value;
    1: FGrayScale := Value;
    2: FNoSmooth := Value;
    3: FOnePass := Value;
  end;
  UpdateIMG;
end;
(*@\\\*)
(*@/// procedure TCustomJPEGImage.UpdateIMG; *)
procedure TCustomJPEGImage.UpdateIMG;
var
  cinfo : jpeg_decompress_struct;
  jerr : jpeg_error_mgr;
  dest_mgr : djpeg_dest_ptr;
  num_scanlines : JDIMENSION;
  OldProgress, NewProgress: Integer;
  OutStream: TMemoryStream;
begin
  if (csReading in ComponentState) then  EXIT;
  f_stream.seek(0,0);
  if f_stream.size=0 then
    self.picture.bitmap:=NIL
  else begin
    dest_mgr := NIL;
    OutStream:=NIL;
    { Initialize the JPEG decompression object with default error handling. }
    cinfo.err := jpeg_std_error(jerr);
    jpeg_create_decompress(@cinfo);
    { Add some application-specific error messages (from cderror.h) }
    {jerr.addon_message_table := cdjpeg_message_table;}
    jerr.first_addon_message := JMSG_FIRSTADDONCODE;
    jerr.last_addon_message := JMSG_LASTADDONCODE;
    { Insert custom COM marker processor. }
    jpeg_set_marker_processor(@cinfo, JPEG_COM, COM_handler);

    { Now safe to enable signal catcher. }
  {$ifdef NEED_SIGNAL_CATCHER}
    enable_signal_catcher(j_common_ptr (@cinfo));
  {$endif}

    { Scan command line to find file names. }
    { It is convenient to use just one switch-parsing routine, but the switch
      values read here are ignored; we will rescan the switches after opening
      the input file.
      (Exception: tracing level set here controls verbosity for COM markers
      found during jpeg_read_header...) }

    parse_switches(@cinfo, 0, FALSE);

    { Open the output stream. }
    try
      OutStream := TMemoryStream.Create;

    { Specify data source for decompression }
      jpeg_stdio_src(@cinfo, f_stream);

      { Read file header, set default decompression parameters }
      {void} jpeg_read_header(@cinfo, TRUE);

      { Adjust default decompression parameters by re-parsing the options }
      parse_switches(@cinfo, 0, TRUE);

      { Initialize the output module now to let it override any crucial
        option settings (for instance, GIF wants to force color quantization). }

      case (requested_fmt) of
    {$ifdef BMP_SUPPORTED}
      FMT_BMP:
        dest_mgr := jinit_write_bmp(@cinfo, FALSE);
    {$endif}
      else
        ERREXIT(j_common_ptr(@cinfo), JERR_UNSUPPORTED_FORMAT);
      end;
      {dest_mgr^.output_file := @output_file;}
      dest_mgr^.output_file := OutStream;

      { Start decompressor }
      {void} jpeg_start_decompress(@cinfo);

      { Write output file header }
      dest_mgr^.start_output (@cinfo, dest_mgr);

      OldProgress := 0;
      { Process data }
      while (cinfo.output_scanline < cinfo.output_height) do begin
        num_scanlines := jpeg_read_scanlines(@cinfo, dest_mgr^.buffer,
                                            dest_mgr^.buffer_height);
        dest_mgr^.put_pixel_rows (@cinfo, dest_mgr, num_scanlines);
        NewProgress := (cinfo.output_scanline * 100 div cinfo.output_height);
        if (NewProgress <> OldProgress) and (NewProgress mod FProgressStep = 0) then begin
          OldProgress := NewProgress;
          if Assigned(FOnProgress) then
            FOnProgress(Self);
          end;
        end;

      { Finish decompression and release memory.
        I must do it in this order because output module has allocated memory
        of lifespan JPOOL_IMAGE; it needs to finish before releasing memory. }

      dest_mgr^.finish_output (@cinfo, dest_mgr);
      {void} jpeg_finish_decompress(@cinfo);
      jpeg_destroy_decompress(@cinfo);

      { All done. }
      if jerr.num_warnings <> 0 then begin
        raise EJPEGException.Create('There are warnings');
        end
      else begin
        OutStream.Position := 0;
        (Picture.Bitmap as TGraphic).LoadFromStream(outstream);
        {I do not know why, but the next line avoids colors to be corrupted on
         256 colors mode}
        Picture.Bitmap.Palette := Picture.Bitmap.Palette;
        end;
    finally
      OutStream.Free;
      end;
    end;
  (* just to be sure *)
  self.invalidate;
  end;
(*@\\\0030000A06000A23000A06*)
(*@/// procedure TCustomJPEGImage.SetFileName(Value: string); *)
procedure TCustomJPEGImage.SetFileName(Value: string);
var
  stream: TFileStream;
begin
  stream:=NIL;
  try
    ffilename:=value;
    if value='' then
      f_stream.clear
    else begin
      stream:=TFileStream.Create(value,fmOpenRead);
      f_stream.loadfromstream(stream);
      end;
    UpdateIMG;
  finally
    stream.free;
    end;
  end;
(*@\\\0000000201*)
(*@/// procedure TCustomJPEGImage.LoadFromStream(value: TStream); *)
procedure TCustomJPEGImage.LoadFromStream(value: TStream);
begin
  if value<>NIL then
    f_stream.loadfromstream(value)
  else
    f_stream.clear;
  updateIMG;
  end;
(*@\\\0000000306*)
(*@/// procedure TCustomJPEGImage.SaveToStream(value:TStream); *)
procedure TCustomJPEGImage.SaveToStream(value:TStream);
(*$ifdef save *)
var
  cinfo : jpeg_compress_struct;
  jerr : jpeg_error_mgr;
  InStream: TMemoryStream;
  src_mgr : cjpeg_source_ptr;
  file_index : int;
  num_scanlines : JDIMENSION;
  OldProgress, NewProgress: Integer;
begin
  if value=NIL then EXIT;
  if Picture.Bitmap=NIL then EXIT;
  src_mgr := NIL;
  { Initialize the JPEG decompression object with default error handling. }
  cinfo.err := jpeg_std_error(jerr);
  jpeg_create_compress(@cinfo);
  { Add some application-specific error messages (from cderror.h) }
  {jerr.addon_message_table := cdjpeg_message_table;}
  jerr.first_addon_message := JMSG_FIRSTADDONCODE;
  jerr.last_addon_message := JMSG_LASTADDONCODE;

  { Set some defaults }
  cinfo.in_color_space := JCS_RGB; { arbitrary guess }
  jpeg_set_defaults(@cinfo);

  { Now safe to enable signal catcher. }
{$ifdef NEED_SIGNAL_CATCHER}
  enable_signal_catcher(j_common_ptr (@cinfo));
{$endif}

  try
    InStream:=TMemorystream.create;
    (Picture.Bitmap as TGraphic).SaveToStream(InStream);
    InStream.seek(0,0);

    src_mgr := jinit_read_bmp(@cinfo);
{     src_mgr^.output_file := @value; }
    src_mgr^.input_file := instream;
    src_mgr^.start_input (@cinfo, src_mgr);

    { Now that we know input colorspace, fix colorspace-dependent defaults }
    jpeg_default_colorspace(@cinfo);
    if grayscale then
      jpeg_set_colorspace(@cinfo, JCS_GRAYSCALE);

    case DCTMethod of
              Integ: cinfo.dct_method := JDCT_ISLOW;
          FastInteg: cinfo.dct_method := JDCT_IFAST;
      FloatingPoint: cinfo.dct_method := JDCT_FLOAT;
      end;

    cinfo.optimize_coding := not fastprocessing;
    jpeg_set_quality(@cinfo, quality, false);
    if progressive then
      jpeg_simple_progression(@cinfo);

    {$ifdef PROGRESS_REPORT}
    start_progress_monitor(j_common_ptr ( @cinfo, @progress);
    {$endif}

    jpeg_stdio_dest(@cinfo, value);

    { Start compressor }
    jpeg_start_compress(@cinfo, TRUE);

    { Process data }
    while (cinfo.next_scanline < cinfo.image_height) do begin
      num_scanlines := src_mgr^.get_pixel_rows (@cinfo, src_mgr);
      {void} jpeg_write_scanlines(@cinfo, src_mgr^.buffer, num_scanlines);
      NewProgress := (cinfo.next_scanline * 100 div cinfo.image_height);
      if (NewProgress <> OldProgress) and (NewProgress mod FProgressStep = 0) then begin
        OldProgress := NewProgress;
        if Assigned(FOnProgress) then
          FOnProgress(Self);
        end;
      end;

    { Finish compression and release memory }
    src_mgr^.finish_input (@cinfo, src_mgr);
    jpeg_finish_compress(@cinfo);
    jpeg_destroy_compress(@cinfo);


  finally
    {$ifdef PROGRESS_REPORT}
    end_progress_monitor(j_common_ptr (@cinfo));
    {$endif}

    InStream.free;
    end;
  end;

(*$else *)
begin
  end;
(*$endif *)
(*@\\\0000004118*)
(*@/// procedure TCustomJPEGImage.SaveToFile(const s:string); *)
procedure TCustomJPEGImage.SaveToFile(const s:string);
var
  stream: TFileStream;
begin
  stream:=NIL;
  try
    stream:=TFileStream.Create(s,fmCreate);
    savetostream(stream);
  finally
    stream.free;
    end;
  end;
(*@\\\0000000701*)
(*@/// procedure TCustomJPEGImage.LoadFromFile(const s:string); *)
procedure TCustomJPEGImage.LoadFromFile(const s:string);
begin
  (* filename is a property and launches the action automagically *)
  filename:=s;
  end;
(*@\\\*)
(*@\\\0000000901*)
(*@/// TDBJPEGImage=class(TCustomJPEGImage) *)
(*@/// function TDBJPEGImage.GetDataField: string; *)
function TDBJPEGImage.GetDataField: string;
begin
  result:=fdatalink.fieldname;
  end;
(*@\\\0000000117*)
(*@/// function TDBJPEGImage.getDataSource: TdataSource; *)
function TDBJPEGImage.getDataSource: TdataSource;
begin
  result:=fdatalink.datasource;
  end;
(*@\\\0000000117*)
(*@/// procedure TDBJPEGImage.SetDataField(const value:string); *)
procedure TDBJPEGImage.SetDataField(const value:string);
begin
  fdatalink.fieldname:=value;
  if fautodisplay and not imageloaded then
    loadimage;
  end;
(*@\\\0000000601*)
(*@/// procedure TDBJPEGImage.SetDataSource(value:TDatasource); *)
procedure TDBJPEGImage.SetDataSource(value:TDatasource);
begin
  fdatalink.datasource:=value;
  (*$ifndef ver80 *)
  if value<>nil then
    value.freenotification(self);
  (*$endif *)
  end;
(*@\\\*)
(*@/// procedure TDBJPEGImage.SetAutoDisplay(value: boolean); *)
procedure TDBJPEGImage.SetAutoDisplay(value: boolean);
begin
  fautodisplay:=value;
  if fautodisplay and not imageloaded then
    loadimage;
  end;
(*@\\\0000000401*)
(*@/// procedure TDBJPEGImage.DataChange(sender:TObject); *)
procedure TDBJPEGImage.DataChange(sender:TObject);
begin
  changing:=true;
  try
    if (fdatalink.field=NIL) then
      self.picture.bitmap:=NIL
    else if (fdatalink.editing and imageloaded) then
    else if not autodisplay then begin
      self.picture.bitmap:=NIL;
      imageloaded:=false;
      end
    else
      loadimage;
  finally
    changing:=false;
    end;
  end;
(*@\\\*)
(*@/// procedure TDBJPEGImage.notification(component:TComponent; operation:TOperation); *)
procedure TDBJPEGImage.notification(component:TComponent; operation:TOperation);
begin
  inherited notification(component,operation);
  if (fdatalink<>NIL) and (component=datasource) and (operation=opremove) then
    datasource:=NIL;
  end;
(*@\\\0000000301*)
(*$ifndef ver80 *)
(*@/// procedure TDBJPEGImage.cmgetdatalink(var msg:TMessage); *)
procedure TDBJPEGImage.cmgetdatalink(var msg:TMessage);
begin
  msg.result:=longint(fdatalink);
  end;
(*@\\\*)
(*$endif *)
(*@/// constructor TDBJPEGImage.create(Owner:TComponent); *)
constructor TDBJPEGImage.create(Owner:TComponent);
begin
  inherited create(owner);
  fdatalink:=tfielddatalink.create;
  fdatalink.ondatachange:=datachange;
  fdatalink.onupdatedata:=updatedata;
{   fdatalink.oneditingchange:=editingchange; }
  fdatalink.control:=NIL;    (* normally self, but as TImage isn't a
                                TWinControl I can't use it *)
  oldchanged:=self.picture.onchange;
  self.picture.onchange:=self.changed;
  end;
(*@\\\0000000A25*)
(*@/// destructor TDBJPEGImage.destroy; *)
destructor TDBJPEGImage.destroy;
begin
  fdatalink.free;
  inherited destroy;
  end;
(*@\\\0000000507*)
(*@/// procedure TDBJPEGImage.LoadImage; *)
procedure TDBJPEGImage.LoadImage;
var
  bs: TBlobStream;
begin
  if fdatalink.field is TBlobfield then begin
    bs:=NIL;
    try
      bs:=TBlobstream.create(tblobfield(fdatalink.field),bmread);
      changing:=true;
      self.loadfromstream(bs);
    finally
      bs.free;
      changing:=false;
      end;
    imageloaded:=true;
    end;
  end;
(*@\\\*)
(*@/// procedure TDBJPEGImage.changed(sender:TObject); *)
procedure TDBJPEGImage.changed(sender:TObject);
begin
  if not (changing or (csDesigning in ComponentState)) then begin
    imageloaded:=true;
    fdatalink.edit;
    fdatalink.modified;
    end;
  if assigned(oldchanged) then oldchanged(sender);
  end;
(*@\\\000C00031700033600030A*)
(*@/// procedure TDBJPEGImage.updatedata(sender:Tobject); *)
procedure TDBJPEGImage.updatedata(sender:Tobject);
var
  bs: TBlobStream;
begin
  if fdatalink.field is TBlobfield then begin
    bs:=NIL;
    try
      bs:=TBlobstream.create(tblobfield(fdatalink.field),bmwrite);
      self.savetostream(bs);
    finally
      bs.free;
      end;
    imageloaded:=true;
    end;
  end;
(*@\\\*)

(*@/// procedure TDBJPEGImage.DBLoadFromStream(stream:TStream); *)
procedure TDBJPEGImage.DBLoadFromStream(stream:TStream);
var
  bs:TBlobstream;
begin
  if fdatalink.field is TBlobfield then begin
    bs:=NIL;
    try
      fdatalink.edit;
      bs:=TBlobstream.create(tblobfield(fdatalink.field),bmwrite);
      bs.copyfrom(stream,0);
      if autodisplay then begin
        changing:=true;
        self.loadfromstream(stream);
        end;
    finally
      changing:=false;
      bs.free;
      end;
    end;
  end;
(*@\\\*)
(*@/// procedure TDBJPEGImage.DBLoadFromFile(const filename:string); *)
procedure TDBJPEGImage.DBLoadFromFile(const filename:string);
var
  fs: TFileStream;
begin
  fs:=NIL;
  try
    fs:=TFilestream.create(filename,fmopenread);
    dbloadfromstream(fs);
  finally
    fs.free;
    end;
  end;
(*@\\\000000072B*)
(*@/// procedure TDBJPEGImage.DBSaveToStream(stream:TStream); *)
procedure TDBJPEGImage.DBSaveToStream(stream:TStream);
var
  bs:TBlobstream;
begin
  if fdatalink.field is TBlobfield then begin
    bs:=NIL;
    try
      bs:=TBlobstream.create(tblobfield(fdatalink.field),bmread);
      stream.copyfrom(bs,0);
    finally
      bs.free;
      end;
    end;
  end;
(*@\\\0000000A01*)
(*@/// procedure TDBJPEGImage.DBSaveToFile(const filename:string); *)
procedure TDBJPEGImage.DBSaveToFile(const filename:string);
var
  fs: TFileStream;
begin
  fs:=NIL;
  try
    fs:=TFilestream.create(filename,fmcreate);
    dbsavetostream(fs);
  finally
    fs.free;
    end;
  end;
(*@\\\*)
(*@\\\0000000E01*)
(*@\\\0002000601000601*)
end.
(*@\\\0001000011000A01*)
