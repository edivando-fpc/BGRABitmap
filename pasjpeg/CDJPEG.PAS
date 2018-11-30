Unit CdJpeg;
{$i FPImage.inc}{$H+}
{ OriginaL : cdjpeg.h+cdjpeg.c ;  Copyright (C) 1994-1996, Thomas G. Lane.

  This file contains common support routines used by the IJG application
  programs (cjpeg, djpeg, jpegtran).

  This file contains common declarations for the sample applications
  cjpeg and djpeg.  It is NOT used by the core JPEG library. }

{$define JPEG_CJPEG_DJPEG}      { define proper options in jconfig.h }
{$define JPEG_INTERNAL_OPTIONS} { cjpeg.c,djpeg.c need to see xxx_SUPPORTED }

(*@/// interface *)
interface

(*@/// uses *)
uses
  Classes,
  jmorecfg, jinclude, jpeglib,
  jdeferr,
  {cderror,          { get application-specific error codes }
  jerror;           { get library error codes too }
(*@\\\0000000303*)

const
  EXIT_SUCCESS = 0;
  EXIT_FAILURE = 1;
  EXIT_WARNING = 2;

type                            { Nomssi }
(*@///   BGRtype = packed record *)
BGRptr = ^BGRtype;
BGRtype = packed record
  b,g,r : byte;
end;
(*@\\\*)
type                            { Nomssi }
(*@///   RGBtype = packed record *)
RGBptr = ^RGBtype;
RGBtype = packed record
  r,g,b : JSAMPLE;
end;
(*@\\\*)

{ Object interface for cjpeg's source file decoding modules }

type
(*@///   cjpeg_source_struct = record *)
cjpeg_source_ptr = ^cjpeg_source_struct;
cjpeg_source_struct = record
  start_input : procedure (cinfo : j_compress_ptr;
                         sinfo : cjpeg_source_ptr);
  get_pixel_rows : function (cinfo : j_compress_ptr;
                           sinfo : cjpeg_source_ptr) : JDIMENSION;
  finish_input : procedure (cinfo : j_compress_ptr;
                          sinfo : cjpeg_source_ptr);
  input_file : TStream;

  buffer : JSAMPARRAY;
  buffer_height : JDIMENSION;
end;
(*@\\\0000000901*)

{ Object interface for djpeg's output file encoding modules }

type
(*@///   djpeg_dest_struct = record *)
djpeg_dest_ptr = ^djpeg_dest_struct;
djpeg_dest_struct = record
  { start_output is called after jpeg_start_decompress finishes.
    The color map will be ready at this time, if one is needed. }

  start_output : procedure (cinfo : j_decompress_ptr;
                            dinfo : djpeg_dest_ptr);
  { Emit the specified number of pixel rows from the buffer. }
  put_pixel_rows : procedure (cinfo : j_decompress_ptr;
                              dinfo : djpeg_dest_ptr;
                              rows_supplied : JDIMENSION);
  { Finish up at the end of the image. }
  finish_output : procedure (cinfo : j_decompress_ptr;
                             dinfo : djpeg_dest_ptr);

  { Target file spec; filled in by djpeg.c after object is created. }
  {output_file : FILEptr;}
   output_file: TStream;

  { Output pixel-row buffer.  Created by module init or start_output.
    Width is cinfo^.output_width * cinfo^.output_components;
    height is buffer_height. }

  buffer : JSAMPARRAY;
  buffer_height : JDIMENSION;
end;
(*@\\\0000001211*)

{ cjpeg/djpeg may need to perform extra passes to convert to or from
  the source/destination file format.  The JPEG library does not know
  about these passes, but we'd like them to be counted by the progress
  monitor.  We use an expanded progress monitor object to hold the
  additional pass count. }

type
(*@///   cdjpeg_progress_mgr = record *)
cd_progress_ptr = ^cdjpeg_progress_mgr;
cdjpeg_progress_mgr = record
  pub : jpeg_progress_mgr;    { fields known to JPEG library }
  completed_extra_passes : int;       { extra passes completed }
  total_extra_passes : int;   { total extra }
  { last printed percentage stored here to avoid multiple printouts }
  percent_done : int;
end;
(*@\\\*)

{GLOBAL}
procedure enable_signal_catcher (cinfo : j_common_ptr);

{ Case-insensitive matching of possibly-abbreviated keyword switches.
  keyword is the constant keyword (must be lower case already),
  minchars is length of minimum legal abbreviation. }

{GLOBAL}
function keymatch (arg : string;
                   const keyword : string;
                   minchars : int) : boolean;
(*@\\\0000001701*)
(*@/// implementation *)
implementation

(*@/// procedure enable_signal_catcher (cinfo : j_common_ptr); *)
procedure enable_signal_catcher (cinfo : j_common_ptr);
begin
  RunError(255);  { not translated - Jacques Nomssi }
end;
(*@\\\*)
(*@/// function keymatch (arg : string; *)
{ Case-insensitive matching of possibly-abbreviated keyword switches.
  keyword is the constant keyword (must be lower case already),
  minchars is length of minimum legal abbreviation. }

function keymatch (arg : string;
                   const keyword : string;
                   minchars : int) : boolean;
var
  {register} i : int;
  ca, ck : char;
  {register} nmatched : int;
begin
  nmatched := 0;

  i := 1;
  if length(arg) > length(keyword) then
  begin
    keymatch := FALSE; { arg longer than keyword, no good }
    exit;
  end;
  while (i <= length(arg)) do
  begin
    ca := UpCase(arg[i]);
    ck := UpCase(keyword[i]);
    Inc(i);
    if (ca <> ck) then
    begin
      keymatch := FALSE;                { no good }
      exit;
    end;
    Inc(nmatched);              { count matched characters }
  end;
  { reached end of argument; fail if it's too short for unique abbrev }
  keymatch := (nmatched >= minchars);
end;
(*@\\\*)
(*@\\\*)
end.

(*@/// Module selection routines for I/O modules. *)
{ Module selection routines for I/O modules. }

EXTERN(cjpeg_source_ptr) jinit_read_bmp ^.(cinfo : j_compress_ptr));
EXTERN(djpeg_dest_ptr) jinit_write_bmp ^.(cinfo : j_decompress_ptr;
                                            boolean is_os2));
EXTERN(cjpeg_source_ptr) jinit_read_gif ^.(cinfo : j_compress_ptr));
EXTERN(djpeg_dest_ptr) jinit_write_gif ^.(cinfo : j_decompress_ptr));
EXTERN(cjpeg_source_ptr) jinit_read_ppm ^.(cinfo : j_compress_ptr));
EXTERN(djpeg_dest_ptr) jinit_write_ppm ^.(cinfo : j_decompress_ptr));
EXTERN(cjpeg_source_ptr) jinit_read_rle ^.(cinfo : j_compress_ptr));
EXTERN(djpeg_dest_ptr) jinit_write_rle ^.(cinfo : j_decompress_ptr));
EXTERN(cjpeg_source_ptr) jinit_read_targa ^.(cinfo : j_compress_ptr));
EXTERN(djpeg_dest_ptr) jinit_write_targa ^.(cinfo : j_decompress_ptr));

{ cjpeg support routines (in rdswitch.c) }

EXTERN(boolean) read_quant_tables (cinfo : j_compress_ptr;
                                   filename : string;
                                   scale_factor : int;
                                   force_baseline : boolean);
EXTERN(boolean) read_scan_script (cinfo : j_compress_ptr; filename : string);
EXTERN(boolean) set_quant_slots (cinfo : j_compress_ptr; arg : char*);
EXTERN(boolean) set_sample_factors (cinfo : j_compress_ptr; arg : char*);

{ djpeg support routines (in rdcolmap.c) }

EXTERN(void) read_color_map ^.(cinfo : j_decompress_ptr; infile : FILE*);

{ common support routines (in cdjpeg.c) }

EXTERN(void) enable_signal_catcher ^.(j_common_ptr cinfo));
EXTERN(void) start_progress_monitor ^.(j_common_ptr cinfo,
                                         cd_progress_ptr progress));
EXTERN(void) end_progress_monitor ^.(j_common_ptr cinfo));
EXTERN(boolean) keymatch ^.(char * arg, const char * keyword, int minchars));
EXTERN(FILE *) read_stdin ^.(void));
EXTERN(FILE *) write_stdout ^.(void));

{ miscellaneous useful macros }
(*@\\\*)
(*@\\\0000000E01*)
