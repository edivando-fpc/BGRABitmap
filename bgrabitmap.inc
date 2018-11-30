{$IFDEF FPC}
  {.$B-} // Boolean short-circuit evaluation ON
  {.$H+} // Long strings ON
  {.$T-} // Type-checked pointers OFF
  {.$X+} // Extended syntax ON

  {$IFNDEF RO_FPC_MODE_SET}
    {$mode Delphi}
    {$DEFINE BDS} //code for delphi compatibility

    {.$mode objfpc}{.$DEFINE OBJ} //for {$mode objfpc} enable {$DEFINE OBJ} together
  {$ENDIF}
  {$POINTERMATH ON}
  {$DEFINE INLINE}

  {$DEFINE BGRABITMAP}

  //(FPC_FULLVERSION<20501) >>> not defined(FPC20501)
  {.$DEFINE FPC20501}
  //(FPC_FULLVERSION<20602) >>> not defined(FPC20602)
  {.$DEFINE FPC20602}
  //(FPC_FULLVERSION<030001) >>> not defined(FPC030001)
  {.$DEFINE FPC030001}

  //(lcl_fullversion<1070000) >>> not defined(LCL_030001)
  {.$DEFINE LCL1070000}

{ You can define the following compiler directives in the package options,
    in tab Compiler options, in section Other. Directives must be prefixed with
    "-d". So for example:

    - to always have RGBA pixel format, write "-dBGRABITMAP_RGBAPIXEL"
    - to always have BGRA pixel format, write "-dBGRABITMAP_BGRAPIXEL"
    - to use fpGUI toolkit, write "-dBGRABITMAP_USE_FPGUI" }
  {$IFNDEF BGRABITMAP_DONT_USE_LCL}
    {$IFDEF LCL}
      {$DEFINE BGRABITMAP_USE_LCL}
    {$ENDIF}
  {$ENDIF}

  {$IFDEF BGRABITMAP_USE_FPGUI}{$UNDEF BGRABITMAP_USE_LCL}{$ENDIF}

  { Extensions of LCL to be used. Add // at the beginning of the line
    to comment them if the functions are not available }
  {$DEFINE BGRABITMAP_USE_LCL12} { Use functions of Lazarus 1.2 }
  {$DEFINE BGRABITMAP_USE_LCL15} { Use functions of Lazarus 1.5 }

  {.$DEFINE BDS} //delphi compatibility

  {$IFDEF OBJ}
  {$MODESWITCH ADVANCEDRECORDS}
  {$MODESWITCH TypeHelpers}
  {$ENDIF}
{$ELSE}
  {.$extendedsyntax on}
  {.$writeableconst on}
  {.$varstringchecks on}
  {.$typedaddress off}
  {.$overflowchecks off}
  {$rangechecks off}
  {.$realcompatibility off}
  {.$openstrings on}
  {$longstrings on}
  {.$booleval off}
  {.$safedivide off}
  {.$optimization on}
  {.$finitefloat off}
  {$EXCESSPRECISION OFF}
  {$POINTERMATH ON}
  {$DEFINE INLINE}
  {$INLINE ON}
  {$DEFINE BGRABITMAP}
  {$DEFINE BDS}
  {$DEFINE BGRABITMAP_USE_LCL}
  {$DEFINE BGRABITMAP_USE_LCL12} { Use functions of Lazarus 1.2 }
  {$DEFINE BGRABITMAP_USE_LCL15} { Use functions of Lazarus 1.5 }
  {$DEFINE FPC030001}
  {$DEFINE ENDIAN_LITTLE}
  {.$DEFINE RENDER_TEXT_ON_TBITMAP}
{$ENDIF}
