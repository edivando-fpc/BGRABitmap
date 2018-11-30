unit JpgImgR;
{$i FPImage.inc}{$H+}  (*$i ah_def.inc *)
(*@/// interface *)
interface

(*@/// uses *)
uses
  SysUtils,
  Classes,
  ExtCtrls,
  graphics,
  controls,
//  (*$ifdef Delphi_ge_6 *)
  DesignIntf,
  DesignEditors,
  VCLEditors,
//  (*$else *)
//  DsgnIntf,
//  (*$endif *)
  Dialogs,   {for property editor}
  JpgImg;
(*@\\\003C000B01000B01000A01000A01*)

type
  (*@/// TJPEGNameProperty = class(TStringProperty) *)
  TJPEGNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;
  (*@\\\*)

procedure Register;
(*@\\\0000000301*)
(*@/// implementation *)
implementation

{$ifdef delphi_1}
  {$R jpgimg.d16}
{$else}
  {$R jpgimg.d32}
{$endif}


(*@/// procedure Register; *)
procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(string), TJPEGImage, 'FileName', TJPEGNameProperty);
  RegisterComponents('Custom', [TJPEGImage]);
  RegisterComponents('Custom', [TDBJPEGImage]);
  end;
(*@\\\*)

(*@/// TJPEGNameProperty=class(TStringProperty) *)
(*@/// function TJPEGNameProperty.GetAttributes: TPropertyAttributes; *)
function TJPEGNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paMultiSelect, paAutoUpdate];
  end;
(*@\\\0000000403*)
(*@/// procedure TJPEGNameProperty.Edit; *)
procedure TJPEGNameProperty.Edit;
begin
  with TOpenDialog.Create(nil) do begin
    DefaultExt := 'jpg';
    Filter := 'JPeg Files (*.jpg)|*.jpg|All Files (*.*)|*.*';
    Options := [ofHideReadOnly,ofPathMustExist,ofFileMustExist];
    FileName := GetStrValue;
    Title := 'Select JPeg File';
    if Execute then SetStrValue(FileName);
    Free;
    end;
  end;
(*@\\\0000000B05*)
(*@\\\*)
(*@\\\*)
end.
(*@\\\0003000201000011000201*)
