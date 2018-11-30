program BGRA_Ribbon;

uses
  Vcl.Forms,
  uMain in 'uMain.pas' {frmMain},
  uresize in 'uresize.pas' {ResizeForm},
  uRibbon in 'uRibbon.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TResizeForm, ResizeForm);
  Application.Run;
end.
