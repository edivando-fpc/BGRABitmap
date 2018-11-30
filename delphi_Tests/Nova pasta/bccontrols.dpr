program bccontrols;

uses
  Vcl.Forms,
  Unit15 in 'Unit15.pas' {Form15},
  Unit2 in 'Unit2.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm15, Form15);
  Application.Run;
end.
