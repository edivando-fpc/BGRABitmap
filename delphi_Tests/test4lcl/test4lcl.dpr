program test4lcl;

uses
  Vcl.Forms,
  test_bcxbutton_ in '..\test_bcxbutton\test_bcxbutton_.pas' {Form16};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm16, Form16);
  Application.Run;
end.
