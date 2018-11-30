unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, JpgImg, StdCtrls, ComCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    JPEGImage1: TJPEGImage;
    OpenDialog1: TOpenDialog;
    Button2: TButton;
    ProgressBar1: TProgressBar;
    Button3: TButton;
    SaveDialog1: TSaveDialog;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure JPEGImage1Progress(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
begin
  OpenDialog1.InitialDir := ExtractFilePath(Application.ExeName);
  if OpenDialog1.Execute
    then JPEGImage1.FileName := OpenDialog1.FileName;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  JPEGImage1.Picture.Bitmap.Assign(nil);
end;

procedure TForm1.JPEGImage1Progress(Sender: TObject);
begin
  with ProgressBar1 do
  begin
    Position := Position + JPEGImage1.ProgressStep;
    if Position = 100 then Position := 0;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  if savedialog1.execute then
    jpegimage1.savetofile(savedialog1.filename);
end;

end.
 