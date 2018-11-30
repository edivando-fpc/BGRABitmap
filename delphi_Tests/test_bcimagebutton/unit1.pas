unit Unit1;

interface

uses
  Forms, Graphics, sysutils,
  BCImageButton, BGRABitmap,
  BCFilters, BCButton, Classes, Vcl.Controls, BCBaseCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    BCImageButton1: TBCImageButton;
    BCImageButton2: TBCImageButton;
    BCImageButton3: TBCImageButton;
    BCImageButton4: TBCImageButton;
    BCImageButton5: TBCImageButton;
    BCImageButton6: TBCImageButton;
    procedure BCImageButton1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  BCImageButton1.BitmapFile := ExtractFilePath(Application.ExeName) + '\' + BCImageButton1.BitmapFile;
  BCImageButton2.BitmapFile := ExtractFilePath(Application.ExeName) + '\' + BCImageButton2.BitmapFile;
  BCImageButton3.BitmapFile := ExtractFilePath(Application.ExeName) + '\' + BCImageButton3.BitmapFile;
  BCImageButton4.BitmapFile := ExtractFilePath(Application.ExeName) + '\' + BCImageButton4.BitmapFile;
  BCImageButton5.BitmapFile := ExtractFilePath(Application.ExeName) + '\' + BCImageButton5.BitmapFile;
  BCImageButton6.BitmapFile := ExtractFilePath(Application.ExeName) + '\' + BCImageButton6.BitmapFile;

  BCImageButton1.LoadFromBitmapFile;
  BCImageButton2.LoadFromBitmapFile;
  BCImageButton2.TextVisible := False;
  BCImageButton3.LoadFromBitmapFile;
  BCImageButton4.LoadFromBitmapFile;
  BCImageButton5.LoadFromBitmapFile;

  GrayScale(BCImageButton1.BitmapOptions.Bitmap);

//  BCImageButton5.SaveToFile('button.bcimagebutton');
//  BCImageButton6.AssignFromFile('button.bcimagebutton');
end;

procedure TForm1.BCImageButton1Click(Sender: TObject);
begin
  BCImageButton1.Assign(BCImageButton2);
end;

end.

