unit umain;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  BCImageButton, BCBaseCtrls;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    BCImageButton1: TBCImageButton;
    BCImageButton2: TBCImageButton;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
//  BCImageButton1.LoadFromBitmapResource('boton3d');
//  BCImageButton2.LoadFromBitmapResource('boton3d');
end;

end.

