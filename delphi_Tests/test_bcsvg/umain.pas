unit umain;


interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, BCSVGViewer, BGRABitmap, BCTypes,
  BGRABitmapTypes, BGRASVG, BCBaseCtrls, BGRAGraphicControl;

type

  { TForm1 }

  TForm1 = class(TForm)
    BCSVGViewer1: TBCSVGViewer;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  BCSVGViewer1.LoadFromFile('SVG_Logo.svg');
//  BCSVGViewer1.LoadFromFile('01 CIRCLE.svg');
//  BCSVGViewer1.LoadFromResource('01 CIRCLE');
end;

end.

