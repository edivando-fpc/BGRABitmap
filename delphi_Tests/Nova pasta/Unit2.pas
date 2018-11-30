unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, DTAnalogCommon, dtthemedclock,
  DTAnalogGauge, BCBaseCtrls, DTAnalogClock, BCRadialProgressBar, BCImageButton,
  BCSVGButton, BCSVGViewer;

type
  TForm2 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    BCImageButton5: TBCImageButton;
    FBCRadialProgressBar : TBCRadialProgressBar;
    BCSVGButton : TBCSVGButton;
    BCSVGViewer : TBCSVGViewer;
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.FormCreate(Sender: TObject);
begin
  BCImageButton5 := TBCImageButton.Create(Self);
  BCImageButton5.Parent := Self;

{  BCSVGButton := TBCSVGButton.Create(Self);
  BCSVGButton.Parent := Self;

  BCSVGViewer := TBCSVGViewer.Create(Self);
  BCSVGViewer.Parent := Self;

  FBCRadialProgressBar := TBCRadialProgressBar.Create(Self);
  FBCRadialProgressBar.Parent := Self;}
end;

end.
