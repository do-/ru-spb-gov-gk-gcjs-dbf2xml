program ProjectDBF2XML;

uses
  Vcl.Forms,
  UnitDBF2XML in 'UnitDBF2XML.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
