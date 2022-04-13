program ProjectDBF2XML;

uses
  Vcl.Forms,
  UnitDBF2XML in 'UnitDBF2XML.pas' {FormMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
