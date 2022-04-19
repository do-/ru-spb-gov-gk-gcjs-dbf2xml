program ProjectDBF2XML;

uses
  Vcl.Forms,
  UnitDBF2XML in 'UnitDBF2XML.pas' {FormMain, Version};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  FormMain.Caption := FormMain.Caption + ' ' + Version;
  Application.Run;
end.
