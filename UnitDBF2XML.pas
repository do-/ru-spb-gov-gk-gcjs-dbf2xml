unit UnitDBF2XML;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Data.DB, dbf;

type
  TForm1 = class(TForm)
    Button: TButton;
    OpenDialog: TOpenDialog;
    ProgressBar: TProgressBar;
    Dbf: TDbf;
    procedure ButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.ButtonClick(Sender: TObject);
begin
  if not OpenDialog.Execute then begin
    ShowMessage ('Wha?');
    Exit;
  end;
  dbf.FilePathFull := ExtractFilePath (OpenDialog.FileName);
  dbf.TableName := ExtractFileName (OpenDialog.FileName);


  dbf.Active := true;
  ProgressBar.Enabled := true;
  ProgressBar.Max := dbf.RecordCount;
  ProgressBar.StepBy (30);
end;

end.
