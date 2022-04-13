unit UnitDBF2XML;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Data.DB, dbf;

type
  TFormMain = class(TForm)
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
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.ButtonClick(Sender: TObject);
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

  dbf.First;

  while (not dbf.Eof) do begin
    dbf.Next;
    ProgressBar.StepIt;
  end;

end;

end.
