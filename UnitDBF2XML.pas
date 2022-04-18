unit UnitDBF2XML;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Data.DB, dbf;

const
  Version = '1.0.0';

type
  TFormMain = class(TForm)
    Button: TButton;
    OpenDialog: TOpenDialog;
    ProgressBar: TProgressBar;
    Dbf: TDbf;
    procedure ButtonClick(Sender: TObject);
  private
    XmlFile: TextFile;
    procedure OpenFiles (FileName: String);
    procedure OpenDBF (FileName: String);
    procedure OpenXML (FileName: String);
    procedure CloseFiles ();
    procedure CloseDBF ();
    procedure CloseXML ();
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.OpenFiles (FileName: String);
begin
  OpenDBF (FileName);
  OpenXML (FileName);
end;

procedure TFormMain.OpenDBF (FileName: String);
begin
  dbf.FilePathFull := ExtractFilePath (FileName);
  dbf.TableName := ExtractFileName (FileName);
  dbf.Active := true;
  dbf.First;
  ProgressBar.Enabled := true;
  ProgressBar.Max := dbf.RecordCount;
end;

procedure TFormMain.OpenXML (FileName: String);
begin
  AssignFile(XmlFile, StringReplace (FileName, 'DBF', 'XML', []));
  Rewrite (XmlFile);
  Writeln (XmlFile, '<?xml version="1.0" encoding="UTF-8" standalone="yes" ?>');
end;

procedure TFormMain.CloseFiles ();
begin
  CloseDBF ();
  CloseXML ();
end;

procedure TFormMain.CloseDBF ();
begin
  dbf.Active := false;
end;

procedure TFormMain.CloseXML ();
begin
  CloseFile (XmlFile);
end;

procedure TFormMain.ButtonClick(Sender: TObject);
begin

  if not OpenDialog.Execute then begin
    ShowMessage ('Файл не выбран. Операция прервана.');
    Exit;
  end;

  OpenFiles (OpenDialog.FileName);

  while (not dbf.Eof) do begin
    dbf.Next;
    ProgressBar.StepIt;
  end;

  CloseFiles ();

end;

end.
