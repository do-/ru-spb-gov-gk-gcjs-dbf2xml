unit UnitDBF2XML;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Data.DB, dbf, RegularExpressions;

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
    YYYY: String;
    MM: String;
    procedure ProcessFile (FileName: String);
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

function IsDigitsOnly (s: String): Boolean;
var i: integer;
begin
  Result := TRegEx.IsMatch (s, '[0-9]+');
end;

procedure TFormMain.OpenFiles (FileName: String);
var
  name: String;
begin
  name := ExtractFileName (FileName);
  if name.Length <> 13 then raise Exception.Create ('Некорректное имя файла (ожидаемая длина: 13 символов)');
  if not name.StartsWith ('GU_') then raise Exception.Create ('Имя файла должно начинаться с "GU_"');
  if not name.EndsWith ('.DBF', true) then raise Exception.Create ('Имя файла должно оканчиваться на ".DBF"');

  YYYY := name.Substring (3, 4);
  if not IsDigitsOnly (YYYY) then raise Exception.Create ('Некорректная цифра года: ' + YYYY);

  MM := name.Substring (7, 2);
  if not IsDigitsOnly (MM) then raise Exception.Create ('Некорректная цифра месяца: ' + MM);

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
  Writeln (XmlFile, '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>');
  Writeln (XmlFile, '<GU PERIOD="' + YYYY + '-' + MM + '" version="' + Version + '">');
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
  Writeln (XmlFile, '</GU>');
  CloseFile (XmlFile);
end;

procedure TFormMain.ProcessFile (FileName: String);
begin

  OpenFiles (FileName);

  while (not dbf.Eof) do begin
    dbf.Next;
    ProgressBar.StepIt;
  end;

  CloseFiles ();
  ShowMessage ('Файл обработан.');

end;

procedure TFormMain.ButtonClick(Sender: TObject);
begin

  if not OpenDialog.Execute then begin
    ShowMessage ('Файл не выбран. Операция прервана.');
    Exit;
  end;

  ProcessFile (OpenDialog.FileName);

end;

end.
