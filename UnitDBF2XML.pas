unit UnitDBF2XML;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.DateUtils, System.Variants, System.Classes, Vcl.Graphics,
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
    procedure ProcessRecord ();
    procedure OpenFiles (FileName: String);
    procedure OpenDBF (FileName: String);
    procedure OpenXML (FileName: String);
    procedure CloseFiles ();
    procedure CloseDBF ();
    procedure CloseXML ();
    procedure WriteAttrValue (v: String);
    procedure WriteStringAttr (FieldName: String);
    procedure WriteDateAttr (FieldName: String);
    procedure WriteDecimalAttr (FieldName: String);
    procedure WriteStringAttr2 (FieldName: String; AttrName: String);
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
  Writeln (XmlFile, '<?xml version="1.0" encoding="cp866" standalone="yes"?>');
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
  while (not dbf.Eof) do ProcessRecord ();
  CloseFiles ();

  ShowMessage ('Файл обработан.');

end;

procedure TFormMain.WriteAttrValue (v: String);
var
  c: char;
  i: integer;
begin
  for I := 1 to v.Length do begin
    c := v [i];
    case c of
      '<': Write (XmlFile, '&lt;');
      '>': Write (XmlFile, '&gt;');
      '&': Write (XmlFile, '&amp;');
      '"': Write (XmlFile, '&quot;');
      '''': Write (XmlFile, '&apos;');
      else Write (XmlFile, c);
    end;
  end;
end;

procedure TFormMain.WriteStringAttr (FieldName: String);
begin
  WriteStringAttr2(FieldName, FieldName);
end;

procedure TFormMain.WriteDateAttr (FieldName: String);
var
  v: string;
  c: char;
  i: integer;
begin
  if dbf.FieldByName(FieldName).IsNull then exit;
  v := DateToISO8601 (dbf.FieldByName(FieldName).AsDateTime).Substring(0, 10);
  Write (XmlFile, ' ');
  Write (XmlFile, FieldName);
  Write (XmlFile, '="');
  Write (XmlFile, v);
  Write (XmlFile, '"');
end;

procedure TFormMain.WriteDecimalAttr (FieldName: String);
var
  v: string;
  c: char;
  i: integer;
begin
  if dbf.FieldByName(FieldName).IsNull then exit;
  v := dbf.FieldByName(FieldName).AsString.Replace (',', '.');
  Write (XmlFile, ' ');
  Write (XmlFile, FieldName);
  Write (XmlFile, '="');
  Write (XmlFile, v);
  Write (XmlFile, '"');
end;

procedure TFormMain.WriteStringAttr2 (FieldName: String; AttrName: String);
var
  v: string;
begin
  if dbf.FieldByName(FieldName).IsNull then exit;
  v := dbf.FieldByName(FieldName).AsString;
  if v.Length = 0 then Exit;
  if (AttrName = 'N_LIVE') and (v.Contains('/')) then Exit;
  Write (XmlFile, ' ');
  Write (XmlFile, AttrName);
  Write (XmlFile, '="');
  WriteAttrValue (v);
  Write (XmlFile, '"');
end;

procedure TFormMain.ProcessRecord ();
begin
  Write (XmlFile, ' <PIN');
  WriteStringAttr2 ('PIN', 'id');
  WriteStringAttr ('PIN_PR');
  WriteStringAttr ('L_NAME');
  WriteStringAttr ('F_NAME');
  WriteStringAttr ('S_NAME');
  WriteStringAttr ('BNF_CAT');
  WriteDateAttr ('INFO_DATE');
  WriteDateAttr ('BEGIN_DATE');
  WriteDateAttr ('END_DATE');
  WriteStringAttr ('N');
  WriteStringAttr ('DISTR_ID');
  WriteStringAttr ('STR_ID');
  WriteStringAttr ('STR_NAME');
  WriteStringAttr ('BUILDING');
  WriteStringAttr ('BLOCK_NUM');
  WriteStringAttr ('FLAT');
  WriteStringAttr ('MYNIC');
  WriteStringAttr ('LS_TYPE');
  WriteStringAttr ('N_ALL');
  WriteStringAttr ('N_LIVE');
  WriteDecimalAttr ('CHARGE_SUM');
  WriteStringAttr ('PRIORITET');
  Writeln (XmlFile, '>');
  Writeln (XmlFile, ' </PIN>');
  dbf.Next;
  ProgressBar.StepIt;
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
