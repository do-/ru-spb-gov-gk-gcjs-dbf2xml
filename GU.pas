unit GU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.DateUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Data.DB, dbf, RegularExpressions, System.Generics.Collections;

const
  Version = '1.0.0';

type
  TGU = class
  private
    Dbf: TDbf;
    XmlFile: TextFile;
    YYYY: String;
    MM: String;
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
    procedure OpenElementPIN ();
    procedure CloseElementPIN ();
    procedure WriteElementMC_ACC (i: integer; f2a: TDictionary <string, TStrings>);
    procedure WriteElementRSO_ACC (i: integer);
    procedure WriteElementREP_ACC ();
  public
    constructor Create (_Dbf: TDbf);
    procedure ProcessFile (FileName: String);
  end;

implementation

constructor TGU.Create (_Dbf: TDbf);
begin
  self.dbf := _dbf;
end;

function IsDigitsOnly (s: String): Boolean;
var i: integer;
begin
  Result := TRegEx.IsMatch (s, '[0-9]+');
end;

procedure TGU.OpenFiles (FileName: String);
var
  name: String;
begin
  name := ExtractFileName (FileName);
  if name.Length <> 13 then raise Exception.Create ('������������ ��� ����� (��������� �����: 13 ��������)');
  if not name.StartsWith ('GU_') then raise Exception.Create ('��� ����� ������ ���������� � "GU_"');
  if not name.EndsWith ('.DBF', true) then raise Exception.Create ('��� ����� ������ ������������ �� ".DBF"');

  YYYY := name.Substring (3, 4);
  if not IsDigitsOnly (YYYY) then raise Exception.Create ('������������ ����� ����: ' + YYYY);

  MM := name.Substring (7, 2);
  if not IsDigitsOnly (MM) then raise Exception.Create ('������������ ����� ������: ' + MM);

  OpenDBF (FileName);
  OpenXML (FileName);

end;

procedure TGU.OpenDBF (FileName: String);
begin
  dbf.FilePathFull := ExtractFilePath (FileName);
  dbf.TableName := ExtractFileName (FileName);
  dbf.Active := true;
  dbf.First;
end;

procedure TGU.OpenXML (FileName: String);
begin
  AssignFile(XmlFile, StringReplace (FileName, 'DBF', 'XML', []));
  Rewrite (XmlFile);
  Write (XmlFile, '<?xml version="1.0" encoding="cp');
  Write (XmlFile, dbf.CodePage);
  Writeln (XmlFile, '" standalone="yes"?>');
  Writeln (XmlFile, '<GU PERIOD="' + YYYY + '-' + MM + '" version="' + Version + '">');
end;

procedure TGU.CloseFiles ();
begin
  CloseDBF ();
  CloseXML ();
end;

procedure TGU.CloseDBF ();
begin
  dbf.Active := false;
end;

procedure TGU.CloseXML ();
begin
  Writeln (XmlFile, '</GU>');
  CloseFile (XmlFile);
end;

procedure TGU.ProcessFile (FileName: String);
begin
  OpenFiles (FileName);
  while (not dbf.Eof) do ProcessRecord ();
  CloseFiles ();
end;

procedure TGU.WriteAttrValue (v: String);
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

procedure TGU.WriteStringAttr (FieldName: String);
begin
  WriteStringAttr2(FieldName, FieldName);
end;

procedure TGU.WriteDateAttr (FieldName: String);
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

procedure TGU.WriteDecimalAttr (FieldName: String);
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

procedure TGU.WriteStringAttr2 (FieldName: String; AttrName: String);
var
  v: string;
begin
  if dbf.FieldByName(FieldName).IsNull then exit;
  v := dbf.FieldByName(FieldName).AsString.Trim;
  if v.Length = 0 then Exit;
  if (AttrName = 'N_LIVE') and (v.Contains('/')) then Exit;
  Write (XmlFile, ' ');
  Write (XmlFile, AttrName);
  Write (XmlFile, '="');
  WriteAttrValue (v);
  Write (XmlFile, '"');
end;

procedure TGU.OpenElementPIN ();
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
  Write (XmlFile, '>');
end;

procedure TGU.CloseElementPIN ();
begin
  Writeln (XmlFile, '</PIN>');
end;

procedure TGU.WriteElementRSO_ACC (i: integer);
var
  pre: string;
begin
  pre := 'RSO' + IntToStr (i);
  if dbf.FieldByName(pre + '_ACC').IsNull then exit;
  Writeln (XmlFile);
  Write (XmlFile, '  <RSO_ACC');
  WriteStringAttr2 (pre + '_ACC', 'id');
  WriteStringAttr (pre + '_CODE');
  WriteStringAttr (pre + '_NAME');
  Write (XmlFile, '/>');
end;

procedure TGU.WriteElementREP_ACC ();
begin
  if dbf.FieldByName('REP_ACC').IsNull then exit;
  Writeln (XmlFile);
  Write (XmlFile, '  <REP_ACC');
  WriteStringAttr2 ('REP_ACC', 'id');
  WriteStringAttr ('REP_CODE');
  WriteStringAttr ('REP_NAME');
  Write (XmlFile, '/>');
end;

procedure TGU.WriteElementMC_ACC (i: integer; f2a: TDictionary <string, TStrings>);
begin
  Writeln (XmlFile);
  Write (XmlFile, '  <MC_ACC id="');
  WriteAttrValue (f2a ['MC_ACC'] [i]);
  Write (XmlFile, '" N_MC="');
  WriteAttrValue (f2a ['N_MC'] [i]);
  Write (XmlFile, '" SQ_PAY="');
  WriteAttrValue (f2a ['SQ_PAY'] [i].Replace (',', '.'));
  Write (XmlFile, '" OWN_TYPE="');
  WriteAttrValue (f2a ['OWN_TYPE'] [i]);
  Write (XmlFile, '" MC_CODE="');
  WriteAttrValue (f2a ['MC_CODE'] [i]);
  Write (XmlFile, '"/>');
end;


procedure TGU.ProcessRecord ();
var
  i, n: integer;
  f2a: TDictionary <string, TStrings>;
  code: string;

  procedure split (FieldName: String);
  var
    list: TStrings;
  begin
    list := TStringList.Create;
    list.CommaText := dbf.FieldByName(FieldName).AsString;
    f2a.Add (FieldName, list);
  end;

  function Verify (): string;
  begin
    exit ('00');
  end;

begin

  f2a := TDictionary <string, TStrings>.Create ();
  split ('MC_ACC');
  split ('N_MC');
  split ('SQ_PAY');
  split ('MC_CODE');
  split ('OWN_TYPE');

  code := Verify;

  n := f2a ['MC_ACC'].Count;

  if
    (n = f2a ['N_MC'].Count) and
    (n = f2a ['SQ_PAY'].Count) and
    (n = f2a ['MC_CODE'].Count) and
    (n = f2a ['OWN_TYPE'].Count)
  then begin
    OpenElementPIN ();
    for I := 0 to n - 1 do WriteElementMC_ACC (i, f2a);
    for I := 1 to 5 do WriteElementRSO_ACC (i);
    WriteElementREP_ACC ();
    CloseElementPIN ();
  end;

  dbf.Edit;
  dbf.FieldByName('RES_CODE').Value := code;
  dbf.Post;
  dbf.Next;

end;

end.