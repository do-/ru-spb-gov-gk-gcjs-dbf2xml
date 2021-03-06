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
    YYYY_MM: String;
    YYYYMM: String;
    BNF_CATs: TStringList;
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
  BNF_CATs := TStringList.Create;
  BNF_CATs.DelimitedText := '?1,?2,?3,03,04,71,61,07,81,17,18,19,08,?,?,3,?,?,?,?,?,?,?';
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
  if name.Length <> 13 then raise Exception.Create ('???????????? ??? ????? (????????? ?????: 13 ????????)');
  if not name.StartsWith ('GU_') then raise Exception.Create ('??? ????? ?????? ?????????? ? "GU_"');
  if not name.EndsWith ('.DBF', true) then raise Exception.Create ('??? ????? ?????? ???????????? ?? ".DBF"');

  YYYY := name.Substring (3, 4);
  if not IsDigitsOnly (YYYY) then raise Exception.Create ('???????????? ????? ????: ' + YYYY);

  MM := name.Substring (7, 2);
  if not IsDigitsOnly (MM) then raise Exception.Create ('???????????? ????? ??????: ' + MM);

  YYYYMM := YYYY + MM;
  YYYY_MM := YYYY + '-' + MM;

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
  AssignFile(XmlFile, StringReplace (FileName, 'DBF', 'XML', []), CP_UTF8);
  Rewrite (XmlFile);
  Write (XmlFile, '<?xml version="1.0" encoding="utf-8" standalone="yes"?>');
  Writeln (XmlFile, '<GU PERIOD="' + YYYY_MM + '" version="' + Version + '">');
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
  WriteStringAttr2 (pre + '_CODE', 'RSO_CODE');
  WriteStringAttr2 (pre + '_NAME', 'RSO_NAME');
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

  if f2a ['MC_ACC'].Count = f2a ['N_MC'].Count then begin
    Write (XmlFile, '" N_MC="');
    WriteAttrValue (f2a ['N_MC'] [i]);
  end;

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
  n: integer;
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

  function Verify01 (): Boolean;
  begin
    if dbf.FieldByName ('PIN').IsNull then exit (false);
    if dbf.FieldByName ('L_NAME').IsNull then exit (false);
    if dbf.FieldByName ('F_NAME').IsNull then exit (false);
    if dbf.FieldByName ('BNF_CAT').IsNull then exit (false);
    if dbf.FieldByName ('INFO_DATE').IsNull then exit (false);
    if dbf.FieldByName ('BEGIN_DATE').IsNull then exit (false);
    if dbf.FieldByName ('N').IsNull then exit (false);
    if dbf.FieldByName ('DISTR_ID').IsNull then exit (false);
    if dbf.FieldByName ('BUILDING').IsNull then exit (false);
    if dbf.FieldByName ('LS_TYPE').IsNull then exit (false);
    if dbf.FieldByName ('N_LIVE').IsNull then exit (false);
    if dbf.FieldByName ('SQ_PAY').IsNull then exit (false);
    if dbf.FieldByName ('OWN_TYPE').IsNull then exit (false);
    if dbf.FieldByName ('MC_ACC').IsNull then exit (false);
    if dbf.FieldByName ('CHARGE_SUM').IsNull then exit (false);
    if dbf.FieldByName ('PRIORITET').IsNull then exit (false);
    exit (true);
  end;

  function Verify07 (): Boolean;
  begin
    if dbf.FieldByName ('END_DATE').IsNull then exit (true);
    if dbf.FieldByName ('BEGIN_DATE').AsDateTime > dbf.FieldByName ('END_DATE').AsDateTime then exit (false);
    exit (true);
  end;

  function Verify23 (): Boolean;
  begin
    if not dbf.FieldByName ('MC_ACC').IsNull then exit (true);
    exit (not dbf.FieldByName ('LS_TYPE').AsInteger in [2, 3]);
  end;

  function Verify32 (): Boolean;
  begin
    exit (dbf.FieldByName ('LS_TYPE').AsInteger in [1, 2, 3, 4]);
  end;

  function Verify35 (): Boolean;
  var
    i: integer;
  begin
    if dbf.FieldByName ('LS_TYPE').AsInteger <> 3 then exit (true);
    for I := 0 to n - 1 do if f2a ['OWN_TYPE'] [i] = '2' then exit (false);
    exit (true);
  end;

  function Verify11 (): Boolean;
  var
    s: string;
    i: integer;
  begin
    for I := 0 to n - 1 do begin
      s := f2a ['OWN_TYPE'] [i];
      if (s <> '1') and (s <> '2') then exit (false);
    end;
    exit (true);
  end;

  function Verify15 (): Boolean;
  begin
    exit (
      DateToISO8601 (dbf.FieldByName('BEGIN_DATE').AsDateTime)
        .Substring (0, 7)
      <= YYYY_MM
    );
  end;

  function Verify25 (): Boolean;
  begin
    exit (dbf.FieldByName ('PERIOD').AsString = YYYYMM);
  end;

  function Verify09 (): Boolean;
  begin
    exit (BNF_CATs.IndexOf(dbf.FieldByName('BNF_CAT').AsString) >= 0);
  end;

  function Verify21 (): Boolean;
  begin
    if n <> f2a ['SQ_PAY'].Count then exit (false);
    if n <> f2a ['MC_CODE'].Count then exit (false);
    if n <> f2a ['OWN_TYPE'].Count then exit (false);
    exit (true);
  end;

  function Verify (): string;
  var
    n_mc_count:   integer;
    n_all_null:   boolean;
    n_mc_null:    boolean;
    n_mc_differs: boolean;

    pin: string;

  begin

    if not verify01 then exit ('01');

    n_all_null   := dbf.FieldByName ('N_ALL').IsNull;
    n_mc_count   := f2a ['N_MC'].Count;
    n_mc_null    := n_mc_count = 0;
    n_mc_differs := not n_mc_null and (n_mc_count <> n);

    pin := dbf.FieldByName ('PIN').AsString;

    if n_all_null and n_mc_null    then exit ('01');
    if n_all_null and n_mc_differs then exit ('21');

    if not verify09 then exit ('09');
    if not verify32 then exit ('32'); // yes, here
    if not verify35 then exit ('35');
    if not verify23 then exit ('23');
    if not verify07 then exit ('07');
    if not verify15 then exit ('15');
    if not verify21 then exit ('21');
    if not verify11 then exit ('11');
    if not verify25 then exit ('25');

    if n_mc_differs then exit ('000');

    exit ('00');

  end;

  procedure WriteRecord ();
  var
    i: integer;
  begin
    OpenElementPIN ();
    for I := 0 to n - 1 do WriteElementMC_ACC (i, f2a);
    for I := 1 to 5 do WriteElementRSO_ACC (i);
    WriteElementREP_ACC ();
    CloseElementPIN ();
  end;

begin

  f2a := TDictionary <string, TStrings>.Create ();
  split ('MC_ACC');
  split ('N_MC');
  split ('SQ_PAY');
  split ('MC_CODE');
  split ('OWN_TYPE');
  n := f2a ['MC_ACC'].Count;

  code := Verify;

  dbf.Edit;
  dbf.FieldByName ('RES_CODE').Value := code;
  dbf.Post;

  if (code = '00') or (code = '000') then WriteRecord ();

  dbf.Next;

end;

end.
