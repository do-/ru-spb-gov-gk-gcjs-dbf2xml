unit UnitDBF2XML;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.DateUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Data.DB, dbf, RegularExpressions, System.Generics.Collections, GU;

const
  Version = '1.0.0';

type
  TFormMain = class(TForm)
    Button: TButton;
    OpenDialog: TOpenDialog;
    ProgressBar: TProgressBar;
    Dbf: TDbf;
    procedure ButtonClick(Sender: TObject);
    procedure DbfAfterScroll(DataSet: TDataSet);
    procedure DbfAfterOpen(DataSet: TDataSet);
  private
    XmlFile: TextFile;
    YYYY: String;
    MM: String;
    procedure ProcessFile (FileName: String);
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.DbfAfterOpen(DataSet: TDataSet);
begin
  ProgressBar.Enabled := true;
  ProgressBar.Position := 0;
  ProgressBar.Max := DataSet.RecordCount;
end;

procedure TFormMain.DbfAfterScroll(DataSet: TDataSet);
begin
  ProgressBar.Position := dataset.RecNo;
end;

procedure TFormMain.ProcessFile (FileName: String);
var gu: TGU;
begin

  gu := TGU.Create (dbf);
  gu.ProcessFile (FileName);
  gu.Destroy;

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
