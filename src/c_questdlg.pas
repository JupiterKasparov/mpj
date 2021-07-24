unit c_questdlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls,
  crDefs;

type

  { TFrmQuestionDlg }

  TFrmQuestionDlg = class(TFrame)
    BtnYes: TPanel;
    BtnNo: TPanel;
    LbMessage: TLabel;
    PnContent: TPanel;
    PnTitle: TPanel;
    procedure goCloseNo(Sender: TObject);
    procedure goCloseYes(Sender: TObject);
  private
    FResult: integer;
  public
    procedure ShowDialog(Title, Message: string);
    procedure Accept;
    procedure Show;
    procedure Hide;
    property DialogResult: integer read FResult;
  end;

implementation

{$R *.lfm}

{ TFrmQuestionDlg }

procedure TFrmQuestionDlg.goCloseYes(Sender: TObject);
begin
  Hide;
  FResult := 1;
end;

procedure TFrmQuestionDlg.goCloseNo(Sender: TObject);
begin
  Hide;
end;

procedure TFrmQuestionDlg.ShowDialog(Title, Message: string);
begin
  Show;
  FResult := -1;
  PnTitle.Caption := Title;
  LbMessage.Caption := Message;
end;

procedure TFrmQuestionDlg.Accept;
begin
  goCloseYes(BtnYes);
end;

procedure TFrmQuestionDlg.Show;
begin
  FResult := -1;
  PnTitle.Caption := '';
  LbMessage.Caption := '';
  inherited;
end;

procedure TFrmQuestionDlg.Hide;
begin
  inherited;
  FResult := 0;
end;

end.

