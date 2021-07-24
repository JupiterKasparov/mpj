unit c_msgbox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls,
  Graphics, crDefs;

type

  { TFrmMsgBox }

  TFrmMsgBox = class(TFrame)
    BtnOK: TPanel;
    LbMessage: TLabel;
    PnContent: TPanel;
    PnTitle: TPanel;
    procedure goClose(Sender: TObject);
  private
    { private declarations }
  public
    procedure ShowDialog(Title, Message: string);
    procedure Show;
  end;

implementation

{$R *.lfm}

{ TFrmMsgBox }

procedure TFrmMsgBox.goClose(Sender: TObject);
begin
  Hide;
end;

procedure TFrmMsgBox.ShowDialog(Title, Message: string);
begin
  Show;
  PnTitle.Caption := Title;
  LbMessage.Caption := Message;
end;

procedure TFrmMsgBox.Show;
begin
  PnTitle.Caption := '';
  LbMessage.Caption := '';
  inherited;
end;

end.

