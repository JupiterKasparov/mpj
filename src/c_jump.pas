unit c_jump;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls,
    Graphics, crDefs, c_song, c_msgbox;

type

    { TFrmJumpToPos }

    TFrmJumpToPos = class(TFrame)
        BtnCancel: TPanel;
        BtnOK: TPanel;
        IntHrs: TEdit;
        IntMins: TEdit;
        IntSecs: TEdit;
        LbHrs: TLabel;
        LbMins: TLabel;
        LbSecs: TLabel;
        PnHrs: TPanel;
        PnMain: TPanel;
        PnMins: TPanel;
        PnSecs: TPanel;
        PnTitle: TPanel;
        procedure goAccept(Sender: TObject);
        procedure goClose(Sender: TObject);
    private
        Fsong: TFrmSong;
        Fmessagebox: TFrmMsgBox;
    public
        constructor Create(AOwner: TComponent; MsgBox: TFrmMsgBox);
        procedure ShowDialog(song: TFrmSong);
        procedure Accept;
        procedure Show;
    end;

implementation

{$R *.lfm}

{ TFrmJumpToPos }

procedure TFrmJumpToPos.goClose(Sender: TObject);
begin
    Hide;
end;

procedure TFrmJumpToPos.goAccept(Sender: TObject);

    procedure InternalShowMsgBox(msg: string);
    begin
        if Assigned(Fmessagebox) then
            Fmessagebox.ShowDialog('Wrong values', msg);
    end;

var
    h, m, s, pos: integer;
begin
    if Assigned(Fsong) then
      begin
          try
            h := StrToInt(IntHrs.Text);
            m := StrToInt(IntMins.Text);
            s := StrToInt(IntSecs.Text);
            if (h < 0) or (m < 0) or (s < 0) then
                InternalShowMsgBox('Negatives are not allowed!')
            else if (m > 59) or (s > 59) then
                InternalShowMsgBox('Minutes and seconds must be between 0 and 59!')
            else
              begin
                pos := s + (m * 60) + (h * 3600);
                if (pos > Fsong.LengthSec) then
                    InternalShowMsgBox('This track is shorter, than the position entered!')
                else
                  begin
                    Fsong.PositionSec := pos;
                    Hide;
                  end;
              end;
          except
            on EConvertError do
                InternalShowMsgBox('Integer numbers expected!');
          end;
      end;
end;

constructor TFrmJumpToPos.Create(AOwner: TComponent; MsgBox: TFrmMsgBox);
begin
    inherited Create(AOwner);
    Fmessagebox := MsgBox;
    Fsong := nil;
end;

procedure TFrmJumpToPos.ShowDialog(song: TFrmSong);
var
    h, m, s: integer;
begin
    Show;
    Fsong := song;
    if Assigned(Fsong) then
      begin
        song.GetLength(h, m, s);
        PnHrs.Enabled := (h > 0);
        PnMins.Enabled := (m > 0);
        PnSecs.Enabled := (s > 0);
        Fsong.GetPosition(h, m, s);
        IntHrs.Text := IntToStr(h);
        IntMins.Text := IntToStr(m);
        IntSecs.Text := IntToStr(s);
      end;
end;

procedure TFrmJumpToPos.Accept;
begin
    goAccept(BtnOK);
end;

procedure TFrmJumpToPos.Show;
begin
    Fsong := nil;
    PnHrs.Enabled := False;
    PnMins.Enabled := False;
    PnSecs.Enabled := False;
    IntHrs.Text := '?';
    IntMins.Text := '?';
    IntSecs.Text := '?';
    inherited;
end;

end.
