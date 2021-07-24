unit c_speed;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls,
    Graphics, crDefs, c_song, c_msgbox;

type

    { TFrmSetSpeed }

    TFrmSetSpeed = class(TFrame)
        BtnCancel: TPanel;
        BtnOK: TPanel;
        FltNewSpeed: TEdit;
        LbNewSpeed: TLabel;
        PnMain: TPanel;
        PnNewSpeed: TPanel;
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

{ TFrmSetSpeed }

procedure TFrmSetSpeed.goClose(Sender: TObject);
begin
    Hide;
end;


procedure TFrmSetSpeed.goAccept(Sender: TObject);

    procedure InternalShowMsgBox(msg: string);
    begin
        if Assigned(Fmessagebox) then
            Fmessagebox.ShowDialog('Wrong value', msg);
    end;

var
    s: real;
begin
    if Assigned(Fsong) then
      begin
        DefaultFormatSettings.DecimalSeparator := '.';
          try
            s := StrToFloat(FltNewSpeed.Text);
            if (s < 0.1) or (s > Fsong.MaxSpeed) then
                InternalShowMsgBox(Format('Use values between 0.1 and %.2f!', [Fsong.MaxSpeed]))
            else
              begin
                Fsong.Speed := s;
                Hide;
              end;
          except
            on EConvertError do
                InternalShowMsgBox('Use only integer or floating-point numbers!');
          end;
      end;
end;

constructor TFrmSetSpeed.Create(AOwner: TComponent; MsgBox: TFrmMsgBox);
begin
    inherited Create(AOwner);
    Fmessagebox := MsgBox;
    Fsong := nil;
end;

procedure TFrmSetSpeed.ShowDialog(song: TFrmSong);
var
    h, m, s: integer;
begin
    Show;
    Fsong := song;
    if Assigned(Fsong) then
      begin
        DefaultFormatSettings.DecimalSeparator := '.';
        LbNewSpeed.Caption := Format('Speed (0.1 - %.2f):', [Fsong.MaxSpeed]);
        FltNewSpeed.Text := FloatToStr(Fsong.Speed);
      end;
end;

procedure TFrmSetSpeed.Accept;
begin
    goAccept(BtnOK);
end;

procedure TFrmSetSpeed.Show;
begin
    Fsong := nil;
    LbNewSpeed.Caption := '??';
    FltNewSpeed.Text := '??';
    inherited;
end;

end.

