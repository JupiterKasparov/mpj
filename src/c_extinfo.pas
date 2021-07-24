unit c_extinfo;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls,
    Graphics, Types, crDefs, c_vertsb, c_song, Windows;

type

    { TFrmExtInfo }

    TFrmExtInfo = class(TFrame)
        BtnOK: TPanel;
        BtnToggleMode: TPanel;
        ScbExtInfo: TFrmVertSb;
        LbExtInfo: TLabel;
        PnExtInfo: TPanel;
        PnMain: TPanel;
        PnTitle: TPanel;
        procedure goClose(Sender: TObject);
        procedure goPaintContent(Sender: TObject);
        procedure goToggleMode(Sender: TObject);
    private
        Fmode: boolean;
        Fpic: TID3PictureList;
        procedure scScrollText(Sender: TObject);
        procedure ChangeMode;
    public
        constructor Create(AOwner: TComponent);
        destructor Destroy; override;
        procedure ShowDialog(Title, Message: string; Pictures: TID3PictureList);
        procedure Accept;
        procedure Show;
    end;

implementation

{$R *.lfm}

{ TFrmExtInfo }

procedure TFrmExtInfo.goClose(Sender: TObject);
begin
    Hide;
end;

procedure TFrmExtInfo.goPaintContent(Sender: TObject);
var
    w, l, t, st, i, pw, ph: integer;
    r: TRect;
begin
    // FIX: Remove leftover artifacts
    PnExtInfo.Canvas.Brush.Style := bsSolid;
    PnExtInfo.Canvas.Brush.Color := PnExtInfo.Color;
    PnExtInfo.Canvas.FillRect(0, 0, PnExtInfo.Width, PnExtInfo.Height);

    // Draw pictures, if any
    if Fmode and Assigned(Fpic) and (Fpic.Count > 0) then
      begin
        if ScbExtInfo.Visible then
            w := PnExtInfo.Width - ScbExtInfo.Width
        else
            w := PnExtInfo.Width;
        l := 0;
        t := 0;
        pw := 0;
        ph := 0;
        for i := 0 to Fpic.Count - 1 do
          begin
            if (Fpic[i].Width > pw) then
                pw := min(w - 16, Fpic[i].Width);
            if (Fpic[i].Height > ph) then
                ph := min(PnExtInfo.Height - 16, Fpic[i].Height);
          end;
        if ScbExtInfo.Visible then
            st := -round(ScbExtInfo.Percentage * ScbExtInfo.Max * (ph + 16))
        else
            st := 0;
        for i := 0 to Fpic.Count - 1 do
          begin
            r.Left := l + 8;
            r.Top := t + st + 8;
            r.Right := r.Left + min(pw, Fpic[i].Width);
            r.Bottom := r.Top + min(ph, Fpic[i].Height);
            PnExtInfo.Canvas.StretchDraw(r, Fpic[i].Bitmap);
            Inc(l, pw + 16); // 1x image + padding
            if ((l + pw + 16) >= w) then
              begin
                Inc(t, ph + 16); //1x image row + padding
                l := 0;
              end;
          end;
      end;

    // App-level paint rule (draw borders)
    crDefs.g_EventSvc.g_PaintElement(PnExtInfo);
end;

procedure TFrmExtInfo.goToggleMode(Sender: TObject);
begin
    Fmode := not Fmode;
    if (not Assigned(Fpic)) or (Fpic.Count = 0) then
        Fmode := False;
    ChangeMode;
end;

procedure TFrmExtInfo.scScrollText(Sender: TObject);
begin
    if Fmode and Assigned(Fpic) and (Fpic.Count > 0) then
        PnExtInfo.Repaint
    else
        LbExtInfo.Top := -round(4 * ScbExtInfo.Percentage * ScbExtInfo.Max) + 8;
end;

procedure TFrmExtInfo.ChangeMode;
var
    r: TRect;
    w, l, t, i, pw, ph: integer;
begin
    // Reset scrollbar
    ScbExtInfo.Visible := False;
    ScbExtInfo.Percentage := 0.0;
    ScbExtInfo.Max := 0;

    // Reset ID3 text
    LbExtInfo.Visible := False;
    LbExtInfo.Left := 8;
    LbExtInfo.Top := 8;
    LbExtInfo.Width := PnExtInfo.Width - 16;

    // Mode: Pictures
    if Fmode and Assigned(Fpic) and (Fpic.Count > 0) then
      begin
        w := PnExtInfo.Width - ScbExtInfo.Width;
        l := 0;
        t := 0;
        pw := 0;
        ph := 0;
        for i := 0 to Fpic.Count - 1 do
          begin
            if (Fpic[i].Width > pw) then
                pw := min(w - 16, Fpic[i].Width);
            if (Fpic[i].Height > ph) then
                ph := min(PnExtInfo.Height - 16, Fpic[i].Height);
          end;
        for i := 1 to Fpic.Count - 1 do
          begin
            Inc(l, pw + 16); // 1x image + padding
            if ((l + pw + 16) >= w) then
              begin
                Inc(t, ph + 16); //1x image row + padding
                if ((t + ph + 16) >= PnExtInfo.Height) then
                  begin
                    ScbExtInfo.Max := ScbExtInfo.Max + 1;
                    if not ScbExtInfo.Visible then
                        ScbExtInfo.Visible := True;
                  end;
                l := 0;
              end;
          end;
      end

    // Mode: ID3
    else
      begin
        r.Width := LbExtInfo.Width;
        r.Right := LbExtInfo.Width;
        r.Top := 0;
        r.Left := 0;
        LbExtInfo.Height := DrawText(LbExtInfo.Canvas.Handle, PChar(LbExtInfo.Caption), Length(LbExtInfo.Caption), r, DT_WORDBREAK or DT_LEFT or DT_CALCRECT);
        ScbExtInfo.Visible := (LbExtInfo.Height + 16) > PnExtInfo.Height;
        if ScbExtInfo.Visible then
          begin
            LbExtInfo.Width := PnExtInfo.Width - 16 - ScbExtInfo.Width;
            r.Width := LbExtInfo.Width;
            r.Right := LbExtInfo.Width;
            LbExtInfo.Height := DrawText(LbExtInfo.Canvas.Handle, PChar(LbExtInfo.Caption), Length(LbExtInfo.Caption), r, DT_WORDBREAK or DT_LEFT or DT_CALCRECT);
            ScbExtInfo.Max := (LbExtInfo.Height div 4) + 4 - (PnExtInfo.Height div 4);
          end;
        LbExtInfo.Visible := True;
      end;

    // HACK: Force repaint, to solve numerous problems
    PnExtInfo.Repaint;
end;

constructor TFrmExtInfo.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    Fmode := False;
    Fpic := nil;
    ScbExtInfo.OnScroll := @scScrollText;
    LbExtInfo.OnMouseWheel := @ScbExtInfo.goScroll;
    PnExtInfo.OnMouseWheel := @ScbExtInfo.goScroll;
end;

destructor TFrmExtInfo.Destroy;
begin
    if Assigned(Fpic) then
      begin
        while (Fpic.Count > 0) do
          begin
            Fpic.Last.Free;
            Fpic.Remove(Fpic.Last);
          end;
        Fpic.Free;
      end;
    inherited;
end;

procedure TFrmExtInfo.ShowDialog(Title, Message: string; Pictures: TID3PictureList);
begin
    Show;
    PnTitle.Caption := Title;
    LbExtInfo.Caption := Message;
    BtnToggleMode.Visible := Assigned(Pictures) and (Pictures.Count > 0);
    if BtnToggleMode.Visible then
        Fpic := Pictures
    else
        Fpic := nil;
    Fmode := False;
    ChangeMode;
end;

procedure TFrmExtInfo.Accept;
begin
    goClose(BtnOK);
end;

procedure TFrmExtInfo.Show;
begin
    if Assigned(Fpic) then
      begin
        while (Fpic.Count > 0) do
          begin
            Fpic.Last.Free;
            Fpic.Remove(Fpic.Last);
          end;
        FreeAndNil(Fpic);
      end;
    Fmode := False;
    ChangeMode;
    BtnToggleMode.Visible := False;
    ScbExtInfo.Visible := False;
    ScbExtInfo.ScDrag.Top := 0;
    PnTitle.Caption := '';
    LbExtInfo.Caption := '';
    LbExtInfo.Left := 0;
    LbExtInfo.Top := 0;
    LbExtInfo.Width := 0;
    inherited;
end;

end.

