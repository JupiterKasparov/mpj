unit c_color;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls, c_horzsb,
    Graphics, Math, crDefs, Types;

type

    { TFrmSelectColorMPJ }

    TFrmSelectColorMPJ = class(TFrame)
        BtnCancel: TPanel;
        BtnOK: TPanel;
        ScbBlue: TFrmHorzSb;
        ScbGreen: TFrmHorzSb;
        ScbRed: TFrmHorzSb;
        IntBlue: TEdit;
        IntRed: TEdit;
        IntGreen: TEdit;
        LbColor: TLabel;
        LbGreen: TLabel;
        LbRed: TLabel;
        LbBlue: TLabel;
        PnGreen: TPanel;
        PnGreenValue: TPanel;
        PnRed: TPanel;
        PnBlue: TPanel;
        PnRedValue: TPanel;
        PnBlueValue: TPanel;
        VwColorRect: TPanel;
        VwColorSpace: TPanel;
        PnMain: TPanel;
        PnTitle: TPanel;
        procedure goChangeColorValues(Sender: TObject);
        procedure goCloseNo(Sender: TObject);
        procedure goCloseYes(Sender: TObject);
        procedure goDrawColorSpace(Sender: TObject);
        procedure goPaintColorRect(Sender: TObject);
        procedure goSelectColorFromColorSpace(Sender: TObject);
    private
        _dresult: boolean;
        procedure scScrollColorValue(Sender: TObject);
        function GetSelectedColor: TColor;
    public
        constructor Create(AOwner: TComponent);
        procedure ShowDialog(cl: TColor);
        procedure Accept;
        procedure Show;
        procedure Hide;
        property DialogResult: boolean read _dresult;
        property SelectedColor: TColor read GetSelectedColor;
    end;

implementation

function HSVtoRGB(H, S, V: real): TColor;
var
    f, p, q, t: real;
    i: integer;
    gr: byte;
begin
    if (S = 0.0) then
      begin
        // achromatic (grey)
        gr := round(V * 255);
        Result := RGBToColor(gr, gr, gr);
      end
    else
      begin
        H := H * 6.0; // sector 0 to 5
        i := floor(H);
        f := H - i; // fractional part of H
        p := V * (1.0 - S);
        q := V * (1.0 - S * f);
        t := V * (1.0 - S * (1.0 - f));
        case i of
            0: Result := RGBToColor(round(V * 255), round(t * 255), round(p * 255));
            1: Result := RGBToColor(round(q * 255), round(V * 255), round(p * 255));
            2: Result := RGBToColor(round(p * 255), round(V * 255), round(t * 255));
            3: Result := RGBToColor(round(p * 255), round(q * 255), round(V * 255));
            4: Result := RGBToColor(round(t * 255), round(p * 255), round(V * 255));
            else
                Result := RGBToColor(round(V * 255), round(p * 255), round(q * 255));
          end;
      end;
end;

{$R *.lfm}

{ TFrmSelectColorMPJ }

procedure TFrmSelectColorMPJ.goDrawColorSpace(Sender: TObject);
var
    h, s, v: real;
    i, j: integer;
begin
    // Draw color space
    for i := 0 to VwColorSpace.Width - 1 do
        for j := 0 to VwColorSpace.Height - 1 do
          begin
            h := i / VwColorSpace.Width;
            s := j / VwColorSpace.Height;
            v := min(1.0, ((VwColorSpace.Height - j) / VwColorSpace.Height) * 2.0);
            VwColorSpace.Canvas.Pixels[i, j] := HSVtoRGB(h, s, v);
          end;
end;

procedure TFrmSelectColorMPJ.goPaintColorRect(Sender: TObject);
begin
    VwColorRect.Canvas.Pen.Color := Font.Color;
    VwColorRect.Canvas.Brush.Color := VwColorRect.Color;
    VwColorRect.Canvas.Rectangle(0, 0, VwColorRect.Width, VwColorRect.Height);
end;

procedure TFrmSelectColorMPJ.goSelectColorFromColorSpace(Sender: TObject);
var
    c: TColor;
    p: TPoint;
begin
    p := VwColorSpace.ScreenToClient(Mouse.CursorPos);
    c := VwColorSpace.Canvas.Pixels[p.x, p.y];
    IntRed.Text := IntToStr(Red(c));
    IntGreen.Text := IntToStr(Green(c));
    IntBlue.Text := IntToStr(Blue(c));
end;

procedure TFrmSelectColorMPJ.goChangeColorValues(Sender: TObject);
var
    r, g, b: byte;
begin
      try
        r := min(255, max(0, StrToInt(IntRed.Text)));
        g := min(255, max(0, StrToInt(IntGreen.Text)));
        b := min(255, max(0, StrToInt(IntBlue.Text)));
        ScbRed.Percentage := r / 255;
        ScbGreen.Percentage := g / 255;
        ScbBlue.Percentage := b / 255;
        VwColorRect.Color := RGBToColor(r, g, b);
      except
        on EConvertError do
            ; // Just skip exception
      end;
end;

procedure TFrmSelectColorMPJ.goCloseNo(Sender: TObject);
begin
    Hide;
end;

procedure TFrmSelectColorMPJ.goCloseYes(Sender: TObject);
begin
    Hide;
    _dresult := True;
end;

procedure TFrmSelectColorMPJ.scScrollColorValue(Sender: TObject);
var
    rc, gc, bc: TNotifyEvent;
begin
    rc := IntRed.OnChange;
    IntRed.OnChange := nil;
    gc := IntGreen.OnChange;
    IntGreen.OnChange := nil;
    bc := IntBlue.OnChange;
    IntBlue.OnChange := nil;
      try
        IntRed.Text := IntToStr(round(ScbRed.Percentage * 255));
        IntGreen.Text := IntToStr(round(ScbGreen.Percentage * 255));
        IntBlue.Text := IntToStr(round(ScbBlue.Percentage * 255));
        goChangeColorValues(Sender);
      finally
        IntRed.OnChange := rc;
        IntGreen.OnChange := gc;
        IntBlue.OnChange := bc;
      end;
end;

function TFrmSelectColorMPJ.GetSelectedColor: TColor;
begin
    Result := VwColorRect.Color;
end;

constructor TFrmSelectColorMPJ.Create(AOwner: TComponent);
begin
    inherited;
    ScbRed.OnScroll := @scScrollColorValue;
    ScbGreen.OnScroll := @scScrollColorValue;
    ScbBlue.OnScroll := @scScrollColorValue;
    _dresult := False;
end;

procedure TFrmSelectColorMPJ.ShowDialog(cl: TColor);
begin
    Show;
    IntRed.Text := IntToStr(Red(cl));
    IntGreen.Text := IntToStr(Green(cl));
    IntBlue.Text := IntToStr(Blue(cl));
    goChangeColorValues(Self);
end;

procedure TFrmSelectColorMPJ.Accept;
begin
    Hide;
    _dresult := True;
end;

procedure TFrmSelectColorMPJ.Show;
begin
    _dresult := False;
    IntRed.Text := '0';
    IntGreen.Text := '0';
    IntBlue.Text := '0';
    goChangeColorValues(Self);
    inherited;
end;

procedure TFrmSelectColorMPJ.Hide;
begin
    _dresult := False;
    inherited;
end;

end.
