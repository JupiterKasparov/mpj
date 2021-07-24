unit crDefs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, Graphics, strutils, Controls;

{ "Common Runtime Definitions" to be used in ALL units of program}

const
  crGrab           = 1;
  crGrabbed        = 2;
  crScrollVert     = 3;
  crScrollHorz     = 4;
  crDefaultCursor  = 5;
  crSave           = 6;
  crOpen           = 7;

  type
    TGlobalEventServiceMPJ = class(TObject)
    private
      _defbtncolor, _defbtndiscolor, _defbtnxorcolor: TColor;
      constructor Create;
    public
      procedure g_PaintElement(Sender: TObject);
      procedure g_PressButton(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      procedure g_ReleaseButton(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      property ButtonColor: TColor read _defbtncolor write _defbtncolor;
      property ButtonDisabledColor: TColor read _defbtndiscolor write _defbtndiscolor;
      property ButtonXorColor: TColor read _defbtnxorcolor write _defbtnxorcolor;
    end;

var
  mpj_temp_dir: string;
  g_EventSvc: TGlobalEventServiceMPJ = nil;

implementation

constructor TGlobalEventServiceMPJ.Create;
begin
  inherited;
  _defbtncolor := clGreen;
  _defbtndiscolor := clMoneyGreen;
  _defbtnxorcolor := High(TColor);
end;

{
 Paint an element by HelpKeyword
 eg. LBRT = Left Right Top Bottom sides
}
procedure TGlobalEventServiceMPJ.g_PaintElement(Sender: TObject);
var
  w, h: integer;
begin
  with Sender as TPanel do
       begin
         if not Enabled then
            begin
              Canvas.Brush.Style := bsSolid;
              Canvas.Brush.Color := Color;
              Canvas.Font.Color := Font.Color;
              Canvas.Pen.Style := psClear;
              Canvas.Rectangle(0, 0, Width, Height);
              w := Canvas.TextWidth(Caption);
              h := Canvas.TextHeight(Caption);
              case Alignment of
                   taLeftJustify: Canvas.TextOut(0, (Height div 2) - (h div 2), Caption);
                   taCenter: Canvas.TextOut((Width div 2) - (w div 2), (Height div 2) - (h div 2), Caption);
                   taRightJustify: Canvas.TextOut(Width - (w div 2), (Height div 2) - (h div 2), Caption);
              end;
            end;
         Canvas.Brush.Style := bsClear;
         Canvas.Pen.Style := psSolid;
         Canvas.Pen.Color := Font.Color;
         if AnsiContainsStr(HelpKeyword, 'L') then
            Canvas.Line(0, 0, 0, Height);
         if AnsiContainsStr(HelpKeyword, 'B') then
            Canvas.Line(0, Height - 1, Width, Height - 1);
         if AnsiContainsStr(HelpKeyword, 'R') then
            Canvas.Line(Width - 1, Height, Width - 1, -1);
         if AnsiContainsStr(HelpKeyword, 'T') then
            Canvas.Line(0, 0, Width, 0);
       end;
end;

procedure TGlobalEventServiceMPJ.g_PressButton(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) then
     with Sender as TPanel do
          begin
            Color := _defbtncolor xor _defbtnxorcolor;
            Font.Color := Font.Color xor _defbtnxorcolor;
            Repaint;
          end;
end;

procedure TGlobalEventServiceMPJ.g_ReleaseButton(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) then
     with Sender as TPanel do
          begin
            Color := _defbtncolor;
            ParentFont := True;
            Repaint;
          end;
end;

initialization
  mpj_temp_dir := Format('%s\mpj_temp_dir', [GetTempDir]);
  g_EventSvc := TGlobalEventServiceMPJ.Create;

finalization
  g_EventSvc.Free;

end.

