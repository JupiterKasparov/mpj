unit c_vertsb;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls,
  Graphics, crDefs, Types;

type

  { TFrmVertSb }

  TFrmVertSb = class(TFrame)
    BtnUp: TPanel;
    BtnDown: TPanel;
    ScDrag: TPanel;
    PnSlider: TPanel;
    procedure goDrag(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure goScroll(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure goEndDrag(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure goScrollUp(Sender: TObject);
    procedure goScrollDown(Sender: TObject);
    procedure goStartDrag(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    Fonscroll: TNotifyEvent;
    Fdragging: boolean;
    Fdragy: integer;
    function GetPercentage: double;
    procedure SetPercentage(pctg: double);
    function GetMax: integer;
    procedure SetMax(maxvalue: integer);
    procedure Change;
  public
    constructor Create(AOwner: TComponent);
    procedure ScrollUp;
    procedure ScrollDown;
    property OnScroll: TNotifyEvent read Fonscroll write Fonscroll;
    property Percentage: double read GetPercentage write SetPercentage;
    property Max: integer read GetMax write SetMax;
  end;

implementation

{$R *.lfm}

{ TFrmVertSb }

procedure TFrmVertSb.goScroll(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  Handled := true;
  if (WheelDelta < 0) then
     ScrollDown
  else if (WheelDelta > 0) then
     ScrollUp;
end;

procedure TFrmVertSb.goDrag(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  maxtop, t: integer; // Use var 'l', to reduce the number of updating of 'Top' property
begin
  if Fdragging then
     begin
       t := ScDrag.Top + (Y - Fdragy);
       maxtop := GetMax;
       if (t < 0) then
          t := 0
       else if (t > maxtop) then
          t := maxtop;
       if (t <> ScDrag.Top) then
          begin
            ScDrag.Top := t;
            Change;
          end;
     end;
end;

procedure TFrmVertSb.goEndDrag(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Fdragging then
     begin
       ScDrag.Cursor := crGrab;
       Fdragging := false;
     end;
end;

procedure TFrmVertSb.goScrollUp(Sender: TObject);
begin
  ScrollUp;
end;

procedure TFrmVertSb.goScrollDown(Sender: TObject);
begin
  ScrollDown;
end;

procedure TFrmVertSb.goStartDrag(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) then
     begin
       ScDrag.Cursor := crGrabbed;
       Fdragging := true;
       Fdragy := Y;
     end;
end;

function TFrmVertSb.GetPercentage: double;
var
  maxtop: integer;
begin
  maxtop := GetMax;
  if (maxtop <= 0) or (ScDrag.Top <= 0) then
     Result := 0.0
  else
    Result := ScDrag.Top / maxtop;
end;

procedure TFrmVertSb.SetPercentage(pctg: double);
var
  maxtop, t: integer; // Use var 'l', to reduce the number of updating of 'Top' property
begin
  maxtop := GetMax;
  if (pctg <= 0.0) then
     t := 0
  else if (pctg > 1.0) or (round(pctg * maxtop) > maxtop) then
     t := maxtop
  else
    t := round(pctg * maxtop);
  if (t <> ScDrag.Top) then
     begin
       ScDrag.Top := t;
       Change;
     end;
end;

function TFrmVertSb.GetMax: integer;
begin
  Result := PnSlider.Height - ScDrag.Height;
end;

procedure TFrmVertSb.SetMax(maxvalue: integer);
var
  h: integer; // Use var, to reduce the number of updating of 'Height' property
begin
  h := PnSlider.Height - maxvalue;
  if (h < 0) then
     h := 0
  else if (h > PnSlider.Height) then
     h := PnSlider.Height;
  if (h <> ScDrag.Height) then
     begin
       ScDrag.Height := h;
       Change;
     end;
end;

procedure TFrmVertSb.Change;
begin
  if Assigned(Fonscroll) then
     Fonscroll(self);
end;

constructor TFrmVertSb.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Fonscroll := nil;
end;

procedure TFrmVertSb.ScrollUp;
var
  t: integer;
begin
  t := ScDrag.Top - 1;
  if (t < 0) then
     t := 0;
  if (t <> ScDrag.Top) then
     begin
       ScDrag.Top := t;
       Change;
     end;
end;

procedure TFrmVertSb.ScrollDown;
var
  maxtop, t: integer;
begin
  maxtop := GetMax;
  t := ScDrag.Top + 1;
  if (t > maxtop) then
     t := maxtop;
  if (t <> ScDrag.Top) then
     begin
       ScDrag.Top := t;
       Change;
     end;
end;

end.

