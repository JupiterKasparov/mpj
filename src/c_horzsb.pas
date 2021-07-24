unit c_horzsb;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls,
  Graphics, crDefs, Types;

type

  { TFrmHorzSb }

  TFrmHorzSb = class(TFrame)
    BtnLeft: TPanel;
    BtnRight: TPanel;
    ScDrag: TPanel;
    PnSlider: TPanel;
    procedure goDrag(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure goScroll(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure goEndDrag(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure goScrollLeft(Sender: TObject);
    procedure goScrollRight(Sender: TObject);
    procedure goStartDrag(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    Fonscroll: TNotifyEvent;
    Fdragging: boolean;
    Fdragx: integer;
    function GetPercentage: double;
    procedure SetPercentage(pctg: double);
    function GetMax: integer;
    procedure SetMax(maxvalue: integer);
    procedure Change;
  public
    constructor Create(AOwner: TComponent);
    procedure ScrollLeft;
    procedure ScrollRight;
    property OnScroll: TNotifyEvent read Fonscroll write Fonscroll;
    property Percentage: double read GetPercentage write SetPercentage;
    property Max: integer read GetMax write SetMax;
  end;

implementation

{$R *.lfm}

{ TFrmHorzSb }

procedure TFrmHorzSb.goScroll(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  Handled := true;
  if (WheelDelta > 0) then
     ScrollRight
  else if (WheelDelta < 0) then
     ScrollLeft;
end;

procedure TFrmHorzSb.goDrag(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  maxleft, l: integer; // Use var 'l', to reduce the number of updating of 'Left' property
begin
  if Fdragging then
     begin
       l := ScDrag.Left + (X - Fdragx);
       maxleft := GetMax;
       if (l < 0) then
          l := 0
       else if (l > maxleft) then
          l := maxleft;
       if (l <> ScDrag.Left) then
          begin
            ScDrag.Left := l;
            Change;
          end;
     end;
end;

procedure TFrmHorzSb.goEndDrag(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Fdragging then
     begin
       ScDrag.Cursor := crGrab;
       Fdragging := false;
     end;
end;

procedure TFrmHorzSb.goScrollLeft(Sender: TObject);
begin
  ScrollLeft;
end;

procedure TFrmHorzSb.goScrollRight(Sender: TObject);
begin
  ScrollRight;
end;

procedure TFrmHorzSb.goStartDrag(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) then
     begin
       ScDrag.Cursor := crGrabbed;
       Fdragging := true;
       Fdragx := X;
     end;
end;

function TFrmHorzSb.GetPercentage: double;
var
  maxleft: integer;
begin
  maxleft := GetMax;
  if (maxleft <= 0) or (ScDrag.Left <= 0) then
     Result := 0.0
  else
    Result := ScDrag.Left / maxleft;
end;

procedure TFrmHorzSb.SetPercentage(pctg: double);
var
  maxleft, l: integer; // Use var 'l', to reduce the number of updating of 'Left' property
begin
  maxleft := GetMax;
  if (pctg <= 0.0) then
     l := 0
  else if (pctg > 1.0) or (round(pctg * maxleft) > maxleft) then
     l := maxleft
  else
    l := round(pctg * maxleft);
  if (l <> ScDrag.Left) then
     begin
       ScDrag.Left := l;
       Change;
     end;
end;

function TFrmHorzSb.GetMax: integer;
begin
  Result := PnSlider.Width - ScDrag.Width;
end;

procedure TFrmHorzSb.SetMax(maxvalue: integer);
var
  w: integer; // Use var, to reduce the number of updating of 'Width' property
begin
  w := PnSlider.Width - maxvalue;
  if (w < 0) then
     w := 0
  else if (w > PnSlider.Width) then
     w := PnSlider.Width;
  if (w <> ScDrag.Width) then
     begin
       ScDrag.Width := w;
       Change;
     end;
end;

procedure TFrmHorzSb.Change;
begin
  if Assigned(Fonscroll) then
     Fonscroll(self);
end;

constructor TFrmHorzSb.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Fonscroll := nil;
end;

procedure TFrmHorzSb.ScrollLeft;
var
  l: integer;
begin
  l := ScDrag.Left - 1;
  if (l < 0) then
     l := 0;
  if (l <> ScDrag.Left) then
     begin
       ScDrag.Left := l;
       Change;
     end;
end;

procedure TFrmHorzSb.ScrollRight;
var
  maxleft, l: integer;
begin
  maxleft := GetMax;
  l := ScDrag.Left + 1;
  if (l > maxleft) then
     l := maxleft;
  if (l <> ScDrag.Left) then
     begin
       ScDrag.Left := l;
       Change;
     end;
end;

end.

