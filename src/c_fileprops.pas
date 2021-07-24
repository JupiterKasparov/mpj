unit c_fileprops;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, crDefs, fgl,
    Graphics, c_settings;

type

    { TFrmFile }

    TFrmFile = class(TFrame)
        PnText: TPanel;
    private
        Fselected, Fisdir: boolean;
        Ffname: string;
        Fselcolor: TColor;
        procedure SetSelected(AValue: boolean);
        procedure SetSelColor(AValue: TColor);
        procedure SetFileName(fname: string);
    public
        constructor Create(AOwner: TComponent; isdir: boolean; fname: string);
        property FileName: string read Ffname write SetFileName;
        property IsSelected: boolean read Fselected write SetSelected;
        property SelectedColor: TColor read Fselcolor write SetSelColor;
    end;

    TFileList = specialize TFPGList<TFrmFile>;

implementation

{$R *.lfm}

procedure TFrmFile.SetSelected(AValue: boolean);
begin
    Fselected := AValue;
    ParentColor := not Fselected;
    if Fselected then
        Color := Fselcolor;
end;

constructor TFrmFile.Create(AOwner: TComponent; isdir: boolean; fname: string);
begin
    inherited Create(AOwner);
    Fselected := False;
    Fisdir := isdir;
    Fselcolor := RGBToColor(ProgramSettings.Colors[10].R, ProgramSettings.Colors[10].G, ProgramSettings.Colors[10].B);
    SetFileName(fname);
end;

procedure TFrmFile.SetSelColor(AValue: TColor);
begin
    Fselcolor := AValue;
    SetSelected(Fselected);
end;

procedure TFrmFile.SetFileName(fname: string);
begin
    Ffname := fname;
    if Fisdir then
        PnText.Caption := Format('[%s]', [Ffname])
    else
      PnText.Caption := Ffname;
end;

end.

