unit c_settings;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls,
    Graphics, crDefs, c_color, c_msgbox;

type

  {
   There are many reserved filelds, these are reserved to be used future
   versions, but allow old settings files to be read by them
  }

    TColorRecMPJ = packed record
        R, G, B: byte;
    end;

    TProgramSettings = packed record
        HighlightShuffled: boolean;
        CopyMusicOnSavePlaylist: boolean;
        ShowLongNamesInSummary: boolean;
        PlaylistUseExtFmt: boolean;
        UseExtID3Info: boolean;
        AllowSkipTracks: boolean;
        bReserved: boolean;
        UseID3Title: boolean;
        PlaylistAddMode: integer;
        Colors: array [0..15] of TColorRecMPJ;
        Keys: array [0..7] of word;
        sReserved1: array [0..1996] of char;
    end;

    { TFrmSettings }

    TFrmSettings = class(TFrame)
        BdPause: TPanel;
        BdStart: TPanel;
        BdDel: TPanel;
        BdTrPrev: TPanel;
        BdTrNext: TPanel;
        BdStop: TPanel;
        BdVolUp: TPanel;
        BtnCancel: TPanel;
        BtnOK: TPanel;
        BtnPLAddModeSelect: TPanel;
        BtnResetDefs: TPanel;
        CbCopyPl: TPanel;
        CbExtInf: TPanel;
        CbExtPLS: TPanel;
        CbHighShuf: TPanel;
        CbID3T: TPanel;
        CbPLAddMode: TPanel;
        CbShowTrunc: TPanel;
        CbSkipEn: TPanel;
        CbtBgColor: TPanel;
        CbtButtonColor: TPanel;
        CbtButtonDisColor: TPanel;
        BdVolDn: TPanel;
        CbtDragColor: TPanel;
        CbtColorMainBg: TPanel;
        CbtColorSong: TPanel;
        CbtColorSongDrag: TPanel;
        CbtColorSongSel: TPanel;
        CbtColorSongShuf: TPanel;
        CbtCbColor: TPanel;
        CbtColorSongSkip: TPanel;
        CbtColorTitleBg: TPanel;
        CbtEditColor: TPanel;
        CbtFontColor: TPanel;
        CbtFontColorDis: TPanel;
        CbtMsgColor: TPanel;
        LbBdCancel: TLabel;
        LbBdPause: TLabel;
        LbBdStart: TLabel;
        LbBdDel: TLabel;
        LbBgColor: TLabel;
        LbBdVolUp: TLabel;
        LbBdTrPrev: TLabel;
        LbBdTrNext: TLabel;
        LbBdStop: TLabel;
        LbButtonColor: TLabel;
        LbButtonDisColor: TLabel;
        LbBdVolDn: TLabel;
        LbDragColor: TLabel;
        LbColorMainBg: TLabel;
        LbColorSong: TLabel;
        LbColorSongDrag: TLabel;
        LbColorSongSel: TLabel;
        LbColorSongShuf: TLabel;
        LbCbColor: TLabel;
        LbColorSongSkip: TLabel;
        LbColorTitleBg: TLabel;
        LbCopyPl: TLabel;
        LbEditColor: TLabel;
        LbExtInf: TLabel;
        LbExtPLS: TLabel;
        LbFontColor: TLabel;
        LbFontColorDis: TLabel;
        LbHighShuf: TLabel;
        LbID3T: TLabel;
        LbMsgColor: TLabel;
        LbPLAddMode: TLabel;
        LbShowTrunc: TLabel;
        LbSkipEn: TLabel;
        LstE_PLAddMode_AddToPL: TPanel;
        LstE_PLAddMode_ResetPL: TPanel;
        LstPLAddMode: TPanel;
        PnBdCancel: TPanel;
        PnBdPause: TPanel;
        PnBdStart: TPanel;
        PnBdDel: TPanel;
        PnBdVolDn: TPanel;
        PnBdTrPrev: TPanel;
        PnBdTrNext: TPanel;
        PnBdStop: TPanel;
        PnBdVolUp: TPanel;
        PnKeyBindings: TPanel;
        PnBgColor: TPanel;
        PnButtonColor: TPanel;
        PnButtonDisColor: TPanel;
        PnDragColor: TPanel;
        PnColorMainBg: TPanel;
        PnColorSettings: TPanel;
        PnColorSong: TPanel;
        PnColorSongDrag: TPanel;
        PnColorSongSel: TPanel;
        PnColorSongShuf: TPanel;
        PnCbColor: TPanel;
        PnColorSongSkip: TPanel;
        PnColorTitleBg: TPanel;
        PnEditColor: TPanel;
        PnFontColor: TPanel;
        PnFontColorDis: TPanel;
        PnMsgColor: TPanel;
        PnWorkSettings: TPanel;
        PnCopyPl: TPanel;
        PnExtInf: TPanel;
        PnExtPLS: TPanel;
        PnHighShuf: TPanel;
        PnID3T: TPanel;
        PnPLAddMode: TPanel;
        PnShowTrunc: TPanel;
        PnSkipEn: TPanel;
        PnMain: TPanel;
        PnTitle: TPanel;
        TmrSettingsKbWait: TTimer;
        TmrSettingsColorWait: TTimer;
        procedure goApply(Sender: TObject);
        procedure goBindKey(Sender: TObject);
        procedure goChangeBoolOption(Sender: TObject);
        procedure goChangeColor(Sender: TObject);
        procedure goClose(Sender: TObject);
        procedure goPaintColorButton(Sender: TObject);
        procedure goResetDefs(Sender: TObject);
        procedure goSetPLAddMode(Sender: TObject);
        procedure goShowPLAddModes(Sender: TObject);
        procedure goSimpleClick(Sender: TObject);
        procedure goWaitForColorDlg(Sender: TObject);
        procedure goWaitForKey(Sender: TObject);
    private
        _color_el, _kb_ctrl: TPanel;
        _kb_color: TColor;
        _keybind: word;
        Fcolordlg: TFrmSelectColorMPJ;
        Fmsgbox: TFrmMsgBox;
        Fisbinding: boolean;
        Fonaccept: TNotifyEvent;
        procedure ResetUI;
    public
        constructor Create(AOwner: TComponent; messagebox: TFrmMsgBox);
        procedure SendKey(k: word);
        procedure Show;
        procedure Hide;
        procedure Accept;
        property ColorDialog: TFrmSelectColorMPJ read Fcolordlg;
        property IsWaitingForKey: boolean read Fisbinding;
        property OnAccept: TNotifyEvent read Fonaccept write Fonaccept;
    end;

var
    ProgramSettings: TProgramSettings;

function GetPanelColor(p: TPanel): TColorRecMPJ;
procedure SetPanelColor(p: TPanel; rec: TColorRecMPJ);
function IsCheckedPanel(p: TPanel): boolean;
procedure SetCheckedPanel(p: TPanel; check: boolean);

implementation

uses
    LCLType;

type
    TKeyBindingMPJ = record
        VK: word;
        Name: string;
    end;

const
    SettingsFile: string = 'settings.mpj'; {BINARY file}
    UnkKeyName: string = 'NONE';
    AllowedKeys: array [0..73] of TKeyBindingMPJ =
        (
        (VK: VK_0; Name: '0'),
        (VK: VK_1; Name: '1'),
        (VK: VK_2; Name: '2'),
        (VK: VK_3; Name: '3'),
        (VK: VK_4; Name: '4'),
        (VK: VK_5; Name: '5'),
        (VK: VK_6; Name: '6'),
        (VK: VK_7; Name: '7'),
        (VK: VK_8; Name: '8'),
        (VK: VK_9; Name: '9'),
        (VK: VK_NUMPAD0; Name: 'Num 0'),
        (VK: VK_NUMPAD1; Name: 'Num 1'),
        (VK: VK_NUMPAD2; Name: 'Num 2'),
        (VK: VK_NUMPAD3; Name: 'Num 3'),
        (VK: VK_NUMPAD4; Name: 'Num 4'),
        (VK: VK_NUMPAD5; Name: 'Num 5'),
        (VK: VK_NUMPAD6; Name: 'Num 6'),
        (VK: VK_NUMPAD7; Name: 'Num 7'),
        (VK: VK_NUMPAD8; Name: 'Num 8'),
        (VK: VK_NUMPAD9; Name: 'Num 9'),
        (VK: VK_A; Name: 'A'),
        (VK: VK_B; Name: 'B'),
        (VK: VK_C; Name: 'C'),
        (VK: VK_D; Name: 'D'),
        (VK: VK_E; Name: 'E'),
        (VK: VK_F; Name: 'F'),
        (VK: VK_G; Name: 'G'),
        (VK: VK_H; Name: 'H'),
        (VK: VK_I; Name: 'I'),
        (VK: VK_J; Name: 'J'),
        (VK: VK_K; Name: 'K'),
        (VK: VK_L; Name: 'L'),
        (VK: VK_M; Name: 'M'),
        (VK: VK_N; Name: 'N'),
        (VK: VK_O; Name: 'O'),
        (VK: VK_P; Name: 'P'),
        (VK: VK_Q; Name: 'Q'),
        (VK: VK_R; Name: 'R'),
        (VK: VK_S; Name: 'S'),
        (VK: VK_T; Name: 'T'),
        (VK: VK_U; Name: 'U'),
        (VK: VK_V; Name: 'V'),
        (VK: VK_W; Name: 'W'),
        (VK: VK_X; Name: 'X'),
        (VK: VK_Y; Name: 'Y'),
        (VK: VK_Z; Name: 'Z'),
        (VK: VK_F1; Name: 'F1'),
        (VK: VK_F2; Name: 'F2'),
        (VK: VK_F3; Name: 'F3'),
        (VK: VK_F4; Name: 'F4'),
        (VK: VK_F5; Name: 'F5'),
        (VK: VK_F6; Name: 'F6'),
        (VK: VK_F7; Name: 'F7'),
        (VK: VK_F8; Name: 'F8'),
        (VK: VK_F9; Name: 'F9'),
        (VK: VK_F10; Name: 'F10'),
        (VK: VK_F11; Name: 'F11'),
        (VK: VK_F12; Name: 'F12'),
        (VK: VK_SPACE; Name: 'SPACE'),
        (VK: VK_RETURN; Name: 'ENTER'),
        (VK: VK_PAUSE; Name: 'BREAK'),
        (VK: VK_END; Name: 'END'),
        (VK: VK_HOME; Name: 'HOME'),
        (VK: VK_PRIOR; Name: 'PG UP'),
        (VK: VK_NEXT; Name: 'PG DN'),
        (VK: VK_MULTIPLY; Name: '*'),
        (VK: VK_DIVIDE; Name: '/'),
        (VK: VK_ADD; Name: '+'),
        (VK: VK_SUBTRACT; Name: '-'),
        (VK: VK_LEFT; Name: 'LEFT'),
        (VK: VK_RIGHT; Name: 'RIGHT'),
        (VK: VK_UP; Name: 'UP'),
        (VK: VK_DOWN; Name: 'DOWN'),
        (VK: VK_DELETE; Name: 'DEL')
        );

var
    DefaultSettings: TProgramSettings = (HighlightShuffled: False;
    CopyMusicOnSavePlaylist: False;
    ShowLongNamesInSummary: True;
    PlaylistUseExtFmt: True;
    UseExtID3Info: False;
    AllowSkipTracks: False;
    bReserved: False;
    UseID3Title: False;
    PlaylistAddMode: 0;
    Colors: ((R: 0; G: 0; B: 0),        //0: Main Form Bg
        (R: 128; G: 0; B: 0),      //1: Title
        (R: 0; G: 128; B: 128),    //2: Bg
        (R: 0; G: 128; B: 0),      //3: Btn
        (R: 192; G: 220; B: 192),  //4: Btn Dis
        (R: 255; G: 255; B: 255),  //5: Font
        (R: 128; G: 128; B: 128),  //6: Font Dis
        (R: 0; G: 0; B: 255),      //7: Edit box
        (R: 0; G: 0; B: 128),      //8: Msg box
        (R: 0; G: 128; B: 128),    //9: Song
        (R: 0; G: 255; B: 255),    //10: Song Cur
        (R: 0; G: 255; B: 0),      //11: Song Moved
        (R: 64; G: 128; B: 192),   //12: Song Skip
        (R: 96; G: 128; B: 128),   //13: Shong Shuf
        (R: 255; G: 128; B: 128),  //14: Check box
        (R: 128; G: 128; B: 0)     //15: Drag
        );
    Keys: (VK_LEFT,   //0: Vol Dn
        VK_RIGHT,  //1: Vol Up
        VK_UP,     //2: Tr Prev
        VK_DOWN,   //3: Tr Next
        VK_UNKNOWN,//4: Pl Stop (unbound)
        VK_SPACE,  //5: Pl P/R
        VK_RETURN, //6: Pl Play
        VK_DELETE  //7: Tr Del
        );
    sReserved1: '';
    );
    CurrentSettings: TProgramSettings;

function GetPanelColor(p: TPanel): TColorRecMPJ;
begin
    Result.R := Red(p.Color);
    Result.G := Green(p.Color);
    Result.B := Blue(p.Color);
end;

procedure SetPanelColor(p: TPanel; rec: TColorRecMPJ);
begin
    p.Color := RGBToColor(rec.R, rec.G, rec.B);
end;

function IsCheckedPanel(p: TPanel): boolean;
begin
    Result := Trim(p.Caption) <> '';
end;

procedure SetCheckedPanel(p: TPanel; check: boolean);
begin
    if check then
        p.Caption := 'X'
    else
        p.Caption := '';
end;

{$R *.lfm}

{ TFrmSettings }

{********
 Event handlers
 ********}

procedure TFrmSettings.goClose(Sender: TObject);
begin
    Hide;
end;

procedure TFrmSettings.goPaintColorButton(Sender: TObject);
var
    p: TPanel;
    w: TWinControl;
begin
    p := Sender as TPanel;
    w := p.Parent;
    while (w.Parent <> nil) and (w.Parent.Enabled) do
        w := w.Parent;
    p.Canvas.Pen.Color := w.Font.Color;
    p.Canvas.Brush.Color := p.Color;
    p.Canvas.Rectangle(0, 0, p.Width, p.Height);
end;

procedure TFrmSettings.goApply(Sender: TObject);
var
    f: file of TProgramSettings;
begin
    Fisbinding := False;
{$I-}
    System.Assign(f, SettingsFile);
    System.Rewrite(f);
    Write(f, CurrentSettings);
    System.Close(f);
{$I+}
    if (IOResult <> 0) then
      begin
        if Assigned(Fmsgbox) then
          Fmsgbox.ShowDialog('Settings save error', 'Could not save new settings to file! New settings won''t be applied!');
      end
    else
      begin
        ProgramSettings := CurrentSettings;
        Hide;
        if Assigned(Fonaccept) then
          Fonaccept(Self);
      end;
end;

procedure TFrmSettings.goBindKey(Sender: TObject);
begin
    if Fisbinding then
      begin
        Fisbinding := False;
        Enabled := False;
        while TmrSettingsKbWait.Enabled do
            Application.ProcessMessages;
        Enabled := True;
      end;
    _keybind := _keybind.MaxValue;
    if (Sender = BdVolDn) then
        _keybind := 0
    else if (Sender = BdVolUp) then
        _keybind := 1
    else if (Sender = BdTrPrev) then
        _keybind := 2
    else if (Sender = BdTrNext) then
        _keybind := 3
    else if (Sender = BdStop) then
        _keybind := 4
    else if (Sender = BdPause) then
        _keybind := 5
    else if (Sender = BdStart) then
        _keybind := 6
    else if (Sender = BdDel) then
        _keybind := 7;
    if (_keybind < _keybind.MaxValue) then
      begin
        _kb_ctrl := Sender as TPanel;
        _kb_color := _kb_ctrl.Color;
        Fisbinding := True;
        TmrSettingsKbWait.Enabled := True;
      end;
end;

procedure TFrmSettings.goChangeBoolOption(Sender: TObject);
var
    p: TPanel;
    Checked: boolean;
begin
    goSimpleClick(Sender);
    p := Sender as TPanel;
    Checked := not IsCheckedPanel(p);
    SetCheckedPanel(p, Checked);
    if (p = CbSkipEn) then
        CurrentSettings.AllowSkipTracks := Checked
    else if (p = CbCopyPl) then
        CurrentSettings.CopyMusicOnSavePlaylist := Checked
    else if (p = CbHighShuf) then
        CurrentSettings.HighlightShuffled := Checked
    else if (p = CbShowTrunc) then
        CurrentSettings.ShowLongNamesInSummary := Checked
    else if (p = CbExtInf) then
        CurrentSettings.UseExtID3Info := Checked
    else if (p = CbExtPLS) then
        CurrentSettings.PlaylistUseExtFmt := Checked
    else if (p = CbID3T) then
        CurrentSettings.UseID3Title := Checked;
end;

procedure TFrmSettings.goChangeColor(Sender: TObject);
begin
    _color_el := Sender as TPanel;
    Fcolordlg.ShowDialog(_color_el.Color);
    TmrSettingsColorWait.Enabled := True;
end;

procedure TFrmSettings.goResetDefs(Sender: TObject);
begin
    Fisbinding := False;
    CurrentSettings := DefaultSettings;
    ResetUI;
end;

procedure TFrmSettings.goSetPLAddMode(Sender: TObject);
begin
    if (Sender = LstE_PLAddMode_ResetPL) then
        CurrentSettings.PlaylistAddMode := 0
    else
        CurrentSettings.PlaylistAddMode := 1;
    CbPLAddMode.Caption := (Sender as TPanel).Caption;
    LstPLAddMode.Hide;
end;

procedure TFrmSettings.goShowPLAddModes(Sender: TObject);
begin
    if not LstPLAddMode.Visible then
      begin
        SetPanelColor(LstE_PLAddMode_ResetPL, ProgramSettings.Colors[7]);
        SetPanelColor(LstE_PLAddMode_AddToPL, ProgramSettings.Colors[7]);
        case CurrentSettings.PlaylistAddMode of
            0: SetPanelColor(LstE_PLAddMode_ResetPL, ProgramSettings.Colors[8]);
            1: SetPanelColor(LstE_PLAddMode_AddToPL, ProgramSettings.Colors[8]);
          end;
        LstPLAddMode.Show;
      end
    else
        LstPLAddMode.Hide;
end;

procedure TFrmSettings.goSimpleClick(Sender: TObject);
begin
    LstPLAddMode.Hide;
end;

{Timer to wait for color dialog}
procedure TFrmSettings.goWaitForColorDlg(Sender: TObject);
var
    ci: integer;
begin
    if not Fcolordlg.Visible then
      begin
        TmrSettingsColorWait.Enabled := False;
        if Fcolordlg.DialogResult then
          begin
            _color_el.Color := Fcolordlg.SelectedColor;
            if (_color_el = CbtColorMainBg) then
                ci := 0
            else if (_color_el = CbtColorTitleBg) then
                ci := 1
            else if (_color_el = CbtBgColor) then
                ci := 2
            else if (_color_el = CbtButtonColor) then
                ci := 3
            else if (_color_el = CbtButtonDisColor) then
                ci := 4
            else if (_color_el = CbtFontColor) then
                ci := 5
            else if (_color_el = CbtFontColorDis) then
                ci := 6
            else if (_color_el = CbtEditColor) then
                ci := 7
            else if (_color_el = CbtMsgColor) then
                ci := 8
            else if (_color_el = CbtColorSong) then
                ci := 9
            else if (_color_el = CbtColorSongSel) then
                ci := 10
            else if (_color_el = CbtColorSongDrag) then
                ci := 11
            else if (_color_el = CbtColorSongSkip) then
                ci := 12
            else if (_color_el = CbtColorSongShuf) then
                ci := 13
            else if (_color_el = CbtCbColor) then
                ci := 14
            else if (_color_el = CbtDragColor) then
                ci := 15
            else
                ci := -1;
            if (ci >= 0) then
                CurrentSettings.Colors[ci] := GetPanelColor(_color_el);
          end;
      end;
end;

{Timer to flash the currently modified key}
procedure TFrmSettings.goWaitForKey(Sender: TObject);
begin
    if Fisbinding then
      begin
        _kb_ctrl.Color := _kb_ctrl.Color xor g_eventSvc.ButtonXorColor;
        _kb_ctrl.Font.Color := _kb_ctrl.Font.Color xor g_eventSvc.ButtonXorColor;
      end
    else
      begin
        TmrSettingsKbWait.Enabled := False;
        _kb_ctrl.ParentFont := True;
        _kb_ctrl.Color := _kb_color;
      end;
end;

{********
 Object procedures
 ********}

procedure TFrmSettings.ResetUI;

    function FindKeyName(VK: word): string;
    var
        i: integer;
    begin
        Result := UnkKeyName;
        for i := 0 to High(AllowedKeys) do
            if (AllowedKeys[i].VK = VK) then
                Exit(AllowedKeys[i].Name);
    end;

begin
    {Flow-control}
    SetCheckedPanel(CbHighShuf, CurrentSettings.HighlightShuffled);
    SetCheckedPanel(CbCopyPl, CurrentSettings.CopyMusicOnSavePlaylist);
    SetCheckedPanel(CbShowTrunc, CurrentSettings.ShowLongNamesInSummary);
    SetCheckedPanel(CbExtPLS, CurrentSettings.PlaylistUseExtFmt);
    SetCheckedPanel(CbExtInf, CurrentSettings.UseExtID3Info);
    SetCheckedPanel(CbSkipEn, CurrentSettings.AllowSkipTracks);
    SetCheckedPanel(CbID3T, CurrentSettings.UseID3Title);
    case CurrentSettings.PlaylistAddMode of
        0: CbPLAddMode.Caption := LstE_PLAddMode_ResetPL.Caption;
        1: CbPLAddMode.Caption := LstE_PLAddMode_AddToPL.Caption;
      end;
    {Colors}
    SetPanelColor(CbtColorMainBg, CurrentSettings.Colors[0]);
    SetPanelColor(CbtColorTitleBg, CurrentSettings.Colors[1]);
    SetPanelColor(CbtBgColor, CurrentSettings.Colors[2]);
    SetPanelColor(CbtButtonColor, CurrentSettings.Colors[3]);
    SetPanelColor(CbtButtonDisColor, CurrentSettings.Colors[4]);
    SetPanelColor(CbtFontColor, CurrentSettings.Colors[5]);
    SetPanelColor(CbtFontColorDis, CurrentSettings.Colors[6]);
    SetPanelColor(CbtEditColor, CurrentSettings.Colors[7]);
    SetPanelColor(CbtMsgColor, CurrentSettings.Colors[8]);
    SetPanelColor(CbtColorSong, CurrentSettings.Colors[9]);
    SetPanelColor(CbtColorSongSel, CurrentSettings.Colors[10]);
    SetPanelColor(CbtColorSongDrag, CurrentSettings.Colors[11]);
    SetPanelColor(CbtColorSongSkip, CurrentSettings.Colors[12]);
    SetPanelColor(CbtColorSongShuf, CurrentSettings.Colors[13]);
    SetPanelColor(CbtCbColor, CurrentSettings.Colors[14]);
    SetPanelColor(CbtDragColor, CurrentSettings.Colors[15]);
    {Keys}
    BdVolDn.Caption := FindKeyName(CurrentSettings.Keys[0]);
    BdVolUp.Caption := FindKeyName(CurrentSettings.Keys[1]);
    BdTrPrev.Caption := FindKeyName(CurrentSettings.Keys[2]);
    BdTrNext.Caption := FindKeyName(CurrentSettings.Keys[3]);
    BdStop.Caption := FindKeyName(CurrentSettings.Keys[4]);
    BdPause.Caption := FindKeyName(CurrentSettings.Keys[5]);
    BdStart.Caption := FindKeyName(CurrentSettings.Keys[6]);
    BdDel.Caption := FindKeyName(CurrentSettings.Keys[7]);
end;

constructor TFrmSettings.Create(AOwner: TComponent; messagebox: TFrmMsgBox);
var
    f: file of TProgramSettings;
begin
    inherited Create(AOwner);
    Fisbinding := False;
    Fonaccept := nil;
    Fmsgbox := messagebox;
    Fcolordlg := TFrmSelectColorMPJ.Create(AOwner);
    {Load settings, and set the buttons}
  {$I-}
    System.Assign(f, SettingsFile);
    System.Reset(f);
    Read(f, CurrentSettings);
    System.Close(f);
  {$I+}
    if (IOResult <> 0) then
        CurrentSettings := DefaultSettings;
    ProgramSettings := CurrentSettings;
    ResetUI;
end;

procedure TFrmSettings.SendKey(k: word);
var
    i, j: integer;
    kn: string;
begin
    if (k = VK_ESCAPE) then
      Fisbinding := False
    else if (k = VK_BACK) then // We can unbind a key, using BkSp
      begin
        CurrentSettings.Keys[_keybind] := VK_UNKNOWN;
        _kb_ctrl.Caption := UnkKeyName;
        Fisbinding := False;
      end
    else // Otherwise check, if we found an allowed key
      begin
        for i := 0 to High(AllowedKeys) do
            if (AllowedKeys[i].VK = k) then
              begin
                // Found an allowed key
                kn := AllowedKeys[i].Name;
                // Check for duplicates
                for j := 0 to High(CurrentSettings.Keys) do
                    if (j <> _keybind) and (CurrentSettings.Keys[j] = k) then
                      begin
                        if Assigned(Fmsgbox) then
                          Fmsgbox.ShowDialog('Duplicate key', 'This key is already bound to a function! Please use another key!');
                        exit;
                      end;
                // Otherwise, OK
                CurrentSettings.Keys[_keybind] := k;
                _kb_ctrl.Caption := kn;
                Fisbinding := False;
                exit;
              end;
        if Assigned(Fmsgbox) then
          Fmsgbox.ShowDialog('Not allowed key', 'This key is not allowed!');
      end;
end;

procedure TFrmSettings.Show;
begin
    CurrentSettings := ProgramSettings;
    ResetUI;
    inherited;
end;

procedure TFrmSettings.Hide;
begin
    Fisbinding := False;
    LstPLAddMode.Hide;
    inherited;
end;

procedure TFrmSettings.Accept;
begin
    goApply(BtnOK);
end;

end.

