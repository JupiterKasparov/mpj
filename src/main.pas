unit main;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
    ExtCtrls, StdCtrls,
    c_vertsb, c_files, c_msgbox, c_questdlg, c_song, c_horzsb, c_jump,
    c_speed, c_settings, c_extinfo,
    MpjPlaylists, crDefs,
    LMessages, LCLType, strutils,
    Windows, Types, simpleipc, Clipbrd;

type

    { TWndMain }

    TWndMain = class(TForm)
        AppMainAppProperties: TApplicationProperties;
        BtnAbortLoad: TPanel;
        BtnAdd: TPanel;
        BtnClear: TPanel;
        BtnClose: TPanel;
        BtnMinimize: TPanel;
        BtnNext: TPanel;
        BtnPlay: TPanel;
        BtnPrev: TPanel;
        BtnSavePlaylist: TPanel;
        BtnStop: TPanel;
        BtnOpenPlaylist: TPanel;
        CbSkip: TPanel;
        ScbTracks: TFrmVertSb;
        ScbVolume: TFrmHorzSb;
        ScbTrackInfo: TFrmHorzSb;
        IpcClient: TSimpleIPCClient;
        IpcMainServer: TSimpleIPCServer;
        LbContinuous: TLabel;
        LbRandom: TLabel;
        LbSkip: TLabel;
        LbTrackInfo: TLabel;
        LbCounter: TLabel;
        LbPos: TLabel;
        CbRandom: TPanel;
        CbContinuous: TPanel;
        PnTextCCP: TPanel;
        PnTextCopy: TPanel;
        PbLoad: TPanel;
        PbSave: TPanel;
        PnHead: TPanel;
        PnLoad: TPanel;
        PnSave: TPanel;
        PnPlayModes: TPanel;
        BtnSettings: TPanel;
        PnSkip: TPanel;
        PnStepBtns: TPanel;
        PnPlaylistOS: TPanel;
        PnTextCut: TPanel;
        PnTextPaste: TPanel;
        PnTextSelectAll: TPanel;
        PnTitle: TPanel;
        PnTrackProps: TPanel;
        PnCounters: TPanel;
        PnPlayPosSlider: TPanel;
        PnPlayPos: TPanel;
        PnVolume: TPanel;
        PnMusicTools: TPanel;
        PnSummary: TPanel;
        PnMain: TPanel;
        PnPlaylist: TPanel;
        PnSHd_Length: TPanel;
        PnSHd_Name: TPanel;
        PnSHd_Num: TPanel;
        PnSHd_Tools: TPanel;
        PnSongHeaders: TPanel;
        PnPosition: TPanel;
        TmrMain: TTimer;
        TmrDragSongCursorSet: TTimer;
        procedure goAbortLoad(Sender: TObject);
        procedure goAdd(Sender: TObject);
        procedure goCheckTrackDragCursor(Sender: TObject);
        procedure goDragWindow(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
        procedure goHandleException(Sender: TObject; E: Exception);
        procedure goScrollPosition(Sender: TObject; Shift: TShiftState; WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
        procedure goTextAction(Sender: TObject);
        procedure goWindowProgressMain(Sender: TObject);
        procedure goClearPlaylist(Sender: TObject);
        procedure goCloseWnd(Sender: TObject);
        procedure goCreateWnd(Sender: TObject);
        procedure goDestroyWnd(Sender: TObject);
        procedure goEndSlidePos(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
        procedure goFilesDropped(Sender: TObject; const FileNames: array of string);
        procedure goLoadFilesOfOtherInstance(Sender: TObject);
        procedure goMinimizeWnd(Sender: TObject);
        procedure goMoveSlidePos(Sender: TObject; Shift: TShiftState; X, Y: integer);
        procedure goNextTrack(Sender: TObject);
        procedure goOpenPlaylist(Sender: TObject);
        procedure goPaintLoadBar(Sender: TObject);
        procedure goPaintSaveBar(Sender: TObject);
        procedure goPlay(Sender: TObject);
        procedure goPrevTrack(Sender: TObject);
        procedure goSavePlaylist(Sender: TObject);
        procedure goSetEndless(Sender: TObject);
        procedure goSetRandomizeTracks(Sender: TObject);
        procedure goSetSkipTrack(Sender: TObject);
        procedure goShortcutPressed(var Msg: TLMKey; var Handled: boolean);
        procedure goShowExtInfo(Sender: TObject);
        procedure goShowSettings(Sender: TObject);
        procedure goShowSummary(Sender: TObject);
        procedure goStartSlidePos(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
        procedure goStop(Sender: TObject);
        procedure goTryCloseWnd(Sender: TObject; var CanClose: boolean);
    private
        Fselectedtxt: TEdit;
        {Flow-control variables}
        check_enabled: boolean;
        songs: TSongList;
        songpos, skp, sdr: integer;
        is_random_track, endless, stopped, pl_abort: boolean;
        sf: TSongList; //Song cannot be freed directly inside single removal -> it's freed using this var
        FilesToLoad: TStrings;
        {Draggable-element-positioning/dragging variables}
        m_pos, m_song: boolean;
        p_x: integer;
        {Progressbar-related variables}
        CurrentLoadStep, CurrentSaveStep, MaxSave: integer;
        {Main instance check}
        IsMainInstance: boolean;
        {Dialogs}
        FrmFileDlg: TFrmFileDialogMPJ;
        MsgBox: TFrmMsgBox;
        QuestionDlg: TFrmQuestionDlg;
        DlgJump: TFrmJumpToPos;
        DlgSpeed: TFrmSetSpeed;
        DlgSet: TFrmSettings;
        DlgExtInf: TFrmExtInfo;
        {Private procedures}
        procedure scScrollVolume(Sender: TObject);
        procedure scScrollTrackInfo(Sender: TObject);
        procedure scScrollTracks(Sender: TObject);
        procedure trReOrder;
        procedure trMoveUp(Sender: TObject);
        procedure trMoveDown(Sender: TObject);
        procedure trDelete(Sender: TObject);
        procedure trPlay(Sender: TObject);
        procedure trChangeSpeed(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
        procedure trSkip(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
        procedure trStartDrag(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
        procedure trDrag(Sender: TObject; Shift: TShiftState; X, Y: integer);
        procedure trEndDrag(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
        procedure wnduCheckForm(form: TScrollingWinControl);
        procedure wnduRecolorForm;
        procedure musLoad(mfile: string);
        procedure evtMusLoad(Sender: TObject);
        procedure evtListLoad(Sender: TObject);
        procedure evtListSave(Sender: TObject);
        procedure evtSettingsChanged(Sender: TObject);
        procedure evtPlaylistError(Error: string);
        procedure pmCustomPopup(Sender: TObject; MousePos: TPoint; var Handled: boolean);
    end;

var
    WndMain: TWndMain;

implementation

const
    strPause: string = 'Pause';
    strPlay: string = 'Play';

procedure BringToTop(c: TWinControl); inline;
begin
    BringWindowToTop(c.Handle);
end;

{$R *.lfm}

{ TWndMain }

{
***************************************
** Published (event handler) methods **
***************************************
}

procedure TWndMain.goAdd(Sender: TObject);
begin
    FrmFileDlg.ShowFileDialog(fdkLoadMusic);
end;

procedure TWndMain.goAbortLoad(Sender: TObject);
begin
    pl_abort := True;
end;

procedure TWndMain.goCheckTrackDragCursor(Sender: TObject);
var
    i: integer;
    p: TPoint;
begin
    if m_song then
      begin
        p := songs[sdr].ScreenToClient(Mouse.CursorPos);
        if (p.x >= 0) and (p.x <= songs[sdr].Width) and (p.y >= 0) and (p.y <= songs[sdr].Height) then
          begin
            for i := 0 to songs[sdr].ComponentCount - 1 do
                if (songs[sdr].Components[i] is TControl) then
                    (songs[sdr].Components[i] as TControl).Cursor := crGrabbed;
          end
        else
            m_song := False;
      end;
    TmrDragSongCursorSet.Enabled := False;
end;

procedure TWndMain.goDragWindow(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
    ReleaseCapture;
    SendMessage(Handle, WM_SYSCOMMAND, SC_MOVE + HTCAPTION, 0);
end;

procedure TWndMain.goHandleException(Sender: TObject; E: Exception);
begin
    Screen.Cursor := crDefault;
    Application.MessageBox(PChar(Format('Exception "%s" occurred at address 0x%p with message:'#13#10'%s', [E.ClassName, ExceptAddr, E.Message])), PChar('Unhandled Exception'), MB_OK + MB_ICONERROR);
end;

procedure TWndMain.goScrollPosition(Sender: TObject; Shift: TShiftState; WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
begin
    Handled := True;
    if (songs.Count > 0) and (not songs[songpos].Stopped) then
      begin
        if (WheelDelta > 0) then
            PnPlayPosSlider.Left := PnPlayPosSlider.Left + 1
        else if (WheelDelta < 0) then
            PnPlayPosSlider.Left := PnPlayPosSlider.Left - 1;
        if (PnPlayPosSlider.Left < 0) then
            PnPlayPosSlider.Left := 0
        else if (PnPlayPosSlider.Left > (PnPlayPos.Width - PnPlayPosSlider.Width)) then
            PnPlayPosSlider.Left := PnPlayPos.Width - PnPlayPosSlider.Width;
        songs[songpos].PositionSec := round((PnPlayPosSlider.Left / 100) * songs[songpos].LengthSec);
      end;
end;

procedure TWndMain.goTextAction(Sender: TObject);
begin
    if (Sender = PnTextCopy) then
        Fselectedtxt.CopyToClipboard
    else if (Sender = PnTextCut) then
        Fselectedtxt.CutToClipboard
    else if (Sender = PnTextPaste) then
        Fselectedtxt.PasteFromClipboard
    else if (Sender = PnTextSelectAll) then
        Fselectedtxt.SelectAll;
    PnTextCCP.Hide;
end;

procedure TWndMain.goWindowProgressMain(Sender: TObject);

    procedure _LoadFiles;
    {%region /fold}
    begin
        if (FilesToLoad.Count > 0) then
          begin
            if PnSave.Visible then
                Exit;
            if not PnLoad.Visible then
              begin
                Screen.Cursor := crHourGlass;
                CurrentLoadStep := 0;
                PnLoad.Show;
                DlgExtInf.Hide;
                DlgExtInf.LbExtInfo.Caption := '';
              end;
            if (CurrentLoadStep < FilesToLoad.Count) and (not pl_abort) then
              begin
                musLoad(FilesToLoad[CurrentLoadStep]);
                Inc(CurrentLoadStep);
                PbLoad.Invalidate;
              end
            else
              begin
                FilesToLoad.Clear;
                PnLoad.Hide;
                Screen.Cursor := crDefault;
                if (DlgExtInf.LbExtInfo.Caption <> '') then
                    DlgExtInf.ShowDialog('Errors during loading', DlgExtInf.LbExtInfo.Caption, nil);
              end;
          end;
    end;

    {%endregion}

    procedure _CheckGUI;
    {%region /fold}

        procedure _CheckButtonColor(colored: TPanel);
        var
            p: TControl;
            _en: boolean = True;
        begin
            p := colored.Parent;
            while Assigned(p) do
              begin
                if not p.Enabled then
                  begin
                    _en := False;
                    break;
                  end;
                p := p.Parent;
              end;
            // Button: we need separate code, do not override the 'button pressed' effect
            if AnsiContainsStr(colored.HelpKeyword, 'X') then
              begin
                if colored.Enabled then
                  begin
                    if not colored.MouseEntered then
                      begin
                        colored.Color := g_EventSvc.ButtonColor;
                        if _en then
                            colored.ParentFont := True
                        else
                            colored.Font.Color := RGBToColor(ProgramSettings.Colors[6].R, ProgramSettings.Colors[6].G, ProgramSettings.Colors[6].B);
                      end;
                  end
                else
                  begin
                    colored.Color := g_EventSvc.ButtonDisabledColor;
                    colored.Font.Color := RGBToColor(ProgramSettings.Colors[6].R, ProgramSettings.Colors[6].G, ProgramSettings.Colors[6].B);
                  end;
              end
            // Other
            else
              begin
                if colored.Enabled and _en then
                    colored.ParentFont := True
                else
                    colored.Font.Color := RGBToColor(ProgramSettings.Colors[6].R, ProgramSettings.Colors[6].G, ProgramSettings.Colors[6].B);
              end;
        end;

        procedure _CheckColors(container: TScrollingWinControl);
        var
            i: integer;
        begin
            for i := 0 to container.ComponentCount - 1 do
              begin
                if (container.Components[i] is TFrame) or (container.Components[i] is TForm) then
                    _CheckColors(container.Components[i] as TScrollingWinControl)
                else if (container.Components[i] is TPanel) then
                    _CheckButtonColor(container.Components[i] as TPanel);
              end;
        end;

    var
        i, j, si, sl, h, m, s, ci: integer;
        temp: TFrmSong;
    begin
        {Main panel and Buttons enabled}
        PnMain.Enabled := (not PnLoad.Visible) and
            (not PnSave.Visible) and
            (not FrmFileDlg.Visible) and
            (not DlgJump.Visible) and
            (not DlgSpeed.Visible) and
            (not DlgSet.Visible) and
            (not DlgExtInf.Visible) and
            (not QuestionDlg.Visible) and
            (not MsgBox.Visible) and
            Active;
        PnHead.Enabled := Active;
        BtnSettings.Enabled := not DlgSet.Visible;
        BtnAdd.Enabled := not FrmFileDlg.Visible;
        BtnPlay.Enabled := (songs.Count > 0);
        if stopped or songs[songpos].Paused then
            BtnPlay.Caption := strPlay
        else
            BtnPlay.Caption := strPause;
        BtnStop.Enabled := not stopped;
        BtnClear.Enabled := (songs.Count > 0);
        BtnOpenPlaylist.Enabled := BtnAdd.Enabled;
        BtnSavePlaylist.Enabled := BtnOpenPlaylist.Enabled and (songs.Count > 0);
        BtnPrev.Enabled := (is_random_track or endless or (songpos > 0)) and (songs.Count > 0);
        BtnNext.Enabled := (is_random_track or endless or (songpos < (songs.Count - 1))) and (songs.Count > 0);
        if PnSkip.Visible then
          begin
            i := PnSkip.ScreenToClient(Mouse.CursorPos).x;
            j := PnSkip.ScreenToClient(Mouse.CursorPos).y;
            if (i < 0) or (j < 0) or (i > PnSkip.Width) or (j > PnSkip.Height) or (skp >= songs.Count) then
                PnSkip.Hide
            else
                BringToTop(PnSkip);
          end;
        {Progress bars enabled}
        PnLoad.Enabled := (not DlgExtInf.Visible) and
            (not QuestionDlg.Visible) and
            (not MsgBox.Visible);
        if PnLoad.Visible then
            BringToTop(PnLoad);
        PnSave.Enabled := PnLoad.Enabled;
        if PnSave.Visible then
            BringToTop(PnSave);
        {Dialogs enabled}
        if DlgSet.ColorDialog.Visible and (not DlgSet.Visible) then
            DlgSet.ColorDialog.Hide
        else if DlgSet.ColorDialog.Visible then
            BringToTop(DlgSet.ColorDialog);
        DlgSet.Enabled := (not DlgSet.ColorDialog.Visible) and
            (not DlgExtInf.Visible) and
            (not QuestionDlg.Visible) and
            (not MsgBox.Visible);
        DlgSet.ColorDialog.Enabled := (not DlgExtInf.Visible) and
            (not QuestionDlg.Visible) and
            (not MsgBox.Visible);
        FrmFileDlg.Enabled := DlgSet.ColorDialog.Enabled;
        DlgJump.Enabled := DlgSet.ColorDialog.Enabled;
        DlgSpeed.Enabled := DlgSet.ColorDialog.Enabled;
        DlgExtInf.Enabled := (not QuestionDlg.Visible) and (not MsgBox.Visible);
        if DlgExtInf.Visible then
            BringToTop(DlgExtInf);
        QuestionDlg.Enabled := not MsgBox.Visible;
        if QuestionDlg.Visible then
            BringToTop(QuestionDlg);
        if MsgBox.Visible then
            BringToTop(MsgBox);
        {Bring menu to front}
        if PnTextCCP.Visible then
          begin
            i := PnTextCCP.ScreenToClient(Mouse.CursorPos).x;
            j := PnTextCCP.ScreenToClient(Mouse.CursorPos).y;
            if (i < 0) or (j < 0) or (i > PnTextCCP.Width) or (j > PnTextCCP.Height) then
                PnTextCCP.Hide
            else
                BringToTop(PnTextCCP);
          end;
        {Tracklist summary, Colors, and Captions}
        if (songs.Count > 0) then
          begin
            si := -1;
            sl := 0;
            for i := 0 to songs.Count - 1 do
              begin
                 {When we get an element from a list, it takes time
                  It takes less time to store the element in a variable,and access that var,
                  than to get it each time we access the element}
                temp := songs[i];
                {Set song color, depending on its state}
                if m_song and (not TmrDragSongCursorSet.Enabled) and (sdr = i) then
                    ci := 11
                else if (i = songpos) then
                    ci := 10
                else if (temp.Shuffled) and ProgramSettings.HighlightShuffled then
                    ci := 13
                else if (temp.SkipIt) and ProgramSettings.AllowSkipTracks then
                    ci := 12
                else
                    ci := 9;
                temp.Color := RGBToColor(ProgramSettings.Colors[ci].R, ProgramSettings.Colors[ci].G, ProgramSettings.Colors[ci].B);
                {The length is only used here, to show the summary; if we have a ShowFullName, do not do it}
                if ProgramSettings.ShowLongNamesInSummary and temp.ShowFullName then
                    si := i
                else if (si < 0) then
                    Inc(sl, temp.LengthSec);
                {Button colors}
                _CheckColors(temp as TScrollingWinControl);
              end;
            if (si >= 0) then
                PnSummary.Caption := songs[si].PnName.Caption
            else
              begin
                h := sl div 3600;
                m := (sl - (h * 3600)) div 60;
                s := sl - (h * 3600) - (m * 60);
                if (songs.Count = 1) then
                  begin
                    if (h = 0) then
                        PnSummary.Caption := Format('Total length: %.2d:%.2d in 1 track', [m, s])
                    else
                        PnSummary.Caption := Format('Total length: %.2d:%.2d:%.2d in 1 track', [h, m, s]);
                  end
                else if (songs.Count > 1) then
                  begin
                    if (h = 0) then
                        PnSummary.Caption := Format('Total length: %.2d:%.2d in %d tracks', [m, s, songs.Count])
                    else
                        PnSummary.Caption := Format('Total length: %.2d:%.2d:%.2d in %d tracks', [h, m, s, songs.Count]);
                  end;
              end;
            LbPos.Caption := Format('%d/%d', [songpos + 1, songs.Count]);
            if (songs.Count > 0) and songs[songpos].HasID3Tag then
              begin
                if not PnTrackProps.Visible then
                    PnTrackProps.Visible := True;
                LbTrackInfo.Caption := songs[songpos].ID3;
                if (LbTrackInfo.Width > (PnTrackProps.Width - 8)) then
                  begin
                    if not ScbTrackInfo.Visible then
                      begin
                        ScbTrackInfo.Max := LbTrackInfo.Width - PnTrackProps.Width + 16;
                        if Assigned(ScbTrackInfo.OnScroll) then
                            ScbTrackInfo.OnScroll(ScbTrackInfo);
                        ScbTrackInfo.Visible := True;
                      end;
                  end
                else
                  begin
                    ScbTrackInfo.Visible := False;
                    LbTrackInfo.Left := 8;
                    ScbTrackInfo.Percentage := 0.0;
                    ScbTrackInfo.Max := 0;
                  end;
              end
            else if PnTrackProps.Visible then
              PnTrackProps.Visible := false;
            if not songs[songpos].Stopped then
              begin
                songs[songpos].GetPosition(h, m, s);
                if (h = 0) then
                    LbCounter.Caption := Format('%.2d:%.2d', [m, s])
                else
                    LbCounter.Caption := Format('%.2d:%.2d:%.2d', [h, m, s]);
                if not m_pos then
                    PnPlayPosSlider.Left := round((songs[songpos].PositionSec / songs[songpos].LengthSec) * 100);
              end
            else
              begin
                DlgJump.Hide;
                if (songs[songpos].LengthSec >= 3600) then
                    LbCounter.Caption := '00:00:00'
                else
                    LbCounter.Caption := '00:00';
                PnPlayPosSlider.Left := 0;
              end;
          end
        else
          begin
            LbPos.Caption := '0/0';
            LbCounter.Caption := '00:00';
            PnSummary.Caption := 'No tracks loaded';
            PnPlayPosSlider.Left := 0;
            PnTrackProps.Visible := False;
          end;
        _CheckColors(Self);
    end;

    {%endregion}

    procedure TracklistMain;
    {%region /fold}

        procedure PlayNextSongRandom;
        var
            viablesongs: TSongList;
            skippedsongs, playedsongs, i: integer;
        begin
            // If we have only 1 song, just play it again
            if (songs.Count = 1) then
                trPlay(songs[0].PnNum)
            // Otherwise, let's see the list of possible songs
            else
              begin
                viablesongs := TSongList.Create;
                // Count the skipped and played songs
                skippedsongs := 0;
                playedsongs := 0;
                for i := 0 to songs.Count - 1 do
                  begin
                    if songs[i].SkipIt and ProgramSettings.AllowSkipTracks then
                        Inc(skippedsongs)
                    else if songs[i].Shuffled then
                        Inc(playedsongs)
                    else if (i <> songpos) then
                        viablesongs.Add(songs[i]);
                  end;
                // No songs remain to be played? First, try to reset the list (next shuffle cycle)
                if (viablesongs.Count = 0) then
                  begin
                    skippedsongs := 0;
                    for i := 0 to songs.Count - 1 do
                      begin
                        songs[i].Shuffled := False;
                        if songs[i].SkipIt and ProgramSettings.AllowSkipTracks then
                            Inc(skippedsongs)
                        else if (i <> songpos) then
                            viablesongs.Add(songs[i]);
                      end;
                    // All songs are marked to be skipped? Select first non-current song
                    if (viablesongs.Count = 0) then
                      begin
                        for i := 0 to songs.Count - 1 do
                            if (i <> songpos) then
                              begin
                                trPlay(songs[i].PnNum);
                                break;
                              end;
                        viablesongs.Free;
                      end
                    else
                      begin
                        // Play a random, viable song
                        trPlay(viablesongs[random(viablesongs.Count)].PnNum);
                        viablesongs.Free;
                      end;
                  end
                else
                  begin
                    // Play a random, viable song
                    trPlay(viablesongs[random(viablesongs.Count)].PnNum);
                    viablesongs.Free;
                  end;
              end;
        end;

    var
        i: integer;
    begin
        {Tracklist main controller}
        for i := 0 to songs.Count - 1 do
            with songs[i] do
              begin
                if is_random_track and not Self.stopped then
                  begin
                    if not Stopped then
                        Shuffled := True;
                  end
                else
                    Shuffled := False;
              end;
        if (songs.Count > 0) then
          begin
            if songs[songpos].Stopped and not stopped then
              begin
                if is_random_track then
                    PlayNextSongRandom
                else
                  begin
                    if (songpos < songs.Count - 1) then
                        trPlay(songs[songpos + 1].PnNum)
                    else if endless then
                        trPlay(songs[0].PnNum)
                    else
                        stopped := True;
                  end;
              end;
          end;
    end;

    {%endregion}

begin
    if not check_enabled then
        exit;
    DisableAutoSizing;
    IpcMainServer.ReadMessage;
    _LoadFiles;
    _CheckGUI;
    TracklistMain;
    EnableAutoSizing;
    while (sf.Count > 0) do
      begin
        sf.Last.Free;
        sf.Remove(sf.Last);
      end;
end;

procedure TWndMain.goClearPlaylist(Sender: TObject);
var
    i: integer;
begin
    check_enabled := False;
    for i := 0 to songs.Count - 1 do
      begin
        songs[i].Hide;
        songs[i].Free;
      end;
    songs.Clear;
    ScbTracks.Max := 0;
    ScbTracks.ScDrag.Top := 0; // HACK
    songpos := 0;
    stopped := True;
    check_enabled := True;
end;

procedure TWndMain.goCloseWnd(Sender: TObject);
begin
    Close;
end;

procedure TWndMain.goCreateWnd(Sender: TObject);

    procedure _PassParamsToMainInstance;
    {%region /fold}
    var
        i: integer;
    begin
        TmrMain.Enabled := False; // Do not start the TIMER
        Visible := False;
        Application.ShowMainForm := False;
        ShowInTaskbar := stNever;
        if (Paramcount >= 1) then
            for i := 1 to Paramcount do
                IpcClient.SendStringMessage(ParamStr(i));
        Close;
        Application.Terminate;
    end;

    {%endregion}

    procedure _LoadParams;
    {%region /fold}
    var
        i, j: integer;
        lst: TPlaylist;
    begin
        for i := 1 to ParamCount do
            if FileExists(ParamStr(i)) then
              begin
                if IsPlaylist(ParamStr(i)) then
                  begin
                    LoadPlaylist(lst, ParamStr(i));
                    for j := 0 to High(lst.Files) do
                        FilesToLoad.Add(lst.Files[j].FileName);
                    SetLength(lst.Files, 0);
                  end
                else
                    FilesToLoad.Add(ParamStr(i));
              end;
    end;

    {%endregion}

begin
    {Main instance check, and Inter-Process communication init}
    IpcMainServer.ServerID := 'MusPlay_Jupiter';
    IpcClient.ServerID := IpcMainServer.ServerID;
    IsMainInstance := not IpcClient.ServerRunning;
    if IsMainInstance then
        IpcMainServer.StartServer;
    IpcClient.Connect;
    {We only allocate resources, if we are the main instance}
    if IsMainInstance then
      begin
        {Main init}
        check_enabled := True;
        Fselectedtxt := nil;
        songs := TSongList.Create;
        songpos := 0;
        skp := 0;
        sdr := 0;
        is_random_track := False;
        endless := False;
        stopped := True;
        pl_abort := False;
        FilesToLoad := TStringList.Create;
        sf := TSongList.Create;
        {Init basic event handlers}
        ScbVolume.OnScroll := @scScrollVolume;
        ScbTrackInfo.OnScroll := @scScrollTrackInfo;
        PnTrackProps.OnMouseWheel := @ScbTrackInfo.goScroll;
        LbTrackInfo.OnMouseWheel := @ScbTrackInfo.goScroll;
        ScbTracks.OnScroll := @scScrollTracks;
        OnPlaylistError := @evtPlaylistError;
        {Initial graphics}
        Application.Title := PnTitle.Caption;
        ScbTracks.Max := 0;
        Left := max(0, Left);
        Top := max(0, Top);
        {Add other windows}
        {Generic message box}
        MsgBox := TFrmMsgBox.Create(Self);
        MsgBox.Left := (Width div 2) - (MsgBox.Width div 2);
        MsgBox.Top := (Height div 2) - (MsgBox.Height div 2);
        MsgBox.Parent := Self;
        {Yes/No (Question) dialog}
        QuestionDlg := TFrmQuestionDlg.Create(Self);
        QuestionDlg.Left := (Width div 2) - (QuestionDlg.Width div 2);
        QuestionDlg.Top := (Height div 2) - (QuestionDlg.Height div 2);
        QuestionDlg.Parent := Self;
        {File dialog}
        FrmFileDlg := TFrmFileDialogMPJ.Create(self, GetCurrentDir);
        FrmFileDlg.Left := (Width div 2) - (FrmFileDlg.Width div 2);
        FrmFileDlg.Top := (Height div 2) - (FrmFileDlg.Height div 2);
        FrmFileDlg.OnAcceptLoadFiles := @evtMusLoad;
        FrmFileDlg.OnAcceptLoadList := @evtListLoad;
        FrmFileDlg.OnAcceptSaveList := @evtListSave;
        FrmFileDlg.Parent := Self;
        {'Jump to position' dialog}
        DlgJump := TFrmJumpToPos.Create(self, MsgBox);
        DlgJump.Left := (Width div 2) - (DlgJump.Width div 2);
        DlgJump.Top := (Height div 2) - (DlgJump.Height div 2);
        DlgJump.Parent := Self;
        {Speed dialog}
        DlgSpeed := TFrmSetSpeed.Create(self, MsgBox);
        DlgSpeed.Left := (Width div 2) - (DlgSpeed.Width div 2);
        DlgSpeed.Top := (Height div 2) - (DlgSpeed.Height div 2);
        DlgSpeed.Parent := Self;
        {Settings dialog}
        DlgSet := TFrmSettings.Create(Self, MsgBox);
        DlgSet.Left := (Width div 2) - (DlgSet.Width div 2);
        DlgSet.Top := (Height div 2) - (DlgSet.Height div 2);
        DlgSet.OnAccept := @evtSettingsChanged;
        DlgSet.Parent := Self;
        {Settings / Select color sub-dialog}
        DlgSet.ColorDialog.Left := (Width div 2) - (DlgSet.ColorDialog.Width div 2);
        DlgSet.ColorDialog.Top := (Height div 2) - (DlgSet.ColorDialog.Height div 2);
        DlgSet.ColorDialog.Parent := Self;
        DlgSet.ColorDialog.Hide;
        {Ext info dialog}
        DlgExtInf := TFrmExtInfo.Create(Self);
        DlgExtInf.Left := (Width div 2) - (DlgExtInf.Width div 2);
        DlgExtInf.Top := (Height div 2) - (DlgExtInf.Height div 2);
        DlgExtInf.Parent := Self;
        {Hide them}
        FrmFileDlg.Hide;
        DlgJump.Hide;
        DlgSpeed.Hide;
        DlgSet.Hide;
        DlgExtInf.Hide;
        MsgBox.Hide;
        QuestionDlg.Hide;
        {Init progressbars}
        PnLoad.Left := (Width div 2) - (PnLoad.Width div 2);
        PnLoad.Top := (Height div 2) - (PnLoad.Height div 2);
        PnSave.Left := (Width div 2) - (PnSave.Width div 2);
        PnSave.Top := (Height div 2) - (PnSave.Height div 2);
        {Define application-wide GUI rules}
        wnduCheckForm(self);
        wnduRecolorForm;
        g_EventSvc.ButtonXorColor := 32767;
        {Load associated files, if the program's been started using one}
        _LoadParams;
      end
    {Otherwise, we pass our CmdLine tracks to the Main Instance}
    else
        _PassParamsToMainInstance;
end;

procedure TWndMain.goDestroyWnd(Sender: TObject);
var
    i: integer;
begin
    {These resources will only be assigned, if we are the main instance}
    if IsMainInstance then
      begin
        {Dialogs free}
        FrmFileDlg.Free;
        DlgJump.Free;
        DlgSpeed.Free;
        DlgSet.Free;
        DlgExtInf.Free;
        MsgBox.Free;
        QuestionDlg.Free;
        {Stop Inter-Process server}
        IpcMainServer.StopServer;
        {Unload tracks}
        for i := 0 to songs.Count - 1 do
          begin
            songs[i].Hide;
            songs[i].Stop;
            songs[i].Free;
          end;
        while (sf.Count > 0) do
          begin
            sf.Last.Free;
            sf.Remove(sf.Last);
          end;
        sf.Free;
        FilesToLoad.Clear;
        FilesToLoad.Free;
        songs.Clear;
        songs.Free;
      end;
end;

procedure TWndMain.goEndSlidePos(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
    if m_pos then
      begin
        if (songs.Count > 0) then
            songs[songpos].PositionSec := round((PnPlayPosSlider.Left / 100) * songs[songpos].LengthSec);
      end
    else
    if (Button = mbRight) and (songs.Count > 0) and not stopped then
        DlgJump.ShowDialog(songs[songpos]);
    PnPlayPosSlider.Cursor := crGrab;
    m_pos := False;
end;

procedure TWndMain.goFilesDropped(Sender: TObject; const FileNames: array of string);
var
    i, j: integer;
    lst: TPlaylist;
begin
    pl_abort := False;
    for i := 0 to High(FileNames) do
        if IsPlaylist(FileNames[i]) then
          begin
            LoadPlaylist(lst, FileNames[i]);
            for j := 0 to High(lst.Files) do
                FilesToLoad.Add(lst.Files[j].FileName);
            SetLength(lst.Files, 0);
          end
        else
            FilesToLoad.Add(FileNames[i]);
end;

procedure TWndMain.goLoadFilesOfOtherInstance(Sender: TObject);
var
    lst: TPlaylist;
    i: integer;
begin
    pl_abort := False;
    if IsPlaylist(IpcMainServer.StringMessage) then
      begin
        LoadPlaylist(lst, IpcMainServer.StringMessage);
        for i := 0 to High(lst.Files) do
            FilesToLoad.Add(lst.Files[i].FileName);
        SetLength(lst.Files, 0);
      end
    else
        FilesToLoad.Add(IpcMainServer.StringMessage);
end;

procedure TWndMain.goMinimizeWnd(Sender: TObject);
begin
    Application.Minimize;
end;

procedure TWndMain.goMoveSlidePos(Sender: TObject; Shift: TShiftState; X, Y: integer);
begin
    if (songs.Count > 0) and (not stopped) and m_pos then
      begin
        PnPlayPosSlider.Left := PnPlayPosSlider.Left + (X - p_x);
        if (PnPlayPosSlider.Left < 0) then
            PnPlayPosSlider.Left := 0
        else if (PnPlayPosSlider.Left > 100) then
            PnPlayPosSlider.Left := 100;
      end;
end;

procedure TWndMain.goNextTrack(Sender: TObject);
begin
    if is_random_track then
      begin
        if stopped then
            trPlay(songs[songpos].PnNum)
        else
            songs[songpos].Stop;
      end
    else
      begin
        if endless then
          begin
            if (songpos < songs.Count - 1) then
                trPlay(songs[songpos + 1].PnNum)
            else
                trPlay(songs[0].PnNum);
          end
        else
            trPlay(songs[songpos + 1].PnNum);
      end;
end;

procedure TWndMain.goOpenPlaylist(Sender: TObject);
begin
    FrmFileDlg.ShowFileDialog(fdkLoadPlaylist);
end;

procedure TWndMain.goPaintLoadBar(Sender: TObject);
begin
    with PbLoad.Canvas do
      begin
        Brush.Color := PbLoad.Color;
        Brush.Style := bsSolid;
        Pen.Style := psSolid;
        Pen.Color := PbLoad.Font.Color;
        Rectangle(0, 0, PbLoad.Width, PbLoad.Height);
        Brush.Color := PbLoad.Color xor g_EventSvc.ButtonXorColor;
        Pen.Style := psClear;
        Rectangle(1, 1, round((CurrentLoadStep / max(FilesToLoad.Count, 1)) * PbLoad.Width), PbLoad.Height);
      end;
end;

procedure TWndMain.goPaintSaveBar(Sender: TObject);
begin
    with PbSave.Canvas do
      begin
        Brush.Color := PbSave.Color;
        Brush.Style := bsSolid;
        Pen.Style := psSolid;
        Pen.Color := PbSave.Font.Color;
        Rectangle(0, 0, PbSave.Width, PbSave.Height);
        Brush.Color := PbSave.Color xor g_EventSvc.ButtonXorColor;
        Pen.Style := psClear;
        Rectangle(1, 1, round((CurrentSaveStep / max(MaxSave, 1)) * PbSave.Width), PbSave.Height);
      end;
end;

procedure TWndMain.goPlay(Sender: TObject);
begin
    if songs[songpos].Stopped then
      begin
        songs[songpos].Volume := round(100 * ScbVolume.Percentage);
        songs[songpos].Play;
      end
    else if songs[songpos].Paused then
        songs[songpos].Resume
    else
        songs[songpos].Pause;
    stopped := False;
end;

procedure TWndMain.goPrevTrack(Sender: TObject);
begin
    if is_random_track then
      begin
        if stopped then
            trPlay(songs[songpos].PnNum)
        else
            songs[songpos].Stop;
      end
    else if endless then
      begin
        if (songpos > 0) then
            trPlay(songs[songpos - 1].PnNum)
        else
            trPlay(songs.Last.PnNum);
      end
    else
        trPlay(songs[songpos - 1].PnNum);
end;

procedure TWndMain.goSavePlaylist(Sender: TObject);
begin
    FrmFileDlg.ShowFileDialog(fdkSavePlaylist);
end;

procedure TWndMain.goSetEndless(Sender: TObject);
begin
    endless := not endless;
    SetCheckedPanel(CbContinuous, endless);
    if is_random_track then
        goSetRandomizeTracks(Sender);
end;

procedure TWndMain.goSetRandomizeTracks(Sender: TObject);
begin
    is_random_track := not is_random_track;
    SetCheckedPanel(CbRandom, is_random_track);
    if endless then
        goSetEndless(Sender);
end;

procedure TWndMain.goSetSkipTrack(Sender: TObject);
begin
    songs[skp].SkipIt := not songs[skp].SkipIt;
    SetCheckedPanel(CbSkip, songs[skp].SkipIt);
    PnSkip.Hide;
end;

procedure TWndMain.goShortcutPressed(var Msg: TLMKey; var Handled: boolean);

    procedure __kVolDn;
    begin
        if PnMain.Enabled then
            ScbVolume.ScrollLeft;
    end;

    procedure __kVolUp;
    begin
        if PnMain.Enabled then
            ScbVolume.ScrollRight;
    end;

    procedure __kTrPrev;
    begin
        if (songs.Count > 0) and PnMain.Enabled then
          begin
            if (songpos > 0) or endless or is_random_track then
                goPrevTrack(BtnPrev)
            else
                songs[songpos].PositionSec := 0;
          end;
    end;

    procedure __kTrNext;
    begin
        if (songs.Count > 0) and PnMain.Enabled then
          begin
            if (songpos < songs.Count - 1) or endless or is_random_track then
                goNextTrack(BtnNext)
            else
                songs[songpos].PositionSec := 0;
          end;
    end;

    procedure __kTrStop;
    begin
        if PnMain.Enabled and (songs.Count > 0) and not stopped then
            goStop(BtnStop);
    end;

    procedure __kTrPR;
    begin
        if PnMain.Enabled and (songs.Count > 0) and not stopped then
            goPlay(BtnPlay);
    end;

    procedure __kTrPlay;
    begin
        if PnMain.Enabled and (songs.Count > 0) then
          begin
            if stopped then
                goPlay(BtnPlay)
            else
                songs[songpos].PositionSec := 0;
          end;
    end;

    procedure __kTrDel;
    begin
        if (songs.Count > 0) and PnMain.Enabled then
          begin
            trDelete(songs[songpos].BtnDel);
            PnSkip.Hide;
          end;
    end;

var
    i: integer;
begin
    if DlgSet.Visible and DlgSet.IsWaitingForKey and DlgSet.Enabled then
        DlgSet.SendKey(Msg.CharCode)
    else
      begin
        case Msg.CharCode of
            VK_ESCAPE:
              begin
                if PnSkip.Visible then
                    PnSkip.Hide
                else if PnTextCCP.Visible then
                    PnTextCCp.Hide
                else if MsgBox.Visible then
                    MsgBox.Hide
                else if QuestionDlg.Visible then
                    QuestionDlg.Hide
                else if DlgJump.Visible then
                    DlgJump.Hide
                else if DlgSpeed.Visible then
                    DlgSpeed.Hide
                else if FrmFileDlg.Visible then
                    FrmFileDlg.Hide
                else if DlgSet.ColorDialog.Visible then
                    DlgSet.ColorDialog.Hide
                else if DlgSet.Visible then
                    DlgSet.Hide
                else if DlgExtInf.Visible then
                    DlgExtInf.Hide
                else if not pl_abort then
                    pl_abort := True;
              end;
            VK_RETURN:
              begin
                if MsgBox.Visible then
                    MsgBox.Hide
                else if QuestionDlg.Visible then
                    QuestionDlg.Accept
                else if DlgJump.Visible then
                    DlgJump.Accept
                else if DlgSpeed.Visible then
                    DlgSpeed.Accept
                else if FrmFileDlg.Visible then
                    FrmFileDlg.Accept
                else if DlgSet.ColorDialog.Visible then
                    DlgSet.ColorDialog.Accept
                else if DlgSet.Visible then
                    DlgSet.Accept
                else if DlgExtInf.Visible then
                    DlgExtInf.Hide
                else
                    for i := 0 to High(ProgramSettings.Keys) do
                        if ProgramSettings.Keys[i] = VK_RETURN then
                          begin
                            case i of
                                0: __kVolDn;
                                1: __kVolUp;
                                2: __kTrPrev;
                                3: __kTrNext;
                                4: __kTrStop;
                                5: __kTrPR;
                                6: __kTrPlay;
                                7: __kTrDel;
                              end;
                            break;
                          end;
              end;
            else
                for i := 0 to High(ProgramSettings.Keys) do
                    if ProgramSettings.Keys[i] = Msg.CharCode then
                      begin
                        case i of
                            0: __kVolDn;
                            1: __kVolUp;
                            2: __kTrPrev;
                            3: __kTrNext;
                            4: __kTrStop;
                            5: __kTrPR;
                            6: __kTrPlay;
                            7: __kTrDel;
                          end;
                        break;
                      end;
          end;
      end;
end;

procedure TWndMain.goShowExtInfo(Sender: TObject);
begin
    if (songs.Count > 0) and ProgramSettings.UseExtID3Info then
      begin
        if songs[songpos].HasID3Tag and not IsEmptyStr(songs[songpos].ExtendedID3, [' ', #9]) then
          DlgExtInf.ShowDialog('Track extended info', songs[songpos].ExtendedID3, songs[songpos].ID3Picture)
        else
            MsgBox.ShowDialog('No extended info', 'This track has no extended information.');
      end;
end;

procedure TWndMain.goShowSettings(Sender: TObject);
begin
    DlgSet.Show;
end;

procedure TWndMain.goShowSummary(Sender: TObject);
var
    i, h, m, s, skh, skm, sks, shh, shm, shs, rh, rm, rs, slen, sfslen, skslen, rlen, sht, skt, rt: integer;
    strTotal, strSkipped, strShufOut, strRemaining: string;
begin
    if (songs.Count > 1) and not stopped and is_random_track then
      begin
        slen := 0; {Total song length}
        sfslen := 0; {Total shuffled-out song length}
        skslen := 0; {Total skipped song length}
        skt := 0; {Total skipped songs}
        sht := 0; {Total shuffled-out songs}
        rt := songs.Count;  {Total remaining songs}
        for i := 0 to songs.Count - 1 do
          begin
            Inc(slen, songs[i].LengthSec);
            if songs[i].Shuffled then
              begin
                Inc(sfslen, songs[i].LengthSec);
                Inc(sht);
                Dec(rt);
              end;
            if songs[i].SkipIt then
              begin
                Inc(skslen, songs[i].LengthSec);
                Inc(skt);
                Dec(rt);
              end;
          end;
        if (skt < songs.Count) then
            rlen := slen - sfslen - skslen
        else
          begin
            rt := songs.Count - sht;
            rlen := slen - sfslen;
          end;
        if (rlen < 0) then
            rlen := 0;
        {Total length}
        h := slen div 3600;
        m := (slen - (h * 3600)) div 60;
        s := slen - (h * 3600) - (m * 60);
        {Total skipped length}
        skh := skslen div 3600;
        skm := (skslen - (skh * 3600)) div 60;
        sks := skslen - (skh * 3600) - (skm * 60);
        {Total shuffled-out length}
        shh := sfslen div 3600;
        shm := (sfslen - (shh * 3600)) div 60;
        shs := sfslen - (shh * 3600) - (shm * 60);
        {Total remaining (not skipped, not shuffled-out)}
        rh := rlen div 3600;
        rm := (rlen - (rh * 3600)) div 60;
        rs := rlen - (rh * 3600) - (rm * 60);
        {Text out}
        strTotal := Format('Total length: %.2d:%.2d:%.2d in %d tracks', [h, m, s, songs.Count]);
        if ProgramSettings.AllowSkipTracks then
          begin
            if (skslen > 0) and (skt > 0) then
              begin
                if (skt > 1) then
                  begin
                    if (skt < songs.Count) then
                        strSkipped := Format('Total skipped length: %.2d:%.2d:%.2d in %d tracks', [skh, skm, sks, skt])
                    else
                        strSkipped := 'All tracks are marked to be skipped! Ignoring!';
                  end
                else
                    strSkipped := Format('Total skipped length: %.2d:%.2d:%.2d in 1 track', [skh, skm, sks]);
              end
            else
                strSkipped := 'No tracks are marked to be skipped.';
          end
        else
            strSkipped := 'Skipping tracks is not allowed.';
        if (sht > 1) then
            strShufOut := Format('Total shuffled-out length: %.2d:%.2d:%.2d in %d tracks', [shh, shm, shs, sht])
        else
            strShufOut := Format('Total shuffled-out length: %.2d:%.2d:%.2d in 1 track', [shh, shm, shs]);
        if (rlen > 0) and (rt > 0) then
          begin
            if (rt > 1) then
                strRemaining := Format('Total remaining length: %.2d:%.2d:%.2d in %d tracks', [rh, rm, rs, rt])
            else
                strRemaining := Format('Total remaining length: %.2d:%.2d:%.2d in 1 track', [rh, rm, rs]);
          end
        else
            strRemaining := 'No more tracks remaining until next shuffle.';
        MsgBox.ShowDialog('Shuffle summary', Format('%s'#13#10'%s'#13#10'%s'#13#10'%s', [strTotal, strSkipped, strShufOut, strRemaining]));
      end;
end;

procedure TWndMain.goStartSlidePos(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
    m_pos := Button = mbLeft;
    if m_pos then
      begin
        p_x := X;
        PnPlayPosSlider.Cursor := crGrabbed;
      end;
end;

procedure TWndMain.goStop(Sender: TObject);
begin
    songs[songpos].Stop;
    stopped := True;
end;

procedure TWndMain.goTryCloseWnd(Sender: TObject; var CanClose: boolean);
begin
    if PnSave.Visible then
      begin
        QuestionDlg.ShowDialog('Ongoing: Saving playlist', 'Playlist saving is in progress! Close anyway?');
        repeat
            Application.ProcessMessages;
        until not QuestionDlg.Visible;
        CanClose := QuestionDlg.DialogResult > 0;
        pl_abort := CanClose;
      end
    else if PnLoad.Visible then
      begin
        QuestionDlg.ShowDialog('Ongoing: Loading playlist', 'Abort loading, and exit?');
        repeat
            Application.ProcessMessages;
        until not QuestionDlg.Visible;
        CanClose := QuestionDlg.DialogResult > 0;
        pl_abort := CanClose;
      end
    else
        CanClose := True;
end;

{
***************************************
*********** Private methods ***********
***************************************
}

procedure TWndMain.scScrollVolume(Sender: TObject);
var
    i: integer;
begin
    for i := 0 to songs.Count - 1 do
        songs[i].Volume := round(100 * ScbVolume.Percentage);
end;

procedure TWndMain.scScrollTrackInfo(Sender: TObject);
begin
    LbTrackInfo.Left := - round(ScbTrackInfo.Percentage * ScbTrackInfo.Max) + 8;
end;

procedure TWndMain.scScrollTracks(Sender: TObject);
begin
    trReOrder;
end;

procedure TWndMain.trReOrder;
var
    st, i: integer;
begin
    st := -round(20 * ScbTracks.Max * ScbTracks.Percentage);
    for i := 0 to songs.Count - 1 do
      begin
        songs[i].Top := st + (i * 20);
        songs[i].ResetCaption(i);
        songs[i].BtnUp.Enabled := i > 0;
        songs[i].BtnDown.Enabled := (i < (songs.Count - 1));
      end;
    wnduRecolorForm;
end;

procedure TWndMain.trMoveUp(Sender: TObject);
var
    p: TFrmSong;
    i: integer;
begin
    check_enabled := False;
    p := (Sender as TPanel).Parent as TFrmSong;
    for i := 0 to songs.Count - 1 do
        if (songs[i] = p) then
            break;
    if (i > 0) then
      begin
        songs.Exchange(i, i - 1);
        if (songpos = i) then
            Dec(songpos)
        else if (songpos = (i - 1)) then
            Inc(songpos);
      end;
    trReOrder;
    check_enabled := True;
end;

procedure TWndMain.trMoveDown(Sender: TObject);
var
    p: TFrmSong;
    i: integer;
begin
    check_enabled := False;
    p := (Sender as TPanel).Parent as TFrmSong;
    for i := 0 to songs.Count - 1 do
        if (songs[i] = p) then
            break;
    if (i < (songs.Count - 1)) then
      begin
        songs.Exchange(i, i + 1);
        if (songpos = i) then
            Inc(songpos)
        else if (songpos = (i + 1)) then
            Dec(songpos);
      end;
    trReOrder;
    check_enabled := True;
end;

procedure TWndMain.trDelete(Sender: TObject);
var
    p: TFrmSong;
    i: integer;
begin
    check_enabled := False;
    p := (Sender as TPanel).Parent as TFrmSong;
    if (songs.Count = 1) then
      begin
        stopped := True;
        p.Stop;
        p.Hide;
      end
    else
      begin
        for i := 0 to songs.Count - 1 do
            if (songs[i] = p) then
                break;
        if (songpos = i) then
          begin
            goStop(p);
            if (songpos > 0) then
                Dec(songpos);
          end
        else if (songpos > i) then
            Dec(songpos);
        if (songs.Count > 20) then
          begin
            ScbTracks.Max := ScbTracks.Max - 1;
            // HACK: vert sb doesn't auto-adjust slider thumb
            if (ScbTracks.ScDrag.Top > (ScbTracks.PnSlider.Height - ScbTracks.ScDrag.Height)) then
                ScbTracks.ScDrag.Top := ScbTracks.PnSlider.Height - ScbTracks.ScDrag.Height;
          end;
      end;
    songs.Remove(p);
    sf.Add(p);  // HACK: free memory after this event handler is executed!
    trReOrder;
    check_enabled := True;
end;

procedure TWndMain.trPlay(Sender: TObject);
var
    i: integer;
    p: TFrmSong;
begin
    check_enabled := False;
    p := (Sender as TPanel).Parent as TFrmSong;
    if (p = songs[songpos]) and not songs[songpos].Stopped then
        songs[songpos].PositionSec := 0
    else
      begin
        for i := 0 to songs.Count - 1 do
            songs[i].Stop;
        for i := 0 to songs.Count - 1 do
            if (songs[i] = p) then
                break;
        songpos := i;
        if songs[songpos].Stopped then
          begin
            songs[songpos].Volume := round(100 * ScbVolume.Percentage);
            songs[songpos].Play;
            stopped := False;
          end;
      end;
    check_enabled := True;
end;

procedure TWndMain.trChangeSpeed(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
    i: integer;
begin
    trEndDrag(Sender, Button, Shift, X, Y);
    if (Button = mbRight) then
        for i := 0 to songs.Count - 1 do
            if ((Sender as TPanel).Parent = songs[i]) then
              begin
                DlgSpeed.ShowDialog(songs[i]);
                break;
              end;
end;

procedure TWndMain.trSkip(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
    i: integer;
    p: TPoint;
begin
    trEndDrag(Sender, Button, Shift, X, Y);
    if (Button = mbRight) and ProgramSettings.AllowSkipTracks and is_random_track then
      begin
        for i := 0 to songs.Count - 1 do
            if ((Sender as TPanel).Parent = songs[i]) then
              begin
                skp := i;
                break;
              end;
        SetCheckedPanel(CbSkip, songs[skp].SkipIt);
        p := PnMain.ScreenToClient(Mouse.CursorPos);
        PnSkip.Left := p.x - (PnSkip.Width div 2);
        PnSkip.Top := p.y;
        PnSkip.Show;
      end;
end;

procedure TWndMain.trStartDrag(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
    i: integer;
begin
    if (Button <> mbLeft) then
        Exit;
    for i := 0 to songs.Count - 1 do
        if (songs[i] = (Sender as TPanel).Parent) then
          begin
            sdr := i;
            break;
          end;
    m_song := True;
    TmrDragSongCursorSet.Enabled := True;
    { Cursor setting has its own timer, to achieve a more friendly effect }
end;

procedure TWndMain.trDrag(Sender: TObject; Shift: TShiftState; X, Y: integer);
begin
    { Dragging is only possible, after the timer has processed, and cursor is set }
    if (not m_song) or TmrDragSongCursorSet.Enabled then
        Exit;
    Y := PnPlaylist.ScreenToClient(songs[sdr].ClientToScreen(Point(X, Y))).y;
    if (Y < songs[sdr].Top) and (sdr > 0) then
      begin
        trMoveUp(songs[sdr].BtnUp);
        Dec(sdr);
      end
    else if (Y > (songs[sdr].Top + songs[sdr].Height)) and (sdr < songs.Count - 1) then
      begin
        trMoveDown(songs[sdr].BtnDown);
        Inc(sdr);
      end;
end;

procedure TWndMain.trEndDrag(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
    i: integer;
begin
    if not m_song then
        Exit;
    m_song := False;
    for i := 0 to songs[sdr].ComponentCount - 1 do
        if (songs[sdr].Components[i] is TControl) then
            (songs[sdr].Components[i] as TControl).Cursor := crDefaultCursor;
end;

procedure TWndMain.wnduCheckForm(form: TScrollingWinControl);
var
    i: integer;
    p: TPanel;
    e: TEdit;
    c: TControl;
begin
    if (form is TForm) or (form is TFrame) then
      begin
        if (form.Cursor = crDefault) then
            form.Cursor := crDefaultCursor;
        for i := 0 to form.ComponentCount - 1 do
          begin
            if (form.Components[i] is TForm) or (form.Components[i] is TFrame) then
                wnduCheckForm(form.Components[i] as TScrollingWinControl)
            else if (form.Components[i] is TControl) then
              begin
                c := form.Components[i] as TControl;
                if (c is TEdit) then
                  begin
                    e := c as TEdit;
                    if not Assigned(e.OnContextPopup) then
                        e.OnContextPopup := @pmCustomPopup;
                    if (e.Cursor = crDefault) then
                        e.Cursor := crIBeam; // we must set it, to see the custom I-Beam cursor
                  end
                else
                  begin
                    if (c is TPanel) then
                      begin
                        p := c as TPanel;
                        if AnsiContainsStr(p.HelpKeyword, 'X') then
                          begin
                            if not Assigned(p.OnMouseDown) then
                                p.OnMouseDown := @g_EventSvc.g_PressButton;
                            if not Assigned(p.OnMouseUp) then
                                p.OnMouseUp := @g_EventSvc.g_ReleaseButton;
                          end;
                        if not Assigned(p.OnPaint) then
                            p.OnPaint := @g_EventSvc.g_PaintElement;
                        p.DoubleBuffered := True;
                      end;
                    if (c.Cursor = crDefault) then
                        c.Cursor := crDefaultCursor;
                  end;
              end;
          end;
      end;
end;

procedure TWndMain.wnduRecolorForm;

    procedure _RecolorForm(form: TScrollingWinControl);
    {%region /fold}
    var
        i: integer;
        p: TPanel;
    begin
        for i := 0 to form.ComponentCount - 1 do
          begin
            if (form.Components[i] is TForm) or (form.Components[i] is TFrame) then
                _RecolorForm(form.Components[i] as TScrollingWinControl)
            else if (form.Components[i] is TPanel) then
              begin
                p := form.Components[i] as TPanel;
                if AnsiContainsStr(p.HelpKeyword, 'e') then
                    SetPanelColor(p, ProgramSettings.Colors[7])
                else if AnsiContainsStr(p.HelpKeyword, 't') then
                    SetPanelColor(p, ProgramSettings.Colors[1])
                else if AnsiContainsStr(p.HelpKeyword, 'b') then
                    SetPanelColor(p, ProgramSettings.Colors[2])
                else if AnsiContainsStr(p.HelpKeyword, 'd') then
                    SetPanelColor(p, ProgramSettings.Colors[15])
                else if AnsiContainsStr(p.HelpKeyword, 'p') or AnsiContainsStr(p.HelpKeyword, 'm') then
                    SetPanelColor(p, ProgramSettings.Colors[8])
                else if AnsiContainsStr(p.HelpKeyword, 'c') then
                    SetPanelColor(p, ProgramSettings.Colors[14]);
              end;
          end;
    end;

    {%endregion}

begin
    _RecolorForm(self);
    Font.Color := RGBToColor(ProgramSettings.Colors[5].R, ProgramSettings.Colors[5].G, ProgramSettings.Colors[5].B);
    Color := RGBToColor(ProgramSettings.Colors[0].R, ProgramSettings.Colors[0].G, ProgramSettings.Colors[0].B);
    g_EventSvc.ButtonColor := RGBToColor(ProgramSettings.Colors[3].R, ProgramSettings.Colors[3].G, ProgramSettings.Colors[3].B);
    g_EventSvc.ButtonDisabledColor := RGBToColor(ProgramSettings.Colors[4].R, ProgramSettings.Colors[4].G, ProgramSettings.Colors[4].B);
end;

procedure TWndMain.musLoad(mfile: string);
var
    song: TFrmSong;
    i: integer;
    p: TPanel;
    fmt: string;
begin
    if (ScbTracks.ScDrag.Height < 16) then // HACK: no function to check it otherwise
      begin
        MsgBox.ShowDialog('Info', 'Tracklist full!');
        pl_abort := True;
        Screen.Cursor := crDefault;
        Exit;
      end;
    song := TFrmSong.Create(mfile);
    if song.IsValid then
      begin
        wnduCheckForm(song);
        for i := 0 to song.ComponentCount - 1 do
            if (song.Components[i] is TPanel) then
              begin
                p := song.Components[i] as TPanel;
                if not Assigned(p.OnMouseWheel) then
                    p.OnMouseWheel := @ScbTracks.goScroll;
              end;
        with song do
          begin
            Left := 1;
            Volume := round(100 * ScbVolume.Percentage);
            PnNum.OnDblClick := @trPlay;
            PnNum.OnMouseDown := @trStartDrag;
            PnNum.OnMouseMove := @trDrag;
            PnNum.OnMouseUp := @trSkip;
            PnName.OnDblClick := @trPlay;
            PnName.OnMouseDown := @trStartDrag;
            PnName.OnMouseMove := @trDrag;
            PnName.OnMouseUp := @trEndDrag;
            PnLength.OnDblClick := @trPlay;
            PnLength.OnMouseDown := @trStartDrag;
            PnLength.OnMouseMove := @trDrag;
            PnLength.OnMouseUp := @trChangeSpeed;
            BtnUp.OnClick := @trMoveUp;
            BtnDown.OnClick := @trMoveDown;
            BtnDel.OnClick := @trDelete;
            Parent := PnPlaylist;
            Show;
          end;
        songs.Add(song);
        if (songs.Count > 20) then
            ScbTracks.Max := ScbTracks.Max + 1;
        trReOrder;
      end
    else
      begin
        fmt := Format('File ''%s'' is invalid!', [mfile]);
        if (FilesToLoad.Count = 1) then
            MsgBox.ShowDialog('Error', fmt)
        else if (DlgExtInf.LbExtInfo.Caption <> '') then
            DlgExtInf.LbExtInfo.Caption := DlgExtInf.LbExtInfo.Caption + #13#10 + fmt
        else
            DlgExtInf.LbExtInfo.Caption := fmt;
        song.Free;
      end;
    trReOrder;
end;

procedure TWndMain.evtMusLoad(Sender: TObject);
var
    i: integer;
begin
    pl_abort := False;
    for i := 0 to FrmFileDlg.SelectedFiles.Count - 1 do
        FilesToLoad.Add(FrmFileDlg.SelectedFiles[i]);
end;

procedure TWndMain.evtListLoad(Sender: TObject);
var
    lst: TPlaylist;
    i, j: integer;
begin
    pl_abort := False;
    for i := 0 to FrmFileDlg.SelectedFiles.Count - 1 do
      begin
        LoadPlaylist(lst, FrmFileDlg.SelectedFiles[i]);
        for j := 0 to High(lst.Files) do
            FilesToLoad.Add(lst.Files[j].FileName);
      end;
end;

procedure TWndMain.evtListSave(Sender: TObject);
var
    i: integer;
    lst: TPlaylist;
    n, s, dn: string;
begin
    pl_abort := False;
    if not IsPlaylist(FrmFileDlg.SelectedFiles[0]) then
      begin
        MsgBox.ShowDialog('Playlist', Format('''%s'' is not a playlist!', [FrmFileDlg.SelectedFiles[0]]));
        Exit;
      end;
    lst.IsExtended := ProgramSettings.PlaylistUseExtFmt;
    SetLength(lst.Files, songs.Count);
    MaxSave := songs.Count - 1;
    PnSave.Show;
    if ProgramSettings.CopyMusicOnSavePlaylist then
      begin
        dn := Format('%s\%s', [ExtractFilePath(FrmFileDlg.SelectedFiles[0]), ChangeFileExt(ExtractFileName(FrmFileDlg.SelectedFiles[0]), '')]);
        if not DirectoryExists(dn) then
            CreateDir(dn);
      end;
    for i := 0 to songs.Count - 1 do
      begin
        if ProgramSettings.CopyMusicOnSavePlaylist then
          begin
            n := ExtractRelativePath(FrmFileDlg.SelectedFiles[0], songs[i].FileName);
            s := IncludeTrailingPathDelimiter(dn) + ExtractFileName(n);
            if not FileExists(s) then
                FileUtil.CopyFile(songs[i].FileName, s, [cffOverwriteFile, cffPreserveTime]);
            s := ExtractRelativePath(FrmFileDlg.SelectedFiles[0], s);
            lst.Files[i].FileName := ReverseString(ExcludeTrailingPathDelimiter(ReverseString(s)));
          end
        else
            lst.Files[i].FileName := ExtractRelativePath(FrmFileDlg.SelectedFiles[0], songs[i].FileName);
        lst.Files[i].LengthSec := songs[i].OriginalLengthSec;
        lst.Files[i].TrackName := songs[i].PnName.Caption;
        CurrentSaveStep := i;
        PbSave.Invalidate;
        Application.ProcessMessages;
        if pl_abort then
            break;
      end;
    if not pl_abort then
        SavePlaylist(lst, FrmFileDlg.SelectedFiles[0]);
    PnSave.Hide;
end;

procedure TWndMain.evtSettingsChanged(Sender: TObject);
begin
    wnduRecolorForm;
end;

procedure TWndMain.evtPlaylistError(Error: string);
begin
    pl_abort := True;
    MsgBox.ShowDialog('Playlist error', Error);
end;

procedure TWndMain.pmCustomPopup(Sender: TObject; MousePos: TPoint; var Handled: boolean);
var
    p: TPoint;
begin
    Handled := True; // No default popup menu
    Fselectedtxt := Sender as TEdit;
    ActiveControl := Fselectedtxt;
    PnTextCopy.Enabled := Fselectedtxt.SelLength > 0;
    PnTextCut.Enabled := Fselectedtxt.SelLength > 0;
    PnTextPaste.Enabled := Clipboard.HasFormat(CF_TEXT);
    PnTextSelectAll.Enabled := Fselectedtxt.SelLength < Length(Fselectedtxt.Text);
    p := PnMain.ScreenToClient(Mouse.CursorPos);
    PnTextCCP.Left := p.x - (PnTextCCP.Width div 2);
    PnTextCCP.Top := p.y;
    wnduRecolorForm; // Force recolor!
    PnTextCCP.Show;
end;


initialization
    SetPriorityClass(GetCurrentProcess, $8000);
  { If the program was started using an associated music file, then
    the working directory is changed to that dir, by the RTL.
    That's fixed using this code - config files need to be located
    BEFORE they're used }
    GetModuleFileName(GetModuleHandle(nil), argv[0], 32767);
    SetCurrentDir(ExtractFilePath(ParamStr(0)));
    randomize;

end.
