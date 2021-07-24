unit c_files;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, LazFileUtils, Forms, Controls, ExtCtrls, StdCtrls,
    Graphics, LCLType,
    c_vertsb, c_fileprops, Types, strutils, crDefs, MpjPlaylists, c_song, c_settings;

type
    TFileDialogKind = (fdkLoadMusic, fdkLoadPlaylist, fdkSavePlaylist, fdkInvalid);

    TFilterArray = array of record
        ExtList: array of string;
        Panel: TFrmFile;
    end;

    { TFrmFileDialogMPJ }

    TFrmFileDialogMPJ = class(TFrame)
        BtnAccept: TPanel;
        BtnCancel: TPanel;
        BtnDown: TPanel;
        BtnDrives: TPanel;
        BtnEnter: TPanel;
        BtnFilter: TPanel;
        BtnUp: TPanel;
        ScbFiles: TFrmVertSb;
        PnDrives: TPanel;
        PnFile: TPanel;
        PnFilelist: TPanel;
        PnDirName: TPanel;
        PnFilter: TPanel;
        PnFilterSelect: TPanel;
        PnFilterValue: TPanel;
        PnFooter: TPanel;
        PnHeader: TPanel;
        PnOutFileName: TPanel;
        PnTitle: TPanel;
        TxtDirName: TEdit;
        TxtFileName: TEdit;
        procedure goAccept(Sender: TObject);
        procedure goChangeDirByName(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure goClose(Sender: TObject);
        procedure goChangeDir(Sender: TObject);
        procedure goListDrives(Sender: TObject);
        procedure goChangeFileName(Sender: TObject);
        procedure goShowFilter(Sender: TObject);
    private
        m_div: byte;
        Extensions: TFilterArray;
        Flistidx, Fmusidx: integer;
        Drives, Files: TFileList;
        Fselected: TStrings;
        FKind: TFileDialogKind;
        Fonacceptloadfiles, Fonacceptloadlist, Fonacceptsavelist: TNotifyEvent;
        Fcurrentdir: string;
        function GetFileName: string;
        function GetSelectedFiles: TStrings;
        procedure scScrollFiles(Sender: TObject);
        procedure flReOrder;
        procedure flSelectFilter(Sender: TObject);
        procedure flEnterDir(Sender: TObject);
        procedure flSelectDir(Sender: TObject);
        procedure flSelectFile(Sender: TObject);
        procedure RereadCurrentDir;
    public
        constructor Create(AOwner: TComponent; dir: string);
        destructor Destroy; override;
        procedure ShowFileDialog(Kind: TFileDialogKind);
        procedure Accept;
        procedure Show;
    public
        property SelectedFiles: TStrings read GetSelectedFiles;
        property OnAcceptLoadFiles: TNotifyEvent read Fonacceptloadfiles write Fonacceptloadfiles;
        property OnAcceptLoadList: TNotifyEvent read Fonacceptloadlist write Fonacceptloadlist;
        property OnAcceptSaveList: TNotifyEvent read Fonacceptsavelist write Fonacceptsavelist;
    end;

implementation

{$R *.lfm}

{ TFrmFileDialogMPJ }

{
***************************************
** Published (event handler) methods **
***************************************
}

{%region /fold}

procedure TFrmFileDialogMPJ.goAccept(Sender: TObject);
var
    evt: TNotifyEvent = nil;
begin
    Hide;
    case FKind of
        fdkLoadMusic: evt := Fonacceptloadfiles;
        fdkLoadPlaylist: evt := Fonacceptloadlist;
        fdkSavePlaylist: evt := Fonacceptsavelist;
      end;
    if (evt <> nil) and (FKind <> FdkInvalid) then
        evt(Self);
end;

procedure TFrmFileDialogMPJ.goChangeDirByName(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if (Key = VK_RETURN) and DirectoryExists(TxtDirName.Text) then
        goChangeDir(Sender);
end;

procedure TFrmFileDialogMPJ.goClose(Sender: TObject);
begin
    Hide;
end;

procedure TFrmFileDialogMPJ.goChangeDir(Sender: TObject);
begin
    PnDrives.Hide;
    PnFilterSelect.Hide;
    Fcurrentdir := TxtDirName.Text;
    RereadCurrentDir;
end;

procedure TFrmFileDialogMPJ.goListDrives(Sender: TObject);
var
    dl: char;
    driveletter: string;
    drive: TFrmFile;
begin
    PnFilterSelect.Hide;
    if PnDrives.Visible then
        PnDrives.Hide
    else
      begin
        while (Drives.Count > 0) do
          begin
            Drives.Last.Hide;
            Drives.Last.Free;
            Drives.Remove(Drives.Last);
          end;
        for dl := 'A' to 'Z' do
          begin
            driveletter := Format('%s:\', [dl]);
            if DirectoryExists(driveletter) then
              begin
                drive := TFrmFile.Create(nil, False, driveletter);
                with drive do
                  begin
                    Width := PnDrives.Width - 2;
                    Top := Drives.Count * 20;
                    Left := 1;
                    PnText.OnClick := @flEnterDir;
                    SelectedColor := RGBToColor(ProgramSettings.Colors[8].R, ProgramSettings.Colors[8].G, ProgramSettings.Colors[8].B);
                    IsSelected := AnsiStartsText(dl, Fcurrentdir);
                    Parent := PnDrives;
                    Show;
                  end;
                Drives.Add(drive);
              end;
          end;
        PnDrives.Height := (Drives.Count * 20) + 1;
        PnDrives.Show;
      end;
end;

procedure TFrmFileDialogMPJ.goChangeFileName(Sender: TObject);
begin
    BtnAccept.Enabled := FileExistsUTF8(GetFileName) or (GetSelectedFiles.Count > 0) or (FKind = fdkSavePlaylist);
end;

procedure TFrmFileDialogMPJ.goShowFilter(Sender: TObject);
var
    isfilterselect: boolean;
begin
    PnDrives.Hide;
    isfilterselect := (Sender = PnFilterValue) or (Sender = BtnFilter);
    if PnFilterSelect.Visible then
        PnFilterSelect.Hide
    else
        PnFilterSelect.Visible := isfilterselect;
end;

{%endregion}

{
***************************************
******** Private methods **************
***************************************
}

{%region /fold}

function TFrmFileDialogMPJ.GetFileName: string;
begin
    Result := IncludeTrailingBackslash(Fcurrentdir) + TxtFileName.Text;
end;

function TFrmFileDialogMPJ.GetSelectedFiles: TStrings;
var
    i: integer;
begin
    Fselected.Clear;
    for i := 0 to Files.Count - 1 do
        if Files[i].IsSelected then
            Fselected.Add(IncludeTrailingBackslash(Fcurrentdir) + Files[i].FileName);
    if (Fselected.Count = 0) and (FileExists(GetFileName) or (FKind = fdkSavePlaylist)) then
        Fselected.Add(GetFileName);
    Result := Fselected;
end;

procedure TFrmFileDialogMPJ.scScrollFiles(Sender: TObject);
begin
    flReOrder;
end;

procedure TFrmFileDialogMPJ.flReOrder;
var
    st, i: integer;
begin
    st := -round(20 * ScbFiles.Max * ScbFiles.Percentage);
    for i := 0 to Files.Count - 1 do
        Files[i].Top := st + (i * 20);
end;

procedure TFrmFileDialogMPJ.flSelectFilter(Sender: TObject);
var
    filter: TFrmFile;
    i: integer;
begin
    PnDrives.Hide;
    PnFilterSelect.Hide;
    filter := (Sender as TPanel).Parent as TFrmFile;
    for i := 0 to High(Extensions) do
      begin
        Extensions[i].Panel.IsSelected := filter = Extensions[i].Panel;
        Extensions[i].Panel.ParentColor := not Extensions[i].Panel.IsSelected;
        if (filter = Extensions[i].Panel) then
          begin
            if (Fkind = fdkLoadMusic) then
                Fmusidx := i
            else
                Flistidx := i;
          end;
      end;
    PnFilterValue.Caption := filter.PnText.Caption;
    RereadCurrentDir;
end;

procedure TFrmFileDialogMPJ.flEnterDir(Sender: TObject);
var
    dir: TFrmFile;
begin
    PnDrives.Hide;
    PnFilterSelect.Hide;
    dir := (Sender as TPanel).Parent as TFrmFile;
    if (dir.FileName = '..') then
      begin
        TxtDirName.Text := ExtractFilePath(ExcludeTrailingPathDelimiter(Fcurrentdir));
        goChangeDir(Sender);
      end
    else if AnsiEndsStr(':\', dir.FileName) then
      begin
        TxtDirName.Text := dir.FileName;
        goChangeDir(Sender);
      end
    else if DirectoryExists(IncludeTrailingBackslash(Fcurrentdir) + dir.FileName) then
      begin
        TxtDirName.Text := IncludeTrailingBackslash(Fcurrentdir) + dir.FileName;
        goChangeDir(Sender);
      end;
end;

procedure TFrmFileDialogMPJ.flSelectDir(Sender: TObject);
begin
    PnDrives.Hide;
    PnFilterSelect.Hide;
    ((Sender as TPanel).Parent as TFrmFile).IsSelected := False;
end;

procedure TFrmFileDialogMPJ.flSelectFile(Sender: TObject);
var
    shift: TShiftState;
    idx1, idx2, i: integer;
    f: TFrmFile;
begin
    PnDrives.Hide;
    PnFilterSelect.Hide;
    f := (Sender as TPanel).Parent as TFrmFile;
    TxtFileName.Text := f.FileName;
    if (FKind = fdkSavePlaylist) then
      begin
        for i := 0 to Files.Count - 1 do
            Files[i].IsSelected := False;
      end
    else
      begin
        shift := GetKeyShiftState;
        if (ssShift in shift) then
          begin
            idx1 := 0;
            for i := 0 to Files.Count - 1 do
                if (f = Files[i]) then
                  begin
                    idx1 := i;
                    break;
                  end;
            idx2 := idx1; // Just for safety
            if (idx1 > 0) then
                for i := idx1 - 1 downto 0 do
                    if Files[i].IsSelected then
                      begin
                        idx2 := i;
                        break;
                      end;
            for i := idx2 to idx1 do
                Files[i].IsSelected := True;
          end
        else if (ssCtrl in shift) then
            f.IsSelected := not f.IsSelected
        else
          begin
            for i := 0 to Files.Count - 1 do
                Files[i].IsSelected := (f = Files[i]);
          end;
      end;
end;

procedure TFrmFileDialogMPJ.RereadCurrentDir;

    procedure LinearSort(var a: TFileList);
    var
        i, j, start, dpos, fpos, k: integer;
        dup: boolean;
        tmp, GoParent: TFrmFile;
        da, fa: array of TFrmFile;
        s1, s2: string;
    begin
        GoParent := nil;
        if (a[0].FileName = '..') then
          begin
            start := 1;
            GoParent := a[0];
          end
        else
            start := 0;
        SetLength(da, 0);
        SetLength(fa, 0);
        dpos := 0;
        fpos := 0;
        {Put files & dirs to separate arrays}
        for i := start to a.Count - 1 do
          begin
            if (a[i].PnText.OnDblClick = @flEnterDir) then
              begin
                SetLength(da, Length(da) + 1);
                da[dpos] := a[i];
                Inc(dpos);
              end
            else
              begin
                SetLength(fa, Length(fa) + 1);
                fa[fpos] := a[i];
                Inc(fpos);
              end;
          end;
        while (a.Count > 0) do
            a.Remove(a.Last);
        {Levente Szabo-style Linear Sort algorithm, used with strings}
        {Sort dirs}
        for i := 0 to High(da) - 1 do
          begin
            for j := i + 1 to High(da) do
              begin
                s1 := da[i].PnText.Caption;
                s2 := da[j].PnText.Caption;
                k := Length(s1);
                if (Length(s2) < k) then
                    k := Length(s2);
                if (CompareText(Copy(s1, 1, k), Copy(s2, 1, k)) > 0) then
                  begin
                    tmp := da[j];
                    da[j] := da[i];
                    da[i] := tmp;
                  end;
              end;
          end;
        {Sort files}
        for i := 0 to High(fa) - 1 do
          begin
            for j := i + 1 to High(fa) do
              begin
                s1 := fa[i].PnText.Caption;
                s2 := fa[j].PnText.Caption;
                k := Length(s1);
                if (Length(s2) < k) then
                    k := Length(s2);
                if (CompareText(Copy(s1, 1, k), Copy(s2, 1, k)) > 0) then
                  begin
                    tmp := fa[j];
                    fa[j] := fa[i];
                    fa[i] := tmp;
                  end;
              end;
          end;
        {Put back the sorted arrays to source array}
        if (start = 1) then
            a.Add(GoParent);
        for i := 0 to High(da) do
            a.Add(da[i]);
        for i := 0 to High(fa) do
          begin
            // Check for duplicate files (can occur if eg. WAW/WAWE is used)
            dup := False;
            for j := 0 to a.Count - 1 do
                if (a[j].FileName = fa[i].FileName) then
                  begin
                    dup := True;
                    break;
                  end;
            if not dup then
                a.Add(fa[i]);
          end;
        {Clean up}
        SetLength(da, 0);
        SetLength(fa, 0);
        GoParent := nil;
        tmp := nil;
    end;

    function GetSelectedExtensionIndex: integer;
    var
        i: integer;
    begin
        for i := 0 to High(Extensions) do
            if Extensions[i].Panel.IsSelected then
                Exit(i);
        Result := 0;
    end;

var
    dp, fp: TSearchRec;
    i: integer;
    cf: string;
begin
    Screen.Cursor := crHourGlass;
    DisableAutoSizing;
    cf := IncludeTrailingBackslash(Fcurrentdir);
    while (Files.Count > 0) do
      begin
        Files.Last.Hide;
        Files.Last.Free;
        Files.Remove(Files.Last);
      end;
    if DirectoryExistsUTF8(Format('%s\..', [cf])) and not (Length(cf) < 4) then
      begin
        Files.Add(TFrmFile.Create(nil, True, '..'));
        with Files.Last do
          begin
            Left := 1;
            PnText.OnDblClick := @flEnterDir;
            PnText.OnClick := @flSelectDir;
            PnText.OnMouseWheel := @ScbFiles.goScroll;
          end;
      end;
    if (FindFirstUTF8(Format('%s\*', [cf]), faDirectory, dp) = 0) then
      begin
        repeat
            if not ((dp.Attr and faDirectory) = faDirectory) then
                continue;
            if (dp.Name = '.') or (dp.Name = '..') then
                continue;
            Files.Add(TFrmFile.Create(nil, True, dp.Name));
            with Files.Last do
              begin
                Left := 1;
                PnText.OnDblClick := @flEnterDir;
                PnText.OnClick := @flSelectDir;
                PnText.OnMouseWheel := @ScbFiles.goScroll;
              end;
        until (FindNextUTF8(dp) <> 0);
      end;
    LazFileUtils.FindCloseUTF8(dp);
    for i := 0 to High(Extensions[GetSelectedExtensionIndex].ExtList) do
      begin
        if (FindFirstUTF8(Format('%s\*.%s', [cf, Extensions[GetSelectedExtensionIndex].ExtList[i]]), faAnyFile, fp) = 0) then
          begin
            repeat
                Files.Add(TFrmFile.Create(nil, False, fp.Name));
                with Files.Last do
                  begin
                    Left := 1;
                    PnText.OnDblClick := @goAccept;
                    PnText.OnClick := @flSelectFile;
                    PnText.OnMouseWheel := @ScbFiles.goScroll;
                  end;
            until (FindNextUTF8(fp) <> 0);
          end;
        LazFileUtils.FindCloseUTF8(fp);
      end;
    if (Files.Count > 0) then
        LinearSort(Files);
    ScbFiles.Max := 0;
    ScbFiles.Percentage := 0;
    ScbFiles.Visible := ((Files.Count * 20) - (PnFilelist.Height div 20)) >= PnFilelist.Height;
    m_div := 1;
    for i := 0 to Files.Count - 1 do
      begin
        Files[i].Parent := PnFilelist;
        if ScbFiles.Visible then
          begin
            Files[i].Width := PnFilelist.Width - 2 - ScbFiles.Width;
            if ((i * 20) >= PnFilelist.Height) then
              begin
                ScbFiles.Max := ScbFiles.Max + 1;
                if (ScbFiles.ScDrag.Height < 16) then // HACK: this is the best solution
                  begin
                    Inc(m_div);
                    ScbFiles.Max := ScbFiles.Max div 2;
                  end;
              end;
          end
        else
            Files[i].Width := PnFilelist.Width - 2;
      end;
    flReOrder;
    EnableAutoSizing;
    Screen.Cursor := crDefault;
end;

{%endregion}

{
***************************************
******** Public methods ***************
***************************************
}

{%region /fold}

constructor TFrmFileDialogMPJ.Create(AOwner: TComponent; dir: string);
begin
    inherited Create(AOwner);
    if (dir <> '') then
        Fcurrentdir := dir
    else
        Fcurrentdir := GetCurrentDir;
    m_div := 1;
    Fonacceptloadfiles := nil;
    Fonacceptloadlist := nil;
    Fonacceptsavelist := nil;
    Drives := TFileList.Create;
    Files := TFileList.Create;
    SetLength(Extensions, 0);
    FSelected := TStringList.Create;
    Flistidx := 0;
    Fmusidx := 0;
    TxtDirName.Text := Fcurrentdir;
    ScbFiles.OnScroll := @scScrollFiles;
    PnFilelist.OnMouseWheel := @ScbFiles.goScroll;
end;

destructor TFrmFileDialogMPJ.Destroy;
var
    i: integer;
begin
    while (Drives.Count > 0) do
      begin
        Drives.Last.Hide;
        Drives.Last.Free;
        Drives.Remove(Drives.Last);
      end;
    Drives.Free;
    while (Files.Count > 0) do
      begin
        Files.Last.Hide;
        Files.Last.Free;
        Files.Remove(Files.Last);
      end;
    Files.Free;
    for i := 0 to High(Extensions) do
      begin
        SetLength(Extensions[i].ExtList, 0);
        if Assigned(Extensions[i].Panel) then
            Extensions[i].Panel.Free;
      end;
    SetLength(Extensions, 0);
    Fselected.Clear;
    Fselected.Free;
    inherited;
end;

procedure TFrmFileDialogMPJ.ShowFileDialog(Kind: TFileDialogKind);

    procedure GetSupportedMusicExtensions(var supported: TFilterArray);
    var
        i, j: integer;
        s: TStrings;
        ft, fs: string;
    begin
        SetLength(supported, Length(SupportedMusic));
        for i := 0 to High(SupportedMusic) do
          begin
              try
                ft := '';
                s := TStringList.Create;
                if AnsiContainsText(SupportedMusic[i], ':') then
                  begin
                    ExtractStrings([':'], [' ', #9], PChar(SupportedMusic[i]), s);
                    ft := s[0];
                    fs := s[1];
                    s.Clear;
                  end
                else
                    fs := SupportedMusic[i];
                if AnsiContainsText(fs, ';') then
                  begin
                    ExtractStrings([';'], [' ', #9], PChar(fs), s);
                    SetLength(supported[i].ExtList, s.Count);
                    for j := 0 to s.Count - 1 do
                        supported[i].ExtList[j] := s[j];
                  end
                else
                  begin
                    SetLength(supported[i].ExtList, 1);
                    supported[i].ExtList[0] := fs;
                  end;
                if (ft = '') then
                    ft := UpCase(supported[i].ExtList[0]);
                if not Assigned(supported[i].Panel) then
                    supported[i].Panel := TFrmFile.Create(nil, False, ft);
                supported[i].Panel.PnText.OnClick := @flSelectFilter;
              finally
                s.Free;
              end;
          end;
    end;

    procedure GetSupportedListExtensions(var supported: TFilterArray);
    var
        i, j: integer;
        s: TStrings;
    begin
        SetLength(supported, Length(SupportedList));
        for i := 0 to High(SupportedList) do
          begin
            if AnsiContainsText(SupportedList[i].Extension, ';') then
              begin
                s := TStringList.Create;
                  try
                    ExtractStrings([';'], [' ', #9], PChar(SupportedList[i].Extension), s);
                    SetLength(supported[i].ExtList, s.Count);
                    for j := 0 to s.Count - 1 do
                        supported[i].ExtList[j] := s[j];
                  finally
                    s.Free;
                  end;
              end
            else
              begin
                SetLength(supported[i].ExtList, 1);
                supported[i].ExtList[0] := SupportedList[i].Extension;
              end;
            if not Assigned(supported[i].Panel) then
                supported[i].Panel := TFrmFile.Create(nil, False, UpCase(supported[i].ExtList[0]));
            supported[i].Panel.PnText.OnClick := @flSelectFilter;
          end;
    end;

var
    i: integer;
begin
    Show;
    for i := 0 to High(Extensions) do
      begin
        SetLength(Extensions[i].ExtList, 0);
        if Assigned(Extensions[i].Panel) then
            Extensions[i].Panel.Free;
      end;
    SetLength(Extensions, 0);
    FKind := Kind;
    case Fkind of
        fdkLoadMusic:
          begin
            PnTitle.Caption := 'Add to playlist';
            BtnAccept.Caption := 'Add';
            BtnAccept.Cursor := crOpen;
            GetSupportedMusicExtensions(Extensions);
            flSelectFilter(Extensions[FMusidx].Panel.PnText);
          end;
        fdkLoadPlaylist:
          begin
            PnTitle.Caption := 'Load playlist';
            BtnAccept.Caption := 'Load';
            BtnAccept.Cursor := crOpen;
            GetSupportedListExtensions(Extensions);
            flSelectFilter(Extensions[Flistidx].Panel.PnText);
          end;
        fdkSavePlaylist:
          begin
            PnTitle.Caption := 'Save playlist';
            BtnAccept.Caption := 'Save';
            BtnAccept.Cursor := crSave;
            GetSupportedListExtensions(Extensions);
            flSelectFilter(Extensions[Flistidx].Panel.PnText);
          end;
      end;
    for i := 0 to High(Extensions) do
      begin
        Extensions[i].Panel.SelectedColor := RGBToColor(ProgramSettings.Colors[8].R, ProgramSettings.Colors[8].G, ProgramSettings.Colors[8].B);
        Extensions[i].Panel.Left := 1;
        Extensions[i].Panel.Top := (i * 20) + 1;
        Extensions[i].Panel.Width := PnFilterSelect.Width - 2;
        Extensions[i].Panel.Parent := PnFilterSelect;
        Extensions[i].Panel.Show;
      end;
    PnFilterSelect.Height := Length(Extensions) * 21;
    PnFilterSelect.Top := PnFooter.Top - PnFilterSelect.Height + 1;
end;

procedure TFrmFileDialogMPJ.Accept;
begin
    if TxtDirName.Focused then
      begin
        if FileExists(TxtDirName.Text) then
          goChangeDir(BtnEnter);
      end
    else if (GetSelectedFiles.Count > 0) then
        goAccept(BtnAccept);
end;

procedure TFrmFileDialogMPJ.Show;
begin
    FKind := fdkInvalid;
    PnTitle.Caption := '??';
    BtnAccept.Caption := '??';
    BtnAccept.Cursor := crDefaultCursor;
    inherited;
end;

{%endregion}

end.

