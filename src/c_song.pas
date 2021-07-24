unit c_song;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, Squall,
    strutils, Graphics, crDefs, MP3FileUtils, ID3v2Frames, contnrs,
    c_settings, fgl;

const
    SupportedMusic: array [0..3] of string = ('MP3:mp3', 'WAV:wav;wave', 'OGG Vorbis:ogg', 'Windows Media Audio:wma');

type

    TID3PictureList = specialize TFPGList<TPicture>;

    { TFrmSong }

    TFrmSong = class(TFrame)
        BtnDel: TPanel;
        BtnUp: TPanel;
        BtnDown: TPanel;
        PnLength: TPanel;
        PnNum: TPanel;
        PnName: TPanel;
    private
        // SQUALL internals
        sq_sample, sq_channel: integer;
        // Stored values for calculations
        _ofreq: integer;
        _origlen: integer;
        _sp: real;
        _vol: integer;
        // Track internals
        _isloaded, _isdummy, _extid3loaded, _id3loaded, _titled: boolean;
        _file, _dummyfile, _extid3, _id3, _title: string;
        // ID3 Tag
        _tag1: TID3v1Tag;
        _tag2: TID3v2Tag;
        // Procedures & functions
        function GetMaxSpeed: real;
        function GetSpeed: real;
        procedure SetSpeed(Sp: real);
        function GetLengthSec: integer;
        function IsPaused: boolean;
        function IsStopped: boolean;
        function GetPlayPosition: integer;
        procedure SetPlayPosition(Pos: integer);
        function GetVolume: integer;
        procedure SetVolume(Vol: integer);
        function GetTitle: string;
        function GetExtID3: string;
        function GetID3: string;
    public
        Shuffled, SkipIt: boolean;
        procedure Play;
        procedure Pause;
        procedure Resume;
        procedure Stop;
        procedure GetPosition(out h, m, s: integer);
        procedure GetLength(out h, m, s: integer);
        procedure Unload;
        function HasID3Tag: boolean;
        function HasID3Picture: boolean;
        function ID3Picture: TID3PictureList;
        function IsValid: boolean;
        function ShowFullName: boolean;
        procedure ResetCaption(num: integer);
        constructor Create(Fname: string);
        destructor Destroy; override;
        property Speed: real read GetSpeed write SetSpeed;
        property LengthSec: integer read GetLengthSec;
        property Stopped: boolean read IsStopped;
        property Paused: boolean read IsPaused;
        property PositionSec: integer read GetPlayPosition write SetPlayPosition;
        property Volume: integer read GetVolume write SetVolume;
        property Title: string read GetTitle;
        property ExtendedID3: string read GetExtID3;
        property ID3: string read GetID3;
        property FileName: string read _file;
        property OriginalLengthSec: integer read _origlen;
        property MaxSpeed: real read GetMaxSpeed;
    end;

    TSongList = specialize TFPGList<TFrmSong>;

implementation

var
    temp_files_count: integer = 0; {This is the number in the filename of the Next temp file}

{$R *.lfm}

{ TFrmSong }

{********
 PRIVATE METHODS
 ********}

function TFrmSong.GetMaxSpeed: real;
var
    part: real;
begin
    if (_ofreq <= 0) then
        Result := 0.0
    else
      begin
        Result := 100000 / _ofreq;
        if (Result > 10.0) then
            Result := 10.0
        else if (Result < 0.1) then
            Result := 0.1
        else
          begin
            // Round down to quarter
            part := frac(Result);
            if (part > 0.75) then
                part := 0.75
            else if (part > 0.5) then
                part := 0.5
            else if (part > 0.25) then
                part := 0.25
            else if (part > 0.0) then
                part := 0.0;
            Result := trunc(Result) + part;
          end;
      end;
end;

function TFrmSong.GetSpeed: real;
begin
    if not IsStopped then
        Result := SQUALL_Channel_GetFrequency(sq_channel) / _ofreq
    else
        Result := _sp;
end;

procedure TFrmSong.SetSpeed(Sp: real);
var
    h, m, s: integer;
begin
    if (Sp >= 0.1) and (Sp <= 10) and (Sp <= GetMaxSpeed) then
      begin
        _sp := Sp;
        if not IsStopped then
            SQUALL_Channel_SetFrequency(sq_channel, round(_ofreq * Sp));
        GetLength(h, m, s);
        GetLength(h, m, s);
        if (h = 0) then
            PnLength.Caption := Format('%.2d:%.2d', [m, s])
        else
            PnLength.Caption := Format('%.2d:%.2d:%.2d', [h, m, s]);
      end;
end;

function TFrmSong.GetLengthSec: integer;
begin
    Result := round(_origlen / GetSpeed);
end;

function TFrmSong.IsPaused: boolean;
begin
    Result := (SQUALL_Channel_Status(sq_channel) = SQUALL_CHANNEL_STATUS_PAUSE);
end;

function TFrmSong.IsStopped: boolean;
var
    s: integer;
begin
    s := SQUALL_Channel_Status(sq_channel);
    Result := (s = SQUALL_CHANNEL_STATUS_NONE) or (s = SQUALL_CHANNEL_STATUS_PREPARED);
end;

function TFrmSong.GetPlayPosition: integer;
begin
    if not IsStopped then
        Result := round(SQUALL_Channel_GetPlayPositionMs(sq_channel) / GetSpeed) div 1000
    else
        Result := 0;
end;

procedure TFrmSong.SetPlayPosition(Pos: integer);
begin
    if not IsStopped then
        SQUALL_Channel_SetPlayPositionMs(sq_channel, round(Pos * GetSpeed) * 1000);
end;

function TFrmSong.GetVolume: integer;
begin
    if IsStopped then
        Result := _vol
    else
        Result := SQUALL_Channel_GetVolume(sq_channel);
end;

procedure TFrmSong.SetVolume(Vol: integer);
begin
    if (Vol < 0) then
        Vol := 0
    else if (Vol > 100) then
        Vol := 100;
    _vol := Vol;
    if not IsStopped then
        SQUALL_Channel_SetVolume(sq_channel, _vol);
end;

function TFrmSong.GetExtID3: string;
var
    i: integer;
    l: TObjectList;
    f: TID3v2Frame;
begin
    if _extid3loaded then
        Exit(_extid3);
    { If not loaded, load it }
    _extid3loaded := True;
    _extid3 := '';
    Result := '';
    if Assigned(_tag2) then
      begin
        l := _tag2.GetAllFrames;
        for i := 0 to l.Count - 1 do
          begin
            f := l[i] as TID3v2Frame;
            if (f.FrameType <> FT_PictureFrame) then
              begin
                Result := Result + Format('%s: %s', [f.FrameTypeDescription, f.GetText]);
                if (i < (l.Count - 1)) then
                    Result := Result + #13#10;
              end;
          end;
        l.Free;
      end;
    _extid3 := Result;
end;

function TFrmSong.GetID3: string;
var
    tTitle, tAlbum, tArtist, tYear, tGenre, tComment: string;
begin
    if _id3loaded then
        Exit(_id3);
    { If ID3 not loaded, then load it }
    _id3loaded := True;
    _id3 := '';
    Result := '';
    if Assigned(_tag2) or Assigned(_tag1) then
      begin
        if Assigned(_tag2) then
          begin
            tTitle := _tag2.Title;
            tAlbum := _tag2.Album;
            tArtist := _tag2.Artist;
            tYear := _tag2.Year;
            tGenre := _tag2.Genre;
            tComment := _tag2.Comment;
          end
        else
          begin
            tTitle := _tag1.Title;
            tAlbum := _tag1.Album;
            tArtist := _tag1.Artist;
            tYear := _tag1.Year;
            tGenre := _tag1.Genre;
            tComment := _tag1.Comment;
          end;
        if (tTitle <> '') then
          begin
            Result := tTitle;
            if (tAlbum <> '') then
                Result := Format('%s'#13#10'Album: %s', [Result, tAlbum]);
            if (tArtist <> '') then
                Result := Format('%s'#13#10'Artist: %s', [Result, tArtist]);
            if (tYear <> '') then
                Result := Format('%s'#13#10'Year: %s', [Result, tYear]);
            if (tGenre <> '') then
                Result := Format('%s'#13#10'Genre: %s', [Result, tGenre]);
            if (tComment <> '') then
                Result := Format('%s'#13#10'Comments: %s', [Result, tComment]);
          end;
      end;
    _id3 := Result;
end;

function TFrmSong.GetTitle: string;
var
    tTitle, tAlbum, tArtist: string;
begin
    if _titled then
        Exit(_title);
    _titled := True;
    {If we do not have a title, then create it}
    if Assigned(_tag2) or Assigned(_tag1) then
      begin
        if Assigned(_tag2) then
          begin
            tTitle := _tag2.Title;
            tAlbum := _tag2.Album;
            tArtist := _tag2.Artist;
          end
        else
          begin
            tTitle := _tag1.Title;
            tAlbum := _tag1.Album;
            tArtist := _tag1.Artist;
          end;
        if (tTitle <> '') then
          begin
            Result := tTitle;
            if (tAlbum <> '') and (tArtist <> '') then
                Result := Format('%s - %s - %s', [Result, tAlbum, tArtist])
            else if (tAlbum <> '') then
                Result := Format('%s - %s', [Result, tAlbum])
            else if (tArtist <> '') then
                Result := Format('%s - %s', [Result, tArtist]);
          end
        else
            Result := ChangeFileExt(ExtractFileName(_file), '');
      end
    else
        Result := ChangeFileExt(ExtractFileName(_file), '');
    _title := Result;
end;

{********
 PUBLIC METHODS
 ********}

procedure TFrmSong.Play;
begin
    if IsStopped then
      begin
        sq_channel := SQUALL_Sample_Play(sq_sample, 0, 0, 1);
        SetSpeed(_sp);
        SetVolume(_vol);
      end;
end;

procedure TFrmSong.Pause;
begin
    if (not IsPaused) and (not IsStopped) then
        SQUALL_Channel_Pause(sq_channel, 1);
end;

procedure TFrmSong.Resume;
begin
    if IsPaused and (not IsStopped) then
        SQUALL_Channel_Pause(sq_channel, 0);
end;

procedure TFrmSong.Stop;
begin
    if not IsStopped then
        SQUALL_Channel_Stop(sq_channel);
end;

procedure TFrmSong.GetPosition(out h, m, s: integer);
var
    pos: integer;
begin
    pos := GetPlayPosition;
    h := pos div 3600;
    m := (pos - (h * 3600)) div 60;
    s := pos - (h * 3600) - (m * 60);
end;

procedure TFrmSong.GetLength(out h, m, s: integer);
var
    ls: integer;
begin
    ls := GetLengthSec;
    h := ls div 3600;
    m := (ls - (h * 3600)) div 60;
    s := ls - (h * 3600) - (m * 60);
end;

procedure TFrmSong.Unload;
begin
    if _isloaded then
      begin
        _isloaded := False;
        if IsValid then
          begin
            if not IsStopped then
                Stop;
            SQUALL_Sample_Unload(sq_sample);
          end;
        if _isdummy and FileExists(_dummyfile) then
            DeleteFile(_dummyfile);
        _extid3 := '';
        _id3 := '';
        if Assigned(_tag1) then
            FreeAndNil(_tag1);
        if Assigned(_tag2) then
            FreeAndNil(_tag2);
      end;
end;

function TFrmSong.HasID3Tag: boolean;
begin
    Result := not IsEmptyStr(ID3, [' ', #9]);
end;

function TFrmSong.HasID3Picture: boolean;
var
    i: integer;
    l: TObjectList;
    f: TID3v2Frame;
begin
    Result := False;
    if Assigned(_tag2) then
          try
            l := _tag2.GetAllFrames;
            for i := 0 to l.Count - 1 do
              begin
                f := l[i] as TID3v2Frame;
                if (f.FrameType = FT_PictureFrame) then
                    exit(True);
              end;
          finally
            l.Free;
          end;
end;

function TFrmSong.ID3Picture: TID3PictureList;
var
    i: integer;
    l: TObjectList;
    f: TID3v2Frame;
    s: TMemoryStream;
    pic: TPicture;
    _mime: string;
    _pictype: byte;
    _descr: WideString;
begin
    Result := TID3PictureList.Create;
    if Assigned(_tag2) then
      begin
        s := TMemoryStream.Create;
        l := _tag2.GetAllFrames;
        for i := 0 to l.Count - 1 do
          begin
            f := l[i] as TID3v2Frame;
            if (f.FrameType = FT_PictureFrame) then
              begin
                s.Clear;
                if f.GetPicture(_mime, _pictype, _descr, s) then
                  begin
                    pic := TPicture.Create;
                    s.Seek(0, soFromBeginning);
                    pic.LoadFromStream(s);
                    Result.Add(pic);
                  end;
              end;
          end;
        l.Free;
        s.Free;
      end;
end;

function TFrmSong.IsValid: boolean;
begin
    Result := sq_sample >= 0;
end;

function TFrmSong.ShowFullName: boolean;
begin
    Result := (PnName.Canvas.TextWidth(PnName.Caption) > PnName.Width) and PnName.MouseEntered;
end;

procedure TFrmSong.ResetCaption(num: integer);
var
    h, m, s: integer;
begin
    // Track number (parameter)
    PnNum.Caption := IntToStr(num + 1);

    // Track title
    if ProgramSettings.UseID3Title and (Title <> '') then
        PnName.Caption := Title
    else
        PnName.Caption := ChangeFileExt(ExtractFileName(_file), '');

    // Track length
    GetLength(h, m, s);
    if (h = 0) then
        PnLength.Caption := Format('%.2d:%.2d', [m, s])
    else
        PnLength.Caption := Format('%.2d:%.2d:%.2d', [h, m, s]);
end;

constructor TFrmSong.Create(Fname: string);
var
    info: squall_sample_default_t;
begin
    inherited Create(nil);
    _isloaded := False;
    _isdummy := False;
    _extid3loaded := False;
    _id3loaded := False;
    _titled := False;
    _extid3 := '';
    _id3 := '';
    _title := '';
    _file := Fname;
    Shuffled := False;
    SkipIt := False;
    sq_sample := SQUALL_Sample_LoadFile(PChar(_file), 0, nil);
    if not IsValid then
      begin
        _dummyfile := Format('%s\tmp%d%s', [mpj_temp_dir, temp_files_count, ExtractFileExt(FileName)]);
        if not DirectoryExists(ExtractFilePath(_dummyfile)) then
            CreateDir(ExtractFilePath(_dummyfile));
        if not CopyFile(_file, _dummyfile, True, False) then
            Exit;
        _isdummy := True;
        sq_sample := SQUALL_Sample_LoadFile(PChar(_dummyfile), 0, nil);
        Inc(temp_files_count);
      end;
    if IsValid then
      begin
        _sp := 1.0;
        SQUALL_Sample_GetDefault(sq_sample, info);
        _ofreq := info.Frequency;
        _origlen := SQUALL_Sample_GetFileLengthMs(sq_sample) div 1000;
        _vol := info.Volume;
        // If something unexpected error happens during ID3 creation, do not die, just return no ID3!
          try
            _tag2 := TID3v2Tag.Create;
            _tag2.ReadFromFile(_file);
            if not _tag2.Exists then
              begin
                FreeAndNil(_tag2);
                _tag1 := TID3v1Tag.Create;
                _tag1.ReadFromFile(_file);
                if not _tag1.Exists then
                    FreeAndNil(_tag1);
              end;
          except
            _extid3 := '';
            _extid3loaded := True;
            _id3 := '';
            _id3loaded := True;
            _title := ChangeFileExt(ExtractFileName(_file), '');
            _titled := True;
          end;
      end;
    _isloaded := True;
    ResetCaption(0);
end;

destructor TFrmSong.Destroy;
begin
    Unload;
    inherited;
end;

initialization
    SQUALL_Init(nil);

finalization
    SQUALL_Sample_UnloadAll;
    SQUALL_Free;

end.
