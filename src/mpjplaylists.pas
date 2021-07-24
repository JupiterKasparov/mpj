unit MpjPlaylists;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, strutils, IniFiles;

type
  TPlaylist = record
    IsExtended: boolean;
    Files: array of record
      IsAbsolutePath: boolean;
      FileName, TrackName: string;
      LengthSec: integer;
    end;
  end;

  TPlaylistExtension = record
      Extension: string;
      Loader, Saver: procedure(var Lst: TPlaylist; Fname: string);
  end;

  TPlaylistEvent = procedure(Error: string) of object;

procedure __LoadPLS(var Lst: TPlaylist; Fname: string);
procedure __SavePLS(var Lst: TPlaylist; Fname: string);
procedure __LoadM3U(var Lst: TPlaylist; Fname: string);
procedure __SaveM3U(var Lst: TPlaylist; Fname: string);

const
  SupportedList: array [0..1] of TPlaylistExtension =
    (
      (Extension: 'pls'; Loader: @__LoadPLS; Saver: @__SavePls),
      (Extension: 'm3u;m3u8'; Loader: @__LoadM3U; Saver: @__SaveM3U)
    );

var
  OnPlaylistError: TPlaylistEvent = nil;

procedure LoadPlaylist(var Lst: TPlaylist; Fname: string);
procedure SavePlaylist(var Lst: TPlaylist; Fname: string);
function IsPlaylist(Fname: string): boolean;

implementation

const
  strUnknownList: string = 'File type ''%s'' not playlist format!';
  strDirectoryNotList = 'File ''%s'' is a directory!';
  PLS_sectname: string = 'playlist';

{
*******************
*** Unit-locals ***
*******************
}

{%region /fold}

function GetExtensionTableFromFile(fn: string): integer;
var
  i, j: integer;
  s: TStrings;
begin
  Result := -1;
  s := TStringList.Create;
  fn := ExtractFileExt(fn);
  if AnsiStartsText('.', fn) then
     fn := RightStr(fn, StrLen(PChar(fn)) - 1);
  for i := 0 to High(SupportedList) do
      if AnsiContainsText(SupportedList[i].Extension, ';') then
         begin
           ExtractStrings([';'], [' ', #9], PChar(SupportedList[i].Extension), s);
           for j := 0 to s.Count - 1 do
               if (CompareText(s[j], fn) = 0) then
                  begin
                    Result := i;
                    break;
                  end;
         end
      else
         begin
           if (CompareText(SupportedList[i].Extension, fn) = 0) then
              begin
                Result := i;
                break;
              end;
         end;
  s.Free;
end;

{
 FALSE = the file name is invalid
 TRUE = the file name is valid
}
function GetFile(listfilename, line: string; var Lst: TPlaylist): boolean;
var
  dl: char;
begin
  Result := True;
  Lst.Files[High(Lst.Files)].IsAbsolutePath := False;
  for dl := 'A' to 'Z' do
      begin
        if AnsiStartsText(Format('%s:\', [dl]), line) then
           begin
            Lst.Files[High(Lst.Files)].IsAbsolutePath := True;
            break;
           end;
      end;
      if AnsiStartsText('\\', line) then
       Lst.Files[High(Lst.Files)].IsAbsolutePath := True;
      if Lst.Files[High(Lst.Files)].IsAbsolutePath then
         begin
          if FileExists(line) then
           Lst.Files[High(Lst.Files)].FileName := line
          else
            Result := False;
         end
      else
         begin
           if FileExists(IncludeTrailingPathDelimiter(ExtractFilePath(listfilename)) + line) then
            Lst.Files[High(Lst.Files)].FileName := IncludeTrailingPathDelimiter(ExtractFilePath(listfilename)) + line
           else
             Result := False;
         end;
end;

{%endregion}

{
*******************
Universal load/save
*******************
}

{%region /fold}

procedure LoadPlaylist(var Lst: TPlaylist; Fname: string);
var
  e: integer;
begin
  e := GetExtensionTableFromFile(Fname);
  if (e >= 0) and not DirectoryExists(Fname) then
     SupportedList[e].Loader(Lst, Fname)
  else if Assigned(OnPlaylistError) then
     begin
       if DirectoryExists(Fname) then
          OnPlaylistError(Format(strDirectoryNotList, [ExtractFileName(Fname)]))
       else
          OnPlaylistError(Format(strUnknownList, [ExtractFileExt(Fname)]));
     end;
end;

procedure SavePlaylist(var Lst: TPlaylist; Fname: string);
var
  e: integer;
begin
  e := GetExtensionTableFromFile(Fname);
  if (e >= 0) and not DirectoryExists(Fname) then
     SupportedList[e].Saver(Lst, Fname)
  else if Assigned(OnPlaylistError) then
     begin
       if DirectoryExists(Fname) then
          OnPlaylistError(Format(strDirectoryNotList, [ExtractFileName(Fname)]))
       else
          OnPlaylistError(Format(strUnknownList, [ExtractFileExt(Fname)]));
     end;
end;

{%endregion}

{
*******************
*** PLS support ***
*******************
}

{%region /fold}

procedure __LoadPLS(var Lst: TPlaylist; Fname: string);
var
  f: TIniFile;
  i, len: integer;
begin
  f := TIniFile.Create(Fname);
  SetLength(Lst.Files, 0);
  try
    try
      len := f.ReadInteger(PLS_sectname, 'NumberOfEntries', 0);
      for i := 0 to len do
          begin
            SetLength(Lst.Files, Length(Lst.Files) + 1);
            if not GetFile(Fname, f.ReadString(PLS_sectname, Format('File%d', [i + 1]), ''), Lst) then
               SetLength(Lst.Files, Length(Lst.Files) - 1);
          end;
    finally
      f.Free;
    end;
  except
    on E: Exception do
       begin
         SetLength(Lst.Files, 0);
         if Assigned(OnPlaylistError) then
          OnPlaylistError(E.ToString);
       end;
  end;
end;

procedure __SavePLS(var Lst: TPlaylist; Fname: string);
var
  f: TIniFile;
  i: integer;
begin
  DeleteFile(Fname);
  f := TIniFile.Create(Fname);
  try
    try
      for i := 0 to High(Lst.Files) do
          begin
          f.WriteString(PLS_sectname, Format('File%d', [i + 1]), Lst.Files[i].FileName);
          f.WriteString(PLS_sectname, Format('Title%d', [i + 1]), Lst.Files[i].TrackName);
          f.WriteString(PLS_sectname, Format('Length%d', [i + 1]), IntToStr(Lst.Files[i].LengthSec));
        end;
      f.WriteInteger(PLS_sectname, 'NumberOfEntries', Length(Lst.Files));
      if Lst.IsExtended then
         f.WriteInteger(PLS_sectname, 'Version', 2);
    finally
      f.Free;
    end;
  except
    on E: Exception do
       if Assigned(OnPlaylistError) then
          OnPlaylistError(E.ToString);
  end;
end;

{%endregion}

{
*******************
*** M3U support ***
*******************
}

{%region /fold}

procedure __LoadM3U(var Lst: TPlaylist; Fname: string);
var
  f: System.Text;
  line: string;
  iores: integer;
  lnum: integer = 2; // The first line is #EXTM3U, but if not extended, lnum is ignored
begin
  SetLength(Lst.Files, 0);
  {$I-}
  System.Assign(f, Fname);
  System.Reset(f);
  readln(f, line);
  Lst.IsExtended := CompareText('#EXTM3U', Trim(line)) = 0;
  if Lst.IsExtended then
     while not EOF(f) do
      begin
        if not EOF(f) then
           begin
            readln(f, line);
            if (Trim(line) <> '') then
               begin
                if not AnsiStartsText('#EXTINF:', line) then
                 if Assigned(OnPlaylistError) then
                  OnPlaylistError(Format('M3U syntax error, line %d, file ''%s''!', [lnum, Fname]));
               end;
            Inc(lnum);
           end;
        if not EOF(f) then
         begin
          readln(f, line);
          if (Trim(line) <> '') then
           begin
            SetLength(Lst.Files, Length(Lst.Files) + 1);
            readln(f, line);
            if not GetFile(Fname, line, Lst) then
             SetLength(Lst.Files, Length(Lst.Files) - 1);
           end;
          Inc(lnum);
         end;
      end
  else
     begin
      System.Reset(f);
      while not EOF(f) do
       begin
        SetLength(Lst.Files, Length(Lst.Files) + 1);
        readln(f, line);
        if not GetFile(Fname, line, Lst) then
         SetLength(Lst.Files, Length(Lst.Files) - 1);
       end;
     end;
  System.Close(f);
  {$I+}
  iores := IOResult;
  if (iores <> 0) then
    if Assigned(OnPlaylistError) then
     OnPlaylistError(Format('M3U read error %d, file ''%s''!', [iores, Fname]));
end;

procedure __SaveM3U(var Lst: TPlaylist; Fname: string);
var
  f: System.Text;
  i, iores: integer;
begin
  {$I-}
  System.Assign(f, Fname);
  System.Rewrite(f);
  if Lst.IsExtended then
    writeln(f, '#EXTM3U');
  for i := 0 to High(Lst.Files) do
   begin
    if Lst.IsExtended then
      writeln(f, Format('#EXTINF:%d,%s', [Lst.Files[i].LengthSec, Lst.Files[i].TrackName]));
    writeln(f, Lst.Files[i].FileName);
   end;
  System.Close(f);
  {$I+}
  iores := IOResult;
  if (iores <> 0) then
    if Assigned(OnPlaylistError) then
     OnPlaylistError(Format('M3U save error %d, file ''%s''!', [iores, Fname]));
end;

{%endregion}

function IsPlaylist(Fname: string): boolean;
begin
  Result := GetExtensionTableFromFile(Fname) >= 0;
end;

end.

