program frames;

{$mode objfpc}{$H+}

uses
 {$IFDEF UNIX}
 {$IFDEF UseCThreads}
  cthreads,
 {$ENDIF}
 {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  main,
  Windows, Controls, crDefs;


{$R *.res}
{$R cursors.rc}

begin
  Screen.Cursors[crGrab] := LoadCursor(hInstance, 'CUR_GRAB');
  Screen.Cursors[crGrabbed] := LoadCursor(hInstance, 'CUR_GRABBED');
  Screen.Cursors[crScrollVert] := LoadCursor(hInstance, 'CUR_SCROLL_VERT');
  Screen.Cursors[crScrollHorz] := LoadCursor(hInstance, 'CUR_SCROLL_HORZ');
  Screen.Cursors[crDefaultCursor] := LoadCursor(hInstance, 'CUR_DEFAULT');
  Screen.Cursors[crSave] := LoadCursor(hInstance, 'CUR_SAVE');
  Screen.Cursors[crOpen] := LoadCursor(hInstance, 'CUR_OPEN');
  Screen.Cursors[crIBeam] := LoadCursor(hInstance, 'CUR_EDIT_TEXT');
  Screen.Cursors[crHourGlass] := LoadCursor(hInstance, 'CUR_HOURGLASS');
  Screen.Cursors[crCross] := LoadCursor(hInstance, 'CUR_MATH');
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TWndMain, WndMain);
  Application.Run;
end.
