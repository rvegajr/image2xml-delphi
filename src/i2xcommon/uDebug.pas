{-----------------------------------------------------------------------------
 Unit Name : DelphiDebug
 Author    : Loïs Bégué
 Date      : 10-Jan-2005
 Purpose   : Send string via Windows API to Delphi's (or other's) Debugger
             The Delphi Debugger will put the messages in the event
             protocol window of the IDE (Ctrl +Alt + V)
             Each line may include a time stamp / duration
-----------------------------------------------------------------------------}
unit uDebug;

interface

uses Windows, Sysutils;


procedure DebugStringStart(aCaption, aText: string);
procedure DebugStringStop(aCaption, aText: string);
procedure DebugString(aCaption, aText: string);
procedure dbg( const aCaption, aText: string);
var
  DebugOutputFileName : string;
  
implementation

uses
  Dialogs;

type

  TDebugStringProc = procedure(aCaption, aText: string);
var
  StartDT: TDateTime;
  StopDT: TDateTime;
  StartDTPrec: Int64;
  StopDTPrec: Int64;
  PerfFrequency: Int64;
  DSStart: TDebugStringProc;
  DSStop: TDebugStringProc;
  DSStr: TDebugStringProc;

  // GetFormatDT - Output = formated DateTime String
function GetFormatDT(aDateTime: TDateTime): string;
begin
  Result := FormatDateTime('dd.mm.yy hh:nn:ss zzz', aDateTime);
end;

// GetFormatT - Output = formated Time String
function GetFormatT(aDateTime: TDateTime): string;
begin
  Result := FormatDateTime('hh:nn:ss zzz', aDateTime)
end;

// _DebugStringStart - internal: Debug string at start time
procedure _DebugStringStart(aCaption, aText: string);
begin
  StartDT := Now;
  OutputDebugString(PChar(Format('[%s][%s] %s',
    [aCaption, GetFormatDT(StartDT),
    aText])));
end;

// _DebugStringStop - internal: Debug string at stop time
procedure _DebugStringStop(aCaption, aText: string);
begin
  StopDT := Now;
  OutputDebugString(PChar(Format('[%s][%s][%s] %s',
    [aCaption, GetFormatDT(StopDT),
    GetFormatT(StopDT - StartDT),
    aText])));
end;

// _DebugStringStart - internal: Debug string at start time (high definition)
procedure _DebugStringStartPrecision(aCaption, aText: string);
begin
  QueryPerformanceCounter(StartDTPrec);
  OutputDebugString(PChar(Format('[%s][%s] %s',
    [aCaption, GetFormatDT(Now()),
    aText])));
end;

// _DebugStringStop - internal: Debug string at stop time (high definition) in ms
procedure _DebugStringStopPrecision(aCaption, aText: string);
begin
  QueryPerformanceCounter(StopDTPrec);
  OutputDebugString(PChar(Format('[%s][%s][%.2n ms] %s',
    [aCaption, GetFormatDT(Now()),
    (1000 * (StopDTPrec - StartDTPrec) / PerfFrequency),
    aText])));
end;

// DebugStringStart - external: wrapper function
procedure DebugStringStart(aCaption, aText: string);
begin
  DSStart(aCaption, aText);
end;

// DebugStringStop - external: wrapper function
procedure DebugStringStop(aCaption, aText: string);
begin
  DSStop(aCaption, aText);
end;

// DebugString - external: direct mode
procedure DebugString(aCaption, aText: string);
begin
  OutputDebugString(PChar(Format('[%s][%s] %s',
    [aCaption, GetFormatDT(Now()),
    aText])));
end;

procedure AppendLine( const stringToWrite, fileName : string );
var
  myFile : TextFile;
  text   : string;
Begin
  AssignFile( myFile, fileName );
  if ( not FileExists( fileName )) then begin
    ReWrite(myFile);
  end else begin
    Append( myFile );
  end;

  WriteLn( myFile, stringToWrite );
  CloseFile( myFile );
End;

// DebugString - external: direct mode
procedure dbg( const aCaption, aText: string);
begin
  if ( Length(DebugOutputFileName) > 0 ) then AppendLine(aText,DebugOutputFileName);
  OutputDebugString( PChar(Format('[%s]: %s',[aCaption, aText])) );
end;

initialization
  DebugOutputFileName := 'Image2XML.log';
  if (( Length(DebugOutputFileName) > 0 ) and FileExists(DebugOutputFileName)) then DeleteFile(DebugOutputFileName);

  // If the high definition mode's available, then
  // link external calls to the "Precision" functions ...
  if QueryPerformanceFrequency(PerfFrequency) then
  begin
    DSStart := _DebugStringStartPrecision;
    DSStop  := _DebugStringStopPrecision;
  end
  // ... else link to the "normal" ones.
  else
  begin
    DSStart := _DebugStringStart;
    DSStop  := _DebugStringStop;
  end;
END.
