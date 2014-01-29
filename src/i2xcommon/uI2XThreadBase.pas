unit uI2XThreadBase;

interface

uses
  SysUtils,
  Classes,
  uI2XDLL,
  uDWImage,
  GR32,
  uI2XConstants,
  uFileDir,
  uI2XTemplate,
  uImage2XML,
  uI2XPluginManager

  ;
type
  TI2XThreadedImageJob = class(TThread)
  private
      FTaskIDFriendly : string;
      FImageMemoryMapID : string;
      FLastStatusMessage : string;
      FLastDebugMessage : string;
      FErrorNo : integer;
      FErrorString: string;
      FDebugLevel : TDebugLevel;
      FDebugPath : string;
      FDebugLog : string;
      FReturnValue : integer;
      FDWImage : TDWImage;
      FTaskCount : integer;
      FResultXML : string;
      FTaskCompleted : integer;
      FOnCancel : TOnCancelEvent;
      FOnJobStart : TOnJobStartEvent;
      FOnJobEnd : TOnJobEndEvent;
      FOnReturnXML : TOnReturnXMLEvent;
      FOnJobError : TOnJobErrorEvent;
      FOnStatusChange : TOnStatusChangeEvent;
      FOnDebugMessage : TOnStatusChangeEvent;
      FEnableJobEvents : boolean;
      FImageFileName : string;
      //Global plugin manager,  this is so we do not have to reload this everytime it runs..
      //  Remember that we cannot use this if we choose to run this class threaded!
      //  It is much easier to mess with threads when objects are created in thier own thread
      FPluginManager : TI2XPluginManager;
      function DefaultDebugPath: string;
    function getImageFileName: string;
    procedure setImageFileName(const Value: string);
  protected
      FCreatePluginMananger : boolean;
      function getPluginManager(): TI2XPluginManager;
      procedure setPluginManager(Value: TI2XPluginManager);
      procedure OnStatusChange( StatusMessage : string );
      procedure OnDebugMessage( DebugMessage : string );
      procedure OnJobStart( TasksCount : integer );
      procedure OnJobError( ErrorNo : integer; ErrorString: string );
      procedure OnCancel();
      procedure OnJobEnd( TasksCompleted : integer; ImageMemoryMapID : string );
      procedure OnReturnXML( ResultXML : string );
      procedure DoJobStart(); virtual;
      procedure DoCancel(); virtual;
      procedure DoJobEnd(); virtual;
      procedure DoReturnXML(); virtual;
      procedure DoJobError(); virtual;
      procedure DoStatusChange(); virtual;
      procedure DoDebugMessage(); virtual;
      procedure OnJobStartNull( TasksCount : integer );
      procedure OnJobEndNull( TasksCompleted : integer; ImageMemoryMapID : string );
      procedure OnReturnXMLNull( ResultXML : string );
      procedure OnJobErrorNull( ErrorNo : integer; ErrorString: string );
    //procedure Execute; override;
  public
    property DWImage : TDWImage read FDWImage write FDWImage;
    property ImageMemoryMapID : string read FImageMemoryMapID write FImageMemoryMapID;
    //This does not always equal the file name bieng OCRed. This is the file name this is
    // written to the Output XML
    property ImageFileName : string read getImageFileName write setImageFileName;
    property LastStatusMessage : string read FLastStatusMessage write FLastStatusMessage;
    property LastDebugMessage : string read FLastDebugMessage write FLastDebugMessage;
    property OnJobStartEvent : TOnJobStartEvent read FOnJobStart write FOnJobStart;
    property OnJobEndEvent : TOnJobEndEvent read FOnJobEnd write FOnJobEnd;
    property OnReturnXMLEvent : TOnReturnXMLEvent read FOnReturnXML write FOnReturnXML;
    property OnJobErrorEvent : TOnJobErrorEvent read FOnJobError write FOnJobError;
    property OnStatusChangeEvent : TOnStatusChangeEvent read FOnStatusChange write FOnStatusChange;
    property OnDebugMessageEvent : TOnStatusChangeEvent read FOnDebugMessage write FOnDebugMessage;
    property DebugLevel : TDebugLevel read FDebugLevel write FDebugLevel;
    property DebugPath : string read FDebugPath write FDebugPath;
    property TaskIDFriendly : string read FTaskIDFriendly write FTaskIDFriendly;
    property EnableJobEvents : boolean read FEnableJobEvents write FEnableJobEvents;
    property PluginManager : TI2XPluginManager read getPluginManager write setPluginManager;
    property ResultXML : string read FResultXML write FResultXML;
    constructor Create(); overload;
    constructor Create( bmp32: TBitmap32 ); overload;
    destructor Destroy; override;

  end;

implementation
{ TI2XThreadedImageJob }

function TI2XThreadedImageJob.DefaultDebugPath : string;
var
  filedir : CFileDir;
begin
  try
    filedir := CFileDir.Create;
    Result := filedir.GetSystemTempDir + TEMP_DIR_QUAL;
  finally
    FreeAndNil( filedir );
  end;
end;

constructor TI2XThreadedImageJob.Create;
begin
  inherited Create(true);
  FDWImage := TDWImage.Create;
  DebugLevel := dbNone;
  FDebugPath := DefaultDebugPath();
  FTaskIDFriendly := IntToStr( self.ThreadID );
  FEnableJobEvents := true;
  FCreatePluginMananger := true;
  FImageFileName := '';
end;

constructor TI2XThreadedImageJob.Create(bmp32: TBitmap32);
begin
  inherited Create(true);
  FDWImage := TDWImage.Create( bmp32 );
  DebugLevel := dbNone;
  FDebugPath := DefaultDebugPath();
  FTaskIDFriendly := IntToStr( self.ThreadID );
  FEnableJobEvents := true;
  FCreatePluginMananger := true;
  FImageFileName := '';
end;

destructor TI2XThreadedImageJob.Destroy;
begin
  FreeAndNil( FDWImage );
  inherited;
end;

procedure TI2XThreadedImageJob.DoCancel;
Begin
  if ( Assigned(self.FOnCancel)) then begin
        FOnCancel( self, self.FTaskIDFriendly );
  end;
End;

procedure TI2XThreadedImageJob.DoDebugMessage;
begin
  if ( Assigned( self.FOnDebugMessage )) then begin
    FOnDebugMessage( self, self.FTaskIDFriendly, self.FLastDebugMessage );
  end;
end;

procedure TI2XThreadedImageJob.DoJobEnd;
begin
  if ( Assigned(self.FOnJobEnd)) then begin
        FOnJobEnd( self, self.FTaskIDFriendly, self.FTaskCompleted, self.FImageMemoryMapID );
  end;
end;

procedure TI2XThreadedImageJob.DoJobError;
begin
  if ( Assigned( self.FOnJobError )) then begin
        FOnJobError( self, self.FTaskIDFriendly, self.FErrorNo, self.FErrorString );
  end;
end;

procedure TI2XThreadedImageJob.DoJobStart;
begin
  if ( Assigned(self.FOnJobStart)) then begin
    FOnJobStart( self, self.FTaskIDFriendly, self.FTaskCount );
  end;
end;

procedure TI2XThreadedImageJob.DoReturnXML;
begin
  if ( Assigned( self.FOnReturnXML )) then begin
    FOnReturnXML( self, self.FTaskIDFriendly, self.FResultXML );
  end;
end;

procedure TI2XThreadedImageJob.DoStatusChange;
begin
  if ( Assigned( self.FOnStatusChange )) then begin
    FOnStatusChange( self, self.FTaskIDFriendly, self.FLastStatusMessage );
  end;
end;

function TI2XThreadedImageJob.getImageFileName: string;
begin
  Result := self.FImageFileName;
end;

function TI2XThreadedImageJob.getPluginManager: TI2XPluginManager;
begin
  Result := self.FPluginManager;
end;

procedure TI2XThreadedImageJob.OnCancel();
begin
  self.Synchronize( self.DoCancel );
end;

procedure TI2XThreadedImageJob.OnDebugMessage(DebugMessage: string);
begin
  self.FLastDebugMessage := DebugMessage;
  self.Synchronize( self.DoDebugMessage );
end;

procedure TI2XThreadedImageJob.OnJobEnd(TasksCompleted : integer; ImageMemoryMapID : string );
begin
  self.FTaskCompleted := TasksCompleted;
  self.FImageMemoryMapID := ImageMemoryMapID;
  self.Synchronize( self.DoJobEnd );
end;

procedure TI2XThreadedImageJob.OnJobEndNull(TasksCompleted: integer; ImageMemoryMapID : string );
begin

end;

procedure TI2XThreadedImageJob.OnJobError(ErrorNo: integer;
  ErrorString: string);
begin
  self.FErrorNo := ErrorNo;
  self.FErrorString := ErrorString;
  self.Synchronize( self.DoJobError );
end;

procedure TI2XThreadedImageJob.OnJobErrorNull(ErrorNo: integer;
  ErrorString: string);
begin

end;

procedure TI2XThreadedImageJob.OnJobStart(TasksCount: integer);
begin
  self.FTaskCount := TasksCount;
  self.Synchronize( self.DoJobStart );
end;

procedure TI2XThreadedImageJob.OnJobStartNull(TasksCount: integer);
begin

end;

procedure TI2XThreadedImageJob.OnReturnXML( ResultXML: string);
begin
  self.ResultXML := ResultXML;
  self.Synchronize( self.DoReturnXML );
end;

procedure TI2XThreadedImageJob.OnReturnXMLNull( ResultXML: string);
begin

end;

procedure TI2XThreadedImageJob.OnStatusChange(StatusMessage: string);
begin
  self.FLastStatusMessage := StatusMessage;
  self.Synchronize( self.DoStatusChange );
end;

procedure TI2XThreadedImageJob.setImageFileName(const Value: string);
begin
  FImageFileName := Value;
end;

procedure TI2XThreadedImageJob.setPluginManager(Value: TI2XPluginManager);
begin
  if ( Value = nil ) then begin
    self.FCreatePluginMananger := true;
    self.FPluginManager := nil;
  end else begin
    self.FCreatePluginMananger := false;
    self.FPluginManager := Value;
  end;
end;

initialization

finalization
END.
