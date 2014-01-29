unit uI2XImageProcJobThread;

interface
uses
  SysUtils,
  Classes,
  uI2XDLL,
  uDWImage,
  GR32,
  uI2XThreadBase,
  uStrUtil,
  uHashTable,
  uI2XConstants,
  uImage2XML,
  uI2XTemplate,
  uI2XOCR,
  uI2XPluginManager
  ;

type
  TI2XImageProcJobThread = class(TI2XThreadedImageJob)
  private
      FInstructionList : TStringList2;
      FReturnValue : integer;
      FTriggerOCRAfter : boolean;
  protected
    procedure Execute; override;
  public
    property InstructionList : TStringList2 read FInstructionList write FInstructionList;
    property TriggerOCRAfter : boolean read FTriggerOCRAfter write FTriggerOCRAfter;
    constructor Create(); overload;
    constructor Create( bmp32: TBitmap32 ); overload;
    destructor Destroy; override;
    function ExecuteImageProc() : boolean;
  end;

implementation

{ TI2XImageProcJobThread }

constructor TI2XImageProcJobThread.Create;
begin
  inherited Create();
  FInstructionList := TStringList2.Create;
  FTriggerOCRAfter := false;
end;

constructor TI2XImageProcJobThread.Create(bmp32: TBitmap32);
begin
  inherited Create( bmp32 );
  FInstructionList := TStringList2.Create;
  FTriggerOCRAfter := false;
end;

destructor TI2XImageProcJobThread.Destroy;
begin
  FreeAndNil( FInstructionList );
  inherited;
end;

procedure TI2XImageProcJobThread.Execute;
Begin
  ExecuteImageProc();
End;

function TI2XImageProcJobThread.ExecuteImageProc() : boolean;
var
  DLLImageProc : TDLLImageProc;
  iInst : smallint;
  sDLLShortName : string;
  ImageDLLs : THashTable;
  Plugins : TI2XPluginManager;
Begin
  try
    Result := false;
    //Load Image Plugins
    try
      if ( self.FCreatePluginMananger ) then begin
        Plugins := TI2XPluginManager.Create( DLLSearchPath );
        if ( Plugins.SearchAndLoadImagePlugins = 0 ) then
          raise Exception.Create('Image Processing DLLS were not found in search path ' + DLLSearchPath );
      end else
        Plugins := self.PluginManager;

      if ( self.FInstructionList.Count > 0 ) then begin
        if EnableJobEvents then OnJobStart( self.FInstructionList.Count );

        for iInst := 0 to self.FInstructionList.Count - 1 do begin
          OnStatusChange( 'Processing instruction ' + IntToStr( iInst + 1 ) + ' of ' +  IntToStr(self.FInstructionList.Count));
          sDLLShortName := Split( FInstructionList[iInst], IMGPROC_PLUGIN_SEP, 0 );
          DLLImageProc := Plugins.ImageDLLByShortName( sDLLShortName );
          if ( DLLImageProc = nil ) then
            raise Exception.Create('ImageProc DLL with shortname "' + sDLLShortName + '" could not be found.' );
          FReturnValue := DLLImageProc.ProcessImage( self.ImageMemoryMapID, FInstructionList[iInst] );
          if ( FReturnValue <> ERROR_OK ) then
            raise Exception.Create( 'IMG-' + sDLLShortName + ':' + DLLImageProc.LastErrorMessage );
        end;
      end;
      OnStatusChange( 'Image processing completed');
    except
      on E : Exception do begin
        self.OnJobError( ERROR_IMAGE_THREAD_FAILED, E.ClassName + ': ' + E.Message );
      end;
    end;
    if EnableJobEvents then OnJobEnd( iInst, self.ImageMemoryMapID );
    Result := true;
  finally
    if ( FCreatePluginMananger ) then FreeAndNil( Plugins );
  end;
End;

end.
