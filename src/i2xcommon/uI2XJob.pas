unit uI2XJob;

interface
uses
  Windows,
  Classes,
  uDebug,
  SysUtils,
  uStrUtil,
  OmniXML,
  uOmniXML,
  Contnrs,
  JclStrings,
  uI2XConstants,
  uI2XPlugin,
  uHashTable,
  uI2XOCR,
  uI2XTemplate,
  uFileDir,
  Math,
  uI2XPluginManager,
  uI2XOCRProcThread,
  uI2XImageProcJobThread,
  uImage2XML,
  uDWImage,
  uI2XOptions,
  uI2XMemMap;

const
  UNIT_NAME = 'uI2XJob';
  NODE_ELEMENT = 1;
  IMAGE_MAX = 10;
  //IMAGE_MAX = MAXINT;

type
  TExecListItem = class(TObject)
    private
      FImageSource : string;
      FLastModified : TDateTime;
      FProcessed : TDateTime;
      FOutput : string;
      FParent : TObject;
    protected
      function getImageSource() : string;
      procedure setImageSource( val : string );
      function getLastModified() : TDateTime;
      procedure setLastModified( val : TDateTime );
      function getProcessed() : TDateTime;
      procedure setProcessed( val : TDateTime );
      function getOutput() : string;
      procedure setOutput( val : string );
      function getParent() : TObject;
      procedure setParent( val : TObject );
    published
      property Parent : TObject read getParent Write setParent;
      property Output : string read getOutput Write setOutput;
      property Processed : TDateTime read getProcessed Write setProcessed;
      property LastModified : TDateTime read getLastModified Write setLastModified;
      property ImageSource  : string read getImageSource Write setImageSource;
    public
      procedure Clear;
      function AsXML() : string;
      function FromXML(xmlString: string): boolean; overload;
      function FromXML(nod: IXMLNode): boolean; overload;
      constructor Create; overload;
      destructor Destroy; override;
      procedure Copy( Source : TExecListItem ); dynamic;
      procedure CopyTo( Target : TExecListItem ); dynamic;
  end;

  TExecList = class(TObject)
    private
      FRunDateTime : TDateTime;
      //FExecList : TObjectList;
      FExecList : THashTable;
      function GetCount(): Cardinal;
    protected
      function GetItem(Index: TIndex): TExecListItem;
      procedure SetItem(Index: TIndex; const Value: TExecListItem);
      function GetRunDateTime(): TDateTime;
      procedure SetRunDateTime(const Value: TDateTime);
      function GetRunDateTimeStr(): string;
      procedure SetRunDateTimeStr(const Value: string);
    published
      function Add( const ItemToAdd : TExecListItem ) : TExecListItem;
      procedure Delete( ItemToDelete : TIndex );
      property RunDateTime : TDateTime read GetRunDateTime write SetRunDateTime;
      property Count : Cardinal read GetCount;
    public
      procedure Clear;
      property RunDateTimeStr : string read GetRunDateTimeStr write SetRunDateTimeStr;
      property Items[ Index: TIndex ]: TExecListItem read GetItem write SetItem; default;
      function AsXML( const RootNodeName : string = 'exec') : string;
      function ImageAlreadyProcessed( fileName : TFileName ) : boolean;
      function SaveToFile( fileName : TFileName ) : boolean;
      function LoadFromFile( fileName : TFileName ) : boolean;
      function FromXML( XMLString : string ) : boolean; overload;
      function FromXML( nod: IXMLNode ): boolean; overload;
      procedure Copy( Source : TExecList ); dynamic;
      procedure CopyTo( Target : TExecList ); dynamic;
      constructor Create; dynamic;
      destructor Destroy; override;
  end;

  TI2XJob = class(TObject)
    private
      FName : string;
      //FJobList : TExecList;
      FJobList : TObjectList;
      FRootPath : string;
      FImagePath : string;
      FOutputPath : string;
      FRestartable : boolean;
      FTemplateFileName : string;
      function GetExecItem(Index: Integer): TExecList;
      procedure SetExecItem(Index: Integer; const Value: TExecList);
      procedure setImagePath(const Value: string);
      procedure setOutputPath(const Value: string);
      procedure setRootPath(const Value: string);
    function GenerateJobFileName: string;
   protected
    published
      property Name : string read FName write FName;
      //property JobList : TExecList read FJobList write FJobList;
      property JobList : TObjectList read FJobList write FJobList;
      property RootPath : string read FRootPath write setRootPath;
      property ImagePath : string read FImagePath write setImagePath;
      property OutputPath : string read FOutputPath write setOutputPath;
      property TemplateFileName : string read FTemplateFileName write FTemplateFileName;
      property Restartable : boolean read FRestartable write FRestartable;
    public
      function Add( const ItemToAdd : TExecList ) : integer;
      procedure Delete( ItemToDelete : Integer );
      function FileExists( const FileName : string; var item : TExecListItem ) : boolean;
      function DeleteFile( const FileName : string ) : boolean;
      property Items[Index: Integer]: TExecList read GetExecItem write SetExecItem; default;
      procedure Clear;
      function AsXML( const RootNodeName : string = 'exec') : string;
      function SaveToFile( fileName : TFileName ) : boolean;
      function Save: boolean;
      property JobFileName : string read GenerateJobFileName;
      function LoadFromFile( fileName : TFileName ) : boolean;
      function LoadXML( XMLString : string ) : boolean;
      procedure Copy( Source : TI2XJob ); dynamic;
      procedure CopyTo( Target : TI2XJob ); dynamic;
      constructor Create; dynamic;
      destructor Destroy; override;
  end;

  TI2XJobExecute = class(TObject)
    private
      FJobFileName : TFileName;
      FileDir : CFileDir;
      FJob : TI2XJob;
      FJobIsLoaded : boolean;
      FTemplate : CTemplate;
      OCR : TI2XOCRProcJobThread;
      ImageProc : TI2XImageProcJobThread;
      FMemoryMapManager : TI2XMemoryMapManager;
      DWImage : TDWImage;
      FDebugOutputOverride : string;
      FCurrentImage : string;
      FProductProducedCount : integer;
      function DoOCR(const sMMID: string; Template: CTemplate; var ResultXML : string ): boolean;
      procedure OnDebugMessage(Sender : TObject; TaskID, sDebugMessage: string);
      procedure OnStatusChange(Sender : TObject; TaskID, sStatusMessage: string);
      function DoImageProcessing(const ImageFileName: string; Template: CTemplate;
        var ImageMMID: string): boolean;
      procedure ConsoleWrite(const msg: string);
      procedure ConsoleStartMessage;
      function PreRunCheck: boolean;
      function SaveBATFile: boolean;
      function GetDebugOutputOverride: TFileName;
      procedure SetDebugOutputOverride(const Value: TFileName);
    protected
      FRootPathAbsolute : string;
      FJobRootPathAbsolute : string;
      FImagePathAbsolute : string;
      FOutputPathAbsolute : string;
      function GetJobFileName(): TFileName;
      procedure SetJobFileName( const Value: TFileName );
    published
      property JobFileName : TFileName read GetJobFileName write SetJobFileName;
      property DebugOutputOverride : TFileName read GetDebugOutputOverride write SetDebugOutputOverride;
    public
      function Run() : boolean;
      constructor Create( const JobFileName : string );
      destructor Destroy; override;
  end;

var
  JobExec : TI2XJobExecute;

implementation

{ TExecListItem }

function TExecListItem.AsXML: string;
var
  sb : TStringBuilder;
begin
  try
    sb := TStringbuilder.Create;
    sb.Append( '<item' );
    sb.Append( ' src="');
    sb.Append( self.FImageSource );
    sb.Append( '"');
    sb.Append( ' lastmoddt="');
    sb.Append( uDateTimeToStr( self.LastModified ) );
    sb.Append( '"');
    sb.Append( ' output="');
    sb.Append( self.FOutput );
    sb.Append( '"');
    sb.Append( ' procdt="');
    sb.Append( uDateTimeToStr( self.FProcessed ) );
    sb.Append( '"');
    sb.Append( ' />' );
    Result := sb.ToString;
  finally
    FreeAndNil( sb );
  end;
end;

function TExecListItem.FromXML(xmlString: string): boolean;
var
  xmlDoc: IXMLDocument;
Begin
  try
    self.Clear;
    if ( XMLString = '' ) then exit;
    xmlDoc := CreateXMLDoc;
    if ( xmlDoc.LoadXML( XMLString ) ) then begin
      self.ImageSource := GetAttr(xmldoc.DocumentElement, 'src', '' );
      self.LastModified := uStrToDateTime( GetAttr(xmldoc.DocumentElement, 'lastmoddt', '' ) );
      self.Output := GetAttr(xmldoc.DocumentElement, 'output', '' );
      self.Processed := uStrToDateTime( GetAttr(xmldoc.DocumentElement, 'procdt', '' ) );
    end else begin
      raise Exception.Create('Could not load XML.  Are you sure it exists and is it well formed?');
    end;
  finally
  end;
End;

function TExecListItem.FromXML(nod: IXMLNode): boolean;
Begin
  try
    Result := self.FromXML( nod.XML );
  finally
  end;
End;

procedure TExecListItem.Clear;
begin
  self.FImageSource := '';
  self.FLastModified := MinDouble;
  self.FOutput := '';
  self.FProcessed := MinDouble;
end;

procedure TExecListItem.Copy(Source: TExecListItem);
begin
  self.FImageSource := Source.FImageSource;
  self.LastModified := Source.LastModified;
  self.FOutput := Source.FOutput;
  self.FProcessed := Source.FProcessed;
end;

procedure TExecListItem.CopyTo(Target: TExecListItem);
begin
  Target.FImageSource := self.FImageSource;
  Target.LastModified := self.LastModified;
  Target.FOutput := self.FOutput;
  Target.FProcessed := self.FProcessed;
end;

constructor TExecListItem.Create;
begin
  inherited;

end;

destructor TExecListItem.Destroy;
begin

  inherited;
end;

function TExecListItem.getImageSource: string;
begin
  Result := self.FImageSource;
end;

function TExecListItem.getLastModified: TDateTime;
begin
  Result := self.FLastModified;
end;

function TExecListItem.getOutput: string;
begin
  Result := self.FOutput;
end;

function TExecListItem.getParent: TObject;
begin
  Result := self.FParent;
end;

function TExecListItem.getProcessed: TDateTime;
begin
  Result := self.FProcessed;
end;

procedure TExecListItem.setImageSource(val: string);
begin
  self.FImageSource := val;
end;

procedure TExecListItem.setLastModified(val: TDateTime);
begin
  self.FLastModified := val;
end;

procedure TExecListItem.setOutput(val: string);
begin
  self.FOutput := val;
end;

procedure TExecListItem.setParent(val: TObject);
begin
  self.FParent := val;
end;

procedure TExecListItem.setProcessed(val: TDateTime);
begin
  self.FProcessed := val;
end;

{ TExecList }

function TExecList.Add(const ItemToAdd: TExecListItem): TExecListItem;
begin
  if ( Length( ItemToAdd.ImageSource ) = 0 ) then
    raise Exception.Create('You must specify an image source');
  FExecList.Add( ItemToAdd.ImageSource, ItemToAdd );
  Result := ItemToAdd;
end;

function TExecList.AsXML(const RootNodeName: string): string;
var
  sb : TStringBuilder;
  i : integer;
begin
  try
    sb := TStringbuilder.Create;
    sb.Append( '<exec ' );
    sb.Append( ' datetime="');
    sb.Append( self.RunDateTimeStr );
    sb.Append( '"');
    sb.Append( '>' );
    sb.Append( '<items>' );
    for i := 0 to self.FExecList.Count - 1 do begin
      sb.Append( self.Items[i].AsXML() );
    end;
    sb.Append( '</items>' );
    sb.Append( '</exec>' );
    Result := sb.ToString;
  finally
    FreeAndNil( sb );
  end;
end;

procedure TExecList.Clear;
begin
  self.FRunDateTime := MinDouble;
  self.FExecList.Clear;
end;

procedure TExecList.Copy(Source: TExecList);
var
  i : integer;
begin
  self.FRunDateTime := Source.FRunDateTime;
  for i := 0 to Source.Count - 1 do begin
    self.Add( Source[ i ] );
  end;
end;

procedure TExecList.CopyTo(Target: TExecList);
var
  i : integer;
begin
  Target.FRunDateTime := self.FRunDateTime;
  for i := 0 to self.Count - 1 do begin
    Target.Add( self[ i ] );
  end;
end;

constructor TExecList.Create;
begin
  //FExecList := TObjectList.Create();
  FExecList := THashTable.Create();
end;

procedure TExecList.Delete(ItemToDelete: TIndex);
var
  item : TExecListItem;
begin
  item := TExecListItem( FExecList.Items[ ItemToDelete ] );
  FExecList.Delete( item.ImageSource );
end;

destructor TExecList.Destroy;
begin
  FreeAndNil( FExecList );
end;

function TExecList.GetCount: Cardinal;
begin
  Result := FExecList.Count;
end;

function TExecList.GetItem(Index: TIndex): TExecListItem;
begin
  Result := TExecListItem( FExecList.Items[ Index ] );
end;

function TExecList.GetRunDateTime: TDateTime;
begin
  Result := self.FRunDateTime;
end;

function TExecList.GetRunDateTimeStr: string;
begin
  Result := uDateTimeToStr( self.GetRunDateTime );
end;

function TExecList.ImageAlreadyProcessed(fileName: TFileName): boolean;
begin
  Result := self.FExecList.ContainsKey( fileName );
end;

function TExecList.LoadFromFile(fileName: TFileName): boolean;
var
  xmlDoc: IXMLDocument;
Begin
  Result := false;
  try
    xmlDoc := CreateXMLDoc;
    if ( xmlDoc.Load( fileName ) ) then begin
      Result := FromXML( xmlDoc.XML );
    end else begin
      raise Exception.Create('Could not load XML from file ' + fileName + '.  Are you sure it exists and is it well formed?');
    end;
  finally
  end;
End;

function TExecList.FromXML(nod: IXMLNode): boolean;
begin
  Result := FromXML ( nod.XML );
end;

function TExecList.FromXML(XMLString: string): boolean;
var
  xmlDoc: IXMLDocument;
  nod, nodItem  : IXMLElement;
  oNod : IXMLNode;
  parsed : boolean;
  xmlNodeList, xmlSubNodeList, nodItemList : IXMLNodeList;
  item: TExecListItem;
  iExecIdx, iItemIdx : integer;
Begin
  try
    Result := false;
    Self.Clear;
    xmlDoc := CreateXMLDoc;
    parsed := xmlDoc.loadXML( XMLString );
    if ( not parsed ) then begin
      raise Exception.Create( 'XML Passed to function could not be parsed.' );
    end;
    self.RunDateTimeStr := GetAttr(xmldoc.DocumentElement, 'datetime');
    xmlNodeList := xmldoc.DocumentElement.ChildNodes;
    for iExecIdx := 0 to xmlNodeList.length - 1 do begin
      nod := IXMLElement(xmlNodeList.Item[iExecIdx]);
      if ( nod.nodeType = ELEMENT_NODE ) then begin
        if (nod.nodeName = 'items' ) then begin
          xmlSubNodeList := nod.ChildNodes;
          for iItemIdx := 0 to xmlSubNodeList.length - 1 do begin
            nodItem := IXMLElement(xmlSubNodeList.Item[iItemIdx]);
            if (( nodItem.nodeType = ELEMENT_NODE ) and
                ( nodItem.nodeName = 'item' )) then begin
              item := TExecListItem.Create();
              item.FromXML( nodItem );
              self.Add( item );
            end;
          end;

        end;
      end;
    end;
  finally
  end;
  Result := true;
End;

function TExecList.SaveToFile(fileName: TFileName): boolean;
var
  xmlDoc: IXMLDocument;
Begin
  Result := false;
  try
    xmlDoc := CreateXMLDoc;
    if ( xmlDoc.LoadXML( AsXML() ) ) then begin
      xmlDoc.Save( fileName );
      Result := true;
    end else begin
      raise Exception.Create('File ' + fileName + ' could not be loaded on into OCR Results.');
    end;
  finally
  end;
End;

procedure TExecList.SetItem(Index: TIndex; const Value: TExecListItem);
Begin
  TExecListItem( FExecList.Items[ Index ] ).Copy( Value );
End;

procedure TExecList.SetRunDateTime(const Value: TDateTime);
begin
  self.FRunDateTime := Value;
end;

procedure TExecList.SetRunDateTimeStr(const Value: string);
begin
  self.SetRunDateTime( uStrToDateTime( Value ) );
end;

{ TI2XJob }

function TI2XJob.Add(const ItemToAdd: TExecList): integer;
begin
  FJobList.Add( TObject( ItemToAdd ) );
end;

function TI2XJob.AsXML(const RootNodeName: string): string;
var
  sb : TStringBuilder;
  i : integer;
  list : TExecList;
begin
  try
    sb := TStringbuilder.Create;
    sb.Append( '<i2x_job ' );
    sb.Append( ' name="');
    sb.Append( self.Name );
    sb.Append( '"');
    sb.Append( '>' );

    sb.Append( '<root_path>' );
    sb.Append( self.RootPath );
    sb.Append( '</root_path>' );

    sb.Append( '<image_path>' );
    sb.Append( self.ImagePath );
    sb.Append( '</image_path>' );

    sb.Append( '<output_path>' );
    sb.Append( self.OutputPath );
    sb.Append( '</output_path>' );

    sb.Append( '<template_file_name>' );
    sb.Append( self.TemplateFileName );
    sb.Append( '</template_file_name>' );

    sb.Append( '<restartable>' );
    sb.Append( uStrUtil.BoolToStr( self.FRestartable, 'Y', 'N' ) );
    sb.Append( '</restartable>' );

    sb.Append( '<job_list>' );
    for i := 0 to FJobList.Count - 1 do begin
      list := TExecList( self.FJobList[i] );
      sb.Append( list.AsXML() );
    end;
    sb.Append( '</job_list>' );

    sb.Append( '</i2x_job>' );
    Result := sb.ToString;
  finally
    FreeAndNil( sb );
  end;
end;

procedure TI2XJob.Clear;
begin
  self.Name := '';
  self.RootPath := '';
  self.ImagePath := '';
  self.OutputPath := '';
  self.Restartable := false;
  self.JobList.Clear;
end;

procedure TI2XJob.Copy(Source: TI2XJob);
var
  i : integer;
  list : TExecList;
begin
  self.Name := Source.Name;
  self.RootPath := Source.RootPath;
  self.ImagePath := Source.ImagePath;
  self.OutputPath := Source.OutputPath;
  self.Restartable := Source.Restartable;

  self.JobList.Free;
  for i := 0 to Source.JobList.Count - 1 do begin
    list := TExecList.Create();
    list.Copy( TExecList( Source.JobList[i] ) );
    self.JobList.Add( list );
  end;
  //self.JobList.FromXML( Source.JobList.AsXML() );
end;

procedure TI2XJob.CopyTo(Target: TI2XJob);
var
  i : integer;
  list : TExecList;
begin
  Target.Name := self.Name;
  Target.RootPath := self.RootPath;
  Target.ImagePath := self.ImagePath;
  Target.OutputPath := self.OutputPath;
  Target.Restartable := self.Restartable;

  Target.JobList.Free;
  for i := 0 to self.JobList.Count - 1 do begin
    list := TExecList.Create();
    list.Copy( TExecList( self.JobList[i] ) );
    Target.JobList.Add( list );
  end;

  //Target.JobList.FromXML( self.JobList.AsXML() );
end;

constructor TI2XJob.Create;
begin
  //self.FJobList := TExecList.Create();
  self.FJobList := TObjectList.Create();
end;

procedure TI2XJob.Delete( ItemToDelete : Integer );
begin
  self.FJobList.Delete( ItemToDelete );
end;

function TI2XJob.DeleteFile(const FileName: string): boolean;
var
  i : integer;
  list : TExecList;
begin
  Result := false;
  for i := 0 to self.JobList.Count - 1 do begin
    list := TExecList( self.JobList[i] );
    if ( list.ImageAlreadyProcessed( FileName ) ) then begin
      list.Delete( FileName );
      Result := true;
      Exit;
    end;
  end;
end;

destructor TI2XJob.Destroy;
begin
  FreeAndNil( FJobList );
  inherited;
end;

function TI2XJob.FileExists(const FileName : string; var item: TExecListItem): boolean;
var
  i : integer;
  list : TExecList;
begin
  item := nil;
  Result := false;
  for i := 0 to self.JobList.Count - 1 do begin
    list := TExecList( self.JobList[i] );
    Result := list.ImageAlreadyProcessed( FileName );
    if ( Result ) then begin
      item := list.Items[ FileName ];
      Exit;
    end;
  end;
end;

function TI2XJob.GenerateJobFileName: string;
var
  sFileName : string;
Begin
  sFileName := self.RootPath;
  if ( sFileName[Length( sFileName )] <> '\' ) then
    sFileName := sFileName + '\';
  if ( not DirectoryExists(sFileName) ) then
    raise Exception.Create('You must set Root Path before Saving the Job Template.');
  sFileName := sFileName + FName + '\';
  ForceDirectories( sFileName );
  Result := sFileName + FName + JOB_FILE_EXT;
  //Result := self.SaveToFile( sFileName );
End;

function TI2XJob.GetExecItem(Index: Integer): TExecList;
begin
  result := TExecList( FJobList.Items[ Index ] );
end;

function TI2XJob.LoadFromFile(fileName: TFileName): boolean;
var
  xmlDoc: IXMLDocument;
Begin
  Result := false;
  try
    xmlDoc := CreateXMLDoc;
    if ( xmlDoc.Load( fileName ) ) then begin
      Result := LoadXML( xmlDoc.XML );
    end else begin
      raise Exception.Create('Could not load XML from file ' + fileName + '.  Are you sure it exists and is it well formed?');
    end;
  finally
  end;
End;

function TI2XJob.LoadXML(XMLString: string): boolean;
var
  xmlDoc: IXMLDocument;
  nod, nodItem  : IXMLElement;
  oNod : IXMLNode;
  parsed : boolean;
  xmlNodeList, xmlSubNodeList, nodExecList : IXMLNodeList;
  item: TExecListItem;
  list : TExecList;
  iNodeIdx, iExecIdx : integer;
Begin
  try
    Result := false;
    Self.Clear;
    xmlDoc := CreateXMLDoc;
    parsed := xmlDoc.loadXML( XMLString );
    if ( not parsed ) then begin
      raise Exception.Create( 'XML Passed to function could not be parsed.' );
    end;
    self.Name := xmlDoc.DocumentElement.attributes.getNamedItem('name').text;
    xmlNodeList := xmldoc.DocumentElement.ChildNodes;
    for iNodeIdx := 0 to xmlNodeList.length - 1 do begin
      nod := IXMLElement(xmlNodeList.Item[iNodeIdx]);
      if ( nod.nodeType = ELEMENT_NODE ) then begin
        if (nod.nodeName = 'root_path' ) then
          self.FRootPath := nod.Text;
        if (nod.nodeName = 'image_path' ) then
          self.FImagePath := nod.Text;
        if (nod.nodeName = 'output_path' ) then
          self.FOutputPath := nod.Text;
        if (nod.nodeName = 'template_file_name' ) then
          self.FTemplateFileName := nod.Text;
        if (nod.nodeName = 'restartable' ) then
          self.FRestartable := (nod.Text = 'Y');
        if (nod.nodeName = 'job_list' ) then begin
          nodExecList := nod.SelectNodes('exec');
          for iExecIdx := 0 to nodExecList.length - 1 do begin
            list := TExecList.Create();
            list.FromXML( nodExecList.Item[iExecIdx] );
            self.FJobList.Add( list );
          end;
        end;
      end;
    end;
  finally
  end;
  Result := true;
End;

function TI2XJob.Save(): boolean;
Begin
  Result := self.SaveToFile( self.JobFileName );
End;

function TI2XJob.SaveToFile(fileName: TFileName): boolean;
var
  xmlDoc: IXMLDocument;
Begin
  Result := false;
  try
    xmlDoc := CreateXMLDoc;
    if ( xmlDoc.LoadXML( AsXML() ) ) then begin
      xmlDoc.Save( fileName );
      Result := true;
    end else begin
      raise Exception.Create('File ' + fileName + ' could not be loaded on into Job.');
    end;
  finally
  end;
End;

procedure TI2XJob.SetExecItem(Index: Integer; const Value: TExecList);
begin
  FJobList.Items[ Index ] := TObject( Value );
end;

procedure TI2XJob.setImagePath(const Value: string);
begin
  FImagePath := Value;
  if ( ( Length(FImagePath) > 0 ) and ( FImagePath[Length( FImagePath )] <> '\') ) then
    FImagePath := FImagePath + '\';
end;

procedure TI2XJob.setOutputPath(const Value: string);
begin
  FOutputPath := Value;
  if ( ( Length(FOutputPath) > 0 ) and ( FOutputPath[Length( FOutputPath )] <> '\') ) then
    FOutputPath := FOutputPath + '\';
end;

procedure TI2XJob.setRootPath(const Value: string);
begin
  FRootPath := Value;
  if (( Length(FRootPath) > 0 ) and ( FRootPath[Length( FRootPath )] <> '\' )) then
    FRootPath := FRootPath + '\';
end;

{ TI2XJobExecute }

constructor TI2XJobExecute.Create( const JobFileName : string );
begin
  FDebugOutputOverride := '';
  self.JobFileName := JobFileName;
  Fjob := TI2XJob.Create();
  self.FJobIsLoaded := Fjob.LoadFromFile( JobFileName );
  FileDir := CFileDir.Create();
  FTemplate := CTemplate.Create();
  DWImage := TDWImage.Create();

  PluginManager.SearchAndLoadPlugins( DLLSearchPath );
  Options.SetupDefaultTests( PluginManager );

  OCR := TI2XOCRProcJobThread.Create();
  OCR.DebugLevel := DebugLevel;
  OCR.DebugPath := FileDir.GetSystemTempDir + TEMP_DIR_QUAL;
  //OCR.OCREngineTests := Options.DLLOCRTestsSelect;
  OCR.OCREngineTests := Options.DLLOCRTestsComplete;
  OCR.OnStatusChangeEvent := self.OnStatusChange;
  OCR.OnDebugMessageEvent := OnDebugMessage;
  OCR.EnableJobEvents := false;
  OCR.PluginManager := PluginManager;

  ImageProc := TI2XImageProcJobThread.Create();
  ImageProc.DebugLevel := DebugLevel;
  ImageProc.DebugPath := FileDir.GetSystemTempDir + TEMP_DIR_QUAL;
  ImageProc.EnableJobEvents := false;
  ImageProc.OnStatusChangeEvent := OnStatusChange;
  ImageProc.OnDebugMessageEvent := OnDebugMessage;
  ImageProc.PluginManager := PluginManager;
end;

destructor TI2XJobExecute.Destroy;
begin
  FreeAndNil( Fjob );
  FreeAndNil( FileDir );
  FreeAndNil( OCR );
  FreeAndNil( ImageProc );
  FreeAndNil( FTemplate );
  FreeAndNil( DWImage );
end;

function TI2XJobExecute.GetDebugOutputOverride: TFileName;
begin
  Result := FDebugOutputOverride;
end;

function TI2XJobExecute.GetJobFileName: TFileName;
begin
  Result := FJobFileName;
end;

function TI2XJobExecute.DoOCR(const sMMID: string; Template: CTemplate; var ResultXML : string ): boolean;
//function TI2XJobExecute.DoOCR( const ImageFileName : string; Template : CTemplate ): boolean;
Begin
  result := false;

  OCR.Template := Template;
  OCR.ImageMemoryMapID := sMMID;
  OCR.ImageFileName := FCurrentImage;
  Result := OCR.ExecuteOCR();
  if ( Result ) then begin
    ResultXML := OCR.ResultXML;
  end;
  //self.OnJobEnd( OCR.TestCount, OCR.ResultXML );
End;

function TI2XJobExecute.DoImageProcessing(const ImageFileName: string; Template: CTemplate; var ImageMMID : string ): boolean;
//function TI2XJobExecute.DoOCR( const ImageFileName : string; Template : CTemplate ): boolean;
Begin
  result := false;

  if ( ImageProc.DWImage.LoadFromFile( ImageFileName ) and
        FMemoryMapManager.Write( ImageMMID, ImageProc.DWImage ) ) then begin
    //ImageProc.DWImage.SaveToMemoryMap( ImageMMID ) ) then begin
    ImageProc.ImageMemoryMapID := ImageMMID;
    ImageProc.InstructionList.Clear;
    ImageProc.InstructionList.fromString( Template.ImageProcessingInstructionString );
    result := ImageProc.ExecuteImageProc();
  end;
End;

procedure TI2XJobExecute.OnStatusChange( Sender : TObject; TaskID : string; sStatusMessage: string);
begin
  WriteLn( sStatusMessage );
  Dbg( sStatusMessage );
end;

procedure TI2XJobExecute.OnDebugMessage( Sender : TObject; TaskID : string; sDebugMessage : string );
begin
  WriteLn( sDebugMessage );
  Dbg( sDebugMessage );
end;

procedure TI2XJobExecute.ConsoleWrite( const msg : string );
Begin
  OnDebugMessage( self, '', msg );
End;

function TI2XJobExecute.SaveBATFile(): boolean;
var
  sb : TStringBuilder;
  BATFileName : string;
begin
  try
    Result := false;
    sb := TStringbuilder.Create;
    sb.Append( 'REM Generated by Image2XML on ' + FormatDateTime('c', now) );
    sb.Append( #10#13 );
    sb.Append( 'REM This BAT file is automatically generated.. copy it else where if you wish to save any changes you make to it.' );
    sb.Append( #10#13 );
    sb.Append( #10#13 );

    sb.Append( AppPath );
    sb.Append( 'Image2XML.exe ' );

    sb.Append( '-x ' );
    sb.Append( QUOTE );
    sb.Append( DLLSearchPath );
    sb.Append( QUOTE );

    sb.Append( ' ' );
    sb.Append( '-d ' );
    sb.Append( ' ' );
    sb.Append( IntToStr( integer( DebugLevel ) ) );


    sb.Append( ' ' );
    sb.Append( '-j ' );
    sb.Append( QUOTE );
    sb.Append( FJob.JobFileName );
    sb.Append( QUOTE );

    BATFileName := FileDir.ChangeExt( FJob.JobFileName, BAT_FILE_EXT);
    self.FileDir.WriteString( sb.ToString, BATFileName );
    Result := true;
  finally
    FreeAndNil( sb );
  end;
End;

procedure TI2XJobExecute.ConsoleStartMessage();
Begin
    ConsoleWrite( APP_NAME + ' Version ' + sVERSION);
    ConsoleWrite( 'This Image2XML Job was started on ' + sNow );
    ConsoleWrite( 'Job File=' + self.JobFileName );
    ConsoleWrite( 'Parameters from Job File are:' );
    ConsoleWrite( '     Job Name=' + self.FJob.Name );
    ConsoleWrite( '    Root Path=' + self.FRootPathAbsolute );
    ConsoleWrite( 'Job Root Path=' + self.FJobRootPathAbsolute );
    ConsoleWrite( '   Image Path=' + self.FImagePathAbsolute );
    ConsoleWrite( '  Output Path=' + self.FOutputPathAbsolute );
    ConsoleWrite( '     Template=' + self.FJob.TemplateFileName );
    if ( self.FJob.Restartable) then
      ConsoleWrite( '  Restartable=Yes' )
    else
      ConsoleWrite( '  Restartable=No' );
End;

function TI2XJobExecute.PreRunCheck: boolean;
Begin
  Result := true;
  Result := DirectoryExists(FImagePathAbsolute);
  if Not Result then begin
    ConsoleWrite( 'Image Path Does not exist.' );
    Exit;
  end;

  Result := Length(FOutputPathAbsolute) > 0;
  if Not Result then begin
    ConsoleWrite( 'An Output path must be specified.' );
    Exit;
  end;

  if Not DirectoryExists(FOutputPathAbsolute) then begin
    ForceDirectories( FOutputPathAbsolute );
  end;

  Result := Length(FJobRootPathAbsolute) > 0;
  if Not Result then begin
    ConsoleWrite( 'An Job root path must be specified.' );
    Exit;
  end;

  if Not DirectoryExists(FJobRootPathAbsolute) then begin
    ForceDirectories( FJobRootPathAbsolute );
  end;

  FTemplate.LoadFromFile( FJob.TemplateFileName );
  Result := (FTemplate.Elements.Count > 0);
  if Not Result then begin
    ConsoleWrite( 'Template from file name ' + FJob.TemplateFileName + ' could not be loaded.' );
    Exit;
  end;
End;

function TI2XJobExecute.Run: boolean;
var
  FileList, DirList : TStringList;
  iDirList, iFileList : integer;
  sImageToProcess, sOutputPath, sNewOutputFileName : TFileName;
  item : TExecListItem;
  thisJobRun: TExecList;
  sExt, sResultXML, sRelativePath, sMMID : string;
  bOk, bSkip, bNewItem : boolean;
begin
  Result := false;
  self.FProductProducedCount := 0;
  if not ( FJobIsLoaded ) then
    raise Exception.Create( 'Job was not loaded,  cannot execute job.  Make sure that job template with name of (' + self.FJobFileName + ') exists.' );
  try
    if ( Length( FDebugOutputOverride ) = 0 ) then
      FDebugOutputOverride := FileDir.ChangeExt( FJob.JobFileName, LOG_FILE_EXT );
    uDebug.DebugOutputFileName := FDebugOutputOverride;

    FRootPathAbsolute := FileDir.Slash( AbsolutePath( FJob.RootPath ) );
    FJobRootPathAbsolute := FileDir.Slash( FRootPathAbsolute + FJob.FName );
    FImagePathAbsolute := FileDir.Slash( AbsolutePath( FJob.ImagePath ) );
    if (( StringCount( FJob.OutputPath, '\' ) <= 1 )) then
      FOutputPathAbsolute := FileDir.Slash( FJobRootPathAbsolute + FJob.OutputPath )
    else
      FOutputPathAbsolute := FileDir.Slash( AbsolutePath( FJob.OutputPath, FJobRootPathAbsolute ) );
    bOk := true;

    AllocConsole();
    ConsoleStartMessage();
    if ( PreRunCheck () )  then begin

      SaveBATFile();
      ConsoleWrite( 'Reading Images in Image Path...' );
      thisJobRun := TExecList.Create();
      thisJobRun.RunDateTime := Now;
      FJob.Add( thisJobRun );

      DirList := FileDir.GetSubDirList( FImagePathAbsolute );
      DirList.Add( FImagePathAbsolute );
      ConsoleWrite( 'Including the intial root path,  the number of directories to process is:' + IntToStr(DirList.Count) );
      for iDirList := 0 to DirList.Count - 1 do begin
        try
          if ( Integer(DebugLevel ) >= 1 ) then ConsoleWrite( 'Processing path ' + DirList[ iDirList ] );
          FileList := FileDir.GetFileList( FileDir.Slash( DirList[ iDirList ] ) + '*.*' );
          if ( Integer(DebugLevel ) >= 1 ) then ConsoleWrite( ' File Count for this path:' + IntToStr(FileList.Count));

          for iFileList := 0 to FileList.Count - 1 do begin
            bNewItem := false;
            sExt := LowerCase( ExtractFileExt( FileList[ iFileList ] ) );
            if ( not IsIn(sExt, [ '.tif','.tiff', '.jpg', '.jpeg' ]) ) then begin
              if ( Integer(DebugLevel ) >= 2 ) then ConsoleWrite( '  Skipping File ' + FileList[ iFileList ] + ' because it is not a valid image file.');
            end else begin
              if ( Integer(DebugLevel ) >= 1 ) then ConsoleWrite( '  Processing File ' + FileList[ iFileList ]);

              sImageToProcess := FileList[ iFileList ];
              if ( FJob.FileExists( sImageToProcess, item ) ) then begin

                bSkip := false;
                if ( FileExists(item.Output) ) then begin
                  bSkip := ( FileDir.GetFileDateTime( sImageToProcess ) <= item.LastModified );
                  if ( bSkip ) then
                    if ( Integer(DebugLevel ) >= 2 ) then ConsoleWrite( '  Image has already been processed,  Image modified <= Cached Image modified');
                end else begin
                  if ( Integer(DebugLevel ) >= 2 ) then ConsoleWrite( '  Image already has been processed,  but output file is missing,  so reprocessing...');
                end;

              end;
              if ( bSkip ) then begin
                ConsoleWrite( '  Skipping ' + sImageToProcess);
              end else begin

                sResultXML := '';
                //sRelativePath := Replace(ExtractFilePath(sImageToProcess), FImagePathAbsolute, '');
                sRelativePath := StringReplace(ExtractFilePath(sImageToProcess), FImagePathAbsolute, '', [rfReplaceAll, rfIgnoreCase]);
                if (( Length( sRelativePath ) > 0 ) and
                    ( sRelativePath[length(sRelativePath)] <> '\' )) then sRelativePath := sRelativePath + '\';
                sNewOutputFileName := FOutputPathAbsolute +
                    sRelativePath + self.FileDir.ucExtractFileName( sImageToProcess, true ) + '.xml';
                if ( Integer(DebugLevel ) >= 2 ) then ConsoleWrite( '    Performing Image Processing...');

                if ( item <> nil ) then begin
                  FJob.DeleteFile( sImageToProcess );
                  item := nil;
                end;
                if ( item = nil ) then begin
                  item := TExecListItem.Create();
                  item.ImageSource := sImageToProcess;
                  FCurrentImage := sImageToProcess;
                  bNewItem := true;
                end;
                item.Output := sNewOutputFileName;
                item.LastModified := FileDir.GetFileDateTime( sImageToProcess );
                item.Processed := Now;

                try
                  //Create and tear down the memory manager every image... because
                  // delete does not seem to do a good enough job of clearing it out
                  FMemoryMapManager := TI2XMemoryMapManager.Create();
                  bOk := DoImageProcessing(sImageToProcess, FTemplate, sMMID );
                  if ( not bOk ) then ConsoleWrite( '    Image Processing failed.');
                  if ( bOk ) then begin
                    if ( Integer(DebugLevel ) >= 2 ) then ConsoleWrite( '    Performing OCR...');
                    bOk := DoOCR( sMMID, FTemplate, sResultXML );
                  end;
                  if ( bOk ) then begin
                    //if ( Integer(DebugLevel ) >= 2 ) then ConsoleWrite( '    Image successfully processed... results written to ' + sNewOutputFileName );
                    self.FileDir.WriteString( sResultXML, sNewOutputFileName, false );
                    Inc( FProductProducedCount );
                    if ( bNewItem ) then thisJobRun.Add(item);
                    self.FJob.Save();
                    ConsoleWrite( '    Image ' + sImageToProcess + ' processed and written to ' + sNewOutputFileName );
                  end else begin
                    FreeAndNil( item );
                    ConsoleWrite( '    OCR for image failed.');
                  end;
                finally
                  FMemoryMapManager.Free;
                  //FreeAndNil( FMemoryMapManager );
                end;
              end;

            end;
          end;
        finally
          FileList.Free;
        end;
      end;

      ConsoleWrite( ' ' );
      ConsoleWrite( '... Completed reading images' );
      Result := true;
    end;
    ConsoleWrite( 'This Image2XML Job Completed on ' + sNow );
  finally
    FreeConsole();
  end;
end;

procedure TI2XJobExecute.SetDebugOutputOverride(const Value: TFileName);
begin
  FDebugOutputOverride := Value;
end;

procedure TI2XJobExecute.SetJobFileName(const Value: TFileName);
begin
  FJobFileName := Value;
end;

END.
