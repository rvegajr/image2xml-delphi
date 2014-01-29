unit uI2XImg;

interface
uses
  Windows,
  Classes,
  MapStream,
  uStrUtil,
  Graphics,
  SysUtils,
  OmniXML,
  uDWImage,
  uI2XConstants,
  uFileDir,
  uHashTable,
  uI2XPlugin;

//GR32, GR32_Image,
const
  UNIT_NAME = 'uI2XImg';

type
  TI2XImg = class(TI2XPlugin)
    private
      Image : TDWImage;
      InstructionList : TStringList2;
    protected
      FParms : THashStringTable;
      function ExecuteInstruction( const InstructionString : string ) : boolean; virtual; abstract;
      property Parms : THashStringTable read FParms write FParms;
      function ParseParmString(const ParmString: string; Parms : THashStringTable): boolean; overload;
      function ParseParmString(const ParmString: string): boolean; overload;
      function ParmMustExist( const ParmName : string ) : boolean;
    public
      function GetCapabilities( CapabilitiesList : TStrings ) : integer;
      //function ProcessImage( MemoryMapName : pchar; InstructionList : pchar ) : boolean; virtual; overload;
      function ProcessImage( const MemoryMapName : string; InstructionList : TStrings ) : boolean; virtual; abstract;
      function ProcessImageDLL( MemoryMapName, InstructionList : PChar;
        LoadImageFromMemoryMap : boolean = true ) : boolean; virtual;
      constructor Create( const GUID, ParmFileName : string ); dynamic;
      destructor Destroy; override; 
  end;


implementation

constructor TI2XImg.Create(const GUID, ParmFileName : string);
begin
  inherited Create(GUID, ParmFileName);
  Image := TDWImage.Create;
  InstructionList := TStringList2.Create;
  FParms := THashStringTable.Create;
end;

destructor TI2XImg.Destroy;
begin
  FreeAndNil( Image );
  FreeAndNil( InstructionList );
  FreeAndNil( FParms );
  inherited;
End;

function TI2XImg.ParseParmString(const ParmString : string; Parms : THashStringTable ) : boolean;
var
  strList : TStringList2;
  sParm, sValue : string;
  i : integer;
begin
  Result := false;
  if (Parms = nil) then
    Parms := THashStringTable.Create;
  try
    strList := TStringList2.Create;
    strList.fromString( ParmString, IMGPROC_PARM_SEP );
    if (strList.Count > 0 ) then begin
      for i := 0 to strList.Count - 1 do begin
        if ( i > 0 ) then begin
          sParm := Split(strList[i], '=', 0);
          sValue := Split(strList[i], '=', 1);
          Parms.Add( sParm, sValue );
        end;
      end;
    end;
    Result := true;
  finally
    FreeAndNil( strList );
  end;
End;

function TI2XImg.GetCapabilities( CapabilitiesList : TStrings ) : integer;
var
  xmlNodeList : IXMLNodeList;
  i : integer;
begin
  CapabilitiesList.Add( 'I2XCFG:' + self.XMLParmFile );
  Result := CapabilitiesList.Count;
end;

function TI2XImg.ParmMustExist(const ParmName: string): boolean;
begin
  Result := ( self.FParms.ContainsKey( ParmName ) );
  if ( not Result ) then
    self.ErrorUpdate( 'Required Parm "' + ParmName + '" is missing!', ERROR_PARM_MISSING  );
    //raise PluginException.Create('Required Parm "' + ParmName + '" is missing!', ERROR_PARM_FILE_INVALID, self );
    //raise Exception.Create( 'Required Parm "' + ParmName + '" is missing!');
end;

function TI2XImg.ParseParmString(const ParmString: string): boolean;
begin
  Result := self.ParseParmString( ParmString, self.FParms );
end;

function TI2XImg.ProcessImageDLL(MemoryMapName, InstructionList: pchar;
  LoadImageFromMemoryMap : boolean): boolean;
var
  sMemoryMapName, sInstructionList : string;
  slInstList : TStringList2;
begin
  try
    sMemoryMapName := PChar(MemoryMapName);
    sInstructionList := PChar(InstructionList);
    slInstList := TStringList2.Create;
    slInstList.fromString( sInstructionList );
    if ( LoadImageFromMemoryMap ) then
      self.FMemoryMapManager.Read( sMemoryMapName, Image );
    //Image.LoadFromMemoryMap( sMemoryMapName );
    Result := self.ProcessImage( sMemoryMapName, slInstList );
  finally
    FreeAndNil( slInstList );
  end;
end;

END.
