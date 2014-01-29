unit uHashTableXML;

interface

uses
  Classes,
  RTLConsts,
  uHashTable,
  OmniXML,
  JclStrings,
  SysUtils;

const
  UNIT_NAME = 'uHashTableXML';

type
  TFileName = string;
  THashStringTableXML = class(THashStringTable)
  private
    FRootNode : String;
  protected
    function XMLToHashTable( const XMLString : string; ht : THashStringTable ) : boolean;
  public
    property RootNode : string read FRootNode write FRootNode;
    function LoadFromFile( const FileName : TFileName ) : boolean;
    function SaveToFile( const FileName : TFileName ) : boolean;
    function AsXML() : string;
    constructor Create;  override;
    constructor Create( const initialCapacity : Cardinal ); override;
    destructor Destroy; override;
  end;

implementation

{ THashStringTableXML }

constructor THashStringTableXML.Create;
begin
  inherited Create();
  FRootNode := 'THashStringTable';
end;

function THashStringTableXML.AsXML: string;
Var
  sb : TStringBuilder;
  i : integer;
  arrKeyList : TKeyList;
  sKeyName : string;
Begin
  try
    sb := TStringbuilder.Create;
    sb.Append( '<' );
    sb.Append( FRootNode );
    sb.Append( '>' );

    arrKeyList := self.Keys;
    for i := 0 to Length(arrKeyList) - 1 do begin
      sKeyName := arrKeyList[i];
      sb.Append( '<' );
      sb.Append( sKeyName );
      sb.Append( '>' );
      sb.Append( self.Items[ sKeyName ] );
      sb.Append( '</' );
      sb.Append( sKeyName );
      sb.Append( '>' );
    end;
    sb.Append( '</' );
    sb.Append( FRootNode );
    sb.Append( '>' );
    Result := sb.ToString;
  finally
    FreeAndNil( sb );
  end;
End;

constructor THashStringTableXML.Create(const initialCapacity: Cardinal);
begin
  inherited Create(initialCapacity);

end;

destructor THashStringTableXML.Destroy;
begin

  inherited;
end;

function THashStringTableXML.LoadFromFile(const FileName: TFileName): boolean;
var
  xmlDoc: IXMLDocument;
Begin
  Result := false;
  try
    xmlDoc := CreateXMLDoc;
    if ( xmlDoc.Load( fileName ) ) then begin
      Result := XMLToHashTable( xmlDoc.XML, self );
    end else begin
      raise Exception.Create('Could not load XML from file ' + fileName + '.  Are you sure it exists and is it well formed?');
    end;
  finally
  end;
End;

function THashStringTableXML.SaveToFile(const FileName: TFileName): boolean;
var
  xmlDoc: IXMLDocument;
  str : string;
Begin
  Result := false;
  try
    xmlDoc := CreateXMLDoc;
    str := AsXML();
    if ( xmlDoc.LoadXML( AsXML() ) ) then begin
      xmlDoc.Save( fileName );
      Result := true;
    end else begin
      raise Exception.Create('File ' + fileName + ' could not be saved.');
    end;
  finally
  end;
End;

function THashStringTableXML.XMLToHashTable(const XMLString: string;
  ht: THashStringTable): boolean;
var
  xmlDoc: IXMLDocument;
  ele: IXMLElement;
  nod : IXMLNode;
  xmlNodeList : IXMLNodeList;
  iImageIdx : integer;
Begin
  ht.Clear;
  try
    xmlDoc := CreateXMLDoc;
    if ( xmlDoc.LoadXML( XMLString ) ) then begin
      xmlNodeList := xmldoc.DocumentElement.ChildNodes;
      for iImageIdx := 0 to xmlNodeList.length - 1 do begin
        nod := xmlNodeList.Item[iImageIdx];
        if ( nod.HasChildNodes() ) then
          ht.Add( nod.NodeName, nod.FirstChild.XML )
        else if ( nod.nodeType = ELEMENT_NODE ) then begin
          ele := IXMLElement(xmlNodeList.Item[iImageIdx]);
          ht.Add( ele.NodeName, ele.Text );
        end;
      end;

    end else begin
      raise Exception.Create('Could not load XML.  Are you sure it exists and is it well formed?');
    end;
  finally
  end;
End;

END.
