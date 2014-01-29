unit uOmniXML;

interface
uses
  SysUtils,
  uI2XConstants,
  OmniXML;

const
  UNIT_NAME = 'uOmniXML';
  UNASSIGNED='UNASSIGNED';

function GetAttr(const nod: IXMLNode; const AttributeName : string; const DefaultValue : string = UNASSIGNED ) : string; overload;
function GetAttr(const nod: IXMLNode; const AttributeName : string; const DefaultValue : integer ) : integer; overload;
function GetAttr(const nod: IXMLNode; const AttributeName : string; const DefaultValue : Extended ) : Extended; overload;

implementation
function GetAttr(const nod: IXMLNode; const AttributeName : string; const DefaultValue : string ) : string;
Begin
  Result := '';
  if ( DefaultValue = UNASSIGNED ) then begin
    if ( nod = nil ) then
      raise Exception.Create('Passed XML Node Element cannot be nil')
    else if ( nod.attributes.getNamedItem( AttributeName ) = nil ) then
      raise Exception.Create('Attribute with a name of "' + AttributeName + '" does not exist.')
    else begin
      Result := nod.attributes.getNamedItem( AttributeName ).text;
    end;
  end else begin
    try
      if ( nod.attributes.getNamedItem( AttributeName ) = nil ) then
        Result := DefaultValue
      else
        Result := nod.attributes.getNamedItem( AttributeName ).text;
    except
      Result := DefaultValue;
    end;
  end;
End;

function GetAttr(const nod: IXMLNode; const AttributeName : string; const DefaultValue : integer ) : integer; overload;
var
  sRes : string;
Begin
  sRes := GetAttr(nod, AttributeName, IntToStr( DefaultValue ) );
  Result := StrToIntDef( sRes, MININT );
  if ( Result = MININT ) then begin
    raise Exception.Create('The node value of "' + sRes + '" is an invalid integer and could not be converted');
  end;
End;

function GetAttr(const nod: IXMLNode; const AttributeName : string; const DefaultValue : Extended ) : Extended; overload;
const
  EXTMIN = -999999999999.9999;
var
  sRes : string;
Begin
  sRes := GetAttr(nod, AttributeName, FloatToStr(DefaultValue) );
  Result := StrToFloatDef( sRes, EXTMIN );
  if ( Result = EXTMIN ) then begin
    raise Exception.Create('The node value of "' + sRes + '" is an invalid Extended (floating point) and could not be converted');
  end;
End;

END.
