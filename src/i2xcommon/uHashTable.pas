unit uHashTable;
{
	THashTable unit - Delphi 1 version
     by kktos, May 1997.
     This code is FREEWARE.
     *** Please, if you enhance it, mail me at kktos@sirius.fr ***
}

{$mode objfpc}{$H+}

interface

uses Classes, RTLConsts, JclContainerIntf, JclHashMaps, JclHashSets, JclArraySets;

const
  UNIT_NAME = 'uHashTable';
  ERROR_MISSING_ENTRY = 'Key ''%s'' does not exist.';
  ERROR_INVALID_INDEX = 'Index ''%s'' is invalid.';
  ERROR_INVALID_KEY_DATATYPE = 'Data Type is not supported as a key for HashTables';
  ERROR_ACTION_NOT_SUPPORTED = 'Action ''%s'' is not supported with this class. It exists purely for backward capability.';
type
  TIndexDataType = (idtInteger, idtString);

  TIndex = record
    DataType: TIndexDataType;

    IntValue: Integer;
    StringValue: AnsiString;

    class operator Implicit(aValue: Integer): TIndex;
    class operator Implicit(const aValue: AnsiString): TIndex;
  end;
	TDeleteType= (dtDelete, dtDetach);
  TKeyList = array of string;

{ Class THashList, from Delphi 2 TList source
	used internally, but you can use it for any purpose
}

	THashItem= record
		key:	longint;
		txt:	string;
    obj:	TObject;
	end;

	THashStringItem= record
		key:	longint;
		txt:	PChar;
    obj:	PChar;
	end;

	PHashItemList = ^THashItemList;
     THashItemList = array[0..0] of THashItem;
	PHashStringItemList = ^THashStringItemList;
     THashStringItemList = array[0..0] of THashStringItem;

  THashList = class(TObject)
  private
    Flist:		    PHashItemList;
    Fcount: 		  integer;
		Fcapacity:	  integer;
    memSize:		  longint;
    FdeleteType:	TDeleteType;
    FFreeObjectsOnDestroy : boolean;
  protected
    procedure Error;
    function Get(Index: TIndex): THashItem;
    procedure Grow;
    procedure Put(Index: TIndex; const Item: THashItem);
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
    function TIndexToIndex(const Index: TIndex): integer;
  public
    constructor Create; overload;
    constructor Create( const initialCapacity : Cardinal ); overload;
    destructor Destroy; override;

    function Add(const Item: THashItem): Integer;
    procedure Clear();
    procedure Delete(Index: Integer);
    procedure Remove(Index: Integer);
    function Expand: THashList;
    function IndexOf(key: longint): Integer;
    procedure Pack;
    property DeleteType: TDeleteType			  read FdeleteType	write FdeleteType;
    property Capacity: Integer				      read FCapacity		write SetCapacity;
    property Count: Integer					        read FCount		write SetCount;
    property FreeObjectsOnDestroy : boolean
        read FFreeObjectsOnDestroy write FFreeObjectsOnDestroy;
		property Items[Index: TIndex]: THashItem	read Get write Put; 	default;
    function Keys(): TKeyList;
    //procedure CopyTo( TargetHT : THashList );
    //procedure CopyFrom( SourceHT : THashList );
  end;

  THashStringList = class(TObject)
  private
    Flist:		    PHashStringItemList;
    Fcount: 		  integer;
		Fcapacity:	  integer;
    memSize:		  longint;
    FdeleteType:	TDeleteType;
  protected
    procedure Error;
    function Get(Index: TIndex): THashStringItem;
    procedure Grow;
    procedure Put(Index: TIndex; const Item: THashStringItem);
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
    function TIndexToIndex(const Index: TIndex): integer;
  public
    constructor Create; overload;
    constructor Create( const initialCapacity : Cardinal ); overload;
    destructor Destroy; override;

    function Add(const Item: THashStringItem): Integer;
    function Keys(): TKeyList;
    procedure Clear();
    procedure Delete(Index: Integer);
    function Expand: THashStringList;
    function IndexOf(key: longint): Integer;
    procedure Pack;
    property DeleteType: TDeleteType			read FdeleteType	write FdeleteType;
    property Capacity: Integer				read FCapacity		write SetCapacity;
    property Count: Integer					read FCount		write SetCount;
	  property Items[Index: TIndex]: THashStringItem	read Get write Put; 	default;
    //procedure CopyTo( TargetHT : THashStringList );
    //procedure CopyFrom( SourceHT : THashStringList );
  end;


{ Class THashTable
	the real hashtable.
}

  THashTable= class(TObject)
  private
		Ftable:	THashList;
    FFreeObjectsOnDestroy : boolean;
		procedure Error;
		function getCount: integer;
          procedure setCount(count: integer);
		function getCapacity: integer;
          procedure setCapacity(capacity: integer);
		function getItem(index: TIndex): TObject;
          procedure setItem(index: TIndex; obj: TObject);
		function getDeleteType: TDeleteType;
          procedure setDeleteType(dt: TDeleteType);
  public
    constructor Create; overload;
    constructor Create( const initialCapacity : Cardinal ); overload;
    destructor Destroy; override;
		procedure Add(const key: string; value: TObject);
		procedure Put(const key: string; value: TObject);
    function Get(const key: string): TObject;
    procedure Delete(const key: string);
    procedure Remove(const key: string);
    procedure Clear(); dynamic;
    procedure Pack;
    property DeleteType: TDeleteType			read getDeleteType	write setDeleteType;
    property Count: integer 					read getCount		write setCount;
    property Capacity: Integer				read getCapacity	write setCapacity;
    property Items[index: TIndex]: TObject	read getItem		write setItem; default;
    property Table: THashList	read Ftable;
    property FreeObjectsOnDestroy : boolean
        read FFreeObjectsOnDestroy write FFreeObjectsOnDestroy;
    function Keys(): TKeyList;
    function ContainsKey(const key: string): boolean;
    //procedure CopyFrom( HTSource : THashTable );
    //procedure CopyTo( HTTarget : THashTable );
  end;

  THashStringTable= class(TObject)
  private
		//Ftable:	THashStringList;
    Ftable:	IJclStrStrMap;
		procedure Error;
		function getCount: integer;
    procedure setCount(count: integer);
		function getCapacity: integer;
    procedure setCapacity(capacity: integer);
		function getItem(index: TIndex): string;
    procedure setItem(index: TIndex; obj: string);
		function getDeleteType: TDeleteType;
    procedure setDeleteType(dt: TDeleteType);
  public
    constructor Create; overload; virtual;
    constructor Create( const initialCapacity : Cardinal ); overload; virtual;
    destructor Destroy; override;
		procedure Add(const key : string; const value: string);
    function Get(const key: string): string; overload;
    function Get(const key, defaultValue: string): string; overload;
    function ContainsKey(const key: string): boolean;
    procedure Delete(const key: string);
    procedure Clear();
    procedure Pack;
    property DeleteType: TDeleteType			read getDeleteType	write setDeleteType;
    property Count: integer 					read getCount		write setCount;
    property Capacity: Integer				read getCapacity	write setCapacity;
    property Items[index: TIndex]: string	read getItem	write setItem;  default;
    property Table: IJclStrStrMap	read Ftable;    // TJclStrStrHashMap
    function Keys(): TKeyList;
    //procedure CopyFrom( HTSource : THashStringTable);
    //procedure CopyTo( HTTarget : THashStringTable);
  end;

function hash(const Value: string): longint;
function SortKeyList( list : TKeyList ) : TKeyList;
function CreateTHashItem(const key: string; value: TObject) : THashItem;
function CreateTHashStringItem(const key : string; value: string) : THashStringItem;
function KeysAsStringList(ht : THashStringTable; list : TStringList ) : integer; overload;
function ValuesAsStringList(ht : THashStringTable; list : TStringList ) : integer; overload;
function KeysAsStringList(ht : THashTable; list : TStringList ) : integer; overload;

implementation

uses SysUtils, Consts;

type
	longArray=	packed array[0..3] of byte;
	longArrayPtr=	^longArray;

	array12=		packed array[0..11] of byte;
	array12Ptr=	^array12;

  longPtr=		^longint;


{ --- Class THashList ---
	brute copy of TList D2 source, with some minors changes
     no comment, see TList
}

{-----------------------------------------------------------------------------}

constructor THashList.Create;
begin
	FdeleteType:= dtDelete;
	FCapacity:= 0;
  FCount:= 0;
  memSize:= 4;
  Flist:= AllocMem(memSize);
  SetCapacity(100);
  FFreeObjectsOnDestroy := true;
end;

constructor THashList.Create(const initialCapacity: Cardinal);
begin
	FdeleteType:= dtDelete;
	FCapacity:= 0;
  FCount:= 0;
  memSize:= 4;
  Flist:= AllocMem(memSize);
  SetCapacity(initialCapacity);
end;

{-----------------------------------------------------------------------------}
destructor THashList.Destroy;
begin
	Clear();
  FreeMem(FList, memSize);
end;

{-----------------------------------------------------------------------------}
function THashList.Add(const Item: THashItem): Integer;
begin
	Result := FCount;
	if(Result = FCapacity) then Grow;
  Initialize( FList^[Result] );
	FList^[Result].key := Item.key;
	FList^[Result].txt := Item.txt;
	FList^[Result].obj := Item.obj;
	Inc(FCount);
end;

{-----------------------------------------------------------------------------}
procedure THashList.Clear();
var
	i:	integer;
begin
    for i := FCount - 1 downto 0 do begin
      //StrDispose( FList^[i].txt );
      FList^[i].txt := '';
      if ( FFreeObjectsOnDestroy ) then begin
	      if ( FList^[i].obj <> nil ) then
          FList^[i].obj.Free;
      end;
    end;

  FreeMem(FList, memSize);
  Flist:= AllocMem(memSize);
	FCapacity:= 0;
  FCount:= 0;
end;
{
procedure THashList.CopyFrom(SourceHT: THashList);
var
  arrKeyList : TKeyList;
  i : cardinal;
begin
  i := 0;
  self.Clear;
  if ( SourceHT.Count > 0 )  then begin
    arrKeyList := SourceHT.Keys;
	  for i := 0 to Length(arrKeyList) - 1 do begin
      self.Add( SourceHT.Items(arrKeyList[i]));
    end;
  end;
End;

procedure THashList.CopyTo(TargetHT: THashList);
var
  arrKeyList : TKeyList;
  i : cardinal;
begin
  i := 0;
  self.Clear;
  if ( self.Count > 0 )  then begin
    arrKeyList := self.Keys;
	  for i := 0 to Length(arrKeyList) - 1 do begin
      TargetHT.Add( self.Items(arrKeyList[i]));
    end;
  end;
End;
}
{-----------------------------------------------------------------------------}
{ know BC++ ? remember TArray::Destroy ? renames delete 'cause destroy...
	if not, Delete remove the item from the list AND dispose the object
}
procedure THashList.Delete(Index: Integer);
begin
	if((Index < 0) or (Index >= FCount)) then Error;
  FList^[Index].txt := '';
	FList^[Index].obj.Free;
	Dec(FCount);
	if( Index < FCount ) then begin
    //StrDispose( FList^[Index].txt );
		System.Move(FList^[Index + 1], FList^[Index], (FCount - Index) * SizeOf(THashItem));
  end;
end;

//Remove will remove the entry without calling the objects FREE/DESTROY method.
procedure THashList.Remove(Index: Integer);
begin
	if((Index < 0) or (Index >= FCount)) then Error;
  FList^[Index].txt := '';
	Dec(FCount);
	if( Index < FCount ) then begin
		System.Move(FList^[Index + 1], FList^[Index], (FCount - Index) * SizeOf(THashItem));
  end;
end;

{-----------------------------------------------------------------------------}
procedure THashList.Error;
begin
	//raise EListError.CreateRes(SListIndexError);
	raise Exception.Create('List Index Error');
end;

{-----------------------------------------------------------------------------}
function THashList.Expand: THashList;
begin
	if(FCount = FCapacity) then Grow;
	Result:= Self;
end;

{-----------------------------------------------------------------------------}
function THashList.Get(Index: TIndex): THashItem;
var
  idx : integer;
begin
  idx := self.TIndexToIndex( Index );
  if (( idx = -1 ) and ( Index.DataType = idtString )) then begin
    self.Add( CreateTHashItem( Index.StringValue, nil ) );
    idx := self.TIndexToIndex( Index );
    Result.key:= FList^[idx].key;
    Result.obj:= FList^[idx].obj;
  end else begin
    if ((idx < 0) or (idx >= FCount))  then
      raise Exception.Create( Format(ERROR_INVALID_INDEX, [IntToStr(idx)]) );
    Result.key:= FList^[idx].key;
    Result.obj:= FList^[idx].obj;
  end;
end;

{-----------------------------------------------------------------------------}
procedure THashList.Grow;
var
  Delta: Integer;
begin
	if FCapacity > 8 then Delta := 16
     else	if FCapacity > 4 then Delta := 8
     else	Delta := 4;
	SetCapacity(FCapacity + Delta);
end;

{-----------------------------------------------------------------------------}
function THashList.IndexOf(key: longint): Integer;
begin
	Result := 0;
	while (Result < FCount) and (FList^[Result].key <> key) do Inc(Result);
	if Result = FCount then Result:= -1;
end;

function THashList.Keys() : TKeyList;
var
  arrKeyList : TKeyList;
  i : cardinal;
begin
  i := 0;
  if ( self.Count > 0 )  then begin
    SetLength(arrKeyList, self.Count);
	  for i := FCount - 1 downto 0 do
	  	arrKeyList[i] := FList^[i].txt;
    result := arrKeyList;
  end else begin
    result := nil;
  end;
End;

function THashList.TIndexToIndex(const Index: TIndex) : integer;
begin
  Result := -1;
  case Index.DataType of
    idtInteger:
      begin
        Result := Index.IntValue;
      end;
    idtString:
      begin
	      Result := IndexOf(hash( Index.StringValue ));
      end;
    else
      raise Exception.Create( ERROR_INVALID_KEY_DATATYPE );
  end;
End;

{-----------------------------------------------------------------------------}
procedure THashList.Put(Index: TIndex; const Item: THashItem);
var
  idx : integer;
Begin
  idx := self.TIndexToIndex( Index );
	if (idx < 0) or (idx >= FCount) then Error;
	//FList^[Index].key:= Item.key; //The key cannot change on a put
	//FList^[Index].txt:= Item.txt;
  if (( FList^[idx].obj <> nil )  and
      ( FList^[idx].obj <> Item.obj )) then
    FList^[idx].obj.Free;
	FList^[idx].obj:= Item.obj;
end;

{-----------------------------------------------------------------------------}
procedure THashList.Pack;
var
  i: Integer;
begin
	for i := FCount - 1 downto 0 do
	  	if FList^[i].obj = nil then Delete(i);
end;

{-----------------------------------------------------------------------------}
procedure THashList.SetCapacity(NewCapacity: Integer);
begin
	if((NewCapacity < FCount) or (NewCapacity > MaxListSize)) then Error;
	if(NewCapacity <> FCapacity) then begin
		//FList:= ReallocMem(FList, memSize, NewCapacity * SizeOf(THashItem));
		ReallocMem(FList, NewCapacity * SizeOf(THashItem));
    memSize:= NewCapacity * SizeOf(THashItem);
		FCapacity:= NewCapacity;
	end;
end;

{-----------------------------------------------------------------------------}
procedure THashList.SetCount(NewCount: Integer);
begin
	if((NewCount < 0) or (NewCount > MaxListSize)) then Error;
	if(NewCount > FCapacity) then SetCapacity(NewCount);
	if(NewCount > FCount) then
		FillChar(FList^[FCount], (NewCount - FCount) * SizeOf(THashItem), 0);
	FCount:= NewCount;
end;

constructor THashStringList.Create(const initialCapacity: Cardinal);
begin
	FdeleteType:= dtDelete;
	FCapacity:= 0;
  FCount:= 0;
  memSize:= 4;
  Flist := AllocMem(memSize);
  SetCapacity(initialCapacity);
end;

constructor THashStringList.Create;
begin
	FdeleteType:= dtDelete;
	FCapacity:= 0;
  FCount:= 0;
  memSize:= 4;
  Flist := AllocMem(memSize);
  SetCapacity(100);
end;

function THashStringList.TIndexToIndex(const Index: TIndex) : integer;
begin
  Result := -1;
  case Index.DataType of
    idtInteger:
      begin
        Result := Index.IntValue;
      end;
    idtString:
      begin
	      Result := IndexOf(hash( Index.StringValue ));
      end;
    else
      raise Exception.Create( ERROR_INVALID_KEY_DATATYPE );
  end;
End;


{-----------------------------------------------------------------------------}
destructor THashStringList.Destroy;
begin
	Clear();
  FreeMem(FList, memSize);
end;

{-----------------------------------------------------------------------------}
function THashStringList.Add(const Item: THashStringItem): Integer;
begin
	Result := FCount;
	if(Result = FCapacity) then Grow;
  Initialize( FList^[Result] );
	FList^[Result].key:= Item.key;
	FList^[Result].obj:= Item.obj;
	FList^[Result].txt:= Item.txt;
	Inc(FCount);
end;

{-----------------------------------------------------------------------------}
procedure THashStringList.Clear();
var
	i:	integer;
begin
  for i := FCount - 1 downto 0 do begin
    FList^[i].txt := '';
    FList^[i].obj := '';
    //StrDispose( FList^[i].txt );
    //StrDispose( FList^[i].obj );
  end;

  FreeMem(FList, memSize);
  Flist:= AllocMem(memSize);
	FCapacity:= 0;
  FCount:= 0;
end;
{
procedure THashStringList.CopyFrom(SourceHT: THashStringList);
var
  arrKeyList : TKeyList;
  i : cardinal;
begin
  i := 0;
  self.Clear;
  if ( SourceHT.Count > 0 )  then begin
    arrKeyList := SourceHT.Keys;
	  for i := 0 to Length(arrKeyList) - 1 do begin
      self.Add( SourceHT.Items(arrKeyList[i]));
    end;
  end;
End;

procedure THashStringList.CopyTo(TargetHT: THashStringList);
var
  arrKeyList : TKeyList;
  i : cardinal;
begin
  i := 0;
  self.Clear;
  if ( self.Count > 0 )  then begin
    arrKeyList := self.Keys;
	  for i := 0 to Length(arrKeyList) - 1 do begin
      TargetHT.Add( self.Items(arrKeyList[i]));
    end;
  end;
End;
}
{-----------------------------------------------------------------------------}
{ know BC++ ? remember TArray::Destroy ? renames delete 'cause destroy...
	if not, Delete remove the item from the list AND dispose the object
}
procedure THashStringList.Delete(Index: Integer);
begin
	if((Index < 0) or (Index >= FCount)) then Error;
	Dec(FCount);
	if(Index < FCount) then begin
    StrDispose(FList^[Index].txt);
    StrDispose(FList^[Index].obj);
    //FList^[Index].txt := '';
    //FList^[Index].obj := '';
    //StrDispose( FList^[Index].txt );
    //StrDispose( FList^[Index].obj );
		System.Move(FList^[Index + 1], FList^[Index], (FCount - Index) * SizeOf(THashStringItem));
  end;
end;

{-----------------------------------------------------------------------------}
procedure THashStringList.Error;
begin
	//raise EListError.CreateRes(SListIndexError);
	raise Exception.Create('List Index Error');
end;

{-----------------------------------------------------------------------------}
function THashStringList.Expand: THashStringList;
begin
	if(FCount = FCapacity) then Grow;
	Result:= Self;
end;

{-----------------------------------------------------------------------------}
function THashStringList.Get(Index: TIndex): THashStringItem;
var
  idx : integer;
begin
  idx := self.TIndexToIndex( Index );
  if (( idx = -1 ) and ( Index.DataType = idtString )) then begin
    self.Add( CreateTHashStringItem( Index.StringValue, '' ) );
    idx := self.TIndexToIndex( Index );
    Result := FList^[idx];
    //Result.key:= FList^[idx].key;
    //Result.obj:= FList^[idx].obj;
  end else begin
	  if ((idx < 0) or (idx >= FCount))  then
      raise Exception.Create( Format(ERROR_INVALID_INDEX, [IntToStr(idx)]) );
    Result := FList^[idx];
	  //Result.key:= FList^[idx].key;
	  //Result.obj:= FList^[idx].obj;
  end;
end;

{-----------------------------------------------------------------------------}
procedure THashStringList.Grow;
var
  Delta: Integer;
begin
	if FCapacity > 8 then Delta := 16
     else	if FCapacity > 4 then Delta := 8
     else	Delta := 4;
	SetCapacity(FCapacity + Delta);
end;

{-----------------------------------------------------------------------------}
function THashStringList.IndexOf(key: longint): Integer;
begin
	Result := 0;
	while (Result < FCount) and (FList^[Result].key <> key) do Inc(Result);
	if Result = FCount then Result:= -1;
end;

function THashStringList.Keys: TKeyList;
var
  arrKeyList : TKeyList;
  i : cardinal;
begin
  i := 0;
  if ( self.Count > 0 )  then begin
    SetLength(arrKeyList, self.Count);
	  for i := FCount - 1 downto 0 do
	  	arrKeyList[i] := self.Flist^[i].txt;
    result := arrKeyList;
  end else begin
    result := nil;
  end;
end;

{-----------------------------------------------------------------------------}
procedure THashStringList.Put(Index: TIndex; const Item: THashStringItem);
var
  idx : integer;
begin
  idx := self.TIndexToIndex( Index );
	if ((idx < 0) or (idx >= FCount))  then
    raise Exception.Create( Format(ERROR_INVALID_INDEX, [IntToStr(idx)]) );
	FList^[idx].key:= Item.key;
	FList^[idx].txt:= Item.txt;
	FList^[idx].obj:= Item.obj;
end;

{-----------------------------------------------------------------------------}
procedure THashStringList.Pack;
var
  i: Integer;
begin
	for i := FCount - 1 downto 0 do
	  	if Length( FList^[i].obj ) = 0 then Delete(i);
end;

{-----------------------------------------------------------------------------}
procedure THashStringList.SetCapacity(NewCapacity: Integer);
begin
	if((NewCapacity < FCount) or (NewCapacity > MaxListSize)) then Error;
	if(NewCapacity <> FCapacity) then begin
		//FList:= ReallocMem(FList, memSize, NewCapacity * SizeOf(THashStringItem));
		//FList:= ReallocMem(FList, NewCapacity * SizeOf(THashStringItem));
		ReallocMem(FList, NewCapacity * SizeOf(THashStringItem));
    memSize:= NewCapacity * SizeOf(THashStringItem);
		FCapacity:= NewCapacity;
	end;
end;

{-----------------------------------------------------------------------------}
procedure THashStringList.SetCount(NewCount: Integer);
begin
	if((NewCount < 0) or (NewCount > MaxListSize)) then Error;
	if(NewCount > FCapacity) then SetCapacity(NewCount);
	if(NewCount > FCount) then
		FillChar(FList^[FCount], (NewCount - FCount) * SizeOf(THashStringItem), 0);
	FCount:= NewCount;
end;

{ --- Class THashTable ---
	it's just a list of THashItems.
     you provide a key (string) and an object;
     a unique numeric key (longint) is compute (see hash);
     when you get an object, you provide string key, and as fast as possible
     the object is here.
     Really fast;
     Really smart, because of string keys.
}


{-----------------------------------------------------------------------------}
constructor THashTable.Create;
begin
	inherited Create;
  FreeObjectsOnDestroy := true;
  Ftable:= THashList.Create;
end;

constructor THashTable.Create(const initialCapacity: Cardinal);
begin
	inherited Create;
  FreeObjectsOnDestroy := true;
  Ftable:= THashList.Create( initialCapacity );
end;

{-----------------------------------------------------------------------------}
destructor THashTable.Destroy;
begin
  Ftable.FreeObjectsOnDestroy := self.FreeObjectsOnDestroy;
	Ftable.Free;
	inherited Destroy;
end;

{-----------------------------------------------------------------------------}
procedure THashTable.Error;
begin
	//raise EListError.CreateRes(SListIndexError);
	raise Exception.Create('List Index Error');
end;

{-----------------------------------------------------------------------------}
{
	Add 'value' object with key 'key'
}
procedure THashTable.Add(const key: string; value: TObject);
var
	item:	THashItem;
Begin
  Initialize( item );
	item.key:= hash(key);
  item.txt := key;
  item.obj:= value;
	Ftable.Add(item);
End;

{-----------------------------------------------------------------------------}
{
	Get object with key 'key'
}
function THashTable.ContainsKey(const key: string): boolean;
var
	index:	integer;
begin
	index:= Ftable.IndexOf(hash( key ));
  result:=(index >= 0);
end;
{
procedure THashTable.CopyFrom(HTSource: THashTable);
begin
  Ftable.CopyFrom( HTSource );
end;

procedure THashTable.CopyTo(HTTarget: THashTable);
begin
  Ftable.CopyTo( HTTarget );
end;
}
function THashTable.Get(const key: string): TObject;
var
	index:	integer;
begin
  if ( not ContainsKey(key )) then
    raise Exception.Create( Format(ERROR_MISSING_ENTRY, [key]) );
	index:= Ftable.IndexOf(hash(key));
  result:= Ftable[index].obj;
end;

{-----------------------------------------------------------------------------}
{
	Delete (remove item, dispose object) object with key 'key'
}
procedure THashTable.Delete(const key: string);
var
	index:	integer;
begin
	index:= Ftable.IndexOf( hash( key ) );
  if( index >= 0) then
    Ftable.Delete(index);
end;

{-----------------------------------------------------------------------------}
{
	Clear the list; i.e: remove all the items (detach or delete depending of 'dt')
}
procedure THashTable.Clear();
begin
	Ftable.Clear();
end;

{-----------------------------------------------------------------------------}
procedure THashTable.Pack;
begin
	Ftable.Pack;
end;

procedure THashTable.Put(const key: string; value: TObject);
var
	index:	integer;
  item : THashItem;
begin
  if ( not ContainsKey( key )) then begin
    self.Add( key, value );
    //raise Exception.Create( Format(ERROR_MISSING_ENTRY, [key]) );
    //Ftable.add( item );
  end else begin
    index:= Ftable.IndexOf(hash(key));
    item.key := Ftable[index].key;
    item.txt := Ftable[index].txt;
    item.obj := value;
    Ftable.Put( index, item );
  end;
end;

procedure THashTable.Remove(const key: string);
var
	index:	integer;
begin
	index:= Ftable.IndexOf( hash( key ) );
  if( index >= 0) then
    Ftable.Remove(index);
end;

{-----------------------------------------------------------------------------}
function  THashTable.getCount: integer;				begin result:= Ftable.Count; end;
procedure THashTable.setCount(count: integer);		begin Ftable.Count:= count; end;
function  THashTable.getCapacity: integer;			begin result:= Ftable.Capacity; end;
procedure THashTable.setCapacity(capacity: integer);	begin Ftable.Capacity:= capacity; end;
function  THashTable.getDeleteType: TDeleteType;		begin result:= Ftable.DeleteType; end;
procedure THashTable.setDeleteType(dt: TDeleteType);	begin Ftable.DeleteType:= dt; end;
function  THashTable.getItem(index: TIndex): TObject;	begin result:= Ftable[index].obj; end;
function  THashTable.Keys: TKeyList; begin result:= Ftable.Keys; end;

{-----------------------------------------------------------------------------}
procedure THashTable.setItem(index: TIndex; obj: TObject);
var
	item :	THashItem;
begin
  item.txt := Ftable[index].txt;
	item.key := Ftable[index].key;
  item.obj := obj;
  if (( Ftable[index].obj <> nil )  and
      ( Ftable[index].obj <> obj )) then
    Ftable[index].obj.Free;
	Ftable[index] := item;
end;

constructor THashStringTable.Create;
begin
	inherited Create;
  Ftable:= TJclStrStrHashMap.Create;
end;

constructor THashStringTable.Create(const initialCapacity: Cardinal);
begin
	inherited Create;
  Ftable:= TJclStrStrHashMap.Create( initialCapacity );
end;

{-----------------------------------------------------------------------------}
destructor THashStringTable.Destroy;
begin
  Ftable.Clear;
	//Ftable.Free;
	inherited Destroy;
end;

{-----------------------------------------------------------------------------}
procedure THashStringTable.Error;
begin
	raise Exception.Create('List Index Error');
end;

function THashStringTable.Get(const key, defaultValue: string): string;
var
	index:	integer;
begin
  if ( not ContainsKey( key )) then begin
    result := defaultValue;
  end else begin
    result := Ftable.Items[ key ];
  end;
end;

{-----------------------------------------------------------------------------}
{
	Add 'value' object with key 'key'
}
procedure THashStringTable.Add(const key : string; const value: string);
var
	item:	THashStringItem;
begin
  Ftable.PutValue( key, value );
end;

{-----------------------------------------------------------------------------}
{
	Get object with key 'key'
}
function THashStringTable.Get(const key: string): string;
begin
  if ( not ContainsKey( key ) ) then
    raise Exception.Create( Format(ERROR_MISSING_ENTRY, [key]) );
  result:= Ftable.Items[ key ];
end;

{-----------------------------------------------------------------------------}
{
	Delete (remove item, dispose object) object with key 'key'
}
procedure THashStringTable.Delete(const key: string);
begin
  if (ContainsKey(key)) then
    fTable.Remove( key );
end;

{-----------------------------------------------------------------------------}
{
	Clear the list; i.e: remove all the items (detach or delete depending of 'dt')
}
procedure THashStringTable.Clear();
begin
	Ftable.Clear();
end;

function THashStringTable.ContainsKey(const key: string): boolean;
//var
//	index:	integer;
begin
  result:= Ftable.ContainsKey( key );
	//index:= Ftable.IndexOf(hash( key ));
  //result:=(index >= 0);
end;
{
procedure THashStringTable.CopyFrom(HTSource: THashStringTable);
begin
  FTable.CopyFrom( HTSource );
end;

procedure THashStringTable.CopyTo(HTTarget: THashStringTable);
begin
  FTable.CopyTo( HTTarget );
end;
}
{-----------------------------------------------------------------------------}
procedure THashStringTable.Pack;
begin
end;

{-----------------------------------------------------------------------------}
function  THashStringTable.getCount: integer;				begin result:= Ftable.Size; end;
procedure THashStringTable.setCount(count: integer);
begin
  raise Exception.Create( Format(ERROR_ACTION_NOT_SUPPORTED, ['THashStringTable.setCount']) );
end;
function  THashStringTable.getCapacity: integer;			begin result:= Ftable.Size; end;
procedure THashStringTable.setCapacity(capacity: integer);
begin
  raise Exception.Create( Format(ERROR_ACTION_NOT_SUPPORTED, ['THashStringTable.setCapacity']) );
end;
function  THashStringTable.getDeleteType: TDeleteType;		begin result:= dtDelete; end;
procedure THashStringTable.setDeleteType(dt: TDeleteType);
begin
  raise Exception.Create( Format(ERROR_ACTION_NOT_SUPPORTED, ['THashStringTable.setDeleteType']) );
end;
function  THashStringTable.getItem(index: TIndex): string;
begin
  result:= Ftable.Items[index.StringValue];
end;
function  THashStringTable.Keys: TKeyList;
var
  It: IJclStrIterator;
  kl : TKeyList;
  i : integer;
begin
  It := FTable.KeySet.First;
  i := 0;
  SetLength( kl, FTable.Size );
  while It.HasNext do begin
    kl[i] := It.Next;
    inc( i );
  end;
  result:= kl;
end;

{-----------------------------------------------------------------------------}
procedure THashStringTable.setItem(index: TIndex; obj: string );
var
  item : THashStringItem;
  idx : integer;
begin
  FTable.PutValue( index.StringValue, obj );
end;

function SortKeyList( list : TKeyList ) : TKeyList;
var
  i, j: Integer;
  temp: string;
begin
  // bubble sort
  for i := 0 to length(list) - 1 do begin
    for j := 0 to ( length( list ) - 1 ) - i do begin
      // Condition to handle i=0 & j = 9. j+1 tries to access x[10] which
      // is not there in zero based array
      if ( j + 1 = length(list) ) then
        continue;
      if ( list[j] > list[j+1] ) then begin
        temp := list[j];
        list[j]   := list[j+1];
        list[j+1] := temp;
      end; // endif
    end; // endwhile
  end; // endwhile
  Result := list;
end;

function hash(const Value: string): longint;
var
  i, x: longint;
begin
  Result := 0;
  for i := 1 to Length(Value) do
  begin
    Result := (Result shl 4) + Ord(Value[i]);
    x := Result and $F0000000;
    if (x <> 0) then
      Result := Result xor (x shr 24);
    Result := Result and (not x);
  end;
end;
{ TIndex }

class operator TIndex.Implicit(aValue: Integer): TIndex;
begin
  Result.DataType := idtInteger;
  Result.IntValue := aValue;
end;

class operator TIndex.Implicit(const aValue: AnsiString): TIndex;
begin
  Result.DataType := idtString;
  Result.StringValue := aValue;
end;

function CreateTHashStringItem(const key : string; value: string) : THashStringItem;
var
	item:	THashStringItem;
begin
  Initialize( item );
  item.key:= hash(key);
  item.txt := StrNew(PChar(key));
  item.obj := StrNew(PChar(value));
  //item.txt := key;
  //item.obj := value;
	Result := item;
end;

function CreateTHashItem(const key: string; value: TObject) : THashItem;
var
	item:	THashItem;
Begin
  Initialize( item );
	item.key:= hash(key);
  item.txt := key;
  item.obj:= value;
	Result := item;
End;

function KeysAsStringList(ht : THashStringTable; list : TStringList ) : integer;
var
  arrKeyList : TKeyList;
  i : integer;
Begin
  arrKeyList := ht.Keys;
  for i := 0 to Length(arrKeyList) - 1 do begin
    list.Add( arrKeyList[i] );
  end;
  Result := list.Count;
End;

function ValuesAsStringList(ht : THashStringTable; list : TStringList ) : integer;
var
  arrKeyList : TKeyList;
  i : integer;
Begin
  arrKeyList := ht.Keys;
  for i := 0 to Length(arrKeyList) - 1 do begin
    list.Add( ht.get( arrKeyList[i] ) );
  end;
  Result := list.Count;
End;

function KeysAsStringList(ht : THashTable; list : TStringList ) : integer;
var
  arrKeyList : TKeyList;
  i : integer;
Begin
  arrKeyList := ht.Keys;
  for i := 0 to Length(arrKeyList) - 1 do begin
    list.Add( arrKeyList[i] );
  end;
  Result := list.Count;
End;

END.


