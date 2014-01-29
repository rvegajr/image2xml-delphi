unit uFileIO;

interface
uses
  Sysutils, FileCtrl, Dialogs, Controls;

const
  UNIT_NAME = 'FileIO';
  CRLF = #13#10;

type
  TOnBeforeHandlerCall = procedure(ErrorNumber : Cardinal; ErrorDescription : String; ErrorSource : String; MiscMsg : String; Cancel : boolean ) of Object;
  TErrorHandlerProc = function(ErrorNumber : Cardinal; ErrorDescription : String; ErrorSource : String; MiscMsg : String ) : Boolean;
  TOnStatusChange = procedure(Sender: TObject; const Msg: string) of Object;
  eFileStatus = (fsClosed, fsOpenRead, fsOpenWrite);
  TOnWrite = procedure(var StringToBeWritten : String; var CancelWrite : boolean) of object;
  CTextFileIO = class
  private
    FAppendMode: Boolean;
    FEndOfFile: Boolean;
    FStatus: eFileStatus;
    FCurrentBytePos: Cardinal;
    FRecordCount: Cardinal;
    FFileName: String;
    FCurrentRecord: String;
    FTextFile : TextFile;
    FOnWrite : TOnWrite;
    FFileSize: Cardinal;
    procedure SetAppendMode(const Value: Boolean);
    procedure SetCurrentRecord(const Value: String);
    procedure SetFileName(const Value: String);
    procedure SetCurrentBytePos(const Value: Cardinal);
    procedure OpenFile;
    procedure CloseFile;
    procedure CommonCreate;
    function bQYNmsg(s: string): boolean;
    procedure Emsg(s: string);
    function GetFileSize(FileName: String): LongInt;
  public
    property OnWrite : TOnWrite read FOnWrite write FOnWrite;
    property FileSize : Cardinal read FFileSize;
    constructor Create; overload;
    constructor Create(FileName : String; AppendMode : boolean = False); overload;
    destructor Destroy; override;
    procedure Free;
    property FileName : String read FFileName write SetFileName;
    property CurrentRecord : String read FCurrentRecord write SetCurrentRecord;
    property CurrentBytePos : Cardinal read FCurrentBytePos write SetCurrentBytePos;
    property EndOfFile : Boolean read FEndOfFile;
    property AppendMode : Boolean read FAppendMode write SetAppendMode;
    property Status : eFileStatus read FStatus;
    property RecordCount : Cardinal read FRecordCount;
    function Read : String;
    procedure WriteRec( RecordToWrite : String = ''; AddCrlf : boolean = True);
    procedure ForceClose;
    procedure Delete( PromptOnDelete : boolean = True );
  end; {CTextFileIO}

  CFileIO = class
  private
    FAppendMode: Boolean;
    FEndOfFile: Boolean;
    FStatus: eFileStatus;
    FCurrentBytePos: Cardinal;
    FRecordCount: Cardinal;
    FFileName: String;
    FCurrentRecord: String;
    FTextFile : TextFile;
    FFile : File;
    FOnWrite : TOnWrite;
    FFileSize: Cardinal;
    FOnStatusChange : TOnStatusChange;
    DataBuffer : array of Char;
    FTotalBytesRead: Cardinal;
    procedure SetAppendMode(const Value: Boolean);
    procedure SetCurrentRecord(const Value: String);
    procedure SetFileName(const Value: String);
    procedure SetCurrentBytePos(const Value: Cardinal);
    procedure OpenFile;
    procedure CloseFile;
    procedure CommonCreate;
    procedure ChangeStatus(const Msg : string );
    function GetBufferSize: Cardinal;
    procedure SetBufferSize(const Value: Cardinal);
    procedure SetTotalBytesRead(const Value: Cardinal);
  public
    property BufferSize : Cardinal read GetBufferSize write SetBufferSize;
    property OnWrite : TOnWrite read FOnWrite write FOnWrite;
    property OnStatusChange : TOnStatusChange read FOnStatusChange write FOnStatusChange;
    property FileSize : Cardinal read FFileSize;
    constructor Create; overload;
    constructor Create(FileName : String; AppendMode : boolean = False); overload;
    destructor Destroy; override;
    procedure Free;
    property FileName : String read FFileName write SetFileName;
    property CurrentRecord : String read FCurrentRecord write SetCurrentRecord;
    property CurrentBytePos : Cardinal read FCurrentBytePos write SetCurrentBytePos;
    property TotalBytesRead : Cardinal read FTotalBytesRead write SetTotalBytesRead;
    property EndOfFile : Boolean read FEndOfFile;
    property AppendMode : Boolean read FAppendMode write SetAppendMode;
    property Status : eFileStatus read FStatus;
    property RecordCount : Cardinal read FRecordCount;
    function Read(BytesToRead : Cardinal = 0) : String;
    procedure WriteRec( RecordToWrite : String = ''; AddCrlf : boolean = True);
    procedure ForceClose;
    procedure Delete( PromptOnDelete : boolean = True );
  end; {CFileIO}

implementation

{ CTextFileIO }
constructor CTextFileIO.Create;
begin
  inherited Create;
  Self.CommonCreate;
end;

procedure CTextFileIO.CloseFile;
begin
  if ( FStatus <> fsClosed ) then begin
    Close(FTextFile);
    FStatus := fsClosed;
  end;
end;

constructor CTextFileIO.Create(FileName: String; AppendMode: boolean);
begin
  inherited Create;
  CommonCreate;
  Self.FileName := FileName;
  Self.AppendMode := AppendMode;
  FEndOfFile := False;
end;

Function CTextFileIO.bQYNmsg( s : string ) : boolean;
begin
  Result := False;
  if MessageDlg(s, mtConfirmation, [mbYes, mbNo], 0) = mrYes then Result := True;
End;

procedure CTextFileIO.Delete(PromptOnDelete: boolean);
var bDeleteFile : boolean;
begin
  //if ( FStatus <> fsClosed ) then CloseFile;
  bDeleteFile := False;
  if FileExists(FFileName) then begin
    if not ( PromptOnDelete ) then begin
      bDeleteFile := True;
    end else begin
      if (bQYNmsg('Are you sure you wish to delete ' + QuotedStr(FFileName) + '?')) then
        bDeleteFile := True;
    end; {else}
  end; {if File Exists}
  if ( bDeleteFile ) then DeleteFile( PChar( FileName));
end;

destructor CTextFileIO.Destroy;
begin
  if ( FStatus <> fsClosed ) then CloseFile;
end;

procedure CTextFileIO.ForceClose;
begin
  CloseFile;
end;

procedure CTextFileIO.Emsg( s : string );
begin
  MessageDlg(s, mtError,[mbOk], 0);
End;

procedure CTextFileIO.OpenFile;
var sLastAction : String;
Begin
  try
    case FStatus of
      fsOpenWrite : begin
        sLastAction := 'File Status=fsOpenWrite';
        ForceDirectories(ExtractFilePath(FFileName));
        if (( FAppendMode ) and ( FileExists(FFileName))) then begin
          AssignFile(FTextFile, FFileName);
          Append(FTextFile);
          sLastAction := sLastAction + ' file opened for Append Mode';
        end else begin
          AssignFile(FTextFile, FFileName);
          Rewrite(FTextFile);
          sLastAction := sLastAction + ' file opened for write Mode';
        end; {else}
      end; {fsOpenWrite}
      fsOpenRead : begin
        sLastAction := 'File Status=fsOpenRead';
        FEndOfFile := False;
        if ( not FileExists(FFileName)) then raise Exception.Create('File ' + FFileName + ' does not exist.');
        sLastAction := 'AssignFile';
        AssignFile(FTextFile, FFileName);
        Reset(FTextFile);
      end; {fsOpenRead}
    end; {case}
    FCurrentBytePos := 0;
  except
    EMsg(UNIT_NAME + ':CTextFileIO.OpenFile( FFileName=' + FFileName + ')');
    raise;
  end; {try}
End;

function CTextFileIO.Read: String;
begin
  if ( FStatus = fsClosed ) then begin
    FStatus := fsOpenRead;
    self.OpenFile;
  end;
  ReadLn( FTextFile, FCurrentRecord);
  if Eof(FTextFile) then FEndOfFile := True;
  result := FCurrentRecord;
  Inc(FCurrentBytePos, Length(FCurrentRecord));
  Inc(FRecordCount);
end;

procedure CTextFileIO.SetAppendMode(const Value: Boolean);
begin
  FAppendMode := Value;
end;

procedure CTextFileIO.SetCurrentBytePos(const Value: Cardinal);
begin
  FCurrentBytePos := Value;
end;

procedure CTextFileIO.SetCurrentRecord(const Value: String);
begin
  FCurrentRecord := Value;
end;

function CTextFileIO.GetFileSize( FileName : String ) : LongInt;
var
  SearchRec : TSearchRec;
  i : integer;
Begin
  result := -1;
  try
    i := FindFirst(FileName, faAnyFile, SearchRec);
    if ( i = 0 ) then begin
      result := SearchRec.Size;
    end;{if}
  except
    EMsg(UNIT_NAME + ':GetFileSize( FileName=' + FileName + ')');
    raise;
  end; {try}
End;

procedure CTextFileIO.SetFileName(const Value: String);
begin
  FFileSize := 0;
  if ( Length(Value) = 0 ) then
    FFileSize := 0
  else
    FFileName := Value;
  if (FileExists(FFileName)) then begin
    FEndOfFile := False;
    FFileSize := GetFileSize( FFileName );
  end else
    FFileSize := 0;
end;



procedure CTextFileIO.WriteRec(RecordToWrite: String; AddCrlf : boolean);
var  bCancelWrite : boolean;
begin
  if ( FStatus = fsClosed ) then begin
    FStatus := fsOpenWrite;
    self.OpenFile;
  end;
  if ( RecordToWrite <> '' ) then FCurrentRecord := RecordToWrite;
  if AddCrlf then FCurrentRecord := FCurrentRecord + #13#10;
  bCancelWrite := False;
  if ( Assigned(FOnWrite)) then FOnWrite(FCurrentRecord, bCancelWrite);
  if ( not bCancelWrite ) then Write(FTextFile, FCurrentRecord);
  Inc(FCurrentBytePos, Length(RecordToWrite));
end;

procedure CTextFileIO.CommonCreate;
begin
  FCurrentBytePos := 0;
  if ( Length(FFileName) = 0 ) then
    FFileName := 'Outfile.txt';
  FStatus := fsClosed;
  FEndOfFile := True;
end;

procedure CTextFileIO.Free;
begin
  if ( FStatus <> fsClosed ) then CloseFile;
  inherited;
end;

{ CFileIO }

procedure CFileIO.ChangeStatus(const Msg: string);
begin

end;

procedure CFileIO.CloseFile;
begin

end;

procedure CFileIO.CommonCreate;
begin

end;

constructor CFileIO.Create(FileName: String; AppendMode: boolean);
begin
  inherited Create;
  Self.CommonCreate;
end;

constructor CFileIO.Create;
begin
  inherited Create;
  Self.CommonCreate;
end;

procedure CFileIO.Delete(PromptOnDelete: boolean);
begin

end;

destructor CFileIO.Destroy;
begin
  inherited;

end;

procedure CFileIO.ForceClose;
begin

end;

procedure CFileIO.Free;
begin

end;

function CFileIO.GetBufferSize: Cardinal;
begin

end;

procedure CFileIO.OpenFile;
begin

end;

function CFileIO.Read(BytesToRead: Cardinal): String;
begin

end;

procedure CFileIO.SetAppendMode(const Value: Boolean);
begin

end;

procedure CFileIO.SetBufferSize(const Value: Cardinal);
begin

end;

procedure CFileIO.SetCurrentBytePos(const Value: Cardinal);
begin

end;

procedure CFileIO.SetCurrentRecord(const Value: String);
begin

end;

procedure CFileIO.SetFileName(const Value: String);
begin

end;

procedure CFileIO.SetTotalBytesRead(const Value: Cardinal);
begin
  FTotalBytesRead := Value;
end;

procedure CFileIO.WriteRec(RecordToWrite: String; AddCrlf: boolean);
begin

end;

END.
