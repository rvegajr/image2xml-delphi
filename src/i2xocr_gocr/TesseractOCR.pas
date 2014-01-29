unit TesseractOCR;
{(C) Copyright 2007, J. Oosting
 Licensed under the Apache License, Version 2.0 (the "License"); 
 you may not use this file except in compliance with the License. 
 You may obtain a copy of the License at 
 http://www.apache.org/licenses/LICENSE-2.0 
 Unless required by applicable law or agreed to in writing, software 
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. 
 See the License for the specific language governing permissions and 
 limitations under the License. } 
interface 
uses Windows, SysUtils,Classes,Graphics,TESSDLL; 


type EOCRDLL = class (SysUtils.Exception); 


type 
  TRecognizer = class 
  private 
    FBitmap:TBitmap; 
    FLanguage: array[0..15] of char; 
    function ConvertEText_Desc2String(EText_Desc:PETEXT_DESC):string; 
    procedure SetLanguage(const Value: string); 
    procedure PrepareOCR; 
    procedure ReleaseOCR; 
    function GetLanguage: string; 
  published 
    constructor Create(aBitmap:TBitmap); 
    destructor Destroy;override; 
    function GetAllWords:string; 
    function GetABlock(block:TRect):string; 
    property Language:string read GetLanguage write SetLanguage; 
  End; 


  procedure GetOCRLanguages(langs:TStrings); 
  function OCRDLLLoaded:boolean; 


implementation 
uses Forms; 


procedure GetOCRLanguages(langs: TStrings); 
var 
  sr:TSearchRec; 
  res:integer; 
  dotpos:integer; 
begin 
  langs.Clear; 
  // look in <appdir>tessdata for language files 
  res:=FindFirst(ExtractFilePath(Application.Exename)+'tessdata\*.inttemp',faAnyFile,sr);
  try 
    while res=0 do 
    begin 
      dotpos:=Pos('.',sr.Name); 
      langs.Add(copy(sr.name,1,dotpos-1)); 
      res:=FindNext(sr); 
    end; 
  finally 
    FindClose(sr); 
  end; 
end; 


function OCRDLLLoaded:boolean; 
begin 
  result:=TessDLLLoaded; 
end; 
{ TRecognizer } 


function TRecognizer.ConvertEText_Desc2String(EText_Desc: PETEXT_DESC): string; 
var 
  i,b:integer; 
  ch:TEANYCODE_CHAR; 
begin 
  result:=''; 
  for I := 0 to EText_Desc^.Count - 1 do 
  begin 
    ch:=EText_Desc^.text[i]; 
    for b:= 0 to ch.blanks - 1 do 
      result:=result+' '; 
    result:=result+chr(ch.char_code); 
    if (ch.formatting and 64) = 64  then   // end of line 
      result:=result+chr(13)+chr(10); 
    if (ch.formatting and 128) = 128  then // end of paragraph 
      result:=result+chr(13)+chr(10)+chr(13)+chr(10); 
  end; 
end; 


constructor TRecognizer.Create(aBitmap: TBitmap); 
const padding=32; 
begin 
  if not OCRDLLLoaded then 
    raise EOCRDLL.Create('Tesseract DLL not loaded'); 
  // make a copy so bitmap conversions will not change the original picture 
  fbitmap:=TBitmap.Create; 
  fBitmap.Assign(aBitmap); 
  FLanguage:='eng'; 
end; 


destructor TRecognizer.Destroy; 
begin 
  FBitmap.Free; 
  inherited; 
end; 


function TRecognizer.GetABlock(block: TRect): string; 
var 
  RecognizedText:PETEXT_DESC; 
begin 
  PrepareOCR; 
  if Integer(FBitmap.ScanLine[0])>Integer(FBitmap.ScanLine[1]) then 
  begin 
    Block.Top:=FBitmap.Height-Block.Top-1; 
    Block.Bottom:=FBitmap.Height-Block.Bottom-1; 
  end; 
  RecognizedText:=TessDllRecognize_a_Block(block.Left,block.Right,block.Top,block.Bottom);
  result:=ConvertEText_Desc2String(RecognizedText); 
  ReleaseOCR; 
end; 


function TRecognizer.GetAllWords: string; 
begin 
  result:=GetABlock(Rect(0,0,fBitmap.width-1,FBitmap.Height-1)); 
end; 


function TRecognizer.GetLanguage: string; 
begin 
  GetLanguage:=FLanguage; 
end; 


procedure TRecognizer.PrepareOCR; 
var 
  bpp:integer; 
  BytesPerLine:integer; 
  VirtualWidth:integer;
begin
  // make sure bitmap is DIB, will hopefully convert some types of bitmaps to recognizable pixelformats
  FBitmap.HandleType:=bmDIB; 
  // convert non-supported bitmap formats and determine bpp 
  case FBitmap.PixelFormat of 
    pfDevice: begin 
      FBitmap.PixelFormat:=pf24bit; 
      bpp:=24; 
    end; 
    pf1bit: bpp:=1; 
    pf4bit: begin 
      FBitmap.PixelFormat:=pf8bit; 
      bpp:=8; 
    end; 
    pf8bit: bpp:=8; 
    pf15bit: begin 
      FBitmap.PixelFormat:=pf24bit; 
      bpp:=24; 
    end; 
    pf16bit: begin 
      FBitmap.PixelFormat:=pf24bit; 
      bpp:=24; 
    end; 
    pf24bit: bpp:=24; 
    pf32bit: begin 
      FBitmap.PixelFormat:=pf24bit; 
      bpp:=24; 
    end; 
  else // pfCustom 
    raise EInvalidGraphic.Create('Graphics format not recognized for OCR'); 
  end; 
  // handle different types of bitmaps
  // Bitmaps in Delphi are 4-byte aligned per line, images in Tesseract can be 1-byte aligned
  // make sure that tesseract thinks lines are 4-byte aligned 
  BytesPerLine:=Integer(FBitmap.ScanLine[0])-Integer(FBitmap.ScanLine[1]); 
  case bpp of 
    1:VirtualWidth:=BytesPerLine*8; 
    8:VirtualWidth:=BytesPerLine; 
  else // 24: 
    fBitmap.Width:=4*((FBitmap.Width+3) div 4); 
    VirtualWidth:=FBitmap.Width; 
  end; 
  if  BytesPerLine>0 then // usually Windows DIB 
    TessDllBeginPageLangBPP(VirtualWidth,FBitmap.Height,FBitmap.ScanLine[FBitmap.Height-1],FLanguage,bpp)
  else // typical TIFF
    TessDllBeginPageUprightBPP(VirtualWidth,FBitmap.Height,FBitmap.ScanLine[0],FLanguage,bpp);
end;


procedure TRecognizer.ReleaseOCR; 
begin 
  TessDllEndPage; 
end; 


procedure TRecognizer.SetLanguage(const Value: string); 
begin 
  StrPCopy(FLanguage, Value); 
end; 


end. 


