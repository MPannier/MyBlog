unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    RadioGroup1: TRadioGroup;
    ButtonStart: TButton;
    LabelStatus: TLabel;
    ButtonRemoveLastLine: TButton;
    procedure ButtonStartClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonRemoveLastLineClick(Sender: TObject);
  private
    FLogFileName: TFileName;
    procedure DoLogTStringList(const Txt: string);
    procedure DoLogTFileStream(const Txt: string);
    procedure DoLogTextFile(const Txt: string);

    procedure DoRemoveLastLineTStringList;
    procedure DoRemoveLastLineTFileStream;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

const
  LOG_ENTRY_COUNT = 1000;
  LOREM_IPSUM = ' Lorem ipsum dolor sit amet, consetetur sadipscing';

procedure TForm1.FormCreate(Sender: TObject);
begin
  FLogFileName := IncludeTrailingBackslash(ExtractFilePath(Application.ExeName)) + 'log.txt';
end;

procedure TForm1.ButtonStartClick(Sender: TObject);
var
  c: Cardinal;
  I: Integer;
begin
  if FileExists(FLogFileName) then
    DeleteFile(FLogFileName);

  c := GetTickCount;

  for I := 1 to LOG_ENTRY_COUNT do
  begin
    case RadioGroup1.ItemIndex of
      0: DoLogTStringList(I.ToString + LOREM_IPSUM);
      1: DoLogTFileStream(I.ToString + LOREM_IPSUM);
      2: DoLogTextFile(I.ToString + LOREM_IPSUM);
    end;
  end;

  c := GetTickCount - c;
  LabelStatus.Caption := c.ToString + ' ticks';
end;

procedure TForm1.DoLogTStringList(const Txt: string);
var
  s: TStringList;
begin
  s := TStringList.Create;
  try
    if FileExists(FLogFileName) then
      s.LoadFromFile(FLogFileName);
    s.Add(Txt);
    s.SaveToFile(FLogFileName);
  finally
    s.Free;
  end;
end;

procedure TForm1.DoLogTFileStream(const Txt: string);
var
  F: TFileStream;
  b: TBytes;
begin
  if FileExists(FLogFileName) then
    F := TFileStream.Create(FLogFileName, fmOpenReadWrite)
  else
    F := TFileStream.Create(FLogFileName, fmCreate);
  try
    F.Seek(0, soFromEnd);
    b := TEncoding.Default.GetBytes(Txt + sLineBreak);
    F.Write(b, Length(b));
  finally
    F.Free;
  end;
end;

procedure TForm1.DoLogTextFile(const Txt: string);
var
  F: TextFile;
begin
  AssignFile(F, FLogFileName);
  try
    if FileExists(FLogFileName) then
      Append(f)
    else
      Rewrite(f);
    WriteLn(f,Txt);
  finally
    CloseFile(F);
  end;
end;

procedure TForm1.ButtonRemoveLastLineClick(Sender: TObject);
var
  c: Cardinal;
begin
  if not FileExists(FLogFileName) then
    Exit;

  c := GetTickCount;

  case RadioGroup1.ItemIndex of
    0: DoRemoveLastLineTStringList;
    1: DoRemoveLastLineTFileStream;
    2: ShowMessage('not implemented');
  end;

  c := GetTickCount - c;
  LabelStatus.Caption := c.ToString + ' ticks';
end;

procedure TForm1.DoRemoveLastLineTStringList;
var
  s: TStringList;
begin
  if FileExists(FLogFileName) then
  begin
    s := TStringList.Create;
    try
      s.LoadFromFile(FLogFileName);
      s.Delete(s.Count - 1);
      s.SaveToFile(FLogFileName);
    finally
      s.Free;
    end;
  end;
end;

procedure TForm1.DoRemoveLastLineTFileStream;
var
  LengthLineBreak: Integer;

  function FindLastLineBreakReturnPosition(FS: TStream): Integer;
  var
    Buf: TBytes;
    s : string;
  begin
    FS.Seek(0, soFromEnd);
    //empty file
    if FS.Position <= LengthLineBreak then
      Exit(0);

    SetLength(Buf, LengthLineBreak);
    while true do
    begin
      //ignore last line break during first loop
      FS.Seek(-(LengthLineBreak+1), soFromCurrent);

      if FS.Position = 0 then //start of file
        break;

      FS.Read(Buf, LengthLineBreak);
      s := TEncoding.UTF8.GetString(Buf);

      if (s = sLineBreak) then
        break;
    end;
    Result := FS.Position;
  end;

var
  F: TFileStream;
  LastLineBreakPos: Integer;
begin
  if FileExists(FLogFileName) then
  begin
    LengthLineBreak := Length(sLineBreak);

    F := TFileStream.Create(FLogFileName, fmOpenReadWrite);
    try
      LastLineBreakPos := FindLastLineBreakReturnPosition(F);
      F.Size := LastLineBreakPos;
    finally
      F.Free;
    end;
  end;
end;

end.
