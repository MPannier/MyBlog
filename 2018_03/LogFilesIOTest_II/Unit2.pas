unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, LoggerPro.GlobalLogger, SyncObjs;

type
  TForm2 = class(TForm)
    RadioGroup1: TRadioGroup;
    LabelStatus: TLabel;
    ButtonStart: TButton;
    procedure ButtonStartClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FLogFileName: TFileName;
    CS: TCriticalSection;
    procedure DoLogTStringList(const Txt: string);
    procedure DoLogTStringListCS(const Txt: string);
    procedure DoLogLoggerPro(const Txt: string);
  public
    { Public-Deklarationen }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

const
  LOG_ENTRY_COUNT = 100;
  LOREM_IPSUM = ' Lorem ipsum dolor sit amet, consetetur sadipscing';

procedure TForm2.FormCreate(Sender: TObject);
begin
  FLogFileName := IncludeTrailingBackslash(ExtractFilePath(Application.ExeName)) + 'log.txt';
  CS := TCriticalSection.Create;
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  CS.Free;
end;

procedure TForm2.ButtonStartClick(Sender: TObject);

  function CreateThread: TThread;
  begin
    Result := TThread.CreateAnonymousThread(
      procedure ()
      var
        I: Integer;
      begin
        for I := 1 to LOG_ENTRY_COUNT do
        begin
          try;
          case RadioGroup1.ItemIndex of
            0: DoLogTStringList(I.ToString + LOREM_IPSUM);
            1: DoLogTStringListCS(I.ToString + LOREM_IPSUM);
            2: DoLogLoggerPro(I.ToString + LOREM_IPSUM);
          end;
          except
          end;
        end;
      end);
    Result.FreeOnTerminate := False;
  end;

var
  c: Cardinal;

var
  T1, T2: TThread;

begin
  if FileExists(FLogFileName) then
    DeleteFile(FLogFileName);

  T1 := CreateThread;
  T2 := CreateThread;

  c := GetTickCount;

  T1.Start;
  T2.Start;

  T1.WaitFor;
  T1.Free;
  T2.WaitFor;
  T2.Free;

  c := GetTickCount - c;
  LabelStatus.Caption := c.ToString + ' ticks';
end;

procedure TForm2.DoLogTStringList(const Txt: string);
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

procedure TForm2.DoLogTStringListCS(const Txt: string);
var
  s: TStringList;
begin
  CS.Enter;
  try
    s := TStringList.Create;
    try
      if FileExists(FLogFileName) then
        s.LoadFromFile(FLogFileName);
      s.Add(Txt);
      s.SaveToFile(FLogFileName);
    finally
      s.Free;
    end;
  finally
    CS.Leave;
  end;
end;

procedure TForm2.DoLogLoggerPro(const Txt: string);
begin
  Log.Info(Txt, '');
end;

end.
