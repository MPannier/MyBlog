program LogFilesIOTest_Threading;

uses
  Vcl.Forms,
  Unit2 in 'Unit2.pas' {Form2},
  LoggerPro.FileAppender in 'LoggerPro\LoggerPro.FileAppender.pas',
  LoggerPro in 'LoggerPro\LoggerPro.pas',
  LoggerPro.GlobalLogger in 'LoggerPro\LoggerPro.GlobalLogger.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
