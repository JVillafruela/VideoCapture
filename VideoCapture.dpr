program VideoCapture;

uses
  Forms,
  VideoCapture_MainForm in 'VideoCapture_MainForm.pas' {FMain},
  VFrames in '.\Common\VFrames.pas',
  VSample in '.\Common\VSample.pas',
  Direct3D9 in '.\Common\DirectX\Direct3D9.pas',
  DirectDraw in '.\Common\DirectX\DirectDraw.pas',
  DirectShow9 in '.\Common\DirectX\DirectShow9.pas',
  DirectSound in '.\Common\DirectX\DirectSound.pas',
  DXTypes in '.\Common\DirectX\DXTypes.pas',
  Settings in 'Settings.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Video Capture';
  Application.CreateForm(TFMain, FMain);
  Application.Run;
end.
