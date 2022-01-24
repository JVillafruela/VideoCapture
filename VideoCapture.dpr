program VideoCapture;

uses
  Forms,
  VideoCapture_MainForm in 'VideoCapture_MainForm.pas' {FMain},
  VFrames in '..\DirectXDelphiWebcamCaptureDemos\Common\VFrames.pas',
  VSample in '..\DirectXDelphiWebcamCaptureDemos\Common\VSample.pas',
  Direct3D9 in '..\DirectXDelphiWebcamCaptureDemos\Common\DirectX\Direct3D9.pas',
  DirectDraw in '..\DirectXDelphiWebcamCaptureDemos\Common\DirectX\DirectDraw.pas',
  DirectShow9 in '..\DirectXDelphiWebcamCaptureDemos\Common\DirectX\DirectShow9.pas',
  DirectSound in '..\DirectXDelphiWebcamCaptureDemos\Common\DirectX\DirectSound.pas',
  DXTypes in '..\DirectXDelphiWebcamCaptureDemos\Common\DirectX\DXTypes.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Video Capture';
  Application.CreateForm(TFMain, FMain);
  Application.Run;
end.
