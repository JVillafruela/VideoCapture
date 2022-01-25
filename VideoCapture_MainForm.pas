unit VideoCapture_MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, jpeg,
  Dialogs, ExtCtrls, StdCtrls, Buttons,
  VFrames, ExtDlgs;

type
  TFMain = class(TForm)
    Panel1: TPanel;
    ComboBox1: TComboBox;
    PaintBox1: TPaintBox;
    Label1: TLabel;
    BitBtn_Start: TBitBtn;
    BitBtn_Stop: TBitBtn;
    BitBtn_Properties: TBitBtn;
    BitBtn_StreamProp: TBitBtn;
    CheckBox_FlipH: TCheckBox;
    BitBtn_SaveJpg: TBitBtn;
    SavePictureDialog1: TSavePictureDialog;
    BitBtn_SaveAs: TBitBtn;
    CheckBox_FlipV: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure BitBtn_StartClick(Sender: TObject);
    procedure BitBtn_StopClick(Sender: TObject);
    procedure BitBtn_PropertiesClick(Sender: TObject);
    procedure BitBtn_StreamPropClick(Sender: TObject);
    procedure BitBtn_SaveJpgClick(Sender: TObject);
    procedure BitBtn_SaveAsClick(Sender: TObject);

  private
    { Private declarations }
    fActivated  : boolean;
    fVideoImage : TVideoImage;
    fVideoBitmap: TBitmap;
    procedure OnNewVideoFrame(Sender : TObject; Width, Height: integer; DataPtr: pointer);
  public
    { Public declarations }
  end;

var
  FMain: TFMain;

implementation

{$R *.dfm}

procedure TFMain.FormCreate(Sender: TObject);
begin
  PaintBox1.Align := alClient;
  fActivated      := false;
  fVideoBitmap    := TBitmap.create;

  // Create instance of our video image class.
  fVideoImage     := TVideoImage.Create;
  // Tell fVideoImage what routine to call whan a new video-frame has arrived.
  // (This way we control painting by ourselves)
  fVideoImage.OnNewVideoFrame := OnNewVideoFrame;
end;



procedure TFMain.OnNewVideoFrame(Sender : TObject; Width, Height: integer; DataPtr: pointer);
var
  i, r,h,w : integer;
  a,b,c,d :TPoint;
begin
  // Retreive latest video image
  fVideoImage.GetBitmap(fVideoBitmap);

  //   Source    Flip H    Flip V   Flip H+V
  //  a-----b   b-----a   d-----c   c-----d
  //  |     |   |     |   |     |   |     |
  //  d-----c   c-----d   a-----b   b-----a
  h := fVideoBitmap.height;
  w := fVideoBitmap.Width;
  a := Point(0,h);
  b := Point(w,h);
  c := Point(w,0);
  d := Point(0,0);
  // Paint image onto screen, either normally or flipped.
  IF CheckBox_FlipH.Checked and not CheckBox_FlipV.Checked then
    Paintbox1.Canvas.CopyRect(Rect(d,b), fVideoBitmap.Canvas, Rect(c,a))
  else if CheckBox_FlipV.Checked  and not CheckBox_FlipH.Checked then
    Paintbox1.Canvas.CopyRect(Rect(d,b), fVideoBitmap.Canvas, Rect(a,c))
  else if CheckBox_FlipH.Checked and CheckBox_FlipV.Checked then
    Paintbox1.Canvas.CopyRect(Rect(d,b), fVideoBitmap.Canvas, Rect(b,d))
  else
    Paintbox1.Canvas.Draw(0, 0, fVideoBitmap);
end;




procedure TFMain.FormActivate(Sender: TObject);
var
  DeviceList : TStringList;
begin
  IF fActivated then
    exit;
  fActivated := true;


  // Get list of available cameras
  DeviceList := TStringList.Create;
  fVideoImage.GetListOfDevices(DeviceList);

  IF DeviceList.Count < 1 then
    begin
      // If no camera has been found, terminate program
      Caption := 'Demo03  [No video device found]';
      //MessageDlg('No video device found.'#10'Application will terminate.', mtError, [mbOK], 0);
      ShowMessage('No video device found.'#10'Application will terminate.');
      Application.Terminate;
      exit;
    end
    else begin
      // If at least one camera has been found.
      Combobox1.items.Assign(DeviceList);
      Combobox1.ItemIndex := 0;
      BitBtn_Start.Enabled := true;
    end;
end;


procedure TFMain.BitBtn_StartClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  BitBtn_Start.Enabled := false;
  Application.ProcessMessages;

  fVideoImage.VideoStart(ComboBox1.Items[ComboBox1.itemindex]);

  BitBtn_Stop.Enabled  := true;
  BitBtn_Properties.Enabled := true;
  BitBtn_StreamProp.Enabled := true;
  BitBtn_SaveJpg.Enabled := true;
  BitBtn_SaveAs.Enabled := true;
  Screen.Cursor := crDefault;
end;


procedure TFMain.BitBtn_StopClick(Sender: TObject);
begin
  FVideoImage.VideoStop;
  BitBtn_Start.Enabled := true;
  BitBtn_Stop.Enabled  := false;
  BitBtn_Properties.Enabled := false;
  BitBtn_StreamProp.Enabled := false;
  BitBtn_SaveJpg.Enabled := false;
  BitBtn_SaveAs.Enabled := false;
end;


procedure TFMain.BitBtn_PropertiesClick(Sender: TObject);
begin
  FVideoImage.ShowProperty;
end;


procedure TFMain.BitBtn_StreamPropClick(Sender: TObject);
begin
  FVideoImage.ShowProperty_Stream;
end;




procedure TFMain.BitBtn_SaveAsClick(Sender: TObject);
var
  Jpg : TJPEGImage;
begin

  IF SavePictureDialog1.Execute then
    begin
      Jpg := TJPEGImage.Create;
      Jpg.Performance := jpBestSpeed;
      Jpg.ProgressiveEncoding := True;
      Jpg.ProgressiveDisplay := True;
      Jpg.Assign(fVideoBitmap);
      Jpg.CompressionQuality := 90;
      Jpg.Compress;
      try
        // Will not save the flipping. Sorry, I'm a lazy guy...
        jpg.SaveToFile(SavePictureDialog1.FileName);
      except
        MessageDlg('Could not save file ' + SavePictureDialog1.FileName + '!', mterror, [mbOK], 0);
      end;
      Jpg.Free;
    end;

end;

procedure TFMain.BitBtn_SaveJpgClick(Sender: TObject);
VAR
  Jpg : TJPEGImage;
begin

    Jpg := TJPEGImage.Create;
    Jpg.Performance := jpBestSpeed;
    Jpg.ProgressiveEncoding := True;
    Jpg.ProgressiveDisplay := True;
    Jpg.Assign(fVideoBitmap);
    Jpg.CompressionQuality := 90;
    try
      jpg.SaveToFile('E:\temp\test.jpg');
    except
      MessageDlg('Could not save jpg file E:\temp\test.jpg', mterror, [mbOK], 0);
    end;
    Jpg.Free;

  end;


end.
