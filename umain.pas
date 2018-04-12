{
This file is part of OvoView
Copyright (C) 2018 Marco Caselli

OvoPlayer is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

}

unit uMain;

{$mode objfpc}
{$ModeSwitch advancedrecords}
{$H+}
interface

uses
  Classes, SysUtils, types, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, Thumbnails, Magick_LCL, uInfo, uAbout, MagickWand, LCLIntf,
  ImageMagick, StdCtrls, Grids, ActnList, Buttons, Menus, StdActns;

type

  { TfrmMain }
  TViewMode = (vmAdapt, vmZoom, vmReal);

  RImage = record
    Wand: PMagickWand;
    Name: string;
    Rotation: integer; // degree
    ZoomRatio: double;
    ViewMode: TViewMode;
    Offset:TPoint;
    VirtualSize:TSize;
    RealSize:TSize;
    procedure Clear;
  end;


  TfrmMain = class(TForm)
    actAbout: TAction;
    actZoomReset: TAction;
    actZoomIn: TAction;
    actZoomOut: TAction;
    actLeft: TAction;
    actRight: TAction;
    actShowInfo: TAction;
    actSlideShow: TAction;
    actNext: TAction;
    actPrev: TAction;
    ActionList: TActionList;
    FileExit1: TFileExit;
    FileOpen1: TFileOpen;
    ImageList1: TImageList;
    imgView: TImage;
    lvThumbnail: TDrawGrid;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    pnlCenter: TPanel;
    sbBottom: TStatusBar;
    TrackingTimer: TTimer;
    tlbTopBar: TPanel;
    ToolButton1: TSpeedButton;
    ToolButton2: TSpeedButton;
    ToolButton3: TSpeedButton;
    ToolButton4: TSpeedButton;
    ToolButton5: TSpeedButton;
    ToolButton6: TSpeedButton;
    ToolButton7: TSpeedButton;
    procedure actAboutExecute(Sender: TObject);
    procedure actLeftExecute(Sender: TObject);
    procedure actNextExecute(Sender: TObject);
    procedure actPrevExecute(Sender: TObject);
    procedure actRightExecute(Sender: TObject);
    procedure actShowInfoExecute(Sender: TObject);
    procedure actZoomInExecute(Sender: TObject);
    procedure actZoomOutExecute(Sender: TObject);
    procedure actZoomResetExecute(Sender: TObject);
    procedure FileOpen1Accept(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure imgViewClick(Sender: TObject);
    procedure imgViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure imgViewMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure imgViewMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lvThumbnailDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure lvThumbnailSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure sbScrollAreaMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure sbScrollAreaMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure sbScrollAreaMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TrackingTimerTimer(Sender: TObject);
  private
    FDragging: Boolean;
    FPrevImagePos: TPoint;
    FPrevTick: Cardinal;
    FSpeedX: Single;
    FSpeedY: Single;
    FStartPos: TPoint;
    function GetImagePos: TPoint;
    procedure SetImagePos(Value: TPoint);
  private
    MaskList: TStringList;
    Manager: TThumbnailManager;
    Current: RImage;
    procedure GetImageInfo(Wand: PMagickWand; var Results: TStringList);
    procedure LoadImage(const Image: TFileName);
    procedure RenderImage(Const DestSize:TSize;  Dest: TPicture);
    procedure SetSize(const X, Y: integer);
    Procedure UpdateLoadProgress(Sender: TThumbnailManager; const Total, Progress: integer);
  public
    Function LoadPath(Path:TFileName): integer;
  public
      property ImagePos: TPoint read GetImagePos write SetImagePos;
  end;

var
  frmMain: TfrmMain;

implementation
uses math;

{$R *.lfm}

{ TfrmMain }

Const
  THUMBNAIL_SIZE = 64;
  ZoomRatio = 1.5;

{ RImage }

procedure RImage.Clear;
begin
  Wand:=nil;
  Name:='';
  Rotation:=0;
  ZoomRatio:=1.0;
  ViewMode:= vmAdapt;
  Offset:= Point(0,0);
  RealSize := TSize.Create(0,0);
  VirtualSize := TSize.Create(0,0);

end;

procedure TfrmMain.FormCreate(Sender: TObject);

begin
  ImageMagick.Initialize();
  Current.Clear;

  Manager := TThumbnailManager.Create;
  Manager.OnLoadThumbnail:=@UpdateLoadProgress;

  Manager.MaxSize := TSize.Create(THUMBNAIL_SIZE,THUMBNAIL_SIZE);
  lvThumbnail.Width:= THUMBNAIL_SIZE + 2 + GetSystemMetrics(2{SM_CXVSCROLL}) ;

  if ParamCount > 0 then
    begin
      LoadPath(ExtractFilePath(ParamStr(1)));
    end;
end;

procedure TfrmMain.actNextExecute(Sender: TObject);
begin
  if lvThumbnail.Row = lvThumbnail.RowCount - 1 then
    lvThumbnail.row := 0
  else
    lvThumbnail.Row:= lvThumbnail.Row + 1;
end;

procedure TfrmMain.actLeftExecute(Sender: TObject);
begin
  Current.Rotation:= Current.Rotation -90;
  if Current.Rotation = -360 then
    Current.Rotation:= 0;
   RenderImage(TSize.Create(imgView.Width, imgView.Height), imgView.Picture);
end;

procedure TfrmMain.actAboutExecute(Sender: TObject);
var
  theForm : TfAbout;
begin
  theForm:= TfAbout.create(application);
  theForm.ShowModal;
end;

procedure TfrmMain.actPrevExecute(Sender: TObject);
begin
  if lvThumbnail.Row = 0 then
    lvThumbnail.row := lvThumbnail.RowCount - 1
  else
    lvThumbnail.Row:= lvThumbnail.Row - 1;
end;

procedure TfrmMain.actRightExecute(Sender: TObject);
begin
  Current.Rotation:= Current.Rotation +90;
  if Current.Rotation = 360 then
    Current.Rotation:= 0;
  RenderImage(TSize.Create(imgView.Width, imgView.Height), imgView.Picture);
end;

procedure TfrmMain.actShowInfoExecute(Sender: TObject);
var
  Properties: TStringList;
  TheForm: TfrmInfo;
begin
  Properties := TStringList.Create;
  try
    GetImageInfo(Current.Wand, Properties);

    TheForm := TfrmInfo.Create(Self);
    TheForm.ValueListEditor1.Strings.Assign(Properties);
    TheForm.Show;
  finally
    Properties.Free;
  end;
end;

procedure TfrmMain.SetSize(Const X,Y:integer);
begin
  Current.VirtualSize := TSize.Create(X,Y);
  RenderImage(Current.VirtualSize, imgView.Picture);
end;

procedure TfrmMain.actZoomInExecute(Sender: TObject);
begin
  Current.ViewMode:=vmZoom;
  SetSize(round(Current.VirtualSize.Width * zoomRatio),round(Current.VirtualSize.Height * zoomRatio));
end;

procedure TfrmMain.actZoomOutExecute(Sender: TObject);
begin
  Current.ViewMode:=vmZoom;
  SetSize(round(Current.VirtualSize.Width * (1/zoomRatio)), round(Current.VirtualSize.Height * (1/zoomRatio)));
end;

procedure TfrmMain.actZoomResetExecute(Sender: TObject);
begin
  Current.ViewMode:=vmAdapt;
  SetSize(Current.RealSize.Width, Current.RealSize.Width);
end;

procedure TfrmMain.FileOpen1Accept(Sender: TObject);
begin
  LoadPath(FileOpen1.Dialog.FileName);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  if Assigned(Current.Wand) then
    Current.Wand := DestroyMagickWand(Current.Wand);

  MaskList.Free;
  Manager.Free;
  ImageMagick.Finalize();
end;

procedure TfrmMain.FormResize(Sender: TObject);
begin
 //
 pnlCenter.Left:= (tlbTopBar.Width - pnlCenter.Width) div 2;
 if Manager.Count = 0 then exit;
 RenderImage(TSize.Create(imgView.Width, imgView.Height), imgView.Picture);
end;

procedure TfrmMain.imgViewClick(Sender: TObject);
begin

end;

procedure TfrmMain.imgViewMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin

end;

procedure TfrmMain.imgViewMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
end;

procedure TfrmMain.imgViewMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin

end;

procedure TfrmMain.lvThumbnailDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
begin
  lvThumbnail.Canvas.FillRect(aRect);

  if Manager.Count = 0 then exit;

  lvThumbnail.Canvas.Draw(aRect.Left+2, aRect.Top+2, Manager.Items[aRow].Image);
end;

procedure TfrmMain.lvThumbnailSelectCell(Sender: TObject; aCol, aRow: Integer;
  var CanSelect: Boolean);
begin
  if Manager.Count = 0 then exit;

  LoadImage(Manager[aRow].FullName);
end;

procedure TfrmMain.sbScrollAreaMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin

  //FDragging := True;
  //sbScrollArea.DoubleBuffered:=true;
  //FPrevTick := GetTickCount;
  //FPrevImagePos := ImagePos;
  //TrackingTimer.Enabled := True;
  //FStartPos := Point(X - ImgView.Left, Y - ImgView.Top);
  //Screen.Cursor := crHandPoint;
end;

procedure TfrmMain.sbScrollAreaMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  //if FDragging then
  //  ImagePos := Point(X - FStartPos.X, Y - FStartPos.Y);

end;

function TfrmMain.GetImagePos: TPoint;
begin
  //with sbScrollArea do
  //    Result := Point(HorzScrollBar.Position, VertScrollBar.Position);
end;

procedure TfrmMain.SetImagePos(Value: TPoint);
begin
  //sbScrollArea.HorzScrollBar.Position := Value.X;
  //sbScrollArea.VertScrollBar.Position := Value.Y;
end;

procedure TfrmMain.TrackingTimerTimer(Sender: TObject);
var
  Delay: Cardinal;
begin
  //Delay := GetTickCount - FPrevTick;
  //if FDragging then
  //begin
  //  if Delay = 0 then
  //    Delay := 1;
  //  FSpeedX := (ImagePos.X - FPrevImagePos.X) / Delay;
  //  FSpeedY := (ImagePos.Y - FPrevImagePos.Y) / Delay;
  //end
  //else
  //begin
  //  if (Abs(FSpeedX) < 0.005) and (Abs(FSpeedY) < 0.005) then
  //    TrackingTimer.Enabled := False
  //  else
  //  begin
  //    ImagePos := Point(FPrevImagePos.X + Round(Delay * FSpeedX),
  //      FPrevImagePos.Y + Round(Delay * FSpeedY));
  //    FSpeedX := 0.83 * FSpeedX;
  //    FSpeedY := 0.83 * FSpeedY;
  //  end;
  //end;
  //FPrevImagePos := ImagePos;
  //FPrevTick := GetTickCount;
end;


procedure TfrmMain.sbScrollAreaMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  //FDragging := False;
  //Screen.Cursor := crDefault;
end;

procedure TfrmMain.RenderImage(const DestSize: TSize; Dest: TPicture);
var
  bm:TBitmap;
  H,W: integer;
  NewSize: TSize;
  CloneWand: PMagickWand;
  Back: PPixelWand;
begin
  CloneWand:=CloneMagickWand(Current.Wand);

  if Current.Rotation <> 0 then
    begin
      Back:= NewPixelWand();
      MagickGetImageBackgroundColor(CloneWand, Back);
      MagickRotateImage(CloneWand, back, Current.Rotation * 1.0);
      DestroyPixelWand(back);
    end;

  H := MagickGetImageHeight(CloneWand);
  W := MagickGetImageWidth(CloneWand);

  case Current.ViewMode of
    vmAdapt:
      begin
      if (H > DestSize.Height) or (W > DestSize.Width) then
        begin
          NewSize:=Manager.GetPreviewScaleSize(W, H, DestSize);
          MagickResizeImage(CloneWand, NewSize.Width, NewSize.Height, LanczosFilter, 1.0);
        end
      else
        NewSize:= TSize.Create(W, H);
      end;
    vmZoom:
      begin
         MagickResizeImage(CloneWand, DestSize.Width, DestSize.Height, MitchellFilter, 1.0);
         NewSize:= TSize.Create(DestSize.Width, DestSize.Height);
      end;
    else
      NewSize:= TSize.Create(W, H);
  end;

   bm:= TBitmap.Create;
  try
    LoadMagickBitmapWand4(CloneWand, bm, NewSize, Current.Offset);
    Dest.Assign(bm);
  finally
    bm.free;
  end;

  if assigned(CloneWand) then
    DestroyMagickWand(CloneWand);

  sbBottom.SimpleText:=format('%s   %dx%d   %.2f%%',[ExtractFileName(Current.Name), W, H, (Newsize.cx / W) * 100]);
end;

procedure TfrmMain.UpdateLoadProgress(Sender: TThumbnailManager; const Total,
  Progress: integer);
begin
  sbBottom.SimpleText:= format('Loading thumbnails: %d of %d',[Progress,total]);
  sbBottom.invalidate;
  Application.ProcessMessages;
end;

procedure TfrmMain.LoadImage(const Image:TFileName);
var
  status: MagickBooleanType;
begin

  if Assigned(Current.Wand) then
    begin
      Current.Wand := DestroyMagickWand(Current.Wand);
    end;

  Current.Clear;
  Current.Wand := NewMagickWand();

  status := MagickReadImage(Current.Wand, PChar(Image));
  Current.Name:= Image;
  Current.RealSize:= Tsize.create( MagickGetImageWidth(Current.Wand), MagickGetImageHeight(Current.Wand));
  Current.VirtualSize := Current.RealSize;
  Current.ViewMode:= vmAdapt;
  RenderImage(TSize.Create(imgView.Width, imgView.Height), imgView.Picture);

end;

procedure TfrmMain.GetImageInfo(Wand: PMagickWand; var Results: TStringList);
var
  i, propNo: cardinal;
  propList: PPChar;
  propertyValue:Pchar;

begin
  Results.Clear;
  propList:= MagickGetImageProperties(Wand, '*', @Propno);
  for i := 0 to pred(propNo) do
    begin
      propertyValue:=MagickGetImageProperty(Wand,propList[i]);
      Results.Values[String(propList[i])]:= String(propertyValue);
    end;
end;

function TfrmMain.LoadPath(Path: TFileName): integer;
var
  i: integer;
begin
  lvThumbnail.Clear;

  Manager.LoadPath(ExtractFilePath(Path));
  lvThumbnail.RowCount:= Manager.Count;

  for I := 0 to Manager.Count -1 do
    lvThumbnail.RowHeights[i]:= Manager[I].Size.Height + 6;
  result := Manager.Count;
end;
{


 StatusBar1.SimpleText:= format('%d x %d (%d%%)',[ImageCover.Picture.Width, ImageCover.Picture.Height,trunc((imageWidth/ImageCover.Picture.Width ) * 100)]);
}

end.

