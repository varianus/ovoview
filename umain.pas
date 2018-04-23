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
    OriginalWand: PMagickWand;
    ModWand: PMagickWand;
    Name: string;
    Rotation: integer; // degree
    ZoomRatio: double;
    ViewMode: TViewMode;
    Offset:TPoint;
    VirtualSize:TSize;
    RealSize:TSize;
    IntBitmap: TBitmap;
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
    imgView: TPaintBox;
    lvThumbnail: TDrawGrid;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    Panel1: TPanel;
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
    procedure imgViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure imgViewMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure imgViewMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure imgViewPaint(Sender: TObject);
    procedure lvThumbnailDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure lvThumbnailSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure TrackingTimerTimer(Sender: TObject);
  private
    FDragging: Boolean;
    FPrevImagePos: TPoint;
    FPrevTick: Cardinal;
    FSpeedX: Single;
    FSpeedY: Single;
    FStartPos: TPoint;
    function GetImagePos: TPoint;
    procedure PaintImage(BitmapSize: TSize);
    procedure SetImagePos(Value: TPoint);
  private
    MaskList: TStringList;
    Manager: TThumbnailManager;
    Current: RImage;
    procedure GetImageInfo(Wand: PMagickWand; var Results: TStringList);
    procedure LoadImage(const Image: TFileName);
    procedure RenderImage(Const VirtualSize:TSize);
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
  OriginalWand:=nil;
  ModWand:= nil;
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
   RenderImage(Current.VirtualSize);
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
  RenderImage(Current.VirtualSize);
end;

procedure TfrmMain.actShowInfoExecute(Sender: TObject);
var
  Properties: TStringList;
  TheForm: TfrmInfo;
begin
  Properties := TStringList.Create;
  try
    GetImageInfo(Current.OriginalWand, Properties);

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
  RenderImage(Current.VirtualSize);
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
  if Assigned(Current.OriginalWand) then
    Current.OriginalWand := DestroyMagickWand(Current.OriginalWand);

  MaskList.Free;
  Manager.Free;
  ImageMagick.Finalize();
end;

procedure TfrmMain.FormResize(Sender: TObject);
begin
 //
 pnlCenter.Left:= (tlbTopBar.Width - pnlCenter.Width) div 2;
 if Manager.Count = 0 then exit;
 RenderImage(Current.VirtualSize);
end;

procedure TfrmMain.imgViewMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin

  Debug('MouseDown');
  if (Current.VirtualSize.Width <= imgView.Width) or
     (Current.VirtualSize.Height <= imgView.Height) then
    exit;

  FDragging := True;
  FPrevTick := GetTickCount;
  FPrevImagePos := ImagePos;
  TrackingTimer.Enabled := True;
  FStartPos := Point(X - ImgView.Left + Current.offset.X, Y - ImgView.Top + Current.offset.Y);
  Screen.Cursor := crHandPoint;
end;

procedure TfrmMain.imgViewMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
 if FDragging then
  ImagePos := Point(FStartPos.X - X , FStartPos.Y - Y );

end;

procedure TfrmMain.imgViewMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FDragging := False;
  Screen.Cursor := crDefault;
end;

procedure TfrmMain.imgViewPaint(Sender: TObject);
begin
 imgView.Canvas.Lock;
 imgView.Canvas.Brush.Style := bsClear;
 imgView.Canvas.FillRect(imgView.ClientRect);

 if Current.IntBitmap <> nil then
  begin
   if (imgView.Width > Current.IntBitmap.Width) or
      (imgView.Height > Current.IntBitmap.Height)  then
     imgView.Canvas.Draw((imgView.Width - Current.IntBitmap.Width) div 2,
                     (imgView.Height - Current.IntBitmap.Height) div 2,
                     Current.IntBitmap)
   else
     imgView.Canvas.Draw(0,0,Current.IntBitmap)

  end;
 imgView.Canvas.Unlock;
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

function TfrmMain.GetImagePos: TPoint;
begin
  Result := Current.Offset;
end;

procedure TfrmMain.SetImagePos(Value: TPoint);
begin
  Current.Offset := Value;
  writeln (format('In  Offset  X: %4.4d Y: %4.4d',[Current.Offset.x, Current.Offset.y]));
  if Current.Offset.x < 0 then
     Current.Offset.x := 0;

  if Current.Offset.y < 0 then
     Current.Offset.y := 0;

  if (Current.Offset.x + imgView.Width) > Current.VirtualSize.Width then
    Current.Offset.x := Current.VirtualSize.Width - imgView.Width;

  if (Current.Offset.y + imgView.Height) > Current.VirtualSize.Height then
    Current.Offset.y := Current.VirtualSize.Height - imgView.Height;

  writeln (format('Out Offset  X: %4.4d Y: %4.4d',[Current.Offset.x, Current.Offset.y]));
  writeln (format('   Virtual  X: %4.4d Y: %4.4d',[Current.VirtualSize.Width, Current.VirtualSize.Height]));
  writeln (format('     Image  X: %4.4d Y: %4.4d',[imgView.Width, imgView.Height]));
  PaintImage(TSize.Create(imgView.Width, imgView.Height));

end;

procedure TfrmMain.TrackingTimerTimer(Sender: TObject);
var
  Delay: Cardinal;
begin
  exit;
  Delay := GetTickCount - FPrevTick;
  if FDragging then
  begin
    if Delay = 0 then
      Delay := 1;
    FSpeedX := (ImagePos.X - FPrevImagePos.X) / Delay;
    FSpeedY := (ImagePos.Y - FPrevImagePos.Y) / Delay;
  end
  else
  begin
    if (Abs(FSpeedX) < 0.005) and (Abs(FSpeedY) < 0.005) then
   //   TrackingTimer.Enabled := False
    else
    begin
      ImagePos := Point(FPrevImagePos.X + Round(Delay * FSpeedX),
        FPrevImagePos.Y + Round(Delay * FSpeedY));
      FSpeedX := 0.83 * FSpeedX;
      FSpeedY := 0.83 * FSpeedY;
    end;
  end;
  FPrevImagePos := ImagePos;
  FPrevTick := GetTickCount;
end;

Procedure TfrmMain.PaintImage(BitmapSize: TSize);
begin
  Current.IntBitmap.SetSize(BitmapSize.Width,BitmapSize.Height);
  LoadMagickBitmapWand4(Current.ModWand, Current.IntBitmap, BitmapSize, Current.Offset);
  imgViewPaint(self);
end;

procedure TfrmMain.RenderImage(const VirtualSize: TSize);
var
  H,W: integer;
  NewSize: TSize;
  Back: PPixelWand;
begin
  If Assigned(Current.ModWand) then
    DestroyMagickWand(Current.ModWand);

  Current.ModWand:=CloneMagickWand(Current.OriginalWand);

  if Current.Rotation <> 0 then
    begin
      Back:= NewPixelWand();
      MagickGetImageBackgroundColor(Current.ModWand, Back);
      MagickRotateImage(Current.ModWand, back, Current.Rotation * 1.0);
      DestroyPixelWand(back);
    end;

  H := MagickGetImageHeight(Current.ModWand);
  W := MagickGetImageWidth(Current.ModWand);

  case Current.ViewMode of
    vmAdapt:
      begin
      if (H > VirtualSize.Height) or (W > VirtualSize.Width) then
        begin
          NewSize:=Manager.GetPreviewScaleSize(W, H, VirtualSize);
          MagickResizeImage(Current.ModWand, NewSize.Width, NewSize.Height, LanczosFilter, 1.0);
        end
      else
        NewSize:= TSize.Create(W, H);
      end;
    vmZoom:
      begin
         MagickResizeImage(Current.ModWand, VirtualSize.Width, VirtualSize.Height, MitchellFilter, 1.0);
         if (VirtualSize.Height > imgView.Height) or (VirtualSize.Width > imgView.Width) then
           begin
              NewSize:= TSize.Create(imgView.Width, imgView.Height);
           end
         else
           NewSize:= TSize.Create(VirtualSize.Width, VirtualSize.Height)
      end;
    else
      NewSize:= TSize.Create(W, H);
  end;
  //writeln('Virtual ', VirtualSize.Width, ' ', VirtualSize.Height);
  //writeln('imgview ', imgview.Width, ' ', imgview.Height);
  //writeln('NewSize ', NewSize.Width, ' ', NewSize.Height);

  PaintImage(NewSize);
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

  if Assigned(Current.OriginalWand) then
    begin
      Current.OriginalWand := DestroyMagickWand(Current.OriginalWand);
      Current.IntBitmap.Free;
    end;


  Current.Clear;
  Current.OriginalWand := NewMagickWand();
  Current.IntBitmap := TBitmap.Create;

  status := MagickReadImage(Current.OriginalWand, PChar(Image));
  Current.Name:= Image;
  Current.RealSize:= Tsize.create( MagickGetImageWidth(Current.OriginalWand), MagickGetImageHeight(Current.OriginalWand));
  Current.VirtualSize := Current.RealSize;
  Current.ViewMode:= vmAdapt;
  RenderImage(TSize.Create(imgView.Width, imgView.Height));

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

