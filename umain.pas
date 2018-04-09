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

  RImage = record
    Wand: PMagickWand;
    Name: string;
    Rotation: integer; // degree
    procedure Clear;
  end;


  TfrmMain = class(TForm)
    actAbout: TAction;
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
    lvThumbnail: TDrawGrid;
    imgView: TImage;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    pnlCenter: TPanel;
    sbBottom: TStatusBar;
    tlbTopBar: TPanel;
    ToolButton1: TSpeedButton;
    ToolButton2: TSpeedButton;
    ToolButton3: TSpeedButton;
    ToolButton4: TSpeedButton;
    ToolButton5: TSpeedButton;
    procedure actAboutExecute(Sender: TObject);
    procedure actLeftExecute(Sender: TObject);
    procedure actNextExecute(Sender: TObject);
    procedure actPrevExecute(Sender: TObject);
    procedure actRightExecute(Sender: TObject);
    procedure actShowInfoExecute(Sender: TObject);
    procedure FileOpen1Accept(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure imgViewClick(Sender: TObject);
    procedure lvThumbnailDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure lvThumbnailSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
  private
    MaskList: TStringList;
    Manager: TThumbnailManager;
    Current: RImage;
    procedure GetImageInfo(Wand: PMagickWand; var Results: TStringList);
    procedure LoadImage(const Image: TFileName);
    procedure RenderImage(Const DestSize:TSize;  Dest: TPicture);
    Procedure UpdateLoadProgress(Sender: TThumbnailManager; const Total, Progress: integer);
  public
    Function LoadPath(Path:TFileName): integer;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

Const
  THUMBNAIL_SIZE = 64;

{ RImage }

procedure RImage.Clear;
begin
  Wand:=nil;
  Name:='';
  Rotation:=0;

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

  if (H > DestSize.Height) or (W > DestSize.Width) then
    begin
      NewSize:=Manager.GetPreviewScaleSize(W, H, DestSize);
      MagickResizeImage(CloneWand, NewSize.Width, NewSize.Height, LanczosFilter, 1.0);
    end
  else
    NewSize:= TSize.Create(W, H);

  bm:= TBitmap.Create;
  try
    LoadMagickBitmapWand4(CloneWand, bm);
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
      Current.Clear;
    end;

  Current.Wand := NewMagickWand();

  status := MagickReadImage(Current.Wand, PChar(Image));
  Current.Name:= Image;
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

end.

