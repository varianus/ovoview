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

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, FilesSupport, Thumbnails, Magick_LCL, uInfo, MagickWand,
  ImageMagick, StdCtrls, Grids, ActnList, DBActns, Buttons;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    Action1: TAction;
    actShowInfo: TAction;
    actSlideShow: TAction;
    actNext: TAction;
    actPrev: TAction;
    ActionList: TActionList;
    ImageList1: TImageList;
    lvThumbnail: TDrawGrid;
    imgView: TImage;
    pnlCenter: TPanel;
    sbBottom: TStatusBar;
    tlbTopBar: TPanel;
    ToolButton1: TSpeedButton;
    ToolButton2: TSpeedButton;
    ToolButton4: TSpeedButton;
    procedure actNextExecute(Sender: TObject);
    procedure actPrevExecute(Sender: TObject);
    procedure actShowInfoExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure lvThumbnailDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure lvThumbnailSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
  private
    MaskList: TStringList;
    Manager: TThumbnailManager;
    Currentwand: PMagickWand;

    procedure GetImageInfo(Wand: PMagickWand; var Results: TStringList);

  public
    Function LoadPath(Path:TFileName): integer;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }


procedure TfrmMain.FormCreate(Sender: TObject);
begin
  ImageMagick.Initialize();
  Currentwand:=nil;

  Manager := TThumbnailManager.Create;

  Manager.MaxSize := TSize.Create(128,128);
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

procedure TfrmMain.actPrevExecute(Sender: TObject);
begin
  if lvThumbnail.Row = 0 then
    lvThumbnail.row := lvThumbnail.RowCount - 1
  else
    lvThumbnail.Row:= lvThumbnail.Row - 1;
end;

procedure TfrmMain.actShowInfoExecute(Sender: TObject);
var
  Properties: TStringList;
  TheForm: TfrmInfo;
begin
  Properties := TStringList.Create;
  try
    GetImageInfo(Currentwand, Properties);

    TheForm := TfrmInfo.Create(Self);
    TheForm.ValueListEditor1.Strings.Assign(Properties);
    TheForm.Show;
  finally
    Properties.Free;
  end;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  if Assigned(Currentwand) then
    Currentwand := DestroyMagickWand(Currentwand);

  MaskList.Free;
  Manager.Free;
  ImageMagick.Finalize();
end;

procedure TfrmMain.FormResize(Sender: TObject);
begin
 //
 pnlCenter.Left:= (tlbTopBar.Width - pnlCenter.Width) div 2;

end;

procedure TfrmMain.lvThumbnailDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
begin
  lvThumbnail.Canvas.FillRect(aRect);
  lvThumbnail.Canvas.Draw(aRect.Left+2, aRect.Top+2, Manager.Items[aRow].Image);
end;

procedure TfrmMain.lvThumbnailSelectCell(Sender: TObject; aCol, aRow: Integer;
  var CanSelect: Boolean);
var
  status: MagickBooleanType;
  bm:TBitmap;
  H,W: integer;
  NewSize, _Constraints: TSize;
begin
  if Manager.Count = 0 then exit;

  if Assigned(Currentwand) then
    begin
      Currentwand := DestroyMagickWand(Currentwand);
      CurrentWand := nil;
    end;

  Currentwand := NewMagickWand();

  status := MagickReadImage(Currentwand, PChar(Manager[aRow].FullName));
  H := MagickGetImageHeight(Currentwand);
  W := MagickGetImageWidth(Currentwand);
  if (H > imgView.Height) or (W > imgView.Width) then
    begin
      _Constraints:= TSize.Create(imgView.Width,imgView.Height);
      NewSize:=Manager.GetPreviewScaleSize(W, H, _Constraints);
      MagickResizeImage(Currentwand, NewSize.cx, NewSize.cy, LanczosFilter, 1.0);
    end
  else
    NewSize:= TSize.Create(W, H);
  bm:= TBitmap.Create;
  LoadMagickBitmapWand4(CurrentWand, bm);
  imgView.picture.Assign(bm);
  bm.free;
  sbBottom.SimpleText:=format('%s   %dx%d   %.2f%%',[ExtractFileName(Manager[aRow].FullName), W, H, (Newsize.cx / W) * 100]);


end;

procedure TfrmMain.GetImageInfo(Wand: PMagickWand; Var Results:TStringList);
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

  Manager.LoadPath(Path);
  lvThumbnail.RowCount:= Manager.Count;

  for I := 0 to Manager.Count -1 do
    lvThumbnail.RowHeights[i]:= Manager[I].Size.cy + 6;
  result := Manager.Count;
end;

end.

