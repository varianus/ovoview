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
  Classes, SysUtils, types, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  FilesSupport, Thumbnails, MagickWand, ImageMagick, StdCtrls, Grids;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    lvThumbnail: TDrawGrid;
    imgView: TImage;
    sbBottom: TStatusBar;
    tlbTopBar: TToolBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lvThumbnailDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure lvThumbnailSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
  private
    MaskList: TStringList;
    Manager: TThumbnailManager;

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


  Manager := TThumbnailManager.Create;

  Manager.MaxSize := TSize.Create(128,128);
  if ParamCount > 0 then
    begin
      LoadPath(ExtractFilePath(ParamStr(1)));
    end;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  MaskList.Free;
  Manager.Free;
  ImageMagick.Finalize();
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
  wand: PMagickWand;
  status: MagickBooleanType;
  BlobStream: TBlobStream;
  Memory:pointer;
  MemorySize: integer;
begin
  wand := NewMagickWand();
  try
    status := MagickReadImage(wand, PChar(Manager[aRow].FullName));
    MagickSetImageFormat(wand,'BMP');
    Memory:= MagickGetImageBlob(Wand, @MemorySize);
    if Assigned(Memory) then
      try
        BlobStream:= TBlobStream.Create(Memory, MemorySize);
        imgView.Picture.LoadFromStream(BlobStream);
        sbBottom.SimpleText:= Manager[aRow].FullName;
        BlobStream.Free;
      finally
        MagickRelinquishMemory(Memory);
      end;

  finally
    wand := DestroyMagickWand(wand);
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

end;

end.

