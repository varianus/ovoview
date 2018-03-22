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

unit Thumbnails;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, types, fgl, FilesSupport;

type
  { TBlobStream - helper class to handle arbitrary memory region as a stream }
  TBlobStream = class(TCustomMemoryStream)
  public
    constructor Create(Ptr: Pointer; ASize: PtrInt);
  end;

  { TThumbnail - this class contains a thumbnail and infos about an image}

  TThumbnail = class
  private
    FFullName: TFileName;
    FImage: TBitmap;
    fInfo: TFileInfo;
    function GetSize: TSize;

  public
    property Image: TBitmap read Fimage;
    property Size: TSize read GetSize;
    Property Info: TFileInfo read fInfo;
    property FullName: TFileName read FFullName;
    constructor Create; overload;
    constructor Create(Stream: TBlobStream); overload;
    destructor Destroy; override;

  end;

  { TThumbnailManager - class to handle a list of thumbnail }
  TThumbnailManager = class(specialize TFPGObjectList<TThumbnail>)
  private
    FMaxSize: TSize;
    FMaskList: TstringList;
    procedure SetMaxSize(AValue: TSize);
  public
    Property MaskList:TSTringList read FMaskList;
    property MaxSize: TSize read FMaxSize write SetMaxSize;
    function LoadPath(Path: TFileName): integer;
    function GetPreviewScaleSize(aWidth, aHeight: Integer): TSize;
    Constructor Create;
    destructor Destroy; override;
  end;


implementation
uses
  MagickWand, ImageMagick;


{ TBlobStream }
constructor TBlobStream.Create(Ptr: Pointer; ASize: PtrInt);
begin
  inherited Create;
  SetPointer(Ptr, ASize);
end;

function TThumbnail.GetSize: TSize;
begin
  Result.cx:=FImage.Width;
  Result.cy:=FImage.Height;
end;

{ TThumbnail }
constructor TThumbnail.Create;
begin
 FImage := TBitmap.Create;
end;

constructor TThumbnail.Create(Stream: TBlobStream);
begin
  Create;
  Stream.Position:=0;
  FImage.LoadFromStream(Stream);
end;

destructor TThumbnail.Destroy;
begin
  FImage.Free;
  inherited Destroy;
end;

{ TThumbnailManager }
function TThumbnailManager.GetPreviewScaleSize(aWidth, aHeight: Integer): TSize;
begin
  if aWidth > aHeight then
    begin
      Result.cx:= FMaxSize.cx;
      Result.cy:= Result.cx * aHeight div aWidth;
      if Result.cy > FMaxSize.cy then
      begin
        Result.cy:= FMaxSize.cy;
        Result.cx:= Result.cy * aWidth div aHeight;
      end;
    end
  else
    begin
      Result.cy:= FMaxSize.cy;
      Result.cx:= Result.cy * aWidth div aHeight;
    end;
end;

constructor TThumbnailManager.Create;
begin
  Inherited Create(true);
  FMaskList:= TStringList.Create;

  // Preload estension of all supported image format
  FMaskList.AddStrings([
{$REGION 'Supported Extensions'}
    '*.aai',
    '*.ai',
    '*.art',
    '*.arw',
    '*.bgra',
    '*.bgro',
    '*.bmp',
    '*.canvas',
    '*.cin',
    '*.cmyk',
    '*.cmyka',
    '*.cr2',
    '*.crw',
    '*.cur',
    '*.cut',
    '*.dcm',
    '*.dcr',
    '*.dcx',
    '*.dds',
    '*.dfont',
    '*.dlib',
    '*.dng',
    '*.dpx',
    '*.dxt1',
    '*.dxt5',
    '*.emf',
    '*.epi',
    '*.eps',
    '*.ept',
    '*.ept2',
    '*.ept3',
    '*.exr',
    '*.fax',
    '*.fits',
    '*.flif',
    '*.g3',
    '*.g4',
    '*.gif',
    '*.gif87',
    '*.gray',
    '*.hald',
    '*.hdr',
    '*.hrz',
    '*.icb',
    '*.ico',
    '*.icon',
    '*.ipl',
    '*.j2k',
    '*.jc2',
    '*.jng',
    '*.jnx',
    '*.jpe',
    '*.jpeg',
    '*.jpg',
    '*.jpm',
    '*.jps',
    '*.jpt',
    '*.kdc',
    '*.label',
    '*.map',
    '*.mrw',
    '*.nef',
    '*.nrw',
    '*.orf',
    '*.otb',
    '*.otf',
    '*.pbm',
    '*.pcd',
    '*.pcds',
    '*.pcl',
    '*.pct',
    '*.pcx',
    '*.pfa',
    '*.pfb',
    '*.pfm',
    '*.picon',
    '*.pict',
    '*.pix',
    '*.pjpeg',
    '*.png',
    '*.png00',
    '*.png24',
    '*.png32',
    '*.png48',
    '*.png64',
    '*.png8',
    '*.pnm',
    '*.ppm',
    '*.ps',
    '*.psb',
    '*.psd',
    '*.psp',
    '*.raf',
    '*.ras',
    '*.rgb',
    '*.rgba',
    '*.rgbo',
    '*.rla',
    '*.rle',
    '*.scr',
    '*.screenshot',
    '*.sgi',
    '*.srf',
    '*.sun',
    '*.svg',
    '*.svgz',
    '*.tif',
    '*.tiff',
    '*.tiff64',
    '*.ttf',
    '*.vda',
    '*.vicar',
    '*.vid',
    '*.viff',
    '*.vmf',
    '*.vraw',
    '*.vst',
    '*.wdp',
    '*.webp',
    '*.wpg',
    '*.x3f',
    '*.xbm',
    '*.xcf',
    '*.yuv'
{$ENDREGION}
              ]);

  inherited Create;
end;

destructor TThumbnailManager.Destroy;
begin
  FMaskList.Free;
  inherited Destroy;
end;

procedure TThumbnailManager.SetMaxSize(AValue: TSize);
begin
  if FMaxSize=AValue then Exit;
  FMaxSize:=AValue;
end;

function TThumbnailManager.LoadPath(Path: TFileName): integer;
var
  intList: TstringList;
  i: integer;
  wand: PMagickWand;
  status: MagickBooleanType;
  Item: TThumbnail;
  size: TSize;
  Memory: PByte;
  MemorySize: integer;
  BlobStream: TBlobStream;
  H,W : integer;
begin
  Self.Clear;

  intList := TStringList.Create;
  intlist.OwnsObjects:= true;

  if not BuildFileList(Path, MaskList, faAnyFile, intList, False) then
    begin
      intList.Free;
      exit;
    end;

  wand := NewMagickWand();

  for I := 0 to intList.Count -1 do
    begin
      status := MagickReadImage(wand, pchar(intList[i]));
      H := MagickGetImageHeight(wand);
      W := MagickGetImageWidth(wand);
      if (H > MaxSize.cy) or (W > MaxSize.cx) then
        begin
          Size:=GetPreviewScaleSize(W, H);
          MagickResizeImage(wand, Size.cx, Size.cy, LanczosFilter, 1.0);
        end;
      MagickSetImageFormat(wand,'BMP');
      Memory:= MagickGetImageBlob(Wand, @MemorySize);
      if Assigned(Memory) then
        try
          BlobStream:= TBlobStream.Create(Memory, MemorySize);
          Item:= TThumbnail.Create(BlobStream);
          Item.fInfo := TFileInfoObject(intList.Objects[i]).info;
          Item.FFullName:=intList[i];
          BlobStream.Free;
        finally
          MagickRelinquishMemory(Memory);
        end;

      Add(Item);
    end;

  wand := DestroyMagickWand(wand);

  intList.Free;

end;


end.

