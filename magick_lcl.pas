unit Magick_LCL;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics,  MagickWand, types;

type
  { TBlobStream - helper class to handle arbitrary memory region as a stream }
  TBlobStream = class(TCustomMemoryStream)
  public
    constructor Create(Ptr: Pointer; ASize: PtrInt);
  end;


procedure LoadMagickBitmapWand1(Wand: PMagickWand; Bmp: TBitmap);
procedure LoadMagickBitmapWand2(Wand: PMagickWand; Bmp: TBitmap);
procedure LoadMagickBitmapWand3(Wand: PMagickWand; Bmp: TBitmap);
procedure LoadMagickBitmapWand4(Wand: PMagickWand; Bmp: TBitmap; Size:TSize; Offset:TPoint);


implementation

uses ImageMagick, FPimage, GraphType, IntfGraphics;

procedure RenderGrid(Target: TBitmap; Height, Width: Integer; Size: Integer;
  Color1, Color2: TColor);
var
  Tmp: TBitmap;
begin
  Tmp := TBitmap.Create;
  try
    Tmp.Canvas.Brush.Color := Color1;
    Tmp.Width := 2 * Size;
    Tmp.Height := 2 * Size;
    Tmp.Canvas.Brush.Color := Color2;
    Tmp.Canvas.FillRect(Rect(0, 0, Size, Size));
    Tmp.Canvas.FillRect(Bounds(Size, Size, Size, Size));
    Target.Canvas.Brush.Bitmap := Tmp;
    if Target.Width * Target.Height = 0 then
      Target.SetSize(Width, Height)
    else
    begin
      Target.SetSize(Width, Height);
      Target.Canvas.FillRect(Rect(0, 0, Width, Height));
    end;
  finally
    Tmp.Free;
  end;
end;

procedure LoadMagickBitmapWand1(Wand: PMagickWand; Bmp: TBitmap);
var
  img: Pimage;
  pack: PPixelPacket;
  limg: TLazIntfImage;
  i, j, wi, he: integer;
  colo: TFPColor;
begin
  img := GetImageFromMagickWand(wand);
  he := MagickGetImageHeight(wand);
  wi := MagickGetImageWidth(wand);
  limg := TLazIntfImage.Create(0, 0);
  try
    limg.DataDescription := GetDescriptionFromDevice(0, wi, he);
    pack := GetAuthenticPixels(img, 0, 0, wi, he, nil);
    for j := 0 to he - 1 do
      for i := 0 to wi - 1 do
      begin
        colo.red := pack^.red;
        colo.green := pack^.green;
        colo.blue := pack^.blue;
        colo.alpha := pack^.opacity;
        limg.Colors[i, j] := colo;
        Inc(pack);
      end;
    Bmp.LoadFromIntfImage(limg);
  finally
    limg.Free;
  end;
end;


procedure LoadMagickBitmapWand2(Wand: PMagickWand; Bmp: TBitmap);
var
  BlobStream: TBlobStream;
  Memory: pointer;
  MemorySize: integer;

begin
  MagickSetImageFormat(wand, 'BMP');
  Memory := MagickGetImageBlob(Wand, @MemorySize);
  if Assigned(Memory) then
    try
      BlobStream := TBlobStream.Create(Memory, MemorySize);
      Bmp.LoadFromStream(BlobStream);
      BlobStream.Free;
    finally
      MagickRelinquishMemory(Memory);
    end;
end;

procedure LoadMagickBitmapWand3(Wand: PMagickWand; Bmp: TBitmap);
var
  r, AWidth: integer;
  AImage: PImage;
  ExceptionInfo: PExceptionInfo;
begin
  // We copy ourself to the bitmap in Dest
  AImage := GetImageFromMagickWand(Wand);
  if not assigned(AImage) then
  begin
    Bmp.Width := 0;
    Bmp.Height := 0;
    exit;
  end;

  // Copy our properties to Dest
  Bmp.PixelFormat := pf24bit; // always use 24-bit
  Bmp.Width := MagickGetImageWidth(wand);
  Bmp.Height := MagickGetImageHeight(wand);

  // Copy image data
  AWidth := Bmp.Width;
  //   GetExceptionInfo(@ExceptionInfo);
  Bmp.BeginUpdate();
  try
    for r := 0 to Bmp.Height - 1 do
    begin
      DispatchImage(AImage, 1, r, AWidth, 0, 'BGR', CharPixel, Bmp.Scanline[r], @ExceptionInfo);
      //        ProcessExceptions(ExceptionInfo);
    end;
    Bmp.EndUpdate();
  finally
    //      DestroyExceptionInfo(@ExceptionInfo);
  end;
  exit;
end;

procedure LoadMagickBitmapWand4(Wand: PMagickWand; Bmp: TBitmap; Size:TSize; Offset:TPoint);
var
  r: integer;
  AImage: PImage;
  limg: TLazIntfImage;
  ExceptionInfo: PExceptionInfo;
  DataDescription: TRawImageDescription;
  //
begin
  // We copy ourself to the bitmap in Dest
  AImage := GetImageFromMagickWand(Wand);
  limg:= TLazIntfImage.Create(0,0);

  if not assigned(AImage) then
  begin
    Bmp.Width := 0;
    Bmp.Height := 0;
    exit;
  end;
  DataDescription.Init_BPP32_B8G8R8A8_BIO_TTB( Size.Width, Size.Height);
  limg.DataDescription := DataDescription;

 //    GetExceptionInfo(@ExceptionInfo);
  try
    for r := 0 to Size.Height - 1 do
    begin
      ExportImagePixels(AImage, 0+Offset.x, r+Offset.y, Size.Width, 1, 'BGRA', CharPixel, limg.GetDataLineStart(r), @ExceptionInfo);
      //        ProcessExceptions(ExceptionInfo);
    end;
  finally
    //      DestroyExceptionInfo(@ExceptionInfo);
  end;
  Bmp.LoadFromIntfImage(limg);
  limg.free;

end;

{ TBlobStream }



{ TBlobStream }
constructor TBlobStream.Create(Ptr: Pointer; ASize: PtrInt);
begin
  inherited Create;
  SetPointer(Ptr, ASize);
end;


end.
