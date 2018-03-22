{
  Copyright 1999-2005 ImageMagick Studio LLC, a non-profit organization
  dedicated to making software imaging solutions freely available.
  
  You may not use this file except in compliance with the License.
  obtain a copy of the License at
  
    http://www.imagemagick.org/script/license.php
  
  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.

  ImageMagick Application Programming Interface declarations.
}
{
  Converted from c by: Felipe Monteiro de Carvalho Dez/2005

	Bug-fixed by Ángel Eduardo García Hernández
	Thanks to Marc Geldon and RuBBeR
}
{Version 0.4}
unit ImageMagick;

{$ifdef FPC}
  {$mode objfpc}
	{$PACKRECORDS C}
{$endif}

interface

uses SysUtils, ctypes, dynlibs;

{$z4}

//type Pcsize_t = ^size_t;

const LIB_CORE_MAGICK =
  {$if defined(Unix)}
    'libMagickCore.so'
  {$else if defined(Windows)}
    'CORE_RL_magickcore_.dll'
  {$endif}
;
const LIB_MAGICK_WAND =
  {$if defined(Unix)}
    'libMagickWand.so'
  {$else if defined(Windows)}
    'CORE_RL_Magickwand_.dll'
  {$endif}
;

{$include MagickType.inc}
{$include Type.inc}
{$include Cache.inc} // <- Exports
{$include CacheView.inc}
{$include Compare.inc}
{$include Constitute.inc}
{$include Draw.inc}
{$include Effect.inc}
{$include Fx.inc}
{$include Pixel.inc}
{$include Quantize.inc}
{$include Statistic.inc}

var LibMagick:TLibHandle=0;
var LibWand:TLibHandle=0;

function Initialize():boolean;
function Finalize():boolean;

implementation

uses
  MagickWand;

function Initialize():boolean;
begin
  Result:=false;
  if (LibMagick=0) then begin
    LibMagick:=dynLibs.LoadLibrary(LIB_CORE_MAGICK);
    if (LibMagick>0) then begin
      Pointer(GetAuthenticPixels):=GetProcAddress(LibMagick, 'GetAuthenticPixels');
    end;
  end;
  if (LibWand=0) then begin
    LibWand:=dynLibs.LoadLibrary(LIB_MAGICK_WAND);
    if (LibWand>0) then begin
      MagickWand.Initialize(LibWand);
      MagickWand.MagickWandGenesis();
    end;
  end;
  Result:=(LibMagick<>0) and (LibWand<>0);
end;

function Finalize():boolean;
begin
  Result:=false;
  if (LibMagick<>0) then begin
    Result:=dynLibs.UnloadLibrary(LibMagick);
    Pointer(GetAuthenticPixels):=nil;
    LibMagick:=0;
  end;
  if (LibWand<>0) then begin
    MagickWand.MagickWandTerminus();
    MagickWand.Finalize(LibWand);
    Result:=dynLibs.UnloadLibrary(LibWand);
    LibWand:=0;
  end;
  Result:=(LibMagick=0) and (LibWand=0);
end;



end.
