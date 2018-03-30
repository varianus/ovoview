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

  Get Magick Wand Properties.
}
{WandExports}
{$IFDEF INTERFACE}
Type
  TMagickGetImageProperty = function(wand:PMagickWand; PropertyName:Pchar):Pchar;cdecl;
  TMagickGetImageProperties = function(wand:PMagickWand; PropertyFilter:Pchar; ProprtiesNo:Pculong): PPchar;cdecl;

var

  MagickGetImageProperty:TMagickGetImageProperty=nil;
  MagickGetImageProperties:TMagickGetImageProperties=nil;

{$ENDIF INTERFACE}

{$IFDEF IMPLEMENTATION}
  Pointer(MagickGetImageProperty):=GetProcAddress(LibWand, 'MagickGetImageProperty');
  Pointer(MagickGetImageProperties):=GetProcAddress(LibWand, 'MagickGetImageProperties');
{$ENDIF IMPLEMENTATION}
