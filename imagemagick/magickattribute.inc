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

  Set or Get Magick Wand Attributes.
}
{WandExports}

Type
  TMagickGetException = function(wand: PMagickWand; severity: PExceptionType): PChar; cdecl;
  TMagickGetFilename= function (const wand: PMagickWand): PChar; cdecl;
  TMagickGetFormat= function (wand: PMagickWand): PChar; cdecl;
  TMagickGetHomeURL=function : PChar; cdecl;
  TMagickGetOption=function (wand: PMagickWand; const key: PChar): PChar; cdecl;
  TMagickGetCompression=function (wand: PMagickWand): CompressionType; cdecl;

  TMagickGetCopyright=function : PChar; cdecl;
  TMagickGetPackageName=function : PChar; cdecl;
  TMagickGetQuantumDepth=function (depth: Pculong): PChar; cdecl;
  TMagickGetQuantumRange=function (range: Pculong): PChar; cdecl;
  TMagickGetReleaseDate=function : PChar; cdecl;
  TMagickGetVersion=function (version: Pculong): PChar; cdecl;

  TMagickGetSamplingFactors=function (wand: PMagickWand; number_factors: Pculong): Pdouble; cdecl;
  TMagickGetInterlaceScheme=function (wand: PMagickWand): InterlaceType; cdecl;


var

  MagickGetException:TMagickGetException=nil;
  MagickGetFilename:TMagickGetFilename=nil;
  MagickGetFormat:TMagickGetFormat=nil;
  MagickGetHomeURL:TMagickGetHomeURL=nil;
  MagickGetOption:TMagickGetOption=nil;
  MagickGetCompression:TMagickGetCompression=nil;

  MagickGetCopyright: TMagickGetCopyright=nil;
  MagickGetPackageName:TMagickGetPackageName=nil;

  MagickGetQuantumDepth:TMagickGetQuantumDepth=nil;
  MagickGetQuantumRange:TMagickGetQuantumRange=nil;
  MagickGetReleaseDate:TMagickGetReleaseDate=nil;
  MagickGetVersion:TMagickGetVersion=nil;

  MagickGetSamplingFactors:TMagickGetSamplingFactors=nil;
  MagickGetInterlaceScheme:TMagickGetInterlaceScheme=nil;

