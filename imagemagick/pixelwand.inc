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

  ImageMagick pixel wand API.
}


{
  Declaration from pixel-wand.c
}
{$IFDEF INTERFACE}
type
  TPixelWand = record
    id        : culong;
    name      : array[1..MaxTextExtent] of char;
    exception : ExceptionInfo;
    pixel     : MagickPixelPacket;
    count     : culong;
    debug     : MagickBooleanType;
    signature : culong;
  end;
  PPixelWand  = ^TPixelWand;
  PPPixelWand = ^TPixelWand;
{WandExport}
  TPixelGetException = function (const wand: PPixelWand; severity: PExceptionType): PChar; cdecl;
  TPixelGetColorAsString = function (const wand: PPixelWand): PChar; cdecl;
  TPixelGetAlpha = function (const wand: PPixelWand): Double; cdecl;
  TPixelGetBlack = function (const wand: PPixelWand): Double; cdecl;
  TPixelGetBlue = function (const wand: PPixelWand): Double; cdecl;
  TPixelGetCyan = function (const wand: PPixelWand): Double; cdecl;
  TPixelGetGreen = function (const wand: PPixelWand): Double; cdecl;
  TPixelGetMagenta = function (const wand: PPixelWand): Double; cdecl;
  TPixelGetOpacity = function (const wand: PPixelWand): Double; cdecl;
  TPixelGetRed = function (const wand: PPixelWand): Double; cdecl;
  TPixelGetYellow = function (const wand: PPixelWand): Double; cdecl;
  TPixelGetIndex = function (const wand: PPixelWand): IndexPacket; cdecl;


  TIsPixelWand = function (const wand: PPixelWand): MagickBooleanType; cdecl;
  TIsPixelWandSimilar = function (p: PPixelWand; q: PPixelWand; const fuzz: double): MagickBooleanType; cdecl;
  TPixelClearException = function (wand: PPixelWand): MagickBooleanType; cdecl;
  TPixelSetColor = function(wand: PPixelWand; const color: PChar): MagickBooleanType; cdecl;

  TDestroyPixelWand = function (wand: PPixelWand): PPixelWand; cdecl;
  TDestroyPixelWands = function (wand: PPPixelWand; const number_wands: culong): PPPixelWand; cdecl;
  TNewPixelWand = function : PPixelWand; cdecl;
  TNewPixelWands = function (const number_wands: culong): PPPixelWand; cdecl;

  TPixelGetAlphaQuantum = function (const wand: PPixelWand): Quantum; cdecl;
  TPixelGetBlackQuantum = function (const wand: PPixelWand): Quantum; cdecl;
  TPixelGetBlueQuantum = function (const wand: PPixelWand): Quantum; cdecl;
  TPixelGetCyanQuantum = function (const wand: PPixelWand): Quantum; cdecl;
  TPixelGetGreenQuantum = function (const wand: PPixelWand): Quantum; cdecl;
  TPixelGetMagentaQuantum = function (const wand: PPixelWand): Quantum; cdecl;
  TPixelGetOpacityQuantum = function (const wand: PPixelWand): Quantum; cdecl;
  TPixelGetRedQuantum = function (const wand: PPixelWand): Quantum; cdecl;
  TPixelGetYellowQuantum = function (const wand: PPixelWand): Quantum; cdecl;
  TPixelGetColorCount = function (const wand: PPixelWand): culong; cdecl;

  TClearPixelWand = procedure (wand: PPixelWand); cdecl;
  TPixelGetQuantumColor = procedure (const wand: PPixelWand; color: PPixelPacket); cdecl;
  TPixelSetAlpha = procedure (wand: PPixelWand; const opacity: Double); cdecl;
  TPixelSetAlphaQuantum = procedure (wand: PPixelWand; const opacity: Quantum); cdecl;
  TPixelSetBlack = procedure (wand: PPixelWand; const opacity: Double); cdecl;
  TPixelSetBlackQuantum = procedure (wand: PPixelWand; const opacity: Quantum); cdecl;
  TPixelSetBlue = procedure (wand: PPixelWand; const opacity: Double); cdecl;
  TPixelSetBlueQuantum = procedure (wand: PPixelWand; const opacity: Quantum); cdecl;
  TPixelSetColorCount = procedure (wand: PPixelWand; const count: culong); cdecl;
  TPixelSetCyan =procedure (wand: PPixelWand; const opacity: Double); cdecl;
  TPixelSetCyanQuantum = procedure (wand: PPixelWand; const opacity: Quantum); cdecl;
  TPixelSetGreen = procedure (wand: PPixelWand; const opacity: Double); cdecl;
  TPixelSetGreenQuantum = procedure (wand: PPixelWand; const opacity: Quantum); cdecl;
  TPixelSetIndex = procedure (wand: PPixelWand; const index: IndexPacket); cdecl;
  TPixelSetMagenta =  procedure (wand: PPixelWand; const opacity: Double); cdecl;
  TPixelSetMagentaQuantum  = procedure (wand: PPixelWand; const opacity: Quantum); cdecl;
  TPixelSetMagickColor = procedure (wand: PPixelWand; const color: PMagickPixelPacket); cdecl;  // Added after 6.2
  TPixelSetOpacity=procedure (wand: PPixelWand; const opacity: Double); cdecl;
  TPixelSetOpacityQuantum = procedure (wand: PPixelWand; const opacity: Quantum); cdecl;
  PTixelSetQuantumColor = procedure (wand: PPixelWand; const color: PPixelPacket); cdecl;
  TPixelSetRed = procedure (wand: PPixelWand; const opacity: Double); cdecl;
  TPixelSetRedQuantum = procedure (wand: PPixelWand; const opacity: Quantum); cdecl;
  TPixelSetYellow = procedure (wand: PPixelWand; const opacity: Double); cdecl;
  TPixelSetYellowQuantum = procedure (wand: PPixelWand; const opacity: Quantum); cdecl;
  TPixelGetMagickColor = procedure (const wand: PPixelWand; packet: PMagickPixelPacket); cdecl;

var
  PixelGetColorAsString:TPixelGetColorAsString=nil;
  PixelGetException:TPixelGetException=nil;
  PixelGetAlpha:TPixelGetAlpha=nil;
  PixelGetBlack:TPixelGetBlack=nil;
  PixelGetBlue:TPixelGetBlue=nil;
  PixelGetCyan:TPixelGetCyan=nil;
  PixelGetMagenta : TPixelGetMagenta=nil;
  PixelGetOpacity : TPixelGetOpacity =nil;
  PixelGetRed : TPixelGetRed=nil;
  PixelGetYellow : TPixelGetYellow=nil;
  PixelGetIndex : TPixelGetIndex =nil;

  IsPixelWand : TIsPixelWand =nil;
  IsPixelWandSimilar : TIsPixelWandSimilar =nil;
  PixelClearException : TPixelClearException=nil;
  PixelSetColor : TPixelSetColor=nil;

  DestroyPixelWand : TDestroyPixelWand= nil;
  DestroyPixelWands : TDestroyPixelWands =nil;
  NewPixelWand : TNewPixelWand = nil;
  NewPixelWands : TNewPixelWands = nil;

  PixelGetAlphaQuantum : TPixelGetAlphaQuantum = nil;
  PixelGetBlackQuantum : TPixelGetBlackQuantum = nil;
  PixelGetBlueQuantum : TPixelGetBlueQuantum = nil;
  PixelGetCyanQuantum : TPixelGetCyanQuantum = nil;
  PixelGetGreenQuantum : TPixelGetGreenQuantum = nil;
  PixelGetMagentaQuantum : TPixelGetMagentaQuantum = nil;
  PixelGetOpacityQuantum : TPixelGetOpacityQuantum = nil;
  PixelGetRedQuantum : TPixelGetRedQuantum = nil;
  PixelGetYellowQuantum : TPixelGetYellowQuantum = nil;

  PixelGetColorCount : TPixelGetColorCount =nil;

  ClearPixelWand :TClearPixelWand=nil;
  PixelGetQuantumColor : TPixelGetQuantumColor =nil;
  PixelSetAlpha: TPixelSetAlpha = nil;
  PixelSetAlphaQuantum: TPixelSetAlphaQuantum = nil;
  PixelSetBlack: TPixelSetBlack =nil;
  PixelSetBlackQuantum : TPixelSetBlackQuantum =nil;
  PixelSetBlue : TPixelSetBlue=nil;
  PixelSetBlueQuantum : TPixelSetBlueQuantum=nil;
  PixelSetColorCount:TPixelSetColorCount=nil;

  PixelSetCyan: TPixelSetCyan=nil;
  PixelSetCyanQuantum:TPixelSetCyanQuantum = nil;
  PixelSetGreen:TPixelSetGreen=nil;
  PixelSetGreenQuantum:TPixelSetGreenQuantum=nil;

  PixelSetIndex:TPixelSetIndex=nil;
  PixelSetMagenta:TPixelSetMagenta=nil;
  PixelSetMagentaQuantum:TPixelSetMagentaQuantum=nil;
  PixelSetMagickColor:TPixelSetMagickColor=nil;
  PixelSetOpacity:TPixelSetOpacity=nil;
  PixelSetOpacityQuantum:TPixelSetOpacityQuantum=nil;

  PixelSetRed:TPixelSetRed=nil;
  PixelSetRedQuantum:TPixelSetRedQuantum=nil;
  PixelSetYellow:TPixelSetYellow=nil;
  PixelSetYellowQuantum:TPixelSetYellowQuantum=nil;

  PixelGetMagickColor:TPixelGetMagickColor=nil;
{$ENDIF INTERFACE}

{$IFDEF IMPLEMENTATION}
Pointer(PixelGetColorAsString):=GetProcAddress(LibWand, 'PixelGetColorAsString');
Pointer(PixelGetException):=GetProcAddress(LibWand, 'PixelGetException');
Pointer(PixelGetAlpha):=GetProcAddress(LibWand, 'PixelGetAlpha');
Pointer(PixelGetBlack):=GetProcAddress(LibWand, 'PixelGetBlack');
Pointer(PixelGetBlue):=GetProcAddress(LibWand, 'PixelGetBlue');
Pointer(PixelGetCyan):=GetProcAddress(LibWand, 'PixelGetCyan');
Pointer(PixelGetMagenta):=GetProcAddress(LibWand, 'PixelGetMagenta');
Pointer(PixelGetOpacity):=GetProcAddress(LibWand, 'PixelGetOpacity');
Pointer(PixelGetRed):=GetProcAddress(LibWand, 'PixelGetRed');
Pointer(PixelGetYellow):=GetProcAddress(LibWand, 'PixelGetYellow');
Pointer(PixelGetIndex):=GetProcAddress(LibWand, 'PixelGetIndex');

Pointer(IsPixelWand):=GetProcAddress(LibWand, 'IsPixelWand');
Pointer(IsPixelWandSimilar):=GetProcAddress(LibWand, 'IsPixelWandSimilar');
Pointer(PixelClearException):=GetProcAddress(LibWand, 'PixelClearException');
Pointer(PixelSetColor):=GetProcAddress(LibWand, 'PixelSetColor');

Pointer(DestroyPixelWand):=GetProcAddress(LibWand, 'DestroyPixelWand');
Pointer(DestroyPixelWands):=GetProcAddress(LibWand, 'DestroyPixelWands');
Pointer(NewPixelWand):=GetProcAddress(LibWand, 'NewPixelWand');
Pointer(NewPixelWands):=GetProcAddress(LibWand, 'NewPixelWands');

Pointer(PixelGetAlphaQuantum):=GetProcAddress(LibWand, 'PixelGetAlphaQuantum');
Pointer(PixelGetBlackQuantum):=GetProcAddress(LibWand, 'PixelGetBlackQuantum');
Pointer(PixelGetBlueQuantum):=GetProcAddress(LibWand, 'PixelGetBlueQuantum');
Pointer(PixelGetCyanQuantum):=GetProcAddress(LibWand, 'PixelGetCyanQuantum');
Pointer(PixelGetGreenQuantum):=GetProcAddress(LibWand, 'PixelGetGreenQuantum');
Pointer(PixelGetMagentaQuantum):=GetProcAddress(LibWand, 'PixelGetMagentaQuantum');
Pointer(PixelGetOpacityQuantum):=GetProcAddress(LibWand, 'PixelGetOpacityQuantum');
Pointer(PixelGetRedQuantum):=GetProcAddress(LibWand, 'PixelGetRedQuantum');
Pointer(PixelGetYellowQuantum):=GetProcAddress(LibWand, 'PixelGetYellowQuantum');

Pointer(PixelGetColorCount):=GetProcAddress(LibWand, 'PixelGetColorCount');

Pointer(ClearPixelWand):=GetProcAddress(LibWand, 'ClearPixelWand');
Pointer(PixelGetQuantumColor):=GetProcAddress(LibWand, 'PixelGetQuantumColor');
Pointer(PixelSetAlpha):=GetProcAddress(LibWand, 'PixelSetAlpha');
Pointer(PixelSetAlphaQuantum):=GetProcAddress(LibWand, 'PixelSetAlphaQuantum');
Pointer(PixelSetBlack):=GetProcAddress(LibWand, 'PixelSetBlack');
Pointer(PixelSetBlackQuantum):=GetProcAddress(LibWand, 'PixelSetBlackQuantum');
Pointer(PixelSetBlue):=GetProcAddress(LibWand, 'PixelSetBlue');
Pointer(PixelSetBlueQuantum):=GetProcAddress(LibWand, 'PixelSetBlueQuantum');
Pointer(PixelSetColorCount):=GetProcAddress(LibWand, 'PixelSetColorCount');

Pointer(PixelSetCyan):=GetProcAddress(LibWand, 'PixelSetCyan');
Pointer(PixelSetCyanQuantum):=GetProcAddress(LibWand, 'PixelSetCyanQuantum');
Pointer(PixelSetGreen):=GetProcAddress(LibWand, 'PixelSetGreen');
Pointer(PixelSetGreenQuantum):=GetProcAddress(LibWand, 'PixelSetGreenQuantum');

Pointer(PixelSetIndex):=GetProcAddress(LibWand, 'PixelSetIndex');
Pointer(PixelSetMagenta):=GetProcAddress(LibWand, 'PixelSetMagenta');
Pointer(PixelSetMagentaQuantum):=GetProcAddress(LibWand, 'PixelSetMagentaQuantum');
Pointer(PixelSetMagickColor):=GetProcAddress(LibWand, 'PixelSetMagickColor');
Pointer(PixelSetOpacity):=GetProcAddress(LibWand, 'PixelSetOpacity');
Pointer(PixelSetOpacityQuantum):=GetProcAddress(LibWand, 'PixelSetOpacityQuantum');

Pointer(PixelSetRed):=GetProcAddress(LibWand, 'PixelSetRed');
Pointer(PixelSetRedQuantum):=GetProcAddress(LibWand, 'PixelSetRedQuantum');
Pointer(PixelSetYellow):=GetProcAddress(LibWand, 'PixelSetYellow');
Pointer(PixelSetYellowQuantum):=GetProcAddress(LibWand, 'PixelSetYellowQuantum');
Pointer(PixelGetMagickColor):=GetProcAddress(LibWand, 'PixelGetMagickColor');

{$ENDIF IMPLEMENTATION}
