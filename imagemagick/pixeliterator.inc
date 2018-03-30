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

  Pixel Iterator Methods.
}
{$IFDEF INTERFACE}
type
  PixelIterator = record
    id: culong;
    name: array[0..MaxTextExtent] of char;
    exception: ExceptionInfo;
    view: PViewInfo;
    region: RectangleInfo;
    y: cint;
    pixel_wand: PPPixelWand;
    debug: MagickBooleanType;
    signature: culong;
  end;
  PPixelIterator = ^PixelIterator;

  {WandExports}

  TPixelGetIteratorException=function(const iterator: PPixeliterator; severity: PExceptionType): PChar; cdecl;
  TIsPixelIterator=function (const iterator: PPixeliterator): MagickBooleanType; cdecl;
  TPixelClearIteratorException=function (iterator: PPixeliterator): MagickBooleanType; cdecl;
  TPixelSetIteratorRow=function (iterator: PPixeliterator; const row: cint): MagickBooleanType; cdecl;
  TPixelSyncIterator=function (iterator: PPixeliterator): MagickBooleanType; cdecl;

  TDestroyPixelIterator=function (iterator: PPixeliterator): PPixelIterator; cdecl;
  TNewPixelIterator=function (wand: PMagickWand): PPixelIterator; cdecl;
  TNewPixelRegionIterator=function ( wand: PMagickWand; const x, y: cint; const columns, rows: culong; const modify: MagickBooleanType): PPixelIterator; cdecl;

  TPixelGetNextIteratorRow=function (iterator: PPixeliterator; var wandCount: culong): PPPixelWand; cdecl;
  TPixelGetPreviousIteratorRow=function (iterator: PPixeliterator; var wandCount: culong): PPPixelWand; cdecl;

  TClearPixelIterator=procedure (iterator: PPixeliterator); cdecl;
  TPixelResetIterator=procedure (iterator: PPixeliterator); cdecl;
  TPixelSetFirstIteratorRow=procedure (iterator: PPixeliterator); cdecl;
  TPixelSetLastIteratorRow=procedure (iterator: PPixeliterator); cdecl;

  var
    PixelGetIteratorException:TPixelGetIteratorException=nil;
    IsPixelIterator:TIsPixelIterator=nil;
    PixelClearIteratorException:TPixelClearIteratorException=nil;
    PixelSetIteratorRow:TPixelSetIteratorRow=nil;
    PixelSyncIterator:TPixelSyncIterator=nil;

    DestroyPixelIterator:TDestroyPixelIterator=nil;
    NewPixelIterator:TNewPixelIterator=nil;
    NewPixelRegionIterator:TNewPixelRegionIterator=nil;

    PixelGetNextIteratorRow:TPixelGetNextIteratorRow=nil;
    PixelGetPreviousIteratorRow:TPixelGetPreviousIteratorRow=nil;

    ClearPixelIterator:TClearPixelIterator=nil;
    PixelResetIterator:TPixelResetIterator=nil;
    PixelSetFirstIteratorRow:TPixelSetFirstIteratorRow=nil;
    PixelSetLastIteratorRow:TPixelSetLastIteratorRow=nil;

{$ENDIF INTERFACE}

{$IFDEF IMPLEMENTATION}
  Pointer(PixelGetIteratorException):=GetProcAddress(LibWand, 'PixelGetIteratorException');
  Pointer(IsPixelIterator):=GetProcAddress(LibWand, 'IsPixelIterator');
  Pointer(PixelClearIteratorException):=GetProcAddress(LibWand, 'PixelClearIteratorException');
  Pointer(PixelSetIteratorRow):=GetProcAddress(LibWand, 'PixelSetIteratorRow');
  Pointer(PixelSyncIterator):=GetProcAddress(LibWand, 'PixelSyncIterator');

  Pointer(DestroyPixelIterator):=GetProcAddress(LibWand, 'DestroyPixelIterator');
  Pointer(NewPixelIterator):=GetProcAddress(LibWand, 'NewPixelIterator');
  Pointer(NewPixelRegionIterator):=GetProcAddress(LibWand, 'NewPixelRegionIterator');

  Pointer(PixelGetNextIteratorRow):=GetProcAddress(LibWand, 'PixelGetNextIteratorRow');
  Pointer(PixelGetPreviousIteratorRow):=GetProcAddress(LibWand, 'PixelGetPreviousIteratorRow');

  Pointer(ClearPixelIterator):=GetProcAddress(LibWand, 'ClearPixelIterator');
  Pointer(PixelResetIterator):=GetProcAddress(LibWand, 'PixelResetIterator');
  Pointer(PixelSetFirstIteratorRow):=GetProcAddress(LibWand, 'PixelSetFirstIteratorRow');
  Pointer(PixelSetLastIteratorRow):=GetProcAddress(LibWand, 'PixelSetLastIteratorRow');

{$ENDIF IMPLEMENTATION}
