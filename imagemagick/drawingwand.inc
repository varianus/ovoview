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

  ImageMagick drawing wand API.
}

{
  Declaration from drawing-wand.c
}
type
 PathOperation = (
  PathDefaultOperation,
  PathCloseOperation,                        // Z|z (none) */
  PathCurveToOperation,                      // C|c (x1 y1 x2 y2 x y)+ */
  PathCurveToQuadraticBezierOperation,       // Q|q (x1 y1 x y)+ */
  PathCurveToQuadraticBezierSmoothOperation, // T|t (x y)+ */
  PathCurveToSmoothOperation,                // S|s (x2 y2 x y)+ */
  PathEllipticArcOperation,                  // A|a (rx ry x-axis-rotation large-arc-flag sweep-flag x y)+ */
  PathLineToHorizontalOperation,             // H|h x+ */
  PathLineToOperation,                       // L|l (x y)+ */
  PathLineToVerticalOperation,               // V|v y+ */
  PathMoveToOperation                        // M|m (x y)+ */
 );

type
 PathMode = (
  DefaultPathMode,
  AbsolutePathMode,
  RelativePathMode
 );

  TDrawingWand = record
    id          : culong;
    name        : array[0..MaxTextExtent] of Char;
    { Support structures }
    image       : PImage;
    exception   : ExceptionInfo;
    { MVG output string and housekeeping }
    mvg         : PChar;               // MVG data
    mvg_alloc   : culong;                // total allocated memory
//    mvg_length: size_t;       // total MVG length
    mvg_width   : culong;      // current line width
    { Pattern support }
    pattern_id      : PChar;
    pattern_bounds  : RectangleInfo;
//    pattern_offset: size_t;
    { Graphic wand }
    index           : culong;          // array index
    graphic_context : PPDrawInfo;
    filter_off      : MagickBooleanType;    // true if not filtering attributes
    { Pretty-printing depth }
    indent_depth    : culong;       // number of left-hand pad characters
    { Path operation support }
    path_operation  : PathOperation;
    path_mode       : PathMode;
    destroy         : MagickBooleanType;
    debug           : MagickBooleanType;
    signature       : culong;
  end;
  PDrawingWand = ^TDrawingWand;

  const x = 9;
{WandExport}
Type
 TDrawGetTextAlignment=function (const wand: PDrawingWand): AlignType; cdecl;
 TDrawGetClipPath=function (const wand: PDrawingWand): PChar; cdecl;
 TDrawGetException=function (const wand: PDrawingWand; severity: PExceptionType): PChar; cdecl;
 TDrawGetFont=function (const wand: PDrawingWand): PChar; cdecl;
 TDrawGetFontFamily=function (const wand: PDrawingWand): PChar; cdecl;
 TDrawGetTextEncoding=function (const wand: PDrawingWand): PChar; cdecl;
 TDrawGetVectorGraphics=function (const wand: PDrawingWand): PChar; cdecl;

 TDrawGetClipUnits=function (const wand: PDrawingWand): ClipPathUnits; cdecl;

 TDrawGetTextDecoration=function (const wand: PDrawingWand): DecorationType; cdecl;

 TDrawGetFillAlpha=function (const wand: PDrawingWand): double; cdecl;
 TDrawGetFontSize=function (const wand: PDrawingWand): double; cdecl;
 TDrawGetStrokeDashArray=function (const wand: PDrawingWand; number_elements: culong): PDouble; cdecl;
 TDrawGetStrokeDashOffset=function (const wand: PDrawingWand): double; cdecl;
 TDrawGetStrokeAlpha=function (const wand: PDrawingWand): double; cdecl;
 TDrawGetStrokeWidth=function (const wand: PDrawingWand): double; cdecl;

 TPeekDrawingWand=function (const wand: PDrawingWand): PDrawInfo; cdecl;

 TCloneDrawingWand=function (const wand: PDrawingWand): PDrawingWand; cdecl;
 TDestroyDrawingWand=function (wand: PDrawingWand): PDrawingWand; cdecl;
 TNewDrawingWand=function : PDrawingWand; cdecl;

 TDrawGetClipRule=function (const wand: PDrawingWand): FillRule; cdecl;
 TDrawGetFillRule=function (const wand: PDrawingWand): FillRule; cdecl;

 TDrawGetGravity=function (const wand: PDrawingWand): GravityType; cdecl;
 TDrawGetStrokeLineCap=function (const wand: PDrawingWand): LineCap; cdecl;

 TDrawGetStrokeLineJoin=function (const wand: PDrawingWand): LineJoin; cdecl;

 TDrawClearException=function (wand: PDrawingWand): MagickBooleanType; cdecl;
 TDrawComposite=function (wand: PDrawingWand; const compose: CompositeOperator; const x, y, width, height: double; magick_wand: PMagickWand): MagickBooleanType; cdecl;
 TDrawGetStrokeAntialias=function (const wand: PDrawingWand): MagickBooleanType; cdecl;
 TDrawGetTextAntialias=function (const wand: PDrawingWand): MagickBooleanType; cdecl;
 TDrawPopPattern=function (wand: PDrawingWand): MagickBooleanType; cdecl;
 TDrawPushPattern=function (wand: PDrawingWand; const pattern_id: PChar; const x, y, width, height: double): MagickBooleanType; cdecl;
 TDrawRender=function (wand: PDrawingWand): MagickBooleanType; cdecl;
 TDrawSetClipPath=function (wand: PDrawingWand; const clip_path: PChar): MagickBooleanType; cdecl;
 TDrawSetFillPatternURL=function (wand: PDrawingWand; const fill_url: PChar): MagickBooleanType; cdecl;
 TDrawSetFont=function (wand: PDrawingWand; const font_name: PChar): MagickBooleanType; cdecl;
 TDrawSetFontFamily=function (wand: PDrawingWand; const font_family: PChar): MagickBooleanType; cdecl;
 TDrawSetStrokeDashArray=function (wand: PDrawingWand; const number_elements: culong; const dash_array: Pdouble): MagickBooleanType; cdecl;
 TDrawSetStrokePatternURL=function (wand: PDrawingWand; const stroke_url: PChar): MagickBooleanType; cdecl;
 TDrawSetVectorGraphics=function (wand: PDrawingWand; const xml: PChar): MagickBooleanType; cdecl;
 TIsDrawingWand=function (const wand: PDrawingWand): MagickBooleanType; cdecl;
 TPopDrawingWand=function (wand: PDrawingWand): MagickBooleanType; cdecl;
 TPushDrawingWand=function (wand: PDrawingWand): MagickBooleanType; cdecl;

 TDrawGetFontStretch=function (const wand: PDrawingWand): StretchType; cdecl;

 TDrawGetFontStyle=function (const wand: PDrawingWand): StyleType; cdecl;

 TDrawGetFontWeight=function (const wand: PDrawingWand): culong; cdecl;
 TDrawGetStrokeMiterLimit=function (const wand: PDrawingWand): culong; cdecl;

 TClearDrawingWand=procedure (wand: PDrawingWand); cdecl;
 TDrawAffine=procedure (wand: PDrawingWand; const affine: PAffineMatrix); cdecl;
 TDrawAnnotation=procedure (wand: PDrawingWand; const  x, y: double; const text: PChar); cdecl;
 TDrawArc=procedure (wand: PDrawingWand; const sx, sy, ex, ey, sd, ed: double); cdecl;
 TDrawBezier=procedure (wand: PDrawingWand; const number_coordinates: culong; const coordinates: PPointInfo); cdecl;
 TDrawCircle=procedure (wand: PDrawingWand; const ox, oy, px, py: double); cdecl;
 TDrawColor=procedure (wand: PDrawingWand; const x, y: double; const paint_method: PaintMethod); cdecl;
 TDrawComment=procedure (wand: PDrawingWand; const comment: PChar); cdecl;
 TDrawEllipse=procedure (wand: PDrawingWand; const ox, oy, rx, ry, start, end_: double); cdecl;
 TDrawGetFillColor=procedure (const wand: PDrawingWand; fill_color: PPixelWand); cdecl;
 TDrawGetStrokeColor=procedure (const wand: PDrawingWand; stroke_color: PPixelWand); cdecl;
 TDrawGetTextUnderColor=procedure (const wand: PDrawingWand; under_color: PPixelWand); cdecl;
 TDrawLine=procedure (wand: PDrawingWand; const sx, sy, ex, ey: double); cdecl;
 TDrawMatte=procedure (wand: PDrawingWand; const x, y: double; const paint_method: PaintMethod); cdecl;
 TDrawPathClose=procedure (wand: PDrawingWand); cdecl;
 TDrawPathCurveToAbsolute=procedure (wand: PDrawingWand; const x1, y1, x2, y2, x, y: double); cdecl;
 TDrawPathCurveToRelative=procedure (wand: PDrawingWand; const x1, y1, x2, y2, x, y: double); cdecl;
 TDrawPathCurveToQuadraticBezierAbsolute=procedure (wand: PDrawingWand; const x1, y1, x, y: double); cdecl;
 TDrawPathCurveToQuadraticBezierRelative=procedure (wand: PDrawingWand; const x1, y1, x, y: double); cdecl;
 TDrawPathCurveToQuadraticBezierSmoothAbsolute=procedure (wand: PDrawingWand; const x, y: double); cdecl;
 TDrawPathCurveToQuadraticBezierSmoothRelative=procedure (wand: PDrawingWand; const x, y: double); cdecl;
 TDrawPathCurveToSmoothAbsolute=procedure (wand: PDrawingWand; const x2, y2, x, y: double); cdecl;
 TDrawPathCurveToSmoothRelative=procedure (wand: PDrawingWand; const x2, y2, x, y: double); cdecl;
 TDrawPathEllipticArcAbsolute=procedure (wand: PDrawingWand; const rx, ry, x_axis_rotation: double; const large_arc_flag: MagickBooleanType; const sweep_flag: MagickBooleanType; const x, y: double); cdecl;
 TDrawPathEllipticArcRelative=procedure (wand: PDrawingWand; const rx, ry, x_axis_rotation: double; const large_arc_flag: MagickBooleanType; const sweep_flag: MagickBooleanType; const x, y: double); cdecl;
 TDrawPathFinish=procedure (wand: PDrawingWand); cdecl;
 TDrawPathLineToAbsolute=procedure (wand: PDrawingWand; const x, y: double); cdecl;
 TDrawPathLineToRelative=procedure (wand: PDrawingWand; const x, y: double); cdecl;
 TDrawPathLineToVerticalAbsolute=procedure (wand: PDrawingWand; const y: double); cdecl;
 TDrawPathLineToVerticalRelative=procedure (wand: PDrawingWand; const y: double); cdecl;
 TDrawPathMoveToAbsolute=procedure (wand: PDrawingWand; const x, y: double); cdecl;
 TDrawPathMoveToRelative=procedure (wand: PDrawingWand; const x, y: double); cdecl;
 TDrawPathStart=procedure (wand: PDrawingWand); cdecl;
 TDrawPoint=procedure (wand: PDrawingWand; const x, y: double); cdecl;
 TDrawPolygon=procedure (wand: PDrawingWand; const number_coordinates: culong; const coordinates: PPointInfo); cdecl;
 TDrawPolyline=procedure (wand: PDrawingWand; const number_coordinates: culong; const coordinates: PPointInfo); cdecl;
 TDrawPopClipPath=procedure (wand: PDrawingWand); cdecl;
 TDrawPopDefs=procedure (wand: PDrawingWand); cdecl;
 TDrawPushClipPath=procedure (wand: PDrawingWand; clip_path_id: PChar); cdecl;
 TDrawPushDefs=procedure (wand: PDrawingWand); cdecl;
 TDrawRectangle=procedure (wand: PDrawingWand; const x1, y1, x2, y2: double); cdecl;
 TDrawRotate=procedure (wand: PDrawingWand; const degrees: double); cdecl;
 TDrawRoundRectangle=procedure (wand: PDrawingWand; const x1, y1, x2, y2, rx, ry: double); cdecl;
 TDrawScale=procedure (wand: PDrawingWand; const x, y: double); cdecl;
 TDrawSetClipRule=procedure (wand: PDrawingWand; const fill_rule: FillRule); cdecl;
 TDrawSetClipUnits=procedure (wand: PDrawingWand; const clip_units: ClipPathUnits); cdecl;
 TDrawSetFillColor=procedure (wand: PDrawingWand; const fill_wand: PPixelWand); cdecl;
 TDrawSetFillAlpha=procedure (wand: PDrawingWand; const fill_opacity: double); cdecl;
 TDrawSetFillRule=procedure (wand: PDrawingWand; const fill_rule: FillRule); cdecl;
 TDrawSetFontSize=procedure (wand: PDrawingWand; const pointsize: double); cdecl;
 TDrawSetFontStretch=procedure (wand: PDrawingWand; const font_stretch: StretchType); cdecl;
 TDrawSetFontStyle=procedure (wand: PDrawingWand; const style: StyleType); cdecl;
 TDrawSetFontWeight=procedure (wand: PDrawingWand; const font_weight: culong); cdecl;
 TDrawSetGravity=procedure (wand: PDrawingWand; const gravity: GravityType); cdecl;
 TDrawSkewX=procedure (wand: PDrawingWand; const degrees: double); cdecl;
 TDrawSkewY=procedure (wand: PDrawingWand; const degrees: double); cdecl;
 TDrawSetStrokeAntialias=procedure (wand: PDrawingWand; const stroke_antialias: MagickBooleanType); cdecl;
 TDrawSetStrokeColor=procedure (wand: PDrawingWand; const stroke_wand: PPixelWand); cdecl;
 TDrawSetStrokeDashOffset=procedure (wand: PDrawingWand; const dash_offset: double); cdecl;
 TDrawSetStrokeLineCap=procedure (wand: PDrawingWand; const linecap_: LineCap); cdecl;
 TDrawSetStrokeLineJoin=procedure (wand: PDrawingWand; const linejoin_: LineJoin); cdecl;
 TDrawSetStrokeMiterLimit=procedure (wand: PDrawingWand; const miterlimit: culong); cdecl;
 TDrawSetStrokeAlpha=procedure (wand: PDrawingWand; const stroke_opacity: double); cdecl;
 TDrawSetStrokeWidth=procedure (wand: PDrawingWand; const troke_width: double); cdecl;
 TDrawSetTextAlignment=procedure (wand: PDrawingWand; const alignment: AlignType); cdecl;
 TDrawSetTextAntialias=procedure (wand: PDrawingWand; const text_antialias: MagickBooleanType); cdecl;
 TDrawSetTextDecoration=procedure (wand: PDrawingWand; const decoration: DecorationType); cdecl;
 TDrawSetTextEncoding=procedure (wand: PDrawingWand; const encoding: PChar); cdecl;
 TDrawSetTextUnderColor=procedure (wand: PDrawingWand; const under_wand: PPixelWand); cdecl;
 TDrawSetViewbox=procedure (wand: PDrawingWand; x1, y1, x2, y2: culong); cdecl;
 TDrawTranslate=procedure (wand: PDrawingWand; const x, y: double); cdecl;

var
  DrawGetTextAlignment:TDrawGetTextAlignment=nil;

  DrawGetClipPath:TDrawGetClipPath=nil;
  DrawGetException:TDrawGetException=nil;
  DrawGetFont:TDrawGetFont=nil;
  DrawGetFontFamily:TDrawGetFontFamily=nil;
  DrawGetTextEncoding:TDrawGetTextEncoding=nil;
  DrawGetVectorGraphics:TDrawGetVectorGraphics=nil;

  DrawGetClipUnits:TDrawGetClipUnits=nil;

  DrawGetTextDecoration:TDrawGetTextDecoration=nil;

  DrawGetFillAlpha:TDrawGetFillAlpha=nil;
  DrawGetFontSize:TDrawGetFontSize=nil;
  DrawGetStrokeDashArray:TDrawGetStrokeDashArray=nil;
  DrawGetStrokeDashOffset:TDrawGetStrokeDashOffset=nil;
  DrawGetStrokeAlpha:TDrawGetStrokeAlpha=nil;
  DrawGetStrokeWidth:TDrawGetStrokeWidth=nil;

  PeekDrawingWand:TPeekDrawingWand=nil;

  CloneDrawingWand:TCloneDrawingWand=nil;
  DestroyDrawingWand:TDestroyDrawingWand=nil;

  NewDrawingWand:TNewDrawingWand=nil;

  DrawGetClipRule:TDrawGetClipRule=nil;
  DrawGetFillRule:TDrawGetFillRule=nil;

  DrawGetGravity:TDrawGetGravity=nil;

  DrawGetStrokeLineCap:TDrawGetStrokeLineCap=nil;

  DrawGetStrokeLineJoin:TDrawGetStrokeLineJoin=nil;

  DrawClearException:TDrawClearException=nil;
  DrawComposite:TDrawComposite=nil;
  DrawGetStrokeAntialias:TDrawGetStrokeAntialias=nil;
  DrawGetTextAntialias:TDrawGetTextAntialias=nil;
  DrawPopPattern:TDrawPopPattern=nil;
  DrawPushPattern:TDrawPushPattern=nil;
  DrawRender:TDrawRender=nil;
  DrawSetClipPath:TDrawSetClipPath=nil;
  DrawSetFillPatternURL:TDrawSetFillPatternURL=nil;
  DrawSetFont:TDrawSetFont=nil;
  DrawSetFontFamily:TDrawSetFontFamily=nil;
  DrawSetStrokeDashArray:TDrawSetStrokeDashArray=nil;
  DrawSetStrokePatternURL:TDrawSetStrokePatternURL=nil;
  DrawSetVectorGraphics:TDrawSetVectorGraphics=nil;
  IsDrawingWand:TIsDrawingWand=nil;
  PopDrawingWand:TPopDrawingWand=nil;
  PushDrawingWand:TPushDrawingWand=nil;

  DrawGetFontStretch:TDrawGetFontStretch=nil;

  DrawGetFontStyle:TDrawGetFontStyle=nil;

  DrawGetFontWeight:TDrawGetFontWeight=nil;
  DrawGetStrokeMiterLimit:TDrawGetStrokeMiterLimit=nil;

  ClearDrawingWand:TClearDrawingWand=nil;
  DrawAffine:TDrawAffine=nil;
  DrawAnnotation:TDrawAnnotation=nil;
  DrawArc:TDrawArc=nil;
  DrawBezier:TDrawBezier=nil;
  DrawCircle:TDrawCircle=nil;
  DrawColor:TDrawColor=nil;
  DrawComment:TDrawComment=nil;
  DrawEllipse:TDrawEllipse=nil;
  DrawGetFillColor:TDrawGetFillColor=nil;
  DrawGetStrokeColor:TDrawGetStrokeColor=nil;
  DrawGetTextUnderColor:TDrawGetTextUnderColor=nil;
  DrawLine:TDrawLine=nil;
  DrawMatte:TDrawMatte=nil;
  DrawPathClose:TDrawPathClose=nil;
  DrawPathCurveToAbsolute:TDrawPathCurveToAbsolute=nil;
  DrawPathCurveToRelative:TDrawPathCurveToRelative=nil;
  DrawPathCurveToQuadraticBezierAbsolute:TDrawPathCurveToQuadraticBezierAbsolute=nil;
  DrawPathCurveToQuadraticBezierRelative:TDrawPathCurveToQuadraticBezierRelative=nil;
  DrawPathCurveToQuadraticBezierSmoothAbsolute:TDrawPathCurveToQuadraticBezierSmoothAbsolute=nil;
  DrawPathCurveToQuadraticBezierSmoothRelative:TDrawPathCurveToQuadraticBezierSmoothRelative=nil;
  DrawPathCurveToSmoothAbsolute:TDrawPathCurveToSmoothAbsolute=nil;
  DrawPathCurveToSmoothRelative:TDrawPathCurveToSmoothRelative=nil;
  DrawPathEllipticArcAbsolute:TDrawPathEllipticArcAbsolute=nil;
  DrawPathEllipticArcRelative:TDrawPathEllipticArcRelative=nil;
  DrawPathFinish:TDrawPathFinish=nil;
  DrawPathLineToAbsolute:TDrawPathLineToAbsolute=nil;
  DrawPathLineToRelative:TDrawPathLineToRelative=nil;
  DrawPathLineToVerticalAbsolute:TDrawPathLineToVerticalAbsolute=nil;
  DrawPathLineToVerticalRelative:TDrawPathLineToVerticalRelative=nil;
  DrawPathMoveToAbsolute:TDrawPathMoveToAbsolute=nil;
  DrawPathMoveToRelative:TDrawPathMoveToRelative=nil;
  DrawPathStart:TDrawPathStart=nil;
  DrawPoint:TDrawPoint=nil;
  DrawPolygon:TDrawPolygon=nil;
  DrawPolyline:TDrawPolyline=nil;
  DrawPopClipPath:TDrawPopClipPath=nil;
  DrawPopDefs:TDrawPopDefs=nil;
  DrawPushClipPath:TDrawPushClipPath=nil;
  DrawPushDefs:TDrawPushDefs=nil;
  DrawRectangle:TDrawRectangle=nil;
  DrawRotate:TDrawRotate=nil;
  DrawRoundRectangle:TDrawRoundRectangle=nil;
  DrawScale:TDrawScale=nil;
  DrawSetClipRule:TDrawSetClipRule=nil;
  DrawSetClipUnits:TDrawSetClipUnits=nil;
  DrawSetFillColor:TDrawSetFillColor=nil;
  DrawSetFillAlpha:TDrawSetFillAlpha=nil;
  DrawSetFillRule:TDrawSetFillRule=nil;
  DrawSetFontSize:TDrawSetFontSize=nil;
  DrawSetFontStretch:TDrawSetFontStretch=nil;
  DrawSetFontStyle:TDrawSetFontStyle=nil;
  DrawSetFontWeight:TDrawSetFontWeight=nil;
  DrawSetGravity:TDrawSetGravity=nil;
  DrawSkewX:TDrawSkewX=nil;
  DrawSkewY:TDrawSkewY=nil;
  DrawSetStrokeAntialias:TDrawSetStrokeAntialias=nil;
  DrawSetStrokeColor:TDrawSetStrokeColor=nil;
  DrawSetStrokeDashOffset:TDrawSetStrokeDashOffset=nil;
  DrawSetStrokeLineCap:TDrawSetStrokeLineCap=nil;
  DrawSetStrokeLineJoin:TDrawSetStrokeLineJoin=nil;
  DrawSetStrokeMiterLimit:TDrawSetStrokeMiterLimit=nil;
  DrawSetStrokeAlpha:TDrawSetStrokeAlpha=nil;
  DrawSetStrokeWidth:TDrawSetStrokeWidth=nil;
  DrawSetTextAlignment:TDrawSetTextAlignment=nil;
  DrawSetTextAntialias:TDrawSetTextAntialias=nil;
  DrawSetTextDecoration:TDrawSetTextDecoration=nil;
  DrawSetTextEncoding:TDrawSetTextEncoding=nil;
  DrawSetTextUnderColor:TDrawSetTextUnderColor=nil;
  DrawSetViewbox:TDrawSetViewbox=nil;
  DrawTranslate:TDrawTranslate=nil;

