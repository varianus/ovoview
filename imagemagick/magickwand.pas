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

  ImageMagick MagickWand API.
}
{
  Based on ImageMagick 6.2

  Converted from c by: Felipe Monteiro de Carvalho Dez/2005

	Bug-fixed by Ángel Eduardo García Hernández
	Thanks to Marc Geldon and RuBBeR
}
{Version 0.4}
unit MagickWand;

{$IFDEF FPC}
  {$mode objfpc}
	{$PACKRECORDS C}
{$ENDIF}

{$z4}

interface

uses ImageMagick, ctypes, DynLibs;

{ Various types }
type
  TMagickWand = record
    id            : culong;
    name          : array[1..MaxTextExtent] of Char;
    exception     : ExceptionInfo;
    image_info    : PImageInfo;
    quantize_info : PQuantizeInfo;
    images        : PImage;
    active        : MagickBooleanType;
    pend          : MagickBooleanType;
    debug         : MagickBooleanType;
    signature     : culong;
  end;
  PMagickWand = ^TMagickWand;

  {$DEFINE INTERFACE}
  {$include PixelWand.inc}      // <- has externals
  {$include DrawingWand.inc}    // <- has externals
  {.$include MagickAttribute.inc} // <- has externals
  {.$include PixelIterator.inc}  // <- has externals
  {$include MagickProperty.inc}
  {$UNDEF INTERFACE}
Type
  TMagickGetImageChannelStatistics=function (wand: PMagickWand): PChannelStatistics; cdecl;
  TMagickGetImageAttribute=function (wand: PMagickWand): PChar; cdecl;
  TMagickGetImageFilename=function (wand: PMagickWand): PChar; cdecl;
  TMagickGetImageFormat=function (wand: PMagickWand): PChar; cdecl;
  TMagickGetImageSignature=function (wand: PMagickWand): PChar; cdecl;
  TMagickIdentifyImage=function (wand: PMagickWand): PChar; cdecl;
  TMagickGetImageCompose=function (wand: PMagickWand): CompositeOperator; cdecl;
  TMagickGetImageColorspace=function (wand: PMagickWand): ColorspaceType; cdecl;
  TMagickGetImageCompression=function (wand: PMagickWand): CompressionType; cdecl;
  TMagickGetImageDispose=function (wand: PMagickWand): DisposeType; cdecl;
  TMagickGetImageGamma=function (wand: PMagickWand): double; cdecl;
  TMagickGetImageTotalInkDensity = function (wand: PMagickWand): double; cdecl;
  TGetImageFromMagickWand=function (wand: PMagickWand): PImage; cdecl;
  TMagickGetImageType=function (wand: PMagickWand): ImageType; cdecl;
  TMagickGetImageInterlaceScheme=function (wand: PMagickWand): InterlaceType; cdecl;
  TMagickGetImageIndex=function (wand: PMagickWand): clong; cdecl;
  TMagickAdaptiveThresholdImage=function (wand: PMagickWand;  const width, height: culong; const offset: clong): MagickBooleanType; cdecl;
  TMagickAddImage=function (wand: PMagickWand; const add_wand: PMagickWand): MagickBooleanType; cdecl;
  TMagickAddNoiseImage=function (wand: PMagickWand; const noise_type: NoiseType): MagickBooleanType; cdecl;
  TMagickAffineTransformImage=function (wand: PMagickWand; const drawing_wand: PDrawingWand): MagickBooleanType; cdecl;

  TMagickAnnotateImage=function (wand: PMagickWand; const drawing_wand: PDrawingWand; const x, y, angle: double; const text: PChar): MagickBooleanType; cdecl;
  TMagickAnimateImages=function (wand: PMagickWand; const server_name: PChar): MagickBooleanType; cdecl;
  TMagickBlackThresholdImage=function (wand: PMagickWand; const threshold: PPixelWand): MagickBooleanType; cdecl;
  TMagickBlurImage=function (wand: PMagickWand; const radius, sigma: double): MagickBooleanType; cdecl;
  TMagickBlurImageChannel=function (wand: PMagickWand; const channel: ChannelType; const radius, sigma: double): MagickBooleanType; cdecl;
  TMagickBorderImage=function (wand: PMagickWand; const bordercolor: PPixelWand; const width, height: culong): MagickBooleanType; cdecl;
  TMagickCharcoalImage=function (wand: PMagickWand; const radius, sigma: double): MagickBooleanType; cdecl;
  TMagickChopImage=function (wand: PMagickWand; const width, height: culong; const x, y: clong): MagickBooleanType; cdecl;
  TMagickClipImage=function (wand: PMagickWand): MagickBooleanType; cdecl;
  TMagickClipPathImage=function (wand: PMagickWand; const pathname: PChar; const inside: MagickBooleanType): MagickBooleanType; cdecl;
  TMagickColorFloodfillImage=function (wand: PMagickWand; const fill: PPixelWand; const fuzz: double; const bordercolor: PPixelWand; const x, y: clong): MagickBooleanType; cdecl;
  TMagickColorizeImage=function (wand: PMagickWand; const colorize, opacity: PPixelWand): MagickBooleanType; cdecl;
  TMagickCommentImage=function (wand: PMagickWand; const comment: PChar): MagickBooleanType; cdecl;
  TMagickCompositeImage=function (wand: PMagickWand; const composite_wand: PMagickWand; const compose: CompositeOperator; const x, y: clong): MagickBooleanType; cdecl;
  TMagickConstituteImage=function (wand: PMagickWand; const columns, rows: culong; const map: PChar; const storage: StorageType; pixels: Pointer): MagickBooleanType; cdecl;
  TMagickContrastImage=function (wand: PMagickWand; const sharpen: MagickBooleanType): MagickBooleanType; cdecl;
  TMagickConvolveImage=function (wand: PMagickWand; const order: culong; const kernel: PDouble): MagickBooleanType; cdecl;
  TMagickConvolveImageChannel=function (wand: PMagickWand; const channel: ChannelType; const order: culong; const kernel: PDouble): MagickBooleanType; cdecl;
  TMagickCropImage=function (wand: PMagickWand; const width, height: culong; const x, y: clong): MagickBooleanType; cdecl;
  TMagickCycleColormapImage=function (wand: PMagickWand; const displace: clong): MagickBooleanType; cdecl;
  TMagickDespeckleImage=function (wand: PMagickWand): MagickBooleanType; cdecl;
  TMagickDisplayImage=function (wand: PMagickWand; const server_name: PChar): MagickBooleanType; cdecl;
  TMagickDisplayImages=function (wand: PMagickWand; const server_name: PChar): MagickBooleanType; cdecl;
  TMagickDrawImage=function (wand: PMagickWand; const drawing_wand: PDrawingWand): MagickBooleanType; cdecl;
  TMagickEdgeImage=function (wand: PMagickWand; const radius: double): MagickBooleanType; cdecl;
  TMagickEmbossImage=function (wand: PMagickWand; const radius, sigma: double): MagickBooleanType; cdecl;
  TMagickEnhanceImage=function (wand: PMagickWand): MagickBooleanType; cdecl;
  TMagickEqualizeImage=function (wand: PMagickWand): MagickBooleanType; cdecl;
  TMagickEvaluateImage=function (wand: PMagickWand; const op: MagickEvaluateOperator; const constant: double): MagickBooleanType; cdecl;
  TMagickEvaluateImageChannel=function (wand: PMagickWand; const op: MagickEvaluateOperator; const constant: double): MagickBooleanType; cdecl;
  TMagickFlipImage=function (wand: PMagickWand): MagickBooleanType; cdecl;
  TMagickFlopImage=function (wand: PMagickWand): MagickBooleanType; cdecl;
  TMagickFrameImage=function (wand: PMagickWand; const matte_color: PPixelWand; const width, height: culong; const inner_bevel, outer_bevel: clong): MagickBooleanType; cdecl;
  TMagickGammaImage=function (wand: PMagickWand; const gamma: double): MagickBooleanType; cdecl;
  TMagickGammaImageChannel=function (wand: PMagickWand; const channel: ChannelType; const gamma: double): MagickBooleanType; cdecl;
  TMagickGaussianBlurImage=function (wand: PMagickWand; const radius, sigma: double): MagickBooleanType; cdecl;
  TMagickGaussianBlurImageChannel=function (wand: PMagickWand; const channel: ChannelType; const radius, sigma: double): MagickBooleanType; cdecl;
  TMagickGetImageBackgroundColor=function (wand: PMagickWand; background_color: PPixelWand): MagickBooleanType; cdecl;
  TMagickGetImageBluePrimary=function (wand: PMagickWand; x, y: Pdouble): MagickBooleanType; cdecl;
  TMagickGetImageBorderColor=function (wand: PMagickWand; border_color: PPixelWand): MagickBooleanType; cdecl;
  TMagickGetImageChannelDistortion=function (wand: PMagickWand; const reference: PMagickWand; const channel: ChannelType; const metric: MetricType; distortion: Pdouble): MagickBooleanType; cdecl;
  TMagickGetImageDistortion=function (wand: PMagickWand; const reference: PMagickWand; const metric: MetricType; distortion: Pdouble): MagickBooleanType; cdecl;
  TMagickGetImageChannelExtrema=function (wand: PMagickWand; const channel: ChannelType; minima, maxima: Pculong): MagickBooleanType; cdecl;
  TMagickGetImageChannelMean=function (wand: PMagickWand; const channel: ChannelType; mean, standard_deviation: Pdouble): MagickBooleanType; cdecl;
  TMagickGetImageColormapColor=function (wand: PMagickWand; const index: culong; color: PPixelWand): MagickBooleanType; cdecl;
  TMagickGetImageExtrema=function (wand: PMagickWand; min, max: culong): MagickBooleanType; cdecl;
  TMagickGetImageGreenPrimary=function (wand: PMagickWand; x, y: Pdouble): MagickBooleanType; cdecl;
  TMagickGetImageMatteColor=function (wand: PMagickWand; matte_color: PPixelWand): MagickBooleanType; cdecl;
  TMagickGetImagePage=function (wand: PMagickWand; width, height: Pculong; x, y: Pclong): MagickBooleanType; cdecl;
  TMagickGetImagePixelColor=function (wand: PMagickWand; const x, y: clong; color: PPixelWand): MagickBooleanType; cdecl;
  TMagickGetImagePixels=function (wand: PMagickWand; const x, y: clong; const columns, rows: culong; const map: PChar; const storage: StorageType; pixels: Pointer): MagickBooleanType; cdecl;
  TMagickGetImageRedPrimary=function (wand: PMagickWand; x, y: Pdouble): MagickBooleanType; cdecl;
  TMagickGetImageResolution=function (wand: PMagickWand; x, y: Pdouble): MagickBooleanType; cdecl;
  TMagickGetImageWhitePoint=function (wand: PMagickWand; x, y: Pdouble): MagickBooleanType; cdecl;
  TMagickHasNextImage=function (wand: PMagickWand): MagickBooleanType; cdecl;
  TMagickHasPreviousImage=function (wand: PMagickWand): MagickBooleanType; cdecl;
  TMagickImplodeImage=function (wand: PMagickWand; const radius: double): MagickBooleanType; cdecl;
  TMagickLabelImage=function (wand: PMagickWand; const _label: PChar): MagickBooleanType; cdecl;
  TMagickLevelImage=function (wand: PMagickWand; const black_point, gamma, white_point: double): MagickBooleanType; cdecl;
  TMagickLevelImageChannel=function (wand: PMagickWand; const channel: ChannelType; const black_point, gamma, white_point: double): MagickBooleanType; cdecl;
  TMagickMagnifyImage=function (wand: PMagickWand): MagickBooleanType; cdecl;
  TMagickMapImage = function (wand: PMagickWand; const map_wand: PMagickWand; const dither: MagickBooleanType): MagickBooleanType; cdecl;
  TMagickMatteFloodfillImage=function (wand: PMagickWand; const opacity: Quantum; const fuzz: double; const bordercolor: PPixelWand; const x, y: clong): MagickBooleanType; cdecl;
  TMagickMedianFilterImage=function (wand: PMagickWand; const radius: double): MagickBooleanType; cdecl;
  TMagickMinifyImage=function (wand: PMagickWand): MagickBooleanType; cdecl;
  TMagickModulateImage=function (wand: PMagickWand; const brightness, saturation, hue: double): MagickBooleanType; cdecl;
  TMagickMotionBlurImage=function (wand: PMagickWand; const radius, sigma, angle: double): MagickBooleanType; cdecl;
  TMagickNegateImage=function (wand: PMagickWand; const gray: MagickBooleanType): MagickBooleanType; cdecl;
  TMagickNewImage=function (wand: PMagickWand; const columns, rows: culong; const background: PPixelWand): MagickBooleanType; cdecl;
  TMagickNextImage=function (wand: PMagickWand): MagickBooleanType; cdecl;
  TMagickNormalizeImage=function (wand: PMagickWand): MagickBooleanType; cdecl;
  TMagickOilPaintImage=function (wand: PMagickWand; const radius: double): MagickBooleanType; cdecl;
  TMagickPaintOpaqueImage=function (wand: PMagickWand; const target, fill: PPixelWand; const fuzz: double): MagickBooleanType; cdecl;
  TMagickPaintTransparentImage=function (wand: PMagickWand; const target: PPixelWand; const opacity: Quantum; const fuzz: double): MagickBooleanType; cdecl;
  TMagickPingImage=function (wand: PMagickWand; const filename: PChar): MagickBooleanType; cdecl;
  TMagickPreviousImage=function (wand: PMagickWand): MagickBooleanType; cdecl;
  TMagickProfileImage=function (wand: PMagickWand; const name: PChar; const profile: PChar; const length: culong): MagickBooleanType; cdecl;
  TMagickQuantizeImage=function (wand: PMagickWand; const number_colors: culong; const colorspace: ColorspaceType; const treedepth: culong; const dither, measure_error): MagickBooleanType; cdecl;
  TMagickQuantizeImages=function (wand: PMagickWand; const number_colors: culong; const colorspace: ColorspaceType; const treedepth: culong; const dither: MagickBooleanType; const measure_error: MagickBooleanType): MagickBooleanType; cdecl;
  TMagickRadialBlurImage=function (wand: PMagickWand; const angle: double): MagickBooleanType; cdecl;
  TMagickRadialBlurImageChannel=function (wand: PMagickWand; const channel: ChannelType; const angle: double): MagickBooleanType; cdecl;
  TMagickRaiseImage=function (wand: PMagickWand; const width, height: culong; const x, y: clong; const raise_: MagickBooleanType): MagickBooleanType; cdecl;
  TMagickReadImage=function (wand: PMagickWand; const filename: PChar): MagickBooleanType; cdecl;
  TMagickReadImageBlob=function (wand: PMagickWand; const blob: Pointer; const length: clong): MagickBooleanType; cdecl;
  TMagickReduceNoiseImage=function (wand: PMagickWand; const radius: double): MagickBooleanType; cdecl;
  TMagickRemoveImage=function (wand: PMagickWand): MagickBooleanType; cdecl;
  TMagickResampleImage=function (wand: PMagickWand; const x_resolution, y_resolution: double; const filter: FilterTypes; const blur: double): MagickBooleanType; cdecl;
  TMagickResizeImage=function (wand: PMagickWand; const columns, rows: culong; const filter: FilterTypes; const blur: double): MagickBooleanType; cdecl;
  TMagickRollImage=function (wand: PMagickWand; const x: clong; const y: culong): MagickBooleanType; cdecl;
  TMagickRotateImage=function (wand: PMagickWand; const background: PPixelWand; const degrees: double): MagickBooleanType; cdecl;
  TMagickSampleImage=function (wand: PMagickWand; const columns, rows: culong): MagickBooleanType; cdecl;
  TMagickScaleImage=function (wand: PMagickWand; const columns, rows: culong): MagickBooleanType; cdecl;
  TMagickSeparateImageChannel=function (wand: PMagickWand; const channel: ChannelType): MagickBooleanType; cdecl;
  TMagickSetImage=function (wand: PMagickWand; const set_wand: PMagickWand): MagickBooleanType; cdecl;
  TMagickSetImageAttribute=function (wand: PMagickWand; const key, attribute: PChar): MagickBooleanType; cdecl;
  TMagickSetImageBackgroundColor=function (wand: PMagickWand; const background: PPixelWand): MagickBooleanType; cdecl;
  TMagickSetImageBias=function (wand: PMagickWand; const bias: double): MagickBooleanType; cdecl;
  TMagickSetImageBluePrimary=function (wand: PMagickWand; const x, y: double): MagickBooleanType; cdecl;
  TMagickSetImageBorderColor=function (wand: PMagickWand; const border: PPixelWand): MagickBooleanType; cdecl;
  TMagickSetImageChannelDepth=function (wand: PMagickWand; const channel: ChannelType; const depth: culong): MagickBooleanType; cdecl;
  TMagickSetImageColormapColor=function (wand: PMagickWand; const index: culong; const color: PPixelWand): MagickBooleanType; cdecl;
  TMagickSetImageColorspace=function (wand: PMagickWand; const colorspace: ColorspaceType): MagickBooleanType; cdecl;
  TMagickSetImageCompose=function (wand: PMagickWand; const compose: CompositeOperator): MagickBooleanType; cdecl;
  TMagickSetImageCompression=function (wand: PMagickWand; const compression: CompressionType): MagickBooleanType; cdecl;
  TMagickSetImageCompressionQuality=function (wand: PMagickWand; const quality: culong): MagickBooleanType; cdecl;
  TMagickSetImageDelay=function (wand: PMagickWand; const delay: culong): MagickBooleanType; cdecl;
  TMagickSetImageDepth=function (wand: PMagickWand; const depth: culong): MagickBooleanType; cdecl;
  TMagickSetImageDispose=function (wand: PMagickWand; const dispose: DisposeType): MagickBooleanType; cdecl;
  TMagickSetImageExtent=function (wand: PMagickWand; const columns, rows: culong): MagickBooleanType; cdecl;
  TMagickSetImageFilename=function (wand: PMagickWand; const filename: PChar): MagickBooleanType; cdecl;
  TMagickSetImageFormat=function (wand: PMagickWand; const format: PChar): MagickBooleanType; cdecl;
  TMagickSetImageGamma=function (wand: PMagickWand; const gamma: Double): MagickBooleanType; cdecl;
  TMagickSetImageGreenPrimary=function (wand: PMagickWand; const x, y: Double): MagickBooleanType; cdecl;
  TMagickSetImageIndex=function (wand: PMagickWand; const index: clong): MagickBooleanType; cdecl;
  TMagickSetImageInterlaceScheme=function (wand: PMagickWand; const interlace_scheme: InterlaceType): MagickBooleanType; cdecl;
  TMagickSetImageIterations=function (wand: PMagickWand; const iterations: culong): MagickBooleanType; cdecl;
  TMagickSetImageMatteColor=function (wand: PMagickWand; const matte: PPixelWand): MagickBooleanType; cdecl;
  TMagickSetImagePage=function (wand: PMagickWand; const width, height: culong; const x, y: clong): MagickBooleanType; cdecl;
  TMagickSetImagePixels=function (wand: PMagickWand; const x, y: clong; const columns, rows: culong; const map: PChar; const storage: StorageType; const pixels: Pointer): MagickBooleanType; cdecl;
  TMagickSetImageProfile=function (wand: PMagickWand; const name: PChar; const profile: Pointer; const length: culong): MagickBooleanType; cdecl;
  TMagickSetImageRedPrimary=function (wand: PMagickWand; const x, y: Double): MagickBooleanType; cdecl;
  TMagickSetImageRenderingIntent=function (wand: PMagickWand; const rendering_intent: RenderingIntent ): MagickBooleanType; cdecl;
  TMagickSetImageResolution=function (wand: PMagickWand; const x_resolution, y_resolution: double ): MagickBooleanType; cdecl;
  TMagickSetImageScene=function (wand: PMagickWand; const scene: culong): MagickBooleanType; cdecl;
  TMagickSetImageType=function (wand: PMagickWand; const image_type: ImageType ): MagickBooleanType; cdecl;
  TMagickSetImageUnits=function (wand: PMagickWand; const units: ResolutionType ): MagickBooleanType; cdecl;
  TMagickSetImageWhitePoint=function (wand: PMagickWand; const x, y: double ): MagickBooleanType; cdecl;
  TMagickShadowImage=function (wand: PMagickWand; const radius, sigma: double; const x, y: clong ): MagickBooleanType; cdecl;
  TMagickSharpenImage=function (wand: PMagickWand; const radius, sigma: double ): MagickBooleanType; cdecl;
  TMagickSharpenImageChannel=function (wand: PMagickWand; const channel: ChannelType; const radius, sigma: double): MagickBooleanType; cdecl;
  TMagickShaveImage=function (wand: PMagickWand; const columns, rows: culong ): MagickBooleanType; cdecl;
  TMagickShearImage=function (wand: PMagickWand; const background: PPixelWand; const x_shear, y_shear: double): MagickBooleanType; cdecl;
  TMagickSigmoidalContrastImage=function (wand: PMagickWand; const sharpen: MagickBooleanType; const alpha, beta: double): MagickBooleanType; cdecl;
  TMagickSigmoidalContrastImageChannel=function (wand: PMagickWand; const channel: ChannelType; const sharpen: MagickBooleanType; const alpha, beta: double): MagickBooleanType; cdecl;
  TMagickSolarizeImage=function (wand: PMagickWand; const threshold: double): MagickBooleanType; cdecl;
  TMagickSpliceImage=function (wand: PMagickWand; const width, height: culong; const x, y: clong): MagickBooleanType; cdecl;
  TMagickSpreadImage=function (wand: PMagickWand; const radius: double): MagickBooleanType; cdecl;
  TMagickStripImage=function (wand: PMagickWand): MagickBooleanType; cdecl;
  TMagickSwirlImage=function (wand: PMagickWand; const degrees: double): MagickBooleanType; cdecl;
  TMagickTintImage=function (wand: PMagickWand; const tint: PPixelWand; const opacity: PPixelWand): MagickBooleanType; cdecl;
  TMagickThresholdImage=function (wand: PMagickWand; const threshold: double): MagickBooleanType; cdecl;
  TMagickThresholdImageChannel=function (wand: PMagickWand; const channel: ChannelType; const threshold: double): MagickBooleanType; cdecl;
  TMagickTrimImage=function (wand: PMagickWand; const fuzz: double): MagickBooleanType; cdecl;
  TMagickUnsharpMaskImage=function (wand: PMagickWand; const radius, sigma, amount, threshold: double): MagickBooleanType; cdecl;
  TMagickUnsharpMaskImageChannel=function (wand: PMagickWand; const channel: ChannelType; const radius, sigma, amount, threshold: double): MagickBooleanType; cdecl;
  TMagickWaveImage=function (wand: PMagickWand; const amplitude, wave_length: double): MagickBooleanType; cdecl;
  TMagickWhiteThresholdImage=function (wand: PMagickWand; const threshold: PPixelWand): MagickBooleanType; cdecl;
  TMagickWriteImage=function (wand: PMagickWand; const filename: PChar): MagickBooleanType; cdecl;
  TMagickWriteImages=function (wand: PMagickWand; const filename: PChar; const adjoin: MagickBooleanType): MagickBooleanType; cdecl;
  TMagickSetImageProgressMonitor=function (wand: PMagickWand; const progress_monitor: MagickProgressMonitor; client_data: Pointer): MagickProgressMonitor; cdecl;
  TMagickGetImageSize=function (wand: PMagickWand): MagickSizeType; cdecl;
  TMagickAppendImages=function (wand: PMagickWand; const stack: MagickBooleanType): PMagickWand; cdecl;
  TMagickAverageImages=function (wand: PMagickWand): PMagickWand; cdecl;
  TMagickCoalesceImages=function (wand: PMagickWand): PMagickWand; cdecl;
  TMagickCombineImages=function (wand: PMagickWand; para2: ChannelType): PMagickWand; cdecl;
  TMagickCompareImageChannels=function (wand: PMagickWand; const reference: PMagickWand; const channel: ChannelType; const metric: MetricType; distortion: PDouble): PMagickWand; cdecl;
  TMagickCompareImages=function (wand: PMagickWand; const reference: PMagickWand; const metric: MetricType; distortion: PDouble): PMagickWand; cdecl;
  TMagickDeconstructImages=function (wand: PMagickWand): PMagickWand; cdecl;
  TMagickFlattenImages=function (wand: PMagickWand): PMagickWand; cdecl;
  TMagickFxImage=function (wand: PMagickWand; const expression: PChar): PMagickWand; cdecl;
  TMagickFxImageChannel=function (wand: PMagickWand; const channel: ChannelType; const expression: PChar): PMagickWand; cdecl;
  TMagickGetImage=function (wand: PMagickWand): PMagickWand; cdecl;
  TMagickGetImageRegion=function (wand: PMagickWand; const width, height: culong; const x, y: clong): PMagickWand; cdecl;
  TMagickMorphImages=function (wand: PMagickWand; const number_frames: culong): PMagickWand; cdecl;
  TMagickMosaicImages=function (wand: PMagickWand): PMagickWand; cdecl;
  TMagickPreviewImages=function (wand: PMagickWand; const preview: PreviewType): PMagickWand; cdecl;
  TMagickSteganoImage=function (wand: PMagickWand; const watermark_wand: PMagickWand;const offset: clong): PMagickWand; cdecl;
  TMagickStereoImage=function (wand: PMagickWand; const offset_wand: PMagickWand): PMagickWand; cdecl;
  TMagickTextureImage=function (wand: PMagickWand; const texture_wand: PMagickWand): PMagickWand; cdecl;
  TMagickTransformImage=function (wand: PMagickWand; const crop, geometry: PChar): PMagickWand; cdecl;
  TNewMagickWandFromImage=function (para1: PImage): PMagickWand; cdecl;

  TMagickGetImageHistogram=function (wand: PMagickWand; number_colors: Pculong): PPPixelWand; cdecl;

  TMagickGetImageRenderingIntent=function (wand: PMagickWand): RenderingIntent; cdecl;

  TMagickGetImageUnits=function (wand: PMagickWand): ResolutionType; cdecl;

  TMagickGetImageBlob=function (wand: PMagickWand; length: Pcsize_t): PByte; cdecl;
  TMagickGetImagesBlob=function (wand: PMagickWand; length: Pcsize_t): PByte; cdecl;
  TMagickGetImageProfile=function (wand: PMagickWand; name: PChar; length: Pcsize_t): PByte; cdecl;
  TMagickRemoveImageProfile=function (wand: PMagickWand; name: PChar; length: Pcsize_t): PByte; cdecl;

  TMagickGetImageColors=function (wand: PMagickWand): culong; cdecl;
  TMagickGetImageCompressionQuality=function (wand: PMagickWand): culong; cdecl;
  TMagickGetImageDelay=function (wand: PMagickWand): culong; cdecl;
  TMagickGetImageChannelDepth=function (wand: PMagickWand; const channel: ChannelType): culong; cdecl;
  TMagickGetImageDepth=function (wand: PMagickWand): culong; cdecl;
  TMagickGetImageHeight=function (wand: PMagickWand): culong; cdecl;
  TMagickGetImageIterations=function (wand: PMagickWand): culong; cdecl;
  TMagickGetImageScene=function (wand: PMagickWand): culong; cdecl;
  TMagickGetImageWidth=function (wand: PMagickWand): culong; cdecl;
  TMagickGetNumberImages=function (wand: PMagickWand): culong; cdecl;

var
  MagickGetImageChannelStatistics:TMagickGetImageChannelStatistics=nil;
  MagickGetImageAttribute:TMagickGetImageAttribute=nil;
  MagickGetImageFilename:TMagickGetImageFilename=nil;
  MagickGetImageFormat:TMagickGetImageFormat=nil;
  MagickGetImageSignature:TMagickGetImageSignature=nil;
  MagickIdentifyImage:TMagickIdentifyImage=nil;
  MagickGetImageCompose:TMagickGetImageCompose=nil;
  MagickGetImageColorspace:TMagickGetImageColorspace=nil;
  MagickGetImageCompression:TMagickGetImageCompression=nil;
  MagickGetImageDispose:TMagickGetImageDispose=nil;
  MagickGetImageGamma:TMagickGetImageGamma=nil;
  MagickGetImageTotalInkDensity:TMagickGetImageTotalInkDensity=nil;
  GetImageFromMagickWand:TGetImageFromMagickWand=nil;
  MagickGetImageType:TMagickGetImageType=nil;
  MagickGetImageInterlaceScheme:TMagickGetImageInterlaceScheme=nil;
  MagickGetImageIndex:TMagickGetImageIndex=nil;
  MagickAdaptiveThresholdImage:TMagickAdaptiveThresholdImage=nil;
  MagickAddImage:TMagickAddImage=nil;
  MagickAddNoiseImage:TMagickAddNoiseImage=nil;
  MagickAffineTransformImage:TMagickAffineTransformImage=nil;
  MagickAnnotateImage:TMagickAnnotateImage=nil;
  MagickAnimateImages:TMagickAnimateImages=nil;
  MagickBlackThresholdImage:TMagickBlackThresholdImage=nil;
  MagickBlurImage:TMagickBlurImage=nil;
  MagickBlurImageChannel:TMagickBlurImageChannel=nil;
  MagickBorderImage:TMagickBorderImage=nil;
  MagickCharcoalImage:TMagickCharcoalImage=nil;
  MagickChopImage:TMagickChopImage=nil;
  MagickClipImage:TMagickClipImage=nil;
  MagickClipPathImage:TMagickClipPathImage=nil;
  MagickColorFloodfillImage:TMagickColorFloodfillImage=nil;
  MagickColorizeImage:TMagickColorizeImage=nil;
  MagickCommentImage:TMagickCommentImage=nil;
  MagickCompositeImage:TMagickCompositeImage=nil;
  MagickConstituteImage:TMagickConstituteImage=nil;
  MagickContrastImage:TMagickContrastImage=nil;
  MagickConvolveImage:TMagickConvolveImage=nil;
  MagickConvolveImageChannel:TMagickConvolveImageChannel=nil;
  MagickCropImage:TMagickCropImage=nil;
  MagickCycleColormapImage:TMagickCycleColormapImage=nil;
  MagickDespeckleImage:TMagickDespeckleImage=nil;
  MagickDisplayImage:TMagickDisplayImage=nil;
  MagickDisplayImages:TMagickDisplayImage=nil;
  MagickDrawImage:TMagickDrawImage=nil;
  MagickEdgeImage:TMagickEdgeImage=nil;
  MagickEmbossImage:TMagickEmbossImage=nil;
  MagickEnhanceImage:TMagickEnhanceImage=nil;
  MagickEqualizeImage:TMagickEqualizeImage=nil;
  MagickEvaluateImage:TMagickEvaluateImage=nil;
  MagickEvaluateImageChannel:TMagickEvaluateImageChannel=nil;
  MagickFlipImage:TMagickFlipImage=nil;
  MagickFlopImage:TMagickFlopImage=nil;
  MagickFrameImage:TMagickFrameImage=nil;
  MagickGammaImage:TMagickGammaImage=nil;
  MagickGammaImageChannel:TMagickGammaImageChannel=nil;
  MagickGaussianBlurImage:TMagickGaussianBlurImage=nil;
  MagickGaussianBlurImageChannel:TMagickGaussianBlurImageChannel=nil;
  MagickGetImageBackgroundColor:TMagickGetImageBackgroundColor=nil;
  MagickGetImageBluePrimary:TMagickGetImageBluePrimary=nil;
  MagickGetImageBorderColor:TMagickGetImageBorderColor=nil;
  MagickGetImageChannelDistortion:TMagickGetImageChannelDistortion=nil;
  MagickGetImageDistortion:TMagickGetImageDistortion=nil;
  MagickGetImageChannelExtrema:TMagickGetImageChannelExtrema=nil;
  MagickGetImageChannelMean:TMagickGetImageChannelMean=nil;
  MagickGetImageColormapColor:TMagickGetImageColormapColor=nil;
  MagickGetImageExtrema:TMagickGetImageExtrema=nil;
  MagickGetImageGreenPrimary:TMagickGetImageGreenPrimary=nil;
  MagickGetImageMatteColor:TMagickGetImageMatteColor=nil;
  MagickGetImagePage:TMagickGetImagePage=nil;
  MagickGetImagePixelColor:TMagickGetImagePixelColor=nil;
  MagickGetImagePixels:TMagickGetImagePixels=nil;
  MagickGetImageRedPrimary:TMagickGetImageRedPrimary=nil;
  MagickGetImageResolution:TMagickGetImageResolution=nil;
  MagickGetImageWhitePoint:TMagickGetImageWhitePoint=nil;
  MagickHasNextImage:TMagickHasNextImage=nil;
  MagickHasPreviousImage:TMagickHasPreviousImage=nil;
  MagickImplodeImage:TMagickImplodeImage=nil;
  MagickLabelImage:TMagickLabelImage=nil;
  MagickLevelImage:TMagickLevelImage=nil;
  MagickLevelImageChannel:TMagickLevelImageChannel=nil;
  MagickMagnifyImage:TMagickMagnifyImage=nil;
  MagickMapImage:TMagickMapImage=nil;
  MagickMatteFloodfillImage:TMagickMatteFloodfillImage=nil;
  MagickMedianFilterImage:TMagickMedianFilterImage=nil;
  MagickMinifyImage:TMagickMinifyImage=nil;
  MagickModulateImage:TMagickModulateImage=nil;
  MagickMotionBlurImage:TMagickMotionBlurImage=nil;
  MagickNegateImage:TMagickNegateImage=nil;
  MagickNewImage:TMagickNewImage=nil;
  MagickNextImage:TMagickNextImage=nil;
  MagickNormalizeImage:TMagickNormalizeImage=nil;
  MagickOilPaintImage:TMagickOilPaintImage=nil;
  MagickPaintOpaqueImage:TMagickPaintOpaqueImage=nil;
  MagickPaintTransparentImage:TMagickPaintTransparentImage=nil;
  MagickPingImage:TMagickPingImage=nil;
  MagickPreviousImage:TMagickPreviousImage=nil;
  MagickProfileImage:TMagickProfileImage=nil;
  MagickQuantizeImage:TMagickQuantizeImage=nil;
  MagickQuantizeImages:TMagickQuantizeImages=nil;
  MagickRadialBlurImage:TMagickRadialBlurImage=nil;
  MagickRadialBlurImageChannel:TMagickRadialBlurImageChannel=nil;
  MagickRaiseImage:TMagickRaiseImage=nil;
  MagickReadImage:TMagickReadImage=nil;
  MagickReadImageBlob:TMagickReadImageBlob=nil;
  MagickReduceNoiseImage:TMagickReduceNoiseImage=nil;
  MagickRemoveImage:TMagickRemoveImage=nil;
  MagickResampleImage:TMagickResampleImage=nil;
  MagickResizeImage:TMagickResizeImage=nil;
  MagickRollImage:TMagickRollImage=nil;
  MagickRotateImage:TMagickRotateImage=nil;
  MagickSampleImage:TMagickSampleImage=nil;
  MagickScaleImage:TMagickScaleImage=nil;
  MagickSeparateImageChannel:TMagickSeparateImageChannel=nil;
  MagickSetImage:TMagickSetImage=nil;
  MagickSetImageAttribute:TMagickSetImageAttribute=nil;
  MagickSetImageBackgroundColor:TMagickSetImageBackgroundColor=nil;
  MagickSetImageBias:TMagickSetImageBias=nil;
  MagickSetImageBluePrimary:TMagickSetImageBluePrimary=nil;
  MagickSetImageBorderColor:TMagickSetImageBorderColor=nil;
  MagickSetImageChannelDepth:TMagickSetImageChannelDepth=nil;
  MagickSetImageColormapColor:TMagickSetImageColormapColor=nil;
  MagickSetImageColorspace:TMagickSetImageColorspace=nil;
  MagickSetImageCompose:TMagickSetImageCompose=nil;
  MagickSetImageCompression:TMagickSetImageCompression=nil;
  MagickSetImageCompressionQuality:TMagickSetImageCompressionQuality=nil;
  MagickSetImageDelay:TMagickSetImageDelay=nil;
  MagickSetImageDepth:TMagickSetImageDepth=nil;
  MagickSetImageDispose:TMagickSetImageDispose=nil;
  MagickSetImageExtent:TMagickSetImageExtent=nil;
  MagickSetImageFilename:TMagickSetImageFilename=nil;
  MagickSetImageFormat:TMagickSetImageFormat=nil;
  MagickSetImageGamma:TMagickSetImageGamma=nil;
  MagickSetImageGreenPrimary:TMagickSetImageGreenPrimary=nil;
  MagickSetImageIndex:TMagickSetImageIndex=nil;
  MagickSetImageInterlaceScheme:TMagickSetImageInterlaceScheme=nil;
  MagickSetImageIterations:TMagickSetImageIterations=nil;
  MagickSetImageMatteColor:TMagickSetImageMatteColor=nil;
  MagickSetImagePage:TMagickSetImagePage=nil;
  MagickSetImagePixels:TMagickSetImagePixels=nil;
  MagickSetImageProfile:TMagickSetImageProfile=nil;
  MagickSetImageRedPrimary:TMagickSetImageRedPrimary=nil;
  MagickSetImageRenderingIntent:TMagickSetImageRenderingIntent=nil;
  MagickSetImageResolution:TMagickSetImageResolution=nil;
  MagickSetImageScene:TMagickSetImageScene=nil;
  MagickSetImageType:TMagickSetImageType=nil;
  MagickSetImageUnits:TMagickSetImageUnits=nil;
  MagickSetImageWhitePoint:TMagickSetImageWhitePoint=nil;
  MagickShadowImage:TMagickShadowImage=nil;
  MagickSharpenImage:TMagickSharpenImage=nil;
  MagickSharpenImageChannel:TMagickSharpenImageChannel=nil;
  MagickShaveImage:TMagickShaveImage=nil;
  MagickShearImage:TMagickShearImage=nil;
  MagickSigmoidalContrastImage:TMagickSigmoidalContrastImage=nil;
  MagickSigmoidalContrastImageChannel:TMagickSigmoidalContrastImageChannel=nil;
  MagickSolarizeImage:TMagickSolarizeImage=nil;
  MagickSpliceImage:TMagickSpliceImage=nil;
  MagickSpreadImage:TMagickSpreadImage=nil;
  MagickStripImage:TMagickStripImage=nil;
  MagickSwirlImage:TMagickSwirlImage=nil;
  MagickTintImage:TMagickTintImage=nil;
  MagickThresholdImage:TMagickThresholdImage=nil;
  MagickThresholdImageChannel:TMagickThresholdImageChannel=nil;
  MagickTrimImage:TMagickTrimImage=nil;
  MagickUnsharpMaskImage:TMagickUnsharpMaskImage=nil;
  MagickUnsharpMaskImageChannel:TMagickUnsharpMaskImageChannel=nil;
  MagickWaveImage:TMagickWaveImage=nil;
  MagickWhiteThresholdImage:TMagickWhiteThresholdImage=nil;
  MagickWriteImage:TMagickWriteImage=nil;
  MagickWriteImages:TMagickWriteImages=nil;
  MagickSetImageProgressMonitor:TMagickSetImageProgressMonitor=nil;
  MagickGetImageSize:TMagickGetImageSize=nil;
  MagickAppendImages:TMagickAppendImages=nil;
  MagickAverageImages:TMagickAverageImages=nil;
  MagickCoalesceImages:TMagickCoalesceImages=nil;
  MagickCombineImages:TMagickCombineImages=nil;
  MagickCompareImageChannels:TMagickCompareImageChannels=nil;
  MagickCompareImages:TMagickCompareImages=nil;
  MagickDeconstructImages:TMagickDeconstructImages=nil;
  MagickFlattenImages:TMagickFlattenImages=nil;
  MagickFxImage:TMagickFxImage=nil;
  MagickFxImageChannel:TMagickFxImageChannel=nil;
  MagickGetImage:TMagickGetImage=nil;
  MagickGetImageRegion:TMagickGetImageRegion=nil;
  MagickMorphImages:TMagickMorphImages=nil;
  MagickMosaicImages:TMagickMosaicImages=nil;
  MagickPreviewImages:TMagickPreviewImages=nil;
  MagickSteganoImage:TMagickSteganoImage=nil;
  MagickStereoImage:TMagickStereoImage=nil;
  MagickTextureImage:TMagickTextureImage=nil;
  MagickTransformImage:TMagickTransformImage=nil;
  NewMagickWandFromImage:TNewMagickWandFromImage=nil;

  MagickGetImageHistogram:TMagickGetImageHistogram=nil;

  MagickGetImageRenderingIntent:TMagickGetImageRenderingIntent=nil;

  MagickGetImageUnits:TMagickGetImageUnits=nil;

  MagickGetImageBlob:TMagickGetImageBlob=nil;
  MagickGetImagesBlob:TMagickGetImagesBlob=nil;
  MagickGetImageProfile:TMagickGetImageProfile=nil;
  MagickRemoveImageProfile:TMagickRemoveImageProfile=nil;

  MagickGetImageColors:TMagickGetImageColors=nil;
  MagickGetImageCompressionQuality:TMagickGetImageCompressionQuality=nil;
  MagickGetImageDelay:TMagickGetImageDelay=nil;
  MagickGetImageChannelDepth:TMagickGetImageChannelDepth=nil;
  MagickGetImageDepth:TMagickGetImageDepth=nil;
  MagickGetImageHeight:TMagickGetImageHeight=nil;
  MagickGetImageIterations:TMagickGetImageIterations=nil;
  MagickGetImageScene:TMagickGetImageScene=nil;
  MagickGetImageWidth:TMagickGetImageWidth=nil;
  MagickGetNumberImages:TMagickGetNumberImages=nil;


Type
  TIsMagickWand=function(const wand: PMagickWand): MagickBooleanType; cdecl;
  TMagickClearException=function (wand: PMagickWand): MagickBooleanType; cdecl;
  TCloneMagickWand=function (const wand: PMagickWand): PMagickWand; cdecl;
  TDestroyMagickWand=function (wand: PMagickWand): PMagickWand; cdecl;
  TNewMagickWand=function : PMagickWand; cdecl;

  TClearMagickWand=procedure (wand: PMagickWand); cdecl;
  TMagickWandGenesis=procedure ; cdecl;
  TMagickWandTerminus=procedure ; cdecl;
  TMagickRelinquishMemory=function  (resource: Pointer): Pointer; cdecl;
  TMagickResetIterator=procedure (wand: PMagickWand); cdecl;
  TMagickSetFirstIterator=procedure (wand: PMagickWand); cdecl;
  TMagickSetLastIterator=procedure (wand: PMagickWand); cdecl;

Var
  IsMagickWand:TIsMagickWand=nil;
  MagickClearException:TMagickClearException=nil;
  CloneMagickWand:TCloneMagickWand=nil;
  DestroyMagickWand:TDestroyMagickWand=nil;
  NewMagickWand:TNewMagickWand=nil;

  ClearMagickWand:TClearMagickWand=nil;
  MagickWandGenesis:TMagickWandGenesis=nil;
  MagickWandTerminus:TMagickWandTerminus=nil;
  MagickRelinquishMemory:TMagickRelinquishMemory=nil;
  MagickResetIterator:TMagickResetIterator=nil;
  MagickSetFirstIterator:TMagickSetFirstIterator=nil;
  MagickSetLastIterator:TMagickSetLastIterator=nil;

  function Initialize(LibWand:TLibHandle):boolean;
  function Finalize(LibWand:TLibHandle):boolean;

implementation

function Initialize(LibWand:TLibHandle):boolean;
begin
  Result:=True;

  Pointer(IsMagickWand):=GetProcAddress(LibWand, 'IsMagickWand');
  Pointer(MagickClearException):=GetProcAddress(LibWand, 'MagickClearException');
  Pointer(CloneMagickWand):=GetProcAddress(LibWand, 'CloneMagickWand');
  Pointer(DestroyMagickWand):=GetProcAddress(LibWand, 'DestroyMagickWand');
  Pointer(NewMagickWand):=GetProcAddress(LibWand, 'NewMagickWand');

  Pointer(ClearMagickWand):=GetProcAddress(LibWand, 'ClearMagickWand');
  Pointer(MagickWandGenesis):=GetProcAddress(LibWand, 'MagickWandGenesis');
  Pointer(MagickWandTerminus):=GetProcAddress(LibWand, 'MagickWandTerminus');
  Pointer(MagickRelinquishMemory):=GetProcAddress(LibWand, 'MagickRelinquishMemory');
  Pointer(MagickResetIterator):=GetProcAddress(LibWand, 'MagickResetIterator');
  Pointer(MagickSetFirstIterator):=GetProcAddress(LibWand, 'MagickSetFirstIterator');
  Pointer(MagickSetLastIterator):=GetProcAddress(LibWand, 'MagickSetLastIterator');


  Pointer(MagickGetImageChannelStatistics):=GetProcAddress(LibWand, 'MagickGetImageChannelStatistics');
  Pointer(MagickGetImageAttribute):=GetProcAddress(LibWand, 'MagickGetImageAttribute');
  Pointer(MagickGetImageFilename):=GetProcAddress(LibWand, 'MagickGetImageFilename');
  Pointer(MagickGetImageFormat):=GetProcAddress(LibWand, 'MagickGetImageFormat');
  Pointer(MagickGetImageSignature):=GetProcAddress(LibWand, 'MagickGetImageSignature');
  Pointer(MagickIdentifyImage):=GetProcAddress(LibWand, 'MagickIdentifyImage');
  Pointer(MagickGetImageCompose):=GetProcAddress(LibWand, 'MagickGetImageCompose');
  Pointer(MagickGetImageColorspace):=GetProcAddress(LibWand, 'MagickGetImageColorspace');
  Pointer(MagickGetImageCompression):=GetProcAddress(LibWand, 'MagickGetImageCompression');
  Pointer(MagickGetImageDispose):=GetProcAddress(LibWand, 'MagickGetImageDispose');
  Pointer(MagickGetImageGamma):=GetProcAddress(LibWand, 'MagickGetImageGamma');
  Pointer(MagickGetImageTotalInkDensity):=GetProcAddress(LibWand, 'MagickGetImageTotalInkDensity');
  Pointer(GetImageFromMagickWand):=GetProcAddress(LibWand, 'GetImageFromMagickWand');
  Pointer(MagickGetImageType):=GetProcAddress(LibWand, 'MagickGetImageType');
  Pointer(MagickGetImageInterlaceScheme):=GetProcAddress(LibWand, 'MagickGetImageInterlaceScheme');
  Pointer(MagickGetImageIndex):=GetProcAddress(LibWand, 'MagickGetImageIndex');
  Pointer(MagickAdaptiveThresholdImage):=GetProcAddress(LibWand, 'MagickAdaptiveThresholdImage');
  Pointer(MagickAddImage):=GetProcAddress(LibWand, 'MagickAddImage');
  Pointer(MagickAddNoiseImage):=GetProcAddress(LibWand, 'MagickAddNoiseImage');
  Pointer(MagickAffineTransformImage):=GetProcAddress(LibWand, 'MagickAffineTransformImage');
  Pointer(MagickAnnotateImage):=GetProcAddress(LibWand, 'MagickAnnotateImage');
  Pointer(MagickAnimateImages):=GetProcAddress(LibWand, 'MagickAnimateImages');
  Pointer(MagickBlackThresholdImage):=GetProcAddress(LibWand, 'MagickBlackThresholdImage');
  Pointer(MagickBlurImage):=GetProcAddress(LibWand, 'MagickBlurImage');
  Pointer(MagickBlurImageChannel):=GetProcAddress(LibWand, 'MagickBlurImageChannel');
  Pointer(MagickBorderImage):=GetProcAddress(LibWand, 'MagickBorderImage');
  Pointer(MagickCharcoalImage):=GetProcAddress(LibWand, 'MagickCharcoalImage');
  Pointer(MagickChopImage):=GetProcAddress(LibWand, 'MagickChopImage');
  Pointer(MagickClipImage):=GetProcAddress(LibWand, 'MagickClipImage');
  Pointer(MagickClipPathImage):=GetProcAddress(LibWand, 'MagickClipPathImage');
  Pointer(MagickColorFloodfillImage):=GetProcAddress(LibWand, 'MagickColorFloodfillImage');
  Pointer(MagickColorizeImage):=GetProcAddress(LibWand, 'MagickColorizeImage');
  Pointer(MagickCommentImage):=GetProcAddress(LibWand, 'MagickCommentImage');
  Pointer(MagickCompositeImage):=GetProcAddress(LibWand, 'MagickCompositeImage');
  Pointer(MagickConstituteImage):=GetProcAddress(LibWand, 'MagickConstituteImage');
  Pointer(MagickContrastImage):=GetProcAddress(LibWand, 'MagickContrastImage');
  Pointer(MagickConvolveImage):=GetProcAddress(LibWand, 'MagickConvolveImage');
  Pointer(MagickConvolveImageChannel):=GetProcAddress(LibWand, 'MagickConvolveImageChannel');
  Pointer(MagickCropImage):=GetProcAddress(LibWand, 'MagickCropImage');
  Pointer(MagickCycleColormapImage):=GetProcAddress(LibWand, 'MagickCycleColormapImage');
  Pointer(MagickDespeckleImage):=GetProcAddress(LibWand, 'MagickDespeckleImage');
  Pointer(MagickDisplayImage):=GetProcAddress(LibWand, 'MagickDisplayImage');
  Pointer(MagickDisplayImages):=GetProcAddress(LibWand, 'MagickDisplayImages');
  Pointer(MagickDrawImage):=GetProcAddress(LibWand, 'MagickDrawImage');
  Pointer(MagickEdgeImage):=GetProcAddress(LibWand, 'MagickEdgeImage');
  Pointer(MagickEmbossImage):=GetProcAddress(LibWand, 'MagickEmbossImage');
  Pointer(MagickEnhanceImage):=GetProcAddress(LibWand, 'MagickEnhanceImage');
  Pointer(MagickEqualizeImage):=GetProcAddress(LibWand, 'MagickEqualizeImage');
  Pointer(MagickEvaluateImage):=GetProcAddress(LibWand, 'MagickEvaluateImage');
  Pointer(MagickEvaluateImageChannel):=GetProcAddress(LibWand, 'MagickEvaluateImageChannel');
  Pointer(MagickFlipImage):=GetProcAddress(LibWand, 'MagickFlipImage');
  Pointer(MagickFlopImage):=GetProcAddress(LibWand, 'MagickFlopImage');
  Pointer(MagickFrameImage):=GetProcAddress(LibWand, 'MagickFrameImage');
  Pointer(MagickGammaImage):=GetProcAddress(LibWand, 'MagickGammaImage');
  Pointer(MagickGammaImageChannel):=GetProcAddress(LibWand, 'MagickGammaImageChannel');
  Pointer(MagickGaussianBlurImage):=GetProcAddress(LibWand, 'MagickGaussianBlurImage');
  Pointer(MagickGaussianBlurImageChannel):=GetProcAddress(LibWand, 'MagickGaussianBlurImageChannel');
  Pointer(MagickGetImageBackgroundColor):=GetProcAddress(LibWand, 'MagickGetImageBackgroundColor');
  Pointer(MagickGetImageBluePrimary):=GetProcAddress(LibWand, 'MagickGetImageBluePrimary');
  Pointer(MagickGetImageBorderColor):=GetProcAddress(LibWand, 'MagickGetImageBorderColor');
  Pointer(MagickGetImageChannelDistortion):=GetProcAddress(LibWand, 'MagickGetImageChannelDistortion');
  Pointer(MagickGetImageDistortion):=GetProcAddress(LibWand, 'MagickGetImageDistortion');
  Pointer(MagickGetImageChannelExtrema):=GetProcAddress(LibWand, 'MagickGetImageChannelExtrema');
  Pointer(MagickGetImageChannelMean):=GetProcAddress(LibWand, 'MagickGetImageChannelMean');
  Pointer(MagickGetImageColormapColor):=GetProcAddress(LibWand, 'MagickGetImageColormapColor');
  Pointer(MagickGetImageExtrema):=GetProcAddress(LibWand, 'MagickGetImageExtrema');
  Pointer(MagickGetImageGreenPrimary):=GetProcAddress(LibWand, 'MagickGetImageGreenPrimary');
  Pointer(MagickGetImageMatteColor):=GetProcAddress(LibWand, 'MagickGetImageMatteColor');
  Pointer(MagickGetImagePage):=GetProcAddress(LibWand, 'MagickGetImagePage');
  Pointer(MagickGetImagePixelColor):=GetProcAddress(LibWand, 'MagickGetImagePixelColor');
  Pointer(MagickGetImagePixels):=GetProcAddress(LibWand, 'MagickGetImagePixels');
  Pointer(MagickGetImageRedPrimary):=GetProcAddress(LibWand, 'MagickGetImageRedPrimary');
  Pointer(MagickGetImageResolution):=GetProcAddress(LibWand, 'MagickGetImageResolution');
  Pointer(MagickGetImageWhitePoint):=GetProcAddress(LibWand, 'MagickGetImageWhitePoint');
  Pointer(MagickHasNextImage):=GetProcAddress(LibWand, 'MagickHasNextImage');
  Pointer(MagickHasPreviousImage):=GetProcAddress(LibWand, 'MagickHasPreviousImage');
  Pointer(MagickImplodeImage):=GetProcAddress(LibWand, 'MagickImplodeImage');
  Pointer(MagickLabelImage):=GetProcAddress(LibWand, 'MagickLabelImage');
  Pointer(MagickLevelImage):=GetProcAddress(LibWand, 'MagickLevelImage');
  Pointer(MagickLevelImageChannel):=GetProcAddress(LibWand, 'MagickLevelImageChannel');
  Pointer(MagickMagnifyImage):=GetProcAddress(LibWand, 'MagickMagnifyImage');
  Pointer(MagickMapImage):=GetProcAddress(LibWand, 'MagickMapImage');
  Pointer(MagickMatteFloodfillImage):=GetProcAddress(LibWand, 'MagickMatteFloodfillImage');
  Pointer(MagickMedianFilterImage):=GetProcAddress(LibWand, 'MagickMedianFilterImage');
  Pointer(MagickMinifyImage):=GetProcAddress(LibWand, 'MagickMinifyImage');
  Pointer(MagickModulateImage):=GetProcAddress(LibWand, 'MagickModulateImage');
  Pointer(MagickMotionBlurImage):=GetProcAddress(LibWand, 'MagickMotionBlurImage');
  Pointer(MagickNegateImage):=GetProcAddress(LibWand, 'MagickNegateImage');
  Pointer(MagickNewImage):=GetProcAddress(LibWand, 'MagickNewImage');
  Pointer(MagickNextImage):=GetProcAddress(LibWand, 'MagickNextImage');
  Pointer(MagickNormalizeImage):=GetProcAddress(LibWand, 'MagickNormalizeImage');
  Pointer(MagickOilPaintImage):=GetProcAddress(LibWand, 'MagickOilPaintImage');
  Pointer(MagickPaintOpaqueImage):=GetProcAddress(LibWand, 'MagickPaintOpaqueImage');
  Pointer(MagickPaintTransparentImage):=GetProcAddress(LibWand, 'MagickPaintTransparentImage');
  Pointer(MagickPingImage):=GetProcAddress(LibWand, 'MagickPingImage');
  Pointer(MagickPreviousImage):=GetProcAddress(LibWand, 'MagickPreviousImage');
  Pointer(MagickProfileImage):=GetProcAddress(LibWand, 'MagickProfileImage');
  Pointer(MagickQuantizeImage):=GetProcAddress(LibWand, 'MagickQuantizeImage');
  Pointer(MagickQuantizeImages):=GetProcAddress(LibWand, 'MagickQuantizeImages');
  Pointer(MagickRadialBlurImage):=GetProcAddress(LibWand, 'MagickRadialBlurImage');
  Pointer(MagickRadialBlurImageChannel):=GetProcAddress(LibWand, 'MagickRadialBlurImageChannel');
  Pointer(MagickRaiseImage):=GetProcAddress(LibWand, 'MagickRaiseImage');
  Pointer(MagickReadImage):=GetProcAddress(LibWand, 'MagickReadImage');
  Pointer(MagickReadImageBlob):=GetProcAddress(LibWand, 'MagickReadImageBlob');
  Pointer(MagickReduceNoiseImage):=GetProcAddress(LibWand, 'MagickReduceNoiseImage');
  Pointer(MagickRemoveImage):=GetProcAddress(LibWand, 'MagickRemoveImage');
  Pointer(MagickResampleImage):=GetProcAddress(LibWand, 'MagickResampleImage');
  Pointer(MagickResizeImage):=GetProcAddress(LibWand, 'MagickResizeImage');
  Pointer(MagickRollImage):=GetProcAddress(LibWand, 'MagickRollImage');
  Pointer(MagickRotateImage):=GetProcAddress(LibWand, 'MagickRotateImage');
  Pointer(MagickSampleImage):=GetProcAddress(LibWand, 'MagickSampleImage');
  Pointer(MagickScaleImage):=GetProcAddress(LibWand, 'MagickScaleImage');
  Pointer(MagickSeparateImageChannel):=GetProcAddress(LibWand, 'MagickSeparateImageChannel');
  Pointer(MagickSetImage):=GetProcAddress(LibWand, 'MagickSetImage');
  Pointer(MagickSetImageAttribute):=GetProcAddress(LibWand, 'MagickSetImageAttribute');
  Pointer(MagickSetImageBackgroundColor):=GetProcAddress(LibWand, 'MagickSetImageBackgroundColor');
  Pointer(MagickSetImageBias):=GetProcAddress(LibWand, 'MagickSetImageBias');
  Pointer(MagickSetImageBluePrimary):=GetProcAddress(LibWand, 'MagickSetImageBluePrimary');
  Pointer(MagickSetImageBorderColor):=GetProcAddress(LibWand, 'MagickSetImageBorderColor');
  Pointer(MagickSetImageChannelDepth):=GetProcAddress(LibWand, 'MagickSetImageChannelDepth');
  Pointer(MagickSetImageColormapColor):=GetProcAddress(LibWand, 'MagickSetImageColormapColor');
  Pointer(MagickSetImageColorspace):=GetProcAddress(LibWand, 'MagickSetImageColorspace');
  Pointer(MagickSetImageCompose):=GetProcAddress(LibWand, 'MagickSetImageCompose');
  Pointer(MagickSetImageCompression):=GetProcAddress(LibWand, 'MagickSetImageCompression');
  Pointer(MagickSetImageCompressionQuality):=GetProcAddress(LibWand, 'MagickSetImageCompressionQuality');
  Pointer(MagickSetImageDelay):=GetProcAddress(LibWand, 'MagickSetImageDelay');
  Pointer(MagickSetImageDepth):=GetProcAddress(LibWand, 'MagickSetImageDepth');
  Pointer(MagickSetImageDispose):=GetProcAddress(LibWand, 'MagickSetImageDispose');
  Pointer(MagickSetImageExtent):=GetProcAddress(LibWand, 'MagickSetImageExtent');
  Pointer(MagickSetImageFilename):=GetProcAddress(LibWand, 'MagickSetImageFilename');
  Pointer(MagickSetImageFormat):=GetProcAddress(LibWand, 'MagickSetImageFormat');
  Pointer(MagickSetImageGamma):=GetProcAddress(LibWand, 'MagickSetImageGamma');
  Pointer(MagickSetImageGreenPrimary):=GetProcAddress(LibWand, 'MagickSetImageGreenPrimary');
  Pointer(MagickSetImageIndex):=GetProcAddress(LibWand, 'MagickSetImageIndex');
  Pointer(MagickSetImageInterlaceScheme):=GetProcAddress(LibWand, 'MagickSetImageInterlaceScheme');
  Pointer(MagickSetImageIterations):=GetProcAddress(LibWand, 'MagickSetImageIterations');
  Pointer(MagickSetImageMatteColor):=GetProcAddress(LibWand, 'MagickSetImageMatteColor');
  Pointer(MagickSetImagePage):=GetProcAddress(LibWand, 'MagickSetImagePage');
  Pointer(MagickSetImagePixels):=GetProcAddress(LibWand, 'MagickSetImagePixels');
  Pointer(MagickSetImageProfile):=GetProcAddress(LibWand, 'MagickSetImageProfile');
  Pointer(MagickSetImageRedPrimary):=GetProcAddress(LibWand, 'MagickSetImageRedPrimary');
  Pointer(MagickSetImageRenderingIntent):=GetProcAddress(LibWand, 'MagickSetImageRenderingIntent');
  Pointer(MagickSetImageResolution):=GetProcAddress(LibWand, 'MagickSetImageResolution');
  Pointer(MagickSetImageScene):=GetProcAddress(LibWand, 'MagickSetImageScene');
  Pointer(MagickSetImageType):=GetProcAddress(LibWand, 'MagickSetImageType');
  Pointer(MagickSetImageUnits):=GetProcAddress(LibWand, 'MagickSetImageUnits');
  Pointer(MagickSetImageWhitePoint):=GetProcAddress(LibWand, 'MagickSetImageWhitePoint');
  Pointer(MagickShadowImage):=GetProcAddress(LibWand, 'MagickShadowImage');
  Pointer(MagickSharpenImage):=GetProcAddress(LibWand, 'MagickSharpenImage');
  Pointer(MagickSharpenImageChannel):=GetProcAddress(LibWand, 'MagickSharpenImageChannel');
  Pointer(MagickShaveImage):=GetProcAddress(LibWand, 'MagickShaveImage');
  Pointer(MagickShearImage):=GetProcAddress(LibWand, 'MagickShearImage');
  Pointer(MagickSigmoidalContrastImage):=GetProcAddress(LibWand, 'MagickSigmoidalContrastImage');
  Pointer(MagickSigmoidalContrastImageChannel):=GetProcAddress(LibWand, 'MagickSigmoidalContrastImageChannel');
  Pointer(MagickSolarizeImage):=GetProcAddress(LibWand, 'MagickSolarizeImage');
  Pointer(MagickSpliceImage):=GetProcAddress(LibWand, 'MagickSpliceImage');
  Pointer(MagickSpreadImage):=GetProcAddress(LibWand, 'MagickSpreadImage');
  Pointer(MagickStripImage):=GetProcAddress(LibWand, 'MagickStripImage');
  Pointer(MagickSwirlImage):=GetProcAddress(LibWand, 'MagickSwirlImage');
  Pointer(MagickTintImage):=GetProcAddress(LibWand, 'MagickTintImage');
  Pointer(MagickThresholdImage):=GetProcAddress(LibWand, 'MagickThresholdImage');
  Pointer(MagickThresholdImageChannel):=GetProcAddress(LibWand, 'MagickThresholdImageChannel');
  Pointer(MagickTrimImage):=GetProcAddress(LibWand, 'MagickTrimImage');
  Pointer(MagickUnsharpMaskImage):=GetProcAddress(LibWand, 'MagickUnsharpMaskImage');
  Pointer(MagickUnsharpMaskImageChannel):=GetProcAddress(LibWand, 'MagickUnsharpMaskImageChannel');
  Pointer(MagickWaveImage):=GetProcAddress(LibWand, '');
  Pointer(MagickWhiteThresholdImage):=GetProcAddress(LibWand, 'MagickWhiteThresholdImage');
  Pointer(MagickWriteImage):=GetProcAddress(LibWand, 'MagickWriteImage');
  Pointer(MagickWriteImages):=GetProcAddress(LibWand, 'MagickWriteImages');
  Pointer(MagickSetImageProgressMonitor):=GetProcAddress(LibWand, 'MagickSetImageProgressMonitor');
  Pointer(MagickGetImageSize):=GetProcAddress(LibWand, 'MagickGetImageSize');
  Pointer(MagickAppendImages):=GetProcAddress(LibWand, 'MagickAppendImages');
  Pointer(MagickAverageImages):=GetProcAddress(LibWand, 'MagickAverageImages');
  Pointer(MagickCoalesceImages):=GetProcAddress(LibWand, 'MagickCoalesceImages');
  Pointer(MagickCombineImages):=GetProcAddress(LibWand, 'MagickCombineImages');
  Pointer(MagickCompareImageChannels):=GetProcAddress(LibWand, 'MagickCompareImageChannels');
  Pointer(MagickCompareImages):=GetProcAddress(LibWand, 'MagickCompareImages');
  Pointer(MagickDeconstructImages):=GetProcAddress(LibWand, 'MagickDeconstructImages');
  Pointer(MagickFlattenImages):=GetProcAddress(LibWand, 'MagickFlattenImages');
  Pointer(MagickFxImage):=GetProcAddress(LibWand, 'MagickFxImage');
  Pointer(MagickFxImageChannel):=GetProcAddress(LibWand, 'MagickFxImageChannel');
  Pointer(MagickGetImage):=GetProcAddress(LibWand, 'MagickGetImage');
  Pointer(MagickGetImageRegion):=GetProcAddress(LibWand, 'MagickGetImageRegion');
  Pointer(MagickMorphImages):=GetProcAddress(LibWand, 'MagickMorphImages');
  Pointer(MagickMosaicImages):=GetProcAddress(LibWand, 'MagickMosaicImages');
  Pointer(MagickPreviewImages):=GetProcAddress(LibWand, 'MagickPreviewImages');
  Pointer(MagickSteganoImage):=GetProcAddress(LibWand, 'MagickSteganoImage');
  Pointer(MagickStereoImage):=GetProcAddress(LibWand, 'MagickStereoImage');
  Pointer(MagickTextureImage):=GetProcAddress(LibWand, 'MagickTextureImage');
  Pointer(MagickTransformImage):=GetProcAddress(LibWand, 'MagickTransformImage');
  Pointer(NewMagickWandFromImage):=GetProcAddress(LibWand, 'NewMagickWandFromImage');

  Pointer(MagickGetImageHistogram):=GetProcAddress(LibWand, 'MagickGetImageHistogram');

  Pointer(MagickGetImageRenderingIntent):=GetProcAddress(LibWand, 'MagickGetImageRenderingIntent');

  Pointer(MagickGetImageUnits):=GetProcAddress(LibWand, 'MagickGetImageUnits');

  Pointer(MagickGetImageBlob):=GetProcAddress(LibWand, 'MagickGetImageBlob');
  Pointer(MagickGetImagesBlob):=GetProcAddress(LibWand, 'MagickGetImagesBlob');
  Pointer(MagickGetImageProfile):=GetProcAddress(LibWand, 'MagickGetImageProfile');
  Pointer(MagickRemoveImageProfile):=GetProcAddress(LibWand, 'MagickRemoveImageProfile');

  Pointer(MagickGetImageColors):=GetProcAddress(LibWand, 'MagickGetImageColors');
  Pointer(MagickGetImageCompressionQuality):=GetProcAddress(LibWand, 'MagickGetImageCompressionQuality');
  Pointer(MagickGetImageDelay):=GetProcAddress(LibWand, 'MagickGetImageDelay');
  Pointer(MagickGetImageChannelDepth):=GetProcAddress(LibWand, 'MagickGetImageChannelDepth');
  Pointer(MagickGetImageDepth):=GetProcAddress(LibWand, 'MagickGetImageDepth');
  Pointer(MagickGetImageHeight):=GetProcAddress(LibWand, 'MagickGetImageHeight');
  Pointer(MagickGetImageIterations):=GetProcAddress(LibWand, 'MagickGetImageIterations');
  Pointer(MagickGetImageScene):=GetProcAddress(LibWand, 'MagickGetImageScene');
  Pointer(MagickGetImageWidth):=GetProcAddress(LibWand, 'MagickGetImageWidth');
  Pointer(MagickGetNumberImages):=GetProcAddress(LibWand, 'MagickGetNumberImages');

  {$DEFINE IMPLEMENTATION}
  {.$include PixelWand.inc}
  {.$include DrawingWand.inc}
  {.$include MagickAttribute.inc}
  {.$include PixelIterator.inc}
  {$include MagickProperty.inc}

  {$UNDEF IMPLEMENTATION}
end;


Function Finalize(LibWand:TLibHandle):boolean;
begin
  Result:=True;
end;

end.
