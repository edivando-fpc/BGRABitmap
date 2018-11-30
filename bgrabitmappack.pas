{ ***************************************************************************
 *                                                                          *
 *  This file is part of BGRABitmap library which is distributed under the  *
 *  modified LGPL.                                                          *
 *                                                                          *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,   *
 *  for details about the copyright.                                        *
 *                                                                          *
 *  This program is distributed in the hope that it will be useful,         *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of          *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    *
 *                                                                          *
 ************************* BGRABitmap library  ******************************

 - Drawing routines with transparency and antialiasing with Lazarus.
   Offers also various transforms.
 - These routines allow to manipulate 32bit images in BGRA format or RGBA
   format (depending on the platform).
 - This code is under modified LGPL (see COPYING.modifiedLGPL.txt).
   This means that you can link this library inside your programs for any purpose.
   Only the included part of the code must remain LGPL.

 - If you make some improvements to this library, please notify here:
   http://www.lazarus.freepascal.org/index.php/topic,12037.0.html

   ********************* Contact : Circular at operamail.com *******************


   ******************************* CONTRIBUTOR(S) ******************************
   - Edivando S. Santos Brasil | mailedivando@gmail.com
     (Compatibility with FPC ($Mode objfpc/delphi) and delphi VCL 11/2018)

   ***************************** END CONTRIBUTOR(S) *****************************}


Unit BGRABitmapPack;

{$warn 5023 off : no warning about unused units}
interface

uses
 BGRAAnimatedGif, BGRABitmap, BGRABitmapTypes, BGRABlend, BGRACanvas, 
 BGRACanvas2D, BGRAColorInt, BGRACompressableBitmap, BGRACoordPool3D, 
 BGRADefaultBitmap, BGRADNetDeserial, BGRAFilters, BGRAFreeType, 
 BGRAGradients, BGRAGradientScanner, BGRALayers, BGRAMatrix3D, BGRAOpenRaster, 
 BGRAPaintNet, BGRAPath, BGRAPen, BGRAPhongTypes, BGRAPolygon, 
 BGRAPolygonAliased, BGRAResample, BGRAScene3D, BGRASliceScaling, BGRASSE, 
 BGRAStreamLayers, BGRAText, BGRATextFX, BGRATransform, BGRATypewriter, 
 BGRAVectorize, BGRAGrayscaleMask, BGRAReadBMP, BGRAReadGif, BGRAReadPCX, 
 BGRAReadPng, BGRAReadPSD, BGRAThumbnail, BGRAReadTGA, BGRAReadIco, 
 BGRAReadJpeg, BGRAReadLzp, UnzipperExt, BGRALzpCommon, BGRAWriteLzp, 
 BGRAReadXPM, BGRASVG, BGRAUnits, BGRASVGShapes, BGRASVGType, 
 BGRAReadBmpMioMap, BGRAArrow, BGRAPalette, BGRAColorQuantization, 
 BGRADithering, BGRAUTF8, BGRALCLBitmap, BGRAWritePNG, BGRAGifFormat, 
 BGRAGraphics, BGRASceneTypes, BGRARenderer3D, BGRAWriteBmpMioMap, 
 BGRAOpenGLType, BGRASpriteGL, BGRAOpenGL, BGRACanvasGL, BGRAFontGL, 
 BGRAOpenGL3D, BGRAPhoxo, BGRAFilterScanner, BGRAFilterType, BGRAFilterBlur, 
 BGRAMultiFileType, BGRAWinResource, BGRALazResource, BGRAIconCursor, 
 BGRABlurGL, BGRAReadTiff, BGRALazPaint, BGRAMemDirectory, BGRAUnicode, 
 BGRATextBidi, BGRALayerOriginal, BGRASVGOriginal, BGRAGradientOriginal, 
 bgratypes, BGRAFillInfo;

implementation

end.
