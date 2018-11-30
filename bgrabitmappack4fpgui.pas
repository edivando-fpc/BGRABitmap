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


Unit BGRABitmapPack4fpGUI;

interface

uses
  BGRAAnimatedGif, BGRABitmap, BGRABitmapTypes, BGRABlend, BGRACanvas, 
  BGRACanvas2D, BGRAColorInt, BGRACompressableBitmap, BGRACoordPool3D, 
  BGRADefaultBitmap, BGRADNetDeserial, BGRAFillInfo, BGRAFilters, 
  BGRAGradients, BGRAGradientScanner, BGRALayers, BGRAMatrix3D, 
  BGRAOpenRaster, BGRAPaintNet, BGRAPath, BGRAPen, BGRAPhongTypes, 
  BGRAPolygon, BGRAPolygonAliased, BGRAResample, BGRAScene3D, 
  BGRASliceScaling, BGRASSE, BGRAStreamLayers, BGRATransform, 
  BGRAGrayscaleMask, BGRAReadBMP, BGRAReadGif, BGRAReadPCX, BGRAReadPng, 
  BGRAReadPSD, BGRAThumbnail, BGRAReadTGA, BGRAReadJpeg, BGRAReadLzp, 
  UnzipperExt, BGRALzpCommon, BGRAWriteLzp, BGRAReadXPM, BGRAUnits, 
  BGRAReadBmpMioMap, BGRAArrow, BGRAGraphics, BGRAUTF8, BGRAfpGUIBitmap, 
  BGRATypewriter, BGRASVG, BGRASVGShapes, BGRASVGType, BGRAPalette, 
  BGRAColorQuantization, BGRADithering, BGRAFreeType, BGRACustomTextFX, 
  BGRAWritePNG, BGRAGifFormat, BGRASceneTypes, BGRARenderer3D, 
  BGRAWriteBmpMioMap, BGRAPhoxo, BGRAFilterScanner, BGRAFilterType, 
  BGRAFilterBlur, BGRAUnicode;

implementation

end.
