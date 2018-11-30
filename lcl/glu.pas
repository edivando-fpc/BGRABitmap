  {
   * SGI FREE SOFTWARE LICENSE B (Version 2.0, Sept. 18, 2008)
   * Copyright (C) 1991-2000 Silicon Graphics, Inc. All Rights Reserved.
   *
   * Permission is hereby granted, free of charge, to any person obtaining a
   * copy of this software and associated documentation files (the "Software"),
   * to deal in the Software without restriction, including without limitation
   * the rights to use, copy, modify, merge, publish, distribute, sublicense,
   * and/or sell copies of the Software, and to permit persons to whom the
   * Software is furnished to do so, subject to the following conditions:
   *
   * The above copyright notice including the dates of first publication and
   * either this permission notice or a reference to
   * http://oss.sgi.com/projects/FreeB/
   * shall be included in all copies or substantial portions of the Software.
   *
   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
   * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
   * SILICON GRAPHICS, INC. BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
   * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF
   * OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   * SOFTWARE.
   *
   * Except as contained in this notice, the name of Silicon Graphics, Inc.
   * shall not be used in advertising or otherwise to promote the sale, use or
   * other dealings in this Software without prior written authorization from
   * Silicon Graphics, Inc.
    }

unit glu;
interface

{
  Automatically converted by H2Pas 1.0.0 from out.h
  The following command line parameters were used:
    -P
    -D
    -l
    glu.dll
    out
    -o
    glu.pp
}

uses
  SysUtils, types, classes,
  Windows,
  GL;

Const
{$IFDEF Windows}
  GLU_Lib = 'glu32.dll';
{$ELSE}
{$IFDEF OS2}
  GLU_Lib = 'opengl.dll';
{$ELSE OS2}
{$ifdef darwin}
  GLU_LIB =  '/System/Library/Frameworks/OpenGL.framework/Libraries/libGLU.dylib';
{$else}
{$IFDEF haiku}
  GLU_LIB = 'libGLU.so';
{$ELSE}
{$ifdef MorphOS}
  GLU_LIB = 'tinygl.library';
{$else}
  GLU_LIB = 'libGLU.so.1';
{$endif}
{$ENDIF}
{$ENDIF}
{$ENDIF OS2}
{$endif}

type
  TViewPortArray = array [0..3] of GLint;
  T16dArray = array [0..15] of GLdouble;
  T3dArray = array [0..2] of GLdouble;
  T4pArray = array [0..3] of Pointer;
  T4fArray = array [0..3] of GLfloat;

const
  GLU_EXT_object_space_tess = 1;
  GLU_EXT_nurbs_tessellator = 1;
{ Boolean  }
  GLU_FALSE = 0;
  GLU_TRUE = 1;
{ Version  }
  GLU_VERSION_1_1 = 1;
  GLU_VERSION_1_2 = 1;
  GLU_VERSION_1_3 = 1;
{ StringName  }
  GLU_VERSION = 100800;
  GLU_EXTENSIONS = 100801;
{ ErrorCode  }
  GLU_INVALID_ENUM = 100900;
  GLU_INVALID_VALUE = 100901;
  GLU_OUT_OF_MEMORY = 100902;
  GLU_INCOMPATIBLE_GL_VERSION = 100903;
  GLU_INVALID_OPERATION = 100904;
{ NurbsDisplay  }
{      GLU_FILL  }
  GLU_OUTLINE_POLYGON = 100240;
  GLU_OUTLINE_PATCH = 100241;
{ NurbsCallback  }
  GLU_NURBS_ERROR = 100103;
  GLU_ERROR = 100103;
  GLU_NURBS_BEGIN = 100164;
  GLU_NURBS_BEGIN_EXT = 100164;
  GLU_NURBS_VERTEX = 100165;
  GLU_NURBS_VERTEX_EXT = 100165;
  GLU_NURBS_NORMAL = 100166;
  GLU_NURBS_NORMAL_EXT = 100166;
  GLU_NURBS_COLOR = 100167;
  GLU_NURBS_COLOR_EXT = 100167;
  GLU_NURBS_TEXTURE_COORD = 100168;
  GLU_NURBS_TEX_COORD_EXT = 100168;
  GLU_NURBS_END = 100169;
  GLU_NURBS_END_EXT = 100169;
  GLU_NURBS_BEGIN_DATA = 100170;
  GLU_NURBS_BEGIN_DATA_EXT = 100170;
  GLU_NURBS_VERTEX_DATA = 100171;
  GLU_NURBS_VERTEX_DATA_EXT = 100171;
  GLU_NURBS_NORMAL_DATA = 100172;
  GLU_NURBS_NORMAL_DATA_EXT = 100172;
  GLU_NURBS_COLOR_DATA = 100173;
  GLU_NURBS_COLOR_DATA_EXT = 100173;
  GLU_NURBS_TEXTURE_COORD_DATA = 100174;
  GLU_NURBS_TEX_COORD_DATA_EXT = 100174;
  GLU_NURBS_END_DATA = 100175;
  GLU_NURBS_END_DATA_EXT = 100175;
{ NurbsError  }
  GLU_NURBS_ERROR1 = 100251;
  GLU_NURBS_ERROR2 = 100252;
  GLU_NURBS_ERROR3 = 100253;
  GLU_NURBS_ERROR4 = 100254;
  GLU_NURBS_ERROR5 = 100255;
  GLU_NURBS_ERROR6 = 100256;
  GLU_NURBS_ERROR7 = 100257;
  GLU_NURBS_ERROR8 = 100258;
  GLU_NURBS_ERROR9 = 100259;
  GLU_NURBS_ERROR10 = 100260;
  GLU_NURBS_ERROR11 = 100261;
  GLU_NURBS_ERROR12 = 100262;
  GLU_NURBS_ERROR13 = 100263;
  GLU_NURBS_ERROR14 = 100264;
  GLU_NURBS_ERROR15 = 100265;
  GLU_NURBS_ERROR16 = 100266;
  GLU_NURBS_ERROR17 = 100267;
  GLU_NURBS_ERROR18 = 100268;
  GLU_NURBS_ERROR19 = 100269;
  GLU_NURBS_ERROR20 = 100270;
  GLU_NURBS_ERROR21 = 100271;
  GLU_NURBS_ERROR22 = 100272;
  GLU_NURBS_ERROR23 = 100273;
  GLU_NURBS_ERROR24 = 100274;
  GLU_NURBS_ERROR25 = 100275;
  GLU_NURBS_ERROR26 = 100276;
  GLU_NURBS_ERROR27 = 100277;
  GLU_NURBS_ERROR28 = 100278;
  GLU_NURBS_ERROR29 = 100279;
  GLU_NURBS_ERROR30 = 100280;
  GLU_NURBS_ERROR31 = 100281;
  GLU_NURBS_ERROR32 = 100282;
  GLU_NURBS_ERROR33 = 100283;
  GLU_NURBS_ERROR34 = 100284;
  GLU_NURBS_ERROR35 = 100285;
  GLU_NURBS_ERROR36 = 100286;
  GLU_NURBS_ERROR37 = 100287;
{ NurbsProperty  }
  GLU_AUTO_LOAD_MATRIX = 100200;
  GLU_CULLING = 100201;
  GLU_SAMPLING_TOLERANCE = 100203;
  GLU_DISPLAY_MODE = 100204;
  GLU_PARAMETRIC_TOLERANCE = 100202;
  GLU_SAMPLING_METHOD = 100205;
  GLU_U_STEP = 100206;
  GLU_V_STEP = 100207;
  GLU_NURBS_MODE = 100160;
  GLU_NURBS_MODE_EXT = 100160;
  GLU_NURBS_TESSELLATOR = 100161;
  GLU_NURBS_TESSELLATOR_EXT = 100161;
  GLU_NURBS_RENDERER = 100162;
  GLU_NURBS_RENDERER_EXT = 100162;
{ NurbsSampling  }
  GLU_OBJECT_PARAMETRIC_ERROR = 100208;
  GLU_OBJECT_PARAMETRIC_ERROR_EXT = 100208;
  GLU_OBJECT_PATH_LENGTH = 100209;
  GLU_OBJECT_PATH_LENGTH_EXT = 100209;
  GLU_PATH_LENGTH = 100215;
  GLU_PARAMETRIC_ERROR = 100216;
  GLU_DOMAIN_DISTANCE = 100217;
{ NurbsTrim  }
  GLU_MAP1_TRIM_2 = 100210;
  GLU_MAP1_TRIM_3 = 100211;
{ QuadricDrawStyle  }
  GLU_POINT = 100010;
  GLU_LINE = 100011;
  GLU_FILL = 100012;
  GLU_SILHOUETTE = 100013;
{ QuadricCallback  }
{      GLU_ERROR  }
{ QuadricNormal  }
  GLU_SMOOTH = 100000;
  GLU_FLAT = 100001;
  GLU_NONE = 100002;
{ QuadricOrientation  }
  GLU_OUTSIDE = 100020;
  GLU_INSIDE = 100021;
{ TessCallback  }
  GLU_TESS_BEGIN = 100100;
  GLU_BEGIN = 100100;
  GLU_TESS_VERTEX = 100101;
  GLU_VERTEX = 100101;
  GLU_TESS_END = 100102;
  GLU_END = 100102;
  GLU_TESS_ERROR = 100103;
  GLU_TESS_EDGE_FLAG = 100104;
  GLU_EDGE_FLAG = 100104;
  GLU_TESS_COMBINE = 100105;
  GLU_TESS_BEGIN_DATA = 100106;
  GLU_TESS_VERTEX_DATA = 100107;
  GLU_TESS_END_DATA = 100108;
  GLU_TESS_ERROR_DATA = 100109;
  GLU_TESS_EDGE_FLAG_DATA = 100110;
  GLU_TESS_COMBINE_DATA = 100111;
{ TessContour  }
  GLU_CW = 100120;
  GLU_CCW = 100121;
  GLU_INTERIOR = 100122;
  GLU_EXTERIOR = 100123;
  GLU_UNKNOWN = 100124;
{ TessProperty  }
  GLU_TESS_WINDING_RULE = 100140;
  GLU_TESS_BOUNDARY_ONLY = 100141;
  GLU_TESS_TOLERANCE = 100142;
{ TessError  }
  GLU_TESS_ERROR1 = 100151;
  GLU_TESS_ERROR2 = 100152;
  GLU_TESS_ERROR3 = 100153;
  GLU_TESS_ERROR4 = 100154;
  GLU_TESS_ERROR5 = 100155;
  GLU_TESS_ERROR6 = 100156;
  GLU_TESS_ERROR7 = 100157;
  GLU_TESS_ERROR8 = 100158;
  GLU_TESS_MISSING_BEGIN_POLYGON = 100151;
  GLU_TESS_MISSING_BEGIN_CONTOUR = 100152;
  GLU_TESS_MISSING_END_POLYGON = 100153;
  GLU_TESS_MISSING_END_CONTOUR = 100154;
  GLU_TESS_COORD_TOO_LARGE = 100155;
  GLU_TESS_NEED_COMBINE_CALLBACK = 100156;
{ TessWinding  }
  GLU_TESS_WINDING_ODD = 100130;
  GLU_TESS_WINDING_NONZERO = 100131;
  GLU_TESS_WINDING_POSITIVE = 100132;
  GLU_TESS_WINDING_NEGATIVE = 100133;
  GLU_TESS_WINDING_ABS_GEQ_TWO = 100134;
const
  GLU_TESS_MAX_COORD = 1.0e150;

{*********************************************************** }

Type
  GLUnurbs = record end;
  GLUquadric = record end;
  GLUtesselator = record end;

  PGLdouble  = ^GLdouble;
  PGLfloat  = ^GLfloat;
  PGLint  = ^GLint;
  PGLubyte  = PAnsiChar; //< this is only used for strings in GLU
  PGLUnurbs  = ^GLUnurbs;
  PGLUquadric  = ^GLUquadric;
  PGLUtesselator  = ^GLUtesselator;

  GLUnurbsObj = GLUnurbs;
  GLUquadricObj = GLUquadric;
  GLUtesselatorObj = GLUtesselator;
  GLUtriangulatorObj = GLUtesselator;
  _GLUfuncptr = procedure ;stdcall;
  TCallback   =  _GLUfuncptr;


var
  gluBeginCurve : procedure(nurb:PGLUnurbs);stdcall;
  gluBeginPolygon : procedure(tess:PGLUtesselator);stdcall;
  gluBeginSurface : procedure(nurb:PGLUnurbs);stdcall;
  gluBeginTrim : procedure(nurb:PGLUnurbs);stdcall;
  gluBuild1DMipmapLevels : function(target:GLenum; internalFormat:GLint; width:GLsizei; format:GLenum; _type:GLenum;
    level:GLint; base:GLint; max:GLint; data:pointer):GLint;stdcall;

  gluBuild1DMipmaps : function(target:GLenum; internalFormat:GLint; width:GLsizei; format:GLenum; _type:GLenum;
    data:pointer):GLint;stdcall;

  gluBuild2DMipmapLevels : function(target:GLenum; internalFormat:GLint; width:GLsizei; height:GLsizei; format:GLenum;
    _type:GLenum; level:GLint; base:GLint; max:GLint; data:pointer):GLint;stdcall;

  gluBuild2DMipmaps : function(target:GLenum; internalFormat:GLint; width:GLsizei; height:GLsizei; format:GLenum;
    _type:GLenum; data:pointer):GLint;stdcall;

  gluBuild3DMipmapLevels : function(target:GLenum; internalFormat:GLint; width:GLsizei; height:GLsizei; depth:GLsizei;
    format:GLenum; _type:GLenum; level:GLint; base:GLint; max:GLint;
    data:pointer):GLint;stdcall;

  gluBuild3DMipmaps : function(target:GLenum; internalFormat:GLint; width:GLsizei; height:GLsizei; depth:GLsizei;
    format:GLenum; _type:GLenum; data:pointer):GLint;stdcall;


  gluCheckExtension : function(extName:PGLubyte; extString:PGLubyte):GLboolean;stdcall;
  gluCylinder : procedure(quad:PGLUquadric; base:GLdouble; top:GLdouble; height:GLdouble; slices:GLint;
    stacks:GLint);stdcall;
  gluDeleteNurbsRenderer : procedure(nurb:PGLUnurbs);stdcall;
  gluDeleteQuadric : procedure(quad:PGLUquadric);stdcall;
  gluDeleteTess : procedure(tess:PGLUtesselator);stdcall;
  gluDisk : procedure(quad:PGLUquadric; inner:GLdouble; outer:GLdouble; slices:GLint; loops:GLint);stdcall;
  gluEndCurve : procedure(nurb:PGLUnurbs);stdcall;
  gluEndPolygon : procedure(tess:PGLUtesselator);stdcall;
  gluEndSurface : procedure(nurb:PGLUnurbs);stdcall;
  gluEndTrim : procedure(nurb:PGLUnurbs);stdcall;

  gluErrorString : function(error:GLenum): PGLubyte;stdcall;
  gluGetNurbsProperty : procedure(nurb:PGLUnurbs; _property:GLenum; data:PGLfloat);stdcall;

  gluGetString : function(name:GLenum):PGLubyte;stdcall;
  gluGetTessProperty : procedure(tess:PGLUtesselator; which:GLenum; data:PGLdouble);stdcall;



  gluLoadSamplingMatrices : procedure(nurb:PGLUnurbs; model:PGLfloat; perspective:PGLfloat; view:PGLint);stdcall;
  gluLookAt : procedure(eyeX:GLdouble; eyeY:GLdouble; eyeZ:GLdouble; centerX:GLdouble; centerY:GLdouble;
    centerZ:GLdouble; upX:GLdouble; upY:GLdouble; upZ:GLdouble);stdcall;
  gluNewNurbsRenderer : function:PGLUnurbs;stdcall;
  gluNewQuadric : function:PGLUquadric;stdcall;
  gluNewTess : function:PGLUtesselator;stdcall;
  gluNextContour : procedure(tess:PGLUtesselator; _type:GLenum);stdcall;
  gluNurbsCallback : procedure(nurb:PGLUnurbs; which:GLenum; CallBackFunc:_GLUfuncptr);stdcall;
  gluNurbsCallbackData : procedure(nurb:PGLUnurbs; userData:PGLvoid);stdcall;
  gluNurbsCallbackDataEXT : procedure(nurb:PGLUnurbs; userData:PGLvoid);stdcall;
  gluNurbsCurve : procedure(nurb:PGLUnurbs; knotCount:GLint; knots:PGLfloat; stride:GLint; control:PGLfloat;
    order:GLint; _type:GLenum);stdcall;
  gluNurbsProperty : procedure(nurb:PGLUnurbs; _property:GLenum; value:GLfloat);stdcall;
  gluNurbsSurface : procedure(nurb:PGLUnurbs; sKnotCount:GLint; sKnots:PGLfloat; tKnotCount:GLint; tKnots:PGLfloat;
    sStride:GLint; tStride:GLint; control:PGLfloat; sOrder:GLint; tOrder:GLint;
    _type:GLenum);stdcall;
  gluOrtho2D : procedure(left:GLdouble; right:GLdouble; bottom:GLdouble; top:GLdouble);stdcall;
  gluPartialDisk : procedure(quad:PGLUquadric; inner:GLdouble; outer:GLdouble; slices:GLint; loops:GLint;
    start:GLdouble; sweep:GLdouble);stdcall;
  gluPerspective : procedure(fovy:GLdouble; aspect:GLdouble; zNear:GLdouble; zFar:GLdouble);stdcall;
  gluPickMatrix : procedure(x:GLdouble; y:GLdouble; delX:GLdouble; delY:GLdouble; viewport:PGLint);stdcall;



  gluProject : function(objX:GLdouble; objY:GLdouble; objZ:GLdouble; model:PGLdouble; proj:PGLdouble;
    view:PGLint; winX:PGLdouble; winY:PGLdouble; winZ:PGLdouble):GLint;stdcall;
  gluPwlCurve : procedure(nurb:PGLUnurbs; count:GLint; data:PGLfloat; stride:GLint; _type:GLenum);stdcall;
  gluQuadricCallback : procedure(quad:PGLUquadric; which:GLenum; CallBackFunc:_GLUfuncptr);stdcall;
  gluQuadricDrawStyle : procedure(quad:PGLUquadric; draw:GLenum);stdcall;
  gluQuadricNormals : procedure(quad:PGLUquadric; normal:GLenum);stdcall;
  gluQuadricOrientation : procedure(quad:PGLUquadric; orientation:GLenum);stdcall;
  gluQuadricTexture : procedure(quad:PGLUquadric; texture:GLboolean);stdcall;

  gluScaleImage : function(format:GLenum; wIn:GLsizei; hIn:GLsizei; typeIn:GLenum; dataIn:pointer;
    wOut:GLsizei; hOut:GLsizei; typeOut:GLenum; dataOut:PGLvoid):GLint;stdcall;
  gluSphere : procedure(quad:PGLUquadric; radius:GLdouble; slices:GLint; stacks:GLint);stdcall;
  gluTessBeginContour : procedure(tess:PGLUtesselator);stdcall;
  gluTessBeginPolygon : procedure(tess:PGLUtesselator; data:PGLvoid);stdcall;
  gluTessCallback : procedure(tess:PGLUtesselator; which:GLenum; CallBackFunc:_GLUfuncptr);stdcall;
  gluTessEndContour : procedure(tess:PGLUtesselator);stdcall;
  gluTessEndPolygon : procedure(tess:PGLUtesselator);stdcall;
  gluTessNormal : procedure(tess:PGLUtesselator; valueX:GLdouble; valueY:GLdouble; valueZ:GLdouble);stdcall;
  gluTessProperty : procedure(tess:PGLUtesselator; which:GLenum; data:GLdouble);stdcall;
  gluTessVertex : procedure(tess:PGLUtesselator; location:PGLdouble; data:PGLvoid);stdcall;



  gluUnProject : function(winX:GLdouble; winY:GLdouble; winZ:GLdouble; model:PGLdouble; proj:PGLdouble;
    view:PGLint; objX:PGLdouble; objY:PGLdouble; objZ:PGLdouble):GLint;stdcall;



  gluUnProject4 : function(winX:GLdouble; winY:GLdouble; winZ:GLdouble; clipW:GLdouble; model:PGLdouble;
    proj:PGLdouble; view:PGLint; nearVal:GLdouble; farVal:GLdouble; objX:PGLdouble;
    objY:PGLdouble; objZ:PGLdouble; objW:PGLdouble):GLint;stdcall;

implementation

  uses
   dynlibs;

  var
    hlib : tlibhandle;


  procedure Freeglu;
    begin
     if (hlib <> 0) then
      FreeLibrary(hlib);
      gluBeginCurve:=nil;
      gluBeginPolygon:=nil;
      gluBeginSurface:=nil;
      gluBeginTrim:=nil;
      gluBuild1DMipmapLevels:=nil;
      gluBuild1DMipmaps:=nil;
      gluBuild2DMipmapLevels:=nil;
      gluBuild2DMipmaps:=nil;
      gluBuild3DMipmapLevels:=nil;
      gluBuild3DMipmaps:=nil;
      gluCheckExtension:=nil;
      gluCylinder:=nil;
      gluDeleteNurbsRenderer:=nil;
      gluDeleteQuadric:=nil;
      gluDeleteTess:=nil;
      gluDisk:=nil;
      gluEndCurve:=nil;
      gluEndPolygon:=nil;
      gluEndSurface:=nil;
      gluEndTrim:=nil;
      gluErrorString:=nil;
      gluGetNurbsProperty:=nil;
      gluGetString:=nil;
      gluGetTessProperty:=nil;
      gluLoadSamplingMatrices:=nil;
      gluLookAt:=nil;
      gluNewNurbsRenderer:=nil;
      gluNewQuadric:=nil;
      gluNewTess:=nil;
      gluNextContour:=nil;
      gluNurbsCallback:=nil;
      gluNurbsCallbackData:=nil;
      gluNurbsCallbackDataEXT:=nil;
      gluNurbsCurve:=nil;
      gluNurbsProperty:=nil;
      gluNurbsSurface:=nil;
      gluOrtho2D:=nil;
      gluPartialDisk:=nil;
      gluPerspective:=nil;
      gluPickMatrix:=nil;
      gluProject:=nil;
      gluPwlCurve:=nil;
      gluQuadricCallback:=nil;
      gluQuadricDrawStyle:=nil;
      gluQuadricNormals:=nil;
      gluQuadricOrientation:=nil;
      gluQuadricTexture:=nil;
      gluScaleImage:=nil;
      gluSphere:=nil;
      gluTessBeginContour:=nil;
      gluTessBeginPolygon:=nil;
      gluTessCallback:=nil;
      gluTessEndContour:=nil;
      gluTessEndPolygon:=nil;
      gluTessNormal:=nil;
      gluTessProperty:=nil;
      gluTessVertex:=nil;
      gluUnProject:=nil;
      gluUnProject4:=nil;
    end;


  procedure Loadglu(lib : pchar);
    begin
      Freeglu;
      hlib:=LoadLibrary(lib);
      if hlib=0 then
        raise Exception.Create(format('Could not load library: %s',[lib]));

      gluBeginCurve:= GetProcAddress(hlib,'gluBeginCurve');
      gluBeginPolygon :=GetProcAddress(hlib,'gluBeginPolygon');
      gluBeginSurface :=GetProcAddress(hlib,'gluBeginSurface');
      gluBeginTrim :=GetProcAddress(hlib,'gluBeginTrim');
      gluBuild1DMipmapLevels :=GetProcAddress(hlib,'gluBuild1DMipmapLevels');
      gluBuild1DMipmaps :=GetProcAddress(hlib,'gluBuild1DMipmaps');
      gluBuild2DMipmapLevels :=GetProcAddress(hlib,'gluBuild2DMipmapLevels');
      gluBuild2DMipmaps :=GetProcAddress(hlib,'gluBuild2DMipmaps');
      gluBuild3DMipmapLevels :=GetProcAddress(hlib,'gluBuild3DMipmapLevels');
      gluBuild3DMipmaps :=GetProcAddress(hlib,'gluBuild3DMipmaps');
      gluCheckExtension :=GetProcAddress(hlib,'gluCheckExtension');
      gluCylinder :=GetProcAddress(hlib,'gluCylinder');
      gluDeleteNurbsRenderer :=GetProcAddress(hlib,'gluDeleteNurbsRenderer');
      gluDeleteQuadric :=GetProcAddress(hlib,'gluDeleteQuadric');
      gluDeleteTess :=GetProcAddress(hlib,'gluDeleteTess');
      gluDisk :=GetProcAddress(hlib,'gluDisk');
      gluEndCurve :=GetProcAddress(hlib,'gluEndCurve');
      gluEndPolygon :=GetProcAddress(hlib,'gluEndPolygon');
      gluEndSurface :=GetProcAddress(hlib,'gluEndSurface');
      gluEndTrim :=GetProcAddress(hlib,'gluEndTrim');
      gluErrorString :=GetProcAddress(hlib,'gluErrorString');
      gluGetNurbsProperty :=GetProcAddress(hlib,'gluGetNurbsProperty');
      gluGetString :=GetProcAddress(hlib,'gluGetString');
      gluGetTessProperty :=GetProcAddress(hlib,'gluGetTessProperty');
      gluLoadSamplingMatrices :=GetProcAddress(hlib,'gluLoadSamplingMatrices');
      gluLookAt :=GetProcAddress(hlib,'gluLookAt');
      gluNewNurbsRenderer :=GetProcAddress(hlib,'gluNewNurbsRenderer');
      gluNewQuadric :=GetProcAddress(hlib,'gluNewQuadric');
      gluNewTess :=GetProcAddress(hlib,'gluNewTess');
      gluNextContour :=GetProcAddress(hlib,'gluNextContour');
      gluNurbsCallback :=GetProcAddress(hlib,'gluNurbsCallback');
      gluNurbsCallbackData :=GetProcAddress(hlib,'gluNurbsCallbackData');
      gluNurbsCallbackDataEXT :=GetProcAddress(hlib,'gluNurbsCallbackDataEXT');
      gluNurbsCurve :=GetProcAddress(hlib,'gluNurbsCurve');
      gluNurbsProperty :=GetProcAddress(hlib,'gluNurbsProperty');
      gluNurbsSurface :=GetProcAddress(hlib,'gluNurbsSurface');
      gluOrtho2D :=GetProcAddress(hlib,'gluOrtho2D');
      gluPartialDisk :=GetProcAddress(hlib,'gluPartialDisk');
      gluPerspective :=GetProcAddress(hlib,'gluPerspective');
      gluPickMatrix :=GetProcAddress(hlib,'gluPickMatrix');
      gluProject :=GetProcAddress(hlib,'gluProject');
      gluPwlCurve :=GetProcAddress(hlib,'gluPwlCurve');
      gluQuadricCallback :=GetProcAddress(hlib,'gluQuadricCallback');
      gluQuadricDrawStyle :=GetProcAddress(hlib,'gluQuadricDrawStyle');
      gluQuadricNormals :=GetProcAddress(hlib,'gluQuadricNormals');
      gluQuadricOrientation :=GetProcAddress(hlib,'gluQuadricOrientation');
      gluQuadricTexture :=GetProcAddress(hlib,'gluQuadricTexture');
      gluScaleImage :=GetProcAddress(hlib,'gluScaleImage');
      gluSphere :=GetProcAddress(hlib,'gluSphere');
      gluTessBeginContour :=GetProcAddress(hlib,'gluTessBeginContour');
      gluTessBeginPolygon :=GetProcAddress(hlib,'gluTessBeginPolygon');
      gluTessCallback :=GetProcAddress(hlib,'gluTessCallback');
      gluTessEndContour :=GetProcAddress(hlib,'gluTessEndContour');
      gluTessEndPolygon :=GetProcAddress(hlib,'gluTessEndPolygon');
      gluTessNormal :=GetProcAddress(hlib,'gluTessNormal');
      gluTessProperty :=GetProcAddress(hlib,'gluTessProperty');
      gluTessVertex :=GetProcAddress(hlib,'gluTessVertex');
      gluUnProject :=GetProcAddress(hlib,'gluUnProject');
      gluUnProject4 :=GetProcAddress(hlib,'gluUnProject4');
    end;


initialization
  LoadGLu(GLU_LIB);
finalization
  Freeglu;
end.
