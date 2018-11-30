TJPEGImage component version 1.3       Luciano Bajo Eloy - Sep 9, 1997
                                       Madrid, Spain
                                       E-mail: lbe20@tid.es

                                       Andreas Hoerstemeier - May 17, 1998
                                       Aachen, Germany
                                       E-Mail: andy@hoerstemeier.de
                                       WWW: http://www.westend.de/~hoerstemeier
-------------------------------------------------------------------------------
Note on Version 1.3: This version now adds the ability to save the JPEG
                     as well as a TDBJPEGImage.  For details see the
                     New Features section.
-------------------------------------------------------------------------------
Important note: I (Luciano) have changed my job. I do not work with Delphi
                currently, I use Visual C++ (I hate it, but such is life).
                That means that I cannot reply several mails a day pertaining
                to TJPEGImage. Instead I have included the source code. If
                you have any problems, you will have to modify the code by
                yourself. Please, do not send me E-mails unless it is truly
                necessary.

Note on installation: Select the file JpgImgR.pas when installing the component

-------------------------------------------------------------------------------
Index:
        0. Answers to common questions and requests
        1. New features (from version 1.0 to 1.1 and 1.2)
        2. Introduction
        3. JPEGImage properties and events
        3a. DBJPEGImage additional properties and events
        4. Remarkable features
        5. Known limitations (future improvements?)
        6. Distribution and source code
        7. Information and libraries on JPEG compression

-------------------------------------------------------------------------------
0. Answers to common questions and requests
-------------------------------------------------------------------------------
        Basically, all of the E-mails I have received pertaining to TJPEGImage
fall into one of these topics:

        -Requests for source code. Well, I did not provide the source code
with TJPEGImage 1.0 (I sended it on request) as a mean to know if the
component was being used. The result is that many people have requested it
and I have had to send several mails a day with the code attached. To avoid
this, I provide the full source code with this version.

        -Support for Delphi 3.0. Delphi 3.0 adds support for JPEG files so
TJPEGImage is no longer required. Drop a TImage on a form (Image1, for
example), add jpeg.pas to the uses clause and load a JPEG file with
Image1.Picture.LoadFromFile. However this component also runs with D3 since
V1.2, only a bit slower than the Delphi one.

        -Loading pictures from a stream. TJPEGImage does not support loading
pictures from a stream. If you need that feature you will have to modify
the code, I think it will not be difficult. (Obsolete, added with V1.2)

        -Coding JPEG images. TJPEGImage is a decoder, not a coder. That means
that you can view JPEG files, but you cannot save files as JPEG. (Obsolete,
added with V1.3).


-------------------------------------------------------------------------------
1. New features (from version 1.0 to 1.1 and 1.2)
-------------------------------------------------------------------------------
From Version 1.0 to 1.1

        -Finally, FileName has its own property editor by courtesy of Keith
Anderson, who sent me the code by E-mail.

        -A new property and a new event have been added for indicating the
progress of the decompression (see section 2 for details).

        -A bug has been fixed. When a corrupted JPEG file was loaded into
the component the file was being left open. Any further action on this file
generated a Sharing Violation exception (IOResult = 32). The bug has been
reported and fixed by Derek (sorry, I do not know your last name).


From Version 1.1 to 1.2 (changes done by Andreas Hoerstemeier)

        -The component can now also load the image from any stream, it now has
a LoadFromStream method, so it can also be used to display files out of a
database (TBlobStream) or downloaded from the web without the need of a
temporary file.

        -As the whole PasJPG library didn't include any 32bit specific parts
the component could be modified easily for Delphi 1 (but note it is slow there
compared to Delphi 2), and in the same way to work with Delphi 3, too. It may
also run in C++ Builder, but I've no chance of testing this.

From version 1.2 to 1.3 (changes done by Andreas Hoerstemeier)

        -The coding part of the JPEG library is now also included and
connected to the component.

        -A new TDBJPEGImage is included which allows to you to link a BLOB
field containing JPEG images to a visual control easily.

From version 1.3 to 1.4 (changes done by Andreas Hoerstemeier)

        -New property Comment to link to the optional comment within the
JPEG file.

-------------------------------------------------------------------------------
2. Introduction
-------------------------------------------------------------------------------
        JPEGImage is based on the work of NOMSSI NZALI Jacques H. C., author
of the PASJPEG library (http://www.tu-chemnitz.de/~nomssi/pub/pasjpeg.zip).
On his own words, PASJPEG is a port of the sixth public release of the IJG
(Independent JPEG Group) C source (release 6a of 7-Feb-96), that implements
JPEG baseline, extended-sequential, and progressive compression processes to
Turbo Pascal 7.0 for DOS (TP).

        My goal was to get a Delphi component for decompressing and viewing
JPEG images. So I began from DJPEG.PAS, a DOS program written by Thomas G.
Lane included in the PASJPEG library for translating JPEG files to BMP or
TARGA files. My work has consisted of the following steps:

        1. Create a new component inherited from TImage.
        2. Change command line options in DJPEG to properties in TJPEGImage.
        3. Fit the output in a memory stream instead of a disk file.
        4. Change WriteLn and Halt procedures to Delphi exceptions.
        5. Put the library on a diet, that is, delete unnecessary files and
           code.

        Once those steps where covered, I found a problem in 256 colors video
mode: the first image loaded had the right colors, but successive images
caused the color palette to become corrupted. Fortunately, I was a little
luck and I could solve the problem.


-------------------------------------------------------------------------------
3. JPEGImage properties and events
-------------------------------------------------------------------------------
        The properties of TJPEGImage are (apart from those in TImage):

        -FileName: Name of the JPEG file.
        -NumOfColors: Colors of the resulting image. Possible values are _16,
                _256, or TrueColor. DO NOT use TrueColor in 256 video modes or
                the colors in the image will become distorted. (Default 256).
                Not used for saving as JPEG images can only be true color or
                grayscale.
        -DCTMethod: Method used for the Discrete Cosine Transform in JPEG
                algorithms. Possible values are Integ (default), FastInteg
                (faster but less accurate) or FloatingPoint (more accurate
                but slower).
        -Dither: Possible values are none (do not use dithering in
                quantization), FS (default) or ordered (medium speed, quality).
                Not for saving.
        -GrayScale: Force grayscale output (default False).
        -NoSmooth: Do not use high-quality upsampling (default False).
                   Loading only.
        -OnePass: Use 1-pass quantization (fast, low quality, default False).
                  Loading only.
        -ProgressStep: Indicates how frequently the OnProgress event is fired.
                For example, if ProgressStep is 10 (default) the OnProgress
                event is fired when the image decompression reaches 10%, 20%
                and so on. If ProgressStep is 5 the event is fired in 5%, 10%,
                15%, ...
        -Quality: The image quality when saving the image. Allowed range is 0
                  to 100, although a reasonable value is around 70. Note that
                  with too low quality values the coding may fail and raise an
                  exception.
        -Progressive: Save the image in progressive mode. Useful for images to
                      be put on a webpage to allow a low-quality image to be
                      shown before the whole image data is transferred. Note
                      some older JPEG viewers may not support this encoding.

        And the new event:

        -OnProgress: It is fired every time the image decompression is
                increased by ProgressStep as shown above.

        Useful methods are
        -LoadFromStream():  Load a JPEG from any stream
        -LoadFromFile(): just to keep it complete, same as a filename:='xyz'
        -SaveToStream(): Save the JPEG to any stream
        -SaveToFile(): Save the JPEG to a file

        Note that due to the fact JPEG is lossy the SaveToFile/Stream routine
        will never return the same data as inserted into the image, with
        several load/save cycles the JPEG artefacts may even add up and the
        image will get worse.

-------------------------------------------------------------------------------
3a. DBJPEGImage properties and events
-------------------------------------------------------------------------------
        The properties of TDBJPEGImage has the same properties as TJPEGImage
        (except the filename), and these new ones:

        -AutoDisplay: Call the LoadImage method automatically when the data
                      has changed.
        -DataSource, DataField: Just as in all other data-aware elements. The
                                field should be on type TBlobField.

        and the following methods

        -LoadImage: load the image from the current record and display it
        -DBLoadFromFile,
         DBLoadFromStream: load an image from a stream/file and put exactly
                           the same data into the database
        -DBSaveToFile,
         DBSaveToStream: save the exact image data from the database into a
                         stream/file. These routines set the field into edit
                         mode and displays the new image if autodisplay=true.
                         However it doesn't call the post automatically.


        While TJPEGImage save the contents of the picture property in the DFM
        file the TDBJPEGImage doesn't - it doesn't make sense as the image is
        loaded from the database anyways.

        As above the SaveToFile/Stream routine won't return the same data as
        inside the database, the same way a JPEG file loaded into a
        TDBJPEGImage won't be the same in the database as the file. To get the
        exact data in/out the database you need to use a TBlobStream directly.

-------------------------------------------------------------------------------
4. Remarkable features
-------------------------------------------------------------------------------
        No DLL's are required.

        JPEGImage displays 256-colors images in 256-colors video modes without
changing the original colors.

        JPEGImage supports more formats than many programs (for instance,
Imaging cannot open the file nonint.jpg supplied with the JPEGImage demo).

        Error messages can be overriden (jdeferror.pas).

        Since TJPEGImage is inherited from TImage it can be used for viewing
BMP files if its Picture property is assigned.


-------------------------------------------------------------------------------
5. Known limitations (future improvements?)
-------------------------------------------------------------------------------
        I apologize for some limitations of the component I am aware of, but
this component has been written in my free time and as usual: my free time is
short and I often prefer to spend it in other things different of programming.
A couple of them:

        -(FileName has to be assigned as a string, instead of using a property
editor. Well, I am completely new to property editors so, if somebody helps me
with source code samples or similar I would try to write one) -> CORRECTED.

        -Due to a limitation in my code, the only ways to clear a image are:
in desing mode, double-clicking in Picture property and selecting Clear button;
by code, doing Picture.Bitmap.Assign(nil); or Picture.Bitmap:=NIL;


-------------------------------------------------------------------------------
6. Distribution and source code
-------------------------------------------------------------------------------
        Feel free to use this component in your applications, either comercial
or not. Since this component is based on PASJPEG library, I would like to
remember some comments of his author:

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the author be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.

        I woulk like to remark that the code of TJPEGImage is hardly an
example of good programming. You must realise that is based on a non
object-oriented code at the same time it adds advanced Delphi object-oriented
features as exceptions and VCL methods and properties mixed together.

-------------------------------------------------------------------------------
7. Information and libraries on JPEG compression
-------------------------------------------------------------------------------
1. A good document describing the JPEG compression standard:
        Wallace, Gregory K.: The JPEG Still Picture Compression Standard
        Location: ftp://ftp.uu.net/graphics/jpeg/wallace.ps.Z

2. The Independent JPEG Group C library for JPEG encoding and decoding, rev 6a.
        ftp://ftp.uu.net/graphics/jpeg/

3. The PASJPEG library, origin for the JPEGImage component:
        http://www.tu-chemnitz.de/~nomssi/pub/pasjpeg.zip
