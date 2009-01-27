CentrED (c) 2009 Andreas Schneider
==================================

License
=======
CentrED is released under the CDDL - the Common Development and
Distribution License by Sun Microsystems. A copy of that license
should be included with this source, but you can also read it
at http://www.opensource.org/licenses/cddl1.php.

Compiling
=========
To build CentrED you need at least FreePascal 2.2.2 and a
recent Lazarus (at least 0.9.26).
You also have to have the following packages installed:
  - VirtualTrees from http://code.google.com/p/luipack/wiki/VirtualTreeview
  - LazOpenGLContext (included with Lazarus)
  - lNet by Almindor: http://wiki.lazarus.freepascal.org/lNet

The client project can be found in Client/CentrED.lpi and the
server projectin Server/cedserver.lpi, both to be opened by
Lazarus or compiled via lazbuild.
Currently supported targets are i386-linux-gtk and
i386-win32-win32. Other targets might work too, but I haven't
tested or approved them. Don't forget to create the necessary
target directories in obj/ first, before trying other targets.

Structure
=========
./		contains generic source files used by the client and server
bin/		contains the linked binaries
Client/		contains the source of the CentrED client
Imaging/	contains the Vampyre Imaging Library with CentrED specific options
MulProvider/	contains my MulProvider library with some wrapper classes around the mul objects
obj/		contains the compiled objects
Server/		contains the Source of the CentrED server
Setup/		contains the InnoSetup project file(s)
UOLib/		contains my UO library for access to many mul files

Thanks
======
Almindor for lNet and his support on how to use it efficently.
Marek Mauder for the great Vampyre Imaging Library and also his support and extensions.
Luiz Américo Pereira Câmara for his nice port of VirtualTrees.
And last but not least the whole FPC and Lazarus team for developing, maintaining
and supporting such great development tools, which make life a lot easier.
