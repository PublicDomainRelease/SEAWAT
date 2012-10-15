INSTRUCTIONS FOR COMPILING USING THE PROVIDED MAKEFILE

This folder contains a makefile that can be used to compile SEAWAT.  To compile using
Intel Visual Fortran, for example, copy the Windows command line short cut that can
be found under the "START > All Programs > Intel Software Development Tools > Intel 
visual Fortran Compiler > FORTRAN Build Environment for applications .." into this
make folder.  Then type NMAKE at the command line and SEAWAT should compile.

Note: to create a 64-bit executable, it may be necessary to modify the gmg1.f source
file located in the ../Source directory.  Instructions appear in the file as:

C--SEAWAT: NOTE THAT TO COMPILE FOR X64, USE THE SECOND DEC ATTRIBUTE STATEMENT
      !DEC$ ATTRIBUTES ALIAS:'_resprint' :: RESPRINT
c      !DEC$ ATTRIBUTES ALIAS:'resprint' :: RESPRINT


This folder also contains Makefile.mac, which was provided by Theo Olsthoorn for
compiling SEAWAT with gfortran on a mac.  Note that when compiling with some
operating systems and some compilers, it may be necessary to edit some of the 
source files.  For example, changes may be required to openspec.inc and filespec.inc
to ensure that binary files are handled correctly.

