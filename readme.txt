README.TXT

SEAWAT-2000 - Version: 3.10 02/13/2004
Three-dimensional variable-density finite-difference ground-water flow model

NOTE: Any use of trade, product or firm names is for descriptive purposes only 
and does not imply endorsement by the U.S. Government.

This version of SEAWAT is packaged for personal computers using Microsoft 
Windows 95, 98, ME, NT, 2000, or XP.

See the file doc/seawat.txt for descriptions, references, and additional 
contacts for this software.  Instructions for installation, execution, and 
testing are provided below.

This version of SEAWAT is referred to as SEAWAT-2000 to distinguish it from
older versions.  See the file doc\seawat.txt for descriptions, references, and 
additional contacts for this software.  Instructions for installation, 
execution, and testing are provided below.

TABLE OF CONTENTS

A. DISTRIBUTION FILE
B. EXTRACTING FILES
C. COMPILING
D. INSTALLING
E. RUNNING THE SOFTWARE
F. TESTING

A. DISTRIBUTION FILE
The following zip file is for use on personal computers:

     SWT2K_V3_10.zip

The distribution file contains:
     Compiled runfiles and source code for SEAWAT
     SEAWAT user guide in pdf format
     Test data sets

B. EXTRACTING FILES
The distribution file is a zipped file that contains all of the necessary files 
for using the SEAWAT program.  Unzipping the distribution file creates numerous 
individual files.  An extraction program allows you to specify the directory in 
which the files should be restored.  The following subdirectory structure will 
be created under the specified directory:

|
|--swt2k_v3_10
|     |doc; documentation files
|     |examples; example files 
|     |     |1_box; test cases for box problems
|     |     |     |case1; first box problem
|     |     |     |case2; second box problem
|     |     |2_henry; test case for henry problem
|     |     |     |1_classic_case1; first henry problem
|     |     |     |2_classic_case2; second henry problem
|     |     |     |3_VDF_no_Trans; henry problem without transport
|     |     |     |4_VDF_uncpl_Trans; henry problem with uncoupled transport
|     |     |     |5_VDF_DualD_Trans; henry problem with dual domain transport
|     |     |3_elder; test case for elder problem
|     |     |4_hydrocoin; test case for hydrocoin problem
|     |     |5_saltlake; test case for saltlake problem
|     |     |6_rotation; test case for rotation problem
|     |exe; compiled executables
|     |source; source code

It is recommended that no user files are kept in the swt2k_v3_10 directory 
structure.  If you do plan to put files in the swt2k_v3_10 directory structure, do 
so only by creating subdirectories.

Included in directory swt2k_v3_10\doc is Portable Document Format (PDF) version of 
the SEAWAT user's manual (OFR 01-434).

The PDF file is readable and printable on various computer platforms using 
Acrobat Reader from Adobe.  The Acrobat Reader is freely available from the 
following World Wide Web sites:
     http://www.adobe.com/
     http://www.shareware.com

C. COMPILING

Although executable versions of the programs are provided, the source code of 
the SEAWAT program is provided in the swt2k_v3_10\source directory so that the 
program can be recompiled if necessary.  However, no support can be provided for 
users generating their own versions of the software.  In general, SEAWAT-2000 
should be compiled for serial processing (as opposed to parallel processing).
For serial-processing mode, all source files are contained in the source directory
except one.  In addition to the files in the source directory, the file para-non.f 
in the source/serial directory must be compiled.

IMPORTANT: For the SEAWAT-2000 program to operate correctly, the program must be
compiled such that all real variables are defined as double precision.  The program 
may not work correctly using the default option (single precision) in most compilers.

D. INSTALLING

There are no special instructions for installing the SEAWAT program provided 
that users run the program using the approach outlined in the SEAWAT-2000 
documentation.  

E. RUNNING THE SOFTWARE

The data arrays in SEAWAT are dynamically allocated, so models are not limited 
by hard-coded array limits. However, it is best to have enough random-access 
memory (RAM) available to hold all of the required data.  If there is less 
available RAM than this, the program will use virtual memory, but this slows 
computations significantly.

The swt2k runfile for use on personal computers uses a different 
structure for unformatted files than has been used in earlier versions 
of MODFLOW distributed by the USGS.  Unformatted files generally have 
a structure that is compiler specific.  MODFLOW-2000 Versions prior to
1.2 that were distributed by the USGS used a structure that was
specific to Lahey 77 and 90 Fortran.  This required that any program
that read unformatted files produced by these MODFLOW runfiles or any
program that generated unformatted files for use by MODFLOW had to be
compiled with one of these Lahey compilers.  For example, Zonebudget
and Modpath use unformatted budget files produced by MODFLOW.  Another
example is head files that are generated by one MODFLOW simulation and
used in a following simulation as initial heads.  Both simulations must
be run using a version of MODFLOW or SEAWAT that uses the same unformatted 
file structure.

The structure of unformatted files used in SEAWAT-2000 is the same as that
used in the previous versions of SEAWAT.  The structure is specified using
the FORM='BINARY' option with the file OPEN statement.  Therefore it will
be easier for others to use different compilers when compiling
applications that use or generate unformatted files.

The swt2k runfile in the exe subdirectory was compiled with the Compaq Visual
FORTRAN compiler (Version 6.6B).

F. TESTING

The benchmark problems reported in the SEAWAT documentation are included to test 
the program and ensure that the program is installed and running on the system.  
The benchmark problems also provide examples of how to design datasets for use 
with the SEAWAT program.  Benchmark problems can be run by double clicking on 
the seawat.bat file located within each of the benchmark subdirectories.

