README.TXT

SEAWAT - Version: 2.1 01/14/2001
Three-dimensional variable-density finite-difference ground-water flow model

NOTE: Any use of trade, product or firm names is for descriptive purposes only 
and does not imply endorsement by the U.S. Government.

This version of SEAWAT is packaged for personal computers using Microsoft 
Windows 95, 98, ME, NE, or 2000.

See the file doc/seawat.txt for descriptions, references, and additional 
contacts for this software.  Instructions for installation, execution, and 
testing are provided below.

TABLE OF CONTENTS

A. DISTRIBUTION FILE
B. EXTRACTING FILES
C. COMPILING
D. INSTALLING
E. RUNNING THE SOFTWARE
F. TESTING

A. DISTRIBUTION FILE
The following zip file is for use on personal computers:

     SEAWAT2_1.zip

The distribution file contains:
     Compiled runfiles and source code for SEAWAT
     SEAWAT user guide in pdf format
     Test data sets
     Animations of selected test problems

B. EXTRACTING FILES
The distribution file is a zipped file that contains all of the necessary files 
for using the SEAWAT program.  Unzipping the distribution file creates numerous 
individual files.  An extraction program allows you to specify the directory in 
which the files should be restored.  The following subdirectory structure will 
be created under the specified directory:

|
|--seawat2_1
|     |animations; animation files
|     |doc; documentation files
|     |examples; example files 
|     |     |boxes; test cases for box problems
|     |     |     |case1; first box problem
|     |     |     |case2; second box problem
|     |     |elder; test case for elder problem
|     |     |henry; test case for henry problem
|     |     |     |case1; first henry problem
|     |     |     |case2; second henry problem
|     |     |hydrocoin; test case for hydrocoin problem
|     |     |saltlake; test case for saltlake problem
|     |exe; compiled executables
|     |source; source code

It is recommended that no user files are kept in the seawat2_1 directory 
structure.  If you do plan to put files in the seawat2_1 directory structure, do 
so only by creating subdirectories.

Included in directory seawat2_1\doc is Portable Document Format (PDF) version of 
the SEAWAT user's manual (OFR 01-434).

The PDF file is readable and printable on various computer platforms using 
Acrobat Reader from Adobe.  The Acrobat Reader is freely available from the 
following World Wide Web sites:
     http://www.adobe.com/
     http://www.shareware.com

C. COMPILING

Although executable versions of the programs are provided, the source code of 
the SEAWAT program is provided in the seawat2_1\source directory so that the 
program can be recompiled if necessary.  However, no support can be provided for 
users generating their own versions of the software.  In general, the 
requirements are a Fortran compiler and the knowledge of using the compiler.  

D. INSTALLING

There are no special instructions for installing the SEAWAT program provided 
that users run the program using the approach outlined in the SEAWAT 
documentation.  

E. RUNNING THE SOFTWARE

The data arrays in SEAWAT are dynamically allocated, so models are not limited 
by hard-coded array limits. However, it is best to have enough random-access 
memory (RAM) available to hold all of the required data.  If there is less 
available RAM than this, the program will use virtual memory, but this slows 
computations significantly.

F. TESTING

The benchmark problems reported in the SEAWAT documentation are included to test 
the program and ensure that the program is installed and running on the system.  
The benchmark problems also provide examples of how to design datasets for use 
with the SEAWAT program.  Benchmark problems can be run by double clicking on 
the seawat.bat file located within each of the benchmark subdirectories.
