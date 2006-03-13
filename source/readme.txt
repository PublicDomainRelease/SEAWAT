Coding issues with SEAWAT2000

MT3DMS FORTRAN variables in the main program with names that conflict with
MODFLOW-2000 variables:

IOBS	IMOBS
ISUM	ISUMY
ISUM2	ISUMIY
LCDELR	LTDELR
LCDELC	LTDELC
LCBUFF	LTBUFF
LCIRCH	LTIRCH
LCRECH	LTRECH
LCCRCH	LTCRCH
LCIEVT	LTIEVT
LCEVTR	LTEVTR
LCCEVT	LTCEVT
IERR    IMT3DERR
LCSS	LCSSM
ICNVG	ICNVGMT
MXITER	MXITERGC
ITER1	ITER1GC
LCA	LCAGC
LCLRCH	LCLRCHGC
LCRHS	LCRHSGC
ACCL	ACCLGC
All references to X(...) are changed to Y(...)
All references to IX(...) are changed to IY(...)
Change COMMMON /FC/ to COMMON /FCMT3D/
Add IMT1 to beginning of all primary subroutine names


The variable ILMTFMT is used by LMT subroutines to determine format of output.
For SEAWAT, ILMTFMT should be 0 whenever writing to $file.umt.
However, when saving fluxes for subsequent run, ILMTFMT can be any value.

Conversely, the variable IFTLFMT should always be zero.  This value is only used by FMI
subroutines to read information from $file.umt.

common statements for both:
COMMON /LINKMT3D/ILMTFMT
COMMON /FTL/IFTLFMT

Note that the common state /LINKMT3D/ILMTFMT had to be added to the main program to store
and then reset value of ILMTFMT.



______________________________________________________________________________________________________________




3/3/05 updating swt2k with mf2k version 1.15.00


The following are the results from a search for 'seawat' in the imt_*.f files:

----------------------------------------
Find 'seawat' in 'D:\Users\langevin\seawat2k\swt2000\source\v3_11_20050303_public\imt1utl4.for' :
D:\Users\langevin\seawat2k\swt2000\source\v3_11_20050303_public\imt1utl4.for(1): C--SEAWAT: REMOVED SUBROUTINES UCOLNO AND URWORD.  CONTAINED IN MODFLOW-2000 SOURCE
D:\Users\langevin\seawat2k\swt2000\source\v3_11_20050303_public\imt1utl4.for(2): C--SEAWAT: MODIFED RARRAY TO READ SINGLE PRECISION BINARY ARRAYS
D:\Users\langevin\seawat2k\swt2000\source\v3_11_20050303_public\imt1utl4.for(476): C--SEAWAT: NEED REAL*4 VARIABLE TO READ SINGLE PRECISION FROM BINARY FILE
D:\Users\langevin\seawat2k\swt2000\source\v3_11_20050303_public\imt1utl4.for(615): C--SEAWAT: MODIFIED THIS SECTION TO READ SINGLE PRECISION BINARY ARRAY
D:\Users\langevin\seawat2k\swt2000\source\v3_11_20050303_public\imt1utl4.for(616): C--SEAWAT:        READ(-IREAD) NTRANS,KSTP,KPER,TOTIM,TEXT,NC,NR,ILAY
D:\Users\langevin\seawat2k\swt2000\source\v3_11_20050303_public\imt1utl4.for(617): C--SEAWAT:        READ(-IREAD) A
D:\Users\langevin\seawat2k\swt2000\source\v3_11_20050303_public\imt1utl4.for(618): C--SEAWAT: THIS SECTION MAY NEED TO BE CHANGED IF FORM='UNFORMATTED'

According to this search, there are seawat modifications in the imt1utl4.for subroutine.  In addition to these modifications, there are some minor differences
between utl4 and utl5:
	1.  utl5 does not have the subroutine namefile.  this is now contained in subroutine btn5open
	2.  All STOP commands are now CALL USTOP(' ')
	3.  Dates on subroutines are changed to 02-15-2005

To implement these changes, I will rename imt1utl4.for to imt1utl5.for and make the 3 changes shown above.


Found 'seawat' 7 time(s).
----------------------------------------
Find 'seawat' in 'D:\Users\langevin\seawat2k\swt2000\source\v3_11_20050303_public\imt1btn4.for' :
D:\Users\langevin\seawat2k\swt2000\source\v3_11_20050303_public\imt1btn4.for(643): C--SEAWAT: INTEGER FIRSTDT
D:\Users\langevin\seawat2k\swt2000\source\v3_11_20050303_public\imt1btn4.for(703): C--SEAWAT: IF DTRANS IS ZERO, SET TO FIRSTDT
D:\Users\langevin\seawat2k\swt2000\source\v3_11_20050303_public\imt1btn4.for(740): C--SEAWAT: FOR NOW, COMMENT FOLLOWING LINES SO RHS IS ALWAYS UPDATED
D:\Users\langevin\seawat2k\swt2000\source\v3_11_20050303_public\imt1btn4.for(741): C--SEAWAT:      IF(ABS(DTRANS-DTOLD).LT.ABS(DTRANS+DTOLD)*EPSILON
D:\Users\langevin\seawat2k\swt2000\source\v3_11_20050303_public\imt1btn4.for(742): C--SEAWAT:     & .AND. NTRANS.GT.1) UPDLHS=.FALSE.
D:\Users\langevin\seawat2k\swt2000\source\v3_11_20050303_public\imt1btn4.for(1040): C--SEAWAT: CHANGED COMMON NAME FC TO FCMT3D TO AVOID CONFLICT WITH SUBROUTINE
D:\Users\langevin\seawat2k\swt2000\source\v3_11_20050303_public\imt1btn4.for(1085): C--SEAWAT CHANGED FOLLOWING TWO STATEMENTS TO WRITE SINGLE PRECISION

Steps for converting btn file that contains btn subroutines
	1.  I replaced all subroutine names with BTN5 to IMT1BTN5
	2.  I made all of the changes shown above in the search for SEAWAT in imt1btn4.for (and found 1 more that converted sorbed concentrations to single precision)



Found 'seawat' 7 time(s).
----------------------------------------
Find 'seawat' in 'D:\Users\langevin\seawat2k\swt2000\source\v3_11_20050303_public\imt1fmi4.for' :
D:\Users\langevin\seawat2k\swt2000\source\v3_11_20050303_public\imt1fmi4.for(17): C--SEAWAT: CHANGED COMMON NAME FC TO FCMT3D TO AVOID CONFLICT WITH SUBROUTINE
D:\Users\langevin\seawat2k\swt2000\source\v3_11_20050303_public\imt1fmi4.for(515): C--SEAWAT: CHANGED COMMON NAME FC TO FCMT3D TO AVOID CONFLICT WITH SUBROUTINE

Steps for converting fmi file that contains fmi subroutines
	1.  Replaced all FMI5 with IMT1FMI5 in subroutine calls
	2.  Changed all common /fc/ to common /fcmt3d/


Found 'seawat' 2 time(s).
----------------------------------------
Find 'seawat' in 'D:\Users\langevin\seawat2k\swt2000\source\v3_11_20050303_public\imt1ssm4.for' :
D:\Users\langevin\seawat2k\swt2000\source\v3_11_20050303_public\imt1ssm4.for(17): C--SEAWAT: CHANGED COMMON NAME FC TO FCMT3D TO AVOID CONFLICT WITH SUBROUTINE
D:\Users\langevin\seawat2k\swt2000\source\v3_11_20050303_public\imt1ssm4.for(135): C--SEAWAT: CHANGED COMMON NAME FC TO FCMT3D TO AVOID CONFLICT WITH SUBROUTINE
D:\Users\langevin\seawat2k\swt2000\source\v3_11_20050303_public\imt1ssm4.for(322): C--SEAWAT: CHANGED COMMON NAME FC TO FCMT3D TO AVOID CONFLICT WITH SUBROUTINE
D:\Users\langevin\seawat2k\swt2000\source\v3_11_20050303_public\imt1ssm4.for(479): C--SEAWAT: CHANGED COMMON NAME FC TO FCMT3D TO AVOID CONFLICT WITH SUBROUTINE
D:\Users\langevin\seawat2k\swt2000\source\v3_11_20050303_public\imt1ssm4.for(601): C--SEAWAT: CHANGED COMMON NAME FC TO FCMT3D TO AVOID CONFLICT WITH SUBROUTINE
D:\Users\langevin\seawat2k\swt2000\source\v3_11_20050303_public\imt1ssm4.for(760): C--SEAWAT: CHANGED COMMON NAME FC TO FCMT3D TO AVOID CONFLICT WITH SUBROUTINE

STeps for converting SSM file that contains ssm subroutines
	1.  rename all ssm5 to imt1ssm5
	2.  change all common /fc/ to common /fcmt3d/

Found 'seawat' 6 time(s).
Search complete, found 'seawat' 22 time(s). (4 files.)


The MT3DMS namefile is read in swt2k using the SGLO1BAS6OPEN subroutine, instead of the subroutine that comes with MT3DMS.

______________________________________________________________________________________________________________
4/7/2005 Adding compatability bewteen MNW and VDF

2 limitations with MNW--heads used in conductance are equivalent freshwater values and all concentrations are
   assumed to be zero (is only highly accurate for wells pumping water of low concentration)

IN swt2k.f
	1.  Add a call to vdf1mnw1fm
	2.  Add a call to lmt6mnw1fm   both in swt2k.f and in lmt6vd.inc  !!
	3.  Add a call to vdf1mnw1bd


In VDF1MNW1.F
	1.  in FM routine, multiplied HCOF and RHS additions by denseref
	2.  in BD routine, multiplied terms added to ratin and ratout by denseref




______________________________________________________________________________________________________________
3/14/2006 Updating with MT3DMS version 5.1

Main program--no differences
BTN
	BTN5OT--slight changes
ADV--no differences
DSP--no differences
SSM
	SSM5FM--changed
	SSM5BD--changed
GCG--no differences
RCT
	RCT5AL
	RCT5RP
	SRCT5R
	RCT5FM
	RCT5BD
TOB--no difference
UTL--no difference
FMI--no difference












