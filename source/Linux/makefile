# Makefile for USGS distribution only
#
# @ (#) Makefile for Modtools 27June1997
# (Supplied by L. Orzol -- erb)
#
SOURCELOC = 
UTILITYLOC = 
NEWMOD = 
PROGRAM = swt2k
INCDIR= .

# Define the Fortran compile flags

# uncomment the 2 lines below for intel fortran compiler (ifc)
F77FLAGS= -O3 -tpp7 -mp -r8 -Vaxlib -w -cm  
F77= ifc
# uncomment the 2 lines below for Lahey fortran compiler (lf95)
#F77FLAGS= --O3 --dbl 
#F77= /usr/local/lf95/bin/lf95

# 
# Define the C compile flags
# -D_UF defines UNIX naming conventions for mixed language compilation.
# 
CFLAGS= -D_UF -O3
CC= cc

# Define GMG objects
#
GMG = r_vector.o\
      solvers.o\
      ccfd.o\
      mf2kgmg.o\
      gmg1.o


# Define the libraries

#SYSLIBS= -lmisalign -ldgc -lm 
#SYSLIBS= -lc
#USRLIB  = gmg1.a

# Define all object files which make up Modtools

OBJECTS = \
	swt2k.o \
	amg1r6.o \
	ctime.o \
	daf1.o \
	de45.o \
	glo1bas6.o \
	gutsdaf.o \
	gwf1bas6.o \
	gwf1bcf6.o \
	gwf1chd6.o \
	gwf1drn6.o \
	gwf1drt1.o \
	gwf1ets1.o \
	gwf1evt6.o \
	gwf1fhb1.o \
	gwf1gag5.o \
	gwf1ghb6.o \
	gwf1hfb6.o \
	gwf1huf2.o \
	gwf1ibs6.o \
	gwf1lak3.o \
	gwf1lpf1.o \
	gwf1mnw1.o \
	gwf1rch6.o \
	gwf1res1.o \
	gwf1riv6.o \
	gwf1sfr1.o \
	gwf1str6.o \
	gwf1sub1.o \
	gwf1wel6.o \
	hufutl2.o \
	hydmod.o \
	lmg1.o \
	lmt6.o \
	memchk.o \
	obs1adv2.o \
	obs1bas6.o \
	obs1drn6.o \
	obs1drt1.o \
	obs1ghb6.o \
	obs1riv6.o \
	obs1str6.o \
	parutl1.o \
	pcg2.o \
	pes1bas6.o \
	pes1gau1.o \
	rtedaf.o \
	sen1bas6.o \
	sen1chd6.o \
	sen1drn6.o \
	sen1drt1.o \
	sen1ets1.o \
	sen1evt6.o \
	sen1ghb6.o \
	sen1hfb6.o \
	sen1huf2.o \
	sen1lpf1.o \
	sen1rch6.o \
	sen1riv6.o \
	sen1str6.o \
	sen1wel6.o \
	sip5.o \
	sor5.o \
	utl6.o \
	imt1adv5.o \
	imt1btn5.o \
	imt1dsp5.o \
	imt1fmi5.o \
	imt1gcg5.o \
	imt1rct5.o \
	imt1ssm5.o \
	imt1tob5.o \
	imt1utl5.o \
	obs1bas6vd.f \
	obs1drn6vd.f \
	obs1ghb6vd.f \
	obs1riv6vd.f \
	vdf1bas6.o \
	vdf1bcf6.f \
	vdf1chd6.f \
	vdf1drn6.f \
	vdf1evt6.f \
	vdf1.f \
	vdf1fhb1.o \
	vdf1ghb6.o \
	vdf1huf2.o \
	vdf1lmt6.o \
	vdf1lpf1.o \
	vdf1mnw1.o \
	vdf1rch6.o \
	vdf1riv6.o \
	vdf1wel6.o \
	para-non.o

install: swt2k

# Define Task Function Program Modtools

all: swt2k

# Define what Modtools is

swt2k: $(OBJECTS) $(GMG)
	-$(F77) $(F77FLAGS) -o swt2k $(OBJECTS) $(GMG) $(USRLIB) $(SYSLIBS)

# Modtools_Object codes

swt2k.o: swt2k.f
	$(F77) $(F77FLAGS) -c swt2k.f


# PARALLEL PROCESSING OPTION.  To use MPI, uncomment the following 2 lines
# and replace "para-non.o" with "para-mpi.o" in definition of OBJECTS above.
#para-mpi.o: parallel/para-mpi.f
#	$(F77) $(F77FLAGS) -I$(INCDIR) -c parallel/para-mpi.f

# SERIAL PROCESSING OPTION.  To use MPI, comment out the following 2 lines:
para-non.o: serial/para-non.f
	$(F77) $(F77FLAGS) -I$(INCDIR) -c serial/para-non.f


# Pth_Object codes of Modtools

.f.o:
	$(F77) $(F77FLAGS) -c $<

.c.o:
	$(CC) $(CFLAGS) -c $<

clean:
	- rm *.o core

#  end
