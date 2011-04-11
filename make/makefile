# Makefile example for compiling GMG and linking GMG to SEAWAT Version 4:

SOURCELOC = ..\Source
PROGRAM = swt_v4

#Intel compile flags
F90FLAGS= -O3 -nologo -real_size:64 -align:dcommons -heap-arrays:0
F90= ifort

#Microsoft C++ compiler
CFLAGS = -nologo
CC = cl

#Link flags
LINKER = link
LFLAGS = -out:$(PROGRAM).exe

# Define GMG objects
GMG = r_vector.obj\
      solvers.obj\
      ccfd.obj\
      mf2kgmg.obj 

# Define the libraries
SYSLIBS =
USRLIB  = 

# Define all object files comprising SEAWAT
# NOTE: Order is important here.

OBJECTS = \
    gwf1bas6.obj      \
    gwf1lpf1.obj      \
    gwf1bcf6.obj      \
    gwf1huf2.obj      \
    mhc1.obj          \
    gmg1.obj          \
    vdf1.obj          \
    vdf1vsc1.obj      \
    vdf1bcf6.obj      \
    vdf1lpf1.obj      \
    obs1drn6vd.obj    \
    vdf1lmt6.obj      \
    obs1riv6vd.obj    \
    vdf1ets1.obj      \
    vdf1chd6.obj      \
    vdf1evt6.obj      \
    obs1drt1vd.obj    \
    swt_v4.obj        \
    vdf1mnw1.obj      \
    vdf1drn6.obj      \
    vdf1wel6.obj      \
    vdf1fhb1.obj      \
    vdf1rch6.obj      \
    vdf1drt1.obj      \
    obs1bas6vd.obj    \
    vdf1ghb6.obj      \
    obs1ghb6vd.obj    \
    vdf1riv6.obj      \
    vdf1huf2.obj      \
    gwf1sub1.obj      \
    gwf1swt1.obj      \
    gwf1ghb6.obj      \
    hufutl2.obj       \
    obs1adv2.obj      \
    parutl1.obj       \
    sen1rch6.obj      \
    gwf1mnw1.obj      \
    imt1gcg5.obj      \
    sen1chd6.obj      \
    sip5.obj          \
    gwf1drt1.obj      \
    imt1utl5.obj      \
    sen1evt6.obj      \
    gwf1hfb6.obj      \
    hydmod.obj        \
    obs1bas6.obj      \
    pcg2.obj          \
    sen1riv6.obj      \
    gutsdaf.obj       \
    gwf1rch6.obj      \
    obs1drt1.obj      \
    sen1drn6.obj      \
    sor5.obj          \
    gwf1ets1.obj      \
    lmg1.obj          \
    obs1str6.obj      \
    sen1ghb6.obj      \
    ctime.obj         \
    imt1adv5.obj      \
    pes1bas6.obj      \
    sen1str6.obj      \
    gwf1res1.obj      \
    sen1drt1.obj      \
    gwf1evt6.obj      \
    lmt6.obj          \
    sen1hfb6.obj      \
    daf1.obj          \
    gwf1ibs6.obj      \
    imt1btn5.obj      \
    pes1gau1.obj      \
    sen1wel6.obj      \
    gwf1riv6.obj      \
    imt1rct5.obj      \
    obs1ghb6.obj      \
    utl6.obj          \
    gwf1fhb1.obj      \
    memchk.obj        \
    sen1huf2.obj      \
    de45.obj          \
    gwf1lak3.obj      \
    imt1dsp5.obj      \
    rtedaf.obj        \
    gwf1chd6.obj      \
    gwf1sfr2.obj      \
    imt1ssm5.obj      \
    gwf1gag5.obj      \
    gwf1wel6.obj      \
    para-non.obj      \
    sen1lpf1.obj      \
    glo1bas6.obj      \
    imt1fmi5.obj      \
    obs1drn6.obj      \
    sen1bas6.obj      \
    gwf1drn6.obj      \
    gwf1str6.obj      \
    imt1tob5.obj      \
    obs1riv6.obj      \
    sen1ets1.obj      \
    vdf1bas6.obj


all: $(PROGRAM) clean

#define the SEAWAT target
$(PROGRAM): $(OBJECTS) $(GMG)
	$(LINKER) $(LFLAGS) $(OBJECTS) $(GMG) $(USRLIB) $(SYSLIBS)

#type nmake clean will delete all obj and mod files
clean:
	del *.obj
	del *.mod

# Pth_Object codes of SEAWAT

{$(SOURCELOC)}.f.obj:
	$(F90) $(F90FLAGS) -c $<

{$(SOURCELOC)}.f90.obj:
	$(F90) $(F90FLAGS) -c $<

{$(SOURCELOC)}.c.obj:
	$(CC) $(CFLAGS) -c $<

