C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C                                                                      %
C                            SEAWAT-2000                               %
C                   a modular three-dimensional model                  %
C          for simulating the flow of variable-density groundwater     %
C                                                                      %
C                          (Version 3.10)                              %
C                                                                      %
C                           Developed by                               %
C                                                                      %
C                        Christian Langevin                            %
C                      U.S. Geological Survey                          %
C                   9100 NW 36th Street, Suite 107                     %
C                          Miami FL 33178                              %
C                     E-Mail: langevin@usgs.gov                        %                                                                      %
C                                                                      %
C                                                                      %
C                        W.Barclay Shoemaker                           %
C                      U.S. Geological Survey                          %
C                   9100 NW 36th Street, Suite 107                     %
C                          Miami FL 33178                              %
C                     E-Mail: bshoemak@usgs.gov                        %
C                                                                      %
C                               and                                    %
C                                                                      %
C                            Weixing Guo                               %
C                           CDM Missmimer                              %
C                     8140 College Parkway, Suite 202                  %
C                        Fort Myers, FL 33919                          %
C                        E-Mail: guow@cdm.com                          %
C                                                                      %
C                                                                      %
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C--SEAWAT:  LANGEVIN 02/13/2004 PREPARED RELEASE VERSION OF SEAWAT-2000
C--SEAWAT:    FOR DISTRIBUTION TO THE GENERAL PUBLIC
C--SEAWAT:  LANGEVIN 03/18/2004 FIXED A BUG THAT PREVENTED THE VALUE OF
C--SEAWAT:    HDRY FROM BEING PRINTED TO THE OUTPUT.
C--SEAWAT:  LANGEVIN 03/29/2004 FIXED TWO BUGS.  THE FIRST WAS IN THE
C--SEAWAT:    CALL TO BAS1OT.  IT DIDN'T WORK FOR AN MT3D NO VDF 
C--SEAWAT:    SIMULATION.  THE SECOND BUG WAS RELATED TO THE LOCATION
C--SEAWAT:    OF THE CALL TO VDF1PS.  CALL MOVED UP A COUPLE OF LINES.
C     Last change:  ERB   3 May 2002   10:07 am
C     ******************************************************************
C     MAIN CODE FOR U.S. GEOLOGICAL SURVEY MODULAR MODEL -- MODFLOW
C           BY MICHAEL G. MCDONALD AND ARLEN W. HARBAUGH
C     MODFLOW-88 documented in:
C        McDonald, M.G. and Harbaugh, A.W., 1988, A modular three-
C           dimensional finite-difference ground-water flow model:
C           U.S. Geological Survey Techniques of Water Resources
C           Investigations, Book 6, Chapter A1, 586 p.
C     MODFLOW-96 documented in:
C        Harbaugh, A.W. and McDonald, M.G., 1996, User's documentation
C           for the U.S. Geological Survey modular finite-difference
C           ground-water flow model: U.S. Geological Survey Open-File
C           Report 96-485
C     MODFLOW-2000 documented in:
C        Harbaugh, A.W., Banta, E.R., Hill, M.C., and McDonald, M.G.,
C           2000, MODFLOW-2000, the U.S. Geological Survey modular
C           ground-water model--User guide to modularization concepts
C           and the Ground-Water Flow Process: U.S. Geological Survey
C           Open-File Report 00-92
C        Hill, M.C., Banta, E.R., Harbaugh, A.W., and Anderman, E.R.,
C           2000, MODFLOW-2000, the U.S. Geological Survey modular
C           ground-water model--User guide to the Observation,
C           Sensitivity, and Parameter-Estimation Processes and three
C           post-processing programs: U.S. Geological Survey Open-
C           File Report 00-184
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C                                                                      %
C                               MT3DMS                                 %
C      a modular three-dimensional multi-species transport model       %
C    for simulation of advection, dispersion and chemical reactions    %
C                of contaminants in groundwater systems                %
C                                                                      %
C                  For Technical Information Contact                   %
C                           Chunmiao Zheng                             %
C                  Department of Geological Sciences                   %
C                        University of Alabama                         %
C                        Tuscaloosa, AL 35487                          %
C                        Email: czheng@ua.edu                          %
C              Web site: http://hydro.geo.ua.edu/mt3d                  %
C                                                                      %
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C MT3DMS is based on MT3D originally developed by Chunmiao Zheng
C at S.S. Papadopulos & Associates, Inc. and documented for
C the United States Environmental Protection Agency.
C MT3DMS is written by Chunmiao Zheng and P. Patrick Wang
C with the iterative solver routine by Tsun-Zee Mai.
C Funding for MT3DMS development is provided, in part, by
C U.S. Army Corps of Engineers, Research and Development Center.
C
C Copyright, 1998-2003, The University of Alabama. All rights reserved.
C
C This program is provided without any warranty.
C No author or distributor accepts any responsibility
C to anyone for the consequences of using it
C or for whether it serves any particular purpose.
C The program may be copied, modified and redistributed,
C but ONLY under the condition that the above copyright notice
C and this notice remain intact.
C
C=======================================================================
C Version history: 06-23-1998 (3.00.A)
C                  05-10-1999 (3.00.B)
C                  11-15-1999 (3.50.A)
C                  08-15-2000 (3.50.B)
C                  08-12-2001 (4.00)
C                  05-27-2003 (4.50)
C

C--SEAWAT: COMPAQ COMPILER NEEDS DFLIB LIBRARY TO USE GETARG RUNTIME SUBROUTINE (CVDP2)
      USE DFLIB
C--SEAWAT:-----ASSIGN VERSION NUMBER AND DATE
      CHARACTER*40 VERSION
      PARAMETER (VERSION='3.10.01 03/30/2004')
C
C-----DECLARE ARRAY TYPES
      REAL GX, X, RX, XHS
      DOUBLE PRECISION GZ, VAR, Z
      INTEGER IG, IX, IR
      CHARACTER*10 EQNAM, NIPRNAM
      CHARACTER*12 NAMES, OBSNAM
      CHARACTER*32 MNWSITE
C
C *** FOR STATIC MEMORY ALLOCATION, THE FOLLOWING PARAMETER AND
C *** DIMENSION STATEMENTS MUST BE UNCOMMENTED.  TO CHANGE THE SIZE OF
C *** AN ARRAY, CHANGE THE VALUE OF THE CORRESPONDING (FORTRAN)
C *** PARAMETER AND RECOMPILE
C      PARAMETER (LENGX=1000000, LENIG=1000000, LENGZ=1000000,
C     &           LENX=2000000, LENIX=1500000, LENZ=1000000,
C     &           LENRX=1000000, LENIR=1000000, LENXHS=1000000,
C     &           NDD=10000, MPRD=100, IPRD=100)
C      DIMENSION GX(LENGX), IG(LENIG), X(LENX), IX(LENIX), RX(LENRX),
C     &          IR(LENIR), GZ(LENGZ), Z(LENZ), XHS(LENXHS),
C     &          EQNAM(MPRD), NIPRNAM(IPRD), NAMES(NDD+MPRD+IPRD),
C     &          OBSNAM(NDD)
C
C *** FOR STATIC MEMORY ALLOCATION, THE FOLLOWING ALLOCATABLE
C *** STATEMENT MUST BE COMMENTED OUT
      ALLOCATABLE GX(:), IG(:), X(:), IX(:), RX(:), IR(:), GZ(:), Z(:),
     &            XHS(:), NIPRNAM(:), EQNAM(:), NAMES(:), OBSNAM(:)
C
	ALLOCATABLE MNWSITE(:)
C
      PARAMETER (NIUNIT=100)
      PARAMETER (MXPER=1000)
C
      DIMENSION PERLEN(MXPER),NSTP(MXPER),TSMULT(MXPER),ISSFLG(MXPER)
      CHARACTER*16 VBNM(NIUNIT)
      DIMENSION VBVL(4,NIUNIT),IUNIT(NIUNIT),IREWND(NIUNIT)
      CHARACTER*80 HEADNG(2)
      DOUBLE PRECISION AP
C
C  UNCOMMENT "INCLUDE mpif.h" DURING TESTING TO DEVELOP MPI CODE IN THIS
C  ROUTINE OR TO ACTIVATE TIMERS AND DEBUG MODE.
C     INCLUDE 'mpif.h'
      INCLUDE 'parallel.inc'
      INCLUDE 'param.inc'
      INCLUDE 'openspec.inc'
C-------SPECIFY SIZE OF ARRAY TO HOLD SENSITIVITIES FROM ONE
C-------PARAMETER-ESTIMATION ITERATION TO THE NEXT WHEN PES, SEN, AND
C-------ANY OBS PACKAGE ARE ACTIVE.  IF IUHEAD IS GREATER THAN ZERO,
C-------LENXHS MAY EQUAL 1.  IF IUHEAD IS LESS THAN OR EQUAL TO ZERO,
C-------LENXHS MUST BE AT LEAST:
C-------NLAY*NCOL*NROW*(NUMBER OF PARAMETERS TO BE ESTIMATED).
C
      COMMON /BCFCOM/LAYCON(200)
      COMMON /DISCOM/LBOTM(200),LAYCBD(200)
      COMMON /LPFCOM/LAYTYP(200),LAYAVG(200),CHANI(200),LAYVKA(200),
     1               LAYWET(200)
      INTEGER LAYHDT(200)
C
      CHARACTER*4 PIDTMP
      CHARACTER*20 CHEDFM,CDDNFM,CBOUFM
      CHARACTER*200 FNAME, OUTNAM, COMLIN
	CHARACTER*200 MNWNAME
C
      LOGICAL EXISTS, BEFIRST, SHOWPROG, RESETDD, RESETDDNEXT, OBSALL
      INTEGER NPEVT, NPGHB, NPDRN, NPHFB, NPRIV, NPSTR, NPWEL, NPRCH
      INTEGER IUBE(2), IBDT(8)
	INTEGER IOWELL2(3)  !FOR MNW1 PACKAGE
      CHARACTER*4 CUNIT(NIUNIT)
      CHARACTER*10 PARNEG(MXPAR)

C--SEAWAT: MT3DMS DECLARATIONS******************************************
      PARAMETER (LENY=999999999,LENIY=999999999)
      PARAMETER (MXPRS=01000,MXSTP=10000,MXOBS=01000,MXCOMP=100)
      PARAMETER (ICNF=199,IUCN=200,IUCN2=300,
     +           IMOBS=400,IMAS=600,ICBM=800)
	PARAMETER (INUHF=95)
      LOGICAL   TRNOP(10),UNIDX,UNIDY,UNIDZ,SAVUCN,SAVCBM,CHKMAS,
     &          FWEL,FDRN,FRCH,FEVT,FRIV,FGHB,PRTOUT,UPDLHS,EXISTED,
     &          FSTR,FRES,FFHB,FIBS,FTLK,FLAK,FMAW,FDRT,FETS,FUSR(3)
      CHARACTER FLNAME*50,FINDEX*30,TUNIT*4,LUNIT*4,MUNIT*4,FPRT*1
C--SEAWAT: CREATE DUMMY FOR COMPATIBILITY WITH PES PROCESS
      CHARACTER*20 DUMMY

      DIMENSION Y(:),IY(:),
     &          TIMPRS(MXPRS),TSLNGH(MXSTP),LOCOBS(3,MXOBS),
     &          NCOUNT(MXCOMP),NPINS(MXCOMP),NRC(MXCOMP),
     &          TMASIO(122,2,MXCOMP),RMASIO(122,2,MXCOMP),
     &          TMASS(4,3,MXCOMP),TMASIN(MXCOMP),TMASOT(MXCOMP),
     &          ERROR(MXCOMP),ERROR2(MXCOMP)
      ALLOCATABLE :: Y,IY
      COMMON   /PD/HORIGN,XMAX,YMAX,ZMAX,UNIDX,UNIDY,UNIDZ
C--SEAWAT: CHANGED COMMON NAME FC TO FCMT3D TO AVOID CONFLICT WITH SUBROUTINE
      COMMON   /FCMT3D/FWEL,FDRN,FRCH,FEVT,FRIV,FGHB,
     &          FSTR,FRES,FFHB,FIBS,FTLK,FLAK,FMAW,FDRT,FETS,FUSR
      COMMON   /OC/IFMTCN,IFMTNP,IFMTRF,IFMTDP,SAVUCN,
     &             SAVCBM,CHKMAS,NPRMAS
      COMMON   /AD/PERCEL,ITRACK,WD,ISEED,DCEPS,NPLANE,NPL,NPH,
     &             NPMIN,NPMAX,SRMULT,INTERP,NLSINK,NPSINK,DCHMOC
      COMMON   /GCGIDX/L(19)
	COMMON   /FTL/iFTLfmt
	COMMON     /LINKMT3D/ILMTFMT

C--SEAWAT: SEAWAT2000 DECLARATIONS**************************************
      ALLOCATABLE VDF(:)
      LOGICAL DONE

	INCLUDE 'vdf.inc'

C--SEAWAT: ASSIGN IUNIT LOCATIONS TO PACKAGES
      DATA CUNIT/'BCF6', 'WEL ', 'DRN ', 'RIV ', 'EVT ', '    ', 'GHB ',  !  7
     &           'RCH ', 'SIP ', 'DE4 ', 'SOR ', 'OC  ', 'PCG ', 'LMG ',  ! 14
     &           'gwt ', 'FHB ', 'RES ', 'STR ', 'IBS ', 'CHD ', 'HFB6',  ! 21
     &           'LAK ', 'LPF ', 'DIS ', 'SEN ', 'PES ', 'OBS ', 'HOB ',  ! 28
     &           'ADV2', 'COB ', 'ZONE', 'MULT', 'DROB', 'RVOB', 'GBOB',  ! 35
     &           'STOB', 'HUF2', 'CHOB', 'ETS ', 'DRT ', 'DTOB', '    ',  ! 42
     &           'HYD ', 'sfr ', 'SFOB', 'GAGE', 'LVDA', '    ', 'LMT6',  ! 49
     &           'MNW1', 'DAF ', 'DAFG', 'KDEP', 'SUB ', '    ', '    ',  ! 56
     &           'VDF ', 'BTN ', 'ADV ', 'DSP ', 'SSM ', 'RCT ', 'GCG ',  ! 63
     &           37*'    '/
C     ------------------------------------------------------------------
cc      open(78,file='mf2k.dbg')
cc      write(*,*)' Opened file mf2k.dbg for debugging'
cc      write(78,2005)
cc 2005 format(' File mf2k.dbg',/)
      CALL PLL1IN
C--SEAWAT: CHANGED TO SEAWAT-2000
      IF (MYID.EQ.MPROC) WRITE (*,1) VERSION
    1 FORMAT (/,34X,'SEAWAT-2000',/,
     &4X,'U.S. GEOLOGICAL SURVEY MODULAR FINITE-DIFFERENCE',
     &' GROUND-WATER FLOW MODEL',/,29X,'Version ',A/)
      WRITE(*,101)
  101 FORMAT(
     +/1X,'This program is public domain and is released on the'
     +/1X,'condition that neither the U.S. Geological Survey nor'
     +/1X,'the United States Government may be held liable for any'
     +/1X,'damages resulting from their authorized or unauthorized'
     +/1X,'use.'//)
      INUNIT = 99
      IBUNIT = 98
      IBOUTS = 97
      IERRU  = 96
      MAXUNIT= INUNIT
C     DEFINE RANGE OF RESERVED FILE UNITS
C--SEAWAT: CHANGED MINRSV TO 95.  INUHF SET TO 95 ABOVE AS PARAMETER
      MINRSV = 95
      MAXRSV = 99
      IBATCH = 0
CLAK
      NSOL = 1
      DUM=0.0D0
C
C--SEAWAT:  SET LCISEN TO 1 BASED ON ARLEN'S SUGGESTION (6/4/03)
	LCISEN=1
      INQUIRE (FILE='modflow.bf',EXIST=EXISTS)
      IF (EXISTS) THEN
        IBATCH = 1
        IF (MYID.EQ.MPROC) THEN
          OPEN (UNIT=IBUNIT,FILE='modflow.bf',STATUS='OLD')
          OPEN (UNIT=IBOUTS,FILE='modbatch.rpt')
          WRITE (IBOUTS,*) ' USGS SEAWAT MODEL BATCH-MODE REPORT'
        ENDIF
      ENDIF
C2------OPEN FILE OF FILE NAMES.
   10 CONTINUE
      IF (MYID.EQ.MPROC) THEN
        IF (IBATCH.GT.0) THEN
          READ (IBUNIT,'(A)',END=11) FNAME
          GOTO 12
   11     CLOSE(IBUNIT)
          CLOSE(IBOUTS)
          FNAME=' '
          GOTO 15
   12     IF (FNAME.EQ.' ') GOTO 10
          WRITE (IBOUTS,'(1X,/1X,A)') FNAME
        ELSE
          FNAME=' '
          COMLIN=' '
C *** Subroutines GETARG and GETCL are extensions to Fortran 90/95 that
C *** allow a program to retrieve command-line arguments.  To enable
C *** Modflow-2000 to read the name of a Name file from the command
C *** line, either GETARG or GETCL must be called, but not both.  As
C *** distributed, the call to GETARG is uncommented.  For compilers
C *** that support GETCL but not GETARG, comment out the call to GETARG
C *** and uncomment the call to GETCL.  The calls to both GETARG and
C *** GETCL may be commented out for compilers that do not support
C *** either extension.
          CALL GETARG(1,COMLIN)
C	     CALL GETCL(COMLIN)
          ICOL = 1
          IF(COMLIN.NE.' ') THEN
            FNAME=COMLIN
          ELSE
            WRITE (*,*) ' Enter the name of the NAME FILE: '
            READ (*,'(A)') FNAME
            CALL URWORD(FNAME,ICOL,ISTART,ISTOP,0,N,R,0,0)
            FNAME=FNAME(ISTART:ISTOP)
          ENDIF
          IF (FNAME.EQ.' ') GOTO 15
          INQUIRE (FILE=FNAME,EXIST=EXISTS)
          IF(.NOT.EXISTS) THEN
            NC=INDEX(FNAME,' ')
            FNAME(NC:NC+3)='.nam'
            INQUIRE (FILE=FNAME,EXIST=EXISTS)
            IF(.NOT.EXISTS) THEN
              WRITE (*,480) FNAME(1:NC-1),FNAME(1:NC+3)
              FNAME=' '
            ENDIF
          ENDIF
        ENDIF
        IF (FNAME.EQ.' ') GOTO 15
        INQUIRE (FILE=FNAME,EXIST=EXISTS)
        IF (.NOT.EXISTS) THEN
          IF (IBATCH.GT.0) THEN
            WRITE (IBOUTS,*) ' Specified name file does not exist.'
            WRITE (IBOUTS,*) ' Processing will continue with the next ',
     &                       'name file in modflow.bf.'
          ENDIF
          GOTO 10
        ENDIF
      ENDIF
   15 CONTINUE
  480 FORMAT(1X,'Can''t find name file ',A,' or ',A)
C
C     BROADCAST FNAME AND OPEN FILE FOR WARNINGS AND ERROR MESSAGES
      CALL PLL1FN(FNAME)
      IF (FNAME.EQ.' ') GOTO 120
      CALL PLL1OP(IERRU,IERR)
      OPEN (UNIT=INUNIT,FILE=FNAME,STATUS='OLD',ACTION=ACTION(1))
      IF (MYID.EQ.MPROC) WRITE(*,490)' Using NAME file: ',FNAME
  490 FORMAT(A,A)
C
C  DEFINE (DF) PROCEDURE
      CALL GLO1BAS6DF(INUNIT,IUNIT,CUNIT,IREWND,NIUNIT,IOUTG,IOUT,
     &                VERSION,NCOL,NROW,NLAY,NPER,ITMUNI,ISUMGX,
     &                MXPER,ISUMIG,ISUMGZ,INBAS,LENUNI,ISUMX,ISUMZ,
     &                ISUMIX,LAYHDT,24,IFREFM,INAMLOC,IPRTIM,IBDT,
     &                SHOWPROG)
      CALL OBS1BAS6DF(IOBS,IOSTAR,IOWTQ,IOWTQDR,IOWTQGB,
     &                IOWTQRV,IOWTQST,IQ1,IUNIT(27),JT,LCCOFF,LCHFB,
     &                LCIPLO,LCIPLP,LCIQOB,LCNDER,LCNQOB,LCOBADV,
     &                LCOBDRN,LCOBGHB,LCOBBAS,LCOBRIV,LCOBSE,LCOBSTR,
     &                LCQCLS,LCROFF,LCSSAD,LCSSCH,LCSSDR,LCSSGB,LCSSGF,
     &                LCSSPI,LCSSRV,LCSSST,LCSSTO,LCWT,LCWTQ,MOBS,NC,ND,
     &                NDMH,NDMHAR,NH,NOBADV,NQ,NQC,NQT,NQT1,NQTDR,
     &                NQTGB,NQTRV,NQTST,NQTCH,NT,NTT2,IOBSUM,LCX,
     &                LCBUF2,NDAR,LCOBDRT,LCSSDT,NQTDT,IOWTQDT,LCSSSF,
     &                NQTSF,LCOBSFR,IOWTQSF,NHT,LCRSQA,LCRSPA,LCBUF1,
     &                LCH,LCHOBS,LCWTQS,LCHANI,LCXND,LCOTIM,OBSALL)
      CALL SEN1BAS6DF(ISENALL,ISEN,IPRINTS,IUNIT(25),LCB1,LCLN,LCSV,NPE,
     &                NPLIST,RCLOSE,IUHEAD,MXSEN,LCSNEW,IOUTG,LCBSCA,
     &                LCISEN)
      CALL PES1BAS6DF(IBEFLG,IFO,IOUB,IPES,IPR,IPRAR,IPRINT,ITERPF,
     &                ITERPK,ITMXP,IUNIT(26),IYCFLG,JMAX,LASTX,LCDMXA,
     &                LCNIPR,LCNPAR,LCPRM,LCWP,LCWTP,LCWTPS,LCW3,LCW4,
     &                MPR,MPRAR,NPNGAR,SOSC,SOSR,BEFIRST,LCBPRI,LCPARE,
     &                LCAMPA,LCAMCA,LCAAP)
      CALL GWF1MNW1DF(LCHANI,LCHK,LCHKCC,LCHUFTHK,LCHY,LCSSHMN,LCTRPY,
     &                NHUFAR)
C
C--SEAWAT: DEFINE MT3D AND SEAWAT2000
      IF(IUNIT(58).GT.0) THEN
	  InameFile=0
        CALL IMT1BTN4DF(IUNIT(58),IOUT,ISUMY,ISUMIY,NCOL,NROW,NLAY,
     &                 NPER,NCOMP,MCOMP,TRNOP,TUNIT,LUNIT,MUNIT,
     &                 NODES,MXCOMP,iNameFile)
	ENDIF
      IF(IUNIT(57).GT.0) 
     &  CALL VDF1DF(IUNIT(57),IOUT,IFREFM,MTDNCONC,
     &                 MFNADVFD,IUNIT(58),NSWTCPL,DNSCRIT,FIRSTDT,
     &                 IWTABLE,IUNIT(25),NCOMP,MXSS,NSSVL,ISUMY,ISUMIY,
     &                 LCCNEW,LCSSM,LCSSMC,LTCRCH,LTCEVT,NCOL,NROW,NLAY)

C       
C  GLOBAL ALLOCATE (AL) PROCEDURE
      CALL GLO1BAS6AL(IUNIT(24),NCNFBD,NBOTM,NCOL,NROW,NLAY,LCBOTM,
     &                LCDELR,LCDELC,ISUMGX,IOUTG,LCHNEW,LCIBOU,LCCR,
     &                LCCC,LCCV,LCRHS,LCHCOF,LCHOLD,LCBUFF,LCSTRT,
     &                ISUMGZ,ISUMIG,ISEN,IOBS,IPES,ISENALL,ITMXP,IPAR,
     &                IUNIT(31),IUNIT(32),NMLTAR,NZONAR,NML,NZN,LCRMLT,
     &                LCIZON,IUNIT(15))
C
C  DYNAMICALLY ALLOCATE GLOBAL ARRAYS GX, GZ, AND IG.  FOR STATIC
C  MEMORY ALLOCATION, THE FOLLOWING THREE ASSIGNMENT AND ONE ALLOCATE
C  STATEMENTS MUST BE COMMENTED OUT
      LENGX = ISUMGX - 1
      LENGZ = ISUMGZ - 1
      LENIG = ISUMIG - 1
      ALLOCATE (GX(LENGX),GZ(LENGZ),IG(LENIG))
C
      CALL MEMCHKG(ISUMGX,ISUMIG,ISUMGZ,LENGX,LENIG,LENGZ,IOUTG,IERR,
     &             IERRU)
      IF (IERR.GT.0) CALL PLL1SD(IERR,IERRU,IOUT,IOUTG)
C
C  GLOBAL READ AND PREPARE (RP) PROCEDURE
      CALL GLO1BAS6RP(IUNIT(24),NCOL,NROW,NLAY,GX(LCBOTM),NBOTM,IOUTG,
     1                GX(LCDELR),GX(LCDELC),NPER,PERLEN,NSTP,TSMULT,
     2                ISSFLG,ITRSS,IUNIT(31),IUNIT(32),NMLTAR,NZONAR,
     3                GX(LCRMLT),IG(LCIZON),NML,NZN)
C
C-----NO rewind AL and RP for Ground-Water Flow Process
      IF(IUNIT(23).GT.0)
     1    CALL GWF1LPF1ALG(ISUMX,LCHK,LCVKA,LCSC1,LCSC2,LCHANI,LCVKCB,
     2                     IUNIT(23),NCOL,NROW,NLAY,IOUTG,ILPFCB,LCWETD,
     3                     HDRY,NPLPF,NCNFBD,LCLAYF,IREWND(23),ISUMIX,
     4                     LAYHDT,ITRSS,LCSV,ISEN)
      IF(IUNIT(37).GT.0) THEN
        CALL GWF1HUF2ALG(ISUMX,LCHK,LCVKA,LCSC1,IUNIT(37),ITRSS,NCOL,
     &                   NROW,NLAY,IOUTG,IHUFCB,LCWETD,HDRY,NPER,
     &                   ISSFLG,LCHGUF,IREWND(37),
     &                   NHUF,NPHUF,LCHUFTHK,LCHKCC,ISUMIX,IOHUFHDS,
     &                   IOHUFFLWS,LAYHDT,LCHUFTMP)
        CALL GWF1HUF2LVDA1ALG(ISUMX,ISUMIX,IUNIT(47),IOUTG,NCOL,
     &                        NROW,NLAY,LCVDHD,LCDVDH,LCVDHT,NPLVDA,
     &                        LCA9)
        CALL GWF1HUF2KDEP1ALG(ISUMX,IUNIT(53),IOUTG,NCOL,NROW,
     &                        LCGS,NPKDEP,IFKDEP)
      ENDIF
      IF(IUNIT(9).GT.0)
     1    CALL SIP5ALG(ISUMX,ISUMIX,LCEL,LCFL,LCGL,LCV,LCHDCG,LCLRCH,
     2                 LCW,MXITER,NPARM,NCOL,NROW,NLAY,IUNIT(9),IOUTG,
     3                 IFREFM,IREWND(9))
      IF(IUNIT(10).GT.0)
     1    CALL DE45ALG(ISUMX,ISUMIX,LCAU,LCAL,LCIUPP,LCIEQP,LCD4B,
     2                 LCLRCH,LCHDCG,MXUP,MXLOW,MXEQ,MXBW,IUNIT(10),
     3                 ITMX,ID4DIR,NCOL,NROW,NLAY,IOUTG,ID4DIM,
     4                 IREWND(10))
      IF(IUNIT(11).GT.0)
     1    CALL SOR5ALG(ISUMX,ISUMIX,LCA,LCRES,LCHDCG,LCLRCH,LCIEQP,
     2                 MXITER,NCOL,NLAY,NSLICE,MBW,IUNIT(11),IOUTG,
     3                 IFREFM,IREWND(11))
      IF(IUNIT(13).GT.0)
     1    CALL PCG2ALG(ISUMX,ISUMIX,LCV,LCSS,LCP,LCCD,LCHCHG,LCLHCH,
     2                 LCRCHG,LCLRCH,MXITER,ITER1,NCOL,NROW,NLAY,
     3                IUNIT(13),IOUTG,NPCOND,LCIT1,LCHCSV,IFREFM,
     4                IREWND(13),ISUMZ,LCHPCG)
      IF(IUNIT(14).GT.0)
     1    CALL LMG1ALG(ISUMZ,ISUMIX,LCA,LCIA,LCJA,LCU1,LCFRHS,
     2                 LCIG,ISIZ1,ISIZ2,ISIZ3,ISIZ4,ICG,NCOL,NROW,NLAY,
     3                 IUNIT(14),IOUTG,1)
C
C-----ALLOCATE SPACE FOR SENSITIVITY CALCULATIONS
      IF (ISEN.GT.0)
     &    CALL SEN1BAS6AL(ISUMX,ISUMIX,NCOL,NROW,NLAY,IOUTG,IUHEAD,
     &                    NPLIST,IUNIT(25),IPAR,LCHCLO,LCRCLO,LCLN,
     &                    IPRINTS,LCISEN,LCBU,LCBL,LCB1,ISENALL,
     &                    IREWND(25),LCSNEW,LCSOLD,ISUMZ,ISEN,ISENSU,
     &                    ISENPU,ISENFM,IPES,MXSEN,LCBSCA,ITMXP,MAXUNIT,
     &                    MINRSV,MAXRSV,NSTP,NPER,NTIMES,LCSEND,LCSNDT)
C-----ALLOCATE SPACE FOR PARAMETER-ESTIMATION PROCESS
      IF (IPES.GT.0)
     &    CALL PES1BAS6AL(ISUMX,ISUMZ,ISUMIX,IOUTG,NPLIST,LCC,LCSCLE,
     &                    LCG,LCDD,LCWP,MPR,LCPRM,LCR,LCU,LCGD,
     &                    LCS,NOPT,IPR,LCWTP,LCWTPS,LCW3,LCW4,LCNIPR,
     &                    LCEIGL,LCEIGV,LCEIGW,LCIPNG,IUNIT(26),
     &                    NPNG,MPRAR,IPRAR,NPNGAR,IREWND(26),
     &                    LCPRNT,LCPARE,ITMXP,LCSSPI,LCSSTO,DMAX,TOL,
     &                    SOSC,IOSTAR,NFIT,SOSR,IPRC,IPRINT,LPRINT,CSA,
     &                    FCONV,LASTX,ISEN,IPES,IPAR,IBEFLG,IYCFLG,
     &                    LCDMXA,LCNPAR,LCBPRI,RMARM,IAP,LCAAP,
     &                    LCAMCA,LCAMPA,RMAR)
C-----READ INPUT RELATED TO ALL OBSERVATIONS AND OPEN
C     PARAMETER-VALUE FILE ON IOUB
      IF (IOBS.GT.0)
     &    CALL OBS1BAS6AL(IOUB,IOUTG,ISCALS,ISEN,IUNIT(27),OUTNAM,
     &                    ISOLDX,ISOLDZ,ISOLDI,ISUMX,ISUMZ,ISUMIX,
     &                    OBSALL)
C-----ALLOCATE SPACE FOR HEAD OBSERVATIONS
      IF (IUNIT(28).GT.0)
     &    CALL OBS1BAS6HAL(IUNIT(28),NH,MOBS,MAXM,ISUMX,ISUMIX,LCNDER,
     &                     LCCOFF,LCROFF,LCIOFF,LCJOFF,LCRINT,LCMLAY,
     &                     LCPR,ND,IOUTG,IOBSUM,LCOBBAS,ITMXP,LCSSGF,
     &                     IOBS,NHT)
C-----ALLOCATE SPACE FOR FLOW OBSERVATIONS
      IF (IUNIT(33).GT.0)
     &    CALL OBS1DRN6AL(IUNIT(33),NQ,NQC,NQT,IOUTG,NQDR,NQTDR,IOBSUM,
     &                    LCOBDRN,ITMXP,LCSSDR,ISUMX,IOBS)
      IF (IUNIT(34).GT.0)
     &    CALL OBS1RIV6AL(IUNIT(34),NQ,NQC,NQT,IOUTG,NQRV,NQTRV,IOBSUM,
     &                    LCOBRIV,ITMXP,LCSSRV,ISUMX,IOBS)
      IF (IUNIT(35).GT.0)
     &    CALL OBS1GHB6AL(IUNIT(35),NQ,NQC,NQT,IOUTG,NQGB,NQTGB,IOBSUM,
     &                    LCOBGHB,ITMXP,LCSSGB,ISUMX,IOBS)
      IF (IUNIT(36).GT.0)
     &    CALL OBS1STR6AL(IUNIT(36),NQ,NQC,NQT,IOUTG,NQST,NQTST,IOBSUM,
     &                    LCOBSTR,ITMXP,LCSSST,ISUMX,IOBS)
      IF (IUNIT(38).GT.0)
     &    CALL OBS1BAS6FAL(IUNIT(38),NQ,NQC,NQT,IOUTG,NQCH,NQTCH,IOBSUM,
     &                     LCOBCHD,ITMXP,LCSSCH,ISUMX,IOBS)
      IF (IUNIT(41).GT.0)
     &    CALL OBS1DRT1AL(IUNIT(41),NQ,NQC,NQT,IOUTG,NQDT,NQTDT,IOBSUM,
     &                    LCOBDRT,ITMXP,LCSSDT,ISUMX,IOBS)
C-----ALLOCATE SPACE FOR ADVECTIVE TRAVEL OBSERVATIONS (ADV PACKAGE)
      IF (IUNIT(29).GT.0)
     &    CALL OBS1ADV2AL(IUNIT(29),NPTH,NTT2,IOUTT2,KTDIM,KTFLG,KTREV,
     &                    ADVSTP,IOUTG,LCICLS,LCPRST,NPRST,LCTT2,LCPOFF,
     &                    LCNPNT,ND,ISUMX,ISUMIX,NROW,NCOL,NLAY,
     &                    IOBSUM,LCOBADV,NOBADV,ITMXP,LCSSAD,IOBS,
     &                    FSNK,NBOTM,IUNIT,NIUNIT,LCDRAI,MXDRN,
     &                    NDRAIN,LCRIVR,MXRIVR,LCBNDS,MXBND,NBOUND,
     &                    LCIRCH,LCRECH,ICSTRM_,LCSTRM_,MXSTRM,NSTREM,
     &                    NDRNVL,NGHBVL,NRIVVL,NRIVER,LCHANI,LCHKCC,
     &                    LCHUFTHK,NHUF,LCGS,LCVDHT,LCDVDH,
     &                    LCWELL,NWELVL,MXWELL,NWELLS,ISEN,IADVHUF)
C-----ALLOCATE SPACE FOR ALL OBSERVATIONS AND FOR RESIDUALS RELATED TO
C     OBSERVATIONS AND PRIOR INFORMATION. ALSO INITIALIZE SOME ARRAYS
      IF (IOBS.GT.0)
     &    CALL OBS1BAS6AC(EV,ISUMX,ISUMZ,ISUMIX,LCTOFF,NH,LCH,ND,
     &                    LCHOBS,LCWT,NDMH,NDMHAR,LCWTQ,LCWTQS,LCW1,
     &                    LCW2,LCX,NPLIST,LCXD,IPAR,IOUTG,IDRY,
     &                    JDRY,NQ,NQAR,NQC,NQCAR,NQT,NQTAR,NHAR,MOBS,
     &                    MOBSAR,LCIBT,LCNQOB,LCNQCL,LCIQOB,LCQCLS,
     &                    LCIPLO,LCIPLP,IPR,MPR,IPRAR,LCBUF1,LCSSTO,
     &                    ITMXP,LBUFF,LCOBSE,ISOLDX,ISOLDZ,ISOLDI,MXSEN,
     &                    LCBUF2,NDAR,NHT,LCRSQA,LCRSPA,LCXND,LCOTIM)
C
C------DYNAMICALLY ALLOCATE X, Z, IX, XHS, NIPRNAM, EQNAM, NAMES, AND
C      OBSNAM ARRAYS FOR OBS, SEN, AND PES PROCESSES; SOLVERS; AND
C      PACKAGES THAT DO ALLOCATION ONCE ONLY.  FOR STATIC MEMORY
C      ALLOCATION, THE FOLLOWING LINES, THROUGH THE ALLOCATE STATEMENTS,
C      MUST BE COMMENTED OUT
      LENX = ISUMX - 1
      IF(LENX.LT.1) LENX=1
      LENZ = ISUMZ - 1
      IF(LENZ.LT.1) LENZ=1
      LENIX = ISUMIX - 1
      IF(LENIX.LT.1) LENIX=1
      IF (ISEN.NE.0 .AND. IUHEAD.LE.0 .AND. MXSEN.GT.0) THEN
        LENXHS = NCOL*NROW*NLAY*MXSEN
      ELSE
        LENXHS = 1
      ENDIF
      NDD = NDAR
      MPRD = MPRAR
      IPRD = IPRAR
      ALLOCATE (X(LENX),Z(LENZ),IX(LENIX),XHS(LENXHS))
      ALLOCATE (NIPRNAM(IPRAR),EQNAM(MPRAR),NAMES(ND+IPRAR+MPRAR),
     &          OBSNAM(NDAR))
C
C------IF THE ARRAYS ARE NOT BIG ENOUGH THEN STOP.
      CALL MEMCHK(ISUMX,ISUMIX,ISUMZ,LENX,LENIX,LENZ,IOUTG,ISEN,IUHEAD,
     &            LENXHS,NCOL,NROW,NLAY,MXSEN,IERR,IERRU,NDD,NDAR,MPRD,
     &            MPRAR,IPRD,IPRAR)
      IF (IERR.GT.0) CALL PLL1SD(IERR,IERRU,IOUT,IOUTG)
C
      IF (ISEN.GT.0 .OR. ISENALL.LT.0 .OR. IBEFLG.EQ.2)
     &    CALL SEN1BAS6RP(X(LCBL),X(LCBU),FAC,IX(LCISEN),IOUTG,
     &                    IUNIT(25),IX(LCLN),NPE,NPLIST,DETWTP,ISENALL,
     &                    X(LCBSCA),MXSEN)
      IF (IPES.GT.0 .OR. IBEFLG.EQ.2)
     &    CALL PES1BAS6RP(IUNIT(26),IOUTG,NPE,X(LCWP),IX(LCLN),DMAX,
     &                    Z(LCDD),FCONV,EV,MPR,X(LCPRM),IX(LCISEN),
     &                    NPLIST,X(LCWTP),X(LCWTPS),Z(LCW3),Z(LCW4),IPR,
     &                    IX(LCNIPR),DETWTP,ND,ADMX,AP,DMX,NIPRNAM,
     &                    EQNAM,MPRAR,IPRAR,IX(LCIPNG),NPNG,NPNGAR,
     &                    IX(LCIPLO),NAMES,PARNEG,MXPAR,LBUFF,FSTAT,
     &                    X(LCBPRI),IERR,IYCFLG,IX(LCNPAR),ITMXP,IBEFLG)
C
C-----INITIALIZE ARRAYS USED FOR OBSERVATION PROCESS
      IF (IOBS.GT.0) CALL OBS1BAS6RP(ND,NDAR,NDMH,NDMHAR,NQCAR,
     &                               X(LCQCLS),RSQO,RSQOO,RSQP,X(LCWT),
     &                               X(LCWTQ),X(LCWTQS),X(LCOTIM))

C
C-----READ AND PREPARE INFORMATION FOR OBSERVATIONS
C
C-----READ HEAD OBSERVATION DATA
      IF (IUNIT(28).GT.0)
     &    CALL OBS1BAS6HRP(NCOL,NROW,NLAY,NPER,IUNIT(28),IOUTG,OBSNAM,
     &                    NH,IX(LCNDER),JT,IX(LCJOFF),IX(LCIOFF),
     &                    X(LCHOBS),X(LCWT),GX(LCDELR),GX(LCDELC),
     &                    X(LCRINT),X(LCCOFF),X(LCROFF),IX(LCMLAY),
     &                    X(LCPR),MOBS,IERR,X(LCTOFF),EV,EVH,MAXM,NSTP,
     &                    PERLEN,TSMULT,ISSFLG,ITRSS,NHAR,MOBSAR,
     &                    IX(LCIPLO),NAMES,ND,IPR,MPR,X(LCOTIM))
C-----READ HEAD-DEPENDENT-BOUNDARY FLOW-OBSERVATION DATA
      IF (IUNIT(33).GT.0)
     &    CALL OBS1DRN6RP(NCOL,NROW,NPER,IUNIT(33),IOUTG,OBSNAM,NHT,JT,
     &                    IX(LCIBT),IX(LCNQOB),IX(LCNQCL),
     &                    IX(LCIQOB),X(LCQCLS),IERR,X(LCHOBS),X(LCTOFF),
     &                    X(LCWTQ),IOWTQ,IPRN,NDMH,NSTP,PERLEN,
     &                    TSMULT,ISSFLG,ITRSS,NQAR,NQCAR,
     &                    NQTAR,IQ1,NQT1,NDD,IUNIT(3),NQDR,NQTDR,NT,
     &                    NC,IX(LCIPLO),NAMES,ND,IPR,MPR,IOWTQDR,
     &                    X(LCOTIM))
      IF (IUNIT(34).GT.0)
     &    CALL OBS1RIV6RP(NCOL,NROW,NPER,IUNIT(34),IOUTG,OBSNAM,
     &                    NH,JT,IX(LCIBT),IX(LCNQOB),
     &                    IX(LCNQCL),IX(LCIQOB),X(LCQCLS),IERR,
     &                    X(LCHOBS),X(LCTOFF),X(LCWTQ),IOWTQ,IPRN,
     &                    NDMH,NSTP,PERLEN,TSMULT,
     &                    ISSFLG,ITRSS,NQAR,NQCAR,NQTAR,IQ1,NQT1,
     &                    NDD,IUNIT(4),NQRV,NQTRV,NT,NC,IX(LCIPLO),
     &                    NAMES,ND,IPR,MPR,IOWTQRV,X(LCOTIM))
      IF (IUNIT(35).GT.0)
     &    CALL OBS1GHB6RP(NCOL,NROW,NPER,IUNIT(35),IOUTG,OBSNAM,
     &                    NHT,JT,IX(LCIBT),IX(LCNQOB),
     &                    IX(LCNQCL),IX(LCIQOB),X(LCQCLS),IERR,
     &                    X(LCHOBS),X(LCTOFF),X(LCWTQ),IOWTQ,IPRN,
     &                    NDMH,NSTP,PERLEN,TSMULT,
     &                    ISSFLG,ITRSS,NQAR,NQCAR,NQTAR,IQ1,NQT1,
     &                    NDD,IUNIT(7),NQGB,NQTGB,NT,NC,IX(LCIPLO),
     &                    NAMES,ND,IPR,MPR,IOWTQGB,X(LCOTIM))
      IF (IUNIT(36).GT.0)
     &    CALL OBS1STR6RP(NPER,IUNIT(36),IOUTG,OBSNAM,NHT,JT,
     &                    IX(LCIBT),IX(LCNQOB),IX(LCNQCL),IX(LCIQOB),
     &                    X(LCQCLS),IERR,X(LCHOBS),X(LCTOFF),X(LCWTQ),
     &                    IOWTQ,IPRN,NDMH,NSTP,PERLEN,TSMULT,ISSFLG,
     &                    ITRSS,NQAR,NQCAR,NQTAR,IQ1,NQT1,IUNIT(18),
     &                    NQST,NQTST,NT,NC,IX(LCIPLO),NAMES,ND,IPR,
     &                    MPR,IOWTQST,X(LCOTIM))
      IF (IUNIT(38).GT.0)
     &    CALL OBS1BAS6FRP(NCOL,NROW,NPER,IUNIT(38),IOUTG,OBSNAM,
     &                     NHT,JT,IX(LCIBT),IX(LCNQOB),
     &                     IX(LCNQCL),IX(LCIQOB),X(LCQCLS),IERR,
     &                     X(LCHOBS),X(LCTOFF),X(LCWTQ),IOWTQ,IPRN,
     &                     NDMH,NSTP,PERLEN,TSMULT,ISSFLG,ITRSS,NQAR,
     &                     NQCAR,NQTAR,IQ1,NQT1,NDD,NQCH,NQTCH,NT,NC,
     &                     IX(LCIPLO),NAMES,ND,IPR,MPR,IOWTQCH,NLAY,
     &                     X(LCOTIM))
      IF (IUNIT(41).GT.0)
     &    CALL OBS1DRT1RP(NCOL,NROW,NPER,IUNIT(41),IOUTG,OBSNAM,NHT,JT,
     &                    IX(LCIBT),IX(LCNQOB),IX(LCNQCL),
     &                    IX(LCIQOB),X(LCQCLS),IERR,X(LCHOBS),X(LCTOFF),
     &                    X(LCWTQ),IOWTQ,IPRN,NDMH,NSTP,PERLEN,
     &                    TSMULT,ISSFLG,ITRSS,NQAR,NQCAR,
     &                    NQTAR,IQ1,NQT1,NDD,IUNIT(40),NQDT,NQTDT,NT,
     &                    NC,IX(LCIPLO),NAMES,ND,IPR,MPR,IOWTQDT,
     &                    X(LCOTIM))
C
C-----READ ADVECTIVE-TRANSPORT DATA
      IF (IUNIT(29).GT.0)
     &    CALL OBS1ADV2RP(IOUTG,NROW,NCOL,NLAY,
     &                    X(LCPRST),NPRST,NPTH,IX(LCNPNT),NTT2,NH,NQT,
     &                    OBSNAM,IX(LCICLS),X(LCPOFF),X(LCTT2),
     &                    X(LCHOBS),GX(LCDELR),GX(LCDELC),X(LCWTQ),ND,
     &                    KTDIM,IUNIT(29),NDMH,IOWTQ,GX(LCBOTM),
     &                    NBOTM,IX(LCIPLO),NAMES,IPR,MPR,JT,NPADV,
     &                    INAMLOC,IPFLG,IADVHUF,NHUF,X(LCOTIM),
     &                    PERLEN,NPER,NSTP,ISSFLG,IADVPER)
C-----CHECK OBSERVATION DATA AGAINST ALLOCATED STORAGE
      IF (IOBS.GT.0) CALL OBS1BAS6CK(NC,ND,NQC,NT,NQT,IOUTG,OBSNAM)
C-----CHECK FOR ERRORS, CALCULATE THE WEIGHT MATRIX AND ITS SQUARE-ROOT
      IF (IPAR.GE.-1)
     &    CALL OBS1BAS6QM(NDMH,X(LCWTQ),X(LCWTQS),DTLWTQ,Z(LCW1),
     &                    Z(LCW2),EV,IOWTQ,IPRN,IOUTG,NDMHAR,OBSALL,
     &                    OUTNAM,ND,NH,X(LCWT))
C
C---------SOLVER PACKAGE
      IF(IUNIT(9).GT.0)
     1    CALL SIP5RPG(NPARM,MXITER,ACCL,HCLOSE,X(LCW),IUNIT(9),IPCALC,
     2                 IPRSIP,IOUTG,IFREFM)
      IF(IUNIT(10).GT.0)
     1    CALL DE45RPG(IUNIT(10),MXITER,NITER,ITMX,ACCL,HCLOSE,IFREQ,
     2                IPRD4,IOUTG,MUTD4)
      IF(IUNIT(11).GT.0)
     1    CALL SOR5RPG(MXITER,ACCL,HCLOSE,IUNIT(11),IPRSOR,IOUTG,IFREFM)
      IF(IUNIT(13).GT.0)
     1    CALL PCG2RPG(MXITER,ITER1,HCLOSE,RCLOSE,NPCOND,NBPOL,RELAX,
     2                IPRPCG,IUNIT(13),IOUTG,MUTPCG,NITER,DAMP,IFREFM)
      IF(IUNIT(14).GT.0)
     1    CALL LMG1RPG(IUNIT(14),MXITER,MXCYC,BCLOSE,DAMP,IOUTAMG,IOUTG,
     2                1,ICG,IADAMP,DUP,DLOW,HCLOSE)
C-----CHECK DATA AND CALCULATE CONVERGENCE CRITERIA FOR SENSITIVITIES
      IF (ISEN.GT.0)
     &    CALL SEN1BAS6CM(JT,IOUTG,IX(LCLN),X(LCB1),IERR,NPER,X(LCHCLO),
     &                    X(LCRCLO),HCLOSE,RCLOSE,IPAR,NPE,NPLIST,
     &                    IX(LCISEN),NSTP,PERLEN,TSMULT,IUNIT(10))
C
C-----READ AND PREPARE FOR PACKAGES WITH NO REWIND
      IF(IUNIT(23).GT.0)
     1    CALL GWF1LPF1RPGD(X(LCHK),X(LCVKA),X(LCVKCB),X(LCHANI),
     2                      X(LCSC1),X(LCSC2),IUNIT(23),ITRSS,NCOL,NROW,
     3                      NLAY,IOUTG,X(LCWETD),NPLPF,WETFCT,IWETIT,
     4                      IHDWET,IX(LCLAYF),GX(LCBOTM),NBOTM,
     5                      GX(LCDELR),GX(LCDELC),1,INAMLOC,
     6                      IX(LCISEN),ISEN,NPLIST)
      IF(IUNIT(37).GT.0)
     &    CALL GWF1HUF2RPGD(IUNIT(37),NCOL,NROW,NLAY,IOUTG,X(LCWETD),
     &                    WETFCT,IWETIT,IHDWET,IX(LCHGUF),
     &                    1,NHUF,NPHUF,X(LCHUFTHK),
     &                    ITRSS)
      IF(IUNIT(47).GT.0)
     &    CALL GWF1HUF2LVDA1RPGD(IUNIT(47),IOUTG,1,NHUF,NPLVDA,NLAY,
     &                           ISEN)
      IF(IUNIT(53).GT.0)
     &    CALL GWF1HUF2KDEP1RPGD(IUNIT(53),IOUTG,1,NPKDEP,IFKDEP,NROW,
     &                           NCOL,X(LCGS),GX(LCBOTM),NHUF)
C
C--SEAWAT: SET MT3D VERSION NUMBER TO 2
      IVER=2
      IF(IUNIT(58).GT.0) THEN
           CALL IMT1BTN4AL(IUNIT(58),IOUT,ISUMY,ISUMIY,NCOL,NROW,NLAY,
     &                   NCOMP,LCLAYC,LTDELR,LTDELC,LCHTOP,LCDZ,LCPR,
     &                   LCXBC,LCYBC,LCZBC,LCQX,LCQY,LCQZ,LCQSTO,LCDH,
     &                   LCIB,LCCOLD,LCCNEW,LCCWGT,LCCADV,LCRETA,LCSR,
     &                   LTBUFF,ISOTHM,LCRHOB,LCPRSITY2,LCRETA2)
C--SEAWAT: OPEN TEMP FILE AND INITIALIZE TIME VARIABLES FOR MT3D
           CALL OPENFL(-INUHF,0,'$file.umt ',1,'')
           HT1=0.
           HT2=0.
           NPS=1
C--SEAWAT: PASS IN ILTMHEAD2 FOR ILMTHEAD
           CALL LMT6BAS6VD(INUNIT,IOUT,NCOL,NROW,NLAY,NPER,ISSFLG(1),
     &          NODES,IUNIT,CUNIT,NIUNIT,IG(LCIBOU),INUHF,ILMTHEAD2)
           REWIND(INUHF)
           CALL IMT1FMI4AL(INUHF,IOUT,TRNOP,NPERFL,ISS,IVER)
           REWIND(INUHF)
      ENDIF
C
C--SEAWAT: ALLOCATE SPACE FOR THE MT3DMS ARRAYS
      IF(IUNIT(59).GT.0)
     &     CALL IMT1ADV4AL(IUNIT(59),IOUT,ISUMY,ISUMIY,NCOL,NROW,
     &                     NLAY,MCOMP,MIXELM,MXPART,PERCEL,NADVFD,
     &                     LCXP,LCYP,LCZP,LCINDX,LCINDY,LCINDZ,LCCNPT,
     &                     LCCHEK,TRNOP)
      IF(IUNIT(60).GT.0) 
     &     CALL IMT1DSP4AL(IUNIT(60),IOUT,ISUMY,ISUMIY,NCOL,NROW,NLAY,
     &                     LCAL,LCTRPT,LCTRPV,LCDM,LCDXX,LCDXY,LCDXZ,
     &                     LCDYX,LCDYY,LCDYZ,LCDZX,LCDZY,LCDZZ)
      IF(IUNIT(61).GT.0) 
     &     CALL IMT1SSM4AL(IUNIT(61),IOUT,ISUMY,ISUMIY,NCOL,NROW,NLAY,
     &                     NCOMP,LTIRCH,LTRECH,LTCRCH,LTIEVT,LTEVTR,
     &                     LTCEVT,MXSS,LCSSM,IVER,LCSSMC)
      IF(IUNIT(62).GT.0)
     &     CALL IMT1RCT4AL(IUNIT(62),IOUT,ISUMY,ISUMIY,NCOL,NROW,NLAY,
     &                     NCOMP,ISOTHM,IREACT,IRCTOP,IGETSC,LCRHOB,
     &                     LCPRSITY2,LCRETA2,LCFRAC,LCSP1,LCSP2,LCRC1,
     &                     LCRC2)
      IF(IUNIT(63).GT.0)
     &     CALL IMT1GCG4AL(IUNIT(63),IOUT,ISUMY,ISUMIY,NCOL,NROW,NLAY,
     &                     MXITERGC,ITER1GC,NCRS,ISOLVE,LCAGC,LCQ,LCWK,
     &                     LCCNCG,LCLRCHGC,LCRHSGC)

      IF(IUNIT(57).GT.0.OR.IUNIT(58).GT.0) THEN
           ALLOCATE(Y(0:ISUMY),IY(0:ISUMIY),STAT=IERR)
           IF (IERR.NE.0) THEN
             WRITE(*,*) 'FAILED TO ALLOCATE ENOUGH MEMORY FOR MT3DMS'
             STOP
           END IF
      ENDIF
C
C--SEAWAT: INITIALIZE MT3DMS VARIABLES
      IMPSOL=0
      IF(TRNOP(5)) THEN
        IMPSOL=1
        ISPD=1
        IF(MIXELM.EQ.0) ISPD=0
      ENDIF
C--SEAWAT: INITILIZE MT3DMS ARRAYS.
      DO I=1,ISUMY
        Y(I)=0.
      ENDDO
      DO I=1,ISUMIY
        IY(I)=0
      ENDDO
      DO IC=1,NCOMP
        DO I=1,122
          TMASIO(I,1,IC)=0.
          TMASIO(I,2,IC)=0.
        ENDDO
        DO I=1,4
          TMASS(I,1,IC)=0.
          TMASS(I,2,IC)=0.
          TMASS(I,3,IC)=0.
        ENDDO
      ENDDO
C
C-------BEGIN ITERATION LOOP FOR PARAMETER ESTIMATION
      DO 105, KITP = 1,ITMXP
        ITERP = KITP
C
C-------SET SENSITIVITY ARRAYS TO ZERO AND STORE ON DISK OR IN MEMORY
        IF (ISEN.GT.0) CALL SEN1BAS6ZS(IUHEAD,LENXHS,NCOL,NPE,NROW,NLAY,
     &                                 Z(LCSNEW),X(LCSOLD),XHS,
     &                                 X(LCSEND),NTIMES)
C-------LOOP TO HERE WHEN CONVERGENCE HAS BEEN ACHIEVED BY TOL CRITERION
 20     CONTINUE
        ITERPK = ITERPK + 1
        ICNVGP = 1
        IF (IPAR.GT.-3) THEN
C-------IF PARAMETER ESTIMATION HAS CONVERGED, SET ITERPF TO
C       CALCULATE HEAD WITH THE NEW PARAMETERS AND THEN STOP
          IF (IFO.GT.0) THEN
            ITERPF = ITERP
          ENDIF
C---------REWIND INPUT FILES
          IF (ITERPK.GT.1) THEN
              CALL PES1BAS6RW(INUNIT,FNAME,CUNIT,IREWND,NIUNIT,IOUT,
     2                        IOUTG,VERSION,IX(LCISEN),ITERP,ITERPF,
     3                        LASTX,NPLIST,ITERPK)
C--SEAWAT: REWIND MT3DMS INPUT FILE HERE
              IF(IUNIT(58).GT.0) THEN 
                 REWIND(IUNIT(58))
                 READ(IUNIT(58),*) DUMMY
                 READ(IUNIT(58),*) DUMMY
                 READ(IUNIT(58),*) DUMMY
                 READ(IUNIT(58),*) DUMMY
                 READ(IUNIT(58),*) DUMMY
                 IF(IUNIT(59).GT.0) REWIND(IUNIT(59))
                 IF(IUNIT(60).GT.0) REWIND(IUNIT(60))
                    IF(IUNIT(61).GT.0) THEN 
                          REWIND(IUNIT(61))
                          READ(IUNIT(61),*) DUMMY
                          READ(IUNIT(61),*) DUMMY
                    ENDIF                  
                 IF(IUNIT(62).GT.0) REWIND(IUNIT(62))
                 IF(IUNIT(63).GT.0) REWIND(IUNIT(63))
              ENDIF
C--SEAWAT: END CREATED IF THEN
            ENDIF
        ENDIF
C
C-------INITIALIZE H AND X ARRAYS, AND UNFLAG OMITTED OBSERVATIONS
        IF (IOBS.GT.0) CALL OBS1BAS6FM(X(LCH),ND,NDAR,NDMH,NDMHAR,
     &                                 X(LCWT),X(LCWTQ))
        IF (ISEN.GT.0 .AND. (ITERPF.EQ.0 .OR. LASTX.GT.0))
     &      CALL OBS1BAS6DR(ND,NPE,X(LCX))
C4------ALLOCATE SPACE IN RX AND IR ARRAYS.
        CALL GWF1BAS6ALP(HEADNG,NPER,TOTIM,NCOL,NROW,NLAY,NODES,INBAS,
     1                   IOUT,IXSEC,ICHFLG,IFREFM,ISUMRX,ISUMIR,LCIOFL,
     2                   ISTRT,IAPART)
        IF(IUNIT(1).GT.0)
     1      CALL GWF1BCF6ALP(ISUMRX,LCSC1,LCHY,LCSC2,LCTRPY,ITRSS,ISS,
     2                       IUNIT(1),NCOL,NROW,NLAY,IOUT,IBCFCB,LCWETD,
     3                       IWDFLG,LCCVWD,WETFCT,IWETIT,IHDWET,HDRY,
     4                       IAPART,IFREFM,LAYHDT)
        IF(IUNIT(2).GT.0)
     1      CALL GWF1WEL6ALP(ISUMRX,LCWELL,MXWELL,NWELLS,IUNIT(2),IOUT,
     2                       IWELCB,NWELVL,IWELAL,IFREFM,NPWEL,IPWBEG,
     3                       NNPWEL,NOPRWL)
        IF(IUNIT(3).GT.0)
     1      CALL GWF1DRN6ALP(ISUMRX,LCDRAI,MXDRN,NDRAIN,IUNIT(3),IOUT,
     2                       IDRNCB,NDRNVL,IDRNAL,IFREFM,NPDRN,IDRNPB,
     3                       NNPDRN,NOPRDR)
        IF(IUNIT(4).GT.0)
     1      CALL GWF1RIV6ALP(ISUMRX,LCRIVR,MXRIVR,NRIVER,IUNIT(4),IOUT,
     2                       IRIVCB,NRIVVL,IRIVAL,IFREFM,NPRIV,IRIVPB,
     3                       NNPRIV,NOPRRV)
        IF(IUNIT(5).GT.0)
     1      CALL GWF1EVT6ALP(ISUMRX,ISUMIR,LCIEVT,LCEVTR,LCEXDP,LCSURF,
     2                       NCOL,NROW,NEVTOP,IUNIT(5),IOUT,IEVTCB,
     3                       IFREFM,NPEVT,IEVTPF)
        IF(IUNIT(7).GT.0)
     1      CALL GWF1GHB6ALP(ISUMRX,LCBNDS,MXBND,NBOUND,IUNIT(7),IOUT,
     2                       IGHBCB,NGHBVL,IGHBAL,IFREFM,NPGHB,IGHBPB,
     3                       NNPGHB,NOPRGB)
        IF(IUNIT(8).GT.0)
     1      CALL GWF1RCH6ALP(ISUMRX,ISUMIR,LCIRCH,LCRECH,NRCHOP,NCOL,
     2                       NROW,IUNIT(8),IOUT,IRCHCB,IFREFM,NPRCH,
     3                       IRCHPF)
        IF(IUNIT(16).GT.0)
     1      CALL GWF1FHB1ALP(ISUMRX,ISUMIR,LCFLLC,LCBDTM,LCFLRT,LCBDFV,
     2                  LCBDHV,LCHDLC,LCSBHD,NBDTIM,NFLW,NHED,IUNIT(16),
     3                  IOUT,IFHBCB,NFHBX1,NFHBX2,IFHBD3,IFHBD4,IFHBD5,
     4                  IFHBSS,ITRSS,NHEDDIM,NFLWDIM,NBDHVDIM)
        IF(IUNIT(18).GT.0)
     1      CALL GWF1STR6ALP(ISUMRX,ISUMIR,LCSTRM_,ICSTRM_,MXSTRM,
     2                  NSTREM,IUNIT(18),IOUT,ISTCB1STR6,ISTCB2STR6,
     3                  NSSSTR6,NTRIB,NDIV,ICALC,CONSTSTR6,LCTBAR,
     4                  LCTRIB,LCIVAR_,LCFGAR,NPSTR,ISTRPB)
        IF(IUNIT(19).GT.0)
     1      CALL GWF1IBS6ALP(ISUMRX,LCHC,LCSCE,LCSCV,LCSUB,NCOL,
     2                  NROW,NLAY,IIBSCB,IIBSOC,IUNIT(19),IOUT,IBSDIM,
     &                  IUNIT(54))
        IF(IUNIT(54).GT.0)
     1      CALL GWF1SUB1ALP(NROW,NCOL,NLAY,ITERP,ISUBCB,ISUBOC,AC1,AC2,
     2                  ITMIN,NNDB,NDB,NPZ,NN,NND1,ND1,ND2,IDSAVE,
     3                  IDREST,ISSFLG,NPER,NSTP,NSTPT,IUNIT(54),IOUT,
     4                  IUNIT(9),LCV,ISEN)
        IF(IUNIT(20).GT.0)
     1      CALL GWF1CHD6ALP(ISUMRX,LCCHDS,NCHDS,MXCHD,IUNIT(20),IOUT,
     2                       NCHDVL,IFREFM,NPCHD,IPCBEG,NNPCHD,NOPRCH)
        IF (IUNIT(17).GT.0)
     &      CALL GWF1RES1ALP(ISUMRX,LCIRES,LCIRSL,LCBRES,LCCRES,LCBBRE,
     &                  LCHRES,LCHRSE,IUNIT(17),IOUT,NRES,IRESCB,NRESOP,
     &                  IRESPT,NPTS,NCOL,NROW,ISUMIR)
        IF (IUNIT(21).GT.0)
     &      CALL GWF1HFB6ALP(IUNIT(21),IOUT,ISUMRX,LCHFB,MXACTFB,NHFBNP,
     &                       NPHFB,MXHFB,IHFB,NOPRHB)
CLAK
        IF(IUNIT(22).GT.0)
     1               CALL GWF1LAK3ALP(ISUMRX,ISUMIR,LCCOND,ICLAKE,
     2     MXLKND,LKNODE,LCSTAG,IUNIT(22),IOUT,ILKCB,NLAKES,INTRB,
     3     INDV,LCCNDF,LCLKPR,LCLKEV,ISTGLD,ISTGNW,IICS,IISUB,ISILL,
     4     LCWTDR,IFREFM,NROW,NCOL,NLAY,IBNLK,ILKBL,LKACC1,LKACC2,
     5     LKACC3,LKACC4,LKACC5,LKACC6,LKACC7,LKACC8,LKACC9,LKACC10,
     6     LKACC11,LKDRY,IBTMS,LKNCNT,LKKSUB,LKSADJ,LKFLXI,LKNCNS,LKSVT,
     7     LKJCLS,THETA,LCRNF,ITRSS,NSSITR,SSCNCR,LKSSMN,LKSSMX,LKNCN,
     8     LKDSR,LKCNN,LKCHN,IAREN,IUNIT(44),LSOVOL,NSS,IUNIT(15),
     9     LSLAKE,LSPPT,LSRNF,LSAUG,NSOL,IMSUB,IMSUB1,LSCGWL,LSSLAK,
     *     LSSWIN,LSSWOT,LSSPPT,LSCDRW,LSSRUN,
     *     LSGWIN,LSGWOT,LSLKSM,LSKLK,LSDONE,LSFLOB,LSRTCO,LSCLKO,
     *     LSALKI,LSALKO,ISTRIN,ISTROT,LKLMRR,IDSTRT,
     *     LKVI,ISTGLD2,LKCLKI,
     *     LKCUM1,LKCUM2,LKCUM3,LKCUM4,LKCUM5,
     *     LKCUM6,LKCUM7,LKCUM8,LKCUM9)
CLAK
        CALL GWF1GAG5ALP(IUNIT(46),ISUMIR,LSGAGE,NUMGAGE,IOUT,IUNIT(44),
     &                   IUNIT(22),LKACC7,LCSTAG,LSLAKE,ICSTRM,
     &                   NSTRM,NLAKES)
        IF(IUNIT(39).GT.0)
     &      CALL GWF1ETS1ALP(ISUMRX,ISUMIR,LCIETS,LCETSR,LCETSX,LCETSS,
     &                       NCOL,NROW,NETSOP,IUNIT(39),IOUT,IETSCB,
     &                       IFREFM,NPETS,IETSPF,NETSEG,LCPXDP,LCPETM,
     &                       NSEGAR)
        IF(IUNIT(40).GT.0)
     &      CALL GWF1DRT1ALP(ISUMRX,LCDRTF,MXDRT,NDRTCL,IUNIT(40),IOUT,
     &                       IDRTCB,NDRTVL,IDRTAL,IFREFM,NPDRT,IDRTPB,
     &                       NDRTNP,IDRTFL,NOPRDT)
        IF (IUNIT(43).GT.0)
     &      CALL GWF1HYD1ALP(ISUMRX,LCHYDM,NHYDM,IHYDMUN,HYDNOH,
     &                      IUNIT(43),IOUT)
        IF(IUNIT(51).GT.0)
     1      CALL GWF1DAF1ALP(IERR,IUNIT(52)+1,IUNIT(52),IUNIT(51),IOUT,
     2                      IDAFCB,IDAFBK)
        IF(IUNIT(50).GT.0) THEN
          CALL GWF1MNW1AL(ISUMRX,LCWEL2,MXWEL2,NWELL2,LCHREF,NODES,
     &                     KSPREF,IUNIT(50),IOUT,IWL2CB,IOWELL2,
     &                     NOMOITER,PLOSSMNW,MNWNAME,FNAME)
C
C         Allocate array for MNW1 site IDs
          IF (ITERPK.EQ.1) ALLOCATE (MNWSITE(MXWEL2))
        ENDIF
C
C------DYNAMICALLY ALLOCATE RX AND IR ARRAYS FOR PACKAGES THAT DO
C      ALLOCATION EVERY PARAMETER-ESTIMATION ITERATION.  FOR STATIC
C      MEMORY ALLOCATION, THE FOLLOWING IF...THEN BLOCK MUST BE
C      COMMENTED OUT
       IF (ITERPK.EQ.1) THEN
         LENRX = ISUMRX - 1
         IF(LENRX.LE.0) LENRX=1
         LENIR = ISUMIR - 1
         IF(LENIR.LE.0) LENIR=1
         ALLOCATE (RX(LENRX),IR(LENIR))
       ENDIF
C
C5------IF THE ARRAYS ARE NOT BIG ENOUGH THEN STOP.
        CALL MEMCHKR(ISUMRX,ISUMIR,LENRX,LENIR,IOUT,IERR,IERRU)
        IF (IERR.GT.0) CALL PLL1SD(IERR,IERRU,IOUT,IOUTG)
C
C--SEAWAT: ALLOCATE SPACE IN VDF ARRAY
        IF (IUNIT(57).GT.0) THEN
          CALL VDF1AL(IOUT,NCOL,NROW,NLAY,ISUMVDF,LPS,LELEV,
     &                      LRHOCR,LRHOCC,LRHOCV,LDCDT,LHSALT,LPSOLD)
          LENVDF=ISUMVDF-1
          IF(LENVDF.LE.0) LENVDF=1
          ALLOCATE (VDF(LENVDF))
        ENDIF 
C
C6------READ AND PREPARE INFORMATION FOR ENTIRE SIMULATION.
C---------BASIC PACKAGE
        CALL GWF1BAS6RPP(IG(LCIBOU),GZ(LCHNEW),GX(LCSTRT),INBAS,HEADNG,
     1                   NCOL,NROW,NLAY,VBVL,IR(LCIOFL),IUNIT(12),
     2                   IHEDFM,IDDNFM,IHEDUN,IDDNUN,IOUT,IPEROC,ITSOC,
     3                   CHEDFM,CDDNFM,IBDOPT,IXSEC,LBHDSV,LBDDSV,
     4                   IFREFM,IBOUUN,LBBOSV,CBOUFM,HNOFLO,NIUNIT,ITS,
     5                   IAUXSV,RESETDD,RESETDDNEXT)
        IF(IUNIT(1).GT.0)
     1      CALL GWF1BCF6RPP(IG(LCIBOU),GZ(LCHNEW),RX(LCSC1),RX(LCHY),
     2                       GX(LCCR),GX(LCCC),GX(LCCV),GX(LCDELR),
     3                       GX(LCDELC),RX(LCSC2),RX(LCTRPY),IUNIT(1),
     4                       ISS,NCOL,NROW,NLAY,IOUT,RX(LCWETD),IWDFLG,
     5                       RX(LCCVWD))
C-------SUBSTITUTE AND PREPARE FOR PACKAGES WITH NO REWIND
        IF(IUNIT(23).GT.0)
     1      CALL GWF1LPF1SP(IG(LCIBOU),GZ(LCHNEW),GX(LCCR),GX(LCCC),
     2                      GX(LCCV),GX(LCDELR),GX(LCDELC),GX(LCBOTM),
     3                      X(LCHK),X(LCVKA),X(LCVKCB),X(LCHANI),
     4                      X(LCSC1),X(LCSC2),ITRSS,NCOL,NROW,NLAY,IOUT,
     5                      X(LCWETD),NPLPF,NBOTM,GX(LCRMLT),IG(LCIZON),
     6                      NMLTAR,NZONAR,IX(LCLAYF),GX(LCBUFF),ITERPK)
C--SEAWAT: NO NEED FOR VDF1HUF2SP BECAUSE HNEW CONTAINS HEADS, NOT EQUIV. FRESH HEAD
        IF(IUNIT(37).GT.0)
     1     CALL GWF1HUF2SP(IG(LCIBOU),GZ(LCHNEW),GX(LCCR),GX(LCCC),
     2                      GX(LCCV),GX(LCDELR),GX(LCDELC),GX(LCBOTM),
     3                      X(LCHK),X(LCVKA),X(LCSC1),ITRSS,NCOL,NROW,
     4                      NLAY,IOUT,X(LCWETD),NHUF,NBOTM,GX(LCRMLT),
     5                      IG(LCIZON),NMLTAR,NZONAR,X(LCHUFTHK),
     6                      X(LCHKCC),HDRY,0,0,0,IX(LCHGUF),
     7                      X(LCHUFTMP),IUNIT(47),
     8                      X(LCVDHD),X(LCVDHT),IWETIT,
     9                      IHDWET,WETFCT,X(LCGS),X(LCA9))
C---------FLOW-SIMULATION OPTIONS
        IF(IUNIT(2).GT.0)
     1      CALL GWF1WEL6RPPD(IUNIT(2),IOUTG,NWELVL,IWELAL,NCOL,NROW,
     2                        NLAY,NPWEL,RX(LCWELL),IPWBEG,MXWELL,
     3                        IFREFM,ITERPK,INAMLOC,NOPRWL)
        IF(IUNIT(3).GT.0)
     1      CALL GWF1DRN6RPPD(IUNIT(3),IOUTG,NDRNVL,IDRNAL,NCOL,NROW,
     2                        NLAY,NPDRN,RX(LCDRAI),IDRNPB,MXDRN,IFREFM,
     &                        ITERPK,INAMLOC,NOPRDR)
        IF(IUNIT(4).GT.0)
     1      CALL GWF1RIV6RPPD(IUNIT(4),IOUTG,NRIVVL,IRIVAL,NCOL,NROW,
     2                        NLAY,NPRIV,RX(LCRIVR),IRIVPB,MXRIVR,
     3                        IFREFM,ITERPK,INAMLOC,NOPRRV)
        IF(IUNIT(5).GT.0)
     &      CALL GWF1EVT6RPPD(IUNIT(5),IOUTG,NPEVT,ITERPK,INAMLOC)
        IF(IUNIT(7).GT.0)
     1      CALL GWF1GHB6RPPD(IUNIT(7),IOUTG,NGHBVL,IGHBAL,NCOL,NROW,
     2                        NLAY,NPGHB,RX(LCBNDS),IGHBPB,MXBND,IFREFM,
     &                        ITERPK,INAMLOC,NOPRGB)
        IF(IUNIT(8).GT.0)
     &      CALL GWF1RCH6RPPD(IUNIT(8),IOUTG,NPRCH,ITERPK,INAMLOC)
        IF(IUNIT(16).GT.0)
     &      CALL GWF1FHB1RPP(IG(LCIBOU),NROW,NCOL,NLAY,IR(LCFLLC),
     &                   RX(LCBDTM),NBDTIM,RX(LCFLRT),NFLW,NHED,
     &                   IR(LCHDLC),RX(LCSBHD),IUNIT(16),IOUT, NFHBX1,
     &                   NFHBX2,IFHBD3,IFHBD5,NHEDDIM,NFLWDIM)
        IF(IUNIT(18).GT.0)
     1      CALL GWF1STR6RPPD(IUNIT(18),IOUTG,NCOL,NROW,NLAY,NPSTR,
     2                  RX(LCSTRM_),IR(ICSTRM_),ISTRPB,MXSTRM,ITERPK,
     &                  INAMLOC)
        IF(IUNIT(19).GT.0)
     1      CALL GWF1IBS6RPP(GX(LCDELR),GX(LCDELC),GZ(LCHNEW),RX(LCHC),
     2                  RX(LCSCE),RX(LCSCV),RX(LCSUB),NCOL,NROW,
     3                  NLAY,NODES,IIBSOC,ISUBFM,ICOMFM,IHCFM,
     4                  ISUBUN,ICOMUN,IHCUN,IUNIT(19),IOUT,IBSDIM)
        IF(IUNIT(54).GT.0)
     1      CALL GWF1SUB1RPP(GX(LCDELR),GX(LCDELC),GZ(LCHNEW),
     2                  GX(LCBUFF),NCOL,NROW,NLAY,NODES,NPER,NSTP,
     3                  ISUBOC,NND1,ND1,ND2,NDB,NNDB,NPZ,NN,IDSAVE,
     4                  IDREST,NSTPT,IUNIT(54),IOUT)
        IF(IUNIT(20).GT.0)
     1      CALL GWF1CHD6RPPD(IUNIT(20),IOUTG,NCHDVL,NCOL,NROW,NLAY,
     2                        NPCHD,RX(LCCHDS),IPCBEG,MXCHD,IFREFM,
     &                        ITERPK,INAMLOC,NOPRCH)
C
        IF (IUNIT(21).GT.0)
     &      CALL GWF1HFB6RPPA(GX(LCBOTM),GX(LCCR),GX(LCCC),GX(LCDELR),
     &                        GX(LCDELC),RX(LCHFB),IUNIT(21),MXACTFB,
     &                        NBOTM,NCOL,NROW,NLAY,NODES,NHFBNP,NHFB,
     &                        NPHFB,IOUT,IOUTG,ITERPK,MXHFB,IHFB,LAYHDT,
     &                        INAMLOC,NOPRHB)
        IF(IUNIT(39).GT.0)
     &      CALL GWF1ETS1RPPD(IUNIT(39),IOUTG,NPETS,ITERPK,INAMLOC)
        IF(IUNIT(40).GT.0)
     &      CALL GWF1DRT1RPPD(IUNIT(40),IOUTG,NDRTVL,IDRTAL,NCOL,NROW,
     &                        NLAY,NPDRT,RX(LCDRTF),IDRTPB,MXDRT,IFREFM,
     &                        ITERPK,IDRTFL,INAMLOC,NOPRDT)
CLAK
C  REVISED IF STATEMENT
C       IF(IUNIT(46).GT.0.AND.(IUNIT(44).GT.0.OR.IUNIT(22).GT.0))
        IF(IUNIT(46).GT.0)
     &      CALL GWF1GAG5RPP(IR(LSGAGE),NUMGAGE,IOUT,IUNIT(46))
C
C-------CHECK THAT PARAMETER DEFINITIONS ARE COMPLETE
        IF (ITERPK.EQ.1) CALL GLO1BAS6CK(IOUTG,ISEN,NPLIST)
        IF ((ISEN.GT.0 .OR. IBEFLG.EQ.2) .AND. ITERPK.EQ.1)
     &      CALL SEN1BAS6CP(IOUTG,NPLIST,ISENSU,CHEDFM)
        IF (IPES.GT.0)
     &      CALL PES1BAS6CK(X(LCBL),X(LCBU),IX(LCISEN),IOUB,IOUTG,
     &                      IX(LCIPNG),IX(LCLN),NPNG,NPLIST,NPNGAR,
     &                      ITERPK,FAC,FCONV,AP,ADMX,TOL,LAYHDT,NLAY,
     &                      X(LCBSCA),X(LCPARE),ITMXP)
        IF (IUNIT(43).GT.0)
     &      CALL GWF1HYD1RPP(RX(LCHYDM),GX(LCSTRT),NHYDM,NUMH,
     &                       GX(LCDELR),GX(LCDELC),NCOL,NROW,NLAY,
     &                       LCHNEW,LCIBOU,IUNIT(43),IOUT)
        IF(IUNIT(43).GT.0 .AND. IUNIT(19).GT.0)
     &      CALL GWF1HYD1IBS2RPP(RX(LCHYDM),NHYDM,NUMH,GX(LCDELR),
     &                       GX(LCDELC),NCOL,NROW,NLAY,LCIBOU,LCSUB,
     &                       LCHC,IUNIT(43),IOUT)
C
C--SEAWAT: READ AND PREPARE MT3DMS INFORMATION (CONSTANT FOR ENTIRE SIMULATION)
        IF (IUNIT(58).GT.0)
     &    CALL IMT1BTN4RP(IUNIT(58),IOUT,IUCN,IUCN2,IMOBS,IMAS,ICNF,
     &                    ICBM,NCOL,NROW,NLAY,NCOMP,ISOTHM,IY(LCLAYC),
     &                    Y(LTDELR),Y(LTDELC),Y(LCHTOP),Y(LCDZ),
     &                    Y(LCPR),IY(LCIB),Y(LCCOLD),Y(LCCNEW),
     &                    Y(LCCADV),CINACT,THKMIN,Y(LCXBC),Y(LCYBC),
     &                    Y(LCZBC),Y(LCRETA),RFMIN,Y(LTBUFF),MXPRS,
     &                    NPRS,TIMPRS,MXOBS,NOBS,NPROBS,LOCOBS,TUNIT,
     &                    LUNIT,MUNIT)
        IF(IUNIT(59).GT.0)
     &    CALL IMT1ADV4RP(IUNIT(59),IOUT,NCOL,NROW,NLAY,MCOMP,MIXELM,
     &                    MXPART,NADVFD,NCOUNT)
        IF(IUNIT(60).GT.0)
     &    CALL IMT1DSP4RP(IUNIT(60),IOUT,NCOL,NROW,NLAY,Y(LCAL),
     &                    Y(LCTRPT),Y(LCTRPV),Y(LCDM))
        IF(IUNIT(62).GT.0)
     &    CALL IMT1RCT4RP(IUNIT(62),IOUT,NCOL,NROW,NLAY,NCOMP,IY(LCIB),
     &                    Y(LCCOLD),Y(LCPR),ISOTHM,IREACT,IRCTOP,IGETSC,
     &                    Y(LCRHOB),Y(LCSP1),Y(LCSP2),Y(LCSR),Y(LCRC1),
     &                    Y(LCRC2),Y(LCRETA),Y(LTBUFF),Y(LCPRSITY2),
     &                    Y(LCRETA2),Y(LCFRAC),RFMIN,IFMTRF,DTRCT)
        IF(IUNIT(63).GT.0)
     &    CALL IMT1GCG4RP(IUNIT(63),IOUT,MXITERGC,ITER1GC,ISOLVE,ACCLGC,
     &                CCLOSE,IPRGCG)
C--SEAWAT: INITIALIZE SEAWAT VARIABLES
        IF(IUNIT(57).GT.0)
     &    CALL VDF1IZ(NCOL,NROW,NLAY,IG(LCIBOU),GX(LCBOTM),
     &                    NBOTM,VDF(LELEV),VDF(LHSALT),GZ(LCHNEW))
C7------SIMULATE EACH STRESS PERIOD.
        DO 100 KPER = 1, NPER
          KKPER = KPER

C--WRITE AN INDENTIFYING MESSAGE
         IF (IUNIT(58).GT.0) THEN
          WRITE(*,50) KKPER
          WRITE(IOUT,51) KKPER
          WRITE(IOUT,'(1X)')
   50     FORMAT(/1X,'STRESS PERIOD NO.',I5)
   51     FORMAT(//35X,62('+')/55X,'STRESS PERIOD NO.',I5.3/35X,62('+'))
         ENDIF
C
C--SEAWAT: STRESS TIMING CONTROLLED BY MT3DMS
         IF(IUNIT(58).GT.0) 
     &    CALL IMT1BTN4ST(IUNIT(58),IOUT,NSTP,MXSTP,TSLNGH,DT0,
     &                    MXSTRN,TTSMULT,TTSMAX,TUNIT)

          CALL GWF1BAS6ST(NSTP(KKPER),DELT,TSMULT(KKPER),PERTIM,KKPER,
     &                    IOUT,PERLEN(KKPER))
          IF(IUNIT(19).GT.0)
     1        CALL GWF1IBS6ST(ISSFLG,KKPER,GZ(LCHNEW),RX(LCHC),NCOL,
     2                        NROW,NLAY,IBSDIM,IOUT)
          IF(IUNIT(54).GT.0)
     1        CALL GWF1SUB1ST(GZ(LCHNEW),NNDB,NDB,ISSFLG,NROW,NCOL,
     1                        NODES,NPER,KPER,NN)
C
C--SEAWAT: READ MT3D SOURCE SINK INFO FOR THIS STRESS PERIOD    
         IF(IUNIT(61).GT.0)
     &    CALL IMT1SSM4RP(IUNIT(61),IOUT,KPER,NCOL,NROW,NLAY,NCOMP,
     &                    IY(LCIB),Y(LCCNEW),Y(LTCRCH),Y(LTCEVT),MXSS,
     &                    NSS,Y(LCSSM),Y(LCSSMC))

C--SEAWAT: READ VDF INFORMATION FOR THIS STRESS PERIOD
         IF(IUNIT(57).GT.0)
     &    CALL VDF1RP(IOUT,IUNIT(57),NCOL,NROW,NLAY,
     &                    GZ(LCHNEW),VDF(LHSALT),VDF(LPS),VDF(LELEV),
     &                    MTDNCONC,IFREFM,Y(LCCNEW),
     &                    NCOMP,CINACT,IG(LCIBOU),GX(LCBOTM),KKPER)

C7B-----READ AND PREPARE INFORMATION FOR STRESS PERIOD.
C----------READ USING PACKAGE READ AND PREPARE MODULES.
          IF(IUNIT(2).GT.0)
     &        CALL GWF1WEL6RPSS(RX(LCWELL),NWELLS,MXWELL,IUNIT(2),IOUT,
     1                          NWELVL,IWELAL,IFREFM,NCOL,NROW,NLAY,
     2                          NNPWEL,NPWEL,IPWBEG,NOPRWL)
          IF(IUNIT(3).GT.0)
     &        CALL GWF1DRN6RPSS(RX(LCDRAI),NDRAIN,MXDRN,IUNIT(3),IOUT,
     1                          NDRNVL,IDRNAL,IFREFM,NCOL,NROW,NLAY,
     2                          NNPDRN,NPDRN,IDRNPB,NOPRDR)
          IF(IUNIT(4).GT.0)
     &        CALL GWF1RIV6RPSS(RX(LCRIVR),NRIVER,MXRIVR,IUNIT(4),IOUT,
     1                          NRIVVL,IRIVAL,IFREFM,NCOL,NROW,NLAY,
     2                          NNPRIV,NPRIV,IRIVPB,NOPRRV)
          IF(IUNIT(5).GT.0)
     &        CALL GWF1EVT6RPSS(NEVTOP,IR(LCIEVT),RX(LCEVTR),RX(LCEXDP),
     1                          RX(LCSURF),GX(LCDELR),GX(LCDELC),NCOL,
     2                          NROW,IUNIT(5),IOUT,IFREFM,NPEVT,
     3                          GX(LCRMLT),IG(LCIZON),NMLTAR,NZONAR,
     &                          IEVTPF)
          IF(IUNIT(7).GT.0)
     &        CALL GWF1GHB6RPSS(RX(LCBNDS),NBOUND,MXBND,IUNIT(7),IOUT,
     1                          NGHBVL,IGHBAL,IFREFM,NCOL,NROW,NLAY,
     2                          NNPGHB,NPGHB,IGHBPB,NOPRGB)
          IF(IUNIT(8).GT.0)
     &        CALL GWF1RCH6RPSS(NRCHOP,IR(LCIRCH),RX(LCRECH),GX(LCDELR),
     1                          GX(LCDELC),NROW,NCOL,IUNIT(8),IOUT,
     2                          IFREFM,NPRCH,GX(LCRMLT),IG(LCIZON),
     &                          NMLTAR,NZONAR,IRCHPF)
          IF (IUNIT(17).GT.0)
     &        CALL GWF1RES1RPS(IR(LCIRES),IR(LCIRSL),RX(LCBRES),
     &                     RX(LCCRES),RX(LCBBRE),RX(LCHRSE),IG(LCIBOU),
     &                     GX(LCDELR),GX(LCDELC),NRES,NRESOP,NPTS,NCOL,
     &                     NROW,NLAY,PERLEN(KKPER),DELT,NSTP(KKPER),
     &                     TSMULT(KKPER),IUNIT(17),IOUT)
          IF (IUNIT(18).GT.0)
     &        CALL GWF1STR6RPSS(RX(LCSTRM_),IR(ICSTRM_),NSTREM,MXSTRM,
     &                    IUNIT(18),IOUT,IR(LCTBAR),NDIV,NSSSTR6,NTRIB,
     &                    IR(LCIVAR_),ICALC,IPTFLG,NCOL,NROW,NLAY,
     &                    NPSTR,ISTRPB)
          IF(IUNIT(20).GT.0)
     &        CALL GWF1CHD6RPSS(RX(LCCHDS),NCHDS,MXCHD,IG(LCIBOU),NCOL,
     &                          NROW,NLAY,IUNIT(20),IOUT,NCHDVL,IFREFM,
     &                          NNPCHD,NPCHD,IPCBEG,NOPRCH)
CLAK
          IF(IUNIT(22).GT.0) THEN
            CALL GWF1LAK3RPS(IR(ICLAKE),LKNODE,MXLKND,
     1        IUNIT(22),IOUT,NLAKES,RX(LCSTAG),RX(LCLKPR),RX(LCLKEV),
     2        RX(LCCOND),NTRB,NDV,IR(INTRB),IR(INDV),KKPER,
     3        GX(LCDELR),GX(LCDELC),
     4        NCOL,NROW,NLAY,IR(IICS),RX(LKACC7),GX(LCBOTM),NBOTM,
     5        IR(IISUB),RX(ISILL),ICMX,NCLS,RX(LCWTDR),LWRT,IFREFM,
     6        IR(IBNLK),RX(ILKBL),IR(IBNLK),RX(ILKBL),NODES,
     7        RX(IBTMS),RX(LCRNF),RX(IAREN),IUNIT(44),NSS,
     8        IUNIT(15),RX(LSLAKE),RX(LSAUG),RX(LSPPT),RX(LSRNF),
     9        NSOL,IOUTS,RX(LKSSMN),RX(LKSSMX),ISSFLG(KKPER),RX(LKVI),
     *        RX(LKCLKI),RX(LKCUM1),RX(LKCUM2),RX(LKCUM3),RX(LKCUM4),
     &        RX(LKCUM5),RX(LKCUM6),RX(LKCUM7),RX(LKCUM8),
     &        RX(LKCUM9))
            IF (IUNIT(1).GT.0) THEN
              CALL GWF1LAK3BCF6RPS(IOUT,RX(LCCOND),IR(IBNLK),
     1             IR(ICLAKE),RX(LCCNDF),GX(LCDELR),GX(LCDELC),
     2             RX(LCHY),RX(LCTRPY),LAYHDT,MXLKND,NCOL,NROW,NLAY,
     3             LKNODE,IWDFLG,RX(LCCVWD))
            ELSE IF (IUNIT(23).GT.0) THEN
              CALL GWF1LAK3LPF1RPS(IOUT,RX(LCCOND),IR(IBNLK),
     1             IR(ICLAKE),RX(LCCNDF),GX(LCDELR),GX(LCDELC),
     2             X(LCHK),X(LCHANI),LAYHDT,MXLKND,NCOL,NROW,NLAY,
     3             LKNODE,X(LCVKA),X(LCVKCB),GX(LCBOTM),NBOTM)
            ELSE IF(IUNIT(37).GT.0) THEN
              CALL GWF1LAK3HUF1RPS(IOUT,RX(LCCOND),IR(IBNLK),
     1             IR(ICLAKE),RX(LCCNDF),GX(LCDELR),GX(LCDELC),
     2             X(LCHK),X(LCHKCC),LAYHDT,MXLKND,NCOL,NROW,NLAY,
     3             LKNODE,X(LCVKA),GX(LCBOTM),NBOTM)
            ELSE
              WRITE(IOUT,*) 'LAK Package requires BCF, LPF, or HUF'
              CALL USTOP(' ')
            END IF
cc  Uncomment following call when SFR is added -- ERB 7/9/01
cc            IF (IUNIT(44).GT.0)
cc     &          CALL GWF1LAK3SFR1RPS(NTRB,NDV,NLAKES,IR(INTRB),IR(INDV),
cc     &                  NSS,IR(LCIVAR),IR(LCOTSG),IOUT,NODES,GX(LCBUFF))
          END IF
CLAK
          IF (IUNIT(46).GT.0.AND.
     &           (IUNIT(44).GT.0.OR.IUNIT(22).GT.0).AND.KKPER.EQ.1)
     &        CALL GWF1GAG5I(IR(LSGAGE),NUMGAGE,IOUT,IUNIT(15),
     &                       RX(LCSTAG),RX(LSLAKE),NLAKES,IR(ICSTRM),
     &                       NSTRM,DUM,NSOL,RX(LKACC7))
          IF(IUNIT(39).GT.0)
     &        CALL GWF1ETS1RPSS(NETSOP,IR(LCIETS),RX(LCETSR),RX(LCETSX),
     &                          RX(LCETSS),GX(LCDELR),GX(LCDELC),NCOL,
     &                          NROW,IUNIT(39),IOUT,IFREFM,NPETS,
     &                          GX(LCRMLT),IG(LCIZON),NMLTAR,NZONAR,
     &                          IETSPF,NETSEG,RX(LCPXDP),RX(LCPETM),
     &                          NSEGAR)
          IF(IUNIT(40).GT.0)
     &        CALL GWF1DRT1RPSS(RX(LCDRTF),NDRTCL,MXDRT,IUNIT(40),IOUT,
     &                          NDRTVL,IDRTAL,IFREFM,NCOL,NROW,NLAY,
     &                          NDRTNP,NPDRT,IDRTPB,IDRTFL,NRFLOW,
     &                          NOPRDT)
          IF(IUNIT(43).GT.0 .AND. IUNIT(18).GT.0 .AND. KPER.EQ.1)
     &        CALL GWF1HYD1STR6RPS(IR(ICSTRM_),RX(LCHYDM),NHYDM,NUMH,
     &                         GX(LCDELR),GX(LCDELC),NCOL,NROW,NLAY,
     &                         LCIBOU,LCSTRM_,NSTREM,IUNIT(43),IOUT,
     &                         MXSTRM)
          IF(IUNIT(43).GT.0 .AND. KPER.EQ.1)
     &        CALL GWF1HYD1OT(GZ,LENGZ,RX,LENRX,IG,LENIG,RX(LCHYDM),
     &                        NUMH,IHYDMUN,0.0,HYDNOH,NROW,NCOL,
     &                        ITMUNI,IOUT)
          IF(IUNIT(50).GT.0)
     &        CALL GWF1MNW1RP(MNWSITE,RX(LCWEL2),NWELL2,MXWEL2,
     &                         GX(LCHOLD),RX(LCHREF),IG(LCIBOU),
     &                         GX(LCDELR),GX(LCDELC),GX(LCCR),GX(LCCC),
     &                         RX(LCHY),GZ(LCHNEW),HCLOSE,SMALL,HDRY,
     &                         NODES,NROW,NCOL,KPER,KSPREF,IUNIT(50),
     &                         IOUT,IOWELL2,TOTIM,LAYHDT,GX(LCBOTM),
     &                         NBOTM,X(LCHK),IUNIT(1),IUNIT(23),
     &                         IUNIT(37),NLAY,PLOSSMNW,RX(LCTRPY),
     &                         X(LCHKCC),X(LCHANI))
C
C-----INITIALIZE SV ARRAY
          IF (ISEN.GT.0 .AND. IUNIT(23).GT.0)
     &        CALL SEN1LPF1SV(IG(LCIZON),KKPER,NCOL,NLAY,NMLTAR,NPLIST,
     &                        NROW,NZONAR,GX(LCRMLT),X(LCSV))
C
C7C-----SIMULATE EACH FLOW TIME STEP.
          DO 90 KSTP = 1, NSTP(KKPER)
            KKSTP = KSTP
C
C--SEAWAT: SET MT3D TIME VARIABLES AND PRINT TIME INFO
            IF (IUNIT(58).GT.0) THEN
			IF (FIRSTDT.EQ.0.) FIRSTDT=0.01
              DELT=TSLNGH(KSTP)
              HT1=HT2
              HT2=HT2+DELT
              TIME2=HT1
              WRITE(*,59) KKPER, KSTP, HT1,HT2
              WRITE(IOUT,61) HT1,HT2
              WRITE(IOUT,'(1X)')
   59         FORMAT(/1X,' STRESS PERIOD ',I4,' TIME STEP ', I4,
     &          ' FROM TIME =',G13.5,' TO ',G13.5/)
   61         FORMAT(//42X,48('=')/57X,'FROM TIME =',G13.5,' TO ',G13.5)
            ELSE
              MXSTRN=1
            ENDIF
C
C--SEAWAT:START OF SWT2K TRANSPORT STEP LOOP
          SWT2KTSLOOP: DO N=1,MXSTRN

C--SEAWAT: USE MT3D TO DETERMINE LENGTH OF TRANSPORT TIMESTEP
           IF(IUNIT(58).GT.0) THEN
             CALL IMT1BTN4AD(N,TRNOP,TIME1,TIME2,HT2,PERLEN,KSTP,NSTP,
     &                   MXPRS,TIMPRS,DT0,MXSTRN,MIXELM,DTRACK,DTRACK2,
     &                   PERCEL,DTDISP,DTSSM,DTRCT,RFMIN,NPRS,NPS,
     &                   DTRANS,PRTOUT,NCOL,NROW,NLAY,NCOMP,IY(LCIB),
     &                   Y(LCCNEW),Y(LCCOLD),CINACT,UPDLHS,IMPSOL,
     &                   TTSMULT,TTSMAX,KPER,Y(LTDELR),Y(LTDELC),
     &                   Y(LCDH),Y(LCPR),Y(LCSR),Y(LCRHOB),Y(LCRETA),
     &                   Y(LCPRSITY2),Y(LCRETA2),ISOTHM,TMASIO,RMASIO,
     &                   TMASS,FIRSTDT)
             DELT=DTRANS
             IF(DTRANS.EQ.0) THEN
               ICNVG=1
               ICNVGMT=1
             ENDIF
		   IF(DTRANS.EQ.0) GOTO 39
           ENDIF
C
C7C1----CALCULATE TIME STEP LENGTH. SET HOLD=HNEW.
           IF(IUNIT(58).EQ.0) THEN
            CALL GWF1BAS6AD(DELT,TSMULT(KKPER),TOTIM,PERTIM,GZ(LCHNEW),
     1                      GX(LCHOLD),KKSTP,NCOL,NROW,NLAY,ITS)
           ELSE
C--SEAWAT:  PASS 1 IN FOR TSMULT SO DELT DOES NOT CHANGE
            CALL GWF1BAS6AD(DELT,1.,TOTIM,PERTIM,GZ(LCHNEW),
     1                      GX(LCHOLD),KKSTP,NCOL,NROW,NLAY,ITS)
           ENDIF
C--SEAWAT: UPDATE CONSTANT HEADS FROM CHD PACKAGE
            IF (IUNIT(20).GT.0.AND.IUNIT(57).GT.0)
     &          CALL VDF1CHD6AD(NCHDS,MXCHD,RX(LCCHDS),GZ(LCHNEW),
     &                          GX(LCHOLD),PERLEN(KKPER),PERTIM,NCOL,
     &                          NROW,NLAY,NCHDVL,IOUT,VDF(LPS),
     &                          VDF(LELEV))
            IF (IUNIT(20).GT.0.AND.IUNIT(57).EQ.0)
     &          CALL GWF1CHD6AD(NCHDS,MXCHD,RX(LCCHDS),GZ(LCHNEW),
     &                          GX(LCHOLD),PERLEN(KKPER),PERTIM,NCOL,
     &                          NROW,NLAY,NCHDVL,IOUT)

            IF (IUNIT(1).GT.0)
     &          CALL GWF1BCF6AD(IG(LCIBOU),GX(LCHOLD),GX(LCBOTM),NBOTM,
     &                          RX(LCWETD),IWDFLG,ISSFLG(KKPER),NCOL,
     &                          NROW,NLAY)
            IF (IUNIT(23).GT.0)
     &          CALL GWF1LPF1AD(IG(LCIBOU),GX(LCHOLD),GX(LCBOTM),
     &                          X(LCWETD),ISSFLG(KKPER),NCOL,NROW,NLAY,
     &                          NBOTM)
            IF (IUNIT(37).GT.0)
     &          CALL GWF1HUF2AD(IG(LCIBOU),GX(LCHOLD),GX(LCBOTM),
     &                          X(LCWETD),ISSFLG(KKPER),NCOL,NROW,NLAY,
     &                          NBOTM)
            IF(IUNIT(16).GT.0.AND.IUNIT(57).EQ.0)
     &          CALL GWF1FHB1AD(GZ(LCHNEW),GX(LCHOLD),NCOL,NROW,NLAY,
     &                      ITRSS,TOTIM,DELT,RX(LCBDTM),NBDTIM,
     &                      RX(LCFLRT),RX(LCBDFV),RX(LCBDHV),NFLW,
     &                      RX(LCSBHD),IR(LCHDLC),NHED,NFHBX1,NFHBX2,
     &                      IFHBD3,IFHBD4,IFHBD5,IFHBSS,NHEDDIM,NFLWDIM,
     &                      NBDHVDIM)
C--SEAWAT: VARIABLE-DENSITY FHB
            IF(IUNIT(16).GT.0.AND.IUNIT(57).GT.0)
     &          CALL VDF1FHB1AD(GZ(LCHNEW),GX(LCHOLD),NCOL,NROW,NLAY,
     &                      ITRSS,TOTIM,DELT,RX(LCBDTM),NBDTIM,
     &                      RX(LCFLRT),RX(LCBDFV),RX(LCBDHV),NFLW,
     &                      RX(LCSBHD),IR(LCHDLC),NHED,NFHBX1,NFHBX2,
     &                      IFHBD3,IFHBD4,IFHBD5,IFHBSS,NHEDDIM,NFLWDIM,
     &                      NBDHVDIM,VDF(LPS),VDF(LELEV))
            IF (IUNIT(17).GT.0)
     &          CALL GWF1RES1AD(RX(LCHRES),RX(LCHRSE),IR(LCIRES),
     &                      RX(LCBRES),GX(LCDELR),GX(LCDELC),NRES,
     &                      IRESPT,NCOL,NROW,PERLEN(KKPER),PERTIM,TOTIM,
     &                      KKSTP,KKPER,IOUT)
CLAK
            IF (IUNIT(22).GT.0)
     1          CALL GWF1LAK3AD(KKPER,KKSTP,NLAKES,RX(ISTGLD),
     2                      RX(ISTGNW),RX(LCSTAG),NROW,NCOL,NLAY,
     3                      RX(LSFLOB),IUNIT(15),LKNODE,RX(ISTGLD2))
            IF(IUNIT(51).GT.0)
     1          CALL GWF1DAF1AD(DELT,IERR,ITMUNI,IUNIT(51),IOUT)
            IF(IUNIT(50).GT.0)
     &          CALL GWF1MNW1AD(NWELL2,MXWEL2,RX(LCWEL2),IG(LCIBOU),
     &                          GX(LCDELR),GX(LCDELC),GX(LCCR),GX(LCCC),
     &                          RX(LCHY),SMALL,HDRY,GZ(LCHNEW),NCOL,
     &                          NROW,NODES,LAYHDT,GX(LCBOTM),NBOTM,
     &                          X(LCHK),IUNIT(1),IUNIT(23),IUNIT(37),
     &                          NLAY,PLOSSMNW,RX(LCTRPY),X(LCHKCC),
     &                          X(LCHANI))
C
C---------INDICATE IN PRINTOUT THAT SOLUTION IS FOR HEADS
            CALL UMESPR('SOLVING FOR HEAD',' ',IOUT)
C-----------SHOW PROGRESS IF REQUESTED
            IF(SHOWPROG)THEN
              IF (ITERPK.GT.1 .AND. NPER.EQ.1) THEN
                WRITE (*,'(A)') ' '
              ENDIF
              WRITE(*,25)KPER,KSTP
            ENDIF
   25 FORMAT('+Solving:  Stress period: ',i5,4x,
     &       'Time step: ',i5,4x,'Ground-Water Flow Eqn.')
C
C--SEAWAT************************************************************
C******************FOR IMPLICIT COUPLING*****************************
            IF (MTDNCONC.GT.0.AND.NSWTCPL.GT.1.AND.MIXELM.NE.0) THEN
             WRITE(IOUT,*) 'MIXELM MUST BE 0 TO USE IMPLICIT COUPLING'
             STOP
            ENDIF

C--SEAWAT:RESET NSWTCPL
            IF(NSWTCPL.EQ.0) NSWTCPL=1
C--SEAWAT:START OF SWT2K IMPLICIT COUPLING LOOP
            SWT2KICPLOOP:  DO M=1,NSWTCPL
              IF (NSWTCPL.GT.1) THEN
                PRINT *
                PRINT *,'IMPLICIT COUPLING ITERATION ',M
                PRINT *
C               SET OLD DENSITY ARRAY (PSOLD) EQUAL TO NEW DENSITY ARRAY (PS)
                DO I=1,NODES
                  VDF(LPSOLD+I-1)=VDF(LPS+I-1)
                ENDDO
              ENDIF

C--SEAWAT******************FOR IMPLICIT COUPLING*********************
C********************************************************************
C
C7C2----ITERATIVELY FORMULATE AND SOLVE THE FLOW EQUATIONS.
C
            DO 30 KITER = 1, MXITER
             KKITER = KITER
C
C7C2A---FORMULATE THE FINITE DIFFERENCE EQUATIONS.
             CALL GWF1BAS6FM(GX(LCHCOF),GX(LCRHS),NODES)
C--SEAWAT: VARIABLE-DENSITY FM SUBROUTINES (IF VDF ACTIVE).
             IF(IUNIT(57).GT.0) THEN
C--SEAWAT: UPDATE THE SALTHEAD ARRAY
              CALL VDF1HSALT(NCOL,NROW,NLAY,GZ(LCHNEW),VDF(LHSALT),
     +                      VDF(LPS),VDF(LELEV),IG(LCIBOU))
              IF (IUNIT(1).GT.0)
     &           CALL VDF1BCF6FM(GX(LCHCOF),GX(LCRHS),GX(LCHOLD),
     &                            RX(LCSC1),GZ(LCHNEW),IG(LCIBOU),
     &                            GX(LCCR),GX(LCCC),GX(LCCV),RX(LCHY),
     &                            RX(LCTRPY),GX(LCBOTM),NBOTM,RX(LCSC2),
     &                            GX(LCDELR),GX(LCDELC),DELT,
     &                            ISSFLG(KKPER),KKITER,KKSTP,KKPER,NCOL,
     &                            NROW,NLAY,IOUT,RX(LCWETD),IWDFLG,
     &                            RX(LCCVWD),WETFCT,IWETIT,IHDWET,HDRY,
     &                            GX(LCBUFF),VDF(LPS),VDF(LELEV),
     &                            VDF(LHSALT))
              IF (IUNIT(23).GT.0)
     &            CALL VDF1LPF1FM(GX(LCHCOF),GX(LCRHS),GX(LCHOLD),
     &                            X(LCSC1),GZ(LCHNEW),IG(LCIBOU),
     &                            GX(LCCR),GX(LCCC),GX(LCCV),X(LCHK),
     &                            X(LCHANI),X(LCVKA),GX(LCBOTM),
     &                            X(LCSC2),GX(LCDELR),GX(LCDELC),DELT,
     &                            ISSFLG(KKPER),KKITER,KKSTP,KKPER,NCOL,
     &                            NROW,NLAY,IOUT,X(LCWETD),WETFCT,
     &                            IWETIT,IHDWET,HDRY,NBOTM,X(LCVKCB),
     &                            VDF(LPS),VDF(LELEV),VDF(LHSALT))
              IF (IUNIT(37).GT.0)
     &            CALL VDF1HUF2FM(GX(LCHCOF),GX(LCRHS),GX(LCHOLD),
     &                            X(LCSC1),GZ(LCHNEW),IG(LCIBOU),
     &                            GX(LCCR),GX(LCCC),GX(LCCV),X(LCHK),
     &                            X(LCVKA),GX(LCBOTM),GX(LCDELR),
     &                            GX(LCDELC),DELT,ITRSS,ISSFLG(KKPER),
     &                            NCOL,NROW,NLAY,IOUT,X(LCWETD),NBOTM,
     &                            NHUF,GX(LCRMLT),IG(LCIZON),NMLTAR,
     &                            NZONAR,X(LCHUFTHK),X(LCHKCC),HDRY,
     &                            KKITER,KSTP,KPER,X(LCHUFTMP),
     &                            IX(LCHGUF),
     &                            IUNIT(47),X(LCVDHD),X(LCVDHT),IWETIT,
     &                            IHDWET,WETFCT,X(LCGS),X(LCA9),
     &                            VDF(LHSALT),VDF(LPS),VDF(LELEV))
              IF (IUNIT(21).GT.0)
     &            CALL GWF1HFB6FM(GX(LCBOTM),GX(LCCC),GX(LCCR),
     &                            GX(LCDELC),GX(LCDELR),RX(LCHFB),
     &                            GZ(LCHNEW),MXACTFB,NBOTM,NCOL,NHFB,
     &                            NLAY,NROW,LAYHDT)
              IF (IUNIT(2).GT.0)
     &            CALL VDF1WEL6FM(NWELLS,MXWELL,GX(LCRHS),RX(LCWELL),
     &                            IG(LCIBOU),NCOL,NROW,NLAY,NWELVL,
     &                            VDF(LPS),MTDNCONC,MXSS,NSS,Y(LCSSM),
     &                            NCOMP,Y(LCSSMC),NSSVL)
              IF (IUNIT(3).GT.0)
     &            CALL VDF1DRN6FM(NDRAIN,MXDRN,RX(LCDRAI),GZ(LCHNEW),
     &                            GX(LCHCOF),GX(LCRHS),IG(LCIBOU),NCOL,
     &                            NROW,NLAY,NDRNVL,VDF(LPS),VDF(LELEV))
              IF (IUNIT(4).GT.0)
     &            CALL VDF1RIV6FM(NRIVER,MXRIVR,RX(LCRIVR),GZ(LCHNEW),
     &                            GX(LCHCOF),GX(LCRHS),IG(LCIBOU),NCOL,
     &                            NROW,NLAY,NRIVVL,VDF(LELEV),VDF(LPS),
     &                            MTDNCONC,MXSS,NSS,Y(LCSSM),NCOMP,
     &                            Y(LCSSMC),NSSVL)
              IF (IUNIT(5).GT.0)
     &           CALL VDF1EVT6FM(NEVTOP,IR(LCIEVT),RX(LCEVTR),
     &                          RX(LCEXDP),RX(LCSURF),GX(LCRHS),
     &                          GX(LCHCOF),IG(LCIBOU),GZ(LCHNEW),NCOL,
     &                          NROW,NLAY,VDF(LPS),VDF(LELEV),MTDNCONC,
     &                          Y(LTCEVT),Y(LCCNEW),NCOMP)

              IF (IUNIT(7).GT.0)
     &            CALL VDF1GHB6FM(NBOUND,MXBND,RX(LCBNDS),GX(LCHCOF),
     &                            GX(LCRHS),IG(LCIBOU),NCOL,NROW,NLAY,
     &                            NGHBVL,VDF(LPS),VDF(LELEV),GZ(LCHNEW),
     &                            MTDNCONC,MXSS,NSS,Y(LCSSM),NCOMP,
     &                            Y(LCSSMC),NSSVL)
              IF (IUNIT(8).GT.0) 
     &             CALL VDF1RCH6FM(NRCHOP,IR(LCIRCH),RX(LCRECH),
     &                          GX(LCRHS),IG(LCIBOU),NCOL,NROW,
     &                          NLAY,VDF(LPS),MTDNCONC,Y(LTCRCH),
     &                          NCOMP)
              IF(IUNIT(16).GT.0)
     &            CALL VDF1FHB1FM(GX(LCRHS),IG(LCIBOU),IR(LCFLLC),
     &                        RX(LCBDFV),NFLW,NCOL,NROW,NLAY,IFHBD4,
     &                        NFLWDIM,VDF(LPS),MTDNCONC,MXSS,NSS,
     &                        Y(LCSSM),NCOMP,Y(LCSSMC),NSSVL)
              CALL VDF1FM(NCOL,NROW,NLAY,GX(LCCR),GX(LCCC),
     &                        GX(LCCV),IG(LCIBOU),VDF(LPS),VDF(LELEV),
     &                        GX(LCRHS),GX(LCHCOF),GZ(LCHNEW),
     &                        GX(LCDELR),GX(LCDELC),GX(LCBOTM),NBOTM,
     &                        MFNADVFD,VDF(LRHOCR),VDF(LRHOCC),
     &                        VDF(LRHOCV),VDF(LDCDT),MTDNCONC,
     &                        VDF(LHSALT),IWTABLE,IUNIT(1))
C--SEAWAT: THE CONSTANT-DENSITY FM SUBROUTINES (FOR GWF PROCESS)
             ELSE 
              IF (IUNIT(1).GT.0)
     &            CALL GWF1BCF6FM(GX(LCHCOF),GX(LCRHS),GX(LCHOLD),
     &                            RX(LCSC1),GZ(LCHNEW),IG(LCIBOU),
     &                            GX(LCCR),GX(LCCC),GX(LCCV),RX(LCHY),
     &                            RX(LCTRPY),GX(LCBOTM),NBOTM,RX(LCSC2),
     &                            GX(LCDELR),GX(LCDELC),DELT,
     &                            ISSFLG(KKPER),KKITER,KKSTP,KKPER,NCOL,
     &                            NROW,NLAY,IOUT,RX(LCWETD),IWDFLG,
     &                            RX(LCCVWD),WETFCT,IWETIT,IHDWET,HDRY,
     &                            GX(LCBUFF))
              IF (IUNIT(23).GT.0)
     &            CALL GWF1LPF1FM(GX(LCHCOF),GX(LCRHS),GX(LCHOLD),
     &                            X(LCSC1),GZ(LCHNEW),IG(LCIBOU),
     &                            GX(LCCR),GX(LCCC),GX(LCCV),X(LCHK),
     &                            X(LCHANI),X(LCVKA),GX(LCBOTM),
     &                            X(LCSC2),GX(LCDELR),GX(LCDELC),DELT,
     &                            ISSFLG(KKPER),KKITER,KKSTP,KKPER,NCOL,
     &                            NROW,NLAY,IOUT,X(LCWETD),WETFCT,
     &                            IWETIT,IHDWET,HDRY,NBOTM,X(LCVKCB))
              IF (IUNIT(37).GT.0)
     &            CALL GWF1HUF2FM(GX(LCHCOF),GX(LCRHS),GX(LCHOLD),
     &                            X(LCSC1),GZ(LCHNEW),IG(LCIBOU),
     &                            GX(LCCR),GX(LCCC),GX(LCCV),X(LCHK),
     &                            X(LCVKA),GX(LCBOTM),GX(LCDELR),
     &                            GX(LCDELC),DELT,ITRSS,ISSFLG(KKPER),
     &                            NCOL,NROW,NLAY,IOUT,X(LCWETD),NBOTM,
     &                            NHUF,GX(LCRMLT),IG(LCIZON),NMLTAR,
     &                            NZONAR,X(LCHUFTHK),X(LCHKCC),HDRY,
     &                            KKITER,KSTP,KPER,X(LCHUFTMP),
     &                            IX(LCHGUF),
     &                            IUNIT(47),X(LCVDHD),X(LCVDHT),IWETIT,
     &                            IHDWET,WETFCT,X(LCGS),X(LCA9))
              IF (IUNIT(21).GT.0)
     &            CALL GWF1HFB6FM(GX(LCBOTM),GX(LCCC),GX(LCCR),
     &                            GX(LCDELC),GX(LCDELR),RX(LCHFB),
     &                            GZ(LCHNEW),MXACTFB,NBOTM,NCOL,NHFB,
     &                            NLAY,NROW,LAYHDT)
              IF (IUNIT(2).GT.0)
     &            CALL GWF1WEL6FM(NWELLS,MXWELL,GX(LCRHS),RX(LCWELL),
     &                            IG(LCIBOU),NCOL,NROW,NLAY,NWELVL)
              IF (IUNIT(3).GT.0)
     &            CALL GWF1DRN6FM(NDRAIN,MXDRN,RX(LCDRAI),GZ(LCHNEW),
     &                            GX(LCHCOF),GX(LCRHS),IG(LCIBOU),NCOL,
     &                            NROW,NLAY,NDRNVL)              
              IF (IUNIT(4).GT.0)
     &            CALL GWF1RIV6FM(NRIVER,MXRIVR,RX(LCRIVR),GZ(LCHNEW),
     &                            GX(LCHCOF),GX(LCRHS),IG(LCIBOU),NCOL,
     &                            NROW,NLAY,NRIVVL)
CLAK
              IF (IUNIT(5).GT.0) THEN
                IF(IUNIT(22).GT.0.AND.NEVTOP.EQ.3)
     1                CALL GWF1LAK3ST(0,NCOL,NROW,NLAY,IG(LCIBOU),
     2                            LKNODE,IR(ICLAKE),MXLKND,NLAKES,
     3                            RX(ISTGLD),GX(LCBOTM),NBOTM)
                CALL GWF1EVT6FM(NEVTOP,IR(LCIEVT),RX(LCEVTR),
     &                          RX(LCEXDP),RX(LCSURF),GX(LCRHS),
     &                          GX(LCHCOF),IG(LCIBOU),GZ(LCHNEW),NCOL,
     &                          NROW,NLAY)
                IF(IUNIT(22).GT.0.AND.NEVTOP.EQ.3)
     1                CALL GWF1LAK3ST(1,NCOL,NROW,NLAY,IG(LCIBOU),
     2                            LKNODE,IR(ICLAKE),MXLKND,NLAKES,
     3                            RX(ISTGLD),GX(LCBOTM),NBOTM)
              END IF

              IF (IUNIT(7).GT.0)
     &            CALL GWF1GHB6FM(NBOUND,MXBND,RX(LCBNDS),GX(LCHCOF),
     &                            GX(LCRHS),IG(LCIBOU),NCOL,NROW,NLAY,
     &                            NGHBVL)
CLAK
              IF (IUNIT(8).GT.0) THEN
                IF(IUNIT(22).GT.0.AND.NRCHOP.EQ.3)
     1                CALL GWF1LAK3ST(0,NCOL,NROW,NLAY,IG(LCIBOU),
     2                            LKNODE,IR(ICLAKE),MXLKND,NLAKES,
     3                            RX(ISTGLD),GX(LCBOTM),NBOTM)
                CALL GWF1RCH6FM(NRCHOP,IR(LCIRCH),RX(LCRECH),
     &                            GX(LCRHS),IG(LCIBOU),NCOL,NROW,NLAY)
                IF(IUNIT(22).GT.0.AND.NRCHOP.EQ.3)
     1                CALL GWF1LAK3ST(1,NCOL,NROW,NLAY,IG(LCIBOU),
     2                            LKNODE,IR(ICLAKE),MXLKND,NLAKES,
     3                            RX(ISTGLD),GX(LCBOTM),NBOTM)
              END IF
              IF(IUNIT(16).GT.0)
     &            CALL GWF1FHB1FM(GX(LCRHS),IG(LCIBOU),IR(LCFLLC),
     &                        RX(LCBDFV),NFLW,NCOL,NROW,NLAY,IFHBD4,
     &                        NFLWDIM)
              IF (IUNIT(17).GT.0)
     &            CALL GWF1RES1FM(IR(LCIRES),IR(LCIRSL),RX(LCBRES),
     &                        RX(LCCRES),RX(LCBBRE),RX(LCHRES),
     &                        IG(LCIBOU),GZ(LCHNEW),GX(LCHCOF),
     &                        GX(LCRHS),NRES,NRESOP,NCOL,NROW,NLAY)
              IF(IUNIT(18).GT.0)
     &            CALL GWF1STR6FM(NSTREM,RX(LCSTRM_),IR(ICSTRM_),
     &                        GZ(LCHNEW),GX(LCHCOF),GX(LCRHS),
     &                        IG(LCIBOU),MXSTRM,NCOL,NROW,NLAY,
     &                        NSSSTR6,IR(LCTBAR),NTRIB,RX(LCTRIB),
     &                        IR(LCIVAR_),IR(LCFGAR),ICALC,
     &                        CONSTSTR6)
              IF (IUNIT(19).GT.0)
     1            CALL GWF1IBS6FM(GX(LCRHS),GX(LCHCOF),GZ(LCHNEW),
     2                            GX(LCHOLD),RX(LCHC),RX(LCSCE),
     3                            RX(LCSCV),IG(LCIBOU),NCOL,NROW,NLAY,
     4                            DELT,ISSFLG(KKPER),IBSDIM)
              IF(IUNIT(54).GT.0)
     1            CALL GWF1SUB1FM(GX(LCRHS),GX(LCHCOF),GZ(LCHNEW),
     2                            GX(LCHOLD),IG(LCIBOU),X(LCV),
     3                            GX(LCDELR),GX(LCDELC),NCOL,NROW,
     4                            NODES,DELT,AC1,AC2,HCLOSE,KKITER,
     5                            ITMIN,NN,NND1,ND1,ND2,NDB,NNDB,NPZ,
     6                            ISSFLG(KKPER),IUNIT(9))
CLAK
              IF (IUNIT(22).GT.0)
     *               CALL GWF1LAK3FM(LKNODE,MXLKND,IR(ICLAKE),
     1                      GZ(LCHNEW),GX(LCHCOF),GX(LCRHS),
     2                      IG(LCIBOU),NCOL,NROW,NLAY,NLAKES,
     3                      RX(ISTGLD),RX(LCCNDF),GX(LCBOTM),NBOTM,
     4                      IOUT,DELT,NSS,NTRB,NDV,IR(INTRB),IR(INDV),
     5                      RX(ISTRIN),RX(ISTROT),RX(ISTGNW),
     6                      RX(LCWTDR),RX(LCLKPR),RX(LCLKEV),
     7                      GX(LCDELR),GX(LCDELC),RX(LKACC1),
     8                      RX(LKACC2),RX(LKACC3),RX(LKACC4),
     9                      RX(LKACC5),RX(LKACC6),THETA,RX(LCRNF),
     *                      KKSTP,KKITER,ISSFLG(KKPER),NSSITR,SSCNCR,
     *                      RX(LKSSMN),RX(LKSSMX),RX(IDSTRT),IR(LKNCN),
     *                      RX(LKDSR),RX(LKCNN),RX(LKCHN),RX(IAREN),
     *                      IR(LKLMRR))
              IF (IUNIT(39).GT.0)
     &            CALL GWF1ETS1FM(NETSOP,IR(LCIETS),RX(LCETSR),
     &                            RX(LCETSX),RX(LCETSS),GX(LCRHS),
     &                            GX(LCHCOF),IG(LCIBOU),GZ(LCHNEW),NCOL,
     &                            NROW,NLAY,NETSEG,RX(LCPXDP),
     &                            RX(LCPETM),NSEGAR)
              IF (IUNIT(40).GT.0)
     &            CALL GWF1DRT1FM(NDRTCL,MXDRT,RX(LCDRTF),GZ(LCHNEW),
     &                            GX(LCHCOF),GX(LCRHS),IG(LCIBOU),NCOL,
     &                            NROW,NLAY,NDRTVL,IDRTFL)
              IF(IUNIT(51).GT.0)
     1            CALL GWF1DAF1FM(IERR,ITMUNI,GZ(LCHNEW),GX(LCHOLD),
     2                            IOUT,IG(LCIBOU),GX(LCHCOF),
     3                            GX(LCRHS),NCOL,NROW,NLAY,KITER,
     4                            IDAFBK)
              IF (IUNIT(50).GT.0)
     &            CALL GWF1MNW1FM(NWELL2,MXWEL2,RX(LCWEL2),IG(LCIBOU),
     &                            GX(LCDELR),GX(LCDELC),GX(LCCR),
     &                            GX(LCCC),RX(LCHY),SMALL,HDRY,
     &                            GX(LCHCOF),GX(LCRHS),GZ(LCHNEW),NCOL,
     &                            NROW,NODES,KITER,NOMOITER,LAYHDT,
     &                            GX(LCBOTM),NBOTM,X(LCHK),IUNIT(1),
     &                            IUNIT(23),IUNIT(37),NLAY,PLOSSMNW,
     &                            RX(LCTRPY),X(LCHKCC),X(LCHANI))
            ENDIF
C
C-------IF HNEW=HOLD=0 AND RHS=0, NO NEED TO SOLVE.
              CALL UNOITER(GX(LCRHS),GZ(LCHNEW),NODES,ISA)
              IF (ISA.EQ.0) THEN
                ICNVG = 1
                GOTO 33
              ENDIF
C
C7C2B---MAKE ONE CUT AT AN APPROXIMATE SOLUTION.
C--SEAWAT:IF(IUNIT(57).GT.0) PASS RHOCR, RHOCC, AND RHOCV
             IF(IUNIT(57).GT.0) THEN
              IF (IUNIT(9).GT.0)
     &            CALL SIP5AP(GZ(LCHNEW),IG(LCIBOU),VDF(LRHOCR),
     &                        VDF(LRHOCC),VDF(LRHOCV),GX(LCHCOF),
     &                                          GX(LCRHS),X(LCEL),
     &                        X(LCFL),X(LCGL),X(LCV),X(LCW),X(LCHDCG),
     &                        IX(LCLRCH),NPARM,KKITER,HCLOSE,ACCL,
     &                        ICNVG,KKSTP,KKPER,IPCALC,IPRSIP,MXITER,
     &                        NSTP(KKPER),NCOL,NROW,NLAY,NODES,IOUT,0,
     &                        IERR,IERRU)
              IF (IUNIT(10).GT.0)
     &            CALL DE45AP(GZ(LCHNEW),IG(LCIBOU),X(LCAU),X(LCAL),
     &                        IX(LCIUPP),IX(LCIEQP),X(LCD4B),MXUP,
     &                        MXLOW,MXEQ,MXBW,VDF(LRHOCR),
     &                        VDF(LRHOCC),VDF(LRHOCV),GX(LCHCOF),
     &                        GX(LCRHS),ACCL,
     &                        KKITER,ITMX,MXITER,NITER,HCLOSE,IPRD4,
     &                        ICNVG,NCOL,NROW,NLAY,IOUT,IX(LCLRCH),
     &                        X(LCHDCG),IFREQ,KKSTP,KKPER,DELT,
     &                        NSTP(KKPER),ID4DIR,ID4DIM,MUTD4,IERR,
     &                        IERRU)
              IF (IUNIT(11).GT.0)
     &            CALL SOR5AP(GZ(LCHNEW),IG(LCIBOU),VDF(LRHOCR),
     &                        VDF(LRHOCC),VDF(LRHOCV),GX(LCHCOF),
     &                        GX(LCRHS),X(LCA),
     &                        X(LCRES),IX(LCIEQP),X(LCHDCG),IX(LCLRCH),
     &                        KKITER,HCLOSE,ACCL,ICNVG,KKSTP,KKPER,
     &                        IPRSOR,MXITER,NSTP(KKPER),NCOL,NROW,NLAY,
     &                        NSLICE,MBW,IOUT,0)
              IF (IUNIT(13).GT.0)
     &            CALL PCG2AP(GZ(LCHNEW),IG(LCIBOU),VDF(LRHOCR),
     &                        VDF(LRHOCC),VDF(LRHOCV),GX(LCHCOF),
     &                        GX(LCRHS),Z(LCV),
     &                        Z(LCSS),Z(LCP),X(LCCD),X(LCHCHG),
     &                        IX(LCLHCH),X(LCRCHG),IX(LCLRCH),KKITER,
     &                        NITER,HCLOSE,RCLOSE,ICNVG,KKSTP,KKPER,
     &                        IPRPCG,MXITER,ITER1,NPCOND,NBPOL,
     &                        NSTP(KKPER),NCOL,NROW,NLAY,NODES,RELAX,
     &                        IOUT,MUTPCG,IX(LCIT1),DAMP,GX(LCBUFF),
     &                        X(LCHCSV),IERR,IERRU,Z(LCHPCG))
              IF (IUNIT(14).GT.0)
     &            CALL LMG1AP(GZ(LCHNEW),IG(LCIBOU),VDF(LRHOCR),
     &                        VDF(LRHOCC),VDF(LRHOCV),GX(LCHCOF),
     &                        GX(LCRHS),Z(LCA),
     &                        IX(LCIA),IX(LCJA),Z(LCU1),Z(LCFRHS),
     &                        IX(LCIG),ISIZ1,ISIZ2,ISIZ3,ISIZ4,KKITER,
     &                        BCLOSE,DAMP,ICNVG,KKSTP,KKPER,MXITER,
     &                        MXCYC,NCOL,NROW,NLAY,NODES,HNOFLO,IOUT,
     &                        IOUTAMG,ICG,IADAMP,DUP,DLOW)
C--SEAWAT:CONSTANT DENSITY, PASS CR, CC,AND CV
             ELSE         
              IF (IUNIT(9).GT.0)
     &            CALL SIP5AP(GZ(LCHNEW),IG(LCIBOU),GX(LCCR),GX(LCCC),
     &                        GX(LCCV),GX(LCHCOF),GX(LCRHS),X(LCEL),
     &                        X(LCFL),X(LCGL),X(LCV),X(LCW),X(LCHDCG),
     &                        IX(LCLRCH),NPARM,KKITER,HCLOSE,ACCL,
     &                        ICNVG,KKSTP,KKPER,IPCALC,IPRSIP,MXITER,
     &                        NSTP(KKPER),NCOL,NROW,NLAY,NODES,IOUT,0,
     &                        IERR,IERRU)
              IF (IUNIT(10).GT.0)
     &            CALL DE45AP(GZ(LCHNEW),IG(LCIBOU),X(LCAU),X(LCAL),
     &                        IX(LCIUPP),IX(LCIEQP),X(LCD4B),MXUP,
     &                        MXLOW,MXEQ,MXBW,GX(LCCR),GX(LCCC),
     &                        GX(LCCV),GX(LCHCOF),GX(LCRHS),ACCL,
     &                        KKITER,ITMX,MXITER,NITER,HCLOSE,IPRD4,
     &                        ICNVG,NCOL,NROW,NLAY,IOUT,IX(LCLRCH),
     &                        X(LCHDCG),IFREQ,KKSTP,KKPER,DELT,
     &                        NSTP(KKPER),ID4DIR,ID4DIM,MUTD4,IERR,
     &                        IERRU)
              IF (IUNIT(11).GT.0)
     &            CALL SOR5AP(GZ(LCHNEW),IG(LCIBOU),GX(LCCR),GX(LCCC),
     &                        GX(LCCV),GX(LCHCOF),GX(LCRHS),X(LCA),
     &                        X(LCRES),IX(LCIEQP),X(LCHDCG),IX(LCLRCH),
     &                        KKITER,HCLOSE,ACCL,ICNVG,KKSTP,KKPER,
     &                        IPRSOR,MXITER,NSTP(KKPER),NCOL,NROW,NLAY,
     &                        NSLICE,MBW,IOUT,0)

              IF (IUNIT(13).GT.0) 
     &            CALL PCG2AP(GZ(LCHNEW),IG(LCIBOU),GX(LCCR),GX(LCCC),
     &                        GX(LCCV),GX(LCHCOF),GX(LCRHS),Z(LCV),
     &                        Z(LCSS),Z(LCP),X(LCCD),X(LCHCHG),
     &                        IX(LCLHCH),X(LCRCHG),IX(LCLRCH),KKITER,
     &                        NITER,HCLOSE,RCLOSE,ICNVG,KKSTP,KKPER,
     &                        IPRPCG,MXITER,ITER1,NPCOND,NBPOL,
     &                        NSTP(KKPER),NCOL,NROW,NLAY,NODES,RELAX,
     &                        IOUT,MUTPCG,IX(LCIT1),DAMP,GX(LCBUFF),
     &                        X(LCHCSV),IERR,IERRU,Z(LCHPCG))
              IF (IUNIT(14).GT.0)
     &            CALL LMG1AP(GZ(LCHNEW),IG(LCIBOU),GX(LCCR),GX(LCCC),
     &                        GX(LCCV),GX(LCHCOF),GX(LCRHS),Z(LCA),
     &                        IX(LCIA),IX(LCJA),Z(LCU1),Z(LCFRHS),
     &                        IX(LCIG),ISIZ1,ISIZ2,ISIZ3,ISIZ4,KKITER,
     &                        BCLOSE,DAMP,ICNVG,KKSTP,KKPER,MXITER,
     &                        MXCYC,NCOL,NROW,NLAY,NODES,HNOFLO,IOUT,
     &                        IOUTAMG,ICG,IADAMP,DUP,DLOW)
C--SEAWAT: ENDIF
             ENDIF
C
C--SEAWAT: UPDATE THE SALTHEAD ARRAY WITH THE NEW HEADS
		    IF(IUNIT(57).GT.0)
     +           CALL VDF1HSALT(NCOL,NROW,NLAY,GZ(LCHNEW),VDF(LHSALT),
     +                      VDF(LPS),VDF(LELEV),IG(LCIBOU))
C
              IF (IERR.GT.0) THEN
C               WRITE MESSAGE RELATED TO BEALE'S MEASURE, IF
C               APPROPRIATE, THEN STOP EXECUTION.
                IF (IBEFLG.EQ.2) CALL PES1BAS6ER(IOUT,ITERPK,NPLIST)
                CALL PLL1SD(IERR,IERRU,IOUT,IOUTG)
              ENDIF
C
C7C2C---IF CONVERGENCE CRITERION HAS BEEN MET STOP ITERATING.
            IF (ICNVG.EQ.1) GOTO 33
  30         CONTINUE
             KITER = MXITER
C
C7C2C-----IF CONVERGENCE CRITERION HAS NOT BEEN MET . . .
C---------IF ESTIMATING PARAMETERS OR CALCULATING BEALE'S MEASURE, USE
C         THE AVAILABLE VALUES AND KEEP GOING
            IF (IPES.GT.0 .OR. IBEFLG.EQ.2) THEN
              ICNVG = 1
              ICNVGP = 0
            ENDIF
C---------PRINT THE DATA TABLE AND WARNING MESSAGES AND STOP EXCEPT
C         AS NOTED ABOVE
            IF (IPAR.GT.-3) THEN
              CALL UNOCONV(X(LCBUF1+IPRAR),OBSNAM,X(LCH),
     &                     X(LCHOBS),IOUT,0,IPAR,IPR,KKPER,KKSTP,
     &                     IX(LCLN),MPR,ND,NDMH,NH,IX(LCNIPR),X(LCPRM),
     &                     X(LCBUF1+IPRAR+ND+MPR+IPR),RSQ,
     &                     RSQP,X(LCWP),X(LCWTPS),X(LCWT),X(LCWTQ),
     &                     X(LCWTQS),NPLIST,MPRAR,IPRAR,OUTNAM,
     &                     IX(LCIPLO),EQNAM,NAMES,IX(LCIPLP),NDMHAR,
     &                     NQTDR,NQTRV,NQTGB,NQTST,NQTCH,IOWTQCH,
     &                     IOWTQDR,IOWTQRV,IOWTQGB,IOWTQST,LCOBBAS,
     &                     LCOBDRN,LCOBRIV,LCOBGHB,LCOBSTR,LCOBCHD,
     &                     LCOBADV,X(LCSSGF),X(LCSSDR),X(LCSSRV),
     &                     X(LCSSGB),X(LCSSST),X(LCSSAD),X(LCSSCH),
     &                     X(LCSSPI),X(LCSSTO),ITMXP,IPES,X(LCBPRI),
     &                     ITERP,IERR,IERRU,NTT2,LCOBDRT,X(LCSSDT),
     &                     NQTDT,IOWTQDT,NRSO,NPOST,NNEGT,NRUNS,NQTSF,
     &                     IOWTQSF,LCOBSFR,X(LCSSSF),KTDIM,NHT,
     &                     X(LCOTIM))
              IF (IPAR.EQ.1 .OR. IBEFLG.EQ.2) THEN
C               CONTINUE EXECUTION, BUT WRITE MESSAGE(S) REGARDING
C               NONCONVERGENCE
                CALL PES1BAS6NC(GX(LCCC),GX(LCCR),GX(LCCV),GX(LCHCOF),
     &                          GZ(LCHNEW),IBEFLG,IG(LCIBOU),IOUTG,
     &                          ITERPK,KKPER,KKSTP,NCOL,NLAY,NROW,
     &                          GX(LCRHS))
              ELSE
C               STOP EXECUTION
                CALL PLL1SD(IERR,IERRU,IOUT,IOUTG)
              ENDIF
            ENDIF
C
   33       CONTINUE
C
C--SEAWAT: SKIP OVER MT3D SOLUTION IF NOT NEEDED
            IF (IUNIT(58).EQ.0) GOTO 39
C--SEAWAT: STORE VALUE OF ILMTFMT IN IILMTFMT
	IILMTFMT=ILMTFMT
C--SEAWAT: RESET ILMTFMT TO 0 SO OUTPUT TO $FILE.UMT IS BINARY
	ILMTFMT=0	
C--SEAWAT: WRITE FLOW INFORMATION TO TEMPORARY FILE (INUHF)
C--SEAWAT: PASS IN ILMTHEAD2 FOR ILMTHEAD
      CALL LMT6BAS6VD(INUNIT,IOUT,NCOL,NROW,NLAY,NPER,ISS,NODES,
     &     IUNIT,CUNIT,NIUNIT,IG(LCIBOU),INUHF,ILMTHEAD2)
      IF(IUNIT(57).GT.0) THEN
C--SEAWAT: WRITE FLOW INFORMATION IF VDF ACTIVE
C--SEAWAT: IN THIS SECTION ALL REFS TO IMT3D HAVE BEEN CHANGED TO INUHF
            IF(IUNIT(1).GT.0) CALL LMT6BCF6VD(GZ(LCHNEW),IG(LCIBOU),
     &        GX(LCCR),GX(LCCC),GX(LCCV),ISS,ISSFLG(KKPER),DELT,
     &        RX(LCSC1),RX(LCSC2),GX(LCHOLD),GX(LCBOTM),NBOTM,NCOL,NROW,
     &        NLAY,KKSTP,KKPER,GX(LCBUFF),INUHF,VDF(LPS),VDF(LELEV),
     &        Y(LTDELR),Y(LTDELC),VDF(LHSALT),IWTABLE,INUHF)
            IF(IUNIT(23).GT.0) CALL LMT6LPF1VD(GZ(LCHNEW),IG(LCIBOU),
     &        GX(LCCR),GX(LCCC),GX(LCCV),ISS,ISSFLG(KKPER),DELT,
     &        X(LCSC1),X(LCSC2),GX(LCHOLD),GX(LCBOTM),NBOTM,NCOL,NROW,
     &        NLAY,KKSTP,KKPER,GX(LCBUFF),INUHF,VDF(LPS),VDF(LELEV),
     &        Y(LTDELR),Y(LTDELC),VDF(LHSALT),IWTABLE,INUHF)
            IF(IUNIT(37).GT.0) CALL LMT6HUF2VD(GZ(LCHNEW),IG(LCIBOU),
     &        GX(LCCR),GX(LCCC),GX(LCCV),ISS,ISSFLG(KKPER),
     &        DELT,GX(LCHOLD),X(LCSC1),GX(LCBOTM),NBOTM,
     &        NCOL,NROW,NLAY,KKSTP,KKPER,
     &        X(LCHUFTHK),NHUF,IG(LCIZON),NZONAR,GX(LCRMLT),NMLTAR,
     &        GX(LCDELR),GX(LCDELC),GX(LCBUFF),INUHF,VDF(LPS),
     &        VDF(LELEV),VDF(LHSALT),IWTABLE,INUHF)
            IF(IUNIT(2).GT.0) CALL LMT6WEL6(NWELLS,MXWELL,NWELVL,
     &        RX(LCWELL),IG(LCIBOU),NCOL,NROW,NLAY,KKSTP,KKPER,INUHF,
     &        INUHF)
            IF(IUNIT(3).GT.0) CALL LMT6DRN6VD(NDRAIN,MXDRN,NDRNVL,
     &        RX(LCDRAI),GZ(LCHNEW),NCOL,NROW,NLAY,IG(LCIBOU),KKSTP,
     &        KKPER,INUHF,VDF(LPS),VDF(LELEV),INUHF)
C--SEAWAT: COMPUTING IRCH ARRAY
            IF(IUNIT(8).GT.0) THEN
              CALL VDFCALCIRCH(NROW,NCOL,NLAY,IG(LCIBOU),IR(LCIRCH)
     &                      ,NRCHOP)
              CALL LMT6RCH6VD(NRCHOP,IR(LCIRCH),RX(LCRECH),IG(LCIBOU),
     &             NROW,NCOL,NLAY,KKSTP,KKPER,GX(LCBUFF),INUHF,INUHF)
            ENDIF
C--SEAWAT: COMPUTING IEVT ARRAY
            IF(IUNIT(5).GT.0) THEN
              CALL VDFCALCIEVT(NROW,NCOL,NLAY,IG(LCIBOU),IR(LCIEVT),
     &                        NEVTOP)
              CALL LMT6EVT6VD(NEVTOP,IR(LCIEVT),RX(LCEVTR),
     &             RX(LCEXDP),RX(LCSURF),IG(LCIBOU),GZ(LCHNEW),NCOL,
     &             NROW,NLAY,KKSTP,KKPER,GX(LCBUFF),INUHF,VDF(LPS),
     &             VDF(LELEV),INUHF)
            ENDIF
            IF(IUNIT(4).GT.0) CALL LMT6RIV6VD(NRIVER,MXRIVR,NRIVVL,
     &       RX(LCRIVR),IG(LCIBOU),GZ(LCHNEW),NCOL,NROW,NLAY,KKSTP,KKPER
     &       ,INUHF,VDF(LELEV),VDF(LPS),MTDNCONC,MXSS,NSS,Y(LCSSM),NCOMP
     &       ,Y(LCSSMC),INUHF)
            IF(IUNIT(7).GT.0) CALL LMT6GHB6VD(NBOUND,MXBND,NGHBVL,
     &       RX(LCBNDS),GZ(LCHNEW),NCOL,NROW,NLAY,IG(LCIBOU),KKSTP,
     &       KKPER,INUHF,VDF(LELEV),VDF(LPS),MTDNCONC,MXSS,NSS,
     &       Y(LCSSM),NCOMP,Y(LCSSMC),INUHF)
            IF(IUNIT(16).GT.0) CALL LMT6FHB1(IR(LCFLLC),RX(LCBDFV),NFLW,
     &       IG(LCIBOU),NCOL,NROW,NLAY,KKSTP,KKPER,IFHBD4,NFLWDIM,INUHF,
     &       INUHF)
      ELSE
C--SEAWAT: WRITE FOR VDF NOT ACTIVE BUT IMT ACTIVE
             IF(IUNIT(1).GT.0) CALL LMT6BCF6(GZ(LCHNEW),IG(LCIBOU),
     &          GX(LCCR),GX(LCCC),GX(LCCV),ISS,ISSFLG(KKPER),DELT,
     &          RX(LCSC1),RX(LCSC2),GX(LCHOLD),GX(LCBOTM),NBOTM,NCOL,
     &          NROW,NLAY,KKSTP,KKPER,GX(LCBUFF),INUHF,INUHF)
             IF(IUNIT(23).GT.0) CALL LMT6LPF1(GZ(LCHNEW),IG(LCIBOU),
     &          GX(LCCR),GX(LCCC),GX(LCCV),ISS,ISSFLG(KKPER),DELT,
     &          X(LCSC1),
     &          X(LCSC2),GX(LCHOLD),GX(LCBOTM),NBOTM,NCOL,NROW,NLAY,
     &          KKSTP,KKPER,GX(LCBUFF),INUHF,INUHF)
             IF(IUNIT(37).GT.0) CALL LMT6HUF2(GZ(LCHNEW),IG(LCIBOU),
     &          GX(LCCR),GX(LCCC),GX(LCCV),ISS,ISSFLG(KKPER),
     &          DELT,GX(LCHOLD),X(LCSC1),GX(LCBOTM),NBOTM,
     &          NCOL,NROW,NLAY,KKSTP,KKPER,
     &          X(LCHUFTHK),NHUF,IG(LCIZON),NZONAR,GX(LCRMLT),NMLTAR,
     &          GX(LCDELR),GX(LCDELC),GX(LCBUFF),INUHF,INUHF)
             IF(IUNIT(2).GT.0) CALL LMT6WEL6(NWELLS,MXWELL,NWELVL,
     &          RX(LCWELL),IG(LCIBOU),NCOL,NROW,NLAY,KKSTP,KKPER,INUHF,
     &          INUHF)
             IF(IUNIT(3).GT.0) CALL LMT6DRN6(NDRAIN,MXDRN,NDRNVL,
     &          RX(LCDRAI),GZ(LCHNEW),NCOL,NROW,NLAY,IG(LCIBOU),KKSTP,
     &          KKPER,INUHF,INUHF)
C--SEAWAT: COMPUTE IRCH ARRAY
             IF(IUNIT(8).GT.0) THEN
              CALL VDFCALCIRCH(NROW,NCOL,NLAY,IG(LCIBOU),IR(LCIRCH),
     &                         NRCHOP)          
              CALL LMT6RCH6(NRCHOP,IR(LCIRCH),RX(LCRECH),
     &          IG(LCIBOU),NROW,NCOL,NLAY,KKSTP,KKPER,GX(LCBUFF),INUHF,
     &          INUHF)
             ENDIF
C--SEAWAT: COMPUTE IEVT ARRAY
            IF(IUNIT(5).GT.0) THEN 
              CALL VDFCALCIEVT(NROW,NCOL,NLAY,IG(LCIBOU),IR(LCIEVT),
     &                         NEVTOP)          
              CALL LMT6EVT6(NEVTOP,IR(LCIEVT),RX(LCEVTR),
     &          RX(LCEXDP),RX(LCSURF),IG(LCIBOU),GZ(LCHNEW),NCOL,NROW,
     &          NLAY,KKSTP,KKPER,GX(LCBUFF),INUHF,INUHF)
            ENDIF
            IF(IUNIT(4).GT.0) CALL LMT6RIV6(NRIVER,MXRIVR,NRIVVL,
     &          RX(LCRIVR),IG(LCIBOU),
     &          GZ(LCHNEW),NCOL,NROW,NLAY,KKSTP,KKPER,INUHF,INUHF)
C--SEAWAT: PASS IN A 1 FOR ILMTHEAD
            IF(IUNIT(18).GT.0) CALL LMT6STR6(NSTREM,RX(LCSTRM_),
     &          IR(ICSTRM_),IG(LCIBOU),MXSTRM,NCOL,NROW,NLAY,KKSTP,KKPER
     &          ,INUHF,1,INUHF)
            IF(IUNIT(7).GT.0) CALL LMT6GHB6(NBOUND,MXBND,NGHBVL,
     &          RX(LCBNDS),GZ(LCHNEW),NCOL,NROW,NLAY,IG(LCIBOU),KKSTP,
     &          KKPER,INUHF,INUHF)
            IF(IUNIT(17).GT.0) CALL LMT6RES1(IR(LCIRES),IR(LCIRSL),
     &          RX(LCBRES),RX(LCCRES),RX(LCBBRE),RX(LCHRES),IG(LCIBOU),
     &          GZ(LCHNEW),GX(LCBUFF),KKSTP,KKPER,NRES,NRESOP,
     &          NCOL,NROW,NLAY,INUHF,INUHF)
            IF(IUNIT(16).GT.0) CALL LMT6FHB1(IR(LCFLLC),RX(LCBDFV),NFLW,
     &          IG(LCIBOU),NCOL,NROW,NLAY,KKSTP,KKPER,IFHBD4,NFLWDIM,
     &          INUHF,INUHF)
      ENDIF
      REWIND(INUHF)

C--SEAWAT: READ FLOW INFORMATION FROM TEMPORARY FILE
            CALL IMT1FMI4AL(INUHF,IOUT,TRNOP,NPERFL,ISS,IVER)
            CALL IMT1FMI4RP1(INUHF,IOUT,KPER,KSTP,NCOL,NROW,NLAY,NCOMP,
     &                 FPRT,IY(LCLAYC),IY(LCIB),HORIGN,Y(LCDH),Y(LCPR),
     &                 Y(LTDELR),Y(LTDELC),Y(LCDZ),Y(LCXBC),Y(LCYBC),
     &                 Y(LCZBC),Y(LCQSTO),Y(LCCOLD),Y(LCCNEW),Y(LCRETA),
     &                 Y(LCQX),Y(LCQY),Y(LCQZ),DTRACK,DTRACK2,THKMIN,
     &                 ISS,IVER)
            IF(IUNIT(61).GT.0)
     &      CALL IMT1FMI4RP2(INUHF,IOUT,KPER,KSTP,NCOL,NROW,NLAY,NCOMP,
     &                 FPRT,IY(LCLAYC),IY(LCIB),Y(LCDH),Y(LCPR),
     &                 Y(LTDELR),Y(LTDELC),IY(LTIRCH),Y(LTRECH),
     &                 IY(LTIEVT),Y(LTEVTR),MXSS,NSS,NTSS,Y(LCSSM),
     &                 Y(LTBUFF),DTSSM)
C--SEAWAT: RESET ILMTFMT BACK TO ORIGINAL VALUE
	ILMTFMT=IILMTFMT
C--SEAWAT: REWIND TEMPORARY FILE
           REWIND(INUHF)
C--SEAWAT: CALCULATE COEFFICIENTS THAT ARE CONSTANT WITHIN EACH TRANSPORT STEP
           IF(IUNIT(60).GT.0)
     &      CALL IMT1DSP4CF(IOUT,KSTP,KPER,NCOL,NROW,NLAY,
     &                IY(LCIB),Y(LCPR),Y(LTDELR),Y(LTDELC),Y(LCDH),
     &                Y(LCQX),Y(LCQY),Y(LCQZ),Y(LCAL),Y(LCTRPT),
     &                Y(LCTRPV),Y(LCDM),DTDISP,Y(LCDXX),Y(LCDXY),
     &                Y(LCDXZ),Y(LCDYX),Y(LCDYY),Y(LCDYZ),Y(LCDZX),
     &                Y(LCDZY),Y(LCDZZ),IFMTDP)
  209      CONTINUE

C--SEAWAT: FOR EACH COMPONENT......
           DO ICOMP=1,NCOMP
C
C--SEAWAT: SOLVE TRANSPORT TERMS WITH EXPLICIT SCHEMES
            IF(IMPSOL.EQ.1 .AND. MIXELM.EQ.0) GOTO 1500
C
C--SEAWAT: FORMULATE AND SOLVE TRANSPORT EQUATION
            CALL IMT1BTN4SV(NCOL,NROW,NLAY,NCOMP,ICOMP,IY(LCIB),
     &            Y(LCCNEW),Y(LCCWGT),CINACT,RMASIO)

CVDF***USE IUNIT CONDITIONALS..STARTED HERE 
            IF(IUNIT(59).GT.0.AND. ICOMP.LE.MCOMP)
     &       CALL IMT1ADV4SV(IOUT,NCOL,NROW,NLAY,MCOMP,ICOMP,MIXELM,
     &                    MXPART,
     &                    NCOUNT,NPINS,NRC,IY(LCCHEK),Y(LCXP),Y(LCYP),
     &                    Y(LCZP),IY(LCINDX),IY(LCINDY),IY(LCINDZ),
     &                    Y(LCCNPT),IY(LCIB),Y(LTDELR),Y(LTDELC),
     &                    Y(LCDZ),Y(LCXBC),Y(LCYBC),Y(LCZBC),Y(LCDH),
     &                    Y(LCPR),Y(LCQX),Y(LCQY),Y(LCQZ),Y(LCRETA),
     &                    Y(LCCOLD),Y(LCCWGT),Y(LCCNEW),Y(LCCADV),
     &                    Y(LTBUFF),DTRANS,IMPSOL,NADVFD,RMASIO)

C
            IF(IMPSOL.EQ.1) GOTO 1500
C
            IF(IUNIT(60).GT.0.AND. ICOMP.LE.MCOMP)
     &       CALL IMT1DSP4SV(NCOL,NROW,NLAY,MCOMP,ICOMP,IY(LCIB),
     &                    Y(LTDELR),
     &                    Y(LTDELC),Y(LCDH),Y(LCRETA),Y(LCPR),Y(LCDXX),
     &                    Y(LCDXY),Y(LCDXZ),Y(LCDYX),Y(LCDYY),Y(LCDYZ),
     &                    Y(LCDZX),Y(LCDZY),Y(LCDZZ),Y(LCCNEW),
     &                    Y(LCCWGT),Y(LTBUFF),DTRANS,RMASIO)

C
            IF(IUNIT(61).GT.0.AND. ICOMP.LE.MCOMP)
     &       CALL IMT1SSM4SV(NCOL,NROW,NLAY,NCOMP,ICOMP,IY(LCIB),Y(LCPR)
     &                    ,Y(LTDELR),Y(LTDELC),Y(LCDH),Y(LCRETA),
     &                    IY(LTIRCH),Y(LTRECH),Y(LTCRCH),IY(LTIEVT),
     &                    Y(LTEVTR),Y(LTCEVT),MXSS,NTSS,NSS,Y(LCSSM),
     &                    Y(LCSSMC),Y(LCQSTO),Y(LCCNEW),Y(LCCWGT),
     &                    DTRANS,MIXELM,ISS,RMASIO)

C
            IF(IUNIT(62).GT.0) 
     &        CALL IMT1RCT4SV(NCOL,NROW,NLAY,NCOMP,ICOMP,IY(LCIB),
     &                Y(LCPR),
     &                Y(LTDELR),Y(LTDELC),Y(LCDH),Y(LCRETA),RFMIN,
     &                DTRANS,ISOTHM,IREACT,Y(LCRHOB),Y(LCSP1),Y(LCSP2),
     &                Y(LCSR),Y(LCRC1),Y(LCRC2),Y(LCPRSITY2),Y(LCRETA2),
     &                Y(LCFRAC),Y(LCCNEW),Y(LCCWGT),RMASIO)
C--SEAWAT
            GOTO 220
 1500       CONTINUE
C
C--SEAWAT: SOLVE TRANSPORT TERMS WITH IMPLICIT SCHEMES
            IF(DTRANS.EQ.0) THEN
              ICNVGMT=1
              GOTO 220
            ENDIF
C
C--SEAWAT: ALWAYS UPDATE MATRIX
            UPDLHS=.TRUE.
C
C--SEAWAT: FOR EACH OUTER ITERATION...
            DO ITO=1,MXITERGC
C
C--SEAWAT: UPDATE COEFFICIENTS THAT VARY WITH ITERATIONS
             IF(IUNIT(62).GT.0.AND.ISOTHM.GT.1)
     &        CALL IMT1RCT4CF(NCOL,NROW,NLAY,NCOMP,ICOMP,IY(LCIB),
     &          Y(LCPR),Y(LCCNEW),Y(LCRETA),RFMIN,Y(LCRHOB),Y(LCSP1),
     &          Y(LCSP2),Y(LCRC1),Y(LCRC2),Y(LCPRSITY2),Y(LCRETA2),
     &          Y(LCFRAC),Y(LCSR),ISOTHM,IREACT,DTRANS)
C
C--SEAWAT: FORMULATE MATRIX COEFFICIENTS
             CALL IMT1BTN4FM(NCOL,NROW,NLAY,NCOMP,ICOMP,IY(LCIB),
     &         Y(LCCADV),Y(LCCOLD),Y(LCRETA),Y(LCPR),Y(LTDELR),
     &         Y(LTDELC),Y(LCDH),DTRANS,
     &         Y(LCAGC),Y(LCRHSGC),NODES,UPDLHS,NCRS,MIXELM)

             IF(IUNIT(59).GT.0.AND.MIXELM.EQ.0 .AND. ICOMP.LE.MCOMP)
     &        CALL IMT1ADV4FM(NCOL,NROW,NLAY,MCOMP,ICOMP,IY(LCIB),
     &         Y(LTDELR),
     &         Y(LTDELC),Y(LCDH),Y(LCQX),Y(LCQY),Y(LCQZ),NADVFD,NODES,
     &         Y(LCAGC),UPDLHS)
C
             IF(IUNIT(60).GT.0.AND. ICOMP.LE.MCOMP)
     &        CALL IMT1DSP4FM(NCOL,NROW,NLAY,MCOMP,ICOMP,IY(LCIB),
     &         Y(LTDELR),Y(LTDELC),Y(LCDH),Y(LCDXX),Y(LCDXY),Y(LCDXZ),
     &         Y(LCDYX),
     &         Y(LCDYY),Y(LCDYZ),Y(LCDZX),Y(LCDZY),Y(LCDZZ),
     &         Y(LCAGC),NODES,UPDLHS,Y(LCCNEW),Y(LCRHSGC),NCRS)
C
             IF(IUNIT(61).GT.0.AND.ICOMP.LE.MCOMP)
     &        CALL IMT1SSM4FM(NCOL,NROW,NLAY,NCOMP,ICOMP,IY(LCIB),
     &         Y(LTDELR),Y(LTDELC),Y(LCDH),IY(LTIRCH),Y(LTRECH),
     &         Y(LTCRCH),
     &         IY(LTIEVT),Y(LTEVTR),Y(LTCEVT),MXSS,NTSS,Y(LCSSM),
     &         Y(LCSSMC),
     &         Y(LCQSTO),Y(LCCNEW),ISS,Y(LCAGC),Y(LCRHSGC),NODES,
     &         UPDLHS,MIXELM)
C
             IF(IUNIT(62).GT.0) 
     &        CALL IMT1RCT4FM(NCOL,NROW,NLAY,NCOMP,ICOMP,
     &         IY(LCIB),Y(LCPR),Y(LTDELR),Y(LTDELC),Y(LCDH),ISOTHM,
     &         IREACT,
     &         Y(LCRHOB),Y(LCSP1),Y(LCSP2),Y(LCSR),Y(LCRC1),Y(LCRC2),
     &         Y(LCPRSITY2),Y(LCRETA2),Y(LCFRAC),Y(LCAGC),Y(LCRHSGC),
     &         NODES,UPDLHS,DTRANS)
C
            CALL IMT1GCG4AP(IOUT,MXITERGC,ITER1GC,ITO,ITP,ISOLVE,ACCLGC,
     &       CCLOSE,ICNVGMT,Y(LCCNCG),IY(LCLRCHGC),NCOL,NROW,NLAY,
     &       NODES,N,
     &       KSTP,KPER,TIME2,HT2,UPDLHS,IPRGCG,IY(LCIB+(ICOMP-1)*NODES),
     &       CINACT,Y(LCAGC),Y(LCCNEW+(ICOMP-1)*NODES),Y(LCRHSGC),Y(LCQ)
     &       ,Y(LCWK),NCRS,ISPD)
C
C--SEAWAT: IF CONVERGED, GO TO NEXT OUTER ITERATION
            IF(ICNVGMT.EQ.1) GOTO 220
C
C--SEAWAT: END OF OUTER ITERATION LOOP
          ENDDO
C
  220     CONTINUE
C
C--SEAWAT: END OF COMPONENT LOOP
         ENDDO

C--SEAWAT:*******************************************************
C******************FOR IMPLICIT COUPLING*****************************

         DONE=.TRUE.
         IF (NSWTCPL.GT.1) 
     &       CALL SWTCOUPL(NCOL,NROW,NLAY,IY(LCIB),M,VDF(LPSOLD),
     &                 Y(LCCNEW),DONE,DNSCRIT,IOUT)
	   IF (NSWTCPL.GT.1.AND..NOT.DONE) THEN
			IF(IUNIT(57).GT.0) CALL VDF1PS(NCOL,NROW,NLAY,NCOMP,
     &			Y(LCCNEW),CINACT,VDF(LPS),IOUT,MTDNCONC)
      		CALL VDF1DC(NCOL,NROW,NLAY,NCOMP,Y(LCCOLD),
     &                 Y(LCCNEW),Y(LCPR),GX(LCDELR),GX(LCDELC),
     &                 Y(LCDH),VDF(LDCDT),DELT,MTDNCONC,IG(LCIBOU),
     &                 VDF(LPS),CINACT)
         ENDIF
         IF (DONE) GOTO 1900
C--SWT2K:END OF IMPLICIT COUPLING LOOP
         ENDDO SWT2KICPLOOP
C
         PRINT *,'MAX COUPLING ITERATIONS EXCEEDED: STOPPING '
         STOP
 1900    CONTINUE
C******************FOR IMPLICIT COUPLING*****************************
C********************************************************************

C--SEAWAT: IF MT3DMS NOT NEEDED, CODE JUMPS TO THIS CONTINUE 
   39    CONTINUE

C
C7C3----DETERMINE WHICH OUTPUT IS NEEDED.
C--SEAWAT: ONLY DO IF N.EQ.1
         IF(N.EQ.1)
     *      CALL GWF1BAS6OC(NSTP(KKPER),KKSTP,ICNVG,IR(LCIOFL),NLAY,
     1                      IBUDFL,ICBCFL,IHDDFL,IUNIT(12),IOUT,KKPER,
     2                      IPEROC,ITSOC,IBDOPT,IXSEC,IFREFM,RESETDD,
     3                      RESETDDNEXT)
C
C--SEAWAT: THESE LINES SHUT OFF MODFLOW OUTPUT IF MT3DMS IS ACTIVE AND 
C--SEAWAT: IT IS NOT THE END OF THE FLOW TIMESTEP.  OUTPUT IS TURNED
C--SEAWAT: BACK ON WHEN FLOW TIMESTEP IS COMPLETED.
         IF (IUNIT(58).GT.0) THEN
           IF (N.EQ.1) THEN
             IICBCFL=ICBCFL
             IIHDDFL=IHDDFL
             IIBUDFL=IBUDFL
             ICBCFL=0
             IHDDFL=0
             IBUDFL=0
           ENDIF
           IF(TIME2.GE.HT2) THEN
             ICBCFL=IICBCFL
             IHDDFL=IIHDDFL
             IBUDFL=IIBUDFL
           ENDIF
         ENDIF
C7C4----CALCULATE BUDGET TERMS. SAVE CELL-BY-CELL FLOW TERMS.
         MSUM = 1
C7C4A---THE ORIGINAL BCF BUDGET MODULE HAS BEEN REPLACED BY THREE
C7C4A---SUBMODULES: SGWF1BCF6S, SGWF1BCF6F, AND SGWF1BCF6B .
C--SEAWAT: BCF6 BUDGET
         IF(IUNIT(1).GT.0) THEN
C--SEAWAT: SVDF1BCF6S AND SGWF1BCF6S              
          IF(IUNIT(57).GT.0) THEN
                CALL SVDF1BCF6S(VBNM,VBVL,MSUM,GZ(LCHNEW),IG(LCIBOU),
     &                        GX(LCHOLD),RX(LCSC1),GX(LCBOTM),NBOTM,
     &                        RX(LCSC2),DELT,ISSFLG(KKPER),NCOL,NROW,
     &                        NLAY,KKSTP,KKPER,IBCFCB,ICBCFL,GX(LCBUFF),
     &                        IOUT,PERTIM,TOTIM,VDF(LPS),VDF(LELEV),
     &                        VDF(LHSALT))
          ELSE
                CALL SGWF1BCF6S(VBNM,VBVL,MSUM,GZ(LCHNEW),IG(LCIBOU),
     &                        GX(LCHOLD),RX(LCSC1),GX(LCBOTM),NBOTM,
     &                        RX(LCSC2),DELT,ISSFLG(KKPER),NCOL,NROW,
     &                        NLAY,KKSTP,KKPER,IBCFCB,ICBCFL,GX(LCBUFF),
     &                        IOUT,PERTIM,TOTIM)                        
          ENDIF
C--SEAWAT: SVDF1BCF6F AND SGWF1BCF6F                        
          IF(IUNIT(57).GT.0) THEN                 
                CALL SVDF1BCF6F(VBNM,VBVL,MSUM,GZ(LCHNEW),IG(LCIBOU),
     &                        GX(LCCR),GX(LCCC),GX(LCCV),DELT,NCOL,NROW,
     &                        NLAY,KKSTP,KKPER,IBCFCB,GX(LCBUFF),IOUT,
     &                        ICBCFL,PERTIM,TOTIM,GX(LCBOTM),NBOTM,
     &                        ICHFLG,VDF(LPS),VDF(LELEV),MFNADVFD,
     &                        GX(LCDELR),GX(LCDELC),IWTABLE,VDF(LHSALT))
          ELSE
                CALL SGWF1BCF6F(VBNM,VBVL,MSUM,GZ(LCHNEW),IG(LCIBOU),
     &                        GX(LCCR),GX(LCCC),GX(LCCV),DELT,NCOL,NROW,
     &                        NLAY,KKSTP,KKPER,IBCFCB,GX(LCBUFF),IOUT,
     &                        ICBCFL,PERTIM,TOTIM,GX(LCBOTM),NBOTM,
     &                        ICHFLG)
          ENDIF
          IBDRET=0
          IC1=1
          IC2=NCOL
          IR1=1
          IR2=NROW
          IL1=1
          IL2=NLAY
          DO 37 IDIR = 1, 3
C--SEAWAT: SVDF1BCF6B AND SGWF1BCF6B
           IF(IUNIT(57).GT.0) THEN
                CALL SVDF1BCF6B(GZ(LCHNEW),IG(LCIBOU),GX(LCCR),GX(LCCC),
     &                          GX(LCCV),NCOL,NROW,NLAY,KKSTP,KKPER,
     &                          IBCFCB,GX(LCBUFF),IOUT,ICBCFL,DELT,
     &                          PERTIM,TOTIM,IDIR,IBDRET,ICHFLG,IC1,IC2,
     &                          IR1,IR2,IL1,IL2,GX(LCBOTM),NBOTM,
     &                          VDF(LELEV),VDF(LPS),GX(LCDELR),
     &                          GX(LCDELC),IWTABLE,VDF(LHSALT))
           ELSE
                CALL SGWF1BCF6B(GZ(LCHNEW),IG(LCIBOU),GX(LCCR),GX(LCCC),
     &                          GX(LCCV),NCOL,NROW,NLAY,KKSTP,KKPER,
     &                          IBCFCB,GX(LCBUFF),IOUT,ICBCFL,DELT,
     &                          PERTIM,TOTIM,IDIR,IBDRET,ICHFLG,IC1,IC2,
     &                          IR1,IR2,IL1,IL2,GX(LCBOTM),NBOTM)
           ENDIF
   37      CONTINUE
           ENDIF
C--SEAWAT: LPF BUDGET
C--SEAWAT: SVDF1LPF1S AND SGWF1LPF1S SUBROUTINES
           IF(IUNIT(23).GT.0) THEN
            IF(IUNIT(57).GT.0) THEN
              CALL SVDF1LPF1S(VBNM,VBVL,MSUM,GZ(LCHNEW),IG(LCIBOU),
     &                        GX(LCHOLD),X(LCSC1),GX(LCBOTM),X(LCSC2),
     &                        DELT,ISSFLG(KKPER),NCOL,NROW,NLAY,KKSTP,
     &                        KKPER,ILPFCB,ICBCFL,GX(LCBUFF),IOUT,
     &                        PERTIM,TOTIM,NBOTM,VDF(LPS),VDF(LELEV),
     &                        VDF(LHSALT))
            ELSE
              CALL SGWF1LPF1S(VBNM,VBVL,MSUM,GZ(LCHNEW),IG(LCIBOU),
     &                        GX(LCHOLD),X(LCSC1),GX(LCBOTM),X(LCSC2),
     &                        DELT,ISSFLG(KKPER),NCOL,NROW,NLAY,KKSTP,
     &                        KKPER,ILPFCB,ICBCFL,GX(LCBUFF),IOUT,
     &                        PERTIM,TOTIM,NBOTM)
            ENDIF
C--SEAWAT: SVDF1LPF1F AND SGWF1LPF1F SUBROUTINES
            IF(IUNIT(57).GT.0) THEN
              CALL SVDF1LPF1F(VBNM,VBVL,MSUM,GZ(LCHNEW),IG(LCIBOU),
     &                        GX(LCCR),GX(LCCC),GX(LCCV),GX(LCBOTM),
     &                        DELT,NCOL,NROW,NLAY,KKSTP,KKPER,ILPFCB,
     &                        GX(LCBUFF),IOUT,ICBCFL,PERTIM,TOTIM,
     &                        NBOTM,ICHFLG,VDF(LPS),VDF(LELEV),MFNADVFD,
     &                        GX(LCDELR),GX(LCDELC),IWTABLE,VDF(LHSALT))

            ELSE
              CALL SGWF1LPF1F(VBNM,VBVL,MSUM,GZ(LCHNEW),IG(LCIBOU),
     &                        GX(LCCR),GX(LCCC),GX(LCCV),GX(LCBOTM),
     &                        DELT,NCOL,NROW,NLAY,KKSTP,KKPER,ILPFCB,
     &                        GX(LCBUFF),IOUT,ICBCFL,PERTIM,TOTIM,
     &                        NBOTM,ICHFLG)
            ENDIF
            IBDRET=0
            IC1=1
            IC2=NCOL
            IR1=1
            IR2=NROW
            IL1=1
            IL2=NLAY
            DO 157 IDIR=1,3
C--SEAWAT: SVDF1LPF1B AND SGWF1LPF1B SUBROUTINES
              IF(IUNIT(57).GT.0) THEN
                CALL SVDF1LPF1B(GZ(LCHNEW),IG(LCIBOU),GX(LCCR),GX(LCCC),
     &                          GX(LCCV),GX(LCBOTM),NCOL,NROW,NLAY,
     &                          KKSTP,KKPER,ILPFCB,GX(LCBUFF),IOUT,
     &                          ICBCFL,DELT,PERTIM,TOTIM,IDIR,IBDRET,
     &                          ICHFLG,IC1,IC2,IR1,IR2,IL1,IL2,NBOTM,
     &                          VDF(LELEV),VDF(LPS),GX(LCDELR),
     &                          GX(LCDELC),IWTABLE,VDF(LHSALT))
              ELSE
                CALL SGWF1LPF1B(GZ(LCHNEW),IG(LCIBOU),GX(LCCR),GX(LCCC),
     &                          GX(LCCV),GX(LCBOTM),NCOL,NROW,NLAY,
     &                          KKSTP,KKPER,ILPFCB,GX(LCBUFF),IOUT,
     &                          ICBCFL,DELT,PERTIM,TOTIM,IDIR,IBDRET,
     &                          ICHFLG,IC1,IC2,IR1,IR2,IL1,IL2,NBOTM)
              ENDIF
157           CONTINUE
            ENDIF
C--SEAWAT: HUF BUDGET SUBROUTINES
C--SEAWAT:SVDF1HUF2S AND SGWF1HUF2S SUBROUTINES
            IF(IUNIT(37).GT.0) THEN
               IF(IUNIT(57).GT.0) THEN
              CALL SVDF1HUF2S(VBNM,VBVL,MSUM,GZ(LCHNEW),IG(LCIBOU),
     &                        GX(LCHOLD),X(LCSC1),GX(LCBOTM),DELT,
     &                        ISSFLG(KKPER),NCOL,NROW,NLAY,KKSTP,KKPER,
     &                        IHUFCB,ICBCFL,GX(LCBUFF),IOUT,PERTIM,
     &                        TOTIM,NBOTM,X(LCHUFTHK),NHUF,IG(LCIZON),
     &                        NZONAR,GX(LCRMLT),NMLTAR,GX(LCDELR),
     &                        GX(LCDELC),VDF(LPS),VDF(LELEV))
               ELSE
              CALL SGWF1HUF2S(VBNM,VBVL,MSUM,GZ(LCHNEW),IG(LCIBOU),
     &                        GX(LCHOLD),X(LCSC1),GX(LCBOTM),DELT,
     &                        ISSFLG(KKPER),NCOL,NROW,NLAY,KKSTP,KKPER,
     &                        IHUFCB,ICBCFL,GX(LCBUFF),IOUT,PERTIM,
     &                        TOTIM,NBOTM,X(LCHUFTHK),NHUF,IG(LCIZON),
     &                        NZONAR,GX(LCRMLT),NMLTAR,GX(LCDELR),
     &                        GX(LCDELC))
               ENDIF
C--SEAWAT:SVDF1HUF1F AND SGWF1HUF1F SUBROUTINES
            IF(IUNIT(57).GT.0) THEN
              CALL SVDF1HUF2F(VBNM,VBVL,MSUM,GZ(LCHNEW),IG(LCIBOU),
     &                        GX(LCCR),GX(LCCC),GX(LCCV),GX(LCBOTM),
     &                        DELT,NCOL,NROW,NLAY,KKSTP,KKPER,IHUFCB,
     &                        GX(LCBUFF),IOUT,ICBCFL,PERTIM,TOTIM,NBOTM,
     &                        ICHFLG,IUNIT(47),X(LCVDHT),VDF(LPS),
     &                        VDF(LELEV),MFNADVFD,GX(LCDELR),GX(LCDELC),
     &                        IWTABLE,VDF(LHSALT))
            ELSE
              CALL SGWF1HUF2F(VBNM,VBVL,MSUM,GZ(LCHNEW),IG(LCIBOU),
     &                        GX(LCCR),GX(LCCC),GX(LCCV),GX(LCBOTM),
     &                        DELT,NCOL,NROW,NLAY,KKSTP,KKPER,IHUFCB,
     &                        GX(LCBUFF),IOUT,ICBCFL,PERTIM,TOTIM,NBOTM,
     &                        ICHFLG,IUNIT(47),X(LCVDHT))
            ENDIF
              IBDRET=0
              IC1=1
              IC2=NCOL
              IR1=1
              IR2=NROW
              IL1=1
              IL2=NLAY
              DO 158 IDIR=1,3
C--SEAWAT: SVDF1HUF1B AND SGWF1HUF1B SUBROUTINES
                IF(IUNIT(57).GT.0) THEN
                CALL SVDF1HUF2B(GZ(LCHNEW),IG(LCIBOU),GX(LCCR),GX(LCCC),
     &                          GX(LCCV),GX(LCBOTM),NCOL,NROW,NLAY,
     &                          KKSTP,KKPER,IHUFCB,GX(LCBUFF),IOUT,
     &                          ICBCFL,DELT,PERTIM,TOTIM,IDIR,IBDRET,
     &                          ICHFLG,IC1,IC2,IR1,IR2,IL1,IL2,NBOTM,
     &                          IUNIT(47),X(LCVDHT),
     &                          VDF(LPS),VDF(LELEV),GX(LCDELR),
     &                          GX(LCDELC),IWTABLE,VDF(LHSALT))
                ELSE
                CALL SGWF1HUF2B(GZ(LCHNEW),IG(LCIBOU),GX(LCCR),GX(LCCC),
     &                          GX(LCCV),GX(LCBOTM),NCOL,NROW,NLAY,
     &                          KKSTP,KKPER,IHUFCB,GX(LCBUFF),IOUT,
     &                          ICBCFL,DELT,PERTIM,TOTIM,IDIR,IBDRET,
     &                          ICHFLG,IC1,IC2,IR1,IR2,IL1,IL2,NBOTM,
     &                          IUNIT(47),X(LCVDHT))
                ENDIF
158           CONTINUE
            ENDIF

C--WELLS
C--SEAWAT: VDF1WEL6BD 
            IF (IUNIT(2).GT.0.AND.IUNIT(57).GT.0)
     &          CALL VDF1WEL6BD(NWELLS,MXWELL,VBNM,VBVL,MSUM,
     &                          RX(LCWELL),IG(LCIBOU),DELT,NCOL,NROW,
     &                          NLAY,KKSTP,
     &                          KKPER,IWELCB,ICBCFL,GX(LCBUFF),IOUT,
     &                          PERTIM,TOTIM,NWELVL,IWELAL,IAUXSV,
     &                          VDF(LPS),MTDNCONC,MXSS,NSS,Y(LCSSM),
     &                          NCOMP,Y(LCSSMC),NSSVL)
            IF (IUNIT(2).GT.0.AND.IUNIT(57).EQ.0)
     &          CALL GWF1WEL6BD(NWELLS,MXWELL,VBNM,VBVL,MSUM,RX(LCWELL),
     &                          IG(LCIBOU),DELT,NCOL,NROW,NLAY,KKSTP,
     &                          KKPER,IWELCB,ICBCFL,GX(LCBUFF),IOUT,
     &                          PERTIM,TOTIM,NWELVL,IWELAL,IAUXSV)
C--DRAINS
C--SEAWAT: VDF1DRN6BD 
            IF (IUNIT(3).GT.0.AND.IUNIT(57).GT.0)
     &          CALL VDF1DRN6BD(NDRAIN,MXDRN,VBNM,VBVL,MSUM,RX(LCDRAI),
     &                          DELT,GZ(LCHNEW),NCOL,NROW,NLAY,
     &                          IG(LCIBOU),KKSTP,KKPER,IDRNCB,ICBCFL,
     &                          GX(LCBUFF),IOUT,PERTIM,TOTIM,NDRNVL,
     &                          IDRNAL,IAUXSV,VDF(LPS),VDF(LELEV))
            IF (IUNIT(3).GT.0.AND.IUNIT(57).EQ.0)
     &          CALL GWF1DRN6BD(NDRAIN,MXDRN,VBNM,VBVL,MSUM,RX(LCDRAI),
     &                          DELT,GZ(LCHNEW),NCOL,NROW,NLAY,
     &                          IG(LCIBOU),KKSTP,KKPER,IDRNCB,ICBCFL,
     &                          GX(LCBUFF),IOUT,PERTIM,TOTIM,NDRNVL,
     &                          IDRNAL,IAUXSV)
C--RIVERS
C--SEAWAT: VDF1RIV6BD 
            IF (IUNIT(4).GT.0.AND.IUNIT(57).GT.0)
     &          CALL VDF1RIV6BD(NRIVER,MXRIVR,RX(LCRIVR),IG(LCIBOU),
     &                          GZ(LCHNEW),NCOL,NROW,NLAY,DELT,VBVL,
     &                          VBNM,MSUM,KKSTP,KKPER,IRIVCB,ICBCFL,
     &                          GX(LCBUFF),IOUT,PERTIM,TOTIM,NRIVVL,
     &                          IRIVAL,IAUXSV,VDF(LELEV),VDF(LPS),
     &                          MTDNCONC,MXSS,NSS,Y(LCSSM),NCOMP,
     &                          Y(LCSSMC),NSSVL)
            IF (IUNIT(4).GT.0.AND.IUNIT(57).EQ.0)
     &          CALL GWF1RIV6BD(NRIVER,MXRIVR,RX(LCRIVR),IG(LCIBOU),
     &                          GZ(LCHNEW),NCOL,NROW,NLAY,DELT,VBVL,
     &                          VBNM,MSUM,KKSTP,KKPER,IRIVCB,ICBCFL,
     &                          GX(LCBUFF),IOUT,PERTIM,TOTIM,NRIVVL,
     &                          IRIVAL,IAUXSV)

C--EVAPOTRANSPIRATION
clak
C--SEAWAT:VDF1EVT6BD
            IF (IUNIT(5).GT.0.AND.IUNIT(57).GT.0) THEN
              IF(IUNIT(22).GT.0.AND.NEVTOP.EQ.3)
     1                CALL GWF1LAK3ST(0,NCOL,NROW,NLAY,IG(LCIBOU),
     2                            LKNODE,IR(ICLAKE),MXLKND,NLAKES,
     3                            RX(ISTGLD),GX(LCBOTM),NBOTM)
              CALL VDF1EVT6BD(NEVTOP,IR(LCIEVT),RX(LCEVTR),RX(LCEXDP),
     &                          RX(LCSURF),IG(LCIBOU),GZ(LCHNEW),NCOL,
     &                          NROW,NLAY,DELT,VBVL,VBNM,MSUM,KKSTP,
     &                          KKPER,IEVTCB,ICBCFL,GX(LCBUFF),IOUT,
     &                          PERTIM,TOTIM,VDF(LPS),VDF(LELEV),
     &                          MTDNCONC,Y(LTCEVT),Y(LCCNEW),NCOMP)
              IF(IUNIT(22).GT.0.AND.NEVTOP.EQ.3)
     1                CALL GWF1LAK3ST(1,NCOL,NROW,NLAY,IG(LCIBOU),
     2                            LKNODE,IR(ICLAKE),MXLKND,NLAKES,
     3                            RX(ISTGLD),GX(LCBOTM),NBOTM)
            END IF

C--SEAWAT: CHANGED IF STATEMENT BELOW
            IF (IUNIT(5).GT.0.AND.IUNIT(57).EQ.0) THEN
              IF(IUNIT(22).GT.0.AND.NEVTOP.EQ.3)
     1                CALL GWF1LAK3ST(0,NCOL,NROW,NLAY,IG(LCIBOU),
     2                            LKNODE,IR(ICLAKE),MXLKND,NLAKES,
     3                            RX(ISTGLD),GX(LCBOTM),NBOTM)
              CALL GWF1EVT6BD(NEVTOP,IR(LCIEVT),RX(LCEVTR),RX(LCEXDP),
     &                          RX(LCSURF),IG(LCIBOU),GZ(LCHNEW),NCOL,
     &                          NROW,NLAY,DELT,VBVL,VBNM,MSUM,KKSTP,
     &                          KKPER,IEVTCB,ICBCFL,GX(LCBUFF),IOUT,
     &                          PERTIM,TOTIM)
              IF(IUNIT(22).GT.0.AND.NEVTOP.EQ.3)
     1                CALL GWF1LAK3ST(1,NCOL,NROW,NLAY,IG(LCIBOU),
     2                            LKNODE,IR(ICLAKE),MXLKND,NLAKES,
     3                            RX(ISTGLD),GX(LCBOTM),NBOTM)
            END IF

C--GENERAL-HEAD BOUNDARIES
C--SEAWAT: VDF1GHB6 
            IF (IUNIT(7).GT.0.AND.IUNIT(57).GT.0)
     &          CALL VDF1GHB6BD(NBOUND,MXBND,VBNM,VBVL,MSUM,RX(LCBNDS)
     &                          ,DELT,GZ(LCHNEW),NCOL,NROW,NLAY,
     &                          IG(LCIBOU),KKSTP,KKPER,IGHBCB,ICBCFL,
     &                          GX(LCBUFF),IOUT,PERTIM,TOTIM,NGHBVL,
     &                          IGHBAL,IAUXSV,VDF(LPS),VDF(LELEV),
     &                          MTDNCONC,MXSS,NSS,Y(LCSSM),NCOMP,
     &                          Y(LCSSMC),NSSVL)
            IF (IUNIT(7).GT.0.AND.IUNIT(57).EQ.0)
     &          CALL GWF1GHB6BD(NBOUND,MXBND,VBNM,VBVL,MSUM,RX(LCBNDS),
     &                          DELT,GZ(LCHNEW),NCOL,NROW,NLAY,
     &                          IG(LCIBOU),KKSTP,KKPER,IGHBCB,ICBCFL,
     &                          GX(LCBUFF),IOUT,PERTIM,TOTIM,NGHBVL,
     &                          IGHBAL,IAUXSV)
C--RECHARGE
clak
C--SEAWAT:VDF1RCH6BD
            IF (IUNIT(8).GT.0.AND.IUNIT(57).GT.0) THEN
              IF(IUNIT(22).GT.0.AND.NRCHOP.EQ.3)
     1                CALL GWF1LAK3ST(0,NCOL,NROW,NLAY,IG(LCIBOU),
     2                            LKNODE,IR(ICLAKE),MXLKND,NLAKES,
     3                            RX(ISTGLD),GX(LCBOTM),NBOTM)

              CALL VDF1RCH6BD(NRCHOP,IR(LCIRCH),RX(LCRECH),IG(LCIBOU),
     &                          NROW,NCOL,NLAY,DELT,VBVL,
     &                          VBNM,MSUM,KKSTP,KKPER,IRCHCB,ICBCFL,
     &                          GX(LCBUFF),IOUT,PERTIM,TOTIM,VDF(LPS),
     &                          MTDNCONC,Y(LTCRCH),NCOMP)

              IF(IUNIT(22).GT.0.AND.NRCHOP.EQ.3)
     1                CALL GWF1LAK3ST(1,NCOL,NROW,NLAY,IG(LCIBOU),
     2                            LKNODE,IR(ICLAKE),MXLKND,NLAKES,
     3                            RX(ISTGLD),GX(LCBOTM),NBOTM)
            END IF
            IF (IUNIT(8).GT.0.AND.IUNIT(57).EQ.0) THEN
              IF(IUNIT(22).GT.0.AND.NRCHOP.EQ.3)
     1                CALL GWF1LAK3ST(0,NCOL,NROW,NLAY,IG(LCIBOU),
     2                            LKNODE,IR(ICLAKE),MXLKND,NLAKES,
     3                            RX(ISTGLD),GX(LCBOTM),NBOTM)
                CALL GWF1RCH6BD(NRCHOP,IR(LCIRCH),RX(LCRECH),IG(LCIBOU),
     &                          NROW,NCOL,NLAY,DELT,VBVL,VBNM,MSUM,
     &                          KKSTP,KKPER,IRCHCB,ICBCFL,GX(LCBUFF),
     &                          IOUT,PERTIM,TOTIM)
              IF(IUNIT(22).GT.0.AND.NRCHOP.EQ.3)
     1                CALL GWF1LAK3ST(1,NCOL,NROW,NLAY,IG(LCIBOU),
     2                            LKNODE,IR(ICLAKE),MXLKND,NLAKES,
     3                            RX(ISTGLD),GX(LCBOTM),NBOTM)
            END IF
C--Specified-Flow and Specified-Head Boundary
            IF(IUNIT(16).GT.0.AND.IUNIT(57).EQ.0)
     &          CALL GWF1FHB1BD(IR(LCFLLC),RX(LCBDFV),NFLW,VBNM,VBVL,
     &                      MSUM,IG(LCIBOU),DELT,NCOL,NROW,NLAY,KKSTP,
     &                      KKPER,IFHBCB,ICBCFL,GX(LCBUFF),IOUT,IFHBD4,
     &                      NFLWDIM)
C--SEAWAT: VARIABLE DENSITY FHB BUDGET
            IF(IUNIT(16).GT.0.AND.IUNIT(57).GT.0)
     &          CALL VDF1FHB1BD(IR(LCFLLC),RX(LCBDFV),NFLW,VBNM,VBVL,
     &                      MSUM,IG(LCIBOU),DELT,NCOL,NROW,NLAY,KKSTP,
     &                      KKPER,IFHBCB,ICBCFL,GX(LCBUFF),IOUT,IFHBD4,
     &                      NFLWDIM,VDF(LPS),MTDNCONC,MXSS,NSS,Y(LCSSM),
     &                      NCOMP,Y(LCSSMC),NSSVL)
C--RESERVOIRS
            IF (IUNIT(17).GT.0)
     &          CALL GWF1RES1BD(IR(LCIRES),IR(LCIRSL),RX(LCBRES),
     &                      RX(LCCRES),RX(LCBBRE),RX(LCHRES),IG(LCIBOU),
     &                      GZ(LCHNEW),GX(LCBUFF),VBVL,VBNM,MSUM,KKSTP,
     &                      KKPER,NRES,NRESOP,NCOL,NROW,NLAY,DELT,
     &                      IRESCB,ICBCFL,IOUT)
C--STREAM-AQUIFER RELATIONS (STR6 PACKAGE)
            IF(IUNIT(18).GT.0)
     &          CALL GWF1STR6BD(NSTREM,RX(LCSTRM_),IR(ICSTRM_),
     &                      IG(LCIBOU),MXSTRM,GZ(LCHNEW),NCOL,NROW,
     &                      NLAY,DELT,VBVL,VBNM,MSUM,KKSTP,KKPER,
     &                      ISTCB1STR6,ISTCB2STR6,ICBCFL,GX(LCBUFF),
     &                      IOUT,NTRIB,NSSSTR6,RX(LCTRIB),IR(LCTBAR),
     &                      IR(LCIVAR_),IR(LCFGAR),ICALC,CONSTSTR6,
     &                      IPTFLG)
C--INTERBED STORAGE
            IF (IUNIT(19).GT.0)
     1          CALL GWF1IBS6BD(IG(LCIBOU),GZ(LCHNEW),GX(LCHOLD),
     2                          RX(LCHC),RX(LCSCE),RX(LCSCV),RX(LCSUB),
     3                          GX(LCDELR),GX(LCDELC),NCOL,NROW,NLAY,
     4                          DELT,VBVL,VBNM,MSUM,KSTP,KPER,IIBSCB,
     5                          ICBCFL,GX(LCBUFF),IOUT,ISSFLG(KKPER),
     6                          IBSDIM)
            IF(IUNIT(54).GT.0)
     1          CALL GWF1SUB1BD(IG(LCIBOU),GZ(LCHNEW),GX(LCHOLD),
     2                          GX(LCBUFF),GX(LCDELR),GX(LCDELC),VBVL,
     3                          VBNM,NN,NND1,ND1,ND2,NDB,NNDB,NPZ,NCOL,
     4                          NROW,NLAY,NODES,DELT,MSUM,NIUNIT,KKSTP,
     5                          KKPER,ISUBCB,ICBCFL,ISSFLG(KKPER),IOUT)
C--LAKES
            IF (IUNIT(22).GT.0)
     &          CALL GWF1LAK3BD(LKNODE,MXLKND,NODES,IR(ICLAKE),
     &                  GZ(LCHNEW),IG(LCIBOU),NCOL,NROW,NLAY,NLAKES,
     &                  DELT,NSS,NTRB,NDV,IR(INTRB),IR(INDV),RX(ISTRIN),
     &                  RX(ISTROT),RX(ISTGLD),RX(ISTGNW),RX(LCCNDF),
     &                  RX(LCLKPR),RX(LCLKEV),GX(LCDELR),GX(LCDELC),
     &                  GX(LCBOTM),NBOTM,VBVL,VBNM,MSUM,KSTP,KPER,
     &                  ILKCB,ICBCFL,GX(LCBUFF),IOUT,RX(LCSTAG),
     &                  PERTIM,TOTIM,IR(IICS),IR(IISUB),RX(ISILL),
     &                  ICMX,NCLS,RX(LCWTDR),LWRT,RX(LKACC1),
     &                  RX(LKACC2),RX(LKACC3),RX(LKACC4),RX(LKACC5),
     &                  RX(LKACC6),RX(LKACC7),RX(LKACC8),RX(LKACC9),
     &                  RX(LKACC10),RX(LKACC11),IR(LKDRY),RX(IBTMS),
     &                  IR(LKNCNT),IR(LKKSUB),RX(LKSADJ),RX(LKFLXI),
     &                  IR(LKNCNS),RX(LKSVT),IR(LKJCLS),RX(LCRNF),
     &                  THETA,RX(LKCNN),RX(LKCHN),RX(IAREN),
     &                  IUNIT(15),KCNT,IR(IMSUB),IR(IMSUB1),
     &                  IUNIT(46),NUMGAGE,IR(LSGAGE),RX(LSOVOL),
     &                  RX(LSFLOB),ISSFLG(KPER),LAYHDT,
     &                  IAUXSV,RX(LKVI),RX(ISTGLD2),RX(LKCUM1),
     &                  RX(LKCUM2),RX(LKCUM3),
     &                  RX(LKCUM4),RX(LKCUM5),RX(LKCUM6),
     &                  RX(LKCUM7),RX(LKCUM8),RX(LKCUM9))
C--EVAPOTRANSPIRATION WITH SEGMENTED RATE FUNCTION
            IF (IUNIT(39).GT.0)
     &          CALL GWF1ETS1BD(NETSOP,IR(LCIETS),RX(LCETSR),RX(LCETSX),
     &                          RX(LCETSS),IG(LCIBOU),GZ(LCHNEW),NCOL,
     &                          NROW,NLAY,DELT,VBVL,VBNM,MSUM,KKSTP,
     &                          KKPER,IETSCB,ICBCFL,GX(LCBUFF),IOUT,
     &                          PERTIM,TOTIM,NETSEG,RX(LCPXDP),
     &                          RX(LCPETM),NSEGAR)
C--DRAINS WITH RETURN FLOW
            IF (IUNIT(40).GT.0)
     &          CALL GWF1DRT1BD(NDRTCL,MXDRT,VBNM,VBVL,MSUM,RX(LCDRTF),
     &                          DELT,GZ(LCHNEW),NCOL,NROW,NLAY,
     &                          IG(LCIBOU),KKSTP,KKPER,IDRTCB,ICBCFL,
     &                          GX(LCBUFF),IOUT,PERTIM,TOTIM,NDRTVL,
     &                          IDRTAL,IDRTFL,NRFLOW,IAUXSV)
            IF(IUNIT(51).GT.0)
     1          CALL GWF1DAF1BD(IUNIT(52)+1,IOUT,ITMUNI,DELT,VBVL,
     2                          VBNM,MSUM,KSTP,KPER,IDAFCB,ICBCFL,
     3                          GX(LCBUFF),PERTIM,TOTIM,NCOL,NROW,
     4                          NLAY,IG(LCIBOU))
C--MULTINODE WELLS
            IF(IUNIT(50).GT.0)
     &          CALL GWF1MNW1BD(MNWSITE,NWELL2,MXWEL2,VBNM,VBVL,MSUM,
     &                          DELT,RX(LCWEL2),IG(LCIBOU),GZ(LCHNEW),
     &                          NCOL,NROW,NODES,NSTP(KKPER),KKSTP,KKPER,
     &                          IWL2CB,ICBCFL,GX(LCBUFF),IOUT,IOWELL2,
     &                          TOTIM,PLOSSMNW,HDRY)
            IF (IUNIT(43).GT.0)
     &          CALL GWF1HYD1OT(GZ,LENGZ,RX,LENRX,IG,LENIG,RX(LCHYDM),
     &                        NUMH,IHYDMUN,TOTIM,HYDNOH,NROW,NCOL,
     &                        ITMUNI,IOUT)
C--SEAWAT: VDF BUDGET
            IF (IUNIT(57).GT.0)
     &          CALL VDF1BD(VBNM,VBVL,MSUM,IG(LCIBOU),DELT,NCOL,
     &                        NROW,NLAY,KKSTP,KKPER,IBCFCB,ILPFCB,
     &                          IHUFCB,ICBCFL,GX(LCBUFF),IOUT,
     &                          PERTIM,TOTIM,VDF(LDCDT),MTDNCONC,
     &                          VDF(LPS))

C--SEAWAT: RECALCULATE DCDT FOR FLOW SOLUTION IN NEXT TRANSPORT STEP
            IF (IUNIT(57).GT.0.AND.MTDNCONC.GT.0) 
     &                 CALL VDF1DC(NCOL,NROW,NLAY,NCOMP,Y(LCCOLD),
     &                 Y(LCCNEW),Y(LCPR),GX(LCDELR),GX(LCDELC),
     &                 Y(LCDH),VDF(LDCDT),DELT,MTDNCONC,IG(LCIBOU),
     &                 VDF(LPS),CINACT)

            IF(IUNIT(58).EQ.0) GOTO 900

C--SEAWAT: CALCULATE MASS BUDGETS FOR IMPLICIT SCHEMES
           IMTIMBLOOP: DO ICOMP=1,NCOMP
C       IF(IMPSOL.NE.1) GOTO 2000
            IF(IUNIT(59).GT.0.AND.MIXELM.EQ.0 .AND. ICOMP.LE.MCOMP)
     &       CALL IMT1ADV4BD(IOUT,NCOL,NROW,NLAY,MCOMP,ICOMP,NADVFD,
     &        IY(LCIB),Y(LTDELR),Y(LTDELC),Y(LCDH),Y(LCQX),Y(LCQY),
     &        Y(LCQZ),Y(LCCNEW),DTRANS,RMASIO)
C
            IF(IUNIT(60).GT.0.AND. ICOMP.LE.MCOMP)
     &       CALL IMT1DSP4BD(NCOL,NROW,NLAY,MCOMP,ICOMP,IY(LCIB),
     &        Y(LTDELR),Y(LTDELC),Y(LCDH),Y(LCDXX),Y(LCDXY),Y(LCDXZ),
     &        Y(LCDYX),Y(LCDYY),Y(LCDYZ),Y(LCDZX),Y(LCDZY),Y(LCDZZ),
     &        Y(LCCNEW),Y(LTBUFF),DTRANS,RMASIO)
C
            IF(IUNIT(61).GT.0.AND. ICOMP.LE.MCOMP)
     &       CALL IMT1SSM4BD(NCOL,NROW,NLAY,NCOMP,ICOMP,IY(LCIB),
     &        Y(LTDELR),Y(LTDELC),Y(LCDH),IY(LTIRCH),Y(LTRECH),
     &        Y(LTCRCH),IY(LTIEVT),Y(LTEVTR),Y(LTCEVT),MXSS,NTSS,
     &        Y(LCSSM),Y(LCSSMC),Y(LCQSTO),Y(LCCNEW),Y(LCRETA),
     &        DTRANS,ISS,RMASIO)
C
            IF(IUNIT(62).GT.0) CALL IMT1RCT4BD(NCOL,NROW,NLAY,NCOMP,
     &        ICOMP,
     &        IY(LCIB),Y(LCPR),Y(LTDELR),Y(LTDELC),Y(LCDH),DTRANS,
     &        ISOTHM,
     &        IREACT,Y(LCRHOB),Y(LCSP1),Y(LCSP2),Y(LCSR),Y(LCRC1),
     &        Y(LCRC2),
     &        Y(LCPRSITY2),Y(LCRETA2),Y(LCFRAC),Y(LCCNEW),Y(LCRETA),
     &        RFMIN,RMASIO)
C
 2000       CONTINUE
C--SEAWAT: CALCULATE GLOBAL MASS BUDGETS AND CHECK MASS BALANCE
            CALL IMT1BTN4BD(KPER,KSTP,N,NCOL,NROW,NLAY,NCOMP,ICOMP,ISS,
     &       IY(LCIB),Y(LTDELR),Y(LTDELC),Y(LCDH),Y(LCPR),Y(LCRETA),
     &       Y(LCCNEW),Y(LCCOLD),Y(LCRHOB),Y(LCSR),Y(LCPRSITY2),
     &       Y(LCRETA2),
     &       ISOTHM,DTRANS,TMASIN,TMASOT,ERROR,ERROR2,TMASIO,RMASIO,
     &       TMASS)
C
C--SEAWAT: SAVE OUTPUTS, CHANGE IOBS TO IMOBS
            CALL IMT1BTN4OT(NCOL,NROW,NLAY,KPER,KSTP,N,NCOMP,ICOMP,
     &       IOUT,IMOBS,IUCN,IUCN2,IMAS,ICBM,MXOBS,NOBS,NPROBS,LOCOBS,
     &       IY(LCIB),TIME2,Y(LCCNEW),MIXELM,NCOUNT,NPINS,NRC,
     &       IY(LCCHEK),ISOTHM,Y(LCRETA),Y(LCSR),TMASIN,TMASOT,ERROR,
     &       ERROR2,TRNOP,TUNIT,MUNIT,PRTOUT,TMASIO,RMASIO,TMASS)
C
C--SEAWAT: ENDDO FOR IMT MASS BUDGETS FOR IMPLICIT SCHEMES
           ENDDO IMTIMBLOOP

clange--moved ps recalculate to here
           IF(IUNIT(57).GT.0) CALL VDF1PS(NCOL,NROW,NLAY,NCOMP,
     &       Y(LCCNEW),CINACT,VDF(LPS),IOUT,MTDNCONC)

           IF(TIME2.GE.HT2) GOTO 900
           IF(IMPSOL.EQ.1.AND.ICNVGMT.EQ.0) THEN
             WRITE(*,*) 'STOP. GCG SOLVER FAILED TO CONVERGE.'
             STOP
           ENDIF

C--END OF SWT2K TRANSPORT STEP LOOP
          ENDDO SWT2KTSLOOP


          IF(TIME2.LT.HT2) THEN
           WRITE(IOUT,810) MXSTRN
  810      FORMAT(/1X,'NUMBER OF TRANSPORT STEPS EXCEEDS',
     &   ' SPECIFIED MAXIMUM (MXSTRN) =',I10)
           STOP
          ENDIF
  900     CONTINUE

CLMT
CLMT----CALL LINK-MT3DMS SUBROUTINES TO SAVE FLOW-TRANSPORT LINK FILE
CLMT----FOR USE BY MT3DMS FOR TRANSPORT SIMULATION
CLMT
            INCLUDE 'lmt6vd.inc'
CLMT


C--SEAWAT: IN THE OBS AND SENS SUBROUTINE TO VALUES W/O MST PROCESS ACTIVE
          IF(IUNIT(58).GT.0) THEN
           DELT=PERLEN(KPER)
           ITS=KPER
          ENDIF

C--SEAWAT: VARIABLES BELOW SHOULD NOT BE AFFECTED IF IMT IS ACTIVE. 
C
C7C5---PRINT AND/OR SAVE HEAD AND DRAWDOWN MATRICES.
C------PRINT OVERALL WATER BUDGET.
		IF(IUNIT(57).EQ.0) THEN
			IF(IUNIT(58).EQ.0) THEN
			  CALL GWF1BAS6OT(GZ(LCHNEW),GX(LCSTRT),ISTRT,GX(LCBUFF),
     &                      IR(LCIOFL),MSUM,IG(LCIBOU),VBNM,VBVL,KKSTP,
     &                      KKPER,DELT,PERTIM,TOTIM,ITMUNI,NCOL,NROW,
     &                      NLAY,ICNVG,IHDDFL,IBUDFL,IHEDFM,IHEDUN,
     &                      IDDNFM,IDDNUN,IOUT,CHEDFM,CDDNFM,IXSEC,
     &                      LBHDSV,LBDDSV,IBOUUN,LBBOSV,CBOUFM,ISA,
     &                      RESETDD)
			ELSE
                CALL VDF1BAS6OT(GZ(LCHNEW),GX(LCSTRT),ISTRT,GX(LCBUFF),
     &                      IR(LCIOFL),MSUM,IG(LCIBOU),VBNM,VBVL,KKSTP,
     &                      KKPER,DELT,PERTIM,TOTIM,ITMUNI,NCOL,NROW,
     &                      NLAY,ICNVG,IHDDFL,IBUDFL,IHEDFM,IHEDUN,
     &                      IDDNFM,IDDNUN,IOUT,CHEDFM,CDDNFM,IXSEC,
     &                      LBHDSV,LBDDSV,IBOUUN,LBBOSV,CBOUFM,ISA,
     &                      RESETDD,
     &                      IUNIT(57),IUNIT(58),DTRANS,N,HT1,HT2)
	        ENDIF
          ELSE
C--SEAWAT: PASS HSALT,IUNIT(57),IUNIT(58),DTRANS,N,HT1,HT2
            CALL VDF1BAS6OT(VDF(LHSALT),GX(LCSTRT),ISTRT,GX(LCBUFF),
     &                      IR(LCIOFL),MSUM,IG(LCIBOU),VBNM,VBVL,KKSTP,
     &                      KKPER,DELT,PERTIM,TOTIM,ITMUNI,NCOL,NROW,
     &                      NLAY,ICNVG,IHDDFL,IBUDFL,IHEDFM,IHEDUN,
     &                      IDDNFM,IDDNUN,IOUT,CHEDFM,CDDNFM,IXSEC,
     &                      LBHDSV,LBDDSV,IBOUUN,LBBOSV,CBOUFM,ISA,
     &                      RESETDD,
     &                      IUNIT(57),IUNIT(58),DTRANS,N,HT1,HT2)
          ENDIF
            IF (IUNIT(19).GT.0)
     1          CALL GWF1IBS6OT(NCOL,NROW,NLAY,PERTIM,TOTIM,KKSTP,
     2                          KKPER,NSTP(KKPER),GX(LCBUFF),RX(LCSUB),
     3                          RX(LCHC),IIBSOC,ISUBFM,ICOMFM,IHCFM,
     4                          ISUBUN,ICOMUN,IHCUN,IUNIT(19),IOUT,
     5                          ISSFLG(KKPER),IBSDIM)
            IF(IUNIT(54).GT.0)
     1          CALL GWF1SUB1OT(NCOL,NROW,NLAY,PERTIM,TOTIM,KKSTP,
     2                          KKPER,NSTP(KKPER),GX(LCBUFF),NODES,NN,
     3                          ND1,ND2,NND1,NNDB,NDB,ISSFLG(KKPER),
     4                          IUNIT(54),IOUT)
C------PRINT AND/OR SAVE HEADS INTERPOLATED TO HYDROGEOLOGIC UNITS
		IF(IUNIT(57).EQ.0) THEN
            IF(IUNIT(37).GT.0.AND.(IOHUFHDS.NE.0.OR.IOHUFFLWS.NE.0))
     &          CALL GWF1HUF2OT(IOHUFHDS,IOHUFFLWS,GZ(LCHNEW),IHEDFM,
     &                      IG(LCIBOU),NHUF,NCOL,NROW,NLAY,X(LCHUFTHK),
     &                      GX(LCBOTM),NBOTM,GX(LCCV),GX(LCDELR),
     &                      GX(LCDELC),GX(LCRMLT),NMLTAR,IG(LCIZON),
     &                      NZONAR,KKSTP,KKPER,ISA,ICNVG,IOUT,HNOFLO,
     &                      CHEDFM,DELT, PERTIM,TOTIM,X(LCHUFTMP),
     &                      X(LCGS),ICBCFL,ICHFLG)
		ELSE
		  IF(IUNIT(37).GT.0.AND.(IOHUFHDS.NE.0.OR.IOHUFFLWS.NE.0))
     &          CALL VDF1HUF2OT(IOHUFHDS,IOHUFFLWS,GZ(LCHNEW),IHEDFM,
     &                      IG(LCIBOU),NHUF,NCOL,NROW,NLAY,X(LCHUFTHK),
     &                      GX(LCBOTM),NBOTM,GX(LCCV),GX(LCDELR),
     &                      GX(LCDELC),GX(LCRMLT),NMLTAR,IG(LCIZON),
     &                      NZONAR,KKSTP,KKPER,ISA,ICNVG,IOUT,HNOFLO,
     &                      CHEDFM,DELT, PERTIM,TOTIM,X(LCHUFTMP),
     &                      X(LCGS),ICBCFL,ICHFLG,VDF(LHSALT),VDF(LPS),
     &                      VDF(LELEV))
		ENDIF
C
C-------OBSERVATION CALCULATIONS
          IF (IPAR.NE.-3 .AND. ND.GT.0) THEN
C
C7C6----IF ITERATION FAILED TO CONVERGE THEN STOP.
          IF (ICNVG.EQ.0) CALL PLL1SD(IERR,IERRU,IOUT,IOUTG)
C-------------SHOW PROGRESS IF REQUESTED
              IF(SHOWPROG)THEN
                IF (KPER.EQ.1 .AND. KSTP.EQ.1 .AND. IPES.GT.0) THEN
                  WRITE(*,26)
                ENDIF
              ENDIF
C
C-------WRITE INITIAL PARAMETER VALUES ON FILE IOUB
          IF (IPES.GT.0)
     &          CALL PES1BAS6WB(X(LCBL),X(LCBU),IX(LCISEN),IOUB,ITERP,
     &                          ITS,IX(LCLN),NPLIST,X(LCBSCA),IOSTAR,
     &                          NPE,X(LCPARE),ITMXP,ITERPF,ITERPK)
C-------------SHOW PROGRESS IF REQUESTED
              IF(SHOWPROG)THEN
                IF (KPER.EQ.1 .AND. KSTP.EQ.1 .AND. IPES.GT.0) THEN
                  WRITE(*,'(A)')' '
                  IF (ITERPK.EQ.1) WRITE(*,'(A)')' '
                ELSEIF (KPER.EQ.NPER .AND. KSTP.EQ.NSTP(KKPER)) THEN
                  WRITE(*,26)
                ENDIF
              ENDIF
C
C-------INTERPOLATE, SAVE AND PRINT DATA FOR OBSERVATIONS.
C--SEAWAT: 
          IF(IUNIT(57).EQ.0) THEN 
              IF (IUNIT(28).NE.0 .AND. NH.GT.0)
     &            CALL OBS1BAS6HFM(NH,IX(LCNDER),IX(LCIOFF),IX(LCJOFF),
     &                             IX(LCMLAY),IG(LCIBOU),X(LCRINT),
     &                             OBSNAM,X(LCCOFF),X(LCROFF),
     &                             GX(LCDELR),GX(LCDELC),NCOL,NROW,NLAY,
     &                             X(LCPR),X(LCH),X(LCWT),GZ(LCHNEW),
     &                             IDRY,NPE,X(LCTOFF),MAXM,JDRY,
     &                             IPAR,IOUT,ITS,NHAR,MOBSAR,ND,IPES,
     &                             IYCFLG,GX(LCSTRT))
			IF (NQ.GT.0) THEN
				IF (IUNIT(3).NE.0)
     &              CALL OBS1DRN6FM(NQ,IX(LCNQOB),IX(LCNQCL),
     &                              IX(LCIQOB),X(LCQCLS),IX(LCIBT),
     &                              GZ(LCHNEW),NCOL,NROW,NLAY,IOUT,
     &                              IG(LCIBOU),NHT,OBSNAM,X(LCH),
     &                              X(LCTOFF),MXDRN,NDRAIN,RX(LCDRAI),
     &                              X(LCWTQ),NDMH,ITS,NQAR,NQCAR,NQTAR,
     &                              NDRNVL,ND)
				IF (IUNIT(4).NE.0)
     &              CALL OBS1RIV6FM(NQ,IX(LCNQOB),IX(LCNQCL),
     &                              IX(LCIQOB),X(LCQCLS),IX(LCIBT),
     &                              MXRIVR,NRIVER,RX(LCRIVR),GZ(LCHNEW),
     &                              NCOL,NROW,NLAY,IOUT,IG(LCIBOU),NH,
     &                              OBSNAM,X(LCH),X(LCTOFF),X(LCWTQ),
     &                              NDMH,ITS,NQAR,NQCAR,NQTAR,NRIVVL,ND)
				IF (IUNIT(7).NE.0)
     &              CALL OBS1GHB6FM(NQ,IX(LCNQOB),IX(LCNQCL),
     &                              IX(LCIQOB),X(LCQCLS),IX(LCIBT),
     &                              MXBND,NBOUND,RX(LCBNDS),GZ(LCHNEW),
     &                              NCOL,NROW,NLAY,IOUT,IG(LCIBOU),NHT,
     &                              OBSNAM,X(LCH),X(LCTOFF),ITS,NQAR,
     &                              NQCAR,NQTAR,NGHBVL,ND,X(LCWTQ),NDMH)
				IF (IUNIT(18).NE.0)
     &              CALL OBS1STR6FM(NQ,IX(LCNQOB),IX(LCNQCL),
     &                              IX(LCIQOB),X(LCQCLS),IX(LCIBT),
     &                              GZ(LCHNEW),NCOL,NROW,NLAY,IOUT,
     &                              IG(LCIBOU),NHT,OBSNAM,X(LCH),
     &                              X(LCTOFF),MXSTRM,NSTREM,RX(LCSTRM_),
     &                              IR(ICSTRM_),X(LCWTQ),NDMH,ITS,NQAR,
     &                              NQCAR,NQTAR,ND)
				IF (IUNIT(38).NE.0)
     &              CALL OBS1BAS6FFM(NQ,IX(LCNQOB),IX(LCNQCL),
     &                               IX(LCIQOB),X(LCQCLS),IX(LCIBT),
     &                               GZ(LCHNEW),NCOL,NROW,NLAY,
     &                               IG(LCIBOU),NHT,X(LCH),
     &                               X(LCTOFF),ITS,NQAR,NQCAR,NQTAR,
     &                               ICHFLG,GX(LCCR),GX(LCCC),GX(LCCV),
     &                               GX(LCBOTM),NBOTM,LAYHDT,ND,IOUT,
     &                               KKPER)
				IF (IUNIT(40).NE.0)
     &              CALL OBS1DRT1FM(NQ,IX(LCNQOB),IX(LCNQCL),
     &                              IX(LCIQOB),X(LCQCLS),IX(LCIBT),
     &                              GZ(LCHNEW),NCOL,NROW,NLAY,IOUT,
     &                              IG(LCIBOU),NHT,OBSNAM,X(LCH),
     &                              X(LCTOFF),MXDRT,NDRTCL,RX(LCDRTF),
     &                              X(LCWTQ),NDMH,ITS,NQAR,NQCAR,NQTAR,
     &                              NDRTVL,ND)
			ENDIF
              IF (IUNIT(29).NE.0 .AND. ISSFLG(KKPER).EQ.1)
     &            CALL OBS1ADV2P(NROW,NCOL,NLAY,GX(LCDELC),GX(LCDELR),
     &                           IOUT,GX(LCCR),GX(LCCC),GX(LCCV),
     &                           GZ(LCHNEW),IG(LCIBOU),OBSNAM,X(LCPOFF),
     &                           NHT,NQT,NTT2,NPTH,IX(LCNPNT),KTDIM,
     &                           KTFLG,KTREV,ADVSTP,
     &                           IX(LCICLS),X(LCPRST),NPRST,0,
     &                           GX(LCRMLT),X(LCHK),IG(LCIZON),X(LCH),
     &                           X(LCX),NPE,ND,X(LCTT2),IPRINT,ITERP,
     &                           IOUTT2,MXBND,NBOUND,RX(LCBNDS),NRCHOP,
     &                           IR(LCIRCH),RX(LCRECH),MXSTRM,NSTREM,
     &                           IR(ICSTRM_),RX(LCSTRM_),MXRIVR,NRIVER,
     &                           RX(LCRIVR),MXDRN,NDRAIN,RX(LCDRAI),
     &                           X(LCSV),NMLTAR,NZONAR,GX(LCBOTM),NBOTM,
     &                           RX(LCWELL),NWELVL,MXWELL,NWELLS,
     &                           Z(LCSNEW),X(LCVKA),
     &                           IUNIT(21),RX(LCHFB),MXACTFB,NHFB,
     &                           X(LCHANI),NGHBVL,NRIVVL,NDRNVL,LAYHDT,
     &                           IX(LCLN),NPLIST,ISCALS,FSNK,X(LCWTQ),
     &                           NDMH,X(LCBSCA),X(LCHKCC),X(LCHUFTHK),
     &                           NHUF,IUNIT(23),IUNIT(37),X(LCGS),
     &                           X(LCVDHT),IUNIT(47),X(LCDVDH),NPADV,
     &                           IPFLG,IADVHUF,IADVPER,KKPER)
              CALL OBS1BAS6SS(IOUT,NPE,NH,OBSNAM,KKPER,KKSTP,X(LCBUF1),
     &                        X(LCX),X(LCH),X(LCWT),X(LCHOBS),IPRINT,
     &                        IFO,ITERP,IPAR,NPER,IX(LCLN),LASTX,ISCALS,
     &                        X(LCWP),MPR,X(LCPRM),RSQ,
     &                        RSQP,IPR,IX(LCNIPR),X(LCWTPS),ND,
     &                        X(LCWTQ),X(LCWTQS),IOWTQ,NDMH,NTT2,KTDIM,
     &                        IOSTAR,NPLIST,NSTP,MPRAR,IPRAR,OUTNAM,
     &                        IX(LCIPLO),EQNAM,NAMES,IX(LCIPLP),NDMHAR,
     &                        NQTDR,NQTRV,NQTGB,NQTST,NQTCH,IOWTQCH,
     &                        IOWTQDR,IOWTQRV,IOWTQGB,IOWTQST,LCOBBAS,
     &                        LCOBDRN,LCOBRIV,LCOBGHB,LCOBSTR,LCOBCHD,
     &                        LCOBADV,X(LCSSGF),X(LCSSDR),X(LCSSRV),
     &                        X(LCSSGB),X(LCSSST),X(LCSSAD),X(LCSSCH),
     &                        X(LCSSPI),X(LCSSTO),ITMXP,IOUTG,X(LCBUF2),
     &                        IPES,X(LCBPRI),X(LCBSCA),X(LCRSQA),
     &                        X(LCRSPA),LCOBDRT,X(LCSSDT),NQTDT,IOWTQDT,
     &                        NRSO,NPOST,NNEGT,NRUNS,NQTSF,IOWTQSF,
     &                        LCOBSFR,X(LCSSSF),NHT,X(LCOTIM),OBSALL)

C--SEAWAT: VARIABLE DENSITY OBSERVATION PROCESS SUBROUTINES
          ELSE
              IF (IUNIT(28).NE.0 .AND. NH.GT.0)
C--SEAWAT: PASS HSALT
     &            CALL OBS1BAS6HFM(NH,IX(LCNDER),IX(LCIOFF),IX(LCJOFF),
     &                             IX(LCMLAY),IG(LCIBOU),X(LCRINT),
     &                             OBSNAM,X(LCCOFF),X(LCROFF),
     &                             GX(LCDELR),GX(LCDELC),NCOL,NROW,NLAY,
     &                             X(LCPR),X(LCH),X(LCWT),VDF(LHSALT),
     &                             IDRY,NPE,X(LCTOFF),MAXM,JDRY,
     &                             IPAR,IOUT,ITS,NHAR,MOBSAR,ND,IPES,
     &                             IYCFLG,GX(LCSTRT))
              IF (NQ.GT.0) THEN
                IF (IUNIT(3).NE.0)
C--SEAWAT: OBS1DRN6FMVD
     &              CALL OBS1DRN6FMVD(NQ,IX(LCNQOB),IX(LCNQCL),
     &                              IX(LCIQOB),X(LCQCLS),IX(LCIBT),
     &                              GZ(LCHNEW),NCOL,NROW,NLAY,IOUT,
     &                              IG(LCIBOU),NHT,OBSNAM,X(LCH),
     &                              X(LCTOFF),MXDRN,NDRAIN,RX(LCDRAI),
     &                              X(LCWTQ),NDMH,ITS,NQAR,NQCAR,NQTAR,
     &                              NDRNVL,ND,VDF(LPS),VDF(LELEV),
     &                              VDF(LHSALT))
C--SEAWAT: OBS1RIV6FMVD
                IF (IUNIT(4).NE.0)
     &              CALL OBS1RIV6FMVD(NQ,IX(LCNQOB),IX(LCNQCL),
     &                              IX(LCIQOB),X(LCQCLS),IX(LCIBT),
     &                              MXRIVR,NRIVER,RX(LCRIVR),GZ(LCHNEW),
     &                              NCOL,NROW,NLAY,IOUT,IG(LCIBOU),NH,
     &                              OBSNAM,X(LCH),X(LCTOFF),X(LCWTQ),
     &                              NDMH,ITS,NQAR,NQCAR,NQTAR,NRIVVL,ND,
     &                              VDF(LPS),VDF(LELEV),VDF(LHSALT),
     &                              MTDNCONC,MXSS,NSS,Y(LCSSM),NCOMP,
     &                              Y(LCSSMC))
C--SEAWAT: OBS1GHB6FMVD
CLANGE--SEAWAT: THIS ONE NEEDS TO BE UPDATED TO MODFLOW 1.11
                IF (IUNIT(7).NE.0)
     &              CALL OBS1GHB6FMVD(NQ,IX(LCNQOB),IX(LCNQCL),
     &                              IX(LCIQOB),X(LCQCLS),IX(LCIBT),
     &                              MXBND,NBOUND,RX(LCBNDS),GZ(LCHNEW),
     &                              NCOL,NROW,NLAY,IOUT,IG(LCIBOU),NHT,
     &                              OBSNAM,X(LCH),X(LCTOFF),ITS,NQAR,
     &                              NQCAR,NQTAR,NGHBVL,ND,X(LCWTQ),
     &                              NDMH,VDF(LPS),VDF(LELEV),
     &                              VDF(LHSALT),MTDNCONC,MXSS,NSS,
     &                              Y(LCSSM),NCOMP,Y(LCSSMC))
                IF (IUNIT(18).NE.0)
     &              CALL OBS1STR6FM(NQ,IX(LCNQOB),IX(LCNQCL),
     &                              IX(LCIQOB),X(LCQCLS),IX(LCIBT),
     &                              GZ(LCHNEW),NCOL,NROW,NLAY,IOUT,
     &                              IG(LCIBOU),NHT,OBSNAM,X(LCH),
     &                              X(LCTOFF),MXSTRM,NSTREM,RX(LCSTRM_),
     &                              IR(ICSTRM_),X(LCWTQ),NDMH,ITS,NQAR,
     &                              NQCAR,NQTAR,ND)
C--SEAWAT: OBS1BAS6FMVD
                IF (IUNIT(38).NE.0)
     &              CALL OBS1BAS6FFMVD(NQ,IX(LCNQOB),IX(LCNQCL),
     &                               IX(LCIQOB),X(LCQCLS),IX(LCIBT),
     &                               GZ(LCHNEW),NCOL,NROW,NLAY,
     &                               IG(LCIBOU),NHT,X(LCH),
     &                               X(LCTOFF),ITS,NQAR,NQCAR,NQTAR,
     &                               ICHFLG,GX(LCCR),GX(LCCC),GX(LCCV),
     &                               GX(LCBOTM),NBOTM,LAYHDT,ND,IOUT,
     &                               KKPER,VDF(LPS),VDF(LELEV),
     &                               GX(LCDELR),GX(LCDELC))
                IF (IUNIT(40).NE.0)
     &              CALL OBS1DRT1FM(NQ,IX(LCNQOB),IX(LCNQCL),
     &                              IX(LCIQOB),X(LCQCLS),IX(LCIBT),
     &                              GZ(LCHNEW),NCOL,NROW,NLAY,IOUT,
     &                              IG(LCIBOU),NHT,OBSNAM,X(LCH),
     &                              X(LCTOFF),MXDRT,NDRTCL,RX(LCDRTF),
     &                              X(LCWTQ),NDMH,ITS,NQAR,NQCAR,NQTAR,
     &                              NDRTVL,ND)
              ENDIF
              IF (IUNIT(29).NE.0 .AND. ISSFLG(KKPER).EQ.1)
     &            CALL OBS1ADV2P(NROW,NCOL,NLAY,GX(LCDELC),GX(LCDELR),
     &                           IOUT,GX(LCCR),GX(LCCC),GX(LCCV),
     &                           GZ(LCHNEW),IG(LCIBOU),OBSNAM,X(LCPOFF),
     &                           NHT,NQT,NTT2,NPTH,IX(LCNPNT),KTDIM,
     &                           KTFLG,KTREV,ADVSTP,
     &                           IX(LCICLS),X(LCPRST),NPRST,0,
     &                           GX(LCRMLT),X(LCHK),IG(LCIZON),X(LCH),
     &                           X(LCX),NPE,ND,X(LCTT2),IPRINT,ITERP,
     &                           IOUTT2,MXBND,NBOUND,RX(LCBNDS),NRCHOP,
     &                           IR(LCIRCH),RX(LCRECH),MXSTRM,NSTREM,
     &                           IR(ICSTRM_),RX(LCSTRM_),MXRIVR,NRIVER,
     &                           RX(LCRIVR),MXDRN,NDRAIN,RX(LCDRAI),
     &                           X(LCSV),NMLTAR,NZONAR,GX(LCBOTM),NBOTM,
     &                           RX(LCWELL),NWELVL,MXWELL,NWELLS,
     &                           Z(LCSNEW),X(LCVKA),
     &                           IUNIT(21),RX(LCHFB),MXACTFB,NHFB,
     &                           X(LCHANI),NGHBVL,NRIVVL,NDRNVL,LAYHDT,
     &                           IX(LCLN),NPLIST,ISCALS,FSNK,X(LCWTQ),
     &                           NDMH,X(LCBSCA),X(LCHKCC),X(LCHUFTHK),
     &                           NHUF,IUNIT(23),IUNIT(37),X(LCGS),
     &                           X(LCVDHT),IUNIT(47),X(LCDVDH),NPADV,
     &                           IPFLG,IADVHUF,IADVPER,KKPER)
              CALL OBS1BAS6SS(IOUT,NPE,NH,OBSNAM,KKPER,KKSTP,X(LCBUF1),
     &                        X(LCX),X(LCH),X(LCWT),X(LCHOBS),IPRINT,
     &                        IFO,ITERP,IPAR,NPER,IX(LCLN),LASTX,ISCALS,
     &                        X(LCWP),MPR,X(LCPRM),RSQ,
     &                        RSQP,IPR,IX(LCNIPR),X(LCWTPS),ND,
     &                        X(LCWTQ),X(LCWTQS),IOWTQ,NDMH,NTT2,KTDIM,
     &                        IOSTAR,NPLIST,NSTP,MPRAR,IPRAR,OUTNAM,
     &                        IX(LCIPLO),EQNAM,NAMES,IX(LCIPLP),NDMHAR,
     &                        NQTDR,NQTRV,NQTGB,NQTST,NQTCH,IOWTQCH,
     &                        IOWTQDR,IOWTQRV,IOWTQGB,IOWTQST,LCOBBAS,
     &                        LCOBDRN,LCOBRIV,LCOBGHB,LCOBSTR,LCOBCHD,
     &                        LCOBADV,X(LCSSGF),X(LCSSDR),X(LCSSRV),
     &                        X(LCSSGB),X(LCSSST),X(LCSSAD),X(LCSSCH),
     &                        X(LCSSPI),X(LCSSTO),ITMXP,IOUTG,X(LCBUF2),
     &                        IPES,X(LCBPRI),X(LCBSCA),X(LCRSQA),
     &                        X(LCRSPA),LCOBDRT,X(LCSSDT),NQTDT,IOWTQDT,
     &                        NRSO,NPOST,NNEGT,NRUNS,NQTSF,IOWTQSF,
     &                        LCOBSFR,X(LCSSSF),NHT,X(LCOTIM),OBSALL)
C--SEAWAT: ENDIF FOR STATEMENT THAT SEPARATES VD AND CD OBSERVATION 
C--SEAWAT: PROCESS SUBROUTINES
            ENDIF
	     ENDIF
C-----------SHOW PROGRESS IF REQUESTED
            IF(SHOWPROG)THEN
              IF (KPER.EQ.NPER .AND. KSTP.EQ.NSTP(KKPER)) THEN
                WRITE(*,26)
              ENDIF
   26         FORMAT('+',77(' '))
            ENDIF
C-----------SKIP OVER SENSITIVITY LOOP?
           IF (IPAR.NE.-3) THEN
             IF (IPAR.EQ.-1) GOTO 90
             IF (IFO.EQ.1 .AND. LASTX.EQ.0) GOTO 90
           ELSE
C7C6------IF ITERATION FAILED TO CONVERGE THEN STOP.
             IF (ICNVG.EQ.0) CALL PLL1SD(IERR,IERRU,IOUT,IOUTG)
             GOTO 90
           ENDIF
C
           CALL PLL1AS(NPE)
C
C-----------LOOP THROUGH THE PARAMETERS THAT ARE TO BE ESTIMATED TO
C           CALCULATE SENSITIVITIES
C
           DO 80 KKIP = 1,NPE
             IP = KKIP
C           TO SUBROUTINE?
C             DO THOSE IP ITEMS ASSIGNED TO THIS PROCESSOR
             IF (IPDO(IP).EQ.MYID) THEN
             ELSE
             GOTO 80
             ENDIF
C-----------ASSIGN PARAMETER-APPROPRIATE CONVERGENCE CRITERIA AND OTHER
C           PARAMETER-SPECIFIC SETTINGS
              CALL SEN1BAS6CC(X(LCHCLO),X(LCRCLO),FAC,HCLOSES,IP,NPLIST,
     &                        RCLOSES,IIPP,PIDTMP,NCOL,NROW,NLAY,IUHEAD,
     &                        Z(LCSNEW),X(LCSOLD),XHS,LENXHS)
C-----------PRINT PARAMETER NAME
              CALL UMESPR('SOLVING PARAMETER SENSITIVITY FOR ',
     &                    PARNAM(IIPP),IOUT)
C-------------SHOW PROGRESS IF REQUESTED
              IF(SHOWPROG)THEN
                WRITE(*,58)KPER,KSTP,PARNAM(IIPP)
   58           FORMAT('+Solving:  Stress period: ',i5,4x,
     &                 'Time step: ',i5,4x,a,' Sensitivity')
              ENDIF
C
C7C2----ITERATIVELY FORMULATE AND SOLVE THE SENSITIVITY EQUATIONS.
C
              DO 60 KITER = 1, MXITER
                KKITER = KITER
C       PREPARE TO CALCULATE SENSITIVITY-EQUATION RHS FOR ONE PARAMETER
                CALL SEN1BAS6FM(NCOL,NLAY,NROW,GX(LCRHS))
C-------------CALCULATE MATRIX AND VECTOR DERIVATIVES, MULTIPLY BY
C-------------HEADS, AND ADD COMPONENTS TO RHS
                IF (PIDTMP.EQ.'GHB ')
     &              CALL SEN1GHB6FM(MXBND,RX(LCBNDS),GZ(LCHNEW),NCOL,
     &                              NROW,NLAY,IG(LCIBOU),GX(LCRHS),
     &                              IIPP,NGHBVL)
                IF (PIDTMP.EQ.'DRN ')
     &              CALL SEN1DRN6FM(MXDRN,RX(LCDRAI),GZ(LCHNEW),NCOL,
     &                              NROW,NLAY,IG(LCIBOU),GX(LCRHS),
     &                              IIPP,NDRNVL)
                IF (PIDTMP.EQ.'RIV ')
     &              CALL SEN1RIV6FM(MXRIVR,RX(LCRIVR),GZ(LCHNEW),NCOL,
     &                              NROW,NLAY,IG(LCIBOU),GX(LCRHS),
     &                              IIPP,NRIVVL)
                IF (PIDTMP.EQ.'STR ')
     &              CALL SEN1STR6FM(NSTREM,MXSTRM,RX(LCSTRM_),
     &                              GZ(LCHNEW),NCOL,NROW,NLAY,
     &                              IG(LCIBOU),GX(LCRHS),IR(ICSTRM_),
     &                              IIPP)
                IF (PIDTMP.EQ.'Q   ')
     &              CALL SEN1WEL6FM(NWELLS,MXWELL,RX(LCWELL),NCOL,NROW,
     &                              NLAY,IG(LCIBOU),GX(LCRHS),IIPP,
     &                              NWELVL)
                IF (PIDTMP.EQ.'HK  ' .OR. PIDTMP.EQ.'VK  ' .OR.
     &              PIDTMP.EQ.'VANI' .OR. PIDTMP.EQ.'SS  ' .OR.
     &              PIDTMP.EQ.'SY  ' .OR. PIDTMP.EQ.'VKCB' .OR.
     &              PIDTMP.EQ.'HANI' .OR. PIDTMP.EQ.'LVDA' .OR.
     &              PIDTMP.EQ.'KDEP' .OR. PIDTMP.EQ.'SYTP') THEN
                  IF (IUNIT(23).NE.0)
     &                CALL SEN1LPF1FM(GX(LCRMLT),GZ(LCHNEW),NCOL,NROW,
     &                                NLAY,ISSFLG(KKPER),PIDTMP,X(LCHK),
     &                                GX(LCDELR),GX(LCDELC),IG(LCIBOU),
     &                                DELT,GX(LCRHS),GX(LCHOLD),
     &                                IG(LCIZON),GX(LCCV),X(LCSV),
     &                                NMLTAR,NZONAR,IIPP,GX(LCBOTM),
     &                                NBOTM,X(LCVKA),IUNIT(21),
     &                                RX(LCHFB),MXACTFB,NHFB,X(LCHANI))
                  IF (IUNIT(37).NE.0) THEN
                    IF (IUNIT(47).EQ.0) THEN
                      CALL SEN1HUF2FM(GZ(LCHNEW),NCOL,NROW,NLAY,PIDTMP,
     &                                X(LCHK),X(LCHKCC),GX(LCDELR),
     &                                GX(LCDELC),IG(LCIBOU),GX(LCRHS),
     &                                GX(LCCV),GX(LCBOTM),NBOTM,
     &                                X(LCHUFTHK),NHUF,IIPP,IG(LCIZON),
     &                                NZONAR,GX(LCRMLT),NMLTAR,
     &                                IUNIT(21),RX(LCHFB),MXACTFB,NHFB,
     &                                GX(LCHOLD),DELT,ISSFLG(KKPER),
     &                                IOUT,X(LCGS))
                    ELSE
                      CALL SEN1HUF2VDFM(GZ(LCHNEW),Z(LCSNEW),IG(LCIBOU),
     &                                  X(LCHK),X(LCHKCC),GX(LCCR),
     &                                  GX(LCCC),GX(LCCV),X(LCVDHT),
     &                                  X(LCVDHD),X(LCDVDH),GX(LCRHS),
     &                                  NCOL,NROW,NLAY,GX(LCDELR),
     &                                  GX(LCDELC),X(LCHUFTHK),NHUF,
     &                                  GX(LCBOTM),NBOTM,IIPP,PIDTMP,
     &                                  IG(LCIZON),NZONAR,GX(LCRMLT),
     &                                  NMLTAR,X(LCGS))
                    ENDIF
                  ENDIF
                ENDIF
                IF (IUNIT(23).NE.0)
     &              CALL SEN1LPF1UN(ISSFLG(KKPER),DELT,NCOL,NROW,NLAY,
     &                              X(LCSOLD),GZ(LCHNEW),Z(LCSNEW),
     &                              GX(LCDELR),GX(LCDELC),IG(LCIBOU),
     &                              GX(LCRHS),X(LCSC1),GX(LCCR),
     &                              GX(LCCC),KKITER,X(LCSC2),X(LCHK),
     &                              GX(LCBOTM),NBOTM,GX(LCHOLD),
     &                              GX(LCCV),X(LCHANI),X(LCVKA))
                IF (IUNIT(37).NE.0 .AND. IUNIT(47).EQ.0)
     &              CALL SEN1HUF2UN(ISSFLG(KKPER),DELT,NCOL,NROW,NLAY,
     &                              X(LCSOLD),GZ(LCHNEW),Z(LCSNEW),
     &                              GX(LCDELR),GX(LCDELC),IG(LCIBOU),
     &                              GX(LCRHS),X(LCSC1),GX(LCCR),
     &                              GX(LCCC),KKITER,X(LCHK),X(LCHKCC),
     &                              GX(LCBOTM),NBOTM,GX(LCHOLD),
     &                              GX(LCCV),X(LCHUFTHK),NHUF,
     &                              IG(LCIZON),NZONAR,GX(LCRMLT),
     &                              NMLTAR,X(LCGS),IOUT)
                IF (PIDTMP.EQ.'HFB ')
     &              CALL SEN1HFB6FM(GX(LCBOTM),GX(LCDELC),GX(LCDELR),
     &                              GZ(LCHNEW),RX(LCHFB),IIPP,MXACTFB,
     &                              MXHFB,NBOTM,NCOL,NLAY,NROW,
     &                              GX(LCRHS),LAYHDT,NHFBNP,IG(LCIBOU))
                IF (PIDTMP.EQ.'RCH ')
     &              CALL SEN1RCH6FM(NCOL,NROW,NLAY,GX(LCDELR),
     &                              GX(LCDELC),GX(LCRMLT),NRCHOP,
     &                              IR(LCIRCH),IG(LCIBOU),GX(LCRHS),
     &                              IG(LCIZON),NMLTAR,NZONAR,IIPP)
                IF (PIDTMP.EQ.'EVT ')
     &              CALL SEN1EVT6FM(NCOL,NROW,NLAY,GX(LCDELR),
     &                              GX(LCDELC),GX(LCRMLT),NEVTOP,
     &                              IR(LCIEVT),IG(LCIBOU),GX(LCRHS),
     &                              RX(LCSURF),RX(LCEXDP),GZ(LCHNEW),
     &                              IG(LCIZON),NMLTAR,NZONAR,IIPP)
                IF (PIDTMP.EQ.'CHD ')
     &              CALL SEN1CHD6FM(MXCHD,RX(LCCHDS),Z(LCSNEW),
     &                              PERLEN(KKPER),PERTIM,NCOL,NROW,NLAY,
     &                              NCHDVL,IOUT,IIPP,IERR,IERRU)
                IF (PIDTMP.EQ.'ETS ')
     &              CALL SEN1ETS1FM(NCOL,NROW,NLAY,GX(LCDELR),
     &                              GX(LCDELC),GX(LCRMLT),NETSOP,
     &                              IR(LCIETS),IG(LCIBOU),GX(LCRHS),
     &                              RX(LCETSS),RX(LCETSX),GZ(LCHNEW),
     &                              IG(LCIZON),NMLTAR,NZONAR,IIPP,
     &                              NETSEG,RX(LCPXDP),RX(LCPETM),NSEGAR)
                IF (PIDTMP.EQ.'DRT ')
     &              CALL SEN1DRT1FM(MXDRT,RX(LCDRTF),GZ(LCHNEW),NCOL,
     &                              NROW,NLAY,IG(LCIBOU),GX(LCRHS),
     &                              IIPP,NDRTVL,IDRTFL)
                IF (IERR.GT.0) GOTO 85
C
C-------IF SNEW=SOLD=0 AND RHS=0, NO NEED TO SOLVE.
                CALL UNOITER(GX(LCRHS),Z(LCSNEW),NODES,ISA)
                IF (ISA.EQ.0) THEN
                  ICNVG = 1
                  GOTO 70
                ENDIF
C
C7C2B---MAKE ONE CUT AT AN APPROXIMATE SOLUTION.
                IF (IUNIT(9).GT.0)
     &              CALL SIP5AP(Z(LCSNEW),IG(LCIBOU),GX(LCCR),GX(LCCC),
     &                          GX(LCCV),GX(LCHCOF),GX(LCRHS),X(LCEL),
     &                          X(LCFL),X(LCGL),X(LCV),X(LCW),X(LCHDCG),
     &                          IX(LCLRCH),NPARM,KKITER,HCLOSES,ACCL,
     &                          ICNVG,KKSTP,KKPER,IPCALC,IPRSIP,MXITER,
     &                          NSTP(KKPER),NCOL,NROW,NLAY,NODES,IOUT,3,
     &                          IERR,IERRU)
                IF (IUNIT(10).GT.0)
     &              CALL DE45AP(Z(LCSNEW),IG(LCIBOU),X(LCAU),X(LCAL),
     &                          IX(LCIUPP),IX(LCIEQP),X(LCD4B),MXUP,
     &                          MXLOW,MXEQ,MXBW,GX(LCCR),GX(LCCC),
     &                          GX(LCCV),GX(LCHCOF),GX(LCRHS),ACCL,
     &                          KKITER,ITMX,MXITER,NITER,HCLOSES,IPRD4,
     &                          ICNVG,NCOL,NROW,NLAY,IOUT,IX(LCLRCH),
     &                          X(LCHDCG),0,KKSTP,KKPER,DELT,
     &                          NSTP(KKPER),ID4DIR,ID4DIM,3,IERR,IERRU)
                IF (IUNIT(11).GT.0)
     &              CALL SOR5AP(Z(LCSNEW),IG(LCIBOU),GX(LCCR),GX(LCCC),
     &                          GX(LCCV),GX(LCHCOF),GX(LCRHS),X(LCA),
     &                          X(LCRES),IX(LCIEQP),X(LCHDCG),
     &                          IX(LCLRCH),KKITER,HCLOSES,ACCL,ICNVG,
     &                          KKSTP,KKPER,IPRSOR,MXITER,NSTP(KKPER),
     &                          NCOL,NROW,NLAY,NSLICE,MBW,IOUT,3)
                IF (IUNIT(13).GT.0)
     &              CALL PCG2AP(Z(LCSNEW),IG(LCIBOU),GX(LCCR),GX(LCCC),
     &                          GX(LCCV),GX(LCHCOF),GX(LCRHS),Z(LCV),
     &                          Z(LCSS),Z(LCP),X(LCCD),X(LCHCHG),
     &                          IX(LCLHCH),X(LCRCHG),IX(LCLRCH),KKITER,
     &                          NITER,HCLOSES,RCLOSES,ICNVG,KKSTP,KKPER,
     &                          IPRPCG,MXITER,ITER1,NPCOND,NBPOL,
     &                          NSTP(KKPER),NCOL,NROW,NLAY,NODES,RELAX,
     &                          IOUT,3,IX(LCIT1),DAMP,GX(LCBUFF),
     &                          X(LCHCSV),IERR,IERRU,Z(LCHPCG))
                IF (IUNIT(14).GT.0)
     &            CALL LMG1AP(Z(LCSNEW),IG(LCIBOU),GX(LCCR),GX(LCCC),
     &                        GX(LCCV),GX(LCHCOF),GX(LCRHS),Z(LCA),
     &                        IX(LCIA),IX(LCJA),Z(LCU1),Z(LCFRHS),
     &                        IX(LCIG),ISIZ1,ISIZ2,ISIZ3,ISIZ4,KKITER,
     &                        BCLOSE,DAMP,ICNVG,KKSTP,KKPER,MXITER,
     &                        MXCYC,NCOL,NROW,NLAY,NODES,HNOFLO,IOUT,
     &                        10,ICG,IADAMP,DUP,DLOW)
C
C7C2C---IF CONVERGENCE CRITERION HAS BEEN MET STOP ITERATING.
                IF (ICNVG.EQ.1) GOTO 70
   60         CONTINUE
              KITER = MXITER
C
C7C2C-----IF CONVERGENCE CRITERION HAS NOT BEEN MET . . .
C---------IF ESTIMATING PARAMETERS OR CALCULATING BEALE'S MEASURE, USE
C         THE AVAILABLE VALUES AND KEEP GOING
              IF (IPES.GT.0 .OR. IBEFLG.EQ.2) THEN
                ICNVG = 1
                ICNVGP = 0
              ENDIF
C---------PRINT THE DATA TABLE AND WARNING MESSAGES AND STOP EXCEPT
C         AS NOTED ABOVE
              CALL UNOCONV(X(LCBUF1+IPRAR),OBSNAM,X(LCH),
     &                     X(LCHOBS),IOUTG,IP,IPAR,IPR,KKPER,KKSTP,
     &                     IX(LCLN),MPR,ND,NDMH,NH,IX(LCNIPR),X(LCPRM),
     &                     X(LCBUF1+IPRAR+ND+MPR+IPR),RSQ,
     &                     RSQP,X(LCWP),X(LCWTPS),X(LCWT),X(LCWTQ),
     &                     X(LCWTQS),NPLIST,MPRAR,IPRAR,OUTNAM,
     &                     IX(LCIPLO),EQNAM,NAMES,IX(LCIPLP),NDMHAR,
     &                     NQTDR,NQTRV,NQTGB,NQTST,NQTCH,IOWTQCH,
     &                     IOWTQDR,IOWTQRV,IOWTQGB,IOWTQST,LCOBBAS,
     &                     LCOBDRN,LCOBRIV,LCOBGHB,LCOBSTR,LCOBCHD,
     &                     LCOBADV,X(LCSSGF),X(LCSSDR),X(LCSSRV),
     &                     X(LCSSGB),X(LCSSST),X(LCSSAD),X(LCSSCH),
     &                     X(LCSSPI),X(LCSSTO),ITMXP,IPES,X(LCBPRI),
     &                     ITERP,IERR,IERRU,NTT2,LCOBDRT,X(LCSSDT),
     &                     NQTDT,IOWTQDT,NRSO,NPOST,NNEGT,NRUNS,NQTSF,
     &                     IOWTQSF,LCOBSFR,X(LCSSSF),KTDIM,NHT,
     &                     X(LCOTIM))
              IF (IPAR.NE.1 .AND. IBEFLG.NE.2) THEN
                IERR = 1
                GOTO 85
              ENDIF
C
   70         CONTINUE
C-------------CHECK ACCURACY OF SENSITIVITY CALCULATIONS
              CALL SEN1BAS6CS(Z(LCSNEW),IG(LCIBOU),GX(LCCR),GX(LCCC),
     &                        GX(LCCV),GX(LCHCOF),GX(LCRHS),NCOL,NROW,
     &                        NLAY,IOUT,X(LCSEND),NPE,NTIMES,IP,ITS)
C
C7C5---PRINT AND/OR SAVE SENSITIVITY MATRICES.
              IF (IPAR.EQ.0 .OR. IPAR.EQ.-2) THEN
                CALL SEN1BAS6OT(IHDDFL,IOUT,ISA,KKSTP,IIPP,PIDTMP,
     &                          Z(LCSNEW),GX(LCBUFF),IR(LCIOFL),
     &                          IG(LCIBOU),KKPER,DELT,PERTIM,TOTIM,
     &                          ITMUNI,NCOL,NROW,NLAY,ICNVG,ISENFM,
     &                          ISENPU,ISENSU,CHEDFM,IXSEC,LBHDSV,
     &                          HNOFLO,IP,NPE,IPRINTS,IERR,X(LCBSCA),
     &                          NPLIST,IX(LCLN))
              ENDIF
C-------OBSERVATION CALCULATIONS
              IF (ND.GT.0) THEN
C
C7C6----IF ITERATION FAILED TO CONVERGE THEN STOP.
                IF (ICNVG.EQ.0) THEN
                  IERR = 1
                  GOTO 85
                ENDIF
C-------INTERPOLATE, SAVE AND PRINT SENSITIVITIES FOR OBSERVATIONS.
                IF (IUNIT(28).NE.0 .AND. NH.GT.0)
     &              CALL OBS1BAS6HDR(NH,IX(LCNDER),IX(LCIOFF),
     &                               IX(LCJOFF),IX(LCMLAY),X(LCRINT),
     &                               NCOL,NROW,NLAY,X(LCPR),X(LCWT),
     &                               Z(LCSNEW),X(LCX),IP,NPE,IX(LCLN),
     &                               X(LCTOFF),MAXM,IPAR,NPLIST,ITS,
     &                               NHAR,MOBSAR,ND)
                IF (NQ.GT.0) THEN
                  CALL OBS1BAS6IQ(X(LCQCLS),NQCAR)
                  IF (IUNIT(3).NE.0)
     &                CALL OBS1DRN6DR(NQ,IX(LCNQOB),IX(LCNQCL),
     &                                IX(LCIQOB),X(LCQCLS),IX(LCIBT),
     &                                GZ(LCHNEW),IP,Z(LCSNEW),NCOL,NROW,
     &                                NLAY,IOUTG,IG(LCIBOU),NHT,X(LCX),
     &                                OBSNAM,NPE,IX(LCLN),X(LCTOFF),
     &                                MXDRN,NDRAIN,RX(LCDRAI),NPLIST,
     &                                ITS,NQAR,NQCAR,NQTAR,NDRNVL,IERR,
     &                                IERRU,ND)
                  IF (IUNIT(4).NE.0)
     &                CALL OBS1RIV6DR(NQ,IX(LCNQOB),IX(LCNQCL),
     &                                IX(LCIQOB),X(LCQCLS),IX(LCIBT),
     &                                MXRIVR,NRIVER,RX(LCRIVR),
     &                                GZ(LCHNEW),IP,Z(LCSNEW),NCOL,NROW,
     &                                NLAY,IOUTG,IG(LCIBOU),NH,X(LCX),
     &                                OBSNAM,NPE,IX(LCLN),X(LCTOFF),
     &                                NPLIST,ITS,NQAR,NQCAR,NQTAR,
     &                                NRIVVL,IERR,IERRU,ND)
                  IF (IUNIT(7).NE.0)
     &                CALL OBS1GHB6DR(NQ,IX(LCNQOB),IX(LCNQCL),
     &                                IX(LCIQOB),X(LCQCLS),IX(LCIBT),
     &                                MXBND,NBOUND,RX(LCBNDS),
     &                                GZ(LCHNEW),IP,Z(LCSNEW),NCOL,NROW,
     &                                NLAY,IOUTG,IG(LCIBOU),NHT,X(LCX),
     &                                OBSNAM,NPE,IX(LCLN),X(LCTOFF),
     &                                NPLIST,ITS,NQAR,NQCAR,NQTAR,
     &                                NGHBVL,IERR,IERRU,ND)
                  IF (IUNIT(18).NE.0)
     &                CALL OBS1STR6DR(NQ,IX(LCNQOB),IX(LCNQCL),
     &                                IX(LCIQOB),X(LCQCLS),IX(LCIBT),
     &                                GZ(LCHNEW),IP,Z(LCSNEW),NCOL,NROW,
     &                                NLAY,IOUTG,IG(LCIBOU),NHT,X(LCX),
     &                                OBSNAM,NPE,IX(LCLN),X(LCTOFF),
     &                                MXSTRM,NSTREM,RX(LCSTRM_),
     &                                IR(ICSTRM_),NPLIST,ITS,NQAR,NQCAR,
     &                                NQTAR,IERR,IERRU,ND)
                  IF (IUNIT(38).NE.0)
     &                CALL OBS1BAS6FDR(NQ,IX(LCNQOB),IX(LCNQCL),
     &                                 IX(LCIQOB),X(LCQCLS),IX(LCIBT),
     &                                 GZ(LCHNEW),IP,Z(LCSNEW),NCOL,
     &                                 NROW,NLAY,IG(LCIBOU),NHT,
     &                                 X(LCX),X(LCTOFF),ITS,NQAR,NQCAR,
     &                                 NQTAR,ICHFLG,GX(LCBOTM),NBOTM,
     &                                 PIDTMP,LAYHDT,GX(LCRMLT),NMLTAR,
     &                                 IG(LCIZON),NZONAR,GX(LCDELC),
     &                                 GX(LCDELR),RX(LCHFB),NHFB,
     &                                 IUNIT(21),MXACTFB,X(LCSV),
     &                                 X(LCVKA),X(LCHK),X(LCHANI),
     &                                 GX(LCCR),GX(LCCC),GX(LCCV),NPE,
     &                                 IERR,IERRU,IOUTG,IUNIT(23),
     &                                 IX(LCLN),NPLIST,ND,IUNIT(37),
     &                                 X(LCHKCC),X(LCHUFTHK),NHUF,
     &                                 X(LCGS))
                  IF (IUNIT(40).NE.0)
     &                CALL OBS1DRT1DR(NQ,IX(LCNQOB),IX(LCNQCL),
     &                                IX(LCIQOB),X(LCQCLS),IX(LCIBT),
     &                                GZ(LCHNEW),IP,Z(LCSNEW),NCOL,NROW,
     &                                NLAY,IOUTG,IG(LCIBOU),NHT,X(LCX),
     &                                OBSNAM,NPE,IX(LCLN),X(LCTOFF),
     &                                MXDRT,NDRTCL,RX(LCDRTF),NPLIST,
     &                                ITS,NQAR,NQCAR,NQTAR,NDRTVL,IERR,
     &                                IERRU,ND)
                  IF (IERR.GT.0) GOTO 85
                ENDIF
                IF (IUNIT(29).NE.0 .AND. ISSFLG(KKPER).EQ.1)
     &            CALL OBS1ADV2P(NROW,NCOL,NLAY,GX(LCDELC),GX(LCDELR),
     &                           IOUT,GX(LCCR),GX(LCCC),GX(LCCV),
     &                           GZ(LCHNEW),IG(LCIBOU),OBSNAM,X(LCPOFF),
     &                           NHT,NQT,NTT2,NPTH,IX(LCNPNT),KTDIM,
     &                           KTFLG,KTREV,ADVSTP,
     &                           IX(LCICLS),X(LCPRST),NPRST,IP,
     &                           GX(LCRMLT),X(LCHK),IG(LCIZON),X(LCH),
     &                           X(LCX),NPE,ND,X(LCTT2),IPRINT,ITERP,
     &                           IOUTT2,MXBND,NBOUND,RX(LCBNDS),NRCHOP,
     &                           IR(LCIRCH),RX(LCRECH),MXSTRM,NSTREM,
     &                           IR(ICSTRM_),RX(LCSTRM_),MXRIVR,NRIVER,
     &                           RX(LCRIVR),MXDRN,NDRAIN,RX(LCDRAI),
     &                           X(LCSV),NMLTAR,NZONAR,GX(LCBOTM),NBOTM,
     &                           RX(LCWELL),NWELVL,MXWELL,NWELLS,
     &                           Z(LCSNEW),X(LCVKA),
     &                           IUNIT(21),RX(LCHFB),MXACTFB,NHFB,
     &                           X(LCHANI),NGHBVL,NRIVVL,NDRNVL,LAYHDT,
     &                           IX(LCLN),NPLIST,ISCALS,FSNK,X(LCWTQ),
     &                           NDMH,X(LCBSCA),X(LCHKCC),X(LCHUFTHK),
     &                           NHUF,IUNIT(23),IUNIT(37),X(LCGS),
     &                           X(LCVDHT),IUNIT(47),X(LCDVDH),NPADV,
     &                           IPFLG,IADVHUF,IADVPER,KKPER)
              ENDIF
C-------IF CONVERGENCE ACHIEVED BY SUM OF SQUARES CRITERIA (SOSC)
              IF (IFO.EQ.2) THEN
C-----------GO TO KSTP LOOP WHEN DONE WITH PARAMETERS
                IF (IP.EQ.NPE) THEN
                  ITERPF = ITERP
                  GOTO 90
                ENDIF
              ENDIF
C-------SAVE CURRENT SENSITIVITY ARRAY
              IF (IFO.EQ.0 .OR. LASTX.NE.0)
     &            CALL SEN1BAS6TM(NCOL,NROW,NLAY,IUHEAD,IP,GX(LCBUFF),
     &                            XHS,LENXHS,Z(LCSNEW))
C
C-------END OF SENSITIVITY LOOP
   80       CONTINUE
C
   85       CONTINUE
            CALL PLL1BR()
            CALL PLL1EH(IERR,IERRU,IOUT,IOUTG,MINERR)
C-----END OF TIME STEP (KSTP) AND STRESS PERIOD (KPER) LOOPS
   90     CONTINUE
  100   CONTINUE
C
C-------SHOW PROGRESS IF REQUESTED
        IF(SHOWPROG)THEN
          WRITE(*,57) '+'
   57     FORMAT(A,77(' '))
        ENDIF
C
        CALL PLL1BR()
        IF (ISEN.GT.0) THEN
          CALL PLL1MX(X(LCX),X(LCXND),NPE,ND)
          IF (IFO.NE.1 .OR. LASTX.GT.0)
     &        CALL SEN1BAS6PD(IOUT,NPE,NPER,NSTP,NTIMES,X(LCSEND),
     &                        X(LCSNDT))
        ENDIF
C
C       Post-processing of MNW list output --- Parses to time series for
C       individual wells
        IF (IUNIT(50).GT.0) CALL GWF1MNW1OT(MNWSITE,RX(LCWEL2),NWELL2,
     &                                      MXWEL2,IOWELL2,MNWNAME)
C
C       PRINT DATA FOR OBSERVED HEADS AND FLOWS.
        IF (ND.GT.0 .AND. (IFO.NE.1 .OR. LASTX.NE.0))
     &      CALL OBS1BAS6OT(IOUT,IOUTG,NPE,NH,OBSNAM,X(LCBUF1),X(LCX),
     &                      X(LCH),X(LCWT),X(LCHOBS),IPRINT,IFO,ITERP,
     &                      IPAR,IX(LCLN),ISCALS,X(LCWP),MPR,X(LCPRM),
     &                      RSQ,RSQP,RSQO,RSQOO,SOSC,SOSR,IPR,
     &                      IX(LCNIPR),X(LCWTPS),ND,X(LCWTQ),
     &                      X(LCWTQS),IOWTQ,NDMH,NTT2,KTDIM,NPLIST,
     &                      MPRAR,IPRAR,OUTNAM,IX(LCIPLO),EQNAM,NAMES,
     &                      IX(LCIPLP),NDMHAR,NQTDR,NQTRV,NQTGB,NQTST,
     &                      NQTCH,IOWTQCH,IOWTQDR,IOWTQRV,IOWTQGB,
     &                      IOWTQST,LCOBBAS,LCOBDRN,LCOBRIV,LCOBGHB,
     &                      LCOBSTR,LCOBCHD,LCOBADV,X(LCSSGF),X(LCSSDR),
     &                      X(LCSSRV),X(LCSSGB),X(LCSSST),X(LCSSAD),
     &                      X(LCSSCH),X(LCSSPI),X(LCSSTO),ITMXP,
     &                      X(LCBUF2),IPES,X(LCBPRI),X(LCBSCA),LCOBDRT,
     &                      X(LCSSDT),NQTDT,IOWTQDT,NRSO,NPOST,
     &                      NNEGT,NRUNS,NQTSF,IOWTQSF,LCOBSFR,X(LCSSSF),
     &                      NHT,X(LCOTIM),OBSALL)
C       PARALLEL CONVERGENCE TEST
        CALL PLL1CV(IFO)
C-------IF CONVERGENCE ACHIEVED BY SUM OF SQUARES CRITERIA (SOSC)
        IF (IFO.EQ.2) THEN
          ITERPF = ITERP
        ENDIF
C
C-----NONLINEAR REGRESSION BY MODIFIED GAUSS-NEWTON
        IF (IPES.GT.0) THEN
          IF (IYCFLG.LT.1) THEN
C---------EXECUTE ONE GAUSS-NEWTON ITERATION
            IF (MYID.EQ.MPROC) THEN
              CALL PES1GAU1AP(X(LCX),ND,NPE,X(LCHOBS),X(LCWT),X(LCWP),
     &                        Z(LCC),Z(LCSCLE),Z(LCG),X(LCH),Z(LCDD),
     &                        DMAX,CSA,TOL,IND,IFO,AMP,AP,DMX,IOUTG,
     &                        X(LCB1),ITERP,IPRINT,IX(LCLN),MPR,
     &                        X(LCPRM),JMAX,NFIT,Z(LCR),Z(LCGD),
     &                        Z(LCU),NOPT,X(LCXD),Z(LCS),SOSR,
     &                        IX(LCNIPR),IPR,GX(LCBUFF),X(LCWTP),NHT,
     &                        X(LCWTQ),IOWTQ,NDMH,IOSTAR,NPLIST,MPRAR,
     &                        IPRAR,NDMHAR,X(LCBPRI),RMARM,IAP,
     &                        Z(LCDMXA),IX(LCNPAR),X(LCAMPA),X(LCAMCA),
     &                        X(LCAAP),ITMXP,RMAR,IX(LCIPNG),NPNG,
     &                        NPNGAR)
C-----------FINAL OUTPUT:
C-----------PRINT SIMULATED EQUIVALENTS AND RESIDUALS IF LEAST-SQUARES
C           COEFFICIENT MATRIX IS SINGULAR OR IF PARAMETER ESTIMATION
C           DOES NOT CONVERGE
              IF (IND.GT.0 .OR. (IFO.EQ.0 .AND. KITP.EQ.ITMXP))
     &            CALL OBS1BAS6OH(X(LCWP),IOUT,NH,X(LCH),X(LCHOBS),
     &                            X(LCWT),OBSNAM,ND,MPR,X(LCPRM),RSQ,
     &                            RSQP,2,IX(LCLN),IPR,IX(LCNIPR),
     &                            X(LCWTPS),X(LCBUF1+IPRAR),
     &                            X(LCBUF1+IPRAR+ND+MPR+IPR),X(LCWTQ),
     &                            X(LCWTQS),NDMH,NTT2,KTDIM,NPLIST,
     &                            MPRAR,IPRAR,OUTNAM,IX(LCIPLO),EQNAM,
     &                            NAMES,IX(LCIPLP),NDMHAR,NQTDR,NQTRV,
     &                            NQTGB,NQTST,NQTCH,IOWTQCH,IOWTQDR,
     &                            IOWTQRV,IOWTQGB,IOWTQST,LCOBBAS,
     &                            LCOBDRN,LCOBRIV,LCOBGHB,LCOBSTR,
     &                            LCOBCHD,LCOBADV,0,X(LCSSGF),X(LCSSDR),
     &                            X(LCSSRV),X(LCSSGB),X(LCSSST),
     &                            X(LCSSAD),X(LCSSCH),X(LCSSPI),
     &                            X(LCSSTO),ITMXP,IPES,X(LCBPRI),
     &                            LCOBDRT,X(LCSSDT),NQTDT,IOWTQDT,
     &                            NRSO,NPOST,NNEGT,NRUNS,NQTSF,IOWTQSF,
     &                            LCOBSFR,X(LCSSSF),NHT,X(LCOTIM))
C-------------SHOW PROGRESS IF REQUESTED
              IF(SHOWPROG)THEN
                WRITE(*,'(A)') ' '
              ENDIF
		  ENDIF
          ENDIF
          CALL PLL1BR()
          IF (NUMPROCS.GT.1) THEN
            CALL PLL1CV(IFO)
            CALL PLL1CV(ITERPF)
            CALL PLL1CV(IND)
            CALL PLL1BA(B,MXPAR)
          ENDIF
          IF (IYCFLG.LT.1) THEN
            IF (IFO.GT.0 .AND. IND.EQ.0 .AND. ITERPF.EQ.0) GOTO 20
C
C     IF PARAMETER ESTIMATION DOES NOT CONVERGE, PRINT
C     OBSERVATION-SENSITIVITY TABLE(S)
            IF (ND.GT.0 .AND. ITERP.EQ.ITMXP .AND. IFO.EQ.0)
     &          CALL OBS1BAS6NC(X(LCBUF1),X(LCBUF2),IOUTG,IOWTQ,
     &                          IX(LCIPLO),IPR,ISCALS,ITERP,IX(LCLN),
     &                          MPR,ND,NDMH,NDMHAR,NHT,NPE,NPLIST,
     &                          OBSNAM,OUTNAM,X(LCWT),X(LCWTQ),
     &                          X(LCWTQS),X(LCX),X(LCBSCA),OBSALL)
C
C-----------PRINT FINAL PARAMETER-ESTIMATION OUTPUT
C
C           WRITE CONTRIBUTIONS TO SSWR OF EACH OBSERVATION TYPE AND
C           PRIOR INFORMATION FOR EACH PARAMETER-ESTIMATION ITERATION
C           TO _ss FILE
            IF ((IFO.GT.0 .OR. ITERP.EQ.ITMXP) .AND. OUTNAM.NE.'NONE'
     &          .AND. MYID.EQ.MPROC) THEN
              CALL OBS1BAS6PR1(IFO,IOUTG,ITERPK,ITERSS,ITMXP,IUSS,
     &                         IX(LCNPAR),OUTNAM)
              IF (NH.GT.0) CALL OBS1BAS6HPR(ITERSS,ITMXP,IUSS,X(LCSSGF))
              IF (NQTCH.GT.0) CALL OBS1BAS6FPR(ITERSS,ITMXP,IUSS,
     &                                         X(LCSSCH))
              IF (NQTDR.GT.0) CALL OBS1DRN6PR(ITERSS,ITMXP,IUSS,
     &                                        X(LCSSDR))
              IF (NQTDT.GT.0) CALL OBS1DRT1PR(ITERSS,ITMXP,IUSS,
     &                                        X(LCSSDT))
              IF (NQTRV.GT.0) CALL OBS1RIV6PR(ITERSS,ITMXP,IUSS,
     &                                        X(LCSSRV))
              IF (NQTGB.GT.0) CALL OBS1GHB6PR(ITERSS,ITMXP,IUSS,
     &                                        X(LCSSGB))
              IF (NQTST.GT.0) CALL OBS1STR6PR(ITERSS,ITMXP,IUSS,
     &                                        X(LCSSST))
              IF (NOBADV.GT.0) CALL OBS1ADV2PR(ITERSS,ITMXP,IUSS,
     &                                         X(LCSSAD))
              IF (MPR.GT.0 .OR. IPR.GT.0)
     &            CALL PES1BAS6PR(ITERSS,ITMXP,IUSS,X(LCSSPI))
              CALL OBS1BAS6PR2(IPR,ITERSS,ITMXP,IUSS,MPR,X(LCSSTO))
            ENDIF
C
C           WRITE PARAMETER-ESTIMATION OUTPUT TO GLOBAL FILE
            CALL PES1BAS6OT(Z(LCC),X(LCWT),NPE,RSQ,IOUTG,GX(LCBUFF),ND,
     &                      IPRC,IFO,IND,Z(LCSCLE),X(LCHOBS),X(LCH),
     &                      X(LCB1),X(LCWP),ITERPF,IX(LCLN),MPR,
     &                      X(LCPRM),LPRINT,IDRY,EV,RSQP,VAR,IPR,
     &                      IX(LCNIPR),X(LCWTPS),DETWTP,X(LCBL),X(LCBU),
     &                      Z(LCEIGL),Z(LCEIGV),Z(LCEIGW),NHT,X(LCWTQ),
     &                      X(LCWTQS),DTLWTQ,IOWTQ,NDMH,NPLIST,MPRAR,
     &                      IPRAR,IOUB,IX(LCISEN),IBEALE,ITERP,ITMXP,
     &                      NDMHAR,X(LCPRNT),OUTNAM,X(LCPARE),X(LCSSPI),
     &                      X(LCSSTO),IX(LCNPAR),Z(LCDMXA),X(LCBPRI),
     &                      X(LCBSCA),IPRINT,X(LCAAP),X(LCAMCA),
     &                      X(LCRSQA),X(LCRSPA),X(LCAMPA),ITERPK,OBSALL,
     &                      IUSS)
            IF (IFO.EQ.0 .AND. ITERP.EQ.ITMXP) GOTO 110
C
          ENDIF
        ENDIF
C-------GENERATE INPUT FILE(S) FOR RESAN-2000, BEALE-2000 AND YCINT-2000
        IF (MYID.EQ.MPROC) THEN
          IF (IYCFLG.LT.1 .AND. IPES.GT.0)
     &        CALL PES1BAS6RS(NPE,ND,NDMH,VAR,Z(LCC),X(LCWT),NHT,
     &                        X(LCWTQS),X(LCX),MPR,X(LCPRM),X(LCWP),
     &                        NPLIST,MPRAR,NDMHAR,OUTNAM,X(LCWTPS),
     &                        IPR,IPRAR,IX(LCNIPR),RSQP,IDRY)
          IF (IBEFLG.GT.0 .AND. (IPES.LE.0 .OR. (IPES.GT.0 .AND.
     &        IFO.GT.0)))
     &        CALL PES1BAS6BE(NPE,ND,MPR,VAR,X(LCH),X(LCWT),X(LCX),
     &                        X(LCWP),IX(LCLN),X(LCPRM),X(LCHOBS),
     &                        Z(LCC),IBEALE,ITERPK,IOUT,OBSNAM,
     &                        GX(LCBUFF),NHT,NDMH,X(LCWTQ),NPLIST,MPRAR,
     &                        IBEFLG,OUTNAM,IUBE,BEFIRST,FSTAT,IERR,
     &                        IERRU,NDMHAR,X(LCWTP),IPR,IPRAR,X(LCBPRI),
     &                        IX(LCNIPR))
          IF (IERR.GT.0) GOTO 103
          IF (IYCFLG.GT.-1)
     &        CALL PES1BAS6YC(NPE,ND,MPR,X(LCH),X(LCWT),X(LCX),Z(LCC),
     &                        IOUT,OBSNAM,NHT,NDMH,X(LCWTQ),OUTNAM,
     &                        IYCFLG,IPR,IX(LCIPLO),IERR,IERRU,NDMHAR)
          IF (IERR.GT.0) GOTO 103
        ENDIF
  103   CONTINUE
        CALL PLL1BR()
        CALL PLL1EH(IERR,IERRU,IOUT,IOUTG,MINERR)
        IF (IBEFLG.EQ.2 .AND. IBEALE.NE.0) GOTO 20
        IF (ITERPF.GT.0) GOTO  107
C
C     END OF PARAMETER-ESTIMATION LOOP
  105 CONTINUE
C
  107 CONTINUE
C-------RESIDUAL ANALYSIS
C        OBS1BAS6RE CHANGES H AND MAY CHANGE HOBS
      IF (MYID.EQ.MPROC) THEN
        IF (ND.GT.0)
     &      CALL OBS1BAS6RE(X(LCWP),IOUTG,IOUT,NHT,X(LCH),X(LCHOBS),
     &                      X(LCWT),NDMH,ND,IPAR,MPR,X(LCPRM),IPR,
     &                      IX(LCNIPR),X(LCWTPS),X(LCBUF1),LBUFF,
     &                      X(LCWTQ),X(LCWTQS),NPLIST,MPRAR,IPRAR,
     &                      NDMHAR,NAMES,IX(LCOBSE),X(LCBPRI),RSQP,
     &                      NRSO,NPOST,NNEGT,NRUNS)
C
C       PRINT FINAL PARAMETER-ESTIMATION OUTPUT
        IF (IPES.GT.0 .AND. IYCFLG.LT.1)
     &      CALL PES1BAS6FO(ICNVGP,IFO,IOUTG)
      ENDIF
C
  110 CONTINUE
C     WRITE ANY RECORDS TO BE USED IN RESTARTING FUTURE SIMULATIONS
C       SAVE RESTART RECORDS FOR SUB PACKAGE
      IF(IUNIT(54).GT.0) CALL GWF1SUB1SV(ND2,IDSAVE)
C8------END OF SIMULATION
      CALL GLO1BAS6ET(IBDT,IOUTG,IPRTIM)
      CALL CLOSEFILES(INUNIT,FNAME)
      IF (IBATCH.GT.0) THEN
C       TO USE STATIC MEMORY ALLOCATION, COMMENT OUT THE FOLLOWING
C       DEALLOCATE STATEMENTS
        DEALLOCATE (GX,GZ,IG,X,Z,IX,XHS,RX,IR,NIPRNAM,EQNAM,NAMES,
     &              OBSNAM)
        IF(IUNIT(50).GT.0) DEALLOCATE(MNWSITE)
        GOTO 10
      ENDIF
C
C     HANDLE WARNINGS AND ERRORS
      CALL PLL1BR()
      CALL PLL1EH(IERR,IERRU,IOUT,IOUTG,MINERR)
      IF (MINERR.LT.0) CALL PLL1SD(IERR,IERRU,IOUT,IOUTG)
      CALL PLL1DE(IERRU,IOUT,IOUTG)
C
  120 CONTINUE
C
      CALL PLL1CL()
      WRITE(*,121)
121   FORMAT(1X,'Normal termination of SEAWAT-2000')
      CALL USTOP(' ')
C
      END

