C
      MODULE VDFMODULE
        INTEGER, SAVE, POINTER              ::MT3DRHOFLG
        INTEGER, SAVE, POINTER              ::MFNADVFD
        INTEGER, SAVE, POINTER              ::NSWTCPL
        INTEGER, SAVE, POINTER              ::IWTABLE
        INTEGER, SAVE, POINTER              ::NSRHOEOS
        REAL,    SAVE, POINTER              ::DENSEMIN        
        REAL,    SAVE, POINTER              ::DENSEMAX
        REAL,    SAVE, POINTER              ::DENSEREF
        REAL,    SAVE, POINTER              ::FIRSTDT
        REAL,    SAVE, POINTER              ::DNSCRIT
        REAL,    SAVE, POINTER              ::DRHODPRHD
        REAL,    SAVE, POINTER              ::PRHDREF
        INTEGER, SAVE, POINTER,  DIMENSION(:)                ::MTRHOSPEC
        REAL,    SAVE, POINTER,  DIMENSION(:,:,:)            ::PS
        REAL,    SAVE, POINTER,  DIMENSION(:,:,:)            ::RHOCR
        REAL,    SAVE, POINTER,  DIMENSION(:,:,:)            ::RHOCC
        REAL,    SAVE, POINTER,  DIMENSION(:,:,:)            ::RHOCV
        REAL,    SAVE, POINTER,  DIMENSION(:,:,:)            ::HSALT
        REAL,    SAVE, POINTER,  DIMENSION(:,:,:)            ::ELEV
        REAL,    SAVE, POINTER,  DIMENSION(:,:,:)            ::DCDT
        REAL,    SAVE, POINTER,  DIMENSION(:,:,:,:)          ::COLDFLW
        REAL,    SAVE, POINTER,  DIMENSION(:,:,:)            ::PSOLDITER
        REAL,    SAVE, POINTER,  DIMENSION(:)                ::DRHODC
        REAL,    SAVE, POINTER,  DIMENSION(:)                ::CRHOREF
      TYPE VDFTYPE
        INTEGER, POINTER              ::MT3DRHOFLG
        INTEGER, POINTER              ::MFNADVFD
        INTEGER, POINTER              ::NSWTCPL
        INTEGER, POINTER              ::IWTABLE
        INTEGER, POINTER              ::NSRHOEOS
        REAL,    POINTER              ::DENSEMIN        
        REAL,    POINTER              ::DENSEMAX
        REAL,    POINTER              ::DENSEREF
        REAL,    POINTER              ::FIRSTDT
        REAL,    POINTER              ::DNSCRIT
        REAL,    POINTER              ::DRHODPRHD
        REAL,    POINTER              ::PRHDREF
        INTEGER, POINTER,  DIMENSION(:)                ::MTRHOSPEC
        REAL,    POINTER,  DIMENSION(:,:,:)            ::PS
        REAL,    POINTER,  DIMENSION(:,:,:)            ::RHOCR
        REAL,    POINTER,  DIMENSION(:,:,:)            ::RHOCC
        REAL,    POINTER,  DIMENSION(:,:,:)            ::RHOCV
        REAL,    POINTER,  DIMENSION(:,:,:)            ::HSALT
        REAL,    POINTER,  DIMENSION(:,:,:)            ::ELEV
        REAL,    POINTER,  DIMENSION(:,:,:)            ::DCDT
        REAL,    POINTER,  DIMENSION(:,:,:,:)          ::COLDFLW
        REAL,    POINTER,  DIMENSION(:,:,:)            ::PSOLDITER
        REAL,    POINTER,  DIMENSION(:)                ::DRHODC
        REAL,    POINTER,  DIMENSION(:)                ::CRHOREF
      END TYPE
      TYPE(VDFTYPE),SAVE:: VDFDAT(10)
      END MODULE VDFMODULE
C
      SUBROUTINE VDF1AR(IN,IGRID,IOUT,NCOL,NROW,NLAY,IFREFM,INBTN,NCOMP)
C     ******************************************************************
C     ALLOCATE ARRAYS AND READ DATA FOR VISCOSITY PACKAGE
C     COLDFLW WAS INTRODUCED TO STORE THE CONCENTRATION ARRAY AT THE 
C     TIME OF THE LAST FLOW SOLUTION.  THIS ARRAY IS USED TO DETERMINE
C     HOW MUCH THE DENISTY HAS CHANGED AND IN THE CALCULATION OF DCDT
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
C-----WHEN SWITCHING TO MODFLOW2005 STRUCTURE, UNCOMMENT FOLLOWING LINE
C     USE GLOBAL,         ONLY: IOUT,NCOL,NROW,NLAY
      USE VDFMODULE,   ONLY: MT3DRHOFLG,MFNADVFD,NSWTCPL,IWTABLE,
     1                       NSRHOEOS,DENSEMIN,DENSEMAX,DENSEREF,
     2                       FIRSTDT,DNSCRIT,DRHODPRHD,PRHDREF,
     3                       MTRHOSPEC,PS,RHOCR,RHOCC,RHOCV,HSALT,ELEV,
     4                       DCDT,COLDFLW,PSOLDITER,DRHODC,CRHOREF
      CHARACTER*200 LINE
C
C     ------------------------------------------------------------------      
C1----ALLOCATE AND INITIALIZE VARIABLES (NOTE: NSWTCPL ALLOCATED IN MAIN)
      IF(INBTN.EQ.0) NCOMP=1
      ALLOCATE(MT3DRHOFLG,MFNADVFD,IWTABLE,NSRHOEOS,DENSEMIN,
     1         DENSEMAX,DENSEREF,DNSCRIT,DRHODPRHD,PRHDREF)
      ALLOCATE(PS(NCOL,NROW,NLAY),RHOCR(NCOL,NROW,NLAY),
     1         RHOCC(NCOL,NROW,NLAY),RHOCV(NCOL,NROW,NLAY),
     2         HSALT(NCOL,NROW,NLAY),ELEV(NCOL,NROW,NLAY),
     3         DCDT(NCOL,NROW,NLAY),COLDFLW(NCOL,NROW,NLAY,NCOMP),
     4         PSOLDITER(NCOL,NROW,NLAY))
C
C1-----INITIALIZE VARIABLES
      DRHODPRHD=0.0
      PRHDREF=0.0
C
C2-----IDENTIFY PACKAGE
      WRITE(IOUT,1) IN
    1 FORMAT(1X,/1X,'VDF -- VARIABLE DENSITY FLOW, VERSION 1',
     1', 2/13/2004',/,9X,'INPUT READ FROM UNIT',I3,/)
C
C3----READ INPUT VARIABLES FROM VDF INPUT FILE
      CALL URDCOM(IN,IOUT,LINE)
      LLOC = 1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MT3DRHOFLG,DUM,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MFNADVFD,DUM,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NSWTCPL,DUM,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IWTABLE,DUM,IOUT,IN)
C
C4----READ DENSITY LIMITERS
      IF(IFREFM.EQ.0) THEN
        READ(IN,'(2F10.0)') DENSEMIN,DENSEMAX
      ELSEIF(IFREFM.NE.0) THEN
        READ(IN,*) DENSEMIN,DENSEMAX
      ENDIF
C
C5----SEAWAT: READ DNSCRIT
      IF((NSWTCPL.GT.1.OR.NSWTCPL.EQ.-1).AND.IFREFM.EQ.0) THEN
        READ(IN,'(F10.0)') DNSCRIT
      ELSEIF((NSWTCPL.GT.1.OR.NSWTCPL.EQ.-1).AND.IFREFM.NE.0) THEN
        READ(IN,*) DNSCRIT
      ENDIF
C-----SEAWAT: RESET DNSCRIT TO DEFAULT IF NOT ENTERED
      IF (DNSCRIT.EQ.0.) DNSCRIT=0.0
C
C6----SEAWAT: PRINT IMPLICIT COUPLING INFO TO IOUT
      IF (NSWTCPL.EQ.-1) WRITE(IOUT,556) DNSCRIT
      IF (NSWTCPL.EQ.0) NSWTCPL=1
	IF (NSWTCPL.GT.1) THEN
	 WRITE(IOUT,558) NSWTCPL,DNSCRIT
	ELSE
	 WRITE(IOUT,559)
	ENDIF
  556 FORMAT(1X,'FLOW FIELD UPDATE OPTION IS ACTIVE',
     &       /1X,'FLOW FIELD WILL BE RECALCULATED ONLY IF',
     &       /1X,'MAXIMUM DENSITY CHANGE IS GREATER THAN ',G10.4)
  558 FORMAT(1X,'COUPLING BETWEEN FLOW AND TRANSPORT IS IMPLICIT',
     &       /1X,G10.4,' COUPLING ITERATIONS',
     &       /1X,G10.4,' IS THE DENSITY CONVERGENCE CRITERIA')
  559 FORMAT(1X,'COUPLING BETWEEN FLOW AND TRANSPORT IS EXPLICIT')
C
C7----WRITE INFO ABOUT WATER-TABLE CORRECTION
      IF (IWTABLE.EQ.0) WRITE(IOUT,600)
  600 FORMAT(1X,'VARIABLE-DENSITY WATER-TABLE CORRECTIONS NOT ADDED')
      IF (IWTABLE.GT.0) WRITE(IOUT,610)
  610 FORMAT(1X,'VARIABLE-DENSITY WATER-TABLE CORRECTIONS ARE ADDED')
C
C8----PRINT MT3DRHOFLG INFO TO OUTPUT FILE
      IF(MT3DRHOFLG.EQ.0) THEN
        WRITE(IOUT,500)
      ELSE
        WRITE(IOUT,510) MT3DRHOFLG
      ENDIF
  500 FORMAT(1X,'FLUID DENSITY IS SPECIFIED BY USER IN THE VDF FILE')
  510 FORMAT(1X,'MT3DMS SPECIES USED IN EQUATION OF STATE FOR FLUID DENS
     &ITY: ',I10)
C
C7----PRINT MFNADVFD INFO TO OUTPUT FILE
      IF(MFNADVFD.EQ.2) THEN
        WRITE(IOUT,553)
      ELSE
        WRITE(IOUT,557)
      ENDIF
  553 FORMAT(1X,'A CENTRAL-IN-SPACE-WEIGHTED ALGORITHM IS USED TO CALCUT
     &E FLUID DENSITY TERMS THAT CONSERVE MASS')
  557 FORMAT(1X,'AN UPSTREAM-WEIGHTED ALGORITHM IS USED TO CALCULATE FLU
     &ID DENSITY TERMS THAT CONSERVE MASS')
C
C9----READ DENSITY EQUATION OF STATE INFORMATION USING SIMPLIFIED METHOD
      IF(MT3DRHOFLG.GE.0) THEN
        NSRHOEOS=1
        ALLOCATE(MTRHOSPEC(NSRHOEOS),DRHODC(NSRHOEOS),CRHOREF(NSRHOEOS))
        IF(MT3DRHOFLG.GT.0) THEN
            MTRHOSPEC(1)=MT3DRHOFLG
        ELSE
            MTRHOSPEC(1)=1
        ENDIF
        CRHOREF(1)=0.
        IF(IFREFM.EQ.0) THEN
            READ(IN,'(2F10.0)') DENSEREF,DRHODC(1)
        ELSE
            READ(IN,*) DENSEREF,DRHODC(1)
        ENDIF
        WRITE(IOUT,560) DENSEREF,DRHODC(1)
  560 FORMAT(1X,G10.4,' REFERENCE DENSITY',
     &       /1X,G10.4,' DENSITY SLOPE FOR EQUATION OF STATE')
      ENDIF
C
C10---READ DENSITY EQUATION OF STATE INFORMATION USING DETAILED METHOD
      IF(MT3DRHOFLG.EQ.-1) THEN
        IF(IFREFM.EQ.0) THEN
            READ(IN,'(3F10.0)') DENSEREF,DRHODPRHD,PRHDREF
            READ(IN,'(I10)') NSRHOEOS
        ELSE
            READ(IN,*) DENSEREF,DRHODPRHD,PRHDREF
            READ(IN,*) NSRHOEOS
        ENDIF
        ALLOCATE(MTRHOSPEC(NSRHOEOS),DRHODC(NSRHOEOS),CRHOREF(NSRHOEOS))
        DO I=1,NSRHOEOS
            IF(IFREFM.EQ.0) THEN
                READ(IN,'(I10,2F10.0)') MTRHOSPEC(I),DRHODC(I),
     &                                  CRHOREF(I)
            ELSE
                READ(IN,*) MTRHOSPEC(I),DRHODC(I),CRHOREF(I)
            ENDIF
        ENDDO
      ENDIF
C
C11---ECHO DENSITY INFORMATION TO OUTPUT FILE
      WRITE(IOUT,2)
    2 FORMAT(1X,'DENSITY EQUATION OF STATE')
      WRITE(IOUT,3) DENSEREF
    3 FORMAT(1X,'RHO = ',G15.6)
      DO I=1,NSRHOEOS
        WRITE(IOUT,4) DRHODC(I),I,CRHOREF(I)
      ENDDO
    4 FORMAT(1X,' + ',G10.4,' * ( CONC(',I4,') - ',G10.4,' )')
      IF(DRHODPRHD.NE.0) WRITE(IOUT,5) DRHODPRHD,PRHDREF
    5 FORMAT(1X,' + ',G10.4,' * ( PRESS HEAD - ',G10.4,' )')
      WRITE(IOUT,'(//)')
C
C12---READ FIRSTDT, AND WRITE TO IOUT
      IF(IFREFM.EQ.0) THEN
        READ(IN,'(F10.0)') FIRSTDT
      ELSEIF(IFREFM.NE.0) THEN
        READ(IN,*) FIRSTDT
      ENDIF      
      WRITE(IOUT,590) FIRSTDT
  590 FORMAT(1X,'FIRSTDT SPECIFIED BY USER IN THE VDF FILE IS: ',
     &       G15.7)
C
C7----RESET POINTERS      
      CALL SVDF1PSV(IGRID)
C      
      RETURN
      END
C
C
      SUBROUTINE VDF1DF(IN,IOUT,IFREFM,INBTN,
     &                  INSEN,NCOMP,MXSS,NSSVL,ISUMY,ISUMIY,LCCNEW,
     &                  LCSSM,LCSSMC,LTCRCH,LTCEVT,NCOL,NROW,NLAY)
C **********************************************************************
CVDF THIS SUBROUTINE DEFINES THE VARIABLE-DENSITY FLOW PROCESS
C **********************************************************************
CVDF CREATED 11/20/01 FOR MF2K VDF PROCESS
C
      USE VDFMODULE,   ONLY: MT3DRHOFLG
C----------------------------------------------------------------------
C
C--SEAWAT: ALLOCATE SPACE FOR MT3D ARRAYS EVEN IF NOT USED
	NSSVL=7
      IF(INBTN.EQ.0) THEN
		NCOMP=1
		MXSS=1
		NSSVL=1
		ISUMY=1
		ISUMIY=1
		LCCNEW=ISUMY
		ISUMY=ISUMY+NCOL*NROW*NLAY*NCOMP
		LCSSM=ISUMY
		ISUMY=ISUMY*NSSVL*MXSS
		LCSSMC=ISUMY
		ISUMY=ISUMY+NCOMP*MXSS
		LTCRCH=ISUMY
		ISUMY=ISUMY+NCOL*NROW*NCOMP
		LTCEVT=ISUMY
		ISUMY=ISUMY+NCOL*NROW*NCOMP
          WRITE(IOUT,1100) ISUMY,ISUMIY
 1100 FORMAT(1X,I8,' DUMMY MT3DMS ELEMENTS USED FOR Y ARRAY ',
     &       1X,I8,' DUMMY MT3DMS ELEMENTS USED FOR IY ARRAY'/)
      ENDIF
C
      IF(INSEN.GT.0) THEN
        PRINT*, 'THE VDF PROCESS IS NOT COMPATIBLE WITH THE SENSITIVITY'
        PRINT*, 'PROCESS. INACTIVATE EITHER ONE OF THESE TWO PROCESSES'
        STOP
      ENDIF
C
C--SWT: STOP PROGRAM IN THIS CIRCUMSTANCE
      IF(MT3DRHOFLG.GT.0.AND.INBTN.EQ.0) THEN
        WRITE(IOUT,580)
        STOP
      ENDIF
  580 FORMAT(1X,'ERROR: MT3DRHOFLG GREATER THAN ZERO, BUT MT3DMS NOT USE
     &D')
C
      RETURN
      END
C
C
      SUBROUTINE VDF1IZ(IOUT,NCOL,NROW,NLAY,IBOUND,BOTM,NBOTM,HNEW,
     &                  MIXELM)
C     ******************************************************************
C     INITIALIZE ELEVATION ARRAY
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
C-----WHEN SWITCHING TO MODFLOW2005 STRUCTURE, UNCOMMENT FOLLOWING LINE
C     USE GLOBAL,         ONLY: IOUT,NCOL,NROW,NLAY
      USE VDFMODULE,   ONLY: MT3DRHOFLG,NSWTCPL,HSALT,ELEV,DCDT
C
      DIMENSION IBOUND(NCOL,NROW,NLAY),BOTM(NCOL,NROW,0:NBOTM),
     &          HNEW(NCOL,NROW,NLAY)
C
C     ------------------------------------------------------------------
C
C       INITIALIZE ELEVATION ARRAY
C       WILL NOT WORK IF QUASI-3D LAYER INCLUDED IN MODEL
      DO K=1,NLAY
      DO I=1,NROW
      DO J=1,NCOL
c         IF(IBOUND(J,I,K).EQ.0) CYCLE
                ELEV(J,I,K)=(BOTM(J,I,K-1)-BOTM(J,I,K))/2+BOTM(J,I,K)
      ENDDO
      ENDDO
      ENDDO
C
C--SEAWAT: SET HSALT EQUAL TO HNEW (AT THIS POINT, HNEW AND HSALT ARE STARTING HEADS)
      HSALT=HNEW
C
C--SEAWAT: INITIALIZE DCDT TO ZERO
      DCDT=0.
C
C
C-----PERFORM CHECK FOR SEAWAT'S ITERATIVE COUPLING SCHEME
      IF (MT3DRHOFLG.NE.0.AND.NSWTCPL.GT.1.AND.MIXELM.GT.0) THEN
          WRITE(IOUT,*) 'MIXELM MUST BE LESS THAN OR EQUAL TO ZERO TO US
     +E ITERATIVE FLOW-TRANSPORT COUPLING'
          WRITE(*,*)    'MIXELM MUST BE LESS THAN OR EQUAL TO ZERO TO US
     +E ITERATIVE FLOW-TRANSPORT COUPLING'
          STOP
      ENDIF
      IF(NSWTCPL.EQ.0) NSWTCPL=1
C
      RETURN
      END     
C
C
      SUBROUTINE VDF1RPSS(IOUT,IN,NCOL,NROW,NLAY,HNEW,IFREFM,CNEW,
     &                      NCOMP,CINACT,IBOUND,BOTM,KKPER)
C***********************************************************************
C--SEAWAT: INITIALIZE DENSITY AND CALCULATE HNEW AS FRESHWATER HEAD
C--SEAWAT:  THIS SUBROUTINE CALLED ONLY ONCE AT BEGINNING OF SIMULATION
C--SEAWAT:  CANNOT BE DONE WITHIN 
C      CREATED 11/20/01
C      THE USER IS ALLOWED TWO DIFFERENT OPTIONS FOR
C      TREATING FLUID DENSITY.
C          1.  CALCULATED FROM CONCENTRATION
C          2.  SPECIFIED IN VDF INPUT FILE
C***********************************************************************
      USE VDFMODULE,   ONLY: MT3DRHOFLG,DENSEREF,PS,HSALT,ELEV,
     &                       MTRHOSPEC
C
      DIMENSION HNEW(NCOL,NROW,NLAY),CNEW(NCOL,NROW,NLAY,NCOMP),
     &          IBOUND(NCOL,NROW,NLAY) 
      CHARACTER*24 ANAME(4)
      DATA ANAME(1) /'     DENSITY LAYER INDEX'/
      DATA ANAME(2) /'           FLUID DENSITY'/
	DATA ANAME(3) /'           CONCENTRATION'/
C-----------------------------------------------------------------------
C     POPULATE FLUID DENSITY ARRAY (PS)
      IF(MT3DRHOFLG.EQ.0) THEN
		IF(IFREFM.EQ.0) THEN
			READ(IN,'(I10)') INDENSE
		ELSE
			READ(IN,*) INDENSE
		ENDIF
		WRITE(IOUT,'(//)')
		WRITE(IOUT,500) INDENSE
		IF(INDENSE.LT.0) WRITE(IOUT,510)
		IF(INDENSE.EQ.0) WRITE(IOUT,520)
		IF(INDENSE.GT.0) WRITE(IOUT,530)
		IF(INDENSE.EQ.2) WRITE(IOUT,540)
C		IF INDENSE LESS THAN ZERO, SET TO DENSEREF
		IF (INDENSE.LT.0.AND.KKPER.EQ.1) THEN
			DO K=1,NLAY
			DO I=1,NROW
			DO J=1,NCOL
				IF(IBOUND(J,I,K).NE.0)
     &				PS(J,I,K)=DENSEREF
			ENDDO
			ENDDO
			ENDDO
		ENDIF
C		IF INDENSE EQUAL TO ZERO, SET TO DENSEREF
		IF (INDENSE.EQ.0) THEN
			DO K=1,NLAY
			DO I=1,NROW
			DO J=1,NCOL
				IF(IBOUND(J,I,K).NE.0)
     &				PS(J,I,K)=DENSEREF
			ENDDO
			ENDDO
			ENDDO
		ENDIF
C		IF INDENSE GREATER THAN ZERO, THEN READ DENSE ARRAY
		ITEMP=2
		IF(INDENSE.EQ.2) ITEMP=3
		IF(INDENSE.GT.0) THEN
			DO K=1,NLAY
				CALL U2DREL(PS(1,1,K),ANAME(ITEMP),NROW,NCOL,K,IN,
     +                        IOUT)
			ENDDO
		ENDIF
C		IF INDENSE EQUAL TO 2, THEN CONVERT DENSITY ARRAY USING EQUATION OF STATE
		IF (INDENSE.EQ.2) THEN
			DO K=1,NLAY
			DO I=1,NROW
			DO J=1,NCOL
				IF(IBOUND(J,I,K).NE.0)
     &				PS(J,I,K)=CALCDENS(J,I,K,PS(J,I,K))
			ENDDO
			ENDDO
			ENDDO
		ENDIF
      ELSE
C         SET DENSITY ARRAY USING CONCENTRATIONS FROM MT3D IF FIRST STRESS PERIOD
          IF(KKPER.EQ.1) THEN
            PS=DENSEREF
            DO K=1,NLAY
            DO I=1,NROW
            DO J=1,NCOL
             IF(CNEW(J,I,K,MTRHOSPEC(1)).NE.CINACT) 
     &           PS(J,I,K)=CALCDENS(J,I,K,CNEW(J,I,K,1:NCOMP))
            ENDDO
            ENDDO
            ENDDO
          ENDIF
      ENDIF
C
C     IF FIRST STRESS PERIOD, CALCULATE HNEW
      IF(KKPER.EQ.1) THEN
        DO K=1,NLAY
        DO I=1,NROW
        DO J=1,NCOL
            IF(IBOUND(J,I,K).EQ.0) CYCLE
            HNEW(J,I,K)=FEHEAD(HSALT(J,I,K),PS(J,I,K),ELEV(J,I,K))
        ENDDO
        ENDDO
        ENDDO
      ENDIF
  500 FORMAT(1X,'INDENSE VALUE SPECIFIED AS:',I4)
  510 FORMAT(1X,'VALUES FOR DENSE ARRAY WILL BE REUSED OR SET TO DENSERE
     +F')
  520 FORMAT(1X,'VALUES FOR THE DENSE ARRAY WILL BE SET TO DENSEREF')
  530 FORMAT(1X,'VALUES FOR THE DENSE ARRAY WILL BE READ FROM VDF FILE')
  540 FORMAT(1X,'VALUES READ AS CONCENTRATION WILL BE CONVERTED TO FLUID
     + DENSITY USING EQUATION OF STATE')
      RETURN
      END
C
C
      SUBROUTINE VDF1FM(NCOL,NROW,NLAY,CR,CC,CV,IBOUND,RHS,
     &                 HCOF,HNEW,DELR,DELC,BOTM,NBOTM,
     &                 IUNITBCF,IUNITLPF,IUNITHUF,IOUT)
******************************************************************************
C--SEAWAT: CALCULATE DENSITY TERMS THAT ARE SUBTRACTED FROM THE RHS ACCUMULATOR
C--SEAWAT: CALCULATE RHOCR, RHOCC, AND RHOCV
C--SEAWAT: CALCULATE WATER TABLE CORRECTIONS
******************************************************************************  
      USE VDFMODULE,   ONLY: MT3DRHOFLG,MFNADVFD,IWTABLE,DENSEREF,PS,
     &                       RHOCR,RHOCC,RHOCV,HSALT,ELEV,DCDT
C
      DIMENSION CR(NCOL,NROW,NLAY),CC(NCOL,NROW,NLAY),
     &          CV(NCOL,NROW,NLAY),IBOUND(NCOL,NROW,NLAY),
     &          RHS(NCOL,NROW,NLAY),DELR(NCOL),
     &          DELC(NROW),BOTM(NCOL,NROW,0:NBOTM),
     &          HCOF(NCOL,NROW,NLAY)
      DOUBLE PRECISION HNEW(NCOL,NROW,NLAY),
     &                 H1,HS1,H2,HS2
      LOGICAL CORRECT
      COMMON /DISCOM/LBOTM(999),LAYCBD(999)
      COMMON /BCFCOM/LAYCON(999)
      COMMON /LPFCOM/LAYTYP(999),LAYAVG(999),CHANI(999),LAYVKA(999),
     1               LAYWET(999)
      COMMON /HUFCOM/HGUHANI(999),HGUVANI(999),LTHUF(999),LAYWT(999)
C-----------------------------------------------------------------------------
      DO K=1,NLAY
      DO I=1,NROW
      DO J=1,NCOL
         D1=0.
         D2=0.
         D3=0.
         D4=0.
         D5=0.
         D6=0.
         IF(IBOUND(J,I,K).LE.0) GOTO 70
C--SEAWAT: CALCULATE DENSITY TERM LEFT (J-1,I,K): D1
         IF(J.EQ.1) GOTO 10
         IF(IBOUND(J-1,I,K).EQ.0) GOTO 10
            H1=HNEW(J,I,K)
            HS1=HSALT(J,I,K)
            H2=HNEW(J-1,I,K)
            HS2=HSALT(J-1,I,K)
            Z1=ELEV(J,I,K)
            Z2=ELEV(J-1,I,K)
            PS1=PS(J,I,K)
            PS2=PS(J-1,I,K)
            TOP1=BOTM(J,I,K-1)
            TOP2=BOTM(J-1,I,K-1)
            BOT1=BOTM(J,I,K)
            BOT2=BOTM(J-1,I,K)
C--SEAWAT: WATER TABLE CORRECTIONS
C--SEAWAT: WTCORR-BCF
            IF(IWTABLE.EQ.1) THEN
              IF(IUNITBCF.GT.0.AND.LAYCON(K).NE.0)
     &         CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                    H2,HS2,PS2,Z2,TOP2,BOT2)
C--SEAWAT: WTCORR-LPF
              IF(IUNITLPF.GT.0.AND.LAYTYP(K).NE.0)
     &         CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                   H2,HS2,PS2,Z2,TOP2,BOT2)
C--SEAWAT: WTCORR-HUF
              IF(IUNITHUF.GT.0.AND.LTHUF(K).NE.0)
     &         CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                   H2,HS2,PS2,Z2,TOP2,BOT2)
             ENDIF
C--SEAWAT: END WATER TABLE CORRECTION PROGRAMMING
            DIS1=DELR(J-1)/2
            DIS2=DELR(J)/2
            AVGDENS=(DIS1*PS(J-1,I,K)+DIS2*PS(J,I,K))/(DIS1+DIS2)
            D1=CR(J-1,I,K)*(AVGDENS-DENSEREF)/DENSEREF*
     &       (Z2-Z1)
C--SEAWAT: CONSERVE MASS WITH WEIGHTED CENTRAL INSPACE
            IF(MFNADVFD.EQ.2) THEN   
               D1=D1*AVGDENS
            ELSE    
C--SEAWAT: CONSERVE MASS WITH UPSTREAM VALUES
C               HDIFF=HNEW(J-1,I,K)-HNEW(J,I,K)
               HDIFF=H2-H1
               FLOWDIR=CR(J-1,I,K)*HDIFF+D1
               IF (FLOWDIR.GT.0.) THEN
                   D1=D1*PS(J-1,I,K)
               ELSE
                   D1=D1*PS(J,I,K)
               ENDIF
            ENDIF
C--SEAWAT: CALCULATE DENSITY TERM RIGHT (J+1,I,K): D2
   10       IF(J.EQ.NCOL) GOTO 20
            IF(IBOUND(J+1,I,K).EQ.0) GOTO 20
            H1=HNEW(J,I,K)
            HS1=HSALT(J,I,K)
            H2=HNEW(J+1,I,K)
            HS2=HSALT(J+1,I,K)
            Z1=ELEV(J,I,K)
            Z2=ELEV(J+1,I,K)
            PS1=PS(J,I,K)
            PS2=PS(J+1,I,K)
            TOP1=BOTM(J,I,K-1)
            TOP2=BOTM(J+1,I,K-1)
            BOT1=BOTM(J,I,K)
            BOT2=BOTM(J+1,I,K)
C--SEAWAT: WATER TABLE CORRECTIONS
C--SEAWAT: WTCORR-BCF
            IF(IWTABLE.EQ.1) THEN
              IF(IUNITBCF.GT.0.AND.LAYCON(K).NE.0)
     &         CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                    H2,HS2,PS2,Z2,TOP2,BOT2)
C--SEAWAT: WTCORR-LPF
              IF(IUNITLPF.GT.0.AND.LAYTYP(K).NE.0)
     &         CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                   H2,HS2,PS2,Z2,TOP2,BOT2)
C--SEAWAT: WTCORR-HUF
              IF(IUNITHUF.GT.0.AND.LTHUF(K).NE.0)
     &          CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                   H2,HS2,PS2,Z2,TOP2,BOT2)
            ENDIF
C--SEAWAT: END WATER TABLE CORRECTION PROGRAMMING
            DIS1=DELR(J+1)/2
            DIS2=DELR(J)/2
            AVGDENS=(DIS1*PS(J+1,I,K)+DIS2*PS(J,I,K))/(DIS1+DIS2)
            D2=CR(J,I,K)*(AVGDENS-DENSEREF)/DENSEREF*
     &       (Z2-Z1)
C--SEAWAT: CONSERVE MASS WITH WEIGHTED CENTRAL INSPACE
            IF(MFNADVFD.EQ.2) THEN
               D2=D2*AVGDENS
            ELSE
C--SEAWAT: CONSERVE MASS WITH UPSTREAM VALUES
C              HDIFF=HNEW(J+1,I,K)-HNEW(J,I,K)
               HDIFF=H2-H1
               FLOWDIR=CR(J,I,K)*HDIFF+D2
             IF (FLOWDIR.GT.0.) THEN
               D2=D2*PS(J+1,I,K)
             ELSE
               D2=D2*PS(J,I,K)
             ENDIF
            ENDIF
C--SEAWAT: CALCULATE DENSITY TERM BACK (J,I-1,K): D3
   20       IF(I.EQ.1) GOTO 30
            IF(IBOUND(J,I-1,K).EQ.0) GOTO 30
            H1=HNEW(J,I,K)
            HS1=HSALT(J,I,K)
            H2=HNEW(J,I-1,K)
            HS2=HSALT(J,I-1,K)
            Z1=ELEV(J,I,K)
            Z2=ELEV(J,I-1,K)
            PS1=PS(J,I,K)
            PS2=PS(J,I-1,K)
            TOP1=BOTM(J,I,K-1)
            TOP2=BOTM(J,I-1,K-1)
            BOT1=BOTM(J,I,K)
            BOT2=BOTM(J,I-1,K)
C--SEAWAT: WATER TABLE CORRECTIONS
            IF(IWTABLE.EQ.1) THEN
C--SEAWAT: WTCORR-BCF
              IF(IUNITBCF.GT.0.AND.LAYCON(K).NE.0)
     &         CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                    H2,HS2,PS2,Z2,TOP2,BOT2)
C--SEAWAT: WTCORR-LPF
              IF(IUNITLPF.GT.0.AND.LAYTYP(K).NE.0)
     &         CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                   H2,HS2,PS2,Z2,TOP2,BOT2)
C--SEAWAT: WTCORR-HUF
              IF(IUNITHUF.GT.0.AND.LTHUF(K).NE.0)
     &         CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                   H2,HS2,PS2,Z2,TOP2,BOT2)
             ENDIF
C--SEAWAT: END WATER TABLE CORRECTION PROGRAMMING
            DIS1=DELC(I-1)/2
            DIS2=DELC(I)/2
            AVGDENS=(DIS1*PS(J,I-1,K)+DIS2*PS(J,I,K))/(DIS1+DIS2)
            D3=CC(J,I-1,K)*(AVGDENS-DENSEREF)/DENSEREF*
     &       (Z2-Z1)
C--SEAWAT: CONSERVE MASS WITH WEIGHTED CENTRAL INSPACE
            IF(MFNADVFD.EQ.2) THEN 
               D3=D3*AVGDENS
            ELSE
C--SEAWAT: CONSERVE MASS WITH UPSTREAM DENSITY
C              HDIFF=HNEW(J,I-1,K)-HNEW(J,I,K)
               HDIFF=H2-H1
               FLOWDIR=CC(J,I-1,K)*HDIFF+D3
             IF (FLOWDIR.GT.0.) THEN
               D3=D3*PS(J,I-1,K)
             ELSE
               D3=D3*PS(J,I,K)
             ENDIF
            ENDIF

C--SEAWAT: CALCULATE DENSITY TERM FRONT (J,I+1,K): D4
   30       IF(I.EQ.NROW) GOTO 40
            IF(IBOUND(J,I+1,K).EQ.0) GOTO 40
             H1=HNEW(J,I,K)
             HS1=HSALT(J,I,K)
             H2=HNEW(J,I+1,K)
             HS2=HSALT(J,I+1,K)
             Z1=ELEV(J,I,K)
             Z2=ELEV(J,I+1,K)
             PS1=PS(J,I,K)
             PS2=PS(J,I+1,K)
             TOP1=BOTM(J,I,K-1)
             TOP2=BOTM(J,I+1,K-1)
             BOT1=BOTM(J,I,K)
             BOT2=BOTM(J,I+1,K)
C--SEAWAT: WATER TABLE CORRECTIONS
            IF(IWTABLE.EQ.1) THEN
C--SEAWAT: WTCORR-BCF
              IF(IUNITBCF.GT.0.AND.LAYCON(K).NE.0)
     &          CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                    H2,HS2,PS2,Z2,TOP2,BOT2)
C--SEAWAT: WTCORR-LPF
              IF(IUNITLPF.GT.0.AND.LAYTYP(K).NE.0)
     &          CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                   H2,HS2,PS2,Z2,TOP2,BOT2)
C--SEAWAT: WTCORR-HUF
              IF(IUNITHUF.GT.0.AND.LTHUF(K).NE.0)
     &          CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                   H2,HS2,PS2,Z2,TOP2,BOT2)
             ENDIF
C--SEAWAT: END WATER TABLE CORRECTION PROGRAMMING
            DIS1=DELC(I+1)/2
            DIS2=DELC(I)/2
            AVGDENS=(DIS1*PS(J,I+1,K)+DIS2*PS(J,I,K))/(DIS1+DIS2)
            D4=CC(J,I,K)*(AVGDENS-DENSEREF)/DENSEREF*
     &       (Z2-Z1)
C--SEAWAT: CONSERVE MASS WITH WEIGHTED CENTRAL IN SPACE
            IF(MFNADVFD.EQ.2) THEN 
               D4=D4*AVGDENS
            ELSE
C--SEAWAT: CONSERVE MASS WITH UPSTREAM DENSITY
C                HDIFF=HNEW(J,I+1,K)-HNEW(J,I,K)
               HDIFF=H2-H1
               FLOWDIR=CC(J,I,K)*HDIFF+D4
               IF (FLOWDIR.GT.0.) THEN
                  D4=D4*PS(J,I+1,K)
               ELSE
                  D4=D4*PS(J,I,K)
               ENDIF
            ENDIF

C--SEAWAT: CALCULATE DENSITY TERM UP (J,I,K-1): D5
   40       IF(K.EQ.1) GOTO 50
            IF(IBOUND(J,I,K-1).EQ.0) GOTO 50
            DIS1=ELEV(J,I,K-1)-BOTM(J,I,K-1)
            DIS2=BOTM(J,I,K-1)-ELEV(J,I,K)
            AVGDENS=(DIS1*PS(J,I,K-1)+DIS2*PS(J,I,K))/(DIS1+DIS2)
            D5=CV(J,I,K-1)*(AVGDENS-DENSEREF)/DENSEREF*
     &       (ELEV(J,I,K-1)-ELEV(J,I,K))
C--SEAWAT: CONSERVE MASS WITH CENTRAL IN SPACE
            IF(MFNADVFD.EQ.2) THEN 
               D5=D5*AVGDENS
            ELSE
C--SEAWAT: CONSERVE MASS WITH UPSTREAM DENSITY
              HDIFF=HNEW(J,I,K-1)-HNEW(J,I,K)
              FLOWDIR=CV(J,I,K-1)*HDIFF+D5
C--SEAWAT: CHECK AND CORRECT FOR DEWATERED CASE
              IF(IUNITBCF.GT.0) THEN
                IFLG=0
                IF(LAYCON(K).EQ.3 .OR. LAYCON(K).EQ.2) IFLG=1
              ENDIF
              IF(IUNITLPF.GT.0) IFGL=LAYTYP(K)
              IF(IUNITHUF.GT.0) IFLG=LTHUF(K)
	        IF(IFGL.GT.0) THEN
	          HS2=SALTHEAD(HNEW(J,I,K),PS(J,I,K),ELEV(J,I,K))
	          BOT1=BOTM(J,I,LBOTM(K)-1)
	          IF(HS2.LT.BOT1) THEN
	            HS1=SALTHEAD(HNEW(J,I,K-1),PS(J,I,K-1),ELEV(J,I,K-1))
	            FLOWDIR=PS(J,I,K-1)/DENSEREF*CV(J,I,K-1)*(HS1-BOT1)
	          ENDIF
	        ENDIF
C--SEAWAT: END DEWATERED CORRECTION
              IF (FLOWDIR.GT.0.) THEN
                 D5=D5*PS(J,I,K-1)
              ELSE
                 D5=D5*PS(J,I,K)
			ENDIF
            ENDIF
C
C--SEAWAT: CALCULATE DENSITY TERM DOWN (J,I,K+1): D6
   50       IF(K.EQ.NLAY) GOTO 60
            IF(IBOUND(J,I,K+1).EQ.0) GOTO 60
            DIS1=BOTM(J,I,K)-ELEV(J,I,K+1)
            DIS2=ELEV(J,I,K)-BOTM(J,I,K)
            AVGDENS=(DIS1*PS(J,I,K+1)+DIS2*PS(J,I,K))/(DIS1+DIS2)
            D6=CV(J,I,K)*(AVGDENS-DENSEREF)/DENSEREF*
     &       (ELEV(J,I,K+1)-ELEV(J,I,K))
C--SEAWAT: CONSERVE MASS WITH CENTRAL IN SPACE
            IF(MFNADVFD.EQ.2) THEN 
              D6=D6*AVGDENS
            ELSE
C--SEAWAT: CONSERVE MASS WITH UPSTREAM DENSITY
              HDIFF=HNEW(J,I,K+1)-HNEW(J,I,K)
              FLOWDIR=CV(J,I,K)*HDIFF+D6
C--CHECK AND CORRECT FOR DEWATERED CASE
              IF(IUNITBCF.GT.0) THEN
                IFLG=0
                IF(LAYCON(K+1).EQ.3 .OR. LAYCON(K+1).EQ.2) IFLG=1
              ENDIF
              IF(IUNITLPF.GT.0) IFGL=LAYTYP(K+1)
              IF(IUNITHUF.GT.0) IFLG=LTHUF(K+1)
	        IF(IFGL.GT.0) THEN
	          HS2=SALTHEAD(HNEW(J,I,K+1),PS(J,I,K+1),ELEV(J,I,K+1))
	          BOT1=BOTM(J,I,LBOTM(K+1)-1)
	          IF(HS2.LT.BOT1) THEN
	            HS1=SALTHEAD(HNEW(J,I,K),PS(J,I,K),ELEV(J,I,K))
	            FLOWDIR=PS(J,I,K)/DENSEREF*CV(J,I,K)*(BOT1-HS1)
	          ENDIF
	        ENDIF
C--SEAWAT: END DEWATERED CORRECTION
             IF (FLOWDIR.GT.0.) THEN
                D6=D6*PS(J,I,K+1)
             ELSE
                D6=D6*PS(J,I,K)
             ENDIF
            ENDIF
   60      CONTINUE
C--SEAWAT: SUBTRACT DENSITY TERMS AND DCDT FROM RHS ACCUMULATOR
           RHS(J,I,K)=RHS(J,I,K)-D1-D2-D3-D4-D5-D6
           IF(MT3DRHOFLG.NE.0) RHS(J,I,K)=RHS(J,I,K)+DCDT(J,I,K)       
   70 CONTINUE
      ENDDO
      ENDDO
      ENDDO
C--SEAWAT:**********CALCULATE MASS CONDUCTANCES*************************
C--SEAWAT: CALCULATE RHOCR (PASSED TO SOLVER TO CONSERVE MASS)
      DO K=1,NLAY
      DO I=1,NROW
      DO J=1,NCOL-1
       IF(IBOUND(J,I,K).NE.0) RHOCR(J,I,K)=CR(J,I,K)*PS(J,I,K)
	 IF(IBOUND(J,I,K).NE.0.AND.IBOUND(J+1,I,K).NE.0) THEN
        H1=HNEW(J,I,K)
        HS1=HSALT(J,I,K)
        H2=HNEW(J+1,I,K)
        HS2=HSALT(J+1,I,K)
        Z1=ELEV(J,I,K)
        Z2=ELEV(J+1,I,K)
        PS1=PS(J,I,K)
        PS2=PS(J+1,I,K)
        TOP1=BOTM(J,I,K-1)
        TOP2=BOTM(J+1,I,K-1)
        BOT1=BOTM(J,I,K)
        BOT2=BOTM(J+1,I,K)
C--SEAWAT WATER TABLE CORRECTIONS
        IF(IWTABLE.EQ.1) THEN
C--SEAWAT: WTCORR-BCF
          IF(IUNITBCF.GT.0.AND.LAYCON(K).NE.0)
     &      CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                    H2,HS2,PS2,Z2,TOP2,BOT2)
C--SEAWAT: WTCORR-LPF
          IF(IUNITLPF.GT.0.AND.LAYTYP(K).NE.0)
     &      CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                   H2,HS2,PS2,Z2,TOP2,BOT2)
C--SEAWAT: WTCORR-HUF
          IF(IUNITHUF.GT.0.AND.LTHUF(K).NE.0)
     &      CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                   H2,HS2,PS2,Z2,TOP2,BOT2)
        ENDIF
C--SEAWAT: END WATER TABLE CORRECTION PROGRAMMING
        DIS1=DELR(J+1)/2
        DIS2=DELR(J)/2
        AVGDENS=(DIS1*PS(J+1,I,K)+DIS2*PS(J,I,K))/(DIS1+DIS2)
        D2=CR(J,I,K)*(AVGDENS-DENSEREF)/DENSEREF*(Z2-Z1)
        HDIFF=H2-H1
        FLOWDIR=CR(J,I,K)*HDIFF+D2
        IF(MFNADVFD.EQ.2) THEN 
           RHOCR(J,I,K)=CR(J,I,K)*AVGDENS
        ELSE
         IF (FLOWDIR.GT.0.) THEN
           RHOCR(J,I,K)=CR(J,I,K)*PS(J+1,I,K)
         ELSE
           RHOCR(J,I,K)=CR(J,I,K)*PS(J,I,K)
         ENDIF
        ENDIF
       ENDIF
      ENDDO
      ENDDO
      ENDDO
C       CALCULATE RHOCC
      DO K=1,NLAY
      DO I=1,NROW-1
      DO J=1,NCOL
	 IF(IBOUND(J,I,K).NE.0) RHOCC(J,I,K)=CC(J,I,K)*PS(J,I,K)
       IF(IBOUND(J,I,K).NE.0.AND.IBOUND(J,I+1,K).NE.0) THEN
        H1=HNEW(J,I,K)
        HS1=HSALT(J,I,K)
        H2=HNEW(J,I+1,K)
        HS2=HSALT(J,I+1,K)
        Z1=ELEV(J,I,K)
        Z2=ELEV(J,I+1,K)
        PS1=PS(J,I,K)
        PS2=PS(J,I+1,K)
        TOP1=BOTM(J,I,K-1)
        TOP2=BOTM(J,I+1,K-1)
        BOT1=BOTM(J,I,K)
        BOT2=BOTM(J,I+1,K)
C--SEAWAT WATER TABLE CORRECTIONS
        IF(IWTABLE.EQ.1) THEN
C--SEAWAT: WTCORR-BCF
          IF(IUNITBCF.GT.0.AND.LAYCON(K).NE.0)
     &      CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                    H2,HS2,PS2,Z2,TOP2,BOT2)
C--SEAWAT: WTCORR-LPF
          IF(IUNITLPF.GT.0.AND.LAYTYP(K).NE.0)
     &      CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                   H2,HS2,PS2,Z2,TOP2,BOT2)
C--SEAWAT: WTCORR-HUF
          IF(IUNITHUF.GT.0.AND.LTHUF(K).NE.0)
     &      CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                   H2,HS2,PS2,Z2,TOP2,BOT2)
        ENDIF
C--SEAWAT: END WATER TABLE CORRECTION PROGRAMMING
          DIS1=DELC(I+1)/2
          DIS2=DELC(I)/2
          AVGDENS=(DIS1*PS(J,I+1,K)+DIS2*PS(J,I,K))/(DIS1+DIS2)
          D4=CC(J,I,K)*(AVGDENS-DENSEREF)/DENSEREF*(Z2-Z1)
          HDIFF=H2-H1
          FLOWDIR=CC(J,I,K)*HDIFF+D4
          IF(MFNADVFD.EQ.2) THEN 
            RHOCC(J,I,K)=CC(J,I,K)*AVGDENS
          ELSE
            IF (FLOWDIR.GT.0.) THEN
               RHOCC(J,I,K)=CC(J,I,K)*PS(J,I+1,K)
            ELSE
               RHOCC(J,I,K)=CC(J,I,K)*PS(J,I,K)
            ENDIF
          ENDIF
       ENDIF
      ENDDO
      ENDDO
      ENDDO
                
C       CALCULATE RHOCV
      DO K=1,NLAY-1
      DO I=1,NROW
      DO J=1,NCOL
	  IF(IBOUND(J,I,K).NE.0) RHOCV(J,I,K)=CV(J,I,K)*PS(J,I,K)
        IF(IBOUND(J,I,K).NE.0.AND.IBOUND(J,I,K+1).NE.0) THEN
          DIS1=BOTM(J,I,K)-ELEV(J,I,K+1)
          DIS2=ELEV(J,I,K)-BOTM(J,I,K)
          AVGDENS=(DIS1*PS(J,I,K+1)+DIS2*PS(J,I,K))/(DIS1+DIS2)
          D6=CV(J,I,K)*(AVGDENS-DENSEREF)/DENSEREF*
     &       (ELEV(J,I,K+1)-ELEV(J,I,K))
          HDIFF=HNEW(J,I,K+1)-HNEW(J,I,K)
          FLOWDIR=CV(J,I,K)*HDIFF+D6
C--CHECK AND CORRECT FOR DEWATERED CASE
          IF(IUNITBCF.GT.0) THEN
            IFLG=0
            IF(LAYCON(K+1).EQ.3 .OR. LAYCON(K+1).EQ.2) IFLG=1
          ENDIF
          IF(IUNITLPF.GT.0) IFGL=LAYTYP(K+1)
          IF(IUNITHUF.GT.0) IFLG=LTHUF(K+1)
	    IF(IFGL.GT.0) THEN
	      HS2=SALTHEAD(HNEW(J,I,K+1),PS(J,I,K+1),ELEV(J,I,K+1))
	      BOT1=BOTM(J,I,LBOTM(K+1)-1)
	      IF(HS2.LT.BOT1) THEN
	        HS1=SALTHEAD(HNEW(J,I,K),PS(J,I,K),ELEV(J,I,K))
	        FLOWDIR=PS(J,I,K)/DENSEREF*CV(J,I,K)*(BOT1-HS1)
	      ENDIF
	    ENDIF
C--SEAWAT: END DEWATERED CORRECTION
          IF(MFNADVFD.EQ.2) THEN
            RHOCV(J,I,K)=CV(J,I,K)*AVGDENS
          ELSE
           IF (FLOWDIR.GT.0.) THEN
            RHOCV(J,I,K)=CV(J,I,K)*PS(J,I,K+1)
           ELSE
            RHOCV(J,I,K)=CV(J,I,K)*PS(J,I,K)
           ENDIF
          ENDIF
         ENDIF
      ENDDO
      ENDDO
      ENDDO
C--SEAWAT:MAKE CORRECTIONS FOR WATER-TABLE CASE
      IF (IWTABLE.EQ.0) GOTO 110
        DO K=1,NLAY
        DO I=1,NROW
        DO J=1,NCOL
          IF(LAYCON(K).NE.0.OR.LAYTYP(K).NE.0.OR.LTHUF(K).NE.0) THEN 
                C1=0.
                C2=0.
                C3=0.
                C4=0.
                B1=0.
                B2=0.
                B3=0.
                B4=0.
                IF (IBOUND(J,I,K).LE.0) GOTO 90
                ZWT2I=(HSALT(J,I,K)+BOTM(J,I,K))/2
C       CORRECT FOR FLOW TO LEFT
                IF(J.EQ.1) GOTO 85
                IF(IBOUND(J-1,I,K).EQ.0) GOTO 85
                CORRECT=.FALSE.
                IF(HSALT(J,I,K).LT.BOTM(J,I,K-1)) CORRECT=.TRUE.
                IF(HSALT(J-1,I,K).LT.BOTM(J-1,I,K-1)) CORRECT=.TRUE.
                IF(.NOT.CORRECT) GOTO 85
                AVGDENS=0.5*(PS(J-1,I,K)+PS(J,I,K))
                B1=CR(J-1,I,K)*(AVGDENS-DENSEREF)/DENSEREF*
     &             (ELEV(J-1,I,K)-ELEV(J,I,K))
                HDIFF=HNEW(J-1,I,K)-HNEW(J,I,K)
                ZWT2NEXT=(HSALT(J-1,I,K)+BOTM(J-1,I,K))/2

                C1=CR(J-1,I,K)*((AVGDENS-DENSEREF)/DENSEREF*
     &          (ELEV(J-1,I,K)-ELEV(J,I,K)+
     &        ZWT2I-ZWT2NEXT)+(PS(J-1,I,K)-DENSEREF)/DENSEREF*(ZWT2NEXT-
     &         ELEV(J-1,I,K))+(PS(J,I,K)-DENSEREF)/DENSEREF*
     &         (ELEV(J,I,K)-ZWT2I))
                FLOWDIR=(CR(J-1,I,K)*HDIFF+B1)-C1
                IF (FLOWDIR.GT.0.) THEN
                  C1=C1*PS(J-1,I,K)
                ELSE
                  C1=C1*PS(J,I,K)
                ENDIF
   85     CONTINUE
C       CORRECT FOR FLOW TO RIGHT
                IF(J.EQ.NCOL) GOTO 86
                IF(IBOUND(J+1,I,K).EQ.0) GOTO 86
                CORRECT=.FALSE.
                IF(HSALT(J,I,K).LT.BOTM(J,I,K-1)) CORRECT=.TRUE.
                IF(HSALT(J+1,I,K).LT.BOTM(J+1,I,K-1)) CORRECT=.TRUE.
                IF(.NOT.CORRECT) GOTO 86
                AVGDENS=0.5*(PS(J+1,I,K)+PS(J,I,K))
                B2=CR(J,I,K)*(AVGDENS-DENSEREF)/DENSEREF*
     &           (ELEV(J+1,I,K)-ELEV(J,I,K))
                HDIFF=HNEW(J+1,I,K)-HNEW(J,I,K)
                ZWT2NEXT=(HSALT(J+1,I,K)+BOTM(J+1,I,K))/2

                C2=CR(J,I,K)*((AVGDENS-DENSEREF)/DENSEREF*
     &       (ELEV(J+1,I,K)-ELEV(J,I,K)+ZWT2I-ZWT2NEXT)+
     &       (PS(J+1,I,K)-DENSEREF)/DENSEREF*(ZWT2NEXT-
     &       ELEV(J+1,I,K))+(PS(J,I,K)-DENSEREF)/DENSEREF*
     &       (ELEV(J,I,K)-ZWT2I))

                FLOWDIR=(CR(J,I,K)*HDIFF+B2)-C2
                IF (FLOWDIR.GT.0.) THEN
                   C2=C2*PS(J+1,I,K)
                ELSE
                   C2=C2*PS(J,I,K)
                ENDIF
   86     CONTINUE
C       CORRECT FOR FLOW TO BACK
                IF(I.EQ.1) GOTO 87
                IF(IBOUND(J,I-1,K).EQ.0) GOTO 87
                CORRECT=.FALSE.
                IF(HSALT(J,I,K).LT.BOTM(J,I,K-1)) CORRECT=.TRUE.
                IF(HSALT(J,I-1,K).LT.BOTM(J,I-1,K-1)) CORRECT=.TRUE.
                IF(.NOT.CORRECT) GOTO 87
                AVGDENS=0.5*(PS(J,I-1,K)+PS(J,I,K))
                B3=CC(J,I-1,K)*(AVGDENS-DENSEREF)/DENSEREF*
     &           (ELEV(J,I-1,K)-ELEV(J,I,K))
                HDIFF=HNEW(J,I-1,K)-HNEW(J,I,K)
                ZWT2NEXT=(HSALT(J,I-1,K)+BOTM(J,I-1,K))/2
                C3=CC(J,I-1,K)*((AVGDENS-DENSEREF)/DENSEREF*
     &          (ELEV(J,I-1,K)-ELEV(J,I,K)+ZWT2I-ZWT2NEXT)+
     &          (PS(J,I-1,K)-DENSEREF)/DENSEREF*(ZWT2NEXT-
     &          ELEV(J,I-1,K))+(PS(J,I,K)-DENSEREF)/DENSEREF*
     &          (ELEV(J,I,K)-ZWT2I))
                FLOWDIR=(CC(J,I-1,K)*HDIFF+B3)-C3
                IF (FLOWDIR.GT.0.) THEN
                  C3=C3*PS(J,I-1,K)
                ELSE
                  C3=C3*PS(J,I,K)
                ENDIF
   87           CONTINUE

C       CORRECT FOR FLOW TO FRONT
                IF(I.EQ.NROW) GOTO 90
                IF(IBOUND(J,I+1,K).EQ.0) GOTO 90
                CORRECT=.FALSE.
                IF(HSALT(J,I,K).LT.BOTM(J,I,K-1)) CORRECT=.TRUE.
                IF(HSALT(J,I+1,K).LT.BOTM(J,I+1,K-1)) CORRECT=.TRUE.
                IF(.NOT.CORRECT) GOTO 90
                AVGDENS=0.5*(PS(J,I+1,K)+PS(J,I,K))
                B4=CC(J,I,K)*(AVGDENS-DENSEREF)/DENSEREF*
     &          (ELEV(J,I+1,K)-ELEV(J,I,K))
                HDIFF=HNEW(J,I+1,K)-HNEW(J,I,K)
                ZWT2NEXT=(HSALT(J,I+1,K)+BOTM(J,I+1,K))/2
                C4=CC(J,I,K)*((AVGDENS-DENSEREF)/DENSEREF*
     &          (ELEV(J,I+1,K)-ELEV(J,I,K)+ZWT2I-ZWT2NEXT)+
     &          (PS(J,I+1,K)-DENSEREF)/DENSEREF*(ZWT2NEXT-
     &          ELEV(J,I+1,K))+(PS(J,I,K)-DENSEREF)/DENSEREF*
     &          (ELEV(J,I,K)-ZWT2I))
                FLOWDIR=(CC(J,I,K)*HDIFF+B4)-C4
                IF (FLOWDIR.GT.0.) THEN
                  C4=C4*PS(J,I+1,K)
                ELSE
                  C4=C4*PS(J,I,K)
                ENDIF
   90           CONTINUE
                RHS(J,I,K)=RHS(J,I,K)+C1+C2+C3+C4
C--SEAWAT: END IF FOR WT CORRECTIONS
       ENDIF
      ENDDO
      ENDDO
      ENDDO  
  110 CONTINUE
      RETURN
      END


	SUBROUTINE VDF1HSALT(NCOL,NROW,NLAY,HNEW,IBOUND)
C **********************************************************************
C THIS SUBROUTINE UPDATES THE HSALT ARRAY
C 
C***********************************************************************
C CREATED 3/12/03
C
      USE VDFMODULE,   ONLY: HSALT,PS,ELEV
	DIMENSION HNEW(NCOL,NROW,NLAY),IBOUND(NCOL,NROW,NLAY)
      DO K=1,NLAY
      DO I=1,NROW
      DO J=1,NCOL
		IF(IBOUND(J,I,K).NE.0) THEN
C			CALCULATE NATIVE HEAD FOR CONSTANT HEADS AND ACTIVE CELLS
      		HSALT(J,I,K)=SALTHEAD(HNEW(J,I,K),PS(J,I,K),ELEV(J,I,K))
		ELSE
C			THIS WILL PUT HDRY INTO A CELL THAT WENT DRY
			HSALT(J,I,K)=HNEW(J,I,K)
		ENDIF
      ENDDO
      ENDDO
      ENDDO
	RETURN
	END	


      SUBROUTINE VDF1PS(NCOL,NROW,NLAY,NCOMP,CNEW,CINACT,IOUT)
C **********************************************************************
C THIS SUBROUTINE CALCULATES THE PS ARRAY
C 
C***********************************************************************
C CREATED 7/21/99
C
      USE VDFMODULE,   ONLY: MT3DRHOFLG,DENSEREF,PS,MTRHOSPEC,NSRHOEOS,
     &                       CRHOREF
C       IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION CNEW(NCOL,NROW,NLAY,NCOMP)
C-----------------------------------------------------------------------
C
      IF(MT3DRHOFLG.EQ.0) RETURN
      DMIN=DENSEREF
C
C-------IF INACTIVE CONCENTRATION CELL, SET DENSE USING A CONCENTRATION OF ZERO
C-------ELSE SET DENSITY USING MT3DMS SPECIES
      DO K=1,NLAY
      DO I=1,NROW
      DO J=1,NCOL
        IF(CNEW(J,I,K,MTRHOSPEC(1)).EQ.CINACT) THEN
            PS(J,I,K)=CALCDENS(J,I,K,CRHOREF)
        ELSE
            PS(J,I,K)=CALCDENS(J,I,K,CNEW(J,I,K,1:NCOMP))
        ENDIF
C-------CHECK FOR DENSITIES LESS THAN DENSEREF
        IF (PS(J,I,K).LT.DMIN) THEN
           DMIN=PS(J,I,K)
           JMIN=J
           IMIN=I
           KMIN=K
        ENDIF
      ENDDO
      ENDDO
      ENDDO
C
C-----WRITE 
      IF (DMIN.LT.DENSEREF) WRITE(IOUT,100) DMIN,IMIN,JMIN,KMIN
  100   FORMAT(1H0,'WARNING: NUMERICAL OSCILLATION ENCOUNTERED'/
     &        1X,'ONE OR MORE CELL CONCENTRATIONS LESS THAN CRHOREF'/
     &        1X,'SMALLEST DENSITY = ',G10.3/
     &        1X,'AT CELL (I,J,K) = (',I4,',',I4,',',I4,')'/)
      RETURN
      END
C
C
      SUBROUTINE VDF1DC(NCOL,NROW,NLAY,NCOMP,COLD,CNEW,PRSITY,
     &  DELR,DELC,DH,DELT,IBOUND,CINACT,IUPDTFLWFLD)
C **********************************************************************
C THIS SUBROUTINE CALCULATES THE DCDT ARRAY
C 
C***********************************************************************
C CREATED 2/14/2002 CDL
C
      USE VDFMODULE,   ONLY: MT3DRHOFLG,PS,COLDFLW,DCDT,MTRHOSPEC
C
      DIMENSION COLD(NCOL,NROW,NLAY,NCOMP),CNEW(NCOL,NROW,NLAY,NCOMP),
     &  PRSITY(NCOL,NROW,NLAY),DELR(NCOL),DELC(NROW),
     &  DH(NCOL,NROW,NLAY),IBOUND(NCOL,NROW,NLAY)
C
C--SEAWAT: DCDT EQUALS ZERO IF CONCENTRATIONS DO NOT CHANGE
	  DCDT=0.
C
C-----DENSITIES SPECIFIED BY USER, SO RETURN
      IF(MT3DRHOFLG.EQ.0) RETURN
C
C-----CALCULATE DCDT USING NEW CONCENTRATION AND DENSITY FROM LAST FLOW SOLUTION
      DO K=1,NLAY
      DO I=1,NROW
      DO J=1,NCOL
	   IF(IBOUND(J,I,K).LE.0) CYCLE
         IF(CNEW(J,I,K,MTRHOSPEC(1)).EQ.CINACT.OR.
     &      COLDFLW(J,I,K,MTRHOSPEC(1)).EQ.CINACT) CYCLE
         DENSENEW=CALCDENS(J,I,K,CNEW(J,I,K,1:NCOMP))
         DENSEOLD=CALCDENS(J,I,K,COLDFLW(J,I,K,1:NCOMP))
         DCDT(J,I,K)=DCDT(J,I,K)+PRSITY(J,I,K)*
     &               (DENSENEW-DENSEOLD)/
     &               DELT*DELR(J)*DELC(I)*DH(J,I,K)
      ENDDO
      ENDDO
      ENDDO
      RETURN
      END
C
C
      SUBROUTINE VDF1BD(VBNM,VBVL,MSUM,IBOUND,DELT,NCOL,NROW,NLAY,
     &  KSTP,KPER,IBCFCB,ILPFCB,IHUFCB,ICBCFL,BUFF,IOUT,PERTIM,
     &  TOTIM)
C-----VERSION  9OCT1999 SWTBD
C     ******************************************************************
C     SUM THE DCDT TERMS FOR BUDGET
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
C       IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      USE VDFMODULE,   ONLY: MT3DRHOFLG,DENSEREF,PS,DCDT
C
      CHARACTER*4 VBNM
      CHARACTER*16 TEXT
      DIMENSION IBOUND(NCOL,NROW,NLAY),VBNM(4,20),VBVL(4,20),
     +  BUFF(NCOL,NROW,NLAY)
      DATA TEXT /'            DCDT'/
C     ------------------------------------------------------------------
C1--INITIALIZE BUDGET ACCUMULATORS
        ZERO=0.
        DCDTIN=ZERO
        DCDTOUT=ZERO
C
C--SEAWAT: EXIT IF MT3D NOT USED (DCDT EQUAL TO ZERO)
      IF (MT3DRHOFLG.EQ.0) GOTO 500

C2--IF CELL-BY-CELL FLOWS ARE NEEDED THEN SET FLAG IBD
        IBD=0

C3--IF CELL-BY-CELL FLOWS ARE NEEDED (IBD IS SET) CLEAR BUFFER
        IF(IBCFCB.GT.0) IBD=ICBCFL
        IF(ILPFCB.GT.0) IBD=ICBCFL
        IF(IHUFCB.GT.0) IBD=ICBCFL


C4--------CLEAR BUFFER
        DO 210 K=1,NLAY
        DO 210 I=1,NROW
        DO 210 J=1,NCOL
        BUFF(J,I,K)=0.
  210 CONTINUE

C4--LOOP THROUGH EVERY CELL IN GRID
        do 400 K=1,NLAY
        do 400 I=1,NROW
        do 400 J=1,NCOL
C			  SKIP NO-FLOW AND CONSTANT HEAD CELLS
                IF (IBOUND(J,I,K).LE.0) GOTO 400
c               switch sign of dcdt for mass balance calculation
                DCDTVOL=-1*DCDT(J,I,K)*DELT
                IF(IBD.EQ.1) BUFF(J,I,K)=DCDTVOL/PS(J,I,K)
                IF(DCDTVOL) 392,400,394
  392     DCDTOUT=DCDTOUT-DCDTVOL
          GOTO 400
  394     DCDTIN=DCDTIN+DCDTVOL
  400 CONTINUE  

C5--IF IBD FLAG IS SET RECORD THE CONTENTS OF THE BUFFER
	ITEMP=0
	IF (IBCFCB.GT.0) ITEMP=IBCFCB
	IF (ILPFCB.GT.0) ITEMP=ILPFCB
	IF (IHUFCB.GT.0) ITEMP=IHUFCB
      IF(ITEMP.GT.0) THEN
        IF(IBD.EQ.1) CALL UBUDSV(KSTP,KPER,TEXT,ITEMP,BUFF,NCOL,NROW,
     1                           NLAY,IOUT)
        IF(IBD.EQ.2) CALL UBDSV1(KSTP,KPER,TEXT,ITEMP,BUFF,NCOL,NROW,
     1                           NLAY,IOUT,DELT,PERTIM,TOTIM,IBOUND)
      ENDIF


C6--ADD TOTAL RATES AND VOLUMES TO VBVL AND PUT TITLES IN VBNM
  500 CONTINUE      
	VBVL(1,MSUM)=VBVL(1,MSUM)+DCDTIN
      VBVL(2,MSUM)=VBVL(2,MSUM)+DCDTOUT
	IF(DELT.GT.0.) THEN
         VBVL(3,MSUM)=DCDTIN/DELT
         VBVL(4,MSUM)=DCDTOUT/DELT
	ENDIF
      VBNM(1,MSUM)='    '
      VBNM(2,MSUM)='    '
      VBNM(3,MSUM)='    '
      VBNM(4,MSUM)='DCDT'
      MSUM=MSUM+1

C7--RETURN
      RETURN
      END     
C
C
      SUBROUTINE VDF1CPL1(M,IOUT)
C--SEAWAT:***********************************************************
C--SEAWAT:  INITIALIZE SEAWAT'S ITERATIVE COUPLING SCHEME
C--SEAWAT:**********************************************************
      USE VDFMODULE,   ONLY: PS,PSOLDITER
C
      PRINT *
      PRINT *,'ITERATIVE COUPLING ITERATION ',M
      WRITE(IOUT,*) 'ITERATIVE COUPLING ITERATION ',M
      PRINT *
      PSOLDITER=PS
C
      RETURN
      END
C
C
      SUBROUTINE VDF1CPL2(NCOL,NROW,NLAY,ICBUND,M,CNEW,COLD,CINACT,DONE,
     +           IOUT,NCOMP,INVSC,IBOUND,DELR,DELC,DH,DELT,PRSITY)
C     MODIFIED 10/04/2002 BY CDL
C--SEAWAT:***********************************************************
C--SEAWAT:LOGIC FOR IMPLICT COUPLING 
C--SEAWAT:**********************************************************
C
      USE VDFMODULE,   ONLY: DNSCRIT,PS,PSOLDITER,NSWTCPL
C
      DIMENSION ICBUND(NCOL,NROW,NLAY),CNEW(NCOL,NROW,NLAY,NCOMP),
     &          IBOUND(NCOL,NROW,NLAY),COLD(NCOL,NROW,NLAY,NCOMP),
     &          DELR(NCOL),DELC(NROW),DH(NCOL,NROW,NLAY),
     &          PRSITY(NCOL,NROW,NLAY)
      LOGICAL DONE
C----------------------------------------------------------------
C-----UPDATE FLUID DENSITIES FOR THIS COUPLING ITERATION
	CALL VDF1PS(NCOL,NROW,NLAY,NCOMP,CNEW,CINACT,IOUT)
C-----UPDATE FLUID VISCOSITIES FOR THIS COUPLING ITERATION
	IF(INVSC.GT.0) CALL VDF1VSC1MU(CNEW,CINACT,NCOMP,
     &                    IBOUND,NCOL,NROW,NLAY)
C-----UPDATE DCDT TERM FOR THIS COUPLING ITERATION
      CALL VDF1DC(NCOL,NROW,NLAY,NCOMP,COLD,CNEW,PRSITY,
     &     DELR,DELC,DH,DELT,IBOUND,CINACT,1)
C
      DONE=.TRUE.
      DIFFMAX=0.0
      ICOL=0
      JROW=0
      KLAY=0
      DO I=1,NROW
      DO J=1,NCOL
      DO K=1,NLAY
		IF (ICBUND(J,I,K).GT.0) THEN
		    DENSENEW=PS(J,I,K)
		    DENSEOLD=PSOLDITER(J,I,K)
			DIFF=DENSENEW-DENSEOLD
			IF (ABS(DIFF).GT.DNSCRIT) THEN
				DONE=.FALSE.
			ENDIF
			IF(ABS(DIFF).GT.ABS(DIFFMAX)) THEN
				DIFFMAX=DIFF
				ICOL=I
				JROW=J
				KLAY=K
			ENDIF
		ENDIF
      ENDDO
      ENDDO
      ENDDO
C
C      IF(.NOT.DONE) THEN
	  WRITE(IOUT,'(//)') 
	  WRITE(IOUT,*) '__________________________________________________'
        WRITE(IOUT,100) M,DIFFMAX,ICOL,JROW,KLAY
	  WRITE(IOUT,'(//)') 
  100   FORMAT(1X,'COMPLETED COUPLING ITERATION ',I5/
     +       1X,'MAXIMUM DENSITY DIFFERENCE = ',G20.5/
     +       1X,'AT CELL (I,J,K) = (',I4,',',I4,',',I4,')'/
     +       1X,'__________________________________________________'/)
C      ENDIF
C
      IF (M.EQ.NSWTCPL.AND..NOT.DONE) THEN
	   WRITE(IOUT,'(//)') 
         WRITE(IOUT,*) 'MAX SEAWAT COUPLING ITERATIONS EXCEEDED: STOPPIN
     &G '
         WRITE(*,*) 'MAX SEAWAT COUPLING ITERATIONS EXCEEDED: STOPPING '
         STOP
      ENDIF
C
      RETURN
      END

      FUNCTION SSMDENSE(J,I,K,NTYPE,MXSS,NSS,SS,NCOMP,SSMC)
C************************************************************************
C   FUNCTION SSMDENSE
C   MODIFIED 8/18/2006 BY CDL
C   FUNCTION RETURNS THE FLUID DENSITY FOR THE SPECIFIED MT3DMS BOUNDARY 
C************************************************************************
      USE VDFMODULE,   ONLY: DENSEREF
C
      DIMENSION SS(7,MXSS),SSMC(NCOMP,MXSS)
      SSMDENSE=DENSEREF
      IF (NSS.LE.0) RETURN
      DO NS=1,NSS
          ITYPE=SS(6,NS)
          KK=SS(1,NS)
          II=SS(2,NS)
          JJ=SS(3,NS)
          IF(ITYPE.EQ.NTYPE.AND.KK.EQ.K.AND.II.EQ.I.AND.JJ.EQ.J) THEN
            SSMDENSE=CALCDENS(J,I,K,SSMC(1:NCOMP,NS))
            RETURN
          ENDIF
      ENDDO
C
      RETURN      
      END
C
C
C--SEAWAT: SALTHEAD FUNCTION
      FUNCTION SALTHEAD(HF,DENSE,ELEV)
C************************************************************************
C   FUNCTION SALTHEAD
C   MODIFIED 11/25/01 BY CDL
C   MODIFIED 3/25/02 BY WBS
C   FUNCTION RETURNS SALTHEAD 
C************************************************************************
      USE VDFMODULE,   ONLY: DENSEREF
C
      DOUBLE PRECISION HF
C-----------------------------------------------------------------------
      SALTHEAD=HF*DENSEREF/DENSE+(DENSE-DENSEREF)/DENSE*ELEV
      END

C--SEAWAT: FEHEAD FUNCTION
      FUNCTION FEHEAD(HS,DENSE,ELEV)
C************************************************************************
C   FUNCTION FEHEAD
C   MODIFIED 11/25/01 BY CDL
C   MODIFIED 3/25/02 BY WBS
C   FUNCTION RETURNS FEHEAD
C************************************************************************
      USE VDFMODULE,   ONLY: DENSEREF
C
      DOUBLE PRECISION HS
C------------------------------------------------------------------------
      FEHEAD=HS*DENSE/DENSEREF-(DENSE-DENSEREF)/DENSEREF*ELEV
      END
C
C        
      FUNCTION CALCDENS(J,I,K,CONCENTRATION)
C************************************************************************
C   THIS FUNCTION CALCULATES FLUID DENSITY AS A FUNCTION OF AN ARRAY
C   OF CONCENTRATIONS AND THE FRESH PRESSURE HEAD.  THE CONCENTRATION 
C   ARRAY SHOULD CONSIST OF ONE CONCENTRATION FOR EACH MT3DMS SPECIES.  
C   BECAUSE THERE IS NO REQUIREMENT ON THE ORDER OF THE SPECIES IN 
C   MTRHOSPEC, THE CONCENTRATION ARRAY MAY BE OF SIZE NSRHOEOS OR NCOMP,
C   HENCE CONCENTRATION(*)
C************************************************************************
      USE VDFMODULE,   ONLY: DENSEREF,DENSEMIN,DENSEMAX,
     &                       DRHODC,NSRHOEOS,MTRHOSPEC,CRHOREF,
     &                       DRHODPRHD,PRHDREF,HSALT,ELEV,PS
C
      DIMENSION CONCENTRATION(*)
C-----------------------------------------------------------------------
C
C-----CALCULATE DENSITY AS A FUNCTION OF MT3DMS SPECIES
      TEMP=DENSEREF
      DO ISPEC=1,NSRHOEOS
        TEMP=TEMP+
     +            DRHODC(ISPEC)*(CONCENTRATION(MTRHOSPEC(ISPEC))-
     +                           CRHOREF(ISPEC))
      ENDDO
C
C-----INCLUDE THE EFFECTS OF PRESSURE ON FLUID DENSITY IF DRHODPRHD.NE.0
      IF(DRHODPRHD.NE.0.0) THEN
        PRHD=FEHEAD(HSALT(J,I,K),PS(J,I,K),ELEV(J,I,K))-ELEV(J,I,K)
        TEMP=TEMP+DRHODPRHD*(PRHD-PRHDREF)
      ENDIF
C
C-----ENFORCE DENSITY LIMITERS IF CALCULATED DENSITY OUTSIDE OF SPECIFIED RANGE
      IF(DENSEMAX.NE.0.AND.TEMP.GT.DENSEMAX) THEN
		TEMP=DENSEMAX
	ENDIF
      IF(DENSEMIN.NE.0.AND.TEMP.LT.DENSEMIN) THEN
		TEMP=DENSEMIN
	ENDIF
C
C-----SET FUNCTION CALCDENS
	CALCDENS=TEMP
      RETURN
      END
C
C
      SUBROUTINE VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,H2,HS2,PS2,Z2,TOP2,
     &                    BOT2)
C************************************************************************
C       THIS SUBROUTINE CALCULATES NEW VALUES FOR Z1 AND H1
C       Z1 IS THE ELEVATION HALFWAY BETWEEN THE WATER TABLE AND THE CELL BOTTOM
C       H1 IS THE EQUIVALENT FRESHWATER HEAD AT Z1
C************************************************************************
C       IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      USE VDFMODULE,   ONLY: DENSEREF
C
      DOUBLE PRECISION H1,HS1,H2,HS2
      LOGICAL CORRECT
C-----------------------------------------------------------------------
C--SEAWAT: NEW LAYCONS TO WORRY ABOUT IN HUF AND LPF (CONVERTIBLE, NONCONVERTIBLE)
      CORRECT=.FALSE.
      IF (HS1.LT.TOP1) CORRECT=.TRUE.
      IF (CORRECT) THEN
         TEMP=(H1+BOT1)/2
         H1=H1+(PS1-DENSEREF)/DENSEREF*(Z1-TEMP)
         Z1=TEMP
      ENDIF
C--SEAWAT: CORRECT NEIGHBOR CELL
      CORRECT=.FALSE.
      IF (HS2.LT.TOP2) CORRECT=.TRUE.
      IF (CORRECT) THEN
         TEMP=(H2+BOT2)/2
         H2=H2+(PS2-DENSEREF)/DENSEREF*(Z2-TEMP)
         Z2=TEMP
      ENDIF
      RETURN
      END     
C
      SUBROUTINE VDF1UPDTFLWFLD1(IOUT,IUPDTFLWFLD,DELT,DTRANS,N,KSTP,
     &                        KPER,NCOL,NROW,NLAY,ICBUND,CNEW,COLD,
     &                        NCOMP,NSWTCPL,TIME2,HT2,INVDF,INBTN)
C     CREATED 12/06/2006 BY CDL
C--SEAWAT:***********************************************************
C--SEAWAT: DETERMINE WHETHER OR NOT TO UPDATE FLOW FIELD.
C--SEAWAT: COLDFLW CONTAINS THE CONCENTRATIONS THAT WERE USED TO
C--SEAWAT: TO CALCULATE THE FLUID DENSITIES USED THE LAST TIME THE
C--SEAWAT: THE FLOW EQUATION WAS SOLVED.
C--SEAWAT: IUPDTFLWFLD IS A FLAG THAT INDICATES WHETHER OR NOT THE
C--SEAWAT: FLOW EQUATION IS SOLVED FOR THIS TRANSPORT TIMESTEP.  IF
C--SEAWAT: IUPDTFLWFLD=1, THE THE FLOW EQUATION IS BEING SOLVED.
C--SEAWAT: DELT IS THE LENGTH OF THE TIME INCREMENT FOR FLOW.
C--SEAWAT: DTRANS IS THE LENGTH OF THE TIME INCREMENT FOR TRANSPORT.
C--SEAWAT:**********************************************************
      USE VDFMODULE,   ONLY: DNSCRIT,PS,COLDFLW
C
      DIMENSION ICBUND(NCOL,NROW,NLAY),CNEW(NCOL,NROW,NLAY,NCOMP),
     &          COLD(NCOL,NROW,NLAY,NCOMP)
C----------------------------------------------------------------
      IUPDTFLWFLD=0
C
C--CASE WHERE MT3DMS IS BEING RUN IN CONSTANT DENSITY MODE WITH GWF PROCESS
C--SOLVE FLOW USING DELT = LENGTH OF THE MODFLOW TIMESTEP
      IF(INVDF.GT.0) GOTO 5
      IF(N.EQ.1) THEN
        IUPDTFLWFLD=1
      ENDIF
      RETURN
   5  CONTINUE
C
C--CASE WHERE VDF IS ACTIVE, BUT NOT USING THE FLOW FIELD UPDATE CONTROL
C--SET DELT=DTRANS, INITIALIZE COLDFLW IF FIRST TRANSPORT STEP OF SIMULATION
C--THEN RETURN
      IF(NSWTCPL.LT.0) GOTO 6
      DELT=DTRANS
      IUPDTFLWFLD=1
      IF(N*KSTP*KPER.EQ.1) COLDFLW=COLD
      RETURN
   6  CONTINUE
C
C--CASE WHERE USING UPDATE FLOW FIELD--3 REASONS TO SOLVE FLOW
C----1. FIRST TRANSPORT STEP OF A MODFLOW TIMESTEP (N=1)
C----2. LAST TRANSPORT STEP OF A MODFLOW TIMESTEP (TIME2.GE.HT2)
C----3. DENSITY AT A CELL CHANGED BY MORE THAN DNSCRIT
C
C--CASE 1.
      IF(N.GT.1) GOTO 7
      DELT=DTRANS
      COLDFLW=COLD
      IUPDTFLWFLD=1
      RETURN
   7  CONTINUE
C
C--FOR CASES 2 AND 3, NEED TO INCREASE LENGTH OF DELT
        WRITE(IOUT,10) DELT,DTRANS
  10    FORMAT(1X,'INCREMENTING FLOW SOLUTION TIME INCREMENT FROM ',
     &         G12.4,' BY DTRANS ',G12.4)
        DELT=DELT+DTRANS
        WRITE(IOUT,20) N,DELT
  20    FORMAT(1X,'ACCUMULATED FLOW SOLUTION TIME INCREMENT AT ',
     &         ' TRANSPORT TIMESTEP ',I6,' IS ',G12.4//) 
C--CASE 2.
      IF(TIME2.LT.HT2) GOTO 8
      IUPDTFLWFLD=1
      RETURN
   8  CONTINUE   
C
C--CASE 3.
C----IF DENSITY HAS CHANGED BY MORE THAN DNSCRIT, UPDATE FLOW FIELD
      DIFFMAX=0.0
      ICOL=0
      JROW=0
      KLAY=0
      DO I=1,NROW
      DO J=1,NCOL
      DO K=1,NLAY
		IF (ICBUND(J,I,K).GT.0) THEN
		    DENSENEW=CALCDENS(J,I,K,CNEW(J,I,K,1:NCOMP))
		    DENSEOLD=CALCDENS(J,I,K,COLDFLW(J,I,K,1:NCOMP))
			DIFF=DENSENEW-DENSEOLD
			IF (ABS(DIFF).GE.DNSCRIT) THEN
				IUPDTFLWFLD=1
			ENDIF
			IF(ABS(DIFF).GT.ABS(DIFFMAX)) THEN
				DIFFMAX=DIFF
				ICOL=I
				JROW=J
				KLAY=K
			ENDIF

		ENDIF
      ENDDO
      ENDDO
      ENDDO
      IF(IUPDTFLWFLD.GT.0.) THEN
	  WRITE(IOUT,'(//)') 
	  WRITE(IOUT,*) '__________________________________________________'
        WRITE(IOUT,100) N,DELT,DIFFMAX,ICOL,JROW,KLAY
	  WRITE(IOUT,'(//)') 
  100   FORMAT(1X,'UPDATING FLOW FIELD AT TRANSPORT STEP ',I6,
     &       ' WITH FLOW TIME INCREMENT OF ',G12.4,/
     +       1X,'MAXIMUM DENSITY DIFFERENCE = ',G20.5/
     +       1X,'AT CELL (I,J,K) = (',I4,',',I4,',',I4,')'/
     +       1X,'__________________________________________________'/)
      ENDIF
      RETURN
      END
C
      SUBROUTINE VDF1UPDTFLWFLD2(DELT,N,TIME2,HT2,COLD,NCOL,NROW,NLAY,
     +                           NCOMP)
C     CREATED 12/06/2006 BY CDL
C--SEAWAT:***********************************************************
C--SEAWAT: RESET DELT AND COLDFLW IF FLOW EQUATION WAS SOLVED
C--SEAWAT:**********************************************************
      USE VDFMODULE,   ONLY: COLDFLW
      DIMENSION COLD(NCOL,NROW,NLAY,NCOMP)
C
C----------------------------------------------------------------
C
C--FLOW EQUATION WAS JUST SOLVED.  NEED TO RESET DELT TO ZERO
C--ONLY DO THIS IF HAVEN'T REACHED END OF FLOW TIMESTEP YET (I.E. TIME2.LT.HT2)
      IF(TIME2.LT.HT2) DELT=0.
      COLDFLW=COLD
      RETURN
      END
      
      subroutine showdcdt(ncol,nrow,nlay,kiter,kstp,kper)
      use vdfmodule, only: dcdt
      dcdtin=0.
      dcdtout=0.
      do k=1,nlay
      do i=1,nrow
      do j=1,ncol
         IF(DCDT(J,I,K).GE.0) THEN
            DCDTIN=DCDTIN+DCDT(J,I,K)
         ELSE
            DCDTOUT=DCDTOUT+ABS(DCDT(J,I,K))
         ENDIF
      ENDDO
      ENDDO
      ENDDO
      print *,kiter,kstp,kper
      print *,'showdcdt ',dcdtin,dcdtout
      return
      end
      
C
      SUBROUTINE SVDF1PSV(IGRID)
      USE VDFMODULE
      VDFDAT(IGRID)%MT3DRHOFLG=>MT3DRHOFLG
      VDFDAT(IGRID)%MFNADVFD=>MFNADVFD
      VDFDAT(IGRID)%NSWTCPL=>NSWTCPL
      VDFDAT(IGRID)%IWTABLE=>IWTABLE
      VDFDAT(IGRID)%NSRHOEOS=>NSRHOEOS
      VDFDAT(IGRID)%DENSEMIN=>DENSEMIN
      VDFDAT(IGRID)%DENSEMAX=>DENSEMAX
      VDFDAT(IGRID)%DENSEREF=>DENSEREF
      VDFDAT(IGRID)%FIRSTDT=>FIRSTDT
      VDFDAT(IGRID)%DNSCRIT=>DNSCRIT
      VDFDAT(IGRID)%DRHODPRHD=>DRHODPRHD
      VDFDAT(IGRID)%PRHDREF=>PRHDREF
      VDFDAT(IGRID)%MTRHOSPEC=>MTRHOSPEC
      VDFDAT(IGRID)%PS=>PS
      VDFDAT(IGRID)%RHOCR=>RHOCR
      VDFDAT(IGRID)%RHOCC=>RHOCC
      VDFDAT(IGRID)%RHOCV=>RHOCV
      VDFDAT(IGRID)%HSALT=>HSALT
      VDFDAT(IGRID)%ELEV=>ELEV
      VDFDAT(IGRID)%DCDT=>DCDT
      VDFDAT(IGRID)%COLDFLW=>COLDFLW
      VDFDAT(IGRID)%PSOLDITER=>PSOLDITER
      VDFDAT(IGRID)%DRHODC=>DRHODC
      VDFDAT(IGRID)%CRHOREF=>CRHOREF
C
      RETURN
      END
C
