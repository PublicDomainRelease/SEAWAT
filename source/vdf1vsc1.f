C
      MODULE VDFVSCMODULE
        INTEGER, SAVE, POINTER                 ::MT3DMUFLG
        REAL,    SAVE, POINTER                 ::VISCMIN,VISCMAX,VISCREF
        INTEGER, SAVE, POINTER                 ::NSMUEOS
        INTEGER, SAVE, POINTER                 ::MUTEMPOPT,MTMUTEMPSPEC,
     +                                           MUNCOEFF
        INTEGER, SAVE, POINTER,  DIMENSION(:)                 ::MTMUSPEC
        REAL,    SAVE, POINTER,  DIMENSION(:)                 ::DMUDC
        REAL,    SAVE, POINTER,  DIMENSION(:)                 ::CMUREF
        REAL,    SAVE, POINTER,  DIMENSION(:)                 ::AMUCOEFF
        REAL,    SAVE, POINTER,  DIMENSION(:,:,:)             ::MU
      TYPE VDFVSCTYPE
        INTEGER, POINTER                       ::MT3DMUFLG
        REAL,    POINTER                       ::VISCMIN,VISCMAX,VISCREF
        INTEGER, POINTER                       ::NSMUEOS
        INTEGER, POINTER                       ::MUTEMPOPT,MTMUTEMPSPEC,
     +                                           MUNCOEFF
        INTEGER, POINTER,  DIMENSION(:)        ::MTMUSPEC
        REAL,    POINTER,  DIMENSION(:)        ::DMUDC
        REAL,    POINTER,  DIMENSION(:)        ::CMUREF
        REAL,    POINTER,  DIMENSION(:)        ::AMUCOEFF
        REAL,    POINTER,  DIMENSION(:,:,:)    ::MU
      END TYPE
      TYPE(VDFVSCTYPE),SAVE:: VDFVSCDAT(10)
      END MODULE VDFVSCMODULE
C
      SUBROUTINE VDF1VSC1AR(IN,IGRID,IOUT,NCOL,NROW,NLAY)
C     ******************************************************************
C     ALLOCATE ARRAYS AND READ DATA FOR VISCOSITY PACKAGE
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
C-----WHEN SWITCHING TO MODFLOW2005 STRUCTURE, UNCOMMENT FOLLOWING LINE
C     USE GLOBAL,         ONLY: IOUT,NCOL,NROW,NLAY
      USE VDFVSCMODULE
      CHARACTER*200 LINE
C     ------------------------------------------------------------------      
C1----ALLOCATE AND INITIALIZE VARIABLES
      ALLOCATE(MT3DMUFLG,VISCMIN,VISCMAX,VISCREF,NSMUEOS,MUTEMPOPT,
     &         MTMUTEMPSPEC,MUNCOEFF)
      ALLOCATE(AMUCOEFF(10))
      ALLOCATE(MU(NCOL,NROW,NLAY))
C-----INITIALIZE
      AMUCOEFF=0.
C
C2----IDENTIFY PACKAGE
      WRITE(IOUT,1) IN
    1 FORMAT(1X,/1X,'VSC -- VISCOSITY PACKAGE, VERSION 1',
     1', 8/15/2006',/,9X,'INPUT READ FROM UNIT',I3,/)
C
C3----READ MT3D SPECIES NUMBER TO BE USED IN EOS FOR VISCOSITY
      CALL URDCOM(IN,IOUT,LINE)
      LLOC = 1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MT3DMUFLG,DUM,IOUT,IN)
      READ(IN,*) VISCMIN,VISCMAX
C      
C4----READ VISCOSITY EQUATION OF STATE INFORMATION USING SIMPLIFIED METHOD
      IF(MT3DMUFLG.GE.0) THEN
        NSMUEOS=1
        MUTEMPOPT=0
        ALLOCATE(MTMUSPEC(NSMUEOS),DMUDC(NSMUEOS),CMUREF(NSMUEOS))
        MTMUSPEC(1)=MT3DMUFLG
        READ(IN,*) VISCREF,DMUDC(1),CMUREF(1)
      ENDIF
C
C5----READ VISCOSITY EQUATION OF STATE INFORMATION USING DETAILED METHOD
      IF(MT3DMUFLG.EQ.-1) THEN
        READ(IN,*) VISCREF
        READ(IN,*) NSMUEOS,MUTEMPOPT
        IF(NSMUEOS.GT.0) THEN
            ALLOCATE(MTMUSPEC(NSMUEOS),DMUDC(NSMUEOS),CMUREF(NSMUEOS))
            DO I=1,NSMUEOS
                READ(IN,*) MTMUSPEC(I),DMUDC(I),CMUREF(I)
            ENDDO
        ENDIF
        IF(MUTEMPOPT.GT.0) THEN
            MUNCOEFF=4
            IF(MUTEMPOPT.EQ.2) MUNCOEFF=5
            IF(MUTEMPOPT.EQ.3) MUNCOEFF=2
            READ(IN,*) MTMUTEMPSPEC,(AMUCOEFF(I),I=1,MUNCOEFF)
        ENDIF
      ENDIF
C      
C6----ECHO VISCOSITY INFORMATION TO OUTPUT FILE
      WRITE(IOUT,2)
    2 FORMAT(1X,'VISCOSITY EQUATION OF STATE')
      WRITE(IOUT,3) VISCREF
    3 FORMAT(1X,'MU = ',G10.2)
      DO I=1,NSMUEOS
        WRITE(IOUT,4) DMUDC(I),I,CMUREF(I)
      ENDDO
    4 FORMAT(1X,' + ',G10.2,' * ( CONC( ',I4,') - ',G10.2,' )')
      IF(MT3DMUFLG.EQ.-1) THEN
        WRITE(IOUT,5) MUTEMPOPT
        IF(MUTEMPOPT.GT.0) THEN
            WRITE(IOUT,6) MTMUTEMPSPEC,MUNCOEFF,
     +                 (AMUCOEFF(I),I=1,MUNCOEFF)
        ENDIF
      ENDIF
    5 FORMAT(1X,'OPTION TO REPRESENT NONLINEAR TEMPERATURE/VISCOSITY REL
     +ATION IS ',I4)
    6 FORMAT(1X,'MT3DMS SPECIES REPRESENTING TEMPERATURE IS ',I4,/,
     +       'NUMBER OF REQUIRED VISCOSITY COEFFICIENTS IS ',I4,/,
     +       'COEFFICIENT VALUES ARE: ',10G15.5)
      WRITE(IOUT,'(//)')

C7----RESET POINTERS      
      CALL SVDF1VSC1PSV(IGRID)
C      
      RETURN
      END
C
      SUBROUTINE VDF1VSC1RPSS(IN,IOUT,NCOL,NROW,NLAY,CNEW,NCOMP,
     &                     CINACT,IBOUND,KKPER)
C     ******************************************************************
C     READ VISCOSITY ARRAY
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
C-----WHEN SWITCHING TO MODFLOW2005 STRUCTURE, UNCOMMENT FOLLOWING LINE
C     USE GLOBAL,         ONLY: IOUT,NCOL,NROW,NLAY
      USE VDFVSCMODULE,   ONLY: MT3DMUFLG,VISCMIN,VISCMAX,VISCREF,
     1                          DMUDC,CMUREF,MU
C     ------------------------------------------------------------------      
      DIMENSION CNEW(NCOL,NROW,NLAY,NCOMP),IBOUND(NCOL,NROW,NLAY)
      CHARACTER*24 ANAME(4)
      DATA ANAME(1) /'   VISCOSITY LAYER INDEX'/
      DATA ANAME(2) /'         FLUID VISCOSITY'/
      DATA ANAME(3) /'           CONCENTRATION'/
C-----------------------------------------------------------------------

C     POPULATE FLUID VISCOSITY ARRAY (MU)
      IF(MT3DMUFLG.EQ.0) THEN
        READ(IN,*) INVISC
        WRITE(IOUT,'(//)')
        WRITE(IOUT,500) INVISC
        IF(INVISC.LT.0) WRITE(IOUT,510)
        IF(INVISC.EQ.0) WRITE(IOUT,520)
        IF(INVISC.GT.0) WRITE(IOUT,530)
        IF(INVISC.EQ.2) WRITE(IOUT,540)
C       IF FIRST STRESS PERIOD, BUT INVISC LESS THAN ZERO, SET TO VISCREF
        IF (INVISC.LT.0.AND.KKPER.EQ.1) THEN
            DO K=1,NLAY
            DO I=1,NROW
            DO J=1,NCOL
                IF(IBOUND(J,I,K).NE.0)
     &             MU(J,I,K)=VISCREF
            ENDDO
            ENDDO
            ENDDO
        ENDIF
C       IF INVISC EQUAL TO ZERO, SET TO VISCREF
        IF (INVISC.EQ.0) THEN
            DO K=1,NLAY
            DO I=1,NROW
            DO J=1,NCOL
                IF(IBOUND(J,I,K).NE.0)
     &             MU(J,I,K)=VISCREF
            ENDDO
            ENDDO
            ENDDO
        ENDIF
C       IF INVISC GREATER THAN ZERO, THEN READ VISC ARRAY
        ITEMP=2
        IF(INVISC.EQ.2) ITEMP=3
        IF(INVISC.GT.0) THEN
            DO K=1,NLAY
                CALL U2DREL(MU(1,1,K),ANAME(ITEMP),NROW,NCOL,K,IN,
     +                        IOUT)
            ENDDO
        ENDIF
C       IF INVISC EQUAL TO 2, THEN CONVERT VISCOSITY ARRAY USING LINEAR EQUATION
        IF (INVISC.EQ.2) THEN
            DO K=1,NLAY
            DO I=1,NROW
            DO J=1,NCOL
                IF(IBOUND(J,I,K).NE.0) 
     +             MU(J,I,K)=VISCREF+DMUDC(1)*(MU(J,I,K)-CMUREF(1))
            ENDDO
            ENDDO
            ENDDO
        ENDIF
      ELSE
C         SET VISCOSITY ARRAY USING CONCENTRATIONS FROM MT3D IF FIRST STRESS PERIOD
          IF(KKPER.EQ.1)
     &       CALL VDF1VSC1MU(CNEW,CINACT,NCOMP,IBOUND,NCOL,NROW,NLAY)
      ENDIF
  500 FORMAT(1X,'INVISC VALUE SPECIFIED AS:',I4)
  510 FORMAT(1X,'VALUES FOR VISCOSITY ARRAY WILL BE REUSED OR SET TO VIS
     +CREF')
  520 FORMAT(1X,'VALUES FOR THE VISCOSITY ARRAY WILL BE SET TO VISCREF')
  530 FORMAT(1X,'VALUES FOR THE VISCOSITY ARRAY WILL BE READ FROM VSC FI
     +LE')
  540 FORMAT(1X,'VALUES READ AS CONCENTRATION WILL BE CONVERTED TO FLUID
     + VISCOSITY USING EQUATION OF STATE')
      RETURN
      END
C
      SUBROUTINE VDF1VSC1MU(CNEW,CINACT,NCOMP,IBOUND,NCOL,NROW,NLAY)
C     ******************************************************************
C     READ VISCOSITY ARRAY
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
C-----WHEN SWITCHING TO MODFLOW2005 STRUCTURE, UNCOMMENT FOLLOWING LINE
C     USE GLOBAL,         ONLY: IOUT,NCOL,NROW,NLAY
      USE VDFVSCMODULE,   ONLY: MT3DMUFLG,VISCMIN,VISCMAX,VISCREF,
     1                          NSMUEOS,MTMUSPEC,DMUDC,CMUREF,MU,
     2                          MUTEMPOPT,MTMUTEMPSPEC
C     ------------------------------------------------------------------      
      DIMENSION CNEW(NCOL,NROW,NLAY,NCOMP),IBOUND(NCOL,NROW,NLAY)
C      
      IF(MT3DMUFLG.EQ.0) RETURN
      MU=VISCREF
      DO K=1,NLAY
      DO I=1,NROW
      DO J=1,NCOL
        IF(IBOUND(J,I,K).EQ.0) CYCLE
C--CHECK FOR INACTIVE CELL AND CYCLE
        IF(CNEW(J,I,K,1).EQ.CINACT) CYCLE
C-------ADD EFFECT OF NONLINEAR TEMPERATURE/VISCOSITY EQUATION
        IF(MUTEMPOPT.GT.0) 
     1           MU(J,I,K)=CALCMUTEMP(CNEW(J,I,K,MTMUTEMPSPEC))
C-------LOOP THROUGH INFLUENCING SPECIES AND ACCUMULATE EFFECTS
        IF(NSMUEOS.GT.0) THEN
            DO ICOMP=1,NSMUEOS
                MU(J,I,K)=MU(J,I,K)+
     1          DMUDC(ICOMP)*
     2          (CNEW(J,I,K,MTMUSPEC(ICOMP))-CMUREF(ICOMP))
            ENDDO
        ENDIF
        IF (VISCMAX.GT.0.) THEN
            IF(MU(J,I,K).GT.VISCMAX) MU(J,I,K)=VISCMAX
        ENDIF
        IF (VISCMIN.GT.0.) THEN
            IF(MU(J,I,K).LT.VISCMIN) MU(J,I,K)=VISCMIN
        ENDIF
      ENDDO
      ENDDO
      ENDDO
C
      RETURN
      END
C
      FUNCTION CALCMUTEMP(TEMPERATURE)
C     ******************************************************************
C     CALCULATE VISCOSITY AS A FUNCTION OF TEMPERATURE
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE VDFVSCMODULE,    ONLY: MUTEMPOPT,AMUCOEFF
C     ------------------------------------------------------------------
C
      IF(MUTEMPOPT.EQ.1)
     &  CALCMUTEMP=AMUCOEFF(1)*AMUCOEFF(2)**
     &                (AMUCOEFF(3)/(TEMPERATURE+AMUCOEFF(4)))
C
      IF(MUTEMPOPT.EQ.2)
     &  CALCMUTEMP=AMUCOEFF(1)*(AMUCOEFF(2)+AMUCOEFF(3)*
     &                (TEMPERATURE+AMUCOEFF(4)))**AMUCOEFF(5)
C
      IF(MUTEMPOPT.EQ.3)
     &  CALCMUTEMP=AMUCOEFF(1)*TEMPERATURE**AMUCOEFF(2)
C
      RETURN
      END
C
      SUBROUTINE SVDF1VSC1PSV(IGRID)
      USE VDFVSCMODULE
      VDFVSCDAT(IGRID)%MT3DMUFLG=>MT3DMUFLG
      VDFVSCDAT(IGRID)%VISCMIN=>VISCMIN
      VDFVSCDAT(IGRID)%VISCMAX=>VISCMAX
      VDFVSCDAT(IGRID)%VISCREF=>VISCREF
      VDFVSCDAT(IGRID)%NSMUEOS=>NSMUEOS
      VDFVSCDAT(IGRID)%MUTEMPOPT=>MUTEMPOPT
      VDFVSCDAT(IGRID)%MTMUTEMPSPEC=>MTMUTEMPSPEC
      VDFVSCDAT(IGRID)%MUNCOEFF=>MUNCOEFF
      VDFVSCDAT(IGRID)%MTMUSPEC=>MTMUSPEC
      VDFVSCDAT(IGRID)%DMUDC=>DMUDC
      VDFVSCDAT(IGRID)%CMUREF=>CMUREF
      VDFVSCDAT(IGRID)%AMUCOEFF=>AMUCOEFF
      VDFVSCDAT(IGRID)%MU=>MU      
C
      RETURN
      END
      