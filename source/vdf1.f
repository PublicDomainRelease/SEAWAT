
      SUBROUTINE VDF1DF(IN,IOUT,IFREFM,MTDNCONC,MFNADVFD,INBTN,
     &                  NSWTCPL,DNSCRIT,FIRSTDT,IWTABLE,INSEN,
     &                  NCOMP,MXSS,NSSVL,ISUMY,ISUMIY,LCCNEW,LCSSM,
     &                  LCSSMC,LTCRCH,LTCEVT,NCOL,NROW,NLAY)
C **********************************************************************
CVDF THIS SUBROUTINE DEFINES THE VARIABLE-DENSITY FLOW PROCESS
C **********************************************************************
CVDF CREATED 11/20/01 FOR MF2K VDF PROCESS
C
      CHARACTER*200 LINE
	INCLUDE 'vdf.inc'
C----------------------------------------------------------------------

C--SEAWAT: ALLOCATE SPACE FOR MT3D ARRAYS EVEN IF NOT USED
	NSSVL=6
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

      IF(INSEN.GT.0) THEN
        PRINT*, 'THE VDF PROCESS IS NOT COMPATIBLE WITH THE SENSITIVITY'
        PRINT*, 'PROCESS. INACTIVATE EITHER ONE OF THESE TWO PROCESSES'
        STOP
      ENDIF

      CALL URDCOM(IN,IOUT,LINE)
      LLOC = 1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MTDNCONC,DUM,IOUT,INHFB)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MFNADVFD,DUM,IOUT,INHFB)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NSWTCPL,DUM,IOUT,INHFB)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IWTABLE,DUM,IOUT,INHFB)

C--SEAWAT: READ DENSITY LIMITERS
      IF(IFREFM.EQ.0) THEN
        READ(IN,'(2F10.0)') DENSEMIN,DENSEMAX
      ELSEIF(IFREFM.NE.0) THEN
        READ(IN,*) DENSEMIN,DENSEMAX
      ENDIF

C--SEAWAT: READ DNSCRIT
      IF(NSWTCPL.GT.1.AND.IFREFM.EQ.0) THEN
        READ(IN,'(F10.0)') DNSCRIT
      ELSEIF(NSWTCPL.GT.1.AND.IFREFM.NE.0) THEN
        READ(IN,*) DNSCRIT
      ENDIF
C--SEAWAT: RESET DNSCRIT TO DEFAULT IF NOT ENTERED
      IF (DNSCRIT.EQ.0) DNSCRIT=1.e-5

C--SEAWAT: PRINT IMPLICIT COUPLING INFO TO IOUT
      IF (NSWTCPL.LE.1) NSWTCPL=1
	IF (NSWTCPL.GT.1) THEN
	 WRITE(IOUT,558) NSWTCPL,DNSCRIT
	ELSE
	 WRITE(IOUT,559)
	ENDIF

C--SWT: STOP PROGRAM IN THIS CIRCUMSTANCE
      IF(MTDNCONC.GT.0.AND.INBTN.EQ.0) THEN
        WRITE(IOUT,580)
        STOP
      ENDIF

C--SWT:PRINT MTDNCONC INFO TO OUTPUT FILE
      IF(MTDNCONC.EQ.0) THEN
        WRITE(IOUT,500)
      ELSE
        WRITE(IOUT,510) MTDNCONC
      ENDIF

C--SWT: PRINT MFNADVFD INFO TO OUTPUT FILE
      IF(MFNADVFD.EQ.2) THEN
        WRITE(IOUT,553)
      ELSE
        WRITE(IOUT,557)
      ENDIF

C--SEAWAT: ADD UTILITY FOR NONLINEAR EQ OF STATE
      IF(IFREFM.EQ.0) THEN
         READ(IN,'(2F10.0)') DENSEREF,DENSESLP
      ELSE
         READ(IN,*) DENSEREF,DENSESLP
      ENDIF

C--SEAWAT: READ FIRSTDT, AND WRITE TO IOUT
      IF(IFREFM.EQ.0) THEN
        READ(IN,'(F10.0)') FIRSTDT
        WRITE(IOUT,590) FIRSTDT
      ELSEIF(IFREFM.NE.0) THEN
        READ(IN,*) FIRSTDT
        WRITE(IOUT,590) FIRSTDT
      ENDIF      

  500 FORMAT(1X,'FLUID DENSITY IS SPECIFIED BY USER IN THE VDF FILE')
  510 FORMAT(1X,'MT3DMS SPECIES USED IN EQUATION OF STATE FOR FLUID DENS
     &ITY: ',I10)
  520 FORMAT(1X,'FLUID VISCOSITY IS SPECIFIED BY USER IN THE VDF FILE')
  530 FORMAT(1X,'MT3DMS SPECIES USED IN EQUATION OF STATE FOR FLUID VISC
     &OSITY: ',I10)
  553 FORMAT(1X,'A CENTRAL-IN-SPACE-WEIGHTED ALGORITHM IS USED TO CALCUT
     &E FLUID DENSITY TERMS THAT CONSERVE MASS')
  557 FORMAT(1X,'AN UPSTREAM-WEIGHTED ALGORITHM IS USED TO CALCULATE FLU
     &ID DENSITY TERMS THAT CONSERVE MASS')

  558 FORMAT(1X,'COUPLING BETWEEN FLOW AND TRANSPORT IS IMPLICIT',
     &       /1X,G10.4,' COUPLING ITERATIONS',
     &       /1X,G10.4,' IS THE DENSITY CONVERGENCE CRITERIA')
  559 FORMAT(1X,'COUPLING BETWEEN FLOW AND TRANSPORT IS EXPLICIT')

      WRITE(IOUT,560) DENSEREF,DENSESLP
  560 FORMAT(1X,G10.4,' REFERENCE DENSITY',
     &       /1X,G10.4,' DENSITY SLOPE FOR EQUATION OF STATE')
  580 FORMAT(1X,'ERROR: MTDNCONC GREATER THAN ZERO, BUT MT3DMS NOT USED'
     &)

  590 FORMAT(1X,'FIRSTDT SPECIFIED BY USER IN THE VDF FILE IS: ',
     &       G15.7)
C--SEAWAT: WRITE INFO ABOUT WATER-TABLE CORRECTION
      IF (IWTABLE.EQ.0) WRITE(IOUT,600)
  600 FORMAT(1X,'VARIABLE-DENSITY WATER-TABLE CORRECTIONS NOT ADDED')
      IF (IWTABLE.GT.0) WRITE(IOUT,610)
  610 FORMAT(1X,'VARIABLE-DENSITY WATER-TABLE CORRECTIONS ARE ADDED')

      RETURN
      END

      SUBROUTINE VDF1AL(IOUT,NCOL,NROW,NLAY,ISUMVDF,LPS,LELEV,
     &           LRHOCR,LRHOCC,LRHOCV,LDCDT,LHSALT,LPSOLD)
C **********************************************************************
C    THIS SUBROUTINE ALLOCATES SPACE FOR VDF ARRAY.
C **********************************************************************
C    MODIFIED 1/30/01 FOR MF2K VDF PROCESS
C
      NODES=NCOL*NROW*NLAY      
      ISUMVDF=1
C
C--COMPUTE LOCATION WITHIN VDF ARRAY
      LELEV=ISUMVDF  
      ISUMVDF=ISUMVDF+NODES  
      LPS=ISUMVDF
      ISUMVDF=ISUMVDF+NODES
      LRHOCR=ISUMVDF
      ISUMVDF=ISUMVDF+NODES
      LRHOCC=ISUMVDF
      ISUMVDF=ISUMVDF+NODES
      LRHOCV=ISUMVDF
      ISUMVDF=ISUMVDF+NODES
      LDCDT=ISUMVDF
      ISUMVDF=ISUMVDF+NODES
      LHSALT=ISUMVDF
C--SEAWAT: ADD LPSOLD
c---left *2 in case program needs to be complied in single precision form.
      ISUMVDF=ISUMVDF+NODES*2
C       ISUMVDF=ISUMVDF+NODES
      LPSOLD=ISUMVDF
      ISUMVDF=ISUMVDF+NODES

C
C--WRITE HOW MANY ELEMENTS OF THE VDF ARRAY ARE USED
      WRITE(IOUT,1090) ISUMVDF
 1090 FORMAT(1X,I8,' ELEMENTS OF THE VDF ARRAY USED BY VDF PROCESS'
     &/)

      END

      SUBROUTINE VDF1IZ(NCOL,NROW,NLAY,IBOUND,BOTM,NBOTM,ELEV,
     &                      HSALT,HNEW)
C***********************************************************************
C--SEAWAT: INITIALIZE ELEVATION ARRAY
C      CREATED 02/11/2002
C***********************************************************************
      DOUBLE PRECISION HNEW,HSALT
      DIMENSION IBOUND(NCOL,NROW,NLAY),BOTM(NCOL,NROW,0:NBOTM),
     &          ELEV(NCOL,NROW,NLAY),PS(NCOL,NROW,NLAY),
     &          HSALT(NCOL,NROW,NLAY),HNEW(NCOL,NROW,NLAY)
	INCLUDE 'vdf.inc'

C       INITIALIZE ELEVATION ARRAY
C       WILL NOT WORK IF QUASI-3D LAYER INCLUDED IN MODEL
      DO K=1,NLAY
      DO I=1,NROW
      DO J=1,NCOL
         IF(IBOUND(J,I,K).EQ.0) GOTO 10
                ELEV(J,I,K)=(BOTM(J,I,K-1)-BOTM(J,I,K))/2+BOTM(J,I,K)
   10 ENDDO
      ENDDO
      ENDDO

C--SEAWAT: SET HSALT EQUAL TO HNEW (AT THIS POINT, HNEW AND HSALT ARE STARTING HEADS)
      HSALT=HNEW
      RETURN
      END     
        
      SUBROUTINE VDF1RP(IOUT,INVDF,NCOL,NROW,NLAY,HNEW,HSALT,
     &                      PS,ELEV,MTDNCONC,IFREFM,CNEW,
     &                      NCOMP,CINACT,IBOUND,BOTM,KKPER)
C***********************************************************************
C--SEAWAT: INITIALIZE DENSITY AND HSALT ARRAYS
C      CREATED 11/20/01
C      THE USER IS ALLOWED TWO DIFFERENT OPTIONS FOR
C      TREATING FLUID DENSITY.
C          1.  CALCULATED FROM CONCENTRATION
C          2.  SPECIFIED IN VDF INPUT FILE
C***********************************************************************
      DOUBLE PRECISION HNEW,HSALT
      DIMENSION HNEW(NCOL,NROW,NLAY),HSALT(NCOL,NROW,NLAY),
     &          PS(NCOL,NROW,NLAY),ELEV(NCOL,NROW,NLAY),
     &          CNEW(NCOL,NROW,NLAY,NCOMP),IBOUND(NCOL,NROW,NLAY),
     &          HTMP(NCOL,NROW,NLAY)   
      CHARACTER*24 ANAME(4)
      DATA ANAME(1) /'     DENSITY LAYER INDEX'/
      DATA ANAME(2) /'           FLUID DENSITY'/
	DATA ANAME(3) /'           CONCENTRATION'/
	INCLUDE 'vdf.inc'
C-----------------------------------------------------------------------
C     POPULATE FLUID DENSITY ARRAY (PS)
      IF(MTDNCONC.EQ.0) THEN
		IF(IFREFM.EQ.0) THEN
			READ(INVDF,'(I10)') INDENSE
		ELSE
			READ(INVDF,*) INDENSE
		ENDIF
		WRITE(IOUT,'(//)')
		WRITE(IOUT,500) INDENSE
		IF(INDENSE.LT.0) WRITE(IOUT,510)
		IF(INDENSE.EQ.0) WRITE(IOUT,520)
		IF(INDENSE.GT.0) WRITE(IOUT,530)
		IF(INDENSE.EQ.2) WRITE(IOUT,540)
C		IF FIRST STRESS PERIOD, BUT INDENSE LESS THAN ZERO, SET TO DENSEREF
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
				CALL U2DREL(PS(1,1,K),ANAME(ITEMP),NROW,NCOL,K,INVDF,
     +                        IOUT)
			ENDDO
		ENDIF
C		IF INDENSE EQUAL TO 2, THEN CONVERT DENSITY ARRAY USING EQUATION OF STATE
		IF (INDENSE.EQ.2) THEN
			DO K=1,NLAY
			DO I=1,NROW
			DO J=1,NCOL
				IF(IBOUND(J,I,K).NE.0)
     &				PS(J,I,K)=CALCDENS(PS(J,I,K))
			ENDDO
			ENDDO
			ENDDO
		ENDIF
      ELSE
C         SET DENSITY ARRAY USING CONCENTRATIONS FROM MT3D
          PS=DENSEREF
          DO K=1,NLAY
          DO I=1,NROW
          DO J=1,NCOL
             IF(CNEW(J,I,K,MTDNCONC).NE.CINACT) 
     &           PS(J,I,K)=CALCDENS(CNEW(J,I,K,MTDNCONC))
          ENDDO
          ENDDO
          ENDDO
      ENDIF

C     RECALCULATE HNEW AS EQUIVALENT FRESHWATER HEAD USING PS
      DO K=1,NLAY
      DO I=1,NROW
      DO J=1,NCOL
       IF(IBOUND(J,I,K).EQ.0) GOTO 20
       HNEW(J,I,K)=FEHEAD(HSALT(J,I,K),PS(J,I,K),ELEV(J,I,K))
   20 ENDDO
      ENDDO
      ENDDO
  500 FORMAT(1X,'INDENSE VALUE SPECIFIED AS:',I4)
  510 FORMAT(1X,'VALUES FOR DENSE ARRAY WILL BE REUSED OR SET TO DENSERE
     +F')
  520 FORMAT(1X,'VALUES FOR THE DENSE ARRAY WILL BE SET TO DENSEREF')
  530 FORMAT(1X,'VALUES FOR THE DENSE ARRAY WILL BE READ FROM VDF FILE')
  540 FORMAT(1X,'VALUES READ AS CONCENTRATION WILL BE CONVERTED TO FLUID
     + DENSITY USING EQUATION OF STATE')
      RETURN
      END

      SUBROUTINE VDF1FM(NCOL,NROW,NLAY,CR,CC,CV,IBOUND,PS,ELEV,RHS,
     &                 HCOF,HNEW,DELR,DELC,BOTM,NBOTM,MFNADVFD,RHOCR,
     &                 RHOCC,RHOCV,DCDT,MTDNCONC,HSALT,IWTABLE,IUNITBCF)
******************************************************************************
C--SEAWAT: CALCULATE DENSITY TERMS THAT ARE SUBTRACTED FROM THE RHS ACCUMULATOR
C--SEAWAT: CALCULATE RHOCR, RHOCC, AND RHOCV
C--SEAWAT: CALCULATE WATER TABLE CORRECTIONS
******************************************************************************  
      DIMENSION CR(NCOL,NROW,NLAY),CC(NCOL,NROW,NLAY),CV(NCOL,NROW,NLAY)
     &          ,IBOUND(NCOL,NROW,NLAY),PS(NCOL,NROW,NLAY),
     &           ELEV(NCOL,NROW,NLAY),RHS(NCOL,NROW,NLAY),DELR(NCOL),
     &           DELC(NROW),BOTM(NCOL,NROW,0:NBOTM),
     &           RHOCR(NCOL,NROW,NLAY),RHOCC(NCOL,NROW,NLAY),
     &           RHOCV(NCOL,NROW,NLAY),DCDT(NCOL,NROW,NLAY),
     &           HCOF(NCOL,NROW,NLAY)
C--SEAWAT: ADD HSALT, WTCORRECTION (H1,HS1,H2,HS2)
      DOUBLE PRECISION HNEW(NCOL,NROW,NLAY),HSALT(NCOL,NROW,NLAY),
     &                 H1,HS1,H2,HS2
	INCLUDE 'vdf.inc'
C--SWT FOR WT TERMS
      LOGICAL CORRECT
      COMMON /BCFCOM/LAYCON(200)
      COMMON /LPFCOM/LAYTYP(200),LAYAVG(200),CHANI(200),LAYVKA(200),
     1               LAYWET(200)
      COMMON /HUFCOM/LTHUF(200),HGUHANI(200),HGUVANI(200),LAYWT(200)

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
              IF(IUNITBCF.EQ.0.AND.LAYTYP(K).NE.0)
     &         CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                   H2,HS2,PS2,Z2,TOP2,BOT2)
C--SEAWAT: WTCORR-HUF
              IF(IUNITBCF.EQ.0.AND.LTHUF(K).NE.0)
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
              IF(IUNITBCF.EQ.0.AND.LAYTYP(K).NE.0)
     &         CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                   H2,HS2,PS2,Z2,TOP2,BOT2)
C--SEAWAT: WTCORR-HUF
              IF(IUNITBCF.EQ.0.AND.LTHUF(K).NE.0)
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
              IF(IUNITBCF.EQ.0.AND.LAYTYP(K).NE.0)
     &         CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                   H2,HS2,PS2,Z2,TOP2,BOT2)
C--SEAWAT: WTCORR-HUF
              IF(IUNITBCF.EQ.0.AND.LTHUF(K).NE.0)
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
              IF(IUNITBCF.EQ.0.AND.LAYTYP(K).NE.0)
     &          CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                   H2,HS2,PS2,Z2,TOP2,BOT2)
C--SEAWAT: WTCORR-HUF
              IF(IUNITBCF.EQ.0.AND.LTHUF(K).NE.0)
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
              IF (FLOWDIR.GT.0.) THEN
                 D5=D5*PS(J,I,K-1)
              ELSE
                 D5=D5*PS(J,I,K)
			ENDIF
            ENDIF
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
             IF (FLOWDIR.GT.0.) THEN
                D6=D6*PS(J,I,K+1)
             ELSE
                D6=D6*PS(J,I,K)
             ENDIF
            ENDIF
   60      CONTINUE
C--SEAWAT: SUBTRACT DENSITY TERMS AND DCDT FROM RHS ACCUMULATOR
           RHS(J,I,K)=RHS(J,I,K)-D1-D2-D3-D4-D5-D6
		 IF(MTDNCONC.GT.0) RHS(J,I,K)=RHS(J,I,K)+DCDT(J,I,K)
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
          IF(IUNITBCF.EQ.0.AND.LAYTYP(K).NE.0)
     &      CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                   H2,HS2,PS2,Z2,TOP2,BOT2)
C--SEAWAT: WTCORR-HUF
          IF(IUNITBCF.EQ.0.AND.LTHUF(K).NE.0)
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
          IF(IUNITBCF.EQ.0.AND.LAYTYP(K).NE.0)
     &      CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                   H2,HS2,PS2,Z2,TOP2,BOT2)
C--SEAWAT: WTCORR-HUF
          IF(IUNITBCF.EQ.0.AND.LTHUF(K).NE.0)
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


	SUBROUTINE VDF1HSALT(NCOL,NROW,NLAY,HNEW,HSALT,PS,ELEV,IBOUND)
C **********************************************************************
C THIS SUBROUTINE UPDATES THE HSALT ARRAY
C 
C***********************************************************************
C CREATED 3/12/03
C
	DIMENSION HNEW(NCOL,NROW,NLAY),HSALT(NCOL,NROW,NLAY),
     +          PS(NCOL,NROW,NLAY),ELEV(NCOL,NROW,NLAY),
     +          IBOUND(NCOL,NROW,NLAY)
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


      SUBROUTINE VDF1PS(NCOL,NROW,NLAY,NCOMP,CNEW,CINACT,PS,IOUT,
     &                 MTDNCONC)
C **********************************************************************
C THIS SUBROUTINE CALCULATES THE PS ARRAY
C 
C***********************************************************************
C CREATED 7/21/99
C
C       IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION PS(NCOL,NROW,NLAY),CNEW(NCOL,NROW,NLAY,NCOMP)
	INCLUDE 'vdf.inc'
C-----------------------------------------------------------------------
      IF(MTDNCONC.EQ.0) RETURN
        CONCMIN=0.

      DO K=1,NLAY
      DO I=1,NROW
      DO J=1,NCOL
         PS(J,I,K)=CNEW(J,I,K,MTDNCONC)
         IF (PS(J,I,K).EQ.CINACT) PS(J,I,K)=0.
         PS(J,I,K)=CALCDENS(PS(J,I,K))
         IF (PS(J,I,K).LT.DENSEREF) THEN
           IF (CNEW(J,I,K,MTDNCONC).LT.CONCMIN) THEN
                CONCMIN=CNEW(J,I,K,MTDNCONC)
              JMIN=J
              IMIN=I
              KMIN=K
           ENDIF
         ENDIF
      ENDDO
      ENDDO
      ENDDO
      IF (CONCMIN.LT.0) WRITE(IOUT,100) CONCMIN,IMIN,JMIN,KMIN
  100   FORMAT(1H0,'WARNING: NUMERICAL DISPERSION ENCOUNTERED'/
     &        1X,'ONE OR MORE CELL CONCENTRATIONS LESS THAN ZERO'/
     &        1X,'LARGEST NEGATIVE CONCENTRATION = ',E10.3/
     &        1X,'AT CELL (I,J,K) = (',I4,',',I4,',',I4,')'/)
      RETURN
      END

      SUBROUTINE VDF1DC(NCOL,NROW,NLAY,NCOMP,COLD,CNEW,PRSITY,
     &  DELR,DELC,DH,DCDT,DELT,MTDNCONC,IBOUND,PS,CINACT)
C **********************************************************************
C THIS SUBROUTINE CALCULATES THE DCDT ARRAY
C 
C***********************************************************************
C CREATED 2/14/2002 CDL
C

      DIMENSION COLD(NCOL,NROW,NLAY,NCOMP),CNEW(NCOL,NROW,NLAY,NCOMP),
     &  PRSITY(NCOL,NROW,NLAY),DELR(NCOL),DELC(NROW),
     &  DH(NCOL,NROW,NLAY),DCDT(NCOL,NROW,NLAY),
     &  IBOUND(NCOL,NROW,NLAY),PS(NCOL,NROW,NLAY)

      REAL CINACT
	INCLUDE 'vdf.inc'

C--SEAWAT: DCDT EQUALS ZERO IF CONCENTRATIONS DO NOT CHANGE
	DO K=1,NLAY
      DO I=1,NROW
      DO J=1,NCOL
		DCDT(J,I,K)=0.
	ENDDO
	ENDDO
	ENDDO
      IF(MTDNCONC.EQ.0) RETURN
      DO K=1,NLAY
      DO I=1,NROW
      DO J=1,NCOL
	   IF(IBOUND(J,I,K).LE.0) GOTO 10
         IF(CNEW(J,I,K,MTDNCONC).EQ.CINACT.OR.
     &      COLD(J,I,K,MTDNCONC).EQ.CINACT) GOTO 10
         DCDT(J,I,K)=PRSITY(J,I,K)*
     &       (CALCDENS(CNEW(J,I,K,MTDNCONC))-
     &       CALCDENS(COLD(J,I,K,MTDNCONC)))/
     &       DELT*DELR(J)*DELC(I)*DH(J,I,K)
   10 ENDDO
      ENDDO
      ENDDO
      RETURN
      END

      SUBROUTINE VDF1BD(VBNM,VBVL,MSUM,IBOUND,DELT,NCOL,NROW,NLAY,
     &  KSTP,KPER,IBCFCB,ILPFCB,IHUFCB,ICBCFL,BUFF,IOUT,PERTIM,
     &  TOTIM,DCDT,MTDNCONC,PS)
C-----VERSION  9OCT1999 SWTBD
C     ******************************************************************
C     SUM THE DCDT TERMS FOR BUDGET
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
C       IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      CHARACTER*4 VBNM,TEXT(4)
      DIMENSION IBOUND(NCOL,NROW,NLAY),VBNM(4,20),VBVL(4,20),
     +  BUFF(NCOL,NROW,NLAY),PS(NCOL,NROW,NLAY),DCDT(NCOL,NROW,NLAY)
      DATA TEXT(1),TEXT(2),TEXT(3),TEXT(4) /'    ','    ','    ','DCDT'/
C     ------------------------------------------------------------------
C1--INITIALIZE BUDGET ACCUMULATORS
        ZERO=0.
        DCDTIN=ZERO
        DCDTOUT=ZERO
C
C--SEAWAT: EXIT IF MT3D NOT USED (DCDT EQUAL TO ZERO)
      IF (MTDNCONC.EQ.0) GOTO 500

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
      VBVL(3,MSUM)=DCDTIN/DELT
      VBVL(4,MSUM)=DCDTOUT/DELT
      VBNM(1,MSUM)=TEXT(1)
      VBNM(2,MSUM)=TEXT(2)
      VBNM(3,MSUM)=TEXT(3)
      VBNM(4,MSUM)=TEXT(4)
      MSUM=MSUM+1

C7--RETURN
      RETURN
      END     

      SUBROUTINE SWTCOUPL(NCOL,NROW,NLAY,ICBUND,M,PSOLD,CNEW,DONE,
     +           DNSCRIT,IOUT)
C     MODIFIED 10/04/2002 BY CDL
C--SEAWAT:***********************************************************
C--SEAWAT:LOGIC FOR IMPLICT COUPLING 
C--SEAWAT:**********************************************************

      DIMENSION ICBUND(NCOL,NROW,NLAY),PSOLD(NCOL,NROW,NLAY),
     +          CNEW(NCOL,NROW,NLAY)
      LOGICAL DONE
C----------------------------------------------------------------       
      DIFFMAX=0.0
      DO I=1,NROW
      DO J=1,NCOL
      DO K=1,NLAY
		IF (ICBUND(J,I,K).GT.0) THEN
			DIFF=CALCDENS(CNEW(J,I,K))-PSOLD(J,I,K)
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
	WRITE(IOUT,*) 
	WRITE(IOUT,*) '__________________________________________________'
      WRITE(IOUT,100) M,DIFFMAX,ICOL,JROW,KLAY
  100 FORMAT(1H0,'COMPLETED COUPLING ITERATION ',I5/
     +       1X,'MAXIMUM DENSITY DIFFERENCE = ',G20.5/
     +       1X,'AT CELL (I,J,K) = (',I4,',',I4,',',I4,')'/
     +       1X,'__________________________________________________'/)
      RETURN
      END

      FUNCTION SSMDENSE(J,I,K,NTYPE,MXSS,NSS,SS,NCOMP,SSMC,MTDNCONC)
C************************************************************************
C   FUNCTION SSMDENSE
C   MODIFIED 11/25/01 BY CDL
C--SEAWAT: ADD DENSITY LIMITERS
C   FUNCTION RETURNS THE FLUID DENSITY FOR THE SPECIFIED MT3DMS BOUNDARY 
C************************************************************************
      DIMENSION SS(6,MXSS),SSMC(NCOMP,MXSS)
	INCLUDE 'vdf.inc'

      SSMDENSE=DENSEREF
      IF (NSS.LE.0) RETURN
       DO 10 NS=1,NSS
          ITYPE=SS(6,NS)
          KK=SS(1,NS)
          II=SS(2,NS)
          JJ=SS(3,NS)
          IF(ITYPE.EQ.NTYPE.AND.KK.EQ.K.AND.II.EQ.I.AND.JJ.EQ.J) THEN
            SSMDENSE=CALCDENS(SSMC(MTDNCONC,NS))

            GOTO 20
          ENDIF
   10 CONTINUE
   20 CONTINUE
      END

      SUBROUTINE CNVT2SH(NCOL,NROW,NLAY,HNEW,PS,ELEV)
C********************************************************
C--SEAWAT: SEND SALTHEADS TO OUTPUT IF INVDF.GT.0
C--SEAWAT: CREATED 3/26/02
C********************************************************
      DOUBLE PRECISION HNEW(NCOL,NROW,NLAY)
      DIMENSION PS(NCOL,NROW,NLAY),ELEV(NCOL,NROW,NLAY)

      DO K=1,NLAY
      DO I=1,NROW
      DO J=1,NCOL
        HNEW(J,I,K)=SALTHEAD(HNEW(J,I,K),PS(J,I,K),ELEV(J,I,K))
      ENDDO
      ENDDO
      ENDDO

      RETURN
      END

      SUBROUTINE CNVT2FE(NCOL,NROW,NLAY,HNEW,PS,ELEV)
C********************************************************
C--SEAWAT: CONVERT BACK TO FE HEADS AFTER OUTPUT IF 
C          INVDF.GT.0
C CREATED 3/26/02
C********************************************************
      DOUBLE PRECISION HNEW(NCOL,NROW,NLAY)
      DIMENSION PS(NCOL,NROW,NLAY),ELEV(NCOL,NROW,NLAY)

      DO K=1,NLAY
      DO I=1,NROW
      DO J=1,NCOL
       HNEW(J,I,K)=FEHEAD(HNEW(J,I,K),PS(J,I,K),ELEV(J,I,K))
      ENDDO
      ENDDO
      ENDDO

      RETURN
      END
C--SEAWAT: SALTHEAD FUNCTION
      FUNCTION SALTHEAD(HF,DENSE,ELEV)
C************************************************************************
C   FUNCTION SALTHEAD
C   MODIFIED 11/25/01 BY CDL
C   MODIFIED 3/25/02 BY WBS
C   FUNCTION RETURNS SALTHEAD 
C************************************************************************
      DOUBLE PRECISION HF
	INCLUDE 'vdf.inc'
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
      DOUBLE PRECISION HS
	INCLUDE 'vdf.inc'
C------------------------------------------------------------------------
      FEHEAD=HS*DENSE/DENSEREF-(DENSE-DENSEREF)/DENSEREF*ELEV
      END

        
      FUNCTION CALCDENS(CONCENTRATION)
C************************************************************************
C   THIS FUNCTION CALCULATES FLUID DENSITY AS A FUNCTION OF CONCENTRATION
C************************************************************************
	INCLUDE 'vdf.inc'
C-----------------------------------------------------------------------
	TEMP=DENSEREF+DENSESLP*CONCENTRATION
      IF(DENSEMAX.NE.0.AND.TEMP.GT.DENSEMAX) THEN
		TEMP=DENSEMAX
	ENDIF
      IF(DENSEMIN.NE.0.AND.TEMP.LT.DENSEMIN) THEN
		TEMP=DENSEMIN
	ENDIF
	CALCDENS=TEMP
      RETURN
      END

c      FUNCTION CALCVISC(CONCENTRATION)
C************************************************************************
C   THIS FUNCTION CALCULATES FLUID VISCOSITY AS A FUNCTION OF CONCENTRATION
C************************************************************************
c	INCLUDE 'vdf.inc'
C-----------------------------------------------------------------------
C--SEAWAT: LINEAR EQUATION OF STATE
c      IF(IVSSTATE.EQ.1) 
c     &  CALCVISC=VISCREF+VISCSLP*CONCENTRATION
c      RETURN
c      END

      SUBROUTINE VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,H2,HS2,PS2,Z2,TOP2,
     &                    BOT2)
C************************************************************************
C       THIS SUBROUTINE CALCULATES NEW VALUES FOR Z1 AND H1
C       Z1 IS THE ELEVATION HALFWAY BETWEEN THE WATER TABLE AND THE CELL BOTTOM
C       H1 IS THE EQUIVALENT FRESHWATER HEAD AT Z1
C************************************************************************
C       IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION H1,HS1,H2,HS2
      LOGICAL CORRECT
	INCLUDE 'vdf.inc'
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
