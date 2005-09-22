

C=======================================================================
      SUBROUTINE OBS1GHB6FMVD(NQ,NQOB,NQCL,IQOB,QCLS,IBT,MXBND,NBOUND
     &                      ,BNDS,HNEW,NCOL,NROW,NLAY,IOUT,IBOUND,NHT,
     &                      OBSNAM,H,TOFF,ITS,NQAR,NQCAR,NQTAR,NGHBVL,
     &                      ND,WTQ,NDMH,PS,ELEV,HSALT,MTDNCONC,MXSS,NSS,
     &                      SS,NCOMP,SSMC)
C     VERSION 19981020 ERB
C     ******************************************************************
C     CALCULATE SIMULATED EQUIVALENTS TO OBSERVED FLOWS FOR THE GENERAL 
C     HEAD BOUNDARY PACKAGE
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
CBARC--SEAWAT: ADD PS,ELEV
      REAL BNDS, C, FACT, H, HB, QCLS, TOFF, ZERO, PS, ELEV

      INTEGER I, IBOUND, IBT, IBT1, IFLAG, II, IOUT, IQ, IQOB, ITS, J,
     &        JJ, K, KK, MXBND, N, NB, NBN, NBOUND, NC, NC1, NC2, NCOL,
     &        NHT, NLAY, NQ, NQCL, NQOB, NROW, NT, NT1, NT2
      CHARACTER*12 OBSNAM(ND)
CBARC--SEAWAT: ADD HSALT
      DOUBLE PRECISION HH, HHNEW,HNEW(NCOL,NROW,NLAY),
     &                 HSALT(NCOL,NROW,NLAY)
CBARC--SEAWAT: ADD PS,ELEV
      DIMENSION BNDS(NGHBVL,MXBND), IBOUND(NCOL,NROW,NLAY), IBT(2,NQAR),
     &          NQOB(NQAR), NQCL(NQAR), IQOB(NQTAR), QCLS(5,NQCAR),
     &          H(ND), TOFF(ND), WTQ(NDMH,NDMH),
     &          PS(NCOL,NROW,NLAY),ELEV(NCOL,NROW,NLAY)
CBARC--SEAWAT: ADD COMMON AND AUX. VARIABLES
	INCLUDE 'vdf.inc'
      COMMON /GHBCOM/GHBAUX(5)
      CHARACTER*16 GHBAUX
CBARC--SEAWAT:MT3DMS 
	DIMENSION SS(7,MXSS),SSMC(NCOMP,MXSS)

      INCLUDE 'param.inc'
C     ------------------------------------------------------------------
  505 FORMAT (/,' OBS#',I6,', ID ',A4,', TIME STEP ',I5)
  520 FORMAT (3I7)
  535 FORMAT (' ALL CELLS INCLUDED IN THIS OBSERVATION ARE INACTIVE.  ',
     &        'THE OBSERVATION WILL',/
     &        ,' BE OMITTED FROM THIS PARAMETER-ESTIMATION ITERATION')
  540 FORMAT (' CELL # ',I6,
     &        ' OF HEAD-DEP. BOUNDARY GAIN OR LOSS OBS# ',I6,' ID=',A,/,
     &        ' NOT FOUND IN CELLS LISTED FOR GHB PACKAGE',/,
     &        ' -- STOP EXECUTION (OBS1GHB6FM)')
C
C-------INITIALIZE VARIABLES
      ZERO = 0.0
      NC = 0
      NT1 = 1
C-------LOOP THROUGH BOUNDARY FLOWS
      DO 60 IQ = 1, NQ
        IBT1 = IBT(1,IQ)
        NT2 = NT1 + NQOB(IQ) - 1
        IF (IBT1.NE.2) GOTO 50
C----------WAS THERE A MEASUREMENT AT THIS BOUNDARY THIS TIME STEP?
        DO 40 NT = NT1, NT2
          IF (IQOB(NT).EQ.ITS .OR. 
     &        (IQOB(NT).EQ.ITS-1.AND.TOFF(NHT+NT).GT.ZERO)) THEN
C----------ASSIGN VARIABLES ACCORDING TO BOUNDARY TYPE
	      KRBOT=0
            NBN = NBOUND
C----------LOOP THROUGH CELLS.
            NC1 = NC + 1
            NC2 = NC + NQCL(IQ)
            NB = 0
            DO 30 N = NC1, NC2
              K = QCLS(1,N)
              I = QCLS(2,N)
              J = QCLS(3,N)
C----------LOOP THROUGH DATA FILE TO FIND A MATCH.
              IFLAG = 0
              DO 10 MNB = 1, NBN
                NB = NB + 1
                IF (NB.GT.NBN) NB = 1
                KK = BNDS(1,NB)
                II = BNDS(2,NB)
                JJ = BNDS(3,NB)
C----------DO CALCULATIONS IF THIS IS A MATCH
                IF (I.EQ.II.AND.J.EQ.JJ.AND.K.EQ.KK) THEN
                  IFLAG = 1
                  IF (IBOUND(J,I,K).EQ.0) THEN
                    KRBOT = KRBOT + 1
                    GOTO 30
                  ENDIF
C-------------ASSIGN VARIABLE VALUES
                  HHNEW = HNEW(J,I,K)
                  HB = BNDS(4,NB)
CBARC--SEAWAT: BUMP HB TO FE
                  HB=FEHEAD(HB,PS(J,I,K),ELEV(J,I,K))
                  C = BNDS(5,NB)

CBARC--SEAWAT: SET GHBELEV
	           LOCGHBELEV=0
	           DO IC=1,5
	            IF(GHBAUX(IC).EQ.'GHBELEV') LOCGHBELEV=IC+5
	           ENDDO 
                 GHBELEV=ELEV(J,I,K)
	           IF(LOCGHBELEV.GT.0) GHBELEV=BNDS(LOCGHBELEV,NB)
CBARC--SEAWAT: SET GHBDENS 
                 IF(MTDNCONC.EQ.0) THEN
	            LOCGHBDENS=0
	            DO IC=1,5
	             IF(GHBAUX(IC).EQ.'GHBDENS') LOCGHBDENS=IC+5
	            ENDDO 
	            GHBDENS=PS(J,I,K)
	            IF(LOCGHBDENS.GT.0) GHBDENS=BNDS(LOCGHBDENS,NB)
CBARC--SET GHBDENS USING SSM PACKAGE
	           ELSE
	            IF(MTDNCONC.GT.0) GHBDENS=SSMDENSE(J,I,K,5,MXSS,NSS,
     &                           SS,NCOMP,SSMC,MTDNCONC)
                 ENDIF
CBARC--SEAWAT: SET RHOAVG
	          RHOAVG=(PS(J,I,K)+GHBDENS)/2
C-------------CALCULATE FLOWS
CBARC--SEAWAT: USE VD FORM DARCY'S LAW
C                  HH = C*(HB-HHNEW)
                   HH=C*(HB-HHNEW+(RHOAVG-DENSEREF)/DENSEREF*
     &                (GHBELEV-ELEV(J,I,K)))	
                  GOTO 20
                ENDIF
   10         CONTINUE
              IF (IFLAG.EQ.0) THEN
                WRITE (IOUT,540) N, NHT + NT, OBSNAM(NHT+NT)
                CALL USTOP(' ')
              ENDIF
C-------------SUM VALUES FROM INDIVIDUAL CELLS.
C----------------CALCULATE FACTOR FOR TEMPORAL INTERPOLATION
   20         FACT = 1.0
              IF (TOFF(NHT+NT).GT.ZERO) THEN
                IF (IQOB(NT).EQ.ITS) FACT = 1. - TOFF(NHT+NT)
                IF (IQOB(NT).EQ.ITS-1) FACT = TOFF(NHT+NT)
              ENDIF
C---------------FLOWS
              H(NHT+NT) = H(NHT+NT) + HH*FACT*QCLS(4,N)
   30       CONTINUE
C-----------CHECK FOR DISCONNECTED OBSERVATIONS
            IF (KRBOT.EQ.NQCL(IQ)) THEN
              WTQ(NT,NT) = -WTQ(NT,NT)
              WRITE (IOUT,535)
            ENDIF
		ENDIF
   40   CONTINUE
C-------UPDATE COUNTERS
   50   NC = NC + NQCL(IQ)
        NT1 = NT2 + 1
C
   60 CONTINUE
      RETURN
      END