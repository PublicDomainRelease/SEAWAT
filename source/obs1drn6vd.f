C=======================================================================
      SUBROUTINE OBS1DRN6FMVD(NQ,NQOB,NQCL,IQOB,QCLS,IBT,HNEW,NCOL,
     &                      NROW,
     &                      NLAY,IOUT,IBOUND,NHT,OBSNAM,H,TOFF,MXDRN,
     &                      NDRAIN,DRAI,WTQ,NDMH,ITS,NQAR,NQCAR,NQTAR,
     &                      NDRNVL,ND)
C     VERSION 19981020 ERB
C     ******************************************************************
C     CALCULATE SIMULATED EQUIVALENTS TO OBSERVED FLOWS FOR THE DRAIN
C     PACKAGE
C--SEAWAT: MODIFIED FOR VARIABLE DENSITY FLOW
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE VDFMODULE,   ONLY: DENSEREF,PS,ELEV,HSALT
C
      REAL C, DRAI, FACT, H, HB, QCLS, TOFF, WTQ, ZERO
      INTEGER I, IBOUND, IBT, IBT1, IFLAG, II, IOUT, IQ, IQOB, IRBOT,
     &        ITS, J, JJ, JRBOT, K, KK, KRBOT, MXDRN, N, NB, NBN, NC,
     &        NC1, NC2, NCOL, NDMH, NDRAIN, NHT, NLAY, NQ, NQCL, NQOB,
     &        NROW, NT, NT1, NT2
      CHARACTER*12 OBSNAM(ND)
      DOUBLE PRECISION HH, HHNEW, HNEW(NCOL,NROW,NLAY)
      DIMENSION IBOUND(NCOL,NROW,NLAY), IBT(2,NQAR), NQOB(NQAR),
     &          NQCL(NQAR), IQOB(NQTAR), QCLS(5,NQCAR), H(ND),
     &          TOFF(ND), DRAI(NDRNVL,MXDRN), WTQ(NDMH,NDMH)
CBARC--SEAWAT: ADD COMMON AND AUX. VARIABLES
      COMMON /DRNCOM/DRNAUX(5)
      CHARACTER*16 DRNAUX
      INCLUDE 'param.inc'
C     ------------------------------------------------------------------
  500 FORMAT (/,
     &' HEADS AT DRAIN CELLS ARE BELOW THE',
     &' BOTTOM OF THE DRAIN AT THE CELLS LISTED',/,
     &' BELOW.  THESE CONDITIONS DIMINISH THE IMPACT',
     &' OF THE OBSERVATION ON ESTIMATES',/,
     &' OF ALL PARAMETERS.  (SEE TEXT FOR MORE INFORMATION).')
  505 FORMAT (/,' OBS# ',I6,', ID ',A,', TIME STEP ',I5)
  510 FORMAT ('    LAYER   ROW  COLUMN')
  520 FORMAT (3I7)
  530 FORMAT (I7,' OF THE',I7,' REACHES OR CELLS USED TO SIMULATE THE',
     &        ' GAIN OR LOSS ARE',/,22X,'AFFECTED.')
  535 FORMAT (' ALL CELLS INCLUDED IN THIS OBSERVATION ARE INACTIVE.  ',
     &        'THE OBSERVATION WILL',/
     &        ,' BE OMITTED FROM THIS PARAMETER-ESTIMATION ITERATION')
  540 FORMAT (' CELL # ',I6,
     &        ' OF HEAD-DEP. BOUNDARY GAIN OR LOSS OBS# ',I6,' ID=',A,/,
     &        ' NOT FOUND IN CELLS LISTED FOR DRAIN PACKAGE',/,
     &        ' -- STOP EXECUTION (OBS1DRN6FM)')
C
C-------INITIALIZE VARIABLES
      ZERO = 0.0
      NC = 0
      NT1 = 1
      JRBOT = 0
C--SEAWAT: GET ZDRN IF AUX IS SPECIFIED
	LOCZDRN=0
	DO IC=1,5
	 IF(DRNAUX(IC).EQ.'DRNBELEV') LOCZDRN=IC+5
	ENDDO
C-------LOOP THROUGH BOUNDARY FLOWS
      DO 60 IQ = 1, NQ
        IBT1 = IBT(1,IQ)
        NT2 = NT1 + NQOB(IQ) - 1
        IF (IBT1.NE.4) GOTO 50
C----------WAS THERE A MEASUREMENT AT THIS BOUNDARY THIS TIME STEP?
        DO 40 NT = NT1, NT2
          IF (IQOB(NT).EQ.ITS .OR. 
     &        (IQOB(NT).EQ.ITS-1.AND.TOFF(NHT+NT).GT.ZERO)) THEN
C----------ASSIGN VARIABLES ACCORDING TO BOUNDARY TYPE
            IRBOT = 0
            KRBOT = 0
            NBN = NDRAIN
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
                KK = DRAI(1,NB)
                II = DRAI(2,NB)
                JJ = DRAI(3,NB)
C----------DO CALCULATIONS IF THIS IS A MATCH
                IF (I.EQ.II.AND.J.EQ.JJ.AND.K.EQ.KK) THEN
                  IFLAG = 1
                  IF (IBOUND(J,I,K).EQ.0) THEN
                    KRBOT = KRBOT + 1
                    GOTO 30
                  ENDIF
C-------------ASSIGN VARIABLE VALUES
                  HHNEW = HNEW(J,I,K)
                  HB = DRAI(4,NB)
CBARC--SEAWAT: ASSUME DENSITY OF DRAIN IS PS(J,I,K)
CBARC--SEAWAT: BUMP HB TO FRESHWATER EQUIVALENT
                  HB=FEHEAD(HB,PS(J,I,K),ELEV(J,I,K))
                  ZDRN=ELEV(J,I,K)
	            IF(LOCZDRN.GT.0) ZDRN=DRAI(LOCZDRN,NB)
                  C = DRAI(5,NB)
C-------------CALCULATE FLOWS
CBARC--USE VD FORM DARCY'S LAW
C                  HH = C*(HB-HHNEW)
                   HH=C*(HB-HHNEW-(DENSEREF-PS(J,I,K))/DENSEREF*
     &                (ELEV(J,I,K)-ZDRN))
CBARC--SALTHEAD COMPARISON
C				IF (HHNEW.LE.RBOT) THEN
				IF (HSALT(J,I,K).LE.HB) THEN
                    HH = 0.0
                    IF (JRBOT.EQ.0) WRITE (IOUT,500)
                    JRBOT = 1
                    IF (IRBOT.EQ.0) THEN
                      WRITE (IOUT,505) NHT + NT, OBSNAM(NHT+NT), ITS
                      WRITE (IOUT,510)
                    ENDIF
                    IRBOT = IRBOT + 1
                    WRITE (IOUT,520) K, I, J
                  ENDIF
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
C-------PRINT NUMBER OF CELLS AT WHICH HEAD IS BELOW THE BOTTOM OF THE
C-------DRAIN; CHECK FOR DISCONNECTED OBSERVATIONS.
            IF (IRBOT.GT.0) WRITE (IOUT,530) IRBOT, NQCL(IQ)
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