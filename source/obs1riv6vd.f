

C=======================================================================
      SUBROUTINE OBS1RIV6FMVD(NQ,NQOB,NQCL,IQOB,QCLS,IBT,MXRIVR,NRIVER
     &                      ,RIVR,HNEW,NCOL,NROW,NLAY,IOUT,IBOUND,
     &                      NHT,OBSNAM,H,TOFF,WTQ,NDMH,ITS,NQAR,NQCAR,
     &                      NQTAR,NRIVVL,ND,PS,ELEV,HSALT,MTDNCONC,
     &                      MXSS,NSS,SS,NCOMP,SSMC)
C     VERSION 19981020 ERB
C     ******************************************************************
C     CALCULATE SIMULATED EQUIVALENTS TO OBSERVED FLOWS FOR THE RIVER
C     PACKAGE
CBARC--SEAWAT: MODIFIED FOR VARIABLE DENSITY FLOW
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
CBARC--SEAWAT: ADD PS, ELEV
      REAL C, FACT, H, HB, HH, HHNEW, QCLS, RBOT, RIVR,
     &     TOFF, WTQ, ZERO, PS, ELEV
      INTEGER I, IBOUND, IBT, IBT1, IFLAG, II, IOUT, IQ,
     &        IQOB, IRBOT, ITS, J, JJ, JRBOT, K, KK, KRBOT,
     &        MXRIVR, N, NB, NBN, NC, NC1, NC2, NCOL, NDMH, NHT,
     &        NLAY, NQ, NQCL, NQOB, NRIVER, NROW, NT,
     &        NT1, NT2
      CHARACTER*12 OBSNAM(ND)
CBARC--SEAWAT: ADD HSALT
      DOUBLE PRECISION HNEW(NCOL,NROW,NLAY),HSALT(NCOL,NROW,NLAY)
CBARC--SEAWAT: ADD PS,ELEV
      DIMENSION RIVR(NRIVVL,MXRIVR), IBOUND(NCOL,NROW,NLAY),
     &          IBT(2,NQAR), NQOB(NQAR), NQCL(NQAR), IQOB(NQTAR),
     &          QCLS(5,NQCAR), H(ND), TOFF(ND), WTQ(NDMH,NDMH),
     &          PS(NCOL,NROW,NLAY),ELEV(NCOL,NROW,NLAY)
CBARC--SEAWAT: ADD COMMON AND AUX. VARIABLES
      COMMON /RIVCOM/RIVAUX(5)
      CHARACTER*16 RIVAUX
	INCLUDE 'vdf.inc'
CBARC--SEAWAT:MT3DMS 
	DIMENSION SS(6,MXSS),SSMC(NCOMP,MXSS)

      INCLUDE 'param.inc'
C     ------------------------------------------------------------------
  500 FORMAT (/,
     &' HEADS AT RIVER CELLS ARE BELOW THE',
     &' BOTTOM OF THE RIVER BED AT THE CELLS LISTED',/,
     &' BELOW.  THESE CONDITIONS DIMINISH THE IMPACT',
     &' OF THE OBSERVATION ON ESTIMATES OF',/,
     &' ALL PARAMETERS EXCEPT THOSE THAT CONTROL THE HYDRAULIC',
     &' CONDUCTIVITY OF THE',/,
     &' RIVER BED.  (SEE TEXT FOR MORE INFORMATION).')
  505 FORMAT (/,' OBS# ',I6,', ID ',A,', TIME STEP ',I5)
  510 FORMAT ('    LAYER   ROW  COLUMN')
  520 FORMAT (3I7)
  530 FORMAT (I7,' OF THE',I7,' CELLS USED TO SIMULATE THE',
     &        ' GAIN OR LOSS ARE',/,22X,'AFFECTED.')
  535 FORMAT (' ALL CELLS INCLUDED IN THIS OBSERVATION ARE INACTIVE.  ',
     &        'THE OBSERVATION WILL',/
     &        ,' BE OMITTED FROM THIS PARAMETER-ESTIMATION ITERATION')
  540 FORMAT (' CELL OR REACH # ',I6,
     &        ' OF HEAD-DEP. BOUNDARY GAIN OR LOSS OBS#',I5,' ID=',A,/,
     &        ' NOT FOUND IN CELLS LISTED FOR RIVER PACKAGE',/,
     &        ' -- STOP EXECUTION (OBS1RIV6FM)')
C
C-------INITIALIZE VARIABLES
      ZERO = 0.0
      NC = 0
      NT1 = 1
      JRBOT = 0
C-------LOOP THROUGH BOUNDARY FLOWS
      DO 60 IQ = 1, NQ
        IBT1 = IBT(1,IQ)
        NT2 = NT1 + NQOB(IQ) - 1
        IF (IBT1.NE.1) GOTO 50
C----------WAS THERE A MEASUREMENT AT THIS BOUNDARY THIS TIME STEP?
        DO 40 NT = NT1, NT2
          IF (IQOB(NT).EQ.ITS .OR. 
     &        (IQOB(NT).EQ.ITS-1.AND.TOFF(NHT+NT).GT.ZERO)) THEN
C----------ASSIGN VARIABLES ACCORDING TO BOUNDARY TYPE
            IRBOT = 0
            KRBOT = 0
            NBN = NRIVER
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
                KK = RIVR(1,NB)
                II = RIVR(2,NB)
                JJ = RIVR(3,NB)
C----------DO CALCULATIONS IF THIS IS A MATCH
                IF (I.EQ.II.AND.J.EQ.JJ.AND.K.EQ.KK) THEN
                  IFLAG = 1
                  IF (IBOUND(J,I,K).EQ.0) THEN
                    KRBOT = KRBOT + 1
                    GOTO 30
                  ENDIF
C-------------ASSIGN VARIABLE VALUES
                  HHNEW = HNEW(J,I,K)
                  HB = RIVR(4,NB)
                  C = RIVR(5,NB)
                  RBOT = RIVR(6,NB)
CBARC--SEAWAT:SET RIVER BED THICKNESS
	            LOCRBDTHK=0
	            DO IC=1,5
	             IF(RIVAUX(IC).EQ.'RBDTHK') LOCRBDTHK=IC+6
	            ENDDO   
C	            RBDTHK=1.0
	            RBDTHK=ABS(RBOT-ELEV(J,I,K))
	            IF(LOCRBDTHK.GT.0) RBDTHK=RIVR(LOCRBDTHK,NB)
CBARC--SEAWAT: SET RIVER DENSITY
                  IF(MTDNCONC.EQ.0) THEN
	             LOCRIVDEN=0
	             DO IC=1,5
	              IF(RIVAUX(IC).EQ.'RIVDEN') LOCRIVDEN=IC+6
	             ENDDO   
	             RIVDENS=PS(J,I,K)
	             IF(LOCRIVDEN.GT.0) RIVDENS=RIVR(LOCRIVDEN,NB)
	            ELSE
	             IF(MTDNCONC.GT.0) RIVDENS=SSMDENSE(J,I,K,4,MXSS,
     &                           NSS,SS,NCOMP,SSMC,MTDNCONC)	
					PRINT*, RIVDENS
				ENDIF
CBARC--SEAWAT: COMPUTE RHOAVG
                  RHOAVG=(RIVDENS+PS(J,I,K))/2
CBARC--SEAWAT: COMPUTE HB AS FE 
                  HB=FEHEAD(HB,RIVDENS,RBOT)
C-------------CALCULATE FLOWS
CBARC--SEAWAT: USE VD FORM DARCY LAW
C                  HH = C*(HB-HHNEW)
                  HH=C*(HB-HNEW(J,I,K)-(PS(J,I,K)-DENSEREF)/DENSEREF*
     &             (ELEV(J,I,K)-RBOT)+(RHOAVG-DENSEREF)/DENSEREF*RBDTHK)
CBARC--SEAWAT: USE SALTHEAD
C                  IF (HHNEW.LE.RBOT) THEN
                  IF (HSALT(J,I,K).LE.RBOT) THEN
C                    HH = C*(HB-RBOT)
                   HH=C*(HB-RBOT+(RIVDENS-DENSEREF)/DENSEREF*RBDTHK)

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
C-------RIVER BED; CHECK FOR DISCONNECTED OBSERVATIONS.
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