C=======================================================================
      SUBROUTINE OBS1BAS6FFMVD(NQ,NQOB,NQCL,IQOB,QCLS,IBT,HNEW,NCOL,
     &                       NROW,NLAY,IBOUND,NHT,H,TOFF,ITS,NQAR,NQCAR,
     &                       NQTAR,ICHFLG,CR,CC,CV,BOTM,NBOTM,LAYHDT,ND,
     &                       IOUT,KKPER,DELR,DELC)
C     VERSION 20011114 ERB
C     ******************************************************************
C     CALCULATE SIMULATED EQUIVALENTS TO OBSERVED CONSTANT-HEAD FLOWS
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE VDFMODULE,   ONLY: PS,ELEV
C
      REAL FACT, H, QCLS, TOFF, ZERO
      INTEGER I, IBOUND, IBT, IBT1, IOUT, IQ, IQOB, ITS, J, K, LAYHDT,
     &        N, NC, NC1, NC2, NCOL, NHT, NLAY, NQ, NQCL, NQOB, NROW,
     &        NT, NT1, NT2
      DOUBLE PRECISION HNEW(NCOL,NROW,NLAY),RATE
      DIMENSION IBOUND(NCOL,NROW,NLAY), IBT(2,NQAR), NQOB(NQAR),
     &          NQCL(NQAR), IQOB(NQTAR), QCLS(5,NQCAR), H(ND), TOFF(ND),
     &          CR(NCOL,NROW,NLAY), CC(NCOL,NROW,NLAY),
     &          CV(NCOL,NROW,NLAY), BOTM(NCOL,NROW,0:NBOTM),
     &          LAYHDT(NLAY)
C--SEAWAT: DIMENSION DELR,DELC
      DIMENSION DELR(NCOL),DELC(NROW)
      INCLUDE 'param.inc'
C     ------------------------------------------------------------------
  500 FORMAT(/,
     &' *** ERROR: CONSTANT-HEAD FLOW OBSERVATION SPECIFIED FOR CELL (',
     &I3,',',I5,',',I5,'),',/,
     &12X,'BUT THIS CELL IS NOT CONSTANT-HEAD IN STRESS PERIOD ',I4,/
     &12X,'-- STOP EXECUTION (OBS1BAS6FFM)')
C
C-------INITIALIZE VARIABLES
      ZERO = 0.0
      NC = 0
      NT1 = 1
C-------LOOP THROUGH BOUNDARY FLOWS
      DO 60 IQ = 1, NQ
        IBT1 = IBT(1,IQ)
        NT2 = NT1 + NQOB(IQ) - 1
        IF (IBT1.NE.5) GOTO 50
C----------WAS THERE A MEASUREMENT AT THIS BOUNDARY THIS TIME STEP?
        DO 40 NT = NT1, NT2
          IF (IQOB(NT).EQ.ITS .OR.
     &        (IQOB(NT).EQ.ITS-1.AND.TOFF(NHT+NT).GT.ZERO)) THEN
C----------LOOP THROUGH CELLS.
            NC1 = NC + 1
            NC2 = NC + NQCL(IQ)
            DO 30 N = NC1, NC2
              K = QCLS(1,N)
              I = QCLS(2,N)
              J = QCLS(3,N)
              IF (IBOUND(J,I,K).GE.0) THEN
                WRITE(IOUT,500) K,I,J,KKPER
                CALL USTOP(' ')
              ENDIF
C-------------CALL SUBROUTINE TO CALCULATE CONSTANT-HEAD FLOW FOR CELL
              CALL SOBS1BAS6FFLWVD(J,I,K,ICHFLG,IBOUND,HNEW,CR,CC,CV,
     &                          BOTM,NBOTM,NCOL,NROW,NLAY,RATE,LAYHDT,
     &                          DELR,DELC)
C-------------SUM VALUES FROM INDIVIDUAL CELLS.
C----------------CALCULATE FACTOR FOR TEMPORAL INTERPOLATION
   20         FACT = 1.0
              IF (TOFF(NHT+NT).GT.ZERO) THEN
                IF (IQOB(NT).EQ.ITS) FACT = 1. - TOFF(NHT+NT)
                IF (IQOB(NT).EQ.ITS-1) FACT = TOFF(NHT+NT)
              ENDIF
C---------------FLOWS
              H(NHT+NT) = H(NHT+NT) + RATE*FACT*QCLS(4,N)
   30       CONTINUE
          ENDIF
   40   CONTINUE
C-------UPDATE COUNTERS
   50   NC = NC + NQCL(IQ)
        NT1 = NT2 + 1
C
   60 CONTINUE
      RETURN
      END
C=======================================================================
      SUBROUTINE SOBS1BAS6FFLWVD(J,I,K,ICHFLG,IBOUND,HNEW,CR,CC,CV,
     &                        BOTM,NBOTM,NCOL,NROW,NLAY,RATE,LAYHDT,
     &                        DELR,DELC)
C     VERSION 20010924 ERB
C     ******************************************************************
C     CALCULATE CONSTANT-HEAD BOUNDARY FLOW FOR A GIVEN CELL
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE VDFMODULE,   ONLY: DENSEREF,PS,ELEV
c
      INTEGER LAYHDT
      DOUBLE PRECISION HNEW,HD,X1,X2,X3,X4,X5,X6,RATE
C
C--SEAWAT: ADD DELR,DELC
      DIMENSION HNEW(NCOL,NROW,NLAY), IBOUND(NCOL,NROW,NLAY),
     1     CR(NCOL,NROW,NLAY), CC(NCOL,NROW,NLAY),
     2     CV(NCOL,NROW,NLAY), BOTM(NCOL,NROW,0:NBOTM), LAYHDT(NLAY),
     3     DELR(NCOL),DELC(NROW)
C
      COMMON /DISCOM/LBOTM(999),LAYCBD(999)
C     ------------------------------------------------------------------
C
C6------CLEAR VALUES FOR FLOW RATE THROUGH EACH FACE OF CELL.
      ZERO=0.
      X1=ZERO
      X2=ZERO
      X3=ZERO
      X4=ZERO
      X5=ZERO
      X6=ZERO
C
C7------CALCULATE FLOW THROUGH THE LEFT FACE.
C7------COMMENTS A-C APPEAR ONLY IN THE SECTION HEADED BY COMMENT 7,
C7------BUT THEY APPLY IN A SIMILAR MANNER TO SECTIONS 8-12.
C
C7A-----IF THERE IS NO FLOW TO CALCULATE THROUGH THIS FACE, THEN GO ON
C7A-----TO NEXT FACE.  NO FLOW OCCURS AT THE EDGE OF THE GRID, TO AN
C7A-----ADJACENT NO-FLOW CELL, OR TO AN ADJACENT CONSTANT-HEAD CELL
C7A-----WHEN ICHFLG IS 0.
      IF(J.EQ.1) GO TO 30
      IF(IBOUND(J-1,I,K).EQ.0) GO TO 30
      IF(ICHFLG.EQ.0 .AND. IBOUND(J-1,I,K).LT.0) GO TO 30
C
C7B-----CALCULATE FLOW THROUGH THIS FACE INTO THE ADJACENT CELL.
CVDF***USE VD FORM OF DARCYS LAW
CVDF***POSITIVE FLOW TO LEFT (OUT OF CONSTANT HEAD CELL)
      DIS1=DELR(J-1)/2
      DIS2=DELR(J)/2
      AVGDENS=(DIS1*PS(J-1,I,K)+DIS2*PS(J,I,K))/(DIS1+DIS2)
      D1=CR(J-1,I,K)*(AVGDENS-DENSEREF)/DENSEREF*(ELEV(J-1,I,K)-
     &   ELEV(J,I,K))      
      HDIFF=HNEW(J-1,I,K)-HNEW(J,I,K)    
      X1=-1*(CR(J-1,I,K)*HDIFF+D1)
C      HDIFF=HNEW(J,I,K)-HNEW(J-1,I,K)
C      X1=HDIFF*CR(J-1,I,K)
C
C8------CALCULATE FLOW THROUGH THE RIGHT FACE.
   30 IF(J.EQ.NCOL) GO TO 60
      IF(IBOUND(J+1,I,K).EQ.0) GO TO 60
      IF(ICHFLG.EQ.0 .AND. IBOUND(J+1,I,K).LT.0) GO TO 60
CVDF***USE VD FORM OF DARCYS LAW
      DIS1=DELR(J+1)/2
      DIS2=DELR(J)/2
      AVGDENS=(DIS1*PS(J+1,I,K)+DIS2*PS(J,I,K))/(DIS1+DIS2)
      D2=CR(J,I,K)*(AVGDENS-DENSEREF)/DENSEREF*(ELEV(J+1,I,K)-
     &    ELEV(J,I,K))
      HDIFF=HNEW(J+1,I,K)-HNEW(J,I,K)
      X2=-1*(CR(J,I,K)*HDIFF+D2)
C      HDIFF=HNEW(J,I,K)-HNEW(J+1,I,K)
C      X2=HDIFF*CR(J,I,K)
C
C9------CALCULATE FLOW THROUGH THE BACK FACE.
   60 IF(I.EQ.1) GO TO 90
      IF (IBOUND(J,I-1,K).EQ.0) GO TO 90
      IF(ICHFLG.EQ.0 .AND. IBOUND(J,I-1,K).LT.0) GO TO 90
      DIS1=DELC(I-1)/2
      DIS2=DELC(I)/2
      AVGDENS=(DIS1*PS(J,I-1,K)+DIS2*PS(J,I,K))/(DIS1+DIS2)
      D3=CC(J,I-1,K)*(AVGDENS-DENSEREF)/DENSEREF*(ELEV(J,I-1,K)-
     &   ELEV(J,I,K))
      HDIFF=HNEW(J,I-1,K)-HNEW(J,I,K)
      X3=-1*(CC(J,I-1,K)*HDIFF+D3)
C      HDIFF=HNEW(J,I,K)-HNEW(J,I-1,K)
C      X3=HDIFF*CC(J,I-1,K)
C
C10-----CALCULATE FLOW THROUGH THE FRONT FACE.
   90 IF(I.EQ.NROW) GO TO 120
      IF(IBOUND(J,I+1,K).EQ.0) GO TO 120
      IF(ICHFLG.EQ.0 .AND. IBOUND(J,I+1,K).LT.0) GO TO 120
CVDF***USE VD FORM OF DARCYS LAW
      DIS1=DELC(I+1)/2
      DIS2=DELC(I)/2
      AVGDENS=(DIS1*PS(J,I+1,K)+DIS2*PS(J,I,K))/(DIS1+DIS2)
      D4=CC(J,I,K)*(AVGDENS-DENSEREF)/DENSEREF*(ELEV(J,I+1,K)-
     &    ELEV(J,I,K))
      HDIFF=HNEW(J,I+1,K)-HNEW(J,I,K)
      X4=-1*(CC(J,I,K)*HDIFF+D4)
C      HDIFF=HNEW(J,I,K)-HNEW(J,I+1,K)
C      X4=HDIFF*CC(J,I,K)
C
C11-----CALCULATE FLOW THROUGH THE UPPER FACE.
  120 IF(K.EQ.1) GO TO 150
      IF (IBOUND(J,I,K-1).EQ.0) GO TO 150
      IF(ICHFLG.EQ.0 .AND. IBOUND(J,I,K-1).LT.0) GO TO 150
      HD=HNEW(J,I,K)
      IF(LAYHDT(K).EQ.0) GO TO 122
      TMP=HD
      TOP=BOTM(J,I,LBOTM(K)-1)
      IF(TMP.LT.TOP) HD=TOP
  122 HDIFF=HNEW(J,I,K-1)-HD
      DIS1=ELEV(J,I,K-1)-BOTM(J,I,K-1)
      DIS2=BOTM(J,I,K-1)-ELEV(J,I,K)
      AVGDENS=(DIS1*PS(J,I,K-1)+DIS2*PS(J,I,K))/(DIS1+DIS2)
      D5=CV(J,I,K-1)*(AVGDENS-DENSEREF)/DENSEREF*(ELEV(J,I,K-1)-
     &     ELEV(J,I,K))
      X5=-1*(CV(J,I,K-1)*HDIFF+D5)
C  122 HDIFF=HD-HNEW(J,I,K-1)
C      X5=HDIFF*CV(J,I,K-1)
C
C--SEAWAT: CHECK AND CORRECT FOR DEWATERED CASE
      IF(LAYHDT(K).GT.0) THEN
        HS2=SALTHEAD(HNEW(J,I,K),PS(J,I,K),ELEV(J,I,K))
        BOT1=BOTM(J,I,LBOTM(K)-1)
        IF(HS2.LT.BOT1) THEN
            HS1=SALTHEAD(HNEW(J,I,K-1),PS(J,I,K-1),ELEV(J,I,K-1))
            X5=-1*PS(J,I,K-1)/DENSEREF*CV(J,I,K-1)*(HS1-BOT1)
        ENDIF
      ENDIF
C--SEAWAT: END DEWATERED CORRECTION
C
C12-----CALCULATE FLOW THROUGH THE LOWER FACE.
  150 IF(K.EQ.NLAY) GO TO 180
      IF(IBOUND(J,I,K+1).EQ.0) GO TO 180
      IF(ICHFLG.EQ.0 .AND. IBOUND(J,I,K+1).LT.0) GO TO 180
      HD=HNEW(J,I,K+1)
      IF(LAYHDT(K+1).EQ.0) GO TO 152
      TMP=HD
      TOP=BOTM(J,I,LBOTM(K+1)-1)
      IF(TMP.LT.TOP) HD=TOP
  152 HDIFF=HD-HNEW(J,I,K)
      DIS1=BOTM(J,I,K)-ELEV(J,I,K+1)
      DIS2=ELEV(J,I,K)-BOTM(J,I,K)
      AVGDENS=(DIS1*PS(J,I,K+1)+DIS2*PS(J,I,K))/(DIS1+DIS2)
      D6=CV(J,I,K)*(AVGDENS-DENSEREF)/DENSEREF*(ELEV(J,I,K+1)-
     &   ELEV(J,I,K))
      X6=-1*(CV(J,I,K)*HDIFF+D6)
C  152 HDIFF=HNEW(J,I,K)-HD
C      X6=HDIFF*CV(J,I,K)
C
C--CHECK AND CORRECT FOR DEWATERED CASE
      IF(LAYHDT(K+1).GT.0) THEN
        HS2=SALTHEAD(HNEW(J,I,K+1),PS(J,I,K+1),ELEV(J,I,K+1))
        BOT1=BOTM(J,I,LBOTM(K+1)-1)
        IF(HS2.LT.BOT1) THEN
            HS1=SALTHEAD(HNEW(J,I,K),PS(J,I,K),ELEV(J,I,K))
            X6=-1*PS(J,I,K)/DENSEREF*CV(J,I,K)*(BOT1-HS1)
        ENDIF
      ENDIF
C--SEAWAT: END DEWATERED CORRECTION
C
C13-----SUM THE FLOWS THROUGH SIX FACES OF CONSTANT HEAD CELL
 180  RATE=X1+X2+X3+X4+X5+X6
C
C-----RETURN
      RETURN
      END