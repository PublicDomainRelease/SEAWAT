C
      SUBROUTINE GETHEADER(IUNIT,NPER,ISS,NODES,IBOUND,IUSTR,IMT3D)
C ***************************************************************
C GATHER FLOW MODEL INFORMATION THAT WILL BE SAVED AS THE HEADER
C IN THE MODFLOW-MT3D LINK FILE.
C ***************************************************************
C last modified: 02-11-97
C
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION IUNIT(24),IBOUND(NODES)
C
C--CHECK OPTIONS USED IN MODFLOW
      MTWEL=IUNIT(2)
      MTDRN=IUNIT(3)
      MTRCH=IUNIT(8)
      MTEVT=IUNIT(5)
      MTRIV=MAX(IUNIT(4),IUSTR)
      MTGHB=IUNIT(7)
      MTISS=ISS
      MTNPER=NPER
      MTCHD=0
      DO N=1,NODES
        IF(IBOUND(N).LT.0) THEN
          MTCHD=1
          GOTO 100
        ENDIF
      ENDDO
  100 CONTINUE
C
C--WRITE A HEADER TO MODFLOW-MT3D LINK FILE
      WRITE(IMT3D) 'MT3D2.00.96',
     &  MTWEL,MTDRN,MTRCH,MTEVT,MTRIV,MTGHB,MTCHD,MTISS,MTNPER
C
      RETURN
      END
C
      SUBROUTINE BAS1MT(HNEW,IBOUND,BOT,TOP,
     &  NCOL,NROW,NLAY,KSTP,KPER,BUFF,IOUT,ELEV,PS,INUHF)
C ***************************************************************
C CALCULATE AND SAVE SATURATED THICKNESS FOR USE IN MT3D.
C INACTIVE CELLS (IBOUND=0) ARE IDENTIFIED WITH 1.E30.
C CELLS IN CONFINED LAYERS ARE IDENTIFIED WITH -111.
C ***************************************************************
C Modified from McDonald & Harbaugh (1988)
C last modified: 04-11-96
C
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      CHARACTER*16 TEXT
      DIMENSION HNEW(NCOL,NROW,NLAY), IBOUND(NCOL,NROW,NLAY),
     &   BUFF(NCOL,NROW,NLAY), BOT(NCOL,NROW,NLAY),
     &   TOP(NCOL,NROW,NLAY), ELEV(NCOL,NROW,NLAY),
     &   PS(NCOL,NROW,NLAY)

      COMMON /FLWCOM/LAYCON(80)

      TEXT='THKSAT'

C
C--INITIALIZE BUFF ARRAY WITH 1.E30 FOR INACTIVE CELLS
C--OR FLAG -111 FOR ACTIVE CELLS
      DO K=1,NLAY
        DO I=1,NROW
          DO J=1,NCOL
            IF(IBOUND(J,I,K).EQ.0) THEN
              BUFF(J,I,K)=1.E30
            ELSE
              BUFF(J,I,K)=-111.
            ENDIF
          ENDDO
        ENDDO
      ENDDO

C
C--CALCULATE SATURATED THICKNESS FOR UNCONFINED/CONVERTIBLE
C--LAYERS AND STORE IN ARRAY BUFF
      KB=0
      KT=0
      DO K=1,NLAY
        IF(LAYCON(K).EQ.1 .OR. LAYCON(K).EQ.3) KB=KB+1
        IF(LAYCON(K).EQ.2 .OR. LAYCON(K).EQ.3) KT=KT+1
        IF(LAYCON(K).EQ.0 .OR. LAYCON(K).EQ.2) GOTO 10

C
        DO I=1,NROW
          DO J=1,NCOL
            IF(IBOUND(J,I,K).NE.0) THEN
C--SEAWAT: MODIFIED TO USE SALTHEAD RATHER THAN FRESHWATER HEAD
              BUFF(J,I,K)=SALTHEAD(HNEW(J,I,K),PS(J,I,K),ELEV(J,I,K))
     &                    -BOT(J,I,KB)
              IF(LAYCON(K).EQ.3) THEN
                THKLAY=TOP(J,I,KT)-BOT(J,I,KB)
                BUFF(J,I,K)=MIN(BUFF(J,I,K),THKLAY)
              ENDIF
            ENDIF
          ENDDO
        ENDDO
C
   10 ENDDO

C
C--SAVE THE CONTENTS OF THE BUFFER
cwxg convert to single precision when write to Standard MT3D Linkfile
       WRITE(IOUT) KPER,KSTP,NCOL,NROW,NLAY,TEXT
       if(iout.eq.INUHF) then
        WRITE(IOUT) BUFF
       else
        WRITE(iout) (((SNGL(BUFF(IC,IR,IL)),IC=1,NCOL),
     +                 IR=1,NROW),IL=1,NLAY)
       endif
C
C--RETURN
      RETURN
      END
C
C
      SUBROUTINE BCF2MT(HNEW,IBOUND,CR,CC,CV,ISS,DELT,SC1,SC2,
     & HOLD,TOP,NCOL,NROW,NLAY,KSTP,KPER,BUFF,IOUT,PS,ELEV,INUHF,
     & BOT,IWTABLE)
C ***************************************************************
C SAVE FLOW ACROSS THREE CELL INTERFACES (QXX, QYY, QZZ),
C FLOW RATE TO OR FROM GROUNDWATER STORAGE (QSTO),
C AND LOCATIONS AND FLOW RATES OF CONSTANT-HEAD CELLS
C FOR USE IN MT3D.
C ***************************************************************
C Modified from McDonald & Harbaugh (1988)
C last modified: 04-11-96
C
CS00-SEAWAT: DELT=DTRANS
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CHARACTER*16 TEXT
      DIMENSION HNEW(NCOL,NROW,NLAY), IBOUND(NCOL,NROW,NLAY),
     & CR(NCOL,NROW,NLAY), CC(NCOL,NROW,NLAY),
     & CV(NCOL,NROW,NLAY), SC1(NCOL,NROW,NLAY), SC2(NCOL,NROW,NLAY),
     & TOP(NCOL,NROW,NLAY),BUFF(NCOL,NROW,NLAY),HOLD(NCOL,NROW,NLAY),
     & BOT(NCOL,NROW,NLAY)

C0S1--SEAWAT: DIMENSION
	DIMENSION PS(NCOL,NROW,NLAY),ELEV(NCOL,NROW,NLAY)

      COMMON /FLWCOM/LAYCON(80)

C0S2--SEAWAT: COMMON
	COMMON /DENSITY/ PF,CONVERSION
C
C--CALCULATE AND SAVE FLOW ACROSS RIGHT FACE
      NCM1=NCOL-1
      IF(NCM1.LT.1) GO TO 405
      TEXT='QXX'
C
C--CLEAR THE BUFFER
      DO K=1,NLAY
        DO I=1,NROW
          DO J=1,NCOL
            BUFF(J,I,K)=0.
          ENDDO
        ENDDO
      ENDDO
C
C--FOR EACH CELL
C--SEAWAT: ADD VARIABLE-DENSITY WATER TABLE CASE
	KB=0
	KT=0
      DO K=1,NLAY
	  IF (LAYCON(K).EQ.3 .OR. LAYCON(K).EQ.2) KT=KT+1
	  IF (LAYCON(K).EQ.1 .OR. LAYCON(K).EQ.3) KB=KB+1
        DO I=1,NROW
          DO J=1,NCM1
          IF(IBOUND(J,I,K).NE.0.AND.IBOUND(J+1,I,K).NE.0) THEN
		H1=HNEW(J,I,K)
		H2=HNEW(J+1,I,K)
		Z1=ELEV(J,I,K)
		Z2=ELEV(J+1,I,K)
		PS1=PS(J,I,K)
		PS2=PS(J+1,I,K)
		IF(LAYCON(K).EQ.3) THEN
			TOP1=TOP(J,I,KT)
			TOP2=TOP(J+1,I,KT)
		ENDIF
		IF(LAYCON(K).NE.2.AND.LAYCON(K).NE.0) THEN
			BOT1=BOT(J,I,KB)
			BOT2=BOT(J+1,I,KB)
		ENDIF
		IF (IWTABLE.EQ.1) CALL VDWTABLE(LAYCON(K),
     +                                    H1,PS1,Z1,TOP1,BOT1,
     +                                    H2,PS2,Z2,TOP2,BOT2)
		AVGDENS=0.5*(PS1+PS2)
		HDIFF=H2-H1
		B2=CR(J,I,K)*(AVGDENS-PF)/PF*(Z2-Z1)
                FLOWDIR=CR(J,I,K)*HDIFF+B2
              BUFF(J,I,K)=-FLOWDIR
          ENDIF
          ENDDO
        ENDDO
      ENDDO
C
C--RECORD CONTENTS OF BUFFER
cwxg
      WRITE(IOUT) KPER,KSTP,NCOL,NROW,NLAY,TEXT

       if(iout.eq.INUHF) then
        WRITE(IOUT) BUFF
       else
      WRITE(iout) (((SNGL(BUFF(IC,IR,IL)),IC=1,NCOL),
     +                 IR=1,NROW),IL=1,NLAY)
       endif

C
  405 CONTINUE
C
C--CALCULATE AND SAVE FLOW ACROSS FRONT FACE
      NRM1=NROW-1
      IF(NRM1.LT.1) GO TO 505
      TEXT='QYY'
C
C--CLEAR THE BUFFER
      DO K=1,NLAY
        DO I=1,NROW
          DO J=1,NCOL
            BUFF(J,I,K)=0.
          ENDDO
        ENDDO
      ENDDO
C
C--FOR EACH CELL
C--SEAWAT: ADD VARIABLE-DENSITY WATER TABLE CASE
	KT=0
	KB=0
      DO K=1,NLAY
      IF (LAYCON(K).EQ.3 .OR. LAYCON(K).EQ.2) KT=KT+1
	IF (LAYCON(K).EQ.1 .OR. LAYCON(K).EQ.3) KB=KB+1
        DO I=1,NRM1
          DO J=1,NCOL
            IF(IBOUND(J,I,K).NE.0.AND.IBOUND(J,I+1,K).NE.0) THEN
		H1=HNEW(J,I,K)
		H2=HNEW(J,I+1,K)
		Z1=ELEV(J,I,K)
		Z2=ELEV(J,I+1,K)
		PS1=PS(J,I,K)
		PS2=PS(J,I+1,K)
		IF(LAYCON(K).EQ.3) THEN
			TOP1=TOP(J,I,KT)
			TOP2=TOP(J,I+1,KT)
		ENDIF
		IF(LAYCON(K).NE.2.AND.LAYCON(K).NE.0) THEN
			BOT1=BOT(J,I,KB)
			BOT2=BOT(J,I+1,KB)
		ENDIF
		IF (IWTABLE.EQ.1) CALL VDWTABLE(LAYCON(K),
     +                                    H1,PS1,Z1,TOP1,BOT1,
     +                                    H2,PS2,Z2,TOP2,BOT2)
		AVGDENS=0.5*(PS1+PS2)
		HDIFF=H2-H1
		B4=CC(J,I,K)*(AVGDENS-PF)/PF*(Z2-Z1)
		FLOWDIR=CC(J,I,K)*HDIFF+B4
		BUFF(J,I,K)=-FLOWDIR
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C
C--RECORD CONTENTS OF BUFFER.
cwxg
      WRITE(IOUT) KPER,KSTP,NCOL,NROW,NLAY,TEXT
       if(iout.eq.INUHF) then
        WRITE(IOUT) BUFF
       else
      WRITE(iout) (((SNGL(BUFF(IC,IR,IL)),IC=1,NCOL),
     +                 IR=1,NROW),IL=1,NLAY)
       endif
C
  505 CONTINUE
C
C--CALCULATE AND SAVE FLOW ACROSS FRONT FACE
      NLM1=NLAY-1
      IF(NLM1.LT.1) GO TO 700
      TEXT='QZZ'
C
C--CLEAR THE BUFFER
      DO K=1,NLAY
        DO I=1,NROW
          DO J=1,NCOL
            BUFF(J,I,K)=0.
          ENDDO
        ENDDO
      ENDDO
C
C--FOR EACH CELL CALCULATE FLOW THRU LOWER FACE & STORE IN BUFFER
      KT=0
      DO K=1,NLM1
        IF(LAYCON(K).EQ.3 .OR. LAYCON(K).EQ.2) KT=KT+1
        DO I=1,NROW
          DO J=1,NCOL
C
            IF(IBOUND(J,I,K).NE.0.AND.IBOUND(J,I,K+1).NE.0) THEN
              HD=HNEW(J,I,K+1)
              IF(LAYCON(K+1).EQ.3 .OR. LAYCON(K+1).EQ.2) THEN
                TMP=HD
CS2-2: USING HEAD FOR COMPARISON, AND "ACTUAL FLOW"
                IF(SALTHEAD(TMP,PS(J,I,K+1),ELEV(J,I,K+1)).LT.
     #                 TOP(J,I,KT+1)) THEN
                   HD=TOP(J,I,KT+1)
                   FLOWDIR=-CV(J,I,K)*((ELEV(J,I,K)-HNEW(J,I,K))
     #                  +AVGDENS/PF*(HD-ELEV(J,I,K)))
                   BUFF(J,I,K)=FLOWDIR
                   goto 1011
                ENDIF
               ENDIF
C              HDIFF=HNEW(J,I,K)-HD

CS3--SEAWAT: USE VARIABLE DENSITY FOR OF DARCY'S LAW
		AVGDENS=0.5*(PS(J,I,K+1)+PS(J,I,K))
		B6=CV(J,I,K)*(AVGDENS-PF)/PF*(ELEV(J,I,K+1)-ELEV(J,I,K))
                HDIFF=HNEW(J,I,K+1)-HNEW(J,I,K)
		FLOWDIR=CV(J,I,K)*HDIFF+B6
                BUFF(J,I,K)=-FLOWDIR
              ENDIF
C
1011      continue
          ENDDO
        ENDDO
      ENDDO
C
C--RECORD CONTENTS OF BUFFER.
cwxg
      WRITE(IOUT) KPER,KSTP,NCOL,NROW,NLAY,TEXT
       if(iout.eq.INUHF) then
        WRITE(IOUT) BUFF
       else
        WRITE(iout) (((SNGL(BUFF(IC,IR,IL)),IC=1,NCOL),
     +                 IR=1,NROW),IL=1,NLAY)
       endif
C
  700 CONTINUE
C
C--CALCULATE AND SAVE GROUNDWATER STORAGE IF TRANSIENT
      IF(ISS.NE.0) GO TO 705
      TEXT='STO'
C
C--CLEAR BUFFER
      DO K=1,NLAY
        DO I=1,NROW
          DO J=1,NCOL
            BUFF(J,I,K)=0.
          ENDDO
        ENDDO
      ENDDO
C
C--RUN THROUGH EVERY CELL IN THE GRID
      KT=0
      DO K=1,NLAY
        LC=LAYCON(K)
        IF(LC.EQ.3 .OR. LC.EQ.2) KT=KT+1
        DO I=1,NROW
          DO J=1,NCOL
C
C--CALCULATE FLOW FROM STORAGE (VARIABLE HEAD CELLS ONLY)
            IF(IBOUND(J,I,K).GT.0) THEN
              HSING=HNEW(J,I,K)
              IF(LC.NE.3 .AND. LC.NE.2) THEN
                SC=SC1(J,I,K)
                STRG=SC*HOLD(J,I,K) - SC*HSING
              ELSE
                TP=TOP(J,I,KT)
                SYA=SC2(J,I,KT)
                SCFA=SC1(J,I,K)
                SOLD=SYA
CS4--SEAWAT: USING HEAD FOR COMPARISON
                IF(SALTHEAD(HOLD(J,I,K),PS(J,I,K),ELEV(J,I,K)).GT.TP)
     #             SOLD=SCFA
                SNEW=SYA
                IF(SALTHEAD(HSING,PS(J,I,K),ELEV(J,I,K)).GT.TP)
     #              SNEW=SCFA
                STRG=SOLD*(HOLD(J,I,K)-TP) + SNEW*TP - SNEW*HSING
              ENDIF
              BUFF(J,I,K)=STRG/DELT
            ENDIF
C
          ENDDO
        ENDDO
      ENDDO
C
C--RECORD CONTENTS OF BUFFER.
      CONTINUE
cwxg  convert to single "STO"
      WRITE(IOUT) KPER,KSTP,NCOL,NROW,NLAY,TEXT
       if(iout.eq.INUHF) then
        WRITE(IOUT) BUFF
       else
        WRITE(iout) (((SNGL(BUFF(IC,IR,IL)),IC=1,NCOL),
     +                 IR=1,NROW),IL=1,NLAY)
       endif
C
  705 CONTINUE
C
C--CALCULATE FLOW INTO OR OUT OF CONSTANT-HEAD CELLS
      TEXT='CNH'
      NCNH=0
C
C--CLEAR BUFFER
      DO K=1,NLAY
        DO I=1,NROW
          DO J=1,NCOL
            BUFF(J,I,K)=0.
          ENDDO
        ENDDO
      ENDDO
C
C--FOR EACH CELL IF IT IS CONSTANT HEAD COMPUTE FLOW ACROSS 6
C--FACES.
      KT=0
      DO K=1,NLAY
        LC=LAYCON(K)
        IF(LC.EQ.3 .OR. LC.EQ.2) KT=KT+1
        DO I=1,NROW
          DO J=1,NCOL
C
C--IF CELL IS NOT CONSTANT HEAD SKIP IT & GO ON TO NEXT CELL.
            IF (IBOUND(J,I,K).GE.0) GOTO 400
            NCNH=NCNH+1
C
C--CLEAR FIELDS FOR SIX FLOW RATES.
            X1=0.
            X2=0.
            X3=0.
            X4=0.
            X5=0.
            X6=0.
C
C--CALCULATE FLOW THROUGH THE LEFT FACE
C
C--IF THERE IS AN INACTIVE CELL ON THE OTHER SIDE OF THIS
C--FACE THEN GO ON TO THE NEXT FACE.
            IF(J.EQ.1) GO TO 30
            IF(IBOUND(J-1,I,K).EQ.0) GO TO 30

CS4--SEAWAT: USE VARIABLE DENSITY FOR OF DARCY'S LAW
	AVGDENS=0.5*(PS(J-1,I,K)+PS(J,I,K))
	B1=CR(J-1,I,K)*(AVGDENS-PF)/PF*(ELEV(J-1,I,K)-ELEV(J,I,K))
	HDIFF=HNEW(J-1,I,K)-HNEW(J,I,K)
	FLOWDIR=CR(J-1,I,K)*HDIFF+B1
	X1=-FLOWDIR
C
C--CALCULATE FLOW THROUGH THE RIGHT FACE
   30       IF(J.EQ.NCOL) GO TO 60
            IF(IBOUND(J+1,I,K).EQ.0) GO TO 60

CS5--SEAWAT: USE VARIABLE DENSITY FOR OF DARCY'S LAW
	AVGDENS=0.5*(PS(J+1,I,K)+PS(J,I,K))
	B2=CR(J,I,K)*(AVGDENS-PF)/PF*(ELEV(J+1,I,K)-ELEV(J,I,K))
	HDIFF=HNEW(J+1,I,K)-HNEW(J,I,K)
	FLOWDIR=CR(J,I,K)*HDIFF+B2
	X2=-FLOWDIR
C
C--CALCULATE FLOW THROUGH THE BACK FACE.
   60       IF(I.EQ.1) GO TO 90
            IF (IBOUND(J,I-1,K).EQ.0) GO TO 90

CS6--SEAWAT: USE VARIABLE DENSITY FOR OF DARCY'S LAW
	AVGDENS=0.5*(PS(J,I-1,K)+PS(J,I,K))
	B3=CC(J,I-1,K)*(AVGDENS-PF)/PF*(ELEV(J,I-1,K)-ELEV(J,I,K))
	HDIFF=HNEW(J,I-1,K)-HNEW(J,I,K)
	FLOWDIR=CC(J,I-1,K)*HDIFF+B3
	X3=-FLOWDIR
C
C--CALCULATE FLOW THROUGH THE FRONT FACE.
   90       IF(I.EQ.NROW) GO TO 120
            IF(IBOUND(J,I+1,K).EQ.0) GO TO 120

CS7--SEAWAT: USE VARIABLE DENSITY FOR OF DARCY'S LAW
	AVGDENS=0.5*(PS(J,I+1,K)+PS(J,I,K))
	B4=CC(J,I,K)*(AVGDENS-PF)/PF*(ELEV(J,I+1,K)-ELEV(J,I,K))
	HDIFF=HNEW(J,I+1,K)-HNEW(J,I,K)
	FLOWDIR=CC(J,I,K)*HDIFF+B4
	X4=-FLOWDIR
C
C--CALCULATE FLOW THROUGH THE UPPER FACE
  120       IF(K.EQ.1) GO TO 150
            IF (IBOUND(J,I,K-1).EQ.0) GO TO 150
            HD=HNEW(J,I,K)
            IF(LC.NE.3 .AND. LC.NE.2) GO TO 122
            TMP=HD
CS7-1--SEAWAT: USING HEAD FOR COMPARISON
            IF(SALTHEAD(TMP,PS(J,I,K),ELEV(J,I,K)).LT.TOP(J,I,KT)) THEN
                  HD=TOP(J,I,KT)
                  AVGDENS=0.5*(PS(J,I,K-1)+PS(J,I,K))
                  X5=-CV(J,I,K-1)*((HNEW(J,I,K-1)-ELEV(J,I,K-1))
     #               +AVGDENS/PF*(ELEV(J,I,K-1)-HD))
                  goto 150
            ENDIF

C  122       HDIFF=HD-HNEW(J,I,K-1)

CS8--SEAWAT: USE VARIABLE DENSITY FOR OF DARCY'S LAW
  122 HDIFF=HNEW(J,I,K-1)-HD
        AVGDENS=0 .5*(PS(J,I,K-1)+PS(J,I,K))
	B5=CV(J,I,K-1)*(AVGDENS-PF)/PF*(ELEV(J,I,K-1)-ELEV(J,I,K))
	FLOWDIR=CV(J,I,K-1)*HDIFF+B5
	X5=-FLOWDIR
C
C--CALCULATE FLOW THROUGH THE LOWER FACE.
  150       IF(K.EQ.NLAY) GO TO 180
            IF(IBOUND(J,I,K+1).EQ.0) GO TO 180
            HD=HNEW(J,I,K+1)
            IF(LAYCON(K+1).NE.3 .AND. LAYCON(K+1).NE.2) GO TO 152
            TMP=HD
CS8-1: SEAWAT: USING HEAD FOR COMPARISON, AND "ACTUAL FLOW"
       IF(SALTHEAD(TMP,PS(J,I,K+1),ELEV(J,I,K+1)).LT.TOP(J,I,KT+1)) THEN
             HD=TOP(J,I,KT+1)
             AVGDENS=0.5*(PS(J,I,K+1)+PS(J,I,K))
             X6=-CV(J,I,K)*((ELEV(J,I,K)-HNEW(J,I,K))
     #          +AVGDENS/PF*(HD-ELEV(J,I,K)))
             GOTO 180
       ENDIF

C  152       HDIFF=HNEW(J,I,K)-HD

CS9--SEAWAT: USE VARIABLE DENSITY FORM OF DARCY'S LAW
  152	HDIFF=HD-HNEW(J,I,K)
	AVGDENS=0.5*(PS(J,I,K+1)+PS(J,I,K))
	B6=CV(J,I,K)*(AVGDENS-PF)/PF*(ELEV(J,I,K+1)-ELEV(J,I,K))
	FLOWDIR=CV(J,I,K)*HDIFF+B6
	X6=-FLOWDIR
C
C--SUM UP FLOWS THROUGH SIX SIDES OF CONSTANT HEAD CELL.
  180       BUFF(J,I,K)=X1+X2+X3+X4+X5+X6
C
  400     ENDDO
        ENDDO
      ENDDO
C
C--RECORD CONTENTS OF BUFFER.
      WRITE(IOUT) KPER,KSTP,NCOL,NROW,NLAY,TEXT,NCNH
C
C--IF THERE ARE NO CONSTANT-HEAD CELLS THEN SKIP
      IF(NCNH.LE.0) GOTO 1000
C
C--WRITE CONSTANT-HEAD CELL LOCATIONS AND RATES
cwxg ocnvert to single
      DO K=1,NLAY
        DO I=1,NROW
          DO J=1,NCOL
            IF(IBOUND(J,I,K).LT.0) then
             if(iout.eq.INUHF) then
               WRITE(IOUT) K,I,J,BUFF(J,I,K)
             else
               WRITE(IOUT) K,I,J,SNGL(BUFF(J,I,K))
             endif
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C
C--RETURN
 1000 CONTINUE
      RETURN
      END
C
C
      SUBROUTINE WEL1MT(NWELLS,MXWELL,WELL,IBOUND,
     &  NCOL,NROW,NLAY,KSTP,KPER,IOUT, INUHF)
C *************************************************************
C SAVE WELL LOCATIONS AND RATES FOR USE IN MT3D.
C *************************************************************
C Modified from McDonald & Harbaugh (1988)
C last modified: 04-11-96
C
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CHARACTER*16 TEXT
      DIMENSION WELL(5,MXWELL),IBOUND(NCOL,NROW,NLAY)
      TEXT='WEL'
C
C--WRITE AN IDENTIFYING HEADER
      WRITE(IOUT) KPER,KSTP,NCOL,NROW,NLAY,TEXT,NWELLS
C
C--IF THERE ARE NO WELLS RETURN
      IF(NWELLS.LE.0) RETURN
C
C--WRITE WELL LOCATION AND RATE ONE AT A TIME
      DO L=1,NWELLS
        IL=WELL(1,L)
        IR=WELL(2,L)
        IC=WELL(3,L)
C
C--IF CELL IS EXTERNAL Q=0
cwxg  convert to single
        Q=0.
        IF(IBOUND(IC,IR,IL).GT.0) Q=WELL(4,L)
        if(iout.eq.INUHF) then
         WRITE(IOUT) IL,IR,IC,Q
        else
         WRITE(IOUT) IL,IR,IC,sngl(Q)
        endif

      ENDDO
C
C--RETURN
      RETURN
      END
C
C
      SUBROUTINE DRN1MT(NDRAIN,MXDRN,DRAI,HNEW,
     &  NCOL,NROW,NLAY,IBOUND,KSTP,KPER,IOUT,PS,ELEV,INUHF)
C ************************************************************
C CALCULATE AND SAVE DRAIN LOCATION AND RATES FOR USE IN MT3D.
C *************************************************************
C Modified from McDonald & Harbaugh (1988)
C last modified: 04-11-96
C
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CHARACTER*16 TEXT
C--SEAWAT: CHANGED TO '6'
      DIMENSION DRAI(6,MXDRN),
     1           HNEW(NCOL,NROW,NLAY),IBOUND(NCOL,NROW,NLAY)

C0S1--SEAWAT: DIMENSION
	DIMENSION PS(NCOL,NROW,NLAY),ELEV(NCOL,NROW,NLAY)

C0S2--SEAWAT COMMON
      COMMON   /DENSITY/ PF,CONVERSION

      TEXT='DRN'
C
C--WRITE AN IDENTIFYING HEADER
      WRITE(IOUT) KPER,KSTP,NCOL,NROW,NLAY,TEXT,NDRAIN
C
C--IF THERE ARE NO DRAINS THEN SKIP
      IF(NDRAIN.LE.0) RETURN
C
C--FOR EACH DRAIN ACCUMULATE DRAIN FLOW
      DO L=1,NDRAIN
C
C--GET LAYER, ROW & COLUMN OF CELL CONTAINING REACH.
        IL=DRAI(1,L)
        IR=DRAI(2,L)
        IC=DRAI(3,L)
        Q=0
	  RHOIJK=PS(IC,IR,IL)
	  ZIJK=ELEV(IC,IR,IL)
	  ZDRN=DRAI(6,L)
C
C--CALCULATE Q FOR ACTIVE CELLS
        IF(IBOUND(IC,IR,IL).GT.0) THEN
C
C--GET DRAIN PARAMETERS FROM DRAIN LIST.
          EL=DRAI(4,L)
          C=DRAI(5,L)
          HHNEW=HNEW(IC,IR,IL)

CS1--SEAWAT: CALCULATE SALTWATER HEAD
		HTEMP=SALTHEAD(HHNEW,RHOIJK,ZIJK)
C
C--IF HEAD LOWER THAN DRAIN THEN FORGET THIS CELL.
C--OTHERWISE, CALCULATE Q=C*(EL-HHNEW).
CS2--SEAWAT: USE SALTWATER HEAD FOR CHECKING IF DRAIN IS ACTIVE
          IF(HTEMP.GT.EL) Q=C*(EL-HHNEW-(RHOIJK-PF)/PF*(ZIJK-ZDRN))
C
        ENDIF
C
C--WRITE DRAIN LOCATION AND RATE
cwxg convert to single
       if(iout.eq.INUHF) then
         WRITE(IOUT) IL,IR,IC,Q
       else
         WRITE(IOUT) IL,IR,IC,SNGL(Q)
       endif
      ENDDO
C
C--RETURN
      RETURN
      END
C
C
      SUBROUTINE RIV1MT(NRIVER,MXRIVR,RIVR,IBOUND,HNEW,
     &  NCOL,NROW,NLAY,KSTP,KPER,IOUT,PS,ELEV, INUHF)
C ***************************************************************
C SAVE RIVER LOCATIONS AND RATES FOR USE IN MT3D.
C ***************************************************************
C Modified from McDonald & Harbaugh (1988)
C last modified: 04-11-96
C
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CHARACTER*16 TEXT
      DIMENSION RIVR(8,MXRIVR),IBOUND(NCOL,NROW,NLAY),
     &          HNEW(NCOL,NROW,NLAY)
     
C0S1--SEAWAT: DIMENSION
	DIMENSION PS(NCOL,NROW,NLAY),ELEV(NCOL,NROW,NLAY)

C0S2--SEAWAT: COMMON
	COMMON /DENSITY/ PF,CONVERSION

      TEXT='RIV'
C
C--WRITE AN IDENTIFYING HEADER
      WRITE(IOUT) KPER,KSTP,NCOL,NROW,NLAY,TEXT,NRIVER
C
C--IF NO REACHES SKIP
      IF(NRIVER.LE.0) RETURN
C
C--FOR EACH RIVER REACH ACCUMULATE RIVER FLOW
      DO L=1,NRIVER
C
C--GET LAYER, ROW & COLUMN OF CELL CONTAINING REACH.
        IL=RIVR(1,L)
        IR=RIVR(2,L)
        IC=RIVR(3,L)
C
C--IF CELL IS EXTERNAL RATE=0
        IF(IBOUND(IC,IR,IL).LE.0) THEN
          RATE=0.
C
C--GET RIVER PARAMETERS FROM RIVER LIST.
        ELSE
          HRIV=RIVR(4,L)
          CRIV=RIVR(5,L)
          RBOT=RIVR(6,L)
          HHNEW=HNEW(IC,IR,IL)
                DENSE=RIVR(7,L)
		ZIJK=ELEV(IC,IR,IL)
		RHOIJK=PS(IC,IR,IL)
		RBDTHK=RIVR(8,L)

CS1--SEAWAT: CALCULATE SALTWATER HEAD
		HTEMP=SALTHEAD(HHNEW,RHOIJK,ZIJK)

C--SEAWAT: CALCULATE FRESHWATER HEAD AT RBOT USING HEAD IN MODEL CELL
	HFRBOT=HHNEW+(RHOIJK-PF)/PF*(ZIJK-RBOT)
C
C
C--COMPARE HEAD IN AQUIFER TO BOTTOM OF RIVERBED.
C
C--AQUIFER HEAD > BOTTOM THEN RATE=CRIV*(HRIV-HNEW).

CS2--SEAWAT: UPSTREAM WEIGHT THE MASS FLUX
          IF(HTEMP.GT.RBOT) THEN
			RHOAVG=(DENSE+RHOIJK)/2
		   DIRECT=-(HRIV-HFRBOT+(RHOAVG-PF)/PF*RBDTHK)
C--SEAWAT:  DIRECT IS POSITIVE, FLOW IS UP INTO RIVER
C--SEAWAT:  DIRECT IS NEGATIVE, FLOW IS DOWN INTO GROUNDWATER
			IF (DIRECT.GT.0.) DENSE=PS(IC,IR,IL)
		RATE=CRIV*(HRIV-HFRBOT+(RHOAVG-PF)/PF*RBDTHK)
		ENDIF
C
C--AQUIFER HEAD < BOTTOM THEN RATE=CRIV*(HRIV-RBOT)
      IF(HTEMP.LE.RBOT) RATE=CRIV*(HRIV-RBOT+(DENSE-PF)/PF*RBDTHK)
        ENDIF
C
C--WRITE RIVER REACH LOCATION AND RATE
cwxg convert to single
        if(iout.eq.INUHF) then
         WRITE(IOUT) IL,IR,IC,RATE
        else
         WRITE(IOUT) IL,IR,IC,SNGL(RATE)
        endif

C
      ENDDO
C
C--RETURN
      RETURN
      END
C
C
      SUBROUTINE RCH1MT(NRCHOP,IRCH,RECH,IBOUND,NROW,NCOL,NLAY,
     &  KSTP,KPER,BUFF,IOUT, INUHF)
C *******************************************************************
C SAVE REACHARGE LAYER INDICES (IF NLAY>1) AND RATES FOR USE IN MT3D.
C *******************************************************************
C Modified from McDonald & Harbaugh, 1988
C last modified: 04-11-96
C
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CHARACTER*16 TEXT
      DIMENSION IRCH(NCOL,NROW),RECH(NCOL,NROW),
     &  IBOUND(NCOL,NROW,NLAY),BUFF(NCOL,NROW,NLAY)
      TEXT='RCH'
C
C--WRITE AN IDENTIFYING HEADER
      WRITE(IOUT) KPER,KSTP,NCOL,NROW,NLAY,TEXT
C
C--CLEAR THE BUFFER.
      DO IL=1,NLAY
        DO IR=1,NROW
          DO IC=1,NCOL
            BUFF(IC,IR,IL)=0.
          ENDDO
        ENDDO
      ENDDO
C
C--IF NRCHOP=1 RECH GOES INTO LAYER 1.
      IF(NRCHOP.EQ.1) THEN
C
        IL=1
        WRITE(IOUT) ((IL,J=1,NCOL),I=1,NROW)
C
C--STORE RECH RATE IN BUFF FOR ACTIVE CELLS
        DO I=1,NROW
          DO J=1,NCOL
            IF(IBOUND(J,I,1).GT.0) BUFF(J,I,1)=RECH(J,I)
          ENDDO
        ENDDO

cwxg convert to single
        if(iout.eq.INUHF) Then
         WRITE(IOUT) ((BUFF(J,I,1),J=1,NCOL),I=1,NROW)
        else
         WRITE(IOUT) ((SNGL(BUFF(J,I,1)),J=1,NCOL),I=1,NROW)
        endif

C
C--IF NRCHOP=2 RECH IS IN LAYER SHOWN IN INDICATOR ARRAY(IRCH).
      ELSEIF(NRCHOP.EQ.2) THEN
C
        WRITE(IOUT) ((IRCH(J,I),J=1,NCOL),I=1,NROW)
C
C--STORE RECH RATE IN BUFF FOR ACTIVE CELLS
        DO I=1,NROW
          DO J=1,NCOL
            IL=IRCH(J,I)
            IF(IBOUND(J,I,IL).GT.0) BUFF(J,I,1)=RECH(J,I)
          ENDDO
        ENDDO
cwxg convert to single
       if(iout.eq.INUHF) Then
         WRITE(IOUT) ((BUFF(J,I,1),J=1,NCOL),I=1,NROW)
       else
         WRITE(IOUT) ((SNGL(BUFF(J,I,1)),J=1,NCOL),I=1,NROW)
       Endif

C
C--IF OPTION=3 RECHARGE IS INTO HIGHEST INTERNAL CELL.
C--FIND HIGHEST INTERNAL CELL AND STORE IT IN BUFFER
      ELSEIF(NRCHOP.EQ.3) THEN
C
        DO IR=1,NROW
          DO IC=1,NCOL
            DO IL=1,NLAY
C
C--IF CELL IS CONSTANT HEAD MOVE ON TO NEXT HORIZONTAL LOCATION.
              IF(IBOUND(IC,IR,IL).LT.0) THEN
                GOTO 20
C
C--IF CELL IS INACTIVE MOVE DOWN TO NEXT CELL.
              ELSEIF (IBOUND(IC,IR,IL).EQ.0) THEN
                GOTO 10
C
C--OTHERWISE, SAVE LAYER INDEX IN BUFF AND MOVE ON.
              ELSE
                BUFF(IC,IR,1)=IL
                GOTO 20
              ENDIF
C
   10       ENDDO
   20     ENDDO
        ENDDO
        WRITE(IOUT) ((INT(BUFF(J,I,1)),J=1,NCOL),I=1,NROW)
C
C--STORE RECHARGE RATE IN BUFFER AND SAVE
        DO I=1,NROW
          DO J=1,NCOL
            IF(BUFF(J,I,1).GT.0) BUFF(J,I,1)=RECH(J,I)
          ENDDO
        ENDDO
cwxg convert to single
      if(iout.eq.INUHF) Then
        WRITE(IOUT) ((BUFF(J,I,1),J=1,NCOL),I=1,NROW)
      else
        WRITE(IOUT) ((SNGL(BUFF(J,I,1)),J=1,NCOL),I=1,NROW)
      endif
C
      ENDIF
C
C--RETURN
      RETURN
      END
C
C
      SUBROUTINE EVT2MT(NEVTOP,IEVT,EVTR,EXDP,SURF,IBOUND,HNEW,
     &  NCOL,NROW,NLAY,KSTP,KPER,BUFF,IOUT,PS,ELEV, INUHF)
C ******************************************************************
C SAVE EVAPOTRANSPIRATION LAYER INDICES (IF NLAY>1) AND RATES
C FOR USE IN MT3D.
C ******************************************************************
C Modified from McDonald & Harbaugh (1988)
C last modified: 04-11-96
C
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CHARACTER*16 TEXT
      DIMENSION IEVT(NCOL,NROW),EVTR(NCOL,NROW),EXDP(NCOL,NROW),
     &          SURF(NCOL,NROW),IBOUND(NCOL,NROW,NLAY),
     &          HNEW(NCOL,NROW,NLAY),BUFF(NCOL,NROW,NLAY)

C0S1--SEAWAT: DIMENSION
	DIMENSION PS(NCOL,NROW,NLAY),ELEV(NCOL,NROW,NLAY)

C0S2--SEAWAT: COMMON
	COMMON /DENSITY/ PF,CONVERSION

	TEXT='EVT'
C
C--WRITE AN IDENTIFYING HEADER
      WRITE(IOUT) KPER,KSTP,NCOL,NROW,NLAY,TEXT
C
C--CLEAR THE BUFFER.
      DO IL=1,NLAY
        DO IR=1,NROW
          DO IC=1,NCOL
            BUFF(IC,IR,IL)=0.
          ENDDO
        ENDDO
      ENDDO   
C
C--PROCESS EACH HORIZONTAL CELL LOCATION
C--AND STORE ET RATES IN BUFFER (IC,IR,1)
      DO IR=1,NROW
        DO IC=1,NCOL
C
C--SET THE LAYER INDEX EQUAL TO 1
          IL=1
C
C--IF OPTION 2 IS SPECIFIED THEN GET LAYER INDEX FROM IEVT ARRAY
          IF(NEVTOP.EQ.2) IL=IEVT(IC,IR)

cl--If option 3 is specified then determine the highest layer
		if (nevtop.eq.3) then
			do 20, i=1,nlay
				il=i
				if(ibound(ic,ir,il).lt.0) then
					ievt(ic,ir)=il
					goto 21
				elseif(ibound(ic,ir,il).eq.0) then
					goto 20
				else
					ievt(ic,ir)=il
					goto 21
				endif
  20		continue
  21		continue
		endif
C
C--IF CELL IS EXTERNAL THEN IGNORE IT.
          IF(IBOUND(IC,IR,IL).LE.0) GOTO 10
C
          C=EVTR(IC,IR)
          S=SURF(IC,IR)
          H=HNEW(IC,IR,IL)

CS1--SEAWAT:  USE SALTHEAD INSTEAD OF FRESHWATER HEAD
          H=SALTHEAD(H,PS(IC,IR,IL),ELEV(IC,IR,IL))
C
C
C--IF AQUIFER HEAD => SURF,SET Q=MAX ET RATE
          IF(H.GE.S) THEN
            Q=-C
C
C--IF DEPTH=>EXTINCTION DEPTH, ET IS 0
C--OTHERWISE, LINEAR RANGE: Q=-EVTR(H-EXEL)/EXDP
          ELSE
            X=EXDP(IC,IR)
            D=S-H
            IF(D.GE.X) THEN
              Q=0
            ELSE
              Q=C*D/X-C
            ENDIF
          ENDIF
C
C--ADD Q TO BUFFER 1
          BUFF(IC,IR,1)=Q
C
   10   ENDDO
      ENDDO


C
C--RECORD THEM.
      IF(NEVTOP.EQ.1) THEN
        IL=1
        WRITE(IOUT) ((IL,J=1,NCOL),I=1,NROW)
cl	added nevtop=3 option
      ELSEIF(NEVTOP.EQ.2.or.nevtop.eq.3) THEN
        WRITE(IOUT) ((IEVT(J,I),J=1,NCOL),I=1,NROW)
      ENDIF
C
c wxg convert to single
      if(iout.eq.INUHF) then
        WRITE(IOUT) ((BUFF(J,I,1),J=1,NCOL),I=1,NROW)
      else
        WRITE(IOUT) ((SNGL(BUFF(J,I,1)),J=1,NCOL),I=1,NROW)
      endif

C
C--RETURN
      RETURN
      END
C
C
      SUBROUTINE GHB1MT(NBOUND,MXBND,BNDS,HNEW,
     &  NCOL,NROW,NLAY,IBOUND,KSTP,KPER,IOUT,PS,ELEV, INUHF)
C *****************************************************************
C SAVE HEAD DEP. BOUND. CELL LOCATIONS AND RATES FOR USE IN MT3D.
C *****************************************************************
C Modified from McDonald & Harbaugh (1988)
C last modified: 04-11-96
C
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CHARACTER*16 TEXT
      DIMENSION BNDS(7,MXBND),HNEW(NCOL,NROW,NLAY),
     +          IBOUND(NCOL,NROW,NLAY),PS(NCOL,NROW,NLAY),
     +          ELEV(NCOL,NROW,NLAY)

      COMMON   /DENSITY/ PF,CONVERSION

      TEXT='GHB'
C
C--WRITE AN IDENTIFYING HEADER
      WRITE(IOUT) KPER,KSTP,NCOL,NROW,NLAY,TEXT,NBOUND
C
C--IF NO BOUNDARIES THEN SKIP
      IF(NBOUND.LE.0) RETURN
C
C--FOR EACH GENERAL HEAD BOUND ACCUMULATE FLOW INTO AQUIFER
      DO L=1,NBOUND
C
C--GET LAYER, ROW AND COLUMN OF EACH GENERAL HEAD BOUNDARY.
        IL=BNDS(1,L)
        IR=BNDS(2,L)
        IC=BNDS(3,L)
	  RHOIJK=PS(IC,IR,IL)
	  ZIJK=ELEV(IC,IR,IL)
C
C--RATE=0 IF IBOUND=<0
        RATE=0.
        IF(IBOUND(IC,IR,IL).GT.0) THEN
C
C--GET PARAMETERS FROM BOUNDARY LIST.
          HHNEW=HNEW(IC,IR,IL)
          HB=BNDS(4,L)
          C=BNDS(5,L)
	    DENSE=BNDS(6,L)
	    GHBELEV=BNDS(7,L)
C
	    RHOAVG=(RHOIJK+DENSE)/2
C--CALCULATE THE FOW RATE INTO THE CELL
          RATE=C*(HB-HHNEW+(RHOAVG-PF)/PF*(GHBELEV-ZIJK))
        ENDIF
C
C--WRITE HEAD DEP. BOUND. LOCATION AND RATE
cwxg convert to single
       if(iout.eq.INUHF) then
         WRITE(IOUT) IL,IR,IC,RATE
       else
         WRITE(IOUT) IL,IR,IC,SNGL(RATE)
       endif

C
      ENDDO
C
C--RETURN
      RETURN
      END
