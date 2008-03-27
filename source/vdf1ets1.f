      SUBROUTINE VDF1ETS1FM(NETSOP,IETS,ETSR,ETSX,ETSS,RHS,HCOF,IBOUND,
     &                      HNEW,NCOL,NROW,NLAY,NETSEG,PXDP,PETM,
     &                      NSEGAR,
     &                      CEVT,NCOMP)
C
C-----VERSION 20000620 ERB
C     ******************************************************************
C        ADD EVAPOTRANSPIRATION TO RHS AND HCOF
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE VDFMODULE,   ONLY: MT3DRHOFLG,DENSEREF,PS,ELEV
C
      DOUBLE PRECISION HNEW, HH, SS, XX, DD, PXDP1, PXDP2
      DIMENSION IETS(NCOL,NROW), ETSR(NCOL,NROW), ETSX(NCOL,NROW),
     &          ETSS(NCOL,NROW), RHS(NCOL,NROW,NLAY),
     &          HCOF(NCOL,NROW,NLAY), IBOUND(NCOL,NROW,NLAY),
     &          HNEW(NCOL,NROW,NLAY), PXDP(NCOL,NROW,NSEGAR),
     &          PETM(NCOL,NROW,NSEGAR)
C--SEAWAT: DIMENSION ADDITIONAL VARIABLES
      DIMENSION CEVT(NCOL,NROW,NCOMP)
C     ------------------------------------------------------------------
C
C1------PROCESS EACH HORIZONTAL CELL LOCATION
      DO 30 IR=1,NROW
        DO 20 IC=1,NCOL
C
C2------SET THE LAYER INDEX EQUAL TO 1      .
          IL=1
C
C3------IF OPTION 2 IS SPECIFIED THEN GET LAYER INDEX FROM IETS ARRAY
          IF (NETSOP.EQ.2) IL=IETS(IC,IR)
          IF (IL.EQ.0) GO TO 20  ! ERB 1/11/07
C
C4------IF THE CELL IS EXTERNAL IGNORE IT.
          IF (IBOUND(IC,IR,IL).GT.0) THEN
C--SEAWAT: DETERMINE DENSE VALUE OF WITHDRAWN ET FLUID
            DENSE=DENSEREF
            IF(MT3DRHOFLG.NE.0) THEN
		        DENSE=PS(IC,IR,IL)
		        DENSEEVT=CALCDENS(IC,IR,IL,CEVT(IC,IR,1:NCOMP))
		        IF(DENSEEVT.LT.DENSE) DENSE=DENSEEVT
            ENDIF
            C=ETSR(IC,IR)
            S=ETSS(IC,IR)
            SS=S
C--SEAWAT: USE ACTUAL HEAD (SALTHEAD) INSTEAD OF FRESHWATER HEAD (HNEW)
C            HH=HNEW(IC,IR,IL)
            HH=SALTHEAD(HNEW(IC,IR,IL),PS(IC,IR,IL),ELEV(IC,IR,IL))
C
C5------IF HEAD IN CELL IS GREATER THAN OR EQUAL TO ETSS, ET IS CONSTANT
            IF(HH.GE.SS) THEN
C
C5A-----SUBTRACT -ETSR FROM RHS
C--SEAWAT: MULTIPLY BY DENSE TO CONSERVE MASS
C              RHS(IC,IR,IL)=RHS(IC,IR,IL) + C
              RHS(IC,IR,IL)=RHS(IC,IR,IL) + C * DENSE
            ELSE
C
C6------IF DEPTH TO WATER>=EXTINCTION DEPTH THEN ET IS 0
              DD=SS-HH
              X=ETSX(IC,IR)
              XX=X
              IF (DD.LT.XX) THEN
C7------VARIABLE RANGE. ADD ET TERMS TO BOTH RHS AND HCOF.
C
                IF (NETSEG.GT.1) THEN
C                 DETERMINE WHICH SEGMENT APPLIES BASED ON HEAD, AND
C                 CALCULATE TERMS TO ADD TO RHS AND HCOF
C
C                 SET PROPORTIONS CORRESPONDING TO ETSS ELEVATION
                  PXDP1 = 0.0
                  PETM1 = 1.0
                  DO 10 ISEG = 1,NETSEG
C                   SET PROPORTIONS CORRESPONDING TO LOWER END OF
C                   SEGMENT
                    IF (ISEG.LT.NETSEG) THEN
                      PXDP2 = PXDP(IC,IR,ISEG)
                      PETM2 = PETM(IC,IR,ISEG)
                    ELSE
                      PXDP2 = 1.0
                      PETM2 = 0.0
                    ENDIF
                    IF (DD.LE.PXDP2*XX) THEN
C                     HEAD IS IN DOMAIN OF THIS SEGMENT
                      GOTO 15
                    ENDIF
C                   PROPORTIONS AT LOWER END OF SEGMENT WILL BE FOR
C                   UPPER END OF SEGMENT NEXT TIME THROUGH LOOP
                    PXDP1 = PXDP2
                    PETM1 = PETM2
   10             CONTINUE
   15             CONTINUE
C                 CALCULATE TERMS TO ADD TO RHS AND HCOF BASED ON
C                 SEGMENT THAT APPLIES AT HEAD ELEVATION

C--SEAWAT                  THCOF = -(PETM1-PETM2)*C/((PXDP2-PXDP1)*X)
C--SEAWAT                  TRHS = THCOF*(S-PXDP1*X) + PETM1*C
C--SEAWAT: REWRITING IN VARIABLE DENSITY FORM
                THCOF = -(PETM1-PETM2)*C/((PXDP2-PXDP1)*X)
     +                                      *DENSE*DENSEREF/PS(IC,IR,IL)
                TRHS = DENSE*(C*((PETM1-PETM2)/(PXDP2-PXDP1)*PXDP1 + 
     +                                                            PETM1)
     +                   - (PETM1-PETM2)/(PXDP2-PXDP1)*C*S/X
     +                   + C/X*ELEV(IC,IR,IL)*(PS(IC,IR,IL)-DENSEREF)/
     +                                                    PS(IC,IR,IL))
                ELSE
C                 CALCULATE TERMS TO ADD TO RHS AND HCOF BASED ON SIMPLE
C                 LINEAR RELATION OF ET VS. HEAD
C--SEAWAT:                  TRHS = C-C*S/X
C--SEAWAT:                  THCOF = -C/X
C--SEAWAT: COPIED FROM EVT PACKAGE
	          TRHS = DENSE*C-DENSE*C*S/X + DENSE*C/X*
     +              (PS(IC,IR,IL)-DENSEREF)/PS(IC,IR,IL)*ELEV(IC,IR,IL)
                THCOF =-DENSE*DENSEREF/PS(IC,IR,IL)*C/X
                ENDIF
                RHS(IC,IR,IL)=RHS(IC,IR,IL)+TRHS
                HCOF(IC,IR,IL)=HCOF(IC,IR,IL)+THCOF
              ENDIF
            ENDIF
          ENDIF
   20   CONTINUE
   30 CONTINUE
C
C8------RETURN
      RETURN
      END
C=======================================================================
      SUBROUTINE VDF1ETS1BD(NETSOP,IETS,ETSR,ETSX,ETSS,IBOUND,HNEW,NCOL,
     &                      NROW,NLAY,DELT,VBVL,VBNM,MSUM,KSTP,KPER,
     &                      IETSCB,ICBCFL,BUFF,IOUT,PERTIM,TOTIM,NETSEG,
     &                      PXDP,PETM,NSEGAR,
     &                      CEVT,NCOMP)
C-----VERSION 20000620 ERB
C     ******************************************************************
C     CALCULATE VOLUMETRIC BUDGET FOR EVAPOTRANSPIRATION SEGMENTS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE VDFMODULE,   ONLY: MT3DRHOFLG,DENSEREF,PS,ELEV
C
      CHARACTER*16 VBNM(MSUM), TEXT
      DOUBLE PRECISION HNEW, RATOUT, QQ, HH, SS, DD, XX, HHCOF, RRHS,
     &                 PXDP1, PXDP2
      DIMENSION IETS(NCOL,NROW), ETSR(NCOL,NROW), ETSX(NCOL,NROW),
     &          ETSS(NCOL,NROW), IBOUND(NCOL,NROW,NLAY),
     &          VBVL(4,MSUM), HNEW(NCOL,NROW,NLAY),
     &          BUFF(NCOL,NROW,NLAY), PXDP(NCOL,NROW,NSEGAR),
     &          PETM(NCOL,NROW,NSEGAR)
C
      DATA TEXT /'     ET SEGMENTS'/
C--SEAWAT: DIMENSION ADDITIONAL ARRAYS
      DIMENSION CEVT(NCOL,NROW,NCOMP)
C     ------------------------------------------------------------------
C
C1------CLEAR THE RATE ACCUMULATOR.
      ZERO=0.
      RATOUT=ZERO
C
C2------SET CELL-BY-CELL BUDGET SAVE FLAG (IBD) AND CLEAR THE BUFFER.
      IBD=0
      IF(IETSCB.GT.0) IBD=ICBCFL
      DO 30 IL=1,NLAY
        DO 20 IR=1,NROW
          DO 10 IC=1,NCOL
            BUFF(IC,IR,IL)=ZERO
   10     CONTINUE
   20   CONTINUE
   30 CONTINUE
C
C3------PROCESS EACH HORIZONTAL CELL LOCATION.
      DO 70 IR=1,NROW
        DO 60 IC=1,NCOL
C
C4------SET THE LAYER INDEX EQUAL TO 1.
          IL=1
C
C5------IF OPTION 2 IS SPECIFIED THEN GET LAYER INDEX FROM IETS ARRAY.
          IF (NETSOP.EQ.2) IL=IETS(IC,IR)
          IF (IL.EQ.0) GO TO 60  ! ERB 1/11/07
C
C6------IF CELL IS EXTERNAL THEN IGNORE IT.
          IF (IBOUND(IC,IR,IL).GT.0) THEN
C--SEAWAT: DETERMINE DENSE VALUE OF WITHDRAWN ET FLUID
            DENSE=DENSEREF
            IF(MT3DRHOFLG.NE.0) THEN
		        DENSE=PS(IC,IR,IL)
		        DENSEEVT=CALCDENS(IC,IR,IL,CEVT(IC,IR,1:NCOMP))
		        IF(DENSEEVT.LT.DENSE) DENSE=DENSEEVT
            ENDIF
            C=ETSR(IC,IR)
            S=ETSS(IC,IR)
            SS=S
C--SEAWAT:      HH=HNEW(IC,IR,IL)
            HH=SALTHEAD(HNEW(IC,IR,IL),PS(IC,IR,IL),ELEV(IC,IR,IL))
C
C7------IF HEAD IN CELL => ETSS,SET Q=MAX ET RATE.
            IF (HH.GE.SS) THEN
C--SEAWAT:CONSERVE MASS
C             QQ=-C
              QQ=-C*DENSE
            ELSE
C
C8------IF DEPTH=>EXTINCTION DEPTH, ET IS 0.
              X=ETSX(IC,IR)
              XX=X
              DD=SS-HH
              IF (DD.LT.XX) THEN
C9------VARIABLE RANGE.  CALCULATE Q DEPENDING ON NUMBER OF SEGMENTS
C
                IF (NETSEG.GT.1) THEN
C                 DETERMINE WHICH SEGMENT APPLIES BASED ON HEAD, AND
C                 CALCULATE TERMS TO ADD TO RHS AND HCOF
C
C                 SET PROPORTIONS CORRESPONDING TO ETSS ELEVATION
                  PXDP1 = 0.0
                  PETM1 = 1.0
                  DO 40 ISEG = 1,NETSEG
C                   SET PROPORTIONS CORRESPONDING TO LOWER END OF
C                   SEGMENT
                    IF (ISEG.LT.NETSEG) THEN
                      PXDP2 = PXDP(IC,IR,ISEG)
                      PETM2 = PETM(IC,IR,ISEG)
                    ELSE
                      PXDP2 = 1.0
                      PETM2 = 0.0
                    ENDIF
                    IF (DD.LE.PXDP2*XX) THEN
C                     HEAD IS IN DOMAIN OF THIS SEGMENT
                      GOTO 50
                    ENDIF
C                   PROPORTIONS AT LOWER END OF SEGMENT WILL BE FOR
C                   UPPER END OF SEGMENT NEXT TIME THROUGH LOOP
                    PXDP1 = PXDP2
                    PETM1 = PETM2
   40             CONTINUE
   50             CONTINUE
C9------CALCULATE ET RATE BASED ON SEGMENT THAT APPLIES AT HEAD
C9------ELEVATION
C--SEAWAT:                  HHCOF = -(PETM1-PETM2)*C/((PXDP2-PXDP1)*X)
C--SEAWAT:                  RRHS = -HHCOF*(S-PXDP1*X) - PETM1*C
                  HHCOF = -(PETM1-PETM2)*C/((PXDP2-PXDP1)*X)
     +                                      *DENSE*DENSEREF/PS(IC,IR,IL)
                  RRHS = DENSE*(C*((PETM1-PETM2)/(PXDP2-PXDP1)*PXDP1 + 
     +                                                            PETM1)
     +                   - (PETM1-PETM2)/(PXDP2-PXDP1)*C*S/X
     +                   + C/X*ELEV(IC,IR,IL)*(PS(IC,IR,IL)-DENSEREF)/
     +                                                    PS(IC,IR,IL))
                ELSE
C10-----SIMPLE LINEAR RELATION.  Q=-ETSR*(HNEW-(ETSS-ETSX))/ETSX, WHICH
C10-----IS FORMULATED AS Q= -HNEW*ETSR/ETSX + (ETSR*ETSS/ETSX -ETSR).
C--SEAWAT:                  HHCOF = -C/X
C--SEAWAT:                  RRHS = (C*S/X) - C
                  HHCOF = -DENSE*DENSEREF/PS(IC,IR,IL)*C/X
                  RRHS = DENSE*C-DENSE*C*S/X + DENSE*C/X*
     +              (PS(IC,IR,IL)-DENSEREF)/PS(IC,IR,IL)*ELEV(IC,IR,IL)
                ENDIF
C--SEAWAT:                 QQ = HH*HHCOF + RRHS
C--SEAWAT: CHANGED TO MINUS RRHS SO RRHS EQUATION WOULD BE SAME AS IN FM
                QQ = HNEW(IC,IR,IL)*HHCOF - RRHS
              ELSE
                QQ = 0.0
              ENDIF
            ENDIF
C
C10-----ACCUMULATE TOTAL FLOW RATE.
            Q=QQ
            RATOUT=RATOUT-QQ
C
C11-----ADD Q TO BUFFER.
C--SEAWAT:STORE AS VOLUME
C      BUFF(IC,IR,IL)=Q
      BUFF(IC,IR,IL)=Q/DENSE
          ENDIF
   60   CONTINUE
   70 CONTINUE
C
C12-----IF CELL-BY-CELL FLOW TO BE SAVED, CALL APPROPRIATE UTILITY
C12-----MODULE SAVE THEM.
      IF(IBD.EQ.1) CALL UBUDSV(KSTP,KPER,TEXT,IETSCB,BUFF,NCOL,NROW,
     &                         NLAY,IOUT)
      IF(IBD.EQ.2) CALL UBDSV3(KSTP,KPER,TEXT,IETSCB,BUFF,IETS,NETSOP,
     &                   NCOL,NROW,NLAY,IOUT,DELT,PERTIM,TOTIM,IBOUND)
C
C13-----MOVE TOTAL ET RATE INTO VBVL FOR PRINTING BY BAS1OT.
      ROUT=RATOUT
      VBVL(3,MSUM)=ZERO
      VBVL(4,MSUM)=ROUT
C
C14-----ADD ET(ET_RATE TIMES STEP LENGTH) TO VBVL.
      VBVL(2,MSUM)=VBVL(2,MSUM)+ROUT*DELT
C
C15-----MOVE BUDGET TERM LABELS TO VBNM FOR PRINT BY MODULE BAS1OT.
      VBNM(MSUM)=TEXT
C
C16-----INCREMENT BUDGET TERM COUNTER.
      MSUM=MSUM+1
C
C17-----RETURN.
      RETURN
      END
C=======================================================================

