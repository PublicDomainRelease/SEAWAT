C     Last change:  CDL  24 May 2002   9:00 AM
C
C ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C LINK-MT3DMS (LMT) PACKAGE FOR MODFLOW-2000
C Documented in:
C     Zheng, C., M.C. Hill, and P.A. Hsieh, 2001,
C         MODFLOW-2000, the U.S. Geological Survey modular ground-water
C         model--User guide to the LMT6 Package, the linkage with
C         MT3DMS for multispecies mass transport modeling:
C         U.S. Geological Survey Open-File Report 01-82
C
C Revision History
C     Version 6.0: 05-25-2001 cz
C             6.1: 05-01-2002 cz
C             6.2: 07-15-2003 cz
C       Variable density version: last change 10-22-2003 cl
C ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
C
C
      SUBROUTINE LMT6BAS6VD(INUNIT,IOUT,NCOL,NROW,NLAY,NPER,ISS,NODES,
     & IUNIT,CUNIT,NIUNIT,IBOUND,IMT3D,ILMTHEAD)
C *********************************************************************
C--SEAWAT: SUBROUTINE SIGNIFICANTLY MODIFIED TO WORK WITH EMBEDDED MT3DMS
C *********************************************************************
C last modified: 07-15-2003
C
      INTEGER     NCOL,NROW,NLAY,IUNIT,NIUNIT,NPER,ISS,NODES,IBOUND,
     &            IU,N,MTISS,MTNPER,MTCHD,IMT3D,MTBCF,MTLPF,MTHUF,
     &            MTWEL,MTDRN,MTRCH,MTEVT,MTRIV,MTSTR,MTGHB,MTRES,
     &            MTFHB,MTTLK,MTIBS,MTLAK,MTMNW,INUNIT,IOUT,ITYP1,
     &            ITYP2,ISTART,ISTOP,INAM1,INAM2,IFLEN,LLOC,INLMT,
     &            ILMTFMT,ILMTHEAD,IERR,MTUSR1,MTUSR2,MTUSR3,
     &            MTDRT,MTETS
      REAL        R
      LOGICAL     LOP
      CHARACTER   CUNIT*4,LINE*200,FNAME*200,NME*200,
     &            OUTPUT_FILE_HEADER*8,OUTPUT_FILE_FORMAT*11
      DIMENSION   IUNIT(NIUNIT),CUNIT(NIUNIT),IBOUND(NODES)
      COMMON     /LINKMT3D/ILMTFMT
      DATA        INLMT,MTBCF,MTLPF,MTHUF,MTWEL,MTDRN,MTRCH,MTEVT,
     &            MTRIV,MTSTR,MTGHB,MTRES,MTFHB,MTDRT,MTETS,MTTLK,
     &            MTIBS,MTLAK,MTMNW,MTUSR1,MTUSR2,MTUSR3
     &           /22*0/
C
C--CHECK for OPTIONS/PACKAGES USED IN MODFLOW-2000
C--SEAWAT--ADD THIS BACK TO SUBROUTINE
C--GATHER AND CHECK KEY FLOW MODEL INFORMATION
      MTISS=ISS
      MTNPER=NPER
      MTCHD=0
      DO N=1,NODES
        IF(IBOUND(N).LT.0) MTCHD=MTCHD+1
      ENDDO

C--SEAWAT:IMT3D=0
      DO IU=1,NIUNIT
        IF(CUNIT(IU).EQ.'LMT6') THEN
          INLMT=IUNIT(IU)
        ELSEIF(CUNIT(IU).EQ.'BCF6') THEN
          MTBCF=IUNIT(IU)
        ELSEIF(CUNIT(IU).EQ.'LPF ') THEN
          MTLPF=IUNIT(IU)
        ELSEIF(CUNIT(IU).EQ.'HUF2') THEN
          MTHUF=IUNIT(IU)
        ELSEIF(CUNIT(IU).EQ.'WEL ') THEN
          MTWEL=IUNIT(IU)
        ELSEIF(CUNIT(IU).EQ.'DRN ') THEN
          MTDRN=IUNIT(IU)
        ELSEIF(CUNIT(IU).EQ.'RCH ') THEN
          MTRCH=IUNIT(IU)
        ELSEIF(CUNIT(IU).EQ.'EVT ') THEN
          MTEVT=IUNIT(IU)
        ELSEIF(CUNIT(IU).EQ.'RIV ') THEN
          MTRIV=IUNIT(IU)
        ELSEIF(CUNIT(IU).EQ.'STR ') THEN
          MTSTR=IUNIT(IU)
        ELSEIF(CUNIT(IU).EQ.'GHB ') THEN
          MTGHB=IUNIT(IU)
        ELSEIF(CUNIT(IU).EQ.'RES ') THEN
          MTRES=IUNIT(IU)
        ELSEIF(CUNIT(IU).EQ.'FHB ') THEN
          MTFHB=IUNIT(IU)
        ELSEIF(CUNIT(IU).EQ.'DRT ') THEN
          MTDRT=IUNIT(IU)
        ELSEIF(CUNIT(IU).EQ.'ETS ') THEN
          MTETS=IUNIT(IU)
        ELSEIF(CUNIT(IU).EQ.'TLK ') THEN
          MTTLK=IUNIT(IU)
        ELSEIF(CUNIT(IU).EQ.'IBS ') THEN
          MTIBS=IUNIT(IU)
        ELSEIF(CUNIT(IU).EQ.'LAK ') THEN
          MTLAK=IUNIT(IU)
        ELSEIF(CUNIT(IU).EQ.'MNW1') THEN
          MTMNW=IUNIT(IU)
        ELSEIF(CUNIT(IU).EQ.'USR1') THEN
          MTUSR1=IUNIT(IU)
        ELSEIF(CUNIT(IU).EQ.'USR2') THEN
          MTUSR2=IUNIT(IU)
        ELSEIF(CUNIT(IU).EQ.'USR3') THEN
          MTUSR3=IUNIT(IU)
        ENDIF
      ENDDO

C--SEAWAT:  SET ILMTHEAD=1 FOR EXTENDED HEADER AND STREAM PACKAGE
      ILMTHEAD=1

C--SEAWAT: ELIMINATED LARGE PART OF THIS SECTION--NOT NEEDED FOR SEAWAT

      WRITE(IMT3D) 'MT3D4.00.00',
     &     MTWEL,MTDRN,MTRCH,MTEVT,MTRIV,MTGHB,MTCHD,MTISS,MTNPER,
     &     MTSTR,MTRES,MTFHB,MTDRT,MTETS,MTTLK,MTIBS,MTLAK,MTMNW,
     &     MTUSR1,MTUSR2,MTUSR3
C
C--NORMAL RETURN
 9999 RETURN
      END

      SUBROUTINE LMT6BCF6VD(HNEW,IBOUND,CR,CC,CV,ISS,ISSCURRENT,DELT,
     & SC1,SC2,HOLD,BOTM,NBOTM,NCOL,NROW,NLAY,KSTP,KPER,BUFF,IOUT,PS,
     & ELEV,DELR,DELC,HSALT,IWTABLE,INUHF)
C *********************************************************************
C SAVE SATURATED CELL THICKNESS; FLOW ACROSS THREE CELL INTERFACES;
C TRANSIENT FLUID-STORAGE; AND LOCATIONS AND FLOW RATES OF
C CONSTANT-HEAD CELLS FOR USE BY MT3D.  THIS SUBROUTINE IS CALLED
C ONLY IF THE 'BCF6' PACKAGE IS USED IN MODFLOW.
C *********************************************************************
C Modified from Harbaugh et al. (2000)
C last modified: 05-01-2002
C
      CHARACTER*16 TEXT
C--SEAWAT: ADD HSING,HTMP,TMP,HSALT,H1,HS1,H2,HS2
      DOUBLE PRECISION HNEW,HD,HSING,HTMP,TMP,HSALT,H1,HS1,H2,HS2

      DIMENSION HNEW(NCOL,NROW,NLAY), IBOUND(NCOL,NROW,NLAY),
     & CR(NCOL,NROW,NLAY), CC(NCOL,NROW,NLAY),
     & CV(NCOL,NROW,NLAY), SC1(NCOL,NROW,NLAY), SC2(NCOL,NROW,NLAY),
     & BOTM(NCOL,NROW,0:NBOTM),BUFF(NCOL,NROW,NLAY),HOLD(NCOL,NROW,NLAY)
      DIMENSION PS(NCOL,NROW,NLAY),ELEV(NCOL,NROW,NLAY),DELR(NCOL),
     &          DELC(NROW),VS(NCOL,NROW,NLAY),HSALT(NCOL,NROW,NLAY)
      COMMON /BCFCOM/LAYCON(200)
      COMMON /DISCOM/LBOTM(200),LAYCBD(200)
      COMMON /LINKMT3D/ILMTFMT
	INCLUDE 'vdf.inc'
C-----------------------------------------------------------------------
C--CALCULATE AND SAVE SATURATED THICKNESS
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
      DO K=1,NLAY
        IF(LAYCON(K).EQ.0 .OR. LAYCON(K).EQ.2) CYCLE
        DO I=1,NROW
          DO J=1,NCOL
            IF(IBOUND(J,I,K).NE.0) THEN
C--SEAWAT: USE SALTHEAD
C             TMP=HNEW(J,I,K)
              TMP=SALTHEAD(HNEW(J,I,K),PS(J,I,K),ELEV(J,I,K))
              BUFF(J,I,K)=TMP-BOTM(J,I,LBOTM(K))
              IF(LAYCON(K).EQ.3) THEN
                THKLAY=BOTM(J,I,LBOTM(K)-1)-BOTM(J,I,LBOTM(K))
                IF(BUFF(J,I,K).GT.THKLAY) BUFF(J,I,K)=THKLAY
              ENDIF
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C
C--SAVE THE CONTENTS OF THE BUFFER
      IF(ILMTFMT.EQ.0) THEN
        WRITE(IOUT) KPER,KSTP,NCOL,NROW,NLAY,TEXT
C--SEAWAT: WRITE AS SINGLE OR DOUBLE DEPENDING ON INUHF
	  IF (IOUT.EQ.INUHF) THEN
		WRITE(IOUT) BUFF
	  ELSE
		WRITE(IOUT) REAL(BUFF,4)
	  ENDIF
      ELSEIF(ILMTFMT.EQ.1) THEN
        WRITE(IOUT,*) KPER,KSTP,NCOL,NROW,NLAY
        WRITE(IOUT,*) TEXT
        WRITE(IOUT,*) BUFF
      ENDIF
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
      DO K=1,NLAY
        DO I=1,NROW
          DO J=1,NCM1
            IF(IBOUND(J,I,K).NE.0.AND.IBOUND(J+1,I,K).NE.0) THEN
C--SEAWAT WATER TABLE CORRECTIONS
C--SEAWAT************************
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
              IF(IWTABLE.EQ.1.AND.LAYCON(K).NE.0) 
     &      CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                    H2,HS2,PS2,Z2,TOP2,BOT2)
C--SEAWAT WATER TABLE CORRECTIONS
C--SEAWAT************************
               DIS1=DELR(J+1)/2
               DIS2=DELR(J)/2
               AVGDENS=(DIS1*PS(J+1,I,K)+DIS2*PS(J,I,K))/(DIS1+DIS2)    
               D=CR(J,I,K)*(AVGDENS-DENSEREF)/DENSEREF*
     &          (Z1-Z2)

               HDIFF=H1-H2
               BUFF(J,I,K)=CR(J,I,K)*HDIFF+D
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C
C--RECORD CONTENTS OF BUFFER
      IF(ILMTFMT.EQ.0) THEN
        WRITE(IOUT) KPER,KSTP,NCOL,NROW,NLAY,TEXT
C--SEAWAT: WRITE AS SINGLE OR DOUBLE DEPENDING ON INUHF
	  IF (IOUT.EQ.INUHF) THEN
		WRITE(IOUT) BUFF
	  ELSE
		WRITE(IOUT) REAL(BUFF,4)
	  ENDIF
      ELSEIF(ILMTFMT.EQ.1) THEN
        WRITE(IOUT,*) KPER,KSTP,NCOL,NROW,NLAY
        WRITE(IOUT,*) TEXT
        WRITE(IOUT,*) BUFF
      ENDIF
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
      DO K=1,NLAY
        DO I=1,NRM1
          DO J=1,NCOL
            IF(IBOUND(J,I,K).NE.0.AND.IBOUND(J,I+1,K).NE.0) THEN
C--SEAWAT WATER TABLE CORRECTIONS
C--SEAWAT************************
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
            IF(IWTABLE.EQ.1.AND.LAYCON(K).NE.0) 
     &      CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                    H2,HS2,PS2,Z2,TOP2,BOT2)
C--SEAWAT WATER TABLE CORRECTIONS
C--SEAWAT************************
               DIS1=DELC(I+1)/2
               DIS2=DELC(I)/2
               AVGDENS=(DIS1*PS(J,I+1,K)+DIS2*PS(J,I,K))/(DIS1+DIS2)
               D=CC(J,I,K)*(AVGDENS-DENSEREF)/DENSEREF*
     &          (Z1-Z2)
               HDIFF=H1-H2
             BUFF(J,I,K)=CC(J,I,K)*HDIFF+D
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C
C--RECORD CONTENTS OF BUFFER.
      IF(ILMTFMT.EQ.0) THEN
        WRITE(IOUT) KPER,KSTP,NCOL,NROW,NLAY,TEXT
C--SEAWAT: WRITE AS SINGLE OR DOUBLE DEPENDING ON INUHF
	  IF (IOUT.EQ.INUHF) THEN
		WRITE(IOUT) BUFF
	  ELSE
		WRITE(IOUT) REAL(BUFF,4)
	  ENDIF
      ELSEIF(ILMTFMT.EQ.1) THEN
        WRITE(IOUT,*) KPER,KSTP,NCOL,NROW,NLAY
        WRITE(IOUT,*) TEXT
        WRITE(IOUT,*) BUFF
      ENDIF
C
  505 CONTINUE
C
C--CALCULATE AND SAVE FLOW ACROSS LOWER FACE
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
      DO K=1,NLM1
        DO I=1,NROW
          DO J=1,NCOL
            IF(IBOUND(J,I,K).NE.0.AND.IBOUND(J,I,K+1).NE.0) THEN
              HD=HNEW(J,I,K+1)
              IF(LAYCON(K+1).EQ.3 .OR. LAYCON(K+1).EQ.2) THEN
                TMP=HD
                IF(TMP.LT.BOTM(J,I,LBOTM(K+1)-1))
     &           HD=BOTM(J,I,LBOTM(K+1)-1)
              ENDIF
              HDIFF=HNEW(J,I,K)-HD
              DIS1=BOTM(J,I,K)-ELEV(J,I,K+1)
                DIS2=ELEV(J,I,K)-BOTM(J,I,K)
                AVGDENS=(DIS1*PS(J,I,K+1)+DIS2*PS(J,I,K))/(DIS1+DIS2)
                D=CV(J,I,K)*(AVGDENS-DENSEREF)/DENSEREF*
     &           (ELEV(J,I,K)-ELEV(J,I,K+1))      
              BUFF(J,I,K)=CV(J,I,K)*HDIFF+D
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C--RECORD CONTENTS OF BUFFER.
      IF(ILMTFMT.EQ.0) THEN
        WRITE(IOUT) KPER,KSTP,NCOL,NROW,NLAY,TEXT
C--SEAWAT: WRITE AS SINGLE OR DOUBLE DEPENDING ON INUHF
	  IF (IOUT.EQ.INUHF) THEN
		WRITE(IOUT) BUFF
	  ELSE
		WRITE(IOUT) REAL(BUFF,4)
	  ENDIF
      ELSEIF(ILMTFMT.EQ.1) THEN
        WRITE(IOUT,*) KPER,KSTP,NCOL,NROW,NLAY
        WRITE(IOUT,*) TEXT
        WRITE(IOUT,*) BUFF
      ENDIF
C
  700 CONTINUE
C
C--CALCULATE AND SAVE GROUNDWATER STORAGE IF TRANSIENT
      IF(ISS.NE.0) GO TO 705
      TEXT='STO'
C
C--INITIALIZE AND CLEAR BUFFER
      ZERO=0.
      ONE=1.
      TLED=ONE/DELT
      DO K=1,NLAY
        DO I=1,NROW
          DO J=1,NCOL
            BUFF(J,I,K)=ZERO
          ENDDO
        ENDDO
      ENDDO
      IF(ISSCURRENT.NE.0) GOTO 704
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
                RHO=SC1(J,I,K)*TLED
                STRG=RHO*HOLD(J,I,K) - RHO*HSING
              ELSE
                TP=BOTM(J,I,LBOTM(K)-1)
                RHO2=SC2(J,I,KT)*TLED
                RHO1=SC1(J,I,K)*TLED
                SOLD=RHO2
C--SEAWAT: COMPARE WITH SALTHEAD
C               IF(HOLD(J,I,K).GT.TP) SOLD=RHO1
                HTMP=HOLD(J,I,K)
                IF(SALTHEAD(HTMP,PS(J,I,K),ELEV(J,I,K)).GT.TP) 
     &          SOLD=RHO1
                SNEW=RHO2
C--SEAWAT: COMPARE WITH SALTHEAD
C               IF(HSING.GT.TP) SNEW=RHO1
                IF(SALTHEAD(HSING,PS(J,I,K),ELEV(J,I,K)).GT.TP) 
     &             SNEW=RHO1
                STRG=SOLD*(HOLD(J,I,K)-TP) + SNEW*TP - SNEW*HSING
              ENDIF
              BUFF(J,I,K)=STRG
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C
C--RECORD CONTENTS OF BUFFER.
  704 IF(ILMTFMT.EQ.0) THEN
        WRITE(IOUT) KPER,KSTP,NCOL,NROW,NLAY,TEXT
C--SEAWAT: WRITE AS SINGLE OR DOUBLE DEPENDING ON INUHF
	  IF (IOUT.EQ.INUHF) THEN
		WRITE(IOUT) BUFF
	  ELSE
		WRITE(IOUT) REAL(BUFF,4)
	  ENDIF
      ELSEIF(ILMTFMT.EQ.1) THEN
        WRITE(IOUT,*) KPER,KSTP,NCOL,NROW,NLAY
        WRITE(IOUT,*) TEXT
        WRITE(IOUT,*) BUFF
      ENDIF
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
      DO K=1,NLAY
        DO I=1,NROW
          DO J=1,NCOL
C
C--IF CELL IS NOT CONSTANT HEAD SKIP IT & GO ON TO NEXT CELL.
            IF(IBOUND(J,I,K).GE.0) CYCLE
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
C--SEAWAT WATER TABLE CORRECTIONS
C--SEAWAT************************
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
            IF(IWTABLE.EQ.1.AND.LAYCON(K).NE.0) 
     &      CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                    H2,HS2,PS2,Z2,TOP2,BOT2)
C--SEAWAT WATER TABLE CORRECTIONS
C--SEAWAT************************
               DIS1=DELR(J-1)/2
               DIS2=DELR(J)/2
               AVGDENS=(DIS1*PS(J-1,I,K)+DIS2*PS(J,I,K))/(DIS1+DIS2)
               D=CR(J-1,I,K)*(AVGDENS-DENSEREF)/DENSEREF*(Z1-Z2)      
               HDIFF=H1-H2
C
C--CALCULATE FLOW THROUGH THIS FACE INTO THE ADJACENT CELL.
               X1=HDIFF*CR(J-1,I,K)+D
C
C--CALCULATE FLOW THROUGH THE RIGHT FACE
   30       IF(J.EQ.NCOL) GO TO 60
            IF(IBOUND(J+1,I,K).EQ.0) GO TO 60
C--SEAWAT WATER TABLE CORRECTIONS
C--SEAWAT************************
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
            IF(IWTABLE.EQ.1.AND.LAYCON(K).NE.0) 
     &      CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                    H2,HS2,PS2,Z2,TOP2,BOT2)
C--SEAWAT WATER TABLE CORRECTIONS
C--SEAWAT************************
               DIS1=DELR(J+1)/2
               DIS2=DELR(J)/2
               AVGDENS=(DIS1*PS(J+1,I,K)+DIS2*PS(J,I,K))/(DIS1+DIS2)
               D=CR(J,I,K)*(AVGDENS-DENSEREF)/DENSEREF*(Z1-Z2)
               HDIFF=H1-H2
               X2=HDIFF*CR(J,I,K)+D
C
C--CALCULATE FLOW THROUGH THE BACK FACE.
   60       IF(I.EQ.1) GO TO 90
            IF (IBOUND(J,I-1,K).EQ.0) GO TO 90
C--SEAWAT WATER TABLE CORRECTIONS
C--SEAWAT************************
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
            IF(IWTABLE.EQ.1.AND.LAYCON(K).NE.0) 
     &      CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                    H2,HS2,PS2,Z2,TOP2,BOT2)
C--SEAWAT WATER TABLE CORRECTIONS
C--SEAWAT************************
               DIS1=DELC(I-1)/2
               DIS2=DELC(I)/2
               AVGDENS=(DIS1*PS(J,I-1,K)+DIS2*PS(J,I,K))/(DIS1+DIS2)
               D=CC(J,I-1,K)*(AVGDENS-DENSEREF)/DENSEREF*(Z1-Z2)
               HDIFF=H1-H2
               X3=HDIFF*CC(J,I-1,K)+D
C
C--CALCULATE FLOW THROUGH THE FRONT FACE.
   90       IF(I.EQ.NROW) GO TO 120
            IF(IBOUND(J,I+1,K).EQ.0) GO TO 120
C--SEAWAT WATER TABLE CORRECTIONS
C--SEAWAT************************
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
            IF(IWTABLE.EQ.1.AND.LAYCON(K).NE.0) 
     &      CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                    H2,HS2,PS2,Z2,TOP2,BOT2)
C--SEAWAT WATER TABLE CORRECTIONS
C--SEAWAT************************
               DIS1=DELC(I+1)/2
               DIS2=DELC(I)/2
               AVGDENS=(DIS1*PS(J,I+1,K)+DIS2*PS(J,I,K))/(DIS1+DIS2)
               D=CC(J,I,K)*(AVGDENS-DENSEREF)/DENSEREF*(Z1-Z2)
               HDIFF=H1-H2
               X4=HDIFF*CC(J,I+1,K)+D
C
C--CALCULATE FLOW THROUGH THE UPPER FACE
  120       IF(K.EQ.1) GO TO 150
            IF (IBOUND(J,I,K-1).EQ.0) GO TO 150
            HD=HNEW(J,I,K)
            IF(LAYCON(K).NE.3 .AND. LAYCON(K).NE.2) GO TO 122
            TMP=HD
            IF(TMP.LT.BOTM(J,I,LBOTM(K)-1))
     &       HD=BOTM(J,I,LBOTM(K)-1)
  122        HDIFF=HD-HNEW(J,I,K-1)
             DIS1=ELEV(J,I,K-1)-BOTM(J,I,K-1)
             DIS2=BOTM(J,I,K-1)-ELEV(J,I,K)
             AVGDENS=(DIS1*PS(J,I,K-1)+DIS2*PS(J,I,K))/(DIS1+DIS2)
             D=CV(J,I,K-1)*(AVGDENS-DENSEREF)/DENSEREF*
     &           (ELEV(J,I,K)-ELEV(J,I,K-1))
             X5=HDIFF*CV(J,I,K-1)+D
C
C--CALCULATE FLOW THROUGH THE LOWER FACE.
  150       IF(K.EQ.NLAY) GO TO 180
            IF(IBOUND(J,I,K+1).EQ.0) GO TO 180
            HD=HNEW(J,I,K+1)
            IF(LAYCON(K+1).NE.3 .AND. LAYCON(K+1).NE.2) GO TO 152
            TMP=HD
            IF(TMP.LT.BOTM(J,I,LBOTM(K+1)-1))
     &       HD=BOTM(J,I,LBOTM(K+1)-1)
  152        HDIFF=HNEW(J,I,K)-HD
             DIS1=BOTM(J,I,K)-ELEV(J,I,K+1)
             DIS2=ELEV(J,I,K)-BOTM(J,I,K)
             AVGDENS=(DIS1*PS(J,I,K+1)+DIS2*PS(J,I,K))/(DIS1+DIS2)
             D=CV(J,I,K)*(AVGDENS-DENSEREF)/DENSEREF*
     &          (ELEV(J,I,K)-ELEV(J,I,K+1))
             X6=HDIFF*CV(J,I,K)+D
C
C--SUM UP FLOWS THROUGH SIX SIDES OF CONSTANT HEAD CELL.
  180       BUFF(J,I,K)=X1+X2+X3+X4+X5+X6
          ENDDO
        ENDDO
      ENDDO
C
C--RECORD CONTENTS OF BUFFER.
      IF(ILMTFMT.EQ.0) THEN
        WRITE(IOUT) KPER,KSTP,NCOL,NROW,NLAY,TEXT,NCNH
      ELSEIF(ILMTFMT.EQ.1) THEN
        WRITE(IOUT,*) KPER,KSTP,NCOL,NROW,NLAY
        WRITE(IOUT,*) TEXT,NCNH
      ENDIF
C
C--IF THERE ARE NO CONSTANT-HEAD CELLS THEN SKIP
      IF(NCNH.LE.0) GOTO 1000
C
C--WRITE CONSTANT-HEAD CELL LOCATIONS AND RATES
      DO K=1,NLAY
        DO I=1,NROW
          DO J=1,NCOL
            IF(IBOUND(J,I,K).LT.0) THEN
              IF(ILMTFMT.EQ.0) THEN
C--SEAWAT WRITE SINGLE OR DOUBLE PRECISION DEPENDING ON INUHF
			  IF(IOUT.EQ.INUHF) WRITE(IOUT) K,I,J,BUFF(J,I,K)
	          IF(IOUT.NE.INUHF) WRITE(IOUT) K,I,J,REAL(BUFF(J,I,K),4)
              ENDIF
			IF(ILMTFMT.EQ.1) WRITE(IOUT,*) K,I,J,BUFF(J,I,K)
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
      SUBROUTINE LMT6LPF1VD(HNEW,IBOUND,CR,CC,CV,ISS,ISSCURRENT,DELT,
     & SC1,SC2,HOLD,BOTM,NBOTM,NCOL,NROW,NLAY,KSTP,KPER,BUFF,IOUT,PS,
     & ELEV,DELR,DELC,HSALT,IWTABLE,INUHF)
C *********************************************************************
C SAVE FLOW ACROSS THREE CELL INTERFACES (QXX, QYY, QZZ), FLOW RATE TO
C OR FROM TRANSIENT FLUID-STORAGE (QSTO), AND LOCATIONS AND FLOW RATES
C OF CONSTANT-HEAD CELLS FOR USE BY MT3D.  THIS SUBROUTINE IS CALLED
C ONLY IF THE 'LPF1' PACKAGE IS USED IN MODFLOW.
C *********************************************************************
C Modified from Harbaugh et al. (2000)
C last modified: 05-01-2002
C
      CHARACTER*16 TEXT
C--SEAWAT: ADD HSING,HTMP,TMP,HSALT,H1,HS1,H2,HS2
      DOUBLE PRECISION HNEW(NCOL,NROW,NLAY),HD,HSING,HTMP,TMP,
     &                 HSALT(NCOL,NROW,NLAY),H1,HS1,H2,HS2

      DIMENSION IBOUND(NCOL,NROW,NLAY),
     & CR(NCOL,NROW,NLAY), CC(NCOL,NROW,NLAY),
     & CV(NCOL,NROW,NLAY), SC1(NCOL,NROW,NLAY), SC2(NCOL,NROW,NLAY),
     & BOTM(NCOL,NROW,0:NBOTM),BUFF(NCOL,NROW,NLAY),HOLD(NCOL,NROW,NLAY)
      COMMON /DISCOM/LBOTM(200),LAYCBD(200)
      COMMON /LPFCOM/LAYTYP(200),LAYAVG(200),CHANI(200),LAYVKA(200),
     &        LAYWET(200)
      COMMON /LINKMT3D/ILMTFMT
C--SEAWAT DIMENSION VARIABLE DENSITY FLOW ARRAYS, DECLARE COMMON VARIABLES
      DIMENSION PS(NCOL,NROW,NLAY),ELEV(NCOL,NROW,NLAY),DELR(NCOL),
     &          DELC(NROW),VS(NCOL,NROW,NLAY)
	INCLUDE 'vdf.inc'
C-----------------------------------------------------------------------
C
C--CALCULATE AND SAVE SATURATED THICKNESS
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
      DO K=1,NLAY
        IF(LAYTYP(K).EQ.0) CYCLE
        DO I=1,NROW
          DO J=1,NCOL
            IF(IBOUND(J,I,K).NE.0) THEN
C--SEAWAT: USE SALTHEAD
C              TMP=HNEW(J,I,K)
              TMP=HSALT(J,I,K)
              BUFF(J,I,K)=TMP-BOTM(J,I,LBOTM(K))
              THKLAY=BOTM(J,I,LBOTM(K)-1)-BOTM(J,I,LBOTM(K))
              IF(BUFF(J,I,K).GT.THKLAY) BUFF(J,I,K)=THKLAY
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C
C--SAVE THE CONTENTS OF THE BUFFER
      IF(ILMTFMT.EQ.0) THEN
        WRITE(IOUT) KPER,KSTP,NCOL,NROW,NLAY,TEXT
C--SEAWAT: WRITE AS SINGLE OR DOUBLE DEPENDING ON INUHF
	  IF (IOUT.EQ.INUHF) THEN
		WRITE(IOUT) BUFF
	  ELSE
		WRITE(IOUT) REAL(BUFF,4)
	  ENDIF
      ELSEIF(ILMTFMT.EQ.1) THEN
        WRITE(IOUT,*) KPER,KSTP,NCOL,NROW,NLAY
        WRITE(IOUT,*) TEXT
        WRITE(IOUT,*) BUFF
      ENDIF
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
      DO K=1,NLAY
        DO I=1,NROW
          DO J=1,NCM1
            IF(IBOUND(J,I,K).NE.0.AND.IBOUND(J+1,I,K).NE.0) THEN
C--SEAWAT WATER TABLE CORRECTIONS
C--SEAWAT************************
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
            IF(IWTABLE.EQ.1.AND.LAYTYP(K).NE.0) 
     &      CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                    H2,HS2,PS2,Z2,TOP2,BOT2)
C--SEAWAT WATER TABLE CORRECTIONS
C--SEAWAT************************
              DIS1=DELR(J+1)/2
              DIS2=DELR(J)/2
              AVGDENS=(DIS1*PS(J+1,I,K)+DIS2*PS(J,I,K))/(DIS1+DIS2)    
              D=CR(J,I,K)*(AVGDENS-DENSEREF)/DENSEREF*(Z1-Z2)
              HDIFF=H1-H2
              BUFF(J,I,K)=HDIFF*CR(J,I,K)+D
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C
C--RECORD CONTENTS OF BUFFER
      IF(ILMTFMT.EQ.0) THEN
        WRITE(IOUT) KPER,KSTP,NCOL,NROW,NLAY,TEXT
C--SEAWAT: WRITE AS SINGLE OR DOUBLE DEPENDING ON INUHF
	  IF (IOUT.EQ.INUHF) THEN
		WRITE(IOUT) BUFF
	  ELSE
		WRITE(IOUT) REAL(BUFF,4)
	  ENDIF
      ELSEIF(ILMTFMT.EQ.1) THEN
        WRITE(IOUT,*) KPER,KSTP,NCOL,NROW,NLAY
        WRITE(IOUT,*) TEXT
        WRITE(IOUT,*) BUFF
      ENDIF
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
      DO K=1,NLAY
        DO I=1,NRM1
          DO J=1,NCOL
            IF(IBOUND(J,I,K).NE.0.AND.IBOUND(J,I+1,K).NE.0) THEN
C--SEAWAT WATER TABLE CORRECTIONS
C--SEAWAT************************
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
            IF(IWTABLE.EQ.1.AND.LAYTYP(K).NE.0) 
     &      CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                    H2,HS2,PS2,Z2,TOP2,BOT2)
C--SEAWAT WATER TABLE CORRECTIONS
C--SEAWAT************************
               DIS1=DELC(I+1)/2
               DIS2=DELC(I)/2
               AVGDENS=(DIS1*PS(J,I+1,K)+DIS2*PS(J,I,K))/(DIS1+DIS2)
               D=CC(J,I,K)*(AVGDENS-DENSEREF)/DENSEREF*
     &          (Z1-Z2)
               HDIFF=H1-H2
               BUFF(J,I,K)=HDIFF*CC(J,I,K)+D
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C
C--RECORD CONTENTS OF BUFFER.
      IF(ILMTFMT.EQ.0) THEN
        WRITE(IOUT) KPER,KSTP,NCOL,NROW,NLAY,TEXT
C--SEAWAT: WRITE AS SINGLE OR DOUBLE DEPENDING ON INUHF
	  IF (IOUT.EQ.INUHF) THEN
		WRITE(IOUT) BUFF
	  ELSE
		WRITE(IOUT) REAL(BUFF,4)
	  ENDIF
      ELSEIF(ILMTFMT.EQ.1) THEN
        WRITE(IOUT,*) KPER,KSTP,NCOL,NROW,NLAY
        WRITE(IOUT,*) TEXT
        WRITE(IOUT,*) BUFF
      ENDIF
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
      DO K=1,NLM1
        DO I=1,NROW
          DO J=1,NCOL
            IF(IBOUND(J,I,K).NE.0.AND.IBOUND(J,I,K+1).NE.0) THEN
              HD=HNEW(J,I,K+1)
              IF(LAYTYP(K+1).NE.0) THEN
                TMP=HD
                TOP=BOTM(J,I,LBOTM(K+1)-1)
                IF(TMP.LT.TOP) HD=TOP
              ENDIF
              HDIFF=HNEW(J,I,K)-HD
              DIS1=BOTM(J,I,K)-ELEV(J,I,K+1)
                DIS2=ELEV(J,I,K)-BOTM(J,I,K)
                AVGDENS=(DIS1*PS(J,I,K+1)+DIS2*PS(J,I,K))/(DIS1+DIS2)
                D=CV(J,I,K)*(AVGDENS-DENSEREF)/DENSEREF*
     &           (ELEV(J,I,K)-ELEV(J,I,K+1))
              BUFF(J,I,K)=HDIFF*CV(J,I,K)+D
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C
C--RECORD CONTENTS OF BUFFER.
      IF(ILMTFMT.EQ.0) THEN
        WRITE(IOUT) KPER,KSTP,NCOL,NROW,NLAY,TEXT
C--SEAWAT: WRITE AS SINGLE OR DOUBLE DEPENDING ON INUHF
	  IF (IOUT.EQ.INUHF) THEN
		WRITE(IOUT) BUFF
	  ELSE
		WRITE(IOUT) REAL(BUFF,4)
	  ENDIF
      ELSEIF(ILMTFMT.EQ.1) THEN
        WRITE(IOUT,*) KPER,KSTP,NCOL,NROW,NLAY
        WRITE(IOUT,*) TEXT
        WRITE(IOUT,*) BUFF
      ENDIF
C
  700 CONTINUE
C
C--CALCULATE AND SAVE GROUNDWATER STORAGE IF TRANSIENT
      IF(ISS.NE.0) GO TO 705
      TEXT='STO'
C
C--INITIALIZE AND CLEAR BUFFER
      ZERO=0.
      ONE=1.
      TLED=ONE/DELT
      DO K=1,NLAY
        DO I=1,NROW
          DO J=1,NCOL
            BUFF(J,I,K)=ZERO
          ENDDO
        ENDDO
      ENDDO
      IF(ISSCURRENT.NE.0) GOTO 704
C
C--RUN THROUGH EVERY CELL IN THE GRID
      KT=0
      DO K=1,NLAY
        LC=LAYTYP(K)
          IF(LC.NE.0) KT=KT+1
        DO I=1,NROW
          DO J=1,NCOL
C
C--CALCULATE FLOW FROM STORAGE (VARIABLE HEAD CELLS ONLY)
            IF(IBOUND(J,I,K).GT.0) THEN
              HSING=HNEW(J,I,K)
              IF(LC.EQ.0) THEN
                RHO=SC1(J,I,K)*TLED
                STRG=RHO*HOLD(J,I,K) - RHO*HSING
              ELSE
                TP=BOTM(J,I,LBOTM(K)-1)
                RHO2=SC2(J,I,KT)*TLED
                RHO1=SC1(J,I,K)*TLED
                SOLD=RHO2
C--SEAWAT:USE SALTHEAD FOR COMPARISON 
C               IF(HOLD(J,I,K).GT.TP) SOLD=RHO1
                HTMP=HOLD(J,I,K)
                IF(SALTHEAD(HTMP,PS(J,I,K),ELEV(J,I,K)).GT.TP) SOLD=RHO1
                SNEW=RHO2
C--SEAWAT:USE SALTHEAD FOR COMPARISON 
C               IF(HSING.GT.TP) SNEW=RHO1
                IF(SALTHEAD(HSING,PS(J,I,K),ELEV(J,I,K)).GT.TP) 
     &             SNEW=RHO1
                STRG=SOLD*(HOLD(J,I,K)-TP) + SNEW*TP - SNEW*HSING
              ENDIF
              BUFF(J,I,K)=STRG
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C
C--RECORD CONTENTS OF BUFFER.
  704 IF(ILMTFMT.EQ.0) THEN
        WRITE(IOUT) KPER,KSTP,NCOL,NROW,NLAY,TEXT
C--SEAWAT: WRITE AS SINGLE OR DOUBLE DEPENDING ON INUHF
	  IF (IOUT.EQ.INUHF) THEN
		WRITE(IOUT) BUFF
	  ELSE
		WRITE(IOUT) REAL(BUFF,4)
	  ENDIF
      ELSEIF(ILMTFMT.EQ.1) THEN
        WRITE(IOUT,*) KPER,KSTP,NCOL,NROW,NLAY
        WRITE(IOUT,*) TEXT
        WRITE(IOUT,*) BUFF
      ENDIF
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
      DO K=1,NLAY
        DO I=1,NROW
          DO J=1,NCOL
C
C--IF CELL IS NOT CONSTANT HEAD SKIP IT & GO ON TO NEXT CELL.
            IF(IBOUND(J,I,K).GE.0) CYCLE
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
C--SEAWAT WATER TABLE CORRECTIONS
C--SEAWAT************************
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
            IF(IWTABLE.EQ.1.AND.LAYTYP(K).NE.0) 
     &      CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                    H2,HS2,PS2,Z2,TOP2,BOT2)
C--SEAWAT WATER TABLE CORRECTIONS
C--SEAWAT************************
               DIS1=DELR(J-1)/2
               DIS2=DELR(J)/2
               AVGDENS=(DIS1*PS(J-1,I,K)+DIS2*PS(J,I,K))/(DIS1+DIS2)
               D=CR(J-1,I,K)*(AVGDENS-DENSEREF)/DENSEREF*(Z1-Z2)      
               HDIFF=H1-H2
C
C--CALCULATE FLOW THROUGH THIS FACE INTO THE ADJACENT CELL.
               X1=HDIFF*CR(J-1,I,K)+D
C
C--CALCULATE FLOW THROUGH THE RIGHT FACE
   30       IF(J.EQ.NCOL) GO TO 60
            IF(IBOUND(J+1,I,K).EQ.0) GO TO 60
C--SEAWAT WATER TABLE CORRECTIONS
C--SEAWAT************************
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
            IF(IWTABLE.EQ.1.AND.LAYTYP(K).NE.0) 
     &      CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                    H2,HS2,PS2,Z2,TOP2,BOT2)
C--SEAWAT WATER TABLE CORRECTIONS
C--SEAWAT************************
               DIS1=DELR(J+1)/2
               DIS2=DELR(J)/2
               AVGDENS=(DIS1*PS(J+1,I,K)+DIS2*PS(J,I,K))/(DIS1+DIS2)
               D=CR(J,I,K)*(AVGDENS-DENSEREF)/DENSEREF*
     &           (Z1-Z2)
               HDIFF=H1-H2
               X2=HDIFF*CR(J,I,K)+D
C
C--CALCULATE FLOW THROUGH THE BACK FACE.
   60       IF(I.EQ.1) GO TO 90
            IF (IBOUND(J,I-1,K).EQ.0) GO TO 90
C--SEAWAT WATER TABLE CORRECTIONS
C--SEAWAT************************
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
            IF(IWTABLE.EQ.1.AND.LAYTYP(K).NE.0) 
     &      CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                    H2,HS2,PS2,Z2,TOP2,BOT2)
C--SEAWAT WATER TABLE CORRECTIONS
C--SEAWAT************************
               DIS1=DELC(I-1)/2
               DIS2=DELC(I)/2
               AVGDENS=(DIS1*PS(J,I-1,K)+DIS2*PS(J,I,K))/(DIS1+DIS2)
               D=CC(J,I-1,K)*(AVGDENS-DENSEREF)/DENSEREF*(Z1-Z2)
               HDIFF=H1-H2
               X3=HDIFF*CC(J,I-1,K)+D
C
C--CALCULATE FLOW THROUGH THE FRONT FACE.
   90       IF(I.EQ.NROW) GO TO 120
            IF(IBOUND(J,I+1,K).EQ.0) GO TO 120
C--SEAWAT WATER TABLE CORRECTIONS
C--SEAWAT************************
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
            IF(IWTABLE.EQ.1.AND.LAYTYP(K).NE.0) 
     &      CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                    H2,HS2,PS2,Z2,TOP2,BOT2)
C--SEAWAT WATER TABLE CORRECTIONS
C--SEAWAT************************
               DIS1=DELC(I+1)/2
               DIS2=DELC(I)/2
               AVGDENS=(DIS1*PS(J,I+1,K)+DIS2*PS(J,I,K))/(DIS1+DIS2)
               D=CC(J,I,K)*(AVGDENS-DENSEREF)/DENSEREF*(Z1-Z2)
               HDIFF=H1-H2
               X4=HDIFF*CC(J,I,K)+D
C
C--CALCULATE FLOW THROUGH THE UPPER FACE
  120       IF(K.EQ.1) GO TO 150
            IF (IBOUND(J,I,K-1).EQ.0) GO TO 150
            HD=HNEW(J,I,K)
            IF(LAYTYP(K).EQ.0) GO TO 122
            TMP=HD
            TOP=BOTM(J,I,LBOTM(K)-1)
            IF(TMP.LT.TOP) HD=TOP
  122       HDIFF=HD-HNEW(J,I,K-1)
              DIS1=ELEV(J,I,K-1)-BOTM(J,I,K-1)
                  DIS2=BOTM(J,I,K-1)-ELEV(J,I,K)
                  AVGDENS=(DIS1*PS(J,I,K-1)+DIS2*PS(J,I,K))/(DIS1+DIS2)
                  D=CV(J,I,K-1)*(AVGDENS-DENSEREF)/DENSEREF*
     &           (ELEV(J,I,K)-ELEV(J,I,K-1))
            X5=HDIFF*CV(J,I,K-1)+D
C
C--CALCULATE FLOW THROUGH THE LOWER FACE.
  150       IF(K.EQ.NLAY) GO TO 180
            IF(IBOUND(J,I,K+1).EQ.0) GO TO 180
            HD=HNEW(J,I,K+1)
            IF(LAYTYP(K+1).EQ.0) GO TO 152
            TMP=HD
            TOP=BOTM(J,I,LBOTM(K+1)-1)
            IF(TMP.LT.TOP) HD=TOP
  152       HDIFF=HNEW(J,I,K)-HD
                  DIS1=BOTM(J,I,K)-ELEV(J,I,K+1)
                  DIS2=ELEV(J,I,K)-BOTM(J,I,K)
                  AVGDENS=(DIS1*PS(J,I,K+1)+DIS2*PS(J,I,K))/(DIS1+DIS2)
                  D=CV(J,I,K)*(AVGDENS-DENSEREF)/DENSEREF*
     &          (ELEV(J,I,K)-ELEV(J,I,K+1))
            X6=HDIFF*CV(J,I,K)+D
C
C--SUM UP FLOWS THROUGH SIX SIDES OF CONSTANT HEAD CELL.
  180       BUFF(J,I,K)=X1+X2+X3+X4+X5+X6
          ENDDO
        ENDDO
      ENDDO
C
C--RECORD CONTENTS OF BUFFER.
      IF(ILMTFMT.EQ.0) THEN
        WRITE(IOUT) KPER,KSTP,NCOL,NROW,NLAY,TEXT,NCNH
      ELSEIF(ILMTFMT.EQ.1) THEN
        WRITE(IOUT,*) KPER,KSTP,NCOL,NROW,NLAY
        WRITE(IOUT,*) TEXT,NCNH
      ENDIF
C
C--IF THERE ARE NO CONSTANT-HEAD CELLS THEN SKIP
      IF(NCNH.LE.0) GOTO 1000
C
C--WRITE CONSTANT-HEAD CELL LOCATIONS AND RATES
      DO K=1,NLAY
        DO I=1,NROW
          DO J=1,NCOL
            IF(IBOUND(J,I,K).LT.0) THEN
              IF(ILMTFMT.EQ.0) THEN
C--SEAWAT WRITE SINGLE OR DOUBLE PRECISION DEPENDING ON INUHF
			  IF(IOUT.EQ.INUHF) WRITE(IOUT) K,I,J,BUFF(J,I,K)
	          IF(IOUT.NE.INUHF) WRITE(IOUT) K,I,J,REAL(BUFF(J,I,K),4)
              ENDIF
              IF(ILMTFMT.EQ.1) WRITE(IOUT,*) K,I,J,BUFF(J,I,K)
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
      SUBROUTINE LMT6HUF2VD(HNEW,IBOUND,CR,CC,CV,ISS,ISSCURRENT,DELT,
     & HOLD,SC1,BOTM,NBOTM,NCOL,NROW,NLAY,KSTP,KPER,HUFTHK,NHUF,IZON,
     & NZONAR,RMLT,NMLTAR,DELR,DELC,BUFF,IOUT,PS,ELEV,HSALT,IWTABLE,
     & INUHF)
C *********************************************************************
C SAVE FLOW ACROSS THREE CELL INTERFACES (QXX, QYY, QZZ), FLOW RATE TO
C OR FROM TRANSIENT FLUID-STORAGE (QSTO), AND LOCATIONS AND FLOW RATES
C OF CONSTANT-HEAD CELLS FOR USE BY MT3D.  THIS SUBROUTINE IS CALLED
C ONLY IF THE 'HUF' PACKAGE IS USED IN MODFLOW.
C *********************************************************************
C Modified from Harbaugh et al. (2000)
C last modified: 07-15-2003
C
      CHARACTER*16 TEXT
      DOUBLE PRECISION HNEW,HN,HD
      DIMENSION HNEW(NCOL,NROW,NLAY), IBOUND(NCOL,NROW,NLAY),
     & CR(NCOL,NROW,NLAY), CC(NCOL,NROW,NLAY), CV(NCOL,NROW,NLAY),
     & SC1(NCOL,NROW,NLAY), HUFTHK(NCOL,NROW,NLAY,NHUF,2),
     & IZON(NCOL,NROW,NZONAR),RMLT(NCOL,NROW,NMLTAR),
     & DELR(NCOL),DELC(NROW),BOTM(NCOL,NROW,0:NBOTM),
     & BUFF(NCOL,NROW,NLAY),HOLD(NCOL,NROW,NLAY)
C--SEAWAT: DIMENSION ADDITIONAL ARRAYS AND INCLUDE vdf.inc
	DIMENSION PS(NCOL,NROW,NLAY),ELEV(NCOL,NROW,NLAY),
     + HSALT(NCOL,NROW,NLAY)
	INCLUDE 'vdf.inc'
      COMMON /DISCOM/LBOTM(200),LAYCBD(200)
      COMMON /HUFCOM/LTHUF(200),HGUHANI(200),HGUVANI(200),LAYWT(200)
      COMMON /LINKMT3D/ILMTFMT
C
C--CALCULATE AND SAVE SATURATED THICKNESS
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
      DO K=1,NLAY
        IF(LTHUF(K).EQ.0) CYCLE
        DO I=1,NROW
          DO J=1,NCOL
            IF(IBOUND(J,I,K).NE.0) THEN
C--SEAWAT: USE HSALT RATHER THAN HNEW TO CALCULATE SATURATED THICKNESS
              TMP=HSALT(J,I,K)
              BUFF(J,I,K)=TMP-BOTM(J,I,LBOTM(K))
              THKLAY=BOTM(J,I,LBOTM(K)-1)-BOTM(J,I,LBOTM(K))
              IF(BUFF(J,I,K).GT.THKLAY) BUFF(J,I,K)=THKLAY
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C
C--SAVE THE CONTENTS OF THE BUFFER
      IF(ILMTFMT.EQ.0) THEN
        WRITE(IOUT) KPER,KSTP,NCOL,NROW,NLAY,TEXT
C--SEAWAT: WRITE AS SINGLE OR DOUBLE DEPENDING ON INUHF
	  IF (IOUT.EQ.INUHF) THEN
		WRITE(IOUT) BUFF
	  ELSE
		WRITE(IOUT) REAL(BUFF,4)
	  ENDIF
      ELSEIF(ILMTFMT.EQ.1) THEN
        WRITE(IOUT,*) KPER,KSTP,NCOL,NROW,NLAY
        WRITE(IOUT,*) TEXT
        WRITE(IOUT,*) BUFF
      ENDIF
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
      DO K=1,NLAY
        DO I=1,NROW
          DO J=1,NCM1
            IF(IBOUND(J,I,K).NE.0.AND.IBOUND(J+1,I,K).NE.0) THEN
C--SEAWAT: REPLACE WITH VARIABLE-DENSITY EQUATIONS
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
               IF(IWTABLE.EQ.1.AND.LTHUF(K).NE.0) 
     &            CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                    H2,HS2,PS2,Z2,TOP2,BOT2)
               DIS1=DELR(J+1)/2
               DIS2=DELR(J)/2
               AVGDENS=(DIS1*PS(J+1,I,K)+DIS2*PS(J,I,K))/(DIS1+DIS2)    
               D=CR(J,I,K)*(AVGDENS-DENSEREF)/DENSEREF*
     &          (Z1-Z2)
               HDIFF=H1-H2
               BUFF(J,I,K)=HDIFF*CR(J,I,K)+D
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C
C--RECORD CONTENTS OF BUFFER
      IF(ILMTFMT.EQ.0) THEN
        WRITE(IOUT) KPER,KSTP,NCOL,NROW,NLAY,TEXT
C--SEAWAT: WRITE AS SINGLE OR DOUBLE DEPENDING ON INUHF
	  IF (IOUT.EQ.INUHF) THEN
		WRITE(IOUT) BUFF
	  ELSE
		WRITE(IOUT) REAL(BUFF,4)
	  ENDIF
      ELSEIF(ILMTFMT.EQ.1) THEN
        WRITE(IOUT,*) KPER,KSTP,NCOL,NROW,NLAY
        WRITE(IOUT,*) TEXT
        WRITE(IOUT,*) BUFF
      ENDIF
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
      DO K=1,NLAY
        DO I=1,NRM1
          DO J=1,NCOL
            IF(IBOUND(J,I,K).NE.0.AND.IBOUND(J,I+1,K).NE.0) THEN
C--SEAWAT: USE VARIABLE-DENSITY EQUATIONS
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
               IF(IWTABLE.EQ.1.AND.LTHUF(K).NE.0) 
     &            CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                    H2,HS2,PS2,Z2,TOP2,BOT2)
               DIS1=DELC(I+1)/2
               DIS2=DELC(I)/2
               AVGDENS=(DIS1*PS(J,I+1,K)+DIS2*PS(J,I,K))/(DIS1+DIS2)
               D=CC(J,I,K)*(AVGDENS-DENSEREF)/DENSEREF*
     &          (Z1-Z2)
               HDIFF=H1-H2
               BUFF(J,I,K)=HDIFF*CC(J,I,K)+D
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C
C--RECORD CONTENTS OF BUFFER.
      IF(ILMTFMT.EQ.0) THEN
        WRITE(IOUT) KPER,KSTP,NCOL,NROW,NLAY,TEXT
C--SEAWAT: WRITE AS SINGLE OR DOUBLE DEPENDING ON INUHF
	  IF (IOUT.EQ.INUHF) THEN
		WRITE(IOUT) BUFF
	  ELSE
		WRITE(IOUT) REAL(BUFF,4)
	  ENDIF
      ELSEIF(ILMTFMT.EQ.1) THEN
        WRITE(IOUT,*) KPER,KSTP,NCOL,NROW,NLAY
        WRITE(IOUT,*) TEXT
        WRITE(IOUT,*) BUFF
      ENDIF
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
      DO K=1,NLM1
        DO I=1,NROW
          DO J=1,NCOL
C
            IF(IBOUND(J,I,K).NE.0.AND.IBOUND(J,I,K+1).NE.0) THEN
              HD=HNEW(J,I,K+1)
              IF(LTHUF(K+1).NE.0) THEN
C--SEAWAT: USE HSALT INSTEAD OF HD
                TMP=HSALT(J,I,K+1)
                TOP=BOTM(J,I,LBOTM(K+1)-1)
                IF(TMP.LT.TOP) HD=TOP
              ENDIF
              HDIFF=HNEW(J,I,K)-HD
              DIS1=BOTM(J,I,K)-ELEV(J,I,K+1)
              DIS2=ELEV(J,I,K)-BOTM(J,I,K)
              AVGDENS=(DIS1*PS(J,I,K+1)+DIS2*PS(J,I,K))/(DIS1+DIS2)
              D=CV(J,I,K)*(AVGDENS-DENSEREF)/DENSEREF*
     &           (ELEV(J,I,K)-ELEV(J,I,K+1))
              BUFF(J,I,K)=HDIFF*CV(J,I,K)+D
            ENDIF
C
          ENDDO
        ENDDO
      ENDDO
C
C--RECORD CONTENTS OF BUFFER.
      IF(ILMTFMT.EQ.0) THEN
        WRITE(IOUT) KPER,KSTP,NCOL,NROW,NLAY,TEXT
C--SEAWAT: WRITE AS SINGLE OR DOUBLE DEPENDING ON INUHF
	  IF (IOUT.EQ.INUHF) THEN
		WRITE(IOUT) BUFF
	  ELSE
		WRITE(IOUT) REAL(BUFF,4)
	  ENDIF
      ELSEIF(ILMTFMT.EQ.1) THEN
        WRITE(IOUT,*) KPER,KSTP,NCOL,NROW,NLAY
        WRITE(IOUT,*) TEXT
        WRITE(IOUT,*) BUFF
      ENDIF
C
  700 CONTINUE
C
C--CALCULATE AND SAVE GROUNDWATER STORAGE IF TRANSIENT
      IF(ISS.NE.0) GO TO 705
      TEXT='STO'
C
C--INITIALIZE and CLEAR BUFFER
      ZERO=0.
      ONE=1.
      TLED=ONE/DELT
      DO K=1,NLAY 
        DO I=1,NROW
          DO J=1,NCOL
            BUFF(J,I,K)=ZERO
          ENDDO
        ENDDO
      ENDDO
      IF(ISSCURRENT.NE.0) GOTO 704
C
C5------LOOP THROUGH EVERY CELL IN THE GRID.
      KT=0
      DO K=1,NLAY
        LC=LTHUF(K)
        IF(LC.NE.0) KT=KT+1
        DO I=1,NROW
          DO J=1,NCOL
C
C6------SKIP NO-FLOW AND CONSTANT-HEAD CELLS.
            IF(IBOUND(J,I,K).LE.0) CYCLE
            HN=HNEW(J,I,K)
            HO=HOLD(J,I,K)
C--SEAWAT: CALCULATE SALTHEADS FOR HN AND HO
	      HNSALT=SALTHEAD(HN,PS(J,I,K),ELEV(J,I,K))
	      HOSALT=SALTHEAD(HO,PS(J,I,K),ELEV(J,I,K))
            STRG=0.
C
C7-----CHECK LAYER TYPE TO SEE IF ONE STORAGE CAPACITY OR TWO.
            IF(LC.EQ.0) GO TO 285
            TOP=BOTM(J,I,LBOTM(K)-1)
            BOT=BOTM(J,I,LBOTM(K))
C--SEAWAT: USE SALTHEADS IN IF STATEMENT
            IF(HOSALT.GT.TOP.AND.HNSALT.GT.TOP) GOTO 285
C
C7A----TWO STORAGE CAPACITIES.
C---------------Compute SC1 Component
C--SEAWAT: USE SALTHEAD IN CONDITIONAL STATEMENT
            IF(HOSALT.GT.TOP) THEN
              STRG=SC1(J,I,K)*(HO-TOP)*TLED
C--SEAWAT: USE SALTHEAD IN CONDITIONAL STATEMENT
            ELSEIF(HNSALT.GT.TOP) THEN
              STRG=SC1(J,I,K)*TLED*(TOP-HN)
            ENDIF
C---------------Compute SC2 Component
C--SEAWAT: PASS HNSALT AND HOSALT FOR HN AND HO AND ALSO PASS IN HN AND HO
            CALL SVDF1HUF2SC2(1,J,I,K,TOP,BOT,HNSALT,HOSALT,TLED,CHCOF,
     &       STRG,HUFTHK,NCOL,NROW,NHUF,IZON,NZONAR,RMLT,NMLTAR,
     &       DELR(J)*DELC(I),IOUT,HN,HO)          
C------STRG=SOLD*(HOLD(J,I,K)-TP) + SNEW*TP - SNEW*HSING
            GOTO 288
C
C7B----ONE STORAGE CAPACITY.
  285       RHO=SC1(J,I,K)*TLED
            STRG=RHO*(HO-HN)
C
C8-----STORE CELL-BY-CELL FLOW IN BUFFER
  288       BUFF(J,I,K)=STRG
C
          ENDDO
        ENDDO
      ENDDO
C
C--RECORD CONTENTS OF BUFFER.
  704 IF(ILMTFMT.EQ.0) THEN
        WRITE(IOUT) KPER,KSTP,NCOL,NROW,NLAY,TEXT
C--SEAWAT: WRITE AS SINGLE OR DOUBLE DEPENDING ON INUHF
	  IF (IOUT.EQ.INUHF) THEN
		WRITE(IOUT) BUFF
	  ELSE
		WRITE(IOUT) REAL(BUFF,4)
	  ENDIF
      ELSEIF(ILMTFMT.EQ.1) THEN
        WRITE(IOUT,*) KPER,KSTP,NCOL,NROW,NLAY
        WRITE(IOUT,*) TEXT
        WRITE(IOUT,*) BUFF
      ENDIF
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
      DO K=1,NLAY
        DO I=1,NROW
          DO J=1,NCOL
C
C--IF CELL IS NOT CONSTANT HEAD SKIP IT & GO ON TO NEXT CELL.
            IF (IBOUND(J,I,K).GE.0) CYCLE
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
C--SEAWAT: VARIABLE-DENSITY FORM OF EQUATIONS
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
	         IF(IWTABLE.EQ.1.AND.LTHUF(K).NE.0) 
     &            CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                    H2,HS2,PS2,Z2,TOP2,BOT2)
               DIS1=DELR(J-1)/2
               DIS2=DELR(J)/2
               AVGDENS=(DIS1*PS(J-1,I,K)+DIS2*PS(J,I,K))/(DIS1+DIS2)
               D=CR(J-1,I,K)*(AVGDENS-DENSEREF)/DENSEREF*(Z1-Z2)      
               HDIFF=H1-H2
C
C--CALCULATE FLOW THROUGH THIS FACE INTO THE ADJACENT CELL.
               X1=HDIFF*CR(J-1,I,K)+D
C
C--CALCULATE FLOW THROUGH THE RIGHT FACE
   30       IF(J.EQ.NCOL) GO TO 60
            IF(IBOUND(J+1,I,K).EQ.0) GO TO 60
C--SEAWAT: USE VARIABLE-DENSITY FORM OF EQUATIONS
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
               IF(IWTABLE.EQ.1.AND.LTHUF(K).NE.0) 
     &            CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                    H2,HS2,PS2,Z2,TOP2,BOT2)
               DIS1=DELR(J+1)/2
               DIS2=DELR(J)/2
               AVGDENS=(DIS1*PS(J+1,I,K)+DIS2*PS(J,I,K))/(DIS1+DIS2)

               D=CR(J,I,K)*(AVGDENS-DENSEREF)/DENSEREF*(Z1-Z2)
               HDIFF=H1-H2
               X2=HDIFF*CR(J,I,K)+D
C
C--CALCULATE FLOW THROUGH THE BACK FACE.
   60       IF(I.EQ.1) GO TO 90
            IF (IBOUND(J,I-1,K).EQ.0) GO TO 90
C--SEAWAT: USE VARIABLE-DENSITY FORM OF EQUATIONS
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
               IF(IWTABLE.EQ.1.AND.LTHUF(K).NE.0) 
     &            CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                    H2,HS2,PS2,Z2,TOP2,BOT2)
               DIS1=DELC(I-1)/2
               DIS2=DELC(I)/2
               AVGDENS=(DIS1*PS(J,I-1,K)+DIS2*PS(J,I,K))/(DIS1+DIS2)
               D=CC(J,I-1,K)*(AVGDENS-DENSEREF)/DENSEREF*(Z1-Z2)
               HDIFF=H1-H2
               X3=HDIFF*CC(J,I-1,K)+D
C
C--CALCULATE FLOW THROUGH THE FRONT FACE.
   90       IF(I.EQ.NROW) GO TO 120
            IF(IBOUND(J,I+1,K).EQ.0) GO TO 120
C--SEAWAT: USE VARIABLE-DENSITY FORM OF EQUATIONS
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
               IF(IWTABLE.EQ.1.AND.LTHUF(K).NE.0) 
     &            CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                    H2,HS2,PS2,Z2,TOP2,BOT2)
               DIS1=DELC(I+1)/2
               DIS2=DELC(I)/2
               AVGDENS=(DIS1*PS(J,I+1,K)+DIS2*PS(J,I,K))/(DIS1+DIS2)
               D=CC(J,I,K)*(AVGDENS-DENSEREF)/DENSEREF*(Z1-Z2)
               HDIFF=H1-H2
               X4=HDIFF*CC(J,I,K)+D
C
C--CALCULATE FLOW THROUGH THE UPPER FACE
C--SEAWAT: USE VARIABLE-DENSITY FORM OF EQUATIONS
  120       IF(K.EQ.1) GO TO 150
            IF (IBOUND(J,I,K-1).EQ.0) GO TO 150
            HD=HNEW(J,I,K)
            IF(LTHUF(K).EQ.0) GO TO 122
            TMP=HD
            TOP=BOTM(J,I,LBOTM(K)-1)
            IF(TMP.LT.TOP) HD=TOP
  122       HDIFF=HD-HNEW(J,I,K-1)
            DIS1=ELEV(J,I,K-1)-BOTM(J,I,K-1)
            DIS2=BOTM(J,I,K-1)-ELEV(J,I,K)
            AVGDENS=(DIS1*PS(J,I,K-1)+DIS2*PS(J,I,K))/(DIS1+DIS2)
            D=CV(J,I,K-1)*(AVGDENS-DENSEREF)/DENSEREF*
     &        (ELEV(J,I,K)-ELEV(J,I,K-1))
            X5=HDIFF*CV(J,I,K-1)+D
C
C--CALCULATE FLOW THROUGH THE LOWER FACE
C--SEAWAT: USE VARIABLE-DENSITY FORM OF EQUATIONS
  150       IF(K.EQ.NLAY) GO TO 180
            IF(IBOUND(J,I,K+1).EQ.0) GO TO 180
            HD=HNEW(J,I,K+1)
            IF(LTHUF(K+1).EQ.0) GO TO 152
            TMP=HD
            TOP=BOTM(J,I,LBOTM(K+1)-1)
            IF(TMP.LT.TOP) HD=TOP
  152       HDIFF=HNEW(J,I,K)-HD
            DIS1=BOTM(J,I,K)-ELEV(J,I,K+1)
            DIS2=ELEV(J,I,K)-BOTM(J,I,K)
            AVGDENS=(DIS1*PS(J,I,K+1)+DIS2*PS(J,I,K))/(DIS1+DIS2)
            D=CV(J,I,K)*(AVGDENS-DENSEREF)/DENSEREF*
     &        (ELEV(J,I,K)-ELEV(J,I,K+1))
            X6=HDIFF*CV(J,I,K)+D
C
C--SUM UP FLOWS THROUGH SIX SIDES OF CONSTANT HEAD CELL.
  180       BUFF(J,I,K)=X1+X2+X3+X4+X5+X6
C
          ENDDO
        ENDDO
      ENDDO
C
C--RECORD CONTENTS OF BUFFER.
      IF(ILMTFMT.EQ.0) THEN
        WRITE(IOUT) KPER,KSTP,NCOL,NROW,NLAY,TEXT,NCNH
      ELSEIF(ILMTFMT.EQ.1) THEN
        WRITE(IOUT,*) KPER,KSTP,NCOL,NROW,NLAY
        WRITE(IOUT,*) TEXT,NCNH
      ENDIF
C
C--IF THERE ARE NO CONSTANT-HEAD CELLS THEN SKIP
      IF(NCNH.LE.0) GOTO 1000
C
C--WRITE CONSTANT-HEAD CELL LOCATIONS AND RATES
      DO K=1,NLAY
        DO I=1,NROW
          DO J=1,NCOL
            IF(IBOUND(J,I,K).LT.0) THEN
              IF(ILMTFMT.EQ.0) THEN
C--SEAWAT WRITE SINGLE OR DOUBLE PRECISION DEPENDING ON INUHF
			  IF(IOUT.EQ.INUHF) WRITE(IOUT) K,I,J,BUFF(J,I,K)
	          IF(IOUT.NE.INUHF) WRITE(IOUT) K,I,J,REAL(BUFF(J,I,K),4)
              ENDIF
              IF(ILMTFMT.EQ.1) WRITE(IOUT,*) K,I,J,BUFF(J,I,K)
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
      SUBROUTINE LMT6DRN6VD(NDRAIN,MXDRN,NDRNVL,DRAI,HNEW,
     &  NCOL,NROW,NLAY,IBOUND,KSTP,KPER,IOUT,PS,ELEV,INUHF)
C ********************************************************************
C SAVE DRAIN CELL LOCATIONS AND VOLUMETRIC FLOW RATES FOR USE BY MT3D.
C ********************************************************************
C Modified from Harbaugh et al. (2000)
C last modified: 05-01-2002
C
      CHARACTER*16 TEXT
C--SEAWAT: ADD EL
      DOUBLE PRECISION HNEW,CEL,CC,HHNEW,EL,EEL
      DIMENSION DRAI(NDRNVL,MXDRN),HNEW(NCOL,NROW,NLAY),
     &          IBOUND(NCOL,NROW,NLAY)
      COMMON /LINKMT3D/ILMTFMT
C--SEAWAT: DIMENSION ADDITIONAL ARRAYS
      DIMENSION PS(NCOL,NROW,NLAY),ELEV(NCOL,NROW,NLAY)
	INCLUDE 'vdf.inc'
      COMMON /DRNCOM/DRNAUX(5)
      CHARACTER*16 DRNAUX
C-------------------------------------------------------------------
      TEXT='DRN'
C
C--WRITE AN IDENTIFYING HEADER
      IF(ILMTFMT.EQ.0) THEN
        WRITE(IOUT) KPER,KSTP,NCOL,NROW,NLAY,TEXT,NDRAIN
      ELSEIF(ILMTFMT.EQ.1) THEN
        WRITE(IOUT,*) KPER,KSTP,NCOL,NROW,NLAY
        WRITE(IOUT,*) TEXT,NDRAIN
      ENDIF
C
C--IF THERE ARE NO DRAINS THEN SKIP
      IF(NDRAIN.LE.0) RETURN
C
C--SEAWAT: GET ZDRN IF AUX IS SPECIFIED
      LOCZDRN=0
      DO I=1,5
         IF(DRNAUX(I).EQ.'DRNBELEV') LOCZDRN=I+5
      ENDDO
C
C--FOR EACH DRAIN ACCUMULATE DRAIN FLOW
      DO L=1,NDRAIN
C
C--GET LAYER, ROW & COLUMN OF CELL CONTAINING REACH.
        IL=DRAI(1,L)
        IR=DRAI(2,L)
        IC=DRAI(3,L)
        Q=0.
C
C--CALCULATE Q FOR ACTIVE CELLS
        IF(IBOUND(IC,IR,IL).GT.0) THEN
C
C--GET DRAIN PARAMETERS FROM DRAIN LIST.
          EL=DRAI(4,L)
C--SEAWAT: MAKE EL FRESHWATER EQUIVLANET
          ZDRN=ELEV(IC,IR,IL)
          IF(LOCZDRN.GT.0) ZDRN=DRAI(LOCZDRN,L)
          EEL=FEHEAD(EL,PS(IC,IR,IL),ZDRN)
          C=DRAI(5,L)
          CEL=C*EL
          CC=C
C
C--IF HEAD LOWER THAN DRAIN THEN FORGET THIS CELL.
C--OTHERWISE, CALCULATE Q=C*(EL-HHNEW).
C--SEAWAT: USE VARIABLE-DENSITY FORM OF EQUATION
	   IF (SALTHEAD(HNEW(IC,IR,IL),PS(IC,IR,IL),ELEV(IC,IR,IL)).GT.EL) 
     &       Q=CC*(EEL-HNEW(IC,IR,IL)-(PS(IC,IR,IL)-DENSEREF)/
     &         DENSEREF*(ELEV(IC,IR,IL)-ZDRN))
         ENDIF
C
C--WRITE DRAIN LOCATION AND RATE
        IF(ILMTFMT.EQ.0) THEN
	    IF(IOUT.EQ.INUHF) WRITE(IOUT) IL,IR,IC,Q
          IF(IOUT.NE.INUHF) WRITE(IOUT) IL,IR,IC,REAL(Q,4)
	  ENDIF
	  IF(ILMTFMT.EQ.1) WRITE(IOUT,*) IL,IR,IC,Q
      ENDDO
C
C--RETURN
      RETURN
      END
C
      SUBROUTINE LMT6RIV6VD(NRIVER,MXRIVR,NRIVVL,RIVR,IBOUND,HNEW,
     &  NCOL,NROW,NLAY,KSTP,KPER,IOUT,ELEV,PS,MTDNCONC,MXSS,NSS,SS,
     &  NCOMP,SSMC,INUHF)
C *********************************************************************
C SAVE RIVER CELL LOCATIONS AND VOLUMETRIC FLOW RATES FOR USE BY MT3D.
C--SEAWAT: IMPLEMENT VD FORM OF DARCY'S LAW 
C *********************************************************************
C Modified from Harbaugh et al. (2000)
C last modified: 05-01-2002
C
      CHARACTER*16 TEXT
C--SEAWAT: ADD HRIV
      DOUBLE PRECISION HNEW,CHRIV,CCRIV,RRBOT,HHNEW,HRIV
      DIMENSION RIVR(NRIVVL,MXRIVR),IBOUND(NCOL,NROW,NLAY),
     &          HNEW(NCOL,NROW,NLAY)
      COMMON /LINKMT3D/ILMTFMT
C--SEAWAT: ADD ADDITIONAL ARRAYS 
      DIMENSION ELEV(NCOL,NROW,NLAY),PS(NCOL,NROW,NLAY),
     &          SS(6,MXSS),SSMC(NCOMP,MXSS)
C--SEAWAT: ADD AUXILLARY VARIABLES
      COMMON /RIVCOM/RIVAUX(5)
      CHARACTER*16 RIVAUX
	INCLUDE 'vdf.inc'
C     ------------------------------------------------------------------
      TEXT='RIV'
C
C--WRITE AN IDENTIFYING HEADER
      IF(ILMTFMT.EQ.0) THEN
        WRITE(IOUT) KPER,KSTP,NCOL,NROW,NLAY,TEXT,NRIVER
      ELSEIF(ILMTFMT.EQ.1) THEN
        WRITE(IOUT,*) KPER,KSTP,NCOL,NROW,NLAY
        WRITE(IOUT,*) TEXT,NRIVER
      ENDIF
C
C--IF NO REACHES SKIP
      IF(NRIVER.LE.0) RETURN
C
C--SEAWAT: CHECK IF RBDTHK AND RIVDEN STORED IN AUX VARIABLE
      LOCRBDTHK=0
      DO I=1,5
		IF(RIVAUX(I).EQ.'RBDTHK') LOCRBDTHK=I+6
      ENDDO   
      LOCRIVDEN=0
      DO I=1,5
		IF(RIVAUX(I).EQ.'RIVDEN') LOCRIVDEN=I+6
      ENDDO   

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
C--SEAWAT: RELOCATED RRBOT=RBOT
          RRBOT=RBOT
          RBDTHK=ABS(RBOT-ELEV(IC,IR,IL))
          IF(LOCRBDTHK.GT.0) RBDTHK=RIVR(LOCRBDTHK,L)
C--SEAWAT: SET RIVER DENSITY
		RIVDENS=PS(IC,IR,IL)
          IF(LOCRIVDEN.GT.0) RIVDENS=RIVR(LOCRIVDEN,L)
          IF(MTDNCONC.GT.0) RIVDENS=SSMDENSE(IC,IR,IL,4,MXSS,NSS,
     &                     SS,NCOMP,SSMC,MTDNCONC)    
C--SEAWAT: COMPUTE HRIV AS FE 
          HRIV=FEHEAD(HRIV,RIVDENS,RBOT+RBDTHK)
          HHNEW=HNEW(IC,IR,IL)
          HTEMP=SALTHEAD(HNEW(IC,IR,IL),PS(IC,IR,IL),ELEV(IC,IR,IL))
C--SEAWAT: CALCULATE FRESHWATER HEAD AT RBOT USING HEAD IN MODEL CELL
          HFRBOT=HHNEW+(PS(IC,IR,IL)-DENSEREF)/DENSEREF*
     &     (ELEV(IC,IR,IL)-RBOT)                
C--SEAWAT: COMPARISON DONE WITH SALTHEAD            
          IF(HTEMP.GT.RRBOT) THEN
             RHOAVG=(RIVDENS+PS(IC,IR,IL))/2
             DIRECT=-(HRIV-HFRBOT+(RHOAVG-DENSEREF)/DENSEREF*RBDTHK)
C--SEAWAT:  DIRECT IS POSITIVE, FLOW IS UP INTO RIVER
C--SEAWAT:  DIRECT IS NEGATIVE, FLOW IS DOWN INTO GROUNDWATER
             IF (DIRECT.GT.0.) RIVDENS=PS(IC,IR,IL)
             RATE=CRIV*(HRIV-HFRBOT+(RHOAVG-DENSEREF)/DENSEREF*RBDTHK)
          ENDIF
          IF(HTEMP.LE.RRBOT) THEN
             RATE=CRIV*(HRIV-RBOT+(RIVDENS-DENSEREF)/DENSEREF*RBDTHK)
          ENDIF
        ENDIF
C
C--WRITE RIVER REACH LOCATION AND RATE
        IF(ILMTFMT.EQ.0) THEN
 	    IF(IOUT.EQ.INUHF) WRITE(IOUT) IL,IR,IC,RATE
          IF(IOUT.NE.INUHF) WRITE(IOUT) IL,IR,IC,REAL(RATE,4)
        ELSEIF(ILMTFMT.EQ.1) THEN
          WRITE(IOUT,*) IL,IR,IC,RATE
        ENDIF
      ENDDO
C
C--RETURN
      RETURN
      END
C
      SUBROUTINE VDFCALCIRCH(NROW,NCOL,NLAY,IBOUND,IRCH,NRCHOP)
C**********************************************************************
C--SEAWAT: SUBROUTINE THAT CALCS THE IRCH ARRAY
C**********************************************************************
      DIMENSION IBOUND(NCOL,NROW,NLAY),IRCH(NCOL,NROW)
      LOGICAL DONE

C--SEAWAT: IRCH ALREADY READ IF NRCHOP IS 1 OR 2 
      IF(NRCHOP.LT.3) RETURN

C--SEAWAT: COMPUTE IRCH IF NRCHOP IS EQUAL TO 3
      DO IR=1,NROW
      DO IC=1,NCOL
		IRCH(IC,IR)=1
		DONE=.FALSE.
   10		IF(IBOUND(IC,IR,IRCH(IC,IR)).NE.0) 
     &		DONE=.TRUE.
		IF(.NOT.DONE) THEN
			IF(IRCH(IC,IR).EQ.NLAY) CYCLE
			IRCH(IC,IR)=IRCH(IC,IR)+1
			GOTO 10
		ENDIF
      ENDDO
      ENDDO
C--SEAWAT: RETURN
      RETURN
      END
C
C
      SUBROUTINE LMT6RCH6VD(NRCHOP,IRCH,RECH,IBOUND,NROW,NCOL,NLAY,
     &  KSTP,KPER,BUFF,IOUT,INUHF)
C *******************************************************************
C SAVE REACHARGE LAYER INDICES (IF NLAY>1) AND VOLUMETRIC FLOW RATES
C FOR USE BY MT3D.
C *******************************************************************
C Modified from Harbaugh et al. (2000)
C last modified: 05-01-2002
C
      CHARACTER*16 TEXT
      DIMENSION IRCH(NCOL,NROW),RECH(NCOL,NROW),
     &  IBOUND(NCOL,NROW,NLAY),BUFF(NCOL,NROW,NLAY)
      COMMON /LINKMT3D/ILMTFMT
      TEXT='RCH'
C
C--WRITE AN IDENTIFYING HEADER
      IF(ILMTFMT.EQ.0) THEN
        WRITE(IOUT) KPER,KSTP,NCOL,NROW,NLAY,TEXT
      ELSEIF(ILMTFMT.EQ.1) THEN
        WRITE(IOUT,*) KPER,KSTP,NCOL,NROW,NLAY
        WRITE(IOUT,*) TEXT
      ENDIF
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
        IL=1
        IF(ILMTFMT.EQ.0) WRITE(IOUT)   ((IL,J=1,NCOL),I=1,NROW)
        IF(ILMTFMT.EQ.1) WRITE(IOUT,*) ((IL,J=1,NCOL),I=1,NROW)
C
C--STORE RECH RATE IN BUFF FOR ACTIVE CELLS
        DO I=1,NROW
          DO J=1,NCOL
            IF(IBOUND(J,I,1).GT.0) BUFF(J,I,1)=RECH(J,I)
          ENDDO
        ENDDO
        IF(ILMTFMT.EQ.0) THEN
	    IF(IOUT.EQ.INUHF) WRITE(IOUT)((BUFF(J,I,1),J=1,NCOL),I=1,NROW)
	    IF(IOUT.NE.INUHF) WRITE(IOUT)((REAL(BUFF(J,I,1),4),J=1,NCOL),
     &                                                       I=1,NROW)
	  ENDIF        
	  IF(ILMTFMT.EQ.1) WRITE(IOUT,*) ((BUFF(J,I,1),J=1,NCOL),I=1,NROW)
C
C--IF NRCHOP=2 OR 3 RECH IS IN LAYER SHOWN IN INDICATOR ARRAY(IRCH).
      ELSEIF(NRCHOP.NE.1) THEN
        IF(ILMTFMT.EQ.0) WRITE(IOUT)   ((IRCH(J,I),J=1,NCOL),I=1,NROW)
        IF(ILMTFMT.EQ.1) WRITE(IOUT,*) ((IRCH(J,I),J=1,NCOL),I=1,NROW)
C
C--STORE RECH RATE IN BUFF FOR ACTIVE CELLS
        DO I=1,NROW
          DO J=1,NCOL
            IL=IRCH(J,I)
            IF(IBOUND(J,I,IL).GT.0) BUFF(J,I,1)=RECH(J,I)
          ENDDO
        ENDDO
        IF(ILMTFMT.EQ.0) THEN
	    IF(IOUT.EQ.INUHF) WRITE(IOUT)((BUFF(J,I,1),J=1,NCOL),I=1,NROW)
	    IF(IOUT.NE.INUHF) WRITE(IOUT)((REAL(BUFF(J,I,1),4),J=1,NCOL),
     &                                                       I=1,NROW)
	  ENDIF
        IF(ILMTFMT.EQ.1) WRITE(IOUT,*) ((BUFF(J,I,1),J=1,NCOL),I=1,NROW)
      ENDIF
C
C--RETURN
      RETURN
      END

C
      SUBROUTINE VDFCALCIEVT(NROW,NCOL,NLAY,IBOUND,IEVT,NEVTOP)
C**********************************************************************
C--SEAWAT: SUBROUTINE THAT CALCS THE IEVT ARRAY
C**********************************************************************
      DIMENSION IBOUND(NCOL,NROW,NLAY),IEVT(NCOL,NROW)
      LOGICAL DONE
C------------------------------------------------------

C--SEAWAT: IEVT ALREADY READ IF NEVTOP IS 1 OR 2
      IF(NEVTOP.LT.3) RETURN

C--SEAWAT: COMPUTE IEVT IF NEVTOP IS EQUAL TO 3
      DO IR=1,NROW
      DO IC=1,NCOL
		IEVT(IC,IR)=1
		DONE=.FALSE.
   10		IF(IBOUND(IC,IR,IEVT(IC,IR)).NE.0) DONE=.TRUE.
		IF(.NOT.DONE) THEN
			IF(IEVT(IC,IR).EQ.NLAY) CYCLE
			IEVT(IC,IR)=IEVT(IC,IR)+1
			GOTO 10
		ENDIF
      ENDDO
      ENDDO
C--SEAWAT: RETURN
      RETURN
      END
C
C
      SUBROUTINE LMT6EVT6VD(NEVTOP,IEVT,EVTR,EXDP,SURF,IBOUND,HNEW,
     &  NCOL,NROW,NLAY,KSTP,KPER,BUFF,IOUT,PS,ELEV,INUHF)
C ******************************************************************
C SAVE EVAPOTRANSPIRATION LAYER INDICES (IF NLAY>1) AND VOLUMETRIC
C FLOW RATES FOR USE BY MT3D.
C ******************************************************************
C Modified from Harbaugh et al. (2000)
C last modified: 05-01-2002
C
      CHARACTER*16 TEXT
      DOUBLE PRECISION HNEW,HH,XX,DD,SS
      DIMENSION IEVT(NCOL,NROW),EVTR(NCOL,NROW),EXDP(NCOL,NROW),
     &          SURF(NCOL,NROW),IBOUND(NCOL,NROW,NLAY),
     &          HNEW(NCOL,NROW,NLAY),BUFF(NCOL,NROW,NLAY)
      COMMON /LINKMT3D/ILMTFMT
C--SEAWAT: DIMENSION ADDITIONAL ARRAYS
      DIMENSION PS(NCOL,NROW,NLAY),ELEV(NCOL,NROW,NLAY)
C-------------------------------------------------------------------
      TEXT='EVT'
C
C--WRITE AN IDENTIFYING HEADER
      IF(ILMTFMT.EQ.0) THEN
        WRITE(IOUT) KPER,KSTP,NCOL,NROW,NLAY,TEXT
      ELSEIF(ILMTFMT.EQ.1) THEN
        WRITE(IOUT,*) KPER,KSTP,NCOL,NROW,NLAY
        WRITE(IOUT,*) TEXT
      ENDIF
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
C--IF OPTION 1 SET THE LAYER INDEX EQUAL TO 1
          IF(NEVTOP.EQ.1) THEN
            IL=1
C
C--IF OPTION 2 OR 3 GET LAYER INDEX FROM IEVT ARRAY
          ELSEIF(NEVTOP.NE.1) THEN
            IL=IEVT(IC,IR)
          ENDIF
C
C--IF CELL IS EXTERNAL THEN IGNORE IT.
          IF(IBOUND(IC,IR,IL).LE.0) CYCLE
          C=EVTR(IC,IR)
          S=SURF(IC,IR)
          SS=S
C          HH=HNEW(IC,IR,IL)
          HH=SALTHEAD(HNEW(IC,IR,IL),PS(IC,IR,IL),ELEV(IC,IR,IL))
          DENSE=PS(IC,IR,IL)
C
C--IF AQUIFER HEAD => SURF,SET Q=MAX ET RATE
          IF(HH.GE.SS) THEN
            Q=-C
C
C--IF DEPTH=>EXTINCTION DEPTH, ET IS 0
C--OTHERWISE, LINEAR RANGE: Q=-EVTR(H-EXEL)/EXDP
          ELSE
            X=EXDP(IC,IR)
            XX=X
            DD=SS-HH
            IF(DD.GE.XX) THEN
              Q=0
            ELSE
              Q=C*DD/X-C
            ENDIF
          ENDIF
C
C--ADD Q TO BUFFER 1
          BUFF(IC,IR,1)=Q
        ENDDO
      ENDDO
C
C--RECORD THEM.
      IF(NEVTOP.EQ.1) THEN
        IL=1
        IF(ILMTFMT.EQ.0) WRITE(IOUT)   ((IL,J=1,NCOL),I=1,NROW)
        IF(ILMTFMT.EQ.1) WRITE(IOUT,*) ((IL,J=1,NCOL),I=1,NROW)
      ELSEIF(NEVTOP.NE.1) THEN
        IF(ILMTFMT.EQ.0) WRITE(IOUT)   ((IEVT(J,I),J=1,NCOL),I=1,NROW)
        IF(ILMTFMT.EQ.1) WRITE(IOUT,*) ((IEVT(J,I),J=1,NCOL),I=1,NROW)
      ENDIF
C
C--SEAWAT: WRITE SINGLE OR DOUBLE PRECISION DEPENDING ON INUHF
      IF(ILMTFMT.EQ.0) THEN
        IF (IOUT.EQ.INUHF) THEN
	     WRITE(IOUT) ((BUFF(J,I,1),J=1,NCOL),I=1,NROW)
	  ELSE
		 WRITE(IOUT) ((REAL(BUFF(J,I,1),4),J=1,NCOL),I=1,NROW)
	  ENDIF
      ELSEIF(ILMTFMT.EQ.1) THEN
        WRITE(IOUT,*) ((BUFF(J,I,1),J=1,NCOL),I=1,NROW)
      ENDIF
C
C--RETURN
      RETURN
      END
C
C
      SUBROUTINE LMT6GHB6VD(NBOUND,MXBND,NGHBVL,BNDS,HNEW,
     &  NCOL,NROW,NLAY,IBOUND,KSTP,KPER,IOUT,ELEV,PS,MTDNCONC,MXSS,NSS,
     &  SS,NCOMP,SSMC,INUHF)
C *****************************************************************
C SAVE HEAD-DEPENDENT BOUNDARY CELL LOCATIONS AND VOLUMETRIC FLOW
C RATES FOR USE BY MT3D.
C--SEAWAT: MODIFIED TO USE VD FORM OF DARCY'S LAW
C *****************************************************************
C Modified from Harbaugh et al. (2000)
C last modified: 05-01-2002
C
      CHARACTER*16 TEXT
C--SEAWAT: MAKE HB AND HHB DOUBLE PERCISION SO FEHEAD CAN BE USED
      DOUBLE PRECISION HNEW,CHB,CC,HHNEW,HB,HHB
      DIMENSION BNDS(NGHBVL,MXBND),
     &           HNEW(NCOL,NROW,NLAY),IBOUND(NCOL,NROW,NLAY)
      COMMON /LINKMT3D/ILMTFMT
C--SEAWAT: DIMENSION ADDITIONAL ARRAYS
      DIMENSION ELEV(NCOL,NROW,NLAY),PS(NCOL,NROW,NLAY),SS(6,MXSS),
     +          SSMC(NCOMP,MXSS)
      COMMON /GHBCOM/GHBAUX(5)
      CHARACTER*16 GHBAUX
	INCLUDE 'vdf.inc'
C------------------------------------------------------------------
      TEXT='GHB'
C
C--WRITE AN IDENTIFYING HEADER
      IF(ILMTFMT.EQ.0) THEN
        WRITE(IOUT) KPER,KSTP,NCOL,NROW,NLAY,TEXT,NBOUND
      ELSEIF(ILMTFMT.EQ.1) THEN
        WRITE(IOUT,*) KPER,KSTP,NCOL,NROW,NLAY
        WRITE(IOUT,*) TEXT,NBOUND
      ENDIF
C
C--IF NO BOUNDARIES THEN SKIP
      IF(NBOUND.LE.0) RETURN
C
C--SEAWAT: CHECK FOR GHBELEV AND GHBDENS AS AUXILIARY VARIABLES
      LOCGHBELEV=0
      DO I=1,5
		IF(GHBAUX(I).EQ.'GHBELEV') LOCGHBELEV=I+5
      ENDDO 
      LOCGHBDENS=0
      DO I=1,5
		IF(GHBAUX(I).EQ.'GHBDENS') LOCGHBDENS=I+5
      ENDDO 

C--FOR EACH GENERAL HEAD BOUND ACCUMULATE FLOW INTO AQUIFER
      DO L=1,NBOUND
C
C--GET LAYER, ROW AND COLUMN OF EACH GENERAL HEAD BOUNDARY.
        IL=BNDS(1,L)
        IR=BNDS(2,L)
        IC=BNDS(3,L)
C
C--RATE=0 IF IBOUND=<0
        RATE=0.
        IF(IBOUND(IC,IR,IL).GT.0) THEN
C
C--GET PARAMETERS FROM BOUNDARY LIST.
          HB=BNDS(4,L)
C--SEAWAT: CONVERT TO FRESHWATER EQUIVALENT
          HHB=HB
          HB=FEHEAD(HHB,PS(IC,IR,IL),ELEV(IC,IR,IL))
          HHNEW=HNEW(IC,IR,IL)
          C=BNDS(5,L)
          CHB=C*HB
          CC=C
          GHBELEV=ELEV(IC,IR,IL)
          IF(LOCGHBELEV.GT.0) GHBELEV=BNDS(LOCGHBELEV,L)
          GHBDENS=PS(IC,IR,IL)
          IF(LOCGHBDENS.GT.0) GHBDENS=BNDS(LOCGHBDENS,L)
          IF(MTDNCONC.GT.0) GHBDENS=SSMDENSE(IC,IR,IL,5,MXSS,NSS,
     &                     SS,NCOMP,SSMC,MTDNCONC)
          RHOAVG=(PS(IC,IR,IL)+GHBDENS)/2
C
C--CALCULATE THE FOW RATE INTO THE CELL
          RATE=C*(HB-HHNEW+(RHOAVG-DENSEREF)/DENSEREF*
     &     (GHBELEV-ELEV(IC,IR,IL)))    
C          RATE=CHB-CC*HHNEW
        ENDIF
C
C--WRITE HEAD DEP. BOUND. LOCATION AND RATE
C--SEAWAT: WRITE SINGLE OR DOUBLE PRECISION DEPENDING ON INUHF
        IF(ILMTFMT.EQ.0) THEN
          IF(IOUT.EQ.INUHF) THEN
		   WRITE(IOUT) IL,IR,IC,RATE
		ELSE
			WRITE(IOUT) IL,IR,IC,REAL(RATE,4)
		ENDIF
        ELSEIF(ILMTFMT.EQ.1) THEN
          WRITE(IOUT,*) IL,IR,IC,RATE
        ENDIF
      ENDDO
C
C--RETURN
      RETURN
      END