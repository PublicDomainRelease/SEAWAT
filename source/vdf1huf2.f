c======================================================================
C--SEAWAT: PASS IN HSALT
      SUBROUTINE VDF1HUF2SP(
     & IBOUND,HNEW,CR,CC,CV,DELR,DELC,BOTM,HK,VKA,SC1,
     & ITRSS,NCOL,NROW,NLAY,IOUT,WETDRY,NHUF,NBOTM,RMLT,IZON,NMLTAR,
     & NZONAR,HUFTHK,HKCC,HDRY,KITER,KSTP,KPER,IHGUFLG,HUFTMP,
     & ILVDA,VDHD,VDHT,IWETIT,IHDWET,WETFCT,GS,A9)
C
C     ******************************************************************
C     SUBSTITUTE AND PREPARE DATA FOR HYDROGEOLOGIC-UNIT FLOW PACKAGE
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE VDFMODULE,   ONLY: HSALT
C
      DOUBLE PRECISION HNEW
      CHARACTER*4 PTYPE(6)
      CHARACTER*10 HGUNAM
      CHARACTER*24 ANAME(6)
C
      DIMENSION IBOUND(NCOL,NROW,NLAY),HNEW(NCOL,NROW,NLAY),
     &    CR(NCOL,NROW,NLAY),CC(NCOL,NROW,NLAY),
     &    CV(NCOL,NROW,NLAY),DELR(NCOL),DELC(NROW),
     &    BOTM(NCOL,NROW,0:NBOTM),HK(NCOL,NROW,NLAY),
     &    VKA(NCOL,NROW,NLAY),SC1(NCOL,NROW,NLAY),
     &    WETDRY(NCOL,NROW,NLAY),RMLT(NCOL,NROW,NMLTAR),
     &    IZON(NCOL,NROW,NZONAR),HUFTHK(NCOL,NROW,NHUF,2),
     &    HKCC(NCOL,NROW,NLAY),HUFHK(999),HUFHANI(999),
     &    HUFVK(999),HUFSS(999),IHGUFLG(5,NHUF),
     &    HUFSY(999),VDHD(NCOL,NROW,NLAY),HUFTMP(NCOL,NROW,NHUF),
     &    VDHT(NCOL*NROW*NLAY,3),HUFKDEP(999),GS(NCOL,NROW),
     &    A9(NCOL*NROW*NLAY,5)
C
      INCLUDE 'param.inc'
      COMMON /HUFCOM/HGUHANI(999),HGUVANI(999),LTHUF(999),LAYWT(999)
      COMMON /HUFCOMC/HGUNAM(999)
      DATA ANAME(1) /'   HYD. COND. ALONG ROWS'/
      DATA ANAME(2) /'  HORIZ. ANI. (COL./ROW)'/
      DATA ANAME(3) /'     VERTICAL HYD. COND.'/
      DATA ANAME(4) /'        SPECIFIC STORAGE'/
      DATA ANAME(5) /'          SPECIFIC YIELD'/
      DATA ANAME(6) /' HORIZ. TO VERTICAL ANI.'/
      DATA PTYPE(1) /'HK'/
      DATA PTYPE(2) /'HANI'/
      DATA PTYPE(3) /'VK'/
      DATA PTYPE(4) /'SS'/
      DATA PTYPE(5) /'SY'/
      DATA PTYPE(6) /'VANI'/

C     ------------------------------------------------------------------
C
C Check for cells that GO DRY/REWET
      DO 5 K=1,NLAY
C--SEAWAT: PASS IN HSALT RATHER THAN HNEW
        CALL SGWF1HUF2WETCHK(HSALT,IBOUND,CC,BOTM,
     &   NBOTM,K,KITER,KSTP,KPER,NCOL,NROW,NLAY,IOUT,WETDRY,
     &   WETFCT,IWETIT,IHDWET,HDRY)
C--SEAWAT: SET HNEW IF HSALT = HDRY OR IBOUND=30000
	   DO I=1,NROW
	   DO J=1,NCOL
	     IF(HSALT(J,I,K).EQ.HDRY) HNEW(J,I,K)=HDRY
	     IF(IBOUND(J,I,K).EQ.30000) HNEW(J,I,K)=HSALT(J,I,K)
	   ENDDO
	   ENDDO
    5 CONTINUE
C
C Zero out arrays
      DO 10 K=1,NLAY
        DO 20 I=1,NROW
          DO 30 J=1,NCOL
            HK(J,I,K)=0.
            HKCC(J,I,K)=0.
            VKA(J,I,K)=0.
            IF(ITRSS.NE.0 .AND. KITER.EQ.0) SC1(J,I,K)=0.
   30     CONTINUE
   20   CONTINUE
   10 CONTINUE
C
C2------DEFINE DATA FOR NAMED PARAMETERS.
C
C Loop through rows and columns
      DO 100 I=1,NROW
      DO 100 J=1,NCOL
C Zero out arrays
        DO 110 NU=1,NHUF
          HUFHK(NU)=0.
          HUFVK(NU)=0.
          HUFHANI(NU)=0.
          HUFSS(NU)=0.
          HUFSY(NU)=0.
          HUFKDEP(NU)=0.
  110   CONTINUE
        DO 115 NK=1,NLAY
          VDHD(J,I,NK) = 0.0
  115   CONTINUE

C
C---Populate HGU arrays for given i,j depending on parameter type
        CALL UHUF2POP(HUFHK,'HK  ',NCOL,NROW,NHUF,I,J,HUFTHK,
     &                    IZON,NZONAR,RMLT,NMLTAR,0,IOUT)
        CALL UHUF2POP(HUFHANI,'HANI',NCOL,NROW,NHUF,I,J,HUFTHK,
     &                    IZON,NZONAR,RMLT,NMLTAR,0,IOUT)
        CALL UHUF2POP(HUFVK,'VK  ',NCOL,NROW,NHUF,I,J,HUFTHK,
     &                    IZON,NZONAR,RMLT,NMLTAR,0,IOUT)
        CALL UHUF2POP(HUFVK,'VANI',NCOL,NROW,NHUF,I,J,HUFTHK,
     &                    IZON,NZONAR,RMLT,NMLTAR,0,IOUT)
        CALL UHUF2POP(HUFKDEP,'KDEP',NCOL,NROW,NHUF,I,J,HUFTHK,
     &                    IZON,NZONAR,RMLT,NMLTAR,0,IOUT)
        IF(ILVDA.NE.0) CALL UHUF2POPL(VDHD,NCOL,NROW,NLAY,I,J,
     &                        IZON,NZONAR,RMLT,NMLTAR)
        IF(ITRSS.NE.0) THEN
          CALL UHUF2POP(HUFSS,'SS  ',NCOL,NROW,NHUF,I,J,HUFTHK,
     &                      IZON,NZONAR,RMLT,NMLTAR,0,IOUT)
          CALL UHUF2POP(HUFSY,'SY  ',NCOL,NROW,NHUF,I,J,HUFTHK,
     &                      IZON,NZONAR,RMLT,NMLTAR,0,IOUT)
        ENDIF
C
C---Populate HANI and VANI from input file if not already defined by
c     a parameter
        DO 120 NU=1,NHUF
          IF(HGUVANI(NU).GT.0..AND.HUFVK(NU).EQ.0.)
     &          HUFVK(NU)=HGUVANI(NU)
          IF(HGUHANI(NU).GT.0..AND.HUFHANI(NU).EQ.0.)
     &          HUFHANI(NU)=HGUHANI(NU)
  120   CONTINUE
C
C---Populate MODEL arrays
        DO 130 NU=1,NHUF
          TOPU=HUFTHK(J,I,NU,1)
          THCKU=HUFTHK(J,I,NU,2)
          IF(ABS(THCKU).LT.1E-4) GOTO 130
          BOTU=TOPU-THCKU
C-----Determine which layer(s) unit applies to
C--SEAWAT: PASS IN HSALT INSTEAD OF HNEW
          CALL SGWF1HUF2HSRCH(NCOL,NROW,NLAY,BOTM,NBOTM,I,J,TOPU,BOTU,
     &                        HSALT,IBOUND,KT,KB,IFLG)
C-----Skip unit if thickness is zero
          IF(IFLG.EQ.1) GOTO 130
C-----Populate arrays
C--SEAWAT: PASS IN HSALT INSTEAD OF HNEW
          CALL SGWF1HUF2HK(NCOL,NROW,NLAY,BOTM,NBOTM,I,J,TOPU,BOTU,KT,
     &                     KB,HK,HKCC,HUFHK,HUFHANI,HUFKDEP(NU),NHUF,NU,
     &                     HSALT,GS)
C
C--SEAWAT: PASS IN HSALT INSTEAD OF HNEW
          CALL SGWF1HUF2VKA(NCOL,NROW,NLAY,BOTM,NBOTM,I,J,TOPU,BOTU,
     &                      VKA,HSALT,IBOUND,HUFHK,HUFVK,NHUF,NU,
     &                      HUFKDEP(NU),IOUT,GS)
          IF(ITRSS.NE.0 .AND. KITER.EQ.0 .AND. KPER.EQ.0 .AND.
     &       KSTP.EQ.0) THEN
            TOPU = HUFTHK(J,I,NU,1)
            BOTU = TOPU - THCKU
            CALL SGWF1HUF2SC1(NCOL,NROW,NLAY,BOTM,NBOTM,I,J,TOPU,
     &                        BOTU,SC1,HUFSS,KT,KB,NHUF,NU)
          ENDIF
  130   CONTINUE
  100 CONTINUE
C
C
C3------CHECK HUF DATA.
C--SEAWAT: PASS IN HSALT INSTEAD OF HNEW
      CALL SGWF1HUF2N(HSALT,IBOUND,HK,VKA,
     &       NCOL,NROW,NLAY,IOUT,WETDRY,BOTM,NBOTM)
C--SEAWAT: SET HNEW TO HCNV IF HSALT=HCNV
	HCNV=888.88
	DO K=1,NLAY
	DO I=1,NROW
	DO J=1,NCOL
		IF (HSALT(J,I,K).EQ.HCNV) HNEW(J,I,K)=HCNV
	ENDDO
	ENDDO
	ENDDO
C
C-------CALCULATE CV
      CALL SGWF1HUF2VCND(IBOUND,CV,VKA,DELR,DELC,
     &       NCOL,NROW,NLAY)
C
      IF(ILVDA.EQ.0) THEN
C
C-------CALCULATE CR AND CC
C--SEAWAT: PASS IN HSALT INSTEAD OF HNEW
        CALL SGWF1HUF2HCND(HSALT,IBOUND,CR,CC,HK,DELR,
     &       DELC,NCOL,NROW,NLAY,IOUT,BOTM,NBOTM,HKCC,
     &       HDRY,KITER,KSTP,KPER)
C--SEAWAT: RESET HNEW TO HDRY IF DRY
	  DO K=1,NLAY
	  DO I=1,NROW
	  DO J=1,NCOL
		IF (HSALT(J,I,K).EQ.HDRY) HNEW(J,I,K)=HDRY
	  ENDDO
	  ENDDO
	  ENDDO
      ELSE
C-------Populate HK, HKCC, VDHD arrays
        CALL SGWF1HUF2VDHHV(
     &    IBOUND,HK,HKCC,VDHD,NCOL,NROW,NLAY,HUFTHK,NHUF,BOTM,NBOTM,
     &    IZON,NZONAR,RMLT,NMLTAR,HNEW,GS)

C-------Populate VDHT array
        CALL SGWF1HUF2VDHT(
     &    IBOUND,HK,HKCC,VDHD,VDHT,NCOL,NROW,NLAY,DELR,DELC)
C-------Populate A9 array
        CALL SGWF1HUF2VDA9(VDHT,A9,IBOUND,NLAY,NROW,NCOL)
      ENDIF
C
C-------CALCULATE SC1
      IF(ITRSS.NE.0 .AND. KITER.EQ.0 .AND. KPER.EQ.0 .AND.
     &   KSTP.EQ.0) THEN
C
C-------CHECK FOR SYTP PARAMETERS
        CALL SGWF1HUF2SYTP(SC1,IBOUND,NCOL,NROW,NLAY,IZON,
     &                     NZONAR,RMLT,NMLTAR)
C
C-------MULTIPLY BY DELR*DELC TO GET SC1
        CALL SGWF1HUF2SC(IBOUND,SC1,DELR,DELC,NCOL,
     &                   NROW,NLAY)
      ENDIF
C
C-----Print HUF data
      IF(KITER.EQ.0) THEN
C-----Print unit arrays depending on flags
        DO 200 NU=1,NHUF
          DO 210 IP=1,5
            IF(IHGUFLG(IP,NU).GT.0) THEN
              IF(IP.EQ.3.AND.HGUVANI(NU).GT.0) THEN
                IA=6
              ELSE
                IA=IP
              ENDIF
C-----Populate HUFHK as a temporary holding array
              DO 220 J=1,NCOL
                DO 220 I=1,NROW
                  HUFHK(NU)=0
                  CALL UHUF2POP(HUFHK,PTYPE(IP),NCOL,NROW,NHUF,I,J,
     &                   HUFTHK,IZON,NZONAR,RMLT,NMLTAR,NU,IOUT)
                  IF(IA.EQ.6)
     &            CALL UHUF2POP(HUFHK,PTYPE(IA),NCOL,NROW,NHUF,I,J,
     &                   HUFTHK,IZON,NZONAR,RMLT,NMLTAR,NU,IOUT)
C-----Transfer to HUFTMP
                  HUFTMP(J,I,NU)=HUFHK(NU)
                  IF(IA.EQ.6.AND.HUFHK(NU).EQ.0.)
     &              HUFTMP(J,I,NU)=HGUVANI(NU)
                  IF(IA.EQ.2.AND.HGUHANI(NU).GT.0..AND.HUFHK(NU).EQ.0.)
     &              HUFTMP(J,I,NU)=HGUHANI(NU)
  220         CONTINUE
C-----Print HUFTMP
              WRITE(IOUT,305) ANAME(IA),HGUNAM(NU)
  305         FORMAT(//A24,' FOR UNIT ',A10)
              CALL ULAPRWC(HUFTMP(1,1,NU),NCOL,NROW,0,IOUT,
     &                     IHGUFLG(IP,NU),ANAME(IA))
            ENDIF
  210     CONTINUE
  200   CONTINUE
      ENDIF
C
C
C4------RETURN
      RETURN
      END

c======================================================================
      SUBROUTINE VDF1HUF2FM(HCOF,RHS,HOLD,SC1,HNEW,IBOUND,CR,CC,CV,HK,
     &    VKA,BOTM,DELR,DELC,DELT,ITRSS,ISS,NCOL,NROW,NLAY,IOUT,WETDRY,
     &    NBOTM,NHUF,RMLT,IZON,NMLTAR,NZONAR,HUFTHK,HKCC,HDRY,KITER,
     &    KSTP,KPER,HUFTMP,IHGUFLG,ILVDA,VDHD,VDHT,
     &    IWETIT,IHDWET,WETFCT,GS,A9)
C     ******************************************************************
C     ADD LEAKAGE CORRECTION AND STORAGE TO HCOF AND RHS, AND CALCULATE
C     CONDUCTANCE AS REQUIRED.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE VDFMODULE,   ONLY: DENSEREF,PS,ELEV,HSALT,MFNADVFD
C
      DOUBLE PRECISION HNEW,HN
C
      DIMENSION HCOF(NCOL,NROW,NLAY),RHS(NCOL,NROW,NLAY),
     &    HOLD(NCOL,NROW,NLAY),SC1(NCOL,NROW,NLAY),HNEW(NCOL,NROW,NLAY),
     &    IBOUND(NCOL,NROW,NLAY),CR(NCOL,NROW,NLAY),
     &    CC(NCOL,NROW,NLAY),CV(NCOL,NROW,NLAY),HK(NCOL,NROW,NLAY),
     &    VKA(NCOL,NROW,NLAY),BOTM(NCOL,NROW,0:NBOTM),DELR(NCOL),
     &    DELC(NROW),WETDRY(NCOL,NROW,NLAY),GS(NCOL,NROW),
     &    RMLT(NCOL,NROW,NMLTAR),IZON(NCOL,NROW,NZONAR),
     &    HUFTHK(NCOL,NROW,NHUF,2),HKCC(NCOL,NROW,NLAY),
     &    IHGUFLG(5,NHUF),
     &    HUFTMP(NCOL,NROW,NHUF),VDHD(NCOL,NROW,NLAY),
     &    VDHT(NCOL,NROW,NLAY,3),A9(NCOL*NROW*NLAY,5)
C
      COMMON /DISCOM/LBOTM(999),LAYCBD(999)
      COMMON /HUFCOM/HGUHANI(999),HGUVANI(999),LTHUF(999),LAYWT(999)
C     ------------------------------------------------------------------
      ONE=1.
	i=1
	j=11
	k=1
C
C1------IF ANY LAYER IS CONVERTIBLE REPOPULATE ARRAYS AND CALCULATE
C          BRANCH CONDUCTANCES
      KLAYFLG=0
      DO 100 K=1,NLAY
        IF(LTHUF(K).NE.0) KLAYFLG=1
  100 CONTINUE
      IF (KLAYFLG.NE.0) THEN
C--SEAWAT: CALL VDF1HUF2SP
        CALL VDF1HUF2SP(
     &   IBOUND,HNEW,CR,CC,CV,DELR,DELC,BOTM,HK,VKA,SC1,
     &   ITRSS,NCOL,NROW,NLAY,IOUT,WETDRY,NHUF,NBOTM,RMLT,IZON,
     &   NMLTAR,NZONAR,HUFTHK,HKCC,HDRY,KITER,KSTP,KPER,
     &   IHGUFLG,HUFTMP,ILVDA,VDHD,VDHT,IWETIT,
     &   IHDWET,WETFCT,GS,A9)
      ENDIF

      IF(ILVDA.GT.0) CALL GWF1HUF2VDFM(HNEW,IBOUND,CR,CC,VDHT,
     &                                 RHS,NCOL,NROW,NLAY,A9)
C
C2------IF THE STRESS PERIOD IS TRANSIENT, ADD STORAGE TO HCOF AND RHS
C
      IF(ISS.EQ.0) THEN
         TLED=ONE/DELT
         DO 200 K=1,NLAY
C
C3------SEE IF THIS LAYER IS CONVERTIBLE OR NON-CONVERTIBLE.
         IF(LTHUF(K).EQ.0) THEN
C4------NON-CONVERTIBLE LAYER, SO USE PRIMARY STORAGE
            DO 140 I=1,NROW
            DO 140 J=1,NCOL
              IF(IBOUND(J,I,K).LE.0) GO TO 140
              RHO=SC1(J,I,K)*TLED
C--SEAWAT: MULTIPLY BY DENSITY
              HCOF(J,I,K)=HCOF(J,I,K)-RHO*PS(J,I,K)
C--SEAWAT: MULTIPLY BY DENSITY
              RHS(J,I,K)=RHS(J,I,K)-RHO*HOLD(J,I,K)*PS(J,I,K)
  140       CONTINUE
         ELSE
C
C5------A CONVERTIBLE LAYER, SO CHECK OLD AND NEW HEADS TO DETERMINE
C5------WHEN TO USE PRIMARY AND SECONDARY STORAGE
            DO 180 I=1,NROW
            DO 180 J=1,NCOL
C
C5A-----IF THE CELL IS EXTERNAL THEN SKIP IT.
              IF(IBOUND(J,I,K).LE.0) GO TO 180
              TOP=BOTM(J,I,LBOTM(K)-1)
              BOT=BOTM(J,I,LBOTM(K))
              HO=HOLD(J,I,K)
              HN=HNEW(J,I,K)
C--SEAWAT: CALCULATE SALTHEADS FOR HO AND HN
	        HOSALT=SALTHEAD(HO,PS(J,I,K),ELEV(J,I,K))
	        HNSALT=SALTHEAD(HN,PS(J,I,K),ELEV(J,I,K))
              CRHS=0.
              CHCOF=0.
C--SEAWAT: USE SALTHEADS IN IF STATEMENT
              IF(HOSALT.GT.TOP.AND.HNSALT.GT.TOP) THEN
                CHCOF=SC1(J,I,K)*TLED
                CRHS=SC1(J,I,K)*HO*TLED
              ELSE
C---------------Compute SC1 Component
C--SEAWAT: USE SALTHEADS IN IF STATEMENT
                IF(HOSALT.GT.TOP) THEN
                  CRHS=SC1(J,I,K)*(HO-TOP)*TLED
                ELSEIF(HNSALT.GT.TOP) THEN
                  CHCOF=SC1(J,I,K)*TLED
                  CRHS=SC1(J,I,K)*TOP*TLED
                ENDIF
C---------------Compute SC2 Component
C--SEAWAT: PASS HEAD IN HNSALT AND HOSALT FOR HN AND HF AND ALSO PASS IN HN AND HO
                CALL SVDF1HUF2SC2(0,J,I,K,TOP,BOT,HNSALT,HOSALT,TLED,
     &                        CHCOF,CRHS,HUFTHK,NCOL,NROW,NHUF,IZON,
     &                        NZONAR,RMLT,NMLTAR,DELR(J)*DELC(I),IOUT,
     &                        HN,HO)
              ENDIF
C
C5D-----ADD STORAGE TERMS TO RHS AND HCOF.
C--SEAWAT: MULTIPLY BY FLUID DENSITY
              HCOF(J,I,K)=HCOF(J,I,K) - CHCOF*PS(J,I,K)
              RHS(J,I,K) = RHS(J,I,K) - CRHS*PS(J,I,K)
C
  180       CONTINUE
         END IF
C
  200    CONTINUE
      END IF
C
C6------FOR EACH LAYER DETERMINE IF CORRECTION TERMS ARE NEEDED FOR
C6------FLOW DOWN INTO PARTIALLY SATURATED LAYERS.
      DO 300 K=1,NLAY
C
C7------SEE IF CORRECTION IS NEEDED FOR LEAKAGE FROM ABOVE.
      IF(LTHUF(K).NE.0 .AND. K.NE.1) THEN
C
C7A-----FOR EACH CELL MAKE THE CORRECTION IF NEEDED.
         DO 220 I=1,NROW
         DO 220 J=1,NCOL
C
C7B-----IF THE CELL IS EXTERNAL(IBOUND<=0) THEN SKIP IT.
         IF(IBOUND(J,I,K).LE.0) GO TO 220
         HTMP=HNEW(J,I,K)
C--SEAWAT: ASSIGN SALTHEAD VARIABLE
	   HTMPSALT=HSALT(J,I,K)
C
C7C-----IF HEAD IS ABOVE TOP THEN CORRECTION NOT NEEDED
         TOP=BOTM(J,I,LBOTM(K)-1)
C--SEAWAT: USE SALTHEAD VARIABLE
         IF(HTMPSALT.GE.TOP) GO TO 220
C
C7D-----WITH HEAD BELOW TOP ADD CORRECTION TERMS TO RHS.
C--SEAWAT:         RHS(J,I,K)=RHS(J,I,K) + CV(J,I,K-1)*(TOP-HTMP)
C--SEAWAT: USE VARIABLE-DENSITY FORM OF CORRECTION TERM
C         RHS(J,I,K)=RHS(J,I,K) + PS(J,I,K-1)*CV(J,I,K-1)*
C     &      (ELEV(J,I,K)-HTMP+PS(J,I,K-1)/DENSEREF*
C     &      (TOP-ELEV(J,I,K)))
C--SEAWAT: THIS SECTION REWRITTEN 9/24/07 USING CLEARER CODING
C--FIRST CALCULATE Q_MCALC, WHICH IS THE MASS FLUX WITH THE STANDARD DARCY EQUATION
      DIS1=ELEV(J,I,K-1)-BOTM(J,I,K-1)
      DIS2=BOTM(J,I,K-1)-ELEV(J,I,K)
      AVGDENS=(DIS1*PS(J,I,K-1)+DIS2*PS(J,I,K))/(DIS1+DIS2)
      Q_MCALC=CV(J,I,K-1)*(HNEW(J,I,K-1)-HNEW(J,I,K)+
     +        (AVGDENS-DENSEREF)/DENSEREF*(ELEV(J,I,K-1)-ELEV(J,I,K)))
      IF(MFNADVFD.EQ.2) THEN 
        Q_MCALC=Q_MCALC*AVGDENS
      ELSE
        IF (Q_MCALC.GT.0.) THEN
                 Q_MCALC=Q_MCALC*PS(J,I,K-1)
        ELSE
                 Q_MCALC=Q_MCALC*PS(J,I,K)
	  ENDIF
      ENDIF
C      
C--CALCULATE Q_MACTUAL, WHICH IS THE TRUE MASS FLUX
      Q_MACTUAL=PS(J,I,K-1)/DENSEREF*CV(J,I,K-1)*
     +        (SALTHEAD(HNEW(J,I,K-1),PS(J,I,K-1),ELEV(J,I,K-1))
     +         -BOTM(J,I,LBOTM(K)-1))
      Q_MACTUAL=Q_MACTUAL*PS(J,I,K-1)
C
C--CALCULATE Q_MCORRECT, WHICH IS THE CORRECTION TERM THAT IS ADDED TO THE RHS
      Q_MCORRECT=Q_MCALC-Q_MACTUAL
      RHS(J,I,K)=RHS(J,I,K)+Q_MCORRECT
  220    CONTINUE
      END IF
C
C8------SEE IF THIS LAYER MAY NEED CORRECTION FOR LEAKAGE TO BELOW.
      IF(K.EQ.NLAY) GO TO 300
      IF(LTHUF(K+1).NE.0) THEN
C
C8A-----FOR EACH CELL MAKE THE CORRECTION IF NEEDED.
         DO 280 I=1,NROW
         DO 280 J=1,NCOL
C
C8B-----IF CELL IS EXTERNAL (IBOUND<=0) THEN SKIP IT.
         IF(IBOUND(J,I,K).LE.0) GO TO 280
C
C8C-----IF HEAD IN THE LOWER CELL IS LESS THAN TOP ADD CORRECTION
C8C-----TERM TO RHS.
         HTMP=HNEW(J,I,K+1)
         TOP=BOTM(J,I,LBOTM(K+1)-1)
C--SEAWAT:         IF(HTMP.LT.TOP) RHS(J,I,K)=RHS(J,I,K)- CV(J,I,K)*(TOP-HTMP)
C--SEAWAT: USE VARIABLE-DENSITY FORM OF CORRECTION TERM
C	   IF(HTMP.LT.TOP) 
C     &        RHS(J,I,K)=RHS(J,I,K) + PS(J,I,K)*CV(J,I,K)*
C     &        (HTMP-ELEV(J,I,K+1)+PS(J,I,K)/DENSEREF*
C     &        (ELEV(J,I,K+1)-TOP))
         IF(HSALT(J,I,K+1).GE.TOP) GOTO 280
C
C--FIRST CALCULATE Q_MCALC, WHICH IS THE MASS FLUX WITH THE STANDARD DARCY EQUATION
         DIS1=BOTM(J,I,K)-ELEV(J,I,K+1)
         DIS2=ELEV(J,I,K)-BOTM(J,I,K)
         AVGDENS=(DIS1*PS(J,I,K+1)+DIS2*PS(J,I,K))/(DIS1+DIS2)
         Q_MCALC=CV(J,I,K)*(HNEW(J,I,K+1)-HNEW(J,I,K)+
     +        (AVGDENS-DENSEREF)/DENSEREF*(ELEV(J,I,K+1)-ELEV(J,I,K)))
         IF(MFNADVFD.EQ.2) THEN 
          Q_MCALC=Q_MCALC*AVGDENS
         ELSE
           IF (Q_MCALC.GT.0.) THEN
             Q_MCALC=Q_MCALC*PS(J,I,K+1)
           ELSE
             Q_MCALC=Q_MCALC*PS(J,I,K)
	     ENDIF
         ENDIF
C      
C--CALCULATE Q_MACTUAL, WHICH IS THE TRUE OR ACTUAL MASS FLUX
      Q_MACTUAL=PS(J,I,K)/DENSEREF*CV(J,I,K)*
     +      (BOTM(J,I,K)-SALTHEAD(HNEW(J,I,K),PS(J,I,K),ELEV(J,I,K)))
      Q_MACTUAL=Q_MACTUAL*PS(J,I,K)
C
C--CALCULATE Q_MCORRECT, WHICH IS THE CORRECTION TERM THAT IS ADDED TO THE RHS
      Q_MCORRECT=Q_MCALC-Q_MACTUAL
      RHS(J,I,K)=RHS(J,I,K)+Q_MCORRECT   
  280    CONTINUE
      END IF
C
  300 CONTINUE
C
C9------RETURN
      RETURN
      END

c======================================================================
      SUBROUTINE SVDF1HUF2SC2(IFLG,J,I,K,TOP,BOT,HN,HO,TLED,CHCOF,CRHS,
     &                    HUFTHK,NCOL,NROW,NHUF,IZON,NZONAR,RMLT,
     &                    NMLTAR,AREA,IOUT,HNFE,HOFE)
C
C     ******************************************************************
C     Compute contributions to HCOF and RHS for convertible cell
C     Enter subroutine when HO and/or HN are below TOP
C     Values for IFLG:
C       IFLG = 0, Calculate contributions to HCOF and RHS
C       IFLG = 1, Calculate contributions to flow within cell
C       IFLG = 2, Calculate contributions to sensitivity calculations
C     Subroutine will halt execution if Sy is not defined.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INCLUDE 'param.inc'
      DOUBLE PRECISION HN
      REAL TOPU, BOTU, THCKU, TOP, BOT, CHCOF, CRHS, AREA
      INTEGER IFLG
      DIMENSION HUFTHK(NCOL,NROW,NHUF,2),IZON(NCOL,NROW,NZONAR),
     &  RMLT(NCOL,NROW,NMLTAR)
C
      IFND=0
C-----Loop through parameters
      DO 100 NP=1,MXPAR
        IF(PARTYP(NP).EQ.'SY') THEN
          BNP=B(NP)*AREA*TLED
C---------Loop through units for this parameter to determine if they apply
C         to this layer
          DO 200 ND=IPLOC(1,NP),IPLOC(2,NP)
            NU=IPCLST(1,ND)
            NM=IPCLST(2,ND)
            NZ=IPCLST(3,ND)
            TOPU=HUFTHK(J,I,NU,1)
            THCKU=HUFTHK(J,I,NU,2)
            BOTU=TOPU-THCKU
C-----------Skip this unit if it is not present in this layer
            IF(TOPU.GT.TOP.AND.BOTU.GE.TOP) GOTO 200
            IF(TOPU.LE.BOT.AND.BOTU.LT.BOT) GOTO 200
            IF(TOPU.GT.TOP) TOPU=TOP
            IF(BOTU.LT.BOT) BOTU=BOT
            CALL UHUF2RMLT(RMLT0,J,I,NZ,NM,ND,IZON,NZONAR,
     &                     RMLT,NMLTAR,NROW,NCOL)
C-----------Skip this unit if it does not apply to this cell
            IF(RMLT0.LE.0) GOTO 200
            IFND=1
C-----------Compute contributions for this unit to flow in layer
            IF(IFLG.LT.2) THEN
              IF(HO.GT.TOP) THEN
C-------------Layer converts, water table is coming down
                IF(HN.LT.TOPU.AND.HN.GT.BOTU) THEN
C---------------New head is in this unit
                  CHCOF=RMLT0*BNP
                  CRHS=CRHS+RMLT0*BNP*TOPU
C--SEAWAT: USE EQUIVALENT FRESHWATER HEAD
                  IF(IFLG.EQ.1) CRHS=CRHS-RMLT0*BNP*HNFE
                ELSEIF(HN.LT.BOTU) THEN
C---------------New head is below this unit
                  CRHS=CRHS+RMLT0*BNP*(TOPU-BOTU)
                ENDIF
              ELSEIF(HN.GT.TOP) THEN
C-------------Layer converts, water table is going up
                IF(HO.LT.TOPU.AND.HO.GT.BOTU) THEN
C---------------Old head is in this unit
C--SEAWAT: USE EQUIVALENT FRESHWATER HEAD
                  CRHS=CRHS+RMLT0*BNP*(HOFE-TOPU)
                ELSEIF(HO.LT.BOTU) THEN
C---------------Old head is below this unit
                  CRHS=CRHS+RMLT0*BNP*(BOTU-TOPU)
                ENDIF
              ELSEIF(HO.LT.TOP.AND.HN.LT.TOP) THEN
C-------------Layer does not convert, just use SC2
                IF(HO.GT.HN) THEN
C---------------Water table is coming down
                  IF(HO.LT.TOPU.AND.HO.GT.BOTU .AND.
     &               HN.LT.TOPU.AND.HN.GT.BOTU) THEN
C-----------------Old and new heads are both in this unit
                    CHCOF=RMLT0*BNP
C--SEAWAT: USE EQUIVALENT FRESHWATER HEAD
                    CRHS=CRHS+RMLT0*BNP*HOFE
C--SEAWAT: USE EQUIVALENT FRESHWATER HEAD
                    IF(IFLG.EQ.1) CRHS=CRHS-RMLT0*BNP*HNFE
                  ELSEIF(HO.LT.TOPU.AND.HO.GT.BOTU) THEN
C-----------------Old head is in this unit
C--SEAWAT: USE EQUIVALENT FRESHWATER HEAD
                    CRHS=CRHS+RMLT0*BNP*(HOFE-BOTU)
                  ELSEIF(HN.LT.TOPU.AND.HN.GT.BOTU) THEN
C-----------------New head is in this unit
                    CHCOF=RMLT0*BNP
                    CRHS=CRHS+RMLT0*BNP*TOPU
C--SEAWAT: USE EQUIVALENT FRESHWATER HEAD
                    IF(IFLG.EQ.1) CRHS=CRHS-RMLT0*BNP*HNFE
                  ELSEIF(HO.GT.TOPU.AND.HN.LT.BOTU) THEN
C-----------------Old head is above and new head is below this unit
                    CRHS=CRHS+RMLT0*BNP*(TOPU-BOTU)
                  ENDIF
                ELSE
C---------------Water table is going up
                  IF(HO.LT.TOPU.AND.HO.GT.BOTU .AND.
     &               HN.LT.TOPU.AND.HN.GT.BOTU) THEN
C-----------------Old and new heads are both in this unit
                    CHCOF=RMLT0*BNP
C--SEAWAT: USE EQUIVALENT FRESHWATER HEAD
                    CRHS=CRHS+RMLT0*BNP*HOFE
C--SEAWAT: USE EQUIVALENT FRESHWATER HEAD
                    IF(IFLG.EQ.1) CRHS=CRHS-RMLT0*BNP*HNFE
                  ELSEIF(HO.LT.TOPU.AND.HO.GT.BOTU) THEN
C-----------------Old head is in this unit
C--SEAWAT: USE EQUIVALENT FRESHWATER HEAD
                    CRHS=CRHS+RMLT0*BNP*(HOFE-TOPU)
                  ELSEIF(HN.LT.TOPU.AND.HN.GT.BOTU) THEN
C-----------------New head is in this unit
                    CHCOF=RMLT0*BNP
                    CRHS=CRHS+RMLT0*BNP*BOTU
C--SEAWAT: USE EQUIVALENT FRESHWATER HEAD
                    IF(IFLG.EQ.1) CRHS=CRHS-RMLT0*BNP*HNFE
                  ELSEIF(HO.LT.BOTU.AND.HN.GT.TOPU) THEN
C-----------------Old head is below and new head is abov this unit
                    CRHS=CRHS+RMLT0*BNP*(BOTU-TOPU)
                  ENDIF
                ENDIF
              ENDIF
            ELSEIF(IFLG.EQ.2) THEN
              IF(HO.LE.TOPU.AND.HO.GT.BOTU) THEN
                CRHS=RMLT0*B(NP)*AREA
                RETURN
              ENDIF
            ENDIF
  200     CONTINUE
        ENDIF
  100 CONTINUE
      IF(IFND.EQ.0) THEN
        WRITE(IOUT,500) K,I,J
  500   FORMAT("Sy not defined for cell at (layer,row,column):",
     &         3(I4,","))
        CALL USTOP(' ')
      ENDIF
C
C4------RETURN
      RETURN
      END
c======================================================================
C--SEAWAT: PASS PS, ELEV
      SUBROUTINE SVDF1HUF2S(VBNM,VBVL,MSUM,HNEW,IBOUND,HOLD,SC1,
     &   BOTM,DELT,ISS,NCOL,NROW,NLAY,KSTP,KPER,IHUFCB,
     &   ICBCFL,BUFF,IOUT,PERTIM,TOTIM,NBOTM,HUFTHK,NHUF,IZON,NZONAR,
     &   RMLT,NMLTAR,DELR,DELC)
C     ******************************************************************
C     COMPUTE STORAGE BUDGET FLOW TERM FOR HUF.
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE VDFMODULE,   ONLY: DENSEREF,PS,ELEV
C
      CHARACTER*16 VBNM(MSUM),TEXT
      DOUBLE PRECISION HNEW,STOIN,STOUT,SSTRG,HN
C
      DIMENSION HNEW(NCOL,NROW,NLAY), IBOUND(NCOL,NROW,NLAY),
     &   HOLD(NCOL,NROW,NLAY),SC1(NCOL,NROW,NLAY),VBVL(4,MSUM),
     &   BOTM(NCOL,NROW,0:NBOTM),BUFF(NCOL,NROW,NLAY),
     &   HUFTHK(NCOL,NROW,NHUF,2),IZON(NCOL,NROW,NZONAR),
     &   RMLT(NCOL,NROW,NMLTAR),DELR(NCOL),DELC(NROW)
C
      COMMON /DISCOM/LBOTM(999),LAYCBD(999)
      COMMON /HUFCOM/HGUHANI(999),HGUVANI(999),LTHUF(999),LAYWT(999)
C
      DATA TEXT /'         STORAGE'/
C     ------------------------------------------------------------------
C
C2------INITIALIZE BUDGET ACCUMULATORS AND 1/DELT.
      ZERO=0.
      STOIN=ZERO
      STOUT=ZERO
C
C1------SKIP STORAGE BUDGET CALCULATIONS IF STEADY STATE.
      IF(ISS.NE.0) GOTO 400
      ONE=1.
      TLED=ONE/DELT
C
C3------IF CELL-BY-CELL FLOWS WILL BE SAVED, SET FLAG IBD.
      IBD=0
      IF(IHUFCB.GT.0) IBD=ICBCFL
C
C4------CLEAR BUFFER.
      DO 210 K=1,NLAY
      DO 210 I=1,NROW
      DO 210 J=1,NCOL
        BUFF(J,I,K)=ZERO
210   CONTINUE
C
C5------LOOP THROUGH EVERY CELL IN THE GRID.
      KT=0
      DO 300 K=1,NLAY
        LC=LTHUF(K)
        IF(LC.NE.0) KT=KT+1
        DO 300 I=1,NROW
        DO 300 J=1,NCOL
C
C6------SKIP NO-FLOW AND CONSTANT-HEAD CELLS.
          IF(IBOUND(J,I,K).LE.0) GO TO 300
          HN=HNEW(J,I,K)
          HO=HOLD(J,I,K)
C--SEAWAT: CALCULATE SALTHEADS FOR HO AND HN
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
C--SEAWAT: USE SALTHEADS IN IF STATEMENT
          IF(HOSALT.GT.TOP) THEN
            STRG=SC1(J,I,K)*(HO-TOP)*TLED
C--SEAWAT: USE SALTHEADS IN IF STATEMENT
          ELSEIF(HNSALT.GT.TOP) THEN
            STRG=SC1(J,I,K)*TLED*(TOP-HN)
          ENDIF
C---------------Compute SC2 Component
C--SEAWAT: PASS HEAD IN HNSALT AND HOSALT FOR HN AND HF AND ALSO PASS IN HN AND HO
          CALL SVDF1HUF2SC2(1,J,I,K,TOP,BOT,HNSALT,HOSALT,TLED,CHCOF,
     &           STRG,HUFTHK,NCOL,NROW,NHUF,IZON,NZONAR,RMLT,NMLTAR,
     &           DELR(J)*DELC(I),IOUT,HN,HO)
C      STRG=SOLD*(HOLD(J,I,K)-TP) + SNEW*TP - SNEW*HSING
          GO TO 288
C
C7B----ONE STORAGE CAPACITY.
  285     RHO=SC1(J,I,K)*TLED
          STRG=RHO*(HO-HN)

C
C8-----STORE CELL-BY-CELL FLOW IN BUFFER AND ADD TO ACCUMULATORS.
  288     BUFF(J,I,K)=STRG
C--SEAWAT: MULTIPLY BY FLUID DENSITY
          SSTRG=STRG*PS(J,I,K)
          IF(STRG) 292,300,294
  292     STOUT=STOUT-SSTRG
          GO TO 300
  294     STOIN=STOIN+SSTRG
C
  300 CONTINUE
C
C9-----IF IBD FLAG IS SET RECORD THE CONTENTS OF THE BUFFER.
      IF(IBD.EQ.1) CALL UBUDSV(KSTP,KPER,TEXT,
     &                       IHUFCB,BUFF,NCOL,NROW,NLAY,IOUT)
      IF(IBD.EQ.2) CALL UBDSV1(KSTP,KPER,TEXT,IHUFCB,
     &            BUFF,NCOL,NROW,NLAY,IOUT,DELT,PERTIM,TOTIM,IBOUND)
C
C10-----ADD TOTAL RATES AND VOLUMES TO VBVL & PUT TITLE IN VBNM.
  400 CONTINUE
      SIN=STOIN
      SOUT=STOUT
      VBVL(1,MSUM)=VBVL(1,MSUM)+SIN*DELT
      VBVL(2,MSUM)=VBVL(2,MSUM)+SOUT*DELT
      VBVL(3,MSUM)=SIN
      VBVL(4,MSUM)=SOUT
      VBNM(MSUM)=TEXT
      MSUM=MSUM+1
C
C11----RETURN.
      RETURN
      END
c======================================================================
      SUBROUTINE SVDF1HUF2F(VBNM,VBVL,MSUM,HNEW,IBOUND,CR,CC,CV,BOTM,
     &         DELT,NCOL,NROW,NLAY,KSTP,KPER,IHUFCB,BUFF,IOUT,ICBCFL,
     &         PERTIM,TOTIM,NBOTM,ICHFLG,ILVDA,VDHT,DELR,DELC)
C     ******************************************************************
C     COMPUTE FLOW FROM CONSTANT-HEAD CELLS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE VDFMODULE,   ONLY: MFNADVFD,IWTABLE,DENSEREF,PS,ELEV,HSALT
C
      CHARACTER*16 VBNM(MSUM),TEXT
      DOUBLE PRECISION HNEW,HD,CHIN,CHOUT,XX1,XX2,XX3,XX4,XX5,XX6
      DOUBLE PRECISION DFL,DFR,DFT,DFB
C
      DIMENSION HNEW(NCOL,NROW,NLAY), IBOUND(NCOL,NROW,NLAY),
     &     CR(NCOL,NROW,NLAY), CC(NCOL,NROW,NLAY),
     &     CV(NCOL,NROW,NLAY), VBVL(4,MSUM),
     &     BOTM(NCOL,NROW,0:NBOTM),BUFF(NCOL,NROW,NLAY),
     &     VDHT(NCOL,NROW,NLAY,3)
C
C--SEAWAT: DIMENSION ADDITIONAL ARRAYS
	DIMENSION DELR(NCOL),DELC(NROW)
C
      COMMON /DISCOM/LBOTM(999),LAYCBD(999)
      COMMON /HUFCOM/HGUHANI(999),HGUVANI(999),LTHUF(999),LAYWT(999)
C
      DATA TEXT /'   CONSTANT HEAD'/
C     ------------------------------------------------------------------
C
C1------SET IBD TO INDICATE IF CELL-BY-CELL BUDGET VALUES WILL BE SAVED.
      IBD=0
      IF(IHUFCB.LT.0 .AND. ICBCFL.NE.0) IBD=-1
      IF(IHUFCB.GT.0) IBD=ICBCFL
C
C2------CLEAR BUDGET ACCUMULATORS.
      ZERO=0.
      CHIN=ZERO
      CHOUT=ZERO
      IBDLBL=0
C
C3------CLEAR BUFFER.
      DO 5 K=1,NLAY
      DO 5 I=1,NROW
      DO 5 J=1,NCOL
      BUFF(J,I,K)=ZERO
5     CONTINUE
C
C3A-----IF SAVING CELL-BY-CELL FLOW IN A LIST, COUNT CONSTANT-HEAD
C3A-----CELLS AND WRITE HEADER RECORDS.
      IF(IBD.EQ.2) THEN
         NCH=0
         DO 7 K=1,NLAY
         DO 7 I=1,NROW
         DO 7 J=1,NCOL
         IF(IBOUND(J,I,K).LT.0) NCH=NCH+1
7        CONTINUE
         CALL UBDSV2(KSTP,KPER,TEXT,IHUFCB,NCOL,NROW,NLAY,
     &          NCH,IOUT,DELT,PERTIM,TOTIM,IBOUND)
      END IF
C
C4------LOOP THROUGH EACH CELL AND CALCULATE FLOW INTO MODEL FROM EACH
C4------CONSTANT-HEAD CELL.
      DO 200 K=1,NLAY
      DO 200 I=1,NROW
      DO 200 J=1,NCOL
C
C5------IF CELL IS NOT CONSTANT HEAD SKIP IT & GO ON TO NEXT CELL.
      IF (IBOUND(J,I,K).GE.0)GO TO 200
C
C6------CLEAR VALUES FOR FLOW RATE THROUGH EACH FACE OF CELL.
      X1=ZERO
      X2=ZERO
      X3=ZERO
      X4=ZERO
      X5=ZERO
      X6=ZERO
      CHCH1=ZERO
      CHCH2=ZERO
      CHCH3=ZERO
      CHCH4=ZERO
      CHCH5=ZERO
      CHCH6=ZERO
C--SEAWAT: INTRODUCE THE TERM FOR STORING VOLUMETRIC FLOW
	FLOW=ZERO
      IF(ILVDA.GT.0)
     &  CALL SGWF1HUF2VDF9(I,J,K,VDHT,HNEW,IBOUND,NLAY,NROW,NCOL,
     &                     DFL,DFR,DFT,DFB)
C7------CALCULATE FLOW THROUGH THE LEFT FACE.
C7------COMMENTS A-C APPEAR ONLY IN THE SECTION HEADED BY COMMENT 7,
C7------BUT THEY APPLY IN A SIMILAR MANNER TO SECTIONS 8-12.
C
C7A-----IF THERE IS NO FLOW TO CALCULATE THROUGH THIS FACE, THEN GO ON
C7A-----TO NEXT FACE.  NO FLOW OCCURS AT THE EDGE OF THE GRID, TO AN
C7A-----ADJACENT NO-FLOW CELL, OR TO AN ADJACENT CONSTANT-HEAD CELL.
      IF(J.EQ.1) GO TO 30
      IF(IBOUND(J-1,I,K).EQ.0) GO TO 30
      IF(IBOUND(J-1,I,K).LT.0 .AND. ICHFLG.EQ.0) GO TO 30
C
C7B-----CALCULATE FLOW THROUGH THE LEFT FACE.
      IF(ILVDA.GT.0) THEN
        CHCH1 = -DFL
      ELSE
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
C--SEAWAT: CALL VDWTABLE IF WATER TABLE CORRECTIONS ARE ACTIVE
      IF(IWTABLE.EQ.1.AND.LTHUF(K).NE.0) 
     & CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &               H2,HS2,PS2,Z2,TOP2,BOT2)
	HDIFF=H2-H1
      DIS1=DELR(J-1)/2
      DIS2=DELR(J)/2
      AVGDENS=(DIS1*PS(J-1,I,K)+DIS2*PS(J,I,K))/(DIS1+DIS2)
      D1=CR(J-1,I,K)*(AVGDENS-DENSEREF)/DENSEREF*(Z2-Z1)      
	FLOWDIR=CR(J-1,I,K)*HDIFF+D1
	FLOW=FLOW-FLOWDIR
	IF(MFNADVFD.EQ.2) THEN 
        CHCH1=-FLOWDIR*AVGDENS
      ELSE
	  IF(FLOWDIR.GE.0.) THEN
          CHCH1=-FLOWDIR*PS(J-1,I,K)
        ELSE
          CHCH1=-FLOWDIR*PS(J,I,K)
        ENDIF
      ENDIF
      ENDIF
      IF(IBOUND(J-1,I,K).LT.0) GO TO 30
      X1=CHCH1
      XX1=X1
C
C7C-----ACCUMULATE POSITIVE AND NEGATIVE FLOW.
      IF (X1) 10,30,20
   10 CHOUT=CHOUT-XX1
      GO TO 30
   20 CHIN=CHIN+XX1
C
C8------CALCULATE FLOW THROUGH THE RIGHT FACE.
   30 IF(J.EQ.NCOL) GO TO 60
      IF(IBOUND(J+1,I,K).EQ.0) GO TO 60
      IF(IBOUND(J+1,I,K).LT.0 .AND. ICHFLG.EQ.0) GO TO 60
      IF(ILVDA.GT.0) THEN
        CHCH2 = DFR
      ELSE
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
C--SEAWAT: CALL VDWTABLE IF WATER TABLE CORRECTIONS ARE ACTIVE
      IF(IWTABLE.EQ.1.AND.LTHUF(K).NE.0) 
     &  CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                H2,HS2,PS2,Z2,TOP2,BOT2)
	HDIFF=H2-H1
      DIS1=DELR(J+1)/2
      DIS2=DELR(J)/2
      AVGDENS=(DIS1*PS(J+1,I,K)+DIS2*PS(J,I,K))/(DIS1+DIS2)
      D2=CR(J,I,K)*(AVGDENS-DENSEREF)/DENSEREF*(Z2-Z1)
      FLOWDIR=CR(J,I,K)*HDIFF+D2	
	FLOW=FLOW-FLOWDIR
      IF(MFNADVFD.EQ.2) THEN 
         CHCH2=-FLOWDIR*AVGDENS
      ELSE
        IF(FLOWDIR.GE.0.) THEN
          CHCH2=-FLOWDIR*PS(J+1,I,K)
        ELSE
          CHCH2=-FLOWDIR*PS(J,I,K)
        ENDIF
      ENDIF   
      ENDIF
      IF(IBOUND(J+1,I,K).LT.0) GO TO 60
      X2=CHCH2
      XX2=X2
      IF(X2)40,60,50
   40 CHOUT=CHOUT-XX2
      GO TO 60
   50 CHIN=CHIN+XX2
C
C9------CALCULATE FLOW THROUGH THE BACK FACE.
   60 IF(I.EQ.1) GO TO 90
      IF (IBOUND(J,I-1,K).EQ.0) GO TO 90
      IF (IBOUND(J,I-1,K).LT.0 .AND. ICHFLG.EQ.0) GO TO 90
      IF(ILVDA.GT.0) THEN
        CHCH3 = -DFT
      ELSE
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
C--SEAWAT: CALL VDWTABLE IF WATER TABLE CORRECTIONS ARE ACTIVE
      IF(IWTABLE.EQ.1.AND.LTHUF(K).NE.0) 
     &  CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                H2,HS2,PS2,Z2,TOP2,BOT2)
	HDIFF=H2-H1
      DIS1=DELC(I-1)/2
      DIS2=DELC(I)/2
      AVGDENS=(DIS1*PS(J,I-1,K)+DIS2*PS(J,I,K))/(DIS1+DIS2)
      D3=CC(J,I-1,K)*(AVGDENS-DENSEREF)/DENSEREF*(Z2-Z1)
      FLOWDIR=CC(J,I-1,K)*HDIFF+D3
	FLOW=FLOW-FLOWDIR
	IF(MFNADVFD.EQ.2) THEN 
         CHCH3=-FLOWDIR*AVGDENS
      ELSE
        IF(FLOWDIR.GE.0.) THEN
          CHCH3=-FLOWDIR*PS(J,I-1,K)
        ELSE
          CHCH3=-FLOWDIR*PS(J,I,K)
        ENDIF
      ENDIF   
      ENDIF
      IF(IBOUND(J,I-1,K).LT.0) GO TO 90
      X3=CHCH3
      XX3=X3
      IF(X3) 70,90,80
   70 CHOUT=CHOUT-XX3
      GO TO 90
   80 CHIN=CHIN+XX3
C
C10-----CALCULATE FLOW THROUGH THE FRONT FACE.
   90 IF(I.EQ.NROW) GO TO 120
      IF(IBOUND(J,I+1,K).EQ.0) GO TO 120
      IF(IBOUND(J,I+1,K).LT.0 .AND. ICHFLG.EQ.0) GO TO 120
      IF(ILVDA.GT.0) THEN
        CHCH4 = DFB
      ELSE
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
     &  CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                H2,HS2,PS2,Z2,TOP2,BOT2)
	HDIFF=H2-H1
      DIS1=DELC(I+1)/2
      DIS2=DELC(I)/2
      AVGDENS=(DIS1*PS(J,I+1,K)+DIS2*PS(J,I,K))/(DIS1+DIS2)
      D4=CC(J,I,K)*(AVGDENS-DENSEREF)/DENSEREF*(Z2-Z1)
	FLOWDIR=CC(J,I,K)*HDIFF+D4
	FLOW=FLOW-FLOWDIR
      IF(MFNADVFD.EQ.2) THEN 
        CHCH4=-FLOWDIR*AVGDENS
      ELSE
        IF(FLOWDIR.GE.0.) THEN
          CHCH4=-FLOWDIR*PS(J,I+1,K)
        ELSE
          CHCH4=-FLOWDIR*PS(J,I,K)
        ENDIF
      ENDIF   
      ENDIF
      IF(IBOUND(J,I+1,K).LT.0) GO TO 120
      X4=CHCH4
      XX4=X4
      IF (X4) 100,120,110
  100 CHOUT=CHOUT-XX4
      GO TO 120
  110 CHIN=CHIN+XX4
C
C11-----CALCULATE FLOW THROUGH THE UPPER FACE.
  120 IF(K.EQ.1) GO TO 150
      IF (IBOUND(J,I,K-1).EQ.0) GO TO 150
      IF (IBOUND(J,I,K-1).LT.0 .AND. ICHFLG.EQ.0) GO TO 150
      HD=HNEW(J,I,K)
      IF(LTHUF(K).EQ.0) GO TO 122
      TMP=HD
      TOP=BOTM(J,I,LBOTM(K)-1)
      IF(TMP.LT.TOP) HD=TOP
  122 HDIFF=HNEW(J,I,K-1)-HD
      DIS1=ELEV(J,I,K-1)-BOTM(J,I,K-1)
      DIS2=BOTM(J,I,K-1)-ELEV(J,I,K)
      AVGDENS=(DIS1*PS(J,I,K-1)+DIS2*PS(J,I,K))/(DIS1+DIS2)
      D5=CV(J,I,K-1)*(AVGDENS-DENSEREF)/DENSEREF*(ELEV(J,I,K-1)-
     &   ELEV(J,I,K))
	FLOWDIR=CV(J,I,K-1)*HDIFF+D5
C--SEAWAT: CHECK AND CORRECT FOR DEWATERED CASE
	IF(LTHUF(K).GT.0) THEN
	  HS2=SALTHEAD(HNEW(J,I,K),PS(J,I,K),ELEV(J,I,K))
	  BOT1=BOTM(J,I,LBOTM(K)-1)
	  IF(HS2.LT.BOT1) THEN
	      HS1=SALTHEAD(HNEW(J,I,K-1),PS(J,I,K-1),ELEV(J,I,K-1))
	      FLOWDIR=PS(J,I,K-1)/DENSEREF*CV(J,I,K-1)*(HS1-BOT1)
	  ENDIF
	ENDIF
C--SEAWAT: END DEWATERED CORRECTION
	FLOW=FLOW-FLOWDIR
      IF(MFNADVFD.EQ.2) THEN 
        CHCH5=-FLOWDIR*AVGDENS
      ELSE
        IF(FLOWDIR.GE.0.) THEN
          CHCH5=-FLOWDIR*PS(J,I,K-1)
        ELSE
          CHCH5=-FLOWDIR*PS(J,I,K)
        ENDIF
      ENDIF   
      IF(IBOUND(J,I,K-1).LT.0) GO TO 150
      X5=CHCH5
      XX5=X5
      IF(X5) 130,150,140
  130 CHOUT=CHOUT-XX5
      GO TO 150
  140 CHIN=CHIN+XX5
C
C12-----CALCULATE FLOW THROUGH THE LOWER FACE.
  150 IF(K.EQ.NLAY) GO TO 180
      IF(IBOUND(J,I,K+1).EQ.0) GO TO 180
      IF(IBOUND(J,I,K+1).LT.0 .AND. ICHFLG.EQ.0) GO TO 180
      HD=HNEW(J,I,K+1)
      IF(LTHUF(K+1).EQ.0) GO TO 152
      TMP=HD
      TOP=BOTM(J,I,LBOTM(K+1)-1)
      IF(TMP.LT.TOP) HD=TOP
  152 HDIFF=HD-HNEW(J,I,K)
      DIS1=BOTM(J,I,K)-ELEV(J,I,K+1)
      DIS2=ELEV(J,I,K)-BOTM(J,I,K)
      AVGDENS=(DIS1*PS(J,I,K+1)+DIS2*PS(J,I,K))/(DIS1+DIS2)
      D6=CV(J,I,K)*(AVGDENS-DENSEREF)/DENSEREF*(ELEV(J,I,K+1)-
     &   ELEV(J,I,K))
	FLOWDIR=CV(J,I,K)*HDIFF+D6
C--SEAWAT: CHECK AND CORRECT FOR DEWATERED CASE
	IF(LTHUF(K+1).GT.0) THEN
	  HS2=SALTHEAD(HNEW(J,I,K+1),PS(J,I,K+1),ELEV(J,I,K+1))
	  BOT1=BOTM(J,I,LBOTM(K+1)-1)
	  IF(HS2.LT.BOT1) THEN
	      HS1=SALTHEAD(HNEW(J,I,K),PS(J,I,K),ELEV(J,I,K))
	      FLOWDIR=PS(J,I,K)/DENSEREF*CV(J,I,K)*(BOT1-HS1)
	  ENDIF
	ENDIF
C--SEAWAT: END DEWATERED CORRECTION
	FLOW=FLOW-FLOWDIR
      IF(MFNADVFD.EQ.2) THEN 
         CHCH6=-FLOWDIR*AVGDENS
      ELSE         
        IF(FLOWDIR.GE.0.) THEN
          CHCH6=-FLOWDIR*PS(J,I,K+1)
        ELSE
          CHCH6=-FLOWDIR*PS(J,I,K)
        ENDIF
      ENDIF   
      IF(IBOUND(J,I,K+1).LT.0) GO TO 180
      X6=CHCH6
      XX6=X6
      IF(X6) 160,180,170
  160 CHOUT=CHOUT-XX6
      GO TO 180
  170 CHIN=CHIN+XX6
C
C13-----SUM THE FLOWS THROUGH SIX FACES OF CONSTANT HEAD CELL, AND
C13-----STORE SUM IN BUFFER.
 180  RATE=CHCH1+CHCH2+CHCH3+CHCH4+CHCH5+CHCH6
C--SEAWAT: SAVE VOLUMETRIC INSTEAD OF MASS FLUX
	RATE=FLOW
      BUFF(J,I,K)=RATE
C
C14-----PRINT THE FLOW FOR THE CELL IF REQUESTED.
      IF(IBD.LT.0) THEN
         IF(IBDLBL.EQ.0) WRITE(IOUT,899) TEXT,KPER,KSTP
  899    FORMAT(1X,/1X,A,'   PERIOD',I3,'   STEP',I3)
         WRITE(IOUT,900) K,I,J,RATE
  900    FORMAT(1X,'LAYER',I3,'   ROW',I4,'   COL',I4,
     &       '   RATE',1PG15.6)
         IBDLBL=1
      END IF
C
C15-----IF SAVING CELL-BY-CELL FLOW IN LIST, WRITE FLOW FOR CELL.
      IF(IBD.EQ.2)
     &   CALL UBDSVA(IHUFCB,NCOL,NROW,J,I,K,RATE,IBOUND,NLAY)
  200 CONTINUE
C
C16-----IF SAVING CELL-BY-CELL FLOW IN 3-D ARRAY, WRITE THE ARRAY.
      IF(IBD.EQ.1) CALL UBUDSV(KSTP,KPER,TEXT,
     &                   IHUFCB,BUFF,NCOL,NROW,NLAY,IOUT)
C
C17-----SAVE TOTAL CONSTANT HEAD FLOWS AND VOLUMES IN VBVL TABLE
C17-----FOR INCLUSION IN BUDGET. PUT LABELS IN VBNM TABLE.
      CIN=CHIN
      COUT=CHOUT
      VBVL(1,MSUM)=VBVL(1,MSUM)+CIN*DELT
      VBVL(2,MSUM)=VBVL(2,MSUM)+COUT*DELT
      VBVL(3,MSUM)=CIN
      VBVL(4,MSUM)=COUT
      VBNM(MSUM)=TEXT
      MSUM=MSUM+1
C
C18-----RETURN.
      RETURN
      END
c======================================================================
C--SEAWAT: PASS IN ADDITIONAL VARIABLES
      SUBROUTINE SVDF1HUF2B(HNEW,IBOUND,CR,CC,CV,BOTM,NCOL,NROW,NLAY,
     &      KSTP,KPER,IHUFCB,BUFF,IOUT,ICBCFL,DELT,PERTIM,TOTIM,
     &      IDIR,IBDRET,ICHFLG,IC1,IC2,IR1,IR2,IL1,IL2,NBOTM,ILVDA,VDHT,
     &      DELR,DELC)
C
C     ******************************************************************
C     COMPUTE FLOW BETWEEN ADJACENT CELLS IN A SUBREGION OF THE GRID
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE VDFMODULE,   ONLY: IWTABLE,DENSEREF,PS,ELEV,HSALT 
C
      CHARACTER*16 TEXT(3)
      DOUBLE PRECISION HNEW,HD,DFL,DFR,DFT,DFB
C
      DIMENSION HNEW(NCOL,NROW,NLAY), IBOUND(NCOL,NROW,NLAY),
     &     CR(NCOL,NROW,NLAY), CC(NCOL,NROW,NLAY),
     &     CV(NCOL,NROW,NLAY), BOTM(NCOL,NROW,0:NBOTM),
     &     BUFF(NCOL,NROW,NLAY),VDHT(NCOL,NROW,NLAY,3)
C
C--SEAWAT: DIMENSION ADDITIONAL ARRAYS
	DIMENSION DELR(NCOL),DELC(NROW)
C
      COMMON /DISCOM/LBOTM(999),LAYCBD(999)
      COMMON /HUFCOM/HGUHANI(999),HGUVANI(999),LTHUF(999),LAYWT(999)
C
      DATA TEXT(1),TEXT(2),TEXT(3)
     & /'FLOW RIGHT FACE ','FLOW FRONT FACE ','FLOW LOWER FACE '/
C     ------------------------------------------------------------------
C
C1------IF CELL-BY-CELL FLOWS WILL BE SAVED IN A FILE, SET FLAG IBD.
C1------RETURN IF FLOWS ARE NOT BEING SAVED OR RETURNED.
      ZERO=0.
      IBD=0
      IF(IHUFCB.GT.0) IBD=ICBCFL
      IF(IBD.EQ.0 .AND. IBDRET.EQ.0) RETURN
C
C2------SET THE SUBREGION EQUAL TO THE ENTIRE GRID IF VALUES ARE BEING
C2------SAVED IN A FILE.
      IF(IBD.NE.0) THEN
         K1=1
         K2=NLAY
         I1=1
         I2=NROW
         J1=1
         J2=NCOL
      END IF
C
C3------TEST FOR DIRECTION OF CALCULATION;  IF NOT ACROSS COLUMNS, GO TO
C3------STEP 4.  IF ONLY 1 COLUMN, RETURN.
      IF(IDIR.NE.1) GO TO 405
      IF(NCOL.EQ.1) RETURN
C
C3A-----CALCULATE FLOW ACROSS COLUMNS (THROUGH RIGHT FACE).  IF NOT
C3A-----SAVING IN A FILE, SET THE SUBREGION.  CLEAR THE BUFFER.
      IF(IBD.EQ.0) THEN
         K1=IL1
         K2=IL2
         I1=IR1
         I2=IR2
         J1=IC1-1
         IF(J1.LT.1) J1=1
         J2=IC2
      END IF
      DO 310 K=K1,K2
      DO 310 I=I1,I2
      DO 310 J=J1,J2
      BUFF(J,I,K)=ZERO
  310 CONTINUE
C
C3B-----FOR EACH CELL CALCULATE FLOW THRU RIGHT FACE & STORE IN BUFFER.
      IF(J2.EQ.NCOL) J2=J2-1
      DO 400 K=K1,K2
      DO 400 I=I1,I2
      DO 400 J=J1,J2
      IF(ICHFLG.EQ.0) THEN
        IF((IBOUND(J,I,K) .EQ. 0) .OR. ((IBOUND(J,I,K).LT.0) .AND. 
     &     (IBOUND(J+1,I,K).LE.0))) GO TO 400
      ELSE
         IF((IBOUND(J,I,K).EQ.0) .OR. (IBOUND(J+1,I,K).EQ.0)) GO TO 400
      END IF
      IF(ILVDA.GT.0) THEN
        CALL SGWF1HUF2VDF9(I,J,K,VDHT,HNEW,IBOUND,NLAY,NROW,NCOL,
     &                 DFL,DFR,DFT,DFB)
        BUFF(J,I,K) = DFR
      ELSE
C--SEAWAT: USE VARIABLE-DENSITY EQUATIONS
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
     &      CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                    H2,HS2,PS2,Z2,TOP2,BOT2)
         DIS1=DELR(J+1)/2
         DIS2=DELR(J)/2
         AVGDENS=(DIS1*PS(J+1,I,K)+DIS2*PS(J,I,K))/(DIS1+DIS2)  
         D=CR(J,I,K)*(AVGDENS-DENSEREF)/DENSEREF*
     &     (Z1-Z2)
         HDIFF=H1-H2
	   BUFF(J,I,K)=HDIFF*CR(J,I,K)+D
      ENDIF
  400 CONTINUE
C
C3C-----RECORD CONTENTS OF BUFFER AND RETURN.
      IF(IBD.EQ.1)
     &   CALL UBUDSV(KSTP,KPER,TEXT(1),IHUFCB,BUFF,NCOL,NROW,NLAY,IOUT)
      IF(IBD.EQ.2)
     &   CALL UBDSV1(KSTP,KPER,TEXT(1),IHUFCB,BUFF,NCOL,NROW,
     &               NLAY,IOUT,DELT,PERTIM,TOTIM,IBOUND)
      RETURN
C
C4------TEST FOR DIRECTION OF CALCULATION;  IF NOT ACROSS ROWS, GO TO
C4------STEP 5.  IF ONLY 1 ROW, RETURN.
  405 IF(IDIR.NE.2) GO TO 505
      IF(NROW.EQ.1) RETURN
C
C4A-----CALCULATE FLOW ACROSS ROWS (THROUGH FRONT FACE).  IF NOT SAVING
C4A-----IN A FILE, SET THE SUBREGION.  CLEAR THE BUFFER.
      IF(IBD.EQ.0) THEN
         K1=IL1
         K2=IL2
         I1=IR1-1
         IF(I1.LT.1) I1=1
         I2=IR2
         J1=IC1
         J2=IC2
      END IF
      DO 410 K=K1,K2
      DO 410 I=I1,I2
      DO 410 J=J1,J2
      BUFF(J,I,K)=ZERO
  410 CONTINUE
C
C4B-----FOR EACH CELL CALCULATE FLOW THRU FRONT FACE & STORE IN BUFFER.
      IF(I2.EQ.NROW) I2=I2-1
      DO 500 K=K1,K2
      DO 500 I=I1,I2
      DO 500 J=J1,J2
      IF(ICHFLG.EQ.0) THEN
         IF((IBOUND(J,I,K) .EQ. 0) .OR. ((IBOUND(J,I,K).LT.0).AND. 
     &      (IBOUND(J,I+1,K).LE.0))) GO TO 500
      ELSE
         IF((IBOUND(J,I,K).EQ.0) .OR. (IBOUND(J,I+1,K).EQ.0)) GO TO 500
      END IF
      IF(ILVDA.GT.0) THEN
        CALL SGWF1HUF2VDF9(I,J,K,VDHT,HNEW,IBOUND,NLAY,NROW,NCOL,
     &                 DFL,DFR,DFT,DFB)
        BUFF(J,I,K) = DFT
      ELSE
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
     &     CALL VDWTABLE(H1,HS1,PS1,Z1,TOP1,BOT1,
     &                    H2,HS2,PS2,Z2,TOP2,BOT2)
         DIS1=DELC(I+1)/2
         DIS2=DELC(I)/2
         AVGDENS=(DIS1*PS(J,I+1,K)+DIS2*PS(J,I,K))/(DIS1+DIS2)
         D=CC(J,I,K)*(AVGDENS-DENSEREF)/DENSEREF*
     &     (Z1-Z2)
         HDIFF=H1-H2
         BUFF(J,I,K)=HDIFF*CC(J,I,K)+D
      ENDIF
  500 CONTINUE
C
C4C-----RECORD CONTENTS OF BUFFER AND RETURN.
      IF(IBD.EQ.1)
     &   CALL UBUDSV(KSTP,KPER,TEXT(2),IHUFCB,BUFF,NCOL,NROW,NLAY,IOUT)
      IF(IBD.EQ.2)
     &   CALL UBDSV1(KSTP,KPER,TEXT(2),IHUFCB,BUFF,NCOL,NROW,
     &     NLAY,IOUT,DELT,PERTIM,TOTIM,IBOUND)
      RETURN
C
C5------DIRECTION OF CALCULATION IS ACROSS LAYERS BY ELIMINATION.  IF
C5------ONLY 1 LAYER, RETURN.
  505 IF(NLAY.EQ.1) RETURN
C
C5A-----CALCULATE FLOW ACROSS LAYERS (THROUGH LOWER FACE).  IF NOT
C5A-----SAVING IN A FILE, SET THE SUBREGION.  CLEAR THE BUFFER.
      IF(IBD.EQ.0) THEN
         K1=IL1-1
         IF(K1.LT.1) K1=1
         K2=IL2
         I1=IR1
         I2=IR2
         J1=IC1
         J2=IC2
      END IF
      DO 510 K=K1,K2
      DO 510 I=I1,I2
      DO 510 J=J1,J2
      BUFF(J,I,K)=ZERO
  510 CONTINUE
C
C5B-----FOR EACH CELL CALCULATE FLOW THRU LOWER FACE & STORE IN BUFFER.
      IF(K2.EQ.NLAY) K2=K2-1
      DO 600 K=1,K2
      IF(K.LT.K1) GO TO 600
      DO 590 I=I1,I2
      DO 590 J=J1,J2
      IF(ICHFLG.EQ.0) THEN
         IF((IBOUND(J,I,K).LE.0) .AND. (IBOUND(J,I,K+1).LE.0)) GO TO 590
      ELSE
         IF((IBOUND(J,I,K).EQ.0) .OR. (IBOUND(J,I,K+1).EQ.0)) GO TO 590
      END IF
      HD=HNEW(J,I,K+1)
      IF(LTHUF(K+1).EQ.0) GO TO 580
C--SEAWAT: USE HSALT
C--SEAWAT:      TMP=HD
	TMP=HSALT(J,I,K+1)
      TOP=BOTM(J,I,LBOTM(K+1)-1)
      IF(TMP.LT.TOP) HD=TOP
  580 HDIFF=HNEW(J,I,K)-HD
C--SEAWAT: USE VARIABLE-DENSITY EQUATION
      DIS1=BOTM(J,I,K)-ELEV(J,I,K+1)
      DIS2=ELEV(J,I,K)-BOTM(J,I,K)
      AVGDENS=(DIS1*PS(J,I,K+1)+DIS2*PS(J,I,K))/(DIS1+DIS2)
      D=CV(J,I,K)*(AVGDENS-DENSEREF)/DENSEREF*
     &  (ELEV(J,I,K)-ELEV(J,I,K+1)) 
      BUFF(J,I,K)=HDIFF*CV(J,I,K)+D
C--SEAWAT: CHECK AND RECALCULATE FOR DEWATERED CASE
	IF(LTHUF(K+1).GT.0) THEN
	  HS2=SALTHEAD(HNEW(J,I,K+1),PS(J,I,K+1),ELEV(J,I,K+1))
	  BOT1=BOTM(J,I,LBOTM(K+1)-1)
	  IF(HS2.LT.BOT1) THEN
	      HS1=SALTHEAD(HNEW(J,I,K),PS(J,I,K),ELEV(J,I,K))
	      BUFF(J,I,K)=PS(J,I,K)/DENSEREF*CV(J,I,K)*(HS1-BOT1)
	  ENDIF
	ENDIF
C--SEAWAT: END DEWATERED CORRECTION	
  590 CONTINUE
  600 CONTINUE
C
C5C-----RECORD CONTENTS OF BUFFER AND RETURN.
      IF(IBD.EQ.1)
     &   CALL UBUDSV(KSTP,KPER,TEXT(3),IHUFCB,BUFF,NCOL,NROW,NLAY,IOUT)
      IF(IBD.EQ.2)
     &   CALL UBDSV1(KSTP,KPER,TEXT(3),IHUFCB,BUFF,NCOL,NROW,
     &               NLAY,IOUT,DELT,PERTIM,TOTIM,IBOUND)
      RETURN
      END
c======================================================================
C--SEAWAT: PASS IN HSALT, PS, ELEV
      SUBROUTINE VDF1HUF2OT(IOHUFHDS,IOHUFFLWS,HNEW,IHEDFM,IBOUND,NHUF,
     &                  NCOL,NROW,NLAY,HUFTHK,BOTM,NBOTM,CV,DELR,DELC,
     &                  RMLT,NMLTAR,IZON,NZONAR,KSTP,KPER,ISA,ICNVG,
     &                  IOUT,HNOFLO,CHEDFM,DELT,PERTIM,TOTIM,HNWHGU,GS,
     &                  ICBCFL,ICHFLG)
C
C     ******************************************************************
C     PRINT AND RECORD HEADS AND FLOWS INTERPOLATED TO HGU'S
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE VDFMODULE,   ONLY: PS,ELEV,HSALT
C
      CHARACTER*16 TEXT
      DOUBLE PRECISION HNEW
      DIMENSION HNEW(NCOL,NROW,NLAY),HNWHGU(NCOL,NROW,NHUF),
     &    IBOUND(NCOL,NROW,NLAY),BOTM(NCOL,NROW,0:NBOTM),
     &    DELR(NCOL),DELC(NROW),CV(NCOL,NROW,NLAY),
     &    HUFTHK(NCOL,NROW,NHUF,2),RMLT(NCOL,NROW,NMLTAR),
     &    IZON(NCOL,NROW,NZONAR),GS(NCOL,NROW)
      CHARACTER*20 CHEDFM
      CHARACTER*10 HGUNAM
      COMMON /HUFCOMC/HGUNAM(999)
C
      DATA TEXT /'     HEAD IN HGU'/
C     ------------------------------------------------------------------
C
C
      IF(ISA.EQ.0.OR.ICNVG.EQ.0) THEN
        IF(ISA.EQ.0) THEN
          WRITE(IOUT,9) KSTP,KPER
    9     FORMAT(1X,/11X,'NO FLOW EQUATION TO SOLVE IN TIME STEP',I3,
     &      ' OF STRESS PERIOD',I3,/1X,'ALL HEADS ARE 0.0')
        END IF
C
C2------IF ITERATIVE PROCEDURE FAILED TO CONVERGE PRINT MESSAGE
        IF(ICNVG.EQ.0) THEN
          WRITE(IOUT,17) KSTP,KPER
   17     FORMAT(1X,/11X,'****FAILED TO CONVERGE IN TIME STEP',I3,
     &      ' OF STRESS PERIOD',I3,'****')
        END IF
      RETURN
      ENDIF
C
C-------CALCULATE HEADS WITHIN UNITS
      IF(IOHUFHDS.GT.0) THEN
C--SEAWAT: PASS IN HSALT INSTEAD OF HNEW
        CALL SGWF1HUF2HDOT(HNWHGU,HSALT,IBOUND,NHUF,NCOL,NROW,NLAY,
     &               HUFTHK,BOTM,NBOTM,CV,DELR,DELC,RMLT,NMLTAR,
     &               IZON,NZONAR,HNOFLO,IOUT,GS)

        IFIRST=1
        DO 100 NU=1,NHUF
C
C-------CALL ULAPRS OR ULAPRW TO PRINT HEAD.
          WRITE(IOUT,72) KSTP,KPER,NU,HGUNAM(NU)
   72     FORMAT('1',
     &      /2X,'HEAD AT END OF TIME STEP',I3,' IN STRESS PERIOD',I3,
     &      /2X,'IN HYDROGEOLOGIC UNIT',I3,' WITH NAME ',A10,
     &      /2X,71('-'))
          IF(IHEDFM.LT.0) CALL ULAPRS(HNWHGU(1,1,NU),TEXT,KSTP,KPER,
     &                 NCOL,NROW,0,-IHEDFM,IOUT)
          IF(IHEDFM.GE.0) CALL ULAPRW(HNWHGU(1,1,NU),TEXT,KSTP,KPER,
     &                 NCOL,NROW,0,IHEDFM,IOUT)
C
C5------FOR EACH UNIT: DETERMINE IF HEAD SHOULD BE SAVED ON DISK.
C5------IF SO THEN CALL ULASAV OR ULASV2 TO SAVE HEAD.
          IF(IFIRST.EQ.1) WRITE(IOUT,74) IOHUFHDS,KSTP,KPER
   74       FORMAT(1X,/1X,'HEAD IN HYDROGEOLOGIC UNITS WILL BE SAVED ',
     &        'ON UNIT ',I4,' AT END OF TIME STEP',I3,', STRESS PERIOD',
     &        I3)
          IF(IFIRST.EQ.1) IFIRST=0
          IF(CHEDFM.EQ.' ') THEN
            CALL ULASAV(HNWHGU(1,1,NU),TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,
     &                  NROW,NU,IOHUFHDS)
          ELSE
            CALL ULASV2(HNWHGU(1,1,NU),TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,
     &                  NROW,NU,IOHUFHDS,CHEDFM,1,IBOUND(1,1,1))
          END IF
  100   CONTINUE
      ENDIF
C
C-------CALCULATE FLOWS WITHIN UNITS
C--SEAWAT: USE VARIABLE-DENSITY EQUATIONS IN THE FOLLOWING 2 SUBROUTINES
C--SEAWAT: PASS PS AND ELEV
      IF(IOHUFFLWS.GT.0) THEN
      CALL SVDF1HUF2CHFLOT(HNWHGU,HNEW,IBOUND,NHUF,NCOL,NROW,NLAY,
     &                   HUFTHK,BOTM,NBOTM,CV,DELR,DELC,RMLT,NMLTAR,
     &                   IZON,NZONAR,IOUT,GS,IOHUFFLWS,ICBCFL,KSTP,KPER,
     &                   ICHFLG)
      CALL SVDF1HUF2FLOT(HNWHGU,HNEW,IBOUND,NHUF,NCOL,NROW,NLAY,
     &                   HUFTHK,BOTM,NBOTM,CV,DELR,DELC,RMLT,NMLTAR,
     &                   IZON,NZONAR,IOUT,GS,IOHUFFLWS,ICBCFL,KSTP,KPER,
     &                   DELT,PERTIM,TOTIM,ICHFLG)
      ENDIF
C
C6------RETURN.
      RETURN
      END
c======================================================================
C--SEAWAT: PASS HSALT, PS, ELEV
      SUBROUTINE SVDF1HUF2FLOT(HNWHGU,HNEW,IBOUND,NHUF,NCOL,NROW,NLAY,
     &                   HUFTHK,BOTM,NBOTM,CV,DELR,DELC,RMLT,NMLTAR,
     &                   IZON,NZONAR,IOUT,GS,IHUFCB,IBD,KSTP,KPER,
     &                   DELT,PERTIM,TOTIM,ICHFLG)
C
C     ******************************************************************
C     CALCULATE FLOWS WITHIN A GIVEN HYDROGEOLOGIC UNIT
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE VDFMODULE,   ONLY: DENSEREF,PS,ELEV,HSALT
C
      CHARACTER*16 TEXT(3)
      DOUBLE PRECISION HNEW,H0,HXR,HYB,HZB,DHXR,DHYB,DHZB
      DIMENSION HNEW(NCOL,NROW,NLAY),HNWHGU(NCOL,NROW,NHUF),
     &    IBOUND(NCOL,NROW,NLAY),BOTM(NCOL,NROW,0:NBOTM),
     &    DELR(NCOL),DELC(NROW),CV(NCOL,NROW,NLAY),
     &    HUFTHK(NCOL,NROW,NHUF,2),RMLT(NCOL,NROW,NMLTAR),
     &    IZON(NCOL,NROW,NZONAR),GS(NCOL,NROW)
      COMMON /DISCOM/LBOTM(999),LAYCBD(999)
C
      DATA TEXT(1),TEXT(2),TEXT(3)
     & /'FLOW RIGHT FACE ','FLOW FRONT FACE ','FLOW LOWER FACE '/
C     ------------------------------------------------------------------
C
C
      DO 50 ICNT=1,3
        IF(ICNT.EQ.3 .AND. NLAY.EQ.1) GOTO 50
C-----LOOP THROUGH ROWS AND COLUMNS
        DO 100 I=1,NROW
        DO 200 J=1,NCOL
          DO 300 NU=1,NHUF
            TOPU=HUFTHK(J,I,NU,1)
            THCKU=HUFTHK(J,I,NU,2)
            IF(THCKU.EQ.0.0) THEN
              HNWHGU(J,I,NU) = 0.0
              GOTO 300
            ENDIF
            BOTU=TOPU-THCKU
            IF(ICNT.LT.3) THEN
C--SEAWAT: PASS IN HSALT INSTEAD OF HNEW
              CALL SGWF1HUF2HSRCH(NCOL,NROW,NLAY,BOTM,NBOTM,I,J,TOPU,
     &                            BOTU,HSALT,IBOUND,KT,KB,IFLG)
C-------------UNIT ABOVE/BELOW MODEL
              IF(IFLG.EQ.1) THEN
                HNWHGU(J,I,NU) = 0.0
                GOTO 300
              ENDIF
              Q = 0.0
              DO 400 KL=KT,KB
C
C-------------GET CONDUCTANCES
C--SEAWAT: PASS IN HSALT INSTEAD OF HNEW
                CALL SGWF1HUF2C(I,J,KL,NU,CRL,CRR,CCT,
     &                          CCB,THK0,THKL,THKR,THKT,THKB,
     &                          HSALT,DELR,DELC,BOTM,NCOL,NROW,NLAY,
     &                          IOUT,NHUF,NBOTM,RMLT,IZON,NMLTAR,
     &                          NZONAR,HUFTHK,GS)
C
                H0 = HNEW(J,I,KL)
                IB0 = IBOUND(J,I,KL)
C---------------FLOW TO J+1
                IF(ICNT.EQ.1) THEN
                  IBR = 0
                  IF (J.LT.NCOL) IBR = IBOUND(J+1,I,KL)
                  IF (IBR.NE.0) HXR = HNEW(J+1,I,KL)
                  IF(ICHFLG.EQ.0) THEN
                     IF(IB0.LE.0 .AND. IBR.LE.0) GO TO 400
                  ELSE
                     IF(IB0.EQ.0 .OR. IBR.EQ.0) GO TO 400
                  END IF
                  DHXR = H0 - HXR
C--SEAWAT: USE VARIABLE-DENSITY EQUATION
                  Z1=ELEV(J,I,KL)
                  IF(J.LT.NCOL) Z2=ELEV(J+1,I,KL)
                  IF(J.LT.NCOL) DIS1=DELR(J+1)/2
                  DIS2=DELR(J)/2
                  IF(J.LT.NCOL) AVGDENS=(DIS1*PS(J+1,I,KL)+
     +                    DIS2*PS(J,I,KL))/(DIS1+DIS2)
	            D=CRR*(AVGDENS-DENSEREF)/DENSEREF*(Z1-Z2)
                  Q = Q + CRR*DHXR+D
C---------------FLOW TO I+1
                ELSEIF(ICNT.EQ.2) THEN
                  IBB = 0
                  IF (I.LT.NROW) IBB = IBOUND(J,I+1,KL)
                  IF (IBB.NE.0) HYB = HNEW(J,I+1,KL)
                  IF(ICHFLG.EQ.0) THEN
                     IF(IB0.LE.0 .AND. IBB.LE.0) GO TO 400
                  ELSE
                     IF(IB0.EQ.0 .OR. IBB.EQ.0) GO TO 400
                  END IF
                  DHYB = H0 - HYB
C--SEAWAT: USE VARIABLE-DENSITY EQUATION
                  Z1=ELEV(J,I,KL)
                  IF(I.LT.NROW) Z2=ELEV(J,I+1,KL)
                  IF(I.LT.NROW) DIS1=DELC(I+1)/2
                  DIS2=DELC(I)/2
                  IF(I.LT.NROW) AVGDENS=(DIS1*PS(J,I+1,KL)+
     +                    DIS2*PS(J,I,KL))/(DIS1+DIS2)
                  D=CCB*(AVGDENS-DENSEREF)/DENSEREF*(Z1-Z2)
                  Q = Q + CCB*DHYB+D
                ENDIF
  400         CONTINUE
C-------------FLOW TO K+1
            ELSE
C--SEAWAT: PASS IN HSALT RATHER THAN HNEW
              CALL SGWF1HUF2VSRCH(NCOL,NROW,NLAY,BOTM,NBOTM,I,J,
     &                            TOPU,BOTU,HSALT,IBOUND,KT,KB,IFLG)
C-------------UNIT ABOVE/BELOW MODEL
              IF(IFLG.EQ.1) THEN
                HNWHGU(J,I,NU) = 0.0
                GOTO 300
              ENDIF
              Q = 0.0
              IF(KB.LT.NLAY) THEN
                IF(ICHFLG.EQ.0) THEN
                  IF(IBOUND(J,I,KB).LE.0 .AND. IBOUND(J,I,KB+1).LE.0)
     &              GO TO 410
                ELSE
                  IF(IBOUND(J,I,KB).EQ.0 .OR. IBOUND(J,I,KB+1).EQ.0)
     &              GO TO 410
                END IF
                H0 = HNEW(J,I,KB)
                HZB = HNEW(J,I,KB+1)
                CVB = CV(J,I,KB)
                DHZB = HZB - H0
C--SEAWAT: USE VARIABLE-DENSITY EQUATION
                Z1=ELEV(J,I,KB)
	          Z2=ELEV(J,I,KB+1)
                DIS1=BOTM(J,I,LBOTM(KB))-ELEV(J,I,KB+1)
                DIS2=ELEV(J,I,KB)-BOTM(J,I,LBOTM(KB))
                AVGDENS=(DIS1*PS(J,I,KB+1)+DIS2*PS(J,I,KB))/(DIS1+DIS2)
                D=CVB*(AVGDENS-DENSEREF)/DENSEREF*(Z2-Z1)
                Q = CVB*DHZB+D
              ENDIF
            ENDIF
  410     HNWHGU(J,I,NU) = Q
  300     CONTINUE
  200   CONTINUE
  100   CONTINUE
C
C-----WRITE ARRAYS
      IF(IBD.EQ.1) THEN
C
C1------WRITE AN UNFORMATTED RECORD IDENTIFYING DATA.
        CALL UBUDSV(KSTP,KPER,TEXT(ICNT),
     &                   IHUFCB,HNWHGU,NCOL,NROW,NHUF,IOUT)
C        WRITE(IOUT,1) TEXT(ICNT),IHUFCB,KSTP,KPER
C    1   FORMAT(1X,'SGWF1HUF2FLOT SAVING "',A16,'" ON UNIT',I3,
C     &       ' AT TIME STEP',I3,', STRESS PERIOD',I3)
C        WRITE(IHUFCB) KSTP,KPER,TEXT(ICNT),NCOL,NROW,NHUF
C
C2------WRITE AN UNFORMATTED RECORD CONTAINING VALUES FOR
C2------EACH CELL IN THE GRID.
C        WRITE(IHUFCB) HNWHGU
      ENDIF
      IF(IBD.EQ.2) THEN
C
C1------WRITE TWO UNFORMATTED RECORDS IDENTIFYING DATA.
        IF(IOUT.GT.0) WRITE(IOUT,2) TEXT(ICNT),IHUFCB,KSTP,KPER
    2   FORMAT(1X,'SGWF1HUF2FLOT SAVING "',A16,'" ON UNIT',I4,
     &       ' AT TIME STEP',I3,', STRESS PERIOD',I3)
        WRITE(IHUFCB) KSTP,KPER,TEXT(ICNT),NCOL,NROW,NHUF
        WRITE(IHUFCB) 1,DELT,PERTIM,TOTIM
C
C2------WRITE AN UNFORMATTED RECORD CONTAINING VALUES FOR
C2------EACH CELL IN THE GRID.
        WRITE(IHUFCB) HNWHGU
      ENDIF
   50 CONTINUE
C6------RETURN.
      RETURN
      END
c======================================================================
      SUBROUTINE SVDF1HUF2CHFLOT(HNWHGU,HNEW,IBOUND,NHUF,NCOL,NROW,NLAY,
     &                   HUFTHK,BOTM,NBOTM,CV,DELR,DELC,RMLT,NMLTAR,
     &                   IZON,NZONAR,IOUT,GS,IHUFCB,IBD,KSTP,KPER,
     &                   ICHFLG)
C     ******************************************************************
C     COMPUTE FLOW FROM CONSTANT-HEAD CELLS IN HGU's
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE VDFMODULE,   ONLY: DENSEREF,PS,ELEV,HSALT
C
      CHARACTER*16 TEXT
      DOUBLE PRECISION HNEW,HDIFF,HD
C
      DIMENSION HNEW(NCOL,NROW,NLAY),HNWHGU(NCOL,NROW,NHUF),
     &     IBOUND(NCOL,NROW,NLAY),CV(NCOL,NROW,NLAY),
     &     BOTM(NCOL,NROW,0:NBOTM),DELR(NCOL),DELC(NROW),
     &     HUFTHK(NCOL,NROW,NHUF,2),RMLT(NCOL,NROW,NMLTAR),
     &     IZON(NCOL,NROW,NZONAR),GS(NCOL,NROW)
C
      COMMON /DISCOM/LBOTM(999),LAYCBD(999)
      COMMON /HUFCOM/HGUHANI(999),HGUVANI(999),LTHUF(999),LAYWT(999)
C
      DATA TEXT /'   CONSTANT HEAD'/
C     ------------------------------------------------------------------
C
C2------CLEAR BUDGET ACCUMULATORS.
      ZERO=0.
C
C4------LOOP THROUGH EACH CELL AND CALCULATE FLOW INTO MODEL FROM EACH
C4------CONSTANT-HEAD CELL.
      DO 200 I=1,NROW
      DO 200 J=1,NCOL
        DO 300 NU=1,NHUF
          TOPU=HUFTHK(J,I,NU,1)
          THCKU=HUFTHK(J,I,NU,2)
          IF(THCKU.EQ.0.0) THEN
            HNWHGU(J,I,NU) = 0.0
            GOTO 300
          ENDIF
          BOTU=TOPU-THCKU
C--SEAWAT: PASS IN HSALT INSTEAD OF HNEW
          CALL SGWF1HUF2HSRCH(NCOL,NROW,NLAY,BOTM,NBOTM,I,J,TOPU,BOTU,
     &                        HSALT,IBOUND,KT,KB,IFLG)
C-------UNIT ABOVE/BELOW MODEL
          IF(IFLG.EQ.1) THEN
            HNWHGU(J,I,NU) = 0.0
            GOTO 300
          ENDIF
C6------CLEAR VALUES FOR FLOW RATE THROUGH EACH FACE OF CELL.
          CHCH1=ZERO
          CHCH2=ZERO
          CHCH3=ZERO
          CHCH4=ZERO
          CHCH5=ZERO
          CHCH6=ZERO
          RATE=ZERO
          DO 400 K=KT,KB
C
C-----------GET CONDUCTANCES
C--SEAWAT: PASS IN HSALT INSTEAD OF HNEW
            CALL SGWF1HUF2C(I,J,K,NU,CRL,CRR,CCT,
     &                      CCB,THK0,THKL,THKR,THKT,THKB,
     &                      HSALT,DELR,DELC,BOTM,NCOL,NROW,NLAY,
     &                      IOUT,NHUF,NBOTM,RMLT,IZON,NMLTAR,
     &                      NZONAR,HUFTHK,GS)
C
C5----------IF CELL IS NOT CONSTANT HEAD SKIP IT & GO ON TO NEXT CELL.
            IF (IBOUND(J,I,K).GE.0)GO TO 400
            H0 = HNEW(J,I,K)
C7----------CALCULATE FLOW THROUGH THE LEFT FACE.
C           IF THERE IS NO FLOW TO CALCULATE THROUGH THIS FACE, THEN GO ON
C           TO NEXT FACE.  NO FLOW OCCURS AT THE EDGE OF THE GRID, TO AN
C           ADJACENT NO-FLOW CELL, OR TO AN ADJACENT CONSTANT-HEAD CELL.
            IF(J.EQ.1) GO TO 30
            IF(IBOUND(J-1,I,K).EQ.0) GO TO 30
            IF(IBOUND(J-1,I,K).LT.0 .AND. ICHFLG.EQ.0) GO TO 30
C
C7B---------CALCULATE FLOW THROUGH THE LEFT FACE.
            HDIFF=H0-HNEW(J-1,I,K)
C--SEAWAT: USE VARIABLE-DENSITY EQUATIONS
            Z1=ELEV(J,I,K)
            Z2=ELEV(J-1,I,K)
	      DIS1=DELR(J-1)/2
            DIS2=DELR(J)/2
            AVGDENS=(DIS1*PS(J-1,I,K)+DIS2*PS(J,I,K))/(DIS1+DIS2)
            D1=CRL*(AVGDENS-DENSEREF)/DENSEREF*(Z1-Z2)     
            CHCH1=HDIFF*CRL+D1
            IF(IBOUND(J-1,I,K).LT.0) GO TO 30
C
C8----------CALCULATE FLOW THROUGH THE RIGHT FACE.
   30       IF(J.EQ.NCOL) GO TO 60
            IF(IBOUND(J+1,I,K).EQ.0) GO TO 60
            IF(IBOUND(J+1,I,K).LT.0 .AND. ICHFLG.EQ.0) GO TO 60
            HDIFF=H0-HNEW(J+1,I,K)
C--SEAWAT: USE VARIABLE-DENSITY EQUATIONS
            Z1=ELEV(J,I,K)
            Z2=ELEV(J+1,I,K)
            AVGDENS=(DIS1*PS(J+1,I,K)+DIS2*PS(J,I,K))/(DIS1+DIS2)
            D2=CRR*(AVGDENS-DENSEREF)/DENSEREF*(Z1-Z2)
            CHCH2=HDIFF*CRR+D2
            IF(IBOUND(J+1,I,K).LT.0) GO TO 60
C
C9----------CALCULATE FLOW THROUGH THE BACK FACE.
   60       IF(I.EQ.1) GO TO 90
            IF (IBOUND(J,I-1,K).EQ.0) GO TO 90
            IF (IBOUND(J,I-1,K).LT.0 .AND. ICHFLG.EQ.0) GO TO 90
            HDIFF=H0-HNEW(J,I-1,K)
C--SEAWAT: USE VARIABLE-DENSITY EQUATIONS
            Z1=ELEV(J,I,K)
            Z2=ELEV(J,I-1,K)
            DIS1=DELC(I-1)/2
            DIS2=DELC(I)/2
            AVGDENS=(DIS1*PS(J,I-1,K)+DIS2*PS(J,I,K))/(DIS1+DIS2)
            D3=CCT*(AVGDENS-DENSEREF)/DENSEREF*(Z1-Z2)
            CHCH3=HDIFF*CCT+D3
            IF(IBOUND(J,I-1,K).LT.0) GO TO 90
C
C10---------CALCULATE FLOW THROUGH THE FRONT FACE.
   90       IF(I.EQ.NROW) GO TO 120
            IF(IBOUND(J,I+1,K).EQ.0) GO TO 120
            IF(IBOUND(J,I+1,K).LT.0 .AND. ICHFLG.EQ.0) GO TO 120
            HDIFF=H0-HNEW(J,I+1,K)
C--SEAWAT: USE VARIABLE-DENSITY EQUATIONS
            Z1=ELEV(J,I,K)
            Z2=ELEV(J,I+1,K)
            DIS1=DELC(I+1)/2
            DIS2=DELC(I)/2
            AVGDENS=(DIS1*PS(J,I+1,K)+DIS2*PS(J,I,K))/(DIS1+DIS2)
            D4=CCB*(AVGDENS-DENSEREF)/DENSEREF*(Z1-Z2)
            CHCH4=HDIFF*CCB
            IF(IBOUND(J,I+1,K).LT.0) GO TO 120
C
C11---------CALCULATE FLOW THROUGH THE UPPER FACE.
  120       IF(K.EQ.1) GO TO 150
            IF(K.GT.KT) GO TO 150
            IF (IBOUND(J,I,K-1).EQ.0) GO TO 150
            IF (IBOUND(J,I,K-1).LT.0 .AND. ICHFLG.EQ.0) GO TO 150
            HD=H0
            IF(LTHUF(K).EQ.0) GO TO 122
C--SEAWAT: SET TEMP TO HSALT(J,I,K) INSTEAD OF HD
            TMP=HSALT(J,I,K)
            TOP=BOTM(J,I,LBOTM(K)-1)
            IF(TMP.LT.TOP) HD=TOP
  122       HDIFF=HD-HNEW(J,I,K-1)
C--SEAWAT: USE VARIABLE-DENSITY EQUATIONS
            DIS1=ELEV(J,I,K-1)-TOP
            DIS2=TOP-ELEV(J,I,K)
            AVGDENS=(DIS1*PS(J,I,K-1)+DIS2*PS(J,I,K))/(DIS1+DIS2)
            D5=CV(J,I,K-1)*(AVGDENS-DENSEREF)/DENSEREF*(ELEV(J,I,K)-
     &         ELEV(J,I,K-1))
            CHCH5=HDIFF*CV(J,I,K-1)
C--SEAWAT: CHECK AND CORRECT FOR DEWATERED CASE
	      IF(LTHUF(K).GT.0) THEN
	        HS2=SALTHEAD(HNEW(J,I,K),PS(J,I,K),ELEV(J,I,K))
	        BOT1=BOTM(J,I,LBOTM(K)-1)
	        IF(HS2.LT.BOT1) THEN
	          HS1=SALTHEAD(HNEW(J,I,K-1),PS(J,I,K-1),ELEV(J,I,K-1))
	          CHCH5=PS(J,I,K-1)/DENSEREF*CV(J,I,K-1)*(HS1-BOT1)
	        ENDIF
	      ENDIF
C--SEAWAT: END DEWATERED CORRECTION
            IF(IBOUND(J,I,K-1).LT.0) GO TO 150
C
C12---------CALCULATE FLOW THROUGH THE LOWER FACE.
  150       IF(K.EQ.NLAY) GO TO 400
            IF(K.LT.KB) GO TO 400
            IF(IBOUND(J,I,K+1).EQ.0) GO TO 400
            IF(IBOUND(J,I,K+1).LT.0 .AND. ICHFLG.EQ.0) GO TO 400
            HD=HNEW(J,I,K+1)
            IF(LTHUF(K+1).EQ.0) GO TO 152
C--SEAWAT: SET TEMP TO HSALT(J,I,K+1) INSTEAD OF HD
            TMP=HSALT(J,I,K+1)
            TOP=BOTM(J,I,LBOTM(K+1)-1)
            IF(TMP.LT.TOP) HD=TOP
  152       HDIFF=HNEW(J,I,K)-HD
C--SEAWAT: USE VARIABLE-DENSITY EQUATIONS
            DIS1=TOP-ELEV(J,I,K+1)
            DIS2=ELEV(J,I,K)-TOP
            AVGDENS=(DIS1*PS(J,I,K+1)+DIS2*PS(J,I,K))/(DIS1+DIS2)
            D6=CV(J,I,K)*(AVGDENS-DENSEREF)/DENSEREF*(ELEV(J,I,K)-
     &         ELEV(J,I,K+1))
            CHCH6=HDIFF*CV(J,I,K)
C--SEAWAT: CHECK AND CORRECT FOR DEWATERED CASE
	      IF(LTHUF(K+1).GT.0) THEN
	        HS2=SALTHEAD(HNEW(J,I,K+1),PS(J,I,K+1),ELEV(J,I,K+1))
	        BOT1=BOTM(J,I,LBOTM(K+1)-1)
	        IF(HS2.LT.BOT1) THEN
	          HS1=SALTHEAD(HNEW(J,I,K),PS(J,I,K),ELEV(J,I,K))
	          CHCH6=PS(J,I,K)/DENSEREF*CV(J,I,K)*(BOT1-HS1)
	        ENDIF
 	      ENDIF
C--SEAWAT: END DEWATERED CORRECTION
            IF(IBOUND(J,I,K+1).LT.0) GO TO 400
  400     CONTINUE
C
C13-----SUM THE FLOWS THROUGH SIX FACES OF CONSTANT HEAD CELL, AND
C13-----STORE SUM IN BUFFER.
  180     RATE=CHCH1+CHCH2+CHCH3+CHCH4+CHCH5+CHCH6
          HNWHGU(J,I,NU) = RATE
  300   CONTINUE
  200 CONTINUE
C
C16-----IF SAVING CELL-BY-CELL FLOW IN 3-D ARRAY, WRITE THE ARRAY.
      IF(IBD.EQ.1) CALL UBUDSV(KSTP,KPER,TEXT,
     &                   IHUFCB,HNWHGU,NCOL,NROW,NHUF,IOUT)
C
C18-----RETURN.
      RETURN
      END