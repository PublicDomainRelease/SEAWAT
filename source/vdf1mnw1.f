      SUBROUTINE VDF1MNW1fm(nwell2,mxwel2,well2,ibound,delr,delc,cr,cc,
     +    hy,small,Hdry, hcof, rhs, hnew, ncol, nrow, nodes,kiter,
     +    NoMoIter,LAYHDT,BOTM,NBOTM,HK,IUBCF,IULPF,IUHUF,NLAY,
     &    TRPY,HKCC,HANI)
C     VERSION 20020819 KJH
c
c----- MNW1 by K.J. Halford
c
c     ******************************************************************
c     add well flow to source term
c     ******************************************************************
c
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE VDFMODULE,   ONLY: DENSEREF
C
      IMPLICIT NONE
      common /rev23/ PLOSS, iwelpt
      INTEGER MXWEL2, NODES, IBOUND, NCOL, NROW, NBOTM, NLAY, IUHUF,
     &        IULPF, IUBCF, NOMOITER, KITER, NWELL2, LAYCON, LAYHDT,
     &        M, N, IFRL, IWELPT, NE, IIN, IQSLV
      REAL DELR, DELC, CR, CC, HY, HCOF, RHS, HANI, HK, HKCC, HDRY,
     &     BOTM, TRPY
      DOUBLE PRECISION PLOSS
      double precision well2
      double precision zero,qres,rw,cond,Qact,sk,Cf,qdes,csum,chsum,
     * hwell,ipole,hlim,href,ddmax,ddsim,ratio,dhc2w,small
      dimension well2(18,mxwel2), ibound(nodes)
      dimension delr(ncol), delc(nrow),cr(nodes),cc(nodes)
      dimension hy(nodes)
      dimension hcof(nodes), rhs(nodes)
      dimension hnew(nodes)
      double precision hnew
      double precision cel2wel
      COMMON /BCFCOM/LAYCON(999)
      DIMENSION BOTM(NCOL,NROW,0:NBOTM),
     &          HANI(NCOL,NROW,NLAY), HK(NODES), HKCC(NCOL,NROW,NLAY),
     &          LAYHDT(NLAY), TRPY(NLAY)
C
C
      zero = 1.0D-20
c
c                 CR( i, j, k)    ------>   CR  i + 1/2
c                 CC( i, j, k)    ------>   CC  j + 1/2
c                 CV( i, j, k)    ------>   CV  k + 1/2
c
c1------if number of wells <= 0 then return.
      if(nwell2.le.0) return
c
c   Compute cell-to-well conductance for each well node
c
      do m = 1, nwell2
        n = ifrl( well2(1,m) )
        qres = well2(15,m)
c-----if the cell is inactive or specified then bypass processing.
        if( ibound(n).ne.0 ) then
          rw = well2(5,m)
          if( rw .lt. -zero ) then
            cond = -rw
          else
            Qact = well2(3,m)
            sk = well2(6,m)
            Cf = well2(16,m)
            cond = cel2wel(delr,delc,cr,cc,hy,hnew,ncol,nrow,nodes,n,rw,
     &                 sk,Qact,Cf,PLoss,small,Hdry,LAYHDT,BOTM,NBOTM,HK,
     &                     IUBCF,IULPF,IUHUF,NLAY,TRPY,HKCC,HANI)
            if( rw .lt. zero ) cond = cond * 1.0D3
          endif
          well2(11,m) = cond
        endif
      enddo
c
c   Prepare components and limits of a multi-node well
      m = 0
      do while( m .lt. nwell2 )
        m = m + 1
        well2(10,m) = 1.0D31
c
c   A very large # in WL reference array (8,m) triggers multi-node calculation
c
        if( well2(8,m) .gt. 1.0D30 ) then
          ne  = ifrl( well2(7,m) )
          qdes = well2(2,ne)
          qact = qdes
          csum = 0.000D0
          chsum = 0.000D0
          do iin = m, ne
            n = ifrl( well2(1,iin) )
            if( ibound(n) .ne. 0 ) then
              csum  = csum  + well2(11,iin)
              chsum = chsum + well2(11,iin)*hnew(n)
            else
              well2(3,iin) = 0.0D0
            endif
          enddo
c---div0 ---  CSUM could go to zero if the entire well is dry
          if( csum .gt. zero ) then
            hwell = ( qact + chsum ) / csum
          else
            hwell = hnew(n)
          endif
c
c   Test DD constraints, Hlim is assumed to be a Max/Min for Injection/Production wells
          ipole = 0
          if( abs(qdes).gt.zero ) ipole = qdes / abs(qdes)
          hlim = well2(7,ne)
          href = well2(8,ne)
          ddmax = ipole*( hlim - href )
          ddsim = ipole*( hwell - href )
c
          if( ddsim .gt. ddmax ) then
            hwell = hlim
            qact = hwell*csum - chsum
c      DD constraints that stop production are not tested until after the 2nd iteration
            if( kiter .gt.2 ) then
              ratio = 1.00D0
              if( abs(qdes) .gt. small ) ratio =  qact / qdes
              if( ratio .lt. 0.00001D0 ) then
                qact  = 0.000D0
                if (csum .gt. 0.0D0) then
                  hwell = chsum / csum
                else
                  hwell = hnew(n)
                endif
              endif
            endif
          endif
c
c   Assign flow rates and water levels to individual nodes
          do iin = m, ne
            n = ifrl( well2(1,iin) )
            well2(10,iin) = hwell
            qact = ( hwell - hnew(n) ) * well2(11,iin)
            well2(3,iin) = qact
          enddo
          m = ne
        endif       !  End of multi-node conditioning IF statement
      enddo       ! End of overall multi-node test loop
c
c2------process each well in the well list.
      m = 0
      do while( m .lt. nwell2 )
        m = m + 1
        n = ifrl( well2(1,m) )
        qdes = well2(2,m)
c-----if the cell is inactive then bypass processing.
        if( ibound(n).gt.0 ) then
          qact = well2(3,m)
          cond = well2(11,m)
c
          hlim = well2(7,m)
          href = well2(8,m)
          if( well2(10,m).gt.1.0D30 .and. cond.gt.zero ) then
            dhc2w = Qact / cond
c   Process single-node wells
c   Test DD constraints, Hlim is assumed to be a Max/Min for Injection/Production wells
            ipole = 0
            if( abs(qdes).gt.zero ) ipole = qdes / abs(qdes)
            hwell = hnew(n) + dhc2w
            well2(10,m) = hwell
            ddsim = ipole*( hwell - href )
            ddmax = ipole*( hlim - href ) - small
            ratio = 1.00D0
            if( abs(qdes) .gt. zero ) ratio =  qact / qdes
            if( abs(ratio).gt. 1.00D0 ) qact = qdes
            if( ratio     .lt. zero ) qact = 0.0D0
c    Well will be simulated as a specified rate or GHB
            iqslv = 0
            if( ddsim.gt.ddmax .and. ddmax.gt.zero ) iqslv = 1
            if((qdes-qact)**2 .gt. small           ) iqslv = 1
            if(abs(qact).lt.zero .and.  ddsim.gt.ddmax) iqslv = 0
            if(abs(qact).lt.zero .and.  ddsim.lt.ddmax) iqslv = 1
            if(abs(qdes).lt.zero .or. ratio.gt.1.0D0-zero ) iqslv = 0
c
          elseif( cond.lt.zero ) then
            qact = 0.0D0
            iqslv = 0
          else
c Process multi-node wells, Constraints were already tested when allocating flow
            if( mod(kiter,2).eq.0 .and. abs(qact).gt.small ) then
              hlim = well2(10,m)
              iqslv = 1
            else
              qact = well2(3,m)
              iqslv = 0
            endif
          endif
c
C--SEAWAT: MULTIPLY HCOF AND RHS TERMS BY DENSEREF
c   Modify HCOF and RHS arrays
          if( iqslv.ne.0 .and. kiter.gt.1 .and. kiter.lt.NoMoIter ) then
            qact = ( hlim - hnew(n) ) * cond
            hcof(n) = hcof(n) - cond * DENSEREF
            rhs(n)  = rhs(n)  - cond * hlim * DENSEREF
          else
c  Specify Q and solve for head;  add Q to RHS accumulator.
            rhs(n) = rhs(n) - qact * DENSEREF
          endif
          well2(3,m) = qact
        endif
      enddo       !    End of DO WHILE loop
c
      return
      end
c
c_________________________________________________________________________________
c
      SUBROUTINE VDF1MNW1bd(MNWsite,nwell2,mxwel2,vbnm,vbvl,msum,delt,
     +        well2,ibound,hnew,ncol,nrow,nodes,nstp,kstp,kper,iwl2cb,
     +             icbcfl,buff,iout,iowell2,totim,Hdry,PERTIM)
C     VERSION 20030710 KJH
c
c----- MNW1 by K.J. Halford        1/31/98
c     ******************************************************************
c     calculate volumetric budget for wells
c     ******************************************************************
c
c        specifications:
c     ------------------------------------------------------------------
      USE VDFMODULE,   ONLY: DENSEREF
C
      IMPLICIT NONE
      common /rev23/ PLOSS, iwelpt
      INTEGER MXWEL2, MSUM , NODES, IBOUND, IBD, NAUX, NLAY, N, M,
     &        IGRP1, M2, IGRP2, IMULT, IL,M IR, IC, NE, IOCH, IWELPT,
     &        IOBYND, IIN, IOQSUM, IOC, NWELVL, IOUT, ICBCFL, IWL2CB,
     &        KPER, KSTP, NSTP, NROW, NCOL, NWELL2, IOWELL2, IFRL, IR
      REAL HDRY, VBVL, BUFF, PERTIM, TOTIM, DELT, q2, WELL2SP
      DOUBLE PRECISION HWELL, PLOSS
      double precision well2
      double precision zero,ratin,ratout,qwsum,qsum,qwbar,DryTest,q,
     * qd,hlim,href,dd,s,ipole,sNL,sL,qin,qout,qwfsum
      dimension MNWsite(mxwel2)
      dimension vbvl(4,msum),well2(18,mxwel2),
     1          ibound(nodes), buff(nodes)
      dimension iowell2(3)
      dimension hnew(nodes)
      double precision hnew
c
      character*16 text,vbnm(msum),AUXTXT(5)
      character*32 MNWsite
C
c             ----+----1----+-
      text = '             MNW'
      zero = 1.D-25
c     ------------------------------------------------------------------
c
cljk moved this line from below
      nlay = nodes / ncol / nrow
c  clear ratin and ratout accumulators.
      ratin=0.D0
      ratout=0.D0
      ibd=0
      IF(IWL2CB.GT.0) IBD=ICBCFL
C
C2-----IF CELL-BY-CELL FLOWS WILL BE SAVED AS A LIST, WRITE HEADER.
      IF(IBD.EQ.2) THEN
         NAUX = 0   !!   Set to zero -- Change order to dump
c         IF(IAUXSV.EQ.0) NAUX=0
         CALL UBDSV4(KSTP,KPER,TEXT,NAUX,auxtxt,IWL2CB,NCOL,NROW,NLAY,
     1          NWELL2,IOUT,DELT,PERTIM,TOTIM,IBOUND)
      END IF
c  clear the buffer.
      do n = 1, nodes
        buff(n)=0.000000000
      enddo
c -----print the header for individual rates if requested(iwl2cb<0).
      if( iwl2cb.lt.0 .and. icbcfl.ne.0 ) then
        write(iout,'(/,1x,a16,9h PERIOD =,i5,8h  STEP =,i5)')
     +              text, kper,kstp
        write(iout,900)
      endif
  900 format(1x,6h Entry,4h LAY,4h ROW,4h COL,
     + 9x,1hQ,6x,6hH-Well,7x,6hH-Cell,7x,6hDD    ,7x,6hQW-Avg,
     + 6x,8hs-LINEAR,3x,11hs-NonLINEAR)

c  Create WEL1 file if iowell2(1) > 0
      if( iowell2(1).gt.0 .and. nstp.eq.kstp )
     +       write(iowell2(1),'(1i10)') nwell2
c
c2------if there are no wells do not accumulate flow
      if(nwell2.gt.0) then
c
c     Compute flow weighted QW values and store in well2(11,m)
c
        do m = 1, nwell2
          well2(11,m) = well2(3,m) * well2(4,m)
          well2(12,m) = 0.D0
          if( well2(4,m).lt.0.D0 .or. well2(3,m).gt.0.D0 ) then
            well2(11,m) = -1.D0
            well2(12,m) =  1.D0
          endif
        enddo
c
        do m = 1, nwell2
          igrp1 = ifrl( well2(9,m) )
          if( well2(12,m) .lt. 0.5D0 ) then
            qwsum = 0.0000D0
            qsum = 0.0000D0
            do m2 = m, nwell2
              igrp2 = ifrl( well2(9,m2) )
              if( igrp1.eq.igrp2 .and. well2(12,m2).lt.0.5D0) then
                qwsum = qwsum + well2(11,m2)
                qsum  = qsum  + well2(3,m2)
                well2(12,m2) = 1
              endif
            enddo
c
            qwbar = qwsum
            if( qsum**2.gt.zero ) qwbar = qwsum / qsum
            do m2 = m, nwell2
              igrp2 = ifrl( well2(9,m2) )
              if( igrp1.eq.igrp2 .and. well2(4,m2).ge.0.0D0 )
     +            well2(11,m2) = QWbar
            enddo
          endif
        enddo
c
        imult = 0
        do m = 1,nwell2
          n = ifrl( well2(1,m) )
          DryTest = Hnew(n) - Hdry
          if(ABS(DryTest).lt.zero) then
            well2(3,m) = 0.0D0
          endif
          q = well2(3,m)
          well2(17,m)=q     !!7/13/2003 - CZ: preserve q
c
c    Report all wells with production less than the desired rate......
          if(ibound(n).ne.0 .or. ABS(DryTest).lt.zero) then
            il = (n-1) / (ncol*nrow) + 1
            ir = mod((n-1),ncol*nrow)/ncol + 1
            ic = mod((n-1),ncol) + 1
            qd = well2(2,m)
            hlim = well2(7,m)
c -----Modified OUTPUT to hide internal pointers that "Look Funny" in DD column--KJH-- July 10, 2003
            if( well2(8,m) .gt. 1.0D30 )then
              imult = 1
              if( well2(7,m) .lt. 1.0D30 )then
                ne = ifrl(well2(7,m))
                href = well2(8,ne)
              else
              endif
            else
              href = well2(8,m)
            endif
            hwell = well2(10,m)
            QWbar = well2(11,m)
            dd  = hwell - href
c
            ioch = 0
            if( iwl2cb.lt.0 .and. icbcfl.ne.0 ) ioch = 1
c -----print the individual rates if requested(iwl2cb<0).
            if( ioch.eq.1 ) then
              s   = hnew(n) - hwell
              IPOLE = 0
              if( abs(s).gt.zero )  ipole = s / abs(s)
              sNL = ipole * well2(16,m) * abs(q)**PLoss
              sL  = s - sNL
              write(iout,'(1x,i6,3i4,9(1x,g12.6))')
     +         m,il,ir,ic,q, hwell,hnew(n), dd, qwbar, sL, sNL
            endif
c
c -----print the individual rates to auxillary file if requested(iwl2cb<0).
            iobynd = abs(iowell2(2))
            if( iobynd.gt.0 ) then
              if(  ioch.eq.1 .or. iowell2(2).lt.0)then
                write(iobynd,'(a32,1x,2i8,6(1x,g14.8))')
     +          MNWsite(m),m,n,totim,q, hwell,hnew(n), qwbar
              endif
            endif
c  Create WEL1 file if iowell2(1) > 0
            if( iowell2(1).gt.0 .and. nstp.eq.kstp ) then
              write(iowell2(1),'(i9,2i10,1x,g10.4,i10,2x,6(1x,g10.4))')
     +        il,ir,ic,q,0,qd, hwell, hnew(n), dd,href,qwbar
            endif
c
            buff(n) = buff(n) + q
C--SEAWAT: MULTIPLY Q BY DENSE (ASSUME ALL CONCENTRATIONS ARE ZERO FOR NOW)
            if( q.ge.0.0D0 ) then
c -----pumping rate is positive(recharge). add it to ratin.
              ratin = ratin + q * DENSEREF
            else
c -----pumping rate is negative(discharge). add it to ratout.
              ratout = ratout - q * DENSEREF
            endif
          endif
        enddo
c
c   Sum components of  multi-node wells
c
c -----print the header for multi-node rates if requested(iwl2cb<0).
        if(  ioch.eq.1  .and. imult.eq.1 ) then
          write(iout,'(/,5x,31h Multi-Node Rates & Average QW )')
          write(iout,901)
  901     format(1x,16hSite Identifier ,5x,18hENTRY: Begin - End,
     +     2x,7hQ-Total,7x,6hH-Well,7x,6hDD    ,7x,6hQW-Avg)
        endif
c
        m = 0
        do while( m .lt. nwell2 )
          m = m + 1
          if( well2(8,m) .gt. 1.0D30 ) then
            ne  = ifrl( well2(7,m) )
            qwsum = 0.000D0
            qwfsum = 0.000D0
            qsum = 0.000D0
            qin  = 0.000D0
            qout = 0.000D0
            do iin = m, ne
              n = ifrl( well2(1,iin) )
              if( ibound(n).eq.0 ) well2(3,iin) = 0.0D0
              if( well2(4,iin).ge.0.0D0 .and. well2(3,iin).le.0.0D0 )
     &            then
                qwfsum  = qwfsum + well2(3,iin)
                qwsum   = qwsum  + well2(3,iin)*well2(4,iin)
              endif
              if( well2(3,iin).le.0.0D0 ) then
                qin = qin  + well2(3,iin)
              else
                qout = qout  + well2(3,iin)
              endif
              qsum  = qsum  + well2(3,iin)
              well2(3,iin) = 0.00000D0
            enddo
            well2(3,ne) = qsum
c -----print the summed rates if requested(iwl2cb<0).
            qwbar = well2(4,ne)
            if(qwfsum**2 .gt. zero ) qwbar = qwsum / qwfsum
            href = well2(8,ne)
            hwell = well2(10,ne)
            dd  = hwell - href
            if(  ioch.eq.1  ) then
              write(iout,'(A26,1x,2i6,6(1x,g12.6))')
     +         MNWsite(m),m,ne,qsum, hwell, dd, qwbar
            endif
c -----print the summed rates to auxillary file if requested .
            ioQsum = abs(iowell2(3))
            if( ioQsum.gt.0 ) then
              if(  ioch.eq.1 .or. iowell2(3).lt.0)then
                write(ioQsum,102)
     +          MNWsite(m),m,ne,totim,qin,qout, qsum, hwell, qwbar
  102           format(A32,1x,i5.5,1h-,i5.5,12(1x,g14.8))
              endif
            endif
            m = ne
          endif
        enddo
        if(  ioch.eq.1  .and. imult.eq.1 ) then
          write(iout,*)
        endif
c
c  ----- END  MULTI-NODE reporting section -------------
c
cljk        nlay = nodes / ncol / nrow
c6------if cell-by-cell flows will be saved call ubudsv to record them
        if( abs(iwl2cb).gt.0 .and. icbcfl.ne.0 ) then           !! BooBoo Fix--July 10,2003  KJH
          ioc = abs(iwl2cb)
          if( ibd.eq.2 ) then   !!  Write COMPACT budget
            NWELVL  = 1  !!  Dummy value
            do m = 1, nwell2
              n = ifrl( well2(1,m) )
              q = well2(3,m)
              q2=well2(17,m)
cljk          call UBDSVB(ioc,ncol, nrow,n,1,1,Q,well2(1,m),
Cerb 8/24/07  call UBDSVB(ioc,ncol, nrow,n,1,1,Q2,well2(1,m),
Cerb 8/24/07  UBDSVB requires REAL arguments; defined WELL2SP so that it gets
Cerb          promoted w/o loss of precision when using DP compiler option
              WELL2SP = well2(1,m)  
              call UBDSVB(ioc,ncol, nrow,n,1,1,Q2,WELL2SP,
     +                    NWELVL,NAUX,5,IBOUND,NLAY)
            enddo
          else                  !!  Write full 3D array
            call ubudsv(kstp,kper,text,ioc, buff,ncol,nrow,nlay,iout)
          endif
        endif
      endif
c
c7------move rates into vbvl for printing by module bas1ot.
      vbvl(3,msum)=ratin
      vbvl(4,msum)=ratout
c
c8------move rates times time step length into vbvl accumulators.
      vbvl(1,msum) = vbvl(1,msum) + ratin*delt
      vbvl(2,msum) = vbvl(2,msum) + ratout*delt
c
c9------move budget term labels into vbnm for printing.
      vbnm(msum) = text
c
c10-----increment budget term counter(msum).
      msum = msum + 1
c
c11-----return
      return
      end
