      PROGRAM boussprop
      integer nalt,ibath,nmode,imode,ieq,nmax,istat,nskip,iflag
      integer kf,np1,np2,i,j,n,ndef,ndepth,nut,ndt,ntmax,itall
      integer ntutot,icorr,idmax,ierr
      integer kl,nrnd,ntotp,nt0,ltape
      parameter(nmax=1000000,ntmax=1000000)
      real u(0:nmax),eta(0:nmax),h(0:nmax),xdepth(nmax),depth(nmax)
      real urnd(0:ntmax),exrnd(0:ntmax),xx(0:ntmax),yy(0:ntmax)
      real w1(7*nmax),w2(7*nmax)
      real eps,x0,xl,l,dx,dt,samp,spos,shval,yu(nmax),ye(nmax),rtall
      real alpha,beta,hmak,hmin,dtp,dtmax,bleft,bright,urt
      character*80 calt(15),fnavn,svar
      logical focus,ecrire(ntmax),ja,spline
      external ja, rtall,itall


      call initkom
      call setkomm('!')



c     Geometry settings
c
c     The basin ranges fron u-node 0 and unode 1
c     Nodes for the depth are in same locations as u nodes,
c     while surface nodes are shifted 0.5*dx to the left
c
c          |                                     |
c          |                                     |
c          |                                     |
c         u(0)                                  u(n)
c         h(0)
c  eta(0)      eta(1)                     eta(n)
c
c

      calt(1)='readfromfile#'
      calt(2)='flat#'
      nalt=2
      call lesfrareg('give bathimetry option#',1,calt,nalt,ibath) 

      if(ibath.eq.1) then
         call promptfil('give depthfile#',fnavn,'depth.dat#',kf,
     %                   xdepth,depth,nmax,ndepth,nskip,iflag)
cc       depth function is now stored in arrays xdepth, depth 
         call gpri(depth,ndepth,dx,xdepth,.true.,0,'hles!',1.0,1.0)
         x0=xdepth(1)
         l=xdepth(ndepth)-x0
         ndef=ndepth+1
      else
        l=rtall(100.0,'give total length#')
        x0=0.0
        ndef=l
      end if

 
      n=itall(ndef,'Give number of grid points#')
      np1=n+1
      np2=n+2
      if(n.gt.nmax) then
        write(0,*)'Too many points'
        stop
      end if

      dx=l/n


cc    make depth-matrix h
cc    h(0) is at point x0

      if(ibath.eq.1) then
       call nureglin(xdepth,depth,ndepth,h,np1,dx,x0,.true.,0.0,0.0,
     %                   eps,iflag) 
      else
         do 100 j=0,n
           h(j)=1.0
 100     continue
      end if


cc    output depth matrix exactly as is used
      call gpri(h,np1,dx,x0,.false.,0,"h!",1.0,1.0)


c     Equation settings

      calt(1)='LSW#'
      calt(2)='NLSW#'
      calt(3)='LBoussinesq#'
      calt(4)='Boussinesq#'
      nalt=4
      call lesfrareg('give equation type#',1,calt,nalt,ieq) 

      if(ja(.true.,'Employ discrete correction term?#')) then
       icorr=1
      else
       icorr=0
      end if

      if(ieq.eq.2.or.ieq.eq.4) then
        alpha=1.0
      else
        alpha=0.0
      end if

      if(ieq.eq.3.or.ieq.eq.4) then
        beta=1.0
      else
        beta=0.0
      end if


cc    Setting of initial conditions

      calt(1)='readfromfile#'
      calt(2)='soliton#'      
      calt(3)='boundary-conditions#'
      calt(4)='combined#'      
      nmode=4
      call lesfrareg('give generation condition#',1,calt,nmode,imode) 

      if(imode.eq.1.or.imode.eq.4) then
        eps=0.0001
        call lesinitial(eps,istat)
        if(istat.ge.2) then
         write(0,*)'No initial conditions read, program stops'
         stop  
        end if
        if(istat.eq.1) write(0,*)'No initial conditions for u invoked'
cc      eta(0) is half a grid increment to the left of u(0)
        xl=x0-0.5*dx
        call exfield(eta,xl,0,np2,dx,1,.false.,.false.,.true.)
        if(istat.eq.0) then
          call exfield(u,x0,0,np1,dx,2,.false.,.false.,.true.)
        end if
      end if

       if(imode.eq.2) then      
        samp=rtall(0.2,'give a/h#')
        spos=0.5*l+x0
        spos=rtall(spos,'give initial position#')
        focus=ja(.true.,'Prop. towards decreasing x#')
cc      dt replaced by zero since dt is not yet assigned value.
cc      Hence u is computed at the same time as eta 
        call soliset(u,eta,x0,dx,h,n,0.0,samp,spos,shval,focus,
     %     ierr,w1,w2)
        if(ierr.gt.0) stop
      end if



      xl=x0-0.5*dx
      call gpri(eta,np2,dx,xl,.false.,0,'eta#',1.0,1.0)

cc      call gpri(u,np1,dx,x0,.false.,0,'u#',1.0,1.0)

cc    find dt,tper and nut

      call dtbouss(h,n,dx,dtmax,beta,hmin,hmak)
      dt=dx/sqrt(hmak)

      nut=itall(0,'Number of cycles#')

cc    reads interval between outputs and find time step
      call lesper(dt,ndt,ntmax)
      ntotp=nut*ndt

      
      call lnumbg('all#','times for printing#',svar,ecrire,0,nut,
     %   ntutot,idmax)


      dtp=dt*0.5
      if(imode.eq.3.or.imode.eq.4) then
        bleft=0.0
        bright=0.0
        spline=.false.
        call regpfil('give u boundary file#',fnavn,'urnd.dat#',kl,xx,yy,
     % ntmax,nrnd,nskip,urnd,dtp,ntotp,dt,spline,eps,.false.,bleft,
     %  bright,iflag,w1)
        call regpfil('give ex boundary file#',fnavn,'exrnd.dat#',kl,xx,
     % yy,ntmax,nrnd,nskip,exrnd,0.0,ntotp,dt,spline,eps,.false.,bleft,
     %  bright,iflag,w1)
       call gpri(urnd,ntotp,dt,dtp,.false.,0,'urnd!',1.0,1.0)
       call gpri(exrnd,ntotp,dt,0.0,.false.,0,'exrnd!',1.0,1.0)
      else
cc    We impose no-flux conditions at the sidewalls
       call null(urnd,1,ntotp,1,1)
       call null(exrnd,1,ntotp,1,1)
      end if


cc    We step u half a time step forward due to the 
cc    staggered grid in time

      urt=(urnd(1)-urnd(0))/dt

      call halv2(u,eta,h,n,dx,dt,dtp,urt,icorr,alpha,beta,w1,w2)

      call gpri(u,np1,dx,x0,.false.,0,'u#',1.0,1.0)

cc    Time integration loop

      ltape=18
      open(unit=ltape,file='rand.dat')

      do 200 i=1,nut
         nt0=(i-1)*ndt
cc       w1 is now ut (acceleration), the value of 
cc       ut is not reused in the next cycle
         call advance(u,w1,eta,h,n,dx,dt,nt0,ndt,urnd,exrnd,icorr,alpha,
     %              beta,ltape,w2)

       
       if(ecrire(i)) then
         call gpri(eta,np2,dx,xl,.false.,i,'eta#',1.0,1.0)
         call gpri(u,np1,dx,x0,.false.,i,'u#',1.0,1.0)
       end if
 200  continue
      close(ltape)

      end


***************************************************************************
      subroutine lesper(dt,nt,ntmax)
      integer nt,ntmax
      real dt
*----------------------------------------------------------------
      integer itall
      real tint,rtall,dt0

      dt0=dt

 100  continue

      write(0,*)'dt_default=',dt 

      tint=rtall(1.0,'give time interval #')

      if(tint.lt.0.0) then
        dt=rtall(dt,'give timestep#')
 150    continue
        nt=itall(0,'give number of timesteps in interval directly!')
        if(nt.gt.ntmax) then
          call primvri('Too large nt#')
          write(0,*)'max:',ntmax,'   value:',nt
          go to 150
        end if
      else
        dt=dt*rtall(1.0,'give reduction factor for dt/dx#')
        nt=tint/dt
        if(nt*dt.lt.tint) nt=nt+1       
        if(nt.gt.ntmax) then
          call primvri('too large nt#')
          write(0,*)'max:',ntmax,'   value:',nt
          dt=dt0
          go to 100
        end if

        dt=tint/nt
      end if


      return
      end




***********************************************************************
      subroutine soliset(u,eta,x0,dx,h,n,dt,samp,spos,shval,focus,
     %                ierr,w1,w2)
      integer n,ierr
      real u(0:n+1),eta(0:n+1),h(0:n+1),w1(*),w2(*)
      real samp,spos,shval,x0,dx,dt
      logical focus
*------------------------------------------------------------------------
      integer np,np2,iflag,i,j
      real xx,eps,lfak,ufak,ds,xmin,xmax,xpos,ev,evx,c,ua,txl,bl2
      real halvb

      ierr=0
      np2=n+2
      np=n+1
      eps=0.001

cc    Find value of shval by linear interpolation

 
      call  rintlin(h(0),np,dx,x0,spos,shval,eps,iflag)
    
      eps=0.0001

      write(0,*)'shval = ',shval
      write(0,*)'spos, x0, dx = ',spos, x0, dx, iflag
cc      shval=1.0
      lfak=shval
      ufak=sqrt(shval)
      if(focus) then
        ufak=-ufak
      end if


      
cc    Compute etas
      txl=(spos-x0+dx*0.5)/lfak
      write(0,*)'txl,eta ',txl
      ds=dx/lfak
      bl2=halvb(samp,eps)
      if(2.0*bl2.ge.14900*ds) then
        write(0,*)'ERROR: To many points in computation of soliton' 
        ierr=1
        return
      end if

      CALL soliprgen(eta,0,np,ds,samp,txl,c,ua,1,0.0,eps) 

      do 50 i=0,np
         eta(i)=eta(i)*lfak
 50   continue  

cc    compute velocities, peak has moved c*dt/2
      
      txl=((spos-ufak*0.5*c*dt)-x0)/lfak
      write(0,*)'txl(u),c,dt,ufak ',txl,c,dt,ufak
      write(0,*)'np,dx,samp,txl,c,ua',np,dx,samp,txl,c,ua
      CALL soliprgen(u,0,n,ds,samp,txl,c,ua,2,0.0,eps)

      do 130 j=0,n
        u(j)=ufak*u(j)
 130   continue



      return
      end


*********************************************************************
*      Routine for reading initial conditions
*      
*           eps - tolerance for recognition of regular grid in depth matrix
*           istat - result in call
*                   istat=0         Both eta and velocities read
*                   istat=1         Only eta given
*                   istat=2         No fields are read
**********************************************************
      subroutine lesinitial(eps,istat)
      integer istat
      real eps
      include 'inp.inc'
*---------------------------------------------------------------
      integer j,kf,nskip,iflag
      real dmin,dmax,rtall
      character*80 stem,navn
      logical ja
      external rtall,ja

 
      istat=0

      do 20 j=1,npmax
         etinp(j)=0.0
         u1(j)=0.0
 20   continue
      
      neta=0
      nu1=0

      

 
      call promptfil('give eta-data#',navn,'eta.in#',kf,xeta,etinp,
     %                npmax,neta,nskip,iflag)
      if(iflag.gt.0) then
        if(iflag.eq.1) write(0,*)'Too many points for eta, max=',npmax
        if(iflag.gt.1) write(0,*)'Irregularities in eta file'
        istat=2
        return
      end if


      call promptfil('give u1-data#',navn,'u1.in#',kf,xu1,u1,
     %                npmax,nu1,nskip,iflag)
      if(iflag.gt.0) then
        if(iflag.eq.1) write(0,*)'Too many points for u max=',npmax
        if(iflag.gt.1) write(0,*)'Irregularities in u file'
        istat=1
        return
      else
        write(0,*)'u1-file = ',navn(1:kf),'  nu1 = ',nu1       
      end if



      call sjekkreg(xeta,neta,dmin,dmax,eps,irege)
      call sjekkreg(xu1,nu1,dmin,dmax,eps,iregu1)

      return
      end

   
**********************************************************************
      subroutine sjekkreg(x,n,dmin,dmax,eps,ir)
      integer n,ir
      real x(n),dmin,dmax,eps
*----------------------------------------------------
      integer i
      real dx

      if(n.eq.1) then
        ir=0
         return
      end if

      dmin=x(2)-x(1)
      dmax=dmin

      do 100 i=3,n
       dx=x(i)-x(i-1)
       if(dx.gt.dmax)dmax=dx      
       if(dx.lt.dmin)dmin=dx      
 100  continue

      if( (dmax-dmin).gt.eps*dmin ) then
        ir=1
      else
        ir=0
      end if

      return
      end


***************************************************************************
      subroutine exfield(y,x,na,nb,dx,nfield,noun,spline,eks)
      integer na,nb,nfield
      real y(na:nb),x(na:nb),dx
      logical noun,spline,eks
      include 'inp.inc'
*---------------------------------------------------------------
      integer i,nw,nval,ierr
      real dxv
      logical spint

c     make grid in wrk
      
      call laggrid(x,na,nb,dx,noun,wrk,nval) 
      nw=nval+1     

      if(spline) then
        if(irege.eq.0) then
          spint=.true.
        else
         spint=.false.
         write(0,*)'exfield, irregular grid, spline selection overruled' 
        end if
      else
        spint=.false.
      end if

      if(spint) then 
        if(nfield.eq.1) then
          dxv=(xeta(neta)-xeta(1))/(neta-1)
          call kstageg(etinp,neta,dxv,xeta(1),y(na),wrk,nval,eks,
     %           0.0,0.0,.false.,0.0,.false.,0.0,ierr,wrk(nw))
        else
          dxv=(xu1(nu1)-xu1(1))/(nu1-1)
          call kstageg(u1,nu1,dxv,xu1(1),y(na),wrk,nval,eks,
     %           0.0,0.0,.false.,0.0,.false.,0.0,ierr,wrk(nw))
        end if
      else
        if(nfield.eq.1) then
           call gureglin(xeta,etinp,neta,wrk,y(na),nval,eks,0.0,0.0,
     %                   0.0001,ierr)
        else
           call gureglin(xu1,u1,nu1,wrk,y(na),nval,eks,0.0,0.0,
     %                   0.0001,ierr)
        end if
      end if

      if(ierr.gt.0) then
       write(0,*)'exfield: ierr =',ierr
      end if

      return
      end


**********************************************************************
      subroutine laggrid(x,na,nb,dx,noun,wx,nval)      
      integer na,nb,nval
      real x(na:nb),wx(1:nb-na+1),dx
      logical noun
*--------------------------------------------------------------
      integer i

      nval=nb-na+1

      if(noun) then
       do 100 i=1,nval
        wx(i)=x(na+i-1)
 100   continue
      else
       do 200 i=1,nval
        wx(i)=x(na)+(i-1)*dx
 200   continue
      end if

      return
      end




*********************************************************************
*      Finds min and max depth and largest stable value of dt
*     for linear solutions
****************************************************************
      subroutine dtbouss(h,n,dx,dt,beta,hmin,hmak)
      integer n,ns,m
      real h(0:n+1),dx,dt,hmin,hmak,beta
*------------------------------------------------------------------
      integer i,np,ntot
      real f1,f2,s,smin,hver,he,hvalg

      ntot=n+2
      f1=dx*dx
      f2=beta*4.0/3.0
      

      CALL ekstrem(h,ntot,HMIN,HMAK)

      if(beta.eq.0.0) then
        dt=dx/sqrt(hmak)
        return
      end if

  
cc    must minimize f1/h+f2*h, he gives minimum for function

      he=sqrt(f1/f2)

      if(he.le.hmak) then
        if(hmin.ge.he) then
          hvalg=hmin
        else
          hvalg=he
        end if
      else
        hvalg=hmak
      end if

      smin=f1/hvalg+f2*hvalg


      dt=sqrt(smin)

      return
      end


*****************************************************************************
      subroutine lnumbg(def,spor,svar,med,n0,ndat,ntot,idmax)
      integer n0,ndat,ntot,idmax
      logical med(n0:ndat)
      character*80 def,spor,svar
*----------------------------------------------------------------------
      integer iflag,i2,kf,j,kjust,nn,ierr
      character*80 mess
      logical fork,ja
                 
      call blank(mess,80)
      mess='min. value:$, max. value:$#'
      nn=1
      call insubst(mess,80,nn,'$',n0,ierr)
      call insubst(mess,80,nn,'$',ndat,ierr)
ccc      write(0,*)'ierr 4 =',ierr
 100  call primvri(mess)


      call lestek(def,spor,svar)
      call frontstrip(svar,80,kjust)
      call strip(svar,80,i2)

      if(fork('all#',svar,1,i2)) then
        call blank(svar,80)
        svar(1:4)='$:*#'
        nn=1
        call insubst(svar,80,nn,'$',n0,ierr)
ccc       write(0,'(a,i6)')'ierr 5 =',ierr
ccc       write(0,'(a,a,a,i8)')'ss ',svar(1:15),':',ierr
       i2=3
      end if
      
ccc      write(0,*)svar(1:15),i2

      call gkonvert(svar,1,i2,n0,ndat,med,iflag)

      if(iflag.gt.0) then
        write(0,*) 'illegal number specification, iflag=',iflag
        if(ja(.true.,'give new  specification?#') ) then
          go to 100
        else
          ntot=0
          return
        end if
      end if

      ntot=0

      do 120 j=n0,ndat
      if(med(j)) then
        ntot=ntot+1
        idmax=j
      end if
 120  continue
     

      return
      end


*********************************************************************
      subroutine advance(u,ut,y,h,n,dx,dt,nt0,nt,urnd,exrnd,ik,
     % alpha,beta,ltape,w2)
      integer n,nt0,nt,ik,ltape
      real u(0:n),ut(0:n),y(0:n+1),h(0:n+1)
      real urnd(0:nt0+nt),exrnd(0:nt0+nt)
      real dx,dt,t,alpha,beta
      real w2(4*n+4)
*-------------------------------------------------------------
      integer i,k,np
      real rv,tu,q0,etm

      np=n+1
      tu=(nt0-0.5)*dt
      do 100 k=1,nt

      rv=-dx*exrnd(k+nt0)
      etm=0.5*(y(0)+y(1))
      call WIMETA(U,y,h,N,-1.0,rv,-1.0,0.0,DX,DT,ALPHA,w2)

      tu=tu+dt

cc      rv=(urnd(k+nt0)-urnd(k+nt0-1))/dt
      rv=(urnd(k+nt0)-u(0))/dt
cc      write(0,*)'rv=',rv

      if(ltape.gt.0) then
       q0=(h(0)+alpha*(0.25*(y(0)+y(1))+0.5*etm))*u(0)
       write(ltape,'(4e16.6)')tu,u(0),rv,q0
      end if

      call wcerut(u,ut,y,h,n,
     %            0.0,rv,0.0,0.0,dx,dt,ik,alpha,beta,w2)

      call usteg(u,ut,n,dt)


 100  continue


      return
      end



************************************************************************
*             W I M E T A
*  Dicrete continuity equation. 
*  Implicit forwarding of surface elevation   
*  parameters:
*              u    = array for velocities                       I
*              ETA1 = array for surfaces                I/O
*              HU   = depth-array (in U-nodes)                   I
*              N    = number of internal nodes, the basin            I
*                       is confined to 0<x<n*dx
*              bv,rv,ah,rh   = coefficients for boundary conditions  I
*                              according to:
*                        eta1(0)+bv*eta1(1)=rv
*                        eta1(n+1)+ah*eta1(n)=rh
*              DX,DT = grid increments i space and time.                I
*              ALF  = value 0.0 gives linear equation                I
*              W   = wor array, observe minimum dimension (below)      D
*
**************************************************************************
      SUBROUTINE WIMETA(U,ETA1,HU,N,BV,RV,AH,RH,DX,DT,ALF,W)
      INTEGER N
      REAL U(0:N),ETA1(0:N+1),HU(0:N),W(4*(n+1)),BV,RV,AH,RH,DX,DT,ALF
*--------------------------------------------------------------
      REAL ETAP,QP,QM,RATE,UM,UP,AL25
      INTEGER J,JP,NM,NP,nv,nd,nhi,nhs

c
c     structure of grid
c
c    t ^
c      |
c      |y   y   y   y   y   y         y   y   
c      |  u   u   u   u   u             u   u  
c      |y   y   y   y   y   y    ...  y   y
c      |  u   u   u   u   u   ...       u   u
c      --------------------------------------------->
c        x=0                           x=n*dx         x
c
c         u-- velocity node
c         y-- surface node
c
c
c

      nv=1
      nd=n+2
      nhi=2*n+3
      nhs=3*n+4

      RATE = DT/DX
      NM   = N-1
      NP=N+1                         
      AL25=0.25*ALF
                   
      ETAP = AL25*(ETA1(0)+ETA1(1))
      UP   = U(0)
      QP   = (ETAP+HU(0))*UP


      DO 100 J=1,N

         JP   = J+1
         ETAP = AL25*(ETA1(J)+ETA1(JP))
         UM   = UP
         UP   = U(J)
         QM   = QP
         QP   = (ETAP+HU(J))*UP
         w(nv-1+J) =-AL25*RATE*UM
         w(nd-1+J)  = 1.0+AL25*RATE*(UP-UM)
         w(nhi-1+J) = AL25*RATE*UP
         w(nhs-1+J)   = ETA1(J)-RATE*(QP-QM)

  100 CONTINUE

         w(nd)  = w(nd)-BV*w(nv)
         w(nhs)   = w(nhs)-w(nv)*RV
         w(nv+N) = AH
         w(nd+N)  = 1.0
         w(nhs+N)   = RH

         CALL TRI(w(nv),w(nd),w(nhi),w(nhs),NP)

         DO 200 J=1,NP
            ETA1(J) = w(nhs-1+J)
 200     CONTINUE

         ETA1(0) = RV-BV*ETA1(1)

      RETURN
      END





**************************************************************************
*                 W C E R U T
*
*  Finds the local accelerations from the Boussinesq momentum equation with 
*  velocity and surface elevation as input. 
*  Grid is as depicted in routine 'Wimeta'.  
*  parameters:  
*              U    = array for velocities                       I
*              UT   = array for local accelerations               O
*              ETA  = array for surface elevations               I
*              HU   = depth array (in u-nodes)                   I
*              N    = number of grid points. The bassin          I
*                     is confined to 0<x<n*dx
*              bv,rv,ah,rh   = coefficients for boundary conditions  I
*                              according to:
*                        ut(0)+bv*ut(1)=rv
*                        ut(n+1)+ah*ut(n)=rh
*              DX,DT = grid increments i space and time.                I
*              DX,DT = gitteravstand i rom og tid.                I
*              IKORR = input value equal to 1 activates correction terms   I
*                      value 0 corresponds to no correction
*              ALF  = value 0.0 gives linear equation                I
*                                                                        *
**************************************************************************
      SUBROUTINE WCERUT(U,UT,ETA,HU,N,BV,RV,AH,RH,DX,DT,IKORR,ALF,ep,w)
      INTEGER N,IKORR
      REAL UT(0:N),U(0:N),ETA(0:N),w(4*n)
      REAL HU(0:N),BV,RV,AH,RH,DX,DT,ALF,ep
*----------------------------------------------------------------
      REAL ETAM,ETAP,HM,HN,HP
      REAL B,A,UN,UP,UM,UR,UL,RATE,QRX,QRT,R6,RDX,RDX2
      INTEGER J,NM,JP,nv,nd,nhi,nhs

      nv=1
      nd=n+1
      nhi=2*n+1
      nhs=3*n+1

      RDX=1.0/DX
      RDX2=RDX*RDX
      RATE=DT/DX
      R6=1.0/6.0
      QRX= IKORR*DX*DX/24.0
      QRT= IKORR*DT*DT/12.0
      NM=N-1
         ETAP=ETA(1)

         HN=HU(0)
         HP=HU(1)
                
C
C        LINEARE LIKNINGER IMPLEMENTERES VED AA NULLE UT ALLE DATA SOM
C        HENTES FRA U-ARRAY.(ALF=0.0 GIR LINEARE LIKN.)
C
         UN=ALF*U(0)
         UP=ALF*U(1)
         UR=0.5*(UN+UP)


         DO 100 J=1,NM

            JP=J+1
            ETAM=ETAP

            ETAP=ETA(JP)
            HM=HN
            HN=HP
            HP=HU(JP)

            UM=UN
            UN=UP
            UP=ALF*U(JP)
            UL=UR
            UR=0.5*(UP+UN)

            A=ep*0.5*HN -QRX/HN + QRT
            B=-ep*HN*HN*R6 -QRX

            w(nv-1+J)=-(A*HM+B)*RDX2-0.25*RATE*UL
            w(nd-1+J) = 1.0 + 2.0*(A*HN+B)*RDX2+RATE*(UP-UM)*0.125
            w(nhi-1+J)=-(A*HP+B)*RDX2+0.25*RATE*UR
            w(nhs-1+J)  = -(ETAP-ETAM)*RDX -(UR*UR-UL*UL)*0.5*RDX

 100     CONTINUE

         w(nd)=w(nd)-BV*w(nv)
         w(nhs)=w(nhs)-w(nv)*RV
         w(nv-1+N)=AH
         w(nd-1+N)=1.0
         w(nhs-1+N)=RH

         CALL TRI(w(nv),w(nd),w(nhi),w(nhs),N)

         DO 200 J=1,N
            UT(J)=w(nhs-1+j)
 200     CONTINUE

         UT(0)=RV-BV*UT(1)

      RETURN
      END



***************************************************************
*                                                             *
*  Computes velocities from accelerations
*  PARAMETERS:  U  =   velocity           I/O                  
*              UT =   acceleration (local)        I
*              N  =   number of nodes      I
*              DT =   timestep            I
*                                                             *
***************************************************************

      SUBROUTINE USTEG(U,UT,N,DT)
      INTEGER N
      REAL U(0:N),UT(0:N),DT
*--------------------------------------------------------------
      INTEGER J

      DO 100 J=0,N
         U(J) = U(J)+DT*UT(J)
  100 CONTINUE

      RETURN
      END


*********************************************************************
*
*    Advances u a time step dtp forward 
*
*******************************************************************
      subroutine halv2(u,y,h,n,dx,dt,dtp,urt,ik,alf,eps,wrk,w2)
      integer n,ik
      real u(0:n+1),y(0:n+1),h(0:n+1)
      real dx,dt,dtp,urt,alf,eps
      real wrk(3*n+7),w2(4*n+4)
*-------------------------------------------------------------
      integer i

      do 60 i=0,n+1
       wrk(i+1)=y(i)
 60   continue

      do 70 i=0,n
         wrk(i+2*n+5)=0.5*(h(i)+h(i+1))
 70   continue

      
      call wCERUT(U,wrk(n+3),wrk(1),wrk(2*n+5),N,
     %            0.0,urt,0.0,0.0,DX,DT,ik,ALF,eps,w2)


      do 80 i=1,n
      U(i)=u(i)+dtp*wrk(n+3+i)
 80   continue



      return
      end
