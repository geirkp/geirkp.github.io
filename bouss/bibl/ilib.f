*************************************************************************
      SUBROUTINE SIIE(G,FI,N,DARG,FI0,FI1,M,EPS,IFLAG,NIT)
      INTEGER N,M,IFLAG,ANTP,NIT
      REAL FI(N),G,DARG,FI0,FI1,EPS
*--------------------------------------------------------
      integer nhelp,i,iit
      real dfi,arg,fih,argh,gh,fiv,fim,argv,gv,a,fix,feil
      real gx,fimid,argx

      IFLAG=0
      ANTP=N
      N=0
      DFI=(FI1-FI0)/M
      ARG=DARG
      FIH=FI0
      ARGH=0
      GH=G(FI0)
      DO 100 I=1,M
      FIV=FIH
      FIH=FIH+DFI
      FIM=0.5*(FIH+FIV)
      ARGV=ARGH
      GV=GH
      GH=G(FIH)
      ARGH=ARGV+DFI*(GH+GV+4.0*G(FIM))/6.0
      A=(ARGH-ARGV)/DFI
   50 CONTINUE
      IF(ARG.GT.ARGH) GO TO 70
      FIX=FIV+(ARG-ARGV)/A
      FEIL=10*EPS
      IIT=0
   60 CONTINUE
      IF(ABS(FEIL).LT.EPS.OR.IIT.GT.NIT) GO TO 65
      GX=G(FIX)
      IIT=IIT+1
      FIMID=(FIX+FIV)*0.5
      ARGX=ARGV+(FIX-FIV)*(GV+GX+4.0*G(FIMID))/6.0
      FEIL=(ARG-ARGX)/GX
      FIX=FIX+FEIL
      GO TO 60
   65 CONTINUE
      IF(ABS(FEIL).GT.EPS) THEN
        IFLAG=IFLAG+1                        
        nhelp=n+1
        write(0,*)'konvergenssvikt i siie'
        write(0,*)'n= ',nhelp,' feil=',feil,'  fix=',fix
      END IF
      N=N+1      
      FI(N)=FIX
      IF(N.GE.ANTP) RETURN
      ARG=ARG+DARG
      GO TO 50
   70 CONTINUE
  100 CONTINUE
      RETURN
      END



*************************************************************

      SUBROUTINE SECANT(F,X0,X1,XTOL,FTOL,NTOL,IFLAG)
      integer ntol,iflag
      real f,x0,x1,xtol,ftol
*-------------------------------------------------------
      integer n
      real f0,f1,deltax,deltaf

      IFLAG=0
      F0=F(X0)
      DELTAX=X1-X0
      DO 20 N=1,NTOL
      F1=F(X1)
      IF(ABS(F1).LE.FTOL)  GO TO 30
      DELTAF=F0-F1
      IF(DELTAF.EQ.0) GO TO 999
      DELTAX=F1/DELTAF*DELTAX
      X0=X1
      X1=X1+DELTAX
      IF(ABS(DELTAX).LE.XTOL) RETURN
   20 F0=F1
  999 IFLAG=2
      RETURN
   30 IFLAG=1
      RETURN
      END



******************************************************************
      SUBROUTINE SECAUT(F,X0,X1,XTOL,FTOL,NTOL,IFLAG)
      integer ntol,iflag
      real f,x0,x1,xtol,ftol
*-------------------------------------------------------
      integer n
      real f0,f1,deltax,deltaf

      write(0,*) 'N,X0,X1,F0,F1'
      IFLAG=0
      F0=F(X0)
      DELTAX=X1-X0
      DO 20 N=1,NTOL
      F1=F(X1)
      write(0,*) N,X0,X1,F0,F1

      IF(ABS(F1).LE.FTOL)  GO TO 30
      DELTAF=F0-F1
      IF(DELTAF.EQ.0) GO TO 999
      DELTAX=F1/DELTAF*DELTAX
      X0=X1
      X1=X1+DELTAX
      IF(ABS(DELTAX).LE.XTOL) RETURN
   20 F0=F1
  999 IFLAG=2
      RETURN
   30 IFLAG=1
      RETURN
      END


*************************************************************
      SUBROUTINE SECA2(H,X0,y0,X1,y1,XTOL,ytol,FTOL,Gtol,
     %         NTOL,ipr,IFLAG)
      integer ntol,ipr,iflag
      real x0,y0,x1,y1,xtol,ytol,ftol,gtol
      external H
*-------------------------------------------------------
      integer n,itry
      real f1,g1,deltax,deltay,fm,gm,fx,gx,fy,gy,det

      IFLAG=0
      itry=1
 
      deltay=y1-y0
      DELTAX=X1-X0

      if(deltax.eq.0.0 .or. deltay.eq.0.0) then
        iflag=5
        return
      end if

      DO 20 N=1,NTOL
      call h(x1,y1,f1,g1)
      IF(ABS(F1).LE.FTOL .and. abs(g1).lt.gtol) then
        if(ipr.eq.1) then
           write(0,'(a,2e14.6)')'utg, f,g=',f1,g1
        end if
        iflag=1
        return
      end if

 70   continue

      call h(x0,y1,fm,gm)
      fx=(f1-fm)/deltax
      gx=(g1-gm)/deltax   

      call h(x1,y0,fm,gm)
      fy=(f1-fm)/deltay
      gy=(g1-gm)/deltay   
      
      det=fx*gy-fy*gx

      IF(DET.EQ.0.0) then
        write(0,*)'seca2, det=0, fx,fy,gx,gy='
        write(0,*)fx,fy,gx,gy
        if(itry.ge.5) then
          iflag=2
          return
        else
          itry=itry+1
          deltax=10*xtol
          deltay=10*ytol
          x0=x1-deltax
          y0=y1-deltay
          go to 70
        end if
      end if

      deltax=-(f1*gy-g1*fy)/det
      deltay=-(g1*fx-f1*gx)/det
 
      if(deltax.ne.0.0) then
        X0=X1
        X1=X1+DELTAX
      else
        deltax=x1-x0
      end if

      if(deltay.ne.0.0) then
        y0=y1
        y1=y1+deltay
      else
        deltay=y1-y0
      end if

      if(ipr.eq.1) then
        write(0,'(i3,7e12.4)')n,x1,y1,deltax,deltay,f1,g1,det
      end if
      IF(ABS(DELTAX).LE.XTOL .and. abs(deltay).le.ytol) RETURN
 20   continue
 
      iflag=3
      RETURN
      END


*************************************************************                  
*                                                           *
*                 T R I                                     * 
*                                                           *  
*  'TRI' solves a tri-diagonal system of equations
*                                                           *                  
*    N = number of equations                             I  *                  
*    A = sub-diagonal elements in matrix              I     *                  
*    D = diagonal elements in matrix                  I     *                  
*    C = super-diagonal elements in matrix            I     *                 
*    B = right hand side at input,                    I/O   *                 
*        the solution at output                             *             
*                                                           *         
*************************************************************                
      SUBROUTINE TRI(A,D,C,B,N)                   
                    
      INTEGER N     
      REAL A(N),D(N),C(N),B(N)                    
*--------------------------------------------------------                     
      REAL XMULT    
      INTEGER I,NM,NMI                            
                    
      NM = N - 1    
                    
      DO 10 I=2,N   
         XMULT = A(I)/D(I-1)                      
         D(I)  = D(I) - XMULT*C(I-1)              
         B(I)  = B(I) - XMULT*B(I-1)              
                    
 10   CONTINUE      
                    
      B(N) = B(N)/D(N)                            
                    
      DO 20 I=1,NM  
                    
         NMI = N - I
         B(NMI) = ( B(NMI) - C(NMI)*B(NMI+1) )/D(NMI)                          
                    
 20   CONTINUE      
                    
      RETURN        
      END           




*************************************************************
*                                                           *
*                 T R I C                                     *
*                                                           *
*  'TRIC' LOESER ET TRIDIAGONALT KOMPLEKST LIKNINGSETT.     *
*                                                           *
*    N = ANTALL LIKNINGER                             I     *
*    A = ELEMENTER TIL VENSTRE FOR DIAGONALEN         I     *
*    D = ELEMENTER PAA DIAGONALEN                     I     *
*    C = ELEMENTER TIL HOEYRE FOR DIAGONALEN          I     *
*    B = HOEYRESIDEN AV LIKNINGEN (INPUT)             I/O   *
*      = LOESNINGEN X  (OUTPUT)                             *
*                                                           *
*************************************************************

      SUBROUTINE TRIC(A,D,C,B,N)
      INTEGER N
      complex  A(N),D(N),C(N),B(N)
*--------------------------------------------------------
      complex XMULT
      INTEGER I,NM,NMI
      NM = N - 1

      DO 10 I=2,N
         XMULT = A(I)/D(I-1)
         D(I)  = D(I) - XMULT*C(I-1)
         B(I)  = B(I) - XMULT*B(I-1)

 10   CONTINUE

      B(N) = B(N)/D(N)

      DO 20 I=1,NM

         NMI = N - I
         B(NMI) = ( B(NMI) - C(NMI)*B(NMI+1) )/D(NMI)

 20   CONTINUE

      RETURN
      END








**************************************************************************
*            Rutinen loeser en andregradslikning p} formen
*             
*               Y'' +F(x,Y,Y')=0
*    
*     vha. midtpunktdiskretisering og Newton iterasjon p} ikkelineariteten.
*     Parameterere
*              y - den ukjente. Ve kall inneholder den et gjett       I/O
*                  p} loesningen.
*              n -antall indre gitterpunkter.                         I
*              dx - gitteravstand                                     I
*              fver - subrutine som definerer F ovenfor.              I
*                     kallet m} se ut som
*                       call fver(x,yv,yder,f,fy,fyd)
*                     der yv og yd er hhv. verdi av y og midtpunktdiff
*                     for y, mens f,fy og fyd er output og svarer til hhv.
*                     F, dF/dY og dF/dY'
*              al,bl,ar,br - definerer randbetingelser ihht:             I
*                     y(0)+al*y(1)=bl
*                     y(n+1)+ar*y(n)=br
*              iit - maksimalt antall newton iterasjoner                 I
*              r   - relaksasjonsfaktor.                                 I
*              epy,epr - konvergenskrit for hhv. den ukjente og residu.  I
*              skr - verdi=.true. angir mellomutskrifter fra iterasjon   I
*                    denne kommer p} standard output.
*              iflag - feilparameter                                     O
*                    =0 konvergerte utfra epy
*                     1 konvergerte utfra epr
*                     2 konvergens ikke oppnaadd
*                     3 for mange punkter
*
***************************************************************************
      subroutine aglikn(y,n,dx,fver,al,bl,ar,br,iit,r,epy,epr
     %,skr,iflag)
      integer n,iit,iflag
      real y(0:n+1),dx,al,bl,ar,br,r,epy,epr
      logical skr
*-----------------------------------------------------------------------
      integer i,nmax,kit,np
      parameter (nmax=1002)
      real vdig(nmax),hdig(nmax),hs(nmax),dig(nmax),rdx2,rdx
      real x,y1,yder,f,fy,fyd,rmax,umax
      external fver

      if(n+2.gt.nmax) then
        write(0,*)'for mange punkter i aglikn'
        iflag=3
        return
      end if
  
      iflag=0
      rdx=1.0/dx
      rdx2=rdx*rdx
      np=n+1

      kit=0

 100  continue

      kit=kit+1
      if(kit.gt.iit) then
        iflag=2
        return
      end if

      rmax=0.0

      do 150 i=1,n
      x=i*dx
      y1=y(i)      
      yder=0.5*rdx*(y(i+1)-y(i-1))
      call fver(x,y1,yder,f,fy,fyd)
      hs(i)=-rdx2*(y(i+1)-2*y(i)+y(i-1))-f
      if(abs(hs(i)).gt.rmax) rmax=abs(hs(i))
      vdig(i)= rdx2 - 0.5*rdx*fyd
      dig(i)= -2*rdx2 + fy
      hdig(i)=rdx2 + 0.5*rdx*fyd
 150  continue

      if(skr) write(6,*)'kit,rmax=',kit,rmax

      if(rmax.le.epr) then
        iflag=1
        return
      end if

      dig(1)=dig(1)-al*vdig(1)
      hs(1)=hs(1)-vdig(1)*(bl -y(0)-al*y(1))
      dig(np)=1.0
      vdig(np)=ar
      hs(np)=br -y(np)-ar*y(n)


      call tri(vdig,dig,hdig,hs,np)

      umax=abs( bl-y(0)-al*y(1)-al*hs(1))
   
      do 160 i=1,np
      if(abs(hs(i)).gt.umax) umax=abs(hs(i))
      y(i)=y(i)+r*hs(i)
 160  continue

      if(skr) write(6,*)'umax=',umax
      y(0)=bl-al*y(1)
      if(umax.le.epy) return
     
      go to 100

      end
      










**************************************************************
      SUBROUTINE SMOX(A,NS,MS,NG,MG)
      integer ns,ms,ng,mg
      REAL A(0:NS,0:MS)
*------------------------------------------------------
      REAL AM,A0,AP
      INTEGER I,K ,NGM

      NGM=NG-1

      DO 100 K=0,MG
      A0=A(0,K)
      AP=A(1,K)
      A(0,K)=0.75*A0+0.5*AP-0.25*A(2,K)
      DO 50 I=1,NGM
      AM=A0
      A0=AP
      AP=A(I+1,K)
      A(I,K)=0.5*A0+0.25*(AM+AP)
  50  CONTINUE
      A(NG,K)=0.75*AP+0.5*A0-0.25*AM
 100  CONTINUE
      RETURN
      END






**************************************************************
      SUBROUTINE SMOY(A,NS,MS,NG,MG)
      INTEGER NS,MS,NG,MG
      REAL A(0:NS,0:MS)
*------------------------------------------------------
      REAL AM,A0,AP
      INTEGER I,K ,MGM

      MGM=MG-1

      DO 100 I=0,NG
      A0=A(I,0)
      AP=A(I,1)
      A(I,0)=0.75*A0+0.5*AP-0.25*A(I,2)
      DO 50 K=1,MGM
      AM=A0
      A0=AP
      AP=A(I,K+1)
      A(I,K)=0.5*A0+0.25*(AM+AP)
  50  CONTINUE
      A(I,MG)=0.75*AP+0.5*A0-0.25*AM
 100  CONTINUE
      RETURN
      END





*******************************************************************
      SUBROUTINE REG(XR,KY,IP,A,B,VAR)
      integer ip
      REAL XR(IP),A,B,VAR
      INTEGER KY(IP)
*-------------------------------------------------------------------
      REAL SX,SA,SXX,SAA,SXA,DET
      INTEGER I

      IF(IP.LT.2) THEN
        VAR=-1.0
        RETURN
      END IF

      SX=0.0
      SA=0.0
      SXA=0.0
      SAA=0.0
      SXX=0.0

      DO 100 I=1,IP
      SX=SX+XR(I)
      SXX=SX+XR(I)*XR(I)
      SA=SA+KY(I)
      SAA=SAA+KY(I)*KY(I)
      SXA=SXA+KY(I)*XR(I)
  100 CONTINUE

      DET=IP*SAA-SA*SA
      A=(SX*SAA-SXA*SA)/DET
      B=(IP*SXA-SA*SX)/DET
      VAR=SXX+IP*A*A+SAA*B*B-2.0*(SX*A+SXA*B+SA*A*B)

      RETURN
      END



**************************************************************
      SUBROUTINE NULL(A,N0,N1,M0,M1)
      INTEGER N0,N1,M0,M1,I,K
      REAL A(N0:N1,M0:M1)
*--------------------------------------------------------------
      DO 100 K=M0,M1
      DO 100 I=N0,N1
  100 A(I,K)=0.0
      RETURN
      END

**************************************************************
      SUBROUTINE NOISE(A,ns,N,M,Mode,styrke)
      INTEGER Ns,n,m,mode
      REAL A(0:ns,0:m+1),styrke
*--------------------------------------------------------------
      integer k,i
      real pi,btall,ver

      pi=4.0*atan(1.0)

      btall = mode*pi/m

      DO 100 K=0,M+1
      ver=styrke*cos(btall*(k-0.5))
      DO 100 I=0,N+1
  100 A(I,K)=A(I,K)+ver


      btall = mode*pi/n

      DO 200 I=0,N+1
      ver=styrke*cos(btall*(I-0.5))
      DO 200 k=0,m+1
  200 A(I,K)=a(i,k)+ver

      RETURN
      END


*******************************************      
      SUBROUTINE BSPL(HV,BS,N,DELT,DER1,DER2)    
      INTEGER N
      REAL HV(N),BS(0:N+1),DELT,DER1,DER2                      
*--------------------------------------------------                            
      integer nmz
      parameter(nmz=3000)
      REAL WK(4*nmz)

      IF(N.GT.nmz) THEN
        WRITE(*,*)'for mange punkter i BSPL'
      ELSE  
        CALL  BSPLW(HV,BS,N,DELT,DER1,DER2,WK)
      END IF

      RETURN
      END 


*******************************************      
      SUBROUTINE BSPLOLD(HV,BS,N,DELT,DER1,DER2)    
      INTEGER N
      REAL HV(N),BS(0:N+1),DELT,DER1,DER2                      
*--------------------------------------------------                            
      REAL SUB(1500),SUP(1500),DIAG(1500),HS(1500)                             
      INTEGER S
                        
      DO 100 S=1,N      
      SUB(S)=1.0       
      SUP(S)=1.0    
      DIAG(S)=4.0   
      HS(S)=6.0*HV(S)                             
  100 CONTINUE      
      SUP(1)=2.0    
      HS(1)=HS(1)+2.0*DELT*DER1                   
      SUB(N)=2.0    
      HS(N)=HS(N)-2.0*DELT*DER2                   
      CALL TRI(SUB,DIAG,SUP,HS,N)                 
      DO 200 S=1,N  
      BS(S)=HS(S)   
  200 CONTINUE      
      BS(0)=BS(2)-2.0*DELT*DER1                   
      BS(N+1)=BS(N-1)+2.0*DELT*DER2               
      RETURN        
      END           
                    


*******************************************    
*
*       Automatisk setting av ende-deriverete i 
*       splineinterpolasjon.
*****************************************************
      SUBROUTINE BSPLAUT(HV,BS,N,W)    
      INTEGER N
      REAL HV(N),BS(0:N+1),W(4*N)                      
*--------------------------------------------------
      REAL DELT,DER1,DER2

      DELT=1.0
      DER1= 2.0*hv(2)-1.5*hv(1)-0.5*hv(3)
      DER2= -( 2.0*hv(n-1)-1.5*hv(n)-0.5*hv(n-2))

      call BSPLW(HV,BS,N,DELT,DER1,DER2,W)    

      return
      end

                    

*******************************************      
      SUBROUTINE BSPLW(HV,BS,N,DELT,DER1,DER2,W)    
      INTEGER N
      REAL HV(N),BS(0:N+1),DELT,DER1,DER2,W(4*N)                      
*--------------------------------------------------
      INTEGER S,ISUB,ISUP,IDIAG,IHS
      INTEGER ISUBA,ISUPA,IDIAGA,IHSA

      ISUB=1
      IDIAG=N+1
      ISUP=2*N+1
      IHS=3*N+1
      ISUBA=ISUB-1
      IDIAGA=IDIAG-1
      ISUPA=ISUP-1
      IHSA=IHS-1
                        
      DO 100 S=1,N      
      W(ISUBA+S)=1.0       
      W(ISUPA+S)=1.0    
      W(IDIAGA+S)=4.0   
      W(IHSA+S)=6.0*HV(S)                             
  100 CONTINUE      
      W(ISUPA+1)=2.0    
      W(IHSA+1)=W(IHSA+1)+2.0*DELT*DER1                   
      W(ISUBA+N)=2.0    
      W(IHSA+N)=W(IHSA+N)-2.0*DELT*DER2                   
      CALL TRI(W(ISUB),W(IDIAG),W(ISUP),W(IHS),N)                 
      DO 200 S=1,N  
      BS(S)=W(IHSA+S)   
  200 CONTINUE      
      BS(0)=BS(2)-2.0*DELT*DER1                   
      BS(N+1)=BS(N-1)+2.0*DELT*DER2               
      RETURN        
      END           
                    

************************************************************
*                 BSPLGEN
*
*     HV - array of values to be interpolated                         I
*     BS - (0:N+1) array with spline coefiicients                     O
*     N - nymber of points                                            I
*     DELT - grid increment (constant)                                I
*     ik1,ik2 type of boundary condition for left and right boundary   I
*         respectively.
*         value=0 : derivative adapted to value d1,d2
*         value=1 : one sided differences used for derivative
*         value=2 : second derivative set to d1 and d2.
*                   if hv(0)=0 and d1=0 we then obtain an antisymmetric
*                   condition at left boundary etc. 
*     d1,d2 - values of derivativea t left and right boundary          I
*     w  - work array, at least 4*N long                                D 
*
****************************************************************************
      SUBROUTINE BSPLGEN(HV,BS,N,DELT,ik1,D1,ik2,D2,W)    
      INTEGER N,ik1,ik2
      REAL HV(N),BS(0:N+1),DELT,D1,D2,W(4*N)                      
*--------------------------------------------------
      INTEGER S,ISUB,ISUP,IDIAG,IHS
      INTEGER ISUBA,ISUPA,IDIAGA,IHSA
      real der1,der2,p1,p2,r1,r2,q1,q2,h1,h2

c
c     The left hand boundary condition, say, is formulated as
c        p1*bs(0)+q1*bs(1)+r1*bs(2)=h1
c     where the coefficients depend on the actual condition. To retain
c     the tri-diagonal structure this equation is then used to eliminate
c     bs(0) from the interpolation equation for hv(1): 
c         bs(0)+4*bs(1)+bs(2)=6*hv(1)
c

      if(ik1.lt.2) then
        if(ik1.eq.0) then
          der1=d1*delt
        else
          der1=2.0*hv(2)-1.5*hv(1)-0.5*hv(3)
        end if
        p1=-0.5
        q1=0.0
        r1=0.5
        h1=der1
      else
        p1=1.0
        q1=-2.0
        r1=1.0
        h1=d1
      end if
        
      if(ik2.lt.2) then
        if(ik2.eq.0) then
          der2=d2*delt
        else
          der2=-( 2.0*hv(n-1)-1.5*hv(n)-0.5*hv(n-2))
        end if
        p2=-0.5
        q2=0.0
        r2=0.5
        h2=der2
      else
        p2=1.0
        q2=-2.0
        r2=1.0
        h2=d2
      end if
        


      ISUB=1
      IDIAG=N+1
      ISUP=2*N+1
      IHS=3*N+1
      ISUBA=ISUB-1
      IDIAGA=IDIAG-1
      ISUPA=ISUP-1
      IHSA=IHS-1
                        
      DO 100 S=1,N      
      W(ISUBA+S)=1.0       
      W(ISUPA+S)=1.0    
      W(IDIAGA+S)=4.0   
      W(IHSA+S)=6.0*HV(S)                             
  100 CONTINUE 
      if(p1.ne.0.0) then     
        W(ISUP)=W(isup)-r1/p1
        W(idiag)=w(idiag)-q1/p1    
        W(IHS)=W(IHS)-h1/p1
      else
        W(ISUP)=r1
        W(idiag)=q1    
        W(IHS)=h1
      end if   

      if(r2.ne.0.0) then     
        W(ISUBA+N)=W(ISUBA+N)-p2/r2   
        W(IHSA+N)=W(IHSA+N)-h2/r2
        W(idiaga+n)=w(idiaga+n)-q2/r2    
      else
        W(ISUBA+N)=p2
        W(idiaga+n)=q2    
        W(IHSA+N)=h2
      end if
   

      CALL TRI(W(ISUB),W(IDIAG),W(ISUP),W(IHS),N)                 
      DO 200 S=1,N  
      BS(S)=W(IHSA+S)   
  200 CONTINUE      

      if(p1.ne.0.0) then     
        BS(0)=-q1*bs(1)/p1 -r1*BS(2)/p1 +h1/p1
      else
        BS(0)=-4.0*bs(1) -BS(2) +6.0*hv(1)
      end if

      if(r2.ne.0.0) then     
        BS(n+1)=-q2*bs(n)/r2 -p2*BS(n-1)/r2 +h2/r2
      else
        BS(n+1)=-4.0*bs(n) -BS(n-1) +6.0*hv(N)
      end if


      RETURN        
      END           
                    
                    
                    
                    
                    
**************************************************************************
*
*                     S P D E R
*   
*     Beregner funksjonsverdier av en splines-interpolant generert ved
*     bsplw. 
*      parametere:
*              X - koordinat
*              X0 - posisjon tilh|rende B-spline med koeff. bs(1)       I
*              DELT - punktavstand                                      I
*              BS - array med spline-koeff.                             I
*              NP - antall interpolasjonspunkter (np+2 B-splines)       I
*              d0,d1,d2 - hhv. funk.ver 1.ste og 2.dre derivert         O
*****************************************************************************
      subroutine spder(X,X0,DELT,BS,NP,d0,d1,d2)                
      INTEGER NP    
      REAL BS(0:NP+1),X,X0,DELT,d0,d1,d2                
*------------------------------------------------ 
      REAL XV,S,sx,bm,bp,bmm,bpp     
      INTEGER NX    
                  
      sx=1.0/delt  
      XV=(X-X0)/DELT+1.0                          
      NX=XV         
      S=XV-NX       
      XV=S-1.0      
      if(nx.lt.0 .or. nx.gt.np) then
        write(0,*)'punkt utenfor interval i spder'
        return
      end if
                    
      bm=bs(nx)
      bp=bs(nx+1)

      IF(NX.EQ.0) THEN                            
        bmm=0.0
      ELSE          
        bmm=bs(nx-1)
      END IF        

      IF(NX.EQ.np) THEN                            
        bpp=0.0
      ELSE          
        bpp=bs(nx+2)
      END IF        

      d0=-bmm*XV*XV*XV/6.0+bm*(2.0/3.0-S*S*(1.0-0.5*S))          
     %   +Bp*(2.0/3.0-XV*XV*(1+0.5*XV))+bpp*S*S*S/6.0               

      d1=sx*(-bmm*XV*XV*0.5+bm*(-2.0*S+1.5*s*s)          
     %   -Bp*(2.0*xv+1.5*xv*xv)+bpp*S*S*0.5)               

      d2=sx*sx*(-bmm*XV + bm*(-2.0+3.0*s)          
     %   -Bp*(2.0+3.0*xv)+bpp*S)               


      RETURN        
      END           

                    
                    
*********************************************************                       
      FUNCTION HG(X,X0,DELT,BS,NP)                
      INTEGER NP    
      REAL BS(0:NP+1),X,X0,DELT,HG                
*------------------------------------------------ 
      REAL XV,S     
      INTEGER NX    
                    
      XV=(X-X0)/DELT+1.0                          
      NX=XV         
      S=XV-NX       
      XV=S-1.0      
                    
      IF(NX.EQ.0) THEN                            
      HG=BS(NX)*(2.0/3.0-S*S*(1.0-0.5*S))         
     %   +BS(NX+1)*(2.0/3.0-XV*XV*(1+0.5*XV))+BS(NX+2)*S*S*S/6.0                
      ELSE          
      IF(NX.GE.NP) THEN                           
      HG=-BS(NX-1)*XV*XV*XV/6.0+BS(NX)*(2.0/3.0-S*S*(1.0-0.5*S))          
     %   +BS(NX+1)*(2.0/3.0-XV*XV*(1+0.5*XV))     
      ELSE          
      HG=-BS(NX-1)*XV*XV*XV/6.0+BS(NX)*(2.0/3.0-S*S*(1.0-0.5*S))          
     %   +BS(NX+1)*(2.0/3.0-XV*XV*(1+0.5*XV))+BS(NX+2)*S*S*S/6.0               
      END IF        
      END IF        
      RETURN        
      END           




******************************************************************
*
*                   S T A G
*
*     Innholdet i et en-D gitter overf|res til et annet med forskj|vet
*     startpunkt og annen gitteravstand. Det benyttes spline interpolasjon.
*     parametere:
*             a -  array for kilde-gitter                                 I
*             na,dxa,xa - hhv antall punkter, gitteravstand og pos av    I
*                         f|rste punkt i kilde-gitter 
*             b - array for kopigitter                                   O
*             nb,dxb,xb - hhv antall punkter, gitteravstand og pos av    I
*                         f|rste punkt i kopi-gitter. Dersom dette gitteret 
*                         g}r utenfor  kilde-gitteret etterfylles med 
*                         hhv. a(1) og a(na).
*             iflag - feilparameter                                      O
*             w  - arbeidsarray                                          D
*
***************************************************************************
      subroutine stag(a,na,dxa,xa,b,nb,dxb,xb,iflag,w)
      integer na,nb,iflag
      real a(na),dxa,xa,b(nb),dxb,xb,w(5*na+15)
*------------------------------------------------------------------
      integer k,k1,k2,na3,k1m
      real xver,hg,x1,x2,der1,der2

      na3=na+3
      iflag=0

      x1=max(xa,xb)
      x2=min(xa+(na-1)*dxa,xb+(nb-1)*dxb)

      if(x1.ge.x2) then
        iflag=1
        return
      end if

      k1=(x1-xb)/dxb+1
      if( (xb+(k1-1)*dxb).lt.x1) k1=k1+1
      k2=(x2-xb)/dxb+1

      DER1=0.5*(4.0*a(2)-3.0*A(1)-A(3))/dxa
      DER2=-0.5*(4.0*A(Na-1)-3.0*A(Na) -A(Na-2))/dxa

      call bsplw(a,w(1),na,dxa,der1,der2,w(na3))

      do 100 k=k1,k2
        xver=(k-1)*dxb+xb
        b(k)=hg(xver,xa,dxa,w(1),na)
 100    continue

      k1m=k1-1

      do 200 k=1,k1m
        b(k)=a(1)
 200    continue

      do 300 k=k2+1,nb
        b(k)=a(na)
 300    continue

      
      return

      end 

**************************************************************
*     OLD variant, impelemented as wrap of staggeg
***********************************************************
      subroutine stagge(a,na,dxa,xa,b,nb,dxb,xb,eks,bleft,bright,
     %           iflag,w)
      integer na,nb,iflag
      real a(na),dxa,xa,b(nb),dxb,xb,bleft,bright,w(5*na+15)
      logical eks
*------------------------------------------------------------------
      real lddum,hddum
      call staggeg(a,na,dxa,xa,b,nb,dxb,xb,eks,bleft,bright,
     %           .false.,lddum,.false.,hddum,iflag,w)
      return
      end


******************************************************************
*
*                   S T A G G E G
*
*     Innholdet i et en-D gitter overf|res til et annet med forskj|vet
*     startpunkt og annen gitteravstand. Det benyttes spline interpolasjon.
*     parametere:
*             a -  array for kilde-gitter                                 I
*             na,dxa,xa - hhv antall punkter, gitteravstand og pos av    I
*                         f|rste punkt i kilde-gitter 
*             b - array for kopigitter                                   O
*             nb,dxb,xb - hhv antall punkter, gitteravstand og pos av    I
*                         f|rste punkt i kopi-gitter. 
*             eks       -Dersom dette gitteret                          I
*                         g}r utenfor  kilde-gitteret etterfylles med 
*                         hhv. a(1) og a(na) hvis eks er sann, med
*                         bleft (utenfor a(1)) og bright ellers
*             bleft, bright - verdier som settes paa b der den gaar      I
*                             utenfor a
*             lse,hse  - mark if boundary derivatives in spline approximant I
*                        are set or are to be  
*                        found from one-sided differences.
*                        lse=.true. indicates that a derivative to the
*                        left is provided etc.
*             lder,hder - Values for boundary derivatives in spline      I
*                         approximant. Used if lse or hse are true. 
*             iflag - feilparameter                                      O
*             w  - arbeidsarray                                          D
*
***************************************************************************
      subroutine staggeg(a,na,dxa,xa,b,nb,dxb,xb,eks,bleft,bright,
     %           lse,lder,hse,hder,iflag,w)
      integer na,nb,iflag
      real a(na),dxa,xa,b(nb),dxb,xb,lder,hder,bleft,bright,w(5*na+15)
      logical eks,lse,hse
*------------------------------------------------------------------
      integer k,k1,k2,na3,k1m
      real xver,hg,x1,x2,der1,der2,lval,rval

      na3=na+3
      iflag=0

      x1=max(xa,xb)
      x2=min(xa+(na-1)*dxa,xb+(nb-1)*dxb)

      if(x1.ge.x2) then
        iflag=1
        return
      end if

      k1=(x1-xb)/dxb+1
      if( (xb+(k1-1)*dxb).lt.x1-0.001*dxb) k1=k1+1
      k2=(x2-xb)/dxb+1
      if( (xb+(k2)*dxb).lt.x2+0.001*dxb) k2=k2+1

      if(lse) then
        der1=lder
      else
        DER1=0.5*(4.0*a(2)-3.0*A(1)-A(3))/dxa
      end if

      if(hse) then
        der2=hder
      else
        DER2=-0.5*(4.0*A(Na-1)-3.0*A(Na) -A(Na-2))/dxa
      end if

      call bsplw(a,w(1),na,dxa,der1,der2,w(na3))

      do 100 k=k1,k2
        xver=(k-1)*dxb+xb
        b(k)=hg(xver,xa,dxa,w(1),na)
 100    continue

      k1m=k1-1

      if(eks) then
       rval=a(na)
       lval=a(1)
      else
       rval=bright
       lval=bleft
      end if

      do 200 k=1,k1m
        b(k)=lval
 200    continue

      do 300 k=k2+1,nb
        b(k)=rval
 300    continue

      
      return

      end 

******************************************************************
*
*                   S T A G G E G
*
*     Innholdet i et en-D gitter overf|res til et annet med forskj|vet
*     startpunkt og annen gitteravstand. Det benyttes spline interpolasjon.
*     parametere:
*             a -  array for kilde-gitter                                 I
*             na,dxa,xa - hhv antall punkter, gitteravstand og pos av    I
*                         f|rste punkt i kilde-gitter 
*             b - array for kopigitter                                   O
*             nb,dxb,xb - hhv antall punkter, gitteravstand og pos av    I
*                         f|rste punkt i kopi-gitter. 
*             irvalg     -Dersom dette gitteret                          I
*                         g}r utenfor  kilde-gitteret etterfylles med: 
*                         irvalg=0 hhv. a(1) og a(na)  
*                         irvalg=1 bleft (utenfor a(1)) og bright ellers
*                         irvalg=2 intet settes utenfor
*             spline - .true. for spline, linear otherwise
*             bleft, bright - verdier som settes paa b der den gaar      I
*                             utenfor a
*             lse,hse  - mark if boundary derivatives in spline approximant I
*                        are set or are to be  
*                        found from one-sided differences.
*                        lse=.true. indicates that a derivative to the
*                        left is provided etc.
*             lder,hder - Values for boundary derivatives in spline      I
*                         approximant. Used if lse or hse are true. 
*             iflag - feilparameter                                      O
*             eps - toleranse for innenfor..                             I
*             w  - arbeidsarray                                          D
*
***************************************************************************
      subroutine gstaggeg(a,na,dxa,xa,b,nb,dxb,xb,irvalg,spline,
     %           bleft,bright,lse,lder,hse,hder,iflag,eps,w)
      integer na,nb,irvalg,iflag
      real a(na),dxa,xa,b(nb),dxb,xb,lder,hder,bleft,bright,eps
      real w(5*na+15)
      logical spline,lse,hse
*------------------------------------------------------------------
      integer k,k1,k2,na3,k1m,nk
      real xver,hg,x1,x2,der1,der2,lval,rval,xbk,umdef

      umdef=-666.0
      na3=na+3
      iflag=0

      x1=max(xa,xb)
      x2=min(xa+(na-1)*dxa,xb+(nb-1)*dxb)

      if(x1.gt.x2) then
        iflag=1
        return
      end if

      k1=(x1-xb)/dxb+1
      if( (xb+(k1-1)*dxb).lt.x1-eps*dxb) k1=k1+1
      k2=(x2-xb)/dxb+1
      if( (xb+(k2)*dxb).lt.x2+eps*dxb) k2=k2+1

      if(spline) then
        if(lse) then
          der1=lder
        else
          DER1=0.5*(4.0*a(2)-3.0*A(1)-A(3))/dxa
        end if

        if(hse) then
          der2=hder
        else
          DER2=-0.5*(4.0*A(Na-1)-3.0*A(Na) -A(Na-2))/dxa
        end if

        call bsplw(a,w(1),na,dxa,der1,der2,w(na3))

        do 100 k=k1,k2
          xver=(k-1)*dxb+xb
          b(k)=hg(xver,xa,dxa,w(1),na)
 100    continue
      else
        do 110 k=1,na
          w(k)=(k-1)*dxa+xa
 110    continue
        nk=(k2-k1)+1
        xbk=xb+(k1-1)*dxb
        call ureglin(w,a,na,b(k1),nk,dxb,xbk,.true.,umdef,umdef,
     %                   eps,iflag)
      end if


      if(irvalg.eq.2) return
      k1m=k1-1

      if(irvalg.eq.0) then
       rval=a(na)
       lval=a(1)
      else
       rval=bright
       lval=bleft
      end if

      do 200 k=1,k1m
        b(k)=lval
 200    continue

      do 300 k=k2+1,nb
        b(k)=rval
 300    continue

      
      return

      end 

******************************************************************
*
*                   K S T A G E G
*
*     Innholdet i et en-D gitter overf|res til et annet med ikke-uniform
*     oppdeling. Det benyttes spline interpolasjon.
*     parametere:
*             a -  array for kilde-gitter                                 I
*             na,dxa,xa - hhv antall punkter, gitteravstand og pos av    I
*                         f|rste punkt i kilde-gitter 
*             b - array for kopigitter                                    O
*             xb - array for b's gitterposisjoner                         I
*                     Dersom dette gitteret 
*                     g}r utenfor  kilde-gitteret etterfylles med 
*                     hhv. a(1) og a(na).
*             nb - antall punkter i b                                    I
*             eks       -Dersom kopi-gitteret                          I
*                         g}r utenfor  kilde-gitteret etterfylles med 
*                         hhv. a(1) og a(na) hvis eks er sann, med
*                         bleft (utenfor a(1)) og bright ellers
*             bleft, bright - verdier som settes paa b der den gaar      I
*                             utenfor a
*             lse,hse  - mark if boundary derivatives in spline approximant I
*                        are set or are to be  
*                        found from one-sided differences.
*                        lse=.true. indicates that a derivative to the
*                        left is provided etc.
*             lder,hder - Values for boundary derivatives in spline      I
*                         approximant. Used if lse or hse are true. 
*             iflag - feilparameter                                      O
*             w  - arbeidsarray                                          D
*
***************************************************************************
      subroutine kstageg(a,na,dxa,xa,b,xb,nb,eks,bleft,bright,
     %           lse,lder,hse,hder,iflag,w) 
      integer na,nb,iflag
      real a(na),dxa,xa,b(nb),xb(nb),lder,hder,bleft,bright,w(5*na+15)
      logical eks,lse,hse
*------------------------------------------------------------------
      integer k,k1,k2,na3,k1m
      real hg,x1,x2,der1,der2,lval,rval

      na3=na+3
      iflag=0

      x1=max(xa,xb(1))
      x2=min(xa+(na-1)*dxa,xb(nb))

      if(x1.ge.x2) then
        iflag=1
        return
      end if

      k1=1
 50   continue
      if(xb(k1).ge.x1) go to 60
      k1=k1+1
      go to 50

 60   continue

      k2=nb
 70   continue
      if(xb(k2).le.x2) go to 80
      k2=k2-1
      go to 70

 80   continue


      if(lse) then
        der1=lder
      else
        DER1=0.5*(4.0*a(2)-3.0*A(1)-A(3))/dxa
      end if

      if(hse) then
        der2=hder
      else
        DER2=-0.5*(4.0*A(Na-1)-3.0*A(Na) -A(Na-2))/dxa
      end if

      call bsplw(a,w(1),na,dxa,der1,der2,w(na3))

      do 100 k=k1,k2
        b(k)=hg(xb(k),xa,dxa,w(1),na)
 100    continue

      k1m=k1-1

      if(eks) then
       rval=a(na)
       lval=a(1)
      else
       rval=bright
       lval=bleft
      end if

      do 200 k=1,k1m
        b(k)=lval
 200    continue

      do 300 k=k2+1,nb
        b(k)=rval
 300    continue

      
      return

      end 


******************************************************************
*
*                   Q S T G
*
*     Innholdet i et en-D gitter overf|res til et annet med ikke-uniform
*     oppdeling. Det benyttes spline interpolasjon. More robust than
*     kstageg, in the sense that xb does not need to be monotoneous 
*     parametere:
*             a -  array for kilde-gitter                                 I
*             na,dxa,xa - hhv antall punkter, gitteravstand og pos av    I
*                         f|rste punkt i kilde-gitter 
*             b - array for kopigitter                                    O
*             xb - array for b's gitterposisjoner                         I
*                     Dersom dette gitteret 
*                     g}r utenfor  kilde-gitteret etterfylles med 
*                     hhv. a(1) og a(na).
*             nb - antall punkter i b                                    I
*             eks       -Dersom kopi-gitteret                          I
*                         g}r utenfor  kilde-gitteret etterfylles med 
*                         hhv. a(1) og a(na) hvis eks er sann, med
*                         bleft (utenfor a(1)) og bright ellers
*             bleft, bright - verdier som settes paa b der den gaar      I
*                             utenfor a
*             lse,hse  - mark if boundary derivatives in spline approximant I
*                        are set or are to be  
*                        found from one-sided differences.
*                        lse=.true. indicates that a derivative to the
*                        left is provided etc.
*             lder,hder - Values for boundary derivatives in spline      I
*                         approximant. Used if lse or hse are true. 
*             iflag - feilparameter                                      O
*             w  - arbeidsarray                                          D
*
***************************************************************************
      subroutine qstg(a,na,dxa,xa,b,xb,nb,eks,bleft,bright,
     %           lse,lder,hse,hder,iflag,w) 
      integer na,nb,iflag
      real a(na),dxa,xa,b(nb),xb(nb),lder,hder,bleft,bright,w(5*na+15)
      logical eks,lse,hse
*------------------------------------------------------------------
      integer k,na3,k1m
      real hg,x1,x2,der1,der2,lval,rval

      na3=na+3
      iflag=0

      x1=xa
      x2=xa+(na-1)*dxa


      if(eks) then
       rval=a(na)
       lval=a(1)
      else
       rval=bright
       lval=bleft
      end if

      if(lse) then
        der1=lder
      else
        DER1=0.5*(4.0*a(2)-3.0*A(1)-A(3))/dxa
      end if

      if(hse) then
        der2=hder
      else
        DER2=-0.5*(4.0*A(Na-1)-3.0*A(Na) -A(Na-2))/dxa
      end if

      call bsplw(a,w(1),na,dxa,der1,der2,w(na3))

      do 100 k=1,nb
        if(xb(k).le.x1) then
          b(k)=lval
        else
          if(xb(k).le.x2) then
            b(k)=hg(xb(k),xa,dxa,w(1),na)
          else
            b(k)=rval
          end if
        end if
 100  continue


      
      return

      end 


******************************************************************
*
*                   UREGLIN
*
*     A one-D dataset given on a non-uniform grid is interpolated onto
*     a uniform grid by linear interpolation. The source grid must 
*     contain coordinates in increasing order.
*     ERRORS:      do not work for nb=1
*     parameters:
*             xs, ys -  arrays for original grid and data               I
*                       The xs must increase monotonously
*             na - number of points in original grid
*             b - array for interpolated values on regular grid          O
*             nb,dxb,xb - number of points, grid increment and position  I
*                         of first point in regular grid 
*             eks       - If the regular grid exeeds the original
*                         grid this parameter defines the extrapolation. I
*                         If eks is .true. a(1) and a(n) is used for
*                         regular nodes that are outside the original grid.
*                         Otherwise the values  bleft ( left of xs(1)) and
*                         bright are used 
*             bleft, bright - see above                                  I
*             eps  - tolerance for defining a point inside original grid   I
*             iflag - feilparameter                                      O
***************************************************************************
      subroutine ureglin(xs,ys,na,b,nb,dxb,xb,eks,bleft,bright,
     %                   eps,iflag)
      integer na,nb,iflag
      real xs(na),ys(na),b(nb),dxb,xb,bleft,bright,eps
      logical eks
*------------------------------------------------------------------
      integer k,k1,k2,k1m,ipos
      real xver,x1,x2,lval,rval,xm,xp,wg

 
      iflag=0

      x1=max(xs(1),xb)
      x2=min(xs(na),xb+(nb-1)*dxb)


      if(na.eq.1) then
        iflag=2
        return
      end if

      if(x1.ge.x2) then
        if(xb.lt.xs(1)) then
          k1=nb+1
          k2=nb
        else
          k1=1
          k2=0
        end if
      else
        k1=(x1-xb)/dxb+1
        if( (xb+(k1-1)*dxb).lt.x1-eps*dxb) k1=k1+1
        k2=(x2-xb)/dxb+1
        if( (xb+k2*dxb).lt.x2+eps*dxb) k2=k2+1
      end if


      ipos=1

      do 100 k=k1,k2
        xver=(k-1)*dxb+xb
 60     continue
        if(xver.le.xs(ipos+1) .or. ipos.eq.(na-1)) go to 80
        ipos=ipos+1
        go to 60
 80     continue

        xp=xs(ipos+1)
        xm=xs(ipos)
        if(xp.le.xm) then
          iflag=3
          return
        end if

        wg=(xver-xm)/(xp-xm)
       
        b(k)=(1.0-wg)*ys(ipos)+wg*ys(ipos+1)

 100  continue

      k1m=k1-1

      if(eks) then
       rval=ys(na)
       lval=ys(1)
      else
       rval=bright
       lval=bleft
      end if

      do 200 k=1,k1m
        b(k)=lval
 200    continue

      do 300 k=k2+1,nb
        b(k)=rval
 300    continue

      
      return

      end 

******************************************************************
*
*                   GUREGLIN
*
*     A one-D dataset given on a non-uniform grid is interpolated onto
*     another non-uniform grid by linear interpolation. Both grids must 
*     contain coordinates in increasing order.
*     ERRORS:      do not work for nb=1
*     parameters:
*             xs, ys -  arrays for original grid and data               I
*                       The xs must increase monotonously
*             na - number of points in original grid
*             xb - array for interpolated grid                            I
*             yb - array for interpolated values                         O
*             nb  - number of interpolated points                        I
*                         of first point in regular grid 
*             eks       - If the regular grid exeeds the original
*                         grid this parameter defines the extrapolation. I
*                         If eks is .true. a(1) and a(n) is used for
*                         regular nodes that are outside the original grid.
*                         Otherwise the values  bleft ( left of xs(1)) and
*                         bright are used 
*             bleft, bright - see above                                  I
*             eps  - tolerance for defining a point inside original grid   I
*             iflag - error parameter                O
***************************************************************************
      subroutine gureglin(xs,ys,na,xb,yb,nb,eks,bleft,bright,
     %                   eps,iflag)
      integer na,nb,iflag
      real xs(na),ys(na),yb(nb),xb(nb),bleft,bright,eps
      logical eks
*------------------------------------------------------------------
      integer k,k1,k2,k1m,ipos
      real xver,x1,x2,lval,rval,xm,xp,wg

 
      iflag=0

      x1=xs(1)
      x2=xs(na)


      if(na.eq.1) then
        iflag=2
        return
      end if


      if(eks) then
       rval=ys(na)
       lval=ys(1)
      else
       rval=bright
       lval=bleft
      end if

      ipos=1

      do 100 k=1,nb
        xver=xb(k)
 60     continue
        if(xver.le.xs(ipos+1) .or. ipos.eq.(na-1)) go to 80
        ipos=ipos+1
        go to 60
 80     continue

        xp=xs(ipos+1)
        xm=xs(ipos)
        if(xp.le.xm) then
          iflag=3
          return
        end if

        if(xver.lt.x1-eps) then
           yb(k)=lval
        else
         if(xver.gt.x2+eps) then
           yb(k)=rval
         else
           wg=(xver-xm)/(xp-xm)
       
           yb(k)=(1.0-wg)*ys(ipos)+wg*ys(ipos+1)
         end if
        end if

 100  continue


      
      return

      end 

******************************************************************
*
*                   NUREGLIN
*
*     A one-D dataset given on a non-uniform grid is interpolated onto
*     a uniform grid by linear interpolation. The source grid must 
*     contain coordinates in increasing order.
*     parameters:
*             xs, ys -  arrays for original grid and data               I
*                       The xs must increase monotonously
*             na - number of points in original grid
*             yb - array for interpolated values on regular grid          O
*             nb,dxb,xb - number of points, grid increment and position  I
*                         of first point in regular grid 
*             eks       - If the regular grid exeeds the original
*                         grid this parameter defines the extrapolation. I
*                         If eks is .true. a(1) and a(n) is used for
*                         regular nodes that are outside the original grid.
*                         Otherwise the values  bleft ( left of xs(1)) and
*                         bright are used 
*             bleft, bright - see above                                  I
*             eps  - tolerance for defining a point inside original grid   I
*             iflag - error parameter                                      O
***************************************************************************
      subroutine nureglin(xs,ys,na,yb,nb,dxb,xb,eks,bleft,bright,
     %                   eps,iflag)
      integer na,nb,iflag
      real xs(na),ys(na),yb(nb),xb,dxb,bleft,bright,eps
      logical eks
*------------------------------------------------------------------
      integer k,k1,k2,k1m,ipos
      real xver,x1,x2,lval,rval,xm,xp,wg

 
      iflag=0

      x1=xs(1)
      x2=xs(na)


      if(na.eq.1) then
        iflag=2
        return
      end if


      if(eks) then
       rval=ys(na)
       lval=ys(1)
      else
       rval=bright
       lval=bleft
      end if

      ipos=1

      do 100 k=1,nb
        xver=xb+(k-1)*dxb
 60     continue
        if(xver.le.xs(ipos+1) .or. ipos.eq.(na-1)) go to 80
        ipos=ipos+1
        go to 60
 80     continue

        xp=xs(ipos+1)
        xm=xs(ipos)
        if(xp.le.xm) then
          iflag=3
          return
        end if

        if(xver.lt.x1-eps) then
           yb(k)=lval
        else
         if(xver.gt.x2+eps) then
           yb(k)=rval
         else
           wg=(xver-xm)/(xp-xm)
       
           yb(k)=(1.0-wg)*ys(ipos)+wg*ys(ipos+1)
         end if
        end if

 100  continue


      
      return

      end 


******************************************************************
*
*                   FGUREGLIN
*
*     A one-D dataset given on a non-uniform grid is interpolated onto
*     another non-uniform grid by linear interpolation. Both grids must 
*     contain coordinates in increasing order.
*     ERRORS:      do not work for nb=1
*     parameters:
*             xs, ys -  arrays for original grid and data               I
*                       The xs must increase monotonously
*             na - number of points in original grid
*             xb - array for interpolated grid                            I
*             yb - array for interpolated values                         O
*             nb  - number of interpolated points                        I
*                         of first point in regular grid 
*             ieks       - If the regular grid exeeds the original
*                         grid this parameter defines the extrapolation. I
*                         ieks=0 : values of yb are left unchanged
*                         ieks=1 :  a(1) and a(n) is used for
*                            regular nodes that are outside the original grid.
*                         ieks >=2 : the values  bleft ( left of xs(1)) and
*                              bright are used 
*                         If eks is .true. a(1) and a(n) is used for
*                         regular nodes that are outside the original grid.
*                         Otherwise the values  bleft ( left of xs(1)) and
*                         bright are used 
*             bleft, bright - see above                                  I
*             eps  - tolerance for defining a point inside original grid   I
*             iflag - error parameter                O
***************************************************************************
      subroutine fgureglin(xs,ys,na,xb,yb,nb,ieks,bleft,bright,
     %                   eps,iflag)
      integer na,nb,ieks,iflag
      real xs(na),ys(na),yb(nb),xb(nb),bleft,bright,eps
*------------------------------------------------------------------
      integer k,k1,k2,k1m,ipos
      real xver,x1,x2,lval,rval,xm,xp,wg

 
      iflag=0

      x1=xs(1)
      x2=xs(na)


      if(na.eq.1) then
        iflag=2
        return
      end if


      if(ieks.eq.1) then
       rval=ys(na)
       lval=ys(1)
      else
       rval=bright
       lval=bleft
      end if

      ipos=1

      do 100 k=1,nb
        xver=xb(k)
 60     continue
        if(xver.le.xs(ipos+1) .or. ipos.eq.(na-1)) go to 80
        ipos=ipos+1
        go to 60
 80     continue

        xp=xs(ipos+1)
        xm=xs(ipos)
        if(xp.le.xm) then
          iflag=3
          return
        end if

        if(xver.lt.x1-eps) then
         if(ieks.gt.0)yb(k)=lval
        else
         if(xver.gt.x2+eps) then
           if(ieks.gt.0)yb(k)=rval
         else
           wg=(xver-xm)/(xp-xm)
       
           yb(k)=(1.0-wg)*ys(ipos)+wg*ys(ipos+1)
         end if
        end if

 100  continue


      
      return

      end 


******************************************************************
*
*                   punktUREG
*
*     A one-D dataset given on a non-uniform grid is interpolated onto
*     a single point by linear interpolation. 
*     parameters:
*             xs, ys -  arrays for original grid and data               I
*                       The xs must increase monotonously
*             na - number of points in original grid
*             xb - interpolation point                            I
*             yb - interpolated values                         O
*             eps  - tolerance for defining a point inside original grid   I
*             iflag - error parameter                O
***************************************************************************
      subroutine punktureg(xs,ys,na,xb,yb,eps,iflag)
      integer na,iflag
      real xs(na),ys(na),yb,xb,eps
*------------------------------------------------------------------
      integer k,k1,k2,k1m,ipos
      real x1,x2,xm,xp,wg

 
      iflag=0

      x1=xs(1)
      x2=xs(na)
  

      if(na.eq.1) then
        iflag=2
        return
      end if

      if(xb.lt.x1) then
        if(xb+eps.ge.x1) then
          yb=ys(1)
        else
          iflag=1
        end if

        return
      end if

      if(xb.gt.x2) then
        if(xb-eps.le.x2) then
          yb=ys(na)
        else
          iflag=1
        end if

        return
      end if



      ipos=1

 60   continue
        if(xb.le.xs(ipos+1) .or. ipos.eq.(na-1)) go to 80
        ipos=ipos+1
      go to 60

 80   continue

      xp=xs(ipos+1)
      xm=xs(ipos)
      if(xp.le.xm) then
        iflag=3
        return
      end if

      wg=(xb-xm)/(xp-xm) 
      yb=(1.0-wg)*ys(ipos)+wg*ys(ipos+1)
      
      return

      end 

******************************************************************
*
*                   HREGuLIN
*
*     A one-D dataset given on a uniform grid is interpolated onto
*     another non-uniform grid by linear interpolation. Both grids must 
*     contain coordinates in increasing order.
*     ERRORS:      do not work for nb=1
*     parameters:
*             ys -  array for original  data                              I
*             na - number of points in original grid
*             dx - increment in source grid                               I
*             x0 - position of ys(1)
*             xb - array for interpolated grid                            I
*             yb - array for interpolated values                         O
*             nb  - number of interpolated points                        I
*                         of first point in regular grid 
*             ieks       - If the regular grid exeeds the original
*                         grid this parameter defines the extrapolation. I
*                         ieks=0 : values of yb are left unchanged
*                         ieks=1 :  a(1) and a(n) is used for
*                            regular nodes that are outside the original grid.
*                         ieks >=2 : the values  bleft ( left of xs(1)) and
*                              bright are used 
*                         If eks is .true. a(1) and a(n) is used for
*                         regular nodes that are outside the original grid.
*                         Otherwise the values  bleft ( left of xs(1)) and
*                         bright are used 
*             bleft, bright - see above                                  I
*             eps  - tolerance for defining a point inside original grid   I
*             iflag - error parameter                O
***************************************************************************
      subroutine hregulin(ys,na,dx,x0,xb,yb,nb,ieks,bleft,bright,
     %                   eps,iflag)
      integer na,nb,ieks,iflag
      real ys(na),dx,x0,yb(nb),xb(nb),bleft,bright,eps
*------------------------------------------------------------------
      integer k,k1,k2,k1m,ipos
      real xver,x1,x2,lval,rval,xm,xp,wg

 
      iflag=0

      x1=x0
      x2=x0+(na-1)*dx


      if(na.eq.1) then
        iflag=2
        return
      end if


      if(ieks.eq.1) then
       rval=ys(na)
       lval=ys(1)
      else
       rval=bright
       lval=bleft
      end if

      ipos=1

      do 100 k=1,nb
        xver=xb(k)
 60     continue
        if(xver.le.x0+ipos*dx .or. ipos.eq.(na-1)) go to 80
        ipos=ipos+1
        go to 60
 80     continue

        xp=x0+ipos*dx
        xm=xp-dx
        if(xp.le.xm) then
          iflag=3
          return
        end if

        if(xver.lt.x1-eps) then
         if(ieks.gt.0)yb(k)=lval
        else
         if(xver.gt.x2+eps) then
           if(ieks.gt.0)yb(k)=rval
         else
           wg=(xver-xm)/(xp-xm)
       
           yb(k)=(1.0-wg)*ys(ipos)+wg*ys(ipos+1)
         end if
        end if

 100  continue


      
      return

      end 

******************************************************************
*
*                   FNUREGLIN
*
*     A one-D dataset given on a non-uniform grid is interpolated onto
*     a uniform grid by linear interpolation. The source grid must 
*     contain coordinates in increasing order.
*     parameters:
*             xs, ys -  arrays for original grid and data               I
*                       The xs must increase monotonously
*             na - number of points in original grid
*             yb - array for interpolated values on regular grid          O
*             nb,dxb,xb - number of points, grid increment and position  I
*                         of first point in regular grid 
*             ieks       - If the regular grid exeeds the original
*                         grid this parameter defines the extrapolation. I
*                         ieks=0 : values of yb are left unchanged
*                         ieks=1 :  a(1) and a(n) is used for
*                            regular nodes that are outside the original grid.
*                         ieks >=2 : the values  bleft ( left of xs(1)) and
*                              bright are used 
*             bleft, bright - see above                                  I
*             eps  - tolerance for defining a point inside original grid   I
*             iflag - error parameter                                      O
***************************************************************************
      subroutine fnureglin(xs,ys,na,yb,nb,dxb,xb,ieks,bleft,bright,
     %                   eps,iflag)
      integer na,nb,ieks,iflag
      real xs(na),ys(na),yb(nb),xb,dxb,bleft,bright,eps
*------------------------------------------------------------------
      integer k,k1,k2,k1m,ipos
      real xver,x1,x2,lval,rval,xm,xp,wg

 
      iflag=0

      x1=xs(1)
      x2=xs(na)


      if(na.eq.1) then
        iflag=2
        return
      end if


      if(ieks.eq.1) then
       rval=ys(na)
       lval=ys(1)
      else
       rval=bright
       lval=bleft
      end if

      ipos=1

      do 100 k=1,nb
        xver=xb+(k-1)*dxb
 60     continue
        if(xver.le.xs(ipos+1) .or. ipos.eq.(na-1)) go to 80
        ipos=ipos+1
        go to 60
 80     continue

        xp=xs(ipos+1)
        xm=xs(ipos)
        if(xp.le.xm) then
          iflag=3
          return
        end if

        if(xver.lt.x1-eps) then
           if(ieks.gt.0)yb(k)=lval
        else
         if(xver.gt.x2+eps) then
           if(ieks.gt.0)yb(k)=rval
         else
           wg=(xver-xm)/(xp-xm)
       
           yb(k)=(1.0-wg)*ys(ipos)+wg*ys(ipos+1)
         end if
        end if

 100  continue


      
      return

      end 

********************************************************************
      subroutine lestwoc(itape,x,y,nmax,n,iflag)
      integer itape,nmax,n,iflag
      real x(nmax),y(nmax)
*------------------------------------------------------------------
      integer ierr,nskip
      real a,b
 
      iflag=0
        
      call skipkom(nskip,itape,ierr)
      if(ierr.gt.0) then
      write(0,*)'skipkom: nskip,ierr=',nskip,ierr
         iflag=2
         close(itape)
         return
      end if

      n=0

 100  continue

      read(itape,*,end=200)a,b
      n=n+1
      if(n.gt.nmax) then
        iflag=5
        close(itape)
        return
      end if
      x(n)=a
      y(n)=b

      go to 100

 200  continue

      return


      end

**********************************************************************
*
*     Checks if a grid is uniform
*     x -grid      I
*     n - no of points
*     dmin,dmax - min and max increment
*     eps - rel tolerence for uniformity
*     ir - =0 uniform, 1 nonuniform                              O 
************************************************************************
      subroutine unifgrid(x,n,dmin,dmax,eps,ir)
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


********************************************************************
*
*                REGPFIL
*                
*     Prompts for name and reads data from two-column file, then 
*     interpolates to a uniform grid
*
*     spor   - question                                                I
*     fnavn,defn  - Name on file and def value                         O
*     kl  -   lengtn of filename. If kl<=0 the name is assumed to be   O
*             ended by '#' or '!' and the length is calculated
*     x,y  -  arrays for data from file                                O
*     nmax  - maximum number of data                                   I
*     n  -    number of data read  from file                           O
*     nskip  -  number of leading comment lines in file                O
*     val - array for regular ouput-data                               O
*     x0,dx,nv - left value, step and num,ber of points in val          I
*     spline - if true then splines are used, when x is uniform         I
*     eks       - If the regular grid exeeds the original
*                         grid this parameter defines the extrapolation. I
*                         If eks is .true. a(1) and a(n) is used for
*                         regular nodes that are outside the original grid.
*                         Otherwise the values  bleft ( left of xs(1)) and
*                         bright are used 
*     bleft, bright - see above                                  I
*     eps  - tolerance for defining a point inside original grid   I
*     iflag  - error flag                                              O
*                0     : OK
*                1     : maximum number of data reached before end of file.
*                        The data that is read is returned in x and y
*               11     : file not opened
*                3     : error in comment processing ( no data ?)
*                30    : inappropriate filename 
*                50    : cannot open file
*     w - working array
********************************************************************
      subroutine regpfil(spor,fnavn,defn,kl,x,y,nmax,n,nskip,
     % val,x0,nv,dx,eks,bleft,bright,iflag,w)
      integer kl,nmax,n,nskip,nv,iflag
      real x(nmax),y(nmax),val(nmax),x0,dx,bleft,bright,eps
      real w(5*nmax)
      logical eks,spline
      character*80 spor,fnavn,defn
*------------------------------------------------------------------
      integer ir,irv,ierr
      real dmax,dmin,dxi
      logical spl

      call promptfil(spor,fnavn,defn,kl,x,y,nmax,n,nskip,iflag)
      if(iflag.gt.1) then
        return
      end if
 
      call unifgrid(x,n,dmin,dmax,eps,ir)
      spl=spline.and.ir.eq.0
      if(n.gt.1) then
       dxi=(x(n)-x(1))/(n-1)
      else
       dxi=1.0
      end if

      if(eks) then
       irv=0
      else
       irv=1
      end if

      if(spl) then 
        call gstaggeg(y,n,dxi,x(1),val,nv,x0,dx,irv,spline,
     %           bleft,bright,.false.,0.0,.false.,0.0,ierr,eps,w)
      else
         call ureglin(x,y,n,val,nv,dx,x0,eks,bleft,bright,
     %                   eps,ierr)
      end if

      if(ierr.gt.0) then
       iflag=100+ierr
      end if



      return
      end




********************************************************************
*               I N G R E R
*
*     Rutinen integrer en spline interpolant for et ekvidistant punktsett.
*   parametere:
*       y  -  y(j) inneholder integralet av f's spline interpolant.      O
*             Integrasjonskonstanten er tilpasset slik at y(inull)=0.0
*             Dette betyr at y(j)= integral av G(x) fra x=inull*dx til
*             x=j*dx, der G(x) er interpolanten. Merk  at j=1 er 
*             identifisert med x=dx og ikke x=0.
*       f  -  Punktsett som skal integreres                              I
*       n  -  antall punkter                                             I
*       dx -  avstand mellom punkter                                     I
*       der1,der2 - deriverte av interpolant i hhv x=dx og x=n*dx        I
*       inull - Definerer integrasjonskonstant som angitt ovenfor        I
*       wk    -   Arbeidsomr}de, minimum st\rrelse er angitt i rutinehode.
*
*****************************************************************************
      subroutine ingrer(y,f,n,dx,der1,der2,inull,wk)      
      integer n,inull
      real y(n),f(n),wk(5*n+2),dx,der1,der2
*------------------------------------------------
      integer i
      real  r1,r2,diff

      call bsplw(f,wk(1),n,dx,der1,der2,wk(n+3))

      r1=dx/24.0
      r2=dx*11.0/24.0

      y(1)=0.0

      do 100 i=2,n
      y(i)=y(i-1)+r1*(wk(i-1)+wk(i+2))+r2*(wk(i)+wk(i+1))
 100  continue

      diff=y(inull)

      do 200 i=1,n
      y(i)=y(i)-diff
 200  continue

      return
      end 



********************************************************************
*               G I N G R E R
*
*     Rutinen integrer en spline interpolant for et ekvidistant punktsett.
*   parametere:
*       y  -  y(j) inneholder integralet av f's spline interpolant.      O
*             Integrasjonskonstanten er tilpasset slik at y(inull)=0.0
*             Dette betyr at y(j)= integral av G(x) fra x=inull*dx til
*             x=j*dx, der G(x) er interpolanten. Merk  at j=1 er 
*             identifisert med x=dx og ikke x=0.
*       f  -  Punktsett som skal integreres                              I
*       n  -  antall punkter                                             I
*       dx -  avstand mellom punkter                                     I
*       der1,der2 - deriverte av interpolant i hhv x=dx og x=n*dx        I
*      ik1,ik2 type of boundary condition for left and right boundary   I
*         respectively.
*         value=0 : derivative adapted to value d1,d2
*         value=1 : one sided differences used for derivative
*         value=2 : second derivative set to d1 and d2.
*                   if hv(0)=0 and d1=0 we then obtain an antisymmetric
*                   condition at left boundary etc. 
*       inull - Definerer integrasjonskonstant som angitt ovenfor        I
*       wk    -   Arbeidsomr}de, minimum st\rrelse er angitt i rutinehode.
*
*****************************************************************************
      subroutine gingrer(y,f,n,dx,ik1,der1,ik2,der2,inull,wk)      
      integer n,ik1,ik2,inull
      real y(n),f(n),wk(5*n+2),dx,der1,der2
*------------------------------------------------
      integer i
      real  r1,r2,diff

      call bsplgen(f,wk(1),n,dx,ik1,der1,ik2,der2,wk(n+3))

      r1=dx/24.0
      r2=dx*11.0/24.0

      y(1)=0.0

      do 100 i=2,n
      y(i)=y(i-1)+r1*(wk(i-1)+wk(i+2))+r2*(wk(i)+wk(i+1))
 100  continue

      diff=y(inull)

      do 200 i=1,n
      y(i)=y(i)-diff
 200  continue

      return
      end 

*********************************************************************
      subroutine trapes(f,n,dx,sum)
      integer n
      real f(n),dx,sum
*--------------------------------------------------------
      integer i,nm

      sum=0.5*(f(1)+f(n))
      nm=n-1
      do 100 i=2,nm
       sum=sum+f(i)
 100  continue

      sum=sum*dx
      return
      end

************************************************************
      function midpu(a,b,n,f)
      integer n
      real a,b,f,midpu
      external f
*-------------------------------------------------------------
      integer i
      real dx,x,sum

      sum=0.0
      dx=(b-a)/n

      do 100 i=1,n
       x=(i-0.5)*dx+a
       sum=sum+f(x)
 100  continue
      midpu=dx*sum

      return
      end


************************************************************
      function simps(a,b,n,f)
      integer n
      real a,b,f,simps
      external f
*-------------------------------------------------------------
      integer i,j,nm
      real dx,x,suml,sumo

      dx=(b-a)/(2.0*n)
      nm=n-1
      suml=0.0
      sumo=0.0

      do 100 i=1,n
       j=2*i-1
       x=a+j*dx
       sumo=sumo+f(x)
 100  continue
      
      do 200 i=1,nm
       j=2*i
       x=a+j*dx
       suml=suml+f(x)
 200  continue

      simps=dx*(f(a)+f(b)+4.0*sumo+2.0*suml)/3.0


      return
      end

************************************************************
      subroutine regitab(res,n,dx,x0,dstol,f,rmet)
      integer n
      real res(n),dx,x0,dstol,f,rmet
      external f,rmet
*-----------------------------------------------------------
      integer i,ni,nm
      real a,b

      nm=n-1
      ni=dx/dstol
      if(ni*dstol.lt.dx) ni=ni+1

      b=x0
      do 100 i=1,nm
        a=b
        b=b+dx
        res(i+1)=res(i)+rmet(a,b,ni,f)
 100  continue


      end 


************************************************************
      subroutine varitab(res,x,n,dstol,f,rmet)
      integer n
      real res(n),x(n),dstol,f,rmet
      external f,rmet
*-----------------------------------------------------------
      integer i,ni,nm
      real a,b,dx

      nm=n-1


      b=x(1)

      do 100 i=1,nm
        a=b
        b=x(i+1)
        dx=b-a
        ni=dx/dstol
        if(ni*dstol.lt.dx) ni=ni+1
        res(i+1)=res(i)+rmet(a,b,ni,f)
 100  continue


      end 

*********************************************************************
      subroutine gtrapes(y,f,n,dx)
      integer n
      real y(n),f(n),dx
*--------------------------------------------------------
      integer i
      real ym,yp,dx05

      dx05=dx*0.5
      
      yp=y(1)
      f(1)=0

      do 100 i=2,n
       ym=yp
       yp=y(i)
       f(i)=f(i-1)+dx05*(yp+ym)
 100  continue

      return
      end


*********************************************************************
      subroutine ktrapes(y,f,x,n)
      integer n
      real y(n),f(n),x(n)
*--------------------------------------------------------
      integer i
      real ym,yp,xm,xp

      yp=y(1)
      xp=x(1)
      f(1)=0

      do 100 i=2,n
       ym=yp
       yp=y(i)
       xm=xp
       xp=x(i)
       f(i)=f(i-1)+(xp-xm)*(yp+ym)
 100  continue

      return
      end




***********************************************************************
* Finds minum and maximum of a(n)
*
***********************************************************************
      subroutine ekstrem(a,n,amin,amax)
      integer n
      real a(n),amax,amin
*------------------------------------------------------
      integer i

      amax=a(1)
      amin=a(1)

      do 100 i=2,n
      if(a(i).gt.amax) amax=a(i)
      if(a(i).lt.amin) amin=a(i)
 100  continue

      return
      end

***********************************************************************
* Finds minum and maximum of a(n)
*
***********************************************************************
      subroutine gekstrem(a,n,imin,amin,imax,amax)
      integer n,imin,imax
      real a(n),amax,amin
*------------------------------------------------------
      integer i

      amax=a(1)
      amin=a(1)
      imin=1
      imax=1
      do 100 i=2,n
      if(a(i).gt.amax) then
         amax=a(i)
         imax=i
      end if

      if(a(i).lt.amin) then
        amin=a(i)
        imin=i
      end if
 100  continue

      return
      end


*************************************************************
      subroutine recol(v,nc,nmax,n,itape,iflag)
      integer nc,nmax,n,itape,iflag
      real v(nmax,nc)
*-----------------------------------------------------------
      integer j
      real buff(50)

      if(nc.gt.50) then
        iflag=2
        return
      end if

      iflag=0
      n=0

 100  continue
      n=n+1
      read(itape,*,end=200) (buff(j), j=1,nc)
      if(n.gt.nmax) then
        iflag=1
        n=n-1
        return
      end if

      do 120 j=1,nc
      v(n,j)=buff(j)
 120  continue

      go to 100

 200  continue
      n=n-1
      return
      

      end

************************************************************************
      function av(y,n,na)
      integer n,na
      real y(n),av
*---------------------------------------------------------------------
      integer nn,i
      real sum

      nn=min(n,na)
      sum=0.0

      do 100 i=1,nn
      sum=sum+y(i)
 100  continue

      av=sum/nn

      return
      end


************************************************************************
      SUBROUTINE DEL(A,B,N,M,NB,MB,N0,M0,N1,M1,IX,IY)
      INTEGER N,M,NB,MB,N0,M0,N1,M1,IX,IY
      REAL A(n*m),B(n*m)
*----------------------------------------------------------
      INTEGER JSTART,KST,K,I

      NB=(N1-N0)/IX +1
      MB=(M1-M0)/IY +1
      JSTART=(M0-1)*N+N0

      DO 100 K=1,MB
      KST=(K-1)*NB
      DO 50 I=1,NB
   50 B(KST+I)=A(JSTART+(I-1)*IX)
      JSTART=JSTART+IY*N
  100 CONTINUE
      RETURN
      END




*******************************************************************
*
*              PART
*
*         Klipper ut en del av en array
*      parametere:
*             U - matrise det skal velges fra                          I
*             Y - resultatmatrise                                      O
*             NU,MU,DXU,DYU,XU,YU - gitterpunkter, avstander           I
*                      og startpunkt for U
*             NY,MY,DXY,DYY,X1,Y1 - gitterpunkter, avstander           O
*                      og startpunkt for Y
***************************************************************************
      SUBROUTINE PART(U,Y,NU,MU,DXU,DYU,XU,YU,NY,MY,DXY,DYY,X1,Y1)
      INTEGER NU,MU,NY,MY
      REAL U(1),Y(1),XU,YU,X1,Y1,DXU,DYU,DXY,DYY
*----------------------------------------------------------------
      INTEGER N0,M0,N1,M1,IX,IY
      CHARACTER C

  50  CONTINUE
      WRITE(0,*)'N=',NU,'    M=',MU
      call lesi4(1,1,nu,mu,'GI N. VEN. OG O. HO. HJOERNE#',N0,M0,N1,M1)

      IF(N0.Gt.N1 .OR. M0.Gt.M1) THEN
        WRITE(0,*)'ULOVLIGE DATA:',n0,m0,n1,m1
        GO TO 50
      END IF

      IF(N0.LE.0) N0=1
      IF(N1.GT.NU) N1=NU
      IF(M0.LE.0) M0=1
      IF(M1.GT.MU) M1=MU

      WRITE(0,*)'N0,M0=',N0,' , ',M0,'   N1,M1=',N1,' , ',M1
      call lesi2(1,1,' GI TETTHET I X OG Y-RETNING#',ix,iy)
      IF(IX.LT.1) IX=1
      IF(IY.LT.1) IY=1

      CALL DEL(U,Y,NU,MU,NY,MY,N0,M0,N1,M1,IX,IY)
      X1=XU+(N0-1)*DXU
      Y1=YU+(M0-1)*DYU
      DXY=IX*DXU
      DYY=IY*DYU

      RETURN
      END


*******************************************************************
*
*              DPART
*
*         Leser klipp-data for en array
*      parametere:
*             NU,MU - gitterpunkter, avstander           I
*                      og startpunkt for oppr. matrise
*             NY,MY,IX,IY,NO,MO - gitterpunkter, avstander           O
*                      og startpunkt for ny matrise
***************************************************************************
      SUBROUTINE DPART(NU,MU,NY,MY,IX,IY,N0,M0)
      INTEGER NU,MU,NY,MY,N0,M0,IX,IY
*----------------------------------------------------------------
      INTEGER N1,M1
      CHARACTER C

  50  CONTINUE
      WRITE(0,*)'N=',NU,'    M=',MU
      call lesi4(1,1,nu,mu,'GI N. VEN. OG O. HO. HJOERNE#',N0,M0,N1,M1)

      IF(N0.GT.N1 .OR. M0.GT.M1) THEN
        WRITE(0,*)'ULOVLIGE DATA:',n0,m0,n1,m1
        GO TO 50
      END IF

      IF(N0.LE.0) N0=1
      IF(N1.GT.NU) N1=NU
      IF(M0.LE.0) M0=1
      IF(M1.GT.MU) M1=MU

      WRITE(0,*)'N0,M0=',N0,' , ',M0,'   N1,M1=',N1,' , ',M1
      call lesi2(1,1,' GI TETTHET I X OG Y-RETNING#',ix,iy)
      IF(IX.LT.1) IX=1
      IF(IY.LT.1) IY=1

      ny= 1+(n1-n0)/ix
      my= 1 +(m1-m0)/iy


      RETURN
      END

          






*************************************************************************
*
*                 TRANPK
*
*       speiler og transponerer i denne rekkefoelge.
*      parametere:
*          Y - datarray     ved input en tett nxm, ved output      I/O
*                     tett mxn dersom transponering
*          n,m - arraystorrelsere ( endres ikke pg trans)          I
*          xspeil,yspeil,transp - angir operasjoner                I
*          W - arbeidsarray av samme storrelse som Y               D
*
************************************************************************
      SUBROUTINE TRANPK(Y,N,M,xspeil,yspeil,transp,W)
      INTEGER N,M
      REAL Y(N*M),W(N*M)
      logical xspeil,yspeil,transp
*---------------------------------------------
      REAL TEMP
      INTEGER I,K,NGM,NTEMP,MH,NH

      NGM=N*M

      IF(xspeil)THEN
        MH=M/2
        DO 200 I=1,N
        DO 200 K=1,MH
        TEMP=Y(I+(K-1)*N)
        Y(I+(K-1)*N)=Y(I+(M-K)*N)
        Y(I+(M-K)*N)=TEMP
  200   CONTINUE
      end if

      IF(yspeil)THEN
        NH=N/2
        DO 300 K=1,M
        DO 300 I=1,NH
        TEMP=Y(I+(K-1)*N)
        Y(I+(K-1)*N)=Y(N+1-I +(K-1)*N)
        Y(N+1-I+(K-1)*N)=TEMP
  300   CONTINUE
      end if 

      if(transp) then
        DO 500 K=1,M
        DO 500 I=1,N
         w(K+(I-1)*M)=Y(I+(K-1)*N)
  500   CONTINUE
        DO 600 I=1,NGM
  600   Y(I)=W(I)
      end if

      END

********************************************************************
*          SMGEN
*
*     Performs 3 or 5 points smoothing of dataset
*
*     parameters
*       f   --  array with field tobe smoothened           I/O
*       n   --  number of points                           I
*       iord -- value =1: 3 point, else 5 point             I
*       sgn1, sgn2  -- govern boundary treatment                    I
*                    = -1 antisymmetric boundary assumed
*                    = 0  one sided smoothing formulas used
*                    = 1  symmetric boundary assumed   
*       bet  -- relaxation parameter  (value=1 full smoothing)      I
*       ierr -- error parameter (0: OK, 1: to few points)           O
***************************************************************************
      subroutine smgen(f,n,iord,sgn1,sgn2,bet,ierr)
      integer n,iord,sgn1,sgn2,ierr
      real f(n),bet
*--------------------------------------------------------------------

      if(iord.eq.1)  call smgen3(f,n,sgn1,sgn2,bet,ierr)
      if(iord.eq.2) call  smgen5(f,n,sgn1,sgn2,bet,ierr)
    
      return
      end


********************************************************************
*          SMGEN5
*
*     Performs 5 points smoothing of dataset
*
*     parameters
*       f   --  array with field to be smoothened           I/O
*       n   --  number of points                           I
*       sgn1, sgn2  -- govern boundary treatment                    I
*                    = -1 antisymmetric boundary assumed
*                    = 0  one sided smoothing formulas used
*                    = 1  symmetric boundary assumed   
*                    = 2  symmetric boundary at intermediate grid-cite 
*       bet  -- relaxation parameter  (value=1 full smoothing)      I
*       ierr -- error parameter (0: OK, 1: too few points)           O
***************************************************************************
      subroutine smgen5(f,n,sgn1,sgn2,bet,ierr)
      integer n,sgn1,sgn2,ierr
      real f(n),bet
*--------------------------------------------------------------------
      integer j,nm2
      real a0,ap,app,f1,f2,fn,fnm,fmm,fm,f0,fp,fpp

      if(n.lt.5) then
        ierr=1
        return
      else
        ierr=0
      end if

      nm2=n-2


      app=-1.0/16.0
      ap=0.25
      a0=5.0/8.0




      if(sgn2.eq.0) then
        fn=15.0*f(n)/16.0+0.25*f(n-1)-3.0*f(n-2)/8.0
     %     +0.25*f(n-3)-f(n-4)/16.0
        fnm=f(n)/16.0+0.75*f(n-1)+3.0*f(n-2)/8.0
     %     -0.25*f(n-3)+f(n-4)/16.0
      else
        if(sgn2.eq.2) then
          fp=f(n)
          fpp=f(n-1)
        else
          fp=sgn2*f(n-1)+(1-sgn2)*f(n)
          fpp=sgn2*f(n-2)+(1-sgn2)*f(n)
        end if
        fn=a0*f(n)+ap*(f(n-1)+fp)+app*(f(n-2)+fpp)
        fnm=a0*f(n-1)+ap*(f(n-2)+f(n))+app*(f(n-3)+fp)
      end if
 
     
      if(sgn1.eq.0) then
        f1=15.0*f(1)/16.0+0.25*f(2)-3.0*f(3)/8.0+
     %     0.25*f(4)-f(5)/16.0
        f2=f(1)/16.0+0.75*f(2)+3.0*f(3)/8.0
     %     -0.25*f(4)+f(5)/16.0
      else
        if(sgn1.eq.2) then
          fm=f(1)
          fmm=f(2)
        else
          fm=sgn1*f(2)+(1-sgn1)*f(1)
          fmm=sgn1*f(3)+(1-sgn1)*f(1)
        end if
        f1=a0*f(1)+ap*(f(2)+fm)+app*(f(3)+fmm)
        f2=a0*f(2)+ap*(f(1)+f(3))+app*(f(4)+fm)
      end if
 

      fm=f(1)
      f0=f(2)
      fp=f(3)
      fpp=f(4)

      do 200 j=3,nm2
        fmm=fm
        fm=f0
        f0=fp
        fp=fpp
        fpp=f(j+2)

        f(j)=(1.0-bet)*f(j)+bet*(a0*f0+ap*(fm+fp)+app*(fpp+fmm))
 200  continue

      f(1)=(1.0-bet)*f(1)+bet*f1
      f(2)=(1.0-bet)*f(2)+bet*f2
      f(n-1)=(1.0-bet)*f(n-1)+bet*fnm
      f(n)=(1.0-bet)*f(n)+bet*fn
      return
      end


********************************************************************
*          SMGEN3
*
*     Performs 3 points smoothing of dataset
*
*     parameters
*       f   --  array with field tobe smoothened           I/O
*       n   --  number of points                           I
*       sgn1, sgn2  -- govern boundary treatment                    I
*                    = -1 antisymmetric boundary assumed
*                    = 0  one sided smoothing formulas used
*                    = 1  symmetric boundary assumed   
*       bet  -- relaxation parameter  (value=1 full smoothing)      I
*       ierr -- error parameter (0: OK, 1: to few points)           O
***************************************************************************
      subroutine smgen3(f,n,sgn1,sgn2,bet,ierr)
      integer n,sgn1,sgn2,ierr
      real f(n),bet
*--------------------------------------------------------------------
      integer j,nm
      real a0,ap,f1,fn,fm,f0,fp

      if(n.lt.3) then
        ierr=1
        return
      else
        ierr=0
      end if

      nm=n-1



      ap=0.25
      a0=0.5

      if(sgn2.eq.0) then
        fn=0.75*f(n)+0.5*f(n-1)-0.25*f(n-2)
      else
        if(sgn2.eq.2) then
          fp=f(n)
        else
          fp=sgn2*f(n-1)+(1-sgn2)*f(n)
        end if
        fn=a0*f(n)+ap*(f(n-1)+fp)
      end if
 
     
      if(sgn1.eq.0) then
        f1=0.75*f(1)+0.5*f(2)-0.25*f(3)
      else
        if(sgn1.eq.2) then
          fm=f(1)
        else
          fm=sgn1*f(2)+(1-sgn1)*f(1)
        end if
        f1=a0*f(1)+ap*(f(2)+fm)
       end if
 

  
      f0=f(1)
      fp=f(2)

      do 200 j=2,nm

        fm=f0
        f0=fp
        fp=f(j+1)

        f(j)=(1.0-bet)*f(j)+bet*(a0*f0+ap*(fm+fp))
 200  continue

      f(1)=(1.0-bet)*f(1)+bet*f1
      f(n)=(1.0-bet)*f(n)+bet*fn
      return
      end




**********************************************************************
      subroutine lagkopi(a,b,nz)
      integer nz
      real a(nz),b(nz)
*---------------------------------------------------------------------
      integer i

      do 100 i=1,nz
       b(i)=a(i)
 100   continue

      return
      end

**********************************************************************
      subroutine affine(x,nz,a,b)
      integer nz
      real x(nz),a,b
*---------------------------------------------------------------------
      integer i

      do 100 i=1,nz
       x(i)=a+b*x(i)
 100   continue

      return
      end


**********************************************************************
      subroutine nullinit(a,na,nz)
      integer na,nz
      real a(na:nz)
*---------------------------------------------------------------------
      integer i

      do 100 i=na,nz
       a(i)=0
 100   continue

      return
      end

**********************************************************************
      subroutine addab(a,b,c,na,nz,wa,wb)
      integer na,nz
      real a(na:nz),b(na:nz),c(na:nz),wa,wb
*---------------------------------------------------------------------
      integer i

      do 100 i=na,nz
       c(i)=wb*b(i)+wa*a(i)
 100   continue

      return
      end

**********************************************************************
      subroutine leggbtoa(a,b,na,nz,wa,wb)
      integer na,nz
      real a(na:nz),b(na:nz),wa,wb
*---------------------------------------------------------------------
      integer i

      do 100 i=na,nz
       a(i)=wb*b(i)+wa*a(i)
 100   continue

      return
      end


******************************************************************
*
*                   RINTLIN
*
*     A one-D dataset on a uniform grid is interpolated at a given point
*             ys -  array  for original    data               I
*             n,dx,xa - number of points, grid increment and position  I
*                         of first point for grid
*             xb -        position  to be interpolated                   I
*             yb -        interpolated value
*             eps  - tolerance for defining a point inside original grid   I
*             iflag - error parameter                                      O
*                      value 0 : OK, 1 xb outside grid                
***************************************************************************
      subroutine rintlin(ys,n,dx,xa,xb,yb,eps,iflag)
      integer n,iflag
      real ys(n),xa,dx,xb,yb,eps
*------------------------------------------------------------------
      integer ipos
      real r,w



 
      iflag=0

cc    find ipos such that xb is between point ipos and ipos+1
      
      ipos=(xb-xa)/dx +1

cc    Special case 1
      if(ipos.eq.0 .and.(xa-xb).gt.eps)ipos=1
cc    Special case 2
      if(ipos.eq.n .and.(xb-xa-(n-1)*dx).lt.eps)ipos=n-1

cc    r is sistance from point ipos
      r=xb-xa-(ipos-1)*dx

      w=r/dx
      yb=w*ys(ipos+1)+(1.0-w)*ys(ipos)

      
      return

      end 


***********************************************************************
      subroutine linspace(xa,xb,n,x)
      integer n
      real xa,xb,x(n)
*----------------------------------------------------------------------
      integer i
      real dx

      if(n.lt.0) return
      if(n.eq.1) then
        x(1)=xa
        return
      end if

      dx=(xb-xa)/(n-1)

      do 100 i=1,n
        x(i)=xa+(i-1.0)*dx
 100  continue
 
      return
      end


***********************************************************************
      subroutine lingitt(x0,dx,n,x)
      integer n
      real x0,dx,x(n)
*----------------------------------------------------------------------
      integer i
 

       do 100 i=1,n
        x(i)=x0+(i-1.0)*dx
 100  continue
 
      return
      end

************************************************************************
*
*     Checks wether r not agrid is uniform
*
*     x - grid array                                        I
*     n - number of points                                  I
*     dx - averaged increment                               O
*     tol - if increments vary less than tol*dx the grid    I
*           is acepted as uniform
*     unif - true if grid is uniform                        O
****************************************************************************
      subroutine klassdat(x,n,dx,tol,unif)
      integer n
      real x(n),dx,tol
      logical unif
*---------------------------------------------------------------
      integer i,imiss
      real dev,dev2,tol2
      if(n.eq.1) then
        dx=0
        unif=.true.
        return
      end if
      tol2=tol*tol*dx*dx
      dx=(x(n)-x(1))/(n-1.0)
      imiss=0

      do 100 i=2,n
       dev=x(i)-x(i-1)-dx
       dev2=dev*dev
       if(dev2.gt.tol2) imiss=imiss+1
 100  continue
      unif=imiss.eq.0
      return
      end
