


**************************************************************
      SUBROUTINE KVAIN(YM,Y0,YP,EM,XM,IER) 
      REAL YM,Y0,YP,EM,XM               
      INTEGER IER
*--------------------------------------------------------------
      REAL A,B
               
      IER=0
      A     = 0.5*(YP-YM)
      B     = 0.5*(YP+YM-2.0*Y0)
      IF(B.EQ. 0.0) THEN
         IER=1
      ELSE
         XM =-0.5*A/B
         EM   = Y0+A*XM+B*XM*XM
         IF( (EM-Y0) .GT. 1.5*MAX(Y0-YM,Y0-YP)) IER=2
      END IF

      RETURN
      END




******************************************************************************
      SUBROUTINE GLMAX(Y,YINP,NS,N,K,GR,AMP,XA,NA)
      INTEGER NS,N,K,NA
      REAL Y(0:NS,0:K),YINP(0:N+1),XA(20),AMP(20),GR
*----------------------------------------------------------------------------
      INTEGER NP2,J

      NP2=N+1

      CALL LIMAX(Y(0,K),YINP,NP2,GR,AMP,XA,NA)                          
                                                                       
      DO 100 J=1,NA
  100 XA(J)=XA(J)-1.0

      RETURN
      END




******************************************************************************
*                                                                            *
*             L I M A X                                                      *
*                                                                            *
*     MAKSIMALPUNKTER FOR SPLINE-INTERPOLANTEN TIL ET TALLSETT FINNES.       *
*     PARAMETERE:                                                            *
*            YR - YR(1)...YR(N) UTGJOER TALLSETTET.            I             *
*            YINP - YINP(0)...YINP(N+1) ER SPLINE KOEFF.       O             *
*            N - ANTALL PUNKTER.                               I             *
*            GR - MAKSP. LOKALISERES I ET INTERVALL (K,K+1)                  *
*                 BARE NAAR YR(K) EL. YR(K+1)>GR               I             *
*            AMP ,XA - MAKSVERDIER AMP(1)...AMP(NA) ER FUNNET                *
*                 I PUNKTER XA(1)<XA(2)<...XA(NA). KOORDINAT-                *
*                 AKSEN ER SLIK AT YR(K) ER VERDIEN I K.       O             *
*            NA - ANTALL LOKALISERTE MAKSIMA. MAKSIMALT                      *
*                   ANTALL ER 20.                              O             *
*                                                                            *
*     RUTINEN BENYTTER "BSPL","SPB","KVAIN","TRI"                            *
******************************************************************************

      SUBROUTINE LIMAX(YR,YINP,N,GR,AMP,XA,NA)                               
      IMPLICIT LOGICAL (A-Z)
      INTEGER N,NA
      REAL YR(N),YINP(0:N+1),XA(20),AMP(20),GR
*-------------------------------------------------
      REAL DER1,DER2,QPP,QP,QM,QMM,Y0,YM,YP,XM
      REAL YMAX
      INTEGER I,NM ,IER,IER1,IM

      NA=0
      NM=N-1

      DER1=0.5*(4.0*YR(2)-3.0*YR(1)-YR(3))
      DER2=-0.5*(4.0*YR(N-1)-3.0*YR(N) -YR(N-2))
      CALL BSPL(YR,YINP,N,1.0,DER1,DER2)

      Y0=YR(1)
      YP=YR(2)

      DO 200 I=2,NM
      YM=Y0
      Y0=YP
      YP=YR(I+1)
      IF( Y0.GE.YP .AND. Y0.GT.YM .AND. Y0.GT.GR) THEN
        NA=NA+1
        IF(YM.GT.YP) THEN
          IM=I-1
        ELSE
          IM=I
        END IF
        QMM=YINP(IM-1)
        QM=YINP(IM)
        QP=YINP(IM+1)
        QPP=YINP(IM+2)
        CALL SPB(QMM,QM,QP,QPP,XM,YMAX,IER)
        IF(IER.EQ.0) THEN
          XA(NA)= IM+XM
          AMP(NA)= YMAX
        else
          IM=2*I-1-IM
          QMM=YINP(IM-1)
          QM=YINP(IM)
          QP=YINP(IM+1)
          QPP=YINP(IM+2)
          IER1=IER
          CALL SPB(QMM,QM,QP,QPP,XM,YMAX,IER)
          IF(IER.EQ.0) THEN
            XA(NA)= IM+XM
            AMP(NA)= YMAX
          else                             
            CALL KVAIN(YM,Y0,YP,YMAX,XM,IER)    
            IF(IER.EQ.0) THEN
               XA(NA)=I+XM
               AMP(NA)=YMAX
            ELSE
               XA(NA)=I
               AMP(NA)=Y0
            end if
          END IF
        END IF
C
COMMENT: MAKSIMALT TILLATT ANTALL MAXIMA ER SATT LIK 20
C
      IF(NA.EQ.20)RETURN
      END IF
  200 CONTINUE
      RETURN
      END



******************************************************************************
*                                                                            *
*             N E W M A X                                                    *
*                                                                            *
*     MAKS(MIN)IMALPUNKTER FOR SPLINE-INTERPOLANTEN TIL ET TALLSETT FINNES.
*
*     PARAMETERE:                                                            *
*            YR - YR(1)...YR(N) UTGJOER TALLSETTET.            I             *
*            YINP - YINP(0)...YINP(N+1) ER SPLINE KOEFF.       O             *
*            N - ANTALL PUNKTER.                               I             *
*            x0 - posisjon av punkt 1
*            dx - gitteravstand
*            ivis - type maks                                  I
*                 <=1    fra venstre, posetive ekstrema
*                  =2    fra hoyre, posetive ekstrema
*                  =3    fra venstre, negative ekstrema
*                  =4    fra hoyre, negative ekstrema
*            GR - MAKSP. LOKALISERES I ET INTERVALL (K,K+1)                  *
*                 BARE NAAR YR(K) EL. YR(K+1)>GR               I
*                 (dersom ivis >2 YR(k) <-GR)                                *
*            AMP ,XA - MAKSVERDIER AMP(1)...AMP(NA) ER FUNNET                *
*                 I PUNKTER XA(1)<XA(2)<...XA(NA). KOORDINAT-                *
*                 AKSEN ER SLIK AT YR(K) ER VERDIEN I K.       O             *
*            namax - maximum number of max-points
*            NA - ANTALL LOKALISERTE MAKSIMA. MAKSIMALT                      *
*                   ANTALL ER 20.                              O            
*            w -arbeidsarray                                                 *
*                                                                            *
*     RUTINEN BENYTTER "BSPL","SPB","KVAIN","TRI"                            *
******************************************************************************

      SUBROUTINE NEWMAX(YR,YINP,N,x0,dx,ivis,GR,AMP,XA,namax,NA,w) 
      INTEGER N,ivis,namax,NA
      REAL YR(N),YINP(0:N+1),x0,dx,XA(namax),AMP(namax),GR,w(5*(n+2))
*-------------------------------------------------
      REAL DER1,DER2,QPP,QP,QM,QMM,Y0,YM,YP,XM
      REAL YMAX,xend,steg
      INTEGER I,NM ,IER,IER1,IM,nw,isgn

      
      if(ivis.le.2) then
        isgn=1
      else
        isgn=-1
      end if

      if(ivis.eq.1 .or. ivis.eq.3) then
        xend=x0
        steg=dx
        do 100 i=1,n
          w(i)=isgn*yr(i)
 100    continue
      else
        xend=x0+(n-1)*dx
        steg=-dx
        do 150 i=1,n
          w(i)=isgn*yr(n+1-i)
 150    continue
      end if
   

      nw=n+1
      NA=0
      NM=N-1

      DER1=0.5*(4.0*W(2)-3.0*W(1)-W(3))
      DER2=-0.5*(4.0*W(N-1)-3.0*W(N) -W(N-2))
      CALL bsplw(W,YINP,N,1.0,DER1,DER2,w(nw))

      Y0=W(1)
      YP=W(2)

      DO 200 I=2,NM
      YM=Y0
      Y0=YP
      YP=W(I+1)
      IF( Y0.GE.YP .AND. Y0.GT.YM .AND. Y0.GT.GR) THEN
        NA=NA+1
        IF(YM.GT.YP) THEN
          IM=I-1
        ELSE
          IM=I
        END IF
        QMM=YINP(IM-1)
        QM=YINP(IM)
        QP=YINP(IM+1)
        QPP=YINP(IM+2)
        CALL SPB(QMM,QM,QP,QPP,XM,YMAX,IER)
        IF(IER.EQ.0) THEN
          XA(NA)= IM+XM
          AMP(NA)= YMAX
        else
          IM=2*I-1-IM
          QMM=YINP(IM-1)
          QM=YINP(IM)
          QP=YINP(IM+1)
          QPP=YINP(IM+2)
          IER1=IER
          CALL SPB(QMM,QM,QP,QPP,XM,YMAX,IER)
          IF(IER.EQ.0) THEN
            XA(NA)= IM+XM
            AMP(NA)= YMAX
          else                             
            CALL KVAIN(YM,Y0,YP,YMAX,XM,IER)    
            IF(IER.EQ.0) THEN
               XA(NA)=I+XM
               AMP(NA)=YMAX
            ELSE
               XA(NA)=I
               AMP(NA)=Y0
            end if
          END IF
        END IF
        xa(na)=xend+(xa(na)-1)*steg
        amp(na)=isgn*amp(na)
C
COMMENT: MAKSIMALT TILLATT ANTALL MAXIMA ER SATT LIK namax
C
        IF(NA.EQ.namax)return
      END IF
  200 CONTINUE
      
      RETURN
      END




*****************************************************************
      SUBROUTINE SPB(QMM,QM,QP,QPP,XM,YMAX,IER)
      IMPLICIT LOGICAL(A-Z)
      REAL QMM,QM,QP,QPP,XM,YMAX
      INTEGER IER
*--------------------------------------------------------------
      REAL A,B,C,D,DISK,S1,S2,GR,AS1,AS2,DTOL

      IER=0
      GR=(ABS(QP)+ABS(QMM))*1.0E-9

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     UTREGNING AV TOLERANSE FOR DISKRIMAND I DEN
C     DERIVERTE BASERER SEG PAA DIFFERANSE FOR
C     ANNEN DERIVERT.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      DTOL=QP-2.0*QM+QMM
      DTOL=0.001*DTOL*DTOL

      A=( QMM +4.0*QM +QP)/6.0
      B=0.5*( QP -QMM )
      C=0.5*( QMM -2*QM +QP )
      D=( QPP -3*QP +3*QM -QMM )/6.0

      IF(ABS(D).LT.GR) THEN
        IF(ABS(C).LT.GR) THEN
          IER=3
          RETURN
        END IF
        XM=-B*0.5/C
        IF(ABS(XM-0.5).GT.(0.5+GR)) THEN
          IER=4
          RETURN
        END IF
        YMAX=A+B*XM+C*XM*XM
        RETURN
      END IF

      DISK=C*C-3.0*B*D

      IF(DISK.LT.0) THEN
        IER=1
        RETURN
      END IF

      IF(DISK.LT.DTOL) THEN
        IER=2
        RETURN
      END IF

      S1=(-C+SQRT(DISK))/(3.0*D)
      S2=(-C-SQRT(DISK))/(3.0*D)
      AS1=ABS(S1-0.5)
      AS2=ABS(S2-0.5)

      IF(AS1.LT.AS2) THEN
        IF(AS1.GT.(0.5+GR)) THEN
          IER=-1
          RETURN
        END IF
        IF(AS2.LT.(0.5-GR)) THEN
          IER=-2
          RETURN
        END IF
        XM=S1
      ELSE
        IF(AS2.GT.(0.5+GR)) THEN
          IER=-1
          RETURN
        END IF
        IF(AS1.LT.(0.5-GR)) THEN
          IER=-2
          RETURN
        END IF
        XM=S2
      END IF

      YMAX=A +B*XM +C*XM*XM +D*XM*XM*XM

      RETURN
      END




******************************************************************************
*                                                                            *
*             L I M A W                                                      *
*                                                                            *
*     MAKSIMALPUNKTER FOR SPLINE-INTERPOLANTEN TIL ET TALLSETT FINNES.       *
*     PARAMETERE:                                                            *
*            YR - YR(1)...YR(N) UTGJOER TALLSETTET.            I             *
*            YINP - YINP(0)...YINP(N+1) ER SPLINE KOEFF.       O             *
*            N - ANTALL PUNKTER.                               I             *
*            GR - MAKSP. LOKALISERES I ET INTERVALL (K,K+1)                  *
*                 BARE NAAR YR(K) EL. YR(K+1)>GR               I             *
*            AMP ,XA - MAKSVERDIER AMP(1)...AMP(NA) ER FUNNET                *
*                 I PUNKTER XA(1)<XA(2)<...XA(NA). KOORDINAT-                *
*                 AKSEN ER SLIK AT YR(K) ER VERDIEN I K.       O             *
*            NA - ANTALL LOKALISERTE MAKSIMA. MAKSIMALT                      *
*                   ANTALL ER 20.                              O             *
*            WRK - arbeisdomraade                              D
*                                                                            *
*     RUTINEN BENYTTER "BSPL","SPB","KVAIN","TRI"                            *
******************************************************************************

      SUBROUTINE LIMAW(YR,YINP,N,GR,AMP,XA,NA,WRK)             
      INTEGER N,NA
      REAL YR(N),YINP(0:N+1),XA(20),AMP(20),GR,wrk(4*N)
*-------------------------------------------------
      REAL DER1,DER2,QPP,QP,QM,QMM,Y0,YM,YP,XM
      REAL YMAX,ein
      INTEGER I,NM ,IER,IER1,IM

      NA=0
      NM=N-1
      ein=1.0

      DER1=0.5*(4.0*YR(2)-3.0*YR(1)-YR(3))
      DER2=-0.5*(4.0*YR(N-1)-3.0*YR(N) -YR(N-2))
      CALL BSPLW(YR,YINP,N,ein,DER1,DER2,WRK)

      Y0=YR(1)
      YP=YR(2)

      DO 200 I=2,NM
      YM=Y0
      Y0=YP
      YP=YR(I+1)
      IF( Y0.GE.YP .AND. Y0.GT.YM .AND. Y0.GT.GR) THEN
        NA=NA+1
        IF(YM.GT.YP) THEN
          IM=I-1
        ELSE
          IM=I
        END IF
        QMM=YINP(IM-1)
        QM=YINP(IM)
        QP=YINP(IM+1)
        QPP=YINP(IM+2)
        CALL SPB(QMM,QM,QP,QPP,XM,YMAX,IER)
        IF(IER.EQ.0) THEN
          XA(NA)= IM+XM
          AMP(NA)= YMAX
        else
          IM=2*I-1-IM
          QMM=YINP(IM-1)
          QM=YINP(IM)
          QP=YINP(IM+1)
          QPP=YINP(IM+2)
          IER1=IER
          CALL SPB(QMM,QM,QP,QPP,XM,YMAX,IER)
          IF(IER.EQ.0) THEN
            XA(NA)= IM+XM
            AMP(NA)= YMAX
          else                             
            CALL KVAIN(YM,Y0,YP,YMAX,XM,IER)    
            IF(IER.EQ.0) THEN
               XA(NA)=I+XM
               AMP(NA)=YMAX
            ELSE
               XA(NA)=I
               AMP(NA)=Y0
            end if
          END IF
        END IF
C
COMMENT: MAKSIMALT TILLATT ANTALL MAXIMA ER SATT LIK 20
C
      IF(NA.EQ.20)RETURN
      END IF
  200 CONTINUE
      RETURN
      END




******************************************************************************
      SUBROUTINE GLMAW(Y,YINP,NS,N,K,GR,AMP,XA,NA,WRK)
      INTEGER NS,N,K,NA
      REAL Y(0:NS,0:K),YINP(0:N+1),XA(20),AMP(20),GR,wrk(4*n+4)
*----------------------------------------------------------------------------
      INTEGER NP2,J

      NP2=N+1

      CALL LIMAW(Y(0,K),YINP,NP2,GR,AMP,XA,NA,wrk)                          
                                                                       
      DO 100 J=1,NA
  100 XA(J)=XA(J)-1.0

      RETURN
      END



**************************************************************************
*
*                     B L I N 2 G
*   
*     Beregner funksjonsverdier av en bilinear interpolant. 
*      parametere:
*              X,y - koordinater                                        I
*              X0,y0 - posisjon tilh|rende B-spline med koeff. bs(1,1)  I
*              xd,dy - punktavstand                                      I
*              B - array med funksjonsverd (bi-lin koeff).         I
*              N,m - antall interpolasjonspunkter                       I
*              eps - justerings-grense, faller et punkt mindre enn      I
*                    eps*dx etc. utenfor, trekkes det inn.
*              fdef - verdi som settes dersom x,y er utenfor
*              f -  verdi av interpolant                                O
*****************************************************************************
      subroutine blin2g(X,y,X0,y0,Dx,dy,B,N,m,eps,fdef,f)   
      INTEGER N,m
      REAL B(N,m),X,Y,X0,Y0,Dx,dy,eps,fdef,f
*------------------------------------------------ 
      INTEGER NX,ny,nn,i,j,nxp,nyp
      REAL Sy,sx,xv,yv

                 

      XV=(X-X0)/DX+1.0                          
      NX=XV         
      SX=XV-NX      

      if(nx.le.0 .or. nx.gt.n) then
        if(nx.eq.0 .and. sx .gt.(1.0-eps)) then
           nx=1
           sx=0.0
        else
          if(nx.eq.n .and. sx .lt.eps) then
           nx=n-1
           sx=1
          else 
            write(0,*)'x-punkt utenfor interval i blin2g'
            f=fdef
            return
          end if
        end if
      end if

      YV=(Y-Y0)/DY+1.0                          
      NY=YV         
      SY=YV-NY       


      if(ny.le.0 .or. ny.gt.m) then
        if(ny.eq.0 .and. sy .gt.(1.0-eps)) then
           ny=1
           sy=0.0
        else
          if(ny.eq.m .and. sy .lt.eps) then
           ny=m-1
           sy=1
          else 
            write(0,*)'y-punkt utenfor interval i blin2g'
            f=0
            return
          end if
        end if
      end if



      nxp=nx+1
      nyp=ny+1
      f=(1.0-sx)*(1.0-sy)*b(nx,ny)+sx*(1.0-sy)*b(nxp,ny)
     %  +sx*sy*b(nxp,nyp)+(1.0-sx)*sy*b(nx,nyp)


      RETURN        
      END           

*******************************************************
*             BASJOIN
*
*     Computes the value of a normalized spline S(x) defined
*     on -1<x<1 according to
*       S(1)=f; S'(1)=fd; S''(1)=fdd;  S(-1)=S'(-1)=S''(-1)=0 
***************************************************************
      function basjoin(f,fd,fdd,x)
      real f,fd,fdd,x,basjoin
*-------------------------------------------------------
      real a0,a1,a2,xm,xp

      xm=x-1.0
      xp=x+1.0

      a0=f*0.125
      a1=fd*0.125-1.5*a0
      a2=fdd/16.0-1.5*a1-0.75*a0

      basjoin=xp*xp*xp*(a0+xm*(a1+xm*a2))
      return
      end


*******************************************************
*             SPJOIN
*
*     Computes the value of a spline P(x) defined
*     on x0-h<x<x0+h according to
*       P(x+h)=r; P'(x+h)=rd; P''(x+h)=rdd;  
*       P(x-h)=l; P'(x-h)=ld; P''(x-h)=ldd;  
***************************************************************
      function spjoin(l,ld,ldd,r,rd,rdd,h,x0,x)
      real r,rd,rdd,l,ld,ldd,h,x0,x,spjoin
*-------------------------------------------------------
      real f,fd,fdd,arg,left,right,rh,basjoin
      external basjoin

      rh=1.0/h
      f=l
      fd=-ld*h
      fdd=ldd*h*h
      arg=(x0-x)*rh
      left=basjoin(f,fd,fdd,arg)

      f=r
      fd=rd*h
      fdd=rdd*h*h
      arg=(x-x0)*rh
      right=basjoin(f,fd,fdd,arg)
 
      spjoin=left+right
      return
      end


*******************************************************
*             glstep
*
*     Computes the value of two shelves joined by a spline P(x), giving
*     a continuous second derivative
*        ya,yb - left and right function values
*        xa,xb - transition interval
***************************************************************
      function glstep(xa,xb,ya,yb,x)
      real ya,yb,xa,xb,x,glstep
*-------------------------------------------------------
      real xm,h,dn,spjoin
      external spjoin

      xm=0.5*(xa+xb)
      h=0.5*(xb-xa)
      dn=0.0

      if(x.lt.xa) then
        glstep=ya
      else
        if(x.lt.xb) then
          glstep=spjoin(ya,dn,dn,yb,dn,dn,h,xm,x)
        else
          glstep=yb
        end if
      end if

      return
      end

*******************************************************
*             smpolyg
*
*     Computes a smoothed polygon with sharp angles replaced by
*     splines with a continuous second derivative
*        xv,yv - x, y values at corners
*        lv - transition intervals for each point
*        n - number of points
*             The interval must not be overlapping
*        x -coordinate at which value is to be computed
*        ia,ib - interval to search for x 
*        ileft - 1...n-1  : OK. it is the position left of x 
*                <=0  : x to small
*                >n  : x to large
*        eps - toleranse for x being outside domain
***************************************************************
      function smpolyg(xv,yv,lv,n,x,ia,ib,ileft,eps)
      integer n,ia,ib,iflag
      real xv(n),yv(n),lv(n),x,smpolyg,eps
*-------------------------------------------------------
      integer i,ipos,ileft
      real spjoin
      real xa,xb,l,ld,ldd,r,rd,rdd,h,x0
      external spjoin


      if(x.le.xv(ia)) then
        if(xv(ia)-x.lt.eps) then
          ileft=ia
          smpolyg=yv(ia)
        else
          ileft=0
        end if
        return
      end if

      if(x.ge.xv(ib)) then
        if(x-xv(ib).lt.eps) then
          ileft=ib
          smpolyg=yv(ib)
        else
          ileft=2*ib
        end if
        return
      end if

      ipos=ia

 100  continue

      if(ipos.ge.n) then
        ileft=ipos
        return
      end if



      xa=xv(ipos)
      xb=xv(ipos+1)
      if(x.ge.xa .and. x.le.xb) go to 200
      ipos=ipos+1
      go to 100

 200  continue

      ileft=ipos
      
      if( (x.lt.xa+lv(ipos)).and.ipos.gt.1) then
       ipos=ipos     
      else
         if(x.gt.xb-lv(ipos+1).and.ipos.lt.(n-1)) then
           ipos=ipos+1
         else
          ld=(yv(ipos+1)-yv(ipos))/(xb-xa)
          smpolyg= yv(ipos)+ld*(x-xa)
          return
         end if
      end if

       
       h=lv(ipos)
       x0=xv(ipos)
       ld=(yv(ipos)-yv(ipos-1))/(xv(ipos)-xv(ipos-1))
       l=yv(ipos)-h*ld
       rd=(yv(ipos+1)-yv(ipos))/(xv(ipos+1)-xv(ipos))
       r=yv(ipos)+h*rd
       ldd=0.0
       rdd=0.0
       smpolyg=spjoin(l,ld,ldd,r,rd,rdd,h,x0,x)

      return
      end

****************************************************************************
*
*     Fill an array with values from smoothed polygon
*         y(n)  - array for  values                                      O
*         n  - array-grense                                                 I
*         dx - gitterinkrement (betydning bare nar noun=.false.)          I
*         x - posisjoner  av y -verdier, naar noun=.false.                  I
*             benyttes bare x(1) som er pos av y(1)
*         noun - .false. angir uniformt gitter                            I
*         undef - value set otside polygon
*         eps - tolerance
*******************************************************************
      subroutine smarrp(xv,yv,lv,nv,y,n,dx,x,noun,undef,eps)
      integer nv,n
      real xv(nv),yv(nv),lv(nv),y(n),x(n),dx,undef,eps
      logical noun
*--------------------------------------------------
      integer ia,ib,i,ileft,ifak
      real xx,dxeff,val,smpolyg
      external smpolyg

      if(noun) then
        dxeff=0.0
        ifak=1
      else
        dxeff=dx
        ifak=0
      end if

      ia=1

      do 100 i=1,n
       xx=x(1+(i-1)*ifak)+(i-1)*dxeff
       val=smpolyg(xv,yv,lv,nv,xx,ia,nv,ileft,eps)
       if(ileft.ge.1 .and.ileft.le.nv) then
        ia=ileft
        y(i)=val
       else
        y(i)=undef
       end if
 100  continue

      return
      end 
