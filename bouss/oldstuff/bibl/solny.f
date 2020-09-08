*******************************************************************
      subroutine setbhkorr(pluss)
      real pluss
      real add
      common/solhas/add
c     add er en forel|pig korreksjonsfaktor for aa leke med
c     solitonhastigheter
*--------------------------------------------------------
      add=pluss

      return
      end 

***************************************************************
*                                                             *
*  BEREGNER BOLGEHASTIGHETEN TIL SOLITONEN                    *
*  PARAMETER:  A =                                            *
*                                                             *
***************************************************************

      FUNCTION BHAST(A)
      REAL A,Bhast
ccc      real add
ccc      common/solhas/add
c     add er en forel|pig korreksjonsfaktor for aa leke med
c     solitonhastigheter, den er siden fjernet
*--------------------------------------------------------------
      REAL EPA

      if(a.gt.0.001) then
        EPA   = 1+A 
        BHAST = EPA*SQRT((EPA*LOG(EPA)-A)/
     %          (A*A*(A/3.0+0.5))) 
      else
        bhast=1.0+0.5*a
      end if

      RETURN
      END



************************************************************************
      function halvb(a,eps)
      real a,eps,halvb
*----------------------------------------------------------------------

      halvb=-log(0.25*eps)/sqrt(3.0*a)
      return
      end 


*****************************************************************************
*              B O U N
*     Beregner randverdier for en inkommende soliton.
*     parametere:
*             b - array for randverdier                                  O
*             ds - gitteravstand langs rand                              I
*             dn - gitteravstand normalt rand                            I
*             dt - tidssteg                                              I
*             k2 - det fylles verdier i b(0:k2)                          I
*             snut - posisjon av topp-punkt. snut = ds gir topp i b(1)   I
*                     etc. Topp kan godt ligge utenfor beregningsintervall.
*                     Dersom psi er lik 90 grader har snut tolkning
*                     som avstand fra rand til boelgetopp.
*             psi -  Vinkel mellom boelgetallsvektor og rand.             I
*                    psi er positiv naar btall peker i pos.
*                    normalretning og beregnes i grader
*             a -    amplitude
*             eps -  trunkeringskriterium, likevekt settes naar          I
*                    overfl. hevn. < eps*a
*             islag - type randbetingelse                                I
*                 =0  df/dn
*                 =1  f
*                 =2  dy/dn
*                 =3  y
*                  der f er potensial og y overflatehelning
*             ik - verdi lik 1 angir korreksjon                          I
*****************************************************************************
      subroutine boun(b,ds,dn,dt,k2,snut,psi,a,eps,islag,ik)   
      integer islag,ik,k2   
      real b(0:k2),ds,dn,dt,snut,psi,a,eps
*-----------------------------------------------------------------------------
      integer irand,j
      real cpsi,spsi,pi,fc,topp,c,ua,dinc

      pi=4.0*atan(1.0)
      cpsi=cos(pi*psi/180.0)
      spsi=sin(pi*psi/180.0)
   

      if(islag.eq.0) then
        fc=ik*(dt*dt-ds*ds*cpsi*cpsi)/24.0
        irand=2
      else
        if(islag.eq.1) then
          fc=ik*(dt*dt-dn*dn*spsi*spsi)/24.0
          irand=3
        else
          fc=0.0
          if(islag.eq.2) then
            irand=5
          else
            irand=1
          end if
        end if
      end if

      if(abs(abs(psi)-90.0).lt.0.001) then
        call soliprgen(b,0,0,ds,A,snut,C,UA,irand,fc,eps)   
        do 100 j=1,k2
        b(j)=b(0)
 100    continue
        return
      end if

      dinc=ds*cpsi
      topp=snut*cpsi
      call soliprgen(b,0,k2,Dinc,A,TOPP,C,UA,irand,fc,eps)   

      if(islag.eq.0 .or. islag.eq.2) then
        do 200 j=0,k2
        b(j)=b(j)*spsi
 200    continue
      end if

      return
      end

      
*****************************************************************************
*
*               S O L I P R G E N
*
*
*     Tabulerer verdier svarende til en soliton-form ved kall p} 'SOLITGEN',
*     men gj|r tre ting i tillegg:
*                 1. Trunkerer solitonen der h|yden er mindre  enn amp*eps
*                 2. fyller y med nuller utenfor trunkert omr}de.
*                 3. S|rger for  at soliton kalles med increment (dx) som
*                    aldri overstiger 1.0. Dette kan v}re viktig for
*                    konvergens av underliggende likningsl|sere.
*     Parameterene svarer til de for 'solitgen' untatt eps som alts} gir
*     trunkerings-kriteriet.
**************************************************************************
      subroutine soliprgen(Y,n0,N,DX,AMP,TOPP,C,UA,ik,fc,eps)   
      INTEGER n0,N,ik
      REAL Y(N0:N),DX,AMP,topp,C,UA,fc,eps
*---------------------------------------------------------------------------
      integer nf,i,na,nb,nrmax,nr
      parameter(nrmax=15000)
      real dxred,yr(nrmax),blen,txr,halvb

      do 50 i=n0,n
         y(i)=0.0
 50   continue

      if(amp.le.0.0) return

      blen=halvb(amp,eps)

      na= (topp-blen)/dx -1
      na=max(na,n0)
      nb=(topp+blen)/dx +1
      nb=min(nb,n)

      nf=dx+1
      dxred= dx/nf

      if(na.gt.nb) then
        if(ik.eq.3 .or. ik.eq.4) then
          txr=blen+2.0*dxred
          nr=2*txr/dxred+2
          call solitgen(Yr,1,Nr,DXred,AMP,TXR,C,UA,ik,fc,eps)   
          do 60 i=n0,n
          y(i)=yr(1)
 60       continue
        end if
        return
      end if

      nr=(nb-na)*nf+1
      txr=topp-na*dx+dxred

      do 70 i=1,nr
      yr(i)=0.0
 70   continue

      if(nr.gt.nrmax) then
        nr=nrmax
        nb=(nr-1)/nf + na
        write(0,*)'advarsel, for mange punkter i soliprod'
      end if


      call solitgen(Yr,1,Nr,DXred,AMP,TXR,C,UA,ik,fc,eps)   

      do 100 i=na,nb
       y(i)=yr((i-na)*nf+1)
 100   continue

      if( (ik.eq.3 .or. ik.eq.4) .and. na.gt.n0) then
        do 200 i=n0,na
        y(i)=y(na)
 200    continue
      end if

      return
      end




*****************************************************************************
*
*               S O L I T G E N
*
*
*     Tabulerer verdier svarende til en soliton-form. Det antas at solitonen
*     forplanter seg mot |kende indekser i y. Har den motsatt forplantning
*     beholdes verdiene for overflatehevninger mens foretegnet byttes for
*     hastighet og potensial. Den aktuelle solitonl|sningen er beskrevet
*     i Pedersen og Rygg (1987) og Pedersen (1988).

*     parametere:
*             y  - array som inneholder verdiene                          O
*             n0,n - arraygrenser for y i det kallende programmet         I
*                    maksimalt antall punkter er 1000.
*             dx - avstand (regnet i dyp) mellom punktene.                I
*             amp - maks overflatehevning (regnet i dyp)                  I
*             topp - posisjon av makspunkt, merk at topp=0                I
*                    vil plassere makspunkt hos y(0), topp=dx
*                    hos y(1) etc. Toppen kan godt v{re lokalisert
*                    utenfor arrayendene. V{r oppmerksom p{ at y(1) svarer
*                    til ulike fysiske posisjoner for ulike ukjente i ett
*                    alternerende gitter.
*             c - forplantningshastighet for solitonen (skalert med       O
*                 line{r gruntvannshastighet)
*             ua - maksverdi for hastighet, skalert som c                 O
*             ik - styringsparameter                                      I 
*                 =1  :  Y fylles med overflatehevninger
*                 =2  :  Y fylles med hastigheter
*                 =3  :  y fylles med potensialverdier. I dette tilfellet
*                          er verdiene satt slik at midtpunktdifferens gir
*                          eksakte hastigheter, og y(n) er valgt lik null 
*                          (potensialet er jo bestemt bare p} en konstant n{r)
*                 =4  :  Som 3, bortsett fra at verdiene finnes ved integrasjon
*                        av en spline-interpolant for hastigheten.
*                 =5  :  Y fylles med rom-deriverte av overflatehevningen
*
*             fc - faktor for modifikasjon av hast. og potensial i hht.   I
*                  hydrostatisk korreksjon av regneskjemaer.         
*                  Den mod. hastighet blir satt ved:
*
*                    U_mod = U +fc*U''
*             eps - trunkeringsgrense                                     I
*
**************************************************************************
      SUBROUTINE SOLITGEN(Y,n0,N,DX,AMP,Topp,C,UA,ik,fc,eps)
      INTEGER n0,N,ik
      REAL Y(N0:N),DX,AMP,topp,C,UA,fc,eps
*---------------------------------------------------------------------------
      INTEGER J,nant,ier,nmax,i1
      parameter(nmax=15000)
      real bhast, toppu,f(nmax),der1,der2,wk(5*nmax+10),uder
      real amplim


      amplim=0.035

      nant=n-n0+1

      if(nant.gt.nmax) then
        write(0,*)'for mange punkter i kall p} soliton'
        return
      end if
      
      c=bhast(amp)     
      ua=c*amp/(1.0+amp)            

      if(ik.eq.3) then
c      
c       hastigheter skal beregnes og legges i y slik at toppunkt er
c       forskj|vet -0.5*dx
c
c       pos av pot:     y(n-2)     y(n-1)      y(n)    ------------->x
c                                                                   
c       pos av hast:          u(n-2)     u(n-1)     
c
        toppu = topp -0.5*dx
      else
        toppu = topp
      end if

      if(amp.gt.amplim) then
         CALL STappg(y,n0,n,AMP,TOPPU,Dx,eps)
C       denne returnerer verdier for hastigheten, vi m} derfor regne
c       om n}r ik=1.
      else
c       for sm} amplituder er denne tryggere.
        call  solipert(Y,n0,N,Dx,0.0,0.0,0.0,0.0,0,0,AMP,TOPPU,C,eps)
        c=bhast(amp)   
        do 80 j=n0,n
         y(j)=c*y(j)/(1.0+y(j))
  80    continue
      end if

      if(ik.eq.1) then
       do 100 j=n0,n
         y(j)=y(j)/(c-y(j))
  100  continue
       return
      end if

      if(ik.eq.5) then
       i1=topp/dx
       i1=min(i1,n)
       do 150 j=n0,i1
         y(j)=c*uder(y(j),amp,ier)/(c-y(j))**2
  150  continue
       i1=max(n0-1,i1)
       do 160 j=i1+1,n
         y(j)=-c*uder(y(j),amp,ier)/(c-y(j))**2
  160  continue
       return

      end if   

      if(ik.eq.4) then

        do 300 j=1,nant
        f(j)=y(j+n0-1)
 300    continue

        der1=uder(f(1),amp,ier)
        der2=uder(f(nant),amp,ier)
        call ingrer(y(n0),f,nant,dx,der1,der2,nant,wk)
        return
      end if

c
c    her modifiseres hastigheter i hht. verdi p} fc
c
     
      do 250 j=n0,n
      y(j)=y(j)+3.0*fc*y(j)*( c-0.5*y(j)-1.0/(c-y(j)) )
 250  continue

      if(ik.eq.3) then
c       vi integrerer fram hastighetspotensialet
c       hastpot. settes lik null i punkt n

        y(n)=0.0

        do 200 j=n-1,n0,-1
        y(j)=y(j+1)-dx*y(j)
  200   continue
      end if
              


      RETURN
      END





*****************************************************************************
*
*               S O L I P R O D
*
*
*     Tabulerer verdier svarende til en soliton-form ved kall p} 'SOLITON',
*     men gj|r tre ting i tillegg:
*                 1. Trunkerer solitonen der h|yden er mindre  enn amp*eps
*                 2. fyller y med nuller utenfor trunkert omr}de.
*                 3. S|rger for  at soliton kalles med increment (dx) som
*                    aldri overstiger 1.0. Dette kan v}re viktig for
*                    konvergens av underliggende likniongsl|sere.
*     Parameterene svarer til de for 'soliton' untatt eps som alts} gir
*     trunkerings-kriteriet.
**************************************************************************
      subroutine soliprod(Y,n0,N,DX,AMP,TOPP,C,UA,ik,eps)   
      INTEGER n0,N,ik
      REAL Y(N0:N),DX,AMP,topp,C,UA,eps
*---------------------------------------------------------------------------
      real fdumm

      fdumm=0.0

      call soliprgen(Y,n0,N,DX,AMP,TOPP,C,UA,ik,fdumm,eps)   

      return
      end




*****************************************************************************
*
*               S O L I T O N
*
*
*     Tabulerer verdier svarende til en soliton-form. Det antas at solitonen
*     forplanter seg mot |kende indekser i y. Har den motsatt forplantning
*     beholdes verdiene for overflatehevninger mens foretegnet byttes for
*     hastighet og potensial. Den aktuelle solitonl|sningen er beskrevet
*     i Pedersen og Rygg (1987) og Pedersen (1988).

*     parametere:
*             y  - array som inneholder verdiene                          O
*             n0,n - arraygrenser for y i det kallende programmet         I
*                    maksimalt antall punkter er 1000.
*             dx - avstand (regnet i dyp) mellom punktene.                I
*             amp - maks overflatehevning (regnet i dyp)                  I
*             topp - posisjon av makspunkt, merk at topp=0                I
*                    vil plassere makspunkt hos y(0), topp=dx
*                    hos y(1) etc. Toppen kan godt v{re lokalisert
*                    utenfor arrayendene. V{r oppmerksom p{ at y(1) svarer
*                    til ulike fysiske posisjoner for ulike ukjente i ett
*                    alternerende gitter.
*             c - forplantningshastighet for solitonen (skalert med       O
*                 line{r gruntvannshastighet)
*             ua - maksverdi for hastighet, skalert som c                 O
*             ik - styringsparameter                                      I 
*                 =1  :  Y fylles med overflatehevninger
*                 =2  :  Y fylles med hastigheter
*                 =3  :  y fylles med potensialverdier. I dette tilfellet
*                          er verdiene satt slik at midtpunktdifferens gir
*                          eksakte hastigheter, og y(n) er valgt lik null 
*                          (potensialet er jo bestemt bare p} en konstant n{r)
*                 =4  :  Som 3, bortsett fra at verdiene finnes ved integrasjon
*                        av en spline-interpolant for hastigheten.
**************************************************************************
      SUBROUTINE SOLITON(Y,n0,N,DX,AMP,Topp,C,UA,ik)
      INTEGER n0,N,ik
      REAL Y(N0:N),DX,AMP,topp,C,UA
*---------------------------------------------------------------------------
      real fcdum,eps
      fcdum=0.0
      eps=0.0001
      call SOLITGEN(Y,n0,N,DX,AMP,Topp,C,UA,ik,fcdum,eps)

      RETURN
      END


*********************************************************************
*     u    -   verdi av hastighet i soliton                       I
*     a    -   amplitude av soliton                               I
*     ier  -   feil-parameter ; ier=1 svarer til ulovlig          O
*              verdi p} u
*     uder -   verdi for du/dx                                    O
*
**********************************************************************
      function uder(u,a,ier)
      integer ier
      real u,a,uder
*--------------------------------------------------------------------
      real bhast,hj,c,ua

      c=bhast(a)

      ua=c*a/(1.0+a)

      if(u.ge.ua .and. (u-ua).lt.0.001*a) then
        ier=0
        uder=0.0
        return
      end if
      if(u.gt.ua .or. u.lt.0.0) then
        ier=1
        uder=0.0
        return
      else
        ier=0
      end if

      hj=c*log(1.0-u/c)+u+0.5*c*u*u-u*u*u/6.0
      if(hj.lt.0.0) hj=0.0
      uder=sqrt(6.0*hj/c)

      return
      end




***************************************************************************
      SUBROUTINE STAPPG(VER,N0,N,A,TOPP,DKSI,eps)
      INTEGER N0,N
      REAL VER(N0:N),A,TOPP,DKSI,eps
*----------------------------------------------------------------------
      REAL UV(0:2000),XTOPP,BL,halvb,tmp,rest,ds,xuv,xver,w(10000)
      INTEGER NADD,NTP,J,nl,nmin,nmax,n1,n2,nb,nuv,iflag
                                  
      BL=halvb(a,eps)
      nl=bl/dksi+2
c     totallet er lagt til for sikring
c    
c     vi finner avstand fra topp til n{rmeste venstre (mindre) punkt      
      NTP=TOPP/DKSI
      tmp=topp +abs((ntp+1)*dksi)
c     sikrer at tmp blir positiv, samtid som avstand beholdes.
      ntp=tmp/dksi
      rest=tmp-ntp*dksi
      
      xtopp = nl*dksi +rest
      if(xtopp.gt.topp) then
        nadd=(xtopp-topp+0.01*dksi)/dksi
      else
        nadd=-(topp-xtopp+0.01*dksi)/dksi
      end if

c     vi maa naa finne hvilket intervall n1,n2 som ligger innenfor
c     trunkeringsender.

      nmax=(topp+bl)/dksi
      nmin=(topp-bl)/dksi

      n1=max(n0,nmin)
      n2=min(n,nmax)      

      DO 5 J=N0,N
      ver(J)=0.0
 5    CONTINUE                             

      if(n1.gt.n2) return


      XTOPP=TOPP+NADD*DKSI                           

      if(2*xtopp/dksi.lt.800) then
         DO 10 J=N1,N2
         UV(J+NADD)=0.0
  10     CONTINUE                             
         CALL STUFF(UV,A,XTOPP,DKSI)
         DO 100 J=N1,N2
         VER(J)=UV(J+NADD)
  100    CONTINUE                        
      else
c       for } unng} overskrivning i stuff m} vi g} via en spline interpolant
        ds=bl/100.0
        xtopp= 101*ds
        call stuff(uv,a,xtopp,ds)
        nuv=202
        uv(0)=0.0
        uv(nuv-1)=0.0
        xuv=topp-xtopp
        xver=n0*dksi
        nb=n-n0+1
        call stag(uv(0),nuv,ds,xuv,ver,nb,dksi,xver,iflag,w)
        if(iflag.gt.0) then
          write(0,*)' problemer med stag i stappg, iflag=',iflag
        end if
      end if
      RETURN
      END





******************************************************************************

      SUBROUTINE STAPP(VER,N0,N,A,TOPP,DKSI)
      INTEGER N0,N
      REAL VER(N0:N),A,TOPP,DKSI
*----------------------------------------------------------------------
      REAL UV(0:1000),XTOPP,HLP,BL
      INTEGER NADD,NTP,J,IBL
                                  
      BL=-LOG(0.0005)/SQRT(3.0*A)
      IBL=BL/DKSI
      NTP=TOPP/DKSI
      NADD=MAX(2+N+N0-2*NTP,2-N0)
      NADD=MAX(0,NADD)
      NADD=MIN(IBL,NADD)
      DO 10 J=N0,N
      UV(J+NADD)=0.0
  10  CONTINUE                             
      XTOPP=TOPP+NADD*DKSI                           
      HLP=XTOPP/DKSI
      CALL STUFF(UV,A,XTOPP,DKSI)
      DO 100 J=N0,N
      VER(J)=UV(J+NADD)
  100 CONTINUE                             
      RETURN
      END


***************************************************************
*                                                             *
* FINNER FORMEN TIL SOLITONEN VED "ANALYTISKE" BEREGNINGER.   *
* PARAMETERE:  UV   =                                         *
*              TOPP =                                         *
*              DKSI =                                         *
* -KALLER 'SIIE,SING,UEN'-                                    *
*                                                             *
***************************************************************

      SUBROUTINE STUFF(UV,AA,TOPP,DKSI)
      REAL UV(0:1000),TOPP,DKSI,AA
      EXTERNAL SING,UEN
*-------------------------------------------------------------
      REAL EPS1,EPS,DARG,S1,S0,HJ(1000),UA,C ,A
      REAL BHAST,sdum
      INTEGER NV,NH,NP,IFLAG,NF,NL,J
      COMMON/SOL/A,UA,C

 11   format(1x,'advarsel,full konvergens i soliton-beregning')
 12   format(1x,'er ikke oppnaadd ved pos. ',i1,' iflag=',i4)     
      A=AA
      C=BHAST(A)
      UA=C*A/(1.0+A)
      EPS  = 1.0E-4 
      EPS1=0.01
      NV   = TOPP/DKSI
      NH   = NV+1
      DARG = 0.5*(TOPP-NV*DKSI)
      NP   = 1
      S1   = SQRT(UA*0.7)

      sdum=0.0
      CALL SIIE(SING,HJ,NP,DARG,sdum,S1,20,EPS,IFLAG,50)    

      if(iflag.gt.0) then
        write(0,11)
        write(0,12) 1,iflag
      end if                     

      S0   = HJ(1)
      UV(NV) = UA-S0*S0
      NP   = NV-1
      DARG = DKSI*0.5

      CALL SIIE(SING,HJ,NP,DARG,S0,S1,30,EPS,IFLAG,50)
      if(iflag.gt.0) then
        write(0,11)
        write(0,12) 2,iflag
           write(0,*)'np=',np
           do 32 j=1,np
           write(0,*) j,hj(j)
 32        continue
      end if                     

      DO 100 J=1,NP
         UV(NV-J) = UA-HJ(J)*HJ(J)
  100 CONTINUE
      NF   = NV-1-NP
      NP   = NF
      IF(NP.GT.0)THEN
         S0 =-LOG(UV(NF+1))
         S1 = 20

         CALL SIIE(UEN,HJ,NP,DKSI,S0,S1,20,EPS1,IFLAG,50)
         if(iflag.gt.0) then
           write(0,11)
           write(0,12) 3 ,iflag
         end if                     

         DO 85 J=1,NP
            UV(NF+1-J) = EXP(-HJ(J))
   85    CONTINUE
      END IF
      NL   = NF+1-NP
      DARG = 0.5*(TOPP-NV*DKSI)
      NP   = 1
      S1   = SQRT(UA*0.7)
      DARG = 0.5*(NH*DKSI-TOPP)

      sdum=0.0
      CALL SIIE(SING,HJ,NP,DARG,sdum,S1,20,EPS,IFLAG,50)
      if(iflag.gt.0) then
        write(0,11)
        write(0,12) 4,iflag
      end if                     

      S0   = HJ(1)
      UV(NH) = UA-S0*S0
      NP   = NV-1
      DARG = DKSI*0.5

      CALL SIIE(SING,HJ,NP,DARG,S0,S1,20,EPS,IFLAG,50)
      if(iflag.gt.0) then
        write(0,11)
        write(0,12) 5,iflag
      end if                     

      DO 200 J=1,NP
         UV(NH+J) = UA-HJ(J)*HJ(J)
  200 CONTINUE
      NF   = NH+NP
      NP   = NV-1-NP
      IF(NP.GT.0)THEN
         S0 =-LOG(UV(NF))
         S1 = 20

         CALL SIIE(UEN,HJ,NP,DKSI,S0,S1,20,EPS1,IFLAG,50)
         if(iflag.gt.0) then
           write(0,11)
           write(0,12) 6,iflag
         end if                     

         DO 185 J=1,NP
            UV(NF+J) = EXP(-HJ(J))
  185    CONTINUE
      END IF

      RETURN
      END

***************************************************************
*                                                             *
* INTEGRANDEN I INTERGRALET SOM TABULERES AV SIIE.            *
* KALLES AV DEN EKSTERNE PROSEDYREN SIIE.                     *
* PARAMETER:  R =                                             *
*                                                             *
***************************************************************

      FUNCTION SING(R)
      REAL R
      REAL A,UA,C
      COMMON/SOL/A,UA,C
*--------------------------------------------------------------
      REAL S,F,TEM1,SF,TEM2,SING
      INTEGER I

      S    = R*R
      F    = 1.0-UA/C
      TEM1 =-6.0/C+3.0*(C*F*F-C+F*S)+S*S/C
      SF   = S/(C-UA)
      IF(SF.GT.0.01) THEN
         TEM2 = 6.0*LOG(1.0+SF)/S
      ELSE
         TEM2 = 0.0
         DO 100 I=0,4
            TEM2 = 1.0/(5.0-I)-SF*TEM2
  100    CONTINUE
         TEM2 = 6.0*TEM2/(C-UA)
      END IF

      SING = 1.0/SQRT(TEM1+TEM2)

      RETURN
      END



***************************************************************
*                                                             *
* INTEGRANDEN I INTERGRALET SOM TABULERES AV SIIE.            *
* KALLES AV DEN EKSTERNE PROSEDYREN SIIE.                     *
* PARAMETER:  T =                                             *
*                                                             *
***************************************************************

      FUNCTION UEN(T)
      REAL T
      REAL A,UA,C
      COMMON/SOL/A,UA,C
*--------------------------------------------------------------
      REAL S,SC,TEM,UEN
      INTEGER I

      S   = EXP(-T)
      SC  = S/C
      IF(S.LT.0.01) THEN
         TEM = 0.0
         DO 70 I=0,5
            TEM = 1.0/(7.0-I)+SC*TEM
   70    CONTINUE
         TEM =-6.0*TEM/(C*C)
      ELSE
         TEM = 6.0*(1.0/(C*S)+LOG(1-SC)/(S*S))
      END IF

      UEN = 1.0/SQRT(3.0-SC+TEM)

      RETURN
      END


*****************************************************************
*
*                        GKDVSOL
*      amp - amplitude( to depth)                         I
*      x - distance (length/depth) from top             I 
*      kdv - kdv sol. if .true., Serre otherwise        I
*      eta - surface elevation                          O
*      etax - x derivative of surface elevation
****************************************************************
      subroutine gkdvsol(amp,x,kdv,eta,etax)
      real amp,x,eta,etax
      logical kdv
*--------------------------------------------------------------
      integer isgn
      real fak,c,arg,e,cc,ss

      if(kdv) then
cc      standard kdv equation with ..... Yxxx=0
        c=1.0+0.5*amp
        fak=sqrt(3.0*amp)*0.5
      else
        c=sqrt(1.0+amp)
        fak=sqrt(3.0*amp)*0.5/c
      end if

      arg=fak*x
      if(arg.gt.0.0) then
        isgn=1
      else
        isgn=-1
        arg=-arg
      end if

      if(arg.gt.20.0) then
        eta=0.0
        etax=0.0
      else
        e=exp(arg)
        cc=0.5*(e+1.0/e)
        ss=0.5*(e-1.0/e)
        eta=amp/(cc*cc)
        etax=-2.0*isgn*amp*fak*(ss/cc)/(cc*cc)
      end if


      return
      end

***************************************************************************
* The subroutine computes strain parameter (alf) and Froude number (f) from
* amplitude/depth (a) according to solution of Fenton (1972).
*
* Leading order surface elevation then becomes (propagation toward pos. x)
*         eta=A*sech^2(alf*(x-f*t)) 
*
***************************************************************************
      subroutine parfent(a,alf,c)
      real a,alf,c
*-------------------------------------------------
      real f2,uinf
      alf=sqrt(0.75*a)*(1.0 +a*(-5.0/8.0+a*71.0/128.0))
      f2=1.0+a*(1.0 -a*(0.05+3.0*a/70.0))
      c=sqrt(f2)
cc    This value is consistent with u=0 at infinity


cc    half wavelength -log(0.25*eps)/(2.0*alf)
      return
      end 


***************************************************************************
* The subroutine computes the surface elevation
*  according to solution of Fenton (1972)
*
* Scaling: same scaling vertically and horizontally (d -equilibrium depth
*          ) , char velocity is sqrt(g*d). Zero velocity at infinity.
*
*  Velocity components are
*    u=u0+y*y*u1+y*y*y*y*u2
*    v=y*(v0+y*y*v1+y*y*y*y*v2)
*  where y is height over bottom.
*  The velocity potential is defined as zero at x=-infinity and is computed at
*  the bottom
*    fb=phi(x,0)
*  The potential at other locations are then given as
*    phi(x,y)=fb(x)+y^2*v0/2+y^4*v1/4+y^6*v2/6
*  fu is the value of fb at x=+infty
*
*  Parameters:
*   a - amplitude (max eta)                                         I
*   alf - strain parameter                                          I
*   x - value of argument (x-c*t) or (x+c*t)                        I
*  eta - surface elevation                                          O
*  etax - derivaive of eta with respect to x                        O
* u0,u1,u2,v0,v1,v2,fb,fu - velocty and potential factors as explained above O 
***************************************************************************
      subroutine fent3(a,alf,x,eta,etax,u0,u1,u2,v0,v1,v2,fb,fu)
      real a,alf,x,eta,etax,u0,u1,u2,v0,v1,v2,fb,fu
*-------------------------------------------------
      real s,t,ss,tt,ssx,ttx,e1,e2,e3,fak,i1,i2,i3
      real iu1,iu2,iu3

      s=1.0/cosh(alf*x)
      t=sinh(alf*x)*s
      ss=s*s
      tt=t*t
      ssx=-2.0*alf*t*ss
      ttx=-ssx

cc    i1 is the integral of ss from x=-\infty, i2 is the integral of ss*ss etc
cc    recursion formula is used to compute values
cc    in=(t*s^(2*n-2)+ (2*n-2)*i(n-1))/(2*n-1)
      i1=t+1
      i2=(t*ss+2.0*i1)/3.0
      i3=(t*ss*ss+4.0*i2)/5.0
cc    iu1 etc is value at infinity
      iu1=2.0
      iu2=2.0*i1/3.0
      iu3=4.0*i2/5.0

      e1=ss
      e2=-0.75*ss*tt
      e3=ss*tt*(5.0/8.0 - ss*101.0/80.0)
      eta=a*(e1+a*(e2+a*e3))
      e1=ssx
      e2=-0.75*(ss*ttx+ssx*tt)
      e3=(ss*ttx+ssx*tt)*5.0/8.0 - ss*(ss*ttx+2.0*ssx*tt)*101.0/80.0
      etax=a*(e1+a*(e2+a*e3))

cc    Fenton has propagation toward decreasing x
cc    Constant term in x in solution of Fenton vanish due to coordinate system
      e1=ss
      e2=-ss*(ss-0.25)
      e3=-ss*(19.0/40.0+ss*(0.2-ss*1.2))       
      u0=a*(e1+a*(e2+a*e3))

      e2=-ss*(1.5-ss*2.25)
      e3=-ss*(-1.5+ss*(-3.75+ss*7.5))
      u1=a*a*(e2+a*e3)
    
      u2=-a*a*a*ss*(-3.0/8.0+ss*(1.0-ss)*45.0/16.0)
      
cc    Calculation of bottom potental, formula for u0 with ss**n replaced by in
cc      e1=ss
cc      e2=-ss*ss+0.25*ss
cc      e3=-19.0/40.0*ss -ss*ss*0.2+ss*ss*ss*1.2    
      e1=i1
      e2=-i2+0.25*i1
      e3=-i1*19.0/40.0 -i2*0.2 + i3*1.2    
      fb=a*(e1+a*(e2+a*e3))/alf

      e1=iu1
      e2=-iu2+0.25*iu1
      e3=-iu1*19.0/40.0 -iu2*0.2 + iu3*1.2    
      fu=a*(e1+a*(e2+a*e3))/alf

      fak=sqrt(3.0*a)*t
      e1=-ss
      e2=ss*(3.0/8.0+2.0*ss)
      e3=ss*(49.0/640.0+ss*(-17.0/20.0-ss*3.6))       
      v0=-fak*a*(e1+a*(e2+a*e3))

      e2=ss*(0.5-ss*1.5)
      e3=ss*(-13.0/16.0+ss*(-25.0/16+ss*7.5))
      v1=-fak*a*a*(e2+a*e3)
    
      v2=-fak*a*a*a*ss*(-3.0/40.0+ss*(1.125-ss*27.0/16.0))


      return
      end 

*****************************************************************************
*
*               S O L F E N T O N
*
*
*     Tabulerer verdier svarende til en soliton-form. Det antas at solitonen
*     forplanter seg mot |kende indekser i y. Har den motsatt forplantning
*     beholdes verdiene for overflatehevninger mens foretegnet byttes for
*     hastighet og potensial. 
*     parametere:
*             eta,etax,u,v,phi  - arrayer som inneholder verdiene av   O
*                    feltene i overflaten 
*             um -  umiddel                                                O
*             ub,phib - felter ved bunn                                 O
*             n - arraygrenser for y i det kallende programmet         I
*                    maksimalt antall punkter er 10000. (strat er 0)
*             dx - avstand (regnet i dyp) mellom punktene.                I
*             amp - maks overflatehevning (regnet i dyp)                  I
*             topp - posisjon av makspunkt, merk at topp=x0                I
*                    vil plassere makspunkt hos y(0), topp=x0+dx
*                    hos y(1) etc. Toppen kan godt v{re lokalisert
*                    utenfor arrayendene. V{r oppmerksom p{ at y(1) svarer
*                    til ulike fysiske posisjoner for ulike ukjente i ett
*                    alternerende gitter.
*             c - forplantningshastighet for solitonen (skalert med       O
*                 line{r gruntvannshastighet)
*             fu - potential at + infinity
*             eps - trunkeringsgrense                                     I
*
**************************************************************************
      subroutine solfenton(eta,etax,u,v,phi,um,ub,phib,n,x0,dx,amp,
     %           topp,c,fu,eps)
      integer n
      real eta(0:n),etax(0:n),u(0:n),v(0:n),phi(0:n)
      real um(0:n),ub(0:n),phib(0:n),x0,dx,amp,topp,c,fu,eps
*---------------------------------------------------------------------------
      integer i
      real alf,halvb,xeff,u0,u1,u2,v0,v1,v2,d,d2

      call parfent(amp,alf,c)
      halvb=-log(0.25*eps)/(2.0*alf)

      do 100 i=0,n
        xeff=x0+i*dx-topp
        if(xeff.gt.halvb.or.xeff.lt.-halvb) then
         eta(i)=0.0
         etax(i)=0.0
         ub(i)=0.0
         u(i)=0.0
         um(i)=0.0
         v(i)=0.0
        else
          call fent3(amp,alf,xeff,eta(i),etax(i),u0,u1,u2,v0,v1,v2
     %     ,phib(i),fu)
          d=1.0+eta(i)
          d2=d*d
          ub(i)=u0
          u(i)=u0+d*d*(u1+d*d*u2)
          um(i)=u0+d*d*(u1/3.0+d*d*u2/5.0)
          v(i)=d*(v0+d*d*(v1+d*d*v2))
          phi(i)=phib(i)+d2*(0.5*v0+d2*(0.25*v1+d2*v2/6.0))
        end if
 100  continue
      return
      end



*****************************************************************************
*
*               ssolip
*
*
*     Tabulerer verdier svarende til en soliton-form. Det antas at solitonen
*     forplanter seg mot |kende indekser i y. Har den motsatt forplantning
*     beholdes verdiene for overflatehevninger mens fortegnet byttes for
*     hastighet og potensial. 

*     parametere:
*             y  - array som inneholder verdiene                          O
*             n0,n - arraygrenser for y i det kallende programmet         I
*             dx - avstand (regnet i dyp) mellom punktene.                I
*             amp - maks overflatehevning (regnet i dyp)                  I
*             topp - posisjon av makspunkt, merk at topp=0                I
*                    vil plassere makspunkt hos y(0), topp=dx
*                    hos y(1) etc. Toppen kan godt v{re lokalisert
*                    utenfor arrayendene. V{r oppmerksom p{ at y(1) svarer
*                    til ulike fysiske posisjoner for ulike ukjente i ett
*                    alternerende gitter.
*             c - wave celerity                                           O
*             ivis - determine type of solitary wave                      I
*                   = 0 KdV
*                     1 Serre with no correction
*                     2 Serre with correction of Nwogu type
*                     3 Serre which reproduces linear dispersion incl. O(k^4)
*                     4 Serre with a specified value on kappa 
*             gamma - parameter in equations, selected automatically       I
*                     in case ivis <=3                      
*
**************************************************************************
      subroutine ssolip(Y,n0,N,DX,AMP,TOPP,c,ivis,gamma)   
      INTEGER n0,N,ivis
      REAL Y(N0:N),DX,AMP,topp,c,gamma
*---------------------------------------------------------------------------
      integer i
      real fak,arg,e,rgamma,z0,cc

      if(ivis.le.1) rgamma=0.0
      if(ivis.eq.2) rgamma=-0.0566862
      if(ivis.eq.3) rgamma=-1.0/15.0
      if(ivis.eq.4) rgamma=gamma

      
      if(ivis.eq.0) then
cc      standard kdv equation with ..... Yxxx=0
        c=1.0+0.5*amp
        fak=sqrt(3.0*amp)*0.5
      else
        c=sqrt(1.0+amp+0.75*rgamma*amp*amp)
        fak=sqrt(3.0*amp)*0.5*sqrt(1.0+3.75*rgamma*amp)/c
      end if


      do 100 i=n0,n
        arg=abs(fak*(i*dx-topp))
        if(arg.gt.20.0) then
          z0=0.0
        else
          e=exp(arg)
          cc=(e+1.0/e)*0.5
          z0=1.0/(cc*cc)
        end if
        y(i)=amp*z0*(1.0+amp*11.25*rgamma*(1.0-z0))
  100 continue

      return
      end


*****************************************************************
*
*                        ZKDVSOL
*      amp - amplitude( to depth)                         I
*      x - distance (length/depth) from top             I 
*      ivis -  determine type of solitary wave        I
*                   = 0 KdV
*                     1 Serre with no correction
*                     2 Serre with correction of Nwogu type
*                     3 Serre which reproduces linear dispersion incl. O(k^4)
*                     4 Serre with a specified value on kappa 
*             gamma - parameter in equations, selected automatically       I
*                     in case ivis <=3                      
*      eta - surface elevation                          O
*      etax - x derivative of surface elevation         O
****************************************************************
      subroutine zkdvsol(amp,x,ivis,gamma,eta,etax)
      integer ivis
      real amp,x,gamma,eta,etax
*--------------------------------------------------------------
      real fak,c,arg,e,cc,ss,z0,z0x,rgamma

      if(ivis.le.1) rgamma=0.0
      if(ivis.eq.2) rgamma=-0.0566862
      if(ivis.eq.3) rgamma=-1.0/15.0
      if(ivis.eq.4) rgamma=gamma

      if(ivis.eq.0) then
cc      standard kdv equation with ..... Yxxx=0
        c=1.0+0.5*amp
        fak=sqrt(3.0*amp)*0.5
      else
        c=sqrt(1.0+amp+0.75*rgamma*amp*amp)
        fak=sqrt(3.0*amp)*0.5*sqrt(1.0+3.75*rgamma*amp)/c
      end if

      arg=fak*x

      if(arg.gt.20.0) then
        eta=0.0
        etax=0.0
      else
        e=exp(arg)
        cc=0.5*(e+1.0/e)
        ss=0.5*(e-1.0/e)
        z0=1.0/(cc*cc)

        z0x=-2.0*fak*ss/(cc*cc*cc)
        eta=amp*z0*(1.0+amp*11.25*rgamma*(1.0-z0))
        etax=z0x*amp*(1.0+amp*11.25*rgamma*(1.0-2.0*z0) )
      end if


      return
      end

