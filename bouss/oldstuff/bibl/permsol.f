******************************************************************************
*
*                  b e r p a r
*      Rutinen beregner hoyere ordens korreksjoner til numerisk soliton-
*  fasong. Uttrykket for overflatehevningen er:
*         y= alph*y0 -0.5*d*(alph*y0)**2       ; y0 =(cosh(k*(s-ct)))**-2
*  der s=x*cos(psi)+y*sin(psi), k=1 +alph*ak1+.. 
*  og c= 1+0.5*alph+c2*alph*alph+... Koordinatene
*  x,y og t er skalert med dyp/sqrt(alph) som karakteristisk lengde.
*
*
*       parametere:
*                dx,dy,dt - gitteravstander. Disse er regnet  i             I
*                        normalskalering, dvs. at dx er regnet i antall
*                        dyp, dt i tidsintervall det tar  aa tilbakelegge
*                        ett dyp med linear grundtvannshastighet.
*                psi   - vinkel med x-aksen, regnet i grader                I
*                ik    - verdi lik 1 gir korreksjon                         I
*                c2    - 2-ordens hastighetskorreksjon                      O
*                ak1   - 1-ordens boelgetallskorreksjon                     O
*                d     - koeff. for ikkkelinear formkorr.                   O
*                sig   - koeff i likning for Y0 :                           O
*                        2*sig*Y0''+(1.5*Y0-1)*Y0=0
******************************************************************************
      subroutine berpar(dx,dy,dt,psi,ik,c2,ak1,d,sig)
      integer ik
      real dx,dy,dt,psi,c2,ak1,d,sig
*---------------------------------------------------------------------
      real gamm1,gamm2,gamm3,kapp1,kapp2,kapp3,kapp4,kapp5
      real kapp6,kapp7,kapp8
      real r2sig,a,b,u,d2u,uy,d2y,d4y,yd2y,d2y2,dydy
      real dx2,dt2,aa,bb,dd,f1,f2,s1,s2,q,rfak,sf2,cf2
      real e2,e4,pi,dy2,my

      pi=atan(1.0)*4.0
      sf2=sin(pi*psi/180.0)
      sf2=sf2*sf2
      cf2=cos(pi*psi/180.0)
      cf2=cf2*cf2
      dt2=dt*dt
      dx2=dx*dx
      dy2=dy*dy
      e2=dx2*cf2*cf2+dy2*sf2*sf2
      e4=dx2*dx2*cf2*cf2*cf2 + dy2*dy2*sf2*sf2*sf2

      my=ik*( (dt2-dx2)*cf2 + (dt2-dy2)*sf2 )/12.0
      rfak=1.0/3.0+my

      gamm1=e2/12.0 +ik*(dx2+dy2)*sf2*cf2/12.0
      gamm2=dt2/24.0
      gamm3=gamm2

      kapp1=e4/360.0 + ik*(dx2+dy2)*(dx2*cf2+dy2*sf2)*cf2*sf2/144.0
      kapp2=dt2*dt2/1920.0
      kapp3=e2/24.0
      kapp4=(e2+dt2)/8.0
      kapp5=kapp3
      kapp6=(2*e2+dt2)/72.0-kapp2 + my*dt2/24.0 + ik*(dx2*cf2*cf2
     %       *(dt2-dx2) +dy2*sf2*sf2*(dt2-dy2))/144.0
csep1      kapp4=kapp3/3.0
csep1      kapp5=dt2/16.0
csep1      kapp6=(2*e2+dt2)/72.0
      kapp7=(2*e2+3*dt2)/12.0
      kapp8=dt2/4.0
csep1      kapp8=2*kapp5

      sig=0.5*(rfak+gamm1-gamm2-gamm3)
      r2sig=0.5/sig

      d2u=rfak+gamm1-gamm3
      uy=2.0
      u=-0.5
      d2y=0.5*rfak-1.5*gamm3-gamm2
      d4y=kapp1-kapp2+kapp6
      yd2y=kapp4+kapp5+kapp7
      d2y2=kapp3
      dydy=-kapp8

c      u= y1 +0.5*y -y*y +q*y''

       q=-gamm1+gamm2
       aa = 0.5*u
       bb =-1.0*u
       d2y = d2y +u*q

       bb = bb+ 0.5*uy
       dd = -uy
       yd2y = yd2y +q*uy

       d2y=d2y +d2u*0.5
       d2y2=d2y2 - d2u
       d4y = d4y +d2u*q

c      y''=f1*y+f2*y*y
c      y''''=f1*y''+f2*(y*y)''

       f1=r2sig
       f2=-1.5*r2sig

       d2y=d2y + f1*d4y
       d2y2=d2y2 + f2*d4y

c      (y*y)''=2y*y'' + 2y'*y'

       yd2y=yd2y + 2.0*d2y2
       dydy= dydy+ 2.0*d2y2

       aa=aa +f1*d2y
       bb=bb +f2*d2y + f1*yd2y
       dd= dd+f2*yd2y

c      y'y'= s1*y*y +s2*y*y*y

       s1=r2sig
       s2=-r2sig

       bb= bb + s1*dydy
       dd= dd + s2*dydy


       a=-0.25 +r2sig*rfak + r2sig*(gamm1-1.5*gamm2-2*gamm3)
     %   -r2sig*r2sig*(rfak+gamm1-gamm3)*(gamm1-gamm2)
     %   +r2sig*r2sig*(kapp1-kapp2+kapp6)

       b=1.5-1.5*rfak*r2sig-1.5*r2sig*(gamm1-1.5*gamm2-2*gamm3)
     %  -2.0*r2sig*(gamm1-gamm2)+r2sig*(kapp4+kapp5+kapp7-kapp8)
     %  -4.0*r2sig*(rfak +gamm1-gamm3-kapp3)
     %   +7.5*r2sig*r2sig*(rfak+gamm1-gamm3)*(gamm1-gamm2)
     %   -7.5*r2sig*r2sig*(kapp1-kapp2+kapp6)       

       d=-2 +3.0*r2sig*(gamm1-gamm2) -1.5*r2sig*(kapp4+kapp5+kapp7)
     %  +r2sig*kapp8
     %  +5*r2sig*(rfak +gamm1-gamm3-kapp3)
     %   -7.5*r2sig*r2sig*(rfak+gamm1-gamm3)*(gamm1-gamm2)
     %   +7.5*r2sig*r2sig*(kapp1-kapp2+kapp6)       

c       write(*,*)'a,b,d,sig'
c       write(*,*)a,b,d,sig

c       write(*,*) 'aa,bb,dd'
cmsol       write(*,*) aa,bb,dd

c       fortegn maa byttes om:
       a=-a
       b=-b
       d=-d
c
c    likning ser ut som  L(Y1)= (2*c2-2*ak1 +a)*Y0 +(3*ak1 +b)*Y0**2 +d*y0**3
c
       ak1=-b/3.0-0.5*d
       c2=ak1-0.5*a

       return
       end

*****************************************************************************
*
*     parametre i commonomraade i per.inc settes.
*     variablene betyr det samme som i foorige rutine.
*
*******************************************************************************
       subroutine settkoeff(A,dx,dy,dt,psi,ik)
       integer ik
       real A,dx,dy,dt,psi
       include 'per.inc'
*--------------------------------------------------------------------------
       real c2,ak1,sig

       call berpar(dx,dy,dt,psi,ik,c2,ak1,d,sig)

       if(A*d.gt.0.5) then
         write(*,*)'amplitude for stor'
       else
         alph=2.0*A/(1.0+sqrt(1.0-2.0*A*d))
       end if
       c=1.0 +alph*0.5+alph*alph*c2
       keff0=0.5*sqrt(0.5*A/sig)
       keff=0.5*sqrt(0.5*alph/sig)
       keff=keff*(1.0+ak1*alph)
c
c      keff er slik at argument til sech**2 er keff*(x-ct) der x og t
c      er regnet i normalkoordinater.
c 
       return
       end

****************************************************************************
*
*         Beregner pert. losn. for diskrete solitoner.
*         parameter:
*               x  -  romkoordinat langs forplantningsretning.           I
*                     Denne er dimensjonert med dypet som lengdeskala.
*               app1,app2 - hhv. laveste og neste approx til overfl.hevn O
***************************************************************************
       subroutine solnum(x,app1,app2)
       real app1,app2,x
       include 'per.inc'
*--------------------------------------------------------------------------
       real temp

       temp=cosh(keff0*x)
       temp=1.0/(temp*temp)
       app1= alph*(1.0-0.5*d*alph)*temp
       temp=cosh(keff*x)
       temp=1.0/(temp*temp)
       app2=alph*temp*(1.0-0.5*d*alph*temp)

       return
       end


*************************************************************************
       subroutine listsol(yr,n,ds,x0,xtopp,amp,dx,dy,dt,psi,proj,
     % ik,itape)
       integer n,itape,ik
       real yr(1500),ds,x0,xtopp,amp,dx,dy,dt,psi
       logical proj
*------------------------------------------------------------------------
       include 'per.inc'

       integer i,nsx,ii
       real xver,yfyll(1500),rtopp,cek,ua,app1,app2
       real capp1,err1,err2,cpsi,pi,dsp

       pi=4.0*atan(1.0)
       if(proj) then
         cpsi=cos(pi*psi/180.0)
       else
         cpsi=1.0
       end if

       dsp=ds*cpsi

       nsx=(dx/ds+0.001)
       if(itape.lt.0) return

       rtopp= (xtopp-x0+ds)*cpsi
       call SOLITON(yfyll,1,N,dsp,AMP,rTopp,Cek,UA,1)

       call settkoeff(amp,dx,dy,dt,psi,ik)

       write(itape,*) 4,3
       write(itape,26) amp,dx,dy,dt,psi
 26    format(1x,'#  Pertub. loes. for  A=',f10.5,'   dx=',f10.5,
     % '    dy=',f10.5,'    dt=',f10.5,'   psi=',f10.5)
       capp1= 1.0+0.5*amp
       write(itape,5) capp1,c,cek,keff
 5     format(1x,'#  1.ord. c=',f8.5,'    2.ord. c=',f8.5,
     %'    eks. c=',f8.5,'  btall=',f8.5)
       write(itape,*)'# kolonne 1: x, 2: 1.ord, 3: 2.ord',
     %  ', 4: eks. analyt'


       do 100 i=1,n
       xver=(x0+(i-1)*ds-xtopp)*cpsi
       call solnum(xver,app1,app2)
       xver=xver/cpsi+xtopp
       write(itape,10) xver,app1,app2,yfyll(i)
 10    format(1x,4f12.6)
       if(itape.gt.0) then
         write(itape+10,20) xver,app1
         write(itape+20,20) xver,app2
         write(itape+30,20) xver,yfyll(i)
 20      format(1x,2f12.6)
       end if
       if(nsx.gt.0) then
         ii=1+(i-1)/nsx
         if( (ii-1)*nsx.eq.i-1 ) then
           err1=abs(yr(ii)-app1)
           err2=abs(yr(ii)-app2)
           write(itape+40,20)xver,err1
           write(itape+50,20)xver,err2
         end if
       end if
 100   continue

       return
       end


********************************************************************
       function alamb(a,eps)
       real a,eps,alamb
*--------------------------------------------------
       alamb=-log(0.25*eps)/sqrt(3.0*a)
       return
       end


*********************************************************************
       subroutine listgitt(y,yr,n,m,dx,dt,eps,mv,a,x0,xtopp,ni,ds,ktape)
       integer n,m,mv,ni,ktape
       real y(1:n,1:m),yr(1500),dx,dt,x0,xtopp,ds,a,eps
*------------------------------------------------------------------
       integer na,i,n1,n2,nds
       real yinp(0:1501),xa(20),amp(20),gr,xver
       real xm
       real rtall,alamb
       
       if(ktape.lt.0) return

       do 100 i=1,n
         yr(i)=y(i,mv)
 100     continue

       gr=0.005
       call LIMAX(YR,YINP,N,GR,AMP,XA,NA)
       if(na.lt.1) then
         write(*,*) 'ingen topper funnet, amp. leses'
         a=rtall(0.1,'gi amplitude#')
         xtopp=alamb(a,eps)
         x0=0.0
         ds=0.25
         ni=2.0*xtopp/ds
         return
       end if

       x0=0.0
       nds=dx/0.25+1
       ds=dx/nds
       xm=(xa(1)-1.0)*dx+x0
       a=amp(1)
       n1=(xm-alamb(a,eps))/dx
       n2=(xm+alamb(a,eps)+1.0)/dx
       n1=max(1,n1)
       n2=min(n,n2)
       xtopp=xm-(n1-1)*dx
       ni=(n2-n1+1)*nds

       write(ktape,*) 2,2
       write(ktape,10) dx,dt
 10    format(1x,'#    gitterdata med dx=',f10.5,'   og dt=',f10.5)
       write(ktape,15) a,xtopp
 15    format(1x,'#    ampl=',f10.6,'  funnet ved:',f12.4)

       do 200 i=n1,n2
       xver= x0+(i-n1)*dx
       write(ktape,20) xver,yr(i)
       yr(i+1-n1)=yr(i)
 20    format(2f12.6)
 200   continue


       return
       end

       subroutine konsjekk(v)
       real v(3,2)
       include 'per.inc'
*-----------------------------------------------------------------
       integer i
       real fak2,fak3,app1,app2,y0,xver,yver

       write(*,*)'i,yver,fak2,fak3'
       do 100 i=1,3
       xver=v(i,1)
       yver=v(i,2)
       call solnum(xver,app1,app2)
       y0=1.0/cosh(keff*xver)
       y0=y0*y0
       fak2=(yver-app1)/(alph*alph*(y0*y0-y0))
       fak3=(yver-app2)/(alph*alph*alph*(y0*y0*y0-y0))
       write(*,*)i,yver,fak2,fak3
 100    continue
      return
      end



*****************************************************************************
*
*               S O L I P E R T
*
*
*     Har tisvarende funksjon som soliprgen i solny, men bruker to-ledds
*     perturbasjonsl|sning og beregner alltid overflatehevning. 
*    Endrede/nye parametere:
*                      ds --  increment i array
*                      dx,dy,dt -- gitteravstander,
*                      psi -- vinkel i grader med x-akse
*                      ivis -- =0 det tabuleres normalt kammen
*                              =1 det tabuleres langs x-aksen
*                              =2 det tabuleres langs y-aksen
*                             Topp refererer seg alltid til tabuleringsretning
*                      ik -- =1 angir korreksjon 
**************************************************************************
      subroutine solipert(Y,n0,N,DS,dx,dy,dt,psi,ivis,ik,AMP,TOPP,C,eps)   
      INTEGER n0,N,ivis,ik
      REAL Y(N0:N),DS,AMP,topp,C,UA,eps,dx,dy,dt,psi
*---------------------------------------------------------------------------
      integer i,na,nb
      real dfak,blen,halvb,pi,xver,app1

      pi=4.0*atan(1.0)

      do 50 i=n0,n
         y(i)=0.0
 50   continue

      if(ivis.eq.0) then
        dfak=1.0
      else
        if(ivis.eq.1) then
          dfak=cos(pi*psi/180.0)
        else
          dfak=sin(pi*psi/180.0)
        end if
      end if

      if(amp.le.0.0) return

      blen=halvb(amp,eps)/dfak

      na= (topp-blen)/ds -1
      na=max(na,n0)
      nb=(topp+blen)/ds +1
      nb=min(nb,n)

      call settkoeff(amp,dx,dy,dt,psi,ik)

      do 100 i=na,nb

      xver= (topp-i*ds)*dfak
      call solnum(xver,app1,y(i))

 100  continue

      return

      end
