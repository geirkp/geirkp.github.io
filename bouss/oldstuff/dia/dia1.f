***************************************************************************
*
*              D I G I T
*
*     Unders|ker om karakteren 'c' er et tall
****************************************************************************
      function digit(c)
      character c
      logical digit
*-------------------------------------------------------------------------
      integer idiff                       

      idiff=ichar(c)-ichar('0')
      digit= idiff.ge.0  .and.  idiff.le.9
      return
      end
         

********************************************************************
*
*                 i t c o n v
*
*     Konverterer heltallet 'i' til teksten 't'. 'kf' angir lengden
*     av den konverterte teksten. 
***********************************************************************
      subroutine itconv(i,kf,t)
      integer i,kf
      character*20 t
*---------------------------------------------------------------------
      integer irest,isiff,inull,ichar,isgn,j,jstop
      character c,char

      inull=ichar('0')

      if(i.ge.0) then
        isgn=1
        irest=i
      else
        isgn=-1
        irest=-i
      end if

      kf=0

 100  continue
      kf=kf+1
      isiff=mod(irest,10)
      t(kf:kf)=char(isiff+inull)
      if(kf.eq.20) go to 300
      irest=irest/10
      if(irest.eq.0) go to 200
      go to 100

 200  continue
      if(isgn.lt.0) then
        kf=kf+1
        t(kf:kf)='-'
      end if

 300  continue
 
      jstop=kf/2
      do 400 j=1,jstop
      c=t(j:j)
      t(j:j)=t(kf+1-j:kf+1-j)
      t(kf+1-j:kf+1-j)=c
 400  continue
      
      return
      end
     
*****************************************************************************
*
*                   L O K O R D
*
*    Lokaliserer foerste sekvens i strengen c(n0:n1) og angir den eventuelle
*    posisjonen c(i0:i1). Med sekvens menes en sammenhengende serie ikke-
*    blanke tegn. 
*        iflag=0   : strengen er tom.
*              1   : sekvens som begynner med bokstav funnet. 
*              2   : sekvens som begynner med tall funnet. 
*              3   : sekvens som begynner med annet tegn funnet. 
*
*
**************************************************************************
      subroutine lokord(c,n0,n1,i0,i1,iflag)
      integer n0,n1,i0,i1,iflag
      character c*80
*--------------------------------------------------------------------
      integer i
      character b

      if(n0.gt.n1) then
        iflag=0
        return
      end if

      i=n0
      b=c(i:i)                

  100 continue
      if(b.ne.' ') go to 200
      if(i.eq.n1) then
        iflag=0
        return
      end if
      i=i+1
      b=c(i:i)
      go to 100
  200 continue

      if((b.ge.'a'.and.b.le.'z').or.(b.ge.'A'.and.b.le.'Z'))then
        iflag=1
      else
        if(b.ge.'0'.and.b.le.'9')then
          iflag=2
        else
          iflag=3
        end if
      end if
      i0=i

                                 
  300 continue
      if(b.eq.' ') go to 400
      if(i.eq.n1) then
        i1=n1
        return
      end if
      i=i+1
      b=c(i:i)
      go to 300

  400 continue
      i1=i-1

      return
      end                                                

****************************************************************
*
*                 B L A N K
*
*     C(1:K) fylles med blanke
*
*****************************************************************          
      SUBROUTINE BLANK(C,K)
      INTEGER K
      CHARACTER*80 C
*--------------------------------------------------------------
      INTEGER I

      DO 100 I=1,K
  100 C(i:i)=' '

      RETURN
      END



****************************************************************             
*
*              U P C H A R
*    Gjoer om smaa bokstaver i c(k1:k2) til store.
****************************************************************
      subroutine upchar(c,k1,k2)
      character*80 c
      integer k1,k2
*------------------------------------------------------------------
      integer ila,ilz,isa,ks,iver,i
                        
      ila=ichar('a')
      ilz=ichar('z')
      isa=ichar('A')
      ks=min(80,k2)
      do 100 i=k1,k2
      iver=ichar(c(i:i))                             
      if(iver.ge.ila .and. iver.le.ilz)
     %   c(i:i)=char(isa+iver-ila)
  100 continue
      return
      end


*********************************************************************
*                 L O K I N T                                       *
*   Lokaliserer foerste heltall i strengen c(n0:n1).Ved output      *
*   angir i0 posisjon av foerste ikke-blanke tegn, et eventuelt     *
*   heltall er lokalisert til c(i0:i1). Kontroll parameter:         *
*     iflag=0    : tekst-streng er tom.                             *
*          =1    : ledende heltall etterfulgt av blank er funnet.   *
*          =2    : ledende heltall etterfulgt av  ikke-blank.       *
*          =3    : ensomt fortegn funnet i posisjon n1.             *
*          =4    : foerste ikkebl. tegn ingen mulig del av heltall. *
*                                                                   *
*********************************************************************
      subroutine lokint(c,n0,n1,i0,i1,iflag)
      integer n0,n1,i0,i1,iflag
      character c*80
*--------------------------------------------------------------------
      integer i
      character b

      if(n0.gt.n1) then
        iflag=0
        return
      end if

      i=n0
      b=c(i:i)                

  100 continue
      if(b.ne.' ') go to 200
      if(i.eq.n1) then
        iflag=0
        return
      end if
      i=i+1
      b=c(i:i)
      go to 100
  200 continue
      i0=i

      if(b.eq.'+' .or. b.eq.'-') then
        if(i.eq.n1) then
          iflag=3
          return
        end if
        i=i+1
        b=c(i:i)
      end if

      if(b.lt.'0' .or. b.gt.'9') then
        iflag=4
        return
      end if
                                 
  300 continue
      if(b.lt.'0' .or. b.gt.'9') go to 400
      if(i.eq.n1) then
        i1=n1
        iflag=1
        return
      end if
      i=i+1
      b=c(i:i)
      go to 300

  400 continue
      i1=i-1
      if(b.eq.' ') then
        iflag=1
      else
        iflag=2
      end if

      return
      end




**************************************************************************
*                                                                        *
*                   I C O N                                              *
*                                                                        *
*    Funksjon som regner om tekst t(k1:k2) til et heltall. Det m} p}     *
*  forh}nd v{re testet at alle enkeltbokstaver i sekvensen er tall       *
*  (bortsett fra fortegn) og at antallet ikke er ulovlig stort.          *
**************************************************************************

      function icon(t,k1,k2)
      integer icon,k1,k2
      character*80 t
*--------------------------------------------------------------------------
      integer i,l,n10,null,isum,kk1,isgn

      null=ichar('0')
      n10=1     
      isum=0                       
      
      if(t(k1:k1).eq.'-') then
        kk1=k1+1
        isgn=-1                          
      else                
        isgn=1
        if(t(k1:k1).eq.'+') then
          kk1=k1+1               
        else
          kk1=k1 
        end if
      end if
         
      do 100 i=kk1,k2
      l=k2 -i + kk1 
      isum=isum+(ichar(t(l:l))-null)*n10
      n10=n10*10
  100 continue
      icon=isgn*isum
      return
      end


**************************************************************************
*                                                                        *
*                   I C O NL                                              *
*                                                                        *
*    Funksjon som regner om tekst t(k1:k2) til et heltall. Det m} p}     *
*  forh}nd v{re testet at alle enkeltbokstaver i sekvensen er tall       *
*  (bortsett fra fortegn) og at antallet ikke er ulovlig stort.          *
**************************************************************************

      function iconl(t,k1,k2)
      integer*8 iconl
      integer k1,k2
      character*80 t
*--------------------------------------------------------------------------
      integer i,l,null,kk1,isgn
      integer*8 isum,n10

      null=ichar('0')
      n10=1     
      isum=0                       
      
      if(t(k1:k1).eq.'-') then
        kk1=k1+1
        isgn=-1                          
      else                
        isgn=1
        if(t(k1:k1).eq.'+') then
          kk1=k1+1               
        else
          kk1=k1 
        end if
      end if
         
      do 100 i=kk1,k2
      l=k2 -i + kk1 
      isum=isum+(ichar(t(l:l))-null)*n10
      n10=n10*10
  100 continue
      iconl=isgn*isum
      return
      end
         



*********************************************************************** 
*                                                                     *
*                 G E T I                                             *
*   Samme anvendelse som 'lokint' men returnerer med verdi av evnt.   *
*   heltall i parameter 'ires'                                        *
*                                                                     *   
*   kaller 'lokint','icon'                                                   *
***********************************************************************
      SUBROUTINE GETI(C,N0,N1,I0,I1,iRES,IFLAG)
      character*80 c
      integer n0,n1,i0,i1,ires,iflag
*------------------------------------------------
      integer icon

      call lokint(c,n0,n1,i0,i1,iflag)
      if(iflag.eq.1 .or. iflag.eq.2)then
        if(i1-i0.gt.9) then
          write(0,*)'for mange siffer >9'
          iflag=6
        else
          ires=icon(c,i0,i1)
        end if
      end if
      return
      end


*********************************************************************** 
*                                                                     *
*                 G E T L                                             *
*   Samme anvendelse som 'GETI' men gir 8 bytes ires                            *                                                                     *   
*   kaller 'lokint','icon'                                                   *
***********************************************************************
      SUBROUTINE GETL(C,N0,N1,I0,I1,iRES,IFLAG)
      character*80 c
      integer n0,n1,i0,i1,iflag
      integer*8 ires
*------------------------------------------------
      integer*8 iconl

      call lokint(c,n0,n1,i0,i1,iflag)
      if(iflag.eq.1 .or. iflag.eq.2)then
        if(i1-i0.gt.17) then
          write(0,*)'for mange siffer'
          iflag=6
        else
          ires=iconl(c,i0,i1)
        end if
      end if
      return
      end


**************************************************************************
*                                                                        *
*                   G E T R                                              *
*   Real versjon av 'geti'. 'iflag'=1,2,3,4 som for 'geti', 'iflag'=6    *
*   angir for stor eksponent.                                            *
*                                                                        *
*   kaller 'geti'                                                        *
**************************************************************************
      subroutine getr(c,n0,n1,i0,i1,res,iflag)
      character*80 c
      integer n0,n1,i0,i1,iflag
      real res
*-------------------------------------------------------------------------
      character b
      integer num,nn0,ia,i,kflag,ie,ib,isgn
      real des
                                           
      isgn=1
      call geti(c,n0,n1,i0,i1,num,iflag)
      if(iflag.eq.3 .or. iflag.eq.0) return
      b=c(i0:i0)
      if(b.eq.'-') isgn=-1
      if(iflag.eq.4) then                
        nn0=i0+1
        if(b.eq.'-'  .or.  b.eq.'+') then
          b=c(nn0:nn0)
          nn0=nn0+1   
        end if
        if(b.ne.'.') return
        res=0.0
        call geti(c,nn0,n1,ia,ib,num,kflag)
        if(kflag.gt.2. .or. kflag.eq.0 .or. ia.gt.nn0) return
        b=c(ia:ia)
      else    
        res=num*1.0
        if(iflag.ne.2) then
          return   
        end if
  
        b=c(i1+1:i1+1)
        if(b.eq.'e' .or. b.eq.'E') then
          ie=i1+1
          go to 300 
        end if

        if(b.ne.'.') return
        i1=i1+1
        nn0=i1+1
        call geti(c,nn0,n1,ia,ib,num,kflag)

        if(kflag.eq.0 .or. ia.gt.nn0) then
          iflag=1
          return
        end if  

        b=c(nn0:nn0)

        if(kflag.gt.2)  then
          if(b.eq.'e' .or. b.eq.'E') then
            ie=nn0
            go to 300
          else
            return
          end if
        end if
 
      end if

      if(b.eq.'-' .or. b.eq.'+') return

      i1=ib
      des=num*1.0*isgn
      do 100 i=ia,i1     
  100 des=des*0.1
      
      res=res+des

      if(kflag.eq.1) then
        iflag=1
        return
      end if
      
      ie=i1+1
  300 b=c(ie:ie) 
      if(b.ne.'e' .and. b.ne.'E') then
        iflag=2
        return
      end if
                               
      nn0=ie+1
      call geti(c,nn0,n1,ia,i1,num,kflag)
      if(kflag.eq.0 .or. kflag.gt.2 .or. ia.gt.nn0) then
        iflag=2
        return
      end if                      

      if(abs(num).gt.99) then
        write(0,*)'for stor eksponent'
        iflag=6
        return
      end if
   

      iflag=kflag
      res=res*10.0**(num*1.0)
      return
      end



**************************************************************************
*                                                                        *
*                   G E T R L                                              *
*   Real versjon av 'geti'. 'iflag'=1,2,3,4 som for 'geti', 'iflag'=6    *
*   angir for stor eksponent.                                            *
*                                                                        *
*   kaller 'geti'                                                        *
**************************************************************************
      subroutine getrl(c,n0,n1,i0,i1,res,iflag)
      character*80 c
      integer n0,n1,i0,i1,iflag
      real res
*-------------------------------------------------------------------------
      character b
      integer num,nn0,ia,i,kflag,ie,ib,isgn
      integer*8 numl
      real des
                                           
      isgn=1
      call geti(c,n0,n1,i0,i1,num,iflag)
      if(iflag.eq.3 .or. iflag.eq.0) return
      b=c(i0:i0)
      if(b.eq.'-') isgn=-1
      if(iflag.eq.4) then                
        nn0=i0+1
        if(b.eq.'-'  .or.  b.eq.'+') then
          b=c(nn0:nn0)
          nn0=nn0+1   
        end if
        if(b.ne.'.') return
        res=0.0
        call geti(c,nn0,n1,ia,ib,num,kflag)
        if(kflag.gt.2. .or. kflag.eq.0 .or. ia.gt.nn0) return
        b=c(ia:ia)
      else    
        res=num*1.0
        if(iflag.ne.2) then
          return   
        end if
  
        b=c(i1+1:i1+1)
        if(b.eq.'e' .or. b.eq.'E') then
          ie=i1+1
          go to 300 
        end if

        if(b.ne.'.') return
        i1=i1+1
        nn0=i1+1
        call getl(c,nn0,n1,ia,ib,numl,kflag)

        if(kflag.eq.0 .or. ia.gt.nn0) then
          iflag=1
          return
        end if  

        b=c(nn0:nn0)

        if(kflag.gt.2)  then
          if(b.eq.'e' .or. b.eq.'E') then
            ie=nn0
            go to 300
          else
            return
          end if
        end if
 
      end if

      if(b.eq.'-' .or. b.eq.'+') return

      i1=ib
      des=numl*1.0*isgn
      do 100 i=ia,i1     
  100 des=des*0.1
      
      res=res+des

      if(kflag.eq.1) then
        iflag=1
        return
      end if
      
      ie=i1+1
  300 b=c(ie:ie) 
      if(b.ne.'e' .and. b.ne.'E') then
        iflag=2
        return
      end if
                               
      nn0=ie+1
      call geti(c,nn0,n1,ia,i1,num,kflag)
      if(kflag.eq.0 .or. kflag.gt.2 .or. ia.gt.nn0) then
        iflag=2
        return
      end if                      

      if(abs(num).gt.99) then
        write(0,*)'for stor eksponent'
        iflag=6
        return
      end if
   

      iflag=kflag
      res=res*10.0**(num*1.0)
      return
      end



               

************************************************************************
*                                                                      *
*                      S T R I P                                       *
*                                                                      *
*    Finner den del, tekst(1:k), som ikke er blank. kmax angir total   *
*    lengde av tekst. Dersom tekst er tom returneres med k=0.
************************************************************************

      subroutine strip(tekst,kmax,k)
      integer k,kmax
      logical def
      character*200 tekst
*-----------------------------------------------------------------------
      integer i                                                         

      k=kmax

  100 continue
      if( tekst(k:k).ne.' ') go to 200
      if(k.eq.1) then
        k=0
        return
      end if
      k=k-1
      go to 100
  200 continue
      return
      end



************************************************************************
*                                                                      *
*                      F R O N T S T R I P                             *
*                                                                      *
*    Fjerner innledende blanke i tekst(1:kmax), kmax angir total          *
*    lengde av tekst. k er antall blanke som er funnet.
************************************************************************

      subroutine frontstrip(tekst,kmax,k)
      integer kmax
      character*200 tekst
*-----------------------------------------------------------------------
      integer k,i             

      k=1

  100 continue
      if( tekst(k:k).ne.' ') go to 200
      if(k.eq.kmax) return
      k=k+1
      go to 100
  200 continue

      do 300 i=k,kmax
      tekst(i+1-k:i+1-k)=tekst(i:i)
 300  continue

      k=k-1
      
      return
      end




************************************************************************
*                                                                      *
*                      X S T R I P                                     *
*                                                                      *
*    Finner den del, tekst(1:k), som ligger foran tegnet '#' eller '!'.*
*    def=true angir '#', def=false angir '!'. Dersom hverken '#' eller *
*    '!' finnes settes k=80 og def=true.                               *
************************************************************************

      subroutine xstrip(tekst,k,def)
      integer k
      logical def
      character*80 tekst
*-----------------------------------------------------------------------
      integer i                                                         
      character b

      k=1
      b=tekst(1:1)
  100 continue
      if( b.eq.'!' .or. b.eq.'#') go to 200
      if(k.eq.80) then
        def=.true.
        return
      end if
      k=k+1
      b=tekst(k:k)
      go to 100
  200 k=k-1
      def=.not.b.eq.'!'
      return
      end


************************************************************************
*                                                                      *
*                      X S T R I P G                                    *
*                                                                      *
*    Finner den del, tekst(1:k), som ligger foran tegnet '#' eller '!'.*
*    def=true angir '#', def=false angir '!'. Dersom hverken '#' eller *
*    '!' finnes settes k=kmax og def=true.                               *
************************************************************************

      subroutine xstripg(tekst,k,kmax,def)
      integer k,kmax
      logical def
      character*200 tekst
*-----------------------------------------------------------------------
      integer i                                                         
      character b

      k=1
      b=tekst(1:1)
  100 continue
      if( b.eq.'!' .or. b.eq.'#') go to 200
      if(k.eq.kmax) then
        def=.true.
        return
      end if
      k=k+1
      b=tekst(k:k)
      go to 100
  200 k=k-1
      def=.not.b.eq.'!'
      return
      end




************************************************************************
*
*                    D E L T E K
*
*    sekvenser i t(na:nb) lokaliseres. iant angir antall sekvenser,sekvens
*    k er lokalisert til t( l(1,k):l(2,k) ).
*
*    kaller 'lokord'
*****************************************************************************
      subroutine deltek(t,na,nb,l,iant)
      integer na,nb,l(2,20),iant
      character*80 t
*-------------------------------------------------------------------------
      integer k1,k2,iflag,n0

      n0=na
      iant=0  

  100 continue
      call lokord(t,n0,nb,k1,k2,iflag)
      if(iflag.eq.0) return
      n0=k2+1     
      iant=iant +1
      l(1,iant)=k1
      l(2,iant)=k2
      go to 100
c     return
      end        



************************************************************************     
*                      R E D
*
*    ordner teksten t(1:nb) slik at den faar form: 'sekvens1 sekvens2 ...
*     sekvensN#' ,der n er antall sekvenser.  
*
*    kaller lokord.
*************************************************************************
      subroutine red(t,nb)
      integer nb
      character*80 t
*-------------------------------------------------------------------------
      integer k1,k2,iflag,n0,kfyll,ka,iant,i

      n0=1
      kfyll=0
      ka=1                                
      iant=0

  100 continue
      call lokord(t,n0,nb,k1,k2,iflag)
      if(iflag.eq.0) then
        kfyll=kfyll+1
        if(kfyll.le.nb) t(kfyll:kfyll)='#'      
        do 75 i=kfyll+1,nb
   75   t(i:i)=' '
        return
      end if
      n0=k2+1
      iant=iant+1
      if(iant.gt.1) then
        t(kfyll+1:kfyll+1)=' '
        ka=2
      end if
      t(kfyll+ka:kfyll+ka+k2-k1)=t(k1:k2)
      kfyll=kfyll+k2-k1+ka
      go to 100
c      return
      end




********************************************************************     
*
*                 A L A C A R T
*
*   sjekker hvor mange tekster i menyen t(n) streng(na:nb) er en
*   forkortelse av. Tekstene i t(n) maa avsluttes med '#' eller '!'.
*   iant er antall treff, l(1)..l(iant) er nummerne i menyen t for
*   tekstene som er truffet. 
*
*   kaller 'fork'
***********************************************************************
      subroutine alacart(t,n,streng,na,nb,l,iant)
      integer n,na,nb,iant,l(n)
      character*80 streng,t(n)
*-------------------------------------------------------------------
      integer i   
      logical fork
      
      iant=0  
      do 100 i=1,n   
      if(fork(t(i),streng,na,nb)) then
        iant=iant+1
        l(iant)=i
      end if
  100 continue
      return
      end

********************************************************************     
*
*                 V E L G C A R T
*
*   Funksjon som alacart, men returnerer med bare ett treff dersom
*   streng(na,nb) er identisk lik en av tekstene i t.
*
*   kaller 'fork'
***********************************************************************
      subroutine velgcart(t,n,streng,na,nb,l,iant)
      integer n,na,nb,iant,l(n)
      character*80 streng,t(n)
*-------------------------------------------------------------------
      integer i,kf
      logical fork,paelm
      
      iant=0  
      do 100 i=1,n   
      if(fork(t(i),streng,na,nb)) then
        call xstrip(t(i),kf,paelm)
        if(kf.eq.(nb-na+1)) then
          iant=1
          l(1)=i
          return
        end if
        iant=iant+1
        l(iant)=i
      end if
  100 continue
      return
      end



********************************************************************     
*
*                 B A L A C
*
*   sjekker hvor mange tekster i menyen t(n) streng(na:nb) er en
*   forkortelse av. Tekstene i t(n) maa avsluttes med '#' eller '!'.
*   iant er antall treff, l(1)..l(iant) er nummerne i menyen t for
*   tekstene som er truffet. Det skilles ikke mellom sm} og store 
*   bokstaver.
*
*   kaller 'fork'.
***********************************************************************
      subroutine balac(t,n,streng,na,nb,l,iant)
      integer n,na,nb,iant,l(n)
      character*80 streng,t(n)
*-------------------------------------------------------------------
      integer i  
      logical fork
      
      iant=0  
      do 100 i=1,n   
        if(fork(t(i),streng,na,nb)) then
          iant=iant+1
          l(iant)=i
        end if
  100 continue
      return
      end




*******************************************************************          
*
*                  F O R K
*
*   tester om s(n1:n2) er en forkortelse av t(1:...). Teksten i t maa
*   avsluttes av '#' eller '!'. Testen virker uavhengig av smaa/store
*   bokstaver.
*
*   kaller 'xstrip','upchar'.
**********************************************************************
      function fork(t,s,n1,n2)
      integer n1,n2
      character*80 t,s
      logical fork
*--------------------------------------------------------------------
      integer kt,ks
      character*80 tt,ss
      logical def

      call xstrip(t,kt,def)    
      ks=n2-n1+1
      if(ks.gt.kt) then
        fork=.false.
        return
      end if

      tt(1:ks)=t(1:ks) 
      ss(1:ks)=s(n1:n2)
      call upchar(tt,1,ks)
      call upchar(ss,1,ks)
      fork= tt(1:ks).eq.ss(1:ks)

      return
      end            



****************************************************************
*                  
*                  FSOK
*
*     Leter fra gitt poisjon i en tekst til n{rmeste forekomst av
*     en gitt karakter.
*
*     parametere:
*           t(1:nmax) - tekst det s|kes i                              I
*           np - ved input: posisjon i t sIket starter fra,            I/O
*                         NB. dvs en finnner n{rmeste forekomst til
*                         utenom posisjonen selv,forekomster i endene
*                         av tekstem m} da spesialbehandles.
*                ved output: posisjon der karakter er funnet.
*                      Dersom ingen forekomst finnes returneres med
*                      np=1 eller np=nmax, avh. av ivis, og iflag=1
*                forekomst i np teller ikke
*           c - karakter som s|kes                                     I
*           ivis - =1 : forover s|k  , ellers bakover s|k              I
*           iflag - feilparameter                                      O
*                   =0 alt ok
*                    1  se ovenfor
*                    2  np er ulovlig ved input
*********************************************************************
      subroutine fsok(t,nmax,np,c,ivis,iflag)
      integer np,nmax,ivis,iflag
      character c
      character*300 t
*------------------------------------------
      integer iadd


      iflag=0

      if(np.lt.1 .or. np.gt.nmax ) then
        iflag=2
        return
      end if

      if(ivis.eq.1) then
        iadd=1
        if (np.eq.nmax) return
      else
        iadd=-1
        if (np.eq.1) return
      end if

 100  continue
      np=np+iadd
      if(np.lt.1 .or. np.gt.nmax )then
        if(np.lt.1) np=1
        if(np.gt.nmax) np=nmax
        iflag=1
        return
      end if      

      if(t(np:np).ne.c) go to 100

      return
      end

****************************************************************
*
*                 INSUBST
*
*   skifter ut foerste forekomst av en gitt karakter med et tall
*   parametere:
*            t - tekst som behandles, maa avsluttes med # el !
*            nmax - max tillat lengde av t
*            np - input: punkt der sok starter, output:
*                 forste posisjon etter insatt tal
*            c - karakter som skal byttes
*            ival - heltall som skal settes inn
*            iflag - feilparameter
*                    =0 subst. gjennomfort
*                    =1 ingen forekomst av c
*                    =2 np for stor
*                    =3 resulterende tekst for lang - intet gjort
****************************************************************
      subroutine insubst(t,nmax,np,c,ival,iflag)
      integer nmax,np,ival,iflag
      character c
      character*80 t
*----------------------------------------------------------------
      integer kf,kt,irest,i,np1
      logical hiv
      character*20 ctall
  

      call xstrip(t,kf,hiv)

      call gsok(t,kf,np,c,1,iflag)
      if(iflag.ne.0) return

      irest=kf-np
      call itconv(ival,kt,ctall)

      if( (kf+kt-1).ge.nmax) then
        iflag=3
        return
      end if

      np1=np+1

c     avsl merke maa med, derfor kf+1 som lokke-grense

      do 100 i=kf+1,np1,-1
      t(i+kt-1:i+kt-1)=t(i:i)
100   continue

      t(np:np+kt-1)=ctall(1:kt)
      np=np+kt

      return
      end
      

****************************************************************
*                  
*                  GSOK
*
*     Leter fra gitt poisjon i en tekst til n{rmeste forekomst av
*     en gitt karakter. Forskjellig fra fsok ved betyd av inp.par np
*
*     parametere:
*           t(1:nmax) - tekst det s|kes i                              I
*           np - ved input:  sIket starter fom. denne posisjon i t     I/O
*                ved output: posisjon der karakter er funnet.
*                      Dersom ingen forekomst finnes returneres med
*                      np=1 eller np=nmax, avh. av ivis, og iflag=1
*                dersom ingen forekomst finnes returneres med np uendret
*           c - karakter som s|kes                                     I
*           ivis - =1 : forover s|k  , ellers bakover s|k              I
*           iflag - feilparameter                                      O
*                   =0 alt ok
*                    1  se ovenfor
*                    2  np er ulovlig ved input
*********************************************************************
      subroutine gsok(t,nmax,np,c,ivis,iflag)
      integer np,nmax,ivis,iflag
      character c
      character*120 t
*------------------------------------------
      integer iadd,np0


      np0=np
      iflag=0

      if(np.lt.1 .or. np.gt.nmax ) then
        iflag=2
        return
      end if

      if(ivis.eq.1) then
        iadd=1
      else
        iadd=-1
      end if

      np=np-iadd

 100  continue
      np=np+iadd

      if(np.lt.1 .or. np.gt.nmax )then
        np=np0
        iflag=1
        return
      end if      

      if(t(np:np).ne.c) go to 100

      return
      end



***************************************************************************
*           K O M P R E S S
*
*     Alle blanke i t(n1:n2) som befinner seg ved siden av karakteren 
*     gitt ved c fjernes og den komprimerte teksten venstrestilles og 
*     returneres med oppdatert verdi for n2.
***************************************************************************
      subroutine kompress(t,c,n1,n2)
      integer n1,n2
      character c
      character*80 t
*-----------------------------------------------------------------------
      integer ipos,i1,iblankl,iblankr,k,i,iskip
      logical eks 


      i1=n1

 100  continue

      call lokchar(t,c,i1,n2,k,eks)
      
      if(.not.eks) return
    

      iblankl=0
 40   continue
      if(k-iblankl.eq.n1) go to 45
      if(t(k-iblankl-1:k-iblankl-1).ne.' ') go to 45
      iblankl=iblankl+1
      go to 40

 45   continue

      iblankr=0
 50   continue
      if(k+iblankr.eq.n2) go to 55
      if(t(k+iblankr+1:k+iblankr+1).ne.' ') go to 55
      iblankr=iblankr+1
      go to 50

 55   continue

      ipos= k-iblankl
      t(k:k)=' '
      t(ipos:ipos)=c

      i1=ipos+1
      iskip=iblankl+iblankr
      ipos=ipos+iskip+1
      do 70 i=ipos,n2
      t(i-iskip:i-iskip)=t(i:i)
 70   continue

      n2=n2-iskip

      go to 100

      end


******************************************************************
*     
*      Naar karakteren lik cjok forekommer, har dette samme virkning
*      som om tallet jokver sto der i stedet. Ellers virker rutinen
*      p} samme vis som geti. Det s|kes i strengen t(ia:ib)
*******************************************************************
      subroutine GETIJOK(t,cjok,jokver,ia,ib,I0,I1,ires,kFLAG)
      integer jokver,ia,ib,I0,I1,ires,kFLAG
      character cjok
      character*80 t
*----------------------------------------------------------------------

      call GETI(t,ia,ib,I0,I1,ires,kFLAG)

      if(kflag.eq.4) then
        if(t(i0:i0).eq.cjok) then
          ires=jokver
c         posisjonsparametere ma na justeres og kflag maa
c        gis verdi som om det sto jokver i stedet for cjok
          i1=i0
          if(i1.eq.ib) then
            kflag=1
          else
            if(t(i1+1:i1+1).eq.' ')then
              kflag=1
            else
              kflag=2
            end if
          end if
        end if
      end if

      return
      end


************************************************************************
*
*     Markerer tall i intervallet 1:nmax, gitt ved sekvenser i t(n1:n2). 
*     Eksempler p} sekvenser
*
*           5     -  posisjon 5 markeres
*           2:5   -  posisjoner 2,3,4 og 5 markeres. Dersom 5 er st|rre
*                    enn nmax markeres bare tom. nmax
*           2:11;2 - posisjoner 2,4,6,8 og 10 markeres, igjen bare tom. nmax
*
*     Det kan gis et vilk}rlig antall sekvenser og blanke omkring : og ; 
*     neglisjeres. Karakteren * betyr nmax og kan benyttes i alle sammen-
*     stillinger.
*     Parametere:
*            t(n1:n2) - tekst med sekvenser                              I
*            nmax    - ovre grense for tall                              I
*            med   -  Dersom med(j) er sann er j markert                 O
*            iflag -  feilparameter                                      O
*                   =0   alt i orden
*                    1   funn av sekvens som begynner ulovlig
*                    2   karakteren ':' er ikke funnet som forventet
*                    3   karakteren ':' er ikke fulgt av heltall ( eller *)
*                    6   karakteren ';' er ikke funnet som forventet
*                    7   karakteren ';' er ikke fulgt av heltall ( eller *)
******************************************************************************

      subroutine konvert(t,n1,n2,nmax,med,iflag)
      integer n1,n2,nmax,iflag
      character*80 t
      logical med(nmax)
*------------------------------------------------------------------------
      integer l(2,20),iant,ia,ib,khigh,kstep,knum,k
      integer j,i0,i1,kflag

      iflag=0

      call kompress(t,':',n1,n2)
      call kompress(t,';',n1,n2)

      call deltek(t,n1,n2,l,iant)

      if(iant.eq.0) then
        do 50 k=1,nmax
        med(k)=.true.
 50     continue
        return
      else
        do 60 k=1,nmax
        med(k)=.false.
 60     continue
      end if

      do 100 j=1,iant
      ia=l(1,j)
      ib=l(2,j)
      call GETIJOK(t,'*',nmax,ia,ib,I0,I1,knum,kFLAG)

      if(kflag.gt.2) then
        iflag=1
        return
      end if

      if(kflag.eq.1)  then
        if(knum.le.nmax) med(knum)=.true.
      else
        if(t(i1+1:i1+1).ne.':') then
          iflag=2
          return
        end if
        ia=i1+2
        call GETIJOK(t,'*',nmax,ia,ib,I0,I1,khigh,kFLAG)
        if(kflag.eq.0 .or.kflag.eq.3) then
           iflag=3
           return
        end if


        if(kflag.eq.1) then
          kstep=1
        else
          if(t(i1+1:i1+1).ne.';') then
            iflag=6
            return
          end if
  
          ia=i1+2
          call GETI(t,ia,ib,I0,I1,kstep,kFLAG)
          if(kflag.ne.1) then
            iflag=7
            return
          end if
 
        end if

        if(khigh.gt.nmax) khigh=nmax

        do 70 k=knum,khigh,kstep
        med(k)=.true.
 70     continue

      end if
 100  continue

      return
      end
          


************************************************************************
*
*     Markerer tall i intervallet n0:nmax, gitt ved sekvenser i t(n1:n2). 
*     Eksempler p} sekvenser
*
*           5     -  posisjon 5 markeres
*           2:5   -  posisjoner 2,3,4 og 5 markeres. Dersom 5 er st|rre
*                    enn nmax markeres bare tom. nmax
*           2:11;2 - posisjoner 2,4,6,8 og 10 markeres, igjen bare tom. nmax
*
*     Det kan gis et vilk}rlig antall sekvenser og blanke omkring : og ; 
*     neglisjeres. Karakteren * betyr nmax og kan benyttes i alle sammen-
*     stillinger.
*     Parametere:
*            t(n1:n2) - tekst med sekvenser                              I
*            nmax    - ovre grense for tall                              I
*            med   -  Dersom med(j) er sann er j markert                 O
*            iflag -  feilparameter                                      O
*                   =0   alt i orden
*                    1   funn av sekvens som begynner ulovlig
*                    2   karakteren ':' er ikke funnet som forventet
*                    3   karakteren ':' er ikke fulgt av heltall ( eller *)
*                    6   karakteren ';' er ikke funnet som forventet
*                    7   karakteren ';' er ikke fulgt av heltall ( eller *)
******************************************************************************

      subroutine gkonvert(t,n1,n2,n0,nmax,med,iflag)
      integer n1,n2,n0,nmax,iflag
      character*80 t
      logical med(n0:nmax)
*------------------------------------------------------------------------
      integer l(2,20),iant,ia,ib,khigh,kstep,knum,k
      integer j,i0,i1,kflag

      iflag=0

      call kompress(t,':',n1,n2)
      call kompress(t,';',n1,n2)

      call deltek(t,n1,n2,l,iant)

      if(iant.eq.0) then
        do 50 k=n0,nmax
        med(k)=.true.
 50     continue
        return
      else
        do 60 k=n0,nmax
        med(k)=.false.
 60     continue
      end if

      do 100 j=1,iant
      ia=l(1,j)
      ib=l(2,j)
      call GETIJOK(t,'*',nmax,ia,ib,I0,I1,knum,kFLAG)

      if(kflag.gt.2) then
        iflag=1
        return
      end if

      if(kflag.eq.1)  then
        if(knum.le.nmax .and. knum.ge.n0) med(knum)=.true.
      else
        if(t(i1+1:i1+1).ne.':') then
          iflag=2
          return
        end if
        ia=i1+2
        call GETIJOK(t,'*',nmax,ia,ib,I0,I1,khigh,kFLAG)
        if(kflag.eq.0 .or.kflag.eq.3) then
           iflag=3
           return
        end if


        if(kflag.eq.1) then
          kstep=1
        else
          if(t(i1+1:i1+1).ne.';') then
            iflag=6
            return
          end if
  
          ia=i1+2
          call GETI(t,ia,ib,I0,I1,kstep,kFLAG)
          if(kflag.ne.1) then
            iflag=7
            return
          end if
 
        end if

        if(khigh.gt.nmax) khigh=nmax

        do 70 k=knum,khigh,kstep
        if(k.ge.n0)med(k)=.true.
 70     continue

      end if
 100  continue

      return
      end
          


***************************************************************************
*
*                SKIPKOM
*
*    Rutinen skipper alle linjer som ikke starter med tall.
*    Den er ment for innlesning av filer i gkurv-format, og bruker
*    "getr" for aa gjenkjenne tall. 
*
*    parametere:
*             n - antall forbigaatte records                        O
*             itape - unitnummer                                    I
*             ierr -  feilparameter                                 O
*                  =0   alt klart for lesning av tallkolonner
*                  =1   EOF funnet under skipping                   
*                  =2   feil i innlesning under skipping
*                  =3   linje funnet som har feil format, men som
*                       ville blitt godkjent av gkurv
**************************************************************************
      subroutine skipkom(n,itape,ierr)
      integer n,itape,ierr
*-------------------------------------------------------------------------
      integer iflag,i0,i1
      real r
      character*80 t

      n=0

 100  continue
      read(itape,'(a80)',err=400,end=300) t(1:80)

      call getrl(t,1,80,i0,i1,r,iflag)

      if(iflag.eq.1) then
        ierr=0
        backspace itape
        return   
      end if

      if(iflag.eq.2 .or. iflag.eq.6) then
        ierr=3
        backspace itape
        return
      end if

      n=n+1
      go to 100


 300  continue

      ierr=1
      return

 400  continue
      ierr=2

      return

      end

***************************************************************************
*
*                SLOPSKIP
*
*    Rutinen skipper alle linjer som ikke starter med tall.
*    Den er ment for innlesning av filer i gkurv-format, og bruker
*    lokint for aa gjenkjenne tall. Mer sloppy en skipkom. 
*
*    parametere:
*             n - antall forbigaatte records                        O
*             itape - unitnummer                                    I
*             ierr -  feilparameter                                 O
*                  =0   alt klart for lesning av tallkolonner
*                  =1   EOF funnet under skipping                   
*                  =2   feil i innlesning under skipping
**************************************************************************
      subroutine slopskip(n,itape,ierr)
      integer n,itape,ierr
*-------------------------------------------------------------------------
      integer iflag,i0,i1
      real r
      character*80 t

      n=0

 100  continue
      read(itape,'(a80)',err=400,end=300) t(1:80)

      call lokint(t,1,80,i0,i1,iflag)

      if(iflag.eq.1 .or. iflag.eq.2) then
        ierr=0
        backspace itape
        return   
      end if


      n=n+1
      go to 100


 300  continue

      ierr=1
      return

 400  continue
      ierr=2

      return

      end

*********************************************************************
*
*             WRIBCHAR
*
*    Rutinen representerer tegnene i teksten t ved sine ascinummer som
*    saa skrives ut uformattert som 32 bits heltall. Hensikten er kunne
*    bringe tekster helskinnet gjennom byte-swap operasjoner.
*    parametere: 
*            t(1:n) - tekst som skal skrives ut                I
*            itape - unitnummer for utskrift                   I
*            iflag - feilparameter                             O
*                  = 0 alt ok.
*                  = 1 for stor n
*                  = 2 feil unders skrivning
**********************************************************************
      subroutine wribchar(t,n,itape,iflag)
      integer n,itape,iflag
      character*200 t
*----------------------------------------------------------------------
      integer i,nr(200),ichar

      if(n.gt.200) then
        iflag=1
        return
      end if


      do 100 i=1,n
      nr(i)=ichar(t(i:i))
 100  continue

      write(itape,err=200)(nr(i) , i=1,n)

      iflag=0
      return

 200  iflag=2
      return

      end

*********************************************************************
*
*             REABCHAR
*
*    Rutinen leser inn uformatterte 32 bits heltall som saa konverteres til
*    tegn i hht. til ascii-nummeret. Rutinen er beregnet for aa lese det som
*    wribchar skriver.
*    parametere: 
*            t    - tekst for plassering av kar.                  O
*            n - antall tegn                                      I
*            itape - unitnummer for lesning                       I
*            iflag - feilparameter                                O
*                  = 0 alt ok.
*                  = 1 for stor n
*                  = 2 feil under lesning
*                  = 3 slutt paa fil itape
*                  = 4 funnet ascii.nr. utenfor 0...127. Det tilhorende 
*                      tegn settes da til char(0)
**********************************************************************
      subroutine reabchar(t,n,itape,iflag)
      integer n,itape,iflag
      character*200 t
*----------------------------------------------------------------------
      integer i,nr(200),nc
      character char

      if(n.gt.200) then
        iflag=1
        return
      end if

      iflag=0

      read(itape,err=200,end=300)(nr(i) , i=1,n)


      do 100 i=1,n
      nc=nr(i)
      if(nc.lt.0 .or. nc.gt.127) then
        iflag=4
        nc=0
      end if
      t(i:i)=char(nc)
 100  continue


      return

 200  iflag=2
      return

 300  iflag=3
      return

      end












