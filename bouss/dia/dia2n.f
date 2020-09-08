*******************************************************************************
*
*                   I T A L L 
*
*    Leser svar paa spoersmaal i teksten spor. Avsluttes dette med '#' tilbys
*    standardverdi idef som oppnaes ved returnering av tom linje. Avslutting
*    med '!' foerer til ignorering av standardverdi idef. Inngaar verken
*    '#' eller '!' i spor antas det aa ha lengde 80 og standardverdi tilbys.
*
*    kaller 'primles','xstrip' og 'geti'.
*******************************************************************************
      function itall(idef,spor)
      integer itall,idef
      character*80 spor
      include 'styr.inc'
*---------------------------------------------------------
      character*80 svar
      integer k,i1,i2,iflag,ib,ires,iverdi
      logical def        

      call xstrip(spor,k,def)
      if(k.eq.0) k=1                           

  200 continue
    
      if(def) then
        write(iustrr,*) spor(1:k) ,'/', idef ,'/'
        if(aktlog.eq.1 .and. aktkom.eq.1)
     %  write(ltape,*) charkom,spor(1:k) ,'/', idef ,'/'
      else
        write(iustrr,*) spor(1:k)
        if(aktlog.eq.1 .and. aktkom.eq.1)
     %     write(ltape,*) charkom,spor(1:k) 
      end if

      call primles(svar,iflag)

      if(iflag.eq.2) stop
      if(iflag.ne.0) go to 200

      call geti(svar,1,80,i1,i2,ires,iflag)
      if(iflag.eq.0) then
        if(def) then
          iverdi=idef
          go to 500
        else
          call primvri('No default value#')
          go to 200
        end if
      end if
      
      if(iflag.gt.1) then
        call primvri('illegal input#')
        go to 200
      else
        iverdi=ires
        ib=i2+1
        call geti(svar,ib,80,i1,i2,ires,iflag)
        if(iflag.ne.0) then
          call primvri('Please give a single integer#')
          go to 200
        end if
      end if

 500  continue

      itall=iverdi
      dispens=0

      return
      end



**************************************************************************
*
*              SETPA
*    Fyller homedir, defdir og hosttype inn i commonomraadet direk og setter
*    iustrr i com. komstyr
********************************************************************
      subroutine setpa
      include 'styr.inc'
*-------------------------------------------------------
      integer kf
      open(unit=20,file='scrp.xx',status='unknown')
      write(20,'(a)')'#!/bin/sh'
      write(20,'(a)')'echo $HOME > qrw.zzz'
      write(20,'(a)')'pwd >> qrw.zzz'
      write(20,'(a)')'hosttype >> qrw.zzz'
      close(20)
      call system('chmod +x scrp.xx ; ./scrp.xx ')
      open(unit=20,file='qrw.zzz',status='unknown')
      call blank(homedir,120)
      read(20,'(a120)') homedir(1:120)
      call strip(homedir,120,nhd)
      nhd=nhd+1
      homedir(nhd:nhd)='/'
      call blank(defdir,120)
      read(20,'(a120)') defdir(1:120)
      call strip(defdir,120,ndd)
      ndd=ndd+1
      defdir(ndd:ndd)='/'
      read(20,'(a20)') hostt(1:20)
      call strip(hostt,20,kf)
      if(hostt(1:kf).eq.'hp') then
        iustrr=7
      else
        iustrr=0
      end if

      call system('rm -f scrp.xx qrw.zzz ')

      return
      end



**************************************************************************
*
*              SETPANY
*    Fyller homedir, defdir, lognavn, prosessnummer, host og hosttype 
*    inn i commonomraadet direk og setter
*    iustrr i com. komstyr. Benytter bla .a. getenv
********************************************************************
      subroutine setpany
      include 'styr.inc'
*-------------------------------------------------------
      integer kf,ierr,getcwd,getpid

      call blank(hostt,40)
      call getenv('HOSTTYPE',hostt)
      call strip(hostt,40,kf)
      if(hostt(1:2).eq.'hp') then
        iustrr=7
      else
        iustrr=0
      end if


      call blank(homedir,120)
      call getenv('HOME',homedir)
      call strip(homedir,120,nhd)
      if(nhd.gt.0) then
        nhd=nhd+1
        homedir(nhd:nhd)='/'
      else
        write(iustrr,'(a)')'$HOME ikke satt'
      end if
      

      call blank(vert,40)
      call getenv('HOST',vert)
      
      call blank(lognav,40)
cc      call getlog(lognav)

      pronum=getpid()

      call blank(defdir,120)
      ierr=getcwd(defdir)
      if(ierr.eq.0.or.hostt(1:2).ne.'ds') then
        call strip(defdir,120,ndd)
        ndd=ndd+1
        defdir(ndd:ndd)='/'
      else
        write(iustrr,'(a)')'finner ikke def-dir'
      end if
        

      return
      end


**********************************************************************
*             FULLPATH
*
*       Gjor om lokalt navn til fullt pathnavn.
*           t - tekst med filnavn                      I/O
*           n - maks lengde av t                       I
*               dersom nodvendig trunkeres fremste part av pathnavn
**********************************************************************
      subroutine fullpath(t,n)
      integer n
      character*200 t
      include 'styr.inc'
*-------------------------------------------------------------- 
      integer kf,kadd

      call frontstrip(t,n,kf)

      if(t(1:1).eq.'/') return

      call strip(t,n,kf)

      kadd=min(ndd,n-kf)

      t(kadd+1:kadd+kf)=t(1:kf)

      t(1:kadd)=defdir(ndd-kadd+1:ndd)

      return
      end

      

***********************************************************************
*
*              FETCHENV
*
*   Henter ut opplysninger i en character*80 array.
*                cinf(1) - inneholder lognavn og pid (fra pos hhv. 1 og 41)
*                     2  -            host
*                     3  -            working dir. ( trunkeres evt. forfra)
*********************************************************************
      subroutine fetchenv(cinf)
      character*80 cinf(3)
      include 'styr.inc'
*------------------------------------------------------------------------
      integer kf,i
      character*20 cpid

      do 100 i=1,3
        call blank(cinf(i),80)
 100  continue  

      call itconv(pronum,kf,cpid)
      cinf(1)(1:40)=lognav(1:40)
      cinf(1)(41:40+kf)=cpid(1:kf)
 
      cinf(2)(1:40)=vert(1:40)
      
      if(ndd.gt.80) then
        cinf(3)(1:80)=defdir(ndd-79:ndd)
      else
        cinf(3)(1:ndd)=defdir(1:ndd)
      end if

      return
      end


**************************************************************************
*
*              SKRIVENV
*    Skriver ut homedir, defdir og hosttype fra commonomraadet direk.
********************************************************************
      subroutine skrivenv(itape)
      integer itape
      include 'styr.inc'
*-------------------------------------------------------
      integer kf,ierr

      write(itape,'(a,a)')'HOSTTYPE=',hostt(1:20)
      write(itape,'(a,a)')'HOME=',homedir(1:nhd)
      write(itape,'(a,a)')'wdir:',defdir(1:ndd)

      return
      end



**********************************************************************
      subroutine initkom
      include 'styr.inc'
*----------------------------------------------------------------------

      aktlog=0
      sattlog=0

      aktkom=0
      sattkom=0
      dispens=0

      intape=5
c     lese-enhet  settes til standard  input

      call setpany
c     def og home dir leses inn.
      return
      end

************************************************************************
*     setter lese-uniten til itape
********************************************************************
      subroutine setinp(itape)
      integer itape
      include 'styr.inc'
*------------------------------------------------------------------
      
      if(itape.le.0 .or. itape .eq.6)then
        write(iustrr,*)'ulovlig enhet for innlesning:',itape
        return
      end if

      intape=itape

      return
      end

************************************************************************
*     returnerer lese-enheten i infast
********************************************************************
      subroutine inpenh(infast)
      integer infast
      include 'styr.inc'
*------------------------------------------------------------------
      infast=intape

      return
      end


************************************************************************
*     returnerer unitnummer for standard error i iero
********************************************************************
      subroutine stdrror(iero)
      integer iero
      include 'styr.inc'
*------------------------------------------------------------------
      iero=iustrr

      return
      end


************************************************************************
*
*     Dersom fnavn inneholder navnet p} en mulig input fil, }pnes denne
*     med unit intape og innum settes som lesenhet. dersom fila ikke
*     finnes returneres uten noen endring.
**********************************************************************
      subroutine inpfil(fnavn,innum)
      integer innum
      character*80 fnavn
*-----------------------------------------------------------------------
      integer kf
      logical hiv
      integer iustrr

      call stdrror(iustrr)


      call xstrip(fnavn,kf,hiv)


      open(unit=innum,file=fnavn(1:kf),status='old',err=200)
      write(iustrr,*)'fil: ',fnavn(1:kf),' }pnet for input'
      call setinp(innum)
      return

 200  continue

      write(iustrr,*)'fil: ',fnavn(1:kf),' kan ikke }pnes'
      return
      end
      
************************************************************************
      subroutine setkomm(c)
      character c
      include 'styr.inc'
*----------------------------------------------------------------------

      sattkom=1
      aktkom=1
      charkom=c

      return
      end

************************************************************************
      subroutine stoppkomm
      include 'styr.inc'
*----------------------------------------------------------------------


      aktkom=0

      return
      end


************************************************************************
      subroutine startkomm
      include 'styr.inc'
*----------------------------------------------------------------------

      if(sattkom.eq.1) then
        aktkom=1
      else
        write(iustrr,*)'comments attempted chosen without enabling'
      end if

      return
      end


************************************************************************
      subroutine setlog(itape)
      integer itape
      include 'styr.inc'
*----------------------------------------------------------------------

      sattlog=1
      aktlog=1
      ltape=itape

      return
      end

************************************************************************
      subroutine stopplog
      include 'styr.inc'
*----------------------------------------------------------------------


      aktlog=0

      return
      end


************************************************************************
      subroutine startlog
      include 'styr.inc'
*----------------------------------------------------------------------


      if(sattlog.eq.1) then
        aktlog=1
      else
        write(iustrr,*)'logging set without enabling'
      end if

      return
      end


**************************************************************************
      subroutine primvri(t)
      character*80 t
      include 'styr.inc'

*----------------------------------------------------------------------
      integer k
      logical def        

      call xstrip(t,k,def)
      if(k.ge.78) k=78
      if(k.gt.0) then                           
        write(iustrr,*) t(1:k)
        if(aktlog.eq.1 .and. aktkom.eq.1 )
     %  write(ltape,*) charkom,t(1:k)
      else
        write(iustrr,*)' '
        if(aktlog.eq.1 .and. aktkom.eq.1)
     %  write(ltape,*) charkom
      end if

      return
      end


***************************************************************************
      subroutine primles(t,iflag)
      integer iflag
      character*80 t
      include 'styr.inc'

*----------------------------------------------------------------------
      integer i0,i1,kflag

      iflag=0

 100  continue

      if(dispens.eq.1) then
        read(5,20,end=50) t
      else
        read(intape,20,end=50) t
      end if
 20   format(a80)
 25   format(a78)

      call lokord(t,1,80,i0,i1,kflag)

      if(intape.ne.5 .and.
     %     t(i0:i0+3).eq.'????' .and.(dispens.eq.0)) then
        dispens=1
        write(iustrr,*)'input fra standard'
        read(5,20,end=50) t
      end if

      if(kflag.ne.0 .and. aktkom.eq.1) then
        if(t(i0:i0).eq.charkom) then
          if(aktlog.eq.1) write(ltape,25) t(1:78)
          go to 100
        end if
      end if

      if(aktlog.eq.1) write(ltape,25) t(1:78) 
      if(intape.ne.5 .and.(dispens.eq.0)) write(iustrr,25) t(1:78) 

      return

 50   continue
      if(dispens.eq.1 .or. intape.eq.5) then
         write(iustrr,*)'end of file funnet p} standard input'
         iflag=2
         return
      end if
      write(iustrr,*)'slutt p} input fra enhet=',intape
      intape=5
      iflag=1
      return
      
      end




*******************************************************************************
*
*                    R T A L L
*
*     Real-utgaven av itall.
*
*     kaller 'xstrip' og 'getr'

*******************************************************************************
*
*                    R T A L L
*
*     Real-utgaven av itall.
*
*     kaller 'xstrip' og 'getr'
******************************************************************************
      function rtallo(rdef,spor)
      real rtallo,rdef
      character*80 spor
      include 'styr.inc'
*---------------------------------------------------------
      character*80 svar
      integer k,i,i1,i2,iflag,ib,kflag
      real rres,rverdi
      logical def        

      call xstrip(spor,k,def)                           
      if(k.eq.0) k=1

  200 continue
   
      if(def) then
        write(iustrr,*) spor(1:k) ,'/', rdef ,'/'
        if(aktlog.eq.1 .and. aktkom.eq.1)
     %  write(ltape,*) charkom,spor(1:k) ,'/', rdef ,'/'
      else
        write(iustrr,*) spor(1:k)
        if(aktlog.eq.1 .and. aktkom.eq.1)
     %     write(ltape,*) charkom,spor(1:k) 
      end if

      call primles(svar,kflag)

      if(kflag.eq.2) stop
      if(kflag.ne.0) go to 200

      call getr(svar,1,80,i1,i2,rres,iflag)
      if(iflag.eq.0) then
        if(def) then
          rverdi=rdef
          go to 500
        else
          call primvri('No default value available#')
          go to 200
        end if
      end if
      
      if(iflag.gt.1) then
        call primvri('illegal input#')
        go to 200
      else
        rverdi=rres
        ib=i2+1
        call getr(svar,ib,80,i1,i2,rres,iflag)
        if(iflag.ne.0) then
          call primvri('Please, give a single real#')
          go to 200
        end if
      end if

 500  continue

      rtallo=rverdi
      dispens=0

      return
      end                 



******************************************************************************
      function rtall(rdef,spor)
      real rtall,rdef
      character*80 spor
      include 'styr.inc'
*---------------------------------------------------------
      character*80 svar
      integer k,i,i1,i2,iflag,ib,kflag
      real rres,rverdi
      logical def        

      call xstrip(spor,k,def)                           
      if(k.eq.0) k=1

  200 continue
   
      if(def) then
        write(iustrr,*) spor(1:k) ,'/', rdef ,'/'
        if(aktlog.eq.1 .and. aktkom.eq.1)
     %  write(ltape,*) charkom,spor(1:k) ,'/', rdef ,'/'
      else
        write(iustrr,*) spor(1:k)
        if(aktlog.eq.1 .and. aktkom.eq.1)
     %     write(ltape,*) charkom,spor(1:k) 
      end if

      call primles(svar,kflag)

      if(kflag.eq.2) stop
      if(kflag.ne.0) go to 200

      call getrl(svar,1,80,i1,i2,rres,iflag)
      if(iflag.eq.0) then
        if(def) then
          rverdi=rdef
          go to 500
        else
          call primvri('No default value available#')
          go to 200
        end if
      end if
      
      if(iflag.gt.1) then
        call primvri('illegal input#')
        go to 200
      else
        rverdi=rres
        ib=i2+1
        call getrl(svar,ib,80,i1,i2,rres,iflag)
        if(iflag.ne.0) then
          call primvri('Please, give a single real#')
          go to 200
        end if
      end if

 500  continue

      rtall=rverdi
      dispens=0

      return
      end                 


*******************************************************************************
*           
*                     L E S T E K
*
*    stiller sporsmal spor og leser svaret som teksten 'tekst'. spor angis
*    som for 'itall', standardverdien tdef avsluttes med # eller '!'(disse
*    blir ikke med i svar). Svar antas aa vaere 80 lang og etterfylles med
*    blanke. En tom tekst kan ikke vare standardverdi.                                                                 
*
*    kaller 'xstrip' og 'lokord'
*******************************************************************************
      subroutine lestek(tdef,spor,svar)
      character*80 tdef,svar, spor
      include 'styr.inc'
*---------------------------------------------------------
      integer k,i,i1,i2,k1,iflag,kflag
      logical def,daf        

      do 70 i=1,80
   70 svar(i:i)=' '

      call xstrip(spor,k,def)
      if(k.eq.0) k=1                           

      if(def) then
        call xstrip(tdef,k1,daf)
        if(k1.eq.0) def=.false.
      end if

  200 continue

      if(def) then
        write(iustrr,*) spor(1:k) ,'/', tdef(1:k1) ,'/'
        if(aktlog.eq.1 .and. aktkom.eq.1)
     %  write(ltape,*) charkom,spor(1:k) ,'/', tdef(1:k1) ,'/'
      else
        call primvri(spor)
      end if

      call primles(svar,kflag)
      if(kflag.eq.2) stop
      if(kflag.ne.0) go to 200

      call lokord(svar,1,80,i1,i2,iflag)
      if(iflag.eq.0) then
        if(def) then
          svar(1:k1)=tdef(1:k1)
          go to 500
        else
          call primvri('No default value available#')
          go to 200
        end if
      end if
      
 500  continue

      dispens=0
      
      return
      end



***********************************************************************
*
*                      L S J E K K
*
*    stiller spoersmal spor ,standardverdi tdef faaes eller ignoreres
*    paa samme vis som i 'itall'. I svaret (teksten svar) lokaliseres
*    sekvensene til l(1,k):l(2,k) k=1..iant. Som lovlig svar godtas
*    bare de hvis foerste sekvens er en entydig forkortelse til et ord
*    i menyen liste(nl); lnum angir hvilket ord. 
*
*    kaller 'alacart','deltek','red' ,'lestek' og 'upchar'.
*****************************************************************************
      subroutine lsjekk(tdef,spor,svar,l,iant,liste,nl,lnum)
      integer iant,lnum,l(2,20),nl
      character*80 tdef,spor,svar,liste(nl)
      include 'styr.inc'
*----------------------------------------------------------------------
      integer ltall,men(20),i



  100 continue
      call lestek(tdef,spor,svar)
      call red(svar,80)
      call deltek(svar,1,80,l,iant)
      i=l(2,iant)
      if(svar(i:i).eq.'#') l(2,iant)=i-1
      call alacart(liste,nl,svar,l(1,1),l(2,1),men,ltall)

      if(ltall.eq.0) then
        write(iustrr,*) 'illegal command: /',svar(l(1,1):l(2,1)),'/'
        if(aktlog.eq.1 .and. aktkom.eq.1) 
     %   write(ltape,*) charkom,'illegal command: /'
        if(aktlog.eq.1 .and. aktkom.eq.1) 
     %   write(ltape,*) charkom,svar(l(1,1):l(2,1)),'/' 
        go to 100
      end if
      if(ltall.gt.1) then
        write(iustrr,*) 'ambiguous abbreviation: /',svar(1:l(2,1)),'/'
        if(aktlog.eq.1 .and. aktkom.eq.1) 
     %   write(ltape,*) charkom,'ambiguous abbreviation: /'
        if(aktlog.eq.1 .and. aktkom.eq.1) 
     %  write(ltape,*) charkom,svar(1:l(2,1)),'/'
        go to 100
      end if

      lnum=men(1)

      return
      end


************************************************************************* 
*
*                     J A
*    leser ja/nei svar(returnerer med true dersom svar er 'ja').
*    Spoersmal spor gis som for 'itall'. Svarene 'ja','nei','yes','no'
*    og deres forkortelser godtas.
*
*    kaller: 'lestek','lokord','fork'
**************************************************************************
      function ja(def,spor)
      logical def,ja
      character*80 spor
      include 'styr.inc'
*-------------------------------------------------------------------
      character*80 cdef,svar
      integer i1,i2,i3,i4,iflag,n0      
      logical fork,lik

      if(def) then
        cdef='yes#'
      else
        cdef='no#'   
      end if
  100 continue
            
      call lestek(cdef,spor,svar)      
      call lokord(svar,1,80,i1,i2,iflag)
      n0=i2+1
      call lokord(svar,n0,80,i3,i4,iflag)
      if(iflag.ne.0) then
        call primvri(' Answer yes or no#')
        go to 100
      end if

      if( fork('ja#',svar,i1,i2) ) then
        ja=.TRUE.
        return
      end if

      if( fork('nei#',svar,i1,i2) ) then
        ja=.false.
        return
      end if
      if( fork('yes#',svar,i1,i2) ) then   
        ja=.true.
        return
      end if
      if( fork('oui#',svar,i1,i2) ) then   
        ja=.true.
        return
      end if
      if( fork('no#',svar,i1,i2) ) then 
        ja=.false.
        return
      end if
     
      call primvri('Answer yes or no#')
      go to 100
      end




*************************************************************************     *  
*
*             L E S O R D 
*
*     spoer om og leser et enkelt ord. Stopptegnene '#' og '!' brukes
*     paa vanlig maate. svaret ligger i svar(1:il).
*
*    kaller: 'lestek','lokord'.
**************************************************************************
      subroutine lesord(wdef,spor,svar,il)
      character*80 wdef,spor,svar 
      integer il
      include 'styr.inc'
*--------------------------------------------------------------------
      integer i,kb,k1,k2,k3,k4,iflag
      character*80 resp

  100 continue
      call lestek(wdef,spor,resp) 
      call lokord(resp,1,80,k1,k2,iflag) 
      kb=k2+1
      call lokord(resp,kb,80,k3,k4,iflag)
      if(iflag.ne.0) then
        call primvri('Give a single word, please#')
        go to 100
      end if
      
      il=k2-k1+1
      svar(1:il)=resp(k1:k2)   
      return
      end



**********************************************************************
*          LESFRAREG
*
*    Leser ett valg fra en liste av ord, forkortelser aksepteres
*    paa vanlig vis.
*        spor - sporsmaal                                    I
*        idef - nummer paa default valg                      I
*        reg - liste av alternativer                         I
*        n  - antall alternativer                            I
*        ires - nummer paa svar                              O
**********************************************************************
      subroutine lesfrareg(spor,idef,reg,n,ires)
      integer n,ires,idef
      character*80 reg(n),spor
*-----------------------------------------------------------------
      character*80 wdef,svar,mess
      integer il,kf,l(50),i,iant,ig
      logical kast,fork

      if(idef.gt.n .or. n.gt.50) then
        call primvri('Error in lesfrareg#')
        return
      end if

      call xstrip(reg(idef),kf,kast)
      wdef(1:kf)=reg(idef)(1:kf)
      wdef(kf+1:kf+1)='#'

 100  continue
      call lesord(wdef,spor,svar,il)
      


      call velgcart(reg,n,svar,1,il,l,iant)

      if(iant.gt.1) then
        call primvri('ambiguous abbreviation#')
        go to 100
      end if

      if(iant.eq.0) then
        if(fork('hjelp#',svar,1,il) .or. fork('help#',svar,1,il)) then
          call primvri('Available alternatives:#')
          do 50 i=1,n
          call xstrip(reg(i),kf,kast)
          mess(1:kf+1)=reg(i)(1:kf+1)
          call primvri(mess)
 50       continue
        else
          mess(1:14)='illegal answer:'
          ig=min(il,64)
          mess(15:15+ig)=svar(1:ig)
          mess(16+ig:16+ig)='#'
          call primvri(mess)
        end if
        go to 100
      end if

      ires=l(1)

      end





**********************************************************************
*          MERKIREG
*
*    Leser ett utvalg fra en liste av ord, forkortelser aksepteres
*    paa vanlig vis dersom de er entydige.
*        spor - sporsmaal                                    I
*        def - default svar, maa væere korekt                      I
*        reg - liste av alternativer                         I
*        n  - antall alternativer                            I
*        mark - gir antall angivelser av hvert ord           O
**********************************************************************
      subroutine merkireg(spor,def,reg,n,mark)
      integer n,mark(n)
      character*80 reg(n),spor,def
*-----------------------------------------------------------------
      character*80 wdef,svar,mess
      integer il,kf,l(100),i,iant,ig,lp(2,40),k,iord,ires
      logical kast,fork

      if(n.gt.100) then
        call primvri('Error in lesfrareg#')
        return
      end if


 100  continue

      do 30 i=1,n
        mark(i)=0
 30   continue
  
      call lestek(def,spor,svar)
      
      call deltek(svar,1,80,lp,iord)

      do 70 k=1,iord
      call velgcart(reg,n,svar,lp(1,k),lp(2,k),l,iant)

      if(iant.gt.1) then
        call primvri('ambiguous abbreviation#')
        go to 100
      end if

      if(iant.eq.0) then
        if(fork('hjelp#',svar,lp(1,k),lp(2,k)) .or.
     %       fork('help#',svar,lp(1,k),lp(2,k))) then
          call primvri('Available alternatives:#')
          do 50 i=1,n
          call xstrip(reg(i),kf,kast)
          mess(1:kf+1)=reg(i)(1:kf+1)
          call primvri(mess)
 50       continue
        else
          mess(1:16)='illegal answer: '
          ig=17+lp(2,k)-lp(1,k)
          mess(17:ig)=svar(lp(1,k):lp(2,k))
          mess(ig+1:ig+1)='#'
          call primvri(mess)
        end if
        go to 100
      end if

      ires=l(1)
      mark(ires)=mark(ires)+1

 70   continue


      end









************************************************************************
*
*              LESNRS
***********************************************************************
      subroutine lesnrs(def,spor,svar,med,irad)
      integer irad
      logical med(irad)
      character*80 def,spor,svar
*----------------------------------------------------------------------
      integer iflag,i2,kf
      character*80 mess

      mess(1:14)='antall rader: '
      call itconv(irad,kf,svar)
      mess(15:15+kf)=svar(1:kf)
      mess(16+kf:16+kf)='#'

 100  call primvri(mess)
      call blank(svar,80)
      call lestek(def,spor,svar)
      call strip(svar,80,i2)
      if(i2.lt.80) svar(i2+1:i2+1)='#'
      call konvert(svar,1,i2,irad,med,iflag)
      if(iflag.gt.0) then
        call primvri('ulovlig nummerangivelse#')
        go to 100
      end if

      return
      end


