**************************************************************************
      subroutine lokchar(t,c,i1,i2,k,eks)
      integer i1,i2,k
      character*80 t
      character c
      logical eks
*------------------------------------------------------------------------
      integer i

      if(i1.gt.i2) then
        eks=.false.
        return
      end if

      i=i1-1

 100  continue
      i=i+1

      if(t(i:i).eq.c) then
        k=i
        eks=.true.
        return
      end if

      if(i.eq.i2) then
        eks=.false.
        return
      end if

      go to 100

      end



***************************************************************************
*
*          t - i1:i2 inneholder ett ord.
*************************************************************************
      subroutine extension(t,i1,i2,ten,kf)
      integer i1,i2,kf
      character*80 t,ten
*-------------------------------------------------------------------------
      integer k,nmax,np,iflag
      logical eks
      character*80 tt

       nmax=i2-i1+1
       tt(1:nmax)=t(i1:i2)
c      call lokchar(t,'.',i1,i2,k,eks)
       np=nmax
       iflag=0
      if(tt(nmax:nmax).ne.'.') call fsok(tt,nmax,np,'.',-1,iflag)
      eks=iflag.eq.0 
      if(eks) then
        kf=nmax-np
        if(kf.gt.0) then
          ten(1:kf)=tt(np+1:nmax)
        end if
      else
        kf=-1
      end if

      return
      end



*************************************************************************
*
*                 FILGEN
*
*  Stiller sporsmaal paa standard error og leser filangivelse som spesifisert i
*  beskrivelse av rutinen 'ordfile'. Filnavn ekspanderes som beskrevet i
*   expand.
*  'Initkom' m} v{re kalt f|r f|rste kall p} filgen. I traad med vanlig 
*   praksis i dia rutiner ender inputstrenger med ! eller #.
*   parametere:
*        ITAPE - unitnummer                                        I/O
*                dersom noe gaar galt settes itape < 0
*        spor  - sporsmaal                                         I
*        defn  - standard filnavn, derom spor ender med ! neglisjeres
*                denne og svar maa gis.                            I
*        NAVN(1:kf) - filnavn                                       O
*        kf - se ovenfor                                            O
*        asc - = .true. angir asci, = .false. binaer               O
*        inp - = .true. angir inputfil, = .false. outputfil        I
*************************************************************************
      SUBROUTINE filgen(ITAPE,SPOR,DEFN,NAVN,KF,asc,inp)
      INTEGER ITAPE,KF
      CHARACTER*300 SPOR,DEFN,NAVN
      logical asc,inp
      include 'styr.inc'
*-----------------------------------------------------------------------
      CHARACTER*300 HNAVN,FNA
      character*11 fmat
      LOGICAL FORK
      INTEGER KF0,iflag,kfex
                 
      kf0=kf
 100  continue

      CALL BLANK(HNAVN,300)
      CALL BLANK(FNA,300)
      CALL LESTEK(DEFN,SPOR,HNAVN)
      call ordfile(hnavn,1,200,fna,kf,asc,iflag)
      if(iflag.gt.0) then
        write(0,*)'iflag=',iflag
        call primvri('ulovlig eller ufullstendig fil-spes.#')
        go to 100
      end if

      IF(KF.GT.KF0) THEN
        WRITE(IUSTRR,11) ' ',KF,KF0
        if(aktlog.eq.1 .and. aktkom.eq.1)
     %     WRITE(ltape,11) charkom,KF,KF0
  11    FORMAT(1X,a1,'LENGDE:',I4,'  MAX. LENGDE:',I4)
        call primvri('GI NYTT OG KORTERE NAVN#')
        GO TO 100
      ELSE       
        CALL BLANK(NAVN,KF0)
        NAVN(1:KF)=fna(1:KF)
      END IF

      IF(FORK('STOPP#',NAVN,1,KF) .or. 
     %     fork('INGEN#',navn,1,kf) ) THEN
        ITAPE=-1
        RETURN
      END IF


      if(asc) then
        IF(FORK('TTY#',NAVN,1,KF) .OR. 
     %       FORK('TERMINAL#',NAVN,1,KF) ) THEN
          if(inp) then
            ITAPE=5
          else
            itape=iustrr
          end if
          RETURN
        END IF


        if(inp) then
          IF(FORK('STANDARD#',NAVN,1,KF) .or. 
     %        fork('input#',navn,1,kf) ) THEN
            ITAPE=5
            RETURN
          END IF  
        else
         IF(FORK('ERROR#',NAVN,1,KF)) THEN
           ITAPE=iustrr
           RETURN
         END IF   

         IF(FORK('OUTPUT#',NAVN,1,KF)) THEN
           ITAPE=6
           RETURN
          END IF   
       end if
          
      end if 

      if(asc) then
        fmat='formatted  '
      else
        fmat='unformatted'
      end if

      call expand(navn,kf,fna,kfex,iflag)
      if(iflag.gt.0) then
        call primvri('ulovlig filnavn#')
        go to 100
      end if

      if(kfex.le.kf0) then
        kf=kfex
        navn(1:kf)=fna(1:kf)
      end if

      if(inp) then
cc        call primvri(fna(1:kfex))
cc        write(0,'(a)')fna(1:kfex)
       open(UNIT=itape,file=fna(1:kfex),ERR=117,status='old',
     % form=fmat)

        return
  117   call primvri('fil ikke funnet:#')
      else
       open(UNIT=itape,file=fna(1:kfex),ERR=119,status='unknown'
     %      ,form=fmat)
       return
 119   call primvri('fil kan ikke }pnes#')
      end if
      call primvri('gi nytt navn ("stopp" dersom du gir opp)#')
      go to 100

      end




***********************************************************************
*
*     Rutinen unders|ker en streng for } finne en filbskrivelse som
*     best}r av navn og type-angivelse. Type angivelse er en forkortelse,
*     minst 3 tegn lang, av 'ascii' eller 'binary'. Store og sm} bokstaver
*     behandles likt. 
*          Dersom to ord finnes i strengen tolkes det ene som
*     type, det andre som filnavn uavhengig av rekkef|lge. Dette betyr
*     at filnavn som er mulige typeangivelser vanligvis ikke kan benyttes.
*          Hvis strengen inneholder bare ett ord, tolkes dette som filnavn.
*     Type settes da som ascii med ett unntak: filnavn inneholder et og
*     bare ett punktum etterfulgt av type angivelse for bin{r fil.
*          eksempler
*   
*               streng              filnavn           type
*          
*             bin  a.dat             a.dat            bin{r
*             a.dat ascii            a.dat            ascii
*             bin asc                    ulovlig streng
*             a.dat                  a.dat            ascii
*             a.binary               a.binary         bin{r
*             a.bin.dat              a.bin.dat        ascii
*             .bin                    .bin            bin{r
*             asci a.bin             a.bin            ascii
*             ascii                      ulovlig streng
*
*     parametere:
*             t(i1:i2) - streng med filangivelse                     I
*               i1,i2  - angir del av tekst t som skal unders|kes    I
*             name(1:kf) - tekst som angir funnet filnavn            O
*             kf        - lengde av filnavn                          O
*             asc       - verdi .true. angir ascii, .false. bin{r    O
*             iflag     - feilparameter                              O
*                        =0  lovlig og fullstendig informasjon i streng
*                        =1  streng er tom
*                        =2  streng inneholder to ord, ingen kan tolkes 
*                            som type-betegnelse
*                        =3  streng inneholder 2 ord som begge er
*                            type-betegnelser
*                        =4,5  streng inneholder bare ett ord som er en
*                            betegnelse  for hhv ascii og bin{r.
*******************************************************************************
      subroutine ordfile(t,i1,i2,name,kf,asc,iflag)
      integer i1,i2,kf,iflag
      character*80 t,name
      logical asc
*-----------------------------------------------------------------------
      integer l(2,20),iant,j,itreff(2),m,nfil,ntyp
      character*80 ten
      logical fork

      iflag=0
      call deltek(t,i1,i2,l,iant)

      if(iant.eq.0) then
        iflag=1
        return
      end if

      if(iant.gt.2) then
        iflag=2
        return
      end if

      if(iant.eq.1) then
        kf=l(2,1)-l(1,1)+1
        name(1:kf)=t(l(1,1):l(2,1))
        call extension(name,1,kf,ten,j)
        if(j.gt.0) then
          asc =.not.(fork('binary#',ten,1,j) .and. j.ge.3)
        else
          asc=.true.
          if(kf.ge.3) then
            if(fork('ascii#',name,1,kf)) iflag=4
            if(fork('binary#',name,1,kf)) iflag=5
          end if
        end if
        return
      end if

      do 100 m=1,2
      if(l(2,m)-l(1,m).lt.2) then
        itreff(m)=0
      else
        if(fork('ascii#',t,l(1,m),l(2,m)) ) then
          itreff(m)=1
        else
          if(fork('binary#',t,l(1,m),l(2,m)) ) then
            itreff(m)=2
          else
            itreff(m)=0
          end if
        end if
      end if

 100  continue

      if( itreff(1).eq.0 .and. itreff(2).eq.0) then
        iflag=2
        return
      end if

      if( itreff(1).gt.0 .and. itreff(2).gt.0) then
        iflag=3
        return
      end if

      if(itreff(1).gt.0) then
        ntyp=1
        nfil=2
      else
        ntyp=2
        nfil=1
      end if

      asc=itreff(ntyp).eq.1
      kf=l(2,nfil)-l(1,nfil)+1
      name(1:kf)=t(l(1,nfil):l(2,nfil))
    
      return
      end



*************************************************************************
*
*                 FILOPN
*
*  AApner fil fra gitt filangivelse som beskrevet i expand og ordfile.
*  'Initkom' m} v{re kalt f|r f|rste kall p} filopn dersom expandering
*   skal virke.
*   parametere:
*        ITAPE - unitnummer                                        I
*        NAVN - filangivelse                                       I
*               eksempe paa lov. ang:  bin ~/hh.jj 
*        fulln(1:kf) -filnavn, dersom det er ekspandert inneholder   O
*                     fulln hele pathen.
*        kf - se ovenfor                                            O
*        asc - = .true. angir asci, = .false. binaer               O
*        inp - = .true. angir inputfil, = .false. outputfil        I
*        ierr - feilparameter                                      O
*              = 0          alt ok
*                1          gal angivelse av type ( bin/asc etc.)
*                2          feil under expandering
*                3          fil kan ikke aapnes
***********************************************************************
      SUBROUTINE FILopn(ITAPE,NAVN,fulln,kf,asc,inp,ierr)
      INTEGER ITAPE,kf,ierr
      CHARACTER*80 NAVN
      character*120 fulln
      logical asc,inp
      include 'styr.inc'
*-----------------------------------------------------------------------
      CHARACTER*120 FNA
      character*11 fmat
      character*7 fstat
      INTEGER iflag,kfa,kfn
      logical paelm
                 
      ierr=0

      call xstrip(navn,kfn,paelm)
      CALL BLANK(FNA,120)
      call ordfile(navn,1,kfn,fna,kfa,asc,iflag)

      if(iflag.gt.0) then
        ierr=1
        return
      end if



      if(asc) then
        fmat='formatted  '
      else
        fmat='unformatted'
      end if

      if(inp) then
        fstat='old    '
      else
        fstat='unknown'
      end if

      call expand(fna,kfa,fulln,kf,iflag)
      if(iflag.gt.0) then
        ierr=2
        return
      end if

      open(UNIT=itape,file=fulln(1:kf),ERR=119,status=fstat
     %      ,form=fmat)
      return
 119  continue
      ierr=3
      return
    

      end


********************************************************************
*
*                LEXYFIL
*                
*     Reads data from two-column file
*
*     fnavn  - Name on file                                            I
*     kl  -   lengtn of filename. If kl<=0 the name is assumed to be   I
*             ended by '#' or '!' and the length is calculated
*     x,y  -  arrays for data                                          O
*     nmax  - maximum number of data                                   I
*     n  -    number of data read                                      O
*     ope  -  set to false if file needs to be opened                  I
*     itape  - unitnumber                                              I
*     nskip  -  number of leading comment lines in file                O  
*     iflag  - error flag                                              O
*                0     : OK
*                1     : maximum number of data reached before end of file.
*                        The data that is read is returned in x and y
*                3     : error in comment processing ( no data ?)
*                30    : inappropriate filename 
*                50    : cannot open file
********************************************************************
      subroutine lexyfil(fnavn,kl,x,y,nmax,n,ope,itape,nskip,iflag)
      integer kl,nmax,n,itape,nskip,iflag
      real x(nmax),y(nmax)
      character*80 fnavn
      logical ope
*------------------------------------------------------------------
      integer kf,ierr
      real a,b
      logical def
 
      iflag=0

      if(kl.le.0) then
      call xstrip(fnavn,kf,def)
         if(kf.le.0) then
           iflag=30
           return
         end if         
      else
         kf=kl
      end if


      if( .not.ope) then
       open(unit=itape,file=fnavn(1:kf),err=117,status='old')
      end if


cc      call skipkom(nskip,itape,ierr)
      call slopskip(nskip,itape,ierr)
      if(ierr.gt.0) then
      write(0,*)'skipkom: nskip,ierr=',nskip,ierr
         iflag=3
         close(itape)
         return
      end if

      n=0

 100  continue

      read(itape,*,end=200)a,b
      n=n+1
      if(n.gt.nmax) then
        iflag=1
        close(itape)
        return
      end if
      x(n)=a
      y(n)=b

      go to 100

 200  continue
      close(itape)
      return

 117  continue
      iflag=50
      return

      end



********************************************************************
*
*                LEXYzFIL
*                
*     Reads data from three-column file
*
*     fnavn  - Name on file                                            I
*     kl  -   lengtn of filename. If kl<=0 the name is assumed to be   I
*             ended by '#' or '!' and the length is calculated
*     x,y,z  -  arrays for data                                          O
*     nmax  - maximum number of data                                   I
*     n  -    number of data read                                      O
*     ope  -  set to false if file needs to be opened                  I
*     itape  - unitnumber                                              I
*     nskip  -  number of leading comment lines in file                O  
*     iflag  - error flag                                              O
*                0     : OK
*                1     : maximum number of data reached before end of file.
*                        The data that is read is returned in x,y and z
*                3     : error in comment processing ( no data ?)
*                30    : inappropriate filename 
*                50    : cannot open file
********************************************************************
      subroutine lexyzfil(fnavn,kl,x,y,z,nmax,n,ope,itape,nskip,iflag)
      integer kl,nmax,n,itape,nskip,iflag
      real x(nmax),y(nmax),z(nmax)
      character*80 fnavn
      logical ope
*------------------------------------------------------------------
      integer kf,ierr
      real a,b,c
      logical def
 
      iflag=0

      if(kl.le.0) then
      call xstrip(fnavn,kf,def)
         if(kf.le.0) then
           iflag=30
           return
         end if         
      else
         kf=kl
      end if


      if( .not.ope) then
       open(unit=itape,file=fnavn(1:kf),err=117,status='old')
      end if


cc      call skipkom(nskip,itape,ierr)
      call slopskip(nskip,itape,ierr)
      if(ierr.gt.0) then
      write(0,*)'skipkom: nskip,ierr=',nskip,ierr
         iflag=3
         close(itape)
         return
      end if

      n=0

 100  continue

      read(itape,*,end=200)a,b,c
      n=n+1
      if(n.gt.nmax) then
        iflag=1
        close(itape)
        return
      end if
      x(n)=a
      y(n)=b
      z(n)=c

      go to 100

 200  continue
      close(itape)
      return

 117  continue
      iflag=50
      return

      end


********************************************************************
*
*                LEXFIL
*                
*     Reads data from one-column file
*
*     fnavn  - Name on file                                            I
*     kl  -   lengtn of filename. If kl<=0 the name is assumed to be   I
*             ended by '#' or '!' and the length is calculated
*     x  -  array for data                                          O
*     nmax  - maximum number of data                                   I
*     n  -    number of data read                                      O
*     ope  -  set to false if file needs to be opened                  I
*     itape  - unitnumber                                              I
*     nskip  -  number of leading comment lines in file                O  
*     iflag  - error flag                                              O
*                0     : OK
*                1     : maximum number of data reached before end of file.
*                        The data that is read is returned in x and y
*                3     : error in comment processing ( no data ?)
*                30    : inappropriate filename 
*                50    : cannot open file
********************************************************************
      subroutine lexfil(fnavn,kl,x,nmax,n,ope,itape,nskip,iflag)
      integer kl,nmax,n,itape,nskip,iflag
      real x(nmax)
      character*80 fnavn
      logical ope
*------------------------------------------------------------------
      integer kf,ierr
      real a
      logical def
 
      iflag=0

      if(kl.le.0) then
      call xstrip(fnavn,kf,def)
         if(kf.le.0) then
           iflag=30
           return
         end if         
      else
         kf=kl
      end if


      if( .not.ope) then
       open(unit=itape,file=fnavn(1:kf),err=117,status='old')
      end if


cc      call skipkom(nskip,itape,ierr)
      call slopskip(nskip,itape,ierr)
      if(ierr.gt.0) then
      write(0,*)'skipkom: nskip,ierr=',nskip,ierr
         iflag=3
         close(itape)
         return
      end if

      n=0

 100  continue

      read(itape,*,end=200)a
      n=n+1
      if(n.gt.nmax) then
        iflag=1
        close(itape)
        return
      end if
      x(n)=a

      go to 100

 200  continue
      close(itape)
      return

 117  continue
      iflag=50
      return

      end


********************************************************************
*
*                LENMFIL
*                
*     Reads data from two-column file
*
*     fnavn  - Name on file                                            I
*     kl  -   lengtn of filename. If kl<=0 the name is assumed to be   I
*             ended by '#' or '!' and the length is calculated
*     nx,ny  -  arrays for data                                        O
*     nmax  - maximum number of data                                   I
*     n  -    number of data read                                      O
*     ope  -  set to false if file needs to be opened                  I
*     itape  - unitnumber                                              I
*     nskip  -  number of leading comment lines in file                O  
*     iflag  - error flag                                              O
*                0     : OK
*                1     : maximum number of data reached before end of file.
*                        The data that is read is returned in x and y
*                3     : error in comment processing ( no data ?)
*                30    : inappropriate filename 
*                50    : cannot open file
********************************************************************
      subroutine lenmfil(fnavn,kl,nx,ny,nmax,n,ope,itape,nskip,iflag)
      integer kl,nmax,n,itape,nskip,iflag,nx(nmax),ny(nmax)
      character*80 fnavn
      logical ope
*------------------------------------------------------------------
      integer kf,ierr,na,nb
      logical def
 
      iflag=0

      if(kl.le.0) then
      call xstrip(fnavn,kf,def)
         if(kf.le.0) then
           iflag=30
           return
         end if         
      else
         kf=kl
      end if


      if( .not.ope) then
       open(unit=itape,file=fnavn(1:kf),err=117,status='old')
      end if


      call skipkom(nskip,itape,ierr)
      if(ierr.gt.0) then
      write(0,*)'skipkom: nskip,ierr=',nskip,ierr
         iflag=3
         close(itape)
         return
      end if

      n=0

 100  continue

      read(itape,*,end=200)na,nb
      n=n+1
      if(n.gt.nmax) then
        iflag=1
        close(itape)
        return
      end if
      nx(n)=na
      ny(n)=nb
 
      go to 100

 200  continue
      close(itape)
      return

 117  continue
      iflag=50
      return

      end





********************************************************************
*
*                PROMPTFIL
*                
*     Prompts for name and reads data from two-column file
*
*     spor   - question                                                I
*     fnavn,defn  - Name on file and def value                         O
*     kl  -   lengtn of filename. If kl<=0 the name is assumed to be   O
*             ended by '#' or '!' and the length is calculated
*     x,y  -  arrays for data                                          O
*     nmax  - maximum number of data                                   I
*     n  -    number of data read                                      O
*     nskip  -  number of leading comment lines in file                O  
*     iflag  - error flag                                              O
*                0     : OK
*                1     : maximum number of data reached before end of file.
*                        The data that is read is returned in x and y
*               11     : file not opened
*                3     : error in comment processing ( no data ?)
*                30    : inappropriate filename 
*                50    : cannot open file
********************************************************************
      subroutine promptfil(spor,fnavn,defn,kl,x,y,nmax,n,nskip,iflag)
      integer kl,nmax,n,nskip,iflag
      real x(nmax),y(nmax)
      character*300 spor,fnavn,defn
*------------------------------------------------------------------
      integer itape
      logical asc

      itape=55
      iflag=0
   
      kl=299
      call filgen(itape,spor,defn,fnavn,kl,asc,.true.)
      if(itape.le.0 .or.(.not.asc)) then
          iflag=11
          return
      end if

      call lexyfil(fnavn,kl,x,y,nmax,n,.true.,itape,nskip,iflag)
 
      return
      end

********************************************************************
*
*                gpromptf
*                
*     Prompts for name and reads data from a one- or two-column file
*
*     spor   - question                                                I
*     fnavn,defn  - Name on file and def value                         O
*     kl  -   lengtn of filename. If kl<=0 the name is assumed to be   O
*             ended by '#' or '!' and the length is calculated
*     x,y  -  arrays for data                                          O
*     nmax  - maximum number of data                                   I
*     ic  - number of columns
*     n  -    number of data read                                      O
*     nskip  -  number of leading comment lines in file                O  
*     iflag  - error flag                                              O
*                0     : OK
*                1     : maximum number of data reached before end of file.
*                        The data that is read is returned in x and y
*               11     : file not opened
*                3     : error in comment processing ( no data ?)
*                30    : inappropriate filename 
*                50    : cannot open file
********************************************************************
      subroutine gpromptf(spor,fnavn,defn,kl,x,y,nmax,ic,n,nskip,iflag)
      integer kl,nmax,ic,n,nskip,iflag
      real x(nmax),y(nmax)
      character*80 spor,fnavn,defn
*------------------------------------------------------------------
      integer itape
      logical asc

      itape=55
      iflag=0
   
      kl=79
      call filgen(itape,spor,defn,fnavn,kl,asc,.true.)
      if(itape.le.0 .or.(.not.asc)) then
          iflag=11
          return
      end if

      if(ic.eq.2) then
       call lexyfil(fnavn,kl,x,y,nmax,n,.true.,itape,nskip,iflag)
      else
       call lexfil(fnavn,kl,x,nmax,n,.true.,itape,nskip,iflag)
      end if

      return
      end


********************************************************************
*
*                nnpromptf
*                
*     Prompts for name and reads data from one, two or three-column file
*
*     spor   - question                                                I
*     fnavn,defn  - Name on file and def value                         O
*     kl  -   lengtn of filename. If kl<=0 the name is assumed to be   O
*             ended by '#' or '!' and the length is calculated
*     x,y,z  -  arrays for data                                          O
*     nmax  - maximum number of data                                   I
*     ic  - number of columns
*     n  -    number of data read                                      O
*     nskip  -  number of leading comment lines in file                O  
*     iflag  - error flag                                              O
*                0     : OK
*                1     : maximum number of data reached before end of file.
*                        The data that is read is returned in x and y
*               11     : file not opened
*                3     : error in comment processing ( no data ?)
*                30    : inappropriate filename 
*                50    : cannot open file
********************************************************************
      subroutine nnpromptf(spor,fnavn,defn,kl,x,y,z,nmax,ic,n,
     %nskip,iflag)
      integer kl,nmax,ic,n,nskip,iflag
      real x(nmax),y(nmax),z(nmax)
      character*80 spor,fnavn,defn
*------------------------------------------------------------------
      integer itape
      logical asc

      itape=55
      iflag=0
   
      kl=79

      call filgen(itape,spor,defn,fnavn,kl,asc,.true.)
      if(itape.le.0 .or.(.not.asc)) then
          iflag=11
          return
      end if


      if(ic.eq.3) then
       call lexyzfil(fnavn,kl,x,y,z,nmax,n,.true.,itape,nskip,iflag)
      else
         if(ic.eq.2) then
           call lexyfil(fnavn,kl,x,y,nmax,n,.true.,itape,nskip,iflag)
         else
           call lexfil(fnavn,kl,x,nmax,n,.true.,itape,nskip,iflag)
         end if
      end if

      return
      end
