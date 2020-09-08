*************************************************************************
      subroutine beglog(blogg,ilogg)
      integer ilogg
      logical blogg
*--------------------------------------------------------
      integer kf
      character*20 lnavn
      logical ja

      call initkom
      call setkomm('!')
      blogg=ja(blogg,'skal dialogen protokolleres?#')
      if(blogg) then
        kf=20
        call utfil(ilogg,'gi log-fil#','indat#',lnavn,kf)
        if(ilogg.gt.0) then
          write(ilogg,*) 'nei'
          call setlog(ilogg)
        else
          if(ilogg.eq.0) then          
             call setlog(ilogg)
          end if
        end if
      else
        ilogg=-2
      end if

      return
      end 



*************************************************************************
      subroutine beglogv(blogg,ilogg,defn)
      integer ilogg
      logical blogg
      character*80 defn
*--------------------------------------------------------
      integer kf
      character*20 lnavn
      logical ja

      call initkom
      call setkomm('!')
      blogg=ja(blogg,'skal dialogen protokolleres?#')
      if(blogg) then
        kf=20
        call utfil(ilogg,'gi log-fil#',defn,lnavn,kf)
        if(ilogg.gt.0) then
          write(ilogg,*) 'nei'
          call setlog(ilogg)
        else
          if(ilogg.eq.0) then          
             call setlog(ilogg)
          end if
        end if
      else
        ilogg=-2
      end if

      return
      end 


*************************************************************************
*
*       Denne gj|r det samme som beglog men skriver intet p} fila og
*       kaller heller ikke initkom. Rutinen er beregnet for bruk
*       etter at dialogen er startet.
************************************************************************
      subroutine openlog(blogg,ilogg)
      integer ilogg
      logical blogg
*--------------------------------------------------------
      integer kf
      character*20 lnavn
      logical ja

      call setkomm('!')
      blogg=ja(blogg,'skal dialogen protokolleres?#')
      if(blogg) then
        kf=20
        call utfil(ilogg,'gi log-fil#','indat#',lnavn,kf)
        if(ilogg.gt.0) then
          call setlog(ilogg)
        else
          if(ilogg.eq.0) then          
             call setlog(ilogg)
          end if
        end if
      else
        ilogg=-2
      end if

      return
      end 


************************************************************************
      subroutine openlogv(blogg,ilogg,defn)
      integer ilogg
      logical blogg
      character*80 defn
*--------------------------------------------------------
      integer kf
      character*20 lnavn
      logical ja

      call setkomm('!')
      blogg=ja(blogg,'skal dialogen protokolleres?#')
      if(blogg) then
        kf=20
        call utfil(ilogg,'gi log-fil#',defn,lnavn,kf)
        if(ilogg.gt.0) then
          call setlog(ilogg)
        else
          if(ilogg.eq.0) then          
             call setlog(ilogg)
          end if
        end if
      else
        ilogg=-2
      end if

      return
      end 



****************************************************************************
*      E X P A N D
*
*   Expanderer filnavn pa c-shell vis dvs. at feks:
*
*        ~/h.dat      gir  $HOME/h.dat
*        ~rupert/foo  gir  /mn/kastor/home/u1/rupert/foo dersom home-dir
*                      er /mn/kastor/home/u1/meg
*        ../kla.bla   gir kla.bla ett direktori opp i forhold til default
*
*  'Initkom' m} v{re kalt f|r f|rste kall p} 'expand'
*  parametere:
*       t(1:nt)  - tekst som skal expanderes                          I
*       tut(1:nut) - expandert tekst                                  O
*       iflag - feilparameter, iflag >0 betyr ulovlig innhold i t     O
*
**************************************************************************
      subroutine expand(t,nt,tut,nut,iflag)
      integer nt,nut,iflag
      character*300 t,tut
      include 'styr.inc'
*--------------------------------------
      integer mg,nb,kflag

      iflag=0

      if(t(1:1).ne.'.' .and. t(1:1).ne.'~') then
        nut=nt
        tut(1:nt)=t(1:nt)
cc        write(0,*)'tut ',tut(1:nt)
        return
      end if

      if(t(1:1).eq.'~') then
        mg=nhd
        if(t(2:2).eq.'/') then
          nb=3
          if(nt.le.2) then
            iflag=1
            return
          end if
        else
          nb=2
          call fsok(homedir,nhd,mg,'/',-1,kflag)
        end if

cc        write(0,*)'home= ',homedir(1:mg) 
        nut=mg+nt+1-nb       
        tut(1:mg)=homedir(1:mg)
        tut(mg+1:nut)=t(nb:nt)
        return
      end if

c  naa er . foerste tegn


      mg=ndd
      nb=1

 100  continue

      if(t(nb:nb).ne.'.' .or. nb.eq.nt) go to 200
      if(t(nb+1:nb+1).eq.'/') then
        nb=nb+2
        if(nb.gt.nt) then
          iflag=2
          return
        end if
        go to 100
      end if
    
      if(t(nb+1:nb+1).ne.'.') then
        go to 200
      else
        if(nb+1.eq.nt) then
          iflag=3
          return
        end if
        if(t(nb+2:nb+2).eq.'/') then
          if(nb+2.eq.nt) then
            iflag=4
            return
          end if
          call  fsok(defdir,ndd,mg,'/',-1,kflag)
          nb=nb+3
        else
          go to 200
        end if
      end if
 
      go to 100

 200  continue

      nut=mg+nt+1-nb
      tut(1:mg)=defdir(1:mg)
      tut(mg+1:nut)=t(nb:nt)
      return
      end




*************************************************************************
      SUBROUTINE INNFILNY(ITAPE,SPOR,DEFN,NAVN,KF,asc)
      INTEGER ITAPE,KF
      CHARACTER*80 SPOR,DEFN,NAVN
      logical asc
      include 'styr.inc'
*-----------------------------------------------------------------------
      CHARACTER*80 HNAVN
      LOGICAL FORK
      INTEGER KF0,kfex,iflag
                 
      KF0=KF
 100  continue
      CALL BLANK(HNAVN,KF0)
      CALL LESORD(DEFN,SPOR,HNAVN,KF)

      IF(KF.GT.KF0) THEN
        WRITE(IUSTRR,11) ' ',KF,KF0
        if(aktlog.eq.1 .and. aktkom.eq.1)
     %    WRITE(ltape,11) charkom,KF,KF0
  11    FORMAT(1X,a1,'LENGDE:',I4,'  MAX. LENGDE:',I4)
        call primvri('GI NYTT OG KORTERE NAVN#')
        GO TO 100
      ELSE       
        CALL BLANK(NAVN,KF0)
        NAVN(1:KF)=HNAVN(1:KF)
      END IF

      IF(FORK('STOPP#',NAVN,1,KF)) THEN
        ITAPE=-1
        RETURN
      END IF


      IF(FORK('TTY#',NAVN,1,KF).and. asc) THEN
        ITAPE=5
        RETURN
      END IF

      IF(FORK('TERMINAL#',NAVN,1,KF) .and. asc) THEN
        ITAPE=5
        RETURN
      END IF   

      IF(FORK('STANDARD#',NAVN,1,KF) .and. asc) THEN
        ITAPE=5
        RETURN
      END IF   

      call expand(navn,kf,hnavn,kfex,iflag)
      if(iflag.gt.0) then
        call primvri('ulovlig filnavn#')
        go to 100
      end if

      if(kfex.le.kf0) then
        kf=kfex
        navn(1:kf)=hnavn(1:kf)
      end if



      if(asc) then
        open(UNIT=itape,file=navn(1:kf),ERR=117,status='old',
     %  form='formatted')
      else
        open(UNIT=itape,file=navn(1:kf),ERR=117,status='old',
     %  form='unformatted')
      end if
      return
  117 call primvri('fil ikke funnet#')
      call primvri('gi nytt navn ("stopp" dersom du gir opp)#')
      go to 100

      end


*************************************************************************
      SUBROUTINE INNFIL(ITAPE,SPOR,DEFN,NAVN,KF)
      INTEGER ITAPE,KF
      CHARACTER*80 SPOR,DEFN,NAVN
      include 'styr.inc'
*-----------------------------------------------------------------------
      CHARACTER*80 HNAVN
      LOGICAL FORK
      INTEGER KF0
                 
      KF0=KF
 100  continue
      CALL BLANK(HNAVN,KF0)
      CALL LESORD(DEFN,SPOR,HNAVN,KF)

      IF(KF.GT.KF0) THEN
        WRITE(IUSTRR,11) ' ',KF,KF0
        if(aktlog.eq.1 .and. aktkom.eq.1)
     %   WRITE(ltape,11) charkom,KF,KF0
  11    FORMAT(1X,a1,'LENGDE:',I4,'  MAX. LENGDE:',I4)
        call primvri('GI NYTT OG KORTERE NAVN#')
        GO TO 100
      ELSE       
        CALL BLANK(NAVN,KF0)
        NAVN(1:KF)=HNAVN(1:KF)
      END IF

      IF(FORK('STOPP#',NAVN,1,KF)) THEN
        ITAPE=-1
        RETURN
      END IF


      IF(FORK('TTY#',NAVN,1,KF)) THEN
        ITAPE=5
        RETURN
      END IF

      IF(FORK('TERMINAL#',NAVN,1,KF)) THEN
        ITAPE=5
        RETURN
      END IF   

      IF(FORK('STANDARD#',NAVN,1,KF)) THEN
        ITAPE=5
        RETURN
      END IF   


      open(UNIT=itape,file=navn(1:kf0),ERR=117,status='old')
      return
  117 call primvri('fil ikke funnet#')
      call primvri('gi nytt navn ("stopp" dersom du gir opp)#')
      go to 100

      end


*************************************************************************
      SUBROUTINE UTFIL(ITAPE,SPOR,DEFN,NAVN,KF)
      INTEGER ITAPE,KF
      CHARACTER*80 SPOR,DEFN,NAVN
      include 'styr.inc'
*-----------------------------------------------------------------------
      CHARACTER*80 HNAVN
      LOGICAL FORK
      INTEGER KF0
                 
      KF0=KF

  100 CONTINUE               

      CALL BLANK(HNAVN,80)                         
      CALL LESORD(DEFN,SPOR,HNAVN,KF)

      IF(KF.GT.KF0) THEN
        WRITE(IUSTRR,11) ' ',KF,KF0
        if(aktlog.eq.1 . and. aktkom.eq.1)
     %        WRITE(ltape,11)charkom,KF,KF0
  11    FORMAT(1X,a1,'LENGDE:',I4,'  MAX. LENGDE:',I4)
        call primvri('GI NYTT OG KORTERE NAVN#')
        GO TO 100
      ELSE       
        CALL BLANK(NAVN,KF0)
        NAVN(1:KF)=HNAVN(1:KF)
      END IF

      IF(FORK('INGEN#',NAVN,1,KF)) THEN
        ITAPE=-1
        RETURN
      END IF

      IF(FORK('TTY#',NAVN,1,KF)) THEN
        ITAPE=iustrr
        RETURN
      END IF

      IF(FORK('TERMINAL#',NAVN,1,KF)) THEN
        ITAPE=iustrr
        RETURN
      END IF   

      IF(FORK('ERROR#',NAVN,1,KF)) THEN
        ITAPE=iustrr
        RETURN
      END IF   

      IF(FORK('OUTPUT#',NAVN,1,KF)) THEN
        ITAPE=6
        RETURN
      END IF   

      open(UNIT=itape,file=navn(1:kf),status='unknown')
      return

      end


************************************************************
      subroutine lukk(itape)
      integer itape
      include 'styr.inc'

      if(itape.ge.0 .and. itape.ne.5 .and. itape.ne.6 
     %    .and. itape.ne.iustrr) then
        close(itape)
        itape=-1
      end if
      return
      end



***********************************************************
*   
*        NUMOPP
*  
*   Aapner fil med navn avledet av stamme pluss nummer.
*   feks. vil nummopp('gagga#',17,23) aapne fil med navn
*   gagga17 og enhet 23.
*   Parametere:
*    stem - navnestamme, avsluttet med # eller !    I
*    isyk - nummer                                    I   
*    itape - unitnummer                              I
********************************************************
      subroutine nummop(stem,isyk,itape)
      integer isyk,itape
      character*80 stem
*--------------------------------------------------------
      integer kf,kf2
      character*80 tall,utfil
      logical def

      call itconv(isyk,kf,tall)
      call xstrip(stem,kf2,def)
      utfil(1:kf2)=stem(1:kf2)
      utfil(kf2+1:kf2+kf)=tall(1:kf)
      kf=kf2+kf

      open(unit=itape,file=utfil(1:kf),status='unknown')
      return
      end

***********************************************************
*   
*        NUMMOPG
*  
*   Aapner fil med navn avledet av stamme pluss nummer.
*   feks. vil nummopp('gagga#',17,23,.true.) aapne ascii fil med navn
*   gagga17 og enhet 23.
*   Parametere:
*    stem - navnestamme, avsluttet med # eller !    I
*    isyk - nummer                                    I   
*    itape - unitnummer                              I
*    asc - true gir asci, false binaert
********************************************************
      subroutine nummopg(stem,isyk,itape,asc)
      integer isyk,itape
      character*80 stem
      logical asc
*--------------------------------------------------------
      integer kf,kf2
      character*80 tall,utfil
      logical def

      call itconv(isyk,kf,tall)
      call xstrip(stem,kf2,def)
      utfil(1:kf2)=stem(1:kf2)
      utfil(kf2+1:kf2+kf)=tall(1:kf)
      kf=kf2+kf

      if(asc) then
        open(unit=itape,file=utfil(1:kf),status='unknown')
      else
        open(unit=itape,file=utfil(1:kf),status='unknown',
     %       form='unformatted')
      end if
      return
      end




***********************************************************
*   
*        NOPPG
*  
*   Aapner fil med navn avledet av stamme pluss nummer.
*   feks. vil nummopp('gagga#',17,23,.true.) aapne ascii fil med navn
*   gagga17 og enhet 23.
*   Parametere:
*    stem - navnestamme, avsluttet med # eller !    I
*            med ! legges ikke isyk til navn
*    isyk - nummer                                    I   
*    itape - unitnummer                              I
*    asc - true gir asci, false binaert
********************************************************
      subroutine noppg(stem,isyk,itape,asc)
      integer isyk,itape
      character*80 stem
      logical asc
*--------------------------------------------------------
      integer kf,kf2
      character*80 tall,utfil
      logical def

      call xstrip(stem,kf2,def)
      utfil(1:kf2)=stem(1:kf2)

      if(def) then
        call itconv(isyk,kf,tall)
        utfil(kf2+1:kf2+kf)=tall(1:kf)
        kf=kf2+kf
      else
         kf=kf2
      end if


      if(asc) then
        open(unit=itape,file=utfil(1:kf),status='unknown')
      else
        open(unit=itape,file=utfil(1:kf),status='unknown',
     %       form='unformatted')
      end if
      return
      end














