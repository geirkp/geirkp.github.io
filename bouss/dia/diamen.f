**************************************************************************
*              MENYSYSTEM.
*   De etterf|lgende rutiner styrer oppbygning og bruk av et kommando-
*   system bygd opp som et hierarki av menyer. En kommandopost ser
*   ut som   <ordre> <spes1> <spes2>...<spesN> der <ordre> er en
*   sekvens som begynner med en bokstav og <spes1> etc. er tall.
*   <ordre> legges inn med rutinen 'setordre' og spesifikasjonene
*   (spes1 etc) opprettes med 'setspes'. Alle ordrer m} h|re
*   hjemme i en meny. Menyer opprettes med 'setmen' og en meny
*   m} v{re opprettet f|r den tilegnes ordre.
*    
*   Menyene er organisert i ett eller flere hierarkier ved at hver
*   ordre i en meny tilordnes en peker til en undermeny. Kommandoene
*   kan kjedes sammen etter m|nsteret:
*        <ordre1><spes...> <ordre2><spes...> <ordre3><spes..>
*   der ordre2 tilh|rer undermenyen til ordre1 etc. Spesifikasjonene
*   kan utelates dersom standardverdier er tilgjengelige. Dialogen
*   som er styrt ved 'kom' kan settes i sekvensiell mode ved kall p}
*   'setsekvens'. Dvs. at ordrer sp|rres om og leses en etter en,
*    og at spesifikasjonene gis p} egen linje etter at ordren de tilh|rer
*   er lest.
*
*   Noen punkter:
*       $ 'initmeny' m} kalles f|r noen andre rutiner
*       $ deler av et svar eller kommandopost adskilles alltid  av
*         en eller flere blanke.
*       $ ingen ordrer kan v{re over 80 lange.
*       $ hele kommandoskjeder og ikke bare neste kommando kan velges
*         standard dersom siste linje lest fra terminalen tar slutt
*         f|r noen feil er funnet eller en ordre uten undermeny er lest.
*         Dette gjelder ikke dersom dialogen er i sekvensiell mode.
*         V{r forsiktig med standardverdier.
*       $ Systemet bygger p} dia-rutinene for lesing og behandling
*         av tekst og tall.
*       $ All behandling av systemets registere er ment } foreg}
*         gjennom kall p} styrerutiner.
***************************************************************************
*---------------------------------------------------------------------------








***************************************************************************
*
*              S E T S E K V E N S
*
*     Dersom argument 'a' er sann settes dialogen til } v{re 
*     sekvensiell, dvs. at kommandoer bare kan gis en om gangen og at
*     spesifikasjoner gis separat etter kommandoen. 'a' lik usann setter
*     dialogen til simultan.
*************************************************************************
      subroutine setsekvens(a)
      logical a
      include 'men.inc'
*---------------------------------------------------------
      sekvens=a
      return
      end


***************************************************************************
*
*                  I N I T M E N Y 
*
*     Denne initialiserer menysystemets registere og m} kalles
*     f|r noen annen rutine som ber|rer systemet.
***************************************************************************
      subroutine initmeny
      include 'men.inc'
*-------------------------------------------------------------------------
      integer i

      sekvens=.false.

      eksmen(1)='HJELP# gir denne utskrift'
      eksmen(2)='LISTE# lister alle ordrer p} dette niv}et'
      eksmen(3)='AVBRYT# kommandosekvens avbrytes og startes p} nytt'
      eksmen(4)='VIS_SPESIFIKASJON# spes. for etterf. ordre listes'

      do 100 i=1,mordre
      spes(i)='#'
      menpek(i)=0
  100 continue

      do 200 i=1,mmen
      meny(i)=0
      menl(i)=0
  200 continue

      iopptatt=0
      menant=0 
      iint=0
      ireal=0
      ifeil=0
      ikommando=0
      menstart=1
              
      return
      end    

****************************************************************************
*
*               S P E S S J E K K
*
*     Rutinen plasserer spesifikasjoner fra teksten 't' inn i registere
*     tilh|rende kommando 'ikomm'.
*     parametere:                                        
*            ikomm    -   kommandonummer (globalt)                    I 
*            t        -   tekststreng inneholdene tallspes.           I
*            l        -   array med posisjoner av ord i 't'.          I
*                         l gis verdier ved kall p} 'deltek'
*            iseq     -   antall sekvenser i 't'                      I
*            ibeg     -   f|rste sekvens som tolkes som spesifik.     I
*            iend     -   siste sekvens som er behandlet              O
*            iflag    -   feilparameter                               O
*                       =0    alt  i orden
*                       =1    ikke eksterende standardverdier fors|kt
*                             benyttet.
*                       =2    ikke nok korrekte tall
*                       =3    for mange tall
*****************************************************************************

      subroutine spessjekk(ikomm,t,l,iseq,ibeg,iend,iflag)
      include 'men.inc'
      integer ikomm,iseq,ibeg,iend,iflag
      integer l(2,iseq)
      character*80 t
*--------------------------------------------------------------------------
      integer ihelt,iflyt,ierr,ibuff(10),iant,na,nb,ia,ib
      integer ix,i,i0
      real rbuff(10),rx
      character c
      logical def,nomore,digit,bkast
                            
      iflag=0

      def=nrordre(ikomm,1).gt.0 

      call xstrip(spes(ikomm),iant,bkast)

      if(ibeg.gt.iseq) then
        nomore=.true.      
      else                
        na=l(1,ibeg)
        c=t(na:na)
        nomore=(.not.digit(c))  .and.  c.ne.'+'  .and.  c.ne.'-' 
     %          .and.  c.ne.'.'
      end if

      if(nomore) then
        if(.not.(nrordre(ikomm,1).gt.0  .or. iant.eq.0)) iflag=1
        iend=ibeg-1
        return
      end if
                 
      if(ibeg+iant-1.gt.iseq) then
        iflag=2
        iend=iseq
        return
      end if


      ihelt=0
      iflyt=0

      do 100 i=1,iant  
        c=spes(ikomm)(i:i)
        na=l(1,ibeg+i-1)
        nb=l(2,ibeg+i-1)

        if(c.eq.'I') then
          call geti(t,na,nb,ia,ib,ix,ierr)
          if(ierr.ne.1) then
            iflag=2
            iend=ibeg-1
            return
          end if
          ihelt=ihelt+1
          ibuff(ihelt)=ix
        else  
          call getr(t,na,nb,ia,ib,rx,ierr)
          if(ierr.ne.1) then
            iflag=2
            iend=ibeg-1
            return
          end if 
          iflyt=iflyt+1
          rbuff(iflyt)=rx 
        end if

  100 continue
 

      if(iseq.gt.(ibeg+iant-1)) then
        na=l(1,ibeg+iant)
        c=t(na:na)
        if(digit(c)) then
          iflag=3
          iend=ibeg-1
          return
        end if
      end if
  

      iend=ibeg+iant-1

      i0=nrordre(ikomm,2)-1
      do 200 i=1,ihelt
        ispes(i0+i)=ibuff(i)
  200 continue

      i0=nrordre(ikomm,3)-1
      do 300 i=1,iflyt
        rspes(i0+i)=rbuff(i)
  300 continue

      if(nrordre(ikomm,1).gt.-1) nrordre(ikomm,1)=nrordre(ikomm,1)+1

      
      return
      end


****************************************************************************
*
*                    S E T M E N
*
*     Definerer en ny meny.
*     parametere:
*          spor  -  sp|rsm}l som stilles ved innlesning av ordre i     I
*                   menyen. Avsluttes med: '#' dersom standardverdi
*                   tillates, med '!' ellers.                              
*          nomax -  maksimalt antall ordrer i menyen                   I
*          numm  -  menynummer                                         O
*          iflag -  dersom meny er opprettet gis denne verdi 0         O     
*****************************************************************************
      subroutine setmen(spor,nomax,numm,iflag)
      include 'men.inc'
      integer nomax,numm,iflag
      character*80 spor
*--------------------------------------------------------------------------
      integer kf
      logical def ,bkast
          
      iflag=0
        
      numm=menant+1                             
      if(numm.gt.mmen .or. (iopptatt+nomax).gt.mordre) then
        iflag=1
        return
      end if                                    

      menant=numm
      meny(numm)=iopptatt+1                      
      menl(numm)=0
      iopptatt=iopptatt+nomax
     
      call xstrip(spor,kf,def)
      if(def) then
        mendef(numm)=0
      else
        mendef(numm)=-1
      end if

      call blank(menspor(numm),80)
      menspor(numm)(1:kf+1)=spor(1:kf+1)

      return
      end        

**************************************************************************** 
*
*                 S E T O R D R E
*
*     Definerer en ordre i en meny
*     parametere:
*          spor - selve ordren. Dersom denne skal kunne brukes som       I
*                 standardverdi m} den avsluttes med '#', hvis ikke
*                 med '!' 
*          bes  - beskrivelse av ordre, avsluttes med '#'. 'spor'        I
*                 pluss 'bes' b|r ikke inneholde mer enn 80 tegn
*                 tilsammen.
*          imen - Meny ordren skal tilh|re.                              I
*          nestemen - nummer for ordrens undermeny. Dersom verdien er    I
*                     0 eller -1 har ordren ingen undermeny. 0 tillater
*                     at kommandoposten etterf|lges av en rest som taes
*                     ut som output av 'kom'. -1 krever at posten
*                     etterf|lges av blanke.
*          numm - Globalt nummer tildelt ordren                          O
*          iflag - Feilparameter.                                        O
*                =0 alt i orden
*                =1 'imen' for stor
*                =2 'imen' svarer ikke til eksisterende meny
*                =3 antall ordrer tilh|rende 'imen' blir for stort
***************************************************************************** 
*
      subroutine setordre(spor,bes,imen,nestemen,numm,iflag)
      include 'men.inc'
      integer imen,nestemen,numm,iflag
      character*80 spor,bes
*--------------------------------------------------------------------------
      integer kf,ierr,kf1,nomax
      logical def ,bkast
          
      iflag=0
         
      if(imen.gt.mmen) then
        iflag=1
        return
      end if
  
      if(meny(imen).eq.0) then
        iflag=2
        return
      end if
                               
      if(imen.eq.mmen) then
        nomax=mordre                            
      else
        if(meny(imen+1).eq.0) then
          nomax=mordre
        else
          nomax=meny(imen+1)-1
        end if
      end if
      numm= meny(imen)+menl(imen)                    
      if(numm.gt.nomax) then
        iflag=3
        return
      end if                                    

      menl(imen)=menl(imen)+1
     
      call xstrip(spor,kf,def)

      call blank(ordre(numm),80)
      ordre(numm)(1:kf+1)=spor(1:kf+1)
      call xstrip(bes,kf1,bkast)
      if(kf1+kf+1.gt.80) kf1=78-kf
      if(kf1.gt.0) ordre(numm)(kf+2:kf1+2+kf)  = bes(1:kf1)

      spes(numm)='#'                            
      menpek(numm)=nestemen

      return
      end           

****************************************************************************
*
*               S E T S P E S
*
*     Setter spesifikasjon for en kommando.
*     parametere:
*           ikomm   -  kommandonummer (globalt)                          I
*           spesi   -  angir hvordan spesifikasjoner skal leses.         I
*                      Strengen 'IRRI#' betyr feks. at tallene gis som:
*                      helt reelt reelt helt. Avsluttes med '#' vil
*                      sist innlagte tallsett v{re standardverdier,
*                      avsluttes med '!' vil standardverdier aldri bli
*                      benyttet. Maksimalt antall spesifikasjoner er 9.
*           spor    -  sp|rsm}l som stilles etter spesifikasjon.         I
*                      avsluttes med '#'
*           bes     -  beskrivelse av sp|rsm}l. lengde av spor og        I
*                      bes b|r i sum ikke overskride 80. Avsluttes med
*                      '#'
*           iflag   -  feilparameter                                     O
*                   =  0     alt i orden
*                   =  1     for lang spesifikasjon
*                   =  2     ulovlig spesifikasjon
*                   =  3     registere er fulle
*******************************************************************************
      subroutine setspes(ikomm,spesi,spor,bes,iflag)
      include 'men.inc'
      integer ikomm,iflag
      character*10 spesi
      character*80 spor,bes
*--------------------------------------------------------------------------
      integer ihelt,iflyt,kf,ierr,kf1
      logical def ,bkast
      character*10 spesil
          
      iflag=0
             
      call xstrip(spesi,kf,def)
                        
  
      if(kf.gt.9) then
        iflag=1
        return
      end if            

      spesil(1:kf+1)=spesi(1:kf+1)
      if(kf.gt.0) call upchar(spesil,1,kf)

      call suspes(spesil,ihelt,iflyt,ierr)
                                 
      if(ierr.gt.0) then
        iflag=2
        return
      end if
            
      if(ireal+iflyt.gt.mspes  .or. iint+ihelt.gt.mspes) then
        iflag=3
        return
      end if

      if(def) then
        nrordre(ikomm,1)=0
      else
        nrordre(ikomm,1)=-1
      end if
                     
                 
      spes(ikomm)(1:kf+1)=spesil(1:kf+1)
      call xstrip(spor,kf,bkast)
      call xstrip(bes,kf1,bkast)
      call blank(spesspor(ikomm),80) 
      spesspor(ikomm)(1:kf+1)=spor(1:kf+1)
      if(kf+kf1+1 .gt.80) kf1=78-kf
      if(kf1.gt.0) spesspor(ikomm)(kf+2:kf+2+kf1)=bes(1:kf1)
      nrordre(ikomm,2)=iint+1
      nrordre(ikomm,3)=ireal+1
      ireal=ireal+iflyt                                                   
      iint=iint+ihelt
 
      return
      end
                                                                       


************************************************************************       
*
*               S U S P E S                                               
*
*     Teller antall flytende og heltall svarende til en streng med 
*     spesifikasjoner.
*        spesil - streng med spesifikasjoner, avsluttes med '#'       I
*                 eller '!' Maks antall=9.
*        ihelt,iflyt - antall av hhv hele og reelle tall.             O
*        iflag  - feilparameter, verdi=1 betyr at det er feil i       O
*                 spesifikasjonene.
************************************************************************
      subroutine suspes(spesil,ihelt,iflyt,iflag)
      integer ihelt,iflyt,iflag
      character*10 spesil
*---------------------------------------------------------------------
      integer kf,i             
      character c  
      logical bkast
                
      iflag=0

      call xstrip(spesil,kf,bkast)
                   
      ihelt=0
      iflyt=0

      do 100 i=1,kf  
        c=spesil(i:i)
        if(c.eq.'I') then
          ihelt=ihelt+1
        else  
          if(c.eq.'R') then
            iflyt=iflyt+1
          else
            iflag=1
            return
          end if
        end if

  100 continue
 
      return
      end



***************************************************************************
*
*                   S P O R S P E S
*
*         Sp|r om spesifikasjoner til en kommando og plasserer svaret i
*         registere.
*         parametere:
*            ikomm   -  kommandonummer (globalt)                         I
*            iflag   -  dersom verdien er 1 har brukeren avbrutt         O
*                       innlesning og registere er ikke endret.
***************************************************************************
      subroutine sporspes(ikomm,iflag)
      include 'men.inc'
      integer ikomm,iflag
*--------------------------------------------------------------------------
      integer l(2,20),kf,kf1,iseq,ib,ie,ierr
      character*80 t,sp 
      logical bkast,fork
      integer iustrr

      call stdrror(iustrr)
                       
      iflag=0

      call xstrip(spes(ikomm),kf1,bkast)
      if(kf1.eq.0) return
      sp(1:80)=spesspor(ikomm)
      call xstrip(sp,kf,bkast)
      if(kf.lt.68) then     
        sp(kf+1:kf+1)='['
        sp(kf+2:kf+1+kf1)=spes(ikomm)(1:kf1)
        sp(kf+2+kf1:kf+2+kf1)=']'
        if(nrordre(ikomm,1).gt.0) then
          sp(kf+3+kf1:kf+3+kf1)='#'
        else
          sp(kf+3+kf1:kf+3+kf1)='!'
        end if
      end if
         
  100 continue

      call lestek('gamle verd.#',sp,t)
      call deltek(t,1,80,l,iseq) 

      if(iseq.eq.1) then
        if(fork('avbryt#',t,l(1,1),l(2,1))) then
          iflag=1
          return
        end if
      end if
                                         
      ib=1
      call spessjekk(ikomm,t,l,iseq,ib,ie,ierr)
      if(ierr.ne.0) then
        write(iustrr,*)'ukorrekt format'
        go to 100
      end if
 
      return
      end




****************************************************************************
*
*              I N U M
*
*     Funksjonen returnerer globalt kommando nummer til lokal kommando
*     'ilokal' i meny nr. 'imeny'. Returnert verdi lik null betyr at 
*     input svarer til en ikke-eksisterende kommando.
****************************************************************************
      function inum(imeny,ilokal)
      include 'men.inc'
      integer inum,imeny,ilokal
*-------------------------------------------------------------
      
      if(imeny.gt.mmen) then
        inum=0
        return
      end if

      if(ilokal.gt.menl(imeny)) then
        inum=0
        return
      end if
                    
      inum=meny(imeny)+ilokal-1
      return
      end


*************************************************************************
*
*          P U T T S P E S
*
*      Henter eller plasserer spesifikasjoner for en kommando.
*
*      parametere
*          ikomm - globalt kommandonummer                                I
*          ibuff,rbuff - arrayer som inneholder hhv. heltallsverdier     I/O
*                        og reelle verdier som hentes eller plasseres.
*          ihelt,iflyt - antall av hhv. hele og reelle tall              O
*          hent - dersom hent er sann hentes tall, hvis ikke plasseres   I
*                 tallene
****************************************************************************
      subroutine puttspes(ikomm,ibuff,rbuff,ihelt,iflyt,hent)
      include 'men.inc'
      integer ikomm,ibuff(10),ihelt,iflyt
      real rbuff(10)
      logical hent
*---------------------------------------------------------------------------
      integer i,i0,iflag
      character*10 sp
     
      sp(1:10)=spes(ikomm)(1:10)
      call suspes(sp,ihelt,iflyt,iflag)
      if(ihelt+iflyt.eq.0) return

      i0=nrordre(ikomm,2)
      do 100 i=1,ihelt
        if(hent) then
          ibuff(i)=ispes(i0+i-1)
        else
          ispes(i0+i-1)=ibuff(i)
        end if
  100 continue

      i0=nrordre(ikomm,3)
      do 200 i=1,iflyt
        if(hent) then
          rbuff(i)=rspes(i0+i-1)
        else
          rspes(i0+i-1)=rbuff(i)
        end if
  200 continue

      if((.not.hent) .and. nrordre(ikomm,1).ne.-1) 
     %     nrordre(ikomm,1)=nrordre(ikomm,1)+1

      return
      end


********************************************************************
*
*                 P R I N T S P E S
*
*    Rutinen printer spesifikasjoner for kommando med globalt nummer
*    ikomm.
*********************************************************************
      subroutine printspes(ikomm)
      include 'men.inc'
      integer ikomm
*------------------------------------------------------------------- 
      integer ihelt,iflyt,ith,rth,ibuff(10),itot,i
      integer kf
      real rbuff(10)
      character*10 sp                   
      logical bkast
      integer iustrr

      call stdrror(iustrr)

                 
      if(ikomm.gt.mordre) then
        write(iustrr,*)' henvist til for stort ordrenummer'
        return
      end if

      sp=spes(ikomm)              
      call puttspes(ikomm,ibuff,rbuff,ihelt,iflyt,.true.)

      call xstrip(ordre(ikomm),kf,bkast)

      if(kf.eq.0) then
        write(iustrr,*)' henvist til ikke-opprettet ordrenummer'
        return
      end if

      write(iustrr,*)'spes. for: /',ordre(ikomm)(1:kf),'/'

      itot=iflyt+ihelt     
      ith=0
      rth=0
      do 100 i=1,itot                         
        if(sp(i:i).eq.'I') then
          ith=ith+1
          write(iustrr,50) ibuff(ith)
        else
          rth=rth+1
          write(iustrr,60) rbuff(rth)
        end if
  100 continue
     
   50 format(1x,i10)
   60 format(14x,E15.6)
      return
      end



**************************************************************************
*
*              S P U N D E F
*
*     Rutinen fjerner defaultsetting for spesifik. til kommando 'ikomm'
**************************************************************************
      subroutine spundef(ikomm)
      include 'men.inc'
      integer ikomm
*------------------------------------------------------------------------
      integer iustrr

      call stdrror(iustrr)

      if(ikomm.gt.mordre) then
       write(iustrr,*)'fors|k p} behandling av for h|yt kommando-nr'
       return
      end if

      if(nrordre(ikomm,1).gt.0) nrordre(ikomm,1)=0

      return
      end


***************************************************************************
*
*                 defset
*
*  Rutinen setter en ordre til } v{re standardsvar i en meny.
*   parametere:
*       imen   - menynummer                                         I
*       iordre - globalt ordrenummer                                I
*       iflag  - feilparameter                                      O
*              =0   alt i orden
*              =1   menynummmer eksisterer ikke
*              =2   ordre eksisterer ikke i angitte meny
*              =3   ordre kan ikke benyttes som standard
***********************************************************************
      subroutine defset(imen,iordre,iflag)
      integer imen,iordre,iflag
      include 'men.inc'
*-----------------------------------------------------------------------
      integer ilok,kf
      logical def

      iflag=0

      if(imen.gt.menant .or. imen.le.0) then
        iflag=1
        return
      end if
 
      ilok=iordre+1-meny(imen)

      if(ilok.lt.0 .or. ilok.gt.menl(imen)) then
        iflag=2
        return
      end if

      call xstrip(ordre(iordre),kf,def)
      
      if(def) then
        mendef(imen)=ilok
      else
        iflag=3
      end if
      return
      end



**************************************************************************
*
*              M E U N D E F
*                                          
*     Rutinen fjerner defaultsetting for meny nr 'imen'
**************************************************************************
      subroutine meundef(imen)
      include 'men.inc'
      integer imen
*------------------------------------------------------------------------
      integer iustrr

      call stdrror(iustrr)

      if(imen.gt.mmen) then
       write(iustrr,*)'fors|k p} behandling av for h|yt meny-nr'
       return
      end if

      if(mendef(imen).gt.0) mendef(imen)=0

      return
      end
                                  

****************************************************************************
*
*           T O P P M E N
*
*     Setter meny nr 'imen' til } v{re |verste meny i hierarkiet
***************************************************************************
      subroutine toppmen(imen)
      include 'men.inc'
      integer imen
*------------------------------------------------------------------------
      integer iustrr

      call stdrror(iustrr)

      if(imen.gt.mmen) then
       write(iustrr,*)'fors|k p} behandling av for h|yt meny-nr'
       return
      end if

      if(menl(imen).eq.0) then     
       write(iustrr,*)'fors|k p} } sette tom meny som toppmeny'
       return
      end if                                             

      menstart=imen
      return
      end 





**************************************************************************
*
*              M E N Y L I S
*
*      Lister ut meny nr kmen
**************************************************************************
      subroutine menylis(kmen) 
      include 'men.inc'
      integer kmen
*-------------------------------------------------------------------------
      character*10 add
      integer i,istop,i0,kf,ii
      logical bkast
      integer iustrr

      call stdrror(iustrr)


      write(iustrr,*)' '
      write(iustrr,*)' '
      write(iustrr,*)' '
      write(iustrr,*)'meny nr:',kmen
      istop=menl(kmen)   
      i0=meny(kmen)

      do 100 i=1,istop
      ii=i0+i-1
      write(iustrr,*) ordre(ii)(1:78)
      call xstrip(spes(ii),kf,bkast)      
      if(kf.gt.0) then 
        kf=kf+1
        add(1:kf)=spes(ii)(1:kf)
      else
        kf=5
        add(1:kf)='ingen'
      end if
      write(iustrr,55) add(1:kf)
   55 format(1x,'          spesifikasjoner:',a10)
  100 continue
      return
      end

      
****************************************************************************
*
*                 K O M
*
*      leser en kommando sekvens og oppdaterer registere for
*      spesifikasjoner.
*      parametere:
*            iord - iord(1)...iord(ioant) inneholder ved utgang            O
*                   lokale nummere for de leste kommandoer.    
*            imen - imen(1)...imen(ioant) inneholder ved utgang            O
*                   menynummere for de leste kommandoer.    
*            ioant- antall kommandoer i sekvensen.                         O
*            rest - ubehandlet del av siste leste svar som eventuelt       O
*                   kan brukes/tolkes senere.
**************************************************************************** 
      subroutine kom(iord,imen,ioant,rest)
      integer iord(10),imen(10),ioant
      character*80 rest
      include 'men.inc'
*-------------------------------------------------------------------------
      integer kmen,i0,idef,kf,l(2,30),iseq,na,nb,ikt,i
      integer ktreff(20),iat,ibeg,iend,iflag,ikomm    
      integer inum
      character*80 sp,tdef,tk,presord
      logical bkast,fork,ja
      integer iustrr

      call stdrror(iustrr)

                  
ccccc      forklaring av tellere/variabler  ccccccccccccccccccccccccccccccc
c                                 
c     i0 -  absolutt posisjon av forste ordre i n}v{rende meny
c     kmen -   nummer p} n{v{rende meny
c     iseq - totalt antall sekvenser i tk
c     ibeg - neste sekvens som skal behandles.
c     iend - sekvens som sist er behandlet i spessjekk
c     ioant- angir fortl|pende antall lovlige kommandoer.
c     menstart - nummer for toppmeny i hierarkiet.
c     kf   - lengde av tekster
c     na,nb - grenser for sekvenser i tekster.
c     idef - nummer for standard ordre i n}v{rende meny.
c     iat  - antall treff i menyen. (er en dersom svaret er lovlig)
c     tk   - siste leste kommandolinje
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

   50 continue  
c
c       Start p} ytterste l|kke. Her startes innlesning av kommando
c       fra begynnelsen. Dersom brukeren avbryter en kommando-givning 
c       g}es det til denne adressen
c
      ioant=0
      kmen=menstart


  100 continue
c
c      Start p} nest ytterste l|kke. Behandling av kommandosekvens 
c      fortsetter ved innl|esning av ny linje fra terminal.
c      Denne adressen opps|kes dersom siste linje lest fra terminal
c      er slutt eller inneholdt en feil. Delen av linjen forut for feilen
c      annuleres ikke.
c
      i0=meny(kmen)
      idef=mendef(kmen)  
      call xstrip(menspor(kmen),kf,bkast)
      if(kf.gt.0) sp(1:kf)=menspor(kmen)(1:kf)
      if(idef.gt.0) then
        tdef=ordre(i0-1+idef)
        sp(kf+1:kf+1)='#'
      else
        tdef='#'
        sp(kf+1:kf+1)='!'
      end if
  
      call lestek(tdef,sp,tk)
      call deltek(tk,1,80,l,iseq)
      if(sekvens .and. iseq.gt.1) then
        write(iustrr,*)'gi kommandoene en om gangen!'
        go to 100
      end if  
                                                         
      na=l(1,1)
      nb=l(2,1)
                        
      ibeg=1

  200 continue        
c
c       Innerste l|kke. Her behandles en kommando
c       og dennes spesifikasjoner pr gjennomgang.
c 
      idef=mendef(kmen)
      if(ibeg.gt.iseq) then
        if(idef.le.0) go to 100
        ktreff(1)=idef
      else
        na=l(1,ibeg)
        nb=l(2,ibeg)
        call balac(ordre(i0),menl(kmen),tk,na,nb,ktreff,iat)
        if(iat.eq.0 ) then
c         er den gitte kommando en hjelp-kommando? I s} fall gis hjelp.
          call balac(eksmen,4,tk,na,nb,ktreff,iat)                     
          if(iat.eq.1) then
            ibeg=ibeg+1
            if(ktreff(1).eq.4) then        
              if(ibeg.gt.iseq) then
                 call lestek('#','spesifikasjon av hva:!',tk)
                 call deltek(tk,1,80,l,iseq)
                 ibeg=1
              end if
              if(iseq.gt.ibeg) then
                call xstrip(eksmen(4),kf,bkast)
                if(kf.eq.0) kf=1
                write(iustrr,*)'/',eksmen(4)(1:kf),
     %                         '/  m} etterf|lges av en'
                write(iustrr,*)'enkelt ordre'
              else           
                na=l(1,ibeg)
                nb=l(2,ibeg)
                call balac(ordre(i0),menl(kmen),tk,na,nb,ktreff,iat)
                if(iat.ne.1) then
                  write(iustrr,*)'feil eller upresis ordre'
                else
                  ikt=inum(kmen,ktreff(1))
                  call printspes(ikt)
                end if
              end if       
              go to 100
            end if     
            if(iseq.gt.ibeg) then
              write(iustrr,*)'ulovlig spesifikasjon av hjelpe-kommando'
              go to 100
            end if
            if(ktreff(1).eq.3) go to 50
            if(ktreff(1).eq.1) then
              do 120 i=1,4
              write(iustrr,*)eksmen(i)(1:80)
 120          continue
            end if
            if(ktreff(1).eq.2) call menylis(kmen)
            go to 100
          end if
        end if                            
        
        if(iat.eq.0) then
          write(iustrr,*)'ulovlig kommando:/',tk(na:nb),'/'
          go to 100
        end if
        if(iat.gt.1) then
          write(iustrr,*)'flertydig fork.:/',tk(na:nb),'/'
          go to 100
        end if                      
              
      end if


      ioant=ioant+1
      if(ioant.gt.10) then
        write(iustrr,*)'kommando for lang'
        go to 50
      end if

      imen(ioant)=kmen
      iord(ioant)=ktreff(1)
      if(idef.gt.-1) then
        call xstrip(ordre(ktreff(1)+i0-1),kf,bkast) 
        if(bkast) mendef(kmen) = ktreff(1)
      end if
      ibeg=ibeg+1
      ikomm=i0+ktreff(1)-1
      presord=ordre(ikomm)      
      
      if(.not.sekvens)
     % call spessjekk(ikomm,tk,l,iseq,ibeg,iend,iflag)
      if(iflag.ne.0 .or. sekvens) then
        if(iflag.ne.0) then
          call xstrip(presord,kf,bkast)                  
          if(kf.eq.0) kf=1
          write(iustrr,*)'feil spesifikasjon for:/',presord(1:kf),'/'
        end if
        call sporspes(ikomm,iflag)
        if(iflag.ne.0) go to 50
      end if     

      ibeg=iend+1
      kmen=menpek(ikomm)
        
      if(kmen.gt.0) then
        i0=meny(kmen)
        if(sekvens) go to 100
        go to 200
      end if
                              
      rest='#'
      if(kmen.eq.0 .and.ibeg.le.iseq) then
        na=l(1,ibeg)
        nb=l(2,iseq)
        kf=nb-na+1
        rest(1:kf)=tk(na:nb)
        if(kf.lt.80) rest(kf+1:kf+1)='#'
      end if  

      if(kmen.lt.0 .and.ibeg.le.iseq) then
        na=l(1,ibeg)
        nb=l(2,iseq)
        write(iustrr,*) 'det er en rest av komm:/',tk(na:nb),'/'
        if(.not.ja(.true.,'ignoreres?#')) go to 50
      end if
     

      return
      end




