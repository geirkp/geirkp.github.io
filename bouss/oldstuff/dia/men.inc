      integer mordre,mmen,meks
      parameter(mordre=100,mmen=30,meks=4)
      character*80 ordre(mordre),menspor(mmen),eksmen(4)
      character*10 spes(mordre)
      integer menpek(mordre),meny(mmen),menl(mmen),mendef(mmen)
      integer menstart,ikommando,ifeil,menant,iopptatt
      logical sekvens
      common/kommando/ordre,menspor,eksmen,
     %spes,menpek,meny,menl,mendef,
     %menstart,ikommando,ifeil,menant,iopptatt,sekvens
                                          
      integer mspes
      parameter(mspes=200)
      character*80 spesspor(mordre)
      integer nrordre(mordre,3),ispes(mspes),iint,ireal
      real rspes(mspes)
      common/spesifik/spesspor,nrordre,ispes,iint,ireal,rspes


**************************************************************************
*
*       Deklarasjoner i common-omr}dene.
*
*      mordre  - maksimalt antall ordre.
*      mmen    - maksimalt antall menyer
*      meks    - maksimalt antall ordrer i ekstra styremeny
*      ordre   - array som inneholder alle ordrer satt av brukeren
*      menspor - array med sp|rsm}l til menyer (satt av bruker)
*      eksmen  - array med sp|rsm}l i ekstrameny
*      spes    - angir spesifikasjoner for ordrer. Dersom spes(5) har
*                verdien 'IIR#' har kommando nr. 5 spesifikasjoner
*                heltall  heltall  reelt-tall  (satt av bruker)
*      menpek  - angir undermenyer for ordre. Dersom menpek(5) har
*                verdien 7 har ordre nr. 5 meny nr. 7 som undermeny.
*                Er verdien 0 har kommandoen ingen undermeny men en rest
*                for behandling utenfor menysystemet tolereres, er verdien
*                derimot -1 m} kommandopost 7 etterf|lges av bare blanke.
*                (satt av bruker)
*      meny    - ordre(meny(k)) er f|rste ordre i meny nr k
*      menl    - angir antall ordrer i menyene
*      mendef  - angir nummer for defaultordre i menyene i lokal
*                nummerering. Dvs. er mendef(I) lik K er standardordren
*                ordre( meny(I)+K-1 )
*      menstart- angir nummer for meny som er |verst i hierarkiet.
*                Verdien p} menstart kan endres ved kall p} 'toppmen'
*                slik at dialogen ved et visst punkt i programmets kj|ring
*                kan begrenses til et underniv}. Tilsvarende kan ogs}
*                to eller flere helt uavhengige menyer inpasses i registeret
*                og aktiveres/deaktiveres ved 'toppmen'.
*      ikommando- teller antall kommandoposter som gis i l|pet av en dialog
*                 (ikke implementert)
*      ifeil    - teller antall feil i en dialog (ikke implementert)
*      menant  - antall menyer som er opprettet ved kall p} 'setmen'
*      iopptatt - antall elementer i ordre-array som er satt av til
*                 de opprettede menyer.
*      sekvens  - verdi lik sann angir at kommandopostene m} gis en etter
*                 en. verdi lik usann betyr at flere poster kan gis p}
*                 samme linje.
*      mspes    - maksimalt antall spesifikasjoner av heltalls-
*                 og reellstall-type. (maksimalt mspes av hver)
*      spesspor - array med sp|rsm}l etter spesifikasjoner (satt av bruker)
*      nrordre  - pekerarray for ordrespesifukasjoner. F|rste indeks angir
*                 globalt ordrenummer, la oss si I. Betydning av elementer:
*                  nrordre(I,1)=-1  : Standardverdier p} spes. ikke lovlig.
*                              = 0  : Standardverdier p} spes. lovlig,
*                                     men ingen verdier er tilgjengelig.
*                              > 0  : Standardverdier tilgjengelige, verdi
*                                     angir antall ganger spesifikasjonene
*                                     er satt.
*                         (I,2) : nummer f|r f|rste element i ispes som
*                                 tilh|rer kommando I. Verdi lik null betyr
*                                 ingen heltallsspes.
*                         (I,3) : Samme som ovenfor for reellstall-spes.
*                                 i rspes.
*     
*      ispes    - array for heltallsspesifikasjoner.
*      iint     - antall heltallsspes. som er avsatt
*      ireal    - antall reelltallsspes. som er avsatt
*      rspes    - array som inneholder reelle spesifikasjoner.
*******************************************************************************
