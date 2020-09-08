
      integer  ltape,intape,iustrr
      character charkom
      integer aktkom,sattkom,aktlog,sattlog,dispens
      common/komstyr/aktkom,sattkom,ltape,intape,iustrr,
     %aktlog,sattlog,dispens,charkom
ccc----------------------------------------------------------------------
      integer nhd,ndd,pronum
      character*40 hostt,vert,lognav
      character*120 homedir,defdir
      common/direk/ nhd,ndd,pronum,hostt,vert,lognav,homedir,defdir
***************************************************************************
*
*          Forklaring p} parametere:
*
*     ltape  - unitnummer for logg-fil
*     intape  - unitnummer for input
*     iustrr - unitnummer for standard error
*     charkom - kommentarmark|r (ascii -nr)
*     aktkom - angir om kommentarsetting er aktivisert
*     sattkom- angir om charcom er satt
*     aktlog- angir om logging er aktivert
*     sattlog- angir om logging er satt
*     dispens - angir om input fra infil ulik standard input er midlertidig
*               suspendert. Intreffer ved minst 4 ? f|rst p} linja i inputfil.
*     nhd- lengde av tekst homedir
*     ndd- lengde av tekst defdir
*     pronum - prosessnummer.
*     hostt- betegnelse p}  vertsmaskin ( avlesning av HOSTTYPE )
*     lognav - login-navn paa bruker
*     vert - navn paa vertsmaskin ( avlesning av HOST )
*     homedir - homedirektory
*     defdir - defaultdirectory.
***************************************************************************







