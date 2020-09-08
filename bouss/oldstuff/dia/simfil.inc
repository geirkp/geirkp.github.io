      integer ipos,itape,irec,iut,irstopp,utpos,iaut
      character*200 bilde,utbuff
      character cm
      common/simfil/ipos,itape,irec,iut,irstopp,utpos,iaut,bilde,
     %  utbuff,cm
************************************************************
*     ipos - foesrte ikkebehandlede posisjon i linjebilde
*     itape - unit for input
*     irec - linjebredde
*     iut - unit for output (aktiv nar verdi>=0)
*     irstopp - effectiv linjebredde etter utkommentering
*     utpos - siste  skrevne  posisjon i utbuff
*     bilde - linjebilde
*     utbuff - outputbuffer
*     iaut - verdi lik 1 gir automatisk utlinje naar ny linje leses
*     cm - kommentar-tegn (aktiv naar ulik ' ')
****************************************************************
