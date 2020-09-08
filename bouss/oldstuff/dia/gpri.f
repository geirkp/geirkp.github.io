***************************************************************************
*
*     GPRI
*
* printer array i plotxy-format p} fil
* parametere:
*         y(n)  - array                   I
*         n  - array-grense                  I
*         dx - gitterinkrement (betydning bare nar noun=.false.)          I
*         x - posisjoner  av y -verdier, naar noun=.false.                  I
*             benyttes bare x(1) som er pos av y(1)
*         noun - .false. angir uniformt gitter             I
*         isyk - nummer paa fil              I
*         stem - navnestamme, feks stem='eta#', isyk=11 gir datafil eta11 I
*                hvis stem ender paa ! ignoreres nummer
*         fak - skaleringsfaktor for y, slik at y/skal skrives ut     I
*         fakx - skaleringsfaktor for x, slik at x/fakx skrives ut    I
****************************************************************************
      subroutine gpri(y,n,dx,x,noun,isyk,stem,fak,fakx)
      integer isyk,n
      real y(n),dx,x(n),fak,fakx
      logical noun
      character*80 stem
*--------------------------------------------------------
      integer i,kf,kf2
      real xver,yver,faki,faxi
      character*80 tall,utfil
      logical def

      faki=1.0/fak
      faxi=1.0/fakx

      call xstrip(stem,kf2,def)
      utfil(1:kf2)=stem(1:kf2)

      if(def) then
        call itconv(isyk,kf,tall)
        utfil(kf2+1:kf2+kf)=tall(1:kf)
        kf=kf2+kf
      else
         kf=kf2
      end if

      open(unit=30,file=utfil(1:kf))


      do 100 i=1,n
      if(noun) then
        xver=faxi*x(i)
      else
        xver=faxi*(x(1)+(i-1)*dx)
      end if
      yver=faki*y(i)
      write(30,*)xver,yver
 100  continue

      close(30)

      return
      end



***************************************************************************
*
*     CPRI
*
* printer complex array i plotxy-format p} fil.
*       Kolonner:  x Re Im abs arg
* parametere:
*         y(n)  - array                   I
*         n  - array-grense                  I
*         dx - gitterinkrement (betydning bare nar noun=.false.)          I
*         x - posisjoner  av y -verdier, naar noun=.false.                  I
*             benyttes bare x(1) som er pos av y(1)
*         noun - .false. angir uniformt gitter             I
*         isyk - nummer paa fil              I
*         stem - navnestamme, feks stem='eta#', isyk=11 gir datafil eta11 I
*                hvis stem ender paa ! ignoreres nummer
*         fak - skaleringsfaktor for y, slik at y/skal skrives ut     I
*         fakx - skaleringsfaktor for x, slik at x/fakx skrives ut    I
****************************************************************************
      subroutine cpri(y,n,dx,x,noun,isyk,stem,fak,fakx)
      integer isyk,n
      real dx,x(n),fak,fakx
      complex y(n)
      logical noun
      character*80 stem
*--------------------------------------------------------
      integer i,kf,kf2
      real xver,faki,faxi,rdel,imdel,abver,arg
      complex yver
      character*80 tall,utfil
      logical def

      faki=1.0/fak
      faxi=1.0/fakx

      call xstrip(stem,kf2,def)
      utfil(1:kf2)=stem(1:kf2)

      if(def) then
        call itconv(isyk,kf,tall)
        utfil(kf2+1:kf2+kf)=tall(1:kf)
        kf=kf2+kf
      else
         kf=kf2
      end if

      open(unit=30,file=utfil(1:kf))


      do 100 i=1,n
      if(noun) then
        xver=faxi*x(i)
      else
        xver=faxi*(x(1)+(i-1)*dx)
      end if
      yver=faki*y(i)
      rdel=real(yver)
cc      imdel=aimag(yver)
cc      abver=cabs(yver)
cc      arg=aimag(log(yver))
      imdel=imag(yver)
      abver=abs(yver)
      arg=imag(log(yver))
      write(30,'(5e16.6)')xver,rdel,imdel,abver,arg
 100  continue

      close(30)

      return
      end



 
***************************************************************************
*
*     IPRI
*
* printer integer array i plotxy-format p} fil
* parametere:
*         ny(n0:n)  - array                   I
*         n0,n  - array-grenser                  I
*         noun - .false. angir uniformt gitter             I
*         isyk - nummer paa fil              I
*         stem - navnestamme, feks stem='eta#', isyk=11 gir datafil eta11 I
****************************************************************************
      subroutine ipri(ny,n0,n,isyk,stem)
      integer isyk,n0,n,ny(n0:n)
      character*80 stem
*--------------------------------------------------------
      integer i,kf,kf2
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

      open(unit=30,file=utfil(1:kf))


      do 100 i=n0,n

      write(30,'(2i10)')i,ny(i)
 100  continue

      close(30)

      return
      end
 

***************************************************************************
*
*     YPRI
*
* prints a single array in a one-column ascii file
* parametere:
*         y(n)  - array                   I
*         n  - array-grense                  I
*         isyk - nummer paa fil              I
*         stem - navnestamme, feks stem='eta#', isyk=11 gir datafil eta11 I
*                hvis stem ender paa ! ignoreres nummer
*         fak - skaleringsfaktor for y, slik at y/skal skrives ut     I
****************************************************************************
      subroutine ypri(y,n,isyk,stem,fak)
      integer isyk,n
      real y(n),dx,x(n),fak,fakx
      logical noun
      character*80 stem
*--------------------------------------------------------
      integer i,kf,kf2
      real yver,faki
      character*80 tall,utfil
      logical def

      faki=1.0/fak

      call xstrip(stem,kf2,def)
      utfil(1:kf2)=stem(1:kf2)

      if(def) then
        call itconv(isyk,kf,tall)
        utfil(kf2+1:kf2+kf)=tall(1:kf)
        kf=kf2+kf
      else
         kf=kf2
      end if

      open(unit=30,file=utfil(1:kf))


      do 100 i=1,n
      yver=faki*y(i)
      write(30,*)yver
 100  continue

      close(30)

      return
      end

