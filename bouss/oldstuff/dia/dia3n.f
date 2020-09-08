******************************************************************************
*
*                L E S I 2
*     virker som 'itall' men leser to heltall ir1,ir2 med standardverdier
*     id1,id2.                                                                
*
*     kaller 'lesveki'.
******************************************************************************
      subroutine lesi2(id1,id2,spor,ir1,ir2)
      integer id1,id2,ir1,ir2
      character*80 spor
*---------------------------------------------------------------------------
      integer id(2),isv(2)
      
      id(1)=id1
      id(2)=id2
      call lesveki(id,spor,isv,2)
      ir1=isv(1)
      ir2=isv(2)
      return
      end

******************************************************************************
      subroutine lesi3(id1,id2,id3,spor,ir1,ir2,ir3)
      integer id1,id2,id3,ir1,ir2,ir3
      character*80 spor
*---------------------------------------------------------------------------
      integer id(3),isv(3)
      
      id(1)=id1
      id(2)=id2
      id(3)=id3
      call lesveki(id,spor,isv,3)
      ir1=isv(1)
      ir2=isv(2)
      ir3=isv(3)
      return
      end


******************************************************************************
      subroutine lesi4(id1,id2,id3,id4,spor,ir1,ir2,ir3,ir4)
      integer id1,id2,id3,id4,ir1,ir2,ir3,ir4
      character*80 spor
*---------------------------------------------------------------------------
      integer id(4),isv(4)
      
      id(1)=id1
      id(2)=id2
      id(3)=id3   
      id(4)=id4
      call lesveki(id,spor,isv,4)
      ir1=isv(1)
      ir2=isv(2)
      ir3=isv(3)                
      ir4=isv(4)
      return
      end



******************************************************************************
*
*                  L E S V E K I
*
*     leser en array med heltall. 
*
*     kaller: 'xstrip','deltek','geti'
*****************************************************************************
      subroutine lesveki(id,spor,isv,n)
      integer n,id(n),isv(n)
      character*80 spor
      include 'styr.inc'
*----------------------------------------------------------------------------
      integer l(2,20),ires,io ,ks ,i ,iflag ,k1,k2,kflag
      logical def
      character*80 linje

      call xstrip(spor,ks,def)
      if(ks.eq.0) ks=1
   
  100 continue

      if(def) then
        write(iustrr,*) spor(1:ks),'/',(id(i) , i=1,n),'/'
        if(aktlog.eq.1 .and. aktkom.eq.1) then
          write(ltape,*) charkom,spor(1:ks),'/'
          do 105 i=1,n
          write(ltape,*)charkom,id(i) 
 105      continue
          write(ltape,*) charkom,'/'
        end if

      else
        call primvri(spor)
      end if              

      call primles(linje,kflag)
      if(kflag.eq.2) stop
      if(kflag.ne.0) go to 100

      call deltek(linje,1,80,l,io)

      if(io.eq.0) then
          if(def) then
          do 50 i=1,n
   50     isv(i)=id(i)
          go to 500
        else
          call primvri('standardverdier er ikke tilgjengelige#')
          go to 100
        end if
      end if

      if(io.ne.n)  then
        call primvri('feil antall tall#')
        go to 100
      end if

      do 200 i=1,n
      call geti(linje,l(1,i),l(2,i),k1,k2,ires,iflag)
      if(iflag.eq.1) then
        isv(i)=ires
      else
        write(iustrr,*)'ulovlig svar:',linje(l(1,i):l(2,i))
        if(aktlog.eq.1 .and. aktkom.eq.1)
     %   write(ltape,*)charkom,'ulovlig svar',linje(l(1,i):l(2,i))
        go to 100
      end if
  200 continue

 500  continue

      dispens=0

      return
      end 

******************************************************************************
      subroutine lesr2(rd1,rd2,spor,rr1,rr2)
      real rd1,rd2,rr1,rr2
      character*80 spor
*---------------------------------------------------------------------------
      real rd(2),rsv(2)
      
      rd(1)=rd1
      rd(2)=rd2
      call lesvekr(rd,spor,rsv,2)
      rr1=rsv(1)
      rr2=rsv(2)
      return
      end

******************************************************************************
      subroutine lesr3(rd1,rd2,rd3,spor,rr1,rr2,rr3)
      real rd1,rd2,rd3,rr1,rr2,rr3
      character*80 spor
*---------------------------------------------------------------------------
      real rd(3),rsv(3)
      
      rd(1)=rd1
      rd(2)=rd2
      rd(3)=rd3
      call lesvekr(rd,spor,rsv,3)
      rr1=rsv(1)
      rr2=rsv(2)
      rr3=rsv(3)
      return
      end


******************************************************************************
      subroutine lesr4(rd1,rd2,rd3,rd4,spor,rr1,rr2,rr3,rr4)
      real rd1,rd2,rd3,rd4,rr1,rr2,rr3,rr4
      character*80 spor
*---------------------------------------------------------------------------
      real rd(4),rsv(4)
      
      rd(1)=rd1
      rd(2)=rd2       
      rd(3)=rd3   
      rd(4)=rd4
      call lesvekr(rd,spor,rsv,4)
      rr1=rsv(1)
      rr2=rsv(2)
      rr3=rsv(3)                
      rr4=rsv(4)
      return
      end



******************************************************************************
      subroutine lesvekr(rd,spor,rsv,n)
      integer n
      real rd(n),rsv(n)
      character*80 spor
      include 'styr.inc'
*----------------------------------------------------------------------------
      integer l(2,20),io ,ks ,i ,iflag ,k1,k2,kflag
      real rres
      logical def
      character*80 linje

      call xstrip(spor,ks,def)   
      if(ks.eq.0) ks=1

  100 continue

      if(def) then
        write(iustrr,*) spor(1:ks),'/',(rd(i) , i=1,n),'/'
        if(aktlog.eq.1 .and. aktkom.eq.1) then
          write(ltape,*) charkom,spor(1:ks),'/'
          do 105 i=1,n
          write(ltape,*)charkom,rd(i) 
 105      continue
          write(ltape,*)charkom, '/'
        end if
      else
        call primvri(spor)
      end if                                  


      call primles(linje,kflag)
      if(kflag.eq.2) stop
      if(kflag.gt.0) go to 100

      call deltek(linje,1,80,l,io)

      if(io.eq.0) then
          if(def) then
          do 50 i=1,n
   50     rsv(i)=rd(i)
          go to 500
        else
          call primvri('standardverdier er ikke tilgjengelige#')
          go to 100
        end if
      end if

      if(io.ne.n)  then
        call primvri('feil antall tall#')
        go to 100
      end if

      do 200 i=1,n
      call getr(linje,l(1,i),l(2,i),k1,k2,rres,iflag)
      if(iflag.eq.1) then
        rsv(i)=rres
      else
        write(iustrr,*)'ulovlig svar:',linje(l(1,i):l(2,i))
        if(aktlog.eq.1 .and. aktkom.eq.1)
     %  write(ltape,*)charkom,'ulovlig svar:',linje(l(1,i):l(2,i))
        go to 100
      end if
  200 continue
 500  continue

      dispens=0

      return
      end 

