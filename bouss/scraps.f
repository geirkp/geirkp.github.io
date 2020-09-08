***************************************************************************
      subroutine ekstreta(y,x,na,nb,dx,noun,spline,eks)
      integer na,nb
      real y(na:nb),x(na:nb),dx
      logical noun,spline,eks
      include 'inp.inc'
*---------------------------------------------------------------
      integer i,nw,nval,ierr
      real dxv
      logical spint

c     make grid in wrk
      
      call laggrid(x,na,nb,dx,noun,wrk,nval) 
      nw=nval+1     

      if(spline) then
        if(irege.eq.0) then
          spint=.true.
        else
         spint=.false.
         write(0,*)'ekstreta, irregular grid, spline overruled' 
        end if
      else
        spint=.false.
      end if

      if(spint) then 
        dxv=(xeta(neta)-xeta(1))/(neta-1)
        call kstageg(etinp,neta,dxv,xeta(1),y(na),wrk,nval,eks,
     %           0.0,0.0,.false.,0.0,.false.,0.0,ierr,wrk(nw))
      else
        call gureglin(xeta,etinp,neta,wrk,y(na),nval,eks,0.0,0.0,
     %                   0.0001,ierr)
      end if

      if(ierr.gt.0) then
       write(0,*)'ekstreta: ierr =',ierr
      end if

      return
      end


