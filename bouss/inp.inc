*******************************************************************
*     npmax - maximum number of points
*     neta,nu1  - number of points in eta and the two u-fields, <=0 means
*                    that no field is set 
*     irege,iregu1 = value=0 regular grid, value=1: irregular grid
*     etinp,u1,wrk,xeta,xu1 - fields and x-positions
*
********************************************************************
      integer npmax
      parameter(npmax=1000000)
      integer neta,nu1,irege,iregu1
      real etinp(npmax),u1(npmax),wrk(npmax*9)
      real xeta(npmax),xu1(npmax)
      common/inpfield/neta,nu1,irege,iregu1,
     %      etinp,u1,wrk,xeta,xu1
