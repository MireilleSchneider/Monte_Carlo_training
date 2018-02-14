subroutine interp1d(xmin,xmax,fg,ndim,x,f,iescape)
  ! ---------------------------------------------------------------------
  ! PURPOSE:
  ! TO MAKE A 1D INTERPOLATION FOR A GIVEN PROFILE
  ! ---------------------------------------------------------------------
  ! INPUT:
  ! - XMIN = MIN VALUE OF THE ABSCISSA X
  ! - XMAX = MAX VALUE OF THE ABSCISSA X
  ! - FG   = INTERPOLATION VECTOR
  ! - NDIM = DIMENSION OF THE INTERPOLATION VECTOR
  ! - X    = VALUE OF THE ABSCISSA X WHERE FG HAS TO BE EVALUATED
  ! ---------------------------------------------------------------------
  ! OUTPUT:
  ! - F    = INTERPOLATED FUNCTION
  ! ---------------------------------------------------------------------
  ! INPUT/OUTPUT:
  ! - IESCAPE = 0 IF POINT INSIDE THE GRID, = 1 OTHERWISE
  ! ---------------------------------------------------------------------
  implicit none

  ! INPUT/OUTPUT VARIABLES
  double precision, intent(in):: xmin,xmax,x
  integer,          intent(in):: ndim
  double precision, intent(in), dimension(ndim):: fg
  double precision, intent(out):: f
  integer,          intent(inout):: iescape

  ! LOCAL VARIABLES
  integer:: ix1,ix2,iescape_orig
  double precision:: dx,xc,cxmin,cxmax

  iescape_orig = iescape
  dx = (xmax-xmin)/(ndim-1) ! SIZE OF THE INTERVAL INSIDE THE VECTOR FG

  if(x.gt.xmin) then
     ! CORRESPONDING INDICES CONFORMING TO INPUT ARRAYS
     ix1 = floor( (x-xmin) / dx ) +1
     ix2 = ix1 + 1

     ! PREVENT FROM INDICES GOING BEYOND THE DIMENSIONS OF THE ARRAYS
     if(ix2.le.ndim) then
        ! X AND Y COORDINATES OF THE FOUR CORNERS OF THE BOX CONTAINING THE PRESENT POINT
        cxmin = (ix1-1) * dx + xmin
        cxmax = ix1     * dx + xmin
        xc = (x - cxmin)/(cxmax - cxmin)
        f = (fg(ix2)-fg(ix1))*xc+fg(ix1)
     else
        f = 99.
        iescape = 1
     endif
  else
     f = fg(1)
     ix1 = 1
     ix2 = 2
  endif

  ! OVERWRITE RESULT WHEN X STRICTLY EQUAL TO XMAX
  if(x.eq.xmax) then
     f = fg(ndim)
     iescape = iescape_orig
  endif

  ! SECURITY WHEN VARIABLE NOT DEFINED TILL THE VERY EDGE OF THE PLASMA
  if(ix2.le.ndim) then
     if(.not.(f.gt.-9e35.and.f.lt.9e35) &
          .and.(fg(ix1).gt.-9e35.and.fg(ix1).lt.9e35) &
          .and..not.(fg(ix2).gt.-9e35.and.fg(ix2).lt.9e35) &
          ) then
        f = fg(ix1)
     endif
  endif

  return
end subroutine interp1d
