subroutine interp2d(xmin,xmax,ymin,ymax,fg,shape_fg,x,y,f,iescape)
  ! ---------------------------------------------------------------------
  ! PURPOSE:
  ! TO MAKE A 2D INTERPOLATION WITH THE 6-POINT METHOD (OR THE
  !  4-POINT METHOD FOR THE POINTS AT THE EDGE OF THE GRID)
  ! ---------------------------------------------------------------------
  ! INPUT:
  ! - XMIN     = MIN VALUE OF THE VARIABLE X
  ! - XMAX     = MAX VALUE OF THE VARIABLE X
  ! - YMIN     = MIN VALUE OF THE VARIABLE Y
  ! - YMAX     = MAX VALUE OF THE VARIABLE Y
  ! - FG       = INTERPOLATION GRID(X,Y)
  ! - SHAPE_FG = DIMENSION OF THE INTERPOLATION GRID
  ! - X        = VALUE OF THE VARIABLE X WHERE FG HAS TO BE EVALUATED
  ! - Y        = VALUE OF THE VARIABLE Y WHERE FG HAS TO BE EVALUATED
  ! ---------------------------------------------------------------------
  ! OUTPUT:
  ! - F = INTERPOLATED FUNCTION (6-PTS OR 4-PTS METHOD)
  ! ---------------------------------------------------------------------
  ! INPUT/OUTPUT:
  ! - IESCAPE = 0 IF POINT STILL INSIDE THE GRID, = 1 OTHERWISE
  ! ---------------------------------------------------------------------
  !     Z |
  !       |
  ! CZMAX .     O-------------O 
  !       |     |             |   BOX WHERE THE DOUBLE
  !       |     |             |   LINEAR INTERPOLATION
  !       |     |             |   IS CARRIED OUT
  !   ZPA .     |    X        |   
  !       |     |             |   
  ! CZMIN .     O_____________O
  !       |  
  !       |_____.____.________.______
  !         CRMIN   RPA     CRMAX   R
  ! ---------------------------------------------------------------------
  implicit none

  ! INPUT/OUTPUT VARIABLES
  integer,          intent(in):: shape_fg(2)
  double precision, intent(in):: xmin,xmax,ymin,ymax,x,y
  double precision, intent(in):: fg(shape_fg(1),shape_fg(2))
  double precision, intent(out):: f
  integer,          intent(inout):: iescape

  ! LOCAL VARIABLES
  integer:: ix1,ix2,iy1,iy2,ix0,iy0
  double precision:: dx,dy,yc,xc,df1,df2
  double precision:: cxmin,cxmax,cymin,cymax

  dx = (xmax-xmin)/(shape_fg(1)-1) ! SIZE OF THE X-INTERVAL INSIDE THE GRID FG
  dy = (ymax-ymin)/(shape_fg(2)-1) ! SIZE OF THE Y-INTERVAL INSIDE THE GRID FG

  ! CORRESPONDING INDICES CONFORMING TO INPUT ARRAYS
  ix1 = int( (x-xmin) / dx ) +1
  iy1 = int( (y-ymin) / dy ) +1
  ix2 = ix1 + 1
  iy2 = iy1 + 1
  ix0 = ix1 - 1
  iy0 = iy1 - 1

  if(ix1.lt.1.or.iy1.lt.1.or.ix1.gt.shape_fg(1).or.iy1.gt.shape_fg(2)) then
     f = 99.
     iescape = 1

  else
     ! --------------------------------------------------------------------------

     if(ix2.le.shape_fg(1).and.iy2.le.shape_fg(2)) then

        ! X AND Y COORDINATES OF THE FOUR CORNERS OF THE BOX CONTAINING THE CONSIDERED POINT
        cxmin = (ix1-1) * dx + xmin
        cxmax = ix1     * dx + xmin
        cymin = (iy1-1) * dy + ymin
        cymax = iy1     * dy + ymin

        xc = (x - cxmin)/(cxmax - cxmin)
        yc = (y - cymin)/(cymax - cymin)

        ! 4-POINTS INTERPOLATION FOR THE LEFT-DOWN POSITION INSIDE THE GRID
        if(ix1.lt.2.or.iy1.lt.2) then

           df1 = (fg(ix1,iy2)-fg(ix1,iy1))*yc+fg(ix1,iy1) ! POINT A1(IR1,ZPA)
           df2 = (fg(ix2,iy2)-fg(ix2,iy1))*yc+fg(ix2,iy1) ! POINT A2(IR2,ZPA)
           f = (df2-df1)*xc+df1                           ! INTERPOLATED POINT

           if(fg(ix1,iy1).eq.99.or.fg(ix1,iy2).eq.99 &
                .or.fg(ix2,iy1).eq.99.or.fg(ix2,iy2).eq.99) iescape = 1

           ! 6-POINTS INTERPOLATION FOR ALL THE OTHER POSITIONS INSIDE THE GRID
        else
           f = yc*(yc-1)/2.*fg(ix1,iy0)             &
                + xc*(xc-1)/2.*fg(ix0,iy1)          &
                + (1+xc*yc-xc**2-yc**2)*fg(ix1,iy1) &
                + xc*(xc-2*yc+1)/2.*fg(ix2,iy1)     &
                + yc*(yc-2*xc+1)/2.*fg(ix1,iy2)     &
                + xc*yc*fg(ix2,iy2)

           ! CLOSE TO THE EDGE: 6-POINTS METHOD NOT FULLY RELIABLE
           if(f.gt.fg(ix0,iy0).and.f.gt.fg(ix0,iy1).and.f.gt.fg(ix0,iy2)      &
                .and.f.gt.fg(ix1,iy0).and.f.gt.fg(ix1,iy1).and.f.gt.fg(ix1,iy2) &
                .and.f.gt.fg(ix2,iy0).and.f.gt.fg(ix2,iy1).and.f.gt.fg(ix2,iy2)) then
              df1 = (fg(ix1,iy2)-fg(ix1,iy1))*yc+fg(ix1,iy1) ! point A1(ir1,zpa)
              df2 = (fg(ix2,iy2)-fg(ix2,iy1))*yc+fg(ix2,iy1) ! point A2(ir2,zpa)
              f = (df2-df1)*xc+df1                           ! interpolated point
           endif

           ! CLOSE TO THE CENTRE: PSI<0 CAN OCCUR WITH THE 6-POINTS METHOD: THEN USE
           ! THE 4-POINTS METHOD
           if(fg(ix0,iy0).gt.0..and.fg(ix0,iy1).gt.0..and.fg(ix0,iy2).gt.0.      &
                .and.fg(ix1,iy0).gt.0..and.fg(ix1,iy1).gt.0..and.fg(ix1,iy2).gt.0. &
                .and.fg(ix2,iy0).gt.0..and.fg(ix2,iy1).gt.0..and.fg(ix2,iy2).gt.0. &
                .and.f.lt.0.) then
              df1 = (fg(ix1,iy2)-fg(ix1,iy1))*yc+fg(ix1,iy1) ! POINT A1(IR1,ZPA)
              df2 = (fg(ix2,iy2)-fg(ix2,iy1))*yc+fg(ix2,iy1) ! POINT A2(IR2,ZPA)
              f = (df2-df1)*xc+df1                           ! INTERPOLATED POINT
           endif
           if(fg(ix0,iy0).eq.99.or.fg(ix0,iy1).eq.99.or.fg(ix0,iy2).eq.99     &
                .or.fg(ix1,iy0).eq.99.or.fg(ix1,iy1).eq.99.or.fg(ix1,iy2).eq.99 &
                .or.fg(ix2,iy0).eq.99.or.fg(ix2,iy1).eq.99.or.fg(ix2,iy2).eq.99 &
                ) iescape = 1

        endif
     else

        f = 99.
        iescape = 1 ! IF INDEX ABOVE MAX OF THE GRID => POINT OUTSIDE THE GRID

     endif

  endif

  return

end subroutine interp2d
