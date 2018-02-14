subroutine bfield_components(scal,inp2d,rpa,zpa,fpa,br,bz,bf,bmod,iescape)
  ! --------------------------------------------------------
  ! PURPOSE:
  ! TO CALCULATE (R,Z,PHI) COMPONENTS OF THE MAGNETIC FIELD
  ! --------------------------------------------------------
  ! INPUT:
  ! - SCAL  = STRUCTORE FOR INPUT SCALAR VARIABLES
  ! - INP2D = STRUCTURE FOR INPUT 2D-GRIDS
  ! - RPA   = R-POSITION OF THE PARTICLE (M)
  ! - ZPA   = Z-POSITION OF THE PARTICLE (M)
  ! --------------------------------------------------------
  ! OUTPUT:
  ! - BR   = R-COMPONENT OF THE MAGNETIC FIELD (T)
  ! - BZ   = Z-COMPONENT OF THE MAGNETIC FIELD (T)
  ! - BPHI = PHI-COMPONENT OF THE MAGNETIC FIELD (T)
  ! - BMOD = MODULE OF THE MAGNETIC FIELD (T)
  ! --------------------------------------------------------
  ! INPUT/OUTPUT:
  ! - IESCAPE = FLAG (=1 IF PARTICLE ESCAPED, =O OTHERWISE)
  ! --------------------------------------------------------

  use mod_constants     ! PHYSICS CONSTANTS
  use mod_input_scalars ! INPUT SCALAR VARIABLES
  use mod_input_2d      ! INPUT 2D VARIABLES

  implicit none

  ! INPUT/OUTPUT VARIABLES
  type(input_scalars), intent(in):: scal
  type(input_2d),      intent(in):: inp2d
  double precision,    intent(in):: rpa,zpa,fpa
  double precision,    intent(out):: br,bz,bf,bmod
  integer,             intent(inout):: iescape

  br   = 0.
  bz   = 0.
  bf   = 0.
  bmod = 0.
  
  call interp2d(scal%rmin,scal%rmax,scal%zmin,scal%zmax,inp2d%br2d,shape(inp2d%br2d),rpa,zpa,br,iescape)
  call interp2d(scal%rmin,scal%rmax,scal%zmin,scal%zmax,inp2d%bz2d,shape(inp2d%bz2d),rpa,zpa,bz,iescape)
  call interp2d(scal%rmin,scal%rmax,scal%zmin,scal%zmax,inp2d%bphi2d,shape(inp2d%bphi2d),rpa,zpa,bf,iescape)

  bmod = sqrt(br**2+bz**2+bf**2)

end subroutine bfield_components
