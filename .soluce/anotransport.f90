subroutine anotransport(scal,inp1d,inp2d,particle)
  ! -------------------------------------------------
  ! PURPOSE:
  ! TO APPLY ANOMALOUS TRANSPORT TO THE PARTICLE
  ! -------------------------------------------------
  ! INPUT:
  ! - SCAL  = STRUCTURE FOR INPUT SCALAR VARIABLES
  ! - INP1D = STRUCTURE FOR INPUT 1D-PROFILES
  ! - INP2D = STRUCTURE FOR INPUT 2D-GRIDS
  ! -------------------------------------------------
  ! INPUT/OUTPUT:
  ! - PARTICLE = PARTICLE STRUCTURE
  ! -------------------------------------------------

  use mod_constants     ! PHYSICS CONSTANTS
  use mod_randomsp      ! SEED FOR RANDOM NUMBERS
  use mod_particle      ! PARTICLE STRUCTURE
  use mod_input_scalars ! INPUT SCALAR VARIABLES
  use mod_input_1d      ! INPUT 1D VARIABLES
  use mod_input_2d      ! INPUT 2D VARIABLES

  implicit none

  ! INPUT/OUTPUT VARIABLES
  type(input_scalars), intent(in):: scal
  type(input_1d),      intent(in):: inp1d
  type(input_2d),      intent(in):: inp2d
  type(partic),        intent(inout):: particle

  ! LOCAL VARIABLES
  double precision:: psirand,dr_geom
  double precision:: r_geom, the_geom

  ! INITIAL VALUE OF GEOMETRIC RADIAL COORDINATE R_GEOM AND GEOMETRICAL ANGLE THE_GEOM
  r_geom   = sqrt((particle%rpa-scal%r0)**2 + (particle%zpa-scal%z0)**2)
  the_geom = atan2(particle%zpa-scal%z0,particle%rpa-scal%r0)

  ! RANDOM NUMBER
  psirand = -sqrt(3.)+2*sqrt(3.)*ran2(0)

  ! R-VARIATION DUE TO ANOMALOUS TRANSPORT
  dr_geom = (scal%convection + scal%diffusion/r_geom)*scal%dt + psirand*sqrt(2*scal%diffusion*scal%dt)

  ! NEW VALUE OF GEOMETRIC RADIAL COORDINATE R_GEOM (THE_GEOM KEPT CONSTANT)
  r_geom = r_geom + dr_geom

  ! NEW (R,Z) POSITION OF THE PARTICLE AFTER ANOMALOUS TRANSPORT
  particle%rpa = scal%r0 + r_geom*cos(the_geom)
  particle%zpa = scal%z0 + r_geom*sin(the_geom)

  ! CHECK IF PARTICLE IS STILL INSIDE THE PLASMA (BY CALCULATING THE LOCAL TOROIDAL FLUX COORDINATE)
  call interp2d(scal%rmin,scal%rmax,scal%zmin,scal%zmax &
       ,inp2d%rho2d,shape(inp2d%rho2d),particle%rpa,particle%zpa,particle%rho,particle%iescape) ! RHO

end subroutine anotransport
