function dummy_generation(scal,flag)
  ! --------------------------------------------------------------------------------
  ! PURPOSE:
  ! TO CARRY OUT A DUMMY UNIFORM GENERATION OF (R,Z,VELOCITH,PITCH ANGLE)
  ! IN ORDER TO REPLACE THE TRUE IMPLEMENTATION (SUCH THAT THE CODE DOES NOT CRASH)
  ! --------------------------------------------------------------------------------
  ! INPUT:
  ! - SCAL = STRUCTORE FOR INPUT SCALAR VARIABLES
  ! - FLAG = TO CHOOSE WHAT VARIABLE TO GENERATE: 1-4 FOR R,Z,V,PITCH RESPECTIVELY
  ! --------------------------------------------------------------------------------
  use mod_randomsp      ! SEED FOR RANDOM NUMBERS
  use mod_input_scalars ! INPUT SCALAR VARIABLES

  implicit none

  ! INPUT/OUTPUT VARIABLES
  type(input_scalars), intent(in):: scal
  integer, intent(in):: flag

  ! LOCAL VARIABLES
  double precision:: dummy_generation

  ! R
  if(flag.eq.1) then
     dummy_generation = scal%rmin+(scal%rmax-scal%rmin)*ran2(0)
  ! Z
  elseif(flag.eq.2) then
     dummy_generation = scal%zmin+(scal%zmax-scal%zmin)*ran2(0)
  ! VELOCITY
  elseif(flag.eq.3) then
     dummy_generation = scal%vmin+(scal%vmax-scal%vmin)*ran2(0)
  ! PITCH ANGLE
  elseif(flag.eq.4) then
     dummy_generation = scal%ximin+(scal%ximax-scal%ximin)*ran2(0)
  endif

end function dummy_generation
