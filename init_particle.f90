subroutine init_particle(partic_struct,array_dim)
  ! -------------------------------------------------------------------
  ! PURPOSE:
  ! TO INITIALIZE THE PARTICLE STRUCTURE ARRAY
  ! -------------------------------------------------------------------
  ! INPUT/OUTPUT:
  ! - PARTIC_STRUCT = PARTICLE STRUCTURE TO INITIALIZE
  ! -------------------------------------------------------------------
  ! INPUT:
  ! - ARRAY_DIM = DIMENSION OF THE PARTICLE STRUCTORE TO INTIIALIZE
  ! -------------------------------------------------------------------

  use mod_particle ! PARTICLE STRUCTURE

  implicit none

  ! INPUT VARIABLE
  integer, intent(in):: array_dim

  ! INPUT/OUTPUT VARIABLE
  type(partic), intent(inout), dimension(array_dim):: partic_struct

  partic_struct%iescape = 0
  partic_struct%hweight = 0.
  partic_struct%rpa     = 0.
  partic_struct%zpa     = 0.
  partic_struct%fpa     = 0.
  partic_struct%vel     = 0.
  partic_struct%vpara   = 0.
  partic_struct%rho     = 0.
  partic_struct%timenow = 0.

end subroutine init_particle

