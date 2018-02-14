subroutine output(scal,inipartic,totpartic,sizetot)
  ! -----------------------------------------------------------------
  ! PURPOSE:
  ! TO EXPORT RESULTS TO ASCII FILES
  ! -----------------------------------------------------------------
  ! INPUT:
  ! - SCAL      = STRUCTURE FOR INPUT SCALAR VARIABLES
  ! - INIPARTIC = STRUCTURE OF INITIAL PARTICLE STATE
  ! - TOTPARTIC = STRUCTURE OF FINAL PARTICLE STATE
  ! - SIZETOT   = DIMENSION OF INIPARTIC AND TOTPARTIC STRUCTURES
  ! -----------------------------------------------------------------

  use mod_constants     ! PHYSICS CONSTANTS
  use mod_input_scalars ! INPUT SCALAR VARIABLES
  use mod_particle      ! PARTICLE STRUCTURE
  use mod_io_management ! ROUTINES FOR INPUT/OUTPUT MANAGEMENT

  implicit none

  ! INPUT/OUTPUT VARIABLES
  integer,             intent(in):: sizetot
  type(input_scalars), intent(in):: scal
  type(partic),        intent(inout),dimension(sizetot):: inipartic,totpartic

  ! DON'T WANT TO GIVE A BIAS TO FINAL DISTRIBUTIONS DUE TO ESCAPED PARTICLES
  ! WHEN THE SOURCE POSITION IMPLEMENTATION HAS NOT BEEN IMPLEMENTED YET 
  ! => IESCAPE FORCED TO 0
  if(scal%rz_source.eq.0) totpartic(:)%iescape = 0

  call writevar('output/rpa_in.txt        ',inipartic(:)%rpa)
  call writevar('output/zpa_in.txt        ',inipartic(:)%zpa)
  call writevar('output/fpa_in.txt        ',inipartic(:)%fpa)
  call writevar('output/vel_in.txt        ',inipartic(:)%vel)
  call writevar('output/energy_in.txt     ',0.5*pmass*scal%a_number*inipartic(:)%vel**2/qel*1e-3)
  call writevar('output/vpara_in.txt      ',inipartic(:)%vpara)
  call writevar('output/hweight_in.txt    ',inipartic(:)%hweight)

  call writevar('output/rpa.txt           ',totpartic(:)%rpa)
  call writevar('output/zpa.txt           ',totpartic(:)%zpa)
  call writevar('output/fpa.txt           ',totpartic(:)%fpa)
  call writevar('output/vel.txt           ',totpartic(:)%vel)
  call writevar('output/energy.txt        ',0.5*pmass*scal%a_number*totpartic(:)%vel**2/qel*1e-3)
  call writevar('output/vpara.txt         ',totpartic(:)%vpara)
  call writevar('output/hweight.txt       ',totpartic(:)%hweight)
  call writevar('output/iescape.txt       ',totpartic(:)%iescape*1.d0)

end subroutine output
