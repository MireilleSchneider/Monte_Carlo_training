function energy_source(scal,vpart,ti)
  ! ----------------------------------------------------
  ! PURPOSE: 
  ! TO PROVIDE THE NORMALIZED SOURCE FUNCTION
  ! OF PARTICLES AS A FUNCTION OF THEIR ENERGY
  ! ----------------------------------------------------
  ! INPUT:
  ! - SCAL  = STRUCTORE FOR INPUT SCALAR VARIABLES
  ! - VPART = PARTICLE VELOCITY (M/S)
  ! - TI    = ION TEMPERATURE (KEV)
  ! ----------------------------------------------------
  ! OUTPUT:
  !   - ENERGY_SOURCE = NORMALIZED SOURCE FUNCTION AS A
  !                     FUNCTION OF THE PARTICLE ENERGY
  ! ----------------------------------------------------
  ! REFERENCE:
  ! BRYSK, H. "FUSION NEUTRON ENERGIES AND SPECTRA."
  ! PLASMA PHYSICS 15, NO. 7 (JULY 1, 1973): 611. 
  ! DOI:10.1088/0032-1028/15/7/001.
  ! ----------------------------------------------------

  use mod_constants     ! PHYSICS CONSTANTS
  use mod_input_scalars ! INPUT SCALAR VARIABLES

  implicit none

  ! INPUT/OUTPUT VARIABLES
  type(input_scalars), intent(in):: scal
  double precision,    intent(in):: vpart,ti

  ! LOCAL VARIABLES
  double precision:: energy_source,enepart,enemean

  enepart = (scal%a_number*pmass)*vpart**2/(qel*2000) ! PARTICLE ENERGY (KEV)
  enemean = scal%energy                             ! MEAN ALPHA ENERGY (KEV)

  ! DISTRIBUTION FUNCTION AS A FUNCTION OF THE ENERGY
  energy_source = exp(-(enepart-enemean)**2/((4*scal%a_number*pmass*ti*enemean) &
       /(scal%a_number*pmass+pmass)))

  ! SECURITY
  if(.not.(energy_source.gt.-9999..and.energy_source.lt.9999.)) then
     if(ti.ne.0.) write(*,*) 'PROBLEM IN THE GENERATION OF THE SOURCE FOR ENERGY'
     energy_source = 0.
  endif

end function energy_source
