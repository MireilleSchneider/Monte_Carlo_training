subroutine icrh_heating(scal,inp1d,inp2d,particle)
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
  double precision:: wcycl,br,bz,bf,bmod,r_res
  integer:: nharm,iharm

  ! CALCULATE THE MAGNETIC FIELD COMPONENTS
  call bfield_components(scal,inp2d,particle%rpa,particle%zpa,particle%fpa,br,bz,bf,bmod,particle%iescape)

  ! NEW PARTICLE MAGNETIC MOMENUTUM
  particle%magnm = scal%a_number*pmass*(particle%vel**2-particle%vpara**2)/(2.*bmod)

  ! CYCLOTRONIC FREQUENCY
  wcycl = scal%z_number*qel*bmod/(scal%a_number*pmass)

  ! RESONANCE OR NOT? TEST THE THREE FIRST HARMONICS FOR THE RESONANCE CONDITION
  particle%icrh_kick = 0

  do iharm=1,3

     ! RESONANCE POSITION
     r_res = iharm * scal%z_number * qel * scal%b0 * scal%r0 / &
          ( scal%a_number * pmass * scal%icrh_frequency * 1e6 * 2 * pi)

     ! CROSSED A RESONANCE?
     if(particle%rpa.gt.particle%rpa_old) then
        if(r_res.gt.particle%rpa_old.and.r_res.le.particle%rpa) then
           particle%icrh_kick = 1
        endif
     else
        if(r_res.gt.particle%rpa.and.r_res.le.particle%rpa_old) then
           particle%icrh_kick = 1
        endif
     endif

     if(particle%icrh_kick.eq.1) write(555,*) particle%rpa,particle%zpa,r_res,iharm

     if(scal%start.eq.1) then
        if(iharm.eq.1) then
           write(*,*) '------------------'
           write(*,*) 'Harmonic R_res (m)'
        endif
        write(*,'(a3,i3,a4,f6.2)') '   ',iharm,'    ',r_res
        if(iharm.eq.3) write(*,*) '------------------'
     endif

  enddo

end subroutine icrh_heating
