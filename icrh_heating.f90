subroutine icrh_heating(scal,inp1d,inp2d,particle)
  ! -------------------------------------------------
  ! PURPOSE:
  ! TO LOCATE THE RESONANCE BETWEEN IONS AND ICRF
  ! WAVES
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
  ! TO_BE_ADDED_HERE
  wcycl = 0.
  ! wcycl = ...

  ! RESONANCE OR NOT? TEST THE THREE FIRST HARMONICS FOR THE RESONANCE CONDITION
  particle%icrh_kick = 0

  do iharm=1,3

     ! RESONANCE POSITION
     ! TO_BE_ADDED_HERE
     r_res = 0.
     ! r_res = ...

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
           write(*,*) 'Cyclotron ion frequency (MHz) = ',wcycl 
           write(*,*) '------------------'
           write(*,*) 'Harmonic R_res (m)'
        endif
        write(*,'(a3,i3,a4,f6.2)') '   ',iharm,'    ',r_res
        if(iharm.eq.3) write(*,*) '------------------'
     endif

  enddo

end subroutine icrh_heating
