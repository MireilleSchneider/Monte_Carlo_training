function bfield_modulation(scal,inp2d,rpa,zpa,fpa,bfield,koption)
  ! --------------------------------------------------------------------------
  ! PURPOSE: CALCULATE MAGNETIC FIELD MODULATION DUE TO RIPPLE
  !          B = B * (1 + DELTA * SIN (NCOIL * FPA) )
  ! --------------------------------------------------------------------------
  ! INPUT:
  ! - SCAL    = STRUCTORE FOR INPUT SCALAR VARIABLES
  ! - INP2D   = STRUCTURE FOR INPUT 2D-GRIDS
  ! - RPA     = R-POSITION OF THE PARTICLE (M)
  ! - ZPA     = Z-POSITION OF THE PARTICLE (M)
  ! - FPA     = PHI-COORDINATE OF THE PARTICLE (-)
  ! - BFIELD  = MAGNETIC FIELD WITHOUT RIPPLE
  ! - KOPTION = FLAG TO CALCULATE MAGNETIC FIELD WITH RIPPLE OR ITS DERIVATIVE
  !     1 = CALCULATE MAGNETIC FIELD WITH RIPPLE
  !     2 = CALCULATE MAGNETIC FIELD DERIVATE DB/DPHI
  ! --------------------------------------------------------------------------
  use mod_constants     ! PHYSICS CONSTANTS
  use mod_input_scalars ! INPUT SCALAR VARIABLES
  use mod_input_2d      ! INPUT 2D VARIABLES

  implicit none

  ! INPUT/OUTPUT VARIABLES
  type(input_scalars), intent(in):: scal
  type(input_2d),      intent(in):: inp2d
  integer,             intent(in):: koption
  double precision,    intent(in):: rpa,zpa,fpa,bfield

  ! LOCAL VARIABLES
  integer:: idum,ncoil
  double precision:: bfield_modulation,delta

  ! NUMBER OF MAGNETIC COILS
  ! TO_BE_ADDED_HERE
  ! ....

  ! MAXIMUM RIPPLE AT THE LOW FIELD SIDE
  call interp2d(scal%rmin,scal%rmax,scal%zmin,scal%zmax,inp2d%delta,shape(inp2d%delta),rpa,zpa,delta,idum)

  !write(111,*) rpa,delta

  ! B-FIELD MODULATION
  if(koption.eq.1) then
     ! TO_BE_ADDED_HERE
     bfield_modulation = bfield
  ! DB/DPHI DERIVATIVE
  elseif(koption.eq.2) then
     ! TO_BE_ADDED_HERE
     bfield_modulation = 0.
  else
     print*,'BAD VALUE FOR KOPTION IN BFIELD_MODULATION_R_Z'
     print*,'=> PROGRAM STOPPED.'
     stop
  endif

  return

end function bfield_modulation

