subroutine orbit_increments(scal,inp2d,particle,particle_increments,iescape)
  ! ---------------------------------------------------------------------------
  ! PURPOSE:
  ! TO CALCULATE THE VARIATION OF THE ION TRAJECTORY AND KINEMATICS
  ! AFTER A TIME STEP DT
  ! ---------------------------------------------------------------------------
  ! INPUT:
  ! - SCAL  = STRUCTORE FOR INPUT SCALAR VARIABLES
  ! - INP2D = STRUCTURE FOR INPUT 2D-GRIDS
  ! - PARTICLE = PARTICLE STRUCTURE
  ! ---------------------------------------------------------------------------
  ! OUTPUT:
  ! - PARTICLE_INCREMENTS = VAROATON OF THE PARTICLE POSITION AND KINEMATICS
  ! ---------------------------------------------------------------------------
  ! INPUT/OUTPUT:
  ! - IESCAPE = FLAG (=1 IF PARTICLE ESCAPED, =O OTHERWISE)   
  ! ---------------------------------------------------------------------------
  use mod_constants     ! PHYSICS CONSTANTS
  use mod_particle      ! PARTICLE STRUCTURE
  use mod_input_scalars ! INPUT SCALAR VARIABLES
  use mod_input_2d      ! INPUT 2D VARIABLES

  implicit none

  ! INPUT/OUTPUT VARIABLES
  type(input_scalars), intent(in):: scal
  type(input_2d),      intent(in):: inp2d
  type(partic),        intent(in):: particle
  type(partic),        intent(out):: particle_increments
  integer,             intent(inout):: iescape

  ! LOCAL VARIABLES
  double precision:: rpa,zpa,fpa,br,bz,bf,pasg
  parameter(pasg=0.01) ! STEPSIZE FOR DERIVATIVES
  double precision:: dbrdr, dbzdr, dbfdr, dbrdz, dbzdz, dbfdz
  double precision:: dbrdf, dbzdf, dbfdf
  double precision:: dbrdrm,dbzdrm,dbfdrm,dbrdzm,dbzdzm,dbfdzm
  double precision:: dbrdrp,dbzdrp,dbfdrp,dbrdzp,dbzdzp,dbfdzp
  double precision:: bmod,bdum,gbr,gbz,gbf,cvd,dr,dz,df,dvpara,vdrift
  double precision:: vpara,vel,mtmagn,vperp2,bfield_modulation,pasf

  ! INITIALIZATIONS
  dr     = 0.
  dz     = 0.
  df     = 0.
  dvpara = 0.

  ! INCREMENT FOR PHI-COORDINATE
  pasf = pasg/rpa

  ! ORIGINAL POSITION, VELOCITY AND PARALLEL VELOCITY OF THE PARTICLE
  rpa    = particle%rpa
  zpa    = particle%zpa
  fpa    = particle%fpa
  vel    = particle%vel
  vpara  = particle%vpara
  mtmagn = particle%magnm

  ! FIELD CALCULATION AT THE PRESENT POINT
  call bfield_components(scal,inp2d,rpa,zpa,fpa,br,bz,bf,bmod,iescape)

  ! R,Z DERIVATIVES
  call bfield_components(scal,inp2d,rpa-pasg,zpa,fpa,dbrdrm,dbzdrm,dbfdrm,bdum,iescape)
  call bfield_components(scal,inp2d,rpa+pasg,zpa,fpa,dbrdrp,dbzdrp,dbfdrp,bdum,iescape)
  call bfield_components(scal,inp2d,rpa,zpa-pasg,fpa,dbrdzm,dbzdzm,dbfdzm,bdum,iescape)
  call bfield_components(scal,inp2d,rpa,zpa+pasg,fpa,dbrdzp,dbzdzp,dbfdzp,bdum,iescape)

  dbrdr = (dbrdrp-dbrdrm)/(2*pasg) ! DBR/DR
  dbzdr = (dbzdrp-dbzdrm)/(2*pasg) ! DBZ/DR
  dbfdr = (dbfdrp-dbfdrm)/(2*pasg) ! DBF/DR

  dbrdz = (dbrdzp-dbrdzm)/(2*pasg) ! DBR/DZ
  dbzdz = (dbzdzp-dbzdzm)/(2*pasg) ! DBZ/DZ
  dbfdz = (dbfdzp-dbfdzm)/(2*pasg) ! DBF/DZ

  ! B-FIELD MODULATION DUE TO RIPPLE (PHI-DERIVATIVE)
  if(scal%ripple.eq.1) then
     bf    = bfield_modulation(scal,inp2d,rpa,zpa,fpa,bf,1)
     dbfdf = bfield_modulation(scal,inp2d,rpa,zpa,fpa,bf,2)
     dbrdf = 0.
     dbzdf = 0.
  else
     dbrdf = 0.
     dbzdf = 0.
     dbfdf = 0.
  endif

  if(iescape.eq.0) then

     bmod = sqrt(br**2+bz**2+bf**2)

     gbr  = (br*dbrdr+bz*dbzdr+bf*dbfdr)/bmod
     gbz  = (br*dbrdz+bz*dbzdz+bf*dbfdz)/bmod
     gbf  = (br*dbrdf+bz*dbzdf+bf*dbfdf)/bmod

     ! PARTICLE CHARACTERISTICS
     vperp2 = vel**2-vpara**2
     cvd    = pmass*scal%a_number/(scal%z_number*qel)*(vperp2/2+vpara**2)/bmod**2

     ! DRIFT VELOCITY (M/S)
     vdrift = cvd*bmod/rpa

     ! VARIATION OF TRAJECTORY AND KINEMATICS
     dr = ( vpara*br+cvd*(bf*gbz-bz*gbf) )*scal%dt/bmod
     dz = ( vpara*bz+cvd*(br*gbf-bf*gbr) )*scal%dt/bmod
     df = ( vpara*bf+cvd*(bz*gbr-br*gbz) )*scal%dt/bmod/rpa
     dvpara = -mtmagn/(pmass*scal%a_number)*(br*gbr+bf*gbf+bz*gbz)*scal%dt/bmod

     ! ------------------------------
     ! OUTPUT OF PARTICLE INCREMENTS
     ! ------------------------------
     particle_increments%rpa   = dr
     particle_increments%zpa   = dz
     particle_increments%fpa   = df
     particle_increments%vpara = dvpara

  endif

  return
end subroutine orbit_increments
