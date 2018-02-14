subroutine ion_trajectory(scal,inp1d,inp2d,bigstep,out1d,particle)
  ! ---------------------------------------------------------------------------
  ! PURPOSE:
  ! TO COMPUTE THE ION TRAJECTORY USING A FOURTH ORDER RUNGE KUTTA INTEGRATION
  ! ---------------------------------------------------------------------------
  ! INPUT:
  ! - SCAL    = STRUCTORE FOR INPUT SCALAR VARIABLES
  ! - INP1D   = STRUCTURE FOR INPUT 1D-PROFILES
  ! - INP2D   = STRUCTURE FOR INPUT 2D-GRIDS
  ! - BIGSTEP = INDEX OF THE BIG TIME STEP
  ! ---------------------------------------------------------------------------
  ! INPUT/OUTPUT:
  ! - OUT1D    = STRUCTURE FOR OUTPUT 1D-PROFILES
  ! - PARTICLE = PARTICLE STRUCTURE
  ! ---------------------------------------------------------------------------

  use mod_constants     ! PHYSICS CONSTANTS
  use mod_particle      ! PARTICLE STRUCTURE
  use mod_input_scalars ! INPUT SCALAR VARIABLES
  use mod_input_1d      ! INPUT 1D VARIABLES
  use mod_input_2d      ! INPUT 2D GRIDS
  use mod_output_1d     ! OUTPUT 1D VARIABLES
  use mod_source        ! STRUCTURE FOR SOURCE OF PARTICLES

  implicit none

  ! INPUT/OUTPUT VARIABLES
  type(input_scalars), intent(inout):: scal
  type(input_1d),      intent(in):: inp1d
  type(input_2d),      intent(in):: inp2d
  integer,             intent(in):: bigstep
  type(output_1d),     intent(inout):: out1d
  type(partic),        intent(inout):: particle

  ! LOCAL VARIABLES
  type(partic):: particle_step,particle_increments
  double precision:: zdr1,zdz1,zdf1,zdvpara1
  double precision:: zdr2,zdz2,zdf2,zdvpara2
  double precision:: zdr3,zdz3,zdf3,zdvpara3
  double precision:: zdr4,zdz4,zdf4,zdvpara4
  double precision:: br,bz,bf,bmod,bfield_modulation

  ! STORE THE R-POSITION BEFORE THE INTEGRATION TIME STEP
  particle%rpa_old = particle%rpa

  ! CALCULATE THE MAGNETIC FIELD COMPONENTS
  call bfield_components(scal,inp2d,particle%rpa,particle%zpa,particle%fpa,br,bz,bf,bmod,particle%iescape)

  ! PARTICLE MAGNETIC MOMENUTUM
  particle%magnm = scal%a_number*pmass*(particle%vel**2-particle%vpara**2)/(2.*bmod)

  ! -------------------------------------------------------------
  ! FIRST STEP RUNGE KUTTA: VARIATION OF R,Z,F AND VPARA
  ! -------------------------------------------------------------
  ! K1 = H * F(TN,YN)
  ! WITH H  = TIME STEP
  !      TN = ELAPSED TIME AT INITIAL COORDINATES
  !      YN = R   , Z   , VPARA
  !      K1 = ZDR1, ZDZ1, ZDVPARA1
  ! -------------------------------------------------------------

  ! CALCULATE K1 = H * F(TN,YN)
  particle_step%rpa     = particle%rpa
  particle_step%zpa     = particle%zpa
  particle_step%fpa     = particle%fpa
  particle_step%vpara   = particle%vpara
  particle_step%vel     = particle%vel
  particle_step%magnm   = particle%magnm

  call orbit_increments(scal,inp2d,particle_step,particle_increments,particle%iescape)
  if(particle%iescape.eq.1) return

  zdr1     = particle_increments%rpa
  zdz1     = particle_increments%zpa
  zdf1     = particle_increments%fpa
  zdvpara1 = particle_increments%vpara

  ! --------------------------------------------------------------
  ! SECOND STEP RUNGE KUTTA: VARIATION OF R,Z,F AND VPARA
  ! --------------------------------------------------------------
  ! K2 = 0.5*H*F(XN,YN) + 0.5*H*F(XN+H,YN+K1)
  ! WITH H         = TIME STEP
  !      XN + H    = ELAPSED TIME AT INTERMEDIATE COORDINATES
  !      YN + K1   = RI,   ZI,   VPARAI (INTERM. COORDINATES)
  !      K1        = ZDR1, ZDZ1, ZDVPARA1
  !      K2        = ZDR2, ZDZ2, ZDVPARA2
  ! --------------------------------------------------------------

  ! NEW COORDINATES TO BE CONSIDERED FOR THIS RK STEP: F(XN+H,YN+K1)
  particle_step%rpa   = particle%rpa   + zdr1
  particle_step%zpa   = particle%zpa   + zdz1
  particle_step%fpa   = particle%fpa   + zdf1
  particle_step%vpara = particle%vpara + zdvpara1
  particle_step%vel   = particle%vel
  particle_step%magnm = particle%magnm

  ! CALCULATE K2 = 0.5*H*F(XN,YN) + 0.5*H*F(XN+H,YN+K1)
  call orbit_increments(scal,inp2d,particle_step,particle_increments,particle%iescape)
  if(particle%iescape.eq.1) return

  zdr2     = 0.5 * zdr1     + 0.5 * particle_increments%rpa
  zdz2     = 0.5 * zdz1     + 0.5 * particle_increments%zpa
  zdf2     = 0.5 * zdf1     + 0.5 * particle_increments%fpa
  zdvpara2 = 0.5 * zdvpara1 + 0.5 * particle_increments%vpara

  ! ---------------------------------------------------------------------
  ! THIRD STEP RUNGE KUTTA: VARIATION OF R,Z,F AND VPARA
  ! ---------------------------------------------------------------------
  ! K3 = 0.5*H*F(XN,YN) + 0.5*H*F(XN+H,YN+K2)
  ! WITH H         = TIME STEP
  !      XN + H    = ELAPSED TIME AT INTERMEDIATE COORDINATES
  !      YN + K2   = RWJ,  ZWJ,  ZDVPARA2 (INTERMEDIATE COORDINATES)
  !      K2        = ZDR2, ZDZ2, ZDVPARA2
  !      K3        = ZDR3, ZDZ3, ZDVPARA3
  ! ---------------------------------------------------------------------

  ! NEW COORDINATES TO BE CONSIDERED FOR THIS RK STEP: F(XN+H,YN+K2)
  particle_step%rpa   = particle%rpa   + zdr2
  particle_step%zpa   = particle%zpa   + zdz2
  particle_step%fpa   = particle%fpa   + zdf2
  particle_step%vpara = particle%vpara + zdvpara2
  particle_step%vel   = particle%vel
  particle_step%magnm = particle%magnm 

  ! CALCULATE K3 = 0.5*H*F(XN,YN) + 0.5*H*F(XN+H,YN+K2)
  call orbit_increments(scal,inp2d,particle_step,particle_increments,particle%iescape)
  if(particle%iescape.eq.1) return

  zdr3     = 0.5 * zdr1     + 0.5 * particle_increments%rpa
  zdz3     = 0.5 * zdz1     + 0.5 * particle_increments%zpa
  zdf3     = 0.5 * zdf1     + 0.5 * particle_increments%fpa
  zdvpara3 = 0.5 * zdvpara1 + 0.5 * particle_increments%vpara

  ! ----------------------------------------------------------------------
  ! FOURTH STEP RUNGE KUTTA: VARIATION OF R,Z,F AND VPARA
  ! ----------------------------------------------------------------------
  ! K4 = H*F(XN+H,YN+K3)
  ! WITH H         = TIME STEP
  !      XN + H    = ELAPSED TIME AT INTERMEDIATE COORDINATES
  !      YN + K3   = RWK,  ZWK,  ZDVPARA3 (INTERMEDIATE COORDINATES)
  !      K3        = ZDR3, ZDZ3, ZDVPARA3
  !      K4        = DRWQ, DZWQ, DVPARAQ
  ! ----------------------------------------------------------------------

  ! NEW COORDINATES TO BE CONSIDERED FOR THIS RK STEP: F(XN+H,YN+K3)
  particle_step%rpa   = particle%rpa   + zdr3
  particle_step%zpa   = particle%zpa   + zdz3
  particle_step%fpa   = particle%fpa   + zdf3
  particle_step%vpara = particle%vpara + zdvpara3
  particle_step%vel   = particle%vel
  particle_step%magnm = particle%magnm 

  ! CALCULATE K4 = H*F(XN+H,YN+K3)
  call orbit_increments(scal,inp2d,particle_step,particle_increments,particle%iescape)
  if(particle%iescape.eq.1) return

  zdr4     = particle_increments%rpa
  zdz4     = particle_increments%zpa
  zdf4     = particle_increments%fpa
  zdvpara4 = particle_increments%vpara

  ! -------------------------------------------------------------
  ! FINAL CHARACTERISTICS AFTER THE RUNGE KUTTA SINGLE TIME STEP
  ! -------------------------------------------------------------
  particle%rpa     = particle%rpa   + zdr1     /6. + zdr2     /3. + zdr3     /3. + zdr4     /6.
  particle%zpa     = particle%zpa   + zdz1     /6. + zdz2     /3. + zdz3     /3. + zdz4     /6.
  particle%fpa     = particle%fpa   + zdf1     /6. + zdf2     /3. + zdf3     /3. + zdf4     /6.
  particle%vpara   = particle%vpara + zdvpara1 /6. + zdvpara2 /3. + zdvpara3 /3. + zdvpara4 /6.

  ! TIME INCREMENTATION
  particle%timenow = particle%timenow + scal%nacc*scal%dt

  ! -----------------------------------------------------------------------------------

  ! MONTE CARLO OPERATOR FOR ANOMALOUS TRANSPORT
  if(scal%ano_transport.eq.1) call anotransport(scal,inp1d,inp2d,particle)

  ! MONTE CARLO OPERATOR FOR COLLISIONS
  if(scal%collision.eq.1) call collision(scal,inp1d,inp2d,bigstep,particle,out1d)

  ! MONTE CARLO OPERATOR FOR INTERACTION WITH ICRF WAVES
  if(scal%icrh_heating.eq.1) call icrh_heating(scal,inp1d,inp2d,particle)

  ! -----------------------------------------------------------------------------------

  ! ORBIT TRACING
  if(scal%one_orbit.eq.1) then
     write(222,*) particle%rpa,particle%zpa,particle%fpa
     if(scal%collision.eq.1) write(333,*) particle%timenow,particle%vel,particle%vpara
  endif

  ! THIS IS NOT THE FIRST BIG TIME STEP, FIRST PARTICLE, FIRST INTEGRATION TIME STEP ANYMORE
  scal%start = 0
 
end subroutine ion_trajectory
