module mod_particle
  implicit none
  ! ---------------------------------------------------------------------------
  ! PARTICLE STRUCTURE
  ! -------------------
  ! IESCAPE   = FLAG FOR ESCAPED PARTICLES (=1 IF ESCAPED, =0 OTHERWISE) (-)
  ! HWEIGHT   = WEIGHT OF THE PARTICLE                                   (-)
  ! RPA       = R-POSITION OF THE PARTICLE                               (M)
  ! ZPA       = Z-POSITION OF THE PARTICLE                               (M)
  ! FPA       = PHI-ANGLE OF THE PARTICLE                                (RAD)
  ! VEL       = VELOCITY OF THE PARTICLE                                 (M/S)
  ! VPARA     = PARALLEL VELOCITY OF THE PARTICLE                        (M/S)
  ! RHO       = TOROIDAL FLUX COORDINATE OF THE PARTICLE                 (M)
  ! MAGNM     = MAGNETIC MOMENTUM OF THE PARTICLE                        (J/T)
  ! TIMENOW   = PRESENT TIME FOR THE PARTICLE                            (S)
  ! RPA_OLD   = R-POSITION AT THE PREVIOUS INTEGRATION TIME STEP         (M)
  ! ICRH_KICK = FLAG=1 IF THERE HAS BEEN AN ICRH KICK                    (-)
  ! ---------------------------------------------------------------------------
  type partic
     integer:: iescape,icrh_kick
     double precision:: hweight,rpa,zpa,fpa,vel,vpara
     double precision:: rho,magnm,timenow,rpa_old
  end type partic
end module mod_particle

