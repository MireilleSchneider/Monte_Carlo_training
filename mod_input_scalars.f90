module mod_input_scalars
! ----------------------------------------------------------------------------------------
! THIS MODULE CONTAINS THE DEFINITION OF THE STRUCTURE FOR SCALAR INPUT VARIABLES
! ALONG WITH ITS CONSTRUCTOR / DESTRUCTOR
! ----------------------------------------------------------------------------------------
! CHARACTERISTICS OF FOLLOWED ION
! --------------------------------
! ENERGY             = CENTRAL ENERGY OF THE FOLLOWED ION                           (KEV)
! ----------------------------------------------------------------------------------------
! RESOLUTIONS
! ------------
! NIN1D              = RESOLUTION OF 1D-INPUT PROFILES                              (-)
! NIN2D              = RESOLUTION OF 2D-INPUT GRIDS                                 (-)
! NOUT1D             = RESOLUTION OF 1D-OUTPUT PROFILES                             (-)
! NOUT2D             = RESOLUTION OF 2D-OUTPUT GRIDS                                (-)
! ----------------------------------------------------------------------------------------
! STARTING FLAG
! START              = 1 FOR THE FIRST PARTICLE, FIRST BIG AND SMALL TIME STEPS     (-)
! ----------------------------------------------------------------------------------------
! BOUNDARIES FOR POSITIONS, VELOCITY, PITCH ANGLE
! ------------------------------------------------
! RMIN               = MINIMUM R-VALUE FOR 2D (R,Z) GRIDS DEFINITION                (M)
! RMAX               = MAXIMUM R-VALUE FOR 2D (R,Z) GRIDS DEFINITION                (M)
! ZMIN               = MINIMUM Z-VALUE FOR 2D (R,Z) GRIDS DEFINITION                (M)
! ZMAX               = MAXIMUM Z-VALUE FOR 2D (R,Z) GRIDS DEFINITION                (M)
! FMIN               = MINIMUM PHI-VALUE FOR 2D (R,Z) GRIDS DEFINITION              (RAD)
! FMAX               = MAXIMUM PHI-VALUE FOR 2D (R,Z) GRIDS DEFINITION              (RAD)
! VMIN               = MINIMUM INITIAL VELOCITY OF THE PARTICLE                     (M/S)
! VMAX               = MAXIMUM INITIAL VELOCITY OF THE PARTICLE                     (M/S)
! XIMIN              = MINIMUM INITIAL PTICH ANGLE OF THE PARTICLE                  (-)
! XIMAX              = MAXIMUM INITIAL PITCH ANGLE OF THE PARTICLE                  (-)
! ----------------------------------------------------------------------------------------
! NUMBER OF PARTICLES / TIME STEPS
! ---------------------------------
! NTOT               = TOTAL NUMBER OF MONTE CARLO PARTICLES TO GENERATE            (-)
! NPERSTEP           = NUMBER OF NEW MONTE CARLO PARTICLES PER BIG TIME STEP        (-)
! NBIGSTEP           = NUMBER OF BIG TIME STEPS IN THE PRESENT SIMUALTION           (-)
! BIGWIDTH           = WIDTH OF A BIG TIME STEP                                     (S)
! DT                 = ORBIT INTEGRATION TIME STEP                                  (S)
! SIMU_DT            = TOTAL TIME TO SIMULATE                                       (S)
! ----------------------------------------------------------------------------------------
! PLASMA CHARACTERISTICS
! ----------------------
! R0                 = R-POSITION OF THE MAGNETIC CENTRE                            (M)
! Z0                 = Z-POSITION OF THE MAGNETIC CENTRE                            (M)
! B0                 = MAGNETIC FIELD IN THE CENTRE                                 (T)
! ----------------------------------------------------------------------------------------
! PLASMA ION SPECIES
! -------------------
! N_IONS             = NUMBER OF ION SPECIES INSIDE THE PLASMA                      (-)
! A_IONS             = MASS NUMBER OF PLASMA ION SPECIES                            (AMU)
! Z_IONS             = CHARGE NUMBER OF PLASMA ION SPECIES                          (-)
! ----------------------------------------------------------------------------------------
! FLAGS
! -----
! ONE_ORBIT          = FLAG =1 FOR FOLLOWING ONLY ONE ORBIT, =0 OTHERWISE           (-)
! ORBIT_FOLLOWING    = FLAG =1 TO FOLLOW PARTICLE ORBITS, =0 OTHERWISE              (-)
! FORCED_INIT        = FLAG =1 WHEN INITIAL PARTICLE STATE IS IMPOSED, =0 OTHERWISE (-)
! ANO_TRANSPORT      = FLAG =1 WHEN ANOMALOUS TRANSPORT IS ACTIVATED, =0 OTHERWISE  (-)
! RIPPLE             = FLAG =1 WHEN MAGNETIC FIELD RIPPLE IS ACTIVATED              (-)
! COLLISION          = FLAG =1 WHEN COLLISIONS ARE ACTIVATED                        (-)
! ICRH_HEATING       = FLAG =1 WHEN INTERACTION WITH ICRF WAVES IS ACTIVATED        (-)
! -----
! FLAGS FOR EXERCISE COMPLETION
! RZ_SOURCE          = FLAG =0 WHEN (R,Z) IMPLEMENTATION NOT COMPLETED              (-)
! ----------------------------------------------------------------------------------------
! FORCED INITIAL STATE
! --------------------
! FORCED_PITCH_INIT  = PITCH ANGLE VALUE WHEN IT IS FORCED                          (-)
! FORCED_ENGY_INIT   = ENERGY VALUE WHEN IT IS FORCED                               (KEV)
! FORCED_RPA_INIT    = R-COORDINATE VALUE WHEN IT IS FORCED                         (M)
! FORCED_ZPA-INIT    = Z-COORDINATE VALUE WHEN IT IS FORCED                         (M)
! ----------------------------------------------------------------------------------------
! ANOMALOUS TRANSPORT COEFFICIENTS
! ---------------------------------
! DIFFUSION          = DIFFUSION COEFFICIENT FOR ANOMALOUS TRANSPORT                (M2/S)
! CONVECTION         = CONVECTION VELOCITY FOR ANOMALOUS TRANSPORT                  (M/S)
! ----------------------------------------------------------------------------------------
! MAGNETIC FIELD RIPPLE
! ----------------------------------------------------------------------------------------
! COLLISIONS
! -----------
! COLFLAG = FLAG =1 TO SIMULATE SLOWING-DOWN + ENGY DIFFUSION ROCESSES, =0 OTHERWISE
! XIFLAG  = FLAG =1 TO SIMULATE PITCH ANGLE SCATTERING, =0 OTHERWISE
! EDIFLAG = FLAG =1 TO SIMULATE ENGY DIFFUSION, =0 OTHERWISE
! NACC    = ACCELERATION FACTOR FOR COLLISIONS
! ----------------------------------------------------------------------------------------
! ICRH WAVES
! -----------
! ICRH_FREQUENCY = FREQUENCY OF THE ICRF WAVES                                      (MHZ)
! A_NUMBER       = MASS NUMBER OF THE FOLLOWED ION                                  (UMA)
! Z_NUMBER       = CHARGE NUMBER OF THE FOLLOWED ION                                (-)
! ----------------------------------------------------------------------------------------

  implicit none

  type input_scalars

     integer:: a_number,z_number
     integer:: nin1d,nin2d,nout1d,nout2d
     integer:: ntot,nbigstep
     integer:: n_ions
     integer:: one_orbit,orbit_following
     integer:: forced_init,ano_transport,ripple
     integer:: rz_source
     integer:: collision,icrh_heating,colflag,xiflag
     integer:: ediflag,nacc,start

     double precision:: energy,nperstep
     double precision:: rmin,rmax,zmin,zmax,fmin,fmax
     double precision:: vmin,vmax,ximin,ximax
     double precision:: bigwidth,dt,simu_dt
     double precision:: r0,z0,b0
     double precision:: forced_pitch_init,forced_engy_init
     double precision:: forced_rpa_init,forced_zpa_init
     double precision:: diffusion,convection, icrh_frequency

     double precision, dimension(:), allocatable:: a_ions,z_ions

  end type input_scalars

contains

  ! ------------------------------------------------------------------

  subroutine input_scalars_constructor(scal,n_ions,nin1d,nin2d,nout1d,nout2d)

    implicit none
    type(input_scalars):: scal
    integer:: n_ions,nin1d,nin2d,nout1d,nout2d

    if(.not.allocated(scal%a_ions)) allocate(scal%a_ions(n_ions))
    if(.not.allocated(scal%z_ions)) allocate(scal%z_ions(n_ions))

    scal%n_ions    = n_ions
    scal%nin1d     = nin1d
    scal%nin2d     = nin2d
    scal%nout1d    = nout1d
    scal%nout2d    = nout2d

    scal%a_number   = 0.
    scal%z_number   = 0.
    scal%nperstep   = 0
    scal%ntot       = 0
    scal%nbigstep   = 0
    scal%rmin       = 0.
    scal%rmax       = 0.
    scal%zmin       = 0.
    scal%zmax       = 0.
    scal%ximin      = 0.
    scal%ximax      = 0.
    scal%energy     = 0.
    scal%bigwidth   = 0.
    scal%r0         = 0.
    scal%z0         = 0.
    scal%b0         = 0.
    scal%dt         = 0.
    scal%simu_dt    = 0.
    scal%a_ions     = 0.
    scal%z_ions     = 0.

  end subroutine input_scalars_constructor

  ! ------------------------------------------------------------------

  subroutine input_scalars_destructor(scal)

    implicit none
    type(input_scalars):: scal

    if(allocated(scal%a_ions)) deallocate(scal%a_ions)
    if(allocated(scal%z_ions)) deallocate(scal%z_ions)

  end subroutine input_scalars_destructor



end module mod_input_scalars

