subroutine input(scal,inp1d,inp2d,out1d,error_flag)
  ! -------------------------------------------------
  ! PURPOSE: 
  ! TO READ INPUT DATA FROM ASCII FILES
  ! -------------------------------------------------
  ! OUTPUT:
  ! - SCAL  = STRUCTORE FOR INPUT SCALAR VARIABLES
  ! - INP1D = STRUCTURE FOR INPUT 1D-PROFILES
  ! - INP2D = STRUCTURE FOR INPUT 2D-GRIDS
  ! - OUT1D = STRUCTURE FOR OUTPUT 1D-PROFILES
  ! -------------------------------------------------

  use mod_constants     ! PHYSICS CONSTANTS
  use mod_io_management ! ROUTINES FOR INPUT/OUTPUT MANAGEMENT
  use mod_input_scalars ! INPUT SCALAR VARIABLES
  use mod_input_1d      ! INPUT 1D VARIABLES
  use mod_input_2d      ! INPUT 2D VARIABLES
  use mod_output_1d     ! OUTPUT 1D VARIABLES

  implicit none

  ! INPUT/OUTPUT VARIABLES
  type(input_scalars), intent(out):: scal
  type(input_1d),      intent(out):: inp1d
  type(input_2d),      intent(out):: inp2d
  type(output_1d),     intent(out):: out1d

  ! LOCAL VARIABLES
  character(len=20):: cdum
  character(len=30):: cline
  integer:: i,n_ions,nin1d,nin2d,nout1d,nout2d,rc,error_flag

  ! NUMBER OF ION SPECIES
  n_ions = 5

  ! INPUT AND OUTPUT RESOLUTION
  nin1d  = 101
  nin2d  = 301
  nout1d = 21
  nout2d = 41

  ! INITIALIZE AND ALLOCATE STRUCTURES
  call input_scalars_constructor(scal,n_ions,nin1d,nin2d,nout1d,nout2d)
  call input_1d_constructor(inp1d,nin1d,nout1d,n_ions)
  call input_2d_constructor(inp2d,nin2d)

  ! READ INPUT PARAMETERS
  open(200,file='input/input_parameters.txt',form='formatted',status='old',iostat=rc)
  read(200,'(a20,f8.4)') cdum,scal%bigwidth           ! DURATION OF A BIG TIME STEP (S)
  read(200,'(a20,f8.4)') cdum,scal%simu_dt            ! TIME TO SIMULATE (S)
  read(200,'(a20,i8)')   cdum,scal%ntot               ! TOTAL NUMBER OF MONTE CARLO PARTICLES TO SIMULATE (-)
  read(200,'(a20,f8.4)') cdum,scal%energy             ! ALPHA PARTICLE ENERGY (KEV)
  read(200,'(a20,f8.4)') cdum,scal%dt                 ! INTERNAL TIME STEP FOR ORBIT INTEGRATION (SEC)
  read(200,'(a30)')      cline
  read(200,'(a20,i8)')   cdum,scal%one_orbit          ! FLAG TO FOLLOW ONLY ONE ORBIT (FOR TESTS OR DISPLAY PURPOSES)
  read(200,'(a20,i8)')   cdum,scal%orbit_following    ! FLAG TO FOLLOW ORBITS
  read(200,'(a20,i8)')   cdum,scal%ano_transport      ! FLAG TO SWITCH ON ANOMALOUS TRANSPORT
  read(200,'(a20,i8)')   cdum,scal%forced_init        ! FLAG TO FORCE THE INITIAL STATE OF THE PARTICLE
  read(200,'(a30)')      cline
  read(200,'(a20,f8.4)') cdum,scal%forced_pitch_init  ! FORCED INITIAL PITCH ANGLE (-)
  read(200,'(a20,f8.4)') cdum,scal%forced_rpa_init    ! FORCED INITIAL R-POSITION (M)
  read(200,'(a20,f8.4)') cdum,scal%forced_zpa_init    ! FORCED INITIAL Z-POSITION (M)
  read(200,'(a20,f8.4)') cdum,scal%forced_engy_init   ! FORCED INITIAL ENERGY (KEV)
  read(200,'(a30)')      cline
  read(200,'(a20,f8.4)') cdum,scal%diffusion          ! DIFFUSION COEFFICIENT FOR ANOMALOUS TRANSPORT (M^2/S)
  read(200,'(a20,f8.4)') cdum,scal%convection         ! CONVECTION VELOCITY FOR ANOMALOUS TRANSPORT (M/S)
  close(200)

  ! NUMBER OF PARTICLES TO GENERATE PER BIG TIME STEP (-)
  scal%nperstep = scal%ntot*scal%bigwidth/scal%simu_dt
  if(scal%nperstep.lt.1) then
     scal%bigwidth = scal%simu_dt/scal%ntot
     write(*,'(a34,f5.3,a60)') ' !!!WARNING!!! bigwidth forced to ',scal%bigwidth &
          ,' sec in order to get at least one particle per big time step'
     scal%nperstep = scal%ntot*scal%bigwidth/scal%simu_dt
  endif

  ! IF ORBITS ARE NOT FOLLOWED, ALL PARTICLES HAVE TO BE GENERATED AT FIRST TIME STEP
  if(scal%orbit_following.eq.0) scal%nperstep = scal%ntot

  ! TOTAL NUMBER OF BIG TIME STEPS (-)
  if(scal%orbit_following.eq.1) then
     scal%nbigstep = max(1.d0,scal%simu_dt/scal%bigwidth)
  else
     scal%nbigstep = 1
  endif

  ! INITIALIZE AND ALLOCATE OUTPUT STRUCTURES
  call output_1d_constructor(out1d,scal%nbigstep)

  ! EXERCISE VALIDATION (=0 WHEN EXERCISE NOT COMPLETED, =1 TO TEST THE IMPLEMENTATION)
  scal%rz_source = 0 ! (R,Z) SOURCE GENERATION

  ! --------------
  ! ARMOURING....
  ! --------------
  
  ! PREVENT RUNNING ONLY ONE ORBIT WHEN THE ORBIT TRAJECTORY IS NOT COMPUTED
  if(scal%one_orbit.eq.1.and.scal%orbit_following.eq.0) then
     write(*,*) 'YOU WANT TO SIMULATE ONLY ONE PARTICLE WITHOUT FOLLOWING ITS ORBIT'
     write(*,*) '=> POINTLESS, PROGRAM STOPPED.'
     error_flag = -1
     return
  endif

  ! PREVENT GENERATING MANY PARTICLES WITH THE SAME INITIAL STATE
  if(scal%one_orbit.eq.0.and.scal%forced_init.eq.1) then
     write(*,*) 'YOU TRY TO RUN MANY ORBITS WITH THE SAME INTIIAL STATE'
     write(*,*) '=> POINTLESS, PROGRAM STOPPED.'
     error_flag = -1
     return
  endif

  ! PREVENT THE CODE FROM RUNNING FOR NOTHING
  if(scal%ano_transport.eq.1) then

     if(scal%orbit_following.eq.0) then
        write(*,*) '.... WARNING, ANOMALOUS TRANSPORT WITHOUT ORBIT FOLLOWING => NOTHING TO BE SEEN'
     endif

     if(scal%diffusion*scal%simu_dt.gt.10.) then
        if(scal%one_orbit.eq.0) then
           write(*,*) 'TOO LARGE DIFFUSION => ALL PARTICLES ARE GOING TO GET LOST'
        else
           write(*,*) 'TOO LARGE DIFFUSION => THE PARTICLE IS GOING TO GET LOST'
        endif
        write(*,*) '=> PROGRAM STOPPED.'
        error_flag = -1
        return
     endif

     if(scal%convection*scal%simu_dt.gt.200.) then
        if(scal%one_orbit.eq.0) then
           write(*,*) 'TOO LARGE CONVECTION => ALL PARTICLES ARE GOING TO GET LOST'
        else
           write(*,*) 'TOO LARGE CONVECTION => THE PARTICLE IS GOING TO GET LOST'
        endif
        write(*,*) '=> PROGRAM STOPPED.'
        error_flag = -1
        return
     endif

     if(scal%one_orbit.eq.0) then
        if(scal%diffusion*scal%simu_dt.le.0.2) then
           write(*,*) '.... WARNING, TOO SMALL DIFFUSION FOR BEING VISIBLE ON THE ION DISTRIBUTION'
        endif
        if(scal%convection*scal%simu_dt.le.0.2) then
           write(*,*) '.... WARNING, TOO SMALL CONVECTION FOR BEING VISIBLE ON THE ION DISTRIBUTION'
        endif
     endif

  endif

  ! THE SIMULATION TIME MUST BE LARGE ENOUGH
  if(scal%bigwidth>scal%simu_dt) then
     write(*,*) 'INCONSISTENT INPUT SET OF PARAMETERS, YOU MUST EITHER:'
     write(*,*) '1) INCREASE SIMU_DT'!,scal%simu_dt
     write(*,*) '2) DECREASE BIGWIDTH'!,scal%bigwidth
     write(*,*) '... SUCH THAT BIGWIDTH < SIMU_DT'
     write(*,*) '=> PROGRAM STOPPED.'
     error_flag = -1
     return
  endif

  ! PREVENT USER FROM GROWING OLD BEFORE THE END OF THE SIMULATION
  if(scal%one_orbit.ne.1.and.scal%orbit_following.eq.1) then
     if(scal%ntot*scal%simu_dt.gt.10) then
        write(*,*) '.... NOT A GOOD IDEA IF YOU WANT A RESULT THIS WEEK.'
        write(*,*) '=> YOU MUST DECREASE SIMU_DT OR NTOT'
        write(*,*) '=> PROGRAM STOPPED.'
        error_flag = -1
        return
     elseif(scal%ntot*scal%simu_dt.gt.0.5) then
        write(*,*) '.... WARNING, THIS SIMULATION IS GOING TO TAKE A WHILE'
     endif
  endif

  ! --------------------------------------------------------------------------------------------------

  ! DEFAULT ACCELERATION FACTOR: NO ACCELERATION
  scal%nacc = 1.

  ! MASS AND CHARGE OF THE ALPHA PARTICLES
  scal%a_number = 4
  scal%z_number = 2

  ! BOUNDARIES OF R,Z,PHI COORDINATES, VELOCITY AND PITCH ANGLE
  scal%rmin = 3.679506
  scal%rmax = 9.839997
  scal%zmin = -3.599022
  scal%zmax = 4.823022
  scal%fmin = 0.
  scal%fmax = 2.*pi
  scal%vmin = 0.
  scal%vmax = sqrt(2e3*qel*scal%energy*2/(scal%a_number*pmass))
  scal%ximin = -1.
  scal%ximax = 1.

  ! MAGNETIC CENTER POSITION (M)
  scal%r0 = 6.83
  scal%z0 = 0.51

  ! -------------------
  ! PLASMA COMPOSITION
  ! -------------------
  scal%a_ions = readvar('input/a_ions.txt         ',scal%a_ions)
  scal%z_ions = readvar('input/z_ions.txt         ',scal%z_ions)

  ! --------------------------------------------------
  ! VOLUME BETWEEN TWO MAGNETIC SURFACES (IN AND OUT)
  ! --------------------------------------------------
  inp1d%vsurf_in  = readvar('input/vsurf_in.txt       ',inp1d%vsurf_in)
  inp1d%vsurf_out = readvar('input/vsurf_out.txt      ',inp1d%vsurf_out)

  ! --------------------------
  ! TOROIDAL FLUX COORDINATES
  ! --------------------------
  inp1d%rho_out = readvar('input/rho_out.txt        ',inp1d%rho_out)
  inp1d%rho_in  = readvar('input/rho_in.txt         ',inp1d%rho_in)

  ! --------------------
  ! 1D-KINETIC PROFILES
  ! --------------------
  inp1d%ne      = readvar('input/ne.txt             ',inp1d%ne)
  inp1d%te      = readvar('input/te.txt             ',inp1d%te)
  inp1d%ni      = readvar('input/ni.txt             ',inp1d%ni)
  inp1d%ti(:,1) = readvar('input/ti.txt             ',inp1d%ti(:,1))
  inp1d%ntot    = readvar('input/ntot.txt           ',inp1d%ntot)

  ! --------------------------
  ! 2D-EQUILIBRIUM QUANTITIES
  ! --------------------------
  inp2d%br2d   = readvar('input/br2d.txt           ',inp2d%br2d)
  inp2d%bz2d   = readvar('input/bz2d.txt           ',inp2d%bz2d)
  inp2d%bphi2d = readvar('input/bphi2d.txt         ',inp2d%bphi2d)
  inp2d%rho2d  = readvar('input/rho2d.txt          ',inp2d%rho2d)

  ! ALL IMPURITIES ARE CONSIDERED AS HAVING THE SAME TEMPERATURE
  do i=2,scal%n_ions
     inp1d%ti(:,i) = inp1d%ti(:,1)
  enddo

  ! OPEN FILE FOR STORING THE PARTICLE TRAJECTORY FOR ORBIT DISPLAY
  if(scal%one_orbit.eq.1) then
     open(unit=222,file='output/ion_trajectory.txt',form='formatted',status='replace',iostat=rc)
  endif

  return

end subroutine input
