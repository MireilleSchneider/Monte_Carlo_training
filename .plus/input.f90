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
  read(200,'(a20,i8)')   cdum,scal%ripple             ! FLAG TO ACTIVATE MAGNETIC RIPPLE MODULATION
  read(200,'(a20,i8)')   cdum,scal%collision          ! FLAG TO ACTIVATE COLLISIONS
  read(200,'(a20,i8)')   cdum,scal%icrh_heating       ! FLAG TO ACTIVATE INTERACTIONS WITH ICRF WAVES
  read(200,'(a20,i8)')   cdum,scal%forced_init        ! FLAG TO FORCE THE INITIAL STATE OF THE PARTICLE
  read(200,'(a30)')      cline
  read(200,'(a20,f8.4)') cdum,scal%forced_pitch_init  ! FORCED INITIAL PITCH ANGLE (-)
  read(200,'(a20,f8.4)') cdum,scal%forced_rpa_init    ! FORCED INITIAL R-POSITION (M)
  read(200,'(a20,f8.4)') cdum,scal%forced_zpa_init    ! FORCED INITIAL Z-POSITION (M)
  read(200,'(a20,f8.4)') cdum,scal%forced_engy_init   ! FORCED INITIAL ENERGY (KEV)
  read(200,'(a30)')      cline
  read(200,'(a20,f8.4)') cdum,scal%diffusion          ! DIFFUSION COEFFICIENT FOR ANOMALOUS TRANSPORT (M^2/S)
  read(200,'(a20,f8.4)') cdum,scal%convection         ! CONVECTION VELOCITY FOR ANOMALOUS TRANSPORT (M/S)
  read(200,'(a30)')      cline
  read(200,'(a20,i8)')   cdum,scal%colflag            ! FLAG FOR SLOWING-DOWN + ENGY DIFFUSION PROCESSES
  read(200,'(a20,i8)')   cdum,scal%xiflag             ! FLAG FOR PITCH ANGLE SCATTERING
  read(200,'(a20,i8)')   cdum,scal%ediflag            ! FLAG FOR ENGY DIFFUSION, =0 OTHERWISE
  read(200,'(a20,i8)')   cdum,scal%nacc               ! ACCELERATION FACTOR FOR COLLISIONS
  read(200,'(a30)')      cline
  read(200,'(a20,f8.4)') cdum,scal%icrh_frequency     ! ICRH FREQUENCY (MHZ)
  read(200,'(a20,i8)')   cdum,scal%a_number           ! MASS NUMBER OF THE FOLLOWED ION (UMA)
  read(200,'(a20,i8)')   cdum,scal%z_number           ! CHARGE NUMBER OF THE FOLLOWED ION (-)
  close(200)

  ! STARING FLAG: =1 FOR FIRST BIG TIME STEP, FIRST PARTICLE, FIRST INTEGRATION TIME STEP
  scal%start = 1

  ! TOTAL NUMBER OF BIG TIME STEPS (-)
  if(scal%orbit_following.eq.1) then
     scal%nbigstep = max(1.d0,scal%simu_dt/scal%bigwidth)
  else
     scal%nbigstep = 1
  endif

  ! INITIALIZE AND ALLOCATE OUTPUT STRUCTURES
  call output_1d_constructor(out1d,scal%nbigstep)

  ! EXERCISE VALIDATION (=0 WHEN EXERCISE NOT COMPLETED, =1 TO TEST THE IMPLEMENTATION)
  scal%rz_source = 1 ! (R,Z) SOURCE GENERATION

  ! NUMBER OF PARTICLES TO GENERATE PER BIG TIME STEP (-)
  scal%nperstep = scal%ntot*scal%bigwidth/scal%simu_dt

  ! --------------
  ! ARMOURING....
  ! --------------
  if(scal%orbit_following.eq.1) then
     if(scal%one_orbit.eq.0) then
        if(scal%nperstep.lt.1.) then
           write(*,*) '-------------------------------------------------------------------'
           write(*,*) 'BIG TIME STEP TOO SMALL COMPARED TO REQUIRED NUMBER OF MC PARTICLES'
           write(*,*) 'AND REQUIRED SIMULATION TIME'
           write(*,'(a39,f10.6)') ' NUMBER OF MC PARTICLE PER TIME STEP = ',scal%nperstep
           write(*,*) '=> YOU SHOULD INCREASE BIGWDITH OR NTOT,'
           write(*,*) '   TO GET AT LEAST 1 MC PARTICLE PER BIG TIME STEP'
           write(*,*) '=> PROGRAM STOPPED.'
           write(*,*) '-------------------------------------------------------------------'
           error_flag = -1
           return
        endif
     else
        scal%nperstep = 1
     endif
  else
     ! IF ORBITS ARE NOT FOLLOWED, ALL PARTICLES HAVE TO BE GENERATED AT FIRST TIME STEP
     if(scal%orbit_following.eq.0) scal%nperstep = scal%ntot
  endif

  if(scal%nacc.lt.1.or.scal%nacc.gt.100) then
     write(*,*) 'NACC SHOULD BE BETWEEN 1 AND 100'
     write(*,*) '=> PROGRAM STOPPED.'
     error_flag = -1
     return
  endif

  if(scal%collision.eq.11) then

     if(scal%bigwidth.lt.1.e-3) then
        write(*,*) 'THE BIG TIME STEP SHOULD NOT BE TOO SMALL WHEN COLLISIONS ARE ACTIVATED'
        write(*,*) '(TO AVOID OSCILLATIONS ON THE POWER TO THE BULK)'
        write(*,*) '=> INCREASE BIGWIDTH (MINIMUM = 1.E-2 SEC)'
        write(*,*) '=> PROGRAM STOPPED.'
        error_flag = -1
        return
     endif

     if(scal%bigwidth/scal%nacc.lt.1.e-3) then
        write(*,*) 'BIGWIDTH/NACC SHOULD BE AT LEAST 1.E-4 SEC WITH COLLISIONS'
        write(*,*) 'INCREASE BIGWIDTH OR DECREASE NACC'
        write(*,*) '=> PROGRAM STOPPED.'
        error_flag = -1
        return
     endif

  endif


  if(scal%forced_init.eq.1.and.scal%energy.ne.scal%forced_engy_init) then
     write(*,*) 'MODE FORCED_INIT=1'
     write(*,*) '=> MEAN ENERGY SCAL%ENERGY FORCED TO BE EQUAL'
     write(*,*) '   TO FORCED VALUE SCAL%FORCED_ENGY_INIT'
     scal%energy = scal%forced_engy_init
  endif

  if(scal%ripple.eq.1.and.scal%dt.gt.1.0001e-7) then
     write(*,*) 'THE INTERNAL TIME STEP MUST BE VERY LOW WHEN RIPPLE IS ACTIVATED'
     write(*,*) '=> SET DT TO 1.E-7 SEC AT MOST'
     write(*,*) '=> PROGRAM STOPPED.'
     error_flag = -1
     return
  endif

  if(scal%ripple.eq.1.and.scal%nacc.gt.1) then
     write(*,*) 'COLLISION ACCELERATION FORBIDDEN WITH RIPPLE'
     write(*,*) '=> YOU MUST SET NACC TO 1'
     write(*,*) '=> PROGRAM STOPPED.'
     error_flag = -1
     return
  endif

  if(scal%ano_transport.eq.1.and.scal%nacc.gt.1) then
     write(*,*) 'COLLISION ACCELERATION FORBIDDEN WITH ANOMALOUS TRANSPORT'
     write(*,*) '=> YOU MUST SET NACC TO 1'
     write(*,*) '=> PROGRAM STOPPED.'
     error_flag = -1
     return
  endif

  if(scal%dt.gt.1.e-6) then
     write(*,*) 'INTERNAL TIME STEP MUCH TOO LARGE TO ACCURATELY DESCRIBE THE ORBIT'
     write(*,*) '=> YOU MUST DECREASE DT'
     write(*,*) '=> PROGRAM STOPPED.'
     error_flag = -1
     return
  endif

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

  ! GOOD ACCURACY NEEDED WITH ICRH HEATING
  if(scal%icrh_heating.eq.1.and.scal%dt.gt.5.001e-8) then
     write(*,*) 'YOU NEED A LOWER INTEGRATION TIME STEP TO GET A NICE RESONANCE LAYER'
     write(*,*) 'MAXIMUM TOLERATED WTH ICRH: DT = 5.E-8 SEC'
     write(*,*) '=> PROGRAM STOPPED.'
     error_flag = -1
     return
  endif

  ! PREVENT THE CODE FROM RUNNING FOR NOTHING
  if(scal%ano_transport.eq.1) then

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

  ! WARNINGS
  if(scal%orbit_following.eq.0) then
     if(scal%ano_transport.eq.1) &
          write(*,*) '.... WARNING, ANOMALOUS TRANSPORT WITHOUT ORBIT FOLLOWING => NOTHING TO BE SEEN'
     if(scal%ripple.eq.1) &
          write(*,*) '.... WARNING, RIPPLE ACTIVATED WITHOUT ORBIT FOLLOWING => NOTHING TO BE SEEN'
     if(scal%collision.eq.1) &
          write(*,*) '.... WARNING, COLLISIONS ACTIVATED WITHOUT ORBIT FOLLOWING => NOTHING TO BE SEEN'
     if(scal%icrh_heating.eq.1) &
          write(*,*) '.... WARNING, ICRH HEATING WITHOUT ORBIT FOLLOWING => NOTHING TO BE SEEN'
  endif

  ! THE SIMULATION TIME MUST BE LARGE ENOUGH
  if(scal%bigwidth>scal%simu_dt) then
     write(*,*) 'INCONSISTENT INPUT SET OF PARAMETERS, YOU MUST EITHER:'
     write(*,*) '1) INCREASE SIMU_DT'
     write(*,*) '2) DECREASE BIGWIDTH'
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
  scal%b0 = 4.9

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
  inp2d%delta  = readvar('input/delta.txt          ',inp2d%rho2d)

  ! ALL IMPURITIES ARE CONSIDERED AS HAVING THE SAME TEMPERATURE
  do i=2,scal%n_ions
     inp1d%ti(:,i) = inp1d%ti(:,1)
  enddo

  ! OPEN FILE FOR STORING THE PARTICLE TRAJECTORY FOR ORBIT DISPLAY
  open(unit=222,file='output/ion_trajectory.txt',form='formatted',status='replace',iostat=rc)
  open(unit=333,file='output/ion_versus_time.txt',form='formatted',status='replace',iostat=rc)
  open(unit=444,file='output/col_ei_vs_time.txt',form='formatted',status='replace',iostat=rc)
  open(unit=555,file='output/icrh_kick_positions.txt',form='formatted',status='replace',iostat=rc)

  return

end subroutine input
