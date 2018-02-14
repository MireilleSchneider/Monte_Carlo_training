subroutine source(scal,inp1d,inp2d,src,kpresent,isource,bigstep,inipartic,totpartic,sizetot,error_flag)
  ! -----------------------------------------------------------------------------------------------
  ! PURPOSE: 
  ! TO GENERATE THE INITIAL STATE OF EACH NEW FUSION-BORN ALPHA PARTICLES
  ! -----------------------------------------------------------------------------------------------
  ! INPUT:
  ! - SCAL     = STRUCTORE FOR INPUT SCALAR VARIABLES
  ! - INP1D    = STRUCTURE FOR INPUT 1D-PROFILES
  ! - INP2D    = STRUCTURE FOR INPUT 2D-GRIDS
  ! - SRC      = ALPHA PARTICLE SOURCE STRUCTURE
  ! - KPRESENT = PRESENT NUMBER OF MC PARTICLES INSIDE PLASMA (-)
  ! - ISOURCE  = NUMBER OF NEW CREATED PARTICLES (-)
  ! - SIZETOT  = DIMENSION OF INIPARTIC AND TOTPARTIC STRUCTURES
  ! -----------------------------------------------------------------------------------------------
  ! INPUT/OUTPUT:
  ! - INIPARTIC = STRUCTORE OF INITIAL PARTICLE STATE
  ! - TOTPARTIC = STRUCTORE OF FINAL PARTICLE STATE
  ! -----------------------------------------------------------------------------------------------
  ! INPUT/OUTPUT:
  ! - ERROR_FLAG = -1 IN CASE OF ERROR, =0 OTHERWISE
  ! -----------------------------------------------------------------------------------------------
  use mod_constants     ! PHYSICS CONSTANTS
  use mod_randomsp      ! SEED FOR RANDOM NUMBERS
  use mod_particle      ! PARTICLE STRUCTURE
  use mod_input_scalars ! INPUT SCALAR VARIABLES
  use mod_input_1d      ! INPUT 1D VARIABLES
  use mod_input_2d      ! INPUT 2D VARIABLES
  use mod_source        ! STRUCTURE FOR SOURCE OF PARTICLES

  implicit none

  ! INPUT/OUTPUT VARIABLES
  type(input_scalars), intent(in):: scal
  type(input_1d),      intent(in):: inp1d
  type(input_2d),      intent(in):: inp2d
  type(alsrc),         intent(in):: src
  integer,             intent(in):: kpresent,isource,bigstep,sizetot
  integer,             intent(inout):: error_flag
  type(partic),        intent(inout), dimension(sizetot):: inipartic,totpartic

  ! LOCAL VARIABLES
  integer:: ktry,inew,imingen,imaxgen,i,j,ifound,iescape,rc
  double precision:: velpart,velpartrand,rpartrand,rpart,fpartrand,fpart,xipartrand,xipart
  double precision:: esource,psource,zrand,zrandv,zpartrand
  double precision:: energy_source,position_source,zpart,ti,rhonorm,rhoinit
  double precision:: ndeut,ntrit,ntot(scal%n_ions),dummy_generation
  double precision:: rhomin,rhomax

  ! INITIALIZATIONS
  rhomin = 0.
  rhomax = 1.

  ! GENERATE NEW INITIAL STATE ONLY FOR NEW PARTICLES
  imingen = kpresent-isource+1
  imaxgen = kpresent

  ! ----------------------------------------------
  ! DETERMINE A POSSIBLE POSITION OF THE PARTICLE
  ! ----------------------------------------------

  ! LOOP OVER NEW PARTICLES TO SELECT A POSITION
  do inew = imingen,imaxgen

     ifound = 0
     ktry   = 0

     do while(ifound.eq.0)

        ! KEEP TRYING
        ktry = ktry+1

        ! CLASSIC MONTE CARLO GENERATION
        if(scal%forced_init.eq.0) then

           ! TO_BE_ADDED_HERE
           ! (R,Z) candidate values have to be generated here
           rpart = dummy_generation(scal,1)
           zpart = dummy_generation(scal,2)

        ! FORCE THE INITIAL STATE
        else
           rpart = scal%forced_rpa_init
           zpart = scal%forced_zpa_init
        endif

        iescape = 0

        ! DETERMINE THE DENSITY OF DEUTERIUM AND TRITIUM AND ION TEMPERATURE
        call interp2d(scal%rmin,scal%rmax,scal%zmin,scal%zmax &
             ,inp2d%rho2d,shape(inp2d%rho2d),rpart,zpart,rhoinit,iescape)  ! RHO
        rhonorm = rhoinit/inp1d%rho_in(scal%nin1d)
        call interp1d(rhomin,rhomax,inp1d%ti(:,1),scal%nin1d,rhonorm,ti,iescape) ! TI
        do i=1,scal%n_ions
           call interp1d(rhomin,rhomax,inp1d%ntot(:,i),scal%nin1d,rhonorm,ntot(i),iescape) ! NTOT
        enddo

        if(iescape.eq.0.or.scal%rz_source.eq.0) then

           ! FIND DEUTERIUM AND TRITIUM INSIDE INPUT PLASMA COMPOSITION
           do i=1,scal%n_ions
              if(scal%a_ions(i).eq.2.and.scal%z_ions(i).eq.1) ndeut = ntot(i)
              if(scal%a_ions(i).eq.3.and.scal%z_ions(i).eq.1) ntrit = ntot(i)
           enddo

           ! CALCULATE S/SMAX FOR THE POSSIBLE PARTICLE POSITION
           psource = position_source(src%nalphamax,ti,ndeut,ntrit,rpart)

           ! TO_BE_ADDED_HERE
           ! selection of good (R,Z) position candidate
           ! (the following if statement has to be replaced by the real test)
           ! once implemented: change scal%rz_source to 1 in input.f90
           if(scal%rz_source.eq.0) then

              ifound = 1 ! A GOOD PARTICLE POSITION HAS BEEN GENERATED

              ! GENERATE RANDOM TOROIDAL ANGLE BETWEEN 0 AND 2*PI
              fpartrand = ran2(0)
              fpart = scal%fmin+(scal%fmax-scal%fmin)*fpartrand

              ! FILL THE TOTAL PARTICLE ARRAY
              totpartic(inew)%rpa     = rpart
              totpartic(inew)%zpa     = zpart
              totpartic(inew)%fpa     = fpart
              totpartic(inew)%rho     = rhoinit
              totpartic(inew)%hweight = src%weight

           endif

        endif ! END TEST IESCAPE

        ! SECURITY
        if(ktry.eq.5000000) then
           write(*,*) 'NO PARTICLE FOUND OVER 5000000 ATTEMPTS (FOR POSITION SELECTION)'
           print*, 'STRANGE...'
           print*, '=> PROGRAM STOPPED'
           error_flag = -1
           return
        endif

     enddo    ! END LOOP OVER IFOUND
  enddo       ! END LOOP OVER INEW

  ! ----------------------------------------------
  ! DETERMINE A POSSIBLE VELOCITY OF THE PARTICLE
  ! ----------------------------------------------

  ! LOOP OVER NEW PARTICLES TO SELECT AN ENERGY
  do inew = imingen,imaxgen

     ifound = 0
     ktry   = 0

     do while(ifound.eq.0)

        ! KEEP TRYING
        ktry = ktry+1

        ! CLASSIC MONTE CARLO GENERATION
        if(scal%forced_init.eq.0) then

           ! TO_BE_ADDED_HERE
           ! (velocity and pitch angle candidate values have to be generated here)
           velpart = dummy_generation(scal,3)
           xipart  = dummy_generation(scal,4)

           ! FORCE THE INITIAL STATE
        else
           velpart = sqrt(2e3*qel*scal%forced_engy_init/(scal%a_number*pmass))
           xipart  = scal%forced_pitch_init
        endif

        ! ION TEMPERATURE (USED IN THE ENERGY DISPERSION OF THE SOURCE)
        rhoinit = totpartic(inew)%rho
        rhonorm = rhoinit/inp1d%rho_in(scal%nin1d)

        iescape = 0
        call interp1d(rhomin,rhomax,inp1d%ti(:,1),scal%nin1d,rhonorm,ti,iescape)

        ! CALCULATE S/SMAX FOR THE POSSIBLE PARTICLE VELOCITY (AND POSITION VIA LOCAL ION TEMPERATURE)
        esource = energy_source(scal,velpart,ti)

        ! TO_BE_ADDED_HERE
        ! (the selection of the good velocity candidate has to be coded here,
        ! i.e the following if statement has to be replaced by the real test)
        if(1.eq.1) then

           ifound = 1 ! A GOOD PARTICLE VELOCITY HAS BEEN GENERATED

           ! FILL THE TOTAL PARTICLE ARRAY
           totpartic(inew)%vel     = velpart
           totpartic(inew)%vpara   = velpart*xipart
           totpartic(inew)%iescape = iescape

        endif

        ! SECURITY
        if(ktry.eq.5000000) then
           write(*,*) 'NO PARTICLE FOUND OVER 5000000 ATTEMPTS (FOR ENERGY SELECTION)'
           print*, 'STRANGE...'
           print*, '=> PROGRAM STOPPED'
           error_flag = -1
           return
        endif

     enddo ! END LOOP OVER IFOUND
  enddo    ! END LOOP OVER INEW

  ! COPY THE PARTICLE STRUCTURE TO SAVE THE INITIAL STATE
  inipartic(imingen:imaxgen) = totpartic(imingen:imaxgen)

  ! CLEAN EXIT
  if(scal%one_orbit.eq.1.and.totpartic(1)%iescape.eq.1.and.bigstep.eq.1) then
     write(*,*) 'THE INITIAL POSITION OF THE PARTICLE IS NOT INSIDE THE PLASMA'
     write(*,*) 'YOU SHOULD FORCE ITS INITIAL STATE (WITH FORCED_INIT=1)'
     write(*,*) '=> PROGRAM STOPPED.'
     error_flag = -1
     return
  endif

  return
end subroutine source
