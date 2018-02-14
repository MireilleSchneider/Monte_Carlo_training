program alpha
  ! ----------------------------------------------------------------------------
  ! MAIN PROGRAM
  ! ------------
  ! FOKKER-PLANCK CALCULATION FOR FUSION-BORN ALPHA PARTICLE ORBIT FOLLOWING 
  ! BY A MONTE CARLO TECHNIQUE
  ! ----------------------------------------------------------------------------

  ! MODULES
  use mod_constants     ! PHYSICS CONSTANTS
  use mod_randomsp      ! SEED FOR RANDOM NUMBERS
  use mod_particle      ! PARTICLE STRUCTURE
  use mod_input_scalars ! INPUT SCALAR VARIABLES
  use mod_input_1d      ! INPUT 1D VARIABLES
  use mod_input_2d      ! INPUT 2D VARIABLES
  use mod_output_1d     ! OUTPUT 1D VARIABLES
  use mod_source        ! STRUCTURE FOR SOURCE OF PARTICLES

  implicit none

  integer:: istep,kpresent,bigstep,error_flag,kpart,isource
  type(input_1d):: inp1d
  type(input_2d):: inp2d
  type(output_1d):: out1d
  type(input_scalars):: scal
  type(alsrc):: src
  type(partic):: onepartic
  type(partic), dimension(:), allocatable:: totpartic,inipartic

  write(*,*) 'START'

  ! INITIALISATIONS
  error_flag = 0
  kpresent   = 0

  ! READ INPUT DATA
  write(*,*) 'Import input files'
  call input(scal,inp1d,inp2d,out1d,error_flag)
  if(error_flag.ne.0) stop

  ! ALLOCATE AND INITIALIZE THE TOTAL PARTICLE ARRAY
  allocate(inipartic(scal%ntot))
  allocate(totpartic(scal%ntot))
  call init_particle(inipartic,scal%ntot) ! INITIIAL STATE
  call init_particle(totpartic,scal%ntot) ! PRESENT STATE

  write(*,'(a20,f8.5,a2)') ' Time to simulate = ',scal%simu_dt,' s'

  ! ALPHA PARTICLE NUMBER AND RATE
  call fusion_reaction(scal,inp1d,inp2d,src)

  write(*,*) 'Main calculation'

  ! LOOP OVER BIG TIME STEPS
  do bigstep=1,scal%nbigstep

     ! DETERMINE THE NUMBER OF NEW-BORN PARTICLES FOR EACH BIGSTEP
     call new_born(scal,bigstep,kpresent,isource)

     ! DETERMINE INITIAL STATE (POSITION,VELOCITY,DIRECTION) OF THE NEW PARTICLES
     call source(scal,inp1d,inp2d,src,kpresent,isource,bigstep,inipartic,totpartic,size(totpartic),error_flag)
     if(error_flag.ne.0) stop

     ! FOLLOW THE PARTICLES
     if(scal%orbit_following.eq.1) then

        write(*,'(a10,f8.5,a2,a15,i7)') 'Time = ',bigstep*scal%bigwidth,' s','Particles = ',kpresent
        
        ! LOOP OVER ALL PRESENT PARTICLES
        do kpart=1,kpresent

           onepartic = totpartic(kpart)

           onepartic%timenow = (bigstep-1)*scal%bigwidth ! INITIAL TIME AT EACH BIGSTEP

           ! LOOP OVER SMALL TIME STEPS FOR ORBIT INTEGRATION
           istep = 0
           do while(onepartic%timenow.lt.scal%bigwidth*bigstep.and.onepartic%iescape.eq.0)
              
              istep = istep+1
           
              ! FOLLOW PARTICLE ORBITS
              call ion_trajectory(scal,inp1d,inp2d,bigstep,out1d,onepartic)

              totpartic(kpart) = onepartic

           enddo ! LOOP SMALL TIME STEPS
        enddo    ! LOOP PARTICLES
     endif       ! TEST FOR ORBIT FOLLOWING
  enddo          ! LOOP BIG TIME STEPS

  ! DATA OUTPUT
  write(*,*) 'Export output files'
  call output(scal,inipartic,totpartic,size(totpartic))

  ! CLOSE OUTPUT ASCII FILES
  if(scal%one_orbit.eq.1) then
     close(222)
     close(333)
     do bigstep=1,scal%nbigstep
        write(444,*) scal%bigwidth*(bigstep-1),out1d%elpower(bigstep),out1d%ionpower(bigstep)
     enddo
     close(444)
     close(555)
  endif

  ! RELEASE MEMORY
  call input_1d_destructor(inp1d)
  call input_2d_destructor(inp2d)
  call output_1d_destructor(out1d)
  call input_scalars_destructor(scal)

  write(*,*) '--> DONE'

end program alpha


