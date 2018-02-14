subroutine new_born(scal,bigstep,kpresent,isource)
  ! ---------------------------------------------------------------------
  ! PURPOSE:
  ! TO EVALUATE THE NUMBER OF NEW-BORN PARTICLES FOR EACH BIG TIME STEP
  ! ---------------------------------------------------------------------
  ! INPUT:
  ! - SCAL      = STRUCTORE FOR INPUT SCALAR VARIABLES
  ! - BIGSTEP   = BIG TIME STEP (S)
  ! ---------------------------------------------------------------------
  ! INPUT/OUTPUT:
  ! - KPRESENT  = PRESENT NUMBER OF MC PARTICLES INSIDE PLASMA (-)
  ! ---------------------------------------------------------------------
  ! OUTPUT:
  ! - ISOURCE   = NUMBER OF NEW CREATED PARTICLES (-)
  ! ---------------------------------------------------------------------

  use mod_randomsp      ! SEED FOR RANDOM NUMBERS
  use mod_input_scalars ! INPUT SCALAR VARIABLES

  implicit none

  ! INPUT/OUTPUT VARIABLES
  type(input_scalars), intent(in):: scal
  integer,             intent(in):: bigstep
  integer,             intent(out):: isource
  integer,             intent(inout):: kpresent

  ! LOCAL VARIABLES
  double precision:: trunc,rsource

  ! ----------------------------------------------------------------
  ! DETERMINE THE NUMBER OF PARTICLES CREATED AT EACH BIG TIME STEP
  ! ----------------------------------------------------------------

  ! REAL NUMBER OF PARTICLES THAT MUST BE SIMULATED
  rsource = 1.*scal%nperstep

  ! WE MUST CONSIDER AN INTEGER NUMBER OF PARTICLES...
  isource = int(rsource)

  ! ... BUT WE CAN TREAT THE NON-INTEGER PART USING A RANDOM TEST
  trunc = rsource - isource
  if(ran2(0).lt.trunc) isource = isource + 1

  ! CONFIGURATION TO FOLLOW ONLY ONE PARTICLE (FOR ORBIT TRACING)
  if(scal%one_orbit.eq.1) then
     if(bigstep.eq.1) then
        isource = 1
     else
        isource = 0
     endif
  endif

  ! NUMBER OF PRESENT PARTICLE AFTER EACH TIME STEP
  kpresent = kpresent + isource

  return
end subroutine new_born
