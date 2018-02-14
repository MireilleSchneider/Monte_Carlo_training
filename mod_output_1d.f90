module mod_output_1d
! ---------------------------------------------------------------------------------------
! THIS MODULE CONTAINS THE DEFINITION OF THE STRUCTURE FOR 1D-OUTPUT PROFILES
! ALONG WITH ITS CONSTRUCTOR / DESTRUCTOR
! ---------------------------------------------------------------------------------------
! VSURF_IN  = PROFILE OF VOLUME BETWEEN 2 MAGNETIC SURFACES WITH OUTPUT RESOLUTION  (M3)
! VSURF_OUT = PROFILE OF VOLUME BETWEEN 2 MAGNETIC SURFACES WITH OUTPUT RESOLUTION (M3)
! ---------------------------------------------------------------------------------------
  implicit none

  type output_1d

     double precision, dimension(:), allocatable:: elpower
     double precision, dimension(:), allocatable:: ionpower

  end type output_1d

contains

  ! ------------------------------------------------------------------

  subroutine output_1d_constructor(out1d,nbigstep)

    implicit none
    type(output_1d):: out1d
    integer:: nbigstep

    if(.not.allocated(out1d%elpower))  allocate(out1d%elpower(nbigstep))
    if(.not.allocated(out1d%ionpower)) allocate(out1d%ionpower(nbigstep))

    out1d%elpower  = 0.
    out1d%ionpower = 0.

  end subroutine output_1d_constructor

  ! ------------------------------------------------------------------

  subroutine output_1d_destructor(out1d)

    implicit none
    type(output_1d):: out1d

    if(allocated(out1d%elpower))  deallocate(out1d%elpower)
    if(allocated(out1d%ionpower)) deallocate(out1d%ionpower)

  end subroutine output_1d_destructor

end module mod_output_1d

