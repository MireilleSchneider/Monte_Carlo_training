module mod_input_2d
! ------------------------------------------------------------------------
! THIS MODULE CONTAINS THE DEFINITION OF THE STRUCTURE FOR 2D-INPUT GRIDS
! ALONG WITH ITS CONSTRUCTOR / DESTRUCTOR
! ------------------------------------------------------------------------
! BR2D   = RADIAL COMPONENT OF MAGNETIC FIELD VERSUS (R,Z)     (T)
! BZ2D   = Z-COMPONENT OF MAGNETIC FIELD VERSUS (R,Z)          (T)
! BPHI2D = TOROIDAL COMPONENT OF MAGNETIC FIELD VERSUS (R,Z)   (T)
! RHO2D  = TOROIDAL FLUX COORDIANTE VERSUS (R,Z)               (M)
! DELTA  = MAGNETIC FIELD RIPPLE (TOROIDAL MODULATOINS)        (-)
! ------------------------------------------------------------------------

  implicit none

  type input_2d

     double precision, dimension(:,:),   allocatable:: br2d
     double precision, dimension(:,:),   allocatable:: bz2d
     double precision, dimension(:,:),   allocatable:: bphi2d
     double precision, dimension(:,:),   allocatable:: rho2d
     double precision, dimension(:,:),   allocatable:: delta

  end type input_2d

contains

  ! ------------------------------------------------------------------

  subroutine input_2d_constructor(inp2d,nin2d)

    implicit none
    type(input_2d):: inp2d
    integer:: nin2d

    if(.not.allocated(inp2d%br2d))   allocate(inp2d%br2d(nin2d,nin2d))
    if(.not.allocated(inp2d%bz2d))   allocate(inp2d%bz2d(nin2d,nin2d))
    if(.not.allocated(inp2d%bphi2d)) allocate(inp2d%bphi2d(nin2d,nin2d))
    if(.not.allocated(inp2d%rho2d))  allocate(inp2d%rho2d(nin2d,nin2d))
    if(.not.allocated(inp2d%delta))  allocate(inp2d%delta(nin2d,nin2d))

    inp2d%br2d   = 0.
    inp2d%bz2d   = 0.
    inp2d%bphi2d = 0.
    inp2d%rho2d  = 0.
    inp2d%delta  = 0.

  end subroutine input_2d_constructor


  ! ------------------------------------------------------------------

  subroutine input_2d_destructor(inp2d)

    implicit none
    type(input_2d):: inp2d

    if(allocated(inp2d%br2d))   deallocate(inp2d%br2d)
    if(allocated(inp2d%bz2d))   deallocate(inp2d%bz2d)
    if(allocated(inp2d%bphi2d)) deallocate(inp2d%bphi2d)
    if(allocated(inp2d%rho2d))  deallocate(inp2d%rho2d)
    if(allocated(inp2d%delta))  deallocate(inp2d%delta)

  end subroutine input_2d_destructor

end module mod_input_2d
