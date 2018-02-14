module mod_input_1d
! ---------------------------------------------------------------------------------------
! THIS MODULE CONTAINS THE DEFINITION OF THE STRUCTURE FOR 1D-INPUT PROFILES
! ALONG WITH ITS CONSTRUCTOR / DESTRUCTOR
! ---------------------------------------------------------------------------------------
! VSURF_IN  = PROFILE OF VOLUME BETWEEN 2 MAGNETIC SURFACES WITH INPUT RESOLUTION  (M3)
! VSURF_OUT = PROFILE OF VOLUME BETWEEN 2 MAGNETIC SURFACES WITH OUTPUT RESOLUTION (M3)
! RHO_IN    = PROFILE OF TOROIDAL FLUX COORDINATE WITH INPUT RESOLUTION            (M3)
! RHO_OUT   = PROFILE OF TOROIDAL FLUX COORDINATE WITH OUTPUT RESOLUTION           (M3)
! NE        = ELECTRON DENSITY PROFILE                                             (M-3)
! NI        = ION DENSITY PROFILE                                                  (M-3)
! TE        = ELECTRON TEMPERATURE PROFILE                                         (KEV)
! TI        = ION TEMPERATURE PROFILE                                              (KEV)
! NTOT      = ION DENSITY FOR EACH SPECIES (DIM = (NIN1D,N_IONS)                   (M-3)
! ---------------------------------------------------------------------------------------
  implicit none

  type input_1d

     double precision, dimension(:), allocatable:: vsurf_in
     double precision, dimension(:), allocatable:: vsurf_out
     double precision, dimension(:), allocatable:: rho_in
     double precision, dimension(:), allocatable:: rho_out
     double precision, dimension(:), allocatable:: ne
     double precision, dimension(:), allocatable:: ni
     double precision, dimension(:), allocatable:: te
     double precision, dimension(:,:), allocatable:: ti
     double precision, dimension(:,:), allocatable:: ntot

  end type input_1d

contains

  ! ------------------------------------------------------------------

  subroutine input_1d_constructor(inp1d,nin,nout,nimp)

    implicit none
    type(input_1d):: inp1d
    integer:: nin,nout,nimp

    if(.not.allocated(inp1d%vsurf_in))  allocate(inp1d%vsurf_in(nin))
    if(.not.allocated(inp1d%vsurf_out)) allocate(inp1d%vsurf_out(nout))
    if(.not.allocated(inp1d%rho_in))    allocate(inp1d%rho_in(nin))
    if(.not.allocated(inp1d%rho_out))   allocate(inp1d%rho_out(nout))
    if(.not.allocated(inp1d%ne))        allocate(inp1d%ne(nin))
    if(.not.allocated(inp1d%ni))        allocate(inp1d%ni(nin))
    if(.not.allocated(inp1d%te))        allocate(inp1d%te(nin))
    if(.not.allocated(inp1d%ti))        allocate(inp1d%ti(nin,nimp))
    if(.not.allocated(inp1d%ntot))      allocate(inp1d%ntot(nin,nimp))

    inp1d%vsurf_in  = 0.
    inp1d%vsurf_out = 0.
    inp1d%rho_in    = 0.
    inp1d%rho_out   = 0.
    inp1d%ne        = 0.
    inp1d%ni        = 0.
    inp1d%te        = 0.
    inp1d%ti        = 0.
    inp1d%ntot      = 0.

  end subroutine input_1d_constructor

  ! ------------------------------------------------------------------

  subroutine input_1d_destructor(inp1d)

    implicit none
    type(input_1d):: inp1d

    if(allocated(inp1d%vsurf_in))  deallocate(inp1d%vsurf_in)
    if(allocated(inp1d%vsurf_out)) deallocate(inp1d%vsurf_out)
    if(allocated(inp1d%rho_in))    deallocate(inp1d%rho_in)
    if(allocated(inp1d%rho_out))   deallocate(inp1d%rho_out)
    if(allocated(inp1d%ne))        deallocate(inp1d%ne)
    if(allocated(inp1d%ni))        deallocate(inp1d%ni)
    if(allocated(inp1d%te))        deallocate(inp1d%te)
    if(allocated(inp1d%ti))        deallocate(inp1d%ti)
    if(allocated(inp1d%ntot))      deallocate(inp1d%ntot)

  end subroutine input_1d_destructor

end module mod_input_1d

