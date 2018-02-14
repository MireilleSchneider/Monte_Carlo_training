module mod_constants
! ---------------------------------------------------------
! THIS MODULE CONTAINS THE DEFINITION OF PHYSICS CONSTANTS
! ---------------------------------------------------------
implicit none

double precision:: pi,qel,pmass,eps0,emass

parameter(pi    = 3.14159265358979) ! PI CONSTANT
parameter(qel   = 1.6022e-19)       ! ELECTRON CHARGE (C)
parameter(pmass = 1.6726e-27)       ! PROTON MASS (KG)
parameter(emass = 9.1095e-31)       ! ELECTRON MASS (KG)
parameter(eps0  = 8.854e-12)

end module mod_constants

