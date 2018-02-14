function position_source(nalphamax,ti,ndeut,ntrit,rpa)
  ! ------------------------------------------------------------------------
  ! PURPOSE: 
  ! TO PROVIDE THE NORMALIZED SOURCE TERM OF PARTICLES AS A 
  ! FUNCTION OF THEIR POSITION INSIDE THE PLASMA
  ! ------------------------------------------------------------------------
  ! INPUT:
  !  - NALPHAMAX = MAXIMUM NUMBER OF (ALPHA PARTICLE RATE * R) (M/S)
  !  - TI        = ION TEMPERATURE (KEV)
  !  - NDEUT     = DEUTERIUM DENSITY (M-3)
  !  - NTRIT     = TRITIUM DENSITY (M-3)
  !  - RPA       = R-COORDINATE OF THE PARTICLE (M)
  ! ------------------------------------------------------------------------
  ! OUTPUT:
  !  - POSITION_SOURCE = NORMALIZED POSITION SOURCE TERM
  ! ------------------------------------------------------------------------
  ! REFERENCE:
  ! H.-S. BOSCH AND G.M. HALE 1992 NUCL. FUSION 32 611
  ! "IMPROVED FORMULAS FOR FUSION CROSS-SECTIONS AND THERMAL REACTIVITIES"
  ! ------------------------------------------------------------------------

  use mod_constants ! PHYSICS CONSTANTS
  use mod_source    ! CROSS SECTION COEFFICIENTS (BG,MRC2,C1,C2,C3,C4,C5,C6,C7)

  implicit none

  ! INPUT/OUTPUT VARIABLES
  double precision, intent(in):: nalphamax,ti,ndeut,ntrit,rpa

  ! LOCAL VARIABLES
  double precision:: vrho,se,sx,snormtlimin,snormtlimout,position_source

  ! RELATIVE LOCAL ALPHA PARTICLE RATE (N*RPA)/(NMAX*R0)
  sx = ti/(1 - ( ti * (c2 + ti * (c4 + ti*c6))) /(1 + ti * (c3 + ti * (c5 + ti*c7))) )
  se = (bg**2/(4*sx))**(1./3.)
  position_source = ndeut*ntrit*rpa*c1*sx*sqrt(se/(mrc2*ti**3))*exp(-3.*se)*1.e-6/nalphamax

  ! SECURITY
  if(.not.(position_source.gt.-9999..and.position_source.lt.9999.)) then
     if(ti.ne.0.) write(*,*) 'PROBLEM IN THE GENERATION OF THE SOURCE FOR POSITION'
     position_source=0.
  endif

end function position_source
