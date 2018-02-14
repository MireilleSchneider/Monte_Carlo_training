subroutine fusion_reaction(scal,inp1d,inp2d,src)
  ! -----------------------------------------------------------------------
  ! PURPOSE:
  ! TO CALCULATE THE ALPHA PARTICLE RATE AND IT MAXIMUM NUMBER
  ! ACCORDING TO D-T CROSS SECTIONS
  ! ----------------------------------------------------
  ! INPUT:
  ! - SCAL  = STRUCTORE FOR INPUT SCALAR VARIABLES
  ! - INP1D = STRUCTURE FOR INPUT 1D-PROFILES
  ! - INP2D = STRUCTURE FOR INPUT 2D-GRIDS
  ! ----------------------------------------------------------------------- 
  ! OUTPUT:
  ! - SRC   = ALPHA PARTICLE SOURCE STRUCTURE
  ! -----------------------------------------------------------------------
  ! REFERENCE: 
  ! H.-S. BOSCH AND G.M. HALE 1992 NUCL. FUSION 32 611
  ! "IMPROVED FORMULAS FOR FUSION CROSS-SECTIONS AND THERMAL REACTIVITIES"
  ! -----------------------------------------------------------------------

  use mod_input_scalars ! INPUT SCALAR VARIABLES
  use mod_input_1d      ! INPUT 1D VARIABLES
  use mod_input_2d      ! INPUT 2D VARIABLES
  use mod_source        ! SOURCE AND CROSS-SECTION COEFTS (BG,MRC2,C1,C2,C3,C4,C5,C6,C7)

  implicit none

  ! INPUT/OUTPUT VARIABLES
  type(input_1d),      intent(in):: inp1d
  type(input_2d),      intent(in):: inp2d
  type(input_scalars), intent(in):: scal
  type(alsrc),         intent(out):: src
  
  ! LOCAL VARIABLES
  integer:: k,i,iescape
  double precision:: rhonorm,ntrit,ndeut,nalbox,sx,se
  double precision:: ti,ntot(scal%n_ions),ion_rate
  double precision:: rhomin,rhomax

  ! INITIALIZATIONS
  src%nalphamax = 0.
  iescape = 0
  rhomin = 0.
  rhomax = 1.

  ion_rate = 0.
  do k=1,scal%nin1d

     ! LOCAL ION TEMPERATURE AND DENSITY
     rhonorm = inp1d%rho_in(k)/inp1d%rho_in(scal%nin1d)
     call interp1d(rhomin,rhomax,inp1d%ti(:,1),scal%nin1d,rhonorm,ti,iescape)  ! TI
     do i=1,scal%n_ions
        call interp1d(rhomin,rhomax,inp1d%ntot(:,i),scal%nin1d,rhonorm,ntot(i),iescape) ! NTOT
     enddo

     ! DEUTERIUM AND TRITIUM DENSITIES
     ntrit=-1.
     do i=1,scal%n_ions
        if(scal%a_ions(i).eq.2.and.scal%z_ions(i).eq.1) ndeut = ntot(i)
        if(scal%a_ions(i).eq.3.and.scal%z_ions(i).eq.1) ntrit = ntot(i)
     enddo

     ! LOCAL ALPHA PARTICLE VOLUME RATE (S-1.M-3)
     sx = ti/(1 - (ti*(c2 + ti * (c4 + ti*c6)))/(1 + ti * (c3 + ti * (c5 + ti*c7))) )
     se = (bg**2/(4*sx))**(1./3.)
     nalbox = ndeut*ntrit*c1*sx*sqrt(se/(mrc2*ti**3))*exp(-3.*se)*1.e-6

     ! LOCAL ALPHA PARTICLE RATE (S-1)
     ion_rate = ion_rate + nalbox*inp1d%vsurf_in(k) ! TOTAL # NEW ALPHAS /S

     ! MAXIMUM NUMBER OF CREATED PARTICLES (S-1.M-2)
     if(nalbox*scal%r0.gt.src%nalphamax) src%nalphamax = nalbox*scal%r0

  enddo

  src%weight = 1.*ion_rate*scal%bigwidth/scal%nperstep

end subroutine fusion_reaction
