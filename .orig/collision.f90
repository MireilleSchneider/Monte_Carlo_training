subroutine collision(scal,inp1d,inp2d,bigstep,particle,out1d)
  ! ------------------------------------------------------------------
  ! PURPOSE: CALCULATE THE ENERGY SLOWING-DOWN, THE ENERGY DIFFUSION
  !          AND THE PITCH ANGLE SCATTERING DUE TO COLLISIONS WITH 
  !          ELECTRONS AND IONS
  ! ------------------------------------------------------------------
  use mod_constants     ! PHYSICS CONSTANTS
  use mod_randomsp      ! SEED FOR RANDOM NUMBERS
  use mod_particle      ! PARTICLE STRUCTURE
  use mod_input_scalars ! INPUT SCALAR VARIABLES
  use mod_input_1d      ! INPUT 1D VARIABLES
  use mod_input_2d      ! INPUT 2D GRIDS
  use mod_output_1d     ! OUTPUT 1D VARIABLES
  use mod_collision     ! COLLISION STRUCTURE
  implicit none

  ! INPUT/OUTPUT VARIABLES
  integer:: idxrhos,bigstep
  type(input_scalars), intent(in):: scal
  type(input_1d),      intent(in):: inp1d
  type(input_2d),      intent(in):: inp2d
  type(output_1d),     intent(inout):: out1d
  type(partic),        intent(inout):: particle

  ! LOCAL VARIABLES
  type(coll):: col
  integer:: i,idum
  double precision:: vth,ai(scal%n_ions),zi(scal%n_ions),ne,ni(scal%n_ions),te,ti
  double precision:: cfi(scal%n_ions),cfe,lfi(scal%n_ions),lfe,xi(scal%n_ions)
  double precision:: xe,xi2(scal%n_ions),xe2,erfunce,erfuncde,erfuncdde
  double precision:: erfunci(scal%n_ions),erfuncdi(scal%n_ions),erfuncddi(scal%n_ions)
  double precision:: gchani(scal%n_ions),gchandi(scal%n_ions),gchane,gchande
  double precision:: derf,pdfrand,gamstix,xipart,xipartnew,vparanew
  double precision:: estixal,estixbe,estixga,vstixbe,vstixga
  double precision:: a_ion,z_ion,cfaste,cfasti,dt,rho,rhonorm
  double precision:: kin_energy,delta_kin_energy,dxipart,zeff,zbar,zkt

  idum = 0

  ! ----------------------------------------------------------
  ! LOCAL TOROIDAL FLUX COORDINATE AND PLASMA CHARACTERISTICS
  ! ----------------------------------------------------------
  call interp2d(scal%rmin,scal%rmax,scal%zmin,scal%zmax &
       ,inp2d%rho2d,shape(inp2d%rho2d),particle%rpa,particle%zpa,rho,idum) ! RHO
  rhonorm = rho/inp1d%rho_in(scal%nin1d)
  ai = scal%a_ions
  zi = scal%z_ions
  dt = scal%dt
  call interp1d(0.d0,1.d0,inp1d%te,scal%nin1d,rhonorm,te,idum) ! TE
  call interp1d(0.d0,1.d0,inp1d%ti,scal%nin1d,rhonorm,ti,idum) ! TI
  call interp1d(0.d0,1.d0,inp1d%ne,scal%nin1d,rhonorm,ne,idum) ! NE
  do i=1,scal%n_ions
     call interp1d(0.d0,1.d0,inp1d%ntot(:,i),scal%nin1d,rhonorm,ni(i),idum) ! NTOT
  enddo

  ! ---------------------------------------------------
  ! MASS, CHARGE AND KINETIC ENEGY OF THE FOLLOWED ION
  ! ---------------------------------------------------
  a_ion = scal%a_number
  z_ion = scal%z_number
  kin_energy = 0.5*a_ion*pmass*particle%vel**2

  ! -------------------------------------------------------------------------------------------------
  ! CARACTERISTIC ENERGIES AND VELOCITIES FOR SLOWING-DOWN, ENERGY DIFFUSION, PITCH ANGLE SCATTERING
  ! -------------------------------------------------------------------------------------------------
  estixal  = 14.8 * te          * a_ion         *zbar(ai,zi,ne,ni,scal%n_ions)**(2./3.)     ! KEV
  estixbe  = 14.8 * te**(1./3.) * a_ion         *zkt(ai,zi,ne,ti,ni,scal%n_ions)**(2./3.)      ! KEV
  estixga  = 14.8 * te          * a_ion**(1./3.)*(2*zeff(zi,ne,ni,scal%n_ions))**(2./3.) ! KEV
  col%vcri = sqrt(2000*qel*estixal/(a_ion*pmass))
  vstixbe  = sqrt(2000*qel*estixbe/(a_ion*pmass))
  vstixga  = sqrt(2000*qel*estixga/(a_ion*pmass))

  ! ------------------
  ! COULOMB LOGARITHM
  ! ------------------
  col%logle = 15.2-0.5*log(ne/1e20)+log(te)+log(z_ion)
  col%logli = 17.3-0.5*log(ne/1e20)+1.5*log(ti)+log(z_ion)

  ! --------------------------------------------------------------------------------
  ! CORRECTION OF THE ANALYTIC CRITICAL VELOCITY ACCORDING TO THE COULOMB LOGARITHM
  ! --------------------------------------------------------------------------------
  col%vcri = col%vcri*(col%logli/col%logle)**(1./3)

  ! ------------------------
  ! LOCAL SLOWING-DOWN TIME
  ! ------------------------
  col%tslow = 6.27e8*a_ion*((te*1000.)**1.5)/((ne*1e-6)*z_ion**2*col%logle)

  ! -----------------------------------------------------------
  ! CALCULATE CF AND LF COEFFICIENTS CONFORMING TO STIX' PAPER
  ! -----------------------------------------------------------
  cfasti = 1./(16*pi**2*eps0**2)*8*pi*z_ion**2*qel**4*col%logli/(a_ion*pmass)**2
  do i=1,scal%n_ions
     cfi(i) = cfasti*ni(i)*zi(i)**2
     lfi(i) = sqrt(pmass*ai(i)/(2*ti*qel*1000))         
  enddo
  cfaste = 1./(16*pi**2*eps0**2)*8*pi*z_ion**2*qel**4*col%logle/(a_ion*pmass)**2
  cfe = cfaste*ne
  lfe = sqrt(emass/(2*te*qel*1000))

  ! --------------------------------------------------------------------------
  ! CHANDRASEKHAR FUNCTIONS: ERROR FUNCTION PHI(X), G(X) AND THEIR DERIVATIVES
  ! --------------------------------------------------------------------------
  do i=1,scal%n_ions
     xi(i)        = lfi(i)*particle%vel
     xi2(i)       = xi(i)*xi(i)
     erfunci(i)   = derf(xi(i))                                 ! PHI(x)
     erfuncdi(i)  = 2/sqrt(pi)*exp(-xi2(i))                     ! PHI'(x)
     erfuncddi(i) = -4*xi(i)/sqrt(pi)*exp(-xi2(i))              ! PHI''(x)
     gchani(i)    = (erfunci(i)-xi(i)*erfuncdi(i))/(2*xi2(i))   ! G(x)
     gchandi(i)   = (-2*erfunci(i)+2*xi(i)*erfuncdi(i)-xi2(i)*erfuncddi(i))/(2*xi(i)**3) ! G'(x)
  enddo
  xe        = lfe*particle%vel
  xe2       = xe*xe
  erfunce   = derf(xe)                                           ! PHI(x)
  erfuncde  = 2/sqrt(pi)*exp(-xe2)                               ! PHI'(x)
  erfuncdde = -4*xe/sqrt(pi)*exp(-xe2)                           ! PHI''(x)
  gchane    = (erfunce-xe*erfuncde)/(2*xe2)                      ! G(x)
  gchande   = (-2*erfunce+2*xe*erfuncde-xe2*erfuncdde)/(2*xe**3) ! G'(x)

  ! ------------------------------------
  ! OPERATOR FOR PITCH ANGLE SCATTERING
  ! ------------------------------------
  vparanew = particle%vpara

  if(scal%xiflag.eq.1) then ! FLAG TELLING IF THIS PROCESS MUST BE CONSIDERED

     ! GAMSTIX: GAMMA ACCORDING TO STIX' PAPER
     gamstix = 0.
     do i=1,scal%n_ions
        gamstix = gamstix + cfi(i)/particle%vel*(erfunci(i)-gchani(i))
     enddo
     gamstix = gamstix + cfe/particle%vel*(erfunce-gchane)

     vth = sqrt(2*0.5*1000*qel/(a_ion*pmass))

     ! COEFFICIENTS OF THE PITCH ANGLE SCATTERING OPERATOR
     xipart = particle%vpara/particle%vel
     col%cxfix  = -gamstix/(2*(particle%vel**2+1.*vth**2))*xipart
     col%cxran  =  gamstix/(2*(particle%vel**2+1.*vth**2))*(1-xipart**2)
     if(col%cxran.gt.0.) then
        col%cxran = sqrt(col%cxran)
     else
        col%cxran = 0.
     endif

     ! NEW PITCH ANGLE AND PARALLEL VELOCITY AFTER PITCH ANGLE SCATTERING
     ! TO_BE_ADDED_HERE
     dxipart = 0.
     ! dxipart = ....

     if(abs(xipart+dxipart).gt.1) dxipart = 2*xipart/abs(xipart) - 2*xipart - dxipart
     xipartnew = xipart + dxipart

  else

     xipartnew = particle%vpara/particle%vel

  endif

  ! ------------------------------------------------------
  ! OPERATOR FOR ENERGY SLOWING-DOWN AND ENERGY DIFFUSION
  ! ------------------------------------------------------
  col%elde  = 0.
  col%ionde = 0.

  if(scal%colflag.eq.1) then ! FLAG TELLING IF THIS PROCESS MUST BE CONSIDERED

     ! A AND B COEFFICIENTS (CF. STIX' PAPER FOR ALPHA, BETA, GAMMA DEFINITIONS)
     col%acoefi   = 0.   ! ACOEFI   = A = V*ALPHA + BETA/2
     col%bcoefi   = 0.   ! BCOEFI   = B = V**2*BETA
     col%dbvcoefi = 0.   ! DBVCOEFI = 1/(V**2)*DDV(V**2*BCOEFI)
     do i=1,scal%n_ions
        col%acoefi   = col%acoefi - cfi(i)*lfi(i)**2*(1+a_ion/ai(i))*gchani(i)*particle%vel &
             + cfi(i)*erfunci(i)/(2*particle%vel)
        col%bcoefi   = col%bcoefi + particle%vel*cfi(i)*gchani(i)
        col%dbvcoefi = 0. !col%dbvcoefi + cfi(i)*erfunci(i)/(2*sqrt(2*Ec/(a_ion*pmass)))
     enddo
     col%acoefe   = - cfe*lfe**2*(1+a_ion*pmass/emass)*gchane*particle%vel + cfe*erfunce/(2*particle%vel)
     col%bcoefe   = particle%vel*cfe*gchane
     col%dbvcoefe = 0. ! cfe*erfunce/(2*sqrt(2*Ec/(a_ion*pmass)))

     ! ENERGY DIFFUSION
     if(scal%ediflag.ne.1) then
        ! A = V*ALPHA (BETA PUT TO ZERO)
        col%acoefi = 0.
        do i=1,scal%n_ions
           col%acoefi = col%acoefi - cfi(i)*lfi(i)**2*(1+a_ion/ai(i))*gchani(i)*particle%vel &
                + cfi(i)*(erfunci(i)-gchani(i))/(2*particle%vel)
        enddo
        col%acoefe   = - cfe*lfe**2*(1+a_ion*pmass/emass)*gchane*particle%vel + cfe*(erfunce-gchane)/(2*particle%vel)
        ! B = 0 (BETA PUT TO ZERO)
        col%bcoefi   = 0.
        col%bcoefe   = 0.
        col%dbvcoefi = 0.
        col%dbvcoefe = 0.
     endif

     ! SUM ELECTRONS + IONS
     col%acoeftot   = col%acoefi+col%acoefe     ! V*ALPHA + BETA/2 
     col%bcoeftot   = col%bcoefi+col%bcoefe     ! V**2*BETA
     col%dbvcoeftot = col%dbvcoefi+col%dbvcoefe ! 1/(V**2)*DDV(V**2*BCOEFI)

     ! ------------------------------------------------
     ! COEFFICIENT FOR DETERMINISTIC TERM (DRIFT TERM)
     ! CEFIX = M*(V*ALPHA+BETA/2) = M*A
     ! ------------------------------------------------
     col%cefixe = a_ion*pmass*col%acoefe
     col%cefixi = a_ion*pmass*col%acoefi
     col%cefix  = col%cefixe + col%cefixi
     
     ! -----------------------------------------------------
     ! COEFFICIENT FOR STOCHASTIC TERM (PROBABILISTIC TERM)
     ! CERAN = M*SQRT(V**2*BETA) = M*SQRT(B)
     ! -----------------------------------------------------
     if(col%bcoefe.gt.0.) then
        col%cerane = a_ion*pmass*sqrt(col%bcoefe)
     else
        col%cerane = 0.
     endif
     if(col%bcoefi.gt.0.) then
        col%cerani = a_ion*pmass*sqrt(col%bcoefi)
     else
        col%cerani = 0.
     endif
     col%ceran = col%cerane + col%cerani

     ! -------------------------------------------------
     ! ENERGY TRANSFERED TO ELECTRONS AND IONS (JOULES)
     ! -------------------------------------------------

     ! TO_BE_ADDED_HERE
     col%elde = 0.
     col%ionde = 0.
     ! col%elde    = ...
     ! col%ionde   = ....

  endif

  ! --------------------------------------------------
  ! NEW ENERGY AFTER COLLISION AND BACK TO VELOCITIES
  ! --------------------------------------------------
  if (kin_energy + col%elde  .lt. 0.)  col%elde  = -2*kin_energy - col%elde
  if (kin_energy + col%ionde .lt. 0.)  col%ionde = -2*kin_energy - col%ionde
  delta_kin_energy = col%ionde + col%elde
  kin_energy = kin_energy + delta_kin_energy

  particle%vel   = sqrt(2*kin_energy/(a_ion*pmass))
  particle%vpara = xipartnew*particle%vel

  ! -------------------------
  ! PREVENT VPARA > VELOCITY
  ! -------------------------
  if(abs(particle%vpara).gt.particle%vel) then
     particle%vpara = particle%vel*particle%vpara/abs(particle%vpara)
  endif

  if(scal%one_orbit.eq.1) then
     out1d%elpower(bigstep)  = out1d%elpower(bigstep)  + col%cefixe*scal%dt
     out1d%ionpower(bigstep) = out1d%ionpower(bigstep) + col%cefixi*scal%dt
  endif

  return
end subroutine collision

! --------------------------------------------------------------------------------------------------
! --------------------------------------------------------------------------------------------------
! --------------------------------------------------------------------------------------------------

function zeff(zi,ne,ntot,nimp)
  ! ----------------------------------------------------------------
  ! PURPOSE: RETURN ZEFF = (SUMJ(NJ*ZJ**2))/NE
  ! ----------------------------------------------------------------
  ! INPUTS:
  ! ZI = ARRAY OF CHARGE NUMBER OF ALL SPECIES
  ! NE = DENSITY OF ELECTRONS
  ! NTOT = DENSITY OF ALL SPECIES
  ! NIMP = NUMBER OF SPECIES
  ! ----------------------------------------------------------------
  implicit none
  integer:: nimp,i
  double precision:: zeff
  double precision:: zi(nimp),ne,ntot(nimp)

  zeff = 0.
  if(ne.gt.0.) then ! AVOID NAN AT THE EDGE OF THE PLASMA
     do i=1,nimp
        zeff= ntot(i)*zi(i)**2
     enddo
     zeff = zeff/ne
  endif

  return
end function zeff

! --------------------------------------------------------------------------------------------------
! --------------------------------------------------------------------------------------------------
! --------------------------------------------------------------------------------------------------

function zkt(ai,zi,ne,ti,ntot,nimp)
  ! ----------------------------------------------------------------
  ! PURPOSE: RETURN ZKT = (SUMJ(NJ*ZJ**2*TJ)/AJ)/NE
  ! ----------------------------------------------------------------
  ! INPUT:
  ! AI   = ARRAY OF ATOMIC NUMBER OF ALL SPECIES
  ! ZI   = ARRAY OF CHARGE NUMBER OF ALL SPECIES
  ! NE   = DENSITY OF ELECTRONS
  ! TI   = TEMPERATURE OF IONS (SUPPOSED THE SAME FOR ALL SPECIES)
  ! NTOT = DENSITY OF ALL SPECIES
  ! NIMP = NUMBER OF SPECIES
  ! ----------------------------------------------------------------
  implicit none
  integer:: nimp,i
  double precision:: zkt
  double precision:: ai(nimp),zi(nimp),ne,ntot(nimp),ti

  zkt = 0.
  if(ne.gt.0.) then ! AVOID NAN AT THE EDGE OF THE PLASMA
     do i=1,nimp
        zkt = zkt + ntot(i)*zi(i)**2*ti/ai(i)
     enddo
     zkt = zkt/ne
  endif

  return
end function zkt

! --------------------------------------------------------------------------------------------------
! --------------------------------------------------------------------------------------------------
! --------------------------------------------------------------------------------------------------

function zbar(ai,zi,ne,ntot,nimp)
  ! ----------------------------------------------------------------
  ! PURPOSE: RETURN ZBAR = (SUMJ(NJ*ZJ**2)/AJ)/NE
  ! ----------------------------------------------------------------
  ! INPUT:
  ! AI   = ARRAY OF ATOMIC NUMBER OF ALL SPECIES
  ! ZI   = ARRAY OF CHARGE NUMBER OF ALL SPECIES
  ! NE   = DENSITY OF ELECTRONS
  ! NTOT = DENSITY OF ALL SPECIES
  ! NIMP = NUMBER OF SPECIES
  ! ----------------------------------------------------------------
  implicit none
  integer:: nimp,i
  double precision:: zbar
  double precision:: ai(nimp),zi(nimp),ne,ntot(nimp)

  zbar = 0.
  if(ne.gt.0.) then ! AVOID NAN AT THE EDGE OF THE PLASMA
     do i=1,nimp
        zbar = zbar + ntot(i)*zi(i)**2/ai(i)
     enddo
     zbar = zbar/ne
  endif

  return
end function zbar
