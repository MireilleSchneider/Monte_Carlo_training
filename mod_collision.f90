module mod_collision
  implicit none
  ! -----------------------------------------------------------------
  ! STRUCTURE FOR THE LOCAL COLLISION COEFFICIENTS OF THE PARTICLE
  ! -----------------------------------------------------------------
  ! THEY ARE NOT EXPLICTLY DETAILED BECAUSE THIS IS BEYOND THE SCOPE
  ! OF THE TRANING
  ! -----------------------------------------------------------------

  type coll

     ! COEFFICIENTS FOR SLOWING DOWN AND ENERGY DIFFUSION
     double precision:: cefix,ceran   ! TOTAL
     double precision:: cefixe,cerane ! TO ELECTRONS
     double precision:: cefixi,cerani ! TO IONS

     ! COEFFICIENTS FOR PITCH ANGLE SCATTERING
     double precision:: cxfix,cxran

     ! SLOWING DOWN TIME,  CRITICAL VELOCITY, COULOMB LOGARITHMS
     double precision:: tslow,vcri,logle,logli

     ! STIX COEFFICIENTS (A, B AND DERIVATIVE)
     double precision:: acoeftot,bcoeftot,dbvcoeftot ! TOTAL
     double precision:: acoefe,bcoefe,dbvcoefe       ! TO ELECTRONS
     double precision:: acoefi,bcoefi,dbvcoefi       ! TO IONS

     ! ACTUAL POWER TRABSFER TO ELECTRONS AND IONS
     double precision:: elde,ionde

  end type coll

end module mod_collision
