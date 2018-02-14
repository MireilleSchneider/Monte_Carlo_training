module mod_source

  implicit none

  double precision:: bg,mrc2
  double precision:: c1,c2,c3,c4,c5,c6,c7

  ! COEFFICIENTS FOR CROSS-SECTION CALCULATION FOR FUSION-BORN ALPHA PARTICLES

  parameter(bg   = 34.3827) ! SQRT(KEV)
  parameter(mrc2 = 1124656) ! REDUCED MASS OF PARTICLES = M1*M2/(M1+M2)*C**2/QEL*1E3 (KEV)

  parameter(c1 =  1.17302e-9)
  parameter(c2 =  1.51361e-2)
  parameter(c3 =  7.51886e-2)
  parameter(c4 =  4.60643e-3)
  parameter(c5 =  1.35000e-2)
  parameter(c6 = -1.06750e-4)
  parameter(c7 =  1.36600e-5)

  type alsrc
     double precision:: nalphamax ! MAXIMUM NUMBER OF ALPHA PARTICLES
     double precision:: weight    ! WEIGHT OF MONTE CARLO PARTICLES
  end type alsrc

end module mod_source

