module mod_randomsp
! -----------------------------
! GENERATION OF RANDOM NUMBERS
! -----------------------------
  implicit none  
  integer, parameter:: seed=985456376

contains

  function ran2(irand)

    integer:: irand,pid,i
    integer, allocatable:: iseed(:)

    double precision:: ran2,y

    allocate(iseed(16))

    if(irand.ne.0) then
       iseed(1) = irand
       do i=1,16
          iseed(i) = i*irand-1
       enddo
       call random_seed(put=iseed)
    endif

    call random_number(y)

    ran2 = y

    return
  end function ran2

end module mod_randomsp
