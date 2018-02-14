! ---------------------------------
! PURPOSE: INPUT/OUTPUT MANAGEMENT
! ---------------------------------
module mod_io_management
  implicit none
  integer, private:: unite
  character(len=25), private:: filename
  private :: open_file_in, open_file_out, recup_unit

  ! -----------------------------------------------------------
  ! GENERIC FUNCTION TO READ INPUT VARIABLES FROM BINARY FILES
  ! -----------------------------------------------------------
  interface readvar
    module procedure readvar_0d, &
                     readvar_1d, &
                     readvar_2d, &
                     readvar_3d, &
                     readvar_4d, &
                     readvar_5d, &
                     readvar_6d, &
                     readvar_7d, &
                     readchar
  end interface readvar

  ! -----------------------------------------------------------
  ! GENERIC FUNCTION TO WRITE OUTPUT VARIABLES TO BINARY FILES
  ! -----------------------------------------------------------
  interface writevar
     module procedure writevar_0d, &
                      writevar_1d, &
                      writevar_2d, &
                      writevar_3d, &
                      writevar_4d, &
                      writevar_5d, &
                      writevar_6d, &
                      writevar_7d
  end interface writevar

contains

  ! -----------------------------------------------------------------------------------------
  ! -----------------------------------------------------------------------------------------
  ! -----------------------------------------------------------------------------------------

  function recup_unit()
    ! -------------------------------------------------------------------
    ! PURPOSE: INITIALIZE AND INCREMENT THE UNIT NUMBER FOR FILE OPENING
    ! -------------------------------------------------------------------
    integer:: recup_unit
    logical:: value
    
    recup_unit=0
    do
       inquire(unit=recup_unit,opened=value )
       if(.not.value) return
       recup_unit = recup_unit + 1
    enddo
  end function recup_unit

  ! -----------------------------------------------------------------------------------------
  ! -----------------------------------------------------------------------------------------
  ! -----------------------------------------------------------------------------------------

  subroutine open_file_in(filename,lengthin)
    ! -------------------------------------------
    ! PURPOSE: OPEN ASCII FILES FOR DATA READING
    ! -------------------------------------------
    character(len=25), intent(in):: filename
    integer, intent(in) :: lengthin
    integer:: rc
  
    unite = recup_unit()

    open( unit=unite,file=trim(filename),form='formatted',status='old',iostat=rc)

  end subroutine open_file_in

  ! -----------------------------------------------------------------------------------------
  ! -----------------------------------------------------------------------------------------
  ! -----------------------------------------------------------------------------------------

  subroutine open_file_out(filename,lengthout)
    ! -------------------------------------------
    ! PURPOSE: OPEN ASCII FILES FOR DATA WRITING
    ! -------------------------------------------
    character(len=25), intent(in):: filename
    integer, intent(in) :: lengthout
    integer:: rc
  
    unite = recup_unit()

    open( unit=unite,file=trim(filename),form='formatted',status='replace',iostat=rc)

  end subroutine open_file_out

  ! -----------------------------------------------------------------------------------------
  ! -----------------------------------------------------------------------------------------
  ! -----------------------------------------------------------------------------------------
  
  function readvar_0d(filename,mold)
    ! ------------------------------------------------------------
    ! PURPOSE: IMPORT SCALAR VARIABLES COMING FROM BINARY FILES
    ! ------------------------------------------------------------
    ! INPUT:
    !   - FILENAME = NAME OF THE INPUT FILE
    !   - MOLD     = VARIABLE TO FILL IN
    ! RETURN VALUE:
    !   - INPUT ARRAY
    ! ------------------------------------------------------------
    implicit none
    character(len=25), intent(in):: filename
    double precision, intent(in) :: mold
    double precision  :: readvar_0d
    integer:: lengthin
  
    call open_file_in(filename,lengthin)
    read(unit=unite,fmt=*) readvar_0d

    close(unite)
    return
  
  300 print*,'PROBLEM IN READING INPUT FILE ', filename
    stop

  end function readvar_0d

  ! -----------------------------------------------------------------------------------------
  ! -----------------------------------------------------------------------------------------
  ! -----------------------------------------------------------------------------------------
  
  function readvar_1d(filename,mold)
    ! ------------------------------------------------------------
    ! PURPOSE: IMPORT 1D VARIABLES COMING FROM BINARY FILES
    ! ------------------------------------------------------------
    ! INPUT:  
    !   - FILENAME = NAME OF THE INPUT FILE
    !   - MOLD     = VARIABLE TO FILL IN
    ! RETURN VALUE:
    !   - INPUT ARRAY
    ! ------------------------------------------------------------
    implicit none
    character(len=25), intent(in):: filename
    double precision, dimension(:), intent(in) :: mold
    double precision, dimension(size(mold,1))  :: readvar_1d
    integer:: lengthin

    call open_file_in(filename,lengthin)
    read(unit=unite,fmt=*) readvar_1d
    close(unite)
    return
  
  300 print*,'PROBLEM IN READING INPUT FILE ', filename
    stop

  end function readvar_1d

  ! -----------------------------------------------------------------------------------------
  ! -----------------------------------------------------------------------------------------
  ! -----------------------------------------------------------------------------------------
  
  function readvar_2d(filename,mold)
    ! ------------------------------------------------------------
    ! PURPOSE: IMPORT 2D VARIABLES COMING FROM BINARY FILES
    ! ------------------------------------------------------------
    ! INPUT:  
    !   - FILENAME = NAME OF THE INPUT FILE
    !   - MOLD     = VARIABLE TO FILL IN
    ! RETURN VALUE:
    !   - INPUT ARRAY
    ! ------------------------------------------------------------
    implicit none
    character(len=25), intent(in):: filename
    double precision, dimension(:,:), intent(in) :: mold
    double precision, dimension(size(mold,1),size(mold,2)):: readvar_2d
    integer:: lengthin
  
    call open_file_in(filename,lengthin)
    read(unit=unite,fmt=*) readvar_2d(1:size(mold,1),1:size(mold,2))
    close(unite)
    return
  
  300 print*,'PROBLEM IN READING INPUT FILE ', filename
    stop

  end function readvar_2d

  ! -----------------------------------------------------------------------------------------
  ! -----------------------------------------------------------------------------------------
  ! -----------------------------------------------------------------------------------------
  
  function readvar_3d(filename,mold)
    ! ------------------------------------------------------------
    ! PURPOSE: IMPORT 3D VARIABLES COMING FROM BINARY FILES
    ! ------------------------------------------------------------
    ! INPUT:  
    !   - FILENAME = NAME OF THE INPUT FILE
    !   - MOLD     = VARIABLE TO FILL IN
    ! RETURN VALUE:
    !   - INPUT ARRAY
    ! ------------------------------------------------------------
    implicit none
    character(len=25), intent(in):: filename
    double precision, dimension(:,:,:), intent(in) :: mold
    double precision, dimension(size(mold,1), &
                                size(mold,2), &
                                size(mold,3))  :: readvar_3d
    integer:: lengthin
  
    call open_file_in(filename,lengthin)
    read(unit=unite,fmt=*) readvar_3d(1:size(mold,1),1:size(mold,2),1:size(mold,3))
    close(unite)
    return
  
  300 print*,'PROBLEM IN READING INPUT FILE ', filename
    stop

  end function readvar_3d
  
  ! -----------------------------------------------------------------------------------------
  ! -----------------------------------------------------------------------------------------
  ! -----------------------------------------------------------------------------------------

  function readvar_4d(filename,mold)
    ! ------------------------------------------------------------
    ! PURPOSE: IMPORT 4D VARIABLES COMING FROM BINARY FILES
    ! ------------------------------------------------------------
    ! INPUT:  
    !   - FILENAME = NAME OF THE INPUT FILE
    !   - MOLD     = VARIABLE TO FILL IN
    ! RETURN VALUE:
    !   - INPUT ARRAY
    ! ------------------------------------------------------------
    implicit none
    character(len=25), intent(in):: filename
    double precision, dimension(:,:,:,:), intent(in) :: mold
    double precision, dimension(size(mold,1), &
                                size(mold,2), &
                                size(mold,3), &
                                size(mold,4))  :: readvar_4d
    integer:: lengthin
  
    call open_file_in(filename,lengthin)
    read(unit=unite,fmt=*) readvar_4d(1:size(mold,1),1:size(mold,2),1:size(mold,3),1:size(mold,4))
    close(unite)
    return
  
  300 print*,'PROBLEM IN READING INPUT FILE ', filename
    stop

  end function readvar_4d
  
  ! -----------------------------------------------------------------------------------------
  ! -----------------------------------------------------------------------------------------
  ! -----------------------------------------------------------------------------------------

  function readvar_5d(filename,mold)
    ! ------------------------------------------------------------
    ! PURPOSE: IMPORT 5D VARIABLES COMING FROM BINARY FILES
    ! ------------------------------------------------------------
    ! INPUT:  
    !   - FILENAME = NAME OF THE INPUT FILE
    !   - MOLD     = VARIABLE TO FILL IN
    ! RETURN VALUE:
    !   - INPUT ARRAY
    ! ------------------------------------------------------------
    implicit none
    character(len=25), intent(in):: filename
    double precision, dimension(:,:,:,:,:), intent(in) :: mold
    double precision, dimension(size(mold,1), &
                                size(mold,2), &
                                size(mold,3), &
                                size(mold,4), &
                                size(mold,5))  :: readvar_5d
    integer:: lengthin
  
    call open_file_in(filename,lengthin)
    read(unit=unite,fmt=*) readvar_5d(1:size(mold,1),1:size(mold,2),1:size(mold,3),1:size(mold,4) &
         ,1:size(mold,5))
    close(unite)
    return
  
  300 print*,'PROBLEM IN READING INPUT FILE ', filename
    stop

  end function readvar_5d
  
  ! -----------------------------------------------------------------------------------------
  ! -----------------------------------------------------------------------------------------
  ! -----------------------------------------------------------------------------------------

  function readvar_6d(filename,mold)
    ! ------------------------------------------------------------
    ! PURPOSE: IMPORT 6D VARIABLES COMING FROM BINARY FILES
    ! ------------------------------------------------------------
    ! INPUT:  
    !   - FILENAME = NAME OF THE INPUT FILE
    !   - MOLD     = VARIABLE TO FILL IN
    ! RETURN VALUE:
    !   - INPUT ARRAY
    ! ------------------------------------------------------------
    implicit none
    character(len=25), intent(in):: filename
    double precision, dimension(:,:,:,:,:,:), intent(in) :: mold
    double precision, dimension(size(mold,1), &
                                size(mold,2), &
                                size(mold,3), &
                                size(mold,4), &
                                size(mold,5), &
                                size(mold,6))  :: readvar_6d
    integer:: lengthin
  
    call open_file_in(filename,lengthin)
    read(unit=unite,fmt=*) readvar_6d(1:size(mold,1),1:size(mold,2),1:size(mold,3),1:size(mold,4) &
         ,1:size(mold,5),1:size(mold,6))
    close(unite)
    return
  
  300 print*,'PROBLEM IN READING INPUT FILE ', filename
    stop

  end function readvar_6d
  
  ! -----------------------------------------------------------------------------------------
  ! -----------------------------------------------------------------------------------------
  ! -----------------------------------------------------------------------------------------

  function readvar_7d(filename,mold)
    ! ------------------------------------------------------------
    ! PURPOSE: IMPORT 7D VARIABLES COMING FROM BINARY FILES
    ! ------------------------------------------------------------
    ! INPUT:  
    !   - FILENAME = NAME OF THE INPUT FILE
    !   - MOLD     = VARIABLE TO FILL IN
    ! RETURN VALUE:
    !   - INPUT ARRAY
    ! ------------------------------------------------------------
    implicit none
    character(len=25), intent(in):: filename
    double precision, dimension(:,:,:,:,:,:,:), intent(in) :: mold
    double precision, dimension(size(mold,1), &
                                size(mold,2), &
                                size(mold,3), &
                                size(mold,4), &
                                size(mold,5), &
                                size(mold,6), &
                                size(mold,7))  :: readvar_7d
    integer:: lengthin
  
    call open_file_in(filename,lengthin)
    read(unit=unite,fmt=*) readvar_7d(1:size(mold,1),1:size(mold,2),1:size(mold,3),1:size(mold,4) &
         ,1:size(mold,5),1:size(mold,6),1:size(mold,7))
    close(unite)
    return
  
  300 print*,'PROBLEM IN READING INPUT FILE ', filename
    stop

  end function readvar_7d
  
  ! -----------------------------------------------------------------------------------------
  ! -----------------------------------------------------------------------------------------
  ! -----------------------------------------------------------------------------------------

  function readchar(filename,mold)
    ! ------------------------------------------------------------
    ! PURPOSE: IMPORT CHARACTER VARIABLES COMING FROM BINARY FILES
    ! ------------------------------------------------------------
    ! INPUT:  
    !   - FILENAME = NAME OF THE INPUT FILE
    !   - MOLD     = VARIABLE TO FILL IN
    ! RETURN VALUE:
    !   - INPUT ARRAY
    ! ------------------------------------------------------------
    implicit none
    character(len=25), intent(in):: filename
    character, dimension(:), intent(in) :: mold
    character, dimension(size(mold,1)) :: readchar
    integer:: lengthin

    call open_file_in(filename,lengthin)
    read(unit=unite,fmt=*) readchar
    close(unite)
    return

300 print*,'PROBLEM IN READING INPUT FILE ', filename
    stop

  end function readchar

  ! -----------------------------------------------------------------------------------------
  ! -----------------------------------------------------------------------------------------
  ! -----------------------------------------------------------------------------------------

  subroutine writevar_0d(filename,mold)
    ! -------------------------------------------------------------
    ! PURPOSE: EXPORT SCALAR VARIABLES TO BINARY FILES
    ! -------------------------------------------------------------
    ! INPUT:  
    !   - FILENAME = NAME OF THE OUTPUT FILE
    !   - MOLD     = VARIABLE TO FILL IN
    ! RETURN VALUE:
    !   - INPUT ARRAY
    ! -------------------------------------------------------------
    implicit none
    character(len=25), intent(in):: filename
    double precision, intent(in) :: mold
    character(len=14) charname
    integer           :: lengthout

    call open_file_out(filename,lengthout)
    write(unit=unite,fmt=*) mold
    close(unite)
    return

300 print*,'PROBLEM IN WRITING OUTPUT FILE ', filename
  end subroutine writevar_0d
  
  ! -----------------------------------------------------------------------------------------
  ! -----------------------------------------------------------------------------------------
  ! -----------------------------------------------------------------------------------------

  subroutine writevar_1d(filename,mold)
    ! -------------------------------------------------------------
    ! PURPOSE: EXPORT 1D VARIABLES TO BINARY FILES
    ! -------------------------------------------------------------
    ! INPUT:  
    !   - FILENAME = NAME OF THE OUTPUT FILE
    !   - MOLD     = VARIABLE TO FILL IN
    ! RETURN VALUE:
    !   - INPUT ARRAY
    ! -------------------------------------------------------------
    implicit none
    character(len=25), intent(in):: filename
    double precision, dimension(:), intent(in) :: mold
    integer:: i
    character(len=14) charname
    integer:: lengthout

    call open_file_out(filename,lengthout)
    do i=1,size(mold,1)
       write(unit=unite,fmt=*) mold(i)
    enddo
    close(unite)
    return

300 print*,'PROBLEM IN WRITING OUTPUT FILE ', filename
  end subroutine writevar_1d
   
  ! -----------------------------------------------------------------------------------------
  ! -----------------------------------------------------------------------------------------
  ! -----------------------------------------------------------------------------------------

  subroutine writevar_2d(filename,mold)
    ! -------------------------------------------------------------
    ! PURPOSE: EXPORT 2D VARIABLES TO BINARY FILES
    ! -------------------------------------------------------------
    ! INPUT:  
    !   - FILENAME = NAME OF THE OUTPUT FILE
    !   - MOLD     = VARIABLE TO FILL IN
    ! RETURN VALUE:
    !   - INPUT ARRAY
    ! -------------------------------------------------------------
    implicit none
    character(len=25), intent(in):: filename
    double precision, dimension(:,:), intent(in) :: mold
    integer:: i,nco
    character(len=14) charname
    character(len=3) chardim
    integer:: lengthout,rw

    write(chardim,'(i3)') size(mold,2)
    nco = scan(chardim,"123456789")
    call open_file_out(filename,lengthout)
    write(unit=unite,fmt='('//chardim(nco:)//'e30.20)',iostat=rw) &
         mold(1:size(mold,1),1:size(mold,2))
    close(unite)
    return

300 print*,'PROBLEM IN WRITING OUTPUT FILE ', filename
  end subroutine writevar_2d
  
  ! -----------------------------------------------------------------------------------------
  ! -----------------------------------------------------------------------------------------
  ! -----------------------------------------------------------------------------------------

  subroutine writevar_3d(filename,mold)
    ! -------------------------------------------------------------
    ! PURPOSE: EXPORT 3D VARIABLES TO BINARY FILES
    ! -------------------------------------------------------------
    ! INPUT:  
    !   - FILENAME = NAME OF THE OUTPUT FILE
    !   - MOLD     = VARIABLE TO FILL IN
    ! RETURN VALUE:
    !   - INPUT ARRAY
    ! -------------------------------------------------------------
    implicit none
    character(len=25), intent(in):: filename
    double precision, dimension(:,:,:), intent(in) :: mold
    integer:: i
    character(len=14) charname
    integer:: lengthout,rw

    call open_file_out(filename,lengthout)
    write(unit=unite,fmt='(e30.20)',iostat=rw) &
         mold(1:size(mold,1),1:size(mold,2),1:size(mold,3))
    close(unite)
    return

300 print*,'PROBLEM IN WRITING OUTPUT FILE ', filename
  end subroutine writevar_3d
  
  ! -----------------------------------------------------------------------------------------
  ! -----------------------------------------------------------------------------------------
  ! -----------------------------------------------------------------------------------------

  subroutine writevar_4d(filename,mold)
    ! -------------------------------------------------------------
    ! PURPOSE: EXPORT 4D VARIABLES TO BINARY FILES
    ! -------------------------------------------------------------
    ! INPUT:  
    !   - FILENAME = NAME OF THE OUTPUT FILE
    !   - MOLD     = VARIABLE TO FILL IN
    ! RETURN VALUE:
    !   - INPUT ARRAY
    ! -------------------------------------------------------------
    implicit none
    character(len=25), intent(in):: filename
    double precision, dimension(:,:,:,:), intent(in) :: mold
    character(len=14) charname
    integer:: lengthout,rw

    call open_file_out(filename,lengthout)
    write(unit=unite,fmt='(e30.20)',iostat=rw) &
         mold(1:size(mold,1),1:size(mold,2),1:size(mold,3),1:size(mold,4))
    close(unite)
    return

300 print*,'PROBLEM IN WRITING OUTPUT FILE ', filename
  end subroutine writevar_4d
  
  ! -----------------------------------------------------------------------------------------
  ! -----------------------------------------------------------------------------------------
  ! -----------------------------------------------------------------------------------------

  subroutine writevar_5d(filename,mold)
    ! -------------------------------------------------------------
    ! PURPOSE: EXPORT 5D VARIABLES TO BINARY FILES
    ! -------------------------------------------------------------
    ! INPUT:  
    !   - FILENAME = NAME OF THE OUTPUT FILE
    !   - MOLD     = VARIABLE TO FILL IN
    ! RETURN VALUE:
    !   - INPUT ARRAY
    ! -------------------------------------------------------------
    implicit none
    character(len=25), intent(in):: filename
    double precision, dimension(:,:,:,:,:), intent(in) :: mold
    character(len=14) charname
    integer:: lengthout,rw

    call open_file_out(filename,lengthout)
    write(unit=unite,fmt='(e30.20)',iostat=rw) &
         mold(1:size(mold,1),1:size(mold,2),1:size(mold,3),1:size(mold,4),1:size(mold,5))
    close(unite)
    return

300 print*,'PROBLEM IN WRITING OUTPUT FILE ', filename
  end subroutine writevar_5d
  
  ! -----------------------------------------------------------------------------------------
  ! -----------------------------------------------------------------------------------------
  ! -----------------------------------------------------------------------------------------

  subroutine writevar_6d(filename,mold)
    ! -------------------------------------------------------------
    ! PURPOSE: EXPORT 6D VARIABLES TO BINARY FILES
    ! -------------------------------------------------------------
    ! INPUT:  
    !   - FILENAME = NAME OF THE OUTPUT FILE
    !   - MOLD     = VARIABLE TO FILL IN
    ! RETURN VALUE:
    !   - INPUT ARRAY
    ! -------------------------------------------------------------
    implicit none
    character(len=25), intent(in):: filename
    double precision, dimension(:,:,:,:,:,:), intent(in) :: mold
    character(len=14) charname
    integer:: lengthout,rw

    call open_file_out(filename,lengthout)
    write(unit=unite,fmt='(e30.20)',iostat=rw) &
         mold(1:size(mold,1),1:size(mold,2),1:size(mold,3),1:size(mold,4),1:size(mold,5),1:size(mold,6))
    close(unite)
    return

300 print*,'PROBLEM IN WRITING OUTPUT FILE ', filename
  end subroutine writevar_6d
 
  ! -----------------------------------------------------------------------------------------
  ! -----------------------------------------------------------------------------------------
  ! -----------------------------------------------------------------------------------------

  subroutine writevar_7d(filename,mold)
    ! -------------------------------------------------------------
    ! PURPOSE: EXPORT 7D VARIABLES TO BINARY FILES
    ! -------------------------------------------------------------
    ! INPUT:
    !   - FILENAME = NAME OF THE OUTPUT FILE
    !   - MOLD     = VARIABLE TO FILL IN
    ! RETURN VALUE:
    !   - INPUT ARRAY
    ! -------------------------------------------------------------
    implicit none
    character(len=25), intent(in):: filename
    double precision, dimension(:,:,:,:,:,:,:), intent(in) :: mold
    character(len=14) charname
    integer:: lengthout,rw

    call open_file_out(filename,lengthout)
    write(unit=unite,fmt='(e30.20)',iostat=rw) &
         mold(1:size(mold,1),1:size(mold,2),1:size(mold,3),1:size(mold,4), &
         1:size(mold,5),1:size(mold,6),1:size(mold,7))
    close(unite)
    return

300 print*,'PROBLEM IN WRITING OUTPUT FILE ', filename
  end subroutine writevar_7d

end module mod_io_management
