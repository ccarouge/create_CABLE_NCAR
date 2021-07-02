! Compilation:
! fort -O0 -g -check bounds -traceback -convert big_endian -assume byterecl -o remap_test remap_IGBP2CABLE.F90
! Run:
! ./remap_test /g/data/w35/LIS/LIS_PARAMS/UMD/1KM/landcover_IGBP_NCEP.1gd4r /g/data/w35/ccc561/LIS-WRF/test_cable_veg.1gd4r
program remap_IGBP2CABLE
    implicit none
    
    integer iargc, i, grec, bb, nrec
    integer block_size
    character*100 inputf, outputf
 
  ! data structures
    INTEGER, parameter :: ic = 36000, ir=15000  !input grid
    REAL, ALLOCATABLE :: igbp(:, :), cable(:, :)
  
    i =  iargc()
    If (i.lt.2) Then   ! wrong cmd line args, print usage
       write(*, *)"Remap the IGBP PFTs on a 1km map to the CABLE PFTs."
       write(*, *)"Usage:"
       write(*, *)"remap_IGBP2CABLE input_file output_file"
       write(*, *)"Usage example: "
       write(*, *)"remap_IGBP2CABLE landcover_IGBP_NCEP.1gd4r landcover_CABLE.1gd4r"
       stop
    End If
  
    call getarg(1, inputf)  
    call getarg(2, outputf)
    
    ! Read and write file by blocks to limit memory load.
    block_size = ir / 5  ! ir = 15000, so each block will be 3000 rows.

    allocate(igbp(ic, block_size))
    allocate(cable(ic, block_size))
    
    open(51, file=inputf, status='old', form='unformatted', &
         access='direct',recl=ic*4)
    open(52, file=outputf, form='unformatted', &
         access='direct',recl=ic*4)

    ! loop through the data
    do bb = 1, ir, block_size
        print*, "Row loop index:", bb
        do i = 1, block_size
            grec = bb + (i-1)
            !print*, "grec:", grec
            read(51, rec=grec) igbp(:, i)            
        end do
        !igbp = 14

        CALL remap_veg(igbp, ic, block_size, cable)

        do i=1, block_size
            nrec = bb + (i-1)
            write(52, rec=nrec) cable(:, i)
        end do
        print*, MAXVAL(cable)
        print*, MINVAL(cable)
    end do
    close(51)
    close(52)
     
contains
    subroutine remap_veg(in_veg, ncol, nrow, out_veg)
    implicit none
    INTEGER, INTENT(IN):: ncol, nrow ! Array dimensions
    REAL,INTENT(IN):: in_veg(ncol,nrow)
    REAL,INTENT(OUT)::  out_veg(ncol,nrow)
    
    ! Local variables
    INTEGER :: i, j ! loop indexes
    
    ! Initialise out_veg with in_veg values for the cases where no mapping is done
    out_veg = in_veg

    do j=1,ncol
        do i = 1, nrow
            
            select case (int(in_veg(j, i)))
            case(5)
                out_veg(j, i) = 1
            case(6)
                out_veg(j, i) = 5
            case(7)
                out_veg(j, i) = 5
            case(8)
                out_veg(j, i) = 6
            case(9)
                out_veg(j, i) = 6
            case(10)
                out_veg(j, i) = 6
            case(12)
                out_veg(j, i) = 9
            case(13)
                out_veg(j, i) = 15
            case(14)
                out_veg(j, i) = 9
            case(15)
                out_veg(j, i) = 17
            case(16)
                out_veg(j, i) = 14
            case(17)
                out_veg(j, i) = 16
            case(18)
                out_veg(j, i) = 8
            case(19)
                out_veg(j, i) = 8
            end select
        end do
    end do
    end subroutine remap_veg

end program remap_IGBP2CABLE