module write_file
  use bl_types
  interface write_val
    module procedure write2file
  end interface
contains

  subroutine write2file(up, step, dx, lo, hi)
    double precision, pointer, dimension(:,:,:,:), intent(in) :: up
    integer,intent(in) :: step
    real(dp_t), pointer, intent(in) :: dx(:)
    integer,intent(in) :: lo(3), hi(3)
    character(len=50) :: filename
    integer :: fileunit, i, j, k

    write(filename,159) step
159 format('vel_', I3.3 ,'.dat')
    open(newunit=fileunit, file=filename)
    write(fileunit, '(a)') "variables = x,y,z,rho,mx,my,mz"
    write(fileunit, '(3(a, I4))') "zone i = ",hi(1)+1-lo(1)," j = ",hi(2)+1-lo(2)," k = ",hi(3)+1-lo(3)

    do k=lo(3),hi(3)
      z = dx(3)*(k + 0.5d0)
      do j=lo(2),hi(2)
        y = dx(2)*(j + 0.5d0)
        do i=lo(1),hi(1)
          x = dx(1)*(i + 0.5d0)
          write(fileunit, '(7(f10.7,2x))') x, y, z, up(i,j,k,1), up(i,j,k,2), up(i,j,k,3), up(i,j,k,4)
        end do
      end do
    end do
  end subroutine

end module write_file
