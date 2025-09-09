! fib_spiral.f90 — prints an ASCII Fibonacci square spiral
program fib_spiral
  implicit none
  integer, parameter :: N = 10          ! how many Fibonacci steps
  integer :: i, k, t, dir
  integer, allocatable :: fib(:)
  integer :: x, y, minx, maxx, miny, maxy
  integer :: width, height, rx, ry
  character(len=1), allocatable :: canvas(:,:)
  integer, parameter :: dx(0:3) = [1, 0, -1, 0]  ! right, up, left, down
  integer, parameter :: dy(0:3) = [0, -1, 0, 1]

  if (N < 1) stop "N must be >= 1"

  ! Build Fibonacci sequence: 1, 1, 2, 3, ...
  allocate(fib(N))
  fib(1) = 1
  if (N >= 2) fib(2) = 1
  do i = 3, N
     fib(i) = fib(i-1) + fib(i-2)
  end do

  ! ---------- First pass: find bounds ----------
  x = 0; y = 0
  minx = 0; maxx = 0; miny = 0; maxy = 0
  dir = 0  ! start moving right

  do k = 1, N
     do t = 1, fib(k)
        x = x + dx(dir)
        y = y + dy(dir)
        if (x < minx) minx = x
        if (x > maxx) maxx = x
        if (y < miny) miny = y
        if (y > maxy) maxy = y
     end do
     dir = mod(dir + 1, 4)
  end do

  width  = maxx - minx + 1
  height = maxy - miny + 1
  if (width <= 0 .or. height <= 0) stop "Degenerate bounds"

  ! ---------- Allocate canvas ----------
  allocate(canvas(height, width))
  canvas = ' '

  ! ---------- Second pass: draw the path ----------
  x = 0; y = 0
  dir = 0

  call plot(x, y, minx, miny, canvas, height, width, 'S')  ! start
  do k = 1, N
     do t = 1, fib(k)
        x = x + dx(dir)
        y = y + dy(dir)
        call plot(x, y, minx, miny, canvas, height, width, '*')
     end do
     dir = mod(dir + 1, 4)
  end do

  ! Mark the final point
  call plot(x, y, minx, miny, canvas, height, width, 'E')

  ! ---------- Print canvas (row 1 = top) ----------
  do ry = 1, height
     write(*,'(1000A1)') canvas(ry, :)
  end do
contains
  subroutine plot(px, py, offx, offy, cvs, h, w, ch)
    implicit none
    integer, intent(in) :: px, py, offx, offy, h, w
    character(len=1), intent(in) :: ch
    character(len=1), intent(inout) :: cvs(h, w)
    integer :: cx, cy
    cy = py - offy + 1      ! row  (y grows down on screen)
    cx = px - offx + 1      ! col
    if (cy >= 1 .and. cy <= h .and. cx >= 1 .and. cx <= w) then
       cvs(cy, cx) = ch
    end if
  end subroutine plot
end program fib_spiral
