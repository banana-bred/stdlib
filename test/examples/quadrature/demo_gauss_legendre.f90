program demo_gauss_legendre
	use iso_fortran_env, dp => real64
	implicit none

	integer, parameter :: N = 6
	real(dp), dimension(N) :: x,w
	call gauss_legendre(x,w)
	print *, "integral of x**2 from -1 to 1 is",  sum(x**2 * w)
end program demo_gauss_legendre
