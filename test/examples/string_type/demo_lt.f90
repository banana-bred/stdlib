program demo_lt
use stdlib_string_type
implicit none
type(string_type) :: string
logical :: res

string = "bcd"
res = string < "abc"
! res .eqv. .false.

res = string < "bcd"
! res .eqv. .false.

res = string < "cde"
! res .eqv. .true.
end program demo_lt
