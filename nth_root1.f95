
program nth_root_N
  implicit none
   real :: p
   real, external :: power_n
   p = power_n(2.0, 4)
   print*, '***', p

end program nth_root_N

recursive function power_n(x, n) result(pow)
    implicit none
    real, intent(in) :: x
    real :: pow
    integer, intent (in) :: n
    
	if(n .eq. 0) then
          pow = 1
	else
          pow = x * power_n(x, (n-1))
        end if
end function power_n



