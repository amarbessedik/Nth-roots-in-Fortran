!AMAR BESSEDIK
!CSC540
!FORTRAN PROJECT
!
!THIS PROGRAM IS FOR COMPUTING THE Nth ROOT OF A NUMBER USING NEWTON-RAPHSON ALGOTHIM
!
program nth_root_N
  implicit none
  real, external :: power_n !COMPUTES X AT THE POWER OF n
  integer, parameter :: num_elements = 4 ! NUM OF ELEMENTS (COULD BE VARIABLE)
  real :: limit
  integer :: i !LOOP COUNTER
  real, dimension(num_elements) :: numbers
  integer, dimension(num_elements) :: powers
  !DATA & INITIALIZATIONS
  limit = 0.0005
  numbers = (/33.0, 2250.0, 148.0, 5231.23/)
  powers = (/3, 4, 5, 6/)

  !COMPUTE THE Nth ROOT OF ALL NUMBERS TO A CERTAIN ACCURACY LIMIT
  do i = 1, num_elements, 1
    call nth_root(numbers(i), powers(i), limit)
  end do

end program nth_root_N

!SUB PROGRAM TO COMPUTE THE nTH ROOT 
subroutine nth_root (X, n, limit)
    implicit none
    integer, intent(in) :: n
    integer :: guess_counter
    real,intent(in):: X, limit
    real :: G, NG, ANSWER
    real, external :: power_n
    character(len = 2) :: root_rank
    !Initializations
    !First of all:
    !Make sure both operands must be greater than 0
    if ((X > 0) .and. (n > 0)) then
     !STARTING GUESS
     if (X < 1) then
       G = X/2.0
     else
       G = 1.0
     end if
     !NEXT GUESS (THE GUESS IMMIDIATELY AFTER THE INITIAL GUESS)
     NG = 1.0/n * ( ((n - 1) * G) + X/power_n(G, (n-1)))
     guess_counter = 2
     !PRINT OUT THE 1ST & 2ND GUESSES
     print '(1x,"COMPUTING THE ",i1, 1x, A2, 1x,"ROOT OF",f8.2,/)', n, root_rank(n), X
     print '(1x "GUESS # 1 : ", f10.6 )', G
     print '(1x "GUESS # 2 : ", f10.6 )', NG

     do
      G = NG
      NG = 1.0/n * ( ((n - 1) * G) + X/power_n(G, (n-1)))
      guess_counter = guess_counter + 1
      print '(1x "GUESS #", i2, 1x, ": ", f10.6 )', guess_counter, NG
      if ((G - NG) <= limit) then
        ANSWER = NG

        print '(/,1x,"NUMBER OF GUESSES: ",i4)', guess_counter
        print '(1x,"THE ANSWER IS : ", f10.6)', ANSWER
        print*,'========================================'
        exit
      end if
     end do
    else
      print*, "BOTH OPERANDS MUST BE GREATER THAN 0"
    end if

end subroutine nth_root

!FUNCTION THE COMPUTES X AT THE POWER OF n (X^n)
recursive function power_n(X, n) result(pow)
    implicit none
    real, intent(in) :: X
    integer, intent (in) :: n
    real :: pow

    if(n == 0) then
        pow = 1
      else
        pow = X * power_n(X, (n-1))
    end if
end function power_n

!THIS FUN IS NOT REQUIRED FOR ACCURACY OF RESULTS,
!USEFUL FOR PRINTING PURPOSES ONLY
function root_rank(n) result(rank)
   implicit none
   integer :: n
   character(len = 2) :: rank
   select case(n)
     case(2)
       rank = "ND"
     case(3)
       rank = "RD"
     case default
       rank = "TH"
     end select
end function root_rank
