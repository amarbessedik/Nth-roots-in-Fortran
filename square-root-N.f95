!AMAR BESSEDIK
!CSC540
!FORTRAN PROJECT
!
!THIS PROGRAM IS FOR COMPUTING THE SQUARE ROOT OF A NUMBER USING NEWTON-RAPHSON ALGOTHIM
!
program squarerootN
  implicit none
  
  integer, parameter :: num_elements = 6 ! NUM OF ELEMENTS (COULD BE VARIABLE)
  real, dimension(num_elements) :: values
  real :: limit
  integer :: i

  limit = 0.0005
  values = (/0.5, 1.0, 100.0, 150.0, 5000.0, 62632.52/)
  
  do i = 1, num_elements, 1
    call sqroot(values(i), limit)
  end do

end program squarerootN  

!SUB PROGRAM TO COMPUTE THE SQUARE ROOT 
subroutine sqroot (N, limit)
    implicit none
    integer :: guess_counter
    integer, parameter :: np = selected_real_kind(4)
    real(np) :: G, NG, N, limit, ans
    !Initializations
    
    !STARTING GUESS
    if(N < 1) then
       G = N/2.0
    else
       G = 1.0
    end if
    !SECOND GUESS
    NG = 0.5*(G + (N/G))
    guess_counter = 2 !(strating guess & the guess immediately after)
    !PRINT OUT THE FIRST & SECOND GUESSES
    print '(/, 1x, "COMPUTING THE SQUARE ROOT OF:", f8.2,/)', N
    print '(1x "GUESS # 1 : ", f12.6 )', G
    print '(1x "GUESS # 2 : ", f12.6 )', NG
    do 
      G = NG
      NG = 0.5 * (G + (N/G))
      guess_counter = guess_counter + 1
      print '(1x "GUESS #", i2, 1x, ": ", f12.6 )', guess_counter, NG
      if ((G - NG) <= limit) then
        ans = NG
        print '(/,1x,"THE NUMBER OF GUESSES IS: ",i4)', guess_counter
        print '(1x,"SQUARE ROOT OF ",f8.2, 1x, "IS: ", f12.6)', N, ans
        print*,'======================================'
        exit
      end if
    end do
end subroutine sqroot






