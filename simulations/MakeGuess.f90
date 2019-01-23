SUBROUTINE MakeGuess
!
! In the income process of the form
! 
!   y_it= deterministic_stuff + alpha_i + beta_i * t + u_it + z_it
!   u_it = epislon_it + theta * epsilon_{i,t-1}
!   z_it = rho * z_{i,t-1} + eta_it
!
USE Parameters
USE Globals

IMPLICIT NONE

! Guesses are standard deviations
! I keep the same guesses as in the UK case

! SD(Individual Fixed Effect i.e. alpha)
vfeguess = sqrt(0.0001)

! SD(Persistent Shock i.e. eta)
vetaguess = sqrt(0.01)

! SD(Transitory Shock i.e. epsilon)
vepsguess = sqrt(0.05)

! AR(1) coefficient i.e. rho
rhoguess = 0.8

! MA(1) coefficient i.e. theta
thetaguess = 0.0

! SD(Profile Heterogeneity i.e. beta)
!vphguess = sqrt(0.02)
vphguess = sqrt(0.02)

! Cov(alpha, beta)
covphguess = 0.0

!PARAM AS IN GS14
!vepsguess=sqrt(0.02)
!vetaguess=sqrt(0.04)
! as in guvenen 2007
!vfeguess = sqrt(0.05)
END SUBROUTINE MakeGuess
