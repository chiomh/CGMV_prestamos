PROGRAM Main
!
! Main.f90 calls subroutines
!
USE Parameters
USE Globals

IMPLICIT NONE

INTEGER :: ibs

write(*,*) 'Results for sex = ', sex, ' and education group = ', edlev

! 0. Load in Empirical Covariances
CALL GetData

! ----------------------------------------------------------
!IF (Bootstrap==0) THEN
! ----------------------------------------------------------

    ! 1. Make guesses
    CALL MakeGuess
    
   ! nmoments = nmomentsALL
write(*,*)
write(*,*) '----------------------------------------------'
write(*,*) 'ESTIMATION RESULTS'
write(*,*) '----------------------------------------------'

    ! 2. Do Minimum Distance Estimation 
    CALL Estimation
    
if (onlyPerm==0) then
    ! 3. Do Simulation
    CALL Simulate
endif

    ! 4. Save Results to File
    CALL OutputResults

! ----------------------------------------------------------
!ELSEIF (Bootstrap==1) THEN
!! ----------------------------------------------------------
!    
!    DO ibs = 1,nbstraps
!
!        globibs = ibs
!
!        ! 1. Make guesses
!        CALL MakeGuess
!
!        nmoments = bsnmoments(ibs)
!
!        write(*,*) 'Doing estimation for bootstrap: ', ibs
!
!        ! 2. Do estimation
!        CALL Estimation
!
!        ! 3. Get final estimates
!        XfinalBS(1,ibs) = rho
!        XfinalBS(2,ibs) = phi
!        XfinalBS(3,ibs) = pi
!        XfinalBS(4:nage+3,ibs)= tau
!        if (ProfileHet ==1) then
!            XfinalBS(nage+4,ibs) = sigb 
!            XfinalBS(nage+5,ibs) = covab
!        end if
!
!    END DO
!    
!    ! 4. Save Results to File
!    CALL OutputBootstrapResults
!
!END IF
END PROGRAM Main
