MODULE Globals
USE Parameters

IMPLICIT NONE


!GLOBALS FOR STORING EMPIRICAL MOMENTS AND DATA
integer :: globibs,npar

integer, dimension(nmoments) :: age,lag,nobs ! age for moment (convention is age, lag is number of periods backward, lag=0 for variance 
real(8), dimension(nmoments) :: cov  ! empirical covariance

!integer, dimension(nbstraps)   ::bsnmoments
!integer, dimension(2000,nbstraps) :: bsage, bsdiff, bsnobs
!real(8), dimension(2000,nbstraps) :: bscov ! empirical covariance
!real(8), dimension(npar,nbstraps)  :: XfinalBS

!GLOBALS USED IN CONSTRUCTION OF THEORETICAL COVARIANCE MATRIX
real(8), dimension(nage)        :: Vz,Vu,Vy,dataVy
real(8), dimension(nage,nlag)   :: COVz,COVu,COVy,dataCOVy

!GLOBALS TO STORE PARAMETERS FOR OBJECTIVE FUNCTION
real(8)                 :: vph,covph,vfe,theta
real(8)                 :: rhoguess,vetaguess,vepsguess,vfeguess,vphguess,covphguess,thetaguess
real(8), dimension(nage)    :: veta,veps
real(8), dimension(nage-1)              :: rho
real(8), dimension(:), ALLOCATABLE      :: Xfinal
real(8), dimension(nmoments)         :: Ffinal
integer, dimension(nage)    :: agevec

!GLOBALS FOR SIMULATIONS
real(8), dimension(nsim)        :: fesim,phsim
real(8), dimension(nsim,nage)   :: etasim,epssim,zsim,usim,fsim,ysim,pEUsim,pUEsim,expysim,lysim,lysimM,durtempsim,duruesim
real(8), dimension(nsim,nage)   :: pPTsim,pPUsim,pTPsim,pTUsim,pUPsim,pUTsim
integer, dimension(nsim,nage)   :: uesimI, tempsimI, workI

!GLOBALS FOR EMPLOYMENT MODELS
! First Stage
real(8) :: kappa(5)
! Transitions tipo upgrade
real(8) :: UEage(5),UEdur(2), TPage(5), UPage(5), UTage(5), TPdur(2),UTdur(2),UPdur(2)
! transitions tipo downgrade
real(8) :: EUage(5),PTage(5),PUage(5),TUage(5),EUy(2),PTy(2),TPy(2),PUy(2),TUy(2)
! reentry earnings
real(8) :: rlage(5),rldur,rly,rlyM,rlrmse,rlUage(5),rlUdur,rlUy,rlUyM,rlUrmse
real(8) :: reUEage(3),reUEdur,reUErmse
real(8) :: reUTage(3),reUTdur,reUTrmse,reUPage(3),reUPdur,reTPy(2),reUPrmse
real(8) :: rlTPage(5),reTPage(3),rlTPdur,reTPdur,rlTPy,rlTPrmse,reTPrmse
real(8) :: rlPTage(5), rlPTy, rlPTrmse

real(8) :: rlUTage(5),rlUTdur,rlUTy,rlUTyM,rlUTrmse
real(8) :: rlUPage(5),rlUPdur,rlUPy,rlUPyM,rlUPrmse
real(8) :: rlUEage(5),rlUEdur,rlUEy,rlUEyM,rlUErmse

END MODULE Globals
