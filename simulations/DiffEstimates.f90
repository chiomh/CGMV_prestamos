SUBROUTINE DiffEstimates

USE Parameters
USE Globals

IMPLICIT NONE

EXTERNAL ObjFunction

INTEGER						::IPARAM(6), LDFJAC, i,info,id
REAL(8)					::RPARAM(7)
REAL(8), DIMENSION(:), ALLOCATABLE	    :: Xguess, Xsol, Xscale
REAL(8), DIMENSION(nmoments)	        :: Fsol, Fscale
REAL(8), DIMENSION(:, :), ALLOCATABLE	:: Fjac
REAL(8), DIMENSION(37)                  :: lveta, lveps

LDFJAC = nmoments
Fscale(:) = 1.0

npar =65				!number of parameters: 1 x vz0, 32 x veta, 32 x veps
if (IncludeMA1 == 1) npar = npar + 1
if (IncludeAR1 == 1) npar = npar + 1
allocate(Xguess(npar),Xsol(npar),Xscale(npar),Xfinal(npar))
allocate(Fjac(nmoments,npar))

Xfinal(1)	    = vz0guess		! v0

OPEN(1, FILE = diffestdir // 'veps.txt')
READ(1,*) lveps
CLOSE(1)
lveps = sqrt(lveps)

OPEN(1, FILE = diffestdir // 'veta.txt')
READ(1,*) lveta
CLOSE(1)
lveta = sqrt(lveta)


Xfinal(2:31)	= lveps(2:31)	! veta
Xfinal(32)	= lveps(33)		! veta
Xfinal(33)	= lveps(35)		! veta
Xfinal(34:63)	= lveta(2:31)	! veps
Xfinal(64)	= lveta(33)	! veps
Xfinal(65)	= lveta(35)	! veps
if (IncludeMA1 == 1) then
    OPEN(1, FILE = diffestdir // 'theta.txt')
    READ(1,*) thetaguess
    CLOSE(1)
    Xfinal(66) = thetaguess    
end if

CALL ObjFunction (nmoments,npar,Xfinal,Ffinal,1)

!get average covariance function
avcovfn(1) = sum(Vy(:,:)) / real(nage*nyears)
do id = 1,nlag
    avcovfn(id+1) = sum(COVy(1+id:nage,1+id:nyears,id)) / real((nage-id)*(nyears-id))
end do

CALL OutputResults

END SUBROUTINE DiffEstimates
