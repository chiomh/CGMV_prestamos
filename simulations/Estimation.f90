SUBROUTINE Estimation

USE Parameters
USE Globals

IMPLICIT NONE

EXTERNAL ObjFunction

!Declare Local Variables
!----------------------------------
INTEGER                                 :: info,ip,ia,it
REAL(8), DIMENSION(:), ALLOCATABLE      :: Xguess, Xsol
REAL(8), DIMENSION(nmoments)            :: Fsol
real(8):: pi_3, phi_3
real(8):: rhoplot(10)


!Allocate variables: depending on the assumed income process (ARMA, fixed
!effects, function of age....) we will have a different number of parameters.
!npar counts the final number of parameters based on the global choices in
!Parameters.f90 and then allocates this dimension to the parameters vector.
!------------------------------------
npar = 0

!rho
IF(AgeEffectsRho<9) npar = npar + AgeEffectsRho +1
IF(AgeEffectsRho==9) npar = npar + nage-1

!theta
IF(IncludeMA1==1)       npar = npar + 1

!fixed effects, profile het
IF(IncludeFE==1)        npar = npar + 1
IF(IncludePH==1) npar = npar+1
IF(IncludePHcovFE==1) npar = npar+1

!pers
IF(AgeEffectsPers==0) THEN
    npar = npar+1       !veta
ELSE IF(AgeEffectsPers<9) THEN
    npar = npar+AgeEffectsPers + 1      !veta
ELSE IF(AgeEffectsPers==9) THEN
    npar = npar + nage-1
END IF

!trans
IF(AgeEffectsTrans==0 ) THEN
    npar = npar+1
ELSE IF (AgeEffectsTrans<9 ) THEN
    npar = npar+AgeEffectsTrans + 1
ELSE IF (AgeEffectsTrans==9 ) THEN
    npar = npar+nage
END IF

! Xguess is the initial vector
! Xsol will store the solution to the minimization (MSM)
! Xfinal will store the final vector of estimates
ALLOCATE(Xguess(npar),Xsol(npar),Xfinal(npar))


!Construct guess
!-------------------
ip = 1

IF(AgeEffectsRho==0) THEN
    Xguess(ip) = rhoguess
    ip = ip +1
ELSE IF(AgeEffectsRho>0 .and. AgeEffectsRho<9) THEN
        Xguess(ip) = rhoguess
        Xguess(ip+1:ip+AgeEffectsRho) = 0.0
        ip = ip +AgeEffectsRho+1
ELSE IF(AgeEffectsRho==9) THEN
        Xguess(ip:ip+nage-2) = rhoguess
        ip = ip +nage-1
END IF

IF(IncludeMA1==1) THEN
    Xguess(ip) = thetaguess
    ip = ip + 1
END IF

IF(IncludeFE==1) THEN
    Xguess(ip) = vfeguess
    ip = ip+1
END IF

IF(IncludePH==1) THEN
    Xguess(ip) = vphguess
    ip = ip+1
END IF  

IF(IncludePHcovFE==1) THEN
    Xguess(ip) = covphguess 
    ip = ip+1
END IF  

IF(AgeEffectsPers==0)   THEN
    Xguess(ip) = vetaguess
    ip = ip+1
ELSE IF(AgeEffectsPers>0 .and. AgeEffectsPers<9) THEN
    Xguess(ip) = vetaguess
    Xguess(ip+1:ip+AgeEffectsPers) = 0.0
    ip = ip +AgeEffectsPers+1
ELSE IF(AgeEffectsPers==9) THEN
    Xguess(ip:ip+nage-2) = vetaguess
    ip = ip +nage-1
END IF

IF(AgeEffectsTrans==0) THEN
    Xguess(ip) = vepsguess
    ip = ip+1
ELSE IF(AgeEffectsTrans>0 .and. AgeEffectsTrans<9) THEN
    Xguess(ip) = vepsguess
    Xguess(ip+1:ip+AgeEffectsTrans) = 0.0
    ip = ip +AgeEffectsTrans+1
ELSE IF(AgeEffectsTrans==9) THEN
    Xguess(ip:ip+nage-1) = vepsguess
    ip = ip +nage
END IF
    

!--------------------------------------------------------------------------
! ESTIMATION- Minimum distance 
!--------------------------------------------------------------------------
CALL lmdif1 ( ObjFunction, nmoments, npar, Xguess, Fsol,  1.0e-6_8, info )
write(*,*) 'info = ',info
Xsol = Xguess
Xfinal = Xsol

!--------------------------------------------------------------------------
!Extract solution: construct age-specific income profiles
!--------------------------------------------------------------------------
ip = 1

IF(AgeEffectsRho==0) THEN
	rho = Xfinal(ip) 
	ip = ip +1
ELSE IF(AgeEffectsRho>0 .and. AgeEffectsRho<9) THEN
	DO ia = 1,nage-1
		rho(ia) = Xfinal(ip)
		DO it = 1,AgeEffectsRho
			rho(ia) = rho(ia) + (dble(ia)**it)*Xfinal(ip+it)
		END DO
	END DO
	ip = ip +AgeEffectsRho+1
ELSE IF(AgeEffectsRho==9) THEN
	rho = Xfinal(ip:ip+nage-2)
	ip = ip +nage-1
END IF
IF(IncludeMA1==1) THEN
	theta = Xfinal(ip)
	ip = ip + 1
ELSE
	theta = 0.0
END IF

IF(IncludeFE==1) THEN
	vfe = Xfinal(ip)
	ip = ip+1
ELSE
	vfe = 0.0
END IF

IF(IncludePH==1) THEN
	vph = Xfinal(ip)
	ip = ip+1
ELSE
	vph = 0.0
END IF	

IF(IncludePHcovFE==1) THEN
	covph = Xfinal(ip) 	!this is covariance between beta and normalized alpha 
	ip = ip+1
ELSE
	covph = 0.0
END IF	

IF(AgeEffectsPers==0) THEN
	veta = Xfinal(ip) 
	ip = ip +1
ELSE IF(AgeEffectsPers>0 .and. AgeEffectsPers<9) THEN
	DO ia = 1,nage
		veta(ia) = Xfinal(ip)
		DO it = 1,AgeEffectsPers
			veta(ia) = veta(ia) + (dble(ia)**it)*Xfinal(ip+it)
		END DO
	END DO
	ip = ip +AgeEffectsPers+1
ELSE IF(AgeEffectsPers==9) THEN
	veta(1:nage-1) = Xfinal(ip:ip+nage-2)
	ip = ip +nage-1
	veta(nage) = veta(nage-1)
END IF

IF(AgeEffectsTrans==0) THEN
	veps = Xfinal(ip) 
	ip = ip +1
ELSE IF(AgeEffectsTrans>0 .and. AgeEffectsTrans<9) THEN
	DO ia = 1,nage
		veps(ia) = Xfinal(ip)
		DO it = 1,AgeEffectsTrans
			veps(ia) = veps(ia) + (dble(ia)**it)*Xfinal(ip+it)
		END DO
	END DO
	ip = ip +AgeEffectsTrans +1
ELSE IF(AgeEffectsTrans==9) THEN
	veps(1:nage) = Xfinal(ip:ip+nage-1)
	ip = ip +nage
END IF


vph = vph**2
veta = veta**2
vfe = vfe**2
veps = veps**2

IF(Bootstrap==0) THEN
	write(*,*) "av veta: ", sum(veta)/real(nage)
	write(*,*) "av veps: ", sum(veps)/real(nage)
	write(*,*) "av rho: ", sum(rho)/real(nage-1)
	if (IncludeMA1 == 1) write(*,*) "theta: ", theta
	if (IncludeFE == 1) write(*,*) "vfe: ", vfe
	if (IncludePH == 1) write(*,*) "vph: ", vph
	if (IncludePHcovFE == 1) write(*,*) "covph: ", covph
END IF

CALL ObjFunction (nmoments,npar,Xfinal,Ffinal,1)

END SUBROUTINE Estimation
