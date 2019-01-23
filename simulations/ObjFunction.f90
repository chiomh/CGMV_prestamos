SUBROUTINE ObjFunction (M,N,X,F,info)

USE Parameters
USE Globals
IMPLICIT NONE

!Local Variables
!-----------------------
INTEGER, INTENT(IN) :: M						!number of equations / moment conditions
INTEGER, INTENT(IN)	:: N,info						!number of parameters
REAL(8), DIMENSION(N), INTENT(IN) :: X		!parameter vector: (rho, lveta, lveps)
REAL(8), DIMENSION(M), INTENT(OUT) :: F		!moment conditions
INTEGER				:: ia, it, im,ip, id
REAL(8)			:: lweight
REAL(8)		    :: lrho(nage-1),lveta(nage),lvfe,lveps(nage),lvph,lcovph,ltheta

! 1. Extract parameters, incorporating identifying assumptions

ip = 1

IF(AgeEffectsRho==0) THEN
	lrho = X(ip) 
	ip = ip +1
ELSE IF(AgeEffectsRho>0 .and. AgeEffectsRho<9) THEN
	DO ia = 1,nage-1
		lrho(ia) = X(ip)
		DO it = 1,AgeEffectsRho
			lrho(ia) = lrho(ia) + (ia**it)*X(ip+it)
		END DO
	END DO
	ip = ip +AgeEffectsRho +1
ELSE IF(AgeEffectsRho==9) THEN
	lrho = X(ip:ip+nage-2)
	ip = ip +nage-1
END IF

IF(IncludeMA1==1) THEN
	ltheta = X(ip)
	ip = ip + 1
ELSE
	ltheta = 0.0
END IF

IF(IncludeFE==1) THEN
	lvfe = X(ip)
	ip = ip+1
ELSE
	lvfe = 0.0
END IF

IF(IncludePH==1) THEN
	lvph = X(ip)
	ip = ip+1
ELSE
	lvph = 0.0
END IF	

IF(IncludePHcovFE==1) THEN
	lcovph = X(ip) 	!this is covariance between beta and normalized alpha 
	ip = ip+1
ELSE
	lcovph = 0.0
END IF	

IF(AgeEffectsPers==0) THEN
	lveta = X(ip) 
	ip = ip +1
ELSE IF(AgeEffectsPers>0 .and. AgeEffectsPers<9) THEN
	DO ia = 1,nage
		lveta(ia) = X(ip)
		DO it = 1,AgeEffectsPers
			lveta(ia) = lveta(ia) + (ia**it)*X(ip+it)
		END DO
	END DO
	ip = ip +AgeEffectsPers +1
ELSE IF(AgeEffectsPers==9) THEN
	lveta(1:nage-1) = X(ip:ip+nage-2)
	ip = ip +nage-1
	lveta(nage) = lveta(nage-1)
END IF

IF(AgeEffectsTrans==0) THEN
	lveps = X(ip) 
	ip = ip +1
ELSE IF(AgeEffectsTrans>0 .and. AgeEffectsTrans<9) THEN
	DO ia = 1,nage
		lveps(ia) = X(ip)
		DO it = 1,AgeEffectsTrans
			lveps(ia) = lveps(ia) + (ia**it)*X(ip+it)
		END DO
	END DO
	ip = ip +AgeEffectsTrans +1
ELSE IF(AgeEffectsTrans==9) THEN
	lveps(1:nage) = X(ip:ip+nage-1)
	ip = ip +nage
END IF


lveta = lveta**2
lveps = lveps**2
lvfe = lvfe**2
lvph = lvph**2

! 2 - Calculate 'theoretical' variance cov matrix of income

! Variance of persistent component, z
Vz(1) = lveta(1)
DO ia = 2, nage
    Vz(ia) = (lrho(ia-1)**2)*Vz(ia-1) + lveta(ia)
END DO

! Variance of transitory component, u
Vu(1) = lveps(1)
DO ia = 2, nage
    Vu(ia) = lveps(ia) + ltheta*lveps(ia-1)
END DO


! Variance of total earnings (residual) y 
DO ia = 1, nage
	Vy(ia) = Vz(ia) + Vu(ia) +lvfe + (ia**2)*lvph + 2*ia*lcovph
END DO

! Calculate covariance of persistent component, z
DO ia = 2, nage
	DO id = 1,min(ia-1,nlag)
		COVz(ia,id) = PRODUCT(lrho(ia-id:ia-1)) * Vz(ia-id)
	END DO
END DO

!Calculate covariance of transitory component, u
DO ia = 2, nage
	COVu(ia,1) = ltheta*lveps(ia-1)
	COVu(ia,2:nlag) = 0.0
END DO

!Calculate age/diff covariances
DO ia = 2, nage
	DO id = 1,min(ia-1,nlag)
		COVy(ia,id) = COVz(ia,id) + COVu(ia,id) + lvfe + ia*(ia-id)*lvph + (ia-id)*lcovph + ia*lcovph
	END DO
END DO


!4. Fill in the theoretical/empirical moment differences corresponding to the data vector
!---------------------------------------------------- 
!IF (Bootstrap==0) THEN
	!$OMP PARALLEL DO PRIVATE(lweight)
	DO im = 1, M
		lweight	= sqrt(real(nobs(im)))
		IF (IdentityWeight==1) lweight = 1.0

		IF (lag(im)==0) THEN
			F(im) = cov(im) - Vy(age(im))
			dataVy(age(im)) = cov(im) 
		ELSE
			F(im) = cov(im) - COVy(age(im),lag(im))
			dataCOVy(age(im),lag(im)) = cov(im) 
		END IF
		
		!multiply by weight
		F(im) = F(im)*lweight
		
	END DO
	!$OMP END PARALLEL DO

!ELSEIF (Bootstrap ==1) THEN
!	DO im = 1, M
!		diff	= bsdiff(im,globibs)
!		age		= bsage(im,globibs)
!		weight	= bsnobs(im,globibs)
!
!		IF (diff==0) THEN
!			F(im) = bscov(im,globibs) - Vy(age-minage+1) 
!			weightV(age-minage+1) = weight
!		ELSE
!			F(im) = bscov(im,globibs) - COVy(age-minage+1, diff)
!			weightCOV(age-minage+1, diff) = weight
!		END IF
!		
!		!multiply by weight
!		F(im) = F(im)*sqrt(weight)
!		
!	END DO
!
!END IF

!OPEN(111, FILE = datadir // 'objectivef.txt')
!write(*,*) sum(F(1:M)*F(1:M))
!CLOSE(111)

END SUBROUTINE ObjFunction
