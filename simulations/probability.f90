MODULE PROBABILITY
IMPLICIT NONE
PRIVATE ARTH,CHOL,seed

INTERFACE ARTH ! Auxiliary routine
    MODULE PROCEDURE arth_r, arth_i
END INTERFACE

! Contains PDF: Normal,Gamma,Exponential,Chi-square,Mixture of Normals
! Contains CDF: Normal,Gamma,Exponential,Chi-square,Mixture of Normals
! Contains Random Number Generation: Uniform,Normal,Gamma,Exponential,Chi-square,
!									 Mixture of Normals,Truncated Normal,Double Truncated Normal,
!									 Dirichlet,Multinomial(returns an integer not a real)
!									 Multivariate Normal
! Contains Inverse CDF's: Normal,Gamma,Exponential,Chi-square,Mixture of Normals


!CONTAINS GAMMA FUNCTIONS:
! GAMMLN Auxiliary log of the gamma function
! GAMINC Auxiliary incompelete gamma function
! GSER Series used by incomplete gamma function
! GCF Fractional used by incomplete gamma function
! Also contains cholesky decomposition (the same routine as in matrix but included here to be selfcontained) it is private
! so no one else can use it adn it does not causes problems when using matrix

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                         PARAMETERS FOR:
! Marsaglia & Tsang generator for random normals & random exponentials.
! Translated from C by Alan Miller (amiller@bigpond.net.au)
! Marsaglia, G. & Tsang, W.W. (2000) `The ziggurat method for generating
! random variables', J. Statist. Software, v5(8).
! This is an electronic journal which can be downloaded from:
! http://www.jstatsoft.org/v05/i08
! N.B. It is assumed that all integers are 32-bit.
! N.B. The value of zigm2 has been halved to compensate for the lack of
!      unsigned integers in Fortran.
! Latest version - February 3 2004
REAL(8), PARAMETER  :: zigm1=2147483648.0d0,zigm2=2147483648.0d0,half=0.5d0
REAL(8) :: zigdn=3.442619855899d0,zigtn=3.442619855899d0,zigvn=0.00991256303526217d0, &
           zigq,zigde=7.697117470131487d0,zigte=7.697117470131487d0,zigve=0.003949659822581572d0
INTEGER, SAVE :: zigiz,zigjz,seed,zigkn(0:127),zigke(0:255),zighz
REAL(8), SAVE :: zigwn(0:127),zigfn(0:127),zigwe(0:255),zigfe(0:255)
LOGICAL, SAVE :: initialized=.FALSE.

CONTAINS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! PDF's !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
REAL(8) FUNCTION NORMPDF(z,mu,varin)
IMPLICIT NONE
REAL(8), INTENT(IN) :: z
REAL(8), INTENT(IN) :: mu,varin
REAL(8), PARAMETER :: logroot2pi = 0.918938533204672780563271317078D0
REAL(8) :: arg,std,var
var=varin
IF (DSQRT(var)<TINY(var)) var=TINY(var)
IF (var<0.0d0) THEN
	PAUSE '***FATAL ERROR: variance has to be positive (NORMPDF)***'
    STOP
END IF      
arg = -0.5D0*(z-mu)*(z-mu)/var
arg = -logroot2pi - 0.5*DLOG(var) + arg
normpdf = DEXP(arg)
END FUNCTION NORMPDF

REAL(8) FUNCTION GAMPDF(x,a,b)
! Returns the pdf of a gamma distribution evaluated at x. a is shape parameter and b inverse scale. 
! such that mean=a/b and var=a/(b*b)
! That is P(x) = [(b^a)/Gamma(a)] * [x^(a-1)] * exp(-x*b) for x>0 and a>=1.
IMPLICIT NONE
REAL(8), INTENT(IN) :: x,a,b
IF (a<0.0d0) THEN
    PAUSE '***FATAL ERROR: a has to be positive (GAMPDF)***'
    STOP
END IF
IF (b<0.0d0) THEN
	PAUSE '***FATAL ERROR: b has to be positive (GAMPDF)***'
    STOP
END IF        
IF (x>0) THEN
    GAMPDF=(a-1.0d0)*DLOG(x)-(x*b)-gammln(a)+a*DLOG(b)
    GAMPDF=DEXP(GAMPDF)
ELSE IF ((x==0.0d0).AND.(a<1.0d0)) THEN
    GAMPDF=HUGE(x)
ELSE IF ((x==0.0d0).AND.(a==1.0d0)) THEN
    GAMPDF=b
ELSE IF ((x==0.0d0).AND.(a>1.0d0)) THEN
	GAMPDF=0.0d0
END IF             
END FUNCTION GAMPDF

REAL(8) FUNCTION EXPPDF(x,a)
! PDF OF EXPONENTIAL DISTRIBUTION P(x|a) = (1/a)*exp(-x/a)
IMPLICIT NONE
REAL(8), INTENT(IN) :: x,a
IF (a<0.0d0) THEN
    PAUSE '***FATAL ERROR: a has to be positive (EXPPDF)***'
    STOP
END IF    
EXPPDF=DEXP(-x/a)/a
END FUNCTION EXPPDF

REAL(8) FUNCTION CHI2PDF(x,df)
IMPLICIT NONE
REAL(8), INTENT(IN) :: x,df
IF (df<0.0d0) THEN
    PAUSE '***FATAL ERROR: Degrees of freedom have to be positive (CHI2PDF)***'
    STOP
END IF    
CHI2PDF=gampdf(x,0.5d0*df,0.5d0)
END FUNCTION CHI2PDF

REAL(8) FUNCTION MIXNORMPDF(x,mu,var,p)
IMPLICIT NONE
REAL(8), INTENT(IN) :: p(:),x,mu(:),var(:)
INTEGER :: j
IF (ANY(var<0.0d0)) THEN
    PAUSE '***FATAL ERROR: variance has to be positive (MIXNORMPDF)***'
    STOP
END IF      
IF ((SUM(p)>1.000001d0).OR.(SUM(p)<0.999999d0)) THEN
    PAUSE '***FATAL ERROR: weights have to add to one (MIXNORMPDF)***'
    STOP
END IF
IF (ANY(p<0.0d0)) THEN
    PAUSE '***FATAL ERROR: weight have to be positive (MIXNORMPDF)***'
    STOP
END IF
MIXNORMPDF = 0.0D0
DO j = 1, SIZE(p)
	MIXNORMPDF = MIXNORMPDF + p(j)*NORMPDF(x,mu(j),var(j))
END DO
END FUNCTION MIXNORMPDF


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! CDF's !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
REAL(8) FUNCTION NORMCDF(zin,mu,var)
IMPLICIT NONE
REAL(8), INTENT(IN) :: zin
REAL(8), INTENT(IN) :: mu,var
REAL(8) :: zabs,p,arg,logpdf,z,std,small
REAL(8), PARAMETER :: p0=220.2068679123761D0,p1=221.2135961699311D0,p2=112.0792914978709D0, &
	p3 = 33.91286607838300D0,p4 = 6.373962203531650D0,p5 = .7003830644436881D0, &
    p6 = .3526249659989109D-01,q0 = 440.4137358247522D0,q1 = 793.8265125199484D0, &
    q2 = 637.3336333788311D0,q3 = 296.5642487796737D0,q4 = 86.78073220294608D0, &
    q5=16.06417757920695D0,q6=1.755667163182642D0,q7=.8838834764831844D-1,cutoff = 7.071D0, &
	logroot2pi = 0.918938533204672780563271317078D0
IF (var<0.0d0) THEN
	PAUSE '***FATAL ERROR: variance has to be positive (NORMCDF)***'
    STOP
END IF    
small=TINY(p)
z=zin-mu
zabs=DABS(z)  
IF (zabs<small) THEN
	NORMCDF=0.5d0
	RETURN
END IF
std = DSQRT(var)
IF (std<small) THEN
	IF (zin-mu>0.0d0) THEN
		NORMCDF = 1.0d0
	ELSE IF	(zin-mu<0.0d0) THEN
		NORMCDF = 0.0d0
	END IF
END IF
zabs=zabs/std
IF (z > 37.0D0) THEN
	NORMCDF = 1.0D0
	RETURN
ELSE IF (z < -37.0D0) THEN
	NORMCDF = 0.0D0
	RETURN
END IF
arg = -0.5D0*zabs*zabs
logpdf = -logroot2pi - DLOG(std) + arg
IF (zabs < cutoff) THEN
	p = arg + DLOG(((((((p6*zabs + p5)*zabs + p4)*zabs + p3)*zabs + &
	p2)*zabs + p1)*zabs + p0)) - DLOG((((((((q7*zabs + q6)*zabs + &
	q5)*zabs + q4)*zabs + q3)*zabs + q2)*zabs + q1)*zabs + &
	q0))
ELSE
	p = logpdf - DLOG((zabs + 1.0D0/(zabs + 2.0D0/(zabs + 3.0D0/(zabs + 4.0D0/ &
	(zabs + 0.65D0))))))
END IF
p = DEXP(p)
IF (z < 0.0D0) THEN
	NORMCDF=p
	RETURN
ELSE
	NORMCDF = 1.0D0 - p
	RETURN
END IF
RETURN
END FUNCTION NORMCDF

REAL(8) FUNCTION GAMCDF(x,a,b)
! Gives me the gamma cdf
! such that mean=a/b and var=a/(b*b)
! That is F(x) = integral(0,x) of [(b^a)/Gamma(a)] * [x^(a-1)] * exp(-x*b) for x>0 and a>=1.
IMPLICIT NONE
REAL(8), INTENT(IN) :: x,a,b
IF (a<0.0d0) THEN
    PAUSE '***FATAL ERROR: a has to be positive (GAMCDF)***'
    STOP
END IF
IF (b<0.0d0) THEN
	PAUSE '***FATAL ERROR: b has to be positive (GAMCDF)***'
    STOP
END IF        
GAMCDF=GAMMINC(x*b,a)
IF (GAMCDF>1.0d0) GAMCDF=1.0d0 
END FUNCTION GAMCDF

REAL(8) FUNCTION EXPCDF(x,a)
! CDF OF EXPONENTIAL DISTRIBUTION P(x|a) = (1/a)*exp(-x/a)
IMPLICIT NONE
REAL(8), INTENT(IN) :: x,a
IF (a<0.0d0) THEN
    PAUSE '***FATAL ERROR: a has to be positive (EXPCDF)***'
    STOP
END IF    
EXPCDF=1.0d0-DEXP(-x/a)
END FUNCTION EXPCDF

REAL(8) FUNCTION CHI2CDF(x,df)
IMPLICIT NONE
REAL(8), INTENT(IN) :: x,df
IF (df<0.0d0) THEN
    PAUSE '***FATAL ERROR: Degrees of freedom have to be positive (CHICDF_S)***'
    STOP
END IF    
CHI2CDF=GAMCDF(x,0.5d0*df,0.5d0)
END FUNCTION CHI2CDF

REAL(8) FUNCTION MIXNORMCDF(x,mu,var,p)
IMPLICIT NONE
REAL(8), INTENT(IN) :: p(:),x,mu(:),var(:)
INTEGER :: j
IF (ANY(var<0.0d0)) THEN
    PAUSE '***FATAL ERROR: variance has to be positive (MIXNORMCDF)***'
    STOP
END IF      
IF (SUM(p).NE.1.0d0) THEN
    PAUSE '***FATAL ERROR: weights have to add to one (MIXNORMCDF)***'
    STOP
END IF
IF (ANY(p<0.0d0)) THEN
    PAUSE '***FATAL ERROR: weight have to be positive (MIXNORMCDF)***'
    STOP
END IF
MIXNORMCDF = 0.0D0
DO j = 1, SIZE(p)
	MIXNORMCDF = MIXNORMCDF + p(j)*NORMCDF(x,mu(j),var(j))
END DO
END FUNCTION MIXNORMCDF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! RANDOM NUMBER GENERATION !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE SET_SEED(seedin)
IMPLICIT NONE
INTEGER, INTENT(IN), OPTIONAL :: seedin
INTEGER :: i
IF (PRESENT(seedin)) THEN
	seed=seedin 
ELSE
	CALL SYSTEM_CLOCK(seed)
END IF
!  Tables for NORMAL
zigq = zigvn*DEXP(half*zigdn*zigdn)
zigkn(0) = (zigdn/zigq)*zigm1
zigkn(1) = 0
zigwn(0) = zigq/zigm1
zigwn(127) = zigdn/zigm1
zigfn(0) = 1.0d0
zigfn(127) = DEXP( -half*zigdn*zigdn )
DO  i = 126, 1, -1
	zigdn = DSQRT( -2.0d0 * DLOG( zigvn/zigdn + DEXP( -half*zigdn*zigdn ) ) )
    zigkn(i+1) = (zigdn/zigtn)*zigm1
    zigtn = zigdn
    zigfn(i) = EXP(-half*zigdn*zigdn)
    zigwn(i) = zigdn/zigm1
END DO
!  Tables for EXPONENTIAL
zigq = zigve*DEXP( zigde )
zigke(0) = (zigde/zigq)*zigm2
zigke(1) = 0
zigwe(0) = zigq/zigm2
zigwe(255) = zigde/zigm2
zigfe(0) = 1.0d0
zigfe(255) = DEXP( -zigde )
DO  i = 254, 1, -1
	zigde = -DLOG( zigve/zigde + DEXP( -zigde ) )
    zigke(i+1) = zigm2 * (zigde/zigte)
    zigte = zigde
    zigfe(i) = EXP( -zigde )
    zigwe(i) = zigde/zigm2
END DO
initialized = .TRUE.
END SUBROUTINE SET_SEED

! Generate random 32-bit integers
INTEGER FUNCTION shr3( )
IF (.NOT.initialized) CALL SET_SEED( )
zigjz = seed
seed = IEOR(seed,ISHFT(seed,13))
seed = IEOR(seed,ISHFT(seed,-17))
seed = IEOR(seed,ISHFT(seed,5))
shr3 = zigjz + seed
END FUNCTION shr3

REAL(8) FUNCTION Sample_Uniform(a,b)
INTEGER ::  ival,zigjz
REAL(8), INTENT(IN) :: a,b
IF (a>b) THEN
	PAUSE 'upper limit lower than lower limit, Sample_Uniform'
	STOP
END IF
Sample_Uniform = 0.5d0 + 0.2328306d-9 * SHR3()
Sample_Uniform = Sample_Uniform*(b-a) + a
END FUNCTION Sample_Uniform

!  Generate random normals
REAL(8) FUNCTION Sample_Normal(mu,var)
REAL(8), INTENT(IN) :: mu,var
REAL(8), PARAMETER ::  r = 3.442620d0
REAL(8) :: x,y
IF (var<0.0d0) THEN
	PAUSE '***FATAL ERROR: variance has to be positive (Sample_Normal)***'
    STOP
END IF      
zighz = shr3( )
zigiz = IAND(zighz,127)
IF(ABS(zighz) < zigkn(zigiz)) THEN
	Sample_Normal = zighz * zigwn(zigiz)
	Sample_Normal = Sample_Normal*DSQRT(var) + mu
ELSE
	DO
		IF(zigiz==0) THEN
			DO
				x = -0.2904764d0*DLOG(Sample_Uniform(0.0d0,1.0d0))
				y = -DLOG(Sample_Uniform(0.0d0,1.0d0))
				IF( y+y >= x*x ) EXIT
            END DO
            Sample_Normal = r+x			
            IF (zighz<=0) Sample_Normal = -Sample_Normal
			Sample_Normal = Sample_Normal*DSQRT(var) + mu
            RETURN
		END IF
        x = zighz * zigwn(zigiz)
        IF (zigfn(zigiz) + Sample_Uniform(0.0d0,1.0d0)* &
				(zigfn(zigiz-1)-zigfn(zigiz))< DEXP(-half*x*x)) THEN
			Sample_Normal = x
			Sample_Normal = Sample_Normal*DSQRT(var) + mu
			RETURN
		END IF
		zighz = shr3( )
		zigiz = IAND(zighz,127)
		IF(ABS(zighz) < zigkn(zigiz)) THEN
            Sample_Normal = zighz * zigwn(zigiz)
            Sample_Normal = Sample_Normal*DSQRT(var) + mu
			RETURN
		END IF
	END DO
END IF
END FUNCTION Sample_Normal

!  Generate random exponentials
REAL(8) FUNCTION Sample_Exponential(a)
REAL(8), INTENT(IN) :: a
REAL(8)  ::  x
IF (a<0.0d0) THEN
    PAUSE '***FATAL ERROR: a has to be positive (Sample_Exponential)***'
    STOP
END IF    
zigjz = shr3( )
zigiz = IAND(zigjz,255)
IF (ABS(zigjz) < zigke(zigiz)) THEN
    Sample_Exponential = (ABS(zigjz) * zigwe(zigiz))*a
    RETURN
END IF
DO
    IF (zigiz==0) THEN
        Sample_Exponential = (7.69711 - DLOG(Sample_Uniform(0.0d0,1.0d0)))*a
        RETURN
    END IF
    x = ABS(zigjz) * zigwe(zigiz)
    IF (zigfe(zigiz) + Sample_Uniform(0.0d0,1.0d0)*(zigfe(zigiz-1) - zigfe(zigiz)) < DEXP(-x)) THEN
        Sample_Exponential = x*a
        RETURN
    END IF
    zigjz = shr3( )
    zigiz = IAND(zigjz,255)
    IF (ABS(zigjz) < zigke(zigiz)) THEN
        Sample_Exponential = (ABS(zigjz)*zigwe(zigiz))*a
        RETURN
    END IF
END DO
END FUNCTION Sample_Exponential

REAL(8) FUNCTION Sample_Gamma(a,b)
! Returns a number distributed as a gamma distribution with shape parameter a and inverse scale b.
! such that mean=a/b and var=a/(b*b)
! That is P(x) = [(b^a)/Gamma(a)] * [x^(a-1)] * exp(-x*b) for x>0 and a>=1.
! Uses the algorithm in
! Marsaglia, G. and Tsang, W.W. (2000) `A simple method for generating
! gamma variables', Trans. om Math. Software (TOMS), vol.26(3), pp.363-372.
IMPLICIT NONE
REAL(8), INTENT(IN) :: a,b
REAL(8) :: c,d,u,v,x
IF (a<0.0d0) THEN
    PAUSE '***FATAL ERROR: a has to be positive (Sample_Gamma)***'
    STOP
END IF
IF (b<0.0d0) THEN
	PAUSE '***FATAL ERROR: b has to be positive (Sample_Gamma)***'
    STOP
END IF        
IF (a<1.0d0) THEN
	PAUSE " This routine only takes a>=1"
	STOP
END IF
d = a - 1.0d0/3.0d0
c = 1.0d0/DSQRT(9.0d0*d)
! Start of main loop
DO
! Generate v = (1+cx)^3 where x is random normal; repeat if v <= 0.
	DO
		x = Sample_Normal(0.0d0,1.0d0)
		v = (1.0d0 + c*x)**3
		IF (v > 0.0d0) EXIT
	END DO
	! Generate uniform variable U
	u = Sample_Uniform(0.0d0,1.0d0)
	IF (u < 1.0d0 - 0.0331d0*x**4) THEN
		Sample_Gamma = d*v
		EXIT
	ELSE IF (DLOG(u) < 0.5d0*x**2 + d*(1.0d0 - v + DLOG(v))) THEN
		Sample_Gamma = d*v
		EXIT
	END IF
END DO
Sample_Gamma=Sample_Gamma/b
END FUNCTION Sample_Gamma

REAL(8) FUNCTION Sample_Mixture_Normal(mu,var,p)
! Returns 1 draws from a mixture of normals with nmix mixture components
! p vector of weights, mu vector of means and sigma vector of variances
IMPLICIT NONE
REAL(8), INTENT(in) :: p(:),mu(:),var(:)
INTEGER :: i, j,nmix
REAL(8) :: cdf(SIZE(p)+1),nor,u
IF (ANY(var<0.0d0)) THEN
   write(*,*) var
    PAUSE '***FATAL ERROR: variance has to be positive (Sample_Normal_Mixture)***'
    STOP
END IF      
IF ((SUM(p)>1.0000001d0).OR.(SUM(p)<0.9999999d0)) THEN
   write(*,*) sum(p)
    PAUSE '***FATAL ERROR: weights have to add to one (Sample_Normal_Mixture)***'
    STOP
END IF
IF (ANY(p<0.0d0)) THEN
   write(*,*) p
    PAUSE '***FATAL ERROR: weight have to be positive (Sample_Normal_Mixture)***'
    STOP
END IF
nmix=SIZE(p)
u = Sample_Uniform(0.0d0,1.0d0)
nor = Sample_normal(0.0d0,1.0d0)
cdf=0.0D0
DO i = 1, nmix
	cdf(i+1) = cdf(i) + p(i)
	IF ((u>cdf(i)).AND.(u<=cdf(i+1))) THEN
	    Sample_Mixture_Normal=mu(i)+DSQRT(var(i))*nor
	    RETURN
    END IF	    
END DO	
END FUNCTION Sample_Mixture_Normal

REAL(8) FUNCTION Sample_Truncated_Mixture_Normal(mu,var,p,a,lb)
! Returns 1 draws from a mixture of normals with nmix mixture components
! p vector of weights, mu vector of means and sigma vector of variances
! that is truncated to be between a and infinity (lb=true) or -infinity and a (lb=false)
! !!!!!!!!!!!!!!!!!!!!!!!!!NOT the same as sampling from a !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! !!!!!!!!!!!!!!!!!!!!!!!!!!mixture of truncated normals!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! the weights need to be updated
IMPLICIT NONE
REAL(8), INTENT(in) :: p(:),mu(:),var(:),a
LOGICAL, INTENT(IN) :: lb
INTEGER :: i, j,nmix
REAL(8) :: cdf(SIZE(p)+1),u,w(SIZE(p))
IF (ANY(var<0.0d0)) THEN
    PAUSE '***FATAL ERROR: variance has to be positive (Sample_Truncated_Mixture_Normal)***'
    STOP
END IF      
IF ((SUM(p)>1.0000001d0).OR.(SUM(p)<0.9999999d0)) THEN
    PAUSE '***FATAL ERROR: weights have to add to one (Sample_Truncated_Mixture_Normal)***'
    STOP
END IF
IF (ANY(p<0.0d0)) THEN
    PAUSE '***FATAL ERROR: weight have to be positive (Sample_Truncated_Mixture_Normal)***'
    STOP
END IF
nmix=SIZE(p)
IF (lb) THEN
	DO i = 1, nmix
		w(i) = (1.0d0 - NORMCDF(a,mu(i),var(i)))*p(i)
	END DO
ELSE
	DO i = 1, nmix
		w(i) = NORMCDF(a,mu(i),var(i))*p(i)
	END DO
END IF
w=w/SUM(w)
u = Sample_Uniform(0.0d0,1.0d0)
cdf=0.0D0
DO i = 1, nmix
	cdf(i+1) = cdf(i) + w(i)
	IF ((u>cdf(i)).AND.(u<=cdf(i+1))) THEN
	    Sample_Truncated_Mixture_Normal=Sample_Truncated_Normal(mu(i),var(i),a,lb)
	    RETURN
    END IF	    
END DO	
END FUNCTION Sample_Truncated_Mixture_Normal

REAL(8) FUNCTION Sample_Truncated_Normal(mu,var,a,lb)
! Returns one draw from a truncated normal with underlying
! mean=mu and VARIANCE=var with truncation
!           (a,+infty)    IF lb=TRUE
!           (-infty,a)    IF lb=FALSE
IMPLICIT NONE
REAL(8), INTENT(IN) :: mu,var,a
LOGICAL, INTENT(IN) :: lb
REAL(8), PARAMETER :: t4=0.45D0
REAL(8) :: u,z,phi_z,c,temp
c=((a-mu)/(DSQRT(var)))
IF (.NOT.lb) THEN
	c=-c
END IF
IF (c < t4) THEN
    ! normal rejection sampling
    DO
		u=Sample_Normal(0.0d0,1.0d0)
        IF (u > c) EXIT
    ENDDO
	temp=u
ELSE
    ! exponential rejection sampling
    DO
		u = Sample_Uniform(0.0d0,1.0d0)
		z = Sample_Exponential(1.0d0/c)
        phi_z=DEXP(-.5D0*(z*z))
        IF (u < phi_z) EXIT
    ENDDO
    temp=c + z
ENDIF
IF (.not.(lb)) THEN
	Sample_Truncated_Normal = mu - (temp*DSQRT(var))
ELSE
	Sample_Truncated_Normal = mu + (temp*DSQRT(var))
ENDIF
END FUNCTION Sample_Truncated_Normal

REAL(8) FUNCTION Sample_DT_Mixture_Normal(mu,var,p,a,b)
! Returns 1 draws from a mixture of normals with nmix mixture components
! p vector of weights, mu vector of means and sigma vector of variances
! that is truncated to be between a and b
! !!!!!!!!!!!!!!!!!!!!!!!!!NOT the same as sampling from a !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! !!!!!!!!!!!!!!!!!!!!!!!!!!mixture of truncated normals!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! the weights need to be updated
IMPLICIT NONE
REAL(8), INTENT(in) :: p(:),mu(:),var(:),a,b
INTEGER :: i, j,nmix
REAL(8) :: cdf(SIZE(p)+1),u,w(SIZE(p))
IF (ANY(var<0.0d0)) THEN
    PAUSE '***FATAL ERROR: variance has to be positive (Sample_Double_Truncated_Mixture_Normal)***'
    STOP
END IF      
IF ((SUM(p)>1.0000001d0).OR.(SUM(p)<0.9999999d0)) THEN
    PAUSE '***FATAL ERROR: weights have to add to one (Sample_Double_Truncated_Mixture_Normal)***'
    STOP
END IF
IF (ANY(p<0.0d0)) THEN
    PAUSE '***FATAL ERROR: weight have to be positive (Sample_Double_Truncated_Mixture_Normal)***'
    STOP
END IF
nmix=SIZE(p)
DO i = 1, nmix
	w(i) = (NORMCDF(b,mu(i),var(i)) - NORMCDF(a,mu(i),var(i)))*p(i)
END DO
w=w/SUM(w)
u = Sample_Uniform(0.0d0,1.0d0)
cdf=0.0D0
DO i = 1, nmix
	cdf(i+1) = cdf(i) + w(i)
	IF ((u>cdf(i)).AND.(u<=cdf(i+1))) THEN
	    Sample_DT_Mixture_Normal=Sample_Double_Truncated_Normal(mu(i),var(i),a,b)
	    RETURN
    END IF	    
END DO	
END FUNCTION Sample_DT_Mixture_Normal

REAL(8) FUNCTION Sample_Double_Truncated_Normal(mu,var,a,b)
! Generates one draw from truncated standard normal distribution (mu,sigma) on (a,b)
IMPLICIT NONE
REAL(8),INTENT(in) :: a,b,mu,var
REAL(8) :: c,c1,c2,u(2),x,cdel,f1,f2,z,az,bz,eps
REAL(8),PARAMETER :: t1=0.375D0,t2=2.18D0,t3=0.725D0,t4=0.45D0
LOGICAL :: lflip
REAL :: aaa
INTEGER :: j
eps=EPSILON(aaa)
az=(a-mu)/SQRT(var)
bz=(b-mu)/SQRT(var)
c1=az
c2=bz
lflip=.false.
IF (c1*c2<0.0D0) THEN
	IF ((f(c1)>t1) .and. (f(c2)>t1)) THEN
		cdel=c2-c1
		DO
			DO j = 1, 2
				u(j) = Sample_Uniform(0.0d0,1.0d0)
			END DO
			x=c1+cdel*u(1)
			IF (u(2)<f(x)) EXIT
		END DO
	ELSE
        DO
			x=Sample_Normal(0.0d0,1.0d0)
			IF ((x>c1) .and. (x<c2)) EXIT
		END DO
    END IF
ELSE 
    IF (c1<0.0D0) THEN
		c=c1
		c1=-c2
        c2=-c
        lflip=.true.
	END IF
    f1=f(c1)
    f2=f(c2)
    IF ((f2<eps) .or. (f1/f2>t2)) THEN
		IF (c1>t3) THEN
			!exponential rejection sampling
            c=c2-c1
			DO
				u(1) = Sample_Uniform(0.0d0,1.0d0)
				z = Sample_Exponential(1.0d0/c1)
                IF ((z<c) .and. (u(1)<f(z))) EXIT
            END DO
                x=c1+z
		ELSE
			!half-normal rejection sampling
            DO
				x=Sample_Normal(0.0d0,1.0d0)
                x=abs(x)
                IF ((x>c1) .and. (x<c2)) EXIT
			END DO
        END IF
	ELSE
		!uniform rejection sampling
        cdel=c2-c1
        DO
			DO j = 1, 2
				u(j) = Sample_Uniform(0.0d0,1.0d0)
			END DO
			x=c1+cdel*u(1)
			IF (u(2)<(f(x)/f1)) EXIT
		END DO
	END IF
END IF
IF (lflip) THEN
	Sample_Double_Truncated_Normal=mu-(SQRT(var)*x)
ELSE
	Sample_Double_Truncated_Normal=mu+(SQRT(var)*x)
END IF
CONTAINS
REAL(8) FUNCTION f(x)
REAL(8) :: x
	f=dexp(-.5D0*(x*x))
END FUNCTION f
END FUNCTION Sample_Double_Truncated_Normal

FUNCTION Sample_Dirichlet(k,a)
! DIRICHLET
IMPLICIT NONE
INTEGER, INTENT(IN) :: k
REAL(8), DIMENSION(:), INTENT(IN) :: a
REAL(8) :: Sample_Dirichlet(k)
REAL(8) :: rg(k),sg
INTEGER :: i
DO i = 1, k
	rg(i) = Sample_Gamma(a(i),1.0d0)
ENDDO
sg=SUM(rg)
Sample_Dirichlet = rg/sg
END FUNCTION Sample_Dirichlet

INTEGER FUNCTION Sample_Multinomial(p)
! INPUT  :   p  is kx1
! Returns a random variable sampled from a multinomial distribution with
! k categories having probabilities p
IMPLICIT NONE
REAL(8), INTENT(IN) :: p(:)
REAL(8) :: u,cdf(0:size(p))
INTEGER :: j,k
k=SIZE(p)
u = Sample_Uniform(0.0d0,1.0d0)
cdf=0.0d0
DO j=1,k
    cdf(j)=cdf(j-1)+p(j)
    IF ((u<=cdf(j)) .and. (u>cdf(j-1))) THEN
        Sample_Multinomial=j
        RETURN
    END IF
END DO
END FUNCTION Sample_Multinomial

FUNCTION Sample_Multivariate_Normal(mu,var) 
! Returns one draw from a multivariate normal with mean mu and varcovar var
IMPLICIT NONE
REAL(8), INTENT(IN) :: mu(:),var(:,:)
REAL(8) :: cvar(SIZE(var,1),SIZE(var,2)),Sample_Multivariate_Normal(SIZE(mu))
INTEGER :: j
cvar=CHOL(var)
DO j = 1, SIZE(mu)
	Sample_Multivariate_Normal(j) = Sample_Normal(0.0d0,1.0d0)
END DO
Sample_Multivariate_Normal = mu + MATMUL(cvar,Sample_Multivariate_Normal)
END FUNCTION Sample_Multivariate_Normal


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! INVERSE CDF's !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
REAL(8) FUNCTION INVNORMCDF(P,muin,varin)
!	Produces the normal deviate Z corresponding to a given lower
!	tail area of P; Z is accurate to about 1 part in 10**16.
IMPLICIT NONE
REAL(8), INTENT(IN) :: P
REAL(8), OPTIONAL, INTENT(IN) :: muin,varin
REAL(8) :: Q,R,mu,var
REAL(8), PARAMETER :: ZERO=0.D0, ONE = 1.D0, HALF = 0.5D0,SPLIT1 = 0.425D0, SPLIT2 = 5.D0, &
	CONST1 = 0.180625D0, CONST2 = 1.6D0
!	Coefficients for P close to 0.5
REAL(8), PARAMETER :: A0 = 3.3871328727963666080D0,A1=1.3314166789178437745D+2, &
A2=1.9715909503065514427D+3,A3=1.3731693765509461125D+4,A4=4.5921953931549871457D+4, &
A5=6.7265770927008700853D+4,A6=3.3430575583588128105D+4,A7=2.5090809287301226727D+3, &
B1=4.2313330701600911252D+1,B2=6.8718700749205790830D+2,B3=5.3941960214247511077D+3, &
B4=2.1213794301586595867D+4,B5=3.9307895800092710610D+4,B6=2.8729085735721942674D+4, &
B7=5.2264952788528545610D+3
!	Coefficients for P not close to 0, 0.5 or 1.
REAL(8), PARAMETER :: C0=1.42343711074968357734D0,C1=4.63033784615654529590D0, &
C2=5.76949722146069140550D0,C3=3.64784832476320460504D0,C4=1.27045825245236838258D0, &
C5=2.41780725177450611770D-1,C6=2.27238449892691845833D-2,C7=7.74545014278341407640D-4, &
D1=2.05319162663775882187D0,D2=1.67638483018380384940D0,D3=6.89767334985100004550D-1, &
D4=1.48103976427480074590D-1,D5=1.51986665636164571966D-2,D6=5.47593808499534494600D-4, &
D7=1.05075007164441684324D-9
!	CoefficientsforP near 0 or 1.
REAL(8), PARAMETER :: E0=6.65790464350110377720D0,E1=5.46378491116411436990D0, &
E2=1.78482653991729133580D0,E3=2.96560571828504891230D-1,E4=2.65321895265761230930D-2, &
E5=1.24266094738807843860D-3,E6=2.71155556874348757815D-5,E7=2.01033439929228813265D-7, &
F1=5.99832206555887937690D-1,F2=1.36929880922735805310D-1,F3=1.48753612908506148525D-2, &
F4=7.86869131145613259100D-4,F5=1.84631831751005468180D-5,F6=1.42151175831644588870D-7, &
F7=2.04426310338993978564D-15
IF ((P>1.0D0).OR.(P<0.0D0)) THEN
	PAUSE '***FATAL ERROR: P has to be between zero and one (INVNORMCDF)***'
	STOP
END IF
mu=0.0d0
IF (PRESENT(muin)) mu=muin
var=1.0d0
IF (PRESENT(varin)) THEN
    IF (varin<0.0d0) THEN
        PAUSE '***FATAL ERROR: variance has to be positive (INVNORMCDF)***'
        STOP
    END IF      
    var=varin
END IF    
Q = P - HALF
IF (ABS(Q) .LE. SPLIT1) THEN
	R = CONST1 - Q * Q
	INVNORMCDF=Q*(((((((A7*R+A6)*R+A5)*R+A4)*R+A3)*R+A2)*R+A1)*R+ A0) / &
	(((((((B7 * R + B6) * R + B5) * R + B4) * R + B3)* R + B2) * R + B1) * R + ONE)
	INVNORMCDF=(DSQRT(var)*INVNORMCDF)+mu
	RETURN
ELSE
	IF (Q .LT. ZERO) THEN
		R = P
	ELSE
		R = ONE - P
	END IF
	R = DSQRT(-DLOG(R))
	IF (R .LE. SPLIT2) THEN
		R = R - CONST2
	    INVNORMCDF=(((((((C7*R + C6) * R + C5) * R + C4) * R + C3)*R+C2)*R+ C1) * R + C0) / &
     (((((((D7 * R + D6) * R + D5) * R + D4) * R + D3)* R + D2) * R + D1) * R + ONE)
	ELSE
		R = R - SPLIT2
	    INVNORMCDF=(((((((E7*R + E6) * R + E5) * R + E4) * R + E3)*R+E2)*R + E1) * R + E0) / &
     (((((((F7 * R + F6) * R + F5) * R + F4) * R + F3)* R + F2) * R + F1) * R + ONE)
	END IF
	IF (Q .LT. ZERO) INVNORMCDF = - INVNORMCDF
	INVNORMCDF=(DSQRT(var)*INVNORMCDF)+mu
	RETURN
END IF
END FUNCTION INVNORMCDF

REAL(8) FUNCTION INVGAMCDF(p,a,b)
IMPLICIT NONE
REAL(8), INTENT(IN) :: p,a,b
REAL(8) :: xk,eps,mn,v,temp,sigma,mu,h,xnew
INTEGER :: j
IF ((p<0.0d0).OR.(p>1.0d0)) THEN
    PAUSE '***FATAL ERROR: P has to be between 0 and 1 (INVGAMCDF)***'
    STOP
END IF
IF (a<0.0d0) THEN
    PAUSE '***FATAL ERROR: a has to be positive (INVGAMCDF)***'
    STOP
END IF
IF (b<0.0d0) THEN
	PAUSE '***FATAL ERROR: b has to be positive (GAMCDF_S)***'
    STOP
END IF        
IF (p==0.0d0) THEN
    INVGAMCDF=0.0d0
    RETURN
ELSE IF (p==1.0d0) THEN
    INVGAMCDF=HUGE(xnew)
    RETURN
ELSE
    eps=EPSILON(eps)
    mn=a*b
    v=mn*b
    temp=dlog(v+(mn**2)) 
    mu=2.0d0*dlog(mn)-0.5d0*temp
    sigma=-2.0d0*dlog(mn)+temp
    xk=dexp(invnormcdf(p,mu,sigma))
    h = 1.0d0
    j=0
    DO WHILE ((abs(h)>DSQRT(eps)*ABS(xnew)).AND.(abs(h)>DSQRT(eps)))!.AND.(j<200))
        j=j+1     
        h=(gamcdf(xk,a,b) - p) / gampdf(xk,a,b)
        xnew = xk - h
        IF (xnew<0.0d0) THEN
            xnew = xk/10.d0
            h = xk-xnew
        END IF 
        xk=xnew
    END DO
    INVGAMCDF=xk
END IF    
END FUNCTION INVGAMCDF

REAL(8) FUNCTION INVEXPCDF(p,a)
IMPLICIT NONE
REAL(8), INTENT(IN) :: p,a
IF ((p<0.0d0).OR.(p>1.0d0)) THEN
    PAUSE '***FATAL ERROR: P has to be between 0 and 1 (INVEXPCDF)***'
    STOP
END IF
IF (a<0.0d0) THEN
    PAUSE '***FATAL ERROR: a has to be positive (INVEXPCDF)***'
    STOP
END IF    
INVEXPCDF=-DLOG(1.0d0-p)*a
END FUNCTION INVEXPCDF

REAL(8) FUNCTION INVCHI2CDF(p,df)
IMPLICIT NONE
REAL(8), INTENT(IN) :: p,df
IF ((p<0.0d0).OR.(p>1.0d0)) THEN
    PAUSE '***FATAL ERROR: P has to be between 0 and 1 (INVCHI2CDF)***'
    STOP
END IF
IF (df<0.0d0) THEN
    PAUSE '***FATAL ERROR: Degrees of freedom have to be positive (INVCHI2CDF)***'
    STOP
END IF    
INVCHI2CDF=INVGAMCDF(p,df/2.0d0,0.5d0)
END FUNCTION INVCHI2CDF

REAL(8) FUNCTION INVMIXNORMCDF(p,mu,var,prob)
IMPLICIT NONE
REAL(8), INTENT(IN) :: p,mu(:),var(:),prob(:)
REAL(8) :: x1,x2,f1,f2,xm,rtbis,dx
INTEGER :: n,j
n=SIZE(mu)
IF ((p<0.0d0).OR.(p>1.0d0)) THEN
    PAUSE '***FATAL ERROR: P has to be between 0 and 1 (INVMIXNORMCDF)***'
    STOP
END IF
IF (ANY(var<0.0d0)) THEN
    PAUSE '***FATAL ERROR: variance has to be positive (INVMIXNORMCDF)***'
    STOP
END IF      
IF (p==0.0d0) THEN
    INVMIXNORMCDF=-HUGE(INVMIXNORMCDF)
    RETURN
ELSE IF (p==1.0d0) THEN
    INVMIXNORMCDF=HUGE(INVMIXNORMCDF)
    RETURN
ELSE
	x1 = -HUGE(x1)
	x2 = 0.0d0
	f1 = MIXNORMCDF(x1,mu,var,prob) - p	
	f2 = MIXNORMCDF(x2,mu,var,prob) - p	
	IF (f2==0.0d0) THEN
		INVMIXNORMCDF=x2
		RETURN
	END IF		
	IF (f1*f2>0.0d0) THEN
		f1 = f2
		x1 = 0.0d0
		x2 = HUGE(x2)
	END IF
	IF (f1<0.0d0) THEN
		INVMIXNORMCDF=x1
		dx=x2-x1
	ELSE
		INVMIXNORMCDF=x2
		dx=x1-x2
	END IF
	DO 
		dx=dx*0.5d0
		x2=INVMIXNORMCDF+dx
		f2=MIXNORMCDF(x2,mu,var,prob) - p	
		IF (f2<=0.0d0) INVMIXNORMCDF=x2
		IF ((DABS(dx)<0.0000000001d0).OR.(f2==0.0d0)) RETURN
	END DO
END IF
END FUNCTION INVMIXNORMCDF


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! GAMMA SPECIAL FUNCTIONS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
REAL(8) FUNCTION gammln(xx)
IMPLICIT NONE
REAL(8), INTENT(IN) :: xx
REAL(8) :: tmp,x
REAL(8) :: stp = 2.5066282746310005d0
REAL(8), DIMENSION(6) :: coef = (/76.18009172947146d0,-86.50532032941677d0, &
    24.01409824083091d0,-1.231739572450155d0,0.1208650973866179d-2,-0.5395239384953d-5/)
IF (xx<0.0d0) THEN
    PAUSE '***FATAL ERROR: argument has to be postive (gammln_s)***'
    STOP
END IF    
x=xx
tmp=x+5.5d0
tmp=(x+0.5d0)*DLOG(tmp)-tmp
gammln=tmp+DLOG(stp*(1.000000000190015d0+&
		SUM(coef(:)/arth(x+1.0d0,1.0d0,SIZE(coef))))/x)
END FUNCTION gammln

REAL(8) FUNCTION gamminc(x,a)
IMPLICIT NONE
REAL(8), INTENT(IN) :: a,x
IF ((x<0.0d0).OR.(a<0.0d0)) THEN
    PAUSE '***FATAL ERROR: arguments have to be postive (gamminc)***'
    STOP
END IF    
IF (x<a+1.0d0) THEN
	gamminc=gser(a,x)
ElSE
	gamminc=1.0d0-gcf(a,x)
END IF
END FUNCTION gamminc

REAL(8) FUNCTION gser(a,x,gln)
IMPLICIT NONE
REAL(8), INTENT(IN) :: a,x
REAL(8), OPTIONAL, INTENT(OUT) :: gln
INTEGER, PARAMETER :: ITMAX=1000
REAL(8), PARAMETER :: EPS=4.0d0*epsilon(x)
INTEGER :: n
REAL(8) :: ap,del,summ
if (x == 0.0d0) then
	gser=0.0d0
	RETURN
end if
ap=a
summ=1.0d0/a
del=summ
do n=1,ITMAX
	ap=ap+1.0d0
	del=del*x/ap
	summ=summ+del
	if (abs(del) < abs(summ)*EPS) exit
end do
if (n > ITMAX) THEN
    PAUSE '***FATAL ERROR: a too large, ITMAX too small in gser***'
    STOP
ENDIF    
if (present(gln)) then
	gln=gammln(a)
	gser=summ*dexp(-x+a*dlog(x)-gln)
else
	gser=summ*dexp(-x+a*dlog(x)-gammln(a))
end if
END FUNCTION gser

REAL(8) FUNCTION gcf(a,x,gln)
IMPLICIT NONE
REAL(8), INTENT(IN) :: a,x
REAL(8), OPTIONAL, INTENT(OUT) :: gln
INTEGER, PARAMETER :: ITMAX=1000
REAL(8), PARAMETER :: EPS=epsilon(x),FPMIN=tiny(x)/EPS
INTEGER :: i
REAL(8) :: an,b,c,d,del,h
if (x == 0.0d0) then
	gcf=1.0d0
	RETURN
end if
b=x+1.0d0-a
c=1.0d0/FPMIN
d=1.0d0/b
h=d
do i=1,ITMAX
	an=-i*(i-a)
	b=b+2.0d0
	d=an*d+b
	if (abs(d) < FPMIN) d=FPMIN
	c=b+an/c
	if (abs(c) < FPMIN) c=FPMIN
	d=1.0d0/d
	del=d*c
	h=h*del
	if (abs(del-1.0d0) <= EPS) exit
end do
if (i > ITMAX) THEN
    PAUSE '***FATAL ERROR: a too large, ITMAX too small in gcf***'
    STOP
ENDIF    
if (present(gln)) then
	gln=gammln(a)
	gcf=dexp(-x+a*dlog(x)-gln)*h
else
	gcf=dexp(-x+a*dlog(x)-gammln(a))*h
end if
END FUNCTION gcf

! Routines relating to polynomials and recurrences:
FUNCTION arth_r(first,increment,n)
! Array function returning an arithmetic progression.
REAL(8), INTENT(IN) :: first,increment
INTEGER, INTENT(IN) :: n
REAL(8), DIMENSION(n) :: arth_r
INTEGER :: k,k2
REAL(8) :: temp
if (n > 0) arth_r(1)=first
if (n <= 16) then
do k=2,n
arth_r(k)=arth_r(k-1)+increment
end do
else
do k=2,8
arth_r(k)=arth_r(k-1)+increment
end do
temp=increment*8
k=8
do
if (k >= n) exit
k2=k+k
arth_r(k+1:min(k2,n))=temp+arth_r(1:min(k,n-k))
temp=temp+temp
k=k2
end do
end if
END FUNCTION arth_r

FUNCTION arth_i(first,increment,n)
INTEGER, INTENT(IN) :: first,increment,n
INTEGER, DIMENSION(n) :: arth_i
INTEGER :: k,k2,temp
if (n > 0) arth_i(1)=first
if (n <= 16) then
do k=2,n
arth_i(k)=arth_i(k-1)+increment
end do
else
do k=2,8
arth_i(k)=arth_i(k-1)+increment
end do
temp=increment*8
k=8
do
if (k >= n) exit
k2=k+k
arth_i(k+1:min(k2,n))=temp+arth_i(1:min(k,n-k))
temp=temp+temp
k=k2
end do
end if
END FUNCTION arth_i

FUNCTION CHOL(AIN)
IMPLICIT NONE
REAL(8), DIMENSION(:,:), INTENT(IN) :: AIN
REAL(8) :: A(SIZE(AIN,1),SIZE(AIN,2)),p(SIZE(AIN,1)),CHOL(SIZE(AIN,1),SIZE(AIN,2))
INTEGER :: i,j,n
REAL(8) :: summ
IF (SIZE(AIN,1).NE.SIZE(AIN,2)) THEN
    PAUSE 'FATAL ERROR: matrix is not square (CHOL)***'
    STOP
END IF    
A=AIN
CHOL=0.0d0
n=SIZE(A,1)
DO i=1,n
    summ=A(i,i)-DOT_PRODUCT(A(i,1:i-1),A(i,1:i-1))
    IF (summ <= 0.0) THEN
        PAUSE 'CHOL failed'
    END IF      
    p(i)=DSQRT(summ)
    A(i+1:n,i)=(A(i,i+1:n)-matmul(A(i+1:n,1:i-1),A(i,1:i-1)))/p(i)
    A(i,i)=p(i)
END DO
FORALL (i=1:n,j=1:n,j<=i) CHOL(i,j)=A(i,j)
END FUNCTION CHOL

END MODULE PROBABILITY


