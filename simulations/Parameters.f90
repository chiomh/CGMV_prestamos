MODULE Parameters
IMPLICIT NONE
SAVE

INTEGER, PARAMETER:: LONG = SELECTED_REAL_KIND (15,100) !15 instead of 8


!OPTIONS
integer, parameter  :: Bootstrap        = 0
integer, parameter  :: IncludeMA1       = 1
integer, parameter  :: IncludeFE        = 1
integer, parameter  :: AgeEffectsTrans  = 3 !0 for none, 1-8 for order of polynomial, 9 for nonparametric
integer, parameter  :: AgeEffectsPers   = 3 !0 for none, 1-8 for order of polynomial, 9 for nonparametric
integer, parameter  :: AgeEffectsRho    = 3 !0 for none, 1-8 for order of polynomial, 9 for nonparametric
integer, parameter  :: IncludePH        = 1
integer, parameter  :: IncludePHcovFE   = 0 !THERE IS A PROBLEM WITH THIS BECAUSE IT DOESNT RESTRICT CORRELATION TO BE IN (-1,1)
integer, parameter  :: IdentityWeight   = 0 
integer, parameter  :: NoUnemployment   = 0
integer, parameter  :: TemporaryEmp     = 1

!which moments to include in the estimation 
integer, parameter  :: IncludeVg        = 0 !1 for variance of earnings growth (moments = 2)
integer, parameter  :: IncludeCOVg      = 0 !1 for covariance b/w earnings growth and earnings (moments = 3)
integer, parameter  :: IncludePHexp     = 0 !1 if model individual heterogeneity with exp function 
integer, parameter  :: moments          = 1
integer, dimension(4),parameter  :: nmomentsALL      = (/273,273,273,252/)

!choose sex and education group for the simulation
integer, parameter  :: sex              = 1
integer, parameter  :: Female           = 0     !governs how persistent are reemployment earnings losses
integer, parameter  :: edlev            = 1 
integer, parameter  :: model            = 4*(sex-1)+edlev   
integer, parameter  :: onlyPerm         = 2

integer, parameter  :: nmoments      = nmomentsALL(edlev)
integer, dimension(4), parameter  :: minageV = (/19,19,19,22/) !19 for nongrad, 22 for grad     
integer, parameter :: minage=minageV(edlev)
integer, parameter  :: maxage           = 60   
integer, parameter  :: nage             = maxage-minage+1

!initial levels of employment
real(8), parameter, dimension(8) :: initpermvec1=(/0.1499297,0.1667443,0.0993653,.1602533,.1165189,.1529437,.1088083,.1681134/), &
                                    inittempvec1=(/.5034224,.4774209,.3454978,.3514369,.3512064,.3835324,.2660354,.380178/) 
real(8), parameter  :: initialperm      = initpermvec1(model) !0.72 for grad males, 0.70 for female grads
real(8), parameter  :: initialtemp      = inittempvec1(model) !0.72 for grad males, 0.70 for female grads
real(8), parameter  :: initialemp       = initialperm+initialtemp- 0.02*(sex-1) !0.72 for grad males, 0.70 for female grads

integer, parameter  :: nlag             = 6 !maximum number of covariance for any age, not including the variance
integer, parameter  :: nlagGr           = 6 ! maximum number of lags for variance and covariance of earnings growth 
 
!integer, parameter     :: nbstraps      = 250

integer, parameter  :: nsim             = 10000 !Number of simulation

! Paths
character(len=*), dimension(3), parameter :: sample=(/"All","Per","Tem"/)
character(len=*), parameter :: rootdir = "~/Google_Drive/maia/"
character(len=*), parameter :: databasedir  = rootdir//"stata/output/"//sample(onlyPerm+1)//"/"
character(len=*), parameter :: savedir      = rootdir//"fortran/simulations/"//sample(onlyPerm+1)//&
                                              "/sex_" //achar(48+sex) //&
                                              "/edgroup_" //achar(48+edlev) // "/"
character(len=*), parameter :: datadir      = rootdir//"stata/output/"//sample(onlyPerm+1)//&
                                              "/sex_" //achar(48+sex) //&
                                              "/edgroup_" //achar(48+edlev) // "/"

END MODULE Parameters
