SUBROUTINE GetData
USE Parameters
USE Globals

IMPLICIT NONE

REAL(8) :: fsmodels(8,5)
! Transitions from U
REAL(8) :: UPmodels(8,7),UTmodels(8,7),UEmodels(8,7)
REAL(8) :: reUPmodels(8,5),reUTmodels(8,5),reUEmodels(8,5),rlUPmodels(8,9),rlUTmodels(8,9),rlUEmodels(8,9)
! Transitions from P
REAL(8) :: PUmodels(8,7),PTmodels(8,7)
REAL(8) :: rlPTmodels(8,7)
! Transitions from T
REAL(8) :: TUmodels(8,7),TPmodels(8,9)
REAL(8) :: reTPmodels(8,5),rlTPmodels(8,8)
! Transitions from E (either P or T)
REAL(8) :: EUmodels(8,7)

INTEGER :: ibs,ia
CHARACTER(len=1000) :: charbs

! ----------------------------------------------------------
!IF (Bootstrap == 0) THEN
! ----------------------------------------------------------

    ! Load in empirical autocovariance - for estimation
    OPEN(1, FILE = datadir // 'age.txt'); READ(1,*) age; CLOSE(1)
    age = age-minage+1
    DO ia = 1,nage
        agevec(ia) = minage + ia-1
    END DO

    OPEN(1, FILE = datadir // 'cov.txt');READ(1,*) cov; CLOSE(1)

    OPEN(1, FILE = datadir // 'lag.txt');READ(1,*) lag; CLOSE(1)

    OPEN(1, FILE = datadir // 'nobs.txt');READ(1,*) nobs; CLOSE(1)
    
    ! Load in employment models

    ! first stage regression
    OPEN(1, FILE = databasedir // 'fsmodels.txt');READ(1,*) fsmodels; CLOSE(1)

    ! models like in Dearden et al (2 states)
    !OPEN(1, FILE = databasedir // 'jfmodels.txt');READ(1,*) jfmodels; CLOSE(1)
    !OPEN(1, FILE = databasedir // 'jlmodels.txt');READ(1,*) jlmodels; CLOSE(1)
    !OPEN(1, FILE = databasedir // 'remodels.txt');READ(1,*) remodels; CLOSE(1)
    !OPEN(1, FILE = databasedir // 'rlmodels.txt');READ(1,*) rlmodels; CLOSE(1)

        if (OnlyPerm==0) then
            ! models with 3 states: temporary contracts included
            OPEN(1, FILE = databasedir // 'UTmodels.txt');READ(1,*) UTmodels; CLOSE(1)
            OPEN(1, FILE = databasedir // 'UPmodels.txt');READ(1,*) UPmodels; CLOSE(1)
            OPEN(1, FILE = databasedir // 'UEmodels.txt');READ(1,*) UEmodels; CLOSE(1)
            OPEN(1, FILE = databasedir // 'EUmodels.txt');READ(1,*) EUmodels; CLOSE(1)
            OPEN(1, FILE = databasedir // 'PUmodels.txt');READ(1,*) PUmodels; CLOSE(1)
            OPEN(1, FILE = databasedir // 'PTmodels.txt');READ(1,*) PTmodels; CLOSE(1)
            OPEN(1, FILE = databasedir // 'TUmodels.txt');READ(1,*) TUmodels; CLOSE(1)
            OPEN(1, FILE = databasedir // 'TPmodels.txt');READ(1,*) TPmodels; CLOSE(1)
            OPEN(1, FILE = databasedir // 'reUTmodels.txt');READ(1,*) reUTmodels; CLOSE(1)
            OPEN(1, FILE = databasedir // 'reUPmodels.txt');READ(1,*) reUPmodels; CLOSE(1)
            OPEN(1, FILE = databasedir // 'reUEmodels.txt');READ(1,*) reUEmodels; CLOSE(1)
            OPEN(1, FILE = databasedir // 'rlUTmodels.txt');READ(1,*) rlUTmodels; CLOSE(1)
            OPEN(1, FILE = databasedir // 'rlUPmodels.txt');READ(1,*) rlUPmodels; CLOSE(1)
            OPEN(1, FILE = databasedir // 'rlUEmodels.txt');READ(1,*) rlUEmodels; CLOSE(1)
            OPEN(1, FILE = databasedir // 'reTPmodels.txt');READ(1,*) reTPmodels; CLOSE(1)
            OPEN(1, FILE = databasedir // 'rlTPmodels.txt');READ(1,*) rlTPmodels; CLOSE(1)
            OPEN(1, FILE = databasedir // 'rlPTmodels.txt');READ(1,*) rlPTmodels; CLOSE(1)
        endif
    kappa = fsmodels(model,1:5) !deterministic age effect

    ! -------------------------
    ! TRANSITION PROBS
    ! -------------------------
    ! U-->P,T,E 
    UPage = UPmodels(model,1:5)
    UTage = UTmodels(model,1:5)
    UEage = UEmodels(model,1:5)
    UPdur = UPmodels(model,6:7)
    UTdur = UTmodels(model,6:7)
    UEdur = UEmodels(model,6:7)

    ! T-->P,U
    TPage = TPmodels(model,1:5)
    TUage = TUmodels(model,1:5)
    TPdur = TPmodels(model,6:7)
    TPy   = TPmodels(model,8:9)
    TUy   = TUmodels(model,6:7)

    ! P-->U,T
    PUage = PUmodels(model,1:5)
    PTage = PTmodels(model,1:5)
    PUy   = PUmodels(model,6:7)
    PTy   = PTmodels(model,6:7)

    ! E-->U
    EUage = EUmodels(model,1:5)
    EUy   = EUmodels(model,6:7)

    ! -------------------------
    ! TRANSITION EARNINGS
    ! -------------------------
    ! U--> P,T,E
    reUTage = reUTmodels(model,1:3)
    reUPage = reUPmodels(model,1:3)
    reUEage = reUEmodels(model,1:3)
    reUTdur = reUTmodels(model,4)
    reUPdur = reUPmodels(model,4)
    reUEdur = reUEmodels(model,4)
    reUTrmse = reUTmodels(model,5)
    reUPrmse = reUPmodels(model,5)
    reUErmse = reUEmodels(model,5)

    rlUPage =  rlUPmodels(model,1:5)
    rlUPdur =  rlUPmodels(model,6)
    rlUPy   =  rlUPmodels(model,7)
    rlUPyM   = rlUPmodels(model,8)
    rlUPrmse = rlUPmodels(model,9)
    rlUTage =  rlUTmodels(model,1:5)
    rlUTdur =  rlUTmodels(model,6)
    rlUTy   =  rlUTmodels(model,7)
    rlUTyM   = rlUTmodels(model,8)
    rlUTrmse = rlUTmodels(model,9)
    rlUEage =  rlUEmodels(model,1:5)
    rlUEdur =  rlUEmodels(model,6)
    rlUEy   =  rlUEmodels(model,7)
    rlUEyM   = rlUEmodels(model,8)
    rlUErmse = rlUEmodels(model,9)

    ! T-->P
    reTPage = reTPmodels(model,1:3)
    reTPdur = reTPmodels(model,4)
    reTPrmse = reTPmodels(model,5)

    rlTPage =  rlTPmodels(model,1:5)
    rlTPdur =  rlTPmodels(model,6)
    rlTPy   =  rlTPmodels(model,7)
    rlTPrmse = rlTPmodels(model,8)

    ! P--T
    rlPTage =  rlPTmodels(model,1:5)
    rlPTy   =  rlPTmodels(model,6)
    rlPTrmse = rlPTmodels(model,7)

! ----------------------------------------------------------
!ELSEIF (Bootstrap == 1) THEN
!! ----------------------------------------------------------
!   
!   DO ibs = 1,nbstraps
!
!       write(charbs,*) ibs
!       charbs = adjustl(charbs)
!
!       OPEN(1, FILE = bsdirPerm // 'nmoments' // trim(charbs) // '.txt')
!       READ(1,*) bsnmoments(ibs)
!       CLOSE(1)
!
!       OPEN(1, FILE = bsdirPerm // 'exper' // trim(charbs) // '.txt')
!       READ(1,*) bsage(1:bsnmoments(ibs),ibs)
!       CLOSE(1)
!
!       OPEN(1, FILE = bsdirPerm // 'empcov' // trim(charbs) // '.txt')
!       READ(1,*) bscov(1:bsnmoments(ibs),ibs)
!       CLOSE(1)
!       
!       OPEN(1, FILE = bsdirPerm // 'diff' // trim(charbs) // '.txt')
!       READ(1,*) bsdiff(1:bsnmoments(ibs),ibs)
!       CLOSE(1)
!
!       OPEN(1, FILE = bsdirPerm // 'nobs' // trim(charbs) // '.txt')
!       READ(1,*) bsnobs(1:bsnmoments(ibs),ibs)
!       CLOSE(1)
!
!   END DO
!
!
!END IF


END SUBROUTINE GetData
