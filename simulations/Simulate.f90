SUBROUTINE Simulate

USE Parameters
USE Globals
USE Procedures
USE random

IMPLICIT NONE

INTEGER         :: in,ia,isize,iseed(2),aa
REAL(8)         :: ltemp,ld1,ld2,lztemp1,lztemp2,lt1,lt2

!draw random numbers
isize = 2
iseed(1) = 6555
iseed(2) = 5444
CALL RANDOM_SEED(size = isize)
CALL RANDOM_SEED(put = iseed)   

! ----------------------------------------------------------------------- 
! Simulate nsim individuals 
! ----------------------------------------------------------------------- 
!$OMP PARALLEL DO PRIVATE(ia,aa,ltemp,ld1,ld2,lztemp1,lztemp2)
DO in = 1,nsim

    ! ----------------------------------------------------------------------- 
    ! Unobserved Heterogeneity (fixed and random effects) 
    ! ----------------------------------------------------------------------- 
    IF(IncludeFE==1) THEN

        ! alpha (unobserved heterogeneity)
        fesim(in) = random_normal()*sqrt(vfe)

        ! gamma (Profile Heterogeneity)
        IF(IncludePH ==1) THEN
            phsim(in) = random_normal()*sqrt(vph- (covph**2)/vfe) + (covph/vfe)*fesim(in)
        ELSE
            phsim(in) = 0.0
        END IF
    ELSE
        fesim(in) = 0.0
        phsim(in) = 0.0
    END IF  

    ! ----------------------------------------------------------------------- 
    ! Initial employment distribution - (uesimI=0 is employed as perm, =1 is
    ! unemployed, and =2 is employed as temporary)
    ! ----------------------------------------------------------------------- 
    

    ! ----------------------------------------------------------------------- 
    ! Assign income at AGE = 1 
    ! ----------------------------------------------------------------------- 

    fsim(in,1) = fesim(in) + phsim(in)

    IF(TemporaryEmp==0) THEN
        IF(NoUnemployment==0) THEN
            ! uesimI=0 is employed, uesimI=1 is unemp
            CALL RandomDiscrete1(uesimI(in,1),2,(/initialemp,1.0-initialemp /))
        ELSE
                uesimI(in,1) = 0
        ENDIF
    ELSE
        ! uesimI=0 is employed perm, uesimI=2 is employed as temp, uesimI=1 is unemp
        CALL RandomDiscrete1(uesimi(in,1),3,(/initialperm,1.0-initialperm-initialtemp,initialtemp /))
    END IF
    uesimI(in,1) = uesimI(in,1)-1

    IF(uesimI(in,1)==0) THEN
        etasim(in,1) = random_normal()*sqrt(veta(1))
        epssim(in,1) = random_normal()*sqrt(veps(1))
        zsim(in,1) = etasim(in,1)
        usim(in,1) = epssim(in,1)
        ysim(in,1) = fsim(in,1) + zsim(in,1) + usim(in,1)
        lysim(in,1) = ysim(in,1)
        lysimM(in,1) = 0.0
        duruesim(in,1) = 0.0
        durtempsim(in,1) = 0.0
    ELSEIF(uesimI(in,1)==1) THEN
        ysim(in,1)  = 0.0
        lysim(in,1) = 0.0
        lysimM(in,1) = 1.0
        duruesim(in,1) = 1.0
        durtempsim(in,1) = 0.0
    ELSEIF(uesimI(in,1)==2) THEN
        etasim(in,1) = random_normal()*sqrt(veta(1))
        epssim(in,1) = random_normal()*sqrt(veps(1))
        zsim(in,1) = etasim(in,1)
        usim(in,1) = epssim(in,1)
        ysim(in,1) = fsim(in,1) + zsim(in,1) + usim(in,1)
        lysim(in,1) = ysim(in,1)
        lysimM(in,1) = 0.0
        duruesim(in,1) = 0.0
        durtempsim(in,1) = 1.0
    ENDIF
    
    ! ----------------------------------------------------------------------- 
    ! AGE > 1. Income is y = f + z + u (f-unobserved het,z-persist,u-trans)
    ! ----------------------------------------------------------------------- 

    DO ia = 2,nage

        ! actual age, i.e. ia=2, aa=23 (20 for non-ed)
        aa = minage + ia - 1
        
        ! fixed part
        fsim(in,ia) = fesim(in) + phsim(in)*ia
        
        ! ----------------------------
        ! EMPLOYMENT TRANSITIONS
        ! ----------------------------

        ! =====================================
        ! P (or E=P or T in 2 state model) -->
        ! =====================================
        IF(uesimI(in,ia-1)==0) THEN !employed as P last period
            
            !check for change of status
            IF(TemporaryEmp==0) THEN
                IF(NoUnemployment==0) THEN
                    ! E --> U
                    !   pr[s(t)=U | s(t-1)=E] = a1+a2*age+a3*age^2+a4*age^3+a5*age^4+ 
                    !                           bu1*y(t-1)+bu2*y(t-1)^2
                    CALL cumnor(EUage(1)+EUage(2)*aa + EUage(3)*(aa**2) +EUage(4)*(aa**3)+EUage(5)*(aa**4) &
                                            + EUy(1)*ysim(in,ia-1) +EUy(2)*(ysim(in,ia-1)**2), pEUsim(in,ia), ltemp )
                    CALL RandomDiscrete1(uesimI(in,ia),2,(/1.0-pEUsim(in,ia),pEUsim(in,ia)/))
                ELSE
                    uesimI(in,ia) = 0
                END IF
            ELSE
                !P --> T
                !   pr[s(t)=T | s(t-1)=E] = a1+a2*age+a3*age^2+a4*age^3+a5*age^4+ 
                !                           bt1*y(t-1)+bt2*y(t-1)^2
                call cumnor(PTage(1)+PTage(2)*aa + PTage(3)*(aa**2) +PTage(4)*(aa**3)+PTage(5)*(aa**4) &
                                        + PTy(1)*ysim(in,ia-1)+PTy(2)*(ysim(in,ia-1)**2),pPTsim(in,ia),ltemp)
                !PU
                call cumnor(PUage(1)+PUage(2)*aa + PUage(3)*(aa**2) +PUage(4)*(aa**3)+PUage(5)*(aa**4) &
                                        + PUy(1)*ysim(in,ia-1)+PUy(2)*(ysim(in,ia-1)**2),pPUsim(in,ia),ltemp)
                ! Asign status
                CALL RandomDiscrete1(uesimI(in,ia),3,(/1.0-pPTsim(in,ia)-pPUsim(in,ia), pPUsim(in,ia),pPTsim(in,ia)/))
            ENDIF
            uesimI(in,ia) = uesimI(in,ia)-1
            
            pTPsim(in,ia) =0.0 
            pTUsim(in,ia) =0.0 
            pUPsim(in,ia) =0.0 
            pUTsim(in,ia) =0.0 
            pEUsim(in,ia) = pPUsim(in,ia)
            pUEsim(in,ia) =0.0 

            ! Assign income

            IF(uesimI(in,ia)==0) THEN !employed this period
                etasim(in,ia) = random_normal()*sqrt(veta(ia))
                epssim(in,ia) = random_normal()*sqrt(veps(ia))
                zsim(in,ia) = rho(ia-1)*zsim(in,ia-1) + etasim(in,ia)
                usim(in,ia) = epssim(in,ia) + theta*epssim(in,ia-1)
                ysim(in,ia) = fsim(in,ia) + zsim(in,ia) + usim(in,ia)
                lysim(in,ia) = ysim(in,ia)
                lysimM(in,ia) = 0.0
                duruesim(in,ia) = 0.0          
                durtempsim(in,1) = 0.0
            ELSEIF(uesimI(in,ia)==1) THEN !unemployed this period
                etasim(in,ia) = 0.0
                epssim(in,ia) = 0.0
                ysim(in,ia) = 0.0
                lysim(in,ia) = ysim(in,ia-1)
                lysimM(in,ia) = 0.0
                duruesim(in,ia) = 1.0
                durtempsim(in,1) = 0.0
            ELSEIF(uesimI(in,ia)==2) THEN !temporary this period
                ysim(in,ia) = rlPTage(1)+rlPTage(2)*aa + rlPTage(3)*(aa**2) + rlPTage(4)*(aa**3) + rlPTage(5)*(aa**4) + rlPTy*lysim(in,ia-1) 
                usim(in,ia) = random_normal()*rlPTrmse
                ysim(in,ia) = ysim(in,ia) +usim(in,ia)
                !lztemp1 = ysim(in,ia) - fsim(in,ia)-usim(in,ia)
                !lztemp2 = random_normal()*sqrt(Vz(ia))  !take from unconditional distribution
                !zsim(in,ia) = 0.35*lztemp1 + 0.65*lztemp2
                !usim(in,ia) = ysim(in,ia) - fsim(in,ia)-zsim(in,ia)                 
                zsim(in,ia) = ysim(in,ia)-usim(in,ia)-fsim(in,ia)
                lysim(in,ia) = ysim(in,ia)
                lysimM(in,ia) = 0.0
                duruesim(in,ia) = 0.0
                durtempsim(in,1) = 1.0
            ENDIF 

        ELSEIF(uesimI(in,ia-1)==1) THEN !U last period
            ! Calculate unemployemnt durations
            ld1 = 0.0
            ld2 = 0.0
            ! if only 1 period unemp
            if(duruesim(in,ia-1)==1.0 .and. ia>2) ld1 = 1.0
            ! if 2 periods unemp
            if(duruesim(in,ia-1)==2.0 .and. ia>3) ld2 = 1.0

            ! Calculate new distribution of employment

            ! Use employment models to estimate probability of transition
            ! 1. Finds a permanent job
            call cumnor(UPage(1)+UPage(2)*aa + UPage(3)*(aa**2) +UPage(4)*(aa**3)+UPage(5)*(aa**4) &
                                    + UPdur(1)*ld1 +UPdur(2)*ld2, pUPsim(in,ia),ltemp)
            ! 2. Finds a temporary job
            call cumnor(UTage(1)+UTage(2)*aa + UTage(3)*(aa**2) +UTage(4)*(aa**3)+UTage(5)*(aa**4) &
                                    + UTdur(1)*ld1 +UTdur(2)*ld2, pUTsim(in,ia),ltemp)

            ! Asign status based on probabilities
            CALL RandomDiscrete1(uesimI(in,ia),3,(/pUPsim(in,ia),1.0-pUTsim(in,ia)-pUPsim(in,ia), pUTsim(in,ia)/)) 
            uesimI(in,ia) = uesimI(in,ia)-1
                
            pPTsim(in,ia) =0.0
            pPUsim(in,ia) =0.0
            pTPsim(in,ia) =0.0 
            pTUsim(in,ia) =0.0 
            pEUsim(in,ia) =0.0
            pUEsim(in,ia) =pUPsim(in,ia)+pUTsim(in,ia)

            ! Assign income

            IF(uesimI(in,ia)==0) THEN !employed this period
                    ysim(in,ia) = rlUPage(1)+rlUPage(2)*aa + rlUPage(3)*(aa**2) + rlUPage(4)*(aa**3) + rlUPage(5)*(aa**4) + rlUPdur*ld1 + rlUPy*lysim(in,ia-1) + rlUPyM*lysimM(in,ia-1)
                    !ysim(in,ia) = reUPage(1)+reUPage(2)*aa + reUPage(3)*(aa**2) + reUPdur*ld1
                IF(Female==0) THEN !for males, it is better if mostly persistent
                        usim(in,ia) = random_normal()*rlUPrmse
                        !usim(in,ia) = random_normal()*reUPrmse
                        ysim(in,ia) = ysim(in,ia) +usim(in,ia)
                        zsim(in,ia) = ysim(in,ia) - fsim(in,ia)-usim(in,ia)
                ELSE IF(Female==1) THEN !better if mostly transitory
                        usim(in,ia) = random_normal()*rlUPrmse
                        !usim(in,ia) = random_normal()*reUPrmse
                        ysim(in,ia) = ysim(in,ia) +usim(in,ia)
                        lztemp1 = ysim(in,ia) - fsim(in,ia)-usim(in,ia)
                        lztemp2 = random_normal()*sqrt(Vz(ia))  !take from unconditional distribution
                        zsim(in,ia) = 0.35*lztemp1 + 0.65*lztemp2
                        usim(in,ia) = ysim(in,ia) - fsim(in,ia)-zsim(in,ia)                 
                END IF
                lysim(in,ia) = ysim(in,ia)
                lysimM(in,ia) = 0.0
                duruesim(in,ia) = 0.0               
                durtempsim(in,ia) = 0.0               
            ELSEIF(uesimI(in,ia)==1) THEN !unemployed this period
                etasim(in,ia) = 0.0
                epssim(in,ia) = 0.0
                ysim(in,ia) = 0.0
                lysim(in,ia) = lysim(in,ia-1)
                lysimM(in,ia) = lysimM(in,ia-1)
                duruesim(in,ia) = duruesim(in,ia-1)+1.0
                durtempsim(in,ia) = 0.0               
            ELSEIF(uesimI(in,ia)==2) THEN !temporary this period
                ysim(in,ia) = rlUTage(1)+rlUTage(2)*aa + rlUTage(3)*(aa**2) + rlUTage(4)*(aa**3) + rlUTage(5)*(aa**4) + rlUTdur*ld1 + rlUTy*lysim(in,ia-1) + rlUTyM*lysimM(in,ia-1)
                !ysim(in,ia) = reUTage(1)+reUTage(2)*aa + reUTage(3)*(aa**2) + reUTdur*ld1
                 IF(Female==0) THEN !for males, it is better if mostly persistent
                    usim(in,ia) = random_normal()*rlUTrmse
                    !usim(in,ia) = random_normal()*reUTrmse
                    ysim(in,ia) = ysim(in,ia) +usim(in,ia)
                    zsim(in,ia) = ysim(in,ia) - fsim(in,ia)-usim(in,ia)
            ELSE IF(Female==1) THEN !better if mostly transitory
                    usim(in,ia) = random_normal()*rlUTrmse
                    !usim(in,ia) = random_normal()*reUTrmse
                    ysim(in,ia) = ysim(in,ia) +usim(in,ia)
                    lztemp1 = ysim(in,ia) - fsim(in,ia)-usim(in,ia)
                    lztemp2 = random_normal()*sqrt(Vz(ia))  !take from unconditional distribution
                    zsim(in,ia) = 0.35*lztemp1 + 0.65*lztemp2
                    usim(in,ia) = ysim(in,ia) - fsim(in,ia)-zsim(in,ia)                 
            END IF
                lysim(in,ia) = ysim(in,ia)
                lysimM(in,ia) = 0.0
                duruesim(in,ia) = 0.0               
                durtempsim(in,ia) = 1.0               
            ENDIF 

        ELSEIF(uesimI(in,ia-1)==2) THEN !employed as T last period
            ! Calculate temporary durations
            ld1 = 0.0
            ld2 = 0.0
            ! if only 1 period unemp
            if(durtempsim(in,ia-1)==1.0 .and. ia>2) ld1 = 1.0
            ! if 2 periods unemp
            if(durtempsim(in,ia-1)==2.0 .and. ia>3) ld2 = 1.0
            ! Calculate new distribution of employment

            ! Use employment models to estimate probability of transition
            ! 1. Finds a permanent job
            call cumnor(TPage(1)+TPage(2)*aa + TPage(3)*(aa**2) +TPage(4)*(aa**3)+TPage(5)*(aa**4) &
                                    + TPdur(1)*ld1 +TPdur(2)*ld2+ TPy(1)*ysim(in,ia-1)+TPy(2)*(ysim(in,ia-1)**2), pTPsim(in,ia),ltemp)
            ! 2.fired 
            call cumnor(TUage(1)+TUage(2)*aa + TUage(3)*(aa**2) +TUage(4)*(aa**3)+TUage(5)*(aa**4) &
                                    + TUy(1)*ysim(in,ia-1)+TUy(2)*(ysim(in,ia-1)**2),pTUsim(in,ia),ltemp)
            ! Asign status based on probabilities
            CALL RandomDiscrete1(uesimI(in,ia),3,(/pTPsim(in,ia),pTUsim(in,ia),1.0-pTPsim(in,ia)-pTUsim(in,ia)/)) 
            uesimI(in,ia) = uesimI(in,ia)-1
            
            pPTsim(in,ia) =0.0
            pPUsim(in,ia) =0.0
            pUPsim(in,ia) =0.0 
            pUTsim(in,ia) =0.0 
            pEUsim(in,ia) =pTUsim(in,ia)
            pUEsim(in,ia) =0.0 

            ! Assign income

            IF(uesimI(in,ia)==0) THEN !employed this period
                !ysim(in,ia) = reTPage(1)+reTPage(2)*aa + reTPage(3)*(aa**2) + reTPdur*ld1 + reTPy(1)*ysim(in,ia-1) + reTPy(2)*(ysim(in,ia-1)**2.)
                ysim(in,ia) = rlTPage(1)+rlTPage(2)*aa + rlTPage(3) * (aa**2) + rlTPage(4) * (aa**3) + rlTPage(5)*(aa**4) + rlTPdur*ld1 + rlTPy*lysim(in,ia-1) 
                !usim(in,ia) = random_normal()*reTPrmse
                usim(in,ia) = random_normal()*rlTPrmse
                ysim(in,ia) = ysim(in,ia) +usim(in,ia)
                zsim(in,ia) = ysim(in,ia) - fsim(in,ia)-usim(in,ia)
                lysim(in,ia) = ysim(in,ia)
                lysimM(in,ia) = 0.0
                duruesim(in,ia) = 0.0               
                durtempsim(in,ia) = 0.0               
            ELSEIF(uesimI(in,ia)==1) THEN !unemployed this period
                etasim(in,ia) = 0.0
                epssim(in,ia) = 0.0
                ysim(in,ia) = 0.0
                lysim(in,ia) = ysim(in,ia-1)
                lysimM(in,ia) = 0.0
                duruesim(in,ia) = 1.0
                durtempsim(in,1) = 0.0
            ELSEIF(uesimI(in,ia)==2) THEN !temporary this period
                etasim(in,ia) = random_normal()*sqrt(veta(ia))
                epssim(in,ia) = random_normal()*sqrt(veps(ia))
                zsim(in,ia) = rho(ia-1)*zsim(in,ia-1) + etasim(in,ia)
                usim(in,ia) = epssim(in,ia) + theta*epssim(in,ia-1)
                ysim(in,ia) = fsim(in,ia) + zsim(in,ia) + usim(in,ia)
                lysim(in,ia) = ysim(in,ia)
                lysimM(in,ia) = 0.0
                duruesim(in,ia) = 0.0
                durtempsim(in,ia) = durtempsim(in,ia-1)+1.0
            ENDIF 
        ENDIF
    ENDDO ! ia=2,nage
    
    DO ia = 1,nage
        aa = ia +minage-1
        !nb: do not include constant here, since this is added back in matlab when we do distribution of constants
        ysim(in,ia) =  ysim(in,ia) + kappa(2)*aa + kappa(3)*(aa**2) + kappa(4)*(aa**3) + kappa(5)*(aa**4)

        IF (uesimI(in,ia) == 1.0) THEN
            workI(in,ia) = 0.0
        ELSE
            workI(in,ia) = 1.0
        ENDIF

        expysim(in,ia) = workI(in,ia)*exp(ysim(in,ia))

    END DO ! ia

END DO ! in=1,nsim
!$OMP END PARALLEL DO

END SUBROUTINE Simulate

