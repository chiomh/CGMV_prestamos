SUBROUTINE OutputResults

USE Parameters
USE Globals
USE Procedures

IMPLICIT NONE

INTEGER :: il
CHARACTER(len=23) ::    a
OPEN(3, FILE = savedir // 'theta.txt', STATUS = 'replace'); WRITE(3,*) theta;CLOSE(3)
OPEN(3, FILE = savedir // 'covph.txt', STATUS = 'replace'); WRITE(3,*) covph; CLOSE(3)
OPEN(3, FILE = savedir // 'vph.txt', STATUS = 'replace'); WRITE(3,*) vph; CLOSE(3)
OPEN(3, FILE = savedir // 'vfe.txt', STATUS = 'replace'); WRITE(3,*) vfe; CLOSE(3)
OPEN(3, FILE = savedir // 'agevec.txt', STATUS = 'replace'); CALL WriteMatrixInteger(3,nage,1,agevec)
OPEN(3, FILE = savedir // 'rho.txt', STATUS = 'replace'); CALL WriteMatrixExpon(3,nage-1,1,rho)
OPEN(3, FILE = savedir // 'veta.txt', STATUS = 'replace'); CALL WriteMatrixExpon(3,nage,1,veta)
OPEN(3, FILE = savedir // 'veps.txt', STATUS = 'replace'); CALL WriteMatrixExpon(3,nage,1,veps)

OPEN(3, FILE = savedir // 'Vy.txt', STATUS = 'replace'); CALL WriteMatrixExpon(3,nage,1,Vy)
OPEN(3, FILE = savedir // 'Vz.txt', STATUS = 'replace'); CALL WriteMatrixExpon(3,nage,1,Vz)
OPEN(3, FILE = savedir // 'Vu.txt', STATUS = 'replace'); CALL WriteMatrixExpon(3,nage,1,Vu)
OPEN(3, FILE = savedir // 'dataVy.txt', STATUS = 'replace'); CALL WriteMatrixExpon(3,nage,1,dataVy)

DO il = 1,nlag
    IF(il<10) write (a,FMT='(I1)') il
    IF(il>=10) write (a,FMT='(I2)') il
    OPEN(3, FILE = savedir // 'COVy' // trim(a)  //'.txt', STATUS = 'replace')
    CALL WriteMatrixExpon(3,nage,1,COVy(:,il))
    OPEN(3, FILE = savedir // 'dataCOVy' // trim(a)  //'.txt', STATUS = 'replace')
    CALL WriteMatrixExpon(3,nage,1,dataCOVy(:,il))
END DO


OPEN(3, FILE = savedir // 'ysim.txt', STATUS = 'replace'); CALL WriteMatrixCSV(3,nsim,nage,ysim)
OPEN(3, FILE = savedir // 'expysim.txt', STATUS = 'replace'); CALL WriteMatrixCSVExpon(3,nsim,nage,expysim)
OPEN(3, FILE = savedir // 'uesim.txt', STATUS = 'replace'); CALL WriteMatrixCSVInteger(3,nsim,nage,uesimI)
OPEN(3, FILE = savedir // 'pEUsim.txt', STATUS = 'replace'); CALL WriteMatrixCSV(3,nsim,nage,pEUsim)
OPEN(3, FILE = savedir // 'pUEsim.txt', STATUS = 'replace'); CALL WriteMatrixCSV(3,nsim,nage,pUEsim)
OPEN(3, FILE = savedir // 'pPUsim.txt', STATUS = 'replace'); CALL WriteMatrixCSV(3,nsim,nage,pPUsim)
OPEN(3, FILE = savedir // 'pPTsim.txt', STATUS = 'replace'); CALL WriteMatrixCSV(3,nsim,nage,pPTsim)
OPEN(3, FILE = savedir // 'pTUsim.txt', STATUS = 'replace'); CALL WriteMatrixCSV(3,nsim,nage,pTUsim)
OPEN(3, FILE = savedir // 'pTPsim.txt', STATUS = 'replace'); CALL WriteMatrixCSV(3,nsim,nage,pTPsim)
OPEN(3, FILE = savedir // 'pUPsim.txt', STATUS = 'replace'); CALL WriteMatrixCSV(3,nsim,nage,pUPsim)
OPEN(3, FILE = savedir // 'pUTsim.txt', STATUS = 'replace'); CALL WriteMatrixCSV(3,nsim,nage,pUTsim)
END SUBROUTINE OutputResults
