MODULE Procedures

USE Parameters
USE Globals

IMPLICIT NONE
CONTAINS

!-----------------------------------------------

SUBROUTINE LinInterp (n,x,y,ni,xi,yi)
!this does linear interpolation of (x,y) at points xi
!requires x to be sorted in ascending order
!extrapolates out of range

INTEGER, INTENT(in)		:: n,ni
REAL(long), INTENT(in)	:: x(n),y(n),xi(ni)
REAL(long), INTENT(out)	:: yi(ni)
REAL(long), DIMENSION(ni)	:: xL,xH,yL,yH
INTEGER					:: i,locL(ni)


DO i = 1,ni

	LocL(i) = MAXLOC(x,1,MASK=xi(i)>x)
	
	IF (xi(i)<=x(1)) THEN
		LocL(i) = 1
	END IF

	IF (LocL(i)>=n) THEN 
		LocL(i) = n-1
	END IF


	xL(i) = x(locL(i))
	xH(i) = x(locL(i)+1)
	yL(i) = y(locL(i))
	yH(i) = y(locL(i)+1)
	
	yi(i) = yL(i) + (xi(i)-xL(i))*((yH(i)-yL(i))/(xH(i)-xL(i)))

END DO


END SUBROUTINE LinInterp

!-----------------------------------------------

SUBROUTINE LinInterp1 (n,x,y,xi,yi)
!this does linear interpolation of (x,y) at points only point,xi
!requires x to be sorted in ascending order
!extrapolates out of range

INTEGER, INTENT(in)		:: n
REAL(long), INTENT(in)	:: x(:),y(:),xi
REAL(long), INTENT(out)	:: yi
REAL(long)	            :: xL,xH,yL,yH
INTEGER					:: locL

LocL = MAXLOC(x,1,MASK=xi>x)

IF (xi<=x(1)) THEN
	LocL = 1
END IF

IF (LocL>=n) THEN 
	LocL = n-1
END IF

xL  = x(locL)
xH  = x(locL +1)
yL  = y(locL)
yH  = y(locL +1)

yi  = yL  + (xi -xL )*((yH -yL )/(xH -xL ))

END SUBROUTINE LinInterp1

!----------------------------------------------
SUBROUTINE WriteMatrix(f,n1,n2,mat)

INTEGER, INTENT(IN)			:: f,n1,n2
REAL(long),INTENT(in)		:: mat(n1,n2)
CHARACTER		::lstring*80
INTEGER			::i1

WRITE(UNIT=lstring, FMT='(I5)') n2
lstring = '('//trim(lstring) // 'F16.6)'
DO i1=1,n1
	WRITE(f,lstring) (mat(i1,:))
END DO

CLOSE(f)

END SUBROUTINE WriteMatrix
!----------------------------------------------
SUBROUTINE WriteMatrixLong(f,n1,n2,mat)

INTEGER, INTENT(IN)			:: f,n1,n2
REAL(long),INTENT(in)		:: mat(n1,n2)
CHARACTER		::lstring*80
INTEGER			::i1

WRITE(UNIT=lstring, FMT='(I5)') n2
lstring = '('//trim(lstring) // 'F20.14)'
DO i1=1,n1
	WRITE(f,lstring) (mat(i1,:))
END DO

CLOSE(f)

END SUBROUTINE WriteMatrixLong

!----------------------------------------------
SUBROUTINE WriteMatrixInteger(f,n1,n2,mat)

INTEGER, INTENT(IN)			:: f,n1,n2
INTEGER,INTENT(in)		:: mat(n1,n2)
CHARACTER		::lstring*80
INTEGER			::i1

WRITE(UNIT=lstring, FMT='(I5)') n2
lstring = '('//trim(lstring) // 'I16)'
DO i1=1,n1
	WRITE(f,lstring) (mat(i1,:))
END DO

CLOSE(f)

END SUBROUTINE WriteMatrixInteger
!----------------------------------------------
SUBROUTINE WriteMatrixCSV(f,n1,n2,mat)

INTEGER, INTENT(IN)			:: f,n1,n2
REAL(long),INTENT(in)		:: mat(n1,n2)
CHARACTER		::lstring*80
INTEGER			::i1

WRITE(UNIT=lstring, FMT='(I5)') n2-1
lstring = '('//trim(lstring) // '(F16.6,","),F16.6)'
DO i1=1,n1
	WRITE(f,lstring) (mat(i1,:))
END DO

CLOSE(f)

END SUBROUTINE WriteMatrixCSV

!----------------------------------------------
SUBROUTINE WriteMatrixExpon(f,n1,n2,mat)

INTEGER, INTENT(IN)			:: f,n1,n2
REAL(long),INTENT(in)		:: mat(n1,n2)
CHARACTER		::lstring*80
INTEGER			::i1

WRITE(UNIT=lstring, FMT='(I5)') n2
lstring = '('//trim(lstring) // 'E16.8)'
DO i1=1,n1
	WRITE(f,lstring) (mat(i1,:))
END DO

CLOSE(f)

END SUBROUTINE WriteMatrixExpon

!----------------------------------------------
SUBROUTINE WriteMatrixCSVExpon(f,n1,n2,mat)

INTEGER, INTENT(IN)			:: f,n1,n2
REAL(long),INTENT(in)		:: mat(n1,n2)
CHARACTER		::lstring*80
INTEGER			::i1

WRITE(UNIT=lstring, FMT='(I5)') n2-1
lstring = '('//trim(lstring) // '(E16.8,","),E16.8)'
DO i1=1,n1
	WRITE(f,lstring) (mat(i1,:))
END DO

CLOSE(f)

END SUBROUTINE WriteMatrixCSVExpon
!----------------------------------------------
SUBROUTINE WriteMatrixCSVInteger(f,n1,n2,mat)

INTEGER, INTENT(IN)			:: f,n1,n2
INTEGER,INTENT(in)		:: mat(n1,n2)
CHARACTER		::lstring*80
INTEGER			::i1

WRITE(UNIT=lstring, FMT='(I5)') n2-1
lstring = '('//trim(lstring) // '(I16,","),I16)'
DO i1=1,n1
	WRITE(f,lstring) (mat(i1,:))
END DO

CLOSE(f)

END SUBROUTINE WriteMatrixCSVInteger
!----------------------------------------------
SUBROUTINE RandomDiscrete(Nout,Xout,Nin,Pin)
!generates Nout random draws from the integers 1 to Nin
!using probabilities in Pin

INTEGER, INTENT(in)			:: Nout,Nin
INTEGER,INTENT(out)		:: Xout(:)
REAL(long), INTENT(in)  ::Pin(:)

INTEGER			::i1,i2
REAL(long)      :: lran(Nout)

!IF(sum(Pin) .ne. 1.0) write(*,*) 'error in RandomDiscrete: Pin doesnt sum to 1.0'

CALL RANDOM_NUMBER(lran)

Xout(:) = 0
DO i1 = 1,Nout
    IF ( lran(i1) .le. Pin(1) ) THEN
        Xout(i1) = 1
    ELSE
        i2 = 2
        DO WHILE (i2 .le. Nin)
            IF ( (lran(i1) .le. SUM(Pin(1:i2)) ).and. (lran(i1) > SUM(Pin(1:i2-1)) ) ) THEN
                Xout(i1) = i2
                i2 = Nin+1
            ELSE
                i2 = i2+1
            END IF
        END DO
    END IF
END DO

END SUBROUTINE RandomDiscrete

!--------------------------------------------------------------
SUBROUTINE RandomDiscrete1(Xout,Nin,Pin)
!generates Nout random draws from the integers 1 to Nin
!using probabilities in Pin

INTEGER, INTENT(in)			:: Nin
INTEGER,INTENT(out)		:: Xout
REAL(long), INTENT(in)  ::Pin(:)

INTEGER			::i2
REAL(long)      :: lran

!IF(sum(Pin) .ne. 1.0) write(*,*) 'error in RandomDiscrete: Pin doesnt sum to 1.0'

CALL RANDOM_NUMBER(lran)

Xout = 0
IF ( lran .le. Pin(1) ) THEN
    Xout = 1
ELSE
    i2 = 2
    DO WHILE (i2 .le. Nin)
        IF ( (lran .le. SUM(Pin(1:i2)) ).and. (lran > SUM(Pin(1:i2-1)) ) ) THEN
            Xout = i2
            i2 = Nin+1
        ELSE
            i2 = i2+1
        END IF
    END DO
END IF


END SUBROUTINE RandomDiscrete1


END MODULE Procedures
