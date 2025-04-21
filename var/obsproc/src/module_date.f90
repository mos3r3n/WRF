MODULE module_date




















CONTAINS



   SUBROUTINE geth_idts (ndate, odate, idts, pass, iunit)
   
      IMPLICIT NONE
      

      
      
      
      
      
      
      
      
      
      CHARACTER (LEN=*) , INTENT(INOUT) :: ndate, odate
      INTEGER           , INTENT(OUT)   :: idts
      LOGICAL, OPTIONAL                 :: pass
      INTEGER, OPTIONAL                 :: iunit

      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      CHARACTER (LEN=24) :: tdate
      INTEGER :: olen, nlen
      INTEGER :: yrnew, monew, dynew, hrnew, minew, scnew
      INTEGER :: yrold, moold, dyold, hrold, miold, scold
      INTEGER :: mday(12), i, newdys, olddys
      LOGICAL :: npass, opass
      INTEGER :: isign, iout

      IF (odate.GT.ndate) THEN
         isign = -1
         tdate=ndate
         ndate=odate
         odate=tdate
      ELSE
         isign = 1
      END IF
      
      
      
      mday( 1) = 31
      mday( 2) = 28
      mday( 3) = 31
      mday( 4) = 30
      mday( 5) = 31
      mday( 6) = 30
      mday( 7) = 31
      mday( 8) = 31
      mday( 9) = 30
      mday(10) = 31
      mday(11) = 30
      mday(12) = 31
      
      
      
      hrold = 0
      miold = 0
      scold = 0
      olen = LEN(odate)
      
      READ(odate(1:4),  '(I4)') yrold
      READ(odate(6:7),  '(I2)') moold
      READ(odate(9:10), '(I2)') dyold
      IF (olen.GE.13) THEN
         READ(odate(12:13),'(I2)') hrold
         IF (olen.GE.16) THEN
            READ(odate(15:16),'(I2)') miold
            IF (olen.GE.19) THEN
               READ(odate(18:19),'(I2)') scold
            END IF
         END IF
      END IF
      
      
      
      hrnew = 0
      minew = 0
      scnew = 0
      nlen = LEN(ndate)

      READ(ndate(1:4),  '(I4)') yrnew
      READ(ndate(6:7),  '(I2)') monew
      READ(ndate(9:10), '(I2)') dynew
      IF (nlen.GE.13) THEN
         READ(ndate(12:13),'(I2)') hrnew
         IF (nlen.GE.16) THEN
            READ(ndate(15:16),'(I2)') minew
            IF (nlen.GE.19) THEN
               READ(ndate(18:19),'(I2)') scnew
            END IF
         END IF
      END IF
      
      
      
      npass = .true.
      opass = .true.

      iout = 0
      IF (PRESENT (pass))  &
          pass = .true.
      IF (PRESENT (iunit)) &
          iout =  iunit
      
      
      
      IF ((monew.GT.12).or.(monew.LT.1)) THEN
         WRITE (iout,'(A,A)') ' GETH_IDTS:  Month of NDATE = ', monew
         npass = .false.
      END IF
      
      
      
      IF ((moold.GT.12).or.(moold.LT.1)) THEN
         WRITE (iout,'(A,I2)') ' GETH_IDTS:  Month of ODATE = ', moold
         opass = .false.
      END IF
      
      
      
      IF (monew.ne.2) THEN
      
         IF ((dynew.GT.mday(monew)).or.(dynew.LT.1)) THEN

            npass = .false.
         END IF
      ELSE IF (monew.eq.2) THEN
      
         IF ((dynew.GT.nfeb(yrnew)).or.(dynew.LT.1)) THEN

            npass = .false.
         END IF
      END IF
      
      
      
      IF (moold.ne.2) THEN
      
         IF ((dyold.GT.mday(moold)).OR.(dyold.LT.1)) THEN

            opass = .false.
         END IF
      ELSE IF (moold.eq.2) THEN
      
         IF ((dyold.GT.nfeb(yrold)).OR.(dyold.LT.1)) THEN

            opass = .false.
         END IF
      END IF
      
      
      
      IF ((hrnew.GT.23).or.(hrnew.LT.0)) THEN

         npass = .false.
      END IF
      
      
      
      IF ((hrold.GT.23).or.(hrold.LT.0)) THEN

         opass = .false.
      END IF
      
      
      
      IF ((minew.GT.59).or.(minew.LT.0)) THEN
            WRITE (iout,'(A,I2)') ' GETH_IDTS:  Minute of NDATE = ', minew
         npass = .false.
      END IF
      
      
      
      IF ((miold.GT.59).or.(miold.LT.0)) THEN
            WRITE (iout,'(A,I2)') ' GETH_IDTS:  Minute of ODATE = ', miold
         opass = .false.
      END IF
      
      
      
      IF ((scnew.GT.59).or.(scnew.LT.0)) THEN
            WRITE (iout,'(A,I2)') ' GETH_IDTS:  SECOND of NDATE = ', scnew
         npass = .false.
      END IF
      
      
      
      IF ((scold.GT.59).or.(scold.LT.0)) THEN
            WRITE (iout,'(A,I2)') ' GETH_IDTS:  Second of ODATE = ', scold
         opass = .false.
      END IF
      
      IF (.not. npass) THEN
         IF (PRESENT (pass)) THEN
             idts = 0
             pass = .false.
             WRITE (iout,'(A,A)') 'Screwy NDATE: ', ndate(1:nlen)
             IF (isign == -1) THEN
                 tdate=ndate
                 ndate=odate
                 odate=tdate
             ENDIF
             RETURN
         ELSE
             STOP 'ndate_2'
         ENDIF
      END IF
      
      IF (.not. opass) THEN
         IF (PRESENT (pass)) THEN
             idts = 0
             pass = .false.
             WRITE (iout,'(A,A)') 'Screwy ODATE: ', odate(1:nlen)
             IF (isign == -1) THEN
                 tdate=ndate
                 ndate=odate
                 odate=tdate
             ENDIF
             RETURN
         ELSE
             STOP 'odate_1'
         ENDIF
      END IF
      
      
      
      
      
      
      
      newdys = 0
      DO i = yrold, yrnew - 1
         newdys = newdys + 365 + (nfeb(i)-28)
      END DO
      
      IF (monew .GT. 1) THEN
         mday(2) = nfeb(yrnew)
         DO i = 1, monew - 1
            newdys = newdys + mday(i)
         END DO
         mday(2) = 28
      END IF
      
      newdys = newdys + dynew-1
      
      
      
      
      olddys = 0
      
      IF (moold .GT. 1) THEN
         mday(2) = nfeb(yrold)
         DO i = 1, moold - 1
            olddys = olddys + mday(i)
         END DO
         mday(2) = 28
      END IF
      
      olddys = olddys + dyold-1
      
      
      
      idts = (newdys - olddys) * 86400
      idts = idts + (hrnew - hrold) * 3600
      idts = idts + (minew - miold) * 60
      idts = idts + (scnew - scold)
      
      IF (isign .eq. -1) THEN
         tdate=ndate
         ndate=odate
         odate=tdate
         idts = idts * isign
      END IF
   
   END SUBROUTINE geth_idts



   SUBROUTINE geth_idts_old (ndate, odate, idts)
   
      IMPLICIT NONE
      

      
      
      
      
      
      
      
      CHARACTER (LEN=*) , INTENT(INOUT) :: ndate, odate
      INTEGER           , INTENT(OUT)   :: idts
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      CHARACTER (LEN=24) :: tdate
      INTEGER :: olen, nlen
      INTEGER :: yrnew, monew, dynew, hrnew, minew, scnew
      INTEGER :: yrold, moold, dyold, hrold, miold, scold
      INTEGER :: mday(12), i, newdys, olddys
      LOGICAL :: npass, opass
      INTEGER :: isign
      
      IF (odate.GT.ndate) THEN
         isign = -1
         tdate=ndate
         ndate=odate
         odate=tdate
      ELSE
         isign = 1
      END IF
      
      
      
      mday( 1) = 31
      mday( 2) = 28
      mday( 3) = 31
      mday( 4) = 30
      mday( 5) = 31
      mday( 6) = 30
      mday( 7) = 31
      mday( 8) = 31
      mday( 9) = 30
      mday(10) = 31
      mday(11) = 30
      mday(12) = 31
      
      
      
      hrold = 0
      miold = 0
      scold = 0
      olen = LEN(odate)
      
      READ(odate(1:4),  '(I4)') yrold
      READ(odate(6:7),  '(I2)') moold
      READ(odate(9:10), '(I2)') dyold
      IF (olen.GE.13) THEN
         READ(odate(12:13),'(I2)') hrold
         IF (olen.GE.16) THEN
            READ(odate(15:16),'(I2)') miold
            IF (olen.GE.19) THEN
               READ(odate(18:19),'(I2)') scold
            END IF
         END IF
      END IF
      
      
      
      hrnew = 0
      minew = 0
      scnew = 0
      nlen = LEN(ndate)

      READ(ndate(1:4),  '(I4)') yrnew
      READ(ndate(6:7),  '(I2)') monew
      READ(ndate(9:10), '(I2)') dynew
      IF (nlen.GE.13) THEN
         READ(ndate(12:13),'(I2)') hrnew
         IF (nlen.GE.16) THEN
            READ(ndate(15:16),'(I2)') minew
            IF (nlen.GE.19) THEN
               READ(ndate(18:19),'(I2)') scnew
            END IF
         END IF
      END IF
      
      
      
      npass = .true.
      opass = .true.
      
      
      
      IF ((monew.GT.12).or.(monew.LT.1)) THEN
         PRINT*, 'GETH_IDTS:  Month of NDATE = ', monew
         npass = .false.
      END IF
      
      
      
      IF ((moold.GT.12).or.(moold.LT.1)) THEN
         PRINT*, 'GETH_IDTS:  Month of ODATE = ', moold
         opass = .false.
      END IF
      
      
      
      IF (monew.ne.2) THEN
      
         IF ((dynew.GT.mday(monew)).or.(dynew.LT.1)) THEN
            PRINT*, 'GETH_IDTS:  Day of NDATE = ', dynew
            npass = .false.
         END IF
      ELSE IF (monew.eq.2) THEN
      
         IF ((dynew.GT.nfeb(yrnew)).or.(dynew.LT.1)) THEN
            PRINT*, 'GETH_IDTS:  Day of NDATE = ', dynew
            npass = .false.
         END IF
      END IF
      
      
      
      IF (moold.ne.2) THEN
      
         IF ((dyold.GT.mday(moold)).OR.(dyold.LT.1)) THEN
            PRINT*, 'GETH_IDTS:  Day of ODATE = ', dyold
            opass = .false.
         END IF
      ELSE IF (moold.eq.2) THEN
      
         IF ((dyold.GT.nfeb(yrold)).OR.(dyold.LT.1)) THEN
            PRINT*, 'GETH_IDTS:  Day of ODATE = ', dyold
            opass = .false.
         END IF
      END IF
      
      
      
      IF ((hrnew.GT.23).or.(hrnew.LT.0)) THEN
         PRINT*, 'GETH_IDTS:  Hour of NDATE = ', hrnew
         npass = .false.
      END IF
      
      
      
      IF ((hrold.GT.23).or.(hrold.LT.0)) THEN
         PRINT*, 'GETH_IDTS:  Hour of ODATE = ', hrold
         opass = .false.
      END IF
      
      
      
      IF ((minew.GT.59).or.(minew.LT.0)) THEN
         PRINT*, 'GETH_IDTS:  Minute of NDATE = ', minew
         npass = .false.
      END IF
      
      
      
      IF ((miold.GT.59).or.(miold.LT.0)) THEN
         PRINT*, 'GETH_IDTS:  Minute of ODATE = ', miold
         opass = .false.
      END IF
      
      
      
      IF ((scnew.GT.59).or.(scnew.LT.0)) THEN
         PRINT*, 'GETH_IDTS:  SECOND of NDATE = ', scnew
         npass = .false.
      END IF
      
      
      
      IF ((scold.GT.59).or.(scold.LT.0)) THEN
         PRINT*, 'GETH_IDTS:  Second of ODATE = ', scold
         opass = .false.
      END IF
      
      IF (.not. npass) THEN
         PRINT*, 'Screwy NDATE: ', ndate(1:nlen)
         STOP 'ndate_2'
      END IF
      
      IF (.not. opass) THEN
         PRINT*, 'Screwy ODATE: ', odate(1:olen)
         STOP 'odate_1'
      END IF
      
      
      
      
      
      
      
      newdys = 0
      DO i = yrold, yrnew - 1
         newdys = newdys + 365 + (nfeb(i)-28)
      END DO
      
      IF (monew .GT. 1) THEN
         mday(2) = nfeb(yrnew)
         DO i = 1, monew - 1
            newdys = newdys + mday(i)
         END DO
         mday(2) = 28
      END IF
      
      newdys = newdys + dynew-1
      
      
      
      
      olddys = 0
      
      IF (moold .GT. 1) THEN
         mday(2) = nfeb(yrold)
         DO i = 1, moold - 1
            olddys = olddys + mday(i)
         END DO
         mday(2) = 28
      END IF
      
      olddys = olddys + dyold-1
      
      
      
      idts = (newdys - olddys) * 86400
      idts = idts + (hrnew - hrold) * 3600
      idts = idts + (minew - miold) * 60
      idts = idts + (scnew - scold)
      
      IF (isign .eq. -1) THEN
         tdate=ndate
         ndate=odate
         odate=tdate
         idts = idts * isign
      END IF
   
   END SUBROUTINE geth_idts_old



   SUBROUTINE geth_newdate (ndate, odate, idt)
   
      IMPLICIT NONE
      

      
   
      
      
   
      
      
      INTEGER , INTENT(IN)           :: idt
      CHARACTER (LEN=*) , INTENT(OUT) :: ndate
      CHARACTER (LEN=*) , INTENT(IN)  :: odate
      
       
      
       
      
      
      
      
      
      
       
      
      
      
      
      
      
       
      
      
      
      
      
      
      
      
      
      
       
      INTEGER :: nlen, olen
      INTEGER :: yrnew, monew, dynew, hrnew, minew, scnew, frnew
      INTEGER :: yrold, moold, dyold, hrold, miold, scold, frold
      INTEGER :: mday(12), nday, nhour, nmin, nsec, nfrac, i, ifrc
      LOGICAL :: opass
      CHARACTER (LEN=10) :: hfrc
      CHARACTER (LEN=1) :: sp
      
      
      
      mday( 1) = 31
      mday( 2) = 28
      mday( 3) = 31
      mday( 4) = 30
      mday( 5) = 31
      mday( 6) = 30
      mday( 7) = 31
      mday( 8) = 31
      mday( 9) = 30
      mday(10) = 31
      mday(11) = 30
      mday(12) = 31
      
      
      
      hrold = 0
      miold = 0
      scold = 0
      frold = 0
      olen = LEN(odate)
      IF (olen.GE.11) THEN
         sp = odate(11:11)
      else
         sp = ' '
      END IF
      
      
      
   
      READ(odate(1:4),  '(I4)') yrold
      READ(odate(6:7),  '(I2)') moold
      READ(odate(9:10), '(I2)') dyold
      IF (olen.GE.13) THEN
         READ(odate(12:13),'(I2)') hrold
         IF (olen.GE.16) THEN
            READ(odate(15:16),'(I2)') miold
            IF (olen.GE.19) THEN
               READ(odate(18:19),'(I2)') scold
               IF (olen.GT.20) THEN
                  READ(odate(21:olen),'(I2)') frold
               END IF
            END IF
         END IF
      END IF
      
      
      
      mday(2) = nfeb(yrold)
      
      
      
      opass = .TRUE.
      
      
      
      IF ((moold.GT.12).or.(moold.LT.1)) THEN
         WRITE(*,*) 'GETH_NEWDATE:  Month of ODATE = ', moold
         opass = .FALSE.
      END IF
      
      
      
      IF ((dyold.GT.mday(moold)).or.(dyold.LT.1)) THEN
         WRITE(*,*) 'GETH_NEWDATE:  Day of ODATE = ', dyold
         opass = .FALSE.
      END IF
      
      
      
      IF ((hrold.GT.23).or.(hrold.LT.0)) THEN
         WRITE(*,*) 'GETH_NEWDATE:  Hour of ODATE = ', hrold
         opass = .FALSE.
      END IF
      
      
      
      IF ((miold.GT.59).or.(miold.LT.0)) THEN
         WRITE(*,*) 'GETH_NEWDATE:  Minute of ODATE = ', miold
         opass = .FALSE.
      END IF
      
      
      
      IF ((scold.GT.59).or.(scold.LT.0)) THEN
         WRITE(*,*) 'GETH_NEWDATE:  Second of ODATE = ', scold
         opass = .FALSE.
      END IF
      
      
      
      

      
      
      
      IF (.not.opass) THEN
         WRITE(*,*) 'GETH_NEWDATE: Crazy ODATE: ', odate(1:olen), olen
         STOP 'odate_3'
      END IF
      
      
      
      
      
      
      IF (olen.GT.20) THEN 
         ifrc = olen-20
         ifrc = 10**ifrc
         nday   = ABS(idt)/(86400*ifrc)
         nhour  = MOD(ABS(idt),86400*ifrc)/(3600*ifrc)
         nmin   = MOD(ABS(idt),3600*ifrc)/(60*ifrc)
         nsec   = MOD(ABS(idt),60*ifrc)/(ifrc)
         nfrac = MOD(ABS(idt), ifrc)
      ELSE IF (olen.eq.19) THEN  
         ifrc = 1
         nday   = ABS(idt)/86400 
         nhour  = MOD(ABS(idt),86400)/3600
         nmin   = MOD(ABS(idt),3600)/60
         nsec   = MOD(ABS(idt),60)
         nfrac  = 0
      ELSE IF (olen.eq.16) THEN 
         ifrc = 1
         nday   = ABS(idt)/1440 
         nhour  = MOD(ABS(idt),1440)/60
         nmin   = MOD(ABS(idt),60)
         nsec   = 0
         nfrac  = 0
      ELSE IF (olen.eq.13) THEN 
         ifrc = 1
         nday   = ABS(idt)/24 
         nhour  = MOD(ABS(idt),24)
         nmin   = 0
         nsec   = 0
         nfrac  = 0
      ELSE IF (olen.eq.10) THEN 
         ifrc = 1
         nday   = ABS(idt)/24 
         nhour  = 0
         nmin   = 0
         nsec   = 0
         nfrac  = 0
      ELSE
         WRITE(*,'(''GETH_NEWDATE: Strange length for ODATE: '', i3)') &
              olen
         WRITE(*,*) odate(1:olen)
         STOP 'odate_4'
      END IF
      
      IF (idt.GE.0) THEN
      
         frnew = frold + nfrac
         IF (frnew.GE.ifrc) THEN
            frnew = frnew - ifrc
            nsec = nsec + 1
         END IF
      
         scnew = scold + nsec
         IF (scnew .GE. 60) THEN
            scnew = scnew - 60
            nmin  = nmin + 1
         END IF
      
         minew = miold + nmin
         IF (minew .GE. 60) THEN
            minew = minew - 60
            nhour  = nhour + 1
         END IF
      
         hrnew = hrold + nhour
         IF (hrnew .GE. 24) THEN
            hrnew = hrnew - 24
            nday  = nday + 1
         END IF
      
         dynew = dyold
         monew = moold
         yrnew = yrold
         DO i = 1, nday
            dynew = dynew + 1
            IF (dynew.GT.mday(monew)) THEN
               dynew = dynew - mday(monew)
               monew = monew + 1
               IF (monew .GT. 12) THEN
                  monew = 1
                  yrnew = yrnew + 1
                  
                  mday(2) = nfeb(yrnew)
               END IF
            END IF
         END DO
      
      ELSE IF (idt.LT.0) THEN
      
         frnew = frold - nfrac
         IF (frnew .LT. 0) THEN
            frnew = frnew + ifrc
            nsec = nsec - 1
         END IF
      
         scnew = scold - nsec
         IF (scnew .LT. 00) THEN
            scnew = scnew + 60
            nmin  = nmin + 1
         END IF
      
         minew = miold - nmin
         IF (minew .LT. 00) THEN
            minew = minew + 60
            nhour  = nhour + 1
         END IF
      
         hrnew = hrold - nhour
         IF (hrnew .LT. 00) THEN
            hrnew = hrnew + 24
            nday  = nday + 1
         END IF
      
         dynew = dyold
         monew = moold
         yrnew = yrold
         DO i = 1, nday
            dynew = dynew - 1
            IF (dynew.eq.0) THEN
               monew = monew - 1
               IF (monew.eq.0) THEN
                  monew = 12
                  yrnew = yrnew - 1
                  
                  mday(2) = nfeb(yrnew)
               END IF
               dynew = mday(monew)
            END IF
         END DO
      END IF
      
      
      
      nlen = LEN(ndate)
      
      IF (nlen.GT.20) THEN
         WRITE(ndate(1:19),19) yrnew, monew, dynew, hrnew, minew, scnew
         WRITE(hfrc,'(I10)') frnew+1000000000
         ndate = ndate(1:19)//'.'//hfrc(31-nlen:10)
      
      ELSE IF (nlen.eq.19.or.nlen.eq.20) THEN
         WRITE(ndate(1:19),19) yrnew, monew, dynew, hrnew, minew, scnew
      19   format(I4,'-',I2.2,'-',I2.2,'_',I2.2,':',I2.2,':',I2.2)
         IF (nlen.eq.20) ndate = ndate(1:19)//'.'
      
      ELSE IF (nlen.eq.16) THEN
         WRITE(ndate,16) yrnew, monew, dynew, hrnew, minew
      16   format(I4,'-',I2.2,'-',I2.2,'_',I2.2,':',I2.2)
      
      ELSE IF (nlen.eq.13) THEN
         WRITE(ndate,13) yrnew, monew, dynew, hrnew
      13   format(I4,'-',I2.2,'-',I2.2,'_',I2.2)
      
      ELSE IF (nlen.eq.10) THEN
         WRITE(ndate,10) yrnew, monew, dynew
      10   format(I4,'-',I2.2,'-',I2.2)
      
      END IF
      
      IF (olen.GE.11) ndate(11:11) = sp
   
   END SUBROUTINE geth_newdate



   FUNCTION nfeb ( year ) RESULT (num_days)
   
      
   
      IMPLICIT NONE
   
      INTEGER :: year
      INTEGER :: num_days
   
      num_days = 28 
      IF (MOD(year,4).eq.0) THEN  
         num_days = 29  
         IF (MOD(year,100).eq.0) THEN
            num_days = 28  
            IF (MOD(year,400).eq.0) THEN
               num_days = 29  
            END IF
         END IF
      END IF
   
   END FUNCTION nfeb




   FUNCTION nfeb_ch ( year_ch ) RESULT (num_days_ch)
   
      
   
      IMPLICIT NONE
   
      INTEGER :: year , num_days
      CHARACTER(LEN=4) :: year_ch
      CHARACTER(LEN=2) :: num_days_ch

      READ ( year_ch , '(I4.4)' ) year
   
      num_days = 28 
      IF (MOD(year,4).eq.0) THEN  
         num_days = 29  
         IF (MOD(year,100).eq.0) THEN
            num_days = 28  
            IF (MOD(year,400).eq.0) THEN
               num_days = 29  
            END IF
         END IF
      END IF

      WRITE ( num_days_ch , '(I2.2)' ) num_days
   
   END FUNCTION nfeb_ch



   SUBROUTINE split_date_char ( date , century_year , month , day , hour , minute , second )
     
      IMPLICIT NONE
   
      
   
      CHARACTER(LEN=19) , INTENT(IN) :: date 
   
      
   
      INTEGER , INTENT(OUT) :: century_year , month , day , hour , minute , second
      
      READ(date,FMT='(    I4.4)') century_year
      READ(date,FMT='( 5X,I2.2)') month
      READ(date,FMT='( 8X,I2.2)') day
      READ(date,FMT='(11X,I2.2)') hour
      READ(date,FMT='(14X,I2.2)') minute
      READ(date,FMT='(17X,I2.2)') second
   
   END SUBROUTINE split_date_char
   


      SUBROUTINE gather_date_char ( date, century_year , month , day , &
                                    hour, minute, second )

      IMPLICIT NONE

      

      INTEGER , INTENT(IN) :: century_year , month , day , &
                               hour , minute , second

      

      CHARACTER(LEN=19) , INTENT(OUT) :: date
      WRITE (DATE,FMT = '(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2)')&
      century_year,month,day,hour,minute,second

END SUBROUTINE gather_date_char



SUBROUTINE make_date ( date , time , date_time_char )





   INTEGER , INTENT(IN) :: date , &
                           time

   CHARACTER (LEN=24) , INTENT(OUT) :: date_time_char


   

   INTEGER :: year , month , day , hour , minute , second , fraction

   year = date / 10000
   month = ( date - year*10000 ) / 100
   day   =  date - year*10000 - month*100


   hour = time / 10000
   minute = ( time - hour*10000 ) / 100
   second = time - hour*10000 - minute*100

   fraction = 0

   WRITE ( date_time_char , &
   FMT = '(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2,".",I4.4) ') &
           year , month ,    day ,    hour  , minute , second , fraction

END SUBROUTINE make_date

SUBROUTINE Julian_DAY(NY,NM,ND,JD,METHOD)                                       






IMPLICIT NONE

integer, DIMENSION(12)  :: MDAY = (/31,28,31,30,31,30,31,31,30,31,30,31/)

integer,  intent(IN)    :: METHOD
integer,  intent(INOUT) :: NY, NM, ND, JD
                                                                                
integer      :: JDLEFT, JDSOFAR, LOOP
      IF(METHOD.EQ.1) THEN                                                      
         JD=0                                                                   
         IF(MOD(NY,4).EQ.0) MDAY(2)=29                                          
   JuDAY:DO LOOP=1,NM-1                                                      
          JD=JD+MDAY(LOOP)                                                    
         ENDDO JuDAY                                                               
         JD=JD+ND                                                               

      ELSE IF(METHOD.EQ.2) THEN                                                 
         IF(MOD(NY,4).EQ.0) MDAY(2)=29                                          
         NM=1                                                                   
         ND=0                                                                   
         JDLEFT=JD                                                              
         JDSOFAR=0                                                              
   NYEAR:DO LOOP=1,11                                                        
          IF(JDLEFT.GT.MDAY(LOOP)) THEN                                       
             JDLEFT=JDLEFT-MDAY(LOOP)                                         
             JDSOFAR=JDSOFAR+MDAY(LOOP)                                       
             NM=NM+1                                                          
             CYCLE NYEAR                                                          
          END IF                                                              
          EXIT NYEAR                                                             
         ENDDO NYEAR                                                               
         ND=JDLEFT                                                              
      END IF                                                                    

END subroutine Julian_DAY

subroutine Sec_to_hhmmss(hh,mm,ss,seconds,method)





  implicit none

  integer,   intent(in)    :: method
  integer,   intent(inout) :: hh, mm, ss, seconds

  integer                  :: zz

  if (method == 1) then
    seconds = hh*3600 + mm*60 + ss
  else if (method == 2) then
    hh = int(seconds/3600.)
    zz = seconds - hh*3600
    mm = int(zz/60.)
    ss = zz - mm*60
    if (mm > 58) then
      mm = 0
      ss = 0 
      hh = hh + 1
    endif
  else
    write(0,'(''Method ='',I3,'' is invalid'')') method
  endif

end subroutine Sec_to_hhmmss

SUBROUTINE get_month (pmm,cdmm)

      INTEGER             :: pmm
      CHARACTER (LEN = *) :: cdmm

      SELECT CASE (PMM)

      CASE ( 1) ;    cdmm = 'JANUARY'
      CASE ( 2) ;    cdmm = 'FEBRUARY'
      CASE ( 3) ;    cdmm = 'MARCH'
      CASE ( 4) ;    cdmm = 'APRIL'
      CASE ( 5) ;    cdmm = 'MAY'
      CASE ( 6) ;    cdmm = 'JUNE'
      CASE ( 7) ;    cdmm = 'JULY'
      CASE ( 8) ;    cdmm = 'AUGUST'
      CASE ( 9) ;    cdmm = 'SEPTEMBER'
      CASE (10) ;    cdmm = 'OCTOBER'
      CASE (11) ;    cdmm = 'NOVEMBER'
      CASE (12) ;    cdmm = 'DECEMBER'
      CASE DEFAULT ; cdmm = 'UNKNOWN'

      END SELECT

END SUBROUTINE get_month

SUBROUTINE geth_idts_2 (ndate, odate, idts, pass, iunit)
   
      IMPLICIT NONE
      

      
      
      
      
      
      
      
      
      
      CHARACTER (LEN=*) , INTENT(IN) :: ndate, odate
      INTEGER           , INTENT(OUT)   :: idts
      LOGICAL, OPTIONAL                 :: pass
      INTEGER, OPTIONAL                 :: iunit

      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      CHARACTER (LEN=24) :: tdate, lndate, lodate
      INTEGER :: olen, nlen
      INTEGER :: yrnew, monew, dynew, hrnew, minew, scnew
      INTEGER :: yrold, moold, dyold, hrold, miold, scold
      INTEGER :: mday(12), i, newdys, olddys
      LOGICAL :: npass, opass
      INTEGER :: isign, iout

      lndate = ndate
      lodate = odate
      IF (odate.GT.ndate) THEN
         isign = -1
         tdate=ndate
         lndate=lodate
         lodate=tdate
      ELSE
         isign = 1
      END IF
      
      
      
      mday( 1) = 31
      mday( 2) = 28
      mday( 3) = 31
      mday( 4) = 30
      mday( 5) = 31
      mday( 6) = 30
      mday( 7) = 31
      mday( 8) = 31
      mday( 9) = 30
      mday(10) = 31
      mday(11) = 30
      mday(12) = 31
      
      
      
      hrold = 0
      miold = 0
      scold = 0
      olen = LEN(lodate)
      
      READ(odate(1:4),  '(I4)') yrold
      READ(odate(6:7),  '(I2)') moold
      READ(odate(9:10), '(I2)') dyold
      IF (olen.GE.13) THEN
         READ(odate(12:13),'(I2)') hrold
         IF (olen.GE.16) THEN
            READ(odate(15:16),'(I2)') miold
            IF (olen.GE.19) THEN
               READ(odate(18:19),'(I2)') scold
            END IF
         END IF
      END IF
      
      
      
      hrnew = 0
      minew = 0
      scnew = 0
      nlen = LEN(lndate)

      READ(ndate(1:4),  '(I4)') yrnew
      READ(ndate(6:7),  '(I2)') monew
      READ(ndate(9:10), '(I2)') dynew
      IF (nlen.GE.13) THEN
         READ(ndate(12:13),'(I2)') hrnew
         IF (nlen.GE.16) THEN
            READ(ndate(15:16),'(I2)') minew
            IF (nlen.GE.19) THEN
               READ(ndate(18:19),'(I2)') scnew
            END IF
         END IF
      END IF
      
      
      
      npass = .true.
      opass = .true.

      iout = 0
      IF (PRESENT (pass))  &
          pass = .true.
      IF (PRESENT (iunit)) &
          iout =  iunit
      
      
      
      IF ((monew.GT.12).or.(monew.LT.1)) THEN
         WRITE (iout,'(A,A)') ' GETH_IDTS:  Month of NDATE = ', monew
         npass = .false.
      END IF
      
      
      
      IF ((moold.GT.12).or.(moold.LT.1)) THEN
         WRITE (iout,'(A,I2)') ' GETH_IDTS:  Month of ODATE = ', moold
         opass = .false.
      END IF
      
      
      
      IF (monew.ne.2) THEN
      
         IF ((dynew.GT.mday(monew)).or.(dynew.LT.1)) THEN

            npass = .false.
         END IF
      ELSE IF (monew.eq.2) THEN
      
         IF ((dynew.GT.nfeb(yrnew)).or.(dynew.LT.1)) THEN

            npass = .false.
         END IF
      END IF
      
      
      
      IF (moold.ne.2) THEN
      
         IF ((dyold.GT.mday(moold)).OR.(dyold.LT.1)) THEN

            opass = .false.
         END IF
      ELSE IF (moold.eq.2) THEN
      
         IF ((dyold.GT.nfeb(yrold)).OR.(dyold.LT.1)) THEN

            opass = .false.
         END IF
      END IF
      
      
      
      IF ((hrnew.GT.23).or.(hrnew.LT.0)) THEN

         npass = .false.
      END IF
      
      
      
      IF ((hrold.GT.23).or.(hrold.LT.0)) THEN

         opass = .false.
      END IF
      
      
      
      IF ((minew.GT.59).or.(minew.LT.0)) THEN
            WRITE (iout,'(A,I2)') ' GETH_IDTS:  Minute of NDATE = ', minew
         npass = .false.
      END IF
      
      
      
      IF ((miold.GT.59).or.(miold.LT.0)) THEN
            WRITE (iout,'(A,I2)') ' GETH_IDTS:  Minute of ODATE = ', miold
         opass = .false.
      END IF
      
      
      
      IF ((scnew.GT.59).or.(scnew.LT.0)) THEN
            WRITE (iout,'(A,I2)') ' GETH_IDTS:  SECOND of NDATE = ', scnew
         npass = .false.
      END IF
      
      
      
      IF ((scold.GT.59).or.(scold.LT.0)) THEN
            WRITE (iout,'(A,I2)') ' GETH_IDTS:  Second of ODATE = ', scold
         opass = .false.
      END IF
      
      IF (.not. npass) THEN
         IF (PRESENT (pass)) THEN
             idts = 0
             pass = .false.
             WRITE (iout,'(A,A)') 'Screwy NDATE: ', lndate(1:nlen)
             IF (isign == -1) THEN
                 tdate=ndate
                 lndate=lodate
                 lodate=tdate
             ENDIF
             RETURN
         ELSE
             STOP 'ndate_2'
         ENDIF
      END IF
      
      IF (.not. opass) THEN
         IF (PRESENT (pass)) THEN
             idts = 0
             pass = .false.
             WRITE (iout,'(A,A)') 'Screwy ODATE: ', odate(1:nlen)
             IF (isign == -1) THEN
                 tdate=lndate
                 lndate=lodate
                 lodate=tdate
             ENDIF
             RETURN
         ELSE
             STOP 'odate_1'
         ENDIF
      END IF
      
      
      
      
      
      
      
      newdys = 0
      DO i = yrold, yrnew - 1
         newdys = newdys + 365 + (nfeb(i)-28)
      END DO
      
      IF (monew .GT. 1) THEN
         mday(2) = nfeb(yrnew)
         DO i = 1, monew - 1
            newdys = newdys + mday(i)
         END DO
         mday(2) = 28
      END IF
      
      newdys = newdys + dynew-1
      
      
      
      
      olddys = 0
      
      IF (moold .GT. 1) THEN
         mday(2) = nfeb(yrold)
         DO i = 1, moold - 1
            olddys = olddys + mday(i)
         END DO
         mday(2) = 28
      END IF
      
      olddys = olddys + dyold-1
      
      
      
      idts = (newdys - olddys) * 86400
      idts = idts + (hrnew - hrold) * 3600
      idts = idts + (minew - miold) * 60
      idts = idts + (scnew - scold)
      
      IF (isign .eq. -1) THEN
         tdate=lndate
         lndate=lodate
         lodate=tdate
         idts = idts * isign
      END IF
   
   END SUBROUTINE geth_idts_2

END MODULE module_date

