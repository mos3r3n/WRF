      MODULE MODULE_ERR_NCEP






































      INTEGER    JPERR
      PARAMETER (JPERR = 6)
      REAL ERR_K (0:JPERR+1)
      REAL ERR_U (0:JPERR+1), ERR_V  (0:JPERR+1)
      REAL ERR_T (0:JPERR+1), ERR_RH (0:JPERR+1)
      REAL ERR_P (0:JPERR+1)
      REAL ERR_H (0:JPERR+1)












      DATA ERR_K  (1:JPERR) &
                 /    100000.,70000.,50000.,30000.,10000.,5000. /
      DATA ERR_U  (1:JPERR) &
                    /   1.4,   2.4,   2.8,   3.4,   2.5,  2.7   /
      DATA ERR_V  (1:JPERR) &
                    /   1.4,   2.4,   2.8,   3.4,   2.5,  2.7   /
      DATA ERR_T  (1:JPERR) &
                    /   1.8,   1.3,   1.3,   2.0,   3.1,  4.0   /
      DATA ERR_RH (1:JPERR) &
                     / 10.0,  10.0,  10.0,  10.0,  10.0,  10.0  /
      DATA ERR_P  (1:JPERR) &
                    / 100.0,  72.7,  54.5,  36.4,  18.2,  13.6  /
      DATA ERR_H  (1:JPERR) &
                    /   7.0,   8.0,   8.6,  18.8,  39.4,  59.3  /






                                                      


      real:: ntop , nbot 
      real:: aa0 , aa90 , nbot0 ,nbot90  
      real, parameter :: pmax    = 15000. , pmsl   = 100000.
      real, parameter :: ntopp   = 0.27   , nbotp0   = 3.0   , nbotp90=1.5
      REAL :: AA

       real :: erh90 ,erh0,err0,err90,hh
       real,parameter :: ha = 12000. , hb = 5500. , hc = 2500., hd=0.
       real,parameter :: ea = 0.3    , eb = 1.3   , ec = 2.5  , ed = 1.5

      CONTAINS

SUBROUTINE obs_err_ncep (nobs_max, obs, number_of_obs)



  USE module_type
  USE module_func
  USE module_intp

  IMPLICIT NONE

  INTEGER, INTENT (in)                             :: nobs_max
  TYPE (report), INTENT (inout), DIMENSION (nobs_max) :: obs
  TYPE (measurement ) , POINTER                    :: current
  INTEGER, INTENT (in)                             :: number_of_obs

  INTEGER                                          :: loop_index, is_sound
  INTEGER                                          :: nvalids, nsoundings
  INTEGER                                          :: nsurfaces, nlevels
  REAL                                             :: pres, temp, error_rh
  REAL                                             :: t9,  p9, rh9, qv9
  REAL                                             :: t,   p,  rh,  qv 
  REAL                                             :: es9, qs9
  REAL                                             :: es,  qs

  INCLUDE 'missing.inc'
  INCLUDE 'constants.inc'



  WRITE (UNIT = 0, FMT = '(A)')  &
'------------------------------------------------------------------------------'
  WRITE ( UNIT = 0, FMT = '(A,/)') '<NCEP> OBSERVATIONAL ERROR:'









      nvalids    = 0
      nsoundings = 0
      nsurfaces  = 0
      nlevels   = 0


record: DO loop_index = 1, number_of_obs





record_valid: IF (obs(loop_index)%info%discard ) THEN

      CYCLE  record

      ELSE record_valid




      nvalids = nvalids + 1 




      obs (loop_index) % ground  % slp % error = 200. 



      IF (obs (loop_index) % ground  % pw  % error .LE. 0.) THEN
        IF (obs(loop_index)%info%platform(1:6) == 'FM-114') THEN
          obs (loop_index) % ground  % pw  % error = 0.005 
        ELSE 
          obs (loop_index) % ground  % pw  % error = 0.2  
        ENDIF
        if (eps_equal(obs(loop_index)%ground %pw %data,missing_r,1.)) THEN
          obs (loop_index) % ground  % pw  % qc = missing
        else
          obs (loop_index) % ground  % pw  % qc = 1  
        endif
      ENDIF

      









      

      obs (loop_index) % ground  % tb19v % error = 1.00
      obs (loop_index) % ground  % tb19h % error = 1.00
      obs (loop_index) % ground  % tb22v % error = 2.33
      obs (loop_index) % ground  % tb37v % error = 3.66
      obs (loop_index) % ground  % tb37h % error = 3.66
      obs (loop_index) % ground  % tb85v % error = 5.00
      obs (loop_index) % ground  % tb85h % error = 5.00




      current => obs (loop_index) % surface





      is_sound   = -1

upper_level: DO WHILE (ASSOCIATED (current))





      is_sound = is_sound + 1
      nlevels  = nlevels  + 1





      IF ((eps_equal (current%meas%pressure%data, missing_r, 1.))  .OR. &
          (eps_equal (current%meas%pressure%data, 0.,        1.))) THEN

           WRITE (0,'(A,A,1X,A)') 'Internal error obs ', &
                                   TRIM (obs (loop_index) % location % id), &
                                   TRIM (obs (loop_index) % location % name)
           WRITE (0,'(A,F12.3)')  'Pressure = ', current%meas%pressure%data
           STOP                   'in obs_err_ncep.F'

      ENDIF




      pres = current%meas%pressure%data








      current % meas % direction % error = 5.






      IF (current % meas  % speed % error .LE. 0.) THEN
          current % meas % speed % error = intplin (pres, err_k (1:JPERR), &
                                                          err_u (1:JPERR))
      ENDIF




      current % meas % u % error = intplin (pres, err_k (1:JPERR), &
                                                  err_u (1:JPERR))




      current % meas % v % error = intplin (pres, err_k (1:JPERR), &
                                                  err_v (1:JPERR))



      current % meas % pressure % error = intplin (pres, err_k (1:JPERR), &
                                                         err_p (1:JPERR))


      current % meas % height   % error = intplog (pres, err_k (1:JPERR), &
                                                         err_h (1:JPERR))



      current % meas % temperature % error = intplog (pres, err_k (1:JPERR), &
                                                            err_t (1:JPERR))



      current % meas % dew_point % error = current % meas % temperature % error





      current % meas % rh % error = intplog (pres, err_k (1:JPERR), &
                                                   err_rh (1:JPERR))




   IF ((.NOT.eps_equal (current % meas % pressure % data,missing_r,1.))   .AND.&
       (.NOT.eps_equal (current % meas % temperature % data,missing_r,1.)).AND.&
       (.NOT.eps_equal (current % meas % rh % data, missing_r, 1.))) THEN



        p9  = current % meas % pressure % data  / 100 
        p   = current % meas % pressure % error / 100 
        t9  = current % meas % temperature % data
        t   = current % meas % temperature % error
        rh9 = current % meas % rh % data
        rh  = current % meas % rh % error

        es9 = 6.112 * EXP (17.67*(t9-273.15) &
                                /(t9-273.15+243.5))
        es = 6.112 &
           * EXP ( 17.67*(t9-273.15) /  (t9-273.15+243.5) ) &
           *     (+17.67* t          /  (t9-273.15+243.5)  &
                  -17.67*(t9-273.15) / ((t9-273.15+243.5)*(t9-273.15+243.5)))


        qs9 = 0.622 * es9 / (p9-es9)
        qs  = 0.622 * es  / (p9-es9) &
            - 0.622 * es9 * (p -es ) / ((p9-es9)*(p9-es9))

        qv9 = 0.01*rh9*qs9
        qv  = 0.01*rh *qs9 + 0.01*rh9 *qs

        

        current % meas % qv % error = MAX (qv, 0.001)

    ELSE

        current % meas % qv % error = missing_r

    ENDIF
    




      current => current%next

      ENDDO upper_level








      if (is_sound .gt. 0) then
          nsoundings = nsoundings + 1
      else 
          nsurfaces  = nsurfaces + 1
      endif




      ENDIF  record_valid



      ENDDO  record




 
      WRITE (UNIT = 0 , FMT = '(A,I6,A,I6,A)' ) &
     "Number of processed stations:           ",nvalids,&
     " = ",nlevels," levels."
      WRITE (UNIT = 0 , FMT = '(A,I6,A,I6,A)' ) &
     "Number of processed surface stations:   ",nsurfaces,&
     " = ",nsurfaces," surface levels."
      WRITE (UNIT = 0 , FMT = '(A,I6,A,I6,A,/)' ) &
     "Number of processed upper-air stations: ",nsoundings,&
     " = ",nlevels-nsurfaces," upper-air levels."



      RETURN

      END SUBROUTINE obs_err_ncep

      FUNCTION ERROR_INTP (PZ, CDVAR, CDTYPE)











































      REAL          PZ
      CHARACTER*(*) CDVAR, CDTYPE
      REAL ERROR_INTP
      INTEGER IK, JK
      REAL DZINF, DZSUP, DZDIF









      ERR_K  (0)       = 200000.      
      ERR_K  (JPERR+1) = 1.           

      ERR_U  (0)       = ERR_U  (1)
      ERR_U  (JPERR+1) = ERR_U  (JPERR)
      ERR_V  (0)       = ERR_V  (1)
      ERR_V  (JPERR+1) = ERR_V  (JPERR)
      ERR_T  (0)       = ERR_T  (1)
      ERR_T  (JPERR+1) = ERR_T  (JPERR)
      ERR_RH (0)       = ERR_RH (1)
      ERR_RH (JPERR+1) = ERR_RH (JPERR)
      ERR_P  (0)       = ERR_P  (1)
      ERR_P  (JPERR+1) = ERR_P  (JPERR)
      ERR_H  (0)       = ERR_H  (1)
      ERR_H  (JPERR+1) = ERR_H  (JPERR)














      IK = -1

      DO JK = 0, JPERR

      IF ((ERR_K (JK) .GE. PZ) .AND. (PZ .GT. ERR_K (JK+1))) &
      THEN
           IK = JK
           EXIT
      ENDIF

      ENDDO

      IF ((IK .LT. 0)  .OR. (IK .GT. JPERR+1)) THEN
           WRITE (0,'(/,A,A,A,F10.2,/)') ' ERROR OBS ',CDVAR, &
                                         ' POSITION PZ = ',PZ 
           STOP  ' in error_intp.F'
      ENDIF





      IF ((CDTYPE (1:5) .EQ. 'LOGAR') .OR. &
          (CDTYPE (1:5) .EQ. 'logar')) THEN

           DZINF = -LOG (PZ / ERR_K (IK  ))
           DZSUP =  LOG (PZ / ERR_K (IK+1))
           DZDIF =  LOG (ERR_K (IK) / ERR_K (IK+1))

      ELSE




           DZINF = -(PZ - ERR_K (IK  ))
           DZSUP =   PZ - ERR_K (IK+1)
           DZDIF =   ERR_K (IK) - ERR_K (IK+1)

      ENDIF




      DZINF = DZINF / DZDIF
      DZSUP = DZSUP / DZDIF

      IF (CDVAR (1:1) .EQ. 'U') THEN

           ERROR_INTP = DZSUP * ERR_U  (IK)  &
                      + DZINF * ERR_U  (IK+1)

      ELSE IF (CDVAR (1:1) .EQ. 'V') THEN

           ERROR_INTP = DZSUP * ERR_V  (IK)  &
                      + DZINF * ERR_V  (IK+1)

      ELSE IF (CDVAR (1:1) .EQ. 'T') THEN

           ERROR_INTP = DZSUP * ERR_T  (IK)  &
                      + DZINF * ERR_T  (IK+1)

      ELSE IF (CDVAR (1:2) .EQ. 'RH') THEN

           ERROR_INTP = DZSUP * ERR_RH (IK)  &
                      + DZINF * ERR_RH (IK+1)

      ELSE IF (CDVAR (1:1) .EQ. 'P') THEN

           ERROR_INTP = DZSUP * ERR_P  (IK)  &
                      + DZINF * ERR_P  (IK+1)

      ELSE IF (CDVAR (1:1) .EQ. 'H') THEN

           ERROR_INTP = DZSUP * ERR_H  (IK)  &
                      + DZINF * ERR_H  (IK+1)

      ELSE 
           WRITE (0,'(/,A,A,/)') ' ERROR VARIABLE TYPE ',CDVAR
           STOP  ' in error_intp.F'
      ENDIF





      RETURN
      END FUNCTION ERROR_INTP

      END MODULE MODULE_ERR_NCEP
