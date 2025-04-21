

SUBROUTINE check_obs (nobs_max, obs, number_of_obs, variable)







  USE module_type
  USE module_func
  USE module_per_type

  IMPLICIT NONE

  INTEGER, INTENT (in)                                :: nobs_max
  TYPE (report), DIMENSION (nobs_max), INTENT (inout) :: obs
  INTEGER, INTENT (in)                                :: number_of_obs
  CHARACTER (LEN = *), INTENT (in), OPTIONAL          :: variable

  TYPE (measurement), POINTER :: current
  TYPE (measurement), POINTER :: previous, temp
  INTEGER                     :: loop_index
  INTEGER                     :: nsurfaces, nuppers
  INTEGER                     :: iunit, io_error
  LOGICAL                     :: found = .FALSE.
  LOGICAL                     :: ok_miss, ok_qc, ok
  CHARACTER (LEN = 80)        :: title, fmt_found
  CHARACTER (LEN = 80)        :: filename
  CHARACTER (LEN = 32)        :: proc_name = 'check_completness: '
  LOGICAL                     :: connected
  LOGICAL                     :: print_uncomplete = .TRUE.

  INCLUDE 'missing.inc'
  INCLUDE 'platform_interface.inc'



      WRITE (0,'(A)')  &
'------------------------------------------------------------------------------'
      WRITE (UNIT = 0, FMT = '(A,/)') "CHECK PRESENCE OF HEIGHT OR/AND PRESSURE"





      IF (print_uncomplete) THEN

      IF (PRESENT (variable)) THEN
          filename = "obs_check_"//TRIM (variable)//".diag"
      ELSE
          filename = 'obs_check.diag'
      ENDIF

      iunit    = 999

      INQUIRE (UNIT = iunit, OPENED = connected)

      IF (connected) CLOSE (iunit)

      OPEN (UNIT = iunit , FILE = filename , FORM = 'FORMATTED'  , &
            ACTION = 'WRITE' , STATUS = 'REPLACE', IOSTAT = io_error )

      IF (io_error .NE. 0) THEN
          CALL error_handler (proc_name,&
         "Unable to open output diagnostic file. " , filename, .TRUE.)
      ELSE
          WRITE (UNIT = 0, FMT = '(A,A,/)') &
         "Diagnostics in file ", TRIM (filename)
      ENDIF

          WRITE (UNIT = iunit , FMT = '(A67)', ADVANCE = 'no') filename

      ENDIF








stations:&
      DO loop_index = 1, number_of_obs







      IF (obs (loop_index) % info % discard .or. &
          (obs (loop_index) % info % platform(4:6) == '111' .or. &  
           obs (loop_index) % info % platform(4:6) == '114')) THEN  

          CYCLE  stations

      ENDIF




      current => obs (loop_index) % surface




      IF (.NOT. ASSOCIATED (current)) THEN

           CYCLE stations
      ENDIF




      IF (print_uncomplete) THEN

          IF (.NOT. found) THEN
              fmt_found = '(TL67,A20,A5,1X,A23,2F9.3)'
          ELSE
              fmt_found = '(//,A20,A5,1X,A23,2F9.3)'
          ENDIF

          WRITE (UNIT = iunit , FMT = TRIM (fmt_found), ADVANCE = 'no') &
         'Found Name and ID = ' ,                                       &
          TRIM  (obs (loop_index) % location % id ) ,                   &
          TRIM  (obs (loop_index) % location % name),                   &
                 obs (loop_index) % location % latitude,                &
                 obs (loop_index) % location % longitude


          found = .TRUE.

      ENDIF







upper: &
      DO WHILE (ASSOCIATED (current))




         IF ((eps_equal (current % meas % height   % data, missing_r, 1.)).AND.&
             (eps_equal (current % meas % pressure % data, missing_r, 1.))) THEN




         IF (print_uncomplete) THEN




           READ (obs (loop_index) % info % platform (4:6), '(I3)') fm

           CALL fm_decoder (fm, platform)




           title = '...Missing pressure and height '//TRIM (platform)
           CALL PRINT_BAD (iunit, title, current)
           found = .TRUE.

           STOP 'in check_obs.F90'

         ENDIF




         ELSE &
         IF ((eps_equal (current % meas % height   % data, missing_r, 1.)) .OR.&
             (eps_equal (current % meas % pressure % data, missing_r, 1.))) THEN




         IF (print_uncomplete) THEN




           READ (obs (loop_index) % info % platform (4:6), '(I3)') fm

           CALL fm_decoder (fm, platform)




           IF (eps_equal (current % meas % height   % data, missing_r, 1.))&
           THEN

               title = '...Missing height '//TRIM (platform)

               CALL PRINT_BAD (iunit, title, current)

               found = .TRUE.

               IF (PRESENT (variable)) THEN
                   IF ((TRIM (variable) == "HEIGHT")  .OR. &
                       (TRIM (variable) == "height")) THEN
                        STOP 'in check_obs.F90'
                    ENDIF
              ENDIF
           ENDIF




           IF (eps_equal (current % meas % pressure   % data, missing_r, 1.))&
           THEN

               title = '...Missing pressure '//TRIM (platform)

               CALL PRINT_BAD (iunit, title, current)

               found = .TRUE.

               IF (PRESENT (variable)) THEN
                   IF ((TRIM (variable) == "PRESSURE")  .OR. &
                       (TRIM (variable) == "pressure")) THEN
                        STOP 'in check_obs.F90'
                    ENDIF
              ENDIF

           ENDIF

        ENDIF

      ENDIF




        current => current % next 


     ENDDO upper

     ENDDO stations

     IF (print_uncomplete) CLOSE (iunit)

END SUBROUTINE check_obs 

 SUBROUTINE print_bad (iunit, title, current)


      USE module_type

      IMPLICIT NONE

      INTEGER,              INTENT (in) :: iunit
      CHARACTER (LEN = 80), INTENT (in) :: title
      TYPE (measurement) :: current

               WRITE (UNIT = iunit, FMT = '(/,2A)', ADVANCE = 'no') &
               TRIM  (title)

               WRITE (UNIT = iunit, FMT = '(7(/,A,1X,F12.3,1X,I8,:))',&
                      ADVANCE = 'no')     &
           '   Height      = ',current % meas % height      % data,      &
                               current % meas % height      % qc,        &
           '   Pressure    = ',current % meas % pressure    % data,      &
                               current % meas % pressure    % qc













 END SUBROUTINE print_bad
