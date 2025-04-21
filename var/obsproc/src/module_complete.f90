
MODULE module_complete


























CONTAINS








SUBROUTINE check_completness (nobs_max, obs, number_of_obs, remove_above_lid, &
                              print_uncomplete)







  USE module_type
  USE module_func
  USE module_per_type

  IMPLICIT NONE

  INTEGER, INTENT (in)                                :: nobs_max
  TYPE (report), DIMENSION (nobs_max), INTENT (inout) :: obs
  INTEGER, INTENT (in)                                :: number_of_obs
  LOGICAL, INTENT (in)                                :: remove_above_lid
  LOGICAL, INTENT (in)                                :: print_uncomplete

  TYPE (measurement), POINTER :: current
  TYPE (measurement), POINTER :: previous, temp
  INTEGER                     :: loop_index
  INTEGER                     :: nsurfaces, nuppers
  INTEGER                     :: iunit, io_error
  LOGICAL                     :: found = .FALSE.
  LOGICAL                     :: ok_miss, ok_qc, ok, lpw, ltb
  CHARACTER (LEN = 80)        :: title, fmt_found
  CHARACTER (LEN = 80)        :: filename
  CHARACTER (LEN = 32)        :: proc_name = 'check_completness: '
  LOGICAL                     :: connected

  INCLUDE 'missing.inc'
  INCLUDE 'platform_interface.inc'



   WRITE (UNIT = 0, FMT = '(A)')  &
'------------------------------------------------------------------------------'
   WRITE (UNIT = 0, FMT = '(A,/)') 'LOOK FOR UNCOMPLETE DATA:'





      IF (print_uncomplete) THEN

      filename = 'obs_uncomplete.diag'
      iunit    = 999

      INQUIRE (UNIT = iunit, OPENED = connected)

      IF (connected) CLOSE (iunit)

      OPEN (UNIT = iunit , FILE = filename , FORM = 'FORMATTED'  , &
            ACTION = 'WRITE' , STATUS = 'REPLACE', IOSTAT = io_error )

      IF (io_error .NE. 0) THEN
          CALL error_handler (proc_name,&
         "Unable to open output diagnostic file. " , filename, .TRUE.)
      ELSE
          WRITE (UNIT = 0, FMT = '(A,A)') &
         "Diagnostics in file ", TRIM (filename)
      ENDIF

          WRITE (UNIT = iunit , FMT = '(A67)', ADVANCE = 'no') filename
      ENDIF








      nsurfaces = 0
      nuppers   = 0


stations: DO loop_index = 1, number_of_obs





stations_valid: IF (obs (loop_index) % info % discard ) THEN

                 CYCLE  stations

      ELSE stations_valid




      IF (.NOT. ASSOCIATED (obs (loop_index) % surface)) THEN
          CYCLE stations
      ENDIF

      READ (obs (loop_index) % info % platform (4:6), '(I3)') fm




     IF (eps_equal (obs (loop_index) % ground % pw % data, missing_r, 1.)) THEN 
         lpw =.FALSE.
     ELSE
         lpw =.TRUE.
     ENDIF




     IF ((eps_equal (obs (loop_index) % ground % tb19v % data,missing_r,1.)).AND.&
         (eps_equal (obs (loop_index) % ground % tb19h % data,missing_r,1.)).AND.&
         (eps_equal (obs (loop_index) % ground % tb22v % data,missing_r,1.)).AND.&
         (eps_equal (obs (loop_index) % ground % tb37v % data,missing_r,1.)).AND.&
         (eps_equal (obs (loop_index) % ground % tb37h % data,missing_r,1.)).AND.&
         (eps_equal (obs (loop_index) % ground % tb85v % data,missing_r,1.)).AND.&
         (eps_equal (obs (loop_index) % ground % tb85h % data,missing_r,1.))) THEN
         ltb =.FALSE.
     ELSE
         ltb =.TRUE.
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




1000  continue

      current => obs (loop_index) % surface







      ok_miss = check_level (current, missing_r)

      ok_qc = .true.
      if (remove_above_lid) ok_qc   = check_qc    (current)
      ok      = ok_miss .AND. ok_qc




single_level:&
      IF (      ASSOCIATED (current)        .AND.  &
          .NOT. ASSOCIATED (current % next) .AND. &
         (.NOT. ok_miss .OR. .NOT. ok_qc))  THEN




           READ (obs (loop_index) % info % platform (4:6), '(I3)') fm

           CALL fm_decoder (fm, platform, &
                            synop=nsynops (icor), ship =nshipss (icor), &
                            metar=nmetars (icor), pilot=npilots (icor), &
                            sound=nsounds (icor), satem=nsatems (icor), &
                            satob=nsatobs (icor), airep=naireps (icor), &
                            other=nothers (icor), gpspw=ngpspws (icor), &
                            gpszd=ngpsztd (icor), gpsrf=ngpsref (icor), &
                            amdar=namdars (icor), qscat=nqscats (icor), &
                            profl=nprofls (icor), buoy=nbuoyss  (icor), &
                            bogus=nboguss (icor), gpsep=ngpseph (icor), &
                            airs=nairss(icor),tamdar=ntamdar(icor) )




           IF (print_uncomplete) THEN

               IF (.NOT. ok_miss) THEN
                    title = '...Discard empty surface station '//TRIM (platform)
               ELSE
                    title='...Discard out of domain surface station '//TRIM (platform)
               ENDIF

               CALL PRINT_BAD (iunit, title,current)

               found = .TRUE.

           ENDIF




           DEALLOCATE (current)
           NULLIFY    (obs (loop_index) % surface)

           nuppers = nuppers + 1




           IF ((.NOT. lpw) .AND. (.NOT. ltb)) THEN
               obs (loop_index) % info % discard = .TRUE.
               nsurfaces = nsurfaces + 1
               nuppers   = nuppers - 1
           ENDIF




           CYCLE stations





      ELSE IF (ASSOCIATED (current)        .AND.  &
               ASSOCIATED (current % next) .AND.  &
              (.NOT. ok_miss .OR. .NOT. ok_qc))  THEN single_level

           IF (print_uncomplete) THEN




               READ (obs (loop_index) % info % platform (4:6), '(I3)') fm

               CALL fm_decoder (fm, platform)

               IF (.NOT. ok_miss)   THEN
                   title = '...Remove empty level '//TRIM (platform)
               ELSE
                   title = '...Remove out of domain level '//TRIM (platform)
               ENDIF

               CALL PRINT_BAD (iunit, title, current)

               found = .TRUE.

           ENDIF




           temp => obs (loop_index) % surface
           obs (loop_index) % surface => current % next

           DEALLOCATE (temp)

           nuppers = nuppers + 1

           go to 1000




      ELSE IF (ASSOCIATED (current)        .AND.  &
               ASSOCIATED (current % next)) THEN single_level




upper_levels:DO

      ok = .TRUE.




      previous => obs (loop_index) % surface
      current  => previous % next




associated_pt:&

      DO WHILE (ASSOCIATED (current))




      ok_miss = check_level (current, missing_r)

      ok_qc = .true.
      if (remove_above_lid) ok_qc   = check_qc    (current)
      ok      = ok_miss .AND. ok_qc




         IF (ok_miss .AND. ok_qc)  THEN

             previous => current
             current  => current % next




         ELSE

            IF (print_uncomplete) THEN

               READ (obs (loop_index) % info % platform (4:6), '(I3)') fm

               CALL fm_decoder (fm, platform)

               IF (.NOT. ok_miss)   THEN
                    title = '...Remove empty level '//TRIM (platform)
               ELSE
                    title = '...Remove out of domain level '//TRIM (platform)
               ENDIF

               CALL PRINT_BAD (iunit, title, current)

               found = .TRUE.

            ENDIF

            nuppers = nuppers + 1

            EXIT associated_pt

         ENDIF

      ENDDO associated_pt





      IF (.NOT. ok_miss .OR. .NOT. ok_qc)  THEN




          IF (ASSOCIATED (current % next)) THEN

              previous % next => current % next

              DEALLOCATE (current)

              CYCLE upper_levels




          ELSE

              DEALLOCATE (previous % next)
              EXIT upper_levels

          ENDIF


      ELSE




               EXIT upper_levels

      ENDIF

      ENDDO upper_levels

      ENDIF single_level







      obs (loop_index) % info % levels = info_levels (obs(loop_index)%surface)




      IF      (obs (loop_index) % info % levels .GT. 1) THEN
               obs (loop_index) % info % is_sound = .TRUE.
      ELSE IF (obs (loop_index) % info % levels .LE. 1) THEN
               obs (loop_index) % info % is_sound = .FALSE.
      ENDIF

      ENDIF stations_valid

      ENDDO stations






      IF (print_uncomplete) CLOSE (iunit)




 
      WRITE (UNIT = 0 , FMT = '(2(A,I5,A,/))' ) &
     "Remove  ",nsurfaces," surface stations.", &
     "Remove  ",nuppers,  " upper-air levels."


END SUBROUTINE CHECK_COMPLETNESS


FUNCTION check_level (current, missing_r) RESULT (ok)

  USE module_type
  USE module_func

  IMPLICIT NONE

  TYPE (measurement),   POINTER  :: current
  REAL                           :: missing_r
  LOGICAL                        :: ok


     ok  = .TRUE.  

     IF (ASSOCIATED (current)) THEN

     IF (eps_equal (current % meas % speed       % data, missing_r, 1.)  .AND.&
         eps_equal (current % meas % direction   % data, missing_r, 1.)  .AND.&
         eps_equal (current % meas % temperature % data, missing_r, 1.)  .AND.&
         eps_equal (current % meas % thickness   % data, missing_r, 1.)  .AND.&
         eps_equal (current % meas % dew_point   % data, missing_r, 1.)  .AND.&
         eps_equal (current % meas % rh          % data, missing_r, 1.) .or.  &
         (current % meas % pressure % qc < 0 .and.                            &
          current % meas % height   % qc < 0) ) THEN

         ok = .FALSE.

      ENDIF

      ENDIF

END FUNCTION check_level


FUNCTION check_qc (current) RESULT (ok)

  USE module_type
  USE module_func

  IMPLICIT NONE

  TYPE (measurement),   POINTER  :: current
  LOGICAL                        :: ok

  INCLUDE 'missing.inc'


     ok  = .TRUE.  

     IF (ASSOCIATED (current)) THEN

     IF (current % meas % height % qc .GE. above_model_lid .or. &
         current % meas %pressure% qc .GE. above_model_lid) THEN

         ok = .FALSE.

      ENDIF

      ENDIF

END FUNCTION check_qc

END MODULE module_complete
