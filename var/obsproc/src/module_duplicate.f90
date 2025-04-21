MODULE module_duplicate




























USE module_type
USE module_func

CONTAINS









SUBROUTINE check_duplicate_loc(obs, index, num_obs, total_dups, time_analysis,&
                                print_duplicate)








   USE module_date
   USE module_obs_merge
   USE module_per_type

   IMPLICIT NONE

   TYPE ( report ) , INTENT ( INOUT ) , DIMENSION ( : ) :: obs
   INTEGER         , INTENT ( IN )    , DIMENSION ( : ) :: index
   INTEGER         , INTENT ( IN )                      :: num_obs 

   INTEGER                                :: current , &
                                             next    , & 
                                             first   , &
                                             second
   INTEGER         , INTENT ( OUT )       :: total_dups


   LOGICAL,              INTENT (IN)      :: print_duplicate
   CHARACTER (LEN = 19)                   :: time_analysis
   INTEGER                                :: total_valid

   INTEGER :: century_year, month, day
   INTEGER :: hour, minute, seconds
   INTEGER :: date, time
   INTEGER :: iunit, io_error

   CHARACTER (LEN =  80):: filename
   CHARACTER (LEN =  80):: proc_name = "check_duplicate_ob"
   CHARACTER (LEN = 160):: error_message
   LOGICAL              :: fatal, connected

   INTEGER              :: fma, fmb
   CHARACTER (LEN = 40) :: platforma, platformb
   INTEGER              :: nsynopb, nmetarb, nshipsb, &
                           nsoundb, npilotb, nairepb, &
                           nsatemb, nsatobb, ngpspwb, &
                           nssmt1b, nssmt2b, nssmib,  &
                           ntovsb,  notherb, namdarb, &
                           nqscatb, nproflb, ngpsepb, nbuoysb, &
                           ngpszdb, ngpsrfb, nbogusb, &
                           nairsb,  ntamdarb
   INTEGER              :: nsynopa, nmetara, nshipsa, &
                           nsounda, npilota, nairepa, &
                           nsatema, nsatoba, ngpspwa, &
                           nssmt1a, nssmt2a, nssmia,  &
                           ntovsa,  nothera, namdara, &
                           nqscata, nprofla, ngpsepa, nbuoysa, &
                           ngpszda, ngpsrfa, nbogusa, &
                           nairsa,  ntamdara

   INCLUDE 'platform_interface.inc'



   WRITE (0,'(A)')  &
'------------------------------------------------------------------------------'
   WRITE ( UNIT = 0, FMT = '(A,/)') 'REMOVE DUPLICATE STATIONS BY LOCATION:'

      

      IF (print_duplicate) THEN

      filename = 'obs_duplicate_loc.diag'
      iunit    = 999

      INQUIRE ( UNIT = iunit, OPENED = connected )

      IF (connected) CLOSE (iunit)

      OPEN (UNIT = iunit , FILE = filename , FORM = 'FORMATTED'  , &
            ACTION = 'WRITE' , STATUS = 'REPLACE', IOSTAT = io_error )

      IF (io_error .NE. 0) THEN
          CALL error_handler (proc_name, &
         "Unable to open output diagnostic file. ", filename, .TRUE.)
      ELSE
          WRITE (UNIT = 0, FMT = '(A,A,/)') &
         "Diagnostics in file ", TRIM (filename)
      ENDIF

      ENDIF

   

   nsynopb = 0; nmetarb = 0; nshipsb = 0;
   nsoundb = 0; npilotb = 0; nairepb = 0;
   nsatemb = 0; nsatobb = 0; ngpspwb = 0; 
   nssmt1b = 0; nssmt2b = 0; nssmib  = 0;
   ntovsb  = 0; notherb = 0; namdarb = 0;
   nqscatb = 0; nproflb = 0; nbuoysb = 0;
   nqscatb = 0; nproflb = 0; nbuoysb = 0;
   nairsb =0; nairsa = 0
   nsynopa = 0; nmetara = 0; nshipsa = 0;
   nsounda = 0; npilota = 0; nairepa = 0;
   nsatema = 0; nsatoba = 0; ngpspwa = 0;
   nssmt1a = 0; nssmt2a = 0; nssmia  = 0;
   ntovsa  = 0; nothera = 0; namdara = 0;
   nqscata = 0; nprofla = 0; nbuoysa = 0;
   ngpszda = 0; ngpszdb = 0; nbogusa = 0;
   ngpsrfa = 0; ngpsrfb = 0; nbogusb = 0;
   ngpsepa = 0; ngpsepb = 0
   ntamdara= 0; ntamdarb= 0
   

count_before:&
   DO current = 1 , num_obs

      first = index(current)

      

      IF ( obs(first)%info%discard ) THEN
         CYCLE count_before
      END IF

      

      READ (obs(first)  % info % platform (4:6), '(I3)') fmb

      CALL fm_decoder (fmb, platformb, &
                       synop=nsynopb, ship =nshipsb, metar=nmetarb,&
                       pilot=npilotb, sound=nsoundb, satem=nsatemb,&
                       satob=nsatobb, airep=nairepb, gpspw=ngpspwb,&
                       gpszd=ngpszdb, gpsrf=ngpsrfb, gpsep=ngpsepb,&
                       bogus=nbogusb, &
                       ssmt1=nssmt1b, ssmt2=nssmt2b, ssmi =nssmib, &
                       tovs =ntovsb,  other=notherb, amdar=namdarb,&
                       qscat=nqscatb, profl=nproflb, buoy = nbuoysb,&
                       airs=nairsb,tamdar=ntamdarb)

   ENDDO count_before

   

   CALL split_date_char (time_analysis, &
                         century_year, month, day, hour, minute, seconds )

   date = century_year * 10000 + month  * 100 + day
   time = hour         * 10000 + minute * 100 + seconds

   

   total_dups  = 0
   total_valid = 0

   

obsloop:&
   DO current = 1 , num_obs - 1

      first = index(current)

      

      IF ( obs(first)%info%discard ) THEN
         CYCLE obsloop
      END IF

      total_valid = total_valid + 1

      
      

      compare: DO next = current + 1 , num_obs

         second = index(next)

         
         



         IF (.NOT. loc_eq (obs(first), obs(second))) THEN
            CYCLE obsloop
         END IF

         

         IF (obs(second)%info%discard) THEN
            CYCLE compare
         END IF

         
         
         





         IF (.NOT. time_eq_old (obs(first)%valid_time, obs(second)%valid_time))&
         THEN

            IF (print_duplicate) THEN

            error_message  = ' Found multiple times for ' &
            // TRIM ( obs(first)%location%id ) // ' ' &
            // TRIM ( obs(first)%location%name ) // ', ' &
            // TRIM ( obs(first)%valid_time%date_char )  // ' and ' &
            // TRIM ( obs(second)%valid_time%date_char ) // '.'

            WRITE (UNIT = iunit, FMT = '(A)') TRIM (error_message)




            ENDIF

            CYCLE compare

         END IF

         

         CALL merge_obs ( obs(first) , obs(second), print_duplicate, iunit)


         

         obs(second)%info%discard  = .true.
         obs(first)%info%num_dups  = obs(first)%info%num_dups + 1
         total_dups = total_dups + 1

         
         



         NULLIFY ( obs(second)%surface ) 



      END DO compare

   END DO obsloop

   total_valid = total_valid + 1

   

count_after:&
   DO current = 1 , num_obs

       first = index(current)

      

      IF ( obs(first)%info%discard ) THEN
         CYCLE count_after
      END IF

      

      READ (obs(first)  % info % platform (4:6), '(I3)') fma

      CALL fm_decoder (fma, platforma, &
                       synop=nsynopa, ship =nshipsa, metar=nmetara,&
                       pilot=npilota, sound=nsounda, satem=nsatema,&
                       satob=nsatoba, airep=nairepa, gpspw=ngpspwa,&
                       gpszd=ngpszda, gpsrf=ngpsrfa, gpsep=ngpsepa,&
                       bogus=nbogusa, &
                       ssmt1=nssmt1a, ssmt2=nssmt2a, ssmi =nssmia, &
                       tovs =ntovsa,  other=nothera, amdar=namdara,&
                       qscat=nqscata, profl=nprofla, buoy = nbuoysa, &
                       airs=nairsa, tamdar=ntamdara)

   ENDDO count_after

   nsynops (icor) = nsynopb - nsynopa 
   nmetars (icor) = nmetarb - nmetara
   nshipss (icor) = nshipsb - nshipsa
   nsounds (icor) = nsoundb - nsounda
   namdars (icor) = namdarb - namdara
   npilots (icor) = npilotb - npilota
   naireps (icor) = nairepb - nairepa  
   ntamdar (icor) = ntamdarb- ntamdara
   nsatems (icor) = nsatemb - nsatema
   nsatobs (icor) = nsatobb - nsatoba
   ngpspws (icor) = ngpspwb - ngpspwa
   ngpsztd (icor) = ngpszdb - ngpszda
   ngpsref (icor) = ngpsrfb - ngpsrfa
   ngpseph (icor) = ngpsepb - ngpsepa
   nssmt1s (icor) = nssmt1b - nssmt1a
   nssmt2s (icor) = nssmt2b - nssmt2a
   nssmis  (icor) = nssmib  - nssmia
   ntovss  (icor) = ntovsb  - ntovsa
   nqscats (icor) = nqscatb - nqscata
   nprofls (icor) = nproflb - nprofla
   nbuoyss (icor) = nbuoysb - nbuoysa
   nboguss (icor) = nbogusb - nbogusa
   nairss  (icor) = nairsb  - nairsa 
   nothers (icor) = notherb - nothera

   WRITE (UNIT = 0 , FMT = '(A,I7,A,/)' ) &
  "Found ",total_dups," location duplicate stations that have been merged."

   IF (print_duplicate) CLOSE (iunit)

END SUBROUTINE check_duplicate_loc




SUBROUTINE check_duplicate_time (obs, index, num_obs, total_dups, time_analysis,print_duplicate)










   USE module_date
   USE module_per_type

   IMPLICIT NONE

   TYPE (report),        INTENT (INOUT), DIMENSION (:) :: obs   
   INTEGER,              INTENT (IN),    DIMENSION (:) :: index 
   INTEGER,              INTENT (IN)                   :: num_obs 
   CHARACTER (LEN = 19), INTENT (INOUT)                :: time_analysis
   INTEGER,              INTENT (OUT)                  :: total_dups
   LOGICAL,              INTENT (IN)                   :: print_duplicate
   INTEGER                                             :: total_valid

   INTEGER :: current, next, first, second
   CHARACTER (LEN = 19) :: time_first, time_second
   INTEGER :: itfirst, itsecond
   LOGICAL :: llfirst, llsecond

   TYPE (report)               :: obs_tmp
   TYPE (measurement), POINTER :: current_tmp
   LOGICAL                     :: remove_duplicate = .TRUE.

   CHARACTER (LEN = 80)        :: filename
   CHARACTER (LEN = 32 ), PARAMETER :: proc_name = 'check_duplicate_time '
   LOGICAL                     :: connected
   INTEGER                     :: iunit, io_error

   INCLUDE 'platform_interface.inc'


              WRITE (0,'(A)')  &
'------------------------------------------------------------------------------'
      WRITE ( UNIT = 0, FMT = '(A,/)') 'REMOVE DUPLICATE STATIONS BY TIME:'

      

      IF (print_duplicate) THEN

      filename = 'obs_duplicate_time.diag_'//time_analysis
      iunit    = 999

      INQUIRE ( UNIT = iunit, OPENED = connected )

      IF (connected) CLOSE (iunit)

      OPEN (UNIT = iunit , FILE = filename , FORM = 'FORMATTED'  , &
            ACTION = 'WRITE' , STATUS = 'REPLACE', IOSTAT = io_error )

      IF (io_error .NE. 0) THEN
          CALL error_handler (proc_name, &
         "Unable to open output diagnostic file. ", filename, .TRUE.)
      ELSE
          WRITE (UNIT = 0, FMT = '(A,A,/)') &
         "Diagnostics in file ", TRIM (filename)
      ENDIF

      ENDIF

   

   total_valid = 0
   total_dups  = 0

   obsloop: DO current = 1 , num_obs - 1

      first = index(current)

      

      IF ( obs(first)%info%discard ) THEN
         CYCLE obsloop
      END IF

      total_valid = total_valid + 1

      
      

      compare: DO next = current + 1 , num_obs

         second = index(next)

         
         

         IF ( .NOT. loc_eq ( obs(first) , obs(second) ) ) THEN
            CYCLE obsloop
         END IF

         

         IF ( obs(second)%info%discard ) THEN
            CYCLE compare
         END IF

         

time_difference: &
         IF (.NOT. time_eq_old (obs(first)%valid_time, obs(second)%valid_time))&
         THEN

         total_dups = total_dups + 1
         llfirst  = .FALSE.
         llsecond = .FALSE.

         IF (print_duplicate) THEN

         WRITE (UNIT = iunit, FMT = '(/,A)') 'Found duplicated stations:'

         WRITE (UNIT = iunit , FMT = '(A,2x,A,A5,A,A23,2F9.3,A,L10)') &
        'Station 1 name and ID = ' , &
         TRIM (obs(first)%info%platform),       &
         TRIM (obs(first)%location%id ) , ' ' , &
         TRIM (obs(first)%location%name ) ,     &
               obs(first)%location%latitude ,   &
               obs(first)%location%longitude, ' ',&
               obs (first)%info%is_sound

         WRITE (UNIT = iunit , FMT = '(A,2x,A,A5,A,A23,2F9.3,A,L10)') &
        'Station 2 name and ID = ' , &
         TRIM (obs(second)%info%platform),       &
         TRIM (obs(second)%location%id ) , ' ' , &
         TRIM (obs(second)%location%name ) ,     &
               obs(second)%location%latitude ,   &
               obs(second)%location%longitude,' ',&
               obs(second)%info%is_sound

         ENDIF

         

is_sound:IF (      obs (first)  % info % is_sound .AND.  &
              .NOT. obs (second) % info % is_sound) THEN

             llfirst  = .TRUE.
             llsecond = .FALSE.

         ELSE IF (.NOT. obs (first)  % info % is_sound .AND. & 
                        obs (second) % info % is_sound) THEN

             llfirst  = .FALSE.
             llsecond = .TRUE.

         ELSE is_sound

         
         
         

           WRITE (time_first, FMT='(A4,"-",A2,"-",A2,"_",A2,":",A2,":",A2)') &
            obs (first) % valid_time % date_char ( 1: 4), &
            obs (first) % valid_time % date_char ( 5: 6), &
            obs (first) % valid_time % date_char ( 7: 8), &
            obs (first) % valid_time % date_char ( 9:10), &
            obs (first) % valid_time % date_char (11:12), &
            obs (first) % valid_time % date_char (13:14)

           WRITE (time_second, FMT='(A4,"-",A2,"-",A2,"_",A2,":",A2,":",A2)') &
            obs (second) % valid_time % date_char ( 1: 4), &
            obs (second) % valid_time % date_char ( 5: 6), &
            obs (second) % valid_time % date_char ( 7: 8), &
            obs (second) % valid_time % date_char ( 9:10), &
            obs (second) % valid_time % date_char (11:12), &
            obs (second) % valid_time % date_char (13:14)

            CALL GETH_IDTS (time_first,  time_analysis, itfirst)
            CALL GETH_IDTS (time_second, time_analysis, itsecond)

            IF (print_duplicate) THEN

            WRITE (UNIT = iunit, FMT = '(2A)') 'Analysis  time = ',time_analysis

            IF (itfirst .GE. 0) THEN
            WRITE (UNIT = iunit, FMT = '(3A,I6,A)') &
                                 'Station 1 time = ',time_first, &
                                               ' = ta + ',itfirst,'s'
            ELSE
            WRITE (UNIT = iunit, FMT = '(3A,I6,A)') &
                                 'Station 1 time = ',time_first, &
                                               ' = ta - ',ABS (itfirst),'s'
            ENDIF

            IF (itsecond .GE. 0) THEN
            WRITE (UNIT = iunit, FMT = '(3A,I6,A)') &
                                 'Station 2 time = ',time_second,&
                                               ' = ta + ',itsecond,'s'
            ELSE
            WRITE (UNIT = iunit, FMT = '(3A,I6,A)') &
                                 'Station 2 time = ',time_second,&
                                               ' = ta - ',ABS (itsecond),'s'
            ENDIF

            ENDIF

            

time_equal: IF (itfirst .EQ. itsecond) THEN
                WRITE (0,'(A)')  ' Internal error:'
                WRITE (0,'(2A)') ' first_time  = ',time_first
                WRITE (0,'(2A)') ' second_time = ',time_second
                STOP ' in check_duplicate_time.F'
            ENDIF time_equal

time_different: IF (abs(itfirst) .LT. abs(itsecond)) THEN
                
                    llfirst  = .TRUE.
                    llsecond = .FALSE.
                ELSE IF (abs(itfirst) .GT. abs(itsecond)) THEN
                
                    llfirst  = .FALSE.
                    llsecond = .TRUE.
                ELSE IF (abs(itfirst) .EQ. abs(itsecond)) THEN
               
               
                    IF ( itfirst >= 0.)  THEN
                    llfirst  = .TRUE.
                    llsecond = .FALSE.
                    ELSE
                    llfirst  = .FALSE.
                    llsecond = .TRUE.
                    END IF
                ENDIF time_different

         END IF is_sound

         

         IF (remove_duplicate) THEN

         IF (llfirst) THEN

             IF (print_duplicate) THEN
              WRITE (UNIT = iunit, FMT = '(A)') &
             'Keep station 1 and reject station 2.'
             ENDIF

             READ (obs(second) % info % platform (4:6), '(I3)') fm

             CALL fm_decoder (fm, platform, &
                              synop=nsynops (icor), ship =nshipss (icor), &
                              metar=nmetars (icor), pilot=npilots (icor), &
                              sound=nsounds (icor), satem=nsatems (icor), &
                              satob=nsatobs (icor), airep=naireps (icor), &
                              gpspw=ngpspws (icor), gpszd=ngpsztd (icor), &
                              gpsrf=ngpsref (icor), gpsep=ngpseph (icor), &
                              ssmt1=nssmt1s (icor), bogus=nboguss (icor), &
                              ssmt2=nssmt2s (icor), ssmi =nssmis  (icor), &
                              tovs =ntovss  (icor), other=nothers (icor), &
                              amdar=namdars (icor), qscat=nqscats (icor), &
                              profl=nprofls (icor), buoy =nbuoyss (icor), &
                              airs =nairss (icor) , tamdar=ntamdar(icor)  )

             obs (second)%info%discard  = .true.
             obs (first)%info%num_dups  = obs (first)%info%num_dups + 1

             NULLIFY (obs(second)%surface ) 

             CYCLE compare


         ELSE IF (llsecond) THEN

             IF (print_duplicate) THEN
              Write (UNIT = iunit, FMT = '(A)') &
             'Keep station 2 and reject station 1.'
             ENDIF


             READ (obs(first) % info % platform (4:6), '(I3)') fm

             CALL fm_decoder (fm, platform, &
                              synop=nsynops (icor), ship =nshipss (icor), &
                              metar=nmetars (icor), pilot=npilots (icor), &
                              sound=nsounds (icor), satem=nsatems (icor), &
                              satob=nsatobs (icor), airep=naireps (icor), &
                              gpspw=ngpspws (icor), gpszd=ngpsztd (icor), &
                              gpsrf=ngpsref (icor), gpsep=ngpseph (icor), &
                              ssmt1=nssmt1s (icor), bogus=nboguss (icor), &
                              ssmt2=nssmt2s (icor), ssmi =nssmis  (icor), &
                              tovs =ntovss  (icor), other=nothers (icor), &
                              amdar=namdars (icor), qscat=nqscats (icor), &
                              profl=nprofls (icor), buoy =nbuoyss (icor), &
                              airs =nairss (icor),  tamdar=ntamdar(icor)  )

             obs (first)%info%discard    = .true.
             obs (second)%info%num_dups  = obs (second)%info%num_dups + 1

             NULLIFY ( obs(first)%surface ) 

             CYCLE obsloop

         ENDIF

         ELSE

         

         IF (llfirst) THEN

         

         ELSE IF (llsecond) THEN

         

              obs_tmp      = obs (second)
              obs (second) = obs (first)
              obs (first)  = obs_tmp

         ENDIF

         ENDIF

      ENDIF time_difference

       
       





      END DO compare

   END DO obsloop

   IF (print_duplicate) CLOSE (iunit)

   total_valid = total_valid + 1

   WRITE (UNIT = 0 , FMT = '(A,I7,A,/)' ) &
  "Found ",total_dups," time duplicate stations that have been removed."

END SUBROUTINE check_duplicate_time

END MODULE module_duplicate
