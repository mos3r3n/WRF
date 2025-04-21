
MODULE module_decoded






























   USE module_type
   use module_stntbl





   USE module_func





  USE module_inside
  use module_namelist, only: gts_from_mmm_archive, calc_psfc_from_QNH





   INCLUDE 'missing.inc'



   INTEGER , PARAMETER                            ::  ok       = 0 , &
                                                      eof_err  = 1 , &
                                                      no_data  = 2 , &
                                                      read_err = 3 , &




                                                      ssmi_qc_index = 0








   CHARACTER ( LEN = 120 ) , PARAMETER :: rpt_format =  &
                ' (2F20.5 , 2A40 , '              & 
             // ' 2A40 , 1F20.5 , 5I10 , 3L10 , ' & 
             // ' 2I10 , A20 , '                  & 
             // ' 13(F13.5 , I7),'                & 
             // '1(:,F13.5 , I7),'                & 
             // '7(:,F13.5 , I7))'                  

   CHARACTER ( LEN = 120 ) , PARAMETER :: meas_format = &
                ' ( 10( F13.5 , I7 ) ) '            

   CHARACTER ( LEN = 120 ) , PARAMETER :: end_format = &
                ' ( 3 ( I7 ) ) '                    

   INTEGER       :: N_air = 0, N_air_cut = 0





CONTAINS












SUBROUTINE read_obs_gts ( file_name,  obs , n_obs , &
total_number_of_obs , fatal_if_exceed_max_obs , print_gts_read , &
ins , jew , &
time_window_min, time_window_max, map_projection , missing_flag)






   USE module_date
   USE module_per_type

   IMPLICIT NONE

   CHARACTER ( LEN = * ) , INTENT ( IN )          :: file_name
   INTEGER, INTENT (INOUT)                        :: n_obs
   INTEGER, INTENT ( IN )                         :: total_number_of_obs
   LOGICAL, INTENT ( IN )                         :: fatal_if_exceed_max_obs
   LOGICAL, INTENT ( IN )                         :: print_gts_read
   TYPE (report), DIMENSION (:), INTENT (OUT)     :: obs
   INTEGER, INTENT ( IN )                         :: ins, jew

   CHARACTER (LEN = 19) , INTENT (IN)             :: time_window_min
   CHARACTER (LEN = 19) , INTENT (IN)             :: time_window_max
   INTEGER                                        :: map_projection
   REAL,    INTENT (OUT)                          :: missing_flag

   INTEGER                              :: file_num
   CHARACTER ( LEN = 32 ) , PARAMETER   :: proc_name = 'read_obs_gts '
   INTEGER                              :: io_error, platform_error
   INTEGER                              :: obs_num
   INTEGER                              :: error_ret

   INTEGER                              :: num_empty , num_outside , bad_count
   LOGICAL                              :: outside_domain, outside_window
   LOGICAL                              :: outside

   TYPE(meas_data)                      :: dummy_middle

   INTEGER                              :: icrs, k, iunit, levels
   CHARACTER ( LEN =  80)               :: filename
   CHARACTER ( LEN = 160)               :: error_message
   CHARACTER ( LEN =  14)               :: newstring
   INTEGER                              :: nlevels, num_unknown, m_miss
   TYPE ( measurement ) , POINTER       :: current
   real :: qnh, elev
   real :: xla, xlo
   integer :: ipos

  INCLUDE 'platform_interface.inc'









             WRITE (UNIT = 0, FMT = '(A)')  &
'------------------------------------------------------------------------------'

      WRITE (UNIT = 0, FMT = '(A,A,/)') &
    'READ GTS OBSERVATIONS IN FILE ', TRIM (file_name)

   
   
   

   num_unknown = 0
   missing_flag = missing_r
   num_empty = 0
   num_outside = 0
   m_miss = 0

   
   IF (print_gts_read) THEN

        filename = 'obs_gts_read.diag'
        iunit    = 999

        OPEN ( UNIT = iunit , FILE = filename , FORM = 'FORMATTED'  , &
               ACTION = 'WRITE' , STATUS = 'REPLACE', IOSTAT = io_error ) 

        IF ( io_error .NE. 0 ) THEN
             CALL error_handler (proc_name, &
            'Unable to open output diagnostic file:', filename, .true.)
        ELSE
             WRITE (UNIT = 0, FMT = '(A,A,/)') &
            "Diagnostics in file ", TRIM (filename)
        ENDIF

   ENDIF

   
   

   file_num = 99
   OPEN ( UNIT = file_num , FILE = file_name , FORM = 'FORMATTED'  , &
          ACTION = 'READ' , IOSTAT = io_error ) 

   IF ( io_error .NE. 0 ) THEN
      CALL error_handler (proc_name, & 
             'Unable to open gts input observations file: ',file_name, .true.)
   ENDIF




      CALL GETENV('OBSPROC_TEST_DATE',newstring)

   


   obs_num = n_obs + 1

   read_obs : DO while ( io_error == 0 ) 
      

      IF (obs_num .GT. total_number_of_obs) THEN

            error_message(1:60)  = &
           'Too many obs for the NAMELIST value of max_number_of_obs = '

            WRITE (error_message(61:67),'(I7)')  total_number_of_obs

            CALL error_handler (proc_name, error_message (1:60), &
                 error_message(61:67),fatal_if_exceed_max_obs)
            
            
            

            CLOSE ( file_num ) 
            IF (print_gts_read) CLOSE ( iunit ) 
            EXIT read_obs

      END IF

      
      
      obs(obs_num)%ground%pw%data = missing_r

      

      READ ( file_num , IOSTAT = io_error , FMT = rpt_format ) &
      obs(obs_num)%location % latitude,     obs(obs_num)%location % longitude, &
      obs(obs_num)%location % id,           obs(obs_num)%location % name,      &
      obs(obs_num)%info % platform,         obs(obs_num)%info % source,        &
      obs(obs_num)%info % elevation,        obs(obs_num)%info % num_vld_fld,   &
      obs(obs_num)%info % num_error,        obs(obs_num)%info % num_warning,   &
      obs(obs_num)%info % seq_num,          obs(obs_num)%info % num_dups,      &
      obs(obs_num)%info % is_sound,         obs(obs_num)%info % bogus,         &
      obs(obs_num)%info % discard,                                             &
      obs(obs_num)%valid_time % sut,        obs(obs_num)%valid_time % julian,  &
      obs(obs_num)%valid_time % date_char,                                     &
      obs(obs_num)%ground%slp%data,         obs(obs_num)%ground%slp%qc,        &
      obs(obs_num)%ground%ref_pres%data,    obs(obs_num)%ground%ref_pres%qc,   &
      obs(obs_num)%ground%ground_t%data,    obs(obs_num)%ground%ground_t%qc,   &
      obs(obs_num)%ground%sst%data,         obs(obs_num)%ground%sst%qc,        &
      obs(obs_num)%ground%psfc%data,        obs(obs_num)%ground%psfc%qc,       &
      obs(obs_num)%ground%precip%data,      obs(obs_num)%ground%precip%qc,     &
      obs(obs_num)%ground%t_max%data,       obs(obs_num)%ground%t_max%qc,      &
      obs(obs_num)%ground%t_min%data,       obs(obs_num)%ground%t_min%qc,      &
      obs(obs_num)%ground%t_min_night%data, obs(obs_num)%ground%t_min_night%qc,&
      obs(obs_num)%ground%p_tend03%data,    obs(obs_num)%ground%p_tend03%qc,   &
      obs(obs_num)%ground%p_tend24%data,    obs(obs_num)%ground%p_tend24%qc,   &
      obs(obs_num)%ground%cloud_cvr%data,   obs(obs_num)%ground%cloud_cvr%qc,  &
      obs(obs_num)%ground%ceiling%data,     obs(obs_num)%ground%ceiling%qc,    &
      obs(obs_num)%ground%pw     %data,     obs(obs_num)%ground%pw%qc,         &
      obs(obs_num)%ground%tb19v  %data,     obs(obs_num)%ground%tb19v%qc,      &
      obs(obs_num)%ground%tb19h  %data,     obs(obs_num)%ground%tb19h%qc,      &
      obs(obs_num)%ground%tb22v  %data,     obs(obs_num)%ground%tb22v%qc,      &
      obs(obs_num)%ground%tb37v  %data,     obs(obs_num)%ground%tb37v%qc,      &
      obs(obs_num)%ground%tb37h  %data,     obs(obs_num)%ground%tb37h%qc,      &
      obs(obs_num)%ground%tb85v  %data,     obs(obs_num)%ground%tb85v%qc,      &
      obs(obs_num)%ground%tb85h  %data,     obs(obs_num)%ground%tb85h%qc

      if ( io_error < 0 ) then 
         write(unit=0, fmt='(A)') 'Have reached the end of observation file.'
         close(file_num)
         if ( print_gts_read ) close(iunit)
         exit read_obs
      end if



      if (len_trim(newstring) .ne. 0 ) then
        obs(obs_num)%valid_time%date_char = newstring 
      endif









      if (obs(obs_num)%info % platform(1:12) == 'AWS SURFACE ') then
          obs(obs_num)%info % platform(1:12) =  'FM-16 AWSSFC'
      endif
      if (obs(obs_num)%info % platform(1:14) == 'FM-32 PROFILER') then
          obs(obs_num)%info % platform(1:14) =  'FM-132 PROFILER'
      endif


     if(index(obs(obs_num)%location%id,   'MODIS') > 0 .or. &
        index(obs(obs_num)%location%id,   'modis') > 0 .or. &
        index(obs(obs_num)%info%source,   'MODIS') > 0 .or. &
        index(obs(obs_num)%info%source,   'modis') > 0      ) then

      if( index(obs(obs_num)%info%platform(1:11), 'FM-88') > 0 ) then
        obs(obs_num)%info%platform(1:11) = 'FM-88 MODIS'                             
        
        obs(obs_num)%location%name       =  obs(obs_num)%location%id
      end if

     end if


      if ( gts_from_mmm_archive ) then
         
         
         
         if ( obs(obs_num)%info%platform(1:10) == 'FM-13 SHIP' ) then
            if ( index(obs(obs_num)%location%name, 'Platform Id >>>') > 0 ) then
               obs(obs_num)%info%platform(1:10) = 'FM-18 BUOY'
               obs(obs_num)%location%id = obs(obs_num)%location%name(17:21)
            end if
         end if
      end if

      call print_extra_obs



         if (IO_error > 0 ) then  
           write(0,'("IO_ERROR=",i2,1x,i7,1x,a,2(f9.3,a),1x,a,1x,f11.3)') &
                             io_error, obs_num, obs(obs_num)%info % platform, &
                                          obs(obs_num)%location%latitude, 'N',&
                                          obs(obs_num)%location%longitude,'E ', &
                                          obs(obs_num)%valid_time%date_char,    &
                                          obs(obs_num)%info % elevation

            
            obs(obs_num)%info % discard = .true.
         endif

         READ (obs(obs_num) % info % platform (4:6), '(I3)', &
                                          IOSTAT = platform_error) fm
         if (platform_error /= 0) then
            write(0,'(A)') &
              "***WARNING: NO WMO CODE FOR THIS NEW TYPE OF OBS***"
            write(0,'(A,I8,A,I4,A,A,A,A)') " ==> obs_num=",obs_num, &
               " platform_error=",platform_error," platform=",      &
               obs(obs_num) % info % platform (1:12), " ID=",       &
               obs(obs_num) % location % id(1:6)
         else if (fm <= 0) then
            write(0,'(A,I8,A,A,A,A)') " ==> obs_num=",obs_num, &
               " INVALID WMO CODE: platform=",      &
               obs(obs_num) % info % platform (1:12), " ID=",       &
               obs(obs_num) % location % id(1:6)
            platform_error = -99


         endif



      

      if ( print_gts_read ) then
      WRITE (UNIT = iunit , FMT = '(A,1X,A,1X,A,1X,A,1X,2(F8.3,A),A,1X)',&
             ADVANCE='no') 'Found' ,           &
             obs(obs_num)%location%id   (1: 5),&
             obs(obs_num)%location%name (1:20),&
             obs(obs_num)%info%platform (1: 12),&
             obs(obs_num)%location%latitude, 'N',&
             obs(obs_num)%location%longitude,'E ', &
             obs(obs_num)%valid_time%date_char
      end if


      IF (IPROJ > 0) THEN
         if (truelat1 > 0.0) then
            if (obs(obs_num)%location%latitude == -90.0) then
              write(0,'(/i7,1x,"modified the original lat =",f8.2," to -89.5"/)') &
                     obs_num, obs(obs_num)%location%latitude  
                     obs(obs_num)%location%latitude = -89.5
            endif
         else if (truelat1 < 0.0) then
            if (obs(obs_num)%location%latitude == 90.0) then
              write(0,'(/i7,1x,"modified the original lat =",f8.2," to  89.5"/)') &
                     obs_num, obs(obs_num)%location%latitude  
                     obs(obs_num)%location%latitude =  89.5
                 endif
         endif
      ENDIF
 


      IF (obs(obs_num)%ground%pw%data .LE. 0.) THEN
          obs(obs_num)%ground%pw%data  = missing_r
          obs(obs_num)%ground%pw%qc    = missing
          obs(obs_num)%ground%pw%error = missing_r
      ELSE
         
         
         
         
         
         
         
         
         
         
         
         

          IF (fm == 111 .or. fm == 114) THEN    
             obs(obs_num)%ground%pw%error =    &
                         REAL(obs(obs_num)%ground%pw%qc)/100. 
          ELSE
             obs(obs_num)%ground%pw%error = missing_r
          ENDIF
          obs(obs_num)%ground%pw%qc    = 0
      ENDIF

      




      

      IF ((obs(obs_num)%ground%tb19v%data .LE. 0.) .OR. &
          (obs(obs_num)%ground%tb19v%qc   .NE. ssmi_qc_index)) THEN
           obs(obs_num)%ground%tb19v%data  = missing_r
           obs(obs_num)%ground%tb19v%qc    = missing
           obs(obs_num)%ground%tb19v%error = missing_r
      ELSE
           obs(obs_num)%ground%tb19v%error = missing_r
           obs(obs_num)%ground%tb19v%qc    = 0
      ENDIF

      IF ((obs(obs_num)%ground%tb19h%data .LE. 0.) .OR. &
          (obs(obs_num)%ground%tb19h%qc   .NE. ssmi_qc_index)) THEN
           obs(obs_num)%ground%tb19h%data  = missing_r
           obs(obs_num)%ground%tb19h%qc    = missing
           obs(obs_num)%ground%tb19h%error = missing_r
      ELSE
           obs(obs_num)%ground%tb19h%error = missing_r
           obs(obs_num)%ground%tb19h%qc    = 0
      ENDIF

      IF ((obs(obs_num)%ground%tb22v%data .LE. 0.) .OR. &
          (obs(obs_num)%ground%tb22v%qc   .NE. ssmi_qc_index)) THEN
           obs(obs_num)%ground%tb22v%data  = missing_r
           obs(obs_num)%ground%tb22v%qc    = missing
           obs(obs_num)%ground%tb22v%error = missing_r
      ELSE
           obs(obs_num)%ground%tb22v%error = missing_r
           obs(obs_num)%ground%tb22v%qc    = 0
      ENDIF

      IF ((obs(obs_num)%ground%tb37v%data .LE. 0.) .OR. &
          (obs(obs_num)%ground%tb37v%qc   .NE. ssmi_qc_index)) THEN
           obs(obs_num)%ground%tb37v%data  = missing_r
           obs(obs_num)%ground%tb37v%qc    = missing
           obs(obs_num)%ground%tb37v%error = missing_r
      ELSE
           obs(obs_num)%ground%tb37v%error = missing_r
           obs(obs_num)%ground%tb37v%qc    = 0
      ENDIF

      IF ((obs(obs_num)%ground%tb37h%data .LE. 0.) .OR. &
          (obs(obs_num)%ground%tb37h%qc   .NE. ssmi_qc_index)) THEN
           obs(obs_num)%ground%tb37h%data  = missing_r
           obs(obs_num)%ground%tb37h%qc    = missing
           obs(obs_num)%ground%tb37h%error = missing_r
      ELSE
           obs(obs_num)%ground%tb37h%error = missing_r
           obs(obs_num)%ground%tb37h%qc    = 0
      ENDIF

      IF ((obs(obs_num)%ground%tb85v%data .LE. 0.) .OR. &
          (obs(obs_num)%ground%tb85v%qc   .NE. ssmi_qc_index)) THEN
           obs(obs_num)%ground%tb85v%data  = missing_r
           obs(obs_num)%ground%tb85v%qc    = missing
           obs(obs_num)%ground%tb85v%error = missing_r
      ELSE
           obs(obs_num)%ground%tb85v%error = missing_r
           obs(obs_num)%ground%tb85v%qc    = 0
      ENDIF

      IF ((obs(obs_num)%ground%tb85h%data .LE. 0.) .OR. &
          (obs(obs_num)%ground%tb85h%qc   .NE. ssmi_qc_index)) THEN
           obs(obs_num)%ground%tb85h%data  = missing_r
           obs(obs_num)%ground%tb85h%qc    = missing
           obs(obs_num)%ground%tb85h%error = missing_r
      ELSE
           obs(obs_num)%ground%tb85h%error = missing_r
           obs(obs_num)%ground%tb85h%qc    = 0
      ENDIF

      

      obs(obs_num)%location%yic = missing_r
      obs(obs_num)%location%yid = missing_r
      obs(obs_num)%location%xjc = missing_r
      obs(obs_num)%location%xjd = missing_r

      

      obs (obs_num) % info % levels = 0

      
      
      

      IF ( io_error .GT. 0 .or. platform_error /= 0) THEN

         WRITE (UNIT = 0, FMT = '(A,A,A,A)') 'Troubles with first line ', &
              TRIM ( obs(obs_num)%location%id ) , &
         ' ', TRIM ( obs(obs_num)%location%name ) 

         

         bad_count = 0

         DO WHILE ( io_error .GE. 0 )

         bad_count = bad_count + 1

         IF     (bad_count .LT. 1000 ) THEN

           READ (file_num, IOSTAT = io_error, FMT = meas_format)               &
           dummy_middle % pressure    % data, dummy_middle % pressure    % qc, &
           dummy_middle % height      % data, dummy_middle % height      % qc, &
           dummy_middle % temperature % data, dummy_middle % temperature % qc, &
           dummy_middle % dew_point   % data, dummy_middle % dew_point   % qc, &
           dummy_middle % speed       % data, dummy_middle % speed       % qc, &
           dummy_middle % direction   % data, dummy_middle % direction   % qc, &
           dummy_middle % u           % data, dummy_middle % u           % qc, &
           dummy_middle % v           % data, dummy_middle % v           % qc, &
           dummy_middle % rh          % data, dummy_middle % rh          % qc, &
           dummy_middle % thickness   % data, dummy_middle % thickness   % qc

           IF (eps_equal (dummy_middle%pressure%data, end_data_r , 1.)) THEN
               READ (file_num , IOSTAT = io_error , FMT = end_format ) &
                     obs(obs_num)%info%num_vld_fld , &
                     obs(obs_num)%info%num_error , &  
                     obs(obs_num)%info%num_warning    
                WRITE (UNIT = 0, FMT = '(A)') 'Starting to READ a new report.'
                CYCLE read_obs
           END IF

         ELSE

              WRITE (UNIT = 0, FMT = '(A)')&
             'Too many attempts to read the data correctly.  Exiting read loop.'
              CLOSE ( file_num ) 
              IF (print_gts_read) CLOSE ( iunit ) 
              EXIT read_obs

         END IF

         END DO 

         
         

         IF ( io_error .LT. 0 ) THEN

            WRITE (UNIT = 0, FMT='(A)') 'Have reached end of observations file.'
            CLOSE ( file_num ) 
            IF (print_gts_read) CLOSE ( iunit ) 
            EXIT read_obs

         END IF 

      
      

      



      
      
      

      ELSE IF ( io_error .EQ. 0 ) THEN 

         IF ( domain_check_h ) then

           CALL inside_domain (obs(obs_num)%location%latitude  , &
                               obs(obs_num)%location%longitude , &
                               ins , jew , outside_domain , &
                               obs(obs_num)%location%xjc, &
                               obs(obs_num)%location%yic, &
                               obs(obs_num)%location%xjd, &
                               obs(obs_num)%location%yid)
         else
           outside_domain = .FALSE.
         endif
          
         
         
         
         
         

         
         
         

         IF      (INDEX (obs(obs_num)%valid_time%date_char , '*' ) .GT. 0 ) THEN
                     obs(obs_num)%valid_time%date_char = '19000101000000'
         ELSE IF  ((obs(obs_num)%valid_time%date_char( 1: 2) .NE. '19' ) .AND. &
                   (obs(obs_num)%valid_time%date_char( 1: 2) .NE. '20' ) ) THEN
                    obs(obs_num)%valid_time%date_char = '19000101000000'
         ELSE IF  ((obs(obs_num)%valid_time%date_char( 5: 6) .LT. '01' ) .OR.  &
                   (obs(obs_num)%valid_time%date_char( 5: 6) .GT. '12' )) THEN
                    obs(obs_num)%valid_time%date_char = '19000101000000'
         ELSE IF  ((obs(obs_num)%valid_time%date_char( 7: 8) .LT. '01' ) .OR.  &
                   (obs(obs_num)%valid_time%date_char( 7: 8) .GT. '31' )) THEN
                    obs(obs_num)%valid_time%date_char = '19000101000000'
         ELSE IF  ((obs(obs_num)%valid_time%date_char( 9:10) .LT. '00' ) .OR.  &
                   (obs(obs_num)%valid_time%date_char( 9:10) .GT. '23' )) THEN
                    obs(obs_num)%valid_time%date_char = '19000101000000'
         ELSE IF  ((obs(obs_num)%valid_time%date_char(11:12) .LT. '00' ) .OR.  &
                   (obs(obs_num)%valid_time%date_char(11:12) .GT. '59' )) THEN
                    obs(obs_num)%valid_time%date_char = '19000101000000'
         ELSE IF  ((obs(obs_num)%valid_time%date_char(13:14) .LT. '00' ) .OR.  &
                   (obs(obs_num)%valid_time%date_char(13:14) .GT. '59' )) THEN
                    obs(obs_num)%valid_time%date_char = '19000101000000'
         ELSE IF (((obs(obs_num)%valid_time%date_char( 5: 6) .EQ. '04' ) .OR.  &
                   (obs(obs_num)%valid_time%date_char( 5: 6) .EQ. '06' ) .OR.  &
                   (obs(obs_num)%valid_time%date_char( 5: 6) .EQ. '09' ) .OR.  &
                   (obs(obs_num)%valid_time%date_char( 5: 6) .EQ. '11' )) .AND.&
                   (obs(obs_num)%valid_time%date_char( 7: 8) .GT. '30' ) ) THEN
                    obs(obs_num)%valid_time%date_char = '19000101000000'
         ELSE IF  ((obs(obs_num)%valid_time%date_char( 5: 6) .EQ. '02' ) .AND. &
                   (nfeb_ch ( obs(obs_num)%valid_time%date_char( 1: 4) ) .LT.  &
                    obs(obs_num)%valid_time%date_char( 7: 8) ) ) THEN
                    obs(obs_num)%valid_time%date_char = '19000101000000'
         END IF

         CALL inside_window (obs(obs_num)%valid_time%date_char, &
                              time_window_min, time_window_max, &
                              outside_window, iunit)

         outside = outside_domain .OR. outside_window
    
         

         WRITE (obs(obs_num)%valid_time%date_mm5, &
                FMT='(A4,"-",A2,"-",A2,"_",A2,":",A2,":",A2)') &
                obs(obs_num)%valid_time%date_char ( 1: 4),     &
                obs(obs_num)%valid_time%date_char ( 5: 6),     &
                obs(obs_num)%valid_time%date_char ( 7: 8),     &
                obs(obs_num)%valid_time%date_char ( 9:10),     &
                obs(obs_num)%valid_time%date_char (11:12),     &
                obs(obs_num)%valid_time%date_char (13:14)

         
         
         

         IF ((obs(obs_num)%ground%slp%data .GT. (undefined1_r - 1.))  .OR.     &
             (obs(obs_num)%ground%slp%data .LT. (undefined2_r + 1.))) THEN
              obs(obs_num)%ground%slp%data  = missing_r
              obs(obs_num)%ground%slp%qc    = missing
         END IF
         IF ((obs(obs_num)%ground%ref_pres%data .GT. (undefined1_r - 1.)) .OR. &
             (obs(obs_num)%ground%ref_pres%data .LT. (undefined2_r + 1.))) THEN
              obs(obs_num)%ground%ref_pres%data = missing_r
              obs(obs_num)%ground%ref_pres%qc   = missing
         END IF
         IF ((obs(obs_num)%ground%ground_t%data .GT. (undefined1_r - 1.)) .OR. &
             (obs(obs_num)%ground%ground_t%data .LT. (undefined2_r + 1.))) THEN
              obs(obs_num)%ground%ground_t%data = missing_r
              obs(obs_num)%ground%ground_t%qc   = missing
         END IF
         IF ((obs(obs_num)%ground%sst%data .GT. (undefined1_r - 1.))  .OR.     &
             (obs(obs_num)%ground%sst%data .LT. (undefined2_r + 1.))) THEN
              obs(obs_num)%ground%sst%data = missing_r
              obs(obs_num)%ground%sst%qc   = missing
         END IF
         IF ((obs(obs_num)%ground%psfc%data .GT. (undefined1_r - 1.))  .OR.    &
             (obs(obs_num)%ground%psfc%data .LT. (undefined2_r + 1.))) THEN 
              obs(obs_num)%ground%psfc%data = missing_r
              obs(obs_num)%ground%psfc%qc   = missing
         END IF
         IF ((obs(obs_num)%ground%precip%data .GT. (undefined1_r - 1.))  .OR.  &
             (obs(obs_num)%ground%precip%data .LT. (undefined2_r + 1.))) THEN
              obs(obs_num)%ground%precip%data = missing_r
              obs(obs_num)%ground%precip%qc   = missing
         END IF
         IF ((obs(obs_num)%ground%t_max%data .GT. (undefined1_r - 1.))  .OR.   &
             (obs(obs_num)%ground%t_max%data .LT. (undefined2_r + 1.))) THEN
              obs(obs_num)%ground%t_max%data = missing_r
              obs(obs_num)%ground%t_max%qc   = missing
         END IF
         IF ((obs(obs_num)%ground%t_min%data .GT. (undefined1_r - 1.))  .OR.   &
             (obs(obs_num)%ground%t_min%data .LT. (undefined2_r + 1.))) THEN
              obs(obs_num)%ground%t_min%data = missing_r
              obs(obs_num)%ground%t_min%qc   = missing
         END IF
         IF ((obs(obs_num)%ground%t_min_night%data .GT. (undefined1_r - 1.))   &
        .OR. (obs(obs_num)%ground%t_min_night%data .LT. (undefined2_r + 1.)))  &
        THEN
              obs(obs_num)%ground%t_min_night%data = missing_r
              obs(obs_num)%ground%t_min_night%qc   = missing
         END IF
         IF ((obs(obs_num)%ground%p_tend03%data .GT. (undefined1_r - 1.)) .OR. &
             (obs(obs_num)%ground%p_tend03%data .LT. (undefined2_r + 1.))) THEN
              obs(obs_num)%ground%p_tend03%data = missing_r
              obs(obs_num)%ground%p_tend03%qc   = missing
         END IF
         IF ((obs(obs_num)%ground%p_tend24%data .GT. (undefined1_r - 1.)) .OR. &
             (obs(obs_num)%ground%p_tend24%data .LT. (undefined2_r + 1.))) THEN
              obs(obs_num)%ground%p_tend24%data = missing_r
              obs(obs_num)%ground%p_tend24%qc   = missing
         END IF
         IF ((obs(obs_num)%ground%cloud_cvr%data .GT. (undefined1_r - 1.)) .OR.&
             (obs(obs_num)%ground%cloud_cvr%data .LT. (undefined2_r + 1.))) THEN
              obs(obs_num)%ground%cloud_cvr%data = missing_r
              obs(obs_num)%ground%cloud_cvr%qc   = missing
         END IF
         IF ((obs(obs_num)%ground%ceiling%data .GT. (undefined1_r - 1.)) .OR.  &
             (obs(obs_num)%ground%ceiling%data .LT. (undefined2_r + 1.))) THEN
              obs(obs_num)%ground%ceiling%data = missing_r
              obs(obs_num)%ground%ceiling%qc   = missing
         END IF

         

         IF (print_gts_read) THEN
             IF (outside_domain) THEN
                 WRITE (UNIT = iunit , FMT = '(A)' ) '=> OUT DOMAIN'
             ELSE IF (outside_window) THEN
                 WRITE (UNIT = iunit , FMT = '(A)' ) '=> OUT WINDOW'
             ELSE
                 WRITE (UNIT = iunit , FMT = '(A)' ) ''
             ENDIF
         ENDIF

         

         READ (obs(obs_num) % info % platform (4:6), '(I3)')fm
         CALL fm_decoder (fm, platform, &
                          synop=nsynops (icor), ship =nshipss (icor), &
                          metar=nmetars (icor), pilot=npilots (icor), &
                          sound=nsounds (icor), satem=nsatems (icor), &
                          satob=nsatobs (icor), airep=naireps (icor), &
                          gpspw=ngpspws (icor), gpszd=ngpsztd (icor), &
                          gpsrf=ngpsref (icor), gpsep=ngpseph (icor), &
                          ssmt1=nssmt1s (icor), &
                          ssmt2=nssmt2s (icor), ssmi =nssmis  (icor), &
                          tovs =ntovss  (icor), other=nothers (icor), &
                          amdar=namdars (icor), qscat=nqscats (icor), &
                          profl=nprofls (icor), buoy =nbuoyss (icor), &
                          bogus=nboguss (icor), airs = nairss(icor),tamdar=ntamdar(icor) )

         
         
         

         NULLIFY (obs(obs_num)%surface)

         CALL read_measurements( file_num , obs(obs_num)%surface , &
         obs(obs_num)%location , obs(obs_num)%info, outside , error_ret ,& 
         ins , jew , map_projection,      &
         obs (obs_num) % info % elevation, obs (obs_num) % info % levels, &
         iunit, print_gts_read)
         
         
         
         
         
         




         IF (error_ret .EQ. read_err ) THEN

            IF (ASSOCIATED (obs(obs_num)%surface ) ) THEN
               
               CALL dealloc_meas (obs(obs_num)%surface)
            END IF

            WRITE (UNIT = 0, FMT =  '(A,A,A,1X,A)')   &
                  "Troubles with measurement lines ", &
                   TRIM (obs(obs_num)%location%id),   &
                   TRIM (obs(obs_num)%location%name) 

            bad_count = 0
            io_error  = 0

            DO WHILE (io_error .GE. 0)

            bad_count = bad_count + 1

            IF ( bad_count .LT. 1000 ) THEN


            READ (file_num , IOSTAT = io_error , FMT = meas_format)      &
            dummy_middle % pressure    % data, dummy_middle % pressure    % qc,&
            dummy_middle % height      % data, dummy_middle % height      % qc,&
            dummy_middle % temperature % data, dummy_middle % temperature % qc,&
            dummy_middle % dew_point   % data, dummy_middle % dew_point   % qc,&
            dummy_middle % speed       % data, dummy_middle % speed       % qc,&
            dummy_middle % direction   % data, dummy_middle % direction   % qc,&
            dummy_middle % u           % data, dummy_middle % u           % qc,&
            dummy_middle % v           % data, dummy_middle % v           % qc,&
            dummy_middle % rh          % data, dummy_middle % rh          % qc,&
            dummy_middle % thickness   % data, dummy_middle % thickness   % qc

            IF (eps_equal (dummy_middle%pressure%data, end_data_r , 1.)) THEN

                READ (file_num , IOSTAT = io_error , FMT = end_format ) &
                      obs(obs_num)%info%num_vld_fld , &
                      obs(obs_num)%info%num_error , &  
                      obs(obs_num)%info%num_warning    

                WRITE (UNIT = 0, FMT = '(A)') 'Starting to READ a new report.'
                CYCLE read_obs

            END IF

            ELSE

                WRITE (UNIT = 0, FMT = '(A)') &
               'Too many attempts to read the measurement data correctly.',&
               'Exiting read loop.'

                CLOSE ( file_num ) 
                IF (print_gts_read) CLOSE ( iunit ) 

                EXIT read_obs
            END IF

            END DO 

            IF (io_error .LT. 0) THEN
                CLOSE ( file_num ) 
                IF (print_gts_read) CLOSE ( iunit ) 
                EXIT read_obs
            END IF

         ELSE IF (error_ret .EQ. no_data .and. &
                  eps_equal(obs(obs_num)%ground%pw %data,missing_r,1.0) .and.&
                        obs(obs_num)%ground%tb19v%qc .ne. 0  .and. &
                        obs(obs_num)%ground%tb19h%qc .ne. 0  .and. &
                        obs(obs_num)%ground%tb22v%qc .ne. 0  .and. &
                        obs(obs_num)%ground%tb37v%qc .ne. 0  .and. &
                        obs(obs_num)%ground%tb37h%qc .ne. 0  .and. &
                        obs(obs_num)%ground%tb85v%qc .ne. 0  .and. &
                        obs(obs_num)%ground%tb85h%qc .ne. 0  .and. &
                  eps_equal(obs(obs_num)%ground%slp%data,missing_r,1.0) ) THEN





            READ (file_num , IOSTAT = io_error , FMT = end_format ) &
                  obs(obs_num)%info%num_vld_fld , &
                  obs(obs_num)%info%num_error , &  
                  obs(obs_num)%info%num_warning    

            READ (obs(obs_num) % info % platform (4:6), '(I3)') fm

            CALL fm_decoder (fm, platform, &
                             synop=nsynops (icor+1), ship =nshipss (icor+1), &
                             metar=nmetars (icor+1), pilot=npilots (icor+1), &
                             sound=nsounds (icor+1), satem=nsatems (icor+1), &
                             satob=nsatobs (icor+1), airep=naireps (icor+1), &
                             gpspw=ngpspws (icor+1), gpszd=ngpsztd (icor+1), &
                             gpsrf=ngpsref (icor+1), gpsep=ngpseph (icor+1), &
                             ssmt1=nssmt1s (icor+1), &
                             ssmt2=nssmt2s (icor+1), ssmi =nssmis  (icor+1), &
                             tovs =ntovss  (icor+1), other=nothers (icor+1), &
                             amdar=namdars (icor+1), qscat=nqscats (icor+1), &
                             profl=nprofls (icor+1), buoy =nbuoyss (icor+1), &
                             bogus=nboguss (icor+1), airs =nairss  (icor+1), tamdar =ntamdar (icor+1)  )

            IF ( ASSOCIATED (obs(obs_num)%surface)) THEN
               
               CALL dealloc_meas ( obs(obs_num)%surface)
            END IF

            num_empty = num_empty + 1

            CYCLE read_obs

         END IF

         
         
         

         IF (outside) THEN





            READ (file_num , IOSTAT = io_error , FMT = end_format ) &
                  obs(obs_num)%info%num_vld_fld , &
                  obs(obs_num)%info%num_error , &  
                  obs(obs_num)%info%num_warning    

            READ (obs(obs_num) % info % platform (4:6), '(I3)') fm

            CALL fm_decoder (fm, platform, &
                             synop=nsynops (icor+2), ship =nshipss (icor+2), &
                             metar=nmetars (icor+2), pilot=npilots (icor+2), &
                             sound=nsounds (icor+2), satem=nsatems (icor+2), &
                             satob=nsatobs (icor+2), airep=naireps (icor+2), &
                             gpspw=ngpspws (icor+2), gpszd=ngpsztd (icor+2), &
                             gpsrf=ngpsref (icor+2), gpsep=ngpseph (icor+2), &
                             ssmt1=nssmt1s (icor+2), &
                             ssmt2=nssmt2s (icor+2), ssmi =nssmis  (icor+2), &
                             tovs =ntovss  (icor+2), other=nothers (icor+2), &
                             amdar=namdars (icor+2), qscat=nqscats (icor+2), &
                             profl=nprofls (icor+2), buoy =nbuoyss (icor+2), &
                             bogus=nboguss (icor+2), airs =nairss  (icor+2),tamdar =ntamdar (icor+2) )

            IF ( ASSOCIATED (obs(obs_num)%surface)) THEN
               
               CALL dealloc_meas ( obs(obs_num)%surface)
            END IF

            num_outside = num_outside + 1

            CYCLE read_obs

         ELSE

      

          IF ( (obs(obs_num)%info%elevation .GT. (undefined1_r - 1.))  .OR. &
               (obs(obs_num)%info%elevation .LT. (undefined2_r + 1.)) ) THEN

             obs(obs_num)%info % elevation  = missing_r

             
             if ( fm .eq. 13 .or. fm .eq. 18 .or. fm .eq. 19 .or.   &
                  fm .eq. 33 .or. fm .eq. 36 ) then
                if ( obs(obs_num)%location%latitude .lt. 41. .or. &
                     obs(obs_num)%location%latitude .gt. 50. .or. &
                     obs(obs_num)%location%longitude .lt. -95. .or. &  
                     obs(obs_num)%location%longitude .gt. -75. ) then
                   obs(obs_num)%info % elevation = 0.
                end if
             else if (fm < 39) then
                m_miss = m_miss + 1
                write(0,'(I7,1X,A,1X,A,1X,A,1X,A,1X,2(F8.3,A),A,1X,f11.3)')&
                   m_miss,'Missing elevation(id,name,platform,lat,lon,date,elv:',  &
                   obs(obs_num)%location%id   (1: 5),&
                   obs(obs_num)%location%name (1:20),&
                   obs(obs_num)%info%platform (1: 12),&
                   obs(obs_num)%location%latitude, 'N',&
                   obs(obs_num)%location%longitude,'E ', &
                   obs(obs_num)%valid_time%date_char,    &
                   obs(obs_num)%info % elevation
             endif

             
             
             
             xla = obs(obs_num)%location%latitude
             xlo = obs(obs_num)%location%longitude
             if ( fm .eq. 13 .or. fm .eq. 33 .or. fm .eq. 36 ) then
                
                if ( ( xlo .ge. -92.5  .and. xlo .le. -84.52 )  .and.    &
                     ( xla .ge.  46.48 .and. xla .le.  49.0 ) ) then
                   
                   obs(obs_num)%info % elevation = 183.
                else if ( ( xlo .ge. -88.1 .and. xlo .le. -84.8 ) .and.  &
                          ( xla .ge. 41.2  .and. xla .le.  46.2 ) ) then
                   
                   obs(obs_num)%info % elevation = 176.
                else if ( ( xlo .ge. -84.8 .and. xlo .le. -79.79 ) .and. &
                          ( xla .ge. 43.0  .and. xla .le.  46.48 ) ) then
                   
                   obs(obs_num)%info % elevation = 176.
                else if ( ( xlo .ge. -84.0 .and. xlo .le. -78.0 ) .and.  &
                          ( xla .ge. 41.0  .and. xla .le.  42.9 ) ) then
                   
                   obs(obs_num)%info % elevation = 174.
                else if ( ( xlo .ge. -80.0 .and. xlo .le. -76.0 ) .and.  &
                       ( xla .ge. 43.1  .and. xla .le.  44.23 ) ) then
                   
                   obs(obs_num)%info % elevation = 74.
                end if
             end if 
             if ( fm .eq. 18 .and. obs(obs_num)%location%id(1:2) .eq. '45' ) then
                
                
                if ( use_msfc_tbl .and. num_stations_msfc > 0 ) then
                   ipos = 0
                   do while ( ipos < num_stations_msfc )
                      ipos = ipos + 1
                      if ( obs(obs_num)%location%id(1:5) == id_tbl(ipos) ) then
                         obs(obs_num)%info % elevation = elev_tbl(ipos)
                         exit
                      end if
                   end do
                end if 
             end if 
          END IF 

         END IF  

         
         
         
         
         
         
         

         IF ((obs (obs_num)%info%platform(1:12) .EQ. 'FM-15 METAR ' .or. &
              obs (obs_num)%info%platform(1:12) .EQ. 'FM-16 SPECI ') .AND. &
             (ASSOCIATED (obs (obs_num)%surface ) ) ) THEN
            if ( calc_psfc_from_QNH .and. gts_from_mmm_archive ) then
               if ( obs(obs_num)%ground%psfc%data > 0.0 .and. &
                    obs(obs_num)%info%elevation > 0.0 ) then
                  QNH  = obs(obs_num)%ground%psfc%data * 0.01 
                  elev = obs(obs_num)%info%elevation
                  obs(obs_num)%ground%psfc%data = psfc_from_QNH(QNH,elev) &
                                                  * 100.0  
                  obs(obs_num)%ground%psfc%qc   = 0
                  if ( associated(obs(obs_num)%surface) ) then
                     obs(obs_num)%surface%meas%pressure%data = &
                        obs(obs_num)%ground%psfc%data
                     obs(obs_num)%surface%meas%pressure%qc   = &
                        obs(obs_num)%ground%psfc%qc
                  end if  
               end if  
            else
               obs(obs_num)%ground%psfc%data = missing_r
               obs(obs_num)%ground%psfc%qc   = missing
            end if  
         END IF  

         
         
         
         
         
         

         
         
         
         
         

         IF ( (obs(obs_num)%info%platform(1:10) == 'FM-13 SHIP') .or. &
              (obs(obs_num)%info%platform(1:10) == 'FM-18 BUOY') ) then
            if ( ASSOCIATED(obs(obs_num)%surface) ) then
               obs(obs_num)%surface%meas%height%data   = &
                  obs(obs_num)%info%elevation
               obs(obs_num)%surface%meas%height%qc     = 0
               if ( (obs(obs_num)%info%elevation == 0.0) ) then
                  
                  
                  
                  obs(obs_num)%surface%meas%pressure%data = &
                     obs(obs_num)%ground%slp%data
                  obs(obs_num)%surface%meas%pressure%qc   = 0
               else
                  if ( eps_equal(obs(obs_num)%surface%meas%pressure%data, &
                                 101301.000, 1.) ) then
                     
                     obs(obs_num)%surface%meas%pressure%data = missing_r
                     obs(obs_num)%surface%meas%pressure%qc   = missing
                  end if
               end if  
            end if  
         end if  

         
         
         
         

         IF ((obs (obs_num)%info%platform(1:6) .EQ. 'FM-281' ) .and. &
             (ASSOCIATED (obs (obs_num)%surface ) ) ) THEN
             if (obs(obs_num)%info%elevation .LT. 0.0) then
                 obs(obs_num)%surface%meas%height%data = 10.0
             else
                 obs(obs_num)%surface%meas%height%data = &
                 obs(obs_num)%info%elevation
             end if
             obs(obs_num)%surface%meas%height%qc = 0
          END IF

         
         
         
         
         
         

         
         
         
         
         
         
         

         
         
         
         
         

         IF ( (obs(obs_num)%info%platform(1:5).EQ.'FM-12') .and.  &
              (ASSOCIATED(obs(obs_num)%surface)) ) THEN
            if ( eps_equal(obs(obs_num)%surface%meas%pressure%data, &
                                          101301.000, 1.) ) then
               obs(obs_num)%surface%meas%pressure%data = missing_r
               obs(obs_num)%surface%meas%pressure%qc   = missing
            endif
         ENDIF  

         








                 


      END IF

      
      
      

      READ (file_num , IOSTAT = io_error , FMT = end_format ) &
            obs(obs_num)%info%num_vld_fld , &
            obs(obs_num)%info%num_error , &  
            obs(obs_num)%info%num_warning    

      
      
      
      
      

      IF ( io_error .NE. 0 ) THEN

         error_message = &
              'Error trying to read last 3 integers in observation '&
              // TRIM ( obs(obs_num)%location%id ) &               
              // TRIM ( obs(obs_num)%location%name ) // '.'

         CALL error_handler (proc_name, error_message, &
                           ' Discarding entire and continuing.', .FALSE.)

         IF ( ASSOCIATED ( obs(obs_num)%surface ) ) THEN
            CALL dealloc_meas ( obs(obs_num)%surface)
         END IF

         CYCLE read_obs

      END IF
   
      
      

      

      IF (.NOT. eps_equal(obs(obs_num)%info%elevation, missing_r, 1.)) &
      CALL surf_first ( obs(obs_num)%surface , obs(obs_num)%info%elevation )

      

      CALL missing_hp ( obs(obs_num)%surface )

      

      obs (obs_num) % info % levels = info_levels (obs(obs_num)%surface)

      

      IF      (obs (obs_num) % info % levels .GT. 1) THEN
               obs (obs_num) % info % is_sound = .TRUE.
      ELSE IF (obs (obs_num) % info % levels .EQ. 1) THEN
               obs (obs_num) % info % is_sound = .FALSE.


 
      ELSE IF (eps_equal(obs(obs_num)%ground%pw %data,missing_r,1.0) .and.&
                         obs(obs_num)%ground%tb19v%qc .ne. 0  .and. &
                         obs(obs_num)%ground%tb19h%qc .ne. 0  .and. &
                         obs(obs_num)%ground%tb22v%qc .ne. 0  .and. &
                         obs(obs_num)%ground%tb37v%qc .ne. 0  .and. &
                         obs(obs_num)%ground%tb37h%qc .ne. 0  .and. &
                         obs(obs_num)%ground%tb85v%qc .ne. 0  .and. &
                         obs(obs_num)%ground%tb85h%qc .ne. 0  .and. &
               eps_equal(obs(obs_num)%ground%slp%data,missing_r,1.0) ) THEN
          
          obs(obs_num) % info % discard = .TRUE.
          CYCLE read_obs
      ENDIF

      
      

      obs_num = obs_num + 1

   END DO read_obs


   
   
   
   

   obs_num = obs_num - 1

   

   WRITE (UNIT = 0, FMT = '(/,A)')  &
'------------------------------------------------------------------------------'
   WRITE (UNIT = 0, FMT = '(A)') 'GTS OBSERVATIONS READ:'

   WRITE (UNIT = 0, FMT = '(A)')
   WRITE (UNIT = 0, FMT = '(A,I7)') ' SYNOP reports:',nsynops (0)
   WRITE (UNIT = 0, FMT = '(A,I7)') ' SHIPS reports:',nshipss (0)
   WRITE (UNIT = 0, FMT = '(A,I7)') ' BUOYS reports:',nbuoyss (0)
   WRITE (UNIT = 0, FMT = '(A,I7)') ' BOGUS reports:',nboguss (0)
   WRITE (UNIT = 0, FMT = '(A,I7)') ' METAR reports:',nmetars (0)
   WRITE (UNIT = 0, FMT = '(A,I7)') ' PILOT reports:',npilots (0)
   WRITE (UNIT = 0, FMT = '(A,I7)') ' SOUND reports:',nsounds (0)
   WRITE (UNIT = 0, FMT = '(A,I7)') ' AMDAR reports:',namdars (0)
   WRITE (UNIT = 0, FMT = '(A,I7)') ' SATEM reports:',nsatems (0)
   WRITE (UNIT = 0, FMT = '(A,I7)') ' SATOB reports:',nsatobs (0)
   WRITE (UNIT = 0, FMT = '(A,I7)') ' GPSPW reports:',ngpspws (0)
   WRITE (UNIT = 0, FMT = '(A,I7)') ' GPSZD reports:',ngpsztd (0)
   WRITE (UNIT = 0, FMT = '(A,I7)') ' GPSRF reports:',ngpsref (0)
   WRITE (UNIT = 0, FMT = '(A,I7)') ' GPSEP reports:',ngpseph (0)
   WRITE (UNIT = 0, FMT = '(A,I7)') ' AIREP reports:',naireps (0)
   WRITE (UNIT = 0, FMT = '(A,I7)') 'TAMDAR reports:',ntamdar (0)
   WRITE (UNIT = 0, FMT = '(A,I7)') ' SSMT1 reports:',nssmt1s (0)
   WRITE (UNIT = 0, FMT = '(A,I7)') ' SSMT2 reports:',nssmt2s (0)
   WRITE (UNIT = 0, FMT = '(A,I7)') ' SSMI  reports:',nssmis  (0)
   WRITE (UNIT = 0, FMT = '(A,I7)') ' TOVS  reports:',ntovss  (0)
   WRITE (UNIT = 0, FMT = '(A,I7)') ' QSCAT reports:',nqscats (0)
   WRITE (UNIT = 0, FMT = '(A,I7)') ' PROFL reports:',nprofls (0)
   WRITE (UNIT = 0, FMT = '(A,I7)') ' AIRST reports:',nairss  (0)
   WRITE (UNIT = 0, FMT = '(A,I7)') ' OTHER reports:',nothers (0)
   WRITE (UNIT = 0, FMT = '(A,I7)') ' Total reports:', &
          nsynops (0) + nshipss (0) + nmetars (0) + npilots (0) + nsounds (0)+&
          nsatems (0) + nsatobs (0) + naireps (0) +  ntamdar (0)+ ngpspws (0) + ngpsztd (0)+&
          ngpsref (0) + ngpseph (0) + &
          nssmt1s (0) + nssmt2s (0) + nssmis  (0) + ntovss  (0) + nboguss (0)+&
          nothers (0) + namdars (0) + nqscats (0) + nprofls(0)  + nbuoyss(0) +&
          nairss(0)

   




    
    WRITE (UNIT = 0, FMT = '(/,4(A,i8,/))' ) &

          "Number of observations read:          ",obs_num+    &
                                                   num_empty+  &
                                                   num_outside-&
                                                   n_obs,      &
          "Number of empty observations:         ",num_empty,  &
          "Number of observations out of domain: ",num_outside,&
          "Number of observations for ingestion: ",obs_num-n_obs

   

    n_obs = obs_num

    write(0,'(/"AIRCRAFT DATA: Total=",I7,"  Above cut_height=",I7)')&
                                                     N_air, N_air_cut
contains

subroutine print_extra_obs

  READ (obs(obs_num) % info % platform (4:6), '(I3)', &
                                          IOSTAT = platform_error) fm

  if (fm == 12 .or. fm ==14) return


  if (fm == 13) return


  if (fm == 15 .or. fm == 16) return


  if (fm == 18 .or. fm == 19) return


  if (fm >= 32 .and. fm <= 34) return


  if (fm >= 35 .and. fm <= 38) return


  if (fm == 42) return


  if (fm == 86) return


  if (fm == 88) return


  if (fm == 96 .or. fm == 97) return


  if (fm == 101) return


  if (fm == 111) return


  if (fm == 114) return


  if (fm == 116) return


  if (fm == 118) return


  if (fm == 121) return


  if (fm == 122) return


  if (fm == 125 .or. fm == 126) return


  if (fm == 131) return


  if (fm == 281) return


  if (fm == 132) return


  if (fm == 135) return

  if (fm == 133) return



  
   num_unknown = num_unknown + 1
   write(0,'(2I8," ID=",a," Name=",a," Platform=",a)') &
                          num_unknown, obs_num, &
                          obs(obs_num)%location % id(1:15), &
                          obs(obs_num)%location % name, &
                          obs(obs_num)%info % platform
end subroutine print_extra_obs
 
END SUBROUTINE read_obs_gts




SUBROUTINE read_measurements (file_num, surface, location, info, bad_data, &
                              error, ins, jew, &
                              map_projection, elevation, nlevels, iunit,&
                              print_gts_read)






   USE module_icao

   IMPLICIT NONE 

   INTEGER , INTENT ( IN )                      :: file_num   
   TYPE ( measurement ) , POINTER               :: surface    
   TYPE ( location_type ) , INTENT ( IN )       :: location   
   TYPE ( source_info ) ,   INTENT ( INOUT )    :: info       
   LOGICAL , INTENT ( IN )                      :: bad_data   
   INTEGER , INTENT ( OUT )                     :: error      

   INTEGER                                      :: ins , jew, k
   INTEGER                                      :: map_projection

   CHARACTER ( LEN = 32 ) , PARAMETER    :: proc_name = 'read_measurements'
   INTEGER                                      :: meas_count
   INTEGER                                      :: io_error
   TYPE ( measurement ) , POINTER               :: current

   CHARACTER ( LEN = 40 )                       :: location_id , &
                                                   location_name
   REAL , INTENT(IN)                            :: elevation
   REAL                                         :: new_press, new_heightt, &
                                                   ref_h
   INTEGER, INTENT (out)                        :: nlevels
   INTEGER, INTENT (in)                         :: iunit
   LOGICAL, INTENT (in)                         :: print_gts_read
   LOGICAL                                      :: no_height, no_pressure
   LOGICAL                                      :: no_temperature

   INTEGER :: icrs, fm








   

   ALLOCATE ( current )
   NULLIFY ( current%next )
   NULLIFY ( surface )
   error = ok
   meas_count = 0
   location_id   = TRIM ( location%id )
   location_name = TRIM ( location%name )

   
   

   read_meas: DO 

      
      




      READ ( file_num , IOSTAT = io_error , FMT = meas_format )  &
      current % meas % pressure    % data, current % meas % pressure    % qc, &
      current % meas % height      % data, current % meas % height      % qc, &
      current % meas % temperature % data, current % meas % temperature % qc, &
      current % meas % dew_point   % data, current % meas % dew_point   % qc, & 
      current % meas % speed       % data, current % meas % speed       % qc, & 
      current % meas % direction   % data, current % meas % direction   % qc, &
      current % meas % u           % data, current % meas % u           % qc, & 
      current % meas % v           % data, current % meas % v           % qc, &
      current % meas % rh          % data, current % meas % rh          % qc, &
      current % meas % thickness   % data, current % meas % thickness   % qc

      
      
      
      

      IF (io_error .GT. 0 ) THEN
         error = read_err

         EXIT read_meas
      ELSE IF (io_error .LT. 0 ) THEN
         error = eof_err
         CLOSE ( file_num ) 
         IF (print_gts_read) CLOSE ( iunit ) 
         EXIT read_meas
      END IF

      
      

      bad_loop_1 : IF (.NOT. bad_data) THEN
   
         
         
         
   
         IF ((current%meas%pressure%data    .GE. ( undefined1_r - 10. ) ) .OR. &
             (current%meas%pressure%data    .LE. ( undefined2_r + 10. ) ) .OR. &
             (current%meas%pressure%data    .LE. 0.0)                      )THEN
              current%meas%pressure%data    = missing_r
              current%meas%pressure%qc      = missing
         END IF
         IF ((current%meas%height%data      .GT. ( undefined1_r - 1. ) )  .OR. &
             (current%meas%height%data      .LT. ( undefined2_r + 1. ) )  .OR. &
             (current%meas%height%data      .GT. ( height_max_icao - 1.))  .OR. &
             (current%meas%height%data      .GT. ( ABS (missing_r) - 1. ))) THEN
              current%meas%height%data      = missing_r
              current%meas%height%qc        = missing
         END IF
         IF ((current%meas%temperature%data .GT. ( undefined1_r - 1. ) )  .OR. &
             (current%meas%temperature%data .LT. ( undefined2_r + 1. ) ) ) THEN
              current%meas%temperature%data = missing_r
              current%meas%temperature%qc   = missing
         END IF
         IF  (current%meas%temperature%data .GT. (    99999.0   - 1. ) )   THEN
              current%meas%temperature%data = missing_r
              current%meas%temperature%qc   = missing
         END IF
         IF ((current%meas%dew_point%data   .GT. ( undefined1_r - 1. ) )  .OR. &
             (current%meas%dew_point%data   .LT. ( undefined2_r + 1. ) ) ) THEN
              current%meas%dew_point%data   = missing_r
              current%meas%dew_point%qc     = missing
         END IF
         IF ((current%meas%speed%data       .GT. ( undefined1_r - 1. ) )  .OR. &
             (current%meas%speed%data       .LT. ( undefined2_r + 1. ) ) ) THEN
              current%meas%speed%data       = missing_r
              current%meas%speed%qc         = missing
         END IF
         IF ((current%meas%direction%data   .GT. ( undefined1_r - 1. ) )  .OR. &
             (current%meas%direction%data   .LT. ( undefined2_r + 1. ) ) ) THEN
              current%meas%direction%data   = missing_r
              current%meas%direction%qc     = missing
         END IF
         IF ((current%meas%u%data           .GT. ( undefined1_r - 1. ) )  .OR. &
             (current%meas%u%data           .LT. ( undefined2_r + 1. ) ) ) THEN
              current%meas%u%data           = missing_r
              current%meas%u%qc             = missing
         END IF
         IF ((current%meas%v%data           .GT. ( undefined1_r - 1. ) )  .OR. &
             (current%meas%v%data           .LT. ( undefined2_r + 1. ) ) ) THEN
              current%meas%v%data           = missing_r
              current%meas%v%qc             = missing
         END IF
         IF ((current%meas%rh%data          .GT. ( undefined1_r - 1. ) )  .OR. &
             (current%meas%rh%data          .LT. ( undefined2_r + 1. ) ) ) THEN
              current%meas%rh%data          = missing_r
              current%meas%rh%qc            = missing
         END IF
         IF ((current%meas%thickness%data   .GT. ( undefined1_r - 1. ) )  .OR. &
             (current%meas%thickness%data   .LT. ( undefined2_r + 1. ) ) ) THEN
              current%meas%thickness%data   = missing_r
              current%meas%thickness%qc     = missing
         END IF

              current%meas%qv%data = missing_r
              current%meas%qv%qc   = missing

      END IF bad_loop_1

      
      
      
      
      
      

      IF (eps_equal (current%meas%pressure%data , end_data_r , 1.) .OR. &
          eps_equal (current%meas%height%data   , end_data_r , 1.)) THEN
          current%meas%pressure%data    = end_data_r
          current%meas%height%data      = end_data_r
          current%meas%temperature%data = end_data_r
          current%meas%dew_point%data   = end_data_r
          current%meas%speed%data       = end_data_r
          current%meas%direction%data   = end_data_r
          current%meas%u%data           = end_data_r
          current%meas%v%data           = end_data_r
          current%meas%rh%data          = end_data_r
          current%meas%thickness%data   = end_data_r
          current%meas%pressure%qc      = end_data  
          current%meas%height%qc        = end_data  
          current%meas%temperature%qc   = end_data  
          current%meas%dew_point%qc     = end_data  
          current%meas%speed%qc         = end_data  
          current%meas%direction%qc     = end_data  
          current%meas%u%qc             = end_data  
          current%meas%v%qc             = end_data  
          current%meas%rh%qc            = end_data  
          current%meas%thickness%qc     = end_data  
          current%meas%qv%data          = end_data
          current%meas%qv%qc            = end_data
          error = ok

          EXIT read_meas


      

      ELSEIF ((eps_equal(current%meas%pressure%data, missing_r , 1.) .OR. &
               eps_equal(current%meas%pressure%data, missing_r , 1.))   .AND. &
              eps_equal (current%meas%speed%data,       missing_r , 1.) .AND. &
              eps_equal (current%meas%direction%data,   missing_r , 1.) .AND. &
              eps_equal (current%meas%temperature%data, missing_r , 1.) .AND. &
              eps_equal (current%meas%dew_point%data,   missing_r , 1.) .AND. &
              eps_equal (current%meas%rh%data,          missing_r , 1.)) THEN

         CYCLE read_meas

      END IF

      
      
      

      IF (bad_data) THEN
          CYCLE read_meas
      END IF

      
      
      

      IF ((eps_equal ( current%meas%pressure%data , missing_r , 1.)) .AND. &
          (eps_equal  ( current%meas%height  %data , missing_r , 1.))) THEN
          CYCLE read_meas
      END IF

      if ( print_gts_read ) then
      IF ((     eps_equal (current%meas%dew_point%data , missing_r , 1.)) .AND.&
          (.NOT.eps_equal (current%meas%rh       %data , missing_r , 1.))) THEN
           WRITE (iunit,'(A,F10.2,/,A,F10.2)') &
          " Td = ",current%meas%dew_point%data,&
          " Rh = ",current%meas%rh%data  
      ENDIF
      end if

      
      
      

      
      
      current%meas%speed%error = 0.0

      READ (info % platform (4:6), '(I3)') fm

      IF ((fm .EQ. 125) .AND. (current%meas%speed%qc .GT. missing)) THEN 

      SELECT CASE (current%meas%speed%qc)

             CASE (0)
                 current%meas%speed%error = 2.  
             CASE (1)
                 current%meas%speed%error = 5.  
             CASE (2)
                 current%meas%speed%error = 10. 
             CASE (3)
                 current%meas%speed%error = 20. 
             CASE DEFAULT
                 current%meas%speed%error = 20. 
      END SELECT

      current%meas%speed%qc = 0

      ELSE IF ((fm == 97 .or. fm == 96 .or. fm == 42) .and. &
               (current%meas%height%qc  == 0 ) ) then

               N_air = N_air + 1
               if (current%meas%height%data > aircraft_cut) then



                  N_air_cut = N_air_cut + 1
             call Aircraft_pressure(current%meas%height, current%meas%pressure)
               endif




      ELSE IF ( fm == 13 .or. fm == 18 .or. fm == 19 ) THEN
           if (current%meas%pressure%data < 85000.0 .and. &
               current%meas%pressure%qc >= 0) then 
               write(0,'(a,3x,a,2x,a,2x,2f13.5,2x,"Pressure=",f10.1,a,i8)') &
                   'Discarded:', info%platform(1:12), trim(location%id), &
                   location%latitude,   location%longitude, &
                   current%meas%pressure%data, " < 85000.0 Pa, qc=", &
                   current%meas%pressure%qc 
              CYCLE read_meas
           endif
      
      ENDIF

      

      IF (ASSOCIATED (current)) THEN
        












      IF ( current%meas%height%qc == 0 .and. current%meas%pressure%qc == 0 &
      .and. .NOT.eps_equal(current%meas%height  %data, missing_r, 1.) .and.&
            .NOT.eps_equal(current%meas%pressure%data, missing_r, 1.) )THEN




        if (current%meas%pressure%data >= 500.0) then
          Ref_h = Ref_height (current%meas%pressure%data)
        else
          Ref_h = Ref_height (500.0)
        endif



         if (abs(Ref_h-current%meas%height%data) > 12000) then
             write(0,'("??? Pressure or Height reported inconsistent:")')
             write(0,'(3x,a,2x,a,2x,2f13.5)') &
                   info%platform(1:12), trim(location%id), &
                   location%latitude,        location%longitude
             write(0,'("(height-Ref_h) > 12000m, p,h,ref_h:",3e15.5/)') &
             current%meas%pressure%data, current%meas%height%data, Ref_h 
             CYCLE read_meas
         endif
      ENDIF
      
      IF (eps_equal (current%meas%pressure%data , missing_r , 1.)) THEN
           
           if (current%meas%height%data > (htop+100.))   CYCLE read_meas 
           current%meas%pressure%data = Ref_pres (current%meas%height%data)
           current%meas%pressure%qc   = missing
      ENDIF
      IF (eps_equal (current%meas%height%data , missing_r , 1.)) THEN
           
           if (current%meas%pressure%data < (ptop-500.)) CYCLE read_meas 
           current%meas%height%data = Ref_height (current%meas%pressure%data)
           current%meas%height%qc   = missing
      ENDIF
      ENDIF

      IF (ASSOCIATED (surface)) THEN
      IF (eps_equal (surface%meas%pressure%data , missing_r , 1.)) THEN
           surface%meas%pressure%data = Ref_pres (surface%meas%height%data)
           surface%meas%pressure%qc   = missing
      ENDIF

      IF (eps_equal (surface%meas%height%data , missing_r , 1.)) THEN
           surface%meas%height%data = Ref_height (surface%meas%pressure%data)
           surface%meas%height%qc   = missing
      ENDIF
      ENDIF

      
      

      meas_count = meas_count + 1

      
     
      CALL insert_at (surface , current , elevation)

      

      nlevels = nlevels + 1 


      
      
      

      ALLOCATE ( current )
      NULLIFY ( current%next )

   END DO read_meas  

   
   

   DEALLOCATE ( current )

   
   
   
   

   IF ( ( meas_count .LT. 1  ) .AND. &
        ( error      .EQ. ok ) .AND. &
        ( .NOT. bad_data     ) ) THEN
          nlevels = 0
          error = no_data
   END IF 

   
   
   
   
   

   SELECT CASE ( error )

      CASE ( eof_err )
         CALL error_handler (proc_name, &
                           ' Found EOF, expected measurement.  Continuing.  ', &
                            TRIM(location_id) // ' ' // TRIM(location_name),   &
                            .FALSE.)

      CASE ( read_err )
         CALL error_handler (proc_name, &
                           ' Error in measurement read. ' // &
                           ' Discarding entire observation and continuing. ', &
                             TRIM(location_id) // ' ' // TRIM(location_name), &
                            .FALSE.)
         CALL dealloc_meas (surface)

      CASE (no_data , ok)

      CASE DEFAULT

        CALL error_handler (proc_name," Internal error: ","bad error number.",&
            .TRUE.)

   END SELECT

END SUBROUTINE read_measurements




SUBROUTINE dealloc_meas ( head )



   IMPLICIT NONE 

   TYPE ( measurement ) , POINTER           :: head     

   TYPE ( measurement ) , POINTER           :: previous &
                                             , temp
   INTEGER                                  :: status

   
   

   IF ( ASSOCIATED ( head ) ) THEN

      previous => head
      list_loop : DO WHILE ( ASSOCIATED ( previous%next ) )
         temp => previous
         previous => previous%next
         DEALLOCATE ( temp , STAT = status) 
         IF (status .NE. 0 ) THEN
             WRITE (UNIT = 0, FMT = '(A)') &
          'Error in DEALLOCATE, continuing by stopping DEALLOCATE on this list.'
            EXIT list_loop
         END IF
      END DO list_loop

      NULLIFY ( head )
   END IF

END SUBROUTINE dealloc_meas




SUBROUTINE sub_info_levels ( surface, levels )





   IMPLICIT NONE

   TYPE ( measurement ) ,  POINTER         :: surface
   INTEGER , INTENT(OUT)                   :: levels

   TYPE ( measurement ) , POINTER          :: current

   

   levels = 0

   IF ( ASSOCIATED ( surface ) ) THEN

      levels = levels + 1 

      current  => surface%next

      DO WHILE ( ASSOCIATED ( current ) ) 

         levels = levels + 1 
         current => current%next

      END DO

   END IF

END SUBROUTINE sub_info_levels


SUBROUTINE missing_hp ( surface )





   IMPLICIT NONE

   TYPE ( measurement ) ,  POINTER         :: surface
   TYPE ( measurement ) , POINTER          :: current

   

   IF ( ASSOCIATED ( surface ) ) THEN

      




      IF (surface%meas%height%qc     == missing) &
          surface%meas%height%data   =  missing_r
      IF (surface%meas%pressure%qc   == missing) &
          surface%meas%pressure%data =  missing_r

          current  => surface%next

      DO WHILE ( ASSOCIATED ( current ) ) 

         IF (current%meas%height%qc     == missing) &
             current%meas%height%data   =  missing_r
         IF (current%meas%pressure%qc   == missing) &
             current%meas%pressure%data =  missing_r

             current => current%next

      END DO

   END IF

END SUBROUTINE missing_hp



SUBROUTINE surf_first ( surface , elevation )





   IMPLICIT NONE

   TYPE ( measurement ) ,  POINTER         :: surface
   REAL , INTENT(IN)                       :: elevation

   TYPE ( measurement ) , POINTER          :: current

   

   IF ( ASSOCIATED ( surface ) ) THEN

      




      current  => surface%next

      find_sfc : DO WHILE ( ASSOCIATED ( current ) ) 

         IF ( eps_equal ( current%meas%height%data , elevation , 1. ) ) THEN
            surface => current
            EXIT find_sfc
         END IF

         current => current%next

      END DO find_sfc

   END IF

END SUBROUTINE surf_first



SUBROUTINE insert_at ( surface , new , elevation)






  USE module_obs_merge

   IMPLICIT NONE

   TYPE ( measurement ) ,  POINTER         :: surface , new
   REAL , INTENT(IN)                       :: elevation

   TYPE ( measurement ) , POINTER          :: current , previous , oldptr
   REAL                                    :: new_pres , new_height
   CHARACTER ( LEN = 32 ) , PARAMETER      :: name = 'insert_at'







   
   

   new_pres   = new%meas%pressure%data
   new_height = new%meas%height%data

   NULLIFY ( new%next )

   
   

   IF ( .NOT. ASSOCIATED ( surface ) ) THEN

      surface => new

   
   
   

   ELSE

      

      previous => surface 
      current => surface


      
      
      
      
      
      

      still_some_data : DO WHILE ( ASSOCIATED ( current ) )

         IF ( current%meas%pressure%data .LT. new_pres ) EXIT still_some_data

         previous => current
         current  => current%next

      END DO still_some_data 

      
      
      
      
      
      
      
      
      

      IF ((eps_equal (previous%meas%pressure%data, new_pres   , 1. ))  .OR.  &
         ((eps_equal (previous%meas%height%data  , new_height , 1. ))  .AND. &
          (eps_equal (previous%meas%height%data  , elevation  , 1. )))) THEN

         CALL merge_measurements (previous%meas , new%meas , 1)
         DEALLOCATE (new)

      ELSE IF (.NOT. ASSOCIATED (current)) THEN

                previous%next => new

      ELSE IF ((eps_equal (current%meas%pressure%data, new_pres   , 1.)) .OR.  &
              ((eps_equal (current%meas%height%data  , new_height , 1.)) .AND. &
               (eps_equal (current%meas%height%data  , elevation  , 1.)))) THEN

                CALL merge_measurements (current%meas, new%meas , 1)

                DEALLOCATE (new)

      ELSE IF  (previous%meas%pressure%data .GT. new_pres) THEN

                oldptr => previous%next
                previous%next => new
                new%next => oldptr

      ELSE IF  (previous%meas%pressure%data .LT. new_pres) THEN



           IF (.NOT. ASSOCIATED (previous, surface)) THEN
                CALL error_handler (name, 'Logic error in IF' ,"", .TRUE.)
           ELSE
                oldptr => surface
                surface => new
                new%next => oldptr
           END IF 

      ELSE

         

        CALL error_handler (name, "Logic error in IF test: ",&
            "for where to put the new observation level.", .TRUE.)

      END IF

   END IF

END SUBROUTINE insert_at




SUBROUTINE output_obs ( obs , unit , file_name , num_obs , out_opt, forinput )





   
   
   
   
   

   IMPLICIT NONE

   TYPE ( report ) , INTENT ( IN ) , DIMENSION ( : ) :: obs
   INTEGER , INTENT ( IN )                           :: num_obs
   INTEGER , INTENT ( IN )                           :: out_opt   
   INTEGER , INTENT ( IN )                           :: unit
   CHARACTER ( LEN = * ) , INTENT ( IN )             :: file_name
   LOGICAL , INTENT ( IN )                           :: forinput

   INTEGER                                           :: i , iout
   TYPE ( measurement ) , POINTER                    :: next
   TYPE ( meas_data   )                              :: end_meas
 
   end_meas%pressure%data    = end_data_r
   end_meas%height%data      = end_data_r
   end_meas%temperature%data = end_data_r
   end_meas%dew_point%data   = end_data_r
   end_meas%speed%data       = end_data_r
   end_meas%direction%data   = end_data_r
   end_meas%u%data           = end_data_r
   end_meas%v%data           = end_data_r
   end_meas%rh%data          = end_data_r
   end_meas%thickness%data   = end_data_r
   end_meas%pressure%qc      = end_data  
   end_meas%height%qc        = end_data  
   end_meas%temperature%qc   = end_data  
   end_meas%dew_point%qc     = end_data  
   end_meas%speed%qc         = end_data  
   end_meas%direction%qc     = end_data  
   end_meas%u%qc             = end_data  
   end_meas%v%qc             = end_data  
   end_meas%rh%qc            = end_data  
   end_meas%thickness%qc     = end_data  

   OPEN ( UNIT = unit , FILE = file_name ,  ACTION = 'write' , FORM = 'formatted' )

   iout = 0

   DO i = 1 , num_obs

      IF (   out_opt .EQ. 0                                   .OR. &
           ( out_opt .GT. 0 .AND. .NOT. obs(i)%info%discard ) .OR. &
           ( out_opt .LT. 0 .AND.       obs(i)%info%discard ) ) THEN

         iout = iout + 1
         IF ( .NOT. forinput ) write(unit,*) '**************** Next Observation *******************'
         WRITE ( UNIT = unit , FMT = rpt_format ) &
            obs(i)%location % latitude,     obs(i)%location % longitude, &
            obs(i)%location % id,           obs(i)%location % name, &
            obs(i)%info % platform,         obs(i)%info % source, &
            obs(i)%info % elevation,        obs(i)%info % num_vld_fld, &
            obs(i)%info % num_error,        obs(i)%info % num_warning, &
            obs(i)%info % seq_num,          obs(i)%info % num_dups, &
            obs(i)%info % is_sound,         obs(i)%info % bogus, &
            obs(i)%info % discard, & 
            obs(i)%valid_time % sut,        obs(i)%valid_time % julian, &
            obs(i)%valid_time % date_char,  &
            obs(i)%ground%slp%data,         obs(i)%ground%slp%qc,&
            obs(i)%ground%ref_pres%data,    obs(i)%ground%ref_pres%qc,&
            obs(i)%ground%ground_t%data,    obs(i)%ground%ground_t%qc,&
            obs(i)%ground%sst%data,         obs(i)%ground%sst%qc,&
            obs(i)%ground%psfc%data,        obs(i)%ground%psfc%qc,&
            obs(i)%ground%precip%data,      obs(i)%ground%precip%qc,&
            obs(i)%ground%t_max%data,       obs(i)%ground%t_max%qc,&
            obs(i)%ground%t_min%data,       obs(i)%ground%t_min%qc,&
            obs(i)%ground%t_min_night%data, obs(i)%ground%t_min_night%qc,&
            obs(i)%ground%p_tend03%data,    obs(i)%ground%p_tend03%qc,&
            obs(i)%ground%p_tend24%data,    obs(i)%ground%p_tend24%qc, &
            obs(i)%ground%cloud_cvr%data,   obs(i)%ground%cloud_cvr%qc, &
            obs(i)%ground%ceiling%data,     obs(i)%ground%ceiling%qc




         next => obs(i)%surface
         DO WHILE ( ASSOCIATED ( next ) )
            if ( obs(i)%info%discard ) exit 

            WRITE ( UNIT = unit , FMT = meas_format )  &
            next%meas % pressure    % data, next%meas % pressure    % qc, &
            next%meas % height      % data, next%meas % height      % qc, &
            next%meas % temperature % data, next%meas % temperature % qc, &
            next%meas % dew_point   % data, next%meas % dew_point   % qc, &
            next%meas % speed       % data, next%meas % speed       % qc, &
            next%meas % direction   % data, next%meas % direction   % qc, &
            next%meas % u           % data, next%meas % u           % qc, &
            next%meas % v           % data, next%meas % v           % qc, &
            next%meas % rh          % data, next%meas % rh          % qc, &
            next%meas % thickness   % data, next%meas % thickness   % qc

            next => next%next
         END DO

         WRITE ( UNIT = unit , FMT = meas_format ) &
            end_meas % pressure    % data, end_meas % pressure    % qc, &
            end_meas % height      % data, end_meas % height      % qc, &
            end_meas % temperature % data, end_meas % temperature % qc, &
            end_meas % dew_point   % data, end_meas % dew_point   % qc, &
            end_meas % speed       % data, end_meas % speed       % qc, &
            end_meas % direction   % data, end_meas % direction   % qc, &
            end_meas % u           % data, end_meas % u           % qc, &
            end_meas % v           % data, end_meas % v           % qc, &
            end_meas % rh          % data, end_meas % rh          % qc, &
            end_meas % thickness   % data, end_meas % thickness   % qc

         WRITE ( UNIT = unit , FMT = end_format ) obs(i)%info%num_vld_fld, &
            obs(i)%info%num_error, obs(i)%info%num_warning
         IF ( .NOT. forinput ) &
            write(unit,*) 'End of measurements for observation ' , i

      END IF

   END DO

   IF ( .NOT. forinput ) THEN
      write(unit,*) '======================================================='
      write(unit,*) 'Total Number of Measurements output ' , iout
   ENDIF

   
   

   CLOSE ( unit )

END SUBROUTINE output_obs

Subroutine Aircraft_pressure(hh, pp)







      implicit none

      Type (field), intent(inout)  :: hh
      Type (field), intent(out)    :: pp
 
      IF (HH%data > 11000.0) THEN 

         PP%data =  226.3 * EXP(1.576106E-4 * (11000.00 - HH%data)) 
 
      ELSE IF (HH%data <= 11000.0) THEN 

         PP%data = 1013.25 * (((288.15 - (0.0065 * HH%data))/288.15)**5.256) 

      END IF 
      PP%data = PP%data * 100.
      PP%qc   = 0

      
      
      

end Subroutine Aircraft_pressure

function psfc_from_QNH(alt, elev) result(psfc)
   real,  intent(in) :: alt  
   real,  intent(in) :: elev 
   real              :: psfc
   psfc = (alt**0.190284-(((1013.25**0.190284)*0.0065/288.15)*elev))**5.2553026
end function psfc_from_QNH

END MODULE module_decoded

