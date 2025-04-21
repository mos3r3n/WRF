
MODULE module_obs_merge










USE module_type
USE module_func


CONTAINS









SUBROUTINE merge_obs ( first , second, print_duplicate, iunit)







   IMPLICIT NONE 

   TYPE ( report ) , INTENT ( INOUT )            :: first , &
                                                    second

   TYPE (measurement), pointer                   :: surface1, surface2
  
   INTEGER, INTENT (in)                          :: iunit
   LOGICAL, INTENT (in)                          :: print_duplicate

   INTEGER                                       :: best
   CHARACTER (LEN =  80) :: sub_name = "merge_obs"
   CHARACTER (LEN = 160) :: error_message
   LOGICAL :: fatal






   IF      ( first%info%num_vld_fld .GT. second%info%num_vld_fld ) THEN
      best = 1
   ELSE IF ( first%info%num_vld_fld .LT. second%info%num_vld_fld ) THEN
      best = 2
   ELSE IF ( first%info%num_error   .LT. second%info%num_error   ) THEN
      best = 1
   ELSE IF ( first%info%num_error   .GT. second%info%num_error   ) THEN
      best = 2
   ELSE IF ( first%info%num_warning .LT. second%info%num_warning ) THEN
      best = 1
   ELSE IF ( first%info%num_warning .GT. second%info%num_warning ) THEN
      best = 2
   ELSE IF ( first%info%seq_num     .GT. second%info%seq_num     ) THEN
      best = 1
   ELSE IF ( first%info%seq_num     .LT. second%info%seq_num     ) THEN
      best = 2
   ELSE
      best = 1

      IF (print_duplicate) THEN
      error_message = &
    " Arbitrarily assuming first obs is better than second for " // &


      TRIM ( first%info%platform) // '.'
      WRITE (UNIT = iunit, FMT = '(A)') TRIM (error_message)




      ENDIF

   END IF

   
   
   

   IF ( best .EQ. 2 ) THEN
      first%info = second%info
   END IF

   




   IF ( .NOT. ground_eq ( first%ground , second%ground ) ) THEN
      CALL keep_best ( first%ground%slp         , second%ground%slp         , best )
      CALL keep_best ( first%ground%ref_pres    , second%ground%ref_pres    , best )
      CALL keep_best ( first%ground%ground_t    , second%ground%ground_t    , best )
      CALL keep_best ( first%ground%sst         , second%ground%sst         , best )
      CALL keep_best ( first%ground%psfc        , second%ground%psfc        , best )
      CALL keep_best ( first%ground%precip      , second%ground%precip      , best )
      CALL keep_best ( first%ground%t_max       , second%ground%t_max       , best )
      CALL keep_best ( first%ground%t_min       , second%ground%t_min       , best )
      CALL keep_best ( first%ground%t_min_night , second%ground%t_min_night , best )
      CALL keep_best ( first%ground%p_tend03    , second%ground%p_tend03    , best )
      CALL keep_best ( first%ground%p_tend24    , second%ground%p_tend24    , best )
      CALL keep_best ( first%ground%cloud_cvr   , second%ground%cloud_cvr   , best )
      CALL keep_best ( first%ground%ceiling     , second%ground%ceiling     , best )
      CALL keep_best ( first%ground%pw          , second%ground%pw           , best ) 
      CALL keep_best ( first%ground%tb19v       , second%ground%tb19v        , best ) 
      CALL keep_best ( first%ground%tb19h       , second%ground%tb19h        , best ) 
      CALL keep_best ( first%ground%tb22v       , second%ground%tb22v        , best ) 
      CALL keep_best ( first%ground%tb37v       , second%ground%tb37v        , best ) 
      CALL keep_best ( first%ground%tb37h       , second%ground%tb37h        , best ) 
      CALL keep_best ( first%ground%tb85v       , second%ground%tb85v        , best ) 
      CALL keep_best ( first%ground%tb85h       , second%ground%tb85h        , best ) 

   END IF

   
   
   
   
 



 
   if ( (.not.associated(first%surface) .and. &
         .not.associated(second%surface)) .or. &
        (associated(first%surface) .and. &
         .not.associated(second%surface)) )then
       return





   else if (.not.associated(first%surface) .and. &
                 associated(second%surface)) then
        surface2 => second%surface
        first%surface => surface2
 
        return 
   endif

   CALL link_levels ( first%surface , second%surface , &
   first%info , second%info , best )

END SUBROUTINE merge_obs
  



SUBROUTINE keep_best ( field1 , field2 , best )





   IMPLICIT none

   TYPE ( field ) , INTENT ( INOUT )      :: field1
   TYPE ( field ) , INTENT ( IN )         :: field2
   INTEGER        , INTENT ( IN )         :: best

   CHARACTER ( LEN = 32 ) , PARAMETER     :: sub_name = 'keep_best'
   CHARACTER ( LEN = 80 )                 :: msg

   INCLUDE 'missing.inc'








   IF ( field_eq ( field1 , field2 ) ) THEN 

      

   ELSE IF ( (       eps_equal ( field1%data , missing_r , 1. ) ) .AND. &
             ( .NOT. eps_equal ( field2%data , missing_r , 1. ) ) ) THEN

      

      field1 = field2

   ELSE IF ( ( .NOT. eps_equal ( field1%data , missing_r , 1. ) ) .AND. &
             (       eps_equal ( field2%data , missing_r , 1. ) ) ) THEN

      

   ELSE IF ( (       eps_equal ( field1%data , missing_r , 1. ) ) .AND. &
             (       eps_equal ( field2%data , missing_r , 1. ) ) ) THEN

      

   ELSE IF ( ( .NOT. eps_equal ( field1%data , missing_r , 1. ) ) .AND. &
             ( .NOT. eps_equal ( field2%data , missing_r , 1. ) ) ) THEN

      

      

      IF ( field1%qc == missing ) THEN

         

          field1 = field2

      ELSE IF ( field2%qc == missing ) THEN
       
         
 
         

      ELSE IF ( field1%qc .LT. field2%qc ) THEN

         

      ELSE IF ( field1%qc .GT. field2%qc ) THEN

         field1 = field2

      ELSE IF ( field1%qc .EQ. field2%qc ) THEN

         


         IF ( best .EQ. 1 ) THEN

            

         ELSE IF ( best .EQ. 2 ) THEN

            field1 = field2

         ELSE


            msg = 'Internal logic error.  Invalid value of ''best'''
            CALL error_handler (sub_name, msg, "", .TRUE.)

         END IF

      ELSE  
 
         
         msg = 'Internal logic error.  Either the QCs are different or the same.'
         CALL error_handler (sub_name, msg, "", .TRUE. )

      END IF

   ELSE  

      
      msg = 'Internal logic error.  Only four combinations of fields missing are possible.'
      CALL error_handler (sub_name, msg , "", .TRUE.)

   END IF

END SUBROUTINE keep_best




SUBROUTINE link_levels ( list1 , list2 , info1 , info2 , best )






   IMPLICIT NONE

   TYPE ( measurement ) , POINTER           :: list1 , list2
   INTEGER , INTENT ( IN )                  :: best

   TYPE ( measurement ) , POINTER           :: next1 , &
                                               next2 , &
                                               current , &
                                               delete_it

   TYPE ( source_info )                     :: info1 , info2

   INCLUDE 'missing.inc'

   

   next1 => list1
   next2 => list2
   NULLIFY ( current )

   

   still_associated : DO WHILE ( ASSOCIATED ( next1 ) .AND. ASSOCIATED ( next2 ) )

      IF (    ( eps_equal ( next1%meas%pressure%data , & 
                            next2%meas%pressure%data , 1. ) ) &
                             .OR.  &
           (  ( eps_equal ( info1%elevation        , & 
                            next1%meas%height%data , .1 ) ) .AND. &
              ( eps_equal ( info2%elevation        , & 
                            next2%meas%height%data , .1 ) ) .AND. &
              ( .NOT. eps_equal ( info1%elevation , missing_r , 1. ) ) .AND. &
              ( eps_equal ( next1%meas%height%data , & 
                            next2%meas%height%data , .1 ) ) ) ) THEN

         
         
         
         
         
         

         CALL merge_measurements ( next1%meas , next2%meas , best )

         

         IF ( .NOT. ASSOCIATED ( current ) ) THEN
            
            
         ELSE
            current%next => next1
         END IF 
         current => next1         
         next1 => next1%next      
         delete_it => next2       
         next2 => next2%next      

         

         DEALLOCATE ( delete_it )

         
         
         
         

         duplicates_list1 : DO WHILE ( ASSOCIATED ( next1 ) ) 

            IF      (    ( eps_equal ( current%meas%pressure%data , next1%meas%pressure%data , 1. ) ) &
                                        .OR.  &
                      (  ( eps_equal ( current%meas%height%data   , next1%meas%height%data , .1 ) ) .AND. &
                         ( .NOT. eps_equal ( current%meas%height%data   , missing_r , 1. ) ) .AND. &
                         ( eps_equal ( info1%elevation            , next1%meas%height%data , .1 ) ) ) ) THEN
      
               CALL merge_measurements ( current%meas , next1%meas , best )
      
               
      
               delete_it => next1       
               next1 => next1%next      
      
               
      
               DEALLOCATE ( delete_it )
           
               
   
               CYCLE duplicates_list1
   
            ELSE
   
               
   
               EXIT duplicates_list1 
   
            END IF
         
         END DO duplicates_list1

         duplicates_list2 : DO WHILE ( ASSOCIATED ( next2 ) ) 

            IF      (    ( eps_equal ( current%meas%pressure%data , next2%meas%pressure%data , 1. ) ) &
                                        .OR.  &
                      (  ( eps_equal ( current%meas%height%data   , next2%meas%height%data , .1 ) ) .AND. &
                         ( .NOT. eps_equal ( current%meas%height%data   , missing_r , 1. ) ) .AND. &
                         ( eps_equal ( info2%elevation            , next2%meas%height%data , .1 ) ) ) ) THEN
      
               CALL merge_measurements ( current%meas , next2%meas , best )
      
               
      
               delete_it => next2       
               next2 => next2%next      
      
               
      
               DEALLOCATE ( delete_it )

            ELSE
   
               
   
               EXIT duplicates_list2 
   
            END IF
         
         END DO duplicates_list2

      ELSE IF ( next1%meas%pressure%data .LT. next2%meas%pressure%data ) THEN

         

         IF ( .NOT. ASSOCIATED ( current ) ) THEN
            
            list1 => next2
         ELSE
            current%next => next2
         END IF
         current => next2
         next2 => next2%next

      ELSE

         

         IF ( .NOT. ASSOCIATED ( current ) ) THEN
            
            
         ELSE
            current%next => next1
         END IF
         current => next1
         next1 => next1%next

      END IF

   END DO still_associated

   
   
   
   
   
   IF      ( ASSOCIATED ( next2 ) ) THEN
      current%next => next2
   ELSE IF ( ASSOCIATED ( next1 ) ) THEN
      current%next => next1
   ELSE
      NULLIFY ( current%next )
   END IF

END SUBROUTINE link_levels




SUBROUTINE merge_measurements ( first ,second , best )





   IMPLICIT NONE 

   TYPE ( meas_data ) , INTENT ( INOUT )       :: first
   TYPE ( meas_data ) , INTENT ( IN )          :: second
   INTEGER , INTENT ( IN )                     :: best

   CALL keep_best ( first%pressure     , second%pressure     , best ) 
   CALL keep_best ( first%height       , second%height       , best ) 




   CALL keep_best ( first%temperature  , second%temperature  , best ) 
   CALL keep_best ( first%dew_point    , second%dew_point    , best ) 
   CALL keep_best ( first%speed        , second%speed        , best ) 
   CALL keep_best ( first%direction    , second%direction    , best ) 
   CALL keep_best ( first%u            , second%u            , best ) 
   CALL keep_best ( first%v            , second%v            , best ) 
   CALL keep_best ( first%rh           , second%rh           , best ) 
   CALL keep_best ( first%thickness    , second%thickness    , best ) 
   
END SUBROUTINE merge_measurements

END MODULE module_obs_merge
