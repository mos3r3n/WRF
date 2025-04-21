MODULE module_func







   USE module_type

CONTAINS





FUNCTION compare_loc ( a , b, flag ) RESULT (compare)





   IMPLICIT NONE

   INTEGER         , INTENT ( IN )     :: flag
   TYPE ( report ) , INTENT ( IN )     :: a  
   TYPE ( report ) , INTENT ( IN )     :: b  
   LOGICAL                             :: compare

   integer           :: a1, b1

   compare = .FALSE.

   read (a%info%platform(4:6),'(I3)') a1
   read (b%info%platform(4:6),'(I3)') b1

   IF ( a1 < b1 ) THEN
      compare = .TRUE.
   ELSE IF ( a%info%platform(4:6) .EQ. b%info%platform(4:6) ) THEN 
     IF ( a%location%longitude .LT. b%location%longitude ) THEN
        compare = .TRUE.
     ELSE IF ( a%location%longitude .eq. b%location%longitude ) THEN
       IF ( a%location%latitude .LT. b%location%latitude ) THEN
          compare = .TRUE. 
       ELSE IF ( a%location%latitude .EQ. b%location%latitude ) THEN
          IF ( LLT ( a%location%id , b%location%id ) ) THEN
            compare = .TRUE.
          ELSE IF ( a%location%id .EQ. b%location%id ) THEN
            IF ( LLT ( a%location%name , b%location%name ) ) THEN
               compare = .TRUE.
            END IF
          END IF
       END IF
     END IF
   ENDIF

   IF (flag == 1) then

   IF ( a%info%platform(4:6) .EQ. b%info%platform(4:6) .and. &
        a%location%longitude .EQ. b%location%longitude .and. &
        a%location%latitude  .EQ. b%location%latitude  .and. &
        a%location%id        .EQ. b%location%id        .and. &
        a%location%name      .EQ. b%location%name        ) THEN
        compare = .TRUE.
   ENDIF

   ENDIF

END FUNCTION compare_loc





FUNCTION compare_tim ( a , b, flag ) RESULT (compare)





   USE module_date

   IMPLICIT NONE

   INTEGER         , INTENT ( IN )     :: flag
   TYPE ( report ) , INTENT ( IN )     :: a  
   TYPE ( report ) , INTENT ( IN )     :: b  
   LOGICAL                             :: compare

   CHARACTER (LEN = 19)                :: time_a, time_b
   INTEGER                             :: its, fma, fmb

   

   WRITE (time_a, FMT='(A4,"-",A2,"-",A2,"_",A2,":",A2,":",A2)') &
          a % valid_time % date_char ( 1: 4), &
          a % valid_time % date_char ( 5: 6), &
          a % valid_time % date_char ( 7: 8), &
          a % valid_time % date_char ( 9:10), &
          a % valid_time % date_char (11:12), &
          a % valid_time % date_char (13:14)

   

   WRITE (time_b, FMT='(A4,"-",A2,"-",A2,"_",A2,":",A2,":",A2)') &
          b % valid_time % date_char ( 1: 4), &
          b % valid_time % date_char ( 5: 6), &
          b % valid_time % date_char ( 7: 8), &
          b % valid_time % date_char ( 9:10), &
          b % valid_time % date_char (11:12), &
          b % valid_time % date_char (13:14)

   

   CALL GETH_IDTS (time_a,  time_b, its)

   

   compare = .FALSE.

   

   IF (its .LT.  0) THEN

      compare = .TRUE.

   ELSE IF (its .EQ.  0) THEN

   

        READ (a % info % platform (4:6), '(I3)') fma
        READ (b % info % platform (4:6), '(I3)') fmb

        IF (fma .LT. fmb) THEN

             compare = .TRUE.

        ELSE IF (fma .EQ. fmb) THEN

             

             IF (compare_loc (a, b, flag)) THEN

                 compare = .TRUE.

             ENDIF

        ENDIF

   END IF



END FUNCTION compare_tim




LOGICAL FUNCTION eps_equal ( a , b , eps )




   IMPLICIT NONE 

   REAL , INTENT ( IN )                     :: a , b , eps

   IF ( ABS ( a - b ) .LT. eps ) THEN
      eps_equal = .TRUE.
   ELSE
      eps_equal = .FALSE.
   END IF

END FUNCTION eps_equal
      



LOGICAL FUNCTION field_eq ( a , b )



   IMPLICIT NONE 

   TYPE ( field ) , INTENT ( IN )                :: a , b
   
   IF ( a%data .EQ. b%data .AND. a%qc .EQ. b%qc ) THEN
      field_eq = .TRUE.
   ELSE
      field_eq = .FALSE.
   END IF
  
END FUNCTION field_eq




LOGICAL FUNCTION ground_eq ( a , b )



   IMPLICIT NONE 

   TYPE ( terrestrial ) , INTENT ( IN )    :: a , b

   IF ( eps_equal ( a%slp%data         , b%slp%data         , .01 ) .AND. &
        eps_equal ( a%ref_pres%data    , b%ref_pres%data    , .01 ) .AND. &
        eps_equal ( a%ground_t%data    , b%ground_t%data    , .01 ) .AND. &
        eps_equal ( a%sst%data         , b%sst%data         , .01 ) .AND. &
        eps_equal ( a%psfc%data        , b%psfc%data        , .01 ) .AND. &
        eps_equal ( a%precip%data      , b%precip%data      , .01 ) .AND. &
        eps_equal ( a%t_max%data       , b%t_max%data       , .01 ) .AND. &
        eps_equal ( a%t_min%data       , b%t_min%data       , .01 ) .AND. &
        eps_equal ( a%t_min_night%data , b%t_min_night%data , .01 ) .AND. &
        eps_equal ( a%p_tend03%data    , b%p_tend03%data    , .01 ) .AND. &
        eps_equal ( a%p_tend24%data    , b%p_tend24%data    , .01 ) .AND. &
        eps_equal ( a%cloud_cvr%data   , b%cloud_cvr%data   , .01 ) .AND. &
        eps_equal ( a%ceiling%data     , b%ceiling%data     , .01 ) .AND. &
        eps_equal ( a%pw%data     ,      b%pw%data          , .01 ) .AND. &
        eps_equal ( a%tb19v%data  ,      b%tb19v%data       , .01 ) .AND. &
        eps_equal ( a%tb19h%data  ,      b%tb19h%data       , .01 ) .AND. &
        eps_equal ( a%tb22v%data  ,      b%tb22v%data       , .01 ) .AND. &
        eps_equal ( a%tb37v%data  ,      b%tb37v%data       , .01 ) .AND. &
        eps_equal ( a%tb37h%data  ,      b%tb37h%data       , .01 ) .AND. &
        eps_equal ( a%tb85v%data  ,      b%tb85v%data       , .01 ) .AND. &
        eps_equal ( a%tb85h%data  ,      b%tb85h%data       , .01 ) .AND. &
        a%slp%qc  .EQ. b%slp%qc  .AND. a%ref_pres%qc  .EQ. b%ref_pres%qc    .AND. &
        a%ground_t%qc  .EQ. b%ground_t%qc  .AND. a%sst%qc  .EQ. b%sst%qc  .AND. &
        a%psfc%qc  .EQ. b%psfc%qc .AND.  a%precip%qc  .EQ. b%precip%qc    .AND. &
        a%t_max%qc .EQ. b%t_max%qc .AND.  a%t_min%qc  .EQ. b%t_min%qc     .AND. &
        a%t_min_night%qc .EQ. b%t_min_night%qc .AND. &
        a%p_tend03%qc .EQ. b%p_tend03%qc .AND. &
        a%p_tend24%qc .EQ. b%p_tend24%qc .AND. &
        a%cloud_cvr%qc  .EQ. b%cloud_cvr%qc  .AND. &
        a%ceiling%qc .EQ. b%ceiling%qc .AND.  a%pw%qc   .EQ. b%pw%qc     .AND. &
        a%tb19v%qc  .EQ. b%tb19v%qc  .AND.  a%tb19h%qc  .EQ. b%tb19h%qc  .AND. &
        a%tb22v%qc  .EQ. b%tb22v%qc  .AND.  a%tb37v%qc  .EQ. b%tb37v%qc  .AND. &
        a%tb37h%qc  .EQ. b%tb37h%qc  .AND.  a%tb85v%qc  .EQ. b%tb85v%qc  .AND. &
        a%tb85h%qc  .EQ. b%tb85h%qc) THEN
        ground_eq = .TRUE.
   ELSE
        ground_eq = .FALSE.
   END IF
  
END FUNCTION ground_eq




LOGICAL FUNCTION loc_eq ( a , b )



   IMPLICIT NONE 

   TYPE ( report ) , INTENT ( IN )     :: a  
   TYPE ( report ) , INTENT ( IN )     :: b  
   
   IF ( eps_equal(a%location%latitude ,b%location%latitude , .001) .AND. &
        eps_equal(a%location%longitude,b%location%longitude, .001) .AND. &
        a%location%id   .EQ. b%location%id   .AND. &
        a%location%name .EQ. b%location%name .AND. &
        a%info%platform(4:6) .EQ. b%info%platform(4:6) ) THEN
      loc_eq = .TRUE.
   ELSE
      loc_eq = .FALSE.
   END IF
  
END FUNCTION loc_eq




LOGICAL FUNCTION time_eq_old ( a , b )



   IMPLICIT NONE 

   TYPE ( time_info ) , INTENT ( IN )           :: a , b
   
   IF ( ( a%sut       .EQ. b%sut       ) .AND. &
        ( a%julian    .EQ. b%julian    ) .AND. &
        ( a%date_char .EQ. b%date_char ) ) THEN
      time_eq_old = .TRUE.
   ELSE
      time_eq_old = .FALSE.
   END IF
  
END FUNCTION time_eq_old




LOGICAL FUNCTION time_eq ( a , b , date , time )



   USE module_date

   IMPLICIT NONE 

   TYPE ( time_info ) , INTENT ( INOUT )        :: a , b
   INTEGER            , INTENT ( IN )           :: date , time 

   

   CHARACTER (LEN=19)               :: target_date , a_date , b_date
   INTEGER                          :: diff_seconds , a_diff_seconds , b_diff_seconds

   

   WRITE (target_date, '(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2)' ) &
   date / 10000 ,  &
   ( date - (date / 10000 ) * 10000 ) / 100 , &
   date - ( date / 100 ) * 100 , &
   time / 10000 , &
   ( time - ( time / 10000 ) * 10000 ) / 100, &
   time - ( time / 100 ) * 100

   

   a_date( 1: 5) = a%date_char( 1: 4) // '-'
   a_date( 6: 8) = a%date_char( 5: 6) // '-'
   a_date( 9:11) = a%date_char( 7: 8) // '_'
   a_date(12:14) = a%date_char( 9:10) // ':'
   a_date(15:17) = a%date_char(11:12) // ':'
   a_date(18:19) = a%date_char(13:14)

   b_date( 1: 5) = b%date_char( 1: 4) // '-'
   b_date( 6: 8) = b%date_char( 5: 6) // '-'
   b_date( 9:11) = b%date_char( 7: 8) // '_'
   b_date(12:14) = b%date_char( 9:10) // ':'
   b_date(15:17) = b%date_char(11:12) // ':'
   b_date(18:19) = b%date_char(13:14)

   

   CALL geth_idts ( a_date , b_date , diff_seconds )
   
   
   

   IF ( ABS ( diff_seconds ) .LT. 1800 ) THEN

      
      
      
      

      CALL geth_idts ( target_date , a_date , a_diff_seconds )
      CALL geth_idts ( target_date , b_date , b_diff_seconds )

      IF ( ( ABS ( a_diff_seconds ) .LT. 3600 ) .OR. &
           ( ABS ( b_diff_seconds ) .LT. 3600 ) ) THEN

         a%date_char( 1: 4) = target_date( 1: 4)
         a%date_char( 5: 6) = target_date( 6: 7) 
         a%date_char( 7: 8) = target_date( 9:10) 
         a%date_char( 9:10) = target_date(12:13) 
         a%date_char(11:12) = target_date(15:16) 
         a%date_char(13:14) = target_date(18:19) 

         b%date_char( 1: 4) = target_date( 1: 4)
         b%date_char( 5: 6) = target_date( 6: 7) 
         b%date_char( 7: 8) = target_date( 9:10) 
         b%date_char( 9:10) = target_date(12:13) 
         b%date_char(11:12) = target_date(15:16) 
         b%date_char(13:14) = target_date(18:19) 
      END IF

      time_eq = .TRUE.

   ELSE

      time_eq = .FALSE.

   END IF
  
END FUNCTION time_eq


FUNCTION info_levels (surface) RESULT (levels)





   IMPLICIT NONE

   TYPE ( measurement ) ,  POINTER         :: surface
   INTEGER                                 :: levels

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

END FUNCTION info_levels


END MODULE module_func
