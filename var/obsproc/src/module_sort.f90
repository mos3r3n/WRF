
MODULE module_sort








USE module_type
USE module_func

CONTAINS






 SUBROUTINE sort_obs ( obs , array_size , compare, index )






   IMPLICIT NONE

   INTEGER , INTENT ( IN )                            :: array_size
   TYPE ( report ) , INTENT ( IN ) , DIMENSION ( : )  :: obs
   INTEGER , INTENT ( OUT )        , DIMENSION ( : )  :: index
   
   INTEGER                                            :: i

   INTERFACE 
    LOGICAL FUNCTION compare (a, b, flag)
      USE module_type
      IMPLICIT NONE
      INTEGER         , INTENT ( IN )     :: flag
      TYPE ( report ) , INTENT ( IN )     :: a  
      TYPE ( report ) , INTENT ( IN )     :: b  
    END FUNCTION compare
   END INTERFACE 

   
   


    do i = 1 , array_size
      index(i) = i
    enddo

   
   
   
   

   CALL merge_sort ( obs , index , 1 , array_size, compare )

  
END SUBROUTINE sort_obs




RECURSIVE SUBROUTINE merge_sort ( obs , index , low , high, compare )









   IMPLICIT NONE

   TYPE ( report )        , INTENT ( IN    ) , DIMENSION ( : ) :: obs 
   INTEGER                , INTENT ( INOUT ) , DIMENSION ( : ) :: index
   INTEGER                , INTENT ( IN    )                   :: low        , &
                                                                  high

   INTEGER , ALLOCATABLE                     , DIMENSION ( : ) :: tmp_old
   INTEGER                                                        mid        , &
                                                                  current    , & 
                                                                  first_ndx  , & 
                                                                  second_ndx , &
                                                                  temp
   INTERFACE 
    LOGICAL FUNCTION compare (a, b, flag)
      USE module_type
      IMPLICIT NONE
      INTEGER         , INTENT ( IN )     :: flag
      TYPE ( report ) , INTENT ( IN )     :: a  
      TYPE ( report ) , INTENT ( IN )     :: b  
    END FUNCTION compare
   END INTERFACE 


   
   

   break_it_down : IF ( high - low .GE. 2 ) THEN


     

      

      mid = ( low + high ) / 2

      

      CALL merge_sort ( obs , index , low , mid , compare)

      CALL merge_sort ( obs , index , mid + 1 , high, compare)


      
      

      ALLOCATE ( tmp_old ( low : mid ) )

      

      tmp_old = index ( low : mid )

      

      first_ndx = low
      second_ndx = mid + 1
      current = low

      

      sort_two_groups : DO WHILE (first_ndx .LE. mid .AND. second_ndx .LE. high)

         
         
         



         IF ( compare (obs(tmp_old(first_ndx)) , obs(index(second_ndx)), 1 ) &
             ) THEN
            index(current) = tmp_old(first_ndx)
            first_ndx = first_ndx + 1
         ELSE 
            index(current) = index(second_ndx)
            second_ndx = second_ndx + 1
         ENDIF

         
         

         current = current + 1

      END DO sort_two_groups

      
      
      

      tail_of_first_group : DO WHILE ( first_ndx .LE. mid )
         index(current) = tmp_old(first_ndx)
         current = current + 1
         first_ndx = first_ndx + 1
      END DO tail_of_first_group

      

      DEALLOCATE ( tmp_old )

   ELSE break_it_down

      
      
      



      small_enough : IF ( compare ( obs(index(high)) , obs(index(low)), 0) &
                         ) THEN
         temp = index(low)
         index(low) = index(high)
         index(high) = temp       
      END IF small_enough

   END IF break_it_down

END SUBROUTINE merge_sort

END MODULE module_sort

