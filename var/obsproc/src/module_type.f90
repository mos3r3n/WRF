MODULE MODULE_TYPE








   TYPE location_type

      
      
      
      
      


      REAL                   :: latitude  , &   
                                longitude       

      CHARACTER ( LEN = 40 ) :: id , &          
                                                
                                                
                                                
                                                
                                                
                                name            
                                                
                                                
                                                



      REAL                   :: yic         , & 
                                xjc         , & 
                                yid         , & 
                                xjd             


   END TYPE location_type




   TYPE source_info



      CHARACTER ( LEN = 40 ) :: platform , &    
                                                
                                source          
                                                
      REAL                   :: elevation       

      
      
      
      
      

      INTEGER              :: num_vld_fld , & 
                                              
                                              
                                              
                                              
                              num_error , &   
                                              
                                              
                              num_warning , & 
                                              
                                              
                              seq_num , &     
                                              
                                              
                              num_dups        
                                              
      LOGICAL              :: is_sound        
                                              
                                              
                                              
      LOGICAL              :: bogus           
                                              
      LOGICAL              :: discard         
                                              
                                              
                                              
      INTEGER              :: levels          

   END TYPE source_info



   TYPE field

      
      
      



      REAL                   :: data            
      INTEGER                :: qc              
                                                
                                                




      REAL                   :: error           
                                                
   END TYPE field



   TYPE terrestrial

      
      
      
      
      



      TYPE ( field )         :: slp       , &   
                                ref_pres  , &   
                                                
                                ground_t  , &   
                                sst       , &   
                                psfc      , &   
                                precip    , &   
                                t_max     , &   
                                t_min     , &   
                                t_min_night , & 
                                p_tend03  , &   
                                p_tend24  , &   
                                cloud_cvr , &   
                                ceiling   , &   



                                pw        , &   
                                tb19v     , &   
                                tb19h     , &   
                                tb22v     , &  
                                tb37v     , &
                                tb37h     , &
                                tb85v     , &
                                tb85h
   END TYPE terrestrial 



   TYPE time_info

      
      
      



      INTEGER                :: sut      , &    
                                                
                                julian          
      CHARACTER ( LEN = 14 )    date_char       



      CHARACTER (LEN = 19)   :: date_mm5

   END TYPE time_info



   TYPE meas_data

      
      
      
      
      
      



      TYPE ( field )         :: pressure    , & 
                                height      , & 
                                temperature , & 
                                dew_point   , & 
                                speed       , & 
                                direction   , & 
                                u           , & 
                                v           , & 
                                rh          , & 
                                thickness   , & 
                                qv              

   END TYPE meas_data



   TYPE measurement

      TYPE ( meas_data )               :: meas  
      TYPE ( measurement ) ,  POINTER  :: next  
                                                
                                                

   END TYPE measurement



   TYPE report                                 
                                               
      TYPE ( location_type ) :: location       
      TYPE ( source_info )   :: info           
      TYPE ( time_info )     :: valid_time     
      TYPE ( terrestrial )   :: ground         
      TYPE ( measurement ) , &
               POINTER       :: surface        

   END TYPE report                            
                                             


   TYPE crs_or_dot

        REAL :: crs
        REAL :: dot

   END TYPE crs_or_dot



   TYPE model_profile

        TYPE (crs_or_dot) :: height
        TYPE (crs_or_dot) :: pressure
        REAL              :: speed
        REAL              :: direction
        REAL              :: u
        REAL              :: v
        REAL              :: t
        REAL              :: td
        REAL              :: rh
        REAL              :: qv

   END TYPE model_profile



END MODULE MODULE_TYPE
