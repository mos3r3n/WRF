



































MODULE map_utils
































































































































IMPLICIT NONE

  
  REAL, PRIVATE, PARAMETER    :: pi = 3.1415927
  REAL, PRIVATE, PARAMETER    :: deg_per_rad = 180./pi
  REAL, PRIVATE, PARAMETER    :: rad_per_deg = pi / 180.

  


  REAL, PUBLIC , PARAMETER    :: earth_radius_m = 6370000. 
  REAL, PUBLIC , PARAMETER    :: radians_per_degree = pi / 180.

  
 
  
  INTEGER, PUBLIC, PARAMETER  :: PROJ_LATLON = 0
  INTEGER, PUBLIC, PARAMETER  :: PROJ_MERC = 1
  INTEGER, PUBLIC, PARAMETER  :: PROJ_LC = 3
  INTEGER, PUBLIC, PARAMETER  :: PROJ_PS = 5

   
  

  TYPE proj_info

    INTEGER          :: code     
    REAL             :: lat1    
    REAL             :: lon1    
    REAL             :: dx       
                                 
    REAL             :: dlat     
    REAL             :: dlon     
    REAL             :: stdlon   
    REAL             :: truelat1 
    REAL             :: truelat2 
    REAL             :: hemi     
    REAL             :: cone     
    REAL             :: polei    
    REAL             :: polej    
    REAL             :: rsw      
    REAL             :: rebydx   
    REAL             :: knowni   
    REAL             :: knownj   
    LOGICAL          :: init     
                                 
  END TYPE proj_info


CONTAINS

  SUBROUTINE map_init(proj)
    

    IMPLICIT NONE
    TYPE(proj_info), INTENT(INOUT)  :: proj

    proj%lat1 =    -999.9
    proj%lon1 =    -999.9
    proj%dx    =    -999.9
    proj%stdlon =   -999.9
    proj%truelat1 = -999.9
    proj%truelat2 = -999.9
    proj%hemi     = 0.0
    proj%cone     = -999.9
    proj%polei    = -999.9
    proj%polej    = -999.9
    proj%rsw      = -999.9
    proj%knowni   = -999.9
    proj%knownj   = -999.9
    proj%init     = .FALSE.
  
  END SUBROUTINE map_init

  SUBROUTINE map_set(proj_code,lat1,lon1,knowni,knownj,dx,stdlon,truelat1,truelat2,proj)
    
    
    
    
    
    
    
    
    

    IMPLICIT NONE
    
    
    INTEGER, INTENT(IN)               :: proj_code
    REAL, INTENT(IN)                  :: lat1
    REAL, INTENT(IN)                  :: lon1
    REAL, INTENT(IN)                  :: dx
    REAL, INTENT(IN)                  :: stdlon
    REAL, INTENT(IN)                  :: truelat1
    REAL, INTENT(IN)                  :: truelat2
    REAL, INTENT(IN)                  :: knowni , knownj
    TYPE(proj_info), INTENT(OUT)      :: proj

    


    

    
    IF ( ABS(lat1) .GT. 90. ) THEN
      WRITE(0,'(A)') 'Latitude of origin corner required as follows:'
      WRITE(0,'(A)') '    -90N <= lat1 < = 90.N'
      STOP 'MAP_INIT'
    ENDIF
    IF ( ABS(lon1) .GT. 180.) THEN
      WRITE(0,'(A)') 'Longitude of origin required as follows:'
      WRITE(0,'(A)') '   -180E <= lon1 <= 180W'
      STOP 'MAP_INIT'
    ENDIF
    IF ((dx .LE. 0.).AND.(proj_code .NE. PROJ_LATLON)) THEN
      WRITE(0,'(A)') 'Require grid spacing (dx) in meters be positive!'
      STOP 'MAP_INIT'
    ENDIF
    IF ((ABS(stdlon) .GT. 180.).AND.(proj_code .NE. PROJ_MERC)) THEN
      WRITE(0,'(A)') 'Need orientation longitude (stdlon) as: '
      WRITE(0,'(A)') '   -180E <= lon1 <= 180W' 
      STOP 'MAP_INIT'
    ENDIF
    IF (ABS(truelat1).GT.90.) THEN
      WRITE(0,'(A)') 'Set true latitude 1 for all projections!'
      STOP 'MAP_INIT'
    ENDIF
   
    CALL map_init(proj) 
    proj%code  = proj_code
    proj%lat1 = lat1
    proj%lon1 = lon1
    proj%knowni = knowni
    proj%knownj = knownj
    proj%dx    = dx
    proj%stdlon = stdlon
    proj%truelat1 = truelat1
    proj%truelat2 = truelat2
    IF (proj%code .NE. PROJ_LATLON) THEN
      proj%dx = dx
      IF (truelat1 .LT. 0.) THEN
        proj%hemi = -1.0 
      ELSE
        proj%hemi = 1.0
      ENDIF
      proj%rebydx = earth_radius_m / dx
    ENDIF
    pick_proj: SELECT CASE(proj%code)

      CASE(PROJ_PS)
        WRITE(0,'(A)') 'Setting up POLAR STEREOGRAPHIC map...'
        CALL set_ps(proj)

      CASE(PROJ_LC)
        WRITE(0,'(A)') 'Setting up LAMBERT CONFORMAL map...'
        IF (ABS(proj%truelat2) .GT. 90.) THEN
          WRITE(0,'(A)') 'Second true latitude not set, assuming a tangent'
          WRITE(0,'(A,F10.3)') 'projection at truelat1: ', proj%truelat1
          proj%truelat2=proj%truelat1
        ENDIF
        CALL set_lc(proj)
   
      CASE (PROJ_MERC)
        WRITE(0,'(A)') 'Setting up MERCATOR map...'
        CALL set_merc(proj)
   
      CASE (PROJ_LATLON)
        WRITE(0,'(A)') 'Setting up CYLINDRICAL EQUIDISTANT LATLON map...'
        
        IF (proj%lon1 .LT. 0.) proj%lon1 = proj%lon1 + 360.
   
      CASE DEFAULT
        WRITE(0,'(A,I2)') 'Unknown projection code: ', proj%code
        STOP 'MAP_INIT'
    
    END SELECT pick_proj
    proj%init = .TRUE.
    RETURN
  END SUBROUTINE map_set

  SUBROUTINE latlon_to_ij(proj, lat, lon, i, j)
    
    

    IMPLICIT NONE
    TYPE(proj_info), INTENT(IN)          :: proj
    REAL, INTENT(IN)                     :: lat
    REAL, INTENT(IN)                     :: lon
    REAL, INTENT(OUT)                    :: i
    REAL, INTENT(OUT)                    :: j

    IF (.NOT.proj%init) THEN
      WRITE(0,'(A)') 'You have not called map_set for this projection!'
      STOP 'LATLON_TO_IJ'
    ENDIF

    SELECT CASE(proj%code)
 
      CASE(PROJ_LATLON)
        CALL llij_latlon(lat,lon,proj,i,j)

      CASE(PROJ_MERC)
        CALL llij_merc(lat,lon,proj,i,j)
        i = i + proj%knowni - 1.0
        j = j + proj%knownj - 1.0

      CASE(PROJ_PS)
        CALL llij_ps(lat,lon,proj,i,j)
      
      CASE(PROJ_LC)
        CALL llij_lc(lat,lon,proj,i,j)
        i = i + proj%knowni - 1.0
        j = j + proj%knownj - 1.0

      CASE DEFAULT
        WRITE(0,'(A,I2)') 'Unrecognized map projection code: ', proj%code
        STOP 'LATLON_TO_IJ'
 
    END SELECT

    RETURN
  END SUBROUTINE latlon_to_ij

  SUBROUTINE ij_to_latlon(proj, ii, jj, lat, lon)
    
    

    IMPLICIT NONE
    TYPE(proj_info),INTENT(IN)          :: proj
    REAL, INTENT(IN)                    :: ii
    REAL, INTENT(IN)                    :: jj
    REAL, INTENT(OUT)                   :: lat
    REAL, INTENT(OUT)                   :: lon
    REAL         :: i, j

    IF (.NOT.proj%init) THEN
      WRITE(0,'(A)') 'You have not called map_set for this projection!'
      STOP 'IJ_TO_LATLON'
    ENDIF

    i = ii
    j = jj

    SELECT CASE (proj%code)

      CASE (PROJ_LATLON)
        CALL ijll_latlon(i, j, proj, lat, lon)

      CASE (PROJ_MERC)
        i = ii - proj%knowni + 1.0
        j = jj - proj%knownj + 1.0
        CALL ijll_merc(i, j, proj, lat, lon)

      CASE (PROJ_PS)
        CALL ijll_ps(i, j, proj, lat, lon)

      CASE (PROJ_LC)

        i = ii - proj%knowni + 1.0
        j = jj - proj%knownj + 1.0
        CALL ijll_lc(i, j, proj, lat, lon)

      CASE DEFAULT
        WRITE(0,'(A,I2)') 'Unrecognized map projection code: ', proj%code
        STOP 'IJ_TO_LATLON'

    END SELECT
    RETURN
  END SUBROUTINE ij_to_latlon

  SUBROUTINE set_ps(proj)
    
    
    
    
    IMPLICIT NONE
 
    
    TYPE(proj_info), INTENT(INOUT)    :: proj

    
    REAL                              :: ala1
    REAL                              :: alo1
    REAL                              :: reflon
    REAL                              :: scale_top

    
    reflon = proj%stdlon + 90.

    
    scale_top = 1. + proj%hemi * SIN(proj%truelat1 * rad_per_deg)

    
    ala1 = proj%lat1 * rad_per_deg
    proj%rsw = proj%rebydx*COS(ala1)*scale_top/(1.+proj%hemi*SIN(ala1))

    
    alo1 = (proj%lon1 - reflon) * rad_per_deg
    proj%polei = proj%knowni - proj%rsw * COS(alo1)
    proj%polej = proj%knownj - proj%hemi * proj%rsw * SIN(alo1)
    WRITE(0,'(A,2F10.1)')'Computed (I,J) of pole point: ',proj%polei,proj%polej
    RETURN
  END SUBROUTINE set_ps

  SUBROUTINE llij_ps(lat,lon,proj,i,j)
    
    
    
    

    IMPLICIT NONE

    
    REAL, INTENT(IN)               :: lat
    REAL, INTENT(IN)               :: lon
    TYPE(proj_info),INTENT(IN)     :: proj

    
    REAL, INTENT(OUT)              :: i 
    REAL, INTENT(OUT)              :: j 

    
    
    REAL                           :: reflon
    REAL                           :: scale_top
    REAL                           :: ala
    REAL                           :: alo
    REAL                           :: rm

    

  
    reflon = proj%stdlon + 90.
   
    

    scale_top = 1. + proj%hemi * SIN(proj%truelat1 * rad_per_deg)

    
    ala = lat * rad_per_deg
    rm = proj%rebydx * COS(ala) * scale_top/(1. + proj%hemi *SIN(ala))
    alo = (lon - reflon) * rad_per_deg
    i = proj%polei + rm * COS(alo)
    j = proj%polej + proj%hemi * rm * SIN(alo)
 
    RETURN
  END SUBROUTINE llij_ps

  SUBROUTINE ijll_ps(i, j, proj, lat, lon)

    
    
    

    IMPLICIT NONE

    
    REAL, INTENT(IN)                    :: i    
    REAL, INTENT(IN)                    :: j    
    TYPE (proj_info), INTENT(IN)        :: proj
    
    
    REAL, INTENT(OUT)                   :: lat     
    REAL, INTENT(OUT)                   :: lon     

    
    REAL                                :: reflon
    REAL                                :: scale_top
    REAL                                :: xx,yy
    REAL                                :: gi2, r2
    REAL                                :: arccos

    

    
    
    reflon = proj%stdlon + 90.
   
    
    scale_top = 1. + proj%hemi * SIN(proj%truelat1 * rad_per_deg)

    
    xx = i - proj%polei
    yy = (j - proj%polej) * proj%hemi
    r2 = xx**2 + yy**2

    
    IF (r2 .EQ. 0.) THEN 
      lat = proj%hemi * 90.
      lon = reflon
    ELSE
      gi2 = (proj%rebydx * scale_top)**2.
      lat = deg_per_rad * proj%hemi * ASIN((gi2-r2)/(gi2+r2))
      arccos = ACOS(xx/SQRT(r2))
      IF (yy .GT. 0) THEN
        lon = reflon + deg_per_rad * arccos
      ELSE
        lon = reflon - deg_per_rad * arccos
      ENDIF
    ENDIF
  
    
    IF (lon .GT. 180.) lon = lon - 360.
    IF (lon .LT. -180.) lon = lon + 360.
    RETURN
  
  END SUBROUTINE ijll_ps

  SUBROUTINE set_lc(proj)
    
    

    IMPLICIT NONE
    
    TYPE(proj_info), INTENT(INOUT)     :: proj

    REAL                               :: arg
    REAL                               :: deltalon1
    REAL                               :: tl1r
    REAL                               :: ctl1r

    
    CALL lc_cone(proj%truelat1, proj%truelat2, proj%cone)
    WRITE(0,'(A,F8.6)') 'Computed cone factor: ', proj%cone
    
    
    deltalon1 = proj%lon1 - proj%stdlon
    IF (deltalon1 .GT. +180.) deltalon1 = deltalon1 - 360.
    IF (deltalon1 .LT. -180.) deltalon1 = deltalon1 + 360.

    
    tl1r = proj%truelat1 * rad_per_deg
    ctl1r = COS(tl1r)

    
    proj%rsw = proj%rebydx * ctl1r/proj%cone * &
           (TAN((90.*proj%hemi-proj%lat1)*rad_per_deg/2.) / &
            TAN((90.*proj%hemi-proj%truelat1)*rad_per_deg/2.))**proj%cone

    
    arg = proj%cone*(deltalon1*rad_per_deg)
    proj%polei = 1. - proj%hemi * proj%rsw * SIN(arg)
    proj%polej = 1. + proj%rsw * COS(arg)  
    WRITE(0,'(A,2F10.3)') 'Computed pole i/j = ', proj%polei, proj%polej
    RETURN
  END SUBROUTINE set_lc                             

  SUBROUTINE lc_cone(truelat1, truelat2, cone)

  

    IMPLICIT NONE
    
    
    REAL, INTENT(IN)             :: truelat1  
    REAL, INTENT(IN)             :: truelat2  

    
    REAL, INTENT(OUT)            :: cone

    

    

    
    


    
    IF (ABS(truelat1-truelat2) .GT. 0.1) THEN
      cone = ALOG10(COS(truelat1*rad_per_deg)) - &
             ALOG10(COS(truelat2*rad_per_deg))
      cone = cone /(ALOG10(TAN((45.0 - ABS(truelat1)/2.0) * rad_per_deg)) - &
             ALOG10(TAN((45.0 - ABS(truelat2)/2.0) * rad_per_deg)))        
    ELSE
       cone = SIN(ABS(truelat1)*rad_per_deg )  
    ENDIF
  RETURN
  END SUBROUTINE lc_cone

  SUBROUTINE ijll_lc( i, j, proj, lat, lon)

  
  

  
  
  
    IMPLICIT NONE

    
    REAL, INTENT(IN)              :: i        
    REAL, INTENT(IN)              :: j        
    TYPE(proj_info),INTENT(IN)    :: proj     

    
    REAL, INTENT(OUT)             :: lat      
    REAL, INTENT(OUT)             :: lon      

    
    REAL                          :: inew
    REAL                          :: jnew
    REAL                          :: r
    REAL                          :: chi,chi1,chi2
    REAL                          :: r2
    REAL                          :: xx
    REAL                          :: yy

    

    chi1 = (90. - proj%hemi*proj%truelat1)*rad_per_deg
    chi2 = (90. - proj%hemi*proj%truelat2)*rad_per_deg

    
    
    IF (proj%hemi .EQ. -1.) THEN 
      inew = -i + 2.
      jnew = -j + 2.
    ELSE
      inew = i
      jnew = j
    ENDIF

    
    xx = inew - proj%polei
    yy = proj%polej - jnew
    r2 = (xx*xx + yy*yy)
    r = SQRT(r2)/proj%rebydx
   
    
    IF (r2 .EQ. 0.) THEN
      lat = proj%hemi * 90.
      lon = proj%stdlon
    ELSE
       
      
      lon = proj%stdlon + deg_per_rad * ATAN2(proj%hemi*xx,yy)/proj%cone
      lon = AMOD(lon+360., 360.)

      
      
      
      
        
      IF (chi1 .EQ. chi2) THEN
        chi = 2.0*ATAN( ( r/TAN(chi1) )**(1./proj%cone) * TAN(chi1*0.5) )
      ELSE
        chi = 2.0*ATAN( (r*proj%cone/SIN(chi1))**(1./proj%cone) * TAN(chi1*0.5)) 
      ENDIF
      lat = (90.0-chi*deg_per_rad)*proj%hemi

    ENDIF

    IF (lon .GT. +180.) lon = lon - 360.
    IF (lon .LT. -180.) lon = lon + 360.
    RETURN
    END SUBROUTINE ijll_lc

  SUBROUTINE llij_lc( lat, lon, proj, i, j)

  
  
    
    IMPLICIT NONE

    
    REAL, INTENT(IN)              :: lat      
    REAL, INTENT(IN)              :: lon      
    TYPE(proj_info),INTENT(IN)      :: proj     

    
    REAL, INTENT(OUT)             :: i        
    REAL, INTENT(OUT)             :: j        

    
    REAL                          :: arg
    REAL                          :: deltalon
    REAL                          :: tl1r
    REAL                          :: rm
    REAL                          :: ctl1r
    

    
    
    
    
    deltalon = lon - proj%stdlon
    IF (deltalon .GT. +180.) deltalon = deltalon - 360.
    IF (deltalon .LT. -180.) deltalon = deltalon + 360.
    
    
    tl1r = proj%truelat1 * rad_per_deg
    ctl1r = COS(tl1r)     
   
    
    rm = proj%rebydx * ctl1r/proj%cone * &
         (TAN((90.*proj%hemi-lat)*rad_per_deg/2.) / &
          TAN((90.*proj%hemi-proj%truelat1)*rad_per_deg/2.))**proj%cone

    arg = proj%cone*(deltalon*rad_per_deg)
    i = proj%polei + proj%hemi * rm * SIN(arg)
    j = proj%polej - rm * COS(arg)

    
    
    
    
    
    IF (proj%hemi .EQ. -1.) THEN
      i = 2. - i  
      j = 2. - j
    ENDIF
    RETURN
  END SUBROUTINE llij_lc

  SUBROUTINE set_merc(proj)
  
    

    IMPLICIT NONE
    TYPE(proj_info), INTENT(INOUT)       :: proj
    REAL                                 :: clain


    

    clain = COS(rad_per_deg*proj%truelat1)
    proj%dlon = proj%dx / (earth_radius_m * clain)

    
    

    proj%rsw = 0.
    IF (proj%lat1 .NE. 0.) THEN
      proj%rsw = (ALOG(TAN(0.5*((proj%lat1+90.)*rad_per_deg))))/proj%dlon
    ENDIF
    RETURN
  END SUBROUTINE set_merc

  SUBROUTINE llij_merc(lat, lon, proj, i, j)

    
  
    IMPLICIT NONE
    REAL, INTENT(IN)              :: lat
    REAL, INTENT(IN)              :: lon
    TYPE(proj_info),INTENT(IN)    :: proj
    REAL,INTENT(OUT)              :: i
    REAL,INTENT(OUT)              :: j
    REAL                          :: deltalon

    deltalon = lon - proj%lon1
    IF (deltalon .LT. -180.) deltalon = deltalon + 360.
    IF (deltalon .GT. 180.) deltalon = deltalon - 360.
    i = 1. + (deltalon/(proj%dlon*deg_per_rad))
    j = 1. + (ALOG(TAN(0.5*((lat + 90.) * rad_per_deg)))) / &
           proj%dlon - proj%rsw

    RETURN
  END SUBROUTINE llij_merc

  SUBROUTINE ijll_merc(i, j, proj, lat, lon)

    

    IMPLICIT NONE
    REAL,INTENT(IN)               :: i
    REAL,INTENT(IN)               :: j    
    TYPE(proj_info),INTENT(IN)    :: proj
    REAL, INTENT(OUT)             :: lat
    REAL, INTENT(OUT)             :: lon 


    lat = 2.0*ATAN(EXP(proj%dlon*(proj%rsw + j-1.)))*deg_per_rad - 90.
    lon = (i-1.)*proj%dlon*deg_per_rad + proj%lon1
    IF (lon.GT.180.) lon = lon - 360.
    IF (lon.LT.-180.) lon = lon + 360.
    RETURN
  END SUBROUTINE ijll_merc

  SUBROUTINE llij_latlon(lat, lon, proj, i, j)
 
    
    IMPLICIT NONE
    REAL, INTENT(IN)             :: lat
    REAL, INTENT(IN)             :: lon
    TYPE(proj_info), INTENT(IN)  :: proj
    REAL, INTENT(OUT)            :: i
    REAL, INTENT(OUT)            :: j

    REAL                         :: deltalat
    REAL                         :: deltalon
    REAL                         :: lon360
    REAL                         :: latinc
    REAL                         :: loninc

    
    
    

    latinc = proj%truelat1
    loninc = proj%stdlon

    
    

    deltalat = lat - proj%lat1

    
    
    IF (lon .LT. 0) THEN 
      lon360 = lon + 360. 
    ELSE 
      lon360 = lon
    ENDIF    
    deltalon = lon360 - proj%lon1      
    
    
    i = deltalon/loninc + 1.
    j = deltalat/latinc + 1.
    RETURN
    END SUBROUTINE llij_latlon

  SUBROUTINE ijll_latlon(i, j, proj, lat, lon)
 
    
    IMPLICIT NONE
    REAL, INTENT(IN)             :: i
    REAL, INTENT(IN)             :: j
    TYPE(proj_info), INTENT(IN)  :: proj
    REAL, INTENT(OUT)            :: lat
    REAL, INTENT(OUT)            :: lon

    REAL                         :: deltalat
    REAL                         :: deltalon
    REAL                         :: lon360
    REAL                         :: latinc
    REAL                         :: loninc

    
    
    

    latinc = proj%truelat1
    loninc = proj%stdlon

    

    deltalat = (j-1.)*latinc
    deltalon = (i-1.)*loninc
    lat = proj%lat1 + deltalat
    lon = proj%lon1 + deltalon

    IF ((ABS(lat) .GT. 90.).OR.(ABS(deltalon) .GT.360.)) THEN
      
      lat = -999.
      lon = -999.
    ELSE
      lon = lon + 360.
      lon = AMOD(lon,360.)
      IF (lon .GT. 180.) lon = lon -360.
    ENDIF

    RETURN
  END SUBROUTINE ijll_latlon
 

END MODULE map_utils
