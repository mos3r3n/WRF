MODULE MODULE_ICAO

















  include 'constants.inc'
  include 'missing.inc'

  REAL, PARAMETER :: p_0    = 101325.,  & 
                     t_0    = 288.,     & 
                     lambda = 0.0065,   & 
                     alpha  = lambda * gasr / g 

  REAL            :: height_max_icao

CONTAINS


 FUNCTION t_from_h_icao (h) RESULT (t)

 IMPLICIT NONE
 REAL :: h, t

 t = t_0 - lambda * h

 END FUNCTION t_from_h_icao

 FUNCTION t_from_p_icao (p) RESULT (t)

 IMPLICIT NONE
 REAL :: p, t

 t = t_0 *(p / p_0) ** alpha

 END FUNCTION t_from_p_icao

 FUNCTION h_from_p_icao (p) RESULT (h)

 IMPLICIT NONE
 REAL :: p, h

 h = t_0 / lambda * (1. - (p / p_0) ** alpha)

 END FUNCTION h_from_p_icao

 FUNCTION p_from_h_icao (h) RESULT (p)

 IMPLICIT NONE
 REAL :: p, h, one_over_alpha

 one_over_alpha = 1. /alpha

   p = p_0 * (1. - lambda * h / t_0)** one_over_alpha

 END FUNCTION p_from_h_icao

END MODULE MODULE_ICAO
