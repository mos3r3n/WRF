subroutine da_radar_rf_2mom (qr, qs, qg, qnr, temp, rho, rze)

   !-----------------------------------------------------------------------
   ! Purpose: calculate background/model radar equivalent reflectivity factor
   !-----------------------------------------------------------------------

   implicit none

   real, intent(in)    :: qr, qs, qg  ! rain, snow and graupel mixing ratio (kg kg^-1)
   real, intent(in)    :: qnr         ! number of rain
   real, intent(in)    :: temp        ! air temperature (K)
   real, intent(in)    :: rho         ! air density (kg m^-3)
   real, intent(inout) :: rze         ! equivalent reflectivity factor (mm^6 m^-3)
   
   real :: zer, zes, zeg
   real :: para_r, para_dg, para_wg, para_ds, para_ws
   real :: p_r, p_ws, p_wg, p_dg, ds_para_a, ds_para_b
   real :: pi !!, rhor

   if (trace_use) call da_trace_entry("da_radar_rf_2mom")
 
   ! parameters
   pi = 3.14159265
!   rhor = 1000.
   p_r  = 2.0
   p_wg = 2.5
   p_dg = 2.5
   p_ws = 2.67
   para_r  = 7.295125e13
!   para_ds = (0.176/0.93) * (6.0/pi)**2 *(0.069/900.0)**2 *1.e18
   para_ds = 4057379571.35256
   para_ws = 1.47e5
   para_wg = 5.54914e12
   para_dg = 1.90409e12
   
   ! get parameters for dry snow
   if (temp .le. 273.15) call cal_coef_wetsnow(temp,ds_para_a, ds_para_b)   
   
   ! rain water
   if (qr .gt. 1.e-7 .and. qnr .gt. 1.) then
      zer = para_r * (rho*qr)**p_r /(rho*qnr)
!      zer = 720.*10.0e18 *(rho *qr)**2 /(pi**2*rhor**2*qnr*rho)
   else
      zer = 1.e-8
   end if
  
   ! graupel
   if (qg .gt. 1.e-7) then
     if (temp .gt. 273.15 ) then  ! wet graupel
        zeg = para_wg*rho**1.75*qg**p_wg
     else ! dry graupel
        zeg = para_dg*rho**1.75*qg**p_dg
     end if
   else
     zeg = 1.e-8 
   end if

   ! snow
   if (qs .gt. 1.e-7) then
     if (temp .gt. 273.15 ) then  ! wet graupel
        zes = para_ws* (qs*1000.)**p_ws
     else ! dry graupel
        zes = para_ds* ds_para_a * (qs*rho/0.069)**ds_para_b
     end if
   else
     zes = 1.e-8
   end if
   
   rze = zer+zes+zeg
   if (rze .lt. 1.0e-20) rze = 1.0e-20

   if (trace_use) call da_trace_exit("da_radar_rf_2mom")

end subroutine da_radar_rf_2mom
