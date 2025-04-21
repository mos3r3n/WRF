subroutine da_dualpol_radar_forward(qr, qs, qg, nr, ns, ng, rho, zh, zdr, kdp, cc)

   implicit none

   real,  intent(in)  :: qr, qs, qg, nr, ns, ng, rho
   real,  intent(out) :: zh, zdr, kdp, cc

   ! local variables
   integer :: i, j
   real :: dm_r, dm_s, dm_g
   real :: w_r, z_s, z_g 
   real :: rho_r, rho_s, rho_g, rho_ds, rho_dg
   real :: gamma_s, gamma_g, pie

   real :: zh_r, zh_s, zh_g
   real :: zdr_r, zdr_s, zdr_g
   real :: kdp_r, kdp_s, kdp_g
   real :: cc_r, cc_s, cc_g, cc_all

   real :: coeff_zh_s(4), coeff_zdr_s(3), coeff_kdp_s(3), coeff_cc_s(3)
   real :: coeff_zh_g(4), coeff_zdr_g(3), coeff_kdp_g(3), coeff_cc_g(3)

   !! coefficients
   ! rain
   real :: coeff_zh_r (5) = (/-0.3078, 20.87, 46.04, 6.403, 0.2248/)
   real :: coeff_zdr_r(5) = (/1.019, -0.143, 0.3165, 0.06498, 0.004163/)
   real :: coeff_kdp_r(5) = (/0.00926, -0.087, 0.1994, 0.02824, 0.001772/)
   real :: coeff_cc_r (5) = (/0.9987, 0.008289, -0.0116, 0.003513, -0.0003187/)

   ! snow
   real :: coeff_zh_snow(4,4) = reshape([ &
		    0.0524000,  1.6980000,  1.1850000, -2.0630000, &
                   -0.0018860, -0.0284600, -0.0281200, -0.0061900, &
                   -0.0000401,  0.0068460, -0.0307100,  0.0469700, &
                    0.0000049, -0.0003777,  0.0016490, -0.0022780 ], [4, 4])
   real :: coeff_zdr_snow(3, 4) = reshape([ &
		    1.0180000,  0.8789000, -0.0736000, -0.2990000, &
                    0.0014320, -0.0227400,  0.2280000, -0.1841000, &
                    0.0004199,  0.0000723, -0.0013050, -0.0026580 ], [3, 4])
   real :: coeff_kdp_snow (3, 4) = reshape([ &
		    0.0011800,  0.1465000,  4.0060000, -3.3560000, &
                    0.0016500, -0.0765500,  0.3985000, -0.2848000, &
                   -0.0000777,  0.0023220, -0.0132700,  0.0066200 ], [3, 4])
   real :: coeff_cc_snow (3, 4) = reshape([ &
		    0.9975000, -0.0101500, -0.0093160,  0.0011870, &
                    0.0001041,  0.0145200, -0.0503400,  0.0296100, &
                   -0.0000137, -0.0010390,  0.0027120, -0.0013910 ], [3, 4])
   ! graupel
   real :: coeff_zh_graupel (4, 4) = reshape([ &
		       0.2929000,  3.3810000, -4.6200000,  2.0670000, &
                      -0.0126500,  0.1995000, -1.2870000,  1.3040000, &
                       0.0012220, -0.0345500,  0.1818000, -0.1624000, &
                      -0.0000437,  0.0010260, -0.0054600,  0.0045330 ], [4, 4])
   real :: coeff_zdr_graupel (3, 4) = reshape([ &
		       1.0166000,  0.6206000, -0.7519000,  1.4930000, &
                       0.0022590, -0.0628000,  0.4363000, -0.3795000, & 
                      -0.0000423,  0.0040270, -0.0206100,  0.0156400 ], [3, 4])
   real :: coeff_kdp_graupel (3, 4) = reshape([ &
		       0.0088920,  0.8146000,  0.5967000,  0.5884000, &
                       0.0007914, -0.0432900,  0.4235000, -0.3595000, &
                      -0.0000576,  0.0022990, -0.0205400,  0.0143300 ], [3, 4])
   real :: coeff_cc_graupel  (3, 4) = reshape([ &
		       0.9922000, -0.0853100,  0.2423000, -0.1572000, &
                       0.0013040, -0.0071040, -0.0229300,  0.0254800, &
                      -0.0000917, -0.0009716,  0.0040210, -0.0028070 ], [3, 4])

   ! percentage of melting
   gamma_s = qr / (qr + qs)
   gamma_g = qr / (qr + qg)

   ! hydrometeor particle density
   rho_r = 1000.; rho_ds = 100.; rho_dg = 500.
   rho_s = rho_ds*(1.-gamma_s**2.0) + rho_r*gamma_s**2.0
   rho_g = rho_dg*(1.-gamma_g**2.0) + rho_r*gamma_g**2.0

   ! mass-weighted diameter Dm
   dm_r = 4. * (rho * qr / pie / rho_r / nr)**(1. / 3.)
   dm_s = 4. * (rho * qs / pie / rho_s / nr)**(1. / 3.)
   dm_g = 4. * (rho * qs / pie / rho_g / nr)**(1. / 3.)

   ! Water content
   w_r = rho * qr

   ! 6th moment
   pie = 3.14159265
   z_s = 11.25 * 1000. * rho * qs / pie / rho_s * dm_s**3.
   z_g = 11.25 * 1000. * rho * qg / pie / rho_g * dm_g**3.

   !----------------------------------------------------- rain water
   zh_r = 0.; zdr_r = 0.; kdp_r = 0.; cc_r = 0.
   do i = 1, 5
      zh_r  = zh_r  + coeff_zh_r(i)  * dm_r ** (i - 1)
      zdr_r = zdr_r + coeff_zdr_r(i) * dm_r ** (i - 1)
      kdp_r = kdp_r + coeff_kdp_r(i) * dm_r ** (i - 1)
      cc_r  = kdp_r + coeff_cc_r(i)  * dm_r ** (i - 1)
   end do
   zh_r  = zh_r**2. * w_r
   kdp_r = kdp_r * w_r

   !----------------------------------------------------- snow
   zh_s = 0.; zdr_s = 0.; kdp_s = 0.; cc_s = 0.
   do i = 1, 4
      ! calculate coefficients as a function of gamma_s
      do j = 1, 4
         coeff_zh_s(i)  = coeff_zh_s(i)  + coeff_zh_snow(i, j)  * gamma_s**j
	 if (i .le. 3) then 
	    coeff_zdr_s(i) = coeff_zdr_s(i) + coeff_zdr_snow(i, j) * gamma_s**j
	    coeff_kdp_s(i) = coeff_kdp_s(i) + coeff_kdp_snow(i, j) * gamma_s**j
	    coeff_cc_s(i)  = coeff_cc_s(i)  + coeff_cc_snow(i, j)  * gamma_s**j 
	 end if
      end do
      ! calculate each dual-pol variables
      zh_s  = zh_s  + coeff_zh_s(i)  * dm_s ** (i - 1)
      if (i .le. 3) then
         zdr_s = zdr_s + coeff_zdr_s(i) * dm_s ** (i - 1)
         kdp_s = kdp_s + coeff_kdp_s(i) * dm_s ** (i - 1)
         cc_s  = kdp_s + coeff_cc_s(i)  * dm_s ** (i - 1)
      end if
   end do
   zh_s  = zh_s**2. * z_s
   kdp_s = rho * qs * kdp_s / rho_s

   !----------------------------------------------------- graupel
   zh_g = 0.; zdr_g = 0.; kdp_g = 0.; cc_g = 0.
   do i = 1, 4
      ! calculate coefficients as a function of gamma_g
      do j = 1, 4
         coeff_zh_g(i)  = coeff_zh_s(i)  + coeff_zh_snow(i, j)  * gamma_s**j
         if (i .le. 3) then
            coeff_zdr_g(i) = coeff_zdr_g(i) + coeff_zdr_graupel(i, j) * gamma_g**j
            coeff_kdp_g(i) = coeff_kdp_g(i) + coeff_kdp_graupel(i, j) * gamma_g**j
            coeff_cc_g(i)  = coeff_cc_g(i)  + coeff_cc_graupel(i, j)  * gamma_g**j
         end if
      end do
      ! calculate each dual-pol variables
      zh_g  = zh_g  + coeff_zh_g(i)  * dm_g ** (i - 1)
      if (i .le. 3) then
         zdr_g = zdr_g + coeff_zdr_g(i) * dm_g ** (i - 1)
         kdp_g = kdp_g + coeff_kdp_g(i) * dm_g ** (i - 1)
         cc_g  = kdp_g + coeff_cc_g(i)  * dm_g ** (i - 1)
      end if
   end do
   zh_g  = zh_g**2. * z_g
   kdp_g = rho * qg * kdp_g / rho_g

   !----------------------------------------------------------sum across all hydrometeors
   ! Horizontal Reflectivity
   zh = zh_r + zh_s + zh_g

   ! Differential Reflectivity
   zdr = 0.
   if (zdr_r .ne. 0.) zdr = zdr + zh_r / zdr_r
   if (zdr_s .ne. 0.) zdr = zdr + zh_s / zdr_s
   if (zdr_g .ne. 0.) zdr = zdr + zh_g / zdr_g
   if (zdr .ne. 0.)   zdr = zh / zdr

   ! Specific Differential Phase
   kdp = kdp_r + kdp_s + kdp_g

   ! Correlation Coefficient 
   cc = 0.
   if (zdr_r .gt. 0.) cc = cc + zh_r / sqrt(zdr_r) * cc_r
   if (zdr_r .gt. 0.) cc_all = cc_all + zh_r / sqrt(zdr_r)
   if (zdr_s .gt. 0.) cc = cc + zh_s / sqrt(zdr_s) * cc_s
   if (zdr_s .gt. 0.) cc_all = cc_all + zh_s / sqrt(zdr_s)
   if (zdr_g .gt. 0.) cc = cc + zh_g / sqrt(zdr_g) * cc_g
   if (zdr_g .gt. 0.) cc_all = cc_all + zh_g / sqrt(zdr_g)
   if (cc_all .ne. 0.) cc = (cc / cc_all) ** 1.5 !! power term to consider decorrelation effects

end subroutine da_dualpol_radar_forward
