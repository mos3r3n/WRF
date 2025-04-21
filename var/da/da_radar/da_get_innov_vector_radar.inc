subroutine da_get_innov_vector_radar (it, grid, ob, iv)

   !-----------------------------------------------------------------------
   ! Purpose: Calculate innovation vectors for radar obs
   ! History:
   !    10/22/2008 - Updated for Analysis on Arakawa-C grid (Syed RH Rizvi, NCAR)
   !    ......
   !    08/08/2016 - Updated to include null-echo assimilation
   !                 (Yu-Shin Kim and Ki-Hong Min, School of Earth System
   !                  Sciences/Kyungpook National University, Daegu, S.Korea)
   !    08/2017 - bug fix for Vr operator (Siou-Ying Jiang, CWB, Taiwan)
   !-----------------------------------------------------------------------

   implicit none

#ifdef DM_PARALLEL
   include 'mpif.h'
#endif

   integer,          intent(in)    :: it       ! External iteration.
   type(domain),     intent(in)    :: grid     ! first guess state.
   type(y_type),     intent(inout) :: ob       ! Observation structure.
   type(iv_type),    intent(inout) :: iv       ! O-B structure.

   integer :: n        ! Loop counter.
   integer :: i, j, k  ! Index dimension.
   real    :: dx, dxm  ! Interpolation weights.
   real    :: dy, dym  ! Interpolation weights.

   real, allocatable :: model_p(:,:)
   real, allocatable :: model_u(:,:)
   real, allocatable :: model_v(:,:)
   real, allocatable :: model_w(:,:)

   real, allocatable :: model_rv(:,:)
   real, allocatable :: model_rf(:,:)
   real, allocatable :: model_ps(:)

   real, allocatable :: model_qv(:,:)
   real, allocatable :: model_qs(:,:)
   real, allocatable :: model_qs_ice(:,:)
   real, allocatable :: model_tc(:,:)

   real, allocatable :: model_rho(:,:)
   real, allocatable :: model_qrn(:,:)
   real, allocatable :: model_qsn(:,:)
   real, allocatable :: model_qgr(:,:)
   real, allocatable :: model_qnr(:,:)
   real, allocatable :: model_dbz(:,:)

   real    :: v_h(kms:kme)      ! Model value h at ob hor. location.

   real    :: xr,yr,zr
   integer :: irv, irvf
   integer :: irf, irff

   real    :: alog_10, czrn, czds, czws, czgr
   real    :: ob_radar_rf, bg_rze, bg_rf, rze
   real    :: cwr, cws ! weighting coefficient for mixing ratio

   ! variables for calculating cloud base
   real, allocatable :: model_lcl(:)           !model LCL at ob locations
   ! for opt=1
   real              :: h_sfc, p_sfc, t_sfc, q_sfc, ev_sfc, td_sfc
   ! for opt=2
   real              :: zlcl(ims:ime,jms:jme)

   logical           :: echo_non_precip, echo_rf_good

   ! variables for neighborhood no-rain scheme (radar_non_precip_opt=2)
   integer              :: proc, i_start, i_end, itmp1, itmp2
   integer              :: norain
   integer              :: nk, ncount_local, ncount_sum, s
   integer, allocatable :: ncount_all(:), rec(:)
   integer, allocatable :: counts(:), displs(:)
   integer, allocatable :: decrease(:), decrease_glob(:), decrease_local(:)
   real                 :: coefa, coefb, coefc
   real,    allocatable :: rf_local(:), i_local(:), j_local(:), z_local(:)
   real,    allocatable :: dbz_local(:), qs_local(:), qv_local(:)
   real,    allocatable :: rf_global(:), x_global(:), y_global(:), z_global(:)
   real,    allocatable :: dbz_global(:), qs_global(:), qv_global(:)

   !--------------------------------------------------------
   !  for background-dependent hydrmeteor retrieval scheme
   !--------------------------------------------------------
   character(len=filename_len) :: hydro_weight_file
   integer              :: hydro_weight_unit
   integer              :: tot_h_index, tot_z_index 
   integer              :: ii, jj, kk
   integer              :: h_index, z_index          
   logical              :: file_exist, qg_exist, use_2mom_rf
   real                 :: n0r, am_r, crg3, crg4, pi, rhor
   real                 :: p_r, p_ws, p_wg, p_dg, ds_para_a, ds_para_b
   real                 :: para_r, para_ws, para_ds, para_dg, para_wg
   real                 :: zrn, zds, zws, zgr
   real                 :: zern_ratio, zews_ratio, zeds_ratio, zegr_ratio  ! contributions of each hydrometeor to total reflectivity
   real, allocatable    :: num_sample(:,:)  ! number of samples from the background
   real, allocatable    :: avg_zern(:,:)    ! ze contributed by bin-averaged rainwater
   real, allocatable    :: avg_zeds(:,:)    ! ze contributed by bin-averaged dry snow
   real, allocatable    :: avg_zews(:,:)    ! ze contributed by bin-averaged wet snow
   real, allocatable    :: avg_zegr(:,:)    ! ze contributed by bin-averaged graupel
   real, allocatable    :: avg_qrn(:,:)     ! bin-averaged rainwater
   real, allocatable    :: avg_qnr(:,:)     ! bin-averaged rainwater number concentration
   real, allocatable    :: avg_qds(:,:)     ! bin-averaged dry snow
   real, allocatable    :: avg_qws(:,:)     ! bin-averaged wet snow
   real, allocatable    :: avg_qgr(:,:)     ! bin-averaged graupel
   real, allocatable    :: avg_rho(:,:)     ! bin-averaged air density
   real, allocatable    :: avg_tk(:,:)      ! bin-averaged air temperature (K)

   !------------------------
   !  for jung et al 2008
   !------------------------
   real    :: qvp,qra,qsn,qgr ! mixing ratio
   real    :: dqra,dqsn,dqgr,dtmk,dqvp
   real    :: dqnr,dqns,dqng
   real    :: zmm,zmm_ref
   real    :: qnr,qns,qng            ! number concentration of rain snow and graupel
   real    :: tmk,prs                ! temperature and pressure
   real    :: dbz                    ! reflectivity in dBZ
   real    :: rn0_r,rn0_s,rn0_g      ! intercept parameter of rain snow and graupel
   real    :: rhos,rhog              ! density of snow and graupel

   !------------------------

   alog_10 = alog(10.0)

   ! Ze=zv*(ro*v)**1.75
   ! Zdb=10*log10(Ze)
   zrn = 3.63*1.00e+9  ! rainwater
   zds = 9.80*1.00e+8  ! dry snow
   zws = 4.26*1.00e+11 ! wet snow
   zgr = 4.33*1.00e+10 ! grauple

   ! For two-moment retrieval
   pi = 3.14159265
   rhor = 1000.
   p_r  = 2.0
   p_wg = 2.5
   p_dg = 2.5
   p_ws = 2.67
   para_r  = 7.295125e13
   para_ds = 4057379571.35256
   para_ws = 1.47e5
   para_wg = 5.54914e12
   para_dg = 1.90409e12
   n0r  = 8.0e6
   crg3 = 24.0
   crg4 = 5040.0
   am_r = pi * 1000. / 6.0
   use_2mom_rf = (use_radar_rnr .and. use_cv_nr)

   !------------------------
   !  for jung et al 2008
   !------------------------
   qnr=0
   qns=0
   qng=0
   rn0_r=8e6
   rn0_s=3e6
   rn0_g=4e6
   rhos=100.0
   rhog=400.0
   !------------------------   
   if (trace_use) call da_trace_entry("da_get_innov_vector_radar")

   irv = 0; irvf = 0; irf = 0; irff = 0


   ! No point in going through and allocating all these variables if we're just going to quit anyway

   if ( use_radar_rf .and. use_radar_rhv ) then
      write(unit=message(1),fmt='(A)') "Both 'use_radar_rf' and 'use_radar_rhv' are set to true"
      write(unit=message(2),fmt='(A)') "You must only choose one of these options"
      call da_error(__FILE__,__LINE__,message(1:2))
   end if

   allocate (model_p(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
   allocate (model_u(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
   allocate (model_v(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
   allocate (model_w(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))

   if ( use_radar_rv ) allocate (model_rv(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
   if ( use_radar_rf ) allocate (model_rf(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
   allocate (model_ps(iv%info(radar)%n1:iv%info(radar)%n2))

   allocate (model_qv(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
   allocate (model_qs(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
   allocate (model_tc(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))

   allocate (model_rho(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
   allocate (model_qrn(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
   allocate (model_qsn(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
   allocate (model_qgr(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
   if (use_cv_nr) allocate (model_qnr(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))

   if ( use_radar_rqv ) then
      allocate (model_lcl(iv%info(radar)%n1:iv%info(radar)%n2))
      allocate (model_qs_ice(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
      if (radar_non_precip_opt > 0) then
         allocate (model_dbz(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
         model_dbz(:,:)=-30.
      end if
      model_qs_ice(:,:) = 0. !initialize
   end if

   model_p(:,:)    = 0.
   model_u(:,:)    = 0.
   model_v(:,:)    = 0.
   model_w(:,:)    = 0.

   if ( use_radar_rv ) model_rv(:,:)   = 0.
   if ( use_radar_rf ) model_rf(:,:)   = 0.
   model_ps(:)     = 0.

   model_qv(:,:)   = 0.
   model_qs(:,:)   = 0.
   model_tc(:,:)   = 0.

   model_rho(:,:)  = 0.
   model_qrn(:,:)  = 0.
   model_qsn(:,:)  = 0.
   model_qgr(:,:)  = 0.
   if ( use_cv_nr ) model_qnr(:,:) = 0.

   if ( it > 1 ) then
     do n=iv%info(radar)%n1,iv%info(radar)%n2
        do k=1,iv%info(radar)%levels(n)
           if (iv%radar(n)%rv(k)%qc == fails_error_max) iv%radar(n)%rv(k)%qc = 0
           if (iv%radar(n)%rf(k)%qc == fails_error_max) iv%radar(n)%rf(k)%qc = 0
        end do
     end do
   end if

   do n=iv%info(radar)%n1,iv%info(radar)%n2
      if (iv%info(radar)%levels(n) < 1) cycle

      ! [1.0] Get horizontal interpolation weights:

      i   = iv%info(radar)%i(1,n)
      j   = iv%info(radar)%j(1,n)
      dx  = iv%info(radar)%dx(1,n)
      dy  = iv%info(radar)%dy(1,n)
      dxm = iv%info(radar)%dxm(1,n)
      dym = iv%info(radar)%dym(1,n)

      do k=kts,kte
         v_h(k) = dym*(dxm*grid%xb%h(i,j,k)+dx*grid%xb%h(i+1,j,k)) + dy*(dxm*grid%xb%h(i,j+1,k)+dx*grid%xb%h(i+1,j+1,k))
      end do

      do k=1, iv%info(radar)%levels(n)
         call da_to_zk(iv%radar(n)%height(k), v_h, v_interp_h, iv%info(radar)%zk(k,n))

         if (iv%info(radar)%zk(k,n) < 1.0) then
            iv%radar(n)%height_qc(k) = below_model_surface
         else if (iv%info(radar)%zk(k,n) > mkz) then
            iv%radar(n)%height_qc(k) = above_model_lid
         end if
      end do
   end do

   call da_convert_zk (iv%info(radar))

   ! [2.0] Interpolate horizontally to ob points:

   call da_interp_lin_3d (grid%xb % p,   iv%info(radar), model_p)
#ifdef A2C
   call da_interp_lin_3d (grid%xb % u,   iv%info(radar), model_u,'u')
   call da_interp_lin_3d (grid%xb % v,   iv%info(radar), model_v,'v')
#else
   call da_interp_lin_3d (grid%xb % u,   iv%info(radar), model_u)
   call da_interp_lin_3d (grid%xb % v,   iv%info(radar), model_v)
#endif
   call da_interp_lin_3d (grid%xb % wh,  iv%info(radar), model_w)
   call da_interp_lin_3d (grid%xb % rho, iv%info(radar), model_rho)
   call da_interp_lin_3d (grid%xb % qrn, iv%info(radar), model_qrn)
   call da_interp_lin_3d (grid%xb % qsn, iv%info(radar), model_qsn)
   IF ( ASSOCIATED( grid%xb%qgr ) ) THEN
      call da_interp_lin_3d (grid%xb % qgr, iv%info(radar), model_qgr)
   END IF

   if ( use_cv_nr ) call da_interp_lin_3d (grid%xb % qnr, iv%info(radar), model_qnr)
   call da_interp_lin_3d (grid%xb % q,  iv%info(radar), model_qv)
   call da_interp_lin_3d (grid%xb % qs, iv%info(radar), model_qs)
   call da_interp_lin_3d (grid%xb % t,  iv%info(radar), model_tc)

   model_tc = model_tc - 273.15 ! degree K to degree C

   if ( use_radar_rqv ) then
      do n=iv%info(radar)%n1,iv%info(radar)%n2
         do k=1,iv%info(radar)%levels(n)
            model_qs_ice(k,n) = model_qs(k,n)*exp((2.837E6 - 2.501E6)/461.5*(1./273.15 - 1./(model_tc(k,n)+273.15)))
	    if (radar_non_precip_opt > 0) then
                call da_radar_rf(model_qrn(k,n),model_qsn(k,n),model_qgr(k,n),model_tc(k,n),model_rho(k,n),bg_rze)
                model_dbz(k,n) = 10.*log10(bg_rze)
            end if
         end do
      end do
   end if

   ! calculate background/model LCL to be used by use_radar_rqv
   if ( use_radar_rqv .and. cloudbase_calc_opt == 2 ) then
      do j = jts, jte
         do i = its, ite
            zlcl(i,j) = 125.0*(grid%xb%t(i,j,1)-grid%xb%td(i,j,1))
         end do
      end do
   end if ! lcl for use_radar_rqv

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! background-dependent hydrometer retrieval scheme !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   if (use_radar_rhv .and. radar_rhv_opt == 2) then
      !! allocate variables
      tot_h_index = 40  ! from 500 m to 20 km, at an interval of 500 m
      tot_z_index = 7   ! from -5 dBZ to 65 dBZ, at an interval of 10 dBZ 
      allocate (num_sample(tot_h_index,tot_z_index))
      allocate (avg_zern(tot_h_index,tot_z_index))
      allocate (avg_zeds(tot_h_index,tot_z_index))
      allocate (avg_zews(tot_h_index,tot_z_index))
      allocate (avg_zegr(tot_h_index,tot_z_index))
      allocate (avg_qrn(tot_h_index,tot_z_index))
      allocate (avg_qds(tot_h_index,tot_z_index))
      allocate (avg_qws(tot_h_index,tot_z_index))
      allocate (avg_qgr(tot_h_index,tot_z_index))
      allocate (avg_rho(tot_h_index,tot_z_index))
      if (use_cv_nr) then
         allocate (avg_qnr(tot_h_index,tot_z_index))
	 allocate (avg_tk(tot_h_index,tot_z_index))
      end if

      !! variable initialization
      num_sample = 0.
      avg_qrn    = 0.
      avg_qws    = 0.
      avg_qds    = 0.
      avg_qgr    = 0.
      avg_rho    = 0.
      avg_zern   = 0.
      avg_zews   = 0.
      avg_zeds   = 0.
      avg_zegr   = 0.
      if ( use_cv_nr ) then
         avg_qnr = 0.
         avg_tk = 0.
      end if

      !! read historical statistics from hydro_mean.dat if available
      hydro_weight_file='hydro_mean.dat'
      inquire(file=trim(hydro_weight_file), exist=file_exist)
      if (file_exist) then
         call da_get_unit(hydro_weight_unit)
         open(unit=hydro_weight_unit, file=trim(hydro_weight_file), form='FORMATTED')
         read(unit=hydro_weight_unit, fmt='(A)')
         do z_index=1,tot_z_index
            do h_index=1,tot_h_index
               read(hydro_weight_unit, fmt='(2(10x), 4(f19.9,2x))') &
                    avg_zern(h_index,z_index), avg_zews(h_index,z_index), avg_zeds(h_index,z_index), avg_zegr(h_index,z_index)
            end do
         end do
         close(hydro_weight_unit)
         call da_free_unit(hydro_weight_unit)
      end if

      !! calculate sum of background states in the current processor 
      do kk=kds, kde
         do jj=jps, jpe
            do ii=ips, ipe
               !! calculate background reflectivity
	       if ( use_cv_nr ) then
                  call da_radar_rf_2mom(grid%xb%qrn(ii,jj,kk),grid%xb%qsn(ii,jj,kk),grid%xb%qgr(ii,jj,kk), &
                                     grid%xb%qnr(ii,jj,kk),grid%xb%t(ii,jj,kk),grid%xb%rho(ii,jj,kk),bg_rze)
	       else
                  call da_radar_rf(grid%xb%qrn(ii,jj,kk), grid%xb%qsn(ii,jj,kk), grid%xb%qgr(ii,jj,kk), &
                                grid%xb%t(ii,jj,kk)-273.15, grid%xb%rho(ii,jj,kk), bg_rze)
	       end if
               bg_rf = 10.*log10(bg_rze)
               !! get the index of reflectvity
               z_index = nint(bg_rf/10.)+1
               z_index = max(z_index, 0) ! set to non-precip if below -5 dBZ
               z_index = min(z_index, tot_z_index) ! set to 65 dBZ if above
               !! get the height index
               h_index = nint(grid%xb%h(ii,jj,kk)/500.)
               h_index = max(h_index, 1) ! set to 500 m if below
               h_index = min(h_index,tot_h_index) ! set to 20 km if above

               !! Sum of the model states of different model levels and reflectivity thresholds
               if (z_index .ne. 0 ) then
                  avg_qrn(h_index,z_index)  = avg_qrn(h_index,z_index)  + grid%xb%qrn(ii,jj,kk)
		  if ( use_cv_nr ) then
		     avg_qnr(h_index,z_index)  = avg_qnr(h_index,z_index)  + grid%xb%qnr(ii,jj,kk)
                     avg_tk(h_index,z_index)  = avg_tk(h_index,z_index)  + grid%xb%t(ii,jj,kk)
		  end if
                  if ( grid%xb%t(ii,jj,kk) > 273.15 ) then
                     avg_qws(h_index,z_index) = avg_qws(h_index,z_index) + grid%xb%qsn(ii,jj,kk)
                  else
                     avg_qds(h_index,z_index) = avg_qds(h_index,z_index) + grid%xb%qsn(ii,jj,kk)
                  end if
                  avg_qgr(h_index,z_index) = avg_qgr(h_index,z_index) + grid%xb%qgr(ii,jj,kk)
                  avg_rho(h_index,z_index) = avg_rho(h_index,z_index) + grid%xb%rho(ii,jj,kk)
                  num_sample(h_index,z_index) = num_sample(h_index,z_index) + 1.
               end if
            end do ! west-east
         end do  ! south-north
      end do ! bottom-top
  
      !! sum of all processors and get the averaged background states
      do z_index=1,tot_z_index
         do h_index=1,tot_h_index
            num_sample(h_index,z_index) = wrf_dm_sum_real(num_sample(h_index,z_index))
            if (num_sample(h_index,z_index) .gt. 0) then
               avg_rho(h_index,z_index) = wrf_dm_sum_real(avg_rho(h_index,z_index)) / num_sample(h_index,z_index)
               avg_qrn(h_index,z_index) = wrf_dm_sum_real(avg_qrn(h_index,z_index)) / num_sample(h_index,z_index)
	       if ( use_cv_nr ) then
                  avg_qnr(h_index,z_index) = wrf_dm_sum_real(avg_qnr(h_index,z_index)) / num_sample(h_index,z_index)
		  avg_tk(h_index,z_index) = wrf_dm_sum_real(avg_tk(h_index,z_index)) / num_sample(h_index,z_index)
	       end if
               avg_qws(h_index,z_index) = wrf_dm_sum_real(avg_qws(h_index,z_index)) / num_sample(h_index,z_index)
               avg_qds(h_index,z_index) = wrf_dm_sum_real(avg_qds(h_index,z_index)) / num_sample(h_index,z_index)
               avg_qgr(h_index,z_index) = wrf_dm_sum_real(avg_qgr(h_index,z_index)) / num_sample(h_index,z_index)
            end if
         end do
      end do

      !! calculate the contributions of each hydrometeor to total reflectivity and save them to hydro_mean.dat.update
      hydro_weight_file='hydro_mean.dat.update'
      if (rootproc) call da_get_unit(hydro_weight_unit)
      if (rootproc) open(unit=hydro_weight_unit, file=trim(hydro_weight_file), form='FORMATTED')
      if (rootproc) write(unit=hydro_weight_unit, fmt='(2(a8,2x), 4(a19,2x))') &
                          "z_index:", "h_index:", "===Rainwater===", "===Wet snow===", "===Dry snow===", "===Graupel==="
      do z_index=1,tot_z_index
         do h_index=1,tot_h_index
            if (num_sample(h_index,z_index) .gt. 10.) then
	       if ( use_cv_nr ) then
                  if (avg_qrn(h_index,z_index) > 0.) then
                    call da_radar_rf_2mom(avg_qrn(h_index,z_index), 0., 0., avg_qnr(h_index,z_index), avg_tk(h_index,z_index), &
                                          avg_rho(h_index,z_index), avg_zern(h_index,z_index))
		  end if
		  if (avg_qws(h_index,z_index) > 0. .or. avg_qds(h_index,z_index) > 0.) then
                    call da_radar_rf_2mom(0., avg_qws(h_index,z_index) + avg_qds(h_index,z_index), 0., 0., avg_tk(h_index,z_index), &
                                          avg_rho(h_index,z_index), avg_zeds(h_index,z_index))
	            if (avg_tk(h_index,z_index) > 273.15) then
                       avg_zews(h_index,z_index) = avg_zeds(h_index,z_index)
		       avg_zeds(h_index,z_index) = 0.
		    else
                       avg_zeds(h_index,z_index) = avg_zeds(h_index,z_index)
		       avg_zews(h_index,z_index) = 0.
		    end if
                  end if
		  if (avg_qgr(h_index,z_index) > 0.) then
                    call da_radar_rf_2mom(0., 0., avg_qgr(h_index,z_index), 0., avg_tk(h_index,z_index), &
                                          avg_rho(h_index,z_index), avg_zegr(h_index,z_index))
                  end if
	       else
                  if (avg_qrn(h_index,z_index) > 0.)  & !! rain water
                     avg_zern(h_index,z_index) =  zrn*(avg_rho(h_index,z_index)*avg_qrn(h_index,z_index))**1.75
                  if (avg_qws(h_index,z_index) > 0.)  & !! wet snow
                     avg_zews(h_index,z_index) =  zws*(avg_rho(h_index,z_index)*avg_qws(h_index,z_index))**1.75
                  if (avg_qds(h_index,z_index) > 0.)  & !! dry snow  
                     avg_zeds(h_index,z_index) =  zds*(avg_rho(h_index,z_index)*avg_qds(h_index,z_index))**1.75
                  if (avg_qgr(h_index,z_index) > 0.)  & !! graupel 
                     avg_zegr(h_index,z_index) =  zgr*(avg_rho(h_index,z_index)*avg_qgr(h_index,z_index))**1.75
	       end if
            end if
            if (rootproc) &
               write(unit=hydro_weight_unit, fmt='(2(i8, 2x), 4(f19.9,2x))')  z_index, h_index, &
                     avg_zern(h_index,z_index), avg_zews(h_index,z_index), avg_zeds(h_index,z_index), avg_zegr(h_index,z_index)
         end do
      end do   !bottom-top
      if (rootproc) close(hydro_weight_unit)
      if (rootproc) call da_free_unit(hydro_weight_unit)
   end if   !! use_radar_rhv .and. radar_rhv_opt == 2

   do n=iv%info(radar)%n1,iv%info(radar)%n2

      if ( use_radar_rf .and. radar_rf_opt==1) then
         ! for Xiao's default scheme
         ! Test 5.0E-8 (kg/kg) as critical value. It can not be smaller.
         do k=1,iv%info(radar)%levels(n)
            model_qrn(k,n)=amax1(5.0E-8,model_qrn(k,n))
         end do
      end if

      i   = iv%info(radar)%i(1,n)
      j   = iv%info(radar)%j(1,n)
      dx  = iv%info(radar)%dx(1,n)
      dy  = iv%info(radar)%dy(1,n)
      dxm = iv%info(radar)%dxm(1,n)
      dym = iv%info(radar)%dym(1,n)

      model_ps(n) = dxm *(dym * grid%xb % psac(i,  j) + dy * grid%xb%psac(i+1,  j)) + &
                 dx  *(dym * grid%xb % psac(i,j+1) + dy * grid%xb%psac(i+1,j+1)) + &
                 grid%xb % ptop

      ! calculate model LCL at ob locations
      if ( use_radar_rqv ) then
         select case ( cloudbase_calc_opt )
         case ( 1 )  !KNU scheme
            ! get surface variable to calculate cloud base
            h_sfc = dxm *(dym * grid%xb % terr(i,  j) + dy * grid%xb% terr(i+1,  j)) + &
                    dx  *(dym * grid%xb % terr(i,j+1) + dy * grid%xb% terr(i+1,j+1))
            p_sfc = dxm *(dym * grid%xb % psfc(i,  j) + dy * grid%xb% psfc(i+1,  j)) + &
                    dx  *(dym * grid%xb % psfc(i,j+1) + dy * grid%xb% psfc(i+1,j+1))
            t_sfc = dxm *(dym * grid%xb % t2(i,  j) + dy * grid%xb% t2(i+1,  j)) + &
                    dx  *(dym * grid%xb % t2(i,j+1) + dy * grid%xb% t2(i+1,j+1))
            q_sfc = dxm *(dym * grid%xb % q2(i,  j) + dy * grid%xb% q2(i+1,  j)) + &
                    dx  *(dym * grid%xb % q2(i,j+1) + dy * grid%xb% q2(i+1,j+1))

            ! calculate cloud base height based on model background variables
            ev_sfc = q_sfc*p_sfc/0.622
            td_sfc = 1./(1./273.15 - 461.51/2.501e6*log(ev_sfc/611.2))
            model_lcl(n) = h_sfc + 125.*(t_sfc-td_sfc)
            model_lcl(n) = amax1(h_sfc+500.,model_lcl(n))
            model_lcl(n) = amin1(3000.     ,model_lcl(n))

         case ( 2 )  !NCAR scheme
            model_lcl(n) = dxm *(dym * zlcl(i,j)   + dy * zlcl(i+1, j))  + &
                           dx  *(dym * zlcl(i,j+1) + dy * zlcl(i+1,j+1))
            ! zlcl_mean is model grid mean zlcl calculated in da_transform_wrftoxb.inc
            model_lcl(n) = max(model_lcl(n), zlcl_mean)

            ! add radar elevation to the calculated model_lcl
            ! iv%radar(n)%height(k) (rather than zr) is later used for checking
            model_lcl(n) = model_lcl(n) + iv%radar(n)%stn_loc%elv

         case default
            model_lcl(n) = 1500.0

         end select
      end if ! lcl for use_radar_rqv

      iv%radar(n)%model_p(1:iv%info(radar)%levels(n))   = model_p(1:iv%info(radar)%levels(n),n)
      iv%radar(n)%model_rho(1:iv%info(radar)%levels(n)) = model_rho(1:iv%info(radar)%levels(n),n)
      iv%radar(n)%model_qrn(1:iv%info(radar)%levels(n)) = model_qrn(1:iv%info(radar)%levels(n),n)
      iv%radar(n)%model_qsn(1:iv%info(radar)%levels(n)) = model_qsn(1:iv%info(radar)%levels(n),n)
      iv%radar(n)%model_qgr(1:iv%info(radar)%levels(n)) = model_qgr(1:iv%info(radar)%levels(n),n)

      if (use_2mom_rf) then
         iv%radar(n)%model_qnr(1:iv%info(radar)%levels(n)) = model_qnr(1:iv%info(radar)%levels(n),n)
      end if

      iv%radar(n)%model_ps     = model_ps(n)

      ! [3.0] Calculate rv, rf at OBS location and initialise components of &
      ! innovation vector:

      if (fg_format == fg_format_wrf_arw_regional .or. &
          fg_format == fg_format_wrf_arw_global ) then
         call da_llxy_wrf(map_info, &
            iv%radar(n)%stn_loc%lat, iv%radar(n)%stn_loc%lon, &
            iv%radar(n)%stn_loc%x,   iv%radar(n)%stn_loc%y)
      else
         call da_llxy_default( iv%radar(n)%stn_loc%lat, iv%radar(n)%stn_loc%lon, &
            iv%radar(n)%stn_loc%x,   iv%radar(n)%stn_loc%y)
      end if

      xr = grid%xb%ds *(iv%info(radar)%x(1,n) - iv%radar(n)%stn_loc%x)
      yr = grid%xb%ds *(iv%info(radar)%y(1,n) - iv%radar(n)%stn_loc%y)

      level_loop: do k=1, iv%info(radar)%levels(n)
         iv % radar(n) % rv(k) % inv = 0.0
         iv % radar(n) % rf(k) % inv = 0.0

         ! initialize
         echo_non_precip = .false.
         echo_rf_good    = .false.

         if (iv % radar(n) % height_qc(k) /= below_model_surface .and.  &
             iv % radar(n) % height_qc(k) /= above_model_lid) then

            zr = iv%radar(n)%height(k) - iv%radar(n)%stn_loc%elv

            ! identify if non-precip obs (rf = radar_non_precip_rf)
            echo_non_precip = (ob%radar(n)%rf(k) .le. radar_non_precip_rf .and. abs(ob % radar(n) % rf(k) - missing_r) > 1.0)

            ! identify if valid rf obs to process
            ! this includes the echo_non_precip case
            echo_rf_good = (abs(ob % radar(n) % rf(k) - missing_r) > 1.0) &
                           .and. (iv % radar(n) % rf(k) % qc >= obs_qc_pointer)

            if (use_radar_rv) then
               !set qc to missing_data for rv of -999.99 (radar_non_precip_rf)
               if ( echo_non_precip ) then
                   iv % radar(n) % rv(k) % qc = missing_data
               end if
               if (abs(iv % radar(n) % rv(k) % qc - missing_data) > 1) then
                  if (abs(ob % radar(n) % rv(k) - missing_r) > 1.0 .AND. &
                       iv % radar(n) % rv(k) % qc >= obs_qc_pointer) then

                     !reference: Sun and Crook (1997)
                     call da_radial_velocity(model_rv(k,n), model_p(k,n),  &
                        model_u(k,n), model_v(k,n), model_w(k,n),          &
                        model_qrn(k,n), model_ps(n), xr, yr, zr,           &
                        model_rho(k,n))

                     iv % radar(n) % rv(k) % inv = ob % radar(n) % rv(k) - model_rv(k,n)*radar_rv_rscl
                  end if
               end if
            end if

            if (use_radar_rf .and. radar_rf_opt==2) then
              iv % radar(n) % zmm(k) % inv = 0
              iv % radar(n) % rf (k) % qc = -5 ! assume bad rf obs first
              echo_rf_good = ob % radar(n) % rf(k) >= rfmin
              if ( echo_rf_good ) then
                  tmk=model_tc(k,n)+273.15
                  prs=model_p(k,n)
                  zmm=0
                  zmm_ref=0
                  call da_radzicevar(model_qv(k,n),model_qrn(k,n),model_qsn(k,n),model_qgr(k,n),qnr,qns,qng,tmk,prs,dbz,        &
                                       0,0,0,rn0_r,rn0_s,rn0_g,                                                                   &
                                       rhos,rhog,dtmk,dqvp,dqra,dqsn,dqgr,dqnr,dqns,dqng,zmm,0,                                   &
                                       0,zmm_ref)
                  model_rf(k,n)=dbz*radar_rf_rscl
                  iv % radar(n) % zmm(k) % inv = zmm
                  iv % radar(n) % rf(k) % inv = ob % radar(n) % rf(k) - model_rf(k,n)
                  if (model_rf(k,n) >= rfmin) iv % radar(n) % rf (k) % qc  = 0
              end if
            end if

            if (use_radar_rf.and.radar_rf_opt==1) then
               if ( echo_rf_good ) then
                  model_rf(k,n) = leh1 + leh2 * alog10(model_rho(k,n) * model_qrn(k,n) * 1.0e+3)
                  iv % radar(n) % rf(k) % inv = ob % radar(n) % rf(k) - model_rf(k,n)
               end if
            end if

            ! Calculate background/model reflectivity
            if (use_radar_rhv .or. use_radar_rqv) then
               if ( echo_rf_good ) then
                  call da_radar_rf (model_qrn(k,n),model_qsn(k,n),model_qgr(k,n),model_tc(k,n),model_rho(k,n),bg_rze)
                  bg_rf = 10.0*log10(bg_rze)  ! Z to dBZ
                  iv % radar(n) % zmm(k) % inv = bg_rf
                  iv % radar(n) % rf(k) % inv = ob % radar(n) % rf(k) - bg_rf
               end if
            end if

            ! Calculate retrieved hydrometeorological variables
            ! Background-dependent retrieval scheme (Chen et al. 2020 AR; Chen et al. 2021 QJRMS) 
            if (use_radar_rhv) then
               if ( echo_rf_good ) then

                  ! compute retrieved hydrometeors rhv
                  if (it.eq.1) then

                     ob_radar_rf = ob % radar(n) % rf(k)

                     if ( radar_non_precip_opt > 0 ) then ! assimilate non_precip echo
                        if ( echo_non_precip ) then ! ob is non-precip
                           if ( bg_rf > -15.0 ) then
                              ! when background/model is precip
                              ! retrieve hydrometeors from -15dBZ
                              ob_radar_rf = -15.0
                           else
                              ! when background/model is also non-precip
                              ! do not need to assimilate this ob
                              iv % radar(n) % rf (k) % qc = -5
                              iv % radar(n) % rrn(k) % qc = -5
                              iv % radar(n) % rsn(k) % qc = -5
                              iv % radar(n) % rgr(k) % qc = -5
                              cycle level_loop
                           end if
                        end if !if echo_non_precip
                     end if

                     ! The original WRFDA hydrometeor retrieval scheme
                     if (model_tc(k,n) .ge. 5.0) then
                        czrn = 1.0
                        czws = 0.0
                        czds = 0.0
                        czgr = 0.0
                     else if (model_tc(k,n) .ge.  0.0) then
                        czrn = (model_tc(k,n)+5.0)/10.0
                        czws = (1.0-czrn)*zws/(zws+zgr)
                        czds = 0.0
                        czgr = (1.0-czrn)*zgr/(zws+zgr)
                     else if (model_tc(k,n) .ge. -5.0) then
                        czrn = (model_tc(k,n)+5.0)/10.0
                        czws = 0.0
                        czds = (1.0-czrn)*zds/(zds+zgr)
                        czgr = (1.0-czrn)*zgr/(zds+zgr)
                     else if (model_tc(k,n) .lt. -5.0) then
                        czrn = 0.0
                        czws = 0.0
                        czds = zds/(zds+zgr)
                        czgr = zgr/(zds+zgr)
                     end if

                     if (radar_rhv_opt == 2) then
                       ! backgound-dependent reflectivity retrival scheme (Chen et al. 2020, AR; Chen et al. 2021, QJRMS)
                       !! get the index of reflectvity
                       z_index = nint(ob_radar_rf/10.+1)
                       z_index = max(z_index, 0)
                       z_index = min(z_index, tot_z_index)
                       !! get the height index
                       h_index = nint(iv%radar(n)%height(k)/500.)
                       h_index = max(h_index, 1)
                       h_index = min(h_index, tot_h_index)

                       if (z_index > 0) then
                          zern_ratio = avg_zern(h_index, z_index)
                          zews_ratio = avg_zews(h_index, z_index)
                          zeds_ratio = avg_zeds(h_index, z_index)
                          zegr_ratio = avg_zegr(h_index, z_index)
                          ! detect whether rain/snow/graupel exists in certain temperatures.
                          qg_exist = .true.
                          ! when T < 273.15K
                          if (model_tc(k,n) .lt. -5.0) zern_ratio = 0.
                          if (model_tc(k,n) .lt.  0.0) zews_ratio = 0.
                          ! when T >= 273.15K
                          if (model_tc(k,n) .ge. 0.0) then
                             zeds_ratio = 0.
                             qg_exist = .false.
                             do nk = k, iv%info(radar)%levels(n)
                                if (model_tc(nk,n) .lt. -5.0 .and. ob % radar(n) % rf(nk) .ge. 40.) qg_exist = .true.
                             end do
                          end if
                          if (model_tc(k,n) .ge. 5.0) zews_ratio = 0.
                          if (.not. qg_exist .or. model_tc(k,n) .ge. 10.0) zegr_ratio = 0.
                        
                          ! determine the contributions of each hydrometeor to reflectivity
                          if ((zern_ratio+zews_ratio+zeds_ratio+zegr_ratio) .gt. 0.) then
                             czrn = zern_ratio/(zern_ratio+zews_ratio+zeds_ratio+zegr_ratio)
                             czws = zews_ratio/(zern_ratio+zews_ratio+zeds_ratio+zegr_ratio)
                             czds = zeds_ratio/(zern_ratio+zews_ratio+zeds_ratio+zegr_ratio)
                             czgr = zegr_ratio/(zern_ratio+zews_ratio+zeds_ratio+zegr_ratio)
                          end if
                        else
                          ob_radar_rf = -15.0 !! Assign reflectivity below -5.0 dBZ to -15.0 dbZ for suppression 
                                              !! No need to tune the weights because of very small impacts
                        end if
                     end if
   
                     ! convert dBZ to Z
                     ob_radar_rf = min(ob_radar_rf, 65.0) ! if dBZ>65.0, set to 65.0
                     rze = 10.0**(ob_radar_rf*0.1) ! dBZ to Z

                     ! Rainwater water
                     if (czrn .gt. 0.) then
		        if ( use_cv_nr ) then
                           ! rain water mixing ratio
                           iv % radar(n) % rrno(k) = n0r * am_r * crg3 / model_rho(k,n) * (czrn*rze*1.e-18 / (n0r*crg4))**(4./7.)
                           iv % radar(n) % rrn(k) % qc = 0
			   if (use_radar_rnr) then
                              ! number concentration of rain water
                              ! iv % radar(n) % rnro(k) = (n0r/model_rho(k,n))**0.75* (iv % radar(n) % rrno(k) / (am_r * crg3))**0.25
                              iv % radar(n) % rnro(k) = para_r * (model_rho(k,n) *iv % radar(n) % rrno(k))**p_r / (czrn*rze*model_rho(k,n))
                              iv % radar(n) % rnr(k) % qc = 0
                              iv % radar(n) % rnro(k) = max(iv%radar(n)%rnro(k), 0.)
			    end if
			else
                           iv % radar(n) % rrno(k) = exp ( log(czrn*rze/zrn)/1.75 )/model_rho(k,n)
                           iv % radar(n) % rrn(k) % qc = 0
			end if
                     end if

                     ! Snow
                     if ((czws+czds) .gt. 0.) then
                        if (model_tc(k,n) .gt. 0.) then
			   if ( use_cv_nr ) then
			      iv % radar(n) % rsno(k) = exp ( log((czds+czws)*rze/para_ws)/p_ws )/1000.
                              iv % radar(n) % rsn(k) % qc = 0
			   else
                              iv % radar(n) % rsno(k) = exp ( log((czws+czds)*rze/zws)/1.75 )/model_rho(k,n)
                              iv % radar(n) % rsn(k) % qc = 0
			   end if
                        else
			   if ( use_cv_nr ) then
			      call cal_coef_wetsnow(model_tc(k,n)+273.15,ds_para_a, ds_para_b)
                              iv % radar(n) % rsno(k) = exp ( log((czds+czws)*rze/para_ds/ds_para_a)/ds_para_b )/model_rho(k,n)*0.069
                              iv % radar(n) % rsn(k) % qc = 0
			   else
                              iv % radar(n) % rsno(k) = exp ( log((czds+czws)*rze/zds)/1.75 )/model_rho(k,n)
                              iv % radar(n) % rsn(k) % qc = 0
			   end if
                        end if
                     end if

                     ! Graupel
                     if (czgr .gt. 0.) then 
		        if ( use_cv_nr ) then
			   if (model_tc(k,n) .gt. 0.) then
                              iv % radar(n) % rgro(k) = exp ( log(czgr*rze/para_wg/model_rho(k,n)**1.75)/p_wg)
                              iv % radar(n) % rgr(k) % qc = 0
			   else
                              iv % radar(n) % rgro(k) = exp ( log(czgr*rze/para_dg/model_rho(k,n)**1.75)/p_dg)
                              iv % radar(n) % rgr(k) % qc = 0
			   end if
			else
                           iv % radar(n) % rgro(k) = exp ( log(czgr*rze/zgr)/1.75 )/model_rho(k,n)
                           iv % radar(n) % rgr(k) % qc = 0
			end if
                     end if

                     if ( radar_rhv_err_opt == 1 ) then
                        ! rainwater error
                        iv % radar(n) % rrn(k) % error = iv % radar(n) % rf(k) % error * iv % radar(n) % rrno(k) * alog_10/leh2
                        iv % radar(n) % rrn(k) % error = amax1(0.0005,iv % radar(n) % rrn(k) % error)
                        iv % radar(n) % rrn(k) % error = amin1( 0.001,iv % radar(n) % rrn(k) % error)
                        ! snow error
                        iv % radar(n) % rsn(k) % error = iv % radar(n) % rf(k) % error * iv % radar(n) % rsno(k) * alog_10/leh2
                        iv % radar(n) % rsn(k) % error = amax1(0.0005,iv % radar(n) % rsn(k) % error)
                        iv % radar(n) % rsn(k) % error = amin1( 0.001,iv % radar(n) % rsn(k) % error)
                        ! graupel error
                        iv % radar(n) % rgr(k) % error = iv % radar(n) % rf(k) % error * iv % radar(n) % rgro(k) * alog_10/leh2
                        iv % radar(n) % rgr(k) % error = amax1(0.0005,iv % radar(n) % rgr(k) % error)
                        iv % radar(n) % rgr(k) % error = amin1( 0.001,iv % radar(n) % rgr(k) % error)
			! rainwater number concentration
			if ( use_2mom_rf ) then
                           iv % radar(n) % rnr(k) % error = amax1(abs(iv % radar(n) % rrno(k) - model_qnr(k,n))*0.2, 100.)
			end if
                     else if ( radar_rhv_err_opt == 2 ) then
                        ! use settings in the namelist
                        ! rainwater error
                        iv % radar(n) % rrn(k) % error = radar_rhv_rrn_err * 0.001 ! g/kg to kg/kg
                        ! snow error
                        iv % radar(n) % rsn(k) % error = radar_rhv_rsn_err * 0.001 ! g/kg to kg/kg
                        ! graupel error
                        iv % radar(n) % rgr(k) % error = radar_rhv_rgr_err * 0.001 ! g/kg to kg/kg
                        ! rainwater number concentration
                        if ( use_2mom_rf ) then
                           iv % radar(n) % rnr(k) % error = radar_rhv_rnr_err
			end if
                     else
                        write(unit=message(1),fmt='(A)') "radar_rhv_err_opt should be 1 or 2"
                        call da_error(__FILE__,__LINE__,message(1:1))
                     end if

                  end if  ! it=1

                  if (iv % radar(n) % rrn(k) % qc >= obs_qc_pointer) then
                     iv % radar(n) % rrn(k) % inv = iv % radar(n) % rrno(k) - model_qrn(k,n)
                  end if
                  if (iv % radar(n) % rsn(k) % qc >= obs_qc_pointer) then
                     iv % radar(n) % rsn(k) % inv = iv % radar(n) % rsno(k) - model_qsn(k,n)
                  end if
                  if (iv % radar(n) % rgr(k) % qc >= obs_qc_pointer) then
                     iv % radar(n) % rgr(k) % inv = iv % radar(n) % rgro(k) - model_qgr(k,n)
                  end if
		  if ( use_2mom_rf .and. iv % radar(n) % rnr(k) % qc >= obs_qc_pointer ) then 
                     iv % radar(n) % rnr(k) % inv = iv % radar(n) % rnro(k) - model_qnr(k,n)
		     iv % radar(n) % rnr(k) % error = iv % radar(n) % rnr(k) % error
		  end if
               end if ! echo_rf_good check
            end if ! rhv

            ! retrieved water vapor
            if (use_radar_rqv) then
               !iv%%rqv and iv%%rqvo were assigned to missing values in read_obs_radar_ascii.inc
               !iter=1, rqv is missing; for second loop, dont change rqv value
               if (it .eq. 1) then
                  iv % radar(n) % rqvo(k) = 1.0*model_qs(k,n)
                  iv % radar(n) % rqv(k) % error = amax1(0.0005,0.1*iv % radar(n) % rqvo(k))
               end if

               if ( echo_rf_good ) then

                  ! initialize as not-assimilating rqv
                  iv % radar(n) % rqv(k) % qc = -5

                  if ( echo_non_precip ) then ! ob is non-precip
                     if ( radar_non_precip_opt == 1 ) then ! assimilate non_precip echo

                        if ( bg_rf >= 20.0 .and. iv%radar(n)%height(k) > model_lcl(n) ) then
                           iv % radar(n) % rqv(k) % qc = 0

                           if ( model_tc(k,n) > 5.0 ) then
                              iv%radar(n)%rqvo(k) =  0.01*radar_non_precip_rh_w*model_qs(k,n)
                           else if ( model_tc(k,n) < -5.0 ) then
                              iv%radar(n)%rqvo(k) =  0.01*radar_non_precip_rh_i*model_qs_ice(k,n)
                           else
                              cwr = (model_tc(k,n)+5.0)/10.0
                              cws = 1.0 - cwr
                              iv%radar(n)%rqvo(k) =  cwr*0.01*radar_non_precip_rh_w*model_qs(k,n) + cws*0.01*radar_non_precip_rh_i*model_qs_ice(k,n)
                           end if

                           iv % radar(n) % rqv(k) % inv = iv % radar(n) % rqvo(k) - model_qv(k,n)
                           ! iv % radar(n) % rqv(k) % inv must be <= 0.0
                           iv % radar(n) % rqv(k) % inv = amin1(0.0,iv % radar(n) % rqv(k) % inv )
                           iv % radar(n) % rqv(k) % error = amax1(0.0005,0.10*model_qs(k,n))
                        end if
                     end if

                  else  ! ob is precip
                     if ( ob % radar(n) % rf(k) >= radar_rqv_thresh1 .and. iv%radar(n)%height(k) >= model_lcl(n) )then

                        if ( radar_rqv_h_lbound >= 0.0 .and. radar_rqv_h_ubound >= 0.0 ) then
                           ! height ranges for applying retrieved rqv are set in the namelist
                           if ( iv%radar(n)%height(k) < radar_rqv_h_lbound .or. &
                                iv%radar(n)%height(k) > radar_rqv_h_ubound ) then
                              ! do not assimilate rqv outside the specified height ranges
                              iv % radar(n) % rqv(k) % qc = -5
                           end if
                        end if

                        ! reduce rqvo for certain rf ranges
                        if ( radar_rqv_thresh1 >= 0.0 .and. radar_rqv_thresh2 >= 0.0 .and. radar_rqv_thresh3 >= 0.0 .and. &
                             radar_rqv_thresh2 >= radar_rqv_thresh1 .and. radar_rqv_thresh3 >= radar_rqv_thresh2 ) then
                           if ( ob % radar(n) % rf(k) >= radar_rqv_thresh1 .and. &
                                ob % radar(n) % rf(k) < radar_rqv_thresh2) then
                              if ( radar_rqv_rh1 >= 0.0 ) then ! RH in percentage
                                 iv % radar(n) % rqvo(k)= 0.01*radar_rqv_rh1*model_qs(k,n)
				 iv % radar(n) % rqv(k) % qc = 0
                              end if
			   else if ( ob % radar(n) % rf(k) >= radar_rqv_thresh2 .and. &
                                ob % radar(n) % rf(k) < radar_rqv_thresh3) then
			      if ( radar_rqv_rh2 >= 0.0 .and. radar_rqv_rh2 >= radar_rqv_rh1) then ! RH in percentage
                                 iv % radar(n) % rqvo(k)= 0.01*radar_rqv_rh2*model_qs(k,n)
				 iv % radar(n) % rqv(k) % qc = 0
                              end if
			   else if ( ob % radar(n) % rf(k) >= radar_rqv_thresh3 ) then
			      if ( radar_rqv_rh3 >= 0.0 .and. radar_rqv_rh3 >= radar_rqv_rh2) then ! RH in percentage
                                 iv % radar(n) % rqvo(k)= 0.01*radar_rqv_rh3*model_qs(k,n)
				 iv % radar(n) % rqv(k) % qc = 0
                              end if
                           end if
                        end if
                        iv % radar(n) % rqv(k) % inv = iv % radar(n) % rqvo(k) - model_qv(k,n)
                        ! iv % radar(n) % rqv(k) % inv must be >= 0.0
                        iv % radar(n) % rqv(k) % inv = amax1(0.0,iv % radar(n) % rqv(k) % inv )
                        iv % radar(n) % rqv(k) % error = amax1(0.001,0.20*iv % radar(n) % rqvo(k))

                     end if

                  end if ! echo_non_precip check

               end if  ! echo_rf_good check
            end if  ! use_radar_rqv

         end if  ! not surface or model lid
      end do level_loop
   end do

   if ( use_radar_rqv .and. radar_non_precip_opt == 2 ) then ! neighborhood no-rain scheme
   
      ncount_local  = 0

      if ( iv%info(radar)%nlocal > 0 ) then
         do n = iv%info(radar)%n1,iv%info(radar)%n2
            do k = 1,iv%info(radar)%levels(n)
               ncount_local = ncount_local + 1
            end do
         end do
      end if

      allocate (ncount_all(0:num_procs-1))
#ifdef DM_PARALLEL
      call mpi_allgather( ncount_local, 1, mpi_integer, &
                          ncount_all,   1, mpi_integer, comm, ierr )
#else
      ncount_all(:) = ncount_local
#endif
      ncount_sum = sum(ncount_all)

      allocate ( rf_local(ncount_local))
      allocate (  i_local(ncount_local))
      allocate (  j_local(ncount_local))
      allocate (  z_local(ncount_local))
      allocate (dbz_local(ncount_local))
      allocate ( qs_local(ncount_local))
      allocate ( qv_local(ncount_local))

      if ( iv%info(radar)%nlocal > 0 ) then
         nk = 0
         do n = iv%info(radar)%n1,iv%info(radar)%n2
            do k = 1,iv%info(radar)%levels(n)
               nk = nk + 1
               rf_local(nk)  = ob%radar(n)%rf(k)
               i_local(nk)   = iv%info(radar)%i(k,n)
               j_local(nk)   = iv%info(radar)%j(k,n)
               z_local(nk)   = iv%radar(n)%height(k) - iv%radar(n)%stn_loc%elv
               dbz_local(nk) = model_dbz(k,n)
               qs_local(nk)  = model_qs(k,n)
               qv_local(nk)  = model_qv(k,n)
            end do
         end do
      end if

      allocate ( rf_global(ncount_sum))
      allocate (  x_global(ncount_sum))
      allocate (  y_global(ncount_sum))
      allocate (  z_global(ncount_sum))
      allocate (dbz_global(ncount_sum))
      allocate ( qs_global(ncount_sum))
      allocate ( qv_global(ncount_sum))

#ifdef DM_PARALLEL
      allocate (counts(0:num_procs-1))
      allocate (displs(0:num_procs-1))
      counts(:) = ncount_all(:)
      displs(0) = 0
      do proc = 1, num_procs-1
         displs(proc) = displs(proc-1) + counts(proc-1)
      end do
      call mpi_allgatherv( rf_local, ncount_local, true_mpi_real,     &
                           rf_global, counts, displs, true_mpi_real, &
                           comm, ierr )
      call mpi_allgatherv( i_local, ncount_local, true_mpi_real,      &
                           x_global, counts, displs, true_mpi_real,   &
                           comm, ierr )
      call mpi_allgatherv( j_local, ncount_local, true_mpi_real,      &
                           y_global, counts, displs, true_mpi_real,   &
                           comm, ierr )
      call mpi_allgatherv( z_local, ncount_local, true_mpi_real,      &
                           z_global, counts, displs, true_mpi_real,   &
                           comm, ierr )
      call mpi_allgatherv( dbz_local, ncount_local, true_mpi_real,    &
                           dbz_global, counts, displs, true_mpi_real, &
                           comm, ierr )
      call mpi_allgatherv( qs_local, ncount_local, true_mpi_real,     &
                           qs_global, counts, displs, true_mpi_real,  &
                           comm, ierr )
      call mpi_allgatherv( qv_local, ncount_local, true_mpi_real,     &
                           qv_global, counts, displs, true_mpi_real,  &
                           comm, ierr )
#else
       rf_global(:) =  rf_local(:)
        x_global(:) =   i_local(:)
        y_global(:) =   j_local(:)
        z_global(:) =   z_local(:)
      dbz_global(:) = dbz_local(:)
       qs_global(:) =  qs_local(:)
       qv_global(:) =  qv_local(:)
#endif

      deallocate ( rf_local)
      deallocate (  i_local)
      deallocate (  j_local)
      deallocate (  z_local)
      deallocate (dbz_local)
      deallocate ( qs_local)
      deallocate ( qv_local)
      deallocate(ncount_all)
      ! determine the loop indices (i_start, i_end) for each proc given ncount_sum
      itmp1 = ncount_sum/num_procs
      itmp2 = mod(ncount_sum, num_procs)
      i_start = myproc * itmp1 + 1 + min(myproc, itmp2)
      i_end   = i_start + itmp1 - 1
      if (itmp2 > myproc) i_end = i_end + 1

      coefa = (grid%dx/norain_hori_range)**2
      coefb = (grid%dx/norain_hori_range)**2
      coefc = (1.0/norain_vert_range)**2

      allocate ( rec(ncount_sum))
      allocate ( decrease(ncount_sum))
      decrease(:) = 0
      do s = i_start, i_end
         ! if non-precip obs (rf = radar_non_precip_rf)
         echo_non_precip = (rf_global(s) .le. radar_non_precip_rf .and. rf_global(s) .gt. missing_r)
         if ( echo_non_precip .and. dbz_global(s) > norain_fg_dbz_thre .and. &
	      (qv_global(s) > norain_fg_rh_thre * qs_global(s)) )then
            i=0
            do n = 1, ncount_sum
               if ( ((x_global(s)-x_global(n))**2*coefa + &
                     (y_global(s)-y_global(n))**2*coefb + &
                     (z_global(s)-z_global(n))**2*coefc) <= 1 ) then
                  i = i+1
                  rec(i) = n
               end if
            end do
            if ( i > 0 ) then
               norain = 0
               do n = 1, i
                  if ( rf_global(rec(n)) .le. radar_non_precip_rf .and. rf_global(rec(n)) .gt. missing_r) then
                     norain = norain + 1
                  end if
               end do
               if ( float(norain)/float(i) >= norain_box_percent ) then
                  decrease(s) = 1
               end if
            end if
         end if
      end do

      deallocate (       rec)
      deallocate ( rf_global)
      deallocate (  x_global)
      deallocate (  y_global)
      deallocate (  z_global)
      deallocate (dbz_global)
      deallocate ( qs_global)
      deallocate ( qv_global)

      allocate (decrease_local(ncount_local))
#ifdef DM_PARALLEL
      allocate ( decrease_glob(ncount_sum))
      call mpi_reduce(decrease, decrease_glob, ncount_sum, &
                      mpi_integer, mpi_sum, root, comm, ierr)
      call mpi_scatterv(decrease_glob, counts, displs, mpi_integer, &
                        decrease_local, ncount_local, mpi_integer,  &
                        root, comm, ierr)
      deallocate (decrease_glob)
#else
      decrease_local = decrease
#endif

      if ( iv%info(radar)%nlocal > 0 ) then
         nk = 0
         do n = iv%info(radar)%n1, iv%info(radar)%n2
            do k = 1, iv%info(radar)%levels(n)
               nk = nk + 1 !index for decrease_local array
               echo_non_precip = (ob%radar(n)%rf(k) .le. radar_non_precip_rf .and. ob%radar(n)%rf(k) .gt. missing_r)
               if ( echo_non_precip .and. decrease_local(nk) == 1 .and. iv%radar(n)%height(k) > model_lcl(n) .and. &
                    model_dbz(k,n) > norain_fg_dbz_thre .and. model_qv(k,n) > (norain_fg_rh_thre * model_qs(k,n)) ) then
                  iv % radar(n) % rqv(k) % qc = 0
                  iv % radar(n) % rf(k) % qc = 0
                  iv % radar(n) % rqvo(k) = norain_rh_reduce * model_qv(k,n)
                  iv % radar(n) % rqv(k) % inv = iv % radar(n) % rqvo(k) - model_qv(k,n)
                  iv % radar(n) % rqv(k) % error = amax1(0.001,0.20*iv % radar(n) % rqvo(k))
               end if
            end do !k loop
         end do !n1-n2 loop
      end if !nlocal>0

      deallocate (decrease)
      deallocate (decrease_local)
#ifdef DM_PARALLEL
      deallocate(counts)
      deallocate(displs)
#endif

   end if ! use_radar_rqv and radar_non_precip_opt=2

   !------------------------------------------------------------------------
   ! [4.0] Perform optional maximum error check:  
   !------------------------------------------------------------------------

   if (check_max_iv)  then
      call da_check_max_iv_radar(iv, it, irv, irf, irvf, irff)
   end if

   if (rootproc .and. check_max_iv_print) then
      write(unit = check_max_iv_unit, fmt ='(/,A,i5,A)')   &
         'For outer iteration ', it, ', Total Rejections for radar follows:'

      if (use_radar_rv) then
          write( unit = check_max_iv_unit, fmt = '(/,2(A,I6))') &
            'Number of failed rv observations:     ',irvf, ' on ',irv
      end if

      if (use_radar_rf) then
         write( unit = check_max_iv_unit, fmt = '(/,2(A,I6))') &
            'Number of failed rf observations:     ',irff, ' on ',irf
      end if
   end if

   deallocate (model_p)
   deallocate (model_u)
   deallocate (model_v)
   deallocate (model_w)

   if ( allocated(model_rv) ) deallocate (model_rv)
   if ( allocated(model_rf) ) deallocate (model_rf)
   deallocate (model_ps)

   deallocate (model_qv)
   deallocate (model_qs)
   deallocate (model_tc)

   deallocate (model_qrn)
   deallocate (model_rho)
   deallocate (model_qsn)
   deallocate (model_qgr)

   if ( allocated(num_sample) )  deallocate (num_sample)
   if ( allocated(avg_zern) )    deallocate (avg_zern)
   if ( allocated(avg_zeds) )    deallocate (avg_zeds)
   if ( allocated(avg_zews) )    deallocate (avg_zews)
   if ( allocated(avg_zegr) )    deallocate (avg_zegr)
   if ( allocated(avg_qrn) )     deallocate (avg_qrn)
   if ( allocated(avg_qds) )     deallocate (avg_qds)
   if ( allocated(avg_qws) )     deallocate (avg_qws)
   if ( allocated(avg_qgr) )     deallocate (avg_qgr)
   if ( allocated(avg_rho) )     deallocate (avg_rho)
   if ( allocated(avg_qnr) )     deallocate (avg_qnr)
   if ( allocated(avg_tk) )      deallocate (avg_tk)

   if ( allocated(model_lcl) )   deallocate (model_lcl)
   if ( allocated(model_qs_ice)) deallocate (model_qs_ice)
   if ( allocated(model_dbz) )   deallocate (model_dbz)     

   if (trace_use) call da_trace_exit("da_get_innov_vector_radar")

end subroutine da_get_innov_vector_radar
