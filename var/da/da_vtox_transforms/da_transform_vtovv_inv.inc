subroutine da_transform_vtovv_inv(grid, cv_size, be, cv, vv)

   !-----------------------------------------------------------------------
   ! Purpose: perform inverse transform of horizontal recursive filter
   !
   ! Author:  Zhiquan (Jake) Liu, NCAR/MMM, 2015-09
   !-----------------------------------------------------------------------

   implicit none

   type(domain),  intent(inout) :: grid
   integer,       intent(in)    :: cv_size ! Size of cv array.
   type(be_type), intent(in)    :: be   ! Background error structure.
   real,          intent(inout) :: cv(cv_size)   ! control variables.
   type(vp_type), intent(inout) :: vv   ! Grid point/EOF control var.

   integer :: s(4)   ! Index bounds into arrays.
   integer :: n      ! Loop counter.
   integer :: mz     ! Vertical truncation.
   integer :: ne     ! Ensemble size.

   logical :: scaling
 
   if (trace_use) call da_trace_entry("da_transform_vtovv_inv")

   if( .not. use_rf .or. do_normalize ) s(1:2)=1


   !-------------------------------------------------------------------------
   ! [2.0] Perform inverse of VToVV Transform:
   !-------------------------------------------------------------------------

   ! [2.1] Transform 1st control variable:
   mz = be % v1 % mz
   if( use_rf .and. mz > 0 .and. len_scaling1(1) /= 0.0) then
      call da_transform_through_rf_inv(grid, mz, be % v1 % rf_alpha, be % v1 % val, vv % v1)
   endif

   ! [2.2] Transform 2nd control variable:

   mz = be % v2 % mz
   if( use_rf .and. mz > 0 .and. len_scaling2(1) /= 0.0) then
      call da_transform_through_rf_inv(grid, mz, be % v2 % rf_alpha, be % v2 % val, vv % v2)
   endif

   ! [2.3] Transform 3rd control variable

   mz = be % v3 % mz
   if( use_rf .and. mz > 0 .and. len_scaling3(1) /= 0.0) then
      call da_transform_through_rf_inv(grid, mz, be % v3 % rf_alpha, be % v3 % val, vv % v3)
   endif
   !s(1)=s(3)+1
   
   ! [2.4] Transform 4th control variable
      
   mz = be % v4 % mz
   if( use_rf .and. mz > 0 .and. len_scaling4(1) /= 0.0) then
      call da_transform_through_rf_inv(grid, mz, be % v4 % rf_alpha, be % v4 % val, vv % v4)
   endif
   !s(1)=s(3)+1

   ! [2.5] Transform 5th control variable

   mz = be % v5 % mz
   if( use_rf .and. mz > 0 .and. len_scaling5(1) /= 0.0) then
      call da_transform_through_rf_inv(grid, mz, be % v5 % rf_alpha, be % v5 % val, vv % v5)
   endif

   if ( use_rf .and. cloud_cv_options <= 1 ) then
      vv % v6  = 0.0
      vv % v7  = 0.0
      vv % v8  = 0.0
      vv % v9  = 0.0
      vv % v10 = 0.0
      vv % v11 = 0.0
      vv % v12 = 0.0
   end if


   ! [2.6] Transform 6th-10th cloud control variables

   if ( use_rf .and. cloud_cv_options >= 2 ) then
      select case ( cloud_cv_options )
         case ( 2 )
!hcl-check array index of len_scaling
            mz = be % v6 % mz
            if ( mz > 0 .and. len_scaling6(1) > 0.0 ) then
               call da_transform_through_rf_inv(grid, mz, be % v6 % rf_alpha, be % v6 % val, vv % v6)
            end if
            mz = be % v7 % mz
            if ( mz > 0 .and. len_scaling7(1) > 0.0 ) then
               call da_transform_through_rf_inv(grid, mz, be % v7 % rf_alpha, be % v7 % val, vv % v7)
            end if
            mz = be % v8 % mz
            if ( mz > 0 .and. len_scaling8(1) > 0.0 ) then
               call da_transform_through_rf_inv(grid, mz, be % v8 % rf_alpha, be % v8 % val, vv % v8)
            end if
            mz = be % v9 % mz
            if ( mz > 0 .and. len_scaling9(1) > 0.0 ) then
               call da_transform_through_rf_inv(grid, mz, be % v9 % rf_alpha, be % v9 % val, vv % v9)
            end if
            mz = be % v10 % mz
            if ( mz > 0 .and. len_scaling10(1) > 0.0 ) then
               call da_transform_through_rf_inv(grid, mz, be % v10 % rf_alpha, be % v10 % val, vv % v10)
            end if
            if ( use_cv_nr ) then
               mz = be % v11 % mz
               if ( mz > 0 .and. len_scaling11(1) > 0.0 ) then
                  call da_transform_through_rf_inv(grid, mz, be % v11 % rf_alpha, be % v11 % val, vv % v11)
               end if
	    end if
   	 case ( 3 )
            scaling = .true.
            mz = be % v6 % mz
            if ( mz > 0 .and. len_scaling6(1) > 0.0 ) then
               call da_transform_through_rf_inv(grid, mz, be % v6 % rf_alpha, be % v6 % val, vv % v6, scaling)
            end if
            mz = be % v7 % mz
            if ( mz > 0 .and. len_scaling7(1) > 0.0 ) then
               call da_transform_through_rf_inv(grid, mz, be % v7 % rf_alpha, be % v7 % val, vv % v7, scaling)
            end if
            mz = be % v8 % mz
            if ( mz > 0 .and. len_scaling8(1) > 0.0 ) then
               call da_transform_through_rf_inv(grid, mz, be % v8 % rf_alpha, be % v8 % val, vv % v8, scaling)
            end if
            mz = be % v9 % mz
            if ( mz > 0 .and. len_scaling9(1) > 0.0 ) then
               call da_transform_through_rf_inv(grid, mz, be % v9 % rf_alpha, be % v9 % val, vv % v9, scaling)
            end if
            mz = be % v10 % mz
            if ( mz > 0 .and. len_scaling10(1) > 0.0 ) then
               call da_transform_through_rf_inv(grid, mz, be % v10 % rf_alpha, be % v10 % val, vv % v10, scaling)
            end if
	    if ( use_cv_nr ) then
               mz = be % v11 % mz
               if ( mz > 0 .and. len_scaling11(1) > 0.0 ) then
                  call da_transform_through_rf_inv(grid, mz, be % v11 % rf_alpha, be % v11 % val, vv % v11, scaling)
               end if
            end if
      end select
   end if

   ! [2.7] Transform w control variable

   if ( use_rf ) then
      if ( .not. use_cv_w ) then
         vv % v12 = 0.0
      else
         mz = be % v12 % mz
         if ( mz > 0 .and. len_scaling12(1) > 0.0 ) then
            if ( cloud_cv_options == 2 ) then
               call da_transform_through_rf_inv(grid, mz, be % v12 % rf_alpha, be % v12 % val, vv % v12)
            else if ( cloud_cv_options == 3 ) then
               scaling = .true.
               call da_transform_through_rf_inv(grid, mz, be % v12 % rf_alpha, be % v12 % val, vv % v12, scaling)
            end if
         end if
      end if
   end if


   ! [2.8] Transform alpha control variable

   ne = be % ne
   if (ne > 0) then
      mz = be % alpha % mz
      if( use_rf )then
         do n = 1, ne
            call da_transform_through_rf_inv(grid, mz, be % alpha % rf_alpha, be % alpha % val, vv % alpha(:,:,:,n))
         end do
      endif
   endif

   if( use_rf )then
      !-------------------------------------------------------------------------
      ! [1.0] Fill 1D cv array from 3-dimensional vv arrays.
      !-------------------------------------------------------------------------
      call da_vv_to_cv( vv, grid%xp, be%cv_mz, be%ncv_mz, cv_size, cv)
   endif

   if (trace_use) call da_trace_exit("da_transform_vtovv_inv")

end subroutine da_transform_vtovv_inv
