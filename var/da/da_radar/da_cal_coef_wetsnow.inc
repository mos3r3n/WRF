subroutine cal_coef_wetsnow(temp,a_out,b_out)

  implicit none

  real, intent(in)  :: temp
  real, intent(out) :: a_out, b_out
  ! local
  real :: a, b, loga, tc0
  real, dimension(10), parameter:: sa = (/ 5.065339, -0.062659, -3.032362, 0.029469, -0.000285, &
                                           0.31255,   0.000204,  0.003199, 0.0,      -0.015952/)
  real, dimension(10), parameter:: sb = (/ 0.476221, -0.015896,  0.165977, 0.007468, -0.000141, &
                                           0.060366,  0.000079,  0.000594, 0.0,      -0.003577/)
  real, parameter:: mu_s = 0.6357
  real, parameter:: bm_s = 2.0
  real, parameter:: bv_s = 0.55

  real, dimension(18):: cse

  if (trace_use) call da_trace_exit("cal_coef_wetsnow")

  cse(1) = bm_s + 1.
  cse(2) = bm_s + 2.
  cse(3) = bm_s*2.
  cse(4) = bm_s + bv_s + 1.
  cse(5) = bm_s*2. + bv_s + 1.
  cse(6) = bm_s*2. + 1.
  cse(7) = bm_s + mu_s + 1.
  cse(8) = bm_s + mu_s + 2.
  cse(9) = bm_s + mu_s + 3.
  cse(10) = bm_s + mu_s + bv_s + 1.
  cse(11) = bm_s*2. + mu_s + bv_s + 1.
  cse(12) = bm_s*2. + mu_s + 1.
  cse(13) = bv_s + 2.
  cse(14) = bm_s + bv_s
  cse(15) = mu_s + 1.
  cse(16) = 1.0 + (1.0 + bv_s)/2.
  cse(17) = cse(16) + mu_s + 1.
  cse(18) = bv_s + mu_s + 3.

  tc0 = MIN(-0.1, temp-273.15)

  loga = sa(1) + sa(2)*tc0 + sa(3)*cse(3) + &
         sa(4)*tc0*cse(3) + sa(5)*tc0*tc0 + &
         sa(6)*cse(3)*cse(3) + sa(7)*tc0*tc0*cse(3) + &
         sa(8)*tc0*cse(3)*cse(3) + sa(9)*tc0*tc0*tc0 + &
         sa(10)*cse(3)*cse(3)*cse(3)
  a = 10.0**loga
  b = sb(1)+ sb(2)*tc0 + sb(3)*cse(3) + sb(4)*tc0*cse(3) + &
      sb(5)*tc0*tc0 + sb(6)*cse(3)*cse(3) + &
      sb(7)*tc0*tc0*cse(3) + sb(8)*tc0*cse(3)*cse(3) + &
      sb(9)*tc0*tc0*tc0 + sb(10)*cse(3)*cse(3)*cse(3)

  a_out = a
  b_out = b

  if (trace_use) call da_trace_exit("cal_coef_wetsnow")

end subroutine cal_coef_wetsnow
