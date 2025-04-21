MODULE module_recoverh







   USE module_type
   USE module_func
   USE module_per_type
   USE module_mm5

   INCLUDE 'missing.inc'

CONTAINS


SUBROUTINE recover_height_from_pressure(max_number_of_obs , &
                             obs , number_of_obs, print_hp_recover)






   IMPLICIT NONE

   INTEGER,       INTENT ( IN )                :: max_number_of_obs
   TYPE (report), DIMENSION (max_number_of_obs):: obs
   INTEGER , INTENT ( IN )                     :: number_of_obs
   LOGICAL , INTENT ( IN )                     :: print_hp_recover

   TYPE (measurement), POINTER                 :: current
   INTEGER                                     :: iunit     
   INTEGER                                     :: qc_flag
   INTEGER                                     :: i, j, nlevel, k, &
                                                  k_start, k_top
   CHARACTER (LEN = 80)                        :: filename
   CHARACTER (LEN = 80)                        :: proc_name = &
                                                 "recover_height_from_pressure"
   LOGICAL                                     :: connected, correct, failed
   INTEGER                                     :: io_error


   TYPE (field)  , dimension(9000)              :: hh
   REAL          , dimension(9000)              :: pp, tt, qq

   INCLUDE 'platform_interface.inc'


             WRITE (0,'(A)')  &
'------------------------------------------------------------------------------'
      WRITE (UNIT = 0, FMT = '(A,/)') 'HEIGHT RECOVERED FROM P, T, Q,..:'


      

      IF (print_hp_recover) THEN

      filename = 'obs_recover_height.diag'
      iunit    = 999

      INQUIRE (UNIT = iunit, OPENED = connected )

      IF (connected) CLOSE (iunit)

      OPEN (UNIT = iunit , FILE = filename , FORM = 'FORMATTED'  , &
            ACTION = 'WRITE' , STATUS = 'REPLACE', IOSTAT = io_error )

      IF (io_error .NE. 0) THEN
          CALL error_handler (proc_name, &
         "Unable to open output diagnostic file. " , filename, .TRUE.)
      ELSE
          WRITE (UNIT = 0, FMT = '(A,A,/)') &
         "Diagnostics in file ", TRIM (filename)
      ENDIF

      ENDIF

      IF (print_hp_recover) &
      WRITE (UNIT = IUNIT, FMT = '(/A/)') &
        'HEIGHT RECOVERED FROM PRESSURE FOR MULTI-LEVEL OBS DATA:'

      failed = .false.




      j = 0




loop_all: &
      DO i = 1, number_of_obs

         IF ((obs (i) % info % discard)  .OR. .NOT. ASSOCIATED &
             (obs (i) % surface)) THEN

             CYCLE loop_all

         ENDIF





surface:&
         IF ((ASSOCIATED (obs (i) % surface)) .AND. &
        (.NOT.ASSOCIATED (obs (i) % surface % next))) THEN

             

             IF (eps_equal (&
                 obs (i) % surface % meas % height % data, missing_r, 1.)) THEN

                 obs (i) % surface % meas % height   % data = ref_height &
                (obs (i) % surface % meas % pressure % data)
                 obs (i) % surface % meas % height   % qc   = Reference_atmosphere

                 obs (i) % surface % meas % height % data = NINT &
                (obs (i) % surface % meas % height % data + .5)
                 obs (i) % surface % meas % height % data = MAX &
                (obs (i) % surface % meas % height % data, 0.)


                 IF (print_hp_recover) THEN

                     WRITE (UNIT = iunit,FMT = '(/,A,A5,1X,A23,2F9.3)')        &
                    "Recover 1 level  station id = ",                          &
                     TRIM  (obs (i) % location % id ) ,                        &
                     TRIM  (obs (i) % location % name),                        &
                            obs (i) % location % latitude,                     &
                            obs (i) % location % longitude
                     WRITE (UNIT = iunit, FMT = '(2(A,I5),A)')                 &
                    "Use reference state to infer height (",                   &
                     INT (obs (i) % surface % meas % height % data),           &
                     "m) from pressure (",&
                     INT (obs (i) % surface % meas % pressure % data/100.),"hPa)."
                 ENDIF

             ENDIF

             







             CYCLE loop_all

         ENDIF surface




         call reorder(obs(i), i, 'pressure', failed)
         if (failed) then
            obs(i) % info % discard = .true.
            cycle loop_all
         endif





         nlevel  = 0
         correct = .FALSE. 
         current => obs(i)%surface

count_level_1:&
         DO WHILE (ASSOCIATED (current))

            nlevel = nlevel + 1

            hh (nlevel) = current%meas%height
            pp (nlevel) = current%meas%pressure%data
            tt (nlevel) = current%meas%temperature%data
            qq (nlevel) = current%meas%qv%data

            IF (eps_equal(current%meas%height%data, missing_r, 1.)) THEN
                correct = .TRUE. 
            ENDIF

            current => current%next

         ENDDO count_level_1




         IF (.not.correct) CYCLE loop_all





levels:&
         IF (nlevel <= 1) THEN

             IF (print_hp_recover) THEN
                 WRITE (UNIT = iunit , FMT = '(A,A5,1X,A23,2F9.3)')     &
                "No level found for sound id= " ,                       &
                 TRIM  (obs (i) % location % id ) ,                     &
                 TRIM  (obs (i) % location % name),                     &
                        obs (i) % location % latitude,                  &
                        obs (i) % location % longitude
             ENDIF

             STOP 'in recover_height.F90'

         ELSE IF (nlevel > 1) THEN levels

             CALL recover_h_from_ptq (pp, tt, qq, hh, nlevel,k_start,k_top)

             IF (k_start >= 1 .or. k_top <= nlevel) THEN

                 IF (print_hp_recover) &
                     WRITE (UNIT = iunit, FMT = '(/,A,A5,1X,A23,2F9.3)') &
                    "Recover upperair station id = ",                    &
                     TRIM  (obs (i) % location % id ) ,                  &
                     TRIM  (obs (i) % location % name),                  &
                            obs (i) % location % latitude,               &
                            obs (i) % location % longitude

                 ENDIF

         ENDIF levels




         k = 0 
         current => obs(i)%surface

correct_levels: &
         DO WHILE (ASSOCIATED (current))

            k = k + 1



                 IF (print_hp_recover) THEN
                    WRITE (UNIT = iunit, FMT = '(2(A,I5),A)')    &
                   "Height missing set to ",INT (hh (k) % data), &
                   "m, pressure = ",INT (pp(k)/100.),"hpa."
                 ENDIF

                 current%meas%height % data = CEILING (hh(k)%data) 
                 current%meas%height % qc   =          hh(k)%qc 



            current => current%next


         ENDDO correct_levels

         call reorder(obs(i), i, 'pressure', failed)
         if (failed) then
            obs(i) % info % discard = .true.
            cycle loop_all
         endif




      ENDDO loop_all

     IF (print_hp_recover) CLOSE (IUNIT)

END SUBROUTINE recover_height_from_pressure



 SUBROUTINE recover_h_from_ptq(P,T,Q,HGT,KXS,K_START,K_TOP)







































  IMPLICIT NONE
 
  INTEGER,                      INTENT(in)    :: KXS 
  REAL        , DIMENSION(KXS), INTENT(in)    :: T, Q, P
  TYPE (field), DIMENSION(KXS), INTENT(inout) :: HGT
  INTEGER,                      INTENT(out)   :: k_start,k_top

  INTEGER                :: k, kk, kwk, L, K0, K1, K2
  REAL                   :: height0, height1, diff_hp, &
                            TM1, TM2, ALNP, DDH, DDP, DHH, &
                            dh1, dh2, dp1, dp2, aa, bb, cc
  INTEGER,dimension(9000) :: KP
  REAL   ,DIMENSION(9000) :: TWK, QWK, PWK, HWK, DHGT
  LOGICAL                :: Vert_ok
  
  TYPE (field), DIMENSION(KXS) :: HGT0, HGT1

  include 'constants.inc'


     HGT0 = HGT






     K_top   = kxs+1
     K_start = 0

first_level: &
     DO k = 1, kxs

        IF (.NOT. eps_equal(P  (k)     , missing_r, 1.) .AND. &
            .NOT. eps_equal(HGT(k)%data, missing_r, 1.) .AND. &
            .NOT. eps_equal(T  (k)     , missing_r, 1.)) THEN
             K_start = k
             EXIT first_level
        ENDIF

     ENDDO first_level

    
    
    
     IF (K_start == 0) THEN

        DO k = 1, kxs

           IF (.NOT. eps_equal(P  (k)     , missing_r, 1.) .AND. &
                     eps_equal(HGT(k)%data, missing_r, 1.)) THEN

                               HGT(k)%data = ref_height (P  (k) )
                               HGT(k)%qc   = reference_atmosphere
           ENDIF

        ENDDO

    
    

1999    Vert_ok = .True.
        
        DO k = 2, kxs
          if ((P(k) < P(K-1)) .and. (HGT(k)%data > HGT(k-1)%data)) then
            cycle
          else
            Vert_ok = .False. 
            if (HGT(k)%qc <= 0) then
            
               if (k > 2) then
                  AA = P(k  )-P(k-2)
                  BB = P(k-1)-P(k-2)
                  CC = AA - BB
                  HGT(k-1)%data = (HGT(k)%data*BB + HGT(k-2)%data * CC) / AA

                  if (HGT(k-1)%qc>0) HGT(k-1)%qc   = - HGT(k-1)%qc


               else
                  if ( k <= kxs-1 ) then
                    AA = P(k+1) - P(k)
                    BB = P(k-1) - P(k)
                    CC = AA - BB
                    HGT(k-1)%data = (HGT(k+1)%data*BB + HGT(k)%data * CC) / AA
                  else

                    AA = HGT(k)%data - ref_height (P  (k) )
                    HGT(k-1)%data = HGT(k-1)%data + AA
                  endif

                  if (HGT(k-1)%qc>0) HGT(k-1)%qc   = - HGT(k-1)%qc
               endif
            else
            
               if ( k <= kxs-1 ) then
                 AA = P(k+1) - P(k-1)
                 BB = P(k)   - P(k-1)
                 CC = AA - BB
                 HGT(k)%data = (HGT(k+1)%data*BB + HGT(k-1)%data * CC) / AA
               else

                 AA = HGT(k-1)%data - ref_height (P  (k-1) )
                 HGT(k)%data = HGT(k)%data + AA
               endif

               if (HGT(k-1)%qc>0) HGT(k-1)%qc   = - HGT(k-1)%qc
            endif
          endif
        ENDDO
        if (.Not. Vert_ok) goto 1999

        K_start = 1
        K_top   = kxs


        DO k = 1, kxs
           HGT(k)%qc   = abs(HGT(k)%qc)

        ENDDO

        RETURN

     ENDIf







     IF (k_start > 1) THEN

         

         height1 = Ref_height (p(k_start))

         DO k = k_start-1, 1, -1

           

           IF (eps_equal (hgt(k)%data, missing_r, 1.)) THEN

               IF (p(k)-p(k_start) > 20000.) THEN

               

                  HGT(k)%data  = Ref_height(P(k))
                  HGT(k)%qc    = reference_atmosphere

               ELSE

                  HGT(k)%data  = HGT(k_start)%data - height1 &
                               + Ref_height(P(k))
                  HGT(k)%qc    = reference_OBS_scaled

               ENDIF

          ENDIF

       ENDDO

     ENDIF

     

     kwk = 0

temp_search: &
      DO k = k_start, KXS

       IF (.NOT.eps_equal(T(k), missing_r, 1.)) THEN
            kwk = kwk+1
            pwk (kwk) = P(k)
            twk (kwk) = T(k)
            qwk (kwk) = q(k)
      ENDIF

     ENDDO temp_search

     HWK(1) = HGT(k_start)%data * G



hydro_int:  &
      DO K=2,KWK

         ALNP = ALOG (PWK(K-1) /PWK(K)) * gasr

         IF (.NOT.eps_equal(QWK(k), missing_r, 1.)) THEN
              TM2 = TWK(K  )*(1.+0.608*QWK(K  ))
         ELSE
              TM2 = TWK(K  )
         ENDIF

         IF (.NOT.eps_equal(QWK(k-1), missing_r, 1.)) THEN
              TM1 = TWK(K-1)*(1.+0.608*QWK(K-1))
         ELSE
              TM1 = TWK(K-1)
         ENDIF
              HWK(K) = HWK(K-1) + .5*(TM1+TM2) * ALNP
      ENDDO hydro_int




      K0 = 1
      KP(1) = 1
      DHGT(1) = 0

calibration: &
      DO K = 1,KXS
         IF (eps_equal(HGT(K)%data, missing_r, 1.)) then
          
        ELSE
          DO KK = 1,KWK
          IF (P(K).EQ.PWK(KK)) THEN
            K0 = K0+1
            KP(K0) = KK
            DHGT(K0) = HWK(KK)/G - HGT(K)%data
            CYCLE calibration
          ENDIF
          ENDDO
        ENDIF

     ENDDO calibration



     DO L = 1,K0-1
        K1 = KP(L)
        K2 = KP(L+1)
        DO KK = K1,K2-1
          DDH = DHGT(L+1) - DHGT(L)
          DDP = ALOG(PWK(K2)/PWK(K1))
          DHH = DHGT(L) + ALOG(PWK(KK)/PWK(K1))*DDH/DDP
          HWK(KK) = HWK(KK)/G - DHH
        END DO
     END DO



     DO K = KP(K0),KWK
         HWK(K) = HWK(K)/G - DHGT(K0)
     END DO






     height1 = Ref_height(Pwk(kwk))

above_k_start: DO K = k_start,KXS

     if (abs(P(K) - PWK(KWK)) < 0.01) k_top = k



      IF (P(K) >= PWK(KWK)) then
  
        DO KK = 1,KWK
          IF (P(K).EQ.PWK(KK)) THEN
             HGT(K)%data = HWK(KK)
          ELSE IF (KK.LT.KWK .AND. &
                 P(K).LT.PWK(KK) .AND. P(K).GT.PWK(KK+1)) THEN



            ALNP = ALOG (P(K)/PWK(KK)) / ALOG(PWK(KK+1)/PWK(KK))
            HGT(K)%data = HWK(KK) + ALNP*(HWK(KK+1)-HWK(KK))
          ENDIF
        ENDDO
        HGT(k)%qc    =  Hydrostatic_recover
      ELSE
        if ((PWK(KWK)-P(K)) > 10000.) then

     

           HGT(k)%data  = Ref_height(P(k))
           HGT(k)%qc    = reference_atmosphere
        else
           HGT(k)%data  = Hwk(kwk) - height1 &
                        + Ref_height(P(k))
           HGT(k)%qc    = reference_OBS_scaled
        endif
      ENDIF



    ENDDO above_k_start














    k0 = 1
    do k = 1,kxs
      if (HGT0(k) % qc == 0) then
        k0 = k
        exit
      endif
    enddo

    k1 = 0

    HGT1 = HGT
    do k = k0, kxs


      if (HGT0(k) % qc /= 0 .and. k1 == 0) then
         k1 = k-1
      else if (HGT0(k) % qc == 0 .and. &
               abs(HGT0(k)%data - HGT(k)%data) <= 0.10*HGT(k)%data ) then
         HGT1(k) = HGT0(k)
      endif


      if (HGT0(k) % qc == 0 .and. k1 > 0 .and. k > k1+1) then
         k2 = k

         if (abs(HGT0(k2)%data - HGT(k2)%data) > 0.10*HGT(k2)%data) then





           HGT0(k) = HGT1(k)

         else

           HGT1(k) = HGT0(k)


         dp2 = p(k2) - p(k1)
         dh1 = HGT0(k1)%data - HGT(k1)%data
         dh2 = HGT0(k2)%data - HGT(k2)%data
         do kk = k1+1, k2-1
           dp1 = p(kk) - p(k1) 
           HGT1(kk)%data = HGT(kk)%data + dh1*(1-dp1/dp2) + dh2*dp1/dp2
           HGT1(kk)%qc   = HGT(kk)%qc
           HGT1(kk)%error= HGT(kk)%error 
         enddo
        endif
         k1 = 0
         k2 = 0
      endif

    enddo


    HGT = HGT1

 END subroutine recover_h_from_ptq


 SUBROUTINE reorder(obs, i, order_component, failed)


  IMPLICIT NONE

  INTEGER,       INTENT (in)    :: i
  CHARACTER*(*), INTENT (in)    :: order_component 
  TYPE (report), INTENT (inout) :: obs
  logical                       :: failed

  INCLUDE 'missing.inc'

  TYPE (measurement), pointer :: current, new, tmp, pre

  integer :: num, ii, num_loop, nlevels
  logical :: need_check

  failed = .false.
 
  ii = 0
  current => obs % surface
  count_levels:  do while (associated(current))
     ii = ii + 1
     current => current%next
  enddo count_levels
  nlevels = ii

  current => obs%surface



   num = 0
   ii  = 0

   missing_check: do while (associated(current))
      num = num + 1
      if(eps_equal(comp_field(current,order_component), &
                                       missing_r, 1.0)) then

         num=num-1
      end if
      current => current%next
   end do missing_check

   if (num /= nlevels) then
     write(0,'(/I5,A,A,A/3X,A,A,A,A,A,2I5,A,f10.3,A,f10.3)') I, &
        ' There are ',order_component,&
        ' at several levels missing. Reordering can not be done.', &
        ' FM=',obs%info%platform(4:5),' ID=',obs%location%id(1:5), &
        ' nlevels, num:', nlevels, num, &
        ' lat=', obs%location%latitude, ' long=', obs%location%longitude

     failed = .true.
   endif



   obs % info % levels = nlevels

   if (nlevels <= 1) return

   ii = 0
   num_loop = 0

   need_check = .true.

   put2order: do while(need_check)

   num_loop = num_loop + 1

      head_check: do while(need_check)

         current => obs%surface



         if(comp_field(current,order_component) < &
            comp_field(current%next,order_component)) then

            tmp => current
            current => current%next
            nullify(tmp%next)

            new  => current

            obs%surface => new



            if(eps_equal(comp_field(current,order_component), &
                                            missing_r, 1.0)) then
               num=num-1

               STOP 'in reorder_missing data'

            end if



            current => obs%surface

            new => obs%surface%next

            nullify(current%next)

            allocate(current%next)
            current%next => tmp
   
            tmp%next => new
         end if

         need_check = .false.
      end do head_check

      if(num < 3) exit put2order


















      pre     => obs%surface
      current => obs%surface%next
      new     => obs%surface%next%next

      do while (associated(new))
         if(comp_field(current,order_component) < &
            comp_field(current%next,order_component)) then

            tmp => new%next

            nullify(pre%next)
            nullify(current%next)
            nullify(new%next)

            current%next => tmp
            new%next => current
            pre%next => new

            need_check = .true.

            exit
         end if

         pre     =>     pre%next
         new     =>     new%next
         current => current%next
      end do















   end do put2order














 END subroutine reorder


 FUNCTION comp_field(current, order_component) result(xxx)


type (measurement), pointer :: current
character*(*),   intent(in) :: order_component
real                        :: xxx
 
     SELECT CASE (order_component)

       CASE ('pressure')

         xxx = current%meas%pressure%data

       CASE ('height')

         xxx =  current%meas%height%data
       
       CASE DEFAULT

         WRITE(0,'(A,A,A)') 'order_component=',order_component, &
                            ' is not defined correctly'
         STOP 'in_reorder'

     END SELECT

 END function comp_field

  
END MODULE module_recoverh
