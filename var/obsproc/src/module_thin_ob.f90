MODULE module_thin_ob

  USE module_type
  USE module_func
  USE module_mm5
  USE module_map
  USE map_utils
  USE module_namelist

CONTAINS

SUBROUTINE THIN_OB (max_number_of_obs, obs, number_of_obs, mix, mjx, &
                                 missing_r, Ob_name, Ob_fm, Delta_P)
















  implicit none

  INTEGER,                                  INTENT (in) :: max_number_of_obs
  TYPE (report), DIMENSION (max_number_of_obs), INTENT (inout) :: obs
  INTEGER,                                      INTENT (in) :: number_of_obs
  CHARACTER (LEN = *),                          INTENT (in) :: Ob_name
  INTEGER,                                      INTENT (in) :: Ob_fm
  REAL,                                         INTENT (in) :: missing_r
  REAL   ,                     OPTIONAL,        INTENT (in) :: Delta_P

  INTEGER                       :: n, fm, i, j, k, kk, num_box, & 
                                   max_num, mix, mjx, num,  nn, total, &
                                   max_box, LV, N_selected, N1_selected
  INTEGER, ALLOCATABLE          :: Num_ob(:,:,:), Ob_index(:,:,:,:)
  REAL                          :: X, Y, dx, dy, RR, Rmin, R1min
  TYPE ( meas_data )            :: new
  INTEGER                       :: nvalids_fm








   

       if (Ob_fm == 88) then
         LV      = 100
         max_box =  50

   

       else if (Ob_fm == 125 .or. Ob_fm == 126 .or. Ob_fm == 281) then
         LV      =   1
         max_box = 400

       else
         WRITE(0,'(A,I3,A)') ' FM=',Ob_fm,' NOT SATELLITE OBS,', &
                             ' NO THINING APPLIED TO IT.'
         return
       endif

       allocate (Num_ob  (mix,mjx,LV))
       allocate (Ob_index(mix,mjx,LV,max_box))

       WRITE(0,'(/"Thining OBS ==> ",A,1X,"FM=",I3,2X,      &
       &                   "mix, mjx, LV, max_box:",4I6)') &
       &    Ob_name, Ob_fm, mix, mjx, LV, max_box 




       Num_ob = 0
       nvalids_fm = 0



      
stations: &
       DO n = 1, number_of_obs

stations_valid: &
         IF (obs (n)%info%discard ) THEN

           CYCLE  stations

         ELSE stations_valid

           READ (obs (n) % info % platform (4:6), '(I3)') fm

           if (fm /= Ob_fm)  CYCLE stations

           nvalids_fm = nvalids_fm + 1

       SELECT CASE (Ob_fm)



       CASE (88)

           new = obs (n) % surface % meas

           if (eps_equal (new%pressure%data, missing_r, 1.)) then
              CYCLE stations
           else



           
           if (fg_format == 'MM5') then
             call LLXY (obs (n) % location % latitude, &
                        obs (n) % location % longitude, X, Y)
           else if (fg_format == 'WRF') then
             call latlon_to_ij(map_info, obs(n)%location%latitude, &
                               obs (n)%location%longitude, X, Y)
             X = X + .5
             Y = Y + .5
           endif
             J = INT(X + .5)
             I = INT(Y + .5)
             K = INT(new % pressure % data / Delta_p)



             Num_ob (i,j,k) = Num_ob (i,j,k) + 1



             Ob_index(i,j,k, Num_ob(i,j,k)) = n

           endif




       CASE (125, 126)



           if (fg_format == 'MM5') then
             call LLXY (obs (n) % location % latitude, &
                        obs (n) % location % longitude, X, Y)
           else if (fg_format == 'WRF') then
             call latlon_to_ij(map_info, obs(n)%location%latitude, &
                               obs (n)%location%longitude, X, Y)
             X = X + .5
             Y = Y + .5
           endif
             J = INT(X + .5)
             I = INT(Y + .5)
             K = 1


             Num_ob (i,j,k) = Num_ob (i,j,k) + 1



             Ob_index(i,j,k, Num_ob(i,j,k)) = n


       
       CASE ( 281)



           if (fg_format == 'MM5') then
             call LLXY (obs (n) % location % latitude, &
                        obs (n) % location % longitude, X, Y)
           else if (fg_format == 'WRF') then
             call latlon_to_ij(map_info, obs(n)%location%latitude, &
                               obs (n)%location%longitude, X, Y)
             X = X + .5
             Y = Y + .5
           endif
             J = INT(X + .5)
             I = INT(Y + .5)
             K = 1


             Num_ob (i,j,k) = Num_ob (i,j,k) + 1



             Ob_index(i,j,k, Num_ob(i,j,k)) = n

       

       CASE DEFAULT;
       
           

       END SELECT
         ENDIF stations_valid

      ENDDO  stations




      WRITE(0,'("VALID NO.=",I6)') nvalids_fm



      num_box = 0
      DO K = 1,LV
      kk = 0
      max_num = 0
        DO I = 1,MIX
        DO J = 1,MJX
          IF (Num_ob (i,j,k) > 0) THEN
            max_num = max(max_num, Num_ob (i,j,k))
            kk = kk + 1
            num_box = num_box + 1
          ENDIF
        ENDDO
        ENDDO
        if (kk >0) &
        WRITE(0,'(A,I5,A,I5,A,I3)') "LEVEL ", K, " NUM=", KK, " Max=", max_num
      ENDDO

      write(0,'(10X,"Total Number of Boxes ",I6)') num_box



        num_box = 0
        DO I = 1,MIX
        DO J = 1,MJX
          kk = 0
          DO K = 1,LV
            KK = KK + Num_ob (i,j,k)
          ENDDO
          if (kk > 0) num_box = num_box + 1
        enddo
        enddo
        write(0,'(10X,"Number of columns =", I5)') num_box




      DO K = 1,LV

          DO I = 1,MIX

  NEXT_box:DO J = 1,MJX

          N_selected  = -99
          N1_selected = -99

          num = Num_ob (i,j,k)
          if (num <= 1) cycle NEXT_box


 
          Rmin  = 100.0
          R1min = 100.0

   Select_ob:do nn = 1,num
            n = Ob_index(i,j,k,nn)

       SELECT CASE (Ob_fm)


 
       CASE (88)

            new = obs (n) % surface % meas
            if (new % pressure  % qc == 0 .and. &
                new % speed     % qc == 0 .and. &
                new % direction % qc == 0) then
                if (fg_format == 'MM5') then
                  call LLXY (obs (n) % location % latitude, &
                             obs (n) % location % longitude, X, Y)
                else if (fg_format == 'WRF') then
                  call latlon_to_ij(map_info, obs(n)%location%latitude, &
                                    obs (n)%location%longitude, X, Y)
                  X = X + .5
                  Y = Y + .5
                endif
                X = X + .5
                Y = Y + .5

                dx = X - float(J)
                dy = Y - float(I)
                RR = dx*dx + dy*dy
                if (RR < Rmin) then
                  N_selected = n
                  Rmin = RR
                endif
            endif




       CASE (125)
                if (fg_format == 'MM5') then
                  call LLXY (obs (n) % location % latitude, &
                             obs (n) % location % longitude, X, Y)
                else if (fg_format == 'WRF') then
                  call latlon_to_ij(map_info, obs(n)%location%latitude, &
                                    obs (n)%location%longitude, X, Y)
                  X = X + .5
                  Y = Y + .5
                endif
             X = X + .5
             Y = Y + .5

             dx = X - float(J)
             dy = Y - float(I)
             RR = dx*dx + dy*dy

    
             IF (ASSOCIATED (obs (n) % surface)) then
                new = obs (n) % surface % meas
                if (new % speed     % qc == 0) then
                   if (RR < Rmin) then
                      N_selected = n
                      Rmin = RR
                   endif
                endif
             endif
    
             if (obs(n)%ground% pw  % qc == 0) then
                if (RR < R1min) then
                   N1_selected = n
                   R1min = RR
                endif
             endif




       CASE (126)

            if (obs (n) % ground % tb19v % qc == 0 .and. &
                obs (n) % ground % tb19h % qc == 0 .and. &
                obs (n) % ground % tb22v % qc == 0 .and. &
                obs (n) % ground % tb37v % qc == 0 .and. &
                obs (n) % ground % tb37h % qc == 0 .and. &
                obs (n) % ground % tb85v % qc == 0 .and. &
                obs (n) % ground % tb85h % qc == 0) then
                if (fg_format == 'MM5') then
                  call LLXY (obs (n) % location % latitude, &
                             obs (n) % location % longitude, X, Y)
                else if (fg_format == 'WRF') then
                  call latlon_to_ij(map_info, obs(n)%location%latitude, &
                                    obs (n)%location%longitude, X, Y)
                  X = X + .5
                  Y = Y + .5
                endif
                X = X + .5
                Y = Y + .5

                dx = X - float(J)
                dy = Y - float(I)
                RR = dx*dx + dy*dy
                if (RR < Rmin) then
                  N_selected = n
                  Rmin = RR
                endif
             endif



       CASE (281)
                if (fg_format == 'MM5') then
                  call LLXY (obs (n) % location % latitude, &
                             obs (n) % location % longitude, X, Y)
                else if (fg_format == 'WRF') then
                  call latlon_to_ij(map_info, obs(n)%location%latitude, &
                                    obs (n)%location%longitude, X, Y)
                  X = X + .5
                  Y = Y + .5
                endif
                X = X + .5
                Y = Y + .5

             dx = X - float(J)
             dy = Y - float(I)
             RR = dx*dx + dy*dy

    
 
             IF (ASSOCIATED (obs (n) % surface)) then
                new = obs (n) % surface % meas
                if (new % speed     % qc == 0) then
                   if (RR < Rmin) then
                      N_selected = n
                      Rmin = RR
                   endif
                endif
             endif

       

       CASE DEFAULT;
       
           

       END SELECT

          enddo Select_ob



   Discard:do nn = 1,num
            n = Ob_index(i,j,k,nn)

       SELECT CASE (Ob_fm)


 
       CASE (88)

            if (n /= N_selected) obs(n) % info % discard = .TRUE.



       CASE (125)

            if (n /= N_selected .and. &
                n /= N1_selected) obs(n) % info % discard = .TRUE.




       CASE (126)

            if (n /= N_selected) obs(n) % info % discard = .TRUE.



       CASE (281)

            if (n /= N_selected) obs(n) % info % discard = .TRUE.

       

       CASE DEFAULT;
       
           

       END SELECT

          enddo Discard

     ENDDO NEXT_box

     ENDDO

     ENDDO



     deallocate (Num_ob)
     deallocate (Ob_index)

END SUBROUTINE THIN_OB 

END MODULE module_thin_ob
