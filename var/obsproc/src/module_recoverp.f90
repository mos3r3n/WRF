MODULE module_recoverp

   USE module_type
   USE module_func
   USE module_mm5

   INCLUDE 'missing.inc'

CONTAINS



 SUBROUTINE recover_pressure_from_height (max_number_of_obs , obs , &
                                          number_of_obs, print_hp_recover)

































   IMPLICIT NONE

   INTEGER,       INTENT ( IN )                :: max_number_of_obs
   TYPE (report), DIMENSION (max_number_of_obs):: obs
   INTEGER , INTENT ( IN )                     :: number_of_obs
   LOGICAL ,      INTENT (IN)                  :: print_hp_recover     

   TYPE (measurement), POINTER                 :: current
   INTEGER                                     :: qc_flag
   INTEGER                                     :: i, nlevel

   CHARACTER (LEN=80)                          :: filename
   LOGICAL                                     :: connected, consistent
   INTEGER                                     :: iunit, io_error
   CHARACTER (LEN = 32)                        :: proc_name = &
                                                 "recover_pressure_from_height"

   INCLUDE 'platform_interface.inc'



      WRITE (0,'(A)')  &
'------------------------------------------------------------------------------'
      WRITE (UNIT = 0, FMT = '(A,/)') 'PRESSURE RECOVERED FROM HEIGHTS:'

      IF (print_hp_recover) THEN

      filename = 'obs_recover_pressure.diag'
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








      IF (print_hp_recover) THEN

      WRITE (UNIT = IUNIT, FMT = '(A,/)') 'PRESSURE RECOVERED FROM HEIGHTS:'

      WRITE(UNIT = iunit , FMT = '(A,/,3(A,F10.2,A),/,3(A,F10.2,A),/)') &
        "REFERENCE STATE: ", &
        " HTOP = ", htop,"m,  ",  &
        " PTOP = ", ptop,"Pa, ", &
        " PS0  = ", ps0 ,"Pa, ", &
        " TS0  = ", ts0 ,"K,  ", &
        " TLP  = ", tlp ,"K,  ",&
        " DIS  = ", DIS(idd),"m."

        ENDIF




loop_all_1:&
      DO i = 1, number_of_obs

         IF ((obs (i) % info % discard)  .OR. .NOT. ASSOCIATED &
             (obs (i) % surface)) THEN

             CYCLE loop_all_1

         ENDIF



 
levels:&
         IF (obs (i) % info % bogus .OR. (.NOT. obs (i) % info % is_sound)) THEN

             

             IF (eps_equal &
                (obs (i) % surface % meas % pressure % data, missing_r, 1.))THEN
                 obs (i) % surface % meas % pressure % data = floor (ref_pres &
                (obs (i) % surface % meas % height   % data))
                 obs (i) % surface % meas % pressure % qc   = &
                                                         reference_atmosphere
             ELSE IF (obs (i) % surface % meas % pressure % qc == 0 .and. &
                      obs (i) % surface % meas % height   % qc == 0 .and. &
                      eps_equal &
                      (obs (i) % surface % meas % height % data, 0., 1.)) then

             
             
             
             
             

                      consistent = hp_consist (  &
                              obs (i) % surface % meas % height   % data, &
                              obs (i) % surface % meas % pressure % data, &
                              print_hp_recover, iunit)
                      if (.not.consistent) obs (i) % info % discard = .true.

             ENDIF

         ELSE levels







          CALL hp_sequence_check (obs (i), i, nlevel, print_hp_recover, iunit)

          CALL hp_missing_check (obs(i), i, nlevel)




          IF ((nlevel == 1) .AND. eps_equal &
              (obs (i) % surface % meas % pressure% data, missing_r, 1.0)) THEN

               obs (i) % info % discard = .TRUE.

               IF (print_hp_recover) THEN

               WRITE (UNIT = iunit, FMT = '(A,A,I5,A,A)') &
              "In recover_pressure_from_height: ","I = ", I," ID = ", &
               obs (i) % location % id (1:5)

               WRITE (UNIT = iunit, FMT = '(A,I5,A,F9.0,A,F9.0)') &
               "nlevel = ",  nlevel, &
               "height = ",  obs (i) % surface % meas % height   % data, &
               "pressure = ",obs (i) % surface % meas % pressure % data

                ENDIF

              CYCLE loop_all_1

          ENDIF




          IF (nlevel /= obs (i) % info % levels) THEN

              IF (print_hp_recover) THEN

                  WRITE (UNIT = iunit, FMT = '(3A,I4,A,I4)') &
                 "ID = ", obs(i)%location%id(1:5), ", expect ", &
                  obs (i) % info % levels, " levels, get ", nlevel

              ENDIF
              
          ENDIF




          CALL recover_p_from_h (obs (i), i, nlevel, print_hp_recover, iunit)

        ENDIF levels

      ENDDO loop_all_1

      IF (print_hp_recover) CLOSE (iunit)

 END SUBROUTINE recover_pressure_from_height



 SUBROUTINE hp_sequence_check (obs, i, nlevel, print_hp_recover, iunit)


      INTEGER,       INTENT (in)    :: i, iunit
      INTEGER,       INTENT (out)   :: nlevel 
      LOGICAL,       INTENT (in)    :: print_hp_recover
      TYPE (report), INTENT (inout) :: obs

      TYPE (measurement), POINTER   :: current
      REAL                          :: height1, height2, & 
                                       press1 , press2
      LOGICAL                       :: first_h, first_p


      current => obs % surface
      first_h = .false.
      first_p = .false.
      nlevel  = 0

loop_hp_level: &
    DO WHILE (ASSOCIATED (current))

       nlevel = nlevel + 1




       IF (.NOT. eps_equal (current%meas%height%data, missing_r, 1.0)) then

           IF (.NOT. first_h) then

                height1 = current%meas%height%data
                first_h = .true.

           ELSE

                height2 = current%meas%height%data

                IF (height2 <= height1) then

                    current%meas%height%qc = missing 
                    current%meas%height%data = missing_r

                    IF (print_hp_recover) THEN

                        WRITE (UNIT = iunit, FMT = '(A,/,4A,A,I3,2(A,f12.2))') &
                        "HEIGHT VIOLATION: ",           &
                        " FM = ", obs % info % platform (4:6), &
                        " ID = ", obs % location % id,         &
                        " LEVEL = ", nlevel,                   &
                        " H2 = ", height2,                     &
                        " H1 = ", height1 

                    ENDIF

                ELSE

                    height1 = height2

                ENDIF

             ENDIF

          ENDIF




          IF (.NOT.eps_equal(current%meas%pressure%data, missing_r, 1.0)) then

             IF (.NOT.first_p) THEN
                  press1  = current%meas%pressure%data
                  first_p = .true.
             ELSE
                  PRESS2 = current%meas%pressure%data

                  IF (press2 >= press1) THEN
                      current%meas%pressure%data = missing_r
                      current%meas%pressure%qc   = missing

                  IF (print_hp_recover) THEN

                      WRITE (UNIT = iunit, FMT = '(A,/,4A,A,I3,2(A,f12.2))') &
                      "PRESSURE VIOLATION: ",           &
                      " FM = ", obs % info % platform (4:6), &
                      " ID = ", obs % location % id,         &
                      " LEVEL = ", nlevel,                   &
                      " P2 = ", press1,                      &
                      " P1 = ", press1 

                 ENDIF

               ELSE

                 press1 = press2

               ENDIF

             ENDIF

          ENDIF

          

          current => current%next

      ENDDO loop_hp_level

END subroutine hp_sequence_check


 SUBROUTINE recover_p_from_h(obs, i, nlevel, print_hp_recover, iunit)


   IMPLICIT NONE

   INTEGER,       INTENT (in)    :: i, iunit, nlevel
   LOGICAL,       INTENT (in)    :: print_hp_recover
   TYPE (report), INTENT (inout) :: obs

   TYPE (measurement), POINTER                 :: current
   TYPE (field)      , dimension(nlevel)       :: pp, hh
   INTEGER                                     :: k, Lb, Le
   REAL                                        :: elev
   LOGICAL                                     :: do_it





      k  = 0
      current => obs % surface

loop_level:&
      DO WHILE (ASSOCIATED (current))

         k = k + 1
         hh (k) = current % meas % height
         pp (k) = current % meas % pressure
         current => current % next

      ENDDO loop_level






 
      CALL pressure_recover (nlevel, pp, hh, iunit, LB, LE, do_it, print_hp_recover)
      



      IF (do_it) THEN

        IF (print_hp_recover) THEN

            WRITE(UNIT = iunit,FMT = '(5A)', ADVANCE = 'no') &
            "== PRESSURE RECOVER DONE: ",    &
            " for FM=", obs % info % platform (4:6), &
            " ID=",     obs % location % id

        ENDIF

        IF (print_hp_recover) THEN

             IF (LB > 1 .OR. LE < nlevel) THEN
                 WRITE(UNIT = iunit,FMT = '(A)') &
               " Reference pressure may have been used."
             ELSE
                 WRITE(UNIT = iunit,FMT = '(A)') " "

             ENDIF

        ENDIF





        current => obs % surface

        k = 0

back_level: &
        DO WHILE (ASSOCIATED (current))

           k = k + 1

           current % meas % height % data   = hh (k) % data
           current % meas % height % qc     =          hh (k) % qc
           current % meas % pressure % data = FLOOR (pp (k) % data)
           current % meas % pressure % qc   =        pp (k) % qc
           current => current % next

        ENDDO back_level

      ENDIF
 
 END SUBROUTINE recover_p_from_h


 FUNCTION hp_consist (hin,pin,print_out,iunit) RESULT (hout)

 
    IMPLICIT NONE

    REAL,    INTENT (in) :: hin, pin
    LOGICAL, INTENT (in) :: print_out
    INTEGER, INTENT (in) :: iunit
    LOGICAL              :: hout
  
    REAL, parameter  :: hmax = 1000. 
                                     
                                     

    REAL             :: h_ref, hdiff

      if (pin <= 0) then
        write(unit=0, fmt='(A,f12.2)') &
           "In function hp_consist, Pressure voilation: P=",pin
        hout = .false.
        return
      endif

      hout = .true.

      h_ref = Ref_height(pin)

      hdiff  = ABS (hin - h_ref)

      IF (hdiff > (50000/pin)*hmax) THEN

          IF (print_out) THEN
              WRITE (UNIT = iunit, FMT = '(/,A,/,3(A,F12.3,/))') &
          "   Pressure / height inconsistency: ", &
          "   Pressure    =  ", pin, &
          "   height      =  ", hin, &
          "   ref_height  =  ", h_ref
          ENDIF

          hout = .false.

      ENDIF

 END FUNCTION hp_consist

subroutine pressure_recover(level, pp, hh, iunit, LB, LE, do_it, print_hp_recover)

   IMPLICIT NONE

   INTEGER,                   INTENT(in)         :: level, iunit
   TYPE (field), dimension(level), INTENT(inout) :: hh, pp
   INTEGER,                   INTENT(out)        :: LB, LE
   LOGICAL,                   INTENT(out)        :: do_it
   LOGICAL,                   INTENT (in)        :: print_hp_recover
                     
   INTEGER               :: k, L, L1, L2, Lstart
   REAL                  :: height1, height2, &
                            press1 , press2 , bb, diff_pr
   LOGICAL               :: first_L, second_L, &
                            consist1, consist2


   LB = 0
   LE = 0
   do_it = .false.

   L1 = 0
   L2 = 0
   first_L  = .false.
   second_L = .false.
   consist1 = .true.
   consist2 = .true.



   if (level == 1) then
     return
   endif

   loop_level: do k = 1,level

   if (.NOT.first_L) then

   

     if (.NOT.eps_equal (pp(k)%data, missing_r, 1.0) .and. &
         .NOT.eps_equal (hh(k)%data, missing_r, 1.0) ) then
         L1 = k
         LB = L1
         Lstart = L1
         first_L = .true.
     endif
   else

   


     if (.NOT.eps_equal (pp(k)%data, missing_r, 1.0) .and. &
         .NOT.eps_equal (hh(k)%data, missing_r, 1.0) ) then
         L2 = k
         second_L = .true.
     endif
   endif

   if (first_L) then
      height1 = hh(L1) % data
      press1  = pp(L1) % data

      if (Lstart > 1) then

   
          if (eps_equal(pp(1)%data, missing_r, 1.0)) then
          L2 = L1
          second_L = .true.
          L1 = 1
          Lstart = 1
          height1 = hh(L1) % data
   
          press1      = Ref_pres(height1)
          press2      = Ref_pres(hh(L2)%data)
          diff_pr     = press1 - press2
          press1      = pp(L2)%data + diff_pr
          pp(L1)%data = press1
          pp(L1)%qc   = reference_atmosphere
          else
            do L = 1, L2
    
              if (eps_equal(pp(L)%data, missing_r, 1.0)) then
                 pp(L)%data = Ref_pres(hh(L)%data)
                 pp(L)%qc   = reference_atmosphere
              endif
            enddo
          endif
          do_it = .true.
      endif

   
      consist1 = hp_consist (height1,press1,print_hp_recover,iunit) 
      if (.NOT.consist1) then
         WRITE(UNIT=IUNIT, FMT='(A,2F12.2)') &
            "FAILED IN H/P CONSISTENCY CHECK: H1,P1:",height1,press1
         first_L = .FALSE.
         hh(L1) % data = missing_r
         hh(L1) % qc   = missing
         consist1 = .true.
         CYCLE loop_level
      endif
   endif

   if (second_L) then

       height2 = hh(L2) % data
       press2  = pp(L2) % data

       consist2 = hp_consist (height2,press2,print_hp_recover,iunit)
      if (.NOT.consist2) then
         WRITE(UNIT=IUNIT, FMT='(A,I4,2F12.2)') &
            "FAILED IN H/P CONSISTENCY CHECK: L2,H2,P2:",L2,height2,press2
         second_L = .FALSE.
         hh(L2) % data = missing_r
         hh(L2) % qc   = missing
         consist2 = .true.
         CYCLE loop_level
      endif

   
 
     if (L2 <= (L1 + 1)) then
       L1 = L2
       second_L = .false.
     else

   

       BB = (height2 - height1)/ALOG(press2/press1)
       do L = L1+1, L2-1

         if (eps_equal(pp(L)%data, missing_r, 1.0)) then
           if (eps_equal(hh(L)%data, missing_r, 1.0)) then
              WRITE(0,'(/A,A)') &
                "Both pressure and height are missing, ", &
                "this should never be happened???"
              STOP 33333
           endif
           pp(L)%data = press1*EXP((hh(L)%data - height1)/BB)
           pp(L)%qc   = 0
           do_it = .true.
         endif
       enddo
       L1 = L2
       second_L = .false.
     endif
   endif

   enddo loop_level

   

   LE = L2
   if (level > L2) then
     do k = L2+1, level
       if (eps_equal(pp(k)%data, missing_r, 1.0)) then
   
          if (L2 > 0) then
            press1  = Ref_pres(hh(L2)%data)
            press2  = Ref_pres(hh(k )%data)
            diff_pr = press1 - press2
            pp(k)%data = pp(L2)%data - diff_pr
          else
            pp(k)%data = Ref_pres(hh(k )%data)
          endif
          pp(k)%qc   =  reference_atmosphere
          do_it = .true.
       endif
     enddo
   endif

end subroutine pressure_recover

 SUBROUTINE hp_missing_check (obs, i, nlevel)


      INTEGER,       INTENT (in)    :: i
      INTEGER,       INTENT (out)   :: nlevel 
      TYPE (report), INTENT (inout) :: obs

      TYPE (measurement), POINTER   :: current, tmp, new, pre
      integer                       :: nn

      current => obs % surface
      new => current % next
      nn = 0

loop_hp_level: &
    DO WHILE (ASSOCIATED (new))
      
      nn = nn + 1

      tmp => new % next


      IF (eps_equal (new % meas % pressure % data, missing_r, 1.) .and. &
          eps_equal (new % meas % height % data, missing_r, 1.) ) THEN

         write(0,'("==> Missing P and H:",i3,2f15.6)') nn, &
                new%meas%pressure%data, new%meas%height%data 
         nullify(current % next)

         current % next => tmp

1001     continue

         tmp => tmp%next


         if (associated (current%next) .and. &
             eps_equal (current%next%meas%pressure% data, missing_r, 1.) .and.&
             eps_equal (current%next%meas% height % data, missing_r, 1.) ) THEN

             nullify(current % next)
             current % next => tmp
             goto 1001

         endif

         current => current % next

      ELSE

         current => current % next

      ENDIF
      new => current % next

    END DO loop_hp_level


      nn = 0
      current =>  obs % surface

      do while (associated(current))
        nn = nn + 1


        current => current%next
      enddo

      nlevel = nn
      if (obs%info%levels /= nlevel ) then
        obs%info%levels = nlevel
        write(0, fmt='(a, i3, 2x, 2a, 2x, a, 2f10.3/)') &
             'After missing check: Number of levels = ', obs%info%levels, &
             'OBS=', obs%info%platform(1:12), &
             'LOC=', obs%location%latitude, obs%location%longitude
      endif
 
END  SUBROUTINE hp_missing_check

END MODULE module_recoverp
