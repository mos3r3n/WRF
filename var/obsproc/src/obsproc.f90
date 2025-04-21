PROGRAM main_obsproc






































   USE module_mm5
   USE module_map
   USE map_utils
   USE module_namelist

   USE module_decoded
   USE module_type
   USE module_func
   USE module_icao
   USE module_sort
   USE module_duplicate
   USE module_per_type
   USE module_recoverp
   USE module_recoverh
   USE module_diagnostics
   USE module_qc
   USE module_err_ncep
   USE module_err_afwa
   USE module_complete
   USE module_thin_ob
   USE module_write
   use module_stntbl

   IMPLICIT NONE

   CHARACTER (LEN = 80)                 :: nml_filename
   CHARACTER (LEN = 80)                 :: title, caption
   LOGICAL                              :: exist
   INTEGER                              :: ii, fm_code, ntime
   INTEGER                              :: ins, jew
   INTEGER                              :: loop_index, number_of_obs
   INTEGER                              :: total_dups_loc, total_dups_time
   INTEGER                              :: total_dups
   INTEGER                              :: map_projection
   REAL                                 :: missing_flag

   TYPE (report),      DIMENSION (:), ALLOCATABLE :: obs, obs_copy
   TYPE (measurement), POINTER                    :: next, current
   INTEGER,            DIMENSION (:), ALLOCATABLE :: index

   real  :: lat, lon, xjd, yid, xxi, yyj, xxi1, yyj1

   CHARACTER (LEN = 19)      :: time_min
   CHARACTER (LEN = 19)      :: time_fg
   CHARACTER (LEN = 19)      :: time_max
   CHARACTER (LEN = 31)      :: SLOT_TITLE
   LOGICAL                   :: dis0, dis1, dis2






      nml_filename = 'namelist.obsproc'

      WRITE (UNIT = 0, FMT = '(/,A,A,/,A)')        &
    ' READ NAMELIST FILE: ',TRIM  (nml_filename),  &
    ' ------------------'

      CALL get_namelist (trim(nml_filename))




   



      htop = 0.; h_tropo = 0.

      h_tropo = Ref_height (pis0)
      htop = Ref_height (ptop)
 
     height_max_icao = h_from_p_icao (0.0)









       CALL setup (domain_check_h, iproj, phic, xlonc, truelat1, truelat2, &
                   maxnes, nestix, nestjx, dis, numc, nesti, nestj, &
                   ixc, jxc, xcntr, ycntr, xn, pole, psi1,  c2, &
                   xim11, xjm11)

      if (fg_format == 'WRF') then
        lat =  map_info%lat1
        lon =  map_info%lon1
        call LLXY(lat, lon, xxi, yyj)
        write(0,'(/"         LLXY:lat, lon, (dot) xxi, yyj:",4f12.5 )') &
                                               lat, lon, xxi, yyj
        call latlon_to_ij(map_info, lat, lon, xxi, yyj)
        write(0,'( "latlon_to_ij:lat, lon, (cross)xxi, yyj:",4f12.5/)') &
                                               lat, lon, xxi, yyj
      endif




       ins = nestix (idd)
       jew = nestjx (idd)




      icor = 0;        
      caption (7*(icor+0)+1:7*(icor+1)) = "   READ"
      caption (7*(icor+1)+1:7*(icor+2)) = "  EMPTY"
      caption (7*(icor+2)+1:7*(icor+3)) = "OUTSIDE"




      nsynops = 0; nshipss = 0; nmetars = 0; npilots = 0; nsounds = 0;
      nsatems = 0; nsatobs = 0; naireps = 0; ngpspws = 0; namdars = 0;
      nssmt1s = 0; nssmt2s = 0; nssmis  = 0; ntovss  = 0; nqscats = 0;
      nothers = 0; nprofls = 0; nbuoyss = 0; ngpsztd = 0; ngpsref = 0;
      nboguss = 0; nairss  = 0; ngpseph = 0; ntamdar = 0;
 



      number_of_obs   = 0
      total_dups_time = 0
      total_dups_loc  = 0




      ALLOCATE (obs   (max_number_of_obs))
      ALLOCATE (index (max_number_of_obs))




      INQUIRE (FILE = obs_gts_filename, EXIST = exist )

      IF (exist .and. LEN(TRIM(obs_gts_filename))>0) THEN

         call read_msfc_table('msfc.tbl')

      

        CALL read_obs_gts (obs_gts_filename, obs, number_of_obs, &
          max_number_of_obs, fatal_if_exceed_max_obs, print_gts_read, &
          ins, jew, time_window_min, time_window_max,                 &
          map_projection, missing_flag)

      

        DO loop_index = number_of_obs+1, max_number_of_obs
           NULLIFY (obs (loop_index) % surface)
        ENDDO

      ELSE
         WRITE (0,'(/,A,/)') "No decoded observation file to read."
         STOP
      ENDIF




      IF (number_of_obs .GT. 0) THEN

      icor = icor + 3




      
      
      
      
      
      
      




      CALL recover_pressure_from_height (max_number_of_obs , &
                                         obs, number_of_obs, print_recoverp)

      
      CALL check_obs (max_number_of_obs, obs, number_of_obs, 'pressure')




      CALL sort_obs (obs ,number_of_obs , compare_loc, index )






      caption (7*icor+1:7*(icor+1)) = "LOCDUPL"

      CALL check_duplicate_loc (obs ,index ,number_of_obs, total_dups_loc, &
                                time_analysis, print_duplicate_loc)

      icor = icor + 1




      caption (7*icor+1:7*(icor+1)) = "TIMDUPL"

      if (use_for /= '4DVAR' )  &
      CALL check_duplicate_time (obs ,index ,number_of_obs, total_dups_time, &
                                 time_analysis, print_duplicate_time)

      icor = icor + 1




      total_dups = total_dups_loc + total_dups_time





      CALL sort_obs (obs ,number_of_obs , compare_tim, index )

      
      









      CALL derived_quantities (max_number_of_obs , obs , number_of_obs)




      CALL recover_height_from_pressure (max_number_of_obs , &
                                         obs, number_of_obs, print_recoverh)

      
      CALL check_obs (max_number_of_obs, obs, number_of_obs, 'height')




      

      INQUIRE (FILE = obs_err_filename, EXIST = exist )

      IF (exist) THEN

          CALL obs_err_afwa (obs_err_filename, &
                         max_number_of_obs, obs, number_of_obs)

      ELSE

         WRITE (0,'(/,A,/)') "No obs err input file "//trim(obs_err_filename)//" found."
         STOP

         

         

         
         

      ENDIF







      CALL proc_qc1 (max_number_of_obs , obs , number_of_obs,            &
                     qc_test_vert_consistency , qc_test_convective_adj , &
                     print_qc_vert, print_qc_conv)




      CALL proc_qc2 (max_number_of_obs , obs , number_of_obs, &
                     qc_test_above_lid, print_qc_lid) 




      caption (7*icor+1:7*(icor+1)) = "UNCOMPL"

      CALL check_completness (max_number_of_obs , &
                              obs, number_of_obs, remove_above_lid, &
                              print_uncomplete)

      icor = icor + 1




      caption (7*icor+1:7*(icor+1)+1) = "INGESTD"
      title = "INGESTED OBSERVATION AFTER CHECKS:"

      CALL print_per_type (TRIM (title), TRIM (caption), icor)

      icor = icor+1







      CALL qc_reduction (max_number_of_obs, obs, number_of_obs)



      IF (Thining_SATOB) &
      CALL THIN_OB (max_number_of_obs, obs, number_of_obs, &
                   nestix(idd), nestjx(idd), missing_flag, &
                                        'SATOB', 88, 1000.0)

      IF (Thining_SSMI) THEN
      CALL THIN_OB (max_number_of_obs, obs, number_of_obs, &
                   nestix(idd), nestjx(idd), missing_flag, &
                                        'SSMI_Rtvl', 125)

      CALL THIN_OB (max_number_of_obs, obs, number_of_obs, &
                   nestix(idd), nestjx(idd), missing_flag, &
                                        'SSMI_Tb', 126)
      ENDIF

      IF (Thining_QSCAT) &
      CALL THIN_OB (max_number_of_obs, obs, number_of_obs, &
                   nestix(idd), nestjx(idd), missing_flag, &
                                        'Qscatcat', 281)
      






      allocate (obs_copy(1:max_number_of_obs))      
      obs_copy = obs




      do ntime = 1, num_time_slots




        if ( num_time_slots > 1 ) then
      



          if ( ntime == 1 .or. ntime == num_time_slots ) then
             idt = slot_len / 2 - 1
          else
             idt = slot_len - 1
          endif

          if (ntime == 1) then
             time_min  = time_window_min
             time_fg  = time_window_min
          else 
             call geth_newdate (time_min, time_max, 1)
             if (ntime == num_time_slots ) then
                 time_fg = time_window_max
             else
                 call geth_newdate (time_fg, time_min, slot_len/2)
             endif           
          endif
          call geth_newdate (time_max, time_min, idt)
        
        else
         



          time_min = time_window_min
          call  geth_newdate (time_max, time_window_max, -1)
          time_fg = time_analysis

        endif



             
        write(0,'(//a,i2,4(2x,a))') "slot=",ntime, & 
            "time_min, time_fg, time_max:", time_min, time_fg, time_max




        deallocate (obs)
        allocate(obs(1:max_number_of_obs))

        obs = obs_copy




        do ii = 1,number_of_obs







        dis0 = obs(ii)%info%discard
        dis1 = .false.
        dis2 = .false.




        read(obs(ii)%info%platform(4:6),'(i3)') fm_code
        if ( (.not.write_synop .and. (fm_code==12  .or. fm_code==14)) .or. &
             (.not.write_ship  .and.  fm_code==13)                    .or. &
             (.not.write_metar .and. (fm_code==15  .or. fm_code==16)) .or. &
             (.not.write_buoy  .and. (fm_code==18  .or. fm_code==19)) .or. &
             (.not.write_pilot .and. (fm_code>=32 .and. fm_code<=34)) .or. &
             (.not.write_sound .and. (fm_code>=35 .and. fm_code<=38)) .or. &
             (.not.write_amdar .and.  fm_code==42)                    .or. &
             (.not.write_satem .and.  fm_code==86)                    .or. &
             (.not.write_satob .and.  fm_code==88)                    .or. &
             (.not.write_airep .and. (fm_code==96  .or. fm_code==97)) .or. &
             (.not.write_tamdar.and.  fm_code==101)                   .or. &
             (.not.write_gpspw .and.  fm_code==111)                   .or. &
             (.not.write_gpsztd.and.  fm_code==114)                   .or. &
             (.not.write_gpsref.and.  fm_code==116)                   .or. &
             (.not.write_gpseph.and.  fm_code==118)                   .or. &
             (.not.write_ssmt1 .and.  fm_code==121)                   .or. &
             (.not.write_ssmt2 .and.  fm_code==122)                   .or. &
             (.not.write_ssmi  .and. (fm_code==125 .or. fm_code==126)).or. &
             (.not.write_tovs  .and.  fm_code==131)                   .or. &
             (.not.write_qscat .and.  fm_code==281)                   .or. &
             (.not.write_profl .and.  fm_code==132)                   .or. &
             (.not.write_bogus .and.  fm_code==135)                   .or. &
             (.not.write_airs  .and.  fm_code==133)                   )    &
             dis1 = .true.



         CALL inside_window (obs(ii)%valid_time%date_char, &
                              time_min, time_max, &
                              dis2)




         obs(ii)%info%discard = dis0 .or. dis1 .or.dis2




      enddo




      if (use_for == '4DVAR' )  then
        CALL sort_obs (obs ,number_of_obs , compare_loc, index )

        CALL check_duplicate_time (obs ,index ,number_of_obs, total_dups_time, &
                                   time_fg, print_duplicate_time)
      endif




      write(SLOT_TITLE,'("OBSERVATIONS FOR OUTPUT SLOT ",I2.2)') ntime
      CALL sort_platform (max_number_of_obs, obs, number_of_obs, &
                          nsynops (icor), nshipss (icor), nmetars (icor), &
                          npilots (icor), nsounds (icor), nsatems (icor), &
                          nsatobs (icor), naireps (icor), ngpspws (icor), &
                          ngpsztd (icor), ngpsref (icor), ngpseph (icor), &
                          nssmt1s (icor), nssmt2s (icor), nssmis  (icor), &
                          ntovss  (icor), nothers (icor), namdars (icor), &
                          nqscats (icor), nprofls (icor), nbuoyss (icor), &
                          nboguss (icor), nairss  (icor), ntamdar (icor), &
                          SLOT_TITLE)




      IF (output_ob_format .eq. 1 .or. output_ob_format .eq. 3) THEN



      CALL output_prep (max_number_of_obs, obs, number_of_obs, index, &
                          prepbufr_table_filename, &
                          prepbufr_output_filename, &
                          nsynops (icor), nshipss (icor), nmetars (icor), &
                          npilots (icor), nsounds (icor), nsatems (icor), &
                          nsatobs (icor), naireps (icor), ngpspws (icor), &
                          ngpsztd (icor), ngpsref (icor), ngpseph (icor), &
                          nssmt1s (icor), nssmt2s (icor), nssmis  (icor), &
                          ntovss  (icor), nothers (icor), namdars (icor), &
                          nqscats (icor), nprofls (icor), nbuoyss (icor), &
                          nboguss (icor), missing_flag, time_analysis)


      ENDIF

      IF (output_ob_format .eq. 2 .or. output_ob_format .eq. 3) THEN




      CALL output_gts_31 (max_number_of_obs, obs, number_of_obs, index, &
                          nsynops (icor), nshipss (icor), nmetars (icor), &
                          npilots (icor), nsounds (icor), nsatems (icor), &
                          nsatobs (icor), naireps (icor), ngpspws (icor), &
                          ngpsztd (icor), ngpsref (icor), ngpseph (icor), &
                          nssmt1s (icor), nssmt2s (icor), nssmis  (icor), &
                          ntovss  (icor), nothers (icor), namdars (icor), &
                          nqscats (icor), nprofls (icor), nbuoyss (icor), &
                          nboguss (icor), nairss  (icor), ntamdar(icor), missing_flag, time_fg)

      CALL output_ssmi_31 (max_number_of_obs, obs, number_of_obs, index, &
                           nssmis  (icor), &
                           missing_flag, time_fg)

      ENDIF


      enddo



      ENDIF
      STOP "99999"

END PROGRAM main_obsproc
