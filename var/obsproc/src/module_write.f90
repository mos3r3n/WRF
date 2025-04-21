MODULE module_write






















  USE module_type
  USE module_func
  USE module_date
  USE module_decoded
  USE module_map

  INCLUDE 'constants.inc'
  INCLUDE '../../../inc/version_decl'

  REAL (kind=8), parameter             :: bufrlib_missing = 10.D10

CONTAINS


SUBROUTINE output_ssmi_31 (max_number_of_obs, obs, number_of_obs, index, &
                           nssmis, missing_r, time_analysis)









  IMPLICIT NONE

  INTEGER,                                      INTENT (in) :: max_number_of_obs
  TYPE (report), DIMENSION (max_number_of_obs), INTENT (inout) :: obs
  INTEGER,                                      INTENT (in) :: number_of_obs
  INTEGER,       DIMENSION (max_number_of_obs), INTENT (in) :: index
  REAL,                                         INTENT (in) :: missing_r
  CHARACTER (LEN =  19),                        INTENT (in) :: time_analysis
  INTEGER,                                      INTENT (in) :: nssmis

  INTEGER                       :: n, loop_index, fm, i, ssmi_125, ssmi_126
  INTEGER                       :: nwrites1, nwrites2
  LOGICAL                       :: connected
  CHARACTER (LEN = 80)          :: filename1, filename2
  CHARACTER (LEN = 120)         :: fmt_info, &
                                   fmt_srfc, &
                                   fmt_each
  REAL                          :: rew_cross, rns_cross


  rew_cross=real(nestjx(idd)-1)
  rns_cross=real(nestix(idd)-1)

      if (nssmis == 0) then
        WRITE(0,'(A,/)') "No SSMI observations available."
        RETURN
      else
        ssmi_125 = 0
        ssmi_126 = 0
counts: &
        DO n = 1, number_of_obs
        loop_index = index (n)
        IF (obs (loop_index)%info%discard ) THEN
          CYCLE  counts
        ELSE 
          READ (obs (loop_index) % info % platform (4:6), '(I3)') fm
          if (fm == 125) THEN
            ssmi_125 = ssmi_125 + 1
          else if (fm == 126) THEN
            ssmi_126 = ssmi_126 + 1
          endif
        ENDIF

        ENDDO counts
      endif





      WRITE (0,'(A)')  &
'------------------------------------------------------------------------------'






     fmt_info = '(a12,1x,a19,1x,a40,1x,i6,3(f12.3,11x),6x,a40)'
     fmt_srfc = '(7(:,f12.3,i4,f7.2))'
     fmt_each = '(3(f12.3,i4,f7.2),11x,3(f12.3,i4,f7.2),11x,1(f12.3,i4,f7.2))'








      if (ssmi_125 > 0) then

        write(filename1,'("obs_ssmi_retrieval_",a,".",a)') time_analysis, use_for

        WRITE (0,'(A,A)') &
        "Write 3DVAR SSMI Retrieval obs in files: ", TRIM (filename1)

        INQUIRE (UNIT = 125, OPENED = connected )

        IF ( .NOT. connected ) THEN
          OPEN ( UNIT   = 125,          &
                 FILE   = filename1,     &
                 FORM   = 'FORMATTED',  &
                 ACCESS = 'SEQUENTIAL', &
                 STATUS = 'REPLACE')
        ENDIF

        REWIND ( UNIT = 125)







        WRITE  (UNIT = 125, FMT = '(A,I7,A)',ADVANCE='no' ) "SSMI  =",ssmi_125,","
        WRITE  (UNIT = 125, FMT = '((A,F8.0),A)') " MISS. = ",missing_r,","




        WRITE (UNIT = 125, FMT = '(A,F7.2,A,F7.2,4(A,F7.2),A)') &
        "PHIC  =", phic,", XLONC =", xlonc,", TRUE1 =", truelat1,&
        ", TRUE2 =",truelat2,", XIM11 =",xim11(idd),", XJM11 =",xjm11(idd),","
        WRITE (UNIT = 125, FMT = '(2(A,F7.2),4(A,F7.0),A)') &
        "base_temp=",  ts0, ", base_lapse=", tlp, &
        ", PTOP  =",  ptop,", base_pres=",  ps0, &
        ", base_tropo_pres=", pis0, ", base_strat_temp=", tis0, ","  




        WRITE (UNIT = 125, FMT = '(5(A,I7),A)' ) &
        "IXC   =", ixc,", JXC   =", jxc,", IPROJ =", iproj,&
        ", IDD   =", idd,", MAXNES=",maxnes,","
        WRITE (UNIT = 125, FMT = '(A,10(:,I7,A))')  &
        "NESTIX=",(nestix (i),", ", i = 1, maxnes)
        WRITE (UNIT = 125, FMT = '(A,10(:,I7,A))')  &
        "NESTJX=",(nestjx (i),", ", i = 1, maxnes)
        WRITE (UNIT = 125, FMT = '(A,10(:,I7,A))')  &
        "NUMC  =",(numc   (i),", ", i = 1, maxnes)
        WRITE (UNIT = 125, FMT = '(A,10(:,F7.2,A))')&
        "DIS   =",(dis    (i),", ",i = 1, maxnes)
        WRITE (UNIT = 125, FMT = '(A,10(:,I7,A))')  &
        "NESTI =",(nesti  (i),", ", i = 1, maxnes)
        WRITE (UNIT = 125, FMT = '(A,10(:,I7,A))')  &
        "NESTJ =",(nestj  (i),", ", i = 1, maxnes)
 



        WRITE (UNIT = 125, FMT = '(A)' ) &
        "INFO  = PLATFORM, DATE, NAME, LEVELS, LATITUDE, LONGITUDE, ELEVATION, ID."
        WRITE (UNIT = 125, FMT = '(A)' ) &
        "SRFC  = WIND SPEED, PW (DATA,QC,ERROR)."




       WRITE (UNIT = 125, FMT = '(2A)' ) 'INFO_FMT = ', TRIM (fmt_info)
       WRITE (UNIT = 125, FMT = '(2A)' ) 'SRFC_FMT = ', TRIM (fmt_srfc)




        WRITE (UNIT = 125, FMT = '(A)') &
"#------------------------------------------------------------------------------#"

      endif




      if (ssmi_126 > 0) then

        write(filename2,'("obs_ssmi_Tb_",a,".",a)') time_analysis, use_for

        WRITE (0,'(A,A)') &
        "Write 3DVAR SSMI TB obs in files: ", TRIM (filename2)

        INQUIRE (UNIT = 126, OPENED = connected )

        IF ( .NOT. connected ) THEN
          OPEN ( UNIT   = 126,          &
                 FILE   = filename2,     &
                 FORM   = 'FORMATTED',  &
                 ACCESS = 'SEQUENTIAL', &
                 STATUS = 'REPLACE')
        ENDIF

        REWIND ( UNIT = 126)







        WRITE  (UNIT = 126, FMT = '(A,I7,A)',ADVANCE='no' ) "SSMI  =",ssmi_126,","
        WRITE  (UNIT = 126, FMT = '((A,F8.0),A)') " MISS. = ",missing_r,","




        WRITE (UNIT = 126, FMT = '(A,F7.2,A,F7.2,4(A,F7.2),A)') &
        "PHIC  =", phic,", XLONC =", xlonc,", TRUE1 =", truelat1,&
        ", TRUE2 =",truelat2,", XIM11 =",xim11(idd),", XJM11 =",xjm11(idd),","

        WRITE (UNIT = 126, FMT = '(2(A,F7.2),4(A,F7.0),A)') &
        "base_temp=",  ts0, ", base_lapse=", tlp, &
        ", PTOP  =",  ptop,", base_pres=",  ps0, &
        ", base_tropo_pres=", pis0, ", base_strat_temp=", tis0, ","  




        WRITE (UNIT = 126, FMT = '(5(A,I7),A)' ) &
        "IXC   =", ixc,", JXC   =", jxc,", IPROJ =", iproj,&
        ", IDD   =", idd,", MAXNES=",maxnes,","
        WRITE (UNIT = 126, FMT = '(A,10(:,I7,A))')  &
        "NESTIX=",(nestix (i),", ", i = 1, maxnes)
        WRITE (UNIT = 126, FMT = '(A,10(:,I7,A))')  &
        "NESTJX=",(nestjx (i),", ", i = 1, maxnes)
        WRITE (UNIT = 126, FMT = '(A,10(:,I7,A))')  &
        "NUMC  =",(numc   (i),", ", i = 1, maxnes)
        WRITE (UNIT = 126, FMT = '(A,10(:,F7.2,A))')&
        "DIS   =",(dis    (i),", ",i = 1, maxnes)
        WRITE (UNIT = 126, FMT = '(A,10(:,I7,A))')  &
        "NESTI =",(nesti  (i),", ", i = 1, maxnes)
        WRITE (UNIT = 126, FMT = '(A,10(:,I7,A))')  &
        "NESTJ =",(nestj  (i),", ", i = 1, maxnes)




        WRITE (UNIT = 126, FMT = '(A)' ) &
        "INFO  = PLATFORM, DATE, NAME, LEVELS, LATITUDE, LONGITUDE, ELEVATION, ID."
        WRITE (UNIT = 126, FMT = '(A)' ) &
        "SRFC  = TB19V, TB19H, TB22V, TB37V, TB37H, TB85V, TB85H (DATA,QC,ERROR)."




        WRITE (UNIT = 126, FMT = '(2A)' ) 'INFO_FMT = ', TRIM (fmt_info)
        WRITE (UNIT = 126, FMT = '(2A)' ) 'SRFC_FMT = ', TRIM (fmt_srfc)




        WRITE (UNIT = 126, FMT = '(A)') &
"#------------------------------------------------------------------------------#"

      endif




      nwrites1 = 0
      nwrites2 = 0





stations: &
       DO n = 1, number_of_obs




      loop_index = index (n)




stations_valid: &
      IF (obs (loop_index)%info%discard ) THEN

          CYCLE  stations

      ELSE stations_valid

      READ (obs (loop_index) % info % platform (4:6), '(I3)') fm

      if ((fm /= 125) .AND. (fm /= 126))  CYCLE stations




      IF (fm == 125) THEN

      WRITE (UNIT = 125, FMT = TRIM (fmt_info))         &
             obs (loop_index) % info     % platform,    &
             obs (loop_index) % valid_time % date_mm5,  &
             obs (loop_index) % location % name,        &
             obs (loop_index) % info % levels,          &
             obs (loop_index) % location % latitude,    &
             obs (loop_index) % location % longitude,   &
             obs (loop_index) % info     % elevation,   &
             obs (loop_index) % location % id

             nwrites1 = nwrites1 + 1

      ELSE IF (fm == 126) THEN

      WRITE (UNIT = 126, FMT = TRIM (fmt_info))          &
             obs (loop_index) % info     % platform,    &
             obs (loop_index) % valid_time % date_mm5,  &
             obs (loop_index) % location % name,        &
             obs (loop_index) % info % levels,          &
             obs (loop_index) % location % latitude,    &
             obs (loop_index) % location % longitude,   &
             obs (loop_index) % info     % elevation,   &
             obs (loop_index) % location % id

             nwrites2 = nwrites2 + 1

      ENDIF




      IF (fm == 125) THEN

      IF (ASSOCIATED (obs (loop_index) % surface)) THEN

         if( domain_check_h .and. &
           (obs (loop_index) % location % xjc < 1.0 .or. &
            obs (loop_index) % location % xjc > rew_cross .or. &
            obs (loop_index) % location % yic < 1.0 .or. &
            obs (loop_index) % location % yic > rns_cross) ) then
            obs (loop_index) % ground  % pw   % qc = -88
         end if

         WRITE (UNIT = 125, FMT = TRIM (fmt_srfc))                  &
                obs (loop_index) % surface % meas % speed % data,  &
                obs (loop_index) % surface % meas % speed % qc,    &
                obs (loop_index) % surface % meas % speed % error, &
                obs (loop_index) % ground  % pw   % data, &
                obs (loop_index) % ground  % pw   % qc,   &
                obs (loop_index) % ground  % pw   % error

      ELSE

         if( domain_check_h .and. &
           (obs (loop_index) % location % xjc < 1.0 .or. &
            obs (loop_index) % location % xjc > rew_cross .or. &
            obs (loop_index) % location % yic < 1.0 .or. &
            obs (loop_index) % location % yic > rns_cross) ) then
            obs (loop_index) % ground  % pw   % qc = -88
         end if

         WRITE (UNIT = 125, FMT = TRIM (fmt_srfc))         &
                missing_r, CEILING (missing_r/10000), 2.5, &
                obs (loop_index) % ground % pw  % data,    &
                obs (loop_index) % ground % pw  % qc,      &
                obs (loop_index) % ground % pw  % error

      ENDIF

                nwrites1 = nwrites1 + 1

      ELSE IF (fm == 126) THEN

         if( domain_check_h .and. &
           (obs (loop_index) % location % xjc < 1.0 .or. &
            obs (loop_index) % location % xjc > rew_cross .or. &
            obs (loop_index) % location % yic < 1.0 .or. &
            obs (loop_index) % location % yic > rns_cross) ) then
            obs (loop_index) % ground % tb19v % qc = -88
            obs (loop_index) % ground % tb19h % qc = -88
            obs (loop_index) % ground % tb22v % qc = -88
            obs (loop_index) % ground % tb37v % qc = -88
            obs (loop_index) % ground % tb37h % qc = -88
            obs (loop_index) % ground % tb85v % qc = -88
            obs (loop_index) % ground % tb85h % qc = -88
         end if

         WRITE (UNIT = 126, FMT = TRIM (fmt_srfc))         &
         obs (loop_index) % ground % tb19v % data,  &
         obs (loop_index) % ground % tb19v % qc,    &
         obs (loop_index) % ground % tb19v % error, &
         obs (loop_index) % ground % tb19h % data,  &
         obs (loop_index) % ground % tb19h % qc,    &
         obs (loop_index) % ground % tb19h % error, &
         obs (loop_index) % ground % tb22v % data,  &
         obs (loop_index) % ground % tb22v % qc,    &
         obs (loop_index) % ground % tb22v % error, &
         obs (loop_index) % ground % tb37v % data,  &
         obs (loop_index) % ground % tb37v % qc,    &
         obs (loop_index) % ground % tb37v % error, &
         obs (loop_index) % ground % tb37h % data,  &
         obs (loop_index) % ground % tb37h % qc,    &
         obs (loop_index) % ground % tb37h % error, &
         obs (loop_index) % ground % tb85v % data,  &
         obs (loop_index) % ground % tb85v % qc,    &
         obs (loop_index) % ground % tb85v % error, &
         obs (loop_index) % ground % tb85h % data,  &
         obs (loop_index) % ground % tb85h % qc,    &
         obs (loop_index) % ground % tb85h % error

         nwrites2 = nwrites2 + 1

      ENDIF




      ENDIF stations_valid




      ENDDO stations





      CLOSE (UNIT = 125) 
      CLOSE (UNIT = 126) 




 
      WRITE (0, '(A)') ' ' 

      if (ssmi_125 > 0) WRITE (0, '(A,I7,A,A)') &
      'Wrote ',nwrites1,' lines of data in file: ',TRIM (filename1) 

      if (ssmi_126 > 0) WRITE (0, '(A,I7,A,A)') &
      'Wrote ',nwrites2,' lines of data in file: ',TRIM (filename2) 

      WRITE (0, '(A)') ' ' 

      RETURN

END SUBROUTINE output_ssmi_31

SUBROUTINE output_gts_31 (max_number_of_obs, obs, number_of_obs, windex,&
                         nsynops, nshipss, nmetars,                    &
                         npilots, nsounds, nsatems,                    &
                         nsatobs, naireps, ngpspws, ngpsztd, ngpsref,  &
                         ngpseph, nssmt1s, nssmt2s, nssmis,  ntovss,   &
                         nothers, namdars, nqscats, nprofls,nbuoyss,   &
                         nboguss, nairss, ntamdar, missing_r, time_analysis)









  IMPLICIT NONE

  INTEGER,                                      INTENT (in) :: max_number_of_obs
  TYPE (report), DIMENSION (max_number_of_obs), INTENT (inout) :: obs
  INTEGER,                                      INTENT (in) :: number_of_obs
  INTEGER,       DIMENSION (max_number_of_obs), INTENT (in) :: windex
  REAL,                                         INTENT (in) :: missing_r
  CHARACTER (LEN =  19),                        INTENT (in) :: time_analysis
  INTEGER,                                      INTENT (in) :: nsynops,nmetars,&
                                                               nshipss,nsounds,&
                                                               npilots,naireps,&
                                                               nsatems,nsatobs,&
                                                               ngpspws,nssmt1s,&
                                                               nssmt2s, nssmis,&
                                                               ntovss, namdars,&
                                                               nqscats,nothers,&
                                                               nprofls,nbuoyss,&
                                                               ngpsztd,ngpsref,&
                                                               ngpseph,nboguss,&
                                                               nairss,ntamdar

  TYPE (measurement ) , POINTER :: current
  INTEGER                       :: loop_index
  INTEGER                       :: i, ii, n, ntotal, k_levels
  INTEGER                       :: nmultis, nsingles, nlevels, nwrites
  INTEGER                       :: is_sound, fm
  LOGICAL                       :: connected
  CHARACTER (LEN = 80)          :: filename
  CHARACTER (LEN = 120)         :: fmt_info, &
                                   fmt_srfc,  &
                                   fmt_each
  CHARACTER (LEN=40) :: SID
  INTEGER   ::    kx
  REAL                          :: val_slp, val_pw, pressure_error
  REAL                          :: val_u,  val_v,  val_p, val_t
  REAL                          :: val_td, val_rh, val_qv

  REAL                          :: rew_cross, rns_cross
  LOGICAL                       :: change_qc

  rew_cross=real(nestjx(idd)-1)
  rns_cross=real(nestix(idd)-1)


      ntotal =   nsynops + nmetars + nshipss + &
                 nsounds + npilots + naireps + &
                 nsatems + nsatobs + ngpspws + &
                 nssmt1s + nssmt2s + ntovss  + &
                 namdars + nqscats + nprofls + &
                 nbuoyss + nothers + ngpsztd + &
                 ngpsref + ngpseph + nboguss + nairss + ntamdar





      WRITE (0,'(A)')  &
'------------------------------------------------------------------------------'






     fmt_info = '(A12,1X,A19,1X,A40,1X,I6,3(F12.3,11X),6X,A40)'
     fmt_srfc = '(F12.3,I4,F7.2,F12.3,I4,F7.3)'
     fmt_each = '(3(F12.3,I4,F7.2),11X,3(F12.3,I4,F7.2),11X,3(F12.3,I4,F7.2))'








      write(filename,'("obs_gts_",a,".",a)') time_analysis, use_for



      WRITE (0,'(7A)') 'Write ',use_for,' GTS observations in file ',&
                        TRIM (filename),' (WRFDA ',TRIM(release_version),')'





      INQUIRE ( UNIT = 99, OPENED = connected )

      IF ( .NOT. connected ) THEN
          OPEN ( UNIT   = 99,           &
                 FILE   = filename,     &
                 FORM   = 'FORMATTED',  &
                 ACCESS = 'SEQUENTIAL', &
                 STATUS = 'REPLACE')
      ENDIF

      REWIND ( UNIT = 99)

      if (ntotal == 0) then
         WRITE(0,'(A,I6,A)') "Ntotal=",ntotal, &
                            " No observations other than SSMI is written out."


      endif







      WRITE  (UNIT = 99, FMT = '((A,I7),A)',ADVANCE='no' )    &
      "TOTAL =",nsynops + nmetars + nshipss + &
                nsounds + npilots + naireps + &
                nsatems + nsatobs + ngpspws + &
                nssmt1s + nssmt2s + namdars + &
                ntovss  + nqscats + nprofls + &
                nbuoyss + nothers + ngpsztd + &
                ngpsref + ngpseph + nboguss + nairss + ntamdar,", "




       WRITE  (UNIT = 99, FMT = '((A,F8.0),A)') "MISS. =",missing_r,","





      WRITE  (UNIT = 99, FMT = '(6(A,I7,A))' )    &
      "SYNOP =",nsynops,", ", &
      "METAR =",nmetars,", ", &
      "SHIP  =",nshipss,", ", &
      "BUOY  =",nbuoyss,", ", &
      "BOGUS =",nboguss,", ", &
      "TEMP  =",nsounds,", ", &
      "AMDAR =",namdars,", ", &
      "AIREP =",naireps,", ", &
      "TAMDAR=",ntamdar,", ", &
      "PILOT =",npilots,", ", &
      "SATEM =",nsatems,", ", &
      "SATOB =",nsatobs,", ", &
      "GPSPW =",ngpspws,", ", &
      "GPSZD =",ngpsztd,", ", &
      "GPSRF =",ngpsref,", ", &
      "GPSEP =",ngpseph,", ", &
      "SSMT1 =",nssmt1s,", ", &
      "SSMT2 =",nssmt2s,", ", &

      "TOVS  =",ntovss, ", ", &
      "QSCAT =",nqscats,", ", &
      "PROFL =",nprofls,", ", &
      "AIRSR =",nairss ,", ", &
      "OTHER =",nothers,", "





        WRITE (UNIT = 99, FMT = '(A,F7.2,A,F7.2,4(A,F7.2),A)') &
        "PHIC  =", phic,", XLONC =", xlonc,", TRUE1 =", truelat1,&
      ", TRUE2 =",truelat2,", XIM11 =",xim11(idd),", XJM11 =",xjm11(idd),","

        WRITE (UNIT = 99, FMT = '(2(A,F7.2),4(A,F7.0),A)') &
        "base_temp=",  ts0, ", base_lapse=", tlp, &
        ", PTOP  =",  ptop,", base_pres=",  ps0, &
        ", base_tropo_pres=", pis0, ", base_strat_temp=", tis0, ","  




        WRITE (UNIT = 99, FMT = '(5(A,I7),A)' ) &
        "IXC   =", ixc,", JXC   =", jxc,", IPROJ =", iproj,&
        ", IDD   =", idd,", MAXNES=",maxnes,","
        WRITE (UNIT = 99, FMT = '(A,10(:,I7,A))')  &
       "NESTIX=",(nestix (i),", ", i = 1, maxnes)
        WRITE (UNIT = 99, FMT = '(A,10(:,I7,A))')  &
       "NESTJX=",(nestjx (i),", ", i = 1, maxnes)
        WRITE (UNIT = 99, FMT = '(A,10(:,I7,A))')  &
       "NUMC  =",(numc   (i),", ", i = 1, maxnes)
        WRITE (UNIT = 99, FMT = '(A,10(:,F7.2,A))')&
       "DIS   =",(dis    (i),", ",i = 1, maxnes)
        WRITE (UNIT = 99, FMT = '(A,10(:,I7,A))')  &
       "NESTI =",(nesti  (i),", ", i = 1, maxnes)
        WRITE (UNIT = 99, FMT = '(A,10(:,I7,A))')  &
       "NESTJ =",(nestj  (i),", ", i = 1, maxnes)






       WRITE (UNIT = 99, FMT = '(A)' ) &
      "INFO  = PLATFORM, DATE, NAME, LEVELS, LATITUDE, LONGITUDE, ELEVATION, ID."
       WRITE (UNIT = 99, FMT = '(A)' ) &
      "SRFC  = SLP, PW (DATA,QC,ERROR)."
       WRITE (UNIT = 99, FMT = '(A)' ) &
      "EACH  = PRES, SPEED, DIR, HEIGHT, TEMP, DEW PT, HUMID (DATA,QC,ERROR)*LEVELS."




      WRITE (UNIT = 99, FMT = '(2A)' ) 'INFO_FMT = ', TRIM (fmt_info)
      WRITE (UNIT = 99, FMT = '(2A)' ) 'SRFC_FMT = ', TRIM (fmt_srfc)
      WRITE (UNIT = 99, FMT = '(2A)' ) 'EACH_FMT = ', TRIM (fmt_each)





        WRITE (UNIT = 99, FMT = '(A)') &
"#------------------------------------------------------------------------------#"





      nmultis  = 0
      nsingles = 0
      nlevels  = 0
      nwrites  = 0





stations: &
       DO n = 1, number_of_obs




      loop_index = windex (n)




stations_valid: &
      IF (obs (loop_index)%info%discard ) THEN

          CYCLE  stations

      ELSE stations_valid

      READ (obs (loop_index) % info % platform (4:6), '(I3)') fm

      if ((fm == 125) .OR. (fm == 126))  CYCLE stations



      if (fm == 86) then
           obs (loop_index) % ground % slp % data =  &
              obs (loop_index) % ground % ref_pres % data
           obs (loop_index) % ground % slp % qc =  &
              obs (loop_index) % ground % ref_pres % qc
           obs (loop_index) % ground % pw % data =  &
              obs (loop_index) % ground % cloud_cvr % data


      endif






      SELECT CASE (fm)
      CASE ( 86, 87, 88 )
        sid = obs (loop_index) % location % id
        kx = index(obs (loop_index) % location % id,'GOES')
        if (kx .eq. 0) then
          kx = index(obs (loop_index) % location % id,'MET')
          if (kx .ne. 0) then
            sid = 'MET' // sid(kx+3:40)
          else
            kx = index(obs (loop_index) % location % id,'MODIS')
            if (kx .ne. 0) sid = 'MODIS'
          endif
        else
          sid = 'G-' // sid(kx+5:40)
        endif
      CASE ( 141 )
        sid = 'MODIS'
      CASE ( 281 )
        sid = 'QSCAT'
      CASE ( 13, 18, 19 )
         kx = index(obs (loop_index) % location % name,' >>>')
         sid = obs (loop_index) % location % name
         if (kx .ne. 0) then
           sid = sid(kx+5:kx+9)
         else
           if (fm .eq. 13) then
             sid = 'SHIP'
           else if (fm .eq. 18) then
             sid = 'BUOY'
           else if (fm .eq. 19) then
             sid = 'C-MAN'
           else
             sid = obs (loop_index) % location % id
           endif
         endif
      CASE ( 15, 16 )
        kx = index(obs (loop_index) % location % name,'ICAO ')
        sid = obs (loop_index) % location % name
        if (kx .ne. 0) then
          sid = sid(kx+5:kx+9)
        else
          sid = obs (loop_index) % location % id
        endif
      CASE ( 133 )
        sid = 'AIRSRET'
      CASE DEFAULT
        sid = obs (loop_index) % location % id
      END SELECT





      k_levels = obs (loop_index) % info % levels 
      if ( (fm == 18 .or. fm == 19 ) .and. &
           obs (loop_index) % info   % elevation == 0.0 ) then
           if ( obs (loop_index) % info % levels == 0 ) &
                obs (loop_index) % info % levels = 1
      endif


      WRITE (UNIT = 99, FMT = TRIM (fmt_info))          &
             obs (loop_index) % info     % platform,    &
             obs (loop_index) % valid_time % date_mm5,  &
             obs (loop_index) % location % name,        &
             obs (loop_index) % info % levels,          &
             obs (loop_index) % location % latitude,    &
             obs (loop_index) % location % longitude,   &
             obs (loop_index) % info     % elevation,   &
             sid





         change_qc = .false.

         if( domain_check_h .and. &
           (obs (loop_index) % location % xjc < 1.0 .or. &
            obs (loop_index) % location % xjc > rew_cross .or. &
            obs (loop_index) % location % yic < 1.0 .or. &
            obs (loop_index) % location % yic > rns_cross) ) then
            obs (loop_index) % ground % slp % qc = -88
            obs (loop_index) % ground % pw  % qc = -88
            change_qc = .true.
         end if

      WRITE (UNIT = 99, FMT = TRIM (fmt_srfc))          &
             obs (loop_index) % ground % slp % data,    &
             obs (loop_index) % ground % slp % qc,      &
             obs (loop_index) % ground % slp % error,   &
             obs (loop_index) % ground % pw  % data,    &
             obs (loop_index) % ground % pw  % qc,      &
             obs (loop_index) % ground % pw  % error

             nwrites = nwrites + 1




      current => obs (loop_index) % surface





















      if ( (fm == 18 .or. fm == 19 ) .and. &
           obs (loop_index) % info   % elevation == 0.0 ) then

           pressure_error = 100. 

           if ( k_levels == 0 ) then

             WRITE (UNIT = 99, FMT = TRIM (fmt_each))    &
               obs (loop_index) % ground % slp % data,    &
               obs (loop_index) % ground % slp % qc,      &
               pressure_error, &
               missing_r, -88, 1.40,  &  
               missing_r, -88, 5.00,  &  
               obs (loop_index) % info   % elevation, 0, 6.0, &
               missing_r, -88,  2.0,  &  
               missing_r, -88,  2.0,  &  
               missing_r, -88, 10.0      
               CYCLE  stations
           endif
      endif





      is_sound = -1

levels:&
      DO WHILE (ASSOCIATED (current))

      nlevels  = nlevels  + 1
      is_sound = is_sound + 1



      if (fm == 86) &
         current % meas % temperature = current % meas % thickness




      if(change_qc) then
         current % meas % temperature % qc = -88
         current % meas % dew_point   % qc = -88
         current % meas % rh          % qc = -88

         if(current % meas % height   % qc >= 0) then
            current % meas % pressure % qc = -88
         end if
      end if





      if (current % meas % pressure % qc < 0) &
          current % meas % pressure % data = missing_r
      if (current % meas % height   % qc < 0) &
          current % meas % height   % data = missing_r


          if (fm == 118 .or. fm == 116) then

      WRITE (UNIT = 99, FMT = TRIM (fmt_each))    &
             current % meas % pressure    % data, &    
             current % meas % pressure    % qc,   & 
             current % meas % pressure    % error,&
             current % meas % u       % data, &        
             current % meas % u       % qc,   & 
             current % meas % u       % error,&
             current % meas % v   % data, &            
             current % meas % v   % qc,   & 
             current % meas % v   % error,&
             current % meas % height      % data, &    
             current % meas % height      % qc,   &
             current % meas % height      % error,&
             current % meas % temperature % data, &    
             current % meas % temperature % qc,   &
             current % meas % temperature % error,&
             current % meas % dew_point   % data, &    
             current % meas % dew_point   % qc,   & 
             current % meas % dew_point   % error,&
             current % meas % direction   % data, &    
             current % meas % direction   % qc,   & 
             current % meas % direction   % error,&
             current % meas % speed       % data, &    
             current % meas % speed       % qc,   & 
             current % meas % speed       % error,&
             current % meas % rh          % data, &    
             current % meas % rh          % qc,   & 
             current % meas % rh          % error
          else

      WRITE (UNIT = 99, FMT = TRIM (fmt_each))    &
             current % meas % pressure    % data, &
             current % meas % pressure    % qc,   & 
             current % meas % pressure    % error,&
             current % meas % speed       % data, & 
             current % meas % speed       % qc,   & 
             current % meas % speed       % error,&
             current % meas % direction   % data, &
             current % meas % direction   % qc,   & 
             current % meas % direction   % error,&
             current % meas % height      % data, &
             current % meas % height      % qc,   &
             current % meas % height      % error,&
             current % meas % temperature % data, &
             current % meas % temperature % qc,   &
             current % meas % temperature % error,&
             current % meas % dew_point   % data, &
             current % meas % dew_point   % qc,   & 
             current % meas % dew_point   % error,&
             current % meas % rh          % data, &
             current % meas % rh          % qc,   & 
             current % meas % rh          % error
          endif

             nwrites = nwrites + 1








      current => current%next

      ENDDO levels




      if (is_sound .gt. 0) then
          nmultis = nmultis + 1
      else 
          nsingles  = nsingles + 1
      endif




      ENDIF stations_valid




      ENDDO stations





      CLOSE (UNIT = 99) 




 
      WRITE (0, '(/,A,I8,A,A)') &
     'Wrote ',nwrites,' lines of data in file: ',TRIM (filename) 
      WRITE (0, '(A)') ' ' 


   RETURN

END SUBROUTINE output_gts_31


SUBROUTINE output_prep (max_number_of_obs, obs, number_of_obs, windex,&
                         prepbufr_table_filename, &
                         prepbufr_output_filename, &
                         nsynops, nshipss, nmetars,                    &
                         npilots, nsounds, nsatems,                    &
                         nsatobs, naireps, ngpspws, ngpsztd, ngpsref,  &
                         ngpseph, nssmt1s, nssmt2s, nssmis,  ntovss,   &
                         nothers, namdars, nqscats, nprofls,nbuoyss,   &
                         nboguss, missing_r, time_analysis)








  IMPLICIT NONE


  INTEGER,                                      INTENT (in) :: max_number_of_obs
  TYPE (report), DIMENSION (max_number_of_obs), INTENT (inout) :: obs
  INTEGER,                                      INTENT (in) :: number_of_obs
  INTEGER,       DIMENSION (max_number_of_obs), INTENT (in) :: windex
  REAL,                                         INTENT (in) :: missing_r
  CHARACTER (LEN =  19),                        INTENT (in) :: time_analysis
  CHARACTER (LEN =  80),                        INTENT(in)  :: prepbufr_table_filename, &
                                                               prepbufr_output_filename
  INTEGER,                                      INTENT (in) :: nsynops,nmetars,&
                                                               nshipss,nsounds,&
                                                               npilots,naireps,&
                                                               nsatems,nsatobs,&
                                                               ngpspws,nssmt1s,&
                                                               nssmt2s, nssmis,&
                                                               ntovss, namdars,&
                                                               nqscats,nothers,&
                                                               nprofls,nbuoyss,&
                                                               ngpsztd,ngpsref,&
                                                               ngpseph,nboguss

  TYPE (measurement ) , POINTER :: current
  INTEGER                       :: bfout, bftable, loop_index
  INTEGER                       :: i, n, nlv, nmax, ntotal
  INTEGER                       :: nmultis, nsingles, nlevels, nwrites
  INTEGER                       :: is_sound, fm, idate
  INTEGER                       :: year, month, day, hour, minute, second
  INTEGER                       :: mxmn, kx
  LOGICAL                       :: connected, vld
  CHARACTER (LEN = 6)           :: cfm(300)
  REAL(kind=8), allocatable, dimension(:,:) :: r8arr
  REAL                          :: mixing_ratio, temperat
  REAL                          :: press
  REAL(kind=8)                  :: wqm, virtual_temperature, &
                                   specific_humidity

  CHARACTER (LEN=40) :: SID
  REAL, parameter  :: PI=3.1415926535











      bfout = 11
      bftable = 10



 
      cfm = 'UNKNOW'
      cfm(12)    = 'ADPSFC'
      cfm(13)    = 'SFCSHP'
      cfm(14:16) = 'ADPSFC'
      cfm(18:19) = 'SFCSHP'
      cfm(32:38) = 'ADPUPA'
      cfm(42)    = 'AIRCFT'
      cfm(86:87) = 'GOESND'
      cfm(88)    = 'SATWND'
      cfm(96:97) = 'AIRCFT'
      cfm(111)   = 'GPSIPW'
      cfm(116)   = 'GPSREF'
      cfm(125)   = 'SPSSMI'
      cfm(132)   = 'PROFLR'
      cfm(135)   = 'SYNDAT'
      cfm(141)   = 'SATWND'     
      cfm(281)   = 'QKSWND'
      
     
      ntotal =   nsynops + nmetars + nshipss + &
                 nsounds + npilots + naireps + &
                 nsatems + nsatobs + ngpspws + &
                 nssmis  + &
                 nssmt1s + nssmt2s + ntovss  + &
                 namdars + nqscats + nprofls + &
                 nbuoyss + nothers + ngpsztd + &
                 ngpsref + nboguss 

      write(0,*) 'ntotal = ',ntotal
      if (ntotal == 0) then
         WRITE(0,'(A,I6,A)') "Ntotal=",ntotal, &
                            " No prepbufr observations written out."
         RETURN
      endif




      nmax = max(nsynops + nmetars, nshipss + nbuoyss, &
                 nsounds, npilots, naireps + namdars, nprofls) 

      mxmn = 58
      ALLOCATE (r8arr(mxmn, 255))




      INQUIRE (UNIT=bftable, OPENED= connected )
      IF (connected) THEN
         WRITE(0,'(A, i4, A)') "A file is already connected to unit", bftable, &
                        "which is needed for the PREPBUFR tables"
         STOP
      ENDIF

      OPEN (UNIT = bftable, FILE = prepbufr_table_filename) 




      OPEN (UNIT = bfout, FILE = prepbufr_output_filename, &
            FORM = "UNFORMATTED") 

      CALL OPENBF (bfout, 'OUT', bftable)

      WRITE (0,'(2A)') 'Write 3DVAR PREPBUFR observations in file ',&
                        TRIM (prepbufr_output_filename)




      nmultis  = 0
      nsingles = 0
      nlevels  = 0
      nwrites  = 0




   print *, 'number of obs=', number_of_obs
stations: &
   DO n = 1, number_of_obs




   loop_index = windex (n)




stations_valid: &
   IF (obs (loop_index)%info%discard ) THEN
      CYCLE stations
   ELSE stations_valid
      READ (obs (loop_index) % info % platform (4:6), '(I3)') fm
      IF (fm == 126 .OR. cfm(fm) == 'UNKNOW') &
          CYCLE stations



      IF (fm == 86) then
           obs (loop_index) % ground % slp % data =  &
              obs (loop_index) % ground % ref_pres % data
           obs (loop_index) % ground % slp % qc =  &
              obs (loop_index) % ground % ref_pres % qc
           obs (loop_index) % ground % pw % data =  &
              obs (loop_index) % ground % cloud_cvr % data


      endif

SUBSET: &
       SELECT CASE (cfm(fm))
       CASE ('ADPSFC', 'SFCSHP', 'AIRCFT', 'ADPUPA', 'AIRCAR', 'PROFLR', 'SATWND', &
             'SYNDAT', 'GPSIPW', 'GPSREF', 'QKSWND', 'SPSSMI')



         CALL split_date_char ( obs (loop_index) % valid_time % date_mm5, &
                                year , month , day , hour , minute , second )

         idate = year * 1000000 + month * 10000 + day * 100 + hour 



         CALL OPENMB (bfout, cfm(fm), idate)



         SELECT CASE (cfm(fm))
         CASE ('SATWND', 'SATEMP','GOESND')
           kx = index(obs (loop_index) % location % id,'GOES')
           if (kx .eq. 0) kx = index(obs (loop_index) % location % id,'MET')
           sid = obs (loop_index) % location % id
           sid = sid(kx:40)
         CASE ('QKSWND')
           sid = 'QUIKSCAT'
         CASE ('SPSSMI')
           sid = 'SSMI'
         CASE ('SFCSHP')
           kx = index(obs (loop_index) % location % name,' >>>')
           sid = obs (loop_index) % location % name
           if (kx .ne. 0) then
             sid = sid(kx+5:kx+9)
           else
             if (fm .eq. 13) then
               sid = 'SHIP'
             else if (fm .eq. 18) then
               sid = 'BUOY'
             else if (fm .eq. 19) then
               sid = 'C-MAN'
             else
               sid = obs (loop_index) % location % id
             endif
           endif
         CASE ('ADPSFC')
           if (fm .eq. 15 .or. fm .eq. 16) then
             kx = index(obs (loop_index) % location % name,'ICAO ')
             sid = obs (loop_index) % location % name
             if (kx .ne. 0) then
               sid = sid(kx+5:kx+9)
             else
               sid = obs (loop_index) % location % id
             endif
           else
             sid = obs (loop_index) % location % id
           endif
         CASE DEFAULT
           sid = obs (loop_index) % location % id
         END SELECT
         r8arr(1,1) = stuff (sid)

         r8arr(2,1) = obs (loop_index) % location % longitude

         IF (r8arr(2,1) < 0.) r8arr(2,1) = 360. + r8arr(2,1)
         r8arr(3,1) = obs (loop_index) % location % latitude 




         r8arr(4,1) = 0



         call assignv( obs (loop_index) % info % elevation, r8arr(5,1) )


         if ( obs (loop_index) % info % seq_num == missing ) then
            r8arr(10,1) = bufrlib_missing
         else
            r8arr(10,1) = obs (loop_index) % info % seq_num
         end if


         r8arr(11,1) = hour + (minute + second / 60.) / 60.


         r8arr(12,1) = 0
         

         r8arr(9,1) = bufrlib_missing


         r8arr(7,1) = t29(fm, obs (loop_index) % location % id)



         r8arr(8,1) = bufrlib_missing

	 current => obs (loop_index) % surface
	 IF (ASSOCIATED (current)) then
	   vld = .true.
	 else
	   vld = .false.
	 endif
	 press = missing_r
	 temperat = missing_r
	 mixing_ratio = missing_r
	 if (vld) then
	   press = obs (loop_index) % surface % meas % pressure % data
	   temperat = obs (loop_index) % surface % meas % temperature % data
	   mixing_ratio = obs (loop_index) % surface % meas % qv % data
         endif


         IF ( .not. eps_equal(mixing_ratio, missing_r, 1.) ) THEN       
           specific_humidity = mixing_ratio / (1 + mixing_ratio)
           IF ( .not. eps_equal(temperat, missing_r, 1.) ) THEN        
             virtual_temperature = temperat * (1 + mixing_ratio/eps) / &    
                                                  (1 + mixing_ratio)
             virtual_temperature = virtual_temperature - celkel       
             r8arr(6,1) = typ (press, cfm(fm), fm, .true., missing_r) 
             specific_humidity = 1e6 * specific_humidity              
           else
             virtual_temperature = bufrlib_missing
             r8arr(6,1) = typ (press, cfm(fm), fm, .false., missing_r) 
           ENDIF
         ELSE                                    
           specific_humidity = bufrlib_missing
           if ( .not. eps_equal(temperat, missing_r, 1.) ) then
             virtual_temperature = temperat - celkel
           else
             virtual_temperature = bufrlib_missing
           endif
           r8arr(6,1) = typ (press, cfm(fm), fm, .false., missing_r) 
         ENDIF


         CALL UFBSEQ (bfout, r8arr, 12, 1, nlv, 'HEADR')



         r8arr(1,1) = 6               
         SELECT CASE (cfm(fm))
         CASE ('ADPSFC', 'SFCSHP', 'AIRCAR', 'AIRCFT', 'SATWND', 'QKSWND')
            if ( .not. eps_equal(temperat, missing_r, 1.)) then
	      if ( eps_equal(obs (loop_index) % surface % meas % u % data, missing_r, 1.) ) &
               r8arr(1,1) = 0  
            endif


         END SELECT

         CALL UFBINT (bfout, r8arr, mxmn, 1, nlv, 'CAT')

         if ( cfm(fm) .ne. 'GPSIPW') then


         call assignv(press, r8arr(1,1))

         IF ( r8arr(1,1) /= bufrlib_missing ) r8arr(1,1) = r8arr(1,1) * .01    

	 r8arr(2,1) = bufrlib_missing
         if (vld) r8arr(2,1) = qz (obs (loop_index) % surface % meas % pressure % qc)   
         r8arr(3,1) = 1                                  
         r8arr(4,1) = 100                                
         CALL UFBINT (bfout, r8arr, mxmn, 1, nlv, 'POB PQM PPC PRC')



	 r8arr(1,1) = bufrlib_missing
         if (vld) r8arr(1,1) = obs (loop_index) % surface % meas % pressure % error * &   
                      0.01
         r8arr(2,1) = bufrlib_missing                                            

         r8arr(3:5,1) = bufrlib_missing            
         CALL UFBINT (bfout, r8arr, mxmn, 1, nlv, &
                   'POE PFC PAN PCL PCS')


         r8arr(1,1) = specific_humidity            
	 r8arr(2,1) = bufrlib_missing
         if (vld) r8arr(2,1) = qz (obs (loop_index) % surface % meas % qv % qc)     
         r8arr(3,1) = 1                            
         r8arr(4,1) = 100                          
         r8arr(5,1) = bufrlib_missing              



         
         r8arr(6,1) = 1.0
         r8arr(7:8,1) = bufrlib_missing


         r8arr(9:11,1) = bufrlib_missing     
         CALL UFBINT (bfout, r8arr, mxmn, 1, nlv, &
                      'QOB QQM QPC QRC TDO QOE QFC QAN PCL PCS')


         r8arr(1,1) = virtual_temperature       
	 r8arr(2,1) = bufrlib_missing
         if (vld) r8arr(2,1) = qz (obs (loop_index) % surface % meas % temperature % qc)    
         r8arr(3,1) = 1                         
         r8arr(4,1) = 100                       
         r8arr(5,1) = bufrlib_missing           


	 r8arr(6,1) = bufrlib_missing
         if (vld) r8arr(6,1) = obs (loop_index) % surface % meas % temperature % error 
         r8arr(7,1) = bufrlib_missing


         r8arr(9:10,1) = bufrlib_missing        
         CALL UFBINT (bfout, r8arr, mxmn, 1, nlv, &
                      'TOB TQM TPC TRC TVO TOE TFC TAN PCL PCS')


	 r8arr(1:2,1) = bufrlib_missing
         if (vld) call assignv( obs (loop_index) % surface % meas % height % data, &
            r8arr(1,1) )                        
	 if (vld) then
         if ( cfm(fm) .eq. 'SFCSHP' .and. &
              eps_equal( obs (loop_index) % surface % meas % height % data, 0., 0.5) ) then
           r8arr(2,1) = 0.          
         else
           r8arr(2,1) = qz (obs (loop_index) % surface % meas % height % qc)     
         endif
         endif
         r8arr(3,1) = 1                         
         r8arr(4,1) = 100                       


	 r8arr(5:6,1) = bufrlib_missing
         if (vld) r8arr(5,1) = obs (loop_index) % surface % meas % height % error * 0.1


         r8arr(7:9,1) = bufrlib_missing         
         CALL UFBINT (bfout, r8arr, mxmn, 1, nlv, &
                      'ZOB ZQM ZPC ZRC ZOE ZFC ZAN PCL PCS')


	 if (vld) then
         if ( eps_equal( obs (loop_index) % surface % meas % speed % data, missing_r, 1.) .or.  &
              eps_equal( obs (loop_index) % surface % meas % direction % data, missing_r, 1.) ) then
           r8arr(1,1) = bufrlib_missing   
           r8arr(5,1) = bufrlib_missing   
         else
           r8arr(1,1) = obs (loop_index) % surface % meas % speed % data * &  
                              (-sin(pi/180.* obs (loop_index) % surface % meas % direction % data))
           r8arr(5,1) = obs (loop_index) % surface % meas % speed % data * &  
                              (-cos(pi/180.* obs (loop_index) % surface % meas % direction % data))
         endif


         r8arr(2,1) = qz (MAX(obs (loop_index) % surface % meas % speed % qc, &    
                          obs (loop_index) % surface % meas % direction % qc))
         wqm = r8arr(2,1)
         r8arr(3,1) = 1                         
         r8arr(4,1) = 100                       


         call assignv( obs (loop_index) % surface % meas % direction % data, &
            r8arr(6,1) )                        

         call assignv( obs (loop_index) % surface % meas % speed % data, &
            r8arr(7,1) )                        
            r8arr(7,1) = r8arr(7,1) * 1.9425    

         r8arr(8,1) = qz (MAX(obs (loop_index) % surface % meas % direction % qc, &
                          obs (loop_index) % surface % meas % speed % qc))        
         r8arr(9,1) = 1                         
         r8arr(10,1) = 100                      
	 endif
         if ( cfm(fm) .ne. 'GPSREF' ) then
           CALL UFBINT (bfout, r8arr, mxmn, 1, nlv, &
                        'UOB WQM WPC WRC VOB DDO FFO DFQ DFP DFR')
         endif

         SELECT CASE (cfm(fm))
         CASE ('ADPSFC', 'ADPUPA', 'AIRCAR', 'AIRCFT', 'SFCSHP', 'SATWND')

            if (vld) then
            r8arr(1,1) = obs (loop_index) % surface % meas % speed % error     
            r8arr(2:3,1) = bufrlib_missing      
            r8arr(4:5,1) = bufrlib_missing      

            r8arr(6:9,1) = bufrlib_missing      
            CALL UFBINT (bfout, r8arr, mxmn, 1, nlv, &
                         'WOE UFC VFC UFC_MOD VFC_MOD UAN VAN PCL PCS')
	    endif
         END SELECT

         SELECT CASE (cfm(fm))
         CASE ('ADPSFC', 'SFCSHP')


            if ( eps_equal( obs (loop_index) % ground % slp % data, &
                                  missing_r, 1.) ) then
              r8arr(1:2,1) = bufrlib_missing      
            else
              r8arr(1,1) = obs (loop_index) % ground % slp % data * 0.01     
              r8arr(2,1) = qz (obs (loop_index) % ground % slp % qc)         
            endif

            CALL UFBINT (bfout, r8arr, 2, 1, nlv, 'PMO PMQ')

            IF (cfm(fm) == 'ADPSFC') THEN
               r8arr(1,1) = bufrlib_missing     
               CALL UFBINT (bfout, r8arr, 1, 1, nlv, 'ALSE')

	       if (vld) then
               IF (obs (loop_index) % surface % meas % direction % data ==  &
                   missing_r) THEN
                  IF ( eps_equal( obs (loop_index) % surface % meas % speed % data, &
                                  missing_r, 1.) ) then
                     r8arr(1,1) =  bufrlib_missing
                  ELSE
                     r8arr(1,1) = obs (loop_index) % surface % meas % speed % data  
                  ENDIF   
               ENDIF   
               r8arr(2,1) = wqm                                      
               CALL UFBINT (bfout, r8arr, 2, 1, nlv, 'SOB SQM')
               endif
            ENDIF
        END SELECT

        else

        SELECT CASE (cfm(fm))
        CASE ('GPSIPW')
          r8arr(1,1) = 6.
          r8arr(2,1) = obs (loop_index) % ground  % pw   % data
          r8arr(3,1) = qz (obs (loop_index) % ground  % pw   % qc)
          r8arr(4,1) = 1
          r8arr(5,1) = 100
          r8arr(6,1) = obs (loop_index) % ground  % pw   % error
          CALL UFBINT (bfout, r8arr, 6, 1, nlv, 'CAT PWO PWQ PWP PWR PWE')
        END SELECT

        endif    

        if (cfm(fm) .eq. 'SPSSMI') then
          r8arr(1:11,1) = bufrlib_missing
          r8arr(1,1) = 6.
          r8arr(2,1) = 10.0*obs (loop_index) % ground  % pw  % data  
          r8arr(3,1) = qz (obs (loop_index) % ground  % pw   % qc)
          r8arr(4,1) = 1
          r8arr(5,1) = 100
          r8arr(6,1) = 10.0*obs (loop_index) % ground  % pw  % error 
	  if (vld) then
            call assignv( obs (loop_index) % surface % meas % speed % data, &
              r8arr(7,1) )                        
            r8arr(7,1) = r8arr(7,1) * 1.9425    

            r8arr(8,1) = qz (MAX(obs (loop_index) % surface % meas % direction % qc, &
                          obs (loop_index) % surface % meas % speed % qc))        
           r8arr(9,1) = 1                         
           r8arr(10,1) = 100                      
           r8arr(11,1) = obs (loop_index) % surface % meas % speed % error     
	  endif
          CALL UFBINT (bfout, r8arr, 11, 1, nlv, 'CAT PWO PWQ PWP PWR PWE FFO DFQ DFP DFR WOE')
        endif

UPA: &
      SELECT CASE (cfm(fm))
      CASE ('ADPUPA', 'PROFLR', 'SYNDAT', 'GPSREF')

      current => obs (loop_index) % surface




      is_sound = -1

      nlevels = 0

levels1:&
      DO WHILE (ASSOCIATED (current))

      nlevels  = nlevels  + 1
      is_sound = is_sound + 1

















      r8arr(1,nlevels) = 1           
      call assignv(current % meas % pressure % data, r8arr(2,nlevels))
      if ( r8arr(2,nlevels) /= bufrlib_missing ) r8arr(2,nlevels) = r8arr(2,nlevels) * .01
      r8arr(3,nlevels) = qz (current % meas % pressure % qc)
      r8arr(4,nlevels) = 1
      r8arr(5,nlevels) = 100
      r8arr(6,nlevels) = current % meas % pressure % error * 0.01
      press = current % meas % pressure % data
      temperat = current % meas % temperature % data
      mixing_ratio = current % meas % qv % data
      if ( .not. eps_equal(mixing_ratio, missing_r, 1.) ) then
        specific_humidity = mixing_ratio / (1 + mixing_ratio)
        if ( .not. eps_equal(temperat, missing_r, 1.) ) then        
          virtual_temperature = temperat * (1 + mixing_ratio/eps) / &     
                                           (1 + mixing_ratio)
          virtual_temperature = virtual_temperature - celkel      
          r8arr(12,nlevels) = virtual_temperature
          specific_humidity = 1e6 * specific_humidity             
        else
          virtual_temperature = bufrlib_missing
        endif
      else                 
        specific_humidity = bufrlib_missing
        if ( .not. eps_equal(temperat, missing_r, 1.) ) then
          virtual_temperature = temperat - celkel
        else
          virtual_temperature = bufrlib_missing
        endif
      endif

      r8arr(7,nlevels) = specific_humidity
      r8arr(12,nlevels) = virtual_temperature
      r8arr(8,nlevels) = qz (current % meas % dew_point % qc)
      r8arr(9,nlevels) = 1
      r8arr(10,nlevels) = 100

      r8arr(11,nlevels) = current % meas % rh % error * 0.1
      r8arr(13,nlevels) = qz (current % meas % temperature % qc)
      r8arr(14,nlevels) = 1
      r8arr(15,nlevels) = 100
      r8arr(16,nlevels) = current % meas % temperature % error 

      current => current%next

      ENDDO levels1

      CALL UFBINT (bfout, r8arr, mxmn, nlevels, nlv, 'CAT POB PQM PPC PRC POE QOB QQM QPC QRC QOE TOB TQM TPC TRC TOE')

      current => obs (loop_index) % surface

      is_sound = -1

      nlevels = 0

      sid = obs (loop_index) % location % id





levels2:&
      DO WHILE (ASSOCIATED (current))

      nlevels  = nlevels  + 1
      is_sound = is_sound + 1











      call assignv(current % meas % height % data, r8arr(1,nlevels))
      if ( r8arr(1,nlevels) /= bufrlib_missing ) r8arr(1,nlevels) = r8arr(1,nlevels) 
      r8arr(2,nlevels) = qz (current % meas % height % qc)
      r8arr(3,nlevels) = 1
      r8arr(4,nlevels) = 100
      r8arr(5,nlevels) = current % meas % height % error 
REF: &
      SELECT CASE (cfm(fm))
      CASE ('GPSREF')
        if ( eps_equal( current % meas % dew_point % data, missing_r, 1.)) then
          r8arr(6,nlevels) = bufrlib_missing
        else
          r8arr(6,nlevels) = current % meas % dew_point % data
        endif
        r8arr(7,nlevels) = qz (current % meas % dew_point % qc)
        r8arr(8,nlevels) = 1
        r8arr(9,nlevels) = 100
        r8arr(10,nlevels) = current % meas % dew_point % error 
        r8arr(11,nlevels) = bufrlib_missing
      CASE DEFAULT
        if ( eps_equal( current % meas % speed % data, missing_r, 1.) .or.  &
             eps_equal( current % meas % direction % data, missing_r, 1.) ) then
          r8arr(6,nlevels) = bufrlib_missing
          r8arr(10,nlevels) = bufrlib_missing
        else
          r8arr(6,nlevels) = current % meas % speed % data * (-sin(pi/180.* current % meas % direction % data))
          r8arr(10,nlevels) = current % meas % speed % data * (-cos(pi/180.* current % meas % direction % data))
        endif
        r8arr(7,nlevels) = qz (MAX(current % meas % direction % qc, current % meas % speed % qc))
        r8arr(8,nlevels) = 1
        r8arr(9,nlevels) = 100
        r8arr(11,nlevels) = current % meas % u % error 

      END SELECT REF

      current => current%next

      ENDDO levels2

      SELECT CASE (cfm(fm))
      CASE ('GPSREF')
        CALL UFBINT (bfout, r8arr, mxmn, nlevels, nlv, 'ZOB ZQM ZPC ZRC ZOE ROB RQM RPC RRC ROE RFC')
      CASE DEFAULT
        CALL UFBINT (bfout, r8arr, mxmn, nlevels, nlv, 'ZOB ZQM ZPC ZRC ZOE UOB WQM WPC WRC VOB WOE')
      END SELECT
      
      END SELECT UPA

      SATWIND: &
      SELECT CASE (cfm(fm))        
      CASE ('SATWND')
        if (index(obs(loop_index) % location % id, 'GOES') .ne. 0) then
          r8arr(1,1) = 252
        else if (index(obs(loop_index) % location % id, 'MET') .ne. 0) then
          r8arr(1,1) = 54
        endif








        CALL UFBINT (bfout, r8arr, 1, 1, nlv, 'SAID')

      CASE ('QKSWND')
        r8arr(1,1) = 281
        CALL UFBINT (bfout, r8arr, 1, 1, nlv, 'SAID')
      END SELECT SATWIND


      CALL WRITSB (bfout)

      CASE DEFAULT   


     END SELECT SUBSET  

      ENDIF stations_valid




      ENDDO stations

      CALL CLOSBF (bfout)


   RETURN

END SUBROUTINE output_prep


   FUNCTION stuff (c)
   REAL (KIND=8) :: stuff








   CHARACTER (LEN=40) c
   CHARACTER (LEN=1) d(8)
   integer (kind=1) :: x(8)
   REAL (KIND=8) :: y
   INTEGER clen, i
   EQUIVALENCE(x, y)

   READ (c, '(8a1)') d
   x = IACHAR(d)
   stuff = y
   RETURN

END FUNCTION stuff


   FUNCTION t29(fm, sid)
   IMPLICIT NONE
   REAL (KIND=8) :: t29
   INTEGER :: fm
   character*40 sid




      SELECT CASE (fm)
      CASE (12, 14)              
         t29 = 511
      CASE (13)             
         IF (TRIM(sid) /= 'SHIP') THEN

            t29 = 522
         ELSE

            t29 = 523
         ENDIF
      CASE (15:16)              
            t29 = 512 
      CASE (18)                 

         IF (TRIM(sid) /= 'DRIB') THEN

            t29 = 561
         ELSE

            t29 = 562
         ENDIF
      CASE (19)               
         t29 = 531
      CASE (32, 35)           
         t29 = 11
      CASE (34, 38)           
         t29 = 13
      CASE (33, 36)           
         t29 = 22
      CASE (37)               
         t29 = 22
      CASE (42, 96:97)        
         t29 = 41
      CASE (86:87)            
         t29 = 61
      CASE (88)               
         t29 = 63
      CASE (111)              
         t29 = 583 
      CASE (116)              
         t29 = 584 
      CASE (125)              
         t29 = 65  
      CASE (132)              
         t29 = 71  
      CASE (141)              
         t29 = 63  
      CASE (281)              
         t29 = 582
      CASE DEFAULT
         t29 = 500            
      END SELECT
END FUNCTION t29


   FUNCTION typ (press, cfmfm, fm, ltemp, missing_r) 
   IMPLICIT NONE
   REAL (KIND=8) :: typ

REAL :: press, missing_r
INTEGER :: fm
CHARACTER*6 :: cfmfm
LOGICAL :: ltemp   




   if ( cfmfm .eq. 'ADPSFC' .or. cfmfm .eq. 'SFCSHP' ) then
   IF ( ltemp ) THEN
      IF ( .not. eps_equal(press, missing_r, 1.) ) THEN

         SELECT CASE(cfmfm)
         CASE ('ADPSFC')
               typ = 181    
         CASE ( 'SFCSHP')
               typ = 180
         END SELECT
      ELSE

         press = bufrlib_missing


         typ = 183
      ENDIF 
   ELSE



      IF (.not. eps_equal(press, missing_r, 1.) ) THEN

         SELECT CASE(cfmfm)
         CASE ('ADPSFC')
            typ = 181
         CASE ('SFCSHP')
            typ = 183
         END SELECT
      ELSE


         typ = 183
         SELECT CASE(cfmfm)
         CASE ('ADPSFC', 'SFCSHP')
            typ = 183
         END SELECT
      ENDIF
   ENDIF
   else
     SELECT CASE (fm)
     CASE (32, 33) 
        typ = 221
     CASE (34, 38) 
        typ = 222
     CASE (35, 36) 
        typ = 120
     CASE (37) 
        typ = 232
     CASE (42) 
        typ = 231
     CASE (86:87)        
        typ = 164
     CASE (88) 
        typ = 146
     CASE (96:97) 
        typ = 230
     CASE (111) 
        typ = 156       
     CASE (116) 
        typ = 160       
     CASE (125) 
        typ = 152       
     CASE (132) 
        typ = 223
     CASE (135)         
        typ = 111
     CASE (141)         
        typ = 146
     CASE (281) 
        typ = 285
     CASE DEFAULT
        write(6,*) 'Cannot determine typ for FM = ',fm
     END SELECT
   endif
END FUNCTION typ


subroutine assignv(x, y)
   real (kind=8) :: y
   real x

   if (eps_equal(x, missing_r, 1.)) then
      y = bufrlib_missing
   else
      y = x
   end if
end subroutine assignv
 

   FUNCTION qz (qc)
   IMPLICIT NONE
   real (kind=8) :: qz
   INTEGER :: qc



if (qc .le. 128 .and. qc .ge. -5 ) then
  qz = 0
else
  qz = 4    
endif

END FUNCTION qz

END MODULE module_write
