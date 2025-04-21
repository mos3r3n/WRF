 SUBROUTINE  FM_DECODER (fm, platform, synop, ship , metar, &
                                       pilot, sound, satem, & 
                                       satob, airep, gpspw, gpszd, &
                                       gpsrf, gpsep, &
                                       ssmt1, ssmt2, ssmi , &
                                       tovs , other, amdar, &
                                       qscat, profl, buoy , bogus, airs, tamdar)

















































    IMPLICIT NONE
    INTEGER,              INTENT (in)    :: fm
    CHARACTER (LEN = 40), INTENT (out)   :: platform
    INTEGER,              INTENT (inout), OPTIONAL :: synop, ship,  metar, &
                                                      pilot, sound, satem, &
                                                      satob, airep, gpspw, &
                                                      gpszd, gpsrf, gpsep, &
                                                      bogus, &
                                                      ssmi,  ssmt1, ssmt2, &
                                                      tovs,  amdar, qscat, &
                                                      profl, buoy, airs, tamdar, other

       SELECT CASE ( fm )

          

          CASE (12) ; platform = 'SYNOP'
                      IF (PRESENT (synop)) synop = synop + 1

          

          CASE (13) ; platform = 'SHIP'
                      IF (PRESENT (ship)) ship  = ship + 1

          

          CASE (14) ; platform = 'SYNOP MOBIL'
                      IF (PRESENT (synop)) synop  = synop + 1

          

          CASE (15) ; platform = 'METAR'
                      IF (PRESENT (metar)) metar  = metar + 1

          
          

          CASE (16) ; platform = 'SPECI'
                      IF (PRESENT (metar)) metar  = metar + 1

          

          CASE (18,19) ; platform = 'BUOY'
                      IF (PRESENT (buoy)) buoy  = buoy + 1

          

          CASE (20) ; platform = 'RADOB'
                      IF (PRESENT (other)) other  = other + 1

          
          

          CASE (22) ; platform = 'RADREP'
                      IF (PRESENT (other)) other  = other + 1

          

          CASE (32) ; platform = 'PILOT'
                      IF (PRESENT (pilot)) pilot = pilot + 1

          

          CASE (33) ; platform = 'PILOT SHIP'
                      IF (PRESENT (pilot)) pilot = pilot + 1

          

          CASE (34) ; platform = 'PILOT MOBIL'
                      IF (PRESENT (pilot)) pilot = pilot + 1

          
          

          CASE (35) ; platform = 'TEMP'
                      IF (PRESENT (sound)) sound = sound + 1

          
          

          CASE (135) ; platform = 'BOGUS'
                      IF (PRESENT (bogus)) bogus = bogus + 1

          
          

          CASE (36) ; platform = 'TEMP SHIP'
                      IF (PRESENT (sound)) sound = sound + 1

          
          

          CASE (37) ; platform = 'TEMP DROP'
                      IF (PRESENT (sound)) sound = sound + 1

          
          

          CASE (38) ; platform = 'TEMP MOBIL'
                      IF (PRESENT (sound)) sound = sound + 1

          
          

          CASE (39) ; platform = 'ROCOB'
                      IF (PRESENT (other)) other = other + 1

          
          

          CASE (40) ; platform = 'ROCOB SHIP'
                      IF (PRESENT (other)) other = other + 1

          
          

          CASE (41) ; platform = 'CODAR'
                      IF (PRESENT (other)) other = other + 1

          

          CASE (42) ; platform = 'AMDAR'
                      IF (PRESENT (amdar)) amdar = amdar + 1

          

          CASE (43) ; platform = 'ICEAN'
                      IF (PRESENT (other)) other = other + 1

          

          CASE (45) ; platform = 'IAC'
                      IF (PRESENT (other)) other = other + 1

          

          CASE (46) ; platform = 'IAC FLEET'
                      IF (PRESENT (other)) other = other + 1

          

          CASE (47) ; platform = 'GRID'
                      IF (PRESENT (other)) other = other + 1

          

          CASE (49) ; platform = 'GRAF'
                      IF (PRESENT (other)) other = other + 1

          

          CASE (50) ; platform = 'WINTEM'
                      IF (PRESENT (other)) other = other + 1

          

          CASE (51) ; platform = 'TAF'
                      IF (PRESENT (other)) other = other + 1

          

          CASE (53) ; platform = 'ARFOR'
                      IF (PRESENT (other)) other = other + 1

          

          CASE (54) ; platform = 'ROFOR'
                      IF (PRESENT (other)) other = other + 1

          
          

          CASE (57) ; platform = 'RADOF'
                      IF (PRESENT (other)) other = other + 1

          

          CASE (61) ; platform = 'MAFOR'
                      IF (PRESENT (other)) other = other + 1



          CASE (62) ; platform = 'TRACKOB'
                      IF (PRESENT (other)) other = other + 1

          

          CASE (63) ; platform = 'BATHY'
                      IF (PRESENT (other)) other = other + 1

          

          CASE (64) ; platform = 'TRESAC'
                      IF (PRESENT (other)) other = other + 1

          
          

          CASE (65) ; platform = 'WAVEOB'
                      IF (PRESENT (other)) other = other + 1

          

          CASE (66) ; platform = 'HYDRA'
                      IF (PRESENT (other)) other = other + 1

          

          CASE (67) ; platform = 'HYFOR'
                      IF (PRESENT (other)) other = other + 1

          

          CASE (71) ; platform = 'CLIMAT'
                      IF (PRESENT (other)) other = other + 1

          

          CASE (72) ; platform = 'CLIMAT SHIP'
                      IF (PRESENT (other)) other = other + 1

          

          CASE (73) ; platform = 'NACLI CLINP SPLCI CLISA INCLI'
                      IF (PRESENT (other)) other = other + 1

          

          CASE (75) ; platform = 'CLIMAT TEMP'
                      IF (PRESENT (other)) other = other + 1

          

          CASE (76) ; platform = 'CLIMAT TEMP SHIP'
                      IF (PRESENT (other)) other = other + 1

          

          CASE (81) ; platform = 'SFAZI'
                      IF (PRESENT (other)) other = other + 1

          
          

          CASE (82) ; platform = 'SFLOC'
                      IF (PRESENT (other)) other = other + 1

          
          

          CASE (83) ; platform = 'SFAZU'
                      IF (PRESENT (other)) other = other + 1

          
          

          CASE (85) ; platform = 'SAREP'
                      IF (PRESENT (other)) other = other + 1

          
          

          CASE (86) ; platform = 'SATEM'
                      IF (PRESENT (satem)) satem = satem + 1

          

          CASE (87) ; platform = 'SARAD'
                      IF (PRESENT (other)) other = other + 1

          
          

          CASE (88) ; platform = 'SATOB'
                      IF (PRESENT (satob)) satob = satob + 1

          

          CASE (96:97) ; platform = 'AIREP'
                         IF (PRESENT (airep)) airep = airep + 1

          

          CASE (101) ; platform = 'TAMDAR'
                         IF (PRESENT (tamdar)) tamdar = tamdar + 1 

          

          CASE (111) ; platform = 'GPSPW'
                      IF (PRESENT (gpspw)) gpspw = gpspw + 1

          

          CASE (114) ; platform = 'GPSZD'
                      IF (PRESENT (gpszd)) gpszd = gpszd + 1

          

          CASE (116) ; platform = 'GPSRF'
                      IF (PRESENT (gpsrf)) gpsrf = gpsrf + 1

          
 
          CASE (118) ; platform = 'GPSEP'
                      IF (PRESENT (gpsep)) gpsep = gpsep + 1

          

          CASE (121) ; platform = 'SSMT1'
                      IF (PRESENT (ssmt1)) ssmt1 = ssmt1 + 1

          

          CASE (122) ; platform = 'SSMT2'
                      IF (PRESENT (ssmt2)) ssmt2 = ssmt2 + 1

          

          CASE (125,126) ; platform = 'SSMI'
                      IF (PRESENT (ssmi)) ssmi = ssmi + 1

          

          CASE (131) ; platform = 'TOVS'
                      IF (PRESENT (tovs)) tovs = tovs + 1


          CASE (132) ; platform = 'PROFL'
                      IF (PRESENT (profl)) profl = profl + 1 

          CASE (133) ; platform = 'AIRSRET'
                      IF (PRESENT (airs)) airs = airs + 1

          

          CASE (281) ; platform = 'QSCAT'
                      IF (PRESENT (qscat)) qscat = qscat + 1

          

          CASE DEFAULT;
                       platform = 'UNKNOWN'
                       IF (PRESENT (other)) other = other + 1

       END SELECT


       

       SELECT CASE (TRIM (platform))

       CASE ('SYNOP','SYNOP MOBIL');                       platform = "SYNOP";
       CASE ('SHIP');                                      platform = "SHIP" ;
       CASE ('BUOY');                                      platform = "BUOY" ;
       CASE ('BOGUS');                                     platform = "BOGUS";
       CASE ('METAR','SPECI');                             platform = "METAR";
       CASE ('PILOT','PILOT SHIP','PILOT MOBIL');          platform = "PILOT";
       CASE ('TEMP','TEMP SHIP','TEMP DROP','TEMP MOBIL'); platform = "SOUND";
       CASE ('SATEM');                                     platform = "SATEM";
       CASE ('SATOB');                                     platform = "SATOB";
       CASE ('AIREP');                                     platform = "AIREP";
       CASE ('TAMDAR');                                    platform = "TAMDAR";
       CASE ('GPSPW');                                     platform = "GPSPW";
       CASE ('GPSZD');                                     platform = "GPSZD";
       CASE ('GPSRF');                                     platform = "GPSRF";
       CASE ('GPSEP');                                     platform = "GPSEP";
       CASE ('SSMT1');                                     platform = "SSMT1";
       CASE ('SSMT2');                                     platform = "SSMT2";
       CASE ('TOVS');                                      platform = "TOVS" ;
       CASE ('SSMI');                                      platform = "SSMI" ;
       CASE ('Qscat');                                     platform = "QSCAT";
       CASE ('PROFL');                                     platform = "PROFL";
       CASE ('AIRSRET');                                   platform = "AIRSRET";
       CASE ('UNKNOWN');                                   platform = "UNKONWN";

       END SELECT
     
END SUBROUTINE fm_decoder 
