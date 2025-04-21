      SUBROUTINE SETUP (domain_check_h, IPROJ, PHIC, XLONC, &
                                        TRUELAT1, TRUELAT2, &
                        MAXNES, NESTIX, NESTJX, DIS, NUMC, NESTI, NESTJ, &
                        IXC, JXC, XCNTR, YCNTR, XN, POLE, PSI1,  C2, &
                        XIM11, XJM11) 





































      IMPLICIT NONE

      INTEGER, INTENT (inout) :: iproj
      REAL,    INTENT (inout) :: phic,  xlonc, truelat1, truelat2
      INTEGER, INTENT (in)    :: maxnes
      INTEGER, INTENT (in), DIMENSION (maxnes) :: nestix, nestjx, NUMC, &
                                              nesti, nestj
      REAL,    INTENT (inout), DIMENSION (maxnes) :: dis

      INTEGER, INTENT (out)   :: ixc, jxc

      REAL,    INTENT (out)   :: pole, xn
      REAL,    INTENT (out)   :: xim11 (maxnes), xjm11 (maxnes)
      REAL,    INTENT (out)   :: psi1, c2, xcntr, ycntr


      REAL :: iexp, aexp
      REAL :: psx, cell, cell2, r, phictr, hdsize, sign
      REAL :: xjc, xdis, yjc, ydis

      REAL :: cntrj0, cntri0
      INTEGER :: ixcmax, jxcmax
      REAL, DIMENSION (maxnes) :: xsouth,xwest,xnorth,xeast

      INTEGER :: incr
      INTEGER :: m, nm, nmc, nd1, nd2, mismatch
      INTEGER :: ioffst, joffst, iimxn, jjmxn
      INTEGER :: iratio (maxnes), nratio (maxnes)

      INTEGER, DIMENSION (3) :: nproj

      CHARACTER (LEN=80) :: project



      include 'constants.inc'
      
      logical  domain_check_h
      DATA nproj / 1, 2, 3  /


      if (iproj == 0 ) then
          PROJECT='CE'
          XCNTR = 0
          YCNTR = 0

          DIS(1) = 110.0
          if (domain_check_h) STOP 'domain_check_h'
          goto 1100
      endif



                                                                              


      IEXP = 0
      AEXP =  -1.001*DIS(1)







                                                                               
      XN = -1.0E08                                                           

      IF (PHIC.LT.0) THEN                                                        
         SIGN=-1.      

      ELSE                                                                      

         SIGN=1.       

      ENDIF                                                                     

      POLE = 90.                                                             

      IF (IPROJ .EQ. NPROJ(1)) THEN

          IF (ABS (TRUELAT1) .GT. 90.) THEN
              TRUELAT1 = 60.
              TRUELAT2 = 30.
              TRUELAT1 = SIGN*TRUELAT1
              TRUELAT2 = SIGN*TRUELAT2
          ENDIF

          IF (ABS(TRUELAT1 - TRUELAT2) .GT. 0.1) then
            XN = ALOG10 (COS (TRUELAT1 / CONV)) - &
                 ALOG10 (COS (TRUELAT2 / CONV))

            XN = XN/(ALOG10 (TAN ((45.0 - SIGN*TRUELAT1/2.0) / CONV)) - &
                     ALOG10 (TAN ((45.0 - SIGN*TRUELAT2/2.0) / CONV)))
          ELSE
            XN = SIGN*SIN(TRUELAT1 / CONV)
          ENDIF

          PSI1 = 90.-SIGN*TRUELAT1

          PROJECT='LC'

      ELSE IF (IPROJ .EQ. NPROJ(2)) THEN

          XN = 1.0

          IF (ABS(TRUELAT1) .GT. 90.) THEN

              TRUELAT1 = 60.
              TRUELAT2 = 0.

              TRUELAT1 = SIGN*TRUELAT1
              TRUELAT2 = SIGN*TRUELAT2

          ENDIF




          PSI1 = 90.-SIGN*TRUELAT1

          PROJECT = 'ST'

      ELSE IF (IPROJ .EQ. NPROJ(3)) THEN

          XN = 0.                                                                

          IF (ABS (TRUELAT1) .GT. 90.) THEN

              TRUELAT1 = 0.
              TRUELAT2 = 0.                                                               
          ENDIF

          IF (TRUELAT1 .NE. 0.) THEN                                                   
              WRITE (0,'(/,A)') &
             "MERCATOR PROJECTION IS ONLY SUPPORTED AT 0 DEGREE TRUE LATITUDE."
              WRITE (0,'(A,/)') &
             "TRUELAT1 IS RESET TO 0"

               TRUELAT1 = 0.                                                               
          ENDIF

          PSI1 = 0.
          PROJECT = 'ME'

      END IF                                                                    

      PSI1 = PSI1 / CONV

      IF (PHIC .LT. 0.) THEN                                                      
          PSI1 = -PSI1
          POLE = -POLE

      ENDIF                                                                     



      IF (IPROJ .NE. NPROJ(3)) THEN

          PSX = (POLE-PHIC)/CONV

          IF (IPROJ .EQ. NPROJ(1)) THEN

              CELL  = A*SIN (PSI1)/XN
              CELL2 = (TAN (PSX/2.)) / (TAN (PSI1/2.))                               
         ENDIF                                                                  

         IF (IPROJ .EQ. NPROJ(2)) THEN
            CELL  = A*SIN (PSX)/XN
            CELL2 = (1. + COS (PSI1))/(1. + COS (PSX))
         ENDIF                                                                  

         R = CELL*(CELL2)**XN                                                   

         XCNTR = 0.0                                                            
         YCNTR = -R                                                             

      ENDIF                                                                     



      IF (IPROJ .EQ. NPROJ(3)) THEN

         C2     = A*COS (PSI1)
         XCNTR  = 0.0                                                           
         PHICTR = PHIC/CONV                                                     
         CELL   = COS (PHICTR)/(1.0+SIN (PHICTR)) 
         YCNTR  = - C2*ALOG (CELL)                                              

      ENDIF                                                                     

1100  continue

      WRITE (0,'(2(A,F8.1),A,2X,A,f10.3)') &
      " COARSE GRID CENTER IS AT X = ",XCNTR," KM AND Y = ",YCNTR," KM.", &
      " DIS(1)=", DIS(1) 







        IXC = NESTIX (1)
        JXC = NESTJX (1)

        IOFFST = 0                                                              
        JOFFST = 0                                                              

      IF (IEXP .EQ. 1) THEN

          INCR = INT (AEXP/DIS (1) + 1.001)

          IXC = NESTIX (1) + INCR*2
          JXC = NESTJX (1) + INCR*2

          IOFFST = INCR
          JOFFST = INCR

          WRITE (0,'(A,I3)') " GRID IS EXPANDED BY ",INCR, &
                             " GRID POINTS ON EACH EDGE."



      ENDIF                                                                     



      CNTRJ0 = FLOAT (JXC+1)/2.
      CNTRI0 = FLOAT (IXC+1)/2.





      IXCMAX = IXC
      JXCMAX = JXC 

      DO M = 1, MAXNES                                                       
         IXCMAX = MAX0 (NESTIX(M),IXCMAX)
         JXCMAX = MAX0 (NESTJX(M),JXCMAX)
      ENDDO






      HDSIZE = (IXC-1)*DIS(1)/2.                                               













      IRATIO (1) = 1
      NRATIO (1) = 1
      XSOUTH (1) = 1.
      XWEST  (1) = 1.
      XNORTH (1) = FLOAT (IXC)
      XEAST  (1) = FLOAT (JXC)

      XJC = (XEAST(1) + 1.0)/2.                                                 







      MISMATCH = 0                                                              
                                                                                
      DO 30 NM = 2, MAXNES                                                      



      NMC = NUMC (NM)

      IF (AMOD ((DIS (NMC)+0.09),DIS (NM)) .GT. 0.1) THEN

         MISMATCH = MISMATCH + 1                                                








        GO TO 30                                                                

      ENDIF                                                                     

      IRATIO (NM) = NINT (DIS (NMC)/ DIS (NM))
      NRATIO (NM) = NINT (DIS (1)  / DIS (NM))                                         




      IF (MOD((NESTIX(NM)-1),IRATIO(NM)).NE.0) THEN                             

        MISMATCH = MISMATCH + 1                                                 

        IIMXN = (INT(FLOAT(NESTIX(NM)-1)/IRATIO(NM))+1)*IRATIO(NM) + 1          





      ENDIF                                                                     

      IF (MOD ((NESTJX (NM)-1),IRATIO (NM)).NE.0) THEN

         MISMATCH = MISMATCH + 1

        JJMXN = (INT(FLOAT(NESTJX(NM)-1)/IRATIO(NM))+1)*IRATIO(NM) + 1          




      ENDIF                                                                    



                                                                                







      XDIS = 0.0                                                                
      YDIS = 0.0                                                                
      ND1 = NM                                                                  
      ND2 = NMC                                                                 

40    CONTINUE                                                                  

      XDIS = (NESTI(ND1)-1)*DIS(ND2) + XDIS
      YDIS = (NESTJ(ND1)-1)*DIS(ND2) + YDIS

      IF (ND2 .GT. 1) THEN                                                      
        ND1 = ND2                                                               
        ND2 = NUMC(ND2)                                                        
        GO TO 40                                                                
      ENDIF                                                                     

      XSOUTH(NM) = XDIS/DIS(1) + FLOAT(IOFFST) + 1                             
      XWEST(NM)  = YDIS/DIS(1) + FLOAT(JOFFST) + 1                             
      XNORTH(NM) = XSOUTH(NM) + FLOAT(NESTIX(NM)-1)*DIS(NM)/DIS(1)             
      XEAST(NM)  = XWEST(NM) + FLOAT(NESTJX(NM)-1)*DIS(NM)/DIS(1)             
                                                                                






30    CONTINUE                                                                 

      IF (MISMATCH.GT.0) THEN                                                  





       ENDIF                                                                    



       
       XIM11 = XSOUTH
       XJM11 = XWEST

       END SUBROUTINE SETUP
