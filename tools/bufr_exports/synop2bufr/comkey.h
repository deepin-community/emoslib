C
      COMMON /COMKEY/ NTYPE,NSBTYPE,NYEAR,NMONTH,NDAY,NHOUR,NMINUTE,
     1                NSECOND,NLAT1,NLON1,NLAT2,NLON2,NOBS,IDSAT,
     1                NLREC,NRDAY,NRHOUR,NRMIN,NRSEC,NCORR,NRECR,
     1                NNIL,NQC,NCORN(4),NPART(4),NBUFTYPE
C
C         KEY DEFINITION
C
C         NTYPE      - REPORT TYPE               NLREC     - LENGTH OF DATA
C         NSBTYPE    - REPORT SUB TYPE           NRDAY     - DA DAY
C         NYEAR      - YEAR                      NRHOUR    - DA HOUR
C         NMONTH     - MONTH                     NRMIN     - DA MINUTE 
C         NDAY       - DAY                       NRSEC     - DA SECOND
C         NHOUR      - HOUR                      NCORR     - CORRECTION NO.
C         NMINUTE    - MINUTE                    NRECR     - RECORD RECEIVED
C         NSECOND    - SECOND                    NNIL      - NIL REPORT
C         NLAT1      - LATITUDE                  NQC       - MINIMUM PERCENTAGE
C         NLON1      - LONGITUDE                             OF Q/C CHECK. 
C         NLAT2      - LATITUDE                  NCORN     - CORRECTION FOR
C         NLON2      - LONGITUDE                             TEMP/PILOT (A,B,..)
C         NOBS       - NUMBER OF OBSERVATIONS    NPART     - TEMP/PILOT PART
C         NBUFTYPE   - BUFR TABLE A TYPE         
