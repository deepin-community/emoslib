C
C     'COMSTAT' CONTAINS CONTAINS COUNTERS TO GATHER STATISTICS
C     ON BULLETINS AND REPORTS RECEIVED AND DECODED , AS WELL AS
C     INFORMATION ON THE TYPE AND NUMBER OF ERRORS.
C      - NUMBULL IS THE NUMBER OF ALL THE BULLETINS
C      - NUMBERR IS THE NUMBER OF BULLETIN ERRORS
C      - NUMRERR IS THE NUMBER OF REPORT ERRORS (FOR EACH CODE TYPE)
C      - NOER    IS THE NUMBER DIFFERENT ERROR TYPES  ( -DITTO- )
C      - NUMREP  IS THE NUMBER DIFFERENT REPORT TYPES ( -DITTO- )
C     IGRIBS  = 1  IF GRIB CODE FIELD(S) WERE AVAILABLE
C             = 0  OTHERWISE
C
C        NUMREP(1) & NOER(1,I) REPRESENT SURFACE     OBSERVATIONS
C         ---- (2) & ----(2,I) --------- AIRCRAFT    ------------
C         ---- (3) & ----(3,I) --------- TEMP PART A ------------
C         ---- (4) & ----(4,I) --------- TEMP PART B ------------
C         ---- (5) & ----(5,I) --------- TEMP PART C ------------
C         ---- (7) & ----(7,I) --------- PILOT PART A ------------
C         ---- (8) & ----(8,I) --------- PILOT PART B ------------
C         ---- (9) & ----(9,I) --------- PILOT PART C ------------
C         --- (10) & ---(10,I) --------- PILOT PART D ------------
C         --- (11) & ---(11,I) --------- SATEM PART A ------------
C         --- (12) & ---(12,I) --------- SATEM PART B ------------
C         --- (13) & ---(13,I) --------- SATEM PART C ------------
C         --- (14) & ---(14,I) --------- SATEM PART D ------------
C         --- (15) & ---(15,I) --------- PAOB --------------------
C         --- (16) & ---(16,I) --------- SATOB SECTION 1,2 AND 3 -
C         --- (17) & ---(17,I) --------- SATOB SECTION 1 AND 4 ---
C         --- (18) & ---(18,I) --------- SATOB SECTION 1 AND 5 ---
C         --- (19) & ---(19,I) SATOB SECTION 1 AND 6 -----------
C         --- (20) & ---(20,I) SATOB SECTION 1 AND 7 -----------
C         --- (21) & ---(21,I) SATOB SECTION 1 AND 8 -----------
C         --- (22) & ---(22,I) DRIBU ---------------------------
C         --- (23) & ---(23,I) BATHY ---------------------------
C         --- (24) & ---(24,I) TESAC ---------------------------
C         --- (25) & ---(25,I) RECCO ---------------------------
C         --- (26) & ---(26,I) GRID SST ANALYSIS ---------------
C
      COMMON / COMSTAT / NUMBULL,NUMBERR(jp18),NUMRERR(jp13),
     &                   NOER(JP13,JP5),NUMREP(JP13)
C
