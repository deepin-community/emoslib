*
*  IBM Assembler version of GBYTE(S) and SBYTE(S).
*
*        ****  DOCUMENTATION  ****
*    WRITTEN BY JORDAN HASTINGS AND DENNIS JOSEPH
*               NCAR, BOULDER, COLORADO
*
*  THE FOLLOWING ROUTINES ALLOW FORTRAN ACCESS TO BIT STRINGS (BYTE
*  OF ARBITRARY LENGTH AND POSITION, PERHAPS CROSSING WORD BOUNDARIES,
*  IN THE MANNER SPECIFIED BELOW:
*
*     CALL GBYTE (PCKD,UNPK,INOFST,NBIT)
*     CALL GBYTES(PCKD,UNPK,INOFST,NBIT, NSKIP,ITER)
*
*        PCKD:     THE FULLWORD IN MEMORY FROM WHICH UNPACKING IS TO
*                  BEGIN; SUCCESSIVE FULLWORDS WILL BE FETCHED AS
*                  REQUIRED.
*        UNPK:     THE FULLWORD IN MEMORY INTO WHICH THE INITIAL BYTE
*                  OF UNPACKED DATA IS TO BE STORED; SUBSEQUENT BYTES
*                  WILL BE STORED INTO SUCCESSIVE FULLWORDS AS
*                  REQUIRED.
*        INOFST:   A FULLWORD INTEGER SPECIFYING THE INITAL OFFSET
*                  IN BITS OF THE FIRST BYTE, COUNTED FROM THE
*                  LEFTMOST BIT IN PCKD.
*        NBITS:    A FULLWORD INTEGER SPECIFYING THE NUMBER OF BITS
*                  IN EACH BYTE TO BE UNPACKED.  LEGAL BYTE WIDTHS
*                  ARE IN THE RANGE 1 - 32; BYTES OF WIDTH .LT. 32
*                  WILL BE RIGHT JUSTIFIED IN THE LOW-ORDER POSITIONS
*                  OF THE UNPK FULLWORDS, WITH HIGH-ORDER ZERO FILL.
*        NSKIP:    A FULLWORD INTEGER SPECIFYING THE NUMBER OF BITS
*                  TO SKIP BETWEEN SUCCESSIVE BYTES.  ALL NON-NEGATIVE
*                  SKIP COUNTS ARE LEGAL.
*        ITER:     A FULLWORD INTEGER SPECIFYING THE TOTAL NUMBER OF
*                  BYTES TO BE UNPACKED, AS CONTROLLED BY INOFST,
*                  NBIT AND NSKIP ABOVE.   ALL NON-NEGATIVE ITERATION
*                  COUNTS ARE LEGAL.
*
*            NOTES ...
*              1)  A MULTIPLE-BYTE ACCESS (GBYTES) WITH ITER=0 (BUT
*                  NOT 1)  IS EXACTLY EQUIVALENT TO A SINGLE-BYTE
*                  ACCESS (GBYTE).
*              2)  AN ERROR DETECTED IN THE CALLING SEQUENCE OF
*                  EITHER GBYTE OR GBYTES SUPPRESSES BYTE ACCESS,
*                  AND SETS THE FIRST ELEMENT OF UNPK = X'FFFFFFFF'
*
GBYT     TITLE 'IBM S/360 VERSION OF NCAR CDC/6600 BIT MANIPULATION'
*
*        ****  REGISTER USAGE  ****
ZERO     EQU   X'0'                CONSTANT ZERO
PCKD     EQU   X'1'                ADDR OF PACKED ARY ELEMENT
UNPK     EQU   X'2'                ADDR OF UNPACKED ARY ELEMENT
OFST     EQU   X'3'                INITIAL BIT OFFSET
NBIT     EQU   X'4'                BYTE LENGTH IN BITS (.LE.32)
NSKP     EQU   X'5'                ITERATIVE BIT SKIP
ITER     EQU   X'6'                ITERATION COUNT (.GE.0)
BITR     EQU   X'7'                BITS REMAINING IN HIWD
BITS     EQU   X'8'                BITS TO SHIFT (OFST OR NSKP)
WORK     EQU   X'9'                WORK REGISTER
HIWD     EQU   X'A'                HI-ORDER WORD (EVEN REGISTER
LOWD     EQU   X'B'                LO-ORDER WORD (ODD REGISTER)
BYTE     EQU   X'C'                RESULTANT BYTE
*        EQU   X'D'                SAVE AREA ADDR (NOT MODIFIED)
*        EQU   X'E'                RETURN ADDR (NOT MODIFIED)
BASE     EQU   X'F'                BASE ADDR REGISTER
NBFW     EQU   4                   NUMBER OF BYTES/FULL WORD
*
*
*        ****  CODE  ****
*
GBYTES   CSECT ,                   PRIME ENTRY POINT
         ENTRY GBYTE               ALTERNATE ENTRY POINT
*
         USING GBYTES,BASE         ENTRY PT ADR ESTB IN R15 BY CALLER
         SAVE      (0,12)          SAVE CALLING PROG REGISTERS (MACRO)
         LM    NSKP,ITER,4*NBFW(1) PICK UP NSKIP, ITER PARM ADDRESSES
         L     NSKP,0(,NSKP)       PICK UP BIT SKIP COUNT
         L     ITER,0(,ITER)       PICK UP ITERATION COUNT
         B     INIT                JUMP INTO CODE PROPER
*
GBYTE    DS    0H                  FORCE HALFWD ALLIGNMENT
         USING GBYTE,BASE          ALT ENTRY ADDR ESTB IN R15 BY CALLER
         SAVE      (0,12)          SAVE CALLING PROG REGISTERS (MACRO)
         SR    ITER,ITER           MAKE ITERATION COUNT ZERO
*
INIT     BALR  BASE,0              RE-ESTABLISH ADDRESSABILITY
         USING *,BASE
         LM    PCKD,NBIT,0(1)      PICK UP ARY AND PARAMETER ADDRESSES
      L         LOWD,0(,OFST)       COMPUTE FIRST PCKD WORD AND OFFSET
      LA        WORK,32
      SR        HIWD,HIWD
      DR        HIWD,WORK
      LR        OFST,HIWD        SET OFFSET TO REMAINDER
      SLA       LOWD,2
      AR        PCKD,LOWD          INCR PCKD ADDRESS BY WHOLE WORDS
         L     HIWD,0(,PCKD)       PICK UP FIRST WORD OF PACKED ARY
         LH    BITR,=H'32'         AFTER HIWD FETCH FULL 32 BITS REMAIN
         L     NBIT,0(,NBIT)       PICK UP NO. OF BITS/BYTE TO UNPACK
         SR    ZERO,ZERO           ZERO OUT REG 0
         LR    WORK,BITR           MAKE WORK=32
         CR    ITER,ZERO           TEST FOR ZERO ITERATION COUNT
         BH    INIT1               IF NOT, CONTINUE WITH INITIALIZATION
INIT0    CR    OFST,BITR           ELSE, TEST INITIAL OFFSET IN RANGE
         BNL   FAIL                      IF NOT .LT. 32, FAIL
         LH    ITER,=H'1'          FORCE EXACTLY 1 ITERATION
         B     INIT2
INIT1    AR    NSKP,NBIT           ADD NBIT TO NSKP FOR MULT ACCESS
INIT2    LA    PCKD,NBFW(PCKD)     INCR PACKED ARY POINTER
         L     LOWD,0(,PCKD)       PICK UP SECOND WD OF PACKED ARRAY
         SR    WORK,NBIT           COMPLEMENT NBITS (MODULO 32)
         CR    WORK,ZERO           TEST NBITS IN RANGE
         BL    FAIL                IF NOT,.LE.32, FAIL
         STH   WORK,STORE+4        ELSE USE TO SET UP BYTE SHIFT COUNT
*                                    OF SRL (RX) INSTR IN STORE SEQ
         LR    BITS,OFST           MAKE BITS TO SHIFT = INITIAL OFFSET
         LR    WORK,BITR           MAKE WORK=32, AGAIN
*
TEST     CR    BITS,BITR           COMPARE BITS TO SHIFT WITH REMAINING
         BL    SHIFT               IF BITS.LT.BITR, PROCEED WITH SHIFT
         BH    SPLIT               IF BITS.GT.BITR, ITER OVER SPLIT WDS
*        BE    COPY                ELSE, IF BITS.EQ.BITR, RECOPY HIWD
*
COPY     L     HIWD,0(,PCKD)       RELOAD HIWD FROM MEM WITH PREV LOWD
         LA    PCKD,NBFW(PCKD)     INCR PACKED ARY POINTER
         L     LOWD,0(,PCKD)         AND PICK UP NEXT PACKED LOWD
         LR    BITR,WORK             RESET BITR TO 32
         B     STORE               PROCEED WITH STORE OPERATIONS
*
SPLIT    STH   BITR,*+6            SET UP BITR AS SLDL SHIFT COUNT
         SLDL  HIWD,*-*            SHIFT LEFT DOUBLE LOGICAL, HIWD/LOWD
         SR    BITS,BITR           DECR BITS TO SHIFT BY SHIFTED BITR
         B     SPLIT1              BYPASS FAST FULLWORD SKIPS
         LR    OFST,BITS           COPY BITS REMAINING TO SHIFT TO OFST
*                                  (DESTROYING INITAL OFFSET PARM)
         SRA   OFST,5              DIVIDE BY 32 TO GET WORDS TO SHIFT
*        CR    OFST,ZERO             SIMULTANEOUSLY SETTING COND CODE
*                                  IF REMAINING SHIFT .LT. 1 WORD
         BNH   SPLIT1              FETCH NEXT SEQUENTIAL LOWD IMMED
         SLA   OFST,2              ELSE, MULTIPLY WDS BY 4 TO GET BYTES
         AR    PCKD,OFST           INCR PACKED ARRAY POINTER
         L     HIWD,0(,PCKD)         AND LOAD NEW HIWD
         SLA   OFST,3              MULTIPY BYTES BY 8 TO GET BITS
         SR    BITS,OFST             AND DECR BITS REMAINING TO SHIFT
SPLIT1   LA    PCKD,NBFW(PCKD)     INCR PACKED ARY PTR
         L     LOWD,0(,PCKD)         AND PICK UP NEXT PACKED LOWD
         LR    BITR,WORK             RESET BITR TO 32
         B     TEST                ITERATE AS NECESSARY
*
SHIFT    STH   BITS,*+6            SET UP BITS AS SLDL SHIFT COUNT
         SLDL  HIWD,*-*            SHIFT LEFT DOUBLE LOGICAL, HIWD/LOWD
         SR    BITR,BITS           DECR BITS REMAINING BY BITS SHIFTED
*
STORE    LR    BYTE,HIWD           COPY HIWD TO BYTE
         SRL   BYTE,*-*            SHIFT RIGHT(WITH HI-ORDER ZERO FILL)
*                                    *-* SHIFT COUNT SET UP AT INIT2
STORE1   ST    BYTE,0(,UNPK)       FILL BYTE INTO UNPACKED ARY
         LA    UNPK,NBFW(UNPK)       AND INCR UNPACKED ARY POINTER
         LR    BITS,NSKP           RESTORE BITS = (NSKP+NBIT)
         BCT   ITER,TEST           DECR ITER COUNT, CONTINUE TILL ZERO
*
*
EXIT     RETURN    (0,12)          RESTORE CALLING PROG REGS (MACRO)
*
FAIL     LH    BYTE,=X'FFFF'       MAKE BYTE A FULL WD OF 1'S
         ST    BYTE,0(,UNPK)         AND FILL INTO UNPACKED ARRAY
         B     EXIT
*
         LTORG
         END
R0       EQU   X'0'
R1       EQU   X'1'
R2       EQU   X'2'
R3       EQU   X'3'
R4       EQU   X'4'
R5       EQU   X'5'
R6       EQU   X'6'
R7       EQU   X'7'
R8       EQU   X'8'
R9       EQU   X'9'
R10      EQU   X'A'
R11      EQU   X'B'
R12      EQU   X'C'
R13      EQU   X'D'
R14      EQU   X'E'
R15      EQU   X'F'
SBYTES   CSECT ,
*
* SBYTE/SBYTES - GENERAL BIT PACKING ROUTINES.
*        SEE NCAR - TN/93, JANUARY 1974.
*        D. JOSEPH           OCTOBER, 1981.
*
         ENTRY SBYTE
         USING SBYTES,R15
         SAVE                (0,12)
         STM   R13,R14,REG13      SAVE REGISTER 13 AND 14 LOCAL.
         LM    R11,R12,16(R1)     GET SKIP AND ITERATION COUNT.
         L     R11,0(R11)
         L     R12,0(R12)
         B     START
SBYTE    DS    0H
         USING SBYTE,R15
         SAVE                (0,12)
         STM   R13,R14,REG13      SAVE REGISTER 13 AND 14 LOCAL.
         SR    R12,R12            ZERO ITERATION COUNT.
START    BALR  R15,R0             RE-ESTABLISH ADDRESSABILITY.
         USING *,R15
         LM    R7,R10,0(R1)       GET REMAINING ARG ADDRESSES.
         LA    R1,1              SET CONSTANT ONE.
         L     R10,0(R10)         GET BYTE SIZE.
         LA    R0,1               MAKE MASK.
         SLL   R0,31
         SR    R10,R1
         STH   R10,MASK+2
         AR    R10,R1
         LA    R6,32              MAKE CONSTANT 32.
         L     R14,COMP           LOAD COMPLEMENT MASK.
MASK     SRA   R0,*-*
         LR    R13,R0             SAVE MASK.
         SLL   R12,2              NUM OF ITERATIONS * FOUR.
         AR    R12,R8             SET ITERATION LIMIT.
         L     R4,0(R9)           APPLY INITIAL OFFSET.
         LA    R1,96
         CR    R4,R1              CHECK FOR LARGE OFFSET.
         BL    NODIV
         LR    R5,R4
         SR    R4,R4
         DR    R4,R6              USE DIVIDE FOR LARGE OFFSET
         SLL   R5,2               CHANGE WORD COUNT TO BYTE COUNT.
         AR    R7,R5
NODIV    EQU   *
         LA    R5,4               LOAD CONSTANT FOUR.
RELOAD   CR    R4,R6              FIND WORD AND OFFSET.
         BL    ILOAD              START IF OFFSET < 32.
         SR    R4,R6
         AR    R7,R5
         B     RELOAD
ILOAD    L     R9,0(R7)        GET STORE WORD.
BEGIN    STH   R4,PMASK+2         POSITION MASK IN DOUBLE WORD.
         SR    R1,R1
PMASK    SRDL  R0,*-*
         L     R3,0(R8)           LOAD BYTE FOR STORE.
         LA    R2,64
         SR    R2,R10             POSITION BYTE.
         SR    R2,R4
         STH   R2,SHIFTB+2
         SR    R2,R2
SHIFTB   SLDL  R2,*-*             SHIFT BYTE TO STORE.
         NR    R2,R0              MASK ON CURRENT MASK.
         XR    R0,R14             COMPLEMENT MASK.
         NR    R9,R0              OPEN HOLE FOR BYTE.
         OR    R9,R2              INSERT BYTE.
         AR    R4,R10             ADD BYTE SIZE.
         CR    R4,R6
         BNH   NOPART             BRANCH IF NO PARTIAL BYTE.
         ST    R9,0(R7)        STORE CURRENT WORD.
         SR    R4,R6
         AR    R7,R5             ADD FOUR TO WORD ADDRESS.
         L     R9,0(R7)        LOAD NEXT WORD.
         NR    R3,R1              ISOLATE PARTIAL BYTE.
         XR    R1,R14             COMPLEMENT MASK.
         NR    R9,R1              OPEN HOLE FOR PARTIAL BYTE.
         OR    R9,R3              INSERT PARTIAL BYTE.
NOPART   AR    R8,R5             COUNT BYTE.
         CR    R8,R12             CHECK IF DONE.
         BNL   DONE
         AR    R4,R11             ADD SKIP AMOUNT.
         LR    R0,R13             RELOAD MASK
         CR    R4,R6
         BL    BEGIN              BRANCH IF IN SAME WORD.
         ST    R9,0(R7)           STORE CURRENT WORD.
         B     RELOAD             GO TO GET NEW WORD.
DONE     ST    R9,0(R7)        STORE LAST WORD.
         LM    R13,R14,REG13      RESTORE REGISTER 13 AND 14.
         RETURN       (0,12)
HMASK    DS    D                  ARE FOR CURRENT MASK.
REG13    DS    F
REG14    DS    F
COMP     DC    X'FFFFFFFF'        COMPLEMENT MASK.
         LTORG
         END
