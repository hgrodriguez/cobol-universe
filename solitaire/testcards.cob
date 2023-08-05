       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTCARDS.

       DATA DIVISION. 

       WORKING-STORAGE SECTION.
      ******************************************************************
      *   THE GAME OVERALL.
       01 GAME.
      *      DEFINES ALL POSSIBLE CARDS IN THE GAME
          02 CARDS.
      *         TABLE OF CARDS IN THE GAME
             04 CARDS-SUIT-T OCCURS 4 TIMES INDEXED BY CARDS-S-I.
                05 CARDS-RANK-T OCCURS 13 TIMES INDEXED BY CARDS-R-I.
                   06 CARD-RANK.
      *                  ALPHA CODE OF RANK:
      *                  A,2,3,4,5,6,7,8,9,T,J,Q,K             
                      07 RANK-A  PIC X.
      *                  NUMBER CODE OF RANK:
      *                  1 - 13
                      07 RANK-N  PIC 99.
                   06 CARD-SUIT.
      *                  ALPHA CODE OF SUIT:
      *                  D(IAMONDS),C(LUB),H(EARTS),S(PADES)
                      07 SUIT-A  PIC X.
      *                  COLOR OF SUIT:
      *                  R(ED), B(LACK)
                      07 SUIT-C  PIC X.
      *                  NUMBER CODE OF SUIT:
      *                  1 - 4
                      07 SUIT-N  PIC 9.

      ******************************************************************
      * VARIABLES FOR THE TEST RUN
       01 TESTS-RUN              PIC 99 VALUE 0.
       01 TESTS-OK               PIC 99 VALUE 0.
       01 TESTS-NOK              PIC 99 VALUE 0.
       01 TESTS-RANK             PIC 99 VALUE 1.

      ******************************************************************
       PROCEDURE DIVISION.

           CALL 'CARDS' USING GAME
           END-CALL.

           DISPLAY "TESTS RUN: " TESTS-RUN
           DISPLAY "SUCCESSFUL / FAILED: " TESTS-NOK " / " TESTS-OK
           STOP RUN.