       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTCARDS.

       DATA DIVISION. 

       WORKING-STORAGE SECTION.
      ******************************************************************
      * CARD DEFINITIONS
      *   DEFINES A CARD FOR THE GAME
       01 CARD.
          02 C-RANK.
             03 RANK-A             PIC X.
             03 RANK-N             PIC 99.
          02 C-SUIT.
             03 SUIT-A             PIC X.
             03 SUIT-C             PIC X.
             03 SUIT-N             PIC 9.

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
                      07 RANK-A    PIC X.
      *                  NUMBER CODE OF RANK:
      *                  1 - 13
                      07 RANK-N    PIC 99.
                   06 CARD-SUIT.
      *                  ALPHA CODE OF SUIT:
      *                  D(IAMONDS),C(LUB),H(EARTS),S(PADES)
                      07 SUIT-A    PIC X.
      *                  COLOR OF SUIT:
      *                  R(ED), B(LACK)
                      07 SUIT-C    PIC X.
      *                  NUMBER CODE OF SUIT:
      *                  1 - 4
                      07 SUIT-N    PIC 9.
      *      DEFINES ALL FOUNDATION STACKS OF THE GAME
          02 FOUNDATION.
      *      THE OPERATION REQUESTED TO BE PERFORMED ON THE FOUNDATION
             05 OP-CODE            PIC 9.
      *      THE ERROR CODE, IF ANY, FOR THE REQUESTED OPERATION
             05 ERR-CODE           PIC 9.
      *      THE SUIT OF THE CARD TO PUSH ONTO THE FOUNDATION
      *          INTO THE STACK WITH NUMBER SUIT-TO-PUSH.
             05 SUIT-TO-PUSH       PIC 9.
      *      THE RANK TO PUSH ONTO THE FOUNDATION INTO THE STACK WITH
      *          NUMBER SUIT-TO-PUSH.
             05 F-STACKS-T OCCURS 4 TIMES INDEXED BY F-STACK-I.
      *         HOW MANY CARDS ARE IN THE STACK.
                10 COUNT-OF-CARDS  PIC 99  VALUE 0.
      *         NEXT ACCEPTABLE RANK
      *         ALWAYS COUNT-OF-CARDS + 1
                10 NEXT-RANK       PIC 99  VALUE 1.
      *         SIGNAL, IF THE STACK IS FULL
                10 IS-FULL         PIC X   VALUE 'N'.
      *           ALPHA CODE OF RANK OF TOP CARD:
      *           A,2,3,4,5,6,7,8,9,T,J,Q,K             
                10 RANK-A          PIC X   VALUE 'X'.
      *           ALPHA CODE OF SUIT OF TOP CARD:
      *           D(IAMONDS),C(LUB),H(EARTS),S(PADES)
                10 SUIT-A          PIC X   VALUE 'X'.
      *   DEFINES THE STOCK OF THE GAME
          02 STOCK.
      *      THE OPERATION REQUESTED TO BE PERFORMED ON THE FOUNDATION
             03 OP-CODE            PIC 9.
      *      HOW MANY CARDS ARE IN THE STOCK.
      *      IN THE INITIALIZATION PHASE, THIS COUNTER GOES UP,
      *        AS IT COUNTS THE CARDS TRANFERRED INTO THE STOCK
      *      THE STOCK SHRINKS OVER TIME, WHEN WE FETCH CARDS
             03 COUNT-OF-CARDS     PIC 99.
      *      TABLE OF CARDS IN THE STOCK
             03 STOCK-T OCCURS 52 TIMES INDEXED BY STOCK-I.
                06 RANK-N          PIC 99.
                06 SUIT-N          PIC 9.

      ******************************************************************
      * VARIABLES FOR THE TEST RUN
       01 TESTS-RUN                PIC 999 VALUE 0.
       01 TESTS-OK                 PIC 999 VALUE 0.
       01 TESTS-NOK                PIC 999 VALUE 0.

      ******************************************************************
       PROCEDURE DIVISION.

           DISPLAY "TESTCARDS"

           CALL 'CARDS' USING GAME
           END-CALL.

      *    WE SHOULD TEST MORE THAN THIS, BUT AT THE MOMENT, 
      *    WE JUST CHECK THE NUMBER CODE OF THE RANK AND THE SUIT.
           PERFORM 01-TEST-SUIT-CODES.

           PERFORM 02-TEST-RANK-CODES.

           DISPLAY "TESTS RUN: " TESTS-RUN
           DISPLAY "SUCCESSFUL / FAILED: " TESTS-NOK " / " TESTS-OK

           STOP RUN.

       01-TEST-SUIT-CODES.
      *    RUN THROUGH ALL SUITS
           PERFORM VARYING CARDS-S-I
              FROM 1 BY 1
              UNTIL CARDS-S-I > 4

      *            RUN THROUGH ALL RANKS
                   PERFORM VARYING CARDS-R-I
                      FROM 1 BY 1
                      UNTIL CARDS-R-I > 13

                           MOVE CARDS-RANK-T(CARDS-S-I, CARDS-R-I)
                              TO CARD
                           ADD 1 TO TESTS-RUN
                           IF SUIT-N OF C-SUIT IS EQUAL
                              TO CARDS-S-I
                              ADD 1 TO TESTS-OK 
                           ELSE
                              ADD 1 TO TESTS-NOK 
                              DISPLAY SUIT-N OF C-SUIT "<>" CARDS-S-I
                           END-IF
                   END-PERFORM
           END-PERFORM.      

       02-TEST-RANK-CODES.
      *    RUN THROUGH ALL SUITS
           PERFORM VARYING CARDS-S-I
              FROM 1 BY 1
              UNTIL CARDS-S-I > 4

      *            RUN THROUGH ALL RANKS
                   PERFORM VARYING CARDS-R-I
                      FROM 1 BY 1
                      UNTIL CARDS-R-I > 13

                           MOVE CARDS-RANK-T(CARDS-S-I, CARDS-R-I)
                              TO CARD
                           ADD 1 TO TESTS-RUN
                           IF RANK-N OF C-RANK IS EQUAL
                              TO CARDS-R-I
                              ADD 1 TO TESTS-OK 
                           ELSE
                              ADD 1 TO TESTS-NOK 
                              DISPLAY RANK-N OF C-RANK "<>" CARDS-R-I
                           END-IF
                   END-PERFORM
           END-PERFORM.