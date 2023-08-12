       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTFOUNDATION.

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
      *          THE FOUNDATION HAS FOUR STACKS TO MAINTAIN
      *          THE INDEX INTO THE SPECIFIC STACK IS DEFINED BY THE
      *          SUIT NUMBER, AS WE HAVE FOUR SUITS
             05 F-STACKS-T OCCURS 4 TIMES INDEXED BY F-STACK-I.
      *            HOW MANY CARDS ARE IN THE STACK.
                10 COUNT-OF-CARDS  PIC 99  VALUE 0.
      *            NEXT ACCEPTABLE RANK
      *            ALWAYS COUNT-OF-CARDS + 1
                10 NEXT-RANK       PIC 99  VALUE 1.
      *            SIGNAL, IF THE STACK IS FULL
                10 IS-FULL         PIC X.
      *            ALPHA CODE OF RANK OF TOP CARD:
      *            A,2,3,4,5,6,7,8,9,T,J,Q,K             
                10 RANK-A          PIC X.
      *            ALPHA CODE OF SUIT OF TOP CARD:
      *            D(IAMONDS),C(LUB),H(EARTS),S(PADES)
                10 SUIT-A          PIC X.
      *      DEFINES THE STOCK OF THE GAME
          02 STOCK.
      *      THE OPERATION REQUESTED TO BE PERFORMED ON THE FOUNDATION
             03 OP-CODE            PIC 9.
      *      THE ERROR CODE, IF ANY, FOR THE REQUESTED OPERATION
             03 ERR-CODE           PIC 9.
      *      THE CARD FETCHED FROM THE STOCK
             03 CARD-FETCHED.
                26 RANK-N          PIC 99.
                26 SUIT-N          PIC 9.
      *      HOW MANY CARDS ARE IN THE STOCK.
      *      IN THE INITIALIZATION PHASE, THIS COUNTER GOES UP,
      *        AS IT COUNTS THE CARDS TRANFERRED INTO THE STOCK
      *      THE STOCK SHRINKS OVER TIME, WHEN WE FETCH CARDS
             03 COUNT-OF-CARDS     PIC 99.
      *      TABLE OF CARDS IN THE STOCK
             03 STOCK-T OCCURS 52 TIMES INDEXED BY STOCK-I.
                06 RANK-N          PIC 99.
                06 SUIT-N          PIC 9.      
      *      DEFINES ALL TABLEAU STACKS OF THE GAME
          02 TABLEAU.
      *      THE OPERATION REQUESTED TO BE PERFORMED ON THE TABLEAU
             05 OP-CODE            PIC 9.
      *      THE ERROR CODE, IF ANY, FOR THE REQUESTED OPERATION
             05 ERR-CODE           PIC 9.
      *         THE STACK-INDEX IN SCOPE FOR THE REQUESTED OPERATION
             05 STACK-I-IN-SCOPE   PIC 99.
      *         THE CARD IN SCOPE FOR THE REQUESTED OPERATION
             05 CARD-IN-SCOPE.
                26 RANK-N          PIC 99.
                26 SUIT-N          PIC 9.
             05 T-COUNT-OF-CARDS   PIC 99.
             05 T-STACKS-T OCCURS 7 TIMES INDEXED BY T-STACK-I.
      *            HOW MANY CARDS ARE IN THE STACK.
                10 COUNT-OF-CARDS  PIC 99  VALUE 0.
      *            THE CARDS IN ONE STACK
                10 CARDS-T OCCURS 52 TIMES INDEXED BY CARDS-T-I.
                   26 RANK-N       PIC 99.
                   26 SUIT-N       PIC 9.

      ******************************************************************
      * VARIABLES FOR THE TEST RUN
       01 T-RANK-A                 PIC X.
       01 T-SUIT-A                 PIC X.
       01 TESTS-RUN                PIC 999 VALUE 0.
       01 TESTS-OK                 PIC 999 VALUE 0.
       01 TESTS-NOK                PIC 999 VALUE 0.
       01 TESTS-RANK               PIC 999 VALUE 1.

      ******************************************************************
       PROCEDURE DIVISION.
           DISPLAY "TESTFOUNDATION"           

           PERFORM 01-TEST-RESET.

           PERFORM VARYING SUIT-TO-PUSH
              FROM 1 BY 1
              UNTIL SUIT-TO-PUSH > 4
                   PERFORM 02-TEST-SUIT-PUSH-ALL-OK
           END-PERFORM

           PERFORM VARYING SUIT-TO-PUSH
              FROM 1 BY 1
              UNTIL SUIT-TO-PUSH > 4
                   PERFORM 03-TEST-SUIT-PUSH-1-TO-MANY
           END-PERFORM

           PERFORM 10-TEST-PICS-OF-TOP-CARD.

           DISPLAY "TESTS RUN: " TESTS-RUN
           DISPLAY "SUCCESSFUL / FAILED: " TESTS-OK " / " TESTS-NOK
           GOBACK.

      ******************************************************************
       01-TEST-RESET.
           PERFORM 01-FOUNDATION-RESET.

           ADD 1 TO TESTS-RUN.
           IF ERR-CODE OF FOUNDATION IS EQUAL TO 0
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "FAILED: 01-TEST-RESET: ERR-CODE <> 0"
           END-IF.

           PERFORM VARYING F-STACK-I
              FROM 1 BY 1
              UNTIL F-STACK-I > 4
                   ADD 1 TO TESTS-RUN
                   IF COUNT-OF-CARDS OF F-STACKS-T(F-STACK-I)
                      IS EQUAL TO 0
                      ADD 1 TO TESTS-OK
                   ELSE
                      ADD 1 TO TESTS-NOK
                      DISPLAY COUNT-OF-CARDS OF F-STACKS-T(F-STACK-I)
                   END-IF

                   ADD 1 TO TESTS-RUN
                   IF NEXT-RANK OF F-STACKS-T(F-STACK-I) IS EQUAL TO 1
                      ADD 1 TO TESTS-OK
                   ELSE
                      ADD 1 TO TESTS-NOK
                      DISPLAY NEXT-RANK OF F-STACKS-T(F-STACK-I)
                   END-IF

                   ADD 1 TO TESTS-RUN
                   IF IS-FULL OF F-STACKS-T(F-STACK-I) IS EQUAL TO 'N'
                      ADD 1 TO TESTS-OK
                   ELSE
                      ADD 1 TO TESTS-NOK
                      DISPLAY IS-FULL OF F-STACKS-T(F-STACK-I)
                   END-IF

                   ADD 1 TO TESTS-RUN
                   IF RANK-A OF F-STACKS-T(F-STACK-I) IS EQUAL TO 'X'
                      ADD 1 TO TESTS-OK
                   ELSE
                      ADD 1 TO TESTS-NOK
                      DISPLAY RANK-A OF F-STACKS-T(F-STACK-I)
                   END-IF

                   ADD 1 TO TESTS-RUN
                   IF SUIT-A OF F-STACKS-T(F-STACK-I) IS EQUAL TO 'X'
                      ADD 1 TO TESTS-OK
                   ELSE
                      ADD 1 TO TESTS-NOK
                      DISPLAY SUIT-A OF F-STACKS-T(F-STACK-I)
                   END-IF
           END-PERFORM.

      ******************************************************************
       02-TEST-SUIT-PUSH-ALL-OK.
           PERFORM 01-FOUNDATION-RESET.

           MOVE SUIT-TO-PUSH TO F-STACK-I.

           PERFORM VARYING TESTS-RANK
              FROM 1 BY 1
              UNTIL TESTS-RANK > 13
                   MOVE 2 TO OP-CODE OF FOUNDATION
                   CALL 'FOUNDATION' USING GAME
                   END-CALL

                   ADD 1 TO TESTS-RUN
                   IF ERR-CODE OF FOUNDATION IS EQUAL TO 0
                      ADD 1 TO TESTS-OK
                   ELSE
                      ADD 1 TO TESTS-NOK
                      DISPLAY "FAILED: 02-TEST-SUIT-PUSH-ALL-OK"
                         WITH NO ADVANCING
                      DISPLAY ": ERR-CODE <> 0"
                   END-IF

                   ADD 1 TO TESTS-RUN
                   IF COUNT-OF-CARDS OF F-STACKS-T(F-STACK-I)
                      IS EQUAL TO TESTS-RANK
                      ADD 1 TO TESTS-OK
                   ELSE
                      ADD 1 TO TESTS-NOK
                      DISPLAY COUNT-OF-CARDS OF F-STACKS-T(F-STACK-I)
                   END-IF
      
                   ADD 1 TO TESTS-RUN
                   IF NEXT-RANK OF F-STACKS-T(F-STACK-I)
                      IS EQUAL TO TESTS-RANK + 1
                      ADD 1 TO TESTS-OK
                   ELSE
                      ADD 1 TO TESTS-NOK
                      DISPLAY NEXT-RANK OF F-STACKS-T(F-STACK-I)
                   END-IF
      
                   IF TESTS-RANK IS LESS THAN 13 THEN
                      ADD 1 TO TESTS-RUN
                      IF IS-FULL OF F-STACKS-T(F-STACK-I) IS EQUAL TO
                         'N'
                         ADD 1 TO TESTS-OK
                      ELSE
                         ADD 1 TO TESTS-NOK
                         DISPLAY "L " IS-FULL OF F-STACKS-T(F-STACK-I)
                      END-IF
                   END-IF
           END-PERFORM.
           ADD 1 TO TESTS-RUN
           IF IS-FULL OF F-STACKS-T(F-STACK-I) IS EQUAL TO 'Y'
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "A " IS-FULL OF F-STACKS-T(F-STACK-I)
           END-IF.

      ******************************************************************
       03-TEST-SUIT-PUSH-1-TO-MANY.
           PERFORM 01-FOUNDATION-RESET.

           MOVE SUIT-TO-PUSH TO F-STACK-I.

           PERFORM VARYING TESTS-RANK
              FROM 1 BY 1
              UNTIL TESTS-RANK > 13
                   MOVE 2 TO OP-CODE OF FOUNDATION
                   CALL 'FOUNDATION' USING GAME
                   END-CALL
           END-PERFORM.
           MOVE 2 TO OP-CODE OF FOUNDATION
           CALL 'FOUNDATION' USING GAME
           END-CALL.
           ADD 1 TO TESTS-RUN
           IF ERR-CODE OF FOUNDATION IS EQUAL TO 1
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "ERR-CODE=" ERR-CODE OF FOUNDATION
           END-IF.

      ******************************************************************
       10-TEST-PICS-OF-TOP-CARD.
           PERFORM 01-FOUNDATION-RESET.

           PERFORM VARYING F-STACK-I
              FROM 1 BY 1
              UNTIL F-STACK-I > 4
                   MOVE F-STACK-I TO SUIT-TO-PUSH
                   PERFORM VARYING TESTS-RANK
                      FROM 1 BY 1
                      UNTIL TESTS-RANK > 13
                           MOVE 2 TO OP-CODE OF FOUNDATION
                           CALL 'FOUNDATION' USING GAME
                           END-CALL

                           ADD 1 TO TESTS-RUN
                           IF ERR-CODE OF FOUNDATION IS EQUAL TO 0
                              ADD 1 TO TESTS-OK
                           ELSE
                              ADD 1 TO TESTS-NOK
                              DISPLAY
                                 "FAILED: 10-TEST-PICS-OF-TOP-CARD:"
                                 WITH NO ADVANCING 
                              DISPLAY " ERR-CODE <> 0"
                           END-IF

      *                    FETCH CARD FROM CARDS FOR SUIT/RANK
                           MOVE CARDS-RANK-T(F-STACK-I, TESTS-RANK)
                              TO CARD
      *                    COMPARE NOW THE DATA OF THE CARD TO THE
      *                    FOUNDATION STACK
      *                    CHECK RANK
                           MOVE RANK-A OF F-STACKS-T(F-STACK-I) TO
                              T-RANK-A
                           ADD 1 TO TESTS-RUN
                           IF RANK-A OF C-RANK IS EQUAL TO T-RANK-A
                              ADD 1 TO TESTS-OK
                           ELSE
                              ADD 1 TO TESTS-NOK
                              DISPLAY
                                 "FAILED: 10-TEST-PICS-OF-TOP-CARD:"
                                 WITH NO ADVANCING 
                              DISPLAY "RANK-A " RANK-A OF C-RANK
                                 WITH NO ADVANCING 
                              DISPLAY " <> " T-RANK-A
                           END-IF

      *                    CHECK RANK
                           MOVE SUIT-A OF F-STACKS-T(F-STACK-I) TO
                              T-SUIT-A
                           IF SUIT-A OF C-SUIT IS EQUAL TO T-SUIT-A
                              ADD 1 TO TESTS-OK
                           ELSE
                              ADD 1 TO TESTS-NOK
                              DISPLAY
                                 "FAILED: 10-TEST-PICS-OF-TOP-CARD:"
                                 WITH NO ADVANCING 
                              DISPLAY "SUIT-A " SUIT-A OF C-SUIT
                                 WITH NO ADVANCING 
                              DISPLAY " <> " T-SUIT-A
                           END-IF
                           
                   END-PERFORM
           END-PERFORM.

      ******************************************************************
      * HERE ARE ALL SUPPORT PROCEDURES                                *
      ******************************************************************
       01-FOUNDATION-RESET.
           CALL 'CARDS' USING GAME
           END-CALL.

           MOVE 1 TO OP-CODE OF FOUNDATION.
           CALL 'FOUNDATION' USING GAME
           END-CALL.