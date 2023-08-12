       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTTABLEAU.

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
       01 TESTS-RUN                PIC 999 VALUE 0.
       01 TESTS-OK                 PIC 999 VALUE 0.
       01 TESTS-NOK                PIC 999 VALUE 0.

      ******************************************************************
       PROCEDURE DIVISION.
           DISPLAY "TESTTABLEAU"

           PERFORM 01-RESET.
           PERFORM 02-TEST-EMPTY.

           PERFORM 01-RESET.
           PERFORM 03-INIT-FROM-STOCK.

           PERFORM 01-RESET.
           PERFORM 04-PUSH-CARD-TO-TABLEAU.

           PERFORM 01-RESET.
           PERFORM 04-POP-FROM-EMPTY-TABLEAU.

           PERFORM 01-RESET.
           PERFORM 05-POP-FROM-EMPTY-STACK.

           PERFORM 01-RESET.
           PERFORM 06-POP-FROM-NON-EMPTY-STACK.

           PERFORM 01-RESET.
           PERFORM 07-MANDATORY-EMPTY-TABLEAU.

           PERFORM 01-RESET.
           PERFORM 08-MANDATORY-NONEMPTY-NOMATCH.

           PERFORM 01-RESET.
           PERFORM 09-MANDATORY-NONEMPTY-MATCH1.
      
           PERFORM 01-RESET.
           PERFORM 10-MANDATORY-NONEMPTY-MATCH2.
      
           PERFORM 01-RESET.
           PERFORM 11-MANDATORY-NONEMPTY-MATCH3.
      
           PERFORM 01-RESET.
           PERFORM 12-MANDATORY-NONEMPTY-MATCH4.
      
           PERFORM 01-RESET.
           PERFORM 13-MANDATORY-NONEMPTY-NOT-TOS.

           DISPLAY "TESTS RUN: " TESTS-RUN
           DISPLAY "SUCCESSFUL / FAILED: " TESTS-OK " / " TESTS-NOK
           GOBACK.

      ******************************************************************
       01-RESET.
           CALL 'CARDS' USING GAME
           END-CALL.
           MOVE 1 TO OP-CODE OF STOCK.
           CALL 'STOCK' USING GAME
           END-CALL.
           MOVE 1 TO OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING GAME
           END-CALL.

      ******************************************************************
       02-TEST-EMPTY.
           MOVE 1 TO OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING GAME
           END-CALL.           

           ADD 1 TO TESTS-RUN
           IF ERR-CODE OF TABLEAU IS EQUAL TO 0
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "ERR-CODE OF TABLEAU <> 0"
           END-IF.


           ADD 1 TO TESTS-RUN
           IF T-COUNT-OF-CARDS OF TABLEAU IS EQUAL TO 0
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "T-COUNT-OF-CARDS OF TABLEAU <> 0"
           END-IF.

           PERFORM VARYING T-STACK-I
              FROM 1 BY 1
              UNTIL T-STACK-I > 7

                   ADD 1 TO TESTS-RUN
                   IF COUNT-OF-CARDS OF T-STACKS-T(T-STACK-I) IS EQUAL
                      TO 0
                      ADD 1 TO TESTS-OK
                   ELSE
                      ADD 1 TO TESTS-NOK
                      DISPLAY
                         "COUNT-OF-CARDS OF T-STACKS-T("
                         WITH NO ADVANCING 
                      DISPLAY T-STACK-I ") <> 0"
                   END-IF

           END-PERFORM.

      ******************************************************************
       03-INIT-FROM-STOCK.
           MOVE 2 TO OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING GAME
           END-CALL.           

           ADD 1 TO TESTS-RUN
           IF ERR-CODE OF TABLEAU IS EQUAL TO 0
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "03-INIT-FROM-STOCK:" WITH NO ADVANCING 
              DISPLAY "ERR-CODE OF TABLEAU <> 0"
           END-IF.

           ADD 1 TO TESTS-RUN
           IF T-COUNT-OF-CARDS OF TABLEAU IS EQUAL TO 28
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "03-INIT-FROM-STOCK:" WITH NO ADVANCING 
              DISPLAY "T-COUNT-OF-CARDS OF TABLEAU="
                 WITH NO ADVANCING
              DISPLAY T-COUNT-OF-CARDS OF TABLEAU
                 WITH NO ADVANCING
              DISPLAY " <> 28"
           END-IF.

           PERFORM VARYING T-STACK-I
              FROM 1 BY 1
              UNTIL T-STACK-I > 7

                   ADD 1 TO TESTS-RUN
                   IF COUNT-OF-CARDS OF T-STACKS-T(T-STACK-I) IS EQUAL
                      TO T-STACK-I
                      ADD 1 TO TESTS-OK
                   ELSE
                      ADD 1 TO TESTS-NOK
                      DISPLAY "03-INIT-FROM-STOCK:" WITH NO ADVANCING 
                      DISPLAY
                         "COUNT-OF-CARDS OF T-STACKS-T("
                         WITH NO ADVANCING 
                      DISPLAY T-STACK-I ")="
                         WITH NO ADVANCING 
                      DISPLAY
                         COUNT-OF-CARDS OF T-STACKS-T(T-STACK-I)
                         WITH NO ADVANCING 
                      DISPLAY " <> " T-STACK-I
                   END-IF

           END-PERFORM.

      ******************************************************************
       04-PUSH-CARD-TO-TABLEAU.
           MOVE RANK-N OF CARD-RANK(1, 10) TO RANK-N OF CARD-IN-SCOPE.
           MOVE SUIT-N OF CARD-SUIT(1, 10) TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 7 TO STACK-I-IN-SCOPE.

           MOVE 3 TO OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING GAME
           END-CALL.           

           ADD 1 TO TESTS-RUN
           IF T-COUNT-OF-CARDS IS EQUAL TO 1
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "04-PUSH-CARD-TO-TABLEAU:" WITH NO ADVANCING 
              DISPLAY
                 "T-COUNT-OF-CARDS="
                 WITH NO ADVANCING 
              DISPLAY
                 T-COUNT-OF-CARDS
                 WITH NO ADVANCING 
              DISPLAY " <> 1"
           END-IF.

           PERFORM VARYING T-STACK-I
              FROM 1 BY 1
              UNTIL T-STACK-I > 6
                   ADD 1 TO TESTS-RUN
                   IF COUNT-OF-CARDS OF T-STACKS-T(T-STACK-I) IS EQUAL
                      TO 0
                      ADD 1 TO TESTS-OK
                   ELSE
                      ADD 1 TO TESTS-NOK
                      DISPLAY "04-PUSH-CARD-TO-TABLEAU:" WITH NO
                         ADVANCING 
                      DISPLAY
                         "COUNT-OF-CARDS OF T-STACKS-T("
                         WITH NO ADVANCING 
                      DISPLAY T-STACK-I ")="
                         WITH NO ADVANCING 
                      DISPLAY
                         COUNT-OF-CARDS OF T-STACKS-T(T-STACK-I)
                         WITH NO ADVANCING 
                      DISPLAY " <> 0"
                   END-IF
           END-PERFORM

           ADD 1 TO TESTS-RUN
           IF COUNT-OF-CARDS OF T-STACKS-T(7) IS EQUAL TO 1
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "04-PUSH-CARD-TO-TABLEAU:" WITH NO ADVANCING 
              DISPLAY
                 "COUNT-OF-CARDS OF T-STACKS-T(5)="
                 WITH NO ADVANCING 
              DISPLAY
                 COUNT-OF-CARDS OF T-STACKS-T(T-STACK-I)
                 WITH NO ADVANCING 
              DISPLAY " <> 1"
           END-IF.

           ADD 1 TO TESTS-RUN
           IF COUNT-OF-CARDS OF T-STACKS-T(7) IS EQUAL TO 1
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "04-PUSH-CARD-TO-TABLEAU:" WITH NO ADVANCING 
              DISPLAY
                 "COUNT-OF-CARDS OF T-STACKS-T(5)="
                 WITH NO ADVANCING 
              DISPLAY
                 COUNT-OF-CARDS OF T-STACKS-T(T-STACK-I)
                 WITH NO ADVANCING 
              DISPLAY " <> 1"
           END-IF.

           ADD 1 TO TESTS-RUN
           IF RANK-N OF CARDS-T(7, 1) IS EQUAL TO
              RANK-N OF CARD-IN-SCOPE
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "04-PUSH-CARD-TO-TABLEAU:" WITH NO ADVANCING 
              DISPLAY
                 "RANK-N OF CARDS-T(7,1)="
                 WITH NO ADVANCING 
              DISPLAY
                 RANK-N OF CARDS-T(7, 1)
                 WITH NO ADVANCING 
              DISPLAY " <> " RANK-N OF CARD-IN-SCOPE
           END-IF

           ADD 1 TO TESTS-RUN
           IF SUIT-N OF CARDS-T(7, 1) IS EQUAL TO
              SUIT-N OF CARD-IN-SCOPE
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "04-PUSH-CARD-TO-TABLEAU:" WITH NO ADVANCING 
              DISPLAY
                 "SUIT-N OF CARDS-T(7,1)="
                 WITH NO ADVANCING 
              DISPLAY
                 SUIT-N OF CARDS-T(7, 1)
                 WITH NO ADVANCING 
              DISPLAY " <> " SUIT-N OF CARD-IN-SCOPE
           END-IF.

      ******************************************************************
       04-POP-FROM-EMPTY-TABLEAU.
           MOVE 4 TO OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING GAME
           END-CALL.           

           ADD 1 TO TESTS-RUN
           IF ERR-CODE OF TABLEAU IS EQUAL TO 1
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "04-POP-CARD-FROM-TABLEAU:" WITH NO ADVANCING 
              DISPLAY
                 "ERR-CODE="
                 WITH NO ADVANCING 
              DISPLAY
                 ERR-CODE OF TABLEAU
                 WITH NO ADVANCING 
              DISPLAY " <> 1"
           END-IF.
       
      ******************************************************************
       05-POP-FROM-EMPTY-STACK.
           MOVE RANK-N OF CARD-RANK(1, 10) TO RANK-N OF CARD-IN-SCOPE.
           MOVE SUIT-N OF CARD-SUIT(1, 10) TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 7 TO STACK-I-IN-SCOPE.

           MOVE 3 TO OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING GAME
           END-CALL.           

           MOVE 1 TO STACK-I-IN-SCOPE.
           MOVE 4 TO OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING GAME
           END-CALL.           

           ADD 1 TO TESTS-RUN
           IF ERR-CODE OF TABLEAU IS EQUAL TO 2
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "05-POP-FROM-EMPTY-STACK:" WITH NO ADVANCING 
              DISPLAY
                 "ERR-CODE="
                 WITH NO ADVANCING 
              DISPLAY
                 ERR-CODE OF TABLEAU
                 WITH NO ADVANCING 
              DISPLAY " <> 2"
           END-IF.

      ******************************************************************
       06-POP-FROM-NON-EMPTY-STACK.
           MOVE RANK-N OF CARD-RANK(1, 10) TO RANK-N OF CARD-IN-SCOPE.
           MOVE SUIT-N OF CARD-SUIT(1, 10) TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 7 TO STACK-I-IN-SCOPE.

           MOVE 3 TO OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING GAME
           END-CALL.           

           MOVE 0 TO RANK-N OF CARD-IN-SCOPE.
           MOVE 0 TO SUIT-N OF CARD-IN-SCOPE.

           MOVE 7 TO STACK-I-IN-SCOPE.
           MOVE 4 TO OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING GAME
           END-CALL.           

           ADD 1 TO TESTS-RUN
           IF ERR-CODE OF TABLEAU IS EQUAL TO 0
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "06-POP-FROM-NON-EMPTY-STACK:" WITH NO ADVANCING 
              DISPLAY
                 "ERR-CODE="
                 WITH NO ADVANCING 
              DISPLAY
                 ERR-CODE OF TABLEAU
                 WITH NO ADVANCING 
              DISPLAY " <> 0"
           END-IF.

           PERFORM VARYING T-STACK-I
              FROM 1 BY 1
              UNTIL T-STACK-I > 7
                   ADD 1 TO TESTS-RUN
                   IF COUNT-OF-CARDS OF T-STACKS-T(T-STACK-I) IS EQUAL
                      TO 0
                      ADD 1 TO TESTS-OK
                   ELSE
                      ADD 1 TO TESTS-NOK
                      DISPLAY "06-POP-FROM-NON-EMPTY-STACK:" WITH NO
                         ADVANCING 
                      DISPLAY
                         "COUNT-OF-CARDS OF T-STACKS-T("
                         WITH NO ADVANCING 
                      DISPLAY T-STACK-I ")="
                         WITH NO ADVANCING 
                      DISPLAY
                         COUNT-OF-CARDS OF T-STACKS-T(T-STACK-I)
                         WITH NO ADVANCING 
                      DISPLAY " <> 0"
                   END-IF
           END-PERFORM

           ADD 1 TO TESTS-RUN
           IF RANK-N OF CARD-IN-SCOPE IS EQUAL
              TO RANK-N OF CARD-RANK(1, 10)
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "06-POP-FROM-NON-EMPTY-STACK:" WITH NO
                 ADVANCING 
              DISPLAY
                 "RANK-N OF CARD-IN-SCOPE="
                 WITH NO ADVANCING 
              DISPLAY RANK-N OF CARD-IN-SCOPE
                 WITH NO ADVANCING 
              DISPLAY " <> " RANK-N OF CARD-RANK(1, 10)
           END-IF

           ADD 1 TO TESTS-RUN
           IF SUIT-N OF CARD-IN-SCOPE IS EQUAL
              TO SUIT-N OF CARD-SUIT(1, 10)
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "06-POP-FROM-NON-EMPTY-STACK:" WITH NO
                 ADVANCING 
              DISPLAY
                 "SUIT-N OF CARD-IN-SCOPE="
                 WITH NO ADVANCING 
              DISPLAY SUIT-N OF CARD-IN-SCOPE
                 WITH NO ADVANCING 
              DISPLAY " <> " SUIT-N OF CARD-SUIT(1, 10)
           END-IF.

      ******************************************************************
       07-MANDATORY-EMPTY-TABLEAU.
           MOVE 5 TO OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING GAME
           END-CALL.           

           ADD 1 TO TESTS-RUN
           IF ERR-CODE OF TABLEAU IS EQUAL TO 1
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "07-MANDATORY-EMPTY-TABLEAU:" WITH NO ADVANCING 
              DISPLAY
                 "ERR-CODE="
                 WITH NO ADVANCING 
              DISPLAY
                 ERR-CODE OF TABLEAU
                 WITH NO ADVANCING 
              DISPLAY " <> 1"
           END-IF.

      ******************************************************************
       08-MANDATORY-NONEMPTY-NOMATCH.
      *    FILL TABLEAU WITH SOME CARDS
           MOVE RANK-N OF CARD-RANK(1, 10) TO RANK-N OF CARD-IN-SCOPE.
           MOVE SUIT-N OF CARD-SUIT(1, 10) TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 7 TO STACK-I-IN-SCOPE.
           MOVE 3 TO OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING GAME
           END-CALL.           

           MOVE RANK-N OF CARD-RANK(2, 3) TO RANK-N OF CARD-IN-SCOPE.
           MOVE SUIT-N OF CARD-SUIT(2, 3) TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 5 TO STACK-I-IN-SCOPE.
           MOVE 3 TO OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING GAME
           END-CALL.           

           MOVE RANK-N OF CARD-RANK(3, 8) TO RANK-N OF CARD-IN-SCOPE.
           MOVE SUIT-N OF CARD-SUIT(3, 8) TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 3 TO STACK-I-IN-SCOPE.
           MOVE 3 TO OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING GAME
           END-CALL.           

           MOVE RANK-N OF CARD-RANK(4, 9) TO RANK-N OF CARD-IN-SCOPE.
           MOVE SUIT-N OF CARD-SUIT(4, 9) TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 1 TO STACK-I-IN-SCOPE.
           MOVE 3 TO OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING GAME
           END-CALL.           

      *    CHECK FOR MANDATORY CARD EXISTING
           MOVE RANK-N OF CARD-RANK(2, 1) TO RANK-N OF CARD-IN-SCOPE.
           MOVE SUIT-N OF CARD-SUIT(2, 1) TO SUIT-N OF CARD-IN-SCOPE.

           MOVE 5 TO OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING GAME
           END-CALL.           

           ADD 1 TO TESTS-RUN
           IF ERR-CODE OF TABLEAU IS EQUAL TO 2
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "08-MANDATORY-NONEMPTY-NOMATCH:"
                 WITH NO ADVANCING 
              DISPLAY
                 "ERR-CODE="
                 WITH NO ADVANCING 
              DISPLAY
                 ERR-CODE OF TABLEAU
                 WITH NO ADVANCING 
              DISPLAY " <> 2"
           END-IF.

      ******************************************************************
       09-MANDATORY-NONEMPTY-MATCH1.
      *    FILL TABLEAU WITH SOME CARDS
           MOVE RANK-N OF CARD-RANK(2, 1) TO RANK-N OF CARD-IN-SCOPE.
           MOVE SUIT-N OF CARD-SUIT(2, 1) TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 7 TO STACK-I-IN-SCOPE.
           MOVE 3 TO OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING GAME
           END-CALL.           

           MOVE RANK-N OF CARD-RANK(2, 3) TO RANK-N OF CARD-IN-SCOPE.
           MOVE SUIT-N OF CARD-SUIT(2, 3) TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 5 TO STACK-I-IN-SCOPE.
           MOVE 3 TO OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING GAME
           END-CALL.           

           MOVE RANK-N OF CARD-RANK(3, 8) TO RANK-N OF CARD-IN-SCOPE.
           MOVE SUIT-N OF CARD-SUIT(3, 8) TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 3 TO STACK-I-IN-SCOPE.
           MOVE 3 TO OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING GAME
           END-CALL.           

           MOVE RANK-N OF CARD-RANK(4, 9) TO RANK-N OF CARD-IN-SCOPE.
           MOVE SUIT-N OF CARD-SUIT(4, 9) TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 1 TO STACK-I-IN-SCOPE.
           MOVE 3 TO OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING GAME
           END-CALL.           

      *    CHECK FOR MANDATORY CARD EXISTING
           MOVE RANK-N OF CARD-RANK(2, 1) TO RANK-N OF CARD-IN-SCOPE.
           MOVE SUIT-N OF CARD-SUIT(2, 1) TO SUIT-N OF CARD-IN-SCOPE.

           MOVE 5 TO OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING GAME
           END-CALL.           

           ADD 1 TO TESTS-RUN
           IF ERR-CODE OF TABLEAU IS EQUAL TO 0
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "09-MANDATORY-NONEMPTY-MATCH1:"
                 WITH NO ADVANCING 
              DISPLAY
                 "ERR-CODE="
                 WITH NO ADVANCING 
              DISPLAY
                 ERR-CODE OF TABLEAU
                 WITH NO ADVANCING 
              DISPLAY " <> 0"
           END-IF.

           ADD 1 TO TESTS-RUN
           IF STACK-I-IN-SCOPE IS EQUAL TO 7
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "09-MANDATORY-NONEMPTY-MATCH1:"
                 WITH NO ADVANCING 
              DISPLAY
                 "STACK-I-IN-SCOPE="
                 WITH NO ADVANCING 
              DISPLAY
                 STACK-I-IN-SCOPE
                 WITH NO ADVANCING 
              DISPLAY " <> 7"
           END-IF.

      ******************************************************************
       10-MANDATORY-NONEMPTY-MATCH2.
      *    FILL TABLEAU WITH SOME CARDS
           MOVE RANK-N OF CARD-RANK(2, 1) TO RANK-N OF CARD-IN-SCOPE.
           MOVE SUIT-N OF CARD-SUIT(2, 1) TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 7 TO STACK-I-IN-SCOPE.
           MOVE 3 TO OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING GAME
           END-CALL.           

           MOVE RANK-N OF CARD-RANK(2, 3) TO RANK-N OF CARD-IN-SCOPE.
           MOVE SUIT-N OF CARD-SUIT(2, 3) TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 5 TO STACK-I-IN-SCOPE.
           MOVE 3 TO OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING GAME
           END-CALL.           

           MOVE RANK-N OF CARD-RANK(3, 8) TO RANK-N OF CARD-IN-SCOPE.
           MOVE SUIT-N OF CARD-SUIT(3, 8) TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 3 TO STACK-I-IN-SCOPE.
           MOVE 3 TO OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING GAME
           END-CALL.           

           MOVE RANK-N OF CARD-RANK(4, 9) TO RANK-N OF CARD-IN-SCOPE.
           MOVE SUIT-N OF CARD-SUIT(4, 9) TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 1 TO STACK-I-IN-SCOPE.
           MOVE 3 TO OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING GAME
           END-CALL.           

      *    CHECK FOR MANDATORY 1. CARD EXISTING
           MOVE RANK-N OF CARD-RANK(2, 1) TO RANK-N OF CARD-IN-SCOPE.
           MOVE SUIT-N OF CARD-SUIT(2, 1) TO SUIT-N OF CARD-IN-SCOPE.

           MOVE 5 TO OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING GAME
           END-CALL.           

           ADD 1 TO TESTS-RUN
           IF ERR-CODE OF TABLEAU IS EQUAL TO 0
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "10-MANDATORY-NONEMPTY-MATCH2:"
                 WITH NO ADVANCING 
              DISPLAY
                 "ERR-CODE(1)="
                 WITH NO ADVANCING 
              DISPLAY
                 ERR-CODE OF TABLEAU
                 WITH NO ADVANCING 
              DISPLAY " <> 0"
           END-IF.

           ADD 1 TO TESTS-RUN
           IF STACK-I-IN-SCOPE IS EQUAL TO 7
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "10-MANDATORY-NONEMPTY-MATCH2:"
                 WITH NO ADVANCING 
              DISPLAY
                 "STACK-I-IN-SCOPE(1)="
                 WITH NO ADVANCING 
              DISPLAY
                 STACK-I-IN-SCOPE
                 WITH NO ADVANCING 
              DISPLAY " <> 7"
           END-IF.

      *    CHECK FOR MANDATORY 2. CARD EXISTING
           MOVE RANK-N OF CARD-RANK(3, 8) TO RANK-N OF CARD-IN-SCOPE.
           MOVE SUIT-N OF CARD-SUIT(3, 8) TO SUIT-N OF CARD-IN-SCOPE.

           MOVE 5 TO OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING GAME
           END-CALL.           

           ADD 1 TO TESTS-RUN
           IF ERR-CODE OF TABLEAU IS EQUAL TO 0
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "10-MANDATORY-NONEMPTY-MATCH2:"
                 WITH NO ADVANCING 
              DISPLAY
                 "ERR-CODE(2)="
                 WITH NO ADVANCING 
              DISPLAY
                 ERR-CODE OF TABLEAU
                 WITH NO ADVANCING 
              DISPLAY " <> 0"
           END-IF.

           ADD 1 TO TESTS-RUN
           IF STACK-I-IN-SCOPE IS EQUAL TO 3
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "10-MANDATORY-NONEMPTY-MATCH2:"
                 WITH NO ADVANCING 
              DISPLAY
                 "STACK-I-IN-SCOPE(2)="
                 WITH NO ADVANCING 
              DISPLAY
                 STACK-I-IN-SCOPE
                 WITH NO ADVANCING 
              DISPLAY " <> 3"
           END-IF.

      ******************************************************************
       11-MANDATORY-NONEMPTY-MATCH3.
      *    FILL TABLEAU WITH SOME CARDS
           MOVE RANK-N OF CARD-RANK(2, 1) TO RANK-N OF CARD-IN-SCOPE.
           MOVE SUIT-N OF CARD-SUIT(2, 1) TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 7 TO STACK-I-IN-SCOPE.
           MOVE 3 TO OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING GAME
           END-CALL.           

           MOVE RANK-N OF CARD-RANK(2, 3) TO RANK-N OF CARD-IN-SCOPE.
           MOVE SUIT-N OF CARD-SUIT(2, 3) TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 5 TO STACK-I-IN-SCOPE.
           MOVE 3 TO OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING GAME
           END-CALL.           

           MOVE RANK-N OF CARD-RANK(3, 8) TO RANK-N OF CARD-IN-SCOPE.
           MOVE SUIT-N OF CARD-SUIT(3, 8) TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 3 TO STACK-I-IN-SCOPE.
           MOVE 3 TO OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING GAME
           END-CALL.           

           MOVE RANK-N OF CARD-RANK(4, 9) TO RANK-N OF CARD-IN-SCOPE.
           MOVE SUIT-N OF CARD-SUIT(4, 9) TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 1 TO STACK-I-IN-SCOPE.
           MOVE 3 TO OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING GAME
           END-CALL.           

      *    CHECK FOR MANDATORY 1. CARD EXISTING
           MOVE RANK-N OF CARD-RANK(2, 1) TO RANK-N OF CARD-IN-SCOPE.
           MOVE SUIT-N OF CARD-SUIT(2, 1) TO SUIT-N OF CARD-IN-SCOPE.

           MOVE 5 TO OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING GAME
           END-CALL.           

           ADD 1 TO TESTS-RUN
           IF ERR-CODE OF TABLEAU IS EQUAL TO 0
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "11-MANDATORY-NONEMPTY-MATCH3:"
                 WITH NO ADVANCING 
              DISPLAY
                 "ERR-CODE(1)="
                 WITH NO ADVANCING 
              DISPLAY
                 ERR-CODE OF TABLEAU
                 WITH NO ADVANCING 
              DISPLAY " <> 0"
           END-IF.

           ADD 1 TO TESTS-RUN
           IF STACK-I-IN-SCOPE IS EQUAL TO 7
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "11-MANDATORY-NONEMPTY-MATCH3:"
                 WITH NO ADVANCING 
              DISPLAY
                 "STACK-I-IN-SCOPE(1)="
                 WITH NO ADVANCING 
              DISPLAY
                 STACK-I-IN-SCOPE
                 WITH NO ADVANCING 
              DISPLAY " <> 7"
           END-IF.

      *    CHECK FOR MANDATORY 2. CARD EXISTING
           MOVE RANK-N OF CARD-RANK(3, 8) TO RANK-N OF CARD-IN-SCOPE.
           MOVE SUIT-N OF CARD-SUIT(3, 8) TO SUIT-N OF CARD-IN-SCOPE.

           MOVE 5 TO OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING GAME
           END-CALL.           

           ADD 1 TO TESTS-RUN
           IF ERR-CODE OF TABLEAU IS EQUAL TO 0
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "11-MANDATORY-NONEMPTY-MATCH3:"
                 WITH NO ADVANCING 
              DISPLAY
                 "ERR-CODE(2)="
                 WITH NO ADVANCING 
              DISPLAY
                 ERR-CODE OF TABLEAU
                 WITH NO ADVANCING 
              DISPLAY " <> 0"
           END-IF.

           ADD 1 TO TESTS-RUN
           IF STACK-I-IN-SCOPE IS EQUAL TO 3
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "11-MANDATORY-NONEMPTY-MATCH3:"
                 WITH NO ADVANCING 
              DISPLAY
                 "STACK-I-IN-SCOPE(2)="
                 WITH NO ADVANCING 
              DISPLAY
                 STACK-I-IN-SCOPE
                 WITH NO ADVANCING 
              DISPLAY " <> 3"
           END-IF.

      *    CHECK FOR MANDATORY 3. CARD EXISTING
           MOVE RANK-N OF CARD-RANK(2, 3) TO RANK-N OF CARD-IN-SCOPE.
           MOVE SUIT-N OF CARD-SUIT(2, 3) TO SUIT-N OF CARD-IN-SCOPE.

           MOVE 5 TO OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING GAME
           END-CALL.           

           ADD 1 TO TESTS-RUN
           IF ERR-CODE OF TABLEAU IS EQUAL TO 0
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "11-MANDATORY-NONEMPTY-MATCH3:"
                 WITH NO ADVANCING 
              DISPLAY
                 "ERR-CODE(3)="
                 WITH NO ADVANCING 
              DISPLAY
                 ERR-CODE OF TABLEAU
                 WITH NO ADVANCING 
              DISPLAY " <> 0"
           END-IF.

           ADD 1 TO TESTS-RUN
           IF STACK-I-IN-SCOPE IS EQUAL TO 5
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "11-MANDATORY-NONEMPTY-MATCH3:"
                 WITH NO ADVANCING 
              DISPLAY
                 "STACK-I-IN-SCOPE(3)="
                 WITH NO ADVANCING 
              DISPLAY
                 STACK-I-IN-SCOPE
                 WITH NO ADVANCING 
              DISPLAY " <> 5"
           END-IF.

      ******************************************************************
       12-MANDATORY-NONEMPTY-MATCH4.
      *    FILL TABLEAU WITH SOME CARDS
           MOVE RANK-N OF CARD-RANK(2, 1) TO RANK-N OF CARD-IN-SCOPE.
           MOVE SUIT-N OF CARD-SUIT(2, 1) TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 7 TO STACK-I-IN-SCOPE.
           MOVE 3 TO OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING GAME
           END-CALL.           

           MOVE RANK-N OF CARD-RANK(2, 3) TO RANK-N OF CARD-IN-SCOPE.
           MOVE SUIT-N OF CARD-SUIT(2, 3) TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 5 TO STACK-I-IN-SCOPE.
           MOVE 3 TO OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING GAME
           END-CALL.           

           MOVE RANK-N OF CARD-RANK(3, 8) TO RANK-N OF CARD-IN-SCOPE.
           MOVE SUIT-N OF CARD-SUIT(3, 8) TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 3 TO STACK-I-IN-SCOPE.
           MOVE 3 TO OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING GAME
           END-CALL.           

           MOVE RANK-N OF CARD-RANK(4, 9) TO RANK-N OF CARD-IN-SCOPE.
           MOVE SUIT-N OF CARD-SUIT(4, 9) TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 1 TO STACK-I-IN-SCOPE.
           MOVE 3 TO OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING GAME
           END-CALL.           

      *    CHECK FOR MANDATORY 1. CARD EXISTING
           MOVE RANK-N OF CARD-RANK(2, 1) TO RANK-N OF CARD-IN-SCOPE.
           MOVE SUIT-N OF CARD-SUIT(2, 1) TO SUIT-N OF CARD-IN-SCOPE.

           MOVE 5 TO OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING GAME
           END-CALL.           

           ADD 1 TO TESTS-RUN
           IF ERR-CODE OF TABLEAU IS EQUAL TO 0
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "12-MANDATORY-NONEMPTY-MATCH4:"
                 WITH NO ADVANCING 
              DISPLAY
                 "ERR-CODE(1)="
                 WITH NO ADVANCING 
              DISPLAY
                 ERR-CODE OF TABLEAU
                 WITH NO ADVANCING 
              DISPLAY " <> 0"
           END-IF.

           ADD 1 TO TESTS-RUN
           IF STACK-I-IN-SCOPE IS EQUAL TO 7
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "12-MANDATORY-NONEMPTY-MATCH4:"
                 WITH NO ADVANCING 
              DISPLAY
                 "STACK-I-IN-SCOPE(1)="
                 WITH NO ADVANCING 
              DISPLAY
                 STACK-I-IN-SCOPE
                 WITH NO ADVANCING 
              DISPLAY " <> 7"
           END-IF.

      *    CHECK FOR MANDATORY 2. CARD EXISTING
           MOVE RANK-N OF CARD-RANK(3, 8) TO RANK-N OF CARD-IN-SCOPE.
           MOVE SUIT-N OF CARD-SUIT(3, 8) TO SUIT-N OF CARD-IN-SCOPE.

           MOVE 5 TO OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING GAME
           END-CALL.           

           ADD 1 TO TESTS-RUN
           IF ERR-CODE OF TABLEAU IS EQUAL TO 0
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "12-MANDATORY-NONEMPTY-MATCH4:"
                 WITH NO ADVANCING 
              DISPLAY
                 "ERR-CODE(2)="
                 WITH NO ADVANCING 
              DISPLAY
                 ERR-CODE OF TABLEAU
                 WITH NO ADVANCING 
              DISPLAY " <> 0"
           END-IF.

           ADD 1 TO TESTS-RUN
           IF STACK-I-IN-SCOPE IS EQUAL TO 3
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "12-MANDATORY-NONEMPTY-MATCH4:"
                 WITH NO ADVANCING 
              DISPLAY
                 "STACK-I-IN-SCOPE(2)="
                 WITH NO ADVANCING 
              DISPLAY
                 STACK-I-IN-SCOPE
                 WITH NO ADVANCING 
              DISPLAY " <> 3"
           END-IF.

      *    CHECK FOR MANDATORY 3. CARD EXISTING
           MOVE RANK-N OF CARD-RANK(2, 3) TO RANK-N OF CARD-IN-SCOPE.
           MOVE SUIT-N OF CARD-SUIT(2, 3) TO SUIT-N OF CARD-IN-SCOPE.

           MOVE 5 TO OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING GAME
           END-CALL.           

           ADD 1 TO TESTS-RUN
           IF ERR-CODE OF TABLEAU IS EQUAL TO 0
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "12-MANDATORY-NONEMPTY-MATCH4:"
                 WITH NO ADVANCING 
              DISPLAY
                 "ERR-CODE(3)="
                 WITH NO ADVANCING 
              DISPLAY
                 ERR-CODE OF TABLEAU
                 WITH NO ADVANCING 
              DISPLAY " <> 0"
           END-IF.

           ADD 1 TO TESTS-RUN
           IF STACK-I-IN-SCOPE IS EQUAL TO 5
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "12-MANDATORY-NONEMPTY-MATCH4:"
                 WITH NO ADVANCING 
              DISPLAY
                 "STACK-I-IN-SCOPE(3)="
                 WITH NO ADVANCING 
              DISPLAY
                 STACK-I-IN-SCOPE
                 WITH NO ADVANCING 
              DISPLAY " <> 5"
           END-IF.

      *    CHECK FOR MANDATORY 4. CARD EXISTING
           MOVE RANK-N OF CARD-RANK(4, 9) TO RANK-N OF CARD-IN-SCOPE.
           MOVE SUIT-N OF CARD-SUIT(4, 9) TO SUIT-N OF CARD-IN-SCOPE.

           MOVE 5 TO OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING GAME
           END-CALL.           

           ADD 1 TO TESTS-RUN
           IF ERR-CODE OF TABLEAU IS EQUAL TO 0
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "12-MANDATORY-NONEMPTY-MATCH4:"
                 WITH NO ADVANCING 
              DISPLAY
                 "ERR-CODE(3)="
                 WITH NO ADVANCING 
              DISPLAY
                 ERR-CODE OF TABLEAU
                 WITH NO ADVANCING 
              DISPLAY " <> 0"
           END-IF.

           ADD 1 TO TESTS-RUN
           IF STACK-I-IN-SCOPE IS EQUAL TO 1
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "12-MANDATORY-NONEMPTY-MATCH4:"
                 WITH NO ADVANCING 
              DISPLAY
                 "STACK-I-IN-SCOPE(3)="
                 WITH NO ADVANCING 
              DISPLAY
                 STACK-I-IN-SCOPE
                 WITH NO ADVANCING 
              DISPLAY " <> 5"
           END-IF.

      ******************************************************************
       13-MANDATORY-NONEMPTY-NOT-TOS.
      *    FILL TABLEAU WITH SOME CARDS
           MOVE RANK-N OF CARD-RANK(2, 1) TO RANK-N OF CARD-IN-SCOPE.
           MOVE SUIT-N OF CARD-SUIT(2, 1) TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 7 TO STACK-I-IN-SCOPE.
           MOVE 3 TO OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING GAME
           END-CALL.           

           MOVE RANK-N OF CARD-RANK(2, 3) TO RANK-N OF CARD-IN-SCOPE.
           MOVE SUIT-N OF CARD-SUIT(2, 3) TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 7 TO STACK-I-IN-SCOPE.
           MOVE 3 TO OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING GAME
           END-CALL.           

           MOVE RANK-N OF CARD-RANK(3, 8) TO RANK-N OF CARD-IN-SCOPE.
           MOVE SUIT-N OF CARD-SUIT(3, 8) TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 3 TO STACK-I-IN-SCOPE.
           MOVE 3 TO OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING GAME
           END-CALL.           

           MOVE RANK-N OF CARD-RANK(4, 9) TO RANK-N OF CARD-IN-SCOPE.
           MOVE SUIT-N OF CARD-SUIT(4, 9) TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 1 TO STACK-I-IN-SCOPE.
           MOVE 3 TO OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING GAME
           END-CALL.           

      *    CHECK FOR MANDATORY CARD EXISTING, BUT NOT TOS
           MOVE RANK-N OF CARD-RANK(2, 1) TO RANK-N OF CARD-IN-SCOPE.
           MOVE SUIT-N OF CARD-SUIT(2, 1) TO SUIT-N OF CARD-IN-SCOPE.

           MOVE 5 TO OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING GAME
           END-CALL.           

           ADD 1 TO TESTS-RUN
           IF ERR-CODE OF TABLEAU IS EQUAL TO 2
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "13-MANDATORY-NONEMPTY-NOT-TOS:"
                 WITH NO ADVANCING 
              DISPLAY
                 "ERR-CODE(1)="
                 WITH NO ADVANCING 
              DISPLAY
                 ERR-CODE OF TABLEAU
                 WITH NO ADVANCING 
              DISPLAY " <> 2"
           END-IF.