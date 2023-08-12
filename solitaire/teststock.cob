       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTSTOCK.

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
       01 T-I-P                    PIC 9.
       01 T-RANK-N                 PIC 99.
       01 T-SUIT-N                 PIC 9.
       01 T-COUNTER                PIC 99.
       01 TESTS-RUN                PIC 999 VALUE 0.
       01 TESTS-OK                 PIC 999 VALUE 0.
       01 TESTS-NOK                PIC 999 VALUE 0.
       01 TESTS-RANK               PIC 999 VALUE 1.
       01 T-COUNTER-CHECK          PIC 99.
      *   THIS IS THE CARDS UNIVERSE TO TEST THE RANDOMIZED STACK
       01 T-CARDS.
          04 T-CARDS-SUIT-T OCCURS 4 TIMES INDEXED BY T-CARDS-S-I.
             05 T-CARDS-RANK-T OCCURS 13 TIMES INDEXED BY T-CARDS-R-I.
      *            IS PRESENT: 0=NO, 1=YES
                07 T-CARDS-I-P     PIC 9.

      ******************************************************************
       PROCEDURE DIVISION.
           DISPLAY "TESTSTOCK"

           PERFORM 01-STOCK-RESET.
           PERFORM 01-TEST-FILL-STOCK.

           PERFORM 01-STOCK-RESET.
           PERFORM 02-TEST-RANDOMIZE-STOCK.

           PERFORM 01-STOCK-RESET.
           PERFORM 03-FETCH-1-FROM-STOCK

           PERFORM 01-STOCK-RESET.
           PERFORM 04-FETCH-ALL-FROM-STOCK

           PERFORM 01-STOCK-RESET.
           PERFORM 05-FETCH-1-2-MANY-FROM-STOCK

           PERFORM 01-STOCK-RESET.
           PERFORM 06-FETCH-28-TO-FILL-TABLEAU

           PERFORM 01-STOCK-RESET.
      *    PERFORM XX-REMOVE FROM- STOCK-7-NEXT

           PERFORM 01-STOCK-RESET.
      *    PERFORM EMPTY-STOCK-AND CHECK IT WAS 52 CARDS

           DISPLAY "TESTS RUN: " TESTS-RUN
           DISPLAY "SUCCESSFUL / FAILED: " TESTS-OK " / " TESTS-NOK
           GOBACK.

      ******************************************************************
       01-TEST-FILL-STOCK.
           MOVE 1 TO OP-CODE OF STOCK.
           CALL 'STOCK' USING GAME
           END-CALL.           


           MOVE 0 TO T-COUNTER.
      *    RUN THROUGH ALL SUITS
           PERFORM VARYING CARDS-S-I
              FROM 1 BY 1
              UNTIL CARDS-S-I > 4
      *            RUN THROUGH ALL RANKS
                   PERFORM VARYING CARDS-R-I
                      FROM 1 BY 1
                      UNTIL CARDS-R-I > 13
                           ADD 1 TO T-COUNTER
                          
                           MOVE RANK-N OF CARDS-RANK-T
                              (CARDS-S-I, CARDS-R-I)
                              TO T-RANK-N
                           ADD 1 TO TESTS-RUN
                           IF RANK-N OF STOCK-T(T-COUNTER)
                              IS EQUAL TO T-RANK-N
                              ADD 1 TO TESTS-OK
                           ELSE
                              ADD 1 TO TESTS-NOK
                              DISPLAY "01-TEST-FILL-STOCK:"
                                 WITH NO ADVANCING 
                              DISPLAY "RANK-N="
                                      RANK-N
                                 OF STOCK-T(T-COUNTER)
                                 WITH NO ADVANCING 
                              DISPLAY " <> T-RANK-N="
                                 WITH NO ADVANCING 
                              DISPLAY T-RANK-N " ,"
                                 WITH NO ADVANCING
                              DISPLAY "T-COUNTER=" T-COUNTER 
                           END-IF

                           MOVE SUIT-N OF CARDS-RANK-T
                              (CARDS-S-I, CARDS-R-I)
                              TO T-SUIT-N
                           ADD 1 TO TESTS-RUN
                           IF SUIT-N OF STOCK-T(T-COUNTER)
                              IS EQUAL TO T-SUIT-N
                              ADD 1 TO TESTS-OK
                           ELSE
                              ADD 1 TO TESTS-NOK
                              DISPLAY "01-TEST-FILL-STOCK:"
                                 WITH NO ADVANCING 
                              DISPLAY "SUIT-N="
                                      SUIT-N
                                 OF STOCK-T(T-COUNTER)
                                 WITH NO ADVANCING 
                              DISPLAY " <> T-SUIT-N="
                                 WITH NO ADVANCING 
                              DISPLAY T-SUIT-N " ,"
                                 WITH NO ADVANCING 
                              DISPLAY "T-COUNTER=" T-COUNTER 
                           END-IF

                   END-PERFORM
           END-PERFORM.


      ******************************************************************
       02-TEST-RANDOMIZE-STOCK.
           PERFORM 02-INIT-TEST-CARD-DATA-SET.

           MOVE 2 TO OP-CODE OF STOCK.
           CALL 'STOCK' USING GAME
           END-CALL.           

      *    NOW WE NEED TO TEST THE RANDOMIZED DATA
      *    FIRST, IT MUST STILL BE 52 CARDS
           ADD 1 TO TESTS-RUN
           IF COUNT-OF-CARDS OF STOCK IS EQUAL TO 52
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "COUNT-OF-CARDS=" WITH NO ADVANCING 
              DISPLAY COUNT-OF-CARDS OF STOCK 
              GOBACK
           END-IF.

      *    NOW IT BECOMES INTERESTING
      *    STRATEGY IS:
      *    WE RUN THROUGH THE WHOLE DECK OF STOCK
      *    WE TEST, IF THE CARD IN THE INDEX WAS ALREADY SEEN
      *       YES -> ERROR
      *    WE MARK THE CARD AS SEEN
           PERFORM VARYING T-COUNTER
              FROM 1 BY 1
              UNTIL T-COUNTER > 52
                   MOVE RANK-N OF STOCK-T(T-COUNTER) TO T-RANK-N 
                   MOVE SUIT-N OF STOCK-T(T-COUNTER) TO T-SUIT-N 
                   MOVE T-CARDS-I-P OF
                      T-CARDS-RANK-T(T-SUIT-N, T-RANK-N) TO T-I-P

                   ADD 1 TO TESTS-RUN
                   IF T-I-P IS EQUAL TO 0
                      ADD 1 TO TESTS-OK
                   ELSE
                      ADD 1 TO TESTS-NOK
                      DISPLAY "SUIT=" WITH NO
                         ADVANCING 
                      DISPLAY T-SUIT-N WITH NO ADVANCING 
                      DISPLAY " , RANK=" WITH NO ADVANCING 
                      DISPLAY T-RANK-N WITH NO ADVANCING 
                      DISPLAY " DOUBLED"
                   END-IF
           END-PERFORM.

      ******************************************************************
       03-FETCH-1-FROM-STOCK.
           MOVE 3 TO OP-CODE OF STOCK
           CALL 'STOCK' USING GAME
           END-CALL

           MOVE 52 TO T-COUNTER-CHECK
           SUBTRACT 1 FROM T-COUNTER-CHECK
           ADD 1 TO TESTS-RUN
           IF COUNT-OF-CARDS OF STOCK IS EQUAL TO T-COUNTER-CHECK
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "03-FETCH-1-FROM-STOCK"
              DISPLAY "COUNT-OF-CARDS OF STOCK=" WITH NO
                 ADVANCING 
              DISPLAY COUNT-OF-CARDS OF STOCK WITH NO ADVANCING 
              DISPLAY " <> T-COUNTER-CHECK=" WITH NO ADVANCING 
              DISPLAY T-COUNTER-CHECK
           END-IF.

           ADD 1 TO TESTS-RUN
           IF CARD-FETCHED OF STOCK IS EQUAL TO STOCK-T(52)
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "03-FETCH-1-FROM-STOCK"
              DISPLAY "CARD-FETCHED=" WITH NO
                 ADVANCING 
              DISPLAY CARD-FETCHED OF STOCK WITH NO ADVANCING 
              DISPLAY " <> STOCK-T(52)=" WITH NO ADVANCING 
              DISPLAY STOCK-T(52)
           END-IF.


      ******************************************************************
       04-FETCH-ALL-FROM-STOCK.
           PERFORM VARYING T-COUNTER
              FROM 1 BY 1
              UNTIL T-COUNTER IS GREATER THAN 52
           
                   MOVE 3 TO OP-CODE OF STOCK
                   CALL 'STOCK' USING GAME
                   END-CALL

           END-PERFORM.

           ADD 1 TO TESTS-RUN
           IF COUNT-OF-CARDS OF STOCK IS EQUAL TO 0
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "04-FETCH-ALL-FROM-STOCK"
              DISPLAY "COUNT-OF-CARDS OF STOCK=" WITH NO
                 ADVANCING 
              DISPLAY COUNT-OF-CARDS OF STOCK WITH NO ADVANCING 
              DISPLAY " <> 0"
           END-IF.

           ADD 1 TO TESTS-RUN
           IF ERR-CODE OF STOCK IS EQUAL TO 0
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "04-FETCH-ALL-FROM-STOCK"
              DISPLAY "ERR-CODE OF STOCK=" WITH NO
                 ADVANCING 
              DISPLAY ERR-CODE OF STOCK WITH NO ADVANCING 
              DISPLAY " <> 0"
           END-IF.

      ******************************************************************
       05-FETCH-1-2-MANY-FROM-STOCK.
           PERFORM VARYING T-COUNTER
              FROM 1 BY 1
              UNTIL T-COUNTER IS GREATER THAN 53
           
                   MOVE 3 TO OP-CODE OF STOCK
                   CALL 'STOCK' USING GAME
                   END-CALL

           END-PERFORM.

           ADD 1 TO TESTS-RUN
           IF COUNT-OF-CARDS OF STOCK IS EQUAL TO 0
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "05-FETCH-1-2-MANY-FROM-STOCK:" WITH NO ADVANCING 
              DISPLAY "COUNT-OF-CARDS OF STOCK=" WITH NO
                 ADVANCING 
              DISPLAY COUNT-OF-CARDS OF STOCK WITH NO ADVANCING 
              DISPLAY " <> 0"
           END-IF.

           ADD 1 TO TESTS-RUN
           IF ERR-CODE OF STOCK IS EQUAL TO 1
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "05-FETCH-1-2-MANY-FROM-STOCK:" WITH NO ADVANCING 
              DISPLAY "ERR-CODE OF STOCK=" WITH NO
                 ADVANCING 
              DISPLAY ERR-CODE OF STOCK WITH NO ADVANCING 
              DISPLAY " <> 1"
           END-IF.

      ******************************************************************
       06-FETCH-28-TO-FILL-TABLEAU.
           PERFORM VARYING T-COUNTER
              FROM 1 BY 1
              UNTIL T-COUNTER IS GREATER THAN 28
           
                   MOVE 3 TO OP-CODE OF STOCK
                   CALL 'STOCK' USING GAME
                   END-CALL

           END-PERFORM.
           MOVE 52 TO T-COUNTER-CHECK
           SUBTRACT 28 FROM T-COUNTER-CHECK
           ADD 1 TO TESTS-RUN
           IF COUNT-OF-CARDS OF STOCK IS EQUAL TO T-COUNTER-CHECK
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "04-FETCH-28-TO-FILL-TABLEAU"
              DISPLAY "COUNT-OF-CARDS OF STOCK=" WITH NO
                 ADVANCING 
              DISPLAY COUNT-OF-CARDS OF STOCK WITH NO ADVANCING 
              DISPLAY " <> T-COUNTER-CHECK=" WITH NO ADVANCING 
              DISPLAY T-COUNTER-CHECK
           END-IF.

      ******************************************************************
       01-STOCK-RESET.
           CALL 'CARDS' USING GAME
           END-CALL.
           MOVE 1 TO OP-CODE OF STOCK.
           CALL 'STOCK' USING GAME
           END-CALL.           

      ******************************************************************
       02-INIT-TEST-CARD-DATA-SET.
      *    INITIALIZE THE TEST CARD DATA SET
      *    RUN THROUGH ALL SUITS
           PERFORM VARYING CARDS-S-I
              FROM 1 BY 1
              UNTIL CARDS-S-I > 4
      *            RUN THROUGH ALL RANKS
                   PERFORM VARYING CARDS-R-I
                      FROM 1 BY 1
                      UNTIL CARDS-R-I > 13
                           MOVE 0 TO T-CARDS-I-P OF
                              T-CARDS-RANK-T(T-CARDS-S-I, T-CARDS-R-I)
                   END-PERFORM
           END-PERFORM.