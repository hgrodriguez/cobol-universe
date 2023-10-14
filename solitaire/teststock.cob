       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTSTOCK.

       DATA DIVISION. 

       WORKING-STORAGE SECTION.

      ******************************************************************
      * VARIABLES FOR THE TEST RUN
       01 CARDS-S-I               PIC 9.
       01 CARDS-R-I               PIC 99.
       01 EXPECTED-RANK-A         PIC X.
       01 EXPECTED-SUIT-A         PIC X.
       01 T-I-P                   PIC 9.
       01 T-RANK-N                PIC 99.
       01 T-SUIT-N                PIC 9.
       01 T-COUNTER               PIC 99.
       01 TESTS-RUN               PIC 999 VALUE 0.
       01 TESTS-OK                PIC 999 VALUE 0.
       01 TESTS-NOK               PIC 999 VALUE 0.
       01 TESTS-RANK              PIC 999 VALUE 1.
       01 T-COUNTER-CHECK         PIC 99.
      *   THIS IS THE CARDS UNIVERSE TO TEST THE RANDOMIZED STACK
       01 T-CARDS.
          04 T-CARDS-SUIT-T OCCURS 4 TIMES INDEXED BY T-CARDS-S-I.
             05 T-CARDS-RANK-T OCCURS 13 TIMES INDEXED BY T-CARDS-R-I.
      *            IS PRESENT: 0=NO, 1=YES
                07 T-CARDS-I-P    PIC 9.

       01 CARDS.
      *      THE REQUES-RESPONSE-BLOCK
          03 REQ-RSP-BLOCK.
      *            THE OPERATION REQUESTED TO BE PERFORMED
      *            1 = INITIALIZE CARDS
             04 REQ-OP-CODE       PIC 9.
      *            RANK NUMBER
             04 REQ-RANK-N        PIC 99.
      *            SUIT NUMBER
             04 REQ-SUIT-N        PIC 9.
      *            THE ERROR CODE, IF ANY, FOR THE REQUESTED OPERATION
      *            1 = ILLEGAL OP
      *            2 = ILLEGAL RANK: LOWER THAN MIN
      *            3 = ILLEGAL RANK: HIGHER THAN MAX
      *            4 = ILLEGAL SUIT: LOWER THAN MIN
      *            5 = ILLEGAL SUIT: HIGHER THAN MAX
             04 RSP-ERR-CODE      PIC 99.
      *            RANK ALPHA CODE OF REQUESTED RANK NUMBER
             04 RSP-RANK-A        PIC X.
      *            SUIT ALPHA CODE OF REQUESTED SUIT NUMBER
             04 RSP-SUIT-A        PIC X.

      ******************************************************************
      *      DEFINES THE STOCK OF THE GAME
       01 STOCK.
          03 REQ-RSP-BLOCK.
      *         THE OPERATION REQUESTED TO BE PERFORMED ON THE STOCK
      *         01 -> FILL-STOCK
      *         02 -> RANDOMIZE-STOCK
      *         03 -> FETCH-CARD
      *         04 -> TOGGLE-PEEK
      *         05 -> PRINT-TOS
      *         06 -> RETURN-NUM-CARDS
      *         99 -> PRINT-STOCK
             04 REQ-OP-CODE       PIC 99.
             04 REQ-CARD-INDEX    PIC 99.
      *      THE ERROR CODE, IF ANY, FOR THE REQUESTED OPERATION
      *            1 = ILLEGAL OP CODE
      *            2 = NO CARDS LEFT
             04 RSP-ERR-CODE      PIC 99.
      *      THE CARD FETCHED FROM THE STOCK
             04 RSP-CARD-FETCHED.
                05 RSP-RANK-N     PIC 99.
                05 RSP-SUIT-N     PIC 9.
      *         TOP OF STOCK PRINT REPRESENTATION
             04 RSP-TOS-PEEK      PIC 9.
             04 RSP-TOS-RANK-A    PIC X.
             04 RSP-TOS-SUIT-A    PIC X.
             04 RSP-NUM-OF-CARDS  PIC 99.

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
           PERFORM 20-TOGGLE-PEEK.

           PERFORM 01-STOCK-RESET.
           PERFORM 21-PRINT-NO-PEEK.

           PERFORM 01-STOCK-RESET.
           PERFORM 22-PRINT-PEEK.

           PERFORM 01-STOCK-RESET.
           PERFORM 23-PRINT-EMPTY-STOCK.

           PERFORM 01-STOCK-RESET.
           PERFORM 30-ILLEGAL-OP-CODE

           PERFORM 01-STOCK-RESET.
           PERFORM 31-ILLEGAL-CARD-INDEX

           DISPLAY "TESTS RUN: " TESTS-RUN
           DISPLAY "SUCCESSFUL / FAILED: " TESTS-OK " / " TESTS-NOK
           GOBACK.

      ******************************************************************
       01-TEST-FILL-STOCK.
           MOVE 1 TO REQ-OP-CODE OF STOCK.
           CALL 'STOCK' USING REQ-RSP-BLOCK OF STOCK
           END-CALL.           

           ADD 1 TO TESTS-RUN
           IF RSP-TOS-PEEK IS EQUAL TO 0
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "01-TEST-FILL-STOCK:" WITH NO ADVANCING 
              DISPLAY "TOS-PEEK=" RSP-TOS-PEEK WITH NO ADVANCING 
              DISPLAY " <> 0"
           END-IF


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
                          
                           MOVE CARDS-R-I TO T-RANK-N

                           MOVE 7 TO REQ-OP-CODE OF STOCK
                           MOVE T-COUNTER TO REQ-CARD-INDEX OF STOCK 
                           CALL 'STOCK' USING REQ-RSP-BLOCK OF STOCK
                           END-CALL
                           ADD 1 TO TESTS-RUN
                           IF RSP-RANK-N OF RSP-CARD-FETCHED
                              IS EQUAL TO T-RANK-N
                              ADD 1 TO TESTS-OK
                           ELSE
                              ADD 1 TO TESTS-NOK
                              DISPLAY "01-TEST-FILL-STOCK:"
                                 WITH NO ADVANCING 
                              DISPLAY "RANK-N="
                                      RSP-RANK-N OF RSP-CARD-FETCHED
                                 WITH NO ADVANCING 
                              DISPLAY " <> T-RANK-N="
                                 WITH NO ADVANCING 
                              DISPLAY T-RANK-N " ,"
                                 WITH NO ADVANCING
                              DISPLAY "T-COUNTER=" T-COUNTER 
                           END-IF

                           MOVE CARDS-S-I TO T-SUIT-N

                           ADD 1 TO TESTS-RUN
                           IF RSP-SUIT-N OF RSP-CARD-FETCHED
                              IS EQUAL TO T-SUIT-N
                              ADD 1 TO TESTS-OK
                           ELSE
                              ADD 1 TO TESTS-NOK
                              DISPLAY "01-TEST-FILL-STOCK:"
                                 WITH NO ADVANCING 
                              DISPLAY "SUIT-N="
                                      RSP-SUIT-N OF RSP-CARD-FETCHED
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

           MOVE 2 TO REQ-OP-CODE OF STOCK.
           CALL 'STOCK' USING REQ-RSP-BLOCK OF STOCK
           END-CALL.           

      *    NOW WE NEED TO TEST THE RANDOMIZED DATA
      *    FIRST, IT MUST STILL BE 52 CARDS
           MOVE 6 TO REQ-OP-CODE OF STOCK.
           CALL 'STOCK' USING REQ-RSP-BLOCK OF STOCK
           END-CALL.           
           ADD 1 TO TESTS-RUN
           IF RSP-NUM-OF-CARDS OF STOCK IS EQUAL TO 52
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "RSP-NUM-OF-CARDS=" WITH NO ADVANCING 
              DISPLAY RSP-NUM-OF-CARDS OF STOCK 
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
                   MOVE 7 TO REQ-OP-CODE OF STOCK
                   MOVE T-COUNTER TO REQ-CARD-INDEX OF STOCK 
                   CALL 'STOCK' USING REQ-RSP-BLOCK OF STOCK
                   END-CALL
                   MOVE RSP-RANK-N OF RSP-CARD-FETCHED TO T-RANK-N 
                   MOVE RSP-SUIT-N OF RSP-CARD-FETCHED TO T-SUIT-N 
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
           MOVE 3 TO REQ-OP-CODE OF STOCK
           CALL 'STOCK' USING REQ-RSP-BLOCK OF STOCK
           END-CALL

           MOVE 52 TO T-COUNTER-CHECK
           SUBTRACT 1 FROM T-COUNTER-CHECK

           MOVE 6 TO REQ-OP-CODE OF STOCK.
           CALL 'STOCK' USING REQ-RSP-BLOCK OF STOCK
           END-CALL.           
           ADD 1 TO TESTS-RUN
           IF RSP-NUM-OF-CARDS OF STOCK IS EQUAL TO T-COUNTER-CHECK
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "03-FETCH-1-FROM-STOCK"
              DISPLAY "RSP-NUM-OF-CARDS OF STOCK=" WITH NO
                 ADVANCING 
              DISPLAY RSP-NUM-OF-CARDS OF STOCK WITH NO ADVANCING 
              DISPLAY " <> T-COUNTER-CHECK=" WITH NO ADVANCING 
              DISPLAY T-COUNTER-CHECK
           END-IF.

      *    STORE RANK/SUIT OF FETCHED CARD
           MOVE RSP-RANK-N OF RSP-CARD-FETCHED TO T-RANK-N
           MOVE RSP-SUIT-N OF RSP-CARD-FETCHED TO T-SUIT-N
      *    FETCH TOS CARD
           MOVE 7 TO REQ-OP-CODE OF STOCK.
           MOVE 52 TO REQ-CARD-INDEX OF STOCK.
           CALL 'STOCK' USING REQ-RSP-BLOCK OF STOCK
           END-CALL.           
      *    CHECK RANK
           ADD 1 TO TESTS-RUN
           IF T-RANK-N IS EQUAL TO RSP-RANK-N OF RSP-CARD-FETCHED
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "03-FETCH-1-FROM-STOCK"
              DISPLAY "T-RANK-N=" WITH NO
                 ADVANCING 
              DISPLAY T-RANK-N WITH NO ADVANCING 
              DISPLAY " <> RANK-N=" WITH NO ADVANCING 
              DISPLAY RSP-RANK-N OF RSP-CARD-FETCHED
           END-IF
      *    CHECK SUIT
           ADD 1 TO TESTS-RUN
           IF T-SUIT-N IS EQUAL TO RSP-SUIT-N OF RSP-CARD-FETCHED
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "03-FETCH-1-FROM-STOCK"
              DISPLAY "T-SUIT-N=" WITH NO
                 ADVANCING 
              DISPLAY T-SUIT-N WITH NO ADVANCING 
              DISPLAY " <> SUIT-N=" WITH NO ADVANCING 
              DISPLAY RSP-SUIT-N OF RSP-CARD-FETCHED
           END-IF.

      ******************************************************************
       04-FETCH-ALL-FROM-STOCK.
           PERFORM VARYING T-COUNTER
              FROM 1 BY 1
              UNTIL T-COUNTER IS GREATER THAN 52
           
                   MOVE 3 TO REQ-OP-CODE OF STOCK
                   CALL 'STOCK' USING REQ-RSP-BLOCK OF STOCK
                   END-CALL

           END-PERFORM.

           MOVE 6 TO REQ-OP-CODE OF STOCK.
           CALL 'STOCK' USING REQ-RSP-BLOCK OF STOCK
           END-CALL.           
           ADD 1 TO TESTS-RUN
           IF RSP-NUM-OF-CARDS OF STOCK IS EQUAL TO 0
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "04-FETCH-ALL-FROM-STOCK"
              DISPLAY "RSP-NUM-OF-CARDS OF STOCK=" WITH NO
                 ADVANCING 
              DISPLAY RSP-NUM-OF-CARDS OF STOCK WITH NO ADVANCING 
              DISPLAY " <> 0"
           END-IF.

           ADD 1 TO TESTS-RUN
           IF RSP-ERR-CODE OF STOCK IS EQUAL TO 0
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "04-FETCH-ALL-FROM-STOCK"
              DISPLAY "ERR-CODE OF STOCK=" WITH NO
                 ADVANCING 
              DISPLAY RSP-ERR-CODE OF STOCK WITH NO ADVANCING 
              DISPLAY " <> 0"
           END-IF.

      ******************************************************************
       05-FETCH-1-2-MANY-FROM-STOCK.
           PERFORM VARYING T-COUNTER
              FROM 1 BY 1
              UNTIL T-COUNTER IS GREATER THAN 53
           
                   MOVE 3 TO REQ-OP-CODE OF STOCK
                   CALL 'STOCK' USING REQ-RSP-BLOCK OF STOCK
                   END-CALL

           END-PERFORM.

           ADD 1 TO TESTS-RUN
           IF RSP-ERR-CODE OF STOCK IS EQUAL TO 2
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "05-FETCH-1-2-MANY-FROM-STOCK:" WITH NO ADVANCING 
              DISPLAY "ERR-CODE OF STOCK=" WITH NO
                 ADVANCING 
              DISPLAY RSP-ERR-CODE OF STOCK WITH NO ADVANCING 
              DISPLAY " <> 2"
           END-IF.

           MOVE 6 TO REQ-OP-CODE OF STOCK.
           CALL 'STOCK' USING REQ-RSP-BLOCK OF STOCK
           END-CALL.           
           ADD 1 TO TESTS-RUN
           IF RSP-NUM-OF-CARDS OF STOCK IS EQUAL TO 0
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "05-FETCH-1-2-MANY-FROM-STOCK:" WITH NO ADVANCING 
              DISPLAY "RSP-NUM-OF-CARDS OF STOCK=" WITH NO
                 ADVANCING 
              DISPLAY RSP-NUM-OF-CARDS OF STOCK WITH NO ADVANCING 
              DISPLAY " <> 0"
           END-IF.


      ******************************************************************
       06-FETCH-28-TO-FILL-TABLEAU.
           PERFORM VARYING T-COUNTER
              FROM 1 BY 1
              UNTIL T-COUNTER IS GREATER THAN 28
           
                   MOVE 3 TO REQ-OP-CODE OF STOCK
                   CALL 'STOCK' USING REQ-RSP-BLOCK OF STOCK
                   END-CALL

           END-PERFORM.
           MOVE 52 TO T-COUNTER-CHECK
           SUBTRACT 28 FROM T-COUNTER-CHECK

           MOVE 6 TO REQ-OP-CODE OF STOCK.
           CALL 'STOCK' USING REQ-RSP-BLOCK OF STOCK
           END-CALL.           
           ADD 1 TO TESTS-RUN
           IF RSP-NUM-OF-CARDS OF STOCK IS EQUAL TO T-COUNTER-CHECK
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "04-FETCH-28-TO-FILL-TABLEAU"
              DISPLAY "RSP-NUM-OF-CARDS OF STOCK=" WITH NO
                 ADVANCING 
              DISPLAY RSP-NUM-OF-CARDS OF STOCK WITH NO ADVANCING 
              DISPLAY " <> T-COUNTER-CHECK=" WITH NO ADVANCING 
              DISPLAY T-COUNTER-CHECK
           END-IF.

      ******************************************************************
       20-TOGGLE-PEEK.
           ADD 1 TO TESTS-RUN
           IF RSP-TOS-PEEK IS EQUAL TO 0
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "20-TOGGLE-PEEK:" WITH NO ADVANCING 
              DISPLAY "TOS-PEEK=" RSP-TOS-PEEK WITH NO ADVANCING 
              DISPLAY " <> 0"
           END-IF
           MOVE 4 TO REQ-OP-CODE OF STOCK.
           CALL 'STOCK' USING REQ-RSP-BLOCK OF STOCK
           END-CALL
           ADD 1 TO TESTS-RUN
           IF RSP-TOS-PEEK IS EQUAL TO 1
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "20-TOGGLE-PEEK:" WITH NO ADVANCING 
              DISPLAY "TOS-PEEK=" RSP-TOS-PEEK WITH NO ADVANCING 
              DISPLAY " <> 1"
           END-IF.

      ******************************************************************
       21-PRINT-NO-PEEK.
           MOVE 5 TO REQ-OP-CODE OF STOCK.
           CALL 'STOCK' USING REQ-RSP-BLOCK OF STOCK
           END-CALL
           ADD 1 TO TESTS-RUN
           IF RSP-TOS-RANK-A IS EQUAL TO 'X'
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "22-PRINT-NO-PEEK:" WITH NO ADVANCING 
              DISPLAY "TOS-RANK-A=" RSP-TOS-RANK-A WITH NO ADVANCING 
              DISPLAY " <> X"
           END-IF.
           ADD 1 TO TESTS-RUN
           IF RSP-TOS-SUIT-A IS EQUAL TO 'X'
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "22-PRINT-NO-PEEK:" WITH NO ADVANCING 
              DISPLAY "TOS-SUIT-A=" RSP-TOS-SUIT-A WITH NO ADVANCING 
              DISPLAY " <> X"
           END-IF.

      ******************************************************************
       22-PRINT-PEEK.
      *    FETCH TOS CARD
           MOVE 7 TO REQ-OP-CODE OF STOCK.
           MOVE 52 TO REQ-CARD-INDEX OF STOCK.
           CALL 'STOCK' USING REQ-RSP-BLOCK OF STOCK
           END-CALL

      *    THE CARDS KNOW HOW TO MAP THIS
           MOVE RSP-RANK-N OF RSP-CARD-FETCHED TO REQ-RANK-N OF CARDS
           MOVE RSP-SUIT-N OF RSP-CARD-FETCHED TO REQ-SUIT-N OF CARDS 
           MOVE 2 TO REQ-OP-CODE OF CARDS
           CALL 'CARDS' USING REQ-RSP-BLOCK OF CARDS
           END-CALL

           MOVE RSP-RANK-A OF CARDS TO EXPECTED-RANK-A
           MOVE RSP-SUIT-A OF CARDS TO EXPECTED-SUIT-A

           MOVE 4 TO REQ-OP-CODE OF STOCK.
           CALL 'STOCK' USING REQ-RSP-BLOCK OF STOCK
           END-CALL

           MOVE 5 TO REQ-OP-CODE OF STOCK.
           CALL 'STOCK' USING REQ-RSP-BLOCK OF STOCK
           END-CALL
           ADD 1 TO TESTS-RUN
           IF RSP-TOS-RANK-A IS EQUAL TO EXPECTED-RANK-A
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "22-PRINT-PEEK:" WITH NO ADVANCING 
              DISPLAY "TOS-RANK-A=" RSP-TOS-RANK-A WITH NO ADVANCING 
              DISPLAY " <> " EXPECTED-RANK-A
           END-IF.
           ADD 1 TO TESTS-RUN
           IF RSP-TOS-SUIT-A IS EQUAL TO EXPECTED-SUIT-A
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "22-PRINT-PEEK:" WITH NO ADVANCING 
              DISPLAY "TOS-SUIT-A=" RSP-TOS-SUIT-A WITH NO ADVANCING 
              DISPLAY " <> " EXPECTED-SUIT-A
           END-IF.

      ******************************************************************
       23-PRINT-EMPTY-STOCK.
           PERFORM VARYING T-COUNTER
              FROM 1 BY 1
              UNTIL T-COUNTER IS GREATER THAN 53
                   MOVE 3 TO REQ-OP-CODE OF STOCK
                   CALL 'STOCK' USING REQ-RSP-BLOCK OF STOCK
                   END-CALL
           END-PERFORM.

           MOVE 5 TO REQ-OP-CODE OF STOCK.
           CALL 'STOCK' USING REQ-RSP-BLOCK OF STOCK
           END-CALL
           ADD 1 TO TESTS-RUN
           IF RSP-TOS-RANK-A IS EQUAL TO 'X'
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "22-PRINT-NO-PEEK:" WITH NO ADVANCING 
              DISPLAY "TOS-RANK-A=" RSP-TOS-RANK-A WITH NO ADVANCING 
              DISPLAY " <> X"
           END-IF.
           ADD 1 TO TESTS-RUN
           IF RSP-TOS-SUIT-A IS EQUAL TO 'X'
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "22-PRINT-NO-PEEK:" WITH NO ADVANCING 
              DISPLAY "TOS-SUIT-A=" RSP-TOS-SUIT-A WITH NO ADVANCING 
              DISPLAY " <> X"
           END-IF.

           MOVE 4 TO REQ-OP-CODE OF STOCK.
           CALL 'STOCK' USING REQ-RSP-BLOCK OF STOCK
           END-CALL
           MOVE 5 TO REQ-OP-CODE OF STOCK.
           CALL 'STOCK' USING REQ-RSP-BLOCK OF STOCK
           END-CALL
           ADD 1 TO TESTS-RUN
           IF RSP-TOS-RANK-A IS EQUAL TO 'X'
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "22-PRINT-NO-PEEK:" WITH NO ADVANCING 
              DISPLAY "TOS-RANK-A=" RSP-TOS-RANK-A WITH NO ADVANCING 
              DISPLAY " <> X"
           END-IF.
           ADD 1 TO TESTS-RUN
           IF RSP-TOS-SUIT-A IS EQUAL TO 'X'
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "22-PRINT-NO-PEEK:" WITH NO ADVANCING 
              DISPLAY "TOS-SUIT-A=" RSP-TOS-SUIT-A WITH NO ADVANCING 
              DISPLAY " <> X"
           END-IF.

      ******************************************************************
       30-ILLEGAL-OP-CODE.
           MOVE 0 TO REQ-OP-CODE OF STOCK.
           CALL 'STOCK' USING REQ-RSP-BLOCK OF STOCK
           END-CALL
           ADD 1 TO TESTS-RUN
           IF RSP-ERR-CODE OF STOCK IS EQUAL TO 1
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "30-ILLEGAL-OP-CODE:" WITH NO ADVANCING 
              DISPLAY "RSP-ERROR-CODE=" RSP-ERR-CODE OF STOCK
                 WITH NO ADVANCING 
              DISPLAY " <> 1"
           END-IF.

      ******************************************************************
       31-ILLEGAL-CARD-INDEX.
           MOVE 7 TO REQ-OP-CODE OF STOCK
           MOVE 0 TO REQ-CARD-INDEX
           CALL 'STOCK' USING REQ-RSP-BLOCK OF STOCK
           END-CALL
           ADD 1 TO TESTS-RUN
           IF RSP-ERR-CODE OF STOCK IS EQUAL TO 3
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "31-ILLEGAL-CARD-INDEX:" WITH NO ADVANCING 
              DISPLAY "RSP-ERROR-CODE=" RSP-ERR-CODE OF STOCK
                 WITH NO ADVANCING 
              DISPLAY " <> 3"
           END-IF.

           MOVE 6 TO REQ-OP-CODE OF STOCK.
           CALL 'STOCK' USING REQ-RSP-BLOCK OF STOCK
           END-CALL.           

           MOVE 7 TO REQ-OP-CODE OF STOCK
           ADD 1 TO RSP-NUM-OF-CARDS 
           MOVE RSP-NUM-OF-CARDS TO REQ-CARD-INDEX
           CALL 'STOCK' USING REQ-RSP-BLOCK OF STOCK
           END-CALL
           ADD 1 TO TESTS-RUN
           IF RSP-ERR-CODE OF STOCK IS EQUAL TO 3
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "31-ILLEGAL-CARD-INDEX:" WITH NO ADVANCING 
              DISPLAY "RSP-ERROR-CODE=" RSP-ERR-CODE OF STOCK
                 WITH NO ADVANCING 
              DISPLAY " <> 3"
           END-IF.

      ******************************************************************
       01-STOCK-RESET.
           MOVE 1 TO REQ-OP-CODE OF CARDS
           CALL 'CARDS' USING REQ-RSP-BLOCK OF CARDS
           END-CALL.
           MOVE 1 TO REQ-OP-CODE OF STOCK.
           CALL 'STOCK' USING REQ-RSP-BLOCK OF STOCK
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