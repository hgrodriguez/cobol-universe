       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTTABLEAU.

       DATA DIVISION. 

       WORKING-STORAGE SECTION.

      ******************************************************************
       01 STOP-HERE                PIC X   VALUE 'Y'.
       01 T-STACK-I                PIC 9.
      * VARIABLES FOR THE TEST RUN
       01 TESTS-OK                 PIC 999 VALUE 0.
       01 TESTS-NOK                PIC 999 VALUE 0.
       01 TESTS-RUN                PIC 999 VALUE 0.

      ******************************************************************
      *   DEFINES ALL POSSIBLE CARDS IN THE GAME
       01 CARDS.
      *      THE REQUEST-RESPONSE-BLOCK
          03 REQ-RSP-BLOCK.
      *            THE OPERATION REQUESTED TO BE PERFORMED
      *            1 = INITIALIZE CARDS
             04 REQ-OP-CODE        PIC 9.
      *            RANK NUMBER
             04 REQ-RANK-N         PIC 99.
      *            SUIT NUMBER
             04 REQ-SUIT-N         PIC 9.
      *            THE ERROR CODE, IF ANY, FOR THE REQUESTED OPERATION
      *            1 = ILLEGAL OP
      *            2 = ILLEGAL RANK: LOWER THAN MIN
      *            3 = ILLEGAL RANK: HIGHER THAN MAX
      *            4 = ILLEGAL SUIT: LOWER THAN MIN
      *            5 = ILLEGAL SUIT: HIGHER THAN MAX
             04 RSP-ERR-CODE       PIC 99.
      *            RANK ALPHA CODE OF REQUESTED RANK NUMBER
             04 RSP-RANK-A         PIC X.
      *            SUIT ALPHA CODE OF REQUESTED SUIT NUMBER
             04 RSP-SUIT-A         PIC X.
      * STOCK
       01 STOCK.
          03 REQ-RSP-BLOCK.
      *         THE OPERATION REQUESTED TO BE PERFORMED ON THE STOCK
      *         01 -> FILL-STOCK
      *         02 -> RANDOMIZE-STOCK
      *         03 -> FETCH-CARD
      *         04 -> TOGGLE-PEEK
      *         05 -> PRINT-TOS
      *         06 -> RETURN-NUM-CARDS
      *         07 -> RETURN-CARD-INDEX
      *         99 -> PRINT-STOCK
             04 REQ-OP-CODE        PIC 99.
             04 REQ-CARD-INDEX     PIC 99.
      *      THE ERROR CODE, IF ANY, FOR THE REQUESTED OPERATION
      *            1 = ILLEGAL OP CODE
      *            2 = NO CARDS LEFT
             04 RSP-ERR-CODE       PIC 99.
      *      THE CARD FETCHED FROM THE STOCK
             04 RSP-CARD-FETCHED.
                05 RSP-RANK-N      PIC 99.
                05 RSP-SUIT-N      PIC 9.
      *         TOP OF STOCK PRINT REPRESENTATION
             04 RSP-TOS-PEEK       PIC 9.
             04 RSP-TOS-RANK-A     PIC X.
             04 RSP-TOS-SUIT-A     PIC X.
             04 RSP-NUM-OF-CARDS   PIC 99.

      ******************************************************************
      *   DEFINES ALL TABLEAU STACKS OF THE GAME
       01 TABLEAU.
          02 REQ-RSP-BLOCK.
      *      THE OPERATION REQUESTED TO BE PERFORMED ON THE TABLEAU
      *         01 -> RESET
      *         02 -> INIT-FROM-STOCK
      *         03 -> PUSH-TO-STACK
      *         04 -> POP-FROM-STACK
      *         05 -> MANDATORY-CHECK
      *         06 -> MOVE-CARDS
      *         07 -> NUMBER OF CARDS IN TABLEAU
      *         08 -> NUMBER OF CARDS IN REQ STACK
      *         09 -> RETURN CARD FROM (STACK, IDX)
      *         99 -> PRINT
             05 REQ-OP-CODE        PIC 99.
      *         THE STACK-INDEX IN SCOPE FOR THE REQUESTED OPERATION
             05 REQ-STCK-IDX       PIC 9.
      *         THE CARD-INDEX IN SCOPE FOR THE REQUESTED OPERATION
             05 REQ-CARD-IDX       PIC 99.
      *         THE CARD IN SCOPE FOR THE REQUESTED OPERATION
             05 CARD-IN-SCOPE.
                26 RANK-N          PIC 99.
                26 SUIT-N          PIC 9.
      *      THE ERROR CODE, IF ANY, FOR THE REQUESTED OPERATION
             05 RSP-ERR-CODE       PIC 9.
      *         NUMBER OF CARDS IN TABLEAU/STACK REQUESTED
             05 RSP-NUM-CARDS      PIC 99.
      *         WHICH STACK IS TO BE USED FOR MANDATORY CARD MOVE
             05 RSP-MNDT-STCK-IDX  PIC 9.
      *         THE RESPONSE CARD IN SCOPE FOR THE REQUESTED OPERATION
             05 RSP-CARD.
                26 RANK-N          PIC 99.
                26 SUIT-N          PIC 9.
      *         DATA WE NEED FOR MOVING CARDS IN THE TABLEAU
      *         SOURCE STACK INDEX
             05 MV-SRC-ST-I        PIC 9.
      *         SOURCE CARD INDEX IN THE SOURCE STACK INDEX
             05 MV-SRC-CA-I        PIC 99.
      *         DESTINATION STACK INDEX
             05 MV-DST-ST-I        PIC 9.

      ******************************************************************
       PROCEDURE DIVISION.
           DISPLAY "TESTTABLEAU"

           PERFORM 01-RESET
           PERFORM 02-TEST-EMPTY

           PERFORM 01-RESET
           PERFORM 03-INIT-FROM-STOCK

           PERFORM 01-RESET
           PERFORM 04-PUSH-CARD-TO-TABLEAU

           PERFORM 01-RESET
           PERFORM 04-POP-FROM-EMPTY-TABLEAU

           PERFORM 01-RESET
           PERFORM 05-POP-FROM-EMPTY-STACK

           PERFORM 01-RESET
           PERFORM 06-POP-FROM-NON-EMPTY-STACK

           PERFORM 01-RESET
           PERFORM 07-MANDATORY-EMPTY-TABLEAU

           PERFORM 01-RESET
           PERFORM 08-MANDATORY-NONEMPTY-NOMATCH

           PERFORM 01-RESET
           PERFORM 09-MANDATORY-NONEMPTY-MATCH1
      
           PERFORM 01-RESET
           PERFORM 10-MANDATORY-NONEMPTY-MATCH2
      
           PERFORM 01-RESET
           PERFORM 11-MANDATORY-NONEMPTY-MATCH3
      
           PERFORM 01-RESET
           PERFORM 12-MANDATORY-NONEMPTY-MATCH4
      
           PERFORM 01-RESET
           PERFORM 13-MANDATORY-NONEMPTY-NOT-TOS

           PERFORM 01-RESET
           PERFORM 20-MV-SRC-IS-EMPTY

           PERFORM 01-RESET
           PERFORM 21-MV-SRC-OUT-OF-RANGE

           PERFORM 01-RESET
           PERFORM 22-MV-SRC-WRONG-RANK

           PERFORM 01-RESET
           PERFORM 23-MV-SRC-WRONG-SUIT

           PERFORM 01-RESET
           PERFORM 24-MV-SRC-NOT-KING

           PERFORM 01-RESET
           PERFORM 25-MV-SRC-KING-DST-NOT-EMPTY

           PERFORM 01-RESET
           PERFORM 26-MV-OK-SRC-NOT-KING

           PERFORM 01-RESET
           PERFORM 27-MV-OK-SRC-KING

           PERFORM 01-RESET
           PERFORM 30-CHECK1-IDX

           DISPLAY "TESTS RUN: " TESTS-RUN
           DISPLAY "SUCCESSFUL / FAILED: " TESTS-OK " / " TESTS-NOK
           GOBACK.

      ******************************************************************
       01-RESET.
           MOVE 1 TO REQ-OP-CODE OF CARDS.
           CALL 'CARDS' USING CARDS
           END-CALL.
           MOVE 1 TO REQ-OP-CODE OF STOCK.
           CALL 'STOCK' USING STOCK
           END-CALL.
           MOVE 1 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.

      ******************************************************************
       02-TEST-EMPTY.
           MOVE 1 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

           ADD 1 TO TESTS-RUN
           IF RSP-ERR-CODE OF TABLEAU IS EQUAL TO 0
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "ERR-CODE OF TABLEAU <> 0"
           END-IF.


           MOVE 7 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

           ADD 1 TO TESTS-RUN
           IF RSP-NUM-CARDS OF TABLEAU IS EQUAL TO 0
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "FAIL: 02-TEST-EMPTY:"
                 WITH NO ADVANCING 
              DISPLAY " RSP-NUM-CARDS=" RSP-NUM-CARDS 
              DISPLAY " <> 0"
           END-IF.

           PERFORM VARYING T-STACK-I
              FROM 1 BY 1
              UNTIL T-STACK-I > 7

                   MOVE 8 TO REQ-OP-CODE OF TABLEAU
                   MOVE T-STACK-I TO REQ-STCK-IDX OF TABLEAU
                   CALL 'TABLEAU' USING TABLEAU
                   END-CALL
                   ADD 1 TO TESTS-RUN
                   IF RSP-NUM-CARDS OF TABLEAU IS EQUAL TO 0
                      ADD 1 TO TESTS-OK
                   ELSE
                      ADD 1 TO TESTS-NOK
                      DISPLAY
                         "RSP-NUM-CARDS OF TABLEAU("
                         WITH NO ADVANCING 
                      DISPLAY T-STACK-I ") <> 0"
                   END-IF

           END-PERFORM.

      ******************************************************************
       03-INIT-FROM-STOCK.
           
           MOVE 2 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

           ADD 1 TO TESTS-RUN
           IF RSP-ERR-CODE OF TABLEAU IS EQUAL TO 0
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "03-INIT-FROM-STOCK:" WITH NO ADVANCING 
              DISPLAY "ERR-CODE OF TABLEAU <> 0"
           END-IF.


           MOVE 7 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           
           ADD 1 TO TESTS-RUN
           IF RSP-NUM-CARDS OF TABLEAU IS EQUAL TO 28
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "03-INIT-FROM-STOCK:" WITH NO ADVANCING 
              DISPLAY "RSP-NUM-CARDS OF TABLEAU="
                 WITH NO ADVANCING
              DISPLAY RSP-NUM-CARDS OF TABLEAU
                 WITH NO ADVANCING
              DISPLAY " <> 28"
           END-IF.

           PERFORM VARYING T-STACK-I
              FROM 1 BY 1
              UNTIL T-STACK-I > 7

                   MOVE 8 TO REQ-OP-CODE OF TABLEAU
                   MOVE T-STACK-I TO REQ-STCK-IDX OF TABLEAU
                   CALL 'TABLEAU' USING TABLEAU
                   END-CALL

                   ADD 1 TO TESTS-RUN
                   IF RSP-NUM-CARDS OF TABLEAU IS EQUAL
                      TO T-STACK-I
                      ADD 1 TO TESTS-OK
                   ELSE
                      ADD 1 TO TESTS-NOK
                      DISPLAY "03-INIT-FROM-STOCK:" WITH NO ADVANCING 
                      DISPLAY
                         "RSP-NUM-CARDS OF TABLEAU("
                         WITH NO ADVANCING 
                      DISPLAY T-STACK-I ")="
                         WITH NO ADVANCING 
                      DISPLAY
                         RSP-NUM-CARDS OF TABLEAU
                         WITH NO ADVANCING 
                      DISPLAY " <> " T-STACK-I
                   END-IF

           END-PERFORM.

      ******************************************************************
       04-PUSH-CARD-TO-TABLEAU.
           MOVE 10 TO RANK-N OF CARD-IN-SCOPE.
           MOVE 1 TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 7 TO REQ-STCK-IDX.
           MOVE 3 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

           MOVE 7 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           
           ADD 1 TO TESTS-RUN
           IF RSP-NUM-CARDS OF TABLEAU IS EQUAL TO 1
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "04-PUSH-CARD-TO-TABLEAU:" WITH NO ADVANCING 
              DISPLAY
                 "RSP-NUM-CARDS OF TABLEAU="
                 WITH NO ADVANCING 
              DISPLAY
                 RSP-NUM-CARDS OF TABLEAU
                 WITH NO ADVANCING 
              DISPLAY " <> 1"
           END-IF.

           PERFORM VARYING T-STACK-I
              FROM 1 BY 1
              UNTIL T-STACK-I > 6
                   MOVE 8 TO REQ-OP-CODE OF TABLEAU
                   MOVE T-STACK-I TO REQ-STCK-IDX OF TABLEAU
                   CALL 'TABLEAU' USING TABLEAU
                   END-CALL
                   ADD 1 TO TESTS-RUN
                   IF RSP-NUM-CARDS OF TABLEAU IS EQUAL TO 0
                      ADD 1 TO TESTS-OK
                   ELSE
                      ADD 1 TO TESTS-NOK
                      DISPLAY "04-PUSH-CARD-TO-TABLEAU:" WITH NO
                         ADVANCING 
                      DISPLAY
                         "RSP-NUM-CARDS OF TABLEAU("
                         WITH NO ADVANCING 
                      DISPLAY T-STACK-I ")="
                         WITH NO ADVANCING 
                      DISPLAY
                         RSP-NUM-CARDS OF TABLEAU
                         WITH NO ADVANCING 
                      DISPLAY " <> 0"
                   END-IF
           END-PERFORM

           MOVE 8 TO REQ-OP-CODE OF TABLEAU
           MOVE 7 TO REQ-STCK-IDX OF TABLEAU
           CALL 'TABLEAU' USING TABLEAU
           END-CALL
           ADD 1 TO TESTS-RUN
           IF RSP-NUM-CARDS OF TABLEAU IS EQUAL TO 1
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "04-PUSH-CARD-TO-TABLEAU:" WITH NO ADVANCING 
              DISPLAY
                 "RSP-NUM-CARDS OF TABLEAU(7)="
                 WITH NO ADVANCING 
              DISPLAY
                 RSP-NUM-CARDS OF TABLEAU
                 WITH NO ADVANCING 
              DISPLAY " <> 1"
           END-IF.

           MOVE 9 TO REQ-OP-CODE OF TABLEAU
           MOVE 7 TO REQ-STCK-IDX OF TABLEAU
           MOVE 1 TO REQ-CARD-IDX OF TABLEAU 
           CALL 'TABLEAU' USING TABLEAU
           END-CALL
           ADD 1 TO TESTS-RUN
           IF RANK-N OF RSP-CARD IS EQUAL TO 10
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "04-PUSH-CARD-TO-TABLEAU:" WITH NO ADVANCING 
              DISPLAY
                 "RANK-N OF RSP-CARD="
                 WITH NO ADVANCING 
              DISPLAY
                 RANK-N OF RSP-CARD
                 WITH NO ADVANCING 
              DISPLAY " <> 10"
           END-IF

           ADD 1 TO TESTS-RUN
           IF SUIT-N OF RSP-CARD IS EQUAL TO 1
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "04-PUSH-CARD-TO-TABLEAU:" WITH NO ADVANCING 
              DISPLAY
                 "SUIT-N OF RSP-CARD="
                 WITH NO ADVANCING 
              DISPLAY
                 SUIT-N OF RSP-CARD
                 WITH NO ADVANCING 
              DISPLAY " <> 1"
           END-IF.

      ******************************************************************
       04-POP-FROM-EMPTY-TABLEAU.
           MOVE 4 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

           ADD 1 TO TESTS-RUN
           IF RSP-ERR-CODE OF TABLEAU IS EQUAL TO 1
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "04-POP-CARD-FROM-TABLEAU:" WITH NO ADVANCING 
              DISPLAY
                 "ERR-CODE="
                 WITH NO ADVANCING 
              DISPLAY
                 RSP-ERR-CODE OF TABLEAU
                 WITH NO ADVANCING 
              DISPLAY " <> 1"
           END-IF.
       
      ******************************************************************
       05-POP-FROM-EMPTY-STACK.
           MOVE 10 TO RANK-N OF CARD-IN-SCOPE.
           MOVE 1 TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 7 TO REQ-STCK-IDX.

           MOVE 3 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

           MOVE 1 TO REQ-STCK-IDX.
           MOVE 4 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

           ADD 1 TO TESTS-RUN
           IF RSP-ERR-CODE OF TABLEAU IS EQUAL TO 2
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "05-POP-FROM-EMPTY-STACK:" WITH NO ADVANCING 
              DISPLAY
                 "ERR-CODE="
                 WITH NO ADVANCING 
              DISPLAY
                 RSP-ERR-CODE OF TABLEAU
                 WITH NO ADVANCING 
              DISPLAY " <> 2"
           END-IF.

      ******************************************************************
       06-POP-FROM-NON-EMPTY-STACK.
           MOVE 10 TO RANK-N OF CARD-IN-SCOPE.
           MOVE 1 TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 7 TO REQ-STCK-IDX.

           MOVE 3 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

           MOVE 0 TO RANK-N OF CARD-IN-SCOPE.
           MOVE 0 TO SUIT-N OF CARD-IN-SCOPE.

           MOVE 7 TO REQ-STCK-IDX.
           MOVE 4 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

           ADD 1 TO TESTS-RUN
           IF RSP-ERR-CODE OF TABLEAU IS EQUAL TO 0
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "06-POP-FROM-NON-EMPTY-STACK:" WITH NO ADVANCING 
              DISPLAY
                 "ERR-CODE="
                 WITH NO ADVANCING 
              DISPLAY
                 RSP-ERR-CODE OF TABLEAU
                 WITH NO ADVANCING 
              DISPLAY " <> 0"
           END-IF.

           PERFORM VARYING T-STACK-I
              FROM 1 BY 1
              UNTIL T-STACK-I > 7

                   MOVE 8 TO REQ-OP-CODE OF TABLEAU
                   MOVE T-STACK-I TO REQ-STCK-IDX OF TABLEAU
                   CALL 'TABLEAU' USING TABLEAU
                   END-CALL

                   ADD 1 TO TESTS-RUN
                   IF RSP-NUM-CARDS OF TABLEAU IS EQUAL
                      TO 0
                      ADD 1 TO TESTS-OK
                   ELSE
                      ADD 1 TO TESTS-NOK
                      DISPLAY "06-POP-FROM-NON-EMPTY-STACK:" WITH NO
                         ADVANCING 
                      DISPLAY
                         "RSP-NUM-CARDS OF TABLEAU("
                         WITH NO ADVANCING 
                      DISPLAY T-STACK-I ")="
                         WITH NO ADVANCING 
                      DISPLAY
                         RSP-NUM-CARDS OF TABLEAU
                         WITH NO ADVANCING 
                      DISPLAY " <> 0"
                   END-IF
           END-PERFORM

           ADD 1 TO TESTS-RUN
           IF RANK-N OF RSP-CARD OF TABLEAU IS EQUAL TO 10
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "06-POP-FROM-NON-EMPTY-STACK:" WITH NO
                 ADVANCING 
              DISPLAY
                 "RANK-N OF RSP-CARD OF TABLEAU="
                 WITH NO ADVANCING 
              DISPLAY RANK-N OF RSP-CARD OF TABLEAU
                 WITH NO ADVANCING 
              DISPLAY " <> 10"
           END-IF

           ADD 1 TO TESTS-RUN
           IF SUIT-N OF RSP-CARD OF TABLEAU IS EQUAL TO 1
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "06-POP-FROM-NON-EMPTY-STACK:" WITH NO
                 ADVANCING 
              DISPLAY
                 "SUIT-N OF RSP-CARD OF TABLEAU="
                 WITH NO ADVANCING 
              DISPLAY SUIT-N OF RSP-CARD OF TABLEAU
                 WITH NO ADVANCING 
              DISPLAY " <> 1"
           END-IF.

      ******************************************************************
       07-MANDATORY-EMPTY-TABLEAU.
           MOVE 5 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

           ADD 1 TO TESTS-RUN
           IF RSP-ERR-CODE OF TABLEAU IS EQUAL TO 1
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "07-MANDATORY-EMPTY-TABLEAU:" WITH NO ADVANCING 
              DISPLAY
                 "ERR-CODE="
                 WITH NO ADVANCING 
              DISPLAY
                 RSP-ERR-CODE OF TABLEAU
                 WITH NO ADVANCING 
              DISPLAY " <> 1"
           END-IF.

      ******************************************************************
       08-MANDATORY-NONEMPTY-NOMATCH.
      *    FILL TABLEAU WITH SOME CARDS
           MOVE 10 TO RANK-N OF CARD-IN-SCOPE.
           MOVE 1 TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 7 TO REQ-STCK-IDX.
           MOVE 3 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

           MOVE 3 TO RANK-N OF CARD-IN-SCOPE.
           MOVE 2 TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 5 TO REQ-STCK-IDX.
           MOVE 3 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

           MOVE 8 TO RANK-N OF CARD-IN-SCOPE.
           MOVE 3 TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 3 TO REQ-STCK-IDX.
           MOVE 3 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

           MOVE 9 TO RANK-N OF CARD-IN-SCOPE.
           MOVE 4 TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 1 TO REQ-STCK-IDX.
           MOVE 3 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

      *    CHECK FOR MANDATORY CARD EXISTING
           MOVE 1 TO RANK-N OF CARD-IN-SCOPE.
           MOVE 2 TO SUIT-N OF CARD-IN-SCOPE.

           MOVE 5 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

           ADD 1 TO TESTS-RUN
           IF RSP-ERR-CODE OF TABLEAU IS EQUAL TO 2
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "08-MANDATORY-NONEMPTY-NOMATCH:"
                 WITH NO ADVANCING 
              DISPLAY
                 "ERR-CODE="
                 WITH NO ADVANCING 
              DISPLAY
                 RSP-ERR-CODE OF TABLEAU
                 WITH NO ADVANCING 
              DISPLAY " <> 2"
           END-IF.

      ******************************************************************
       09-MANDATORY-NONEMPTY-MATCH1.
      *    FILL TABLEAU WITH SOME CARDS
           MOVE 1 TO RANK-N OF CARD-IN-SCOPE.
           MOVE 2 TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 7 TO REQ-STCK-IDX.
           MOVE 3 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

           MOVE 3 TO RANK-N OF CARD-IN-SCOPE.
           MOVE 2 TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 5 TO REQ-STCK-IDX.
           MOVE 3 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

           MOVE 8 TO RANK-N OF CARD-IN-SCOPE.
           MOVE 3 TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 3 TO REQ-STCK-IDX.
           MOVE 3 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

           MOVE 9 TO RANK-N OF CARD-IN-SCOPE.
           MOVE 4 TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 1 TO REQ-STCK-IDX.
           MOVE 3 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

      *    CHECK FOR MANDATORY CARD EXISTING
           MOVE 1 TO RANK-N OF CARD-IN-SCOPE.
           MOVE 2 TO SUIT-N OF CARD-IN-SCOPE.

           MOVE 5 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

           ADD 1 TO TESTS-RUN
           IF RSP-ERR-CODE OF TABLEAU IS EQUAL TO 0
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "09-MANDATORY-NONEMPTY-MATCH1:"
                 WITH NO ADVANCING 
              DISPLAY
                 "ERR-CODE="
                 WITH NO ADVANCING 
              DISPLAY
                 RSP-ERR-CODE OF TABLEAU
                 WITH NO ADVANCING 
              DISPLAY " <> 0"
           END-IF.

           ADD 1 TO TESTS-RUN
           IF RSP-MNDT-STCK-IDX IS EQUAL TO 7
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "09-MANDATORY-NONEMPTY-MATCH1:"
                 WITH NO ADVANCING 
              DISPLAY
                 "RSP-MNDT-STCK-IDX="
                 WITH NO ADVANCING 
              DISPLAY
                 RSP-MNDT-STCK-IDX
                 WITH NO ADVANCING 
              DISPLAY " <> 7"
           END-IF.

      ******************************************************************
       10-MANDATORY-NONEMPTY-MATCH2.
      *    FILL TABLEAU WITH SOME CARDS
           MOVE 1 TO RANK-N OF CARD-IN-SCOPE.
           MOVE 2 TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 7 TO REQ-STCK-IDX.
           MOVE 3 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

           MOVE 3 TO RANK-N OF CARD-IN-SCOPE.
           MOVE 2 TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 5 TO REQ-STCK-IDX.
           MOVE 3 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

           MOVE 8 TO RANK-N OF CARD-IN-SCOPE.
           MOVE 3 TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 3 TO REQ-STCK-IDX.
           MOVE 3 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

           MOVE 9 TO RANK-N OF CARD-IN-SCOPE.
           MOVE 4 TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 1 TO REQ-STCK-IDX.
           MOVE 3 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

      *    CHECK FOR MANDATORY 1. CARD EXISTING
           MOVE 1 TO RANK-N OF CARD-IN-SCOPE.
           MOVE 2 TO SUIT-N OF CARD-IN-SCOPE.

           MOVE 5 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

           ADD 1 TO TESTS-RUN
           IF RSP-ERR-CODE OF TABLEAU IS EQUAL TO 0
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "10-MANDATORY-NONEMPTY-MATCH2:"
                 WITH NO ADVANCING 
              DISPLAY
                 "ERR-CODE(1)="
                 WITH NO ADVANCING 
              DISPLAY
                 RSP-ERR-CODE OF TABLEAU
                 WITH NO ADVANCING 
              DISPLAY " <> 0"
           END-IF.

           ADD 1 TO TESTS-RUN
           IF RSP-MNDT-STCK-IDX IS EQUAL TO 7
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "10-MANDATORY-NONEMPTY-MATCH2:"
                 WITH NO ADVANCING 
              DISPLAY
                 "RSP-MNDT-STCK-IDX(1)="
                 WITH NO ADVANCING 
              DISPLAY
                 RSP-MNDT-STCK-IDX
                 WITH NO ADVANCING 
              DISPLAY " <> 7"
           END-IF.

      *    CHECK FOR MANDATORY 2. CARD EXISTING
           MOVE 8 TO RANK-N OF CARD-IN-SCOPE.
           MOVE 3 TO SUIT-N OF CARD-IN-SCOPE.

           MOVE 5 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

           ADD 1 TO TESTS-RUN
           IF RSP-ERR-CODE OF TABLEAU IS EQUAL TO 0
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "10-MANDATORY-NONEMPTY-MATCH2:"
                 WITH NO ADVANCING 
              DISPLAY
                 "ERR-CODE(2)="
                 WITH NO ADVANCING 
              DISPLAY
                 RSP-ERR-CODE OF TABLEAU
                 WITH NO ADVANCING 
              DISPLAY " <> 0"
           END-IF.

           ADD 1 TO TESTS-RUN
           IF RSP-MNDT-STCK-IDX IS EQUAL TO 3
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "10-MANDATORY-NONEMPTY-MATCH2:"
                 WITH NO ADVANCING 
              DISPLAY
                 "RSP-MNDT-STCK-IDX(2)="
                 WITH NO ADVANCING 
              DISPLAY
                 RSP-MNDT-STCK-IDX
                 WITH NO ADVANCING 
              DISPLAY " <> 3"
           END-IF.

      ******************************************************************
       11-MANDATORY-NONEMPTY-MATCH3.
      *    FILL TABLEAU WITH SOME CARDS
           MOVE 1 TO RANK-N OF CARD-IN-SCOPE.
           MOVE 2 TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 7 TO REQ-STCK-IDX.
           MOVE 3 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

           MOVE 3 TO RANK-N OF CARD-IN-SCOPE.
           MOVE 2 TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 5 TO REQ-STCK-IDX.
           MOVE 3 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

           MOVE 8 TO RANK-N OF CARD-IN-SCOPE.
           MOVE 3 TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 3 TO REQ-STCK-IDX.
           MOVE 3 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

           MOVE 9 TO RANK-N OF CARD-IN-SCOPE.
           MOVE 4 TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 1 TO REQ-STCK-IDX.
           MOVE 3 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

      *    CHECK FOR MANDATORY 1. CARD EXISTING
           MOVE 1 TO RANK-N OF CARD-IN-SCOPE.
           MOVE 2 TO SUIT-N OF CARD-IN-SCOPE.

           MOVE 5 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

           ADD 1 TO TESTS-RUN
           IF RSP-ERR-CODE OF TABLEAU IS EQUAL TO 0
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "11-MANDATORY-NONEMPTY-MATCH3:"
                 WITH NO ADVANCING 
              DISPLAY
                 "ERR-CODE(1)="
                 WITH NO ADVANCING 
              DISPLAY
                 RSP-ERR-CODE OF TABLEAU
                 WITH NO ADVANCING 
              DISPLAY " <> 0"
           END-IF.

           ADD 1 TO TESTS-RUN
           IF RSP-MNDT-STCK-IDX IS EQUAL TO 7
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "11-MANDATORY-NONEMPTY-MATCH3:"
                 WITH NO ADVANCING 
              DISPLAY
                 "RSP-MNDT-STCK-IDX(1)="
                 WITH NO ADVANCING 
              DISPLAY
                 RSP-MNDT-STCK-IDX
                 WITH NO ADVANCING 
              DISPLAY " <> 7"
           END-IF.

      *    CHECK FOR MANDATORY 2. CARD EXISTING
           MOVE 8 TO RANK-N OF CARD-IN-SCOPE.
           MOVE 3 TO SUIT-N OF CARD-IN-SCOPE.

           MOVE 5 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

           ADD 1 TO TESTS-RUN
           IF RSP-ERR-CODE OF TABLEAU IS EQUAL TO 0
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "11-MANDATORY-NONEMPTY-MATCH3:"
                 WITH NO ADVANCING 
              DISPLAY
                 "ERR-CODE(2)="
                 WITH NO ADVANCING 
              DISPLAY
                 RSP-ERR-CODE OF TABLEAU
                 WITH NO ADVANCING 
              DISPLAY " <> 0"
           END-IF.

           ADD 1 TO TESTS-RUN
           IF RSP-MNDT-STCK-IDX IS EQUAL TO 3
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "11-MANDATORY-NONEMPTY-MATCH3:"
                 WITH NO ADVANCING 
              DISPLAY
                 "RSP-MNDT-STCK-IDX(2)="
                 WITH NO ADVANCING 
              DISPLAY
                 RSP-MNDT-STCK-IDX
                 WITH NO ADVANCING 
              DISPLAY " <> 3"
           END-IF.

      *    CHECK FOR MANDATORY 3. CARD EXISTING
           MOVE 3 TO RANK-N OF CARD-IN-SCOPE.
           MOVE 2 TO SUIT-N OF CARD-IN-SCOPE.

           MOVE 5 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

           ADD 1 TO TESTS-RUN
           IF RSP-ERR-CODE OF TABLEAU IS EQUAL TO 0
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "11-MANDATORY-NONEMPTY-MATCH3:"
                 WITH NO ADVANCING 
              DISPLAY
                 "ERR-CODE(3)="
                 WITH NO ADVANCING 
              DISPLAY
                 RSP-ERR-CODE OF TABLEAU
                 WITH NO ADVANCING 
              DISPLAY " <> 0"
           END-IF.

           ADD 1 TO TESTS-RUN
           IF RSP-MNDT-STCK-IDX IS EQUAL TO 5
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "11-MANDATORY-NONEMPTY-MATCH3:"
                 WITH NO ADVANCING 
              DISPLAY
                 "RSP-MNDT-STCK-IDX(3)="
                 WITH NO ADVANCING 
              DISPLAY
                 RSP-MNDT-STCK-IDX
                 WITH NO ADVANCING 
              DISPLAY " <> 5"
           END-IF.

      ******************************************************************
       12-MANDATORY-NONEMPTY-MATCH4.
      *    FILL TABLEAU WITH SOME CARDS
           MOVE 1 TO RANK-N OF CARD-IN-SCOPE.
           MOVE 2 TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 7 TO REQ-STCK-IDX.
           MOVE 3 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

           MOVE 3 TO RANK-N OF CARD-IN-SCOPE.
           MOVE 2 TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 5 TO REQ-STCK-IDX.
           MOVE 3 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

           MOVE 8 TO RANK-N OF CARD-IN-SCOPE.
           MOVE 3 TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 3 TO REQ-STCK-IDX.
           MOVE 3 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

           MOVE 9 TO RANK-N OF CARD-IN-SCOPE.
           MOVE 4 TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 1 TO REQ-STCK-IDX.
           MOVE 3 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

      *    CHECK FOR MANDATORY 1. CARD EXISTING
           MOVE 1 TO RANK-N OF CARD-IN-SCOPE.
           MOVE 2 TO SUIT-N OF CARD-IN-SCOPE.

           MOVE 5 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

           ADD 1 TO TESTS-RUN
           IF RSP-ERR-CODE OF TABLEAU IS EQUAL TO 0
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "12-MANDATORY-NONEMPTY-MATCH4:"
                 WITH NO ADVANCING 
              DISPLAY
                 "ERR-CODE(1)="
                 WITH NO ADVANCING 
              DISPLAY
                 RSP-ERR-CODE OF TABLEAU
                 WITH NO ADVANCING 
              DISPLAY " <> 0"
           END-IF.

           ADD 1 TO TESTS-RUN
           IF RSP-MNDT-STCK-IDX IS EQUAL TO 7
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "12-MANDATORY-NONEMPTY-MATCH4:"
                 WITH NO ADVANCING 
              DISPLAY
                 "RSP-MNDT-STCK-IDX(1)="
                 WITH NO ADVANCING 
              DISPLAY
                 RSP-MNDT-STCK-IDX
                 WITH NO ADVANCING 
              DISPLAY " <> 7"
           END-IF.

      *    CHECK FOR MANDATORY 2. CARD EXISTING
           MOVE 8 TO RANK-N OF CARD-IN-SCOPE.
           MOVE 3 TO SUIT-N OF CARD-IN-SCOPE.

           MOVE 5 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

           ADD 1 TO TESTS-RUN
           IF RSP-ERR-CODE OF TABLEAU IS EQUAL TO 0
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "12-MANDATORY-NONEMPTY-MATCH4:"
                 WITH NO ADVANCING 
              DISPLAY
                 "ERR-CODE(2)="
                 WITH NO ADVANCING 
              DISPLAY
                 RSP-ERR-CODE OF TABLEAU
                 WITH NO ADVANCING 
              DISPLAY " <> 0"
           END-IF.

           ADD 1 TO TESTS-RUN
           IF RSP-MNDT-STCK-IDX IS EQUAL TO 3
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "12-MANDATORY-NONEMPTY-MATCH4:"
                 WITH NO ADVANCING 
              DISPLAY
                 "RSP-MNDT-STCK-IDX(2)="
                 WITH NO ADVANCING 
              DISPLAY
                 RSP-MNDT-STCK-IDX
                 WITH NO ADVANCING 
              DISPLAY " <> 3"
           END-IF.

      *    CHECK FOR MANDATORY 3. CARD EXISTING
           MOVE 3 TO RANK-N OF CARD-IN-SCOPE.
           MOVE 2 TO SUIT-N OF CARD-IN-SCOPE.

           MOVE 5 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

           ADD 1 TO TESTS-RUN
           IF RSP-ERR-CODE OF TABLEAU IS EQUAL TO 0
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "12-MANDATORY-NONEMPTY-MATCH4:"
                 WITH NO ADVANCING 
              DISPLAY
                 "ERR-CODE(3)="
                 WITH NO ADVANCING 
              DISPLAY
                 RSP-ERR-CODE OF TABLEAU
                 WITH NO ADVANCING 
              DISPLAY " <> 0"
           END-IF.

           ADD 1 TO TESTS-RUN
           IF RSP-MNDT-STCK-IDX IS EQUAL TO 5
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "12-MANDATORY-NONEMPTY-MATCH4:"
                 WITH NO ADVANCING 
              DISPLAY
                 "RSP-MNDT-STCK-IDX(3)="
                 WITH NO ADVANCING 
              DISPLAY
                 RSP-MNDT-STCK-IDX
                 WITH NO ADVANCING 
              DISPLAY " <> 5"
           END-IF.

      *    CHECK FOR MANDATORY 4. CARD EXISTING
           MOVE 9 TO RANK-N OF CARD-IN-SCOPE.
           MOVE 4 TO SUIT-N OF CARD-IN-SCOPE.

           MOVE 5 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

           ADD 1 TO TESTS-RUN
           IF RSP-ERR-CODE OF TABLEAU IS EQUAL TO 0
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "12-MANDATORY-NONEMPTY-MATCH4:"
                 WITH NO ADVANCING 
              DISPLAY
                 "ERR-CODE(3)="
                 WITH NO ADVANCING 
              DISPLAY
                 RSP-ERR-CODE OF TABLEAU
                 WITH NO ADVANCING 
              DISPLAY " <> 0"
           END-IF.

           ADD 1 TO TESTS-RUN
           IF RSP-MNDT-STCK-IDX IS EQUAL TO 1
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "12-MANDATORY-NONEMPTY-MATCH4:"
                 WITH NO ADVANCING 
              DISPLAY
                 "RSP-MNDT-STCK-IDX(3)="
                 WITH NO ADVANCING 
              DISPLAY
                 RSP-MNDT-STCK-IDX
                 WITH NO ADVANCING 
              DISPLAY " <> 5"
           END-IF.

      ******************************************************************
       13-MANDATORY-NONEMPTY-NOT-TOS.
      *    FILL TABLEAU WITH SOME CARDS
           MOVE 1 TO RANK-N OF CARD-IN-SCOPE.
           MOVE 2 TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 7 TO REQ-STCK-IDX.
           MOVE 3 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

           MOVE 3 TO RANK-N OF CARD-IN-SCOPE.
           MOVE 2 TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 7 TO REQ-STCK-IDX.
           MOVE 3 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

           MOVE 8 TO RANK-N OF CARD-IN-SCOPE.
           MOVE 3 TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 3 TO REQ-STCK-IDX.
           MOVE 3 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

           MOVE 9 TO RANK-N OF CARD-IN-SCOPE.
           MOVE 4 TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 1 TO REQ-STCK-IDX.
           MOVE 3 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

      *    CHECK FOR MANDATORY CARD EXISTING, BUT NOT TOS
           MOVE 1 TO RANK-N OF CARD-IN-SCOPE.
           MOVE 2 TO SUIT-N OF CARD-IN-SCOPE.

           MOVE 5 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

           ADD 1 TO TESTS-RUN
           IF RSP-ERR-CODE OF TABLEAU IS EQUAL TO 2
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "13-MANDATORY-NONEMPTY-NOT-TOS:"
                 WITH NO ADVANCING 
              DISPLAY
                 "ERR-CODE(1)="
                 WITH NO ADVANCING 
              DISPLAY
                 RSP-ERR-CODE OF TABLEAU
                 WITH NO ADVANCING 
              DISPLAY " <> 2"
           END-IF.

      ******************************************************************
       20-MV-SRC-IS-EMPTY.
           MOVE 1 TO MV-SRC-ST-I.
           MOVE 6 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

           ADD 1 TO TESTS-RUN
           IF RSP-ERR-CODE OF TABLEAU IS EQUAL TO 1
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "20-MV-SRC-IS-EMPTY:"
                 WITH NO ADVANCING 
              DISPLAY
                 "ERR-CODE="
                 WITH NO ADVANCING 
              DISPLAY
                 RSP-ERR-CODE OF TABLEAU
                 WITH NO ADVANCING 
              DISPLAY " <> 1"
           END-IF.

      ******************************************************************
       21-MV-SRC-OUT-OF-RANGE.
      *    FILL TABLEAU WITH SOME CARDS
           MOVE 1 TO RANK-N OF CARD-IN-SCOPE.
           MOVE 2 TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 1 TO REQ-STCK-IDX.
           MOVE 3 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

           MOVE 1 TO MV-SRC-ST-I.
           MOVE 2 TO MV-SRC-CA-I.
           MOVE 6 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

           ADD 1 TO TESTS-RUN
           IF RSP-ERR-CODE OF TABLEAU IS EQUAL TO 2
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "21-MV-SRC-OUT-OF-RANGE:"
                 WITH NO ADVANCING 
              DISPLAY
                 "ERR-CODE="
                 WITH NO ADVANCING 
              DISPLAY
                 RSP-ERR-CODE OF TABLEAU
                 WITH NO ADVANCING 
              DISPLAY " <> 2"
           END-IF.

      ******************************************************************       
       22-MV-SRC-WRONG-RANK.
      *    FILL TABLEAU WITH SOME CARDS
           MOVE 5 TO RANK-N OF CARD-IN-SCOPE.
           MOVE 2 TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 1 TO REQ-STCK-IDX.
           MOVE 3 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

           MOVE 7 TO RANK-N OF CARD-IN-SCOPE.
           MOVE 3 TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 2 TO REQ-STCK-IDX.
           MOVE 3 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

           MOVE 1 TO MV-SRC-ST-I.
           MOVE 1 TO MV-SRC-CA-I.
           MOVE 2 TO MV-DST-ST-I.
           MOVE 6 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

           ADD 1 TO TESTS-RUN
           IF RSP-ERR-CODE OF TABLEAU IS EQUAL TO 3
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "22-MV-SRC-WRONG-RANK:"
                 WITH NO ADVANCING 
              DISPLAY
                 "ERR-CODE="
                 WITH NO ADVANCING 
              DISPLAY
                 RSP-ERR-CODE OF TABLEAU
                 WITH NO ADVANCING 
              DISPLAY " <> 3"
           END-IF.

      ******************************************************************
       23-MV-SRC-WRONG-SUIT.
      *    FILL TABLEAU WITH SOME CARDS
           MOVE 5 TO RANK-N OF CARD-IN-SCOPE.
           MOVE 2 TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 1 TO REQ-STCK-IDX.
           MOVE 3 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

           MOVE 6 TO RANK-N OF CARD-IN-SCOPE.
           MOVE 4 TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 2 TO REQ-STCK-IDX.
           MOVE 3 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

           MOVE 1 TO MV-SRC-ST-I.
           MOVE 1 TO MV-SRC-CA-I.
           MOVE 2 TO MV-DST-ST-I.
           MOVE 6 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

           ADD 1 TO TESTS-RUN
           IF RSP-ERR-CODE OF TABLEAU IS EQUAL TO 4
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "23-MV-SRC-WRONG-SUIT:"
                 WITH NO ADVANCING 
              DISPLAY
                 "ERR-CODE="
                 WITH NO ADVANCING 
              DISPLAY
                 RSP-ERR-CODE OF TABLEAU
                 WITH NO ADVANCING 
              DISPLAY " <> 4"
           END-IF.

      ******************************************************************
       24-MV-SRC-NOT-KING.
      *    FILL TABLEAU WITH SOME CARDS
           MOVE 5 TO RANK-N OF CARD-IN-SCOPE.
           MOVE 2 TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 1 TO REQ-STCK-IDX.
           MOVE 3 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

           MOVE 1 TO MV-SRC-ST-I.
           MOVE 1 TO MV-SRC-CA-I.
           MOVE 2 TO MV-DST-ST-I.
           MOVE 6 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

           ADD 1 TO TESTS-RUN
           IF RSP-ERR-CODE OF TABLEAU IS EQUAL TO 5
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "24-MV-SRC-NOT-KING:"
                 WITH NO ADVANCING 
              DISPLAY
                 "ERR-CODE="
                 WITH NO ADVANCING 
              DISPLAY
                 RSP-ERR-CODE OF TABLEAU
                 WITH NO ADVANCING 
              DISPLAY " <> 5"
           END-IF.

      ******************************************************************
       25-MV-SRC-KING-DST-NOT-EMPTY.
      *    FILL TABLEAU WITH SOME CARDS
           MOVE 13 TO RANK-N OF CARD-IN-SCOPE.
           MOVE 2 TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 1 TO REQ-STCK-IDX.
           MOVE 3 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

           MOVE 6 TO RANK-N OF CARD-IN-SCOPE.
           MOVE 3 TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 2 TO REQ-STCK-IDX.
           MOVE 3 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

           MOVE 1 TO MV-SRC-ST-I.
           MOVE 1 TO MV-SRC-CA-I.
           MOVE 2 TO MV-DST-ST-I.
           MOVE 6 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

           ADD 1 TO TESTS-RUN
           IF RSP-ERR-CODE OF TABLEAU IS EQUAL TO 3
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "25-MV-SRC-KING-DST-NOT-EMPTY:"
                 WITH NO ADVANCING 
              DISPLAY
                 "ERR-CODE="
                 WITH NO ADVANCING 
              DISPLAY
                 RSP-ERR-CODE OF TABLEAU
                 WITH NO ADVANCING 
              DISPLAY " <> 0"
           END-IF.

      ******************************************************************
       26-MV-OK-SRC-NOT-KING.
      *    FILL TABLEAU WITH SOME CARDS
           MOVE 5 TO RANK-N OF CARD-IN-SCOPE.
           MOVE 2 TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 1 TO REQ-STCK-IDX.
           MOVE 3 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

           MOVE 7 TO RANK-N OF CARD-IN-SCOPE.
           MOVE 4 TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 1 TO REQ-STCK-IDX.
           MOVE 3 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

           MOVE 6 TO RANK-N OF CARD-IN-SCOPE.
           MOVE 3 TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 2 TO REQ-STCK-IDX.
           MOVE 3 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

           MOVE 1 TO MV-SRC-ST-I.
           MOVE 1 TO MV-SRC-CA-I.
           MOVE 2 TO MV-DST-ST-I.
           MOVE 6 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

           ADD 1 TO TESTS-RUN
           IF RSP-ERR-CODE OF TABLEAU IS EQUAL TO 0
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "26-MV-OK-SRC-NOT-KING:"
                 WITH NO ADVANCING 
              DISPLAY
                 "ERR-CODE="
                 WITH NO ADVANCING 
              DISPLAY
                 RSP-ERR-CODE OF TABLEAU
                 WITH NO ADVANCING 
              DISPLAY " <> 0"
           END-IF.

           MOVE 8 TO REQ-OP-CODE OF TABLEAU
           MOVE 1 TO REQ-STCK-IDX OF TABLEAU
           CALL 'TABLEAU' USING TABLEAU
           END-CALL
           ADD 1 TO TESTS-RUN
           IF RSP-NUM-CARDS OF TABLEAU IS EQUAL TO 0
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "26-MV-OK-SRC-NOT-KING:"
                 WITH NO ADVANCING 
              DISPLAY
                 "RSP-NUM-CARDS OF TABLEAU(1)="
                 WITH NO ADVANCING 
              DISPLAY
                 RSP-NUM-CARDS OF TABLEAU
                 WITH NO ADVANCING 
              DISPLAY " <> 0"
           END-IF.

           MOVE 8 TO REQ-OP-CODE OF TABLEAU
           MOVE 2 TO REQ-STCK-IDX OF TABLEAU
           CALL 'TABLEAU' USING TABLEAU
           END-CALL
           ADD 1 TO TESTS-RUN
           IF RSP-NUM-CARDS OF TABLEAU IS EQUAL TO 3
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "26-MV-OK-SRC-NOT-KING:"
                 WITH NO ADVANCING 
              DISPLAY
                 "RSP-NUM-CARDS OF TABLEAU(2)="
                 WITH NO ADVANCING 
              DISPLAY
                 RSP-NUM-CARDS OF TABLEAU
                 WITH NO ADVANCING 
              DISPLAY " <> 2"
           END-IF.

      *    CHECK TARGET STACK FOR CORRECT CARDS
           MOVE 2 TO REQ-STCK-IDX.
           MOVE 4 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           
           ADD 1 TO TESTS-RUN
           IF RANK-N OF RSP-CARD IS EQUAL TO 7
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "26-MV-OK-SRC-NOT-KING:"
                 WITH NO ADVANCING 
              DISPLAY
                 "RANK-N OF RSP-CARD(2)="
                 WITH NO ADVANCING 
              DISPLAY
                 RANK-N OF RSP-CARD
                 WITH NO ADVANCING 
              DISPLAY " <> 7"
           END-IF.
           ADD 1 TO TESTS-RUN
           IF SUIT-N OF RSP-CARD IS EQUAL TO 4
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "26-MV-OK-SRC-NOT-KING:"
                 WITH NO ADVANCING 
              DISPLAY
                 "SUIT-N OF RSP-CARD(2)="
                 WITH NO ADVANCING 
              DISPLAY
                 SUIT-N OF RSP-CARD
                 WITH NO ADVANCING 
              DISPLAY " <> 4"
           END-IF.

           MOVE 2 TO REQ-STCK-IDX.
           MOVE 4 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           
           ADD 1 TO TESTS-RUN
           IF RANK-N OF RSP-CARD IS EQUAL TO 5
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "26-MV-OK-SRC-NOT-KING:"
                 WITH NO ADVANCING 
              DISPLAY
                 "RANK-N OF RSP-CARD(2)="
                 WITH NO ADVANCING 
              DISPLAY
                 RANK-N OF RSP-CARD
                 WITH NO ADVANCING 
              DISPLAY " <> 5"
           END-IF.
           ADD 1 TO TESTS-RUN
           IF SUIT-N OF RSP-CARD IS EQUAL TO 2
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "26-MV-OK-SRC-NOT-KING:"
                 WITH NO ADVANCING 
              DISPLAY
                 "SUIT-N OF RSP-CARD(2)="
                 WITH NO ADVANCING 
              DISPLAY
                 SUIT-N OF RSP-CARD
                 WITH NO ADVANCING 
              DISPLAY " <> 2"
           END-IF.

           MOVE 2 TO REQ-STCK-IDX.
           MOVE 4 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           
           ADD 1 TO TESTS-RUN
           IF RANK-N OF RSP-CARD IS EQUAL TO 6
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "26-MV-OK-SRC-NOT-KING:"
                 WITH NO ADVANCING 
              DISPLAY
                 "RANK-N OF RSP-CARD(2)="
                 WITH NO ADVANCING 
              DISPLAY
                 RANK-N OF RSP-CARD
                 WITH NO ADVANCING 
              DISPLAY " <> 6"
           END-IF.
           ADD 1 TO TESTS-RUN
           IF SUIT-N OF RSP-CARD IS EQUAL TO 3
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "26-MV-OK-SRC-NOT-KING:"
                 WITH NO ADVANCING 
              DISPLAY
                 "SUIT-N OF RSP-CARD(2)="
                 WITH NO ADVANCING 
              DISPLAY
                 SUIT-N OF RSP-CARD
                 WITH NO ADVANCING 
              DISPLAY " <> 3"
           END-IF.

      ******************************************************************
       27-MV-OK-SRC-KING.
      *    FILL TABLEAU WITH SOME CARDS
           MOVE 3 TO RANK-N OF CARD-IN-SCOPE.
           MOVE 2 TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 1 TO REQ-STCK-IDX.
           MOVE 3 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

           MOVE 13 TO RANK-N OF CARD-IN-SCOPE.
           MOVE 2 TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 1 TO REQ-STCK-IDX.
           MOVE 3 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

           MOVE 6 TO RANK-N OF CARD-IN-SCOPE.
           MOVE 3 TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 2 TO REQ-STCK-IDX.
           MOVE 3 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

           MOVE 1 TO MV-SRC-ST-I.
           MOVE 2 TO MV-SRC-CA-I.
           MOVE 3 TO MV-DST-ST-I.
           MOVE 6 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

           ADD 1 TO TESTS-RUN
           IF RSP-ERR-CODE OF TABLEAU IS EQUAL TO 0
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "27-MV-OK-SRC-KING:"
                 WITH NO ADVANCING 
              DISPLAY
                 "ERR-CODE="
                 WITH NO ADVANCING 
              DISPLAY
                 RSP-ERR-CODE OF TABLEAU
                 WITH NO ADVANCING 
              DISPLAY " <> 0"
           END-IF.

           MOVE 8 TO REQ-OP-CODE OF TABLEAU
           MOVE 1 TO REQ-STCK-IDX OF TABLEAU
           CALL 'TABLEAU' USING TABLEAU
           END-CALL
           ADD 1 TO TESTS-RUN
           IF RSP-NUM-CARDS OF TABLEAU IS EQUAL TO 1
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "27-MV-OK-SRC-KING:"
                 WITH NO ADVANCING 
              DISPLAY
                 "RSP-NUM-CARDS OF TABLEAU(1)="
                 WITH NO ADVANCING 
              DISPLAY
                 RSP-NUM-CARDS OF TABLEAU
                 WITH NO ADVANCING 
              DISPLAY " <> 1"
           END-IF.

           MOVE 8 TO REQ-OP-CODE OF TABLEAU
           MOVE 2 TO REQ-STCK-IDX OF TABLEAU
           CALL 'TABLEAU' USING TABLEAU
           END-CALL
           ADD 1 TO TESTS-RUN
           IF RSP-NUM-CARDS OF TABLEAU IS EQUAL TO 1
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "27-MV-OK-SRC-KING:"
                 WITH NO ADVANCING 
              DISPLAY
                 "RSP-NUM-CARDS OF TABLEAU(2)="
                 WITH NO ADVANCING 
              DISPLAY
                 RSP-NUM-CARDS OF TABLEAU
                 WITH NO ADVANCING 
              DISPLAY " <> 1"
           END-IF.

           MOVE 8 TO REQ-OP-CODE OF TABLEAU
           MOVE 3 TO REQ-STCK-IDX OF TABLEAU
           CALL 'TABLEAU' USING TABLEAU
           END-CALL
           ADD 1 TO TESTS-RUN
           IF RSP-NUM-CARDS OF TABLEAU IS EQUAL TO 1
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "27-MV-OK-SRC-KING:"
                 WITH NO ADVANCING 
              DISPLAY
                 "RSP-NUM-CARDS OF TABLEAU(3)="
                 WITH NO ADVANCING 
              DISPLAY
                 RSP-NUM-CARDS OF TABLEAU
                 WITH NO ADVANCING 
              DISPLAY " <> 1"
           END-IF.

      ******************************************************************
       30-CHECK1-IDX.
      *    FILL TABLEAU WITH 1 CARD
           MOVE 3 TO RANK-N OF CARD-IN-SCOPE.
           MOVE 2 TO SUIT-N OF CARD-IN-SCOPE.
           MOVE 1 TO REQ-STCK-IDX.
           MOVE 3 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.           

           MOVE 9 TO REQ-OP-CODE OF TABLEAU
           MOVE 1 TO REQ-STCK-IDX OF TABLEAU
           MOVE 1 TO REQ-CARD-IDX OF TABLEAU 
           CALL 'TABLEAU' USING TABLEAU
           END-CALL
           ADD 1 TO TESTS-RUN
           IF RANK-N OF RSP-CARD IS EQUAL TO 3
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "30-CHECK1-IDX:" WITH NO ADVANCING 
              DISPLAY
                 "RSP-RANK-N OF RSP-CARD="
                 WITH NO ADVANCING 
              DISPLAY
                 RANK-N OF RSP-CARD
                 WITH NO ADVANCING 
              DISPLAY " <> 3"
           END-IF.

      ******************************************************************
      ******************************************************************