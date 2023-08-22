       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTCARDS.

       DATA DIVISION. 

       WORKING-STORAGE SECTION.
      ******************************************************************
      * CARD DEFINITIONS
      *   DEFINES A CARD FOR THE GAME
       01 CARD.
          02 C-RANK.
             03 RANK-A        PIC X.
             03 RANK-N        PIC 99.
          02 C-SUIT.
             03 SUIT-A        PIC X.
             03 SUIT-C        PIC X.
             03 SUIT-N        PIC 9.

      ******************************************************************
      *      DEFINES ALL POSSIBLE CARDS IN THE GAME
       01 CARDS.
      *      THE REQUEST-RESPONSE-BLOCK
          03 REQ-RSP-BLOCK.
      *            THE OPERATION REQUESTED TO BE PERFORMED
      *            1 = INITIALIZE CARDS
             04 REQ-OP-CODE   PIC 9.
      *            RANK NUMBER
             04 REQ-RANK-N    PIC 99.
      *            SUIT NUMBER
             04 REQ-SUIT-N    PIC 9.
      *            THE ERROR CODE, IF ANY, FOR THE REQUESTED OPERATION
      *            1 = ILLEGAL OP
      *            2 = ILLEGAL RANK: LOWER THAN MIN
      *            3 = ILLEGAL RANK: HIGHER THAN MAX
      *            4 = ILLEGAL SUIT: LOWER THAN MIN
      *            5 = ILLEGAL SUIT: HIGHER THAN MAX
             04 RSP-ERR-CODE  PIC 99.
      *            RANK ALPHA CODE OF REQUESTED RANK NUMBER
             04 RSP-RANK-A    PIC X.
      *            SUIT ALPHA CODE OF REQUESTED SUIT NUMBER
             04 RSP-SUIT-A    PIC X.

      ******************************************************************
      * VARIABLES FOR THE TEST RUN
       01 TESTS-RUN           PIC 999 VALUE 0.
       01 TESTS-OK            PIC 999 VALUE 0.
       01 TESTS-NOK           PIC 999 VALUE 0.

      ******************************************************************
       PROCEDURE DIVISION.

           DISPLAY "TESTCARDS"

           MOVE 1 TO REQ-OP-CODE OF CARDS.
           CALL 'CARDS' USING REQ-RSP-BLOCK OF CARDS
           END-CALL.
           
           PERFORM 03-ILLEGAL-OP-CODE.

           PERFORM 04-CHECK-1-OK-RANK-A.

           PERFORM 05-CHECK-1-OK-SUIT-A.

           PERFORM 06-CHECK-1-RANK-LT-MIN.

           PERFORM 07-CHECK-1-RANK-GT-MAX.

           PERFORM 08-CHECK-1-SUIT-LT-MIN.

           PERFORM 09-CHECK-1-SUIT-GT-MAX.

           DISPLAY "TESTS RUN: " TESTS-RUN
           DISPLAY "SUCCESSFUL / FAILED: " TESTS-OK " / " TESTS-NOK

           GOBACK.

      ******************************************************************
       03-ILLEGAL-OP-CODE.
           MOVE 0 TO REQ-OP-CODE OF CARDS.
           CALL 'CARDS' USING REQ-RSP-BLOCK OF CARDS
           END-CALL.
           ADD 1 TO TESTS-RUN
           IF RSP-ERR-CODE OF CARDS IS EQUAL TO 1
              ADD 1 TO TESTS-OK 
           ELSE
              ADD 1 TO TESTS-NOK 
              DISPLAY RSP-ERR-CODE OF CARDS " <> 1"
           END-IF.

      ******************************************************************
       04-CHECK-1-OK-RANK-A.
           MOVE 2 TO REQ-OP-CODE OF CARDS.
           MOVE 1 TO REQ-RANK-N OF CARDS.
           MOVE 1 TO REQ-SUIT-N OF CARDS.
           CALL 'CARDS' USING REQ-RSP-BLOCK OF CARDS
           END-CALL.

           ADD 1 TO TESTS-RUN
           IF RSP-ERR-CODE OF CARDS IS EQUAL TO 0
              ADD 1 TO TESTS-OK 
           ELSE
              ADD 1 TO TESTS-NOK 
              DISPLAY "04-CHECK-1-OK-RANK-A:" WITH NO ADVANCING 
              DISPLAY RSP-ERR-CODE OF CARDS " <> 0"
           END-IF.

           ADD 1 TO TESTS-RUN
           IF RSP-RANK-A OF CARDS IS EQUAL TO 'A'
              ADD 1 TO TESTS-OK 
           ELSE
              ADD 1 TO TESTS-NOK 
              DISPLAY "04-CHECK-1-OK-RANK-A:" WITH NO ADVANCING 
              DISPLAY RSP-RANK-A OF CARDS " <> A"
           END-IF.

      ******************************************************************
       05-CHECK-1-OK-SUIT-A.
           MOVE 2 TO REQ-OP-CODE OF CARDS.
           MOVE 1 TO REQ-RANK-N OF CARDS.
           MOVE 1 TO REQ-SUIT-N OF CARDS.
           CALL 'CARDS' USING REQ-RSP-BLOCK OF CARDS
           END-CALL.

           ADD 1 TO TESTS-RUN
           IF RSP-ERR-CODE OF CARDS IS EQUAL TO 0
              ADD 1 TO TESTS-OK 
           ELSE
              ADD 1 TO TESTS-NOK 
              DISPLAY "05-CHECK-1-OK-SUIT-A:" WITH NO ADVANCING 
              DISPLAY RSP-ERR-CODE OF CARDS " <> 0"
           END-IF.

           ADD 1 TO TESTS-RUN
           IF RSP-SUIT-A OF CARDS IS EQUAL TO 'D'
              ADD 1 TO TESTS-OK 
           ELSE
              ADD 1 TO TESTS-NOK 
              DISPLAY "05-CHECK-1-OK-SUIT-A:" WITH NO ADVANCING 
              DISPLAY RSP-SUIT-A OF CARDS " <> D"
           END-IF.

      ******************************************************************
       06-CHECK-1-RANK-LT-MIN.
           MOVE 2 TO REQ-OP-CODE OF CARDS.
           MOVE 0 TO REQ-RANK-N OF CARDS.
           MOVE 1 TO REQ-SUIT-N OF CARDS.
           CALL 'CARDS' USING REQ-RSP-BLOCK OF CARDS
           END-CALL.

           ADD 1 TO TESTS-RUN
           IF RSP-ERR-CODE OF CARDS IS EQUAL TO 2
              ADD 1 TO TESTS-OK 
           ELSE
              ADD 1 TO TESTS-NOK 
              DISPLAY "06-CHECK-1-RANK-LT-MIN:" WITH NO ADVANCING 
              DISPLAY RSP-ERR-CODE OF CARDS " <> 2"
           END-IF.

      ******************************************************************
       07-CHECK-1-RANK-GT-MAX.
           MOVE 2 TO REQ-OP-CODE OF CARDS.
           MOVE 14 TO REQ-RANK-N OF CARDS.
           MOVE 1 TO REQ-SUIT-N OF CARDS.
           CALL 'CARDS' USING REQ-RSP-BLOCK OF CARDS
           END-CALL.

           ADD 1 TO TESTS-RUN
           IF RSP-ERR-CODE OF CARDS IS EQUAL TO 3
              ADD 1 TO TESTS-OK 
           ELSE
              ADD 1 TO TESTS-NOK 
              DISPLAY "07-CHECK-1-RANK-GT-MAX:" WITH NO ADVANCING 
              DISPLAY RSP-ERR-CODE OF CARDS " <> 3"
           END-IF.

      ******************************************************************
       08-CHECK-1-SUIT-LT-MIN.
           MOVE 2 TO REQ-OP-CODE OF CARDS.
           MOVE 1 TO REQ-RANK-N OF CARDS.
           MOVE 0 TO REQ-SUIT-N OF CARDS.
           CALL 'CARDS' USING REQ-RSP-BLOCK OF CARDS
           END-CALL.

           ADD 1 TO TESTS-RUN
           IF RSP-ERR-CODE OF CARDS IS EQUAL TO 4
              ADD 1 TO TESTS-OK 
           ELSE
              ADD 1 TO TESTS-NOK 
              DISPLAY "08-CHECK-1-SUIT-LT-MIN:" WITH NO ADVANCING 
              DISPLAY RSP-ERR-CODE OF CARDS " <> 4"
           END-IF.

      ******************************************************************
       09-CHECK-1-SUIT-GT-MAX.
           MOVE 2 TO REQ-OP-CODE OF CARDS.
           MOVE 1 TO REQ-RANK-N OF CARDS.
           MOVE 5 TO REQ-SUIT-N OF CARDS.
           CALL 'CARDS' USING REQ-RSP-BLOCK OF CARDS
           END-CALL.

           ADD 1 TO TESTS-RUN
           IF RSP-ERR-CODE OF CARDS IS EQUAL TO 5
              ADD 1 TO TESTS-OK 
           ELSE
              ADD 1 TO TESTS-NOK 
              DISPLAY "09-CHECK-1-SUIT-GT-MAX:" WITH NO ADVANCING 
              DISPLAY RSP-ERR-CODE OF CARDS " <> 5"
           END-IF.