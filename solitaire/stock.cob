      ******************************************************************
      * SUBPROGRAM FOR THE STOCK IN THE SOLITAIRE GAME
       IDENTIFICATION DIVISION.
       PROGRAM-ID. STOCK.

       DATA DIVISION.
       WORKING-STORAGE SECTION. 
      ******************************************************************
      *   RANK ALPHA OF TOP OF STOCK
       01 RANK-A-TOS                    PIC X.
      *   SUIT ALPHA OF TOP OF STOCK
       01 SUIT-A-TOS                    PIC X.
      *   SUIT INDEX INTO THE CARD TABLE
       01 CARDS-S-I                     PIC 9.
      *   RANK INDEX INTO THE CARD TABLE
       01 CARDS-R-I                     PIC 99.
      *   MIN AND MAX OF STOCK INDICES
       01 MIN-NUMBER                    PIC 99    VALUE 1.
       01 MAX-NUMBER                    PIC 99    VALUE 52.
      *   INDEX FOR RANDOMIZATION OD THE STOCK
       01 RANDOM-INDEX                  PIC 99.
      *   RANDOM SEED
       01 SEED                          PIC 9999 COMP-3.
      *   THE RESULT OF THE CURRENT DATE/TIME CALL
      *   USED AS INPUT FOR THE SEED
       01 WS-CURRENT-DATE-DATA.
          05 WS-CURRENT-DATE.
             10 WS-CURRENT-YEAR         PIC 9(04).
             10 WS-CURRENT-MONTH        PIC 9(02).
             10 WS-CURRENT-DAY          PIC 9(02).
          05 WS-CURRENT-TIME.
             10 WS-CURRENT-HOURS        PIC 9(02).
             10 WS-CURRENT-MINUTE       PIC 9(02).
             10 WS-CURRENT-SECOND       PIC 9(02).
             10 WS-CURRENT-MILLISECONDS PIC 9(02).
      *   THIS SHADOW IS THE BASE FOR FETCHING RANDOM CARDS
       01 SHADOW-STOCK.
          03 SHDW-STOCK-T OCCURS 52 TIMES INDEXED BY SHADOW-STOCK-I.
             06 SHDW-RANK-N             PIC 99.
             06 SHDW-SUIT-N             PIC 9.
             06 WAS-FETCHED             PIC X     VALUE 'N'.
      *   HOW MANY CARDS ARE IN THE STOCK.
      *   IN THE INITIALIZATION PHASE, THIS COUNTER GOES UP,
      *     AS IT COUNTS THE CARDS TRANFERRED INTO THE STOCK
      *   THE STOCK SHRINKS OVER TIME, WHEN WE FETCH CARDS
       01 COUNT-OF-CARDS                PIC 99.
      *   TABLE OF CARDS IN THE STOCK
       01 STOCK-T OCCURS 52 TIMES INDEXED BY STOCK-I.
          06 RANK-N                     PIC 99.
          06 SUIT-N                     PIC 9.      

      ******************************************************************
       01 CARDS-API.
      *   DOCUMENTATION SEE CARDS.COB      
      *      THE REQUES-RESPONSE-BLOCK
          02 REQ-RSP-BLOCK.
             03 REQ-OP-CODE             PIC 9.
             03 REQ-RANK-N              PIC 99.
             03 REQ-SUIT-N              PIC 9.
             03 RSP-ERR-CODE            PIC 99.
             03 RSP-RANK-A              PIC X.
             03 RSP-SUIT-A              PIC X.

       LINKAGE SECTION. 
      ******************************************************************
      *      DEFINES THE API FOR THE STOCK OF THE GAME
       01 STOCK-API.
          03 REQ-RSP-BLOCK.
      *         THE OPERATION REQUESTED TO BE PERFORMED ON THE STOCK
      *         01 -> FILL-STOCK
      *         02 -> RANDOMIZE-STOCK
      *         03 -> FETCH-CARD
      *         04 -> TOGGLE-PEEK
      *         05 -> RETURN-TOS
      *         06 -> RETURN-NUM-CARDS
      *         07 -> RETURN-CARD-INDEX
      *         08 -> PRINT-TOS
      *         99 -> PRINT-STOCK
             04 REQ-OP-CODE             PIC 99.
             04 REQ-CARD-INDEX          PIC 99.
      *      THE ERROR CODE, IF ANY, FOR THE REQUESTED OPERATION
      *            1 = ILLEGAL OP CODE
      *            2 = NO CARDS LEFT
      *            3 = ILLEGAL CARD INDEX
             04 RSP-ERR-CODE            PIC 99.
      *      THE CARD FETCHED FROM THE STOCK
             04 RSP-CARD-FETCHED.
                05 RSP-RANK-N           PIC 99.
                05 RSP-SUIT-N           PIC 9.
      *         TOP OF STOCK PRINT REPRESENTATION
             04 RSP-TOS-PEEK            PIC 9.
             04 RSP-TOS-RANK-A          PIC X.
             04 RSP-TOS-SUIT-A          PIC X.
             04 RSP-NUM-OF-CARDS        PIC 99.

      ******************************************************************
       PROCEDURE DIVISION USING STOCK-API.
      *    PRE-CONDITION: 
      *       CARDS. ARE FILLED, AS THIS IS THE BASE FOR OUR STOCK.

           MOVE 0 TO RSP-ERR-CODE OF STOCK-API
           EVALUATE REQ-OP-CODE OF STOCK-API 
           WHEN 1
                PERFORM 01-FILL-STOCK
           WHEN 2
                PERFORM 02-RANDOMIZE-STOCK
           WHEN 3
                PERFORM 03-FETCH-CARD
           WHEN 4
                PERFORM 04-TOGGLE-PEEK
           WHEN 5
                PERFORM 05-RETURN-TOS
           WHEN 6
                PERFORM 06-RETURN-NUM-CARDS
           WHEN 7
                PERFORM 07-RETURN-CARD-INDEX
           WHEN 8
                PERFORM 08-PRINT-TOS
           WHEN OTHER 
                MOVE 1 TO RSP-ERR-CODE OF STOCK-API 
           END-EVALUATE

           GOBACK.

      ******************************************************************
       01-FILL-STOCK.
           MOVE 0 TO COUNT-OF-CARDS.

      *    RUN THROUGH ALL SUITS
           PERFORM VARYING CARDS-S-I
              FROM 1 BY 1
              UNTIL CARDS-S-I > 4

      *            RUN THROUGH ALL RANKS
                   PERFORM VARYING CARDS-R-I
                      FROM 1 BY 1
                      UNTIL CARDS-R-I > 13
                           ADD 1 TO COUNT-OF-CARDS

                           MOVE CARDS-R-I
                              TO RANK-N OF STOCK-T(COUNT-OF-CARDS)
                           MOVE CARDS-S-I
                              TO SUIT-N OF STOCK-T(COUNT-OF-CARDS)
                   END-PERFORM
           END-PERFORM.
      *    MAKE SURE, THAT WE DO NOT PEEK THE TOP OF STACK
           MOVE 0 TO RSP-TOS-PEEK.

      ******************************************************************
       02-RANDOMIZE-STOCK.
           PERFORM 10-CREATE-SHADOW-STOCK.

           PERFORM 11-SEED-RANDOM.
      *    RESET STOCK AND FETCH CARDS FORM SHADOW
           MOVE 0 TO COUNT-OF-CARDS.
      *    WE STOP, IF WE HAVE AGAIN 52 CARDS
           PERFORM UNTIL COUNT-OF-CARDS IS EQUAL TO 52
      *            WHAT TO FETCH FROM THE SHADOW STOCK
                   COMPUTE RANDOM-INDEX = FUNCTION RANDOM *
                      (MAX-NUMBER - MIN-NUMBER + 1) +
                      MIN-NUMBER
                   IF WAS-FETCHED OF SHDW-STOCK-T(RANDOM-INDEX)
                      IS EQUAL TO 'N'
      *               THIS CARD HAS NOT BEEN FETCHED BEFORE -> USE IT
                      ADD 1 TO COUNT-OF-CARDS
                      MOVE SHDW-RANK-N OF
                         SHDW-STOCK-T(RANDOM-INDEX) TO
                         RANK-N OF STOCK-T(COUNT-OF-CARDS)
                      MOVE SHDW-SUIT-N OF
                         SHDW-STOCK-T(RANDOM-INDEX) TO
                         SUIT-N OF STOCK-T(COUNT-OF-CARDS)
      *               MARK CARD AS FETCHED
                      MOVE 'Y' TO
                         WAS-FETCHED OF SHDW-STOCK-T(RANDOM-INDEX)
                   END-IF
           END-PERFORM.

      ******************************************************************
       03-FETCH-CARD.
           IF COUNT-OF-CARDS IS EQUAL TO 0
      *       NO CARDS LEFT TO FETCH
              MOVE 2 TO RSP-ERR-CODE OF STOCK-API
              GOBACK
           END-IF
           MOVE STOCK-T(COUNT-OF-CARDS) TO RSP-CARD-FETCHED
           SUBTRACT 1 FROM COUNT-OF-CARDS.

      ******************************************************************
       04-TOGGLE-PEEK.
           IF RSP-TOS-PEEK IS EQUAL TO 0
              MOVE 1 TO RSP-TOS-PEEK
           ELSE
              MOVE 0 TO RSP-TOS-PEEK
           END-IF.
           
      ******************************************************************
       05-RETURN-TOS.
           IF RSP-TOS-PEEK IS EQUAL TO 0 OR
              COUNT-OF-CARDS IS EQUAL TO 0
              MOVE 'X' TO RSP-TOS-RANK-A 
              MOVE 'X' TO RSP-TOS-SUIT-A 
           ELSE
      *       THE CARDS KNOW HOW TO MAP RANK AND SUIT NUMBER
              MOVE RANK-N OF STOCK-T(COUNT-OF-CARDS)
                 TO REQ-RANK-N OF CARDS-API
              MOVE SUIT-N OF STOCK-T(COUNT-OF-CARDS)
                 TO REQ-SUIT-N OF CARDS-API 
              MOVE 2 TO REQ-OP-CODE OF CARDS-API
              CALL 'CARDS' USING REQ-RSP-BLOCK OF CARDS-API
              END-CALL

              MOVE RSP-RANK-A OF CARDS-API TO RSP-TOS-RANK-A
              MOVE RSP-SUIT-A OF CARDS-API TO RSP-TOS-SUIT-A
           END-IF.
      
      ******************************************************************
       06-RETURN-NUM-CARDS.
           MOVE COUNT-OF-CARDS TO RSP-NUM-OF-CARDS.

      ******************************************************************
       07-RETURN-CARD-INDEX.
           IF REQ-CARD-INDEX IS EQUAL TO 0
              MOVE 3 TO RSP-ERR-CODE OF STOCK-API 
              GOBACK
           END-IF
           IF REQ-CARD-INDEX IS GREATER THAN COUNT-OF-CARDS
              MOVE 3 TO RSP-ERR-CODE OF STOCK-API 
              GOBACK
           END-IF
           MOVE STOCK-T(REQ-CARD-INDEX) TO RSP-CARD-FETCHED.

      ******************************************************************
       08-PRINT-TOS.
           IF RSP-TOS-PEEK IS EQUAL TO 0 OR
              COUNT-OF-CARDS IS EQUAL TO 0
              MOVE 'X' TO RANK-A-TOS
              MOVE 'X' TO SUIT-A-TOS
           ELSE
      *       THE CARDS KNOW HOW TO MAP THIS
              MOVE RANK-N OF STOCK-T(COUNT-OF-CARDS)
                 TO REQ-RANK-N OF CARDS-API
              MOVE SUIT-N OF STOCK-T(COUNT-OF-CARDS)
                 TO REQ-SUIT-N OF CARDS-API 
              MOVE 2 TO REQ-OP-CODE OF CARDS-API
              CALL 'CARDS' USING REQ-RSP-BLOCK OF CARDS-API
              END-CALL

              MOVE RSP-RANK-A OF CARDS-API TO RANK-A-TOS
              MOVE RSP-SUIT-A OF CARDS-API TO SUIT-A-TOS
           END-IF.
           DISPLAY RANK-A-TOS WITH NO ADVANCING 
           DISPLAY SUIT-A-TOS.

      ******************************************************************
       10-CREATE-SHADOW-STOCK.
           MOVE 0 TO SHADOW-STOCK-I.

      *    RUN THROUGH ALL SUITS
           PERFORM VARYING CARDS-S-I
              FROM 1 BY 1
              UNTIL CARDS-S-I > 4
      *            RUN THROUGH ALL RANKS
                   PERFORM VARYING CARDS-R-I
                      FROM 1 BY 1
                      UNTIL CARDS-R-I > 13
                           ADD 1 TO SHADOW-STOCK-I
                           MOVE 'N' TO WAS-FETCHED OF SHDW-STOCK-T
                              (SHADOW-STOCK-I)
                           MOVE CARDS-R-I
                              TO SHDW-RANK-N OF
                              SHADOW-STOCK(SHADOW-STOCK-I)
                           MOVE CARDS-S-I
                              TO SHDW-SUIT-N OF
                              SHADOW-STOCK(SHADOW-STOCK-I)
                   END-PERFORM
           END-PERFORM.

      ******************************************************************
       11-SEED-RANDOM.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-DATA
           ADD WS-CURRENT-MILLISECONDS TO SEED
           MULTIPLY 100 BY SEED
           MOVE WS-CURRENT-SECOND TO SEED
           COMPUTE RANDOM-INDEX = FUNCTION RANDOM(SEED).