       IDENTIFICATION DIVISION.
       PROGRAM-ID. STOCK.

       DATA DIVISION.

       WORKING-STORAGE SECTION. 
       01 CARDS-S-I                     PIC 9.
       01 CARDS-R-I                     PIC 99.
       01 PRINT-INDEX                   PIC 99.
       01 MIN-NUMBER                    PIC 99    VALUE 1.
       01 MAX-NUMBER                    PIC 99    VALUE 52.
       01 RANDOM-INDEX                  PIC 99.
       01 SEED                          PIC 9999 COMP-3.
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
       01 SHADOW-STOCK.
          03 SHDW-STOCK-T OCCURS 52 TIMES INDEXED BY SHADOW-STOCK-I.
             06 SHDW-RANK-N             PIC 99.
             06 SHDW-SUIT-N             PIC 9.
             06 WAS-FETCHED             PIC X     VALUE 'N'.

      ******************************************************************
       01 CARDS.
      *      THE REQUES-RESPONSE-BLOCK
          03 REQ-RSP-BLOCK.
      *            THE OPERATION REQUESTED TO BE PERFORMED
      *            1 = INITIALIZE CARDS
             04 REQ-OP-CODE             PIC 9.
      *            RANK NUMBER
             04 REQ-RANK-N              PIC 99.
      *            SUIT NUMBER
             04 REQ-SUIT-N              PIC 9.
      *            THE ERROR CODE, IF ANY, FOR THE REQUESTED OPERATION
      *            1 = ILLEGAL OP
      *            2 = ILLEGAL RANK: LOWER THAN MIN
      *            3 = ILLEGAL RANK: HIGHER THAN MAX
      *            4 = ILLEGAL SUIT: LOWER THAN MIN
      *            5 = ILLEGAL SUIT: HIGHER THAN MAX
             04 RSP-ERR-CODE            PIC 99.
      *            RANK ALPHA CODE OF REQUESTED RANK NUMBER
             04 RSP-RANK-A              PIC X.
      *            SUIT ALPHA CODE OF REQUESTED SUIT NUMBER
             04 RSP-SUIT-A              PIC X.

       LINKAGE SECTION. 
      ******************************************************************
      *      DEFINES THE STOCK OF THE GAME
       01 STOCK.
          03 REQ-RSP-BLOCK.
      *      THE OPERATION REQUESTED TO BE PERFORMED ON THE FOUNDATION
             04 REQ-OP-CODE             PIC 99.
      *      THE ERROR CODE, IF ANY, FOR THE REQUESTED OPERATION
      *            1 = ILLEGAL OP CODE
      *            2 = NO CARDS LEFT
             04 RSP-ERR-CODE            PIC 9.
      *      THE CARD FETCHED FROM THE STOCK
             04 RSP-CARD-FETCHED.
                05 RSP-RANK-N           PIC 99.
                05 RSP-SUIT-N           PIC 9.
      *         TOP OF STOCK PRINT REPRESENTATION
             04 RSP-TOS-PEEK            PIC 9.
             04 RSP-TOS-RANK-A          PIC X.
             04 RSP-TOS-SUIT-A          PIC X.
      *      HOW MANY CARDS ARE IN THE STOCK.
      *      IN THE INITIALIZATION PHASE, THIS COUNTER GOES UP,
      *        AS IT COUNTS THE CARDS TRANFERRED INTO THE STOCK
      *      THE STOCK SHRINKS OVER TIME, WHEN WE FETCH CARDS
          03 COUNT-OF-CARDS             PIC 99.
      *      TABLE OF CARDS IN THE STOCK
          03 STOCK-T OCCURS 52 TIMES INDEXED BY STOCK-I.
             06 RANK-N                  PIC 99.
             06 SUIT-N                  PIC 9.      

      ******************************************************************
       PROCEDURE DIVISION USING STOCK.
      *    PRE-CONDITION: 
      *       CARDS. ARE FILLED, AS THIS IS THE BASE FOR OUR STOCK.

           MOVE 0 TO RSP-ERR-CODE OF STOCK
           EVALUATE REQ-OP-CODE OF STOCK 
           WHEN 1
                PERFORM 01-FILL-STOCK
           WHEN 2
                PERFORM 02-RANDOMIZE-STOCK
           WHEN 3
                PERFORM 03-FETCH-CARD
           WHEN 4
                PERFORM 04-TOGGLE-PEEK
           WHEN 5
                PERFORM 05-PRINT-TOS
           WHEN 9
                PERFORM 99-PRINT-STOCK
           END-EVALUATE

           GOBACK.

      ******************************************************************
       01-FILL-STOCK.
           MOVE 0 TO COUNT-OF-CARDS OF STOCK.

      *    RUN THROUGH ALL SUITS
           PERFORM VARYING CARDS-S-I
              FROM 1 BY 1
              UNTIL CARDS-S-I > 4
      *            RUN THROUGH ALL RANKS
                   PERFORM VARYING CARDS-R-I
                      FROM 1 BY 1
                      UNTIL CARDS-R-I > 13
                           ADD 1 TO COUNT-OF-CARDS OF STOCK

                           MOVE CARDS-R-I
                              TO RANK-N OF STOCK-T(COUNT-OF-CARDS OF
                              STOCK)
                           MOVE CARDS-S-I
                              TO SUIT-N OF STOCK-T(COUNT-OF-CARDS OF
                              STOCK)
                   END-PERFORM
           END-PERFORM.

           MOVE 0 TO RSP-TOS-PEEK.

      ******************************************************************
       02-RANDOMIZE-STOCK.
           PERFORM 10-CREATE-SHADOW-STOCK.

           PERFORM 11-SEED-RANDOM.
      *    RESET STOCK AND FETCH CARDS FORM SHADOW
           MOVE 0 TO COUNT-OF-CARDS OF STOCK.
      *    WE STOP, IF WE HAVE AGAIN 52 CARDS
           PERFORM UNTIL COUNT-OF-CARDS OF STOCK IS EQUAL TO 52
      *            WHAT TO FETCH FROM THE SHADOW STOCK
                   COMPUTE RANDOM-INDEX = FUNCTION RANDOM *
                      (MAX-NUMBER - MIN-NUMBER + 1) +
                      MIN-NUMBER
                   IF WAS-FETCHED OF SHDW-STOCK-T(RANDOM-INDEX)
                      IS EQUAL TO 'N'
                      ADD 1 TO COUNT-OF-CARDS OF STOCK
                      MOVE SHDW-RANK-N OF
                         SHDW-STOCK-T(RANDOM-INDEX) TO
                         RANK-N OF STOCK-T(COUNT-OF-CARDS OF STOCK)
                      MOVE SHDW-SUIT-N OF
                         SHDW-STOCK-T(RANDOM-INDEX) TO
                         SUIT-N OF STOCK-T(COUNT-OF-CARDS OF STOCK)
                      MOVE 'Y' TO
                         WAS-FETCHED OF SHDW-STOCK-T(RANDOM-INDEX)
                   END-IF
           END-PERFORM.

      ******************************************************************
       03-FETCH-CARD.
           IF COUNT-OF-CARDS OF STOCK IS EQUAL TO 0
      *       NO CARDS LEFT TO FETCH
              MOVE 2 TO RSP-ERR-CODE OF STOCK
              GOBACK
           END-IF
           MOVE STOCK-T(COUNT-OF-CARDS OF STOCK) TO RSP-CARD-FETCHED
           SUBTRACT 1 FROM COUNT-OF-CARDS OF STOCK.

      ******************************************************************
       04-TOGGLE-PEEK.
           IF RSP-TOS-PEEK IS EQUAL TO 0
              MOVE 1 TO RSP-TOS-PEEK
           ELSE
              MOVE 0 TO RSP-TOS-PEEK
           END-IF.
           

      ******************************************************************
       05-PRINT-TOS.
           IF RSP-TOS-PEEK IS EQUAL TO 0 OR
              COUNT-OF-CARDS OF STOCK IS EQUAL TO 0
              MOVE 'X' TO RSP-TOS-RANK-A 
              MOVE 'X' TO RSP-TOS-SUIT-A 
           ELSE

      *       THE CARDS KNOW HOW TO MAP THIS
              MOVE RANK-N OF STOCK-T(COUNT-OF-CARDS OF STOCK)
                 TO REQ-RANK-N OF CARDS
              MOVE SUIT-N OF STOCK-T(COUNT-OF-CARDS OF STOCK)
                 TO REQ-SUIT-N OF CARDS 
              MOVE 2 TO REQ-OP-CODE OF CARDS
              CALL 'CARDS' USING REQ-RSP-BLOCK OF CARDS
              END-CALL

              MOVE RSP-RANK-A OF CARDS TO RSP-TOS-RANK-A
              MOVE RSP-SUIT-A OF CARDS TO RSP-TOS-SUIT-A
           END-IF.
      
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

      ******************************************************************
       99-PRINT-STOCK.
           DISPLAY 'COUNT-OF-CARDS=' COUNT-OF-CARDS OF STOCK
           PERFORM VARYING PRINT-INDEX
              FROM 1 BY 1
              UNTIL PRINT-INDEX > 52
                   DISPLAY PRINT-INDEX WITH NO ADVANCING 
                   DISPLAY " RANK=" RANK-N OF STOCK-T(PRINT-INDEX)
                      WITH NO ADVANCING 
                   DISPLAY ", SUIT=" SUIT-N OF STOCK-T(PRINT-INDEX)
           END-PERFORM.
           DISPLAY " ".