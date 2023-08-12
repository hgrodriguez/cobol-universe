       IDENTIFICATION DIVISION.
       PROGRAM-ID. STOCK.

       DATA DIVISION.

       WORKING-STORAGE SECTION. 
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

       LINKAGE SECTION. 
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
                      07 RANK-A         PIC X.
      *                  NUMBER CODE OF RANK:
      *                  1 - 13
                      07 RANK-N         PIC 99.
                   06 CARD-SUIT.
      *                  ALPHA CODE OF SUIT:
      *                  D(IAMONDS),C(LUB),H(EARTS),S(PADES)
                      07 SUIT-A         PIC X.
      *                  COLOR OF SUIT:
      *                  R(ED), B(LACK)
                      07 SUIT-C         PIC X.
      *                  NUMBER CODE OF SUIT:
      *                  1 - 4
                      07 SUIT-N         PIC 9.
      *      DEFINES ALL FOUNDATION STACKS OF THE GAME
          02 FOUNDATION.
      *      THE OPERATION REQUESTED TO BE PERFORMED ON THE FOUNDATION
             05 OP-CODE                 PIC 9.
      *      THE ERROR CODE, IF ANY, FOR THE REQUESTED OPERATION
             05 ERR-CODE                PIC 9.
      *      THE SUIT OF THE CARD TO PUSH ONTO THE FOUNDATION
      *          INTO THE STACK WITH NUMBER SUIT-TO-PUSH.
             05 SUIT-TO-PUSH            PIC 9.
      *          THE FOUNDATION HAS FOUR STACKS TO MAINTAIN
      *          THE INDEX INTO THE SPECIFIC STACK IS DEFINED BY THE
      *          SUIT NUMBER, AS WE HAVE FOUR SUITS
             05 F-STACKS-T OCCURS 4 TIMES INDEXED BY F-STACK-I.
      *            HOW MANY CARDS ARE IN THE STACK.
                10 COUNT-OF-CARDS       PIC 99    VALUE 0.
      *            NEXT ACCEPTABLE RANK
      *            ALWAYS COUNT-OF-CARDS + 1
                10 NEXT-RANK            PIC 99    VALUE 1.
      *            SIGNAL, IF THE STACK IS FULL
                10 IS-FULL              PIC X.
      *            ALPHA CODE OF RANK OF TOP CARD:
      *            A,2,3,4,5,6,7,8,9,T,J,Q,K             
                10 RANK-A               PIC X.
      *            ALPHA CODE OF SUIT OF TOP CARD:
      *            D(IAMONDS),C(LUB),H(EARTS),S(PADES)
                10 SUIT-A               PIC X.
      *      DEFINES THE STOCK OF THE GAME
          02 STOCK.
      *      THE OPERATION REQUESTED TO BE PERFORMED ON THE FOUNDATION
             03 OP-CODE                 PIC 9.
      *      THE ERROR CODE, IF ANY, FOR THE REQUESTED OPERATION
             03 ERR-CODE                PIC 9.
      *      THE CARD FETCHED FROM THE STOCK
             03 CARD-FETCHED.
                26 RANK-N               PIC 99.
                26 SUIT-N               PIC 9.
      *         TOP OF STOCK PRINT REPRESENTATION
             03 TOS-PEEK                PIC 9.
             03 TOS-RANK-A              PIC X.
             03 TOS-SUIT-A              PIC X.
      *      HOW MANY CARDS ARE IN THE STOCK.
      *      IN THE INITIALIZATION PHASE, THIS COUNTER GOES UP,
      *        AS IT COUNTS THE CARDS TRANFERRED INTO THE STOCK
      *      THE STOCK SHRINKS OVER TIME, WHEN WE FETCH CARDS
             03 COUNT-OF-CARDS          PIC 99.
      *      TABLE OF CARDS IN THE STOCK
             03 STOCK-T OCCURS 52 TIMES INDEXED BY STOCK-I.
                06 RANK-N               PIC 99.
                06 SUIT-N               PIC 9.      
      *      DEFINES ALL TABLEAU STACKS OF THE GAME
          02 TABLEAU.
      *      THE OPERATION REQUESTED TO BE PERFORMED ON THE TABLEAU
             05 OP-CODE                 PIC 9.
      *      THE ERROR CODE, IF ANY, FOR THE REQUESTED OPERATION
             05 ERR-CODE                PIC 9.
      *         THE STACK-INDEX IN SCOPE FOR THE REQUESTED OPERATION
             05 STACK-I-IN-SCOPE        PIC 99.
      *         THE CARD IN SCOPE FOR THE REQUESTED OPERATION
             05 CARD-IN-SCOPE.
                26 RANK-N               PIC 99.
                26 SUIT-N               PIC 9.
      *         DATA WE NEED FOR MOVING CARDS IN THE TABLEAU
      *         SOURCE STACK INDEX
             05 MV-SRC-ST-I             PIC 9.
      *         SOURCE CARD INDEX IN THE SOURCE STACK INDEX
             05 MV-SRC-CA-I             PIC 99.
      *         DESTINATION STACK INDEX
             05 MV-DST-ST-I             PIC 9.
      *         HOW MANY CARDS ARE IN THE TABLEAU.
             05 T-COUNT-OF-CARDS        PIC 99.
             05 T-STACKS-T OCCURS 7 TIMES INDEXED BY T-STACK-I.
      *            HOW MANY CARDS ARE IN THE STACK.
                10 COUNT-OF-CARDS       PIC 99    VALUE 0.
      *            THE CARDS IN ONE STACK
                10 CARDS-T OCCURS 52 TIMES INDEXED BY CARDS-T-I.
                   26 RANK-N            PIC 99.
                   26 SUIT-N            PIC 9.


      ******************************************************************
       PROCEDURE DIVISION USING GAME.
      *    PRE-CONDITION: 
      *       CARDS. ARE FILLED, AS THIS IS THE BASE FOR OUR STOCK.

           EVALUATE OP-CODE OF STOCK 
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

                           MOVE RANK-N OF CARDS-RANK-T
                              (CARDS-S-I, CARDS-R-I)
                              TO RANK-N OF STOCK-T(COUNT-OF-CARDS OF
                              STOCK)

                           MOVE SUIT-N OF CARDS-RANK-T
                              (CARDS-S-I, CARDS-R-I)
                              TO SUIT-N OF STOCK-T(COUNT-OF-CARDS OF
                              STOCK)
                   END-PERFORM
           END-PERFORM.

           MOVE 0 TO TOS-PEEK.

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
              MOVE 1 TO ERR-CODE OF STOCK
              GOBACK
           END-IF
           MOVE STOCK-T(COUNT-OF-CARDS OF STOCK) TO CARD-FETCHED
           SUBTRACT 1 FROM COUNT-OF-CARDS OF STOCK.

      ******************************************************************
       04-TOGGLE-PEEK.
           IF TOS-PEEK IS EQUAL TO 0
              MOVE 1 TO TOS-PEEK
           ELSE
              MOVE 0 TO TOS-PEEK
           END-IF.
           

      ******************************************************************
       05-PRINT-TOS.
           IF TOS-PEEK IS EQUAL TO 0 OR
              COUNT-OF-CARDS OF STOCK IS EQUAL TO 0
              MOVE 'X' TO TOS-RANK-A 
              MOVE 'X' TO TOS-SUIT-A 
           ELSE
              MOVE RANK-A OF CARDS-RANK-T(SUIT-N OF
                 STOCK-T(COUNT-OF-CARDS OF STOCK),
                 RANK-N OF STOCK-T(COUNT-OF-CARDS OF STOCK))
                 TO TOS-RANK-A

              MOVE SUIT-A OF CARDS-SUIT-T(SUIT-N OF
                 STOCK-T(COUNT-OF-CARDS OF STOCK),
                 RANK-N OF STOCK-T(COUNT-OF-CARDS OF STOCK))
                 TO TOS-SUIT-A
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

                           MOVE RANK-N OF CARDS-RANK-T
                              (CARDS-S-I, CARDS-R-I)
                              TO SHDW-RANK-N OF
                              SHADOW-STOCK(SHADOW-STOCK-I)

                           MOVE SUIT-N OF CARDS-RANK-T
                              (CARDS-S-I, CARDS-R-I)
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