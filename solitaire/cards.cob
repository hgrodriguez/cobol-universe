       IDENTIFICATION DIVISION.
       PROGRAM-ID. CARDS.

       DATA DIVISION.

       WORKING-STORAGE SECTION. 

      ******************************************************************
      *   DEFINES ALL POSSIBLE RANKS FOR A CARD IN THE GAME
       01 RANKS.
          02 RANK-MIN              PIC 99 VALUE 1.
          02 RANK-MAX              PIC 99 VALUE 13.
      *      TABLE OF RANKS IN THE GAME
          02 RANK-T OCCURS 13 TIMES INDEXED BY RANK-I.
      *         THE ALPHA LETTER OF THE RANK
             03 RANK-A             PIC X.
      *         THE NUMBER OF THE RANK
             03 RANK-N             PIC 99.                     

      ******************************************************************
      *   DEFINES ALL POSSIBLE SUITS FOR A CARD IN THE GAME
       01 SUITS.
      *      TABLE OF SUITS IN THE GAME
          02 SUIT-MIN              PIC 9  VALUE 1.
          02 SUIT-MAX              PIC 9  VALUE 4.
          02 SUIT-T OCCURS 4 TIMES INDEXED BY SUIT-I.
      *         THE ALPHA LETTER OF THE SUIT
             03 SUIT-A             PIC X.
      *         THE COLOR OF THE SUIT
             03 SUIT-C             PIC X.                     
      *         THE NUMBER OF THE SUIT
             03 SUIT-N             PIC 9.

      ******************************************************************
      * CARD DEFINITIONS
      *   DEFINES A CARD FOR THE GAME
       01 CARD.
      *      CARD RANK PART
          02 C-RANK.
      *         RANK ALPHA/ASCII CODE
             03 RANK-A             PIC X.
      *         RANK NUMBER
             03 RANK-N             PIC 99.
      *      CARD SUIT PART
          02 C-SUIT.
             03 SUIT-A             PIC X.
             03 SUIT-C             PIC X.
             03 SUIT-N             PIC 9.

       LINKAGE SECTION. 
      ******************************************************************
      *   THE GAME OVERALL.
       01 GAME.
      *      DEFINES ALL POSSIBLE CARDS IN THE GAME
          02 CARDS.
      *      THE REQUEST-RESPONSE-BLOCK
             03 REQ-RSP-BLOCK.
      *            THE OPERATION REQUESTED TO BE PERFORMED
      *            1 = INITIALIZE CARDS
                04 REQ-OP-CODE     PIC 9.
      *            RANK NUMBER
                04 REQ-RANK-N      PIC 99.
      *            SUIT NUMBER
                04 REQ-SUIT-N      PIC 9.
      *            THE ERROR CODE, IF ANY, FOR THE REQUESTED OPERATION
      *            1 = ILLEGAL OP
      *            2 = ILLEGAL RANK: LOWER THAN MIN
      *            3 = ILLEGAL RANK: HIGHER THAN MAX
      *            4 = ILLEGAL SUIT: LOWER THAN MIN
      *            5 = ILLEGAL SUIT: HIGHER THAN MAX
                04 RSP-ERR-CODE    PIC 99.
      *            RANK ALPHA CODE OF REQUESTED RANK NUMBER
                04 RSP-RANK-A      PIC X.
      *            SUIT ALPHA CODE OF REQUESTED SUIT NUMBER
                04 RSP-SUIT-A      PIC X.
      *         TABLE OF CARDS IN THE GAME
             03 CARDS-SUIT-T OCCURS 4 TIMES INDEXED BY CARDS-S-I.
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
                10 COUNT-OF-CARDS  PIC 99 VALUE 0.
      *         NEXT ACCEPTABLE RANK
      *         ALWAYS COUNT-OF-CARDS + 1
                10 NEXT-RANK       PIC 99 VALUE 1.
      *         SIGNAL, IF THE STACK IS FULL
                10 IS-FULL         PIC X  VALUE 'N'.
      *           ALPHA CODE OF RANK OF TOP CARD:
      *           A,2,3,4,5,6,7,8,9,T,J,Q,K             
                10 RANK-A          PIC X  VALUE 'X'.
      *           ALPHA CODE OF SUIT OF TOP CARD:
      *           D(IAMONDS),C(LUB),H(EARTS),S(PADES)
                10 SUIT-A          PIC X  VALUE 'X'.
      *   DEFINES THE STOCK OF THE GAME
          02 STOCK.
      *      THE OPERATION REQUESTED TO BE PERFORMED ON THE FOUNDATION
             03 OP-CODE            PIC 9.
      *      THE ERROR CODE, IF ANY, FOR THE REQUESTED OPERATION
             03 ERR-CODE           PIC 9.
      *      THE CARD FETCHED FROM THE STOCK
             03 CARD-FETCHED.
                26 RANK-N          PIC 99.
                26 SUIT-N          PIC 9.
      *         TOP OF STOCK PRINT REPRESENTATION
             03 TOS-PEEK           PIC 9.
             03 TOS-RANK-A         PIC X.
             03 TOS-SUIT-A         PIC X.
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
      *         DATA WE NEED FOR MOVING CARDS IN THE TABLEAU
      *         SOURCE STACK INDEX
             05 MV-SRC-ST-I        PIC 9.
      *         SOURCE CARD INDEX IN THE SOURCE STACK INDEX
             05 MV-SRC-CA-I        PIC 99.
      *         DESTINATION STACK INDEX
             05 MV-DST-ST-I        PIC 9.
      *         HOW MANY CARDS ARE IN THE TABLEAU.
             05 T-COUNT-OF-CARDS   PIC 99.
             05 T-STACKS-T OCCURS 7 TIMES INDEXED BY T-STACK-I.
      *            HOW MANY CARDS ARE IN THE STACK.
                10 COUNT-OF-CARDS  PIC 99 VALUE 0.
      *            THE CARDS IN ONE STACK
                10 CARDS-T OCCURS 52 TIMES INDEXED BY CARDS-T-I.
                   26 RANK-N       PIC 99.
                   26 SUIT-N       PIC 9.


      ******************************************************************
       PROCEDURE DIVISION USING GAME.

           MOVE 0 TO RSP-ERR-CODE OF CARDS.
           EVALUATE REQ-OP-CODE OF CARDS
           WHEN 1
      *         INITIALIZE THE CARDS WITH ALL SUITS AND RANKS.
                PERFORM RANK-T-FILL-ALL
                PERFORM SUIT-T-FILL-ALL
                PERFORM CARD-T-FILL-ALL
           WHEN 2
      *         RESPOND BACK WITH THE RANK-A/SUIT-A
                PERFORM 02-RESPOND-RANK-SUIT
           WHEN OTHER
      *         ILLEGAL OP-CODE
                MOVE 1 TO RSP-ERR-CODE OF CARDS
           END-EVALUATE

           GOBACK.

      ******************************************************************
       RANK-T-FILL-ALL.
      *    SET ACE / 1
           MOVE 'A' TO RANK-A OF RANK-T(1).
           MOVE 1 TO RANK-N OF RANK-T(1).

      *    SET 2 / 2
           MOVE '2' TO RANK-A OF RANK-T(2).
           MOVE 2 TO RANK-N OF RANK-T(2).

      *    SET 3 / 3
           MOVE '3' TO RANK-A OF RANK-T(3).
           MOVE 3 TO RANK-N OF RANK-T(3).

      *    SET 4 / 4
           MOVE '4' TO RANK-A OF RANK-T(4).
           MOVE 4 TO RANK-N OF RANK-T(4).

      *    SET 5 / 5
           MOVE '5' TO RANK-A OF RANK-T(5).
           MOVE 5 TO RANK-N OF RANK-T(5).

      *    SET 6 / 6
           MOVE '6' TO RANK-A OF RANK-T(6).
           MOVE 6 TO RANK-N OF RANK-T(6).

      *    SET 7 / 7
           MOVE '7' TO RANK-A OF RANK-T(7).
           MOVE 7 TO RANK-N OF RANK-T(7).

      *    SET 8 / 8
           MOVE '8' TO RANK-A OF RANK-T(8).
           MOVE 8 TO RANK-N OF RANK-T(8).

      *    SET 9 / 9
           MOVE '9' TO RANK-A OF RANK-T(9).
           MOVE 9 TO RANK-N OF RANK-T(9).

      *    SET 10[T] / 10
           MOVE 'T' TO RANK-A OF RANK-T(10).
           MOVE 10 TO RANK-N OF RANK-T(10).

      *    SET JACK / 11
           MOVE 'J' TO RANK-A OF RANK-T(11).
           MOVE 11 TO RANK-N OF RANK-T(11).

      *    SET QUEEN / 12
           MOVE 'Q' TO RANK-A OF RANK-T(12).
           MOVE 12 TO RANK-N OF RANK-T(12).

      *    SET KING / 13
           MOVE 'K' TO RANK-A OF RANK-T(13).
           MOVE 13 TO RANK-N OF RANK-T(13).

      ******************************************************************
       SUIT-T-FILL-ALL.
      *    SET DIAMOND / RED
           MOVE 'D' TO SUIT-A OF SUIT-T(1).
           MOVE 'R' TO SUIT-C OF SUIT-T(1).
           MOVE 1 TO SUIT-N OF SUIT-T(1).

      *    SET CLUB / BLACK
           MOVE 'C' TO SUIT-A OF SUIT-T(2).
           MOVE 'B' TO SUIT-C OF SUIT-T(2).
           MOVE 2 TO SUIT-N OF SUIT-T(2).

      *    SET HEART / RED
           MOVE 'H' TO SUIT-A OF SUIT-T(3).
           MOVE 'R' TO SUIT-C OF SUIT-T(3).
           MOVE 3 TO SUIT-N OF SUIT-T(3).

      *    SET SPADE / BLACK
           MOVE 'S' TO SUIT-A OF SUIT-T(4).
           MOVE 'B' TO SUIT-C OF SUIT-T(4).
           MOVE 4 TO SUIT-N OF SUIT-T(4).

      ******************************************************************
       CARD-T-FILL-ALL.
      *    RUN THROUGH ALL SUITS
           PERFORM VARYING SUIT-I
              FROM 1 BY 1
              UNTIL SUIT-I > SUIT-MAX
      *            MOVE ALL DATA OF CURRENT SUIT TO CARD
                   MOVE SUIT-T(SUIT-I) TO C-SUIT

      *            RUN THROUGH ALL RANKS
                   PERFORM VARYING RANK-I
                      FROM 1 BY 1
                      UNTIL RANK-I > RANK-MAX

      *                    MOVE ALL DATA OF CURRENT RANK TO CARD
                           MOVE RANK-T(RANK-I) TO C-RANK

      *                    HERE WE HAVE ALL DATA MOVED, NOW MOVE CARD
                           MOVE CARD TO CARDS-RANK-T(SUIT-I, RANK-I)
                   END-PERFORM
           END-PERFORM.

      ******************************************************************
       02-RESPOND-RANK-SUIT.
           IF REQ-RANK-N IS LESS THAN RANK-MIN
              MOVE 2 TO RSP-ERR-CODE OF CARDS
              GOBACK
           END-IF

           IF REQ-RANK-N IS GREATER THAN RANK-MAX
              MOVE 3 TO RSP-ERR-CODE OF CARDS
              GOBACK
           END-IF

           IF REQ-SUIT-N IS LESS THAN SUIT-MIN
              MOVE 4 TO RSP-ERR-CODE OF CARDS
              GOBACK
           END-IF

           IF REQ-SUIT-N IS GREATER THAN SUIT-MAX
              MOVE 5 TO RSP-ERR-CODE OF CARDS
              GOBACK
           END-IF

           MOVE RANK-A OF RANK-T(REQ-RANK-N OF CARDS) TO RSP-RANK-A.
           MOVE SUIT-A OF SUIT-T(REQ-SUIT-N OF CARDS) TO RSP-SUIT-A.