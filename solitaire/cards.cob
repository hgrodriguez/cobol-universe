       IDENTIFICATION DIVISION.
       PROGRAM-ID. CARDS.

       DATA DIVISION.

       WORKING-STORAGE SECTION. 

      ******************************************************************
      *   DEFINES ALL POSSIBLE RANKS FOR A CARD IN THE GAME
       01 RANKS.
      *      TABLE OF RANKS IN THE GAME
          02 RANK-T OCCURS 13 TIMES INDEXED BY RANK-I.
      *         THE ALPHA LETTER OF THE RANK
             03 RANK-A           PIC X.
      *         THE NUMBER OF THE RANK
             03 RANK-N           PIC 99.                     

      ******************************************************************
      *   DEFINES ALL POSSIBLE SUITS FOR A CARD IN THE GAME
       01 SUITS.
      *      TABLE OF SUITS IN THE GAME
          02 SUIT-T OCCURS 4 TIMES INDEXED BY SUIT-I.
      *         THE ALPHA LETTER OF THE SUIT
             03 SUIT-A           PIC X.
      *         THE COLOR OF THE SUIT
             03 SUIT-C           PIC X.                     
      *         THE NUMBER OF THE SUIT
             03 SUIT-N           PIC 9.

      ******************************************************************
      * CARD DEFINITIONS
      *   DEFINES A CARD FOR THE GAME
       01 CARD.
          02 C-RANK.
             03 RANK-A           PIC X.
             03 RANK-N           PIC 99.
          02 C-SUIT.
             03 SUIT-A           PIC X.
             03 SUIT-C           PIC X.
             03 SUIT-N           PIC 9.

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
                      07 RANK-A  PIC X.
      *                  NUMBER CODE OF RANK:
      *                  1 - 13
                      07 RANK-N  PIC 99.
                   06 CARD-SUIT.
      *                  ALPHA CODE OF SUIT:
      *                  D(IAMONDS),C(LUB),H(EARTS),S(PADES)
                      07 SUIT-A  PIC X.
      *                  COLOR OF SUIT:
      *                  R(ED), B(LACK)
                      07 SUIT-C  PIC X.
      *                  NUMBER CODE OF SUIT:
      *                  1 - 4
                      07 SUIT-N  PIC 9.

      ******************************************************************
       PROCEDURE DIVISION USING GAME.
      *    WE ONLY OFFER ONE THING AND ONE THING ONLY:
      *    INITIALIZE THE CARDS WITH ALL SUITS AND RANKS.

           PERFORM RANK-T-FILL-ALL.

           PERFORM SUIT-T-FILL-ALL.

           PERFORM CARD-T-FILL-ALL.

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
              UNTIL SUIT-I > 4
      *            MOVE ALL DATA OF CURRENT SUIT TO CARD
                   MOVE SUIT-A OF SUITS(SUIT-I)
                      TO SUIT-A OF C-SUIT
                   MOVE SUIT-C OF SUITS(SUIT-I)
                      TO SUIT-C OF C-SUIT
                   MOVE SUIT-N OF SUITS(SUIT-I)
                      TO SUIT-N OF C-SUIT

      *            RUN THROUGH ALL RANKS
                   PERFORM VARYING RANK-I
                      FROM 1 BY 1
                      UNTIL RANK-I > 13

      *                    MOVE ALL DATA OF CURRENT RANK TO CARD
                           MOVE RANK-A OF RANKS(RANK-I)
                              TO RANK-A OF C-RANK
                           MOVE RANK-N OF RANKS(RANK-I)
                              TO RANK-N OF C-RANK
      *                    HERE WE HAVE ALL DATA MOVED, NOW MOVE CARD
                           MOVE CARD TO CARDS-RANK-T(SUIT-I, RANK-I)
                   END-PERFORM
           END-PERFORM.