       IDENTIFICATION DIVISION.
       PROGRAM-ID. RANKS.
      * HANDLES THE RANKS LOGIC
       DATA DIVISION. 
       LINKAGE SECTION. 
      ******************************************************************
      * RANK DEFINITIONS
      *   DEFINES A RANK FOR A CARD IN THE GAME
       01 RANK.
      *      THE ALPHA LETTER OF THE RANK
          02 RANK-A     PIC X.
      *      THE NUMBER OF THE RANK
          02 RANK-N     PIC 99.
      
      *   DEFINES ALL POSSIBLE RANKS FOR A CARD IN THE GAME
       01 RANKS.
      *      TABLE OF RANKS IN THE GAME
          02 RANK-T OCCURS 13 TIMES INDEXED BY RANK-I.
      *         THE ALPHA LETTER OF THE RANK
             03 RANK-A  PIC X.
      *         THE NUMBER OF THE RANK
             03 RANK-N  PIC 99.                     

       PROCEDURE DIVISION USING RANKS.
           PERFORM RANK-T-FILL-ALL.
           EXIT PROGRAM.

       RANK-T-FILL-ALL.
      *    SET ACE / 1
           MOVE 'A' TO RANK-A OF RANK.
           MOVE 1 TO RANK-N OF RANK.
           MOVE RANK TO RANK-T(1).

      *    SET 2 / 2
           MOVE '2' TO RANK-A OF RANK.
           MOVE 2 TO RANK-N OF RANK.
           MOVE RANK TO RANK-T(2).

      *    SET 3 / 3
           MOVE '3' TO RANK-A OF RANK.
           MOVE 3 TO RANK-N OF RANK.
           MOVE RANK TO RANK-T(3).

      *    SET 4 / 4
           MOVE '4' TO RANK-A OF RANK.
           MOVE 4 TO RANK-N OF RANK.
           MOVE RANK TO RANK-T(4).

      *    SET 5 / 5
           MOVE '5' TO RANK-A OF RANK.
           MOVE 5 TO RANK-N OF RANK.
           MOVE RANK TO RANK-T(5).

      *    SET 6 / 6
           MOVE '6' TO RANK-A OF RANK.
           MOVE 6 TO RANK-N OF RANK.
           MOVE RANK TO RANK-T(6).

      *    SET 7 / 7
           MOVE '7' TO RANK-A OF RANK.
           MOVE 7 TO RANK-N OF RANK.
           MOVE RANK TO RANK-T(7).

      *    SET 8 / 8
           MOVE '8' TO RANK-A OF RANK.
           MOVE 8 TO RANK-N OF RANK.
           MOVE RANK TO RANK-T(8).

      *    SET 9 / 9
           MOVE '9' TO RANK-A OF RANK.
           MOVE 9 TO RANK-N OF RANK.
           MOVE RANK TO RANK-T(9).

      *    SET 10[T] / 10
           MOVE 'T' TO RANK-A OF RANK.
           MOVE 10 TO RANK-N OF RANK.
           MOVE RANK TO RANK-T(10).           

      *    SET JACK / 11
           MOVE 'J' TO RANK-A OF RANK.
           MOVE 11 TO RANK-N OF RANK.
           MOVE RANK TO RANK-T(11).           

      *    SET QUEEN / 12
           MOVE 'Q' TO RANK-A OF RANK.
           MOVE 12 TO RANK-N OF RANK.
           MOVE RANK TO RANK-T(12).           

      *    SET KING / 13
           MOVE 'K' TO RANK-A OF RANK.
           MOVE 13 TO RANK-N OF RANK.
           MOVE RANK TO RANK-T(13).