       IDENTIFICATION DIVISION.
       PROGRAM-ID. SOLITAIRE.

       DATA DIVISION. 

       WORKING-STORAGE SECTION. 
      ******************************************************************
      * RANK DEFINITIONS
      *   DEFINES A RANK FOR A CARD IN THE GAME
       01 RANK.
      *      THE ALPHA LETTER OF THE RANK
          02 RANK-A               PIC X.
      *      THE NUMBER OF THE RANK
          02 RANK-N               PIC 99.
      
      *   DEFINES ALL POSSIBLE RANKS FOR A CARD IN THE GAME
       01 RANKS.
      *      TABLE OF RANKS IN THE GAME
          02 RANK-T OCCURS 13 TIMES INDEXED BY RANK-I.
      *         THE ALPHA LETTER OF THE RANK
             03 RANK-A            PIC X.
      *         THE NUMBER OF THE RANK
             03 RANK-N            PIC 99.                     

      ******************************************************************
      * SUIT DEFINITIONS
      *   DEFINES A SUIT FOR A CARD IN THE GAME
       01 SUIT.
      *      THE ALPHA LETTER OF THE SUIT
          02 SUIT-A               PIC X.
      *      THE COLOR OF THE SUIT
          02 SUIT-C               PIC X.
      *      THE NUMBER OF THE SUIT
          02 SUIT-N               PIC 9.

      
      ******************************************************************
      *   DEFINES ALL POSSIBLE SUITS FOR A CARD IN THE GAME
       01 SUITS.
      *      TABLE OF SUITS IN THE GAME
          02 SUIT-T OCCURS 4 TIMES INDEXED BY SUIT-I.
      *         THE ALPHA LETTER OF THE SUIT
             03 SUIT-A            PIC X.
      *         THE COLOR OF THE SUIT
             03 SUIT-C            PIC X.                     
      *         THE NUMBER OF THE SUIT
             03 SUIT-N            PIC 9.

      ******************************************************************
      * CARD DEFINITIONS
      *   DEFINES A CARD FOR THE GAME
       01 CARD.
          02 C-RANK.
             03 RANK-A            PIC X.
             03 RANK-N            PIC 99.
          02 C-SUIT.
             03 SUIT-A            PIC X.
             03 SUIT-C            PIC X.
             03 SUIT-N            PIC 9.

      ******************************************************************
      *   DEFINES ALL POSSIBLE CARDS IN THE GAME
       01 CARDS.
      *      TABLE OF CARDS IN THE GAME
          04 CARDS-T OCCURS 52 TIMES INDEXED BY CARDS-I.
             05 CARDS-RANK.
                06 RANK-A         PIC X.
                06 RANK-N         PIC 99.
             05 CARDS-SUIT.
                06 SUIT-A         PIC X.
                06 SUIT-C         PIC X.
                06 SUIT-N         PIC 9.

      ******************************************************************
      *   DEFINES THE STOCK OF THE GAME
       01 STOCK.
          02 RANDOM-INDEX         PIC 99.
      *      HOW MANY CARDS ARE IN THE STOCK.
      *      IN THE INITIALIZATION PHASE, THIS COUNTER GOES UP,
      *        AS IT COUNTS THE CARDS TRANFERRED INTO THE STOCK
      *      THE STOCK SHRINKS OVER TIME, WHEN WE FETCH CARDS
          02 COUNT-OF-CARDS       PIC 99 VALUE 0.
      *      TABLE OF CARDS IN THE STOCK
          02 STOCK-T OCCURS 52 TIMES INDEXED BY STOCK-I.
             05 STOCK-RANK.
                06 RANK-A         PIC X.
                06 RANK-N         PIC 99.
             05 STOCK-SUIT.
                06 SUIT-A         PIC X.
                06 SUIT-C         PIC X.
                06 SUIT-N         PIC 9.

      ******************************************************************
      *   DEFINES ALL FOUNDATION STACKS OF THE GAME
       01 F-STACKS.
      *      THE FOUNDATION HAS FOUR STACKS TO MAINTAIN
          05 F-STACKS-T OCCURS 4 TIMES INDEXED BY F-STACK-I.
      *         THE SUIT OF THIS FOUNDATION STACK.
             10 STACK-SUIT-A      PIC X.
      *         HOW MANY CARDS ARE IN THE STACK.
             10 COUNT-OF-CARDS    PIC 99 VALUE 0.
      *         NEXT ACCEPTABLE RANK
      *         ALWAYS COUNT-OF-CARDS + 1
             10 NEXT-RANK         PIC 99 VALUE 1.
      *         SIGNAL, IF THE STACK IS FULL
             10 IS-FULL           PIC X  VALUE 'N'.

      ******************************************************************
      *   DEFINES ALL TABLEAU STACKS OF THE GAME
       01 T-STACKS.
      *   THE TABLEAU HAS SEVEN STACKS TO MAINTAIN
          05 T-STACKS-T OCCURS 7 TIMES INDEXED BY T-STACK-I.
      *      HOW MANY CARDS ARE IN THE STACK.
             10 COUNT-OF-CARDS    PIC 99 VALUE 0.
      *      TABLE OF CARDS IN THE STACK
             10 STACK-T OCCURS 52 TIMES INDEXED BY STACK-I.
                30 STACK-RANK.
                   40 RANK-A      PIC X.
                   40 RANK-N      PIC 99.
                30 STACK-SUIT.
                   40 SUIT-A      PIC X.
                   40 SUIT-C      PIC X.
                   40 SUIT-N      PIC 9.

      ******************************************************************
      *   DEFINES THE USER MENU SELECTION
       01 USER-SELECTION.
          02 MENU-TO-SHOW         PIC 99.
          02 MENU-ENTRY-SELECTED  PIC X.

      ******************************************************************
       PROCEDURE DIVISION.
           
           PERFORM INITIALIZE-WORLD.

           MOVE 1 TO MENU-TO-SHOW.
           CALL 'MENUS' USING USER-SELECTION
           END-CALL

           DISPLAY RANKS.
           DISPLAY " "

           DISPLAY SUITS.
           DISPLAY " "
           
           PERFORM VARYING CARDS-I
              FROM 1 BY 1
              UNTIL CARDS-I > 52
           
                   DISPLAY CARDS-T(CARDS-I)
           END-PERFORM

           PERFORM VARYING STOCK-I
              FROM 1 BY 1
              UNTIL STOCK-I > 52
           
                   DISPLAY STOCK-T(STOCK-I)
           END-PERFORM
           
           STOP RUN.

      ******************************************************************
       INITIALIZE-WORLD.
           PERFORM RANK-T-FILL-ALL.
           PERFORM SUIT-T-FILL-ALL.
           PERFORM CARD-T-FILL-ALL.
           PERFORM STOCK-FILL.
           PERFORM INITIALIZE-FOUNDATION.

      ******************************************************************
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

      ******************************************************************
       SUIT-T-FILL-ALL.
      *    SET DIAMOND / RED
           MOVE 'D' TO SUIT-A OF SUIT.
           MOVE 'R' TO SUIT-C OF SUIT.
           MOVE 1 TO SUIT-N OF SUIT.
           MOVE SUIT TO SUIT-T(1).

      *    SET CLUB / BLACK
           MOVE 'C' TO SUIT-A OF SUIT.
           MOVE 'B' TO SUIT-C OF SUIT.
           MOVE 2 TO SUIT-N OF SUIT.
           MOVE SUIT TO SUIT-T(2).

      *    SET HEART / RED
           MOVE 'H' TO SUIT-A OF SUIT.
           MOVE 'R' TO SUIT-C OF SUIT.
           MOVE 3 TO SUIT-N OF SUIT.
           MOVE SUIT TO SUIT-T(3).

      *    SET SPADE / BLACK
           MOVE 'S' TO SUIT-A OF SUIT.
           MOVE 'B' TO SUIT-C OF SUIT.
           MOVE 4 TO SUIT-N OF SUIT.
           MOVE SUIT TO SUIT-T(4).

      ******************************************************************
       CARD-T-FILL-ALL.
      *    SET COUNTER TO INITIAL VALUE FOR THE TABLE
           MOVE 1 TO CARDS-I.
      *    RUN THROUGH ALL SUITS
           PERFORM VARYING SUIT-I
              FROM 1 BY 1
              UNTIL SUIT-I > 4
      *            RUN THROUGH ALL RANKS
                   PERFORM VARYING RANK-I
                      FROM 1 BY 1
                      UNTIL RANK-I > 13
      *                    HANDLE ALL ELEMENTS FOR THE CARD
                           MOVE SUIT-T(SUIT-I) TO C-SUIT OF CARD
                           MOVE RANK-T(RANK-I) TO C-RANK OF CARD
      *                    ASSIGN THE CONSTRUCTED CARD
                           MOVE CARD TO CARDS-T(CARDS-I)
      *                    NEXT CARD TO PROCESS
                           ADD 1 TO CARDS-I
                   END-PERFORM
           END-PERFORM.

      ******************************************************************
       STOCK-FILL.
           COMPUTE RANDOM-INDEX = FUNCTION RANDOM *(52 - 1 + 1) + 1
           DISPLAY "RANDOM=" RANDOM-INDEX.

           COMPUTE RANDOM-INDEX = FUNCTION RANDOM *(52 - 1 + 1) + 1
           DISPLAY "RANDOM=" RANDOM-INDEX.

           COMPUTE RANDOM-INDEX = FUNCTION RANDOM *(52 - 1 + 1) + 1
           DISPLAY "RANDOM=" RANDOM-INDEX.


      ******************************************************************
       INITIALIZE-FOUNDATION.
      *    RUN THROUGH ALL SUITS
           PERFORM VARYING SUIT-I
              FROM 1 BY 1
              UNTIL SUIT-I > 4
      *            ASSIGN THE SUIT CODE TO THE CORRECT FOUNDATION
                   MOVE SUIT-A OF SUIT-T(SUIT-I) TO
                      STACK-SUIT-A OF F-STACKS-T(SUIT-I)
           END-PERFORM.

      *
      * IDENTIFICATION DIVISION.                                   
      * PROGRAM-ID. EXT7.                                           
      * DATA DIVISION.                                             
      * WORKING-STORAGE SECTION.                                   
      * 01 MIN-NUMBER PIC 99 VALUE 10.                             
      * 01 MAX-NUMBER PIC 99 VALUE 20.                             
      * 01 RANDOM-NUMBER PIC 99.                                   
      * PROCEDURE DIVISION.                                         
      * MAIN-PARA.                                                 
      *      PERFORM 10 TIMES                                       
      *          COMPUTE RANDOM-NUMBER = FUNCTION RANDOM *         
      *                             (MAX-NUMBER - MIN-NUMBER + 1) +
      *                              MIN-NUMBER                     
      *          DISPLAY 'RANDOM NUMBER:' RANDOM-NUMBER             
      *      END-PERFORM.                                           
      *      STOP RUN.     