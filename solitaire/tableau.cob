       IDENTIFICATION DIVISION.
       PROGRAM-ID. TABLEAU.

       DATA DIVISION.

       WORKING-STORAGE SECTION. 

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
      *          THE FOUNDATION HAS FOUR STACKS TO MAINTAIN
      *          THE INDEX INTO THE SPECIFIC STACK IS DEFINED BY THE
      *          SUIT NUMBER, AS WE HAVE FOUR SUITS
             05 F-STACKS-T OCCURS 4 TIMES INDEXED BY F-STACK-I.
      *            HOW MANY CARDS ARE IN THE STACK.
                10 COUNT-OF-CARDS  PIC 99 VALUE 0.
      *            NEXT ACCEPTABLE RANK
      *            ALWAYS COUNT-OF-CARDS + 1
                10 NEXT-RANK       PIC 99 VALUE 1.
      *            SIGNAL, IF THE STACK IS FULL
                10 IS-FULL         PIC X.
      *            ALPHA CODE OF RANK OF TOP CARD:
      *            A,2,3,4,5,6,7,8,9,T,J,Q,K             
                10 RANK-A          PIC X.
      *            ALPHA CODE OF SUIT OF TOP CARD:
      *            D(IAMONDS),C(LUB),H(EARTS),S(PADES)
                10 SUIT-A          PIC X.
      *      DEFINES THE STOCK OF THE GAME
          02 STOCK.
      *      THE OPERATION REQUESTED TO BE PERFORMED ON THE FOUNDATION
             03 OP-CODE            PIC 9.
      *      THE ERROR CODE, IF ANY, FOR THE REQUESTED OPERATION
             03 ERR-CODE           PIC 9.
      *      THE CARD FETCHED FROM THE STOCK
             03 CARD-FETCHED.
                26 RANK-N          PIC 99.
                26 SUIT-N          PIC 9.
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
      *         THE OPERATION REQUESTED TO BE PERFORMED ON THE TABLEAU
             05 OP-CODE            PIC 9.
      *         THE ERROR CODE, IF ANY, FOR THE REQUESTED OPERATION
             05 ERR-CODE           PIC 9.
      *         THE STACK-INDEX IN SCOPE FOR THE REQUESTED OPERATION
             05 STACK-I-IN-SCOPE   PIC 99.
      *         THE CARD IN SCOPE FOR THE REQUESTED OPERATION
             05 CARD-IN-SCOPE.
                26 RANK-N          PIC 99.
                26 SUIT-N          PIC 9.
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
      *    PRE-CONDITION: 
      *       CARDS, STOCK ARE FILLED.

           EVALUATE OP-CODE OF TABLEAU
           WHEN 1
                PERFORM 01-RESET
           WHEN 2
                PERFORM 02-INIT-FROM-STOCK
           WHEN 3
                PERFORM 03-PUSH-TO-STACK
           WHEN 4
                PERFORM 04-POP-FROM-STACK
           WHEN 5
                PERFORM 05-MANDATORY-CHECK
           WHEN 9
                PERFORM 99-PRINT
           END-EVALUATE

           GOBACK.

      ******************************************************************
       01-RESET.
           MOVE 0 TO ERR-CODE OF TABLEAU.
           MOVE 0 TO T-COUNT-OF-CARDS OF TABLEAU.

           PERFORM VARYING T-STACK-I
              FROM 1 BY 1
              UNTIL T-STACK-I > 7
                   MOVE 0 TO COUNT-OF-CARDS OF T-STACKS-T(T-STACK-I)
           END-PERFORM.

      ******************************************************************
       02-INIT-FROM-STOCK.
           PERFORM VARYING T-STACK-I
              FROM 1 BY 1
              UNTIL T-STACK-I > 7

                   PERFORM VARYING CARDS-T-I
                      FROM 1 BY 1
                      UNTIL CARDS-T-I > T-STACK-I

                           MOVE 3 TO OP-CODE OF STOCK
                           CALL 'STOCK' USING GAME
                           END-CALL

                           MOVE CARD-FETCHED OF STOCK TO CARDS-T
                              (T-STACK-I, CARDS-T-I)

                           ADD 1 TO T-COUNT-OF-CARDS OF TABLEAU 
                           ADD 1 TO COUNT-OF-CARDS OF T-STACKS-T
                              (T-STACK-I)
                   END-PERFORM
           END-PERFORM.

      ******************************************************************
       03-PUSH-TO-STACK.
           ADD 1 TO T-COUNT-OF-CARDS OF TABLEAU 
           ADD 1 TO COUNT-OF-CARDS OF T-STACKS-T(STACK-I-IN-SCOPE).

           MOVE RANK-N OF CARD-IN-SCOPE TO
              RANK-N OF CARDS-T(STACK-I-IN-SCOPE, COUNT-OF-CARDS
              OF T-STACKS-T(STACK-I-IN-SCOPE))

           MOVE SUIT-N OF CARD-IN-SCOPE TO
              SUIT-N OF CARDS-T(STACK-I-IN-SCOPE, COUNT-OF-CARDS
              OF T-STACKS-T(STACK-I-IN-SCOPE)).

      ******************************************************************
       04-POP-FROM-STACK.
           IF T-COUNT-OF-CARDS OF TABLEAU IS EQUAL TO 0
              MOVE 1 TO ERR-CODE OF TABLEAU
              GOBACK
           END-IF

           IF COUNT-OF-CARDS OF T-STACKS-T(STACK-I-IN-SCOPE)
              IS EQUAL TO 0
              MOVE 2 TO ERR-CODE OF TABLEAU
              GOBACK
           END-IF.

           MOVE RANK-N OF CARDS-T(STACK-I-IN-SCOPE, COUNT-OF-CARDS
              OF T-STACKS-T(STACK-I-IN-SCOPE)) TO
              RANK-N OF CARD-IN-SCOPE

           MOVE SUIT-N OF CARDS-T(STACK-I-IN-SCOPE, COUNT-OF-CARDS
              OF T-STACKS-T(STACK-I-IN-SCOPE)) TO
              SUIT-N OF CARD-IN-SCOPE

           SUBTRACT 1 FROM T-COUNT-OF-CARDS OF TABLEAU 
           SUBTRACT 1 FROM COUNT-OF-CARDS OF
              T-STACKS-T(STACK-I-IN-SCOPE).

      ******************************************************************
       05-MANDATORY-CHECK.
           IF T-COUNT-OF-CARDS OF TABLEAU IS EQUAL TO 0
              MOVE 1 TO ERR-CODE OF TABLEAU
              GOBACK
           END-IF.

           PERFORM VARYING T-STACK-I
              FROM 1 BY 1
              UNTIL T-STACK-I > 7
                   IF (RANK-N OF CARD-IN-SCOPE IS EQUAL TO
                      RANK-N OF CARDS-T(T-STACK-I, COUNT-OF-CARDS
                      OF T-STACKS-T(T-STACK-I))) AND
                      (SUIT-N OF CARD-IN-SCOPE IS EQUAL TO
                      SUIT-N OF CARDS-T(T-STACK-I, COUNT-OF-CARDS
                      OF T-STACKS-T(T-STACK-I)))
      *               MATCH FOUND -> LEAVE
                      MOVE 0 TO ERR-CODE OF TABLEAU
                      MOVE T-STACK-I TO STACK-I-IN-SCOPE
                      GOBACK
           END-PERFORM
      *    NO MATCH FOUND
           MOVE 2 TO ERR-CODE OF TABLEAU.

      ******************************************************************
       99-PRINT.