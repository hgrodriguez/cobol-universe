       IDENTIFICATION DIVISION.
       PROGRAM-ID. TABLEAU.

       DATA DIVISION.

       WORKING-STORAGE SECTION. 
      ******************************************************************

       01 ACCEPT-RANK              PIC 99.
       01 SRC-SUIT-OF-CARD         PIC 9.
       01 DST-SUIT-OF-CARD         PIC 9.
       01 ACCEPT-S-1               PIC 9.
       01 ACCEPT-S-2               PIC 9.
       01 XFER.
      *      HOW MANY CARDS ARE TO BE MOVED.
          02 MOVE-COUNT            PIC 99.
      *      HOW MANY CARDS ARE IN THE STOCK.
          02 XFER-COUNT            PIC 99.
      *      TABLE OF CARDS IN THE STOCK
          02 XFER-T OCCURS 52 TIMES INDEXED BY XFER-I.
             03 XFER-RANK-N        PIC 99.
             03 XFER-SUIT-N        PIC 9.      

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
           WHEN 6
                PERFORM 06-MOVE-CARDS
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
       06-MOVE-CARDS.
           IF COUNT-OF-CARDS OF T-STACKS-T(MV-SRC-ST-I)
              IS EQUAL TO 0
              MOVE 1 TO ERR-CODE OF TABLEAU
              GOBACK
           END-IF.

      *    ILLEGAL INDEX INTO THE SOURCE STACK
           IF MV-SRC-CA-I IS GREATER THAN
              COUNT-OF-CARDS OF T-STACKS-T(MV-SRC-ST-I) 
              MOVE 2 TO ERR-CODE OF TABLEAU
              GOBACK
           END-IF.

           IF COUNT-OF-CARDS OF T-STACKS-T(MV-DST-ST-I) IS EQUAL TO 0
      *       FIRST CHECK FOR KING
              MOVE 13 TO ACCEPT-RANK
              IF RANK-N OF CARDS-T(MV-SRC-ST-I, MV-SRC-CA-I)
                 IS NOT EQUAL TO ACCEPT-RANK
                 MOVE 5 TO ERR-CODE OF TABLEAU
              ELSE
      *          MOVE THE KING STACK AND GET OUT OF HERE
                 PERFORM 80-MOVE-CARDS
              END-IF
              GOBACK
           END-IF.

      *    CHECK FOR RANK SUITABILITY
           MOVE RANK-N OF CARDS-T(MV-DST-ST-I, COUNT-OF-CARDS
              OF T-STACKS-T(MV-DST-ST-I)) TO ACCEPT-RANK
           SUBTRACT 1 FROM ACCEPT-RANK

           IF RANK-N OF CARDS-T(MV-SRC-ST-I, MV-SRC-CA-I)
              IS NOT EQUAL TO ACCEPT-RANK
              MOVE 3 TO ERR-CODE OF TABLEAU
              GOBACK
           END-IF.

      *    CHECK FOR SUIT SUITABILITY
           MOVE SUIT-N OF CARDS-T(MV-DST-ST-I, COUNT-OF-CARDS
              OF T-STACKS-T(MV-DST-ST-I)) TO DST-SUIT-OF-CARD

      *    FILL IN THE COMPLEMENTARY SUITS
           IF DST-SUIT-OF-CARD IS EQUAL TO 1 OR
              DST-SUIT-OF-CARD IS EQUAL TO 3
              MOVE 2 TO ACCEPT-S-1
              MOVE 4 TO ACCEPT-S-2
           ELSE
              MOVE 1 TO ACCEPT-S-1
              MOVE 3 TO ACCEPT-S-2
           END-IF.


           MOVE SUIT-N OF CARDS-T(MV-SRC-ST-I, MV-SRC-CA-I)
              TO SRC-SUIT-OF-CARD 

           IF (SRC-SUIT-OF-CARD IS NOT EQUAL TO ACCEPT-S-1)
      *       FIRST OPTION IS ALREADY WRONG
              IF (SRC-SUIT-OF-CARD IS NOT EQUAL TO ACCEPT-S-2)
      *          SECOND OPTION IS A MISS, TOO
                 MOVE 4 TO ERR-CODE OF TABLEAU
                 GOBACK
              END-IF
           END-IF.

      *    SO HERE WE MOVE THE CARDS, AS EVERYTHING SEEMS OK
           PERFORM 80-MOVE-CARDS.

      ******************************************************************
       80-MOVE-CARDS.
      *    SO HERE WE MOVE THE CARDS, AS EVERYTHING SEEMS OK
      *    CALCULATE HOW MANY CARDS NEED TO MOVE
           MOVE COUNT-OF-CARDS OF T-STACKS-T(MV-SRC-ST-I)
              TO MOVE-COUNT
           SUBTRACT MV-SRC-CA-I FROM MOVE-COUNT
           ADD 1 TO MOVE-COUNT

      *    RESET COUNTER FOR XFER TABLE
           MOVE 0 TO XFER-COUNT

      *    LOOP NOW OVER THE COUNT OF CARDS TO BE MOVED
      *    WE STORE THEM IN THE XFER TABLE
           PERFORM VARYING MOVE-COUNT
              FROM MOVE-COUNT BY -1
              UNTIL MOVE-COUNT = 0

      *            DO NOT RE-INVENT THE WHEEL
      *            WE KNOW HOW TO POP A CARD FROM A STACK IN THE
      *            TABLEAU
                   MOVE MV-SRC-ST-I TO STACK-I-IN-SCOPE
                   PERFORM 04-POP-FROM-STACK
      *            MOVE THIS POPPED CARD INTO THE XFER TABLE
                   ADD 1 TO XFER-COUNT
                   MOVE RANK-N OF CARD-IN-SCOPE
                      TO XFER-RANK-N OF XFER-T(XFER-COUNT)
                   MOVE SUIT-N OF CARD-IN-SCOPE
                      TO XFER-SUIT-N OF XFER-T(XFER-COUNT)
           END-PERFORM.

      *    OK, HERE WE HAVE ALL CARDS OF THE PORTION TO MOVE
      *    STORED IN THE XFER TABLE
      *    NOW WE REMOVE THOSE IN REVERSE ORDER INTO THE TABLEAU
           PERFORM VARYING XFER-I
              FROM XFER-COUNT BY -1
              UNTIL XFER-I IS EQUAL TO 0
      *            WE KNOW HOW TO PUSH A CARD TO A STACK IN THE
      *            TABLEAU
                   MOVE XFER-RANK-N OF XFER-T(XFER-COUNT)
                      TO RANK-N OF CARD-IN-SCOPE
                   MOVE XFER-SUIT-N OF XFER-T(XFER-COUNT)
                      TO SUIT-N OF CARD-IN-SCOPE

                   MOVE MV-DST-ST-I TO STACK-I-IN-SCOPE
                   MOVE 3 TO OP-CODE OF TABLEAU
                   PERFORM 03-PUSH-TO-STACK
           END-PERFORM.
           
           MOVE 0 TO ERR-CODE OF TABLEAU.

      ******************************************************************
       99-PRINT.