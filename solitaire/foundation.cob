       IDENTIFICATION DIVISION.
       PROGRAM-ID. FOUNDATION.

       DATA DIVISION.

       WORKING-STORAGE SECTION. 

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
           EVALUATE OP-CODE OF FOUNDATION 
           WHEN 1
                PERFORM 01-RESET
           WHEN 2
                PERFORM 01-PUSH-1-CARD
           END-EVALUATE

           GOBACK.
       
      ******************************************************************
       01-RESET.
      *    RUN THROUGH ALL SUITS
           PERFORM VARYING F-STACK-I
              FROM 1 BY 1
              UNTIL F-STACK-I > 4
                   MOVE 0 TO COUNT-OF-CARDS OF F-STACKS-T(F-STACK-I)
                   MOVE 1 TO NEXT-RANK OF F-STACKS-T(F-STACK-I)
                   MOVE 'N' TO IS-FULL OF F-STACKS-T(F-STACK-I)
                   MOVE 'X' TO RANK-A OF F-STACKS-T(F-STACK-I)
                   MOVE 'X' TO SUIT-A OF F-STACKS-T(F-STACK-I)
           END-PERFORM.

           MOVE 0 TO ERR-CODE OF FOUNDATION.

      ******************************************************************
       01-PUSH-1-CARD.
           IF COUNT-OF-CARDS OF F-STACKS-T(SUIT-TO-PUSH) IS EQUAL TO 13
              MOVE 1 TO ERR-CODE OF FOUNDATION
              GOBACK
           END-IF
      *    STACK HAS STILL SPACE FOR ANOTHER CARD
           ADD 1 TO COUNT-OF-CARDS OF F-STACKS-T(SUIT-TO-PUSH).
           ADD 1 TO NEXT-RANK OF F-STACKS-T(SUIT-TO-PUSH).
           IF COUNT-OF-CARDS OF F-STACKS-T(SUIT-TO-PUSH)
              IS EQUAL TO 13 THEN
              MOVE 'Y' TO IS-FULL OF F-STACKS-T(SUIT-TO-PUSH)
           END-IF.
      *    DEFINE THE PICTURE OF THE TOP CARD
      *    THE CARDS KNOW HOW TO MAP THIS
           MOVE COUNT-OF-CARDS OF F-STACKS-T(SUIT-TO-PUSH)
              TO REQ-RANK-N OF CARDS
           MOVE SUIT-TO-PUSH TO REQ-SUIT-N OF CARDS 
           MOVE 2 TO REQ-OP-CODE OF CARDS
           CALL 'CARDS' USING GAME
           END-CALL

           MOVE RSP-RANK-A OF CARDS TO RANK-A OF
              F-STACKS-T(SUIT-TO-PUSH)
           MOVE RSP-SUIT-A OF CARDS TO SUIT-A OF
              F-STACKS-T(SUIT-TO-PUSH).