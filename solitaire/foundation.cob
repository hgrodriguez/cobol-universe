       IDENTIFICATION DIVISION.
       PROGRAM-ID. FOUNDATION.

       DATA DIVISION.

       WORKING-STORAGE SECTION. 

      ******************************************************************
       01 CARDS.
      *      THE REQUEST-RESPONSE-BLOCK
          03 REQ-RSP-BLOCK.
      *            THE OPERATION REQUESTED TO BE PERFORMED
      *            1 = INITIALIZE CARDS
             04 REQ-OP-CODE       PIC 9.
      *            RANK NUMBER
             04 REQ-RANK-N        PIC 99.
      *            SUIT NUMBER
             04 REQ-SUIT-N        PIC 9.
      *            THE ERROR CODE, IF ANY, FOR THE REQUESTED OPERATION
      *            1 = ILLEGAL OP
      *            2 = ILLEGAL RANK: LOWER THAN MIN
      *            3 = ILLEGAL RANK: HIGHER THAN MAX
      *            4 = ILLEGAL SUIT: LOWER THAN MIN
      *            5 = ILLEGAL SUIT: HIGHER THAN MAX
             04 RSP-ERR-CODE      PIC 99.
      *            RANK ALPHA CODE OF REQUESTED RANK NUMBER
             04 RSP-RANK-A        PIC X.
      *            SUIT ALPHA CODE OF REQUESTED SUIT NUMBER
             04 RSP-SUIT-A        PIC X.

       LINKAGE SECTION. 
      ******************************************************************
      *      DEFINES ALL FOUNDATION STACKS OF THE GAME
       01 FOUNDATION.
          03 REQ-RSP-BLOCK.
      *      THE OPERATION REQUESTED TO BE PERFORMED ON THE FOUNDATION
             04 REQ-OP-CODE       PIC 9.
      *      THE SUIT OF THE CARD TO PUSH ONTO THE FOUNDATION
      *          INTO THE STACK WITH NUMBER SUIT-TO-PUSH.
             04 REQ-SUIT-TO-PUSH  PIC 9.
      *      THE ERROR CODE, IF ANY, FOR THE REQUESTED OPERATION
             04 RSP-ERR-CODE      PIC 9.
      *          THE FOUNDATION HAS FOUR STACKS TO MAINTAIN
      *          THE INDEX INTO THE SPECIFIC STACK IS DEFINED BY THE
      *          SUIT NUMBER, AS WE HAVE FOUR SUITS
          03 F-STACKS-T OCCURS 4 TIMES INDEXED BY F-STACK-I.
      *            HOW MANY CARDS ARE IN THE STACK.
             04 COUNT-OF-CARDS    PIC 99 VALUE 0.
      *            NEXT ACCEPTABLE RANK
      *            ALWAYS COUNT-OF-CARDS + 1
             04 NEXT-RANK         PIC 99 VALUE 1.
      *            SIGNAL, IF THE STACK IS FULL
             04 IS-FULL           PIC X.
      *            ALPHA CODE OF RANK OF TOP CARD:
      *            A,2,3,4,5,6,7,8,9,T,J,Q,K             
             04 RANK-A            PIC X.
      *            ALPHA CODE OF SUIT OF TOP CARD:
      *            D(IAMONDS),C(LUB),H(EARTS),S(PADES)
             04 SUIT-A            PIC X.

      ******************************************************************
       PROCEDURE DIVISION USING FOUNDATION.
           EVALUATE REQ-OP-CODE OF FOUNDATION 
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

           MOVE 0 TO RSP-ERR-CODE OF FOUNDATION.

      ******************************************************************
       01-PUSH-1-CARD.
           IF COUNT-OF-CARDS OF F-STACKS-T(REQ-SUIT-TO-PUSH)
              IS EQUAL TO 13
              MOVE 1 TO RSP-ERR-CODE OF FOUNDATION
              GOBACK
           END-IF
      *    STACK HAS STILL SPACE FOR ANOTHER CARD
           ADD 1 TO COUNT-OF-CARDS OF F-STACKS-T(REQ-SUIT-TO-PUSH).
           ADD 1 TO NEXT-RANK OF F-STACKS-T(REQ-SUIT-TO-PUSH).
           IF COUNT-OF-CARDS OF F-STACKS-T(REQ-SUIT-TO-PUSH)
              IS EQUAL TO 13 THEN
              MOVE 'Y' TO IS-FULL OF F-STACKS-T(REQ-SUIT-TO-PUSH)
           END-IF.
      *    DEFINE THE PICTURE OF THE TOP CARD
      *    THE CARDS KNOW HOW TO MAP THIS
           MOVE COUNT-OF-CARDS OF F-STACKS-T(REQ-SUIT-TO-PUSH)
              TO REQ-RANK-N OF CARDS
           MOVE REQ-SUIT-TO-PUSH TO REQ-SUIT-N OF CARDS 
           MOVE 2 TO REQ-OP-CODE OF CARDS
           CALL 'CARDS' USING REQ-RSP-BLOCK IN CARDS
           END-CALL

           MOVE RSP-RANK-A OF CARDS TO RANK-A OF
              F-STACKS-T(REQ-SUIT-TO-PUSH)
           MOVE RSP-SUIT-A OF CARDS TO SUIT-A OF
              F-STACKS-T(REQ-SUIT-TO-PUSH).