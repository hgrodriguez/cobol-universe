       IDENTIFICATION DIVISION.
       PROGRAM-ID. FOUNDATION.

       DATA DIVISION.

       WORKING-STORAGE SECTION. 

       LINKAGE SECTION. 
      ******************************************************************
      *   THE GAME OVERALL.
       01 GAME.
      *      DEFINES ALL FOUNDATION STACKS OF THE GAME
          02 FOUNDATION.
      *      THE OPERATION REQUESTED TO BE PERFORMED ON THE FOUNDATION
             05 OP-CODE            PIC 9.
      *      THE ERROR CODE, IF ANY, FOR THE REQUESTED OPERATION
             05 ERR-CODE           PIC 9.
      *      THE SUIT OF THE CARD TO PUSH ONTO THE FOUNDATION
      *          INTO THE STACK WITH NUMBER SUIT-TO-PUSH.
             05 SUIT-TO-PUSH       PIC 9.
      *      THE FOUNDATION HAS FOUR STACKS TO MAINTAIN
      *      THE INDEX INTO THE SPECIFIC STACK IS DEFINED BY THE
      *          SUIT NUMBER, AS WE HAVE FOUR SUITS
             05 F-STACKS-T OCCURS 4 TIMES INDEXED BY F-STACK-I.
      *         HOW MANY CARDS ARE IN THE STACK.
                10 COUNT-OF-CARDS  PIC 99 VALUE 0.
      *         NEXT ACCEPTABLE RANK
      *         ALWAYS COUNT-OF-CARDS + 1
                10 NEXT-RANK       PIC 99 VALUE 1.
      *         SIGNAL, IF THE STACK IS FULL
                10 IS-FULL         PIC X  VALUE 'N'.

      ******************************************************************
       PROCEDURE DIVISION USING GAME.
           EVALUATE OP-CODE
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
           END-PERFORM.

           MOVE 0 TO ERR-CODE.

      ******************************************************************
       01-PUSH-1-CARD.
           IF COUNT-OF-CARDS OF F-STACKS-T(SUIT-TO-PUSH) IS EQUAL TO 13
              MOVE 1 TO ERR-CODE
              GOBACK
           END-IF
           ADD 1 TO COUNT-OF-CARDS OF F-STACKS-T(SUIT-TO-PUSH).
           ADD 1 TO NEXT-RANK OF F-STACKS-T(SUIT-TO-PUSH).
           IF COUNT-OF-CARDS OF F-STACKS-T(SUIT-TO-PUSH)
              IS EQUAL TO 13 THEN
              MOVE 'Y' TO IS-FULL OF F-STACKS-T(SUIT-TO-PUSH)
           END-IF.