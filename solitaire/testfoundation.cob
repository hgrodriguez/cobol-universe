       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTFOUNDATION.

       DATA DIVISION. 

       WORKING-STORAGE SECTION.
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

      ******************************************************************
      * VARIABLES FOR THE TEST RUN
       01 TESTS-RUN                PIC 99 VALUE 0.
       01 TESTS-OK                 PIC 99 VALUE 0.
       01 TESTS-NOK                PIC 99 VALUE 0.
       01 TESTS-RANK               PIC 99 VALUE 1.

      ******************************************************************
       PROCEDURE DIVISION.
           
           PERFORM 01-TEST-RESET.

           PERFORM VARYING SUIT-TO-PUSH
              FROM 1 BY 1
              UNTIL SUIT-TO-PUSH > 4
                   PERFORM 02-TEST-SUIT-PUSH-ALL-OK
           END-PERFORM

           PERFORM VARYING SUIT-TO-PUSH
              FROM 1 BY 1
              UNTIL SUIT-TO-PUSH > 4
                   PERFORM 03-TEST-SUIT-PUSH-1-TO-MANY
           END-PERFORM

           DISPLAY "TESTS RUN: " TESTS-RUN
           DISPLAY "SUCCESSFUL / FAILED: " TESTS-NOK " / " TESTS-OK
           STOP RUN.

      ******************************************************************
       01-FOUNDATION-RESET.
           MOVE 1 TO OP-CODE.
           CALL 'FOUNDATION' USING GAME
           END-CALL.

      ******************************************************************
       01-TEST-RESET.
           PERFORM 01-FOUNDATION-RESET.

           ADD 1 TO TESTS-RUN.
           IF ERR-CODE IS EQUAL TO 0
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "FAILED: 01-TEST-RESET: ERR-CODE <> 0"
           END-IF.

           PERFORM VARYING F-STACK-I
              FROM 1 BY 1
              UNTIL F-STACK-I > 4
                   ADD 1 TO TESTS-RUN
                   IF COUNT-OF-CARDS OF F-STACKS-T(F-STACK-I)
                      IS EQUAL TO 0
                      ADD 1 TO TESTS-OK
                   ELSE
                      ADD 1 TO TESTS-NOK
                      DISPLAY COUNT-OF-CARDS OF F-STACKS-T(F-STACK-I)
                   END-IF

                   ADD 1 TO TESTS-RUN
                   IF NEXT-RANK OF F-STACKS-T(F-STACK-I) IS EQUAL TO 1
                      ADD 1 TO TESTS-OK
                   ELSE
                      ADD 1 TO TESTS-NOK
                      DISPLAY NEXT-RANK OF F-STACKS-T(F-STACK-I)
                   END-IF

                   ADD 1 TO TESTS-RUN
                   IF IS-FULL OF F-STACKS-T(F-STACK-I) IS EQUAL TO 'N'
                      ADD 1 TO TESTS-OK
                   ELSE
                      ADD 1 TO TESTS-NOK
                      DISPLAY IS-FULL OF F-STACKS-T(F-STACK-I)
                   END-IF
           END-PERFORM.

      ******************************************************************
       02-TEST-SUIT-PUSH-ALL-OK.
           PERFORM 01-FOUNDATION-RESET.

           MOVE SUIT-TO-PUSH TO F-STACK-I.

           PERFORM VARYING TESTS-RANK
              FROM 1 BY 1
              UNTIL TESTS-RANK > 13
                   MOVE 2 TO OP-CODE
                   CALL 'FOUNDATION' USING GAME
                   END-CALL

                   ADD 1 TO TESTS-RUN
                   IF COUNT-OF-CARDS OF F-STACKS-T(F-STACK-I)
                      IS EQUAL TO TESTS-RANK
                      ADD 1 TO TESTS-OK
                   ELSE
                      ADD 1 TO TESTS-NOK
                      DISPLAY COUNT-OF-CARDS OF F-STACKS-T(F-STACK-I)
                   END-IF
      
                   ADD 1 TO TESTS-RUN
                   IF NEXT-RANK OF F-STACKS-T(F-STACK-I)
                      IS EQUAL TO TESTS-RANK + 1
                      ADD 1 TO TESTS-OK
                   ELSE
                      ADD 1 TO TESTS-NOK
                      DISPLAY NEXT-RANK OF F-STACKS-T(F-STACK-I)
                   END-IF
      
                   IF TESTS-RANK IS LESS THAN 13 THEN
                      ADD 1 TO TESTS-RUN
                      IF IS-FULL OF F-STACKS-T(F-STACK-I) IS EQUAL TO
                         'N'
                         ADD 1 TO TESTS-OK
                      ELSE
                         ADD 1 TO TESTS-NOK
                         DISPLAY "L " IS-FULL OF F-STACKS-T(F-STACK-I)
                      END-IF
                   END-IF
           END-PERFORM.
           ADD 1 TO TESTS-RUN
           IF IS-FULL OF F-STACKS-T(F-STACK-I) IS EQUAL TO 'Y'
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "A " IS-FULL OF F-STACKS-T(F-STACK-I)
           END-IF.

       03-TEST-SUIT-PUSH-1-TO-MANY.
           PERFORM 01-FOUNDATION-RESET.

           MOVE SUIT-TO-PUSH TO F-STACK-I.

           PERFORM VARYING TESTS-RANK
              FROM 1 BY 1
              UNTIL TESTS-RANK > 13
                   MOVE 2 TO OP-CODE
                   CALL 'FOUNDATION' USING GAME
                   END-CALL
           END-PERFORM.
           MOVE 2 TO OP-CODE
           CALL 'FOUNDATION' USING GAME
           END-CALL.
           ADD 1 TO TESTS-RUN
           IF ERR-CODE IS EQUAL TO 1
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "ERR-CODE=" ERR-CODE
           END-IF.