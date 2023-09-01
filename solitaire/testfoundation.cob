       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTFOUNDATION.

       DATA DIVISION. 

       WORKING-STORAGE SECTION.
      
      ******************************************************************
      * VARIABLES FOR THE TEST RUN
       01 T-RANK-A                PIC X.
       01 T-SUIT-A                PIC X.
       01 TESTS-RUN               PIC 999 VALUE 0.
       01 TESTS-OK                PIC 999 VALUE 0.
       01 TESTS-NOK               PIC 999 VALUE 0.
       01 TESTS-RANK              PIC 99  VALUE 1.

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
             04 COUNT-OF-CARDS    PIC 99  VALUE 0.
      *            NEXT ACCEPTABLE RANK
      *            ALWAYS COUNT-OF-CARDS + 1
             04 NEXT-RANK         PIC 99  VALUE 1.
      *            SIGNAL, IF THE STACK IS FULL
             04 IS-FULL           PIC X.
      *            ALPHA CODE OF RANK OF TOP CARD:
      *            A,2,3,4,5,6,7,8,9,T,J,Q,K             
             04 RANK-A            PIC X.
      *            ALPHA CODE OF SUIT OF TOP CARD:
      *            D(IAMONDS),C(LUB),H(EARTS),S(PADES)
             04 SUIT-A            PIC X.

      ******************************************************************
       PROCEDURE DIVISION.
           DISPLAY "TESTFOUNDATION"           

           PERFORM 01-TEST-RESET.

           PERFORM VARYING REQ-SUIT-TO-PUSH
              FROM 1 BY 1
              UNTIL REQ-SUIT-TO-PUSH > 4
                   PERFORM 02-TEST-SUIT-PUSH-ALL-OK
           END-PERFORM

           PERFORM VARYING REQ-SUIT-TO-PUSH
              FROM 1 BY 1
              UNTIL REQ-SUIT-TO-PUSH > 4
                   PERFORM 03-TEST-SUIT-PUSH-1-TO-MANY
           END-PERFORM

           PERFORM 10-TEST-PICS-OF-TOP-CARD.

           DISPLAY "TESTS RUN: " TESTS-RUN
           DISPLAY "SUCCESSFUL / FAILED: " TESTS-OK " / " TESTS-NOK
           GOBACK.

      ******************************************************************
       01-TEST-RESET.
           PERFORM 01-FOUNDATION-RESET.

           ADD 1 TO TESTS-RUN.
           IF RSP-ERR-CODE OF FOUNDATION IS EQUAL TO 0
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

                   ADD 1 TO TESTS-RUN
                   IF RANK-A OF F-STACKS-T(F-STACK-I) IS EQUAL TO 'X'
                      ADD 1 TO TESTS-OK
                   ELSE
                      ADD 1 TO TESTS-NOK
                      DISPLAY RANK-A OF F-STACKS-T(F-STACK-I)
                   END-IF

                   ADD 1 TO TESTS-RUN
                   IF SUIT-A OF F-STACKS-T(F-STACK-I) IS EQUAL TO 'X'
                      ADD 1 TO TESTS-OK
                   ELSE
                      ADD 1 TO TESTS-NOK
                      DISPLAY SUIT-A OF F-STACKS-T(F-STACK-I)
                   END-IF
           END-PERFORM.

      ******************************************************************
       02-TEST-SUIT-PUSH-ALL-OK.
           PERFORM 01-FOUNDATION-RESET.

           MOVE REQ-SUIT-TO-PUSH TO F-STACK-I.

           PERFORM VARYING TESTS-RANK
              FROM 1 BY 1
              UNTIL TESTS-RANK > 13
                   MOVE 2 TO REQ-OP-CODE OF FOUNDATION
                   CALL 'FOUNDATION' USING REQ-RSP-BLOCK OF FOUNDATION
                   END-CALL

                   ADD 1 TO TESTS-RUN
                   IF RSP-ERR-CODE OF FOUNDATION IS EQUAL TO 0
                      ADD 1 TO TESTS-OK
                   ELSE
                      ADD 1 TO TESTS-NOK
                      DISPLAY "FAILED: 02-TEST-SUIT-PUSH-ALL-OK"
                         WITH NO ADVANCING
                      DISPLAY ": ERR-CODE <> 0"
                   END-IF

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

      ******************************************************************
       03-TEST-SUIT-PUSH-1-TO-MANY.
           PERFORM 01-FOUNDATION-RESET.

           MOVE REQ-SUIT-TO-PUSH TO F-STACK-I.

           PERFORM VARYING TESTS-RANK
              FROM 1 BY 1
              UNTIL TESTS-RANK > 13
                   MOVE 2 TO REQ-OP-CODE OF FOUNDATION
                   CALL 'FOUNDATION' USING REQ-RSP-BLOCK OF FOUNDATION
                   END-CALL
           END-PERFORM.
           MOVE 2 TO REQ-OP-CODE OF FOUNDATION
           CALL 'FOUNDATION' USING REQ-RSP-BLOCK OF FOUNDATION
           END-CALL.
           ADD 1 TO TESTS-RUN
           IF RSP-ERR-CODE OF FOUNDATION IS EQUAL TO 1
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "ERR-CODE=" RSP-ERR-CODE OF FOUNDATION
           END-IF.

      ******************************************************************
       10-TEST-PICS-OF-TOP-CARD.
           PERFORM 01-FOUNDATION-RESET.

           PERFORM VARYING F-STACK-I
              FROM 1 BY 1
              UNTIL F-STACK-I > 4
                   MOVE F-STACK-I TO REQ-SUIT-TO-PUSH
                   PERFORM VARYING TESTS-RANK
                      FROM 1 BY 1
                      UNTIL TESTS-RANK > 13
                           MOVE 2 TO REQ-OP-CODE OF FOUNDATION
                           CALL 'FOUNDATION' USING REQ-RSP-BLOCK
                              OF FOUNDATION
                           END-CALL

                           ADD 1 TO TESTS-RUN
                           IF RSP-ERR-CODE OF FOUNDATION IS EQUAL TO 0
                              ADD 1 TO TESTS-OK
                           ELSE
                              ADD 1 TO TESTS-NOK
                              DISPLAY
                                 "FAILED: 10-TEST-PICS-OF-TOP-CARD:"
                                 WITH NO ADVANCING 
                              DISPLAY " ERR-CODE <> 0"
                           END-IF

      *       THE CARDS KNOW HOW TO MAP THIS
                           MOVE TESTS-RANK TO REQ-RANK-N OF CARDS
                           MOVE F-STACK-I TO REQ-SUIT-N OF CARDS 
                           MOVE 2 TO REQ-OP-CODE OF CARDS
                           CALL 'CARDS' USING REQ-RSP-BLOCK OF CARDS
                           END-CALL

                           MOVE RSP-RANK-A OF CARDS TO RANK-A OF CARD 
                           MOVE RSP-SUIT-A OF CARDS TO SUIT-A OF CARD 

      *                    COMPARE NOW THE DATA OF THE CARD TO THE
      *                    FOUNDATION STACK
      *                    CHECK RANK
                           MOVE RANK-A OF F-STACKS-T(F-STACK-I) TO
                              T-RANK-A
                           ADD 1 TO TESTS-RUN
                           IF RANK-A OF C-RANK IS EQUAL TO T-RANK-A
                              ADD 1 TO TESTS-OK
                           ELSE
                              ADD 1 TO TESTS-NOK
                              DISPLAY
                                 "FAILED: 10-TEST-PICS-OF-TOP-CARD:"
                                 WITH NO ADVANCING 
                              DISPLAY "RANK-A " RANK-A OF C-RANK
                                 WITH NO ADVANCING 
                              DISPLAY " <> " T-RANK-A
                           END-IF

      *                    CHECK SUIT
                           MOVE SUIT-A OF F-STACKS-T(F-STACK-I) TO
                              T-SUIT-A
                           ADD 1 TO TESTS-RUN
                           IF SUIT-A OF C-SUIT IS EQUAL TO T-SUIT-A
                              ADD 1 TO TESTS-OK
                           ELSE
                              ADD 1 TO TESTS-NOK
                              DISPLAY
                                 "FAILED: 10-TEST-PICS-OF-TOP-CARD:"
                                 WITH NO ADVANCING 
                              DISPLAY "SUIT-A " SUIT-A OF C-SUIT
                                 WITH NO ADVANCING 
                              DISPLAY " <> " T-SUIT-A
                           END-IF
                           
                   END-PERFORM
           END-PERFORM.

      ******************************************************************
      * HERE ARE ALL SUPPORT PROCEDURES                                *
      ******************************************************************
       01-FOUNDATION-RESET.
           MOVE 1 TO REQ-OP-CODE OF CARDS.
           CALL 'CARDS' USING REQ-RSP-BLOCK OF CARDS
           END-CALL.

           MOVE 1 TO REQ-OP-CODE OF FOUNDATION.
           CALL 'FOUNDATION' USING REQ-RSP-BLOCK OF FOUNDATION
           END-CALL.