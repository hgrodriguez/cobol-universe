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
       01 TESTS-NEXT-RANK         PIC 99  VALUE 2.
       01 T-STACK-I               PIC 9.

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
      *      01 -> RESET
      *      02 -> PUSH-1-CARD
      *      03 -> RETURN NUMBER OF CARDS IN STACK
      *      04 -> RETURN NEXT RANK IN STACK
      *      05 -> RETURN THE FULL STATUS OF STACK
      *      06 -> RETURN RANK-A OF STACK
      *      07 -> RETURN SUIT-A OF STACK
      *      99 -> PRINT
             04 REQ-OP-CODE       PIC 99.
      *      THE SUIT OF THE CARD TO PUSH ONTO THE FOUNDATION
      *          INTO THE STACK WITH NUMBER SUIT-TO-PUSH.
             04 REQ-SUIT-TO-PUSH  PIC 9.
      *         THE STACK NUMBER FOR THE REQUEST
             04 REQ-STACK-NUM     PIC 9.
      *      THE ERROR CODE, IF ANY, FOR THE REQUESTED OPERATION
             04 RSP-ERR-CODE      PIC 99.
      *         RESPONSE FOR COUNT OF CARDS IN STACK REQUESTED
             04 RSP-CNT-STACK     PIC 99.
      *         RESPONSE FOR NEXT RANK IN STACK REQUESTED
             04 RSP-NXT-RANK      PIC 99.
      *         RESPONSE FOR IS FULL STATE OF STACK REQUESTED
             04 RSP-IS-FULL       PIC X.
      *         RESPONSE OF ALPHA CODE OF RANK OF TOP CARD OF STACK
      *         REQUESTED
             04 RSP-RANK-A        PIC X.
      *         RESPONSE OF ALPHA CODE OF SUIT OF TOP CARD OF STACK
      *         REQUESTED
             04 RSP-SUIT-A        PIC X.

      ******************************************************************
       PROCEDURE DIVISION.
           DISPLAY "TESTFOUNDATION"           

           PERFORM 01-TEST-RESET.

           MOVE 9 TO REQ-OP-CODE OF FOUNDATION.
           CALL 'FOUNDATION' USING REQ-RSP-BLOCK OF FOUNDATION
           END-CALL.
           DISPLAY ' '

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

           PERFORM 04-ILLEGAL-OP-CODES.

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

           PERFORM VARYING T-STACK-I
              FROM 1 BY 1
              UNTIL T-STACK-I > 4
      *            CHECK NUMBER OF CARDS IN STACK
                   MOVE 3 TO REQ-OP-CODE OF FOUNDATION
                   MOVE T-STACK-I TO REQ-STACK-NUM
                   CALL 'FOUNDATION' USING REQ-RSP-BLOCK OF FOUNDATION
                   END-CALL
                   ADD 1 TO TESTS-RUN
                   IF RSP-CNT-STACK OF FOUNDATION
                      IS EQUAL TO 0
                      ADD 1 TO TESTS-OK
                   ELSE
                      ADD 1 TO TESTS-NOK
                      DISPLAY "FAILED: 01-TEST-RESET: RSP-CNT-STACK = "
                         WITH NO ADVANCING
                      DISPLAY RSP-CNT-STACK " <> 0"
                   END-IF

      *            CHECK NEXT RANK IN STACK
                   MOVE 4 TO REQ-OP-CODE OF FOUNDATION
                   MOVE T-STACK-I TO REQ-STACK-NUM
                   CALL 'FOUNDATION' USING REQ-RSP-BLOCK OF FOUNDATION
                   END-CALL
                   ADD 1 TO TESTS-RUN
                   IF RSP-NXT-RANK OF FOUNDATION IS EQUAL TO 1
                      ADD 1 TO TESTS-OK
                   ELSE
                      ADD 1 TO TESTS-NOK
                      DISPLAY "FAILED: 01-TEST-RESET: RSP-NXT-RANK ="
                         WITH NO ADVANCING 
                      DISPLAY RSP-NXT-RANK OF FOUNDATION " <> 1"
                   END-IF

      *            CHECK IS FULL STATE OF STACK
                   MOVE 5 TO REQ-OP-CODE OF FOUNDATION
                   MOVE T-STACK-I TO REQ-STACK-NUM
                   CALL 'FOUNDATION' USING REQ-RSP-BLOCK OF FOUNDATION
                   END-CALL
                   ADD 1 TO TESTS-RUN
                   IF RSP-IS-FULL OF FOUNDATION IS EQUAL TO 'N'
                      ADD 1 TO TESTS-OK
                   ELSE
                      ADD 1 TO TESTS-NOK
                      DISPLAY "FAILED: 01-TEST-RESET: RSP-IS-FULL ="
                         WITH NO ADVANCING 
                      DISPLAY RSP-IS-FULL OF FOUNDATION
                         WITH NO ADVANCING 
                      DISPLAY " <> N"
                   END-IF

      *            CHECK RANK-A OF STACK
                   MOVE 6 TO REQ-OP-CODE OF FOUNDATION
                   MOVE T-STACK-I TO REQ-STACK-NUM
                   CALL 'FOUNDATION' USING REQ-RSP-BLOCK OF FOUNDATION
                   END-CALL
                   ADD 1 TO TESTS-RUN
                   IF RSP-RANK-A OF FOUNDATION IS EQUAL TO 'X'
                      ADD 1 TO TESTS-OK
                   ELSE
                      ADD 1 TO TESTS-NOK
                      DISPLAY "FAILED: 01-TEST-RESET: RSP-RANK-A ="
                         WITH NO ADVANCING 
                      DISPLAY RSP-RANK-A OF FOUNDATION
                         WITH NO ADVANCING 
                      DISPLAY " <> X"
                   END-IF

      *            CHECK SUIT-A OF STACK
                   MOVE 7 TO REQ-OP-CODE OF FOUNDATION
                   MOVE T-STACK-I TO REQ-STACK-NUM
                   CALL 'FOUNDATION' USING REQ-RSP-BLOCK OF FOUNDATION
                   END-CALL
                   ADD 1 TO TESTS-RUN
                   IF RSP-SUIT-A OF FOUNDATION IS EQUAL TO 'X'
                      ADD 1 TO TESTS-OK
                   ELSE
                      ADD 1 TO TESTS-NOK
                      DISPLAY "FAILED: 01-TEST-RESET: RSP-RANK-A ="
                         WITH NO ADVANCING 
                      DISPLAY RSP-SUIT-A OF FOUNDATION
                         WITH NO ADVANCING 
                      DISPLAY " <> X"
                   END-IF

           END-PERFORM.

      ******************************************************************
       02-TEST-SUIT-PUSH-ALL-OK.
           PERFORM 01-FOUNDATION-RESET.

           MOVE REQ-SUIT-TO-PUSH TO T-STACK-I.

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

      *            CHECK NUMBER OF CARDS IN STACK
                   MOVE 3 TO REQ-OP-CODE OF FOUNDATION
                   MOVE T-STACK-I TO REQ-STACK-NUM
                   CALL 'FOUNDATION' USING REQ-RSP-BLOCK OF FOUNDATION
                   END-CALL
                   ADD 1 TO TESTS-RUN
                   IF RSP-CNT-STACK OF FOUNDATION
                      IS EQUAL TO TESTS-RANK
                      ADD 1 TO TESTS-OK
                   ELSE
                      ADD 1 TO TESTS-NOK
                      DISPLAY "FAILED: 02-TEST-SUIT-PUSH-ALL-OK:"
                         WITH NO ADVANCING 
                      DISPLAY " RSP-CNT-STACK = "
                         WITH NO ADVANCING
                      DISPLAY RSP-CNT-STACK " <> " TESTS-RANK
                   END-IF

      *            CHECK NEXT RANK IN STACK
                   MOVE 4 TO REQ-OP-CODE OF FOUNDATION
                   MOVE T-STACK-I TO REQ-STACK-NUM
                   CALL 'FOUNDATION' USING REQ-RSP-BLOCK OF FOUNDATION
                   END-CALL
                   ADD 1 TO TESTS-RUN
                   MOVE TESTS-RANK TO TESTS-NEXT-RANK
                   ADD 1 TO TESTS-NEXT-RANK 
                   IF RSP-NXT-RANK OF FOUNDATION IS
                      EQUAL TO TESTS-NEXT-RANK
                      ADD 1 TO TESTS-OK
                   ELSE
                      ADD 1 TO TESTS-NOK
                      DISPLAY "FAILED: 02-TEST-SUIT-PUSH-ALL-OK:"
                         WITH NO ADVANCING 
                      DISPLAY "RSP-NXT-RANK ="
                         WITH NO ADVANCING 
                      DISPLAY RSP-NXT-RANK OF FOUNDATION " <> "
                         WITH NO ADVANCING 
                      DISPLAY TESTS-NEXT-RANK
                   END-IF

                   IF TESTS-RANK IS LESS THAN 13 THEN
      *               CHECK IS FULL STATE OF STACK
                      MOVE 5 TO REQ-OP-CODE OF FOUNDATION
                      MOVE T-STACK-I TO REQ-STACK-NUM
                      CALL 'FOUNDATION' USING REQ-RSP-BLOCK OF
                         FOUNDATION
                      END-CALL
                      ADD 1 TO TESTS-RUN
                      IF RSP-IS-FULL OF FOUNDATION IS EQUAL TO 'N'
                         ADD 1 TO TESTS-OK
                      ELSE
                         ADD 1 TO TESTS-NOK
                         DISPLAY "FAILED: 02-TEST-SUIT-PUSH-ALL-OK:"
                            WITH NO ADVANCING 
                         DISPLAY "RSP-IS-FULLK ="
                            WITH NO ADVANCING 
                         DISPLAY RSP-IS-FULL OF FOUNDATION
                            WITH NO ADVANCING 
                         DISPLAY " <> N"
                      END-IF
                   END-IF
           END-PERFORM.

      *    CHECK IS FULL STATE OF STACK
           MOVE 5 TO REQ-OP-CODE OF FOUNDATION
           MOVE T-STACK-I TO REQ-STACK-NUM
           CALL 'FOUNDATION' USING REQ-RSP-BLOCK OF
              FOUNDATION
           END-CALL
           ADD 1 TO TESTS-RUN
           IF RSP-IS-FULL OF FOUNDATION IS EQUAL TO 'Y'
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "FAILED: 02-TEST-SUIT-PUSH-ALL-OK:"
                 WITH NO ADVANCING 
              DISPLAY "RSP-IS-FULLK ="
                 WITH NO ADVANCING 
              DISPLAY RSP-IS-FULL OF FOUNDATION
                 WITH NO ADVANCING 
              DISPLAY " <> Y"
           END-IF.

      ******************************************************************
       03-TEST-SUIT-PUSH-1-TO-MANY.
           PERFORM 01-FOUNDATION-RESET.

           MOVE REQ-SUIT-TO-PUSH TO T-STACK-I.

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
       04-ILLEGAL-OP-CODES.
           MOVE 0 TO REQ-OP-CODE OF FOUNDATION
           CALL 'FOUNDATION' USING REQ-RSP-BLOCK OF FOUNDATION
           END-CALL.
           ADD 1 TO TESTS-RUN
           IF RSP-ERR-CODE OF FOUNDATION IS EQUAL TO 2
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "04-ILLEGAL-OP-CODES=0:" WITH NO ADVANCING 
              DISPLAY " ERR-CODE=" RSP-ERR-CODE OF FOUNDATION
                 WITH NO ADVANCING 
              DISPLAY " <> 2"
           END-IF.

           MOVE 8 TO REQ-OP-CODE OF FOUNDATION
           CALL 'FOUNDATION' USING REQ-RSP-BLOCK OF FOUNDATION
           END-CALL.
           ADD 1 TO TESTS-RUN
           IF RSP-ERR-CODE OF FOUNDATION IS EQUAL TO 2
              ADD 1 TO TESTS-OK
           ELSE
              ADD 1 TO TESTS-NOK
              DISPLAY "04-ILLEGAL-OP-CODES=3:" WITH NO ADVANCING 
              DISPLAY " ERR-CODE=" RSP-ERR-CODE OF FOUNDATION
                 WITH NO ADVANCING 
              DISPLAY " <> 2"
           END-IF.

      ******************************************************************
       10-TEST-PICS-OF-TOP-CARD.
           PERFORM 01-FOUNDATION-RESET.

           PERFORM VARYING T-STACK-I
              FROM 1 BY 1
              UNTIL T-STACK-I > 4
                   MOVE T-STACK-I TO REQ-SUIT-TO-PUSH
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
                           MOVE T-STACK-I TO REQ-SUIT-N OF CARDS 
                           MOVE 2 TO REQ-OP-CODE OF CARDS
                           CALL 'CARDS' USING REQ-RSP-BLOCK OF CARDS
                           END-CALL

                           MOVE RSP-RANK-A OF CARDS TO RANK-A OF CARD 
                           MOVE RSP-SUIT-A OF CARDS TO SUIT-A OF CARD 

      *                    COMPARE NOW THE DATA OF THE CARD TO THE
      *                    FOUNDATION STACK
      *                    CHECK RANK-A OF STACK
                           MOVE 6 TO REQ-OP-CODE OF FOUNDATION
                           MOVE T-STACK-I TO REQ-STACK-NUM
                           CALL 'FOUNDATION' USING REQ-RSP-BLOCK OF
                              FOUNDATION
                           END-CALL
                           MOVE RSP-RANK-A OF FOUNDATION TO
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

      *                    CHECK SUIT-A OF STACK
                           MOVE 7 TO REQ-OP-CODE OF FOUNDATION
                           MOVE T-STACK-I TO REQ-STACK-NUM
                           CALL 'FOUNDATION' USING REQ-RSP-BLOCK OF
                              FOUNDATION
                           END-CALL
                           MOVE RSP-SUIT-A OF FOUNDATION TO
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