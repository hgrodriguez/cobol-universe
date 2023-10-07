       IDENTIFICATION DIVISION.
       PROGRAM-ID. SOLITAIRE.

       DATA DIVISION. 

       WORKING-STORAGE SECTION. 

      ******************************************************************
      *   UNTIL THE USER DECIDES TO QUIT, WE RUN AGAIN
       01 STAY-OPEN                PIC X  VALUE 'Y'.

      ******************************************************************
      *   DEFINES THE USER MENU SELECTION
       01 USER-SELECTION.
          02 MENU-TO-SHOW          PIC 99.
          02 MENU-ENTRY-SELECTED   PIC X.

      ******************************************************************
      *   THIS WILL BE 1, IF WE MOVED ONE MANDATORY CARD
       01 M-CARD-WAS-MOVED         PIC 9.
      *   FLAG, IF AT LEAST ONE MANDATORY CARD WAS MOVED,
      *   THEN WE HAVE TO PRINT THE GAME AGAIN
       01 M-PRINT-GAME             PIC 9.
      *   THIS IS ONE CARD WHICH IS MOVING AROUND
       01 CARD.
          02 RANK-N                PIC 99.
          02 SUIT-N                PIC 9.
      *   THE MAXIMUM NUMBER OF CARDS WHICH CAN BE FETCHED FROM
      *   THE STOCK.
       01 MAX-TO-FETCH             PIC 99.     
      *   THE FETCH INDEX
       01 FETCH-INDEX              PIC 9.
       
      ******************************************************************
      *   CARDS API
       01 CARDS.
      *      THE REQUEST-RESPONSE-BLOCK
          02 REQ-RSP-BLOCK.
      *            THE OPERATION REQUESTED TO BE PERFORMED
      *            1 = INITIALIZE CARDS
             03 REQ-OP-CODE        PIC 9.
      *            RANK NUMBER
             03 REQ-RANK-N         PIC 99.
      *            SUIT NUMBER
             03 REQ-SUIT-N         PIC 9.
      *            THE ERROR CODE, IF ANY, FOR THE REQUESTED OPERATION
      *            1 = ILLEGAL OP
      *            2 = ILLEGAL RANK: LOWER THAN MIN
      *            3 = ILLEGAL RANK: HIGHER THAN MAX
      *            4 = ILLEGAL SUIT: LOWER THAN MIN
      *            5 = ILLEGAL SUIT: HIGHER THAN MAX
             03 RSP-ERR-CODE       PIC 99.
      *            RANK ALPHA CODE OF REQUESTED RANK NUMBER
             03 RSP-RANK-A         PIC X.
      *            SUIT ALPHA CODE OF REQUESTED SUIT NUMBER
             03 RSP-SUIT-A         PIC X.
      *   STOCK API
       01 STOCK.
          03 REQ-RSP-BLOCK.
      *         THE OPERATION REQUESTED TO BE PERFORMED ON THE STOCK
      *         01 -> FILL-STOCK
      *         02 -> RANDOMIZE-STOCK
      *         03 -> FETCH-CARD
      *         04 -> TOGGLE-PEEK
      *         05 -> PRINT-TOS
      *         06 -> RETURN-NUM-CARDS
      *         07 -> RETURN-CARD-INDEX
      *         99 -> PRINT-STOCK
             04 REQ-OP-CODE        PIC 99.
             04 REQ-CARD-INDEX     PIC 99.
      *      THE ERROR CODE, IF ANY, FOR THE REQUESTED OPERATION
      *            1 = ILLEGAL OP CODE
      *            2 = NO CARDS LEFT
             04 RSP-ERR-CODE       PIC 99.
      *      THE CARD FETCHED FROM THE STOCK
             04 RSP-CARD-FETCHED.
                05 RSP-RANK-N      PIC 99.
                05 RSP-SUIT-N      PIC 9.
      *         TOP OF STOCK PRINT REPRESENTATION
             04 RSP-TOS-PEEK       PIC 9.
             04 RSP-TOS-RANK-A     PIC X.
             04 RSP-TOS-SUIT-A     PIC X.
             04 RSP-NUM-OF-CARDS   PIC 99.
      *   FOUNDATION API
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
             04 REQ-OP-CODE        PIC 99.
      *      THE SUIT OF THE CARD TO PUSH ONTO THE FOUNDATION
      *          INTO THE STACK WITH NUMBER SUIT-TO-PUSH.
             04 REQ-SUIT-TO-PUSH   PIC 9.
      *         THE STACK NUMBER FOR THE REQUEST
             04 REQ-STACK-NUM      PIC 9.
      *      THE ERROR CODE, IF ANY, FOR THE REQUESTED OPERATION
             04 RSP-ERR-CODE       PIC 99.
      *         RESPONSE FOR COUNT OF CARDS IN STACK REQUESTED
             04 RSP-CNT-STACK      PIC 99.
      *         RESPONSE FOR NEXT RANK IN STACK REQUESTED
             04 RSP-NXT-RANK       PIC 99.
      *         RESPONSE FOR IS FULL STATE OF STACK REQUESTED
             04 RSP-IS-FULL        PIC X.
      *         RESPONSE OF ALPHA CODE OF RANK OF TOP CARD OF STACK
      *         REQUESTED
             04 RSP-RANK-A         PIC X.
      *         RESPONSE OF ALPHA CODE OF SUIT OF TOP CARD OF STACK
      *         REQUESTED
             04 RSP-SUIT-A         PIC X.
      *   TABLEAU API
       01 TABLEAU.
          02 REQ-RSP-BLOCK.
      *      THE OPERATION REQUESTED TO BE PERFORMED ON THE TABLEAU
      *         01 -> RESET
      *         02 -> INIT-FROM-STOCK
      *         03 -> PUSH-TO-STACK
      *         04 -> POP-FROM-STACK
      *         05 -> MANDATORY-CHECK
      *         06 -> MOVE-CARDS
      *         07 -> NUMBER OF CARDS IN TABLEAU
      *         08 -> NUMBER OF CARDS IN REQ STACK
      *         09 -> RETURN CARD FROM (STACK, IDX)
      *         99 -> PRINT
             05 REQ-OP-CODE        PIC 99.
      *         THE STACK-INDEX IN SCOPE FOR THE REQUESTED OPERATION
             05 REQ-STCK-IDX       PIC 9.
      *         THE CARD-INDEX IN SCOPE FOR THE REQUESTED OPERATION
             05 REQ-CARD-IDX       PIC 99.
      *         THE CARD IN SCOPE FOR THE REQUESTED OPERATION
             05 CARD-IN-SCOPE.
                26 RANK-N          PIC 99.
                26 SUIT-N          PIC 9.
      *      THE ERROR CODE, IF ANY, FOR THE REQUESTED OPERATION
             05 RSP-ERR-CODE       PIC 9.
      *         NUMBER OF CARDS IN TABLEAU/STACK REQUESTED
             05 RSP-NUM-CARDS      PIC 99.
      *         WHICH STACK IS TO BE USED FOR MANDATORY CARD MOVE
             05 RSP-MNDT-STCK-IDX  PIC 9.
      *         THE RESPONSE CARD IN SCOPE FOR THE REQUESTED OPERATION
             05 RSP-CARD.
                26 RANK-N          PIC 99.
                26 SUIT-N          PIC 9.
      *         DATA WE NEED FOR MOVING CARDS IN THE TABLEAU
      *         SOURCE STACK INDEX
             05 MV-SRC-ST-I        PIC 9.
      *         SOURCE CARD INDEX IN THE SOURCE STACK INDEX
             05 MV-SRC-CA-I        PIC 99.
      *         DESTINATION STACK INDEX
             05 MV-DST-ST-I        PIC 9.

      ******************************************************************
       PROCEDURE DIVISION.
           
           PERFORM START-GAME
           
           PERFORM UNTIL STAY-OPEN IS EQUAL TO 'N'

                   PERFORM MOVE-MANDATORY-CARDS
                   PERFORM DISPLAY-GAME

                   MOVE 1 TO MENU-TO-SHOW
                   CALL 'MENUS' USING USER-SELECTION
                   END-CALL

                   EVALUATE MENU-ENTRY-SELECTED
                   WHEN 'F'
                        PERFORM FETCH-FROM-STOCK
                   WHEN 'H'
                        PERFORM SHOW-HELP
                   WHEN 'M'
                        PERFORM MOVE-CARD
                   WHEN 'Q'
                        DISPLAY "QUITTING."
                        MOVE 'N' TO STAY-OPEN
                   WHEN 'S'
                        PERFORM START-GAME
                   END-EVALUATE
           END-PERFORM.
           STOP RUN.

      ******************************************************************
       START-GAME.
           PERFORM INITIALIZE-WORLD.

      ******************************************************************
       DISPLAY-GAME.
           DISPLAY '   ' WITH NO ADVANCING.
           MOVE 99 TO REQ-OP-CODE OF FOUNDATION.
           CALL 'FOUNDATION' USING REQ-RSP-BLOCK OF FOUNDATION
           END-CALL.
           DISPLAY ' ' WITH NO ADVANCING.

           DISPLAY '     ' WITH NO ADVANCING.
           MOVE 8 TO REQ-OP-CODE OF STOCK.
           CALL 'STOCK' USING REQ-RSP-BLOCK OF STOCK
           END-CALL.
           DISPLAY ' '.

           MOVE 99 TO REQ-OP-CODE OF TABLEAU
           CALL 'TABLEAU' USING REQ-RSP-BLOCK OF TABLEAU
           END-CALL.
           DISPLAY ' '.

      ******************************************************************
       FETCH-FROM-STOCK.
           MOVE 6 TO REQ-OP-CODE OF STOCK.
           CALL 'STOCK' USING REQ-RSP-BLOCK OF STOCK
           END-CALL.           
           IF RSP-NUM-OF-CARDS IS EQUAL TO 0 THEN
              DISPLAY 'STOCK IS EMPTY, NOTHING CAN BE FETCHED'
           ELSE
      *    DEFINE HOW MANY CARDS WE CAN FETCH     
              IF RSP-NUM-OF-CARDS IS GREATER THAN 7 THEN
                 MOVE 7 TO MAX-TO-FETCH
              ELSE
                 MOVE RSP-NUM-OF-CARDS TO MAX-TO-FETCH
              END-IF

      *    FIRST CARD TO MOVE WILL GO INTO STACK #1 OF TABLEAU     
              MOVE 1 TO REQ-STCK-IDX
              PERFORM VARYING FETCH-INDEX
                 FROM 1 BY 1
                 UNTIL FETCH-INDEX IS GREATER THAN MAX-TO-FETCH

                      MOVE 3 TO REQ-OP-CODE OF STOCK
                      CALL 'STOCK' USING REQ-RSP-BLOCK OF STOCK
                      END-CALL

      *    MOVE CARD FROM STOCK TO TABLEAU
                      MOVE RSP-RANK-N OF RSP-CARD-FETCHED
                         TO RANK-N OF CARD-IN-SCOPE 
                      MOVE RSP-SUIT-N OF RSP-CARD-FETCHED
                         TO SUIT-N OF CARD-IN-SCOPE 
                      MOVE 3 TO REQ-OP-CODE OF TABLEAU
                      CALL 'TABLEAU' USING TABLEAU
                      END-CALL
                      ADD 1 TO REQ-STCK-IDX
              END-PERFORM
           END-IF.

      ******************************************************************
       MOVE-CARD.
           DISPLAY 'MOVING CARDS'
           DISPLAY 'WHICH STACK TO MOVE FROM (1-7)? ' WITH NO ADVANCING
           ACCEPT MV-SRC-ST-I
           DISPLAY 'WHICH CARD INDEX TO MOVE FROM? ' WITH NO ADVANCING
           ACCEPT MV-SRC-CA-I
           DISPLAY 'WHICH STACK TO MOVE TO (1-7)? ' WITH NO ADVANCING
           ACCEPT MV-DST-ST-I

           MOVE 6 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL     
           EVALUATE RSP-ERR-CODE OF REQ-RSP-BLOCK OF TABLEAU
           WHEN 0 
      *    MOVE THE CARD(S)
                MOVE 6 TO REQ-OP-CODE OF TABLEAU
                CALL 'TABLEAU' USING TABLEAU
                END-CALL
           WHEN 1
                DISPLAY 'SOURCE STACK IS EMPTY'
           WHEN 2 
                DISPLAY 'ILLEGAL CARD INDEX'
           WHEN 3 
                DISPLAY 'RANK DOES NOT MATCH'
           WHEN 4 
                DISPLAY 'SUIT DOES NOT MATCH'
           WHEN 5 
                DISPLAY 'KING ONLY ON EMTPY STACK'
           WHEN OTHER 
                DISPLAY "INTERNAL ERROR, CALL THE ENGINEERS"
           END-EVALUATE.


      ******************************************************************
       MOVE-MANDATORY-CARDS.
           MOVE 0 TO M-PRINT-GAME.
           MOVE 0 TO M-CARD-WAS-MOVED.
           PERFORM WITH TEST AFTER UNTIL M-CARD-WAS-MOVED IS EQUAL TO 0
                   MOVE 0 TO M-CARD-WAS-MOVED
      *            WE GO THROUGH ALL STACKS IN THE FOUNDATION
      *            THE STACK INDEX IS EQUAL TO THE SUIT
                   PERFORM VARYING REQ-STACK-NUM
                      FROM 1 BY 1
                      UNTIL REQ-STACK-NUM > 4
      *    CHECK IF STACK IS FULL, THEN NOTHING TO MOVE ANYMORE
                           MOVE 5 TO REQ-OP-CODE OF FOUNDATION
                           CALL 'FOUNDATION' USING REQ-RSP-BLOCK OF
                              FOUNDATION
                           END-CALL
                           IF RSP-IS-FULL OF FOUNDATION IS EQUAL TO 'N'
                              THEN
      *    CHECK NEXT RANK IN STACK
                              MOVE 4 TO REQ-OP-CODE OF FOUNDATION
                              CALL 'FOUNDATION' USING REQ-RSP-BLOCK OF
                                 FOUNDATION
                              END-CALL
      *    NOW WE CHECK THE TABLEAU, IF ANY TOS IS THE ONE WE NEED FOR
      *    MOVING ONTO THE FOUNDATION
                              MOVE RSP-NXT-RANK TO RANK-N OF
                                 CARD-IN-SCOPE
                              MOVE REQ-STACK-NUM TO SUIT-N OF
                                 CARD-IN-SCOPE 
                              MOVE 5 TO REQ-OP-CODE OF TABLEAU 
                              CALL 'TABLEAU' USING TABLEAU
                              END-CALL
                              IF RSP-ERR-CODE OF TABLEAU IS EQUAL TO 0
                                 THEN
      *    WE FOUND A CANDIDATE TO MOVE
      *    POP THIS CARD FROM THE TABLEAU
                                 MOVE RSP-MNDT-STCK-IDX TO REQ-STCK-IDX
                                 MOVE 4 TO REQ-OP-CODE OF TABLEAU
                                 CALL 'TABLEAU' USING TABLEAU
                                 END-CALL
      *    PUSH THIS CARD ONTO THE FOUNDATION
                                 MOVE 2 TO REQ-OP-CODE OF FOUNDATION
                                 MOVE REQ-STACK-NUM TO REQ-SUIT-TO-PUSH
                                 CALL 'FOUNDATION' USING REQ-RSP-BLOCK
                                    OF FOUNDATION
                                 END-CALL
                                 MOVE 1 TO M-CARD-WAS-MOVED
                              END-IF
                           END-IF
                   END-PERFORM
           END-PERFORM.

      ******************************************************************
       SHOW-HELP.
           DISPLAY 'HELP OVERVIEW'.

      ******************************************************************
       INITIALIZE-WORLD.
           MOVE 1 TO REQ-OP-CODE OF CARDS.
           CALL 'CARDS' USING REQ-RSP-BLOCK OF CARDS
           END-CALL.
           
           MOVE 1 TO REQ-OP-CODE OF STOCK.
           CALL 'STOCK' USING REQ-RSP-BLOCK OF STOCK
           END-CALL.           

           MOVE 2 TO REQ-OP-CODE OF STOCK.
           CALL 'STOCK' USING REQ-RSP-BLOCK OF STOCK
           END-CALL.           

           MOVE 1 TO REQ-OP-CODE OF FOUNDATION.
           CALL 'FOUNDATION' USING REQ-RSP-BLOCK OF FOUNDATION
           END-CALL.

           MOVE 1 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.

           MOVE 2 TO REQ-OP-CODE OF TABLEAU.
           CALL 'TABLEAU' USING TABLEAU
           END-CALL.
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