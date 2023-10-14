      ******************************************************************
      * THIS IS THE MAIN PROGRAM FOR THE SOLITAIRE GAME
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
          02 MENU-PARAMETER        PIC X.

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
       01 CARDS-API.
      *   DOCUMENTATION: SEE CARDS.COB
          02 REQ-RSP-BLOCK.
             03 REQ-OP-CODE        PIC 9.
             03 REQ-RANK-N         PIC 99.
             03 REQ-SUIT-N         PIC 9.
             03 RSP-ERR-CODE       PIC 99.
             03 RSP-RANK-A         PIC X.
             03 RSP-SUIT-A         PIC X.

      ******************************************************************
       01 STOCK-API.
      *   DOCUMENTATION: SEE STOCK.COB
          02 REQ-RSP-BLOCK.
             03 REQ-OP-CODE        PIC 99.
             03 REQ-CARD-INDEX     PIC 99.
             03 RSP-ERR-CODE       PIC 99.
             03 RSP-CARD-FETCHED.
                04 RSP-RANK-N      PIC 99.
                04 RSP-SUIT-N      PIC 9.
             03 RSP-TOS-PEEK       PIC 9.
             03 RSP-TOS-RANK-A     PIC X.
             03 RSP-TOS-SUIT-A     PIC X.
             03 RSP-NUM-OF-CARDS   PIC 99.

      ******************************************************************
       01 FOUNDATION-API.
      *   DOCUMENTATION: SEE FOUNDATION.COB
          02 REQ-RSP-BLOCK.
             03 REQ-OP-CODE        PIC 99.
             03 REQ-SUIT-TO-PUSH   PIC 9.
             03 REQ-STACK-NUM      PIC 9.
             03 RSP-ERR-CODE       PIC 99.
             03 RSP-CNT-STACK      PIC 99.
             03 RSP-NXT-RANK       PIC 99.
             03 RSP-IS-FULL        PIC X.
             03 RSP-RANK-A         PIC X.
             03 RSP-SUIT-A         PIC X.

      ******************************************************************
       01 TABLEAU-API.
      *   DOCUMENTATION: SEE TABLEAU.COB
          02 REQ-RSP-BLOCK.
             05 REQ-OP-CODE        PIC 99.
             05 REQ-STCK-IDX       PIC 9.
             05 REQ-CARD-IDX       PIC 99.
             05 CARD-IN-SCOPE.
                26 RANK-N          PIC 99.
                26 SUIT-N          PIC 9.
             05 RSP-ERR-CODE       PIC 9.
             05 RSP-NUM-CARDS      PIC 99.
             05 RSP-MNDT-STCK-IDX  PIC 9.
             05 RSP-CARD.
                26 RANK-N          PIC 99.
                26 SUIT-N          PIC 9.
             05 MV-SRC-ST-I        PIC 9.
             05 MV-SRC-CA-I        PIC 99.
             05 MV-DST-ST-I        PIC 9.

      ******************************************************************
       PROCEDURE DIVISION.
           
           PERFORM INITIALIZE-WORLD
           
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
                   WHEN 'R'
                        PERFORM INITIALIZE-WORLD
                   WHEN 'S'
                        PERFORM SETTINGS-DIALOG
                   END-EVALUATE
           END-PERFORM.
           STOP RUN.

      ******************************************************************
       DISPLAY-GAME.
           DISPLAY '   ' WITH NO ADVANCING.
           MOVE 99 TO REQ-OP-CODE OF FOUNDATION-API.
           CALL 'FOUNDATION' USING REQ-RSP-BLOCK OF FOUNDATION-API
           END-CALL.
           DISPLAY ' ' WITH NO ADVANCING.

           DISPLAY '     ' WITH NO ADVANCING.
           MOVE 8 TO REQ-OP-CODE OF STOCK-API.
           CALL 'STOCK' USING REQ-RSP-BLOCK OF STOCK-API
           END-CALL.
           DISPLAY ' '.

           MOVE 99 TO REQ-OP-CODE OF TABLEAU-API
           CALL 'TABLEAU' USING REQ-RSP-BLOCK OF TABLEAU-API
           END-CALL.
           DISPLAY ' '.

      ******************************************************************
       FETCH-FROM-STOCK.
           MOVE 6 TO REQ-OP-CODE OF STOCK-API.
           CALL 'STOCK' USING REQ-RSP-BLOCK OF STOCK-API
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

                      MOVE 3 TO REQ-OP-CODE OF STOCK-API
                      CALL 'STOCK' USING REQ-RSP-BLOCK OF STOCK-API
                      END-CALL

      *    MOVE CARD FROM STOCK TO TABLEAU
                      MOVE RSP-RANK-N OF RSP-CARD-FETCHED
                         TO RANK-N OF CARD-IN-SCOPE 
                      MOVE RSP-SUIT-N OF RSP-CARD-FETCHED
                         TO SUIT-N OF CARD-IN-SCOPE 
                      MOVE 3 TO REQ-OP-CODE OF TABLEAU-API
                      CALL 'TABLEAU' USING TABLEAU-API
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

           MOVE 6 TO REQ-OP-CODE OF TABLEAU-API.
           CALL 'TABLEAU' USING TABLEAU-API
           END-CALL     
           EVALUATE RSP-ERR-CODE OF REQ-RSP-BLOCK OF TABLEAU-API
           WHEN 0 
      *    MOVE THE CARD(S)
                MOVE 6 TO REQ-OP-CODE OF TABLEAU-API
                CALL 'TABLEAU' USING TABLEAU-API
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
       SETTINGS-DIALOG.
           MOVE RSP-TOS-PEEK TO MENU-PARAMETER
           MOVE 2 TO MENU-TO-SHOW
           CALL 'MENUS' USING USER-SELECTION
           END-CALL
           EVALUATE MENU-ENTRY-SELECTED
           WHEN 'T'
                MOVE 4 TO REQ-OP-CODE OF STOCK-API
                CALL 'STOCK' USING REQ-RSP-BLOCK OF STOCK-API
                END-CALL
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
                           MOVE 5 TO REQ-OP-CODE OF FOUNDATION-API
                           CALL 'FOUNDATION' USING REQ-RSP-BLOCK OF
                              FOUNDATION-API
                           END-CALL
                           IF RSP-IS-FULL OF FOUNDATION-API
                              IS EQUAL TO 'N' THEN
      *    CHECK NEXT RANK IN STACK
                              MOVE 4 TO REQ-OP-CODE OF FOUNDATION-API
                              CALL 'FOUNDATION' USING REQ-RSP-BLOCK OF
                                 FOUNDATION-API
                              END-CALL
      *    NOW WE CHECK THE TABLEAU, IF ANY TOS IS THE ONE WE NEED FOR
      *    MOVING ONTO THE FOUNDATION
                              MOVE RSP-NXT-RANK TO RANK-N OF
                                 CARD-IN-SCOPE
                              MOVE REQ-STACK-NUM TO SUIT-N OF
                                 CARD-IN-SCOPE 
                              MOVE 5 TO REQ-OP-CODE OF TABLEAU-API 
                              CALL 'TABLEAU' USING TABLEAU-API
                              END-CALL
                              IF RSP-ERR-CODE OF TABLEAU-API
                                 IS EQUAL TO 0 THEN
      *    WE FOUND A CANDIDATE TO MOVE
      *    POP THIS CARD FROM THE TABLEAU
                                 MOVE RSP-MNDT-STCK-IDX TO REQ-STCK-IDX
                                 MOVE 4 TO REQ-OP-CODE OF TABLEAU-API
                                 CALL 'TABLEAU' USING TABLEAU-API
                                 END-CALL
      *    PUSH THIS CARD ONTO THE FOUNDATION
                                 MOVE 2 TO REQ-OP-CODE OF FOUNDATION-API
                                 MOVE REQ-STACK-NUM TO REQ-SUIT-TO-PUSH
                                 CALL 'FOUNDATION' USING REQ-RSP-BLOCK
                                    OF FOUNDATION-API
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
           MOVE 1 TO REQ-OP-CODE OF CARDS-API.
           CALL 'CARDS' USING REQ-RSP-BLOCK OF CARDS-API
           END-CALL.
           
           MOVE 1 TO REQ-OP-CODE OF STOCK-API.
           CALL 'STOCK' USING REQ-RSP-BLOCK OF STOCK-API
           END-CALL.           

           MOVE 2 TO REQ-OP-CODE OF STOCK-API.
           CALL 'STOCK' USING REQ-RSP-BLOCK OF STOCK-API
           END-CALL.           

           MOVE 1 TO REQ-OP-CODE OF FOUNDATION-API.
           CALL 'FOUNDATION' USING REQ-RSP-BLOCK OF FOUNDATION-API
           END-CALL.

           MOVE 1 TO REQ-OP-CODE OF TABLEAU-API.
           CALL 'TABLEAU' USING TABLEAU-API
           END-CALL.

           MOVE 2 TO REQ-OP-CODE OF TABLEAU-API.
           CALL 'TABLEAU' USING TABLEAU-API
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