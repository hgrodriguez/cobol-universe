      ******************************************************************
      * SUBPROGRAM TABLEAU OF THE GAME OF SOLITAIRE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TABLEAU.

       DATA DIVISION.
       WORKING-STORAGE SECTION. 
      ******************************************************************
      *   IN MOVING CARDS, THIS IS THE RANK WE CAN ACCEPT
       01 ACCEPT-RANK              PIC 99.
      *   THE SUIT OF THE SOURCE CARD TO MOVE
       01 SRC-SUIT-OF-CARD         PIC 9.
      *   THE SUIT OF THE DESTINATION CARD WHICH ACCEPTS THE MOVING CARD
       01 DST-SUIT-OF-CARD         PIC 9.
      *   THE TWO SUITS, WHICH CAN BE ACCEPTED
      *   THEY ARE BOTH EITHER BLACK OR RED
       01 ACCEPT-S-1               PIC 9.
       01 ACCEPT-S-2               PIC 9.
      *   THE TRANSFER BUFFER FOR THE CARDS TO BE MOVED
       01 XFER.
      *      HOW MANY CARDS ARE TO BE MOVED.
          02 MOVE-COUNT            PIC 99.
      *      HOW MANY CARDS ARE IN THE STOCK.
          02 XFER-COUNT            PIC 99.
      *      TABLE OF CARDS IN THE STOCK
          02 XFER-T OCCURS 52 TIMES INDEXED BY XFER-I.
             03 XFER-RANK-N        PIC 99.
             03 XFER-SUIT-N        PIC 9.      
      *   HOW MANY CARDS ARE IN THE TABLEAU.
       01 T-COUNT-OF-CARDS         PIC 99.
      *   WE HAVE 7 STACKS IN THE TABLEAU
       01 T-STACKS-T OCCURS 7 TIMES INDEXED BY T-STACK-I.
      *      HOW MANY CARDS ARE IN THE STACK.
          02 COUNT-OF-CARDS        PIC 99 VALUE 0.
      *      THE CARDS IN ONE STACK
          02 CARDS-T OCCURS 52 TIMES INDEXED BY CARDS-T-I.
             03 RANK-N             PIC 99.
             03 SUIT-N             PIC 9.
      *   THESE ARE FOR PRINTING THE TABLEAU
      *   THE DEEPEST STACK DEPTH TO HANDLE
       01 MAX-STACK-DEPTH          PIC 99.
      *   STACK NUMBER TO PRINT
       01 PRINT-STACK              PIC 99.
      *   NUMBER OF STACK TO PRINT
       01 COLUMN-NUM               PIC 9.
       
      ******************************************************************
       01 CARDS-API.
      *   DOCUMENTATION SEE: CARDS.COB
          03 REQ-RSP-BLOCK.
             04 REQ-OP-CODE        PIC 9.
             04 REQ-RANK-N         PIC 99.
             04 REQ-SUIT-N         PIC 9.
             04 RSP-ERR-CODE       PIC 99.
             04 RSP-RANK-A         PIC X.
             04 RSP-SUIT-A         PIC X.

      ******************************************************************
       01 STOCK-API.
      *   DOCUMENTATION SEE: STOCK.COB
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

       LINKAGE SECTION. 
      ******************************************************************
       01 TABLEAU-API.
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
       PROCEDURE DIVISION USING TABLEAU-API.
      *    PRE-CONDITION: 
      *       CARDS, STOCK ARE FILLED.

           MOVE 0 TO RSP-ERR-CODE OF TABLEAU-API

           EVALUATE REQ-OP-CODE OF TABLEAU-API
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
           WHEN 7
                PERFORM 07-RETURN-NUM-OF-CARDS
           WHEN 8
                PERFORM 08-RETURN-NUM-CARDS-IN-STACK
           WHEN 9
                PERFORM 09-RETURN-CARD-STACK-INDEX
           WHEN 99
                PERFORM 99-PRINT
           END-EVALUATE

           GOBACK.

      ******************************************************************
       01-RESET.
           MOVE 0 TO RSP-ERR-CODE OF TABLEAU-API.
           MOVE 0 TO T-COUNT-OF-CARDS.

           PERFORM VARYING T-STACK-I
              FROM 1 BY 1
              UNTIL T-STACK-I > 7
                   MOVE 0 TO COUNT-OF-CARDS OF T-STACKS-T(T-STACK-I)
           END-PERFORM.

      ******************************************************************
       02-INIT-FROM-STOCK.
      *    GO THROUGH ALL STACKS WE HAVE
           PERFORM VARYING T-STACK-I
              FROM 1 BY 1
              UNTIL T-STACK-I > 7

      *            THE DEPTH OF THE STACK IS DEFINED BY THE OWN
      *            STACK NUMBER
      *            WE NEED A TRIANGLE TO START THE GAME
                   PERFORM VARYING CARDS-T-I
                      FROM 1 BY 1
                      UNTIL CARDS-T-I > T-STACK-I

                           MOVE 3 TO REQ-OP-CODE OF STOCK-API
                           CALL 'STOCK' USING STOCK-API
                           END-CALL

                           MOVE RSP-CARD-FETCHED OF STOCK-API TO
                              CARDS-T(T-STACK-I, CARDS-T-I)

                           ADD 1 TO T-COUNT-OF-CARDS
                           ADD 1 TO COUNT-OF-CARDS OF T-STACKS-T
                              (T-STACK-I)
                   END-PERFORM
           END-PERFORM.

      ******************************************************************
       03-PUSH-TO-STACK.
      *    THIS CAN PUSH ANY CARD ONTO THE STACK DEFINED
      *    THIS IS USED WHEN WE MOVE A CARD FROM THE STOCK
           ADD 1 TO T-COUNT-OF-CARDS
           ADD 1 TO COUNT-OF-CARDS OF T-STACKS-T(REQ-STCK-IDX).

           MOVE RANK-N OF CARD-IN-SCOPE TO
              RANK-N OF CARDS-T(REQ-STCK-IDX, COUNT-OF-CARDS
              OF T-STACKS-T(REQ-STCK-IDX))

           MOVE SUIT-N OF CARD-IN-SCOPE TO
              SUIT-N OF CARDS-T(REQ-STCK-IDX, COUNT-OF-CARDS
              OF T-STACKS-T(REQ-STCK-IDX)).

      ******************************************************************
       04-POP-FROM-STACK.
           IF T-COUNT-OF-CARDS IS EQUAL TO 0
              MOVE 1 TO RSP-ERR-CODE OF TABLEAU-API
              GOBACK
           END-IF

           IF COUNT-OF-CARDS OF T-STACKS-T(REQ-STCK-IDX)
              IS EQUAL TO 0
              MOVE 2 TO RSP-ERR-CODE OF TABLEAU-API
              GOBACK
           END-IF.

           MOVE RANK-N OF CARDS-T(REQ-STCK-IDX, COUNT-OF-CARDS
              OF T-STACKS-T(REQ-STCK-IDX)) TO
              RANK-N OF RSP-CARD OF TABLEAU-API

           MOVE SUIT-N OF CARDS-T(REQ-STCK-IDX, COUNT-OF-CARDS
              OF T-STACKS-T(REQ-STCK-IDX)) TO
              SUIT-N OF RSP-CARD OF TABLEAU-API

           SUBTRACT 1 FROM T-COUNT-OF-CARDS
           SUBTRACT 1 FROM COUNT-OF-CARDS OF
              T-STACKS-T(REQ-STCK-IDX).

      ******************************************************************
       05-MANDATORY-CHECK.
      *    CHECKS ALL STACKS, IF THERE IS A CARD WHICH MUST BE
      *    MOVED TO THE FOUNDATION DUE TO THE GAME RULES
           IF T-COUNT-OF-CARDS IS EQUAL TO 0
              MOVE 1 TO RSP-ERR-CODE OF TABLEAU-API
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
                      MOVE 0 TO RSP-ERR-CODE OF TABLEAU-API
                      MOVE T-STACK-I TO RSP-MNDT-STCK-IDX
                      GOBACK
           END-PERFORM
      *    NO MATCH FOUND
           MOVE 2 TO RSP-ERR-CODE OF TABLEAU-API.

      ******************************************************************
       06-MOVE-CARDS.
      *    MOVES ONE OR MORE CARDS FROM ONE STACK TO ANOTHER
      *    GIVEN THAT THE RULES ARE RESPECTED
           IF COUNT-OF-CARDS OF T-STACKS-T(MV-SRC-ST-I)
              IS EQUAL TO 0
              MOVE 1 TO RSP-ERR-CODE OF TABLEAU-API
              GOBACK
           END-IF.

      *    ILLEGAL INDEX INTO THE SOURCE STACK
           IF MV-SRC-CA-I IS GREATER THAN
              COUNT-OF-CARDS OF T-STACKS-T(MV-SRC-ST-I) 
              MOVE 2 TO RSP-ERR-CODE OF TABLEAU-API
              GOBACK
           END-IF.

           IF COUNT-OF-CARDS OF T-STACKS-T(MV-DST-ST-I) IS EQUAL TO 0
      *       FIRST CHECK FOR KING
              MOVE 13 TO ACCEPT-RANK
              IF RANK-N OF CARDS-T(MV-SRC-ST-I, MV-SRC-CA-I)
                 IS NOT EQUAL TO ACCEPT-RANK
      *          ONLY A KING CAN BE MOVED ONTO AN EMPTY STACK
                 MOVE 5 TO RSP-ERR-CODE OF TABLEAU-API
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
              MOVE 3 TO RSP-ERR-CODE OF TABLEAU-API
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
                 MOVE 4 TO RSP-ERR-CODE OF TABLEAU-API
                 GOBACK
              END-IF
           END-IF.

      *    SO HERE WE MOVE THE CARDS, AS EVERYTHING SEEMS OK
           PERFORM 80-MOVE-CARDS.

      ******************************************************************
       07-RETURN-NUM-OF-CARDS.
           MOVE T-COUNT-OF-CARDS TO RSP-NUM-CARDS.

      ******************************************************************
       08-RETURN-NUM-CARDS-IN-STACK.
           MOVE COUNT-OF-CARDS OF T-STACKS-T(REQ-STCK-IDX)
              TO RSP-NUM-CARDS.

      ******************************************************************
       09-RETURN-CARD-STACK-INDEX.
           MOVE CARDS-T(REQ-STCK-IDX, REQ-CARD-IDX)
              TO RSP-CARD.

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
                   MOVE MV-SRC-ST-I TO REQ-STCK-IDX
                   PERFORM 04-POP-FROM-STACK
      *            MOVE THIS POPPED CARD INTO THE XFER TABLE
                   ADD 1 TO XFER-COUNT
                   MOVE RANK-N OF RSP-CARD
                      TO XFER-RANK-N OF XFER-T(XFER-COUNT)
                   MOVE SUIT-N OF RSP-CARD
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
                   MOVE XFER-RANK-N OF XFER-T(XFER-I)
                      TO RANK-N OF CARD-IN-SCOPE
                   MOVE XFER-SUIT-N OF XFER-T(XFER-I)
                      TO SUIT-N OF CARD-IN-SCOPE

                   MOVE MV-DST-ST-I TO REQ-STCK-IDX
                   PERFORM 03-PUSH-TO-STACK
           END-PERFORM.
           
           MOVE 0 TO RSP-ERR-CODE OF TABLEAU-API.

      ******************************************************************
       99-PRINT.
           IF T-COUNT-OF-CARDS IS EQUAL TO 0 THEN
      *       NOTHING TO DO
              GOBACK
           END-IF
      *    FIND THE DEEPEST STACK -> THIS IS THE MAX WE HAVE TO PRINT     
           MOVE 0 TO MAX-STACK-DEPTH.
           PERFORM VARYING T-STACK-I
              FROM 1 BY 1
              UNTIL T-STACK-I > 7
                   IF COUNT-OF-CARDS OF T-STACKS-T(T-STACK-I)
                      IS GREATER THAN MAX-STACK-DEPTH THEN
                      MOVE COUNT-OF-CARDS OF T-STACKS-T(T-STACK-I) TO
                         MAX-STACK-DEPTH
                   END-IF
           END-PERFORM.
      *    PRINT THE STACK INDICES
           DISPLAY '    ' WITH NO ADVANCING 
           PERFORM VARYING T-STACK-I
              FROM 1 BY 1
              UNTIL T-STACK-I > 7
                   MOVE T-STACK-I TO COLUMN-NUM
                   DISPLAY COLUMN-NUM WITH NO ADVANCING 
                   DISPLAY '  ' WITH NO ADVANCING 
           END-PERFORM
           DISPLAY ' '

           PERFORM VARYING PRINT-STACK
              FROM 1 BY 1
              UNTIL PRINT-STACK > MAX-STACK-DEPTH
                   DISPLAY PRINT-STACK WITH NO ADVANCING 
                   DISPLAY ' ' WITH NO ADVANCING 
                   PERFORM VARYING T-STACK-I
                      FROM 1 BY 1
                      UNTIL T-STACK-I > 7
                           IF PRINT-STACK IS GREATER THAN
                              COUNT-OF-CARDS OF T-STACKS-T(T-STACK-I)
                              THEN
      *               NOTHING TO SHOW, AS THIS STACK IS ALREADY DONE
                              DISPLAY '  ' WITH NO ADVANCING
                           ELSE
      *               THERE IS A CARD TO BE DISPLAYED
                              MOVE 2 TO REQ-OP-CODE OF CARDS-API
                              MOVE RANK-N OF CARDS-T(T-STACK-I,
                                 PRINT-STACK)
                                 TO REQ-RANK-N OF CARDS-API
                              MOVE SUIT-N OF CARDS-T(T-STACK-I,
                                 PRINT-STACK)
                                 TO REQ-SUIT-N OF CARDS-API
                              CALL 'CARDS' USING
                                 REQ-RSP-BLOCK OF CARDS-API
                              END-CALL
                              DISPLAY RSP-RANK-A OF CARDS-API
                                 WITH NO ADVANCING 
                              DISPLAY RSP-SUIT-A OF CARDS-API
                                 WITH NO ADVANCING 
                           END-IF
                           DISPLAY ' ' WITH NO ADVANCING 
                   END-PERFORM
                   DISPLAY ' '
           END-PERFORM.