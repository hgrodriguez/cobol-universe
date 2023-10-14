      ******************************************************************
      * SUBPROGRAM FOR THE FOUNDATION OF THE SOLITAIRE GAME
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FOUNDATION.

       DATA DIVISION.

       WORKING-STORAGE SECTION. 

      ******************************************************************
       01 CARDS-API.
      *   DOCUMENTATION SEE: CARDS.COB
          02 REQ-RSP-BLOCK.
             03 REQ-OP-CODE       PIC 9.
             03 REQ-RANK-N        PIC 99.
             03 REQ-SUIT-N        PIC 9.
             03 RSP-ERR-CODE      PIC 99.
             03 RSP-RANK-A        PIC X.
             03 RSP-SUIT-A        PIC X.

      ******************************************************************
      *   THE FOUNDATION HAS FOUR STACKS TO MAINTAIN
      *   THE INDEX INTO THE SPECIFIC STACK IS DEFINED BY THE
      *   SUIT NUMBER, AS WE HAVE FOUR SUITS
       01 F-STACKS-T OCCURS 4 TIMES INDEXED BY F-STACK-I.
      *      HOW MANY CARDS ARE IN THE STACK.
          02 COUNT-OF-CARDS       PIC 99 VALUE 0.
      *      NEXT ACCEPTABLE RANK
      *      ALWAYS COUNT-OF-CARDS + 1
          02 NEXT-RANK            PIC 99 VALUE 1.
      *      SIGNAL, IF THE STACK IS FULL
          02 IS-FULL              PIC X.
      *      ALPHA CODE OF RANK OF TOP CARD:
      *      A,2,3,4,5,6,7,8,9,T,J,Q,K             
          02 RANK-A               PIC X.
      *      ALPHA CODE OF SUIT OF TOP CARD:
      *      D(IAMONDS),C(LUB),H(EARTS),S(PADES)
          02 SUIT-A               PIC X.

       LINKAGE SECTION. 
      ******************************************************************
      *      DEFINES ALL FOUNDATION STACKS OF THE GAME
       01 FOUNDATION-API.
          02 REQ-RSP-BLOCK.
      *         THE OPERATION REQUESTED TO BE PERFORMED
      *         01 -> RESET
      *         02 -> PUSH-1-CARD
      *         03 -> RETURN NUMBER OF CARDS IN STACK
      *         04 -> RETURN NEXT RANK IN STACK
      *         05 -> RETURN THE FULL STATUS OF STACK
      *         06 -> RETURN RANK-A OF STACK
      *         07 -> RETURN SUIT-A OF STACK
      *         99 -> PRINT
             03 REQ-OP-CODE       PIC 99.
      *      THE SUIT OF THE CARD TO PUSH ONTO THE FOUNDATION
      *          INTO THE STACK WITH NUMBER SUIT-TO-PUSH.
             03 REQ-SUIT-TO-PUSH  PIC 9.
      *         THE STACK NUMBER FOR THE REQUEST
             03 REQ-STACK-NUM     PIC 9.
      *         THE ERROR CODE, IF ANY, FOR THE REQUESTED OPERATION
             03 RSP-ERR-CODE      PIC 99.
      *         RESPONSE FOR COUNT OF CARDS IN STACK REQUESTED
             03 RSP-CNT-STACK     PIC 99.
      *         RESPONSE FOR NEXT RANK IN STACK REQUESTED
             03 RSP-NXT-RANK      PIC 99.
      *         RESPONSE FOR IS FULL STATE OF STACK REQUESTED
             03 RSP-IS-FULL       PIC X.
      *         RESPONSE OF ALPHA CODE OF RANK OF TOP CARD OF STACK
      *         REQUESTED
             03 RSP-RANK-A        PIC X.
      *         RESPONSE OF ALPHA CODE OF SUIT OF TOP CARD OF STACK
      *         REQUESTED
             03 RSP-SUIT-A        PIC X.

      ******************************************************************
       PROCEDURE DIVISION USING FOUNDATION-API.
           MOVE 0 TO RSP-ERR-CODE OF FOUNDATION-API 
           EVALUATE REQ-OP-CODE OF FOUNDATION-API 
           WHEN 1
                PERFORM 01-RESET
           WHEN 2
                PERFORM 02-PUSH-1-CARD
           WHEN 3
                PERFORM 03-RETURN-STCK-CNT
           WHEN 4
                PERFORM 04-RETURN-NXT-RANK
           WHEN 5
                PERFORM 05-RETURN-IS-FULL
           WHEN 6
                PERFORM 06-RETURN-RANK-A
           WHEN 7
                PERFORM 07-RETURN-SUIT-A
           WHEN 99
                PERFORM 99-PRINT
           WHEN OTHER
                MOVE 2 TO RSP-ERR-CODE OF FOUNDATION-API 
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

           MOVE 0 TO RSP-ERR-CODE OF FOUNDATION-API.

      ******************************************************************
       02-PUSH-1-CARD.
           IF COUNT-OF-CARDS OF F-STACKS-T(REQ-SUIT-TO-PUSH)
              IS EQUAL TO 13
              MOVE 1 TO RSP-ERR-CODE OF FOUNDATION-API
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
              TO REQ-RANK-N OF CARDS-API
           MOVE REQ-SUIT-TO-PUSH TO REQ-SUIT-N OF CARDS-API 
           MOVE 2 TO REQ-OP-CODE OF CARDS-API
           CALL 'CARDS' USING REQ-RSP-BLOCK IN CARDS-API
           END-CALL

           MOVE RSP-RANK-A OF CARDS-API TO RANK-A OF
              F-STACKS-T(REQ-SUIT-TO-PUSH)
           MOVE RSP-SUIT-A OF CARDS-API TO SUIT-A OF
              F-STACKS-T(REQ-SUIT-TO-PUSH).

      ******************************************************************
       03-RETURN-STCK-CNT.
           MOVE COUNT-OF-CARDS OF F-STACKS-T(REQ-STACK-NUM)
              TO RSP-CNT-STACK.
           
      ******************************************************************
       04-RETURN-NXT-RANK.
           MOVE NEXT-RANK OF F-STACKS-T(REQ-STACK-NUM)
              TO RSP-NXT-RANK.

      ******************************************************************
       05-RETURN-IS-FULL.
           MOVE IS-FULL OF F-STACKS-T(REQ-STACK-NUM)
              TO RSP-IS-FULL.

      ******************************************************************
       06-RETURN-RANK-A.
           MOVE RANK-A OF F-STACKS-T(REQ-STACK-NUM)
              TO RSP-RANK-A OF FOUNDATION-API.

      ******************************************************************
       07-RETURN-SUIT-A.
           MOVE SUIT-A OF F-STACKS-T(REQ-STACK-NUM)
              TO RSP-SUIT-A OF FOUNDATION-API.

      ******************************************************************
       99-PRINT.
           PERFORM VARYING F-STACK-I
              FROM 1 BY 1
              UNTIL F-STACK-I > 4
                   DISPLAY RANK-A OF F-STACKS-T(F-STACK-I)
                      WITH NO ADVANCING 
                   DISPLAY SUIT-A OF F-STACKS-T(F-STACK-I)
                      WITH NO ADVANCING 
                   DISPLAY ' '
                      WITH NO ADVANCING 
           END-PERFORM.