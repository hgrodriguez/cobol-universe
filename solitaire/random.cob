       IDENTIFICATION DIVISION.
       PROGRAM-ID. RANDOM-PRG.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       01 MIN-NUMBER                    PIC 99    VALUE 1.
       01 MAX-NUMBER                    PIC 99    VALUE 52.
       01 RANDOM-NUMBER                 PIC 99.
       01 SEED                          PIC 9999 COMP-3.

       01 STOP-THE-GENERATION           PIC X     VALUE 'N'.

       01 CHECK-ARRAY.
          03 ELEMENTS OCCURS 52 TIMES INDEXED BY I.
             06 SEEN                    PIC X     VALUE 'N'.
       01 COUNTER                       PIC 99.


       01 WS-CURRENT-DATE-DATA.
          05 WS-CURRENT-DATE.
             10 WS-CURRENT-YEAR         PIC 9(04).
             10 WS-CURRENT-MONTH        PIC 9(02).
             10 WS-CURRENT-DAY          PIC 9(02).
          05 WS-CURRENT-TIME.
             10 WS-CURRENT-HOURS        PIC 9(02).
             10 WS-CURRENT-MINUTE       PIC 9(02).
             10 WS-CURRENT-SECOND       PIC 9(02).
             10 WS-CURRENT-MILLISECONDS PIC 9(02).

       PROCEDURE DIVISION.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-DATA
           DISPLAY WS-CURRENT-DATE-DATA

           MOVE WS-CURRENT-SECOND TO SEED
           MULTIPLY 100 BY SEED
           ADD WS-CURRENT-MILLISECONDS TO SEED
           DISPLAY SEED
           COMPUTE RANDOM-NUMBER = FUNCTION RANDOM(SEED)

           PERFORM UNTIL STOP-THE-GENERATION IS EQUAL TO 'Y'

                   COMPUTE RANDOM-NUMBER = FUNCTION RANDOM *
                      (MAX-NUMBER - MIN-NUMBER + 1) +
                      MIN-NUMBER
                   IF SEEN OF ELEMENTS(RANDOM-NUMBER) IS EQUAL TO 'N'
      *               NEW CANDIDATE TO CHECK
                      MOVE 'Y' TO SEEN OF ELEMENTS(RANDOM-NUMBER)
                      DISPLAY CHECK-ARRAY
      *               NOW WE CHECK THE WHOLE ARRAY
      *               ASSUME, WE CAN STOP
                      MOVE 'Y' TO STOP-THE-GENERATION
                      PERFORM VARYING COUNTER
                         FROM 1 BY 1
                         UNTIL COUNTER > 52
                              IF SEEN OF ELEMENTS(COUNTER) IS EQUAL TO
                                 'N'
      *                          NOT FINISHED YET, FORCE CONTINUE
                                 MOVE 'N' TO STOP-THE-GENERATION
                              END-IF
                      END-PERFORM
                      
                   END-IF
           END-PERFORM.

           STOP RUN.