       IDENTIFICATION DIVISION. 
       PROGRAM-ID. MAINGETSUMREF.
       DATA DIVISION. 
       WORKING-STORAGE SECTION. 
       01 NUM-1  PIC 9  VALUE 5.
       01 NUM-2  PIC 9  VALUE 4.
       01 SUM-0  PIC 99.

       PROCEDURE DIVISION.
           CALL 'GETSUMREF' USING
              BY REFERENCE NUM-1,
              BY REFERENCE NUM-2,
              BY REFERENCE SUM-0
           END-CALL.
           DISPLAY NUM-1 " + " NUM-2 " = " SUM-0.

           STOP RUN.