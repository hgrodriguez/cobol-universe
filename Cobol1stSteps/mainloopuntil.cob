       IDENTIFICATION DIVISION. 
       PROGRAM-ID. MAINLOOPUNTIL.
       DATA DIVISION. 
       WORKING-STORAGE SECTION. 
       01 IND  PIC 9 VALUE 0.

       PROCEDURE DIVISION.
           PERFORM OUTPUT-DATA WITH TEST AFTER UNTIL IND > 5
           GO TO FOR-LOOP.

       OUTPUT-DATA.
           DISPLAY 'OUTPUT-DATA, INDEX = ' IND.
           ADD 1 TO IND.

       FOR-LOOP.
           PERFORM OUTPUT-DATA2 VARYING IND FROM 1 BY 1 UNTIL IND = 5

           STOP RUN.

       OUTPUT-DATA2.
           DISPLAY 'OUTPUT-DATA2, INDEX = ' IND.