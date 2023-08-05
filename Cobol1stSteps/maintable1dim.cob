       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAINTABLES.

       ENVIRONMENT DIVISION. 
       DATA DIVISION. 
           
       WORKING-STORAGE SECTION. 
       01 TABLE1.
          02 FRIEND  PIC X(15) OCCURS 4 TIMES.

       PROCEDURE DIVISION.
           MOVE 'Joy' TO FRIEND(1).
           MOVE 'Willow' TO FRIEND(2).
           MOVE 'Ivy' TO FRIEND(3).

           DISPLAY FRIEND(1).
           DISPLAY TABLE1.

           STOP RUN.