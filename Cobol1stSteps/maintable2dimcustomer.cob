       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAINTABLES.

       ENVIRONMENT DIVISION. 
       DATA DIVISION. 
           
       WORKING-STORAGE SECTION. 
       01 CUSTOMER-TABLE.
          02 CUSTOMER-NAME OCCURS 5 TIMES.
             03 F-NAME      PIC X(15).
             03 L-NAME      PIC X(15).

       PROCEDURE DIVISION .
           MOVE 'Paul' TO F-NAME(1).
           MOVE 'Smith' TO L-NAME(1).

           MOVE 'Sally' TO F-NAME(2).
           MOVE 'Smith' TO L-NAME(2).

           DISPLAY CUSTOMER-NAME(1).
           DISPLAY CUSTOMER-TABLE.

           STOP RUN.