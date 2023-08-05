       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAINCUSTOMERCREATE.

       ENVIRONMENT DIVISION. 
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 
           SELECT CUSTOMER-FILE ASSIGN TO 'customer.dat'
              ORGANIZATION IS LINE SEQUENTIAL
              ACCESS IS SEQUENTIAL.
       DATA DIVISION. 
       FILE SECTION. 
       FD CUSTOMER-FILE.
       01 CUSTOMER-DATA.
          02 ID-NUM            PIC 9(5).
          02 CUSTOMER-NAME.
             03 FIRST-NAME     PIC X(15).
             03 LAST-NAME      PIC X(15).
       WORKING-STORAGE SECTION. 
       01 WS-CUSTOMER-DATA.
          02 WS-ID-NUM         PIC 9(5).
          02 WS-CUSTOMER-NAME.
             03 WS-FIRST-NAME  PIC X(15).
             03 WS-LAST-NAME   PIC X(15).

       PROCEDURE DIVISION .
           OPEN OUTPUT CUSTOMER-FILE.
           MOVE 00001 TO ID-NUM.
           MOVE 'Doug' TO FIRST-NAME.
           MOVE 'Thomas' TO LAST-NAME.
           WRITE CUSTOMER-DATA
           END-WRITE.
           CLOSE CUSTOMER-FILE.
           STOP RUN.