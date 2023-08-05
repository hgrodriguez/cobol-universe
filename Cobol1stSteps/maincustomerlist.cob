       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAINCUSTOMERLIST.

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
       01 WS-EOF               PIC A(1).

       PROCEDURE DIVISION .
           OPEN INPUT CUSTOMER-FILE.
           PERFORM UNTIL WS-EOF = 'Y'
                   READ CUSTOMER-FILE INTO WS-CUSTOMER-DATA
                   AT END
                      MOVE 'Y' TO WS-EOF
                   NOT AT END
                       DISPLAY WS-CUSTOMER-DATA
                   END-READ
           END-PERFORM
           CLOSE CUSTOMER-FILE.

           STOP RUN.