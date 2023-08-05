       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAINCUSTOMERREPORT.

       ENVIRONMENT DIVISION. 
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 
           SELECT CUSTOMER-REPORT ASSIGN TO 'customer.rpt'
              ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CUSTOMER-FILE ASSIGN TO 'customer.dat'
              ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION. 
       FILE SECTION. 
       FD CUSTOMER-REPORT.
       01 PRINT-LINE            PIC  X(44).
       
       FD CUSTOMER-FILE.
       01 CUSTOMER-DATA.
          02 ID-NUM             PIC 9(5).
          02 CUSTOMER-NAME.
             03 FIRST-NAME      PIC X(15).
             03 LAST-NAME       PIC X(15).
                88 EOF                     VALUE HIGH-VALUE.
           
       WORKING-STORAGE SECTION. 
       01 PAGE-HEADING.
          02 FILLER             PIC X(13)  VALUE 'Customer List'.
       01 PAGE-FOOTER.
          02 FILLER             PIC X(15)  VALUE SPACES.
          02 FILLER             PIC X(7)   VALUE 'Page : '.
          02 PAGE-NUM           PIC Z9.
       01 HEADS                 PIC X(36)  VALUE
             "IDNum    FirstName    LastName".
       01 CUSTOMER-DETAIL-LINE.
          02 FILLER             PIC X      VALUE SPACE.
          02 CDL-ID-NUM         PIC 9(5).
          02 FILLER             PIC X(4)   VALUE SPACES.
          02 CDL-FIRST-NAME     PIC X(15).
          02 FILLER             PIC X(2)   VALUE SPACES.
          02 CDL-LAST-NAME      PIC X(15).
       01 REPORT-FOOTING        PIC X(13)  VALUE "END OF REPORT".
       01 LINE-COUNT            PIC 99     VALUE ZERO.
          88 NEW-PAGE-REQUIRED             VALUE 40 THRU 99.
       01 PAGE-COUNT            PIC 99     VALUE ZERO.


       PROCEDURE DIVISION .
           OPEN INPUT CUSTOMER-FILE.
           OPEN OUTPUT CUSTOMER-REPORT.
           PERFORM PRINT-HEADING
           READ CUSTOMER-FILE
           AT END
              SET EOF TO TRUE
           END-READ
           PERFORM PRINT-REPORT-BODY UNTIL EOF
           WRITE PRINT-LINE FROM REPORT-FOOTING AFTER ADVANCING 5 LINES .
           CLOSE CUSTOMER-REPORT.
           CLOSE CUSTOMER-FILE.

           STOP RUN.

       PRINT-HEADING.
           WRITE PRINT-LINE FROM PAGE-HEADING AFTER ADVANCING PAGE
           WRITE PRINT-LINE FROM HEADS AFTER ADVANCING 5 LINES
           MOVE 3 TO LINE-COUNT
           ADD 1 TO PAGE-COUNT.
      
       PRINT-REPORT-BODY.
           IF NEW-PAGE-REQUIRED 
              MOVE PAGE-COUNT TO PAGE-NUM
              WRITE PRINT-LINE FROM PAGE-FOOTER AFTER ADVANCING 5 LINES
              PERFORM PRINT-HEADING
           END-IF.
           MOVE ID-NUM TO CDL-ID-NUM
           MOVE FIRST-NAME TO CDL-FIRST-NAME
           MOVE LAST-NAME TO CDL-LAST-NAME
           WRITE PRINT-LINE FROM CUSTOMER-DETAIL-LINE AFTER
              ADVANCING 1 LINE
           ADD 1 TO LINE-COUNT
           READ CUSTOMER-FILE
           AT END
              SET EOF TO TRUE.