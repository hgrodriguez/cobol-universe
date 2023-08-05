       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAINCUSTOMERINTERACTIVE.

       ENVIRONMENT DIVISION. 
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 
           SELECT CUSTOMER-FILE ASSIGN TO 'customers.txt'
              ORGANIZATION IS INDEXED ACCESS MODE IS 
              RANDOM RECORD KEY IS ID-NUM.
       DATA DIVISION. 
       FILE SECTION. 
       
       FD CUSTOMER-FILE.
       01 CUSTOMER-DATA.
          02 ID-NUM        PIC 9(2).
          02 FIRST-NAME    PIC X(15).
          02 LAST-NAME     PIC X(15).
           
       WORKING-STORAGE SECTION. 
       01 CHOICE           PIC X.
       01 STAY-OPEN        PIC X     VALUE 'Y'.
       01 CUSTOMER-EXISTS  PIC X.

       PROCEDURE DIVISION .

       MAIN-LOOP.
           OPEN I-O CUSTOMER-FILE.
           PERFORM UNTIL STAY-OPEN IS EQUAL TO 'N'
                   DISPLAY " "
                   DISPLAY "Customer Interactive Program"
                   DISPLAY "Please select the operation"
                   DISPLAY "1) Add Customer"
                   DISPLAY "2) Delete Customer"
                   DISPLAY "3) Update Customer"
                   DISPLAY "4) Get Customer"
                   DISPLAY "0) Quit"
                   DISPLAY ": " WITH NO ADVANCING 
                   ACCEPT CHOICE
                   EVALUATE CHOICE
                   WHEN '1'
                        PERFORM ADD-CUSTOMER
                   WHEN '2'
                        PERFORM DELETE-CUSTOMER
                   WHEN '3' 
                        PERFORM UPDATE-CUSTOMER
                   WHEN '4'
                        PERFORM GET-CUSTOMER
                   WHEN '0'
                        MOVE 'N' TO STAY-OPEN
                   WHEN OTHER 
                        DISPLAY "wrong choice, please try again"
                   END-EVALUATE
           END-PERFORM.

           CLOSE CUSTOMER-FILE.

           STOP RUN.
       
       ADD-CUSTOMER.
           DISPLAY " "
           DISPLAY "ADD: Enter ID: " WITH NO ADVANCING.
           ACCEPT ID-NUM.
           DISPLAY "ADD: Enter First Name: " WITH NO ADVANCING.
           ACCEPT FIRST-NAME.
           DISPLAY "ADD: Enter Last Name: " WITH NO ADVANCING.
           ACCEPT LAST-NAME.
           DISPLAY " "
           WRITE CUSTOMER-DATA
           INVALID KEY
                   DISPLAY "ID Taken"
           END-WRITE.           

       DELETE-CUSTOMER.
           DISPLAY " "
           DISPLAY "DEL: Enter ID: " WITH NO ADVANCING.
           ACCEPT ID-NUM.
           DISPLAY " "
           DELETE CUSTOMER-DATA
           INVALID KEY
                   DISPLAY "ID does not exist"
           END-DELETE.           

       UPDATE-CUSTOMER.
           MOVE 'Y' TO CUSTOMER-EXISTS.
           DISPLAY " "
           DISPLAY "UPD: Enter ID: " WITH NO ADVANCING
           ACCEPT ID-NUM
           DISPLAY " "
           READ CUSTOMER-DATA
           INVALID KEY
                   MOVE 'N' TO CUSTOMER-EXISTS
           END-READ
           IF CUSTOMER-EXISTS IS EQUAL TO 'N'
              DISPLAY "Customer with this ID does not exist"
           ELSE
              DISPLAY "ADD: Enter New First Name: "
                 WITH NO ADVANCING
              ACCEPT FIRST-NAME
              DISPLAY "ADD: Enter New Last Name: "
                 WITH NO ADVANCING
              ACCEPT LAST-NAME
           END-IF.
           REWRITE CUSTOMER-DATA
           INVALID KEY
                   DISPLAY "Customer was not updated."
           END-REWRITE.


       GET-CUSTOMER.
           MOVE 'Y' TO CUSTOMER-EXISTS.
           DISPLAY " "
           DISPLAY "GET: Enter ID: " WITH NO ADVANCING
           ACCEPT ID-NUM
           DISPLAY " "
           READ CUSTOMER-FILE
           INVALID KEY
                   MOVE 'N' TO CUSTOMER-EXISTS
           END-READ
           IF CUSTOMER-EXISTS IS EQUAL TO 'N'
              DISPLAY "Customer with this ID does not exist"
           ELSE
              DISPLAY "GET: ID: " WITH NO ADVANCING
              DISPLAY ID-NUM
              DISPLAY "GET: First Name: "
                 WITH NO ADVANCING
              DISPLAY FIRST-NAME
              DISPLAY "GET: Last Name: "
                 WITH NO ADVANCING
              DISPLAY LAST-NAME
           END-IF.