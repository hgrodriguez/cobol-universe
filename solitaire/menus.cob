       IDENTIFICATION DIVISION.
       PROGRAM-ID. MENUS.

       DATA DIVISION.

       WORKING-STORAGE SECTION. 
       01 CHOICE                  PIC X.

       LINKAGE SECTION. 
      ******************************************************************
      *   DEFINES THE USER MENU SELECTION
       01 USER-SELECTION.
          02 MENU-TO-SHOW         PIC 99.
          02 MENU-ENTRY-SELECTED  PIC X.


       PROCEDURE DIVISION USING USER-SELECTION.
           EVALUATE MENU-TO-SHOW
           WHEN 1
                PERFORM 01-MAIN-MENU
           WHEN 2
                PERFORM 02-SETTINGS-MENU
           WHEN OTHER 
                DISPLAY "wrong choice, please try again"
           END-EVALUATE

           EXIT PROGRAM.       
       
       01-MAIN-MENU.
           DISPLAY "01-MAIN-MENU".
           MOVE ' ' TO CHOICE.
           PERFORM UNTIL CHOICE IS EQUAL TO 'Q'
                   DISPLAY " "
                   DISPLAY "Customer Interactive Program"
                   DISPLAY "Please select the operation"
                   DISPLAY "1) Add Customer"
                   DISPLAY "2) Delete Customer"
                   DISPLAY "3) Update Customer"
                   DISPLAY "4) Get Customer"
                   DISPLAY "Q) Quit"
                   DISPLAY ": " WITH NO ADVANCING 
                   ACCEPT CHOICE
                   EVALUATE CHOICE
                   WHEN '1'
                        DISPLAY "1) Add Customer"
                   WHEN '2'
                        DISPLAY "2) Delete Customer"
                   WHEN '3' 
                        DISPLAY "3) Update Customer"
                   WHEN '4'
                        DISPLAY "4) Get Customer"
                   WHEN OTHER 
                        DISPLAY "wrong choice, please try again"
                   END-EVALUATE
           END-PERFORM.

       02-SETTINGS-MENU.
           DISPLAY "02-SETTINGS-MENU".