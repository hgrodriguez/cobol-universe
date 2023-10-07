       IDENTIFICATION DIVISION.
       PROGRAM-ID. MENUS.

       DATA DIVISION.

       WORKING-STORAGE SECTION. 
       01 CHOICE                  PIC X.
       01 ENTRY-IS-VALID          PIC X.

       LINKAGE SECTION. 
      ******************************************************************
      *   DEFINES THE USER MENU SELECTION
       01 USER-SELECTION.
          02 MENU-TO-SHOW         PIC 99.
          02 MENU-ENTRY-SELECTED  PIC X.


      ******************************************************************
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
       
      ******************************************************************
       01-MAIN-MENU.
           DISPLAY "01-MAIN-MENU".
           MOVE ' ' TO CHOICE.
           MOVE 'N' TO ENTRY-IS-VALID.
           PERFORM UNTIL ENTRY-IS-VALID IS EQUAL TO 'Y'
                   DISPLAY " "
                   DISPLAY "F)ETCH CARDS FROM STOCK"
                   DISPLAY "H)ELP"
                   DISPLAY "M)OVE"
                   DISPLAY "S)TART GAME"
                   DISPLAY "Q) Quit"
                   DISPLAY ": " WITH NO ADVANCING 
                   ACCEPT CHOICE
                   EVALUATE CHOICE
                   WHEN 'F'
                        MOVE CHOICE TO MENU-ENTRY-SELECTED
                        MOVE 'Y' TO ENTRY-IS-VALID
                   WHEN 'H'
                        MOVE CHOICE TO MENU-ENTRY-SELECTED
                        MOVE 'Y' TO ENTRY-IS-VALID
                   WHEN 'M'
                        MOVE CHOICE TO MENU-ENTRY-SELECTED
                        MOVE 'Y' TO ENTRY-IS-VALID
                   WHEN 'S'
                        MOVE CHOICE TO MENU-ENTRY-SELECTED
                        MOVE 'Y' TO ENTRY-IS-VALID
                   WHEN 'Q'
                        MOVE CHOICE TO MENU-ENTRY-SELECTED
                        MOVE 'Y' TO ENTRY-IS-VALID
                   WHEN OTHER 
                        DISPLAY "wrong choice, please try again"
                   END-EVALUATE
           END-PERFORM.

      ******************************************************************
       02-SETTINGS-MENU.
           DISPLAY "02-SETTINGS-MENU".