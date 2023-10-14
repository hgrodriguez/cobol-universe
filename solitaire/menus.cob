      ******************************************************************
      *  SUBPROGRAM MENU FOR THE SOLITAIRE GAME
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MENUS.

       DATA DIVISION.
       WORKING-STORAGE SECTION. 

      ******************************************************************
      *   CHOICE OF THE USER ENTERED
       01 CHOICE                  PIC X.
      *   WAS IT A VALID ENTRY? Y/N
       01 ENTRY-IS-VALID          PIC X.

       LINKAGE SECTION. 
      ******************************************************************
      *   DEFINES THE USER MENU SELECTION
       01 USER-SELECTION.
      *      WHICH MENU TO SHOW?
          02 MENU-TO-SHOW         PIC 99.
      *      WHAT WAS THE MENU ENTRY SELECTED BY THE  USER?
          02 MENU-ENTRY-SELECTED  PIC X.
      *      GENERIC PARAMETER FOR THE MENU TO SHOW
      *      CURRENTLY ONLY USED FOR THE SETTINGS MENU
          02 MENU-PARAMETER       PIC X.

      ******************************************************************
       PROCEDURE DIVISION USING USER-SELECTION.
           EVALUATE MENU-TO-SHOW
           WHEN 1
                PERFORM 01-MAIN-MENU
           WHEN 2
                PERFORM 02-SETTINGS-MENU
           WHEN OTHER 
                DISPLAY "WRONG CHOICE, PLEASE TRY AGAIN"
           END-EVALUATE
           GOBACK.
       
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
                   DISPLAY "R)ESTART GAME"
                   DISPLAY "S)ETTINGS"
                   DISPLAY "Q)uit"
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
                   WHEN 'R'
                        MOVE CHOICE TO MENU-ENTRY-SELECTED
                        MOVE 'Y' TO ENTRY-IS-VALID
                   WHEN 'S'
                        MOVE CHOICE TO MENU-ENTRY-SELECTED
                        MOVE 'Y' TO ENTRY-IS-VALID
                   WHEN 'Q'
                        MOVE CHOICE TO MENU-ENTRY-SELECTED
                        MOVE 'Y' TO ENTRY-IS-VALID
                   WHEN OTHER 
                        DISPLAY "WRONG CHOICE, PLEASE TRY AGAIN"
                   END-EVALUATE
           END-PERFORM.

      ******************************************************************
       02-SETTINGS-MENU.
           DISPLAY "02-SETTINGS-MENU".
           MOVE ' ' TO CHOICE.
           MOVE 'N' TO ENTRY-IS-VALID.
           PERFORM UNTIL ENTRY-IS-VALID IS EQUAL TO 'Y'
                   DISPLAY " "
                   DISPLAY "T)OGGLE STOCK PEEK (IS=" MENU-PARAMETER
                      WITH NO ADVANCING
                   DISPLAY ")"
                   DISPLAY ": " WITH NO ADVANCING 
                   ACCEPT CHOICE
                   EVALUATE CHOICE
                   WHEN 'T'
                        MOVE CHOICE TO MENU-ENTRY-SELECTED
                        MOVE 'Y' TO ENTRY-IS-VALID
                   WHEN OTHER 
                        DISPLAY "WRONG CHOICE, PLEASE TRY AGAIN"
                   END-EVALUATE
           END-PERFORM.