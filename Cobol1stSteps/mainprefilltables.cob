       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAINPREFILLTABLES.

       ENVIRONMENT DIVISION. 
       DATA DIVISION. 
           
       WORKING-STORAGE SECTION. 

       01 PROD-TABLE.
          02 PROD-DATA.
             03 FILLER         PIC X(8)      VALUE 'Red  SML'.
             03 FILLER         PIC X(8)      VALUE 'Blue SML'.
             03 FILLER         PIC X(8)      VALUE 'GreenSML'.
          02 FILLER REDEFINES PROD-DATA.
             03 SHIRT OCCURS 3 TIMES.
                04 PROD-NAME   PIC X(5).
                04 PROD-SIZES  PIC A OCCURS 3 TIMES.
                 
       01 CHANGE-ME.
          02 TEXT-NUM          PIC X(6).
          02 FLOAT-NUM REDEFINES TEXT-NUM
                               PIC 9(4)V99.

       01 STR-NUM              PIC X(7).
       01 SPLIT-NUM.
          02 W-NUM             PIC 9(4)      VALUE ZERO.
          02 F-NUM             PIC 99        VALUE ZERO.
       01 FL-NUM REDEFINES SPLIT-NUM
                               PIC 9999V99.
       01 DOLLAR-NUM           PIC $$,$$9.99.
       
       PROCEDURE DIVISION .
           DISPLAY SHIRT(1).
           MOVE '123456' TO TEXT-NUM.
           DISPLAY FLOAT-NUM.

           DISPLAY "enter a float : " WITH NO ADVANCING 
           ACCEPT STR-NUM
           UNSTRING STR-NUM
              DELIMITED BY '.' OR ALL SPACES
              INTO W-NUM, F-NUM
           MOVE FL-NUM TO DOLLAR-NUM
           DISPLAY DOLLAR-NUM.