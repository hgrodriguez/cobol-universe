       IDENTIFICATION DIVISION. 
       PROGRAM-ID. MAINPICTURES.
       DATA DIVISION. 
       WORKING-STORAGE SECTION. 
       01 STARTNUM  PIC 9(8)V99       VALUE 00001123.55.
       01 NOZERO    PIC ZZZZZZZ9.99.
       01 NOZPLUSC  PIC ZZ,ZZ,ZZ9.99.
       01 DOLLAR    PIC $$,$$$,$$9.99.
       01 BDAY-US   PIC 9(8)          VALUE 07152023.
       01 ADATE-US  PIC 99/99/9999.
       01 BDAY-ISO  PIC 9(8)          VALUE 20230715.
       01 INT-BDAY  PIC 9(8).
       01 ISO-BDAY  PIC X(10).

       PROCEDURE DIVISION.
           MOVE STARTNUM TO NOZERO
           DISPLAY NOZERO
           MOVE STARTNUM TO NOZPLUSC
           DISPLAY NOZPLUSC
           MOVE STARTNUM TO DOLLAR
           DISPLAY DOLLAR
           MOVE BDAY-US TO ADATE-US
           DISPLAY ADATE-US
           MOVE FUNCTION INTEGER-OF-DATE(BDAY-ISO) TO INT-BDAY
           DISPLAY INT-BDAY
           MOVE FUNCTION FORMATTED-DATE('YYYY-MM-DD',
      -     INT-BDAY) TO ISO-BDAY
           DISPLAY ISO-BDAY
           STOP RUN.