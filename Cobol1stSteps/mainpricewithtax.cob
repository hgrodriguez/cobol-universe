       IDENTIFICATION DIVISION. 
       PROGRAM-ID. MAINPRICEWITHTAX.
       DATA DIVISION. 
       WORKING-STORAGE SECTION. 
       01 PRICE      PIC 9(4)V99.
       01 TAXRATE    PIC V999    VALUE .075.
       01 TAXPRICE   PIC 9(4)V99.
       01 FULLPRICE  PIC 9(4)V99.
       
       PROCEDURE DIVISION.
           DISPLAY 'Enter the price:' WITH NO ADVANCING
           ACCEPT PRICE
           DISPLAY 'Price entered= ' PRICE
           DISPLAY 'tax rate = ' TAXRATE
           MULTIPLY PRICE BY TAXRATE GIVING TAXPRICE
           DISPLAY 'tax rate applied to the price = ' TAXPRICE
           COMPUTE FULLPRICE ROUNDED = PRICE + TAXPRICE
           DISPLAY 'Price plus tax = ' FULLPRICE
           STOP RUN.