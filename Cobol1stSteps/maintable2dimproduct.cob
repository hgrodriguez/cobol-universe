       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAINTABLE2DIMPRODUCT.

       ENVIRONMENT DIVISION. 
       DATA DIVISION. 
           
       WORKING-STORAGE SECTION. 

       01 ORDER-TABLE.
          02 PRODUCT OCCURS 2 TIMES INDEXED BY I.
             03 PROD-NAME     PIC X(10).
             03 PROD-SIZE OCCURS 3 TIMES INDEXED BY J.
                04 SIZE-TYPE  PIC A.
                 
       PROCEDURE DIVISION .
           SET I J TO 1.
           
           MOVE 'Blue Shirt' TO PRODUCT(I).
           MOVE 'S' TO PROD-SIZE(I, J).
           SET J UP BY 1.
           MOVE 'M' TO PROD-SIZE(I, J).

           SET J DOWN BY 1.
      *          1234567890   1234567890
      *                    123          123
           MOVE 'Blue ShirtSMLRed Shirt SML' TO ORDER-TABLE.
           
           PERFORM GET-PRODUCT VARYING I FROM 1 BY 1 UNTIL I > 2.
           
           GO TO LOOK-UP.

       GET-PRODUCT.
           DISPLAY PRODUCT(I).

           PERFORM GET-SIZES VARYING J FROM 1 BY 1 UNTIL J > 3.

       GET-SIZES.

           DISPLAY PROD-SIZE(I, J).

       LOOK-UP.
           SET I TO 1.
           SEARCH PRODUCT
           AT END
              DISPLAY 'Product not found'
           WHEN PROD-NAME(I) = 'Red Shirt '
                DISPLAY 'RED SHIRT FOUND'
           END-SEARCH.
                

           STOP RUN.