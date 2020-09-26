       IDENTIFICATION DIVISION.
       PROGRAM-ID.    XSDCHEXE.
       AUTHOR.        Carlos Roberto de Souza Nunes.
       DATE-WRITTEN.  09/09/2017.
       SECURITY.      *************************************************
                      *                                               *
                      * Suporte a programas gerados pelo XSEED        *
                      * Verifica existencia de programa               *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  X91.
           05 X91-RESULT           PIC  9(002) COMP-X VALUE 0.
           05 X91-F15              PIC  9(002) COMP-X VALUE 15.
           05 X91-BINARIO-COBOL.
              10 LEN-BINARIO-COBOL PIC  9(002) VALUE 0 COMP-X.
              10 BINARIO-COBOL     PIC  X(065) VALUE SPACES.

       LINKAGE SECTION.

       01  GLB-TITLE               PIC X(60).
       01  GLB-STATUSCODE          PIC X(02).
       01  GLB-LSN                 PIC X(05).

       PROCEDURE DIVISION USING GLB-TITLE
                                GLB-STATUSCODE
                                GLB-LSN.
           MOVE GLB-TITLE            TO BINARIO-COBOL
           MOVE LENGTH BINARIO-COBOL TO LEN-BINARIO-COBOL
           CALL X"91" USING X91-RESULT X91-F15
                            X91-BINARIO-COBOL
           IF   X91-RESULT NOT = ZERO
                MOVE '30' TO GLB-STATUSCODE
           ELSE
                MOVE '00' TO GLB-STATUSCODE
           END-IF
           GOBACK.
       END PROGRAM XSDCHEXE.
