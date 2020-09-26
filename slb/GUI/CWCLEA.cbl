       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWCLEA.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  15/05/2004.
       SECURITY.      *************************************************
                      *                                               *
                      *  Simula CBL_CLEAR_SCR Clear screen            *
                      *                                               *
                      *************************************************
       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01  PARAMETROS-CWUSER.
           COPY CWUSER.

       LINKAGE SECTION.

       01  CARACTERE PIC X(001).
       01  ATRIBUTO  PIC X(001).

       PROCEDURE DIVISION USING CARACTERE
                                ATRIBUTO.
       000-INICIO.

           MOVE 01         TO cwuser-LINE
                              cwuser-COLUMN
           MOVE LOW-VALUES TO cwuser-CHARACTERS
           MOVE LOW-VALUES TO cwuser-ATTRIBUTES
           MOVE 2000       TO cwuser-LENGTH-CHAR
                              cwuser-LENGTH-ATTR
           INSPECT cwuser-CHARACTERS CONVERTING LOW-VALUE TO CARACTERE
           INSPECT cwuser-ATTRIBUTES CONVERTING LOW-VALUE TO ATRIBUTO
           CALL "CWUSER" USING X"00" PARAMETROS-CWUSER.

       000-99-FIM. GOBACK.
       END PROGRAM CWCLEA.
