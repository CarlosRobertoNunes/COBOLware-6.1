       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWSCSZ.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  15/05/2004.
       SECURITY.      *************************************************
                      *                                               *
                      * CBL_GET_SCR_SIZE                              *
                      * Get screen size                               *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       LINKAGE SECTION.
       01  LINHAS   PIC 9(02) COMP-X.
       01  COLUNAS  PIC 9(02) COMP-X.

       PROCEDURE DIVISION USING LINHAS COLUNAS.

       000-INICIO.

           MOVE 25 TO LINHAS
           MOVE 80 TO COLUNAS
           MOVE 0  TO RETURN-CODE.

       000-99-FIM. GOBACK.
       END PROGRAM CWSCSZ.
