       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWMOUS.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  31/08/1991.
       SECURITY.      *************************************************
                      *                                               *
                      *  Simula escape key pelo mouse SP2 retorna     *
                      *  sempre 255                                   *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       LINKAGE SECTION.

       COPY CWMOUS.

       PROCEDURE DIVISION USING PARAMETROS-CWMOUS.

       000-INICIO.

           MOVE 255 TO CWMOUS-KEY
           goback.

       END PROGRAM CWMOUS.
