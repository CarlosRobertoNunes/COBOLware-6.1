       IDENTIFICATION DIVISION.
       PROGRAM-ID.    GRIEOK.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  23/02/2011.
       SECURITY.      *************************************************
                      *                                               *
                      *  Compatibilza chmada GRIEOK da vers∆o 3       *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.


       LINKAGE SECTION.

       COPY CWIEOK.

       SCREEN SECTION.

       PROCEDURE DIVISION USING PARAMETROS-CWIEOK.

       000-INICIO.

           CALL "CWIEOK" USING PARAMETROS-CWIEOK.

       000-99-FIM. GOBACK.

       END PROGRAM GRIEOK.
