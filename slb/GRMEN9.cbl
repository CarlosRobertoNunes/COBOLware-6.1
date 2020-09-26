       IDENTIFICATION DIVISION.
       PROGRAM-ID.    GRMEN9.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  23/02/2011.
       SECURITY.      *************************************************
                      *                                               *
                      *  Compatibilza chmada GRMEN9 da vers∆o 3       *
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

       SCREEN SECTION.

       PROCEDURE DIVISION.

       000-INICIO.

           CALL "CWMEN9".

       000-99-FIM. GOBACK.

       END PROGRAM GRMEN9.
