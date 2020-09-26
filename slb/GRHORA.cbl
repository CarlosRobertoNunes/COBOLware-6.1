       IDENTIFICATION DIVISION.
       PROGRAM-ID.    GRHORA.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  23/02/2011.
       SECURITY.      *************************************************
                      *                                               *
                      *  Compatibilza chmada GRHORA da vers∆o 3       *
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

           CALL "CWATCH".

       000-99-FIM. GOBACK.

       END PROGRAM GRHORA.
