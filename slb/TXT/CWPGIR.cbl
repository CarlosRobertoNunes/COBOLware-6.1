       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWPGIR.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  17/11/2009.
       SECURITY.      *************************************************
                      *                                               *
                      *  Atualiza nome do programa no rodap‚          *
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

       01   PROGRAMA PIC X(7).

       PROCEDURE DIVISION USING PROGRAMA.

       000-INICIO.

           DISPLAY PROGRAMA AT LINE 23 COLUMN 73.

       000-99-FIM. GOBACK.

       END PROGRAM CWPGIR.
