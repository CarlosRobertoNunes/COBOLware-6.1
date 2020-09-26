       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWREL9.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  21/07/2000.
       SECURITY.      *************************************************
                      *                                               *
                      * Trasfere tabela de ordenacao                  *
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

       01  AREAS-DE-TRABALHO.
           05 SAVE-ORDENACAO      PIC X(211) VALUE SPACES.

       LINKAGE SECTION.

       01   ORDENACAO PIC X(211).

       PROCEDURE DIVISION USING ORDENACAO.

       000-INICIO.

           ON   1
                MOVE ORDENACAO TO SAVE-ORDENACAO.

           MOVE SAVE-ORDENACAO TO ORDENACAO.

       000-99-FIM. GOBACK.
       END PROGRAM CWREL9.
