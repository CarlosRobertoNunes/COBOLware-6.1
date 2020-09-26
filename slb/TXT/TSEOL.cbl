       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TSEOL.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  14/06/2017.
       SECURITY.      *************************************************
                      *                                               *
                      *  Descreva a finalidade do seu novo programa   *
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
           05 TECLA               PIC 9(003) VALUE 0. COPY CWKEYS.

       LINKAGE SECTION.

       SCREEN SECTION.

       PROCEDURE DIVISION.

       000-INICIO.

           EXEC COBOLware EOL DOS
                FILE 'buceta.txt'
           END-EXEC.

       000-99-FIM. GOBACK.

       END PROGRAM TSEOL.
