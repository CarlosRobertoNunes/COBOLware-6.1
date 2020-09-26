       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWUSERPGM.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  02/10/2019.
       SECURITY.      *************************************************
                      *                                               *
                      * Armazena/recupera nome do programa chamador   *
                      * Da CWUSER                                     *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.

       01  SAVE-PGM PIC X(030) VALUE 'CWMENU'.

       LINKAGE SECTION.

       01  FUNCAO PIC X.
       01  PGM    PIC X(30).

       PROCEDURE DIVISION USING FUNCAO PGM.

           IF  FUNCAO = 'G' OR 'g'
               MOVE SAVE-PGM TO PGM
           ELSE
               MOVE PGM TO SAVE-PGM
           END-IF

           GOBACK.

       END PROGRAM CWUSERPGM.
