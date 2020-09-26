       IDENTIFICATION DIVISION.
       PROGRAM-ID.    GRHELP.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  23/02/2011.
       SECURITY.      *************************************************
                      *                                               *
                      *  Compatibiliza chamada GRHELP da vers∆o 3     *
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
       01  AREAS-DE-TRABALHO-1.
           05 X91-RESULT        COMP-X PIC  9(002) VALUE 0.
           05 X91-FUNCTION      COMP-X PIC  9(002) VALUE 16.
           05 X91-PARAMETER     COMP-X PIC  9(002) VALUE 0.

       LINKAGE SECTION.

       01  USUARIO-L            PIC   X(030).
       01  TASK-L               PIC   9(006).
       01  PROGRAMA-L           PIC   X(008).
       01  GRMENU               PIC   X(001).

       PROCEDURE DIVISION USING USUARIO-L TASK-L PROGRAMA-L GRMENU.

           CALL X"91" USING X91-RESULT X91-FUNCTION X91-PARAMETER
           IF   X91-PARAMETER = 0
                STOP RUN
           ELSE
                CALL "CWGETU" USING USUARIO-L TASK-L PROGRAMA-L GRMENU
           END-IF.
