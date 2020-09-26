       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWSETK.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  25/08/2008.
       SECURITY.      *************************************************
                      *                                               *
                      *  Armazena lista de codigo de teclas habilitas *
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
           05 KEYS                   PIC  X(097) VALUE LOW-VALUES.
           05 I               COMP-X PIC  9(002) VALUE 0.
           05 X91-RESULT      COMP-X PIC  9(002) VALUE 0.
           05 X91-FUNCTION    COMP-X PIC  9(002) VALUE 16.
           05 X91-PARAMETER   COMP-X PIC  9(002) VALUE 0.

       LINKAGE SECTION.

       COPY CWSETK.
       01  OPTION PIC X.

       PROCEDURE DIVISION USING PARAMETROS-CWSETK OPTION.

       000-INICIO.

           CALL X"91" USING X91-RESULT X91-FUNCTION X91-PARAMETER

           IF   X91-PARAMETER > 1
           AND  OPTION = 'G'
                MOVE KEYS TO PARAMETROS-CWSETK
           ELSE
                MOVE LOW-VALUES TO KEYS
                IF CWSETK-KEYS > 96
                   MOVE 96 TO CWSETK-KEYS
                END-IF
                IF CWSETK-KEYS > 0
                   COMPUTE I = CWSETK-KEYS + 1
                   MOVE PARAMETROS-CWSETK (1: I) TO KEYS (1: I)
                END-IF
           END-IF.

       000-99-FIM. GOBACK.

       END PROGRAM CWSETK.
