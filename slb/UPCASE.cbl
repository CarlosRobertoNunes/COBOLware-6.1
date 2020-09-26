       IDENTIFICATION DIVISION.
       PROGRAM-ID.    UPCASE.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  28/04/2006.
       SECURITY.      *************************************************
                      *                                               *
                      *  Simula _MSUPCASE                             *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.  COPY CWCASE.
           05 LEX PIC 9(16) VALUE 0.
           05 I   PIC 9(02) VALUE 0.
           05 Y   PIC 9(02) VALUE 0.

       LINKAGE SECTION.

       01  TEXTO        PIC X.
       01  LEN-TEXTO    PIC 9(008) COMP-X.
       01  LEN          PIC X.
       01  LEN-LEN      PIC 9(008) COMP-X.

       PROCEDURE DIVISION USING TEXTO
                                LEN-TEXTO
                                LEN
                                LEN-LEN.

       000-INICIO.

           MOVE 0  TO LEX
           MOVE 16 TO Y
           PERFORM VARYING I FROM LEN-LEN BY -1 UNTIL I = 0
                                                   OR Y = 0
                   MOVE LEN(I:1) TO LEX(Y:1)
                   SUBTRACT 1 FROM Y
           END-PERFORM

           INSPECT TEXTO (1: LEX)
                   CONVERTING MINUSCULAS TO MAIUSCULAS.

       000-99-FIM. GOBACK.
       END PROGRAM UPCASE.
