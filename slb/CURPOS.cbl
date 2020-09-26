       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CURPOS.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  28/04/2006.
       SECURITY.      *************************************************
                      *                                               *
                      *  Simula CURPOS                                *
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

       01   SCREEN-POSITION.
            05 ROW-NUMBER        PIC  9(002) COMP-X VALUE 0.
            05 COLUMN-NUMBER     PIC  9(002) COMP-X VALUE 0.

       LINKAGE SECTION.

       01 L PIC 9(02).
       01 C PIC 9(02).

       PROCEDURE DIVISION USING L C.

       000-INICIO.

           COMPUTE ROW-NUMBER    = L - 1
           COMPUTE COLUMN-NUMBER = C - 1
           CALL "CBL_SET_CSR_POS" USING SCREEN-POSITION.

       000-99-FIM. GOBACK.

       END PROGRAM CURPOS.
