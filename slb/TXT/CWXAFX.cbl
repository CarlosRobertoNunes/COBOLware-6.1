       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWXAFX.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  31/07/2004.
       SECURITY.      *************************************************
                      *                                               *
                      *  Simula X"AF"                                 *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  CARACTER      PIC  X(001) VALUE SPACES.
       01  TECLA-EDIT    PIC  9(003) VALUE 0.

       LINKAGE SECTION.

       01  XAF-FUNCTION   PIC 9(2) COMP-X.
       01  XAF-PARAMETERS PIC X(4).

       PROCEDURE DIVISION USING XAF-FUNCTION XAF-PARAMETERS.

       000-INICIO.

           IF   XAF-FUNCTION = 26
      *         CALL "CWKBDC" USING XAF-FUNCTION XAF-PARAMETERS (1: 3)
                CALL "CWKBDC" USING "0000"  CARACTER TECLA-EDIT
                                            XAF-PARAMETERS (1: 3)
           ELSE
                CALL X"AF" USING XAF-FUNCTION XAF-PARAMETERS
           END-IF.

       000-99-FIM. GOBACK.
       END PROGRAM CWXAFX.
