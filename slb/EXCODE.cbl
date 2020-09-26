       IDENTIFICATION DIVISION.
       PROGRAM-ID.    EXCODE.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  28/04/2006.
       SECURITY.      *************************************************
                      *                                               *
                      *  Simula _MSEXCODE                             *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 TECLA               PIC 9(003) VALUE 0. COPY CWKEYS.

       LINKAGE SECTION.

       01  CODEX        PIC 9(18).
       01  LEN-CODEX    PIC 9(008) COMP-X.

       PROCEDURE DIVISION USING CODEX
                                LEN-CODEX.

       000-INICIO.

           MOVE CODEX (1: LEN-CODEX) TO RETURN-CODE.

       000-99-FIM. GOBACK.

       END PROGRAM EXCODE.
