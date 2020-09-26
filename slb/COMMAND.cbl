       IDENTIFICATION DIVISION.
       PROGRAM-ID.    COMMAND.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  28/04/2006.
       SECURITY.      *************************************************
                      *                                               *
                      *  Simula _MSCOMMAND                            *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY CWEXEC.
       LINKAGE SECTION.

       01  LINHA-COMANDO PIC X(255).
       01  LEN-LINHA     PIC 9(008) COMP-X.

       PROCEDURE DIVISION USING LINHA-COMANDO
                                LEN-LINHA.

       000-INICIO.

           IF LEN-LINHA > 255 MOVE 255 TO LEN-LINHA.
           MOVE LINHA-COMANDO (1: LEN-LINHA) TO CWEXEC-COMANDO
           SET CWEXEC-NOWARNING TO TRUE
           CALL "CWEXEC" USING PARAMETROS-CWEXEC.

       000-99-FIM. GOBACK.
       END PROGRAM COMMAND.
