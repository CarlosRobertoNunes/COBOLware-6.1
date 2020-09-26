       IDENTIFICATION DIVISION.
       PROGRAM-ID.    SPOOL INITIAL.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  04/07/2002.
       SECURITY.      *************************************************
                      *                                               *
                      *  Transfere arquivo para spool                 *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT RELATO ASSIGN TO DISK
                  ORGANIZATION  IS BINARY SEQUENTIAL
                  LOCK MODE IS EXCLUSIVE
                  FILE STATUS   IS FS-RELATO.

       DATA DIVISION.
       FILE SECTION.

       FD  RELATO
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-RELATO.

       01  RELATO-REG.
           05 RELATO-TEXTO             PIC  X(001).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 MENSAGEM                 PIC  X(255) VALUE SPACES.
           05 TEMP                     PIC  X(255) VALUE SPACES.
           05 ANTERIOR                 PIC  X(001) VALUE SPACE.
           05 I                        PIC  9(018) VALUE 0.
           05 Y                        PIC  9(003) VALUE 0.
           05 OS                       PIC  9(001) VALUE 0.
           05 ER-RELATO.
              10 FS-RELATO             PIC  X(002) VALUE "00".
              10 LB-RELATO             PIC  X(255) VALUE SPACES.

       LINKAGE SECTION.

       01  PARAMETROS-CWSPLF.
           05 CWSPLF-FILE         PIC X(050).
           05 CWSPLF-REPORT       PIC X(007).
           05 CWSPLF-FORM-TYPE    PIC X(001).
           05 CWSPLF-TITLE        PIC X(174).
           05 CWSPLF-NOTE         PIC X(020).

       PROCEDURE DIVISION USING PARAMETROS-CWSPLF.

       000-INICIO.

           IF   CWSPLF-FILE = SPACES
                GOBACK
           END-IF

           MOVE CWSPLF-FILE TO LB-RELATO
           OPEN INPUT RELATO
           IF   FS-RELATO NOT = "00"
                MOVE SPACES TO MENSAGEM
                STRING "Arquivo " DELIMITED BY SIZE
                        LB-RELATO DELIMITED BY SPACE
                        " n∆o encontrado." DELIMITED BY SIZE
                INTO MENSAGEM
                EXEC COBOLware Send
                     Message MENSAGEM
                END-EXEC
                GOBACK
           END-IF
           CLOSE RELATO
           MOVE SPACES TO MENSAGEM
           ON   1
                EXEC COBOLware SYSTEM OS-CODE OS END-EXEC.

           IF   OS NOT = 1
                DISPLAY "TEMP" UPON ENVIRONMENT-NAME
                ACCEPT TEMP FROM ENVIRONMENT-VALUE
                STRING "PRINT "      DELIMITED BY SIZE
                       TEMP          DELIMITED BY SPACE
                       LB-RELATO(6:) DELIMITED BY SPACE
                       INTO MENSAGEM
           ELSE
                STRING "sh putty "  DELIMITED BY SIZE
                       LB-RELATO    DELIMITED BY SPACE
                       INTO MENSAGEM
           END-IF
           EXEC COBOLware ExecSys Command MENSAGEM END-EXEC
           DELETE FILE RELATO.

       000-99-FIM. GOBACK.
       END PROGRAM SPOOL.

