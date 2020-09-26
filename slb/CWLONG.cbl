      $Set NoMS
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWLONG.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  04/07/2009.
       SECURITY.      *************************************************
                      * Tratamento de mensagens longas (At‚ 255 bytes)*
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT OPTIONAL CWHELP ASSIGN TO DISK
                  ORGANIZATION  IS LINE SEQUENTIAL
                  FILE STATUS   IS FS-CWHELP
                  RESERVE NO ALTERNATE AREA
                  LOCK MODE     IS EXCLUSIVE.

       DATA DIVISION.
       FILE SECTION.

       FD  CWHELP
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS CWHELP-FILE.

       01  CWHELP-REG PIC X(80).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO. COPY CWCASE.
           05 S                        PIC  9(003) VALUE 0.
           05 Y                        PIC  9(003) VALUE 0.
           05 M                        PIC  9(003) VALUE 0.
           05 L                        PIC  9(003) VALUE 0.
           05 Z                        PIC  9(003) VALUE 0.
           05 I                        PIC  9(003) VALUE 0.
           05 FS-CWHELP                PIC  X(002) VALUE "00".
           05 USUARIO                  PIC  X(030).
           05 TASK                     PIC  9(006) VALUE 0.
           05 PROGRAMA                 PIC  X(008).
           05 CRON                     PIC  X(003) VALUE SPACES.

       COPY CWUNIX.
       COPY CWSEND.
       COPY CWHELP.

       LINKAGE SECTION.

       01  FUNCAO                 PIC X(001).
       01  MENSAGEM-LONGA         PIC X(255).

       PROCEDURE DIVISION USING FUNCAO MENSAGEM-LONGA.

       000-INICIO.

           ON 1
              DISPLAY "CWCRON" UPON ENVIRONMENT-NAME
              ACCEPT  CRON   FROM ENVIRONMENT-VALUE
              INSPECT CRON
                   CONVERTING MINUSCULAS TO MAIUSCULAS
              CALL "CWUNIX" USING PARAMETROS-CWUNIX
              CALL "CWGETU" USING USUARIO TASK PROGRAMA "?".

           IF FUNCAO = '$'
              MOVE 'ON' TO CRON
              GOBACK
           END-IF

           INSPECT MENSAGEM-LONGA CONVERTING LOW-VALUES TO SPACES

           PERFORM VARYING S FROM LENGTH OF MENSAGEM-LONGA BY -1
                            UNTIL S = 1
                               OR (MENSAGEM-LONGA (S:1) NOT = SPACE)
                    CONTINUE
           END-PERFORM

           IF  MENSAGEM-LONGA(1:1) = '('
           OR  MENSAGEM-LONGA(1:4) = 'ORA-'
               INSPECT MENSAGEM-LONGA(1:S)
                       CONVERTING ACENTOS-WINDOWS
                               TO ACENTOS-850
           END-IF

           IF S < 60
           OR CRON = 'ON'
              EXEC COBOLware LogWrite
                             FUNCTION FUNCAO
                             TEXT     MENSAGEM-LONGA
              END-EXEC
              IF CRON = 'ON'
                 PERFORM VARYING I FROM 1 BY 1
                         UNTIL I NOT < LENGTH MENSAGEM-LONGA
                              OR MENSAGEM-LONGA(I:) = SPACES
                 END-PERFORM
                 DISPLAY MENSAGEM-LONGA(1:I) UPON CONSOLE
                 GOBACK
              ELSE
                 EXEC COBOLware Send Message MENSAGEM-LONGA END-EXEC
              END-IF
           ELSE
              PERFORM 010-HELP THRU 010-99-FIM
           END-IF.

       000-99-FIM. GOBACK.

       010-HELP.

           IF FUNCAO = '^'
              MOVE SPACE TO FUNCAO
           END-IF

           MOVE SPACES TO CWHELP-FILE
           IF   CWUNIX-ON
                STRING "$TEMP/cw" TASK ".hlp" DELIMITED BY SIZE
                  INTO CWHELP-FILE
           ELSE
                STRING "$TEMP\cw" TASK ".hlp" DELIMITED BY SIZE
                  INTO CWHELP-FILE
           END-IF

           OPEN OUTPUT CWHELP
           MOVE 0      TO Y
                          CWHELP-VERTICAL-LENGTH
           MOVE SPACES TO CWHELP-REG
           PERFORM VARYING M FROM 1 BY 1 UNTIL M > S
                   ADD 1 TO Y
                   IF Y > 35
                      ADD 1 TO CWHELP-VERTICAL-LENGTH
                      WRITE CWHELP-REG
                      EXEC COBOLware LogWrite
                                     FUNCTION FUNCAO
                                     TEXT     CWHELP-REG
                      END-EXEC
                      MOVE 1      TO Y
                      MOVE SPACES TO CWHELP-REG
                   END-IF
                   MOVE MENSAGEM-LONGA (M:1) TO CWHELP-REG (Y:1)
                   IF CWHELP-REG = SPACES
                      MOVE 0 TO Y
                      EXIT PERFORM CYCLE
                   END-IF
                   IF CWHELP-REG (Y:1) NOT = SPACE
                   AND M > 1
                      MOVE M TO Z
                      PERFORM VARYING L FROM Y BY 1 UNTIL L > 35
                              OR Z > 255
                              OR MENSAGEM-LONGA (Z:1) = SPACE
                                 ADD 1 TO Z
                      END-PERFORM
                      IF L > 35
                      AND CWHELP-REG (2:) = SPACES
                          PERFORM UNTIL Y = 35
                                  MOVE SPACES TO MENSAGEM-LONGA (M: 1)
                                  ADD 1 TO M Y
                                  MOVE MENSAGEM-LONGA (M: 1)
                                    TO CWHELP-REG (Y:1)
                          END-PERFORM
                          ADD 1 TO Y M
                          ADD 1 TO M
                      END-IF
                      IF L > 35
                         MOVE SPACES TO CWHELP-REG (Y:)
                         ADD 1 TO CWHELP-VERTICAL-LENGTH
                         WRITE CWHELP-REG
                         EXEC COBOLware LogWrite
                                        FUNCTION FUNCAO
                                        TEXT     CWHELP-REG
                         END-EXEC
                         MOVE 0      TO Y
                         MOVE SPACES TO CWHELP-REG
                         SUBTRACT 2 FROM M
                      END-IF
                   END-IF
           END-PERFORM
           IF CWHELP-REG NOT = SPACES
              ADD 1 TO CWHELP-VERTICAL-LENGTH
              WRITE CWHELP-REG
              EXEC COBOLware LogWrite
                             FUNCTION FUNCAO
                             TEXT     CWHELP-REG
              END-EXEC
              MOVE 0      TO Y
              MOVE SPACES TO CWHELP-REG
           END-IF
           CLOSE CWHELP
           INITIALIZE PARAMETROS-CWSEND
           MOVE 1              TO CWSEND-OPTION
           MOVE SPACES         TO CWSEND-SCREENS
           MOVE " ~Exibir"     TO CWSEND-SCREEN (1)
           MOVE " ~Fechar_"    TO CWSEND-SCREEN (2)
           MOVE "Aten‡Æo!, erro no sistema." TO CWSEND-MSG
           PERFORM UNTIL CWSEND-OPTION = 2
                   MOVE 1 TO CWSEND-OPTION
                   CALL "CWSEND" USING PARAMETROS-CWSEND
                   IF   CWSEND-OPTION = 1
                        MOVE 13 TO CWHELP-LINE
                        MOVE 10 TO CWHELP-COLUMN
                        MOVE 35 TO CWHELP-HORIZONTAL-LENGTH
                        CALL "CWHELP" USING PARAMETROS-CWHELP
                   END-IF
           END-PERFORM
           INITIALIZE PARAMETROS-CWSEND
           MOVE 1 TO CWSEND-TIMEOUT-STATUS
           DELETE FILE CWHELP.

       010-99-FIM. EXIT.

       END PROGRAM CWLONG.
