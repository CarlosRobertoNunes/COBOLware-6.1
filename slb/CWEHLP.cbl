       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWEHLP.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  15/12/2003.
       SECURITY.      *************************************************
                      *                                               *
                      *  Editor e exibidor de help do contexto        *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT HELP ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS HELP-CHAVE
                  LOCK MODE     IS MANUAL
                  FILE STATUS   IS FS-HELP.

           SELECT TEXTO ASSIGN TO DISK
                  ORGANIZATION IS BINARY SEQUENTIAL
                  FILE STATUS  IS FS-TEXTO.

       DATA DIVISION.
       FILE SECTION.

       FD  HELP
           LABEL RECORD IS STANDARD
           RECORD VARYING FROM 38 TO 32794 DEPENDING ON SZ-HELP
           VALUE OF FILE-ID IS LB-HELP.

       01  HELP-REG.
           05 HELP-CHAVE.
              10 HELP-CONTEXT-ID      PIC  X(038).
           05 HELP-TEXTO              PIC  X(001) OCCURS 0 TO 32756
              DEPENDING ON S.

       FD  TEXTO
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-TEXTO.

       01  TEXTO-REG PIC X(001).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 HELPDIR            PIC  X(080) VALUE SPACES.
           05 LINHAS             PIC  9(006) VALUE 0.
           05 BARRA              PIC  X(001) VALUE SPACE.
           05 I                  PIC  9(006) VALUE 0.
           05 Y                  PIC  9(006) VALUE 0.
           05 S                  PIC  9(006) VALUE 0.
           05 SZ-HELP            PIC  9(006) VALUE 0.
           05 TMP-LB             PIC  X(012) VALUE "CWE00000.TXT".
           05 COBWARE            PIC  X(250) VALUE SPACES.
           05 ER-HELP.
              10 FS-HELP         PIC  X(002) VALUE "00".
              10 LB-HELP         PIC  X(255) VALUE "HELP".
           05 ER-TEXTO.
              10 FS-TEXTO        PIC  X(002) VALUE "00".
              10 LB-TEXTO        PIC  X(255) VALUE "HELP".
       01  WS-CURPOS.
           05 CURPOS-LIN         PIC  9(002).
           05 CURPOS-COL         PIC  9(002).

       01  COMANDO                     PIC X(256).
       01  CMDSHOW                     PIC 9(4) COMP-5.

       COPY CWGETS.
       COPY CWGETL.
       COPY CWUNIX.
       COPY CWBOXW.
       COPY CWNOTE.
       COPY CWNCOR.

       LINKAGE SECTION.

       01  CONTEXT-ID            PIC  X(038).
       01  TECLA-EDIT            PIC  9(003). COPY CWEDIT.
       01  CURPOS                PIC  X(004).
       01  CWUSER-LK             PIC  X(001).

       PROCEDURE DIVISION USING CONTEXT-ID TECLA-EDIT CURPOS CWUSER-LK.

       000-INICIO.

           ON  1
               CALL "CWUNIX" USING PARAMETROS-CWUNIX
               CALL "CWGETS" USING PARAMETROS-CWGETS
               MOVE CWGETS-TASK (2: 5) TO TMP-LB (4: 5)
               DISPLAY "COBOLWARE" UPON ENVIRONMENT-NAME
               ACCEPT   COBWARE  FROM ENVIRONMENT-VALUE
               MOVE SPACES TO LB-HELP
               STRING "$TEMP" DELIMITED BY SPACE
                      "/"     DELIMITED BY SIZE
                      TMP-LB  DELIMITED BY SPACE
                 INTO LB-TEXTO
               IF  CWUNIX-OFF
                   INSPECT LB-TEXTO CONVERTING "/" TO "\"
                   MOVE "/" TO BARRA
               ELSE
                   MOVE "\" TO BARRA
                   INSPECT LB-TEXTO CONVERTING "\" TO "/"
               END-IF.

           MOVE CURPOS TO WS-CURPOS
      *    IF  CWUSER-LK = "C"
               PERFORM UNTIL (CURPOS-LIN + 10) < 23
                       SUBTRACT 1 FROM CURPOS-LIN
               END-PERFORM
      *    ELSE
      *        PERFORM UNTIL (CURPOS-LIN + 10) < 26
      *                SUBTRACT 1 FROM CURPOS-LIN
      *        END-PERFORM
      *    END-IF

           PERFORM UNTIL (CURPOS-COL + 44) < 78
                   SUBTRACT 1 FROM CURPOS-COL
           END-PERFORM

           IF  CONTEXT-ID (1: 2) = "CW"
               MOVE SPACES TO LB-HELP
               STRING COBWARE DELIMITED BY SPACE
                      "/cwhlpf" DELIMITED BY SIZE
                                INTO LB-HELP
           ELSE
               MOVE "cwhlps"    TO LB-HELP
               CALL "CWFILE" USING LB-HELP
               IF  LB-HELP = "cwhlps"
                   CALL "CWGETL" USING PARAMETROS-CWGETL
                   IF   CWUNIX-ON
                        MOVE CWGETL-HELPDIR-U TO HELPDIR
                   ELSE
                        MOVE CWGETL-HELPDIR-D TO HELPDIR
                   END-IF
                   IF   HELPDIR NOT = SPACES
                        PERFORM VARYING Y FROM LENGTH OF HELPDIR
                                     BY -1
                                     UNTIL Y = 1
                                     OR HELPDIR (Y: ) NOT = SPACE
                                CONTINUE
                        END-PERFORM
                        IF   HELPDIR (Y: ) = "\" OR "/"
                             MOVE SPACE TO HELPDIR (Y: 1)
                             SUBTRACT 1 FROM Y
                        END-IF
                        ADD 1 TO Y
                        MOVE BARRA TO HELPDIR (Y:)
                        ADD 1 TO Y
                        MOVE "cwhlps" TO HELPDIR (Y:)
                        MOVE HELPDIR  TO LB-HELP
                   END-IF
                END-IF
           END-IF

           IF   EDIT-ALT-H
                OPEN I-O HELP
           ELSE
                OPEN INPUT HELP
           END-IF

           OPEN OUTPUT TEXTO
           MOVE 0 TO LINHAS
           MOVE CONTEXT-ID TO HELP-CONTEXT-ID
           READ HELP IGNORE LOCK
           IF  FS-HELP = "00"
           AND SZ-HELP > 38
               ADD 1 TO LINHAS
               COMPUTE S = SZ-HELP - 38
               PERFORM VARYING I FROM 1 BY 1 UNTIL I > S
                        IF  (NOT CWUNIX-ON)
                        OR  (HELP-TEXTO (I) NOT = X"0A")
                            WRITE TEXTO-REG FROM HELP-TEXTO (I)
                        END-IF
               IF  HELP-TEXTO (I) = X"0A"
                   ADD 1 TO LINHAS
               END-IF
               END-PERFORM
           END-IF

           CLOSE TEXTO

           IF  EDIT-ALT-H
           OR (SZ-HELP > 38 AND EDIT-F1)
               IF   EDIT-F1
                    SET CWNOTE-VIEW-WINDOW   TO TRUE
               ELSE
                    SET CWNOTE-UPDATE-WINDOW TO TRUE
               END-IF
               MOVE 0           TO TECLA-EDIT
               MOVE LB-TEXTO    TO CWNOTE-FILE
               MOVE CURPOS-LIN  TO CWNOTE-LINE
               MOVE CURPOS-COL  TO CWNOTE-COLUMN
               MOVE 10          TO CWNOTE-VERTICAL-LENGTH
               MOVE 44          TO CWNOTE-HORIZONTAL-LENGTH
               COMPUTE CWBOXW-LINE   = CWNOTE-LINE     - 1
               COMPUTE CWBOXW-COLUMN = CWNOTE-COLUMN   - 1
               COMPUTE CWBOXW-VERTICAL-LENGTH
                     = CWNOTE-VERTICAL-LENGTH - 0
               COMPUTE CWBOXW-HORIZONTAL-LENGTH
                     = CWNOTE-HORIZONTAL-LENGTH - 0
               IF  CWUSER-LK = "C"
                   SET CWBOXW-OPEN TO TRUE
                   CALL "CWBOXW" USING PARAMETROS-CWBOXW
               END-IF
               IF   CWNOTE-VIEW-WINDOW
                    MOVE 078 TO CWNOTE-COLOR-FRAME
                                CWNOTE-COLOR-BORDER
                    CALL "CWNOTE" USING PARAMETROS-CWNOTE "@"
               ELSE
                    MOVE 110 TO CWNOTE-COLOR-FRAME
                                CWNOTE-COLOR-BORDER
                    CALL "CWNOTE" USING PARAMETROS-CWNOTE CWUSER-LK
               END-IF
               IF  CWUSER-LK = "C"
                   SET CWBOXW-CLOSE TO TRUE
                   CALL "CWBOXW" USING PARAMETROS-CWBOXW "#"
               END-IF
               IF   NOT EDIT-F1
                    OPEN INPUT TEXTO
                    MOVE 38         TO SZ-HELP
                    MOVE CONTEXT-ID TO HELP-CONTEXT-ID
                    READ HELP
                    DELETE HELP RECORD
                    INITIALIZE HELP-REG
                               S
                    MOVE 38         TO SZ-HELP
                    MOVE CONTEXT-ID TO HELP-CONTEXT-ID
                    PERFORM TEST AFTER UNTIL FS-TEXTO > "09"
                            READ TEXTO
                            IF  FS-TEXTO < "10"
                                ADD 1 TO SZ-HELP S
                                MOVE TEXTO-REG TO HELP-TEXTO (S)
                            END-IF
                    END-PERFORM
                    WRITE HELP-REG
                    CLOSE TEXTO
               END-IF
           END-IF

           DELETE FILE TEXTO
           CLOSE HELP.

       000-99-FIM. GOBACK.

       END PROGRAM CWEHLP.
