       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWHELP INITIAL.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  01/03/1992.
       SECURITY.      *************************************************
                      *                                               *
                      *   Exibe arquivo texto em janela               *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA
                         CRT STATUS IS CRT-STATUS.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT INI  ASSIGN TO DISK
                  LOCK MODE    IS EXCLUSIVE
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS  IS FS-INI.

           SELECT HELPOUT ASSIGN TO DISK
                  ORGANIZATION  IS BINARY SEQUENTIAL
                  LOCK MODE     IS EXCLUSIVE.

      $Set IdxFormat"14" DataCompress"1" KeyCompress"7"
           SELECT HELPWK ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD KEY    IS HELPWK-CHAVE
                  LOCK MODE IS EXCLUSIVE
                  FILE STATUS   IS FS-HELPWK.

       DATA DIVISION.
       FILE SECTION.

       FD  INI
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-INI.

       01  INI-REG PIC X(503).

       FD  HELPOUT
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-COPY.

       01  HELPOUT-REG PIC X.

       FD  HELPWK
           RECORD VARYING 10 TO 32778 DEPENDING ON SZ-HELPWK
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-HELPWK.

       01  HELPWK-REG.
           05 HELPWK-CHAVE        COMP-3 PIC  9(018).
           05 HELPWK-TEXTO.
              10 HELPWK-CHAR OCCURS 1 TO 32768 DEPENDING ON SZ-TEXTO
                                         PIC  X(001).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO. COPY CWCASE.
           05 HELPIN-REG           PIC  X.
           05 TESTE-COLOR          PIC  X(006) VALUE SPACES.
           05 JANINI               PIC  9(001) VALUE 2.
           05 TIT-ON               PIC  9(001) VALUE 0.
           05 SPOOL                PIC  9(001) VALUE 0.
           05 COBWARE              PIC  X(100) VALUE SPACES.
           05 X91-RESULT    COMP-X PIC  9(002) VALUE 0.
           05 X91-FUNCTION  COMP-X PIC  9(002) VALUE 16.
           05 X91-PARAMETER COMP-X PIC  9(002) VALUE 0.
           05 HELPINPUT            PIC  X(007) VALUE SPACES.
           05 pesquisa             PIC  X(080) VALUE spaces.
           05 cadeia               PIC  X(080) VALUE spaces.
           05 work                 PIC  X(080) VALUE spaces.
           05 SISTEMA              PIC  9(001) VALUE ZERO.
           05 FULL-ED              PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.
           05 EMPTY-ED             PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.
           05 FINDED               PIC  9(002) VALUE ZERO.
           05 P                    PIC  9(002) VALUE ZERO.
           05 CP                   PIC  9(002) VALUE ZERO.
           05 X                    PIC  9(002) VALUE ZERO.
           05 Y                    PIC  9(002) VALUE ZERO.
           05 TECLA                PIC  9(002) VALUE ZERO. COPY CWKEYS.
           05 JANCODE              PIC  X(010) VALUE SPACES.
           05 LINHAS        COMP-3 PIC  9(018) VALUE 0.
           05 PONTEIRO      COMP-3 PIC  9(018) VALUE 1.
           05 PONTEIRO-A    COMP-3 PIC  9(018) VALUE 1.
           05 COLUNAS       COMP-3 PIC  9(005) VALUE 0.
           05 POS           COMP-3 PIC  9(005) VALUE 0.
           05 POS-A         COMP-3 PIC  9(005) VALUE 0.
           05 H             COMP-3 PIC  9(005) VALUE 0.
           05 T             COMP-3 PIC  9(005) VALUE 0.
           05 S             COMP-3 PIC  9(005) VALUE 0.
           05 FLAG-TOP             PIC  9(001) VALUE 0.
           05 JANLINE              PIC  9(002) VALUE 0.
           05 JANLINS              PIC  9(002) VALUE 0.
           05 JANCOLS              PIC  9(002) VALUE 0.
           05 THUMB-V              PIC  9(005) VALUE 0.
           05 THUMB-VA             PIC  9(005) VALUE 1.
           05 THUMB-H              PIC  9(005) VALUE 0.
           05 THUMB-HA             PIC  9(005) VALUE 2.
           05 ER-HELPIN.
              10 FS-HELPIN         PIC  X(002) VALUE "00".
              10 LB-HELPIN         PIC  X(255) VALUE SPACES.
           05 SZ-TEXTO      COMP-3 PIC  9(005) VALUE 0.
           05 SZ-HELPWK     COMP-3 PIC  9(005) VALUE 0.
           05 ER-HELPWK.
              10 FS-HELPWK         PIC  X(002) VALUE "00".
              10 LB-HELPWK         PIC  X(255) VALUE 'cwhelp$$$'.
           05 ER-INI.
              10 FS-INI            PIC  X(002) VALUE "00".
              10 LB-INI            PIC  X(255) VALUE "C:\HELP##.INI".
           05 LB-COPY              PIC  X(255) VALUE SPACES.
           05 TEXTO                PIC X(32768) VALUE SPACES.
           05 TEXTO2               PIC X(32768) VALUE SPACES.
           05 TOP-STRING           PIC X(32768) VALUE SPACES.
           05 MOSTRA               PIC  X(080) VALUE SPACES.
           05 TOP-ATTR             PIC  X(080) VALUE LOW-VALUES.
           05 MOSTRA-ATTR          PIC  X(080) VALUE LOW-VALUES.
           05 ATTR                 PIC  X(080) VALUE LOW-VALUES.
           05 ATTR-LINE                        VALUE LOW-VALUES.
              10 ATTR-N            PIC  9(002) COMP-X.
           05 ATTR-3               PIC  9(003) VALUE 0.
           05 TITULO               PIC  X(080) VALUE SPACES.
           05 NADA                 PIC  X(001) VALUE SPACES.
           05 CRT-STATUS.
              10 t-tecla           pic  X(001).
              10 t2                pic  9(002) comp-x. copy cwedit.
              10 t3                pic  9(002) comp-x.
           05 SCREEN-POSITION.
              15 ROW-NUMBER        PIC  9(002) COMP-X.
              15 COLUMN-NUMBER     PIC  9(002) COMP-X.
           05 STRING-LENGTH        PIC  9(004) COMP-X.
           05 CHARACTER-BUFFER.
              15 CHAR-LIN          PIC  X(080) OCCURS 25.
           05 ATTRIBUTE-BUFFER.
              15 ATTR-LIN          PIC  X(080) OCCURS 25.

       COPY CWSEND.
       COPY CWIMPR.
       COPY CWEXEC.

       LINKAGE SECTION.

       COPY CWHELP.

       01  LIXO PIC X.

       SCREEN SECTION.

       01  TELA-TESTE.
           02 line 1 column 1 pic x(80) using pesquisa size p.

       PROCEDURE DIVISION USING PARAMETROS-CWHELP LIXO.

       000-INICIO.

060617*    ON 1
              DISPLAY 'CWHELP-SPOOL' UPON ENVIRONMENT-NAME
              ACCEPT CADEIA FROM ENVIRONMENT-VALUE
              IF CADEIA = SPACES
                 DISPLAY 'CWHELP_SPOOL' UPON ENVIRONMENT-NAME
                 ACCEPT CADEIA FROM ENVIRONMENT-VALUE
              END-IF
              INSPECT CADEIA CONVERTING 'on' TO 'ON'
              IF  CADEIA = 'ON'
                  MOVE 1 TO SPOOL
              END-IF
              MOVE SPACES TO CADEIA
              EXEC COBOLware System OS-CODE;sistema END-EXEC.

           DISPLAY 'CWHELP-RUN' UPON ENVIRONMENT-NAME
           DISPLAY 'ON'         UPON ENVIRONMENT-VALUE

           IF   CWHELP-TIMEOUT-RETURN = 9
                IF CWHELP-SPOOL
                   MOVE 1 TO SPOOL
                END-IF
           END-IF
           IF   CWHELP-TYPE = 9
                MOVE 0 TO CWHELP-TYPE
                MOVE 1 TO TIT-ON
           ELSE
                MOVE 0 TO TIT-ON
           END-IF
           IF   CWHELP-OLDFILE = SPACES
                MOVE CWHELP-FILE    TO LB-HELPIN
           ELSE
                MOVE CWHELP-OLDFILE TO LB-HELPIN
           END-IF
           CALL 'CWHELPFS' USING 'I'
                                  LB-HELPIN
                                  FS-HELPIN
                                     HELPIN-REG

           IF FS-HELPIN NOT = '00'
              CALL 'CWISAM' USING ER-HELPIN
              GOBACK
           END-IF
           IF   CWHELP-TIMEOUT-RETURN = 9
                INSPECT TOP-ATTR
                        CONVERTING LOW-VALUES TO CWHELP-COLOR-TOP(1:1)
           ELSE
                INSPECT TOP-ATTR
                        CONVERTING LOW-VALUES TO CWHELP-COLOR-FRAME(1:1)
           END-IF
           INSPECT MOSTRA-ATTR
                   CONVERTING LOW-VALUES TO CWHELP-COLOR-FRAME(1:1)

           CALL X"91" USING X91-RESULT X91-FUNCTION X91-PARAMETER

           IF X91-PARAMETER > 1
           AND LIXO = '#'
               MOVE 'DOS' TO HELPINPUT
           ELSE
               DISPLAY "CWHELPINPUT" UPON ENVIRONMENT-NAME
               ACCEPT   HELPINPUT    FROM ENVIRONMENT-VALUE
               INSPECT  HELPINPUT   CONVERTING MINUSCULAS TO MAIUSCULAS
           END-IF

           MOVE ZERO TO SZ-TEXTO
                        LINHAS
           MOVE LENGTH HELPWK-CHAVE TO SZ-HELPWK
           OPEN I-O HELPWK
           PERFORM UNTIL FS-HELPIN > '09'
                   CALL 'CWHELPFS' USING 'R'
                                          LB-HELPIN
                                          FS-HELPIN
                                             HELPIN-REG
                   IF  FS-HELPIN < '10'
                       EVALUATE HELPIN-REG
                           WHEN X'0D'
                                CONTINUE
                           WHEN X'0A'
                                CALL 'CWATCH'
                                ADD 1 TO LINHAS
                                IF   CWHELP-TIMEOUT-RETURN = 9
                                AND  LINHAS = 1
                                AND  FLAG-TOP = 0
                                     IF CWHELP-TOP
                                        MOVE HELPWK-TEXTO TO TOP-STRING
                                        MOVE 1            TO FLAG-TOP
                                        MOVE 0            TO LINHAS
                                                             SZ-TEXTO
                                        MOVE LENGTH HELPWK-CHAVE
                                                          TO SZ-HELPWK
                                        EXIT PERFORM CYCLE
                                     END-IF
                                ELSE
                                     IF   LINHAS = 1
                                     AND  TIT-ON = 1
                                     AND  FLAG-TOP = 0
                                          MOVE HELPWK-TEXTO
                                            TO TITULO
                                          MOVE 2  TO FLAG-TOP
                                          MOVE 0  TO LINHAS
                                                     SZ-TEXTO
                                          MOVE LENGTH HELPWK-CHAVE
                                            TO SZ-HELPWK
                                          EXIT PERFORM CYCLE
                                     END-IF
                                END-IF
                                MOVE LINHAS TO HELPWK-CHAVE
                                PERFORM UNTIL SZ-TEXTO = 0
                                      OR HELPWK-CHAR (SZ-TEXTO) <> SPACE
                                        SUBTRACT 1 FROM SZ-TEXTO
                                END-PERFORM
                                IF SZ-TEXTO > COLUNAS
                                   MOVE SZ-TEXTO TO COLUNAS
                                END-IF
                                COMPUTE SZ-HELPWK = LENGTH HELPWK-CHAVE
                                                  + SZ-TEXTO
                                WRITE HELPWK-REG
                                MOVE 0                   TO SZ-TEXTO
                                MOVE LENGTH HELPWK-CHAVE TO SZ-HELPWK
                           WHEN OTHER
                                IF SZ-TEXTO < 32768
                                   ADD 1 TO SZ-TEXTO
                                            SZ-HELPWK
                                   MOVE HELPIN-REG
                                     TO HELPWK-CHAR (SZ-TEXTO)
                                END-IF
                       END-EVALUATE
                   END-IF
           END-PERFORM

           CALL 'CWHELPFS' USING 'C'
                                  LB-HELPIN
                                  FS-HELPIN
                                     HELPIN-REG

           CANCEL 'CWHELPFS'
           IF   CWHELP-TIMEOUT-RETURN = 9
                IF CWHELP-TITLE = SPACES
                   IF   LB-HELPIN(1:5) = '$TEMP'
                        MOVE SPACES      TO TITULO
                   ELSE
                        MOVE CWHELP-FILE TO TITULO
                   END-IF
                ELSE
                   MOVE CWHELP-TITLE     TO TITULO
                END-IF
           END-IF

           IF   LINHAS > CWHELP-VERTICAL-LENGTH
                ADD 1 TO CWHELP-VERTICAL-LENGTH
           END-IF

           IF  sistema = 6 OR 7
               IF   CWHELP-VERTICAL-LENGTH > 23
                    MOVE 23 TO CWHELP-VERTICAL-LENGTH
               END-IF
           ELSE
               IF   CWHELP-VERTICAL-LENGTH > 22
                    MOVE 22 TO CWHELP-VERTICAL-LENGTH
               END-IF
               ADD 1 TO JANINI
           END-IF

           EXEC COBOLware Window Open
                HEADER  TITULO
                  LINE  CWHELP-LINE
                COLUMN  CWHELP-COLUMN
                 LINES  CWHELP-VERTICAL-LENGTH
                  SIZE  CWHELP-HORIZONTAL-LENGTH
           COLOR-FRAME  CWHELP-COLOR-FRAME
           COLOR-BORDER CWHELP-COLOR-BORDER
           COLOR-SHADE  CWHELP-COLOR-SHADE
             REFERENCE;JANCODE
                 ERASE
           END-EXEC

           MOVE CWHELP-VERTICAL-LENGTH   TO JANLINS
           MOVE CWHELP-HORIZONTAL-LENGTH TO JANCOLS

           IF  CWHELP-TOP
               ADD 1 TO JANINI
           END-IF

           IF   LINHAS > JANLINS
                SUBTRACT 2 FROM JANLINS
                if sistema = 6 or 7
                SUBTRACT 1 FROM JANLINS
                end-if
                EXEC COBOLware OBJECT VERTICAL SCROLL
                       LINE 1
                     COLUMN JANCOLS
                     HEIGHT JANLINS
                     KEY F2 THUMB THUMB-V
                END-EXEC
                SUBTRACT 1 FROM JANCOLS
                ADD      2   TO JANLINS
                if sistema = 6 or 7
                   ADD      1   TO JANLINS
                end-if
           END-IF

           IF   COLUNAS > JANCOLS
                EXEC COBOLware OBJECT HORIZONTAL SCROLL
                     LINE   JANLINS
                     COLUMN 1
                     WIDTH  JANCOLS
                     KEY F4 THUMB THUMB-H
                END-EXEC
                SUBTRACT 1 FROM JANLINS
           END-IF

           IF  sistema = 6 OR 7
               COMPUTE P = CWHELP-HORIZONTAL-LENGTH - 18
           ELSE
               COMPUTE P = CWHELP-HORIZONTAL-LENGTH - 24
           END-IF
                    SUBTRACT 1 FROM JANLINS

           COMPUTE CP = P + 2
           EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                LINE 1 COLUMN CP
                CAPTION "~Procura" WIDTH 7
                KEY F3 TAB-OFF
           END-EXEC
           ADD 8 TO CP
           EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                LINE 1 COLUMN CP
                CAPTION "~Imprime" WIDTH 7
                KEY F5 TAB-OFF
           END-EXEC
           IF  NOT (sistema = 6 OR 7)
               ADD 8 TO CP
               EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                   LINE 1 COLUMN CP
                   CAPTION "~Fecha" WIDTH 5
                   KEY ESC TAB-OFF
               END-EXEC
           END-IF

           IF  SISTEMA = 6 OR 7
               MOVE 1         TO Y
           ELSE
               MOVE CWHELP-COLUMN TO Y
               IF CWHELP-COLUMN = 1
                  COMPUTE Y = CWHELP-COLUMN + 1
               END-IF
           END-IF

           MOVE 1 TO POS
           PERFORM UNTIL ESC
                   CALL "CBL_READ_SCR_CHATTRS" USING X'0000'
                                                     CHARACTER-BUFFER
                                                     ATTRIBUTE-BUFFER
                                                     X'07D0'
                   MOVE PONTEIRO TO HELPWK-CHAVE
                   MOVE JANINI   TO JANLINE
                   PERFORM JANLINS TIMES
                      MOVE X'00' TO ATTR-LINE
                      IF  FLAG-TOP = 1
                      AND JANLINE = JANINI
                          MOVE TOP-STRING(POS:) TO MOSTRA
                          MOVE TOP-ATTR         TO ATTR
                      ELSE
                          READ HELPWK
                          IF FS-HELPWK > '09'
                             MOVE SPACES TO MOSTRA
                          ELSE
                             COMPUTE SZ-TEXTO = SZ-HELPWK - 10
                             MOVE HELPWK-TEXTO(1:SZ-TEXTO) TO TEXTO
                             MOVE TEXTO(1:6) TO TESTE-COLOR
                             INSPECT TESTE-COLOR CONVERTING MINUSCULAS
                                                         TO MAIUSCULAS
                             IF TESTE-COLOR = '$COLOR'
                                MOVE TEXTO(7:3) TO ATTR-3(1:3)
                                MOVE TEXTO(10:) TO TEXTO2
                                MOVE TEXTO2     TO TEXTO
                                MOVE ATTR-3     TO ATTR-N
                             END-IF
                             MOVE TEXTO(POS:)              TO MOSTRA
                             MOVE MOSTRA-ATTR              TO ATTR
                             ADD 1 TO HELPWK-CHAVE
                          END-IF
                      END-IF
                      IF  sistema = 6 OR 7
                          MOVE JANLINE TO X
                          IF X = 2
                             MOVE MOSTRA-ATTR TO ATTR-LIN(1)(Y:JANCOLS)
                          END-IF
                      ELSE
                          if cwhelp-line = 1
                             COMPUTE X = CWHELP-LINE + JANLINE - 1
                          else
                             COMPUTE X = CWHELP-LINE + JANLINE - 2
                          end-if
                      END-IF
                      MOVE MOSTRA      TO CHAR-LIN(X)(Y:JANCOLS)
                      IF ATTR-LINE = X'00'
                         MOVE ATTR     TO ATTR-LIN(X)(Y:JANCOLS)
                      ELSE
                         MOVE LOW-VALUES TO ATTR-LIN(X)(Y:JANCOLS)
                         INSPECT ATTR-LIN(X)(Y:JANCOLS)
                                 CONVERTING LOW-VALUE TO ATTR-LINE
                      END-IF
                      ADD 1 TO JANLINE
                   END-PERFORM
                   CALL "CBL_WRITE_SCR_CHATTRS" USING X'0000'
                                                     CHARACTER-BUFFER
                                                     ATTRIBUTE-BUFFER
                                                     X'07D0'
                   MOVE THUMB-V TO THUMB-VA
                   MOVE THUMB-H TO THUMB-HA
                   ACCEPT TELA-TESTE
                   ACCEPT TECLA FROM ESCAPE KEY
                   MOVE PONTEIRO TO PONTEIRO-A
                   MOVE POS      TO POS-A
                   EVALUATE TRUE
                        WHEN F5
                          IF   TITULO = SPACES
                               PERFORM VARYING H FROM LENGTH LB-HELPIN
                                          BY -1
                                       UNTIL H = 1
                                          OR LB-HELPIN(H:1) = "\" OR "/"
                                       CONTINUE
                               END-PERFORM
                               IF   H > 1
                                    ADD 1 TO H
                               END-IF
                               MOVE LB-HELPIN(H:) TO CWIMPR-TITLE
                          ELSE
                              MOVE TITULO        TO CWIMPR-TITLE
                          END-IF
                          EXEC COBOLware Pack String CWIMPR-TITLE
                                         WIDTH T
                          END-EXEC
                          IF   SISTEMA = 1
                          OR   SPOOL = 1
                               PERFORM 830-IMPRIME THRU 830-99-FIM
                          ELSE
                               PERFORM 835-IMPRIME-POSTER
                                  THRU 835-99-FIM
                          END-IF
                          MOVE 0 TO TECLA T2
                        WHEN F3 OR(TECLA = ZERO AND T2 = 11 AND T3 = 13)
                         IF  PESQUISA NOT = SPACES
                             MOVE PESQUISA TO CADEIA
                             IF PESQUISA(1:1) = SPACE
                                MOVE 1 TO FINDED
                             ELSE
                                MOVE 0 TO FINDED
                             END-IF
                             EXEC COBOLware Pack String CADEIA
                                             WIDTH T
                             END-EXEC
                             IF FINDED = 1
                                MOVE CADEIA TO WORK
                                MOVE SPACE  TO CADEIA
                                MOVE WORK   TO CADEIA(2:)
                                ADD  1      TO T
                             END-IF
                             INSPECT CADEIA
                               CONVERTING ACENTOS-850 TO ACENTOS-OFF
                             INSPECT CADEIA
                                CONVERTING MINUSCULAS  TO MAIUSCULAS
                             IF CADEIA(T:1) = '_'
                                MOVE SPACE TO CADEIA(T:1)
                             END-IF
                             MOVE PONTEIRO TO HELPWK-CHAVE
                             MOVE 0 TO FINDED
                             PERFORM UNTIL FS-HELPWK > '09'
                                     ADD 1 TO HELPWK-CHAVE
                                     CALL 'CWATCH'
                                     READ HELPWK
                                     IF FS-HELPWK < '09'
                                        PERFORM 100-PESQUISA
                                           THRU 100-99-FIM
                                        IF FINDED = 1
                                           MOVE HELPWK-CHAVE
                                             TO PONTEIRO
                                           EXIT PERFORM
                                        END-IF
                                     END-IF
                             END-PERFORM
                             IF  FINDED = 0
                                 EXEC COBOLware Send
                                      Message 'NÆo encontrado'
                                 END-EXEC
                             END-IF
                         END-IF
                         EXIT PERFORM CYCLE
                        WHEN F2
                             EVALUATE TRUE
                               WHEN THUMB-V = THUMB-VA + 1
                                    SET CURSOR-DOWN TO TRUE
                               WHEN THUMB-V = THUMB-VA - 1
                                    SET CURSOR-UP TO TRUE
                               WHEN THUMB-V < 2
                                    SET CONTROL-PAGE-UP TO TRUE
                               WHEN THUMB-V > 99
                                    SET CONTROL-PAGE-DOWN TO TRUE
                               WHEN THUMB-V NOT = THUMB-VA
                                    COMPUTE PONTEIRO
                                        = (LINHAS / 100) * THUMB-V
                             END-EVALUATE
                        WHEN F4
                             EVALUATE TRUE
                               WHEN THUMB-H = THUMB-HA + 1
                                AND POS < (COLUNAS - JANCOLS)
                                    ADD 1 TO POS
                               WHEN THUMB-H = THUMB-HA - 1
                                 AND POS > 1
                                     SUBTRACT 1 FROM POS
                               WHEN THUMB-H < 2
                                    MOVE 1 TO POS
                               WHEN THUMB-H > 99
                                    COMPUTE POS = COLUNAS - JANCOLS
                               WHEN THUMB-H NOT = THUMB-HA
                                    COMPUTE POS = (COLUNAS / 100)
                                                * THUMB-H
                             END-EVALUATE
                   END-EVALUATE
                   EVALUATE TRUE
                        WHEN EDIT-CONTROL-CURSOR-LEFT
                             MOVE 1 TO POS
                        WHEN EDIT-CONTROL-CURSOR-RIGHT
                             COMPUTE POS = COLUNAS - JANCOLS
                        WHEN T-TECLA = 2
                         AND T2 = 3
                             IF POS > 1
                                SUBTRACT 1 FROM POS
                             END-IF
                        WHEN T-TECLA = 2
                         AND T2 = 4
                         AND POS < (COLUNAS - JANCOLS)
                             ADD 1 TO POS
                        WHEN CURSOR-UP
                             IF PONTEIRO > 1
                                SUBTRACT 1 FROM PONTEIRO
                             END-IF
                        WHEN CURSOR-DOWN OR F9
                             IF (PONTEIRO + JANLINS) < LINHAS
                                ADD 1 TO PONTEIRO
                             END-IF
                        WHEN PAGE-DOWN
                             IF (PONTEIRO + JANLINS) < LINHAS
                                ADD JANLINS TO PONTEIRO
                             END-IF
                        WHEN PAGE-UP
                             IF PONTEIRO > JANLINS
                                SUBTRACT JANLINS FROM PONTEIRO
                             ELSE
                                MOVE 1 TO PONTEIRO
                             END-IF
                        WHEN CONTROL-PAGE-UP
                             MOVE 1 TO PONTEIRO
                        WHEN CONTROL-PAGE-DOWN
                             MOVE LINHAS TO PONTEIRO
                             PERFORM JANLINS TIMES
                                  IF PONTEIRO > 1
                                     SUBTRACT 1 FROM PONTEIRO
                                  END-IF
                             END-PERFORM
                   END-EVALUATE
                   COMPUTE THUMB-V = PONTEIRO / LINHAS * 100
                   IF((TECLA NOT = 0)
                   OR (T2 NOT = 0))
                   AND  PONTEIRO = PONTEIRO-A
                   AND POS = POS-A
                   AND NOT F3
                   AND NOT F5
                   AND NOT ESC
                       CALL X'E5'
                   END-IF
           END-PERFORM

           CLOSE HELPWK
           EXEC COBOLWARE OBJECT DROP END-EXEC
           EXEC COBOLWARE WINDOW CLOSE
                REFERENCE (JANCODE)
           END-EXEC
           DISPLAY 'CWHELP-RUN' UPON ENVIRONMENT-NAME
           DISPLAY SPACES       UPON ENVIRONMENT-VALUE.

       000-FIM. EXIT PROGRAM.

       100-PESQUISA.

      *    EXEC COBOLware Pack String HELPWK-TEXTO(1:SZ-TEXTO)
           EXEC COBOLware Pack String HELPWK-TEXTO
                WIDTH H
           END-EXEC
           COMPUTE SZ-TEXTO = SZ-HELPWK - LENGTH HELPWK-CHAVE
           INSPECT HELPWK-TEXTO(1:H)
                   CONVERTING ACENTOS-850 TO ACENTOS-OFF
           INSPECT HELPWK-TEXTO(1:H)
                   CONVERTING MINUSCULAS  TO MAIUSCULAS
           ADD 1 TO H
           PERFORM VARYING S FROM 1 BY 1 UNTIL (S + T) > H
                   IF CADEIA (1:T) = HELPWK-TEXTO(S:T)
                      MOVE 1 TO FINDED
                      EXIT PERFORM
                   END-IF
           END-PERFORM.

       100-99-FIM. EXIT.

       830-IMPRIME.

           MOVE SPACES TO CWSEND-MSG
           IF COLUNAS > 500
              STRING "Imprimir " CWIMPR-TITLE(1:T)
                 " ? (excedente de 500 colunas ser  truncado)"
                            DELIMITED BY SIZE
                           INTO CWSEND-MSG
           ELSE
              STRING "Imprimir " CWIMPR-TITLE(1:T) " ?"
                     DELIMITED BY SIZE
                           INTO CWSEND-MSG
           END-IF
           MOVE "   ~Ok___"  TO CWSEND-SCREEN (1)
           MOVE "~Cancelar"  TO CWSEND-SCREEN (2)
           CALL "CWSEND" USING PARAMETROS-CWSEND
           MOVE SPACES TO CWSEND-SCREENS
           IF   CWSEND-OPTION = 2
                GO TO 830-99-FIM
           END-IF
           DISPLAY 'Imprimindo...' AT 0101
                                  WITH SIZE CWHELP-HORIZONTAL-LENGTH
           MOVE "CWHELP"         TO CWIMPR-REPORT
           IF TOP-STRING = SPACES
              MOVE 99            TO CWIMPR-SIZE-PAGE
           ELSE
              MOVE TOP-STRING    TO CWIMPR-HEADER-1
           END-IF
           MOVE 0                TO CWIMPR-FULL
           MOVE LINHAS           TO CWIMPR-EMPTY EMPTY-ED
           MOVE "Texto de ajuda" TO CWIMPR-NOTE
           MOVE 0                TO HELPWK-CHAVE
           START HELPWK KEY NOT LESS HELPWK-CHAVE
           IF COLUNAS > 500
              MOVE 500 TO CWIMPR-HORIZONTAL-LENGTH
           ELSE
              MOVE COLUNAS TO CWIMPR-HORIZONTAL-LENGTH
           END-IF
           PERFORM UNTIL FS-HELPWK > "09"
                      OR CWIMPR-END-PRINT
                   MOVE SPACES TO HELPWK-TEXTO
                   READ HELPWK NEXT RECORD
                   IF   FS-HELPWK < "10"
                        MOVE HELPWK-TEXTO TO CWIMPR-DETAIL
                        IF HELPINPUT = 'WINDOWS'
                           INSPECT CWIMPR-DETAIL
                                   CONVERTING ACENTOS-WINDOWS
                                           TO ACENTOS-850
                        END-IF
                        ADD  1           TO CWIMPR-FULL
                        MOVE CWIMPR-FULL TO FULL-ED
                        MOVE SPACES      TO CWSEND-MSG
                        STRING FULL-ED(1:) ' de ' EMPTY-ED(1:)
                        ' linhas.'
                           DELIMITED BY SIZE INTO CWSEND-MSG
                        EXEC COBOLware Pack String CWSEND-MSG
                                        WIDTH T
                        END-EXEC
                        DISPLAY CWSEND-MSG AT 0115 WITH SIZE T
                        CALL "CWIMPF" USING PARAMETROS-CWIMPR
                   END-IF
           END-PERFORM

           DISPLAY SPACES AT 0101 WITH SIZE CWHELP-HORIZONTAL-LENGTH

           IF   NOT CWIMPR-END-PRINT
                SET CWIMPR-CLOSE TO TRUE
                CALL "CWIMPF" USING PARAMETROS-CWIMPR
                MOVE "Texto de ajuda impresso" TO CWSEND-MSG
                CALL "CWSEND" USING PARAMETROS-CWSEND
           END-IF
           CANCEL "CWIMPF".

       830-99-FIM. EXIT.

       835-IMPRIME-POSTER.

           DISPLAY "TEMP" UPON ENVIRONMENT-NAME
           ACCEPT  LB-INI FROM ENVIRONMENT-VALUE
           MOVE    LB-INI   TO LB-COPY
           PERFORM VARYING H FROM LENGTH OF LB-INI BY -1
                   UNTIL H = 1
                      OR (LB-INI(H:1) NOT = SPACES)
                   CONTINUE
           END-PERFORM
           IF   H > 1
                ADD 1 TO H
           END-IF

           MOVE "\cwhelp##.ini" TO LB-INI (H: )
           MOVE "\cwhelp##.txt" TO LB-COPY (H: )
           CALL "CWFILE" USING LB-INI
           CALL "CWFILE" USING LB-COPY
           OPEN OUTPUT INI
           IF HELPINPUT NOT = 'WINDOWS'
              CALL "CBL_COPY_FILE" USING LB-HELPIN LB-COPY
           ELSE
               CALL 'CWHELPFS' USING 'I'
                                      LB-HELPIN
                                      FS-HELPIN
                                         HELPIN-REG
               OPEN OUTPUT HELPOUT
               PERFORM TEST AFTER UNTIL FS-HELPIN NOT =  '00'
                       CALL 'CWHELPFS' USING 'R'
                                              LB-HELPIN
                                              FS-HELPIN
                                                 HELPOUT-REG
                        IF FS-HELPIN = '00'
                           INSPECT HELPOUT-REG
                           CONVERTING ACENTOS-WINDOWS TO ACENTOS-850
                           WRITE HELPOUT-REG
                        END-IF
               END-PERFORM
               CALL 'CWHELPFS' USING 'C'
                                     LB-HELPIN
                                     FS-HELPIN
                                        HELPIN-REG

               CANCEL 'CWHELPFS'
               CLOSE HELPOUT
           END-IF
           WRITE INI-REG FROM "[Config]"
           MOVE "Infile=" TO INI-REG
           MOVE LB-COPY   TO INI-REG(8:)
           WRITE INI-REG
           WRITE INI-REG FROM "OutFile=Printer"
           MOVE  SPACES TO INI-REG
           STRING "Title=" CWIMPR-TITLE(1:T) DELIMITED BY SIZE
                                   INTO INI-REG
           WRITE INI-REG
           CLOSE INI
           MOVE SPACES TO CWEXEC-COMANDO
           DISPLAY "COBOLWARE" UPON ENVIRONMENT-NAME
           ACCEPT  COBWARE FROM ENVIRONMENT-VALUE
           STRING COBWARE DELIMITED BY SPACE
                  "\poster.exe " DELIMITED BY SIZE
                  LB-INI DELIMITED BY SIZE
             INTO CWEXEC-COMANDO
           SET  CWEXEC-ASSYNCRONE TO TRUE
           SET  CWEXEC-NOWARNING  TO TRUE
=====>*    SET  CWEXEC-HIDE       TO TRUE
           CALL "CWEXEC"   USING PARAMETROS-CWEXEC.

       835-99-FIM. EXIT.

       END PROGRAM CWHELP.
