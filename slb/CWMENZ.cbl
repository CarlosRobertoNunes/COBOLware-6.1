       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWMENZ.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  09/05/1990.
       SECURITY.      *************************************************
                      *                                               *
                      *   Manutencao de parametros genericos          *
                      *                                               *
                      *   Modulo de manutencao de GRUPOS              *
                      *  (Antigo)                                     *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      $Set IdxFormat"14" DataCompress"1" KeyCompress"7"
           SELECT OPTIONAL CWMENW ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS CWMENW-CHAVE
                  LOCK MODE     IS AUTOMATIC
                  FILE STATUS   IS FS-CWMENW.

       DATA DIVISION.
       FILE SECTION.

       FD  CWMENW
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-CWMENW.

       01  CWMENW-REG.
           05 CWMENW-CHAVE.
              10 CWMENW-PAGINA          PIC 9(003).
           05 CWMENW-LINHA OCCURS 8.
              10 CWMENW-PROGRAMA        PIC X(008).
              10 CWCHECK-ACESSO         PIC X(001).
              10 CWCHECK-ALTERACAO      PIC X(001).
              10 CWCHECK-CONSULTA       PIC X(001).
              10 CWCHECK-EXCLUSAO       PIC X(001).
              10 CWCHECK-INCLUSAO       PIC X(001).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1. COPY CWCASE.
           05 footline-t.
              10 footline              PIC  9(002) VALUE 23.
           05 MENULIST                 PIC  X(002) VALUE SPACES.
           05 ITEM                     PIC  9(006) VALUE 0.
           05 ITEM-99                  PIC  9(006) VALUE 0.
           05 OUTRO                    PIC  X(050) VALUE SPACES.
           05 SAVE-ID                  PIC  X(005) VALUE SPACES.
           05 NEW-ITEM                 PIC  X(080) VALUE SPACES.
           05 FIM                      PIC  9(001) VALUE ZERO.
           05 HOME                     PIC  9(001) VALUE ZERO.
           05 K                        PIC  9(002) VALUE ZERO.
           05 K2                       PIC  9(002) VALUE ZERO.
           05 F                        PIC  9(003) VALUE 0.
           05 B                        PIC  9(003) VALUE 0.
           05 U                        PIC  9(002) VALUE 0.
           05 OC                       PIC  9(002) VALUE 0.
           05 ESPACOS                  PIC  X(080) VALUE SPACES.
           05 TESTE-OPCAO              PIC  X(001) VALUE SPACE.
           05 ADM                      PIC  X(001) VALUE SPACE.
           05 TMP                      PIC  X(050) VALUE SPACES.
           05 TMP-LB                   PIC  X(012) VALUE "CW500000".
           05 BYTE-X                   PIC  X(001) VALUE SPACE.
           05 TABELA-CORES.
              10 COR PIC 9(002) COMP-X OCCURS 256.
           05 TABELA-MOLDURA.
              10 BASE-MOLDURA PIC X(8) OCCURS 9.
           05 CURSOR-POSITION.
              10                       PIC  9(002) COMP-X VALUE 00.
              10                       PIC  9(002) COMP-X VALUE 00.
           05 SALVA-CHAVE.
              10                       PIC  X(002) VALUE SPACES.
              10 GRUPO                 PIC  X(022) VALUE SPACES.
              10                       PIC  X(008) VALUE SPACES.
           05 OK                       PIC  9(001) VALUE ZERO.
           05 MODO                     PIC  9(001) VALUE ZERO.
              88 CONSULTA-R                        VALUE 1.
              88 ALTERAR-R                         VALUE 0.
           05 TECLA                    PIC  9(003) VALUE 0. COPY CWKEYS.
           05 FL-EXIT                  PIC  9(001) VALUE 1.
           05 PROGRAMA-X               PIC  X(008) VALUE SPACES.
           05 TASK                     PIC  9(006) VALUE ZERO.
           05 PROGRAMA                 PIC  X(008) VALUE "CWMENU".
              88 MODULO-CWMENU VALUE "CWMENU" "CWPAGE".
           05 REDEFINES PROGRAMA.
              10 PK-X OCCURS 8 COMP-X  PIC  9(002).
           05 NOME                     PIC  X(030) VALUE "LOGON".
           05 YY                       PIC  9(003) VALUE 0.
           05 II                       PIC  9(003) VALUE 0.
           05 I                        PIC  9(003) VALUE 0.
           05 Y                 COMP-X PIC  9(008) VALUE ZERO.
           05 Z                        PIC  9(003) VALUE 0.
           05 POP-W                                VALUE "000469".
              10 LL                    PIC  9(002).
              10 CC.
                 15 CCA                PIC  9(002).
                 15 CCB                PIC  9(002).
           05 POP                      PIC  X(080) VALUE SPACES.
           05 PAGINA                   PIC  9(003) VALUE 1.
           05 ULTIMA                   PIC  9(003) VALUE 0.
           05 ER-CWMENW.
              10 FS-CWMENW             PIC  X(002) VALUE "00".
              10 LB-CWMENW             PIC  X(255) VALUE SPACES.

       01  AREAS-DE-TRABALHO-2.
           05 MENSAGENS-DE-ERRO.
              10 PIC X(30) VALUE "Entre com os dados            ".
              10 PIC X(30) VALUE "Grupo n∆o cadastrado          ".
              10 PIC X(30) VALUE "Confirme exclus∆o             ".
              10 PIC X(30) VALUE "Grupo j† cadastrado           ".
              10           VALUE "Grupo contÇm 9999 usu†rio(s)  ".
                 11          PIC X(13).
                 11 USUARIOS PIC 9(04).
                 11          PIC X(13).
           05 FILLER REDEFINES MENSAGENS-DE-ERRO.
              10 MSG OCCURS 5 PIC X(30).
           05 MSG-7 PIC X(07) VALUE "P†gina".

       01  W-REG.
           05 W-CHAVE.
              10 W-PAGINA                PIC 9(003).
           05 W-LINHA OCCURS 8.
              10 W-PROGRAMA              PIC X(008).
              10 W-ACESSO                PIC X(001).
              10 W-ALTERACAO             PIC X(001).
              10 W-CONSULTA              PIC X(001).
              10 W-EXCLUSAO              PIC X(001).
              10 W-INCLUSAO              PIC X(001).

       COPY CWFUNC.
       COPY CWSEND.
       COPY CWBOXS.
       COPY CWBOXC.
       COPY CWUNIX.
       COPY CWCONF.

       SCREEN SECTION.

       01  CWMENAE AUTO.
           05 LINE 08 COLUMN 09 PIC X(022) USING CWCONF-NOME-GRUPO.

       01  CWMENAA.
           05 LINE 08 COLUMN 65 PIC X(009) USING ADM.

       01  CTAC-LIT-CWMENA.
           05 LINE 08 COLUMN 03 VALUE "Grupo".
           05 LINE 08 COLUMN 43 VALUE "Modo de administraá∆o".
           05 LINE 10 COLUMN 03 VALUE "Restriá‰es".
           05 LINE 12 COLUMN 05 VALUE "Programa".
           05 LINE 12 COLUMN 17 VALUE "Acesso".
           05 LINE 12 COLUMN 27 VALUE "Alteraá∆o".
           05 LINE 12 COLUMN 40 VALUE "Consulta".
           05 LINE 12 COLUMN 52 VALUE "Exclus∆o".
           05 LINE 12 COLUMN 64 VALUE "Inclus∆o".

       01  CTAC-VAR-CWMENA.
           05 LINE 08 COLUMN 65 PIC X(009) FROM ADM.
           05 LINE 13 COLUMN 05 PIC X(008) USING CWMENW-PROGRAMA(1).
           05 LINE 13 COLUMN 20 PIC X(001) USING CWCHECK-ACESSO(1).
           05 LINE 13 COLUMN 30 PIC X(001)
                 USING CWCHECK-ALTERACAO(1).
           05 LINE 13 COLUMN 43 PIC X(001) USING CWCHECK-CONSULTA(1).
           05 LINE 13 COLUMN 55 PIC X(001) USING CWCHECK-EXCLUSAO(1).
           05 LINE 13 COLUMN 67 PIC X(001) USING CWCHECK-INCLUSAO(1).
           05 LINE 14 COLUMN 05 PIC X(008) USING CWMENW-PROGRAMA(2).
           05 LINE 14 COLUMN 20 PIC X(001) USING CWCHECK-ACESSO(2).
           05 LINE 14 COLUMN 30 PIC X(001)
                 USING CWCHECK-ALTERACAO(2).
           05 LINE 14 COLUMN 43 PIC X(001) USING CWCHECK-CONSULTA(2).
           05 LINE 14 COLUMN 55 PIC X(001) USING CWCHECK-EXCLUSAO(2).
           05 LINE 14 COLUMN 67 PIC X(001) USING CWCHECK-INCLUSAO(2).
           05 LINE 15 COLUMN 05 PIC X(008) USING CWMENW-PROGRAMA(3).
           05 LINE 15 COLUMN 20 PIC X(001) USING CWCHECK-ACESSO(3).
           05 LINE 15 COLUMN 30 PIC X(001)
                 USING CWCHECK-ALTERACAO(3).
           05 LINE 15 COLUMN 43 PIC X(001) USING CWCHECK-CONSULTA(3).
           05 LINE 15 COLUMN 55 PIC X(001) USING CWCHECK-EXCLUSAO(3).
           05 LINE 15 COLUMN 67 PIC X(001) USING CWCHECK-INCLUSAO(3).
           05 LINE 16 COLUMN 05 PIC X(008) USING CWMENW-PROGRAMA(4).
           05 LINE 16 COLUMN 20 PIC X(001) USING CWCHECK-ACESSO(4).
           05 LINE 16 COLUMN 30 PIC X(001)
                 USING CWCHECK-ALTERACAO(4).
           05 LINE 16 COLUMN 43 PIC X(001) USING CWCHECK-CONSULTA(4).
           05 LINE 16 COLUMN 55 PIC X(001) USING CWCHECK-EXCLUSAO(4).
           05 LINE 16 COLUMN 67 PIC X(001) USING CWCHECK-INCLUSAO(4).
           05 LINE 17 COLUMN 05 PIC X(008) USING CWMENW-PROGRAMA(5).
           05 LINE 17 COLUMN 20 PIC X(001) USING CWCHECK-ACESSO(5).
           05 LINE 17 COLUMN 30 PIC X(001)
                 USING CWCHECK-ALTERACAO(5).
           05 LINE 17 COLUMN 43 PIC X(001) USING CWCHECK-CONSULTA(5).
           05 LINE 17 COLUMN 55 PIC X(001) USING CWCHECK-EXCLUSAO(5).
           05 LINE 17 COLUMN 67 PIC X(001) USING CWCHECK-INCLUSAO(5).
           05 LINE 18 COLUMN 05 PIC X(008) USING CWMENW-PROGRAMA(6).
           05 LINE 18 COLUMN 20 PIC X(001) USING CWCHECK-ACESSO(6).
           05 LINE 18 COLUMN 30 PIC X(001)
                 USING CWCHECK-ALTERACAO(6).
           05 LINE 18 COLUMN 43 PIC X(001) USING CWCHECK-CONSULTA(6).
           05 LINE 18 COLUMN 55 PIC X(001) USING CWCHECK-EXCLUSAO(6).
           05 LINE 18 COLUMN 67 PIC X(001) USING CWCHECK-INCLUSAO(6).
           05 LINE 19 COLUMN 05 PIC X(008) USING CWMENW-PROGRAMA(7).
           05 LINE 19 COLUMN 20 PIC X(001) USING CWCHECK-ACESSO(7).
           05 LINE 19 COLUMN 30 PIC X(001)
                 USING CWCHECK-ALTERACAO(7).
           05 LINE 19 COLUMN 43 PIC X(001) USING CWCHECK-CONSULTA(7).
           05 LINE 19 COLUMN 55 PIC X(001) USING CWCHECK-EXCLUSAO(7).
           05 LINE 19 COLUMN 67 PIC X(001) USING CWCHECK-INCLUSAO(7).
           05 LINE 20 COLUMN 05 PIC X(008) USING CWMENW-PROGRAMA(8).
           05 LINE 20 COLUMN 20 PIC X(001) USING CWCHECK-ACESSO(8).
           05 LINE 20 COLUMN 30 PIC X(001)
                 USING CWCHECK-ALTERACAO(8).
           05 LINE 20 COLUMN 43 PIC X(001) USING CWCHECK-CONSULTA(8).
           05 LINE 20 COLUMN 55 PIC X(001) USING CWCHECK-EXCLUSAO(8).
           05 LINE 20 COLUMN 67 PIC X(001) USING CWCHECK-INCLUSAO(8).

       PROCEDURE DIVISION.

       000-INICIO.

           PERFORM 800-INICIAIS      THRU 800-99-FIM
           PERFORM 100-PROCESSAMENTO THRU 100-99-FIM
                   UNTIL FINALIZAR
           PERFORM 900-FINAIS        THRU 900-99-FIM.

       000-99-FIM.

           IF   PARAR
                STOP RUN
           ELSE
                CANCEL "CWMEN4"
                GOBACK
           END-IF.

       100-PROCESSAMENTO.

           IF   FL-EXIT EQUAL 1
                EXEC COBOLware Object Drop END-EXEC
                MOVE SPACE TO FUNCAO
                CALL "CWIAEF" USING FUNCAO
                MOVE ZERO  TO FL-EXIT
           END-IF

           ON   1
                IF   NOT FINALIZAR
                     EXEC COBOLware BOXW
                          LINE         11
                          COLUMN        3
                          LINES         9
                          COLUMNS      69
                          COLOR-FRAME  71
                          COLOR-BORDER 71
                     END-EXEC
                     EXEC COBOLware BOXW POPUP END-EXEC
                     DISPLAY CTAC-LIT-CWMENA.

           MOVE "23" TO FS-CWCONF

           IF   NOT FINALIZAR
      *         MOVE SPACES TO CWCONF-NOME-GRUPO
                PERFORM 170-EXIBE-DADOS THRU 170-99-FIM
                PERFORM 140-LER-CWCONF  THRU 140-99-FIM
                        UNTIL FS-CWCONF EQUAL "00"
                        OR    FL-EXIT EQUAL 1
BOI   *         IF  (NOT INCLUSAO)
BOI   *         AND (FL-EXIT NOT EQUAL 1)
BOI   *            PERFORM 180-RESTRICOES THRU 180-99-FIM
BOI   *         END-IF
                IF  (INCLUSAO OR ALTERACAO)
                AND  FL-EXIT NOT EQUAL 1
                     IF   INCLUSAO
                          MOVE SPACES TO CWCONF-GRUPO-ID
                     END-IF
                     MOVE    SPACE              TO COMANDO
                     MOVE    MSG (1)            TO CWSEND-MSG
                     PERFORM 130-CRITICA      THRU 130-99-FIM
                             UNTIL CWSEND-MSG EQUAL SPACES
                             OR    ABORTAR
                     IF   ABORTAR
                     AND  INCLUSAO
                          SET CWSQLC-DELETE TO TRUE
                          CALL "CWCONF" USING CWSQLC
                                              CWCONF-REG
                                              FS-CWCONF
                                              KCO PCO
                          MOVE SPACES TO CWCONF-NOME-GRUPO
                          CALL "CWMEN4" USING TECLA CWCONF-GRUPO-ID
                                              CWCONF-NOME-GRUPO
                     END-IF
                ELSE
                     IF   FL-EXIT NOT EQUAL 1
                     AND  EXCLUSAO
                          CALL "CWMSGW" USING "230330" MSG (3)
                          PERFORM 160-CHECK-COMANDO THRU 160-99-FIM
                          CALL "CWMSGW" USING "230330" ESPACOS
                     END-IF
                END-IF
           END-IF

           MOVE "00" TO FS-CWCONF

           IF   EFETIVAR
           AND  NOT FINALIZAR
                MOVE CWCONF-GRUPO-ID TO SAVE-ID
                MOVE ADM             TO CWCONF-ADM
                IF   INCLUSAO
                     SET CWSQLC-REWRITE TO TRUE
                     CALL "CWCONF" USING CWSQLC
                                         CWCONF-REG
                                         FS-CWCONF
                                         KCO PCO
                ELSE
                     IF   EXCLUSAO
                          SET CWSQLC-DELETE TO TRUE
                          CALL "CWCONF" USING CWSQLC
                                              CWCONF-REG
                                              FS-CWCONF
                                              KCO PCO
                          MOVE SPACES TO CWCONF-NOME-GRUPO
                          CALL "CWMEN4" USING TECLA CWCONF-GRUPO-ID
                                              CWCONF-NOME-GRUPO
                          MOVE SPACES TO SAVE-ID CWCONF-GRUPO-ID
                     ELSE
                          SET CWSQLC-REWRITE TO TRUE
                          CALL "CWCONF" USING CWSQLC
                                              CWCONF-REG
                                              FS-CWCONF
                                              KCO PCO
                     END-IF
                END-IF
                PERFORM 200-SALVA-CWMENW THRU 200-99-FIM
                MOVE SAVE-ID TO CWCONF-GRUPO-ID
           END-IF

           IF   FS-CWCONF EQUAL "GU"
                MOVE "00" TO FS-CWCONF
           END-IF.

       100-99-FIM. EXIT.

       130-CRITICA.

           MOVE SPACES TO CWSEND-MSG
           PERFORM TEST AFTER UNTIL (NOT F2) AND (NOT F3)
                   IF   CWCONF-ADM = "I"
                        MOVE "I" TO ADM
                   ELSE
                        MOVE "E" TO ADM
                   END-IF
                   PERFORM ADM-LIST
                   EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                             LINE footline COLUMN 03 WIDTH 16
                             CAPTION " f2-~Impressoras "
                             KEY F2 TAB-OFF
                   END-EXEC
                   EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                             LINE footline COLUMN 20 WIDTH 13
                             CAPTION " f3-~Estaá‰es "
                             KEY F3 TAB-OFF
                   END-EXEC
                   EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                             LINE footline COLUMN 34 WIDTH 13
                             CAPTION " f4-~Continua "
                             KEY F4 TAB-OFF
                   END-EXEC
                   ACCEPT CWMENAA
                   ACCEPT TECLA FROM ESCAPE KEY
                   EXEC COBOLware OBJECT DROP END-EXEC
                   IF  F2 OR F3
                       CALL "CWMEN4" USING TECLA CWCONF-GRUPO-ID
                                           CWCONF-NOME-GRUPO
                       SET CWSQLC-REWRITE TO TRUE
                       CALL "CWCONF" USING CWSQLC
                                           CWCONF-REG
                                           FS-CWCONF
                                           KCO PCO
                   END-IF
           END-PERFORM
           IF   ESC
                SET ABORTAR TO TRUE
                MOVE 1   TO FL-EXIT
                GO TO 130-99-FIM
           END-IF
           IF   ADM = "I"
                MOVE "I"  TO CWCONF-ADM
           ELSE
                MOVE "E"  TO CWCONF-ADM
           END-IF
           MOVE SPACES TO CWSEND-MSG
           MOVE 0      TO TECLA
           SET ALTERAR-R TO TRUE
           PERFORM 180-RESTRICOES THRU 180-99-FIM

           IF   ESC
                MOVE 1   TO FL-EXIT
           ELSE
                PERFORM 160-CHECK-COMANDO THRU 160-99-FIM
           END-IF.

       130-99-FIM.  EXIT.

       ADM-LIST.

           EXEC COBOLware OBJECT COMBO-BOX
                LINE 8 COLUMN 65 Caption "Modo"
                WIDTH 9 HEIGHT 2
                       STRING-1(1) "E"
                       STRING-1(2) "I"
                       STRING-2(1) "Bloquear"
                       STRING-2(2) "Liberar"
                       STRING-2-LENGTH 9
                       OPTION ADM NOEDIT
           END-EXEC.

       FIM-ADM. EXIT.

       140-LER-CWCONF.

           PERFORM ADM-LIST
           EXEC COBOLware OBJECT DROP END-EXEC
           CALL "CWSEND" USING PARAMETROS-CWSEND
           MOVE SPACES      TO CWSEND-MSG
           IF  NOT INCLUSAO
               EXEC COBOLware OBJECT COMBO-BOX
                    CAPTION "Grupos"
                    LINE 08 COLUMN 09 HEIGHT 14 WIDTH 22
                    PROGRAM "CWMENF"
                    OPTION CWCONF-NOME-GRUPO
                    ORDER 1 RETURN 1
                    STRING-1-LENGTH 22
                    WORK-AREA "GU                              1"

               END-EXEC
               COPY CWPGON REPLACING 23 BY footline.
           END-IF

           COPY CWESCP REPLACING 23 BY footline.

           MOVE 0      TO TECLA
                          FIM
           MOVE SPACES TO CWCONF-PROG-GRUPO

           PERFORM TEST AFTER
                   UNTIL(NOT (PAGE-UP   AND PAGE-UP-OFF))
                     AND(NOT (PAGE-DOWN AND PAGE-DOWN-OFF))
                   ACCEPT CWMENAE
                   ACCEPT TECLA FROM ESCAPE KEY
           END-PERFORM
           EXEC COBOLware OBJECT DROP END-EXEC

           IF   ESC
                MOVE 1 TO FL-EXIT
                GO TO 140-99-FIM
           END-IF

           MOVE "GU" TO CWCONF-TIPO

           CALL "CWSEND" USING PARAMETROS-CWSEND
           INSPECT CWCONF-NOME-GRUPO CONVERTING MINUSCULAS TO MAIUSCULAS

           IF   CWCONF-NOME-GRUPO EQUAL SPACES
           AND  NOT (PAGE-UP OR PAGE-DOWN)
                MOVE "44" TO FS-CWCONF
           ELSE
           IF   FL-EXIT NOT EQUAL 1
                IF   (PAGE-UP OR PAGE-DOWN)
                AND  NOT INCLUSAO
                AND  OK EQUAL ZERO
                     MOVE 1    TO OK
                     PERFORM TEST AFTER UNTIL FS-CWCONF NOT = "9D"
                                       AND (CWCONF-CHAVE <> SALVA-CHAVE)
                             IF PAGE-DOWN
                                SET CWSQLC-NOT-LESS TO TRUE
                                MOVE LOW-VALUES TO CWCONF-CHAVE
                             END-IF
                             IF PAGE-UP
                                SET CWSQLC-NOT-GREATER TO TRUE
                                MOVE HIGH-VALUES TO CWCONF-CHAVE
                             END-IF
                             SET CWSQLC-START TO TRUE
                             MOVE "GU" TO CWCONF-CHAVE(1:2)
                             CALL "CWCONF" USING CWSQLC
                                                 CWCONF-REG
                                                 FS-CWCONF
                                                 KCO PCO
                             IF   FS-CWCONF = "9D"
                                  CALL "CWCONF" USING "ISAM"
                             END-IF
                     END-PERFORM
                END-IF
                IF   (PAGE-UP OR PAGE-DOWN)
                AND  NOT INCLUSAO
                     PERFORM TEST AFTER
                             UNTIL   (FS-CWCONF NOT EQUAL "00")
                                OR (CWCONF-TIPO NOT EQUAL "GU")
                                OR (CWCONF-TIPO EQUAL "GU"
                                AND CWCONF-PROG-GRUPO = SPACES
                                AND FS-CWCONF = "00")
                             EVALUATE TRUE
                             WHEN PAGE-DOWN
                                  PERFORM TEST AFTER
                                          UNTIL FS-CWCONF NOT = "9D"
                                       AND (CWCONF-CHAVE <> SALVA-CHAVE)
                                     SET CWSQLC-READ TO TRUE
                                     SET CWSQLC-NEXT TO TRUE
                                     CALL "CWCONF" USING CWSQLC
                                                         CWCONF-REG
                                                         FS-CWCONF
                                                         KCO PCO
                                     IF   FS-CWCONF = "9D"
                                          CALL "CWCONF" USING "ISAM"
                                     ELSE
                                      IF  CWCONF-TIPO EQUAL "GU"
                                      AND CWCONF-PROG-GRUPO NOT = SPACES
                                          MOVE "9D" TO FS-CWCONF
                                      END-IF
                                      PERFORM 400-CHECK-GOOD
                                         THRU 400-99-FIM
                                     END-IF
                                     IF FS-CWCONF < '10'
                                     AND CWCONF-TIPO EQUAL "GU"
                                     AND PAGE-UP-OFF
                                         SET PAGE-UP-ON TO TRUE
                                     END-IF
                                  END-PERFORM
                             WHEN PAGE-UP
                                  PERFORM TEST AFTER
                                          UNTIL FS-CWCONF NOT = "9D"
                                       AND (CWCONF-CHAVE <> SALVA-CHAVE)
                                     SET CWSQLC-READ TO TRUE
                                     SET CWSQLC-PREVIOUS TO TRUE
                                     CALL "CWCONF" USING CWSQLC
                                                         CWCONF-REG
                                                         FS-CWCONF
                                                         KCO PCO
                                     IF   FS-CWCONF = "9D"
                                          CALL "CWCONF" USING "ISAM"
                                      IF  CWCONF-TIPO EQUAL "GU"
                                      AND CWCONF-PROG-GRUPO NOT = SPACES
                                          MOVE "9D" TO FS-CWCONF
                                      END-IF
                                      PERFORM 400-CHECK-GOOD
                                         THRU 400-99-FIM
                                     END-IF
                                     IF FS-CWCONF < '10'
                                     AND CWCONF-TIPO EQUAL "GU"
                                     AND PAGE-DOWN-OFF
                                         SET PAGE-DOWN-ON TO TRUE
                                     END-IF
                                  END-PERFORM
                             END-EVALUATE
                     END-PERFORM
                     IF   CWCONF-TIPO EQUAL "GU"
                     AND  FS-CWCONF < "09"
                          PERFORM 170-EXIBE-DADOS THRU 170-99-FIM
                          SET CONSULTA-R TO TRUE
                          PERFORM 180-RESTRICOES THRU 180-99-FIM
mal                           SET ALTERAR-R TO TRUE
                          GO TO 140-LER-CWCONF
                     ELSE
                          IF FS-CWCONF = "10"
                          OR (CWCONF-TIPO NOT EQUAL "GU")
                             MOVE 1 TO FIM
                          END-IF
                          MOVE ZERO TO OK
                          MOVE "GU" TO CWCONF-REGGU
                          MOVE "44" TO FS-CWCONF
                     END-IF
                ELSE
                     PERFORM TEST AFTER UNTIL FS-CWCONF NOT = "9D"
                       SET CWSQLC-UNLOCK TO TRUE
                       CALL "CWCONF" USING CWSQLC
                                           CWCONF-REG
                                           FS-CWCONF
                                           KCO PCO
                       SET CWSQLC-READ TO TRUE
                       SET CWSQLC-EQUAL TO TRUE
                       SET CWSQLC-LOCK TO TRUE
                       CALL "CWCONF" USING CWSQLC
                                           CWCONF-REG
                                           FS-CWCONF
                                           KCO PCO
                       IF   FS-CWCONF = "9D"
                            CALL "CWCONF" USING "ISAM"
                       END-IF
                     END-PERFORM
                END-IF
                IF   FS-CWCONF < "09"
                     PERFORM 170-EXIBE-DADOS THRU 170-99-FIM
                     IF   INCLUSAO
                          MOVE MSG (4) TO CWSEND-MSG
                          MOVE "44" TO FS-CWCONF
                     END-IF
                ELSE
                     SET READY-OFF TO TRUE
                     IF   NOT INCLUSAO
                          MOVE MSG (2) TO CWSEND-MSG
2807                      MOVE SALVA-CHAVE TO CWCONF-CHAVE
                          SET CWSQLC-READ  TO TRUE
                          SET CWSQLC-EQUAL TO TRUE
                          CALL "CWCONF" USING CWSQLC
                                              CWCONF-REG
                                              FS-CWCONF
                                              KCO PCO
                          IF  FS-CWCONF < '10'
                              SET READY-ON TO TRUE
                          END-IF
                          MOVE '10' TO FS-CWCONF
                     ELSE
                          MOVE SPACES TO CWCONF-PROG-GRUPO
                          IF   ADM = "I"
                               MOVE "I" TO CWCONF-ADM
                          ELSE
                               MOVE "E" TO CWCONF-ADM
                          END-IF
                          SET CWSQLC-WRITE TO TRUE
                          CALL "CWCONF" USING CWSQLC
                                              CWCONF-REG
                                              FS-CWCONF
                                              KCO PCO
                          MOVE "00" TO FS-CWCONF
                          PERFORM TEST AFTER
                            UNTIL FS-CWCONF NOT = "9D"
                            SET CWSQLC-UNLOCK TO TRUE
                            CALL "CWCONF" USING CWSQLC
                                                CWCONF-REG
                                                FS-CWCONF
                                                KCO PCO
                            SET CWSQLC-READ TO TRUE
                            SET CWSQLC-EQUAL TO TRUE
                            SET CWSQLC-LOCK TO TRUE
                            CALL "CWCONF" USING CWSQLC
                                                CWCONF-REG
                                                FS-CWCONF
                                                KCO PCO
                            IF   FS-CWCONF = "9D"
                                 CALL "CWCONF" USING "ISAM"
                            END-IF
                          END-PERFORM
                          PERFORM 170-EXIBE-DADOS THRU 170-99-FIM
                     END-IF
                END-IF
           ELSE
                MOVE ZERO   TO FS-CWCONF
                MOVE SPACES TO COMANDO
                               FUNCAO
           END-IF.

       140-99-FIM. EXIT.

       160-CHECK-COMANDO.

           IF   EXCLUSAO
                MOVE CWCONF-REG TO SALVA-CHAVE
                MOVE "PS"       TO CWCONF-CHAVE
                MOVE 0          TO USUARIOS
                PERFORM TEST AFTER UNTIL FS-CWCONF NOT = "9D"
                        SET CWSQLC-START TO TRUE
                        SET CWSQLC-NOT-LESS TO TRUE
                        CALL "CWCONF" USING CWSQLC
                                            CWCONF-REG
                                            FS-CWCONF
                                            KCO PCO
                        IF   FS-CWCONF = "9D"
                             CALL "CWCONF" USING "ISAM"
                        END-IF
                END-PERFORM
                PERFORM TEST AFTER UNTIL FS-CWCONF > "09"
                          OR CWCONF-TIPO NOT = "PS"
                        PERFORM TEST AFTER UNTIL FS-CWCONF NOT = "9D"
                                SET CWSQLC-READ TO TRUE
                                SET CWSQLC-NEXT TO TRUE
                                CALL "CWCONF" USING CWSQLC
                                                    CWCONF-REG
                                                    FS-CWCONF
                                                    KCO PCO
                                IF   FS-CWCONF = "9D"
                                     CALL "CWCONF" USING "ISAM"
                                END-IF
                                PERFORM 400-CHECK-GOOD
                                THRU 400-99-FIM
                        END-PERFORM
                        IF   FS-CWCONF < "10"
                        AND  CWCONF-TIPO = "PS"
                        AND  CWCONF-GRUPO = GRUPO
                             ADD 1 TO USUARIOS
                        END-IF
                END-PERFORM
                MOVE SALVA-CHAVE TO CWCONF-REG
                PERFORM TEST AFTER UNTIL FS-CWCONF > "09"
                          OR CWCONF-TIPO NOT = "PS"
                        PERFORM TEST AFTER UNTIL FS-CWCONF NOT = "9D"
                                SET CWSQLC-READ TO TRUE
                                SET CWSQLC-EQUAL TO TRUE
                                CALL "CWCONF" USING CWSQLC
                                                    CWCONF-REG
                                                    FS-CWCONF
                                                    KCO PCO
                                IF   FS-CWCONF = "9D"
                                     CALL "CWCONF" USING "ISAM"
                                END-IF
                        END-PERFORM
                END-PERFORM
           END-IF

           IF   EXCLUSAO
           AND (USUARIOS NOT = 0)
                EXEC COBOLware SEND
                     MSG MSG(5)
                END-EXEC
                SET ABORTAR TO TRUE
           ELSE
                COPY CWEFAB.

           MOVE SPACES TO CWSEND-MSG.

       160-99-FIM. EXIT.

       170-EXIBE-DADOS.

           IF   FS-CWCONF < "09"
           AND  CWCONF-PROG-GRUPO = SPACES
                MOVE CWCONF-ADM TO ADM
           END-IF
           PERFORM ADM-LIST
           DISPLAY CWMENAE CWMENAA.
           EXEC COBOLware OBJECT DROP END-EXEC.

       170-99-FIM. EXIT.

       180-RESTRICOES.

           CLOSE CWMENW DELETE FILE CWMENW
           OPEN I-O CWMENW
           MOVE CWCONF-CHAVE TO SALVA-CHAVE
           MOVE 1            TO I PAGINA ULTIMA
           MOVE SPACES       TO CWMENW-REG
                                POP
           STRING MSG-7 PAGINA "/" ULTIMA DELIMITED BY SIZE INTO POP
           CALL "CWMSGW" USING "106014" POP
           IF   GRUPO = SPACES
                GO TO 180-99-FIM
           END-IF
           PERFORM TEST AFTER UNTIL FS-CWCONF > "09"
                   PERFORM TEST AFTER UNTIL FS-CWCONF NOT = "9D"
                           SET CWSQLC-READ TO TRUE
                           SET CWSQLC-NEXT TO TRUE
                           CALL "CWCONF" USING CWSQLC
                                               CWCONF-REG
                                               FS-CWCONF
                                               KCO PCO
                           IF   FS-CWCONF = "9D"
                                CALL "CWCONF" USING "ISAM"
                           END-IF
                           PERFORM 400-CHECK-GOOD THRU 400-99-FIM
                   END-PERFORM
                   IF   CWCONF-TIPO NOT = "GU"
                   OR   CWCONF-NOME-GRUPO NOT = GRUPO
                        MOVE "10" TO FS-CWCONF
                   ELSE
                        IF   CWCONF-PROG-GRUPO NOT = SPACES
                        AND  FS-CWCONF < "10"
                             MOVE CWCONF-REGGU(25: ) TO CWMENW-LINHA (I)
                             INSPECT CWMENW-LINHA (I) (9: )
                                     CONVERTING X"2AFE20" TO "110"
                             ADD  1                  TO I
                             IF   I = 9
                                  MOVE ULTIMA TO CWMENW-PAGINA
                                  WRITE CWMENW-REG
                                  ADD  1      TO ULTIMA
                                  MOVE 1      TO I
                                  MOVE SPACES TO CWMENW-REG
                             END-IF
                        END-IF
                   END-IF
           END-PERFORM
           IF  I NOT = 1
               MOVE ULTIMA TO CWMENW-PAGINA
               WRITE CWMENW-REG
           ELSE
               SUBTRACT 1 FROM ULTIMA
           END-IF
           CALL "CWMSGW" USING "107103" ULTIMA
           MOVE SALVA-CHAVE TO CWCONF-CHAVE
           PERFORM TEST AFTER UNTIL FS-CWCONF NOT = "9D"
                   SET CWSQLC-READ TO TRUE
                   SET CWSQLC-EQUAL TO TRUE
                   CALL "CWCONF" USING CWSQLC
                                       CWCONF-REG
                                       FS-CWCONF
                                       KCO PCO
                   IF   FS-CWCONF = "9D"
                        CALL "CWCONF" USING "ISAM"
                   END-IF
           END-PERFORM
           MOVE SPACES TO CWMENW-REG

           MOVE 1 TO PAGINA
           MOVE 0 TO TECLA
           PERFORM TEST AFTER UNTIL F2 OR ESC OR CONSULTA-R
                   MOVE PAGINA TO CWMENW-PAGINA
                   READ CWMENW
                        INVALID KEY
                             IF NOT CONSULTA-R
                                MOVE SPACES TO CWMENW-REG
                                MOVE PAGINA TO CWMENW-PAGINA
                                               ULTIMA
                                WRITE CWMENW-REG
                                MOVE PAGINA TO CWMENW-PAGINA
                                READ CWMENW
                                     INVALID KEY CONTINUE END-READ
                             ELSE
                                IF   PAGINA > 1
                                     SUBTRACT 1 FROM PAGINA
                                     MOVE PAGINA TO CWMENW-PAGINA
                                     READ CWMENW
                                     INVALID KEY CONTINUE END-READ
                                END-IF
                             END-IF
                   END-READ
                   MOVE SPACES TO POP
                   STRING MSG-7 PAGINA "/" ULTIMA
                          DELIMITED BY SIZE INTO POP
                   CALL "CWMSGW" USING "106014" POP
                   IF  CONSULTA-R
                       PERFORM ADM-LIST
                       DISPLAY CTAC-VAR-CWMENA
                       EXEC COBOLware OBJECT DROP END-EXEC
                   ELSE
                       PERFORM OUTROS-OBJETOS
                       PERFORM TEST AFTER UNTIL F2
                                  OR ESC
                                  OR PAGE-UP
                                  OR PAGE-DOWN
                                  OR CONTROL-PAGE-UP
                                  OR CONTROL-PAGE-DOWN
                               ACCEPT CTAC-VAR-CWMENA
                               ACCEPT TECLA FROM ESCAPE KEY
                               IF  F3
                                   PERFORM 171-LISTA THRU 171-99-FIM
                               END-IF
                       END-PERFORM
                       IF ESC
                         MOVE 1   TO FL-EXIT
                       END-IF
                       EXEC COBOLware OBJECT DROP END-EXEC
                       REWRITE CWMENW-REG
                       EVALUATE TRUE
                          WHEN PAGE-UP AND PAGINA > 1
                               SUBTRACT 1 FROM PAGINA
                          WHEN PAGE-DOWN
                               ADD 1 TO PAGINA
                          WHEN CONTROL-PAGE-UP
                               MOVE 1 TO PAGINA
                          WHEN CONTROL-PAGE-DOWN
                               MOVE ULTIMA TO PAGINA
                       END-EVALUATE
                   END-IF
           END-PERFORM.

       180-99-FIM. EXIT.

       OUTROS-OBJETOS.

           COPY CWESCP REPLACING 23 BY footline.
           PERFORM ADM-LIST
           EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                LINE footline COLUMN 03 CAPTION " f2-~Aceita " WIDTH 11
                KEY F2 TAB-OFF
           END-EXEC
           EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                LINE footline COLUMN 14 CAPTION " f3-~Lista "  WIDTH 10
                KEY F3 TAB-OFF
           END-EXEC
           EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                LINE footline COLUMN 24 CAPTION " pgup "      WIDTH 06
                KEY PAGE-UP TAB-OFF
           END-EXEC
           EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                LINE footline COLUMN 30 CAPTION " pgdn "      WIDTH 06
                KEY PAGE-DOWN TAB-OFF
           END-EXEC
           EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                LINE footline COLUMN 36 CAPTION " c-pg~Up "   WIDTH 08
                KEY CONTROL-PAGE-UP TAB-OFF
           END-EXEC
           EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                LINE footline COLUMN 45 CAPTION " ~c-pg~Dn "  WIDTH 08
                KEY CONTROL-PAGE-DOWN TAB-OFF
           END-EXEC.

       FIM-OUTROS-OBJETOS. EXIT.

       171-LISTA.

           IF   CWBOXC-VERTICAL-LENGTH NOT = 0
                PERFORM VARYING I FROM 1 BY 1 UNTIL I > 8
                        IF CWMENW-PROGRAMA (I) = SPACES
                           EXIT PERFORM
                        END-IF
                END-PERFORM
                IF  I > 8
                    EXEC COBOLware SEND
                         MSG
                         "Um nome de programa precisa estar em branco"
                    END-EXEC
                    GO TO 171-99-FIM
                END-IF
                SET  CWBOXC-SHOW TO TRUE
                MOVE SPACES TO CWBOXC-OPTION
                CALL "CWBOXC" USING PARAMETROS-CWBOXC
                IF   CWBOXC-OPTION NOT = SPACES
                     IF  MENULIST = "ON"
                         MOVE CWBOXC-OPTION(36:) TO CWMENW-PROGRAMA (I)
                     ELSE
                         MOVE CWBOXC-OPTION TO CWMENW-PROGRAMA (I)
                     END-IF
                END-IF
           ELSE
                MOVE SPACES      TO CWSEND-SCREENS
                MOVE "Tabela de programas vazia"
                                 TO CWSEND-MSG
                CALL "CWSEND" USING PARAMETROS-CWSEND
           END-IF.

       171-99-FIM. EXIT.

       190-CONDENSA.

           MOVE CWMENW-REG TO W-REG
           MOVE SPACES     TO CWMENW-REG
           MOVE PAGINA     TO CWMENW-PAGINA
           MOVE 0          TO Z
           PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > 8
                   IF   W-PROGRAMA (Y) NOT = SPACE
                        ADD 1 TO Z
                        MOVE W-PROGRAMA  (Y) TO CWMENW-PROGRAMA   (Z)
                        MOVE W-ACESSO    (Y) TO CWCHECK-ACESSO    (Z)
                        MOVE W-ALTERACAO (Y) TO CWCHECK-ALTERACAO (Z)
                        MOVE W-CONSULTA  (Y) TO CWCHECK-CONSULTA  (Z)
                        MOVE W-EXCLUSAO  (Y) TO CWCHECK-EXCLUSAO  (Z)
                        MOVE W-INCLUSAO  (Y) TO CWCHECK-INCLUSAO  (Z)
                   END-IF
           END-PERFORM.

       190-99-FIM. EXIT.

       200-SALVA-CWMENW.

           MOVE CWCONF-CHAVE TO SALVA-CHAVE
           PERFORM TEST AFTER UNTIL FS-CWCONF > "09"
                   PERFORM TEST AFTER UNTIL FS-CWCONF NOT = "9D"
                           SET CWSQLC-READ TO TRUE
                           SET CWSQLC-NEXT TO TRUE
                           CALL "CWCONF" USING CWSQLC
                                               CWCONF-REG
                                               FS-CWCONF
                                               KCO PCO
                           IF   FS-CWCONF = "9D"
                                CALL "CWCONF" USING "ISAM"
                           END-IF
                           PERFORM 400-CHECK-GOOD THRU 400-99-FIM
                   END-PERFORM
                   IF   CWCONF-TIPO NOT = "GU"
                   OR   CWCONF-NOME-GRUPO NOT = GRUPO
                        MOVE "10" TO FS-CWCONF
                   ELSE
                        IF   CWCONF-PROG-GRUPO NOT = SPACES
                        AND  FS-CWCONF < "10"
                             SET CWSQLC-DELETE TO TRUE
                             CALL "CWCONF" USING CWSQLC
                                                 CWCONF-REG
                                                 FS-CWCONF
                                                 KCO PCO
                        END-IF
                   END-IF
           END-PERFORM

           IF   NOT EXCLUSAO
                PERFORM TEST AFTER VARYING PAGINA FROM 1 BY 1
                             UNTIL FS-CWMENW > "09"
                   MOVE PAGINA TO CWMENW-PAGINA
                   READ CWMENW
                   IF   FS-CWMENW < "10"
                        PERFORM VARYING I FROM 1 BY 1 UNTIL I > 8
                                IF   CWMENW-PROGRAMA (I) NOT = SPACES
                                     MOVE SALVA-CHAVE TO CWCONF-CHAVE
                                     MOVE CWMENW-LINHA (I)
                                       TO CWCONF-REGGU(25: )
HDBAD                                INSPECT CWCONF-REGGU(33: )
HDBAD                                        CONVERTING "0" TO SPACE
                                     IF   CWCONF-NOME-GRUPO NOT = SPACE
                                          MOVE SPACES TO CWCONF-ADM
                                          SET CWSQLC-WRITE TO TRUE
                                          CALL "CWCONF" USING CWSQLC
                                                              CWCONF-REG
                                                              FS-CWCONF
                                                              KCO PCO
                                     END-IF
                                END-IF
                        END-PERFORM
                   END-IF
                END-PERFORM
           END-IF

           MOVE SALVA-CHAVE TO CWCONF-CHAVE
           PERFORM TEST AFTER UNTIL FS-CWCONF NOT = "9D"
                   SET CWSQLC-READ TO TRUE
                   SET CWSQLC-EQUAL TO TRUE
                   CALL "CWCONF" USING CWSQLC
                                       CWCONF-REG
                                       FS-CWCONF
                                       KCO PCO
                   IF   FS-CWCONF = "9D"
                        CALL "CWCONF" USING "ISAM"
                   END-IF
           END-PERFORM.

       200-99-FIM. EXIT.

       400-CHECK-GOOD.

           IF   FS-CWCONF < "10"
           AND  CWCONF-NOME-GRUPO (1: 7) = "Acesso "
                MOVE "9D" TO FS-CWCONF
           END-IF.

       400-99-FIM. EXIT.

       800-INICIAIS.

           DISPLAY "CWFOOTLINE"  UPON ENVIRONMENT-NAME
           ACCEPT   footline-t   FROM ENVIRONMENT-VALUE
           CALL "CWGETU" USING NOME TASK PROGRAMA "?"
           CALL "CWUNIX" USING PARAMETROS-CWUNIX
           DISPLAY "CWMENULIST"   UPON ENVIRONMENT-NAME
           ACCEPT  MENULIST       FROM ENVIRONMENT-VALUE
           INSPECT MENULIST CONVERTING MINUSCULAS TO MAIUSCULAS

           DISPLAY "TEMP"         UPON ENVIRONMENT-NAME
           ACCEPT  TMP            FROM ENVIRONMENT-VALUE
           IF   TMP = SPACES
                DISPLAY "TMP"     UPON ENVIRONMENT-NAME
                ACCEPT  TMP       FROM ENVIRONMENT-VALUE
           END-IF

           MOVE TASK (2: 5)  TO TMP-LB (4: 5)
           IF   TMP NOT = SPACE
                MOVE SPACES TO LB-CWMENW
                STRING TMP    DELIMITED BY SPACE
                       "/"    DELIMITED BY SIZE
                       TMP-LB DELIMITED BY SPACE
                  INTO LB-CWMENW
           ELSE
                MOVE TMP-LB TO LB-CWMENW
           END-IF

           SET CWSQLC-UPDATE TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           IF   FS-CWCONF NOT EQUAL "00"
                CALL "CWCONF" USING "ISAM"
                EXIT PROGRAM
           END-IF

           SET  CWBOXC-LOAD  TO TRUE
           MOVE "Programas:" TO CWBOXC-TITLE
           MOVE 11           TO CWBOXC-LINE
           MOVE 14           TO CWBOXC-COLUMN
           MOVE  0           TO CWBOXC-VERTICAL-LENGTH
           MOVE 45           TO CWBOXC-HORIZONTAL-LENGTH
           MOVE 1            TO CWBOXC-ORDER
           MOVE 2            TO CWBOXC-RETURN
           MOVE 35           TO CWBOXC-STRING-1-LENGTH
           MOVE 8            TO CWBOXC-STRING-2-LENGTH
           IF  MENULIST = "ON"
               MOVE 0        TO CWBOXC-STRING-1-LENGTH
               MOVE 43       TO CWBOXC-STRING-2-LENGTH
           END-IF
           MOVE 062          TO CWBOXC-COLOR-FRAME
                                CWBOXC-COLOR-BORDER

           PERFORM TEST AFTER UNTIL FS-CWCONF > "09"
                   SET CWSQLC-READ TO TRUE
                   SET CWSQLC-NEXT TO TRUE
                   SET CWSQLC-IGNORE-LOCK TO TRUE
                   CALL "CWCONF" USING CWSQLC
                                       CWCONF-REG
                                       FS-CWCONF
                                       KCO PCO
                   IF   FS-CWCONF < "10"
                   AND (CWCONF-TIPO = "99" OR "SM")
                        PERFORM 810-CRIA-LISTA THRU 810-99-FIM
                                VARYING OC
                                   FROM 1
                                     BY 1 UNTIL OC > 26
                   END-IF
           END-PERFORM.

       800-99-FIM. EXIT.

       810-CRIA-LISTA.

           MOVE CWCONF-PROG  (OC) TO PROGRAMA
           IF   CWCONF-PROG  (OC) = SPACES
                GO TO 810-99-FIM
           END-IF
           CALL "CWCODE" USING "D" CWCONF-SIZE-P-99  (OC)
                                   CWCONF-FATOR-P-99 (OC)
                                   PROGRAMA
           INSPECT PROGRAMA CONVERTING MINUSCULAS TO MAIUSCULAS

           IF   NOT MODULO-CWMENU
                MOVE CWCONF-NM-OPCAO(OC) TO TESTE-OPCAO
                IF   TESTE-OPCAO = "/"
                     MOVE CWCONF-NM-OPCAO(OC) (3: ) TO CWBOXC-STRING-1
                 ELSE
                      MOVE CWCONF-NM-OPCAO(OC)       TO CWBOXC-STRING-1
                 END-IF
                PERFORM 820-AJUSTA-STRING THRU 820-99-FIM
                MOVE PROGRAMA            TO CWBOXC-STRING-2 TESTE-OPCAO
                IF   PROGRAMA = "CWBOXS" OR "GRBOXS"
                     MOVE ":"             TO CWBOXC-STRING-2
                     MOVE CWCONF-HELP(OC) TO CWBOXC-STRING-2 (2: )
                ELSE
                     IF   PROGRAMA = "CWREL2"
                     AND (CWCONF-HELP(OC) NOT = SPACES)
                         MOVE "*"             TO CWBOXC-STRING-2
                         MOVE CWCONF-HELP(OC) TO CWBOXC-STRING-2 (2: )
                         INSPECT CWBOXC-STRING-2 (2: )
                                 CONVERTING MINUSCULAS TO MAIUSCULAS
                     END-IF
                END-IF
                IF  MENULIST = "ON"
                    IF CWCONF-TIPO = '99'
                       ADD 1 TO ITEM-99
                       MOVE ITEM-99 TO ITEM
                    ELSE
                       MOVE CWCONF-PAGINA(1:4) TO ITEM(1:4)
                       MOVE OC(1:2)            TO ITEM(5:2)
                    END-IF
                    IF  CWBOXC-STRING-2(1:1) = ":"
                        MOVE 5 TO II
                        MOVE 0 TO ITEM
                        PERFORM VARYING YY
                                   FROM LENGTH OF CWBOXC-STRING-2
                                      BY -1 UNTIL YY = 0
                                      OR II = 1
                                 IF CWBOXC-STRING-2(YY: 1) NUMERIC
                                    SUBTRACT 1 FROM II
                                    MOVE CWBOXC-STRING-2(YY: 1)
                                      TO ITEM(II:1)
                                 END-IF
                        END-PERFORM
                    END-IF
                    MOVE CWBOXC-STRING-1 TO OUTRO
                    MOVE CWBOXC-STRING-2 TO OUTRO (36:)
                    MOVE OUTRO           TO CWBOXC-STRING-2
                    MOVE ITEM            TO CWBOXC-STRING-1
                END-IF
                CALL "CWBOXC"         USING PARAMETROS-CWBOXC
                IF   CWBOXC-VERTICAL-LENGTH < 10
                     ADD 1 TO CWBOXC-VERTICAL-LENGTH
                END-IF
           END-IF.

       810-99-FIM. EXIT.

       820-AJUSTA-STRING.

           MOVE SPACES TO NEW-ITEM
           MOVE 0      TO K2
           MOVE 35     TO Y
           PERFORM VARYING K FROM 1 BY 1 UNTIL K > Y
                   IF   CWBOXC-STRING-1 (K: 1) = X"7E"
                        ADD 1 TO K
                   END-IF
                   ADD 1 TO K2
                   MOVE CWBOXC-STRING-1 (K: 1) TO NEW-ITEM (K2: 1)
           END-PERFORM
           MOVE NEW-ITEM    TO CWBOXC-STRING-1.

       820-99-FIM. EXIT.

       900-FINAIS.

           IF   CWBOXC-VERTICAL-LENGTH NOT = 0
                SET  CWBOXC-DELETE TO TRUE
                CALL "CWBOXC"   USING PARAMETROS-CWBOXC
           END-IF
           EXEC COBOLware OBJECT DROP END-EXEC
           SET CWSQLC-CLOSE TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           DELETE FILE CWMENW.

       900-99-FIM. EXIT.

       END PROGRAM CWMENZ.

