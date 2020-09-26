       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWMEN8.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  09/05/1990.
       SECURITY.      *************************************************
                      *                                               *
                      *   Manutencao de parametros genericos          *
                      *                                               *
                      *   Modulo de manutencao de USUARIOS            *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1.
           05 footline-t.
              10 footline              PIC  9(002) VALUE 23.
           05 GU-CLASSE                            VALUE "GU".
              10                       PIC  X(032).
              10 CLASSE-GU             PIC  9.
           05 OBS                      PIC  X(035) VALUE SPACES.
           05 SAVE-USER                PIC X(2008) VALUE SPACES.
           05 ECHOPASS                 PIC  X(003) VALUE SPACES.
           05 SENHA-DG                 PIC  X(030) VALUE SPACES.
           05 FIM                      PIC  9(001) VALUE ZERO.
           05 BLOQUEADO                PIC  X(001) VALUE SPACE.
           05 HOJE                     PIC  9(008) VALUE 0.
           05 EXPIRE                   PIC  9(003) VALUE 0.
           05 SEQ                      PIC  9(003) VALUE 0.
           05 SAVES                    PIC  9(003) VALUE 0.
           05 CODED-SMTP               PIC  9(001) VALUE 0.
           05 CWCHECK-1                PIC  X(001) VALUE "0".
           05 CWCHECK-2                PIC  X(001) VALUE "0".
           05 CWCHECK-3                PIC  X(001) VALUE "0".
           05 CWCHECK-4                PIC  X(001) VALUE "0".
           05 PRINTER-DEFAULT          PIC  X(015) VALUE SPACES.
           05 NIVEL.
              10 NIVEL-N               PIC  9(001) VALUE 0.
              10                       PIC  X(024) VALUE SPACES.
           05 PROV                     PIC  X(001) VALUE "0".
           05 INIT-NOME                PIC  X(030) VALUE SPACES.
           05 CLASSE-OCCUR             PIC  9(002) VALUE 0.
           05 CLASSE-HEIGHT            PIC  9(002) VALUE 0.
           05 SENHA-PROV               PIC  9(006) VALUE 0.
           05 N                        PIC  9(002) VALUE 0.
           05 SENHA-AUTO                           VALUE SPACES.
              10 SENHA-X  OCCURS 30    PIC 9(2) COMP-X.
           05 MODO-MENU                PIC  9(001) VALUE 0.
           05 SENHA-ORIGINAL           PIC  X(030) VALUE SPACES.
           05 NOME2                    PIC  X(032) VALUE SPACES.
           05 ESPACOS                  PIC  X(080) VALUE SPACES.
           05 SALVA-NOME               PIC  X(030) VALUE SPACES.
           05 GRUPO-ATUAL              PIC  X(022) VALUE SPACES.
           05 CAMPO                    PIC  9(002) VALUE 0.
           05 ERRO                     PIC  9(001) VALUE 0.
           05 MINUSCULAS PIC X(26) VALUE "abcdefghijklmnopqrstuvwxyz".
           05 MAIUSCULAS PIC X(26) VALUE "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
           05 OK                       PIC  9(001) VALUE ZERO.
           05 LINHA-BRANCA             PIC  X(040) VALUE SPACES.
           05 TECLA                    PIC  9(003) VALUE 0. COPY CWKEYS.
           05 I                        PIC  9(002) VALUE 1.
           05 Y                        PIC  9(002) VALUE ZERO.
           05 FL-EXIT                  PIC  9(001) VALUE 1.
           05 SALVA-CHAVE              PIC  X(032) VALUE SPACES.
           05 MENSAGEM-ERRO            PIC  X(074) VALUE SPACES.
              88 SEM-ERRO                          VALUE SPACES.
           05 MENSAGENS-DE-ERRO.
              10 F PIC X(30) VALUE "Entre com os dados            ".
              10 F PIC X(30) VALUE "Usu†rio n∆o cadastrado        ".
              10 F PIC X(30) VALUE "Confirme exclus∆o             ".
              10 F PIC X(30) VALUE "Usu†rio j† cadastrado         ".
              10 F PIC X(30) VALUE "Usu†rio de privilÇgio alto    ".
              10 F PIC X(30) VALUE "Falta a senha                 ".
              10 F PIC X(30) VALUE "Senha n∆o confere             ".
              10 F PIC X(30) VALUE "Grupo n∆o cadastrado          ".
           05 FILLER REDEFINES MENSAGENS-DE-ERRO.
              10 MSG OCCURS 8 PIC X(30).
           05 TAB-USUARIO.
              10              PIC  X(22) VALUE "0 Digitador I".
              10              PIC  X(22) VALUE "1 Digitador II".
              10              PIC  X(22) VALUE "2 Operador".
              10              PIC  X(22) VALUE "3 Usu†rio".
              10              PIC  X(22) VALUE "4 Programador".
              10              PIC  X(22) VALUE "5 Gerente de produá∆o".
              10              PIC  X(22) VALUE "6 Analista de sistemas".
              10              PIC  X(22) VALUE "7 Chefe de equipe".
              10              PIC  X(22) VALUE "8 Gerente de sistemas".
              10              PIC  X(22) VALUE "9 Super usu†rio".
           05 REDEFINES TAB-USUARIO.
              10 TBU          PIC  X(22) OCCURS 10.
           05 NADA                     PIC  X(001) VALUE SPACE.
           05 WORK-PAR.
              10                       PIC  X(002) VALUE "PS".
              10 WORK-NOME             PIC  X(030) VALUE SPACES.
              10 WORK-CLASSE           PIC  9(001) VALUE 0.

       COPY CWFUNC.
       COPY CWSEND.
       COPY CWTIME.
       COPY CWCONF.

       LINKAGE SECTION.

       01  CLASSE                       PIC  9(001).
       01  NOME                         PIC  X(030).

       SCREEN SECTION.

       01  CWMEN8A.
           05 LINE 08 COLUMN 03 VALUE "Usu†rio       ".
           05 LINE 08 COLUMN 55 VALUE "Bloqueado".
           05 LINE 10 COLUMN 03 VALUE "Senha         ".
           05 LINE 12 COLUMN 03 VALUE "Grupo         ".
           05 LINE 12 COLUMN 43 VALUE "PrivilÇgio".
           05 LINE 14 COLUMN 03 VALUE "Impressora    ".
           05 LINE 15 COLUMN 03 VALUE "Pasta de spool".
           05 LINE 16 COLUMN 07 VALUE "Auto-visualizaá∆o".
           05 LINE 18 COLUMN 03 VALUE "SMTP   ".
           05 LINE 19 COLUMN 03 VALUE "E-mail ".
           05 LINE 20 COLUMN 03 VALUE "Senha ".
           05 LINE 21 COLUMN 03 VALUE "Porta ".
           05 LINE 21 COLUMN 20 VALUE "SSL".
           05 LINE 21 COLUMN 28 VALUE "Autenticado".

       01  CWMEN8B AUTO.
       02  CWMEN8B-A.
           05 LINE 12 COLUMN 18 PIC X(022) USING CWCONF-GRUPO.
       05  CWMEN8B-A2.
           06 LINE 12 COLUMN 55 PIC X(022) USING NIVEL.
           06 LINE 14 COLUMN 18 PIC X(015) USING PRINTER-DEFAULT.
           06 LINE 15 COLUMN 18 PIC X(030) USING CWCONF-PATH-SPOOL.
           06 LINE 16 COLUMN 04 PIC X(001) USING CWCHECK-1.
           06 LINE 18 COLUMN 10 PIC X(042) USING CWCONF-SMTP.
           06 LINE 19 COLUMN 10 PIC X(042) USING CWCONF-E-MAIL.
           06 LINE 20 COLUMN 10 PIC X(020) USING CWCONF-SMTP-PASSWORD
                                           SECURE.
           06 LINE 21 COLUMN 10 PIC X(005) USING CWCONF-SMTP-PORT.
           06 LINE 21 COLUMN 17 PIC X(001) USING CWCHECK-3.
           06 LINE 21 COLUMN 25 PIC X(001) USING CWCHECK-4.
        02 CWMEN8B-B.
           05 LINE 08 COLUMN 52 PIC X(001) USING CWCHECK-2.

       01  CWMEN8E AUTO.
           05 LINE 08 COLUMN 18 PIC X(030)   USING CWCONF-NOME.
           05 LINE 10 COLUMN 18 PIC X(006)   FROM  SPACES.

       01  CWMEN8F AUTO.
           05 LINE 10 COLUMN 18 PIC X(006)   USING SENHA-PROV.

       PROCEDURE DIVISION USING CLASSE NOME.

       000-INICIO.

           PERFORM 800-INICIAIS      THRU 800-99-FIM
           PERFORM 100-PROCESSAMENTO THRU 100-99-FIM
                   UNTIL FINALIZAR
           PERFORM 900-FINAIS        THRU 900-99-FIM.

       000-99-FIM.

           IF   PARAR
                STOP RUN
           ELSE
                GOBACK
           END-IF.

       100-PROCESSAMENTO.

           EXEC COBOLware Time
                DATE-FINAL;HOJE
           END-EXEC
010516     on 1
010516     DISPLAY CWMEN8A
010516             CWMEN8B
010516             CWMEN8E.
           IF   FL-EXIT EQUAL 1
           AND  CLASSE GREATER 1
                EXEC COBOLware Object Drop END-EXEC
                MOVE SPACE TO FUNCAO
                CALL "CWIAEF" USING FUNCAO
                MOVE ZERO  TO FL-EXIT
           END-IF

           IF   CLASSE LESS 2
                MOVE ZERO TO FL-EXIT
                MOVE "A"  TO FUNCAO
           END-IF

           MOVE "23" TO FS-CWCONF

           IF   NOT FINALIZAR
                IF   NOT CONSULTA
                     PERFORM 110-LIMPAR THRU 110-99-FIM
                     IF  INCLUSAO
                         MOVE SPACES TO CWCONF-NOME
                     END-IF
                END-IF
                PERFORM 140-LER-CWCONF  THRU 140-99-FIM
                        UNTIL FS-CWCONF EQUAL "00"
                        OR    FL-EXIT EQUAL 1
                IF   INCLUSAO
                     PERFORM 110-LIMPAR THRU 110-99-FIM
                END-IF
                CALL "CWMSGW" USING "230340" LINHA-BRANCA
                PERFORM 170-EXIBE-DADOS THRU 170-99-FIM
                IF  (INCLUSAO OR ALTERACAO)
                AND  FL-EXIT NOT EQUAL 1
                     MOVE    SPACE              TO COMANDO
                     MOVE    MSG (1)            TO MENSAGEM-ERRO
                     PERFORM 130-CRITICA      THRU 130-99-FIM
                             UNTIL MENSAGEM-ERRO EQUAL SPACES
                             OR    ABORTAR
                     IF   ABORTAR
                          IF   INCLUSAO
                               SET CWSQLC-DELETE TO TRUE
                          ELSE
                               SET CWSQLC-READ   TO TRUE
                          END-IF
                          CALL "CWCONF" USING CWSQLC
                                              CWCONF-REG
                                              FS-CWCONF
                                              KCO PCO
                          IF   INCLUSAO
                               PERFORM 110-LIMPAR THRU 110-99-FIM
                          END-IF
                          PERFORM 170-EXIBE-DADOS THRU 170-99-FIM
      *                   MOVE SPACES TO CWCONF-NOME
                     END-IF
                ELSE
                     IF   FL-EXIT NOT EQUAL 1
                     AND  EXCLUSAO
                          MOVE    SPACE TO COMANDO
                          PERFORM 160-CHECK-COMANDO THRU 160-99-FIM
                     END-IF
                END-IF
           END-IF

           MOVE "00" TO FS-CWCONF

           IF   EFETIVAR
           AND  NOT FINALIZAR
                MOVE SPACE       TO COMANDO
                MOVE CWCONF-NOME TO NOME2
                CALL "CWLOGW" USING FUNCAO NOME2
                IF   INCLUSAO
                     MOVE NOME          TO CWCONF-PAI
                     SET CWSQLC-REWRITE TO TRUE
                     CALL "CWCONF" USING CWSQLC
                                         CWCONF-REG
                                         FS-CWCONF
                                         KCO PCO
                ELSE
                     IF   EXCLUSAO
                          MOVE CWCONF-NOME TO SALVA-NOME
                          MOVE "92"        TO CWCONF-TIPO
                          MOVE SALVA-NOME  TO CWCONF-ELEMENTO
                          PERFORM TEST AFTER UNTIL FS-CWCONF NOT = "9D"
                                  MOVE "92"       TO CWCONF-TIPO
                                  MOVE SALVA-NOME TO CWCONF-ELEMENTO
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
                          IF  FS-CWCONF < "10"
                              SET CWSQLC-DELETE TO TRUE
                              CALL "CWCONF" USING CWSQLC
                                                  CWCONF-REG
                                                  FS-CWCONF
                                                  KCO PCO
                          END-IF
                          PERFORM TEST AFTER UNTIL FS-CWCONF NOT = "9D"
                                  MOVE "AT"       TO CWCONF-TIPO
                                  MOVE SALVA-NOME TO CWCONF-ELEMENTO
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
                          IF  FS-CWCONF < "10"
                              SET CWSQLC-DELETE TO TRUE
                              CALL "CWCONF" USING CWSQLC
                                                  CWCONF-REG
                                                  FS-CWCONF
                                                  KCO PCO
                          END-IF
                          MOVE "94"       TO CWCONF-REG
                          SET CWSQLC-START TO TRUE
                          SET CWSQLC-NOT-LESS TO TRUE
                          CALL "CWCONF" USING CWSQLC
                                              CWCONF-REG
                                              FS-CWCONF
                                              KCO PCO
                          PERFORM TEST AFTER
                             UNTIL (FS-CWCONF NOT = "9D")
                                 AND (CWCONF-TIPO NOT = "94")
                                SET CWSQLC-READ TO TRUE
                                SET CWSQLC-NEXT TO TRUE
                                CALL "CWCONF" USING CWSQLC
                                                    CWCONF-REG
                                                    FS-CWCONF
                                                    KCO PCO
                                IF   FS-CWCONF = "9D"
                                     CALL "CWCONF" USING "ISAM"
                                ELSE
                                     IF  CWCONF-TIPO = "94"
                                     AND FS-CWCONF < "10"
                                     AND CWCONF-NAME-REPORT = SALVA-NOME
                                     AND CWCONF-NAME-REPORT NOT = SPACES
                                         SET CWSQLC-DELETE TO TRUE
                                         CALL "CWCONF" USING CWSQLC
                                                             CWCONF-REG
                                                             FS-CWCONF
                                                             KCO PCO
                                     END-IF
                                END-IF
                          END-PERFORM
                          PERFORM TEST AFTER UNTIL FS-CWCONF NOT = "9D"
                                  MOVE "PS"       TO CWCONF-REG
                                  MOVE SALVA-NOME TO CWCONF-NOME
                                  SET CWSQLC-READ TO TRUE
                                  SET CWSQLC-EQUAL TO TRUE
                                  CALL "CWCONF" USING CWSQLC
                                                      CWCONF-REG
                                                      FS-CWCONF
                                                      KCO PCO
                                  IF   FS-CWCONF = "9D"
                                       CALL "CWCONF" USING "ISAM"
                                  ELSE
                                     IF  FS-CWCONF < "10"
                                         SET CWSQLC-DELETE TO TRUE
                                         CALL "CWCONF" USING CWSQLC
                                                             CWCONF-REG
                                                             FS-CWCONF
                                                             KCO PCO
                                     END-IF
                                  END-IF
                          END-PERFORM
                     ELSE
                          SET CWSQLC-REWRITE TO TRUE
                          CALL "CWCONF" USING CWSQLC
                                              CWCONF-REG
                                              FS-CWCONF
                                              KCO PCO
                          IF  FS-CWCONF < '10'
                          AND (CWCONF-BLOQUEADO <> BLOQUEADO)
                              PERFORM VARYING I FROM LENGTH CWCONF-NOME
                                   BY -1 UNTIL I = 1
                                          OR (CWCONF-NOME (I:1) <> ' ')
                                      CONTINUE
                              END-PERFORM
                              MOVE SPACES TO OBS
                              IF BLOQUEADO = '0'
                              STRING '"' DELIMITED BY SIZE
                                    CWCONF-NOME(1:I) DELIMITED BY SIZE
                                    '" bloqueado manualmente'
                                    DELIMITED BY SIZE
                                INTO OBS
                              ELSE
                              STRING '"' DELIMITED BY SIZE
                                    CWCONF-NOME(1:I) DELIMITED BY SIZE
                                    '" desbloqueado manualmente'
                                    DELIMITED BY SIZE
                                INTO OBS
                              END-IF
                              CALL "CWLOGW" USING "#" OBS
                          END-IF
                     END-IF
                END-IF
           END-IF

           IF   FS-CWCONF EQUAL "PS"
                MOVE "00" TO FS-CWCONF
           END-IF

           IF   CLASSE LESS 2
                MOVE "V" TO FUNCAO
           END-IF.

       100-99-FIM. EXIT.

       110-LIMPAR.

           MOVE SPACES  TO CWCONF-PASSWORD
                           CWCONF-PATH-SPOOL
                           CWCONF-E-MAIL
                           CWCONF-SMTP
                           CWCONF-SMTP-PASSWORD
                           CWCONF-PRINTER-DEFAULT2
                           CWCONF-PRINTER-DEFAULT
                           CWCONF-ESQUECI
           MOVE ZERO    TO CWCONF-FATOR-PS
                           CWCONF-NIVEL-PS
                           CWCONF-SMTP-SIZE
                           CWCONF-SMTP-FATOR
                           CWCONF-LOGIN-ERRO
                           CWCONF-LOGIN-LAST
                           CWCONF-ESQUECI-SIZE
                           CWCONF-ESQUECI-FATOR
           MOVE "0"     TO CWCONF-BLOQUEADO
           MOVE HOJE    TO CWCONF-DATA-SENHA
           MOVE ALL "0" TO CWCONF-SPOOL-OPTIONS
           MOVE SPACES  TO CWCONF-GRUPO.

       110-99-FIM. EXIT.

       130-CRITICA.

           IF   CLASSE < 5
                MOVE GRUPO-ATUAL TO CWCONF-GRUPO
           END-IF

           MOVE SPACES           TO MENSAGEM-ERRO
           MOVE 0                TO TECLA
           MOVE 1                TO CAMPO
           MOVE CWCONF-AUTOVIEW  TO CWCHECK-1
           MOVE CWCONF-BLOQUEADO TO CWCHECK-2
           MOVE CWCONF-SSL       TO CWCHECK-3
           MOVE CWCONF-AUTENTICACAO TO CWCHECK-4
           IF  (CWCHECK-2 = "0" OR SPACE)
           AND  CWCONF-LOGIN-ERRO > 2
                MOVE "1" TO CWCHECK-2
           END-IF
           MOVE 0               TO ERRO
           COMPUTE I = CWCONF-NIVEL-PS + 1
           MOVE TBU (I) TO NIVEL
           IF  (CWCONF-SMTP-PASSWORD NOT EQUAL SPACES)
           AND (CWCONF-SMTP-SIZE NOT = 32)
                CALL "CWCODE" USING "D" CWCONF-SMTP-SIZE
                                        CWCONF-SMTP-FATOR
                                        CWCONF-SMTP-PASSWORD
           END-IF
           IF  CLASSE = 9
           AND (INIT-NOME NOT = CWCONF-NOME)
               ACCEPT CWMEN8B
           ELSE
               IF   CLASSE > 4
                    ACCEPT CWMEN8B-A
               ELSE
                    ACCEPT CWMEN8B-A2
               END-IF
           END-IF
           IF  CWCONF-SMTP-PASSWORD NOT = SPACES
               PERFORM VARYING CWCONF-SMTP-SIZE FROM 20 BY -1
                       UNTIL CWCONF-SMTP-PASSWORD (CWCONF-SMTP-SIZE:1)
                             NOT = SPACE
               END-PERFORM
               CALL "CWCODE" USING "C" CWCONF-SMTP-SIZE
                                       CWCONF-SMTP-FATOR
                                       CWCONF-SMTP-PASSWORD
           END-IF
           MOVE PRINTER-DEFAULT      TO CWCONF-PRINTER-DEFAULT
           MOVE PRINTER-DEFAULT (9:) TO CWCONF-PRINTER-DEFAULT2
           MOVE NIVEL-N TO CWCONF-NIVEL-PS
           ACCEPT TECLA FROM ESCAPE KEY
           MOVE   CWCHECK-1     TO CWCONF-AUTOVIEW
           MOVE   CWCHECK-3     TO CWCONF-SSL
           MOVE   CWCHECK-4     TO CWCONF-AUTENTICACAO
           IF  CLASSE = 9
           AND (INIT-NOME NOT = CWCONF-NOME)
               IF  CWCHECK-2 = "0"
                   MOVE  0  TO CWCONF-LOGIN-ERRO
                   MOVE "0" TO CWCONF-BLOQUEADO
               ELSE
                   MOVE "1" TO CWCONF-BLOQUEADO
               END-IF
           END-IF

           IF  CLASSE > 7
           AND ALTERACAO
           AND F4
               IF  CWCONF-NIVEL-PS < 9
                  MOVE SPACES      TO CWSEND-SCREENS
                  MOVE "Remover senha" TO CWSEND-MSG
                  MOVE 1           TO CWSEND-OPTION
                  MOVE "~Sim"      TO CWSEND-SCREEN (1)
                  MOVE "O"         TO CWSEND-CHAR   (1)
                  MOVE "~N∆o"      TO CWSEND-SCREEN (2) CWSEND-CHAR (2)
                  PERFORM TEST AFTER UNTIL COMANDO = "S" OR "N"
                          CALL "CWSEND"        USING PARAMETROS-CWSEND
                          MOVE CWSEND-OPTION-CHAR TO COMANDO
                  END-PERFORM
                  MOVE SPACES      TO CWSEND-SCREENS
                  IF  COMANDO = "S"
                      INITIALIZE CWCONF-SENHA
                                 CWCONF-SIZE-PS
                                 CWCONF-FATOR-PS
                  END-IF
               ELSE
                  MOVE SPACES      TO CWSEND-SCREENS
                  MOVE "N∆o Ç permitido remover senha de supervisor"
                    TO CWSEND-MSG
                  CALL "CWSEND"        USING PARAMETROS-CWSEND
               END-IF
               GO TO 130-CRITICA
           END-IF

           IF   ESC
                MOVE 1   TO FL-EXIT
                EVALUATE TRUE
                    WHEN ALTERACAO
                         MOVE "Cancelar alteraá∆o ?" TO CWSEND-MSG
                    WHEN EXCLUSAO
                         MOVE "Cancelar exclus∆o ?" TO CWSEND-MSG
                    WHEN INCLUSAO
                         MOVE "Cancelar inclus∆o ?" TO CWSEND-MSG
                END-EVALUATE
                MOVE SPACES      TO CWSEND-SCREENS
                MOVE 1           TO CWSEND-OPTION
                MOVE "~Sim"  TO CWSEND-SCREEN (1)
                MOVE "O"         TO CWSEND-CHAR   (1)
                MOVE "~N∆o"  TO CWSEND-SCREEN (2) CWSEND-CHAR (2)
                PERFORM TEST AFTER UNTIL COMANDO = "S" OR "N"
                        CALL "CWSEND"        USING PARAMETROS-CWSEND
                        MOVE CWSEND-OPTION-CHAR TO COMANDO
                END-PERFORM
                MOVE SPACES      TO CWSEND-SCREENS
                IF  COMANDO = "S"
                    SET ABORTAR TO TRUE
                ELSE
                    GO TO 130-CRITICA
                END-IF
           ELSE
                IF   INCLUSAO AND CWCONF-NOME = SPACES
                     MOVE "Identifique o usu†rio" TO MENSAGEM-ERRO
                END-IF
                IF   MENSAGEM-ERRO = SPACES
                     PERFORM 160-CHECK-COMANDO THRU 160-99-FIM
                ELSE
                     MOVE MENSAGEM-ERRO TO CWSEND-MSG
                     CALL "CWSEND" USING PARAMETROS-CWSEND
                END-IF
           END-IF.

       130-99-FIM.  EXIT.

       140-LER-CWCONF.

           MOVE SPACES TO SENHA-DG
           EXEC COBOLware OBJECT DROP END-EXEC
           MOVE MENSAGEM-ERRO TO CWSEND-MSG
           IF FIM = ZERO
              CALL "CWSEND" USING PARAMETROS-CWSEND
           END-IF
           IF   MENSAGEM-ERRO EQUAL SPACES
300715     OR   FIM = 1
                IF  (NOT INCLUSAO)
                AND (CLASSE GREATER 1)
                     MOVE NOME TO WORK-NOME
                     EXEC COBOLware OBJECT COMBO-BOX NOEDIT
                               LINE 08 HEIGHT 15 COLUMN 18 WIDTH 30
                               PROGRAM "CWMENF" WORK-AREA WORK-PAR
                               OPTION CWCONF-NOME ORDER 1 RETURN 1
                               STRING-1-LENGTH 30
                     END-EXEC
                     COPY CWPGON REPLACING 23 BY footline.
                END-IF
           END-IF

           MOVE SPACES TO MENSAGEM-ERRO
           COPY CWESCP REPLACING 23 BY footline.

           MOVE 0 TO TECLA
                     FIM

           IF   CLASSE GREATER 1
                IF   INCLUSAO
                     MOVE SPACES TO CWCONF-NOME
                END-IF
                PERFORM TEST AFTER
                        UNTIL(NOT (PAGE-UP   AND PAGE-UP-OFF))
                          AND(NOT (PAGE-DOWN AND PAGE-DOWN-OFF))
                     ACCEPT CWMEN8E
                     ACCEPT TECLA FROM ESCAPE KEY
                END-PERFORM
                IF   INCLUSAO
                AND  CWCONF-NOME = SPACES
                AND  (NOT ESC)
                     MOVE "Identifique o usu†rio" TO MENSAGEM-ERRO
                     GO TO 140-LER-CWCONF
                END-IF
           ELSE
                MOVE NOME TO CWCONF-NOME
                DISPLAY CWMEN8E
           END-IF

           EXEC COBOLware OBJECT DROP END-EXEC
           IF   ESC
                MOVE 1 TO FL-EXIT
           ELSE
                COPY CWESCP REPLACING 23 BY footline.
                IF   ALTERACAO OR INCLUSAO
                     IF   CLASSE > 4
                          MOVE CLASSE TO CLASSE-GU
                          EXEC COBOLware OBJECT COMBO-BOX NOEDIT
                               CAPTION "Grupos"
                               LINE 12 HEIGHT 05 COLUMN 18 WIDTH 22
                               PROGRAM "CWMENF"
                               OPTION CWCONF-GRUPO
                               ORDER 1 RETURN 1
                               STRING-1-LENGTH 22
                               WORK-AREA GU-CLASSE
                          END-EXEC
                     END-IF
                     COMPUTE CLASSE-OCCUR  = CLASSE + 1
                     COMPUTE CLASSE-HEIGHT = CLASSE + 1
      *              IF   CLASSE-HEIGHT > 7
      *                   MOVE 7 TO CLASSE-HEIGHT
      *              END-IF
                     EXEC COBOLware OBJECT LIST-BOX
                                    CAPTION "PrivilÇgio"
                                    LINE 12 COLUMN 55
                                    HEIGHT CLASSE-HEIGHT WIDTH 22
                                    OPTION NIVEL
                                    STRING-1(01) TBU(01)
                                    STRING-1(02) TBU(02)
                                    STRING-1(03) TBU(03)
                                    STRING-1(04) TBU(04)
                                    STRING-1(05) TBU(05)
                                    STRING-1(06) TBU(06)
                                    STRING-1(07) TBU(07)
                                    STRING-1(08) TBU(08)
                                    STRING-1(09) TBU(09)
                                    STRING-1(10) TBU(10)
                                    STRING-1-LENGTH 22
                     END-EXEC
                     EXEC COBOLware OBJECT COMBO-BOX NOEDIT
                          CAPTION "Impressoras"
                          LINE 14 COLUMN 18
                          HEIGHT 08 WIDTH 15
                          PROGRAM "CWMENF"
                          OPTION PRINTER-DEFAULT
                          ORDER 1 RETURN 1
                          STRING-1-LENGTH 15
                          WORK-AREA "03"
                     END-EXEC
                     EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                               LINE footline COLUMN 02 WIDTH 13
                               CAPTION " f2-~Concluir "
                               KEY F2
                     END-EXEC
                     IF  CLASSE > 7
                     AND ALTERACAO
                     AND CWCONF-NIVEL-PS < 9
                         EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                                   LINE footline COLUMN 15 WIDTH 18
                                   CAPTION " f4-remover ~Senha "
                                   KEY F4
                         END-EXEC
                     END-IF
                END-IF
           END-IF
           MOVE "PS" TO CWCONF-TIPO

           MOVE MENSAGEM-ERRO TO CWSEND-MSG
           CALL "CWSEND" USING PARAMETROS-CWSEND

           INSPECT CWCONF-NOME CONVERTING MINUSCULAS TO MAIUSCULAS
           IF   PROV = "1"
           AND  CWCONF-NOME = NOME
                MOVE SPACES TO CWCONF-NOME
           END-IF

           IF   CWCONF-NOME EQUAL SPACES
           AND  NOT (PAGE-UP OR PAGE-DOWN)
                MOVE "44" TO FS-CWCONF
           ELSE
           IF   FL-EXIT NOT EQUAL 1
                IF   (PAGE-UP OR PAGE-DOWN)
                AND  NOT INCLUSAO
                AND  OK EQUAL ZERO
                     MOVE 1    TO OK
                     PERFORM TEST AFTER UNTIL (FS-CWCONF NOT = "9D")
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
                             MOVE "PS" TO CWCONF-CHAVE(1:2)
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
                             UNTIL (CLASSE GREATER CWCONF-NIVEL-PS
                                    OR CLASSE EQUAL 9)
                                OR NOME EQUAL CWCONF-NOME
                                OR (FS-CWCONF NOT EQUAL "00")
                                OR (CWCONF-TIPO NOT EQUAL "PS")
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
                                     END-IF
                                     IF   PROV = "1"
                                     AND  FS-CWCONF < "10"
                                     AND  CWCONF-NOME = NOME
                                          MOVE "9D" TO FS-CWCONF
                                     END-IF
                                     IF FS-CWCONF < '10'
                                     AND CWCONF-TIPO EQUAL "PS"
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
                                     END-IF
                                     IF   PROV = "1"
                                     AND  FS-CWCONF < "10"
                                     AND  CWCONF-NOME = NOME
                                          MOVE "9D" TO FS-CWCONF
                                     END-IF
                                     IF FS-CWCONF < '10'
                                     AND CWCONF-TIPO EQUAL "PS"
                                     AND PAGE-DOWN-OFF
                                         SET PAGE-DOWN-ON TO TRUE
                                     END-IF
                                  END-PERFORM
                             END-EVALUATE
                     END-PERFORM
                     IF   CWCONF-TIPO EQUAL "PS"
                     AND  FS-CWCONF EQUAL "00"
                          SET READY-ON TO TRUE
                          MOVE CWCONF-CHAVE TO SALVA-CHAVE
                          PERFORM 170-EXIBE-DADOS THRU 170-99-FIM
                          GO TO 140-LER-CWCONF
                     ELSE
                          IF FS-CWCONF = "10"
                          OR (CWCONF-TIPO NOT EQUAL "PS")
                             MOVE 1 TO FIM
                             IF PAGE-UP
                                SET PAGE-UP-OFF TO TRUE
                             END-IF
                             IF PAGE-DOWN
                                SET PAGE-DOWN-OFF TO TRUE
                             END-IF
                          END-IF
                          MOVE ZERO TO OK
                          MOVE "PS" TO CWCONF-REG
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
                       MOVE CWCONF-BLOQUEADO TO BLOQUEADO
                       IF   BLOQUEADO NOT = '1'
                            MOVE '0' TO BLOQUEADO
                       END-IF
                       IF   FS-CWCONF = "9D"
                            CALL "CWCONF" USING "ISAM"
                       END-IF
                     END-PERFORM
                END-IF
                IF   FS-CWCONF EQUAL "00"
                     IF  (CWCONF-NIVEL-PS NOT LESS CLASSE)
                     AND (CWCONF-NOME     NOT EQUAL NOME)
                     AND (CLASSE          NOT EQUAL 9)
                          MOVE MSG (5) TO MENSAGEM-ERRO
                          MOVE "44" TO FS-CWCONF
                          GO TO 140-99-FIM
                     END-IF
                     PERFORM 170-EXIBE-DADOS THRU 170-99-FIM
                     IF   INCLUSAO
                          MOVE MSG (4) TO MENSAGEM-ERRO
                          MOVE "44" TO FS-CWCONF
                     ELSE
                          NEXT SENTENCE
                     END-IF
                ELSE
                     SET READY-OFF TO TRUE
                     IF   NOT INCLUSAO
                          MOVE MSG (2) TO MENSAGEM-ERRO
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
                          IF  FIM = 1
                              MOVE "Fim da leitura" TO MENSAGEM-ERRO
                          END-IF
                          PERFORM 170-EXIBE-DADOS THRU 170-99-FIM
                     ELSE
                          MOVE MODO-MENU TO CWCONF-MODO-MENU
                          IF CLASSE < 8
                             EXEC COBOLware GetSystem
                             Group;CWCONF-GRUPO
                             END-EXEC
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
                MOVE ZERO        TO FS-CWCONF
                MOVE SPACES      TO COMANDO
                                    FUNCAO
3007       END-IF
           END-IF.

       140-99-FIM. EXIT.

       160-CHECK-COMANDO.

           COPY CWEFAB.

       160-99-FIM. EXIT.

       170-EXIBE-DADOS.

           IF  CWCONF-GRUPO = "Acesso sem restriáîes"
           OR  CWCONF-GRUPO = "Acesso sem restriá‰es"
           OR  CWCONF-GRUPO = "Acesso sem restricoes"
           OR  CWCONF-GRUPO = SPACES
               MOVE "Acesso irrestrito" TO CWCONF-GRUPO
           END-IF
           MOVE CWCONF-PRINTER-DEFAULT  TO PRINTER-DEFAULT
           MOVE CWCONF-PRINTER-DEFAULT2 TO PRINTER-DEFAULT (9:)
           MOVE CWCONF-AUTOVIEW TO CWCHECK-1

           IF   PRINTER-DEFAULT = SPACES OR "Spool"
                MOVE "<Spool>" TO PRINTER-DEFAULT
           END-IF

           COMPUTE I = CWCONF-NIVEL-PS + 1
           MOVE TBU (I) TO NIVEL
           DISPLAY CWMEN8E
                   CWMEN8B
           IF  CONSULTA
           AND NOME = CWCONF-NOME
           AND (ECHOPASS NOT = 'OFF')
           AND TECLA = 0
               SET CWTIME-NORMAL       TO TRUE
               SET CWTIME-TODAY        TO TRUE
               CALL "CWTIME"        USING PARAMETROS-CWTIME
               EXEC COBOLware GetSys
                    Password;SENHA-AUTO
               END-EXEC
               INSPECT SENHA-AUTO CONVERTING MINUSCULAS
                                          TO MAIUSCULAS
               MOVE 0 TO SENHA-PROV
               PERFORM VARYING I FROM 1 BY 1 UNTIL I > 30
                       IF   SENHA-X (I) NOT = 32
                            ADD SENHA-X (I) TO SENHA-PROV
                       END-IF
               END-PERFORM
               MOVE CWTIME-DATE-FINAL (8: 2) TO N
               ADD  N                   TO SENHA-PROV
               MOVE CWTIME-DATE-FINAL (1: 2) TO N
               ADD  N                   TO SENHA-PROV
               MOVE CWTIME-TIME-FINAL (1: 2) TO N
               ADD  N                   TO SENHA-PROV
               MOVE CWTIME-DATE-FINAL (3: 2) TO N
               ADD  N                   TO SENHA-PROV
               COMPUTE SENHA-PROV = (1 / SENHA-PROV) * 100000000
               IF SENHA-DG = SPACES
                  ACCEPT SENHA-DG AT 1018 WITH SECURE
                  INSPECT SENHA-DG CONVERTING MINUSCULAS
                                           TO MAIUSCULAS
               END-IF
               IF  SENHA-DG = SENHA-AUTO
                   DISPLAY CWMEN8F
               ELSE
                   MOVE SPACES TO SENHA-DG
               END-IF
           END-IF.

       170-99-FIM. EXIT.

       800-INICIAIS.

           DISPLAY "CWFOOTLINE"  UPON ENVIRONMENT-NAME
           ACCEPT   footline-t   FROM ENVIRONMENT-VALUE
           DISPLAY "CWECHOPASS" UPON ENVIRONMENT-NAME
           ACCEPT ECHOPASS FROM ENVIRONMENT-VALUE
           INSPECT ECHOPASS CONVERTING "ofn" TO "OFN"

           MOVE NOME   TO CWCONF-NOME INIT-NOME
           MOVE CLASSE TO WORK-CLASSE I
           ADD  2 TO I
           PERFORM VARYING I FROM I BY 1 UNTIL I > 10
                   MOVE SPACES TO TBU(I)
           END-PERFORM
           MOVE "&"    TO PROV
           CALL "CWGETU" USING "." "." "." PROV
           SET CWSQLC-UPDATE TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           IF   FS-CWCONF NOT EQUAL "00"
                CALL "CWCONF" USING "ISAM"
                EXIT PROGRAM
           END-IF
           MOVE "GUAcesso irrestrito" TO CWCONF-REGGU
           SET CWSQLC-READ TO TRUE
           SET CWSQLC-EQUAL TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           IF  FS-CWCONF = "23"
               MOVE "E" TO CWCONF-ADM
               SET CWSQLC-WRITE TO TRUE
               CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           END-IF
           MOVE "GUAcesso sem restriáîes" TO CWCONF-REGGU
           SET CWSQLC-READ TO TRUE
           SET CWSQLC-EQUAL TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           IF  FS-CWCONF = "00"
               SET CWSQLC-DELETE TO TRUE
               CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           END-IF
           MOVE "GUAcesso sem restriá‰es" TO CWCONF-REGGU
           SET CWSQLC-READ TO TRUE
           SET CWSQLC-EQUAL TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           IF  FS-CWCONF = "00"
               SET CWSQLC-DELETE TO TRUE
               CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           END-IF
           MOVE "03Spool" TO CWCONF-REG03
           SET CWSQLC-READ TO TRUE
           SET CWSQLC-EQUAL TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           IF  FS-CWCONF = "00"
               SET CWSQLC-DELETE TO TRUE
               CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           END-IF

           MOVE "LG" TO CWCONF-REG
           SET CWSQLC-READ TO TRUE
           SET CWSQLC-EQUAL TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           IF  FS-CWCONF = "00"
               IF   CWCONF-EXPIRE NOT NUMERIC
                    MOVE 0 TO CWCONF-EXPIRE
               END-IF
               IF   CWCONF-REUSE NOT NUMERIC
                    MOVE 0 TO CWCONF-REUSE
               END-IF
               MOVE CWCONF-EXPIRE TO EXPIRE
           ELSE
               MOVE 0             TO EXPIRE
           END-IF

010516*    DISPLAY CWMEN8A

           PERFORM TEST AFTER UNTIL FS-CWCONF NOT = "9D"
                   MOVE "PS" TO CWCONF-REG
                   MOVE NOME TO CWCONF-NOME
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

           IF  FS-CWCONF > "09"
           AND CLASSE < 5
               MOVE SPACES TO CWSEND-MSG CWSEND-SCREENS
               MOVE "Vocà est† excluido do sistema, peáa ajuda"
                 TO CWSEND-MSG
               CALL "CWSEND" USING PARAMETROS-CWSEND
               EXIT PROGRAM
           END-IF

           IF  FS-CWCONF < "10"
               SET  READY-ON     TO TRUE
               MOVE CWCONF-CHAVE TO SALVA-CHAVE
           END-IF

           MOVE CWCONF-MODO-MENU TO MODO-MENU
           MOVE CWCONF-GRUPO     TO GRUPO-ATUAL

           IF   PROV = 1
                SUBTRACT 1 FROM CLASSE
           END-IF
           IF   PROV = 1
                ADD 1 TO CLASSE
           END-IF.

       800-99-FIM. EXIT.

       900-FINAIS.

           SET CWSQLC-CLOSE TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO.

       900-99-FIM. EXIT.

       END PROGRAM CWMEN8.

