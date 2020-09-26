       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWMEN7.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  09/05/1990.
       SECURITY.      *************************************************
                      *                                               *
                      *   Manutencao de RELATORIOS                    *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1. COPY CWCASE.
           05 footline-t.
              10 footline              PIC  9(002) VALUE 23.
           05 FIM                      PIC  9(001) VALUE ZERO.
           05 CWCHECK-KEEP             PIC  X(001) VALUE "0".
           05 CWCHECK-QUIET            PIC  X(001) VALUE "0".
           05 G                   COMP PIC  9(006) VALUE ZERO.
           05 GRUPO-TESTE                          VALUE SPACES.
              10 GRUPO-BYTE OCCURS 8   PIC  X(001).
           05 GRUPO-PRT                PIC  X(008) VALUE SPACES.
           05 GRUPO                    PIC  X(008) VALUE SPACES.
           05 FLAG-GRUPO               PIC  9(001) VALUE 0.
              88 GRUPO-ERRADO                      VALUE 1.
           05 OPERADOR                 PIC  X(030).
           05 TASK                     PIC  9(006).
           05 WK94                                 VALUE "94".
              10 FILLER                PIC  X(007).
              10 NOME                  PIC  X(023).
           05 PROGRAMA                 PIC  X(008) VALUE SPACES.
           05 OK                       PIC  9(001) VALUE ZERO.
           05 LINHA-BRANCA             PIC  X(040) VALUE SPACES.
           05 TECLA                    PIC  9(003) VALUE 0. COPY CWKEYS.
           05 I                        PIC  9(002) VALUE 1.
           05 Y                        PIC  9(002) VALUE 1.
           05 FL-EXIT                  PIC  9(001) VALUE 1.
           05 SALVA-CHAVE              PIC  X(032) VALUE SPACES.
           05 MENSAGEM-ERRO            PIC  X(030) VALUE SPACES.
              88 SEM-ERRO                          VALUE SPACES.
           05 POSICOES.
              10 PIC X(48) VALUE
                 "100111011207130714131513161917191825192520312131".
              10 PIC X(48) VALUE
                 "220123012407250726132713281929193025312532313331".
           05 REDEFINES POSICOES.
              10 OCCURS 24 TIMES.
                 15 CAMPO-TAB   PIC 9(002).
                 15 POSICAO-TAB PIC 9(002).

       01  AREAS-DE-TRABALHO-2.
           05 MENSAGENS-DE-ERRO.
              10 F PIC X(30) VALUE "Entre com os dados            ".
              10 F PIC X(30) VALUE "Relat¢rio n∆o cadastrado      ".
              10 F PIC X(30) VALUE "Confirme exclus∆o             ".
              10 F PIC X(30) VALUE "Relat¢rio j† cadastrado       ".
           05 FILLER REDEFINES MENSAGENS-DE-ERRO.
              10 MSG OCCURS 4 PIC X(30).

       COPY CWBOXS.
       COPY CWFUNC.
       COPY CWSEND.
       COPY CWCONF.
       SCREEN SECTION.

       01  CWMEN7A.
           05 LINE 08 COLUMN 03 VALUE "Relat¢rio".
           05 LINE 09 COLUMN 03 VALUE "Comprime impressora".
           05 LINE 10 COLUMN 03 VALUE "Descomprime impressora".
           05 LINE 12 COLUMN 03 VALUE "T°tulo".
           05 LINE 15 COLUMN 03 VALUE "Subt°tulo".
           05 LINE 07 COLUMN 39 VALUE "Tipo de formul†rio".
           05 LINE 08 COLUMN 39 VALUE "Linhas por p†gina".
           05 LINE 09 COLUMN 39 VALUE "Impressora".
           05 LINE 10 COLUMN 39 VALUE "Manter spool".
           05 LINE 10 COLUMN 62 VALUE "Modo quieto".
           05 LINE 18 COLUMN 03 VALUE "Colunas suprimidas".
           05 LINE 19 COLUMN 11 VALUE "tabuladas".
           05 LINE 20 COLUMN 03 VALUE
              "Empresa                        Sistema".

       01  CWMEN7B AUTO.
       05  LINE 09 COLUMN 26 PIC XX USING CWCONF-PROGRAMA-PRINTER.
       05  LINE 10 COLUMN 26 PIC XX USING CWCONF-DESPROGRAMA.
       05  LINE 12 COLUMN 26 PIC X(43) USING CWCONF-TITLE-1.
       05  LINE 13 COLUMN 26 PIC X(43) USING CWCONF-TITLE-2.
       05  LINE 15 COLUMN 26 PIC X(43) USING CWCONF-SUB-TITLE-1.
       05  LINE 16 COLUMN 26 PIC X(43) USING CWCONF-SUB-TITLE-2.
       05  LINE 07 COLUMN 58 PIC X(20) USING CWCONF-TIPO-FORM.
       05  LINE 08 COLUMN 58 PIC Z(2)  USING CWCONF-SIZE-PAGE.
       05  LINE 09 COLUMN 58 PIC X(15) USING CWCONF-SAIDA.
       05  LINE 10 COLUMN 36 PIC X     USING CWCHECK-KEEP.
       05  LINE 10 COLUMN 59 PIC X     USING CWCHECK-QUIET.
       05  LINE 18 COLUMN 26 PIC Z(3)  USING CWCONF-INICIO-AP (1).
       05  LINE 18 COLUMN 30 PIC Z(3)  USING CWCONF-FIM-AP    (1).
       05  LINE 18 COLUMN 35 PIC Z(3)  USING CWCONF-INICIO-AP (2).
       05  LINE 18 COLUMN 39 PIC Z(3)  USING CWCONF-FIM-AP    (2).
       05  LINE 18 COLUMN 44 PIC Z(3)  USING CWCONF-INICIO-AP (3).
       05  LINE 18 COLUMN 48 PIC Z(3)  USING CWCONF-FIM-AP    (3).
       05  LINE 18 COLUMN 53 PIC Z(3)  USING CWCONF-INICIO-AP (4).
       05  LINE 18 COLUMN 57 PIC Z(3)  USING CWCONF-FIM-AP    (4).
       05  LINE 18 COLUMN 62 PIC Z(3)  USING CWCONF-INICIO-AP (5).
       05  LINE 18 COLUMN 66 PIC Z(3)  USING CWCONF-FIM-AP    (5).
       05  LINE 18 COLUMN 71 PIC Z(3)  USING CWCONF-INICIO-AP (6).
       05  LINE 18 COLUMN 75 PIC Z(3)  USING CWCONF-FIM-AP    (6).
       05  LINE 19 COLUMN 26 PIC Z(3)  USING CWCONF-INICIO-TB (1).
       05  LINE 19 COLUMN 30 PIC Z(3)  USING CWCONF-FIM-TB    (1).
       05  LINE 19 COLUMN 35 PIC Z(3)  USING CWCONF-INICIO-TB (2).
       05  LINE 19 COLUMN 39 PIC Z(3)  USING CWCONF-FIM-TB    (2).
       05  LINE 19 COLUMN 44 PIC Z(3)  USING CWCONF-INICIO-TB (3).
       05  LINE 19 COLUMN 48 PIC Z(3)  USING CWCONF-FIM-TB    (3).
       05  LINE 19 COLUMN 53 PIC Z(3)  USING CWCONF-INICIO-TB (4).
       05  LINE 19 COLUMN 57 PIC Z(3)  USING CWCONF-FIM-TB    (4).
       05  LINE 19 COLUMN 62 PIC Z(3)  USING CWCONF-INICIO-TB (5).
       05  LINE 19 COLUMN 66 PIC Z(3)  USING CWCONF-FIM-TB    (5).
       05  LINE 19 COLUMN 71 PIC Z(3)  USING CWCONF-INICIO-TB (6).
       05  LINE 19 COLUMN 75 PIC Z(3)  USING CWCONF-FIM-TB    (6).
       05  LINE 21 COLUMN 03 PIC X(30) USING CWCONF-EMPRESA-ALT.
       05  LINE 21 COLUMN 34 PIC X(30) USING CWCONF-SISTEMA-ALT.

       01  CWMEN7E AUTO.
           05 LINE 08 COLUMN 26 PIC X(7) USING CWCONF-RELATORIO.

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
                     DISPLAY CWMEN7A
                END-IF.

           MOVE "11" TO FS-CWCONF

           IF   NOT FINALIZAR
                MOVE SPACES TO MENSAGEM-ERRO
                PERFORM 170-EXIBE-DADOS THRU 170-99-FIM
                PERFORM 140-LER-CWCONF  THRU 140-99-FIM
                        UNTIL FS-CWCONF EQUAL "00"
                        OR    FL-EXIT EQUAL 1
                DISPLAY (footline, 3) LINHA-BRANCA WITH SIZE 40
                IF  (INCLUSAO OR ALTERACAO)
                AND  FL-EXIT NOT EQUAL 1
                     MOVE    MSG (1)            TO MENSAGEM-ERRO
                     MOVE    SPACE              TO COMANDO
                     PERFORM 130-CRITICA      THRU 130-99-FIM
                             UNTIL MENSAGEM-ERRO EQUAL SPACES
                             OR    ABORTAR
                     IF   ABORTAR
                     AND  INCLUSAO
                          SET CWSQLC-DELETE TO TRUE
                          CALL "CWCONF" USING CWSQLC
                                              CWCONF-REG
                                              FS-CWCONF
                                              KCO PCO
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
                IF   CWCONF-PROGRAMA-PRINTER = "15"
                     MOVE X"0F00" TO CWCONF-PROGRAMA-PRINTER
                END-IF
                IF   CWCONF-DESPROGRAMA = "18"
                     MOVE X"1200" TO CWCONF-DESPROGRAMA
                END-IF
                INSPECT CWCONF-PROGRAMA-PRINTER
                        CONVERTING SPACE TO X"00"
                INSPECT CWCONF-DESPROGRAMA
                        CONVERTING SPACE TO X"00"
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
                     ELSE
                          SET CWSQLC-REWRITE TO TRUE
                          CALL "CWCONF" USING CWSQLC
                                              CWCONF-REG
                                              FS-CWCONF
                                              KCO PCO
                     END-IF
                END-IF
           END-IF

           IF   FS-CWCONF EQUAL "94"
                MOVE "00" TO FS-CWCONF
           END-IF

           SET CWSQLC-UNLOCK TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO.

       100-99-FIM. EXIT.

       130-CRITICA.

           MOVE SPACES TO MENSAGEM-ERRO
           ACCEPT CWMEN7B
           MOVE NOME          TO CWCONF-NAME-REPORT
           MOVE CWCHECK-KEEP  TO CWCONF-KEEP
           MOVE CWCHECK-QUIET TO CWCONF-QUIET

           PERFORM 170-EXIBE-DADOS   THRU 170-99-FIM
           PERFORM 160-CHECK-COMANDO THRU 160-99-FIM.

       130-99-FIM.  EXIT.

       140-LER-CWCONF.

           EXEC COBOLware OBJECT DROP END-EXEC
           IF FIM = ZERO
              EXEC COBOLware SEND
                   MSG MENSAGEM-ERRO
              END-EXEC
           END-IF
           MOVE SPACES TO MENSAGEM-ERRO

           IF   MENSAGEM-ERRO EQUAL SPACES
           OR   FIM = 1
                IF   NOT INCLUSAO
                     EXEC COBOLware OBJECT COMBO-BOX
                               LINE 08 COLUMN 26 HEIGHT 15 WIDTH 07
                               PROGRAM "CWMENF" WORK-AREA WK94
                               OPTION CWCONF-RELATORIO ORDER 1 RETURN 1
                               STRING-1-LENGTH 07
                     END-EXEC
                     COPY CWPGON REPLACING 23 BY footline.
                END-IF
           END-IF

           MOVE SPACES TO MENSAGEM-ERRO
           MOVE "94" TO CWCONF-TIPO

           COPY CWESCP REPLACING 23 BY footline.
           MOVE 0 TO TECLA
                     FIM

           PERFORM TEST AFTER
                   UNTIL(NOT (PAGE-UP   AND PAGE-UP-OFF))
                     AND(NOT (PAGE-DOWN AND PAGE-DOWN-OFF))
                   ACCEPT CWMEN7E
                   ACCEPT TECLA FROM ESCAPE KEY
           END-PERFORM
           EXEC COBOLware OBJECT DROP END-EXEC

           IF  ESC
               MOVE 1 TO FL-EXIT
           ELSE
               COPY CWESCP REPLACING 23 BY footline.
               IF   ALTERACAO OR INCLUSAO
                    EXEC COBOLware OBJECT COMBO-BOX
                              LINE 09 COLUMN 58 HEIGHT 10 WIDTH 15
                              PROGRAM "CWMENF" WORK-AREA "03"
                              OPTION CWCONF-SAIDA ORDER 1 RETURN 1
                              STRING-1-LENGTH 15 NOEDIT
                    END-EXEC
                    EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                              LINE footline COLUMN 02 WIDTH 13
                              CAPTION " f2-~Concluir "
                              KEY F2
                    END-EXEC
               END-IF
           END-IF

           EXEC COBOLware SEND
                MSG MENSAGEM-ERRO
           END-EXEC

           INSPECT CWCONF-RELATORIO CONVERTING MINUSCULAS TO MAIUSCULAS
           DISPLAY CWMEN7E

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
                             MOVE "94" TO CWCONF-CHAVE(1:2)
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
                             UNTIL (NOME = CWCONF-NAME-REPORT)
                                OR  FS-CWCONF      >         "09"
                                OR  CWCONF-TIPO    NOT EQUAL "94"
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
                                     IF FS-CWCONF < '10'
                                     AND CWCONF-TIPO EQUAL "94"
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
                                     IF FS-CWCONF < '10'
                                     AND CWCONF-TIPO EQUAL "94"
                                     AND PAGE-DOWN-OFF
                                         SET PAGE-DOWN-ON TO TRUE
                                     END-IF
                                  END-PERFORM
                             END-EVALUATE
                     END-PERFORM
                     IF   CWCONF-TIPO EQUAL "94"
                     AND  FS-CWCONF EQUAL "00"
                     AND  NOME = CWCONF-NAME-REPORT
                          SET READY-ON TO TRUE
                          MOVE CWCONF-CHAVE TO SALVA-CHAVE
                          PERFORM 170-EXIBE-DADOS THRU 170-99-FIM
                          GO TO 140-LER-CWCONF
                     ELSE
                          IF FS-CWCONF = "10"
                          OR (CWCONF-TIPO NOT EQUAL "94")
                             MOVE 1 TO FIM
                             IF PAGE-UP
                                SET PAGE-UP-OFF TO TRUE
                             END-IF
                             IF PAGE-DOWN
                                SET PAGE-DOWN-OFF TO TRUE
                             END-IF
                          END-IF
                          MOVE ZERO TO OK
                          MOVE "44" TO FS-CWCONF
                          INITIALIZE CWCONF-REG94
                          PERFORM 170-EXIBE-DADOS THRU 170-99-FIM
                     END-IF
                ELSE
                     MOVE NOME TO CWCONF-NAME-REPORT
                     PERFORM TEST AFTER UNTIL FS-CWCONF NOT = "9D"
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
                IF   FS-CWCONF < "10"
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
300715                    MOVE SPACES TO CWCONF-DADOS
                          MOVE NOME TO CWCONF-NAME-REPORT
                          SET CWSQLC-WRITE TO TRUE
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
                          MOVE "00" TO FS-CWCONF
                          PERFORM 170-EXIBE-DADOS THRU 170-99-FIM
                     END-IF
                END-IF
           ELSE
                MOVE ZERO   TO FS-CWCONF
                MOVE SPACES TO COMANDO
                               FUNCAO
           END-IF

           IF   CWCONF-RELATORIO EQUAL SPACES
                MOVE "44" TO FS-CWCONF
           END-IF.

       140-99-FIM. EXIT.

       160-CHECK-COMANDO.

           COPY CWEFAB.

       160-99-FIM. EXIT.

       170-EXIBE-DADOS.

           MOVE CWCONF-KEEP  TO CWCHECK-KEEP
           MOVE CWCONF-QUIET TO CWCHECK-QUIET
           IF   CWCONF-PROGRAMA-PRINTER = X"0F00" OR X"000F"
                MOVE "15" TO CWCONF-PROGRAMA-PRINTER
           END-IF
           IF   CWCONF-DESPROGRAMA = X"1200" OR X"0012"
                MOVE "18" TO CWCONF-DESPROGRAMA
           END-IF

           INSPECT CWCONF-PROGRAMA-PRINTER
                   CONVERTING X"00" TO SPACE

           INSPECT CWCONF-DESPROGRAMA
                   CONVERTING X"00" TO SPACE

           DISPLAY CWMEN7E
           DISPLAY CWMEN7B.

       170-99-FIM. EXIT.

       800-INICIAIS.

           DISPLAY "CWFOOTLINE"  UPON ENVIRONMENT-NAME
           ACCEPT   footline-t   FROM ENVIRONMENT-VALUE
           SET CWSQLC-UPDATE TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           IF   FS-CWCONF NOT EQUAL "00"
                CALL "CWCONF" USING "ISAM"
                EXIT PROGRAM
           END-IF

           CALL  "CWGETU"   USING OPERADOR
                                  TASK
                                  PROGRAMA
                                  '?'
           MOVE "Y"            TO CWBOXS-ERASE
           MOVE 11             TO CWBOXS-LINE
                                  CWBOXS-COLUMN
           MOVE 1              TO CWBOXS-OPTION
           MOVE SPACES         TO CWBOXS-ITENS
           MOVE "Configurar"   TO CWBOXS-TITLE
           MOVE "Acesso geral" TO CWBOXS-TEXT   (1)
                                  CWBOXS-CHAR   (1)
           MOVE "Usu†rio"      TO CWBOXS-TEXT   (2)
                                  CWBOXS-CHAR   (2)
           MOVE OPERADOR       TO CWBOXS-TEXT   (2) (9: )
           CALL "CWBOXS"  USING PARAMETROS-CWBOXS

           EVALUATE CWBOXS-OPTION
                    WHEN 0
                         SET CWSQLC-CLOSE TO TRUE
                         CALL "CWCONF" USING CWSQLC
                                             CWCONF-REG
                                             FS-CWCONF
                                             KCO PCO
                         EXIT PROGRAM
                    WHEN 1
                         MOVE SPACES   TO NOME
                         DISPLAY (7, 3) "Acesso geral"
                    WHEN 2
                         MOVE OPERADOR TO NOME
                         DISPLAY (7, 3) "Somente do usu†rio " NOME
           END-EVALUATE

           MOVE "PS"     TO CWCONF-REGPS
           MOVE OPERADOR TO CWCONF-NOME
           SET CWSQLC-READ TO TRUE
           SET CWSQLC-EQUAL TO TRUE
           SET CWSQLC-IGNORE-LOCK TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO

           MOVE CWCONF-GRUPO         TO GRUPO
           MOVE "94" TO CWCONF-REG94.

       800-99-FIM. EXIT.

       900-FINAIS.

           SET CWSQLC-CLOSE TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO.

       900-99-FIM. EXIT.

       END PROGRAM CWMEN7.
