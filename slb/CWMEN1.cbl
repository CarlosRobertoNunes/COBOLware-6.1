       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWMEN1.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  06/05/1990.
       SECURITY.      *************************************************
                      *                                               *
                      *   Manutencao de labels de arquivos            *
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
           05 MINUSCULAS PIC X(26) VALUE "abcdefghijklmnopqrstuvwxyz".
           05 MAIUSCULAS PIC X(26) VALUE "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
           05 FIM                      PIC  9(001) VALUE ZERO.
           05 OK                       PIC  9(001) VALUE ZERO.
           05 OTHER-PROGRAM            PIC  X(255) VALUE SPACES.
           05 LINHA-BRANCA             PIC  X(040) VALUE SPACES.
           05 TECLA                    PIC  9(003) VALUE 0. COPY CWKEYS.
           05 FL-EXIT                  PIC  9(001) VALUE 1.
           05 SALVA-CHAVE              PIC  X(032) VALUE SPACES.
           05 MENSAGEM-ERRO            PIC  X(030) VALUE SPACES.
              88 SEM-ERRO                          VALUE SPACES.
           05 MENSAGENS-DE-ERRO.
              10 F PIC X(30) VALUE "Entre com os dados            ".
              10 F PIC X(30) VALUE "Arquivo n∆o cadastrado        ".
              10 F PIC X(30) VALUE "Confirme exclus∆o             ".
              10 F PIC X(30) VALUE "Arquivo j† cadastrado         ".
              10 F PIC X(30) VALUE "Label em branco               ".
           05 FILLER REDEFINES MENSAGENS-DE-ERRO.
              10 MSG OCCURS 5 PIC X(30).

       COPY CWFUNC.
       COPY CWCONF.

       SCREEN SECTION.

       01  CWMEN1A.
           05 LINE 08 COLUMN 03 VALUE "Arquivo".
           05 LINE 10 COLUMN 03 VALUE "Label  ".

       01  CWMEN1B AUTO.
           05 LINE 10 COLUMN 11 PIC   X(051) USING CWCONF-LABEL-1.
           05 LINE 11 COLUMN 11 PIC   X(051) USING CWCONF-LABEL-2.
           05 LINE 12 COLUMN 11 PIC   X(051) USING CWCONF-LABEL-3.
           05 LINE 13 COLUMN 11 PIC   X(051) USING CWCONF-LABEL-4.
           05 LINE 14 COLUMN 11 PIC   X(051) USING CWCONF-LABEL-5.

       01  CWMEN1E AUTO.
           05 LINE 08 COLUMN 11 PIC   X(030) USING CWCONF-ARQUIVO.

       PROCEDURE DIVISION.

       000-INICIO.

           DISPLAY 'CWMEN1' UPON ENVIRONMENT-NAME
           ACCEPT OTHER-PROGRAM FROM ENVIRONMENT-VALUE
           IF   OTHER-PROGRAM NOT = SPACES
                CALL OTHER-PROGRAM
                ON EXCEPTION CONTINUE
                NOT ON EXCEPTION
                    CANCEL OTHER-PROGRAM
                    GOBACK
           END-IF
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
                     DISPLAY CWMEN1A
                END-IF.

           MOVE "23" TO FS-CWCONF

           IF   NOT FINALIZAR
                IF   INCLUSAO
                     MOVE SPACES TO CWCONF-LABEL
                END-IF
                MOVE SPACES TO MENSAGEM-ERRO
                PERFORM 170-EXIBE-DADOS THRU 170-99-FIM
                PERFORM 140-LER-CWCONF  THRU 140-99-FIM
                        UNTIL (FS-CWCONF EQUAL "00")
                        OR    (FL-EXIT   EQUAL 1)
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

           IF   FS-CWCONF EQUAL "02"
                MOVE "00" TO FS-CWCONF
           END-IF

           SET CWSQLC-UNLOCK TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO.


       100-99-FIM. EXIT.

       130-CRITICA.
u
           MOVE    SPACES     TO MENSAGEM-ERRO

           ACCEPT  CWMEN1B
           ACCEPT TECLA FROM ESCAPE KEY
           IF ESC
              SET ABORTAR TO TRUE
              EXIT PARAGRAPH
           END-IF
           PERFORM 170-EXIBE-DADOS THRU 170-99-FIM

           IF   CWCONF-LABEL EQUAL SPACES
                MOVE MSG(5) TO MENSAGEM-ERRO
                EXEC COBOLware Send
                     Message MENSAGEM-ERRO
                END-EXEC
           ELSE
                MOVE    SPACE               TO COMANDO
                PERFORM 160-CHECK-COMANDO THRU 160-99-FIM
           END-IF.


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
                     EXEC COBOLware OBJECT (COMBO-BOX)
                               LINE 08 HEIGHT 10 COLUMN 11 WIDTH 30
                               PROGRAM "CWMENF" WORK-AREA "02"
                               OPTION CWCONF-ARQUIVO ORDER 1 RETURN 1
                               STRING-1-LENGTH 30
                     END-EXEC
                     COPY CWPGON REPLACING 23 BY footline.
                END-IF
           END-IF

           MOVE SPACES TO MENSAGEM-ERRO
           MOVE "02"   TO CWCONF-TIPO

           COPY CWESCP REPLACING 23 BY footline.
           MOVE 0 TO TECLA
                     FIM

           PERFORM TEST AFTER
                   UNTIL(NOT (PAGE-UP   AND PAGE-UP-OFF))
                     AND(NOT (PAGE-DOWN AND PAGE-DOWN-OFF))
                   ACCEPT CWMEN1E
                   ACCEPT TECLA FROM ESCAPE KEY
           END-PERFORM
           EXEC COBOLware OBJECT DROP END-EXEC

           IF  ESC
               MOVE 1 TO FL-EXIT
           ELSE
               COPY CWESCP REPLACING 23 BY footline.
               IF   ALTERACAO OR INCLUSAO
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

           INSPECT CWCONF-ARQUIVO CONVERTING MINUSCULAS TO MAIUSCULAS
           DISPLAY CWMEN1E

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
                             MOVE "02" TO CWCONF-CHAVE(1:2)
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
                             UNTIL (CWCONF-ARQUIVO NOT EQUAL "PRN"
                               AND  CWCONF-ARQUIVO NOT EQUAL SPACES)
                                OR  FS-CWCONF      >         "09"
                                OR  CWCONF-TIPO    NOT EQUAL "02"
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
                                     AND CWCONF-TIPO EQUAL "02"
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
                                     AND CWCONF-TIPO EQUAL "02"
                                     AND PAGE-DOWN-OFF
                                         SET PAGE-DOWN-ON TO TRUE
                                     END-IF
                                  END-PERFORM
                             END-EVALUATE
                     END-PERFORM
                     IF   CWCONF-TIPO EQUAL "02"
                     AND  FS-CWCONF EQUAL "00"
                          SET READY-ON TO TRUE
                          MOVE CWCONF-CHAVE TO SALVA-CHAVE
                          PERFORM 170-EXIBE-DADOS THRU 170-99-FIM
                          GO TO 140-LER-CWCONF
                     ELSE
                          IF FS-CWCONF = "10"
                          OR (CWCONF-TIPO NOT EQUAL "02")
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
                          INITIALIZE CWCONF-REG02
                          PERFORM 170-EXIBE-DADOS THRU 170-99-FIM
                     END-IF
                ELSE
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

           IF   CWCONF-ARQUIVO EQUAL SPACES
                MOVE "44" TO FS-CWCONF
           END-IF.

       140-99-FIM. EXIT.

       160-CHECK-COMANDO.

           COPY CWEFAB.

       160-99-FIM. EXIT.

       170-EXIBE-DADOS.

           DISPLAY CWMEN1B.

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

           MOVE "02" TO CWCONF-REG02.

       800-99-FIM. EXIT.

       900-FINAIS.

           SET CWSQLC-CLOSE TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO.

       900-99-FIM. EXIT.

       END PROGRAM CWMEN1.

