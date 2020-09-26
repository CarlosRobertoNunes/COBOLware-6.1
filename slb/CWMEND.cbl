       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWMEND.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  01/07/1996.
       SECURITY.      *************************************************
                      *                                               *
                      *   Manutencao de jobs                          *
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
           05 OBJ                      PIC  X(001) VALUE SPACE.
           05 CAMPO                    PIC  9(002) VALUE 0.
           05 MINUSCULAS PIC X(26) VALUE "abcdefghijklmnopqrstuvwxyz".
           05 MAIUSCULAS PIC X(26) VALUE "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
           05 MEU-GRUPO                PIC  X(022) VALUE SPACES.
           05 OK                       PIC  9(001) VALUE ZERO.
           05 LINHA-BRANCA             PIC  X(040) VALUE SPACES.
           05 TECLA                    PIC  9(003) VALUE 0. COPY CWKEYS.
           05 I                        PIC  9(002) VALUE 1.
           05 Y                        PIC  9(002) VALUE ZERO.
           05 FL-EXIT                  PIC  9(001) VALUE 1.
           05 SALVA-CHAVE              PIC  X(032) VALUE SPACES.
           05 MENSAGEM-ERRO            PIC  X(030) VALUE SPACES.
              88 SEM-ERRO                          VALUE SPACES.

       01  AREAS-DE-TRABALHO-2.
           05 MENSAGENS-DE-ERRO.
              10 F PIC X(30) VALUE "Entre com os dados            ".
              10 F PIC X(30) VALUE "Job n∆o cadastrado            ".
              10 F PIC X(30) VALUE "Confirme exclus∆o             ".
              10 F PIC X(30) VALUE "Job j† cadastrado             ".
              10 F PIC X(30) VALUE "Informe m¢dulo carga          ".
              10 F PIC X(30) VALUE "$pppp,tt,x(40)                ".
              10 F PIC X(30) VALUE "                              ".
              10 F PIC X(30) VALUE "                              ".
           05 FILLER REDEFINES MENSAGENS-DE-ERRO.
              10 MSG OCCURS 8 PIC X(30).

       COPY CWBOXS.
       COPY CWMOUS.
       COPY CWFUNC.
       COPY CWSEND.
       COPY CWCONF.

       SCREEN SECTION.

       01  CWMENDA.
           05 LINE 08 COLUMN 03 VALUE "Job".
           05 LINE 10 COLUMN 03 VALUE "Execut†vel".
           05 LINE 12 COLUMN 03 VALUE "Tipo".
           05 LINE 14 COLUMN 03 VALUE "ParÉmetros".
           05 LINE 16 COLUMN 03 VALUE "Pr¢ximo".
           05 LINE 16 COLUMN 10 VALUE " job (se ok)".
           05 LINE 16 COLUMN 31 VALUE "(com erro)".
           05 LINE 18 COLUMN 03 VALUE "Mensagem".

       01  CWMENDB AUTO.
           05 LINE 10 COLUMN 14 PIC X(050) USING CWCONF-JOB-MODULO.
           05 LINE 12 COLUMN 14 PIC X(007) USING CWCONF-JOB-TIPO.
           05 LINE 14 COLUMN 14 PIC X(050) USING CWCONF-JOB-PARAMETRO.
           05 LINE 16 COLUMN 23 PIC X(007)
              USING CWCONF-JOB-PROXIMO-RC-OK.
           05 LINE 16 COLUMN 42 PIC X(007)
              USING CWCONF-JOB-PROXIMO-NAO-OK.
           05 LINE 18 COLUMN 14 PIC X(050) USING CWCONF-JOB-MENSAGEM.

       01  CWMENDE AUTO.
           05 LINE 08 COLUMN 14 PIC X(007) USING CWCONF-JOB.

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
                     DISPLAY CWMENDA
                END-IF.

           MOVE "23" TO FS-CWCONF

           IF   NOT FINALIZAR
                IF CWCONF-TIPO NOT = 'JB'
                   INITIALIZE CWCONF-REGJB
                   MOVE "JB" TO CWCONF-TIPO
                END-IF
                IF   NOT CONSULTA
                     PERFORM 170-EXIBE-DADOS THRU 170-99-FIM
                END-IF
                PERFORM 140-LER-CWCONF  THRU 140-99-FIM
                        UNTIL FS-CWCONF EQUAL "00"
                        OR    FL-EXIT EQUAL 1
                CALL "CWMSGW" USING "230340" LINHA-BRANCA
                IF  (INCLUSAO OR ALTERACAO)
                AND  FL-EXIT NOT EQUAL 1
                     MOVE    SPACE              TO COMANDO
                     MOVE    MSG (1)            TO MENSAGEM-ERRO
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
                          PERFORM 160-CHECK-COMANDO THRU 160-99-FIM
                     END-IF
                END-IF
           END-IF

           MOVE ZERO TO FS-CWCONF

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

           IF   FS-CWCONF EQUAL "JB"
                MOVE "00" TO FS-CWCONF
           END-IF.

       100-99-FIM. EXIT.

       130-CRITICA.

           MOVE SPACES TO MENSAGEM-ERRO

           ACCEPT CWMENDB
           IF   CWCONF-JOB-MODULO = SPACES
                MOVE MSG(5) TO MENSAGEM-ERRO
           END-IF
           IF   CWCONF-JOB-PARAMETRO(1: 1) = "$"
           AND CWCONF-JOB-COBOL
           AND(CWCONF-JOB-PARAMETRO(2: 4) NOT NUMERIC
            OR CWCONF-JOB-PARAMETRO(6: 1) NOT = ","
            OR CWCONF-JOB-PARAMETRO(7: 2) NOT NUMERIC
            OR CWCONF-JOB-PARAMETRO(9: 1) NOT = ",")
               MOVE MSG(6) TO MENSAGEM-ERRO
           END-IF
           IF   ESC
                MOVE 1   TO FL-EXIT
                IF  MENSAGEM-ERRO NOT = SPACES
                    SET ABORTAR TO TRUE
                    GO TO 130-99-FIM
                END-IF
           END-IF
           IF  MENSAGEM-ERRO NOT = SPACES
               EXEC COBOLware SEND
                    MSG MENSAGEM-ERRO
               END-EXEC
           ELSE
               PERFORM 160-CHECK-COMANDO THRU 160-99-FIM
           END-IF.

       130-99-FIM.  EXIT.

       140-LER-CWCONF.

           EXEC COBOLware OBJECT DROP END-EXEC
           MOVE SPACE TO OBJ
           EXEC COBOLware SEND
                MSG MENSAGEM-ERRO
           END-EXEC
           MOVE SPACES TO MENSAGEM-ERRO

           IF   NOT INCLUSAO
                EXEC COBOLware OBJECT COMBO-BOX NOEDIT
                          LINE 08 COLUMN 14 HEIGHT 10 WIDTH 7
                          PROGRAM "CWMENF" WORK-AREA "JB"
                          OPTION CWCONF-JOB ORDER 1 RETURN 1
                          STRING-1-LENGTH 7
                END-EXEC
                COPY CWPGON REPLACING 23 BY footline.
           END-IF

           COPY CWESCP REPLACING 23 BY footline.

           MOVE 0 TO TECLA

           PERFORM TEST AFTER
                   UNTIL(NOT (PAGE-UP   AND PAGE-UP-OFF))
                     AND(NOT (PAGE-DOWN AND PAGE-DOWN-OFF))
                   ACCEPT CWMENDE
                   ACCEPT TECLA FROM ESCAPE KEY
           END-PERFORM
      *    display CWMENDE
           INSPECT CWCONF-JOB CONVERTING MINUSCULAS TO MAIUSCULAS
           EXEC COBOLware OBJECT DROP END-EXEC
           COPY CWESCP REPLACING 23 BY footline.

           IF  ESC
               MOVE 1 TO FL-EXIT
           ELSE
               EXEC COBOLware OBJECT COMBO-BOX NOEDIT
                    LINE 12 COLUMN 14 Caption "Tipos"
                            WIDTH 7 HEIGHT 3
                            STRING-1(1) "1" STRING-2(1) "Bin†rio"
                            STRING-1(2) "2" STRING-2(2) "COBOL"
                            STRING-1(3) "3" STRING-2(3) "Windows"
                            STRING-2-LENGTH 7 RETURN 1 ORDER 2
                            OPTION CWCONF-JOB-TIPO
               END-EXEC
               MOVE "1" TO OBJ
               IF   ALTERACAO OR INCLUSAO
                    EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                              LINE footline COLUMN 02 WIDTH 10
                              CAPTION " ~Concluir "
                              KEY F3
                    END-EXEC
               END-IF
           END-IF

           MOVE "JB" TO CWCONF-TIPO

           MOVE SPACES        TO CWSEND-SCREENS
           MOVE MENSAGEM-ERRO TO CWSEND-MSG
           CALL "CWSEND" USING PARAMETROS-CWSEND

           IF   CWCONF-JOB EQUAL SPACES
           AND  NOT (PAGE-UP OR PAGE-DOWN)
                MOVE "44" TO FS-CWCONF
           ELSE
           IF   FL-EXIT NOT EQUAL 1
                IF   (PAGE-UP OR PAGE-DOWN)
                AND  NOT INCLUSAO
                AND  OK EQUAL ZERO
                     MOVE 1    TO OK
                     PERFORM TEST AFTER UNTIL FS-CWCONF NOT = "9D"
                             IF PAGE-DOWN
                                SET CWSQLC-NOT-LESS TO TRUE
                                MOVE LOW-VALUES TO CWCONF-CHAVE
                             END-IF
                             IF PAGE-UP
                                SET CWSQLC-NOT-GREATER TO TRUE
                                MOVE HIGH-VALUES TO CWCONF-CHAVE
                             END-IF
                             SET CWSQLC-START TO TRUE
                             MOVE "JB" TO CWCONF-CHAVE(1:2)
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
                             UNTIL (CWCONF-JOB NOT EQUAL SPACES)
                                OR (FS-CWCONF NOT EQUAL "00")
                                OR (CWCONF-TIPO NOT EQUAL "JB")
                             EVALUATE TRUE
                             WHEN PAGE-DOWN
                                  PERFORM TEST AFTER
                                          UNTIL FS-CWCONF NOT = "9D"
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
                                     AND CWCONF-TIPO EQUAL "JB"
                                     AND PAGE-UP-OFF
                                         SET PAGE-UP-ON TO TRUE
                                     END-IF
                                  END-PERFORM
                             WHEN PAGE-UP
                                  PERFORM TEST AFTER
                                          UNTIL FS-CWCONF NOT = "9D"
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
                                     AND CWCONF-TIPO EQUAL "JB"
                                     AND PAGE-DOWN-OFF
                                         SET PAGE-DOWN-ON TO TRUE
                                     END-IF
                                  END-PERFORM
                             END-EVALUATE
                     END-PERFORM
                     IF   CWCONF-TIPO EQUAL "JB"
                     AND  FS-CWCONF < "10"
                          MOVE CWCONF-CHAVE TO SALVA-CHAVE
                          PERFORM 170-EXIBE-DADOS THRU 170-99-FIM
                          GO TO 140-LER-CWCONF
                     ELSE
=                         MOVE ZERO TO OK
                          MOVE "JB" TO CWCONF-REGJB
                          MOVE "10" TO FS-CWCONF
                     END-IF
                ELSE
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
                END-IF
                IF FS-CWCONF = "10"
                OR (CWCONF-TIPO NOT EQUAL "JB")
                   IF PAGE-UP
                      SET PAGE-UP-OFF TO TRUE
                   END-IF
                   IF PAGE-DOWN
                      SET PAGE-DOWN-OFF TO TRUE
                   END-IF
2807               MOVE SALVA-CHAVE TO CWCONF-CHAVE
                   SET CWSQLC-READ  TO TRUE
                   SET CWSQLC-EQUAL TO TRUE
                   CALL "CWCONF" USING CWSQLC
                                       CWCONF-REG
                                       FS-CWCONF
                                       KCO PCO
                    MOVE "10" TO FS-CWCONF
                END-IF
                IF   FS-CWCONF < '10'
                     PERFORM 170-EXIBE-DADOS THRU 170-99-FIM
                     IF   INCLUSAO
                          MOVE MSG (4) TO MENSAGEM-ERRO
                          MOVE "44" TO FS-CWCONF
                     END-IF
                ELSE
                     IF   INCLUSAO
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
           END-IF
           END-IF.

       140-99-FIM. EXIT.

       160-CHECK-COMANDO.

           COPY CWEFAB.

       160-99-FIM. EXIT.

       170-EXIBE-DADOS.

           IF  NOT CWCONF-JOB-TIPO-OK
               MOVE OBJ TO CWCONF-JOB-TIPO
           END-IF
           DISPLAY CWMENDE
                   CWMENDB.

       170-99-FIM. EXIT.

       800-INICIAIS.

           DISPLAY "CWFOOTLINE"  UPON ENVIRONMENT-NAME
           ACCEPT   footline-t   FROM ENVIRONMENT-VALUE
           SET CWSQLC-UPDATE TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO.
           IF   FS-CWCONF NOT EQUAL "00"
                CALL "CWCONF" USING "ISAM"
                GOBACK
           END-IF.

       800-99-FIM. EXIT.

       900-FINAIS.

           SET CWSQLC-CLOSE TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO.

       900-99-FIM. EXIT.

       END PROGRAM CWMEND.

