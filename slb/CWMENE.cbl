       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWMENE.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  16/07/2000.
       SECURITY.      *************************************************
                      *                                               *
                      *   Manutencao de parametros genericos          *
                      *                                               *
                      *   Modulo de manutencao de Estilos             *
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
           05 OK                       PIC  9(001) VALUE ZERO.
           05 F1-ON                    PIC  9(001) VALUE ZERO.
           05 F2-ON                    PIC  9(001) VALUE ZERO.
           05 TECLA                    PIC  9(003) VALUE 0. COPY CWKEYS.
           05 FL-EXIT                  PIC  9(001) VALUE 1.
           05 SALVA-CHAVE              PIC  X(032) VALUE SPACES.
           05 MENSAGEM-ERRO            PIC  X(030) VALUE SPACES.
              88 SEM-ERRO                          VALUE SPACES.
           05 a                        PIC  9(002) VALUE 01.
           05 b                        PIC  9(002) VALUE 02.
           05 c                        PIC  9(002) VALUE 03.
           05 d                        PIC  9(002) VALUE 04.
           05 e                        PIC  9(002) VALUE 05.
           05 f                        PIC  9(002) VALUE 06.
           05 g                        PIC  9(002) VALUE 07.
           05 h                        PIC  9(002) VALUE 08.
           05 i                        PIC  9(002) VALUE 09.
           05 j                        PIC  9(002) VALUE 10.
           05 k                        PIC  9(002) VALUE 11.
           05 l                        PIC  9(002) VALUE 12.
           05 m                        PIC  9(002) VALUE 13.
           05 n                        PIC  9(002) VALUE 14.
           05 o                        PIC  9(002) VALUE 15.
           05 p                        PIC  9(002) VALUE 16.

       01  AREAS-DE-TRABALHO-2.
           05 MENSAGENS-DE-ERRO.
              10 PIC X(30) VALUE "Entre com os dados            ".
              10 PIC X(30) VALUE "Estilo nÆo cadastrado         ".
              10 PIC X(30) VALUE "Confirme exclusÆo             ".
              10 PIC X(30) VALUE "Estilo j  cadastrado          ".
              10 PIC X(30) VALUE "Estilo importado              ".
           05 REDEFINES MENSAGENS-DE-ERRO.
              10 MSG OCCURS 5 PIC X(30).

       COPY CWBOXS.
       COPY CWBOXF.
       COPY CWFUNC.
       COPY CWSEND.
       COPY CWCONF.

       SCREEN SECTION.

       01  CWMENEA.
           05 LINE 08 COLUMN 04 VALUE "Estilo".

           05 LINE 10 COLUMN 04 VALUE '\a 6/"'.
           05 LINE 11 COLUMN 04 VALUE '\b 8/"'.
           05 LINE 12 COLUMN 04 VALUE "\m Microavan‡o".

           05 LINE 10 COLUMN 44 VALUE "\c Azul".
           05 LINE 11 COLUMN 44 VALUE "\o Verde".
           05 LINE 12 COLUMN 44 VALUE "\p Vermelho".

           05 LINE 14 COLUMN 04 VALUE "\d Carta".
           05 LINE 15 COLUMN 04 VALUE "\e Condensar".
           05 LINE 16 COLUMN 04 VALUE "\f Draft".
           05 LINE 17 COLUMN 04 VALUE "\g Expandir".
           05 LINE 18 COLUMN 04 VALUE "\h Italico".
           05 LINE 19 COLUMN 04 VALUE "\i Negrito".
           05 LINE 20 COLUMN 04 VALUE "\n Sublinhar".

           05 LINE 14 COLUMN 44 VALUE "\j Normal".
           05 LINE 15 COLUMN 44 VALUE "\l Reset".
           05 LINE 20 COLUMN 44 VALUE "\k Paisagem".

       01  CHAVE.
           05 LINE 08 COLUMN 11 PIC X(030) USING CWCONF-ARQUIVO.

       01  CTAC-VAR-CWMENE.
           05 LINE 10 COLUMN 19 PIC Z(003) USING CWCONF-ASC (a 01).
           05 LINE 10 COLUMN 23 PIC Z(003) USING CWCONF-ASC (a 02).
           05 LINE 10 COLUMN 27 PIC Z(003) USING CWCONF-ASC (a 03).
           05 LINE 10 COLUMN 31 PIC Z(003) USING CWCONF-ASC (a 04).
           05 LINE 10 COLUMN 35 PIC Z(003) USING CWCONF-ASC (a 05).
           05 LINE 10 COLUMN 39 PIC Z(003) USING CWCONF-ASC (a 06).
           05 LINE 11 COLUMN 19 PIC Z(003) USING CWCONF-ASC (b 01).
           05 LINE 11 COLUMN 23 PIC Z(003) USING CWCONF-ASC (b 02).
           05 LINE 11 COLUMN 27 PIC Z(003) USING CWCONF-ASC (b 03).
           05 LINE 11 COLUMN 31 PIC Z(003) USING CWCONF-ASC (b 04).
           05 LINE 11 COLUMN 35 PIC Z(003) USING CWCONF-ASC (b 05).
           05 LINE 11 COLUMN 39 PIC Z(003) USING CWCONF-ASC (b 06).
           05 LINE 12 COLUMN 19 PIC Z(003) USING CWCONF-ASC (m 01).
           05 LINE 12 COLUMN 23 PIC Z(003) USING CWCONF-ASC (m 02).
           05 LINE 12 COLUMN 27 PIC Z(003) USING CWCONF-ASC (m 03).
           05 LINE 12 COLUMN 31 PIC Z(003) USING CWCONF-ASC (m 04).
           05 LINE 12 COLUMN 35 PIC Z(003) USING CWCONF-ASC (m 05).
           05 LINE 12 COLUMN 39 PIC Z(003) USING CWCONF-ASC (m 06).

           05 LINE 14 COLUMN 19 PIC Z(003) USING CWCONF-ASC (d 01).
           05 LINE 14 COLUMN 23 PIC Z(003) USING CWCONF-ASC (d 02).
           05 LINE 14 COLUMN 27 PIC Z(003) USING CWCONF-ASC (d 03).
           05 LINE 14 COLUMN 31 PIC Z(003) USING CWCONF-ASC (d 04).
           05 LINE 14 COLUMN 35 PIC Z(003) USING CWCONF-ASC (d 05).
           05 LINE 14 COLUMN 39 PIC Z(003) USING CWCONF-ASC (d 06).
           05 LINE 15 COLUMN 19 PIC Z(003) USING CWCONF-ASC (e 01).
           05 LINE 15 COLUMN 23 PIC Z(003) USING CWCONF-ASC (e 02).
           05 LINE 15 COLUMN 27 PIC Z(003) USING CWCONF-ASC (e 03).
           05 LINE 15 COLUMN 31 PIC Z(003) USING CWCONF-ASC (e 04).
           05 LINE 15 COLUMN 35 PIC Z(003) USING CWCONF-ASC (e 05).
           05 LINE 15 COLUMN 39 PIC Z(003) USING CWCONF-ASC (e 06).
           05 LINE 16 COLUMN 19 PIC Z(003) USING CWCONF-ASC (f 01).
           05 LINE 16 COLUMN 23 PIC Z(003) USING CWCONF-ASC (f 02).
           05 LINE 16 COLUMN 27 PIC Z(003) USING CWCONF-ASC (f 03).
           05 LINE 16 COLUMN 31 PIC Z(003) USING CWCONF-ASC (f 04).
           05 LINE 16 COLUMN 35 PIC Z(003) USING CWCONF-ASC (f 05).
           05 LINE 16 COLUMN 39 PIC Z(003) USING CWCONF-ASC (f 06).
           05 LINE 17 COLUMN 19 PIC Z(003) USING CWCONF-ASC (g 01).
           05 LINE 17 COLUMN 23 PIC Z(003) USING CWCONF-ASC (g 02).
           05 LINE 17 COLUMN 27 PIC Z(003) USING CWCONF-ASC (g 03).
           05 LINE 17 COLUMN 31 PIC Z(003) USING CWCONF-ASC (g 04).
           05 LINE 17 COLUMN 35 PIC Z(003) USING CWCONF-ASC (g 05).
           05 LINE 17 COLUMN 39 PIC Z(003) USING CWCONF-ASC (g 06).
           05 LINE 18 COLUMN 19 PIC Z(003) USING CWCONF-ASC (h 01).
           05 LINE 18 COLUMN 23 PIC Z(003) USING CWCONF-ASC (h 02).
           05 LINE 18 COLUMN 27 PIC Z(003) USING CWCONF-ASC (h 03).
           05 LINE 18 COLUMN 31 PIC Z(003) USING CWCONF-ASC (h 04).
           05 LINE 18 COLUMN 35 PIC Z(003) USING CWCONF-ASC (h 05).
           05 LINE 18 COLUMN 39 PIC Z(003) USING CWCONF-ASC (h 06).
           05 LINE 19 COLUMN 19 PIC Z(003) USING CWCONF-ASC (i 01).
           05 LINE 19 COLUMN 23 PIC Z(003) USING CWCONF-ASC (i 02).
           05 LINE 19 COLUMN 27 PIC Z(003) USING CWCONF-ASC (i 03).
           05 LINE 19 COLUMN 31 PIC Z(003) USING CWCONF-ASC (i 04).
           05 LINE 19 COLUMN 35 PIC Z(003) USING CWCONF-ASC (i 05).
           05 LINE 19 COLUMN 39 PIC Z(003) USING CWCONF-ASC (i 06).
           05 LINE 20 COLUMN 19 PIC Z(003) USING CWCONF-ASC (n 01).
           05 LINE 20 COLUMN 23 PIC Z(003) USING CWCONF-ASC (n 02).
           05 LINE 20 COLUMN 27 PIC Z(003) USING CWCONF-ASC (n 03).
           05 LINE 20 COLUMN 31 PIC Z(003) USING CWCONF-ASC (n 04).
           05 LINE 20 COLUMN 35 PIC Z(003) USING CWCONF-ASC (n 05).
           05 LINE 20 COLUMN 39 PIC Z(003) USING CWCONF-ASC (n 06).

           05 LINE 10 COLUMN 56 PIC Z(003) USING CWCONF-ASC (c 01).
           05 LINE 10 COLUMN 60 PIC Z(003) USING CWCONF-ASC (c 02).
           05 LINE 10 COLUMN 64 PIC Z(003) USING CWCONF-ASC (c 03).
           05 LINE 10 COLUMN 68 PIC Z(003) USING CWCONF-ASC (c 04).
           05 LINE 10 COLUMN 72 PIC Z(003) USING CWCONF-ASC (c 05).
           05 LINE 10 COLUMN 76 PIC Z(003) USING CWCONF-ASC (c 06).
           05 LINE 11 COLUMN 56 PIC Z(003) USING CWCONF-ASC (o 01).
           05 LINE 11 COLUMN 60 PIC Z(003) USING CWCONF-ASC (o 02).
           05 LINE 11 COLUMN 64 PIC Z(003) USING CWCONF-ASC (o 03).
           05 LINE 11 COLUMN 68 PIC Z(003) USING CWCONF-ASC (o 04).
           05 LINE 11 COLUMN 72 PIC Z(003) USING CWCONF-ASC (o 05).
           05 LINE 11 COLUMN 76 PIC Z(003) USING CWCONF-ASC (o 06).
           05 LINE 12 COLUMN 56 PIC Z(003) USING CWCONF-ASC (p 01).
           05 LINE 12 COLUMN 60 PIC Z(003) USING CWCONF-ASC (p 02).
           05 LINE 12 COLUMN 64 PIC Z(003) USING CWCONF-ASC (p 03).
           05 LINE 12 COLUMN 68 PIC Z(003) USING CWCONF-ASC (p 04).
           05 LINE 12 COLUMN 72 PIC Z(003) USING CWCONF-ASC (p 05).
           05 LINE 12 COLUMN 76 PIC Z(003) USING CWCONF-ASC (p 06).

           05 LINE 14 COLUMN 56 PIC Z(003) USING CWCONF-ASC (j 01).
           05 LINE 14 COLUMN 60 PIC Z(003) USING CWCONF-ASC (j 02).
           05 LINE 14 COLUMN 64 PIC Z(003) USING CWCONF-ASC (j 03).
           05 LINE 14 COLUMN 68 PIC Z(003) USING CWCONF-ASC (j 04).
           05 LINE 14 COLUMN 72 PIC Z(003) USING CWCONF-ASC (j 05).
           05 LINE 14 COLUMN 76 PIC Z(003) USING CWCONF-ASC (j 06).
           05 LINE 15 COLUMN 56 PIC Z(003) USING CWCONF-ASC (l 01).
           05 LINE 15 COLUMN 60 PIC Z(003) USING CWCONF-ASC (l 02).
           05 LINE 15 COLUMN 64 PIC Z(003) USING CWCONF-ASC (l 03).
           05 LINE 15 COLUMN 68 PIC Z(003) USING CWCONF-ASC (l 04).
           05 LINE 15 COLUMN 72 PIC Z(003) USING CWCONF-ASC (l 05).
           05 LINE 15 COLUMN 76 PIC Z(003) USING CWCONF-ASC (l 06).
           05 LINE 20 COLUMN 56 PIC Z(003) USING CWCONF-ASC (k 01).
           05 LINE 20 COLUMN 60 PIC Z(003) USING CWCONF-ASC (k 02).
           05 LINE 20 COLUMN 64 PIC Z(003) USING CWCONF-ASC (k 03).
           05 LINE 20 COLUMN 68 PIC Z(003) USING CWCONF-ASC (k 04).
           05 LINE 20 COLUMN 72 PIC Z(003) USING CWCONF-ASC (k 05).
           05 LINE 20 COLUMN 76 PIC Z(003) USING CWCONF-ASC (k 06).

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
                     DISPLAY CWMENEA
                END-IF.

           MOVE "11" TO FS-CWCONF

           IF   NOT FINALIZAR
                MOVE 4          TO CWBOXS-OPTION
                IF  INCLUSAO
                OR  FS-CWCONF > "11"
                    MOVE ALL "0" TO CWCONF-ESTILOS
                END-IF
                PERFORM 170-EXIBE-DADOS THRU 170-99-FIM
                PERFORM 140-LER-CWCONF  THRU 140-99-FIM
                        UNTIL FS-CWCONF EQUAL "00"
                        OR    FL-EXIT EQUAL 1
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

           IF  (NOT CONSULTA)
           AND  EFETIVAR
           AND (NOT FINALIZAR)
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

           IF   FS-CWCONF EQUAL "ES"
                MOVE "00" TO FS-CWCONF
           END-IF

           SET CWSQLC-UNLOCK TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO.

       100-99-FIM. EXIT.

       130-CRITICA.

           MOVE    SPACES     TO MENSAGEM-ERRO
           ACCEPT CTAC-VAR-CWMENE
           ACCEPT TECLA FROM ESCAPE KEY
           IF  ESC
               MOVE 1 TO FL-EXIT
           END-IF
           PERFORM 170-EXIBE-DADOS THRU 170-99-FIM

           MOVE MENSAGEM-ERRO TO CWSEND-MSG
           CALL "CWSEND" USING PARAMETROS-CWSEND

           MOVE    SPACE               TO COMANDO
           PERFORM 160-CHECK-COMANDO THRU 160-99-FIM.

       130-99-FIM.  EXIT.

       140-LER-CWCONF.

           EXEC COBOLware OBJECT DROP END-EXEC
           IF   NOT INCLUSAO
                COPY CWPGON REPLACING 23 BY footline.
                IF   CWCONF-ARQUIVO = SPACES OR LOW-VALUES
                OR   FS-CWCONF > "11"
                OR  (CWCONF-TIPO NOT = "ES")
                     MOVE 0         TO F2-ON
                ELSE
                     EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                               LINE footline COLUMN 39 WIDTH 12
                               CAPTION " f2-~Exporta "
                               KEY F2 TAB-OFF
                     END-EXEC
                     MOVE 1         TO F2-ON
                END-IF
           ELSE
                EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                          LINE footline COLUMN 03 WIDTH 12
                          CAPTION " f1-~Importa "
                          KEY F2 TAB-OFF
                END-EXEC
           END-IF

           MOVE MENSAGEM-ERRO TO CWSEND-MSG
           CALL "CWSEND" USING PARAMETROS-CWSEND
           MOVE SPACES TO MENSAGEM-ERRO
           COPY CWESCP REPLACING 23 BY footline.

           MOVE 0 TO TECLA
           MOVE "ES" TO CWCONF-TIPO

           IF  NOT INCLUSAO
               EXEC COBOLware OBJECT COMBO-BOX NOEDIT
                         CAPTION "Estilos"
                         LINE 08 COLUMN 11 HEIGHT 15 WIDTH 30
                         PROGRAM "CWMENF" WORK-AREA "ES"
                         OPTION CWCONF-ARQUIVO ORDER 1 RETURN 1
                         STRING-1-LENGTH 30
               END-EXEC
           END-IF
           PERFORM TEST AFTER
                   UNTIL(NOT (PAGE-UP   AND PAGE-UP-OFF))
                     AND(NOT (PAGE-DOWN AND PAGE-DOWN-OFF))
                   ACCEPT CHAVE
                   ACCEPT TECLA FROM ESCAPE KEY
           END-PERFORM
           INSPECT CWCONF-ARQUIVO
                   CONVERTING MINUSCULAS TO MAIUSCULAS
           EXEC COBOLware OBJECT DROP END-EXEC
           DISPLAY CHAVE

           IF   F2-ON = 1
           AND  F2
                CALL "CWEXIM" USING "E" "ES" CWCONF-ARQUIVO
                CANCEL "CWEXIM"
                GO TO 140-LER-CWCONF
           END-IF

           MOVE 0 TO F1-ON
           IF   F1
           AND  INCLUSAO
                CALL "CWEXIM" USING "I" "ES" CWCONF-ARQUIVO
                CANCEL "CWEXIM"
                SET CWSQLC-START TO TRUE
                SET CWSQLC-NOT-LESS TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
                SET PAGE-DOWN TO TRUE
                MOVE 1 TO F1-ON
           END-IF

           COPY CWESCP REPLACING 23 BY footline.

           IF  ESC
               MOVE 1 TO FL-EXIT
           ELSE
               IF   ALTERACAO OR INCLUSAO
                    EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                              LINE footline COLUMN 02 WIDTH 13
                              CAPTION " F2-Concluir "
                              KEY F2
                    END-EXEC
               END-IF
           END-IF

           IF   CWCONF-ARQUIVO EQUAL SPACES
           AND  NOT (PAGE-UP OR PAGE-DOWN)
                MOVE "44" TO FS-CWCONF
                NEXT SENTENCE
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
                             MOVE "ES" TO CWCONF-CHAVE(1:2)
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
                                OR  CWCONF-TIPO    NOT EQUAL "ES"
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
                                     AND CWCONF-TIPO EQUAL "ES"
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
                                     AND CWCONF-TIPO EQUAL "ES"
                                     AND PAGE-DOWN-OFF
                                         SET PAGE-DOWN-ON TO TRUE
                                     END-IF
                                  END-PERFORM
                             END-EVALUATE
                     END-PERFORM
                     IF   CWCONF-TIPO EQUAL "ES"
                     AND  FS-CWCONF < "10"
                          SET READY-ON TO TRUE
                          MOVE CWCONF-CHAVE TO SALVA-CHAVE
                          PERFORM 170-EXIBE-DADOS THRU 170-99-FIM
                          GO TO 140-LER-CWCONF
                     ELSE
                          IF FS-CWCONF = "10"
                          OR (CWCONF-TIPO NOT EQUAL "ES")
                             IF PAGE-UP
                                SET PAGE-UP-OFF TO TRUE
                             END-IF
                             IF PAGE-DOWN
                                SET PAGE-DOWN-OFF TO TRUE
                             END-IF
                          END-IF
                          MOVE ZERO TO OK
                          MOVE "44" TO FS-CWCONF
                          MOVE "ES" TO CWCONF-REGES
                     END-IF
                ELSE
                     PERFORM TEST AFTER
                             UNTIL FS-CWCONF NOT = "9D"
                             SET CWSQLC-READ TO TRUE
                             SET CWSQLC-EQUAL TO TRUE
                             SET CWSQLC-LOCK TO TRUE
                             CALL "CWCONF" USING CWSQLC
                                                 CWCONF-REG
                                                 FS-CWCONF
                                                 KCO PCO
                     END-PERFORM
                END-IF
                IF   FS-CWCONF < "10"
                     PERFORM 170-EXIBE-DADOS THRU 170-99-FIM
                     IF   INCLUSAO
                          IF   F1-ON = 1
                               MOVE MSG (5) TO MENSAGEM-ERRO
                          ELSE
                               MOVE MSG (4) TO MENSAGEM-ERRO
                          END-IF
                          MOVE "44" TO FS-CWCONF
                     END-IF
                ELSE
                     SET READY-OFF TO TRUE
                     IF   NOT INCLUSAO
                          IF  FS-CWCONF = '23'
                              MOVE MSG (2) TO MENSAGEM-ERRO
                          ELSE
                              MOVE SPACES  TO MENSAGEM-ERRO
                          END-IF
                          MOVE SALVA-CHAVE TO CWCONF-CHAVE
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
                     END-IF
                     PERFORM 170-EXIBE-DADOS THRU 170-99-FIM
                END-IF
           ELSE
                MOVE ZERO   TO FS-CWCONF
                MOVE SPACES TO COMANDO
                               FUNCAO
           END-IF
           END-IF

           IF   CWCONF-ARQUIVO EQUAL SPACES
                MOVE "44" TO FS-CWCONF
           END-IF.

       140-99-FIM. EXIT.

       160-CHECK-COMANDO.

           COPY CWEFAB.

       160-99-FIM. EXIT.

       170-EXIBE-DADOS.

           DISPLAY CTAC-VAR-CWMENE.

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

           MOVE "ES" TO CWCONF-REGES.

       800-99-FIM. EXIT.

       900-FINAIS.

           SET CWSQLC-CLOSE TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO.

       900-99-FIM. EXIT.

       END PROGRAM CWMENE.

