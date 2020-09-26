       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWMENB.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  23/06/1994.
       SECURITY.      *************************************************
                      *                                               *
                      * Manutencao de parametros genericos            *
                      *                                               *
                      * Modulo de manutencao de impressoras           *
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
           05 SPOOL-DEV                PIC  X(001) VALUE SPACE.
           05 OK                       PIC  9(001) VALUE ZERO.
           05 FIM                      PIC  9(001) VALUE ZERO.
           05 OPT                      PIC  9(002) VALUE 0.
           05 SAVE-OP                  PIC  9(001) VALUE ZERO.
           05 CAMPO                    PIC  9(001) VALUE ZERO.
           05 LINHA-BRANCA             PIC  X(040) VALUE SPACES.
           05 POP                      PIC  X(080) VALUE SPACES.
           05 TECLA                    PIC  9(003) VALUE 0. COPY CWKEYS.
           05 FL-EXIT                  PIC  9(001) VALUE 1.
           05 SALVA-CHAVE              PIC  X(032) VALUE SPACES.

       01  AREAS-DE-TRABALHO-2.
           05 MENSAGENS-DE-ERRO.
              10 F PIC X(30) VALUE "Entre com os dados            ".
              10 F PIC X(30) VALUE "Impressora nÆo cadastrada     ".
              10 F PIC X(30) VALUE "Confirme exclusÆo             ".
              10 F PIC X(30) VALUE "Impressora j  cadastrada      ".
              10 F PIC X(30) VALUE "Device em branco              ".
              10 F PIC X(30) VALUE "Device nÆo suporta preparo    ".
              10 F PIC X(30) VALUE "Estilo nÆo definido           ".
           05 FILLER REDEFINES MENSAGENS-DE-ERRO.
              10 MSG OCCURS 7 PIC X(30).

       COPY CWBOXF.
       COPY CWFUNC.
       COPY CWSEND.
       COPY CWCONF.

       SCREEN SECTION.

       01  CWMENBA.
           05 LINE 07 COLUMN 03 VALUE "Impressora".
           05 LINE 08 COLUMN 03 VALUE "Estilo".
           05 LINE 10 COLUMN 03 VALUE "Device".
           05 LINE 12 COLUMN 03 VALUE "Preparo".
           05 LINE 14 COLUMN 03 VALUE "Cadeia ASCII inicial".
           05 LINE 18 COLUMN 03 VALUE "Cadeia ASCII final  ".
           05 LINE 12 COLUMN 41 VALUE "Formato          Acentos".

       01  CWMENBB AUTO.
           05 LINE 07 COLUMN 24 PIC X(015) USING CWCONF-ARQUIVO.

       01  CWMENBC AUTO.
           05 LINE 08 COLUMN 24 PIC X(030) USING CWCONF-ESTILO.
           05 LINE 10 COLUMN 24 PIC X(050) USING CWCONF-LABEL.
           05 LINE 12 COLUMN 24 PIC X(014) USING CWCONF-EJECT-MODE.
           05 LINE 12 COLUMN 49 PIC X(006) USING CWCONF-FORMATO.
           05 LINE 12 COLUMN 66 PIC X(007) USING CWCONF-CODEPAGE.
           05 LINE 14 COLUMN 24 PIC Z(003) USING CWCONF-ASCII-I(01).
           05 LINE 14 COLUMN 28 PIC Z(003) USING CWCONF-ASCII-I(02).
           05 LINE 14 COLUMN 32 PIC Z(003) USING CWCONF-ASCII-I(03).
           05 LINE 14 COLUMN 36 PIC Z(003) USING CWCONF-ASCII-I(04).
           05 LINE 14 COLUMN 40 PIC Z(003) USING CWCONF-ASCII-I(05).
           05 LINE 14 COLUMN 44 PIC Z(003) USING CWCONF-ASCII-I(06).
           05 LINE 14 COLUMN 48 PIC Z(003) USING CWCONF-ASCII-I(07).
           05 LINE 14 COLUMN 52 PIC Z(003) USING CWCONF-ASCII-I(08).
           05 LINE 14 COLUMN 56 PIC Z(003) USING CWCONF-ASCII-I(09).
           05 LINE 14 COLUMN 60 PIC Z(003) USING CWCONF-ASCII-I(10).
           05 LINE 14 COLUMN 64 PIC Z(003) USING CWCONF-ASCII-I(11).
           05 LINE 14 COLUMN 68 PIC Z(003) USING CWCONF-ASCII-I(12).
           05 LINE 14 COLUMN 72 PIC Z(003) USING CWCONF-ASCII-I(13).
           05 LINE 15 COLUMN 24 PIC Z(003) USING CWCONF-ASCII-I(14).
           05 LINE 15 COLUMN 28 PIC Z(003) USING CWCONF-ASCII-I(15).
           05 LINE 15 COLUMN 32 PIC Z(003) USING CWCONF-ASCII-I(16).
           05 LINE 15 COLUMN 36 PIC Z(003) USING CWCONF-ASCII-I(17).
           05 LINE 15 COLUMN 40 PIC Z(003) USING CWCONF-ASCII-I(18).
           05 LINE 15 COLUMN 44 PIC Z(003) USING CWCONF-ASCII-I(19).
           05 LINE 15 COLUMN 48 PIC Z(003) USING CWCONF-ASCII-I(20).
           05 LINE 15 COLUMN 52 PIC Z(003) USING CWCONF-ASCII-I(21).
           05 LINE 15 COLUMN 56 PIC Z(003) USING CWCONF-ASCII-I(22).
           05 LINE 15 COLUMN 60 PIC Z(003) USING CWCONF-ASCII-I(23).
           05 LINE 15 COLUMN 64 PIC Z(003) USING CWCONF-ASCII-I(24).
           05 LINE 15 COLUMN 68 PIC Z(003) USING CWCONF-ASCII-I(25).
           05 LINE 15 COLUMN 72 PIC Z(003) USING CWCONF-ASCII-I(26).
           05 LINE 16 COLUMN 24 PIC Z(003) USING CWCONF-ASCII-I(27).
           05 LINE 16 COLUMN 28 PIC Z(003) USING CWCONF-ASCII-I(28).
           05 LINE 16 COLUMN 32 PIC Z(003) USING CWCONF-ASCII-I(29).
           05 LINE 16 COLUMN 36 PIC Z(003) USING CWCONF-ASCII-I(30).
           05 LINE 16 COLUMN 40 PIC Z(003) USING CWCONF-ASCII-I(31).
           05 LINE 16 COLUMN 44 PIC Z(003) USING CWCONF-ASCII-I(32).
           05 LINE 16 COLUMN 48 PIC Z(003) USING CWCONF-ASCII-I(33).
           05 LINE 16 COLUMN 52 PIC Z(003) USING CWCONF-ASCII-I(34).
           05 LINE 16 COLUMN 56 PIC Z(003) USING CWCONF-ASCII-I(35).
           05 LINE 16 COLUMN 60 PIC Z(003) USING CWCONF-ASCII-I(36).
           05 LINE 16 COLUMN 64 PIC Z(003) USING CWCONF-ASCII-I(37).
           05 LINE 16 COLUMN 68 PIC Z(003) USING CWCONF-ASCII-I(38).
           05 LINE 16 COLUMN 72 PIC Z(003) USING CWCONF-ASCII-I(39).
           05 LINE 17 COLUMN 24 PIC Z(003) USING CWCONF-ASCII-I(40).
           05 LINE 17 COLUMN 28 PIC Z(003) USING CWCONF-ASCII-I(41).
           05 LINE 17 COLUMN 32 PIC Z(003) USING CWCONF-ASCII-I(42).
           05 LINE 17 COLUMN 36 PIC Z(003) USING CWCONF-ASCII-I(43).
           05 LINE 17 COLUMN 40 PIC Z(003) USING CWCONF-ASCII-I(44).
           05 LINE 17 COLUMN 44 PIC Z(003) USING CWCONF-ASCII-I(45).
           05 LINE 17 COLUMN 48 PIC Z(003) USING CWCONF-ASCII-I(46).
           05 LINE 17 COLUMN 52 PIC Z(003) USING CWCONF-ASCII-I(47).
           05 LINE 17 COLUMN 56 PIC Z(003) USING CWCONF-ASCII-I(48).
           05 LINE 17 COLUMN 60 PIC Z(003) USING CWCONF-ASCII-I(49).
           05 LINE 17 COLUMN 64 PIC Z(003) USING CWCONF-ASCII-I(50).
           05 LINE 18 COLUMN 24 PIC Z(003) USING CWCONF-ASCII-F(01).
           05 LINE 18 COLUMN 28 PIC Z(003) USING CWCONF-ASCII-F(02).
           05 LINE 18 COLUMN 32 PIC Z(003) USING CWCONF-ASCII-F(03).
           05 LINE 18 COLUMN 36 PIC Z(003) USING CWCONF-ASCII-F(04).
           05 LINE 18 COLUMN 40 PIC Z(003) USING CWCONF-ASCII-F(05).
           05 LINE 18 COLUMN 44 PIC Z(003) USING CWCONF-ASCII-F(06).
           05 LINE 18 COLUMN 48 PIC Z(003) USING CWCONF-ASCII-F(07).
           05 LINE 18 COLUMN 52 PIC Z(003) USING CWCONF-ASCII-F(08).
           05 LINE 18 COLUMN 56 PIC Z(003) USING CWCONF-ASCII-F(09).
           05 LINE 18 COLUMN 60 PIC Z(003) USING CWCONF-ASCII-F(10).
           05 LINE 18 COLUMN 64 PIC Z(003) USING CWCONF-ASCII-F(11).
           05 LINE 18 COLUMN 68 PIC Z(003) USING CWCONF-ASCII-F(12).
           05 LINE 18 COLUMN 72 PIC Z(003) USING CWCONF-ASCII-F(13).
           05 LINE 19 COLUMN 24 PIC Z(003) USING CWCONF-ASCII-F(14).
           05 LINE 19 COLUMN 28 PIC Z(003) USING CWCONF-ASCII-F(15).
           05 LINE 19 COLUMN 32 PIC Z(003) USING CWCONF-ASCII-F(16).
           05 LINE 19 COLUMN 36 PIC Z(003) USING CWCONF-ASCII-F(17).
           05 LINE 19 COLUMN 40 PIC Z(003) USING CWCONF-ASCII-F(18).
           05 LINE 19 COLUMN 44 PIC Z(003) USING CWCONF-ASCII-F(19).
           05 LINE 19 COLUMN 48 PIC Z(003) USING CWCONF-ASCII-F(20).
           05 LINE 19 COLUMN 52 PIC Z(003) USING CWCONF-ASCII-F(21).
           05 LINE 19 COLUMN 56 PIC Z(003) USING CWCONF-ASCII-F(22).
           05 LINE 19 COLUMN 60 PIC Z(003) USING CWCONF-ASCII-F(23).
           05 LINE 19 COLUMN 64 PIC Z(003) USING CWCONF-ASCII-F(24).
           05 LINE 19 COLUMN 68 PIC Z(003) USING CWCONF-ASCII-F(25).
           05 LINE 19 COLUMN 72 PIC Z(003) USING CWCONF-ASCII-F(26).
           05 LINE 20 COLUMN 24 PIC Z(003) USING CWCONF-ASCII-F(27).
           05 LINE 20 COLUMN 28 PIC Z(003) USING CWCONF-ASCII-F(28).
           05 LINE 20 COLUMN 32 PIC Z(003) USING CWCONF-ASCII-F(29).
           05 LINE 20 COLUMN 36 PIC Z(003) USING CWCONF-ASCII-F(30).
           05 LINE 20 COLUMN 40 PIC Z(003) USING CWCONF-ASCII-F(31).
           05 LINE 20 COLUMN 44 PIC Z(003) USING CWCONF-ASCII-F(32).
           05 LINE 20 COLUMN 48 PIC Z(003) USING CWCONF-ASCII-F(33).
           05 LINE 20 COLUMN 52 PIC Z(003) USING CWCONF-ASCII-F(34).
           05 LINE 20 COLUMN 56 PIC Z(003) USING CWCONF-ASCII-F(35).
           05 LINE 20 COLUMN 60 PIC Z(003) USING CWCONF-ASCII-F(36).
           05 LINE 20 COLUMN 64 PIC Z(003) USING CWCONF-ASCII-F(37).
           05 LINE 20 COLUMN 68 PIC Z(003) USING CWCONF-ASCII-F(38).
           05 LINE 20 COLUMN 72 PIC Z(003) USING CWCONF-ASCII-F(39).
           05 LINE 21 COLUMN 24 PIC Z(003) USING CWCONF-ASCII-F(40).
           05 LINE 21 COLUMN 28 PIC Z(003) USING CWCONF-ASCII-F(41).
           05 LINE 21 COLUMN 32 PIC Z(003) USING CWCONF-ASCII-F(42).
           05 LINE 21 COLUMN 36 PIC Z(003) USING CWCONF-ASCII-F(43).
           05 LINE 21 COLUMN 40 PIC Z(003) USING CWCONF-ASCII-F(44).
           05 LINE 21 COLUMN 44 PIC Z(003) USING CWCONF-ASCII-F(45).
           05 LINE 21 COLUMN 48 PIC Z(003) USING CWCONF-ASCII-F(46).
           05 LINE 21 COLUMN 52 PIC Z(003) USING CWCONF-ASCII-F(47).
           05 LINE 21 COLUMN 56 PIC Z(003) USING CWCONF-ASCII-F(48).
           05 LINE 21 COLUMN 60 PIC Z(003) USING CWCONF-ASCII-F(49).
           05 LINE 21 COLUMN 64 PIC Z(003) USING CWCONF-ASCII-F(50).

       PROCEDURE DIVISION.

       000-INICIO.

           EXEC COBOLware BOXS
                LINE    8
                COLUMN  8
                ITENS   SPACES
                TITLE   "Manuten‡Æo de:"
                TEXT(1) "~Modelos"
                TEXT(2) "~Estilos"
                OPTION OPT;
           END-EXEC
           EVALUATE OPT
               WHEN 1 PERFORM 800-INICIAIS      THRU 800-99-FIM
                      PERFORM 100-PROCESSAMENTO THRU 100-99-FIM
                              UNTIL FINALIZAR
                      PERFORM 900-FINAIS        THRU 900-99-FIM
               WHEN 2 CALL "CWMENE" CANCEL "CWMENE"
               WHEN OTHER GOBACK
           END-EVALUATE.

       000-99-FIM.

           IF   PARAR
                STOP RUN
           ELSE
                GOBACK
           END-IF.

       100-PROCESSAMENTO.

           ON   1
                IF   NOT FINALIZAR
                     DISPLAY CWMENBA
                END-IF.

           IF   FL-EXIT EQUAL 1
                EXEC COBOLware Object Drop END-EXEC
                PERFORM 150-OBJ THRU 150-99-FIM
                DISPLAY CWMENBC
                MOVE SPACE TO FUNCAO
                CALL "CWIAEF" USING FUNCAO
                MOVE ZERO  TO FL-EXIT
           END-IF

           MOVE "11" TO FS-CWCONF

           IF   NOT FINALIZAR
                IF  INCLUSAO
                OR  FS-CWCONF > "11"
                    MOVE SPACES TO CWCONF-ARQUIVO
                    PERFORM 110-LIMPA THRU 110-99-FIM
                END-IF
                PERFORM 140-LER-CWCONF  THRU 140-99-FIM
                        UNTIL FS-CWCONF EQUAL "00"
                        OR    FL-EXIT EQUAL 1
                CALL "CWMSGW" USING "230340" LINHA-BRANCA
                IF  (INCLUSAO OR ALTERACAO)
                AND  FL-EXIT NOT EQUAL 1
                     MOVE    MSG (1)            TO CWSEND-MSG
                     MOVE    SPACES             TO COMANDO
                     PERFORM 130-CRITICA      THRU 130-99-FIM
                             UNTIL CWSEND-MSG EQUAL SPACES
                             OR    ABORTAR
                             OR    ESC
                     IF   ESC
                          MOVE 1 TO FL-EXIT
                     END-IF
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

           IF   FS-CWCONF EQUAL "03"
                MOVE "00" TO FS-CWCONF
           END-IF

           SET CWSQLC-UNLOCK TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO.

       100-99-FIM. EXIT.

       110-LIMPA.

           MOVE X"FF"      TO CWCONF-ASCII
           MOVE SPACES     TO CWCONF-LABEL
                              CWCONF-ESTILO
                              CWCONF-FORMATO
                              CWCONF-CODEPAGE
           MOVE "N"        TO CWCONF-FORMATO
           MOVE "8"        TO CWCONF-CODEPAGE
           MOVE "00"       TO CWCONF-EJECT-MODE
           MOVE LOW-VALUES TO CWCONF-CADEIA-ASCII-INICIAL
                              CWCONF-CADEIA-ASCII-FINAL.

       110-99-FIM. EXIT.

       130-CRITICA.

           DISPLAY CWMENBC
           ACCEPT CWMENBC
           ACCEPT TECLA FROM ESCAPE KEY
           MOVE SPACES TO CWSEND-MSG CWSEND-SCREENS
           IF   ESC
                GO TO 130-99-FIM
           END-IF

           IF   CWCONF-LABEL EQUAL SPACES
                MOVE MSG (5) TO CWSEND-MSG
           ELSE
                MOVE CWCONF-LABEL TO SPOOL-DEV
md==> *         IF   SPOOL-DEV EQUAL "$"
md==> *         AND((CWCONF-CADEIA-ASCII-INICIAL NOT = LOW-VALUES)
md==> *         OR  (CWCONF-CADEIA-ASCII-FINAL   NOT = LOW-VALUES)
md==> *         OR  (CWCONF-EJECT-MODE           NOT = "00"))
      *         OR  (CWCONF-ESTILO               NOT = SPACES))
md==> *              MOVE MSG (6) TO CWSEND-MSG
md==> *         END-IF
           END-IF

           IF   CWSEND-MSG NOT = SPACES
                CALL "CWSEND" USING PARAMETROS-CWSEND
                MOVE 0 TO TECLA
           ELSE
                MOVE    SPACE               TO COMANDO
                PERFORM 160-CHECK-COMANDO THRU 160-99-FIM
                MOVE SPACES                 TO CWSEND-MSG
           END-IF.

       130-99-FIM.  EXIT.

       140-LER-CWCONF.

           IF FIM = ZERO
              CALL "CWSEND" USING PARAMETROS-CWSEND
           END-IF
           EXEC COBOLware OBJECT DROP END-EXEC

           IF   CWSEND-MSG EQUAL SPACES
2807       OR   FIM = 1
                IF   NOT INCLUSAO
                     EXEC COBOLware OBJECT COMBO-BOX
                          CAPTION "Impressoras"
                          LINE 07 COLUMN 24
                          HEIGHT 15 WIDTH 15 NOEDIT
                          PROGRAM "CWMENF" ORDER 1 RETURN 1
                          OPTION CWCONF-ARQUIVO
                          STRING-1-LENGTH 15
                          WORK-AREA "03                              1"
                     END-EXEC
                     COPY CWPGON REPLACING 23 BY footline.
                END-IF
           END-IF
           COPY CWESCP REPLACING 23 BY footline.

           MOVE SPACES TO CWSEND-MSG
           MOVE 0      TO TECLA
                          FIM

           PERFORM TEST AFTER
                   UNTIL(NOT (PAGE-UP   AND PAGE-UP-OFF))
                     AND(NOT (PAGE-DOWN AND PAGE-DOWN-OFF))
                   ACCEPT CWMENBB
                   INSPECT CWCONF-ARQUIVO
                CONVERTING MINUSCULAS TO MAIUSCULAS
                   ACCEPT TECLA FROM ESCAPE KEY
                  DISPLAY CWMENBB
           END-PERFORM
           EXEC COBOLware OBJECT DROP END-EXEC
           IF  NOT ESC
               PERFORM 150-OBJ THRU 150-99-FIM
               COPY CWESCP REPLACING 23 BY footline.
               IF   ALTERACAO OR INCLUSAO
                    EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                              LINE footline COLUMN 02 WIDTH 13
                              CAPTION " f2-~Concluir "
                              KEY F2
                    END-EXEC
               END-IF
           ELSE
               MOVE 1 TO FL-EXIT
           END-IF

           PERFORM 170-EXIBE-DADOS THRU 170-99-FIM
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
                             MOVE "03" TO CWCONF-CHAVE(1:2)
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
                             UNTIL((CWCONF-ARQUIVO <> "PRN")
                               AND (CWCONF-ARQUIVO <> SPACES)
                               AND (CWCONF-ARQUIVO <> "<Default>")
                               AND (CWCONF-ARQUIVO <> "Spool")
                               AND (CWCONF-ARQUIVO <> "<Spool>"))
                                OR  FS-CWCONF      <> "00"
                                OR  CWCONF-TIPO    <> "03"
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
                                     AND CWCONF-TIPO EQUAL "03"
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
                                     AND CWCONF-TIPO EQUAL "03"
                                     AND PAGE-DOWN-OFF
                                         SET PAGE-DOWN-ON TO TRUE
                                     END-IF
                                  END-PERFORM
                             END-EVALUATE
                     END-PERFORM
                     IF   CWCONF-TIPO EQUAL "03"
                     AND  FS-CWCONF < "10"
                          MOVE CWCONF-CHAVE TO SALVA-CHAVE
                          PERFORM 170-EXIBE-DADOS THRU 170-99-FIM
                          GO TO 140-LER-CWCONF
                     ELSE
                          IF FS-CWCONF = "10"
                          OR (CWCONF-TIPO NOT EQUAL "03")
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
                          INITIALIZE CWCONF-REG03
                          MOVE "03" TO CWCONF-TIPO
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
                     IF   FS-CWCONF = "9D"
                          CALL "CWCONF" USING "ISAM"
                     END-IF
                     END-PERFORM
                END-IF
                IF   FS-CWCONF EQUAL "00"
                     PERFORM 170-EXIBE-DADOS THRU 170-99-FIM
                     IF   INCLUSAO
                          MOVE MSG (4) TO CWSEND-MSG
                          MOVE "44" TO FS-CWCONF
                     END-IF
                ELSE
                     IF   NOT INCLUSAO
                          MOVE MSG (2)     TO CWSEND-MSG
2807                      MOVE SALVA-CHAVE TO CWCONF-CHAVE
                          SET CWSQLC-READ  TO TRUE
                          SET CWSQLC-EQUAL TO TRUE
                          CALL "CWCONF" USING CWSQLC
                                              CWCONF-REG
                                              FS-CWCONF
                                              KCO PCO
                          MOVE '10' TO FS-CWCONF
                          IF  FIM = 1
                              MOVE "Fim da leitura" TO CWSEND-MSG
                          END-IF
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
>>>        END-IF

           IF   CWCONF-ARQUIVO EQUAL SPACES
                MOVE "44" TO FS-CWCONF
           END-IF.

       140-99-FIM. EXIT.

       150-OBJ.

           EXEC COBOLware OBJECT COMBO-BOX
                CAPTION "Preparo"
                LINE 12 COLUMN 24 HEIGHT 4 WIDTH 14
                ORDER 2 RETURN 1
                OPTION CWCONF-EJECT-MODE
                STRING-2-LENGTH 14 NOEDIT
                STRING-1(1) "10" STRING-2(1) "In¡cio"
                STRING-1(2) "11" STRING-2(2) "In¡cio e final"
                STRING-1(3) "01" STRING-2(3) "Final"
                STRING-1(4) "00" STRING-2(4) "Sem salto"
           END-EXEC
           EXEC COBOLware OBJECT COMBO-BOX
                CAPTION "Formato"
                LINE 12 COLUMN 49 HEIGHT 3 WIDTH 6
                ORDER 2 RETURN 1
                OPTION CWCONF-FORMATO
                STRING-2-LENGTH 6 NOEDIT
                STRING-1(1) "D" STRING-2(1) "DOS"
                STRING-1(2) "U" STRING-2(2) "Unix"
                STRING-1(3) "N" STRING-2(3) "Nativo"
           END-EXEC
           EXEC COBOLware OBJECT COMBO-BOX
                CAPTION "Acentos"
                LINE 12 COLUMN 66 HEIGHT 4 WIDTH 7
                ORDER 2 RETURN 1
                OPTION CWCONF-CODEPAGE
                STRING-2-LENGTH 7 NOEDIT
                STRING-1(1) "0" STRING-2(1) "001"
                STRING-1(2) "O" STRING-2(2) "Off"
                STRING-1(3) "W" STRING-2(3) "Windows"
                STRING-1(4) "8" STRING-2(4) "850"
           END-EXEC
           EXEC COBOLware OBJECT COMBO-BOX
                     CAPTION "Estilos" NOEDIT
                     LINE 08 COLUMN 24 HEIGHT 15 WIDTH 30
                     PROGRAM "CWMENF" WORK-AREA "ES"
                     OPTION CWCONF-ESTILO ORDER 1 RETURN 1
                     STRING-1-LENGTH 30
           END-EXEC.

       150-99-FIM. EXIT.

       160-CHECK-COMANDO.

           COPY CWEFAB.

       160-99-FIM. EXIT.

       170-EXIBE-DADOS.

           IF  CWCONF-ASCII NOT = X"FF"
               MOVE X"FF"      TO CWCONF-ASCII
               MOVE LOW-VALUES TO CWCONF-CADEIA-ASCII-INICIAL
                                  CWCONF-CADEIA-ASCII-FINAL
           END-IF
           IF   NOT CWCONF-FORMATO-OK
                SET CWCONF-FORMATO-NATIVO TO TRUE
           END-IF
           IF   NOT CWCONF-CODEPAGE-OK
                SET CWCONF-CODEPAGE-850  TO TRUE
           END-IF
           DISPLAY CWMENBC.

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

           INITIALIZE CWCONF-REG03
           MOVE "03" TO CWCONF-TIPO.

       800-99-FIM. EXIT.

       900-FINAIS.

           SET CWSQLC-CLOSE TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO.

       900-99-FIM. EXIT.

       END PROGRAM CWMENB.
