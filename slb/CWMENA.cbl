       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWMENA.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  09/05/1990.
       SECURITY.      *************************************************
                      *                                               *
                      *   Manutencao de parametros genericos          *
                      *                                               *
                      *   Modulo de manutencao de GRUPOS              *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT OPTIONAL CWMENX ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS CWMENX-ITEM
                  ALTERNATE RECORD KEY IS CWMENX-PROGRAMA
                                          WITH DUPLICATES
                  LOCK MODE     IS AUTOMATIC
                  FILE STATUS   IS FS-CWMENX.

           SELECT OPTIONAL CWORK ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS CWORK-ITEM
                  LOCK MODE     IS AUTOMATIC
                  FILE STATUS   IS FS-CWORK.

       DATA DIVISION.
       FILE SECTION.

       FD  CWMENX
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-CWMENX.

       01  CWMENX-REG.
           05 CWMENX-ITEM               PIC 9(006).
           05 CWMENX-DESCRICAO          PIC X(049).
           05 CWMENX-PROGRAMA           PIC X(008).
           05 CWMENX-CHAVE              PIC X(006).
           05 CWMENX-NIVEL              PIC 9(001).
           05 CWMENX-FLAGS.
              10 CWMENX-ID.
                 15 CWMENX-ACESSO       PIC X(001).
                 15 CWMENX-ALTERACAO    PIC X(001).
                 15 CWMENX-CONSULTA     PIC X(001).
                 15 CWMENX-EXCLUSAO     PIC X(001).
                 15 CWMENX-INCLUSAO     PIC X(001).
              10 CWMENX-DUPLICADO       PIC X(001).

       FD  CWORK
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-CWORK.

       01  CWORK-REG.
           05 CWORK-ITEM               PIC 9(006).
           05 CWORK-DESCRICAO          PIC X(049).
           05 CWORK-PROGRAMA           PIC X(008).
           05 CWORK-CHAVE              PIC X(006).
           05 CWORK-NIVEL              PIC 9(001).
           05 CWORK-FLAGS.
              10 CWORK-ID.
                 15 CWORK-ACESSO       PIC X(001).
                 15 CWORK-ALTERACAO    PIC X(001).
                 15 CWORK-CONSULTA     PIC X(001).
                 15 CWORK-EXCLUSAO     PIC X(001).
                 15 CWORK-INCLUSAO     PIC X(001).
              10 CWORK-DUPLICADO       PIC X(001).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1. COPY CWCASE.
           05 footline-t.
              10 footline              PIC  9(002) VALUE 23.
           05 ELEMENTO                 PIC  X(030) VALUE SPACES.
           05 ARQUIVO                  PIC  X(050) VALUE SPACES.
           05 SETA-DOWN                PIC  X(001) VALUE X"19".
           05 SETA-UP                  PIC  X(001) VALUE X"18".
           05 PATH                     PIC  X(007) VALUE SPACES.
           05 COPIADO                  PIC  9(001) VALUE 0.
           05 DESCE                    PIC  9(001) VALUE 0.
           05 BRANCO                   PIC  9(003) VALUE 112.
           05 TECLOU                   PIC  9(001) VALUE 0.
           05 POSICAO                  PIC  9(004) VALUE 0.
           05 ITEM-MAX                 PIC  9(004) VALUE 0.
           05 NIVEL                    PIC  9(002) VALUE 0.
           05 NIVEIS.
              10 NIVEL-ID OCCURS 7.
                 15 NIVEL-ACESSO       PIC  X(001).
                 15 NIVEL-ALTERACAO    PIC  X(001).
                 15 NIVEL-CONSULTA     PIC  X(001).
                 15 NIVEL-EXCLUSAO     PIC  X(001).
                 15 NIVEL-INCLUSAO     PIC  X(001).
           05 SALVAS.
              10 SAVEX OCCURS 7.
                 15 SAVE-CHAVE         PIC  X(32).
                 15 SAVE-I             PIC  9(02).
                 15 MAX-i              PIC  9(02).
                 15 ITENS              PIC  9(02).
           05 TP                       PIC  X(001) VALUE SPACE.
           05 IP                       PIC  9(002) VALUE 0.
           05 YP                       PIC  9(002) VALUE 0.
           05 SM-FIL                   PIC  9(001) VALUE 0.
           05 SUB-CWCONF.
              10 SM-ATT                PIC  9(004) OCCURS 7.
           05 SUB-CWCONF-2             PIC  X(028) VALUE SPACES.
           05 USUARIOS                 PIC  9(005) VALUE 0.
           05 GRUPO-DIGITADO           PIC  X(022) VALUE SPACES.
           05 SENDMSG                  PIC  X(030) VALUE SPACES.
           05 GU-CLASSE.
              10                       PIC  X(032) VALUE "GU".
              10 CLASSE-GU             PIC  9.
              10                       PIC  X      VALUE '='.
           05 CBL-READ-WRITE-SCR-CHARS-ATTR.
              10 CURSOR-POSITION.
                 15 ROW-CURSOR         PIC  9(002) COMP-X VALUE 00.
                 15 COLUMN-CURSOR      PIC  9(002) COMP-X VALUE 03.
              10 ATTRIBUTE-BUFFER      PIC  X(073) VALUE SPACES.
              10 CARACTER-BUFFER       PIC  X(073) VALUE SPACES.
              10 STRING-LENGTH         PIC  9(004) COMP-X VALUE 73.
           05 STRING-1                 PIC  X(080) VALUE SPACES.
           05 STRING-2                             VALUE SPACES.
              10 STRING-P              PIC  X(005).
              10                       PIC  X(075).
           05 LOCK-LEVEL               PIC  9(001) VALUE 0.
           05 PROGRAMAS                PIC  9(006) VALUE 0.
           05 CHAVE                    PIC  9(002) VALUE 3.
           05 ITEM                     PIC  9(006) VALUE 0.
           05 OUTRO                    PIC  X(050) VALUE SPACES.
           05 SAVE-ID                  PIC  X(005) VALUE SPACES.
           05 NEW-ITEM                 PIC  X(080) VALUE SPACES.
           05 FIM                      PIC  9(001) VALUE ZERO.
           05 HOME                     PIC  9(001) VALUE ZERO.
           05 K                        PIC  9(002) VALUE ZERO.
           05 K2                       PIC  9(002) VALUE ZERO.
           05 F                        PIC  9(003) VALUE 0.
           05 B                        PIC  9(003) VALUE 0.
           05 PAGINAX                  PIC  9(004) VALUE ZERO.
           05 U                        PIC  9(002) VALUE 0.
           05 OC                       PIC  9(002) VALUE 0.
           05 ESPACOS                  PIC  X(080) VALUE SPACES.
           05 TESTE-OPCAO              PIC  X(001) VALUE SPACE.
           05 ADM                      PIC  X(001) VALUE SPACE.
           05 BYTE-X                   PIC  X(001) VALUE SPACE.
           05 TABELA-CORES.
              10 COR                   PIC  9(002) COMP-X OCCURS 256.
           05 TABELA-MOLDURA.
              10 BASE-MOLDURA          PIC  X(008) OCCURS 9.
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
           05 SALVA-END                PIC  X(032) VALUE SPACES.
           05 PROGRAMA-X               PIC  X(008) VALUE SPACES.
           05 TASK                     PIC  9(006) VALUE ZERO.
           05 PROGRAMA                 PIC  X(008) VALUE "CWMENU".
              88 MODULO-CWMENU VALUE "CWMENU" "GRMENU".
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
           05 PG                       PIC  9(003) VALUE 1.
           05 ULTIMA                   PIC  9(003) VALUE 0.
           05 ER-CWMENX.
              10 FS-CWMENX             PIC  X(002) VALUE "00".
              10 LB-CWMENX             PIC  X(255) VALUE SPACES.
           05 ER-CWORK.
              10 FS-CWORK              PIC  X(002) VALUE "00".
              10 LB-CWORK              PIC  X(255) VALUE SPACES.

       01  AREAS-DE-TRABALHO-2.
           05 MENSAGENS-DE-ERRO.
              10 PIC X(30) VALUE "Entre com os dados            ".
              10 PIC X(30) VALUE "Grupo nÆo cadastrado          ".
              10 PIC X(30) VALUE "Confirme exclusÆo             ".
              10 PIC X(30) VALUE "Grupo j  cadastrado           ".
              10 PIC X(30) VALUE SPACES.
           05 FILLER REDEFINES MENSAGENS-DE-ERRO.
              10 MSG OCCURS 5 PIC X(30).
           05 MSG-7 PIC X(07) VALUE "P gina".

       01  CWMENW-REG.
           05 CWMENW-LINHA OCCURS 11.
              10 CWMENW-PROGRAMA        PIC X(008).
              10 CWCHECK-ACESSO         PIC X(001).
              10 CWCHECK-ALTERACAO      PIC X(001).
              10 CWCHECK-CONSULTA       PIC X(001).
              10 CWCHECK-EXCLUSAO       PIC X(001).
              10 CWCHECK-INCLUSAO       PIC X(001).
              10 CWMENW-DESCRICAO       PIC X(049).

       COPY CWFUNC.
       COPY CWSEND.
       COPY CWUNIX.
       COPY CWCONF.

       SCREEN SECTION.

       01  CWMENAE AUTO.
           05 LINE 07 COLUMN 09 PIC X(022) USING CWCONF-NOME-GRUPO.

       01  CTAC-LIT-CWMENA.
           05 LINE 07 COLUMN 03 VALUE "Grupo".
           05 LINE 08 COLUMN 04 VALUE "Restri‡äes".
           05 LINE 09 COLUMN 05 VALUE "Item do menu".
           05 LINE 09 COLUMN 54 VALUE "Altera".
           05 LINE 09 COLUMN 62 VALUE "Exclui".
           05 LINE 09 COLUMN 70 VALUE "Inclui".

       01  CTAC-VAR-CWMENA.
           03 T-10.
           04 T-9.
           05 T-8.
           06 T-7.
           07 T-6.
           08 T-5.
           09 T-4.
           10 T-3.
           11 T-2.
           12 T-1.
           13 LINE 10 COLUMN 05 PIC X(001) USING CWCHECK-ACESSO(1).
           13 LINE 10 COLUMN 07 PIC X(049) FROM CWMENW-DESCRICAO(1).
           13 LINE 10 COLUMN 57 PIC X(001)
                 USING CWCHECK-ALTERACAO(1).
           13 LINE 10 COLUMN 65 PIC X(001) USING CWCHECK-EXCLUSAO(1).
           13 LINE 10 COLUMN 73 PIC X(001) USING CWCHECK-INCLUSAO(1).
           12 LINE 11 COLUMN 05 PIC X(001) USING CWCHECK-ACESSO(2).
           12 LINE 11 COLUMN 07 PIC X(049) FROM CWMENW-DESCRICAO(2).
           12 LINE 11 COLUMN 57 PIC X(001)
                 USING CWCHECK-ALTERACAO(2).
           12 LINE 11 COLUMN 65 PIC X(001) USING CWCHECK-EXCLUSAO(2).
           12 LINE 11 COLUMN 73 PIC X(001) USING CWCHECK-INCLUSAO(2).
           11 LINE 12 COLUMN 05 PIC X(001) USING CWCHECK-ACESSO(3).
           11 LINE 12 COLUMN 07 PIC X(049) FROM CWMENW-DESCRICAO(3).
           11 LINE 12 COLUMN 57 PIC X(001)
                 USING CWCHECK-ALTERACAO(3).
           11 LINE 12 COLUMN 65 PIC X(001) USING CWCHECK-EXCLUSAO(3).
           11 LINE 12 COLUMN 73 PIC X(001) USING CWCHECK-INCLUSAO(3).
           10 LINE 13 COLUMN 05 PIC X(001) USING CWCHECK-ACESSO(4).
           10 LINE 13 COLUMN 07 PIC X(049) FROM CWMENW-DESCRICAO(4).
           10 LINE 13 COLUMN 57 PIC X(001)
                 USING CWCHECK-ALTERACAO(4).
           10 LINE 13 COLUMN 65 PIC X(001) USING CWCHECK-EXCLUSAO(4).
           10 LINE 13 COLUMN 73 PIC X(001) USING CWCHECK-INCLUSAO(4).
           09 LINE 14 COLUMN 05 PIC X(001) USING CWCHECK-ACESSO(5).
           09 LINE 14 COLUMN 07 PIC X(049) FROM CWMENW-DESCRICAO(5).
           09 LINE 14 COLUMN 57 PIC X(001)
                 USING CWCHECK-ALTERACAO(5).
           09 LINE 14 COLUMN 65 PIC X(001) USING CWCHECK-EXCLUSAO(5).
           09 LINE 14 COLUMN 73 PIC X(001) USING CWCHECK-INCLUSAO(5).
           08 LINE 15 COLUMN 05 PIC X(001) USING CWCHECK-ACESSO(6).
           08 LINE 15 COLUMN 07 PIC X(049) FROM CWMENW-DESCRICAO(6).
           08 LINE 15 COLUMN 57 PIC X(001)
                 USING CWCHECK-ALTERACAO(6).
           08 LINE 15 COLUMN 65 PIC X(001) USING CWCHECK-EXCLUSAO(6).
           08 LINE 15 COLUMN 73 PIC X(001) USING CWCHECK-INCLUSAO(6).
           07 LINE 16 COLUMN 05 PIC X(001) USING CWCHECK-ACESSO(7).
           07 LINE 16 COLUMN 07 PIC X(049) FROM CWMENW-DESCRICAO(7).
           07 LINE 16 COLUMN 57 PIC X(001)
                 USING CWCHECK-ALTERACAO(7).
           07 LINE 16 COLUMN 65 PIC X(001) USING CWCHECK-EXCLUSAO(7).
           07 LINE 16 COLUMN 73 PIC X(001) USING CWCHECK-INCLUSAO(7).
           06 LINE 17 COLUMN 05 PIC X(001) USING CWCHECK-ACESSO(8).
           06 LINE 17 COLUMN 07 PIC X(049) FROM CWMENW-DESCRICAO(8).
           06 LINE 17 COLUMN 57 PIC X(001)
                 USING CWCHECK-ALTERACAO(8).
           06 LINE 17 COLUMN 65 PIC X(001) USING CWCHECK-EXCLUSAO(8).
           06 LINE 17 COLUMN 73 PIC X(001) USING CWCHECK-INCLUSAO(8).
           05 LINE 18 COLUMN 05 PIC X(001) USING CWCHECK-ACESSO(9).
           05 LINE 18 COLUMN 07 PIC X(049) FROM CWMENW-DESCRICAO(9).
           05 LINE 18 COLUMN 57 PIC X(001)
                 USING CWCHECK-ALTERACAO(9).
           05 LINE 18 COLUMN 65 PIC X(001) USING CWCHECK-EXCLUSAO(9).
           05 LINE 18 COLUMN 73 PIC X(001) USING CWCHECK-INCLUSAO(9).
           04 LINE 19 COLUMN 05 PIC X(001) USING CWCHECK-ACESSO(10).
           04 LINE 19 COLUMN 07 PIC X(049) FROM CWMENW-DESCRICAO(10).
           04 LINE 19 COLUMN 57 PIC X(001)
                 USING CWCHECK-ALTERACAO(10).
           04 LINE 19 COLUMN 65 PIC X(001)
                 USING CWCHECK-EXCLUSAO(10).
           04 LINE 19 COLUMN 73 PIC X(001)
                 USING CWCHECK-INCLUSAO(10).

           03 LINE 20 COLUMN 05 PIC X(001) USING CWCHECK-ACESSO(11).
           03 LINE 20 COLUMN 07 PIC X(049) FROM CWMENW-DESCRICAO(11).
           03 LINE 20 COLUMN 57 PIC X(001)
                 USING CWCHECK-ALTERACAO(11).
           03 LINE 20 COLUMN 65 PIC X(001)
                 USING CWCHECK-EXCLUSAO(11).
           03 LINE 20 COLUMN 73 PIC X(001)
                 USING CWCHECK-INCLUSAO(11).

       PROCEDURE DIVISION.

       000-INICIO.

           PERFORM 800-INICIAIS      THRU 800-99-FIM
           PERFORM 100-PROCESSAMENTO THRU 100-99-FIM
                   UNTIL FINALIZAR
                      OR PROGRAMAS = 0
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
                          LINE          8
                          COLUMN        3
                          LINES        12
                          COLUMNS      73
                          COLOR-FRAME  BRANCO
                          COLOR-BORDER BRANCO
                     END-EXEC
                     EXEC COBOLware BOXW POPUP END-EXEC
                     DISPLAY CTAC-LIT-CWMENA
                     PERFORM VARYING I FROM 1 BY 1 UNTIL I > 11
                             PERFORM 182-APAGA THRU 182-99-FIM
                     END-PERFORM
                END-IF.

           MOVE "23" TO FS-CWCONF

           IF   NOT FINALIZAR
                PERFORM 170-EXIBE-DADOS THRU 170-99-FIM
                PERFORM 140-LER-CWCONF  THRU 140-99-FIM
                        UNTIL FS-CWCONF EQUAL "00"
                        OR    FL-EXIT EQUAL 1
                IF  (INCLUSAO OR ALTERACAO)
                AND (FL-EXIT NOT EQUAL 1)
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
                     IF  (FL-EXIT NOT EQUAL 1)
                     AND  EXCLUSAO
                          CALL "CWMSGW" USING "230330" MSG (3)
                          PERFORM 160-CHECK-COMANDO THRU 160-99-FIM
                          CALL "CWMSGW" USING "230330" ESPACOS
                     END-IF
                END-IF
           END-IF

           MOVE "00" TO FS-CWCONF

           IF   EFETIVAR
           AND (NOT FINALIZAR)
                MOVE CWCONF-GRUPO-ID TO SAVE-ID
                MOVE "E"             TO CWCONF-ADM
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

           IF  CONSULTA-R
               MOVE 4 TO CHAVE
           ELSE
               EXEC COBOLware BoxSelect
                    TITLE "Modificar"
                    LINE 10 COLUMN 10
                    TEXT(1) "~Esta‡äes"
                    TEXT(2) "~Impressoras"
                    TEXT(3) "~Restri‡äes"
                    OPTION 3;CHAVE
               END-EXEC
           END-IF

           EVALUATE CHAVE
               WHEN 1
                    SET CWSQLC-UNLOCK TO TRUE
                    CALL "CWCONF" USING CWSQLC
                                        CWCONF-REG
                                        FS-CWCONF
                                        KCO PCO
                    SET F3 TO TRUE
                    CALL "CWMEN4" USING TECLA CWCONF-GRUPO-ID
                                        CWCONF-NOME-GRUPO
                    SET CWSQLC-REWRITE TO TRUE
                    CALL "CWCONF" USING CWSQLC
                                        CWCONF-REG
                                        FS-CWCONF
                                        KCO PCO
               WHEN 2
                    SET CWSQLC-UNLOCK TO TRUE
                    CALL "CWCONF" USING CWSQLC
                                        CWCONF-REG
                                        FS-CWCONF
                                        KCO PCO
                    SET F2 TO TRUE
                    CALL "CWMEN4" USING TECLA CWCONF-GRUPO-ID
                                        CWCONF-NOME-GRUPO
                    SET CWSQLC-REWRITE TO TRUE
                    CALL "CWCONF" USING CWSQLC
                                        CWCONF-REG
                                        FS-CWCONF
                                        KCO PCO
               WHEN 3
                    MOVE SPACES TO CWSEND-MSG
                    MOVE 0      TO TECLA
                    SET ALTERAR-R TO TRUE
                    PERFORM 180-RESTRICOES THRU 180-99-FIM
                    IF   ESC
                         MOVE 1   TO FL-EXIT
                    ELSE
                         PERFORM 160-CHECK-COMANDO THRU 160-99-FIM
                    END-IF
               WHEN 4 CONTINUE
               WHEN OTHER
                SET ABORTAR TO TRUE
                MOVE 1   TO FL-EXIT
                GO TO 130-99-FIM
           END-EVALUATE.

       130-99-FIM.  EXIT.

       140-LER-CWCONF.

           EXEC COBOLware OBJECT DROP END-EXEC
           IF FIM = ZERO
              CALL "CWSEND" USING PARAMETROS-CWSEND
           END-IF
           MOVE SPACES      TO CWSEND-MSG
           IF  NOT INCLUSAO
               EXEC COBOLware OBJECT COMBO-BOX
                    CAPTION "Grupos"
                    LINE 07 COLUMN 09 HEIGHT 14 WIDTH 22
                    PROGRAM "CWMENF"
                    OPTION CWCONF-NOME-GRUPO
                    ORDER 1 RETURN 1
                    STRING-1-LENGTH 22
                    WORK-AREA GU-CLASSE
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
           MOVE CWCONF-NOME-GRUPO TO GRUPO-DIGITADO
           INSPECT CWCONF-NOME-GRUPO CONVERTING MINUSCULAS TO MAIUSCULAS

           IF   CWCONF-NOME-GRUPO EQUAL SPACES
           AND (NOT (PAGE-UP OR PAGE-DOWN))
                MOVE "44" TO FS-CWCONF
           ELSE
           IF   FL-EXIT NOT EQUAL 1
                IF   (PAGE-UP OR PAGE-DOWN)
                AND (NOT INCLUSAO)
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
                AND (NOT INCLUSAO)
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
                                          AND CWCONF-PROG-GRUPO <> SPACES
                                              MOVE "9D" TO FS-CWCONF
                                          ELSE
                                              PERFORM 400-CHECK-GOOD
                                                 THRU 400-99-FIM
                                          END-IF
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
                                     ELSE
                                          IF  CWCONF-TIPO EQUAL "GU"
                                          AND CWCONF-PROG-GRUPO <> SPACES
                                              MOVE "9D" TO FS-CWCONF
                                          ELSE
                                              PERFORM 400-CHECK-GOOD
                                                 THRU 400-99-FIM
                                          END-IF
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
                          SET READY-ON TO TRUE
                          MOVE CWCONF-CHAVE TO SALVA-END
                          PERFORM 170-EXIBE-DADOS THRU 170-99-FIM
                          SET CONSULTA-R TO TRUE
                          PERFORM 180-RESTRICOES THRU 180-99-FIM
                          SET ALTERAR-R TO TRUE
                          GO TO 140-LER-CWCONF
                     ELSE
                          IF FS-CWCONF = "10"
                          OR (CWCONF-TIPO NOT EQUAL "GU")
                             MOVE 1 TO FIM
                             IF PAGE-UP
                                SET PAGE-UP-OFF TO TRUE
                             END-IF
                             IF PAGE-DOWN
                                SET PAGE-DOWN-OFF TO TRUE
                             END-IF
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
                       IF   FS-CWCONF = "23"
                       AND  EXCLUSAO
                            MOVE GRUPO-DIGITADO TO CWCONF-NOME-GRUPO
                            SET CWSQLC-LOCK TO TRUE
                            CALL "CWCONF" USING CWSQLC
                                                CWCONF-REG
                                                FS-CWCONF
                                                KCO PCO
                       END-IF
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
2807                      MOVE SALVA-END   TO CWCONF-CHAVE
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
                              MOVE "Fim da leitura" TO CWSEND-MSG
                          END-IF
                     ELSE
                          MOVE SPACES TO CWCONF-PROG-GRUPO
                          MOVE "E" TO CWCONF-ADM
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
                IF USUARIOS = 1
                   EXEC COBOLware SEND
                        MSG 'Grupo cont‚m um usu rio'
                   END-EXEC
                ELSE
                   MOVE SPACES TO MSG(5)
                   PERFORM VARYING U FROM 1 BY 1
                     UNTIL USUARIOS (U:1) NOT = '0'
                        MOVE SPACE TO USUARIOS(U:1)
                   END-PERFORM
                   STRING 'Grupo cont‚m '
                          USUARIOS (U:)
                          ' usu rios '
                          DELIMITED BY SIZE
                          INTO MSG(5)
                   EXEC COBOLware SEND
                        MSG MSG(5)
                   END-EXEC
                END-IF
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
           DISPLAY CWMENAE
           EXEC COBOLware OBJECT DROP END-EXEC.

       170-99-FIM. EXIT.

       180-RESTRICOES.

           MOVE CWCONF-CHAVE TO SALVA-CHAVE
           IF   GRUPO = SPACES
                GO TO 180-99-FIM
           END-IF

           PERFORM 184-RELOAD  THRU 184-99-FIM
           MOVE 1 TO POSICAO
           MOVE 0 TO TECLA
           PERFORM TEST AFTER UNTIL F2 OR ESC OR CONSULTA-R
                   INITIALIZE CWMENW-REG
                   MOVE POSICAO TO CWMENX-ITEM
                   MOVE 0       TO I
                   PERFORM 11 TIMES
                      READ CWMENX
                      IF FS-CWMENX < '09'
                         ADD  1                TO I
                         MOVE CWMENX-PROGRAMA  TO CWMENW-PROGRAMA   (I)
                         MOVE CWMENX-ACESSO    TO CWCHECK-ACESSO    (I)
                         MOVE CWMENX-ALTERACAO TO CWCHECK-ALTERACAO (I)
                         MOVE CWMENX-CONSULTA  TO CWCHECK-CONSULTA  (I)
                         MOVE CWMENX-EXCLUSAO  TO CWCHECK-EXCLUSAO  (I)
                         MOVE CWMENX-INCLUSAO  TO CWCHECK-INCLUSAO  (I)
                         MOVE CWMENX-DESCRICAO TO CWMENW-DESCRICAO  (I)
                         IF CWMENX-DUPLICADO = '1'
                            EXEC COBOLware SETFOCUS PROTECT
                                 FIELD CWCHECK-ACESSO
                                 SUBSCRIPT I
                            END-EXEC
                            EXEC COBOLware SETFOCUS PROTECT
                                 FIELD CWCHECK-ALTERACAO
                                 SUBSCRIPT I
                            END-EXEC
                            EXEC COBOLware SETFOCUS PROTECT
                                 FIELD CWCHECK-EXCLUSAO
                                 SUBSCRIPT I
                            END-EXEC
                            EXEC COBOLware SETFOCUS PROTECT
                                 FIELD CWCHECK-INCLUSAO
                                 SUBSCRIPT I
                            END-EXEC
                         ELSE
                            IF   DESCE = 1
                                 EXEC COBOLware SETFOCUS
                                      FIELD CWCHECK-INCLUSAO
                                      SUBSCRIPT I
                                 END-EXEC
                            END-IF
                         END-IF
                         ADD  1                TO CWMENX-ITEM
                      ELSE
                         EXIT PERFORM
                      END-IF
                   END-PERFORM
                   MOVE 0 TO DESCE
                   PERFORM OUTROS-OBJETOS
                   EVALUATE I
                       WHEN 1  DISPLAY T-1
                       WHEN 2  DISPLAY T-2
                       WHEN 3  DISPLAY T-3
                       WHEN 4  DISPLAY T-4
                       WHEN 5  DISPLAY T-5
                       WHEN 6  DISPLAY T-6
                       WHEN 7  DISPLAY T-7
                       WHEN 8  DISPLAY T-8
                       WHEN 9  DISPLAY T-9
                       WHEN 10 DISPLAY T-10
                       WHEN 11 DISPLAY CTAC-VAR-CWMENA
                   END-EVALUATE
                   IF  NOT CONSULTA-R
                       EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                            LINE 07 COLUMN 36 KEY F4 TAB-OFF
                            CAPTION " f4-~Tudo " WIDTH 9
                       END-EXEC
                       EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                            LINE 07 COLUMN 46 KEY F5 TAB-OFF
                            CAPTION " f5-~Nada " WIDTH 9
                       END-EXEC
                       EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                            LINE 07 COLUMN 56 KEY F6 TAB-OFF
                            CAPTION " f6-~Copiar " WIDTH 12
                       END-EXEC
                       IF COPIADO = 1
                          EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                               LINE 07 COLUMN 68 KEY F7 TAB-OFF
                               CAPTION " f7-co~Lar " WIDTH 11
                          END-EXEC
                       END-IF
                   END-IF
                   MOVE 0 TO TECLOU
                   PERFORM TEST AFTER UNTIL F2 OR CONSULTA-R
                              OR ESC
                              OR PAGE-UP
                              OR PAGE-DOWN
                              OR CONTROL-PAGE-UP
                              OR CONTROL-PAGE-DOWN
                              OR CURSOR-UP
                              OR CURSOR-DOWN
                           MOVE 0 TO TECLOU
                           IF  NOT CONSULTA-R
                               EVALUATE I
                                   WHEN 1  ACCEPT T-1
                                           MOVE 1 TO TECLOU
                                   WHEN 2  ACCEPT T-2
                                           MOVE 1 TO TECLOU
                                   WHEN 3  ACCEPT T-3
                                           MOVE 1 TO TECLOU
                                   WHEN 4  ACCEPT T-4
                                           MOVE 1 TO TECLOU
                                   WHEN 5  ACCEPT T-5
                                           MOVE 1 TO TECLOU
                                   WHEN 6  ACCEPT T-6
                                           MOVE 1 TO TECLOU
                                   WHEN 7  ACCEPT T-7
                                           MOVE 1 TO TECLOU
                                   WHEN 8  ACCEPT T-8
                                           MOVE 1 TO TECLOU
                                   WHEN 9  ACCEPT T-9
                                           MOVE 1 TO TECLOU
                                   WHEN 10 ACCEPT T-10
                                           MOVE 1 TO TECLOU
                                   WHEN 11 ACCEPT CTAC-VAR-CWMENA
                                           MOVE 1 TO TECLOU
                               END-EVALUATE
                               ACCEPT TECLA FROM ESCAPE KEY
                               IF  F9
                                   SET CURSOR-DOWN TO TRUE
                               END-IF
                               IF  (F4 OR F5)
                                   PERFORM 183-TUDO-NADA THRU 183-99-FIM
                                   EXIT PERFORM
                               END-IF
                               MOVE CWCONF-NOME-GRUPO TO ELEMENTO
                               IF  F6
                                   IF COPIADO = 0
                                      MOVE 1  TO COPIADO
                                      EXEC COBOLware OBJECT PUSH-BUTTON
                                           SMALL
                                        LINE 07 COLUMN 68 KEY F7 TAB-OFF
                                        CAPTION " f7-co~Lar " WIDTH 11
                                      END-EXEC
                                   ELSE
                                      CLOSE CWORK
                                      DELETE FILE CWORK
                                   END-IF
                                   OPEN I-O CWORK
                                   MOVE 0 TO CWMENX-ITEM
                                   START CWMENX KEY NOT < CWMENX-ITEM
                                   PERFORM UNTIL FS-CWMENX > '09'
                                        READ CWMENX NEXT RECORD
                                        IF FS-CWMENX < '10'
                                           WRITE CWORK-REG
                                            FROM CWMENX-REG
                                        END-IF
                                   END-PERFORM
                                   EXIT PERFORM
                               END-IF
                               IF  F7
                                   CLOSE CWMENX
                                   DELETE FILE CWMENX
                                   OPEN I-O CWMENX
                                   MOVE 0 TO CWORK-ITEM
                                   START CWORK KEY NOT < CWORK-ITEM
                                   PERFORM UNTIL FS-CWORK > '09'
                                        READ CWORK NEXT RECORD
                                        IF FS-CWORK < '10'
                                           WRITE CWMENX-REG
                                            FROM CWORK-REG
                                        END-IF
                                   END-PERFORM
                                   EXIT PERFORM
                               END-IF
                               IF ESC
                                  SET F3 TO TRUE
                                  MOVE SPACE TO COMANDO
                                  EXEC COBOLware Send
                                       Message "Abortar altera‡Æo ?"
                                       Option 2
                                       Caption(1) "   ~Ok___"
                                       Caption(2) "~Cancelar"
                                       OPTION-CHAR;COMANDO
                                  END-EXEC
                                  IF  COMANDO = 'O'
                                      MOVE CWCONF-CHAVE TO SALVA-CHAVE
                                      PERFORM 184-RELOAD
                                         THRU 184-99-FIM
                                      SET ESC TO TRUE
                                  END-IF
                                  EXIT PERFORM
                               END-IF
                           END-IF
                   END-PERFORM
                   IF ESC
                      MOVE 1 TO FL-EXIT
                   END-IF
                   EXEC COBOLware OBJECT DROP END-EXEC
                   IF F4 OR F5 OR F7
                      MOVE 0 TO TECLA
                      EXIT PERFORM CYCLE
                   ELSE
                      MOVE 0 TO Y
                      MOVE POSICAO TO CWMENX-ITEM
                      PERFORM I TIMES
                              ADD 1 TO Y
                              READ CWMENX
                              IF FS-CWMENX < '10'
                                 MOVE CWCHECK-ACESSO    (Y)
                                   TO CWMENX-ACESSO
                                 MOVE CWCHECK-ALTERACAO (Y)
                                   TO CWMENX-ALTERACAO
                                 MOVE CWCHECK-CONSULTA  (Y)
                                   TO CWMENX-CONSULTA
                                 MOVE CWCHECK-EXCLUSAO  (Y)
                                   TO CWMENX-EXCLUSAO
                                 MOVE CWCHECK-INCLUSAO  (Y)
                                   TO CWMENX-INCLUSAO
                                 REWRITE CWMENX-REG
                                 ADD 1 TO CWMENX-ITEM
                              END-IF
                      END-PERFORM
                   END-IF
                   EVALUATE TRUE
                      WHEN TECLOU = 0
                           CONTINUE
                      WHEN CURSOR-UP AND POSICAO > 1
                           IF POSICAO > 1
                              SUBTRACT 1 FROM POSICAO
                           END-IF
                      WHEN CURSOR-DOWN
                       AND POSICAO < ITEM-max
                           ADD  1 TO POSICAO
                           MOVE 1 TO DESCE
                      WHEN PAGE-UP AND POSICAO > 1
                           IF POSICAO > 11
                              SUBTRACT 11 FROM POSICAO
                           ELSE
                              MOVE 1 TO POSICAO
                           END-IF
                      WHEN PAGE-DOWN
                        AND POSICAO < ITEM-MAX
                            ADD 11 TO POSICAO
                            IF POSICAO NOT < ITEM-MAX
                               IF (POSICAO - 11) > 0
                                  COMPUTE POSICAO = POSICAO - 11
                                  IF POSICAO = 1
                                     ADD 1 TO POSICAO
                                  END-IF
                               ELSE
                                  IF ITEM-MAX > 11
                                     COMPUTE POSICAO = ITEM-MAX - 10
                                  ELSE
                                     MOVE 1 TO POSICAO
                                  END-IF
                               END-IF
                            END-IF
                      WHEN CONTROL-PAGE-UP
                           MOVE 1 TO POSICAO
                      WHEN CONTROL-PAGE-DOWN
                           IF (POSICAO - 11) > 0
                              COMPUTE POSICAO = POSICAO - 11
                           ELSE
                              IF ITEM-MAX > 11
                                 COMPUTE POSICAO = ITEM-MAX - 11
                              ELSE
                                 MOVE 1 TO POSICAO
                              END-IF
                           END-IF
                   END-EVALUATE
           END-PERFORM.

       180-99-FIM. EXIT.

       181-CHECK-GRUPO.

           SET CWSQLC-READ      TO TRUE
           SET CWSQLC-EQUAL     TO TRUE
           MOVE "GU"            TO CWCONF-REGGU
           MOVE GRUPO           TO CWCONF-NOME-GRUPO
           MOVE CWMENX-PROGRAMA TO CWCONF-PROG-GRUPO
           CALL "CWCONF" USING CWSQLC
                               CWCONF-REG
                               FS-CWCONF
                               KCO PCO
           IF   FS-CWCONF < "10"
                MOVE CWCONF-GRUPO-ID TO CWMENX-ID
                INSPECT CWMENX-ID
                        CONVERTING X"2AFE20" TO "110"
                IF   ADM = "I"
                     MOVE  "0" TO CWMENX-ACESSO
                END-IF
                IF CWMENX-ACESSO = '1'
                   MOVE "1" TO CWMENX-ACESSO
                END-IF
           ELSE
                MOVE  "0" TO CWMENX-ACESSO
                             CWMENX-ALTERACAO
                             CWMENX-EXCLUSAO
                             CWMENX-INCLUSAO
                IF   ADM = "I"
                     MOVE  "1" TO CWMENX-ACESSO
                END-IF
           END-IF

           MOVE '0' TO CWMENX-CONSULTA.
           REWRITE CWMENX-REG.

       181-99-FIM. EXIT.

       OUTROS-OBJETOS.

           COPY CWESCP REPLACING 23 BY footline.
           EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                LINE footline COLUMN 02 CAPTION " f2-~Aceita " WIDTH 11
                KEY F2 TAB-OFF
           END-EXEC
           EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                LINE footline COLUMN 14 CAPTION " pgup "      WIDTH 06
                KEY PAGE-UP TAB-OFF
           END-EXEC
           EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                LINE footline COLUMN 21 CAPTION " pgdn "      WIDTH 06
                KEY PAGE-DOWN TAB-OFF
           END-EXEC
           EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                LINE footline COLUMN 28 CAPTION " ctrl-pg~Up " WIDTH 11
                KEY CONTROL-PAGE-UP TAB-OFF
           END-EXEC
           EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                LINE footline COLUMN 39 CAPTION " ctrl-pg~Dn " WIDTH 11
                KEY CONTROL-PAGE-DOWN TAB-OFF
           END-EXEC
           EXEC COBOLware OBJECT PUSH-BUTTON SMALL
             LINE footline COLUMN 50 CAPTION SETA-UP WIDTH 1
             KEY CURSOR-UP TAB-OFF
           END-EXEC
           EXEC COBOLware OBJECT PUSH-BUTTON SMALL
             LINE footline COLUMN 52 CAPTION SETA-DOWN WIDTH 1
             KEY F9 TAB-OFF
           END-EXEC
           PERFORM VARYING I FROM 11 BY -1 UNTIL I = 1
                   OR CWMENW-PROGRAMA(I) NOT = SPACES
                      PERFORM 182-APAGA THRU 182-99-FIM
           END-PERFORM.

       FIM-OUTROS-OBJETOS. EXIT.

       182-APAGA.

           MOVE SPACES TO CARACTER-BUFFER
           COMPUTE ROW-CURSOR = I + 8
           CALL "CBL_WRITE_SCR_CHARS" USING CURSOR-POSITION
                                            CARACTER-BUFFER
                                            STRING-LENGTH.

       182-99-FIM. EXIT.

       183-TUDO-NADA.

           MOVE 0 TO CWMENX-ITEM
           START CWMENX KEY NOT LESS CWMENX-ITEM
           PERFORM UNTIL FS-CWMENX > '09'
                   READ CWMENX NEXT RECORD
                   IF FS-CWMENX < '10'
                      IF F4
                         MOVE '1' TO CWMENX-ACESSO
                      ELSE
                         MOVE '0' TO CWMENX-ACESSO
                                     CWMENX-ALTERACAO
                                     CWMENX-CONSULTA
                                     CWMENX-EXCLUSAO
                                     CWMENX-INCLUSAO
                      END-IF
                      REWRITE CWMENX-REG
                   END-IF
           END-PERFORM.

       183-99-FIM. EXIT.

       184-RELOAD.

           MOVE 0            TO I ITEM-MAX
           MOVE LOW-VALUES   TO CWMENX-REG
           START CWMENX KEY NOT LESS CWMENX-ITEM
           PERFORM UNTIL FS-CWMENX > "09"
                   READ CWMENX NEXT RECORD
                   IF  FS-CWMENX < "10"
                   AND CWMENX-DUPLICADO = '0'
                       MOVE CWMENX-ITEM TO ITEM-MAX
                       PERFORM 181-CHECK-GRUPO THRU 181-99-FIM
                   END-IF
           END-PERFORM

           MOVE SALVA-CHAVE TO CWCONF-CHAVE
           SET CWSQLC-READ TO TRUE
           SET CWSQLC-EQUAL TO TRUE
           CALL "CWCONF" USING CWSQLC
                               CWCONF-REG
                               FS-CWCONF
                               KCO PCO.

       184-99-FIM. EXIT.

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
                   IF  (CWCONF-TIPO NOT = "GU")
                   OR  (CWCONF-NOME-GRUPO NOT = GRUPO)
                        MOVE "10" TO FS-CWCONF
                   ELSE
                        IF  (CWCONF-PROG-GRUPO NOT = SPACES)
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
                MOVE 0 TO LOCK-LEVEL CWMENX-ITEM
                START CWMENX KEY NOT LESS CWMENX-ITEM
                PERFORM TEST AFTER
                             UNTIL FS-CWMENX > "09"
                   READ CWMENX NEXT RECORD
                   IF   FS-CWMENX < "10"
                   AND  CWMENX-DUPLICADO = '0'
                        PERFORM VARYING NIVEL FROM CWMENX-NIVEL BY 1
                                UNTIL NIVEL > 7
                                MOVE ALL '0' TO NIVEL-ID (NIVEL)
                        END-PERFORM
                        IF  CWMENX-PROGRAMA (1:1) = ':'
                            MOVE CWMENX-ACESSO
                              TO NIVEL-ACESSO    (CWMENX-NIVEL)
                            MOVE CWMENX-ALTERACAO
                              TO NIVEL-ALTERACAO (CWMENX-NIVEL)
                            MOVE CWMENX-EXCLUSAO
                              TO NIVEL-EXCLUSAO  (CWMENX-NIVEL)
                            MOVE CWMENX-INCLUSAO
                              TO NIVEL-INCLUSAO  (CWMENX-NIVEL)
                        END-IF
                        PERFORM VARYING NIVEL FROM 1 BY 1
                           UNTIL NIVEL NOT LESS CWMENX-NIVEL
                           IF NIVEL-ACESSO (NIVEL) = '1'
                              MOVE '1' TO CWMENX-ACESSO
                           END-IF
                           IF NIVEL-ALTERACAO (NIVEL) = '1'
                              MOVE '1' TO CWMENX-ALTERACAO
                           END-IF
                           IF NIVEL-EXCLUSAO (NIVEL) = '1'
                              MOVE '1' TO CWMENX-EXCLUSAO
                           END-IF
                           IF NIVEL-INCLUSAO (NIVEL) = '1'
                              MOVE '1' TO CWMENX-INCLUSAO
                           END-IF
                        END-PERFORM
                        IF CWMENX-ID NOT = ALL '0'
                           MOVE "GU"  TO CWCONF-REG
                           MOVE GRUPO TO CWCONF-NOME-GRUPO
                           MOVE CWMENX-PROGRAMA TO CWCONF-PROG-GRUPO
                           MOVE CWMENX-ID       TO CWCONF-GRUPO-ID
                           INSPECT CWCONF-REGGU(33: 5)
                                   CONVERTING "0" TO SPACE
                           SET CWSQLC-WRITE TO TRUE
                           CALL "CWCONF" USING CWSQLC
                                               CWCONF-REG
                                               FS-CWCONF
                                               KCO PCO
                        END-IF
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
           EXEC COBOLware GetSystem
                Level;CLASSE-GU
           END-EXEC
           CALL "CWGETU" USING NOME TASK PROGRAMA "?"
           CALL "CWUNIX" USING PARAMETROS-CWUNIX
           IF CWUNIX-ON
              MOVE 'v' TO SETA-DOWN
              MOVE '^' TO SETA-UP
           ELSE
              IF CWUNIX-GUI
                 MOVE 0 TO BRANCO
              END-IF
           END-IF
           MOVE SPACES TO LB-CWMENX
                          LB-CWORK
           STRING '$TEMP/CWMENAx' TASK DELIMITED BY SIZE INTO LB-CWMENX
           STRING '$TEMP/CWMENAy' TASK DELIMITED BY SIZE INTO LB-CWORK
           SET CWSQLC-UPDATE TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           IF   FS-CWCONF NOT EQUAL "00"
                CALL "CWCONF" USING "ISAM"
                EXIT PROGRAM
           END-IF

           OPEN I-O CWMENX
           MOVE "GU"  TO CWCONF-REG
           SET CWSQLC-START     TO TRUE
           SET CWSQLC-NOT-LESS  TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG99
                               FS-CWCONF KCO PCO
           PERFORM TEST AFTER UNTIL FS-CWCONF > "09"
                                 OR (CWCONF-TIPO NOT = "GU")
                   SET CWSQLC-READ TO TRUE
                   SET CWSQLC-NEXT TO TRUE
                   SET CWSQLC-IGNORE-LOCK TO TRUE
                   CALL "CWCONF" USING CWSQLC CWCONF-REG
                                           FS-CWCONF KCO PCO
                   IF   FS-CWCONF < "10"
                   AND  CWCONF-TIPO = "GU"
                   AND  CWCONF-NOME-GRUPO = SPACES
                        SET CWSQLC-DELETE TO TRUE
                        CALL "CWCONF" USING CWSQLC CWCONF-REG
                                                FS-CWCONF KCO PCO

                   END-IF
           END-PERFORM
           MOVE "99"  TO CWCONF-REG
           SET CWSQLC-START     TO TRUE
           SET CWSQLC-NOT-LESS  TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG99
                               FS-CWCONF KCO PCO
           PERFORM TEST AFTER UNTIL FS-CWCONF > "09"
                                 OR((CWCONF-TIPO NOT = "99")
                                 AND(CWCONF-TIPO NOT = "SM"))
                   SET CWSQLC-READ TO TRUE
                   SET CWSQLC-NEXT TO TRUE
                   SET CWSQLC-IGNORE-LOCK TO TRUE
                   CALL "CWCONF" USING CWSQLC CWCONF-REG
                                           FS-CWCONF KCO PCO
                   IF   FS-CWCONF < "10"
                   AND  CWCONF-TIPO = "99"
                        INITIALIZE SALVAS
                        MOVE 1 TO NIVEL
                        PERFORM VARYING k FROM 1 BY 1
                           UNTIL k > 26
                           IF  CWCONF-PROG (k) NOT = SPACES
                               CALL "CWCODE" USING "D"
                                   CWCONF-SIZE-P-99  (k)
                                   CWCONF-FATOR-P-99 (k)
                                   CWCONF-PROG (k)
                               INSPECT CWCONF-PROG (k)
                                       CONVERTING MINUSCULAS
                                               TO MAIUSCULAS

      *                         IF CWCONF-PROG (k) = 'CWPAGE'
      *                                           OR 'GRPAGE'
      *                            MOVE SPACES TO CWCONF-PROG (k)
      *                         END-IF
                           END-IF
                        END-PERFORM
                        PERFORM VARYING Y FROM 26 BY -1
                                UNTIL CWCONF-PROG(Y)
                                      NOT = SPACES
                                    OR Y = 1
                                CONTINUE
                        END-PERFORM
                        MOVE Y TO MAX-i (NIVEL)
                        PERFORM VARYING I FROM 1 BY 1
                           UNTIL NIVEL = 0
                              OR I > 26
                            MOVE CWCONF-PROG (I) TO PROGRAMA
                            IF (PROGRAMA NOT = SPACES)
                            AND(PROGRAMA NOT = ALL '_')
                            IF  PROGRAMA = 'CWBOXS' OR 'GRBOXS'
                                IF  NIVEL < 7
                                    MOVE I            TO SAVE-I (NIVEL)
                                    MOVE CWCONF-CHAVE
                                      TO SAVE-CHAVE(NIVEL)
                                    PERFORM 810-cria-lista
                                       thru 810-99-fim
                                    PERFORM 830-TOPAGINA
                                ELSE
                                    EXIT PERFORM CYCLE
                                END-IF
                            ELSE
                                PERFORM 810-cria-lista thru 810-99-fim
                            END-IF
                            END-IF
                            IF  NIVEL > 1
                            AND (I NOT < MAX-I (NIVEL))
                                SUBTRACT 1 FROM NIVEL
                                MOVE SAVE-CHAVE(NIVEL) TO CWCONF-REG99
                                MOVE SAVE-I (NIVEL)    TO I
                                SET CWSQLC-READ TO TRUE
                                SET CWSQLC-EQUAL TO TRUE
                                CALL "CWCONF" USING CWSQLC CWCONF-REG99
                                                    FS-CWCONF KCO PCO
                                PERFORM VARYING k FROM 1 BY 1
                                   UNTIL k > 26
                                         OR FS-CWCONF > '09'
                                   IF  CWCONF-PROG (k) NOT = SPACES
                                       CALL "CWCODE" USING "D"
                                           CWCONF-SIZE-P-99  (k)
                                           CWCONF-FATOR-P-99 (k)
                                           CWCONF-PROG (k)
                                       INSPECT CWCONF-PROG (k)
                                               CONVERTING MINUSCULAS
                                                       TO MAIUSCULAS
                                   END-IF
                                END-PERFORM
                            END-IF
                        END-PERFORM
                   END-IF
           END-PERFORM.

           IF   PROGRAMAS = 0
                MOVE SPACES      TO CWSEND-SCREENS
                MOVE "Tabela de programas vazia"
                                 TO CWSEND-MSG
                CALL "CWSEND" USING PARAMETROS-CWSEND
           END-IF.

       800-99-FIM. EXIT.

       810-CRIA-LISTA.

           IF   NOT MODULO-CWMENU
                MOVE CWCONF-NM-OPCAO(I) TO TESTE-OPCAO
                MOVE SPACES TO STRING-1
                MOVE SPACES              TO PATH
                MOVE 0                   TO K
                MOVE PROGRAMA            TO STRING-2 TESTE-OPCAO
                IF   PROGRAMA = "CWBOXS" OR "GRBOXS"  OR "GRPAGE"
                                                      OR "CWPAGE"
                     IF   TESTE-OPCAO = "/"
                          MOVE CWCONF-NM-OPCAO(I) (3: ) TO STRING-1
                     ELSE
                          MOVE CWCONF-NM-OPCAO(I)       TO STRING-1
                     END-IF
                     IF   PROGRAMA = 'CWPAGE' OR 'GRPAGE'
                          MOVE ";"        TO STRING-2
                     ELSE
                          MOVE ":"        TO STRING-2
                     END-IF
                     MOVE CWCONF-HELP (I) TO STRING-2 (2: )
                     MOVE SPACES          TO PROGRAMA
                     PERFORM VARYING K FROM 1 BY 1
                               UNTIL K > NIVEL
                             IF K = NIVEL
                                IF I = MAX-i (NIVEL)
                                   MOVE 'À' TO PATH(K:)
                                ELSE
                                   MOVE 'Ã' TO PATH(K:)
                                   IF ITENS (K) = 0
                                   AND K = 1
                                      MOVE 'Ú' TO PATH(K:)
                                   END-IF
                                END-IF
                             ELSE
                                IF  SAVE-I (K) < MAX-i (K)
                                    MOVE '³' TO PATH(K:)
                                ELSE
                                    MOVE X'FF' TO PATH(K:)
                                END-IF
                             END-IF
                     END-PERFORM
                ELSE
                     IF   TESTE-OPCAO = "/"
                          MOVE CWCONF-NM-OPCAO(I)(3: ) TO STRING-1 (2:)
                     ELSE
                          MOVE CWCONF-NM-OPCAO(I)      TO STRING-1 (2:)
                     END-IF
                     IF NIVEL = 0
                        MOVE 'Ä' TO PATH
                     ELSE
                        PERFORM VARYING K FROM 1 BY 1
                                  UNTIL K > NIVEL
                                IF K = NIVEL
                                   IF I = MAX-i (K)
                                      MOVE 'À' TO PATH(K:)
                                   ELSE
                                      MOVE 'Ã' TO PATH(K:)
                                      IF ITENS (K) = 0
                                      AND K = 1
                                         MOVE 'Ú' TO PATH(K:)
                                      END-IF
                                   END-IF
                                ELSE
                                   IF SAVE-I (K) = MAX-i (K)
                                      MOVE X'FF' TO PATH(K:)
                                   ELSE
                                      MOVE '³' TO PATH(K:)
                                   END-IF
                                END-IF
                        END-PERFORM
                     END-IF
                     IF   PROGRAMA = "CWREL2"
                     AND (CWCONF-HELP (I) NOT = SPACES)
                         MOVE "*"             TO STRING-2
                         MOVE CWCONF-HELP (I) TO STRING-2 (2: )
                         INSPECT STRING-2 (2: )
                                 CONVERTING MINUSCULAS TO MAIUSCULAS
                     END-IF
                END-IF
                MOVE SPACES   TO CWMENX-REG
                MOVE ALL '0'  TO CWMENX-FLAGS
                ADD  1        TO ITEM ITENS (NIVEL)
                MOVE ITEM     TO CWMENX-ITEM
                MOVE NIVEL    TO CWMENX-NIVEL
                PERFORM 820-AJUSTA-STRING THRU 820-99-FIM
                PERFORM VARYING K FROM LENGTH PROGRAMA BY -1
                        UNTIL PROGRAMA (K:1) NOT = SPACE
                        CONTINUE
                END-PERFORM
                STRING PATH DELIMITED BY SPACE
                       PROGRAMA (1: K) DELIMITED BY SIZE
                       STRING-1 DELIMITED BY SIZE INTO CWMENX-DESCRICAO
                INSPECT CWMENX-DESCRICAO CONVERTING HIGH-VALUE TO SPACE
                MOVE STRING-2 TO CWMENX-PROGRAMA
                IF STRING-2 (1:1) = ':' OR ';'
                   PERFORM VARYING K FROM LENGTH CWMENX-DESCRICAO
                                    BY -1 UNTIL K = 1
                                OR (CWMENX-DESCRICAO (K:1) NOT = SPACE)
                           CONTINUE
                   END-PERFORM
                   ADD 1 TO K
                   PERFORM VARYING YY FROM 1 BY 1
                      UNTIL YY > LENGTH STRING-P
                        OR (STRING-P (YY: 1) NUMERIC
                        AND STRING-P (YY: 1) > '0')
                        CONTINUE
                   END-PERFORM
                   STRING '('            DELIMITED BY SIZE
                          STRING-P (YY:) DELIMITED BY SPACE
                          ')'            DELIMITED BY SIZE
                                    INTO CWMENX-DESCRICAO(K:)
                END-IF
                ADD  1        TO PROGRAMAS
                WRITE CWMENX-REG
                IF FS-CWMENX = '02'
                   MOVE '1' TO CWMENX-DUPLICADO
                   REWRITE CWMENX-REG
                END-IF
                CALL 'CWATCH'
           END-IF.

       810-99-FIM. EXIT.

       820-AJUSTA-STRING.

           MOVE SPACES TO NEW-ITEM
           MOVE 0      TO K2
           PERFORM VARYING K FROM 1 BY 1 UNTIL K > 35
                   IF   STRING-1 (K: 1) = X"7E"
                        ADD 1 TO K
                   END-IF
                   ADD 1 TO K2
                   MOVE STRING-1 (K: 1) TO NEW-ITEM (K2: 1)
           END-PERFORM
           MOVE NEW-ITEM    TO STRING-1.

       820-99-FIM. EXIT.

       830-TOPAGINA.

           MOVE 0        TO PAGINAX
           MOVE 7        TO SM-FIL
           MOVE 5        TO YP
           INITIALIZE SUB-CWCONF
           PERFORM VARYING IP FROM 20 BY -1
                   UNTIL IP = 0
                      OR SM-FIL < 1
                   IF IP = 20
                   AND cwconf-help(i) (5:1) = ':'
                       MOVE 4 TO IP
                   END-IF
                   MOVE cwconf-help(i) (IP: 1) TO TP
                   IF TP NUMERIC
                   AND YP > 1
                       SUBTRACT 1 FROM YP
                       MOVE     TP  TO PAGINAX (YP: 1)
                   ELSE
                       IF   TP = ","
                            MOVE PAGINAX TO SM-ATT (SM-FIL)
                            MOVE 0       TO PAGINAX
                            MOVE 5       TO YP
                            SUBTRACT 1 FROM SM-FIL
                       END-IF
                   END-IF
           END-PERFORM

           IF   SM-FIL NOT = 7
                MOVE PAGINAX TO SM-ATT (SM-FIL)
                IF  SM-FIL NOT = 1
                    COMPUTE U = (SM-FIL - 1) * 4 + 1
                    MOVE SUB-CWCONF (U: )      TO SUB-CWCONF-2
                    MOVE SUB-CWCONF-2          TO SUB-CWCONF
                    INSPECT SUB-CWCONF CONVERTING SPACE
                                               TO ZERO
                END-IF
           END-IF
           IF   SUB-CWCONF NOT = ZEROS
                MOVE SM-ATT (1) TO PAGINAX
           END-IF
           IF CWCONF-PROG(I) = 'CWBOXS' OR 'GRBOXS'
              MOVE 'SM' TO CWCONF-REG99
           END-IF
           MOVE PAGINAX TO CWCONF-PAGINA
           SET CWSQLC-READ TO TRUE
           SET CWSQLC-EQUAL TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG99
                               FS-CWCONF KCO PCO
           PERFORM VARYING k FROM 1 BY 1
              UNTIL k > 26
                 OR FS-CWCONF > '09'
              IF  CWCONF-PROG (k) NOT = SPACES
                  CALL "CWCODE" USING "D"
                      CWCONF-SIZE-P-99  (k)
                      CWCONF-FATOR-P-99 (k)
                      CWCONF-PROG (k)
                  INSPECT CWCONF-PROG (k)
                          CONVERTING MINUSCULAS
                                  TO MAIUSCULAS
              END-IF
           END-PERFORM
           IF  FS-CWCONF > '09'
               EXIT PARAGRAPH
           END-IF
           MOVE CWCONF-CHAVE TO CWMENX-CHAVE
           REWRITE CWMENX-REG
           ADD  1 TO NIVEL
           INITIALIZE SAVEX (NIVEL)
           PERFORM VARYING Y FROM 26 BY -1
                   UNTIL CWCONF-PROG(Y)
                         NOT = SPACES
                       OR Y = 1
                   CONTINUE
           END-PERFORM
           MOVE Y TO MAX-i (NIVEL)
           MOVE 0 TO I SAVE-I (NIVEL).

       900-FINAIS.

           EXEC COBOLware OBJECT DROP END-EXEC
           SET CWSQLC-CLOSE TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           CLOSE CWMENX
           DELETE FILE CWMENX.

       900-99-FIM. EXIT.

       END PROGRAM CWMENA.

