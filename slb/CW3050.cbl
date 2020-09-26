       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CW3050.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  18/01/2001.
       SECURITY.      *************************************************
                      *                                               *
                      *  Importa CWCONFIG da versao 3.0               *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CONFIG ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS CONFIG-CHAVE
                  LOCK MODE     IS MANUAL
                  FILE STATUS   IS FS-CONFIG.

           SELECT CWINI  ASSIGN TO DISK
                  ORGANIZATION  IS BINARY SEQUENTIAL
                  FILE STATUS   IS FS-CWINI.

       DATA DIVISION.
       FILE SECTION.

      ******************************************************************
      *          CONFIguracao do COBOLware 3.0                         *
      ******************************************************************

       FD  CONFIG
           LABEL RECORD IS STANDARD
           RECORDING MODE IS V
           RECORD CONTAINS 32 TO 1956 CHARACTERS
           VALUE OF FILE-ID IS LB-CONFIG.

       01  CONFIG-REG.
           05 CONFIG-CHAVE.
              10 CONFIG-TIPO                        PIC  X(002).
              10 CONFIG-ELEMENTO                    PIC  X(030).

       01  CONFIG-REG00.
           05 FILLER                                PIC  X(032).
           05 CONFIG-FATOR-00                COMP-X PIC  9(002).
           05 CONFIG-USERID.
              10 CONFIG-USUARIO                     PIC  X(030).
              10 CONFIG-SISTEMA                     PIC  X(030).
           05 REDEFINES CONFIG-USERID.
              10 CONFIG-BYTE-U     OCCURS 60 COMP-X PIC  9(002).
           05 CONFIG-QUADRO                         PIC  9(001).
           05 CONFIG-EJECT-MODE-OLD                 PIC  X(002).

       01  CONFIG-REG02.
           05 FILLER                                PIC  X(002).
           05 CONFIG-ARQUIVO                        PIC  X(030).
           05 CONFIG-LABEL.
              10 CONFIG-LABEL-BYTE OCCURS 50        PIC  X(001).
           05 CONFIG-EJECT-MODE                     PIC  X(002).
           05 CONFIG-ASCII                          PIC  X(001).
           05 CONFIG-CADEIA-ASCII-INICIAL.
              10 CONFIG-ASCII-I    OCCURS 50 COMP-X PIC  9(002).
           05 CONFIG-CADEIA-ASCII-FINAL.
              10 CONFIG-ASCII-F    OCCURS 50 COMP-X PIC  9(002).
           05 CONFIG-ESTILO                         PIC  X(030).
      * Utlizado tambem com reg 03 (tabela de impressoras)

       01  CONFIG-REG92.
           05 FILLER                                PIC  X(032).
           05 CONFIG-BEEP                           PIC  X(001).
              88 CONFIG-BEEP-ON  VALUE "S" "s"
                                       "Y" "y".
           05 CONFIG-STOP                           PIC  X(001).
              88 CONFIG-STOP-ON  VALUE "S" "s"
                                       "Y" "y".
           05 CONFIG-ERASE                          PIC  X(001).
              88 CONFIG-ERASE-ON VALUE "S" "s"
                                       "Y" "y".
           05 CONFIG-TIMER                          PIC  9(002).

       01  CONFIG-REG94.
           05 FILLER                                PIC  X(002).
           05 CONFIG-RELATORIO                      PIC  X(030).
           05 REDEFINES CONFIG-RELATORIO.
              10 CONFIG-BYTE       OCCURS 30 COMP-X PIC  9(002).
           05 REDEFINES CONFIG-RELATORIO.
              10 FILLER                             PIC  X(007).
              10 CONFIG-NAME-REPORT                 PIC  X(023).
           05 CONFIG-PROGRAMA-PRINTER.
              10 CONFIG-PROGRAMA-ASCII1      COMP-X PIC  9(002).
              10 CONFIG-PROGRAMA-ASCII2      COMP-X PIC  9(002).
           05 CONFIG-DESPROGRAMA.
              10 CONFIG-DESPROGRAMA-ASCII1   COMP-X PIC  9(002).
              10 CONFIG-DESPROGRAMA-ASCII2   COMP-X PIC  9(002).
           05 CONFIG-TIPO-FORM                      PIC  X(020).
           05 CONFIG-SIZE-PAGE                      PIC  9(002).
           05 CONFIG-SAIDA                          PIC  X(015).
           05 REDEFINES CONFIG-SAIDA.
              10 CONFIG-BYTE-SAIDA OCCURS 15 COMP-X PIC  9(002).
           05 CONFIG-TITLE.
              10 CONFIG-TITLE-1                     PIC  X(043).
              10 CONFIG-TITLE-2                     PIC  X(043).
           05 CONFIG-SUB-TITLE.
              10 CONFIG-SUB-TITLE-1                 PIC  X(043).
              10 CONFIG-SUB-TITLE-2                 PIC  X(043).
           05 CONFIG-CAMPOS-AP.
              10 CONFIG-CAMPOS-APAGADOS OCCURS 6.
                 15 CONFIG-INICIO-AP                PIC  9(003).
                 15 CONFIG-FIM-AP                   PIC  9(003).
           05 CONFIG-CAMPOS-TB.
              10 CONFIG-CAMPOS-TABULADOS OCCURS 6.
                 15 CONFIG-INICIO-TB                PIC  9(003).
                 15 CONFIG-FIM-TB                   PIC  9(003).

           66 CONFIG-DADOS RENAMES CONFIG-PROGRAMA-ASCII1
                              THRU CONFIG-CAMPOS-TB.

       01  CONFIG-REG98.
           05 FILLER                                PIC  X(032).
           05 CONFIG-ESTADOS                        PIC  9(002).
           05 CONFIG-ARRAY-UF.
              10 CONFIG-ESTADO            OCCURS 50 PIC  X(002).

       01  CONFIG-REG99.
           05 FILLER                                PIC  X(002).
           05 CONFIG-PAGINA                         PIC  9(004).
           05 FILLER                                PIC  X(026).
           05 CONFIG-OPCOES.
              10 CONFIG-OPCAO OCCURS 26.
                 15 CONFIG-PROG                     PIC  X(008).
                 15 REDEFINES CONFIG-PROG.
                    20 PROG-X       OCCURS 8 COMP-X PIC  9(002).
                 15 CONFIG-NIVEL                    PIC  9(001).
                 15 CONFIG-CHECK                    PIC  X(001).
                 15 CONFIG-PASS                     PIC  X(006).
                 15 REDEFINES CONFIG-PASS.
                    20 PASS-X       OCCURS 6 COMP-X PIC  9(002).
                 15 CONFIG-HELP                     PIC  X(020).
                 15 CONFIG-FATOR             COMP-X PIC  9(002).
                 15                                 PIC  X(001).
                 15 CONFIG-NO-OPCAO                 PIC  9(002).
                 15 CONFIG-NM-OPCAO                 PIC  X(034).
                 15 REDEFINES CONFIG-NM-OPCAO.
                    20 CONFIG-BYTE-NM OCCURS 34 PIC X(001).

       01  CONFIG-REGAT.
           05 FILLER                                PIC  X(032).
           05 CONFIG-ATRIBUTOS.
              10 CONFIG-CORES OCCURS 11.
                 15 CONFIG-COR-TEXTO                PIC  9(002).
                 15 CONFIG-COR-FUNDO                PIC  9(002).

       01  CONFIG-REGJB.
           05 FILLER                                PIC  X(002).
           05 CONFIG-JOB                            PIC  X(030).
           05 CONFIG-JOB-MODULO                     PIC  X(050).
           05 CONFIG-JOB-TIPO                       PIC  9(001).
              88 CONFIG-JOB-BINARIO                           VALUE 1.
              88 CONFIG-JOB-COBOL                             VALUE 2.
           05 CONFIG-JOB-PARAMETRO                  PIC  X(050).
           05 CONFIG-JOB-TAMANHO                    PIC  9(002).
           05 CONFIG-JOB-PROXIMO-RC-OK              PIC  X(007).
           05 CONFIG-JOB-PROXIMO-NAO-OK             PIC  X(007).
           05 CONFIG-JOB-MENSAGEM                   PIC  X(079).

       01  CONFIG-REGLG.
           05 FILLER                                PIC  X(032).
           05 CONFIG-LOG                            PIC  X(001).

       01  CONFIG-REGPS.
           05 FILLER                                PIC  X(002).
           05 CONFIG-NOME                           PIC  X(030).
           05 CONFIG-SENHA                          PIC  X(006).
           05 REDEFINES CONFIG-SENHA.
              10 CONFIG-S           OCCURS 6 COMP-X PIC  9(002).
           05 CONFIG-FATOR-PS                COMP-X PIC  9(002).
           05 CONFIG-NIVEL-PS                COMP-X PIC  9(002).
           05 CONFIG-GRUPO                          PIC  X(022).
           05 CONFIG-MODO-MENU                      PIC  9(001).
           05 CONFIG-PATH-SPOOL                     PIC  X(030).
           05 CONFIG-PRINTER-DEFAULT                PIC  X(008).
           05 CONFIG-QUADRO-PS                      PIC  9(001).

       01  CONFIG-REGGU.
           05 FILLER                                PIC  X(002).
           05 CONFIG-NOME-GRUPO                     PIC  X(022).
           05 CONFIG-PROG-GRUPO                     PIC  X(008).
           05 CONFIG-ACESSO-GRUPO                   PIC  X(001).
           05 CONFIG-ALTERACAO-GRUPO                PIC  X(001).
           05 CONFIG-CONSULTA-GRUPO                 PIC  X(001).
           05 CONFIG-EXCLUSAO-GRUPO                 PIC  X(001).
           05 CONFIG-INCLUSAO-GRUPO                 PIC  X(001).
           05 CONFIG-ADM                            PIC  X(001).

       01  CONFIG-REGRT.
           05 FILLER                                PIC  X(032).
           05 CONFIG-ROTINAS.
              10 CONFIG-ROTINA            OCCURS 84 PIC  X(008).

       01  CONFIG-REGRV.
           05 FILLER                                PIC  X(032).
           05 CONFIG-REVISAO                        PIC  99V99.

       01  CONFIG-REGVD.
           05 FILLER                                PIC  X(032).
           05 CONFIG-OLD-VALIDADE                   PIC  9(006) COMP-3.
           05 CONFIG-TRAVADO                        PIC  X(001).
           05 CONFIG-SENHA-ATIVACAO                 PIC  9(006) COMP-3.
           05 CONFIG-FLAG-2000                      PIC  9(001).
           05 CONFIG-VALIDADE                       PIC  9(008) COMP-3.
           05 CONFIG-VALIDADE-10                    PIC  9(008) COMP-3.
           05 CONFIG-VALIDADE-15                    PIC  9(008) COMP-3.
           05 CONFIG-VALIDADE-20                    PIC  9(008) COMP-3.
           05 CONFIG-ULTIMO-LOGIN.
              10 CONFIG-ULTIMO-LOGIN-DATA           PIC  9(008) COMP-3.
              10 CONFIG-ULTIMO-LOGIN-HORA           PIC  9(006) COMP-3.

       01  CONFIG-REGES.
           05 FILLER                                PIC  X(032).
           05 CONFIG-ESTILOS                        PIC  X(768).

       FD  CWINI
           VALUE OF FILE-ID IS LB-CWINI.

       01  CWINI-REG                   PIC X(045).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 CWCONF-FS                PIC X(003) VALUE SPACES.
           05 PRINTER-STATUS           PIC 9(002) COMP-X VALUE 0.
           05 SIZE-OLD-DIR             PIC 9(002) COMP-X VALUE 50.
           05 OLD-DRIVE                PIC X(001) VALUE SPACE.
           05 OLD-DIRECTORY            PIC X(255) VALUE SPACES.
           05 CWCONF                   PIC X(255) VALUE SPACES.
           05 ER-CONFIG.
              10 FS-CONFIG             PIC  X(002) VALUE "00".
              10 LB-CONFIG             PIC  X(255) VALUE "./CWCONFIG".
           05 ER-CWINI.
              10 FS-CWINI              PIC  X(002) VALUE "00".
              10 LB-CWINI              PIC  X(255) VALUE SPACES.
           05 QUADRO                   PIC  9(001) VALUE ZERO.
           05 LIDOS                    PIC  9(004) VALUE ZERO.
           05 AT-ON                    PIC  9(001) VALUE ZERO.
           05 LOG                      PIC  9(001) VALUE 1.
           05 X                        PIC  X(001) VALUE SPACE.
           05 I                        PIC  9(003) VALUE ZERO.
           05 Y                        PIC  9(003) VALUE ZERO.
           05 MINUSCULAS PIC X(26) VALUE "abcdefghijklmnopqrstuvwxyz".
           05 MAIUSCULAS PIC X(26) VALUE "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
           05 PROGRAMA                 PIC  X(008).
           05 REDEFINES PROGRAMA.
              10 PK-X     OCCURS 8 COMP-X PIC  9(002).
           05 CHECK-PASS-3                         VALUE SPACES.
              10 SENHA-X3 OCCURS 6        PIC 9(2) COMP-X.
           05 TESTE-LIVRE                 PIC X(6) VALUE SPACES.
           05 ROTINAS-OBSOLETAS.
              10 PIC X(6) VALUE "GRADAY".
              10 PIC X(6) VALUE "GRIDAT".
              10 PIC X(6) VALUE "GRMATA".
              10 PIC X(6) VALUE "GRREFE".
              10 PIC X(6) VALUE "GRSDAY".
              10 PIC X(6) VALUE "GRVDAT".
              10 PIC X(6) VALUE "GRWEEK".
           05 REDEFINES ROTINAS-OBSOLETAS.
              10 OBSOLETA PIC X(6) OCCURS 7.

       COPY CWSEND.
       COPY CWUNIX.
       COPY CWCONF.

       PROCEDURE DIVISION.

       000-INICIO.

           SET CWSQLC-OPEN TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           IF   FS-CWCONF < "10"
                SET CWSQLC-CLOSE TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
                GOBACK
           END-IF
           OPEN INPUT CONFIG
           IF   FS-CONFIG > "09"
                MOVE "config.cws" TO LB-CONFIG
                OPEN INPUT CONFIG
           END-IF
           IF   FS-CONFIG > "09"
                DISPLAY "CWCONF" UPON ENVIRONMENT-NAME
                ACCEPT   CWCONF  FROM ENVIRONMENT-VALUE
                DISPLAY "CWCONF_FS" UPON ENVIRONMENT-NAME
                ACCEPT  CWCONF-FS   FROM ENVIRONMENT-VALUE
                IF CWCONF-FS = SPACES
                   DISPLAY "CWCONF-FS" UPON ENVIRONMENT-NAME
                   ACCEPT  CWCONF-FS   FROM ENVIRONMENT-VALUE
                END-IF
                INSPECT CWCONF-FS  CONVERTING MINUSCULAS TO MAIUSCULAS
                IF   CWCONF (1:1) = '.' OR '/' OR '\'
                OR   CWCONF (2:1) = ':'
                OR   CWCONF-FS = "ON"
                     IF    CWCONF-FS = "ON"
                           STRING 'Falta o arquivo de configura‡Æo "'
                                                   DELIMITED BY SIZE
                                   CWCONF          DELIMITED BY SPACE
                                   '" no FileShare'
                                                   DELIMITED BY SIZE
                              INTO CWSEND-MSG
                     ELSE
                           STRING 'Falta o arquivo de configura‡Æo "'
                                                   DELIMITED BY SIZE
                                   CWCONF          DELIMITED BY SPACE
                                   '"'             DELIMITED BY SIZE
                              INTO CWSEND-MSG
                    END-IF
                ELSE
                     CALL "CBL_READ_DIR"   USING OLD-DIRECTORY
                                                 SIZE-OLD-DIR
                     CALL "CWUNIX" USING PARAMETROS-CWUNIX
                     IF   CWUNIX-OFF
                          CALL "PC_READ_DRIVE"  USING OLD-DRIVE
                                                      PRINTER-STATUS
                          IF OLD-DIRECTORY = ' ' OR '\'
                             STRING 'Falta o arquivo de configura‡Æo "'
                                     OLD-DRIVE ':\'
                                               DELIMITED BY SIZE
                                     CWCONF    DELIMITED BY SPACE
                                     '"'       DELIMITED BY SIZE
                               INTO CWSEND-MSG
                          ELSE
                              STRING 'Falta o arquivo de configura‡Æo "'
                                      OLD-DRIVE ':\'
                                                     DELIMITED BY SIZE
                                      OLD-DIRECTORY  DELIMITED BY SPACE
                                      '\'            DELIMITED BY SIZE
                                      CWCONF         DELIMITED BY SPACE
                                      '"'            DELIMITED BY SIZE
                                 INTO CWSEND-MSG
                          END-IF
                     ELSE
                         STRING 'Falta o arquivo de configura‡Æo "'
                                                  DELIMITED BY SIZE
                               OLD-DIRECTORY      DELIMITED BY SPACE
                               '/'                DELIMITED BY SIZE
                               CWCONF             DELIMITED BY SPACE
                               '"'                DELIMITED BY SIZE
                         INTO CWSEND-MSG
                     END-IF
                END-IF
                CALL "CWSEND" USING PARAMETROS-CWSEND
                STOP RUN
           END-IF
           DISPLAY "Convertendo CWCONFIG para CWCONF..." AT 1010
           SET CWSQLC-CREATE TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO

           PERFORM TEST AFTER UNTIL FS-CONFIG > "09"
                   MOVE SPACES TO CONFIG-REG99
                   READ CONFIG NEXT RECORD IGNORE LOCK
                   IF   FS-CONFIG < "10"
                   ADD 1 TO LIDOS DISPLAY LIDOS AT 1045
                   EVALUATE CONFIG-TIPO
                       WHEN "00" PERFORM SEQ-00 THRU SEQ-00-99-FIM
                       WHEN "02" SET CWSQLC-WRITE TO TRUE
                                 CALL "CWCONF" USING CWSQLC
                                                     CONFIG-REG02
                                                     FS-CWCONF
                                                     KCO PCO
                       WHEN "03" SET CWSQLC-WRITE TO TRUE
                                 CALL "CWCONF" USING CWSQLC
                                                     CONFIG-REG02
                                                     FS-CWCONF
                                                     KCO PCO
                       WHEN "92" SET CWSQLC-WRITE TO TRUE
                                 CALL "CWCONF" USING CWSQLC
                                                     CONFIG-REG92
                                                     FS-CWCONF
                                                     KCO PCO
                       WHEN "94"
                            MOVE "94"               TO CWCONF-REG94
                            MOVE CONFIG-RELATORIO   TO CWCONF-RELATORIO
                            MOVE CONFIG-PROGRAMA-PRINTER
                              TO CWCONF-PROGRAMA-PRINTER
                            MOVE CONFIG-DESPROGRAMA
                              TO CWCONF-DESPROGRAMA
                            MOVE CONFIG-TIPO-FORM   TO CWCONF-TIPO-FORM
                            MOVE CONFIG-SIZE-PAGE   TO CWCONF-SIZE-PAGE
                            MOVE CONFIG-SAIDA       TO CWCONF-SAIDA
                            MOVE CONFIG-TITLE       TO CWCONF-TITLE
                            MOVE CONFIG-SUB-TITLE   TO CWCONF-SUB-TITLE
                            MOVE CONFIG-CAMPOS-AP   TO CWCONF-CAMPOS-AP
                            MOVE CONFIG-CAMPOS-TB   TO CWCONF-CAMPOS-TB
                            SET CWSQLC-WRITE TO TRUE
                            CALL "CWCONF" USING CWSQLC
                                                CWCONF-REG
                                                FS-CWCONF
                                                KCO PCO
                       WHEN "99" MOVE "99" TO CWCONF-REG99
                                 MOVE CONFIG-PAGINA TO CWCONF-PAGINA
                                 PERFORM SEQ-99 THRU SEQ-99-99-FIM
                                  VARYING Y FROM 1 BY 1 UNTIL Y > 26
                                 SET CWSQLC-WRITE TO TRUE
                                 CALL "CWCONF" USING CWSQLC
                                                     CWCONF-REG
                                                     FS-CWCONF
                                                     KCO PCO
                       WHEN "SM" MOVE "SM" TO CWCONF-REG99
                                 MOVE CONFIG-PAGINA TO CWCONF-PAGINA
                                 PERFORM SEQ-99 THRU SEQ-99-99-FIM
                                   VARYING Y FROM 1 BY 1 UNTIL Y > 26
                                 SET CWSQLC-WRITE TO TRUE
                                 CALL "CWCONF" USING CWSQLC
                                                     CWCONF-REG
                                                     FS-CWCONF
                                                     KCO PCO
                       WHEN "AT" MOVE "AT" TO CWCONF-TIPO
                                 INITIALIZE CWCONF-ATRIBUTOS
                                 PERFORM VARYING Y FROM 1 BY 1
                                    UNTIL Y > 11
                                         PERFORM
                                          UNTIL CONFIG-COR-TEXTO (Y) < 8
                                                SUBTRACT 8 FROM
                                                CONFIG-COR-TEXTO (Y)
                                         END-PERFORM
                                         PERFORM
                                          UNTIL CONFIG-COR-FUNDO (Y) < 8
                                                SUBTRACT 8 FROM
                                                CONFIG-COR-FUNDO (Y)
                                         END-PERFORM
                                         MOVE CONFIG-COR-TEXTO (Y)
                                           TO CWCONF-COR-TEXTO (Y)
                                         MOVE CONFIG-COR-FUNDO (Y)
                                           TO CWCONF-COR-FUNDO (Y)
                                        IF Y = 2
                                         MOVE CONFIG-COR-TEXTO (Y)
                                           TO CWCONF-COR-TEXTO (12)
                                              CWCONF-COR-TEXTO (14)
                                              CWCONF-COR-TEXTO (15)
                                              CWCONF-COR-TEXTO (16)
                                         MOVE CONFIG-COR-FUNDO (Y)
                                           TO CWCONF-COR-FUNDO (12)
                                              CWCONF-COR-FUNDO (14)
                                              CWCONF-COR-FUNDO (15)
                                              CWCONF-COR-FUNDO (16)
                                        END-IF
                                        IF Y = 5
                                         MOVE CONFIG-COR-TEXTO (Y)
                                           TO CWCONF-COR-TEXTO (13)
                                              CWCONF-COR-TEXTO (17)
                                              CWCONF-COR-TEXTO (18)
                                         MOVE CONFIG-COR-FUNDO (Y)
                                           TO CWCONF-COR-FUNDO (13)
                                              CWCONF-COR-FUNDO (17)
                                              CWCONF-COR-FUNDO (18)
                                        END-IF
                                        IF Y = 10
                                         MOVE CONFIG-COR-TEXTO (Y)
                                           TO CWCONF-COR-TEXTO (19)
                                              CWCONF-COR-TEXTO (20)
                                         MOVE CONFIG-COR-FUNDO (Y)
                                           TO CWCONF-COR-FUNDO (19)
                                              CWCONF-COR-FUNDO (20)
                                        END-IF
                                 END-PERFORM
                                 IF   CONFIG-ELEMENTO = SPACES
                                 OR   "LOGON"
      *                          OR   LOW-VALUES
                                      MOVE SPACES TO CONFIG-ELEMENTO
                                 END-IF
                                 MOVE CONFIG-ELEMENTO TO CWCONF-ELEMENTO
                                 IF   CWCONF-ELEMENTO = SPACES
                                      MOVE 1 TO AT-ON
                                 END-IF
                                 SET CWSQLC-WRITE TO TRUE
                                 CALL "CWCONF" USING CWSQLC
                                                     CWCONF-REG
                                                     FS-CWCONF
                                                     KCO PCO
                       WHEN "JB" MOVE "JB" TO CWCONF-REGJB
                                 MOVE CONFIG-JOB
                                   TO CWCONF-JOB
                                 MOVE CONFIG-JOB-MODULO
                                   TO CWCONF-JOB-MODULO
                                 MOVE CONFIG-JOB-TIPO
                                   TO CWCONF-JOB-TIPO
                                 MOVE CONFIG-JOB-PARAMETRO
                                   TO CWCONF-JOB-PARAMETRO
                                 MOVE CONFIG-JOB-PROXIMO-RC-OK
                                   TO CWCONF-JOB-PROXIMO-RC-OK
                                 MOVE CONFIG-JOB-PROXIMO-NAO-OK
                                   TO CWCONF-JOB-PROXIMO-NAO-OK
                                 MOVE CONFIG-JOB-MENSAGEM
                                   TO CWCONF-JOB-MENSAGEM
                                 SET CWSQLC-WRITE TO TRUE
                                 CALL "CWCONF" USING CWSQLC
                                                     CWCONF-REG
                                                     FS-CWCONF
                                                     KCO PCO
                       WHEN "LG" MOVE "LG"       TO CWCONF-REGLG
                                 INITIALIZE CWCONF-OPCOES-LOG
                                 MOVE CONFIG-LOG TO CWCONF-LOG
                                 MOVE 1           TO LOG
                       WHEN "PS" PERFORM SEQ-PS THRU SEQ-PS-99-FIM
                       WHEN "GU" SET CWSQLC-WRITE TO TRUE
                                 CALL "CWCONF" USING CWSQLC
                                                     CWCONF-REG
                                                     FS-CWCONF
                                                     KCO PCO
                       WHEN "RT" MOVE "RT" TO CWCONF-REGRT
                                 MOVE 0 TO Y
                                 PERFORM VARYING I FROM 1 BY 1
                                           UNTIL I > 84
                                         IF   CONFIG-ROTINA (I) (1: 2)
                                              NOT = "GR"
                                              ADD 1 TO Y
                                              MOVE CONFIG-ROTINA (I)
                                                TO CWCONF-ROTINA (Y)
                                         END-IF
                                 END-PERFORM
                                 PERFORM VARYING I FROM 1 BY 1
                                           UNTIL I > 7
                                         IF   Y < 84
                                              ADD 1 TO Y
                                              MOVE OBSOLETA (I)
                                                TO CWCONF-ROTINA (Y)
                                         END-IF
                                 END-PERFORM
                                 MOVE Y TO CWCONF-TOT-ROTINAS
                                 SET CWSQLC-WRITE TO TRUE
                                 CALL "CWCONF" USING CWSQLC
                                                     CWCONF-REG
                                                     FS-CWCONF
                                                     KCO PCO
                       WHEN "RV" MOVE "RV" TO CWCONF-REGRT
                                 MOVE 4    TO CWCONF-VERSAO
                                 MOVE 0    TO CWCONF-REVISAO
                       WHEN "VD" SET CWSQLC-WRITE TO TRUE
                                 CALL "CWCONF" USING CWSQLC
                                                     CWCONF-REG
                                                     FS-CWCONF
                                                     KCO PCO
                       WHEN "ES" SET CWSQLC-WRITE TO TRUE
                                 CALL "CWCONF" USING CWSQLC
                                                     CWCONF-REG
                                                     FS-CWCONF
                                                     KCO PCO
                   END-EVALUATE
                   END-IF
           END-PERFORM
           IF   AT-ON = 0
                MOVE "AT"     TO CWCONF-REGAT
                MOVE ALL "71" TO CWCONF-ATRIBUTOS
                SET CWSQLC-WRITE TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           END-IF
           IF   LOG = 0
                MOVE    "LG"       TO CWCONF-REGLG
                INITIALIZE CWCONF-OPCOES-LOG
                MOVE    1          TO CWCONF-LOG
                PERFORM GRAVA-LG THRU FIM-GRAVA-LG
           END-IF
           CLOSE CONFIG
           SET CWSQLC-CLOSE TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO.

       000-99-FIM. GOBACK.

       SEQ-00.

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 60
               SUBTRACT CONFIG-FATOR-00 FROM CONFIG-BYTE-U (I)
               SUBTRACT I               FROM CONFIG-BYTE-U (I)
           END-PERFORM
           MOVE "00"                  TO CWCONF-REG00
           MOVE CONFIG-USUARIO        TO CWCONF-USUARIO
                                         CWCONF-USUARIO-P
           MOVE CONFIG-SISTEMA        TO CWCONF-SISTEMA
                                         CWCONF-SISTEMA-P
           MOVE CONFIG-QUADRO         TO CWCONF-QUADRO QUADRO
           MOVE CONFIG-EJECT-MODE-OLD TO CWCONF-EJECT-MODE-OLD
           MOVE LENGTH OF CWCONF-USUARIO TO CWCONF-SIZE-U CWCONF-SIZE-UP
                                            CWCONF-SIZE-S CWCONF-SIZE-SP
           CALL "CWCODE" USING "C" CWCONF-SIZE-U
                                   CWCONF-FATOR-00-U
                                   CWCONF-USUARIO
           CALL "CWCODE" USING "C" CWCONF-SIZE-S
                                   CWCONF-FATOR-00-S
                                   CWCONF-SISTEMA
           CALL "CWCODE" USING "C" CWCONF-SIZE-UP
                                   CWCONF-FATOR-00-UP
                                   CWCONF-USUARIO-P
           CALL "CWCODE" USING "C" CWCONF-SIZE-SP
                                   CWCONF-FATOR-00-SP
                                   CWCONF-SISTEMA-P
           SET CWSQLC-WRITE TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO.

       SEQ-00-99-FIM. EXIT.

       SEQ-99.

           IF   CONFIG-NM-OPCAO (Y) = SPACES
                GO TO SEQ-99-99-FIM
           END-IF
           MOVE CONFIG-PROG (Y) TO PROGRAMA
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 8
                                            OR PROGRAMA = SPACES
                   SUBTRACT CONFIG-FATOR (Y) FROM PK-X (I)
                   SUBTRACT I                FROM PK-X (I)
           END-PERFORM
           MOVE CONFIG-PASS (Y) TO CHECK-PASS-3
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 6
                                            OR CHECK-PASS-3 = SPACES
                   SUBTRACT CONFIG-FATOR (Y) FROM SENHA-X3 (I)
                   SUBTRACT I                FROM SENHA-X3 (I)
           END-PERFORM
           MOVE CHECK-PASS-3 TO TESTE-LIVRE
           INSPECT PROGRAMA CONVERTING MINUSCULAS TO MAIUSCULAS
           INSPECT TESTE-LIVRE CONVERTING MINUSCULAS TO MAIUSCULAS
           IF   TESTE-LIVRE = "LIVRE"
                MOVE SPACES TO CHECK-PASS-3
           END-IF
           IF   PROGRAMA (1: 7) = "RELATOR"
                MOVE PROGRAMA (8: 1) TO X
                MOVE "CWREL"         TO PROGRAMA
                MOVE X               TO PROGRAMA (6: )
           END-IF
           EVALUATE PROGRAMA
               WHEN "GRBOXS" MOVE "CWBOXS" TO PROGRAMA
               WHEN "GRMEN9" MOVE "CWMEN9" TO PROGRAMA
               WHEN "GRMENU" MOVE "CWMENU" TO PROGRAMA
               WHEN "GRCONF" GO TO SEQ-99-99-FIM
               WHEN "GRLOGS" MOVE "CWLOGS" TO PROGRAMA
               WHEN "GRPAGE" MOVE "CWPAGE" TO PROGRAMA
           END-EVALUATE
           IF   CONFIG-NIVEL (Y) NOT NUMERIC
                MOVE 0 TO CONFIG-NIVEL (Y)
           END-IF
           MOVE CONFIG-NIVEL    (Y) TO CWCONF-NIVEL     (Y)
           MOVE CONFIG-CHECK    (Y) TO CWCONF-CHECK     (Y)
           MOVE CONFIG-HELP     (Y) TO CWCONF-HELP      (Y)
           MOVE CONFIG-NO-OPCAO (Y) TO CWCONF-NO-OPCAO  (Y)
           MOVE CONFIG-NM-OPCAO (Y) TO CWCONF-NM-OPCAO  (Y)
           MOVE 8 TO CWCONF-SIZE-P-99 (Y)
           CALL "CWCODE" USING "C" CWCONF-SIZE-P-99  (Y)
                                   CWCONF-FATOR-P-99 (Y)
                                   CWCONF-PROG (Y)
           MOVE 6 TO CWCONF-SIZE-S-99 (Y)
           CALL "CWCODE" USING "C" CWCONF-SIZE-S-99  (Y)
                                   CWCONF-FATOR-S-99 (Y)
                                   CHECK-PASS-3
           MOVE PROGRAMA            TO CWCONF-PROG (Y)
           MOVE CHECK-PASS-3        TO CWCONF-PASS (Y).

       SEQ-99-99-FIM. EXIT.

       SEQ-PS.

           PERFORM VARYING I FROM 1 BY 1
                    UNTIL I > 6
                    SUBTRACT CONFIG-FATOR-PS FROM CONFIG-S (I)
                    SUBTRACT I               FROM CONFIG-S (I)
           END-PERFORM

           MOVE "PS"                   TO CWCONF-REGPS
           MOVE CONFIG-NOME            TO CWCONF-NOME
           MOVE CONFIG-SENHA           TO CWCONF-SENHA
           MOVE CONFIG-NIVEL-PS        TO CWCONF-NIVEL-PS
           MOVE CONFIG-GRUPO           TO CWCONF-GRUPO
           MOVE CONFIG-MODO-MENU       TO CWCONF-MODO-MENU
           MOVE CONFIG-PATH-SPOOL      TO CWCONF-PATH-SPOOL
           MOVE CONFIG-PRINTER-DEFAULT TO CWCONF-PRINTER-DEFAULT
           IF   CONFIG-QUADRO-PS NOT NUMERIC
                MOVE QUADRO           TO CWCONF-QUADRO-PS
           ELSE
                MOVE CWCONF-QUADRO-PS TO CWCONF-QUADRO-PS
           END-IF
           MOVE LENGTH OF CWCONF-SENHA TO CWCONF-SIZE-PS
           CALL "CWCODE" USING "C" CWCONF-SIZE-PS
                                   CWCONF-FATOR-PS
                                   CWCONF-SENHA
           MOVE 0 TO CWCONF-ZEBRA
                     CWCONF-FULL
                     CWCONF-COLUNA-SORT
                     CWCONF-OPCAO-SORT
                     CWCONF-RELATOR-IN
                     CWCONF-RELATOR-OUT
                     CWCONF-ESTILO-FLAG

           SET CWSQLC-WRITE TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO.

       SEQ-PS-99-FIM. EXIT.

       GRAVA-LG.

           MOVE 0          TO CWCONF-AUTOPASS
           MOVE 1          TO CWCONF-CLOCK
           MOVE 1          TO CWCONF-MOUSE
           MOVE 1          TO CWCONF-DIR
           MOVE 60         TO CWCONF-TIMEOUT
           MOVE ALL "1"    TO CWCONF-FILLER (1: )
           MOVE "."        TO CWCONF-LOGDIR
           MOVE 1          TO CWCONF-PRTPOSIT
           SET CWSQLC-WRITE TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO.

       FIM-GRAVA-LG. EXIT.
       END PROGRAM CW3050.

