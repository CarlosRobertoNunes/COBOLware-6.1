      $Set CallFH"FHREDIR"
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWLOGS.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  07/01/1990.
       SECURITY.      *************************************************
                      *                                               *
                      *   Relaciona LOG de execucoes                  *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT OPTIONAL CWLOGF ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  RECORD KEY    IS CWLOGF-SEQUENCIA
                  ACCESS MODE   IS SEQUENTIAL
                  FILE STATUS   IS FS-CWLOGF
                  LOCK MODE     IS MANUAL.

       DATA DIVISION.
       FILE SECTION.

       COPY CWLOGF.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1.
           05 TEMP                    PIC  X(050) VALUE SPACES.
           05 FATOR-W          COMP-X PIC  9(002) VALUE 0.
           05 MINUSCULAS PIC X(26) VALUE "abcdefghijklmnopqrstuvwxyz".
           05 MAIUSCULAS PIC X(26) VALUE "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
           05 ANO                      PIC  9(002) VALUE ZERO.
           05 DETALHE                              VALUE SPACES.
              10 DET-X PIC 9(02) COMP-X OCCURS 80.
           05 FILLER REDEFINES DETALHE.
              10 DET-A PIC X(01) OCCURS 80.
           05 DATA-COM-BARRAS.
              10 DD-B                  PIC  9(002).
              10 FILLER                PIC  X(001).
              10 MM-B                  PIC  9(002).
              10 FILLER                PIC  X(001).
              10 AA-B                  PIC  9(004).
           05 DATA-TESTE.
              10 AA-B                  PIC  9(004).
              10 MM-B                  PIC  9(002).
              10 DD-B                  PIC  9(002).
           05 HOJE                     PIC  9(008) VALUE ZERO.
           05 FILLER REDEFINES HOJE.
              10 AAX                   PIC  9(004).
              10 MMX                   PIC  9(002).
              10 DD                    PIC  9(002).
           05 INICIO                   PIC  9(008) VALUE ZERO.
           05 U                        PIC  9(002) VALUE ZERO.
           05 I                        PIC  9(003) VALUE ZERO.
           05 L                        PIC  9(003) VALUE ZERO.
           05 FIM                      PIC  9(008) VALUE ZERO.
           05 AAMM-INICIAL.
              10 AA-INICIAL            PIC  9(004).
              10 MM-INICIAL            PIC  9(002).
           05 AAMM-FIM.
              10 AA-FIM                PIC  9(004).
              10 MM-FIM                PIC  9(002).
           05 LD-CWLOGF                PIC  9(006) VALUE ZEROS.
           05 GR-PRNTER                PIC  9(006) VALUE ZEROS.
           05 ER-CWLOGF.
              10 FS-CWLOGF             PIC  X(002) VALUE "00".
              10 LB-CWLOGF             PIC  X(255) VALUE SPACES.
           05 N-CWLOGF.
              10 FILLER                PIC  X(005) VALUE "cwlog".
              10 FILLER                PIC  X(001) VALUE "-".
              10 ANO-CWLOGF            PIC  9(004) VALUE ZERO.
              10 FILLER                PIC  X(001) VALUE "-".
              10 MES-CWLOGF            PIC  9(002) VALUE ZERO.
              10 LETRA                 PIC  X(002) VALUE SPACE.
              10 FILLER                PIC  X(035) VALUE SPACES.
           05 MES-ED.
              10                       PIC X(006) VALUE "Lidos".
              10 ED-MES                PIC 9(002)/ VALUE 0 BLANK ZERO.
              10 ED-ANO                PIC 9(004)  VALUE 0 BLANK ZERO.
           05 ACENTOS     PIC X(36) VALUE
              " ‚¡¢£…Š•—„”ƒˆ“‡†‹Ÿ–‘’˜©Ž™š‰Œ€äÆ".
           05 SEM-ACENTOS PIC X(36) VALUE
              "AEIOUAEIOUAOUAEOCAEIOUAEIOUAOUAEOCOA".

       01  AREAS-DE-TRABALHO-2.
           05 MENSAGENS-DE-ERRO.
              10 F PIC X(30) VALUE "Data inicial inv lida         ".
              10 F PIC X(30) VALUE "Data final inv lida           ".
              10 F PIC X(30) VALUE "Data inicial > data final     ".
              10 F PIC X(30) VALUE "                              ".
           05 FILLER REDEFINES MENSAGENS-DE-ERRO.
              10 MSG OCCURS 4 PIC X(30).
           05 USERTXT PIC X(08) VALUE "Usu rio".

       COPY CWIMPR.
       COPY CWACOR.
       COPY CWTIME.
       COPY CWSEND.
       COPY CWGETL.
       COPY CWBOXC.
       COPY CWCONF.

       01  LINHAS-DE-IMPRESSAO-CLIC.
       02  LINHA-01.
           05 FILLER                         PIC  X(004) VALUE "LOG ".
           05 USUARIO                        PIC  X(030) VALUE SPACES.
       02  LINHA-02.
           05 FILLER                         PIC  X(003) VALUE
              "DE ".
           05 CLIC-INICIO                    PIC  99/99/9999.
           05 FILLER                         PIC  X(003) VALUE " A ".
           05 CLIC-FIM                       PIC  99/99/9999B.
       02  LINHA-03.
           05 FILLER                         PIC  X(049) VALUE
              "DATA       HORA     MODULO   OBJETIVO/OCORRENCIA ".
           05 FILLER                         PIC  X(022) VALUE
              "               USUARIO".

       SCREEN SECTION.

       01  CWLOGSA.
           04 FOREGROUND-COLOR CWACOR-F (09)
              BACKGROUND-COLOR CWACOR-B (09).
           05 NOVO-MES.
              10 LINE 12 COLUMN 03 PIC X(20) FROM MES-ED.
           05 T-LD-CWLOGF LINE 12 COLUMN 25 PIC ZZZ.ZZ9 FROM LD-CWLOGF.
           05 NADAY       LINE 14 COLUMN 03 VALUE "Impressos".
           05 T-GR-PRNTER LINE 14 COLUMN 25 PIC ZZZ.ZZ9 FROM GR-PRNTER.

       01  CWLOGSB.
           04 FOREGROUND-COLOR CWACOR-F (09)
              BACKGROUND-COLOR CWACOR-B (09).
           05 LINE 08 COLUMN 03 VALUE
             "Informe per¡odo de referˆncia de".
           05 LINE 08 COLUMN 47 VALUE "a".
           05 LINE 08 COLUMN 36 PIC 99/99/9999 USING INICIO.
           05 LINE 08 COLUMN 49 PIC 99/99/9999 USING FIM.
           05 LINE 10 COLUMN 03 PIC X(009) FROM USERTXT.
           05 USR LINE 10 COLUMN 13 PIC X(030) FROM USUARIO.

       PROCEDURE DIVISION.

       000-INICIO.

           PERFORM 800-INICIAIS      THRU 800-99-FIM
           PERFORM 100-PROCESSAMENTO THRU 100-99-FIM
           PERFORM 900-FINAIS        THRU 900-99-FIM.

       000-99-FIM.

           EXIT PROGRAM.

       100-PROCESSAMENTO.

           PERFORM UNTIL AAMM-INICIAL GREATER AAMM-FIM
              PERFORM VARYING L FROM 0 BY 1
                UNTIL L > LENGTH OF MAIUSCULAS
                   IF L = 0
                      MOVE SPACE TO LETRA
                   ELSE
                      MOVE "-"               TO LETRA(1:1)
                      MOVE MAIUSCULAS (L: 1) TO LETRA(2:1)
                   END-IF
                   MOVE AA-INICIAL     TO ANO-CWLOGF
                   MOVE MM-INICIAL     TO MES-CWLOGF
                   IF   CWGETL-LOGDIR = "."
                        MOVE N-CWLOGF TO LB-CWLOGF
                   ELSE
                        MOVE SPACES TO LB-CWLOGF
                        PERFORM VARYING I FROM LENGTH OF CWGETL-LOGDIR
                                            BY -1
                                  UNTIL I = 1
                                    OR (CWGETL-LOGDIR (I: 1) NOT = SPACE
                                   AND (CWGETL-LOGDIR (I: 1) NOT = "/")
                                   AND (CWGETL-LOGDIR (I: 1) NOT = "\"))
                               CONTINUE
                        END-PERFORM
                        CALL "CBL_CREATE_DIR" USING CWGETL-LOGDIR (1: I)
                        STRING CWGETL-LOGDIR (1: I) "/"
                                             DELIMITED BY SIZE
                               N-CWLOGF      DELIMITED BY SPACE
                             INTO LB-CWLOGF
                   END-IF
                   OPEN INPUT CWLOGF
                   IF   FS-CWLOGF EQUAL "00"
                        MOVE MES-CWLOGF TO ED-MES
                        MOVE ANO-CWLOGF TO ED-ANO
                        DISPLAY NOVO-MES
                   ELSE
                        IF  FS-CWLOGF = "9)"
                            CALL "CWISAM" USING ER-CWLOGF
                            STOP RUN
                        END-IF
                   END-IF
                   PERFORM 110-LER-CWLOGF THRU 110-99-FIM
                           UNTIL FS-CWLOGF NOT EQUAL "00"
                   CLOSE CWLOGF
              END-PERFORM
              MOVE ZERO TO LD-CWLOGF
              ADD 1 TO MM-INICIAL
              IF   MM-INICIAL EQUAL 13
                   MOVE 1 TO MM-INICIAL
                   ADD  1 TO AA-INICIAL
              END-IF
           END-PERFORM.

       100-99-FIM. EXIT.

       110-LER-CWLOGF.

           CALL "CWATCH"
           PERFORM TEST AFTER
                  UNTIL FS-CWLOGF > "09"
                     OR USUARIO = "<Geral>"
                     OR USUARIO = CWLOGF-OPERADOR
                   READ CWLOGF IGNORE LOCK
                   IF   FS-CWLOGF EQUAL "00"
                   AND (CWLOGF-FATOR NOT = SPACES)
                   AND  CWLOGF-FATOR NUMERIC
                        MOVE CWLOGF-FATOR TO FATOR-W
                        CALL "CWCODE" USING "D" X"64"
                                                FATOR-W
                                                CWLOGF-REG
                   END-IF
           END-PERFORM
           IF   FS-CWLOGF EQUAL "00"
                ADD 1 TO LD-CWLOGF
                DISPLAY T-LD-CWLOGF
                MOVE CWLOGF-DATA-DE-HOJE  TO DATA-COM-BARRAS
                MOVE CORR DATA-COM-BARRAS TO DATA-TESTE
                IF   DATA-TESTE GREATER FIM
                     GO TO 110-LER-CWLOGF
      *              MOVE "10" TO FS-CWLOGF
                ELSE
                     IF   DATA-TESTE NOT LESS INICIO
                          MOVE CWLOGF-REG TO DETALHE
                          PERFORM VARYING U FROM 17 BY 1
                                    UNTIL U GREATER 80
                                  INSPECT DET-A (U)
                                          CONVERTING MINUSCULAS
                                                  TO MAIUSCULAS
                                  INSPECT DET-A (U)
                                          CONVERTING ACENTOS
                                                  TO SEM-ACENTOS
                          END-PERFORM
                          ADD  1 TO GR-PRNTER
                          DISPLAY T-GR-PRNTER
                          MOVE DETALHE TO CWIMPR-DETAIL
                          CALL "CWIMPR" USING PARAMETROS-CWIMPR
                          IF   CWIMPR-END-PRINT
                               CLOSE CWLOGF
                               EXIT PROGRAM
                          END-IF
                     END-IF
                END-IF
           END-IF.

       110-99-FIM. EXIT.

       800-INICIAIS.

           CALL "CWTEXT" USING AREAS-DE-TRABALHO-2
                     LENGTH OF AREAS-DE-TRABALHO-2
           CALL "CWACOR"       USING PARAMETROS-CWACOR
           CALL "CWGETL"       USING PARAMETROS-CWGETL
           MOVE "2"               TO CWIMPR-FORM-TYPE
           MOVE  "CWLOGSA"        TO CWIMPR-REPORT
           SET CWTIME-REVERSED    TO TRUE
           SET CWTIME-TODAY       TO TRUE
           CALL "CWTIME"       USING PARAMETROS-CWTIME
           MOVE CWTIME-DATE-FINAL TO HOJE
           MOVE   HOJE            TO FIM
           MOVE   01              TO DD
           MOVE   HOJE            TO INICIO

           MOVE INICIO            TO CWTIME-DATE
           SET CWTIME-REVERSED    TO TRUE
           SET CWTIME-REVERSE     TO TRUE
           CALL "CWTIME"       USING PARAMETROS-CWTIME
           MOVE CWTIME-DATE-FINAL TO INICIO

           MOVE FIM               TO CWTIME-DATE
           SET CWTIME-REVERSED    TO TRUE
           SET CWTIME-REVERSE     TO TRUE
           CALL "CWTIME"       USING PARAMETROS-CWTIME
           MOVE CWTIME-DATE-FINAL TO FIM

           DISPLAY CWLOGSB

           PERFORM TEST AFTER
                   UNTIL INICIO NOT EQUAL ZERO
                     AND FIM    NOT EQUAL ZERO
                   ACCEPT CWLOGSB
                          ON ESCAPE EXIT PROGRAM
                   END-ACCEPT
                   MOVE INICIO            TO CWTIME-DATE
                   SET CWTIME-NORMAL      TO TRUE
                   SET CWTIME-VALIDATE    TO TRUE
                   CALL "CWTIME"       USING PARAMETROS-CWTIME
                   MOVE CWTIME-DATE-FINAL TO INICIO

                   MOVE FIM               TO CWTIME-DATE
                   SET CWTIME-NORMAL      TO TRUE
                   SET CWTIME-VALIDATE    TO TRUE
                   CALL "CWTIME"       USING PARAMETROS-CWTIME
                   MOVE CWTIME-DATE-FINAL TO FIM

                   IF   INICIO EQUAL ZERO
                        MOVE MSG (1) TO CWSEND-MSG
                        CALL "CWSEND" USING PARAMETROS-CWSEND
                   ELSE
                        IF   FIM EQUAL ZERO
                             MOVE MSG (2) TO CWSEND-MSG
                             CALL "CWSEND" USING PARAMETROS-CWSEND
                        ELSE
                             MOVE    INICIO      TO CLIC-INICIO
                             MOVE    FIM         TO CLIC-FIM

                             MOVE INICIO            TO CWTIME-DATE
                             SET CWTIME-NORMAL      TO TRUE
                             SET CWTIME-REVERSE     TO TRUE
                             CALL "CWTIME"       USING PARAMETROS-CWTIME
                             MOVE CWTIME-DATE-FINAL TO INICIO

                             MOVE FIM               TO CWTIME-DATE
                             SET CWTIME-NORMAL      TO TRUE
                             SET CWTIME-REVERSE     TO TRUE
                             CALL "CWTIME"       USING PARAMETROS-CWTIME
                             MOVE CWTIME-DATE-FINAL TO FIM

                             IF   INICIO GREATER FIM
                                  MOVE MSG (3) TO CWSEND-MSG
                                  CALL "CWSEND" USING PARAMETROS-CWSEND
                                  MOVE ZERO TO INICIO
                                               FIM
                             ELSE
                                  MOVE INICIO TO AAMM-INICIAL
                                  MOVE FIM    TO AAMM-FIM
                                  MOVE MSG (4) TO CWSEND-MSG
                                  CALL "CWSEND" USING PARAMETROS-CWSEND
                             END-IF
                        END-IF
                   END-IF
           END-PERFORM

           PERFORM 010-GET-USER THRU 010-99-FIM
           MOVE LINHA-01          TO CWIMPR-TITLE
                                     CWIMPR-NOTE
           MOVE LINHA-02          TO CWIMPR-SUB-TITLE
           MOVE LINHA-03          TO CWIMPR-HEADER-1
           MOVE ZERO              TO HOJE

           MOVE SPACES TO CWIMPR-TIME-REPORT

           DISPLAY CWLOGSA.

       800-99-FIM. EXIT.

       900-FINAIS.

           MOVE "CLOSE"     TO CWIMPR-TIME-REPORT
           CALL "CWIMPR" USING PARAMETROS-CWIMPR.

       900-99-FIM. EXIT.

       010-GET-USER.

           SET CWSQLC-OPEN TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           MOVE "PS" TO CWCONF-REG
           SET CWSQLC-START TO TRUE
           SET CWSQLC-NOT-LESS TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           SET CWBOXC-LOAD TO TRUE
           MOVE 10  TO CWBOXC-LINE
           MOVE 12  TO CWBOXC-COLUMN
           MOVE 10  TO CWBOXC-VERTICAL-LENGTH
           MOVE 30  TO CWBOXC-HORIZONTAL-LENGTH
           MOVE  1  TO CWBOXC-ORDER
           MOVE 22  TO CWBOXC-STRING-1-LENGTH
           MOVE 0   TO CWBOXC-STRING-2-LENGTH
           MOVE 1   TO CWBOXC-RETURN
           MOVE "<Geral>"    TO CWBOXC-STRING-1
           CALL "CWBOXC"  USING PARAMETROS-CWBOXC

           MOVE "Usu rios" TO CWBOXC-TITLE
           PERFORM TEST AFTER UNTIL FS-CWCONF > "09"
                           OR CWCONF-TIPO NOT = "PS"
              SET CWSQLC-READ        TO TRUE
              SET CWSQLC-NEXT        TO TRUE
              SET CWSQLC-IGNORE-LOCK TO TRUE
              CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
              IF   FS-CWCONF < "10"
              AND  CWCONF-TIPO = "PS"
                   MOVE CWCONF-NOME       TO CWBOXC-STRING-1
                   CALL "CWBOXC"       USING PARAMETROS-CWBOXC
                   IF   CWBOXC-VERTICAL-LENGTH < 10
                        ADD 1 TO CWBOXC-VERTICAL-LENGTH
                   END-IF
              END-IF
           END-PERFORM

           SET CWBOXC-SHOW        TO TRUE
           CALL "CWBOXC"       USING PARAMETROS-CWBOXC
           MOVE CWBOXC-OPTION     TO USUARIO
           MOVE "D"               TO CWBOXC-FUNCTION
           SET CWBOXC-DELETE      TO TRUE
           CALL "CWBOXC"       USING PARAMETROS-CWBOXC
           SET CWSQLC-CLOSE       TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           IF  USUARIO = SPACES
               EXIT PROGRAM
           END-IF
           DISPLAY USR.

       010-99-FIM. EXIT.

       END PROGRAM CWLOGS.
