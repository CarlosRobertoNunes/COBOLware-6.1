       IDENTIFICATION DIVISION.
       PROGRAM-ID.    WINDOW.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  30/10/1996.
       SECURITY.      *************************************************
                      *                                               *
                      * Emulacao de rotinas do Micro Base COBOL em    *
                      * Micro Focus COBOL                             *
                      *                                               *
                      *************************************************
                      *                                               *
                      * Controle de janelas                           *
                      *                                               *
                      *************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      $Set IdxFormat"14" DataCompress"1" KeyCompress"7"
           SELECT WINDWRK ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  RECORD KEY    IS WINDWRK-CHAVE
                  ACCESS MODE   IS DYNAMIC
                  LOCK MODE     IS MANUAL
                  FILE STATUS   IS FS-WINDWRK.

       DATA DIVISION.
       FILE SECTION.

       FD  WINDWRK
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "WINDWRK.DAT".

       01  WINDWRK-REG.
           05 WINDWRK-CHAVE.
              10 WINDWRK-NUM            PIC  9(0006).
              10 WINDWRK-IDENT          PIC  9(0003).
           05 WINDWRK-CARACTER-BUFFER   PIC  X(2000).
           05 WINDWRK-ATTRIBUTE-BUFFER  PIC  X(2000).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1.
           05 ATT-X.
              10 ATT-N                  PIC  9(002) VALUE 7 COMP-X.
           05 FORE-COLOR                PIC  9(002) VALUE 0 COMP-X.
           05 BACK-COLOR                PIC  9(002) VALUE 0 COMP-X.
           05 WLIN                      PIC  9(002) VALUE 0 COMP.
           05 WLIN2                     PIC  9(002) VALUE 0 COMP.
           05 WLIN3                     PIC  9(002) VALUE 0 COMP.
           05 WFIM                      PIC  9(002) VALUE 0 COMP.
           05 WCOL                      PIC  9(002) VALUE 0 COMP.
           05 LINHA-INFERIOR            PIC  9(004) VALUE 0 COMP.
           05 WHOR                      PIC  9(004) VALUE 0 COMP.
           05 WHOR-T                    PIC  9(004) VALUE 0 COMP.
           05 FS-WINDWRK                PIC  X(002) VALUE "00".
           05 NUM                       PIC  9(006) VALUE 0.
           05 IDENT                     PIC  9(003) VALUE 0.
           05 VEZ                       PIC  9(001) VALUE 0.
           05 VEZES                     PIC  9(002) VALUE 0.
           05 PILHA                     PIC  9(003) VALUE 0.
           05 LIXO                      PIC  X(001) VALUE SPACE.
           05 CBL-READ-WRITE-SCR-CHARS-ATTR.
              10 SCREEN-POSITION.
                 15 ROW-NUMBER        PIC  9(002) COMP-X VALUE 0.
                 15 COLUMN-NUMBER     PIC  9(002) COMP-X VALUE 0.
              10 CARACTER-BUFFER.
                 15 CARACTER-LIN                  OCCURS 25.
                    20 CARACTER-COL   PIC  X(001) OCCURS 80.
              10 ATTRIBUTE-BUFFER.
                 15 ATTRIBUTE-LIN                 OCCURS 25.
                    20 ATTRIBUTE-COL  PIC  X(001) OCCURS 80.
              10 STRING-LENGTH        PIC  9(004) COMP-X VALUE 2000.
           05 CONVERTE-CORES.
              10 FILLER                         PIC  X(048) VALUE
                 "000016032048064080096112128144160176192208224240".
              10 FILLER                         PIC  X(048) VALUE
                 "001017033049065081097113129145161177193209225241".
              10 FILLER                         PIC  X(048) VALUE
                 "002018034050066082098114130146162178194210226242".
              10 FILLER                         PIC  X(048) VALUE
                 "003019035051067083099115131147163179195211227243".
              10 FILLER                         PIC  X(048) VALUE
                 "004020036052068084100116132148164180196212228244".
              10 FILLER                         PIC  X(048) VALUE
                 "005021037053069085101117133149165181197213229245".
              10 FILLER                         PIC  X(048) VALUE
                 "006022038054070086102118134150166182198214230246".
              10 FILLER                         PIC  X(048) VALUE
                 "007023039055071087103119135151167183199215231247".
              10 FILLER                         PIC  X(048) VALUE
                 "008024040056072088104120136152168184200216232248".
              10 FILLER                         PIC  X(048) VALUE
                 "009025041057073089105121137153169185201217233249".
              10 FILLER                         PIC  X(048) VALUE
                 "010026042058074090106122138154170186202218234250".
              10 FILLER                         PIC  X(048) VALUE
                 "011027043059075091107123139155171187203219235251".
              10 FILLER                         PIC  X(048) VALUE
                 "012028044060076092108124140156172188204220236252".
              10 FILLER                         PIC  X(048) VALUE
                 "013029045061077093109125141157173189205221237253".
              10 FILLER                         PIC  X(048) VALUE
                 "014030046062078094110126142158174190206222238254".
              10 FILLER                         PIC  X(048) VALUE
                 "015031047063079095111127143159175191207223239255".
           05 REDEFINES CONVERTE-CORES.
              10 OCCURS 16.
                 15 B      OCCURS 16       PIC 9(003).

       LINKAGE SECTION.

       01  PARAMETRO-JANELA.
           05 IDENT-JANELA                 PIC 9(02) COMP-X.
           05 CODIGO-FUNCAO                PIC 9(02) COMP-X.
           05 LINHA-SUPERIOR               PIC 9(02) COMP-X.
           05 COLUNA-ESQUERDA              PIC 9(02) COMP-X.
           05 NUMERO-LINHAS                PIC 9(02) COMP-X.
           05 TAMANHO-LINHAS               PIC 9(02) COMP-X.
           05 DADOS-JANELA.
              10 BYTE-JANELA               PIC X(01) OCCURS 2500.
           05 REDEFINES DADOS-JANELA.
              10 BYTE-X             COMP-X PIC 9(02) OCCURS 2500.
           05 REDEFINES DADOS-JANELA.
              10 LINHAS-A-ROLAR            PIC 9(02) COMP-X.
              10 SENTIDO-A-ROLAR           PIC 9(02) COMP-X.
           05 REDEFINES DADOS-JANELA.
              10 CANTO-E-S                 PIC X(01).
              10 CANTO-D-S                 PIC X(01).
              10 CANTO-E-I                 PIC X(01).
              10 CANTO-D-I                 PIC X(01).
              10 LINHA-E-V                 PIC X(01).
              10 LINHA-D-V                 PIC X(01).
              10 LINHA-H-S                 PIC X(01).
              10 LINHA-H-I                 PIC X(01).

       PROCEDURE DIVISION USING PARAMETRO-JANELA.

       000-INICIO.

           CALL "CBL_READ_SCR_CHATTRS" USING SCREEN-POSITION
                                             CARACTER-BUFFER
                                             ATTRIBUTE-BUFFER
                                             STRING-LENGTH

           IF   VEZ = 0
                MOVE 1 TO VEZ
                OPEN INPUT WINDWRK
                IF   FS-WINDWRK = "30" OR "35"
                     OPEN OUTPUT WINDWRK
                     CLOSE WINDWRK
                     OPEN I-O WINDWRK
                     IF   FS-WINDWRK > "09"
                          DISPLAY (1, 1) ERASE
                           "Erro " FS-WINDWRK
                           " na criacao do arquivo WINDWRK.DAT [Enter]"
                           ACCEPT LIXO
                           STOP RUN
                     END-IF
                ELSE
                     IF   FS-WINDWRK > "09"
                          DISPLAY (1, 1) ERASE
                           "Erro " FS-WINDWRK
                           "no acesso ao arquivo WINDWRK.DAT [Enter]"
                           ACCEPT LIXO
                           STOP RUN
                     ELSE
                          CLOSE WINDWRK
                          OPEN I-O WINDWRK
                     END-IF
                END-IF
                MOVE HIGH-VALUES TO WINDWRK-CHAVE
                START WINDWRK KEY NOT > WINDWRK-CHAVE
                READ  WINDWRK PREVIOUS RECORD
                IF   FS-WINDWRK > "09"
                     MOVE 1          TO WINDWRK-NUM NUM
                ELSE
                     ADD  1           TO WINDWRK-NUM
                     MOVE WINDWRK-NUM TO NUM
                END-IF
                MOVE 999 TO WINDWRK-IDENT
                WRITE WINDWRK-REG
           END-IF

           EVALUATE TRUE
               WHEN CODIGO-FUNCAO = 0
                AND IDENT-JANELA  = 0
                    ADD  1                TO PILHA
                    MOVE NUM              TO WINDWRK-NUM
                    MOVE PILHA            TO WINDWRK-IDENT
                                             IDENT-JANELA
                    MOVE CARACTER-BUFFER  TO WINDWRK-CARACTER-BUFFER
                    MOVE ATTRIBUTE-BUFFER TO WINDWRK-ATTRIBUTE-BUFFER
                    WRITE WINDWRK-REG
               WHEN CODIGO-FUNCAO  = 0
                AND IDENT-JANELA  NOT = 0
                    MOVE NUM              TO WINDWRK-NUM
                    MOVE IDENT-JANELA     TO WINDWRK-IDENT
                    READ WINDWRK
                    MOVE CARACTER-BUFFER  TO WINDWRK-CARACTER-BUFFER
                    MOVE ATTRIBUTE-BUFFER TO WINDWRK-ATTRIBUTE-BUFFER
                    EVALUATE FS-WINDWRK
                        WHEN "00" REWRITE WINDWRK-REG
                        WHEN "23" WRITE   WINDWRK-REG
                                  ADD 1 TO PILHA
                    END-EVALUATE
               WHEN (CODIGO-FUNCAO  = 1 OR 2 OR 3 OR 4)
                AND IDENT-JANELA  NOT = 0
                    MOVE NUM                      TO WINDWRK-NUM
                    MOVE IDENT-JANELA             TO WINDWRK-IDENT
                    READ WINDWRK
                    MOVE WINDWRK-CARACTER-BUFFER  TO CARACTER-BUFFER
                    MOVE WINDWRK-ATTRIBUTE-BUFFER TO ATTRIBUTE-BUFFER
                    IF   CODIGO-FUNCAO = 1 OR 2
                         CALL "CBL_WRITE_SCR_CHATTRS"
                               USING SCREEN-POSITION
                                     CARACTER-BUFFER
                                     ATTRIBUTE-BUFFER
                                     STRING-LENGTH
                    END-IF
                    IF   CODIGO-FUNCAO = 2 OR 3
                         PERFORM TEST AFTER
                                      UNTIL WINDWRK-IDENT NOT < PILHA
                                 DELETE WINDWRK RECORD
                                 ADD 1 TO WINDWRK-IDENT
                                 READ WINDWRK
                         END-PERFORM
                         SUBTRACT 1 FROM IDENT-JANELA
                         MOVE IDENT-JANELA TO PILHA
                         PERFORM ELIMINA-WORKFILE
                            THRU FIM-ELIMINA-WORKFILE
                    END-IF
                    IF   CODIGO-FUNCAO = 4
                         DELETE WINDWRK RECORD
                         IF   IDENT-JANELA = PILHA
                              SUBTRACT 1 FROM IDENT-JANELA
                              MOVE IDENT-JANELA TO PILHA
                              PERFORM ELIMINA-WORKFILE
                                 THRU FIM-ELIMINA-WORKFILE
                         END-IF
                    END-IF
               WHEN CODIGO-FUNCAO = 5
                    MOVE LINHA-SUPERIOR TO WLIN
                    COMPUTE LINHA-INFERIOR = LINHA-SUPERIOR
                                           + NUMERO-LINHAS
                                           - 1
                    IF   SENTIDO-A-ROLAR = 0
                         PERFORM NUMERO-LINHAS TIMES
                         MOVE ALL " " TO CARACTER-LIN (WLIN)
                                       (COLUNA-ESQUERDA: TAMANHO-LINHAS)
                         ADD  1       TO WLIN
                         END-PERFORM
                    END-IF
                    IF   SENTIDO-A-ROLAR = 6
                         PERFORM LINHAS-A-ROLAR TIMES
                                 MOVE LINHA-SUPERIOR TO WLIN2
                                 PERFORM NUMERO-LINHAS TIMES
                                   COMPUTE WLIN3 = WLIN2 - 1
                                  IF WLIN2 > LINHA-SUPERIOR
                                     MOVE CARACTER-LIN (WLIN2)
                                       (COLUNA-ESQUERDA: TAMANHO-LINHAS)
                                     TO CARACTER-LIN (WLIN3)
                                   (COLUNA-ESQUERDA: TAMANHO-LINHAS)
                                  END-IF
                                   ADD 1 TO WLIN2
                                 END-PERFORM
                                 MOVE ALL " " TO CARACTER-LIN
                                                  (LINHA-INFERIOR)
                                 (COLUNA-ESQUERDA: TAMANHO-LINHAS)
                         END-PERFORM
                    END-IF
                    IF   SENTIDO-A-ROLAR = 7
                         PERFORM LINHAS-A-ROLAR TIMES
                                 MOVE LINHA-INFERIOR TO WLIN2
                                 PERFORM NUMERO-LINHAS TIMES
                                   COMPUTE WLIN3 = WLIN2 + 1
                                  IF WLIN2 < LINHA-INFERIOR
                                     MOVE CARACTER-LIN (WLIN2)
                                       (COLUNA-ESQUERDA: TAMANHO-LINHAS)
                                     TO CARACTER-LIN (WLIN3)
                                   (COLUNA-ESQUERDA: TAMANHO-LINHAS)
                                  END-IF
                                   SUBTRACT 1 FROM WLIN2
                                 END-PERFORM
                                 MOVE ALL " " TO CARACTER-LIN
                                                  (LINHA-SUPERIOR)
                                 (COLUNA-ESQUERDA: TAMANHO-LINHAS)
                         END-PERFORM
                    END-IF
                    CALL "CBL_WRITE_SCR_CHARS" USING SCREEN-POSITION
                                                     CARACTER-BUFFER
                                                     STRING-LENGTH
               WHEN CODIGO-FUNCAO  = 10
                    MOVE LINHA-SUPERIOR TO WLIN
                    MOVE 0              TO WHOR
                    MOVE 8              TO BACK-COLOR
                    MOVE 1              TO FORE-COLOR
                    PERFORM NUMERO-LINHAS TIMES
                            COMPUTE WHOR-T = COLUNA-ESQUERDA - 1
                            PERFORM TAMANHO-LINHAS TIMES
                              ADD 1 TO WHOR
                              IF  BYTE-JANELA (WHOR) = "~"
                                  ADD  1 TO WHOR
                                  MOVE BYTE-X (WHOR) TO BACK-COLOR
                                  PERFORM UNTIL BACK-COLOR < 16
                                          SUBTRACT 16 FROM BACK-COLOR
                                  END-PERFORM
                                  ADD  1 TO WHOR
                                  MOVE BYTE-X (WHOR) TO FORE-COLOR
                                  PERFORM UNTIL FORE-COLOR < 16
                                          SUBTRACT 16 FROM FORE-COLOR
                                  END-PERFORM
                                  ADD  1 TO FORE-COLOR BACK-COLOR
                                  MOVE B (FORE-COLOR BACK-COLOR)
                                    TO ATT-N
                                  ADD 1 TO WHOR
                              END-IF
                              ADD  1     TO WHOR-T
                              MOVE ATT-X TO ATTRIBUTE-COL (WLIN WHOR-T)
                              MOVE BYTE-JANELA (WHOR)
                                TO CARACTER-COL (WLIN WHOR-T)
                            END-PERFORM
                            ADD 1 TO WLIN
                    END-PERFORM
                    CALL "CBL_WRITE_SCR_CHATTRS" USING SCREEN-POSITION
                                                       CARACTER-BUFFER
                                                       ATTRIBUTE-BUFFER
                                                       STRING-LENGTH
               WHEN CODIGO-FUNCAO  = 11
                    MOVE LINHA-SUPERIOR  TO WLIN
                    MOVE COLUNA-ESQUERDA TO WCOL
                    COMPUTE WFIM = WLIN + NUMERO-LINHAS - 1
                    MOVE CANTO-E-S       TO CARACTER-COL (WLIN WCOL)
                    MOVE CANTO-E-I       TO CARACTER-COL (WFIM WCOL)
                    COMPUTE VEZES = TAMANHO-LINHAS - 2
                    PERFORM VEZES TIMES
                            ADD  1         TO WCOL
                            MOVE LINHA-H-S TO CARACTER-COL (WLIN WCOL)
                            MOVE LINHA-H-I TO CARACTER-COL (WFIM WCOL)
                    END-PERFORM
                    ADD  1         TO WCOL
                    MOVE CANTO-D-S TO CARACTER-COL (WLIN WCOL)
                    MOVE CANTO-D-I TO CARACTER-COL (WFIM WCOL)

                    MOVE COLUNA-ESQUERDA TO WCOL
                    COMPUTE WFIM = WCOL + TAMANHO-LINHAS - 1
                    COMPUTE WLIN = LINHA-SUPERIOR + 1
                    COMPUTE VEZES = NUMERO-LINHAS - 2
                    PERFORM VEZES TIMES
                            MOVE LINHA-E-V TO CARACTER-COL (WLIN WCOL)
                            MOVE LINHA-D-V TO CARACTER-COL (WLIN WFIM)
                            ADD  1         TO WLIN
                    END-PERFORM
                    CALL "CBL_WRITE_SCR_CHARS" USING SCREEN-POSITION
                                                     CARACTER-BUFFER
                                                     STRING-LENGTH
           END-EVALUATE.

       000-99-FIM. GOBACK.

       ELIMINA-WORKFILE.

           IF   PILHA < 2
                MOVE 999 TO WINDWRK-IDENT
                MOVE 0   TO VEZ
                READ WINDWRK
                DELETE WINDWRK RECORD
                CLOSE WINDWRK
                DELETE FILE WINDWRK
           END-IF.

       FIM-ELIMINA-WORKFILE. EXIT.

       END PROGRAM WINDOW.

