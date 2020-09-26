       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWACTR.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  23/05/1996.
       SECURITY.      *************************************************
                      *                                               *
                      *  Reatorio de chaves de ativacao               *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 footline-t.
              10 footline         PIC  9(002) VALUE 23.
           05 TECLA               PIC 9(03) VALUE 0. COPY CWKEYS.
           05 MESES               PIC 9(04) VALUE 0.
           05 SENHA               PIC 9(06) VALUE 0.
           05 VALIDADE            PIC 9(08) VALUE 0.
           05 REDEFINES VALIDADE.
              06 DIA PIC 99.
              06 MES PIC 99.
              06 ANO PIC 9999.
           05 PROGRAMA                 PIC X(008) VALUE SPACES.
           05 NOME                     PIC X(030) VALUE SPACES.
           05 TASK                     PIC 9(006) VALUE 0.
           05 NIVEL                    PIC 9(001) VALUE 0.

       01  AREAS-DE-TRABALHO-2.
           05 MSG-1 PIC X(11) VALUE "[esc]-Sa¡da".

       01  LINHAS-DE-IMPRESSAO-CLIC.
       02  LINHA-01.
           05 FILLER                         PIC  X(018) VALUE
              "CHAVES DA LICENCA ".
           05 CLIC-SENHA                     PIC  9(006) VALUE ZEROS.
       02  LINHA-02.
           05 CLIC-MESES                     PIC  Z(004) VALUE ZEROS.
           05 FILLER                         PIC  X(013) VALUE
              " MESES DESDE ".
           05 CLIC-VALIDADE-TOP              PIC  99/99/9999.
       02  LINHA-03.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 FILLER PIC  X(020) VALUE "ATE        CHAVE".
           05 FILLER PIC  X(020) VALUE "+10 DIAS   CHAVE".
           05 FILLER PIC  X(020) VALUE "+15 DIAS   CHAVE".
           05 FILLER PIC  X(020) VALUE "+20 DIAS   CHAVE".
       02  LINHA-04.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CLIC-VALIDADE                  PIC  99/99/9999.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CLIC-ATIVACAO                  PIC  999999.
           05 FILLER                         PIC  X(003) VALUE SPACES.
           05 CLIC-VALIDADE-10               PIC  99/99/9999.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CLIC-ATIVACAO-10               PIC  999999.
           05 FILLER                         PIC  X(003) VALUE SPACES.
           05 CLIC-VALIDADE-15               PIC  99/99/9999.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CLIC-ATIVACAO-15               PIC  999999.
           05 FILLER                         PIC  X(003) VALUE SPACES.
           05 CLIC-VALIDADE-20               PIC  99/99/9999.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CLIC-ATIVACAO-20               PIC  999999.

       COPY CWIMPR.
       COPY CWACTV.
       COPY CWTIME.
       COPY CWSEND.
       COPY CWCONF.

       SCREEN SECTION.

       01  CTAC-LIT-CWACTR.
           05 LINE 08 COLUMN 03 VALUE "Licen‡a           ".
           05 LINE 10 COLUMN 03 VALUE "Validade inicial  ".
           05 LINE 12 COLUMN 03 VALUE "Meses a relacionar".

       01  CTAC-VAR-CWACTR.
           05 LINE 08 COLUMN 22 PIC Z(006) USING SENHA.
           05 LINE 10 COLUMN 22 PIC 99/99/9999
                                    USING VALIDADE BLANK ZERO.
           05 LINE 12 COLUMN 22 PIC Z(004) USING MESES.

       PROCEDURE DIVISION.

       000-INICIO.

           PERFORM 800-INICIAIS      THRU 800-99-FIM
           PERFORM 100-PROCESSAMENTO THRU 100-99-FIM
           PERFORM 900-FINAIS        THRU 900-99-FIM.

       000-99-FIM. GOBACK.

       100-PROCESSAMENTO.

            PERFORM MESES TIMES
                    MOVE SENHA    TO CWACTV-SENHA
                    MOVE VALIDADE TO CWACTV-VALIDADE CLIC-VALIDADE-TOP
                    CALL "CWACTV" USING PARAMETROS-CWACTV
                    ADD 1 TO MES
                    IF  MES = 13
                        ADD 1  TO ANO
                        MOVE 1 TO MES
                    END-IF

                    MOVE VALIDADE           TO CLIC-VALIDADE
                    MOVE CWACTV-VALIDADE-10 TO CLIC-VALIDADE-10
                    MOVE CWACTV-VALIDADE-15 TO CLIC-VALIDADE-15
                    MOVE CWACTV-VALIDADE-20 TO CLIC-VALIDADE-20

                    MOVE CWACTV-ATIVACAO    TO CLIC-ATIVACAO
                    MOVE CWACTV-ATIVACAO-10 TO CLIC-ATIVACAO-10
                    MOVE CWACTV-ATIVACAO-15 TO CLIC-ATIVACAO-15
                    MOVE CWACTV-ATIVACAO-20 TO CLIC-ATIVACAO-20

                    MOVE LINHA-04    TO CWIMPR-DETAIL
                    CALL "CWIMPR" USING PARAMETROS-CWIMPR
                    MOVE SPACES      TO LINHA-04
                    IF   CWIMPR-END-PRINT
                         GO TO 100-99-FIM
                    END-IF
            END-PERFORM.

       100-99-FIM. EXIT.

       800-INICIAIS.

           CALL "CWGETU" USING NOME TASK PROGRAMA '?'
           CALL "CWTEXT" USING AREAS-DE-TRABALHO-2
                     LENGTH OF AREAS-DE-TRABALHO-2
           DISPLAY "CWFOOTLINE"  UPON ENVIRONMENT-NAME
           ACCEPT   footline-t   FROM ENVIRONMENT-VALUE
           SET CWSQLC-OPEN TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           MOVE "PS" TO CWCONF-REGPS
           MOVE NOME TO CWCONF-ELEMENTO
           SET CWSQLC-READ TO TRUE
           SET CWSQLC-IGNORE-LOCK TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           IF   FS-CWCONF = "00"
                MOVE CWCONF-NIVEL-PS TO NIVEL
           END-IF
           SET CWSQLC-CLOSE TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           IF  NIVEL NOT = 9
           OR  PROGRAMA NOT = "CWACTR"
               GOBACK
           END-IF

           DISPLAY CTAC-LIT-CWACTR
           EXEC COBOLware Object PUSH-BUTTON SMALL
                     LINE FOOTLINE COLUMN 60 COLUMNS 11
                     LABEL "[esc]-~Sa¡da" KEY ESC TAB-OFF
           END-EXEC
           PERFORM TEST AFTER UNTIL ESC OR VALIDADE NOT = 0
                   ACCEPT CTAC-VAR-CWACTR
                   ACCEPT TECLA FROM ESCAPE KEY
                   IF   NOT ESC
                        SET  CWTIME-NORMAL   TO TRUE
                        SET  CWTIME-VALIDATE TO TRUE
                        MOVE VALIDADE        TO CWTIME-DATE
                        CALL "CWTIME"     USING PARAMETROS-CWTIME
                        MOVE CWTIME-DATE-FINAL TO VALIDADE
                        IF   VALIDADE = 0
                             MOVE "Data de validade inicial inv lida"
                                  TO CWSEND-MSG
                             CALL "CWSEND" USING PARAMETROS-CWSEND
                        END-IF
                   END-IF
           END-PERFORM
           EXEC COBOLware OBJECT (DROP) END-EXEC
           IF   ESC
                GOBACK
           ELSE
                MOVE SENHA     TO CLIC-SENHA
                MOVE VALIDADE  TO CLIC-VALIDADE-TOP
                MOVE MESES     TO CLIC-MESES
                IF MESES = 1
                   MOVE SPACES TO LINHA-02 (9: 2)
                END-IF
                MOVE LINHA-01  TO CWIMPR-TITLE
                MOVE LINHA-02  TO CWIMPR-SUB-TITLE
                MOVE LINHA-03  TO CWIMPR-HEADER-1
                MOVE "CWACTRA" TO CWIMPR-REPORT
                SET  CWIMPR-SIZE-080  TO TRUE
                MOVE "CHAVES DE ATIVACAO" TO CWIMPR-NOTE
           END-IF.

       800-99-FIM. EXIT.

       900-FINAIS.

           IF  NOT CWIMPR-END-PRINT
               SET CWIMPR-CLOSE TO TRUE
               CALL "CWIMPR" USING PARAMETROS-CWIMPR
           END-IF.

       900-99-FIM. EXIT.

       END PROGRAM CWACTR.
