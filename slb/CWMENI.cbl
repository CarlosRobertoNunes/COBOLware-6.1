       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWMENI INITIAL.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  18/02/2001.
       SECURITY.      *************************************************
                      *                                               *
                      *  Configura‡Æo de op‡äes do Relator            *
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
           05 TRAVADO             PIC X(002) VALUE "9D".
           05 CWCHECK-RG          PIC X(001) VALUE SPACE.
           05 CWCHECK-CT          PIC X(001) VALUE SPACE.
           05 MENSAGEM-ERRO       PIC X(060) VALUE SPACES.
           05 CAMPO                    PIC  9(002) VALUE ZERO.
           05 TECLA                    PIC  9(003) VALUE ZERO.
              COPY CWKEYS.
           05 RELATOR-IN               PIC  9(009) VALUE ZERO.
           05 RELATOR-OUT              PIC  9(009) VALUE ZERO.
           05 RELATOR                  PIC  X(050) VALUE "relator".
           05 RELATOR-YEAR             PIC  9(002) VALUE 28.
           05 I                        PIC  9(002) VALUE 25.
           05 Y                        PIC  9(002) VALUE 8.
           05 OPERADOR                 PIC  X(030) VALUE SPACES.
           05 TASK                     PIC  9(006) VALUE 0.
           05 CWIMPR-TASK              PIC  9(006) VALUE 0.
           05 PROGRAMA                 PIC  X(008) VALUE SPACES.

       COPY CWSEND.
       COPY CWMOUS.
       COPY CWCONF.

       SCREEN SECTION.

       01  CTAC-LIT-CWMENI.
           05 LINE 09 COLUMN 10 VALUE "Registros".
           05 LINE 10 COLUMN 10 VALUE "Contadores".
           05 LINE 14 COLUMN 06 VALUE "Lidos".
           05 LINE 15 COLUMN 06 VALUE "Selecionados".
           05 LINE 19 COLUMN 06 VALUE "Pasta do Relator".
           05 LINE 20 COLUMN 06 VALUE 'Ano base de "Janelamento"'.

       01  CTAC-VAR-CWMENI.
           05 LINE 09 COLUMN 07 PIC X(001) USING CWCHECK-RG.
           05 LINE 10 COLUMN 07 PIC X(001) USING CWCHECK-CT.
           05 LINE 14 COLUMN 19 PIC Z(009) USING RELATOR-IN.
           05 LINE 15 COLUMN 19 PIC Z(009) USING RELATOR-OUT.
           05 LINE 19 COLUMN 23 PIC X(050) USING RELATOR.
           05 LINE 20 COLUMN 32 PIC 9(002) USING RELATOR-YEAR.

       PROCEDURE DIVISION.

       000-INICIO.

           DISPLAY "CWFOOTLINE"  UPON ENVIRONMENT-NAME
           ACCEPT   footline-t   FROM ENVIRONMENT-VALUE
           SET CWSQLC-UPDATE TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           MOVE "RL" TO CWCONF-CHAVE
           SET CWSQLC-READ TO TRUE
           SET CWSQLC-EQUAL TO TRUE
           SET CWSQLC-IGNORE-LOCK TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           IF   FS-CWCONF = "00"
                MOVE CWCONF-RELATOR       TO RELATOR
                MOVE CWCONF-RELATOR-ROLL  TO CWCHECK-RG
                IF   CWCHECK-RG = "2"
                     MOVE "0" TO CWCHECK-RG
                END-IF
                MOVE CWCONF-RELATOR-COUNT TO CWCHECK-CT
                MOVE CWCONF-RELATOR-YEAR  TO RELATOR-YEAR
           END-IF
           CALL  "CWGETU"      USING OPERADOR
                                     TASK
                                     PROGRAMA
                                     '?'
           MOVE "PS" TO CWCONF-CHAVE
           MOVE OPERADOR TO CWCONF-NOME
           SET CWSQLC-READ TO TRUE
           SET CWSQLC-EQUAL TO TRUE
           SET CWSQLC-IGNORE-LOCK TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           IF   FS-CWCONF = "00"
                MOVE CWCONF-RELATOR-IN  TO RELATOR-IN
                MOVE CWCONF-RELATOR-OUT TO RELATOR-OUT
           END-IF
           EXEC COBOLware OBJECT GROUP
                     LINE 08 COLUMN 04
                     HEIGHT 02 WIDTH 18
                     Caption " Exibir"
           END-EXEC
           EXEC COBOLware OBJECT GROUP
                     LINE 13 COLUMN 04
                     HEIGHT 02 WIDTH 26
                     Caption " Limites para testes"
           END-EXEC
           EXEC COBOLware OBJECT GROUP
                     LINE 18 COLUMN 04
                     HEIGHT 02 WIDTH 71
                     Caption " Processamento"
           END-EXEC
      *    EXEC COBOLware OBJECT PUSH-BUTTON SMALL
      *              LINE 23 COLUMN 02 WIDTH 10
      *              CAPTION " Concluir "
      *              KEY F3
      *    END-EXEC
           EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                     LINE footline COLUMN 59 WIDTH 13
                     CAPTION " [esc]-~Sa¡da " KEY ESC TAB-OFF
           END-EXEC
           DISPLAY CTAC-LIT-CWMENI
                   CTAC-VAR-CWMENI
           ACCEPT CTAC-VAR-CWMENI
           ACCEPT TECLA FROM ESCAPE KEY
           MOVE "RL" TO CWCONF-CHAVE
           PERFORM TEST AFTER UNTIL FS-CWCONF NOT = TRAVADO
                   SET CWSQLC-READ TO TRUE
                   SET CWSQLC-EQUAL TO TRUE
                   CALL "CWCONF" USING CWSQLC
                                       CWCONF-REG
                                       FS-CWCONF
                                       KCO PCO
                   IF   FS-CWCONF = TRAVADO
                        CALL "CWCONF" USING "ISAM"
                   END-IF
           END-PERFORM
           MOVE RELATOR       TO CWCONF-RELATOR
           IF   CWCHECK-RG = "0"
                MOVE "2" TO CWCHECK-RG
           END-IF
           MOVE CWCHECK-RG    TO CWCONF-RELATOR-ROLL
           MOVE CWCHECK-CT    TO CWCONF-RELATOR-COUNT
           MOVE RELATOR-YEAR  TO CWCONF-RELATOR-YEAR
           IF   FS-CWCONF = "23"
                SET CWSQLC-WRITE TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           ELSE
                SET CWSQLC-REWRITE TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           END-IF
           MOVE "PS"     TO CWCONF-CHAVE
           MOVE OPERADOR TO CWCONF-NOME
           PERFORM TEST AFTER UNTIL FS-CWCONF NOT = TRAVADO
                   SET CWSQLC-READ TO TRUE
                   SET CWSQLC-EQUAL TO TRUE
                   CALL "CWCONF" USING CWSQLC
                                       CWCONF-REG
                                       FS-CWCONF
                                       KCO PCO
                   IF   FS-CWCONF = TRAVADO
                        CALL "CWCONF" USING "ISAM"
                   END-IF
           END-PERFORM
           MOVE RELATOR-IN  TO CWCONF-RELATOR-IN
           MOVE RELATOR-OUT TO CWCONF-RELATOR-OUT
           SET CWSQLC-REWRITE TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           SET CWSQLC-CLOSE TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO.

       000-99-FIM. GOBACK.

       END PROGRAM CWMENI.
