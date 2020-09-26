       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWMEN3.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  19/01/2001.
       SECURITY.      *************************************************
                      *                                               *
                      *  Configura‡Æo de op‡äes                       *
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
           05 FS                  PIC  X(002) VALUE SPACES.
           05 SAVE-FILESHARE      PIC  X(070) VALUE SPACES.
           05 WINDIR              PIC  X(050) VALUE SPACES.
           05 HELP                PIC  X(050) VALUE SPACES.
           05 CAMPO               PIC  9(002) VALUE ZERO.
           05 TECLA                    PIC  9(003) VALUE ZERO.
              COPY CWKEYS.
           05 CHECKS.
              10 CWCHECK-LOG           PIC  X(001) VALUE "0".
              10 CWCHECK-CODELOG       PIC  X(001) VALUE "0".
              10 CWCHECK-MOUSE         PIC  X(001) VALUE "0".
              10 CWCHECK-AUTOPASS      PIC  X(001) VALUE "0".
              10 CWCHECK-HIGH          PIC  X(001) VALUE "0".
              10 CWCHECK-DIR           PIC  X(001) VALUE "0".
              10 CWCHECK-SPOOL         PIC  X(001) VALUE "0".
              10 CWCHECK-PRTPOSIT      PIC  X(001) VALUE "0".

       COPY CWSEND.
       COPY CWUNIX.
       COPY CWCONF.

       SCREEN SECTION.

       01  CTAC-LIT-CWMEN3.
           05 LINE 08 COLUMN 04 VALUE "Geral".
           05 LINE 08 COLUMN 30 VALUE "Plug-ins".
           05 LINE 08 COLUMN 50 VALUE "FileShare".
           05 LINE 09 COLUMN 07 VALUE "Ativar mouse (texto)".
           05 LINE 09 COLUMN 30 VALUE "Abrir".
           05 LINE 09 COLUMN 50 VALUE "Servidor".
           05 LINE 10 COLUMN 07 VALUE "Colorir menu (texto)".
           05 LINE 10 COLUMN 30 VALUE "Fechar".
           05 LINE 10 COLUMN 50 VALUE "IP".
           05 LINE 11 COLUMN 07 VALUE "Exibir pasta corrent".
           05 LINE 11 COLUMN 27 VALUE "e".
           05 LINE 11 COLUMN 30 VALUE "Logon".
           05 LINE 11 COLUMN 50 VALUE "Usu rio".
           05 LINE 12 COLUMN 07 VALUE "Relat¢rios em spool".
           05 LINE 12 COLUMN 30 VALUE "Logoff".
           05 LINE 12 COLUMN 50 VALUE "Senha".
           05 LINE 13 COLUMN 07 VALUE "Verificar impressora".
           05 LINE 13 COLUMN 30 VALUE "Pr‚-op‡Æo".
           05 LINE 14 COLUMN 30 VALUE "P¢s-op‡Æo".
           05 LINE 15 COLUMN 04 VALUE "Seguran‡a".
           05 LINE 16 COLUMN 04 VALUE "Senhas, m¡nimo".
           05 LINE 16 COLUMN 22 VALUE "caracteres, Expirar ".
           05 LINE 16 COLUMN 42 VALUE "em".
           05 LINE 16 COLUMN 49 VALUE "dias, Hist¢rico".
           05 LINE 16 COLUMN 69 VALUE "senhas".
           05 LINE 17 COLUMN 04 VALUE "Bloqueio em".
           05 LINE 17 COLUMN 20 VALUE "Tentativas".
           05 LINE 17 COLUMN 31 VALUE "erradas".
           05 LINE 17 COLUMN 40 VALUE "Limite de ociosidade".
           05 LINE 17 COLUMN 65 VALUE "segundos".
           05 LINE 18 COLUMN 07 VALUE "Gerar Log na pasta".
           05 LINE 19 COLUMN 07 VALUE "Log codificado".
           05 LINE 21 COLUMN 04 VALUE "Pasta de help".

       01  CTAC-VAR-CWMEN3.
           05 LINE 09 COLUMN 04 PIC X(001) USING CWCHECK-MOUSE.
           05 LINE 10 COLUMN 04 PIC X(001) USING CWCHECK-HIGH.
           05 LINE 11 COLUMN 04 PIC X(001) USING CWCHECK-DIR.
           05 LINE 12 COLUMN 04 PIC X(001) USING CWCHECK-SPOOL.
           05 LINE 13 COLUMN 04 PIC X(001) USING CWCHECK-PRTPOSIT.
           05 LINE 09 COLUMN 40 PIC X(008) USING CWCONF-MASTER.
           05 LINE 10 COLUMN 40 PIC X(008) USING CWCONF-END.
           05 LINE 11 COLUMN 40 PIC X(008) USING CWCONF-LOGIN.
           05 LINE 12 COLUMN 40 PIC X(008) USING CWCONF-LOGOUT.
           05 LINE 13 COLUMN 40 PIC X(008) USING CWCONF-CALLIN.
           05 LINE 14 COLUMN 40 PIC X(008) USING CWCONF-CALLOUT.
           05 LINE 09 COLUMN 59 PIC X(015) USING CWCONF-FSSERVER.
           05 LINE 10 COLUMN 59 PIC X(015) USING CWCONF-CCITCP2.
           05 LINE 11 COLUMN 59 PIC X(020) USING CWCONF-FSUSERNAME.
           05 LINE 12 COLUMN 59 PIC X(020)
                 USING CWCONF-FSPASSWORD SECURE.
           05 LINE 16 COLUMN 19 PIC Z(002) USING CWCONF-MIN-SENHA.
           05 LINE 16 COLUMN 45 PIC Z(003) USING CWCONF-EXPIRE.
           05 LINE 16 COLUMN 65 PIC Z(003) USING CWCONF-REUSE.
           05 LINE 17 COLUMN 16 PIC Z(003) USING CWCONF-RETRY.
           05 LINE 17 COLUMN 61 PIC Z(003) USING CWCONF-TIMEOUT.
           05 LINE 18 COLUMN 04 PIC X(001) USING CWCHECK-LOG.
           05 LINE 18 COLUMN 26 PIC X(050) USING CWCONF-LOGDIR.
           05 LINE 19 COLUMN 04 PIC X(001) USING CWCHECK-CODELOG.
           05 LINE 21 COLUMN 18 PIC X(050) USING HELP.

       01  CTAC-VAR-CWMEN3-U.
           05 LINE 09 COLUMN 04 PIC X(001) USING CWCHECK-MOUSE.
           05 LINE 10 COLUMN 04 PIC X(001) USING CWCHECK-HIGH.
           05 LINE 11 COLUMN 04 PIC X(001) USING CWCHECK-DIR.
           05 LINE 12 COLUMN 04 PIC X(001) USING CWCHECK-SPOOL.
           05 LINE 13 COLUMN 04 PIC X(001) USING CWCHECK-PRTPOSIT.
           05 LINE 09 COLUMN 40 PIC X(008) USING CWCONF-MASTER.
           05 LINE 10 COLUMN 40 PIC X(008) USING CWCONF-END.
           05 LINE 11 COLUMN 40 PIC X(008) USING CWCONF-LOGIN.
           05 LINE 12 COLUMN 40 PIC X(008) USING CWCONF-LOGOUT.
           05 LINE 13 COLUMN 40 PIC X(008) USING CWCONF-CALLIN.
           05 LINE 14 COLUMN 40 PIC X(008) USING CWCONF-CALLOUT.
           05 LINE 16 COLUMN 19 PIC Z(002) USING CWCONF-MIN-SENHA.
           05 LINE 16 COLUMN 45 PIC Z(003) USING CWCONF-EXPIRE.
           05 LINE 16 COLUMN 65 PIC Z(003) USING CWCONF-REUSE.
           05 LINE 17 COLUMN 16 PIC Z(003) USING CWCONF-RETRY.
           05 LINE 17 COLUMN 61 PIC Z(003) USING CWCONF-TIMEOUT.
           05 LINE 18 COLUMN 04 PIC X(001) USING CWCHECK-LOG.
           05 LINE 18 COLUMN 26 PIC X(050) USING CWCONF-LOGDIR.
           05 LINE 19 COLUMN 04 PIC X(001) USING CWCHECK-CODELOG.
           05 LINE 21 COLUMN 18 PIC X(050) USING HELP.

       PROCEDURE DIVISION.

       000-INICIO.

           PERFORM 800-INICIAIS      THRU 800-99-FIM
           PERFORM 100-PROCESSAMENTO THRU 100-99-FIM
           PERFORM 900-FINAIS        THRU 900-99-FIM.

       000-99-FIM. GOBACK.

       100-PROCESSAMENTO.

           EXEC COBOLware OBJECT VALIDATE
                FIELD CWCONF-MASTER PROGRAM "CWMENM" USING CWCONF-MASTER
           END-EXEC

           EXEC COBOLware OBJECT VALIDATE
                FIELD CWCONF-END    PROGRAM "CWMENM" USING CWCONF-END
           END-EXEC

           EXEC COBOLware OBJECT VALIDATE
                FIELD CWCONF-LOGIN  PROGRAM "CWMENM" USING CWCONF-LOGIN
           END-EXEC

           EXEC COBOLware OBJECT VALIDATE
                FIELD CWCONF-LOGOUT PROGRAM "CWMENM" USING CWCONF-LOGOUT
           END-EXEC

           EXEC COBOLware OBJECT VALIDATE
                FIELD CWCONF-CALLIN PROGRAM "CWMENM" USING CWCONF-CALLIN
           END-EXEC

           EXEC COBOLware OBJECT VALIDATE
                FIELD CWCONF-CALLOUT
                PROGRAM "CWMENM" USING CWCONF-CALLOUT
           END-EXEC

           EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                LINE footline COLUMN 2 CAPTION " f2-~Aplicar " WIDTH 12
                KEY F2
           END-EXEC
           EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                LINE footline COLUMN 14 CAPTION " f3-~Cancelar "
                WIDTH 10
                KEY F3
           END-EXEC
           EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                     LINE footline COLUMN 59 WIDTH 13
                     CAPTION " [esc]-~Sa¡da " KEY ESC TAB-OFF
           END-EXEC.

       DONE.
           MOVE CWCONF-LOG        TO CWCHECK-LOG
           MOVE CWCONF-CODELOG    TO CWCHECK-CODELOG
           MOVE CWCONF-MOUSE      TO CWCHECK-MOUSE
           MOVE CWCONF-AUTOPASS   TO CWCHECK-AUTOPASS
           MOVE CWCONF-HIGH       TO CWCHECK-HIGH
           MOVE CWCONF-DIR        TO CWCHECK-DIR
           MOVE CWCONF-SPOOL      TO CWCHECK-SPOOL
           MOVE CWCONF-PRTPOSIT   TO CWCHECK-PRTPOSIT
           INSPECT CHECKS CONVERTING "2" TO "0"
           DISPLAY CTAC-LIT-CWMEN3
           IF  CWUNIX-ON
               ACCEPT  CTAC-VAR-CWMEN3-U
           ELSE
               ACCEPT  CTAC-VAR-CWMEN3
           END-IF
           if  cwconf-retry < 3
               move 0 to cwconf-retry
           end-if

           ACCEPT TECLA FROM ESCAPE KEY
           IF   ESC OR  F3
                GO TO 100-99-FIM
           END-IF

           INSPECT CHECKS CONVERTING "0" TO "2"
           MOVE CWCHECK-LOG        TO CWCONF-LOG
           MOVE CWCHECK-CODELOG    TO CWCONF-CODELOG
           MOVE CWCHECK-MOUSE      TO CWCONF-MOUSE
           MOVE CWCHECK-AUTOPASS   TO CWCONF-AUTOPASS
           MOVE CWCHECK-HIGH       TO CWCONF-HIGH
           MOVE CWCHECK-DIR        TO CWCONF-DIR
           MOVE CWCHECK-SPOOL      TO CWCONF-SPOOL
           MOVE CWCHECK-PRTPOSIT   TO CWCONF-PRTPOSIT
           IF   CWUNIX-ON
                MOVE HELP TO CWCONF-HELPDIR-U
           ELSE
                MOVE HELP TO CWCONF-HELPDIR-D
           END-IF
           IF   F2
                IF   CWCONF-FILESHARE NOT = SAVE-FILESHARE
                     MOVE 1 TO CWCONF-FSCHANGED
                ELSE
                     MOVE 0 TO CWCONF-FSCHANGED
                END-IF
                SET CWSQLC-DELETE TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
                SET CWSQLC-WRITE TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
                MOVE "LG" TO CWCONF-CHAVE
                SET CWSQLC-READ TO TRUE
                SET CWSQLC-EQUAL TO TRUE
                SET CWSQLC-IGNORE-LOCK TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           END-IF
           GO TO DONE.

       100-99-FIM. EXIT.

       800-INICIAIS.

           DISPLAY "CWFOOTLINE"  UPON ENVIRONMENT-NAME
           ACCEPT   footline-t   FROM ENVIRONMENT-VALUE
           DISPLAY "CWIN"   UPON ENVIRONMENT-NAME
           ACCEPT   WINDIR  FROM ENVIRONMENT-VALUE
           CALL "CWUNIX" USING PARAMETROS-CWUNIX
           SET CWSQLC-UPDATE TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           MOVE "LG" TO CWCONF-CHAVE
           SET CWSQLC-READ TO TRUE
           SET CWSQLC-EQUAL TO TRUE
           SET CWSQLC-IGNORE-LOCK TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           IF   FS-CWCONF = "23"
                MOVE "LG"     TO CWCONF-REGLG
                INITIALIZE CWCONF-OPCOES-LOG
                MOVE 1        TO CWCONF-LOG
                MOVE 0        TO CWCONF-AUTOPASS
                MOVE 1        TO CWCONF-CLOCK
                MOVE 1        TO CWCONF-MOUSE
                MOVE 1        TO CWCONF-DIR
                MOVE 60       TO CWCONF-RETRY
                MOVE 3        TO CWCONF-TIMEOUT
                MOVE ALL "1"  TO CWCONF-FILLER (1: )
                MOVE "."      TO CWCONF-LOGDIR
                MOVE 1        TO CWCONF-PRTPOSIT
                SET CWSQLC-WRITE TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
                MOVE "LG" TO CWCONF-CHAVE
                SET CWSQLC-READ TO TRUE
                SET CWSQLC-EQUAL TO TRUE
                SET CWSQLC-IGNORE-LOCK TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           END-IF
           IF   CWUNIX-ON
                MOVE CWCONF-HELPDIR-U TO HELP
           ELSE
                MOVE CWCONF-HELPDIR-D TO HELP
           END-IF
           IF   CWCONF-LOGDIR = LOW-VALUES
                MOVE "." TO CWCONF-LOGDIR
                MOVE 0   TO CWCONF-CODELOG
           END-IF
           IF (CWCONF-RETRY NOT NUMERIC)
           OR  CWCONF-RETRY = 1111
               MOVE 3 TO CWCONF-RETRY
           END-IF
           INSPECT CWCONF-FILESHARE CONVERTING X"00" TO SPACE
           MOVE CWCONF-FILESHARE TO SAVE-FILESHARE
           DISPLAY CTAC-LIT-CWMEN3
                   CTAC-VAR-CWMEN3.

       800-99-FIM. EXIT.

       900-FINAIS.

           EXEC COBOLware Object (DROP) END-EXEC
           CANCEL "CWGETL"
           SET CWSQLC-CLOSE TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           IF   CWCONF-FSCHANGED = 1
                EXEC COBOLware Send
                     Message "Altera‡Æo de FileShare requer logon"
                END-EXEC
                DISPLAY "CWLG" UPON ENVIRONMENT-NAME
                DISPLAY "ON"   UPON ENVIRONMENT-VALUE
Junior*         STOP RUN
           END-IF.

       900-99-FIM. EXIT.

       END PROGRAM CWMEN3.
