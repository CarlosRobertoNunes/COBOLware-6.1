       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWLOGO.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  09/07/2012.
       SECURITY.      *************************************************
                      *                                               *
                      *  Mudan‡a de usu rio ativo                     *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.

       78  SP2 value "SP2".
       01  AREAS-DE-TRABALHO-1. COPY CWCASE.
           05 NIVEL-LOGON              PIC  X(001) VALUE SPACES.
           05 cwNOframe                PIC  X(003) VALUE SPACES.
           05 cwframe                  PIC  X(018) VALUE SPACES.
           05 NOME-A                   PIC  X(030) VALUE SPACES.
           05 NOME-W                   PIC  X(030) VALUE SPACES.
           05 NOME                     PIC  X(030) VALUE SPACES.
           05 TASK                     PIC  9(006) VALUE ZERO.
           05 PROGRAMA                 PIC  X(008) VALUE SPACES.
           05 NUMERO                   PIC  9(018) VALUE 0.
           05 I                        PIC  9(004) VALUE ZERO.
           05 II                       PIC  9(004) VALUE 0.
           05 CWNUMERO                 PIC  X(018) VALUE SPACES.
           05 LOGON-LINE               PIC  9(002) VALUE 11.
           05 LOGON-COLUMN             PIC  9(002) VALUE 20.
           05 LOGON-COLOR              PIC  9(003) VALUE 62.
           05 COR                      PIC  9(003) VALUE ZERO.
           05 RESPOSTA                 PIC  X(001) VALUE "S".
           05 TTY                      PIC  X(010) VALUE SPACES.
           05 TTY-NOME                 PIC  X(029) VALUE SPACES.
           05 LOGIN-FLAG               PIC  9(001) VALUE 0.
           05 SENHA-AUTO                           VALUE SPACES.
              10 SENHA-X  OCCURS 30    PIC  9(002) COMP-X.
           05 CHECK-NOME               PIC  X(001) VALUE "N".
              88 NOME-OK                           VALUE "S".
           05 SENHA                    PIC  X(030).
           05 CHECK-SENHA              PIC  X(001).
              88 SENHA-OK                          VALUE "S".
           05 ENTROU                   PIC  9(001) VALUE 0.
           05 CWLOGPGM                 PIC  X(010) VALUE SPACES.
           05 HOJE                     PIC  9(008) VALUE 0.
           05 DIAS-SENHA               PIC  9(007) VALUE 0.
           05 EXPIRE                   PIC  9(003) VALUE 0.
           05 EXPIRADA                 PIC  9(001) VALUE 0.
           05 GRUPO                    PIC  X(022) VALUE SPACES.
           05 CHECK-NIVEL              PIC  X(001) VALUE '0'.
           05 FSSERVER                 PIC  X(050) VALUE SPACES.
           05 ESQUECI                  PIC  X(060) VALUE SPACES.
           05 SENHA-PROV               PIC  9(006) VALUE 0.
           05 N                        PIC  9(002) VALUE 0.
           05 IMPRESSORA               PIC  X(008) VALUE SPACES.
           05 OBS                      PIC  X(035) VALUE SPACES.
           05 LEN-LEMBRETE             PIC  9(002) VALUE 0.
           05 ESQUECI-RESPOSTA         PIC  X(030) VALUE SPACES.
           05 lb-prnter-3              PIC  X(024) VALUE SPACES.
           05 TTY-NOME-A               PIC  X(024) VALUE SPACES.
           05 ADM                      PIC  X(001) VALUE SPACE.
           05 ER-PRNTER.
              10 FS-PRNTER             PIC  X(002) VALUE "00".
              10 LB-PRNTER                         VALUE "PRN".
                 15 IMPR               PIC  X(008).
                 15 FILLER             PIC  X(247).

       COPY CWUNIX.
       COPY CWTIME.
       COPY CWSEND.
       COPY CWGETL.
       COPY CWCONF.
       COPY CWSPWS.
       COPY CWSTAT.

       LINKAGE SECTION.

       COPY CWLOGO.

       SCREEN SECTION.

       01 TELA-NOME.
          10 LINE 02 COLUMN 40 PIC X      FROM SPACE.
          10 LINE 02 COLUMN 41 PIC X(024) FROM TTY-NOME.
          10 LINE 05 COLUMN 41 PIC X(028) FROM lb-prnter-3.

       PROCEDURE DIVISION USING PARAMETROS-CWLOGO.

       000-INICIO.

           DISPLAY 'CWLOGON-NIVEL' UPON ENVIRONMENT-NAME
           ACCEPT  NIVEL-LOGON     FROM ENVIRONMENT-VALUE
           DISPLAY "CWLOGPGM"    UPON ENVIRONMENT-NAME
           ACCEPT   CWLOGPGM     FROM ENVIRONMENT-VALUE
           INSPECT  CWLOGPGM   CONVERTING MINUSCULAS TO MAIUSCULAS
           CALL "CWUNIX" USING PARAMETROS-CWUNIX
           CALL "CWGETL" USING PARAMETROS-CWGETL
           CALL "CWGETU" USING NOME TASK PROGRAMA "?"
           MOVE NOME TO NOME-A
           IF CWUNIX-GUI
              COPY CWSPPD.
              MOVE LOW-VALUES TO SP2-PD-DATA
              MOVE "CWMENU"   TO SP2-PD-NAME
              CALL SP2   USING SP2-GET-PANEL-DEF  SP2-PANEL-DEF
              MOVE SP2-PD-MSG-TEXT   TO STATUS-DEF
              DISPLAY "CWLOGON-LINE"  UPON ENVIRONMENT-NAME
              PERFORM AJUSTA
              IF  NUMERO NOT = 0
                  MOVE NUMERO TO LOGON-LINE
              END-IF
              DISPLAY "CWLOGON-COLUMN"  UPON ENVIRONMENT-NAME
              PERFORM AJUSTA
              IF  NUMERO NOT = 0
                  MOVE NUMERO TO LOGON-COLUMN
              END-IF
              DISPLAY "CWLOGON-COLOR"  UPON ENVIRONMENT-NAME
              PERFORM AJUSTA
              IF  NUMERO NOT = 0
                  MOVE NUMERO TO LOGON-COLOR
              END-IF
           ELSE
              DISPLAY "CWNOFRAME" UPON ENVIRONMENT-NAME
              ACCEPT CWNOFRAME    FROM ENVIRONMENT-VALUE
              INSPECT CWNOFRAME   CONVERTING MINUSCULAS TO MAIUSCULAS
           END-IF
           MOVE 0 TO LOGIN-FLAG
           SET CWSQLC-UPDATE TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           PERFORM UNTIL (NOME-OK
                    AND   SENHA-OK)
                    OR    RESPOSTA = "N"
                   PERFORM LOGIN THRU END-LOGIN
           END-PERFORM
           IF  (RESPOSTA NOT = "N")
           AND (NOME NOT = NOME-A)
                DISPLAY 'CWLOGN'    UPON ENVIRONMENT-NAME
                DISPLAY  NOME       UPON ENVIRONMENT-VALUE
                DISPLAY 'CWLOGG'    UPON ENVIRONMENT-NAME
                DISPLAY  GRUPO      UPON ENVIRONMENT-VALUE
                DISPLAY 'CWLOGI'    UPON ENVIRONMENT-NAME
                DISPLAY IMPRESSORA  UPON ENVIRONMENT-VALUE
                DISPLAY 'CWLOGP'    UPON ENVIRONMENT-NAME
                DISPLAY CHECK-NIVEL UPON ENVIRONMENT-VALUE
                IF NOT CWUNIX-GUI
                   DISPLAY "CWFRAMEPGM"  UPON ENVIRONMENT-NAME
                   ACCEPT   CWFRAME      FROM ENVIRONMENT-VALUE
                   IF CWFRAME NOT = SPACES
                      CALL CWFRAME USING 'U' NOME
                           ON EXCEPTION
                              MOVE SPACES TO CWFRAME
                      END-CALL
                   END-IF
                   IF CWFRAME = SPACES
                      MOVE NOME TO NOME-W
                      MOVE NOME-A TO NOME
                      PERFORM 250-AJUSTA-TTY-NOME THRU 250-99-FIM
                      MOVE NOME-W TO NOME
                      CALL "CBL_READ_SCR_CHARS" USING X"0128"
                                                TTY-NOME-A
                                                X"0018"
                      IF TTY-NOME = TTY-NOME-A
                         PERFORM 250-AJUSTA-TTY-NOME THRU 250-99-FIM
                         DISPLAY TELA-NOME
                      END-IF
                  ELSE
                      CALL CWFRAME USING 'i' lb-prnter-3
                  END-IF
                END-IF
           END-IF

           SET CWSQLC-CLOSE TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO.

       000-99-FIM. GOBACK.

       LOGIN.

           IF CWUNIX-GUI
              CALL SP2   USING SP2-DISPLAY-WINDOW SP2-NULL-PARM
           END-IF
           SET CWTIME-REVERSED     TO TRUE
           SET CWTIME-TODAY        TO TRUE
           CALL "CWTIME"        USING PARAMETROS-CWTIME
           MOVE CWTIME-DATE-FINAL  TO HOJE
           MOVE SPACE TO RESPOSTA

           IF  NOT NOME-OK
           OR  NOT SENHA-OK
               MOVE 1 TO ENTROU
               IF   NOME = "LOGON"
                    MOVE SPACES TO NOME
               END-IF
               MOVE "N" TO CHECK-SENHA
               IF  CWLOGPGM NOT = SPACES
                   CALL CWLOGPGM USING NOME SENHA
                                       RESPOSTA
                                       cwlogo-user cwlogo-password
                   ON EXCEPTION
                      CONTINUE
                   NOT ON EXCEPTION
                       GO TO LOGIN-OK
                   END-CALL
               END-IF
               IF  NOME-OK
               OR (cwlogo-user NOT = SPACES)
                   IF  cwlogo-user = SPACES
                   AND (NOME NOT = SPACES)
                       MOVE NOME TO cwlogo-user
                   END-IF
                   IF  cwlogo-password = SPACES
                       IF CWUNIX-GUI
                          MOVE 23 TO COR
                       ELSE
                          MOVE LOGON-COLOR TO COR
                       END-IF
                       EXEC COBOLware BoxDialog
                            LINE LOGON-LINE COLUMN LOGON-COLUMN
                            HEADER "Logon"
                            COLOR COR
                            Caption(1) "Usu rio"
                            Caption(2) "Senha  "
                            Size(1) 30
                            Size(2) 30 (Secure(2))
                            Data(1) cwlogo-user (Protected(1))
                            Data(2) cwlogo-password;SENHA
                            CANCEL ;RESPOSTA
                       END-EXEC
                   ELSE
                       MOVE cwlogo-password TO SENHA
                   END-IF
                   MOVE cwlogo-user TO NOME
               ELSE
                   IF  cwlogo-password NOT = SPACES
                       EXEC COBOLware BoxDialog
                            LINE LOGON-LINE COLUMN LOGON-COLUMN
                            HEADER "Logon"
                            COLOR LOGON-COLOR
                            Caption(1) "Usu rio"
                            Caption(2) "Senha  "
                            Size(1) 30
                            Size(2) 30 (Secure(2))
                            Data(1) cwlogo-user;NOME
                            Data(2) cwlogo-password (Protected(2))
                            CANCEL ;RESPOSTA
                       END-EXEC
                       MOVE cwlogo-password TO SENHA
                   ELSE
                       EXEC COBOLware BoxDialog
                            LINE LOGON-LINE COLUMN LOGON-COLUMN
                            HEADER "Logon"
                            COLOR LOGON-COLOR
                            Caption(1) "Usu rio"
                            Caption(2) "Senha  "
                            Size(1) 30
                            Size(2) 30 (Secure(2))
                            Data(1) cwlogo-user;NOME
                            Data(2) cwlogo-password;SENHA
                            CANCEL ;RESPOSTA
                       END-EXEC
                   END-IF
               END-IF

               IF  RESPOSTA = "Y"
                   MOVE RESPOSTA TO CWLOGO-CANCEL
                   MOVE "N" TO CHECK-NOME
                               CHECK-SENHA
                               RESPOSTA
               ELSE
                   MOVE SPACE TO RESPOSTA
               END-IF
           END-IF.

       LOGIN-OK.

           IF  (NOT NOME-OK)
           AND (RESPOSTA NOT = "N")
               IF   CHECK-NOME = "+"
                    MOVE "S" TO CHECK-NOME
               END-IF
               IF   cwlogo-user NOT = SPACES
                    MOVE cwlogo-user TO NOME
                    MOVE SPACES TO cwlogo-user
               END-IF
               INSPECT NOME CONVERTING MINUSCULAS TO MAIUSCULAS
               MOVE "PS" TO CWCONF-REG
               MOVE NOME TO CWCONF-NOME
               SET CWSQLC-READ TO TRUE
               SET CWSQLC-EQUAL TO TRUE
               SET CWSQLC-IGNORE-LOCK TO TRUE
               CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
               IF   NOME NOT = SPACES
               AND  FS-CWCONF = "00"
                    CALL "CWMENL" USING FS-CWCONF
                                        CWCONF-NOME
                                        CWCONF-GRUPO
                    CANCEL "CWMENL"
               END-IF
               PERFORM 250-AJUSTA-TTY-NOME THRU 250-99-FIM
               IF   FS-CWCONF NOT = "00"
               OR   NOME = SPACES
               OR   NOME = LOW-VALUES
                    MOVE "Informe nome do usu rio."
                      TO CWSEND-MSG
                    IF   NOME NOT = LOW-VALUES
                    AND  NOME NOT = SPACES
                         MOVE SPACES TO CWSEND-MSG
                         PERFORM VARYING I FROM LENGTH NOME
                                     BY -1
                                  UNTIL NOME (I: 1) NOT = SPACE
                                 CONTINUE
                         END-PERFORM
                         STRING 'Usu rio "' DELIMITED BY SIZE
                               NOME (1: I) DELIMITED BY SIZE
                         '" nÆo autorizado.' DELIMITED BY SIZE
                                       INTO CWSEND-MSG
                    END-IF
                    CALL "CWSEND" USING PARAMETROS-CWSEND
               ELSE
                    IF NIVEL-LOGON > CWCONF-NIVEL-PS
                      MOVE 'Privil‚gio insuficiente para o item de menu'
                        TO CWSEND-MSG
                       CALL "CWSEND" USING PARAMETROS-CWSEND
                       MOVE SPACES TO CWLOGO-USER
                                      CWLOGO-PASSWORD
                       GO TO LOGIN
                    END-IF
                    IF   CWCONF-SENHA = SPACES
                         CALL "CWPASS" USING NOME
                         CANCEL "CWPASS"
                         SET CWSQLC-READ TO TRUE
                         SET CWSQLC-EQUAL TO TRUE
                         CALL "CWCONF" USING CWSQLC
                                             CWCONF-REG
                                             FS-CWCONF
                                             KCO PCO
                         SET SENHA-OK TO TRUE
                    END-IF
                    MOVE "S"          TO CHECK-NOME
                    MOVE CWCONF-GRUPO TO GRUPO
                    MOVE CWCONF-NIVEL-PS TO CHECK-NIVEL
               END-IF
           END-IF

           IF  CWGETL-RETRY > 2
           AND(CWCONF-LOGIN-ERRO NOT LESS CWGETL-RETRY)
               IF  CWCONF-NIVEL-PS = 9
               AND HOJE > CWCONF-LOGIN-LAST
                   MOVE 0 TO CWCONF-LOGIN-ERRO
               ELSE
                   EXEC COBOLware Send
                        Message
                        "Senha expirada por viola‡Æo de seguran‡a."
                   END-EXEC
                   MOVE 2 TO EXPIRADA
               END-IF
           END-IF

           IF  CWCONF-BLOQUEADO = 1
               EXEC COBOLware Send
                  Message "Usu rio bloqueado pelo administrador."
               END-EXEC
               MOVE "N" TO CHECK-NOME
           END-IF

           IF  NOT NOME-OK
           AND (RESPOSTA NOT = "N")
               GO TO LOGIN
           END-IF

           IF  NOME-OK
           AND (NOT SENHA-OK)
           AND (RESPOSTA NOT = "N")
               DISPLAY "FSSERVER" UPON ENVIRONMENT-NAME
               ACCEPT  FSSERVER   FROM ENVIRONMENT-VALUE
               IF  FSSERVER NOT = SPACES
                   SET CWSQLC-READ TO TRUE
                   SET CWSQLC-EQUAL TO TRUE
                   MOVE "PS" TO CWCONF-REG
                   MOVE NOME TO CWCONF-NOME
                   CALL "CWCONF" USING CWSQLC
                                       CWCONF-REG
                                       FS-CWCONF
                                       KCO PCO
                   IF  FS-CWCONF < "10"
                       MOVE CWCONF-SENHA   TO SENHA-AUTO
                       MOVE CWCONF-ESQUECI TO ESQUECI
                       CALL "CWFSPW" USING CWCONF-CHAVE
                                           CWCONF-SENHA
                                           CWCONF-SIZE-PS
                                           CWCONF-FATOR-PS
                                           CWCONF-ESQUECI-SIZE
                                           CWCONF-ESQUECI-FATOR
                                           CWCONF-ESQUECI
                       IF  (CWCONF-SENHA   NOT = SENHA-AUTO)
                       OR  (CWCONF-ESQUECI NOT = ESQUECI)
                            SET CWSQLC-REWRITE TO TRUE
                            CALL "CWCONF" USING CWSQLC
                                                CWCONF-REG
                                                FS-CWCONF
                                                KCO PCO
                       END-IF
                   END-IF
               END-IF
               MOVE CWCONF-SENHA TO SENHA-AUTO
               CALL "CWCODE" USING "D" CWCONF-SIZE-PS
                                       CWCONF-FATOR-PS
                                       SENHA-AUTO
               INSPECT SENHA-AUTO CONVERTING MINUSCULAS
                                          TO MAIUSCULAS
               INSPECT SENHA CONVERTING MINUSCULAS TO MAIUSCULAS
               IF   CWCONF-PRINTER-DEFAULT = SPACES
               OR   LOW-VALUES
                    MOVE "Spool" TO CWCONF-PRINTER-DEFAULT
               END-IF
               SET CWTIME-NORMAL       TO TRUE
               SET CWTIME-TODAY        TO TRUE
               CALL "CWTIME"        USING PARAMETROS-CWTIME
               MOVE 0 TO SENHA-PROV
               PERFORM VARYING I FROM 1 BY 1 UNTIL I > 30
                       IF   SENHA-X (I) NOT = 32
                            ADD SENHA-X (I) TO SENHA-PROV
                       END-IF
               END-PERFORM
               MOVE CWTIME-DATE-FINAL (8: 2) TO N
               ADD  N                   TO SENHA-PROV
               MOVE CWTIME-DATE-FINAL (1: 2) TO N
               ADD  N                   TO SENHA-PROV
               MOVE CWTIME-TIME-FINAL (1: 2) TO N
               ADD  N                   TO SENHA-PROV
               MOVE CWTIME-DATE-FINAL (3: 2) TO N
               ADD  N                   TO SENHA-PROV
               COMPUTE SENHA-PROV = (1 / SENHA-PROV) * 100000000
               IF   SENHA = SENHA-AUTO
               OR   SENHA (1: 6) = SENHA-PROV
                    IF NOME = NOME-A
                       GO TO END-LOGIN
                    END-IF
                    IF   SENHA (1: 6) = SENHA-PROV
                         CALL "CWGETU" USING NOME TASK PROGRAMA "$"
                    ELSE
                         CALL "CWGETU" USING NOME TASK PROGRAMA "%"
                    END-IF
      *             MOVE 0 TO CWCONF-LOGIN-ERRO
                    SET CWSQLC-REWRITE TO TRUE
                    CALL "CWCONF" USING CWSQLC
                                        CWCONF-REG
                                        FS-CWCONF
                                        KCO PCO
                    CALL "CWLOGW" USING "#" OBS
                    CANCEL 'CWLOGW'
                    CALL "CWGETU" USING NOME TASK PROGRAMA "3"
                    MOVE 'Novo usu rio' TO OBS
                    CALL "CWLOGW" USING "#" OBS
                    MOVE 1                      TO LOGIN-FLAG
                    MOVE CWCONF-PRINTER-DEFAULT TO IMPRESSORA
                    MOVE "S"                    TO CHECK-SENHA
                    MOVE CWCONF-NIVEL-PS        TO CHECK-NIVEL
               ELSE
                    IF   CWCONF-LOGIN-ERRO < 3
                    AND (SENHA NOT = SPACES)
                         ADD 1 TO CWCONF-LOGIN-ERRO
                         SET CWSQLC-REWRITE TO TRUE
                         CALL "CWCONF" USING CWSQLC
                                             CWCONF-REG
                                             FS-CWCONF
                                             KCO PCO
                    END-IF
                    IF   SENHA = SPACES
                         MOVE "Informe sua senha." TO CWSEND-MSG
                         IF   CWCONF-ESQUECI NOT = SPACES
                              MOVE "~OK"       TO CWSEND-SCREEN(1)
                              MOVE "~Esqueci"  TO CWSEND-SCREEN(2)
                         END-IF
                    ELSE
                         IF CWGETL-RETRY < 3
                            MOVE 0 TO CWCONF-LOGIN-ERRO
                         END-IF
                         PERFORM VARYING I FROM LENGTH NOME
                              BY -1 UNTIL I = 1
                                     OR (NOME (I:1) <> ' ')
                                 CONTINUE
                         END-PERFORM
                         MOVE SPACES TO OBS
                         STRING '"' DELIMITED BY SIZE
                                NOME(1:I) DELIMITED BY SIZE
                               '", com senha incorreta.'
                               DELIMITED BY SIZE
                           INTO OBS
                         CALL "CWLOGW" USING "#" OBS
                         EVALUATE CWCONF-LOGIN-ERRO
                             WHEN 1 THRU (CWGETL-RETRY - 2)
                                  MOVE
               "Senha incorreta, pode ser bloqueada se insistir."
                               TO CWSEND-MSG
                             WHEN (CWGETL-RETRY - 1)  MOVE
                    "Senha incorreta, ser  bloqueada no pr¢ximo erro."
                               TO CWSEND-MSG
                             WHEN CWGETL-RETRY MOVE
                                   "Senha incorreta, bloqueada."
                                      TO CWSEND-MSG
                                  STRING '"' DELIMITED BY SIZE
                                         NOME(1:I) DELIMITED BY SIZE
                                        '", bloqueado pelo sistema'
                                        DELIMITED BY SIZE
                                    INTO OBS
                                  CALL "CWLOGW" USING "#" OBS
                             WHEN OTHER
                                  MOVE "Senha incorreta." TO CWSEND-MSG
                         END-EVALUATE
                    END-IF
                    CALL "CWSEND" USING PARAMETROS-CWSEND
                    MOVE SPACES TO SENHA cwlogo-password
                    MOVE SPACES TO CWSEND-SCREENS
                    IF   CWSEND-OPTION = 2
                         CALL "CWCODE" USING "D" CWCONF-ESQUECI-SIZE
                                                 CWCONF-ESQUECI-FATOR
                                                 CWCONF-ESQUECI
                         PERFORM VARYING LEN-LEMBRETE FROM
                                 LENGTH OF CWCONF-RESPOSTA BY -1
                                 UNTIL CWCONF-RESPOSTA
                                 (LEN-LEMBRETE: 1) <> SPACE
                                  CONTINUE
                         END-PERFORM
                         EXEC COBOLware BoxDialog
                              LINE 11 COLUMN 22
                              HEADER "Lembrete da senha"
                              Caption(1) CWCONF-PERGUNTA
                              Size(1) LEN-LEMBRETE (Secure(1))
                             Data(1) ESQUECI-RESPOSTA;ESQUECI-RESPOSTA
                             Color 23
                         END-EXEC
                         INSPECT ESQUECI-RESPOSTA
                                 CONVERTING MINUSCULAS
                                         TO MAIUSCULAS
                         INSPECT ESQUECI-RESPOSTA
                                 CONVERTING ACENTOS-850
                                         TO ACENTOS-OFF
                         INSPECT CWCONF-RESPOSTA
                                 CONVERTING MINUSCULAS
                                         TO MAIUSCULAS
                         INSPECT CWCONF-RESPOSTA
                                 CONVERTING ACENTOS-850
                                         TO ACENTOS-OFF
                         IF   CWCONF-RESPOSTA = ESQUECI-RESPOSTA
                              CALL "CWCODE" USING "C"
                                                  CWCONF-ESQUECI-SIZE
                                                  CWCONF-ESQUECI-FATOR
                                                  CWCONF-ESQUECI
                              MOVE SPACES TO CWCONF-SENHA
                              SET CWSQLC-REWRITE TO TRUE
                              CALL "CWCONF" USING CWSQLC
                                                  CWCONF-REG
                                                  FS-CWCONF
                                                  KCO PCO
                              CALL "CWPASS" USING NOME
                              CANCEL "CWPASS"
                              SET CWSQLC-READ TO TRUE
                              SET CWSQLC-EQUAL TO TRUE
                              CALL "CWCONF" USING CWSQLC
                                                  CWCONF-REG
                                                  FS-CWCONF
                                                  KCO PCO
                              GO TO LOGIN
                         ELSE
                              EXEC COBOLware Send
                              Message
                              "Resposta errada contate o administrador"
                              END-EXEC
                              MOVE SPACES TO CWLOGO-USER
                                             CWLOGO-PASSWORD
                              GO TO LOGIN
                         END-IF
                    END-IF
               END-IF
           END-IF

           IF  NOT SENHA-OK
           AND (RESPOSTA NOT = "N")
               GO TO LOGIN
           END-IF

           IF   NOME-OK
           AND  SENHA-OK
                MOVE 0 TO CWCONF-LOGIN-ERRO
                IF  EXPIRADA = 2
                    MOVE 1 TO EXPIRADA
                ELSE
                    MOVE 0 TO EXPIRADA
                END-IF
                MOVE HOJE TO CWCONF-LOGIN-LAST
                IF   CWCONF-DATA-SENHA = 0
                OR   CWCONF-DATA-SENHA = LOW-VALUES
                OR  (CWCONF-DATA-SENHA NOT NUMERIC)
                     MOVE HOJE TO CWCONF-DATA-SENHA
                ELSE
                     IF  EXPIRE NOT = 0
                         EXEC COBOLware Time (Interval) (AAAAMMDD)
                              Date CWCONF-DATA-SENHA
                              Date-Final HOJE
                              DAYS-FINAL;DIAS-SENHA
                         END-EXEC
                         IF   DIAS-SENHA > EXPIRE
                              EXEC COBOLware Send
           Message "Senha expirada op‡äes bloqueadas, altere sua senha."
                              END-EXEC
                              MOVE SPACES TO CWCONF-SENHA
                              SET CWSQLC-REWRITE TO TRUE
                              CALL "CWCONF" USING CWSQLC
                                                  CWCONF-REG
                                                  FS-CWCONF
                                                  KCO PCO
      *                       MOVE 1 TO EXPIRADA
                              CALL "CWPASS" USING NOME
                              CANCEL "CWPASS"
                              SET CWSQLC-READ TO TRUE
                              SET CWSQLC-EQUAL TO TRUE
                              CALL "CWCONF" USING CWSQLC
                                                  CWCONF-REG
                                                  FS-CWCONF
                                                  KCO PCO
                         END-IF
                     END-IF
                END-IF
                SET CWSQLC-REWRITE TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           END-IF

           IF   ENTROU = 1
           AND  NOME-OK
           AND  SENHA-OK
                IF  (GRUPO NOT = SPACES)
                AND (GRUPO NOT = "Acesso sem restri‡”es")
                AND (GRUPO NOT = "Acesso sem restri‡äes")
                AND (GRUPO NOT = "Acesso sem restricoes")
                AND (GRUPO NOT = "Acesso irrestrito")
                     MOVE "GU"            TO CWGRPS-REG
                     MOVE GRUPO           TO CWGRPS-NOME-GRUPO
                     SET CWSQLC-READ TO TRUE
                     SET CWSQLC-EQUAL TO TRUE
                     SET CWSQLC-IGNORE-LOCK TO TRUE
                     CALL "CWCONF" USING CWSQLC
                                         CWGRPS-REG
                                         FS-CWGRPS
                                         KGR PGR
                     IF   FS-CWGRPS < "09"
                          MOVE CWGRPS-ADM TO ADM
                          MOVE GRUPO      TO CWGRPS-NOME-GRUPO
                          MOVE PROGRAMA   TO CWGRPS-PROG-GRUPO
                          SET CWSQLC-READ TO TRUE
                          SET CWSQLC-EQUAL TO TRUE
                          SET CWSQLC-IGNORE-LOCK TO TRUE
                          CALL "CWCONF" USING CWSQLC
                                              CWGRPS-REG
                                              FS-CWGRPS
                                              KGR PGR
                          IF  (FS-CWGRPS < "09"
                          AND  CWGRPS-ACESSO-GRUPO NOT = SPACE
                          AND  ADM                 NOT = "I")
                          OR  (FS-CWGRPS = "23"
                          AND  ADM                     = "I")
                               MOVE "M¢dulo nÆo autorizado"
                                 TO CWSEND-MSG
                               CALL "CWSEND" USING PARAMETROS-CWSEND
                               MOVE SPACES TO CWLOGO-USER
                                              CWLOGO-PASSWORD
                               GO TO LOGIN
                          END-IF
                     END-IF
                END-IF
                CALL "CWLOCK" USING "L" NOME TASK
                MOVE 0                  TO ENTROU
                MOVE "03"       TO CWCONF-REG03
                MOVE IMPRESSORA TO CWCONF-ARQUIVO
                SET CWSQLC-READ        TO TRUE
                SET CWSQLC-EQUAL       TO TRUE
                SET CWSQLC-IGNORE-LOCK TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
                MOVE CWCONF-ARQUIVO      TO LB-PRNTER
                IF   FS-CWCONF < "10"
                     MOVE "Impressora:" TO lb-prnter-3
                     MOVE IMPR          TO lb-prnter-2
                                           lb-prnter-3 (13: )
                ELSE
                     MOVE "ImpressÆo em spool" TO lb-prnter-3
                     MOVE "Spool"              TO LB-PRNTER-2
                END-IF
                IF CWUNIX-GUI
                   MOVE NOME              TO STATUS-NOME
                   MOVE STATUS-DEF        TO SP2-PD-MSG-TEXT
                   CALL SP2   USING SP2-SET-PANEL-DEF  SP2-PANEL-DEF
                   CALL SP2   USING SP2-DISPLAY-WINDOW SP2-NULL-PARM
                END-IF
           END-IF.

       END-LOGIN. EXIT.

       AJUSTA.

           MOVE SPACES TO CWNUMERO
           MOVE 0      TO NUMERO
           ACCEPT CWNUMERO FROM ENVIRONMENT-VALUE
           MOVE 18 TO II
           PERFORM VARYING I FROM LENGTH OF CWNUMERO BY -1
                   UNTIL I = 0
                   IF  CWNUMERO (I: 1) NUMERIC
                       MOVE CWNUMERO (I: 1)
                         TO   NUMERO (II: 1)
                       SUBTRACT 1 FROM II
                   END-IF
           END-PERFORM.

       FIM-AJUSTA. EXIT.

       250-AJUSTA-TTY-NOME.

           IF   CWUNIX-ON
                DISPLAY "TTY"      UPON ENVIRONMENT-NAME
                ACCEPT  TTY        FROM ENVIRONMENT-VALUE
                IF TTY EQUAL SPACES
                   DISPLAY "SSH_TTY" UPON ENVIRONMENT-NAME
                   ACCEPT  TTY       FROM ENVIRONMENT-VALUE
                END-IF
                IF   TTY (1: 5) = "/dev/"
                     MOVE TTY (6: ) TO TTY-NOME
                     MOVE TTY-NOME  TO TTY
                END-IF
           END-IF

           MOVE SPACES TO TTY-NOME

           IF  (FS-CWCONF NOT = "00")
           OR   NOME = SPACES
           OR   NOME = LOW-VALUES
                IF   TTY = SPACES
                     MOVE "LOGON" TO TTY-NOME
                ELSE
                     STRING "LOGON(" DELIMITED BY SIZE
                                TTY  DELIMITED BY SPACE
                                ")"  DELIMITED BY SIZE
                       INTO TTY-NOME
                END-IF
           ELSE
                IF   TTY = SPACES
                     MOVE NOME TO TTY-NOME
                ELSE
                     PERFORM VARYING I FROM LENGTH OF NOME BY -1
                                       UNTIL NOME (I: 1) NOT = SPACE
                             CONTINUE
                     END-PERFORM
                     STRING NOME (1: I) DELIMITED BY SIZE
                                   "("  DELIMITED BY SIZE
                                   TTY  DELIMITED BY SPACE
                                   ")"  DELIMITED BY SIZE
                       INTO TTY-NOME
                END-IF
           END-IF.

       250-99-FIM. EXIT.

       END PROGRAM CWLOGO.
