       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWPASS INITIAL.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  29/05/2005.
       SECURITY.      *************************************************
                      *                                               *
                      *  Definicao de senha                           *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO. COPY CWCASE.
           05 REMINDER                 PIC  X(003) VALUE SPACES.
           05 SAVE-RESPOSTA            PIC  X(030) VALUE SPACES.
           05 FSSERVER                 PIC  X(050) VALUE SPACES.
           05 X91-RESULT        COMP-X PIC  9(002) VALUE 0.
           05 X91-FUNCTION      COMP-X PIC  9(002) VALUE 16.
           05 X91-PARAMETER     COMP-X PIC  9(002) VALUE 0.
           05 SENHA-VELHA              PIC  X(030) VALUE SPACE.
           05 CANCELOU                 PIC  X(001) VALUE SPACE.
           05 LEN-LEMBRETE             PIC  9(002) VALUE 0.
           05 ERRO-SENHA               PIC  9(001) VALUE ZERO.
           05 DADOS                                VALUE SPACES.
              06 CRITICA-SENHA.
                 10 SENHA-AUTO         PIC  X(030).
                 10 SENHA-AUTO2        PIC  X(030).
              06 ESQUECI.
                 10 ESQUECI-PERGUNTA   PIC  X(030).
                 10 ESQUECI-RESPOSTA   PIC  X(030).
                 10 ESQUECI-RESPOSTA2  PIC  X(030).
                 10 ESQUECI-RESPOSTA3  PIC  X(030).

       COPY CWCONF.

       LINKAGE SECTION.

       01  USUARIO PIC X(030).
       01  MODO    PIC X(001).

       PROCEDURE DIVISION USING USUARIO MODO.

       000-INICIO.

           DISPLAY 'CWREMINDER' UPON ENVIRONMENT-NAME
           ACCEPT     REMINDER  FROM ENVIRONMENT-VALUE
           INSPECT    REMINDER  CONVERTING 'onf' TO 'ONF'
           CALL X"91" USING X91-RESULT X91-FUNCTION X91-PARAMETER
           SET CWSQLC-UPDATE TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           IF   FS-CWCONF NOT = "00"
                CALL "CWCONF" USING "ISAM"
                GOBACK
           END-IF
           MOVE "PS" TO CWCONF-REGPS
           MOVE USUARIO TO CWCONF-NOME
           PERFORM TEST AFTER UNTIL FS-CWCONF NOT = "9D"
                   SET CWSQLC-READ  TO TRUE
                   SET CWSQLC-EQUAL TO TRUE
                   SET CWSQLC-LOCK  TO TRUE
                   CALL "CWCONF" USING CWSQLC
                                       CWCONF-REG
                                       FS-CWCONF
                                       KCO PCO
                   IF   FS-CWCONF = "9D"
                        CALL "CWCONF" USING "ISAM"
                   END-IF
           END-PERFORM
           IF   FS-CWCONF NOT = "00"
                SET CWSQLC-CLOSE TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
                GOBACK
           END-IF
           IF   CWCONF-ESQUECI NOT = SPACES
                CALL "CWCODE" USING "D" CWCONF-ESQUECI-SIZE
                                        CWCONF-ESQUECI-FATOR
                                        CWCONF-ESQUECI
                MOVE CWCONF-ESQUECI TO ESQUECI
           END-IF

           IF   CWCONF-SENHA NOT = SPACES
                CALL "CWCODE" USING "D" CWCONF-SIZE-PS
                                        CWCONF-FATOR-PS
                                        CWCONF-SENHA
           END-IF
           MOVE 0          TO ERRO-SENHA
           PERFORM TEST AFTER UNTIL (SENHA-AUTO NOT = SPACES)
                     AND ERRO-SENHA = 0
                IF  CWCONF-SENHA = SPACES
                    IF REMINDER = 'OFF'
                       EXEC COBOLware BoxDialog
                            LINE 11 COLUMN 21
                            HEADER "Defini‡Æo de senha pessoal"
                            Caption(1) "Senha"
                            Caption(2) "Confirme senha"
                            (Secure(1)) (Secure(2))
                            Data(1) SENHA-AUTO;SENHA-AUTO   Size(1) 30
                            Data(2) SENHA-AUTO2;SENHA-AUTO2 Size(2) 30
                            CANCEL;CANCELOU
                       END-EXEC
                       MOVE SPACE TO ESQUECI-PERGUNTA
                                     ESQUECI-RESPOSTA
                    ELSE
                       EXEC COBOLware BoxDialog
                            LINE 11 COLUMN 21
                            HEADER "Defini‡Æo de senha pessoal"
                            Caption(1) "Senha"
                            Caption(2) "Confirme senha"
                            Caption(3) "Lembrete"
                            Caption(4) "Resposta"
                            (Secure(1)) (Secure(2))
                            Data(1) SENHA-AUTO;SENHA-AUTO   Size(1) 30
                            Data(2) SENHA-AUTO2;SENHA-AUTO2 Size(2) 30
                            Data(3) ESQUECI-PERGUNTA;ESQUECI-PERGUNTA
                                    Size(3) 30
                            Data(4) SPACES;ESQUECI-RESPOSTA
                                    Size(4) 30 (Secure(4))
                            CANCEL;CANCELOU
                       END-EXEC
                    END-IF
                ELSE
                    MOVE ESQUECI-RESPOSTA TO SAVE-RESPOSTA
                    IF REMINDER = 'OFF'
                       EXEC COBOLware BoxDialog
                         LINE 11 COLUMN 21
                         HEADER "Defini‡Æo de senha pessoal"
                         Caption(1) "Senha atual"
                         Caption(2) "Nova Senha"
                         Caption(3) "Confirme senha"
                         (Secure(2)) (Secure(3))
                         Data(1) SPACES;SENHA-VELHA      Size(1) 30
                                 Secure(1)
                         Data(2) SENHA-AUTO;SENHA-AUTO   Size(2) 30
                         Data(3) SENHA-AUTO2;SENHA-AUTO2 Size(3) 30
                         CANCEL;CANCELOU
                       END-EXEC
                       MOVE SPACE TO ESQUECI-PERGUNTA
                                     ESQUECI-RESPOSTA
                    ELSE
                       EXEC COBOLware BoxDialog
                         LINE 11 COLUMN 21
                         HEADER "Defini‡Æo de senha pessoal"
                         Caption(1) "Senha atual"
                         Caption(2) "Nova Senha"
                         Caption(3) "Confirme senha"
                         Caption(4) "Lembrete"
                         Caption(5) "Resposta"
                         (Secure(2)) (Secure(3))
                         Data(1) SPACES;SENHA-VELHA      Size(1) 30
                                 Secure(1)
                         Data(2) SENHA-AUTO;SENHA-AUTO   Size(2) 30
                         Data(3) SENHA-AUTO2;SENHA-AUTO2 Size(3) 30
                         Data(4) ESQUECI-PERGUNTA;ESQUECI-PERGUNTA
                                 Size(4) 30
                         Data(5) SPACES;ESQUECI-RESPOSTA
                                 Size(5) 30 (Secure(5))
                         CANCEL;CANCELOU
                       END-EXEC
                    END-IF
                    IF ESQUECI-RESPOSTA = SPACES
                       MOVE SAVE-RESPOSTA TO ESQUECI-RESPOSTA
                    END-IF
                END-IF
                IF   CWCONF-SENHA NOT = SPACES
                     INSPECT CWCONF-SENHA CONVERTING MINUSCULAS
                                                  TO MAIUSCULAS
                     INSPECT SENHA-VELHA  CONVERTING MINUSCULAS
                                                  TO MAIUSCULAS
                     IF   CWCONF-SENHA NOT = SENHA-VELHA
                           EXEC COBOLware Send
                            Message "Senha atual inv lida"
                           END-EXEC
                          SET CWSQLC-CLOSE TO TRUE
                          CALL "CWCONF" USING CWSQLC
                                              CWCONF-REG
                                              FS-CWCONF
                                              KCO PCO
                          GOBACK
                     END-IF
                END-IF
                IF   CANCELOU = "Y"
                AND  X91-PARAMETER > 1
                AND (MODO = "M" OR "m")
                     SET CWSQLC-CLOSE TO TRUE
                     CALL "CWCONF" USING CWSQLC
                                         CWCONF-REG
                                         FS-CWCONF
                                         KCO PCO
                     GOBACK
                END-IF
                IF   DADOS = SPACES
                     SET CWSQLC-CLOSE TO TRUE
                     CALL "CWCONF" USING CWSQLC
                                         CWCONF-REG
                                         FS-CWCONF
                                         KCO PCO
                     IF   X91-PARAMETER > 1
                     AND (MODO = "M" OR "m")
                         GOBACK
                     ELSE
                         STOP RUN
                     END-IF
                END-IF
                MOVE 0 TO ERRO-SENHA
                DISPLAY "CWMENK" UPON ENVIRONMENT-NAME
                DISPLAY USUARIO  UPON ENVIRONMENT-VALUE
                CALL "CWMENK" USING ERRO-SENHA CRITICA-SENHA
                CANCEL "CWMENK"
                IF   ERRO-SENHA = 0
                     IF (REMINDER NOT = 'OFF')
                     IF   ESQUECI-RESPOSTA = SPACES
                     OR   ESQUECI-PERGUNTA = SPACES
                          MOVE 2 TO ERRO-SENHA
                          EXEC COBOLware Send
                               Message
           "Defina lembrete e resposta para o caso de esquecer a senha"
                          END-EXEC
                     ELSE
                          MOVE SPACES TO ESQUECI-RESPOSTA2
                          MOVE ESQUECI-RESPOSTA TO ESQUECI-RESPOSTA3
                          INSPECT ESQUECI-RESPOSTA3
                                  CONVERTING MINUSCULAS
                                          TO MAIUSCULAS
                          INSPECT ESQUECI-RESPOSTA3
                                  CONVERTING ACENTOS-850
                                          TO ACENTOS-OFF
                          PERFORM VARYING LEN-LEMBRETE FROM
                                  LENGTH OF ESQUECI-RESPOSTA
                                  BY -1
                                  UNTIL ESQUECI-RESPOSTA
                                  (LEN-LEMBRETE: 1) <> SPACE
                                  CONTINUE
                          END-PERFORM
                          EXEC COBOLware BoxDialog
                               LINE 11 COLUMN 22
                               HEADER "Teste de lembrete da senha"
                               Caption(1) ESQUECI-PERGUNTA
                               Size(1) LEN-LEMBRETE (Secure(1))
                               color 23
                         Data(1) ESQUECI-RESPOSTA2;ESQUECI-RESPOSTA2
                          END-EXEC
                          INSPECT ESQUECI-RESPOSTA2
                                  CONVERTING MINUSCULAS
                                          TO MAIUSCULAS
                          INSPECT ESQUECI-RESPOSTA2
                                  CONVERTING ACENTOS-850
                                          TO ACENTOS-OFF
                        IF ESQUECI-RESPOSTA2 NOT = ESQUECI-RESPOSTA3
                           EXEC COBOLware Send
                            Message "Resposta do lembrete incorreta"
                           END-EXEC
                           MOVE 3 TO ERRO-SENHA
                        END-IF
                     END-IF
                     END-IF
                ELSE
                     MOVE SPACES TO CRITICA-SENHA
                END-IF
           END-PERFORM
           MOVE SENHA-AUTO TO CWCONF-SENHA
           MOVE LENGTH OF CWCONF-SENHA TO CWCONF-SIZE-PS
           CALL "CWCODE" USING "C" CWCONF-SIZE-PS
                                   CWCONF-FATOR-PS
                                   CWCONF-SENHA
           MOVE ESQUECI TO CWCONF-ESQUECI
           MOVE LENGTH OF CWCONF-ESQUECI TO CWCONF-ESQUECI-SIZE
           CALL "CWCODE" USING "C" CWCONF-ESQUECI-SIZE
                                   CWCONF-ESQUECI-FATOR
                                   CWCONF-ESQUECI
           EXEC COBOLware Time
                DATE-FINAL;CWCONF-DATA-SENHA
           END-EXEC
           MOVE 0 TO CWCONF-LOGIN-ERRO
ks         SET CWSQLC-DELETE  TO TRUE
ks         CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
ks         SET CWSQLC-WRITE   TO TRUE
ks         CALL "CWCONF"   USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           SET CWSQLC-CLOSE   TO TRUE
           CALL "CWCONF"   USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO

           DISPLAY "FSSERVER"  UPON ENVIRONMENT-NAME
           ACCEPT  FSSERVER    FROM ENVIRONMENT-VALUE
           IF  FSSERVER NOT = SPACES
               INSPECT CWCONF-CHAVE CONVERTING MAIUSCULAS TO MINUSCULAS
               CALL "CWFSPW" USING CWCONF-CHAVE
                                   CWCONF-SENHA
                                   CWCONF-SIZE-PS
                                   CWCONF-FATOR-PS
                                   CWCONF-ESQUECI-SIZE
                                   CWCONF-ESQUECI-FATOR
                                   CWCONF-ESQUECI
           END-IF

           CALL "CWCODE" USING "D" CWCONF-SIZE-PS
                                   CWCONF-FATOR-PS
                                   CWCONF-SENHA
           CALL "CWFSPW" USING CWCONF-CHAVE
                               CWCONF-SENHA
                               CWCONF-SIZE-PS
                               CWCONF-FATOR-PS
                               "W".

       000-99-FIM. GOBACK.

       END PROGRAM CWPASS.
