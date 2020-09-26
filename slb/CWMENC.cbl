       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWMENC.
       AUTHOR.        COBOLware Service Ltda.
       DATE-WRITTEN.  19/12/1994.
       SECURITY.      *************************************************
                      *                                               *
                      *  Registra prazo de validade do sistema        *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 MAXUSERS                 PIC  9(006) VALUE 0.
           05 SENHA                    PIC  9(006) VALUE 0.
           05 VALIDADE                 PIC  9(008) VALUE 0.
           05 DATA-OK                  PIC  9(001) VALUE 0.
           05 RESPOSTA                 PIC  X(001) VALUE SPACE.
           05 CURSOR-POSITION.
              10                       PIC  9(004) COMP-X VALUE 00.
              10                       PIC  9(004) COMP-X VALUE 00.

       COPY CWTIME.
       COPY CWSEND.
       COPY CWCONF.

       PROCEDURE DIVISION.

       INICIO.

           SET CWSQLC-UPDATE TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO

           MOVE "MX" TO CWCONF-REGLG
           SET CWSQLC-READ TO TRUE
           SET CWSQLC-EQUAL TO TRUE
           SET CWSQLC-IGNORE-LOCK TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO

           IF   FS-CWCONF < "09"
                MOVE CWCONF-MAXUSERS TO MAXUSERS
           END-IF

           MOVE "VD" TO CWCONF-REGLG
           SET CWSQLC-READ TO TRUE
           SET CWSQLC-EQUAL TO TRUE
           SET CWSQLC-IGNORE-LOCK TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO

           IF   FS-CWCONF < "09"
                IF  CWCONF-FLAG-2000 NOT NUMERIC
                    MOVE 1                   TO CWCONF-FLAG-2000
                    MOVE CWCONF-OLD-VALIDADE TO CWCONF-VALIDADE
                    ADD  1900                TO CWCONF-VALIDADE
                END-IF
                MOVE CWCONF-VALIDADE       TO VALIDADE
                MOVE CWCONF-SENHA-ATIVACAO TO SENHA
                IF   CWCONF-EXIBE-LICENCA = "S"
                     MOVE 1 TO CWSEND-OPTION
                ELSE
                     MOVE 2 TO CWSEND-OPTION
                END-IF
           END-IF

           SET  CWTIME-REVERSED   TO TRUE
           SET  CWTIME-REVERSE    TO TRUE
           MOVE VALIDADE          TO CWTIME-DATE
           CALL "CWTIME"       USING PARAMETROS-CWTIME
           MOVE CWTIME-DATE-FINAL TO VALIDADE

           PERFORM UNTIL DATA-OK = 1
                   EXEC COBOLware BoxDialog
                        LINE 11 COLUMN 22
                             HEADER "Validade do sistema"
                             Caption(1) "Data"
                             Caption(2) "Licen‡a"
                             Caption(3) "Usu rios"
                             Data(1) VALIDADE;VALIDADE
                             Data(2) SENHA;SENHA
                             Data(3) MAXUSERS;MAXUSERS
                             Pic(1) "99/99/9999"
                             Size(1) 8
                             Size(2) 6
                             Size(3) 6
                             Pic(3) "ZZZZZZ"
                             Numeric(2)
                             Numeric(3)
                             Cancel;Resposta
                   END-EXEC
                   MOVE 1 TO DATA-OK
                   IF   VALIDADE NOT = 0
                        MOVE VALIDADE TO CWTIME-DATE
                        SET CWTIME-NORMAL   TO TRUE
                        SET CWTIME-VALIDATE TO TRUE
                        CALL "CWTIME" USING PARAMETROS-CWTIME
                        IF   CWTIME-DATE-FINAL = 0
                             MOVE 0 TO VALIDADE
                                       DATA-OK
                        END-IF
                   END-IF
           END-PERFORM

           IF   RESPOSTA = "Y"
                SET CWSQLC-CLOSE TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
                GOBACK
           END-IF

           PERFORM TEST AFTER UNTIL FS-CWCONF NOT = "9D"
                   MOVE "MX" TO CWCONF-REGLG
                   SET CWSQLC-READ TO TRUE
                   SET CWSQLC-EQUAL TO TRUE
                   CALL "CWCONF" USING CWSQLC
                                       CWCONF-REG
                                       FS-CWCONF
                                       KCO PCO
                   IF   FS-CWCONF = "9D"
                        CALL "CWCONF" USING "ISAM"
                   ELSE
                        IF   FS-CWCONF < "10"
                             SET CWSQLC-DELETE TO TRUE
                             CALL "CWCONF" USING CWSQLC
                                                 CWCONF-REG
                                                 FS-CWCONF
                                                 KCO PCO
                        END-IF
                   END-IF
           END-PERFORM

           PERFORM TEST AFTER UNTIL FS-CWCONF NOT = "9D"
                   MOVE "VD" TO CWCONF-REGLG
                   SET CWSQLC-READ TO TRUE
                   SET CWSQLC-EQUAL TO TRUE
                   CALL "CWCONF" USING CWSQLC
                                       CWCONF-REG
                                       FS-CWCONF
                                       KCO PCO
                   IF   FS-CWCONF = "9D"
                        CALL "CWCONF" USING "ISAM"
                   ELSE
                        IF   FS-CWCONF < "10"
                             SET CWSQLC-DELETE TO TRUE
                             CALL "CWCONF" USING CWSQLC
                                                 CWCONF-REG
                                                 FS-CWCONF
                                                 KCO PCO
                        END-IF
                   END-IF
           END-PERFORM

           IF   MAXUSERS NOT = 0
                MOVE "MX"     TO CWCONF-REGLG
                MOVE MAXUSERS TO CWCONF-MAXUSERS
                SET CWSQLC-WRITE TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           END-IF

           IF   VALIDADE NOT = 0
                MOVE "VD"              TO CWCONF-REGLG
                SET  CWTIME-NORMAL     TO TRUE
                SET  CWTIME-REVERSE    TO TRUE
                MOVE VALIDADE          TO CWTIME-DATE
                CALL "CWTIME"       USING PARAMETROS-CWTIME
                MOVE CWTIME-DATE-FINAL TO CWCONF-VALIDADE
                MOVE "N"               TO CWCONF-TRAVADO
                MOVE SENHA             TO CWCONF-SENHA-ATIVACAO
                MOVE 1                 TO CWCONF-FLAG-2000
                SET  CWTIME-REVERSED   TO TRUE
                SET  CWTIME-TODAY      TO TRUE
                CALL "CWTIME"       USING PARAMETROS-CWTIME
                MOVE CWTIME-DATE-FINAL TO CWCONF-ULTIMO-LOGIN-DATA
                MOVE CWTIME-TIME-FINAL TO CWCONF-ULTIMO-LOGIN-HORA
                MOVE 99999999          TO CWCONF-VALIDADE-10
                                          CWCONF-VALIDADE-15
                                          CWCONF-VALIDADE-20
                MOVE "Exibir licen‡a na reativa‡Æo ?" TO CWSEND-MSG
                MOVE SPACES   TO CWSEND-SCREENS
                MOVE "_~Sim_" TO CWSEND-SCREEN (1)
                MOVE "_~NÆo_" TO CWSEND-SCREEN (2)
                CALL "CWSEND" USING PARAMETROS-CWSEND
                IF   CWSEND-OPTION = 1
                     MOVE "S" TO CWCONF-EXIBE-LICENCA
                ELSE
                     MOVE "N" TO CWCONF-EXIBE-LICENCA
                END-IF
                MOVE
             "Liberar viola‡Æo (Rel¢gio/Calend rio) ap¢s 3 tentativas ?"
                  TO CWSEND-MSG
                CALL "CWSEND" USING PARAMETROS-CWSEND
                IF   CWSEND-OPTION = 1
                     MOVE "S" TO CWCONF-DESTRAVA
                ELSE
                     MOVE "N" TO CWCONF-DESTRAVA
                END-IF
                MOVE 0 TO CWCONF-TENTATIVAS
                SET CWSQLC-WRITE TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           END-IF

           SET CWSQLC-CLOSE TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO.

       FIM. GOBACK.

       END PROGRAM CWMENC.
