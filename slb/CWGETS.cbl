       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWGETS.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  23/04/2001.
       SECURITY.      *************************************************
                      *                                               *
                      *  Retorna ambiente do CWMENU                   *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 NIVEL                    PIC  9(001) VALUE 0.
           05 ADM                      PIC  X(001) VALUE SPACE.
           05 TIPO-REG                 PIC  X(002) VALUE SPACE.

       COPY CWCONF.

       LINKAGE SECTION.

       COPY CWGETS.

       PROCEDURE DIVISION USING PARAMETROS-CWGETS.

       000-INICIO.

           CALL "CWGETU" USING CWGETS-USUARIO
                               CWGETS-TASK
                               CWGETS-PROGRAMA
                               "?"
           SET CWSQLC-OPEN TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           IF   FS-CWCONF > "09"
                CALL "CWCONF" USING "ISAM"
           END-IF
           MOVE "00"   TO CWCONF-TIPO
           MOVE SPACES TO CWCONF-ELEMENTO
           SET CWSQLC-READ TO TRUE
           SET CWSQLC-IGNORE-LOCK TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           IF   FS-CWCONF < "10"
                CALL "CWCODE" USING "D" CWCONF-SIZE-U
                                        CWCONF-FATOR-00-U
                                        CWCONF-USUARIO
                CALL "CWCODE" USING "D" CWCONF-SIZE-UP
                                        CWCONF-FATOR-00-UP
                                        CWCONF-USUARIO-P
                CALL "CWCODE" USING "D" CWCONF-SIZE-S
                                        CWCONF-FATOR-00-S
                                        CWCONF-SISTEMA
                CALL "CWCODE" USING "D" CWCONF-SIZE-SP
                                        CWCONF-FATOR-00-SP
                                        CWCONF-SISTEMA-P
                MOVE CWCONF-USUARIO   TO CWGETS-EMPRESA-TELA
                MOVE CWCONF-USUARIO-P TO CWGETS-EMPRESA-RELATORIOS
                MOVE CWCONF-SISTEMA   TO CWGETS-SISTEMA-TELA
                MOVE CWCONF-SISTEMA-P TO CWGETS-SISTEMA-RELATORIOS
           ELSE
                CALL "CWCONF" USING "ISAM"
           END-IF

           MOVE "PS"           TO CWCONF-REG
           MOVE CWGETS-USUARIO TO CWCONF-NOME
           MOVE SPACES         TO CWGETS-ACESSO
                                  CWGETS-ALTERACAO
                                  CWGETS-CONSULTA
                                  CWGETS-EXCLUSAO
                                  CWGETS-INCLUSAO
                                  CWGETS-GRUPO

           SET CWSQLC-READ TO TRUE
           SET CWSQLC-IGNORE-LOCK TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           IF   FS-CWCONF < "10"
                MOVE CWCONF-SENHA TO CWGETS-SENHA
                CALL "CWCODE" USING "D" CWCONF-SIZE-PS
                                        CWCONF-FATOR-PS
                                        CWGETS-SENHA
                MOVE CWCONF-GRUPO    TO CWGETS-GRUPO
                MOVE CWCONF-NIVEL-PS TO NIVEL
                MOVE NIVEL           TO CWGETS-NIVEL
                IF (CWCONF-SMTP-PASSWORD NOT = SPACES)
                AND(CWCONF-SMTP-PASSWORD (1: 1) NOT = X"00")
                AND (CWCONF-SMTP-SIZE NOT = 32)
                    CALL "CWCODE" USING "D"
                                  CWCONF-SMTP-SIZE
                                  CWCONF-SMTP-FATOR
                                  CWCONF-SMTP-PASSWORD
                END-IF
                MOVE CWCONF-SMTP          TO CWGETS-SMTP
                MOVE CWCONF-E-MAIL        TO CWGETS-E-MAIL
                MOVE CWCONF-SMTP-PASSWORD TO CWGETS-SMTP-PASSWORD
                MOVE CWCONF-SSL           TO CWGETS-SSL
                MOVE CWCONF-SMTP-PORT     TO CWGETS-SMTP-PORT
                MOVE CWCONF-AUTENTICACAO  TO CWGETS-AUTHENTICATION
                DISPLAY 'CWLOGN'      UPON ENVIRONMENT-NAME
                DISPLAY  CWCONF-NOME  UPON ENVIRONMENT-VALUE
                DISPLAY 'CWLOGG'      UPON ENVIRONMENT-NAME
                DISPLAY  CWCONF-GRUPO UPON ENVIRONMENT-VALUE
                PERFORM 010-LER-GRUPO THRU 010-99-FIM
                IF  CWGETS-ACESSO NOT = "*"
                    MOVE NIVEL TO CWGETS-ACESSO
                END-IF
           ELSE
                MOVE SPACES          TO CWGETS-SENHA
                                        CWGETS-GRUPO
           END-IF

           MOVE "L5" TO CWCONF-REG
           SET CWSQLC-READ TO TRUE
           SET CWSQLC-IGNORE-LOCK TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           IF   FS-CWCONF < "10"
                MOVE CWCONF-LICENCIADO TO CWGETS-LICENCIADO
           ELSE
                MOVE SPACES            TO CWGETS-LICENCIADO
           END-IF
           SET CWSQLC-CLOSE TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           CALL "CWMODE" USING "C" CWGETS-PAGINA CWGETS-MODULO TIPO-REG.

       000-99-FIM. GOBACK.

       010-LER-GRUPO.

           IF  (CWGETS-GRUPO NOT = SPACES)
           AND (CWGETS-GRUPO NOT = "Acesso sem restri‡”es")
           AND (CWGETS-GRUPO NOT = "Acesso sem restri‡äes")
           AND (CWGETS-GRUPO NOT = "Acesso sem restricoes")
           AND (CWGETS-GRUPO NOT = "Acesso irrestrito")
                MOVE "GU"            TO CWCONF-REG
                MOVE CWGETS-GRUPO    TO CWCONF-NOME-GRUPO
                SET CWSQLC-READ TO TRUE
                SET CWSQLC-IGNORE-LOCK TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
                IF   FS-CWCONF < "09"
                     MOVE CWCONF-ADM      TO ADM
                     MOVE CWGETS-GRUPO    TO CWCONF-NOME-GRUPO
                     MOVE CWGETS-PROGRAMA TO CWCONF-PROG-GRUPO
                     SET CWSQLC-READ TO TRUE
                     SET CWSQLC-IGNORE-LOCK TO TRUE
                     CALL "CWCONF" USING CWSQLC CWCONF-REG
                                         FS-CWCONF KCO PCO
                     IF  (FS-CWCONF < "09"
                     AND  CWCONF-ACESSO-GRUPO NOT = SPACE
                     AND  ADM                 NOT = "I")
                     OR  (FS-CWCONF = "23"
                     AND  ADM                     = "I")
                          MOVE "*" TO CWGETS-ACESSO
                     END-IF
                     IF   FS-CWCONF < "09"
                          IF   CWCONF-ALTERACAO-GRUPO NOT = SPACE
                               MOVE "*" TO CWGETS-ALTERACAO
                          END-IF
                          IF   CWCONF-CONSULTA-GRUPO  NOT = SPACE
                               MOVE "*" TO CWGETS-CONSULTA
                          END-IF
                          IF   CWCONF-EXCLUSAO-GRUPO  NOT = SPACE
                               MOVE "*" TO CWGETS-EXCLUSAO
                          END-IF
                          IF   CWCONF-INCLUSAO-GRUPO  NOT = SPACE
                               MOVE "*" TO CWGETS-INCLUSAO
                          END-IF
                     END-IF
                END-IF
           END-IF.

       010-99-FIM. EXIT.

       END PROGRAM CWGETS.
