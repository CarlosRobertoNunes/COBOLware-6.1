       IDENTIFICATION DIVISION.
       PROGRAM-ID.    LICENCIA.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  26/09/2004.
       SECURITY.      *************************************************
                      *                                               *
                      *  Licenciamento do COBOLware 5.0               *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT LICENCAS ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS LICENCAS-CHAVE
                  ALTERNATE RECORD KEY IS LICENCAS-LICENCIADO
                                          WITH DUPLICATES
                  LOCK MODE     IS MANUAL
                  FILE STATUS   IS FS-LICENCAS.

       DATA DIVISION.
       FILE SECTION.

       FD  LICENCAS
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-LICENCAS.

       01  LICENCAS-REG.
           05 LICENCAS-CHAVE.
              10 LICENCAS-CODIGO   COMP-3 PIC  9(004).
           05 LICENCAS-LICENCIADO         PIC  X(040).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1. COPY CWCASE.
           05 LICENCIADO.
              10   OCCURS 40       PIC  X(001).
                 88 BYTE-OK VALUE "A" THRU "Z" "0" THRU "9".
           05 LICENCIADO2          PIC  X(040) VALUE SPACES.
           05 REDEFINES LICENCIADO2.
              10 U OCCURS 40       PIC  9(002) COMP-X.
           05 M                    PIC  9(004) VALUE 0.
           05 LICENCA              PIC  9(009) VALUE 0.
           05 REDEFINES LICENCA.
              10 LICENCA-SERIECW   PIC  9(004).
              10 LICENCA-KEY       PIC  9(005).
           05 I                    PIC  9(004) VALUE 0.
           05 I2                   PIC  9(004) VALUE 0.
           05 SOMA                 PIC  9(006) VALUE 0.
           05 POSICIONA                PIC  9(001) VALUE ZERO.
           05 OBS                      PIC  X(035) VALUE SPACES.
           05 ERRO                     PIC  9(001) VALUE ZERO.
           05 CHAVE                    PIC  9(002) VALUE ZERO.
           05 CHAVE-ANTERIOR           PIC  9(002) VALUE ZERO.
           05 CAMPO                    PIC  9(002) VALUE ZERO.
           05 SALVA-REG                PIC X(4096) VALUE ZERO.
           05 SALVA-CHAVE              PIC  X(255) VALUE SPACES.
           05 TECLA                    PIC  9(002) VALUE ZERO.
              COPY CWKEYS.
           05 COMANDO                  PIC  X(001) VALUE SPACE.
              88 COMANDO-OK                        VALUE "A" "a"
                                                         "E" "e".
              88 ABORTAR                           VALUE "C".
              88 EFETIVAR                          VALUE "O".
           05 FUNCAO-ANTERIOR          PIC  X(001) VALUE SPACE.
           05 FUNCAO                   PIC  X(001) VALUE SPACE.
              88 ALTERACAO                         VALUE "A".
              88 EXCLUSAO                          VALUE "E".
              88 INCLUSAO                          VALUE "I".
              88 FINALIZAR                         VALUE "F" "V".
              88 PARAR                             VALUE "F".
              88 CONSULTA                          VALUE "C".
           05 MENSAGENS-DE-ACESSO.
              10 PIC X(30) VALUE "Licenciado nÆo cadastrado".
              10 PIC X(30) VALUE "Licenciado j  cadastrado".
              10 PIC X(30) VALUE "Informe c¢digo do licenciado".
           05 REDEFINES MENSAGENS-DE-ACESSO.
              10 MSG-A OCCURS 3 PIC X(30).
           05 CAMPOS              PIC 9(002) VALUE 1.
           05 ER-LICENCAS.
              10 FS-LICENCAS      PIC  X(002) VALUE "00".
              10 LB-LICENCAS      PIC  X(050) VALUE "LICENCAS".

       COPY CWHELP.
       COPY CWBOXS.
       COPY CWSEND.
       SCREEN SECTION.

       01  CTAC-LIT-CWCADS.
           05 LINE 08 COLUMN 03 VALUE "C¢digo    :".
           05 LINE 10 COLUMN 03 VALUE "Licenciado:".
           05 LINE 12 COLUMN 03 VALUE "Key       :".

       01  CTAC-VAR-CWCADS.
           05 T00 LINE 08 COLUMN 15 PIC Z(004) USING LICENCAS-CODIGO.
           05 T01 LINE 10 COLUMN 15 PIC X(040)
                             USING LICENCAS-LICENCIADO.
           05 T02 LINE 12 COLUMN 15 PIC 9(009) FROM LICENCA
           BLANK ZERO.

       PROCEDURE DIVISION.

       000-INICIO.

           CALL "CWFILE" USING LB-LICENCAS

           OPEN INPUT LICENCAS

           IF   FS-LICENCAS > "29"
           AND  FS-LICENCAS < "39"
                OPEN OUTPUT LICENCAS
                CLOSE LICENCAS
           ELSE
                IF   FS-LICENCAS > "09"
                     CALL "CWISAM" USING ER-LICENCAS
                     GOBACK
                ELSE
                     CLOSE LICENCAS
                END-IF
           END-IF

           OPEN I-O LICENCAS

           IF   FS-LICENCAS > "09"
                GOBACK
           END-IF

           INITIALIZE LICENCAS-REG
           CANCEL "CWFILE"
           DISPLAY CTAC-LIT-CWCADS
                   CTAC-VAR-CWCADS

           PERFORM TEST AFTER UNTIL FINALIZAR
                   IF   FUNCAO = SPACE
                        MOVE 0 TO CHAVE
                   END-IF
                   CALL "CWIAEF" USING FUNCAO
                   IF  NOT FINALIZAR
                       PERFORM 100-PROCESSAMENTO THRU 100-99-FIM
                   END-IF
           END-PERFORM

           CLOSE LICENCAS

           IF   PARAR
                STOP RUN
           ELSE
                GOBACK
           END-IF.

       100-PROCESSAMENTO.

           COPY CWESCP.
           CALL "CWATCH"
           MOVE 0 TO LICENCA

           EVALUATE TRUE
            WHEN INCLUSAO
                 PERFORM 140-NOVA-CHAVE THRU 140-99-FIM
                 ACCEPT T00
                 IF   LICENCAS-CHAVE = SALVA-CHAVE
                      PERFORM 140-NOVA-CHAVE THRU 140-99-FIM
                 END-IF
            WHEN ALTERACAO OR EXCLUSAO OR CONSULTA
                 MOVE 0            TO POSICIONA
                 MOVE LICENCAS-REG TO SALVA-REG
                 PERFORM UNTIL CHAVE NOT = 0
                         MOVE 10            TO CWBOXS-LINE
                                               CWBOXS-COLUMN
                         MOVE "Pesquisar:"  TO CWBOXS-TITLE
                         MOVE "~C¢digo"     TO CWBOXS-TEXT   (1)
                         MOVE "~Licenciado" TO CWBOXS-TEXT   (2)
                         CALL "CWBOXS"   USING PARAMETROS-CWBOXS
                         MOVE CWBOXS-OPTION TO CHAVE
                                               POSICIONA
                         IF  CHAVE = 0
                             MOVE 99    TO CHAVE
                             MOVE SPACE TO FUNCAO
                         END-IF
                 END-PERFORM
                 EVALUATE CHAVE
                     WHEN 1
                          ACCEPT T00
                     WHEN 2
                          ACCEPT T01
                 END-EVALUATE
                 IF   LICENCAS-REG = SALVA-REG
                 AND  CHAVE        = CHAVE-ANTERIOR
                 AND  FUNCAO       = FUNCAO-ANTERIOR
                      MOVE 0 TO POSICIONA
                 END-IF
                 MOVE CHAVE  TO CHAVE-ANTERIOR
                 MOVE FUNCAO TO FUNCAO-ANTERIOR
                 IF   POSICIONA    NOT = 0
                 OR   LICENCAS-REG NOT = SALVA-REG
                      MOVE 1 TO POSICIONA
                      EVALUATE CHAVE
                          WHEN 1
                         START LICENCAS KEY NOT < LICENCAS-CHAVE
                               INVALID KEY
                                       START LICENCAS
                                         KEY NOT > LICENCAS-CHAVE
                                       END-START
                         END-START
                          WHEN 2
                         START LICENCAS KEY NOT < LICENCAS-LICENCIADO
                               INVALID KEY
                                       START LICENCAS
                                         KEY NOT > LICENCAS-LICENCIADO
                                       END-START
                         END-START
                      END-EVALUATE
                 END-IF
           END-EVALUATE

           IF   POSICIONA = 0
                ACCEPT TECLA FROM ESCAPE KEY
           ELSE
                SET PAGE-DOWN TO TRUE
           END-IF

           MOVE SPACES      TO CWSEND-MSG

           EVALUATE TRUE
               WHEN ESC
                    MOVE SPACE TO FUNCAO
               WHEN INCLUSAO
                AND LICENCAS-CODIGO = 0
                    MOVE MSG-A (3) TO CWSEND-MSG
                    PERFORM 150-ENVIA-MENSAGEM THRU 150-99-FIM
               WHEN INCLUSAO
                    PERFORM TEST AFTER
                                 UNTIL FS-LICENCAS NOT = "9D"
                            READ LICENCAS
                    END-PERFORM
                    IF   FS-LICENCAS < "10"
                         DISPLAY CTAC-VAR-CWCADS
                         MOVE MSG-A (2) TO CWSEND-MSG
                         PERFORM 150-ENVIA-MENSAGEM THRU 150-99-FIM
                    ELSE
                         IF   FS-LICENCAS = "23"
                              WRITE LICENCAS-REG
                              READ LICENCAS WITH LOCK
                              PERFORM 120-CRITICA THRU 120-99-FIM
                              IF   EFETIVAR
                                   REWRITE LICENCAS-REG
                                   UNLOCK LICENCAS
                              ELSE
                                   DELETE LICENCAS RECORD
                              END-IF
                         ELSE
                              CALL "CWISAM" USING ER-LICENCAS
                         END-IF
                    END-IF
               WHEN (ALTERACAO OR EXCLUSAO OR CONSULTA)
                AND (PAGE-DOWN OR PAGE-UP)
                    IF   FS-LICENCAS = "23"
                         EVALUATE CHAVE
                             WHEN 1
                                  START LICENCAS
                                        KEY NOT < LICENCAS-CHAVE
                                        INVALID KEY
                                          START LICENCAS
                                            KEY NOT > LICENCAS-CHAVE
                                          END-START
                                  END-START
                             WHEN 2
                                  START LICENCAS
                                        KEY NOT < LICENCAS-LICENCIADO
                                        INVALID KEY
                                          START LICENCAS
                                        KEY NOT > LICENCAS-LICENCIADO
                                    END-START
                                  END-START
                         END-EVALUATE
                    END-IF
                    EVALUATE TRUE
                             WHEN PAGE-DOWN
                                  READ LICENCAS
                                       NEXT RECORD IGNORE LOCK
                             WHEN PAGE-UP
                                  READ LICENCAS
                                       PREVIOUS RECORD IGNORE LOCK
                    END-EVALUATE
                    IF   FS-LICENCAS > "09"
                         IF   FS-LICENCAS NOT = "10"
                         AND  FS-LICENCAS > "09"
                              MOVE "V" TO FUNCAO
                         END-IF
                         CALL "CWISAM" USING ER-LICENCAS
                    ELSE
                         DISPLAY CTAC-VAR-CWCADS
                    END-IF
                    IF  FS-LICENCAS = "10"
                        EVALUATE TRUE
                                 WHEN PAGE-DOWN
                                      READ LICENCAS
                                      PREVIOUS RECORD IGNORE LOCK
                                 WHEN PAGE-UP
                                      READ LICENCAS
                                      NEXT RECORD IGNORE LOCK
                        END-EVALUATE
                        IF   FS-LICENCAS NOT = "10"
                             MOVE "10" TO FS-LICENCAS
                        END-IF
                    END-IF
               WHEN (ALTERACAO OR EXCLUSAO OR CONSULTA)
                AND ENTER-KEY
                    MOVE SPACE TO COMANDO
                    PERFORM TEST AFTER UNTIL FS-LICENCAS NOT = "9D"
                            MOVE 0 TO  LICENCA-KEY
                            IF   CONSULTA
                                 READ LICENCAS IGNORE LOCK
                                 PERFORM CALCULO
                            ELSE
                                 READ LICENCAS WITH LOCK
                            END-IF
                    END-PERFORM
                    MOVE 1      TO POSICIONA
                    MOVE SPACES TO SALVA-REG
                    DISPLAY CTAC-VAR-CWCADS
                    EVALUATE TRUE
                        WHEN FS-LICENCAS = "23"
                             MOVE MSG-A (1) TO CWSEND-MSG
                             PERFORM 150-ENVIA-MENSAGEM THRU 150-99-FIM
                        WHEN FS-LICENCAS > "09"
                         AND FS-LICENCAS NOT = "9D"
                             MOVE "V"         TO FUNCAO
                             CALL "CWISAM" USING ER-LICENCAS
                        WHEN ALTERACAO
                             PERFORM 120-CRITICA THRU 120-99-FIM
                             IF   EFETIVAR
                                  REWRITE LICENCAS-REG
                             ELSE
                                  UNLOCK LICENCAS
                             END-IF
                        WHEN EXCLUSAO
                             PERFORM 130-CONFIRMA THRU 130-99-FIM
                             IF   EFETIVAR
                                  DELETE LICENCAS RECORD
                             ELSE
                                  UNLOCK LICENCAS
                             END-IF
                    END-EVALUATE
                    IF   FS-LICENCAS > "09"
                    AND  EFETIVAR
                         MOVE "V" TO FUNCAO
                         CALL "CWISAM" USING ER-LICENCAS
                    END-IF
           END-EVALUATE.

       100-99-FIM. EXIT.

       120-CRITICA.

           MOVE 1 TO CAMPO

           PERFORM TEST AFTER UNTIL ESC
                                 OR CAMPO > CAMPOS
              PERFORM 150-ENVIA-MENSAGEM THRU 150-99-FIM
              MOVE SPACES      TO CWSEND-MSG
              MOVE 0           TO ERRO
              EVALUATE CAMPO
                 WHEN 01 ACCEPT T01
                         IF   LICENCAS-LICENCIADO = SPACES
                              MOVE 1 TO ERRO
                         END-IF
              END-EVALUATE
              ACCEPT TECLA FROM ESCAPE KEY
              IF   ERRO = 1
                   MOVE "Falta nome" TO CWSEND-MSG
              END-IF
              EVALUATE TRUE
                  WHEN (ENTER-KEY OR CURSOR-DOWN)
                   AND CWSEND-MSG = SPACES
                       ADD 1 TO CAMPO
                  WHEN CURSOR-UP AND CAMPO > 1
                       SUBTRACT 1    FROM CAMPO
                       MOVE     SPACES TO CWSEND-MSG
              END-EVALUATE
           END-PERFORM

           MOVE SPACE TO COMANDO

           IF   NOT ESC
                PERFORM 130-CONFIRMA THRU 130-99-FIM
           ELSE
                MOVE SPACE TO FUNCAO
           END-IF.

       120-99-FIM. EXIT.

       130-CONFIRMA.

           COPY CWEFAB.
           IF   EFETIVAR
                MOVE LICENCAS-CODIGO TO OBS
                CALL "CWLOGW"     USING FUNCAO OBS.

       130-99-FIM. EXIT.

       140-NOVA-CHAVE.

           MOVE ALL X"FF" TO LICENCAS-REG
           START LICENCAS KEY NOT > LICENCAS-CHAVE
            INVALID KEY
                    INITIALIZE LICENCAS-REG
                    MOVE 1 TO LICENCAS-CODIGO
                    DISPLAY CTAC-VAR-CWCADS
               NOT INVALID KEY
                   READ LICENCAS PREVIOUS RECORD IGNORE LOCK
                   ADD  1              TO LICENCAS-CODIGO
                   MOVE LICENCAS-CHAVE TO SALVA-CHAVE
                   INITIALIZE LICENCAS-REG
                   MOVE SALVA-CHAVE    TO LICENCAS-CHAVE
                   DISPLAY CTAC-VAR-CWCADS
           END-START
           DISPLAY T00.

       140-99-FIM. EXIT.

       150-ENVIA-MENSAGEM.

           MOVE SPACES      TO CWSEND-SCREENS
           CALL "CWSEND" USING PARAMETROS-CWSEND.

       150-99-FIM. EXIT.

       calculo.

           MOVE LICENCAS-LICENCIADO TO LICENCIADO
           MOVE LICENCAS-CODIGO     TO LICENCA-SERIECW

           MOVE 0          TO SOMA I2
           IF   LICENCIADO = LOW-VALUES
                MOVE SPACES TO LICENCIADO
           END-IF
           INSPECT LICENCIADO CONVERTING ACENTOS-850 TO ACENTOS-OFF
           INSPECT LICENCIADO CONVERTING MINUSCULAS  TO MAIUSCULAS
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LENGTH OF LICENCIADO
                   IF   BYTE-OK (I)
                        ADD  1                 TO I2
                        MOVE LICENCIADO (I: 1) TO LICENCIADO2 (I2: 1)
                        COMPUTE M = (U (I2) * I2) + (I * U (I2))
                        ADD M                  TO SOMA
                        SUBTRACT LICENCA-SERIECW FROM SOMA
                   END-IF
           END-PERFORM
           MOVE SOMA (2: 5)     TO LICENCA-KEY
           DISPLAY T02.

       fim-calculo. exit.
       END PROGRAM LICENCIA.
