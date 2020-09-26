      $Set CallFH"FHREDIR" Gnt()
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CARREGADOR-FILENAME.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  01/08/2017 11:57:05.
       SECURITY.      *************************************************
                      * Carrega arquivo extraido (FNAME)              *
                      *                      http://www.COBOLware.com *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
        DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT XFILENAME ASSIGN TO DISK
                  ORGANIZATION IS LINE SEQUENTIAL
                  LOCK MODE IS EXCLUSIVE
                  STATUS IS FS-XFILENAME.
       DATA DIVISION.
       FILE SECTION.
       FD  XFILENAME
           VALUE OF FILE-ID IS LB-XFILENAME.
       01 XFILENAME-REG.
           05 FILENAME-CHAVE.
              10 TXT-CODIGO                 PIC  9(005).
           05 TXT-DESCRICAO                 PIC  X(030).
           05 TXT-PRECO                     PIC  9(008)V99.
           05 TXT-TIPO                      PIC  9(001).
           05 FILENAME-OPCOES.
              10 TXT-IMPORTADO              PIC  9(001).
              10 TXT-GARANTIA               PIC  9(001).
              10 TXT-DURAVEL                PIC  9(001).
       WORKING-STORAGE SECTION.
      *FD  FILENAME                                                     COBOLware
      *    LABEL RECORD STANDARD                                        COBOLware
      *        VALUE OF FILE-ID IS LB-FILENAME.                         COBOLware
       01 FILENAME-REG.
           05 FILENAME-CHAVE.
              10 FILENAME-CODIGO PIC 9(005).
           05 FILENAME-DESCRICAO PIC X(030).
           05 FILENAME-PRECO PIC 9(008)V99.
           05 FILENAME-TIPO PIC 9(001).
           05 FILENAME-OPCOES.
              10 FILENAME-IMPORTADO PIC 9(001).
              10 FILENAME-GARANTIA PIC 9(001).
              10 FILENAME-DURAVEL PIC 9(001).
       COPY CWSQLC.
       01  PT-FILENAME VALUE SPACES.
           05                               PIC  X(004).
           05 RK-FILENAME                   PIC  X(030).
           05                               PIC  X(003).
           05 FILENAME-REC-FLAG             PIC  X(001).
           05 FILENAME-REC-LENGTH    COMP-X PIC  9(004).
           05                               PIC  X(343).
           05 POINTER-VOLUME         COMP-3 PIC  9(018).
       77  ONE                              PIC  9(001)    VALUE 1.
       COPY CWSEND.
       01  ER-FILENAME.
           05 FS-FILENAME                   PIC  X(002)    VALUE "00".
           05 LB-FILENAME                   PIC  X(255)    value
           "P:\COBWARE\FileName".
       77  CWCARDISPLAY                     PIC  9(008)    VALUE 0.
       77  CWNUMERO                         PIC  X(008)    VALUE SPACES.
       77  NUMCHECK                         PIC  9(001)    VALUE 1.
       77  ERROS-FILENAME                   PIC  9(012)    VALUE 0.
       77  ERROS-STATUS                     PIC  9(012)    VALUE 0.
       77  I                                PIC  9(002)    VALUE 0.
       77  II                               PIC  9(002)    VALUE 0.
       77  LD-FILENAME                      PIC  9(012)    VALUE 0.
       77  LD-FILENAME-ED                   PIC ZZZ.ZZZ.ZZZ.ZZ9B.
       77  CWNUMC                           PIC  X(8)      value
           "CWNUMC".
       01  ER-XFILENAME.
           05 FS-XFILENAME                  PIC  X(002)    VALUE "00".
           05 LB-XFILENAME                  PIC  X(255)    value
           "txt/filename.txt".
       77  CWOBS                            PIC  X(5000)   VALUE SPACES.
       01  ORACLE-LOGIN.
           05 ORACLE-USERNAME               PIC  X(010)    VALUE SPACES.
           05 ORACLE-PASSWORD               PIC  X(010)    VALUE SPACES.
           05 ORACLE-PLUS                   PIC  X(050)    VALUE SPACES.
       77  SQLHANDLER                       PIC  X(50)     value
           "oracle-filename".
       PROCEDURE DIVISION.
       INICIO.
           DISPLAY "CARREGADOR-FILENAME" AT 0803
                   "Carregador do arquivo FILENAME"
                   AT 0903
                   "Copyright (C) 2017 COBOLware Services Ltda."
                   AT 1003
                   WITH BACKGROUND-COLOR 7 FOREGROUND-COLOR 0 SIZE 43
                   "COBOL"
                   AT 1022
                   WITH BACKGROUND-COLOR 7 FOREGROUND-COLOR 1
                   "ware"
                   AT 1027
                   WITH BACKGROUND-COLOR 7 FOREGROUND-COLOR 4
                   "http://www.COBOLware.com"
                   AT 1103
                   WITH BACKGROUND-COLOR 7 FOREGROUND-COLOR 0 SIZE 43
                   "COBOL"
                   AT 1114
                   WITH BACKGROUND-COLOR 7 FOREGROUND-COLOR 1
                   "ware "
                   AT 1119
                   WITH BACKGROUND-COLOR 7 FOREGROUND-COLOR 4
                   " "
                   AT 1202
           OPEN INPUT XFILENAME
           IF  FS-XFILENAME NOT = "00"
               MOVE SPACES TO CWOBS
               string "Erro no OPEN XFILENAME = " FS-XFILENAME
                      DELIMITED BY SIZE INTO CWOBS
               CALL "CWCLOG" USING Z" Z" CWOBS
                 ON EXCEPTION
                    CONTINUE
               END-CALL
               DISPLAY CWOBS WITH SIZE 70 FOREGROUND-COLOR 4
               GOBACK
           END-IF
           DISPLAY "ORACLE-USERNAME" UPON ENVIRONMENT-NAME
           ACCEPT   ORACLE-USERNAME  FROM ENVIRONMENT-VALUE
           DISPLAY "ORACLE-PASSWORD" UPON ENVIRONMENT-NAME
           ACCEPT   ORACLE-PASSWORD  FROM ENVIRONMENT-VALUE
           IF ORACLE-USERNAME = SPACES
           AND ORACLE-PASSWORD = SPACES
               DISPLAY "ORACLE-STRING" UPON ENVIRONMENT-NAME
               ACCEPT   ORACLE-LOGIN   FROM ENVIRONMENT-VALUE
               IF  ORACLE-LOGIN = SPACES
                   DISPLAY "ORACLE_STRING" UPON ENVIRONMENT-NAME
                   ACCEPT   ORACLE-LOGIN   FROM ENVIRONMENT-VALUE
               END-IF
           END-IF
           IF ORACLE-LOGIN = SPACES
              MOVE "Faltam os sets de acesso ao Oracle" TO CWSEND-MSG
              CALL "CWSEND" USING PARAMETROS-CWSEND
               ON EXCEPTION
                  DISPLAY CWSEND-MSG
                  STOP "[Enter para encerrar]"
              END-CALL
              STOP RUN
           ELSE
              CALL "CWORCN" USING ORACLE-LOGIN
           END-IF
           DISPLAY "CWNUMC" UPON ENVIRONMENT-NAME
           ACCEPT  CWNUMC FROM ENVIRONMENT-VALUE
           INSPECT CWNUMC CONVERTING "of" TO "OF"
           IF   CWNUMC = "OFF"
                MOVE 0 TO NUMCHECK
           END-IF
           MOVE "*" TO FILENAME-REC-FLAG
           MOVE LENGTH FILENAME-REG TO FILENAME-REC-LENGTH
                           PERFORM TEST AFTER
                                  UNTIL FS-FILENAME NOT = "9A"
           set CWSQLC-CREATE to true
           call SQLHANDLER using CWSQLC FILENAME-REG
                ER-FILENAME PT-FILENAME
                ON EXCEPTION
           CALL "ORACLE-FILENAME" using CWSQLC FILENAME-REG
                ER-FILENAME PT-FILENAME end-call
                   MOVE "ORACLE-FILENAME" TO SQLHANDLER
           END-CALL
                              IF  FS-FILENAME = "9A"
                                  DISPLAY "Tabela em uso"  AT 1298
                                  CALL "system" USING Z"sleep 60"
                              END-IF
                              DISPLAY SPACES AT 1208 WITH SIZE 60
                           END-PERFORM
           IF  FS-FILENAME > "09"
               MOVE SPACES TO CWOBS
               string "Erro no OPEN FILENAME = " FS-FILENAME
                      DELIMITED BY SIZE INTO CWOBS
               CALL "CWCLOG" USING Z" Z" CWOBS
                 ON EXCEPTION
                    CONTINUE
               END-CALL
               DISPLAY CWOBS WITH SIZE 70 FOREGROUND-COLOR 4
               GOBACK
           END-IF
           DISPLAY "              0 Carregando FILENAME" AT 1208
           MOVE "Carregador de FILENAME iniciado" TO CWOBS
           CALL "CWCLOG" USING Z"x" CWOBS
             ON EXCEPTION
                CONTINUE
           END-CALL
           PERFORM AJUSTA THRU FIM-AJUSTA
           set CWSQLC-COMMIT-MANUAL to true
           call SQLHANDLER using CWSQLC FILENAME-REG
                ER-FILENAME PT-FILENAME
           PERFORM UNTIL FS-XFILENAME > "09"
                   READ XFILENAME
                   IF   FS-XFILENAME < "10"
                        ADD 1 TO LD-FILENAME
                        IF NUMCHECK = 1
                           call CWNUMC using "R" address LB-XFILENAME
                                             LD-FILENAME
                                            XFILENAME-REG
                                     LENGTH XFILENAME-REG
                            ON EXCEPTION
                               MOVE 0 TO NUMCHECK
                            END-CALL
                        END-IF
                        IF NUMCHECK = 1 AND (TXT-CODIGO NOT NUMERIC)
                        AND (TXT-CODIGO(1:) NOT = SPACES)
                           call CWNUMC using "F" TXT-CODIGO
                                     length TXT-CODIGO
                                          Z"FILENAME-CODIGO"
                        END-IF
                        MOVE TXT-CODIGO TO FILENAME-CODIGO
                        MOVE TXT-DESCRICAO TO FILENAME-DESCRICAO
                        MOVE TXT-PRECO TO FILENAME-PRECO
                        IF NUMCHECK = 1 AND (TXT-TIPO NOT NUMERIC)
                        AND (TXT-TIPO(1:) NOT = SPACES)
                           call CWNUMC using "F" TXT-TIPO
                                     length TXT-TIPO
                                          Z"FILENAME-TIPO"
                        END-IF
                        MOVE TXT-TIPO TO FILENAME-TIPO
                        IF NUMCHECK = 1 AND (TXT-IMPORTADO NOT NUMERIC)
                        AND (TXT-IMPORTADO(1:) NOT = SPACES)
                           call CWNUMC using "F" TXT-IMPORTADO
                                     length TXT-IMPORTADO
                                          Z"FILENAME-IMPORTADO"
                        END-IF
                        MOVE TXT-IMPORTADO TO FILENAME-IMPORTADO
                        IF NUMCHECK = 1 AND (TXT-GARANTIA NOT NUMERIC)
                        AND (TXT-GARANTIA(1:) NOT = SPACES)
                           call CWNUMC using "F" TXT-GARANTIA
                                     length TXT-GARANTIA
                                          Z"FILENAME-GARANTIA"
                        END-IF
                        MOVE TXT-GARANTIA TO FILENAME-GARANTIA
                        IF NUMCHECK = 1 AND (TXT-DURAVEL NOT NUMERIC)
                        AND (TXT-DURAVEL(1:) NOT = SPACES)
                           call CWNUMC using "F" TXT-DURAVEL
                                     length TXT-DURAVEL
                                          Z"FILENAME-DURAVEL"
                        END-IF
                        MOVE TXT-DURAVEL TO FILENAME-DURAVEL
                        IF LB-XFILENAME(50:1) = "*"
                           ADD 1 TO ERROS-FILENAME
                        ELSE
                          perform salvas
                           set CWSQLC-WRITE to true
                           call SQLHANDLER using CWSQLC FILENAME-REG
                                ER-FILENAME PT-FILENAME
                        END-IF
                        IF  FS-XFILENAME = "10"
                        OR  CWCARDISPLAY = 0
                        OR  CWCARDISPLAY (ii:) = LD-FILENAME (ii:)
                            MOVE    LD-FILENAME TO LD-FILENAME-ED
                            DISPLAY LD-FILENAME-ED AT 1208
                                    WITH FOREGROUND-COLOR 6 HIGH
                        END-IF
                        IF  FS-FILENAME > "09"
                            DISPLAY "Erro no WRITE FILENAME" AT 2010
                            WITH FOREGROUND-COLOR 4
                                " = " FS-FILENAME
                                WITH FOREGROUND-COLOR 4
                            ADD 1 TO ERROS-FILENAME
                                     ERROS-STATUS
                         IF ERROS-STATUS < 101
                            MOVE SPACES TO CWOBS
                            MOVE LD-FILENAME TO LD-FILENAME-ED
                            PERFORM VARYING I FROM 1 BY 1
                                    UNTIL LD-FILENAME-ED (I:1)
                                    NOT = SPACE
                                    CONTINUE
                            END-PERFORM
                            string "Erro " FS-FILENAME
                                   " no registro " LD-FILENAME-ED(I:)
                                   "em FILENAME=" XFILENAME-REG
                                   DELIMITED BY SIZE INTO CWOBS
                            CALL "CWCLOG" USING Z"x" CWOBS
                              ON EXCEPTION
                                 CONTINUE
                            END-CALL
                         END-IF
                        END-IF
                   ELSE
                        IF   FS-XFILENAME = "10"
                             CONTINUE
                        ELSE
                              CALL "CWISAM" USING ER-XFILENAME
                                ON EXCEPTION
                                   DISPLAY "Erro no READ "
                                   "XFILENAME = " FS-XFILENAME
                                    WITH FOREGROUND-COLOR 4
                              END-CALL
                        END-IF
                   END-IF
           END-PERFORM
           CLOSE XFILENAME
           set CWSQLC-COMMIT to true
           call SQLHANDLER using CWSQLC FILENAME-REG
                ER-FILENAME PT-FILENAME
           set CWSQLC-CLOSE to true
           call SQLHANDLER using CWSQLC FILENAME-REG
                ER-FILENAME PT-FILENAME
           If cwsqlc-cancel cancel SQLHANDLER end-if
           SUBTRACT ERROS-FILENAME FROM LD-FILENAME
           MOVE LD-FILENAME TO LD-FILENAME-ED
           MOVE SPACES TO CWOBS
           string "Carregador de FILENAME encerrado,"
                  LD-FILENAME-ED(1:) "registros carregados"
                  DELIMITED BY SIZE INTO CWOBS
           CALL "CWPACK" USING CWOBS LENGTH CWOBS
             ON EXCEPTION
                CONTINUE
           END-CALL
           CALL "CWCLOG" USING Z"x" CWOBS
             ON EXCEPTION
                CONTINUE
           END-CALL
           IF ERROS-FILENAME NOT = ZERO
              MOVE ERROS-FILENAME TO LD-FILENAME-ED
              MOVE SPACES TO CWOBS
              STRING LD-FILENAME-ED (1:) "ignorados por erros"
                     DELIMITED BY SIZE
                INTO CWOBS
              CALL "CWPACK" USING CWOBS LENGTH CWOBS
                ON EXCEPTION
                   CONTINUE
              END-CALL
              CALL "CWCLOG" USING Z"x" CWOBS
                ON EXCEPTION
                   CONTINUE
              END-CALL
           END-IF
           GOBACK.
       AJUSTA.
           DISPLAY "CWCARDISPLAY" UPON ENVIRONMENT-NAME
           MOVE SPACES TO CWNUMERO
           ACCEPT CWNUMERO FROM ENVIRONMENT-VALUE
           MOVE 8 TO II
           PERFORM VARYING I FROM LENGTH OF CWNUMERO BY -1
                   UNTIL I = 0
                   IF  CWNUMERO (I: 1) NUMERIC
                       MOVE CWNUMERO (I: 1) TO CWCARDISPLAY (II: 1)
                       SUBTRACT 1 FROM II
                   END-IF
           END-PERFORM
           IF  II < 8
               ADD 1 TO II
           END-IF.
       FIM-AJUSTA. EXIT.
       SALVAS.
           on 1000 and every 1000
              set CWSQLC-COMMIT to true
           call SQLHANDLER using CWSQLC FILENAME-REG
                ER-FILENAME PT-FILENAME.
       FIM-SALVAS. EXIT.
