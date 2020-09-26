      $Set CallFH"FHREDIR"
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWLOGW.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  05/05/1989.
       SECURITY.      *************************************************
                      *                                               *
                      *  Grava funcao (CWIAEF) no log                 *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT OPTIONAL CWLOGF ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  RECORD KEY    IS CWLOGF-SEQUENCIA
                  ACCESS MODE   IS DYNAMIC
                  FILE STATUS   IS FS-CWLOGF
                  LOCK MODE     IS MANUAL.

       DATA DIVISION.
       FILE SECTION.

       COPY CWLOGF.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1. COPY CWCASE.
           05 CWLOGRETRY              PIC  X(003) VALUE SPACES.
           05 FROMGUI                 PIC  X(006) VALUE SPACES.
           05 ISAM                    PIC  X(002) VALUE SPACES.
           05 LOG-ABERTO              PIC  9(001) VALUE 0.
           05 BAD                     PIC  X(002) VALUE SPACES.
           05 CCITCP2                 PIC  X(110) VALUE SPACES.
           05 DADOS                   PIC  X(110) VALUE SPACES.
           05 CWLOGP                  PIC  X(008) VALUE SPACES.
           05 X91-RESULT       COMP-X PIC  9(002) VALUE 0.
           05 X91-FUNCTION     COMP-X PIC  9(002) VALUE 16.
           05 X91-PARAMETER    COMP-X PIC  9(002) VALUE 0.
           05 TEXTO                   PIC  X(035) VALUE SPACES.
           05 FATOR-W          COMP-X PIC  9(002) VALUE 0.
           05 I                COMP-X PIC  9(002) VALUE 0.
           05 L                COMP-X PIC  9(002) VALUE 0.
           05 R                COMP-X PIC  9(002) VALUE 0.
           05 PROGRAMA                PIC  X(008) VALUE "CWMENU".
           05 NOME                    PIC  X(030) VALUE "LOGON".
           05 TASK                    PIC  9(006) VALUE 0.
           05 DATA-DE-HOJE            PIC  X(010) VALUE SPACES.
           05 HORA                    PIC  X(008) VALUE SPACES.
           05 LOG                     PIC  X(001) VALUE "1".
              88 LOG-ON                           VALUE "1".
              88 LOG-OFF                          VALUE "0".
           05 ER-CWLOGF.
              10 FS-CWLOGF             PIC  X(002) VALUE "00".
              10 LB-CWLOGF             PIC  X(255) VALUE SPACES.
           05 N-CWLOGF.
              10 FILLER                PIC  X(005) VALUE "cwlog".
              10 FILLER                PIC  X(001) VALUE "-".
              10 ANO-CWLOGF            PIC  9(004) VALUE ZERO.
              10 FILLER                PIC  X(001) VALUE "-".
              10 MES-CWLOGF            PIC  9(002) VALUE ZERO.
              10 LETRA                 PIC  X(002) VALUE SPACES.
              10 FILLER                PIC  X(035) VALUE SPACES.
           05 VEZ                      PIC  9(001) VALUE 1.
           05 MSG                      PIC  X(060) VALUE SPACES.

       COPY CWBOXS.
       COPY CWNCOR.
       COPY CWTIME.
       COPY CWGETL.
       COPY CWUNIX.
       COPY CWEXEC.

       LINKAGE SECTION.

       COPY CWLOGW.

       01  OBS                               PIC  X(035).

       PROCEDURE DIVISION USING PARAMETROS-CWLOGW OBS.

       000-INICIO.

           DISPLAY "CWFROMGUI"  UPON ENVIRONMENT-NAME
           ACCEPT  FROMGUI      FROM ENVIRONMENT-VALUE
           IF FROMGUI = ALL '*'
              GOBACK
           END-IF
           DISPLAY "CWLOGW"     UPON ENVIRONMENT-NAME
           DISPLAY "ON"         UPON ENVIRONMENT-VALUE
           MOVE SPACES      TO CWLOGF-REG
           CALL "CWGETU" USING NOME TASK PROGRAMA '?'
           CALL "CWGETL" USING PARAMETROS-CWGETL
           MOVE CWGETL-LOG TO LOG
           IF   VEZ = 1
                CALL "CWUNIX" USING PARAMETROS-CWUNIX
                DISPLAY "CWLOGP"     UPON ENVIRONMENT-NAME
                ACCEPT CWLOGP        FROM ENVIRONMENT-VALUE
                DISPLAY "CWLOGRETRY" UPON ENVIRONMENT-NAME
                ACCEPT CWLOGRETRY    FROM ENVIRONMENT-VALUE
                INSPECT CWLOGRETRY CONVERTING LOW-VALUE TO SPACE
                DISPLAY "CCITCP2"    UPON ENVIRONMENT-NAME
                ACCEPT CCITCP2       FROM ENVIRONMENT-VALUE
                MOVE 2 TO VEZ
      *         SET CWSQLC-OPEN TO TRUE
      *         CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
      *         MOVE "LG" TO CWCONF-REGLG
      *         SET CWSQLC-READ        TO TRUE
      *         SET CWSQLC-IGNORE-LOCK TO TRUE
      *         CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
      *         IF   FS-CWCONF < "10"
      *              MOVE CWCONF-LOG TO LOG
      *         END-IF
      *         SET CWSQLC-CLOSE TO TRUE
      *         CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           END-IF

           IF   LOG-OFF
           OR   CWGETL-LOG = 2
           OR   FS-CWLOGF = '9«'
                GO TO 000-99-FIM
           END-IF

           CALL X"91" USING X91-RESULT X91-FUNCTION X91-PARAMETER
           IF  X91-PARAMETER > 1
               MOVE OBS         TO TEXTO
           ELSE
               MOVE CWLOGW-TEXT TO TEXTO
           END-IF
           INSPECT TEXTO CONVERTING LOW-VALUE TO SPACE

           SET CWTIME-NORMAL       TO TRUE
           SET CWTIME-TODAY        TO TRUE
           CALL "CWTIME"        USING PARAMETROS-CWTIME
           MOVE CWTIME-TIME-FINAL  TO CWTIME-TIME
           IF ( CWTIME-DATE-FINAL (5: 4) <> ANO-CWLOGF )
           OR ( CWTIME-DATE-FINAL (2: 2) <> MES-CWLOGF )
              IF LOG-ABERTO = 1
                 CLOSE CWLOGF
                 MOVE 0 TO LOG-ABERTO
              END-IF
              MOVE CWTIME-DATE-FINAL (5: 4) TO ANO-CWLOGF
              MOVE CWTIME-DATE-FINAL (3: 2) TO MES-CWLOGF
              MOVE 0                        TO L
              MOVE SPACE                    TO LETRA
           END-IF
           SET CWTIME-EDIT         TO TRUE
           MOVE CWTIME-DATE-FINAL  TO CWTIME-DATE
           CALL "CWTIME"        USING PARAMETROS-CWTIME
           MOVE CWTIME-DATE-EDITED TO DATA-DE-HOJE
           MOVE CWTIME-TIME-EDITED TO HORA
           SET CWTIME-NORMAL       TO TRUE
           SET CWTIME-TODAY        TO TRUE
           CALL "CWTIME"        USING PARAMETROS-CWTIME
           MOVE CWTIME-TIME-FINAL  TO CWTIME-TIME
           SET CWTIME-EDIT         TO TRUE
           MOVE CWTIME-DATE-FINAL  TO CWTIME-DATE
           CALL "CWTIME"        USING PARAMETROS-CWTIME
           MOVE CWTIME-DATE-EDITED TO DATA-DE-HOJE
           MOVE CWTIME-TIME-EDITED TO HORA
           MOVE DATA-DE-HOJE       TO CWLOGF-DATA-DE-HOJE
           MOVE HORA               TO CWLOGF-HORA
           MOVE PROGRAMA           TO CWLOGF-PROGRAMA
      *    MOVE "-"                TO CWLOGF-PROGRAMA (9: 1)
           MOVE TEXTO              TO CWLOGF-FUNCAO-PROGRAMA (12: )
           EVALUATE CWLOGW-FUNCTION
             WHEN "A"
                MOVE "Alteraá∆o " TO CWLOGF-FUNCAO-PROGRAMA (1: 11)
             WHEN "C"
                MOVE "Consulta  " TO CWLOGF-FUNCAO-PROGRAMA (1: 11)
             WHEN "E"
                MOVE "Exclus∆o  " TO CWLOGF-FUNCAO-PROGRAMA (1: 11)
             WHEN "I"
                MOVE "Inclus∆o  " TO CWLOGF-FUNCAO-PROGRAMA (1: 11)
             WHEN "F"
                MOVE "Finalizar " TO CWLOGF-FUNCAO-PROGRAMA (1: 11)
             WHEN "V"
                MOVE "Volta menu" TO CWLOGF-FUNCAO-PROGRAMA (1: 11)
             WHEN OTHER
                MOVE TEXTO        TO CWLOGF-FUNCAO-PROGRAMA
           END-EVALUATE
           MOVE NOME            TO CWLOGF-OPERADOR
           MOVE TASK            TO CWLOGF-TASK
           IF CWLOGW-FUNCTION = '^'
              MOVE OBS (1: 80) TO CWLOGF-RESTO
              INSPECT CWLOGF-RESTO CONVERTING LOW-VALUE TO SPACE
           END-IF
           IF   CWLOGP NOT = SPACES
                CALL CWLOGP USING CWLOGF-REG
                GO TO 000-99-FIM
           END-IF

           IF LOG-ABERTO = 0
              IF   CWGETL-LOGDIR = "."
                   MOVE N-CWLOGF TO LB-CWLOGF
              ELSE
                   PERFORM VARYING R FROM LENGTH OF CWGETL-LOGDIR BY -1
                             UNTIL R = 1
                               OR (CWGETL-LOGDIR (R: 1) NOT = SPACE
                              AND (CWGETL-LOGDIR (R: 1) NOT = "/")
                              AND (CWGETL-LOGDIR (R: 1) NOT = "\"))
                          CONTINUE
                   END-PERFORM
                   IF CCITCP2 = SPACES
                      CALL "CBL_CREATE_DIR" USING CWGETL-LOGDIR (1: R)
                      IF  RETURN-CODE = 0
                      AND CWUNIX-ON
                          MOVE SPACES TO CWEXEC-COMANDO
                          STRING 'chmod 777 '
                                  CWGETL-LOGDIR (1: R) DELIMITED BY SIZE
                                  X'00'       DELIMITED BY SIZE
                              INTO CWEXEC-COMANDO
                          CALL 'system' USING CWEXEC-COMANDO
                      END-IF
                   ELSE
                      CALL "FS_CREATE_DIR" USING CWGETL-LOGDIR (1: R)
                   END-IF
                   MOVE SPACES TO LB-CWLOGF
                   STRING CWGETL-LOGDIR (1: R) "/" DELIMITED BY SIZE
                          N-CWLOGF                 DELIMITED BY SPACE
                        INTO LB-CWLOGF
              END-IF
              MOVE 0 TO I
              PERFORM TEST AFTER UNTIL FS-CWLOGF < "10"
                                    OR FS-CWLOGF = "9|"
                                    OR CWLOGRETRY = 'OFF'
                      OPEN I-O CWLOGF
                      IF  FS-CWLOGF = '9«'
                          EXIT PERFORM
                      END-IF
                      IF  FS-CWLOGF = "9%"
                          MOVE SPACES TO MSG
                          STRING "Acesso ao arquivo " DELIMITED BY SIZE
                                 LB-CWLOGF DELIMITED BY SPACE
                                 " negado" DELIMITED BY SIZE
                            INTO MSG

                          DISPLAY 'CWSENDLOG' UPON ENVIRONMENT-NAME
                          MOVE SPACES TO BAD
                          ACCEPT        BAD  FROM ENVIRONMENT-VALUE
                          IF   BAD = 'ON'
                               DISPLAY MSG UPON CONSOLE
                               EXIT PERFORM
                          ELSE
                               EXEC COBOLware Send
                                    Message MSG
                               END-EXEC
                          END-IF
                          STOP RUN
                      END-IF
                      IF  CWUNIX-ON
                      AND FS-CWLOGF = "05"
                      AND CCITCP2 = SPACES
                          MOVE SPACES TO CWEXEC-COMANDO
                          STRING 'chmod 666 ' DELIMITED BY SIZE
                                  LB-CWLOGF   DELIMITED BY SPACE
                                  '*'         DELIMITED BY SIZE
                                  X'00'       DELIMITED BY SIZE
                              INTO CWEXEC-COMANDO
                          CALL 'system' USING CWEXEC-COMANDO
                      END-IF
                      IF  FS-CWLOGF = "41"
                          MOVE '00' TO FS-CWLOGF
                      END-IF
                      IF  FS-CWLOGF > "10"
                          DISPLAY 'CWLOGBAD' UPON ENVIRONMENT-NAME
                          MOVE SPACES TO BAD
                          ACCEPT        BAD  FROM ENVIRONMENT-VALUE
                          DISPLAY 'CWLOGI'   UPON ENVIRONMENT-NAME
                          ACCEPT        ISAM FROM ENVIRONMENT-VALUE
                          IF ((BAD = SPACES OR '9A')
                          AND (FS-CWLOGF NOT = "9)")
                          AND (FS-CWLOGF NOT = "9|")
                          AND (ISAM      NOT = "ON"))
                          OR  CWLOGRETRY = 'OFF'
                              DISPLAY FS-CWLOGF UPON ENVIRONMENT-VALUE
                              CALL "CWISAM" USING ER-CWLOGF
                          END-IF
                      END-IF
                      IF  FS-CWLOGF = "9)" OR '39'
                          IF CWLOGRETRY = 'OFF'
                             STOP RUN
                          END-IF
                          IF L < 26
                             ADD  1                 TO L
                             MOVE "-"               TO LETRA(1:1)
                             MOVE MAIUSCULAS (L: 1) TO LETRA(2:1)
                             IF R = 0
                                MOVE N-CWLOGF TO LB-CWLOGF
                             ELSE
                                MOVE SPACES TO LB-CWLOGF
                                STRING CWGETL-LOGDIR (1: R) "/"
                                       DELIMITED BY SIZE
                                       N-CWLOGF DELIMITED BY SPACE
                                  INTO LB-CWLOGF
                             END-IF
                          ELSE
                             EXIT PERFORM
                          END-IF
                      ELSE
                          EXIT PERFORM
                      END-IF
              END-PERFORM
           END-IF
           IF   FS-CWLOGF > "09"
                GO TO 000-99-FIM
           END-IF
           MOVE 1 TO LOG-ABERTO
           IF   CWGETL-CODELOG = 1
                CALL "CWCODE" USING "C" X"64"
                                        FATOR-W
                                        CWLOGF-DADOS
                 MOVE FATOR-W TO CWLOGF-FATOR
           END-IF
           MOVE CWLOGF-DADOS TO DADOS
           MOVE HIGH-VALUES  TO CWLOGF-SEQUENCIA(1:)
           START CWLOGF KEY NOT GREATER CWLOGF-SEQUENCIA
           IF FS-CWLOGF = '23'
              MOVE 0 TO CWLOGF-SEQUENCIA
           ELSE
              READ CWLOGF PREVIOUS RECORD
           END-IF
           PERFORM TEST AFTER UNTIL FS-CWLOGF NOT = '22'
                   ADD 1 TO CWLOGF-SEQUENCIA
                   MOVE DADOS TO CWLOGF-DADOS
                   WRITE CWLOGF-REG
           END-PERFORM.
      *    CLOSE CWLOGF.

       000-99-FIM.
           DISPLAY "CWLOGW" UPON ENVIRONMENT-NAME
           DISPLAY "OFF"    UPON ENVIRONMENT-VALUE.
           GOBACK.

       END PROGRAM CWLOGW.
