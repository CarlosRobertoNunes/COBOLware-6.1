       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWFILE.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  03/02/1990.
       SECURITY.      *************************************************
                      *                                               *
                      *   Fornece label do arquivo                    *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 CWFILEPGM                PIC  X(008) VALUE SPACES.
           05 LABEL-LENGTH             PIC  9(003) VALUE 50.
           05 NUMERO                   PIC  9(018) VALUE 0.
           05 CWNUMERO                 PIC  X(018) VALUE SPACES.
           05 II                       PIC  9(003) VALUE 0.
           05 CWGETL                   PIC  X(003) VALUE SPACES.
           05 LB-CWCONF                PIC  X(255) VALUE SPACES.
           05 ARQUIVO2                 PIC  X(255) VALUE SPACES.
           05 CURDIR                   PIC  X(255) VALUE SPACES.
           05 T                   COMP PIC  9(002) VALUE 0.
           05 I                   COMP PIC  9(002) VALUE 0.
           05 P                   COMP PIC  9(002) VALUE 0.
           05 BARRA                    PIC  X(001) VALUE SPACE.
           05 CWLEVEL                  PIC  X(001) VALUE SPACE.
           05 CWDRIVE                  PIC  X(003) VALUE SPACE.
           05 CWPREFIX                 PIC  X(255) VALUE SPACES.
           05 CWSUFIX                  PIC  X(003) VALUE SPACES.
           05 SUFIX                    PIC  X(003) VALUE SPACES.
           05 CWFILE-CASE              PIC  X(003) VALUE SPACES.
           05 CWFORCE                  PIC  X(002) VALUE SPACES.
           05 CWLOCATE                 PIC  X(002) VALUE SPACES.
           05 OPERADOR                 PIC  X(030) VALUE SPACES.
           05 TASK                     PIC  9(006) VALUE 0.
           05 PROGRAMA                 PIC  X(008) VALUE SPACES.
           05 MINUSCULAS PIC X(26) VALUE "abcdefghijklmnopqrstuvwxyz".
           05 MAIUSCULAS PIC X(26) VALUE "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
           05 LB-WS                    PIC  X(255) VALUE SPACES.
           05 ARQUIVO                  PIC  X(255) VALUE SPACES.
           05 LEN               COMP-X PIC  9(008) VALUE ZERO.
           05 X91-RESULT        COMP-X PIC  9(002) VALUE 0.
           05 X91-FUNCTION      COMP-X PIC  9(002) VALUE 16.
           05 X91-PARAMETER     COMP-X PIC  9(002) VALUE 0.

       COPY CWUNIX.
       COPY CWSEND.
       COPY CWCONF.

       LINKAGE SECTION.

       01  LK-ARQUIVO    PIC X.
       01  LK-LEN COMP-X PIC 9(008).

       PROCEDURE DIVISION USING LK-ARQUIVO LK-LEN.

       000-INICIO.

           ON   1
                DISPLAY "CWFILE"   UPON ENVIRONMENT-NAME
                ACCEPT   CWFILEPGM FROM ENVIRONMENT-VALUE
                INSPECT CWFILEPGM CONVERTING MINUSCULAS TO MAIUSCULAS
                IF  (CWFILEPGM NOT = 'CWFILE')
                AND (CWFILEPGM NOT = 'OFF')
                    DISPLAY 'CWLABEL-LENGTH' UPON ENVIRONMENT-NAME
                    PERFORM AJUSTA
                    IF  NUMERO NOT = 0
                        MOVE NUMERO TO LABEL-LENGTH
                        IF LABEL-LENGTH > 255
                           MOVE 255 TO LABEL-LENGTH
                        END-IF
                    ELSE
                        DISPLAY 'CWLABEL_LENGTH' UPON ENVIRONMENT-NAME
                        PERFORM AJUSTA
                        IF  NUMERO NOT = 0
                            MOVE NUMERO TO LABEL-LENGTH
                            IF LABEL-LENGTH > 255
                               MOVE 255 TO LABEL-LENGTH
                            END-IF
                        END-IF
                    END-IF
                END-IF.

           CALL X"91" USING X91-RESULT X91-FUNCTION X91-PARAMETER

           EVALUATE X91-PARAMETER
               WHEN 0
                    GOBACK
               WHEN 1
                    MOVE 50 TO LEN
               WHEN OTHER
                    MOVE LK-LEN TO LEN
                    IF   LEN > 255
                         MOVE 255 TO LEN
                    END-IF
           END-EVALUATE
           MOVE LK-ARQUIVO(1:LEN) TO ARQUIVO

           IF  CWFILEPGM = 'OFF'
               GOBACK
           ELSE
              IF  (CWFILEPGM NOT = SPACES)
              AND (CWFILEPGM NOT = 'ON')
                  CALL CWFILEPGM USING ARQUIVO(1:LABEL-LENGTH)
                  GOBACK
              END-IF
           END-IF
           CALL "CWSETS" USING "G" "DIR" CURDIR

           ON   1
                DISPLAY "CWLEVEL"  UPON ENVIRONMENT-NAME
                ACCEPT   CWLEVEL   FROM ENVIRONMENT-VALUE
                DISPLAY "CWPREFIX" UPON ENVIRONMENT-NAME
                ACCEPT   CWPREFIX  FROM ENVIRONMENT-VALUE
                IF  CWPREFIX NOT = SPACES
                    PERFORM VARYING P FROM 50 BY -1
                            UNTIL CWPREFIX (P: 1) NOT = SPACE
                            CONTINUE
                    END-PERFORM
                END-IF
                DISPLAY "CWFILE-CASE"  UPON ENVIRONMENT-NAME
                ACCEPT CWFILE-CASE FROM ENVIRONMENT-VALUE
                IF CWFILE-CASE = SPACES
                   DISPLAY "CWFILE_CASE"  UPON ENVIRONMENT-NAME
                   ACCEPT CWFILE-CASE FROM ENVIRONMENT-VALUE
                END-IF
                INSPECT CWFILE-CASE CONVERTING MINUSCULAS TO MAIUSCULAS
                DISPLAY "CWSUFIX"  UPON ENVIRONMENT-NAME
                ACCEPT   CWSUFIX   FROM ENVIRONMENT-VALUE
                MOVE CWSUFIX TO SUFIX
                INSPECT SUFIX CONVERTING MINUSCULAS TO MAIUSCULAS
                IF SUFIX = "OFF"
                   MOVE SPACES TO CWSUFIX
                END-IF
                DISPLAY "CWFORCE"  UPON ENVIRONMENT-NAME
                ACCEPT   CWFORCE   FROM ENVIRONMENT-VALUE
                INSPECT CWFORCE
                        CONVERTING MINUSCULAS TO MAIUSCULAS
                DISPLAY "CWLOCATE"  UPON ENVIRONMENT-NAME
                ACCEPT   CWLOCATE  FROM ENVIRONMENT-VALUE
                INSPECT CWLOCATE
                        CONVERTING MINUSCULAS TO MAIUSCULAS
                DISPLAY "CWDRIVE"  UPON ENVIRONMENT-NAME
                ACCEPT   CWDRIVE   FROM ENVIRONMENT-VALUE
                INSPECT CWDRIVE
                        CONVERTING MINUSCULAS TO MAIUSCULAS
                CALL "CWUNIX" USING PARAMETROS-CWUNIX
                IF   CWUNIX-ON
                     MOVE "/" TO BARRA
                     IF   CWDRIVE = SPACES
                          MOVE "OFF" TO CWDRIVE
                     END-IF
                ELSE
                     MOVE "\" TO BARRA
                END-IF
                CALL  "CWGETU" USING OPERADOR
                                     TASK
                                     PROGRAMA
                                     '?'.

           IF   ARQUIVO = SPACES
           OR  (ARQUIVO (1: 5) = "CWBOX" AND ARQUIVO (2: 2) = "##")
                GOBACK
           ELSE
                PERFORM VARYING I FROM 1 BY 1 UNTIL I = 50
                                    OR ARQUIVO (I: 1) NOT = SPACE
                        CONTINUE
                END-PERFORM
                MOVE ARQUIVO (I: ) TO LB-WS
                MOVE LB-WS         TO ARQUIVO
           END-IF

           DISPLAY "CWGETL" UPON ENVIRONMENT-NAME
           ACCEPT   CWGETL  FROM ENVIRONMENT-VALUE
           IF CWGETL NOT = 'OFF'
              SET CWSQLC-OPEN TO TRUE
              CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
              IF   FS-CWCONF > "09"
                   CALL "CWCONF" USING "ISAM"
                   EXIT PROGRAM
              END-IF
              MOVE "02"        TO CWCONF-REG02
              MOVE ARQUIVO     TO CWCONF-ARQUIVO
              INSPECT CWCONF-ARQUIVO
                      CONVERTING MINUSCULAS TO MAIUSCULAS
              SET CWSQLC-READ TO TRUE
              SET CWSQLC-IGNORE-LOCK TO TRUE
              CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           ELSE
              MOVE '23' TO FS-CWCONF
           END-IF
           IF   FS-CWCONF EQUAL "00"
                MOVE CWCONF-LABEL-file TO ARQUIVO
                IF ARQUIVO (1:1) = '['
                   SET CWSQLC-CLOSE TO TRUE
                   CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF
                                       KCO PCO
                   INSPECT ARQUIVO
                           CONVERTING MAIUSCULAS TO MINUSCULAS
                   MOVE ARQUIVO TO LK-ARQUIVO(1:LEN)
                   GOBACK
                END-IF
                PERFORM INSERT-PATH
                IF   CWDRIVE = "OFF"
                AND  ARQUIVO (2: 1) = ":"
                     MOVE ARQUIVO (3: ) TO LB-WS
                     MOVE LB-WS TO ARQUIVO
                     MOVE ARQUIVO TO LK-ARQUIVO(1:LEN)
                END-IF
           ELSE
                IF   FS-CWCONF NOT EQUAL "23"
                     CALL "CWCONF" USING "ISAM"
                ELSE
                     IF   CWFORCE = "ON"
                     AND  (ARQUIVO NOT = "cwused")
                     AND  (CWGETL NOT = 'OFF')
                          MOVE SPACES TO CWSEND-MSG
                          DISPLAY "CWCONF" UPON ENVIRONMENT-NAME
                          ACCEPT LB-CWCONF FROM ENVIRONMENT-VALUE
                          STRING "Arquivo " DELIMITED BY SIZE
                                    ARQUIVO DELIMITED BY SPACE
                          " nÆo consta na tabela " LB-CWCONF
                                            DELIMITED BY SIZE
                            INTO CWSEND-MSG
                          CALL "CWSEND" USING PARAMETROS-CWSEND
                          STOP RUN
                     ELSE
                          IF   P > 0
                          AND (CWPREFIX (1: P) = ARQUIVO (1: P))
                              CONTINUE
                          ELSE
                          PERFORM INSERT-PATH
                          IF   CWPREFIX NOT = SPACES
                          AND (ARQUIVO (1: 1) NOT = "$")
                          AND (ARQUIVO (1: 1) NOT = "/")
                          AND (ARQUIVO (1: 1) NOT = "\")
                          AND (ARQUIVO (2: 1) NOT = ":")
                          AND (ARQUIVO NOT = "cwused")
                               MOVE SPACES TO LB-WS
                               STRING CWPREFIX DELIMITED BY SPACE
                                      BARRA    DELIMITED BY SIZE
                                      ARQUIVO  DELIMITED BY SPACE
                                 INTO LB-WS
                               MOVE LB-WS TO ARQUIVO
                          ELSE
                               IF   CWDRIVE = "OFF"
                               AND  ARQUIVO (2: 1) = ":"
                                    MOVE ARQUIVO (3: ) TO LB-WS
                                    MOVE LB-WS TO ARQUIVO
                               END-IF
                          END-IF
                          END-IF
                          IF ((CWSUFIX NOT = SPACES)
                          OR  (SUFIX = "OFF"))
                          AND (ARQUIVO NOT = "cwused")
                               MOVE SPACES TO LB-WS
                               PERFORM VARYING I FROM 1 BY 1
                                         UNTIL I = 50
                                            OR ARQUIVO (I: 1) = "."
                                            OR ARQUIVO (I: 1) = " "
                                       CONTINUE
                               END-PERFORM
                               IF   ARQUIVO (I: 1) = "."
                                    SUBTRACT 1 FROM I
                               END-IF
                               IF  SUFIX = "OFF"
                                   MOVE ARQUIVO TO LB-WS
                               ELSE
                                   STRING ARQUIVO (1: I)
                                               DELIMITED BY SPACE
                                      "."      DELIMITED BY SIZE
                                      CWSUFIX  DELIMITED BY SPACE
                                 INTO LB-WS
                               END-IF
                               MOVE LB-WS TO ARQUIVO
                          END-IF
                     END-IF
                END-IF
           END-IF

           IF  CWLEVEL = "/"
               INSPECT ARQUIVO CONVERTING "\" TO "/"
           ELSE
               IF  CWLEVEL = "\"
                   INSPECT ARQUIVO CONVERTING "/" TO "\"
               END-IF
           END-IF

           MOVE 6 TO T
           PERFORM VARYING I FROM 50 BY -1 UNTIL I = 1
                                              OR T = 0
                   IF   ARQUIVO (I: 1) = "#"
                   OR  (ARQUIVO (I: 1) = "$" AND I > 1)
                        MOVE TASK (T: 1) TO ARQUIVO (I: 1)
                        SUBTRACT 1 FROM T
                   END-IF
           END-PERFORM
           IF CWFILE-CASE = "UPP" OR "LOW"
              PERFORM VARYING I FROM 50 BY -1 UNTIL I = 0
                      OR  (ARQUIVO (I: 1) = "/" OR "\")
                      CONTINUE
              END-PERFORM
              ADD 1 TO I
              IF CWFILE-CASE = "UPP"
                 INSPECT ARQUIVO (1: I)
                         CONVERTING MINUSCULAS TO MAIUSCULAS
              ELSE
                 INSPECT ARQUIVO (1: I)
                         CONVERTING MAIUSCULAS TO MINUSCULAS
              END-IF
           END-IF

           IF CWGETL NOT = 'OFF'
              SET CWSQLC-CLOSE TO TRUE
              CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           END-IF

           MOVE ARQUIVO TO LK-ARQUIVO(1:LEN)
           GOBACK.

       INSERT-PATH.

           IF   CWLOCATE = "ON"
           AND (ARQUIVO (2: 1) <> ":")
           AND (ARQUIVO (1: 1) <> "/")
           AND (ARQUIVO (1: 1) <> "\")
           AND (ARQUIVO        <> "cwused")
                MOVE SPACES TO ARQUIVO2
                STRING CURDIR DELIMITED BY SPACE
                       BARRA
                       ARQUIVO DELIMITED BY SIZE
                       INTO ARQUIVO2
                MOVE ARQUIVO2 TO ARQUIVO
           END-IF.

       FIM-INSERT-PATH. EXIT.

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
       END PROGRAM CWFILE.


