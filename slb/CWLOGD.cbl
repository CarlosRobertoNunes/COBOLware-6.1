      $Set CallFH"FHREDIR"
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWLOGD.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  25/03/2002.
       SECURITY.      *************************************************
                      *                                               *
                      *  Verifica usuarios conectados                 *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CWUSED ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS CWUSED-CHAVE
                  ALTERNATE RECORD KEY IS CWUSED-LISTA
                                          = CWUSED-USUARIO CWUSED-TASK
                  LOCK MODE     IS MANUAL
                  FILE STATUS   IS FS-CWUSED.

           SELECT HELPWK ASSIGN TO DISK
                  ORGANIZATION  IS LINE SEQUENTIAL
                  FILE STATUS   IS FS-HELPWK.

           SELECT HELPWK2 ASSIGN TO DISK
                  ORGANIZATION  IS LINE SEQUENTIAL
                  FILE STATUS   IS FS-HELPWK.

       DATA DIVISION.
       FILE SECTION.

       FD  CWUSED
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-CWUSED.

       01  CWUSED-REG.
           05 CWUSED-CHAVE.
              10 CWUSED-TASK               PIC  9(006) COMP-3.
           05 CWUSED-LOGIN-DATA            PIC  9(008) COMP-3.
           05 CWUSED-LOGIN-HORA            PIC  9(006) COMP-3.
           05 CWUSED-USUARIO               PIC  X(030).
           05 CWUSED-MODULO                PIC  X(008).
           05 CWUSED-COMPUTERNAME          PIC  X(025).

       FD  HELPWK
           VALUE OF FILE-ID LB-HELPWK.

       01  HELPWK-REG.
           05 HELPWK-TASK                  PIC  Z(006)B.
           05 HELPWK-USUARIO               PIC  X(016).
           05 HELPWK-DATA                  PIC  X(011).
           05 HELPWK-HORA                  PIC  X(009).
           05 HELPWK-MODULO                PIC  X(009).
           05 HELPWK-TEMPO                 PIC  X(010).
           05 HELPWK-COMPUTERNAME          PIC  X(016).

       FD  HELPWK2
           VALUE OF FILE-ID LB-HELPWK.

       01  HELPWK2-REG.
           05 HELPWK2-TASK                 PIC  9(006).
           05 HELPWK2-DATA                 PIC  9(008).
           05 HELPWK2-HORA                 PIC  9(006).
           05 HELPWK2-MODULO               PIC  X(008).
           05 HELPWK2-FLAG                 PIC  X(001).
           05 HELPWK2-USUARIO              PIC  X(030).
           05 HELPWK2-COMPUTERNAME         PIC  X(025).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 Z                        PIC  9(002) VALUE 0.
           05 ZZ                       PIC  9(002) VALUE 0.
           05 TECLA                    PIC  9(002) VALUE 0. COPY CWKEYS.
           05 LOCKOPT                  PIC  X(003) VALUE SPACES.
           05 BARRA                    PIC  X(001) VALUE SPACE.
           05 SAVE-REG                 PIC  X(080) VALUE SPACES.
           05 ER-CWUSED.
              10 FS-CWUSED             PIC  X(002) VALUE "00".
              10 LB-CWUSED             PIC  X(255) VALUE "cwused".
           05 ER-HELPWK.
              10 FS-HELPWK             PIC  X(002) VALUE "00".
              10 LB-HELPWK             PIC  X(255) VALUE SPACES.
           05 I                        PIC  9(002) VALUE 0.
           05 LOGADOS                  PIC  9(006) VALUE 0.
           05 LOGADOS-ED               PIC  ZZZ.ZZ9.
           05 TMP                      PIC  X(255) VALUE SPACES.
           05 TMP-LB                   PIC  X(012) VALUE "CW800000.TMP".
           05 NOME                     PIC  X(030) VALUE SPACES.
           05 TASK                     PIC  9(006) VALUE 0.
           05 PROGRAMA                 PIC  X(008) VALUE SPACES.
           05 X91-RESULT        COMP-X PIC  9(002) VALUE 0.
           05 X91-FUNCTION      COMP-X PIC  9(002) VALUE 16.
           05 X91-PARAMETER     COMP-X PIC  9(002) VALUE 0.

       01  LINHAS-DE-IMPRESSAO-CLIC.
       02  LINHA-01.
           05 FILLER                         PIC  X(055) VALUE
              "  Task Usu rio            Data      Hora   M¢dulo    Te".
           05 FILLER                         PIC  X(014) VALUE
              "mpo    Esta‡Æo".

       COPY CWUNIX.
       COPY CWHELP.
       COPY CWTIME.
       COPY CWREVS.

       LINKAGE SECTION.

       COPY CWLOGD.

       PROCEDURE DIVISION USING PARAMETROS-CWLOGD.

       000-INICIO.

           ON 1
              DISPLAY 'CWLOCK' UPON ENVIRONMENT-NAME
              ACCEPT  LOCKOPT  FROM ENVIRONMENT-VALUE
              INSPECT LOCKOPT  CONVERTING 'of' TO 'OF'.

           CALL X"91" USING X91-RESULT X91-FUNCTION X91-PARAMETER
           MOVE 0           TO LOGADOS
           CALL "CWUNIX" USING PARAMETROS-CWUNIX
           CALL "CWGETU" USING NOME TASK PROGRAMA '?'

           IF   X91-PARAMETER NOT = 0
                MOVE    6                TO Z
                PERFORM VARYING ZZ FROM 50 BY -1
                          UNTIL ZZ = 1 OR Z = 0
                        IF   CWLOGD-FILE (ZZ: 1) = "#" OR "$"
                             MOVE TASK (Z: 1)
                               TO CWLOGD-FILE (ZZ: 1)
                             SUBTRACT 1 FROM Z
                        END-IF
                END-PERFORM
                MOVE CWLOGD-FILE TO LB-HELPWK
           ELSE
               IF LOCKOPT = 'OFF'
                  Exec COBOLware Send                                        ^
                       MESSAGE
                       "Controle de usu rios desabilitado"
                  End-EXEC
                  GOBACK
                END-IF
                DISPLAY "TEMP"         UPON ENVIRONMENT-NAME
                ACCEPT  TMP            FROM ENVIRONMENT-VALUE
                IF   TMP = SPACES
                     DISPLAY "TMP"     UPON ENVIRONMENT-NAME
                     ACCEPT  TMP       FROM ENVIRONMENT-VALUE
                END-IF
                MOVE TASK (2: 5)  TO TMP-LB (4: 5)
                IF   TMP NOT = SPACE
                     IF   CWUNIX-ON
                          MOVE "/" TO BARRA
                     ELSE
                          MOVE "\" TO BARRA
                     END-IF
                     MOVE SPACES TO LB-HELPWK
                     STRING TMP    DELIMITED BY SPACE
                            BARRA  DELIMITED BY SIZE
                            TMP-LB DELIMITED BY SPACE
                       INTO LB-HELPWK
                ELSE
                     MOVE TMP-LB TO LB-HELPWK
                END-IF
           END-IF

           MOVE "cwused"    TO LB-CWUSED
           CALL "CWFILE" USING LB-CWUSED
           OPEN I-O CWUSED
           IF   LB-HELPWK NOT = SPACES
                IF   X91-PARAMETER = 0
                     OPEN OUTPUT HELPWK
                ELSE
                     OPEN OUTPUT HELPWK2
                END-IF
           END-IF
           IF   X91-PARAMETER = 0
                WRITE HELPWK-REG FROM LINHA-01
      *         WRITE HELPWK-REG FROM SPACES
           END-IF
           MOVE LOW-VALUES TO CWUSED-REG
           START CWUSED KEY NOT LESS CWUSED-LISTA
           PERFORM TEST AFTER UNTIL FS-CWUSED > "09"
              READ CWUSED NEXT RECORD IGNORE LOCK
                   IF   FS-CWUSED = "00"
                        IF CWUSED-TASK NOT = TASK
                           DELETE CWUSED RECORD
                        END-IF
                        IF   FS-CWUSED > "09"
                        OR   CWUSED-TASK = TASK
                             IF   CWUSED-TASK = TASK
                                  MOVE CWUSED-REG TO SAVE-REG
                             END-IF
                             PERFORM 110-GRAVA THRU 110-99-FIM
                             START CWUSED KEY GREATER CWUSED-LISTA
                             ADD 1 TO LOGADOS
                        END-IF
                   END-IF
           END-PERFORM
           CLOSE CWUSED
           CALL "CWLOCK" USING "R" CWUSED-USUARIO TASK
           IF   X91-PARAMETER = 0
      *         WRITE HELPWK-REG FROM SPACES
                IF  LOGADOS = 1
                    MOVE " 1 usu rio conectado" TO HELPWK-REG
                ELSE
                    MOVE LOGADOS TO LOGADOS-ED
                    PERFORM VARYING I FROM 1 BY 1
                            UNTIL LOGADOS-ED (I: 1) NOT = SPACE
                           CONTINUE
                    END-PERFORM
                    MOVE LOGADOS-ED (I: ) TO HELPWK-REG (2: )
                    COMPUTE I = LENGTH OF LOGADOS-ED - I + 4
                    MOVE "usu rios conectados" TO HELPWK-REG (I: )
                END-IF
      *         MOVE CWREVS-REVISAO TO HELPWK-REG (57: )
                WRITE HELPWK-REG
      *         WRITE HELPWK-REG FROM SPACES
           END-IF
           IF   LB-HELPWK NOT = SPACES
                IF   X91-PARAMETER = 0
                     CLOSE HELPWK
                ELSE
                     CLOSE HELPWK2
                END-IF
           END-IF
           IF   X91-PARAMETER = 0
      *         ADD  4         TO LOGADOS
                ADD  2         TO LOGADOS
                MOVE LB-HELPWK TO CWHELP-FILE
                MOVE 73        TO CWHELP-HORIZONTAL-LENGTH
                MOVE 10        TO CWHELP-LINE
                MOVE 03        TO CWHELP-COLUMN
                MOVE 10        TO CWHELP-VERTICAL-LENGTH
                IF   LOGADOS < CWHELP-VERTICAL-LENGTH
                     MOVE LOGADOS TO CWHELP-VERTICAL-LENGTH
                END-IF
                MOVE CWREVS-REVISAO TO CWHELP-TITLE
                CALL "CWHELP" USING PARAMETROS-CWHELP
                DELETE FILE HELPWK
           ELSE
                MOVE LOGADOS TO CWLOGD-USERS
           END-IF.

       000-99-FIM. GOBACK.

       110-GRAVA.

      *    IF  CWUSED-TASK = TASK
      *        UNLOCK CWUSED
      *        CALL "CWLOCK" USING "R" CWUSED-USUARIO
      *                                TASK
      *    END-IF
           IF   LB-HELPWK NOT = SPACES
                IF   X91-PARAMETER = 0
                     MOVE CWUSED-TASK       TO HELPWK-TASK
                     IF  CWUSED-TASK = TASK
                         MOVE "*"           TO HELPWK-TASK (7: 1)
                     END-IF
                     MOVE CWUSED-USUARIO    TO HELPWK-USUARIO
                     MOVE CWUSED-LOGIN-DATA TO CWTIME-DATE
                     MOVE CWUSED-LOGIN-HORA TO CWTIME-TIME
                     SET  CWTIME-REVERSED
                          CWTIME-EDIT TO TRUE
                     CALL "CWTIME" USING PARAMETROS-CWTIME
                     MOVE CWTIME-DATE-EDITED TO HELPWK-DATA
                     MOVE CWTIME-TIME-EDITED TO HELPWK-HORA
                     SET CWTIME-TODAY    TO TRUE
                     CALL "CWTIME" USING PARAMETROS-CWTIME
                     SET CWTIME-INTERVAL TO TRUE
                     MOVE CWUSED-LOGIN-DATA TO CWTIME-DATE
                     MOVE CWUSED-LOGIN-HORA TO CWTIME-TIME
                     CALL "CWTIME" USING PARAMETROS-CWTIME
                     MOVE SPACES             TO HELPWK-TEMPO
                     STRING CWTIME-TOTAL-HOURS (7: 3)
                            ":"
                            CWTIME-TOTAL-MINUTES
                            ":"
                            CWTIME-TOTAL-SECONDS
                            DELIMITED BY SIZE
                         INTO HELPWK-TEMPO
                     MOVE CWUSED-MODULO       TO HELPWK-MODULO
                     MOVE CWUSED-COMPUTERNAME TO HELPWK-COMPUTERNAME
                     WRITE HELPWK-REG
                ELSE
                     MOVE SPACES              TO HELPWK2-REG
                     MOVE CWUSED-TASK         TO HELPWK2-TASK
                     MOVE CWUSED-LOGIN-DATA   TO HELPWK2-DATA
                     MOVE CWUSED-LOGIN-HORA   TO HELPWK2-HORA
                     MOVE CWUSED-USUARIO      TO HELPWK2-USUARIO
                     MOVE CWUSED-MODULO       TO HELPWK2-MODULO
                     MOVE CWUSED-COMPUTERNAME TO HELPWK2-COMPUTERNAME
                     IF   CWUSED-TASK = TASK
                          MOVE "*" TO HELPWK2-FLAG
                     ELSE
                          MOVE SPACE TO HELPWK2-FLAG
                     END-IF
                     WRITE HELPWK2-REG
                END-IF
           END-IF.

       110-99-FIM. EXIT.

       END PROGRAM CWLOGD.
