      $Set CallFH"FHREDIR" NoWriteLock Remove"QUEUE"
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWQUEUE.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  08/10/2014.
       SECURITY.      *************************************************
                      *                                               *
                      * Aplica fila de altera‡äes ao banco de dados   *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY QUEUE.SL REPLACING ==(FILENAME)== BY ==QUEUE==.
           COPY QUEUE.SL REPLACING ==(FILENAME)== BY ==TRASH==.

           SELECT LOG    ASSIGN TO DISK
                  ORGANIZATION  IS LINE SEQUENTIAL
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-LOG.

           SELECT CAL    ASSIGN TO DISK
                  ORGANIZATION  IS LINE SEQUENTIAL
                  FILE STATUS   IS FS-CAL.

           SELECT CMD    ASSIGN TO DISK
                  ORGANIZATION  IS LINE SEQUENTIAL
                  FILE STATUS   IS FS-CMD.

           SELECT stat ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS stat-chave
                  RESERVE NO ALTERNATE AREA
                  FILE STATUS   IS FS-stat.

      $Set IdxFormat"8" DataCompress"1"
           COPY QUEUE.SL REPLACING ==(FILENAME)== BY ==QUEUE8==.

       DATA DIVISION.
       FILE SECTION.

       FD  QUEUE
           LABEL RECORD IS STANDARD
           RECORD VARYING DEPENDING ON LEN-QUEUE
           VALUE OF FILE-ID IS LB-QUEUE.

       01  QUEUE-REG.
       COPY QUEUE.FD REPLACING ==(FILENAME)== BY ==QUEUE==.

       FD  TRASH
           LABEL RECORD IS STANDARD
           RECORD VARYING DEPENDING ON LEN-TRASH
           VALUE OF FILE-ID IS LB-TRASH.

       01  TRASH-REG.
       COPY QUEUE.FD REPLACING ==(FILENAME)== BY ==TRASH==.

       FD  LOG
           VALUE OF FILE-ID IS LB-LOG.

       01  LOG-REG.
           05 LOG-DATA        PIC X(11).
           05 LOG-HORA        PIC X(09).
           05 LOG-MSG.
              10 LOG-FS       PIC X(03).
              10 LOG-MENSAGEM PIC X(500).

       FD  CAL
           VALUE OF FILE-ID IS LB-CAL.

       01  CAL-REG           PIC X(523).

       FD  CMD
           VALUE OF FILE-ID IS LB-CMD.

       01  CMD-REG           PIC X(50).

       FD  stat
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-STAT.

       01  stat-REG.
           05 stat-CHAVE.
              10 stat-TABELA     PIC X(030).
              10 stat-LABEL      PIC X(066).
           05 stat-COUNT         PIC 9(018) COMP-3.

       FD  QUEUE8
           LABEL RECORD IS STANDARD
           RECORD VARYING DEPENDING ON LEN-QUEUE8
           VALUE OF FILE-ID IS LB-QUEUE8.

       01  QUEUE8-REG.
       COPY QUEUE.FD REPLACING ==(FILENAME)== BY ==QUEUE8==.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
       02  FS-COMMITS                PIC  X(002) VALUE '00'.
       02  TABELA                    PIC  X(050) VALUE SPACES.
       02  REMOVER                   PIC  X(030) VALUE SPACES.
       02  SAVE-CHAVE.
           10 SAVE-HANDLER           PIC  X(030) VALUE SPACES.
           10 SAVE-SEQUENCE   COMP-3 PIC  9(018) VALUE ZERO.
       02  FORMAT8                   PIC  X(002) VALUE SPACES.
       02  LIDOS              COMP-3 PIC  9(018) VALUE ZERO.
       02  LIDOS-TABELA       COMP-3 PIC  9(018) VALUE ZERO.
       02  MAXIMO             COMP-3 PIC  9(018) VALUE 10000.
       02  MAXIMO-TABELA      COMP-3 PIC  9(018) VALUE 1000.
       02  CWNUMERO                  PIC  X(018) VALUE SPACES.
       02  NUMERO                    PIC  9(018) VALUE ZERO.
       02  HOJE                      PIC  9(006) VALUE ZERO.
       02  AGORA                     PIC  9(008) VALUE ZERO.
       02  PAUSED                    PIC  9(001) VALUE ZERO.
       02  KILLED                    PIC  9(001) VALUE ZERO.
       02  I                  COMP-X PIC  9(004) VALUE ZERO.
       02  Y                  COMP-X PIC  9(004) VALUE ZERO.
       02  LIXO                      PIC  9(001) VALUE ZERO.
       02  PULAR                     PIC  9(001) VALUE ZERO.
       02  retry                     PIC  9(002) VALUE ZERO.
       02  LEN-QUEUE          COMP-X PIC  9(004) VALUE ZERO.
       02  LEN-QUEUE8         COMP-X PIC  9(004) VALUE ZERO.
       02  LEN-TRASH          COMP-X PIC  9(004) VALUE ZERO.
       02  LB-LOCK                   PIC  X(255) VALUE SPACES.
       02  LB-SAVE                   PIC  X(255) VALUE SPACES.
       02  ER-QUEUE.
           10 FS-QUEUE               PIC  X(002) VALUE "00".
           10 LB-QUEUE               PIC  X(255) VALUE SPACES.
           10 LB-QUEUE-BASICO        PIC  X(255) VALUE SPACES.
       02  ER-TRASH.
           10 FS-TRASH               PIC  X(002) VALUE "00".
           10 LB-TRASH               PIC  X(255) VALUE SPACES.
       02  ER-QUEUE8.
           10 FS-QUEUE8              PIC  X(002) VALUE "00".
           10 LB-QUEUE8              PIC  X(255) VALUE SPACES.
       02  ER-SQL.
           10 FS-SQL                 PIC  X(002) VALUE '00'.
           10 LB-SQL                 PIC  X(255) VALUE SPACES.
       02  ER-LOG.
           10 FS-LOG                 PIC  X(002) VALUE '00'.
           10 LB-LOG                 PIC  X(255) VALUE SPACES.
       02  ER-CAL.
           10 FS-CAL                 PIC  X(002) VALUE '00'.
           10 LB-CAL                 PIC  X(255) VALUE SPACES.
       02  ER-CMD.
           10 FS-CMD                 PIC  X(002) VALUE '00'.
           10 LB-CMD                 PIC  X(255) VALUE SPACES.
       02  FS-stat                   PIC  X(002) VALUE "00".
       02  LB-STAT                   PIC  X(255) VALUE SPACES.
       02     flag-stat              PIC  X(003) VALUE SPACES.
       02  SLEEP                     PIC  X(255) VALUE SPACES.
       02  SQLERRMC                  PIC  X(255) VALUE SPACES.
       02  CWQUEUESTOP               PIC  X(003) VALUE SPACES.
240517*02  CWQUEUETRUNCATE           PIC  X(003) VALUE SPACES.
       01  CASES. COPY CWCASE.
       01  cwqueue-POINTER.
           05 cwqueue-NUMBER         PIC  9(003).
           05 ESPACOS-1              PIC  X(001).
           05 cwqueue-KEY            PIC  X(030).
           05 cwqueue-FROM-STATUS    PIC  X(002).
           05 cwqueue-COMMIT-MODE    PIC  9(001).
           05 cwqueue-REC-FLAG       PIC  X(001) VALUE SPACES.
           05 cwqueue-REC-LENGTH     PIC  9(004) COMP-X.
           05 ESPACOS-2              PIC  X(343) VALUE SPACES.
           05 cwqueue-VOLUME         PIC  9(018) COMP-3.

       01  ws-REG.
           02 ws-FIXED.
              05 ws-CHAVE.
                 10 ws-HANDLER       PIC  X(030).
                 10 ws-SEQUENCE      PIC  9(018) COMP-3.
              05 ws-CWSQLC           PIC  X(018).
              05 ws-PROGRAM          PIC  X(030).
              05 ws-POINTER.
                 10 ws-REC-FLAG      PIC  X(001).
                 10 ws-REC-LENGTH    PIC  9(004) COMP-X.
                 10 ws-VOLUME        PIC  9(018) COMP-3.
              05 ws-NAME             PIC  X(031).
              05 ws-LABEL            PIC  X(224).
              05 ws-MAX-REC-LENGTH   PIC  9(004) COMP-X.
              05 ws-RELATIVE-KEY     PIC  9(009) COMP-X.
              05 ws-SESSION-ID       PIC  9(009) COMP-X.
              05 ws-BUFFER           PIC  X(32402).

       copy CWSQLC.

       PROCEDURE DIVISION.

       000-INICIO.

           CALL 'CWLONG' USING '$'
              ON EXCEPTION
                 CONTINUE
           END-CALL
           CALL "CWSQLT" USING "Set" '1'
           INITIALIZE ws-REG
           DISPLAY 'CWQUEUEINPUT' UPON ENVIRONMENT-NAME
           ACCEPT  LB-QUEUE       FROM ENVIRONMENT-VALUE

           IF  LB-QUEUE NOT = SPACES
               MOVE LB-QUEUE TO LB-QUEUE-BASICO
               DISPLAY 'CWQUEUESTOP' UPON ENVIRONMENT-NAME
               ACCEPT  CWQUEUESTOP   FROM ENVIRONMENT-VALUE
               INSPECT CWQUEUESTOP CONVERTING 'on' TO 'ON'
240517*        DISPLAY 'CWQUEUETRUNCATE' UPON ENVIRONMENT-NAME
240517*        ACCEPT  CWQUEUETRUNCATE   FROM ENVIRONMENT-VALUE
240517*        INSPECT CWQUEUETRUNCATE CONVERTING 'on' TO 'ON'
               STRING LB-QUEUE DELIMITED BY '.'
                 INTO LB-SQL
               STRING LB-SQL   DELIMITED BY SPACE
                      '.log'   DELIMITED BY SIZE
                 INTO LB-LOG
               STRING LB-SQL   DELIMITED BY SPACE
                      '.call'  DELIMITED BY SIZE
                 INTO LB-CAL
               STRING LB-SQL   DELIMITED BY SPACE
                      '.lock'  DELIMITED BY SIZE
                 INTO LB-lock
               STRING LB-SQL   DELIMITED BY SPACE
                      '.cmd'   DELIMITED BY SIZE
                 INTO LB-CMD
               DISPLAY 'CWQUEUESTAT' UPON ENVIRONMENT-NAME
               ACCEPT  flag-stat   FROM ENVIRONMENT-VALUE
               INSPECT flag-stat   CONVERTING MINUSCULAS TO MAIUSCULAS
               IF flag-stat = 'ON'
                 STRING LB-SQL   DELIMITED BY SPACE
                      '_stat'  DELIMITED BY SIZE
                 INTO LB-stat
                 OPEN I-O stat
                 IF FS-STAT > '09'
                    DELETE FILE stat
                    OPEN I-O stat
                 END-IF
                 IF FS-STAT > '09'
                    MOVE 'OFF' TO flag-stat
                 END-IF
               END-IF
               OPEN EXTEND LOG
               IF   FS-LOG > '09'
                    GOBACK
               END-IF
               PERFORM TEST AFTER UNTIL (FS-QUEUE NOT = '9A')
                                    AND (FS-QUEUE NOT = '9#')
                       OPEN INPUT QUEUE
                       IF FS-QUEUE = '9A' OR '9#'
                          CALL 'system' USING Z'sleep 5'
                       END-IF
               END-PERFORM
               IF  NOT (FS-QUEUE = '35' OR '30')
               AND FS-QUEUE > '09'
                   MOVE ER-QUEUE TO ER-SQL
                   PERFORM GRAVA-LOG THRU FIM-GRAVA-LOG
                   CLOSE LOG
                   GOBACK
               END-IF
               DISPLAY 'CWFORMAT8' UPON ENVIRONMENT-NAME
               ACCEPT   FORMAT8    FROM ENVIRONMENT-VALUE
               INSPECT  FORMAT8    CONVERTING MINUSCULAS TO MAIUSCULAS
               IF  FS-QUEUE = '35' OR '30'
                   PERFORM TEST AFTER UNTIL (FS-QUEUE NOT = '9A')
                                        AND (FS-QUEUE NOT = '9#')
                           IF FORMAT8 = 'ON'
                              MOVE LB-QUEUE TO LB-QUEUE8
                              OPEN I-O QUEUE8
                              IF FS-QUEUE8 = '9­'
                                 MOVE 'OFF' TO FORMAT8
                              ELSE
                                 CLOSE QUEUE8
                              END-IF
                           END-IF
                           OPEN I-O QUEUE
                           IF FS-QUEUE = '9A' OR '9#'
                              CALL 'system' USING Z'sleep 5'
                           END-IF
                   END-PERFORM
                   IF  FS-QUEUE > '09'
                       MOVE ER-QUEUE TO ER-SQL
                       PERFORM GRAVA-LOG THRU FIM-GRAVA-LOG
                       CLOSE LOG
                       GOBACK
                   END-IF
               ELSE
                   CLOSE QUEUE
                   OPEN I-O QUEUE
               END-IF
               CALL 'CWCMTS' USING 'O'
           ELSE
              GOBACK
           END-IF

           DISPLAY "CWQUEUECOMMIT" UPON ENVIRONMENT-NAME
           PERFORM AJUSTA THRU FIM-AJUSTA

           IF  NUMERO NOT = 0
               MOVE NUMERO TO MAXIMO
           END-IF

           IF   MAXIMO NOT = 0
                SUBTRACT 1 FROM MAXIMO
           END-IF

           DISPLAY "CWQUEUELIMIT" UPON ENVIRONMENT-NAME
           PERFORM AJUSTA THRU FIM-AJUSTA

           IF  NUMERO NOT = 0
               MOVE NUMERO TO MAXIMO-TABELA
           END-IF

           IF   MAXIMO-TABELA NOT = ZERO
                SUBTRACT 1 FROM MAXIMO-TABELA
           END-IF

           DISPLAY 'CWQUEUESTATUS' UPON ENVIRONMENT-NAME
           DISPLAY '*'             UPON ENVIRONMENT-VALUE
           DISPLAY 'CWQUEUESLEEP'  UPON ENVIRONMENT-NAME
           ACCEPT  SLEEP           FROM ENVIRONMENT-VALUE

           IF  SLEEP NOT = SPACES
               MOVE SPACES TO ESPACOS-2
               STRING 'sleep ' DELIMITED BY SIZE
                      SLEEP    DELIMITED BY SPACE
                      X'00'    DELIMITED BY SIZE
                 INTO ESPACOS-2
               MOVE ESPACOS-2 TO SLEEP
               MOVE SPACES    TO ESPACOS-2
           END-IF

           PERFORM UNTIL 0 > 1
              IF LIDOS NOT = ZERO
                 PERFORM COMMITA THRU FIM-COMMITA
              END-IF
              MOVE SPACES TO TABELA
              OPEN INPUT CMD
              IF   FS-CMD < '10'
                   READ CMD INTO TABELA
                   CLOSE CMD
              END-IF
              INSPECT TABELA CONVERTING MINUSCULAS TO MAIUSCULAS
              IF   TABELA = 'STOP'
                   DELETE FILE CMD
                   CLOSE QUEUE
                   EXIT PERFORM
              END-IF
              IF   TABELA = 'PAUSE'
                   IF PAUSED = 0
                      CLOSE QUEUE
                      MOVE 1 TO PAUSED
                   END-IF
                   CALL 'system' USING z'sleep 5'
                   EXIT PERFORM CYCLE
              END-IF
              IF   TABELA = 'CONNECT'
                   CALL 'CWSQLC' USING 'CN'
                   DELETE FILE CMD
                   EXIT PERFORM CYCLE
              END-IF
              IF   PAUSED = 1
                   MOVE 0 TO PAUSED
                   PERFORM TEST AFTER UNTIL (FS-QUEUE NOT = '9A')
                                        AND (FS-QUEUE NOT = '9#')
                           OPEN I-O QUEUE
                           IF FS-QUEUE = '9A' OR '9#'
                              CALL 'system' USING Z'sleep 5'
                           END-IF
                   END-PERFORM
              END-IF
              IF TABELA(1:6) = 'REMOVE'
                 MOVE SPACES TO REMOVER
                 MOVE 0      TO Y
                 PERFORM VARYING I FROM 7 BY 1 UNTIL I > LENGTH TABELA
                         IF TABELA(I:1) NOT = SPACE
                         AND Y < 30
                            ADD 1 TO Y
                            MOVE TABELA(I:1) TO REMOVER(Y:1)
                         END-IF
                 END-PERFORM
                 MOVE LOW-VALUES TO QUEUE-CHAVE
                 MOVE REMOVER    TO QUEUE-HANDLER
                 START QUEUE KEY NOT LESS QUEUE-CHAVE
                 IF FS-QUEUE < '10'
                    PERFORM TEST AFTER UNTIL FS-QUEUE > '09'
                     READ QUEUE NEXT RECORD
                     IF  FS-QUEUE < '09'
                     AND QUEUE-HANDLER = REMOVER
                         DELETE QUEUE RECORD
                     ELSE
                         MOVE '10' TO FS-QUEUE
                     END-IF
                    END-PERFORM
                 END-IF
                 MOVE SPACES TO REMOVER TABELA
                 DELETE FILE CMD
              END-IF
              MOVE LOW-VALUES TO QUEUE-CHAVE
              START QUEUE KEY NOT LESS QUEUE-CHAVE
              IF  FS-QUEUE > '09'
                  IF FS-QUEUE = '23'
                     CLOSE QUEUE
240517               IF   CWQUEUESTOP = 'ON'
240517                    DELETE FILE QUEUE
240517                    EXIT PERFORM
240517*              ELSE
240517*                   IF  CWQUEUETRUNCATE = 'ON'
240517*                       OPEN OUTPUT QUEUE
240517*                       CLOSE QUEUE
240517*                   END-IF
240517               END-IF
                     DELETE FILE CAL
                     PERFORM TEST AFTER UNTIL FS-QUEUE < '10'
                             IF SLEEP NOT = SPACES
                                CALL 'system' USING SLEEP
                             END-IF
                             PERFORM TEST AFTER
                               UNTIL FS-QUEUE NOT = '9A'
                                     OPEN INPUT QUEUE
                             END-PERFORM
                             IF FS-QUEUE < '10'
                                CLOSE QUEUE
                                PERFORM TEST AFTER
                                  UNTIL (FS-QUEUE NOT = '9A')
                                    AND (FS-QUEUE NOT = '9#')
                                        OPEN I-O QUEUE
                                        IF FS-QUEUE = '9A' OR '9#'
                                           CALL 'system'
                                           USING Z'sleep 5'
                                        END-IF
                                END-PERFORM
                             ELSE
                                OPEN INPUT CMD
                                IF   FS-CMD < '10'
                                     CLOSE CMD
                                     EXIT PERFORM
                                END-IF
                             END-IF
                             IF  FS-QUEUE > '10'
                                 MOVE ER-QUEUE TO ER-SQL
                                 PERFORM GRAVA-LOG THRU FIM-GRAVA-LOG
                                 STOP RUN
                             END-IF
                     END-PERFORM
                  END-IF
              ELSE
                  PERFORM TEST AFTER UNTIL FS-QUEUE > '09'
                     IF RETRY = 0
                        READ QUEUE NEXT RECORD
                        IF  FS-QUEUE > '09'
                            EXIT PERFORM CYCLE
                        END-IF
                        IF  QUEUE-NAME NOT = ws-NAME
130717                      open input cmd
130717                      if  fs-cmd < '10'
130717                          close cmd
130717                          move '10' to fs-queue
130717                          exit perform cycle
130717                      end-if
                            IF  ws-NAME NOT = SPACES
                            OR  KILLED = 1
                                INITIALIZE CWSQLC
                                SET  CWSQLC-CLOSE  TO TRUE
                                MOVE 1             TO CWSQLC-ISAM
                                MOVE 0             TO KILLED
                                PERFORM CHAMA-ws THRU FIM-CHAMA-ws
                                IF ws-HANDLER NOT = 'CWSQLM'
                                   CANCEL ws-HANDLER
                                END-IF
                            END-IF
                            PERFORM LABEL-LIXO THRU FIM-LABEL-LIXO
                            MOVE    0            TO LIXO LIDOS
                                                    LIDOS-TABELA
                            MOVE    QUEUE-REG    TO ws-REG
                            OPEN INPUT TRASH
                            IF   FS-TRASH < '10'
                                 CLOSE TRASH
                                 MOVE 1 TO LIXO
                            ELSE
                                 MOVE QUEUE-CWSQLC TO CWSQLC
                                 OPEN OUTPUT CAL
                                 WRITE CAL-REG FROM TABELA
                                 CLOSE CAL
                                 IF  NOT CWSQLC-KILL
                                     SET     CWSQLC-UPDATE TO TRUE
                                     PERFORM CHAMA-ws    THRU
                                         FIM-CHAMA-ws
                                     IF  FS-SQL > '09'
                                         PERFORM GRAVA-LOG
                                            THRU FIM-GRAVA-LOG
                                         MOVE 1 TO LIXO
                                     END-IF
                                     MOVE 1 TO KILLED
                                 END-IF
                            END-IF
                            IF LIXO = 0
                               MOVE SPACES TO SQLERRMC
                               DISPLAY "SQLERRMC"
                                  UPON ENVIRONMENT-NAME
                               DISPLAY  SQLERRMC
                                  UPON ENVIRONMENT-VALUE
                            END-IF
                        END-IF
                        IF  LIXO = 1
                        AND PULAR = 0
                            PERFORM GRAVA-LIXO THRU FIM-GRAVA-LIXO
                        END-IF
                     END-IF
                     IF  LIXO = 0
                         MOVE QUEUE-REG    TO ws-REG
                         MOVE QUEUE-CWSQLC TO CWSQLC
                         MOVE 1            TO CWSQLC-ISAM
                         IF  CWSQLC-DELETE
                         OR  CWSQLC-REWRITE
                         OR  CWSQLC-WRITE
                             INITIALIZE CWSQLC
                             MOVE 1 TO CWSQLC-ISAM
                             SET CWSQLC-READ  TO TRUE
                             SET CWSQLC-EQUAL TO TRUE
                             PERFORM CHAMA-ws THRU FIM-CHAMA-ws
                             MOVE QUEUE-CWSQLC TO CWSQLC
                             IF FS-SQL < '10'
                                MOVE ws-POINTER TO QUEUE-POINTER
                                IF  CWSQLC-WRITE
                                    SET CWSQLC-REWRITE TO TRUE
                                END-IF
                             ELSE
                                IF  CWSQLC-REWRITE
                                    SET CWSQLC-WRITE TO TRUE
                                    MOVE '00' TO FS-SQL
                                ELSE
                                    IF  CWSQLC-WRITE
                                    AND FS-SQL = '23'
                                        MOVE '00' TO FS-SQL
                                    END-IF
                                END-IF
                             END-IF
                         ELSE
                              IF  CWSQLC-KILL
                                  SET CWSQLC-CLOSE TO TRUE
                                  PERFORM CHAMA-ws THRU FIM-CHAMA-ws
                                  IF FS-SQL = "9­"
                                     PERFORM GRAVA-LOG
                                        THRU FIM-GRAVA-LOG
                                     MOVE 1 TO LIXO
                                  END-IF
                                  SET CWSQLC-KILL TO TRUE
                              END-IF
                              MOVE '00' TO FS-SQL
                         END-IF
                         IF FS-SQL < '10'
                            MOVE 1            TO CWSQLC-ISAM
                            PERFORM HANDLER THRU FIM-HANDLER
                            IF   FS-SQL = '22'
                            AND  RETRY < 10
                                 ADD 1 TO RETRY
                                 CALL 'system' USING z'sleep 60'
                                 EXIT PERFORM CYCLE
                            END-IF
                            MOVE 0 TO RETRY
                            IF FS-SQL > '09'
                               PERFORM GRAVA-LOG THRU FIM-GRAVA-LOG
                               MOVE 1 TO LIXO
                            END-IF
                         END-IF
                     END-IF
                     IF PULAR = 0
                        CALL "CWCMTS" USING 'W' FS-COMMITS QUEUE-CHAVE
                     END-IF
                     ADD 1 TO LIDOS LIDOS-TABELA
                     IF LIDOS-TABELA > MAXIMO-TABELA
280717               OR PULAR = 1
                        MOVE QUEUE-HANDLER TO SAVE-HANDLER
                        MOVE HIGH-VALUES   TO SAVE-SEQUENCE(1:)
                        PERFORM COMMITA  THRU FIM-COMMITA
                        MOVE SAVE-CHAVE    TO QUEUE-CHAVE
                        START QUEUE KEY NOT LESS QUEUE-CHAVE
                        MOVE 0             TO LIDOS-TABELA
280717                                        PULAR
                     END-IF
                     IF LIDOS > MAXIMO
                        PERFORM COMMITA THRU FIM-COMMITA
                     END-IF
130717               if lidos = zero
130717                  open input cmd
130717                  if  fs-cmd < '10'
130717                      close cmd
130717                      move '10' to fs-queue
130717                  end-if
130717               end-if
                  END-PERFORM
              END-IF
           END-PERFORM
           CLOSE LOG
           CALL 'CWCMTS' USING 'C'
           DELETE FILE CAL
           MOVE SPACES TO TABELA
           OPEN INPUT LOG
           READ LOG INTO TABELA
           CLOSE LOG
           IF   TABELA = SPACES
                DELETE FILE LOG
           END-IF.

       000-99-FIM. GOBACK.

       CHAMA-ws.

           INITIALIZE cwqueue-POINTER
           MOVE SPACES         TO ws-buffer
           MOVE QUEUE-BUFFER(1:QUEUE-REC-LENGTH)
                               TO ws-buffer(1:QUEUE-REC-LENGTH)
           MOVE ws-NAME        TO LB-SQL
           MOVE ws-LABEL       TO LB-SQL(32:)
           MOVE ws-REC-FLAG    TO cwqueue-REC-FLAG
           MOVE ws-REC-LENGTH  TO cwqueue-REC-LENGTH
           MOVE ws-VOLUME      TO cwqueue-VOLUME
           MOVE 0              TO CWSQLC-KEY pular
           CALL ws-HANDLER  USING CWSQLC
                                  ws-BUFFER(1:ws-MAX-REC-LENGTH)
                                  ER-SQL
                                  cwqueue-POINTER
                                  LB-SQL
                                  ws-MAX-REC-LENGTH
                                  ws-RELATIVE-KEY
                                  ws-SESSION-ID
                ON EXCEPTION
                   MOVE SPACES TO SQLERRMC
                   STRING 'Manipulador SQL(' DELIMITED BY SIZE
                          QUEUE-HANDLER      DELIMITED BY SPACE
                          ') indispon¡vel'   DELIMITED BY SIZE
                     INTO SQLERRMC
                   DISPLAY "SQLERRMC" UPON ENVIRONMENT-NAME
                   DISPLAY  SQLERRMC  UPON ENVIRONMENT-VALUE
                   MOVE '9­' TO FS-SQL
           END-CALL

           IF (CWSQLC-KILL OR CWSQLC-CLOSE)
           AND FS-SQL = '9d'
               DISPLAY "SQLERRMC" UPON ENVIRONMENT-NAME
               MOVE SPACES TO SQLERRMC
               ACCEPT   SQLERRMC  FROM ENVIRONMENT-VALUE
               IF SQLERRMC (1: 9) = 'ORA-00942'
                  MOVE '00' TO FS-SQL
               END-IF
           END-IF

           IF  FS-SQL = '9D'
               DISPLAY "SQLERRMC" UPON ENVIRONMENT-NAME
               MOVE SPACES TO SQLERRMC
               ACCEPT   SQLERRMC  FROM ENVIRONMENT-VALUE
               IF SQLERRMC (1: 9) = 'ORA-00904'
                  exit paragraph
               END-IF
               MOVE SPACES TO LOG-REG
               STRING QUEUE-HANDLER DELIMITED BY SPACE
                      ' Waiting for Oracle locking...' DELIMITED BY SIZE
                  INTO LOG-MENSAGEM
               PERFORM INSERT-ORA THRU FIM-INSERT-ORA
               move    1            to pular
               PERFORM CAL-MSG    THRU FIM-CAL-MSG
               CALL 'system' USING z'sleep 60'
               exit paragraph
           END-IF

           IF  FS-SQL = '35'
               DISPLAY "SQLERRMC" UPON ENVIRONMENT-NAME
               ACCEPT   SQLERRMC  FROM ENVIRONMENT-VALUE
               IF SQLERRMC (1: 9) = 'ORA-03114'
                  MOVE SPACES TO LOG-REG
                  STRING QUEUE-HANDLER DELIMITED BY SPACE
                      ' Waiting for Oracle locking...' DELIMITED BY SIZE
                  INTO LOG-MENSAGEM
                  PERFORM INSERT-ORA THRU FIM-INSERT-ORA
                  move    1            to pular
                  PERFORM CAL-MSG THRU FIM-CAL-MSG
                  CALL 'system' USING Z'sleep 240'
                  CALL 'CWSQLC' USING 'CN'
                  exit paragraph
               END-IF
           END-IF.

       FIM-CHAMA-ws. EXIT.

       HANDLER.

           MOVE SPACES            TO ws-buffer
           MOVE QUEUE-BUFFER(1:QUEUE-REC-LENGTH)
                                  TO ws-buffer(1:QUEUE-REC-LENGTH)
           MOVE QUEUE-NAME        TO LB-SQL
           MOVE QUEUE-LABEL       TO LB-SQL(32:)
           MOVE QUEUE-REC-FLAG    TO cwqueue-REC-FLAG
           MOVE QUEUE-REC-LENGTH  TO cwqueue-REC-LENGTH
           MOVE QUEUE-VOLUME      TO cwqueue-VOLUME
           MOVE 0                 TO CWSQLC-KEY pular
           CALL QUEUE-HANDLER  USING CWSQLC
                             ws-BUFFER(1:QUEUE-MAX-REC-LENGTH)
                             ER-SQL
                             cwqueue-POINTER
                             LB-SQL
                             QUEUE-MAX-REC-LENGTH
                             QUEUE-RELATIVE-KEY
                ON EXCEPTION
                   MOVE SPACES TO SQLERRMC
                   STRING 'Manipulador SQL(' DELIMITED BY SIZE
                          QUEUE-HANDLER      DELIMITED BY SPACE
                          ') indispon¡vel'   DELIMITED BY SIZE
                     INTO SQLERRMC
                   DISPLAY "SQLERRMC" UPON ENVIRONMENT-NAME
                   DISPLAY  SQLERRMC  UPON ENVIRONMENT-VALUE
                   MOVE '9­' TO FS-SQL
           END-CALL

           IF  CWSQLC-KILL
           AND FS-SQL = '9d'
               DISPLAY "SQLERRMC" UPON ENVIRONMENT-NAME
               MOVE SPACES TO SQLERRMC
               ACCEPT   SQLERRMC  FROM ENVIRONMENT-VALUE
               IF SQLERRMC (1: 9) = 'ORA-00942'
                  MOVE '00' TO FS-SQL
                  exit paragraph
               END-IF
           END-IF

           if  flag-stat = 'ON'
           and fs-SQL < '10'
           and (not CWSQLC-COMMIT)
           and (not CWSQLC-KILL)
               MOVE QUEUE-HANDLER TO stat-TABELA
               MOVE SPACES TO stat-LABEL
               READ stat
               IF FS-stat = '23'
                  MOVE 1 TO stat-COUNT
                  WRITE stat-reg
               ELSE
                  ADD 1 TO stat-COUNT
                  REWRITE stat-reg
               END-IF
               IF FS-stat > '09'
                  MOVE 'OFF' TO flag-stat
               else
                  MOVE QUEUE-LABEL TO stat-LABEL
                  READ stat
                  IF FS-stat = '23'
                     MOVE 1 TO stat-COUNT
                     WRITE stat-reg
                  ELSE
                     ADD 1 TO stat-COUNT
                     REWRITE stat-reg
                  END-IF
                  IF FS-stat > '09'
                     MOVE 'OFF' TO flag-stat
                  END-IF
               END-IF
           end-if

           IF  FS-SQL = '9D'
               DISPLAY "SQLERRMC" UPON ENVIRONMENT-NAME
               MOVE SPACES TO SQLERRMC
               ACCEPT   SQLERRMC  FROM ENVIRONMENT-VALUE
               IF SQLERRMC (1: 9) = 'ORA-00904'
                  exit paragraph
               END-IF
               MOVE SPACES TO LOG-REG
               STRING QUEUE-HANDLER DELIMITED BY SPACE
                      ' Waiting for Oracle locking...' DELIMITED BY SIZE
                  INTO LOG-MENSAGEM
               PERFORM INSERT-ORA THRU FIM-INSERT-ORA
               move    1            to pular
               PERFORM CAL-MSG    THRU FIM-CAL-MSG
               CALL 'system' USING z'sleep 60'
               exit paragraph
           END-IF

           IF  FS-SQL = '35'
               DISPLAY "SQLERRMC" UPON ENVIRONMENT-NAME
               ACCEPT   SQLERRMC  FROM ENVIRONMENT-VALUE
               IF SQLERRMC (1: 9) = 'ORA-03114'
                  MOVE SPACES TO LOG-REG
                  STRING QUEUE-HANDLER DELIMITED BY SPACE
                      ' Waiting for Oracle locking...' DELIMITED BY SIZE
                  INTO LOG-MENSAGEM
                  PERFORM INSERT-ORA THRU FIM-INSERT-ORA
                  move    1            to pular
                  PERFORM CAL-MSG THRU FIM-CAL-MSG
                  CALL 'system' USING Z'sleep 240'
                  CALL 'CWSQLC' USING 'CN'
                  exit paragraph
               END-IF
           END-IF.

       FIM-HANDLER. EXIT.

       GRAVA-LOG.

           MOVE SPACES TO LOG-REG
           ACCEPT HOJE  FROM DATE
           ACCEPT AGORA FROM TIME
           STRING HOJE(5:2) '/' HOJE(3:2) '/20' HOJE(1:2)
                  DELIMITED BY SIZE
             INTO LOG-DATA
           STRING AGORA(1:2) ':' AGORA(3:2) ':' AGORA(5:2)
                  DELIMITED BY SIZE
             INTO LOG-HORA
           MOVE FS-SQL TO LOG-FS
           MOVE LB-SQL TO LOG-MENSAGEM
           PERFORM INSERT-ORA THRU FIM-INSERT-ORA
           INSPECT LOG-REG CONVERTING X'000D0A' TO X'202020'
           WRITE LOG-REG
           IF   CWQUEUESTOP = 'ON'
                CLOSE LOG CAL
                DELETE FILE CAL
                STOP RUN
           END-IF.

       FIM-GRAVA-LOG. EXIT.

       GRAVA-LIXO.

200417*    PERFORM LABEL-LIXO THRU FIM-LABEL-LIXO
200417*
           PERFORM TEST AFTER UNTIL FS-TRASH < '10'
                   PERFORM TEST AFTER UNTIL (FS-TRASH NOT = '9A')
                                        AND (FS-TRASH NOT = '9#')
                           IF FORMAT8 = 'ON'
                              MOVE LB-TRASH TO LB-QUEUE8
                              OPEN I-O QUEUE8
                              IF FS-QUEUE8 = '9­'
                                 MOVE 'OFF' TO FORMAT8
                              ELSE
                                 CLOSE QUEUE8
                              END-IF
                           END-IF
                           OPEN I-O TRASH
                           IF FS-TRASH = '9A' OR '9#'
                              CALL 'system' USING Z'sleep 5'
                           END-IF
                   END-PERFORM
                   IF FS-TRASH > "09"
                      MOVE ER-TRASH TO ER-SQL
                      PERFORM GRAVA-LOG THRU FIM-GRAVA-LOG
                   END-IF
           END-PERFORM

           IF FS-TRASH < '10'
              PERFORM TEST AFTER
                UNTIL FS-TRASH NOT = '22'
                      MOVE HIGH-VALUES   TO TRASH-CHAVE(1:)
                      MOVE QUEUE-HANDLER TO TRASH-HANDLER
                      START TRASH
                            KEY NOT GREATER TRASH-CHAVE
                      IF   FS-TRASH = '23'
                           MOVE 1 TO TRASH-SEQUENCE
                      ELSE
                           READ TRASH PREVIOUS RECORD IGNORE LOCK
                           IF  FS-TRASH < '10'
                               ADD  1 TO TRASH-SEQUENCE
                           ELSE
                               MOVE 1 TO TRASH-SEQUENCE
                           END-IF
                      END-IF
                      IF   FS-TRASH < '11'
                      OR   FS-TRASH = '23'
                           MOVE LEN-QUEUE TO LEN-TRASH
                           COMPUTE I = LENGTH TRASH-CHAVE + 1
                           COMPUTE Y = LEN-QUEUE - I
                           MOVE QUEUE-REG(I:Y) TO TRASH-REG(I:Y)
                           WRITE TRASH-REG
                      END-IF
              END-PERFORM
              IF  FS-TRASH > '09'
              AND (FS-TRASH NOT = '22')
                  MOVE ER-TRASH TO ER-SQL
                  PERFORM GRAVA-LOG THRU FIM-GRAVA-LOG
                  CLOSE TRASH
                  GO TO GRAVA-LIXO
              END-IF
           END-IF
           CLOSE TRASH.

       FIM-GRAVA-LIXO. EXIT.

       LABEL-LIXO.

           MOVE SPACES TO LB-TRASH

           IF   QUEUE-HANDLER(1:7) = 'ORACLE-' OR 'oracle-'
                MOVE QUEUE-HANDLER(8:) TO TABELA
           ELSE
                MOVE QUEUE-HANDLER TO TABELA
           END-IF

           IF   TABELA = 'CWSQLM'
                MOVE QUEUE-NAME(2:) TO TABELA
                INSPECT TABELA CONVERTING ']' TO SPACE
           END-IF

           STRING LB-QUEUE-BASICO DELIMITED BY SPACE
                  '-'    DELIMITED BY SIZE
                  TABELA DELIMITED BY SPACE
             INTO LB-TRASH.

       FIM-LABEL-LIXO. EXIT.

       COMMITA.

           SET CWSQLC-COMMIT TO TRUE
           PERFORM HANDLER THRU FIM-HANDLER
           MOVE ZERO TO LIDOS
           CALL "CWCMTS" USING 'S'
           PERFORM TEST AFTER UNTIL FS-COMMITS > '10'
                   CALL "CWCMTS" USING 'R' FS-COMMITS QUEUE-CHAVE
                   IF FS-COMMITS < '10'
                      READ QUEUE
                           IF FS-QUEUE < '10'
                              DELETE QUEUE RECORD
                           END-IF
                   END-IF
           END-PERFORM
           CALL "CWCMTS" USING 'C'
           CALL "CWCMTS" USING 'O'.

       FIM-COMMITA. EXIT.

       AJUSTA.

           MOVE SPACES TO CWNUMERO
           MOVE 0      TO NUMERO
           ACCEPT CWNUMERO FROM ENVIRONMENT-VALUE
           MOVE 18 TO Y
           PERFORM VARYING I FROM LENGTH OF CWNUMERO BY -1
                   UNTIL I = 0
                   IF  CWNUMERO (I: 1) NUMERIC
                       MOVE CWNUMERO (I: 1)
                         TO   NUMERO (Y: 1)
                       SUBTRACT 1 FROM Y
                   END-IF
           END-PERFORM.

       FIM-AJUSTA. EXIT.

       CAL-MSG.

           ACCEPT HOJE  FROM DATE
           ACCEPT AGORA FROM TIME
           STRING HOJE(5:2) '/' HOJE(3:2) '/20' HOJE(1:2)
                  DELIMITED BY SIZE
             INTO LOG-DATA
           STRING AGORA(1:2) ':' AGORA(3:2) ':' AGORA(5:2)
                  DELIMITED BY SIZE
             INTO LOG-HORA
           MOVE FS-SQL TO LOG-FS
           IF PULAR = 1
              MOVE LB-CAL  TO LB-SAVE
              MOVE LB-LOCK TO LB-CAL
           END-IF
           OPEN OUTPUT CAL
           WRITE CAL-REG FROM LOG-REG
           CLOSE CAL
           IF PULAR = 1
              MOVE LB-SAVE TO LB-CAL
           END-IF.

       FIM-CAL-MSG. EXIT.

       INSERT-ORA.

           DISPLAY "SQLERRMC" UPON ENVIRONMENT-NAME
           MOVE SPACES TO SQLERRMC
           ACCEPT   SQLERRMC  FROM ENVIRONMENT-VALUE
           IF SQLERRMC NOT = SPACES
              PERFORM VARYING I FROM LENGTH LOG-MENSAGEM BY -1
                       UNTIL I = 1
                          OR LOG-MENSAGEM(I:1) NOT = SPACE
                        CONTINUE
              END-PERFORM
              ADD 2 TO I
              MOVE SQLERRMC TO LOG-MENSAGEM(I:)
              MOVE SPACES TO SQLERRMC
              DISPLAY SQLERRMC UPON ENVIRONMENT-VALUE
           END-IF.

       FIM-INSERT-ORA. EXIT.
      $Set CallFH"EXTFH"
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWCMTS.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  07/04/2017.
       SECURITY.      *************************************************
                      *                                               *
                      * Manipula controle de commits sem FileShare    *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      $Set IdxFormat"14" DataCompress"1" KeyCompress"7"
           SELECT COMMITS ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS COMMITS-CHAVE
                  LOCK MODE     IS MANUAL
                  FILE STATUS   IS FS-COMMITS.

       DATA DIVISION.
       FILE SECTION.

       FD  COMMITS
           VALUE OF FILE-ID IS LB-COMMITS.

       01  COMMITS-REG.
           05 COMMITS-CHAVE.
              10 COMMITS-HANDLER       PIC  X(030).
              10 COMMITS-SEQUENCE      PIC  9(018) COMP-3.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
       02  ER-COMMITS.
           10 FS-COMMITS         PIC X(002) VALUE '00'.
           10 LB-COMMITS         PIC X(255) VALUE 'cwqueue-$$$'.

       LINKAGE SECTION.

       01   FUNCAO PIC X.
       01   FS     PIC XX.
       01   QUEUE-CHAVE.
            10 QUEUE-HANDLER       PIC  X(030).
            10 QUEUE-SEQUENCE      PIC  9(018) COMP-3.

       PROCEDURE DIVISION USING FUNCAO FS QUEUE-CHAVE.

           EVALUATE FUNCAO
               WHEN 'O'
                    OPEN I-O COMMITS
               WHEN 'O'
                    CLOSE COMMITS
               WHEN 'S'
                    MOVE LOW-VALUES TO COMMITS-CHAVE
                    START COMMITS KEY NOT LESS COMMITS-CHAVE
               WHEN 'R'
                    READ COMMITS NEXT RECORD
                    MOVE FS-COMMITS    TO FS
                    MOVE COMMITS-CHAVE TO QUEUE-CHAVE
               WHEN 'W'
                    WRITE COMMITS-REG FROM QUEUE-CHAVE
           END-EVALUATE.

       END PROGRAM CWCMTS.
       END PROGRAM CWQUEUE.
