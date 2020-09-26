      $Set CallFH"FHREDIR" NoWriteLock Remove"QUEUE"
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWQUEUEM.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  14/07/2017.
       SECURITY.      *************************************************
                      *                                               *
                      *  Monitora fila transferindo excessos para     *
                      *  recarga em batch                             *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY QUEUE.SL REPLACING ==(FILENAME)== BY ==QUEUE==.

           SELECT REMOVE    ASSIGN TO DISK
                  ORGANIZATION  IS LINE SEQUENTIAL
                  FILE STATUS   IS FS-REMOVE.

       DATA DIVISION.
       FILE SECTION.

       FD  QUEUE
           LABEL RECORD IS STANDARD
           RECORD VARYING DEPENDING ON LEN-QUEUE
           VALUE OF FILE-ID IS LB-QUEUE.

       01  QUEUE-REG IS GLOBAL.
       COPY QUEUE.FD REPLACING ==(FILENAME)== BY ==QUEUE==.

       FD  REMOVE
           VALUE OF FILE-ID IS LB-REMOVE.

       01  REMOVE-REG           PIC X(50).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
       02  ER-QUEUE.
           10 FS-QUEUE       PIC X(002) VALUE "00".
           10 LB-QUEUE       PIC X(255) VALUE SPACES.
       02  ER-REMOVE.
           10 FS-REMOVE      PIC X(002) VALUE '00'.
           10 LB-REMOVE      PIC X(255) VALUE SPACES.
       02  LEN-QUEUE         PIC 9(004) COMP-X.
       02  WS-HANDLER        PIC X(030) VALUE SPACES.
       02  MAXIMO     COMP-3 PIC 9(018) VALUE 10000.
       02  CWNUMERO          PIC X(018) VALUE SPACES.
       02  NUMERO            PIC 9(018) VALUE 0.
       02  I                 PIC 9(004) COMP-X.
       02  Y                 PIC 9(004) COMP-X.
       02  TABELA            PIC X(050) VALUE SPACES.
       02  LETRA             PIC X(001) VALUE SPACE.
       02  LIDOS             PIC 9(018) COMP-3 VALUE ZERO.
       02  ACUMULADOS        PIC 9(018) COMP-3 VALUE ZERO.
       02  CMD               PIC X(255) VALUE SPACES.

       PROCEDURE DIVISION.

           DISPLAY 'LETRA'        UPON ENVIRONMENT-NAME
           ACCEPT  LETRA          FROM ENVIRONMENT-VALUE
           DISPLAY 'CWQUEUEFILE'  UPON ENVIRONMENT-NAME
           ACCEPT  LB-QUEUE       FROM ENVIRONMENT-VALUE
           STRING LB-QUEUE  DELIMITED BY SPACE
                  '.cmd' DELIMITED BY SIZE
            INTO LB-REMOVE
           IF LB-QUEUE = SPACES
           OR LETRA    = SPACES
              GOBACK
           ELSE
              OPEN INPUT QUEUE
              IF FS-QUEUE > '10'
                 GOBACK
              END-IF
           END-IF.
           DISPLAY "CWQUEUEMAX" UPON ENVIRONMENT-NAME
           PERFORM AJUSTA THRU FIM-AJUSTA

           IF  NUMERO NOT = 0
               MOVE NUMERO TO MAXIMO
           END-IF

           IF   MAXIMO NOT = 0
                SUBTRACT 1 FROM MAXIMO
           END-IF.

       000-INICIO.

           MOVE LOW-VALUES TO QUEUE-CHAVE
           START QUEUE KEY NOT LESS QUEUE-CHAVE
           PERFORM TEST AFTER UNTIL FS-QUEUE > '09'
              READ QUEUE NEXT RECORD IGNORE LOCK
               IF FS-QUEUE < '10'
                  MOVE QUEUE-HANDLER TO WS-HANDLER
                  MOVE QUEUE-SEQUENCE TO LIDOS
                  MOVE HIGH-VALUES TO QUEUE-CHAVE(31:)
                  START QUEUE KEY NOT GREATER QUEUE-CHAVE
                  READ QUEUE PREVIOUS RECORD IGNORE LOCK
                  IF FS-QUEUE < '10'
                     IF QUEUE-HANDLER = WS-HANDLER
                        COMPUTE ACUMULADOS = QUEUE-SEQUENCE
                                           - LIDOS + 1
                        IF ACUMULADOS > MAXIMO
                           MOVE SPACES          TO REMOVE-REG
                           STRING 'REMOVE ' DELIMITED BY SIZE
                                  QUEUE-HANDLER DELIMITED BY SPACE
                              INTO REMOVE-REG
                           OPEN OUTPUT REMOVE
                           IF FS-REMOVE < '10'
                              WRITE REMOVE-REG
                              CLOSE REMOVE
                              PERFORM TEST AFTER
                                UNTIL FS-REMOVE = '30' OR '35'
                                     OPEN INPUT REMOVE
                                     IF FS-REMOVE < '10'
                                        CLOSE REMOVE
                                        CALL 'system'
                                        USING Z'sleep 60'
                                     END-IF
                              END-PERFORM
                              IF QUEUE-HANDLER (1:7) = 'ORACLE-'
                                                    OR 'oracle-'
                                 MOVE QUEUE-HANDLER(8:) TO TABELA
                              ELSE
                                 MOVE QUEUE-HANDLER     TO TABELA
                              END-IF
                              IF TABELA = 'CWSQLM'
                                 MOVE QUEUE-NAME(2:) TO TABELA
                                 INSPECT TABELA CONVERTING ']'
                                     TO SPACE
                              END-IF
                              MOVE SPACES TO CMD
                              STRING 'nohup tooracle '
                                                 DELIMITED BY SIZE
                                     TABELA      DELIMITED BY ' '
                                     ' ' LETRA ' & echo OK'  X'00'
                                                 DELIMITED BY SIZE
                                    INTO CMD
                               CALL 'system' USING CMD
                           END-IF
                        END-IF
                     ELSE
                        READ QUEUE PREVIOUS RECORD IGNORE LOCK
                     END-IF
                  END-IF
               END-IF
           END-PERFORM
           CALL 'system' USING Z'sleep 60'
           GO TO 000-INICIO.

       000-99-FIM. GOBACK.

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

       END PROGRAM CWQUEUEM.
