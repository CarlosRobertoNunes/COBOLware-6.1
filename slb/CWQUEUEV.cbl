      $Set CallFH"FHREDIR" NoWriteLock Remove"QUEUE"
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWQUEUEV.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  30/10/2014.
       SECURITY.      *************************************************
                      *                                               *
                      *  Contagem de entadas na fila                  *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY QUEUE.SL REPLACING ==(FILENAME)== BY ==QUEUE==.

           SELECT LISTA ASSIGN TO DISK
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS  IS FS-LISTA.

           SELECT CAL    ASSIGN TO DISK
                  ORGANIZATION  IS LINE SEQUENTIAL
                  FILE STATUS   IS FS-CAL.

       DATA DIVISION.
       FILE SECTION.

       FD  QUEUE
           LABEL RECORD IS STANDARD
           RECORD VARYING DEPENDING ON LEN-QUEUE
           VALUE OF FILE-ID IS LB-QUEUE.

       01  QUEUE-REG IS GLOBAL.
       COPY QUEUE.FD REPLACING ==(FILENAME)== BY ==QUEUE==.

       FD  LISTA
           VALUE OF FILE-ID LB-LISTA.

       01  LISTA-REG PIC X(80).

       FD  CAL
           VALUE OF FILE-ID IS LB-CAL.

       01  CAL-REG           PIC X(30).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
       02  ER-LISTA.
           10 FS-LISTA       PIC X(002) VALUE "00".
           10 LB-LISTA       PIC X(255) VALUE SPACES.
       02  ER-QUEUE.
           10 FS-QUEUE       PIC X(002) VALUE "00".
           10 LB-QUEUE       PIC X(255) VALUE SPACES.
       02  ER-CAL.
           10 FS-CAL         PIC X(002) VALUE '00'.
           10 LB-CAL         PIC X(255) VALUE SPACES.
       02  LEN-QUEUE         PIC 9(004) COMP-X.
       02  WS-HANDLER        PIC X(030) VALUE SPACES.
       02  TABELA            PIC X(030) VALUE SPACES.
       02  TABELA-ANTERIOR   PIC X(030) VALUE SPACES.
       02  LIDOS             PIC 9(018) COMP-3 VALUE ZERO.
       02  LIDOS-ED          PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.

       PROCEDURE DIVISION.

       000-INICIO.

           DISPLAY 'CWQUEUEFILE'  UPON ENVIRONMENT-NAME
           ACCEPT  LB-QUEUE       FROM ENVIRONMENT-VALUE
           IF LB-QUEUE NOT = SPACES
              STRING LB-QUEUE DELIMITED BY SPACE
                     '.lst'   DELIMITED BY SIZE
                INTO LB-LISTA
              DISPLAY 'CWQUEUELIST' UPON ENVIRONMENT-NAME
              ACCEPT  LB-LISTA      FROM ENVIRONMENT-VALUE
              STRING LB-QUEUE  DELIMITED BY SPACE
                     '.call' DELIMITED BY SIZE
               INTO LB-CAL
              OPEN INPUT QUEUE
              IF FS-QUEUE < '10'
                 OPEN OUTPUT LISTA
                 MOVE LOW-VALUES TO QUEUE-CHAVE
                 OPEN INPUT CAL
                 READ CAL
                 CLOSE CAL
                 MOVE SPACES TO TABELA-ANTERIOR
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
                              COMPUTE LIDOS-ED = QUEUE-SEQUENCE - LIDOS
                                                                + 1
                              MOVE SPACES TO LISTA-REG
                              IF QUEUE-HANDLER (1:7) = 'ORACLE-'
                                                    OR 'oracle-'
                                 MOVE QUEUE-HANDLER(8:) TO TABELA
                              ELSE
                                 MOVE QUEUE-HANDLER     TO TABELA
                              END-IF
                              IF TABELA = 'CWSQLM'
                                 MOVE QUEUE-NAME(2:) TO TABELA
                                 INSPECT TABELA CONVERTING ']' TO SPACE
                              END-IF
                              IF TABELA = CAL-REG
                                 MOVE SPACES TO QUEUE-HANDLER
                                 STRING CAL-REG DELIMITED BY SPACE
                                        '<----' DELIMITED BY SIZE
                                        INTO TABELA
                              END-IF
                              IF TABELA NOT = TABELA-ANTERIOR
                                 MOVE TABELA TO TABELA-ANTERIOR
                                 STRING LIDOS-ED ' ' TABELA
                                        DELIMITED BY SIZE INTO LISTA-REG
                                 WRITE LISTA-REG
                              END-IF
                           ELSE
                              READ QUEUE PREVIOUS RECORD IGNORE LOCK
                           END-IF
                        END-IF
                     END-IF
                 END-PERFORM
                 CLOSE QUEUE
                 WRITE LISTA-REG FROM LB-QUEUE
                 CLOSE LISTA
              END-IF
           END-IF.

       000-99-FIM. GOBACK.

       END PROGRAM CWQUEUEV.
