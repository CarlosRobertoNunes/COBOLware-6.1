      $Set CallFH"FHREDIR" NoWriteLock Remove"QUEUE" Remove"TITLE"
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWQUEUES.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  03/08/2017.
       SECURITY.      *************************************************
                      *                                               *
                      *  Exibe estatistica de fila de espelhamento BD *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT stat ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS stat-chave
                  RESERVE NO ALTERNATE AREA
                  FILE STATUS   IS FS-stat.

           SELECT LIST  ASSIGN TO DISK
                  ORGANIZATION  IS LINE SEQUENTIAL
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-LIST.

       DATA DIVISION.
       FILE SECTION.

       FD  stat
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-STAT.

       01  stat-REG.
           05 stat-CHAVE.
              10 stat-TABELA     PIC X(030).
              10 stat-LABEL      PIC X(066).
           05 stat-COUNT         PIC 9(018) COMP-3.

       FD  LIST
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-LIST.

       01  LIST-REG PIC X(80).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
       02  LB-QUEUE              PIC  X(255) VALUE SPACES.
       02  FS-stat               PIC  X(002) VALUE "00".
       02  LB-STAT               PIC  X(255) VALUE SPACES.
       02  ER-LIST.
           05 FS-LIST             PIC  X(002) VALUE SPACES.
           05 LB-LIST
                  PIC  X(255) VALUE '$TEMP/CWQUEUES.######'.

       01  LINHAS-DE-IMPRESSAO-CLIC.
       02  LINHA-01.
           05 CLIC-FILE                      PIC  X(066) VALUE SPACES.
           05 CLIC-COUNT                     PIC  ZZZ.ZZZ.ZZ9.

       PROCEDURE DIVISION.

       000-INICIO.

           DISPLAY 'CWQUEUEFILE' UPON ENVIRONMENT-NAME
           ACCEPT  LB-QUEUE       FROM ENVIRONMENT-VALUE
           IF  LB-QUEUE NOT = SPACES
               STRING LB-QUEUE DELIMITED BY SPACE
                      '_stat'  DELIMITED BY SIZE
                 INTO LB-stat
               OPEN INPUT stat
               IF FS-STAT < '10'
                  EXEC COBOLware FileName
                       LABEL LB-LIST
                  END-EXEC
                  OPEN OUTPUT LIST
                  INITIALIZE STAT-REG
                  START STAT KEY NOT LESS STAT-CHAVE
                  PERFORM UNTIL FS-STAT > '09'
                       READ STAT NEXT RECORD
                       IF FS-STAT < '10'
                          IF stat-LABEL = SPACES
                             MOVE stat-TABELA TO CLIC-FILE
                          ELSE
                             MOVE stat-LABEL  TO CLIC-FILE
                          END-IF
                          MOVE  stat-COUNT    TO CLIC-COUNT
                          WRITE LIST-REG    FROM LINHA-01
                       END-IF
                  END-PERFORM
                  CLOSE STAT LIST
                  EXEC COBOLware Help
                       FILE  LB-LIST
                       TITLE LB-QUEUE
                       HORIZONTAL-LENGTH 78
                       VERTICAL-LENGTH   21
                  END-EXEC
                  DELETE FILE LIST
               END-IF
           ELSE
               EXEC COBOLware Send
                    MESSAGE
                'Falta o nome da fila (vari vel CWQUEUEFILE)'
               END-EXEC
               GOBACK
           END-IF.

       000-99-FIM. GOBACK.

       END PROGRAM CWQUEUES.
