      $Set WriteLock CallFH"FHREDIR"
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWLOCKV.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  31/05/2017.
       SECURITY.      *************************************************
                      *                                               *
                      * Lista posilicao de registros travados         *
                      * p/investigar dead locks.                      *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY LOCKS.sl.

           SELECT LIST  ASSIGN TO DISK
                  ORGANIZATION  IS LINE SEQUENTIAL
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-LIST.

      $Set IdxFormat"14"
           SELECT WORK  ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS WORK-CHAVE
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-WORK.

       DATA DIVISION.
       FILE SECTION.

       FD  LIST
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-LIST.

       01  LIST-REG PIC X(80).

       COPY LOCKS.fd.

       FD  WORK
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-WORK.

       01  WORK-REG.
           05 WORK-CHAVE.
              10 WORK-LABEL           PIC  X(080).
              10 WORK-LOCK            PIC  9(001).
                 88 WORK-LOCKING           VALUE 1.
                 88 WORK-WAITING           VALUE 2.
              10 WORK-USERNAME        PIC  X(030).
              10 WORK-DEVICE          PIC  X(030).
           05 WORK-STATUS             PIC  X(002).
              88 WORK-RECORD-LOCKED        VALUE 'D9'.
              88 WORK-FILE-LOCKED          VALUE 'A9'.
              88 WORK-WAITING-RECORD       VALUE '9D'.
              88 WORK-WAITING-FILE         VALUE '9A'.
              88 WORK-OPENED               VALUE '++'.
              88 WORK-CLOSED               VALUE '--'.
              88 WORK-UNLOCKED             VALUE '00'.

       WORKING-STORAGE SECTION.

       01  ER-LOCKS.
           05 FS-LOCKS            PIC  X(002) VALUE SPACES.
           05 LB-LOCKS            PIC  X(255) VALUE SPACES.
       01  ER-WORK.
           05 FS-WORK             PIC  X(002) VALUE SPACES.
           05 LB-WORK             PIC  X(255) VALUE 'cwlockv$.'.
       01  ER-LIST.
           05 FS-LIST             PIC  X(002) VALUE SPACES.
           05 LB-LIST
                  PIC  X(255) VALUE '$TEMP/CWLOCKV.######'.

       77  TTY-NOME               PIC  X(030) VALUE SPACES.
       77  MESMO                  PIC  9(001) VALUE ZERO.
       77  APAGAR                 PIC  9(001) VALUE ZERO.
       77  LOCK-A                 PIC  9(001) VALUE ZERO.
       77  TASK-Z                 PIC  Z(006).
       77  TRAVADOS               PIC  9(018) VALUE ZERO.
       77  ED                     PIC  ZZZ.ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.
       77  OPT                    PIC  X(006) VALUE SPACE.
       77  USERNAME               PIC  X(030) VALUE SPACES.
       77  DEVICE                 PIC  X(030) VALUE SPACES.
       77  LB                     PIC  X(080) VALUE SPACES.

       COPY CWLOCKV.lay.

       PROCEDURE DIVISION.

       000-INICIO.

           DISPLAY 'CWLOCKS' UPON ENVIRONMENT-NAME
           ACCEPT  LB-LOCKS FROM ENVIRONMENT-VALUE
           IF  LB-LOCKS = SPACES
               EXEC COBOLware Send
                    MESSAGE
                'Falta o nome do controle de locks(vari vel CWLOCKS)'
               END-EXEC
               GOBACK
           ELSE
               OPEN INPUT LOCKS
               IF  FS-LOCKS > '09'
                   EXEC COBOLware ISAM
                        STATUS FS-LOCKS
                        LABEL  LB-LOCKS
                   END-EXEC
                   GOBACK
               END-IF
           END-IF

           EXEC COBOLware FileName
                LABEL LB-LIST
           END-EXEC.

       RETRY.

           EXEC COBOLware BoxDialog
                LINE 08 COLUMN 04
                HEADER "Definir pesquisa, em branco para tudo:"
                Caption(1) "Arquivo"
                Caption(2) "Usu rio"
                Caption(3) "Terminal"
               Data(1) LB      ;LB       Size(1) 60
               Data(2) USERNAME;USERNAME Size(2) 30
               Data(3) DEVICE  ;DEVICE   Size(3) 30
                CANCEL;OPT
           END-EXEC
           IF   OPT = "Y"
                CLOSE LOCKS
                GOBACK
           END-IF
           OPEN I-O WORK
      *    MOVE LB       TO LOCKS-LABEL
      *    MOVE USERNAME TO LOCKS-USERNAME
      *    MOVE DEVICE   TO LOCKS-DEVICE
           MOVE ZEROS    TO TRAVADOS
           INITIALIZE LOCKS-REG
           START LOCKS KEY NOT LESS LOCKS-CHAVE
           PERFORM UNTIL FS-LOCKS > '09'
                READ LOCKS NEXT RECORD
                   EVALUATE TRUE
                       WHEN FS-LOCKS = '10'
                            EXIT PERFORM
                       WHEN FS-LOCKS < '10'
                            EXIT PERFORM CYCLE
                       WHEN FS-LOCKS = '9D'
                            START LOCKS KEY GREATER LOCKS-CHAVE
                            ADD 1 TO TRAVADOS
                            IF (LB NOT = SPACES)
                            AND(LOCKS-LABEL NOT = LB)
                                EXIT PERFORM CYCLE
                            END-IF
                            IF  (USERNAME NOT = SPACES)
                            AND (LOCKS-USERNAME NOT = USERNAME)
                                EXIT PERFORM CYCLE
                            END-IF
                            IF  (DEVICE NOT = SPACES)
                            AND (LOCKS-DEVICE  NOT = DEVICE)
                                EXIT PERFORM CYCLE
                            END-IF
                            MOVE LOCKS-LABEL    TO WORK-LABEL
                            MOVE LOCKS-USERNAME TO WORK-USERNAME
                            MOVE LOCKS-DEVICE   TO WORK-DEVICE
                            MOVE LOCKS-STATUS   TO WORK-STATUS
                            SET WORK-WAITING TO TRUE
                            IF  WORK-RECORD-LOCKED
                            OR  WORK-FILE-LOCKED
                                SET WORK-LOCKING TO TRUE
                            END-IF
                            WRITE WORK-REG
                       WHEN OTHER
                            EXEC COBOLware ISAMerror
                                 STATUS FS-LOCKS
                                 LABEL  LB-LOCKS
                            END-EXEC
                      END-EVALUATE
           END-PERFORM

           MOVE 0          TO LOCK-A
           MOVE SPACE      TO LIST-LABEL
           OPEN OUTPUT LIST
           MOVE LOW-VALUES TO WORK-CHAVE
           START WORK KEY NOT LESS WORK-CHAVE
           IF  FS-WORK > '09'
               WRITE LIST-REG FROM 'Nada travado no momento'
               IF TRAVADOS NOT = 0
                  MOVE SPACES TO LIST-REG
                  MOVE TRAVADOS TO ED
                  STRING 'Existem ' ED
                  ' situa‡äes de travamendo no sistema,'
                    DELIMITED BY SIZE
                    INTO LIST-REG
                    EXEC COBOLware Pack String LIST-REG END-EXEC
                  WRITE LIST-REG
                  WRITE LIST-REG
                   FROM 'Mas nada referente ao pesquisado.'
                  IF  LB NOT = SPACES
                      MOVE 'Aquivo:  '  TO LIST-REG
                      MOVE LB           TO LIST-REG(9:)
                      WRITE LIST-REG
                  END-IF
                  IF  USERNAME NOT = SPACES
                      MOVE 'Usu rio: '  TO LIST-REG
                      MOVE USERNAME     TO LIST-REG(10:)
                      WRITE LIST-REG
                  END-IF
                  IF   DEVICE NOT = SPACES
                       MOVE 'Terminal:' TO LIST-REG
                       MOVE DEVICE      TO LIST-REG(11:)
                       WRITE LIST-REG
                  END-IF
               END-IF
           ELSE
               WRITE LIST-REG FROM LINHA-02
           END-IF

           PERFORM UNTIL FS-WORK > '09'
                READ WORK NEXT RECORD
                IF FS-WORK < '10'
                   IF WORK-LABEL NOT = LIST-LABEL
                      MOVE WORK-LABEL TO LIST-LABEL
                      WRITE LIST-REG FROM LINHA-01
                      MOVE 3 TO LOCK-A
                   END-IF
                   IF WORK-LOCK NOT = LOCK-A
                      MOVE WORK-LOCK TO LOCK-A
                      IF WORK-LOCKING
                         MOVE 'Travando:'  TO LIST-LOCK
                      ELSE
                         MOVE 'Esperando:' TO LIST-LOCK
                      END-IF
                   END-IF
                   MOVE WORK-USERNAME TO LIST-USERNAME
                   MOVE WORK-DEVICE   TO LIST-DEVICE
                   EVALUATE TRUE
                       WHEN WORK-RECORD-LOCKED
                         OR WORK-WAITING-RECORD
                            MOVE "Registro"  TO LIST-STATUS
                       WHEN WORK-FILE-LOCKED
                         OR WORK-WAITING-FILE
                            MOVE "Arquivo  " TO LIST-STATUS
                   END-EVALUATE
                   WRITE LIST-REG FROM LINHA-03
                   MOVE SPACES TO LIST-LOCK
                END-IF
           END-PERFORM

           CLOSE LIST WORK
           EXEC COBOLware Help Top
                FILE  LB-LIST
                TITLE "Travamento"
                HORIZONTAL-LENGTH 78
                VERTICAL-LENGTH   21
           END-EXEC
           DELETE FILE LIST.
           GO TO RETRY.

       000-99-FIM. GOBACK.

       END PROGRAM CWLOCKV.
