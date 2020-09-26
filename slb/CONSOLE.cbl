      $SET MS(2) CallFH"FHREDIR"  Remove"CONSOLE"
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CONSOLE.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  15/05/1997.
       SECURITY.      *************************************************
                      *                                               *
                      *  Simulador de console de mainfarmes           *
                      *  Envia Mensagem para console c/opcao aguardar *
                      *  simulando STOP Literal                       *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA
                         CRT STATUS IS CRT-STATUS.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT WIN-CONSOLE ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  RECORD KEY    IS WIN-KEY
                  ALTERNATE key is win-RESP WITH DUPLICATES
                  ACCESS MODE   IS DYNAMIC
                  LOCK   MODE   IS MANUAL
                  FILE STATUS   IS FS-WIN-CONSOLE.

           SELECT WIN-TASK    ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  RECORD KEY    IS WIN-TASK-KEY
                  ACCESS MODE   IS DYNAMIC
                  LOCK   MODE   IS MANUAL
                  FILE STATUS   IS FS-WIN-TASK.

           SELECT WIN-LOCK    ASSIGN TO DISK
                  ORGANIZATION  IS LINE SEQUENTIAL
                  LOCK   MODE   IS EXCLUSIVE
                  FILE STATUS   IS FS-WIN-LOCK.

       DATA DIVISION.
       FILE SECTION.

       FD  WIN-CONSOLE
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-WIN-CONSOLE.

       01  WIN-CONSOLE-REG.
           05 WIN-KEY         PIC 9(018) COMP-3.
           05 WIN-LEN         PIC 9(002).
           05 WIN-MSG         PIC X(275).
           05 REDEFINES WIN-MSG.
              10 WIN-ID       PIC X(033).
              10 WIN-ACC      PIC X(242).
           05 WIN-RESP        PIC 9(001).
           05 WIN-MSG2        PIC X(001).

       FD  WIN-TASK
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-WIN-TASK.

       01  WIN-TASK-REG.
           05 WIN-TASK-KEY    PIC 9(008) COMP-3.
           05 WIN-TASK-STA    PIC X(001).
           05 WIN-TASK-ACC    PIC X(047).

       FD  WIN-LOCK
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-WIN-LOCK.

       01  WIN-LOCK-REG PIC X.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1.
           05 TECLA              PIC  9(003) VALUE 0. COPY CWKEYS.
           05 SAVE-PROGRAM       PIC  X(050) VALUE SPACES.
           05 ER-WIN-CONSOLE.
              10 FS-WIN-CONSOLE  PIC  X(002) VALUE "00".
              10 LB-WIN-CONSOLE  PIC  X(255) VALUE "consoleMSG".
           05 ER-WIN-TASK.
              10 FS-WIN-TASK     PIC  X(002) VALUE "00".
              10 LB-WIN-TASK     PIC  X(255) VALUE "consoleTASK".
           05 ER-WIN-LOCK.
              10 FS-WIN-LOCK     PIC  X(002) VALUE "00".
              10 LB-WIN-LOCK     PIC  X(255) VALUE "consoleLOCK".
           05 janlines           PIC  9(002) VALUE 23.
           05 L                  PIC  9(002) VALUE 0.
           05 I                  PIC  9(003) VALUE 0.
           05 NADA               PIC  X(001) VALUE SPACE.
           05 X91-RESULT        COMP-X PIC  9(002) VALUE 0.
           05 X91-FUNCTION      COMP-X PIC  9(002) VALUE 16.
           05 X91-PARAMETER     COMP-X PIC  9(002) VALUE 0.
           05 VEZ                      PIC  9(001) VALUE 1.
           05 JANCODE                  PIC  X(010) VALUE SPACES.
           05 THUMB-V                  PIC  9(005) VALUE 0.
           05 THUMB-VA                 PIC  9(005) VALUE 1.
           05 THUMB-H                  PIC  9(005) VALUE 0.
           05 THUMB-HA                 PIC  9(005) VALUE 2.
           05 MOSTRA                   PIC  X(077) VALUE SPACES.
           05 POS-INI       COMP-3 PIC  9(005) VALUE 0.
           05 POS           COMP-3 PIC  9(005) VALUE 1.
           05 POS-A         COMP-3 PIC  9(005) VALUE 0.
           05 JANLINS              PIC  9(002) VALUE 0.
           05 JANCOLS              PIC  9(002) VALUE 0.
           05    COLS              PIC  9(002) VALUE 78.
           05 COLUNAS       COMP-3 PIC  9(005) VALUE 0.
           05 PONTEIRO      COMP-3 PIC  9(018) VALUE 1.
           05 PONTEIRO-A    COMP-3 PIC  9(018) VALUE 1.
           05 LINHAS        COMP-3 PIC  9(018) VALUE 0.
           05 CRT-STATUS.
              10 t-tecla           pic  X(001).
              10 t2                pic  9(002) comp-x. copy cwedit.
              10 t3                pic  9(002) comp-x.
           05 QUEST-ARRAY.
              10 QUESTS            PIC  9(002).
              10 QUEST-KEY  COMP-3 PIC  9(018) OCCURS 21.
              10 LLS.
                 15 L01            PIC  9(002).
                 15 L02            PIC  9(002).
                 15 L03            PIC  9(002).
                 15 L04            PIC  9(002).
                 15 L05            PIC  9(002).
                 15 L06            PIC  9(002).
                 15 L07            PIC  9(002).
                 15 L08            PIC  9(002).
                 15 L09            PIC  9(002).
                 15 L10            PIC  9(002).
                 15 L11            PIC  9(002).
                 15 L12            PIC  9(002).
                 15 L13            PIC  9(002).
                 15 L14            PIC  9(002).
                 15 L15            PIC  9(002).
                 15 L16            PIC  9(002).
                 15 L17            PIC  9(002).
                 15 L18            PIC  9(002).
                 15 L19            PIC  9(002).
                 15 L20            PIC  9(002).
                 15 L21            PIC  9(002).
              10 REDEFINES LLS.
                 15 LN             PIC  9(002) OCCURS 21.
              10 Q-R        PIC  X(047) OCCURS 21.
              10 CLS.
                 15 C01            PIC  9(002).
                 15 C02            PIC  9(002).
                 15 C03            PIC  9(002).
                 15 C04            PIC  9(002).
                 15 C05            PIC  9(002).
                 15 C06            PIC  9(002).
                 15 C07            PIC  9(002).
                 15 C08            PIC  9(002).
                 15 C09            PIC  9(002).
                 15 C10            PIC  9(002).
                 15 C11            PIC  9(002).
                 15 C12            PIC  9(002).
                 15 C13            PIC  9(002).
                 15 C14            PIC  9(002).
                 15 C15            PIC  9(002).
                 15 C16            PIC  9(002).
                 15 C17            PIC  9(002).
                 15 C18            PIC  9(002).
                 15 C19            PIC  9(002).
                 15 C20            PIC  9(002).
                 15 C21            PIC  9(002).
              10 REDEFINES CLS.
                 15 CL             PIC  9(002) OCCURS 21.
              10 SZS.
                 15 S01            PIC  9(002).
                 15 S02            PIC  9(002).
                 15 S03            PIC  9(002).
                 15 S04            PIC  9(002).
                 15 S05            PIC  9(002).
                 15 S06            PIC  9(002).
                 15 S07            PIC  9(002).
                 15 S08            PIC  9(002).
                 15 S09            PIC  9(002).
                 15 S10            PIC  9(002).
                 15 S11            PIC  9(002).
                 15 S12            PIC  9(002).
                 15 S13            PIC  9(002).
                 15 S14            PIC  9(002).
                 15 S15            PIC  9(002).
                 15 S16            PIC  9(002).
                 15 S17            PIC  9(002).
                 15 S18            PIC  9(002).
                 15 S19            PIC  9(002).
                 15 S20            PIC  9(002).
                 15 S21            PIC  9(002).
              10 REDEFINES SZS.
                 15 SZ             PIC  9(002) OCCURS 21.

       01  TTY-WRITE.
           05 TTY-TERMINAL                   PIC  Z(008) VALUE 0.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 TTY-DIA                        PIC  9(002) VALUE ZEROS.
           05 FILLER                         PIC  X(001) VALUE "/".
           05 TTY-MES                        PIC  9(002) VALUE ZEROS.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 TTY-HORA                       PIC  9(002) VALUE ZEROS.
           05 FILLER                         PIC  X(001) VALUE ":".
           05 TTY-MINUTO                     PIC  9(002) VALUE ZEROS.
           05 FILLER                         PIC  X(001) VALUE ":".
           05 TTY-SEGUNDO                    PIC  9(002) VALUE ZEROS.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 TTY-STRING                     PIC  X(251) VALUE SPACES.

       COPY CWTIME.
       COPY CWNCOR.

       LINKAGE SECTION.

       COPY CONSOLE.

       01  LK-STOP-ACCT        PIC X(004).

       SCREEN SECTION.

       01  T21.
       02 line L21 column C21 pic x(47) using Q-R(21) size S21.
           02 T20.
       03 line L20 column C20 pic x(47) using Q-R(20) size S20.
           03 T19.
       04 line L19 column C19 pic x(47) using Q-R(19) size S19.
           04 T18.
       05 line L18 column C18 pic x(47) using Q-R(18) size S18.
           05 T17.
       06 line L17 column c17 pic x(47) using Q-R(17) size S17.
           06 T16.
       07 line L16 column c16 pic x(47) using Q-R(16) size S16.
           07 T15.
       08 line L15 column c15 pic x(47) using Q-R(15) size S15.
           08 T14.
       09 line L14 column c14 pic x(47) using Q-R(14) size S14.
           09 T13.
       10 line L13 column c13 pic x(47) using Q-R(13) size S13.
           10 T12.
       11 line L12 column c12 pic x(47) using Q-R(12) size S12.
           11 T11.
       12 line L11 column c11 pic x(47) using Q-R(11) size S11.
           12 T10.
       13 line L10 column c10 pic x(47) using Q-R(10) size S10.
           13 T09.
       14 line L09 column c09 pic x(47) using Q-R(09) size S09.
           14 T08.
       15 line L08 column c08 pic x(47) using Q-R(08) size S08.
           15 T07.
       16 line L07 column c07 pic x(47) using Q-R(07) size S07.
           16 T06.
       17 line L06 column c06 pic x(47) using Q-R(06) size S06.
           17 T05.
       18 line L05 column c05 pic x(47) using Q-R(05) size S05.
           18 T04.
       19 line L04 column c04 pic x(47) using Q-R(04) size S04.
           19 T03.
       20 line L03 column c03 pic x(47) using Q-R(03) size S03.
           20 T02.
       21 line L02 column c02 pic x(47) using Q-R(02) size S02.
           21 T01.
       22 line L01 column c01 pic x(47) using Q-R(01) size S01.

       PROCEDURE DIVISION USING PARAMETROS-CONSOLE LK-STOP-ACCT.

       000-INICIO.

           CALL X"91" USING X91-RESULT X91-FUNCTION X91-PARAMETER
           IF   X91-PARAMETER = 1
                continue
           else
                if  (LK-STOP-ACCT not = 'STOP')
                and (LK-STOP-ACCT not = 'ACCT')
                    go to operador
                end-if
           END-IF

           IF VEZ = 1
              MOVE 2                 TO VEZ
              SET CWTIME-TODAY       TO TRUE
              SET CWTIME-NORMAL      TO TRUE
              DELETE FILE WIN-TASK
              OPEN I-O WIN-TASK
              MOVE HIGH-VALUES TO WIN-TASK-REG
              START WIN-TASK KEY NOT GREATER WIN-TASK-KEY
              READ WIN-TASK PREVIOUS RECORD
              IF FS-WIN-TASK < '10' OR FS-WIN-TASK = '9D'
                 ADD 1 TO WIN-TASK-KEY
              ELSE
                 MOVE 1 TO WIN-TASK-KEY
              END-IF
              MOVE SPACES TO WIN-TASK-STA
                             WIN-TASK-ACC
              MOVE WIN-TASK-KEY TO TTY-TERMINAL
              MOVE 0              to WIN-LEN
              WRITE WIN-TASK-REG
           END-IF

           IF CONSOLE-MSG = 'ENDED'
              DISPLAY 'CWSTARTED' UPON ENVIRONMENT-NAME
              DISPLAY SPACES      UPON ENVIRONMENT-VALUE
           END-IF

           CALL "CWTIME" USING PARAMETROS-CWTIME
           MOVE CWTIME-DATE-FINAL (1: 2) TO TTY-DIA
           MOVE CWTIME-DATE-FINAL (3: 2) TO TTY-MES
           MOVE CWTIME-TIME-FINAL (1: 2) TO TTY-HORA
           MOVE CWTIME-TIME-FINAL (3: 2) TO TTY-MINUTO
           MOVE CWTIME-TIME-FINAL (5: 2) TO TTY-SEGUNDO
           MOVE SPACES                   TO TTY-STRING win-msg2
           IF CONSOLE-PROGRAM = 'CWUSER'
              MOVE SAVE-PROGRAM TO CONSOLE-PROGRAM
           ELSE
              MOVE CONSOLE-PROGRAM TO SAVE-PROGRAM
           END-IF
           STRING CONSOLE-PROGRAM DELIMITED BY SPACE
                   " : " CONSOLE-MSG DELIMITED BY SIZE
                   INTO TTY-STRING
           move space                    TO WIN-MSG2
           MOVE SPACES                   TO CONSOLE-MSG
           PERFORM TEST AFTER UNTIL FS-WIN-CONSOLE < "10"
                                 OR FS-WIN-CONSOLE = "9D"
                   OPEN I-O WIN-CONSOLE
                   IF FS-WIN-CONSOLE = "39"
                      DELETE FILE WIN-CONSOLE
                      OPEN I-O WIN-CONSOLE
                   END-IF
                   MOVE HIGH-VALUES TO WIN-KEY(1:)
                   START WIN-CONSOLE KEY NOT GREATER WIN-KEY
                   READ WIN-CONSOLE PREVIOUS RECORD
                   IF FS-WIN-CONSOLE > '09'
                   AND (FS-WIN-CONSOLE NOT = '9D')
                      MOVE 0 TO WIN-KEY
                      MOVE '00' TO FS-WIN-CONSOLE
                   END-IF
                   ADD  1         TO WIN-KEY
           END-PERFORM
           IF   X91-PARAMETER > 1
                MOVE TTY-TERMINAL TO WIN-TASK-KEY
                READ WIN-TASK
                MOVE SPACE TO WIN-TASK-STA
                              WIN-TASK-ACC
                REWRITE WIN-TASK-REG
                MOVE CONSOLE-LENGTH TO WIN-LEN
                IF   LK-STOP-ACCT = "STOP"
                     MOVE '1' TO WIN-MSG2
                ELSE
                     MOVE '2' TO WIN-MSG2
                END-IF
      *         MOVE ">"       TO TTY-DIR
                MOVE TTY-WRITE TO WIN-MSG
                MOVE 4 TO WIN-RESP
                PERFORM TEST AFTER UNTIL FS-WIN-CONSOLE NOT = '22'
                        WRITE WIN-CONSOLE-REG
                        IF FS-WIN-CONSOLE = '22'
                           ADD 1 TO WIN-KEY
                        END-IF
                END-PERFORM
                READ WIN-CONSOLE WITH LOCK
                PERFORM TEST AFTER UNTIL WIN-TASK-STA = '*'
                        READ WIN-TASK
                END-PERFORM
                UNLOCK WIN-CONSOLE
                PERFORM TEST AFTER UNTIL FS-WIN-TASK < "10"
                                     AND WIN-RESP = 3
                        READ WIN-CONSOLE
                END-PERFORM
                MOVE 0 TO WIN-RESP
                IF win-mSG2  = '2'
                   MOVE WIN-TASK-ACC TO CONSOLE-MSG
                else
                   MOVE spaces       TO CONSOLE-MSG
                end-if
                REWRITE WIN-CONSOLE-REG
                MOVE SPACE TO WIN-TASK-STA
                              WIN-TASK-ACC
                REWRITE WIN-TASK-REG
                CALL "CWTIME" USING PARAMETROS-CWTIME
                MOVE CWTIME-DATE-FINAL (1: 2) TO TTY-DIA
                MOVE CWTIME-DATE-FINAL (3: 2) TO TTY-MES
                MOVE CWTIME-TIME-FINAL (1: 2) TO TTY-HORA
                MOVE CWTIME-TIME-FINAL (3: 2) TO TTY-MINUTO
                MOVE CWTIME-TIME-FINAL (5: 2) TO TTY-SEGUNDO
                MOVE SPACES                   TO win-msg2
                IF win-mSG2  = '2'
                   MOVE WIN-TASK-ACC TO tty-string
                else
                   MOVE spaces       TO tty-string
                end-if
                STRING CONSOLE-PROGRAM DELIMITED BY SPACE
                        " < " CONSOLE-MSG DELIMITED BY SIZE
                        INTO TTY-STRING
                move space                    TO WIN-MSG2
                MOVE TTY-WRITE TO WIN-MSG
                PERFORM TEST AFTER UNTIL FS-WIN-CONSOLE NOT = '22'
                        WRITE WIN-CONSOLE-REG
                        IF FS-WIN-CONSOLE = '22'
                           ADD 1 TO WIN-KEY
                        END-IF
                END-PERFORM
           ELSE
                MOVE 0         TO WIN-RESP
                MOVE TTY-WRITE TO WIN-MSG
                PERFORM TEST AFTER UNTIL FS-WIN-CONSOLE NOT = '22'
                        WRITE WIN-CONSOLE-REG
                        IF FS-WIN-CONSOLE = '22'
                           ADD 1 TO WIN-KEY
                        END-IF
                END-PERFORM
           END-IF
           CLOSE WIN-CONSOLE.

       000-99-FIM. GOBACK.

       OPERADOR.

           IF   FS-WIN-LOCK = '9A'
                EXEC COBOLware Send
                     Message "Console em uso"
                END-EXEC
                GOBACk
           END-IF

           EXEC COBOLware Window Open
                HEADER  'Console'
                  LINE  1
                COLUMN  1
                 LINES  janlines
                  SIZE  cols
           COLOR-FRAME  BLACK-GREEN-HIGH
           COLOR-BORDER BLACK-GREEN-LOW
             REFERENCE;JANCODE
                 ERASE
           END-EXEC
           COMPUTE JANLINS = janlines - 1
           EXEC COBOLware OBJECT VERTICAL SCROLL
                  LINE 1
                COLUMN cols
                HEIGHT janlines
                KEY F2 THUMB THUMB-V
           END-EXEC
           EXEC COBOLware OBJECT HORIZONTAL SCROLL
                LINE   janlines
                COLUMN 1
                WIDTH  cols
                KEY F4 THUMB THUMB-H
           END-EXEC
           subtract 2 from janlines
           COMPUTE JANCOLS = cols - 1
           OPEN I-O WIN-CONSOLE.
       TOPO.
           MOVE 4 TO WIN-RESP
           START WIN-CONSOLE KEY NOT GREATER WIN-RESP
           READ WIN-CONSOLE PREVIOUS RECORD
           IF (FS-WIN-CONSOLE NOT = '9D')
           OR (WIN-RESP NOT = 4)
               MOVE HIGH-VALUES TO WIN-KEY(1:)
           END-IF
           START WIN-CONSOLE KEY NOT GREATER WIN-KEY
           IF FS-WIN-CONSOLE > '09'
              MOVE 0 TO WIN-KEY
           ELSE
              PERFORM janlines TIMES
                      READ WIN-CONSOLE PREVIOUS RECORD IGNORE LOCK
                      IF FS-WIN-CONSOLE < '10'
                         MOVE WIN-KEY TO PONTEIRO
                      END-IF
                      IF FS-WIN-CONSOLE > '09'
                      AND (FS-WIN-CONSOLE NOT = '9D')
                         EXIT PERFORM
                      END-IF
                      IF LINHAS = 0
                         MOVE WIN-KEY TO LINHAS
                      END-IF
                      PERFORM VARYING POS-INI FROM 1 BY 1
                              UNTIL POS-INI > LENGTH WIN-ID
                                 OR WIN-ID (POS-INI:1) <> SPACE
                              CONTINUE
                      END-PERFORM
                      SUBTRACT 1 FROM POS-INI
              END-PERFORM
           END-IF.

       GET-COMMAND.

           INITIALIZE QUEST-ARRAY
           MOVE 1 TO L
           MOVE PONTEIRO TO WIN-KEY
           PERFORM varying l from 1 by 1 until l > janlines
                   READ WIN-CONSOLE
                   IF   FS-WIN-CONSOLE > '09'
                   AND (FS-WIN-CONSOLE NOT = '9D')
                       MOVE SPACES TO MOSTRA
                   ELSE
                       MOVE WIN-MSG (POS + pos-ini:)  TO MOSTRA
                       PERFORM VARYING I FROM LENGTH WIN-MSG
                               BY -1 UNTIL I = 1
                                           OR WIN-MSG (I:1) <> SPACE
                              CONTINUE
                       END-PERFORM
                       IF I > COLUNAS
                          MOVE I TO COLUNAS
                       END-IF
                   END-IF
                   DISPLAY MOSTRA LINE L COLUMN 1
                   IF  FS-WIN-CONSOLE = '9D'
                   AND WIN-RESP = 4
                       ADD  1       TO QUESTS
                       MOVE WIN-KEY TO QUEST-KEY(QUESTS)
                       MOVE '00'    TO FS-WIN-CONSOLE
                       ADD  1       TO L
                       MOVE 47      TO SZ(QUESTS)
                       IF WIN-MSG2  = '1'
                          MOVE "[Enter] para continuar..."  TO MOSTRA
                          MOVE 27                        TO CL(QUESTS)
                          MOVE 1                         TO SZ(QUESTS)
                       ELSE
                          MOVE "Aguardando resposta..."  TO MOSTRA
                          MOVE 24                        TO CL(QUESTS)
                          IF  WIN-LEN > 0
                          AND (WIN-LEN NOT > 47)
                              MOVE WIN-LEN TO SZ(QUESTS)
                          END-IF
                       END-IF
                       MOVE L       TO LN(QUESTS)
                       DISPLAY MOSTRA LINE L COLUMN 1
                   END-IF
                   ADD 1 TO WIN-KEY
           END-PERFORM

           IF LINHAS = 0 GO TO TOPO.

           EVALUATE QUESTS
               WHEN 01 ACCEPT T01
               WHEN 02 ACCEPT T02
               WHEN 03 ACCEPT T03
               WHEN 04 ACCEPT T04
               WHEN 05 ACCEPT T05
               WHEN 06 ACCEPT T06
               WHEN 07 ACCEPT T07
               WHEN 08 ACCEPT T08
               WHEN 09 ACCEPT T09
               WHEN 10 ACCEPT T10
               WHEN 11 ACCEPT T11
               WHEN 12 ACCEPT T12
               WHEN 13 ACCEPT T13
               WHEN 14 ACCEPT T14
               WHEN 15 ACCEPT T15
               WHEN 16 ACCEPT T16
               WHEN 17 ACCEPT T17
               WHEN 18 ACCEPT T18
               WHEN 19 ACCEPT T19
               WHEN 20 ACCEPT T20
               WHEN 21 ACCEPT T21
               WHEN OTHER
                    ACCEPT NADA
                      LINE 23 COLUMN 78
                      WITH AUTO-SKIP timeout 1 SECURE
                    DISPLAY SPACES LINE 22 COLUMN 1 WITH SIZE 76
           END-EVALUATE
           ACCEPT TECLA FROM ESCAPE KEY

           EVALUATE TRUE
            WHEN ALT-F11
             AND (WIN-KEY NOT < LINHAS)
              OR (LINHAS = ZERO)
                 IF LINHAS = ZERO
                    MOVE ZERO TO WIN-KEY PONTEIRO
                    START WIN-CONSOLE KEY NOT LESS WIN-KEY
                 END-IF
                 PERFORM TEST AFTER UNTIL FS-WIN-CONSOLE > '09'
                         READ WIN-CONSOLE NEXT RECORD
                         IF FS-WIN-CONSOLE < '10'
                         OR FS-WIN-CONSOLE = '9D'
                            MOVE WIN-KEY TO LINHAS
                            IF PONTEIRO NOT = ZERO
                               ADD  1       TO PONTEIRO
                            END-IF
                         END-IF
                 END-PERFORM
                 IF PONTEIRO = ZERO
                    MOVE 1 TO PONTEIRO
                 END-IF
            WHEN F2
                 EVALUATE TRUE
                   WHEN THUMB-V = THUMB-VA + 1
                        SET CURSOR-DOWN TO TRUE
                   WHEN THUMB-V = THUMB-VA - 1
                        SET CURSOR-UP TO TRUE
                   WHEN THUMB-V < 2
                        SET CONTROL-PAGE-UP TO TRUE
                   WHEN THUMB-V > 99
                        SET CONTROL-PAGE-DOWN TO TRUE
                   WHEN THUMB-V NOT = THUMB-VA
                        COMPUTE PONTEIRO
                            = (LINHAS / 100) * THUMB-V
                 END-EVALUATE
            WHEN F4
                 EVALUATE TRUE
                   WHEN THUMB-H = THUMB-HA + 1
                    AND POS < (COLUNAS - JANCOLS)
                        ADD 1 TO POS
                   WHEN THUMB-H = THUMB-HA - 1
                     AND POS > 1
                         SUBTRACT 1 FROM POS
                   WHEN THUMB-H < 2
                        MOVE 1 TO POS
                   WHEN THUMB-H > 99
                        COMPUTE POS = COLUNAS - JANCOLS
                   WHEN THUMB-H NOT = THUMB-HA
                        COMPUTE POS = (COLUNAS / 100)
                                    * THUMB-H
                END-EVALUATE
           END-EVALUATE
           EVALUATE TRUE
                WHEN EDIT-CONTROL-CURSOR-LEFT
                     MOVE 1 TO POS
                WHEN EDIT-CONTROL-CURSOR-RIGHT
                     COMPUTE POS = COLUNAS - JANCOLS
                WHEN T-TECLA = 2
                 AND T2 = 3
                     IF POS > 1
                        SUBTRACT 1 FROM POS
                     END-IF
                WHEN T-TECLA = 2
                 AND T2 = 4
                 AND POS < (COLUNAS - JANCOLS)
                     ADD 1 TO POS
                WHEN CURSOR-UP
                     IF PONTEIRO > 1
                        SUBTRACT 1 FROM PONTEIRO
                     END-IF
                WHEN CURSOR-DOWN OR F9
                     IF (PONTEIRO + JANLINS) < LINHAS
                        ADD 1 TO PONTEIRO
                     END-IF
                WHEN PAGE-DOWN
                     IF (PONTEIRO + JANLINS) < LINHAS
                        ADD JANLINS TO PONTEIRO
                     END-IF
                WHEN PAGE-UP
                     IF PONTEIRO > JANLINS
                        SUBTRACT JANLINS FROM PONTEIRO
                     ELSE
                        MOVE 1 TO PONTEIRO
                     END-IF
                WHEN CONTROL-PAGE-UP
                     MOVE 1 TO PONTEIRO
                WHEN CONTROL-PAGE-DOWN
                     MOVE LINHAS TO PONTEIRO
                     PERFORM JANLINS TIMES
                          IF PONTEIRO > 1
                             SUBTRACT 1 FROM PONTEIRO
                          END-IF
                     END-PERFORM
           END-EVALUATE
           COMPUTE THUMB-V = PONTEIRO / LINHAS * 100

           IF((TECLA NOT = 0)
           OR (T2 NOT = 0))
           AND  PONTEIRO = PONTEIRO-A
           AND POS = POS-A
           AND NOT F3
           AND NOT F5
           AND NOT ALT-F11
           AND NOT ESC
               CALL X'E5'
           END-IF

           IF   ESC
                EXEC COBOLWARE OBject Drop END-EXEC
                EXEC COBOLWARE WINDOW CLOSE
                     REFERENCE (JANCODE)
                END-EXEC
                CLOSE WIN-LOCK
                DELETE FILE WIN-LOCK
                GOBACK
           ELSE
                IF TECLA = 0
                   OPEN I-O WIN-TASK
                   PERFORM UNTIL QUESTS = 0
                           MOVE QUEST-KEY (QUESTS) TO WIN-KEY
                           READ WIN-CONSOLE IGNORE LOCK
                           IF FS-WIN-CONSOLE = '00'
                              MOVE WIN-MSG             TO TTY-WRITE
                              MOVE TTY-TERMINAL        TO WIN-TASK-KEY
                              READ WIN-TASK
                              MOVE '*'                 TO WIN-TASK-STA
                              MOVE Q-R (QUESTS) TO WIN-TASK-ACC
                              REWRITE WIN-TASK-REG
                              READ WIN-CONSOLE LOCK WAIT
                              MOVE 3 TO WIN-RESP
                              REWRITE WIN-CONSOLE-REG
                              UNLOCK WIN-CONSOLE
                           END-IF
                           SUBTRACT 1 FROM QUESTS
                   END-PERFORM
                   CLOSE WIN-TASK
                END-IF
                GO TO GET-COMMAND
           END-IF.

       FIM-MOSTRA-LINHA. EXIT.
       EXIT PROGRAM.
       END PROGRAM CONSOLE.
