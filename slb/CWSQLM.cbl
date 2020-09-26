      $SET CALLFH"EXTFH"
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWSQLM.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  23/04/2008.
       SECURITY.      *************************************************
                      *                                               *
                      *  Seletor de acessos a multiplos tipos de      *
                      *  registros                                    *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      *-----> LE CWSLQC.INI
           SELECT SQLCONF ASSIGN TO DISK
                  ORGANIZATION  IS LINE SEQUENTIAL
                  FILE STATUS   IS FS-SQLCONF.

      $Set IdxFormat"14" DataCompress"1" KeyCompress"7"
           SELECT GRADE ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS GRADE-ID
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-GRADE.

           SELECT FILA  ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS FILA-POINTER WITH DUPLICATES
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-FILA.

           SELECT OPENID ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS OPENID-ID
                  ALTERNATE KEY IS OPENID-SESSION-ID
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-OPENID.

       DATA DIVISION.
       FILE SECTION.

       FD  SQLCONF
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-SQLCONF.

       01  SQLCONF-REG           PIC  X(255).

       FD  GRADE
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-GRADE.

       01  GRADE-REG.
           05 GRADE-ID.
              10 GRADE-FILENAME         PIC  X(255).
              10 GRADE-LIT              PIC  X(010).
           05 GRADE-HANDLER             PIC  X(255).
           05 REDEFINES GRADE-HANDLER.
              10 GRADE-POSITION         PIC  9(005).
              10 GRADE-LENGTH           PIC  9(002).
              10                        PIC  X(248).
           05 GRADE-STATUS              PIC  X(002).
           05 GRADE-TESTADO             PIC  X(001).

       FD  FILA
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-FILA.

       01  FILA-REG.
           05 FILA-POINTER             PIC  X(255).
           05 FILA-HANDLER             PIC  X(255).
           05 FILA-CWSQLC              PIC  X(009).

       FD  OPENID
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-OPENID.

       01  OPENID-REG.
           05 OPENID-ID.
              10 OPENID-FILENAME       PIC  X(255).
              10 OPENID-LIT            PIC  X(010).
              10 OPENID-SESSION-ID     PIC  9(009) COMP-X.
           05 OPENID-POINTER           PIC  X(383).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 CWFILE               PIC  X(255) VALUE 'CWFILE'.
           05 MINUSCULAS           PIC  X(026) VALUE
              "abcdefghijklmnopqrstuvwxyz".
           05 MAIUSCULAS           PIC  X(026) VALUE
              "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
           05 FILENAME             PIC  X(255)  VALUE SPACES.
           05 CWSQLC-MASTER        PIC  X(009)  VALUE SPACES.
           05 CWSQLC-NextPrev      PIC  X(009)  VALUE SPACES.
           05 BUFFER-WS            PIC X(65536) VALUE SPACES.
           05 WS-ER-FILE.
              10 FILE-STATUS-WS    PIC  X(002)  VALUE '00'.
              10 LABEL-WS          PIC  X(050)  VALUE SPACES.
           05 POINTERS-WS.
              10 POINTERS-BUFFER-WS  PIC  X(383)  VALUE SPACES.
              10 POINTERS-VOLUME-WS  PIC  9(018) COMP-3.
           05 MSG                  PIC  X(080)  VALUE SPACES.
           05 NOME                 PIC  X(080)  VALUE SPACES.
           05 NOME-CASE            PIC  X(080)  VALUE SPACES.
           05 FS-OPEN              PIC  X(002)  VALUE SPACES.
           05 CONTEUDO             PIC  X(255)  VALUE SPACES.
           05 I                    PIC  9(003)  VALUE 0.
           05 Y                    PIC  9(003)  VALUE 0.
           05 P                    PIC  9(004)  VALUE 0.
           05 L                    PIC  9(004)  VALUE 0.
           05 L2                   PIC  9(004)  VALUE 0.
           05 ER-SQLCONF.
              10 FS-SQLCONF        PIC  X(002) VALUE "00".
              10 LB-SQLCONF        PIC  X(255) VALUE "cwsqlc.ini".
           05 ER-GRADE.
              10 FS-GRADE          PIC  X(002) VALUE "00".
              10 LB-GRADE          PIC  X(255) VALUE "cwsqlm-$$$".
           05 ER-FILA.
              10 FS-FILA           PIC  X(002) VALUE "00".
              10 LB-FILA           PIC  X(255) VALUE "cwsqlf-$$$".
           05 ER-OPENID.
              10 FS-OPENID         PIC  X(002) VALUE "00".
              10 LB-OPENID         PIC  X(255) VALUE "cwsqlO-$$$".
           05 MSG-E                PIC  X(060) VALUE SPACES.
           05 FLAG                 PIC  X(001) VALUE SPACE.
              88 CWQUEUE                       VALUE '*'.

       01  X91.
           05 X91-RESULT           PIC  9(002) COMP-X VALUE 0.
           05 X91-F15              PIC  9(002) COMP-X VALUE 15.
           05 X91-MANIPULADOR.
              10 LEN-MANIPULADOR   PIC  9(002) VALUE 0 COMP-X.
              10 MANIPULADOR       PIC  X(065) VALUE SPACES.

       COPY CWSEND.

       LINKAGE SECTION.

       COPY CWSQLC.
       01 BUFFER                PIC X(65536).
       01 ER-FILE.
          05 FILE-STATUS        PIC X(002).
          05 FILE-LABEL         PIC X(050).
       01 POINTERS.
          05 POINTERS-BUFFER    PIC X(383).
          05 POINTERS-VOLUME    PIC 9(018) COMP-3.
       01 FILENAME-LK           PIC X(050).
       01 MAX-REC-LENGTH        PIC 9(004) COMP-X.
       01 RELATIVE-KEY          PIC 9(009) COMP-X.
       01 FCD-OPENID-ID         PIC 9(9) COMP-X.
       01 FCD-KEY-DEF-ADDRESS  USAGE POINTER.

       PROCEDURE DIVISION USING CWSQLC
                                BUFFER
                                ER-FILE
                                POINTERS
                                FILENAME-LK
                                MAX-REC-LENGTH
                                RELATIVE-KEY
                                FCD-OPENID-ID
                                FCD-KEY-DEF-ADDRESS.

       INICIO.

           ON 1
              PERFORM LOAD-INI
              DISPLAY 'CWQUEUESTATUS' UPON ENVIRONMENT-NAME
              ACCEPT  FLAG FROM ENVIRONMENT-VALUE.


           MOVE POINTERS-VOLUME TO POINTERS-VOLUME-WS

           IF FILENAME-LK (1:1) = '['
              MOVE SPACES TO FILENAME
              STRING FILENAME-LK (2: ) DELIMITED BY ']'
                     INTO FILENAME
           ELSE
              MOVE FILENAME-LK TO FILENAME
           END-IF
           MOVE FILENAME    TO GRADE-FILENAME
           MOVE LOW-VALUES  TO GRADE-LIT
           READ GRADE
           IF FS-GRADE NOT = '00'
              STRING 'Grade ['  DELIMITED BY SIZE
                     FILENAME DELIMITED BY SPACE
                     '] nÆo consta em ' DELIMITED BY SIZE
                     LB-SQLCONF DELIMITED BY SPACE
                     INTO MSG
              IF CWQUEUE
                 DISPLAY "SQLERRMC" UPON ENVIRONMENT-NAME
                 DISPLAY MSG        UPON ENVIRONMENT-VALUE
                 MOVE    '9­'         TO FILE-STATUS
                 GOBACK
              ELSE
                 MOVE MSG TO CWSEND-MSG  MSG-E
                 CALL "CWLOGW" USING "s" MSG-E
                   ON EXCEPTION
                      CONTINUE
                 END-CALL
                 CALL "CWSEND" USING PARAMETROS-CWSEND
                 ON EXCEPTION
                       DISPLAY MSG UPON CONSOLE
                 END-CALL
                 STOP RUN
              END-IF
           END-IF

           MOVE GRADE-POSITION TO P
           MOVE GRADE-LENGTH   TO L

           EVALUATE TRUE
               WHEN CWSQLC-NO-DATA
                 OR CWSQLC-COMMIT-MANUAL
                 OR CWSQLC-COMMIT-AUTOMATIC
                 OR CWSQLC-START
                 OR CWSQLC-READ
                    IF CWSQLC-READ
                    OR CWSQLC-START
                       OPEN I-O FILA
                    END-IF
                    MOVE SPACES TO FS-OPEN
                    PERFORM TEST AFTER UNTIL FS-GRADE > '09'
                            READ GRADE NEXT RECORD
                            IF FS-GRADE < '10'
                               IF GRADE-FILENAME = FILENAME
                                 IF CWSQLC-CREATE
                                 AND GRADE-STATUS > '09'
                                      MOVE '00' TO GRADE-STATUS
                                      REWRITE GRADE-REG
                                 END-IF
                                 IF NOT CWSQLC-OPEN
                                 AND GRADE-STATUS > '09'
                                     EXIT PERFORM CYCLE
                                 END-IF
                                 MOVE POINTERS-BUFFER
                                   TO POINTERS-BUFFER-WS
                                 IF CWSQLC-READ
                                 OR CWSQLC-START
                                    MOVE GRADE-FILENAME
                                      TO OPENID-FILENAME
                                    MOVE GRADE-LIT      TO OPENID-LIT
                                    MOVE FCD-OPENID-ID
                                      TO OPENID-SESSION-ID
                                    READ OPENID
                                    IF  FS-OPENID = '00' AND
                                       (OPENID-POINTER NOT = SPACES)
                                        MOVE OPENID-POINTER
                                          TO POINTERS-BUFFER-WS
                                    END-IF
                                 END-IF
                                 MOVE BUFFER(1:MAX-REC-LENGTH)
                                   TO BUFFER-WS(1:MAX-REC-LENGTH)
                                 MOVE FILE-LABEL TO LABEL-WS
                                 MOVE CWSQLC TO CWSQLC-MASTER
                                 IF GRADE-TESTADO = SPACE
                                    MOVE 'S' TO GRADE-TESTADO
                                    INSPECT GRADE-HANDLER
                                            CONVERTING MINUSCULAS
                                                    TO MAIUSCULAS
                                    MOVE GRADE-HANDLER TO MANIPULADOR
                                    MOVE LENGTH MANIPULADOR
                                             TO LEN-MANIPULADOR
                                    CALL X"91" USING X91-RESULT X91-F15
                                                     X91-MANIPULADOR
                                    IF   X91-RESULT NOT = 0
                                         MOVE GRADE-HANDLER
                                           TO MANIPULADOR
                                         INSPECT MANIPULADOR
                                                 CONVERTING MAIUSCULAS
                                                         TO MINUSCULAS
                                         MOVE LENGTH MANIPULADOR
                                                  TO LEN-MANIPULADOR
                                         CALL X"91" USING X91-RESULT
                                                          X91-F15
                                                        X91-MANIPULADOR
                                         IF   X91-RESULT = 0
                                              INSPECT GRADE-HANDLER
                                                 CONVERTING MAIUSCULAS
                                                         TO MINUSCULAS
                                         END-IF
                                    END-IF
                                    IF   X91-RESULT NOT = 0
                                         MOVE SPACES TO MSG-E
                                         STRING 'Manipulador SQL('
                                                DELIMITED BY SIZE
                                                GRADE-HANDLER
                                                DELIMITED BY SPACE
                                                ') indispon¡vel'
                                                DELIMITED BY SIZE
                                         INTO MSG-E
                                         IF CWQUEUE
                                            DISPLAY "SQLERRMC"
                                               UPON ENVIRONMENT-NAME
                                            DISPLAY MSG-E
                                               UPON ENVIRONMENT-VALUE
                                            MOVE '9­' TO FILE-STATUS
                                            GOBACK
                                         ELSE
                                            CALL "CWLOGW"
                                                 USING "s" MSG-E
                                              ON EXCEPTION
                                                 CONTINUE
                                            END-CALL
                                            MOVE MSG-E TO CWSEND-MSG
                                            CALL "CWSEND" USING
                                                 PARAMETROS-CWSEND
                                              ON EXCEPTION
                                                 DISPLAY MSG-E
                                                 UPON CONSOLE
                                         END-CALL
                                         stop run
                                         END-IF
                                    END-IF
                                 END-IF
                                 CALL GRADE-HANDLER
                                      USING CWSQLC
                                            BUFFER-WS(1:MAX-REC-LENGTH)
                                            WS-ER-FILE
                                            POINTERS-WS
                                            FILENAME-LK
                                            MAX-REC-LENGTH
                                            RELATIVE-KEY
                                            FCD-OPENID-ID
                                            FCD-KEY-DEF-ADDRESS
                                  IF CWSQLC-READ
                                  AND (BUFFER-WS (P: L)
                                      NOT = GRADE-LIT (1:L))
                                      MOVE '23' TO FILE-STATUS-WS
                                  END-IF
                                  IF CWSQLC-OPEN
                                     MOVE FILE-STATUS-WS TO GRADE-STATUS
                                     REWRITE GRADE-REG
                                  END-IF
                                  IF CWSQLC-OPEN
                                  AND (FS-OPEN = SPACES
                                    OR (FILE-STATUS-WS = '00'
                                     AND FS-OPEN >'09'))
                                    MOVE FILE-STATUS-WS TO FS-OPEN
                                    MOVE POINTERS-BUFFER-WS
                                      TO POINTERS-BUFFER
                                    MOVE FILE-STATUS-WS
                                      TO FILE-STATUS
                                  END-IF
                                  IF (CWSQLC-START
                                  OR  CWSQLC-READ)
                                  AND FILE-STATUS-WS < '10'
                                      IF CWSQLC-START
                                         MOVE CWSQLC TO CWSQLC-NextPrev
                                         SET CWSQLC-READ  TO TRUE
                                         IF CWSQLC-NOT-GREATER
                                            SET CWSQLC-PREVIOUS TO TRUE
                                         ELSE
                                            SET CWSQLC-NEXT     TO TRUE
                                         END-IF
                                         CALL GRADE-HANDLER
                                                 USING CWSQLC
                                            BUFFER-WS(1:MAX-REC-LENGTH)
                                            WS-ER-FILE
                                            POINTERS-WS
                                            FILENAME-LK
                                            MAX-REC-LENGTH
                                            RELATIVE-KEY
                                            FCD-OPENID-ID
                                            FCD-KEY-DEF-ADDRESS
                                         IF FILE-STATUS-WS > '09'
                                         OR (BUFFER-WS (P: L)
                                            NOT = GRADE-LIT (1:L))
                                            MOVE '23' TO FILE-STATUS-WS
                                         END-IF
                                         MOVE CWSQLC-NextPrev TO CWSQLC
                                      END-IF
                                      MOVE POINTERS-BUFFER-WS(75:255)
                                        TO FILA-POINTER
                                      MOVE GRADE-HANDLER
                                        TO FILA-HANDLER
                                      MOVE CWSQLC TO FILA-CWSQLC
                                      IF FILE-STATUS-WS < '09'
                                         WRITE FILA-REG
                                      END-IF
                                      MOVE GRADE-FILENAME
                                        TO OPENID-FILENAME
                                      MOVE GRADE-LIT
                                        TO OPENID-LIT
                                      MOVE FCD-OPENID-ID
                                        TO OPENID-SESSION-ID
                                      READ OPENID
                                      IF FS-OPENID = '00'
                                         DELETE OPENID RECORD
                                      END-IF
                                      MOVE POINTERS-BUFFER-WS
                                        TO OPENID-POINTER
SAV                                   WRITE OPENID-REG
                                  END-IF
                                  MOVE CWSQLC-MASTER TO CWSQLC
                                  IF CWSQLC-COMMIT
                                  OR CWSQLC-ROLLBACK
                                     MOVE '10' TO FS-GRADE
                                  END-IF
                                  IF CWSQLC-CANCEL
                                     CANCEL GRADE-HANDLER
                                  END-IF
                               ELSE
                                  MOVE '10' TO FS-GRADE
                               END-IF
                            END-IF
                    END-PERFORM
                    IF CWSQLC-START
                    OR CWSQLC-READ
                       MOVE POINTERS-BUFFER(75:255) TO FILA-POINTER
                       IF CWSQLC-NEXT
                       OR CWSQLC-GREATER
                       OR CWSQLC-NOT-LESS
                       OR CWSQLC-EQUAL
                          START FILA KEY NOT LESS FILA-POINTER
                          IF FS-FILA NOT > '09'
                             READ FILA NEXT RECORD
                          END-IF
                       ELSE
                          START FILA KEY NOT GREATER FILA-POINTER
                          IF FS-FILA NOT > '09'
                             READ FILA PREVIOUS RECORD
                          END-IF
                       END-IF
                       IF FS-FILA NOT > '09'
                          CALL FILA-HANDLER USING FILA-CWSQLC
                                            BUFFER(1:MAX-REC-LENGTH)
                                            ER-FILE
                                            POINTERS
                                            FILENAME-LK
                                            MAX-REC-LENGTH
                                            RELATIVE-KEY
                                            FCD-OPENID-ID
                                            FCD-KEY-DEF-ADDRESS
                          MOVE FILA-CWSQLC        TO CWSQLC
                       ELSE
                          MOVE POINTERS-BUFFER-WS TO POINTERS-BUFFER
                          MOVE FILE-STATUS-WS     TO FILE-STATUS
                          MOVE BUFFER-WS(1:MAX-REC-LENGTH)
                            TO BUFFER(1:MAX-REC-LENGTH)
                       END-IF
                       CLOSE FILA
                    END-IF
                    IF CWSQLC-CLOSE
                       PERFORM TEST AFTER UNTIL FS-OPENID > '09'
                               MOVE FCD-OPENID-ID TO OPENID-SESSION-ID
                               READ OPENID KEY IS OPENID-SESSION-ID
                               IF FS-OPENID = '00'
                                  DELETE OPENID RECORD
                               END-IF
                       END-PERFORM
                    END-IF
               WHEN CWSQLC-WRITE
                 OR CWSQLC-REWRITE
                 OR CWSQLC-DELETE
                    MOVE BUFFER (P: L) TO GRADE-LIT
                    READ GRADE
                    IF   FS-GRADE = '23'
                    AND  L > 1
                         PERFORM VARYING L2 FROM L BY -1
                                 UNTIL L2 = 1
                                    OR FS-GRADE < '10'
                                 MOVE '^' TO GRADE-LIT(L2:1)
                                 READ GRADE
                         END-PERFORM
                    END-IF
                    IF FS-GRADE NOT = '00'
                       STRING 'Tipo "'  DELIMITED BY SIZE
                              BUFFER(P: L) DELIMITED BY SIZE
                              '" nÆo consta em ' DELIMITED BY SIZE
                              LB-SQLCONF DELIMITED BY SPACE
                              INTO MSG
                       MOVE MSG TO CWSEND-MSG
                       CALL "CWSEND" USING PARAMETROS-CWSEND
                       ON EXCEPTION
                          DISPLAY MSG UPON CONSOLE
                       END-CALL
                       STOP RUN
                    END-IF
                    CALL GRADE-HANDLER USING CWSQLC
                                             BUFFER(1:MAX-REC-LENGTH)
                                             ER-FILE
                                             POINTERS
                                             FILENAME-LK
                                             MAX-REC-LENGTH
                                             RELATIVE-KEY
                                             FCD-OPENID-ID
                                             FCD-KEY-DEF-ADDRESS
           END-EVALUATE.

           IF FILE-STATUS = SPACES OR LOW-VALUES
              MOVE '00' TO FILE-STATUS
           END-IF.

       FIM. GOBACK.

       LOAD-INI.

              DISPLAY "CWFILE" UPON ENVIRONMENT-NAME
              ACCEPT CWFILE FROM ENVIRONMENT-VALUE
              CALL   CWFILE USING LB-SQLCONF
                ON EXCEPTION CONTINUE
              END-CALL
              DISPLAY "CWSQLC"  UPON ENVIRONMENT-NAME
              ACCEPT LB-SQLCONF FROM ENVIRONMENT-VALUE

              OPEN INPUT SQLCONF
              IF FS-SQLCONF NOT = '00'
                 CALL "CWISAM" USING ER-SQLCONF
                    ON EXCEPTION CONTINUE
                 END-CALL
                 STOP RUN
              END-IF

              OPEN I-O GRADE OPENID
              INITIALIZE GRADE-REG
              PERFORM UNTIL FS-SQLCONF NOT = '00'
                 READ SQLCONF IGNORE LOCK
                 IF FS-SQLCONF = '00'
                 AND (SQLCONF-REG NOT = SPACES)
                     PERFORM VARYING I FROM 1 BY 1
                              UNTIL I > LENGTH SQLCONF-REG
                                 OR SQLCONF-REG (I: 1) NOT = SPACE
                              CONTINUE
                     END-PERFORM
                     IF SQLCONF-REG (I: 1) = '['
                        MOVE SPACES TO FILENAME
                        STRING SQLCONF-REG (I + 1: ) DELIMITED BY ']'
                          INTO FILENAME
                        INSPECT FILENAME CONVERTING MINUSCULAS
                                               TO MAIUSCULAS
                        MOVE FILENAME          TO GRADE-REG
                        MOVE 1                 TO GRADE-POSITION
                                                  GRADE-LENGTH
                        MOVE LOW-VALUES        TO GRADE-LIT
                        IF FILENAME NOT = 'CWSQLC'
                           WRITE GRADE-REG
                        END-IF
                     ELSE
                        MOVE SPACES   TO NOME CONTEUDO
                        MOVE FILENAME TO GRADE-REG
                        MOVE 0        TO Y
                        PERFORM VARYING I FROM I BY 1
                                  UNTIL I > LENGTH SQLCONF-REG
                                IF SQLCONF-REG (I: 1) = '='
                                   ADD  1 TO I
                                   MOVE 1 TO Y
                                   MOVE SQLCONF-REG (I: 1)
                                     TO CONTEUDO (Y:1)
                                ELSE
                                   ADD 1 TO Y
                                   IF CONTEUDO = SPACES
                                      MOVE SQLCONF-REG (I: 1)
                                        TO NOME        (Y:1)
                                   ELSE
                                      MOVE SQLCONF-REG (I: 1)
                                        TO CONTEUDO    (Y:1)
                                  END-IF
                               END-IF
                        END-PERFORM
                        MOVE NOME TO NOME-CASE
                        INSPECT NOME-CASE CONVERTING MINUSCULAS
                                             TO MAIUSCULAS
                        IF (NOME NOT = SPACES)
                        AND (NOME(1:1) NOT = '*')
                        AND (NOME(1:2) NOT = '/*')
                        AND (NOME-CASE(1:4) NOT = 'REM ')
                        AND (CONTEUDO NOT = SPACES)
                        AND (FILENAME NOT = 'CWSQLC')
                            EVALUATE NOME-CASE
                             WHEN "POSITION"
                                  MOVE LOW-VALUES        TO GRADE-LIT
                                  READ GRADE
                                  MOVE LENGTH OF GRADE-POSITION TO Y
                                  PERFORM VARYING I
                                             FROM LENGTH OF CONTEUDO
                                               BY -1
                                           UNTIL I = 0 OR Y = 0
                                           IF CONTEUDO(I:1) NUMERIC
                                              MOVE CONTEUDO(I:1)
                                                TO GRADE-POSITION(Y:1)
                                              SUBTRACT 1 FROM Y
                                           END-IF
                                  END-PERFORM
                                  REWRITE GRADE-REG
                             WHEN "LENGTH"
                                  MOVE LOW-VALUES        TO GRADE-LIT
                                  READ GRADE
                                  MOVE LENGTH OF GRADE-LENGTH TO Y
                                  PERFORM VARYING I
                                             FROM LENGTH OF CONTEUDO
                                               BY -1
                                           UNTIL I = 0 OR Y = 0
                                           IF CONTEUDO(I:1) NUMERIC
                                              MOVE CONTEUDO(I:1)
                                                TO GRADE-LENGTH(Y:1)
                                              SUBTRACT 1 FROM Y
                                           END-IF
                                  END-PERFORM
                                  REWRITE GRADE-REG
                             WHEN OTHER
                                  MOVE CONTEUDO TO GRADE-LIT
                                  MOVE NOME     TO GRADE-HANDLER
                                  WRITE GRADE-REG
                            END-EVALUATE
                     END-IF
                 END-IF
              END-PERFORM
              CLOSE SQLCONF.

       FIM-LOAD-INI. EXIT.

       END PROGRAM CWSQLM.
