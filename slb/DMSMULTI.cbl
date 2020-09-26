      $SET NoCallFH
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    DMSMULTI.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  14/10/2012.
       SECURITY.      *************************************************
                      *                                               *
                      * Seletor de manipuladores em acessos a datasets*
                      * embutidos (multiplos tipos de registro)       *                             *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

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

       DATA DIVISION.
       FILE SECTION.

       FD  SQLCONF
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-SQLCONF.

       01  SQLCONF-REG           PIC  X(080).

       FD  GRADE
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-GRADE.

       01  GRADE-REG.
           05 GRADE-ID.
              10 GRADE-FILENAME         PIC  X(050).
              10 GRADE-LIT              PIC  X(010).
           05 GRADE-HANDLER             PIC  X(050).
           05 REDEFINES GRADE-HANDLER.
              10 GRADE-POSITION         PIC  9(005).
              10 GRADE-LENGTH           PIC  9(002).
              10                        PIC  X(043).
           05 GRADE-STATUS              PIC  X(002).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 CWSQLC-MASTER        PIC  X(009)  VALUE SPACES.
           05 MSG                  PIC  X(080)  VALUE SPACES.
           05 FILENAME             PIC  X(050)  VALUE SPACES.
           05 MINUSCULAS           PIC  X(026) VALUE
              "abcdefghijklmnopqrstuvwxyz".
           05 MAIUSCULAS           PIC  X(026) VALUE
              "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
           05 I                    PIC  9(002) VALUE 0.
           05 Y                    PIC  9(002) VALUE 0.
           05 P                    PIC  9(004) VALUE 0.
           05 L                    PIC  9(004) VALUE 0.
           05 ER-SQLCONF.
              10 FS-SQLCONF        PIC  X(002) VALUE "00".
              10 LB-SQLCONF        PIC  X(255) VALUE "cwsqlc.ini".
           05 ER-GRADE.
              10 FS-GRADE          PIC  X(002) VALUE "00".
              10 LB-GRADE          PIC  X(255) VALUE "cwsqlm-$$$".
           05 NOME                 PIC  X(080) VALUE SPACES.
           05 CONTEUDO             PIC  X(080) VALUE SPACES.

       LINKAGE SECTION.

       COPY CWSQLC.
       01  BUFFER                PIC X(65536).
       01  ER-FILE.
           05 FILE-STATUS        PIC   X(002).
           05 FILE-LABEL         PIC   X(050).
           05 FILE-LRECL         PIC   9(005).
       01  POINTERS-BUFFER       PIC   X(383).

       PROCEDURE DIVISION USING CWSQLC
                                BUFFER
                                ER-FILE
                                POINTERS-BUFFER.

       000-INICIO.

           ON 1 PERFORM LOAD-INI.

           MOVE FILE-LABEL  TO GRADE-FILENAME FILENAME

           MOVE LOW-VALUES  TO GRADE-LIT
           READ GRADE
           IF FS-GRADE NOT = '00'
              STRING 'Grade ['  DELIMITED BY SIZE
                     FILENAME DELIMITED BY SPACE
                     '] nÆo consta em ' DELIMITED BY SIZE
                     LB-SQLCONF DELIMITED BY SPACE
                     INTO MSG
              EXEC COBOLware Send Message MSG END-EXEC
              STOP RUN
           END-IF

           MOVE GRADE-POSITION TO P
           MOVE GRADE-LENGTH   TO L

           IF  (CWSQLC-READ
           OR  CWSQLC-START
           OR  CWSQLC-WRITE
           OR  CWSQLC-REWRITE
           OR  CWSQLC-DELETE)
           AND BUFFER (P: L) NOT = ALL '0'
               MOVE BUFFER (P: L) TO GRADE-LIT
               READ GRADE
               IF FS-GRADE NOT = '00'
                  STRING 'Tipo "'  DELIMITED BY SIZE
                         BUFFER(P: L) DELIMITED BY SIZE
                         '" nÆo consta em ' DELIMITED BY SIZE
                         LB-SQLCONF DELIMITED BY SPACE
                         INTO MSG
                  EXEC COBOLware Send Message MSG END-EXEC
                  STOP RUN
               END-IF
               CALL GRADE-HANDLER USING  CWSQLC
                                    BUFFER(1:FILE-LRECL)
                                    ER-FILE
                                    POINTERS-BUFFER
            ELSE
               PERFORM UNTIL FS-GRADE > '09'
                       OR    GRADE-FILENAME NOT = FILENAME
                       READ GRADE NEXT RECORD
                       IF  FS-GRADE < '10'
                       AND GRADE-FILENAME = FILENAME
                           MOVE CWSQLC TO CWSQLC-MASTER
                           CALL GRADE-HANDLER USING  CWSQLC
                                                BUFFER(1:FILE-LRECL)
                                                ER-FILE
                                                POINTERS-BUFFER
                           IF  CWSQLC-CANCEL
                           OR  CWSQLC-OPEN
                               CANCEL GRADE-HANDLER
                           END-IF
                           MOVE CWSQLC-MASTER TO CWSQLC
                           IF  CWSQLC-UNLOCK
                           OR  CWSQLC-ROLLBACK
                               MOVE "10" TO FS-GRADE
                           END-IF
                       END-IF
               END-PERFORM
            END-IF.

       000-99-FIM. GOBACK.

       LOAD-INI.

              CALL "CWFILE" USING LB-SQLCONF
              OPEN INPUT SQLCONF

              IF FS-SQLCONF NOT = '00'
                 CALL "CWISAM" USING ER-SQLCONF
                    ON EXCEPTION CONTINUE
                 END-CALL
                 STOP RUN
              END-IF

              OPEN I-O GRADE
              INITIALIZE GRADE-REG
              PERFORM UNTIL FS-SQLCONF NOT = '00'
                 READ SQLCONF IGNORE LOCK
                 IF FS-SQLCONF = '00'
                 AND (SQLCONF-REG NOT = SPACES)
                     PERFORM VARYING I FROM 1 BY 1
                              UNTIL SQLCONF-REG (I: 1) NOT = SPACE
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
                        WRITE GRADE-REG
                     ELSE
                        MOVE SPACES   TO NOME CONTEUDO
                        MOVE FILENAME TO GRADE-REG
                        MOVE 0        TO Y
                        PERFORM VARYING I FROM I BY 1 UNTIL I > 80
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
                        INSPECT NOME CONVERTING MINUSCULAS
                                             TO MAIUSCULAS
                        IF (NOME NOT = SPACES)
                        AND (NOME(1:1) NOT = '*')
                        AND (NOME(1:2) NOT = '/*')
                        AND (NOME(1:4) NOT = 'REM ')
                        AND (CONTEUDO NOT = SPACES)
                            EVALUATE NOME
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

       END PROGRAM DMSMULTI.
