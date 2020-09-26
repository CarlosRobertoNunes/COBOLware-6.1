      $Set NoMs PreProcess(cobsql)
      *Set LinkCount(128) Gnt()
      $Set LinkCount(128)
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWORCN.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  21/11/2000.
       SECURITY.      *************************************************
                      *                                               *
                      *  Conecta ao Banco Oracle                      *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 CWMENU      PIC X     VALUE SPACES.
           05 RECO        PIC X(3)  VALUE SPACES.
           05 SHOWCONNECT PIC X(3)  VALUE SPACES.
           05 SQLMSG      PIC X(50) VALUE SPACES.

       01  AREAS-DE-TRABALHO-1.
           05 MSG                      PIC  X(068) VALUE SPACES.
           05 MINUSCULAS PIC X(26) VALUE "abcdefghijklmnopqrstuvwxyz".
           05 MAIUSCULAS PIC X(26) VALUE "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
           05 ONOFF                    PIC  9(001) VALUE 3.
           05 U                        PIC  9(003) VALUE ZERO.
           05                          PIC  9(001) VALUE ZERO.
              88 COM-BANCO                         VALUE 1.
              88 SEM-BANCO                         VALUE 0.
           05                          PIC  9(001) VALUE ZERO.
              88 COM-STRING                        VALUE 1.
              88 SEM-STRING                        VALUE 0.

       COPY CWSEND.
       COPY CWUNIX.

           EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  USERNAME          PIC X(10) VARYING.
       01  PASSWD            PIC X(10) VARYING.
       01  DATABASE          PIC X(20) VALUE SPACES.
       01  LOGIN             PIC X(70) VARYING.
           EXEC SQL END DECLARE SECTION END-EXEC.
           EXEC SQL INCLUDE SQLCA END-EXEC.

       LINKAGE SECTION.

       01 ORACLE-LOGIN.
          05 ORACLE-USERNAME PIC X(10).
          05 ORACLE-PASSWORD PIC X(10).
          05 ORACLE-PLUS     PIC X(50).

       PROCEDURE DIVISION USING ORACLE-LOGIN.

       000-INICIO.

           ON   1
                DISPLAY 'RECO-ON-COMMIT' UPON ENVIRONMENT-NAME
                ACCEPT RECO FROM ENVIRONMENT-VALUE
                INSPECT RECO CONVERTING 'on' TO 'ON'
                DISPLAY 'SQLMSG' UPON ENVIRONMENT-NAME
                ACCEPT SQLMSG FROM ENVIRONMENT-VALUE
                CALL "CWUNIX" USING PARAMETROS-CWUNIX
                  ON EXCEPTION
                     MOVE '@' TO CWUNIX-OS-CODE(1:1)
                  NOT ON EXCEPTION
                     DISPLAY 'CWSHOWCONNECT' UPON ENVIRONMENT-NAME
                     ACCEPT SHOWCONNECT FROM ENVIRONMENT-VALUE
                     INSPECT SHOWCONNECT CONVERTING 'on' TO 'ON'
                     IF SHOWCONNECT NOT = 'ON'
                        MOVE '*' TO CWUNIX-OS-CODE(1:1)
                     END-IF
                END-CALL.

           IF ORACLE-LOGIN (1:6) = 'COMMIT'
              EXEC SQL
                   COMMIT
              END-EXEC
              IF RECO = 'ON'
                 GO TO RETRY
              END-IF
              GOBACK
           END-IF

           IF ORACLE-LOGIN (1:8)  = 'ROLLBACK'
              EXEC SQL
                   ROLLBACK
              END-EXEC
              GOBACK
           END-IF
           DISPLAY 'CWORCN' UPON ENVIRONMENT-NAME
           MOVE SPACES TO CWMENU
           ACCEPT   CWMENU FROM ENVIRONMENT-VALUE
           IF CWMENU = '*'
              DISPLAY SPACES UPON ENVIRONMENT-VALUE
              GOBACK
           END-IF
           IF   ONOFF = 1
                MOVE 2 TO ONOFF
                IF CWUNIX-OS-CODE NUMERIC display MSG AT 2501 END-IF
                EXEC SQL COMMIT RELEASE END-EXEC
                EXIT PROGRAM
           END-IF
           DISPLAY "DB_LINK" UPON ENVIRONMENT-NAME
           ACCEPT DATABASE   FROM ENVIRONMENT-VALUE
           IF   DATABASE = SPACES
                DISPLAY "DB-LINK" UPON ENVIRONMENT-NAME
                ACCEPT DATABASE   FROM ENVIRONMENT-VALUE
           END-IF
           IF  (DATABASE NOT = SPACES)
           AND (DATABASE NOT = LOW-VALUES)
                SET COM-BANCO TO TRUE
           ELSE
                SET SEM-BANCO TO TRUE
           END-IF

           SET SEM-STRING TO TRUE

           PERFORM VARYING U
                      FROM 1 BY 1 UNTIL U > LENGTH ORACLE-LOGIN
                   IF ORACLE-LOGIN (U:1) = '@'
                      MOVE SPACES TO USERNAME-ARR
                      STRING ORACLE-LOGIN DELIMITED BY '/'
                          INTO USERNAME-ARR
                      SET COM-STRING TO TRUE
                      EXIT PERFORM
                   END-IF
           END-PERFORM

           MOVE SPACES TO MSG

           IF COM-STRING
              PERFORM VARYING LOGIN-LEN
                         FROM LENGTH OF ORACLE-LOGIN
                           BY -1
                        UNTIL ORACLE-LOGIN (LOGIN-LEN: 1) NOT = SPACE
                      CONTINUE
              END-PERFORM
              MOVE ORACLE-LOGIN(1:LOGIN-LEN)
                TO LOGIN-ARR
           ELSE
              PERFORM VARYING USERNAME-LEN
                         FROM LENGTH OF USERNAME-ARR
                           BY -1
                     UNTIL ORACLE-USERNAME (USERNAME-LEN: 1) NOT = SPACE
                      CONTINUE
              END-PERFORM
              PERFORM VARYING PASSWD-LEN
                         FROM LENGTH OF PASSWD-ARR
                           BY -1
                       UNTIL ORACLE-PASSWORD (PASSWD-LEN: 1) NOT = SPACE
                      CONTINUE
              END-PERFORM
              MOVE ORACLE-USERNAME(1:USERNAME-LEN) TO USERNAME-ARR
              MOVE ORACLE-PASSWORD(1:PASSWD-LEN) TO PASSWD-ARR
           END-IF.

       RETRY.

           IF   COM-BANCO
                STRING "Conectando " DELIMITED BY SIZE
                        USERNAME-ARR DELIMITED BY SPACE
                        " ao banco " DELIMITED BY SIZE
                        DATABASE     DELIMITED BY SPACE
                        "..."        DELIMITED BY SIZE
                   INTO MSG
                IF CWUNIX-OS-CODE NUMERIC display MSG AT 2501 END-IF
                INSPECT DATABASE CONVERTING MAIUSCULAS TO MINUSCULAS
                IF SEM-STRING
                   EXEC SQL
                        CONNECT :USERNAME IDENTIFIED BY :PASSWD
                          USING :DATABASE
                   END-EXEC
                ELSE
                   EXEC SQL
                        CONNECT :LOGIN
                          USING :DATABASE
                   END-EXEC
                END-IF
                MOVE SPACES TO MSG
                STRING "Desconectando " DELIMITED BY SIZE
                        USERNAME-ARR DELIMITED BY SPACE
                        " do banco " DELIMITED BY SIZE
                        DATABASE     DELIMITED BY SPACE
                        "..."        DELIMITED BY SIZE
                   INTO MSG
           ELSE
                STRING "Conectando "  DELIMITED BY SIZE
                       USERNAME-ARR   DELIMITED BY SPACE
                       " ao banco..." DELIMITED BY SIZE
                  INTO MSG
                 IF CWUNIX-OS-CODE NUMERIC display MSG AT 2501 END-IF
                 IF SEM-STRING
                    EXEC SQL
                         CONNECT :USERNAME IDENTIFIED BY :PASSWD
                    END-EXEC
                 ELSE
                    EXEC SQL
                         CONNECT :LOGIN
                    END-EXEC
                 END-IF
                 MOVE SPACES TO MSG
                 STRING "Desconectando "  DELIMITED BY SIZE
                        USERNAME-ARR   DELIMITED BY SPACE
                        " do banco..." DELIMITED BY SIZE
                   INTO MSG
           END-IF

           IF   SQLCODE NOT = 0
                MOVE SQLERRMC (1: SQLERRML - 1) TO CWSEND-MSG
                CALL "CWSEND" USING PARAMETROS-CWSEND
                   ON EXCEPTION
                      if sqlmsg not = spaces
                         CALL SQLMSG USING SQLERRM
                          ON EXCEPTION
                             display CWSEND-MSG
                         END-CALL
                      else
                         display CWSEND-MSG
                      end-if
                END-CALL
                IF SQLCODE = 28002 OR 28011
                   MOVE 0 TO SQLCODE
                END-IF
                MOVE SQLCODE TO RETURN-CODE
                IF   SQLCODE = -1034
                OR   SQLCODE = -1033
                     GO TO RETRY
                ELSE
                     IF   SQLCODE NOT = 0
                          STOP RUN
                     END-IF
                END-IF
           ELSE
                MOVE 0       TO RETURN-CODE
                MOVE 1       TO ONOFF
           END-IF

           MOVE SPACES TO MSG
           IF CWUNIX-OS-CODE NUMERIC display MSG AT 2501 END-IF.

       000-99-FIM. EXIT PROGRAM.

       END PROGRAM CWORCN.
